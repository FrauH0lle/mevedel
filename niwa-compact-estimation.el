;;; niwa-compact-estimation.el -- Compaction estimation and admission -*- lexical-binding: t -*-

;;; Commentary:

;; Owns provider token accounting, model budgeting, and request-data estimates
;; used to decide whether compaction is admissible.  Transcript projection,
;; target mutation, and asynchronous settlement have separate owners.

;;; Code:

(require 'niwa-engine)

(require 'cl-lib)
(require 'niwa-models)
(require 'niwa-telemetry)
(require 'niwa-telemetry-usage)
(require 'niwa-transcript-audit)

;; `gptel-request'
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `niwa-models'
(declare-function niwa-model-effective-context-window
                  "niwa-models" (&optional model))
(declare-function niwa-model-context-window
                  "niwa-models" (&optional model))
(declare-function niwa-model-resolve-workload
                  "niwa-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function niwa-model-usable-input-tokens "niwa-models" (policy))
(defvar niwa-model-context-limit)

;; `niwa-structs'
(declare-function niwa-request-id "niwa-structs" (cl-x) t)
(declare-function niwa-request-p "niwa-structs" (cl-x))
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-telemetry'
(declare-function niwa-telemetry-record
                  "niwa-telemetry" (session event &rest props))

(defcustom niwa-compact-estimation-token-threshold 0.80
  "Fraction of usable context that triggers auto-compaction."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (< 0.0 value)
                  (< value 1.0)))))
  :group 'niwa)

(defcustom niwa-compact-estimation-image-token-estimate 1844
  "Token estimate for one native image in realized request data.
This is used only by local auto-compaction estimates.  API-reported
token usage remains authoritative once a request has completed."
  :type 'natnum
  :group 'niwa)

(defvar-local niwa-compact-estimation--known-token-baseline nil
  "Last known non-compaction request token baseline plist.")

(defun niwa-compact-estimation--file-local-variables-start ()
  "Return position where file-local variables block begins, or nil.
Searches forward from beginning of buffer for the first Local Variables
block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Local Variables:" nil t)
      (line-beginning-position))))

(defun niwa-compact-estimation-usable-tokens ()
  "Return usable context tokens after response reserve."
  (niwa-model-usable-input-tokens
   (list :backend gptel-backend
         :model gptel-model
         :max-tokens gptel-max-tokens
         :request-params gptel--request-params)))

(defun niwa-compact-estimation-threshold-tokens (&optional usable-tokens)
  "Return the compaction threshold for USABLE-TOKENS."
  (let ((threshold niwa-compact-estimation-token-threshold)
        (usable (or usable-tokens (niwa-compact-estimation-usable-tokens))))
    (unless (and (floatp threshold)
                 (< 0.0 threshold)
                 (< threshold 1.0))
      (user-error "Compaction threshold must be a float strictly between 0.0 and 1.0"))
    (round (* threshold usable))))

(defun niwa-compact-estimation-workload-policy ()
  "Resolve and return the summarization workload model policy."
  (append '(:max-tokens nil :request-params nil)
          (niwa-model-resolve-workload 'summarization)))

(defun niwa-compact-estimation-target-policy ()
  "Return the current request's model policy for compaction budgeting."
  (list :backend gptel-backend
        :model gptel-model
        :effort (and (boundp 'gptel-reasoning-effort)
                     gptel-reasoning-effort)
        :max-tokens gptel-max-tokens
        :request-params gptel--request-params))

(defun niwa-compact-estimation-policy-threshold-tokens (policy)
  "Return the compaction threshold for model POLICY."
  (niwa-compact-estimation-threshold-tokens
   (niwa-model-usable-input-tokens policy)))

(defun niwa-compact-estimation-context-diagnostics (&optional target-policy)
  "Return context provenance and trigger budgets for TARGET-POLICY.
Default to the current request policy.  Include both target and summarizer:
the smaller threshold triggers local compaction.  This reports declared or
fallback budgets, not provider confirmation or native Claude admission."
  (let* ((target (or target-policy (niwa-compact-estimation-target-policy)))
         (summary (niwa-compact-estimation-workload-policy))
         (target-model (plist-get target :model))
         (summary-model (plist-get summary :model))
         (target-window (niwa-model-effective-context-window target-model))
         (summary-window (niwa-model-effective-context-window summary-model))
         (target-usable (niwa-model-usable-input-tokens target))
         (summary-usable (niwa-model-usable-input-tokens summary))
         (target-threshold
          (niwa-compact-estimation-threshold-tokens target-usable))
         (summary-threshold
          (niwa-compact-estimation-threshold-tokens summary-usable)))
    (list :target-model target-model
          :model-context-window target-window
          :model-context-source (niwa-model-context-window-source target-model)
          :target-reserve-tokens (- target-window target-usable)
          :target-usable-tokens target-usable
          :target-threshold target-threshold
          :summary-model summary-model
          :summary-context-window summary-window
          :summary-context-source (niwa-model-context-window-source summary-model)
          :summary-reserve-tokens (- summary-window summary-usable)
          :summary-usable-tokens summary-usable
          :summary-threshold summary-threshold
          :threshold-ratio niwa-compact-estimation-token-threshold
          :threshold (min target-threshold summary-threshold))))

(defun niwa-compact-estimation--token-usage-count (tokens)
  "Return total prompt/output TOKENS from a gptel token-usage plist."
  (when-let* ((input (niwa-compact-estimation--token-usage-input tokens))
              (output (or (plist-get tokens :output) 0))
              ((and (numberp output) (>= output 0))))
    (+ input output)))

(defun niwa-compact-estimation--token-usage-input (tokens)
  "Return input/cached prompt TOKENS from a gptel token-usage plist."
  (let ((keys '(:input :cached :cache-read :cache_read)))
    (when (and (proper-list-p tokens)
               (cl-some (lambda (key) (plist-member tokens key)) keys))
      (let ((values
             (mapcar (lambda (key) (or (plist-get tokens key) 0)) keys)))
        (when (cl-every (lambda (value)
                          (and (numberp value) (>= value 0)))
                        values)
          (apply #'+ values))))))

(defun niwa-compact-estimation-summary-request-p (info)
  "Return non-nil when INFO belongs to a context-summary request."
  (let ((context (and (listp info) (plist-get info :context))))
    (and (listp context)
         (plist-get context :niwa-context-summary))))

(defun niwa-compact-estimation-record-token-baseline (fsm)
  "Record FSM's active-context baseline for future estimates.

Context-summary requests are ignored so their token
count never becomes the baseline for the chat buffer."
  (when-let* ((info (and fsm (niwa-engine-info fsm)))
              ((not (plist-get info :external-history)))
              ((not (niwa-compact-estimation-summary-request-p info)))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((provider-usage (plist-get info :tokens))
             (cumulative-usage (plist-get info :tokens-full))
             (provider-input
              (niwa-compact-estimation--token-usage-input provider-usage))
             (provider-count
              (niwa-compact-estimation--token-usage-count provider-usage))
             (cumulative-count
              (niwa-compact-estimation--token-usage-count cumulative-usage))
             (model (or (plist-get info :model) gptel-model))
             (context-window
              (niwa-model-effective-context-window model))
             (provider-status
              (cond
               ((null provider-count) 'missing)
               ((or (null provider-input) (<= provider-input 0)) 'zero)
               ((> provider-count context-window) 'over-window)
               (t 'valid)))
             (source (if (eq provider-status 'valid)
                         'provider-context
                       'fresh-estimate))
             ;; A whole-transcript scan per response is only worth paying
             ;; when it becomes the baseline.
             (fresh-estimate
              (and (eq source 'fresh-estimate)
                   (niwa-compact-estimation-estimate-buffer-tokens
                    chat-buffer)))
             (chosen (if (eq source 'provider-context)
                         provider-count
                       fresh-estimate))
             (request-id (plist-get info :niwa-request-id)))
        (setq niwa-compact-estimation--known-token-baseline
              (list :tokens chosen
                    :source source
                    :request-id request-id
                    :model model
                    :provider-context-usage (copy-tree provider-usage)
                    :cumulative-usage (copy-tree cumulative-usage)
                    :provider-context-tokens provider-count
                    :cumulative-usage-tokens cumulative-count
                    :provider-context-status provider-status
                    :fresh-visible-prompt-estimate fresh-estimate
                    :model-context-window context-window
                    :input-tokens provider-input
                    :output-tokens
                    (and (numberp (plist-get provider-usage :output))
                         (plist-get provider-usage :output))
                    :position (copy-marker (point-max))))
        (when (bound-and-true-p niwa--session)
          (niwa-telemetry-record
           niwa--session 'token-baseline-recorded
           :request-id request-id
           :model model
           :provider-context-usage (niwa-telemetry-usage-normalize provider-usage)
           :cumulative-usage (niwa-telemetry-usage-normalize cumulative-usage)
           :provider-context-tokens provider-count
           :cumulative-usage-tokens cumulative-count
           :provider-context-status provider-status
           :fresh-visible-prompt-estimate fresh-estimate
           :chosen-active-context-tokens chosen
           :chosen-source source
           :model-context-window context-window
           :baseline-marker-position (point-max)
           :input-tokens provider-input
           :output-tokens
           (and (numberp (plist-get provider-usage :output))
                (plist-get provider-usage :output))))))))

(defun niwa-compact-estimation-estimate-tokens ()
  "Estimate the number of tokens in the current buffer.
Only counts text not marked with the `gptel' property `ignore' and
excludes file-local variables block."
  (if-let* ((baseline niwa-compact-estimation--known-token-baseline)
            (tokens (plist-get baseline :tokens))
            (position (plist-get baseline :position))
            ((integer-or-marker-p position))
            (pos (if (markerp position) (marker-position position) position))
            ((<= (point-min) pos))
            ((<= pos (point-max))))
      (+ tokens
         (/ (save-restriction
              (narrow-to-region pos (point-max))
              (niwa-compact-estimation-model-visible-chars))
            4))
    (/ (niwa-compact-estimation-model-visible-chars) 4)))

(defconst niwa-compact-estimation--reminder-wrapper-chars
  (length "<system-reminder>\n\n</system-reminder>")
  "Characters the provider wrapper adds around one reminder body.")

(defun niwa-compact-estimation--reminder-chars (items)
  "Return the provider-visible length of reminder ITEMS.
Each body is sent wrapped in a system-reminder element and the elements are
joined by newlines; count that shape without building it."
  (let ((total (max 0 (1- (length items)))))
    (dolist (entry items total)
      (let ((body (plist-get entry :body)))
        (cl-incf total
                 (+ niwa-compact-estimation--reminder-wrapper-chars
                    (length (if (stringp body) body (format "%s" body)))))))))

(defun niwa-compact-estimation-model-visible-chars ()
  "Return model-visible character count in the current buffer."
  (let ((pos (point-min))
        (total 0)
        (flv-start (niwa-compact-estimation--file-local-variables-start)))
    (while (< pos (point-max))
      (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
             (end (if (and flv-start (> next flv-start)) flv-start next)))
        (when (or (null flv-start) (< pos flv-start))
          (pcase (get-text-property pos 'gptel)
            ('niwa-hook-audit
             (dolist (span (niwa-transcript-audit-buffer-spans
                            'injected-reminders pos end))
               (cl-incf total
                        (niwa-compact-estimation--reminder-chars
                         (plist-get (plist-get span :record) :items)))))
            ((or 'ignore 'niwa-render-data))
            (_ (cl-incf total (- end pos)))))
        (setq pos next)))
    total))

(defun niwa-compact-estimation-current-request-id ()
  "Return the active request identifier in the current buffer, or nil."
  (and (bound-and-true-p niwa--current-request)
       (niwa-request-p niwa--current-request)
       (niwa-request-id niwa--current-request)))

(defun niwa-compact-estimation-telemetry-inputs (estimate target-policy)
  "Return detailed compaction telemetry for ESTIMATE and TARGET-POLICY."
  (let* ((baseline niwa-compact-estimation--known-token-baseline)
         (position (plist-get baseline :position)))
    (append
     (niwa-compact-estimation-context-diagnostics target-policy)
     (list
      :request-id (niwa-compact-estimation-current-request-id)
      :baseline-request-id (plist-get baseline :request-id)
      :provider-context-model (plist-get baseline :model)
      :provider-context-window (plist-get baseline :model-context-window)
      :provider-context-usage
      (niwa-telemetry-usage-normalize (plist-get baseline :provider-context-usage))
      :cumulative-usage
      (niwa-telemetry-usage-normalize (plist-get baseline :cumulative-usage))
      :provider-context-tokens (plist-get baseline :provider-context-tokens)
      :cumulative-usage-tokens (plist-get baseline :cumulative-usage-tokens)
      :provider-context-status (plist-get baseline :provider-context-status)
      :chosen-active-context-tokens estimate
      :chosen-source (or (plist-get baseline :source) 'fresh-estimate)
      :baseline-marker-position
      (and (integer-or-marker-p position)
           (if (markerp position) (marker-position position) position))
      :buffer-chars-total (buffer-size)))))

(defun niwa-compact-estimation-admission (estimate target-policy)
  "Return compaction admission for ESTIMATE and TARGET-POLICY, or nil."
  (let* ((summary-policy (niwa-compact-estimation-workload-policy))
         (target-threshold
          (niwa-compact-estimation-policy-threshold-tokens target-policy))
         (summary-threshold
          (niwa-compact-estimation-policy-threshold-tokens summary-policy)))
    (when (>= estimate (min target-threshold summary-threshold))
      (list :summary-policy summary-policy
            :target-pressure (>= estimate target-threshold)))))

(defun niwa-compact-estimation-baseline-source ()
  "Return the source of the current buffer's token baseline."
  (or (plist-get niwa-compact-estimation--known-token-baseline :source)
      'fresh-estimate))

(defun niwa-compact-estimation-clear-baseline ()
  "Clear the current buffer's token baseline."
  (setq niwa-compact-estimation--known-token-baseline nil))

(defun niwa-compact-estimation-estimate-buffer-tokens (buffer)
  "Return a fresh token estimate for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let (niwa-compact-estimation--known-token-baseline)
        (niwa-compact-estimation-estimate-tokens)))))

(defun niwa-compact-estimation-estimate-transformed-request-tokens
    (source-buffer prompt-buffer)
  "Estimate a transformed request from SOURCE-BUFFER and PROMPT-BUFFER.

SOURCE-BUFFER may have an API-corrected token baseline.  PROMPT-BUFFER
is gptel's temporary prompt buffer after niwa prompt transforms have
run."
  (when (and (buffer-live-p source-buffer)
             (buffer-live-p prompt-buffer))
    (with-current-buffer source-buffer
      (let ((source-estimate (niwa-compact-estimation-estimate-tokens))
            (source-fresh (niwa-compact-estimation-estimate-buffer-tokens
                           source-buffer))
            (prompt-fresh (niwa-compact-estimation-estimate-buffer-tokens
                           prompt-buffer)))
        (if niwa-compact-estimation--known-token-baseline
            (+ source-estimate (max 0 (- prompt-fresh source-fresh)))
          prompt-fresh)))))

(defun niwa-compact-estimation--string-lengths (&rest values)
  "Return the total length of string VALUES."
  (let ((chars 0))
    (dolist (value values chars)
      (when (stringp value)
        (cl-incf chars (length value))))))

(defun niwa-compact-estimation--image-data-url-payload-start (url)
  "Return the payload start index for image data URL string URL."
  (when (and (stringp url)
             (string-prefix-p "data:" url t))
    (when-let* ((comma (string-search "," url)))
      (let* ((metadata (substring url 5 comma))
             (parts (split-string metadata ";"))
             (mime (car parts)))
        (when (and (stringp mime)
                   (string-prefix-p "image/" mime t)
                   (cl-some
                    (lambda (part)
                      (string-equal (downcase part) "base64"))
                    (cdr parts)))
          (1+ comma))))))

(defun niwa-compact-estimation--image-data-url-estimate (url)
  "Return (CHARS . TOKENS) for image data URL string URL."
  (when-let* ((payload-start
               (niwa-compact-estimation--image-data-url-payload-start url)))
    (cons payload-start niwa-compact-estimation-image-token-estimate)))

(defun niwa-compact-estimation--openai-image-block-estimate (block)
  "Return (CHARS . TOKENS) for OpenAI native image BLOCK."
  (when-let* (((consp block))
              (type (plist-get block :type))
              ((member type '("input_image" "image_url")))
              (image-url (plist-get block :image_url))
              (url (if (stringp image-url)
                       image-url
                     (and (consp image-url)
                          (plist-get image-url :url))))
              (estimate (niwa-compact-estimation--image-data-url-estimate url)))
    (cons (+ (niwa-compact-estimation--string-lengths type)
             (car estimate))
          (cdr estimate))))

(defun niwa-compact-estimation--anthropic-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Anthropic native image BLOCK."
  (when-let* (((consp block))
              ((equal (plist-get block :type) "image"))
              (source (plist-get block :source))
              ((consp source))
              ((equal (plist-get source :type) "base64"))
              (mime (plist-get source :media_type))
              ((and (stringp mime) (string-prefix-p "image/" mime t)))
              ((stringp (plist-get source :data))))
    (cons (niwa-compact-estimation--string-lengths
           (plist-get block :type)
           (plist-get source :type)
           mime)
          niwa-compact-estimation-image-token-estimate)))

(defun niwa-compact-estimation--bedrock-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Bedrock native image BLOCK."
  (when-let* (((consp block))
              (image (plist-get block :image))
              ((consp image))
              (format (plist-get image :format))
              ((stringp format))
              (source (plist-get image :source))
              ((consp source))
              ((stringp (plist-get source :bytes))))
    (cons (niwa-compact-estimation--string-lengths format)
          niwa-compact-estimation-image-token-estimate)))

(defun niwa-compact-estimation--media-block-estimate (block)
  "Return (CHARS . TOKENS) when BLOCK is native media request data."
  (or (niwa-compact-estimation--openai-image-block-estimate block)
      (niwa-compact-estimation--anthropic-image-block-estimate block)
      (niwa-compact-estimation--bedrock-image-block-estimate block)))

(defun niwa-compact-estimation-estimate-data-tokens (data)
  "Return a token estimate for realized request DATA.
Text is still estimated with chars/4.  Native image payloads are counted
as model-visible images instead of raw base64 text."
  (let ((chars 0)
        (media-tokens 0))
    (cl-labels
        ((walk (value)
           (cond
            ((stringp value)
             (cl-incf chars (length value)))
            ((vectorp value)
             (mapc #'walk value))
            ((consp value)
             (if-let* ((estimate
                        (niwa-compact-estimation--media-block-estimate value)))
                 (progn
                   (cl-incf chars (car estimate))
                   (cl-incf media-tokens (cdr estimate)))
               (walk (car value))
               (walk (cdr value)))))))
      (walk data))
    (+ (/ chars 4) media-tokens)))


(provide 'niwa-compact-estimation)

;;; niwa-compact-estimation.el ends here
