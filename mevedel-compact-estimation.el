;;; mevedel-compact-estimation.el -- Compaction estimation and admission -*- lexical-binding: t -*-

;;; Commentary:

;; Owns provider token accounting, model budgeting, and request-data estimates
;; used to decide whether compaction is admissible.  Transcript projection,
;; target mutation, and asynchronous settlement have separate owners.

;;; Code:

(require 'cl-lib)
(require 'mevedel-models)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

;; `mevedel-models'
(declare-function mevedel-model-effective-context-window
                  "mevedel-models" (&optional model))
(declare-function mevedel-model-context-window
                  "mevedel-models" (&optional model))
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-usable-input-tokens "mevedel-models" (policy))
(defvar mevedel-model-context-limit)

;; `mevedel-structs'
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record
                  "mevedel-telemetry" (session event &rest props))

(defcustom mevedel-compact-estimation-token-threshold 0.80
  "Fraction of usable context that triggers auto-compaction."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (< 0.0 value)
                  (< value 1.0)))))
  :group 'mevedel)

(defcustom mevedel-compact-estimation-image-token-estimate 1844
  "Token estimate for one native image in realized request data.
This is used only by local auto-compaction estimates.  API-reported
token usage remains authoritative once a request has completed."
  :type 'natnum
  :group 'mevedel)

(defvar-local mevedel-compact-estimation--known-token-baseline nil
  "Last known non-compaction request token baseline plist.")

(defun mevedel-compact-estimation--file-local-variables-start ()
  "Return position where file-local variables block begins, or nil.
Searches forward from beginning of buffer for the first Local Variables
block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Local Variables:" nil t)
      (line-beginning-position))))

(defun mevedel-compact-estimation-usable-tokens ()
  "Return usable context tokens after response reserve."
  (mevedel-model-usable-input-tokens
   (list :backend gptel-backend
         :model gptel-model
         :max-tokens gptel-max-tokens
         :request-params gptel--request-params)))

(defun mevedel-compact-estimation-threshold-tokens (&optional usable-tokens)
  "Return the compaction threshold for USABLE-TOKENS."
  (let ((threshold mevedel-compact-estimation-token-threshold)
        (usable (or usable-tokens (mevedel-compact-estimation-usable-tokens))))
    (unless (and (floatp threshold)
                 (< 0.0 threshold)
                 (< threshold 1.0))
      (user-error "Compaction threshold must be a float strictly between 0.0 and 1.0"))
    (round (* threshold usable))))

(defun mevedel-compact-estimation-workload-policy ()
  "Resolve and return the summarization workload model policy."
  (append '(:max-tokens nil :request-params nil)
          (mevedel-model-resolve-workload 'summarization)))

(defun mevedel-compact-estimation-target-policy ()
  "Return the current request's model policy for compaction budgeting."
  (list :backend gptel-backend
        :model gptel-model
        :effort (and (boundp 'gptel-reasoning-effort)
                     gptel-reasoning-effort)
        :max-tokens gptel-max-tokens
        :request-params gptel--request-params))

(defun mevedel-compact-estimation-policy-threshold-tokens (policy)
  "Return the compaction threshold for model POLICY."
  (mevedel-compact-estimation-threshold-tokens
   (mevedel-model-usable-input-tokens policy)))

(defun mevedel-compact-estimation--token-usage-count (tokens)
  "Return total prompt/output TOKENS from a gptel token-usage plist."
  (when-let* ((input (mevedel-compact-estimation--token-usage-input tokens))
              (output (or (plist-get tokens :output) 0))
              ((and (numberp output) (>= output 0))))
    (+ input output)))

(defun mevedel-compact-estimation--token-usage-input (tokens)
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

(defun mevedel-compact-estimation-summary-request-p (info)
  "Return non-nil when INFO belongs to a context-summary request."
  (let ((context (and (listp info) (plist-get info :context))))
    (and (listp context)
         (plist-get context :mevedel-context-summary))))

(defun mevedel-compact-estimation-record-token-baseline (fsm)
  "Record FSM's active-context baseline for future estimates.

Context-summary requests are ignored so their token
count never becomes the baseline for the chat buffer."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              ((not (mevedel-compact-estimation-summary-request-p info)))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((provider-usage (plist-get info :tokens))
             (cumulative-usage (plist-get info :tokens-full))
             (provider-input
              (mevedel-compact-estimation--token-usage-input provider-usage))
             (provider-count
              (mevedel-compact-estimation--token-usage-count provider-usage))
             (cumulative-count
              (mevedel-compact-estimation--token-usage-count cumulative-usage))
             (fresh-estimate
              (mevedel-compact-estimation-estimate-buffer-tokens chat-buffer))
             (model (or (plist-get info :model) gptel-model))
             (context-window
              (mevedel-model-effective-context-window model))
             (provider-status
              (cond
               ((null provider-count) 'missing)
               ((or (null provider-input) (<= provider-input 0)) 'zero)
               ((> provider-count context-window) 'over-window)
               (t 'valid)))
             (source (if (eq provider-status 'valid)
                         'provider-context
                       'fresh-estimate))
             (chosen (if (eq source 'provider-context)
                         provider-count
                       fresh-estimate))
             (request-id (plist-get info :mevedel-request-id)))
        (setq mevedel-compact-estimation--known-token-baseline
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
                    :input-tokens
                    (mevedel-compact-estimation--token-usage-input provider-usage)
                    :output-tokens
                    (and (numberp (plist-get provider-usage :output))
                         (plist-get provider-usage :output))
                    :position (copy-marker (point-max))))
        (when (and (bound-and-true-p mevedel--session)
                   (fboundp 'mevedel-telemetry-record))
          (mevedel-telemetry-record
           mevedel--session 'token-baseline-recorded
           :request-id request-id
           :model model
           :provider-context-usage provider-usage
           :cumulative-usage cumulative-usage
           :provider-context-tokens provider-count
           :cumulative-usage-tokens cumulative-count
           :provider-context-status provider-status
           :fresh-visible-prompt-estimate fresh-estimate
           :chosen-active-context-tokens chosen
           :chosen-source source
           :model-context-window context-window
           :baseline-marker-position (point-max)
           :input-tokens
           (mevedel-compact-estimation--token-usage-input provider-usage)
           :output-tokens
           (and (numberp (plist-get provider-usage :output))
                (plist-get provider-usage :output))))))))

(defun mevedel-compact-estimation-estimate-tokens ()
  "Estimate the number of tokens in the current buffer.
Only counts text not marked with the `gptel' property `ignore' and
excludes file-local variables block."
  (if-let* ((baseline mevedel-compact-estimation--known-token-baseline)
            (tokens (plist-get baseline :tokens))
            (position (plist-get baseline :position))
            ((integer-or-marker-p position))
            (pos (if (markerp position) (marker-position position) position))
            ((<= (point-min) pos))
            ((<= pos (point-max))))
      (+ tokens (/ (- (point-max) pos) 4))
    (let ((pos (point-min))
          (total 0)
          (flv-start (mevedel-compact-estimation--file-local-variables-start)))
      (while (< pos (point-max))
        (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
               (prop (get-text-property pos 'gptel)))
          (unless (memq prop
                        '(ignore mevedel-render-data mevedel-hook-audit))
            ;; Only count if not in file-local-variables region
            (when (or (null flv-start) (< pos flv-start))
              (setq total (+ total (- (if (and flv-start (> next flv-start))
                                          flv-start
                                        next)
                                      pos)))))
          (setq pos next)))
      (/ total 4))))

(defun mevedel-compact-estimation-model-visible-chars ()
  "Return model-visible character count in the current buffer."
  (let ((pos (point-min))
        (total 0)
        (flv-start (mevedel-compact-estimation--file-local-variables-start)))
    (while (< pos (point-max))
      (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
             (end (if (and flv-start (> next flv-start)) flv-start next)))
        (when (and (not (memq (get-text-property pos 'gptel)
                              '(ignore mevedel-render-data
                                       mevedel-hook-audit)))
                   (or (null flv-start) (< pos flv-start)))
          (cl-incf total (- end pos)))
        (setq pos next)))
    total))

(defun mevedel-compact-estimation-current-request-id ()
  "Return the active request identifier in the current buffer, or nil."
  (and (bound-and-true-p mevedel--current-request)
       (mevedel-request-p mevedel--current-request)
       (mevedel-request-id mevedel--current-request)))

(defun mevedel-compact-estimation-telemetry-inputs (estimate target-policy)
  "Return detailed compaction telemetry for ESTIMATE and TARGET-POLICY."
  (let* ((baseline mevedel-compact-estimation--known-token-baseline)
         (position (plist-get baseline :position))
         (summary-policy (mevedel-compact-estimation-workload-policy)))
    (list
     :request-id (mevedel-compact-estimation-current-request-id)
     :baseline-request-id (plist-get baseline :request-id)
     :provider-context-model (plist-get baseline :model)
     :provider-context-window (plist-get baseline :model-context-window)
     :provider-context-usage (plist-get baseline :provider-context-usage)
     :cumulative-usage (plist-get baseline :cumulative-usage)
     :provider-context-tokens (plist-get baseline :provider-context-tokens)
     :cumulative-usage-tokens (plist-get baseline :cumulative-usage-tokens)
     :provider-context-status (plist-get baseline :provider-context-status)
     :chosen-active-context-tokens estimate
     :chosen-source (or (plist-get baseline :source) 'fresh-estimate)
     :fresh-visible-prompt-estimate
     (mevedel-compact-estimation-estimate-buffer-tokens (current-buffer))
     :target-model (plist-get target-policy :model)
     :model-context-window
     (mevedel-model-context-window (plist-get target-policy :model))
     :threshold
     (min (mevedel-compact-estimation-policy-threshold-tokens target-policy)
          (mevedel-compact-estimation-policy-threshold-tokens summary-policy))
     :baseline-marker-position
     (and (integer-or-marker-p position)
          (if (markerp position) (marker-position position) position))
     :buffer-chars-total (buffer-size)
     :buffer-chars-model-visible (mevedel-compact-estimation-model-visible-chars))))

(defun mevedel-compact-estimation-admission (estimate target-policy)
  "Return compaction admission for ESTIMATE and TARGET-POLICY, or nil."
  (let* ((summary-policy (mevedel-compact-estimation-workload-policy))
         (target-threshold
          (mevedel-compact-estimation-policy-threshold-tokens target-policy))
         (summary-threshold
          (mevedel-compact-estimation-policy-threshold-tokens summary-policy)))
    (when (>= estimate (min target-threshold summary-threshold))
      (list :summary-policy summary-policy
            :target-pressure (>= estimate target-threshold)))))

(defun mevedel-compact-estimation-baseline-source ()
  "Return the source of the current buffer's token baseline."
  (or (plist-get mevedel-compact-estimation--known-token-baseline :source)
      'fresh-estimate))

(defun mevedel-compact-estimation-clear-baseline ()
  "Clear the current buffer's token baseline."
  (setq mevedel-compact-estimation--known-token-baseline nil))

(defun mevedel-compact-estimation-estimate-buffer-tokens (buffer)
  "Return a fresh token estimate for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let (mevedel-compact-estimation--known-token-baseline)
        (mevedel-compact-estimation-estimate-tokens)))))

(defun mevedel-compact-estimation-estimate-transformed-request-tokens
    (source-buffer prompt-buffer)
  "Estimate a transformed request from SOURCE-BUFFER and PROMPT-BUFFER.

SOURCE-BUFFER may have an API-corrected token baseline.  PROMPT-BUFFER
is gptel's temporary prompt buffer after mevedel prompt transforms have
run."
  (when (and (buffer-live-p source-buffer)
             (buffer-live-p prompt-buffer))
    (let* ((source-state
            (with-current-buffer source-buffer
              (list :baseline mevedel-compact-estimation--known-token-baseline
                    :estimate (mevedel-compact-estimation-estimate-tokens))))
           (source-estimate (plist-get source-state :estimate))
           (source-fresh (mevedel-compact-estimation-estimate-buffer-tokens
                          source-buffer))
           (prompt-fresh (mevedel-compact-estimation-estimate-buffer-tokens
                          prompt-buffer)))
      (cond
       ((not (plist-get source-state :baseline))
        prompt-fresh)
       ((and source-estimate source-fresh prompt-fresh)
        (+ source-estimate
           (max 0 (- prompt-fresh source-fresh))))
       (source-estimate)
       (prompt-fresh)))))

(defun mevedel-compact-estimation--string-lengths (&rest values)
  "Return the total length of string VALUES."
  (let ((chars 0))
    (dolist (value values chars)
      (when (stringp value)
        (cl-incf chars (length value))))))

(defun mevedel-compact-estimation--image-data-url-payload-start (url)
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

(defun mevedel-compact-estimation--image-data-url-estimate (url)
  "Return (CHARS . TOKENS) for image data URL string URL."
  (when-let* ((payload-start
               (mevedel-compact-estimation--image-data-url-payload-start url)))
    (cons payload-start mevedel-compact-estimation-image-token-estimate)))

(defun mevedel-compact-estimation--openai-image-block-estimate (block)
  "Return (CHARS . TOKENS) for OpenAI native image BLOCK."
  (when-let* (((consp block))
              (type (plist-get block :type))
              ((member type '("input_image" "image_url")))
              (image-url (plist-get block :image_url))
              (url (if (stringp image-url)
                       image-url
                     (and (consp image-url)
                          (plist-get image-url :url))))
              (estimate (mevedel-compact-estimation--image-data-url-estimate url)))
    (cons (+ (mevedel-compact-estimation--string-lengths type)
             (car estimate))
          (cdr estimate))))

(defun mevedel-compact-estimation--anthropic-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Anthropic native image BLOCK."
  (when-let* (((consp block))
              ((equal (plist-get block :type) "image"))
              (source (plist-get block :source))
              ((consp source))
              ((equal (plist-get source :type) "base64"))
              (mime (plist-get source :media_type))
              ((and (stringp mime) (string-prefix-p "image/" mime t)))
              ((stringp (plist-get source :data))))
    (cons (mevedel-compact-estimation--string-lengths
           (plist-get block :type)
           (plist-get source :type)
           mime)
          mevedel-compact-estimation-image-token-estimate)))

(defun mevedel-compact-estimation--bedrock-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Bedrock native image BLOCK."
  (when-let* (((consp block))
              (image (plist-get block :image))
              ((consp image))
              (format (plist-get image :format))
              ((stringp format))
              (source (plist-get image :source))
              ((consp source))
              ((stringp (plist-get source :bytes))))
    (cons (mevedel-compact-estimation--string-lengths format)
          mevedel-compact-estimation-image-token-estimate)))

(defun mevedel-compact-estimation--media-block-estimate (block)
  "Return (CHARS . TOKENS) when BLOCK is native media request data."
  (or (mevedel-compact-estimation--openai-image-block-estimate block)
      (mevedel-compact-estimation--anthropic-image-block-estimate block)
      (mevedel-compact-estimation--bedrock-image-block-estimate block)))

(defun mevedel-compact-estimation-estimate-data-tokens (data)
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
                        (mevedel-compact-estimation--media-block-estimate value)))
                 (progn
                   (cl-incf chars (car estimate))
                   (cl-incf media-tokens (cdr estimate)))
               (walk (car value))
               (walk (cdr value)))))))
      (walk data))
    (+ (/ chars 4) media-tokens)))


(provide 'mevedel-compact-estimation)

;;; mevedel-compact-estimation.el ends here
