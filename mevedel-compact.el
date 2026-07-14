;;; mevedel-compact.el --- Chat compaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Summarizes older portions of a chat session to reduce token usage.  Compaction
;; rotates to a new on-disk segment whose first block is an anchored summary
;; followed by a preserved recent tail.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel))
(require 'mevedel-transcript)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `cl-seq'
(declare-function cl-find-if "cl-seq" (cl-pred cl-list &rest cl-keys))
(declare-function cl-remove-if-not "cl-seq" (cl-pred cl-list &rest cl-keys))

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--markdown-block-map)
(defvar gptel-mode)

;; `gptel-request'
(declare-function gptel--create-prompt-buffer "ext:gptel-request"
                  (&optional prompt-end))
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--merge-plists "ext:gptel-request" (&rest plists))
(declare-function gptel--model-request-params "ext:gptel-request" (model))
(declare-function gptel--realize-query "ext:gptel-request" (fsm))
(declare-function gptel-backend-request-params "ext:gptel-request" (backend))
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-request "ext:gptel-request")
(defvar gptel--request-alist)
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; `mevedel-agent-exec'
(declare-function mevedel-agent-exec--flush-transcript-save
                  "mevedel-agent-exec" (invocation))
(declare-function mevedel-agent-exec--record-activity
                  "mevedel-agent-exec" (invocation item &optional reserved))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)

;; `mevedel-chat'
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))

;; `mevedel-hooks'
(declare-function mevedel-hooks-additional-context-string "mevedel-hooks"
                  (decision &optional event))
(declare-function mevedel-hooks-context-audit-records
                  "mevedel-hooks" (decision event type &optional omit-context))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))

;; `mevedel-mentions'
(declare-function mevedel--transform-expand-mentions "mevedel-mentions" (fsm))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-reminders'
(declare-function mevedel-reminders--transform "mevedel-reminders" (fsm))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--segment-path
                  "mevedel-session-persistence" (session-dir segment))
(declare-function mevedel-session-persistence--segment-summary-bounds
                  "mevedel-session-persistence" ())
(declare-function mevedel-session-persistence--strip-summary-handoff-prefix
                  "mevedel-session-persistence" (summary))
(declare-function mevedel-session-persistence--summary-block
                  "mevedel-session-persistence" (summary))
(declare-function mevedel-session-persistence-rotate-segment
                  "mevedel-session-persistence" (session buffer summary
                                                         &rest keys))
(defvar mevedel-session--read-only-mode)

;; `mevedel-structs'
(declare-function mevedel-file-interaction-modified-turn
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-file-interaction-read-turn
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-enqueue-pending-reminder
                  "mevedel-structs" (session body))
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-invoked-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-touched-files "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-args
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-name
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-origin
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-role
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-turn
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

;; `mevedel-utilities'
(declare-function mevedel--format-hook-audit-record "mevedel-utilities"
                  (record))
(declare-function mevedel--same-file-p "mevedel-utilities" (a b))
(declare-function mevedel--strip-hook-audit-blocks "mevedel-utilities"
                  (text))

;; `mevedel-view'
(declare-function mevedel-view--full-rerender "mevedel-view" ())

;; `mevedel-view-stream'
(declare-function mevedel-view--stop-request-progress
                  "mevedel-view-stream" ())
(declare-function mevedel-view--stop-spinner "mevedel-view-stream" ())
(declare-function mevedel-view--update-spinner
                  "mevedel-view-stream" (status))


(defcustom mevedel-compact-context-limit nil
  "Fallback context window in tokens.

Mevedel uses this only when the active model has no declared
`:context-window'.  When nil, the fallback is 128000 tokens."
  :type '(choice (const :tag "Use built-in fallback" nil)
          (natnum :tag "Fallback token count"))
  :group 'mevedel)

(defcustom mevedel-compact-token-threshold 0.80
  "Fraction of usable context that triggers auto-compaction."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (< 0.0 value)
                  (< value 1.0)))))
  :group 'mevedel)

(defcustom mevedel-compact-reserve-tokens 20000
  "Token headroom reserved below the model context window."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-image-token-estimate 1844
  "Token estimate for one native image in realized request data.
This is used only by local auto-compaction estimates.  API-reported
token usage remains authoritative once a request has completed."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-auto t
  "Whether mevedel automatically compacts persisted sessions."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-compact-tail-turns 2
  "Target number of recent complete turns to preserve verbatim."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-tail-budget 0.25
  "Maximum fraction of usable context reserved for preserved tail."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (<= 0.0 value)
                  (<= value 1.0)))))
  :group 'mevedel)

(defcustom mevedel-compact-tail-tool-output-max 4000
  "Per-tool-result character cap inside the preserved tail."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-body-tool-output-max 8000
  "Per-tool-result character cap inside the compaction request body."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-file-reference-reminder-limit 20
  "Maximum number of compacted file references to cite in one reminder."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-warn-on-completion t
  "Whether to show a one-shot accuracy message after compaction."
  :type 'boolean
  :group 'mevedel)

(defvar-local mevedel--known-token-baseline nil
  "Last known non-compaction request token baseline plist.")

(defvar-local mevedel--compaction-in-flight nil
  "Non-nil while a compaction request is active in this buffer.")

(defvar-local mevedel--compact-auto-disabled nil
  "Non-nil when auto-compaction is disabled for this session.")

(defvar-local mevedel--compact-failure-count 0
  "Consecutive compaction failures in this session.")

(defvar-local mevedel--compact-warning-shown nil
  "Non-nil after showing the post-compaction accuracy warning.")

(defvar-local mevedel--compact-auto-ineligible-warning-shown nil
  "Non-nil after warning that auto-compaction cannot run here.")

(defvar-local mevedel--compact-current-request-reminder nil
  "Reminder body to inject into the current auto-compacted request.")

(defun mevedel--file-local-variables-start ()
  "Return position where file-local variables block begins, or nil.
Searches forward from beginning of buffer for the first Local Variables
block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Local Variables:" nil t)
      (line-beginning-position))))

(defsubst mevedel--model-context-window (&optional model)
  "Return MODEL's context window in tokens.
MODEL defaults to `gptel-model'.  gptel stores `:context-window' in
thousands of tokens, sometimes as a float."
  (when-let* ((m (or model gptel-model))
              (kt (get m :context-window)))
    (round (* kt 1000))))

(defun mevedel--compact-context-limit ()
  "Return the active model's effective context window in tokens."
  (or (mevedel--model-context-window)
      mevedel-compact-context-limit
      128000))

(defun mevedel--compact-model-max-output-tokens ()
  "Return the effective configured max-output token count, or 0."
  (or gptel-max-tokens
      (when (and (bound-and-true-p gptel-backend)
                 (fboundp 'gptel--merge-plists)
                 (fboundp 'gptel-backend-request-params)
                 (fboundp 'gptel--model-request-params))
        (let ((params (gptel--merge-plists
                       gptel--request-params
                       (gptel-backend-request-params gptel-backend)
                       (gptel--model-request-params gptel-model))))
          (or (plist-get params :max_tokens)
              (plist-get params :maxOutputTokens)
              (plist-get params :max_output_tokens)
              (plist-get params :num_predict))))
      0))

(defun mevedel--compact-usable-tokens ()
  "Return usable context tokens after response reserve."
  (let* ((context (mevedel--compact-context-limit))
         ;; A fixed 20k reserve is sensible for large hosted models,
         ;; but would collapse 8k/16k local models to a one-token
         ;; threshold.  Cap the reserve to half the context window so
         ;; fractional thresholds remain meaningful on small models.
         (reserve-cap (max 1 (/ context 2)))
         (requested-reserve
          (max mevedel-compact-reserve-tokens
               (or (mevedel--compact-model-max-output-tokens) 0)))
         (effective-reserve (min requested-reserve reserve-cap)))
    (max 1 (- context effective-reserve))))

(defun mevedel--compact-threshold-tokens (&optional usable-tokens)
  "Return the compaction threshold for USABLE-TOKENS."
  (let ((threshold mevedel-compact-token-threshold)
        (usable (or usable-tokens (mevedel--compact-usable-tokens))))
    (unless (and (floatp threshold)
                 (< 0.0 threshold)
                 (< threshold 1.0))
      (user-error "Compaction threshold must be a float strictly between 0.0 and 1.0"))
    (round (* threshold usable))))

(defun mevedel--compact-workload-policy ()
  "Resolve and return the compaction workload model policy."
  (require 'mevedel-models)
  (append '(:max-tokens nil :request-params nil)
          (mevedel-model-resolve-workload 'compaction)))

(defun mevedel--compact-target-policy ()
  "Return the current request's model policy for compaction budgeting."
  (list :backend gptel-backend
        :model gptel-model
        :max-tokens gptel-max-tokens
        :request-params gptel--request-params))

(defun mevedel--compact-policy-usable-tokens (policy)
  "Return usable input context tokens for model POLICY."
  (let ((gptel-backend (plist-get policy :backend))
        (gptel-model (plist-get policy :model))
        (gptel-max-tokens (plist-get policy :max-tokens))
        (gptel--request-params (plist-get policy :request-params)))
    (mevedel--compact-usable-tokens)))

(defun mevedel--compact-policy-threshold-tokens (policy)
  "Return the compaction threshold for model POLICY."
  (mevedel--compact-threshold-tokens
   (mevedel--compact-policy-usable-tokens policy)))

(defun mevedel--compact-summary-request-token-estimate (body system-prompt)
  "Estimate tokens for summary BODY and SYSTEM-PROMPT."
  (with-temp-buffer
    (insert system-prompt "\n\n" body)
    (mevedel--estimate-tokens)))

(defun mevedel--compact-token-usage-count (tokens)
  "Return total prompt/output TOKENS from a gptel token-usage plist."
  (when (listp tokens)
    (+ (or (plist-get tokens :input) 0)
       (or (plist-get tokens :cached) 0)
       (or (plist-get tokens :cache-read) 0)
       (or (plist-get tokens :cache_read) 0)
       (or (plist-get tokens :output) 0))))

(defun mevedel--compact-token-usage-input (tokens)
  "Return input/cached prompt TOKENS from a gptel token-usage plist."
  (when (listp tokens)
    (+ (or (plist-get tokens :input) 0)
       (or (plist-get tokens :cached) 0)
       (or (plist-get tokens :cache-read) 0)
       (or (plist-get tokens :cache_read) 0))))

(defun mevedel--compact-compaction-context-p (info)
  "Return non-nil when INFO belongs to a compaction request."
  (let ((context (and (listp info) (plist-get info :context))))
    (and (listp context)
         (plist-get context :mevedel-compaction))))

(defun mevedel--compact-record-token-baseline (fsm)
  "Record API-reported token usage from FSM for future estimates.

Compaction requests are ignored so the summary request's own token
count never becomes the baseline for the chat buffer."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              ((not (mevedel--compact-compaction-context-p info)))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer))
              (tokens (or (plist-get info :tokens)
                          (plist-get info :tokens-full)))
              (count (mevedel--compact-token-usage-count tokens)))
    (with-current-buffer chat-buffer
      (setq mevedel--known-token-baseline
            (list :tokens count
                  :input-tokens (mevedel--compact-token-usage-input tokens)
                  :output-tokens (or (plist-get tokens :output) 0)
                  :position (copy-marker (point-max)))))))

(defun mevedel--estimate-tokens ()
  "Estimate the number of tokens in the current buffer.
Only counts text not marked with the `gptel' property `ignore' and
excludes file-local variables block."
  (if-let* ((baseline mevedel--known-token-baseline)
            (tokens (plist-get baseline :tokens))
            (position (plist-get baseline :position))
            ((integer-or-marker-p position))
            (pos (if (markerp position) (marker-position position) position))
            ((<= (point-min) pos))
            ((<= pos (point-max))))
      (+ tokens (/ (- (point-max) pos) 4))
    (let ((pos (point-min))
          (total 0)
          (flv-start (mevedel--file-local-variables-start)))
      (while (< pos (point-max))
        (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
               (prop (get-text-property pos 'gptel)))
          (unless (eq prop 'ignore)
            ;; Only count if not in file-local-variables region
            (when (or (null flv-start) (< pos flv-start))
              (setq total (+ total (- (if (and flv-start (> next flv-start))
                                          flv-start
                                        next)
                                      pos)))))
          (setq pos next)))
      (/ total 4))))

(defun mevedel--compact-estimate-buffer-tokens (buffer)
  "Return a fresh token estimate for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let (mevedel--known-token-baseline)
        (mevedel--estimate-tokens)))))

(defun mevedel--compact-estimate-transformed-request-tokens
    (source-buffer prompt-buffer)
  "Estimate a transformed request from SOURCE-BUFFER and PROMPT-BUFFER.

SOURCE-BUFFER may have an API-corrected
`mevedel--known-token-baseline'.  PROMPT-BUFFER is gptel's temporary
prompt buffer after mevedel prompt transforms have run."
  (when (and (buffer-live-p source-buffer)
             (buffer-live-p prompt-buffer))
    (let* ((source-state
            (with-current-buffer source-buffer
              (list :baseline mevedel--known-token-baseline
                    :estimate (mevedel--estimate-tokens))))
           (source-estimate (plist-get source-state :estimate))
           (source-fresh (mevedel--compact-estimate-buffer-tokens
                          source-buffer))
           (prompt-fresh (mevedel--compact-estimate-buffer-tokens
                          prompt-buffer)))
      (cond
       ((not (plist-get source-state :baseline))
        prompt-fresh)
       ((and source-estimate source-fresh prompt-fresh)
        (+ source-estimate
           (max 0 (- prompt-fresh source-fresh))))
       (source-estimate)
       (prompt-fresh)))))

(defun mevedel--compact-string-lengths (&rest values)
  "Return the total length of string VALUES."
  (let ((chars 0))
    (dolist (value values chars)
      (when (stringp value)
        (cl-incf chars (length value))))))

(defun mevedel--compact-image-data-url-payload-start (url)
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

(defun mevedel--compact-image-data-url-estimate (url)
  "Return (CHARS . TOKENS) for image data URL string URL."
  (when-let* ((payload-start
               (mevedel--compact-image-data-url-payload-start url)))
    (cons payload-start mevedel-compact-image-token-estimate)))

(defun mevedel--compact-openai-image-block-estimate (block)
  "Return (CHARS . TOKENS) for OpenAI native image BLOCK."
  (when-let* (((consp block))
              (type (plist-get block :type))
              ((member type '("input_image" "image_url")))
              (image-url (plist-get block :image_url))
              (url (if (stringp image-url)
                       image-url
                     (and (consp image-url)
                          (plist-get image-url :url))))
              (estimate (mevedel--compact-image-data-url-estimate url)))
    (cons (+ (mevedel--compact-string-lengths type)
             (car estimate))
          (cdr estimate))))

(defun mevedel--compact-anthropic-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Anthropic native image BLOCK."
  (when-let* (((consp block))
              ((equal (plist-get block :type) "image"))
              (source (plist-get block :source))
              ((consp source))
              ((equal (plist-get source :type) "base64"))
              (mime (plist-get source :media_type))
              ((and (stringp mime) (string-prefix-p "image/" mime t)))
              ((stringp (plist-get source :data))))
    (cons (mevedel--compact-string-lengths
           (plist-get block :type)
           (plist-get source :type)
           mime)
          mevedel-compact-image-token-estimate)))

(defun mevedel--compact-bedrock-image-block-estimate (block)
  "Return (CHARS . TOKENS) for Bedrock native image BLOCK."
  (when-let* (((consp block))
              (image (plist-get block :image))
              ((consp image))
              (format (plist-get image :format))
              ((stringp format))
              (source (plist-get image :source))
              ((consp source))
              ((stringp (plist-get source :bytes))))
    (cons (mevedel--compact-string-lengths format)
          mevedel-compact-image-token-estimate)))

(defun mevedel--compact-media-block-estimate (block)
  "Return (CHARS . TOKENS) when BLOCK is native media request data."
  (or (mevedel--compact-openai-image-block-estimate block)
      (mevedel--compact-anthropic-image-block-estimate block)
      (mevedel--compact-bedrock-image-block-estimate block)))

(defun mevedel--compact-estimate-data-tokens (data)
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
                        (mevedel--compact-media-block-estimate value)))
                 (progn
                   (cl-incf chars (car estimate))
                   (cl-incf media-tokens (cdr estimate)))
               (walk (car value))
               (walk (cdr value)))))))
      (walk data))
    (+ (/ chars 4) media-tokens)))

(defun mevedel--compact-tool-output-prop-p (prop)
  "Return non-nil if PROP is a gptel tool output span."
  (and (consp prop) (eq (car prop) 'tool)))

(defun mevedel--compact-truncation-marker (omitted)
  "Return the marker inserted when OMITTED tool-output chars are removed."
  (format "\n[mevedel: tool output truncated; omitted %d chars]\n"
          omitted))

(defun mevedel--compact-string-arg-marker (omitted)
  "Return the marker inserted when OMITTED string-argument chars are removed."
  (format "\n[mevedel: string argument truncated; omitted %d chars]"
          omitted))

(defun mevedel--compact-truncate-string-arg (string limit)
  "Return STRING shortened to LIMIT chars, preserving a truncation marker."
  (if (and (integerp limit) (> (length string) limit))
      (concat (substring string 0 limit)
              (mevedel--compact-string-arg-marker
               (- (length string) limit)))
    string))

(defun mevedel--compact-truncate-tool-args (value limit)
  "Return VALUE with nested string arguments shortened to LIMIT chars."
  (cond
   ((stringp value)
    (mevedel--compact-truncate-string-arg value limit))
   ((vectorp value)
    (vconcat (mapcar (lambda (item)
                       (mevedel--compact-truncate-tool-args item limit))
                     value)))
   ((consp value)
    (cons (mevedel--compact-truncate-tool-args (car value) limit)
          (mevedel--compact-truncate-tool-args (cdr value) limit)))
   (t value)))

(defun mevedel--compact-tool-arg-limit (cap)
  "Return the string argument retention limit for tool CAP."
  (max 80 (min 800 (or cap 800))))

(defun mevedel--compact-truncate-tool-header (text limit)
  "Return TEXT with a trailing readable org tool header shortened to LIMIT."
  (if (string-match "#\\+begin_tool[[:space:]]+" text)
      (condition-case nil
          (let* ((form-start (match-end 0))
                 (read-result (read-from-string text form-start))
                 (form (car read-result))
                 (form-end (cdr read-result))
                 (trailing (substring text form-end)))
            (if (and (consp form)
                     (string-match-p "\\`[[:space:]]*\\'" trailing))
                (concat (substring text 0 form-start)
                        (prin1-to-string
                         (mevedel--compact-truncate-tool-args form limit))
                        trailing)
              text))
        (error text))
    text))

(defun mevedel--compact-escape-tool-body-markers (text)
  "Return TEXT with org tool markers escaped for use inside tool bodies."
  (replace-regexp-in-string "^#\\+\\(begin\\|end\\)_tool" "# +\\1_tool" text
                            nil nil))

(defun mevedel--compact-tool-output-close (text)
  "Return the trailing org tool close marker in TEXT, or nil."
  (when (string-match "\n#\\+end_tool[^\n]*\n?\\'" text)
    (cons (match-beginning 0) (match-end 0))))

(defun mevedel--compact-raw-tool-truncation (text cap)
  "Return TEXT truncated to CAP chars with the standard omitted marker."
  (concat (substring text 0 cap)
          (mevedel--compact-truncation-marker (- (length text) cap))))

(defun mevedel--compact-truncate-tool-body (body cap)
  "Return BODY truncated to CAP chars with the standard omitted marker."
  (if (and (integerp cap) (> (length body) cap))
      (concat (mevedel--compact-escape-tool-body-markers
               (substring body 0 cap))
              (mevedel--compact-truncation-marker (- (length body) cap)))
    (mevedel--compact-escape-tool-body-markers body)))

(defun mevedel--compact-structural-tool-span (text cap)
  "Return a structurally safe compacted org tool span for TEXT.
CAP is the maximum retained body size.  Return nil when TEXT is not
parseable as a persisted org tool span."
  (when-let* ((sexp-start (string-match "(\\s-*:name\\_>" text)))
    (condition-case nil
        (let* ((read-result (read-from-string text sexp-start))
               (sexp (car read-result))
               (sexp-end (cdr read-result)))
          (when (and (listp sexp) (stringp (plist-get sexp :name)))
            (let* ((arg-limit (mevedel--compact-tool-arg-limit cap))
                   (safe-sexp
                    (mevedel--compact-truncate-tool-args sexp arg-limit))
                   (prefix (mevedel--compact-truncate-tool-header
                            (substring text 0 sexp-start) arg-limit))
                   (suffix (substring text sexp-end))
                   (close (mevedel--compact-tool-output-close suffix))
                   (body-end (or (car close) (length suffix)))
                   (body (substring suffix 0 body-end))
                   (close-text (and close
                                    (substring suffix (car close) (cdr close))))
                   (trailing (if close
                                 (substring suffix (cdr close))
                               "")))
              (concat prefix
                      (prin1-to-string safe-sexp)
                      (mevedel--compact-truncate-tool-body body cap)
                      close-text
                      trailing))))
      (error nil))))

(defun mevedel--compact-tool-sexp-start (text)
  "Return the readable tool sexp start in TEXT, or nil.
For org tool blocks, only accept the sexp immediately after the
`#+begin_tool' header and whitespace.  For raw tool spans, require the
span itself to start with the sexp, allowing leading whitespace."
  (if (string-prefix-p "#+begin_tool" text)
      (when-let* ((header-end (string-match "\n" text)))
        (let ((pos (1+ header-end)))
          (while (and (< pos (length text))
                      (memq (aref text pos) '(?\s ?\t ?\n)))
            (setq pos (1+ pos)))
          (when (and (string-match "(\\s-*:name\\_>" text pos)
                     (= (match-beginning 0) pos))
            pos)))
    (when (string-match "\\`[ \t\n]*(\\s-*:name\\_>" text)
      (match-beginning 0))))

(defun mevedel--compact-tool-subranges (text)
  "Return parseable tool subranges for compacted org tool TEXT.

The returned plist has `:tool-start', `:tool-end', `:prefix-start',
`:prefix-end', `:suffix-start', and `:suffix-end'.  The tool range starts
at the readable `(:name ...)' sexp and excludes org scaffolding."
  (when-let* ((sexp-start (mevedel--compact-tool-sexp-start text)))
    (condition-case nil
        (let* ((read-result (read-from-string text sexp-start))
               (sexp (car read-result))
               (sexp-end (cdr read-result)))
          (when (and (listp sexp) (stringp (plist-get sexp :name)))
            (let* ((close (string-match "\n#\\+end_tool[^\n]*\n?" text sexp-end))
                   (tool-end (or close (length text)))
                   (suffix-end (if close (match-end 0) (length text))))
              (list :prefix-start 0
                    :prefix-end sexp-start
                    :tool-start sexp-start
                    :tool-end tool-end
                    :suffix-start tool-end
                    :suffix-end suffix-end))))
      (error nil))))

(defun mevedel--compact-propertize-tool-span (text prop no-properties)
  "Return TEXT with PROP restored unless NO-PROPERTIES is non-nil.
When TEXT is an org tool block, restore PROP only on the readable
`(:name ...)' sexp and result body so provider parsers can `read' the
range directly."
  (unless no-properties
    (remove-text-properties 0 (length text) '(gptel nil) text)
    (if-let* ((parts (mevedel--compact-tool-subranges text)))
        (progn
          (add-text-properties (plist-get parts :prefix-start)
                               (plist-get parts :prefix-end)
                               '(gptel ignore)
                               text)
          (add-text-properties (plist-get parts :tool-start)
                               (plist-get parts :tool-end)
                               `(gptel ,prop)
                               text)
          (add-text-properties (plist-get parts :suffix-start)
                               (plist-get parts :suffix-end)
                               '(gptel ignore)
                               text))
      (add-text-properties 0 (length text)
                           (if (or (string-prefix-p "#+begin_tool" text)
                                   (string-match-p "\\`[ \\t\\n]*(" text))
                               '(gptel ignore)
                             `(gptel ,prop))
                           text)))
  text)

(defun mevedel--compact-tool-span-with-output-cap (text prop cap no-properties)
  "Return compacted tool span TEXT with PROP preserved when appropriate.
CAP limits the visible result body.  When NO-PROPERTIES is non-nil, return
plain text."
  (mevedel--compact-propertize-tool-span
   (or (mevedel--compact-structural-tool-span text cap)
       (mevedel--compact-raw-tool-truncation text cap))
   prop no-properties))

(defun mevedel--compact-region-with-tool-output-cap (beg end cap
                                                         &optional no-properties)
  "Return text from BEG to END, truncating each tool output span to CAP.

When NO-PROPERTIES is non-nil, strip text properties from copied text."
  (let ((pos beg)
        (parts nil))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'gptel nil end))
             (prop (get-text-property pos 'gptel))
             (tool-output-p (mevedel--compact-tool-output-prop-p prop))
             (span-len (- next pos))
             (text-fn (if no-properties
                          #'buffer-substring-no-properties
                        #'buffer-substring))
             (text (funcall text-fn pos next)))
        (push (cond
               ((and tool-output-p
                     (integerp cap)
                     (> span-len cap))
                (mevedel--compact-tool-span-with-output-cap
                 (substring-no-properties text) prop cap no-properties))
               ((and tool-output-p (integerp cap))
                (if-let* ((compacted
                           (mevedel--compact-structural-tool-span
                            (substring-no-properties text) cap)))
                    (mevedel--compact-propertize-tool-span
                     compacted prop no-properties)
                  (mevedel--compact-propertize-tool-span
                   (substring-no-properties text) prop no-properties)))
               ((and (integerp cap)
                     (null prop)
                     (< next end)
                     (mevedel--compact-tool-output-prop-p
                      (get-text-property next 'gptel)))
                (mevedel--compact-truncate-tool-header
                 text (mevedel--compact-tool-arg-limit cap)))
               (t text))
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun mevedel--compact-skills-section (session)
  "Return the anchored `Skills Invoked' section body for SESSION."
  (if-let* ((session session)
            (records (mevedel-session-invoked-skills session)))
      (mapconcat
       (lambda (rec)
         (format "- $%s%s (role: %s, origin: %s, turn: %s)"
                 (mevedel-skill-invocation-record-name rec)
                 (let ((args (mevedel-skill-invocation-record-args rec)))
                   (if (and args (not (string-empty-p args)))
                       (concat " " args)
                     ""))
                 (or (mevedel-skill-invocation-record-role rec) "?")
                 (or (mevedel-skill-invocation-record-origin rec) "?")
                 (or (mevedel-skill-invocation-record-turn rec) "?")))
       records "\n")
    "- (none)"))

(defun mevedel--compact-prompt (&optional previous-summary instructions session)
  "Return the anchored compaction system prompt.
PREVIOUS-SUMMARY selects update mode when non-nil.  INSTRUCTIONS are
manual user instructions.  SESSION supplies invoked skill records."
  (require 'mevedel-system)
  (mevedel-system-render-prompt-file
   "prompts/compaction/summary.md"
   `(("MODE_INSTRUCTIONS" .
      ,(if previous-summary
           (concat
            "Update the anchored summary below using the conversation history above.\n"
            "The previous summary is authoritative retained context from older compacted history. "
            "Do NOT replace it with only the recent conversation.\n"
            "Your output must merge BOTH sources: keep still-true details from the previous summary, "
            "remove stale or contradicted details, and add new facts from the recent conversation.\n"
            "If the recent conversation is a separate completed task, preserve the older completed "
            "task under Progress/Critical Context/Relevant Files instead of dropping it.\n\n"
            "<previous-summary>\n" previous-summary "\n</previous-summary>\n")
         "Create a new anchored summary from the conversation history above.\n"))
     ("SKILLS_INVOKED" . ,(mevedel--compact-skills-section session))
     ("ADDITIONAL_INSTRUCTIONS" .
      ,(when (and instructions (not (string-blank-p instructions)))
         (concat "## Additional Instructions\n" instructions "\n\n")))
     ("PREVIOUS_SUMMARY_RULE" .
      ,(when previous-summary
         "- Do not discard previous-summary details merely because they are not repeated in the recent conversation.\n")))))

(defun mevedel--compact-buffer-active-p (buf)
  "Return non-nil if BUF has an active gptel request."
  (cl-find-if
   (lambda (entry)
     (eq (thread-first (cadr entry)
                       (gptel-fsm-info)
                       (plist-get :buffer))
         buf))
   gptel--request-alist))

(defun mevedel--compact-find-boundary ()
  "Find the compaction boundary in the current buffer.
Return the position just after the last response, or nil if no response
exists."
  (let (boundary)
    (dolist (seg (mevedel-transcript-segments (point-min) (point-max)))
      (when (eq (car seg) 'response)
        (setq boundary (caddr seg))))
    boundary))

(defun mevedel--compact-turn-starts-before (limit &optional body-start)
  "Return complete turn start positions before LIMIT, oldest first.

User-authored text after the previous assistant response begins a turn.
Tool-call/result spans between assistant response chunks do not create a
new turn.  BODY-START defaults to the main-session body start."
  (let ((after-response t)
        starts)
    (dolist (seg (mevedel-transcript-segments
                  (or body-start (mevedel--compact-body-start)) limit))
      (let ((seg-end (min (caddr seg) limit)))
        (when (< (cadr seg) seg-end)
          (pcase (car seg)
            ('response
             (setq after-response t))
            ('user
             (when-let* ((prompt-start
                          (and after-response
                               (mevedel-transcript--user-prompt-start
                                (cadr seg) seg-end nil))))
               (push prompt-start starts)
               (setq after-response nil)))))))
    (nreverse starts)))

(defun mevedel--compact-tail-start (limit aggressive &optional body-start)
  "Return tail start before LIMIT, or LIMIT when AGGRESSIVE.
The tail starts after the response preceding the preserved recent turns.
If keeping `mevedel-compact-tail-turns' turns would exceed
`mevedel-compact-tail-budget', older preserved turns are dropped.
BODY-START defaults to the main-session body start."
  (if aggressive
      limit
    (let* ((body-start (or body-start (mevedel--compact-body-start)))
           (starts (mevedel--compact-turn-starts-before limit body-start))
           (count (length starts))
           (max-turns (max 0 mevedel-compact-tail-turns))
           (budget-chars
            (* 4 (round (* mevedel-compact-tail-budget
                           (mevedel--compact-usable-tokens)))))
           (turns (min max-turns count))
           start)
      (cl-labels
          ((start-for (n)
             (if (or (zerop n) (zerop count))
                 limit
               (max body-start
                    (if (<= count n)
                        body-start
                      (nth (- count n) starts))))))
        (setq start (start-for turns))
        (while (and (> turns 1)
                    (> (- limit start) budget-chars))
          (cl-decf turns)
          (setq start (start-for turns))))
      start)))

(defun mevedel--compact-pending-text-from-prompt-buffer ()
  "Return pending request text from the current prompt buffer.

The prompt buffer may already contain expanded mentions and injected
system reminders, so source-buffer positions are not reliable here."
  (when-let* ((start (mevedel--compact-find-boundary)))
    (buffer-substring start (point-max))))

(defun mevedel--compact-prefix-before-pending (pending-text)
  "Return current buffer text before the reattached PENDING-TEXT."
  (let ((boundary (mevedel--compact-find-boundary)))
    (unless boundary
      (when (and pending-text
                 (not (string-empty-p pending-text))
                 (string-suffix-p
                  (substring-no-properties pending-text)
                  (buffer-substring-no-properties (point-min) (point-max))))
        (setq boundary (- (point-max) (length pending-text)))))
    (when boundary
      (buffer-substring (point-min) boundary))))

(defun mevedel--compact-marker-range-live-p (start end)
  "Return non-nil when START and END delimit a live marker range."
  (and (markerp start)
       (markerp end)
       (marker-buffer start)
       (marker-buffer end)
       (<= (marker-position start) (marker-position end))))

(defun mevedel--compact-rebuild-prompt-buffer
    (prompt-buffer source-buffer source-pending-text
                   prompt-history-start prompt-pending-start)
  "Rebuild PROMPT-BUFFER after SOURCE-BUFFER has been compacted.

SOURCE-PENDING-TEXT identifies the pending prompt in SOURCE-BUFFER.
PROMPT-HISTORY-START and PROMPT-PENDING-START delimit the old transcript
span.  Only that span is replaced, so prompt transforms that ran while
compaction was in flight remain in place."
  (let ((compacted-prefix
         (with-current-buffer source-buffer
           (mevedel--compact-prefix-before-pending source-pending-text))))
    (with-current-buffer prompt-buffer
      (let ((inhibit-read-only t))
        (if (and compacted-prefix
                 (mevedel--compact-marker-range-live-p
                  prompt-history-start prompt-pending-start))
            (progn
              (delete-region prompt-history-start prompt-pending-start)
              (goto-char prompt-history-start)
              (insert compacted-prefix))
          (erase-buffer)
          (insert-buffer-substring source-buffer))))))

(defun mevedel--compact-system-reminder-block (body)
  "Return BODY wrapped as a model-visible system reminder block."
  (format "<system-reminder>\n%s\n</system-reminder>" body))

(defun mevedel--compact-insert-current-request-reminder (body)
  "Insert reminder BODY before the pending request in the current buffer."
  (when (and (stringp body) (not (string-empty-p body)))
    (save-excursion
      (goto-char (or (mevedel--compact-find-boundary) (point-min)))
      (let ((start (point)))
        (insert "\n" (mevedel--compact-system-reminder-block body) "\n")
        (remove-text-properties
         start (point)
         '(gptel nil response nil invisible nil front-sticky nil))))))

(defun mevedel--compact-summary-bounds ()
  "Return plist bounds for the leading summary block, or nil.
The plist contains `:begin', `:body-begin', `:body-end' and `:end'."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence--segment-summary-bounds))

(defun mevedel--compact-previous-summary ()
  "Return the leading compaction summary body, or nil."
  (when-let* ((bounds (mevedel--compact-summary-bounds)))
    (require 'mevedel-utilities)
    (mevedel-session-persistence--strip-summary-handoff-prefix
     (string-trim
      (mevedel--strip-hook-audit-blocks
       (buffer-substring-no-properties
        (plist-get bounds :body-begin)
        (plist-get bounds :body-end)))))))

(defun mevedel--compact-body-start ()
  "Return the position after the leading summary block, if present."
  (if-let* ((bounds (mevedel--compact-summary-bounds)))
      (plist-get bounds :end)
    (point-min)))

(defun mevedel--compact-current-persisted-p ()
  "Return non-nil when current buffer can use segment rotation."
  (and (boundp 'mevedel--session)
       mevedel--session
       (mevedel-session-save-path mevedel--session)
       buffer-file-name
       (progn
         (require 'mevedel-session-persistence)
         (string=
          (expand-file-name buffer-file-name)
          (expand-file-name
           (mevedel-session-persistence--segment-path
            (mevedel-session-save-path mevedel--session)
            (mevedel-session-current-segment mevedel--session)))))))

(defun mevedel--compact-auto-eligible-p ()
  "Return non-nil when automatic compaction may run in this buffer."
  (and mevedel-compact-auto
       (not mevedel--compact-auto-disabled)
       (not mevedel--compaction-in-flight)
       (not (bound-and-true-p mevedel-session--read-only-mode))
       (mevedel--compact-current-persisted-p)))

(defun mevedel--compact-auto-ineligible-reason ()
  "Return a short reason automatic compaction cannot run, or nil."
  (cond
   ((not mevedel-compact-auto) "auto-compaction is disabled")
   (mevedel--compact-auto-disabled "auto-compaction is disabled after repeated failures")
   (mevedel--compaction-in-flight "compaction is already in progress")
   ((bound-and-true-p mevedel-session--read-only-mode) "session is read-only")
   ((not (and (boundp 'mevedel--session)
              mevedel--session
              (mevedel-session-save-path mevedel--session)))
    "session is not materialized on disk")
    ((not (mevedel--compact-current-persisted-p))
     "current buffer is not the active persisted segment")))

(defun mevedel--compact-admission (estimate target-policy)
  "Return compaction admission for ESTIMATE and TARGET-POLICY, or nil."
  (let* ((summary-policy (mevedel--compact-workload-policy))
         (target-threshold
          (mevedel--compact-policy-threshold-tokens target-policy))
         (summary-threshold
          (mevedel--compact-policy-threshold-tokens summary-policy)))
    (when (>= estimate (min target-threshold summary-threshold))
      (list :summary-policy summary-policy
            :target-pressure (>= estimate target-threshold)))))

(defun mevedel--compact-should-compact-p (&optional token-estimate)
  "Return automatic compaction admission for TOKEN-ESTIMATE, or nil."
  (let* ((estimate (or token-estimate (mevedel--estimate-tokens)))
         (target-policy (mevedel--compact-target-policy))
         (admission (mevedel--compact-admission estimate target-policy)))
    (cond
     ((not admission) nil)
     ((mevedel--compact-auto-eligible-p)
      admission)
     ((not mevedel--compact-auto-ineligible-warning-shown)
      (setq mevedel--compact-auto-ineligible-warning-shown t)
      (display-warning
       'mevedel
       (format "Auto-compaction skipped: %s"
               (or (mevedel--compact-auto-ineligible-reason)
                   "session is not eligible"))
       :warning)
      nil)
     (t nil))))

(defun mevedel--compact-apply
    (summary &optional tail-text pending-text hook-audits)
  "Rotate the current segment with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are persisted as ignored side-channel records next to the
summary."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (unless (and session (mevedel--compact-current-persisted-p))
      (user-error "Session is not materialized on disk"))
    (remove-text-properties 0 (length summary) '(gptel nil face nil) summary)
    (setq summary (mevedel--compact-append-hook-audits summary hook-audits))
    (let ((path
           (mevedel-session-persistence-rotate-segment
            session (current-buffer) summary
            :tail-text tail-text
            :pending-text pending-text)))
      (when (and path (mevedel-session-goal session))
        (mevedel-session-enqueue-pending-reminder
         session
         "Compaction completed. Current Goal context and artifact pointers were regenerated from persisted state."))
      path)))

(defun mevedel--compact-preserved-tail-turn-count (tail-start limit aggressive)
  "Return the number of complete user-authored requests in retained tail.
TAIL-START and LIMIT delimit the retained tail.  AGGRESSIVE means no
tail is retained."
  (if aggressive
      0
    (length
     (cl-remove-if-not
      (lambda (start) (>= start tail-start))
      (mevedel--compact-turn-starts-before limit)))))

(defun mevedel--compact-omitted-file-references (session preserved-tail-turns)
  "Return touched files likely omitted from SESSION's compacted history.
PRESERVED-TAIL-TURNS is the actual number of complete recent user turns
retained after tail-budget and aggressive-compaction decisions."
  (when-let* ((table (and session (mevedel-session-touched-files session)))
              ((hash-table-p table)))
    (let* ((turn (or (mevedel-session-turn-count session) 0))
           (cutoff (max 0 (- turn (max 0 (or preserved-tail-turns 0)))))
           files)
      (maphash
       (lambda (path interaction)
         (let ((last-turn (or (mevedel-file-interaction-modified-turn
                               interaction)
                              (mevedel-file-interaction-read-turn
                               interaction)
                              0)))
          (when (< last-turn cutoff)
            (push path files))))
      table)
      (sort files #'string<))))

(defun mevedel--compact-file-reference-reminder-body
    (session preserved-tail-turns)
  "Return reminder body for SESSION file references omitted by compaction.
PRESERVED-TAIL-TURNS is the actual count returned by
`mevedel--compact-preserved-tail-turn-count'."
  (when-let* ((files (mevedel--compact-omitted-file-references
                      session preserved-tail-turns)))
    (let* ((limit mevedel-compact-file-reference-reminder-limit)
           (shown (cl-subseq files 0 (min limit (length files))))
           (omitted (- (length files) (length shown))))
      (concat
       "Compaction omitted older transcript content for files you previously read or edited. Re-read any file before relying on exact contents, line numbers, or stale diffs.\n\n"
       (mapconcat (lambda (path) (format "- %s" path)) shown "\n")
       (when (> omitted 0)
         (format "\n- ... %d more file references omitted" omitted))))))

(defun mevedel--compact-queue-file-reference-reminder
    (session preserved-tail-turns)
  "Queue a reminder for SESSION file references omitted by compaction.
Return the queued reminder body, or nil when no reminder was queued.
PRESERVED-TAIL-TURNS is the actual count returned by
`mevedel--compact-preserved-tail-turn-count'."
  (when-let* ((body (mevedel--compact-file-reference-reminder-body
                    session preserved-tail-turns)))
    (mevedel-session-enqueue-pending-reminder session body)
    body))

(defun mevedel--compact-hook-audit-records (decision)
  "Return PreCompact audit records for hook DECISION."
  (mevedel-hooks-context-audit-records decision 'PreCompact 'compact-context))

(defun mevedel--compact-append-hook-audits (summary records)
  "Return SUMMARY followed by ignored PreCompact audit RECORDS."
  (if (and records (stringp summary))
      (progn
        (require 'mevedel-utilities)
         (concat summary
                 (mapconcat #'mevedel--format-hook-audit-record records "")))
    summary))

(defun mevedel--compact-agent-summary-bounds ()
  "Return bounds for the anchored summary in an agent transcript, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin_summary\\b.*$" nil t)
      (let ((begin (match-beginning 0))
            (body-begin (match-end 0)))
        (when (re-search-forward "^#\\+end_summary\\b.*$" nil t)
          (list :begin begin
                :body-begin (1+ body-begin)
                :body-end (match-beginning 0)
                :end (match-end 0)))))))

(defun mevedel--compact-agent-target (invocation)
  "Return the private compaction target for persisted INVOCATION, or nil."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (buffer (mevedel-agent-invocation-buffer invocation))
              ((eq buffer (current-buffer)))
              (session (mevedel-agent-invocation-parent-session invocation))
              (save-path (mevedel-session-save-path session))
              (relative-path
               (mevedel-agent-invocation-transcript-relative-path invocation))
              (canonical-path (expand-file-name relative-path save-path))
              (buffer-path buffer-file-name)
              ((mevedel--same-file-p buffer-path canonical-path))
              ((file-regular-p canonical-path))
              ((not (file-symlink-p canonical-path)))
              ((file-writable-p canonical-path))
              ((file-writable-p (file-name-directory canonical-path)))
              (task-heading
               (save-excursion
                 (goto-char (point-min))
                 (when (re-search-forward "^\\* Agent Task:" nil t)
                   (match-beginning 0))))
              (first-response
               (cl-find-if
                (lambda (segment) (eq (car segment) 'response))
                (mevedel-transcript-segments (point-min) (point-max)))))
    (let* ((summary-bounds (mevedel--compact-agent-summary-bounds))
           (anchor-end (or (plist-get summary-bounds :begin)
                           (cadr first-response))))
      (when (<= task-heading anchor-end)
        (let ((previous-summary
               (when summary-bounds
                 (require 'mevedel-utilities)
                 (mevedel-session-persistence--strip-summary-handoff-prefix
                  (string-trim
                   (mevedel--strip-hook-audit-blocks
                    (buffer-substring-no-properties
                     (plist-get summary-bounds :body-begin)
                     (plist-get summary-bounds :body-end))))))))
          (list :buffer buffer
                :invocation invocation
                :session session
                :workspace (mevedel-session-workspace session)
                :transcript-path canonical-path
                :origin (mevedel-agent-invocation-agent-id invocation)
                :anchor-text (buffer-substring (point-min) anchor-end)
                :body-start (or (plist-get summary-bounds :end) anchor-end)
                :previous-summary previous-summary
                :prompt-session nil
                :eligible-p t
                :apply #'mevedel--compact-agent-apply
                :start #'mevedel--compact-agent-start
                :complete #'mevedel--compact-agent-complete
                :resume #'mevedel--compact-target-resume
                :fail #'mevedel--compact-agent-terminal-failure
                :warn-on-completion nil))))))

(defun mevedel--compact-agent-archive-path (canonical-path)
  "Return the next unused numbered archive for CANONICAL-PATH."
  (let ((stem (if (string-suffix-p ".chat.org" canonical-path)
                  (string-remove-suffix ".chat.org" canonical-path)
                canonical-path)))
    (cl-loop for number from 1
             for path = (format "%s.compact-%04d.chat.org" stem number)
             unless (file-exists-p path) return path)))

(defun mevedel--compact-agent-apply
    (target summary tail-text pending-text hook-audits
            &optional _auto _preserved-tail-turns)
  "Apply agent TARGET compaction with SUMMARY, TAIL-TEXT, and PENDING-TEXT.
HOOK-AUDITS are stored beside SUMMARY.  Return the recovery archive path."
  (let* ((invocation (plist-get target :invocation))
         (canonical-path (plist-get target :transcript-path))
         (archive-path (mevedel--compact-agent-archive-path canonical-path))
         (summary (mevedel--compact-append-hook-audits summary hook-audits)))
    (unless (mevedel-agent-exec--flush-transcript-save invocation)
      (error "Could not persist agent transcript before compaction"))
    (copy-file canonical-path archive-path nil)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (plist-get target :anchor-text))
      (unless (bolp) (insert "\n"))
      (insert (mevedel-session-persistence--summary-block summary))
      (when tail-text (insert tail-text))
      (when pending-text (insert pending-text))
      (set-buffer-modified-p t))
    (unless (mevedel-agent-exec--flush-transcript-save invocation)
      (error "Could not persist compacted agent transcript"))
    archive-path))

(defun mevedel--compact-main-apply
    (target summary tail-text pending-text hook-audits
            auto preserved-tail-turns)
  "Apply main-session TARGET compaction and arrange its file reminder."
  (let ((session (plist-get target :session)))
    (mevedel--compact-apply
     summary tail-text pending-text hook-audits)
    (let ((reminder
           (mevedel--compact-file-reference-reminder-body
            session preserved-tail-turns)))
      (cond
       (auto
        (setq mevedel--compact-current-request-reminder reminder))
       (reminder
        (mevedel-session-enqueue-pending-reminder session reminder))))))

(defun mevedel--compact-main-start (_target)
  "Show main-session compaction progress for TARGET."
  (when-let* ((view-buffer mevedel--view-buffer)
              (_ (buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (mevedel-view--update-spinner "Compacting..."))))

(defun mevedel--compact-agent-start (target)
  "Show agent TARGET compaction progress."
  (mevedel-agent-exec--record-activity
   (plist-get target :invocation)
   '(:type status :summary "Compacting..."))
  (gptel--update-status " Compacting..." 'warning))

(defun mevedel--compact-main-complete (_target auto)
  "Restore main-session display state after compaction.
AUTO is non-nil for automatic compaction."
  (when-let* ((view-buffer mevedel--view-buffer)
              (_ (buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (mevedel-view--full-rerender)
      (unless auto
        (if (fboundp 'mevedel-view--stop-request-progress)
            (mevedel-view--stop-request-progress)
          (mevedel-view--stop-spinner))))))

(defun mevedel--compact-agent-complete (target _auto)
  "Restore ordinary continuation status for agent TARGET."
  (mevedel-agent-exec--record-activity
   (plist-get target :invocation)
   '(:type status :summary "waiting"))
  (gptel--update-status " Calling Agent..." 'font-lock-escape-face))

(defun mevedel--compact-main-target ()
  "Return the private target adapter for the current main-session segment."
  (let ((session mevedel--session))
    (list :buffer (current-buffer)
          :session session
          :workspace (and session (mevedel-session-workspace session))
          :invocation nil
          :transcript-path (and session (mevedel-session-save-path session))
          :origin "main"
          :body-start (mevedel--compact-body-start)
          :previous-summary (mevedel--compact-previous-summary)
          :prompt-session session
          :eligible-p (mevedel--compact-current-persisted-p)
          :apply #'mevedel--compact-main-apply
          :start #'mevedel--compact-main-start
          :complete #'mevedel--compact-main-complete
          :resume #'mevedel--compact-target-resume
          :resume-status #'mevedel--compact-main-resume-status
          :fail #'mevedel--compact-main-failure
          :warn-on-completion t)))

(defun mevedel--compact-target-call (target operation &rest args)
  "Invoke TARGET OPERATION with TARGET followed by ARGS."
  (let ((function (plist-get target operation)))
    (unless (functionp function)
      (error "Compaction target lacks %s operation" operation))
    (apply function target args)))

(cl-defun mevedel--compact-run
    (&key aggressive instructions pending-start callback auto
          admission target)
  "Run compaction in the current chat buffer.
AGGRESSIVE drops the preserved tail.  INSTRUCTIONS are manual summary
instructions.  PENDING-START marks an inserted-but-unsent prompt region.
CALLBACK receives (ERR) when compaction settles.  AUTO marks an
auto-compaction call.  ADMISSION carries the pre-resolved summarizer
policy and whether the active model crossed its own threshold.  TARGET
is the private adapter for a persisted agent transcript; nil selects
the active main-session segment."
  (let* ((chat-buffer (current-buffer))
         (target (or target (mevedel--compact-main-target)))
         (invocation (plist-get target :invocation))
         (session (plist-get target :session))
         (workspace (plist-get target :workspace))
         (tokens-before (mevedel--estimate-tokens))
         (settled nil))
    (cl-labels
        ((finish (finish-err)
           (unless settled
             (setq settled t)
             (when (buffer-live-p chat-buffer)
               (with-current-buffer chat-buffer
                 (setq-local mevedel--compaction-in-flight nil)))
             (when callback (funcall callback finish-err)))))
      (setq mevedel--compact-current-request-reminder nil)
      (when mevedel--compaction-in-flight
        (user-error "Compaction already in progress"))
      (when (bound-and-true-p mevedel-session--read-only-mode)
        (user-error "Session is read-only"))
      (when (and (not pending-start)
                 (mevedel--compact-buffer-active-p chat-buffer))
        (user-error "Cannot compact while a request is active"))
      (unless (plist-get target :eligible-p)
        (user-error "Current buffer is not the active persisted segment"))
      (let* ((limit (or pending-start (mevedel--compact-find-boundary))))
        (unless limit
          (if auto
              (cl-return-from mevedel--compact-run
                (when callback (funcall callback :skip)))
            (user-error "Not enough conversation content to compact")))
        (let* ((body-start (plist-get target :body-start))
               (tail-start (mevedel--compact-tail-start
                            limit aggressive body-start))
               (compact-end (max body-start tail-start))
               (preserved-tail-turns
                (mevedel--compact-preserved-tail-turn-count
                 tail-start limit aggressive))
               (old-content
                (mevedel--compact-region-with-tool-output-cap
                 body-start compact-end
                 mevedel-compact-body-tool-output-max
                 t))
               (tail-text (unless aggressive
                            (mevedel--compact-region-with-tool-output-cap
                             tail-start limit
                             mevedel-compact-tail-tool-output-max)))
               (pending-text (when pending-start
                               (buffer-substring pending-start (point-max))))
               (previous-summary (plist-get target :previous-summary))
               (system-prompt
                (mevedel--compact-prompt previous-summary
                                         instructions
                                         (plist-get target :prompt-session)))
               (policy (or (plist-get admission :summary-policy)
                           (mevedel--compact-workload-policy)))
               (pre-compact-hook-audits nil)
               (attempt 0)
               (max-attempts 3))
          (when (string-blank-p old-content)
            (if auto
                (cl-return-from mevedel--compact-run
                  (when callback
                    (funcall callback
                             (if (plist-get admission :target-pressure)
                                 "No compactable history remains at target pressure"
                               :skip))))
              (user-error "Not enough conversation content to compact")))
	          (require 'mevedel-hooks)
	          (let ((trigger (if auto "auto" "manual")))
            (cl-labels
                ((fail (fail-err retryable &optional ignore-failure-count)
                   (if (and retryable (< attempt max-attempts))
                       (let ((delay (expt 2 (1- attempt))))
                         (message
                          "mevedel: compaction failed, retrying in %ss (%s)"
                          delay fail-err)
                         (run-at-time
                          delay nil
                          (lambda ()
                            (when (buffer-live-p chat-buffer)
                              (with-current-buffer chat-buffer
                                (send-request))))))
                     (unless ignore-failure-count
                       (cl-incf mevedel--compact-failure-count))
                     (display-warning 'mevedel fail-err :warning)
                     (unless auto
                       (when-let* ((vb mevedel--view-buffer)
                                   (_ (buffer-live-p vb)))
                         (with-current-buffer vb
                           (if (fboundp 'mevedel-view--stop-request-progress)
                               (mevedel-view--stop-request-progress)
                             (mevedel-view--stop-spinner)))))
                     (finish fail-err)))
                 (send-request ()
                   (cl-incf attempt)
                   (let ((request-stream gptel-stream)
                         (summary-parts nil)
                         (summary-applied nil))
                     (cl-labels
                         ((apply-summary
                           (summary)
                           (unless summary-applied
                             (setq summary-applied t)
                             (condition-case apply-err
                                 (progn
                                   (mevedel--compact-target-call
                                    target :apply summary tail-text pending-text
                                    pre-compact-hook-audits auto
                                    preserved-tail-turns)
                                   (setq mevedel--known-token-baseline nil)
                                   (setq mevedel--compact-failure-count 0)
                                   (mevedel--compact-target-call
                                    target :complete auto)
                                   (let ((tokens-after
                                          (mevedel--estimate-tokens)))
                                     (message
                                      "mevedel: compaction complete (%dk -> %dk tokens, %d turns preserved)"
                                      (/ tokens-before 1000)
                                      (/ tokens-after 1000)
                                      (if aggressive
                                          0
                                        mevedel-compact-tail-turns))
                                     (mevedel-hooks-run-event
                                      'PostCompact
                                      (mevedel-hooks-event-plist
                                       'PostCompact
                                       session workspace
                                       :trigger trigger
                                       :summary summary
                                       :tokens-before tokens-before
                                       :tokens-after tokens-after
                                       :aggressive aggressive
                                       :origin (plist-get target :origin)
                                       :transcript-path
                                       (plist-get target :transcript-path))
                                      #'ignore
                                      session workspace nil invocation))
                                   (when (and (plist-get
                                              target :warn-on-completion)
                                              mevedel-compact-warn-on-completion
                                              (not mevedel--compact-warning-shown))
                                     (setq mevedel--compact-warning-shown t)
                                     (message
                                      "mevedel: long threads with multiple compactions can reduce model accuracy; consider starting a new session for unrelated work"))
                                   (finish nil))
                               (error
                                (fail (format "%s" apply-err) nil))))))
                       (condition-case request-err
                           (let ((gptel-use-tools nil)
                                 (gptel-tools nil))
                             (gptel-with-preset 'gptel-default
                               (let ((request-fn
                                      (lambda ()
                                        (gptel-request old-content
                                          :system system-prompt
                                          :buffer chat-buffer
                                          :stream request-stream
                                          :transforms nil
                                          :context (list :mevedel-compaction t)
                                          :callback
                                          (lambda (response info)
                                            (with-current-buffer chat-buffer
                                              (pcase response
                                                ('nil
                                                 (fail
                                                  (format "Compaction failed: %s"
                                                          (or (plist-get info :error)
                                                              (plist-get info :status)
                                                              "unknown error"))
                                                  t))
                                                ('abort
                                                 (fail "Compaction aborted" nil))
                                                ((pred stringp)
                                                 (if (plist-get info :stream)
                                                     (push response summary-parts)
                                                   (apply-summary response)))
                                                ('t
                                                 (apply-summary
                                                  (apply #'concat
                                                         (nreverse
                                                          summary-parts)))))))))))
                                 (let ((gptel-backend
                                        (plist-get policy :backend))
                                       (gptel-model
                                        (plist-get policy :model))
                                       (gptel-reasoning-effort
                                        (plist-get policy :effort)))
                                   (funcall request-fn)))))
                         (error
                          (fail (format "%s" request-err) t)))))))
              (setq mevedel--compaction-in-flight t)
              (condition-case hook-err
                  (mevedel-hooks-run-event
                   'PreCompact
                   (mevedel-hooks-event-plist
                    'PreCompact session workspace
                    :trigger trigger
                    :tokens-before tokens-before
                    :aggressive aggressive
                    :instructions instructions
                    :origin (plist-get target :origin)
                    :transcript-path (plist-get target :transcript-path))
                   (lambda (decision)
                     (cond
                      ((and (plist-member decision :continue)
                            (not (plist-get decision :continue)))
                       (finish (or (plist-get decision :stop-reason)
                                   "PreCompact hook stopped compaction")))
                      (t
                       (when-let* ((context
                                    (mevedel-hooks-additional-context-string
                                     decision 'PreCompact)))
                         (setq pre-compact-hook-audits
                               (mevedel--compact-hook-audit-records
                                decision))
                         (setq system-prompt
                               (concat system-prompt "\n\n" context)))
                       (let ((request-tokens
                              (mevedel--compact-summary-request-token-estimate
                               old-content system-prompt))
                             (usable-tokens
                              (mevedel--compact-policy-usable-tokens policy)))
                         (if (> request-tokens usable-tokens)
                             (fail
                              (format
                               "Compaction request (%d tokens) exceeds summarizer usable context (%d tokens)"
                               request-tokens usable-tokens)
                              nil t)
                           (message "mevedel: compacting (%dk -> ...)"
                                    (/ tokens-before 1000))
                           (mevedel--compact-target-call target :start)
                           (require 'gptel)
                           (send-request))))))
                   session workspace nil invocation)
                (error
                 (finish (format "%s" hook-err))
                 (signal (car hook-err) (cdr hook-err)))))))))))

;;;###autoload
(defun mevedel-compact (&optional aggressive instructions)
  "Compact the current mevedel chat buffer.
With prefix argument AGGRESSIVE, compact without preserving a recent
tail.  INSTRUCTIONS is an optional string of manual summary guidance."
  (interactive "P")
  (let* ((chat-buffer
          (cond
           ((and (bound-and-true-p gptel-mode) (bound-and-true-p mevedel--workspace))
            (current-buffer))
           (t (mevedel--active-chat-buffer)))))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (user-error "No mevedel chat buffer found"))
    (with-current-buffer chat-buffer
      (mevedel--compact-run
       :aggressive aggressive
       :instructions instructions))))

(defun mevedel--compact-auto-failure (chat-buffer err)
  "Surface automatic compaction failure ERR for CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (when (>= mevedel--compact-failure-count 3)
        (setq mevedel--compact-auto-disabled t))
      (display-warning
       'mevedel
       (format
        (if mevedel--compact-auto-disabled
            "Auto-compaction disabled after repeated failures; request not sent: %s"
          "Auto-compaction failed; request not sent: %s")
        err)
       :warning)
      (when (fboundp 'gptel--update-status)
        (gptel--update-status " Compaction failed" 'error))
      (when-let* ((vb mevedel--view-buffer)
                  (_ (buffer-live-p vb)))
        (with-current-buffer vb
          (if (fboundp 'mevedel-view--stop-request-progress)
              (mevedel-view--stop-request-progress)
            (mevedel-view--stop-spinner)))))))

(defun mevedel--compact-continuation-wait-p (fsm)
  "Return non-nil when FSM is entering WAIT for a tool continuation."
  (when-let* ((info (and fsm (gptel-fsm-info fsm))))
    (and (not (mevedel--compact-compaction-context-p info))
         (or (eq (car (plist-get info :history)) 'TRET)
             (plist-get info :tool-result)))))

(defun mevedel--compact-rebuild-info-data-from-buffer (fsm chat-buffer)
  "Rebuild realized request data for FSM from CHAT-BUFFER.

The rebuilt data keeps the effective backend, model, and active tool
set already stored on FSM's info plist."
  (let* ((info (gptel-fsm-info fsm))
         (old-data (plist-get info :data))
         (had-dry-run (plist-member info :dry-run))
         (old-dry-run (plist-get info :dry-run))
         (backend (plist-get info :backend))
         (model (plist-get info :model))
         (tools (plist-get info :tools))
         (request-reminder
          (buffer-local-value 'mevedel--compact-current-request-reminder
                              chat-buffer))
         (prompt-buffer nil))
    (condition-case err
        (unwind-protect
            (progn
              (with-current-buffer chat-buffer
                (save-excursion
                  (goto-char (point-max))
                  (let ((mark-active nil))
                    (setq prompt-buffer
                          (gptel--create-prompt-buffer (point))))))
              (with-current-buffer prompt-buffer
                (mevedel--compact-insert-current-request-reminder
                 request-reminder)
                (when backend
                  (setq-local gptel-backend backend))
                (when model
                  (setq-local gptel-model model))
                (when (plist-member info :tools)
                  (setq-local gptel-tools tools)
                  (setq-local gptel-use-tools (and tools t))))
              (plist-put info :data prompt-buffer)
              (plist-put info :dry-run t)
              (gptel--realize-query fsm))
          (when (buffer-live-p prompt-buffer)
            (kill-buffer prompt-buffer))
          (when (buffer-live-p chat-buffer)
            (with-current-buffer chat-buffer
              (setq mevedel--compact-current-request-reminder nil)))
          (if had-dry-run
              (plist-put info :dry-run old-dry-run)
            (cl-remf info :dry-run)))
      (error
       (plist-put info :data old-data)
       (signal (car err) (cdr err))))))

(defun mevedel--compact-main-resume-status (target)
  "Show ordinary request progress after compacting main TARGET."
  (let ((chat-buffer (plist-get target :buffer)))
    (when (buffer-live-p chat-buffer)
      (with-current-buffer chat-buffer
        (when-let* ((view-buffer mevedel--view-buffer)
                    (_ (buffer-live-p view-buffer)))
          (with-current-buffer view-buffer
            (mevedel-view--update-spinner "Thinking...")))))))

(defun mevedel--compact-target-resume (target fsm)
  "Rebuild FSM from compacted TARGET and resume its continuation once."
  (let ((buffer (plist-get target :buffer))
        (info (gptel-fsm-info fsm)))
    (unless (buffer-live-p buffer)
      (error "Compaction target buffer is no longer live"))
    (with-current-buffer buffer
      (when-let* ((marker (plist-get info :position)))
        (set-marker marker (point-max) buffer))
      (when-let* ((status-function (plist-get target :resume-status)))
        (funcall status-function target)))
    (mevedel--compact-rebuild-info-data-from-buffer fsm buffer)
    (gptel--handle-wait fsm)))

(defun mevedel--compact-agent-terminal-failure (_target fsm err)
  "Terminate agent FSM with compaction failure ERR."
  (let ((info (gptel-fsm-info fsm)))
    (plist-put info :status (format "Compaction failed: %s" err))
    (plist-put info :error
               (list :type "compaction_error" :message (format "%s" err)))
    (gptel--fsm-transition fsm 'ERRS)))

(defun mevedel--compact-main-failure (target _fsm err)
  "Surface automatic main TARGET compaction failure ERR."
  (mevedel--compact-auto-failure (plist-get target :buffer) err))

(defun mevedel--compact-handle-target-wait (fsm target admission)
  "Gate FSM continuation through TARGET using precomputed ADMISSION."
  (if (not admission)
      (gptel--handle-wait fsm)
    (let ((pending-start (mevedel--compact-find-boundary)))
      (if (not pending-start)
          (if (plist-get admission :target-pressure)
              (mevedel--compact-target-call
               target :fail fsm
               "No compactable history remains at target pressure")
            (gptel--handle-wait fsm))
        (mevedel--compact-run
         :pending-start pending-start
         :auto t
         :admission admission
         :target target
         :callback
         (lambda (err)
           (cond
            ((eq err :skip)
             (gptel--handle-wait fsm))
            (err
             (mevedel--compact-target-call target :fail fsm err))
            (t
             (condition-case rebuild-err
                 (mevedel--compact-target-call target :resume fsm)
               (error
                (mevedel--compact-target-call
                 target :fail fsm
                 (error-message-string rebuild-err))))))))))))

(defun mevedel--compact-handle-agent-wait (fsm)
  "Run persisted-agent compaction before a continuation request in FSM."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (agent-buffer (and (listp info) (plist-get info :buffer)))
         (invocation
          (and (listp info) (plist-get info :mevedel-agent-invocation))))
    (if (or (not (mevedel--compact-continuation-wait-p fsm))
            (not (buffer-live-p agent-buffer))
            (not (mevedel-agent-invocation-p invocation)))
        (gptel--handle-wait fsm)
      (with-current-buffer agent-buffer
        (let* ((target (mevedel--compact-agent-target invocation))
               (target-policy
                (or (plist-get info :mevedel-compaction-target-policy)
                    (mevedel--compact-target-policy)))
               (estimate
                (mevedel--compact-estimate-data-tokens
                 (plist-get info :data)))
               (admission
                (and target
                     mevedel-compact-auto
                     (not mevedel--compact-auto-disabled)
                     (not mevedel--compaction-in-flight)
                     (mevedel--compact-admission estimate target-policy))))
          (if target
              (mevedel--compact-handle-target-wait fsm target admission)
            (gptel--handle-wait fsm)))))))

(defun mevedel--compact-handle-wait (fsm)
  "Run continuation auto-compaction for FSM before `gptel--handle-wait'."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (chat-buffer (and (listp info) (plist-get info :buffer))))
    (if (or (not (mevedel--compact-continuation-wait-p fsm))
            (not (buffer-live-p chat-buffer)))
        (gptel--handle-wait fsm)
      (with-current-buffer chat-buffer
        (let* ((target-policy
                (or (plist-get info :mevedel-compaction-target-policy)
                    (let ((gptel-backend
                           (or (plist-get info :backend) gptel-backend))
                          (gptel-model
                           (or (plist-get info :model) gptel-model)))
                      (mevedel--compact-target-policy))))
               (token-estimate
                (mevedel--compact-estimate-data-tokens
                 (plist-get info :data)))
               (gptel-backend (plist-get target-policy :backend))
               (gptel-model (plist-get target-policy :model))
               (gptel-max-tokens (plist-get target-policy :max-tokens))
               (gptel--request-params
                (plist-get target-policy :request-params))
               (admission
                (mevedel--compact-should-compact-p token-estimate)))
          (mevedel--compact-handle-target-wait
           fsm (mevedel--compact-main-target) admission))))))

(defun mevedel--compact-transform-auto (continue fsm)
  "Run auto-compaction before request realization.
CONTINUE is gptel's async transform continuation.  FSM is the request
state machine."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (source-buffer (and (listp info) (plist-get info :buffer)))
         (prompt-buffer (current-buffer))
         (effective-backend gptel-backend)
         (effective-model gptel-model)
         (effective-max-tokens gptel-max-tokens)
         (effective-request-params gptel--request-params)
         (context (and (listp info) (plist-get info :context))))
    (if (or (and (listp context) (plist-get context :mevedel-compaction))
            (not (buffer-live-p source-buffer)))
        (funcall continue)
      (with-current-buffer source-buffer
        (let ((target-policy
               (let ((gptel-backend effective-backend)
                     (gptel-model effective-model)
                     (gptel-max-tokens effective-max-tokens)
                     (gptel--request-params effective-request-params))
                 (mevedel--compact-target-policy)))
              (admission
               (unless mevedel--compaction-in-flight
                 (let ((gptel-backend effective-backend)
                       (gptel-model effective-model)
                       (gptel-max-tokens effective-max-tokens)
                       (gptel--request-params effective-request-params))
                   (mevedel--compact-should-compact-p
                    (mevedel--compact-estimate-transformed-request-tokens
                     source-buffer prompt-buffer))))))
          (plist-put info :mevedel-compaction-target-policy target-policy)
          (cond
           (mevedel--compaction-in-flight
            (user-error "Compaction already in progress"))
           ((not admission)
            (funcall continue))
           (t
            (let ((pending-start (mevedel--compact-find-boundary)))
              (if (not pending-start)
                  (if (plist-get admission :target-pressure)
                      (mevedel--compact-auto-failure
                       source-buffer
                       "No compactable history remains at target pressure")
                    (funcall continue))
                (let ((source-pending-text
                       (buffer-substring pending-start (point-max)))
                      (prompt-history-start
                       (when (buffer-live-p prompt-buffer)
                         (with-current-buffer prompt-buffer
                           (copy-marker (point-min) t))))
                      (prompt-pending-start
                       (when (buffer-live-p prompt-buffer)
                         (with-current-buffer prompt-buffer
                           (when-let* ((start (mevedel--compact-find-boundary)))
                             (copy-marker start nil))))))
                  (mevedel--compact-run
                   :pending-start pending-start
                   :auto t
                   :admission admission
                   :callback
                   (lambda (err)
                     (unwind-protect
                         (cond
                          ((eq err :skip)
                           (funcall continue))
                          (err
                           (mevedel--compact-auto-failure source-buffer err))
                          (t
                           (when (and (buffer-live-p prompt-buffer)
                                      (buffer-live-p source-buffer))
                             (with-current-buffer source-buffer
                               (when-let* ((marker (plist-get info :position)))
                                 (set-marker marker (point-max) source-buffer)))
                             (when-let* ((vb mevedel--view-buffer)
                                         (_ (buffer-live-p vb)))
                               (with-current-buffer vb
                                 (mevedel-view--update-spinner "Thinking...")))
                             (mevedel--compact-rebuild-prompt-buffer
                              prompt-buffer source-buffer source-pending-text
                              prompt-history-start prompt-pending-start)
                             (when-let* ((reminder
                                          (buffer-local-value
                                           'mevedel--compact-current-request-reminder
                                           source-buffer)))
                               (with-current-buffer prompt-buffer
                                 (mevedel--compact-insert-current-request-reminder
                                  reminder))
                               (with-current-buffer source-buffer
                                 (setq mevedel--compact-current-request-reminder
                                       nil))))
                           (funcall continue)))
                       (when (markerp prompt-history-start)
                         (set-marker prompt-history-start nil))
                       (when (markerp prompt-pending-start)
                         (set-marker prompt-pending-start nil)))))))))))))))

(provide 'mevedel-compact)
;;; mevedel-compact.el ends here
