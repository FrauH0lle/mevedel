;;; mevedel-context-summary.el --- Model-generated context summaries -*- lexical-binding: t; -*-

;;; Commentary:

;; Generates validated continuation and handoff context summaries from frozen,
;; neutrally projected evidence.  Consumers own evidence selection, retries,
;; lifecycle hooks, persistence, and application.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel))

;; `gptel'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-backend-name "ext:gptel" (backend))
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)

;; `gptel-request'
(declare-function gptel--merge-plists "ext:gptel-request" (&rest plists))
(declare-function gptel--model-request-params "ext:gptel-request" (model))
(declare-function gptel-backend-request-params "ext:gptel-request" (backend))
(declare-function gptel-request "ext:gptel-request")

;; `mevedel-compact'
(defvar mevedel-compact-context-limit)
(defvar mevedel-compact-reserve-tokens)

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-finish "mevedel-telemetry"
                  (span &rest props))
(declare-function mevedel-telemetry-start "mevedel-telemetry"
                  (session event &rest props))


(defconst mevedel-context-summary--common-headings
  '("Scope"
    "Constraints & Preferences"
    "Work & Evidence"
    "Key Decisions"
    "Open Questions & Risks"
    "Critical Context"
    "Relevant Files"
    "Skills Invoked")
  "Ordered headings shared by every context-summary purpose.")

(defconst mevedel-context-summary--guidance-max 4000
  "Maximum caller-guidance length in characters.")

(defun mevedel-context-summary--headings (purpose)
  "Return the required ordered heading names for PURPOSE."
  (append mevedel-context-summary--common-headings
          (when (eq purpose 'continuation) '("Next Steps"))))

(defun mevedel-context-summary--validate-output (summary purpose)
  "Return trimmed SUMMARY when its headings match PURPOSE exactly."
  (unless (and (stringp summary) (not (string-blank-p summary)))
    (error "Context summary response is empty"))
  (let ((start 0)
        headings)
    (while (string-match "^## \\(.+\\)$" summary start)
      (push (match-string 1 summary) headings)
      (setq start (match-end 0)))
    (setq headings (nreverse headings))
    (unless (equal headings (mevedel-context-summary--headings purpose))
      (error "Context summary headings are invalid for %s: %S"
             purpose headings)))
  (string-trim summary))

(defun mevedel-context-summary--prompt (purpose)
  "Return the fixed system prompt for context-summary PURPOSE."
  (require 'mevedel-system)
  (mevedel-system-render-prompt-file
   "prompts/context-summary/summary.md"
   `(("PURPOSE_RULE" .
      ,(if (eq purpose 'continuation)
           (concat
            "This is a continuation context summary. Preserve unresolved "
            "work as actionable context and emit the final Next Steps section.")
         (concat
          "This is a handoff context summary. Filter evidence for the "
          "separately supplied focus task. Do not restate that task, assign "
          "work, or emit Next Steps.")))
     ("NEXT_STEPS_STRUCTURE" .
      ,(if (eq purpose 'continuation)
           "\n## Next Steps\n- [ordered next actions or \"(none)\"]"
         "")))))

(defun mevedel-context-summary--input
    (source purpose previous-summary focus guidance)
  "Return model input for SOURCE and summary PURPOSE.
PREVIOUS-SUMMARY is retained continuation state.  FOCUS and GUIDANCE are
consumer-supplied relevance data."
  (mapconcat
   #'identity
   (delq
    nil
    (list
     (format "Context-summary purpose: %s" purpose)
     (and focus
          (concat "\n--- focus data (do not reproduce) ---\n" focus
                  "\n--- end focus data ---"))
     (and previous-summary
          (concat "\n--- authoritative previous continuation summary ---\n"
                  previous-summary
                  "\n--- end previous continuation summary ---"))
     (and guidance
          (concat "\n--- bounded caller guidance ---\n" guidance
                  "\n--- end caller guidance ---"))
     (concat "\n--- frozen untrusted evidence ---\n" source
             "\n--- end frozen untrusted evidence ---")))
   "\n"))

(defun mevedel-context-summary--model-max-output-tokens (policy)
  "Return POLICY's configured maximum output token count, or zero."
  (let ((gptel-backend (plist-get policy :backend))
        (gptel-model (plist-get policy :model))
        (gptel-max-tokens (plist-get policy :max-tokens))
        (gptel--request-params (plist-get policy :request-params)))
    (or gptel-max-tokens
        (when (and gptel-backend
                   (fboundp 'gptel--merge-plists)
                   (fboundp 'gptel-backend-request-params)
                   (fboundp 'gptel--model-request-params))
          (let ((params
                 (gptel--merge-plists
                  gptel--request-params
                  (gptel-backend-request-params gptel-backend)
                  (gptel--model-request-params gptel-model))))
            (or (plist-get params :max_tokens)
                (plist-get params :maxOutputTokens)
                (plist-get params :max_output_tokens)
                (plist-get params :num_predict))))
        0)))

(defun mevedel-context-summary--usable-tokens (policy)
  "Return usable input tokens for summarization model POLICY."
  (let* ((model (plist-get policy :model))
         (context
          (or (when-let* ((thousands (and model
                                          (get model :context-window))))
                (round (* thousands 1000)))
              (and (boundp 'mevedel-compact-context-limit)
                   mevedel-compact-context-limit)
              128000))
         (reserve
          (max (if (boundp 'mevedel-compact-reserve-tokens)
                   mevedel-compact-reserve-tokens
                 20000)
               (mevedel-context-summary--model-max-output-tokens policy))))
    (max 1 (- context (min reserve (max 1 (/ context 2)))))))

(defun mevedel-context-summary--estimated-tokens (system input)
  "Return a conservative token estimate for exact SYSTEM and INPUT text."
  (/ (+ (length system) (length input) 5) 4))

(cl-defun mevedel-context-summary-generate
    (source purpose callback
            &key session previous-summary focus guidance policy)
  "Generate one context summary from frozen SOURCE for PURPOSE.

PURPOSE is `continuation' or `handoff'.  CALLBACK receives one plist with
`:outcome' equal to `success', `error', or `aborted'.  Success also carries
`:summary'; errors carry `:error' and `:error-class'.  The return value is a
zero-argument cancellation thunk.  SESSION is used only for model policy and
telemetry ownership.  PREVIOUS-SUMMARY is valid only for continuation.  FOCUS
and bounded GUIDANCE influence relevance without changing the output contract.
POLICY, when non-nil, is a previously resolved summarization model policy."
  (unless (and (stringp source) (not (string-blank-p source)))
    (user-error "Context summary source must be non-empty text"))
  (unless (memq purpose '(continuation handoff))
    (user-error "Unknown context summary purpose: %S" purpose))
  (unless (functionp callback)
    (error "Context summary callback must be a function"))
  (when (and previous-summary (not (eq purpose 'continuation)))
    (user-error "Previous summary is valid only for continuation"))
  (dolist (entry `((,previous-summary . "Previous summary")
                   (,focus . "Focus")
                   (,guidance . "Guidance")))
    (unless (or (null (car entry)) (stringp (car entry)))
      (user-error "%s must be text" (cdr entry))))
  (when (and guidance (> (length guidance)
                         mevedel-context-summary--guidance-max))
    (user-error "Context summary guidance exceeds %d characters"
                mevedel-context-summary--guidance-max))
  (require 'gptel)
  (require 'mevedel-models)
  (let* ((policy
          (or policy
              (append '(:max-tokens nil :request-params nil)
                      (mevedel-model-resolve-workload 'summarization))))
         (system (mevedel-context-summary--prompt purpose))
         (input (mevedel-context-summary--input
                 source purpose previous-summary focus guidance))
         (request-buffer (generate-new-buffer " *mevedel-context-summary*"))
         (settled nil)
         (request-started nil)
         (span
          (and session
               (fboundp 'mevedel-telemetry-start)
               (mevedel-telemetry-start
                session 'context-summary-request
                :purpose purpose
                :backend
                (when-let* ((backend (plist-get policy :backend)))
                  (or (ignore-errors (gptel-backend-name backend))
                      (format "%s" backend)))
                :model (plist-get policy :model)
                :effort (plist-get policy :effort))))
         (settle
          (lambda (result &optional info)
            (unless settled
              (setq settled t)
              (when span
                (mevedel-telemetry-finish
                 span
                 :outcome (plist-get result :outcome)
                 :error-class (plist-get result :error-class)
                 :input-tokens
                 (and (listp (plist-get info :tokens))
                      (plist-get (plist-get info :tokens) :input))
                 :output-tokens
                 (and (listp (plist-get info :tokens))
                      (plist-get (plist-get info :tokens) :output))))
              (when (buffer-live-p request-buffer)
                (kill-buffer request-buffer))
              (funcall callback
                       (append
                        result
                        (list :backend (plist-get policy :backend)
                              :model (plist-get policy :model)
                              :effort (plist-get policy :effort)))))))
         (provider-callback
          (lambda (response info)
            (pcase response
              ('abort
               (funcall settle '(:outcome aborted) info))
              ((pred stringp)
               (condition-case err
                   (funcall
                    settle
                    (list :outcome 'success
                          :summary
                          (mevedel-context-summary--validate-output
                           response purpose))
                    info)
                 (error
                  (funcall
                   settle
                   (list :outcome 'error
                         :error (error-message-string err)
                         :error-class 'validation)
                   info))))
              (_
               (funcall
                settle
                (list :outcome 'error
                      :error
                      (format "Context summary request failed: %s"
                              (or (plist-get info :error)
                                  (plist-get info :status)
                                  "unknown error"))
                      :error-class 'provider)
                info))))))
    (condition-case err
        (let ((estimate (mevedel-context-summary--estimated-tokens
                         system input))
              (usable (mevedel-context-summary--usable-tokens policy)))
          (if (> estimate usable)
              (funcall
               settle
               (list :outcome 'error
                     :error
                     (format
                      "Context summary request (%d tokens) exceeds usable context (%d tokens)"
                      estimate usable)
                     :error-class 'size))
            (let ((gptel-use-tools nil)
                  (gptel-tools nil))
              (gptel-with-preset 'gptel-default
                (let ((gptel-backend (plist-get policy :backend))
                      (gptel-model (plist-get policy :model))
                      (gptel-reasoning-effort (plist-get policy :effort))
                      (gptel-max-tokens (plist-get policy :max-tokens))
                      (gptel--request-params
                       (plist-get policy :request-params))
                      (gptel-stream nil))
                  (setq request-started t)
                  (gptel-request
                   input
                   :system system
                   :buffer request-buffer
                   :stream nil
                   :transforms nil
                   :context
                   (list :mevedel-context-summary t :purpose purpose)
                   :callback provider-callback))))))
      (error
       (funcall
        settle
        (list :outcome 'error
              :error (error-message-string err)
              :error-class (car-safe err)))))
    (lambda ()
      (unless settled
        (when (and request-started (buffer-live-p request-buffer))
          (ignore-errors (gptel-abort request-buffer)))
        (funcall settle '(:outcome aborted))))))

(provide 'mevedel-context-summary)

;;; mevedel-context-summary.el ends here
