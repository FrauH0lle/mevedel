;;; mevedel-context-summary.el -- Model-generated context summaries -*- lexical-binding: t -*-

;;; Commentary:

;; Generates validated continuation and handoff context summaries from frozen,
;; neutrally projected evidence.  Consumers own evidence selection, retries,
;; lifecycle hooks, persistence, and application.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(declare-function gptel-backend-name "ext:gptel" (backend))
(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-system-prompt)
(defvar gptel-tools)
(defvar gptel-track-response)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `gptel-request'
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-request "ext:gptel-request")

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-usable-input-tokens "mevedel-models" (policy))

;; `mevedel-structs'
(declare-function mevedel-session-p "mevedel-structs" (object))
(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))
(autoload 'mevedel-system-render-prompt-file "mevedel-system")

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
  (let ((fenced nil)
        headings)
    ;; Only top-level headings count; a summary may legitimately quote
    ;; Markdown inside a fenced code block.
    (dolist (line (split-string summary "\n"))
      (cond
       ((string-match-p "\\`[ \t]*\\(?:```\\|~~~\\)" line)
        (setq fenced (not fenced)))
       ((and (not fenced) (string-match "\\`## \\(.+\\)\\'" line))
        (push (match-string 1 line) headings))))
    (setq headings (nreverse headings))
    (unless (equal headings (mevedel-context-summary--headings purpose))
      (error "Context summary headings are invalid for %s: %S"
             purpose headings)))
  (string-trim summary))

(defun mevedel-context-summary--prompt (purpose)
  "Return the fixed system prompt for context-summary PURPOSE."
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
consumer-supplied relevance data.  The result carries no text
properties: buffer-lifted inputs can hold gptel spans that gptel's
prompt parser would otherwise interpret as transcript structure."
  (substring-no-properties
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
   "\n")))

(defun mevedel-context-summary--estimated-tokens (system input)
  "Return an upper bound on the tokens exact SYSTEM and INPUT text cost.
A provider counts tokens over UTF-8 bytes, and byte-level encodings never
emit more tokens than bytes, so a non-ASCII byte is charged one token
while ASCII keeps the historic four-characters-per-token ratio.  This
gate promises not to dispatch a request the provider will refuse, so it
has to bound the cost rather than approximate it: counting characters
under-reads CJK several-fold and emoji further still."
  (let* ((chars (+ (length system) (length input)))
         (bytes (+ (string-bytes system) (string-bytes input)))
         ;; Every non-ASCII character costs at least two bytes, so this
         ;; lower-bounds how many of the characters were ASCII.
         (ascii (max 0 (- (* 2 chars) bytes))))
    (+ (/ (+ ascii 5) 4) (- bytes ascii))))

(defun mevedel-context-summary--policy-buffer (session)
  "Return SESSION's live root data buffer for workload resolution.
Without a SESSION the caller owns the policy context, so return the
current buffer."
  (if (not (and session
                (fboundp 'mevedel-session-p)
                (mevedel-session-p session)))
      (current-buffer)
    (or
     (cl-find-if
      (lambda (buffer)
        (and (buffer-live-p buffer)
             (eq session (buffer-local-value 'mevedel--session buffer))
             (not (buffer-local-value 'mevedel--agent-invocation buffer))
             (not (buffer-local-value 'mevedel--data-buffer buffer))))
      (buffer-list))
     (error "Context summary session buffer is unavailable"))))

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
  (let* ((request-buffer (generate-new-buffer " *mevedel-context-summary*"))
         ;; gptel runs response callbacks in its own process buffer, so
         ;; restore the caller's buffer before settling.  Consumers resume
         ;; session-local work (dispatch, buffer-local state) from here.
         (caller-buffer (current-buffer))
         ;; Streaming is a session choice, not this request's: some
         ;; providers reject stream false outright, others cannot
         ;; stream, and the session already holds the working value.
         ;; The callback accepts both delivery shapes.
         (stream (condition-case nil
                     (buffer-local-value
                      'gptel-stream
                      (mevedel-context-summary--policy-buffer session))
                   (error gptel-stream)))
         (settled nil)
         (request-started nil)
         (chunks nil)
         span
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
              (with-current-buffer (if (buffer-live-p caller-buffer)
                                       caller-buffer
                                     (current-buffer))
                (funcall callback
                         (append
                          result
                          (list :backend (plist-get policy :backend)
                                :model (plist-get policy :model)
                                :effort (plist-get policy :effort))))))))
         (provider-callback
          (lambda (response info)
            (pcase response
              (`(reasoning . ,_))
              ('abort
               (funcall settle '(:outcome aborted) info))
              ((and (pred stringp)
                    (guard (plist-get info :stream)))
               (push response chunks))
              ((or 't (pred stringp))
               (let ((text (if (stringp response)
                               response
                             (apply #'concat (nreverse chunks)))))
                 (condition-case err
                     (funcall
                      settle
                      (list :outcome 'success
                            :summary
                            (mevedel-context-summary--validate-output
                             text purpose))
                      info)
                   (error
                    (funcall
                     settle
                     (list :outcome 'error
                           :error (error-message-string err)
                           :error-class 'validation)
                     info)))))
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
        (progn
          (unless policy
            (setq policy
                  (with-current-buffer
                      (mevedel-context-summary--policy-buffer session)
                    ;; Resolver keys must win: `plist-get' returns the
                    ;; first occurrence, so leading defaults would shadow
                    ;; a resolved :max-tokens or :request-params.
                    (append (mevedel-model-resolve-workload 'summarization)
                            '(:max-tokens nil :request-params nil)))))
          (setq span
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
          (let* ((system (mevedel-context-summary--prompt purpose))
                 (input (mevedel-context-summary--input
                         source purpose previous-summary focus guidance))
                 (estimate (mevedel-context-summary--estimated-tokens
                            system input))
                 (usable (mevedel-model-usable-input-tokens policy)))
            (if (> estimate usable)
                (funcall
                 settle
                 (list :outcome 'error
                       :error
                       (format
                        "Context summary request (%d tokens) exceeds usable context (%d tokens)"
                        estimate usable)
                       :error-class 'size))
              ;; gptel snapshots request configuration from the :buffer
              ;; with `buffer-local-value', which falls back to global
              ;; defaults and never sees dynamic let bindings made in a
              ;; buffer that holds these variables buffer-locally.  The
              ;; policy must live on the request buffer itself, or the
              ;; request silently ships the user's global defaults.
              (with-current-buffer request-buffer
                (setq-local gptel-backend (plist-get policy :backend)
                            gptel-model (plist-get policy :model)
                            gptel-reasoning-effort (plist-get policy :effort)
                            gptel-max-tokens (plist-get policy :max-tokens)
                            gptel--request-params
                            (plist-get policy :request-params)
                            gptel-system-prompt system
                            gptel-use-tools nil
                            gptel-tools nil
                            gptel-use-context nil
                            gptel-stream stream
                            ;; The prompt parser must treat the evidence
                            ;; as plain text: with response tracking on,
                            ;; a stray gptel text property makes it
                            ;; `read' arbitrary evidence content as a
                            ;; tool-call plist.
                            gptel-track-response nil)
                (setq request-started t)
                (gptel-request
                 input
                 :system system
                 :buffer request-buffer
                 :stream stream
                 :transforms nil
                 :context
                 (list :mevedel-context-summary t :purpose purpose)
                 :callback provider-callback)))))
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
