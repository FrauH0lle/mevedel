;;; mevedel-context-summary.el -- Model-generated context summaries -*- lexical-binding: t -*-

;;; Commentary:

;; Generates validated continuation, handoff, and journal digest text from
;; frozen, neutrally projected evidence.  Consumers own evidence selection, retries,
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
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel-abort "ext:gptel-request" (buf))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest slots))
(declare-function gptel-request "ext:gptel-request")
(defvar gptel-request--handlers)

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))
(declare-function mevedel-model-supported-efforts "mevedel-models" (model))
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
  "Ordered headings shared by continuation and handoff summaries.")

(defconst mevedel-context-summary--guidance-max 4000
  "Maximum caller-guidance length in characters.")

(defconst mevedel-context-summary--digest-max-bytes 16384
  "Maximum UTF-8 byte size of a journal digest.")

;;;###autoload
(defun mevedel-context-summary-digest-policy (policy)
  "Freeze digest defaults in newly resolved summarization POLICY.
Keep explicit reasoning choices.  When supported, disable unspecified
reasoning so it cannot consume the entire short digest output allowance.
Callers load `mevedel-models' at their request or capture boundary."
  (let ((policy (copy-sequence policy)))
    (unless (plist-get policy :effort)
      (let ((efforts (mevedel-model-supported-efforts (plist-get policy :model))))
        (when-let* ((effort (or (and (memq 'disabled efforts) 'disabled)
                               (and (memq 'none efforts) 'none))))
          (setq policy (plist-put policy :effort effort)))))
    (plist-put policy :max-tokens (min 4000 (or (plist-get policy :max-tokens) 4000)))))

(defun mevedel-context-summary--headings (purpose)
  "Return the required ordered heading names for PURPOSE."
  (if (eq purpose 'digest)
      '("Done" "Learned" "Surprised" "Unfinished")
    (append mevedel-context-summary--common-headings
            (when (eq purpose 'continuation) '("Next Steps")))))

(defun mevedel-context-summary--validate-output (summary purpose)
  "Return trimmed SUMMARY when its headings match PURPOSE exactly."
  (unless (and (stringp summary) (not (string-blank-p summary)))
    (error "Context summary response is empty"))
  (when (eq purpose 'digest)
    (when (> (string-bytes summary) mevedel-context-summary--digest-max-bytes)
      (error "Digest exceeds %d bytes" mevedel-context-summary--digest-max-bytes))
    (let (section bullet)
      (dolist (line (split-string summary "\n"))
        (cond
         ((string-blank-p line))
         ((string-prefix-p "## " line)
          (when (and section (not bullet))
            (error "Digest section has no bullets"))
          (setq section t bullet nil))
         ((and section (string-match-p "\\`- \\S-" line))
          (when (or (eq bullet 'empty)
                    (and bullet (equal line "- none")))
            (error "Digest empty marker must be the section's only bullet"))
          (setq bullet (if (equal line "- none") 'empty t)))
         ((and (eq bullet t) (string-match-p "\\`  +\\S-" line)
               (not (string-match-p "\\`[ \t]*\\(?:```\\|~~~\\)" line))))
         (t (error "Digest contains text outside its bullet lists"))))
      (unless bullet
        (error "Digest section has no bullets"))))
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
  (if (eq purpose 'digest)
      (mevedel-system-render-prompt-file "prompts/context-summary/digest.md")
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
           ""))))))

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

(defun mevedel-context-summary--limit-digest-request (fsm limit)
  "Bound FSM's supported provider output limits to LIMIT before dispatch.
Providers without a token control retain the client's byte and time bounds.
Copy the data because gptel merges shared model and backend parameter lists."
  (let* ((info (gptel-fsm-info fsm))
         (data (copy-tree (plist-get info :data))))
    (dolist (params (list data
                          (plist-get data :generationConfig)
                          (plist-get data :inferenceConfig)
                          (plist-get data :options)))
      (dolist (key '(:max_tokens :max_completion_tokens :max_output_tokens
                     :maxOutputTokens :maxTokens :num_predict))
        (when (plist-member params key)
          (let ((value (plist-get params key)))
            (unless (and (integerp value) (> value 0))
              (error "Digest provider output limit is not a positive integer"))
            (plist-put params key (min limit value))))))
    (plist-put info :data data)))

(cl-defun mevedel-context-summary-generate
    (source purpose callback
            &key session previous-summary focus guidance policy)
  "Generate one context summary from frozen SOURCE for PURPOSE.

PURPOSE is `continuation', `handoff', or `digest'.
CALLBACK receives one plist with
`:outcome' equal to `success', `error', or `aborted'.  Success also carries
`:summary'; errors carry `:error' and `:error-class'.  Available provider usage
is returned as `:input-tokens', `:cached-tokens', and `:output-tokens',
including without SESSION.  Input and cached token counts are exclusive.
The return value is a
zero-argument cancellation thunk.  SESSION is used only for model policy and
telemetry ownership.  PREVIOUS-SUMMARY is valid only for continuation.  FOCUS
and bounded GUIDANCE influence relevance without changing the output contract.
POLICY, when non-nil, is a previously resolved summarization model policy."
  (unless (and (stringp source) (not (string-blank-p source)))
    (user-error "Context summary source must be non-empty text"))
  (unless (memq purpose '(continuation handoff digest))
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
         (output-bytes 0)
         span
         (settle
          (lambda (result &optional info)
            (unless settled
              (setq settled t chunks nil)
              (let* ((tokens (and (listp (plist-get info :tokens)) (plist-get info :tokens)))
                     (usage (list :input-tokens (plist-get tokens :input)
                                  :cached-tokens (plist-get tokens :cached)
                                  :output-tokens (plist-get tokens :output))))
                (when span
                  (apply #'mevedel-telemetry-finish
                         span
                         :outcome (plist-get result :outcome)
                         :error-class (plist-get result :error-class)
                         usage))
                (when (buffer-live-p request-buffer)
                  (kill-buffer request-buffer))
                (with-current-buffer (if (buffer-live-p caller-buffer)
                                         caller-buffer
                                       (current-buffer))
                  (funcall callback
                           (append
                            result
                            (list :backend (plist-get policy :backend))
                            usage
                            (list :model (plist-get policy :model)
                                  :effort (plist-get policy :effort)))))))))
         (provider-callback
          (lambda (response info)
            (pcase response
              ((guard settled))
              (`(reasoning . ,_))
              ('abort
               (funcall settle '(:outcome aborted) info))
              ((and (pred stringp)
                    (guard (plist-get info :stream)))
               (setq output-bytes (+ output-bytes (string-bytes response)))
               (if (and (eq purpose 'digest)
                        (> output-bytes mevedel-context-summary--digest-max-bytes))
                   (progn
                     ;; Settle first: gptel-abort calls this callback again.
                     (funcall settle
                              (list :outcome 'error :error-class 'validation
                                    :error "Digest exceeds its byte limit") info)
                     (ignore-errors (gptel-abort request-buffer)))
                 (push response chunks)))
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
          ;; A frozen digest policy was already bounded at capture; applying
          ;; the same idempotent defaults keeps every digest request clamped.
          (when (eq purpose 'digest)
            (setq policy (mevedel-context-summary-digest-policy policy)))
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
                            ;; Codex OAuth has no server token control. Keep
                            ;; the policy reserve for admission, but do not
                            ;; pass a setting gptel must warn about discarding.
                            gptel-max-tokens
                            (unless (eq (type-of gptel-backend) 'gptel-openai-oauth)
                              (plist-get policy :max-tokens))
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
                 :fsm
                 (if (eq purpose 'digest)
                     (gptel-make-fsm
                      :handlers
                      (cons
                       (list
                        'WAIT
                        (lambda (fsm)
                          (condition-case err
                              (progn
                                (mevedel-context-summary--limit-digest-request
                                 fsm (plist-get policy :max-tokens))
                                (gptel--handle-wait fsm))
                            (error
                             (funcall settle
                                      (list :outcome 'error
                                            :error-class 'provider
                                            :error (error-message-string err)))))))
                       (assq-delete-all 'WAIT
                                        (copy-tree gptel-request--handlers))))
                   (gptel-make-fsm))
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
