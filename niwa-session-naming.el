;;; niwa-session-naming.el --- Stable session display names -*- lexical-binding: t -*-

;;; Commentary:

;; Owns metadata-only rename and bounded background titles for automatically
;; named root sessions.  Inference never owns the root conversation request,
;; and it inherits the root buffer's streaming choice so providers that reject
;; a non-streaming request can still answer it.
;;
;; The bounded authored input is retained with an `attempted' state, so
;; `niwa-retry-session-naming' can repeat a failed title explicitly.  A
;; positively unavailable naming provider falls back once to the root
;; session's own provider.  The reply deadline starts after submission, so
;; slow provider startup, bounded by its own lifecycle, does not consume it.
;;
;; Every terminal operation is one `session-naming' telemetry event carrying
;; a categorical outcome, an `:error-class', and -- for provider failures --
;; the provider's status, structured error type, and error code.  Provider
;; message text is never recorded.  A failure also leaves an owner-facing
;; recovery notice naming the provider, stage and cause; shared readers see
;; only its category.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'niwa-structs)
(require 'niwa-engine)
(require 'niwa-models)
(require 'niwa-recovery)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-transport)
(require 'niwa-utilities)

;; `gptel-request'
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-track-response)

;; `niwa-session-publication'
(declare-function niwa-session-publication-discard-rolled-back
                  "niwa-session-publication" (session))

;; `niwa-telemetry'
(declare-function niwa-telemetry-record "niwa-telemetry" (session event &rest props))
(autoload 'niwa-telemetry-record "niwa-telemetry")

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)

(defvar-local niwa-session-naming--cancel nil
  "Cancellation thunk for this root buffer's pending title operation.")

(defvar niwa-session-naming-renamed-hook nil
  "Hook run with the session after a display name has been published.")

(defun niwa-session-naming-cancel ()
  "Cancel the current root buffer's pending title operation."
  (when niwa-session-naming--cancel
    (funcall niwa-session-naming--cancel)))

(defun niwa-session-naming-normalize (name)
  "Return readable single-line NAME, rejecting an empty name."
  (let ((normalized (string-join (split-string (substring-no-properties name)) " ")))
    (when (string-empty-p normalized)
      (user-error "Empty session name"))
    normalized))

(defun niwa-session-naming-rename (session buffer name &optional current-p)
  "Set SESSION's display NAME and update its root BUFFER.
Only metadata and presentation change; storage identity stays fixed.
Publish before announcing the name.  Keep a committed name on a post-commit
failure, but restore in-memory metadata when publication did not commit.
CURRENT-P marks an automatic title and rechecks its operation after I/O.
Without CURRENT-P the name is explicit and disables automatic replacement.
Either kind resolves a retained naming-failure notice."
  (setq name (niwa-session-naming-normalize name))
  (unless (and (niwa-session-persistence-root-data-buffer-p buffer)
               (eq session (buffer-local-value 'niwa--session buffer)))
    (user-error "Session root buffer is unavailable"))
  (niwa-session-artifacts-assert-mutation-authority session buffer)
  (unless (and (buffer-live-p buffer)
               (eq session (buffer-local-value 'niwa--session buffer))
               (or (null current-p) (funcall current-p)))
    (user-error "Session naming operation is no longer current"))
  (when (niwa-session-publication-active-p session)
    (user-error "Session publication is busy"))
  (with-current-buffer buffer (niwa-session-naming-cancel))
  (let ((old-name (niwa-session-name session))
        (old-state (niwa-session-naming-state session))
        (old-input (niwa-session-naming-input session))
        (notice (cl-find "session-naming" (niwa-session-recovery-issues session)
                         :key (lambda (issue) (plist-get issue :id)) :test #'equal))
        (head (plist-get (niwa-session-publication session) :head))
        failure)
    (setf (niwa-session-name session) name
          (niwa-session-naming-state session) (if current-p 'attempted 'explicit))
    (unless current-p (setf (niwa-session-naming-input session) nil))
    (niwa-recovery-clear session "session-naming")
    (condition-case err
        (when (and (niwa-session-save-path session)
                   (niwa-session-codec-portable-authority-p session)
                   (niwa-session-artifacts-artifact-present-p
                    session "session.meta.el" t))
          (niwa-session-artifacts-publish-sidecar-state session buffer))
      (error
       (if (equal head (plist-get (niwa-session-publication session) :head))
           (progn
             (setf (niwa-session-name session) old-name
                   (niwa-session-naming-state session) old-state
                   (niwa-session-naming-input session) old-input)
             (when notice
               (niwa-recovery-report session "session-naming"
                                        (intern (plist-get notice :category))
                                        (plist-get notice :message) nil))
             (when (niwa-session-codec-portable-authority-p session)
               (niwa-session-publication-discard-rolled-back session))
             (signal (car err) (cdr err)))
         (setq failure err))))
    (with-current-buffer buffer
      (rename-buffer (niwa-session-buffer-name
                      name (niwa-session-workspace session)) t)
      (niwa-session-persistence-notify-session-event
       session 'rename (buffer-name)))
    (run-hook-with-args 'niwa-session-naming-renamed-hook session)
    (when failure (signal (car failure) (cdr failure)))))

(defun niwa-session-naming-adopt-explicit (session name)
  "Give SESSION the explicit display NAME; the caller publishes it.
Drop recorded naming input and resolve a retained naming-failure notice."
  (setf (niwa-session-name session) name
        (niwa-session-naming-state session) 'explicit
        (niwa-session-naming-input session) nil)
  (niwa-recovery-clear session "session-naming"))

(defun niwa-session-naming--command-root ()
  "Return (SESSION . ROOT-BUFFER) for a command run in a chat or view buffer."
  (let* ((buffer (if (bound-and-true-p niwa--data-buffer)
                     niwa--data-buffer (current-buffer)))
         (session (and (buffer-live-p buffer)
                       (buffer-local-value 'niwa--session buffer))))
    (unless session (niwa-diagnostic-user-error (niwa-diagnostic-scope buffer) "Not in a niwa chat or view buffer"))
    (cons session buffer)))

;;;###autoload
(defun niwa-rename-session (new-name)
  "Rename the current session to NEW-NAME without changing its identity."
  (interactive "sNew session name: ")
  (pcase-let ((`(,session . ,buffer) (niwa-session-naming--command-root)))
    (condition-case err
        (niwa-session-naming-rename session buffer new-name)
      (user-error (niwa-diagnostic-user-error
                   (niwa-diagnostic-scope session) "%s" (error-message-string err))))
    (message "niwa: session renamed to %s" (niwa-session-name session))))

(defconst niwa-session-naming--reply-limit 4096
  "Maximum characters of a title reply; a longer reply is unusable.")

(defconst niwa-session-naming--response-seconds 30
  "Seconds a started title request may take to reply.
Provider startup is bounded separately by its own lifecycle.")

(defconst niwa-session-naming--unavailable-codes
  '(401 403 "401" "403" "invalid_api_key" "model_not_available" unavailable)
  "Structured failure codes meaning the provider cannot serve this host now.
HTTP 401/403 and provider codes come from gptel; `unavailable' marks a failed
launch check.  An unknown model is configuration, not availability.")

(defconst niwa-session-naming--acp-auth-required -32000
  "ACP's authentication-required error code.
JSON-RPC also uses it for generic server errors, so it classifies only a
failure during startup, before any prompt was sent.")

(defun niwa-session-naming--input (prompt)
  "Return bounded, property-free naming input from authored PROMPT, or nil."
  (when (stringp prompt)
    (let ((text (string-trim (substring-no-properties prompt))))
      (unless (string-empty-p text)
        (string-trim-right
         (substring text 0 (min niwa-session-codec-naming-input-limit (length text))))))))

(defun niwa-session-naming--error-code (info)
  "Return the structured error code of failure INFO, or nil."
  (let ((error-data (plist-get info :error)))
    (or (plist-get info :code)
        (and (listp error-data) (plist-get error-data :code)))))

(defun niwa-session-naming--provider-props (info)
  "Return bounded provider diagnostics from gptel INFO for a naming event.

Only categorical facts travel: the status line gptel reports and the
provider's own structured error type and code.  Message text stays out,
because telemetry is durable and `docs/telemetry.md' admits only
classifications and bounded identifiers there."
  (let ((error-data (plist-get info :error)))
    (list :provider-status (plist-get info :status)
          :provider-error-type (and (listp error-data)
                                    (plist-get error-data :type))
          :provider-error-code (niwa-session-naming--error-code info))))

(defun niwa-session-naming--unavailable-p (info)
  "Return non-nil when failure INFO positively shows an unusable provider.
Only structured codes and HTTP status classify; message text never does."
  (let ((code (niwa-session-naming--error-code info))
        (status (plist-get info :status)))
    (or (and code (member code niwa-session-naming--unavailable-codes) t)
        (and (eql code niwa-session-naming--acp-auth-required)
             (eq 'startup (plist-get info :stage)))
        (and (stringp status)
             (string-match-p "\\`HTTP/[0-9.]+ 40[13]\\b" status)))))

(defun niwa-session-naming--bounded (text)
  "Return TEXT as one line of at most 200 characters."
  (let ((line (string-join (split-string (or text "")) " ")))
    (if (> (length line) 200) (concat (substring line 0 199) "…") line)))

(defun niwa-session-naming--provider-cause (info)
  "Return a credential-free cause for a provider failure described by INFO.
An ACP failure carries niwa's own diagnostic.  An HTTP provider's message
text is never shown, as it may echo credentials; its status and code are."
  (if (and (plist-member info :stage) (stringp (plist-get info :error)))
      (plist-get info :error)
    (let* ((props (niwa-session-naming--provider-props info))
           (facts (delq nil (list (plist-get props :provider-status)
                                  (and (plist-get props :provider-error-code)
                                       (format "code %s" (plist-get props :provider-error-code)))))))
      (concat (if (niwa-session-naming--unavailable-p info)
                  "the provider is unavailable or needs sign-in"
                "the provider reported an error")
              (and facts (format " (%s)" (string-join facts ", ")))))))

(defun niwa-session-naming--notice (failures)
  "Return the owner-facing notice for FAILURES, oldest attempt first.
Each failure is (LABEL STAGE CAUSE)."
  (format "Automatic session naming failed; the existing name is retained. %s. Retry with M-x niwa-retry-session-naming"
          (string-join
           (cl-loop for (label stage cause) in failures
                    for first = t then nil
                    collect (format "%s %s failed %s: %s"
                                    (if first "Naming with" "fallback to")
                                    label
                                    (pcase stage
                                      ('start "before sending")
                                      ('startup "during startup")
                                      (_ "waiting for the reply"))
                                    (niwa-session-naming--bounded cause)))
           "; ")))

(cl-defstruct (niwa-session-naming--candidate
               (:constructor niwa-session-naming--candidate-create)
               (:copier nil))
  "One provider attempt of a title operation."
  backend model effort label buffer canceller timer streamed
  launching   ; inside the engine call, before its canceller is known
  finished    ; the provider request reached a terminal reply
  done)       ; retired; later callbacks and timers are stale

(defun niwa-session-naming--candidate (backend model effort)
  "Return a title candidate for BACKEND, MODEL and EFFORT."
  (niwa-session-naming--candidate-create
   :backend backend :model model :effort effort
   :label (format "%s:%s" (gptel-backend-name backend) (gptel--model-name model))))

(defun niwa-session-naming--fallback (buffer primary)
  "Return the root BUFFER's own provider as a fallback distinct from PRIMARY.
The snapshot keeps the root's effort only when its model supports it; the
root session's provider and preset are never changed."
  (with-current-buffer buffer
    (when (and gptel-backend gptel-model
               (not (and (eq gptel-backend (niwa-session-naming--candidate-backend primary))
                         (eq gptel-model (niwa-session-naming--candidate-model primary)))))
      (niwa-session-naming--candidate
       gptel-backend gptel-model
       (condition-case nil
           (niwa-model-validate-effort gptel-model gptel-reasoning-effort)
         (error nil))))))

(defun niwa-session-naming-consider (session prompt)
  "Consider naming SESSION from its first accepted authored PROMPT.
Call only after accepting model-bound user input in its destination session.
Explicit names are never considered.  Consumed automatic attempts are not
considered again until reset by clear; `niwa-retry-session-naming' retries
one explicitly."
  (let ((buffer (and session (niwa-session-root-buffer session)))
        (input (niwa-session-naming--input prompt)))
    (when (and session (eq (niwa-session-naming-state session) 'pending)
               (not (niwa-session-audit-session session))
               (buffer-live-p buffer) input)
      (setf (niwa-session-naming-state session) 'attempted
            (niwa-session-naming-input session) input)
      (niwa-session-naming--start session buffer input 'automatic))))

;;;###autoload
(defun niwa-retry-session-naming (&optional input)
  "Request a new automatic title for the current session.
Use the authored prompt recorded for its last attempt, or ask for INPUT when
none was recorded.  With a prefix argument, always ask.  A running title
request is replaced.  An explicit name is never replaced automatically."
  (interactive (list (and current-prefix-arg
                          (read-string "Describe this session to name it: "))))
  (pcase-let ((`(,session . ,buffer) (niwa-session-naming--command-root)))
    (unless (and (niwa-session-persistence-root-data-buffer-p buffer)
                 (eq buffer (niwa-session-root-buffer session)))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session root buffer is unavailable"))
    (when (niwa-session-audit-session session)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Transient conversations are not named"))
    (when (eq (niwa-session-naming-state session) 'explicit)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "This session has an explicit name; change it with M-x niwa-rename-session"))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope session)
      (niwa-session-artifacts-assert-mutation-authority session buffer)
      (niwa-session-artifacts-assert-new-mutation-authority session))
    (let ((text (niwa-session-naming--input
                 (or input (niwa-session-naming-input session)
                     (read-string "Describe this session to name it: ")))))
      (unless text (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Session naming needs a nonblank description"))
      (with-current-buffer buffer (niwa-session-naming-cancel))
      (setf (niwa-session-naming-state session) 'attempted
            (niwa-session-naming-input session) text)
      (niwa-session-naming--start session buffer text 'retry)
      (message "niwa: requesting a session title"))))

(defun niwa-session-naming--start (session buffer input trigger)
  "Start SESSION's background title operation from authored INPUT.
BUFFER is SESSION's root buffer.  TRIGGER, `automatic' or `retry', is
recorded with the operation's one terminal telemetry event.

The configured `naming' workload runs first.  When it is positively
unavailable, the root buffer's own distinct provider runs once instead.
Each provider's reply deadline starts once its request is submitted."
  (with-current-buffer buffer
    (add-hook 'kill-buffer-hook #'niwa-session-naming-cancel nil t)
    (let ((key (list 'session-naming buffer))
          (scope (niwa-diagnostic-scope session))
          (segment (or (niwa-session-current-segment session) 1))
          (path (niwa-session-working-directory session))
          ;; Streaming is a session choice, not this request's: providers
          ;; reject a non-streaming request outright, and the root buffer
          ;; already holds the working value.
          (stream (buffer-local-value 'gptel-stream buffer))
          start-timer settled cancel reported current fallback fallback-used failures)
      (cl-labels
          ((active-p ()
             (and (not settled) (buffer-live-p buffer)
                  (eq session (buffer-local-value 'niwa--session buffer))
                  (equal segment (or (niwa-session-current-segment session) 1))
                  (eq cancel (buffer-local-value 'niwa-session-naming--cancel buffer))))
           ;; The rename path settles through the cancel thunk before it
           ;; succeeds, so settlement and observation stay separate: this
           ;; records the operation's one terminal event whoever settles it.
           (report (outcome properties)
             (unless reported
               (setq reported t)
               (when (memq outcome '(provider-error timeout error invalid-response))
                 (niwa-recovery-report session "session-naming" 'request
                                          (niwa-session-naming--notice
                                           (reverse failures))
                                          nil))
               (apply #'niwa-telemetry-record
                      session 'session-naming
                      :outcome outcome :trigger trigger
                      (append (and fallback-used '(:fallback t)) properties))))
           (release (candidate)
             ;; Abort an unfinished request through its own canceller, then
             ;; drop the private buffer.  A late `abort' callback is stale.
             (let ((canceller (niwa-session-naming--candidate-canceller candidate)))
               (when (and canceller
                          (not (niwa-session-naming--candidate-finished candidate)))
                 (setf (niwa-session-naming--candidate-finished candidate) t)
                 (condition-case-unless-debug err
                     (funcall canceller)
                   (error (niwa-diagnostic-message
                           scope "Title request cancellation failed: %S" err)))))
             (let ((request (niwa-session-naming--candidate-buffer candidate)))
               (when (buffer-live-p request) (kill-buffer request))))
           (retire (candidate)
             (unless (niwa-session-naming--candidate-done candidate)
               (setf (niwa-session-naming--candidate-done candidate) t)
               (stop-timer candidate)
               ;; An engine still inside its call has no canceller yet; the
               ;; launch releases the request once the call returns.
               (unless (niwa-session-naming--candidate-launching candidate)
                 (release candidate))))
           (stop-timer (candidate)
             (when-let* ((timer (niwa-session-naming--candidate-timer candidate)))
               (cancel-timer timer)
               (setf (niwa-session-naming--candidate-timer candidate) nil)))
           (finish (&optional outcome properties)
             (unless settled
               (setq settled t)
               (when start-timer (cancel-timer start-timer))
               (when current (retire current))
               (niwa-transport-cancel-pending key)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (eq cancel niwa-session-naming--cancel)
                     (setq niwa-session-naming--cancel nil))))
               (when outcome (report outcome properties))))
           (live-p (candidate)
             (and (eq candidate current) (active-p)
                  (not (niwa-session-naming--candidate-done candidate))))
           (fail (candidate outcome properties stage cause)
             (when (live-p candidate)
               (retire candidate)
               (push (list (niwa-session-naming--candidate-label candidate) stage cause)
                     failures)
               (if (and fallback (not fallback-used)
                        (eq 'unavailable (plist-get properties :error-class)))
                   (progn (setq fallback-used t)
                          (launch fallback))
                 (finish outcome (append (list :stage stage) properties)))))
           (apply-title (response)
             (when (active-p)
               (let (title)
                 (condition-case err
                     (let ((plain (string-trim response "[ \t\n\r\"`]+" "[ \t\n\r\"`]+")))
                       (setq title (niwa-session-naming-normalize plain)
                             title (string-join (seq-take (split-string title) 4) " ")
                             title (string-trim-right
                                    (substring title 0 (min 60 (length title)))))
                       (niwa-session-naming-rename session buffer title #'active-p)
                       (finish)
                       (report 'renamed nil))
                   (error
                    (finish)
                    (if (and title (equal title (niwa-session-name session)))
                        ;; Published; only later presentation failed.  The
                        ;; rename keeps a committed name, and so does the report.
                        (report 'renamed nil)
                      (when current
                        (push (list (niwa-session-naming--candidate-label current)
                                    'response (error-message-string err))
                              failures))
                      (report 'error (list :error-class (car-safe err)))))))))
           (complete (candidate text)
             ;; The reply arrived: its deadline no longer applies, even while
             ;; the title waits for publication.
             (stop-timer candidate)
             (if (or (null text) (> (length text) niwa-session-naming--reply-limit))
                 (unusable candidate)
               (dispatch (lambda () (apply-title text)))))
           (unusable (candidate)
             (fail candidate 'invalid-response '(:error-class validation)
                   'response "the reply was not a usable title"))
           (receive (candidate response info)
             (when (live-p candidate)
               (let ((stage (or (plist-get info :stage)
                                (if (niwa-session-naming--candidate-timer candidate)
                                    'response 'startup))))
                 (cond
                  ;; Streamed delivery: chunks arrive separately, and the
                  ;; stream ends with `t'.  Bound what is retained.
                  ((and (stringp response) (plist-get info :stream))
                   (let ((streamed (concat (niwa-session-naming--candidate-streamed candidate)
                                           response)))
                     (setf (niwa-session-naming--candidate-streamed candidate) streamed)
                     (when (> (length streamed) niwa-session-naming--reply-limit)
                       (unusable candidate))))
                  ((stringp response)
                   (setf (niwa-session-naming--candidate-finished candidate) t)
                   (complete candidate response))
                  ((eq response t)
                   (setf (niwa-session-naming--candidate-finished candidate) t)
                   (let ((plain (niwa-session-naming--candidate-streamed candidate)))
                     (setf (niwa-session-naming--candidate-streamed candidate) nil)
                     (complete candidate plain)))
                  ((eq response 'abort)
                   (setf (niwa-session-naming--candidate-finished candidate) t)
                   (fail candidate 'aborted '(:error-class aborted)
                         stage "the request was aborted"))
                  ((null response)
                   (setf (niwa-session-naming--candidate-finished candidate) t)
                   (fail candidate 'provider-error
                         (append (list :error-class
                                       (if (niwa-session-naming--unavailable-p info)
                                           'unavailable 'provider))
                                 (niwa-session-naming--provider-props info))
                         stage (niwa-session-naming--provider-cause info)))))))
           (ready (candidate)
             ;; Submission happened: the reply deadline starts now, once.
             (when (and (live-p candidate)
                        (not (niwa-session-naming--candidate-finished candidate))
                        (not (niwa-session-naming--candidate-timer candidate)))
               (let (timer)
                 (setq timer
                       (run-at-time
                        niwa-session-naming--response-seconds nil
                        (lambda ()
                          ;; A cancelled timer can still fire once.
                          (when (eq timer (niwa-session-naming--candidate-timer candidate))
                            (fail candidate 'timeout '(:error-class timeout) 'response
                                  (format "no reply within %d seconds"
                                          niwa-session-naming--response-seconds))))))
                 (setf (niwa-session-naming--candidate-timer candidate) timer))))
           (launch (candidate)
             (setq current candidate)
             (let ((request (generate-new-buffer " *niwa-title*")))
               (setf (niwa-session-naming--candidate-buffer candidate) request)
               (condition-case err
                   (let ((reason
                          (with-current-buffer request
                            (setq-local
                             gptel-backend (niwa-session-naming--candidate-backend candidate)
                             gptel-model (niwa-session-naming--candidate-model candidate)
                             gptel-reasoning-effort (niwa-session-naming--candidate-effort candidate)
                             gptel-system-prompt
                             "Generate a descriptive session title for the user's request. Use the user's language, 1-4 descriptive words and at most 60 characters. Return only the title, without quotes or Markdown. Do not answer or carry out the request."
                             gptel-use-tools nil gptel-tools nil
                             gptel-use-context nil gptel-track-response nil
                             gptel-stream stream gptel-max-tokens nil)
                            (niwa-engine-text-unavailable gptel-backend))))
                     (if reason
                         (fail candidate 'provider-error '(:error-class unavailable)
                               'start reason)
                       (setf (niwa-session-naming--candidate-launching candidate) t)
                       (let ((canceller
                              (unwind-protect
                                  (with-current-buffer request
                                    (niwa-engine-request-text
                                     gptel-backend input gptel-system-prompt
                                     (lambda (response info) (receive candidate response info))
                                     stream
                                     (list :niwa-telemetry-session session
                                           :niwa-telemetry-workload 'naming)
                                     (lambda () (ready candidate))))
                                (setf (niwa-session-naming--candidate-launching candidate) nil))))
                         (setf (niwa-session-naming--candidate-canceller candidate) canceller)
                         ;; Retired during the call: cancel now that we can.
                         (when (niwa-session-naming--candidate-done candidate)
                           (release candidate)))))
                 (error
                  (if (niwa-session-naming--candidate-done candidate)
                      (release candidate)
                    (fail candidate 'error (list :error-class (car-safe err))
                          'start (error-message-string err)))))))
           (start ()
             (when (active-p)
               (condition-case err
                   (let* ((policy (with-current-buffer buffer
                                    (niwa-model-resolve-workload 'naming)))
                          (primary (niwa-session-naming--candidate
                                    (plist-get policy :backend) (plist-get policy :model)
                                    (plist-get policy :effort))))
                     (setq fallback (niwa-session-naming--fallback buffer primary))
                     ;; Persist the consumed attempt and its input with the
                     ;; accepted prompt, so resume cannot turn a failed request
                     ;; into a retry and an explicit retry has its input.
                     (with-current-buffer buffer
                       (niwa-session-artifacts-save session buffer))
                     (when (active-p) (launch primary)))
                 (error
                  (push (list "the configured naming model" 'start (error-message-string err))
                        failures)
                  (finish)
                  (report 'error (list :error-class (car-safe err)))))))
           (dispatch (function)
             (when (active-p)
               (unless (niwa-transport-run-when-idle
                        key path
                        (lambda ()
                          (when (active-p)
                            (if (niwa-session-publication-active-p session)
                                (setq start-timer
                                      (run-at-time
                                       niwa-transport-retry-seconds nil
                                       (lambda () (dispatch function))))
                              (funcall function))))
                        cancel)
                 (finish 'cancelled '(:error-class cancelled))))))
        (setq cancel (lambda () (finish))
              niwa-session-naming--cancel cancel
              start-timer
              (run-at-time
               0 nil
               (lambda ()
                 (dispatch #'start))))))))

(provide 'niwa-session-naming)
;;; niwa-session-naming.el ends here
