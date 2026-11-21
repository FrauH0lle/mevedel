;;; niwa-recovery.el --- Retained operational issues -*- lexical-binding: t -*-

;;; Commentary:
;; Small, credential-free recovery state shared by local and browser views.
;; Execution owners report failures; presentation does not own retry policy.
;;
;; Issue messages contain no credentials.  Readiness and runtime issues may
;; carry host diagnostics, so shared views show other readers only their
;; category (`niwa-recovery-public-message').
;;
;; A blocking issue refuses root requests.  Only owners that also re-check and
;; clear their issue report one: provider readiness ("authentication"), preset
;; restore ("preset") and saved-model restore ("model").  Every other issue is
;; informational and lasts until dismissed or the next root request starts,
;; which is the user's retry.  Retrying a retained agent also clears that
;; agent's previous notice once dispatch succeeds.

;;; Code:

(require 'cl-lib)
(require 'niwa-structs)
(require 'subr-x)

;; `niwa-collaboration'
(declare-function niwa-collaboration-notify-request-changed "niwa-collaboration" (buffer))

;; `niwa-collaboration-recovery'
(declare-function niwa-collaboration-recovery-refresh "niwa-collaboration-recovery" (buffer))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-save-agent-registry "niwa-session-artifacts" (session buffer))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-save-agent-registry "niwa-session-artifacts")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-save-agent-state-soon "niwa-session-persistence" (session))
(autoload 'niwa-session-persistence-save-agent-state-soon "niwa-session-persistence")

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--render-status "niwa-view" (&optional data-buf))
(defvar niwa--view-buffer)

(defun niwa-recovery--changed (session)
  "Show SESSION's changed issues in its view and to collaboration peers."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer)))
    (when (fboundp 'niwa-collaboration-notify-request-changed)
      (niwa-collaboration-notify-request-changed buffer))
    (when (fboundp 'niwa-collaboration-recovery-refresh)
      ;; Presentation must not fail the request path that reported the issue.
      (let ((scope (niwa-diagnostic-scope session)))
        (condition-case err
            (niwa-collaboration-recovery-refresh buffer)
          (error
           (niwa-diagnostic-message
            scope "%s (Recovery publication failed)" (error-message-string err))))))
    (when-let* (((local-variable-p 'niwa--view-buffer buffer))
                (view (buffer-local-value 'niwa--view-buffer buffer))
                ((buffer-live-p view)))
      (condition-case nil
          (with-current-buffer view (niwa-view--render-status buffer))
        (error nil)))))

(defun niwa-recovery-report (session id category message blocking)
  "Retain SESSION issue ID of CATEGORY with MESSAGE and BLOCKING status.
MESSAGE contains no credentials.  Return non-nil when the issues changed."
  (when session
    (let ((issue (list :id id :category (symbol-name category)
                       :message message :blocking (and blocking t)))
          (issues (niwa-session-recovery-issues session)))
      (unless (member issue issues)
        (setf (niwa-session-recovery-issues session)
              (cons issue (cl-remove id issues :key (lambda (row) (plist-get row :id))
                                     :test #'equal)))
        (niwa-recovery--changed session)
        t))))

(defun niwa-recovery-clear (session &rest ids)
  "Clear SESSION's resolved issues IDS.  Return non-nil when one was removed."
  (when session
    (let* ((issues (niwa-session-recovery-issues session))
           (kept (cl-remove-if (lambda (row) (member (plist-get row :id) ids)) issues)))
      (unless (= (length kept) (length issues))
        (setf (niwa-session-recovery-issues session) kept)
        (niwa-recovery--changed session)
        t))))

(defun niwa-recovery-clear-informational (session)
  "Clear SESSION's non-blocking issues as a new root request starts."
  (apply #'niwa-recovery-clear session
         (cl-loop for row in (niwa-session-recovery-issues session)
                  unless (plist-get row :blocking) collect (plist-get row :id))))

(defun niwa-recovery-dismiss (session issue)
  "Dismiss the exact informational ISSUE in SESSION and save the change.
Never dismiss a blocker or a replacement issue with the same id.  Return
non-nil when the notice was removed; retained results and transcripts stay
unchanged.  Session mutation authority is checked at activation time."
  (when (and (memq issue (niwa-session-recovery-issues session))
             (not (plist-get issue :blocking)))
    (niwa-session-artifacts-assert-new-mutation-authority session)
    ;; Admission may consult a remote target.  Revalidate the exact notice
    ;; after it returns rather than clear a replacement by id.
    (when (and (memq issue (niwa-session-recovery-issues session))
               (not (plist-get issue :blocking))
               (niwa-recovery-clear session (plist-get issue :id)))
      (niwa-recovery-save session)
      t)))

(defun niwa-recovery-blocker (session)
  "Return SESSION's first execution-blocking issue, or nil."
  (and (niwa-session-p session)
       (cl-find-if (lambda (issue) (plist-get issue :blocking))
                   (niwa-session-recovery-issues session))))

(defun niwa-recovery-assert-ready (session)
  "Refuse execution while SESSION has an unresolved blocking issue."
  (when-let* ((issue (niwa-recovery-blocker session)))
    (user-error "%s" (plist-get issue :message))))

(defun niwa-recovery-category (message &optional code status)
  "Classify failure MESSAGE with structured provider CODE and HTTP STATUS.
The category labels an issue for display.  Only structured signals identify
an unavailable model: free text such as \"model is unavailable\" also
describes transient overload."
  (let ((text (downcase (or message ""))))
    (cond
     ((or (eql code 401) (equal status "401")
          (string-match-p
           "\\bauth\\(?:entication\\|orization\\)?\\b\\|unauthori[sz]ed\\|\\blog ?in\\b\\|credential\\|token.*expir"
           text))
      'authentication)
     ((or (member code '("model_not_found" "model_not_available"))
          (equal status "404"))
      'model)
     ((string-match-p "\\binstall\\|update.*adapter\\|version.*required" text) 'dependency)
     ((string-match-p "preset\\|reasoning effort" text) 'configuration)
     ((string-match-p "native history\\|transcript was edited\\|another installation" text) 'history)
     (t 'request))))

(defun niwa-recovery-message (category)
  "Return a description of a CATEGORY failure safe for every reader."
  (pcase category
    ((or 'authentication "authentication") "Provider sign-in is required")
    ((or 'model "model") "The selected model is unavailable")
    ((or 'dependency "dependency") "The provider runtime needs attention")
    ((or 'configuration "configuration") "The session configuration needs attention")
    ((or 'history "history") "Native history needs recovery")
    (_ "The last request failed")))

(defun niwa-recovery-public-message (issue)
  "Return ISSUE's message for readers other than the host and owners.
Readiness, runtime and session-naming issues carry host diagnostics, which
can name local paths or providers; other readers see their category instead."
  (pcase (plist-get issue :id)
    ("session-naming" "Automatic session naming failed")
    ((or "authentication" "runtime") (niwa-recovery-message (plist-get issue :category)))
    (_ (plist-get issue :message))))

(defconst niwa-recovery--input-keys
  '(:id :category :input :guest-name :guest-id :guest-paths :guest-invoke
    :guest-skills :guest-role :shared-question :inert-skills
    :dropped-file-grants :queued-at-time :queued-at-turn :queued-at-goal-id
    :state :request-id)
  "Durable input fields; prepared submissions and callbacks are never serialized.")

(defun niwa-recovery-persist-inputs (session &optional steering)
  "Return SESSION's unsubmitted inputs without live preparation state.
STEERING selects undelivered steering, always restored for explicit review."
  (mapcar (lambda (entry)
            (cl-loop for key in niwa-recovery--input-keys
                     when (plist-member entry key)
                     append (list key (if (eq key :input)
                                          (substring-no-properties (plist-get entry key))
                                        (copy-tree (plist-get entry key))))))
          (if steering (niwa-session-pending-steering session)
            (niwa-session-pending-follow-ups session))))

(defun niwa-recovery-save (session)
  "Schedule a coalesced sidecar-only save of SESSION's recovery and input state.
Queued input and pause changes ride the debounced sidecar save; a full
transcript save is never needed for them."
  (when (niwa-session-p session)
    (niwa-session-persistence-save-agent-state-soon session)))

(defun niwa-recovery-save-now (session)
  "Publish SESSION's sidecar now, without touching its transcript.
This is the durable delivery intent written before queued input dispatches.
A session without a committed sidecar has nothing to restore yet."
  (when-let* (((niwa-session-p session))
              ((niwa-session-save-path session))
              (buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer)))
    (niwa-session-artifacts-save-agent-registry session buffer)))

(provide 'niwa-recovery)
;;; niwa-recovery.el ends here
