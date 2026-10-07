;;; mevedel-recovery.el --- Retained operational issues -*- lexical-binding: t -*-

;;; Commentary:
;; Small, credential-free recovery state shared by local and browser views.
;; Execution owners report failures; presentation does not own retry policy.
;;
;; Issue messages contain no credentials.  Readiness and runtime issues may
;; carry host diagnostics, so shared views show other readers only their
;; category (`mevedel-recovery-public-message').
;;
;; A blocking issue refuses root requests.  Only owners that also re-check and
;; clear their issue report one: provider readiness ("authentication"), preset
;; restore ("preset") and saved-model restore ("model").  Every other issue is
;; informational and lasts until the next root request starts, which is the
;; user's retry.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)
(require 'subr-x)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration-notify-request-changed "mevedel-collaboration" (buffer))

;; `mevedel-collaboration-recovery'
(declare-function mevedel-collaboration-recovery-refresh "mevedel-collaboration-recovery" (buffer))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save-agent-registry "mevedel-session-artifacts" (session buffer))
(autoload 'mevedel-session-artifacts-save-agent-registry "mevedel-session-artifacts")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save-agent-state-soon "mevedel-session-persistence" (session))
(autoload 'mevedel-session-persistence-save-agent-state-soon "mevedel-session-persistence")

;; `mevedel-view'
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(defvar mevedel--view-buffer)

(defun mevedel-recovery--changed (session)
  "Show SESSION's changed issues in its view and to collaboration peers."
  (when-let* ((buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer)))
    (when (fboundp 'mevedel-collaboration-notify-request-changed)
      (mevedel-collaboration-notify-request-changed buffer))
    (when (fboundp 'mevedel-collaboration-recovery-refresh)
      ;; Presentation must not fail the request path that reported the issue.
      (with-demoted-errors "mevedel: recovery publication failed: %S"
        (mevedel-collaboration-recovery-refresh buffer)))
    (when-let* (((local-variable-p 'mevedel--view-buffer buffer))
                (view (buffer-local-value 'mevedel--view-buffer buffer))
                ((buffer-live-p view)))
      (condition-case nil
          (with-current-buffer view (mevedel-view--render-status buffer))
        (error nil)))))

(defun mevedel-recovery-report (session id category message blocking)
  "Retain SESSION issue ID of CATEGORY with MESSAGE and BLOCKING status.
MESSAGE contains no credentials.  Return non-nil when the issues changed."
  (when session
    (let ((issue (list :id id :category (symbol-name category)
                       :message message :blocking (and blocking t)))
          (issues (mevedel-session-recovery-issues session)))
      (unless (member issue issues)
        (setf (mevedel-session-recovery-issues session)
              (cons issue (cl-remove id issues :key (lambda (row) (plist-get row :id))
                                     :test #'equal)))
        (mevedel-recovery--changed session)
        t))))

(defun mevedel-recovery-clear (session &rest ids)
  "Clear SESSION's resolved issues IDS.  Return non-nil when one was removed."
  (when session
    (let* ((issues (mevedel-session-recovery-issues session))
           (kept (cl-remove-if (lambda (row) (member (plist-get row :id) ids)) issues)))
      (unless (= (length kept) (length issues))
        (setf (mevedel-session-recovery-issues session) kept)
        (mevedel-recovery--changed session)
        t))))

(defun mevedel-recovery-clear-informational (session)
  "Clear SESSION's non-blocking issues as a new root request starts."
  (apply #'mevedel-recovery-clear session
         (cl-loop for row in (mevedel-session-recovery-issues session)
                  unless (plist-get row :blocking) collect (plist-get row :id))))

(defun mevedel-recovery-blocker (session)
  "Return SESSION's first execution-blocking issue, or nil."
  (and (mevedel-session-p session)
       (cl-find-if (lambda (issue) (plist-get issue :blocking))
                   (mevedel-session-recovery-issues session))))

(defun mevedel-recovery-assert-ready (session)
  "Refuse execution while SESSION has an unresolved blocking issue."
  (when-let* ((issue (mevedel-recovery-blocker session)))
    (user-error "%s" (plist-get issue :message))))

(defun mevedel-recovery-category (message &optional code status)
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

(defun mevedel-recovery-message (category)
  "Return a description of a CATEGORY failure safe for every reader."
  (pcase category
    ((or 'authentication "authentication") "Provider sign-in is required")
    ((or 'model "model") "The selected model is unavailable")
    ((or 'dependency "dependency") "The provider runtime needs attention")
    ((or 'configuration "configuration") "The session configuration needs attention")
    ((or 'history "history") "Native history needs recovery")
    (_ "The last request failed")))

(defun mevedel-recovery-public-message (issue)
  "Return ISSUE's message for readers other than the host and owners.
Readiness and runtime issues carry host diagnostics, which can name local
paths; other readers see their category instead."
  (if (member (plist-get issue :id) '("authentication" "runtime"))
      (mevedel-recovery-message (plist-get issue :category))
    (plist-get issue :message)))

(defconst mevedel-recovery--input-keys
  '(:id :category :input :guest-name :guest-id :guest-paths :guest-invoke
    :guest-skills :guest-role :shared-question :inert-skills :scope
    :dropped-file-grants :queued-at-time :queued-at-turn :queued-at-goal-id
    :state :request-id)
  "Durable input fields; prepared submissions and callbacks are never serialized.")

(defun mevedel-recovery-persist-inputs (session &optional steering)
  "Return SESSION's unsubmitted inputs without live preparation state.
STEERING selects undelivered steering, always restored for explicit review."
  (mapcar (lambda (entry)
            (cl-loop for key in mevedel-recovery--input-keys
                     when (plist-member entry key)
                     append (list key (if (eq key :input)
                                          (substring-no-properties (plist-get entry key))
                                        (copy-tree (plist-get entry key))))))
          (if steering (mevedel-session-pending-steering session)
            (mevedel-session-pending-follow-ups session))))

(defun mevedel-recovery-save (session)
  "Schedule a coalesced sidecar-only save of SESSION's recovery and input state.
Queued input and pause changes ride the debounced sidecar save; a full
transcript save is never needed for them."
  (when (mevedel-session-p session)
    (mevedel-session-persistence-save-agent-state-soon session)))

(defun mevedel-recovery-save-now (session)
  "Publish SESSION's sidecar now, without touching its transcript.
This is the durable delivery intent written before queued input dispatches.
A session without a committed sidecar has nothing to restore yet."
  (when-let* (((mevedel-session-p session))
              ((mevedel-session-save-path session))
              (buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer)))
    (mevedel-session-artifacts-save-agent-registry session buffer)))

(provide 'mevedel-recovery)
;;; mevedel-recovery.el ends here
