;;; mevedel-recovery.el --- Retained operational issues -*- lexical-binding: t -*-

;;; Commentary:
;; Small, credential-free recovery state shared by local and browser views.
;; Execution owners report failures; presentation does not own retry policy.

;;; Code:

(require 'mevedel-structs)
(require 'subr-x)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration-notify-request-changed "mevedel-collaboration" (buffer))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save "mevedel-session-artifacts" (session buffer &optional settled force))
(autoload 'mevedel-session-artifacts-save "mevedel-session-artifacts")

;; `mevedel-view'
(declare-function mevedel-view--render-status "mevedel-view" (&optional data-buf))
(defvar mevedel--view-buffer)

(defun mevedel-recovery-report (session id category message blocking)
  "Retain SESSION issue ID of CATEGORY with MESSAGE and BLOCKING status.
MESSAGE must be safe for every conversation reader and contain no credentials."
  (when session
    (let ((issue (list :id id :category (symbol-name category)
                       :message message :blocking (and blocking t))))
      (setf (mevedel-session-recovery-issues session)
            (cons issue (cl-remove id (mevedel-session-recovery-issues session)
                                   :key (lambda (row) (plist-get row :id)) :test #'equal))))
    (when-let* ((buffer (mevedel-session-root-buffer session))
                ((buffer-live-p buffer))
                ((fboundp 'mevedel-collaboration-notify-request-changed)))
      (mevedel-collaboration-notify-request-changed buffer))
    (when-let* ((buffer (mevedel-session-root-buffer session))
                ((buffer-live-p buffer))
                (view (buffer-local-value 'mevedel--view-buffer buffer))
                ((buffer-live-p view)))
      (condition-case nil
          (with-current-buffer view (mevedel-view--render-status buffer))
        (error nil)))))

(defun mevedel-recovery-clear (session id)
  "Clear SESSION's resolved issue ID."
  (when session
    (setf (mevedel-session-recovery-issues session)
          (cl-remove id (mevedel-session-recovery-issues session)
                     :key (lambda (row) (plist-get row :id)) :test #'equal))))

(defun mevedel-recovery-blocker (session)
  "Return SESSION's first execution-blocking issue, or nil."
  (and (mevedel-session-p session)
       (cl-find-if (lambda (issue) (plist-get issue :blocking))
                   (mevedel-session-recovery-issues session))))

(defun mevedel-recovery-assert-ready (session)
  "Refuse execution while SESSION has an unresolved blocking issue."
  (when-let* ((issue (mevedel-recovery-blocker session)))
    (user-error "%s" (plist-get issue :message))))

(defun mevedel-recovery-category (message &optional code)
  "Classify safe failure MESSAGE and structured provider CODE for recovery."
  (let ((text (downcase (or message ""))))
    (cond
     ((or (eql code 401)
          (string-match-p "auth\\|login\\|log in\\|credential\\|token.*expir" text)) 'authentication)
     ((or (member code '("model_not_found" "model_not_available"))
          (string-match-p "model.*\\(unavailable\\|not[ _-]found\\|not registered\\|does not exist\\)" text)) 'model)
     ((string-match-p "install\\|upgrade\\|update.*adapter\\|version.*required" text) 'dependency)
     ((string-match-p "preset\\|reasoning effort" text) 'configuration)
     ((string-match-p "native history\\|transcript was edited\\|another installation" text) 'history)
     (t 'request))))

(defconst mevedel-recovery--input-keys
  '(:id :category :input :guest-name :guest-id :guest-paths :guest-invoke
	:guest-skills :guest-role :shared-question :inert-skills :scope
	:dropped-file-grants :queued-at-time :queued-at-turn :queued-at-goal-id :blocked :state :request-id)
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

(defun mevedel-recovery-enqueue (session category entry)
  "Durably accept SESSION input ENTRY in CATEGORY, rolling back on failure."
  (let ((before (mevedel-session-pending-inputs session category))
        (id (mevedel-session-pending-input-next-id session)))
    (condition-case err
        (let ((accepted (mevedel-session-enqueue-pending-input session category entry)))
          (mevedel-recovery-save session)
          accepted)
      (error
       (mevedel-session-set-pending-inputs session category before)
       (setf (mevedel-session-pending-input-next-id session) id)
       (signal (car err) (cdr err))))))

(defun mevedel-recovery-save (session)
  "Publish SESSION's recovery and input changes through its ordinary save seam."
  (when-let* (((mevedel-session-p session))
              (buffer (mevedel-session-root-buffer session))
              ((buffer-live-p buffer))
              ((mevedel-session-workspace session)))
    (mevedel-session-artifacts-save session buffer)))

(provide 'mevedel-recovery)
;;; mevedel-recovery.el ends here
