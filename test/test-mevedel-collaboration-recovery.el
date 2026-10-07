;;; test-mevedel-collaboration-recovery.el --- Recovery authority -*- lexical-binding: t -*-
;;; Commentary:
;; Login and repair frames have the same explicit owner boundary.
;;; Code:
(require 'helpers (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "helpers"))
(require 'mevedel-collaboration-recovery)
(require 'mevedel-collaboration)
(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name)) "mevedel-engine-test-support"))

(mevedel-deftest mevedel-collaboration-recovery-handle (:quiet t)
  ,test
  (test)
  :doc "ordinary guests cannot change recovery state"
  (let* ((guests (make-hash-table :test #'eql))
	 (room (list :guests guests)))
    (puthash 1 '(:writable t :owner nil) guests)
    (dolist (action '("model" "preset" "history" "retry" "login" "login-code" "cancel-login" "update" "input-discard"))
      (should-error (mevedel-collaboration-recovery-handle room 1 (list :action action)) :type 'user-error)))
  :doc "owner requeue discards failed live preparation and keeps input provenance"
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((submission (mevedel-prompt-submission-create :input "fresh" :session session :state 'reserved))
           (entry (list :id 1 :category 'steering :state 'failed-turn :input "fresh"
                        :guest-name "Owner" :request-id "old" :submission submission
                        :request-context '(:old t)))
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests)))
      (puthash 1 '(:owner t :writable t) guests)
      (setf (mevedel-session-pending-steering session) (list entry)
            (mevedel-session-pending-input-next-id session) 1
            (mevedel-session-pending-input-failure-paused session) t)
      (cl-letf (((symbol-function 'mevedel-collaboration--publish-status) #'ignore)
                ((symbol-function 'mevedel-collaboration--publish-queue) #'ignore)
                ((symbol-function 'mevedel-collaboration-recovery-send) #'ignore))
        (mevedel-collaboration-recovery-handle room 1 '(:action "input-requeue" :value 1)))
      (let ((queued (car (mevedel-session-pending-follow-ups session))))
        (should (equal "fresh" (plist-get queued :input)))
        (should (equal "Owner" (plist-get queued :guest-name)))
        (should-not (plist-get queued :submission))
        (should-not (plist-get queued :request-context))
        (should-not (plist-get queued :request-id))
        (should-not (mevedel-session-pending-steering session))
        (should (mevedel-session-pending-input-failure-paused session)))))
  :doc "owner model choice uses offered models and resolves model and login gates"
  (test-mevedel-collaboration-recovery--with-owner
    (mevedel-recovery-report session "model" 'model "Gone:old is unavailable; select a model" t)
    (mevedel-recovery-report session "authentication" 'authentication "Expired" t)
    (should-error (mevedel-collaboration-recovery-handle
                   room 1 '(:action "model" :value "Recovery:missing"))
                  :type 'user-error)
    (mevedel-collaboration-recovery-handle room 1 '(:action "model" :value "Recovery:second"))
    (should (eq 'second gptel-model))
    (should (equal "Recovery:second" (mevedel-session-model-provider session)))
    (should-not (mevedel-recovery-blocker session)))
  :doc "retry clears the failure pause once retained input is resolved"
  (test-mevedel-collaboration-recovery--with-owner
    (mevedel-session-set-pending-input-failure-paused session t)
    (mevedel-collaboration-recovery-handle room 1 '(:action "retry"))
    (should-not (mevedel-session-pending-input-failure-paused session)))
  :doc "runtime updates are refused outside Claude sessions"
  (test-mevedel-collaboration-recovery--with-owner
    (should-error (mevedel-collaboration-recovery-handle room 1 '(:action "update"))
                  :type 'user-error))
  :doc "a replacement preset keeps permission and sandbox modes, including unset ones"
  (test-mevedel-collaboration-recovery--with-owner
    (let ((mevedel-preset--registry nil)
          (gptel--known-presets nil))
      (mevedel-define-preset test-recovery-preset
        :description "Recovery"
        :permission-mode 'full-auto)
      (should (assq 'mevedel-permission-mode
                    (plist-get (alist-get 'test-recovery-preset mevedel-preset--registry)
                               :settings)))
      (setf (mevedel-session-preset-name session) 'missing)
      (mevedel-recovery-report session "preset" 'configuration "Preset missing is unavailable" t)
      (kill-local-variable 'mevedel-permission-mode)
      (cl-letf (((symbol-function 'mevedel-agents--setup-for-request) #'ignore)
                ((symbol-function 'mevedel-preset--setup-catalog) #'ignore)
                ((symbol-function 'mevedel-preset--setup-extras) #'ignore))
        (mevedel-collaboration-recovery-handle
         room 1 '(:action "preset" :value "test-recovery-preset")))
      (should (eq 'test-recovery-preset (mevedel-session-preset-name session)))
      (should-not (local-variable-p 'mevedel-permission-mode))
      (should (eq 'full-auto (mevedel-session-permission-mode session)))
      (should-not (mevedel-recovery-blocker session)))))

(defmacro test-mevedel-collaboration-recovery--with-owner (&rest body)
  "Run BODY with an idle fixture session shared to owner peer 1.
BODY sees ROOM, SESSION, BUFFER and SENT, the frames sent to peers."
  (declare (indent 0) (debug t))
  `(mevedel-engine-test--with-session
     (mevedel-request-end)
     (let* ((gptel--known-backends nil)
            (backend (gptel-make-openai "Recovery" :key "test" :models '(first second)))
            (guests (make-hash-table :test #'eql))
            (room (list :session session :data-buffer buffer :guests guests :transport 'transport))
            (mevedel-collaboration--rooms (mevedel-test-room-registry room))
            sent)
       (puthash 1 '(:owner t :writable t) guests)
       (setq-local gptel-backend backend gptel-model 'first)
       (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                  (lambda (_transport peer frame) (push (cons peer frame) sent)))
                 ((symbol-function 'mevedel-collaboration--publish-status) #'ignore)
                 ((symbol-function 'mevedel-collaboration--publish-queue) #'ignore))
         ,@body))))

(mevedel-deftest mevedel-collaboration-recovery-send (:quiet t)
  ,test
  (test)
  :doc "sends nothing to a peer without an owner link"
  (let ((room (list :guests (make-hash-table :test #'eql))) sent)
    (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
	       (lambda (&rest _) (setq sent t))))
      (mevedel-collaboration-recovery-send room 1)
      (should-not sent)))
  :doc "gives owners reviewable input, recoverable histories and host details"
  (test-mevedel-collaboration-recovery--with-owner
    (setf (mevedel-session-pending-steering session)
          (list (list :id 4 :category 'steering :state 'failed-turn :input "lost")
                (list :id 5 :category 'steering :input "pending"))
          (mevedel-session-external-conversations session)
          (list (list "root" :engine 'claude-code) (list "/root/pruned" :engine 'claude-code)))
    (mevedel-recovery-report session "authentication" 'dependency "Searching for program: /opt/claude" t)
    (setq sent nil)
    (mevedel-collaboration-recovery-send room 1)
    (let ((frame (cdr (assq 1 sent))))
      (should (equal "recovery" (plist-get frame :t)))
      (should (equal [(:id 4 :text "lost")] (plist-get frame :steering)))
      ;; A pruned child's history cannot be recovered, so it is not offered.
      (should (equal ["root"] (plist-get frame :histories)))
      (should (string-search "/opt/claude" (prin1-to-string (plist-get frame :issues))))
      (should (member "Recovery:second" (append (plist-get frame :models) nil)))
      ;; Runtime maintenance belongs to Claude sessions only.
      (should-not (plist-get frame :runtime)))))

(mevedel-deftest mevedel-collaboration-recovery-auth-changed ()
  (test-mevedel-collaboration-recovery--with-owner
    (let ((other (gptel-make-openai-oauth "Other")))
      (setq sent nil)
      (mevedel-collaboration-recovery-auth-changed other '(:status "login"))
      (should-not sent)
      (setq room (plist-put room :recovery-login other))
      (mevedel-collaboration-recovery-auth-changed other '(:status "login"))
      (should (equal "Other" (plist-get (cdr (assq 1 sent)) :provider)))
      ;; While pending, issue refreshes and a reconnect's hello agree.
      (setq sent nil)
      (mevedel-collaboration-recovery-refresh buffer)
      (mevedel-collaboration-recovery-send room 1)
      (should (equal '("Other" "Other") (mapcar (lambda (row) (plist-get (cdr row) :provider)) sent)))
      ;; A settled login stops routing; every frame reports the session again.
      (dolist (settled '((:status "ready") nil))
        (setq room (plist-put room :recovery-login other))
        (mevedel-collaboration-recovery-auth-changed other settled)
        (should-not (plist-get room :recovery-login))
        (setq sent nil)
        (mevedel-collaboration-recovery-refresh buffer)
        (mevedel-collaboration-recovery-send room 1)
        (should (equal '("Recovery" "Recovery")
                       (mapcar (lambda (row) (plist-get (cdr row) :provider)) sent))))
      ;; Only a login that actually started is followed.
      (dolist (case '(("failed" . nil) ("login" . t)))
        (cl-letf (((symbol-function 'mevedel-auth-start)
                   (lambda (_backend) (list :status (car case)))))
          (mevedel-collaboration-recovery-handle room 1 '(:action "login" :provider "Other")))
        (should (eq (cdr case) (and (plist-get room :recovery-login) t)))))))

(provide 'test-mevedel-collaboration-recovery)
;;; test-mevedel-collaboration-recovery.el ends here
