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
      (puthash 1 (list :owner t :writable t) guests)
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
       (puthash 1 (list :owner t :writable t) guests)
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
          (list (list "root" :engine 'claude-code :id "native" :state 'diverged)
                (list "/root/pruned" :engine 'claude-code :id "child" :state 'diverged)))
    (setq sent nil)
    ;; Reporting the issue refreshes the owner; an unchanged frame is not resent.
    (mevedel-recovery-report session "authentication" 'dependency "Searching for program: /opt/claude" t)
    (mevedel-collaboration-recovery-send room 1)
    (should (= 1 (length sent)))
    (let ((frame (cdr (assq 1 sent))))
      (should (equal "recovery" (plist-get frame :t)))
      (should (equal [(:id 4 :text "lost")] (plist-get frame :steering)))
      ;; A pruned child's history cannot be recovered, so it is not offered.
      (should (equal ["root"] (plist-get frame :histories)))
      ;; The queue is not paused, and an API-key provider has no browser login.
      (should (eq :json-false (plist-get frame :paused)))
      (should (equal [] (plist-get frame :providers)))
      (should-not (plist-get frame :provider))
      (should (string-search "/opt/claude" (prin1-to-string (plist-get frame :issues))))
      (should (member "Recovery:second" (append (plist-get frame :models) nil)))
      ;; The current model is named as the picker offers it.
      (should (equal "Recovery:first" (plist-get frame :model)))
      ;; Runtime maintenance belongs to Claude sessions only.
      (should-not (plist-get frame :runtime))))

  :doc "live queue and request updates expose new recovery choices only to owners"
  (mevedel-engine-test--with-session
    (mevedel-request-end)
    (let* ((gptel--known-backends nil)
           (guests (make-hash-table :test #'eql))
           (room (list :session session :data-buffer buffer :guests guests))
           (mevedel-collaboration--rooms (make-hash-table :test #'eq))
           (model-candidates (symbol-function 'mevedel-model-candidates))
           (model-reads 0)
           (runtime-reads 0)
           sent)
      (puthash buffer room mevedel-collaboration--rooms)
      ;; Runtime state belongs to Claude sessions.
      (setq-local gptel-backend (mevedel-claude-code-register))
      (puthash 1 (list :owner t :writable t :ready t) guests)
      (puthash 2 (list :owner nil :writable t :ready t) guests)
      (cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
                 (lambda (_transport peer frame) (push (cons peer frame) sent) t))
                ((symbol-function 'mevedel-collaboration--transport-control) #'ignore)
                ((symbol-function 'mevedel-model-candidates)
                 (lambda () (cl-incf model-reads) (funcall model-candidates)))
                ((symbol-function 'mevedel-claude-code-maintenance-state)
                 (lambda () (cl-incf runtime-reads) nil)))
        (mevedel-collaboration-recovery-send room 1)
        (setq sent nil)
        (setf (mevedel-session-pending-steering session)
              (list (list :id 1 :category 'steering :state 'failed-turn :input "retained")))
        (mevedel-collaboration-notify-queue-changed session)
        (let ((frames (cl-remove-if-not
                       (lambda (entry) (equal "recovery" (plist-get (cdr entry) :t))) sent)))
          (should (= 1 (length frames)))
          (should (= 1 (caar frames)))
          (should (equal "retained" (plist-get (aref (plist-get (cdar frames) :steering) 0) :text))))
        (setq sent nil)
        (mevedel-collaboration-notify-queue-changed session)
        (should-not sent)
        (setf (mevedel-session-external-conversations session)
              (list (cons "root" (list :engine 'claude-code :id "fixture" :start-failed t))))
        (mevedel-recovery-report session "history" 'history "Recover history" t)
        (let ((frames (cl-remove-if-not
                       (lambda (entry) (equal "recovery" (plist-get (cdr entry) :t))) sent)))
          (should (= 1 (length frames)))
          (should (equal ["root"] (plist-get (cdar frames) :histories))))
        (should (= runtime-reads 1))
        (setq sent nil)
        (let ((before model-reads))
          (dotimes (_ 20) (mevedel-collaboration--publish-status room))
          (should (= before model-reads))
          (should-not sent))
        (mevedel-collaboration-recovery-runtime-changed '(:status "ready" :message "Updated"))
        (should (= runtime-reads 1))
        (should (equal "Updated" (plist-get (plist-get (cdar sent) :runtime) :message)))))))

(mevedel-deftest mevedel-collaboration-recovery-auth-changed
  (:doc "authentication changes refresh only owners viewing that credential store")
  (let* ((gptel--known-backends nil)
         (codex (gptel-make-openai-oauth "Recovery Codex"))
         (claude (mevedel-claude-code-register))
         (first (generate-new-buffer " *recovery-codex*"))
         (second (generate-new-buffer " *recovery-claude*"))
         (mevedel-collaboration--rooms (make-hash-table :test #'eq))
         (first-guests (make-hash-table :test #'eql))
         (second-guests (make-hash-table :test #'eql))
         sent)
    (unwind-protect
        (progn
          (with-current-buffer first (setq-local gptel-backend codex))
          (with-current-buffer second (setq-local gptel-backend claude))
          (puthash 1 (list :owner t :writable t) first-guests)
          (puthash 2 (list :owner t :writable t :recovery-backend codex) second-guests)
          (puthash 3 (list :owner t :writable t) second-guests)
          (puthash first (list :data-buffer first :guests first-guests) mevedel-collaboration--rooms)
          (puthash second (list :data-buffer second :guests second-guests) mevedel-collaboration--rooms)
          (cl-letf (((symbol-function 'mevedel-collaboration-recovery-send)
                     (lambda (_room peer) (push peer sent))))
            (mevedel-collaboration-recovery-auth-changed claude nil)
            (should (equal '(3) sent))
            (setq sent nil)
            (mevedel-collaboration-recovery-auth-changed codex nil)
            (should (equal '(1 2) (sort sent #'<)))
            (should (eq codex (plist-get (gethash 2 second-guests) :recovery-backend)))
            (should-not (plist-get (gethash 1 first-guests) :recovery-backend))))
      (kill-buffer first)
      (kill-buffer second))))

(provide 'test-mevedel-collaboration-recovery)
;;; test-mevedel-collaboration-recovery.el ends here
