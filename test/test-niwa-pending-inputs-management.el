;;; test-niwa-pending-inputs-management.el --- Direct queue controls -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise composer withdrawal, inline deletion and explicit queue resumption.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-pending-inputs)
(require 'niwa-mention-bindings)
(require 'niwa-prompt-submission)
(require 'niwa-recovery)
(require 'niwa-structs)
(require 'niwa-session-artifacts)
(require 'niwa-tools)
(require 'niwa-claude-code-context)
(require 'niwa-hooks)
(require 'niwa-mentions)
(require 'niwa-reminders)
(require 'niwa-turn)
(require 'gptel-openai)
(require 'niwa-view)
(require 'niwa-view-composer)
(require 'niwa-view-interaction)

(defmacro niwa-pending-management-test--with-session (&rest body)
  "Run BODY with a real paired composer and its SESSION."
  (declare (indent 0) (debug t))
  `(niwa-view-test--with-buffers
     (let* ((workspace (niwa-workspace--create
                        :type 'file :id "management" :name "management"
                        :root default-directory))
            (session (niwa-session-create "main" workspace)))
       (niwa-session-set-root-buffer session data-buf)
       (with-current-buffer data-buf
         (setq-local niwa--session session
                     niwa--workspace workspace
                     niwa--view-buffer view-buf))
       (with-current-buffer view-buf
         (setq-local niwa--session session))
       (cl-letf (((symbol-function 'niwa-recovery-save) #'ignore)
                 ((symbol-function 'niwa-recovery-save-now) #'ignore)
                 ((symbol-function 'niwa-pending-inputs-schedule-held-input-delivery)
                  #'ignore))
         ,@body))))

(niwa-deftest niwa-view-retract-pending (:quiet t)
  ,test
  (test)
  :doc "withdraws newest acceptance across categories, independent of queue order"
  (niwa-pending-management-test--with-session
    (let* ((first (niwa-session-enqueue-pending-input
                   session 'steering '(:input "first" :queued-at-time 10)))
           (second (niwa-session-enqueue-pending-input
                    session 'follow-up '(:input "second" :queued-at-time 10)))
           (third (niwa-session-enqueue-pending-input
                   session 'steering '(:input "third" :queued-at-time 10))))
      (niwa-session-set-pending-inputs session 'steering (list third first))
      (with-current-buffer view-buf
        (niwa-view-retract-pending)
        (should (equal "third" (niwa-view-composer-draft-text)))
        (should (equal (list first) (niwa-session-pending-steering session)))
        (niwa-view--clear-input)
        (niwa-view-retract-pending)
        (should (equal "second" (niwa-view-composer-draft-text)))
        (should-not (niwa-session-pending-follow-ups session))
        (should-not (niwa-session-pending-input-paused session))
        (should (eq 'follow-up (plist-get second :category))))))
  :doc "refuses even a whitespace draft without changing either queue"
  (niwa-pending-management-test--with-session
    (let ((entry (niwa-session-enqueue-pending-input
                  session 'follow-up '(:input "queued"))))
      (with-current-buffer view-buf
        (niwa-view-composer-insert " \n ")
        (should-error (niwa-view-retract-pending) :type 'user-error)
        (should (equal " \n " (niwa-view-composer-draft-text)))
        (should (eq entry (car (niwa-session-pending-follow-ups session)))))))
  :doc "refuses the newest dispatching entry rather than withdrawing an older one"
  (niwa-pending-management-test--with-session
    (niwa-session-enqueue-pending-input session 'steering '(:input "old"))
    (dolist (flag '(:state :delivering))
      (let ((entry (niwa-session-enqueue-pending-input
                    session 'follow-up (list :input "in flight" flag
                                            (if (eq flag :state) 'dispatching t)))))
        (with-current-buffer view-buf
          (should-error (niwa-view-retract-pending) :type 'user-error)
          (should (string-empty-p (niwa-view-composer-draft-text))))
        (should (memq entry (niwa-session-pending-follow-ups session))))))
  :doc "restores bound mention text, attachment grants and reserved hook context"
  (niwa-pending-management-test--with-session
    (let* ((text (copy-sequence "@file:picture.png"))
           (submission (niwa-prompt-submission-create
                        :session session :state 'reserved
                        :context-entries '((:text "context"))))
           (path (expand-file-name "picture.png")))
      (niwa-mention-bindings-set
       0 (length text) (list :kind 'file :token text :path path) text)
      (niwa-session-enqueue-pending-input
       session 'steering (list :input text :submission submission
                              :dropped-file-grants (list path)))
      (with-current-buffer view-buf
        (niwa-view-retract-pending)
        (should (equal-including-properties
                 text (niwa-mention-bindings-copy-text
                       (niwa-view-composer-draft-text)))))
      (should (equal (list path) (niwa-session-dropped-file-grants session)))
      (should (equal '((:text "context"))
                     (niwa-session-hook-context-pending session)))
      (should-not (niwa-session-pending-steering session))))
  :doc "does not overwrite a draft typed while mutation authority is checked"
  (niwa-pending-management-test--with-session
    (let ((entry (niwa-session-enqueue-pending-input session 'follow-up '(:input "keep"))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
                   (lambda (_)
                     (niwa-view-composer-insert
                      "> new draft\nsecond line" nil view-buf))))
          (should-error (niwa-view-retract-pending) :type 'user-error))
        (should (equal "> new draft\nsecond line" (niwa-view-composer-draft-text))))
      (should (eq entry (car (niwa-session-pending-follow-ups session)))))))

(niwa-deftest niwa-pending-inputs--remove ()
  ,test
  (test)
  :doc "rechecks ownership after an authority check yields to dispatch"
  (niwa-pending-management-test--with-session
    (let ((entry (niwa-session-enqueue-pending-input
                  session 'follow-up '(:input "racing"))))
      (cl-letf (((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (_) (plist-put entry :delivering t))))
        (should-error (niwa-pending-inputs--remove session entry) :type 'user-error))
      (should (eq entry (car (niwa-session-pending-follow-ups session))))))
  :doc "refuses foreign mutation authority before touching input or its context"
  (niwa-pending-management-test--with-session
    (let ((entry (niwa-session-enqueue-pending-input
                  session 'follow-up '(:input "read only"))))
      (cl-letf (((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
                 (lambda (_) (user-error "Session is read-only"))))
        (should-error (niwa-pending-inputs--remove session entry) :type 'user-error))
      (should (eq entry (car (niwa-session-pending-follow-ups session)))))))

(niwa-deftest niwa-pending-inputs-drop-at-point ()
  ,test
  (test)
  :doc "d and DEL drop only the selected full or abbreviated row, preserving a draft"
  (niwa-pending-management-test--with-session
    (save-window-excursion
      (switch-to-buffer view-buf)
      (niwa-session-enqueue-pending-input session 'steering '(:input "short"))
      (niwa-session-enqueue-pending-input session 'follow-up '(:input "long\nbody"))
      (niwa-view-composer-insert "> draft\nsecond line")
      (dolist (pair '(("short" . "d") ("long" . "DEL")))
        (niwa-view--interaction-rebuild)
        (goto-char (point-min))
        (search-forward (car pair))
        (let ((command (key-binding (kbd (cdr pair)))))
          (should (eq command #'niwa-pending-inputs-drop-at-point))
          (call-interactively command))
        (should (equal "> draft\nsecond line" (niwa-view-composer-draft-text)))
        (should-not (niwa-session-pending-input-paused session)))
      (should-not (niwa-session-pending-input-p session))))
  :doc "stale rows and dispatching inputs refuse deletion"
  (niwa-pending-management-test--with-session
    (with-current-buffer view-buf
      (let ((entry (niwa-session-enqueue-pending-input
                    session 'follow-up '(:input "racing" :state dispatching))))
        (niwa-view--interaction-rebuild)
        (goto-char (point-min))
        (search-forward "racing")
        (should-error (niwa-pending-inputs-drop-at-point) :type 'user-error)
        (niwa-session-set-pending-inputs session 'follow-up nil)
        (should-error (niwa-pending-inputs-drop-at-point) :type 'user-error)
        (should (eq 'dispatching (plist-get entry :state)))))))

(niwa-deftest niwa-view-resume-pending (:quiet t)
  ,test
  (test)
  :doc "starts one combined steering message, clears holds and then drains follow-ups"
  (niwa-pending-management-test--with-session
    (niwa-session-enqueue-pending-input
     session 'steering '(:input "first" :state failed-turn))
    (niwa-session-enqueue-pending-input
     session 'steering '(:input "second" :state pending))
    (let ((later (niwa-session-enqueue-pending-input
                  session 'follow-up '(:input "later")))
          messages)
      (niwa-session-set-pending-input-paused session t)
      (niwa-session-set-pending-input-failure-paused session t)
      (cl-letf (((symbol-function 'niwa-view--submit-planned-input)
                 (lambda (input before _blocked _dispatch after &rest _)
                   (when before (funcall before))
                   (push input messages)
                   (funcall after))))
        (with-current-buffer view-buf (niwa-view-resume-pending))
        (should (equal '("first\n\nsecond") messages))
        (should-not (niwa-session-pending-steering session))
        (should-not (niwa-session-pending-input-failure-paused session))
        (should-not (niwa-session-pending-input-paused session))
        (should (equal (list later) (niwa-session-pending-follow-ups session)))
        (niwa-view--drain-follow-up data-buf)
        (should (equal '("later" "first\n\nsecond") messages))
        (should-not (niwa-session-pending-follow-ups session)))))
  :doc "retains combined input after a blocked dispatch instead of losing steering"
  (niwa-pending-management-test--with-session
    (niwa-session-enqueue-pending-input session 'steering '(:input "keep"))
    (cl-letf (((symbol-function 'niwa-view--submit-planned-input)
               (lambda (_input _before blocked &rest _) (funcall blocked))))
      (with-current-buffer view-buf (niwa-view-resume-pending)))
    (should (equal "keep" (plist-get (car (niwa-session-pending-follow-ups session)) :input)))
    (should-not (plist-get (car (niwa-session-pending-follow-ups session)) :delivering)))
  :doc "refuses a running turn or a draft without releasing the interrupt hold"
  (niwa-pending-management-test--with-session
    (niwa-session-enqueue-pending-input session 'steering '(:input "keep"))
    (niwa-session-set-pending-input-paused session t)
    (with-current-buffer data-buf
      (setq-local niwa--current-request (niwa-request--create :id "active")))
    (with-current-buffer (niwa-view-composer-buffer view-buf)
      (should-error (niwa-view-resume-pending) :type 'user-error)
      (with-current-buffer data-buf (setq-local niwa--current-request nil))
      (niwa-view-composer-insert "draft")
      (should-error (niwa-view-resume-pending) :type 'user-error))
    (should (niwa-session-pending-input-paused session))))

(niwa-deftest niwa-view-resume-pending/mixed-preparation (:quiet t)
  (niwa-pending-management-test--with-session
    (let ((live (niwa-prompt-submission-create :session session :state 'reserved))
          hooks forwarded)
      (niwa-session-enqueue-pending-input
       session 'steering '(:input "COLD" :state failed-turn))
      (niwa-session-enqueue-pending-input
       session 'steering (list :input "LIVE" :model-input "FROZEN-LIVE"
                              :transcript-payload "LIVE" :submission live))
      (niwa-session-set-pending-input-failure-paused session t)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event data callback &rest _)
                   (should (eq event 'UserPromptSubmit))
                   (push (plist-get data :prompt) hooks)
                   (funcall callback '(:continue t))))
                ((symbol-function 'niwa-view--forward-input-now)
                 (lambda (_input &rest keys)
                   (setq forwarded (plist-get keys :model-input))
                   (niwa-prompt-submission-commit (plist-get keys :submission))
                   (funcall (plist-get keys :after-insert)))))
        (with-current-buffer view-buf (niwa-view-resume-pending)))
      (should (equal '("COLD") hooks))
      (should (equal "COLD\n\nFROZEN-LIVE" forwarded))
      (should (eq 'reserved (niwa-prompt-submission-state live)))
      (should-not (niwa-session-pending-input-p session)))))

(niwa-deftest niwa-pending-inputs-cancel-resume (:quiet t)
  (niwa-pending-management-test--with-session
    (let ((live (niwa-prompt-submission-create :session session :state 'reserved))
          callback cold)
      (niwa-session-enqueue-pending-input
       session 'steering (list :input "LIVE" :model-input "FROZEN-LIVE"
                              :transcript-payload "LIVE" :submission live))
      (setq cold (niwa-session-enqueue-pending-input
                  session 'steering '(:input "COLD" :state failed-turn)))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _data done &rest _) (setq callback done))))
        (with-current-buffer view-buf
          (niwa-view-resume-pending)
          (should niwa-view--prompt-hook-pending)
          (should (plist-get cold :delivering))
          (niwa-view-abort)
          (should-not niwa-view--prompt-hook-pending)
          (should-not (plist-get cold :delivering))
          (funcall callback '(:continue t))
          (should-not (plist-get cold :submission))
          (should (= 2 (length (niwa-session-pending-steering session))))
          (niwa-view-retract-pending)
          (should (equal "COLD" (niwa-view-composer-draft-text)))))
      (should (eq 'reserved (niwa-prompt-submission-state live))))))

(niwa-deftest niwa-view-resume-pending/paused-goal (:quiet t)
  (niwa-pending-management-test--with-session
    (let* ((goal (niwa-goal--create :id "goal" :status 'paused))
           (later (niwa-session-enqueue-pending-input
                   session 'follow-up '(:input "goal follow-up" :queued-at-goal-id "goal")))
           messages)
      (setf (niwa-session-goal session) goal)
      (niwa-session-enqueue-pending-input session 'steering '(:input "explicit steering"))
      (niwa-session-set-pending-input-paused session t)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _data callback &rest _) (funcall callback '(:continue t))))
                ((symbol-function 'niwa-view--forward-input-now)
                 (lambda (input &rest keys)
                   (push input messages)
                   (niwa-prompt-submission-commit (plist-get keys :submission))
                   (funcall (plist-get keys :after-insert)))))
        (with-current-buffer view-buf (niwa-view-resume-pending))
        (niwa-view--drain-follow-up data-buf))
      (should (equal '("explicit steering") messages))
      (should (eq 'paused (niwa-goal-status goal)))
      (should (equal (list later) (niwa-session-pending-follow-ups session))))))

(niwa-deftest niwa-pending-inputs--resume-entry ()
  ,test
  (test)
  :doc "preserves prepared model text, transcript, audits, skills, grants and context"
  (niwa-pending-management-test--with-session
    (let* ((context '((:text "retained")))
           (one (niwa-prompt-submission-create
                 :session session :state 'reserved :context-entries context))
           (two (niwa-prompt-submission-create :session session :state 'reserved))
           (first (niwa-session-enqueue-pending-input
                   session 'steering
                   (list :input "first" :model-input "expanded first"
                         :transcript-payload "stored first" :submission one
                         :hook-audits '((:event first))
                         :request-context '(:invoked-skills ((:name "one")))
                         :dropped-file-grants '("/tmp/one"))))
           (second (niwa-session-enqueue-pending-input
                    session 'steering
                    (list :input "second" :model-input "expanded second"
                          :transcript-payload "stored second" :submission two
                          :hook-audits '((:event second))
                          :request-context '(:invoked-skills ((:name "two")))
                          :dropped-file-grants '("/tmp/two"))))
           (combined (niwa-pending-inputs--resume-entry session (list first second)))
           (submission (plist-get combined :submission))
           (outcome (niwa-prompt-submission-outcome submission)))
      (should (equal "first\n\nsecond" (plist-get combined :input)))
      (should (equal "expanded first\n\nexpanded second" (plist-get outcome :model-input)))
      (should (equal "stored first\n\nstored second" (plist-get outcome :transcript-input)))
      (should (equal '((:event first) (:event second)) (plist-get outcome :hook-audits)))
      (should (equal '((:name "one") (:name "two"))
                     (plist-get (plist-get outcome :request-context) :invoked-skills)))
      (should (equal '("/tmp/one" "/tmp/two") (plist-get combined :dropped-file-grants)))
      (should (equal context (niwa-prompt-submission-context-entries submission)))
      (niwa-prompt-submission-restore submission)
      (should (equal context (niwa-session-hook-context-pending session))))))

(niwa-deftest niwa-pending-inputs--save-follow-up ()
  (niwa-pending-management-test--with-session
    (let* ((context '((:text "retained")))
           (submission (niwa-prompt-submission-create
                        :session session :state 'reserved :context-entries context))
           (entry (list :input "combined" :submission submission))
           replacement)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'niwa-pending-inputs--finish-replacement)
                   (lambda (_state value) (setq replacement value))))
          (niwa-pending-inputs--save-follow-up (list :entry entry) "edited" session)))
      (should (equal "edited" (plist-get replacement :input)))
      (should-not (plist-get replacement :submission))
      (should (equal context (niwa-session-hook-context-pending session)))
      (should (eq 'pending (niwa-prompt-submission-state submission))))))

(niwa-deftest niwa-tools-steering-claim ()
  ,test
  (test)
  :doc "reserves the complete snapshot before expansion can run a withdrawal command"
  (niwa-pending-management-test--with-session
    (let ((first (niwa-session-enqueue-pending-input session 'steering '(:input "first")))
          (second (niwa-session-enqueue-pending-input session 'steering '(:input "second"))))
      (niwa-tools-steering-claim session (list first second))
      (dolist (entry (list first second))
        (should (plist-get entry :delivering))
        (should-error (niwa-pending-inputs--remove session entry) :type 'user-error))
      (with-current-buffer view-buf
        (should-error (niwa-view-retract-pending) :type 'user-error))))
  :doc "rejects a stale snapshot without partially reserving its remaining entries"
  (niwa-pending-management-test--with-session
    (let ((first (niwa-session-enqueue-pending-input session 'steering '(:input "first")))
          (second (niwa-session-enqueue-pending-input session 'steering '(:input "second"))))
      (niwa-session-set-pending-inputs session 'steering (list first))
      (should-error (niwa-tools-steering-claim session (list first second)) :type 'user-error)
      (should-not (plist-get first :delivering)))))

(niwa-deftest niwa-tools--handle-steering-inject/claim-release (:quiet t)
  (niwa-pending-management-test--with-session
    (dolist (input '("first" "second"))
      (niwa-session-enqueue-pending-input
       session 'steering (list :input input :request-id "active")))
    (let ((fsm (gptel-make-fsm :info (list :buffer data-buf :history '(TRET)
                                         :niwa-request-id "active"))))
      (cl-letf (((symbol-function 'niwa-tools-steering-expand)
                 (lambda (&rest _) (error "Injected expansion failure"))))
        (should-error (niwa-tools--handle-steering-inject fsm t)))
      (dolist (entry (niwa-session-pending-steering session))
        (should-not (plist-get entry :delivering)))
      (with-current-buffer view-buf
        (niwa-view-retract-pending)
        (should (equal "second" (niwa-view-composer-draft-text)))))))

(niwa-deftest niwa-tools--handle-steering-inject/post-injection-error ()
  (niwa-pending-management-test--with-session
    (let* ((gptel--known-backends (copy-tree gptel--known-backends))
           (backend (gptel-make-openai "Claim test" :stream nil :key "unused"
                                       :host "example.test" :models '(steering-test)))
           (data (list :messages []))
           (first (niwa-session-enqueue-pending-input
                   session 'steering '(:input "first" :request-id "active")))
           (second (niwa-session-enqueue-pending-input
                    session 'steering '(:input "second" :request-id "active")))
           (fsm (gptel-make-fsm :info (list :buffer data-buf :history '(TRET)
                                          :backend backend :data data :model 'steering-test
                                          :niwa-request-id "active"))))
      (cl-letf (((symbol-function 'niwa-tools-steering-expand)
                 (lambda (&rest _)
                   '(:text "sent" :reminder-items ((:key test :body "reminder")))))
                ((symbol-function 'niwa-reminders-stage-entry)
                 (lambda (&rest _) (error "Injected reminder staging failure"))))
        (should-error (niwa-tools--handle-steering-inject fsm t)))
      (should (equal "sent" (plist-get (aref (plist-get data :messages) 0) :content)))
      (should (plist-get first :delivering))
      (should-not (plist-get second :delivering))
      (should-error (niwa-pending-inputs--remove session first) :type 'user-error)
      (niwa-pending-inputs--remove session second)
      (should (equal (list first) (niwa-session-pending-steering session))))))

(niwa-deftest niwa-tools-steering-expand/abort-reentry (:quiet t)
  (dolist (native '(nil t))
    (niwa-pending-management-test--with-session
      (let* ((request (niwa-request--create :id "active" :session session))
             (info (list :buffer data-buf :history '(TRET) :niwa-request-id "active"))
             (owner (if native request (gptel-make-fsm :info info))))
        (when native (setf (niwa-engine-info owner) info))
        (niwa-session-enqueue-pending-input
         session 'steering '(:input "keep" :request-id "active"))
        (cl-letf (((symbol-function 'niwa-mentions-expand-user-input)
                   (lambda (&rest _)
                     (niwa--turn-fail-pending-input owner 'aborted)
                     '(:text "stale expansion"))))
          (should-error (if native (niwa-claude-code-context--steering owner)
                          (niwa-tools--handle-steering-inject owner t))
                        :type 'user-error))
        (let ((entry (car (niwa-session-pending-steering session))))
          (should-not (plist-get entry :request-id))
          (should-not (plist-get entry :delivering)))
        (with-current-buffer view-buf
          (niwa-view-retract-pending)
          (should (equal "keep" (niwa-view-composer-draft-text))))))))

(niwa-deftest niwa-pending-inputs-cockpit-holds-p ()
  (niwa-pending-management-test--with-session
    (should-not (niwa-pending-inputs-cockpit-holds-p session))
    (let ((cockpit (get-buffer-create niwa-pending-inputs-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer cockpit
              (setq-local niwa-cockpit--context (list :session session)))
            (should (niwa-pending-inputs-cockpit-holds-p session))
            (should-not (niwa-pending-inputs-cockpit-holds-p
                         (niwa-session-create "other" workspace))))
        (kill-buffer cockpit)))))

(niwa-deftest niwa-pending-management-bindings ()
  ,test
  (test)
  :doc "retains clear-input and provides nonconflicting withdraw and Resume keys"
  (should (eq #'niwa-view-history-clear-input
              (lookup-key niwa-view--composer-keymap (kbd "C-c C-u"))))
  (should (eq #'niwa-view-retract-pending
              (lookup-key niwa-view--composer-keymap (kbd "C-c C-w"))))
  (should (eq #'niwa-view-resume-pending
              (lookup-key niwa-view--composer-keymap (kbd "C-c C-r"))))
  (dolist (key '("C-c C-w" "C-c C-r"))
    (should-not (lookup-key niwa-view--side-conversation-keymap (kbd key))))
  (niwa-pending-management-test--with-session
    (niwa-session-enqueue-pending-input session 'steering '(:input "keep"))
    (with-current-buffer view-buf
      (niwa-view--interaction-rebuild)
      (should (string-search "C-c C-w" (buffer-string)))
      (should (string-search "C-c C-r" (buffer-string))))))

(provide 'test-niwa-pending-inputs-management)
;;; test-niwa-pending-inputs-management.el ends here
