;;; test-mevedel-pending-inputs-management.el --- Direct queue controls -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise composer withdrawal, inline deletion and explicit queue resumption.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-pending-inputs)
(require 'mevedel-mention-bindings)
(require 'mevedel-prompt-submission)
(require 'mevedel-recovery)
(require 'mevedel-structs)
(require 'mevedel-session-artifacts)
(require 'mevedel-tools)
(require 'mevedel-claude-code-context)
(require 'mevedel-hooks)
(require 'mevedel-mentions)
(require 'mevedel-reminders)
(require 'mevedel-turn)
(require 'gptel-openai)
(require 'mevedel-view)
(require 'mevedel-view-composer)
(require 'mevedel-view-interaction)

(defmacro mevedel-pending-management-test--with-session (&rest body)
  "Run BODY with a real paired composer and its SESSION."
  (declare (indent 0) (debug t))
  `(mevedel-view-test--with-buffers
     (let* ((workspace (mevedel-workspace--create
                        :type 'file :id "management" :name "management"
                        :root default-directory))
            (session (mevedel-session-create "main" workspace)))
       (mevedel-session-set-root-buffer session data-buf)
       (with-current-buffer data-buf
         (setq-local mevedel--session session
                     mevedel--workspace workspace
                     mevedel--view-buffer view-buf))
       (with-current-buffer view-buf
         (setq-local mevedel--session session))
       (cl-letf (((symbol-function 'mevedel-recovery-save) #'ignore)
                 ((symbol-function 'mevedel-recovery-save-now) #'ignore)
                 ((symbol-function 'mevedel-pending-inputs-schedule-held-input-delivery)
                  #'ignore))
         ,@body))))

(mevedel-deftest mevedel-view-retract-pending (:quiet t)
  ,test
  (test)
  :doc "withdraws newest acceptance across categories, independent of queue order"
  (mevedel-pending-management-test--with-session
    (let* ((first (mevedel-session-enqueue-pending-input
                   session 'steering '(:input "first" :queued-at-time 10)))
           (second (mevedel-session-enqueue-pending-input
                    session 'follow-up '(:input "second" :queued-at-time 10)))
           (third (mevedel-session-enqueue-pending-input
                   session 'steering '(:input "third" :queued-at-time 10))))
      (mevedel-session-set-pending-inputs session 'steering (list third first))
      (with-current-buffer view-buf
        (mevedel-view-retract-pending)
        (should (equal "third" (mevedel-view--input-text)))
        (should (equal (list first) (mevedel-session-pending-steering session)))
        (mevedel-view--clear-input)
        (mevedel-view-retract-pending)
        (should (equal "second" (mevedel-view--input-text)))
        (should-not (mevedel-session-pending-follow-ups session))
        (should-not (mevedel-session-pending-input-paused session))
        (should (eq 'follow-up (plist-get second :category))))))
  :doc "refuses even a whitespace draft without changing either queue"
  (mevedel-pending-management-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "queued"))))
      (with-current-buffer view-buf
        (goto-char (point-max))
        (insert " \n ")
        (should-error (mevedel-view-retract-pending) :type 'user-error)
        (should (equal " \n " (mevedel-view--visible-draft)))
        (should (eq entry (car (mevedel-session-pending-follow-ups session)))))))
  :doc "refuses the newest dispatching entry rather than withdrawing an older one"
  (mevedel-pending-management-test--with-session
    (mevedel-session-enqueue-pending-input session 'steering '(:input "old"))
    (dolist (flag '(:state :delivering))
      (let ((entry (mevedel-session-enqueue-pending-input
                    session 'follow-up (list :input "in flight" flag
                                            (if (eq flag :state) 'dispatching t)))))
        (with-current-buffer view-buf
          (should-error (mevedel-view-retract-pending) :type 'user-error)
          (should (string-empty-p (mevedel-view--input-text))))
        (should (memq entry (mevedel-session-pending-follow-ups session))))))
  :doc "restores bound mention text, attachment grants and reserved hook context"
  (mevedel-pending-management-test--with-session
    (let* ((text (copy-sequence "@file:picture.png"))
           (submission (mevedel-prompt-submission-create
                        :session session :state 'reserved
                        :context-entries '((:text "context"))))
           (path (expand-file-name "picture.png")))
      (mevedel-mention-bindings-set
       0 (length text) (list :kind 'file :token text :path path) text)
      (mevedel-session-enqueue-pending-input
       session 'steering (list :input text :submission submission
                              :dropped-file-grants (list path)))
      (with-current-buffer view-buf
        (mevedel-view-retract-pending)
        (should (equal-including-properties text (mevedel-view--input-text))))
      (should (equal (list path) (mevedel-session-dropped-file-grants session)))
      (should (equal '((:text "context"))
                     (mevedel-session-hook-context-pending session)))
      (should-not (mevedel-session-pending-steering session))))
  :doc "does not overwrite a draft typed while mutation authority is checked"
  (mevedel-pending-management-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input session 'follow-up '(:input "keep"))))
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-new-mutation-authority)
                   (lambda (_)
                     (goto-char (point-max)) (insert "> new draft\nsecond line"))))
          (should-error (mevedel-view-retract-pending) :type 'user-error))
        (should (equal "> new draft\nsecond line" (mevedel-view--input-text))))
      (should (eq entry (car (mevedel-session-pending-follow-ups session)))))))

(mevedel-deftest mevedel-pending-inputs--assert-retract-composer ()
  ,test
  (test)
  :doc "refuses a destination scope's saved draft even when the visible composer is empty"
  (mevedel-pending-management-test--with-session
    (with-current-buffer view-buf
      (let ((scope '(:directive-id "d1" :action discuss)))
        (puthash '("d1" discuss nil) '(:text "> saved\nsecond line")
                 mevedel-view--composer-drafts)
        (should-error (mevedel-pending-inputs--assert-retract-composer scope)
                      :type 'user-error)))))

(mevedel-deftest mevedel-pending-inputs--remove ()
  ,test
  (test)
  :doc "rechecks ownership after an authority check yields to dispatch"
  (mevedel-pending-management-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "racing"))))
      (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-new-mutation-authority)
                 (lambda (_) (plist-put entry :delivering t))))
        (should-error (mevedel-pending-inputs--remove session entry) :type 'user-error))
      (should (eq entry (car (mevedel-session-pending-follow-ups session))))))
  :doc "refuses foreign mutation authority before touching input or its context"
  (mevedel-pending-management-test--with-session
    (let ((entry (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "read only"))))
      (cl-letf (((symbol-function 'mevedel-session-artifacts-assert-new-mutation-authority)
                 (lambda (_) (user-error "Session is read-only"))))
        (should-error (mevedel-pending-inputs--remove session entry) :type 'user-error))
      (should (eq entry (car (mevedel-session-pending-follow-ups session)))))))

(mevedel-deftest mevedel-pending-inputs-drop-at-point ()
  ,test
  (test)
  :doc "d and DEL drop only the selected full or abbreviated row, preserving a draft"
  (mevedel-pending-management-test--with-session
    (save-window-excursion
      (switch-to-buffer view-buf)
      (mevedel-session-enqueue-pending-input session 'steering '(:input "short"))
      (mevedel-session-enqueue-pending-input session 'follow-up '(:input "long\nbody"))
      (goto-char (point-max))
      (insert "> draft\nsecond line")
      (dolist (pair '(("short" . "d") ("long" . "DEL")))
        (mevedel-view--interaction-rebuild)
        (goto-char (point-min))
        (search-forward (car pair))
        (let ((command (key-binding (kbd (cdr pair)))))
          (should (eq command #'mevedel-pending-inputs-drop-at-point))
          (call-interactively command))
        (should (equal "> draft\nsecond line" (mevedel-view--input-text)))
        (should-not (mevedel-session-pending-input-paused session)))
      (should-not (mevedel-session-pending-input-p session))))
  :doc "stale rows and dispatching inputs refuse deletion"
  (mevedel-pending-management-test--with-session
    (with-current-buffer view-buf
      (let ((entry (mevedel-session-enqueue-pending-input
                    session 'follow-up '(:input "racing" :state dispatching))))
        (mevedel-view--interaction-rebuild)
        (goto-char (point-min))
        (search-forward "racing")
        (should-error (mevedel-pending-inputs-drop-at-point) :type 'user-error)
        (mevedel-session-set-pending-inputs session 'follow-up nil)
        (should-error (mevedel-pending-inputs-drop-at-point) :type 'user-error)
        (should (eq 'dispatching (plist-get entry :state)))))))

(mevedel-deftest mevedel-view-resume-pending (:quiet t)
  ,test
  (test)
  :doc "starts one combined steering message, clears holds and then drains follow-ups"
  (mevedel-pending-management-test--with-session
    (mevedel-session-enqueue-pending-input
     session 'steering '(:input "first" :state failed-turn))
    (mevedel-session-enqueue-pending-input
     session 'steering '(:input "second" :state pending))
    (let ((later (mevedel-session-enqueue-pending-input
                  session 'follow-up '(:input "later")))
          messages)
      (mevedel-session-set-pending-input-paused session t)
      (mevedel-session-set-pending-input-failure-paused session t)
      (cl-letf (((symbol-function 'mevedel-view--submit-planned-input)
                 (lambda (input before _blocked _dispatch after &rest _)
                   (when before (funcall before))
                   (push input messages)
                   (funcall after))))
        (with-current-buffer view-buf (mevedel-view-resume-pending))
        (should (equal '("first\n\nsecond") messages))
        (should-not (mevedel-session-pending-steering session))
        (should-not (mevedel-session-pending-input-failure-paused session))
        (should-not (mevedel-session-pending-input-paused session))
        (should (equal (list later) (mevedel-session-pending-follow-ups session)))
        (mevedel-view--drain-follow-up data-buf)
        (should (equal '("later" "first\n\nsecond") messages))
        (should-not (mevedel-session-pending-follow-ups session)))))
  :doc "retains combined input after a blocked dispatch instead of losing steering"
  (mevedel-pending-management-test--with-session
    (mevedel-session-enqueue-pending-input session 'steering '(:input "keep"))
    (cl-letf (((symbol-function 'mevedel-view--submit-planned-input)
               (lambda (_input _before blocked &rest _) (funcall blocked))))
      (with-current-buffer view-buf (mevedel-view-resume-pending)))
    (should (equal "keep" (plist-get (car (mevedel-session-pending-follow-ups session)) :input)))
    (should-not (plist-get (car (mevedel-session-pending-follow-ups session)) :delivering)))
  :doc "refuses a running turn or a draft without releasing the interrupt hold"
  (mevedel-pending-management-test--with-session
    (mevedel-session-enqueue-pending-input session 'steering '(:input "keep"))
    (mevedel-session-set-pending-input-paused session t)
    (with-current-buffer data-buf
      (setq-local mevedel--current-request (mevedel-request--create :id "active")))
    (with-current-buffer view-buf
      (should-error (mevedel-view-resume-pending) :type 'user-error)
      (with-current-buffer data-buf (setq-local mevedel--current-request nil))
      (goto-char (point-max)) (insert "draft")
      (should-error (mevedel-view-resume-pending) :type 'user-error))
    (should (mevedel-session-pending-input-paused session))))

(mevedel-deftest mevedel-view-resume-pending/mixed-preparation (:quiet t)
  (mevedel-pending-management-test--with-session
    (let ((live (mevedel-prompt-submission-create :session session :state 'reserved))
          hooks forwarded)
      (mevedel-session-enqueue-pending-input
       session 'steering '(:input "COLD" :state failed-turn))
      (mevedel-session-enqueue-pending-input
       session 'steering (list :input "LIVE" :model-input "FROZEN-LIVE"
                              :transcript-payload "LIVE" :submission live))
      (mevedel-session-set-pending-input-failure-paused session t)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event data callback &rest _)
                   (should (eq event 'UserPromptSubmit))
                   (push (plist-get data :prompt) hooks)
                   (funcall callback '(:continue t))))
                ((symbol-function 'mevedel-view--forward-input-now)
                 (lambda (_input &rest keys)
                   (setq forwarded (plist-get keys :model-input))
                   (mevedel-prompt-submission-commit (plist-get keys :submission))
                   (funcall (plist-get keys :after-insert)))))
        (with-current-buffer view-buf (mevedel-view-resume-pending)))
      (should (equal '("COLD") hooks))
      (should (equal "COLD\n\nFROZEN-LIVE" forwarded))
      (should (eq 'reserved (mevedel-prompt-submission-state live)))
      (should-not (mevedel-session-pending-input-p session)))))

(mevedel-deftest mevedel-pending-inputs-cancel-resume (:quiet t)
  (mevedel-pending-management-test--with-session
    (let ((live (mevedel-prompt-submission-create :session session :state 'reserved))
          callback cold)
      (mevedel-session-enqueue-pending-input
       session 'steering (list :input "LIVE" :model-input "FROZEN-LIVE"
                              :transcript-payload "LIVE" :submission live))
      (setq cold (mevedel-session-enqueue-pending-input
                  session 'steering '(:input "COLD" :state failed-turn)))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _data done &rest _) (setq callback done))))
        (with-current-buffer view-buf
          (mevedel-view-resume-pending)
          (should mevedel-view--prompt-hook-pending)
          (should (plist-get cold :delivering))
          (mevedel-view-abort)
          (should-not mevedel-view--prompt-hook-pending)
          (should-not (plist-get cold :delivering))
          (funcall callback '(:continue t))
          (should-not (plist-get cold :submission))
          (should (= 2 (length (mevedel-session-pending-steering session))))
          (mevedel-view-retract-pending)
          (should (equal "COLD" (mevedel-view--input-text)))))
      (should (eq 'reserved (mevedel-prompt-submission-state live))))))

(mevedel-deftest mevedel-view-resume-pending/paused-goal (:quiet t)
  (mevedel-pending-management-test--with-session
    (let* ((goal (mevedel-goal--create :id "goal" :status 'paused))
           (later (mevedel-session-enqueue-pending-input
                   session 'follow-up '(:input "goal follow-up" :queued-at-goal-id "goal")))
           messages)
      (setf (mevedel-session-goal session) goal)
      (mevedel-session-enqueue-pending-input session 'steering '(:input "explicit steering"))
      (mevedel-session-set-pending-input-paused session t)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _data callback &rest _) (funcall callback '(:continue t))))
                ((symbol-function 'mevedel-view--forward-input-now)
                 (lambda (input &rest keys)
                   (push input messages)
                   (mevedel-prompt-submission-commit (plist-get keys :submission))
                   (funcall (plist-get keys :after-insert)))))
        (with-current-buffer view-buf (mevedel-view-resume-pending))
        (mevedel-view--drain-follow-up data-buf))
      (should (equal '("explicit steering") messages))
      (should (eq 'paused (mevedel-goal-status goal)))
      (should (equal (list later) (mevedel-session-pending-follow-ups session))))))

(mevedel-deftest mevedel-pending-inputs--resume-entry ()
  ,test
  (test)
  :doc "preserves prepared model text, transcript, audits, skills, grants and context"
  (mevedel-pending-management-test--with-session
    (let* ((context '((:text "retained")))
           (one (mevedel-prompt-submission-create
                 :session session :state 'reserved :context-entries context))
           (two (mevedel-prompt-submission-create :session session :state 'reserved))
           (first (mevedel-session-enqueue-pending-input
                   session 'steering
                   (list :input "first" :model-input "expanded first"
                         :transcript-payload "stored first" :submission one
                         :hook-audits '((:event first))
                         :request-context '(:invoked-skills ((:name "one")))
                         :dropped-file-grants '("/tmp/one"))))
           (second (mevedel-session-enqueue-pending-input
                    session 'steering
                    (list :input "second" :model-input "expanded second"
                          :transcript-payload "stored second" :submission two
                          :hook-audits '((:event second))
                          :request-context '(:invoked-skills ((:name "two")))
                          :dropped-file-grants '("/tmp/two"))))
           (combined (mevedel-pending-inputs--resume-entry session (list first second)))
           (submission (plist-get combined :submission))
           (outcome (mevedel-prompt-submission-outcome submission)))
      (should (equal "first\n\nsecond" (plist-get combined :input)))
      (should (equal "expanded first\n\nexpanded second" (plist-get outcome :model-input)))
      (should (equal "stored first\n\nstored second" (plist-get outcome :transcript-input)))
      (should (equal '((:event first) (:event second)) (plist-get outcome :hook-audits)))
      (should (equal '((:name "one") (:name "two"))
                     (plist-get (plist-get outcome :request-context) :invoked-skills)))
      (should (equal '("/tmp/one" "/tmp/two") (plist-get combined :dropped-file-grants)))
      (should (equal context (mevedel-prompt-submission-context-entries submission)))
      (mevedel-prompt-submission-restore submission)
      (should (equal context (mevedel-session-hook-context-pending session))))))

(mevedel-deftest mevedel-pending-inputs--save-follow-up ()
  (mevedel-pending-management-test--with-session
    (let* ((context '((:text "retained")))
           (submission (mevedel-prompt-submission-create
                        :session session :state 'reserved :context-entries context))
           (entry (list :input "combined" :submission submission))
           replacement)
      (with-current-buffer view-buf
        (cl-letf (((symbol-function 'mevedel-pending-inputs--finish-replacement)
                   (lambda (_state value) (setq replacement value))))
          (mevedel-pending-inputs--save-follow-up (list :entry entry) "edited" session)))
      (should (equal "edited" (plist-get replacement :input)))
      (should-not (plist-get replacement :submission))
      (should (equal context (mevedel-session-hook-context-pending session)))
      (should (eq 'pending (mevedel-prompt-submission-state submission))))))

(mevedel-deftest mevedel-tools-steering-claim ()
  ,test
  (test)
  :doc "reserves the complete snapshot before expansion can run a withdrawal command"
  (mevedel-pending-management-test--with-session
    (let ((first (mevedel-session-enqueue-pending-input session 'steering '(:input "first")))
          (second (mevedel-session-enqueue-pending-input session 'steering '(:input "second"))))
      (mevedel-tools-steering-claim session (list first second))
      (dolist (entry (list first second))
        (should (plist-get entry :delivering))
        (should-error (mevedel-pending-inputs--remove session entry) :type 'user-error))
      (with-current-buffer view-buf
        (should-error (mevedel-view-retract-pending) :type 'user-error))))
  :doc "rejects a stale snapshot without partially reserving its remaining entries"
  (mevedel-pending-management-test--with-session
    (let ((first (mevedel-session-enqueue-pending-input session 'steering '(:input "first")))
          (second (mevedel-session-enqueue-pending-input session 'steering '(:input "second"))))
      (mevedel-session-set-pending-inputs session 'steering (list first))
      (should-error (mevedel-tools-steering-claim session (list first second)) :type 'user-error)
      (should-not (plist-get first :delivering)))))

(mevedel-deftest mevedel-tools--handle-steering-inject/claim-release (:quiet t)
  (mevedel-pending-management-test--with-session
    (dolist (input '("first" "second"))
      (mevedel-session-enqueue-pending-input
       session 'steering (list :input input :request-id "active")))
    (let ((fsm (gptel-make-fsm :info (list :buffer data-buf :history '(TRET)
                                         :mevedel-request-id "active"))))
      (cl-letf (((symbol-function 'mevedel-tools-steering-expand)
                 (lambda (&rest _) (error "Injected expansion failure"))))
        (should-error (mevedel-tools--handle-steering-inject fsm t)))
      (dolist (entry (mevedel-session-pending-steering session))
        (should-not (plist-get entry :delivering)))
      (with-current-buffer view-buf
        (mevedel-view-retract-pending)
        (should (equal "second" (mevedel-view--input-text)))))))

(mevedel-deftest mevedel-tools--handle-steering-inject/post-injection-error ()
  (mevedel-pending-management-test--with-session
    (let* ((gptel--known-backends (copy-tree gptel--known-backends))
           (backend (gptel-make-openai "Claim test" :stream nil :key "unused"
                                       :host "example.test" :models '(steering-test)))
           (data (list :messages []))
           (first (mevedel-session-enqueue-pending-input
                   session 'steering '(:input "first" :request-id "active")))
           (second (mevedel-session-enqueue-pending-input
                    session 'steering '(:input "second" :request-id "active")))
           (fsm (gptel-make-fsm :info (list :buffer data-buf :history '(TRET)
                                          :backend backend :data data :model 'steering-test
                                          :mevedel-request-id "active"))))
      (cl-letf (((symbol-function 'mevedel-tools-steering-expand)
                 (lambda (&rest _)
                   '(:text "sent" :reminder-items ((:key test :body "reminder")))))
                ((symbol-function 'mevedel-reminders-stage-entry)
                 (lambda (&rest _) (error "Injected reminder staging failure"))))
        (should-error (mevedel-tools--handle-steering-inject fsm t)))
      (should (equal "sent" (plist-get (aref (plist-get data :messages) 0) :content)))
      (should (plist-get first :delivering))
      (should-not (plist-get second :delivering))
      (should-error (mevedel-pending-inputs--remove session first) :type 'user-error)
      (mevedel-pending-inputs--remove session second)
      (should (equal (list first) (mevedel-session-pending-steering session))))))

(mevedel-deftest mevedel-tools-steering-expand/abort-reentry (:quiet t)
  (dolist (native '(nil t))
    (mevedel-pending-management-test--with-session
      (let* ((request (mevedel-request--create :id "active" :session session))
             (info (list :buffer data-buf :history '(TRET) :mevedel-request-id "active"))
             (owner (if native request (gptel-make-fsm :info info))))
        (when native (setf (mevedel-engine-info owner) info))
        (mevedel-session-enqueue-pending-input
         session 'steering '(:input "keep" :request-id "active"))
        (cl-letf (((symbol-function 'mevedel-mentions-expand-user-input)
                   (lambda (&rest _)
                     (mevedel--turn-fail-pending-input owner 'aborted)
                     '(:text "stale expansion"))))
          (should-error (if native (mevedel-claude-code-context--steering owner)
                          (mevedel-tools--handle-steering-inject owner t))
                        :type 'user-error))
        (let ((entry (car (mevedel-session-pending-steering session))))
          (should-not (plist-get entry :request-id))
          (should-not (plist-get entry :delivering)))
        (with-current-buffer view-buf
          (mevedel-view-retract-pending)
          (should (equal "keep" (mevedel-view--input-text))))))))

(mevedel-deftest mevedel-pending-inputs-cockpit-holds-p ()
  (mevedel-pending-management-test--with-session
    (should-not (mevedel-pending-inputs-cockpit-holds-p session))
    (let ((cockpit (get-buffer-create mevedel-pending-inputs-buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer cockpit
              (setq-local mevedel-cockpit--context (list :session session)))
            (should (mevedel-pending-inputs-cockpit-holds-p session))
            (should-not (mevedel-pending-inputs-cockpit-holds-p
                         (mevedel-session-create "other" workspace))))
        (kill-buffer cockpit)))))

(mevedel-deftest mevedel-pending-management-bindings ()
  ,test
  (test)
  :doc "retains clear-input and provides nonconflicting withdraw and Resume keys"
  (should (eq #'mevedel-view-history-clear-input
              (lookup-key mevedel-view--composer-keymap (kbd "C-c C-u"))))
  (should (eq #'mevedel-view-retract-pending
              (lookup-key mevedel-view--composer-keymap (kbd "C-c C-w"))))
  (should (eq #'mevedel-view-resume-pending
              (lookup-key mevedel-view--composer-keymap (kbd "C-c C-r"))))
  (dolist (key '("C-c C-w" "C-c C-r"))
    (should-not (lookup-key mevedel-view--side-conversation-keymap (kbd key))))
  (mevedel-pending-management-test--with-session
    (mevedel-session-enqueue-pending-input session 'steering '(:input "keep"))
    (with-current-buffer view-buf
      (mevedel-view--interaction-rebuild)
      (should (string-search "C-c C-w" (buffer-string)))
      (should (string-search "C-c C-r" (buffer-string))))))

(provide 'test-mevedel-pending-inputs-management)
;;; test-mevedel-pending-inputs-management.el ends here
