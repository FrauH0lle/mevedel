;;; test-niwa-session-rewind-native.el --- Native Rewind preparation -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise native preparation callbacks against real published session heads.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-session-rewind)
(require 'niwa-session-artifacts)
(require 'niwa-session-publication)
(require 'niwa-session-durability)
(require 'niwa-chat)
(require 'niwa-acp)
(require 'niwa-claude-code)
(require 'niwa-claude-code-fork)
(require 'niwa-engine)

(niwa-deftest niwa-engine-record-history-edit/detached ()
  ,test
  (test)
  :doc "editing a detached root or child prevents native boundary resurrection"
  (dolist (scope '("root" "/root/child"))
    (with-temp-buffer
      (org-mode)
      (insert "Prompt\n" (propertize "Native answer\n" 'gptel 'response) "Draft")
      (let* ((child (not (equal scope "root")))
             (record (list :engine 'claude-code :state 'unstarted
                           :boundaries
                           (list (list :turn 1 :id "old" :message-id "answer"
                                       :host (system-name) :directory "/native"
                                       :input-boundary (cons (if child 0 1) 7))
                                 '(:turn 2 :detached t)))))
        (setq-local niwa--session
                    (niwa-session--create
                     :current-segment 1
                     :external-conversations (list (cons scope record))
                     :agent-transcripts (when child (list (list "child-id" :agent-path scope :parent-turn 1)))))
        (setq-local niwa--agent-invocation
                    (when child (niwa-agent-invocation--create :path scope)))
        (add-hook 'before-change-functions #'niwa-engine-record-history-edit nil t)
        (goto-char (point-max))
        (insert " more draft")
        (should (= 2 (length (plist-get record :boundaries))))
        (goto-char (point-min))
        (delete-char 1)
        (should (eq 'unstarted (plist-get record :state)))
        (should (equal '((:turn 2 :detached t)) (plist-get record :boundaries)))
        (should-not (niwa-session-rewind--reduce-native-histories niwa--session 1))))))

(niwa-deftest niwa-session-rewind--reduce-native-histories ()
  ,test
  (test)
  :doc "selects exact native boundaries, detachments, and surviving child scopes"
  (let* ((root '(:engine claude-code :id "later" :state ready
                :boundaries ((:turn 2 :id "early" :message-id "answer"
                              :host "original-host" :directory "/original")
                             (:turn 4 :detached t)
                             (:turn 6 :id "later" :message-id "last"
                              :host "later-host" :directory "/later"))))
         (child '(:engine claude-code :id "child" :state ready
                 :boundaries ((:turn 2 :id "child" :message-id "child-answer"
                               :host "child-host" :directory "/child"))))
         (idle '(:engine claude-code :id "idle" :state ready
                 :boundaries ((:turn 1 :id "idle" :message-id "idle-answer"
                               :host "idle-host" :directory "/idle"))))
         (session (niwa-session--create
                   :external-conversations (list (cons "root" root)
                                                 (cons "/kept" child)
                                                 (cons "/gone" child)
                                                 (cons "/idle" idle))
                   :agent-transcripts '(("kept-id" :agent-path "/kept" :parent-turn 1 :status completed)
                                        ("idle-id" :agent-path "/idle" :parent-turn 1 :status completed)))))
    (should-not (assoc "/idle" (niwa-session-agent-transcripts session)))
    (should (equal '("/idle") (mapcar #'car (niwa-session-rewind--reduce-native-histories session 1))))
    (let ((reduced (niwa-session-rewind--reduce-native-histories session 3)))
      (should (equal '("root" "/kept" "/idle") (mapcar #'car reduced)))
      (should (eq 'ready (plist-get (cdr (assoc "/idle" reduced)) :state)))
      (should (equal "early" (plist-get (cdar reduced) :id)))
      (should (equal "original-host" (plist-get (cdar reduced) :host)))
      (should (equal "/original" (plist-get (cdar reduced) :directory)))
      (should (eq 'uncertain (plist-get (cdar reduced) :state)))
      (should (= 1 (length (plist-get (cdar reduced) :boundaries)))))
    (plist-put idle :state 'uncertain)
    (should (eq 'uncertain
                (plist-get (cdr (assoc "/idle" (niwa-session-rewind--reduce-native-histories session 3))) :state)))
    (plist-put idle :state 'ready)
    (plist-put idle :boundaries
               (append (plist-get idle :boundaries)
                       '((:turn 4 :id "later-child" :message-id "later-answer"
                          :host "child-host" :directory "/child"))))
    (should (eq 'uncertain
                (plist-get (cdr (assoc "/idle" (niwa-session-rewind--reduce-native-histories session 3))) :state)))
    (should (eq 'ready
                (plist-get (cdr (assoc "/idle" (niwa-session-rewind--reduce-native-histories session 1))) :state)))
    (should (eq 'ready (plist-get (cdar (niwa-session-rewind--reduce-native-histories session 2)) :state)))
    (let ((detached (cdar (niwa-session-rewind--reduce-native-histories session 5))))
      (should (eq 'unstarted (plist-get detached :state)))
      (should-not (plist-get detached :id)))
    (should-not (niwa-session-rewind--reduce-native-histories session 0))
    (should (equal "later" (plist-get root :id)))
    (plist-put root :state 'diverged)
    (let (failure)
      (niwa-claude-code-fork-histories
       (niwa-session-rewind--reduce-native-histories session 3)
       (lambda (_histories error _cleanup) (setq failure error)))
      (should (string-match-p "transcript was edited" failure)))
    (setf (niwa-session-external-conversations session)
          '(("root" :engine claude-code :id "legacy" :state ready)))
    (should (string-match-p
             "No recorded native answer boundary"
             (error-message-string
              (should-error (niwa-session-rewind--reduce-native-histories session 3)))))))

(niwa-deftest niwa-session-rewind-rewind/native-committed ()
  ,test
  (test)
  :doc "synchronous Rewind and Redo keep their committed marker local"
  (let ((niwa-session-rewind--native-committed 'outer))
    (cl-letf (((symbol-function 'niwa-session-rewind--rewind)
               (lambda (&rest _)
                 (should-not niwa-session-rewind--native-committed)
                 (setq niwa-session-rewind--native-committed t)))
              ((symbol-function 'niwa-session-rewind--restore-head)
               (lambda (&rest _)
                 (should-not niwa-session-rewind--native-committed)
                 (setq niwa-session-rewind--native-committed t))))
      (should (niwa-session-rewind-rewind (current-buffer) nil))
      (should (eq 'outer niwa-session-rewind--native-committed))
      (should (niwa-session-rewind-restore-head nil (current-buffer) nil))
      (should (eq 'outer niwa-session-rewind--native-committed)))))

(niwa-deftest niwa-session-rewind--prepare-native (:quiet t)
  ,test
  (test)
  :doc "prepares asynchronously, rejects races, and cleans only uncommitted forks"
  (dolist (scenario '(success uncertain detached source-race head-race cas-failure precommit postcommit redo))
    (cl-destructuring-bind (workspace . directory)
        (test-niwa-session-persistence--make-tempdir-workspace)
      (let* ((session (niwa-session-create "native" workspace))
             (buffer (generate-new-buffer " *native-rewind*"))
             (niwa-claude-code-directory directory)
             (niwa-claude-code-fork--sessions nil)
             (publish (symbol-function 'niwa-session-publication-publish))
             (commit-head (symbol-function 'niwa-session-durability-commit-publication-head))
             (stage (symbol-function 'niwa-session-rewind--stage-rewind))
             pending deleted completion target original-head
             (cas-attempts 0) (file-preparations 0) (confirmations 0))
        (unwind-protect
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session niwa--workspace workspace)
              (setq-local gptel-backend (test-niwa-session-persistence--agent-backend))
              (setq-local gptel-model 'test-model)
              (gptel-mode 1)
              (dotimes (index (if (eq scenario 'uncertain) 3 2))
                (let ((turn (1+ index)))
                  (goto-char (point-max))
                  (insert (format "Prompt %d\n" turn)
                          (propertize (format "Answer %d\n" turn) 'gptel 'response))
                  (setf (niwa-session-turn-count session) turn
                        (niwa-session-external-conversations session)
                        (list (cons "root"
                                    (list :engine 'claude-code :id "old" :host (system-name)
                                          :directory directory :state 'ready
                                          :boundaries
                                          (cl-loop for n from 1 to turn
                                                   collect (list :turn n :id "old"
                                                                 :host (system-name) :directory directory
                                                                 :message-id (format "answer-%d" n)))))))
                  (when (eq scenario 'uncertain)
                    (let ((record (cdar (niwa-session-external-conversations session))))
                      (plist-put record :state 'uncertain)
                      (plist-put record :boundaries (list (car (plist-get record :boundaries))))))
                  (when (and (eq scenario 'detached) (= turn 2))
                    (setf (niwa-session-external-conversations session)
                          (list (cons "root"
                                      (list :engine 'claude-code :state 'unstarted
                                            :boundaries
                                            (list (car (plist-get (cdar (niwa-session-external-conversations session))
                                                                  :boundaries))
                                                  '(:turn 2 :detached t)))))))
                  (niwa-session-artifacts-save session buffer t)))
              (setq original-head (plist-get (niwa-session-publication session) :head)
                    target (cdr (cl-find-if
                                 (lambda (entry) (= (if (eq scenario 'uncertain) 2 1)
                                                    (plist-get (cdr entry) :cum-turn)))
                                 (niwa-session-rewind--prompt-candidates session))))
              (cl-letf (((symbol-function 'niwa-claude-code-launch)
                         (lambda (&rest _) '(:meta nil)))
                        ((symbol-function 'niwa-acp-session-operation)
                         (lambda (launch operation id callback)
                           (if (eq operation 'delete)
                               (progn (push id deleted) (funcall callback '(:status success)))
                             (should (equal "old" id))
                             (should (alist-get 'messageId
                                               (alist-get 'fork (alist-get 'air
                                                 (alist-get 'jetbrains (plist-get launch :meta))))))
                             (setq pending callback))))
                        ((symbol-function 'niwa-session-publication-publish)
                         (lambda (&rest args)
                           (prog1 (apply publish args)
                             (when (eq scenario 'postcommit) (error "Injected postcommit failure")))))
                        ((symbol-function 'niwa-session-durability-commit-publication-head)
                         (lambda (&rest args)
                           (if (eq scenario 'cas-failure)
                               (progn (cl-incf cas-attempts) nil)
                             (apply commit-head args))))
                        ((symbol-function 'niwa-session-rewind--stage-rewind)
                         (lambda (&rest args)
                           (when (eq scenario 'precommit) (error "Injected staging failure"))
                           (apply stage args)))
                        ((symbol-function 'niwa-session-rewind--prepare-buffers-for-restore)
                         (lambda (_candidate _turn plan &rest _)
                           (should-not (memq session niwa-claude-code-fork--sessions))
                           (cl-incf file-preparations)
                           plan))
                        ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
                (should (eq :pending
                            (niwa-session-rewind-rewind
                             buffer target 'after 'conversation (lambda (_) t)
                             (lambda (result error) (setq completion (list result error))))))
                (should-not completion)
                (should (= (if (eq scenario 'uncertain) 3 2) (niwa-session-turn-count session)))
                (pcase scenario
                  ('source-race (insert "changed"))
                  ('head-race
                   (funcall publish session
                            (list (list :path (niwa-session-artifacts-sidecar-path
                                               (niwa-session-save-path session))
                                        :content (niwa-session-artifacts-printed-value
                                                  (niwa-session-artifacts--live-sidecar session buffer))
                                        :commit-marker t)) t)
                   (should-not (equal original-head
                                      (plist-get (niwa-session-publication-read
                                                  (niwa-session-save-path session)) :head)))))
                (funcall pending '(:status success :session-id "fresh"))
                (should-not niwa-claude-code-fork--sessions)
                (if (memq scenario '(success uncertain detached redo))
                    (progn
                      (should (equal '(t nil) completion))
                      (should (= (if (eq scenario 'uncertain) 2 1) (niwa-session-turn-count session)))
                      (when (eq scenario 'uncertain)
                        (should (eq 'uncertain (plist-get (cdar (niwa-session-external-conversations session)) :state))))
                      (should (equal "fresh" (plist-get (cdar (niwa-session-external-conversations session)) :id))))
                  (unless (stringp (cadr completion))
                    (ert-fail (list :scenario scenario :completion completion))))
                (should (equal (unless (memq scenario '(success uncertain detached redo postcommit)) '("fresh")) deleted))
                (when (eq scenario 'cas-failure) (should (= 1 cas-attempts)))
                (when (eq scenario 'redo)
                  (setq completion nil)
                  (should (eq :pending (niwa-session-rewind-restore-head
                                        session buffer original-head
                                        (lambda (_impact)
                                          (should-not (memq session niwa-claude-code-fork--sessions))
                                          (cl-incf confirmations) t)
                                        nil
                                        (lambda (result error) (setq completion (list result error))))))
                  (should (= 1 file-preparations))
                  (should (= 1 confirmations))
                  (should-not completion)
                  (funcall pending '(:status success :session-id "redo-fresh"))
                  (should (= 1 file-preparations))
                  (should (= 1 confirmations))
                  (should (equal '(t nil) completion))
                  (should (= 2 (niwa-session-turn-count session)))
                  (should (equal "redo-fresh" (plist-get (cdar (niwa-session-external-conversations session)) :id))))))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (delete-directory directory t)
          (niwa-workspace-clear-registry))))))

(provide 'test-niwa-session-rewind-native)
;;; test-niwa-session-rewind-native.el ends here
