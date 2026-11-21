;;; test-niwa-claude-code-history.el --- Exact native checkpoints -*- lexical-binding: t -*-

;;; Commentary:
;; Check exact assistant IDs, independent child histories and native compaction.

;;; Code:

(require 'niwa-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-engine-test-support"))
(require 'niwa-claude-code-agent)
(require 'niwa-claude-code)
(require 'niwa-claude-code-session)

(niwa-deftest niwa-acp-turn--event/native-boundary ()
  (let ((owner (niwa-request--create)))
    (dolist (update '(((sessionUpdate . "agent_message_chunk") (messageId . "first"))
                      ((sessionUpdate . "agent_thought_chunk") (messageId . " exact:segment:2 "))
                      ((sessionUpdate . "user_message_chunk") (messageId . "user"))
                      ((sessionUpdate . "agent_message_chunk") (messageId . ""))
                      ((sessionUpdate . "agent_message_chunk"))))
      (niwa-acp-turn--event owner `((params . ((update . ,update))))))
    (should (equal " exact:segment:2 "
                   (plist-get (niwa-engine-info owner) :niwa-native-message-id)))))

(niwa-deftest niwa-claude-code-history-settle/native-boundary ()
  ,test
  (test)
  (dolist (child '(nil t))
    (let* ((owner (if child (niwa-agent-invocation--create :parent-turn 4)
                    (niwa-request--create :turn 4)))
           (prior '(:boundaries ((:turn 1 :id "old" :host "old-host" :directory "/old"
                                  :message-id "one" :input-boundary (1 . 2))
                                 (:turn 4 :detached t))))
           (original (copy-tree prior))
           (record (niwa-claude-code-history-open owner "new" (cons 2 3) prior)))
      (setf (niwa-engine-info owner)
            (plist-put (niwa-engine-info owner) :niwa-native-message-id "four"))
      (niwa-claude-code-history-settle owner prior record '(:status error))
      (should (equal (plist-get original :boundaries) (plist-get record :boundaries)))
      (dotimes (_ 2)
        (niwa-claude-code-history-settle owner prior record '(:status success)))
      (should (equal `((:turn 1 :id "old" :host "old-host" :directory "/old"
                        :message-id "one" :input-boundary (1 . 2))
                       (:turn 4 :id "new" :host ,(system-name)
                        :directory ,(expand-file-name niwa-claude-code-directory)
                        :message-id "four" :input-boundary (2 . 3)))
                     (plist-get record :boundaries)))
      (setcdr (plist-get record :input-boundary) 99)
      (should (equal '(2 . 3) (plist-get (cadr (plist-get record :boundaries)) :input-boundary)))
      (setcdr (plist-get (car (plist-get record :boundaries)) :input-boundary) 99)
      (should (equal prior original))))
  (let* ((owner (niwa-request--create :turn 1))
         (record (niwa-claude-code-history-open owner "no-id")))
    (niwa-claude-code-history-settle owner nil record '(:status success))
    (should-not (plist-get record :boundaries))))

(niwa-deftest niwa-claude-code-release-history/native-boundary (:quiet t)
  (niwa-engine-test--with-claude-session
    (dolist (scope '("root" "/root/child"))
      (dolist (state '(ready uncertain diverged))
        (let* ((history (niwa-claude-code-history-open request "native"))
               (boundaries (list (list :turn 0 :id "native" :message-id "prior-answer"
                                       :host (system-name) :directory root))))
          (plist-put history :state state)
          (plist-put history :boundaries (copy-tree boundaries))
          (setf (niwa-session-external-conversations session) (list (cons scope history)))
          (niwa-claude-code-release-history session scope)
          (let ((marker (alist-get scope (niwa-session-external-conversations session) nil nil #'equal)))
            (should (eq 'unstarted (plist-get marker :state)))
            (should-not (plist-get marker :id))
            (should (equal (append (unless (eq state 'diverged) boundaries)
                                   '((:turn 1 :detached t)))
                           (plist-get marker :boundaries))))
          (should (equal boundaries (plist-get history :boundaries))))))))

(niwa-deftest niwa-session-codec-validate-current-sidecar/native-boundary (:quiet t)
  (niwa-engine-test--with-session
    (dolist (state '(ready unstarted))
      (setf (niwa-session-external-conversations session)
            (list (append (list "root" :engine 'claude-code :state state)
                          (when (eq state 'ready)
                            (list :id "native" :host "fixture" :directory root)))))
      (let ((encoded (niwa-session-codec-serialize session)))
        (should (niwa-session-codec-validate-current-sidecar encoded))
        (dolist (boundaries '(((:turn 0 :detached t)
                               (:turn 1 :id "native" :message-id "exact" :host "fixture"
                                :directory "/tmp" :input-boundary (1 . 9)))
                              ((:turn 2 :id "native" :message-id "exact" :host "fixture" :directory "/tmp"))))
          (let ((valid (copy-tree encoded)))
            (plist-put (cdar (plist-get valid :external-conversations)) :boundaries boundaries)
            (should (niwa-session-codec-validate-current-sidecar valid))
            (should (equal boundaries
                           (plist-get (cdar (niwa-session-external-conversations
                                             (plist-get (niwa-session-codec-deserialize valid workspace) :session)))
                                      :boundaries)))))
        (dolist (bad '(wrong ((:turn -1 :detached t)) ((:turn 1 :detached maybe))
                            ((:turn 1 :id "native" :host "fixture" :directory "/tmp"))
                            ((:turn 1 :id "" :message-id "exact" :host "fixture" :directory "/tmp"))
                            ((:turn 1 :id "native" :message-id " " :host "fixture" :directory "/tmp"))
                            ((:turn 1 :id "native" :message-id "exact" :host "fixture" :directory "/tmp"
                              :input-boundary (1 . -1)))
                            ((:turn 1 :id "native" :message-id "exact" :host "fixture"))
                            ((:turn 1 :id "native" :message-id "exact" :directory "/tmp"))
                            ((:turn 1 :id "native" :message-id "exact" :host "fixture" :directory "relative"))
                            ((:turn 1 :id "native" :message-id "exact" :host "fixture"
                              :directory "/ssh:remote:/tmp"))
                            ((:turn 1 :detached t) (:turn 1 :detached t))))
          (let ((invalid (copy-tree encoded)))
            (plist-put (cdar (plist-get invalid :external-conversations)) :boundaries bad)
            (should-error (niwa-session-codec-validate-current-sidecar invalid))))))))

(niwa-deftest niwa--send-request/native-boundaries (:quiet t)
  (niwa-engine-test--with-claude-session
    (let (record invocation snapshot owner)
      (setq-local gptel-system-prompt "History fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-claude-code-launch)
                     (niwa-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta '((compactionEvents .
                                       [((sessionUpdate . "agent_message_chunk") (messageId . "exact-assistant")
                                         (content . ((type . "text") (text . "Native answer\n"))))
                                        ((sessionUpdate . "compaction_update") (compactionId . "history")
                                         (status . "completed")
                                         (summary . [((type . "text") (text . "Retained summary"))]))])))))))
            (dotimes (index 2)
              (goto-char (point-max))
              (niwa--insert-user-turn "Root input")
              (niwa--send-request "Root input")
              (setq owner niwa--current-request)
              (niwa-test--await 5 "Native root history did not settle" (not (niwa-turn-busy-p buffer)))
              (let* ((history (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal))
                     (boundaries (plist-get history :boundaries)))
                (should (= (1+ index) (length boundaries)))
                (should (equal "exact-assistant" (plist-get (car (last boundaries)) :message-id)))
                (when snapshot
                  (should (equal snapshot (car boundaries)))
                  (should (equal (list snapshot)
                                 (plist-get (plist-get (niwa-engine-info owner)
                                                      :niwa-native-before-history) :boundaries))))
                (setq snapshot (copy-tree (car boundaries)))))
            (niwa-agent-control-spawn
             session "history_child" "Child input" (lambda (value) (setq record (plist-get value :record)))
             :agent (niwa-agent--create :name "history_child" :description "Answer"
                                          :tools nil :max-turns nil :system-prompt "Child fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (niwa-test--await 5 "Native child history did not settle"
              (niwa-agent-invocation-runtime-settled-p invocation))
            (let* ((scope "/root/history_child")
                   (history (alist-get scope (niwa-session-external-conversations session) nil nil #'equal))
                   (old (copy-tree history)))
              (niwa-agent-control-send-message session scope "Follow up")
              (setq invocation (niwa-agent-record-invocation record))
              (niwa-test--await 5 "Native child follow-up did not settle"
                (niwa-agent-invocation-runtime-settled-p invocation))
              (should (equal old (plist-get (niwa-engine-info invocation) :niwa-native-before-history)))
              (should (= 1 (length (plist-get (alist-get scope (niwa-session-external-conversations session) nil nil #'equal)
                                            :boundaries))))
              (should (= (niwa-agent-invocation-parent-turn invocation)
                         (plist-get (car (plist-get history :boundaries)) :turn))))
            (should (= 2 (length (plist-get (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)
                                          :boundaries))))
            (niwa-claude-code-release-history session)
            (let ((marker (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)))
              (should (eq 'unstarted (plist-get marker :state)))
              (should (equal snapshot (car (plist-get marker :boundaries))))
              (should (equal '(:turn 3 :detached t) (car (last (plist-get marker :boundaries))))))
            (goto-char (point-max))
            (niwa--insert-user-turn "Return to native history")
            (niwa--send-request "Return to native history")
            (niwa-test--await 5 "Detached history did not restart" (not (niwa-turn-busy-p buffer)))
            (let ((boundaries (plist-get (alist-get "root" (niwa-session-external-conversations session) nil nil #'equal)
                                         :boundaries)))
              (should (= 3 (length boundaries)))
              (should (equal snapshot (car boundaries)))
              (should (= 2 (plist-get (cadr boundaries) :turn)))
              (should (plist-get (cadr boundaries) :message-id))
              (should (= 3 (plist-get (caddr boundaries) :turn)))))
        (when (and record (buffer-live-p (niwa-agent-record-conversation-buffer record)))
          (kill-buffer (niwa-agent-record-conversation-buffer record)))))))

(provide 'test-niwa-claude-code-history)
;;; test-niwa-claude-code-history.el ends here
