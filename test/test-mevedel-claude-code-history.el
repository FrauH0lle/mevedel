;;; test-mevedel-claude-code-history.el --- Exact native checkpoints -*- lexical-binding: t -*-

;;; Commentary:
;; Check exact assistant IDs, independent child histories and native compaction.

;;; Code:

(require 'mevedel-engine-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-engine-test-support"))
(require 'mevedel-claude-code-agent)
(require 'mevedel-claude-code)
(require 'mevedel-claude-code-session)

(mevedel-deftest mevedel-acp-turn--event/native-boundary ()
  (let ((owner (mevedel-request--create)))
    (dolist (update '(((sessionUpdate . "agent_message_chunk") (messageId . "first"))
                      ((sessionUpdate . "agent_thought_chunk") (messageId . " exact:segment:2 "))
                      ((sessionUpdate . "user_message_chunk") (messageId . "user"))
                      ((sessionUpdate . "agent_message_chunk") (messageId . ""))
                      ((sessionUpdate . "agent_message_chunk"))))
      (mevedel-acp-turn--event owner `((params . ((update . ,update))))))
    (should (equal " exact:segment:2 "
                   (plist-get (mevedel-engine-info owner) :mevedel-native-message-id)))))

(mevedel-deftest mevedel-claude-code-history-settle/native-boundary ()
  ,test
  (test)
  (dolist (child '(nil t))
    (let* ((owner (if child (mevedel-agent-invocation--create :parent-turn 4)
                    (mevedel-request--create :turn 4)))
           (prior '(:boundaries ((:turn 1 :id "old" :host "old-host" :directory "/old"
                                  :message-id "one" :input-boundary (1 . 2))
                                 (:turn 4 :detached t))))
           (original (copy-tree prior))
           (record (mevedel-claude-code-history-open owner "new" (cons 2 3) prior)))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-native-message-id "four"))
      (mevedel-claude-code-history-settle owner prior record '(:status error))
      (should (equal (plist-get original :boundaries) (plist-get record :boundaries)))
      (dotimes (_ 2)
        (mevedel-claude-code-history-settle owner prior record '(:status success)))
      (should (equal `((:turn 1 :id "old" :host "old-host" :directory "/old"
                        :message-id "one" :input-boundary (1 . 2))
                       (:turn 4 :id "new" :host ,(system-name)
                        :directory ,(expand-file-name mevedel-claude-code-directory)
                        :message-id "four" :input-boundary (2 . 3)))
                     (plist-get record :boundaries)))
      (setcdr (plist-get record :input-boundary) 99)
      (should (equal '(2 . 3) (plist-get (cadr (plist-get record :boundaries)) :input-boundary)))
      (setcdr (plist-get (car (plist-get record :boundaries)) :input-boundary) 99)
      (should (equal prior original))))
  (let* ((owner (mevedel-request--create :turn 1))
         (record (mevedel-claude-code-history-open owner "no-id")))
    (mevedel-claude-code-history-settle owner nil record '(:status success))
    (should-not (plist-get record :boundaries))))

(mevedel-deftest mevedel-claude-code-release-history/native-boundary (:quiet t)
  (mevedel-engine-test--with-claude-session
    (dolist (scope '("root" "/root/child"))
      (dolist (state '(ready uncertain diverged))
        (let* ((history (mevedel-claude-code-history-open request "native"))
               (boundaries (list (list :turn 0 :id "native" :message-id "prior-answer"
                                       :host (system-name) :directory root))))
          (plist-put history :state state)
          (plist-put history :boundaries (copy-tree boundaries))
          (setf (mevedel-session-external-conversations session) (list (cons scope history)))
          (mevedel-claude-code-release-history session scope)
          (let ((marker (alist-get scope (mevedel-session-external-conversations session) nil nil #'equal)))
            (should (eq 'unstarted (plist-get marker :state)))
            (should-not (plist-get marker :id))
            (should (equal (append (unless (eq state 'diverged) boundaries)
                                   '((:turn 1 :detached t)))
                           (plist-get marker :boundaries))))
          (should (equal boundaries (plist-get history :boundaries))))))))

(mevedel-deftest mevedel-session-codec-validate-current-sidecar/native-boundary (:quiet t)
  (mevedel-engine-test--with-session
    (dolist (state '(ready unstarted))
      (setf (mevedel-session-external-conversations session)
            (list (append (list "root" :engine 'claude-code :state state)
                          (when (eq state 'ready)
                            (list :id "native" :host "fixture" :directory root)))))
      (let ((encoded (mevedel-session-codec-serialize session)))
        (should (mevedel-session-codec-validate-current-sidecar encoded))
        (dolist (boundaries '(((:turn 0 :detached t)
                               (:turn 1 :id "native" :message-id "exact" :host "fixture"
                                :directory "/tmp" :input-boundary (1 . 9)))
                              ((:turn 2 :id "native" :message-id "exact" :host "fixture" :directory "/tmp"))))
          (let ((valid (copy-tree encoded)))
            (plist-put (cdar (plist-get valid :external-conversations)) :boundaries boundaries)
            (should (mevedel-session-codec-validate-current-sidecar valid))
            (should (equal boundaries
                           (plist-get (cdar (mevedel-session-external-conversations
                                             (plist-get (mevedel-session-codec-deserialize valid workspace) :session)))
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
            (should-error (mevedel-session-codec-validate-current-sidecar invalid))))))))

(mevedel-deftest mevedel--send-request/native-boundaries (:quiet t)
  (mevedel-engine-test--with-claude-session
    (let (record invocation snapshot owner)
      (setq-local gptel-system-prompt "History fixture" gptel-tools nil)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-claude-code-launch)
                     (mevedel-engine-test--claude-launch
                      (lambda (_system mcp _model _effort &optional id _hook)
                        (list :meta '((compactionEvents .
                                       [((sessionUpdate . "agent_message_chunk") (messageId . "exact-assistant")
                                         (content . ((type . "text") (text . "Native answer\n"))))
                                        ((sessionUpdate . "compaction_update") (compactionId . "history")
                                         (status . "completed")
                                         (summary . [((type . "text") (text . "Retained summary"))]))])))))))
            (dotimes (index 2)
              (goto-char (point-max))
              (mevedel--insert-user-turn "Root input")
              (mevedel--send-request "Root input")
              (setq owner mevedel--current-request)
              (mevedel-test--await 5 "Native root history did not settle" (not (mevedel-turn-busy-p buffer)))
              (let* ((history (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal))
                     (boundaries (plist-get history :boundaries)))
                (should (= (1+ index) (length boundaries)))
                (should (equal "exact-assistant" (plist-get (car (last boundaries)) :message-id)))
                (when snapshot
                  (should (equal snapshot (car boundaries)))
                  (should (equal (list snapshot)
                                 (plist-get (plist-get (mevedel-engine-info owner)
                                                      :mevedel-native-before-history) :boundaries))))
                (setq snapshot (copy-tree (car boundaries)))))
            (mevedel-agent-control-spawn
             session "history_child" "Child input" (lambda (value) (setq record (plist-get value :record)))
             :agent (mevedel-agent--create :name "history_child" :description "Answer"
                                          :tools nil :max-turns nil :system-prompt "Child fixture")
             :on-invocation (lambda (value) (setq invocation value)))
            (mevedel-test--await 5 "Native child history did not settle"
              (mevedel-agent-invocation-runtime-settled-p invocation))
            (let* ((scope "/root/history_child")
                   (history (alist-get scope (mevedel-session-external-conversations session) nil nil #'equal))
                   (old (copy-tree history)))
              (mevedel-agent-control-send-message session scope "Follow up")
              (setq invocation (mevedel-agent-record-invocation record))
              (mevedel-test--await 5 "Native child follow-up did not settle"
                (mevedel-agent-invocation-runtime-settled-p invocation))
              (should (equal old (plist-get (mevedel-engine-info invocation) :mevedel-native-before-history)))
              (should (= 1 (length (plist-get (alist-get scope (mevedel-session-external-conversations session) nil nil #'equal)
                                            :boundaries))))
              (should (= (mevedel-agent-invocation-parent-turn invocation)
                         (plist-get (car (plist-get history :boundaries)) :turn))))
            (should (= 2 (length (plist-get (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal)
                                          :boundaries))))
            (mevedel-claude-code-release-history session)
            (let ((marker (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal)))
              (should (eq 'unstarted (plist-get marker :state)))
              (should (equal snapshot (car (plist-get marker :boundaries))))
              (should (equal '(:turn 3 :detached t) (car (last (plist-get marker :boundaries))))))
            (goto-char (point-max))
            (mevedel--insert-user-turn "Return to native history")
            (mevedel--send-request "Return to native history")
            (mevedel-test--await 5 "Detached history did not restart" (not (mevedel-turn-busy-p buffer)))
            (let ((boundaries (plist-get (alist-get "root" (mevedel-session-external-conversations session) nil nil #'equal)
                                         :boundaries)))
              (should (= 3 (length boundaries)))
              (should (equal snapshot (car boundaries)))
              (should (= 2 (plist-get (cadr boundaries) :turn)))
              (should (plist-get (cadr boundaries) :message-id))
              (should (= 3 (plist-get (caddr boundaries) :turn)))))
        (when (and record (buffer-live-p (mevedel-agent-record-conversation-buffer record)))
          (kill-buffer (mevedel-agent-record-conversation-buffer record)))))))

(provide 'test-mevedel-claude-code-history)
;;; test-mevedel-claude-code-history.el ends here
