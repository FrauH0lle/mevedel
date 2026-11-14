;;; test-mevedel-session-takeover-agent-settlement.el --- Fenced agent settlement -*- lexical-binding: t -*-

;;; Commentary:

;; A real retained transcript and settlement callback must retire locally after
;; another client takes the lease, without publishing the old epoch's output.

;;; Code:

(require 'mevedel-agent-conversation)
(require 'mevedel-agent-runtime)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(mevedel-deftest mevedel-agent-runtime--settle/takeover (:quiet t)
  ,test
  (test)
  :doc "owned settlement publishes, takeover settles only local runtime state"
  (dolist (takeover '(nil t))
    (test-mevedel-publication-async--fixture
     (lambda (session root)
       (mevedel-tools-register)
       (test-mevedel-publication-async--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (let* ((relative "agents/busy.chat.org")
              (invocation
               (mevedel-agent-invocation--create
                :agent-id "busy" :path "/root/busy" :description "busy"
                :parent-session session :parent-data-buffer root
                :transcript-relative-path relative :transcript-status 'running
                :frozen-configuration
                (test-mevedel-session-persistence--agent-configuration)))
              (child (mevedel-agent-conversation-open invocation root))
              (record
               (mevedel-agent-record--create
                :id "busy" :path "/root/busy" :parent-path "/root"
                :role "default" :activity 'running :invocation invocation
                :configuration (mevedel-agent-invocation-frozen-configuration invocation)
                :conversation-buffer child :conversation-location relative))
              (original-publish
               (symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state))
              (publications 0) cancelled before head)
         (setf (mevedel-agent-invocation-buffer invocation) child
               (mevedel-agent-invocation-runtime-fsm invocation)
               (gptel-make-fsm :state 'WAIT :info (list :buffer child))
               (mevedel-agent-invocation-runtime-cancel invocation)
               (lambda () (setq cancelled t) nil)
               (mevedel-agent-invocation-runtime-settle-callback invocation)
               (apply-partially #'mevedel-agent-control--settle session record)
               (mevedel-session-agent-registry session)
               (list (cons "/root/busy" record))
               (mevedel-session-agent-transcripts session)
               `(("busy" :agent-path "/root/busy" :status running :path ,relative)))
         (with-current-buffer child
           (setq buffer-file-name
                 (expand-file-name relative (mevedel-session-save-path session)))
           (insert "* Agent Task: busy\n")
           (should (mevedel-agent-conversation-save invocation))
           (goto-char (point-max))
           (insert (propertize "Unpublished partial response.\n" 'gptel 'response))
           (setq before (buffer-substring (point-min) (point-max))))
         (when takeover
           (let ((successor (mevedel-session-create
                             "successor" (mevedel-session-workspace session)))
                 (mevedel-session-durability--client-id (make-string 64 ?b)))
             (setf (mevedel-session-save-path successor) (mevedel-session-save-path session)
                   (mevedel-session-session-id successor) (mevedel-session-session-id session))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
               (should (mevedel-session-durability-lease-acquire
                        (mevedel-session-save-path successor) "successor"
                        successor nil t)))
             (mevedel-session-durability--cancel-renewal successor))
           (setq head (mevedel-session-durability--lease-head
                       (mevedel-session-durability--lease-path
                        (mevedel-session-save-path session)))))
         (cl-letf (((symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args)
                      (cl-incf publications)
                      (apply original-publish args))))
           (if takeover
               (progn
                 (should-not (mevedel-session-durability-lease-renew session))
                 (mevedel-session-control-transfer-taken-over session head))
             (mevedel-agent-runtime-interrupt invocation "interrupted by user")))
         (should cancelled)
         (should (mevedel-agent-invocation-runtime-settled-p invocation))
         (should-not (mevedel-agent-invocation-runtime-fsm invocation))
         (should-not (mevedel-agent-invocation-runtime-cancel invocation))
         (should-not (mevedel-agent-control--active-p record))
         (should-not (mevedel-agent-record-invocation record))
         (should (buffer-live-p child))
         (if takeover
             (progn
               (should (= 0 publications))
               (should-not (mevedel-agent-conversation-save invocation t))
               (should-not (mevedel-agent-invocation-transcript-save-timer invocation))
               (should-not (mevedel-agent-conversation--write invocation))
               (should (eq 'following (plist-get (mevedel-session-control-transfer session) :state)))
               (should-not (mevedel-session-lease-renewal-timer session))
               (should (equal head (mevedel-session-durability--lease-head
                                    (mevedel-session-durability--lease-path
                                     (mevedel-session-save-path session)))))
               (with-current-buffer child
                 (should (equal-including-properties before (buffer-string)))
                 (should (buffer-modified-p)))
               (should-not (test-mevedel-publication-async--mail session)))
           (should (= 1 publications))
           (should (eq 'RESULT (plist-get (car (test-mevedel-publication-async--restored-mail session)) :type)))))))))

(provide 'test-mevedel-session-takeover-agent-settlement)
;;; test-mevedel-session-takeover-agent-settlement.el ends here
