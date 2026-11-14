;;; test-mevedel-session-takeover-agent-settlement.el --- Fenced agent settlement -*- lexical-binding: t -*-

;;; Commentary:

;; A real retained transcript and settlement callback must retire locally after
;; another client takes the lease, without publishing the old epoch's output.

;;; Code:

(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-publication)
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
                :lease-generation (plist-get (mevedel-session-lease session) :generation)
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

(mevedel-deftest mevedel-agent-runtime--settle/takeback (:quiet t)
  ,test
  (test)
  :doc "a deferred old terminal stays fenced after takeback; new turns publish"
  (test-mevedel-publication-async--fixture
   (lambda (session root)
     (mevedel-tools-register)
     (test-mevedel-publication-async--wait
      (lambda () (not (mevedel-session-publication-busy-p session))))
     (let* ((configuration (test-mevedel-session-persistence--agent-configuration))
            (run (symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state))
            (record (mevedel-agent-record--create
                     :id "busy" :path "/root/busy" :parent-path "/root"
                     :role "default" :activity 'idle :configuration configuration))
            callbacks cancelled invocation child before old-generation head
            (new-request (mevedel-request--create :session session :origin "/root/busy"))
            (publications 0))
       (setf (mevedel-session-agent-reservations session) (list (cons "/root/busy" record)))
       (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                  (lambda (callback _role _description owner buffer)
                    (push (cons owner callback) callbacks)
                    (setf (mevedel-agent-invocation-runtime-cancel owner)
                          (lambda () (setq cancelled t) 'deferred)
                          (mevedel-agent-invocation-runtime-fsm owner)
                          (gptel-make-fsm :state 'WAIT :info (list :buffer buffer)))))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (with-current-buffer root
           (setq invocation
                 (mevedel-agent-runtime-dispatch
                  nil "busy" "Keep working."
                  :path "/root/busy" :prepared-turn '(:prompt "Keep working.")
                  :frozen-configuration configuration
                  :on-invocation (apply-partially #'mevedel-agent-control--record-invocation session record)
                  :on-settle (apply-partially #'mevedel-agent-control--settle session record))))
         (setq child (mevedel-agent-invocation-buffer invocation)
               old-generation (plist-get (mevedel-session-lease session) :generation))
         (should (equal old-generation (mevedel-agent-invocation-lease-generation invocation)))
         (with-current-buffer child
           (goto-char (point-max))
           (insert (propertize "Old unpublished answer.\n" 'gptel 'response))
           (setq before (buffer-string)))
         (let ((successor (mevedel-session-create "successor" (mevedel-session-workspace session)))
               (mevedel-session-durability--client-id (make-string 64 ?b)))
           (setf (mevedel-session-save-path successor) (mevedel-session-save-path session)
                 (mevedel-session-session-id successor) (mevedel-session-session-id session))
           (should (mevedel-session-durability-lease-acquire
                    (mevedel-session-save-path successor) "successor" successor nil t))
           (mevedel-session-durability--cancel-renewal successor))
         (should-not (mevedel-session-durability-lease-renew session))
         (should cancelled)
         (should-not (mevedel-agent-invocation-runtime-settled-p invocation))
         (should (eq 'following (plist-get (mevedel-session-control-transfer session) :state)))
         ;; Takeback must not wait for the deferred provider acknowledgement.
         (should (mevedel-session-control-transfer-acquire session root t))
         (should (eq 'owned (plist-get (mevedel-session-lease session) :state)))
         (should-not (equal old-generation (plist-get (mevedel-session-lease session) :generation)))
         (setq head (mevedel-session-durability--lease-head
                     (mevedel-session-durability--lease-path (mevedel-session-save-path session))))
         ;; Isolate request-end's identity guard, not a full new provider turn.
         (with-current-buffer child
           (setq-local mevedel--agent-invocation
                       (mevedel-agent-invocation--create :buffer child :parent-session session)
                       mevedel--current-request new-request))
         (cl-letf (((symbol-function 'mevedel-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args) (cl-incf publications) (apply run args))))
           ;; Deliver through the real callback captured at provider dispatch.
           (funcall (cdr (assq invocation callbacks)) "Late old answer.")
           (should (mevedel-agent-invocation-runtime-settled-p invocation))
           (should (= 0 publications))
           (should (equal head (mevedel-session-durability--lease-head
                                (mevedel-session-durability--lease-path (mevedel-session-save-path session)))))
           (with-current-buffer child
             (should (eq new-request mevedel--current-request))
             (setq mevedel--current-request nil)
             (should (equal-including-properties before (buffer-string)))
             (should (buffer-modified-p)))
           (should-not (test-mevedel-publication-async--mail session))
           ;; A fresh turn in the newly owned epoch must still publish normally.
           (let ((fresh-record (mevedel-agent-record--create
                                :id "fresh" :path "/root/fresh" :parent-path "/root"
                                :role "default" :activity 'idle :configuration configuration))
                 fresh)
             (push (cons "/root/fresh" fresh-record) (mevedel-session-agent-reservations session))
             (with-current-buffer root
               (setq fresh
                     (mevedel-agent-runtime-dispatch
                      nil "fresh" "New work."
                      :path "/root/fresh" :prepared-turn '(:prompt "New work.")
                      :frozen-configuration configuration
                      :on-invocation (apply-partially #'mevedel-agent-control--record-invocation session fresh-record)
                      :on-settle (apply-partially #'mevedel-agent-control--settle session fresh-record))))
             (should-not (mevedel-agent-conversation-fenced-p fresh))
             (should (equal (plist-get (mevedel-session-lease session) :generation)
                            (mevedel-agent-invocation-lease-generation fresh)))
             ;; Unconfirmed loss with the same epoch remains recoverable.
             (setf (mevedel-session-lease session)
                   (plist-put (copy-sequence (mevedel-session-lease session)) :state 'lost))
             (should-not (mevedel-agent-conversation-fenced-p fresh))
             (should (mevedel-session-durability-lease-renew session))
             (funcall (cdr (assq fresh callbacks)) "New answer.")
             (should (= 1 publications))
             (should (mevedel-agent-invocation-runtime-settled-p fresh))
             (should (eq 'RESULT (plist-get (car (test-mevedel-publication-async--restored-mail session)) :type)))
             (let ((next
                    (with-current-buffer root
                      (mevedel-agent-runtime-dispatch
                       nil "followup" "Continue new work."
                       :path "/root/fresh" :frozen-configuration configuration
                       :retained-id (mevedel-agent-invocation-agent-id fresh)
                       :retained-buffer (mevedel-agent-invocation-buffer fresh)
                       :retained-transcript (mevedel-agent-invocation-transcript-relative-path fresh)
                       :on-invocation (apply-partially #'mevedel-agent-control--record-invocation session fresh-record)
                       :on-settle (apply-partially #'mevedel-agent-control--settle session fresh-record)))))
               (should-not (eq fresh next))
               (should (eq (mevedel-agent-invocation-buffer fresh)
                           (mevedel-agent-invocation-buffer next)))
               (should (equal (plist-get (mevedel-session-lease session) :generation)
                              (mevedel-agent-invocation-lease-generation next)))
               (funcall (cdr (assq next callbacks)) "Followup answer.")
               (should (= 2 publications))
               (should (mevedel-session-control-fs-wait-async 5))))))))))

(mevedel-deftest mevedel-agent-runtime--settle/reentrant-epoch (:quiet t)
  ,test
  (test)
  :doc "authority checks and staged callbacks cannot publish a superseded epoch"
  (dolist (boundary '(control publisher staged))
    (test-mevedel-publication-async--fixture
     (lambda (session root)
       (mevedel-tools-register)
       (test-mevedel-publication-async--wait (lambda () (not (mevedel-session-publication-busy-p session))))
       (let* ((configuration (test-mevedel-session-persistence--agent-configuration))
              (relative "agents/reentrant.chat.org")
              (invocation (mevedel-agent-invocation--create
                           :agent-id "busy" :path "/root/busy" :parent-session session :parent-data-buffer root
                           :lease-generation (plist-get (mevedel-session-lease session) :generation)
                           :transcript-relative-path relative :transcript-status 'running :frozen-configuration configuration))
              (child (mevedel-agent-conversation-open invocation root))
              (record (mevedel-agent-record--create
                       :id "busy" :path "/root/busy" :parent-path "/root" :role "default" :activity 'running
                       :invocation invocation :configuration configuration :conversation-buffer child :conversation-location relative))
              (authority (symbol-function 'mevedel-session-artifacts-assert-mutation-authority))
              (publish (symbol-function 'mevedel-session-publication-publish))
              (armed t) (publications 0) head before)
         (setf (mevedel-agent-invocation-buffer invocation) child
               (mevedel-agent-invocation-runtime-cancel invocation) (lambda () 'deferred)
               (mevedel-session-agent-registry session) (list (cons "/root/busy" record))
               (mevedel-session-agent-transcripts session) `(("busy" :agent-path "/root/busy" :status running :path ,relative)))
         (with-current-buffer child
           (setq buffer-file-name (expand-file-name relative (mevedel-session-save-path session)))
           (insert "* Agent Task: busy\n")
           (should (mevedel-agent-conversation-save invocation))
           (goto-char (point-max))
           (insert (propertize "Unpublished reentrant answer.\n" 'gptel 'response))
           (setq before (buffer-string)))
         (cl-labels
             ((takeback ()
                (setq armed nil)
                (let ((successor (mevedel-session-create "successor" (mevedel-session-workspace session)))
                      (mevedel-session-durability--client-id (make-string 64 ?b)))
                  (setf (mevedel-session-save-path successor) (mevedel-session-save-path session)
                        (mevedel-session-session-id successor) (mevedel-session-session-id session))
                  (should (mevedel-session-durability-lease-acquire (mevedel-session-save-path session) "successor" successor nil t))
                  (mevedel-session-durability--cancel-renewal successor))
                (should-not (mevedel-session-durability-lease-renew session))
                (should (mevedel-session-control-transfer-acquire session root t))
                (setq head (mevedel-session-durability--lease-head
                            (mevedel-session-durability--lease-path (mevedel-session-save-path session))))))
           (setf (mevedel-agent-invocation-runtime-settle-callback invocation)
                 (lambda (&rest args)
                   (let ((transaction (apply #'mevedel-agent-control--settle session record args)))
                     (when (and armed (eq boundary 'staged)) (takeback))
                     transaction)))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'mevedel-session-artifacts-assert-mutation-authority)
                      (lambda (owner &rest args)
                        (prog1 (apply authority owner args)
                          (when (and armed (eq owner session) (not (eq boundary 'staged))
                                     (eq (eq boundary 'publisher) (and (memq session mevedel-session-artifacts--terminal-sessions) t)))
                            (takeback)))))
                     ((symbol-function 'mevedel-session-publication-publish)
                      (lambda (&rest args) (cl-incf publications) (apply publish args))))
             (mevedel-agent-runtime--settle invocation "Finished old answer."))
           (should-not armed)
           (should (= 0 publications))
           (should (mevedel-agent-invocation-runtime-settled-p invocation))
           (should-not (mevedel-agent-control--active-p record))
           (should-not (test-mevedel-publication-async--mail session))
           (should (equal head (mevedel-session-durability--lease-head
                                (mevedel-session-durability--lease-path (mevedel-session-save-path session)))))
           (with-current-buffer child
             (should (equal-including-properties before (buffer-string)))
             (should (buffer-modified-p)))
           (should (mevedel-session-control-fs-wait-async 5))))))))

(provide 'test-mevedel-session-takeover-agent-settlement)
;;; test-mevedel-session-takeover-agent-settlement.el ends here
