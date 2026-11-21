;;; test-niwa-session-takeover-agent-settlement.el --- Fenced agent settlement -*- lexical-binding: t -*-

;;; Commentary:

;; A real retained transcript and settlement callback must retire locally after
;; another client takes the lease, without publishing the old epoch's output.

;;; Code:

(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-agent-runtime)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-rewind)
(require 'niwa-utilities)
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))

(niwa-deftest niwa-agent-runtime--settle/takeover (:quiet t)
  ,test
  (test)
  :doc "owned settlement publishes, takeover settles only local runtime state"
  (dolist (takeover '(nil t))
    (test-niwa-publication-async--fixture
     (lambda (session root)
       (niwa-tools-register)
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (let* ((relative "agents/busy.chat.org")
              (invocation
               (niwa-agent-invocation--create
                :agent-id "busy" :path "/root/busy" :description "busy"
                :parent-session session :parent-data-buffer root
                :ownership-epoch (niwa-session-ownership-epoch session)
                :transcript-relative-path relative :transcript-status 'running
                :frozen-configuration
                (test-niwa-session-persistence--agent-configuration)))
              (child (niwa-agent-conversation-open invocation root))
              (record
               (niwa-agent-record--create
                :id "busy" :path "/root/busy" :parent-path "/root"
                :role "default" :activity 'running :invocation invocation
                :configuration (niwa-agent-invocation-frozen-configuration invocation)
                :conversation-buffer child :conversation-location relative))
              (original-publish
               (symbol-function 'niwa-session-artifacts-publish-agent-terminal-state))
              (publications 0) cancelled before head)
         (setf (niwa-agent-invocation-buffer invocation) child
               (niwa-agent-invocation-runtime-fsm invocation)
               (gptel-make-fsm :state 'WAIT :info (list :buffer child))
               (niwa-agent-invocation-runtime-cancel invocation)
               (lambda () (setq cancelled t) nil)
               (niwa-agent-invocation-runtime-settle-callback invocation)
               (apply-partially #'niwa-agent-control--settle session record)
               (niwa-session-agent-registry session)
               (list (cons "/root/busy" record))
               (niwa-session-agent-transcripts session)
               `(("busy" :agent-path "/root/busy" :status running :path ,relative)))
         (with-current-buffer child
           (setq buffer-file-name
                 (expand-file-name relative (niwa-session-save-path session)))
           (insert "* Agent Task: busy\n")
           (should (niwa-agent-conversation-save invocation))
           (goto-char (point-max))
           (insert (propertize "Unpublished partial response.\n" 'gptel 'response))
           (setq before (buffer-substring (point-min) (point-max))))
         (when takeover
           (let ((successor (niwa-session-create
                             "successor" (niwa-session-workspace session)))
                 (niwa-session-durability--client-id (make-string 64 ?b)))
             (setf (niwa-session-save-path successor) (niwa-session-save-path session)
                   (niwa-session-session-id successor) (niwa-session-session-id session))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
               (should (niwa-session-durability-lease-acquire
                        (niwa-session-save-path successor) "successor"
                        successor nil t)))
             (niwa-session-durability--cancel-renewal successor))
           (setq head (niwa-session-durability--lease-head
                       (niwa-session-durability--lease-path
                        (niwa-session-save-path session)))))
         (cl-letf (((symbol-function 'niwa-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args)
                      (cl-incf publications)
                      (apply original-publish args))))
           (if takeover
               (progn
                 (should-not (niwa-session-durability-lease-renew session))
                 (niwa-session-control-transfer-taken-over session head))
             (niwa-agent-runtime-interrupt invocation "interrupted by user")))
         (should cancelled)
         (should (niwa-agent-invocation-runtime-settled-p invocation))
         (should-not (niwa-agent-invocation-runtime-fsm invocation))
         (should-not (niwa-agent-invocation-runtime-cancel invocation))
         (should-not (niwa-agent-control--active-p record))
         (should-not (niwa-agent-record-invocation record))
         (should (buffer-live-p child))
         (if takeover
             (progn
               (should (= 0 publications))
               (should-not (niwa-agent-conversation-save invocation t))
               (should-not (niwa-agent-invocation-transcript-save-timer invocation))
               (should-not (niwa-agent-conversation--write invocation))
               (should (eq 'following (plist-get (niwa-session-control-transfer session) :state)))
               (should-not (niwa-session-lease-renewal-timer session))
               (should (equal head (niwa-session-durability--lease-head
                                    (niwa-session-durability--lease-path
                                     (niwa-session-save-path session)))))
               (with-current-buffer child
                 (should (equal-including-properties before (buffer-string)))
                 (should (buffer-modified-p)))
               (should-not (test-niwa-publication-async--mail session)))
           (should (= 1 publications))
           (should (eq 'RESULT (plist-get (car (test-niwa-publication-async--restored-mail session)) :type)))))))))

(niwa-deftest niwa-agent-runtime--settle/takeback (:quiet t)
  ,test
  (test)
  :doc "a deferred old terminal stays fenced after takeback; new turns publish"
  (test-niwa-publication-async--fixture
   (lambda (session root)
     (niwa-tools-register)
     (test-niwa-publication-async--wait
      (lambda () (not (niwa-session-publication-busy-p session))))
     (let* ((configuration (test-niwa-session-persistence--agent-configuration))
            (run (symbol-function 'niwa-session-artifacts-publish-agent-terminal-state))
            (record (niwa-agent-record--create
                     :id "busy" :path "/root/busy" :parent-path "/root"
                     :role "default" :activity 'idle :configuration configuration))
            callbacks cancelled invocation child before old-generation old-epoch head
            (new-request (niwa-request--create :session session :origin "/root/busy"))
            (publications 0))
       (setf (niwa-session-agent-reservations session) (list (cons "/root/busy" record)))
       (cl-letf (((symbol-function 'niwa-agent-exec-run)
                  (lambda (callback _role _description owner buffer)
                    (push (cons owner callback) callbacks)
                    (setf (niwa-agent-invocation-runtime-cancel owner)
                          (lambda () (setq cancelled t) 'deferred)
                          (niwa-agent-invocation-runtime-fsm owner)
                          (gptel-make-fsm :state 'WAIT :info (list :buffer buffer)))))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (with-current-buffer root
           (setq invocation
                 (niwa-agent-runtime-dispatch
                  nil "busy" "Keep working."
                  :path "/root/busy" :prepared-turn '(:prompt "Keep working.")
                  :frozen-configuration configuration
                  :on-invocation (apply-partially #'niwa-agent-control--record-invocation session record)
                  :on-settle (apply-partially #'niwa-agent-control--settle session record))))
         (setq child (niwa-agent-invocation-buffer invocation)
               old-generation (plist-get (niwa-session-lease session) :generation)
               old-epoch (niwa-session-ownership-epoch session))
         (should (equal old-epoch (niwa-agent-invocation-ownership-epoch invocation)))
         (with-current-buffer child
           (goto-char (point-max))
           (insert (propertize "Old unpublished answer.\n" 'gptel 'response))
           (setq before (buffer-string)))
         (let ((successor (niwa-session-create "successor" (niwa-session-workspace session)))
               (niwa-session-durability--client-id (make-string 64 ?b)))
           (setf (niwa-session-save-path successor) (niwa-session-save-path session)
                 (niwa-session-session-id successor) (niwa-session-session-id session))
           (should (niwa-session-durability-lease-acquire
                    (niwa-session-save-path successor) "successor" successor nil t))
           (niwa-session-durability--cancel-renewal successor))
         (should-not (niwa-session-durability-lease-renew session))
         (should cancelled)
         (should-not (niwa-agent-invocation-runtime-settled-p invocation))
         (should (eq 'following (plist-get (niwa-session-control-transfer session) :state)))
         ;; Takeback must not wait for the deferred provider acknowledgement.
         (should (niwa-session-control-transfer-acquire session root t))
         (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
         (should-not (equal old-generation (plist-get (niwa-session-lease session) :generation)))
         (should (> (niwa-session-ownership-epoch session) old-epoch))
         (let ((snapshot (copy-niwa-session session))
               (epoch (niwa-session-ownership-epoch session)))
           (should (> epoch 0))
           (setf (niwa-session-ownership-epoch snapshot) 0)
           (niwa-session-rewind--copy-rewind-session-state snapshot session)
           (should (= epoch (niwa-session-ownership-epoch session))))
         (setq head (niwa-session-durability--lease-head
                     (niwa-session-durability--lease-path (niwa-session-save-path session))))
         ;; Isolate request-end's identity guard, not a full new provider turn.
         (with-current-buffer child
           (setq-local niwa--agent-invocation
                       (niwa-agent-invocation--create :buffer child :parent-session session)
                       niwa--current-request new-request))
         (cl-letf (((symbol-function 'niwa-session-artifacts-publish-agent-terminal-state)
                    (lambda (&rest args) (cl-incf publications) (apply run args))))
           ;; Deliver through the real callback captured at provider dispatch.
           (funcall (cdr (assq invocation callbacks)) "Late old answer.")
           (should (niwa-agent-invocation-runtime-settled-p invocation))
           (should (= 0 publications))
           (should (equal head (niwa-session-durability--lease-head
                                (niwa-session-durability--lease-path (niwa-session-save-path session)))))
           (with-current-buffer child
             (should (eq new-request niwa--current-request))
             (setq niwa--current-request nil)
             (should (equal-including-properties before (buffer-string)))
             (should (buffer-modified-p)))
           (should-not (test-niwa-publication-async--mail session))
           ;; A fresh turn in the newly owned epoch must still publish normally.
           (let ((fresh-record (niwa-agent-record--create
                                :id "fresh" :path "/root/fresh" :parent-path "/root"
                                :role "default" :activity 'idle :configuration configuration))
                 fresh)
             (push (cons "/root/fresh" fresh-record) (niwa-session-agent-reservations session))
             (with-current-buffer root
               (setq fresh
                     (niwa-agent-runtime-dispatch
                      nil "fresh" "New work."
                      :path "/root/fresh" :prepared-turn '(:prompt "New work.")
                      :frozen-configuration configuration
                      :on-invocation (apply-partially #'niwa-agent-control--record-invocation session fresh-record)
                      :on-settle (apply-partially #'niwa-agent-control--settle session fresh-record))))
             (should-not (niwa-agent-conversation-fenced-p fresh))
             (should (equal (niwa-session-ownership-epoch session)
                            (niwa-agent-invocation-ownership-epoch fresh)))
             ;; A real renewal error is not evidence of another owner.
             (let ((warnings 0))
               (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                          (lambda (&rest _) (error "Injected target clock failure")))
                         ((symbol-function 'niwa-diagnostic-warning)
                          (lambda (&rest _) (cl-incf warnings))))
                 (should-not (niwa-session-durability-lease-renew session)))
               (should (= 1 warnings)))
             (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
             (should-not (niwa-agent-conversation-fenced-p fresh))
             (should (niwa-session-durability-lease-renew session))
             (funcall (cdr (assq fresh callbacks)) "New answer.")
             (should (= 1 publications))
             (should (niwa-agent-invocation-runtime-settled-p fresh))
             (should (eq 'RESULT (plist-get (car (test-niwa-publication-async--restored-mail session)) :type)))
             (let ((next
                    (with-current-buffer root
                      (niwa-agent-runtime-dispatch
                       nil "followup" "Continue new work."
                       :path "/root/fresh" :frozen-configuration configuration
                       :retained-id (niwa-agent-invocation-agent-id fresh)
                       :retained-buffer (niwa-agent-invocation-buffer fresh)
                       :retained-transcript (niwa-agent-invocation-transcript-relative-path fresh)
                       :on-invocation (apply-partially #'niwa-agent-control--record-invocation session fresh-record)
                       :on-settle (apply-partially #'niwa-agent-control--settle session fresh-record)))))
               (should-not (eq fresh next))
               (should (eq (niwa-agent-invocation-buffer fresh)
                           (niwa-agent-invocation-buffer next)))
               (should (equal (niwa-session-ownership-epoch session)
                              (niwa-agent-invocation-ownership-epoch next)))
               (funcall (cdr (assq next callbacks)) "Followup answer.")
               (should (= 2 publications))
               (should (niwa-session-control-fs-wait-async 5))))))))))

(niwa-deftest niwa-agent-runtime--settle/reentrant-epoch (:quiet t)
  ,test
  (test)
  :doc "authority checks and staged callbacks cannot publish a superseded epoch"
  (dolist (boundary '(control publisher staged))
    (test-niwa-publication-async--fixture
     (lambda (session root)
       (niwa-tools-register)
       (test-niwa-publication-async--wait (lambda () (not (niwa-session-publication-busy-p session))))
       (let* ((configuration (test-niwa-session-persistence--agent-configuration))
              (relative "agents/reentrant.chat.org")
              (invocation (niwa-agent-invocation--create
                           :agent-id "busy" :path "/root/busy" :parent-session session :parent-data-buffer root
                           :ownership-epoch (niwa-session-ownership-epoch session)
                           :transcript-relative-path relative :transcript-status 'running :frozen-configuration configuration))
              (child (niwa-agent-conversation-open invocation root))
              (record (niwa-agent-record--create
                       :id "busy" :path "/root/busy" :parent-path "/root" :role "default" :activity 'running
                       :invocation invocation :configuration configuration :conversation-buffer child :conversation-location relative))
              (authority (symbol-function 'niwa-session-artifacts-assert-mutation-authority))
              (publish (symbol-function 'niwa-session-publication-publish))
              (armed t) (publications 0) head before)
         (setf (niwa-agent-invocation-buffer invocation) child
               (niwa-agent-invocation-runtime-cancel invocation) (lambda () 'deferred)
               (niwa-session-agent-registry session) (list (cons "/root/busy" record))
               (niwa-session-agent-transcripts session) `(("busy" :agent-path "/root/busy" :status running :path ,relative)))
         (with-current-buffer child
           (setq buffer-file-name (expand-file-name relative (niwa-session-save-path session)))
           (insert "* Agent Task: busy\n")
           (should (niwa-agent-conversation-save invocation))
           (goto-char (point-max))
           (insert (propertize "Unpublished reentrant answer.\n" 'gptel 'response))
           (setq before (buffer-string)))
         (cl-labels
             ((takeback ()
                (setq armed nil)
                (let ((successor (niwa-session-create "successor" (niwa-session-workspace session)))
                      (niwa-session-durability--client-id (make-string 64 ?b)))
                  (setf (niwa-session-save-path successor) (niwa-session-save-path session)
                        (niwa-session-session-id successor) (niwa-session-session-id session))
                  (should (niwa-session-durability-lease-acquire (niwa-session-save-path session) "successor" successor nil t))
                  (niwa-session-durability--cancel-renewal successor))
                (should-not (niwa-session-durability-lease-renew session))
                (should (niwa-session-control-transfer-acquire session root t))
                (setq head (niwa-session-durability--lease-head
                            (niwa-session-durability--lease-path (niwa-session-save-path session))))))
           (setf (niwa-agent-invocation-runtime-settle-callback invocation)
                 (lambda (&rest args)
                   (let ((transaction (apply #'niwa-agent-control--settle session record args)))
                     (when (and armed (eq boundary 'staged)) (takeback))
                     transaction)))
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                     ((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                      (lambda (owner &rest args)
                        (prog1 (apply authority owner args)
                          (when (and armed (eq owner session) (not (eq boundary 'staged))
                                     (eq (eq boundary 'publisher) (and (memq session niwa-session-artifacts--terminal-sessions) t)))
                            (takeback)))))
                     ((symbol-function 'niwa-session-publication-publish)
                      (lambda (&rest args) (cl-incf publications) (apply publish args))))
             (niwa-agent-runtime--settle invocation "Finished old answer."))
           (should-not armed)
           (should (= 0 publications))
           (should (niwa-agent-invocation-runtime-settled-p invocation))
           (should-not (niwa-agent-control--active-p record))
           (should-not (test-niwa-publication-async--mail session))
           (should (equal head (niwa-session-durability--lease-head
                                (niwa-session-durability--lease-path (niwa-session-save-path session)))))
           (with-current-buffer child
             (should (equal-including-properties before (buffer-string)))
             (should (buffer-modified-p)))
           (should (niwa-session-control-fs-wait-async 5))))))))

(niwa-deftest niwa-agent-runtime--settle/own-continuity (:quiet t)
  ,test
  (test)
  :doc "same-owner latch rotations and expired reclamation preserve settlement"
  (dolist (transition '(unchanged latch reclaim))
    (test-niwa-publication-async--fixture
     (lambda (session root)
       (niwa-tools-register)
       (test-niwa-publication-async--wait
        (lambda () (not (niwa-session-publication-busy-p session))))
       (let* ((configuration (test-niwa-session-persistence--agent-configuration))
              (record (niwa-agent-record--create
                       :id "busy" :path "/root/busy" :parent-path "/root"
                       :role "default" :activity 'idle :configuration configuration))
              (publish (symbol-function 'niwa-session-artifacts-publish-agent-terminal-state))
              (directory (niwa-session-durability--lease-path (niwa-session-save-path session)))
              (epoch (niwa-session-ownership-epoch session))
              (generation (plist-get (niwa-session-lease session) :generation))
              callback invocation (publications 0))
         (should (= generation 1))
         (setf (niwa-session-agent-reservations session) (list (cons "/root/busy" record)))
         (cl-letf (((symbol-function 'niwa-agent-exec-run)
                    (lambda (settle _role _description owner buffer)
                      (setq callback settle)
                      (setf (niwa-agent-invocation-runtime-fsm owner)
                            (gptel-make-fsm :state 'WAIT :info (list :buffer buffer)))))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) (ert-fail "Own continuity asked for takeover")))
                   ((symbol-function 'y-or-n-p) (lambda (&rest _) (ert-fail "Own continuity asked for takeover"))))
           (with-current-buffer root
             (setq invocation
                   (niwa-agent-runtime-dispatch
                    nil "busy" "Retain own work."
                    :path "/root/busy" :prepared-turn '(:prompt "Retain own work.")
                    :frozen-configuration configuration
                    :on-invocation (apply-partially #'niwa-agent-control--record-invocation session record)
                    :on-settle (apply-partially #'niwa-agent-control--settle session record))))
           (with-current-buffer (niwa-agent-invocation-buffer invocation)
             (goto-char (point-max))
             (insert (propertize "Same-owner answer.\n" 'gptel 'response)))
           (pcase transition
             ('latch
              (should (niwa-session-durability-set-unsettled-mutation session t))
              (should (= 2 (plist-get (niwa-session-durability--lease-head directory) :generation)))
              (should (niwa-session-durability-unsettled-mutation-p session))
              (should (niwa-session-durability-set-unsettled-mutation session nil))
              (should (= 3 (plist-get (niwa-session-durability--lease-head directory) :generation)))
              (should-not (niwa-session-durability-unsettled-mutation-p session)))
             ('reclaim
              (let ((expired (copy-sequence (niwa-session-durability--lease-head directory)))
                    (now (float-time)))
                (plist-put expired :renewed-at (- now 601))
                (plist-put expired :expires-at (- now 1))
                ;; Only disk time changes; renewal must observe and bind the real record.
                (niwa-session-durability--write-generation directory expired))
              (should (niwa-session-durability-lease-renew session))
              (should (= 2 (plist-get (niwa-session-durability--lease-head directory) :generation)))))
           (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
           (should (equal niwa-session-durability--client-id
                          (plist-get (niwa-session-durability--lease-head directory) :client-id)))
           (should (= epoch (niwa-session-ownership-epoch session)))
           (should (= epoch (niwa-agent-invocation-ownership-epoch invocation)))
           (should-not (niwa-agent-conversation-fenced-p invocation))
           (cl-letf (((symbol-function 'niwa-session-artifacts-publish-agent-terminal-state)
                      (lambda (&rest args) (cl-incf publications) (apply publish args))))
             (funcall callback "Same-owner answer.")
             (funcall callback "Duplicate answer.")
             (should (= 1 publications)))
           (should (niwa-agent-invocation-runtime-settled-p invocation))
           (let ((mail (test-niwa-publication-async--restored-mail session)))
             (should (= 1 (length mail)))
             (should (eq 'RESULT (plist-get (car mail) :type))))
           (should (string-match-p
                    "Same-owner answer"
                    (niwa-session-artifacts-read-artifact
                     session (niwa-agent-invocation-transcript-relative-path invocation) t)))
           (should (niwa-session-control-fs-wait-async 5))))))))

(provide 'test-niwa-session-takeover-agent-settlement)
;;; test-niwa-session-takeover-agent-settlement.el ends here
