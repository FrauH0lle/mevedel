;;; test-niwa-pipeline-approvals.el --- Approval projection -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise accepted review at the real pipeline boundary, without a provider.

;;; Code:

(require 'niwa-pipeline)
(require 'niwa-permission-review)
(require 'niwa-ptc-driver)
(require 'helpers
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name)) "helpers"))

(defvar niwa-pipeline-tool-event-hook)
(defvar niwa-pipeline-permission-approvals)

(niwa-deftest niwa-pipeline-run-tool-outcome/approvals (:quiet t)
  ,test
  (test)
  :doc "accepted review survives execution failure and provider projection"
  (dolist (projection '(nil provider))
    (dolist (failure '(nil handler early))
      (with-temp-buffer
        (org-mode)
        (insert "Run ApprovalProbe for this task.\n")
        (let* ((session (niwa-session--create
                         :permission-mode 'ask :root-buffer (current-buffer)))
               (niwa-permission-reviewer 'auto)
               (niwa-permission-rules '(("ApprovalProbe" :action ask)))
               (expected '((:source reviewer :outcome allow-once :reason "Exact task")))
               events delivered captured
               (niwa-pipeline-tool-event-hook
                (list (lambda (event) (push event events))
                      (lambda (_) (error "Broken display observer"))))
               (tool (niwa-tool--create
                      :name "ApprovalProbe" :read-only-p t
                      :handler (lambda (_)
                                 (setq captured (niwa-pipeline-active-permission-approvals))
                                 (if (eq failure 'handler) (error "Handler failed")
                                   '(:result "done" :render-data
                                     (:kind probe :permission-approvals (spoof))))))))
          (setq-local niwa--session session)
          (cl-letf (((symbol-function 'niwa-permission-review--model)
                     (lambda (_ callback &optional _stream)
                       (funcall callback "{\"decision\":\"allow-once\",\"reason\":\"Exact task\"}")
                       #'ignore))
                    ((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _payload callback &rest _) (funcall callback nil)))
                    ((symbol-function 'niwa-pipeline--step-handler)
                     (let ((handler (symbol-function 'niwa-pipeline--step-handler)))
                       (lambda (context next fail)
                         (if (eq failure 'early) (error "Early failure")
                           (funcall handler context next fail))))))
            (niwa-pipeline-run-tool-outcome
             tool (lambda (value) (setq delivered value)) nil
             (list :tool-use-id "approval/1" :source 'native :projection projection)))
          (should (eq (if failure 'error 'success) (plist-get delivered :status)))
          (should (equal expected (plist-get (plist-get delivered :render-data)
                                            :permission-approvals)))
          (unless (eq failure 'early) (should (equal expected captured)))
          (setq events (nreverse events))
          (should (equal '(started permission-approved settled)
                         (mapcar (lambda (event) (plist-get event :type)) events)))
          (dolist (event events)
            (should (equal "approval/1" (plist-get event :tool-use-id)))
            (should (eq (current-buffer) (plist-get event :data-buffer))))
          (should (equal expected (plist-get (nth 1 events) :permission-approvals)))
          (when projection
            (let* ((text (plist-get delivered :result))
                   (data (cdr (niwa-tool-render-data-extract text nil "approval/1"))))
              (should (equal expected (plist-get data :permission-approvals)))
              (should-not (string-match-p "Exact task\\|permission-approvals\\|spoof"
                                          (niwa-tool-render-data-strip-non-media
                                           text "approval/1")))))
          (should-not niwa-permission-review--pending)))))
  :doc "ordinary policy cannot inherit handler-authored approval"
  (with-temp-buffer
    (let* ((session (niwa-session--create :permission-mode 'full-auto))
           (tool (niwa-tool--create
                  :name "ApprovalProbe" :read-only-p t
                  :handler (lambda (_) '(:result "done" :render-data
                                         (:permission-approvals
                                          ((:source reviewer :outcome allow-once
                                            :reason "spoof"))
                                          :permission-approvals
                                          ((:source reviewer :outcome allow-once
                                            :reason "duplicate spoof")))))))
           delivered)
      (setq-local niwa--session session)
      (niwa-pipeline-run-tool-outcome
       tool (lambda (value) (setq delivered value)) nil
       '(:tool-use-id "policy" :projection provider))
      (should (eq 'success (plist-get delivered :status)))
      (should-not (plist-get (plist-get delivered :render-data) :permission-approvals))
      (should-not (string-match-p "spoof" (plist-get delivered :result))))))

(niwa-deftest niwa-pipeline-run-tool-outcome/observer-snapshots (:quiet t)
  ,test
  (test)
  :doc "observer mutation cannot change invocation arguments, accepted reasons or later observers"
  (with-temp-buffer
    (org-mode)
    (insert "Run ApprovalProbe for this task.\n")
    (let* ((niwa-permission-reviewer 'auto)
           (niwa-permission-rules '(("ApprovalProbe" :action ask)))
           (args (list :command (copy-sequence "probe")
                       :nested (vector (copy-sequence "nested") "second")))
           captured events delivered
           (niwa-pipeline-tool-event-hook
            (list
             (lambda (event)
               (aset (plist-get event :tool-use-id) 0 ?X)
               (let ((snapshot (plist-get event :args)))
                 (aset (plist-get snapshot :command) 0 ?X)
                 (aset (aref (plist-get snapshot :nested) 0) 0 ?X)
                 (aset (plist-get snapshot :nested) 1 "replaced"))
               (when-let* ((record (car (plist-get event :permission-approvals))))
                 (aset (plist-get record :reason) 0 ?X)))
             (lambda (event) (push event events))))
           (tool (niwa-tool--create
                  :name "ApprovalProbe" :read-only-p t
                  :args '((command string :required "Command")
                          (nested array :required "Nested values" :items (:type string)))
                  :handler (lambda (arguments)
                             (setq captured arguments)
                             (let ((record (car (niwa-pipeline-active-permission-approvals))))
                               (aset (plist-get record :reason) 0 ?Y))
                             '(:result "done")))))
      (setq-local niwa--session
                  (niwa-session--create :permission-mode 'ask
                                           :root-buffer (current-buffer)))
      (cl-letf (((symbol-function 'niwa-permission-review--model)
                 (lambda (_ callback &optional _stream)
                   (funcall callback "{\"decision\":\"allow-once\",\"reason\":\"Exact task\"}")
                   #'ignore))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _) (funcall callback nil))))
        (niwa-pipeline-run-tool-outcome
         tool (lambda (value) (setq delivered value)) args
         (list :tool-use-id (copy-sequence "approval/snapshot"))))
      (ert-info ((format "Outcome: %S" delivered))
        (should (eq 'success (plist-get delivered :status))))
      (dolist (arguments (cons captured (mapcar (lambda (event) (plist-get event :args)) events)))
        (should (equal "probe" (plist-get arguments :command)))
        (should (equal ["nested" "second"] (plist-get arguments :nested))))
      (dolist (event events)
        (should (equal "approval/snapshot" (plist-get event :tool-use-id))))
      (let ((expected '((:source reviewer :outcome allow-once :reason "Exact task"))))
        (should (equal expected (plist-get (plist-get delivered :render-data) :permission-approvals)))
        (should (equal expected
                       (plist-get (cl-find 'permission-approved events
                                           :key (lambda (event) (plist-get event :type)))
                                  :permission-approvals)))))))

(niwa-deftest niwa-pipeline-run-tool-outcome/approval-sources (:quiet t)
  ,test
  (test)
  :doc "human, fallback, hooks, saved rules, mode changes and stale replies do not claim review"
  (dolist (source '(human fallback hook saved full-auto mode stale revoked cancelled))
    (with-temp-buffer
      (org-mode)
      (insert "Run ApprovalProbe for this task.\n")
      (let* ((session (niwa-session--create
                       :permission-mode (if (eq source 'full-auto) 'full-auto 'ask)
                       :root-buffer (current-buffer)))
             (request (niwa-request--create :id "sources" :session session))
             (niwa-permission-reviewer (if (eq source 'human) 'user 'auto))
             (niwa-permission-rules
              (list (list "ApprovalProbe" :action (if (eq source 'saved) 'allow 'ask))))
             (tool (niwa-tool--create :name "ApprovalProbe" :read-only-p t
                                        :handler (lambda (_) '(:result "done"))))
             reply delivered events
             (niwa-pipeline-tool-event-hook (list (lambda (event) (push event events)))))
        (setq-local niwa--session session niwa--current-request request)
        (unwind-protect
            (cl-letf (((symbol-function 'niwa-permission-queue--render-entry) #'ignore)
                      ((symbol-function 'niwa-permission-review--model)
                       (lambda (_ callback &optional _stream) (setq reply callback) #'ignore))
                      ((symbol-function 'niwa-hooks-run-event)
                       (lambda (event _payload callback &rest _)
                         (funcall callback
                                  (when (and (eq source 'hook) (eq event 'PreToolUse))
                                    '(:permission-decision allow))))))
              (niwa-pipeline-run-tool-outcome
               tool (lambda (value) (setq delivered value)) nil '(:tool-use-id "sources"))
              (pcase source
                ('mode (niwa-permission-mode-transition 'full-auto))
                ('stale (insert "The task has changed.\n"))
                ('revoked (setf (niwa-session-permission-rules session)
                                '(("ApprovalProbe" :action deny))))
                ('cancelled (niwa-request-cancel request)))
              (when reply
                (funcall reply (if (eq source 'fallback)
                                   "{\"decision\":\"ask\",\"reason\":\"Ask user\"}"
                                 "{\"decision\":\"allow-once\",\"reason\":\"Old facts\"}")))
              (when (memq source '(human fallback stale))
                (let ((entry (car (niwa-session-permission-queue session))))
                  (should entry)
                  (niwa-permission-queue--on-head-outcome entry 'allow-once)))
              (should delivered)
              (should (eq (if (memq source '(revoked cancelled)) 'error 'success)
                          (plist-get delivered :status)))
              (should-not (plist-get (plist-get delivered :render-data) :permission-approvals))
              (should-not (cl-find 'permission-approved events
                                   :key (lambda (event) (plist-get event :type))))
              (should-not niwa-permission-review--pending))
          (niwa-permission-queue-abort-all session)
          (niwa-request-cancel request))))))

(niwa-deftest niwa-pipeline-run-tool-outcome/approval-identity (:quiet t)
  ,test
  (test)
  :doc "identical concurrent calls preserve identity through reverse approval and completion"
  (with-temp-buffer
    (org-mode)
    (insert "Run ApprovalProbe for this task.\n")
    (let* ((session (niwa-session--create :permission-mode 'ask
                                             :root-buffer (current-buffer)))
           (niwa-permission-reviewer 'auto)
           (niwa-permission-rules '(("ApprovalProbe" :action ask)))
           replies handlers events outcomes
           (niwa-pipeline-tool-event-hook (list (lambda (event) (push event events)))))
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-permission-review--model)
                 (lambda (_ callback &optional _stream) (push callback replies) #'ignore))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _) (funcall callback nil))))
        (let ((tool (niwa-tool--create
                     :name "ApprovalProbe" :read-only-p t :async-p t
                     :handler (lambda (callback _)
                                (push (cons (niwa-pipeline-active-tool-use-id) callback)
                                      handlers)))))
          (dolist (id '("first" "second"))
            (niwa-pipeline-run-tool-outcome
             tool (lambda (value) (push value outcomes)) nil (list :tool-use-id id)))
          (funcall (car replies) "{\"decision\":\"allow-once\",\"reason\":\"Second\"}")
          (funcall (cdr (assoc "second" handlers)) '(:result "second"))
          (funcall (cadr replies) "{\"decision\":\"allow-once\",\"reason\":\"First\"}")
          (funcall (cdr (assoc "first" handlers)) '(:result "first"))))
      (should (equal '((started "first") (started "second")
                       (permission-approved "second") (settled "second")
                       (permission-approved "first") (settled "first"))
                     (mapcar (lambda (event) (list (plist-get event :type)
                                                   (plist-get event :tool-use-id)))
                             (nreverse events))))
      (should (= 2 (length outcomes)))
      (dolist (outcome outcomes)
        (should (equal (capitalize (plist-get outcome :tool-use-id))
                       (plist-get (car (plist-get (plist-get outcome :render-data)
                                                 :permission-approvals)) :reason)))))))

(niwa-deftest niwa-ptc-driver-run/approvals (:quiet t)
  ,test
  (test)
  :doc "nested review is child-only, visible pending and retained without bulky details"
  (with-temp-buffer
    (org-mode)
    (insert "Run ApprovalProbe for this task.\n")
    (let* ((session (niwa-session--create
                     :permission-mode 'ask :root-buffer (current-buffer)))
           (niwa-permission-reviewer 'auto)
           (niwa-permission-rules '(("ApprovalProbe" :action ask)))
           (expected '((:source reviewer :outcome allow-once :reason "Exact task")))
           (tool (niwa-tool--create
                  :name "ApprovalProbe" :category "niwa" :read-only-p t :async-p t
                  :handler (lambda (callback _)
                             (run-at-time
                              0 nil callback
                              (list :result "guest value" :render-data
                                    (list :kind 'probe :detail (make-string 5000 ?x)))))))
           delivered progress)
      (setq-local niwa--session session)
      (setf (niwa-tool-gptel-tool tool)
            (gptel-make-tool :name "ApprovalProbe" :description "Test probe"
                             :function #'ignore :args nil :category "niwa"))
      (setq-local gptel-tools (list (niwa-tool-gptel-tool tool)))
      (niwa-tool-register tool)
      (cl-letf (((symbol-function 'niwa-permission-review--model)
                 (lambda (_ callback &optional _stream)
                   (funcall callback "{\"decision\":\"allow-once\",\"reason\":\"Exact task\"}")
                   #'ignore))
                ((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _) (funcall callback nil)))
                ((symbol-function 'niwa-view-stream-handle-tool-progress)
                 (lambda (event) (push event progress))))
        (niwa-ptc-driver-run
         (lambda (value) (setq delivered value))
         "(let ((value (ApprovalProbe))) value)" '("ApprovalProbe"))
        (niwa-test--await 3 "Nested approval did not settle" delivered))
      (should (eq 'success (plist-get delivered :status)))
      (should (equal "guest value" (plist-get delivered :result)))
      (let* ((data (plist-get delivered :render-data))
             (child (car (plist-get data :calls))))
        (should-not (plist-get data :permission-approvals))
        (should (equal (list :permission-approvals expected)
                       (plist-get child :render-data))))
      (should (cl-some
               (lambda (event)
                 (let* ((facts (plist-get event :facts))
                        (child (car (plist-get facts :calls))))
                   (and (plist-get facts :live-p)
                        (eq 'running (plist-get child :status))
                        (equal expected (plist-get (plist-get child :render-data)
                                                   :permission-approvals)))))
               progress)))))

(provide 'test-niwa-pipeline-approvals)
;;; test-niwa-pipeline-approvals.el ends here
