;;; test-mevedel-agent-control.el --- Durable agent control tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the session-owned agent registry's validation, admission, rollback,
;; settlement, result bounding, and provider-dispatch boundary.

;;; Code:

(require 'mevedel-agent-control)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-agents)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-agent-control-test--session ()
  "Return a fresh in-memory session for agent-control tests."
  (mevedel-session-create
   "main"
   (mevedel-workspace--create
    :type 'project
    :id "agent-control"
    :root temporary-file-directory
    :name "agent-control")))

(mevedel-deftest mevedel-agent-control--active-p ()
  ,test
  (test)
  :doc "starting and running records own capacity while idle records do not"
  (dolist (case '((starting . t) (running . t) (idle)))
    (should (eq (cdr case)
                (and (mevedel-agent-control--active-p
                      (mevedel-agent-record--create :activity (car case)))
                     t)))))

(mevedel-deftest mevedel-agent-control--active-count ()
  ,test
  (test)
  :doc "counts active records across the session registry"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/a"
                 (mevedel-agent-record--create :activity 'starting))
           (cons "/root/b"
                 (mevedel-agent-record--create :activity 'running))
           (cons "/root/c"
                 (mevedel-agent-record--create :activity 'idle))))
    (should (= 2 (mevedel-agent-control--active-count session)))))

(mevedel-deftest mevedel-agent-control--validate-spawn ()
  ,test
  (test)
  :doc "accepts the minimal spawn contract and rejects malformed values"
  (let ((session (mevedel-agent-control-test--session)))
    (should-not
     (mevedel-agent-control--validate-spawn session "task_2" "Do work."))
    (dolist (task-name '(nil "" "Upper" "two/parts" "root"))
      (should-error
       (mevedel-agent-control--validate-spawn session task-name "Do work.")))
    (dolist (message '(nil ""))
      (should-error
       (mevedel-agent-control--validate-spawn session "task" message)))))

(mevedel-deftest mevedel-agent-control--reserve ()
  ,test
  (test)
  :doc "reserves paths and capacity while retaining idle identities"
  (let* ((session (mevedel-agent-control-test--session))
         (record nil))
    (setf (mevedel-session-agent-turn-capacity session) 1)
    (setq record (mevedel-agent-control--reserve session "first"))
    (should (equal "/root/first" (mevedel-agent-record-path record)))
    (should-error (mevedel-agent-control--reserve session "first"))
    (should-error (mevedel-agent-control--reserve session "second"))
    (setf (mevedel-agent-record-activity record) 'idle)
    (should (mevedel-agent-control--reserve session "second"))))

(mevedel-deftest mevedel-agent-control--rollback ()
  ,test
  (test)
  :doc "removes the reserved identity and legacy transcript index entry"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (buffer (generate-new-buffer " *agent-control-rollback*"))
         (record (mevedel-agent-record--create
                  :path "/root/retry"
                  :activity 'starting
                  :invocation invocation)))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--rollback")
    (setf (mevedel-agent-invocation-buffer invocation) buffer)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/retry.chat.org")
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/retry" record)))
    (setf (mevedel-session-agent-transcripts session)
          '(("default--rollback" :path "agents/retry.chat.org")))
    (mevedel-agent-control--rollback session record)
    (should-not (mevedel-session-agent-registry session))
    (should-not (mevedel-session-agent-transcripts session))
    (should-not (buffer-live-p buffer))))

(mevedel-deftest mevedel-agent-control--bounded-result ()
  ,test
  (test)
  :doc "keeps small results intact and bounds large head/tail previews"
  (let ((record
         (mevedel-agent-record--create
          :conversation-location "agents/large.chat.org")))
    (should (equal "small"
                   (mevedel-agent-control--bounded-result record "small")))
    (let ((payload
           (mevedel-agent-control--bounded-result
            record (concat "HEAD" (make-string 40000 ?x) "TAIL"))))
      (should (<= (length payload) (* 32 1024)))
      (should (string-prefix-p "HEAD" payload))
      (should (string-match-p "TAIL" payload))
      (should (string-match-p "agents/large.chat.org" payload)))))

(mevedel-deftest mevedel-agent-control--settle ()
  ,test
  (test)
  :doc "settles once, releases capacity, and emits concise canonical RESULT"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (record (mevedel-agent-record--create
                  :path "/root/work"
                  :parent-path "/root"
                  :activity 'running
                  :invocation invocation)))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'error)
    (mevedel-agent-control--settle
     session record invocation "legacy details"
     '(:error-details "Provider failed"))
    (mevedel-agent-control--settle session record invocation "duplicate")
    (let ((result (car (mevedel-session-messages session))))
      (should (= 1 (length (mevedel-session-messages session))))
      (should (eq 'idle (mevedel-agent-record-activity record)))
      (should (eq 'RESULT (plist-get result :type)))
      (should (eq 'errored (plist-get result :outcome)))
      (should (equal "Provider failed" (plist-get result :payload))))))

(mevedel-deftest mevedel-agent-control--record-invocation ()
  ,test
  (test)
  :doc "records the stable opaque identity and persisted conversation location"
  (let ((record (mevedel-agent-record--create))
        (invocation (mevedel-agent-invocation-create
                     (mevedel-agent-default))))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--opaque")
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/default.chat.org")
    (mevedel-agent-control--record-invocation record invocation)
    (should (equal "default--opaque" (mevedel-agent-record-id record)))
    (should (eq invocation (mevedel-agent-record-invocation record)))
    (should (equal "agents/default.chat.org"
                   (mevedel-agent-record-conversation-location record)))))

(mevedel-deftest mevedel-agent-control-spawn
  (:after-each (mevedel-workspace-clear-registry))
  ,test
  (test)
  :doc "commits a real retained conversation while stubbing only provider start"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-agent-control-" t)))
         (workspace (mevedel-workspace-get-or-create
                     'project "agent-control" root "agent-control"))
         (session (mevedel-session-create "main" workspace))
         (parent (generate-new-buffer " *agent-control-parent*"))
         invocations)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
                     (lambda (_callback _role _description _message child
                                        _buffer &optional _configure)
                       (push child invocations)
                       'provider-request)))
            (let ((record
                   (mevedel-agent-control-spawn
                    session "worker" "Implement the change.")))
              (should (equal "/root/worker"
                             (mevedel-agent-record-path record)))
              (should (eq 'running (mevedel-agent-record-activity record)))
              (should (stringp
                       (mevedel-agent-record-conversation-location record)))))
          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
                     (lambda (callback _role _description _message child
                                       _buffer &optional _configure)
                       (push child invocations)
                       (funcall callback "Synchronous completion.")
                       'provider-request)))
            (let ((record
                   (mevedel-agent-control-spawn
                    session "synchronous" "Complete immediately.")))
              (should (eq 'idle (mevedel-agent-record-activity record)))
              (should (= 1 (mevedel-agent-control--active-count session)))
              (should (= 1 (length (mevedel-session-messages session))))))
          (let (runner-called)
            (cl-letf (((symbol-function
                        'mevedel-agent-exec--save-transcript-buffer)
                       (lambda (_invocation) nil))
                      ((symbol-function 'mevedel-agent-exec--run)
                       (lambda (&rest _)
                         (setq runner-called t)
                         'provider-request)))
              (should-error
               (mevedel-agent-control-spawn
                session "ephemeral" "Require a durable transcript."))
              (should-not runner-called)
              (should-not (assoc "/root/ephemeral"
                                 (mevedel-session-agent-registry session))))))
      (dolist (invocation invocations)
        (when-let* ((buffer (mevedel-agent-invocation-buffer invocation))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (mevedel-session-save-path session)
        (mevedel-session-persistence-lock-release
         (mevedel-session-save-path session)))
      (when (buffer-live-p parent)
        (kill-buffer parent))
      (delete-directory root t))))

(provide 'test-mevedel-agent-control)

;;; test-mevedel-agent-control.el ends here
