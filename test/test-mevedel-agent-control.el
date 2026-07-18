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
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-tools-register)

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

(mevedel-deftest mevedel-agent-control-retained-buffer-p ()
  ,test
  (test)
  :doc "recognizes only live conversation buffers owned by the session registry"
  (let ((session (mevedel-agent-control-test--session))
        (buffer (generate-new-buffer " *agent-control-retained*")))
    (unwind-protect
        (progn
          (should-not
           (mevedel-agent-control-retained-buffer-p session buffer))
          (setf (mevedel-session-agent-registry session)
                (list
                 (cons "/root/worker"
                       (mevedel-agent-record--create
                        :path "/root/worker"
                        :conversation-buffer buffer))))
          (should
           (mevedel-agent-control-retained-buffer-p session buffer)))
      (kill-buffer buffer))
    (should-not
     (mevedel-agent-control-retained-buffer-p session buffer))))

(mevedel-deftest mevedel-agent-control-teardown-session ()
  ,test
  (test)
  :doc "kills all registered conversation buffers without requiring open views"
  (let ((session (mevedel-agent-control-test--session))
        (first (generate-new-buffer " *agent-control-teardown-one*"))
        (second (generate-new-buffer " *agent-control-teardown-two*")))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/one"
                 (mevedel-agent-record--create
                  :path "/root/one" :conversation-buffer first))
           (cons "/root/two"
                 (mevedel-agent-record--create
                  :path "/root/two" :conversation-buffer second))))
    (unwind-protect
        (progn
          (mevedel-agent-control-teardown-session session)
          (should-not (buffer-live-p first))
          (should-not (buffer-live-p second)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))))

  :doc "flushes modified retained transcripts before killing their buffers"
  (let* ((root (file-name-as-directory
                (make-temp-file "agent-control-teardown-save-" t)))
         (relative "agents/worker.chat.org")
         (absolute (expand-file-name relative root))
         (session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         buffer)
    (make-directory (file-name-directory absolute) t)
    (write-region "previous\n" nil absolute nil 'silent)
    (setq buffer (find-file-noselect absolute))
    (setf (mevedel-session-save-path session) root)
    (setf (mevedel-agent-invocation-agent-id invocation) "default--worker")
    (setf (mevedel-agent-invocation-buffer invocation) buffer)
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          relative)
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer buffer))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation invocation)
            (goto-char (point-max))
            (insert "latest\n"))
          (mevedel-agent-control-teardown-session session)
          (should-not (buffer-live-p buffer))
          (with-temp-buffer
            (insert-file-contents absolute)
            (should (equal "previous\nlatest\n" (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)))

  :doc "keeps a modified retained buffer alive when its flush cannot persist"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (buffer (generate-new-buffer " *agent-control-unsaved*")))
    (setf (mevedel-agent-invocation-buffer invocation) buffer)
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (mevedel-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer buffer))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation invocation)
            (insert "unsaved"))
          (let ((warning-minimum-level :error))
            (mevedel-agent-control-teardown-session session))
          (should (buffer-live-p buffer))
          (should (buffer-modified-p buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-agent-control-current-path ()
  ,test
  (test)
  :doc "returns known caller paths and rejects unregistered children"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (record (mevedel-agent-record--create
                  :id "default--opaque"
                  :path "/root/worker"
                  :activity 'running))
         (buffer (generate-new-buffer " *agent-control-current*")))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--opaque")
    (setf (mevedel-agent-invocation-path invocation) "/root/worker")
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (progn
          (should (equal "/root"
                         (mevedel-agent-control-current-path session)))
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation invocation)
            (should (equal "/root/worker"
                           (mevedel-agent-control-current-path session)))))
      (kill-buffer buffer))
    (setq invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default))
          buffer (generate-new-buffer " *agent-control-unregistered*"))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--missing")
    (setf (mevedel-agent-invocation-path invocation) "/root/missing")
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--agent-invocation invocation)
          (should-error (mevedel-agent-control-current-path session)
                        :type 'error))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-control-resolve-path ()
  ,test
  (test)
  :doc "resolves canonical and relative descendants without accepting IDs"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/alpha"
                 (mevedel-agent-record--create :path "/root/alpha"))
           (cons "/root/alpha/child"
                 (mevedel-agent-record--create
                  :path "/root/alpha/child"))))
    (should (equal "/root/alpha"
                   (mevedel-agent-control-resolve-path
                    session "/root" "alpha")))
    (should (equal "/root/alpha/child"
                   (mevedel-agent-control-resolve-path
                    session "/root/alpha" "child")))
    (should (equal "/root"
                   (mevedel-agent-control-resolve-path
                    session "/root/alpha" "/root")))
    (dolist (target '("../alpha" "./alpha" "alpha//child"
                      "default--opaque" "/root/unknown" ""))
      (should-error
       (mevedel-agent-control-resolve-path session "/root" target)))))

(mevedel-deftest mevedel-agent-control-list-agents ()
  ,test
  (test)
  :doc "lists path-sorted minimal roster entries with subtree filtering"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/zeta"
                 (mevedel-agent-record--create
                  :id "private-z" :path "/root/zeta"
                  :role "worker" :activity 'idle
                  :conversation-location "agents/zeta.chat.org"))
           (cons "/root/alpha/child"
                 (mevedel-agent-record--create
                  :id "private-c" :path "/root/alpha/child"
                  :role "explorer" :activity 'starting))
           (cons "/root/alpha"
                 (mevedel-agent-record--create
                  :id "private-a" :path "/root/alpha"
                  :role "default" :activity 'running))))
    (should
     (equal '((:path "/root" :role "default" :activity "idle")
              (:path "/root/alpha" :role "default" :activity "running")
              (:path "/root/alpha/child" :role "explorer" :activity "starting")
              (:path "/root/zeta" :role "worker" :activity "idle"))
            (mevedel-agent-control-list-agents session)))
    (setf (mevedel-session-agent-root-activity session) 'running)
    (should (equal "running"
                   (plist-get
                    (car (mevedel-agent-control-list-agents session))
                    :activity)))
    (should
     (equal '((:path "/root/alpha" :role "default" :activity "running")
              (:path "/root/alpha/child" :role "explorer" :activity "starting"))
            (mevedel-agent-control-list-agents session "/root/alpha")))
    (should-not
     (mevedel-agent-control-list-agents session "/root/missing"))))

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
    (setq record (mevedel-agent-control--reserve
                  session "/root" "first" "default"))
    (should (equal "/root/first" (mevedel-agent-record-path record)))
    (should-error (mevedel-agent-control--reserve
                   session "/root" "first" "default"))
    (should-error (mevedel-agent-control--reserve
                   session "/root" "second" "worker"))
    (setf (mevedel-agent-record-activity record) 'idle)
    (let ((child
           (mevedel-agent-control--reserve
            session "/root/first" "second" "worker")))
      (should (equal "/root/first/second"
                     (mevedel-agent-record-path child)))
      (should (equal "/root/first"
                     (mevedel-agent-record-parent-path child)))
      (should (equal "worker" (mevedel-agent-record-role child))))))

(mevedel-deftest mevedel-agent-control-context-path ()
  ,test
  (test)
  :doc "maps root and retained invocation contexts to canonical paths"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation--create
                      :agent-id "alpha-id" :path "/root/alpha")))
    (setf (mevedel-agent-invocation-parent-session invocation) session)
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/alpha"
                 (mevedel-agent-record--create
                  :id "alpha-id" :path "/root/alpha"))))
    (should (equal "/root" (mevedel-agent-control-context-path session)))
    (should (equal "/root/alpha"
                   (mevedel-agent-control-context-path invocation)))
    (setf (mevedel-session-agent-registry session) nil)
    (should-error (mevedel-agent-control-context-path invocation))))

(mevedel-deftest mevedel-agent-control-direct-children ()
  ,test
  (test)
  :doc "returns only direct children sorted by path with compact role data"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/zeta"
                 (mevedel-agent-record--create
                  :path "/root/zeta" :parent-path "/root"
                  :role "explorer"))
           (cons "/root/alpha/deep"
                 (mevedel-agent-record--create
                  :path "/root/alpha/deep" :parent-path "/root/alpha"
                  :role "reviewer"))
           (cons "/root/alpha"
                 (mevedel-agent-record--create
                  :path "/root/alpha" :parent-path "/root"
                  :role "worker"))))
    (should
     (equal '((:path "/root/alpha" :role "worker")
              (:path "/root/zeta" :role "explorer"))
            (mevedel-agent-control-direct-children session "/root")))))

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

(mevedel-deftest mevedel-agent-control-interrupt ()
  ,test
  (test)
  :doc "rejects root, self, malformed, unknown, and opaque-id targets"
  (let* ((session (mevedel-agent-control-test--session))
         (caller-invocation
          (mevedel-agent-invocation--create :agent-id "caller-id"))
         (caller
          (mevedel-agent-record--create
           :id "caller-id" :path "/root/caller" :activity 'running
           :invocation caller-invocation))
         (peer
          (mevedel-agent-record--create
           :id "peer-id" :path "/root/peer" :activity 'idle)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/caller" caller)
                (cons "/root/peer" peer)))
    (let ((mevedel--agent-invocation caller-invocation))
      (dolist (target '("/root" "/root/caller" "/root/missing"
                        "peer-id" "../peer"))
        (should-error
         (mevedel-agent-control-interrupt session target)))))

  :doc "idle interruption is a successful no-op preserving retained state"
  (let* ((session (mevedel-agent-control-test--session))
         (mail '((:type MAIL :sender "/root" :recipient "/root/idle"
                  :payload "remember")))
         (record
          (mevedel-agent-record--create
           :id "idle-id" :path "/root/idle" :parent-path "/root"
           :activity 'idle :mailbox mail
           :configuration '(:role "default"))))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/idle" record)))
    (let ((result
           (mevedel-agent-control-interrupt session "/root/idle")))
      (should (equal "/root/idle" (plist-get result :path)))
      (should (eq 'idle (plist-get result :previous-activity)))
      (should (eq 'idle (mevedel-agent-record-activity record)))
      (should (eq mail (mevedel-agent-record-mailbox record)))
      (should (assoc "/root/idle"
                     (mevedel-session-agent-registry session)))
      (should-not (mevedel-session-messages session))))

  :doc "active interruption settles once, retains descendants, and permits follow-up"
  (let* ((session (mevedel-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-interrupt*"))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "parent-id" :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "parent-id" :path "/root/parent" :parent-path "/root"
           :activity 'running :invocation invocation
           :conversation-buffer buffer))
         (descendant
          (mevedel-agent-record--create
           :id "child-id" :path "/root/parent/child"
           :parent-path "/root/parent" :activity 'running))
         interrupted)
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/parent" record)
                      (cons "/root/parent/child" descendant)))
          (cl-letf
              (((symbol-function 'mevedel-agent-runtime-interrupt)
                (lambda (seen-invocation reason)
                  (setq interrupted (list seen-invocation reason))
                  (setf (mevedel-agent-invocation-terminal-reason
                         seen-invocation)
                        reason)
                  (setf (mevedel-agent-invocation-transcript-status
                         seen-invocation)
                        'aborted)
                  "Agent turn interrupted.\n\nReason: interrupted by /root\n\nPartial response: useful work")))
            (let ((result
                   (mevedel-agent-control-interrupt
                    session "/root/parent")))
              (should (eq 'running (plist-get result :previous-activity)))
              (should (eq invocation (car interrupted)))
              (should (string-match-p "/root" (cadr interrupted)))))
          (let ((result (car (mevedel-session-messages session))))
            (should (= 1 (length (mevedel-session-messages session))))
            (should (eq 'RESULT (plist-get result :type)))
            (should (eq 'interrupted (plist-get result :outcome)))
            (should (string-match-p "useful work"
                                    (plist-get result :payload))))
          (should (eq 'idle (mevedel-agent-record-activity record)))
          (should-not (mevedel-agent-record-invocation record))
          (should (eq 'running
                      (mevedel-agent-record-activity descendant)))
          (should (assoc "/root/parent"
                         (mevedel-session-agent-registry session)))
          (should (assoc "/root/parent/child"
                         (mevedel-session-agent-registry session)))
          (cl-letf
              (((symbol-function 'mevedel-agent-runtime-dispatch)
                (lambda (_callback _agent _description _message &rest keys)
                  (let ((next
                         (mevedel-agent-invocation--create
                          :agent-id "parent-id" :parent-session session
                          :buffer buffer
                          :path (plist-get keys :path))))
                    (funcall (plist-get keys :on-invocation) next)
                    t))))
            (mevedel-agent-control-followup
             session "/root/parent" "continue"))
          (should (eq 'running (mevedel-agent-record-activity record))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "provider abort failure leaves the retained turn active"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "abort-fails-id" :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "abort-fails-id" :path "/root/abort_fails"
           :parent-path "/root" :activity 'running
           :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/abort_fails" record)))
    (cl-letf
        (((symbol-function 'mevedel-agent-runtime-interrupt)
          (lambda (_invocation _reason)
            (error "Backend abort failed"))))
      (should-error
       (mevedel-agent-control-interrupt
        session "/root/abort_fails")))
    (should (eq 'running (mevedel-agent-record-activity record)))
    (should (eq invocation (mevedel-agent-record-invocation record)))
    (should-not (mevedel-session-messages session)))

  :doc "interrupt-versus-settlement races emit exactly one RESULT"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "race-id" :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "race-id" :path "/root/race" :parent-path "/root"
           :activity 'running :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/race" record)))
    (cl-letf
        (((symbol-function 'mevedel-agent-runtime-interrupt)
          (lambda (seen-invocation _reason)
            (setf (mevedel-agent-invocation-transcript-status
                   seen-invocation)
                  'aborted)
            "interrupted first")))
      (mevedel-agent-control-interrupt session "/root/race"))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'completed)
    (mevedel-agent-control--settle
     session record invocation "late completion")
    (should (= 1 (length (mevedel-session-messages session))))
    (should (eq 'interrupted
                (plist-get (car (mevedel-session-messages session))
                           :outcome))))

  :doc "a completion that wins the race remains the only settlement"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "winner-id" :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "winner-id" :path "/root/winner" :parent-path "/root"
           :activity 'running :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/winner" record)))
    (cl-letf
        (((symbol-function 'mevedel-agent-runtime-interrupt)
          (lambda (seen-invocation _reason)
            (setf (mevedel-agent-invocation-transcript-status
                   seen-invocation)
                  'completed)
            (mevedel-agent-control--settle
             session record seen-invocation "completed first")
            "too late")))
      (let ((result
             (mevedel-agent-control-interrupt session "/root/winner")))
        (should (eq 'running (plist-get result :previous-activity)))))
    (should (= 1 (length (mevedel-session-messages session))))
    (should (eq 'completed
                (plist-get (car (mevedel-session-messages session))
                           :outcome)))))

(mevedel-deftest mevedel-agent-control--record-invocation ()
  ,test
  (test)
  :doc "records the stable opaque identity and persisted conversation location"
  (let ((record (mevedel-agent-record--create :path "/root/worker"))
        (invocation (mevedel-agent-invocation-create
                     (mevedel-agent-default)))
        (buffer (generate-new-buffer " *agent-control-record*")))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--opaque")
    (setf (mevedel-agent-invocation-path invocation) "/root/worker")
    (setf (mevedel-agent-invocation-buffer invocation) buffer)
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/default.chat.org")
    (mevedel-agent-control--record-invocation record invocation)
    (should (equal "default--opaque" (mevedel-agent-record-id record)))
    (should (eq invocation (mevedel-agent-record-invocation record)))
    (should (eq buffer (mevedel-agent-record-conversation-buffer record)))
    (should (equal "agents/default.chat.org"
                   (mevedel-agent-record-conversation-location record)))
    (kill-buffer buffer)))

(mevedel-deftest mevedel-agent-control-followup ()
  ,test
  (test)
  :doc "rejects root and idle activation at capacity but permits live steering"
  (let* ((session (mevedel-agent-control-test--session))
         (target (mevedel-agent-record--create
                  :path "/root/target" :activity 'idle))
         (busy (mevedel-agent-record--create
                :path "/root/busy" :activity 'running))
         steered)
    (setf (mevedel-session-agent-turn-capacity session) 1)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/target" target)
                (cons "/root/busy" busy)))
    (should-error
     (mevedel-agent-control-followup session "/root" "Do not run root."))
    (should-error
     (mevedel-agent-control-followup session "target" "Start now."))
    (should (eq 'idle (mevedel-agent-record-activity target)))
    (setf (mevedel-agent-record-activity target) 'running)
    (cl-letf (((symbol-function 'mevedel-agent-runtime-steer)
               (lambda (invocation sender message)
                 (setq steered (list invocation sender message)))))
      (should (eq target
                  (mevedel-agent-control-followup
                   session "target" "Steer safely."))))
    (should (equal '(nil "/root" "Steer safely.") steered)))

  :doc "attributes peer steering while returning results to the spawn parent"
  (let* ((session (mevedel-agent-control-test--session))
         (peer-invocation (mevedel-agent-invocation-create
                           (mevedel-agent-default)))
         (target-invocation (mevedel-agent-invocation-create
                             (mevedel-agent-default)))
         (peer (mevedel-agent-record--create
                :id "default--peer" :path "/root/peer"
                :parent-path "/root" :activity 'running
                :invocation peer-invocation))
         (target (mevedel-agent-record--create
                  :id "default--target" :path "/root/target"
                  :parent-path "/root" :activity 'running
                  :invocation target-invocation))
         (buffer (generate-new-buffer " *agent-control-peer-followup*"))
         sender)
    (setf (mevedel-agent-invocation-agent-id peer-invocation)
          "default--peer")
    (setf (mevedel-agent-invocation-path peer-invocation) "/root/peer")
    (setf (mevedel-agent-invocation-path target-invocation) "/root/target")
    (setf (mevedel-agent-invocation-transcript-status target-invocation)
          'completed)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/peer" peer)
                (cons "/root/target" target)))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--agent-invocation peer-invocation)
          (cl-letf (((symbol-function 'mevedel-agent-runtime-steer)
                     (lambda (_invocation seen-sender _message)
                       (setq sender seen-sender))))
            (mevedel-agent-control-followup
             session "/root/target" "Review the peer's update."))
          (should (equal "/root/peer" sender))
          (mevedel-agent-control--settle
           session target target-invocation "Peer review complete."))
      (kill-buffer buffer))
    (let ((result (car (mevedel-session-messages session))))
      (should (equal "/root/target" (plist-get result :sender)))
      (should (equal "/root" (plist-get result :recipient))))))

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
         invocations
         roles)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (setq-local mevedel--workspace workspace)
          (cl-letf (((symbol-function 'mevedel-agent-exec--run)
                     (lambda (_callback role _description _message child
                                        _buffer &optional _configure)
                       (push role roles)
                       (push child invocations)
                       'provider-request)))
            (let ((record
                   (mevedel-agent-control-spawn
                    session "worker" "Implement the change.")))
              (should (equal "/root/worker"
                             (mevedel-agent-record-path record)))
              (should (eq 'running (mevedel-agent-record-activity record)))
              (should (stringp
                       (mevedel-agent-record-conversation-location record))))
            (let* ((parent-invocation (car invocations))
                   (mevedel--agent-invocation parent-invocation)
                   (nested
                    (mevedel-agent-control-spawn
                     session "research" "Investigate first."
                     :role "explorer")))
              (should (equal "/root/worker/research"
                             (mevedel-agent-record-path nested)))
              (should (equal "/root/worker"
                             (mevedel-agent-record-parent-path nested)))
              (should (equal "explorer"
                             (mevedel-agent-record-role nested)))
              (should (equal "explorer" (car roles)))
              (let ((nested-invocation
                     (mevedel-agent-record-invocation nested)))
                (mevedel-agent-control--settle
                 session nested nested-invocation "Nested result."))
              (should-not (mevedel-session-messages session))
              (let ((result
                     (car (mevedel-agent-control--mailbox
                           session "/root/worker"))))
                (should (eq 'RESULT (plist-get result :type)))
                (should (equal "/root/worker/research"
                               (plist-get result :sender)))
                (should (equal "/root/worker"
                               (plist-get result :recipient)))))
            (should-error
             (mevedel-agent-control-spawn
              session "unknown" "Fail before reservation."
              :role "not_a_role")
             :type 'user-error)
            (should-not (assoc "/root/unknown"
                               (mevedel-session-agent-registry session))))
          (let ((mevedel-agent-exec--agents
                 '(("explorer" :description "available"))))
            (should-error
             (mevedel-agent-control-spawn
              session "unavailable" "Reject session-excluded role."
              :role "worker")
             :type 'user-error)
            (should-not (assoc "/root/unavailable"
                               (mevedel-session-agent-registry session))))
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

(mevedel-deftest mevedel-agent-control-send-message ()
  ,test
  (test)
  :doc "queues canonical MAIL records in FIFO order without activating recipients"
  (let* ((session (mevedel-agent-control-test--session))
         (idle (mevedel-agent-record--create
                :id "idle-id" :path "/root/idle" :activity 'idle))
         (running (mevedel-agent-record--create
                   :id "running-id" :path "/root/running"
                   :activity 'running)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/idle" idle)
                (cons "/root/running" running)))
    (should (equal "/root/idle"
                   (mevedel-agent-control-send-message
                    session "/root/idle" "first")))
    (mevedel-agent-control-send-message session "/root/idle" "second")
    (mevedel-agent-control-send-message session "/root/running" "active")
    (should (eq 'idle (mevedel-agent-record-activity idle)))
    (should (eq 'running (mevedel-agent-record-activity running)))
    (should
     (equal
      '((:type MAIL :sender "/root" :recipient "/root/idle"
         :payload "first")
        (:type MAIL :sender "/root" :recipient "/root/idle"
         :payload "second"))
      (mapcar
       (lambda (record)
         (cl-loop for (key value) on record by #'cddr
                  unless (eq key :timestamp)
                  append (list key value)))
       (mevedel-agent-control--mailbox session "/root/idle"))))
    (should (= 1 (length
                  (mevedel-agent-control--mailbox
                   session "/root/running")))))

  :doc "accepts tree-wide peer paths and rejects empty or unknown targets"
  (let* ((session (mevedel-agent-control-test--session))
         (sender (mevedel-agent-record--create
                  :id "sender-id" :path "/root/sender" :activity 'running))
         (peer (mevedel-agent-record--create
                :id "peer-id" :path "/root/peer" :activity 'idle))
         (mevedel--agent-invocation
          (mevedel-agent-invocation--create
           :agent-id "sender-id" :path "/root/sender")))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/sender" sender)
                (cons "/root/peer" peer)))
    (mevedel-agent-control-send-message session "/root/peer" "hello")
    (let ((record (car (mevedel-agent-control--mailbox
                        session "/root/peer"))))
      (should (equal "/root/sender" (plist-get record :sender))))
    (dolist (args '(("/root/peer" "") ("" "body")
                    ("/root/missing" "body")))
      (should-error
       (mevedel-agent-control-send-message
        session (car args) (cadr args))))))

(mevedel-deftest mevedel-agent-control-wait ()
  ,test
  (test)
  :doc "releases immediately when mail was queued before the wait"
  (let ((session (mevedel-agent-control-test--session))
        reasons)
    (mevedel-agent-control-send-message session "/root" "ready")
    (mevedel-agent-control-wait
     session (lambda (reason) (push reason reasons)) nil)
    (should (equal '(mailbox) reasons))
    (should-not (mevedel-session-agent-root-waiter session)))

  :doc "releases exactly once when mail arrives after the wait starts"
  (let ((session (mevedel-agent-control-test--session))
        reasons scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'fake-timer)))
      (mevedel-agent-control-wait
       session (lambda (reason) (push reason reasons)) 10000)
      (should (mevedel-session-agent-root-waiter session))
      (mevedel-agent-control-send-message session "/root" "arrived")
      (should (equal '(mailbox) reasons))
      (apply (car scheduled) (cdr scheduled))
      (should (equal '(mailbox) reasons))))

  :doc "times out successfully and validates the inclusive millisecond bounds"
  (let ((session (mevedel-agent-control-test--session))
        reasons scheduled delay)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seen-delay _repeat function &rest args)
                 (setq delay seen-delay
                       scheduled (cons function args))
                 'fake-timer)))
      (mevedel-agent-control-wait
       session (lambda (reason) (push reason reasons)) nil)
      (should (= 30 delay))
      (apply (car scheduled) (cdr scheduled))
      (should (equal '(timeout) reasons)))
    (dolist (timeout '(9999 3600001 10000.0 "10000"))
      (should-error
       (mevedel-agent-control-wait session #'ignore timeout)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (mevedel-agent-control-wait session #'ignore 10000)
      (mevedel-agent-control-cancel-wait session "/root")
      (mevedel-agent-control-wait session #'ignore 3600000)
      (mevedel-agent-control-cancel-wait session "/root")))

  :doc "new root user steering is queued for injection and interrupts the wait"
  (let ((session (mevedel-agent-control-test--session))
        reasons)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (mevedel-agent-control-wait
       session (lambda (reason) (push reason reasons)) 10000)
      (should (mevedel-agent-control-steer-user session "new direction"))
      (should (equal '(user) reasons))
      (let ((message (car (mevedel-agent-control--mailbox
                           session "/root"))))
        (should (eq 'USER (plist-get message :type)))
        (should (equal "new direction" (plist-get message :payload))))
      (should-not
       (mevedel-agent-control-steer-user session "too late"))))

  :doc "yielded Bash completion wakes a root wait through mailbox notification"
  (let ((session (mevedel-agent-control-test--session))
        reason)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (mevedel-agent-control-wait
       session (lambda (seen) (setq reason seen)) 10000)
      (should
       (mevedel-agent-runtime-queue-execution-completion
        session "/root" "root execution complete"))
      (should (eq 'mailbox reason))))

  :doc "yielded Bash completion wakes a retained non-root wait"
  (let* ((session (mevedel-agent-control-test--session))
         (parent-buffer (generate-new-buffer " *agent-control-execution*"))
         (agent-id "worker-execution")
         (invocation
          (mevedel-agent-invocation--create
           :path "/root/worker"
           :agent-id agent-id
           :parent-session session
           :parent-data-buffer parent-buffer))
         (record
          (mevedel-agent-record--create
           :id agent-id :path "/root/worker" :activity 'running
           :invocation invocation))
         (fsm (gptel-make-fsm
               :state 'WAIT
               :info (list :mevedel-agent-invocation invocation)))
         reason)
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/worker" record)))
          (with-current-buffer parent-buffer
            (setq-local mevedel-agent-runtime--fsms
                        (list (cons agent-id fsm))))
          (cl-letf (((symbol-function 'run-at-time)
                     (lambda (&rest _) 'fake-timer)))
            (should
             (mevedel-agent-runtime-queue-execution-completion
              invocation "/root/worker" "already complete"))
            (let ((mevedel--agent-invocation invocation))
              (mevedel-agent-control-wait
               session (lambda (seen) (setq reason seen)) 10000))
            (should (eq 'mailbox reason))
            (setf (mevedel-agent-invocation-messages invocation) nil)
            (setq reason nil)
            (let ((mevedel--agent-invocation invocation))
              (mevedel-agent-control-wait
               session (lambda (seen) (setq reason seen)) 10000))
            (should
             (mevedel-agent-runtime-queue-execution-completion
              invocation "/root/worker" "agent execution complete"))
            (should (eq 'mailbox reason))))
      (kill-buffer parent-buffer)))

  :doc "a suspended non-root caller retains its active-turn slot"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "worker-id" :path "/root/worker"
           :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "worker-id" :path "/root/worker" :activity 'running
           :invocation invocation))
         reason)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (let ((mevedel--agent-invocation invocation))
        (mevedel-agent-control-wait
         session (lambda (seen) (setq reason seen)) 10000))
      (should (= 1 (mevedel-agent-control--active-count session)))
      (mevedel-agent-control-send-message session "/root/worker" "wake")
      (should (eq 'mailbox reason))
      (should (= 1 (mevedel-agent-control--active-count session)))))

  :doc "follow-up steering wakes a non-root wait without an FSM wait state"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation
          (mevedel-agent-invocation--create
           :agent-id "steered-id" :path "/root/steered"
           :parent-session session))
         (record
          (mevedel-agent-record--create
           :id "steered-id" :path "/root/steered" :activity 'running
           :invocation invocation))
         reason)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/steered" record)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer))
              ((symbol-function 'mevedel-agent-runtime-steer)
               (lambda (seen-invocation _sender _message)
                 (mevedel-agent-control-notify-context-mailbox
                  seen-invocation 'steering)
                 t)))
      (let ((mevedel--agent-invocation invocation))
        (mevedel-agent-control-wait
         session (lambda (seen) (setq reason seen)) 10000))
      (mevedel-agent-control-followup
       session "/root/steered" "new direction")
      (should (eq 'steering reason))
      (should (eq 'running (mevedel-agent-record-activity record))))))

(provide 'test-mevedel-agent-control)

;;; test-mevedel-agent-control.el ends here
