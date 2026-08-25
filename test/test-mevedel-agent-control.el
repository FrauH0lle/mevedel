;;; test-mevedel-agent-control.el --- Durable agent control tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the session-owned agent registry's validation, admission, rollback,
;; settlement, result bounding, and provider-dispatch boundary.

;;; Code:

(require 'mevedel-agent-control)
(require 'mevedel-agent-conversation)
(require 'mevedel-agent-exec)
(require 'mevedel-agent-runtime)
(require 'mevedel-agents)
(require 'mevedel-compact-evidence)
(require 'mevedel-compact-target)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `gptel'
(declare-function gptel-make-openai "ext:gptel-openai" (name &rest args))
(defvar gptel--known-backends)
(defvar gptel-backend)
(defvar gptel-model)

(mevedel-tools-register)

(defun mevedel-agent-control-test--configuration (&optional context)
  "Return a small frozen configuration with optional gptel CONTEXT."
  (mevedel-agent-configuration--create
   :agent
   (mevedel-agent--create
    :name "default"
    :description "Persisted default agent"
    :tools '((:tool "Read"))
    :system-prompt "Frozen instructions"
    :max-turns 12
    :hook-rules nil
    :frozen-p t)
   :request-locals
   (list (cons 'gptel-backend gptel-backend)
         (cons 'gptel-model 'test-model)
         (cons 'gptel-tools
               (list (gptel-get-tool '("mevedel" "Read"))))
         (cons 'gptel-context context))))

(defun mevedel-agent-control-test--session ()
  "Return a fresh in-memory session for agent-control tests."
  (mevedel-session-create
   "main"
   (mevedel-workspace--create
    :type 'project
    :id "agent-control"
    :root temporary-file-directory
    :name "agent-control")))

(defun mevedel-agent-control-test--settle
    (session record invocation response &optional event)
  "Settle one test turn through the runtime-owned commit boundary."
  (when-let* ((transaction
               (mevedel-agent-control--settle
                session record invocation response event)))
    (let (committed)
      (condition-case err
          (progn
            (mevedel-agent-control-commit-session session)
            (setq committed t)
            (funcall (cdr transaction)))
        (error
         (unless committed
           (funcall (car transaction)))
         (signal (car err) (cdr err)))))
    transaction))

(mevedel-deftest mevedel-agent-control-active-activity-p ()
  ,test
  (test)
  :doc "recognizes every activity that owns one tree-wide capacity slot"
  (dolist (case '((starting . t) (running . t) (waiting . t)
                  (permission-blocked . t) (interaction-blocked . t)
                  (idle)))
    (should (eq (cdr case)
                (and (mevedel-agent-control-active-activity-p (car case))
                     t)))))

(mevedel-deftest mevedel-agent-control--active-p ()
  ,test
  (test)
  :doc "every persisted in-flight activity owns capacity while idle does not"
  (dolist (case '((starting . t) (running . t) (waiting . t)
                  (permission-blocked . t) (interaction-blocked . t) (idle)))
    (should (eq (cdr case)
                (and (mevedel-agent-control--active-p
                      (mevedel-agent-record--create :activity (car case)))
                     t)))))

(mevedel-deftest mevedel-agent-control--active-count ()
  ,test
  (test)
  :doc "counts published active turns and unpublished reservations"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/a"
                 (mevedel-agent-record--create :activity 'starting))
           (cons "/root/b"
                 (mevedel-agent-record--create :activity 'running))
           (cons "/root/c"
                 (mevedel-agent-record--create :activity 'idle))))
    (setf (mevedel-session-agent-reservations session)
          (list (cons "/root/d"
                      (mevedel-agent-record--create :activity 'starting))))
    (should (= 3 (mevedel-agent-control--active-count session)))))

(mevedel-deftest mevedel-agent-control-active-turn-p ()
  ,test
  (test)
  :doc "reports whether any active agent turn owns capacity"
  (let ((session (mevedel-agent-control-test--session)))
    (should-not (mevedel-agent-control-active-turn-p session))
    (setf (mevedel-session-agent-registry session)
          (list
           (cons "/root/idle"
                 (mevedel-agent-record--create :activity 'idle))))
    (should-not (mevedel-agent-control-active-turn-p session))
    (setf (mevedel-agent-record-activity
           (cdar (mevedel-session-agent-registry session)))
          'permission-blocked)
    (should (mevedel-agent-control-active-turn-p session))))

(mevedel-deftest mevedel-agent-control-block-turn ()
  ,test
  (test)
  :doc "composes overlapping blockers and ignores stale or duplicate releases"
  (let* ((session (mevedel-agent-control-test--session))
         (first (mevedel-agent-invocation--create :path "/root/worker"))
         (second (mevedel-agent-invocation--create :path "/root/worker"))
         (record
          (mevedel-agent-record--create
           :activity 'running :invocation first))
         release-wait release-permission stale-release)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (let ((mevedel-agent-control-suppress-persistence t))
      (setq release-wait
            (mevedel-agent-control-block-turn
             session "/root/worker" 'waiting)
            release-permission
            (mevedel-agent-control-block-turn
             session "/root/worker" 'permission-blocked))
      (should (eq 'permission-blocked
                  (mevedel-agent-record-activity record)))
      (should (= 2 (length (mevedel-agent-record-blockers record))))
      (funcall release-wait)
      (funcall release-wait)
      (should (eq 'permission-blocked
                  (mevedel-agent-record-activity record)))
      (funcall release-permission)
      (should (eq 'running (mevedel-agent-record-activity record)))
      (should-not (mevedel-agent-record-blockers record))
      (setq stale-release
            (mevedel-agent-control-block-turn
             session "/root/worker" 'interaction-blocked))
      (setf (mevedel-agent-record-invocation record) second
            (mevedel-agent-record-blockers record) nil
            (mevedel-agent-record-activity record) 'running)
      (funcall stale-release)
      (should (eq 'running (mevedel-agent-record-activity record)))
      (should-not
       (mevedel-agent-control-block-turn session "/root" 'waiting))
      (should-error
       (mevedel-agent-control-block-turn
        session "/root/worker" 'invalid)))))

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

(mevedel-deftest mevedel-agent-control-teardown-session (:quiet t)
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
  :doc "lists path-sorted roster entries with exact activity and subtree filtering"
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
                  :role "default" :activity 'permission-blocked))))
    (should
     (equal '((:path "/root" :role "default" :activity "idle")
              (:path "/root/alpha" :role "default"
               :activity "permission-blocked")
              (:path "/root/alpha/child" :role "explorer" :activity "starting")
              (:path "/root/zeta" :role "worker" :activity "idle"))
            (mevedel-agent-control-list-agents session)))
    (setf (mevedel-session-agent-root-activity session) 'running)
    (should (equal "running"
                   (plist-get
                    (car (mevedel-agent-control-list-agents session))
                    :activity)))
    (should
     (equal '((:path "/root/alpha" :role "default"
               :activity "permission-blocked")
              (:path "/root/alpha/child" :role "explorer" :activity "starting"))
            (mevedel-agent-control-list-agents session "/root/alpha")))
    (should-not
     (mevedel-agent-control-list-agents session "/root/missing"))))

(mevedel-deftest mevedel-agent-control-commit-session ()
  ,test
  (test)
  :doc "requires a successful materialized persistence commit"
  (let ((session (mevedel-agent-control-test--session))
        seen)
    (setf (mevedel-session-save-path session) temporary-file-directory)
    (cl-letf (((symbol-function
                'mevedel-session-persistence-save-agent-state)
               (lambda (value) (setq seen value) t)))
      (should (mevedel-agent-control-commit-session session)))
    (should (eq session seen))
    (cl-letf (((symbol-function
                'mevedel-session-persistence-save-agent-state)
               (lambda (_value) nil)))
      (should-error (mevedel-agent-control-commit-session session))))
  :doc "accepts unmaterialized or explicitly staged state"
  (let ((session (mevedel-agent-control-test--session)))
    (should (mevedel-agent-control-commit-session session))
    (setf (mevedel-session-save-path session) temporary-file-directory)
    (let ((mevedel-agent-control-suppress-persistence t))
      (should (mevedel-agent-control-commit-session session)))))

(mevedel-deftest mevedel-agent-control--persist-session ()
  ,test
  (test)
  :doc "keeps observational persistence best effort"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-save-path session) temporary-file-directory)
    (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
               (lambda (_session) (error "Commit unavailable"))))
      (should-not (mevedel-agent-control--persist-session session))))
  :doc "does nothing before session materialization or while suppressed"
  (let ((session (mevedel-agent-control-test--session)))
    (should-not (mevedel-agent-control--persist-session session))
    (setf (mevedel-session-save-path session) temporary-file-directory)
    (let ((mevedel-agent-control-suppress-persistence t))
      (should-not (mevedel-agent-control--persist-session session)))))


(mevedel-deftest mevedel-agent-control-recover-interrupted ()
  ,test
  (test)
  :doc "idles every active shape and enqueues one ordinary parent RESULT"
  (let ((session (mevedel-agent-control-test--session)))
    (setf (mevedel-session-agent-registry session)
          (cl-loop for activity in mevedel-agent-control--active-activities
                   for index from 1
                   for path = (format "/root/task%d" index)
                   collect
                   (cons path
                         (mevedel-agent-record--create
                          :path path :parent-path "/root"
                          :activity activity))))
    (should (= 5 (mevedel-agent-control-recover-interrupted session)))
    (should (cl-every
             (lambda (entry)
               (eq 'idle (mevedel-agent-record-activity (cdr entry))))
             (mevedel-session-agent-registry session)))
    (should (= 5 (length (mevedel-session-messages session))))
    (should (cl-every
             (lambda (message)
               (let ((record (cdr (assoc (plist-get message :sender)
                                         (mevedel-session-agent-registry
                                          session)))))
                 (and (eq 'RESULT (plist-get message :type))
                      (eq 'interrupted (plist-get message :outcome))
                      (equal (mevedel-agent-record-settled-result record)
                             (plist-get message :payload)))))
             (mevedel-session-messages session)))
    (should (cl-every
             (lambda (entry)
               (let ((record (cdr entry)))
                 (and (eq 'interrupted
                          (mevedel-agent-record-settled-outcome record))
                      (stringp (mevedel-agent-record-settled-result record)))))
             (mevedel-session-agent-registry session)))
    (should (zerop (mevedel-agent-control-recover-interrupted session)))
    (should (= 5 (length (mevedel-session-messages session)))))

  :doc "includes useful restored partial output and transcript location"
  (let* ((session (mevedel-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-recovery-partial*"))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (record
          (mevedel-agent-record--create
           :path "/root/partial" :parent-path "/root" :activity 'running
           :conversation-buffer buffer
           :conversation-location "agents/partial.chat.org")))
    (unwind-protect
        (progn
          (setf (mevedel-session-agent-registry session)
                (list (cons "/root/partial" record)))
          (setf (mevedel-agent-invocation-buffer invocation) buffer)
          (with-current-buffer buffer
            (setq-local mevedel--agent-invocation invocation)
            (insert (propertize "Useful restored work." 'gptel 'response)))
          (should (= 1 (mevedel-agent-control-recover-interrupted session)))
          (let ((payload (mevedel-agent-record-settled-result record)))
            (should (string-match-p "Useful restored work" payload))
            (should (string-match-p "agents/partial.chat.org" payload))))
      (kill-buffer buffer))))

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
    (should-not (mevedel-session-agent-registry session))
    (should (eq record
                (cdar (mevedel-session-agent-reservations session))))
    (should-error (mevedel-agent-control--reserve
                   session "/root" "first" "default"))
    (should-error (mevedel-agent-control--reserve
                   session "/root" "second" "worker"))
    (setf (mevedel-agent-record-activity record) 'idle)
    (setf (mevedel-session-agent-reservations session) nil
          (mevedel-session-agent-registry session)
          (list (cons "/root/first" record)))
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
    ;; A not-yet-published invocation (fresh spawn before first WAIT)
    ;; resolves to nil instead of erroring.
    (should-not (mevedel-agent-control-context-path invocation))))

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
  :doc "removes the reserved identity and associated transcript index entry"
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
    (setf (mevedel-session-agent-reservations session)
          (list (cons "/root/retry" record)))
    (setf (mevedel-session-agent-transcripts session)
          '(("default--rollback" :path "agents/retry.chat.org")))
    (mevedel-agent-control--rollback session record)
    (should-not (mevedel-session-agent-registry session))
    (should-not (mevedel-session-agent-reservations session))
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

(mevedel-deftest mevedel-agent-control--publish-result ()
  ,test
  (test)
  :doc "durably consumes a workflow RESULT before invoking its handler"
  (let* ((session (mevedel-agent-control-test--session))
         (result '(:type RESULT :sender "/root/review"
                   :recipient "/root" :outcome completed :payload "done"))
         (seen nil)
         (queued-during-handler nil)
         (persisted 0)
         (record
          (mevedel-agent-record--create
           :path "/root/review" :parent-path "/root"
           :result-handler
           (lambda (value)
             (setq seen value)
             (setq queued-during-handler
                   (memq value (mevedel-session-messages session)))))))
    (setf (mevedel-session-save-path session) temporary-file-directory)
    (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
               (lambda (_session) (cl-incf persisted))))
      (mevedel-agent-control--publish-result session record result))
    (should (eq result seen))
    (should-not queued-during-handler)
    (should (= 2 persisted))
    (should-not (mevedel-agent-record-result-handler record))
    (should-not (mevedel-session-messages session)))

  :doc "falls back to durable parent mail when the workflow handler fails"
  (let* ((session (mevedel-agent-control-test--session))
         (queued-during-handler nil)
         (record
          (mevedel-agent-record--create
           :path "/root/review" :parent-path "/root"
           :result-handler
           (lambda (value)
             (setq queued-during-handler
                   (memq value (mevedel-session-messages session)))
             (error "Broken workflow"))))
         (result '(:type RESULT :sender "/root/review"
                   :recipient "/root" :outcome completed :payload "done"))
         warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (setq warning args))))
      (mevedel-agent-control--publish-result session record result))
    (should-not queued-during-handler)
    (should (eq result (car (mevedel-session-messages session))))
    (should-not (mevedel-agent-record-result-handler record))
    (should (string-match-p "result handler failed"
                            (cadr warning))))

  :doc "withholds a successful handler until RESULT consumption commits"
  (let* ((session (mevedel-agent-control-test--session))
         (result '(:type RESULT :sender "/root/review"
                   :recipient "/root" :outcome completed :payload "done"))
         (calls 0)
         (commits 0)
         (record
          (mevedel-agent-record--create
           :path "/root/review" :parent-path "/root"
           :result-handler (lambda (_value) (cl-incf calls)))))
    (cl-letf (((symbol-function 'mevedel-agent-control-commit-session)
               (lambda (_session)
                 (cl-incf commits)
                 (when (= commits 2)
                   (error "Consumption commit unavailable")))))
      (should-error
       (mevedel-agent-control--publish-result session record result))
      (should (zerop calls))
      (should (memq result (mevedel-session-messages session)))
      (should (mevedel-agent-record-result-handler record))
      (mevedel-agent-control--publish-result session record result t))
    (should (= 1 calls))
    (should-not (mevedel-session-messages session))
    (should-not (mevedel-agent-record-result-handler record))))

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
                  :invocation invocation
                  :blockers '((permission-blocked stale)))))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'error)
    (mevedel-agent-control-test--settle
     session record invocation "fallback details"
     '(:error-details "Provider failed"))
    (mevedel-agent-control-test--settle
     session record invocation "duplicate")
    (let ((result (car (mevedel-session-messages session))))
      (should (= 1 (length (mevedel-session-messages session))))
      (should (eq 'idle (mevedel-agent-record-activity record)))
      (should-not (mevedel-agent-record-blockers record))
      (should (eq 'RESULT (plist-get result :type)))
      (should (eq 'errored (plist-get result :outcome)))
      (should (equal "Provider failed" (plist-get result :payload)))
      (should (equal "Provider failed"
                     (mevedel-agent-record-settled-result record)))
      (should (eq 'errored
                  (mevedel-agent-record-settled-outcome record)))))

  :doc "settles a workflow-owned turn once through its RESULT handler"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         results
         (record
          (mevedel-agent-record--create
           :path "/root/review" :parent-path "/root"
           :activity 'running :invocation invocation
           :result-handler (lambda (result) (push result results)))))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'completed)
    (mevedel-agent-control-test--settle
     session record invocation "review result")
    (mevedel-agent-control-test--settle
     session record invocation "duplicate")
    (should (= 1 (length results)))
    (should (eq 'completed (plist-get (car results) :outcome)))
    (should (equal "review result" (plist-get (car results) :payload)))
    (should (equal "review result"
                   (mevedel-agent-record-settled-result record)))
    (should (eq 'completed
                (mevedel-agent-record-settled-outcome record)))
    (should (eq 'idle (mevedel-agent-record-activity record)))
    (should-not (mevedel-agent-record-invocation record))
    (should-not (mevedel-session-messages session))))

  :doc "retains a complete oversized response while mailbox delivery is bounded"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (payload (concat "response-head\n"
                          (make-string 40000 ?x)
                          "\nresponse-tail"))
         (record (mevedel-agent-record--create
                  :path "/root/large"
                  :parent-path "/root"
                  :activity 'running
                  :invocation invocation)))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'completed)
    (mevedel-agent-control-test--settle
     session record invocation payload)
    (let* ((message (car (mevedel-session-messages session)))
           (delivered (plist-get message :payload))
           (retained (mevedel-agent-control-settled-result record)))
      (should (equal payload
                     (mevedel-agent-record-settled-result record)))
      (should (eq 'completed
                  (mevedel-agent-record-settled-outcome record)))
      (should (equal payload (plist-get retained :payload)))
      (should (eq 'completed (plist-get retained :outcome)))
      (should (< (length delivered) (length payload)))
      (should (<= (length delivered)
                  mevedel-agent-control--result-limit))
      (should-not (equal payload delivered))))

  :doc "commit failure retains the active turn and withholds RESULT and wake"
  (let* ((session (mevedel-agent-control-test--session))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (record (mevedel-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation
                  :blockers '((waiting live))))
         reasons)
    (setf (mevedel-session-save-path session) temporary-file-directory
          (mevedel-session-agent-root-waiter session)
          (mevedel-agent-waiter--create
           :callback (lambda (reason) (push reason reasons)))
          (mevedel-agent-invocation-transcript-status invocation) 'completed)
    (dolist (failure '(nil error))
      (cl-letf (((symbol-function
                  'mevedel-session-persistence-save-agent-state)
                 (if (eq failure 'error)
                     (lambda (&rest _) (error "Injected commit failure"))
                   (lambda (&rest _) nil))))
        (should-error
         (mevedel-agent-control-test--settle
          session record invocation "retryable result")))
      (should (eq 'running (mevedel-agent-record-activity record)))
      (should (eq invocation (mevedel-agent-record-invocation record)))
      (should (equal '((waiting live))
                     (mevedel-agent-record-blockers record)))
      (should-not (mevedel-agent-record-settled-result record))
      (should-not (mevedel-session-messages session))
      (should-not reasons)
      (should (mevedel-session-agent-root-waiter session))))

(mevedel-deftest mevedel-agent-control-settled-result ()
  ,test
  (test)
  :doc "hides a retained result while its agent turn is active"
  (let ((record
         (mevedel-agent-record--create
          :path "/root/active"
          :activity 'running
          :settled-result "old result"
          :settled-outcome 'completed)))
    (should-not (mevedel-agent-control-settled-result record)))
  :doc "returns a retained result and terminal outcome when idle"
  (let* ((record
          (mevedel-agent-record--create
           :path "/root/idle"
           :activity 'idle
           :settled-result "complete result"
           :settled-outcome 'completed))
         (result (mevedel-agent-control-settled-result record)))
    (should (equal '(:outcome completed :payload "complete result") result))
    (should (equal "complete result"
                   (mevedel-agent-record-settled-result record)))))

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
           :configuration
           (mevedel-agent-configuration--create
            :agent (mevedel-agent--create
                    :name "default" :frozen-p t))
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
                  (let ((response
                         "Agent turn interrupted.\n\nReason: interrupted by /root\n\nPartial response: useful work"))
                    (mevedel-agent-control-test--settle
                     session record seen-invocation response
                     (list :mevedel-agent-terminal-status 'aborted
                           :response response))
                    response))))
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
          (should (eq 'interrupted
                      (mevedel-agent-record-settled-outcome record)))
          (should (equal (plist-get (car (mevedel-session-messages session))
                                    :payload)
                         (mevedel-agent-record-settled-result record)))
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
                (lambda (_agent _description _message &rest keys)
                  (let ((next
                         (mevedel-agent-invocation--create
                          :agent-id "parent-id" :parent-session session
                          :buffer buffer
                          :path (plist-get keys :path))))
                    (setf (mevedel-agent-invocation-frozen-configuration next)
                          (plist-get keys :frozen-configuration))
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
            (mevedel-agent-control-test--settle
             session record seen-invocation "interrupted first"
             '(:mevedel-agent-terminal-status aborted
               :response "interrupted first")))))
      (mevedel-agent-control-interrupt session "/root/race"))
    (setf (mevedel-agent-invocation-transcript-status invocation) 'completed)
    (mevedel-agent-control-test--settle
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
            (mevedel-agent-control-test--settle
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
  :doc "publishes a reserved identity with its conversation location"
  (let ((session (mevedel-agent-control-test--session))
        (record (mevedel-agent-record--create :path "/root/worker"))
        (invocation (mevedel-agent-invocation-create
                     (mevedel-agent-default)))
        (buffer (generate-new-buffer " *agent-control-record*")))
    (setf (mevedel-session-agent-reservations session)
          (list (cons "/root/worker" record)))
    (setf (mevedel-agent-invocation-agent-id invocation) "default--opaque")
    (setf (mevedel-agent-invocation-path invocation) "/root/worker")
    (setf (mevedel-agent-invocation-buffer invocation) buffer)
    (setf (mevedel-agent-invocation-transcript-relative-path invocation)
          "agents/default.chat.org")
    (mevedel-agent-control--record-invocation session record invocation)
    (should (equal "default--opaque" (mevedel-agent-record-id record)))
    (should (eq invocation (mevedel-agent-record-invocation record)))
    (should (eq buffer (mevedel-agent-record-conversation-buffer record)))
    (should (equal "agents/default.chat.org"
                   (mevedel-agent-record-conversation-location record)))
    (should-not (mevedel-session-agent-reservations session))
    (should (eq record
                (cdr (assoc "/root/worker"
                            (mevedel-session-agent-registry session)))))
    (kill-buffer buffer))

  :doc "commit failure restores the reservation and record fields"
  (let* ((session (mevedel-agent-control-test--session))
         (record (mevedel-agent-record--create
                  :path "/root/worker" :activity 'starting
                  :configuration '(:role "worker")))
         (invocation (mevedel-agent-invocation-create
                      (mevedel-agent-default)))
         (buffer (generate-new-buffer " *agent-control-record-failure*")))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) temporary-file-directory
                (mevedel-session-agent-reservations session)
                (list (cons "/root/worker" record))
                (mevedel-agent-invocation-agent-id invocation) "new-id"
                (mevedel-agent-invocation-path invocation) "/root/worker"
                (mevedel-agent-invocation-buffer invocation) buffer
                (mevedel-agent-invocation-transcript-relative-path invocation)
                "agents/new.chat.org")
          (dolist (failure '(nil error))
            (cl-letf (((symbol-function
                        'mevedel-session-persistence-save-agent-state)
                       (if (eq failure 'error)
                           (lambda (&rest _)
                             (error "Injected commit failure"))
                         (lambda (&rest _) nil))))
              (should-error
               (mevedel-agent-control--record-invocation
                session record invocation)))
            (should (eq record
                        (cdr (assoc
                              "/root/worker"
                              (mevedel-session-agent-reservations session)))))
            (should-not (mevedel-session-agent-registry session))
            (should (eq 'starting (mevedel-agent-record-activity record)))
            (should-not (mevedel-agent-record-id record))
            (should-not (mevedel-agent-record-invocation record))
            (should (equal '(:role "worker")
                           (mevedel-agent-record-configuration record)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-agent-control--set-hook-context ()
  ,test
  (test)
  :doc "copies pending context onto its retained identity"
  (let* ((record (mevedel-agent-record--create :path "/root/worker"))
         (entries (list (list :source "hook" :body "context"))))
    (mevedel-agent-control--set-hook-context record entries)
    (setf (plist-get (car entries) :body) "changed")
    (should
     (equal '((:source "hook" :body "context"))
            (mevedel-agent-record-hook-context-pending record)))))

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
    (should (eq target
                (mevedel-agent-control-followup
                 session "target" "Steer safely.")))
    (setq steered (car (mevedel-agent-record-mailbox target)))
    (should (eq 'MAIL (plist-get steered :type)))
    (should (equal "/root" (plist-get steered :sender)))
    (should (equal "Steer safely." (plist-get steered :payload))))

  :doc "rolls a failed retained dispatch back to durable idle state"
  (let* ((session (mevedel-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-failed-followup*"))
         (record
          (mevedel-agent-record--create
           :id "worker--retained"
           :path "/root/worker"
           :parent-path "/root"
           :role "worker"
           :configuration (mevedel-agent-control-test--configuration)
           :activity 'idle
           :conversation-buffer buffer
           :settled-result "previous result"
           :settled-outcome 'completed
           :conversation-location "agents/worker.chat.org"))
         (persisted 0))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-agent-runtime-dispatch)
                   (lambda (&rest _) (error "Provider did not start")))
                  ((symbol-function 'mevedel-agent-control--persist-session)
                   (lambda (_session) (cl-incf persisted))))
          (should-error
           (mevedel-agent-control-followup
            session "/root/worker" "Try the next turn."))
          (should (eq 'idle (mevedel-agent-record-activity record)))
          (should-not (mevedel-agent-record-invocation record))
          (should-not (mevedel-agent-control-settled-result record))
          (should (= 1 persisted)))
      (kill-buffer buffer)))

  :doc "clears the previous result before a retained followup becomes active"
  (let* ((session (mevedel-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-clears-result*"))
         (record
          (mevedel-agent-record--create
           :id "worker--retained"
           :path "/root/worker"
           :parent-path "/root"
           :role "worker"
           :configuration (mevedel-agent-control-test--configuration)
           :activity 'idle
           :conversation-buffer buffer
           :conversation-location "agents/worker.chat.org"
           :settled-result "previous result"
           :settled-outcome 'completed)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-agent-runtime-dispatch)
              (lambda (_agent _description _message &rest keys)
                (should-not (mevedel-agent-control-settled-result record))
                (let ((invocation
                       (mevedel-agent-invocation--create
                        :agent-id "worker--retained"
                        :path "/root/worker"
                        :parent-session session
                        :buffer buffer)))
                  (setf (mevedel-agent-invocation-frozen-configuration
                         invocation)
                        (plist-get keys :frozen-configuration))
                  (funcall (plist-get keys :on-invocation) invocation)
                  t))))
          (mevedel-agent-control-followup
           session "/root/worker" "Start a fresh turn."))
      (kill-buffer buffer))
    (should (eq 'running (mevedel-agent-record-activity record)))
    (should-not (mevedel-agent-control-settled-result record)))

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
         (buffer (generate-new-buffer " *agent-control-peer-followup*")))
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
          (mevedel-agent-control-followup
           session "/root/target" "Review the peer's update.")
          (let ((mail (car (mevedel-agent-record-mailbox target))))
            (should (equal "/root/peer" (plist-get mail :sender)))
            (should (equal "Review the peer's update."
                           (plist-get mail :payload))))
          (mevedel-agent-control-test--settle
           session target target-invocation "Peer review complete."))
      (kill-buffer buffer))
    (let ((result (car (mevedel-session-messages session))))
      (should (equal "/root/target" (plist-get result :sender)))
      (should (equal "/root" (plist-get result :recipient))))))

(mevedel-deftest mevedel-agent-control--dispatch-followup
  ()
  ,test
  (test)
  :doc "reuses frozen configuration without resolving the current role"
  (let* ((session (mevedel-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-frozen-followup*"))
         (agent (mevedel-agent--create
                 :name "worker" :system-prompt "Frozen." :frozen-p t))
         (configuration
          (mevedel-agent-configuration--create :agent agent))
         (turn (mevedel-agent-invocation-create agent))
         (record
          (mevedel-agent-record--create
           :id "worker--retained"
           :path "/root/worker"
           :parent-path "/root"
           :role "worker"
           :configuration configuration
           :activity 'starting
           :conversation-buffer buffer
           :conversation-location "agents/worker.chat.org"))
         captured-agent
         captured-configuration
         captured-tool-use-id)
    (setf (mevedel-agent-invocation-agent-id turn) "worker--retained"
          (mevedel-agent-invocation-path turn) "/root/worker"
          (mevedel-agent-invocation-buffer turn) buffer
          (mevedel-agent-invocation-frozen-configuration turn) configuration
          (mevedel-agent-invocation-transcript-relative-path turn)
          "agents/worker.chat.org"
          (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-agent-resolve-role)
                   (lambda (&rest _)
                     (error "Follow-up re-resolved the mutable role")))
                  ((symbol-function 'mevedel-agent-runtime-dispatch)
                   (lambda (seen-agent _description _message &rest keys)
                     (setq captured-agent seen-agent
                           captured-configuration
                           (plist-get keys :frozen-configuration)
                           captured-tool-use-id
                           (plist-get keys :parent-tool-use-id))
                     (funcall (plist-get keys :on-invocation) turn)
                     t)))
          (mevedel-agent-control--dispatch-followup
           session record "Continue." "call_followup"))
      (kill-buffer buffer))
    (should-not captured-agent)
    (should (eq configuration captured-configuration))
    (should (equal "call_followup" captured-tool-use-id))
    (should (= 1 (length (mevedel-session-agent-registry session))))))

(mevedel-deftest mevedel-agent-control--normalize-context
  ()
  ,test
  (test)
  :doc "normalizes supported context fork modes before reserving a path"
  (progn
    (should (eq 'none (mevedel-agent-control--normalize-context nil)))
    (should (eq 'all (mevedel-agent-control--normalize-context "all")))
    (should (eq 'none (mevedel-agent-control--normalize-context "none")))
    (should (eq 'summary
                (mevedel-agent-control--normalize-context "summary")))
    (should (= 12 (mevedel-agent-control--normalize-context "12")))
    (dolist (value '("" "0" "-1" "+1" "1.0" "ALL" 1))
      (should-error
       (mevedel-agent-control--normalize-context value)
       :type 'user-error))))

(mevedel-deftest mevedel-agent-control-spawn
  (:quiet t
   :before-each (mevedel-tools-register)
   :after-each (mevedel-workspace-clear-registry))
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
          (setq-local gptel-backend
                      (let ((gptel--known-backends nil))
                        (gptel-make-openai
                         "Agent Control" :key "test"
                         :models '(test-model))))
          (setq-local gptel-model 'test-model)
          (cl-letf (((symbol-function 'mevedel-version)
                     (lambda (&rest _) "test"))
                    ((symbol-function 'mevedel-agent-exec-run)
                     (lambda (_callback role _description child
                                        _buffer &optional _configure)
                       (push role roles)
                       (push child invocations)
                       'provider-request)))
            (let (outcome)
              (mevedel-agent-control-spawn
               session "worker" "Implement the change."
               (lambda (value) (setq outcome value)))
              (should-not (plist-get outcome :error))
              (should
               (file-exists-p
                (file-name-concat (mevedel-session-save-path session)
                                  "session.meta.el")))
              (let ((record (plist-get outcome :record)))
              (should (equal "/root/worker"
                             (mevedel-agent-record-path record)))
              (should (eq 'running (mevedel-agent-record-activity record)))
              (should (stringp
                       (mevedel-agent-record-conversation-location record)))))
            (let* ((parent-invocation (car invocations))
                   (mevedel--agent-invocation parent-invocation)
                   outcome)
              (cl-letf (((symbol-function
                          'mevedel-session-persistence-save-agent-state)
                         (lambda (&rest _) t)))
                (mevedel-agent-control-spawn
                 session "research" "Investigate first."
                 (lambda (value) (setq outcome value))
                 :role "explorer"))
              (should-not (plist-get outcome :error))
              (let ((nested (plist-get outcome :record)))
              (should (equal "/root/worker/research"
                             (mevedel-agent-record-path nested)))
              (should (equal "/root/worker"
                             (mevedel-agent-record-parent-path nested)))
              (should (equal "explorer"
                             (mevedel-agent-record-role nested)))
              (should (equal "explorer" (car roles)))
              (let ((nested-invocation
                     (mevedel-agent-record-invocation nested)))
                (cl-letf (((symbol-function
                            'mevedel-session-persistence-save-agent-state)
                           (lambda (&rest _) t)))
                  (mevedel-agent-control-test--settle
                   session nested nested-invocation "Nested result.")))
              (should-not (mevedel-session-messages session))
              (let ((result
                     (car (mevedel-agent-control--mailbox
                           session "/root/worker"))))
                (should (eq 'RESULT (plist-get result :type)))
                (should (equal "/root/worker/research"
                               (plist-get result :sender)))
                (should (equal "/root/worker"
                               (plist-get result :recipient))))))
            (let ((start-count 0)
                  (submit-count 0)
                  outcome)
              (let ((mevedel-subagent-start-functions
                     (list (lambda (_event)
                             (cl-incf start-count)
                             nil)))
                    (mevedel-user-prompt-submit-functions
                     (list (lambda (_event)
                             (cl-incf submit-count)
                             '(:updated-input "Hook-accepted task")))))
                (mevedel-agent-control-spawn
                 session "hooked" "Original task"
                 (lambda (value) (setq outcome value))
                 :result-handler #'ignore))
              (let* ((record (plist-get outcome :record))
                     (invocation (mevedel-agent-record-invocation record)))
                (should (= 1 start-count))
                (should (= 1 submit-count))
                (with-current-buffer
                    (mevedel-agent-invocation-buffer invocation)
                  (should (= 1 (how-many "Hook-accepted task"
                                         (point-min) (point-max)))))
                (mevedel-agent-control-test--settle
                 session record invocation "Hooked result.")))
            (should-error
             (mevedel-agent-control-spawn
              session "unknown" "Fail before reservation."
              #'ignore
              :role "not_a_role")
             :type 'user-error)
            (should-not (assoc "/root/unknown"
                               (mevedel-session-agent-registry session))))
          (let (outcome)
            (cl-letf (((symbol-function
                        'mevedel-session-persistence-save-agent-state)
                       (lambda (&rest _)
                         (error "Injected commit failure"))))
              (mevedel-agent-control-spawn
               session "commit_error" "Fail durable publication."
               (lambda (value) (setq outcome value))))
            (should (eq 'error (plist-get outcome :outcome)))
            (should-not (assoc "/root/commit_error"
                               (mevedel-session-agent-registry session)))
            (should-not (assoc "/root/commit_error"
                               (mevedel-session-agent-reservations session))))
          (let (outcome diagnostics)
            (cl-letf
                (((symbol-function 'mevedel-agent-exec-run)
                  (lambda (_callback _role _description invocation
                                     _buffer &optional _configure)
                    (push invocation invocations)
                    (setf (mevedel-agent-invocation-runtime-fsm invocation)
                          'provider-request)
                    'provider-request))
                 ((symbol-function
                   'mevedel-session-persistence-save-agent-state)
                  (lambda (&rest _) t)))
              (mevedel-test--with-captured-diagnostics diagnostics
                (mevedel-agent-control-spawn
                 session "observer_error" "Keep the committed provider."
                 (lambda (value) (setq outcome value))
                 :result-handler #'ignore
                 :on-invocation
                 (lambda (_invocation)
                   (error "Injected observer failure")))))
            (should-not (plist-get outcome :error))
            (should (eq 'success (plist-get outcome :outcome)))
            (should (string-match-p "Injected observer failure" diagnostics))
            (let* ((record (plist-get outcome :record))
                   (invocation (mevedel-agent-record-invocation record)))
              (should (eq 'running (mevedel-agent-record-activity record)))
              (should (eq 'provider-request
                          (mevedel-agent-invocation-runtime-fsm invocation)))
              (cl-letf (((symbol-function
                          'mevedel-session-persistence-save-agent-state)
                         (lambda (&rest _) t)))
                (mevedel-agent-control-test--settle
                 session record invocation "Observer-safe result."))))
          (let ((mevedel-agents--specs
                 '(("explorer" :description "available"))))
            (should-error
             (mevedel-agent-control-spawn
              session "unavailable" "Reject session-excluded role."
              #'ignore
              :role "worker")
             :type 'user-error)
            (should-not (assoc "/root/unavailable"
                               (mevedel-session-agent-registry session))))
          (let (synchronous-results scheduled
                (publication-fails t))
            (cl-letf (((symbol-function 'mevedel-version)
                       (lambda (&rest _) "test"))
                      ((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (setq scheduled (cons function args))
                         'fake-terminal-retry))
                      ;; A portable session commits its terminal agent
                      ;; state through the publication, local target
                      ;; included.
                      ((symbol-function
                        'mevedel-session-artifacts-publish-agent-terminal-state)
                       (lambda (_invocation)
                         (when publication-fails
                           (error "Injected terminal publication failure"))
                         t))
                      ((symbol-function 'mevedel-agent-exec-run)
                     (lambda (callback _role _description child
                                       buffer &optional _configure)
                       (push child invocations)
                       (let ((adapter
                              (mevedel-agent-exec--make-callback
                               callback "worker" "Complete immediately."
                               (with-current-buffer buffer (point-min-marker))
                               (list nil))))
                         (funcall
                          adapter "Synchronous completion."
                          (list :mevedel-agent-invocation child)))
                       'provider-request)))
              (let (outcome)
                (mevedel-agent-control-spawn
                 session "synchronous" "Complete immediately."
                 (lambda (value) (setq outcome value))
                 :result-handler
                 (lambda (result) (push result synchronous-results)))
                (should-not (plist-get outcome :error))
                (let ((record (plist-get outcome :record)))
                  (should (eq 'running
                              (mevedel-agent-record-activity record)))
                  (should scheduled)
                  (let ((retry scheduled))
                    (setq scheduled nil)
                    (apply (car retry) (cdr retry)))
                  (should (eq 'running
                              (mevedel-agent-record-activity record)))
                  (should scheduled)
                  (setq publication-fails nil)
                  (let ((retry scheduled))
                    (setq scheduled nil)
                    (apply (car retry) (cdr retry)))
                  (should (eq 'idle (mevedel-agent-record-activity record)))
                  (should-not scheduled)
                  (should (= 1 (mevedel-agent-control--active-count session)))
                  (should (= 1 (length synchronous-results)))
                  (should (eq 'completed
                              (plist-get (car synchronous-results) :outcome)))
                  (should-not (mevedel-session-messages session))))))
          (let (runner-called)
            (cl-letf (((symbol-function
                        'mevedel-agent-conversation-save)
                       (lambda (_invocation) nil))
                      ((symbol-function 'mevedel-agent-exec-run)
                       (lambda (&rest _)
                         (setq runner-called t)
                         'provider-request)))
              (let (outcome)
                (mevedel-agent-control-spawn
                 session "ephemeral" "Require a durable transcript."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not runner-called)
              (should-not (assoc "/root/ephemeral"
                                 (mevedel-session-agent-registry session)))))
          (let ((mevedel-subagent-start-functions
                 (list
                  (lambda (_event)
                    (should-not
                     (cl-find
                      "/root/blocked"
                      (mevedel-agent-control-list-agents session)
                      :key (lambda (entry) (plist-get entry :path))
                      :test #'equal))
                    (should-error
                     (mevedel-agent-control-resolve-path
                      session "/root" "/root/blocked")
                     :type 'user-error)
                    '(:continue nil :stop-reason "blocked by policy"))))
                runner-called)
            (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                       (lambda (&rest _)
                         (setq runner-called t)
                         'provider-request)))
              (let (outcome)
                (mevedel-agent-control-spawn
                 session "blocked" "Never publish this agent."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not runner-called)
              (should-not (assoc "/root/blocked"
                                 (mevedel-session-agent-registry session)))
              (should-not (mevedel-session-agent-reservations session))))
          (let (prepare-callback outcome dispatched cancel)
            (setf (mevedel-session-agent-turn-capacity session) 2)
            (cl-letf
                (((symbol-function 'mevedel-agent-runtime-prepare-task)
                  (lambda (_agent _description _prompt _path callback
                           &rest _)
                    (setq prepare-callback callback)))
                 ((symbol-function 'mevedel-agent-runtime-dispatch)
                  (lambda (&rest _)
                    (setq dispatched t))))
              (setq cancel
                    (mevedel-agent-control-spawn
                     session "delayed" "Prepare later."
                     (lambda (value) (setq outcome value))))
              (should (assoc "/root/delayed"
                             (mevedel-session-agent-reservations session)))
              (should-not
               (cl-find "/root/delayed"
                        (mevedel-agent-control-list-agents session)
                        :key (lambda (entry) (plist-get entry :path))
                        :test #'equal))
              (should-error
               (mevedel-agent-control-spawn
                session "delayed" "Duplicate." #'ignore))
              (should-error
               (mevedel-agent-control-spawn
                session "at_capacity" "No slot." #'ignore))
              (funcall cancel)
              (should (eq 'aborted (plist-get outcome :outcome)))
              (should-not (mevedel-session-agent-reservations session))
              (funcall prepare-callback
                       '(:outcome success :turn (:prompt "Too late")))
              (should-not dispatched))
            (setf (mevedel-session-agent-turn-capacity session) 3))
          (let ((mevedel-subagent-stop-functions
                 (list (lambda (_event) (push 'stop roles)))))
            (cl-letf (((symbol-function 'mevedel-agent-exec-run)
                       (lambda (&rest _) nil)))
              (let (outcome)
                (mevedel-agent-control-spawn
                 session "launch_error" "Fail before publication."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not
               (assoc "/root/launch_error"
                      (mevedel-session-agent-registry session)))
              (should-not (memq 'stop roles)))))
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
      (delete-directory root t)))

  :doc "generates one unpublished task-focused summary before child dispatch"
  (let* ((session (mevedel-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary*"))
         (parent-invocation
          (mevedel-agent-invocation-create (mevedel-agent-default)))
         (parent-record
          (mevedel-agent-record--create
           :id "default--parent" :path "/root/parent" :activity 'running))
         summary-callback summary-cancelled captured-source captured-focus
         provider-callbacks (summary-calls 0) outcome cancel)
    (setf (mevedel-agent-invocation-agent-id parent-invocation)
          "default--parent"
          (mevedel-agent-invocation-path parent-invocation) "/root/parent"
          (mevedel-session-agent-registry session)
          (list (cons "/root/parent" parent-record)))
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session
                      mevedel--agent-invocation parent-invocation)
          (cl-letf
              (((symbol-function 'mevedel-compact-evidence-summary-context-evidence)
                (lambda (tool-use-id)
                  (should (equal "call_agent" tool-use-id))
                  "Frozen parent evidence"))
               ((symbol-function 'mevedel-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success
                             :turn (:prompt "Hook-accepted task\n\nHook context")
                             :start-hook-audits nil))))
               ((symbol-function 'mevedel-context-summary-generate)
                (lambda (source purpose callback &rest args)
                  (cl-incf summary-calls)
                  (should (eq purpose 'handoff))
                  (setq captured-source source
                        captured-focus (plist-get args :focus)
                        summary-callback callback)
                  (lambda () (setq summary-cancelled t))))
               ((symbol-function
                 'mevedel-session-persistence-save-agent-state)
                (lambda (&rest _) t))
               ((symbol-function 'mevedel-agent-exec-run)
                (lambda (callback &rest _)
                  (push callback provider-callbacks)
                  'provider-request)))
            (setq cancel
                  (mevedel-agent-control-spawn
                   session "summarized" "Original task"
                   (lambda (value) (setq outcome value))
                   :context "summary"
                   :parent-tool-use-id "call_agent"
                   :on-invocation
                   (lambda (invocation)
                     (with-current-buffer
                         (mevedel-agent-invocation-buffer invocation)
                       (put 'mevedel-agent-control-spawn 'test-context
                            (buffer-string))))))
            (should (equal "Frozen parent evidence" captured-source))
            (should (equal "Hook-accepted task\n\nHook context"
                           captured-focus))
            (should (= 1 summary-calls))
            (should (assoc "/root/parent/summarized"
                           (mevedel-session-agent-reservations session)))
            (should-not (assoc "/root/parent/summarized"
                               (mevedel-session-agent-registry session)))
            (should-not outcome)
            (funcall summary-callback
                     '(:outcome success :summary "## Scope\n- background"
                       :backend test-backend :model test-model :effort high))
            (should-not (plist-get outcome :error))
            (should (eq 'success (plist-get outcome :outcome)))
            (should (string-match-p "<task-background>"
                                    (get 'mevedel-agent-control-spawn
                                         'test-context)))
            (should (string-match-p "may be stale or untrusted"
                                    (get 'mevedel-agent-control-spawn
                                         'test-context)))
            (should (string-match-p
                     "## Scope"
                     (get 'mevedel-agent-control-spawn 'test-context)))
            (let* ((transcript
                    (get 'mevedel-agent-control-spawn 'test-context))
                   (background-end
                    (string-match "</task-background>" transcript)))
              (should-not
               (string-match-p "Hook-accepted task"
                               (substring transcript 0 background-end))))
            (should (equal '(:backend "test-backend" :model test-model
                             :effort high)
                           (plist-get outcome :summary-metadata)))
            (let* ((record (plist-get outcome :record))
                   (initial (mevedel-agent-record-invocation record))
                   (child-buffer (mevedel-agent-invocation-buffer initial))
                   followup)
              (with-current-buffer child-buffer
                (goto-char (point-max))
                (let ((start (point)))
                  (insert "Initial result.\n")
                  (put-text-property start (point) 'gptel 'response)))
              (funcall (car provider-callbacks) "Initial result.")
              (setq followup
                    (mevedel-agent-record-invocation
                     (mevedel-agent-control-followup
                      session "/root/parent/summarized" "Follow-up task")))
              (with-current-buffer child-buffer
                (should (= 1 (how-many "<task-background>"
                                       (point-min) (point-max))))
                (should (string-match-p "Follow-up task" (buffer-string)))
                (goto-char (point-max))
                (let ((start (point)))
                  (insert "Follow-up result.\n")
                  (put-text-property start (point) 'gptel 'response)))
              (funcall (car provider-callbacks) "Follow-up result.")
              (with-current-buffer child-buffer
                (basic-save-buffer)
                (let ((target
                       (mevedel-compact-target-agent-target followup)))
                  (should (string-match-p
                           "task-background"
                           (plist-get
                            (mevedel-compact-evidence-select
                             target (point-max) t)
                            :content)))
                  (mevedel-compact-target-call
                   target :apply "Child continuation summary." nil nil nil)
                  (should-not (string-match-p "<task-background>"
                                              (buffer-string)))
                  (should (string-match-p "Child continuation summary"
                                          (buffer-string))))))
            (funcall cancel)
            (should-not summary-cancelled)))
      (put 'mevedel-agent-control-spawn 'test-context nil)
      (when-let* ((record (plist-get outcome :record))
                  (invocation (mevedel-agent-record-invocation record))
                  (buffer (mevedel-agent-invocation-buffer invocation))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "summary cancellation releases capacity and suppresses late dispatch"
  (let* ((session (mevedel-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary-cancel*"))
         summary-callback (cancelled (list nil)) dispatched outcome cancel)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function 'mevedel-compact-evidence-summary-context-evidence)
                (lambda (_) "Frozen evidence"))
               ((symbol-function 'mevedel-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success :turn (:prompt "Final task")))))
               ((symbol-function 'mevedel-context-summary-generate)
                (lambda (_source _purpose callback &rest _)
                  (setq summary-callback callback)
                  (lambda () (setcar cancelled t))))
               ((symbol-function 'mevedel-agent-runtime-dispatch)
                (lambda (&rest _) (setq dispatched t))))
            (setq cancel
                  (mevedel-agent-control-spawn
                   session "cancelled_summary" "Task"
                   (lambda (value) (setq outcome value))
                   :context "summary"))
            (should-not outcome)
            (funcall cancel)
            (should (car cancelled))
            (should (eq 'aborted (plist-get outcome :outcome)))
            (should-not (mevedel-session-agent-reservations session))
            (funcall summary-callback
                     '(:outcome success :summary "Late summary"))
            (should-not dispatched)))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "summary generation failure releases the unpublished reservation"
  (let* ((session (mevedel-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary-error*"))
         dispatched outcome)
    (unwind-protect
        (with-current-buffer parent
          (setq-local mevedel--session session)
          (cl-letf
              (((symbol-function 'mevedel-compact-evidence-summary-context-evidence)
                (lambda (_) "Frozen evidence"))
               ((symbol-function 'mevedel-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success :turn (:prompt "Final task")))))
               ((symbol-function 'mevedel-context-summary-generate)
                (lambda (_source _purpose callback &rest _)
                  (funcall callback
                           '(:outcome error :error "Invalid summary"
                             :error-class validation))
                  #'ignore))
               ((symbol-function 'mevedel-agent-runtime-dispatch)
                (lambda (&rest _) (setq dispatched t))))
            (mevedel-agent-control-spawn
             session "failed_summary" "Task"
             (lambda (value) (setq outcome value))
             :context "summary")
            (should (eq 'error (plist-get outcome :outcome)))
            (should (string-match-p "Invalid summary"
                                    (plist-get outcome :error)))
            (should-not dispatched)
            (should-not (mevedel-session-agent-reservations session))))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "reserves parallel summaries per tree without a global lock"
  (let* ((first (mevedel-agent-control-test--session))
         (second (mevedel-agent-control-test--session))
         (first-buffer (generate-new-buffer " *summary-capacity-first*"))
         (second-buffer (generate-new-buffer " *summary-capacity-second*"))
         callbacks cancels)
    (setf (mevedel-session-agent-turn-capacity first) 1
          (mevedel-session-agent-turn-capacity second) 1)
    (unwind-protect
        (progn
          (cl-letf
            (((symbol-function 'mevedel-compact-evidence-summary-context-evidence)
              (lambda (_) "Frozen evidence"))
             ((symbol-function 'mevedel-agent-runtime-prepare-task)
              (lambda (_agent _description _message _path callback &rest _)
                (funcall callback
                         '(:outcome success :turn (:prompt "Final task")))))
             ((symbol-function 'mevedel-context-summary-generate)
              (lambda (_source _purpose callback &rest _)
                (push callback callbacks)
                #'ignore)))
          (with-current-buffer first-buffer
            (setq-local mevedel--session first)
            (push (mevedel-agent-control-spawn
                   first "one" "Task" #'ignore :context "summary")
                  cancels)
            (should-error
             (mevedel-agent-control-spawn
              first "two" "Task" #'ignore :context "summary")
             :type 'user-error))
          (with-current-buffer second-buffer
            (setq-local mevedel--session second)
            (push (mevedel-agent-control-spawn
                   second "one" "Task" #'ignore :context "summary")
                  cancels))
          (should (= 2 (length callbacks)))
          (should (= 1 (length (mevedel-session-agent-reservations first))))
          (should (= 1 (length
                        (mevedel-session-agent-reservations second))))))
      (mapc #'funcall cancels)
      (kill-buffer first-buffer)
      (kill-buffer second-buffer))))

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
        session (car args) (cadr args)))))

  :doc "commit failure restores the queue and does not wake its consumer"
  (let* ((session (mevedel-agent-control-test--session))
         (record (mevedel-agent-record--create
                  :id "worker" :path "/root/worker" :activity 'idle))
         reasons)
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record))
          (mevedel-session-save-path session) temporary-file-directory
          (mevedel-agent-record-waiter record)
          (mevedel-agent-waiter--create
           :callback (lambda (reason) (push reason reasons))))
    (dolist (failure '(nil error))
      (cl-letf (((symbol-function
                  'mevedel-session-persistence-save-agent-state)
                 (if (eq failure 'error)
                     (lambda (&rest _) (error "Injected commit failure"))
                   (lambda (&rest _) nil))))
        (should-error
         (mevedel-agent-control-send-message
          session "/root/worker" "must persist")))
      (should-not (mevedel-agent-record-mailbox record))
      (should-not reasons)
      (should (mevedel-agent-record-waiter record)))))

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

  :doc "retains the waiter when its callback fails after durable enqueue"
  (let ((session (mevedel-agent-control-test--session))
        (attempts 0))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (mevedel-agent-control-wait
       session
       (lambda (_reason)
         (cl-incf attempts)
         (when (= attempts 1)
           (error "Injected delivery failure")))
       10000)
      (should-error
       (mevedel-agent-control-send-message session "/root" "arrived"))
      (should (= 1 attempts))
      (should (mevedel-session-agent-root-waiter session))
      (should (= 1 (length (mevedel-agent-control--mailbox session "/root"))))
      (should (mevedel-agent-control--wake session "/root" 'mailbox))
      (should (= 2 attempts))
      (should-not (mevedel-session-agent-root-waiter session))))

  :doc "times out and clamps or defaults out-of-range timeouts"
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
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seen-delay &rest _)
                 (setq delay seen-delay)
                 'fake-timer)))
      (mevedel-agent-control-wait session #'ignore 1)
      (should (= 10 delay))
      (mevedel-agent-control-cancel-wait session "/root")
      ;; Out-of-range values clamp, coercible values coerce, and
      ;; garbage falls back to the default instead of erroring.
      (dolist (case '((0 . 10) (-1 . 10) (3600001 . 3600) (10000.0 . 10)
                      ("10000" . 10) ("nonsense" . 30) (t . 30)))
        (mevedel-agent-control-wait session #'ignore (car case))
        (should (= (cdr case) delay))
        (mevedel-agent-control-cancel-wait session "/root")))
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
      (mevedel-agent-control-enqueue-execution-result
       session "/root" "root execution complete")
      (should (eq 'mailbox reason))))

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
      (should (eq 'waiting (mevedel-agent-record-activity record)))
      (should (= 1 (mevedel-agent-control--active-count session)))
      (mevedel-agent-control-send-message session "/root/worker" "wake")
      (should (eq 'mailbox reason))
      (should (eq 'running (mevedel-agent-record-activity record)))
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
               (lambda (&rest _) 'fake-timer)))
      (let ((mevedel--agent-invocation invocation))
        (mevedel-agent-control-wait
         session (lambda (seen) (setq reason seen)) 10000))
      (mevedel-agent-control-followup
       session "/root/steered" "new direction")
      (should (eq 'steering reason))
      (should (eq 'running (mevedel-agent-record-activity record))))))

(mevedel-deftest mevedel-agent-control-enqueue-execution-result ()
  ,test
  (test)
  :doc "queues a distinct root execution record and acknowledges delivery"
  (let ((session (mevedel-agent-control-test--session)))
    (should
     (mevedel-agent-control-enqueue-execution-result
      session "/root" "root execution complete"))
    (let ((message (car (mevedel-agent-control-context-mailbox session))))
      (should (eq 'EXECUTION (plist-get message :type)))
      (should (equal "/root" (plist-get message :sender)))
      (should (equal "root execution complete"
                     (plist-get message :payload))))))

(mevedel-deftest mevedel-agent-control-wake-root-user ()
  ,test
  (test)
  :doc "wakes WaitAgent without creating a mailbox message"
  (let ((session (mevedel-agent-control-test--session))
        reason)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (mevedel-agent-control-wait
       session (lambda (seen) (setq reason seen)) 10000)
      (mevedel-agent-control-wake-root-user session))
    (should (eq 'user reason))
    (should-not (mevedel-agent-control--mailbox session "/root"))))

(provide 'test-mevedel-agent-control)

;;; test-mevedel-agent-control.el ends here
