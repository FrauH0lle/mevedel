;;; test-niwa-agent-control.el --- Durable agent control tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the session-owned agent registry's validation, admission, rollback,
;; settlement, result bounding, and provider-dispatch boundary.

;;; Code:

(require 'niwa-agent-control)
(require 'niwa-agent-conversation)
(require 'niwa-agent-exec)
(require 'niwa-agent-persistence)
(require 'niwa-agent-runtime)
(require 'niwa-agents)
(require 'niwa-compact-evidence)
(require 'niwa-compact-target)
(require 'niwa-pending-inputs)
(require 'niwa-session-artifacts)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-tools)
(require 'niwa-transport)
(require 'niwa-workspace)
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

;; `niwa-session-recovery'
(defvar niwa-session-recovery--mutation-cache)

(defun niwa-agent-control-test--configuration ()
  "Return a frozen configuration for stubbed retained dispatches."
  (niwa-agent-configuration--create
   :agent (niwa-agent--create :name "default" :frozen-p t)))

(defun niwa-agent-control-test--session ()
  "Return a fresh in-memory session for agent-control tests."
  (let ((session
         (niwa-session-create
          "main"
          (niwa-workspace--create
           :type 'file
           :id "agent-control"
           :root (niwa-test--placeholder-root)
           :name "agent-control"))))
    ;; File-workspace fixtures have no portable lease or root conversation.
    ;; Portable authority and target replacement run in persistence suites.
    session))

(defun niwa-agent-control-test--stage-settle (&rest args)
  "Stage a settlement with ARGS; in-memory fixtures have no lease to admit."
  (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
             #'ignore))
    (apply #'niwa-agent-control--settle args)))

(defun niwa-agent-control-test--settle
    (session record invocation response &optional event)
  "Settle one test turn through the runtime-owned commit boundary."
  (when-let* ((transaction
               (niwa-agent-control-test--stage-settle
                session record invocation response event)))
    (condition-case err
        (niwa-agent-control-commit-session session)
      (error
       (funcall (car transaction))
       (signal (car err) (cdr err))))
    (funcall (cdr transaction))
    (niwa-agent-control-settlement-finished session)
    transaction))

(niwa-deftest niwa-agent-control-active-activity-p ()
  ,test
  (test)
  :doc "recognizes every activity and record that owns a capacity slot"
  (dolist (case '((starting . t) (running . t) (waiting . t)
                  (permission-blocked . t) (interaction-blocked . t)
                  (idle)))
    (should (eq (cdr case)
                (and (niwa-agent-control-active-activity-p (car case))
                     t)))
    (should (eq (cdr case)
                (and (niwa-agent-control--active-p
                      (niwa-agent-record--create :activity (car case)))
                     t)))))

(niwa-deftest niwa-agent-control--active-count ()
  ,test
  (test)
  :doc "counts published active turns and unpublished reservations"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/a"
                 (niwa-agent-record--create :activity 'starting))
           (cons "/root/b"
                 (niwa-agent-record--create :activity 'running))
           (cons "/root/c"
                 (niwa-agent-record--create :activity 'idle))))
    (setf (niwa-session-agent-reservations session)
          (list (cons "/root/d"
                      (niwa-agent-record--create :activity 'starting))))
    (should (= 3 (niwa-agent-control--active-count session)))))

(niwa-deftest niwa-agent-control-active-turn-p ()
  ,test
  (test)
  :doc "reports whether any active agent turn owns capacity"
  (let ((session (niwa-agent-control-test--session)))
    (should-not (niwa-agent-control-active-turn-p session))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/idle"
                 (niwa-agent-record--create :activity 'idle))))
    (should-not (niwa-agent-control-active-turn-p session))
    (setf (niwa-agent-record-activity
           (cdar (niwa-session-agent-registry session)))
          'permission-blocked)
    (should (niwa-agent-control-active-turn-p session))))

(niwa-deftest niwa-agent-control-block-turn ()
  ,test
  (test)
  :doc "composes overlapping blockers and ignores stale or duplicate releases"
  (let* ((session (niwa-agent-control-test--session))
         (first (niwa-agent-invocation--create :path "/root/worker"))
         (second (niwa-agent-invocation--create :path "/root/worker"))
         (record
          (niwa-agent-record--create
           :activity 'running :invocation first))
         release-wait release-permission stale-release)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (let ((niwa-agent-control-suppress-persistence t))
      (setq release-wait
            (niwa-agent-control-block-turn
             session "/root/worker" 'waiting)
            release-permission
            (niwa-agent-control-block-turn
             session "/root/worker" 'permission-blocked))
      (should (eq 'permission-blocked
                  (niwa-agent-record-activity record)))
      (should (= 2 (length (niwa-agent-record-blockers record))))
      (funcall release-wait)
      (funcall release-wait)
      (should (eq 'permission-blocked
                  (niwa-agent-record-activity record)))
      (funcall release-permission)
      (should (eq 'running (niwa-agent-record-activity record)))
      (should-not (niwa-agent-record-blockers record))
      (setq stale-release
            (niwa-agent-control-block-turn
             session "/root/worker" 'interaction-blocked))
      (setf (niwa-agent-record-invocation record) second
            (niwa-agent-record-blockers record) nil
            (niwa-agent-record-activity record) 'running)
      (funcall stale-release)
      (should (eq 'running (niwa-agent-record-activity record)))
      (should-not
       (niwa-agent-control-block-turn session "/root" 'waiting))
      (should-error
       (niwa-agent-control-block-turn
        session "/root/worker" 'invalid)))))

(niwa-deftest niwa-agent-control-retained-buffer-p ()
  ,test
  (test)
  :doc "recognizes only live conversation buffers owned by the session registry"
  (let ((session (niwa-agent-control-test--session))
        (buffer (generate-new-buffer " *agent-control-retained*")))
    (unwind-protect
        (progn
          (should-not
           (niwa-agent-control-retained-buffer-p session buffer))
          (setf (niwa-session-agent-registry session)
                (list
                 (cons "/root/worker"
                       (niwa-agent-record--create
                        :path "/root/worker"
                        :conversation-buffer buffer))))
          (should
           (niwa-agent-control-retained-buffer-p session buffer)))
      (kill-buffer buffer))
    (should-not
     (niwa-agent-control-retained-buffer-p session buffer))))

(niwa-deftest niwa-agent-control-teardown-session (:quiet t)
  ,test
  (test)
  :doc "kills all registered conversation buffers without requiring open views"
  (let ((session (niwa-agent-control-test--session))
        (first (generate-new-buffer " *agent-control-teardown-one*"))
        (second (generate-new-buffer " *agent-control-teardown-two*")))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/one"
                 (niwa-agent-record--create
                  :path "/root/one" :conversation-buffer first))
           (cons "/root/two"
                 (niwa-agent-record--create
                  :path "/root/two" :conversation-buffer second))))
    (unwind-protect
        (progn
          (niwa-agent-control-teardown-session session)
          (should-not (buffer-live-p first))
          (should-not (buffer-live-p second)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))))

  :doc "flushes modified retained transcripts before killing their buffers"
  (let* ((root (file-name-as-directory
                (make-temp-file "agent-control-teardown-save-" t)))
         (relative "agents/worker.chat.org")
         (absolute (expand-file-name relative root))
         (session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         buffer)
    (make-directory (file-name-directory absolute) t)
    (write-region "previous\n" nil absolute nil 'silent)
    (setq buffer (find-file-noselect absolute))
    (setf (niwa-session-save-path session) root)
    (setf (niwa-agent-invocation-agent-id invocation) "default--worker")
    (setf (niwa-agent-invocation-path invocation) "/root/worker")
    (setf (niwa-agent-invocation-buffer invocation) buffer)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (setf (niwa-agent-invocation-transcript-relative-path invocation)
          relative)
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (niwa-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer buffer))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation invocation)
            (goto-char (point-max))
            (insert "latest\n"))
          (niwa-agent-control-teardown-session session)
          (should-not (buffer-live-p buffer))
          (with-temp-buffer
            (insert (niwa-session-artifacts-read-artifact
                     session relative))
            (should (equal "previous\nlatest\n" (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)))

  :doc "keeps a modified retained buffer alive when its flush cannot persist"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (buffer (generate-new-buffer " *agent-control-unsaved*")))
    (setf (niwa-agent-invocation-buffer invocation) buffer)
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/worker"
                 (niwa-agent-record--create
                  :path "/root/worker"
                  :conversation-buffer buffer))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation invocation)
            (insert "unsaved"))
          (let ((warning-minimum-level :error))
            (niwa-agent-control-teardown-session session))
          (should (buffer-live-p buffer))
          (should (buffer-modified-p buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(niwa-deftest niwa-agent-control-current-path ()
  ,test
  (test)
  :doc "returns known caller paths and rejects unregistered children"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (record (niwa-agent-record--create
                  :id "default--opaque"
                  :path "/root/worker"
                  :activity 'running))
         (buffer (generate-new-buffer " *agent-control-current*")))
    (setf (niwa-agent-invocation-agent-id invocation) "default--opaque")
    (setf (niwa-agent-invocation-path invocation) "/root/worker")
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (progn
          (should (equal "/root"
                         (niwa-agent-control-current-path session)))
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation invocation)
            (should (equal "/root/worker"
                           (niwa-agent-control-current-path session)))))
      (kill-buffer buffer))
    (setq invocation (niwa-agent-invocation-create
                      (niwa-agent-default))
          buffer (generate-new-buffer " *agent-control-unregistered*"))
    (setf (niwa-agent-invocation-agent-id invocation) "default--missing")
    (setf (niwa-agent-invocation-path invocation) "/root/missing")
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--agent-invocation invocation)
          (should-error (niwa-agent-control-current-path session)
                        :type 'error))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-control-resolve-path ()
  ,test
  (test)
  :doc "resolves canonical and relative descendants without accepting IDs"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/alpha"
                 (niwa-agent-record--create :path "/root/alpha"))
           (cons "/root/alpha/child"
                 (niwa-agent-record--create
                  :path "/root/alpha/child"))))
    (should (equal "/root/alpha"
                   (niwa-agent-control-resolve-path
                    session "/root" "alpha")))
    (should (equal "/root/alpha/child"
                   (niwa-agent-control-resolve-path
                    session "/root/alpha" "child")))
    (should (equal "/root"
                   (niwa-agent-control-resolve-path
                    session "/root/alpha" "/root")))
    (dolist (target '("../alpha" "./alpha" "alpha//child"
                      "default--opaque" "/root/unknown" ""))
      (should-error
       (niwa-agent-control-resolve-path session "/root" target)))))

(niwa-deftest niwa-agent-control-list-agents ()
  ,test
  (test)
  :doc "lists path-sorted roster entries with exact activity and subtree filtering"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/zeta"
                 (niwa-agent-record--create
                  :id "private-z" :path "/root/zeta"
                  :role "worker" :activity 'idle
                  :conversation-location "agents/zeta.chat.org"))
           (cons "/root/alpha/child"
                 (niwa-agent-record--create
                  :id "private-c" :path "/root/alpha/child"
                  :role "explorer" :activity 'starting))
           (cons "/root/alpha"
                 (niwa-agent-record--create
                  :id "private-a" :path "/root/alpha"
                  :role "default" :activity 'permission-blocked))))
    (should
     (equal '((:path "/root" :role "default" :activity "idle")
              (:path "/root/alpha" :role "default"
               :activity "permission-blocked")
              (:path "/root/alpha/child" :role "explorer" :activity "starting")
              (:path "/root/zeta" :role "worker" :activity "idle"))
            (niwa-agent-control-list-agents session)))
    (setf (niwa-session-agent-root-activity session) 'running)
    (should (equal "running"
                   (plist-get
                    (car (niwa-agent-control-list-agents session))
                    :activity)))
    (should
     (equal '((:path "/root/alpha" :role "default"
               :activity "permission-blocked")
              (:path "/root/alpha/child" :role "explorer" :activity "starting"))
            (niwa-agent-control-list-agents session "/root/alpha")))
    (should-not
     (niwa-agent-control-list-agents session "/root/missing"))))

(niwa-deftest niwa-agent-control-commit-session ()
  ,test
  (test)
  :doc "requires a successful materialized persistence commit"
  (let ((session (niwa-agent-control-test--session))
        seen)
    (setf (niwa-session-save-path session) temporary-file-directory)
    (cl-letf (((symbol-function
                'niwa-session-persistence-save-agent-state)
               (lambda (value &optional _token) (setq seen value) t)))
      (should (niwa-agent-control-commit-session session)))
    (should (eq session seen))
    (cl-letf (((symbol-function
                'niwa-session-persistence-save-agent-state)
               (lambda (_value &optional _token) nil)))
      (should-error (niwa-agent-control-commit-session session))))
  :doc "accepts unmaterialized or explicitly staged state"
  (let ((session (niwa-agent-control-test--session)))
    (should (niwa-agent-control-commit-session session))
    (setf (niwa-session-save-path session) temporary-file-directory)
    (let ((niwa-agent-control-suppress-persistence t))
      (should (niwa-agent-control-commit-session session)))))

(niwa-deftest niwa-agent-control--persist-session ()
  ,test
  (test)
  :doc "debounces observational persists instead of committing each"
  (let ((session (niwa-agent-control-test--session))
        committed)
    (setf (niwa-session-save-path session) temporary-file-directory)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-session-persistence-save-agent-state)
                   (lambda (_session) (setq committed t))))
          (niwa-agent-control--persist-session session)
          (should-not committed)
          (should (gethash
                   session
                   niwa-session-persistence--deferred-agent-saves))
          ;; Repeated transitions coalesce into the one pending save.
          (niwa-agent-control--persist-session session)
          (should (timerp
                   (gethash
                    session
                    niwa-session-persistence--deferred-agent-saves))))
      (niwa-session-persistence-cancel-deferred-agent-save session))
    (should-not (gethash
                 session
                 niwa-session-persistence--deferred-agent-saves)))
  :doc "keeps observational persistence best effort"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-save-path session) temporary-file-directory)
    (cl-letf (((symbol-function
                'niwa-session-persistence-save-agent-state-soon)
               (lambda (_session) (error "Scheduling unavailable"))))
      (should-not (niwa-agent-control--persist-session session))))
  :doc "does nothing before session materialization or while suppressed"
  (let ((session (niwa-agent-control-test--session)))
    (should-not (niwa-agent-control--persist-session session))
    (setf (niwa-session-save-path session) temporary-file-directory)
    (let ((niwa-agent-control-suppress-persistence t))
      (should-not (niwa-agent-control--persist-session session))
      (should-not (gethash
                   session
                   niwa-session-persistence--deferred-agent-saves)))))


(niwa-deftest niwa-agent-control-clear-context-mailbox ()
  ,test
  (test)
  :doc "persists only when clearing a populated root mailbox"
  (let ((session (niwa-agent-control-test--session))
        (persisted 0))
    (cl-letf (((symbol-function 'niwa-agent-control--persist-session)
               (lambda (_session) (cl-incf persisted))))
      (niwa-agent-control-clear-context-mailbox session)
      (should (= 0 persisted))
      (niwa-session--set-messages session (list (list :id "m1")))
      (niwa-agent-control-clear-context-mailbox session)
      (should-not (niwa-session-messages session))
      (should (= 1 persisted))
      (niwa-agent-control-clear-context-mailbox session)
      (should (= 1 persisted)))))

(niwa-deftest niwa-agent-control-recover-interrupted ()
  ,test
  (test)
  :doc "idles every active shape and enqueues one ordinary parent RESULT"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (cl-loop for activity in niwa-agent-control--active-activities
                   for index from 1
                   for path = (format "/root/task%d" index)
                   collect
                   (cons path
                         (niwa-agent-record--create
                          :path path :parent-path "/root"
                          :activity activity))))
    (should (= 5 (niwa-agent-control-recover-interrupted session)))
    (should (cl-every
             (lambda (entry)
               (eq 'idle (niwa-agent-record-activity (cdr entry))))
             (niwa-session-agent-registry session)))
    (should (= 5 (length (niwa-session-messages session))))
    (should (cl-every
             (lambda (message)
               (let ((record (cdr (assoc (plist-get message :sender)
                                         (niwa-session-agent-registry
                                          session)))))
                 (and (eq 'RESULT (plist-get message :type))
                      (eq 'interrupted (plist-get message :outcome))
                      (equal (niwa-agent-record-settled-result record)
                             (plist-get message :payload)))))
             (niwa-session-messages session)))
    (should (cl-every
             (lambda (entry)
               (let ((record (cdr entry)))
                 (and (eq 'interrupted
                          (niwa-agent-record-settled-outcome record))
                      (stringp (niwa-agent-record-settled-result record)))))
             (niwa-session-agent-registry session)))
    (should (zerop (niwa-agent-control-recover-interrupted session)))
    (should (= 5 (length (niwa-session-messages session)))))

  :doc "includes useful restored partial output and transcript location"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-recovery-partial*"))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (record
          (niwa-agent-record--create
           :path "/root/partial" :parent-path "/root" :activity 'running
           :conversation-buffer buffer
           :conversation-location "agents/partial.chat.org")))
    (unwind-protect
        (progn
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/partial" record)))
          (setf (niwa-agent-invocation-buffer invocation) buffer)
          (with-current-buffer buffer
            (setq-local niwa--agent-invocation invocation)
            (insert (propertize "Useful restored work." 'gptel 'response)))
          (should (= 1 (niwa-agent-control-recover-interrupted session)))
          (let ((payload (niwa-agent-record-settled-result record)))
            (should (string-match-p "Useful restored work" payload))
            (should (string-match-p "agents/partial.chat.org" payload))))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-control--validate-spawn ()
  ,test
  (test)
  :doc "accepts the minimal spawn contract and rejects malformed values"
  (let ((session (niwa-agent-control-test--session)))
    (dolist (task-name '("task_2" "switch-buffer-error" "task-2_child" "-"
                        "" "Upper" "two/parts" "root" "." ".."
                        "two words" "task.name" "task\\name" "caf\u00e9"))
      (should-not
       (niwa-agent-control--validate-spawn session task-name "Do work.")))
    (dolist (task-name '(nil 42 ("task")))
      (let ((err (should-error
                  (niwa-agent-control--validate-spawn
                   session task-name "Do work.")
                  :type 'user-error)))
        (should (equal "Agent task_name must be plain text"
                       (cadr err)))))
    (dolist (message '(nil ""))
      (should-error
       (niwa-agent-control--validate-spawn session "task" message)))))

(niwa-deftest niwa-agent-control--task-segment ()
  ,test
  (test)
  :doc "derives safe segments from display names, path attempts, and unusable text"
  (dolist (pair '(("Review code-block copy" . "review-code-block-copy")
                  ("task_2" . "task_2")
                  ("Task-2_child" . "task-2_child")
                  ("two/parts" . "two-parts")
                  ("../../escape\\name" . "escape-name")
                  ("  task.name!  " . "task-name")
                  ("caf\u00e9" . "caf")
                  ("ROOT" . "agent-root")
                  ("root" . "agent-root")
                  ("" . "agent")
                  ("  \n\t" . "agent")
                  ("..." . "agent")
                  ("-" . "agent")
                  ("\u65e5\u672c\u8a9e" . "agent")))
    (let ((segment (niwa-agent-control--task-segment (car pair))))
      (should (equal (cdr pair) segment))
      (should (niwa-agent-path-p (concat "/root/" segment)))
      (should-not (equal "root" segment))))
  :doc "caps long bases at 40 characters, preferring hyphen boundaries"
  (dolist (pair (list
                (cons (make-string 41 ?a) (make-string 40 ?a))
                (cons (make-string 40 ?a) (make-string 40 ?a))
                (cons (concat (make-string 40 ?a) "-next") (make-string 40 ?a))
                (cons (concat (make-string 38 ?a) "--next") (make-string 38 ?a))
                (cons "Review code-block copy behavior across all rendered transcripts"
                      "review-code-block-copy-behavior-across")
                (cons (concat "root-" (make-string 50 ?a)) "agent-root")))
    (let ((segment (niwa-agent-control--task-segment (car pair))))
      (should (equal (cdr pair) segment))
      (should (<= (length segment) 40))
      (should-not (string-suffix-p "-" segment))
      (should (niwa-agent-path-p (concat "/root/" segment))))))

(niwa-deftest niwa-agent-control--reserve ()
  ,test
  (test)
  :doc "reserves paths and capacity while retaining idle identities"
  (let* ((session (niwa-agent-control-test--session))
         (record nil))
    (setf (niwa-session-agent-turn-capacity session) 1)
    (setq record (niwa-agent-control--reserve
                  session "/root" "first" "default"))
    (should (equal "/root/first" (niwa-agent-record-path record)))
    (should-not (niwa-session-agent-registry session))
    (should (eq record
                (cdar (niwa-session-agent-reservations session))))
    (should-error (niwa-agent-control--reserve
                   session "/root" "first" "default"))
    (should-error (niwa-agent-control--reserve
                   session "/root" "second" "worker"))
    (setf (niwa-agent-record-activity record) 'idle)
    (setf (niwa-session-agent-reservations session) nil
          (niwa-session-agent-registry session)
          (list (cons "/root/first" record)))
    (let ((child
           (niwa-agent-control--reserve
            session "/root/first" "second" "worker")))
      (should (equal "/root/first/second"
                     (niwa-agent-record-path child)))
      (should (equal "/root/first"
                     (niwa-agent-record-parent-path child)))
      (should (equal "worker" (niwa-agent-record-role child)))))
  :doc "drains publication before reserving a child path or capacity"
  (let ((session (niwa-agent-control-test--session)) drained)
    (cl-letf (((symbol-function 'niwa-session-publication-barrier)
               (lambda (actual &optional _token)
                 (should (eq actual session))
                 (should-not (niwa-session-agent-reservations session))
                 (setq drained t))))
      (niwa-agent-control--reserve session "/root" "first" "default"))
    (should drained)
    (should (assoc "/root/first" (niwa-session-agent-reservations session))))
  :doc "suffixes retained and preparing collisions without reusing identities"
  (let* ((session (niwa-agent-control-test--session))
         (retained (niwa-agent-record--create
                    :path "/root/review-copy" :parent-path "/root"
                    :activity 'idle)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/review-copy" retained)))
    (dolist (pair '(("Review Copy" . "/root/review-copy-2")
                    ("review/copy" . "/root/review-copy-3")
                    ("review-copy-2" . "/root/review-copy-2-2")
                    ("root" . "/root/agent-root")
                    ("ROOT" . "/root/agent-root-2")
                    ("" . "/root/agent")
                    ("..." . "/root/agent-2")))
      (should (equal (cdr pair)
                     (niwa-agent-record-path
                      (niwa-agent-control--reserve
                       session "/root" (car pair) "default")))))
    (should (eq retained (cdar (niwa-session-agent-registry session))))
    (should (equal "/root/review-copy/review-copy"
                   (niwa-agent-record-path
                    (niwa-agent-control--reserve
                     session "/root/review-copy" "Review Copy" "default"))))))

(niwa-deftest niwa-agent-control-context-path ()
  ,test
  (test)
  :doc "maps root and retained invocation contexts to canonical paths"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation--create
                      :agent-id "alpha-id" :path "/root/alpha")))
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/alpha"
                 (niwa-agent-record--create
                  :id "alpha-id" :path "/root/alpha"))))
    (should (equal "/root" (niwa-agent-control-context-path session)))
    (should (equal "/root/alpha"
                   (niwa-agent-control-context-path invocation)))
    (setf (niwa-session-agent-registry session) nil)
    ;; A not-yet-published invocation (fresh spawn before first WAIT)
    ;; resolves to nil instead of erroring.
    (should-not (niwa-agent-control-context-path invocation))))

(niwa-deftest niwa-agent-control-direct-children ()
  ,test
  (test)
  :doc "returns only direct children sorted by path with compact role data"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (list
           (cons "/root/zeta"
                 (niwa-agent-record--create
                  :path "/root/zeta" :parent-path "/root"
                  :role "explorer"))
           (cons "/root/alpha/deep"
                 (niwa-agent-record--create
                  :path "/root/alpha/deep" :parent-path "/root/alpha"
                  :role "reviewer"))
           (cons "/root/alpha"
                 (niwa-agent-record--create
                  :path "/root/alpha" :parent-path "/root"
                  :role "worker"))))
    (should
     (equal '((:path "/root/alpha" :role "worker")
              (:path "/root/zeta" :role "explorer"))
            (niwa-agent-control-direct-children session "/root")))))

(niwa-deftest niwa-agent-control--rollback ()
  ,test
  (test)
  :doc "removes the reserved identity and associated transcript index entry"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (buffer (generate-new-buffer " *agent-control-rollback*"))
         (record (niwa-agent-record--create
                  :path "/root/retry"
                  :activity 'starting
                  :invocation invocation)))
    (setf (niwa-agent-invocation-agent-id invocation) "default--rollback")
    (setf (niwa-agent-invocation-buffer invocation) buffer)
    (setf (niwa-agent-invocation-parent-session invocation) session)
    (setf (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/retry.chat.org")
    (setf (niwa-session-agent-reservations session)
          (list (cons "/root/retry" record)))
    (setf (niwa-session-agent-transcripts session)
          '(("default--rollback" :path "agents/retry.chat.org")))
    (let (offered)
      (cl-letf (((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                 (lambda (_) (setq offered t))))
        (niwa-agent-control--rollback session record))
      ;; The released slot may let a waiting agent start.
      (should offered))
    (should-not (niwa-session-agent-registry session))
    (should-not (niwa-session-agent-reservations session))
    (should-not (niwa-session-agent-transcripts session))
    (should-not (buffer-live-p buffer))))

(niwa-deftest niwa-agent-control--bounded-result ()
  ,test
  (test)
  :doc "keeps small results intact and bounds large head/tail previews"
  (let ((record
         (niwa-agent-record--create
          :conversation-location "agents/large.chat.org")))
    (should (equal "small"
                   (niwa-agent-control--bounded-result record "small")))
    (let ((payload
           (niwa-agent-control--bounded-result
            record (concat "HEAD" (make-string 40000 ?x) "TAIL"))))
      (should (<= (length payload) (* 32 1024)))
      (should (string-prefix-p "HEAD" payload))
      (should (string-match-p "TAIL" payload))
      (should (string-match-p "agents/large.chat.org" payload)))))

(niwa-deftest niwa-agent-control--deliver-mail ()
  ,test
  (test)
  :doc "does not mark recovery records or records consumed during commit"
  (dolist (mode '(recovery consumed))
    (let* ((session (niwa-agent-control-test--session))
           (result (list :type 'RESULT :sender "/root/worker"
                         :recipient "/root" :payload "done"))
           offered)
      (unless (eq mode 'consumed)
        (setf (niwa-session-messages session) (list result)))
      (let ((niwa-agent-control-suppress-persistence (eq mode 'recovery)))
        (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event)
                   (lambda (&rest _) (setq offered t))))
          (niwa-agent-control--deliver-mail session "/root" result)))
      (should-not (plist-get result :wake))
      (should-not offered)))

  :doc "marks agent mail and offers a wake only to an idle agent"
  (let* ((session (niwa-agent-control-test--session))
         (record (niwa-agent-record--create
                  :path "/root/worker" :activity 'running))
         scheduled)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (cl-letf (((symbol-function 'niwa-agent-control--schedule-agent-wakes)
               (lambda (value)
                 (should (eq session value))
                 (setq scheduled (1+ (or scheduled 0))))))
      (dolist (activity '(running idle))
        (setf (niwa-agent-record-activity record) activity)
        (let ((mail (niwa-agent-control-test--mail
                     "/root" "/root/worker" (symbol-name activity)))
              (own (list :type 'EXECUTION :sender "/root/worker"
                         :recipient "/root/worker" :payload "done")))
          (setf (niwa-agent-record-mailbox record) (list own mail))
          (niwa-agent-control--deliver-mail session "/root/worker" mail)
          (niwa-agent-control--deliver-mail session "/root/worker" own)
          (should (plist-get mail :wake))
          (should-not (plist-get own :wake)))))
    (should (eql 1 scheduled))))

(niwa-deftest niwa-agent-control--publish-result ()
  ,test
  (test)
  :doc "only ordinary root delivery and committed handler fallback offer a wake"
  (dolist (parent '("/root" "/root/parent"))
    (dolist (handler-mode '(nil success failure))
      (let* ((session (niwa-agent-control-test--session))
             (result (list :type 'RESULT :sender (concat parent "/worker")
                           :recipient parent :outcome 'completed :payload "done"))
             (commits 0) offered
             (record
              (niwa-agent-record--create
               :path (concat parent "/worker") :parent-path parent
               :result-handler
               (and handler-mode
                    (lambda (_)
                      (should-not (niwa-agent-control-wake-pending-p session))
                      (when (eq handler-mode 'failure) (error "Broken handler")))))))
        (setf (niwa-session-agent-registry session)
              (list (cons parent (niwa-agent-record--create :path parent))))
        (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                   (lambda (_) (cl-incf commits)))
                  ((symbol-function 'niwa-pending-inputs-offer-root-event)
                   (lambda (&rest _)
                     (should (= commits 1))
                     (should (memq result (niwa-session-messages session)))
                     (push t offered))))
          (niwa-test--with-captured-diagnostics nil
            (niwa-agent-control--publish-result session record result)))
        (let ((wakes (and (equal parent "/root") (not (eq handler-mode 'success)))))
          (should (eq wakes (niwa-agent-control-wake-pending-p session)))
          (should (= (if wakes 1 0) (length offered)))))))

  :doc "successful workflow delivery records a receipt and defers durable removal"
  (with-temp-buffer
    (let* ((session (niwa-agent-control-test--session))
           (result '(:type RESULT :sender "/root/review"
                     :recipient "/root" :outcome completed :payload "done" :timestamp 0))
           (seen nil)
           (queued-during-handler nil)
           (persisted 0)
           (record
            (niwa-agent-record--create
             :path "/root/review" :parent-path "/root"
             :result-handler
             (lambda (value)
               (setq seen value)
               (setq queued-during-handler
                     (memq value (niwa-session-messages session)))))))
      (setf (niwa-session-root-buffer session) (current-buffer))
      (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                 (lambda (_session) (cl-incf persisted))))
        (niwa-agent-control--publish-result session record result))
      (should (eq result seen))
      (should queued-during-handler)
      (should (= 1 persisted))
      (should-not (niwa-agent-record-result-handler record))
      (should-not (niwa-session-messages session))
      (should (equal (niwa-agent-persistence-sanitize-mailbox (list result) "/root")
                     (plist-get (niwa-session-checkpoint-project session nil) :messages)))
      (should (equal (list (list :type 'mailbox-receipt :id (plist-get result :id)))
                     (niwa-transcript-audit-records (buffer-string) 'mailbox-receipt)))))

  :doc "falls back to durable parent mail when the workflow handler fails"
  (let* ((session (niwa-agent-control-test--session))
         (queued-during-handler nil)
         (record
          (niwa-agent-record--create
           :path "/root/review" :parent-path "/root"
           :result-handler
           (lambda (value)
             (setq queued-during-handler
                   (memq value (niwa-session-messages session)))
             (niwa-agent-control-send-message session "/root" "nested arrival")
             (error "Broken workflow"))))
         (result '(:type RESULT :sender "/root/review"
                   :recipient "/root" :outcome completed :payload "done"))
         warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest args) (setq warning args))))
      (niwa-agent-control--publish-result session record result))
    (should queued-during-handler)
    (should (memq result (niwa-session-messages session)))
    (should (equal "nested arrival"
                   (plist-get (car (niwa-session-messages session)) :payload)))
    (should-not (niwa-agent-record-result-handler record))
    (should (string-match-p "result handler failed"
                            (cadr warning))))

  :doc "withholds a handler until its RESULT enqueue commits, not consumption"
  (let* ((session (niwa-agent-control-test--session))
         (result '(:type RESULT :sender "/root/review"
                   :recipient "/root" :outcome completed :payload "done"))
         (calls 0)
         (commits 0)
         (record
          (niwa-agent-record--create
           :path "/root/review" :parent-path "/root"
           :result-handler (lambda (_value) (cl-incf calls)))))
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_session)
                 (cl-incf commits)
                 (when (= commits 1)
                   (error "Enqueue commit unavailable")))))
      (should-error
       (niwa-agent-control--publish-result session record result))
      (should (zerop calls))
      (should-not (niwa-session-messages session))
      (should (niwa-agent-record-result-handler record))
      (niwa-agent-control--publish-result session record result))
    (should (= 1 calls))
    (should-not (niwa-session-messages session))
    (should-not (niwa-agent-record-result-handler record))))

(niwa-deftest niwa-agent-control--settlement-mail-id ()
  ,test
  (test)
  :doc "producer keys freeze distinct identities within one invocation"
  (let* ((invocation (niwa-agent-invocation-create (niwa-agent-default)))
         (id (niwa-agent-control--settlement-mail-id invocation 'result)))
    (should (equal id (niwa-agent-control--settlement-mail-id invocation 'result)))
    (should-not (equal id (niwa-agent-control--settlement-mail-id invocation 'execution)))))

(niwa-deftest niwa-agent-control--settle ()
  ,test
  (test)
  :doc "authority preflight runs before terminal staging and snapshots nested arrivals"
  (dolist (failure '(nil t))
    (let* ((session (niwa-agent-control-test--session))
           (invocation (niwa-agent-invocation-create (niwa-agent-default)))
           (record (niwa-agent-record--create
                    :path "/root/work" :parent-path "/root"
                    :activity 'running :invocation invocation))
           transaction entered)
      (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
                 (lambda (value &optional _buffer)
                   (should (eq session value))
                   (setq entered t)
                   (should (eq 'running (niwa-agent-record-activity record)))
                   (should-not (niwa-agent-record-settled-result record))
                   (should-not (niwa-session-messages session))
                   (niwa-agent-control-send-message session "/root" "preflight arrival")
                   (when failure (error "Target acknowledgement failed"))
                   t)))
        (if failure
            (should-error (niwa-agent-control--settle session record invocation "answer"))
          (setq transaction (niwa-agent-control--settle session record invocation "answer"))))
      (should entered)
      (when transaction (funcall (car transaction)))
      (should (eq 'running (niwa-agent-record-activity record)))
      (should (eq invocation (niwa-agent-record-invocation record)))
      (should-not (niwa-agent-record-settled-result record))
      (should (equal '("preflight arrival")
                     (mapcar (lambda (message) (plist-get message :payload))
                             (niwa-session-messages session))))))

  :doc "all terminal outcomes wake the root only after settlement commits"
  (dolist (status '(completed error aborted))
    (let* ((session (niwa-agent-control-test--session))
           (invocation (niwa-agent-invocation--create
                        :path "/root/worker" :parent-session session
                        :transcript-status status))
           (record (niwa-agent-record--create
                    :path "/root/worker" :parent-path "/root"
                    :activity 'running :invocation invocation))
           committed offered)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
                 (lambda (_)
                   (should-not (niwa-agent-control-wake-pending-p session))
                   (setq committed t)))
                ((symbol-function 'niwa-pending-inputs-offer-root-event)
                 (lambda (&rest _)
                   (should committed)
                   (setq offered t))))
        (niwa-agent-control-test--settle session record invocation "response"))
      (should offered)
      (should (niwa-agent-control-wake-pending-p session))
      (should (eq (pcase status ('error 'errored) ('aborted 'interrupted) (_ 'completed))
                  (plist-get (car (niwa-session-messages session)) :outcome)))
      (should (eq 'idle (niwa-agent-record-activity record)))))

  :doc "publishes Bash completions separately from the exact terminal result"
  (dolist (parent-path '("/root" "/root/parent"))
    (dolist (workflow-p '(nil t))
      (let* ((session (niwa-agent-control-test--session))
             (path (concat parent-path "/review"))
             (invocation (niwa-agent-invocation--create
                          :path path :parent-session session
                          :runtime-execution-results
                          '(("second Bash" :execution-id "e2") ("first Bash"))))
             (parent (niwa-agent-record--create :path parent-path))
             (answer "{\"findings\": []}")
             delivered
             (record (niwa-agent-record--create
                      :path path :parent-path parent-path
                      :activity 'running :invocation invocation
                      :result-handler
                      (and workflow-p
                           (lambda (result) (setq delivered result))))))
        (setf (niwa-session-agent-registry session)
              (list (cons path record) (cons parent-path parent)))
        (niwa-agent-control-test--settle session record invocation answer)
        (niwa-agent-control-test--settle session record invocation "duplicate")
        (let ((messages (niwa-agent-control--mailbox session parent-path)))
          (unless workflow-p
            (setq delivered (pop messages)))
          (should (eq 'RESULT (plist-get delivered :type)))
          (should (equal answer (plist-get delivered :payload)))
          (should (equal answer (niwa-agent-record-settled-result record)))
          (should (equal '("first Bash" "second Bash")
                         (mapcar (lambda (msg) (plist-get msg :payload)) messages)))
          (should (equal '(:execution-id "e2")
                         (plist-get (cadr messages) :meta)))
          (should-not (plist-get (car messages) :meta))
          (dolist (message messages)
            (should (eq 'EXECUTION (plist-get message :type)))
            (should (equal path (plist-get message :sender)))
            (should (equal parent-path (plist-get message :recipient))))))))
  :doc "settles once, releases capacity, and emits concise canonical RESULT"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (record (niwa-agent-record--create
                  :path "/root/work"
                  :parent-path "/root"
                  :activity 'running
                  :invocation invocation
                  :blockers '((permission-blocked stale)))))
    (setf (niwa-agent-invocation-transcript-status invocation) 'error)
    (niwa-agent-control-test--settle
     session record invocation "fallback details"
     '(:error-details "Provider failed"))
    (niwa-agent-control-test--settle
     session record invocation "duplicate")
    (let ((result (car (niwa-session-messages session))))
      (should (= 1 (length (niwa-session-messages session))))
      (should (eq 'idle (niwa-agent-record-activity record)))
      (should-not (niwa-agent-record-blockers record))
      (should (eq 'RESULT (plist-get result :type)))
      (should (eq 'errored (plist-get result :outcome)))
      (should (equal "Provider failed" (plist-get result :payload)))
      (should (equal "Provider failed"
                     (niwa-agent-record-settled-result record)))
      (should (eq 'errored
                  (niwa-agent-record-settled-outcome record)))
      (should-not (plist-member result :usage))))

  :doc "offers a wake for mail missed during the final answer unless the turn failed"
  (dolist (status '(completed error))
    (let* ((session (niwa-agent-control-test--session))
           (invocation (niwa-agent-invocation-create
                        (niwa-agent-default)))
           (late (niwa-agent-control-test--mail
                  "/root" "/root/work" "late" :wake t))
           (record (niwa-agent-record--create
                    :path "/root/work" :parent-path "/root/parent"
                    :activity 'running :invocation invocation
                    :mailbox (list late)))
           (parent (niwa-agent-record--create
                    :path "/root/parent" :parent-path "/root"
                    :activity 'idle))
           scheduled)
      (setf (niwa-agent-invocation-transcript-status invocation) status
            (niwa-session-agent-registry session)
            (list (cons "/root/work" record) (cons "/root/parent" parent)))
      (cl-letf (((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                 (lambda (_) (setq scheduled t))))
        (niwa-agent-control-test--settle
         session record invocation "done"))
      (should scheduled)
      (should (eq (eq status 'completed) (plist-get late :wake)))
      ;; The RESULT wakes the idle parent.
      (let ((result (car (niwa-agent-record-mailbox parent))))
        (should (eq 'RESULT (plist-get result :type)))
        (should (plist-get result :wake)))))

  :doc "rolling back settlement restores the retained provider snapshot"
  :doc "a delivery retry leaves the agent's next turn its wait and wake marks"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create (niwa-agent-default)))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation))
         (failures 1)
         transaction)
    (setf (niwa-agent-invocation-transcript-status invocation) 'error
          (niwa-session-agent-registry session)
          (list (cons "/root/work" record)))
    (setq transaction (niwa-agent-control-test--stage-settle
                       session record invocation "failed"))
    (cl-letf* ((publish (symbol-function 'niwa-agent-control--publish-result))
               ((symbol-function 'niwa-agent-control--publish-result)
                (lambda (&rest args)
                  (if (> failures 0)
                      (progn (cl-decf failures) (error "Parent waiter failed"))
                    (apply publish args)))))
      (should-error (funcall (cdr transaction)))
      ;; The agent's next turn waits and receives new mail meanwhile.
      (let ((waiter (niwa-agent-waiter--create :callback #'ignore))
            (mail (niwa-agent-control-test--mail
                   "/root" "/root/work" "for the next turn" :wake t)))
        (setf (niwa-agent-record-waiter record) waiter)
        (push mail (niwa-agent-record-mailbox record))
        (funcall (cdr transaction))
        (should (eq waiter (niwa-agent-record-waiter record)))
        (should (plist-get mail :wake))))
    (should (eq 'RESULT (plist-get (car (niwa-session-messages session))
                                   :type))))

  :doc "rolling back settlement restores the retained provider snapshot"
  (let* ((session (niwa-agent-control-test--session))
         (old '(:backend "Old" :model "old" :effort nil))
         (invocation (niwa-agent-invocation--create
                      :frozen-configuration
                      (niwa-agent-configuration--create
                       :request-locals
                       `((gptel-backend . ,(gptel--make-backend :name "New"))
                         (gptel-model . new)))))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation
                  :model-metadata old))
         (transaction (niwa-agent-control-test--stage-settle
                       session record invocation "answer")))
    (should (equal '(:backend "New" :model "new" :effort nil)
                   (niwa-agent-record-model-metadata record)))
    (funcall (car transaction))
    (should (eq old (niwa-agent-record-model-metadata record)))
    (should (eq invocation (niwa-agent-record-invocation record)))
    (should-not (niwa-session-messages session)))

  :doc "settlement rollback preserves nested arrivals and forwarded execution identity"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation--create
                      :path "/root/work" :parent-session session))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation))
         ;; The runtime binds this around the whole settlement, so an
         ;; arrival for the already-idle record waits instead of starting it.
         (niwa-agent-control-settling (list invocation))
         transaction execution-id)
    (setf (niwa-session-agent-registry session) (list (cons "/root/work" record)))
    (niwa-agent-control-enqueue-execution-result session "/root/work" "unread execution")
    (setq execution-id (plist-get (car (niwa-agent-record-mailbox record)) :id)
          transaction (niwa-agent-control-test--stage-settle session record invocation "answer"))
    (should (equal execution-id (plist-get (car (niwa-session-messages session)) :id)))
    (niwa-agent-control-send-message session "/root" "new root arrival")
    (niwa-agent-control-send-message session "/root/work" "new agent arrival")
    (funcall (car transaction))
    (should (equal '("new root arrival")
                   (mapcar (lambda (message) (plist-get message :payload))
                           (niwa-session-messages session))))
    (should (equal '("new agent arrival" "unread execution")
                   (mapcar (lambda (message) (plist-get message :payload))
                           (niwa-agent-record-mailbox record))))
    (should (equal execution-id (plist-get (cadr (niwa-agent-record-mailbox record)) :id))))

  :doc "failed settlement retry preserves IDs but the next turn gets fresh IDs"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation--create
                      :path "/root/work" :parent-session session
                      :runtime-execution-results '(("completion"))))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation))
         (transaction (niwa-agent-control-test--stage-settle session record invocation "answer"))
         (ids (mapcar (lambda (message) (plist-get message :id))
                      (niwa-session-messages session))))
    (funcall (car transaction))
    (setq transaction (niwa-agent-control-test--stage-settle session record invocation "answer"))
    (should (equal ids (mapcar (lambda (message) (plist-get message :id))
                              (niwa-session-messages session))))
    (funcall (cdr transaction))
    (should-not (plist-get (niwa-engine-info invocation) :niwa-settlement-mail-ids))
    ;; Even reusing the same invocation and completion entry cannot retain
    ;; the previous turn's IDs.  An ordinary new turn allocates fresh ones.
    (setf (niwa-session-messages session) nil
          (niwa-agent-record-activity record) 'running
          (niwa-agent-record-invocation record) invocation)
    (setq transaction (niwa-agent-control-test--stage-settle session record invocation "next answer"))
    (should-not (cl-intersection ids
                                 (mapcar (lambda (message) (plist-get message :id))
                                         (niwa-session-messages session))
                                 :test #'equal))
    (funcall (car transaction)))

  :doc "parent WAIT cannot consume tentative terminal mail before commit or rollback"
  (with-temp-buffer
    (let* ((session (niwa-agent-control-test--session))
           (invocation (niwa-agent-invocation--create
                        :path "/root/work" :parent-session session
                        :runtime-execution-results '(("completion"))))
           (record (niwa-agent-record--create
                    :path "/root/work" :parent-path "/root"
                    :activity 'running :invocation invocation))
           (data (list :messages []))
           (fsm (gptel-make-fsm :info (list :buffer (current-buffer) :backend nil :data data)))
           transaction)
      (setq-local niwa--session session)
      (setf (niwa-session-root-buffer session) (current-buffer))
      (setq transaction (niwa-agent-control-test--stage-settle session record invocation "answer"))
      ;; This is the receiving WAIT interleaved with the writer's before-save
      ;; I/O, while its ordinary raw sidecar still needs the tentative records.
      (should (= 2 (length (niwa-session-messages session))))
      (niwa-tools--handle-message-inject fsm)
      (should (= 0 (length (plist-get data :messages))))
      (should (= 0 (buffer-size)))
      (should-not (plist-get (niwa-session-checkpoint-project session nil) :messages))
      (funcall (car transaction))
      (should-not (niwa-session-messages session))
      (setq transaction (niwa-agent-control-test--stage-settle session record invocation "answer"))
      (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event) #'ignore))
        (funcall (cdr transaction)))
      (niwa-tools--handle-message-inject fsm)
      (should (= 2 (length (plist-get data :messages))))
      (should (= 2 (length (niwa-transcript-audit-records (buffer-string) 'mailbox-receipt))))
      (should-not (niwa-session-messages session))))

  :doc "reports the child's normalized token usage while its FSM is attached"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create (niwa-agent-default)))
         (record (niwa-agent-record--create
                  :path "/root/verify" :parent-path "/root"
                  :activity 'running :invocation invocation)))
    (setf (niwa-agent-invocation-runtime-fsm invocation)
          (gptel-make-fsm :info '(:tokens-full (:input 7 :output 3 :cached 100))))
    (niwa-agent-control-test--settle session record invocation "VERDICT: PASS")
    (should (= 10 (plist-get (car (niwa-session-messages session)) :usage))))

  :doc "settles a workflow-owned turn once through its RESULT handler"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         results
         (record
          (niwa-agent-record--create
           :path "/root/review" :parent-path "/root"
           :activity 'running :invocation invocation
           :result-handler (lambda (result) (push result results)))))
    (setf (niwa-agent-invocation-transcript-status invocation) 'completed)
    (niwa-agent-control-test--settle
     session record invocation "review result")
    (niwa-agent-control-test--settle
     session record invocation "duplicate")
    (should (= 1 (length results)))
    (should (eq 'completed (plist-get (car results) :outcome)))
    (should (equal "review result" (plist-get (car results) :payload)))
    (should (equal "review result"
                   (niwa-agent-record-settled-result record)))
    (should (eq 'completed
                (niwa-agent-record-settled-outcome record)))
    (should (eq 'idle (niwa-agent-record-activity record)))
    (should-not (niwa-agent-record-invocation record))
    (should-not (niwa-session-messages session)))

  :doc "retains a complete oversized response while mailbox delivery is bounded"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (payload (concat "response-head\n"
                          (make-string 40000 ?x)
                          "\nresponse-tail"))
         (record (niwa-agent-record--create
                  :path "/root/large"
                  :parent-path "/root"
                  :activity 'running
                  :invocation invocation)))
    (setf (niwa-agent-invocation-transcript-status invocation) 'completed)
    (niwa-agent-control-test--settle
     session record invocation payload)
    (let* ((message (car (niwa-session-messages session)))
           (delivered (plist-get message :payload))
           (retained (niwa-agent-control-settled-result record)))
      (should (equal payload
                     (niwa-agent-record-settled-result record)))
      (should (eq 'completed
                  (niwa-agent-record-settled-outcome record)))
      (should (equal payload (plist-get retained :payload)))
      (should (eq 'completed (plist-get retained :outcome)))
      (should (< (length delivered) (length payload)))
      (should (<= (length delivered)
                  niwa-agent-control--result-limit))
      (should-not (equal payload delivered))))

  :doc "commit failure retains the active turn and withholds RESULT and wake"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (metadata '(:backend "Launch" :model "requested" :effort "high"))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root"
                  :activity 'running :invocation invocation
                  :model-metadata metadata
                  :blockers '((waiting live))))
         reasons)
    (setf (niwa-agent-invocation-frozen-configuration invocation)
          (niwa-agent-configuration--create
           :request-locals
           `((gptel-backend . ,(gptel--make-backend :name "Served"))
             (gptel-model . served) (gptel-reasoning-effort . nil))))
    (setf (niwa-session-save-path session) temporary-file-directory
          (niwa-session-agent-root-waiter session)
          (niwa-agent-waiter--create
           :callback (lambda (reason) (push reason reasons)))
          (niwa-agent-invocation-transcript-status invocation) 'completed)
    (setf (niwa-agent-invocation-runtime-execution-results invocation)
          '(("Bash output")))
    (dolist (failure '(nil error))
      (cl-letf (((symbol-function
                  'niwa-session-persistence-save-agent-state)
                 (if (eq failure 'error)
                     (lambda (&rest _) (error "Injected commit failure"))
                   (lambda (&rest _) nil))))
        (should-error
         (niwa-agent-control-test--settle
          session record invocation "retryable result")))
      (should (eq 'running (niwa-agent-record-activity record)))
      (should (eq invocation (niwa-agent-record-invocation record)))
      (should (eq metadata (niwa-agent-record-model-metadata record)))
      (should (equal '(:backend "Served" :model "served" :effort nil)
                     (niwa-agent-control-record-model-metadata record)))
      (should (equal '((waiting live))
                     (niwa-agent-record-blockers record)))
      (should-not (niwa-agent-record-settled-result record))
      (should-not (niwa-session-messages session))
      (should-not reasons)
      (should (niwa-session-agent-root-waiter session)))
    ;; A later successful publication retains the effective nil effort.
    (let ((niwa-agent-control-suppress-persistence t))
      (niwa-agent-control-test--settle
       session record invocation "retryable result"))
    (should (eq 'idle (niwa-agent-record-activity record)))
    (should (equal '(:backend "Served" :model "served" :effort nil)
                   (niwa-agent-record-model-metadata record)))))

(niwa-deftest niwa-agent-control-settled-result ()
  ,test
  (test)
  :doc "hides a retained result while its agent turn is active"
  (let ((record
         (niwa-agent-record--create
          :path "/root/active"
          :activity 'running
          :settled-result "old result"
          :settled-outcome 'completed)))
    (should-not (niwa-agent-control-settled-result record)))
  :doc "returns a retained result and terminal outcome when idle"
  (let* ((record
          (niwa-agent-record--create
           :path "/root/idle"
           :activity 'idle
           :settled-result "complete result"
           :settled-outcome 'completed))
         (result (niwa-agent-control-settled-result record)))
    (should (equal '(:outcome completed :payload "complete result") result))
    (should (equal "complete result"
                   (niwa-agent-record-settled-result record)))))

(niwa-deftest niwa-agent-control-interrupt-user ()
  ,test
  (test)
  :doc "interrupts an active agent with a user reason, leaves idle agents alone"
  (let* ((session (niwa-agent-control-test--session))
         (invocation (niwa-agent-invocation--create :agent-id "busy-id"))
         (busy (niwa-agent-record--create
                :id "busy-id" :path "/root/busy" :parent-path "/root"
                :activity 'running :invocation invocation))
         (idle (niwa-agent-record--create
                :id "idle-id" :path "/root/idle" :parent-path "/root"
                :activity 'idle
                :mailbox (list (list :type 'MAIL :sender "/root"
                                     :payload "waiting for a slot" :wake t))))
         interrupted)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/busy" busy) (cons "/root/idle" idle)))
    (cl-letf (((symbol-function 'niwa-agent-runtime-interrupt)
               (lambda (seen reason) (push (list seen reason) interrupted))))
      (should (niwa-agent-control-interrupt-user session "/root/busy"))
      (should (equal (list (list invocation "interrupted by user")) interrupted))
      (should-not (niwa-agent-control-interrupt-user session "/root/idle"))
      (should (= 1 (length interrupted)))
      ;; An idle agent waiting for a slot stops waiting.
      (should-not (plist-get (car (niwa-agent-record-mailbox idle)) :wake))))
  :doc "refuses root and agents that are gone"
  (let ((session (niwa-agent-control-test--session)))
    (should-error (niwa-agent-control-interrupt-user session "/root")
                  :type 'user-error)
    (should-error (niwa-agent-control-interrupt-user session "/root/gone")
                  :type 'user-error)))

(niwa-deftest niwa-agent-control-interrupt ()
  ,test
  (test)
  :doc "rejects root, self, malformed, unknown, and opaque-id targets"
  (let* ((session (niwa-agent-control-test--session))
         (caller-invocation
          (niwa-agent-invocation--create :agent-id "caller-id"))
         (caller
          (niwa-agent-record--create
           :id "caller-id" :path "/root/caller" :activity 'running
           :invocation caller-invocation))
         (peer
          (niwa-agent-record--create
           :id "peer-id" :path "/root/peer" :activity 'idle)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/caller" caller)
                (cons "/root/peer" peer)))
    (let ((niwa--agent-invocation caller-invocation))
      (dolist (target '("/root" "/root/caller" "/root/missing"
                        "peer-id" "../peer"))
        (should-error
         (niwa-agent-control-interrupt session target)))))

  :doc "idle interruption is a successful no-op preserving retained state"
  (let* ((session (niwa-agent-control-test--session))
         (mail '((:type MAIL :sender "/root" :recipient "/root/idle"
                  :payload "remember")))
         (record
          (niwa-agent-record--create
           :id "idle-id" :path "/root/idle" :parent-path "/root"
           :activity 'idle :mailbox mail
           :configuration '(:role "default"))))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/idle" record)))
    (let ((result
           (niwa-agent-control-interrupt session "/root/idle")))
      (should (equal "/root/idle" (plist-get result :path)))
      (should (eq 'idle (plist-get result :previous-activity)))
      (should (eq 'idle (niwa-agent-record-activity record)))
      (should (eq mail (niwa-agent-record-mailbox record)))
      (should (assoc "/root/idle"
                     (niwa-session-agent-registry session)))
      (should-not (niwa-session-messages session))))

  :doc "active interruption settles once, retains descendants, and permits follow-up"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-interrupt*"))
         (invocation
          (niwa-agent-invocation--create
           :agent-id "parent-id" :parent-session session))
         (record
          (niwa-agent-record--create
           :id "parent-id" :path "/root/parent" :parent-path "/root"
           :activity 'running :invocation invocation
           :configuration
           (niwa-agent-configuration--create
            :agent (niwa-agent--create
                    :name "default" :frozen-p t))
           :conversation-buffer buffer))
         (descendant
          (niwa-agent-record--create
           :id "child-id" :path "/root/parent/child"
           :parent-path "/root/parent" :activity 'running))
         interrupted)
    (unwind-protect
        (progn
          (setf (niwa-session-agent-registry session)
                (list (cons "/root/parent" record)
                      (cons "/root/parent/child" descendant)))
          (cl-letf
              (((symbol-function 'niwa-agent-runtime-interrupt)
                (lambda (seen-invocation reason)
                  (setq interrupted (list seen-invocation reason))
                  (setf (niwa-agent-invocation-terminal-reason
                         seen-invocation)
                        reason)
                  (setf (niwa-agent-invocation-transcript-status
                         seen-invocation)
                        'aborted)
                  (let ((response
                         "Agent turn interrupted.\n\nReason: interrupted by /root\n\nPartial response: useful work"))
                    (niwa-agent-control-test--settle
                     session record seen-invocation response
                     (list :niwa-agent-terminal-status 'aborted
                           :response response))
                    response))))
            (let ((result
                   (niwa-agent-control-interrupt
                    session "/root/parent")))
              (should (eq 'running (plist-get result :previous-activity)))
              (should (eq invocation (car interrupted)))
              (should (string-match-p "/root" (cadr interrupted)))))
          (let ((result (car (niwa-session-messages session))))
            (should (= 1 (length (niwa-session-messages session))))
            (should (eq 'RESULT (plist-get result :type)))
            (should (eq 'interrupted (plist-get result :outcome)))
            (should (string-match-p "useful work"
                                    (plist-get result :payload))))
          (should (eq 'interrupted
                      (niwa-agent-record-settled-outcome record)))
          (should (equal (plist-get (car (niwa-session-messages session))
                                    :payload)
                         (niwa-agent-record-settled-result record)))
          (should (eq 'idle (niwa-agent-record-activity record)))
          (should-not (niwa-agent-record-invocation record))
          (should (eq 'running
                      (niwa-agent-record-activity descendant)))
          (should (assoc "/root/parent"
                         (niwa-session-agent-registry session)))
          (should (assoc "/root/parent/child"
                         (niwa-session-agent-registry session)))
          (cl-letf
              (((symbol-function 'niwa-agent-runtime-dispatch)
                (lambda (_agent _description _message &rest keys)
                  (let ((next
                         (niwa-agent-invocation--create
                          :agent-id "parent-id" :parent-session session
                          :buffer buffer
                          :path (plist-get keys :path))))
                    (setf (niwa-agent-invocation-frozen-configuration next)
                          (plist-get keys :frozen-configuration))
                    (funcall (plist-get keys :on-invocation) next)
                    t))))
            (niwa-agent-control-send-message
             session "/root/parent" "continue"))
          (should (eq 'running (niwa-agent-record-activity record))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "provider abort failure leaves the retained turn active"
  (let* ((session (niwa-agent-control-test--session))
         (invocation
          (niwa-agent-invocation--create
           :agent-id "abort-fails-id" :parent-session session))
         (record
          (niwa-agent-record--create
           :id "abort-fails-id" :path "/root/abort_fails"
           :parent-path "/root" :activity 'running
           :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/abort_fails" record)))
    (cl-letf
        (((symbol-function 'niwa-agent-runtime-interrupt)
          (lambda (_invocation _reason)
            (error "Backend abort failed"))))
      (should-error
       (niwa-agent-control-interrupt
        session "/root/abort_fails")))
    (should (eq 'running (niwa-agent-record-activity record)))
    (should (eq invocation (niwa-agent-record-invocation record)))
    (should-not (niwa-session-messages session)))

  :doc "interrupt-versus-settlement races emit exactly one RESULT"
  (let* ((session (niwa-agent-control-test--session))
         (invocation
          (niwa-agent-invocation--create
           :agent-id "race-id" :parent-session session))
         (record
          (niwa-agent-record--create
           :id "race-id" :path "/root/race" :parent-path "/root"
           :activity 'running :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/race" record)))
    (cl-letf
        (((symbol-function 'niwa-agent-runtime-interrupt)
          (lambda (seen-invocation _reason)
            (setf (niwa-agent-invocation-transcript-status
                   seen-invocation)
                  'aborted)
            (niwa-agent-control-test--settle
             session record seen-invocation "interrupted first"
             '(:niwa-agent-terminal-status aborted
               :response "interrupted first")))))
      (niwa-agent-control-interrupt session "/root/race"))
    (setf (niwa-agent-invocation-transcript-status invocation) 'completed)
    (niwa-agent-control-test--settle
     session record invocation "late completion")
    (should (= 1 (length (niwa-session-messages session))))
    (should (eq 'interrupted
                (plist-get (car (niwa-session-messages session))
                           :outcome))))

  :doc "a completion that wins the race remains the only settlement"
  (let* ((session (niwa-agent-control-test--session))
         (invocation
          (niwa-agent-invocation--create
           :agent-id "winner-id" :parent-session session))
         (record
          (niwa-agent-record--create
           :id "winner-id" :path "/root/winner" :parent-path "/root"
           :activity 'running :invocation invocation)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/winner" record)))
    (cl-letf
        (((symbol-function 'niwa-agent-runtime-interrupt)
          (lambda (seen-invocation _reason)
            (setf (niwa-agent-invocation-transcript-status
                   seen-invocation)
                  'completed)
            (niwa-agent-control-test--settle
             session record seen-invocation "completed first")
            "too late")))
      (let ((result
             (niwa-agent-control-interrupt session "/root/winner")))
        (should (eq 'running (plist-get result :previous-activity)))))
    (should (= 1 (length (niwa-session-messages session))))
    (should (eq 'completed
                (plist-get (car (niwa-session-messages session))
                           :outcome)))))

(niwa-deftest niwa-agent-control--record-invocation ()
  ,test
  (test)
  :doc "publishes a reserved identity with its conversation location"
  (let ((session (niwa-agent-control-test--session))
        (record (niwa-agent-record--create :path "/root/worker"))
        (invocation (niwa-agent-invocation-create
                     (niwa-agent-default)))
        (buffer (generate-new-buffer " *agent-control-record*")))
    (setf (niwa-session-agent-reservations session)
          (list (cons "/root/worker" record)))
    (setf (niwa-agent-invocation-agent-id invocation) "default--opaque")
    (setf (niwa-agent-invocation-path invocation) "/root/worker")
    (setf (niwa-agent-invocation-buffer invocation) buffer)
    (setf (niwa-agent-invocation-transcript-relative-path invocation)
          "agents/default.chat.org")
    (niwa-agent-control--record-invocation session record invocation)
    (should (equal "default--opaque" (niwa-agent-record-id record)))
    (should (eq invocation (niwa-agent-record-invocation record)))
    (should (eq buffer (niwa-agent-record-conversation-buffer record)))
    (should (equal "agents/default.chat.org"
                   (niwa-agent-record-conversation-location record)))
    (should-not (niwa-session-agent-reservations session))
    (should (eq record
                (cdr (assoc "/root/worker"
                            (niwa-session-agent-registry session)))))
    (kill-buffer buffer))

  :doc "commit failure restores the reservation and record fields"
  (let* ((session (niwa-agent-control-test--session))
         (record (niwa-agent-record--create
                  :path "/root/worker" :activity 'starting
                  :configuration '(:role "worker")))
         (invocation (niwa-agent-invocation-create
                      (niwa-agent-default)))
         (buffer (generate-new-buffer " *agent-control-record-failure*")))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) temporary-file-directory
                (niwa-session-agent-reservations session)
                (list (cons "/root/worker" record))
                (niwa-agent-invocation-agent-id invocation) "new-id"
                (niwa-agent-invocation-path invocation) "/root/worker"
                (niwa-agent-invocation-buffer invocation) buffer
                (niwa-agent-invocation-transcript-relative-path invocation)
                "agents/new.chat.org")
          (dolist (failure '(nil error))
            (cl-letf (((symbol-function
                        'niwa-session-persistence-save-agent-state)
                       (if (eq failure 'error)
                           (lambda (&rest _)
                             (error "Injected commit failure"))
                         (lambda (&rest _) nil))))
              (should-error
               (niwa-agent-control--record-invocation
                session record invocation)))
            (should (eq record
                        (cdr (assoc
                              "/root/worker"
                              (niwa-session-agent-reservations session)))))
            (should-not (niwa-session-agent-registry session))
            (should (eq 'starting (niwa-agent-record-activity record)))
            (should-not (niwa-agent-record-id record))
            (should-not (niwa-agent-record-invocation record))
            (should (equal '(:role "worker")
                           (niwa-agent-record-configuration record)))))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-control--set-hook-context ()
  ,test
  (test)
  :doc "copies pending context onto its retained identity"
  (let* ((record (niwa-agent-record--create :path "/root/worker"))
         (entries (list (list :source "hook" :body "context"))))
    (niwa-agent-control--set-hook-context record entries)
    (setf (plist-get (car entries) :body) "changed")
    (should
     (equal '((:source "hook" :body "context"))
            (niwa-agent-record-hook-context-pending record)))))

(niwa-deftest niwa-agent-control--activate ()
  ,test
  (test)
  :doc "failed retained dispatch preserves the prior settled result"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-failed-followup*"))
         (record
          (niwa-agent-record--create
           :id "worker--retained"
           :path "/root/worker"
           :parent-path "/root"
           :role "worker"
           :configuration (niwa-agent-control-test--configuration)
           :activity 'idle
           :conversation-buffer buffer
           :settled-result "previous result"
           :settled-outcome 'completed
           :conversation-location "agents/worker.chat.org"))
         (persisted 0)
         offered)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-runtime-dispatch)
                   (lambda (&rest _) (error "Provider did not start")))
                  ((symbol-function 'niwa-agent-control--persist-session)
                   (lambda (_session) (cl-incf persisted)))
                  ((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                   (lambda (_) (setq offered t))))
          (should-error
           (niwa-agent-control--activate
            session record "Try the next turn."))
          (should (eq 'idle (niwa-agent-record-activity record)))
          (should-not (niwa-agent-record-invocation record))
          (should
           (equal '(:outcome completed :payload "previous result")
                  (niwa-agent-control-settled-result record)))
          (should (= 1 persisted))
          ;; The slot this start held may let a waiting agent start.
          (should offered))
      (kill-buffer buffer)))

  :doc "unread mail from a Plan sender stamps the started turn read-only"
  (let* ((session (niwa-agent-control-test--session))
         (record (niwa-agent-record--create
                  :path "/root/worker" :activity 'idle
                  :mailbox (list (list :type 'MAIL :sender "/root"
                                       :payload "inspect" :plan-read-only t))))
         stamp)
    (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
               (lambda (_session _record _message _tool-id plan-read-only)
                 (setq stamp plan-read-only))))
      (niwa-agent-control--activate session record "Edit it."))
    (should (eq t stamp)))

  :doc "clears the previous result before a retained turn becomes active"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-clears-result*"))
         (record
          (niwa-agent-record--create
           :id "worker--retained"
           :path "/root/worker"
           :parent-path "/root"
           :role "worker"
           :configuration (niwa-agent-control-test--configuration)
           :activity 'idle
           :conversation-buffer buffer
           :conversation-location "agents/worker.chat.org"
           :settled-result "previous result"
           :settled-outcome 'completed)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-agent-runtime-dispatch)
              (lambda (_agent _description _message &rest keys)
                (should-not (niwa-agent-control-settled-result record))
                (let ((invocation
                       (niwa-agent-invocation--create
                        :agent-id "worker--retained"
                        :path "/root/worker"
                        :parent-session session
                        :buffer buffer)))
                  (setf (niwa-agent-invocation-frozen-configuration
                         invocation)
                        (plist-get keys :frozen-configuration))
                  (funcall (plist-get keys :on-invocation) invocation)
                  t))))
          (niwa-agent-control--activate
           session record "Start a fresh turn."))
      (kill-buffer buffer))
    (should (eq 'running (niwa-agent-record-activity record)))
    (should-not (niwa-agent-control-settled-result record)))

  :doc "execution mail persists during activation and survives rollback"
  (dolist (outcome '(success error quit))
    (let* ((gptel--known-backends nil)
           (backend (gptel-make-openai "Followup Persistence Test"
                      :models '(test-model)))
           (session (niwa-agent-control-test--session))
           (configuration
            (niwa-agent-configuration--create
             :agent (niwa-agent--create
                     :name "default" :description "Retained test agent"
                     :frozen-p t)
             :request-locals
             (mapcar (lambda (symbol)
                       (cons symbol (and (eq symbol 'gptel-backend) backend)))
                     niwa-agent-request-local-symbols)))
           (record (niwa-agent-record--create
                    :id "retained" :path "/root/worker" :parent-path "/root"
                    :role "default" :configuration configuration
                    :activity 'idle :settled-result "previous result"
                    :settled-outcome 'completed
                    :conversation-location "agents/worker.chat.org"))
           observed)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (cl-letf
          (((symbol-function 'niwa-agent-control-commit-session)
            (lambda (owner)
              (niwa-agent-persistence-serialize-registry owner)))
           ((symbol-function 'niwa-agent-control--persist-session)
            (lambda (owner)
              (niwa-agent-persistence-serialize-registry owner)))
           ((symbol-function 'niwa-agent-control--dispatch-followup)
            (lambda (owner current _message _tool-id _plan-read-only)
              ;; Reproduce a process sentinel delivering another command's
              ;; result while provider startup yields to Emacs.
              (niwa-agent-control-enqueue-execution-result
               owner "/root" "Full suite passed")
              (setq observed t)
              (should (eq 'starting (niwa-agent-record-activity current)))
              (should-not (niwa-agent-record-settled-result current))
              (should-not (niwa-agent-record-settled-outcome current))
              (if (memq outcome '(error quit))
                  (signal outcome '("Dispatch interrupted"))
                (niwa-agent-control--record-invocation
                 owner current
                 (niwa-agent-invocation--create
                  :agent-id "retained" :path "/root/worker"
                  :frozen-configuration configuration
                  :transcript-relative-path "agents/worker.chat.org"))))))
        (let ((caught
               (condition-case err
                   (progn
                     (niwa-agent-control--activate
                      session record "Continue")
                     nil)
                 ((error quit) err))))
          (should (equal caught (and (not (eq outcome 'success))
                                     (list outcome "Dispatch interrupted"))))
          (should observed)
          (should (equal "Full suite passed"
                         (plist-get (car (niwa-session-messages session))
                                    :payload)))
          (if (eq outcome 'success)
              (should (eq 'running (niwa-agent-record-activity record)))
            (should (eq 'idle (niwa-agent-record-activity record)))
            (should-not (niwa-agent-record-invocation record))
            (should (equal '(:outcome completed :payload "previous result")
                           (niwa-agent-control-settled-result record)))))))))

;; Waking idle agents

(defun niwa-agent-control-test--mail (sender recipient payload &rest props)
  "Return a committed MAIL record from SENDER to RECIPIENT with PAYLOAD."
  (append (list :id (md5 payload) :type 'MAIL :sender sender
                :recipient recipient :payload payload
                :timestamp (current-time))
          props))

(niwa-deftest niwa-agent-control--startable-p ()
  ,test
  (test)
  :doc "no turn starts while a settlement in its session runs"
  (let* ((session (niwa-agent-control-test--session))
         (other-session (niwa-agent-control-test--session))
         (idle (niwa-agent-record--create :path "/root/idle" :activity 'idle)))
    (should (niwa-agent-control--startable-p session idle))
    (let ((niwa-agent-control-settling
           (list (niwa-agent-invocation--create :parent-session session))))
      (should-not (niwa-agent-control--startable-p session idle)))
    ;; Another session's settlement offers only its own agents afterwards,
    ;; so it must not hold this session's start.
    (let ((niwa-agent-control-settling
           (list (niwa-agent-invocation--create
                  :parent-session other-session))))
      (should (niwa-agent-control--startable-p session idle)))
    (setf (niwa-agent-record-activity idle) 'running)
    (should-not (niwa-agent-control--startable-p session idle))))

(niwa-deftest niwa-agent-control-settlement-finished ()
  ,test
  (test)
  :doc "offers the session's waiting agents a turn"
  (let ((session (niwa-agent-control-test--session))
        offered)
    (cl-letf (((symbol-function 'niwa-agent-control--schedule-agent-wakes)
               (lambda (value) (push value offered))))
      (niwa-agent-control-settlement-finished session))
    (should (equal (list session) offered))))

(niwa-deftest niwa-agent-control--capacity-p ()
  ,test
  (test)
  :doc "admits a turn only below the tree's active-turn capacity"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-turn-capacity session) 1
          (niwa-session-agent-registry session)
          (list (cons "/root/idle" (niwa-agent-record--create
                                    :path "/root/idle" :activity 'idle))))
    (should (niwa-agent-control--capacity-p session))
    (setf (niwa-agent-record-activity
           (cdar (niwa-session-agent-registry session)))
          'waiting)
    (should-not (niwa-agent-control--capacity-p session))))

(niwa-deftest niwa-agent-control-clear-agent-wakes ()
  ,test
  (test)
  :doc "keeps the mail queued but stops it from waking; unknown paths are ignored"
  (let* ((session (niwa-agent-control-test--session))
         (mail (niwa-agent-control-test--mail
                "/root" "/root/worker" "later" :wake t))
         (record (niwa-agent-record--create
                  :path "/root/worker" :activity 'idle :mailbox (list mail))))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (niwa-agent-control-clear-agent-wakes session "/root/worker")
    (niwa-agent-control-clear-agent-wakes session "/root/missing")
    (should (equal (list mail) (niwa-agent-record-mailbox record)))
    (should-not (plist-get mail :wake))))

(niwa-deftest niwa-agent-control--schedule-agent-wakes ()
  ,test
  (test)
  :doc "coalesces offers into one deferred run and does nothing without a root"
  (let ((session (niwa-agent-control-test--session))
        (buffer (generate-new-buffer " *agent-control-wake-root*"))
        scheduled)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (key _path thunk _on-cancel delay)
                     (push (list key delay) scheduled)
                     (should (functionp thunk))
                     t)))
          (niwa-agent-control--schedule-agent-wakes session)
          (should-not scheduled)
          (setf (niwa-session-root-buffer session) buffer)
          (niwa-agent-control--schedule-agent-wakes session)
          (should (equal (list (list (list 'agent-wake session) 0))
                         scheduled)))
      (kill-buffer buffer))))

(niwa-deftest niwa-agent-control--offer-agent-wakes ()
  ,test
  (test)
  :doc "starts idle agents oldest mail first while capacity allows"
  (let* ((session (niwa-agent-control-test--session))
         (root (generate-new-buffer " *agent-control-offer-root*"))
         (old (niwa-agent-control-test--mail "/root" "/root/b" "old" :wake t))
         (new (niwa-agent-control-test--mail "/root" "/root/a" "new" :wake t))
         (a (niwa-agent-record--create :path "/root/a" :activity 'idle
                                          :mailbox (list new)))
         (b (niwa-agent-record--create :path "/root/b" :activity 'idle
                                          :mailbox (list old)))
         (quiet (niwa-agent-record--create
                 :path "/root/quiet" :activity 'idle
                 :mailbox (list (niwa-agent-control-test--mail
                                 "/root" "/root/quiet" "read later"))))
         started)
    (setf (plist-get new :timestamp) (time-add (plist-get old :timestamp) 1))
    (setf (niwa-session-root-buffer session) root
          (niwa-session-agent-turn-capacity session) 1
          (niwa-session-agent-registry session)
          (list (cons "/root/a" a) (cons "/root/b" b)
                (cons "/root/quiet" quiet)))
    (with-current-buffer root (setq-local niwa--session session))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
                   (lambda (_session record message _tool-id plan-read-only)
                     (push (list (niwa-agent-record-path record) message
                                 plan-read-only)
                           started)
                     (setf (niwa-agent-record-activity record) 'running))))
          (niwa-agent-control--offer-agent-wakes session)
          (should (equal `(("/root/b" ,niwa-agent-control--wake-prompt nil))
                         started))
          ;; The slot frees; the agent still waiting starts next.
          (setf (niwa-agent-record-activity b) 'idle
                (niwa-agent-record-mailbox b) nil)
          (niwa-agent-control--offer-agent-wakes session)
          (should (equal "/root/a" (car (car started))))
          (should (= 2 (length started))))
      (kill-buffer root))
    (should (eq 'idle (niwa-agent-record-activity quiet))))

  :doc "a Plan sender's mail stamps the woken turn read-only"
  (let* ((session (niwa-agent-control-test--session))
         (root (generate-new-buffer " *agent-control-offer-plan*"))
         (record (niwa-agent-record--create
                  :path "/root/a" :activity 'idle
                  :mailbox (list (niwa-agent-control-test--mail
                                  "/root" "/root/a" "look" :wake t
                                  :plan-read-only t))))
         stamp)
    (setf (niwa-session-root-buffer session) root
          (niwa-session-agent-registry session) (list (cons "/root/a" record)))
    (with-current-buffer root (setq-local niwa--session session))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
                   (lambda (_session _record _message _tool-id plan-read-only)
                     (setq stamp plan-read-only))))
          (niwa-agent-control--offer-agent-wakes session))
      (kill-buffer root))
    (should (eq t stamp)))

  :doc "re-checks each agent after an earlier start yielded"
  (let* ((session (niwa-agent-control-test--session))
         (root (generate-new-buffer " *agent-control-offer-recheck*"))
         (first (niwa-agent-control-test--mail "/root" "/root/a" "1" :wake t))
         (second (niwa-agent-control-test--mail "/root" "/root/b" "2" :wake t))
         (a (niwa-agent-record--create :path "/root/a" :activity 'idle
                                          :mailbox (list first)))
         (b (niwa-agent-record--create :path "/root/b" :activity 'idle
                                          :mailbox (list second)))
         started)
    (setf (plist-get second :timestamp) (time-add (plist-get first :timestamp) 1)
          (niwa-session-root-buffer session) root
          (niwa-session-agent-registry session)
          (list (cons "/root/a" a) (cons "/root/b" b)))
    (with-current-buffer root (setq-local niwa--session session))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
                   (lambda (_session record &rest _)
                     (push (niwa-agent-record-path record) started)
                     ;; While A starts, the user interrupts B's slot wait.
                     (niwa-agent-control-interrupt-user session "/root/b"))))
          (niwa-agent-control--offer-agent-wakes session))
      (kill-buffer root))
    (should (equal '("/root/a") started)))

  :doc "a quit during startup does not restart the agent"
  (let* ((session (niwa-agent-control-test--session))
         (root (generate-new-buffer " *agent-control-offer-quit*"))
         (mail (niwa-agent-control-test--mail "/root" "/root/a" "q" :wake t))
         (record (niwa-agent-record--create
                  :path "/root/a" :activity 'idle :mailbox (list mail)))
         messages)
    (setf (niwa-session-root-buffer session) root
          (niwa-session-agent-registry session) (list (cons "/root/a" record)))
    (with-current-buffer root (setq-local niwa--session session))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
                   (lambda (&rest _) (signal 'quit nil)))
                  ((symbol-function 'niwa-agent-control--persist-session)
                   #'ignore)
                  ((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                   #'ignore))
          (niwa-test--with-captured-messages messages
            (niwa-agent-control--offer-agent-wakes session)))
      (kill-buffer root))
    (should (string-match-p "could not start /root/a for its mail: quit" messages))
    (should (equal (list mail) (niwa-agent-record-mailbox record)))
    (should-not (plist-get mail :wake)))

  :doc "a turn that cannot start leaves its mail unread without retrying"
  (let* ((session (niwa-agent-control-test--session))
         (root (generate-new-buffer " *agent-control-offer-fail*"))
         (mail (niwa-agent-control-test--mail "/root" "/root/a" "x" :wake t))
         (record (niwa-agent-record--create
                  :path "/root/a" :activity 'idle :mailbox (list mail)))
         messages)
    (setf (niwa-session-root-buffer session) root
          (niwa-session-agent-registry session) (list (cons "/root/a" record)))
    (with-current-buffer root (setq-local niwa--session session))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--dispatch-followup)
                   (lambda (&rest _) (error "Hook stopped the task")))
                  ((symbol-function 'niwa-agent-control--persist-session)
                   #'ignore))
          (niwa-test--with-captured-messages messages
            (niwa-agent-control--offer-agent-wakes session)))
      (kill-buffer root))
    (should (string-match-p "could not start /root/a" messages))
    (should (eq 'idle (niwa-agent-record-activity record)))
    (should (equal (list mail) (niwa-agent-record-mailbox record)))
    (should-not (plist-get mail :wake))))

(niwa-deftest niwa-agent-control--dispatch-followup
  ()
  ,test
  (test)
  :doc "reuses frozen configuration without resolving the current role"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-frozen-followup*"))
         (agent (niwa-agent--create
                 :name "worker" :system-prompt "Frozen." :frozen-p t))
         (configuration
          (niwa-agent-configuration--create :agent agent))
         (turn (niwa-agent-invocation-create agent))
         (record
          (niwa-agent-record--create
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
    (setf (niwa-agent-invocation-agent-id turn) "worker--retained"
          (niwa-agent-invocation-path turn) "/root/worker"
          (niwa-agent-invocation-buffer turn) buffer
          (niwa-agent-invocation-frozen-configuration turn) configuration
          (niwa-agent-invocation-transcript-relative-path turn)
          "agents/worker.chat.org"
          (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-resolve-role)
                   (lambda (&rest _)
                     (error "Follow-up re-resolved the mutable role")))
                  ((symbol-function 'niwa-agent-runtime-dispatch)
                   (lambda (seen-agent _description _message &rest keys)
                     (setq captured-agent seen-agent
                           captured-configuration
                           (plist-get keys :frozen-configuration)
                           captured-tool-use-id
                           (plist-get keys :parent-tool-use-id))
                     (funcall (plist-get keys :on-invocation) turn)
                     t)))
          (niwa-agent-control--dispatch-followup
           session record "Continue." "call_followup"))
      (kill-buffer buffer))
    (should-not captured-agent)
    (should (eq configuration captured-configuration))
    (should (equal "call_followup" captured-tool-use-id))
    (should (= 1 (length (niwa-session-agent-registry session)))))

  :doc "a removed working directory fails before dispatch and names the fix"
  (let* ((session (niwa-agent-control-test--session))
         (gone (make-temp-file "niwa-agent-control-gone-" t))
         (record (niwa-agent-record--create
                  :path "/root/reviewer" :parent-path "/root"
                  :activity 'starting :working-directory gone))
         dispatched)
    (delete-directory gone)
    (cl-letf (((symbol-function 'niwa-agent-persistence-ensure-conversation)
               (lambda (&rest _) (setq dispatched 'hydrated)))
              ((symbol-function 'niwa-agent-runtime-dispatch)
               (lambda (&rest _) (setq dispatched t))))
      (let ((err (should-error
                  (niwa-agent-control--dispatch-followup
                   session record "Continue.")
                  :type 'user-error)))
        (should (string-search "/root/reviewer" (cadr err)))
        (should (string-search "working_directory" (cadr err)))))
    (should-not dispatched)))

(niwa-deftest niwa-agent-control--normalize-context
  ()
  ,test
  (test)
  :doc "normalizes supported context fork modes before reserving a path"
  (progn
    (should (eq 'none (niwa-agent-control--normalize-context nil)))
    (should (eq 'all (niwa-agent-control--normalize-context "all")))
    (should (eq 'none (niwa-agent-control--normalize-context "none")))
    (should (eq 'summary
                (niwa-agent-control--normalize-context "summary")))
    (should (= 12 (niwa-agent-control--normalize-context "12")))
    (dolist (value '("" "0" "-1" "+1" "1.0" "ALL" 1))
      (should-error
       (niwa-agent-control--normalize-context value)
       :type 'user-error))))

(niwa-deftest niwa-agent-control-spawn
  (:quiet t
   :before-each (niwa-tools-register)
   :after-each (niwa-workspace-clear-registry))
  ,test
  (test)
  :doc "commits a real retained conversation while stubbing only provider start"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-agent-control-" t)))
         (workspace (niwa-workspace-get-or-create
                     'project "agent-control" root "agent-control"))
         (session (niwa-session-create "main" workspace))
         (parent (generate-new-buffer " *agent-control-parent*"))
         invocations
         roles)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (setq-local niwa--workspace workspace)
          (setq-local gptel-backend
                      (let ((gptel--known-backends nil))
                        (gptel-make-openai
                         "Agent Control" :key "test"
                         :models '(test-model))))
          (setq-local gptel-model 'test-model)
          (cl-letf (((symbol-function 'niwa-version)
                     (lambda (&rest _) "test"))
                    ((symbol-function 'niwa-agent-exec-run)
                     (lambda (_callback role _description child
                                        _buffer &optional _configure)
                       (push role roles)
                       (push child invocations)
                       'provider-request)))
            (let (outcome)
              (niwa-agent-control-spawn
               session "worker" "Implement the change."
               (lambda (value) (setq outcome value)))
              (should-not (plist-get outcome :error))
              (should
               (file-exists-p
                (file-name-concat (niwa-session-save-path session)
                                  "session.meta.el")))
              (let ((record (plist-get outcome :record)))
              (should (equal "/root/worker"
                             (niwa-agent-record-path record)))
              (should (eq 'running (niwa-agent-record-activity record)))
              (should (stringp
                       (niwa-agent-record-conversation-location record)))))
            (let* ((parent-invocation (car invocations))
                   outcome)
              (cl-letf (((symbol-function
                          'niwa-session-persistence-save-agent-state)
                         (lambda (&rest _) t)))
                (with-current-buffer (niwa-agent-invocation-buffer parent-invocation)
                  (niwa-agent-control-spawn
                   session "research" "Investigate first."
                   (lambda (value) (setq outcome value))
                   :role "explorer")))
              (should-not (plist-get outcome :error))
              (let ((nested (plist-get outcome :record)))
              (should (equal "/root/worker/research"
                             (niwa-agent-record-path nested)))
              (should (equal "/root/worker"
                             (niwa-agent-record-parent-path nested)))
              (should (equal "explorer"
                             (niwa-agent-record-role nested)))
              (should (equal "explorer" (car roles)))
              (let ((nested-invocation
                     (niwa-agent-record-invocation nested)))
                (cl-letf (((symbol-function
                            'niwa-session-persistence-save-agent-state)
                           (lambda (&rest _) t)))
                  (niwa-agent-control-test--settle
                   session nested nested-invocation "Nested result.")))
              (should-not (niwa-session-messages session))
              (let ((result
                     (car (niwa-agent-control--mailbox
                           session "/root/worker"))))
                (should (eq 'RESULT (plist-get result :type)))
                (should (equal "/root/worker/research"
                               (plist-get result :sender)))
                (should (equal "/root/worker"
                               (plist-get result :recipient))))))
            (let ((start-count 0)
                  (submit-count 0)
                  outcome)
              (let ((niwa-subagent-start-functions
                     (list (lambda (_event)
                             (cl-incf start-count)
                             nil)))
                    (niwa-user-prompt-submit-functions
                     (list (lambda (_event)
                             (cl-incf submit-count)
                             '(:updated-input "Hook-accepted task")))))
                (niwa-agent-control-spawn
                 session "hooked" "Original task"
                 (lambda (value) (setq outcome value))
                 :result-handler #'ignore))
              (let* ((record (plist-get outcome :record))
                     (invocation (niwa-agent-record-invocation record)))
                (should (= 1 start-count))
                (should (= 1 submit-count))
                (with-current-buffer
                    (niwa-agent-invocation-buffer invocation)
                  (should (= 1 (how-many "Hook-accepted task"
                                         (point-min) (point-max)))))
                (niwa-agent-control-test--settle
                 session record invocation "Hooked result.")))
            (should-error
             (niwa-agent-control-spawn
              session "unknown" "Fail before reservation."
              #'ignore
              :role "not_a_role")
             :type 'user-error)
            (should-not (assoc "/root/unknown"
                               (niwa-session-agent-registry session))))
          (let (outcome)
            (cl-letf (((symbol-function
                        'niwa-session-persistence-save-agent-state)
                       (lambda (&rest _)
                         (error "Injected commit failure"))))
              (niwa-agent-control-spawn
               session "commit_error" "Fail durable publication."
               (lambda (value) (setq outcome value))))
            (should (eq 'error (plist-get outcome :outcome)))
            (should-not (assoc "/root/commit_error"
                               (niwa-session-agent-registry session)))
            (should-not (assoc "/root/commit_error"
                               (niwa-session-agent-reservations session))))
          (let ((provider (gptel-make-fsm)) outcome diagnostics)
            (cl-letf
                (((symbol-function 'niwa-agent-exec-run)
                  (lambda (_callback _role _description invocation
                                     _buffer &optional _configure)
                    (push invocation invocations)
                    (setf (niwa-agent-invocation-runtime-fsm invocation)
                          provider)
                    provider))
                 ((symbol-function
                   'niwa-session-persistence-save-agent-state)
                  (lambda (&rest _) t)))
              (niwa-test--with-captured-diagnostics diagnostics
                (niwa-agent-control-spawn
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
                   (invocation (niwa-agent-record-invocation record)))
              (should (eq 'running (niwa-agent-record-activity record)))
              (should (eq provider
                          (niwa-agent-invocation-runtime-fsm invocation)))
              (cl-letf (((symbol-function
                          'niwa-session-persistence-save-agent-state)
                         (lambda (&rest _) t)))
                (niwa-agent-control-test--settle
                 session record invocation "Observer-safe result."))))
          (let ((niwa-agents--specs
                 '(("explorer" :description "available"))))
            (should-error
             (niwa-agent-control-spawn
              session "unavailable" "Reject session-excluded role."
              #'ignore
              :role "worker")
             :type 'user-error)
            (should-not (assoc "/root/unavailable"
                               (niwa-session-agent-registry session))))
          (let (synchronous-results scheduled
                (publication-fails t))
            (cl-letf (((symbol-function 'niwa-version)
                       (lambda (&rest _) "test"))
                      ((symbol-function 'run-at-time)
                       (lambda (_delay _repeat function &rest args)
                         (setq scheduled (cons function args))
                         'fake-terminal-retry))
                      ;; A portable session commits its terminal agent
                      ;; state through the publication, local target
                      ;; included.
                      ((symbol-function
                        'niwa-session-artifacts-publish-agent-terminal-state)
                       (lambda (_invocation &optional _token)
                         (when publication-fails
                           (error "Injected terminal publication failure"))
                         t))
                      ((symbol-function 'niwa-agent-exec-run)
                     (lambda (callback _role _description child
                                       buffer &optional _configure)
                       (push child invocations)
                       (let ((adapter
                              (niwa-agent-exec--make-callback
                               callback "worker" "Complete immediately."
                               (with-current-buffer buffer (point-min-marker))
                               (list nil))))
                         (funcall
                          adapter "Synchronous completion."
                          (list :niwa-agent-invocation child)))
                       'provider-request)))
              (let (outcome)
                (niwa-agent-control-spawn
                 session "synchronous" "Complete immediately."
                 (lambda (value) (setq outcome value))
                 :result-handler
                 (lambda (result) (push result synchronous-results)))
                (should-not (plist-get outcome :error))
                (let ((record (plist-get outcome :record)))
                  (should (eq 'running
                              (niwa-agent-record-activity record)))
                  (should scheduled)
                  (let ((retry scheduled))
                    (setq scheduled nil)
                    (apply (car retry) (cdr retry)))
                  (should (eq 'running
                              (niwa-agent-record-activity record)))
                  (should scheduled)
                  (setq publication-fails nil)
                  (let ((retry scheduled))
                    (setq scheduled nil)
                    (apply (car retry) (cdr retry)))
                  (should (eq 'idle (niwa-agent-record-activity record)))
                  (should-not scheduled)
                  (should (= 1 (niwa-agent-control--active-count session)))
                  (should (= 1 (length synchronous-results)))
                  (should (eq 'completed
                              (plist-get (car synchronous-results) :outcome)))
                  (should-not (niwa-session-messages session))))))
          (let (runner-called)
            (cl-letf (((symbol-function
                        'niwa-agent-conversation-save)
                       (lambda (_invocation &optional _deferred _token) nil))
                      ((symbol-function 'niwa-agent-exec-run)
                       (lambda (&rest _)
                         (setq runner-called t)
                         'provider-request)))
              (let (outcome)
                (niwa-agent-control-spawn
                 session "ephemeral" "Require a durable transcript."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not runner-called)
              (should-not (assoc "/root/ephemeral"
                                 (niwa-session-agent-registry session)))))
          (let ((niwa-subagent-start-functions
                 (list
                  (lambda (_event)
                    (should-not
                     (cl-find
                      "/root/blocked"
                      (niwa-agent-control-list-agents session)
                      :key (lambda (entry) (plist-get entry :path))
                      :test #'equal))
                    (should-error
                     (niwa-agent-control-resolve-path
                      session "/root" "/root/blocked")
                     :type 'user-error)
                    '(:continue nil :stop-reason "blocked by policy"))))
                runner-called)
            (cl-letf (((symbol-function 'niwa-agent-exec-run)
                       (lambda (&rest _)
                         (setq runner-called t)
                         'provider-request)))
              (let (outcome)
                (niwa-agent-control-spawn
                 session "blocked" "Never publish this agent."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not runner-called)
              (should-not (assoc "/root/blocked"
                                 (niwa-session-agent-registry session)))
              (should-not (niwa-session-agent-reservations session))))
          (let (prepare-callback outcome dispatched cancel)
            (setf (niwa-session-agent-turn-capacity session) 2)
            (cl-letf
                (((symbol-function 'niwa-agent-runtime-prepare-task)
                  (lambda (_agent _description _prompt _path callback
                           &rest _)
                    (setq prepare-callback callback)))
                 ((symbol-function 'niwa-agent-runtime-dispatch)
                  (lambda (&rest _)
                    (setq dispatched t))))
              (setq cancel
                    (niwa-agent-control-spawn
                     session "delayed" "Prepare later."
                     (lambda (value) (setq outcome value))))
              (should (assoc "/root/delayed"
                             (niwa-session-agent-reservations session)))
              (should-not
               (cl-find "/root/delayed"
                        (niwa-agent-control-list-agents session)
                        :key (lambda (entry) (plist-get entry :path))
                        :test #'equal))
              (should-error
               (niwa-agent-control-spawn
                session "delayed" "Duplicate." #'ignore))
              (should-error
               (niwa-agent-control-spawn
                session "at_capacity" "No slot." #'ignore))
              (funcall cancel)
              (should (eq 'aborted (plist-get outcome :outcome)))
              (should-not (niwa-session-agent-reservations session))
              (funcall prepare-callback
                       '(:outcome success :turn (:prompt "Too late")))
              (should-not dispatched))
            (setf (niwa-session-agent-turn-capacity session) 3))
          (let ((niwa-subagent-stop-functions
                 (list (lambda (_event) (push 'stop roles)))))
            (cl-letf (((symbol-function 'niwa-agent-exec-run)
                       (lambda (&rest _) nil)))
              (let (outcome)
                (niwa-agent-control-spawn
                 session "launch_error" "Fail before publication."
                 (lambda (value) (setq outcome value)))
                (should (eq 'error (plist-get outcome :outcome))))
              (should-not
               (assoc "/root/launch_error"
                      (niwa-session-agent-registry session)))
              (should-not (memq 'stop roles)))))
      (dolist (invocation invocations)
        (when-let* ((buffer (niwa-agent-invocation-buffer invocation))
                    ((buffer-live-p buffer)))
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (niwa-session-save-path session)
        (niwa-session-persistence-lock-release
         (niwa-session-save-path session)))
      (when (buffer-live-p parent)
        (kill-buffer parent))
      (delete-directory root t)))

  :doc "generates one unpublished task-focused summary before child dispatch"
  (let* ((session (niwa-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary*"))
         (parent-invocation
          (niwa-agent-invocation-create (niwa-agent-default)))
         (parent-record
          (niwa-agent-record--create
           :id "default--parent" :path "/root/parent" :activity 'running))
         summary-callback summary-cancelled captured-source captured-focus conversation
         provider-callbacks (summary-calls 0) outcome cancel transcript)
    (setf (niwa-agent-invocation-agent-id parent-invocation)
          "default--parent"
          (niwa-agent-invocation-path parent-invocation) "/root/parent"
          (niwa-session-agent-registry session)
          (list (cons "/root/parent" parent-record)))
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session
                      niwa--agent-invocation parent-invocation)
          (cl-letf
              (((symbol-function 'niwa-compact-evidence-summary-context-evidence)
                (lambda (tool-use-id)
                  (should (equal "call_agent" tool-use-id))
                  "Frozen parent evidence"))
               ((symbol-function 'niwa-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success
                             :turn (:prompt "Hook-accepted task\n\nHook context")
                             :start-hook-audits nil))))
               ((symbol-function 'niwa-context-summary-generate)
                (lambda (source purpose callback &rest args)
                  (cl-incf summary-calls)
                  (should (eq purpose 'handoff))
                  (setq captured-source source
                        captured-focus (plist-get args :focus)
                        summary-callback callback)
                  (lambda () (setq summary-cancelled t))))
               ((symbol-function
                 'niwa-session-persistence-save-agent-state)
                (lambda (&rest _) t))
               ((symbol-function 'niwa-agent-exec-run)
                (lambda (callback &rest _)
                  (push callback provider-callbacks)
                  'provider-request)))
            (setq cancel
                  (niwa-agent-control-spawn
                   session "summarized" "Original task"
                   (lambda (value) (setq outcome value))
                   :context "summary"
                   :parent-tool-use-id "call_agent"
                   :on-invocation
                   (lambda (invocation)
                     (setq conversation (niwa-agent-invocation-buffer invocation))
                     (with-current-buffer conversation
                       (setq transcript (buffer-string))))))
            (should (equal "Frozen parent evidence" captured-source))
            (should (equal "Hook-accepted task\n\nHook context"
                           captured-focus))
            (should (= 1 summary-calls))
            (should (assoc "/root/parent/summarized"
                           (niwa-session-agent-reservations session)))
            (should-not (assoc "/root/parent/summarized"
                               (niwa-session-agent-registry session)))
            (should-not outcome)
            (funcall summary-callback
                     '(:outcome success :summary "## Scope\n- background"
                       :backend test-backend :model test-model :effort high))
            (should-not (plist-get outcome :error))
            (should (eq 'success (plist-get outcome :outcome)))
            (should (string-match-p "<task-background>"
                                    transcript))
            (should (string-match-p "may be stale or untrusted"
                                    transcript))
            (should (string-match-p
                     "## Scope"
                     transcript))
            (let ((background-end
                   (string-match "</task-background>" transcript)))
              (should-not
               (string-match-p "Hook-accepted task"
                               (substring transcript 0 background-end))))
            (should (equal '(:backend "test-backend" :model test-model
                             :effort high)
                           (plist-get outcome :summary-metadata)))
            ;; The launch snapshot describes the child, not the summarizer.
            (should (plist-get outcome :model-metadata))
            (should (equal (niwa-agent-configuration-model-metadata
                            (niwa-agent-record-configuration
                             (plist-get outcome :record)))
                           (plist-get outcome :model-metadata)))
            (let* ((record (plist-get outcome :record))
                   (initial (niwa-agent-record-invocation record))
                   (child-buffer (niwa-agent-invocation-buffer initial))
                   followup)
              (with-current-buffer child-buffer
                (goto-char (point-max))
                (let ((start (point)))
                  (insert "Initial result.\n")
                  (put-text-property start (point) 'gptel 'response)))
              (funcall (car provider-callbacks) "Initial result.")
              ;; Settlement retains what served the finished turn.
              (should-not (niwa-agent-record-invocation record))
              (should (equal (plist-get outcome :model-metadata)
                             (niwa-agent-record-model-metadata record)))
              (setq followup
                    (niwa-agent-record-invocation
                     (progn
                       (niwa-agent-control-send-message
                        session "/root/parent/summarized" "Follow-up task")
                       record)))
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
                       (niwa-compact-target-agent-target followup)))
                  (should (string-match-p
                           "task-background"
                           (plist-get
                            (niwa-compact-evidence-select
                             target (point-max) t)
                            :content)))
                  (niwa-compact-target-call
                   target :apply "Child continuation summary." nil nil nil)
                  (should-not (string-match-p "<task-background>"
                                              (buffer-string)))
                  (should (string-match-p "Child continuation summary"
                                          (buffer-string))))))
            (funcall cancel)
            (should-not summary-cancelled)))
      ;; Settlement clears the record's invocation; retain the created
      ;; conversation directly so teardown cannot leak it into exit autosave.
      (when (buffer-live-p conversation)
        (with-current-buffer conversation (set-buffer-modified-p nil))
        (kill-buffer conversation))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "defers role freezing until the effective request is installed"
  (let* ((session (niwa-agent-control-test--session))
         (agent (niwa-agent--create
                 :name "dynamic"
                 :description "Dynamic role"
                 :system-prompt (lambda () "Dynamic.")))
         prepared-agent outcome cancel)
    (cl-letf (((symbol-function 'niwa-agent-runtime-prepare-task)
               (lambda (resolved &rest _)
                 (setq prepared-agent resolved))))
      (setq cancel
            (niwa-agent-control-spawn
             session "dynamic" "Prepare dynamic role."
             (lambda (value) (setq outcome value))
             :agent agent
             :model-policy '(:backend backend :model model :effort high)))
      (should (eq agent prepared-agent))
      (should-not (niwa-agent-frozen-p prepared-agent))
      (funcall cancel)
      (should (eq 'aborted (plist-get outcome :outcome)))))

  :doc "summary cancellation releases capacity and suppresses late dispatch"
  (let* ((session (niwa-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary-cancel*"))
         summary-callback (cancelled (list nil)) dispatched outcome cancel)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (cl-letf
              (((symbol-function 'niwa-compact-evidence-summary-context-evidence)
                (lambda (_) "Frozen evidence"))
               ((symbol-function 'niwa-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success :turn (:prompt "Final task")))))
               ((symbol-function 'niwa-context-summary-generate)
                (lambda (_source _purpose callback &rest _)
                  (setq summary-callback callback)
                  (lambda () (setcar cancelled t))))
               ((symbol-function 'niwa-agent-runtime-dispatch)
                (lambda (&rest _) (setq dispatched t))))
            (setq cancel
                  (niwa-agent-control-spawn
                   session "cancelled_summary" "Task"
                   (lambda (value) (setq outcome value))
                   :context "summary"))
            (should-not outcome)
            (funcall cancel)
            (should (car cancelled))
            (should (eq 'aborted (plist-get outcome :outcome)))
            (should-not (niwa-session-agent-reservations session))
            (funcall summary-callback
                     '(:outcome success :summary "Late summary"))
            (should-not dispatched)))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "summary generation failure releases the unpublished reservation"
  (let* ((session (niwa-agent-control-test--session))
         (parent (generate-new-buffer " *agent-control-summary-error*"))
         dispatched outcome)
    (unwind-protect
        (with-current-buffer parent
          (setq-local niwa--session session)
          (cl-letf
              (((symbol-function 'niwa-compact-evidence-summary-context-evidence)
                (lambda (_) "Frozen evidence"))
               ((symbol-function 'niwa-agent-runtime-prepare-task)
                (lambda (_agent _description _message _path callback &rest _)
                  (funcall callback
                           '(:outcome success :turn (:prompt "Final task")))))
               ((symbol-function 'niwa-context-summary-generate)
                (lambda (_source _purpose callback &rest _)
                  (funcall callback
                           '(:outcome error :error "Invalid summary"
                             :error-class validation))
                  #'ignore))
               ((symbol-function 'niwa-agent-runtime-dispatch)
                (lambda (&rest _) (setq dispatched t))))
            (niwa-agent-control-spawn
             session "failed_summary" "Task"
             (lambda (value) (setq outcome value))
             :context "summary")
            (should (eq 'error (plist-get outcome :outcome)))
            (should (string-match-p "Invalid summary"
                                    (plist-get outcome :error)))
            (should-not dispatched)
            (should-not (niwa-session-agent-reservations session))))
      (when (buffer-live-p parent) (kill-buffer parent))))

  :doc "reserves parallel summaries per tree without a global lock"
  (let* ((first (niwa-agent-control-test--session))
         (second (niwa-agent-control-test--session))
         (first-buffer (generate-new-buffer " *summary-capacity-first*"))
         (second-buffer (generate-new-buffer " *summary-capacity-second*"))
         callbacks cancels)
    (setf (niwa-session-agent-turn-capacity first) 1
          (niwa-session-agent-turn-capacity second) 1)
    (unwind-protect
        (progn
          (cl-letf
            (((symbol-function 'niwa-compact-evidence-summary-context-evidence)
              (lambda (_) "Frozen evidence"))
             ((symbol-function 'niwa-agent-runtime-prepare-task)
              (lambda (_agent _description _message _path callback &rest _)
                (funcall callback
                         '(:outcome success :turn (:prompt "Final task")))))
             ((symbol-function 'niwa-context-summary-generate)
              (lambda (_source _purpose callback &rest _)
                (push callback callbacks)
                #'ignore)))
          (with-current-buffer first-buffer
            (setq-local niwa--session first)
            (push (niwa-agent-control-spawn
                   first "one" "Task" #'ignore :context "summary")
                  cancels)
            (should-error
             (niwa-agent-control-spawn
              first "two" "Task" #'ignore :context "summary")
             :type 'user-error))
          (with-current-buffer second-buffer
            (setq-local niwa--session second)
            (push (niwa-agent-control-spawn
                   second "one" "Task" #'ignore :context "summary")
                  cancels))
          (should (= 2 (length callbacks)))
          (should (= 1 (length (niwa-session-agent-reservations first))))
          (should (= 1 (length
                        (niwa-session-agent-reservations second))))))
      (mapc #'funcall cancels)
      (kill-buffer first-buffer)
      (kill-buffer second-buffer))))

(niwa-deftest niwa-agent-control--identify-record ()
  ,test
  (test)
  :doc "assigns once and rejects malformed supplied producer identities"
  (let* ((record (list :type 'MAIL))
         (identified (niwa-agent-control--identify-record record))
         (id (plist-get identified :id)))
    (should (eq record identified))
    (should (stringp id))
    (should (equal id (plist-get (niwa-agent-control--identify-record record) :id))))
  (dolist (id '("" 12))
    (should-error (niwa-agent-control--identify-record (list :type 'MAIL :id id)))))

(niwa-deftest niwa-agent-control-message-receipt ()
  ,test
  (test)
  :doc "workflow and native receipts use the same hidden durable ID representation"
  (let ((receipt (niwa-agent-control-message-receipt '(:id "durable" :payload "not copied"))))
    (should (equal '((:type mailbox-receipt :id "durable"))
                   (niwa-transcript-audit-records receipt 'mailbox-receipt)))
    (should-not (string-search "not copied" receipt))))

(niwa-deftest niwa-agent-control--enqueue-records ()
  ,test
  (test)
  :doc "producer identities survive commit retry and remain distinct for equal payloads"
  (let* ((session (niwa-agent-control-test--session))
         (records (cl-loop for type in '(MAIL EXECUTION RESULT USER MONITOR MONITOR_TERMINAL)
                           collect (list :type type :recipient "/root" :payload "same")))
         ids)
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_) (error "Injected enqueue failure"))))
      (should-error (niwa-agent-control--enqueue-records session "/root" records)))
    (should-not (niwa-session-messages session))
    (setq ids (mapcar (lambda (record) (plist-get record :id)) records))
    (should (cl-every #'stringp ids))
    (should (= 6 (length (delete-dups (copy-sequence ids)))))
    (niwa-agent-control--enqueue-records session "/root" records)
    (should (equal ids (mapcar (lambda (record) (plist-get record :id))
                              (niwa-agent-control-context-mailbox session)))))

  :doc "failed enqueue commit preserves mail produced by a nested hook"
  (let ((session (niwa-agent-control-test--session)) nested)
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_)
                 (unless nested
                   (setq nested t)
                   (niwa-agent-control-send-message session "/root" "nested arrival")
                   (error "Injected outer enqueue failure")))))
      (should-error (niwa-agent-control-send-message session "/root" "outer mail")))
    (should (equal '("nested arrival")
                   (mapcar (lambda (message) (plist-get message :payload))
                           (niwa-session-messages session)))))

  :doc "a receiving WAIT cannot see an ordinary enqueue before its commit"
  (let ((session (niwa-agent-control-test--session)) entered)
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_)
                 (setq entered t)
                 (should (= 1 (length (niwa-session-messages session))))
                 (should-not (niwa-agent-control-context-mailbox session)))))
      (niwa-agent-control-send-message session "/root" "committed mail"))
    (should entered)
    (should (= 1 (length (niwa-agent-control-context-mailbox session))))))

(niwa-deftest niwa-agent-control-acknowledge-mail ()
  ,test
  (test)
  :doc "consumption removes live mail without I/O but retains durable redelivery"
  (let* ((session (niwa-agent-control-test--session))
         captured uncaptured)
    (niwa-agent-control-send-message session "/root" "captured")
    (setq captured (car (niwa-session-messages session)))
    (niwa-agent-control-send-message session "/root" "uncaptured")
    (setq uncaptured (car (niwa-session-messages session)))
    (setf (niwa-session-save-path session) temporary-file-directory)
    ;; Clearing the mailbox schedules a deferred agent-state save for this
    ;; root-less fixture session; release it so later tests don't drain it.
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-session-persistence-autosave-buffer)
                     (lambda (&rest _) (ert-fail "Consumption must not save"))))
            ;; A detached copy models capture/restore; identity, not object eq, wins.
            (niwa-agent-control-acknowledge-mail session (list (copy-tree captured))))
          (should (equal (list uncaptured) (niwa-session-messages session)))
          (should (equal (niwa-agent-persistence-sanitize-mailbox
                          (list uncaptured captured) "/root")
                         (plist-get (niwa-session-checkpoint-project
                                     session (list :messages (niwa-session-messages session)))
                                    :messages)))
          ;; Explicit discard is not a delivery receipt: it only drops live mail.
          (niwa-agent-control-clear-context-mailbox session)
          (should (equal (niwa-agent-persistence-sanitize-mailbox (list captured) "/root")
                         (plist-get (niwa-session-checkpoint-project session nil)
                                    :messages))))
      (niwa-session-persistence-cancel-deferred-agent-save session))))

(niwa-deftest niwa-agent-control-notify ()
  ,test
  (test)
  :doc "immediate notification runs with fresh transaction observations"
  (let ((session (niwa-agent-control-test--session))
        (niwa-session-durability--transaction-clock '(clock))
        (niwa-session-durability--asserted-directories '(directories))
        (niwa-session-durability--commit-riders '(riders))
        (niwa-session-durability--final-commit t)
        (niwa-session-durability-async-process-function #'ignore)
        (niwa-session-recovery--mutation-cache '(recovery))
        observed)
    (niwa-agent-control-notify
     session 'immediate
     (lambda ()
       (setq observed
             (list niwa-session-durability--transaction-clock
                   niwa-session-durability--asserted-directories
                   niwa-session-durability--commit-riders
                   niwa-session-durability--final-commit
                   niwa-session-durability-async-process-function
                   niwa-session-recovery--mutation-cache))))
    (should (equal '(nil nil nil nil nil nil) observed))
    (should (equal '(clock) niwa-session-durability--transaction-clock))
    (should-not (niwa-session-mail-notifications session)))

  :doc "a busy preparation coalesces one FIFO outside the mutation queue"
  (let ((session (niwa-agent-control-test--session)) calls)
    (with-temp-buffer
      (niwa-session-set-root-buffer session (current-buffer))
      (setf (niwa-session-publication-preparation-token session) '(owner))
      (unwind-protect
          (progn
            (niwa-agent-control-notify
             session 'first (lambda () (push 'first calls)))
            (let ((timer (niwa-session-mail-notification-timer session)))
              (should (timerp timer))
              (niwa-agent-control-notify
               session 'first (lambda () (push 'duplicate calls)))
              (niwa-agent-control-notify
               session 'second (lambda () (push 'second calls)))
              (should (eq timer (niwa-session-mail-notification-timer session))))
            (should-not calls)
            (should-not (niwa-session-publication-preparations session))
            (setf (niwa-session-publication-preparation-token session) nil)
            (let ((deadline (+ (float-time) 2)))
              (while (and (niwa-session-mail-notifications session)
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (should (equal '(first second) (reverse calls)))
            (should-not (niwa-session-mail-notifications session))
            (should-not (niwa-session-mail-notification-timer session)))
        (setf (niwa-session-publication-preparation-token session) nil)
        (niwa-agent-control--cancel-notifications session))))

  :doc "stopping retains offers without timers and immediate callback failure is diagnostic"
  (let ((session (niwa-agent-control-test--session)) calls diagnostics)
    (setf (niwa-session-publication-stopping-p session) t)
    (niwa-agent-control-notify session 'stopped (lambda () (push 'stopped calls)))
    (should-not calls)
    (should-not (niwa-session-mail-notification-timer session))
    (should (= 1 (length (niwa-session-mail-notifications session))))
    (niwa-agent-control--cancel-notifications session)
    (setf (niwa-session-publication-stopping-p session) nil)
    (niwa-test--with-captured-diagnostics diagnostics
      (niwa-agent-control-notify
       session 'failure (lambda () (error "Injected notification failure"))))
    (should (string-match-p "Mailbox notification failed: Injected notification failure"
                            diagnostics))
    (niwa-agent-control-notify session 'next (lambda () (push 'next calls)))
    (should (equal '(next) calls))))

(niwa-deftest niwa-agent-control--offer-at-gate-open ()
  ,test
  (test)
  :doc "pending offers run in FIFO order at once despite a queued successor"
  (let ((session (niwa-agent-control-test--session)) calls)
    (with-temp-buffer
      (niwa-session-set-root-buffer session (current-buffer))
      (setf (niwa-session-publication-preparation-token session) '(owner))
      (unwind-protect
          (progn
            (niwa-agent-control-notify session 'first (lambda () (push 'first calls)))
            (niwa-agent-control-notify session 'second (lambda () (push 'second calls)))
            (let ((timer (niwa-session-mail-notification-timer session)))
              (should (timerp timer))
              ;; The gate opens with another preparation still queued.
              (setf (niwa-session-publication-preparation-token session) nil
                    (niwa-session-publication-preparations session)
                    (list (cons #'ignore nil)))
              (should-not calls)
              (niwa-agent-control--offer-at-gate-open session)
              (should (equal '(first second) (reverse calls)))
              (should-not (memq timer timer-list))
              (should-not (niwa-session-mail-notifications session))
              (should-not (niwa-session-mail-notification-timer session))))
        (setf (niwa-session-publication-preparation-token session) nil
              (niwa-session-publication-preparations session) nil)
        (niwa-agent-control--cancel-notifications session))))
  :doc "without pending offers it runs nothing and arms no timer"
  (let ((session (niwa-agent-control-test--session))
        (timers (copy-sequence timer-list)))
    (with-temp-buffer
      (niwa-session-set-root-buffer session (current-buffer))
      (niwa-agent-control--offer-at-gate-open session)
      (should-not (niwa-session-mail-notification-timer session))
      (should-not (seq-difference timer-list timers)))))

(niwa-deftest niwa-agent-control-send-message ()
  ,test
  (test)
  :doc "working_directory moves a retained agent before delivery and validates first"
  (let* ((base (file-name-as-directory
                (file-truename (make-temp-file "niwa-send-directory-" t))))
         (next (file-name-as-directory (file-name-concat base "next")))
         (session (niwa-session-create
                   "main"
                   (niwa-workspace--create
                    :type 'file :id "send-directory" :root base
                    :name "send-directory")))
         (record (niwa-agent-record--create
                  :path "/root/reviewer" :parent-path "/root" :activity 'running
                  :working-directory (file-name-concat base "removed/")))
         queued)
    (make-directory next)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/reviewer" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control--enqueue)
                   (lambda (_session recipient &rest _) (push recipient queued))))
          (should-error
           (niwa-agent-control-send-message
            session "/root/reviewer" "Review" :working-directory "absent")
           :type 'user-error)
          (should-not queued)
          (should (equal (file-name-concat base "removed/")
                         (niwa-agent-record-working-directory record)))
          (should-error
           (niwa-agent-control-send-message
            session "/root" "Review" :working-directory next)
           :type 'user-error)
          (should-not queued)
          ;; A delivery that fails after validation leaves the agent in place.
          (let (persisted)
            (cl-letf (((symbol-function 'niwa-agent-control--enqueue)
                       (lambda (&rest _) (error "Commit failed")))
                      ((symbol-function 'niwa-agent-control--persist-session)
                       (lambda (_) (setq persisted t))))
              (should-error
               (niwa-agent-control-send-message
                session "/root/reviewer" "Review" :working-directory next)))
            (should persisted))
          (should (equal (file-name-concat base "removed/")
                         (niwa-agent-record-working-directory record)))
          (should (equal '(:path "/root/reviewer" :delivery queued)
                         (niwa-agent-control-send-message
                          session "/root/reviewer" "Review"
                          :working-directory next)))
          (should (equal '("/root/reviewer") queued))
          (should (equal next (niwa-agent-record-working-directory record))))
      (delete-directory base t)))

  :doc "committed agent mail wakes the root but self-mail and child mail do not"
  (let* ((session (niwa-agent-control-test--session))
         (sender (niwa-agent-record--create
                  :path "/root/sender" :activity 'running))
         (child (niwa-agent-record--create
                 :path "/root/child" :activity 'running))
         (invocation (niwa-agent-invocation--create
                      :path "/root/sender" :parent-session session))
         committed offered)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/sender" sender) (cons "/root/child" child)))
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_) (setq committed t)))
              ((symbol-function 'niwa-pending-inputs-offer-root-event)
               (lambda (value &optional _buffer)
                 (should committed)
                 (should (eq session value))
                 (push (niwa-agent-control-context-mailbox session) offered))))
      (let ((niwa--agent-invocation invocation))
        (niwa-agent-control-send-message session "/root" "report")
        (should (niwa-agent-control-wake-pending-p session))
        (should (= 1 (length offered)))
        (niwa-agent-control-clear-context-mailbox session)
        (niwa-agent-control-send-message session "/root/child" "child mail"))
      (niwa-agent-control-send-message session "/root" "self mail")
      (should-not (niwa-agent-control-wake-pending-p session))
      (should (plist-get (car (niwa-agent-record-mailbox child)) :wake))
      (should (= 1 (length offered)))))

  :doc "marks before releasing WaitAgent and does not reoffer consumed mail"
  (let* ((session (niwa-agent-control-test--session))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :path "/root/sender" :parent-session session))
         (calls 0) offered)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/sender"
                      (niwa-agent-record--create :path "/root/sender")))
          (niwa-session-agent-root-waiter session)
          (niwa-agent-waiter--create
           :callback
           (lambda (reason)
             (cl-incf calls)
             (should (eq 'mailbox reason))
             (should (niwa-agent-control-wake-pending-p session))
             (niwa-agent-control-acknowledge-mail
              session (niwa-agent-control-context-mailbox session)))))
    (cl-letf (((symbol-function 'niwa-pending-inputs-offer-root-event)
               (lambda (&rest _) (setq offered t))))
      (niwa-agent-control-send-message session "/root" "report"))
    (should (= 1 calls))
    (should-not offered)
    (should-not (niwa-agent-control-wake-pending-p session))
    (should-not (niwa-session-agent-root-waiter session)))

  :doc "a failed root mail commit rolls back without marking or offering a wake"
  (let* ((session (niwa-agent-control-test--session))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :path "/root/sender" :parent-session session))
         offered)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/sender" (niwa-agent-record--create :path "/root/sender"))))
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_) (error "Commit failed")))
              ((symbol-function 'niwa-pending-inputs-offer-root-event)
               (lambda (&rest _) (setq offered t))))
      (should-error (niwa-agent-control-send-message session "/root" "report")))
    (should-not (niwa-agent-control-context-mailbox session))
    (should-not offered))

  :doc "queues FIFO MAIL that wakes idle agents waiting for a slot"
  (let* ((session (niwa-agent-control-test--session))
         (idle (niwa-agent-record--create
                :id "idle-id" :path "/root/idle" :activity 'idle))
         (running (niwa-agent-record--create
                   :id "running-id" :path "/root/running"
                   :activity 'running)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/idle" idle)
                (cons "/root/running" running)))
    ;; The running agent fills the only slot.
    (setf (niwa-session-agent-turn-capacity session) 1)
    (should (equal '(:path "/root/idle" :delivery waiting)
                   (niwa-agent-control-send-message
                    session "/root/idle" "first")))
    (niwa-agent-control-send-message session "/root/idle" "second")
    (should (equal '(:path "/root/running" :delivery queued)
                   (niwa-agent-control-send-message
                    session "/root/running" "active")))
    (should (eq 'idle (niwa-agent-record-activity idle)))
    (should (eq 'running (niwa-agent-record-activity running)))
    (should
     (equal
      '((:type MAIL :sender "/root" :recipient "/root/idle"
         :payload "first" :wake t)
        (:type MAIL :sender "/root" :recipient "/root/idle"
         :payload "second" :wake t))
      (mapcar
       (lambda (record)
         (should (stringp (plist-get record :id)))
         (cl-loop for (key value) on record by #'cddr
                  unless (memq key '(:timestamp :id))
                  append (list key value)))
       (niwa-agent-control--mailbox session "/root/idle"))))
    (should (= 1 (length
                  (niwa-agent-control--mailbox
                   session "/root/running")))))

  :doc "a Plan read-only caller cannot reach an unstamped running turn"
  (let* ((session (niwa-agent-control-test--session))
         (idle (niwa-agent-record--create
                :id "idle-id" :path "/root/idle" :activity 'idle))
         (plain (niwa-agent-record--create
                 :id "plain-id" :path "/root/plain" :activity 'running
                 :invocation (niwa-agent-invocation--create)))
         (planning (niwa-agent-record--create
                    :id "planning-id" :path "/root/planning"
                    :activity 'running
                    :invocation (niwa-agent-invocation--create
                                 :plan-read-only t)))
         (sender (niwa-agent-record--create
                  :id "sender-id" :path "/root/sender" :activity 'running))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :agent-id "sender-id" :path "/root/sender"
           :parent-session session :plan-read-only t)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/idle" idle)
                (cons "/root/plain" plain)
                (cons "/root/planning" planning)
                (cons "/root/sender" sender)))
    (setf (niwa-session-agent-turn-capacity session) 3)
    (should-error
     (niwa-agent-control-send-message session "/root/plain" "Edit it.")
     :type 'user-error)
    (should-not (niwa-agent-control--mailbox session "/root/plain"))
    (should (equal '(:path "/root/planning" :delivery queued)
                   (niwa-agent-control-send-message
                    session "/root/planning" "Check the tests too.")))
    (should (equal '(:path "/root" :delivery queued)
                   (niwa-agent-control-send-message
                    session "/root" "Found the entry point.")))
    ;; An idle agent waiting for a slot keeps the sender's Plan limits.
    (should (equal '(:path "/root/idle" :delivery waiting)
                   (niwa-agent-control-send-message
                    session "/root/idle" "Inspect it.")))
    (should (plist-get (car (niwa-agent-record-mailbox idle))
                       :plan-read-only))
    (should (= 1 (length (niwa-agent-control--mailbox session "/root")))))

  :doc "accepts tree-wide peer paths and rejects empty or unknown targets"
  (let* ((session (niwa-agent-control-test--session))
         (sender (niwa-agent-record--create
                  :id "sender-id" :path "/root/sender" :activity 'running))
         (peer (niwa-agent-record--create
                :id "peer-id" :path "/root/peer" :activity 'running))
         (niwa--agent-invocation
          (niwa-agent-invocation--create
           :agent-id "sender-id" :path "/root/sender")))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/sender" sender)
                (cons "/root/peer" peer)))
    (niwa-agent-control-send-message session "/root/peer" "hello")
    (let ((record (car (niwa-agent-control--mailbox
                        session "/root/peer"))))
      (should (equal "/root/sender" (plist-get record :sender))))
    (dolist (args '(("/root/peer" "") ("" "body")
                    ("/root/missing" "body")))
      (should-error
       (niwa-agent-control-send-message
        session (car args) (cadr args)))))

  :doc "commit failure restores the queue and does not wake its consumer"
  (let* ((session (niwa-agent-control-test--session))
         (record (niwa-agent-record--create
                  :id "worker" :path "/root/worker" :activity 'running))
         reasons)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record))
          (niwa-session-save-path session) temporary-file-directory
          (niwa-agent-record-waiter record)
          (niwa-agent-waiter--create
           :callback (lambda (reason) (push reason reasons))))
    (dolist (failure '(nil error))
      (cl-letf (((symbol-function
                  'niwa-session-persistence-save-agent-state)
                 (if (eq failure 'error)
                     (lambda (&rest _) (error "Injected commit failure"))
                   (lambda (&rest _) nil))))
        (should-error
         (niwa-agent-control-send-message
          session "/root/worker" "must persist")))
      (should-not (niwa-agent-record-mailbox record))
      (should-not reasons)
      (should (niwa-agent-record-waiter record))))

  :doc "starts an idle agent's turn with the message as its prompt"
  (let* ((session (niwa-agent-control-test--session))
         (buffer (generate-new-buffer " *agent-control-send-start*"))
         (record
          (niwa-agent-record--create
           :id "worker--retained" :path "/root/worker" :parent-path "/root"
           :role "worker" :configuration (niwa-agent-control-test--configuration)
           :activity 'idle :conversation-buffer buffer
           :conversation-location "agents/worker.chat.org"))
         seen)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-runtime-dispatch)
                   (lambda (_agent _description prompt &rest keys)
                     (setq seen (list prompt (plist-get keys :parent-tool-use-id)
                                      (plist-get keys :plan-read-only)))
                     (funcall (plist-get keys :on-invocation)
                              (niwa-agent-invocation--create
                               :agent-id "worker--retained" :path "/root/worker"
                               :frozen-configuration
                               (plist-get keys :frozen-configuration)))
                     t)))
          (should (equal '(:path "/root/worker" :delivery started)
                         (niwa-agent-control-send-message
                          session "worker" "Review the diff."
                          :parent-tool-use-id "call_send"))))
      (kill-buffer buffer))
    (should (equal '("Review the diff." "call_send" nil) seen))
    (should (eq 'running (niwa-agent-record-activity record)))
    (should-not (niwa-agent-record-mailbox record)))

  :doc "mail sent while a settlement runs waits for the offer after it"
  (let* ((session (niwa-agent-control-test--session))
         (record (niwa-agent-record--create
                  :path "/root/work" :parent-path "/root" :activity 'idle))
         ;; The settling agent already reads idle.
         (niwa-agent-control-settling
          (list (niwa-agent-invocation--create :parent-session session))))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/work" record)))
    (should (equal '(:path "/root/work" :delivery waiting)
                   (niwa-agent-control-send-message
                    session "/root/work" "next")))
    (should (eq 'idle (niwa-agent-record-activity record)))
    (should (plist-get (car (niwa-agent-record-mailbox record)) :wake)))

  :doc "attributes peer mail while returning results to the spawn parent"
  (let* ((session (niwa-agent-control-test--session))
         (peer-invocation (niwa-agent-invocation-create
                           (niwa-agent-default)))
         (target-invocation (niwa-agent-invocation-create
                             (niwa-agent-default)))
         (peer (niwa-agent-record--create
                :id "default--peer" :path "/root/peer"
                :parent-path "/root" :activity 'running
                :invocation peer-invocation))
         (target (niwa-agent-record--create
                  :id "default--target" :path "/root/target"
                  :parent-path "/root" :activity 'running
                  :invocation target-invocation))
         (buffer (generate-new-buffer " *agent-control-peer-mail*")))
    (setf (niwa-agent-invocation-agent-id peer-invocation)
          "default--peer")
    (setf (niwa-agent-invocation-path peer-invocation) "/root/peer")
    (setf (niwa-agent-invocation-path target-invocation) "/root/target")
    (setf (niwa-agent-invocation-transcript-status target-invocation)
          'completed)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/peer" peer)
                (cons "/root/target" target)))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--agent-invocation peer-invocation)
          (niwa-agent-control-send-message
           session "/root/target" "Review the peer's update.")
          (let ((mail (car (niwa-agent-record-mailbox target))))
            (should (equal "/root/peer" (plist-get mail :sender)))
            (should (equal "Review the peer's update."
                           (plist-get mail :payload))))
          (niwa-agent-control-test--settle
           session target target-invocation "Peer review complete."))
      (kill-buffer buffer))
    (let ((result (car (niwa-session-messages session))))
      (should (equal "/root/target" (plist-get result :sender)))
      (should (equal "/root" (plist-get result :recipient))))))

(niwa-deftest niwa-agent-control-wait ()
  ,test
  (test)
  :doc "releases immediately when mail was queued before the wait"
  (let ((session (niwa-agent-control-test--session))
        reasons)
    (niwa-agent-control-send-message session "/root" "ready")
    (niwa-agent-control-wait
     session (lambda (reason) (push reason reasons)) nil)
    (should (equal '(mailbox) reasons))
    (should-not (niwa-session-agent-root-waiter session)))

  :doc "releases exactly once when mail arrives after the wait starts"
  (let ((session (niwa-agent-control-test--session))
        reasons scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))
                 'fake-timer)))
      (niwa-agent-control-wait
       session (lambda (reason) (push reason reasons)) 10000)
      (should (niwa-session-agent-root-waiter session))
      (niwa-agent-control-send-message session "/root" "arrived")
      (should (equal '(mailbox) reasons))
      (apply (car scheduled) (cdr scheduled))
      (should (equal '(mailbox) reasons))))

  :doc "a wait started inside a remote command keeps its timeout"
  (let ((session (niwa-agent-control-test--session))
        timer)
    (niwa-transport--handler-advice
     (lambda ()
       (let (timer-list timer-idle-list)
         (niwa-agent-control-wait session #'ignore 10000))))
    (setq timer (niwa-agent-waiter-timer
                 (niwa-session-agent-root-waiter session)))
    (should (memq timer timer-list))
    (niwa-agent-control-cancel-wait session "/root")
    (should-not (memq timer timer-list)))

  :doc "a failed callback releases its wait without losing the triggering mail"
  (let ((session (niwa-agent-control-test--session))
        (attempts 0) diagnostics)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait
       session
       (lambda (_reason)
         (cl-incf attempts)
         (when (= attempts 1)
           (error "Injected delivery failure")))
       10000)
      (niwa-test--with-captured-diagnostics diagnostics
        (should (equal '(:path "/root" :delivery queued)
                       (niwa-agent-control-send-message session "/root" "arrived"))))
      (should (string-match-p "Mailbox notification failed: Injected delivery failure"
                              diagnostics))
      (should (= 1 attempts))
      (should-not (niwa-session-agent-root-waiter session))
      (should (= 1 (length (niwa-agent-control--mailbox session "/root"))))
      (niwa-agent-control-wait
       session
       (lambda (reason)
         (should (eq 'mailbox reason))
         (should (equal "arrived"
                        (plist-get (car (niwa-agent-control--mailbox session "/root"))
                                   :payload)))
         (cl-incf attempts))
       10000)
      (should (= 2 attempts))
      (should-not (niwa-session-agent-root-waiter session))))

  :doc "a synchronous callback may start a successor wait without losing it"
  (let ((session (niwa-agent-control-test--session))
        first successor reasons)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait
       session
       (lambda (reason)
         (push reason reasons)
         (niwa-agent-control-wait
          session (lambda (next) (push next reasons)) 10000)
         (setq successor (niwa-session-agent-root-waiter session)))
       10000)
      (setq first (niwa-session-agent-root-waiter session))
      (should (niwa-agent-control--wake session "/root" 'timeout))
      (should successor)
      (should-not (eq first successor))
      (should (eq successor (niwa-session-agent-root-waiter session)))
      (niwa-agent-control--wait-timeout session "/root" first)
      (should (equal '(timeout) reasons))
      (should (niwa-agent-control--wake session "/root" 'user))
      (should (equal '(user timeout) reasons))
      (should-not (niwa-session-agent-root-waiter session))))

  :doc "times out and clamps or defaults out-of-range timeouts"
  (let ((session (niwa-agent-control-test--session))
        reasons scheduled delay)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seen-delay _repeat function &rest args)
                 (setq delay seen-delay
                       scheduled (cons function args))
                 'fake-timer)))
      (niwa-agent-control-wait
       session (lambda (reason) (push reason reasons)) nil)
      (should (= 30 delay))
      (apply (car scheduled) (cdr scheduled))
      (should (equal '(timeout) reasons)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seen-delay &rest _)
                 (setq delay seen-delay)
                 'fake-timer)))
      (niwa-agent-control-wait session #'ignore 1)
      (should (= 10 delay))
      (niwa-agent-control-cancel-wait session "/root")
      ;; Out-of-range values clamp, coercible values coerce, and
      ;; garbage falls back to the default instead of erroring.
      (dolist (case '((0 . 10) (-1 . 10) (3600001 . 3600) (10000.0 . 10)
                      ("10000" . 10) ("nonsense" . 30) (t . 30)))
        (niwa-agent-control-wait session #'ignore (car case))
        (should (= (cdr case) delay))
        (niwa-agent-control-cancel-wait session "/root")))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait session #'ignore 10000)
      (niwa-agent-control-cancel-wait session "/root")
      (niwa-agent-control-wait session #'ignore 3600000)
      (niwa-agent-control-cancel-wait session "/root")))

  :doc "new root user steering is queued for injection and interrupts the wait"
  (let ((session (niwa-agent-control-test--session))
        reasons)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait
       session (lambda (reason) (push reason reasons)) 10000)
      (should (niwa-agent-control-steer-user session "new direction"))
      (should (equal '(user) reasons))
      (let ((message (car (niwa-agent-control--mailbox
                           session "/root"))))
        (should (eq 'USER (plist-get message :type)))
        (should (equal "new direction" (plist-get message :payload))))
      (should-not
       (niwa-agent-control-steer-user session "too late"))))

  :doc "yielded Bash completion wakes a root wait through mailbox notification"
  (let ((session (niwa-agent-control-test--session))
        reason)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait
       session (lambda (seen) (setq reason seen)) 10000)
      (niwa-agent-control-enqueue-execution-result
       session "/root" "root execution complete")
      (should (eq 'mailbox reason))))

  :doc "a suspended non-root caller retains its active-turn slot"
  (let* ((session (niwa-agent-control-test--session))
         (invocation
          (niwa-agent-invocation--create
           :agent-id "worker-id" :path "/root/worker"
           :parent-session session))
         (record
          (niwa-agent-record--create
           :id "worker-id" :path "/root/worker" :activity 'running
           :invocation invocation))
         reason)
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (let ((niwa--agent-invocation invocation))
        (niwa-agent-control-wait
         session (lambda (seen) (setq reason seen)) 10000))
      (should (eq 'waiting (niwa-agent-record-activity record)))
      (should (= 1 (niwa-agent-control--active-count session)))
      (niwa-agent-control-send-message session "/root/worker" "wake")
      (should (eq 'mailbox reason))
      (should (eq 'running (niwa-agent-record-activity record)))
      (should (= 1 (niwa-agent-control--active-count session))))))

(niwa-deftest niwa-agent-control-enqueue-execution-result ()
  ,test
  (test)
  :doc "queues a distinct root execution record and acknowledges delivery"
  (let ((session (niwa-agent-control-test--session)))
    (should
     (niwa-agent-control-enqueue-execution-result
      session "/root" "root execution complete"))
    (let ((message (car (niwa-agent-control-context-mailbox session))))
      (should (eq 'EXECUTION (plist-get message :type)))
      (should (equal "/root" (plist-get message :sender)))
      (should (equal "root execution complete"
                     (plist-get message :payload)))
      (should-not (plist-get message :wake))))
  :doc "carries the execution identity and a runtime wake mark"
  (let ((session (niwa-agent-control-test--session)))
    (niwa-agent-control-enqueue-execution-result
     session "/root" "done" "exec-1" t)
    (let ((message (car (niwa-agent-control-context-mailbox session))))
      (should (equal "exec-1" (plist-get message :execution-id)))
      (should (plist-get message :wake))))
  :doc "a failed save leaves no record behind"
  (let ((session (niwa-agent-control-test--session)))
    (cl-letf (((symbol-function 'niwa-agent-control-commit-session)
               (lambda (_session) (error "Disk full"))))
      (should-error (niwa-agent-control-enqueue-execution-result
                     session "/root" "done" "exec-1" t)))
    (should-not (niwa-agent-control-context-mailbox session))
    (should-not (niwa-agent-control-wake-pending-p session))))

(niwa-deftest niwa-agent-control-enqueue-monitor-events ()
  ,test
  (test)
  :doc "queues a root MONITOR record with its watch identity"
  (let ((session (niwa-agent-control-test--session)))
    (should (niwa-agent-control-enqueue-monitor-events
             session "exec-1" "CI watch" '(("line one\n")) t))
    (let ((message (car (niwa-agent-control-context-mailbox session))))
      (should (eq 'MONITOR (plist-get message :type)))
      (should (equal "/root" (plist-get message :recipient)))
      (should (equal "exec-1" (plist-get message :execution-id)))
      (should (equal "CI watch" (plist-get message :description)))
      (should (plist-get message :wake))))
  :doc "rejects records without identity"
  (should-error (niwa-agent-control-enqueue-monitor-events
                 (niwa-agent-control-test--session) nil "d" '(("x")) t))
  :doc "queues several entries in order, the terminal as its own type"
  (let ((session (niwa-agent-control-test--session)))
    (niwa-agent-control-enqueue-monitor-events
     session "exec-1" "w"
     '(("a") ("b" (:omitted 2)) ("done" (:termination "exited") t)) nil)
    (let ((messages (niwa-agent-control-context-mailbox session)))
      (should (equal '(MONITOR MONITOR MONITOR_TERMINAL)
                     (mapcar (lambda (m) (plist-get m :type)) messages)))
      (should (equal '("a" "b" "done")
                     (mapcar (lambda (m) (plist-get m :payload)) messages)))
      (should (equal '(:omitted 2) (plist-get (cadr messages) :meta))))))

(niwa-deftest niwa-agent-control-wake-pending-p ()
  ,test
  (test)
  :doc "is true only while an unread root record carries a wake mark"
  (let ((session (niwa-agent-control-test--session)))
    (should-not (niwa-agent-control-wake-pending-p session))
    (niwa-agent-control-enqueue-execution-result session "/root" "quiet")
    (should-not (niwa-agent-control-wake-pending-p session))
    (niwa-agent-control-enqueue-execution-result
     session "/root" "done" "exec-1" t)
    (should (eq t (niwa-agent-control-wake-pending-p session)))
    ;; Reading the mailbox at the next step consumes it.
    (niwa-agent-control-clear-context-mailbox session)
    (should-not (niwa-agent-control-wake-pending-p session)))
  :doc "a nil session has nothing pending"
  (should-not (niwa-agent-control-wake-pending-p nil)))

(niwa-deftest niwa-agent-control-clear-wake ()
  ,test
  (test)
  :doc "clears only the named root execution's wake and keeps its mail"
  (let ((session (niwa-agent-control-test--session)))
    (niwa-agent-control-enqueue-execution-result
     session "/root" "one" "exec-1" t)
    (niwa-agent-control-enqueue-execution-result
     session "/root" "two" "exec-2" t)
    (niwa-agent-control-clear-wake session "/root" "exec-1")
    (should (= 2 (length (niwa-agent-control-context-mailbox session))))
    (should (niwa-agent-control-wake-pending-p session))
    (niwa-agent-control-clear-wake session "/root" "exec-2")
    (should-not (niwa-agent-control-wake-pending-p session)))
  :doc "ignores agent owners"
  (let ((session (niwa-agent-control-test--session)))
    (niwa-agent-control-enqueue-execution-result
     session "/root" "one" "exec-1" t)
    (niwa-agent-control-clear-wake session "/root/child" "exec-1")
    (should (niwa-agent-control-wake-pending-p session))))

(niwa-deftest niwa-agent-control-monitor-unread-bytes ()
  ,test
  (test)
  :doc "sums only the named monitor's unread payload bytes"
  (let ((session (niwa-agent-control-test--session)))
    (should (= 0 (niwa-agent-control-monitor-unread-bytes session "exec-1")))
    (niwa-agent-control-enqueue-monitor-events session "exec-1" "d" '(("abc")) t)
    (niwa-agent-control-enqueue-monitor-events session "exec-1" "d" '(("é")) t)
    (niwa-agent-control-enqueue-monitor-events session "exec-2" "d" '(("zzzz")) t)
    (should (= 5 (niwa-agent-control-monitor-unread-bytes session "exec-1")))))

(niwa-deftest niwa-agent-control-wake-root-user ()
  ,test
  (test)
  :doc "wakes WaitAgent without creating a mailbox message"
  (let ((session (niwa-agent-control-test--session))
        reason)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _) 'fake-timer)))
      (niwa-agent-control-wait
       session (lambda (seen) (setq reason seen)) 10000)
      (niwa-agent-control-wake-root-user session))
    (should (eq 'user reason))
    (should-not (niwa-agent-control--mailbox session "/root"))))

(defun niwa-agent-control-test--provider-configuration (model effort)
  "Return a frozen configuration for MODEL at EFFORT on a test backend."
  (niwa-agent-configuration--create
   :agent (niwa-agent-default)
   :request-locals `((gptel-backend . ,(gptel--make-backend :name "Test Provider"))
                     (gptel-model . ,model)
                     (gptel-reasoning-effort . ,effort))))

(niwa-deftest niwa-agent-control-record-model-metadata ()
  ,test
  (test)
  :doc "a running turn reports its invocation's effective provider"
  (let ((record (niwa-agent-record--create
                 :configuration (niwa-agent-control-test--provider-configuration
                                 'requested 'high)
                 :model-metadata '(:backend "Old" :model "old" :effort nil)
                 :invocation (niwa-agent-invocation--create
                              :frozen-configuration
                              (niwa-agent-control-test--provider-configuration
                               'served nil)))))
    (should (equal '(:backend "Test Provider" :model "served" :effort nil)
                   (niwa-agent-control-record-model-metadata record))))
  :doc "an idle agent reports the last served provider, then its frozen one"
  (let ((record (niwa-agent-record--create
                 :configuration (niwa-agent-control-test--provider-configuration
                                 'requested 'high)
                 :model-metadata
                 '(:backend "Test Provider" :model "served" :effort nil))))
    (should (equal '(:backend "Test Provider" :model "served" :effort nil)
                   (niwa-agent-control-record-model-metadata record)))
    (setf (niwa-agent-record-model-metadata record) nil)
    (should (equal '(:backend "Test Provider" :model "requested" :effort "high")
                   (niwa-agent-control-record-model-metadata record))))
  :doc "reports nothing for an agent without a known provider"
  (should-not (niwa-agent-control-record-model-metadata
               (niwa-agent-record--create))))

(niwa-deftest niwa-agent-control-model-metadata ()
  ,test
  (test)
  :doc "looks up a retained agent by path and never another agent's provider"
  (let ((session (niwa-agent-control-test--session)))
    (setf (niwa-session-agent-registry session)
          (list (cons "/root/helper"
                      (niwa-agent-record--create
                       :path "/root/helper"
                       :configuration (niwa-agent-control-test--provider-configuration
                                       'helper-model 'low)))))
    (should (equal '(:backend "Test Provider" :model "helper-model" :effort "low")
                   (niwa-agent-control-model-metadata session "/root/helper")))
    (should-not (niwa-agent-control-model-metadata session "/root"))
    (should-not (niwa-agent-control-model-metadata session "/root/missing"))))

(provide 'test-niwa-agent-control)

;;; test-niwa-agent-control.el ends here
