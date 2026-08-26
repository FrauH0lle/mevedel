;;; test-mevedel-permission-queue.el -- Tests for mevedel-permission-queue -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the session permission FIFO queue.  Covers:
;; - enqueue order + head-only render
;; - coalesce on rule-creating outcomes (allow-session, always-allow,
;;   deny-session) using captured session context
;; - coalesce skip on once outcomes
;; - per-agent sweep on terminal state
;; - abort flush
;; - kind dispatch (generic / bash / eval)
;; - outcome vocabulary translation for bash adapters

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-execution-target)
(require 'mevedel-structs)
(require 'mevedel-permissions)
(require 'mevedel-permission-log)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-ui)
(require 'mevedel-tools)
(require 'mevedel-view)
(require 'mevedel-mentions)
(require 'mevedel-skills-ui)
(require 'mevedel-permission-rules)
(require 'mevedel-permission-mode)

(defun test-pq--make-session (&optional rules target)
  "Create a fresh queue-test session with optional RULES and TARGET."
  (mevedel-session--create
   :name "test"
   :workspace nil
   :execution-target target
   :permission-rules rules
   :permission-mode 'ask
   :permission-queue nil
   :pending-plan-approval nil))

(defun test-pq--read-permission-log (session)
  "Read SESSION's permission log entries."
  (let ((file (mevedel-permission-log-path session))
        entries)
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (push (read (current-buffer)) entries))
          (end-of-file nil))))
    (nreverse entries)))


;;
;;; Enqueue order + head render

(mevedel-deftest mevedel-permission--enqueue
  (:quiet t :doc "FIFO permission queue contract")
  ,test
  (test)

  :doc "enqueue keeps the visible head rendered while siblings wait"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (rendered nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Edit"
             :origin "/root" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Write"
             :origin "/root" :callback #'ignore)))
    ;; The visible head is re-rendered as siblings arrive so pending
    ;; counts stay current, but FIFO order is unchanged.
    (should (= 3 (length rendered)))
    (should (cl-every (lambda (entry)
                        (equal "Read" (plist-get entry :tool-name)))
                      rendered))
    ;; All three entries are on the queue in FIFO order.
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 3 (length q)))
      (should (equal "Read" (plist-get (nth 0 q) :tool-name)))
      (should (equal "Edit" (plist-get (nth 1 q) :tool-name)))
      (should (equal "Write" (plist-get (nth 2 q) :tool-name)))))

  :doc "entry captures :session at enqueue so settlement is buffer-independent"
  (let* ((session (test-pq--make-session))
         (mevedel--session session))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root" :callback #'ignore)))
    (let* ((q (mevedel-session-permission-queue session))
           (entry (car q)))
      (should (eq session (plist-get entry :session)))))

  :doc "no-session enqueue aborts without direct rendering"
  (let ((mevedel--session nil)
        (rendered nil)
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (entry) (push entry rendered))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root"
             :callback (lambda (o) (setq outcome o)))))
    (should (null rendered))
    (should (eq 'aborted outcome)))

  :doc "enqueue rejects a missing origin"
  (let ((session (test-pq--make-session)))
    (should-error
     (mevedel-permission--enqueue
      (list :kind 'generic :tool-name "Read" :callback #'ignore)
      session)
     :type 'error))

  :doc "enqueue rejects malformed canonical paths"
  (let ((session (test-pq--make-session)))
    (dolist (origin '(nil "" "root" "/root/" "/root/Bad"
                          "/root/explorer-agent" "explorer--abc"))
      (should-error
       (mevedel-permission--enqueue
        (list :kind 'generic :tool-name "Read"
              :origin origin :callback #'ignore)
        session)
       :type 'error)))

  :doc "marks retained requesters blocked until their final queued permission settles"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (invocation
          (mevedel-agent-invocation--create :path "/root/worker"))
         (record
          (mevedel-agent-record--create
           :path "/root/worker" :activity 'running
           :invocation invocation)))
    (setf (mevedel-session-agent-registry session)
          (list (cons "/root/worker" record)))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
               (lambda (&rest _args) nil)))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root/worker" :callback #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Edit"
             :origin "/root/worker" :callback #'ignore))
      (should (eq 'permission-blocked
                  (mevedel-agent-record-activity record)))
      (mevedel-permission-queue--on-head-outcome
       (car (mevedel-session-permission-queue session)) 'allow-once)
      (should (eq 'permission-blocked
                  (mevedel-agent-record-activity record)))
      (mevedel-permission-queue--on-head-outcome
       (car (mevedel-session-permission-queue session)) 'allow-once)
      (should (eq 'running (mevedel-agent-record-activity record))))))


;;
;;; Permission diagnostics

(mevedel-deftest mevedel-permission-log
  ()
  ,test
  (test)
  :doc "buffers diagnostics until the session is materialized"
  (let ((session (test-pq--make-session))
        (mevedel-permission-log-enabled t))
    (mevedel-permission-log session 'permission-decision :tool-name "Read")
    (should (= 1 (length (mevedel-session-permission-log-pending session))))
    (should-not (mevedel-permission-log-path session)))

  :doc "retains a materialized diagnostic when persistence fails"
  (let* ((root (make-temp-file "mevedel-permission-log-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (session (test-pq--make-session))
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (mevedel-session-save-path session) blocked)
          (let (diagnostics)
            (mevedel-test--with-captured-diagnostics diagnostics
              (mevedel-permission-log
               session 'permission-decision :tool-name "Read"))
            (should (string-match-p "persistence failed" diagnostics)))
          (should (= 1
                     (length
                      (mevedel-session-permission-log-pending session)))))
      (delete-directory root t)))

  :doc "defers remote diagnostics and combines one serialized append"
  (let* ((root (make-temp-file "mevedel-permission-log-remote-" t))
         (target
          (mevedel-execution-target-create
           "/ssh:permission-host:/workspace/"))
         (session (test-pq--make-session nil target))
         (mevedel-permission-log-enabled t)
         calls)
    (setf (mevedel-session-save-path session) root)
    (unwind-protect
        (cl-letf
            (((symbol-function
               'mevedel-session-publication-append-diagnostic)
              (lambda (_session path content)
                (push (list path content) calls)
                t))
             ((symbol-function 'mevedel-telemetry-record) #'ignore))
          (mevedel-permission-log
           session 'permission-enqueued :tool-name "Read")
          (mevedel-permission-log
           session 'permission-resolved :tool-name "Read")
          (should (= 2 (length
                        (mevedel-session-permission-log-pending session))))
          (should-not calls)
          (mevedel-permission-log-flush session)
          (should-not (mevedel-session-permission-log-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should
             (equal (file-name-concat root "permission-log.el") path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (eq 'permission-enqueued
                          (plist-get (read (current-buffer)) :event)))
              (should (eq 'permission-resolved
                          (plist-get (read (current-buffer)) :event))))))
      (delete-directory root t)))

  :doc "side queue lifecycle forwards only whitelisted durable audit fields"
  (let* ((parent (test-pq--make-session))
         (side (test-pq--make-session))
         calls)
    (setf (mevedel-session-audit-session side) parent)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore)
              ((symbol-function 'mevedel-telemetry-record)
               (lambda (session event &rest props)
                 (push (list session event props) calls))))
      (mevedel-permission--enqueue
       (list :kind 'sandbox :tool-name "Bash"
             :specifier-key :path :specifier-value "/private/path"
             :resource-path "/private/resource"
             :resource-access 'write :protected-path t
             :additional-permissions '(:network t)
             :justification "private justification"
             :origin "/root" :callback #'ignore)
       side))
    (let ((call
           (cl-find-if
            (lambda (entry)
              (and (eq parent (car entry))
                   (eq 'permission-enqueued (cadr entry))))
            calls)))
      (should call)
      (let ((props (nth 2 call)))
        (should (eq 'btw (plist-get props :conversation-scope)))
        (should (eq 'sandbox (plist-get props :kind)))
        (should (eq :path (plist-get props :specifier-key)))
        (should (eq 'write (plist-get props :resource-access)))
        (dolist (key '(:specifier-value :resource-path
                       :additional-permissions :justification))
          (should-not (plist-member props key)))
        (should-not
         (string-match-p
          (regexp-opt '("/private/path" "/private/resource"
                        "private justification"))
          (prin1-to-string props))))))

  :doc "permission queue writes enqueue and resolve diagnostics"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t)
         outcome)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'generic
                   :tool-name "Read"
                   :specifier-key :path
                   :specifier-value "/tmp/a.el"
                   :origin "/root/verifier"
                   :callback (lambda (o) (setq outcome o)))
             session))
          (let ((entry (car (mevedel-session-permission-queue session))))
            (mevedel-permission-queue--on-head-outcome entry 'deny-once))
          (should (eq 'deny-once outcome))
          (let ((entries (test-pq--read-permission-log session)))
            (should (= 2 (length entries)))
            (should (eq 'permission-enqueued
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'permission-resolved
                        (plist-get (nth 1 entries) :event)))
            (should (equal "Read" (plist-get (nth 0 entries) :tool-name)))
            (should (equal "/root/verifier"
                           (plist-get (nth 0 entries) :origin)))
            (should (eq 'deny-once
                        (plist-get (nth 1 entries) :outcome)))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "Bash lifecycle diagnostics omit raw command payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'bash
                   :command "printf SECRET_TOKEN"
                   :commands-summary "printf"
                   :command-class 'unknown
                   :origin "/root"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (equal "printf" (plist-get entry :commands-summary)))
            (should-not (plist-member entry :command))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "Eval lifecycle diagnostics omit raw expression payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'eval
                   :expression "(message \"SECRET_TOKEN\")"
                   :mode "live"
                   :origin "/root"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (equal "live" (plist-get entry :mode)))
            (should-not (plist-member entry :expression))))
      (when (file-directory-p dir)
        (delete-directory dir t))))
  :doc "network escalation records authority without the command payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) dir)
          (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
                     #'ignore))
            (mevedel-permission--enqueue
             (list :kind 'sandbox
                   :tool-name "Bash"
                   :detail "curl https://secret.example"
                   :sandbox-permissions 'additive
                   :additional-permissions '(:network t)
                   :justification "Download the requested page?"
                   :origin "/root"
                   :callback #'ignore)
             session))
          (let ((entry (car (test-pq--read-permission-log session))))
            (should (eq 'permission-enqueued (plist-get entry :event)))
            (should (eq 'additive
                        (plist-get entry :sandbox-permissions)))
            (should (equal '(:network t)
                           (plist-get entry :additional-permissions)))
            (should (equal "Download the requested page?"
                           (plist-get entry :justification)))
            (should-not (plist-member entry :detail))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(mevedel-deftest mevedel-permission-log-flush
  (:quiet t :doc "retains failed queued diagnostics and clears them after retry")
  (let* ((root (make-temp-file "mevedel-permission-flush-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (test-pq--make-session))
         (entry '(:event permission-decision :time "now"))
         later-entry
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (mevedel-session-save-path session) blocked
                (mevedel-session-permission-log-pending session)
                (list entry))
          (mevedel-permission-log-flush session)
          (should (equal (list entry)
                         (mevedel-session-permission-log-pending session)))
          (setf (mevedel-session-save-path session) restored)
          (cl-letf (((symbol-function 'mevedel-telemetry-record) #'ignore))
            (mevedel-permission-log session 'permission-resolved))
          (setq later-entry
                (cadr (mevedel-session-permission-log-pending session)))
          (should (= 2 (length
                        (mevedel-session-permission-log-pending session))))
          (mevedel-permission-log-flush session)
          (should-not (mevedel-session-permission-log-pending session))
          (should (equal (list entry later-entry)
                         (test-pq--read-permission-log session))))
      (delete-directory root t))))


;;
;;; Coalesce: rule-creating outcomes

(mevedel-deftest mevedel-permission-queue--coalesce
  (:doc "coalesce re-evaluates queued siblings against the just-created rule")
  ,test
  (test)

  :doc "allow-session coalesces queued path siblings via a resource grant"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (resolved-outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      ;; Enqueue two identical Read requests.
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "Read1" o) resolved-outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "Read2" o) resolved-outcomes))))
      ;; User answers allow-session for the head.  Simulate the exact
      ;; resource grant written by the normal permission prompt.
      (mevedel-permission-add-session-resource-grant
       session "/foo/bar.el" 'read)
      ;; Coalesce should resolve the queued sibling as 'allow.
      (mevedel-permission-queue--coalesce 'allow-session session))
    ;; Read2 was coalesced; Read1 (the head) wasn't touched by
    ;; --coalesce (the head's callback already fired before
    ;; --coalesce was called).
    (should (assoc "Read2" resolved-outcomes))
    (should (eq 'allow (cdr (assoc "Read2" resolved-outcomes))))
    ;; Queue is now empty (Read1 was already dropped before
    ;; --coalesce; Read2 was just resolved).
    (should (null (mevedel-session-permission-queue session))))

  :doc "deny-session coalesces queued siblings to deny"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/x.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push '("Read" :path "/x.el" :action deny)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (memq 'deny outcomes)))

  :doc "removes coalesced entries before reentrant teardown settles callbacks"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dolist (id '(first second))
        (let ((entry-id id))
          (mevedel-permission--enqueue
           (list :kind 'generic :tool-name "Read"
                 :specifier-value "/reentrant.el"
                 :resource-access 'read
                 :origin "/root"
                 :callback
                 (lambda (outcome)
                   (push (cons entry-id outcome) outcomes)
                   (when (eq entry-id 'first)
                     (mevedel-permission-queue-abort-all session)))))))
      (mevedel-permission-add-session-resource-grant
       session "/reentrant.el" 'read)
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (equal '((second . allow) (first . allow)) outcomes))
    (should-not (mevedel-session-permission-queue session)))

  :doc "queued sibling whose rule does not cover it stays queued"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      ;; Two queued entries with different paths.
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/foo.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "foo" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/bar.el"
             :resource-access 'read
             :origin "/root"
             :callback (lambda (o) (push (cons "bar" o) outcomes))))
      ;; Resource authority covers /foo.el only.
      (mevedel-permission-add-session-resource-grant
       session "/foo.el" 'read)
      (mevedel-permission-queue--coalesce 'allow-session session))
    ;; /foo.el's queued sibling resolved; /bar.el stayed.
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "/bar.el" (plist-get (car q) :specifier-value)))))

  :doc "coalesce honors non-path generic specifier keys"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "WebFetch"
             :specifier-key :domain
             :specifier-value "example.com"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push '("WebFetch" :domain "example.com" :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (memq 'allow outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "protected paths do not coalesce allow rules but do coalesce deny rules"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (mevedel-protected-paths '(("**/.git/**" . read-only)))
         (path "/repo/.git/config")
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value path
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (push `("Read" :path ,path :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session)
      (should-not outcomes)
      (should (= 2 (length (mevedel-session-permission-queue session))))
      (setf (mevedel-session-permission-rules session)
            (list `("Read" :path ,path :action deny)))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny deny) outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "rule write failure leaves queued siblings pending"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :specifier-value "/uncovered.el"
             :origin "/root"
             :callback (lambda (o) (push o outcomes))))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should-not outcomes)
    (should (= 2 (length (mevedel-session-permission-queue session)))))

  :doc "filesystem sandbox siblings coalesce through exact resource grants"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (mevedel-permission--enqueue
         (list :kind 'sandbox :tool-name "Bash"
               :resource-path "/tmp/secret"
               :resource-access 'read
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (mevedel-permission-add-session-resource-grant
       session "/tmp/secret" 'read)
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (mevedel-session-permission-queue session)))

  :doc "combined filesystem siblings wait until every exact grant exists"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes
         (missing
          '(:file-system
            ((:path "/tmp/input" :access read)
             (:path "/tmp/output" :access write)))))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (mevedel-permission--enqueue
         (list :kind 'sandbox :tool-name "Bash"
               :resource-path "/tmp/input"
               :resource-access 'read
               :missing-additional-permissions missing
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (mevedel-permission-add-session-resource-grant
       session "/tmp/input" 'read)
      (mevedel-permission-queue--coalesce 'allow-session session)
      (should-not outcomes)
      (should (= 2 (length (mevedel-session-permission-queue session))))
      (mevedel-permission-add-session-resource-grant
       session "/tmp/output" 'write)
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (mevedel-session-permission-queue session)))

  :doc "filesystem sandbox siblings coalesce through exact path denies"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (mevedel-permission--enqueue
         (list :kind 'sandbox :tool-name "Bash"
               :resource-path "/tmp/secret"
               :resource-access 'read
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Bash" :path "/tmp/secret" :action deny)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny-once deny-once) outcomes))
    (should-not (mevedel-session-permission-queue session)))

  :doc "full escalation siblings coalesce through a qualified session allow"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (mevedel-permission--enqueue
         (list :kind 'sandbox :tool-name "Bash"
               :detail "emacs --batch -Q"
               :sandbox-permissions 'require-escalated
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Bash" :pattern "emacs --batch -Q"
                     :sandbox-permissions require-escalated
                     :action allow)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'allow-session session))
    (should (equal '(allow-once allow-once) outcomes))
    (should-not (mevedel-session-permission-queue session)))

  :doc "full escalation siblings coalesce through a qualified session deny"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         outcomes)
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (dotimes (_ 2)
        (mevedel-permission--enqueue
         (list :kind 'sandbox :tool-name "Eval"
               :detail "(message \"hello\")"
               :sandbox-permissions 'require-escalated
               :origin "/root"
               :callback (lambda (outcome) (push outcome outcomes)))))
      (push '("Eval" :sandbox-permissions require-escalated :action deny)
            (mevedel-session-permission-rules session))
      (mevedel-permission-queue--coalesce 'deny-session session))
    (should (equal '(deny-once deny-once) outcomes))
    (should-not (mevedel-session-permission-queue session))))


;;
;;; Render dispatch

(mevedel-deftest mevedel-permission-queue--attribution-origin
  (:doc "attributes prompts to non-root canonical agent paths")
  ,test
  (test)

  :doc "the root path needs no attribution line"
  (should-not
   (mevedel-permission-queue--attribution-origin '(:origin "/root")))

  :doc "nested canonical paths retain attribution"
  (let* ((origin "/root/worker/verifier")
         (entry (list :origin origin)))
    (should (equal origin
                   (mevedel-permission-queue--attribution-origin entry))))

  :doc "root ToolScript calls identify the envelope and child"
  (should
   (equal "ToolScript ptc-1 (child ptc-1/2)"
          (mevedel-permission-queue--attribution-origin
           '(:origin "/root" :call-source ptc
             :tool-use-id "ptc-1/2" :parent-tool-use-id "ptc-1")))))

(mevedel-deftest mevedel-permission-queue--render-generic
  (:quiet t :doc "renders generic permission queue entries")
  ,test
  (test)

  :doc "ToolScript prompts show the envelope and child identity"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'generic :tool-name "Read"
                      :origin "/root" :call-source 'ptc
                      :tool-use-id "ptc-1/2"
                      :parent-tool-use-id "ptc-1"
                      :session session :callback #'ignore))
         attribution)
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
               (lambda (_tool _path _always origin _cont _count _entry)
                 (setq attribution origin))))
      (mevedel-permission-queue--render-generic entry))
    (should (equal "ToolScript ptc-1 (child ptc-1/2)" attribution)))

  :doc "no-workspace entries still render through the generic prompt adapter"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'generic
                      :tool-name "Read"
                      :specifier-value nil
                      :include-always nil
                      :workspace nil
                      :session session
                      :callback #'ignore))
         captured)
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
               (lambda (tool path include-always _origin cont count rendered-entry)
                 (setq captured
                       (list tool path include-always cont count
                             rendered-entry)))))
      (mevedel-permission-queue--render-generic entry))
    (should (equal "Read" (nth 0 captured)))
    (should (null (nth 1 captured)))
    (should (= 1 (nth 4 captured)))
    (should (eq entry (nth 5 captured))))

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (mevedel--session session)
           (outcome nil)
           (entry (list :kind 'generic
                        :tool-name "Read"
                        :specifier-value "/tmp/file.txt"
                        :include-always nil
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (mevedel-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (mevedel-session-permission-queue session)))))

  :doc "Grep prompt with session back-reference renders without mutating the entry plist"
  (let ((data-buf (generate-new-buffer " *test-pq-grep-data*"))
        (view-buf (generate-new-buffer " *test-pq-grep-view*"))
        (session (test-pq--make-session))
        (render-error nil)
        entry
        interaction-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask))
                    ((symbol-function 'display-warning)
                     (lambda (_type message &optional _level _buffer-name)
                       (when (string-match-p "permission-queue: render error"
                                             message)
                         (setq render-error message)))))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Grep"
                     :args (list :pattern "mevedel-check-permission"
                                 :path default-directory
                                 :glob "*.el"
                                 :output_mode "content"
                                 :head_limit nil
                                 :offset nil
                                 :context nil
                                 :-A nil
                                 :-B nil
                                 :-C nil
                                 :-i nil
                                 :-n t
                                 :type nil
                                 :multiline nil)
                     :specifier-key :path
                     :specifier-value
                     (file-name-concat
                      (file-name-directory
                       (directory-file-name default-directory))
                      "**")
                     :protected-path nil
                     :include-always t
                     :workspace nil
                     :origin "/root"
                     :callback #'ignore)
               session)
              (setq entry (car (mevedel-session-permission-queue session)))
              (setq interaction-id
                    (mevedel-queue--entry-metadata-get
                     entry :interaction-id))
              (should-not render-error)
              (should interaction-id)
              (should-not (plist-member entry :interaction-id))
              (should-not (plist-member entry :view-buffer))
              (with-current-buffer view-buf
                (should (gethash interaction-id
                                 mevedel-view--interaction-overlays))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-permission-queue--allow-once-advance
  (:doc "allow-once settles only the visible head and renders the next prompt")
  ,test
  (test)

  (let ((data-buf (generate-new-buffer " *test-pq-once-data*"))
        (view-buf (generate-new-buffer " *test-pq-once-view*"))
        (session (test-pq--make-session))
        outcomes)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/one.el"
                     :include-always t
                     :origin "/root"
                     :callback
                     (lambda (outcome)
                       (push (cons "one" outcome) outcomes)))
               session)
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-key :path
                     :specifier-value "/tmp/two.el"
                     :include-always t
                     :origin "/root"
                     :callback
                     (lambda (outcome)
                       (push (cons "two" outcome) outcomes)))
               session))
            (with-current-buffer view-buf
              (let ((first-id (mevedel-queue--entry-metadata-get
                               (car (mevedel-session-permission-queue session))
                               :interaction-id)))
                (should first-id)
                (should (gethash first-id mevedel-view--interaction-overlays))
                (mevedel--prompt--settle
                 (gethash first-id mevedel-view--interaction-overlays)
                 'allow-once)
                (should (equal '(("one" . allow-once)) outcomes))
                (should (= 1 (length (mevedel-session-permission-queue session))))
                (let ((next-id (mevedel-queue--entry-metadata-get
                                (car (mevedel-session-permission-queue session))
                                :interaction-id)))
                  (should next-id)
                  (should (gethash next-id mevedel-view--interaction-overlays)))))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))

(mevedel-deftest mevedel-permission-queue--gptel-batch-dispatch
  (:doc "queues every concurrently dispatched outside-workspace Read")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-pq-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "mevedel-pq-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-batch-data*"))
         (view-buf (generate-new-buffer " *test-pq-batch-view*"))
         (workspace (mevedel-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (mevedel-session-create "main" workspace))
         (_register (mevedel-tool-fs--register))
         (read-tool (mevedel-tool-get "Read"))
         (gptel-tool (mevedel-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local temporary-file-directory workspace-root))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'ask))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let* ((first-id
                        (mevedel-queue--entry-metadata-get
                         (car (mevedel-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         mevedel-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (should (overlay-get ov 'mevedel--callback))
                  (mevedel--prompt--settle ov 'allow-once))
                (should (= 3 (length (mevedel-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (mevedel-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (mevedel-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-permission-queue--gptel-batch-deny-once
  (:doc "deny-once settles one queued gptel-dispatched permission")
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-pq-deny-workspace-" t)))
         (outside-root (file-name-as-directory
                        (make-temp-file "mevedel-pq-deny-outside-" t)))
         (data-buf (generate-new-buffer " *test-pq-deny-data*"))
         (view-buf (generate-new-buffer " *test-pq-deny-view*"))
         (workspace (mevedel-workspace-get-or-create
                     'project workspace-root workspace-root "workspace"))
         (session (mevedel-session-create "main" workspace))
         (_register (mevedel-tool-fs--register))
         (read-tool (mevedel-tool-get "Read"))
         (gptel-tool (mevedel-tool-gptel-tool read-tool))
         (paths (list (file-name-concat outside-root "outside-a.el")
                      (file-name-concat outside-root "outside-b.el")
                      (file-name-concat outside-root "outside-c.el")
                      (file-name-concat outside-root "outside-d.el")))
         processed-results)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local temporary-file-directory workspace-root))
          (mevedel-view--setup view-buf data-buf)
          (let ((mevedel-permission-rules nil)
                (mevedel-permission-mode 'ask))
            (cl-letf (((symbol-function 'gptel-agent--block-bg)
                       (lambda () 'ask)))
              (with-current-buffer data-buf
                (dolist (path paths)
                  (funcall (gptel-tool-function gptel-tool)
                           (lambda (result)
                             (push result processed-results))
                           path)))
              (should-not processed-results)
              (should (= 4 (length (mevedel-session-permission-queue session))))
              (with-current-buffer view-buf
                (should (string-match-p
                         "4 permissions pending"
                         (mevedel-view--interaction-count-label)))
                (let* ((first-id
                        (mevedel-queue--entry-metadata-get
                         (car (mevedel-session-permission-queue session))
                         :interaction-id))
                       (ov (and first-id
                                (gethash first-id
                                         mevedel-view--interaction-overlays))))
                  (should first-id)
                  (should ov)
                  (mevedel--prompt--settle ov 'deny-once))
                (should (= 3 (length (mevedel-session-permission-queue
                                      session))))
                (should (string-match-p
                         "3 permissions pending"
                         (mevedel-view--interaction-count-label))))
              (should (= 1 (length processed-results)))
              (should (string-match-p
                       "Error: Permission denied"
                       (car processed-results)))
              (mevedel-permission-queue-abort-all session))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (when (file-directory-p workspace-root)
        (delete-directory workspace-root t))
      (when (file-directory-p outside-root)
        (delete-directory outside-root t))
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-permission-queue--render-bash
  (:quiet t :doc "renders queued Bash permission prompts")
  ,test
  (test)

  :doc "missing Bash UI helper produces the pinned denial outcome"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (entry (list :kind 'bash
                      :command "sudo ls"
                      :command-class 'dangerous
                      :include-always nil
                      :session session))
         (outcome nil)
         (saved (and (fboundp 'mevedel-permission--prompt-async-bash)
                     (symbol-function
                      'mevedel-permission--prompt-async-bash))))
    (setq entry (plist-put entry :callback (lambda (o) (setq outcome o))))
    (unwind-protect
        (progn
          (when saved
            (fmakunbound 'mevedel-permission--prompt-async-bash))
          (setf (mevedel-session-permission-queue session) (list entry))
          (mevedel-permission-queue--render-head session)
          (should (equal '(deny . "Bash permission UI unavailable")
                         outcome))
          (should (null (mevedel-session-permission-queue session))))
	  (when saved
	    (fset 'mevedel-permission--prompt-async-bash saved))))

  :doc "Bash approval resumes exactly once"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'bash
                      :command "git status"
                      :command-class 'read-only
                      :include-always nil
                      :session session))
         (outcomes nil)
         rendered)
    (setq entry (plist-put entry :callback
                           (lambda (o) (push o outcomes))))
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               (lambda (next-entry) (push next-entry rendered))))
      (mevedel-permission-queue--on-head-outcome entry 'allow-once)
      (mevedel-permission-queue--on-head-outcome entry 'allow-once))
    (should (equal '(allow-once) outcomes))
    (should (null rendered))
    (should (null (mevedel-session-permission-queue session)))))

(mevedel-deftest mevedel-permission-queue--render-eval
  (:quiet t :doc "renders queued Eval permission prompts")
  ,test
  (test)

  :doc "missing live view aborts the visible head"
  (with-temp-buffer
    (let* ((session (test-pq--make-session))
           (mevedel--session session)
           (outcome nil)
           (entry (list :kind 'eval
                        :expression "(message \"hi\")"
                        :session session
                        :callback (lambda (o) (setq outcome o)))))
      (setf (mevedel-session-permission-queue session) (list entry))
      (mevedel-permission-queue--render-head session)
      (should (eq 'aborted outcome))
      (should (null (mevedel-session-permission-queue session)))))

  :doc "agent Eval permission renders in the parent interaction view"
  (let ((parent-data (generate-new-buffer " *test-pq-parent-data*"))
        (parent-view (generate-new-buffer " *test-pq-parent-view*"))
        (agent-data (generate-new-buffer " *test-pq-agent-data*"))
        (session (test-pq--make-session)))
    (setf (mevedel-session-agent-transcripts session)
          '(("/root/verifier"
             :status running)))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create
                         :agent-id
                         "/root/verifier")))
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer agent-data
              (mevedel-permission--enqueue
               (list :kind 'eval
                     :expression "(message \"hi\")"
                     :mode "batch"
                     :origin "/root/verifier"
                     :callback #'ignore)
               session)))
          (with-current-buffer parent-view
            (should (string-match-p "The LLM is requesting permission to evaluate elisp"
                                    (buffer-string)))
            (should (string-match-p "from /root/verifier"
                                    (buffer-string)))
            (should (string-match-p "Mode: batch"
                                    (buffer-string)))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data))))

  :doc "agent Eval permissions survive blocked status redraw and rebuild"
  (let ((parent-data (generate-new-buffer " *test-pq-parent-status-data*"))
        (parent-view (generate-new-buffer " *test-pq-parent-status-view*"))
        (agent-data (generate-new-buffer " *test-pq-agent-status-data*"))
        (session (test-pq--make-session))
        outcomes)
    (setf (mevedel-session-agent-transcripts session)
          '(("/root/verifier"
             :status running)))
    (unwind-protect
        (progn
          (with-current-buffer parent-data
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup parent-view parent-data)
          (with-current-buffer agent-data
            (org-mode)
            (setq-local mevedel--session session)
            (setq-local mevedel--view-buffer parent-view)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create
                         :agent-id
                         "/root/verifier")))
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask))
                    ((symbol-function 'mevedel-view--agent-status-collect)
                     (lambda ()
                       (list (list :path
                                   "/root/verifier"
                                   :status 'blocked
                                   :role "verifier"
                                   :description "Verify tracked diff"
                                   :calls 18)))))
            (with-current-buffer parent-view
              (mevedel-view--render-agent-status))
            (with-current-buffer agent-data
              (dotimes (i 3)
                (mevedel-permission--enqueue
                 (list :kind 'eval
                       :expression (format "(+ %d 1)" i)
                       :mode "batch"
                       :origin
                       "/root/verifier"
                       :callback (lambda (outcome)
                                   (push outcome outcomes)))
                 session)))
            (with-current-buffer parent-view
              (mevedel-view--render-agent-status)
              (mevedel-view--interaction-rebuild)
              (should-not outcomes)
              (should (= 3 (length (mevedel-session-permission-queue session))))
              (let* ((text (buffer-substring-no-properties
                            (point-min) mevedel-view--input-marker))
                     (agent-pos (string-search
                                 "Blocked /root/verifier" text))
                     (prompt-pos (string-search
                                  "The LLM is requesting permission to evaluate elisp"
                                  text)))
                (should agent-pos)
                (should prompt-pos)
                (should (< agent-pos prompt-pos))
                (should (equal "3 permissions pending"
                               (mevedel-view--interaction-count-label)))
                (save-excursion
                  (goto-char (point-min))
                  (search-forward "3 permissions pending"
                                  mevedel-view--input-marker)
                  (should (eq 'interaction
                              (get-text-property
                               (match-beginning 0)
                               'mevedel-view-zone-namespace)))
                  (should (eq :separator
                              (get-text-property
                               (match-beginning 0)
                               'mevedel-view-zone-id))))
                (should (string-search "from /root/verifier" text))
                (should (string-search "Mode: batch" text))))))
      (when (buffer-live-p agent-data) (kill-buffer agent-data))
      (when (buffer-live-p parent-view) (kill-buffer parent-view))
      (when (buffer-live-p parent-data) (kill-buffer parent-data)))))

(mevedel-deftest mevedel-permission-queue--render-sandbox
  (:doc "renders additive sandbox permission prompts")
  ,test
  (test)

  :doc "network request uses the once-only sandbox prompt adapter"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'sandbox
                      :tool-name "Bash"
                      :detail "curl https://example.test"
                      :justification "Download the requested page?"
                      :additional-permissions '(:network t)
                      :origin "/root"
                      :session session
                      :callback #'ignore))
         captured)
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-sandbox)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission-queue--render-sandbox entry))
    (should (equal "Bash" (nth 0 captured)))
    (should (equal "curl https://example.test" (nth 1 captured)))
    (should (equal "Download the requested page?" (nth 2 captured)))
    (should (= 1 (nth 5 captured)))
    (should (eq entry (nth 6 captured))))

  :doc "filesystem request preserves its exact resource metadata"
  (let* ((session (test-pq--make-session))
         (entry (list :kind 'sandbox
                      :tool-name "Bash"
                      :detail "cat /tmp/secret"
                      :justification "Read the requested file?"
                      :resource-path "/tmp/secret"
                      :resource-access 'read
                      :origin "/root"
                      :session session
                      :callback #'ignore))
         captured)
    (setf (mevedel-session-permission-queue session) (list entry))
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-sandbox)
               (lambda (&rest args) (setq captured args))))
      (mevedel-permission-queue--render-sandbox entry))
    (should (equal "cat /tmp/secret" (nth 1 captured)))
    (should (= 1 (nth 5 captured)))
    (should (eq entry (nth 6 captured)))))


;;
;;; Coalesce vocabulary translation

(mevedel-deftest mevedel-permission-queue--translate-coalesce-outcome
  (:doc "translates 'allow / 'deny to the kind's expected callback vocabulary")
  ,test
  (test)

  :doc "generic kind passes 'allow / 'deny through unchanged"
  (should (eq 'allow
              (mevedel-permission-queue--translate-coalesce-outcome
               'generic 'allow)))
  (should (eq 'deny
              (mevedel-permission-queue--translate-coalesce-outcome
               'generic 'deny)))

  :doc "bash kind passes 'allow through"
  (should (eq 'allow
              (mevedel-permission-queue--translate-coalesce-outcome
               'bash 'allow)))
  (should (eq 'deny
              (mevedel-permission-queue--translate-coalesce-outcome
               'bash 'deny)))

  :doc "eval kind translates 'allow to authoritative allow-once"
  (should (eq 'allow-once
              (mevedel-permission-queue--translate-coalesce-outcome
               'eval 'allow)))

  :doc "sandbox kind translates both outcomes to once-only vocabulary"
  (should (eq 'allow-once
              (mevedel-permission-queue--translate-coalesce-outcome
               'sandbox 'allow)))
  (should (eq 'deny-once
              (mevedel-permission-queue--translate-coalesce-outcome
               'sandbox 'deny))))


;;
;;; Abort flush

(mevedel-deftest mevedel-permission-queue-abort-all
  (:doc "abort flushes queue and fires 'aborted on every callback")
  ,test
  (test)

  :doc "every queued callback fires 'aborted; queue is empty after"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root"
             :callback (lambda (o) (push (cons "Read" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'bash :command "rm /tmp/x"
             :origin "/root"
             :callback (lambda (o) (push (cons "Bash" o) outcomes))))
      (mevedel-permission-queue-abort-all session))
    (should (= 2 (length outcomes)))
    (should (cl-every (lambda (o) (eq 'aborted (cdr o))) outcomes))
    (should (null (mevedel-session-permission-queue session))))

  :doc "abort on empty queue is a no-op"
  (let ((session (test-pq--make-session)))
    (should-not (mevedel-permission-queue-abort-all session))
    (should (null (mevedel-session-permission-queue session)))))


;;
;;; Per-request sweep

(mevedel-deftest mevedel-permission-queue-sweep-request
  (:doc "sweep fires 'aborted on entries owned by REQUEST-ID; others stay")
  ,test
  (test)

  :doc "same-origin entries from other requests remain queued"
  (let* ((session (test-pq--make-session))
         (mevedel--session session)
         (outcomes nil))
    (cl-letf (((symbol-function 'mevedel-permission-queue--render-entry)
               #'ignore))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-1"
             :callback (lambda (o) (push (cons "explorer" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-2"
             :callback (lambda (o) (push (cons "main" o) outcomes))))
      (mevedel-permission--enqueue
       (list :kind 'generic :tool-name "Read"
             :origin "/root/explorer"
             :request-id "request-1"
             :callback (lambda (o) (push (cons "explore2" o) outcomes))))
      (mevedel-permission-queue-sweep-request "request-1" session))
    (should (eq 'aborted (cdr (assoc "explorer" outcomes))))
    (should (eq 'aborted (cdr (assoc "explore2" outcomes))))
    (should-not (assoc "main" outcomes))
    (let ((q (mevedel-session-permission-queue session)))
      (should (= 1 (length q)))
      (should (equal "request-2" (plist-get (car q) :request-id)))))

  :doc "sweeping the visible head removes its interaction overlay"
  (let ((data-buf (generate-new-buffer " *test-pq-sweep-data*"))
        (view-buf (generate-new-buffer " *test-pq-sweep-view*"))
        (session (test-pq--make-session))
        outcomes
        swept-id)
    (unwind-protect
        (progn
          (with-current-buffer data-buf
            (org-mode)
            (setq-local mevedel--session session))
          (mevedel-view--setup view-buf data-buf)
          (cl-letf (((symbol-function 'gptel-agent--block-bg)
                     (lambda () 'ask)))
            (with-current-buffer data-buf
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/agent.txt"
                     :include-always nil
                     :origin "/root/explorer"
                     :request-id "request-1"
                     :callback (lambda (o) (push (cons "agent" o) outcomes))))
              (setq swept-id
                    (mevedel-queue--entry-metadata-get
                     (car (mevedel-session-permission-queue session))
                     :interaction-id))
              (mevedel-permission--enqueue
               (list :kind 'generic
                     :tool-name "Read"
                     :specifier-value "/tmp/main.txt"
                     :include-always nil
                     :origin "/root/explorer"
                     :request-id "request-2"
                     :callback (lambda (o) (push (cons "main" o) outcomes))))
              (should swept-id)
              (with-current-buffer view-buf
                (should (gethash swept-id mevedel-view--interaction-overlays)))
              (mevedel-permission-queue-sweep-request "request-1" session)))
          (should (eq 'aborted (cdr (assoc "agent" outcomes))))
          (should-not (assoc "main" outcomes))
          (with-current-buffer view-buf
            (should-not (gethash swept-id mevedel-view--interaction-overlays))
            (should (= 1 (hash-table-count
                          mevedel-view--interaction-overlays))))
          (let ((q (mevedel-session-permission-queue session)))
            (should (= 1 (length q)))
            (should (equal "request-2"
                           (plist-get (car q) :request-id)))))
      (when (buffer-live-p view-buf) (kill-buffer view-buf))
      (when (buffer-live-p data-buf) (kill-buffer data-buf)))))


(provide 'test-mevedel-permission-queue)
;;; test-mevedel-permission-queue.el ends here
