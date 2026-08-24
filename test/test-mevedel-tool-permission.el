;;; test-mevedel-tool-permission.el -- Tool permission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission-path fan-out, decision logging, hooks, and prompt
;; orchestration independently from Pipeline step ordering.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agents)
(require 'mevedel-bash-analysis)
(require 'mevedel-bash-policy)
(require 'mevedel-execution)
(require 'mevedel-hooks)
(require 'mevedel-permission-log)
(require 'mevedel-permission-prompt)
(require 'mevedel-permission-queue)
(require 'mevedel-permission-rules)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-tool-exec)
(require 'mevedel-tool-patch)
(require 'mevedel-tool-permission)
(require 'mevedel-tool-registry)


;;

;;; Permission step

(mevedel-deftest mevedel-tool-permission--origin ()
  ,test
  (test)
  :doc "prefers an explicit prompt origin over request and invocation owners"
  (let ((request
         (mevedel-request--create :origin "/root/worker")))
    (should
     (equal
      "/root/worker/verifier"
      (mevedel-tool-permission--origin
       (list :request request)
       "/root/worker/verifier"))))
  :doc "uses the request path before an agent or root fallback"
  (let ((request
         (mevedel-request--create :origin "/root/worker")))
    (should
     (equal
      "/root/worker"
      (mevedel-tool-permission--origin (list :request request)))))
  :doc "falls back to root without a scoped owner"
  (should (equal "/root" (mevedel-tool-permission--origin nil))))

(mevedel-deftest mevedel-tool-permission-log-decision ()
  ,test
  (test)
  :doc "carries nested identity and source into the permission log"
  (let* ((mevedel-permission-log-enabled t)
         (session (mevedel-session--create
                   :name "permission-log" :permission-mode 'ask))
         (tool (mevedel-tool--create :name "Child"))
         (context (list :session session :tool tool
                        :tool-use-id "parent/1"
                        :parent-tool-use-id "parent"
                        :call-source 'ptc)))
    (mevedel-tool-permission-log-decision
     context '(:outcome allow :raw-outcome allow :via test))
    (let ((entry (car (mevedel-session-permission-log-pending session))))
      (should (equal "parent/1" (plist-get entry :tool-use-id)))
      (should (equal "parent" (plist-get entry :parent-tool-use-id)))
      (should (eq 'ptc (plist-get entry :call-source))))))

(mevedel-deftest mevedel-tool-permission--denial-outcome-p ()
  ,test
  (test)
  :doc "recognizes every interactive denial outcome"
  (dolist (outcome '(deny deny-once deny-session
                          (deny . "reason") (feedback . "reason")))
    (should (mevedel-tool-permission--denial-outcome-p outcome)))
  :doc "rejects allow, abort, and unrelated compound outcomes"
  (dolist (outcome '(allow allow-once allow-session always-allow aborted
                           (allow . "reason") (unknown . "reason") nil))
    (should-not (mevedel-tool-permission--denial-outcome-p outcome))))

(mevedel-deftest mevedel-tool-permission--denial-provenance ()
  ,test
  (test)
  :doc "prefers stored interactive provenance over decision metadata"
  (should
   (eq 'user
       (mevedel-tool-permission--denial-provenance
        '(:permission-denial-provenance user)
        '(:via deny-rule))))
  :doc "uses decision metadata when no interactive provenance is stored"
  (should
   (eq 'deny-rule
       (mevedel-tool-permission--denial-provenance
        nil '(:via deny-rule))))
  :doc "falls back to policy when neither source is present"
  (should
   (eq 'policy
       (mevedel-tool-permission--denial-provenance nil nil))))

(mevedel-deftest mevedel-tool-permission--request ()
  ,test
  (test)
  :doc "unresolved requests enter the queue and user denials retain provenance"
  (let (progress queued settled-context settled-outcome)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback nil)))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session) (setq queued entry))))
      (mevedel-tool-permission--request
       (list :progress-callback (lambda (event) (setq progress event))
             :tool-use-id "ptc-1/2"
             :parent-tool-use-id "ptc-1"
             :call-source 'ptc)
       '(:kind generic :callback ignore) nil
       (lambda (context outcome)
         (setq settled-context context settled-outcome outcome)))
      (should queued)
      (should (eq 'permission-wait progress))
      (should (equal "ptc-1/2" (plist-get queued :tool-use-id)))
      (should (equal "ptc-1" (plist-get queued :parent-tool-use-id)))
      (should (eq 'ptc (plist-get queued :call-source)))
      (funcall (plist-get queued :callback) 'deny-session)
      (should (eq 'deny-session settled-outcome))
      (should (eq 'user
                  (plist-get settled-context
                             :permission-denial-provenance)))))
  :doc "hook allow and deny settle without queue admission"
  (dolist (decision '((:permission-decision allow)
                      (:permission-decision deny
                                            :permission-reason "blocked")))
    (let (queued settled-context settled-outcome)
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback decision)))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (&rest _) (setq queued t))))
        (mevedel-tool-permission--request
         nil '(:kind generic :callback ignore) nil
         (lambda (context outcome)
           (setq settled-context context settled-outcome outcome)))
        (should-not queued)
        (if (eq 'allow (plist-get decision :permission-decision))
            (should (eq 'allow settled-outcome))
          (should (eq 'deny (car settled-outcome)))
          (should (eq 'PermissionRequest
                      (plist-get settled-context
                                 :permission-denial-provenance)))))))
  :doc "an unresolved request uses its card-free fallback"
  (let (queued settled-outcome)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback nil)))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq queued t))))
      (mevedel-tool-permission--request
       nil '(:kind generic :callback ignore) nil
       (lambda (_context outcome) (setq settled-outcome outcome))
       nil 'allow)
      (should-not queued)
      (should (eq 'allow settled-outcome))))
  :doc "an explicit hook ask overrides the card-free fallback"
  (let (queued settled-outcome)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback '(:permission-decision ask))))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session) (setq queued entry))))
      (mevedel-tool-permission--request
       nil '(:kind generic :callback ignore) nil
       (lambda (_context outcome) (setq settled-outcome outcome))
       nil 'allow)
      (should queued)
      (should-not settled-outcome)))
  :doc "one-shot prompts retain their authoritative side data buffer"
  (let ((side-buffer (generate-new-buffer " *mevedel-side-queue*"))
        queued)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback nil)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session) (setq queued entry))))
          (mevedel-tool-permission--request
           `(:one-shot-mutations-p t :buffer ,side-buffer)
           '(:kind generic :mutation-p t :callback ignore) nil
           #'ignore)
          (should (plist-get queued :once-only))
          (should (eq side-buffer (plist-get queued :data-buffer))))
      (kill-buffer side-buffer))))

(mevedel-deftest mevedel-tool-permission-step
  (:before-each
   (advice-add 'display-warning :around
               #'mevedel-test--drop-sessionless-permission-warning)
   :after-each
   (advice-remove 'display-warning
                  #'mevedel-test--drop-sessionless-permission-warning))
  ,test
  (test)
  :doc "allows read-only tool in ask mode"
  (let* ((tool (mevedel-tool--create
                :name "Read"
                :read-only-p t))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "authorizes every path declared by one aggregate edit"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-multi-permission-" t)))
         (path-a (file-name-concat dir "a.txt"))
         (path-b (file-name-concat dir "b.txt"))
         (gptel--known-tools nil)
         (mevedel-tool--registry (make-hash-table :test #'equal))
         (session (mevedel-session--create
                   :name "multi" :save-path dir
                   :permission-mode 'full-auto
                   :resource-grants
                   (list (list :path path-a :access 'write)
                         (list :path path-b :access 'write))))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-log-enabled t)
         called)
    (unwind-protect
        (progn
          (mevedel-define-tool
           :wrap (gptel-make-tool
                  :name "AggregateEdit"
                  :function #'ignore
                  :description "Edit several files"
                  :args nil
                  :category "test-aggregate")
           :groups (edit)
           :get-paths (lambda (_args) (list path-a path-b)))
          (mevedel-tool-permission-step
           (list :tool (mevedel-tool-get
                        "AggregateEdit" "mevedel-test-aggregate")
                 :args nil :session session)
           (lambda (_context) (setq called t)) #'ignore)
          (should called)
          (should
           (equal (list path-a path-b)
                  (mapcar
                   (lambda (entry)
                     (plist-get entry :specifier-value))
                   (cl-remove-if-not
                    (lambda (entry)
                      (eq (plist-get entry :event)
                          'permission-decision))
                    (mevedel-test--permission-log-entries
                     session))))))
      (delete-directory dir t)))
  :doc "rejects an aggregate edit when any affected path is denied"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-multi-denial-" t)))
         (path-a (file-name-concat dir "a.txt"))
         (path-b (file-name-concat dir "b.txt"))
         (session (mevedel-session--create
                   :name "multi-denial" :save-path dir
                   :permission-mode 'full-auto))
         (tool (mevedel-tool--create
                :name "ApplyPatch" :groups '(edit)
                :get-paths (lambda (_args) (list path-a path-b))))
         (mevedel-permission-rules
          (list (list "ApplyPatch" :path path-a :action 'allow)
                (list "ApplyPatch" :path path-b :action 'deny)))
         (mevedel-protected-paths nil)
         next-called failure)
    (unwind-protect
        (progn
          (mevedel-tool-permission-step
           (list :tool tool :args nil :session session)
           (lambda (_context) (setq next-called t))
           (lambda (message &rest _) (setq failure message)))
          (should-not next-called)
          (should (string-match-p "Permission denied" failure)))
      (delete-directory dir t)))
  :doc "Bash and network asks render as one combined authority card"
  (let* ((tool (mevedel-tool-ensure "Bash"))
         (session (mevedel-session--create
                   :name "combined"
                   :permission-mode 'ask))
         (ctx
          (list
           :tool tool
           :args
           '(:command "unknown-combined-command"
                      :sandbox_permissions "with_additional_permissions"
                      :additional_permissions (:network t)
                      :justification "Reach the package registry?")
           :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-guardian nil)
         content)
    (cl-letf
        (((symbol-function 'mevedel-permission-queue--render-entry)
          #'ignore))
      (mevedel-tool-permission-step ctx #'ignore #'ignore))
    (should (= 1 (length
                  (mevedel-session-permission-queue session))))
    (cl-letf
        (((symbol-function
           'mevedel-permission--prompt-async-with-content)
          (lambda (text &rest _args)
            (setq content
                  (substring-no-properties text)))))
      (mevedel-permission-queue--render-bash
       (car (mevedel-session-permission-queue session))))
    (should (string-match-p "Authority" content))
    (should (string-match-p "\\[ \\] Command" content))
    (should (string-match-p "\\[ \\] Network" content)))
  :doc "fails with Permission denied when rules deny"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules '(("Edit" :action deny)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason fail-context fail-kind)
    (mevedel-tool-permission-step
     ctx #'ignore
     (lambda (reason context kind)
       (setq fail-reason reason
             fail-context context
             fail-kind kind)))
    (should (equal fail-reason "Permission denied"))
    (should (equal fail-context ctx))
    (should (eq fail-kind 'permission-denied)))
  :doc "allows when explicit allow rule matches"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules '(("Edit" :action allow)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "allows in full-auto mode"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'full-auto)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "full-auto allow writes permission-decision diagnostic"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (mevedel-session--create
                   :name "test" :save-path dir
                   :permission-mode 'full-auto))
         (tool (mevedel-tool--create
                :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (mevedel-tool-permission-step ctx #'ignore #'ignore)
          (let ((entry (car (mevedel-test--permission-log-entries
                             session))))
            (should (eq 'permission-decision
                        (plist-get entry :event)))
            (should (equal "Edit" (plist-get entry :tool-name)))
            (should (eq 'full-auto (plist-get entry :mode)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'mode (plist-get entry :via)))))
      (delete-directory dir t)))
  :doc "session rule decisions include session bucket"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (session (mevedel-session--create
                   :name "test" :save-path dir
                   :permission-rules '(("Edit" :action allow))))
         (tool (mevedel-tool--create
                :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (progn
          (mevedel-tool-permission-step ctx #'ignore #'ignore)
          (let ((entry (car (mevedel-test--permission-log-entries
                             session))))
            (should (eq 'permission-decision
                        (plist-get entry :event)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'rule (plist-get entry :via)))
            (should (eq :session (plist-get entry :bucket)))))
      (delete-directory dir t)))
  :doc "protected path prompt logs decision and queue events"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-permission-log-" t)))
         (path (expand-file-name ".git/config" dir))
         (session (mevedel-session--create
                   :name "test" :save-path dir))
         (tool (mevedel-tool--create
                :name "Write" :read-only-p nil
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths
          (list (cons path 'inaccessible)))
         (mevedel-permission-mode 'ask)
         (mevedel-permission-log-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
                   (lambda (&rest _args) nil)))
          (make-directory (file-name-directory path) t)
          (mevedel-tool-permission-step ctx #'ignore #'ignore)
          (let ((entries (mevedel-test--permission-log-entries
                          session)))
            (should (eq 'permission-decision
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'ask (plist-get (nth 0 entries) :outcome)))
            (should (eq 'protected-path (plist-get (nth 0 entries) :via)))
            (should (plist-get (nth 0 entries) :protected-path))
            (should (eq 'permission-enqueued
                        (plist-get (nth 1 entries) :event)))))
      (delete-directory dir t)))
  :doc "guardian context is advisory and includes deterministic confinement facts"
  (let* ((session (mevedel-session--create
                   :name "guardian" :permission-mode 'ask))
         (tool (mevedel-tool-ensure "Bash"))
         (facts '(:sandbox bubblewrap
                           :filesystem workspace-write
                           :proc fresh
                           :network isolated))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-bash-dangerous-commands '("rm"))
         guardian-context
         (mevedel-permission-guardian
          (lambda (_command context callback)
            (setq guardian-context context)
            (funcall callback
                     '(:risk "critical"
                             :recommendation "deny"
                             :reason "Deletes a file."
                             :class "read-only"))))
         entry
         next-called
         fail-reason)
    (require 'mevedel-sandbox)
    (cl-letf (((symbol-function 'mevedel-sandbox-pending-facts)
               (lambda (&rest _) facts))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)))
              ((symbol-function
                'mevedel-permission-queue--render-head)
               #'ignore))
      (mevedel-tool-permission-step
       (list :tool tool :args '(:command "rm file")
             :session session)
       (lambda (_context) (setq next-called t))
       (lambda (reason &rest _) (setq fail-reason reason))))
    (should (eq 'dangerous (plist-get guardian-context :class)))
    (should
     (equal facts
            (plist-get guardian-context :sandbox-facts)))
    (should-not next-called)
    (should-not fail-reason)
    (should-not
     (plist-member
      (car (plist-get entry :guardian-cell)) :class))
    ;; Even a deny recommendation remains advisory in ask mode.
    (funcall (plist-get entry :callback) 'allow-once)
    (should next-called)
    (should-not fail-reason))
  :doc "guardian failure preserves interactive Bash prompts in ask and edits"
  (dolist (mode '(ask edits))
    (let* ((session (mevedel-session--create
                     :name "guardian" :permission-mode mode))
           (tool (mevedel-tool-ensure "Bash"))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths nil)
           (mevedel-bash-dangerous-commands '("rm"))
           (mevedel-permission-guardian
            (lambda (_command _context callback)
              (funcall callback nil)))
           entry
           next-called
           fail-reason)
      (cl-letf (((symbol-function 'mevedel-sandbox-pending-facts)
                 (lambda (&rest _)
                   '(:sandbox bubblewrap
                              :filesystem workspace-write
                              :network isolated)))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (queued &optional _session)
                   (setq entry queued)))
                ((symbol-function
                  'mevedel-permission-queue--render-head)
                 #'ignore))
        (mevedel-tool-permission-step
         (list :tool tool :args '(:command "rm file")
               :session session)
         (lambda (_context) (setq next-called t))
         (lambda (reason &rest _) (setq fail-reason reason))))
      (should entry)
      (should-not next-called)
      (should-not fail-reason)
      (funcall (plist-get entry :callback) 'allow-once)
      (should next-called)))
  :doc "model guardian errors preserve prompts and full-auto execution"
  (dolist (mode '(ask edits full-auto))
    (let* ((session (mevedel-session--create
                     :name "guardian" :permission-mode mode))
           (tool (mevedel-tool-ensure "Bash"))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths nil)
           (mevedel-bash-dangerous-commands '("rm"))
           (mevedel-permission-guardian t)
           entry
           next-called
           fail-reason)
      (cl-letf (((symbol-function 'mevedel-sandbox-pending-facts)
                 (lambda (&rest _)
                   '(:sandbox bubblewrap
                              :filesystem workspace-write
                              :network isolated)))
                ((symbol-function 'mevedel-model-resolve-workload)
                 (lambda (&rest _)
                   (user-error "Guardian model unavailable")))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (queued &optional _session)
                   (setq entry queued)))
                ((symbol-function
                  'mevedel-permission-queue--render-head)
                 #'ignore))
        (mevedel-tool-permission-step
         (list :tool tool :args '(:command "rm file")
               :session session)
         (lambda (_context) (setq next-called t))
         (lambda (reason &rest _) (setq fail-reason reason))))
      (if (eq mode 'full-auto)
          (should next-called)
        (should entry)
        (should-not next-called))
      (should-not fail-reason)))
  :doc "full-auto guardian may veto suspicious Bash but failure allows"
  (dolist (guardian-result
           '((:risk "high" :recommendation "deny"
                    :reason "Deletes a file.")
             nil))
    (let* ((session (mevedel-session--create
                     :name "guardian"
                     :permission-mode 'full-auto))
           (tool (mevedel-tool-ensure "Bash"))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths nil)
           (mevedel-bash-dangerous-commands '("rm"))
           (mevedel-permission-guardian
            (lambda (_command _context callback)
              (funcall callback guardian-result)))
           next-called
           fail-reason)
      (cl-letf (((symbol-function 'mevedel-sandbox-pending-facts)
                 (lambda (&rest _)
                   '(:sandbox bubblewrap
                              :filesystem workspace-write
                              :network isolated))))
        (mevedel-tool-permission-step
         (list :tool tool :args '(:command "rm file")
               :session session)
         (lambda (_context) (setq next-called t))
         (lambda (reason &rest _) (setq fail-reason reason))))
      (if guardian-result
          (progn
            (should-not next-called)
            (should (equal "Permission denied" fail-reason)))
        (should next-called)
        (should-not fail-reason))))
  :doc "tool check-permission returning allow is respected"
  (let* ((tool (mevedel-tool--create
                :name "CustomTool"
                :check-permission (lambda (_ts _input) 'allow)
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "reads session rules from context, not buffer-local"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (session (mevedel-session--create
                   :name "test"
                   :permission-rules '(("Edit" :action allow))))
         (ctx (list :tool tool :args nil :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "ignores buffer-local session - only context :session counts"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules '(("Edit" :action deny)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         (mevedel--session (mevedel-session--create
                            :name "phantom"
                            :permission-rules '(("Edit" :action allow))))
         fail-reason)
    ;; The dynamic mevedel--session has an allow rule but the step must
    ;; not look at it; only the missing :session in `ctx' applies.
    (mevedel-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied")))
  :doc "sync slot signaling permission-denied surfaces REASON via fail"
  (let* ((tool (mevedel-tool--create
                :name "Custom"
                :check-permission
                (lambda (_ts _input)
                  (signal 'mevedel-permission-denied '("user feedback X")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason)
    (mevedel-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: user feedback X")))
  :doc "async slot returning 'allow advances to next"
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'allow))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "async slot returning (deny . REASON) surfaces via fail"
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont)
                  (funcall cont '(deny . "Custom slot reason")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason)
    (mevedel-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: Custom slot reason")))
  :doc "async slot returning (feedback . TEXT) maps to scoped denial with text"
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont)
                  (funcall cont '(feedback . "user typed this")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason)
    (mevedel-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: user typed this")))
  :doc "async slot returning 'aborted surfaces as fail aborted"
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'aborted))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason)
    (mevedel-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "aborted")))
  :doc "async slot returning nil falls through to chain"
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont nil))
                :read-only-p t))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    ;; Read-only + ask mode -> step 8 returns 'allow.
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "paths inside allowed roots advance without prompting"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (extra (file-name-as-directory
                 (make-temp-file "mevedel-pipeline-extra-" t)))
         (path (file-name-concat extra "file.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws
                   :permission-mode 'edits))
         (tool (mevedel-tool--create
                :name "Write"
                :groups '(edit)
                :read-only-p nil
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         called enqueued)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root extra)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (&rest _args) (setq enqueued t))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should called)
          (should-not enqueued))
      (delete-directory root t)
      (delete-directory extra t)))
  :doc "ask prompts for native edits even inside allowed roots"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-ask-edit-" t)))
         (path (file-name-concat root "file.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "ask-edit" :root root
              :name "ask-edit" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws
                   :permission-mode 'ask))
         (tool (mevedel-tool--create
                :name "Edit" :groups '(edit)
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool :args (list :file_path path)
                    :session session :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         called enqueued)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (&rest _args) (setq enqueued t))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should-not called)
          (should enqueued))
      (delete-directory root t)))
  :doc "edits advances native edit tools inside allowed roots"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-auto-edit-" t)))
         (ws (mevedel-workspace--create
              :type 'project :id "auto-edit" :root root
              :name "auto-edit" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws
                   :permission-mode 'edits))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (dolist (spec '(("Edit" :file_path)
                          ("Write" :file_path)
                          ("MkDir" :path)))
            (let* ((name (car spec))
                   (key (cadr spec))
                   (path (file-name-concat root (downcase name)))
                   (tool (mevedel-tool--create
                          :name name :groups '(edit)
                          :get-path (lambda (args)
                                      (plist-get args key))))
                   (ctx (list :tool tool :args (list key path)
                              :session session :workspace ws))
                   called enqueued)
              (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                         (lambda (&rest _args) (setq enqueued t))))
                (mevedel-tool-permission-step
                 ctx (lambda (_c) (setq called t)) #'ignore))
              (should called)
              (should-not enqueued))))
      (delete-directory root t)))
  :doc "edits keeps Bash and Eval behind their permission checks"
  (dolist (name '("Bash" "Eval"))
    (let* ((session (mevedel-session--create
                     :name "test" :permission-mode 'edits))
           (tool (mevedel-tool--create
                  :name name :groups '(eval)))
           (ctx (list :tool tool :args nil :session session))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths nil)
           called enqueued)
      (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                 (lambda (&rest _args) (setq enqueued t))))
        (mevedel-tool-permission-step
         ctx (lambda (_c) (setq called t)) #'ignore))
      (should-not called)
      (should enqueued)))
  :doc "session-scoped dropped-file grant allows exact Read outside workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws))
         (tool (mevedel-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called enqueued)
    (with-temp-file path (insert "dropped\n"))
    (mevedel-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (&rest _args) (setq enqueued t))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should called)
          (should-not enqueued))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "outside Read approval records exact session resource authority"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-outside-" t)))
         (path (file-name-concat outside "file.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws))
         (tool (mevedel-tool--create
                :name "Read" :read-only-p t
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         captured-entry called)
    (with-temp-file path (insert "outside\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (setq captured-entry entry))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should-not called)
          (should (eq 'read
                      (plist-get captured-entry :resource-access)))
          (should (equal path
                         (plist-get captured-entry :specifier-value)))
          (funcall (plist-get captured-entry :callback)
                   'allow-session)
          (should called)
          (should (equal (list (list :path path :access 'read))
                         (mevedel-session-resource-grants session)))
          (should-not (mevedel-session-permission-rules session))
          (setq called nil
                captured-entry nil)
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should called)
          (should-not captured-entry))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "session-scoped dropped-file grant does not allow descendant Read"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped"))
         (descendant-dir (file-name-as-directory path))
         (descendant (file-name-concat descendant-dir "secret.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws))
         (tool (mevedel-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path descendant)
                    :session session
                    :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called captured-entry)
    (make-directory descendant-dir)
    (with-temp-file descendant (insert "secret\n"))
    (mevedel-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (setq captured-entry entry))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should-not called)
          (should captured-entry))
      (when (file-exists-p descendant)
        (delete-file descendant))
      (when (file-directory-p descendant-dir)
        (delete-directory descendant-dir))
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "session-scoped dropped-file grant does not allow Write"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "mevedel-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped.txt"))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws))
         (tool (mevedel-tool--create
                :name "Write"
                :read-only-p nil
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called captured-entry)
    (with-temp-file path (insert "dropped\n"))
    (mevedel-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel--all-allowed-roots)
                   (lambda (&optional _buffer) (list root)))
                  ((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (setq captured-entry entry))))
          (mevedel-tool-permission-step
           ctx (lambda (_c) (setq called t)) #'ignore)
          (should-not called)
          (should captured-entry))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "workspace-root path is not broadened to parent directory when prompted"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-pipeline-root-" t)))
         (root-without-slash (directory-file-name root))
         (ws (mevedel-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (mevedel-session--create
                   :name "test" :workspace ws))
         (tool (mevedel-tool--create
                :name "Grep"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :path))))
         (ctx (list :tool tool
                    :args (list :path root-without-slash)
                    :session session
                    :workspace ws))
         (mevedel-permission-rules
          `(("Grep" :path ,root-without-slash :action ask)))
         (mevedel-protected-paths nil)
         captured-entry)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (setq captured-entry entry))))
          (mevedel-tool-permission-step ctx #'ignore #'ignore)
          (should (equal "Grep" (plist-get captured-entry :tool-name)))
          (should (eq :path (plist-get captured-entry :specifier-key)))
          (should (equal root-without-slash
                         (plist-get captured-entry :specifier-value))))
      (delete-directory root t)))
  :doc "error from async prompt callback surfaces through fail, not a strand"
  ;; When apply-prompt-result throws (e.g. a persistent-rule write
  ;; failing), the error fires after the runner's outer `condition-case'
  ;; has already unwound - the dispatcher must catch and route to
  ;; `fail' rather than letting the error escape and strand the FSM.
  (let* ((tool (mevedel-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'ask))
                :read-only-p nil))
         (session (mevedel-session--create :name "test"))
         (ctx (list :tool tool :args nil :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         next-called fail-reason)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-attributed)
               (lambda (_t _p _a _origin cont &optional _count _entry)
                 (funcall cont 'always-allow)))
              ((symbol-function 'mevedel-permission--apply-prompt-result)
               (lambda (&rest _) (error "Disk write failed"))))
      (let ((mevedel--session session))
        (mevedel-tool-permission-step
         ctx
         (lambda (_c) (setq next-called t))
         (lambda (r &rest _) (setq fail-reason r)))))
    (should-not next-called)
    (should (stringp fail-reason))
    (should (string-match-p "disk write failed" fail-reason)))
  :doc "fail-closed PermissionRequest stop beats allow"
  (let* ((tool (mevedel-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules '(("Edit" :action ask)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         fail-reason fail-context
         next-called)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback
                          '(:continue nil
                                      :stop-reason "hook failed"
                                      :permission-decision allow)))))
      (mevedel-tool-permission-step
       ctx
       (lambda (_c) (setq next-called t))
       (lambda (reason context &rest _)
         (setq fail-reason reason
               fail-context context))))
    (should-not next-called)
    (should (equal fail-reason
                   "Permission denied: blocked by PermissionRequest: hook failed"))
    (let ((record (car (plist-get fail-context :hook-audit-records))))
      (should (eq (plist-get record :type) 'tool-permission))
      (should (equal (plist-get record :event)
                     "PermissionRequest"))
      (should (equal (plist-get record :outcome) "deny"))))
  :doc "generic Bash and Eval asks run PermissionRequest before real queue admission"
  (dolist (case
           (list
            (list (mevedel-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil 'generic)
            (list (mevedel-tool-ensure "Bash")
                  '(:command "unknown-hook-probe") 'bash)
            (list (mevedel-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live") 'eval)))
    (pcase-let* ((`(,tool ,args ,kind) case)
                 (session (mevedel-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (request (mevedel-request--create
                           :id "request-permission-hook"
                           :session session
                           :origin "/root/worker"))
                 (mevedel--session session)
                 (mevedel-permission-rules nil)
                 (mevedel-protected-paths nil)
                 (mevedel-permission-guardian nil)
                 (hook-count 0))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (when (eq event 'PermissionRequest)
                     (cl-incf hook-count))
                   (funcall callback nil)))
                ((symbol-function
                  'mevedel-permission-queue--render-entry)
                 #'ignore))
        (mevedel-tool-permission-step
         (list :tool tool :args args :session session
               :request request)
         #'ignore #'ignore)
        (let ((entry (car (mevedel-session-permission-queue
                           session))))
          (should entry)
          (should (eq kind (plist-get entry :kind)))
          (should
           (equal "request-permission-hook"
                  (plist-get entry :request-id)))
          (should (= 1 hook-count))
          (mevedel-permission-queue--render-head session)
          (mevedel-permission-queue--reevaluate entry)
          (should (= 1 hook-count))))))
  :doc "PreToolUse allow runs PermissionRequest and skips all cards"
  (dolist (case
           (list
            (list (mevedel-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (mevedel-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (mevedel-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (mevedel-session--create
                           :name "pre-tool-allow"
                           :permission-mode 'ask))
                 (mevedel-permission-rules nil)
                 (mevedel-protected-paths nil)
                 (mevedel-permission-guardian nil)
                 (enqueued nil)
                 (events nil)
                 (next-called nil))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (funcall callback nil)))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (&rest _) (setq enqueued t))))
        (mevedel-tool-permission-step
         (list :tool tool :args args :session session
               :hook-permission-decision 'allow)
         (lambda (_context) (setq next-called t)) #'ignore))
      (should next-called)
      (should-not enqueued)
      (should (equal '(PermissionRequest) events))))
  :doc "PermissionRequest allow settles generic Bash and Eval asks without cards"
  (dolist (case
           (list
            (list (mevedel-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (mevedel-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (mevedel-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (mevedel-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (mevedel-permission-rules nil)
                 (mevedel-protected-paths nil)
                 (mevedel-permission-guardian nil)
                 (enqueued nil)
                 (next-called nil))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback
                            '(:permission-decision allow))))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (&rest _) (setq enqueued t))))
        (mevedel-tool-permission-step
         (list :tool tool :args args :session session)
         (lambda (_context) (setq next-called t)) #'ignore))
      (should next-called)
      (should-not enqueued)))
  :doc "PermissionRequest deny settles generic Bash and Eval asks with provenance"
  (dolist (case
           (list
            (list (mevedel-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (mevedel-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (mevedel-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (mevedel-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (mevedel-permission-rules nil)
                 (mevedel-protected-paths nil)
                 (mevedel-permission-guardian nil)
                 (events nil)
                 (provenance nil)
                 (enqueued nil)
                 (failure nil))
      (cl-letf (((symbol-function 'mevedel-hooks-run-event)
                 (lambda (event payload callback &rest _)
                   (push event events)
                   (when (eq event 'PermissionDenied)
                     (setq provenance
                           (plist-get payload
                                      :permission-provenance)))
                   (funcall callback
                            (and (eq event 'PermissionRequest)
                                 '(:permission-decision deny
                                                        :permission-reason "hook denied")))))
                ((symbol-function 'mevedel-permission--enqueue)
                 (lambda (&rest _) (setq enqueued t))))
        (mevedel-tool-permission-step
         (list :tool tool :args args :session session)
         #'ignore (lambda (reason &rest _) (setq failure reason))))
      (should (equal '(PermissionRequest PermissionDenied)
                     (nreverse events)))
      (should (eq 'PermissionRequest provenance))
      (should (string-match-p "hook denied" failure))
      (should-not enqueued)))
  :doc "policy denials retain their rule provenance"
  (let* ((tool (mevedel-tool--create
                :name "Edit" :read-only-p nil))
         (mevedel-permission-rules '(("Edit" :action deny)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         provenance)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (event payload callback &rest _)
                 (when (eq event 'PermissionDenied)
                   (setq provenance
                         (plist-get payload :permission-provenance)))
                 (funcall callback nil))))
      (mevedel-tool-permission-step
       (list :tool tool :args nil) #'ignore #'ignore))
    (should (eq 'deny-rule provenance)))
  :doc "queued user denials retain user provenance"
  (let* ((tool (mevedel-tool--create
                :name "Edit" :read-only-p nil))
         (mevedel-permission-rules '(("Edit" :action ask)))
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         entry
         provenance)
    (cl-letf (((symbol-function 'mevedel-hooks-run-event)
               (lambda (event payload callback &rest _)
                 (when (eq event 'PermissionDenied)
                   (setq provenance
                         (plist-get payload :permission-provenance)))
                 (funcall callback nil)))
              ((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued))))
      (mevedel-tool-permission-step
       (list :tool tool :args nil) #'ignore #'ignore)
      (funcall (plist-get entry :callback) 'deny-once))
    (should (eq 'user provenance)))

  :doc "sub-agent sees parent's session rules through context :session"
  ;; The agent buffer carries `mevedel--session' set buffer-locally
  ;; to the parent session struct (by reference).  When the pipeline
  ;; captures `mevedel--session' at tool entry and threads it
  ;; through `:session' on the context, the permission step honors
  ;; the parent's session rules.
  (let* ((tool (mevedel-tool--create
                :name "Edit" :read-only-p nil :groups '(edit)))
         (parent-session
          (mevedel-session--create
           :name "parent"
           :permission-rules '(("Edit" :action allow))))
         ;; Agent buffer would carry the same struct by reference.
         (sub-agent-session-alias parent-session)
         (ctx (list :tool tool :args nil
                    :session sub-agent-session-alias))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask)
         called)
    (mevedel-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))

  :doc "rule added through sub-agent alias is visible on parent's struct"
  (let* ((parent-session (mevedel-session--create :name "parent"))
         (sub-agent-alias parent-session))
    (mevedel-permission--add-session-rule
     sub-agent-alias "Edit" 'allow "/foo/*")
    (should (equal (mevedel-session-permission-rules parent-session)
                   '(("Edit" :path "/foo/*" :action allow)))))

  :doc "permission-mode toggle on parent observed by sub-agent next call"
  (let* ((tool (mevedel-tool--create
                :name "Edit" :read-only-p nil :groups '(edit)))
         (parent-session
          (mevedel-session--create
           :name "parent" :permission-mode 'ask))
         (sub-agent-alias parent-session)
         (ctx (list :tool tool :args nil :session sub-agent-alias))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'ask))
    (setf (mevedel-session-permission-mode parent-session) 'full-auto)
    (let (next-called)
      (mevedel-tool-permission-step
       ctx (lambda (_context) (setq next-called t)) #'ignore)
      (should next-called)))

  :doc "tripwire warning fires when non-read-only tool runs without session"
  (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'full-auto)
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (kind msg &rest _)
                 (when (and (eq kind 'mevedel)
                            (string-match-p "no session in context" msg))
                   (setq warned t)))))
      (mevedel-tool-permission-step
       ctx (lambda (_c) nil) #'ignore))
    (should warned))

  :doc "no tripwire when session is present"
  (let* ((tool (mevedel-tool--create :name "Edit" :read-only-p nil))
         (session (mevedel-session--create :name "p"))
         (ctx (list :tool tool :args nil :session session))
         (mevedel-permission-rules nil)
         (mevedel-protected-paths nil)
         (mevedel-permission-mode 'full-auto)
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (kind msg &rest _)
                 (when (and (eq kind 'mevedel)
                            (string-match-p "no session in context" msg))
                   (setq warned t)))))
      (mevedel-tool-permission-step
       ctx (lambda (_c) nil) #'ignore))
    (should-not warned)))

(provide 'test-mevedel-tool-permission)
;;; test-mevedel-tool-permission.el ends here
