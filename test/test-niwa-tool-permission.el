;;; test-niwa-tool-permission.el -- Tool permission tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests permission-path fan-out, decision logging, hooks, and prompt
;; orchestration independently from Pipeline step ordering.

;;; Code:

(require 'niwa-tool-web-registration)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-agents)
(require 'niwa-bash-analysis)
(require 'niwa-bash-policy)
(require 'niwa-execution)
(require 'niwa-hooks)
(require 'niwa-permission-log)
(require 'niwa-permission-prompt)
(require 'niwa-permission-queue)
(require 'niwa-permission-rules)
(require 'niwa-permissions)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'niwa-telemetry)
(require 'niwa-tool-exec)
(require 'niwa-tool-patch)
(require 'niwa-tool-permission)
(require 'niwa-tool-registry)
(require 'niwa-tool-web)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))


;;

;;; Permission step

(niwa-deftest niwa-tool-permission--origin ()
  ,test
  (test)
  :doc "prefers an explicit prompt origin over request and invocation owners"
  (let ((request
         (niwa-request--create :origin "/root/worker")))
    (should
     (equal
      "/root/worker/verifier"
      (niwa-tool-permission--origin
       (list :request request)
       "/root/worker/verifier"))))
  :doc "uses the request path before an agent or root fallback"
  (let ((request
         (niwa-request--create :origin "/root/worker")))
    (should
     (equal
      "/root/worker"
      (niwa-tool-permission--origin (list :request request)))))
  :doc "falls back to root without a scoped owner"
  (should (equal "/root" (niwa-tool-permission--origin nil))))

(niwa-deftest niwa-tool-permission-log-decision ()
  ,test
  (test)
  :doc "carries nested identity and source into the permission log"
  (let* ((niwa-permission-log-enabled t)
         (session (niwa-session--create
                   :name "permission-log" :permission-mode 'ask))
         (tool (niwa-tool--create :name "Child"))
         (context (list :session session :tool tool
                        :tool-use-id "parent/1"
                        :parent-tool-use-id "parent"
                        :call-source 'ptc)))
    (niwa-tool-permission-log-decision
     context '(:outcome allow :raw-outcome allow :via test))
    (let ((entry (car (niwa-session-permission-log-pending session))))
      (should (equal "parent/1" (plist-get entry :tool-use-id)))
      (should (equal "parent" (plist-get entry :parent-tool-use-id)))
      (should (eq 'ptc (plist-get entry :call-source))))))

(defun test-niwa-tool-permission--decide (rules &optional mode hook-rules)
  "Decide WebFetch of example.org under session RULES, MODE and HOOK-RULES.
Return the outcome and the logged entry."
  (let* ((niwa-permission-log-enabled t)
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-hook-rules hook-rules)
         (session (niwa-session--create
                   :name "decide-now" :permission-mode (or mode 'ask)
                   :permission-rules rules)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (let ((outcome (niwa-tool-permission-decide-now
                      "WebFetch" '(:url "https://example.org/x") nil 'redirect)))
        (list outcome
              (car (last (niwa-session-permission-log-pending session))))))))

(niwa-deftest niwa-tool-permission-decide-now
  (:before-each (niwa-tool-web--register))
  ,test
  (test)
  :doc "allows a read-only call with no rule through the permission mode"
  (pcase-let ((`(,outcome ,entry) (test-niwa-tool-permission--decide nil)))
    (should (eq 'allow outcome))
    (should (eq 'mode (plist-get entry :via)))
    (should (eq 'redirect (plist-get entry :reason)))
    (should (equal "example.org" (plist-get entry :specifier-value))))

  :doc "follows session allow, ask and deny rules for the target domain"
  (should (eq 'allow (car (test-niwa-tool-permission--decide
                           '(("WebFetch" :domain "*.org" :action allow))))))
  (should (eq 'ask (car (test-niwa-tool-permission--decide
                         '(("WebFetch" :domain "example.org" :action ask))))))
  (should (eq 'deny (car (test-niwa-tool-permission--decide
                          '(("WebFetch" :domain "example.org" :action deny))))))
  (should (eq 'allow (car (test-niwa-tool-permission--decide
                           '(("WebFetch" :domain "other.org" :action deny))))))

  :doc "turns allow into ask when a PreToolUse hook would run for the tool"
  (pcase-let ((`(,outcome ,entry)
               (test-niwa-tool-permission--decide
                nil nil '((PreToolUse ((:matcher "WebFetch"
                                        :hooks ((:type command :command "true")))))))))
    (should (eq 'ask outcome))
    (should (eq 'pre-tool-hook (plist-get entry :via))))
  (should (eq 'allow (car (test-niwa-tool-permission--decide
                           nil nil '((PreToolUse ((:matcher "Bash"
                                                   :hooks ((:type command :command "true"))))))))))

  :doc "a hook never relaxes a deny"
  (should (eq 'deny (car (test-niwa-tool-permission--decide
                          '(("WebFetch" :domain "example.org" :action deny))
                          nil '((PreToolUse ((:matcher "WebFetch"
                                              :hooks ((:type command :command "true"))))))))))

  :doc "asks without a live buffer or session"
  (let ((buffer (generate-new-buffer " *decide-now*")))
    (kill-buffer buffer)
    (should (eq 'ask (niwa-tool-permission-decide-now
                      "WebFetch" '(:url "https://example.org/") buffer))))
  (with-temp-buffer
    (should (eq 'ask (niwa-tool-permission-decide-now
                      "WebFetch" '(:url "https://example.org/"))))))

(niwa-deftest niwa-tool-permission--denial-outcome-p ()
  ,test
  (test)
  :doc "recognizes every interactive denial outcome"
  (dolist (outcome '(deny deny-once deny-session
                          (deny . "reason") (feedback . "reason")))
    (should (niwa-tool-permission--denial-outcome-p outcome)))
  :doc "rejects allow, abort, and unrelated compound outcomes"
  (dolist (outcome '(allow allow-once allow-session always-allow aborted
                           (allow . "reason") (unknown . "reason") nil))
    (should-not (niwa-tool-permission--denial-outcome-p outcome))))

(niwa-deftest niwa-tool-permission--denial-provenance ()
  ,test
  (test)
  :doc "prefers stored interactive provenance over decision metadata"
  (should
   (eq 'user
       (niwa-tool-permission--denial-provenance
        '(:permission-denial-provenance user)
        '(:via deny-rule))))
  :doc "uses decision metadata when no interactive provenance is stored"
  (should
   (eq 'deny-rule
       (niwa-tool-permission--denial-provenance
        nil '(:via deny-rule))))
  :doc "falls back to policy when neither source is present"
  (should
   (eq 'policy
       (niwa-tool-permission--denial-provenance nil nil))))

(niwa-deftest niwa-tool-permission--request ()
  ,test
  (test)
  :doc "unresolved requests enter the queue and user denials retain provenance"
  (niwa-pq-test--with-queue (session _data view)
    (let (progress settled-context settled-outcome)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil))))
        (niwa-tool-permission--request
         (list :progress-callback (lambda (event) (setq progress event))
               :tool-use-id "ptc-1/2"
               :parent-tool-use-id "ptc-1"
               :call-source 'ptc)
         '(:kind generic :origin "/root" :callback ignore) session
         (lambda (context outcome)
           (setq settled-context context settled-outcome outcome)))
        (let* ((card (niwa-pq-test--card view))
               (queued (plist-get card :entry)))
          (should queued)
          (should (eq 'permission-wait progress))
          (should (equal "ptc-1/2" (plist-get queued :tool-use-id)))
          (should (equal "ptc-1" (plist-get queued :parent-tool-use-id)))
          (should (eq 'ptc (plist-get queued :call-source)))
          (niwa-pq-test--press card "D"))
        (should (eq 'deny-session settled-outcome))
        (should (eq 'user
                    (plist-get settled-context
                               :permission-denial-provenance))))))
  :doc "hook allow and deny settle without queue admission"
  (dolist (decision '((:permission-decision allow)
                      (:permission-decision deny
                                            :permission-reason "blocked")))
    (niwa-pq-test--with-queue (session _data _view)
      (let ((niwa-permission-log-enabled t)
            settled-context settled-outcome)
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback decision))))
          (niwa-tool-permission--request
           nil '(:kind generic :origin "/root" :callback ignore) session
           (lambda (context outcome)
             (setq settled-context context settled-outcome outcome)))
          (should-not (niwa-pq-test--requested-p session))
          (if (eq 'allow (plist-get decision :permission-decision))
              (should (eq 'allow settled-outcome))
            (should (eq 'deny (car settled-outcome)))
            (should (eq 'PermissionRequest
                        (plist-get settled-context
                                   :permission-denial-provenance))))))))
  :doc "hook approval cannot authorize an unrepresentable directory write"
  (let ((root (make-temp-file "niwa-hook-directory-" t))
        outcome)
    (unwind-protect
        (dolist (available-p '(t nil))
          (with-temp-buffer
            (setq-local niwa-permission-mode 'edits)
            (cl-letf (((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _payload callback &rest _)
                         (funcall callback '(:permission-decision allow))))
                      ((symbol-function 'niwa-sandbox-probe)
                       (lambda (&rest _)
                         (list :available available-p :reason "Fixture confinement"))))
              (niwa-tool-permission--request
               (list :buffer (current-buffer))
               (list :kind 'sandbox :requested-additional-permissions
                     (list :file-system (list (list :path root :access 'write))))
               nil (lambda (_context value) (setq outcome value)))
              (should (eq 'deny (car-safe outcome)))
              (should (string-match-p "directory-tree scope" (cdr outcome))))))
      (delete-directory root t)))

  :doc "hook, fallback and queued approvals retain only their explicit resource selection"
  (dolist (source '(hook fallback queued coalesced denied))
    (niwa-pq-test--with-queue (session _data view)
      (let* ((selection '((:path "/service/socket" :access write)))
             ;; Read access, so the head's session grant can settle the sibling
             ;; through the real queue; the selection under test stays a write.
             (entry (list :kind 'generic :tool-name "Read" :origin "/root"
                          :specifier-key :path
                          :specifier-value "/service/socket"
                          :resource-access 'read :callback #'ignore
                          :resource-selection-cell (list (copy-tree selection))))
             settled-context settled-outcome)
        (cl-letf (((symbol-function 'niwa-hooks-run-event)
                   (lambda (_event _payload callback &rest _)
                     (funcall callback
                              (pcase source
                                ('hook '(:permission-decision allow))
                                ('denied '(:permission-decision deny)))))))
          (when (eq source 'coalesced)
            ;; A head request answered for the session stores the grant that
            ;; the real queue then uses to settle this sibling.
            (niwa-permission-request
             (list :kind 'generic :tool-name "Read" :origin "/root"
                   :specifier-key :path :specifier-value "/service/socket"
                   :resource-access 'read :include-always nil
                   :callback (lambda (outcome)
                               (when (eq outcome 'allow-session)
                                 (niwa-permission-add-session-resource-grant
                                  session "/service/socket" 'read))))
             session))
          (niwa-tool-permission--request
           nil entry session
           (lambda (context outcome)
             (setq settled-context context settled-outcome outcome))
           nil (and (eq source 'fallback) 'allow))
          (when (eq source 'queued)
            (niwa-pq-test--press (niwa-pq-test--card view) "a"))
          (when (eq source 'coalesced)
            (niwa-pq-test--press
             (car (niwa-pq-test--cards view)) "s"))
          (if (memq source '(denied coalesced))
              (progn
                (if (eq source 'denied)
                    (should (eq 'deny (car settled-outcome)))
                  (should (eq 'allow settled-outcome)))
                (should-not (plist-get settled-context :approved-resources)))
            (should (memq settled-outcome '(allow allow-once)))
            (should (equal selection (plist-get settled-context :approved-resources)))
            (should-not (eq (car (plist-get entry :resource-selection-cell))
                            (plist-get settled-context :approved-resources))))))))

  :doc "an unresolved request uses its card-free fallback"
  (niwa-pq-test--with-queue (session _data _view)
    (let ((niwa-permission-log-enabled t)
          settled-outcome)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback nil))))
        (niwa-tool-permission--request
         nil '(:kind generic :origin "/root" :callback ignore) session
         (lambda (_context outcome) (setq settled-outcome outcome))
         nil 'allow)
        (should-not (niwa-pq-test--requested-p session))
        (should (eq 'allow settled-outcome)))))
  :doc "an explicit hook ask overrides the card-free fallback"
  (niwa-pq-test--with-queue (session _data view)
    (let (settled-outcome)
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback '(:permission-decision ask)))))
        (niwa-tool-permission--request
         nil '(:kind generic :origin "/root" :callback ignore) session
         (lambda (_context outcome) (setq settled-outcome outcome))
         nil 'allow)
        (should (niwa-pq-test--card view))
        (should-not settled-outcome))))
  :doc "one-shot prompts retain their authoritative side data buffer"
  (let ((side-buffer (generate-new-buffer " *niwa-side-queue*"))
        (session (niwa-pq-test--make-session)))
    (unwind-protect
        (with-current-buffer side-buffer
          (setq-local niwa--session session)
          (niwa-pq-test--with-view-here view
            (cl-letf (((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _payload callback &rest _)
                         (funcall callback nil))))
              (niwa-tool-permission--request
               `(:one-shot-mutations-p t :buffer ,side-buffer)
               '(:kind generic :mutation-p t :origin "/root" :callback ignore)
               session #'ignore)
              (let ((queued (plist-get (niwa-pq-test--card view) :entry)))
                (should (plist-get queued :once-only))
                (should (eq side-buffer (plist-get queued :data-buffer)))))
            (niwa-permission-queue-abort-all session)))
      (kill-buffer side-buffer))))

(niwa-deftest niwa-tool-permission-step
  (:before-each
   (advice-add 'display-warning :around
               #'niwa-test--drop-sessionless-permission-warning)
   :after-each
   (advice-remove 'display-warning
                  #'niwa-test--drop-sessionless-permission-warning))
  ,test
  (test)
  :doc "allows read-only tool in ask mode"
  (let* ((tool (niwa-tool--create
                :name "Read"
                :read-only-p t))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "refreshes global grants before admission and honors explicit workspace denies"
  (let* ((dir (make-temp-file "niwa-global-admission-" t))
         (niwa-user-dir (file-name-concat dir "global"))
         (workspace (niwa-workspace--create :type 'project :root dir))
         (session (niwa-session--create :workspace workspace :permission-mode 'ask))
         (path (file-name-concat dir "protected"))
         (tool (niwa-tool--create :name "Read" :read-only-p t))
         (context (list :tool tool :session session :workspace workspace
                        :permission-path path))
         (file (niwa-permission-persistence-file workspace 'global))
         (niwa-permission-rules nil)
         (niwa-protected-paths (list (cons path 'inaccessible)))
         allowed denied)
    (unwind-protect
        (niwa-pq-test--with-session-view session view
          (niwa-permission-persistence-write-store
           file `(:rules nil :resource-grants ((:path ,path :access read))))
          (niwa-tool-permission-step
           context (lambda (_) (setq allowed t)) #'ignore)
          (should allowed)
          (should-not (niwa-pq-test--requested-p session))
          ;; External revocation must not be hidden by the store cache.
          (with-temp-file file
            (insert "(:rules nil :resource-grants nil)"))
          (setq allowed nil)
          (niwa-tool-permission-step
           context (lambda (_) (setq allowed t)) #'ignore)
          (should-not allowed)
          (should (niwa-session-permission-queue session))
          (niwa-permission-persistence-write-store
           file `(:rules nil :resource-grants ((:path ,path :access read))))
          (niwa-permission-persistence-save-rule
           workspace '("Read" :action deny))
          (niwa-tool-permission-step
           context (lambda (_) (setq allowed t))
           (lambda (&rest _) (setq denied t)))
          (should denied)
          (should-not allowed))
      (delete-directory dir t)))
  :doc "observes an external persistent revocation at the next tool"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-refresh-" t)))
         (niwa-user-dir (file-name-concat dir "global"))
         (workspace (niwa-workspace--create
                     :type 'project :id dir :root dir :name "refresh"))
         (session (niwa-session--create
                   :name "refresh" :workspace workspace
                   :permission-mode 'full-auto))
         (tool (niwa-tool--create :name "Edit" :read-only-p nil))
         (file (niwa-permission-persistence-file workspace 'workspace))
         (context (list :tool tool :args nil
                        :session session :workspace workspace))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         allowed denied)
    (unwind-protect
        (progn
          (clrhash niwa-permission--store-cache)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "(:rules ((\"Edit\" :action allow)) :resource-grants nil)"))
          (niwa-tool-permission-step
           context (lambda (_) (setq allowed t)) #'ignore)
          (should allowed)
          ;; Simulate another Emacs changing the workspace-owned store.
          (with-temp-file file
            (insert "(:rules ((\"Edit\" :action deny)) :resource-grants nil)"))
          (niwa-tool-permission-step
           context #'ignore (lambda (&rest _) (setq denied t)))
          (should denied))
      (clrhash niwa-permission--store-cache)
      (delete-directory dir t)))
  :doc "keeps one persistent snapshot throughout an admitted aggregate tool"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/tmp/snapshot/"
                     :root "/tmp/snapshot/" :name "snapshot"))
         (session (niwa-session--create
                   :name "snapshot" :workspace workspace
                   :permission-mode 'full-auto))
         (tool (niwa-tool--create
                :name "AggregateEdit" :groups '(edit)
                :get-paths (lambda (_) '("/tmp/snapshot/a"
                                         "/tmp/snapshot/b"))))
         (rules '(("AggregateEdit" :action allow)))
         (grants '((:path "/tmp/snapshot" :access write)))
         contexts pending completed)
    (cl-letf (((symbol-function 'niwa-permission-persistence-refresh)
               (lambda (_workspace continuation &optional _on-cancel _on-error)
                 (funcall continuation)))
              ((symbol-function 'niwa-permission-persistence-load-rules)
               (lambda (_) rules))
              ((symbol-function
                'niwa-permission-persistence-load-resource-grants)
               (lambda (_) grants))
              ((symbol-function 'niwa-check-permission-async-with-metadata)
               (lambda (_tool continuation &rest args)
                 (push (plist-get args :normalized-context) contexts)
                 (setq pending continuation))))
      (niwa-tool-permission-step
       (list :tool tool :args nil :session session)
       (lambda (_) (setq completed t)) #'ignore)
      (should (= 1 (length contexts)))
      ;; Another Emacs changes authority after this invocation was admitted.
      (setq rules '(("AggregateEdit" :action deny))
            grants nil)
      (funcall pending '(:outcome allow :raw-outcome allow :via test))
      (should (= 2 (length contexts)))
      (should (equal '(("AggregateEdit" :action allow))
                     (plist-get (car contexts) :persistent-rules)))
      (should (equal '((:path "/tmp/snapshot" :access write))
                     (plist-get (car contexts) :resource-grants)))
      (funcall pending '(:outcome allow :raw-outcome allow :via test))
      (should completed)))
  :doc "keeps a frozen aggregate snapshot across an async continuation"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/tmp/frozen/"
                     :root "/tmp/frozen/" :name "frozen"))
         (session (niwa-session--create
                   :name "frozen" :workspace workspace
                   :permission-mode 'full-auto))
         (tool (niwa-tool--create
                :name "AggregateEdit" :groups '(edit)
                :get-paths (lambda (_) '("/tmp/frozen/a"
                                         "/tmp/frozen/b"))))
         contexts pending completed)
    (with-temp-buffer
      (setq-local niwa-permission--context-frozen-p t
                  niwa-permission--frozen-persistent-rules
                  '(("AggregateEdit" :action allow))
                  niwa-permission--frozen-resource-grants
                  '((:path "/tmp/frozen" :access write)))
      (cl-letf (((symbol-function 'niwa-permission-persistence-refresh)
                 (lambda (&rest _) (error "Frozen context refreshed")))
                ((symbol-function
                  'niwa-check-permission-async-with-metadata)
                 (lambda (_tool continuation &rest args)
                   (push (plist-get args :normalized-context) contexts)
                   (setq pending continuation))))
        (niwa-tool-permission-step
         (list :tool tool :args nil :session session
               :buffer (current-buffer))
         (lambda (_) (setq completed t)) #'ignore)
        (should (= 1 (length contexts)))
        ;; Permission callbacks are free to resume from another buffer.
        (with-temp-buffer
          (funcall pending '(:outcome allow :raw-outcome allow :via test)))
        (should (= 2 (length contexts)))
        (should (equal '(("AggregateEdit" :action allow))
                       (plist-get (car contexts) :persistent-rules)))
        (should (equal '((:path "/tmp/frozen" :access write))
                       (plist-get (car contexts) :resource-grants)))
        (funcall pending '(:outcome allow :raw-outcome allow :via test))
        (should completed))))
  :doc "authorizes every path declared by one aggregate edit"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-multi-permission-" t)))
         (path-a (file-name-concat dir "a.txt"))
         (path-b (file-name-concat dir "b.txt"))
         (gptel--known-tools nil)
         (niwa-tool--registry (make-hash-table :test #'equal))
         (session (niwa-session--create
                   :name "multi" :save-path dir
                   :permission-mode 'full-auto
                   :resource-grants
                   (list (list :path path-a :access 'write)
                         (list :path path-b :access 'write))))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-log-enabled t)
         called)
    (unwind-protect
        (progn
          (niwa-define-tool
           :wrap (gptel-make-tool
                  :name "AggregateEdit"
                  :function #'ignore
                  :description "Edit several files"
                  :args nil
                  :category "test-aggregate")
           :groups (edit)
           :get-paths (lambda (_args) (list path-a path-b)))
          (niwa-tool-permission-step
           (list :tool (niwa-tool-get
                        "AggregateEdit" "niwa-test-aggregate")
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
                    (niwa-test--permission-log-entries
                     session))))))
      (delete-directory dir t)))
  :doc "rejects an aggregate edit when any affected path is denied"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-multi-denial-" t)))
         (path-a (file-name-concat dir "a.txt"))
         (path-b (file-name-concat dir "b.txt"))
         (session (niwa-session--create
                   :name "multi-denial" :save-path dir
                   :permission-mode 'full-auto))
         (tool (niwa-tool--create
                :name "ApplyPatch" :groups '(edit)
                :get-paths (lambda (_args) (list path-a path-b))))
         (niwa-permission-rules
          (list (list "ApplyPatch" :path path-a :action 'allow)
                (list "ApplyPatch" :path path-b :action 'deny)))
         (niwa-protected-paths nil)
         next-called failure)
    (unwind-protect
        (progn
          (niwa-tool-permission-step
           (list :tool tool :args nil :session session)
           (lambda (_context) (setq next-called t))
           (lambda (message &rest _) (setq failure message)))
          (should-not next-called)
          (should (string-match-p "Permission denied" failure)))
      (delete-directory dir t)))
  :doc "Bash and network asks render as one combined authority card"
  (let* ((tool (niwa-tool-ensure "Bash"))
         (session (niwa-session--create
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
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-reviewer 'user)
         content)
    (cl-letf
        (((symbol-function 'niwa-permission-queue--render-entry)
          #'ignore))
      (niwa-tool-permission-step ctx #'ignore #'ignore))
    (should (= 1 (length
                  (niwa-session-permission-queue session))))
    (cl-letf
        (((symbol-function
           'niwa-permission--prompt-async-with-content)
          (lambda (text &rest _args)
            (setq content
                  (substring-no-properties text)))))
      (let ((queued (car (niwa-session-permission-queue session))))
        (niwa-permission-queue--render-entry queued)
        ;; Folded: the reason and complete scope, not the breakdown.
        (should (string-match-p "Model's reason: Reach the package registry?" content))
        (should (string-match-p "Scope: Network" content))
        (should-not (string-match-p "Authority" content))
        (niwa-queue--entry-metadata-put queued :expanded t)
        (niwa-permission-queue--render-entry queued)))
    (should (string-match-p "Authority" content))
    (should (string-match-p "\\[ \\] Command" content))
    (should (string-match-p "\\[ \\] Network" content)))
  :doc "fails with Permission denied when rules deny"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules '(("Edit" :action deny)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason fail-context fail-kind)
    (niwa-tool-permission-step
     ctx #'ignore
     (lambda (reason context kind)
       (setq fail-reason reason
             fail-context context
             fail-kind kind)))
    (should (equal fail-reason "Permission denied"))
    (should (equal fail-context ctx))
    (should (eq fail-kind 'permission-denied)))
  :doc "allows when explicit allow rule matches"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules '(("Edit" :action allow)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "allows in full-auto mode"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'full-auto)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "full-auto allow writes permission-decision diagnostic"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-log-" t)))
         (session (niwa-session--create
                   :name "test" :save-path dir
                   :permission-mode 'full-auto))
         (tool (niwa-tool--create
                :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (niwa-tool-permission-step ctx #'ignore #'ignore)
          (let ((entry (car (niwa-test--permission-log-entries
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
               (make-temp-file "niwa-permission-log-" t)))
         (session (niwa-session--create
                   :name "test" :save-path dir
                   :permission-rules '(("Edit" :action allow))))
         (tool (niwa-tool--create
                :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (progn
          (niwa-tool-permission-step ctx #'ignore #'ignore)
          (let ((entry (car (niwa-test--permission-log-entries
                             session))))
            (should (eq 'permission-decision
                        (plist-get entry :event)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'rule (plist-get entry :via)))
            (should (eq :session (plist-get entry :bucket)))))
      (delete-directory dir t)))
  :doc "protected path prompt logs decision and queue events"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-permission-log-" t)))
         (path (expand-file-name ".git/config" dir))
         (session (niwa-session--create
                   :name "test" :save-path dir))
         (tool (niwa-tool--create
                :name "Write" :read-only-p nil
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths
          (list (cons path 'inaccessible)))
         (niwa-permission-mode 'ask)
         (niwa-permission-log-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-permission--prompt-async-attributed)
                   (lambda (&rest _args) nil)))
          (make-directory (file-name-directory path) t)
          (niwa-tool-permission-step ctx #'ignore #'ignore)
          (let ((entries (niwa-test--permission-log-entries
                          session)))
            (should (eq 'permission-decision
                        (plist-get (nth 0 entries) :event)))
            (should (eq 'ask (plist-get (nth 0 entries) :outcome)))
            (should (eq 'protected-path (plist-get (nth 0 entries) :via)))
            (should (plist-get (nth 0 entries) :protected-path))
            (should (eq 'permission-enqueued
                        (plist-get (nth 1 entries) :event)))))
      (delete-directory dir t)))
  :doc "tool check-permission returning allow is respected"
  (let* ((tool (niwa-tool--create
                :name "CustomTool"
                :check-permission (lambda (_ts _input) 'allow)
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "reads session rules from context, not buffer-local"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (session (niwa-session--create
                   :name "test"
                   :permission-rules '(("Edit" :action allow))))
         (ctx (list :tool tool :args nil :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "ignores buffer-local session - only context :session counts"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules '(("Edit" :action deny)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         (niwa--session (niwa-session--create
                            :name "phantom"
                            :permission-rules '(("Edit" :action allow))))
         fail-reason)
    ;; The dynamic niwa--session has an allow rule but the step must
    ;; not look at it; only the missing :session in `ctx' applies.
    (niwa-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied")))
  :doc "sync slot signaling permission-denied surfaces REASON via fail"
  (let* ((tool (niwa-tool--create
                :name "Custom"
                :check-permission
                (lambda (_ts _input)
                  (signal 'niwa-permission-denied '("user feedback X")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason)
    (niwa-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: user feedback X")))
  :doc "async slot returning 'allow advances to next"
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'allow))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "async slot returning (deny . REASON) surfaces via fail"
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont)
                  (funcall cont '(deny . "Custom slot reason")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason)
    (niwa-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: Custom slot reason")))
  :doc "async slot returning (feedback . TEXT) maps to scoped denial with text"
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont)
                  (funcall cont '(feedback . "user typed this")))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason)
    (niwa-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "Permission denied: user typed this")))
  :doc "async slot returning 'aborted surfaces as fail aborted"
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'aborted))
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason)
    (niwa-tool-permission-step
     ctx #'ignore (lambda (r &rest _) (setq fail-reason r)))
    (should (equal fail-reason "aborted")))
  :doc "async slot returning nil falls through to chain"
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont nil))
                :read-only-p t))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    ;; Read-only + ask mode -> step 8 returns 'allow.
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))
  :doc "paths inside allowed roots advance without prompting"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (extra (file-name-as-directory
                 (make-temp-file "niwa-pipeline-extra-" t)))
         (path (file-name-concat extra "file.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws
                   :permission-mode 'edits))
         (tool (niwa-tool--create
                :name "Write"
                :groups '(edit)
                :read-only-p nil
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         called)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root extra))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should called)
            (should-not (niwa-pq-test--requested-p session))))
      (delete-directory root t)
      (delete-directory extra t)))
  :doc "ask prompts for native edits even inside allowed roots"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-ask-edit-" t)))
         (path (file-name-concat root "file.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "ask-edit" :root root
              :name "ask-edit" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws
                   :permission-mode 'ask))
         (tool (niwa-tool--create
                :name "Edit" :groups '(edit)
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool :args (list :file_path path)
                    :session session :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         called)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should-not called)
            (should (niwa-pq-test--card view))))
      (delete-directory root t)))
  :doc "edits advances native edit tools inside allowed roots"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-auto-edit-" t)))
         (ws (niwa-workspace--create
              :type 'project :id "auto-edit" :root root
              :name "auto-edit" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws
                   :permission-mode 'edits))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (dolist (spec '(("Edit" :file_path)
                          ("Write" :file_path)
                          ("MkDir" :path)))
            (let* ((name (car spec))
                   (key (cadr spec))
                   (path (file-name-concat root (downcase name)))
                   (tool (niwa-tool--create
                          :name name :groups '(edit)
                          :get-path (lambda (args)
                                      (plist-get args key))))
                   (ctx (list :tool tool :args (list key path)
                              :session session :workspace ws))
                   called)
              (niwa-pq-test--with-session-view session view
                (niwa-tool-permission-step
                 ctx (lambda (_c) (setq called t)) #'ignore)
                (should called)
                (should-not (niwa-pq-test--requested-p session))))))
      (delete-directory root t)))
  :doc "edits keeps Bash and Eval behind their permission checks"
  (dolist (name '("Bash" "Eval"))
    (let* ((session (niwa-session--create
                     :name "test" :permission-mode 'edits))
           (tool (niwa-tool--create
                  :name name :groups '(eval)))
           (ctx (list :tool tool :args nil :session session))
           (niwa-permission-rules nil)
           (niwa-protected-paths nil)
           called)
      (niwa-pq-test--with-session-view session view
        (niwa-tool-permission-step
         ctx (lambda (_c) (setq called t)) #'ignore)
        (should-not called)
        (should (niwa-pq-test--card view)))))
  :doc "session-scoped dropped-file grant allows exact Read outside workspace"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "niwa-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws))
         (tool (niwa-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (with-temp-file path (insert "dropped\n"))
    (niwa-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should called)
            (should-not (niwa-pq-test--requested-p session))))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "outside Read approval records exact session resource authority"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "niwa-pipeline-outside-" t)))
         (path (file-name-concat outside "file.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws))
         (tool (niwa-tool--create
                :name "Read" :read-only-p t
                :get-path (lambda (args)
                            (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (with-temp-file path (insert "outside\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should-not called)
            (let* ((card (niwa-pq-test--card view))
                   (captured-entry (plist-get card :entry)))
              (should (eq 'read
                          (plist-get captured-entry :resource-access)))
              (should (equal path
                             (plist-get captured-entry :specifier-value)))
              (niwa-pq-test--press card "s"))
            (should called)
            (should (equal (list (list :path path :access 'read))
                           (niwa-session-resource-grants session)))
            (should-not (niwa-session-permission-rules session))
            (setq called nil)
            (niwa-pq-test--forget-requests session)
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should called)
            (should-not (niwa-pq-test--requested-p session))))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "session-scoped dropped-file grant does not allow descendant Read"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "niwa-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped"))
         (descendant-dir (file-name-as-directory path))
         (descendant (file-name-concat descendant-dir "secret.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws))
         (tool (niwa-tool--create
                :name "Read"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path descendant)
                    :session session
                    :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (make-directory descendant-dir)
    (with-temp-file descendant (insert "secret\n"))
    (niwa-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should-not called)
            (should (niwa-pq-test--card view))))
      (when (file-exists-p descendant)
        (delete-file descendant))
      (when (file-directory-p descendant-dir)
        (delete-directory descendant-dir))
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "session-scoped dropped-file grant does not allow Write"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (outside (file-name-as-directory
                   (make-temp-file "niwa-pipeline-outside-" t)))
         (path (file-name-concat outside "dropped.txt"))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws))
         (tool (niwa-tool--create
                :name "Write"
                :read-only-p nil
                :get-path (lambda (args) (plist-get args :file_path))))
         (ctx (list :tool tool
                    :args (list :file_path path)
                    :session session
                    :workspace ws))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (with-temp-file path (insert "dropped\n"))
    (niwa-session-activate-dropped-file-grants session (list path))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa--all-allowed-roots)
                   (lambda (&optional _buffer) (list root))))
          (niwa-pq-test--with-session-view session view
            (niwa-tool-permission-step
             ctx (lambda (_c) (setq called t)) #'ignore)
            (should-not called)
            (should (niwa-pq-test--card view))))
      (delete-file path)
      (delete-directory outside t)
      (delete-directory root t)))
  :doc "workspace-root path is not broadened to parent directory when prompted"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-pipeline-root-" t)))
         (root-without-slash (directory-file-name root))
         (ws (niwa-workspace--create
              :type 'project :id "root" :root root
              :name "root" :file-cache nil))
         (session (niwa-session--create
                   :name "test" :workspace ws))
         (tool (niwa-tool--create
                :name "Grep"
                :read-only-p t
                :get-path (lambda (args) (plist-get args :path))))
         (ctx (list :tool tool
                    :args (list :path root-without-slash)
                    :session session
                    :workspace ws))
         (niwa-permission-rules
          `(("Grep" :path ,root-without-slash :action ask)))
         (niwa-protected-paths nil)
         )
    (unwind-protect
        (niwa-pq-test--with-session-view session view
          (niwa-tool-permission-step ctx #'ignore #'ignore)
          (let ((captured-entry (plist-get (niwa-pq-test--card view) :entry)))
            (should (equal "Grep" (plist-get captured-entry :tool-name)))
            (should (eq :path (plist-get captured-entry :specifier-key)))
            (should (equal root-without-slash
                           (plist-get captured-entry :specifier-value)))))
      (delete-directory root t)))
  :doc "error from async prompt callback surfaces through fail, not a strand"
  ;; When apply-prompt-result throws (e.g. a persistent-rule write
  ;; failing), the error fires after the runner's outer `condition-case'
  ;; has already unwound - the dispatcher must catch and route to
  ;; `fail' rather than letting the error escape and strand the FSM.
  (let* ((tool (niwa-tool--create
                :name "AsyncSlot"
                :check-permission-async
                (lambda (_ts _input cont) (funcall cont 'ask))
                :read-only-p nil))
         (session (niwa-session--create :name "test"))
         (ctx (list :tool tool :args nil :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         next-called fail-reason)
    (cl-letf (((symbol-function 'niwa-permission--prompt-async-attributed)
               (lambda (_t _p _a _origin cont &optional _count _entry)
                 (funcall cont 'always-allow)))
              ((symbol-function 'niwa-permission--apply-prompt-result)
               (lambda (&rest _) (error "Disk write failed"))))
      (let ((niwa--session session))
        (niwa-tool-permission-step
         ctx
         (lambda (_c) (setq next-called t))
         (lambda (r &rest _) (setq fail-reason r)))))
    (should-not next-called)
    (should (stringp fail-reason))
    (should (string-match-p "disk write failed" fail-reason)))
  :doc "fail-closed PermissionRequest stop beats allow"
  (let* ((tool (niwa-tool--create
                :name "Edit"
                :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules '(("Edit" :action ask)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         fail-reason fail-context
         next-called)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (_event _payload callback &rest _)
                 (funcall callback
                          '(:continue nil
                                      :stop-reason "hook failed"
                                      :permission-decision allow)))))
      (niwa-tool-permission-step
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
            (list (niwa-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil 'generic)
            (list (niwa-tool-ensure "Bash")
                  '(:command "unknown-hook-probe") 'bash)
            (list (niwa-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live") 'eval)))
    (pcase-let* ((`(,tool ,args ,kind) case)
                 (session (niwa-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (request (niwa-request--create
                           :id "request-permission-hook"
                           :session session
                           :origin "/root/worker"))
                 (niwa--session session)
                 (niwa-permission-rules nil)
                 (niwa-protected-paths nil)
                 (niwa-permission-reviewer 'user)
                 (hook-count 0))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (when (eq event 'PermissionRequest)
                     (cl-incf hook-count))
                   (funcall callback nil)))
                ((symbol-function
                  'niwa-permission-queue--render-entry)
                 #'ignore))
        (niwa-tool-permission-step
         (list :tool tool :args args :session session
               :request request)
         #'ignore #'ignore)
        (let ((entry (car (niwa-session-permission-queue
                           session))))
          (should entry)
          (should (eq kind (plist-get entry :kind)))
          (should
           (equal "request-permission-hook"
                  (plist-get entry :request-id)))
          (should (= 1 hook-count))
          (niwa-permission-queue--render-head session)
          (niwa-permission-queue--reevaluate entry)
          (should (= 1 hook-count))))))
  :doc "PreToolUse allow runs PermissionRequest and skips all cards"
  (dolist (case
           (list
            (list (niwa-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (niwa-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (niwa-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (niwa-session--create
                           :name "pre-tool-allow"
                           :permission-mode 'ask))
                 (niwa-permission-rules nil)
                 (niwa-protected-paths nil)
                 (niwa-permission-reviewer 'user)
                 (events nil)
                 (next-called nil))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event _payload callback &rest _)
                   (push event events)
                   (funcall callback nil))))
        (niwa-pq-test--with-session-view session view
          (niwa-tool-permission-step
           (list :tool tool :args args :session session
                 :hook-permission-decision 'allow)
           (lambda (_context) (setq next-called t)) #'ignore)
          (should next-called)
          (should-not (niwa-pq-test--requested-p session))))
      (should (equal '(PermissionRequest) events))))
  :doc "full-auto ignores PreToolUse asks but retains hook denies"
  (dolist (hook '(ask deny))
    (let ((session (niwa-session--create :permission-mode 'full-auto))
          (niwa-permission-rules nil)
          allowed denied)
      (niwa-pq-test--with-session-view session view
        (niwa-tool-permission-step
         (list :tool (niwa-tool--create :name "HookProbe")
               :session session :hook-permission-decision hook)
         (lambda (_context) (setq allowed t))
         (lambda (&rest _) (setq denied t)))
        (should-not (niwa-pq-test--requested-p session)))
      (should (eq (eq hook 'ask) allowed))
      (should (eq (eq hook 'deny) denied))))
  :doc "PermissionRequest allow settles generic Bash and Eval asks without cards"
  (dolist (case
           (list
            (list (niwa-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (niwa-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (niwa-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (niwa-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (niwa-permission-rules nil)
                 (niwa-protected-paths nil)
                 (niwa-permission-reviewer 'user)
                 (next-called nil))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (_event _payload callback &rest _)
                   (funcall callback
                            '(:permission-decision allow)))))
        (niwa-pq-test--with-session-view session view
          (niwa-tool-permission-step
           (list :tool tool :args args :session session)
           (lambda (_context) (setq next-called t)) #'ignore)
          (should next-called)
          (should-not (niwa-pq-test--requested-p session))))))
  :doc "PermissionRequest deny settles generic Bash and Eval asks with provenance"
  (dolist (case
           (list
            (list (niwa-tool--create
                   :name "Prompted" :read-only-p nil)
                  nil)
            (list (niwa-tool-ensure "Bash")
                  '(:command "unknown-hook-probe"))
            (list (niwa-tool-ensure "Eval")
                  '(:expression "(+ 1 2)" :mode "live"))))
    (pcase-let* ((`(,tool ,args) case)
                 (session (niwa-session--create
                           :name "permission-hook"
                           :permission-mode 'ask))
                 (niwa-permission-rules nil)
                 (niwa-protected-paths nil)
                 (niwa-permission-reviewer 'user)
                 (events nil)
                 (provenance nil)
                 (failure nil))
      (cl-letf (((symbol-function 'niwa-hooks-run-event)
                 (lambda (event payload callback &rest _)
                   (push event events)
                   (when (eq event 'PermissionDenied)
                     (setq provenance
                           (plist-get payload
                                      :permission-provenance)))
                   (funcall callback
                            (and (eq event 'PermissionRequest)
                                 '(:permission-decision deny
                                                        :permission-reason "hook denied"))))))
        (niwa-pq-test--with-session-view session view
          (niwa-tool-permission-step
           (list :tool tool :args args :session session)
           #'ignore (lambda (reason &rest _) (setq failure reason)))
          (should-not (niwa-pq-test--requested-p session))))
      (should (equal '(PermissionRequest PermissionDenied)
                     (nreverse events)))
      (should (eq 'PermissionRequest provenance))
      (should (string-match-p "hook denied" failure))))
  :doc "policy denials retain their rule provenance"
  (let* ((tool (niwa-tool--create
                :name "Edit" :read-only-p nil))
         (niwa-permission-rules '(("Edit" :action deny)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         provenance)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (event payload callback &rest _)
                 (when (eq event 'PermissionDenied)
                   (setq provenance
                         (plist-get payload :permission-provenance)))
                 (funcall callback nil))))
      (niwa-tool-permission-step
       (list :tool tool :args nil) #'ignore #'ignore))
    (should (eq 'deny-rule provenance)))
  :doc "queued user denials retain user provenance"
  (let* ((tool (niwa-tool--create
                :name "Edit" :read-only-p nil))
         (niwa-permission-rules '(("Edit" :action ask)))
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         (session (niwa-session--create :permission-mode 'ask))
         provenance)
    (cl-letf (((symbol-function 'niwa-hooks-run-event)
               (lambda (event payload callback &rest _)
                 (when (eq event 'PermissionDenied)
                   (setq provenance
                         (plist-get payload :permission-provenance)))
                 (funcall callback nil))))
      (niwa-pq-test--with-session-view session view
        (niwa-tool-permission-step
         (list :tool tool :args nil :session session) #'ignore #'ignore)
        (niwa-pq-test--press (niwa-pq-test--card view) "d")))
    (should (eq 'user provenance)))

  :doc "sub-agent sees parent's session rules through context :session"
  ;; The agent buffer carries `niwa--session' set buffer-locally
  ;; to the parent session struct (by reference).  When the pipeline
  ;; captures `niwa--session' at tool entry and threads it
  ;; through `:session' on the context, the permission step honors
  ;; the parent's session rules.
  (let* ((tool (niwa-tool--create
                :name "Edit" :read-only-p nil :groups '(edit)))
         (parent-session
          (niwa-session--create
           :name "parent"
           :permission-rules '(("Edit" :action allow))))
         ;; Agent buffer would carry the same struct by reference.
         (sub-agent-session-alias parent-session)
         (ctx (list :tool tool :args nil
                    :session sub-agent-session-alias))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask)
         called)
    (niwa-tool-permission-step
     ctx (lambda (_c) (setq called t)) #'ignore)
    (should called))

  :doc "rule added through sub-agent alias is visible on parent's struct"
  (let* ((parent-session (niwa-session--create :name "parent"))
         (sub-agent-alias parent-session))
    (niwa-permission--add-session-rule
     sub-agent-alias '("Edit" :path "/foo/*" :action allow))
    (should (equal (niwa-session-permission-rules parent-session)
                   '(("Edit" :path "/foo/*" :action allow)))))

  :doc "permission-mode toggle on parent observed by sub-agent next call"
  (let* ((tool (niwa-tool--create
                :name "Edit" :read-only-p nil :groups '(edit)))
         (parent-session
          (niwa-session--create
           :name "parent" :permission-mode 'ask))
         (sub-agent-alias parent-session)
         (ctx (list :tool tool :args nil :session sub-agent-alias))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'ask))
    (setf (niwa-session-permission-mode parent-session) 'full-auto)
    (let (next-called)
      (niwa-tool-permission-step
       ctx (lambda (_context) (setq next-called t)) #'ignore)
      (should next-called)))

  :doc "tripwire warning fires when non-read-only tool runs without session"
  (let* ((tool (niwa-tool--create :name "Edit" :read-only-p nil))
         (ctx (list :tool tool :args nil))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'full-auto)
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (kind msg &rest _)
                 (when (and (eq kind 'niwa)
                            (string-match-p "no session in context" msg))
                   (setq warned t)))))
      (niwa-tool-permission-step
       ctx (lambda (_c) nil) #'ignore))
    (should warned))

  :doc "no tripwire when session is present"
  (let* ((tool (niwa-tool--create :name "Edit" :read-only-p nil))
         (session (niwa-session--create :name "p"))
         (ctx (list :tool tool :args nil :session session))
         (niwa-permission-rules nil)
         (niwa-protected-paths nil)
         (niwa-permission-mode 'full-auto)
         (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (kind msg &rest _)
                 (when (and (eq kind 'niwa)
                            (string-match-p "no session in context" msg))
                   (setq warned t)))))
      (niwa-tool-permission-step
       ctx (lambda (_c) nil) #'ignore))
    (should-not warned)))

(provide 'test-niwa-tool-permission)
;;; test-niwa-tool-permission.el ends here
