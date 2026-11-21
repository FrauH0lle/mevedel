;;; test-niwa-tool-exec-permission.el -- Tests for Bash and Eval permission adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Bash and Eval permission adapters.

;;; Code:

(require 'niwa-tool-exec-permission)
(require 'niwa-tool-permission)
(require 'cl-lib)
(require 'seq)
(require 'niwa-bash-analysis)
(require 'niwa-bash-policy)
(require 'niwa-structs)
(require 'niwa-execution-target)
(require 'niwa-hooks)
(require 'niwa-permission-mode)
(require 'niwa-permission-persistence)
(require 'niwa-permission-prompt)
(require 'niwa-permission-log)
(require 'niwa-permission-queue)
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-permission-rules)
(require 'niwa-permissions)
(require 'niwa-sandbox)
(require 'niwa-telemetry)
(require 'niwa-turn)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun niwa-tool-exec-permission-test--read-permission-log (session)
  "Read permission log entries for SESSION."
  (let ((file (niwa-permission-log-path session))
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

(niwa-deftest niwa-tool-exec-permission--capture-permission-origin ()
  ,test
  (test)
  :doc "captures local or data-buffer session without changing the input"
  (let ((data (generate-new-buffer " *permission-capture-data*"))
        (dead (generate-new-buffer " *permission-capture-dead*"))
        (local-session (niwa-session--create :name "local"))
        (data-session (niwa-session--create :name "data")))
    (unwind-protect
        (progn
          (kill-buffer dead)
          (with-current-buffer data
            (setq-local niwa--session data-session))
          (dolist (link (list data dead nil))
            (dolist (local (list local-session nil))
              (with-temp-buffer
                (setq-local niwa--session local
                            niwa--data-buffer link)
                (let* ((input (list :command "pwd"))
                       (captured
                        (niwa-tool-exec-permission--capture-permission-origin
                         input)))
                  (should (equal '(:command "pwd") input))
                  (should (eq (or local (and (eq link data) data-session))
                              (plist-get (plist-get captured :permission-context)
                                         :session))))))))
      (when (buffer-live-p data) (kill-buffer data))
      (when (buffer-live-p dead) (kill-buffer dead))))
  :doc "explicit session and explicit absence survive ambient session fallback"
  (with-temp-buffer
    (setq-local niwa--session (niwa-session--create :name "ambient"))
    (dolist (session (list nil (niwa-session--create :name "explicit")))
      (let* ((context (list :session session :origin "/root/worker"
                            :execution-directory "/captured/"))
             (input (list :command "pwd" :permission-context context))
             (captured
              (niwa-tool-exec-permission--capture-permission-origin input)))
        (should (equal context (plist-get captured :permission-context)))
        (should (eq context (plist-get input :permission-context)))
        (should-not (eq context (plist-get captured :permission-context)))))))

(niwa-deftest niwa-tool-exec-permission--decide ()
  ,test
  (test)
  :doc "an operation deny is final and carries the operation's own cause"
  (let ((niwa-permission-rules nil))
    (should
     (equal '(deny rule)
            (let ((decision
                   (niwa-tool-exec-permission--decide
                    "Bash" "rm -rf build"
                    '(:sandbox-permissions additive
                      :additional-permissions (:network t))
                    (niwa-permission--invocation-context
                     :tool-name "Bash" :pattern "rm -rf build" :mode 'full-auto
                     :session-rules '(("Bash" :pattern "rm *" :action deny))))))
              (list (plist-get decision :outcome) (plist-get decision :via))))))
  :doc "an unsettled operation ask names the operation and its cause"
  (let ((niwa-permission-rules nil)
        (niwa-permission-reviewer 'user))
    (let ((decision (niwa-tool-exec-permission--decide
                     "Bash" "make test" '(:sandbox-permissions additive)
                     (niwa-permission--invocation-context
                      :tool-name "Bash" :pattern "make test" :mode 'ask))))
      (should (eq 'ask (plist-get decision :outcome)))
      (should (eq 'operation (plist-get decision :needs)))
      (should (eq 'mode (plist-get decision :via)))))
  :doc "a settled operation moves on to the remaining capabilities"
  (let ((niwa-permission-rules nil)
        (niwa-permission-reviewer 'user))
    (let ((decision (niwa-tool-exec-permission--decide
                     "Bash" "make test"
                     '(:sandbox-permissions additive
                       :additional-permissions (:network t)
                       :operation-pattern "make test")
                     (niwa-permission--invocation-context
                      :tool-name "Bash" :pattern "make test" :mode 'ask)
                     '(:operation t))))
      (should (eq 'ask (plist-get decision :outcome)))
      (should (eq 'capabilities (plist-get decision :needs)))
      (should (eq 'sandbox-network (plist-get decision :via)))
      (should (plist-get (plist-get decision :state) :missing))))
  :doc "the operation card's approval covers the requested capabilities"
  (let ((niwa-permission-rules nil)
        (niwa-permission-reviewer 'user))
    (should
     (eq 'allow
         (plist-get (niwa-tool-exec-permission--decide
                     "Bash" "make test"
                     '(:sandbox-permissions additive
                       :additional-permissions (:network t)
                       :operation-pattern "make test"
                       :approval-cell (allow-once))
                     (niwa-permission--invocation-context
                      :tool-name "Bash" :pattern "make test" :mode 'ask)
                     '(:operation t))
                    :outcome))))
  :doc "escalation honors qualified allows, explicit asks, and denies in order"
  (let ((niwa-permission-rules nil)
        (niwa-sandbox-mode 'required)
        (request '(:sandbox-permissions require-escalated)))
    (cl-flet ((decide (rules)
                (let ((decision
                       (niwa-tool-exec-permission--decide
                        "Bash" "make test" request
                        (niwa-permission--invocation-context
                         :tool-name "Bash" :pattern "make test" :mode 'edits
                         :session-rules rules))))
                  (list (plist-get decision :outcome) (plist-get decision :via)
                        (plist-get decision :needs)))))
      (should (equal '(ask sandbox-full-escalation escalation) (decide nil)))
      (should (equal '(allow sandbox-full-escalation nil)
                     (decide '(("Bash" :pattern "make test"
                                :sandbox-permissions require-escalated
                                :action allow)))))
      (should (equal '(ask rule escalation)
                     (decide '(("Bash" :pattern "make test" :action ask)
                               ("Bash" :pattern "make test"
                                :sandbox-permissions require-escalated
                                :action allow)))))
      (should (equal '(deny sandbox-full-escalation nil)
                     (decide '(("Bash" :pattern "make test"
                                :sandbox-permissions require-escalated
                                :action deny))))))))

(niwa-deftest niwa-tool-exec-permission-reevaluate ()
  ,test
  (test)
  :doc "queued calls recheck command profiles and independent grants separately"
  (let* ((grant '(:path "/tmp/profile-cache" :access write :recursive t))
         (operation '("Bash" :pattern "make:*" :action allow))
         (profile `("Bash" :pattern "make test" :file-system (,grant) :action allow))
         (entry `(:kind sandbox :tool-name "Bash" :detail "make test"
                  :sandbox-permissions additive
                  :requested-additional-permissions (:file-system (,grant))))
         (niwa-permission-reviewer 'user))
    (dolist (case (list (list (list operation profile) nil 'allow)
                       (list (list operation) nil 'ask)
                       (list (list operation) (list grant) 'allow)
                       (list (list operation profile
                                   '("Bash" :path "/tmp/profile-cache" :action deny))
                             nil 'deny)
                       (list (list operation profile
                                   '("Bash" :path "/tmp/profile-cache" :action ask))
                             nil 'ask)))
      (should
       (eq (nth 2 case)
           (niwa-tool-exec-permission-reevaluate
            entry `(:tool-name "Bash" :mode ask
                    :buckets ((:session ,@(car case)))
                    :resource-grants ,(nth 1 case)))))))
  :doc "ordinary operation authority cannot clear full escalation"
  (let ((niwa-permission-rules nil))
    (should
     (eq 'ask
         (niwa-tool-exec-permission-reevaluate
          '(:kind sandbox :tool-name "Bash" :detail "make test"
            :sandbox-permissions require-escalated)
          (niwa-permission--invocation-context
           :tool-name "Bash" :pattern "make test" :mode 'edits
           :session-rules '(("Bash" :pattern "make *" :action allow)))))))
  :doc "coalescing checks the selected tree instead of the original exact request"
  (let* ((exact '(:path "/tmp/profile-cache" :access write))
         (tree (append exact '(:recursive t)))
         (entry `(:kind sandbox :tool-name "Bash" :detail "make test"
                  :sandbox-permissions additive
                  :resource-selection-cell ((,tree))
                  :requested-additional-permissions (:file-system (,exact)))))
    (dolist (grant (list exact tree))
      (should
       (eq (if (eq grant tree) 'allow 'ask)
           (niwa-tool-exec-permission-reevaluate
            entry `(:tool-name "Bash" :mode edits
                    :resource-grants (,grant)))))))
)

(niwa-deftest niwa-tool-exec-permission--request-permission ()
  ,test
  (test)
  :doc "uses the pipeline boundary when present"
  (let (seen)
    (niwa-tool-exec-permission--request-permission
     (list :callback #'ignore)
     (list :permission-request
           (lambda (entry session callback)
             (setq seen (list entry session callback))))
     'session)
    (should (equal 'session (cadr seen)))
    (should (eq #'ignore (nth 2 seen))))
  :doc "falls back to direct queue admission outside the pipeline"
  (let ((niwa-permission-mode 'ask) seen)
    ;; Isolation stub: this case checks only what the fallback hands the queue;
    ;; the real queue is covered by test-niwa-permission-queue-decisions.el.
    (cl-letf (((symbol-function 'niwa-permission-request)
               (lambda (entry &optional session)
                 (setq seen (list entry session)))))
      (niwa-tool-exec-permission--request-permission
       '(:kind bash) nil 'session))
    (should (eq 'bash (plist-get (car seen) :kind)))
    (should (eq 'ask (plist-get (car seen) :permission-mode-effective)))
    (should (eq 'session (cadr seen))))
  :doc "combines pending operation and additive authority"
  (let ((cell (list nil))
        queued
        settled)
    ;; Isolation stub: this case checks only what the fallback hands the queue;
    ;; the real queue is covered by test-niwa-permission-queue-decisions.el.
    (cl-letf (((symbol-function 'niwa-permission-request)
               (lambda (entry &optional _session)
                 (setq queued entry))))
      (niwa-tool-exec-permission--request-permission
       (list :kind 'bash :callback
             (lambda (outcome) (setq settled outcome)))
       `(:sandbox-request
         (:justification "Run the integration test?"
                         :approval-cell ,cell
                         :authority-state
                         (:requested (:network t)
                                     :missing (:network t)
                                     :granted nil)))))
    (should (plist-get queued :operation-pending-p))
    (should (equal '(:network t)
                   (plist-get queued :missing-additional-permissions)))
    (funcall (plist-get queued :callback) 'allow-once)
    (should (car cell))
    (should (eq 'allow-once settled)))
  :doc "constrains one-shot mutation prompts and outcomes"
  (let ((remember-cell (list '(:operation t)))
        queued
        settled)
    ;; Isolation stub: this case checks only what the fallback hands the queue;
    ;; the real queue is covered by test-niwa-permission-queue-decisions.el.
    (cl-letf (((symbol-function 'niwa-permission-request)
               (lambda (entry &optional _session)
                 (setq queued entry))))
      (niwa-tool-exec-permission--request-permission
       (list :kind 'bash
             :callback (lambda (outcome) (setq settled outcome))
             :include-always t
             :remember-authority-cell remember-cell
             :reusable-operation-p t)
       '(:one-shot-mutations-p t)))
    (should (plist-get queued :once-only))
    (should-not (plist-get queued :include-always))
    (should-not (plist-get queued :remember-authority-cell))
    (should-not (plist-get queued :reusable-operation-p))
    (funcall (plist-get queued :callback) 'allow-session)
    (should (eq 'allow-once settled))
    (should (equal '(:operation t) (car remember-cell)))))

(niwa-deftest niwa-tool-exec-permission--sandbox-request ()
  ,test
  (test)
  :doc "default request:
`niwa-tool-exec-permission--sandbox-request' normalizes omitted authority"
  (should (equal '(:level use-default :additional-permissions nil)
                 (niwa-tool-exec-permission--sandbox-request nil 'bash)))
  :doc "fresh and live mode normalization cannot contaminate later default requests"
  (dolist (mode '(fresh live))
    (niwa-tool-exec-permission-effective-sandbox-request
     (list :mode (symbol-name mode)) "Eval" "(+ 1 2)" mode)
    (should (equal '(:level use-default :additional-permissions nil)
                   (niwa-tool-exec-permission--sandbox-request nil 'bash))))
  :doc "provider-shaped default request:
empty optional capabilities and an unused justification do not request authority"
  (should
   (equal
    '(:level use-default :additional-permissions nil)
    (niwa-tool-exec-permission--sandbox-request
     '(:sandbox_permissions "use_default"
                            :additional_permissions
                            (:network :json-false :file_system (:read [] :write []))
                            :justification "Run the requested read-only workspace inspection.")
     'bash)))
  :doc "network request:
`niwa-tool-exec-permission--sandbox-request' accepts a justified non-empty profile"
  (should
   (equal '(:level additive :additional-permissions (:network t)
                   :justification "Download dependencies?")
          (niwa-tool-exec-permission--sandbox-request
           '(:sandbox_permissions "with_additional_permissions"
                                  :additional_permissions (:network t)
                                  :justification "  Download dependencies?  ")
           'bash)))
  :doc "filesystem request:
additive authority normalizes exact read and write paths"
  (let ((read-path (expand-file-name "readable" temporary-file-directory))
        (write-path (expand-file-name "writable" temporary-file-directory)))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:file-system ((:path ,read-path :access read)
                              (:path ,write-path :access write)))
               :justification "Inspect and update the protected files?")
      (niwa-tool-exec-permission--sandbox-request
       `(:sandbox_permissions "with_additional_permissions"
                              :additional_permissions
                              (:file_system (:read [,read-path] :write (,write-path)))
                              :justification "Inspect and update the protected files?")
       'bash))))
  :doc "remote filesystem request:
target-native absolute and home paths qualify through the session target"
  (let* ((root (make-temp-file "niwa-bash-remote-grant-" t))
         (native-path (file-name-concat root "resource"))
         (remote-root (format "/niwamock:grant:%s/" root)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("grant")
                                              (let* ((target (niwa-execution-target-create remote-root))
                                                     (session
                                                      (niwa-session--create
                                                       :execution-target target
                                                       :working-directory remote-root))
                                                     (_ (setf (niwa-execution-target-environment target)
                                                              '(("HOME" . "/home/dev"))))
                                                     (request
                                                      (niwa-tool-exec-permission--sandbox-request
                                                       `(:sandbox_permissions "with_additional_permissions"
                                                                              :additional_permissions
                                                                              (:file_system (:read [,native-path "~/.config/token"]))
                                                                              :justification "Read the target file?")
                                                       'bash nil `(:session ,session))))
                                                (should
                                                 (equal (list (concat (file-remote-p remote-root) native-path)
                                                              (concat (file-remote-p remote-root)
                                                                      "/home/dev/.config/token"))
                                                        (mapcar
                                                         (lambda (grant) (plist-get grant :path))
                                                         (plist-get
                                                          (plist-get request :additional-permissions)
                                                          :file-system))))))
      (delete-directory root t)))
  :doc "write subsumes read:
duplicate exact paths normalize to the stronger access level"
  (let ((path (expand-file-name "resource" temporary-file-directory)))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:file-system ((:path ,path :access write)))
               :justification "Update the protected file?")
      (niwa-tool-exec-permission--sandbox-request
       `(:sandbox_permissions "with_additional_permissions"
                              :additional_permissions
                              (:file_system (:read [,path] :write [,path]))
                              :justification "Update the protected file?")
       'bash))))
  :doc "relative filesystem path:
filesystem authority requires exact absolute paths"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read ["relative/path"]))
                           :justification "Read the protected file?")
    'bash))
  :doc "empty filesystem profile:
an additive profile must contain a real capability"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read [] :write []))
                           :justification "Allow this?")
    'bash))
  :doc "empty profile:
additive authority cannot be requested without a capability"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions nil
                           :justification "Allow this?")
    'bash))
  :doc "false network:
JSON false does not make an additive profile non-empty"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network :json-false)
                           :justification "Allow this?")
    'bash))
  :doc "missing justification:
non-default authority requires a user-facing question"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network t))
    'bash))
  :doc "default extras:
default execution rejects stray escalation arguments"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:additional_permissions (:network t)) 'bash))
  :doc "full Bash escalation:
full authority requires a justification and no additive profile"
  (should
   (equal '(:level escalated
                   :sandbox-permissions require-escalated
                   :additional-permissions nil
                   :justification "Run without confinement?")
          (niwa-tool-exec-permission--sandbox-request
           '(:sandbox_permissions "require_escalated"
                                  :justification "  Run without confinement?  ")
           'bash)))
  :doc "provider-shaped full escalation:
an empty optional capability object does not conflict with full authority"
  (should
   (eq
    'escalated
    (plist-get
     (niwa-tool-exec-permission--sandbox-request
      '(:sandbox_permissions "require_escalated"
                             :additional_permissions
                             (:network :json-false :file_system (:read [] :write []))
                             :justification "Run without confinement?")
      'bash)
     :level)))
  :doc "full fresh Eval escalation:
fresh Eval may request full authority"
  (should
   (eq 'escalated
       (plist-get
        (niwa-tool-exec-permission--sandbox-request
         '(:sandbox_permissions "require_escalated"
                                :justification "Run fresh Eval without confinement?")
         'eval 'fresh)
        :level)))
  :doc "full escalation justification:
full authority rejects a missing justification"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated") 'bash))
  :doc "full escalation profile:
full authority cannot be combined with additive permissions"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated"
                           :additional_permissions (:network t)
                           :justification "Run without confinement?")
    'bash))
  :doc "full live Eval escalation:
live Eval cannot request child-confinement authority"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated"
                           :justification "Run without confinement?")
    'eval 'live))
  :doc "live Eval:
additive child permissions are available only to fresh Eval"
  (should-error
   (niwa-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network t)
                           :justification "Fetch package metadata?")
    'eval 'live)))

(niwa-deftest niwa-tool-exec-permission--additional-profile ()
  ,test
  (test)
  :doc "omits absent capabilities"
  (should-not (niwa-tool-exec-permission--additional-profile nil nil))
  :doc "combines network and exact filesystem capabilities"
  (should
   (equal
    '(:network t
               :file-system ((:path "/tmp/input" :access read)))
    (niwa-tool-exec-permission--additional-profile
     t '((:path "/tmp/input" :access read))))))

(niwa-deftest niwa-tool-exec-permission-current-context ()
  ,test
  (test)
  :doc "derives the current Bash permission facts from the active session"
  (let ((root (make-temp-file "niwa-permission-context-" t)))
    (unwind-protect
        (let* ((workspace (niwa-workspace--create :type 'file :root root))
               (session
                (niwa-session--create
                 :name "context" :workspace workspace)))
          (with-temp-buffer
            (let ((context
                   (niwa-tool-exec-permission-current-context
                    "Bash" '(:command "printf ok") session)))
              (should (eq session (plist-get context :session)))
              (should (eq workspace (plist-get context :workspace)))
              (should (eq (current-buffer) (plist-get context :buffer)))
              (should (equal "printf ok" (plist-get context :pattern))))))
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission--merge-additional-profiles ()
  ,test
  (test)
  :doc "unions capabilities and keeps the strongest exact-path access"
  (let ((path (expand-file-name "profile" temporary-file-directory)))
    (should
     (equal
      `(:network t :file-system ((:path ,path :access write)))
      (niwa-tool-exec-permission--merge-additional-profiles
       `(:file-system ((:path ,path :access read)))
       `(:network t
                  :file-system ((:path ,path :access write)
                                (:path "relative" :access read)))))))
  :doc "keeps exact and recursive grants of the same path distinct"
  (let ((path (expand-file-name "tree" temporary-file-directory)))
    (should
     (equal
      `(:file-system ((:path ,path :access read)
                      (:path ,path :access write :recursive t)))
      (niwa-tool-exec-permission--merge-additional-profiles
       `(:file-system ((:path ,path :access read)
                       (:path ,path :access read :recursive t)))
       `(:file-system ((:path ,path :access write :recursive t)))))))
  :doc "drops grants with a non-boolean recursive flag"
  (let ((path (expand-file-name "tree" temporary-file-directory)))
    (should-not
     (niwa-tool-exec-permission--merge-additional-profiles
      `(:file-system ((:path ,path :access read :recursive yes)))))))

(niwa-deftest niwa-tool-exec-permission--direct-resource-grants ()
  ,test
  (test)
  :doc "returns exact grants owned directly by the session"
  (let* ((grant '(:path "/tmp/niwa-direct" :access read))
         (session
          (niwa-session--create
           :name "direct" :resource-grants (list grant))))
    (should
     (equal (list grant)
            (niwa-tool-exec-permission--direct-resource-grants
             `(:session ,session)))))
  :doc "treats normalized resource grants as authoritative even when empty"
  (cl-letf (((symbol-function
              'niwa-permission-persistence-load-resource-grants)
             (lambda (_workspace)
               (ert-fail "reloaded persistent resource grants"))))
    (should-not
     (niwa-tool-exec-permission--direct-resource-grants
      '(:workspace workspace :resource-grants nil)))))

(niwa-deftest niwa-tool-exec-permission--remembered-additional-profile ()
  ,test
  (test)
  :doc "resolves matching direct profiles without separate resource grants"
  (let* ((path "/tmp/niwa-remembered")
         (profile `((:path ,path :access write)))
         (rules
          `(("Bash" :pattern "make:*" :network t
             :file-system ,profile :action allow)))
         (session
          (niwa-session--create
           :name "remembered" :resource-grants nil)))
    (should
     (equal
      `(:network t :file-system ,profile)
      (niwa-tool-exec-permission--remembered-additional-profile
       "Bash" "make report"
       `(:session ,session :buckets ((:session ,@rules)))))))
  :doc "recursive profiles authorize their own extent without a companion grant"
  (let* ((grant '(:path "/tmp/niwa-tree" :access read :recursive t))
         (rule `("Bash" :pattern "make:*" :file-system (,grant) :action allow)))
    (should
     (equal `(:file-system (,grant))
            (niwa-tool-exec-permission--remembered-additional-profile
             "Bash" "make report" `(:buckets ((:session ,rule)) :resource-grants nil)))))
  :doc "uses either target store as effective network and cache authority"
  (dolist (scope '(global workspace))
    (let* ((tmp-dir (make-temp-file "niwa-profile-store-" t))
           (user-home (file-name-concat tmp-dir "home"))
           (niwa-user-dir (file-name-concat tmp-dir "global"))
           (workspace
            (niwa-workspace--create
             :type 'project :id "profile" :root tmp-dir :name "profile"))
           (session
            (niwa-session--create
             :name "profile" :workspace workspace))
           (file (niwa-permission-persistence-file workspace scope)))
      (unwind-protect
          (let ((process-environment
		 (cons (concat "HOME=" user-home) process-environment)))
            (make-directory (file-name-directory file) t)
            (with-temp-file file
              (pp '(:rules
                    (("Bash" :pattern "npx @emacs-eask/cli *"
                      :network t
                      :file-system ((:path "~/.npm" :access write))
                      :action allow))
                    :resource-grants nil)
                  (current-buffer)))
            (let* ((context
                    `(:session ,session :workspace ,workspace
                               :buckets
                               ((:persistent
				 ,@(niwa-permission-persistence-load-rules
                                    workspace)))))
                   (path (expand-file-name "~/.npm")))
              (should
               (equal
		`(:network t :file-system ((:path ,path :access write)))
		(niwa-tool-exec-permission--remembered-additional-profile
		 "Bash" "npx @emacs-eask/cli test" context)))
              ;; An external edit must revoke the mount for the next call.
              (with-temp-file file
                (pp '(:rules (("Bash" :pattern "npx @emacs-eask/cli *" :action allow))
                      :resource-grants nil)
                    (current-buffer)))
              (niwa-permission-persistence-refresh workspace #'ignore)
              (should
               (equal '(:level use-default :additional-permissions nil)
                      (niwa-tool-exec-permission-effective-sandbox-request
                       '(:command "npx @emacs-eask/cli test")
                       "Bash" "npx @emacs-eask/cli test" nil
                       `(:session ,session :workspace ,workspace))))))
	(delete-directory tmp-dir t)))))

(niwa-deftest niwa-tool-exec-permission--apply-remembered-authority ()
  ,test
  (test)
  :doc "saves command paths and independent paths without duplicating authority"
  (let* ((command-grant '(:path "/tmp/command-cache" :access write :recursive t))
         (independent '(:path "/tmp/shared-input" :access read))
         (session (niwa-session--create :name "scopes"))
         (request
          `(:operation-pattern "make report" :remember-patterns ("make:*")
            :remember-cell ((:operation t :file-system (,command-grant)
                            :resource-grants (,independent))))))
    (niwa-tool-exec-permission--apply-remembered-authority
     'allow-session "Bash" request session nil)
    (should (equal (list independent) (niwa-session-resource-grants session)))
    (should (equal (list command-grant)
                   (plist-get (cdar (niwa-session-permission-rules session))
                              :file-system))))
  :doc "independent paths can be remembered without authorizing any command"
  (let* ((grant '(:path "/tmp/shared-input" :access read))
         (session (niwa-session--create :name "independent"))
         (request `(:operation-pattern "make report" :remember-patterns ("make:*")
                    :remember-cell ((:resource-grants (,grant))))))
    (niwa-tool-exec-permission--apply-remembered-authority
     'allow-session "Bash" request session nil)
    (should (equal (list grant) (niwa-session-resource-grants session)))
    (should-not (niwa-session-permission-rules session)))
  :doc "binds a compound command profile to the complete workload"
  (let* ((command "pwd && package fetch dependencies")
         (session (niwa-session--create :name "compound"))
         (request
          `(:operation-pattern ,command
                               :remember-patterns ("pwd" "package fetch:*")
                               :remember-cell ((:operation t :network t)))))
    (niwa-tool-exec-permission--apply-remembered-authority
     'allow-session "Bash" request session nil)
    (let ((rules (niwa-session-permission-rules session)))
      (should (member '("Bash" :pattern "pwd" :action allow) rules))
      (should
       (member '("Bash" :pattern "package fetch:*" :action allow) rules))
      (should
       (member `("Bash" :pattern ,command :network t :action allow)
               rules))
      (should-not
       (seq-some
        (lambda (rule)
          (and (equal "pwd" (plist-get (cdr rule) :pattern))
               (plist-get (cdr rule) :network)))
        rules)))))

(niwa-deftest niwa-tool-exec-permission-effective-sandbox-request ()
  ,test
  (test)
  :doc "reuses a matching direct network profile for a default Bash call"
  (let* ((rules
          '(("Bash" :pattern "npx @emacs-eask/cli:*"
             :network t :action allow)))
         (context
          `(:buckets ((:session ,@rules))
                     :session-rules ,rules)))
    (should
     (equal
      '(:level additive :additional-permissions (:network t))
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:command "npx @emacs-eask/cli test ert")
       "Bash" "npx @emacs-eask/cli test ert" nil context))))
  :doc "does not copy a profile to a non-matching Bash command"
  (let* ((rules
          '(("Bash" :pattern "npx @emacs-eask/cli:*"
             :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      '(:level use-default :additional-permissions nil)
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:command "git status") "Bash" "git status" nil context))))
  :doc "unions remembered authority with an explicit capability delta"
  (let* ((path "/tmp/niwa-output")
         (rules
          '(("Bash" :pattern "npx test" :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:network t :file-system ((:path ,path :access write)))
               :justification "Write the report?")
      (niwa-tool-exec-permission-effective-sandbox-request
       `(:command "npx test"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:file_system (:write [,path]))
                  :justification "Write the report?")
       "Bash" "npx test" nil context))))
  :doc "reattaches profile paths independently of unrelated resource grants"
  (let* ((path "/tmp/niwa-input")
         (profile `((:path ,path :access write)))
         (rule `("Bash" :pattern "make:*" :file-system ,profile :action allow)))
    (dolist (grants (list nil `((:path ,path :access read))))
      (should
       (equal `(:level additive :additional-permissions (:file-system ,profile))
              (niwa-tool-exec-permission-effective-sandbox-request
               '(:command "make build") "Bash" "make build" nil
               `(:resource-grants ,grants :buckets ((:session ,rule))))))))
  :doc "unions matching direct profiles and keeps the strongest path access"
  (let* ((path "/tmp/niwa-output")
         (rules
          `(("Bash" :pattern "make:*" :network t :action allow)
            ("Bash" :pattern "make:*"
             :file-system ((:path ,path :access read)) :action allow)
            ("Bash" :pattern "make:*"
             :file-system ((:path ,path :access write)) :action allow)))
         (session
          (niwa-session--create
           :name "write"
           :resource-grants `((:path ,path :access write)))))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:network t :file-system ((:path ,path :access write))))
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:session ,session :buckets ((:session ,@rules)))))))
  :doc "a compound command receives profiles only when rules allow every segment"
  (let* ((npm "/tmp/niwa-profile-npm")
         (git "/tmp/niwa-profile-git")
         (rules
          `(("Bash" :pattern "npx @emacs-eask/cli *" :network t
             :file-system ((:path ,npm :access write)) :action allow)
            ("Bash" :pattern "git add:*"
             :file-system ((:path ,git :access write)) :action allow)))
         (context `(:buckets ((:persistent ,@rules)))))
    (pcase-dolist (`(,command ,network ,paths)
                   `(("npx @emacs-eask/cli test" t (,npm))
                     ("cd sub && npx @emacs-eask/cli clean elc && npx @emacs-eask/cli test"
                      t (,npm))
                     ("npx @emacs-eask/cli test && git add foo" t (,git ,npm))
                     ("npx @emacs-eask/cli test && rm -rf build" nil nil)
                     ("cat a > b && npx @emacs-eask/cli test" nil nil)
                     ("npx @emacs-eask/cli test | tee log" nil nil)
                     ("rm -rf build; npx @emacs-eask/cli test" nil nil)
                     ("npx @emacs-eask/cli test $(rm -rf build)" nil nil)
                     ("npx @emacs-eask/cli test & rm -rf build" nil nil)))
      (let ((profile
             (plist-get (niwa-tool-exec-permission-effective-sandbox-request
                         `(:command ,command) "Bash" command nil context)
                        :additional-permissions)))
        (should (eq network (plist-get profile :network)))
        (should (equal paths
                       (sort (mapcar (lambda (grant) (plist-get grant :path))
                                     (plist-get profile :file-system))
                             #'string<))))))
  :doc "a listed safe assignment does not hide a command from its profile"
  (let ((context '(:buckets ((:persistent
                              ("Bash" :pattern "npx @emacs-eask/cli *"
                               :network t :action allow)))
                   :safe-env-vars ("CHECK_BASE"))))
    (pcase-dolist (`(,command ,network)
                   '(("CHECK_BASE=abc npx @emacs-eask/cli run script check" t)
                     ("cd sub && CHECK_BASE=abc npx @emacs-eask/cli test" t)
                     ("OTHER=abc npx @emacs-eask/cli test" nil)
                     ("CHECK_BASE=$(rm -rf x) npx @emacs-eask/cli test" nil)))
      (should
       (eq network
           (plist-get
            (plist-get (niwa-tool-exec-permission-effective-sandbox-request
                        `(:command ,command) "Bash" command nil context)
                       :additional-permissions)
            :network)))))
  :doc "delegated profiles and full escalation never become sticky additions"
  (let ((delegated
         '(("Bash" :pattern "make:*" :network t :action allow))))
    (should
     (equal
      '(:level use-default :additional-permissions nil)
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:buckets ((:invocation ,@delegated))))))
    (should
     (equal
      '(:level escalated
               :sandbox-permissions require-escalated
               :additional-permissions nil
               :justification "Run directly?" :eval-mode fresh)
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:expression "(+ 1 2)" :mode "fresh"
                     :sandbox_permissions "require_escalated"
                     :justification "Run directly?")
       "Eval" "(+ 1 2)" 'fresh
       '(:buckets ((:session)))))))
  :doc "reuses an exact direct Eval profile"
  (let* ((rules
          '(("Eval" :pattern "(url-retrieve-synchronously url)"
             :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      '(:level additive :additional-permissions (:network t) :eval-mode fresh)
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:expression "(url-retrieve-synchronously url)" :mode "fresh")
       "Eval" "(url-retrieve-synchronously url)" 'fresh context))))
  :doc "reuses an independent protected child tree but never infers one from its parent"
  (let* ((root (make-temp-file "niwa-reuse-child-" t))
         (git (file-name-concat root ".git"))
         (niwa-protected-paths '(("**/.git/**" . read-only)))
         (args `(:sandbox_permissions "with_additional_permissions"
                 :additional_permissions (:file_system (:write [,git]))
                 :justification "Write child")))
    (unwind-protect
        (progn
          (make-directory git)
          (dolist (grants (list `((:path ,root :access write :recursive t))
                               `((:path ,git :access write :recursive t))))
            (let ((grant (car (plist-get
                               (plist-get
                                (niwa-tool-exec-permission-effective-sandbox-request
                                 args "Bash" "true" nil
                                 `(:resource-grants ,grants :buckets ((:session))))
                                :additional-permissions)
                               :file-system))))
              (should (equal git (plist-get grant :path)))
              (should (eq (equal git (plist-get (car grants) :path))
                          (plist-get grant :recursive))))))
      (delete-directory root t)))
  :doc "does not attach a remembered child profile to live Eval"
  (let* ((rules
          '(("Eval" :pattern "(url-retrieve-synchronously url)"
             :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      '(:level use-default :additional-permissions nil :eval-mode live)
      (niwa-tool-exec-permission-effective-sandbox-request
       '(:expression "(url-retrieve-synchronously url)" :mode "live")
       "Eval" "(url-retrieve-synchronously url)" 'live context))))
  :doc "an optional live Eval justification creates no authority request"
  (dolist (justification '("Inspect the buffer list" "  " nil))
    (should
     (equal
      '(:level use-default :additional-permissions nil :eval-mode live)
      (niwa-tool-exec-permission-effective-sandbox-request
       (list :expression "(buffer-list)" :mode "live" :justification justification)
       "Eval" "(buffer-list)" 'live '(:buckets ((:session))))))))

(ert-deftest niwa-tool-exec-permission-socket-admission ()
  "Directory authority cannot approve a host socket at the permission seam."
  (let* ((root (make-temp-file "niwa-socket-admission-" t))
         (path (file-name-concat root "socket"))
         (server (make-network-process :name "socket-admission" :family 'local
                                       :server t :service path :noquery t))
         (parent `(:path ,root :access write :recursive t))
         (exact `(:path ,path :access write))
         (niwa-permission-rules nil))
    (unwind-protect
        (dolist (tool '("Bash" "Eval"))
          (dolist (source '(resource profile invocation))
            (dolist (approved '(nil t))
              (let* ((grants (if approved (list parent exact) (list parent)))
                     (rule `(,tool :pattern "true" :action allow
                             ,@(when (eq source 'profile)
                                 (list :file-system grants))))
                     (context (niwa-permission--invocation-context
                               :tool-name tool :pattern "true" :mode 'ask
                               :session-rules (list rule)
                               :resource-grants (and (eq source 'resource) grants)))
                     (niwa-tool-exec-permission-approved-resources
                      (and (eq source 'invocation) approved (list exact)))
                     (request (niwa-tool-exec-permission-effective-sandbox-request
                               `(:sandbox_permissions "with_additional_permissions"
                                 :additional_permissions (:file_system (:write [,path]))
                                 :justification "Connect")
                               tool "true" (and (equal tool "Eval") 'fresh) context))
                     (request (plist-put request :operation-pattern "true"))
                     (request (plist-put request :approval-cell
                                         (list (and (eq source 'invocation) approved))))
                     (decision (niwa-tool-exec-permission--decide
                                tool "true" request context))
                     (launch (plist-get (plist-get request :additional-permissions)
                                        :file-system)))
                (ert-info ((format "%s %s approved=%s request=%S"
                                   tool source approved request))
                  (should (eq (if approved 'allow 'ask) (plist-get decision :outcome)))
                  (cl-letf (((symbol-function 'niwa-sandbox--wayland-socket)
                             (lambda () nil)))
                    (should (equal (and approved (list path))
                                   (niwa-sandbox--socket-allow-list launch nil)))))))))
      (delete-process server)
      (delete-directory root t))))

(ert-deftest niwa-tool-exec-permission-coalesced-file-is-not-socket-approval ()
  "Coalescing under a stored tree cannot mint an exact socket selection."
  (let* ((root (make-temp-file "niwa-coalesced-socket-" t))
         (path (file-name-concat root "file"))
         (selection `((:path ,path :access write)))
         (session (niwa-session--create
                   :name "coalesced-socket" :permission-mode 'ask
                   :permission-rules '(("Bash" :pattern "true" :action allow))))
         (args `(:sandbox_permissions "with_additional_permissions"
                 :additional_permissions (:file_system (:write [,path]))
                 :justification "Write file"))
         (context (list :session session :buffer (current-buffer)))
         (niwa-permission-rules nil)
         settled outcome server)
    (unwind-protect
        (progn
          (write-region "file" nil path nil 'silent)
          (cl-letf (((symbol-function 'niwa-hooks-run-event)
                     (lambda (_event _payload callback &rest _) (funcall callback nil))))
            (niwa-pq-test--answering (nil :session session)
                                        (setq context (plist-put context :buffer (current-buffer)))
                                        (niwa-tool-permission--request
                                         context (list :kind 'sandbox :tool-name "Bash" :command "true"
                                                       :detail "true" :origin "/root"
                                                       :sandbox-permissions 'additive
                                                       :requested-additional-permissions (list :file-system selection)
                                                       :resource-selection-cell (list selection))
                                         session (lambda (updated result) (setq settled updated outcome result)))
                                        ;; The real card is on screen; coalescing settles it.
                                        (should (= 1 (length (niwa-permission-queue--get session))))
                                        (should (eq 'ask (niwa-permission-queue--reevaluate
                                                          (car (niwa-permission-queue--get session)))))
                                        (setf (niwa-session-resource-grants session)
                                              `((:path ,root :access write :recursive t)))
                                        (niwa-permission-queue--coalesce 'allow-session session)
                                        (should (eq 'allow-once outcome))
                                        (should-not (niwa-permission-queue--get session))))
          (should-not (plist-get settled :approved-resources))
          (delete-file path)
          (setq server (make-network-process :name "coalesced-socket" :family 'local
                                            :server t :service path :noquery t))
          (let* ((niwa-tool-exec-permission-approved-resources
                  (plist-get settled :approved-resources))
                 (permission-context (niwa-permission--invocation-context
                                      :tool-name "Bash" :pattern "true"
                                      :session (plist-get settled :session)))
                 (request (niwa-tool-exec-permission-effective-sandbox-request
                           args "Bash" "true" nil permission-context))
                 (grants (plist-get (plist-get request :additional-permissions)
                                    :file-system)))
            (cl-letf (((symbol-function 'niwa-sandbox--wayland-socket) (lambda () nil)))
              (should-not (niwa-sandbox--socket-allow-list grants nil)))))
      (when server (delete-process server))
      (niwa-permission-queue--set nil session)
      (delete-directory root t))))

(ert-deftest niwa-tool-exec-permission-socket-proof-not-forgeable ()
  "Public additions cannot carry internal launch authorization."
  (should-error
   (niwa-tool-exec-permission--normalize-additional-permissions
    '(:file_system (:write ["/tmp/socket"] :socket-authorized t))))
  (should-error
   (niwa-tool-exec-permission--normalize-additional-permissions
    '(:socket-authorized t :file_system (:write ["/tmp/socket"])))))

(ert-deftest niwa-tool-exec-permission-socket-launch-type-change ()
  "A file approved through a directory cannot become socket authority later."
  (let* ((root (make-temp-file "niwa-socket-race-" t))
         (path (file-name-concat root "file"))
         (parent `(:path ,root :access write :recursive t))
         (context `(:resource-grants (,parent) :buckets ((:session))))
         server)
    (unwind-protect
        (progn
          (write-region "file" nil path nil 'silent)
          (let* ((request (niwa-tool-exec-permission-effective-sandbox-request
                           `(:sandbox_permissions "with_additional_permissions"
                             :additional_permissions (:file_system (:write [,path]))
                             :justification "Write file") "Bash" "true" nil context))
                 (launch (plist-get (plist-get request :additional-permissions)
                                    :file-system)))
            (should-not (plist-get (car launch) :socket-authorized))
            (delete-file path)
            (setq server (make-network-process :name "socket-race" :family 'local
                                              :server t :service path :noquery t))
            (cl-letf (((symbol-function 'niwa-sandbox--wayland-socket)
                       (lambda () nil)))
              (should-not (niwa-sandbox--socket-allow-list launch nil)))))
      (when server (delete-process server))
      (delete-directory root t))))

(niwa-deftest niwa-tool-exec-permission--additional-authority-state ()
  ,test
  (test)
  :doc "a command profile authorizes its paths without independent grants"
  (dolist (tool '("Bash" "Eval"))
    (let* ((grant '(:path "/tmp/command-cache" :access write :recursive t))
           (request `(:operation-pattern "operation"
                      :additional-permissions (:file-system (,grant))))
           (rule `(,tool :pattern "operation" :file-system (,grant) :action allow)))
      (dolist (bucket '(:session :persistent :defcustom))
        (let ((state (niwa-tool-exec-permission--additional-authority-state
                      tool request `(:buckets ((,bucket ,rule)) :resource-grants nil))))
          (should-not (plist-get state :missing))
          (should (equal `(:file-system (,grant)) (plist-get state :granted)))))
      (dolist (bucket '(:invocation :request))
        (should
         (plist-get
          (niwa-tool-exec-permission--additional-authority-state
           tool request `(:buckets ((,bucket ,rule)) :resource-grants nil))
          :missing)))))
  :doc "existing read plus requested write leaves only write unresolved"
  (let* ((read '(:path "/tmp/external" :access read))
         (write '(:path "/tmp/external" :access write))
         (state
          (niwa-tool-exec-permission--additional-authority-state
           "Bash"
           `(:additional-permissions (:file-system (,write)))
           `(:mode ask
                   :buckets ((:session))
                   :resource-grants (,read)))))
    (should
     (equal `(:file-system (,write)) (plist-get state :missing)))
    (should-not (plist-get state :granted)))
  :doc "granted resources remain context while only missing upgrades prompt"
  (let* ((read '(:path "/tmp/input" :access read))
         (write '(:path "/tmp/output" :access write))
         (state
          (niwa-tool-exec-permission--additional-authority-state
           "Bash"
           `(:additional-permissions
             (:network t :file-system (,read ,write)))
           `(:mode ask
                   :buckets ((:session))
                   :resource-grants (,read)))))
    (should
     (equal `(:network t :file-system (,write))
            (plist-get state :missing)))
    (should
     (equal `(:file-system (,read))
            (plist-get state :granted))))
  :doc "full-auto treats an explicit network request as granted"
  (let ((state
         (niwa-tool-exec-permission--additional-authority-state
          "Bash"
          '(:additional-permissions (:network t))
          '(:mode full-auto :buckets ((:session))))))
    (should-not (plist-get state :missing))
    (should (equal '(:network t) (plist-get state :granted))))
  :doc "protected child needs explicit tree authority in every direct bucket, and revocation restores the ask"
  (let* ((root (make-temp-file "niwa-authority-child-" t))
         (git (file-name-concat root ".git"))
         (parent `(:path ,root :access write :recursive t))
         (child `(:path ,git :access write :recursive t))
         (exact `(:path ,git :access write))
         (niwa-protected-paths '(("**/.git/**" . read-only)))
         (request `(:operation-pattern "operation"
                    :additional-permissions (:file-system (,exact)))))
    (unwind-protect
        (progn
          (make-directory git)
          (dolist (bucket '(:session :persistent :defcustom))
            (dolist (approved (list nil child))
              (let* ((rule `("Bash" :pattern "operation" :action allow
                            :file-system ,(delq nil (list parent approved))))
                     (context `(:buckets ((,bucket ,rule)) :resource-grants nil))
                     (state (niwa-tool-exec-permission--additional-authority-state
                             "Bash" request context)))
                (should (eq (null approved) (and (plist-get state :missing) t)))
                (dolist (action '(deny ask))
                  (let ((state (niwa-tool-exec-permission--additional-authority-state
                                "Bash" request
                                `(:resource-grants (,parent ,child)
                                  :buckets ((:session ("Bash" :path ,git :action ,action)))))))
                    (if (eq action 'deny)
                        (should (eq 'sandbox-filesystem (plist-get state :deny-via)))
                      (should (plist-get state :missing)))))))))
      (delete-directory root t)))
  :doc "explicit resource deny settles before prompting"
  (let* ((write '(:path "/tmp/output" :access write))
         (state
          (niwa-tool-exec-permission--additional-authority-state
           "Bash"
           `(:additional-permissions (:file-system (,write)))
           '(:mode full-auto
                   :buckets
                   ((:session
                     ("Bash" :path "/tmp/output" :action deny)))))))
    (should (eq 'sandbox-filesystem (plist-get state :deny-via)))))

(niwa-deftest niwa-tool-exec-permission--prepare-additional-authority-request ()
  ,test
  (test)
  :doc "keeps default requests unchanged"
  (let ((request '(:level use-default :additional-permissions nil)))
    (should
     (eq request
         (niwa-tool-exec-permission--prepare-additional-authority-request
          "Bash" request nil))))
  :doc "attaches one shared approval cell to additive requests"
  (let ((request
         (niwa-tool-exec-permission--prepare-additional-authority-request
          "Bash"
          '(:level additive :additional-permissions (:network t))
          '(:mode ask :buckets ((:session))))))
    (should (equal '(:network t)
                   (plist-get (plist-get request :authority-state)
                              :missing)))
    (should (equal '(nil) (plist-get request :approval-cell)))))

(niwa-deftest niwa-tool-exec-permission--log-additional-authority ()
  ,test
  (test)
  :doc "logs each capability that the invocation receives"
  (let (calls)
    (cl-letf (((symbol-function
                'niwa-tool-exec-permission--log-permission-decision)
               (lambda (&rest args) (push args calls))))
      (niwa-tool-exec-permission--log-additional-authority
       "Bash"
       '(:requested
         (:network t
                   :file-system ((:path "/tmp/input" :access read))))
       nil t))
    (should (= 2 (length calls)))
    (should (seq-some
             (lambda (call) (memq 'sandbox-network call))
             calls))
    (should (seq-some
             (lambda (call) (memq 'sandbox-filesystem call))
             calls))))

(niwa-deftest niwa-tool-exec-permission--capabilities-prompt-async ()
  ,test
  (test)
  :doc "a prompted capability approval stores and logs each capability once"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules nil)
        (niwa-permission-reviewer 'user)
        logged outcome)
    (cl-letf (((symbol-function 'niwa-tool-exec-permission--log-permission-decision)
               (lambda (_tool outcome via &rest _props)
                 (push (cons outcome via) logged))))
 (niwa-pq-test--answering ((list 'allow-once))
                             (niwa-tool-exec-permission-check-bash-async
                              nil
                              '(:command "true"
                                         :sandbox_permissions "with_additional_permissions"
                                         :additional_permissions
                                         (:file_system (:read ["/tmp/capability-once/input"]))
                                         :justification "Read the fixture input"
                                         :permission-decision-metadata t)
                              (lambda (result) (setq outcome result)))))
    (should (eq 'allow (plist-get outcome :outcome)))
    (should (equal '((allow . sandbox-filesystem))
                   (cl-remove-if-not (lambda (item) (eq (cdr item) 'sandbox-filesystem))
                                     logged))))
  :doc "ask Bash:
recognized command authority is followed by a once-only network prompt"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules nil)
        entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) 'allow-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Reach the package registry?")
                                 (lambda (result) (setq outcome result))))
    (should (eq 'sandbox (plist-get entry :kind)))
    (should (equal '(:network t)
                   (plist-get entry :additional-permissions)))
    (should (eq 'allow outcome)))
  :doc "session Bash approval:
the default selection remembers the complete requested profile"
  (let* ((root (make-temp-file "niwa-bash-remember-" t))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         outcome)
    (unwind-protect
        (niwa-pq-test--answering
         ((list (lambda (card)
                  (should
                   (equal '(:operation t :network t)
                          (car
                           (plist-get
                            (plist-get card :entry) :remember-authority-cell)))))
                'allow-session)
          :session session)
         (niwa-tool-exec-permission-check-bash-async
          nil
          `(:command "pwd"
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions (:network t)
                     :justification "Contact the service?"
                     :permission-context
                     (:session ,session :workspace ,workspace :mode ask))
          (lambda (result) (setq outcome result)))
         (should (eq 'allow outcome))
         (should
          (seq-some
           (lambda (rule)
             (and (equal "pwd" (plist-get (cdr rule) :pattern))
                  (plist-get (cdr rule) :network)))
           (niwa-session-permission-rules session))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "session Bash approval:
selecting network stores qualified authority and reuses it without prompting"
  (let* ((root (make-temp-file "niwa-bash-network-" t))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (niwa-pq-test--answering
           ((list (lambda (card)
                    (cl-incf prompts)
                    ;; Select network on the card, unless it already is.
                    (unless (plist-get
                             (car (plist-get (plist-get card :entry)
                                             :remember-authority-cell))
                             :network)
                      (niwa-pq-test--press card "n")))
                  'allow-session)
            :session session)
           (niwa-tool-exec-permission-check-bash-async
            nil
            `(:command "pwd"
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions (:network t)
                       :justification "Contact the service?"
                       :permission-context
                       (:session ,session :workspace ,workspace :mode ask))
            (lambda (result) (setq outcome result))))
          (should (eq 'allow outcome))
          (should
           (seq-some
            (lambda (rule)
              (and (equal "pwd" (plist-get (cdr rule) :pattern))
                   (plist-get (cdr rule) :network)))
            (niwa-session-permission-rules session)))
          (setq outcome nil)
          (niwa-pq-test--answering ((list (lambda (_card)
                                               (cl-incf prompts))) :session session)
                                      (niwa-tool-exec-permission-check-bash-async
                                       nil
                                       `(:command "pwd"
                                                  :permission-context
                                                  (:session ,session :workspace ,workspace :mode ask))
                                       (lambda (result) (setq outcome result))))
          (should (= 1 prompts))
          (should (eq 'allow outcome)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "edits Bash:
additive network authority still prompts in edits mode"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules nil)
        enqueued outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq enqueued t))) 'deny-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Contact the service?")
                                 (lambda (result) (setq outcome result))))
    (should enqueued)
    (should (eq 'deny outcome)))
  :doc "full-auto Bash:
network authority proceeds without a prompt"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules nil)
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Contact the service?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome)))
  :doc "network-qualified Bash authority:
the matching command and its network request proceed without a prompt"
  (let* ((session
          (niwa-session--create
           :name "network"
           :permission-mode 'ask
           :permission-rules
           '(("Bash" :pattern "npx test" :action allow)
             ("Bash" :pattern "npx test" :network t :action allow))))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         enqueued outcome)
    (niwa-pq-test--answering (nil :session session)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 `(:command "npx test"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Reach the package registry?"
                                            :permission-context
                                            (:session ,session
                                                      :session-rules
                                                      (("Bash" :pattern "npx test" :action allow)
                                                       ("Bash" :pattern "npx test" :network t :action allow))))
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome)))
  :doc "workspace Bash approval:
selected operation and network authority are reused by another session"
  (let* ((root (make-temp-file "niwa-bash-workspace-network-" t))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (first (niwa-session-create "first" workspace))
         (second (niwa-session-create "second" workspace))
         (niwa--session first)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (niwa-pq-test--answering
           ((list (lambda (card)
                    (cl-incf prompts)
                    (unless (plist-get
                             (car (plist-get (plist-get card :entry)
                                             :remember-authority-cell))
                             :network)
                      (niwa-pq-test--press card "n")))
                  ;; Injected: this card offers no always key; the closure stores the rule.
                  '(inject always-allow))
            :session first)
           (niwa-tool-exec-permission-check-bash-async
            nil
            `(:command "pwd"
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions (:network t)
                       :justification "Contact the service?"
                       :permission-context
                       (:session ,first :workspace ,workspace :mode ask))
            (lambda (result) (setq outcome result))))
          (setq outcome nil
                niwa--session second)
          (niwa-pq-test--answering ((list (lambda (_card)
                                               (cl-incf prompts))))
                                      (niwa-tool-exec-permission-check-bash-async
                                       nil
                                       `(:command "pwd"
                                                  :sandbox_permissions "with_additional_permissions"
                                                  :additional_permissions (:network t)
                                                  :justification "Contact the service?"
                                                  :permission-context
                                                  (:session ,second :workspace ,workspace :mode ask))
                                       (lambda (result) (setq outcome result))))
          (should (= 1 prompts))
          (should (eq 'allow outcome)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "sandbox off:
network authority changes no boundary and does not prompt"
  (let* ((session (niwa-session--create
                   :name "off" :sandbox-mode 'off))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Contact the service?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome)))
  :doc "edits protected resource:
an ungranted exact filesystem path still prompts and stores session authority"
  (let* ((root (make-temp-file "niwa-bash-resource-" t))
         (path (file-name-concat root "secret"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (session (niwa-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'edits))
         (niwa--session session)
         (niwa-permission-mode 'edits)
         (niwa-permission-rules nil)
         (niwa-protected-paths `((,path . inaccessible)))
         entry outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (niwa-pq-test--answering
           ((list (lambda (card)
                    (setq entry (plist-get card :entry))
                    ;; The card's own default already selects this path.
                    (should (equal (list :operation t
                                         :file-system
                                         (list (list :path path :access 'read)))
                                   (car (plist-get entry
                                                   :remember-authority-cell)))))
                  'allow-session)
            :session session)
           (niwa-tool-exec-permission-check-bash-async
            nil
            `(:command ,(format "cat %s" path)
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions (:file_system (:read [,path]))
                       :justification "Read the protected file?"
                       :permission-context
                       (:mode edits :session ,session :workspace ,workspace
                              :resource-grants nil))
            (lambda (result) (setq outcome result))))
          (should (eq 'sandbox (plist-get entry :kind)))
          (should (equal path (plist-get entry :resource-path)))
          (should (eq 'read (plist-get entry :resource-access)))
          (should (eq 'allow outcome))
          (should-not (niwa-session-resource-grants session))
          (should
           (member `(:path ,path :access read)
                   (plist-get
                    (niwa-tool-exec-permission--remembered-additional-profile
                     "Bash" (format "cat %s" path) `(:session ,session))
                    :file-system))))
      (delete-directory root t)))
  :doc "session resource approval:
the complete requested path profile is selected by default"
  (let* ((root (make-temp-file "niwa-bash-resource-once-" t))
         (path (file-name-concat root "secret"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (session (niwa-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'edits))
         (niwa--session session)
         (niwa-permission-mode 'edits)
         (niwa-permission-rules nil)
         (niwa-protected-paths `((,path . inaccessible)))
         outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (niwa-pq-test--answering
           ((list (lambda (card)
                    (should-not
                     (null
                      (plist-get
                       (car
                        (plist-get (plist-get card :entry)
                                   :remember-authority-cell))
                       :file-system))))
                  'allow-session)
            :session session)
           (niwa-tool-exec-permission-check-bash-async
            nil
            `(:command ,(format "cat %s" path)
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions (:file_system (:read [,path]))
                       :justification "Read the protected file?"
                       :permission-context
                       (:mode edits :session ,session :workspace ,workspace
                              :resource-grants nil))
            (lambda (result) (setq outcome result))))
          (should (eq 'allow outcome))
          (should-not (niwa-session-resource-grants session))
          (should
           (member `(:path ,path :access read)
                   (plist-get
                    (niwa-tool-exec-permission--remembered-additional-profile
                     "Bash" (format "cat %s" path) `(:session ,session))
                    :file-system))))
      (delete-directory root t)))
  :doc "pregranted protected resource:
an exact session grant skips only the filesystem prompt"
  (let* ((root (make-temp-file "niwa-bash-pregrant-" t))
         (path (file-name-concat root "secret"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (grant `(:path ,path :access read))
         (session (niwa-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'edits
                   :resource-grants (list grant)))
         (niwa--session session)
         (niwa-permission-mode 'edits)
         (niwa-permission-rules nil)
         (niwa-protected-paths `((,path . inaccessible)))
         enqueued outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (niwa-pq-test--answering (nil :session session)
                                      (niwa-tool-exec-permission-check-bash-async
                                       nil
                                       `(:command ,(format "cat %s" path)
                                                  :sandbox_permissions "with_additional_permissions"
                                                  :additional_permissions (:file_system (:read [,path]))
                                                  :justification "Read the protected file?"
                                                  :permission-context
                                                  (:mode edits :session ,session :workspace ,workspace
                                                         :resource-grants (,grant)))
                                       (lambda (result) (setq outcome result))))
          (should-not (niwa-pq-test--admitted-p))
          (should (eq 'allow outcome)))
      (delete-directory root t)))
  :doc "target replacement:
the session command survives but its filesystem profile requires fresh approval"
  (let* ((root (make-temp-file "niwa-bash-incarnation-" t))
         (path (file-name-concat root "cache"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (profile `((:path ,path :access write)))
         (rule `("Bash" :pattern "make build"
                 :file-system ,profile :action allow))
         (session (niwa-session-create "resource" workspace))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         (prompts 0)
         prompt-kind
         outcome)
    (setf (niwa-session-permission-rules session) (list rule)
          (niwa-session-resource-grants session) profile)
    (unwind-protect
        (cl-labels
            ((check ()
               (setq outcome nil)
               (niwa-tool-exec-permission-check-bash-async
                nil
                `(:command "make build"
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:write [,path]))
                           :justification "Write the build cache?"
                           :permission-context
                           (:mode ask :session ,session :workspace ,workspace))
                (lambda (result) (setq outcome result)))))
          (niwa-pq-test--answering ((list (lambda (card)
                                               (let ((entry (plist-get card :entry)))
                                                 (cl-incf prompts)
                                                 (setq prompt-kind (plist-get entry :kind)))) 'allow-session))
                                      (check)
                                      (should (eq 'allow outcome))
                                      (should (= 0 prompts))
                                      (niwa-permission-invalidate-target-grants session)
                                      (check)
                                      (should (eq 'allow outcome))
                                      (should (= 1 prompts))
                                      (should (eq 'sandbox prompt-kind))
                                      (check)
                                      (should (eq 'allow outcome))
                                      (should (= 1 prompts))))
      (delete-directory root t)))
  :doc "pregranted resource with explicit ask:
an exact ask rule remains authoritative over the stored grant"
  (let* ((root (make-temp-file "niwa-bash-ask-grant-" t))
         (path (file-name-concat root "secret"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (grant `(:path ,path :access read))
         (session (niwa-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'edits
                   :resource-grants (list grant)
                   :permission-rules
                   `(("Bash" :path ,path :action ask))))
         (niwa--session session)
         (niwa-permission-mode 'edits)
         entry outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (niwa-pq-test--answering ((list (lambda (card)
                                               (let ((queued (plist-get card :entry)))
                                                 (setq entry queued))) 'deny-once) :session session)
                                      (niwa-tool-exec-permission-check-bash-async
                                       nil
                                       `(:command ,(format "cat %s" path)
                                                  :sandbox_permissions "with_additional_permissions"
                                                  :additional_permissions (:file_system (:read [,path]))
                                                  :justification "Read the protected file?"
                                                  :permission-context
                                                  (:mode edits :session ,session :workspace ,workspace
                                                         :session-rules (("Bash" :path ,path :action ask))
                                                         :resource-grants (,grant)))
                                       (lambda (result) (setq outcome result))))
          (should (eq 'sandbox (plist-get entry :kind)))
          (should (eq 'deny outcome)))
      (delete-directory root t)))
  :doc "resource deny:
an explicit exact path deny prevents filesystem escalation"
  (let* ((root (make-temp-file "niwa-bash-resource-deny-" t))
         (path (file-name-concat root "secret"))
         (workspace (niwa-workspace--create :type 'file :root root))
         (session (niwa-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto))
         (niwa--session session)
         (niwa-permission-mode 'full-auto)
         (niwa-permission-rules
          `(("Bash" :path ,path :action deny)))
         (niwa-protected-paths `((,path . inaccessible)))
         enqueued outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (niwa-pq-test--answering (nil :session session)
                                      (niwa-tool-exec-permission-check-bash-async
                                       nil
                                       `(:command ,(format "cat %s" path)
                                                  :sandbox_permissions "with_additional_permissions"
                                                  :additional_permissions (:file_system (:read [,path]))
                                                  :justification "Read the protected file?"
                                                  :permission-context
                                                  (:mode full-auto :session ,session :workspace ,workspace))
                                       (lambda (result) (setq outcome result))))
          (should-not (niwa-pq-test--admitted-p))
          (should (eq 'deny outcome)))
      (delete-directory root t)))
  :doc "explicit Bash deny:
command denial prevents network authority even in full-auto"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules '(("Bash" :pattern "pwd" :action deny)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "with_additional_permissions"
                                            :additional_permissions (:network t)
                                            :justification "Contact the service?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny outcome)))
  :doc "ask fresh Eval:
one prompt approves both Eval and additive network authority"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules nil)
        entries outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (push entry entries))) 'allow-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil
                                 '(:expression "(+ 1 2)"
                                               :mode "fresh"
                                               :sandbox_permissions "with_additional_permissions"
                                               :additional_permissions (:network t)
                                               :justification "Fetch package metadata?")
                                 (lambda (result) (setq outcome result))))
    (should (= 1 (length entries)))
    (should (eq 'eval (plist-get (car entries) :kind)))
    (should
     (equal '(:network t)
            (plist-get (car entries)
                       :missing-additional-permissions)))
    (should (eq 'allow outcome)))
  :doc "session fresh Eval approval:
the exact expression and selected network authority are reused together"
  (let* ((root (make-temp-file "niwa-eval-network-" t))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (niwa-pq-test--answering
           ((list (lambda (card)
                    (cl-incf prompts)
                    (unless (plist-get
                             (car (plist-get (plist-get card :entry)
                                             :remember-authority-cell))
                             :network)
                      (niwa-pq-test--press card "n")))
                  'allow-session)
            :session session)
           (niwa-tool-exec-permission-check-eval-async
            nil
            `(:expression "(+ 1 2)"
                          :mode "fresh"
                          :sandbox_permissions "with_additional_permissions"
                          :additional_permissions (:network t)
                          :justification "Fetch package metadata?"
                          :permission-context
                          (:session ,session :workspace ,workspace :mode ask))
            (lambda (result) (setq outcome result))))
          (should
           (member
            '("Eval" :expression "(+ 1 2)" :network t :action allow)
            (niwa-session-permission-rules session)))
          (setq outcome nil)
          (niwa-pq-test--answering ((list (lambda (_card)
                                               (cl-incf prompts))) :session session)
                                      (niwa-tool-exec-permission-check-eval-async
                                       nil
                                       `(:expression "(+ 1 2)"
                                                     :mode "fresh"
                                                     :permission-context
                                                     (:session ,session :workspace ,workspace :mode ask))
                                       (lambda (result) (setq outcome result))))
          (should (= 1 prompts))
          (should (eq 'allow outcome)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "Bash combines multiple filesystem capabilities under one owner"
  (let* ((origin "/root/worker")
         (request (niwa-request--create :origin origin))
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         origins
         outcome)
    (niwa-pq-test--answering
     ((list (lambda (card)
              (push (plist-get (plist-get card :entry) :origin) origins)
              ;; Answer from an unrelated buffer, on the card's own closure.
              (with-temp-buffer
                (niwa-pq-test--answer-captured card 'allow-once)))))
     (setq-local niwa--current-request request)
     (niwa-tool-exec-permission-check-bash-async
      nil
      '(:command "pwd"
                 :sandbox_permissions "with_additional_permissions"
                 :additional_permissions
                 (:file_system
                  (:read ["/tmp/niwa-first" "/tmp/niwa-second"]))
                 :justification "Inspect two requested files?")
      (lambda (result) (setq outcome result))))
    (should (equal (list origin) (nreverse origins)))
    (should (eq 'allow outcome)))
  :doc "filesystem callback logging retains its scoped session and owner"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-scoped-permission-log-" t)))
         (origin "/root/worker")
         (session (niwa-session--create
                   :name "main" :save-path dir :permission-mode 'ask))
         (request (niwa-request--create
                   :session session :origin origin))
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         (niwa-permission-log-enabled t)
         queued
         outcome)
    (unwind-protect
        (progn
          (niwa-pq-test--answering
           ((list (lambda (card) (setq queued card))) :session session)
           (setq-local niwa--session session
                       niwa--current-request request)
           (niwa-tool-exec-permission-check-bash-async
            nil
            '(:command "pwd"
                       :permission-decision-metadata t
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions
                       (:file_system (:read ["/tmp/niwa-scoped-log"]))
                       :justification "Inspect the requested file?")
            (lambda (result) (setq outcome result)))
           ;; Answer from an unrelated buffer, on the card's own closure.
           (with-temp-buffer
             (niwa-pq-test--answer-captured queued 'allow-once)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry
                 (cl-find
                  'sandbox-filesystem
                  (niwa-tool-exec-permission-test--read-permission-log session)
                  :key (lambda (item) (plist-get item :via)))))
            (should (equal origin (plist-get entry :origin)))
            (should (eq 'sandbox-filesystem (plist-get entry :via)))))
      (delete-directory dir t)))
  :doc "combined Eval authority retains one scoped request owner"
  (let* ((origin "/root/worker")
         (request (niwa-request--create :origin origin))
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         entries
         outcome)
    (niwa-pq-test--answering
     ((list (lambda (card)
              (push (cons (plist-get (plist-get card :entry) :kind)
                          (plist-get (plist-get card :entry) :origin))
                    entries)
              (with-temp-buffer
                (niwa-pq-test--answer-captured card 'allow-once)))))
     (setq-local niwa--current-request request)
     (niwa-tool-exec-permission-check-eval-async
      nil
      '(:expression "(+ 1 2)"
                    :mode "fresh"
                    :sandbox_permissions "with_additional_permissions"
                    :additional_permissions (:network t)
                    :justification "Fetch package metadata?")
      (lambda (result) (setq outcome result))))
    (should (equal (list (cons 'eval origin))
                   (nreverse entries)))
    (should (eq 'allow outcome)))
  :doc "full-auto fresh Eval:
both Eval and network authority proceed without prompts"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules nil)
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil
                                 '(:expression "(+ 1 2)"
                                               :mode "fresh"
                                               :sandbox_permissions "with_additional_permissions"
                                               :additional_permissions (:network t)
                                               :justification "Fetch package metadata?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome))))

(niwa-deftest niwa-tool-exec-permission--escalation-prompt-async ()
  ,test
  (test)
  :doc "full escalation cannot override a Plan request's mutation restriction"
  (let* ((niwa--current-request (niwa-request--create :plan-read-only t))
         (niwa-permission-mode 'full-auto)
         (niwa-permission-rules nil)
         entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued)))))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "make test" :sandbox_permissions "require_escalated"
                                            :justification "Run the build outside confinement")
                                 (lambda (result) (setq outcome result))))
    (should-not entry)
    (should (eq 'deny outcome)))
  :doc "explicit operation asks survive remembered escalation at admission and recheck"
  (dolist (command '("make test" "cat /tmp/review-protected/value"))
    (let ((niwa-protected-paths '(("/tmp/review-protected/**" . inaccessible)))
          (niwa-permission-mode 'edits)
          (niwa-permission-rules
           `(("Bash" :pattern ,command :action ask)
             ("Bash" :pattern ,command :sandbox-permissions require-escalated :action allow)))
          entry outcome)
      (niwa-pq-test--answering ((list (lambda (card)
                                           (let ((queued (plist-get card :entry)))
                                             (setq entry queued)))))
                                  (niwa-tool-exec-permission-check-bash-async
                                   nil `(:command ,command :sandbox_permissions "require_escalated"
                                                  :justification "Run the build without confinement")
                                   (lambda (result) (setq outcome result))))
      (should entry)
      (should-not outcome)
      (should (eq 'rule (plist-get entry :permission-via)))
      (should (eq 'ask
                  (niwa-tool-exec-permission-reevaluate
                   entry (list :tool-name "Bash" :mode 'edits
                               :buckets (list (cons :defcustom niwa-permission-rules))))))))
  :doc "edits still asks:
full escalation prompts without a directly authored qualified rule"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules nil)
        entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) 'allow-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should (eq 'sandbox (plist-get entry :kind)))
    (should (eq 'require-escalated
                (plist-get entry :sandbox-permissions)))
    (should (eq 'allow outcome)))
  :doc "SSH host retry approval: allow-once does not authorize a later invocation"
  (let* ((session (niwa-session--create
                   :name "ssh-host-retry"
                   :sandbox-mode 'required))
         (niwa--session session)
         (niwa-permission-mode 'edits)
         (niwa-permission-rules nil)
         (args '(:command "ssh -G localhost"
                         :sandbox_permissions "require_escalated"
                         :justification "Confirmed system SSH owner UID mapping failure"))
         entries outcomes)
    (niwa-pq-test--answering
     ((lambda (n _card)
        (list (lambda (card) (push (plist-get card :entry) entries))
              (if (= n 1) 'allow-once 'deny-once)))
      :session session)
     (dotimes (_ 2)
       (niwa-tool-exec-permission-check-bash-async
        nil args (lambda (result) (push result outcomes)))))
    (should (equal '(deny allow) outcomes))
    (should (= 2 (length entries)))
    (dolist (entry entries)
      (should (eq 'sandbox (plist-get entry :kind)))
      (should (eq 'require-escalated (plist-get entry :sandbox-permissions)))
      (should (equal "ssh -G localhost" (plist-get entry :detail))))
    (should-not (niwa-session-permission-rules session))
    (should-not (niwa-session-resource-grants session)))
  :doc "sandbox off:
full escalation adds no boundary prompt but ordinary command authority remains"
  (let* ((session (niwa-session--create
                   :name "off" :sandbox-mode 'off))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         entries outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (push entry entries))) 'allow-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "rm file"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Confinement is already disabled")
                                 (lambda (result) (setq outcome result))))
    (should (= 1 (length entries)))
    (should (eq 'bash (plist-get (car entries) :kind)))
    (should (eq 'allow outcome)))
  :doc "qualified direct allow:
an exact user-authored escalation rule skips the prompt"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules
         '(("Bash" :pattern "pwd"
            :sandbox-permissions require-escalated
            :action allow)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome)))
  :doc "one-shot qualified allow:
an inherited escalation rule still requires one-time sandbox approval"
  (let* ((session (niwa-session--create
                   :name "one-shot-escalation"
                   :permission-mode 'ask :sandbox-mode 'best-effort))
         (niwa--session session)
         ;; A dynamic fallback must not replace this session's Ask authority.
         (niwa-permission-mode 'full-auto)
         (niwa-permission-rules
          '(("Bash" :pattern "pwd"
             :sandbox-permissions require-escalated
             :action allow)))
         entry outcome)
    (should (eq 'ask (niwa-permission-mode-effective session)))
    ;; Injected: this escalation card offers no session key; the case checks
    ;; what the closure stores for a session outcome.
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) '(inject allow-session)) :session session)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 `(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?"
                                            :permission-context
                                            (:session ,session :one-shot-mutations-p t))
                                 (lambda (result) (setq outcome result))))
    (should (eq 'sandbox (plist-get entry :kind)))
    (should (plist-get entry :once-only))
    (should (eq 'allow outcome))
    (should-not (niwa-session-permission-rules session))
    (should-not (niwa-session-resource-grants session))
    (should (eq 'ask (niwa-session-permission-mode session))))
  :doc "ordinary allow is insufficient:
an unqualified command rule cannot authorize full escalation"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules
         '(("Bash" :pattern "pwd" :action allow)))
        enqueued outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq enqueued t))) 'deny-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should enqueued)
    (should (eq 'deny outcome)))
  :doc "delegated allow is insufficient:
an invocation-qualified rule cannot grant full escalation"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules nil)
        enqueued outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq enqueued t))) 'deny-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?"
                                            :permission-context
                                            (:buckets
                                             ((:invocation
                                               ("Bash" :sandbox-permissions require-escalated
                                                :action allow)))))
                                 (lambda (result) (setq outcome result))))
    (should enqueued)
    (should (eq 'deny outcome)))
  :doc "ordinary deny remains final:
an explicit command deny prevents escalation without prompting"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules
         '(("Bash" :pattern "pwd" :action deny)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny outcome)))
  :doc "qualified compound deny remains final:
a denied segment cannot hide behind a broad full-escalation allow"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)
           ("Bash" :pattern "rm *"
            :sandbox-permissions require-escalated :action deny)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd && rm -rf /"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny outcome)))
  :doc "qualified nested deny remains final:
a harvested command substitution cannot hide behind a broad allow"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)
           ("Bash" :pattern "rm *"
            :sandbox-permissions require-escalated :action deny)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "echo $(rm -rf /)"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny outcome)))
  :doc "session approval:
the prompt stores an exact execution-level-qualified pattern rule"
  (let* ((session (niwa-session--create :name "full-escalation"))
         (niwa--session session)
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil)
         outcome)
    (niwa-pq-test--answering ((list 'allow-session) :session session)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 `(:command "pwd"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?"
                                            :permission-context (:session ,session))
                                 (lambda (result) (setq outcome result))))
    (should (eq 'allow outcome))
    (should
     (equal
      '(("Bash" :pattern "pwd"
         :sandbox-permissions require-escalated
         :action allow))
      (niwa-session-permission-rules session))))
  :doc "delegated expansion:
trust-literal execution cannot use even a matching direct escalation rule"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules
         '(("Bash" :pattern "pwd"
            :sandbox-permissions require-escalated
            :action allow)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "pwd"
                                            :trust-literal-p t
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny (car outcome))))
  :doc "reusable escalation rules:
prompts propose only literal stable Bash and Eval rules"
  (should (niwa-tool-exec-permission--full-escalation-reusable-rule-p "Bash" "pwd"))
  (should
   (niwa-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "rm -rf /"))
  (should-not
   (niwa-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "echo $(pwd)"))
  (should-not
   (niwa-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "printf '%s' '*.tmp'"))
  (should
   (niwa-tool-exec-permission--full-escalation-reusable-rule-p
    "Eval" "(+ 1 2)"))
  :doc "dangerous escalation prompt:
literal dangerous Bash offers reusable full-escalation authority"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules nil)
        entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) 'deny-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "rm -rf /"
                                            :sandbox_permissions "require_escalated"
                                            :justification "Run without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should (plist-get entry :include-always))
    (should (eq 'deny outcome)))
  :doc "Eval escalation prompt:
literal fresh Eval offers reusable full-escalation authority"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules nil)
        entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) 'deny-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil
                                 '(:expression "(delete-file \"important\")"
                                               :mode "fresh"
                                               :sandbox_permissions "require_escalated"
                                               :justification "Run fresh Eval without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should (plist-get entry :include-always))
    (should (eq 'deny outcome)))
  :doc "fresh Eval direct allow:
a broad qualified Eval rule authorizes unconfined fresh execution"
  (let ((niwa-permission-mode 'ask)
        (niwa-permission-rules
         '(("Eval" :sandbox-permissions require-escalated :action allow)))
        enqueued outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil
                                 '(:expression "(+ 1 2)"
                                               :mode "fresh"
                                               :sandbox_permissions "require_escalated"
                                               :justification "Run fresh Eval without confinement?")
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'allow outcome)))
  :doc "full escalation diagnostics:
the decision log identifies complete confinement bypass authority"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-full-escalation-log-" t)))
         (session (niwa-session--create
                   :name "full-escalation" :save-path dir))
         (niwa--session session)
         (niwa-permission-log-enabled t)
         (niwa-permission-rules
          '(("Bash" :pattern "pwd"
             :sandbox-permissions require-escalated
             :action allow)))
         outcome)
    (unwind-protect
        (progn
          (niwa-tool-exec-permission-check-bash-async
           nil
           `(:command "pwd"
                      :sandbox_permissions "require_escalated"
                      :justification "Run without confinement?"
                      :permission-decision-metadata t
                      :permission-context (:session ,session))
           (lambda (result) (setq outcome result)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (niwa-tool-exec-permission-test--read-permission-log
                             session))))
            (should (eq 'sandbox-full-escalation (plist-get entry :via)))
            (should (eq 'require-escalated
                        (plist-get entry :sandbox-permissions)))))
      (delete-directory dir t))))

(niwa-deftest niwa-tool-exec-permission-check-bash-async ()
  ,test
  (test)
  :doc "extracts command from input and returns permission"
  (let ((niwa-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (niwa-bash-dangerous-commands nil)
        outcome)
    (niwa-tool-exec-permission-check-bash-async
     nil '(:command "echo hello") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "outside-root resource:
default Bash requests exact additive read authority before parent traversal"
  (let* ((parent (make-temp-file "niwa-bash-boundary-" t))
         (root (file-name-concat parent "workspace"))
         (default-directory (file-name-as-directory root))
         (niwa-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (make-directory root)
          (niwa-tool-exec-permission-check-bash-async
           nil
           `(:command "rg -n TODO .."
                      :permission-context
                      (:mode ask :allowed-roots (,root) :resource-grants nil))
           (lambda (result) (setq outcome result)))
          (should (eq 'deny (car-safe outcome)))
          (should (string-match-p "additional_permissions.file_system.read"
                                  (cdr outcome))))
      (delete-directory parent t)))
  :doc "inside-root symlink:
default Bash authorizes the resolved outside resource"
  (let* ((parent (make-temp-file "niwa-bash-boundary-link-" t))
         (root (file-name-concat parent "workspace"))
         (secret (file-name-concat parent "secret"))
         (link (file-name-concat root "innocent"))
         (default-directory (file-name-as-directory root))
         (niwa-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (make-directory root)
          (write-region "secret" nil secret nil 'silent)
          (make-symbolic-link secret link)
          (niwa-tool-exec-permission-check-bash-async
           nil
           `(:command ,(format "cat %s" link)
                      :permission-context
                      (:mode ask :allowed-roots (,root) :resource-grants nil))
           (lambda (result) (setq outcome result)))
          (should (eq 'deny (car-safe outcome)))
          (should (string-match-p (regexp-quote secret) (cdr outcome))))
      (delete-directory parent t)))
  :doc "inside-root resource:
default Bash keeps bare dot inspection automatic"
  (let* ((root (make-temp-file "niwa-bash-boundary-" t))
         (default-directory (file-name-as-directory root))
         (niwa-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (niwa-tool-exec-permission-check-bash-async
           nil
           `(:command "rg -n TODO ."
                      :permission-context
                      (:mode ask :allowed-roots (,root) :resource-grants nil))
           (lambda (result) (setq outcome result)))
          (should (eq 'allow outcome)))
      (delete-directory root t)))
  :doc "returns nil when input has no command"
  (let (outcome)
    (niwa-tool-exec-permission-check-bash-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (null outcome)))
  :doc "queued Bash retains the policy cause and admission mode"
  (dolist (case '((ask nil mode)
                  (edits (("Bash" :pattern "make *" :action ask)) rule)))
    (pcase-let ((`(,mode ,rules ,via) case))
      (let ((niwa-permission-reviewer 'user) entry outcome)
        (niwa-pq-test--answering
         ((list (lambda (card) (setq entry (plist-get card :entry)))
                'deny-once)
          :mode mode)
         (niwa-tool-exec-permission-check-bash-async
          nil
          (list :command "make test" :permission-context
                (list :mode mode :buckets (list (cons :session rules))))
          (lambda (result) (setq outcome result))))
        (should (eq 'deny outcome))
        (should (eq mode (plist-get entry :permission-mode-effective)))
        (should (eq via (plist-get entry :permission-via))))))
  :doc "returns deny for denied commands"
  (let ((niwa-permission-rules
         '(("Bash" :pattern "rm*" :action deny)))
        (niwa-bash-dangerous-commands nil)
        outcome)
    (niwa-tool-exec-permission-check-bash-async
     nil '(:command "rm -rf /") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "metadata mode logs sanitized Bash allow decision"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-bash-log-" t)))
         (session (niwa-session--create
                   :name "main" :save-path dir
                   :permission-mode 'ask))
         (niwa-permission-rules
          '(("Bash" :pattern "git log:*" :action allow)))
         (niwa-bash-dangerous-commands nil)
         (niwa-permission-log-enabled t)
         outcome)
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-tool-exec-permission-check-bash-async
           nil '(:command "git log --oneline secret-token"
                          :permission-decision-metadata t)
           (lambda (r) (setq outcome r)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (niwa-tool-exec-permission-test--read-permission-log
                             session))))
            (should (eq 'permission-decision (plist-get entry :event)))
            (should (equal "Bash" (plist-get entry :tool-name)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'bash-classifier (plist-get entry :via)))
            (should (equal "git" (plist-get entry :specifier-value)))
            (should-not (equal "git log --oneline secret-token"
                               (plist-get entry :specifier-value)))))
      (delete-directory dir t)))
  :doc "side Bash decisions forward no command-derived values to parent audit"
  (let* ((parent (niwa-session--create :name "parent"))
         (session (niwa-session--create
                   :name "side" :audit-session parent
                   :permission-mode 'ask))
         (niwa-permission-rules
          '(("Bash" :pattern "git log:*" :action allow)))
         (niwa-bash-dangerous-commands nil)
         calls outcome)
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-telemetry-record)
                 (lambda (target event &rest props)
                   (push (list target event props) calls))))
        (niwa-tool-exec-permission-check-bash-async
         nil '(:command "git log --oneline secret-token"
                        :permission-decision-metadata t)
         (lambda (result) (setq outcome result)))))
    (should (eq 'allow (plist-get outcome :outcome)))
    (let ((call
           (cl-find-if
            (lambda (entry)
              (and (eq parent (car entry))
                   (eq 'permission-decision (cadr entry))))
            calls)))
      (should call)
      (let ((props (nth 2 call)))
        (should (eq 'btw (plist-get props :conversation-scope)))
        (should (eq 'bash-classifier (plist-get props :via)))
        (should (eq :pattern (plist-get props :specifier-key)))
        (should-not (plist-member props :specifier-value))
        (should-not
         (string-match-p (regexp-opt '("git" "secret-token"))
                         (prin1-to-string props))))))
  :doc "literal dangerous approval stores only the exact command"
  (let* ((workspace
          (niwa-workspace--create
           :type 'project :id "dangerous-rule" :root temporary-file-directory
           :name "dangerous-rule" :file-cache nil))
         (session
          (niwa-session--create
           :name "dangerous-rule" :workspace workspace
           :permission-mode 'ask))
         (niwa-permission-rules nil)
         (niwa-permission-reviewer 'user)
         ;; The resource has to sit inside the allowed root, or the exact
         ;; command never reaches the rule it is meant to store.
         (target (file-name-concat temporary-file-directory
                                   "niwa-exact-target"))
         entry outcome)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued))) 'allow-session) :session session)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 `(:command ,(format "rm -rf %s" target)
                                            :permission-context
                                            (:session ,session :workspace ,workspace
                                                      :mode ask :allowed-roots (,temporary-file-directory)))
                                 (lambda (result) (setq outcome result))))
    (should (plist-get entry :include-always))
    (should (eq outcome 'allow))
    (should
     (equal
      `(("Bash" :pattern ,(format "rm -rf %s" target) :action allow))
      (niwa-session-permission-rules session))))
  :doc "full-auto allows dangerous Bash without enqueueing"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules nil)
        (niwa-bash-dangerous-commands '("rm"))
        enqueued
        outcome)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-bash-async
                                 nil '(:command "rm foo") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not (niwa-pq-test--admitted-p)))
  :doc "queued prompt entries preserve raw commands and add counted summary"
  (let ((niwa-permission-rules nil)
        (niwa-bash-dangerous-commands nil)
        captured)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq captured entry)))))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil
                                 '(:command "git add -- a && git commit -m x && git add -- b && git commit -m y")
                                 #'ignore))
    (should (equal '("git" "git" "git" "git")
                   (plist-get captured :commands)))
    (should (equal "git (4)"
                   (plist-get captured :commands-summary))))
  :doc "prompts user and returns allow when pattern says ask and user approves"
  ;; Bash prompts through the queue's 5-button overlay instead of the
  ;; direct prompt primitive.  Mock the queued entry point and
  ;; exercise the allow-once outcome.
  (let ((niwa-permission-rules nil)
        (niwa-bash-dangerous-commands '("sudo"))
        outcome)
    (niwa-pq-test--answering ((list 'allow-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "prompts user and returns deny when pattern says ask and user denies"
  (let ((niwa-permission-rules nil)
        (niwa-bash-dangerous-commands '("sudo"))
        outcome)
    (niwa-pq-test--answering ((list 'deny-once))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  ;; Feedback is part of the authoritative queued prompt vocabulary.
  ;; Mock the queue entry point and deliver it directly to the
  ;; adapter's callback.
  (let ((niwa-permission-rules nil)
        (niwa-bash-dangerous-commands '("sudo"))
        outcome)
    (niwa-pq-test--answering ((list '(feedback . "use git instead")))
                                (niwa-tool-exec-permission-check-bash-async
                                 nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Command cancelled by user. Feedback: use git instead"
                   (cdr outcome))))
  :doc "allow-session stores the suggested reusable Bash prefix pattern"
  (let* ((root (make-temp-file "niwa-bash-rules-" t))
         (workspace (niwa-workspace-get-or-create
                     'file root root "test"))
         (session (niwa-session-create "main" workspace))
         (niwa--session session)
         (niwa-permission-rules nil)
         (niwa-bash-dangerous-commands nil)
         outcome)
    (unwind-protect
        (niwa-pq-test--answering ((list 'allow-session))
                                    (niwa-tool-exec-permission-check-bash-async
                                     nil '(:command "git describe --tags")
                                     (lambda (r) (setq outcome r)))
                                    (should (eq outcome 'allow))
                                    (should (member '("Bash" :pattern "git describe:*" :action allow)
                                                    (niwa-session-permission-rules session))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-tool-exec-permission--normalize-prompt-result ()
  ,test
  (test)
  :doc "normalizes stored and feedback prompt outcomes"
  (let (stored)
    (should
     (eq 'allow
         (niwa-tool-exec-permission--normalize-prompt-result
          'allow-session
          (lambda (outcome) (setq stored outcome) 'allow)
          "Feedback: ")))
    (should (eq stored 'allow-session))
    (should
     (equal '(deny . "Feedback: no")
            (niwa-tool-exec-permission--normalize-prompt-result
             '(feedback . "no") #'ignore "Feedback: ")))))

(niwa-deftest niwa-tool-exec-permission--eval-prompt-result ()
  ,test
  (test)
  :doc "normalizes direct, feedback, abort, and unknown Eval outcomes"
  (should (eq 'allow
              (niwa-tool-exec-permission--eval-prompt-result
               'allow-once nil nil "(+ 1 2)" nil)))
  (should (eq 'deny
              (niwa-tool-exec-permission--eval-prompt-result
               'deny-once nil nil "(+ 1 2)" nil)))
  (should (equal '(deny . "Eval cancelled by user. Feedback: no")
                 (niwa-tool-exec-permission--eval-prompt-result
                  '(feedback . "no") nil nil "(+ 1 2)" nil)))
  (should (eq 'aborted
              (niwa-tool-exec-permission--eval-prompt-result
               'aborted nil nil "(+ 1 2)" nil)))
  (should (eq 'deny
              (niwa-tool-exec-permission--eval-prompt-result
               'unexpected nil nil "(+ 1 2)" nil)))
  :doc "applies stored Eval outcomes through the canonical rule writer"
  (let ((session (niwa-session--create :name "eval-prompt")))
    (should (eq 'allow
                (niwa-tool-exec-permission--eval-prompt-result
                 'allow-session session nil "(+ 1 2)" nil)))
    (should (eq 'deny
                (niwa-tool-exec-permission--eval-prompt-result
                 'deny-session session nil "(delete-file x)" nil)))
    (should
     (equal
      '(("Eval" :expression "(+ 1 2)" :action allow)
        ("Eval" :expression "(delete-file x)" :action deny))
      (niwa-session-permission-rules session))))
  :doc "persists always-allow Eval rules to real workspace storage"
  (let* ((root (make-temp-file "niwa-eval-prompt-" t))
         (workspace
          (niwa-workspace--create
           :type 'project :id "eval-prompt" :root root :name "eval-prompt"))
         (session
          (niwa-session--create
           :name "eval-prompt" :workspace workspace)))
    (unwind-protect
        (progn
          (should
           (eq 'allow
               (niwa-tool-exec-permission--eval-prompt-result
                'always-allow session workspace "(message x)" nil)))
          (should
           (member '("Eval" :expression "(message x)" :action allow)
                   (niwa-permission-persistence-load-rules workspace))))
      (delete-directory root t)))
  :doc "returns structured Eval metadata when requested"
  (let ((result (niwa-tool-exec-permission--eval-prompt-result
                 '(deny . "reason") nil nil "(+ 1 2)" t)))
    (should (eq 'deny (plist-get result :outcome)))
    (should (equal '(deny . "reason") (plist-get result :raw-outcome)))
    (should (eq 'eval-policy (plist-get result :via)))
    (should (plist-get result :logged))))

(niwa-deftest niwa-tool-exec-permission-check-eval-async ()
  ,test
  (test)
  :doc "remembered Eval matches the complete literal expression in every authority scope"
  (dolist (authority '(ordinary network escalated))
    (dolist (scope '(allow-session always-allow))
      (dolist (text '("*" "?" "[ab]"))
        (let* ((root (make-temp-file "niwa-literal-eval-" t))
               (niwa-user-dir (file-name-concat root "global"))
               (niwa-permission--store-cache (make-hash-table :test #'equal))
               (workspace (niwa-workspace--create :type 'project :root root))
               (session (niwa-session--create
                                               :name "literal" :workspace workspace))
               (mode (if (eq authority 'escalated) 'full-auto 'ask))
               (niwa-permission-rules nil)
               (expression (format "(message %S)" text))
               (prompts 0)
               outcome)
          (unwind-protect
              (with-temp-buffer
                (setq-local niwa--session session)
                (cl-labels
                    ((check (code)
                       (setq outcome nil)
                       (niwa-tool-exec-permission-check-eval-async
                        nil
                        (append
                         (list :expression code :mode (if (eq authority 'ordinary) "live" "fresh")
                               :permission-context (list :session session :workspace workspace :mode mode))
                         (pcase authority
                           ('network '(:sandbox_permissions "with_additional_permissions"
                                       :additional_permissions (:network t)
                                       :justification "Test network authority"))
                           ('escalated '(:sandbox_permissions "require_escalated"
                                         :justification "Test full escalation"))))
                        (lambda (result) (setq outcome result)))))
                  (niwa-pq-test--answering
                   ((list (lambda (card)
                            (cl-incf prompts)
                            (when (and (eq authority 'network)
                                       (not (plist-get
                                             (car (plist-get
                                                   (plist-get card :entry)
                                                   :remember-authority-cell))
                                             :network)))
                              (niwa-pq-test--press card "n")))
                          scope))
                   (check expression))
                  (should (eq outcome 'allow))
                  (should (= prompts 1))
                  ;; Workspace reuse must survive a fresh disk read, with no
                  ;; session rule or persistence-cache entry to supply it.
                  (when (eq scope 'always-allow)
                    (setf (niwa-session-permission-rules session) nil)
                    (clrhash niwa-permission--store-cache))
                  (niwa-pq-test--answering ((list (lambda (card)
                                                       (let ((entry (plist-get card :entry)))
                                                         (cl-incf prompts))) 'deny-once))
                                              (check expression)
                                              (should (eq outcome 'allow))
                                              (should (= prompts 1))
                                              (dolist (other (list "(message \"a\")" (concat expression " ")))
                                                (let ((previous prompts))
                                                  (check other)
                                                  (should (eq outcome 'deny))
                                                  (should (= prompts (1+ previous))))))))
            (delete-directory root t))))))
  :doc "returns deny when input has no expression"
  (let (outcome)
    (niwa-tool-exec-permission-check-eval-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "metadata mode logs Eval allow without expression payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "niwa-eval-log-" t)))
         (session (niwa-session--create
                   :name "main" :save-path dir
                   :permission-mode 'full-auto))
         (niwa-permission-rules nil)
         (niwa-permission-log-enabled t)
         outcome)
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session session)
          (niwa-tool-exec-permission-check-eval-async
           nil '(:expression "(delete-file \"secret\")"
                             :permission-decision-metadata t)
           (lambda (r) (setq outcome r)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (niwa-tool-exec-permission-test--read-permission-log
                             session))))
            (should (eq 'permission-decision (plist-get entry :event)))
            (should (equal "Eval" (plist-get entry :tool-name)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'eval-policy (plist-get entry :via)))
            (should-not (plist-member entry :expression))))
      (delete-directory dir t)))
  :doc "returns allow when user approves"
  (let (outcome)
    (niwa-pq-test--answering ((list 'allow-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "enqueues requested mode and preserve_ui metadata"
  (let (entry)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((queued (plist-get card :entry)))
                                           (setq entry queued)))))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(delete-other-windows)"
                                                   :mode "live"
                                                   :preserve_ui :json-false)
                                 #'ignore))
    (should (equal "live" (plist-get entry :mode)))
    (should-not (plist-get entry :preserve-ui)))
  :doc "returns deny reason for invalid mode instead of signaling"
  (dolist (mode '("bogus" "batch"))
    (let (outcome)
      (niwa-tool-exec-permission-check-eval-async
       nil (list :expression "(+ 1 2)" :mode mode)
       (lambda (r) (setq outcome r)))
      (should (consp outcome))
      (should (eq 'deny (car outcome)))
      (should (equal (format "Invalid execution permission request: %S"
                             (format "Unknown Eval mode: %s" mode))
                     (cdr outcome)))))
  :doc "refuses remote Fresh Eval before prompting"
  (let* ((target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (niwa-session--create
                   :name "remote-eval"
                   :execution-target target
                   :working-directory "/ssh:user@host:/srv/project/"))
         outcome
         enqueued)
    (niwa-pq-test--answering (nil :session session)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil
                                 `(:expression "(+ 1 2)"
                                               :mode "fresh"
                                               :permission-context (:session ,session))
                                 (lambda (result) (setq outcome result))))
    (should-not (niwa-pq-test--admitted-p))
    (should (eq 'deny (car outcome)))
    (should
     (string-match-p "Fresh Eval is unavailable for remote sessions"
                     (cdr outcome))))
  :doc "returns deny when user denies"
  (let (outcome)
    (niwa-pq-test--answering ((list 'deny-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  (let (outcome)
    (niwa-pq-test--answering ((list '(feedback . "too dangerous")))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(delete-file \"/etc/passwd\")")
                                 (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Eval cancelled by user. Feedback: too dangerous"
                   (cdr outcome))))
  :doc "trusted Eval with active allow bypasses prompt"
  (let ((niwa-permission-rules '(("Eval" :action allow)))
        outcome enqueued)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)" :trust-literal-p t)
                                 (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not (niwa-pq-test--admitted-p)))

  :doc "full-auto Eval bypasses prompt without active allow"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules nil)
        outcome enqueued)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)")
                                 (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not (niwa-pq-test--admitted-p)))

  :doc "explicit context Eval uses context mode without ambient session"
  (let* ((session (niwa-session--create
                   :name "test" :permission-mode 'full-auto))
         (context (niwa-permission--invocation-context
                   :tool-name "Eval"
                   :session session))
         (niwa-permission-mode 'ask)
         (niwa-permission-rules nil))
    (should (eq 'allow
                (plist-get (niwa-tool-exec-permission--check-eval-permission
                            :permission-context context)
                           :outcome))))

  :doc "queued Eval retains permission cause separately from execution mode"
  (dolist (case '((ask nil mode)
                  (edits (("Eval" :action ask)) rule)))
    (pcase-let ((`(,mode ,rules ,via) case))
      (let (entry outcome)
        (niwa-pq-test--answering
         ((list (lambda (card) (setq entry (plist-get card :entry)))
                'deny-once)
          :mode mode)
         (niwa-tool-exec-permission-check-eval-async
          nil
          (list :expression "(+ 1 2)" :mode "fresh" :permission-context
                (list :mode mode :buckets (list (cons :session rules))))
          (lambda (result) (setq outcome result))))
        (should (eq 'deny outcome))
        (should (equal "fresh" (plist-get entry :mode)))
        (should (eq mode (plist-get entry :permission-mode-effective)))
        (should (eq via (plist-get entry :permission-via))))))
  :doc "explicit Eval deny beats full-auto"
  (let ((niwa-permission-mode 'full-auto)
        (niwa-permission-rules '(("Eval" :action deny)))
        outcome enqueued)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)")
                                 (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny))
    (should-not (niwa-pq-test--admitted-p)))

  :doc "trusted Eval without active allow denies without prompting"
  (let ((niwa-permission-rules nil)
        outcome enqueued)
    (niwa-pq-test--answering (nil)
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)" :trust-literal-p t)
                                 (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should-not (niwa-pq-test--admitted-p)))

  :doc "explicit Eval deny beats trusted allow"
  (let* ((ws (niwa-workspace--create
              :type 'file :id "eval-deny" :root "/tmp/eval-deny"
              :name "eval-deny"
              :file-cache (niwa-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (niwa-session-create "main" ws))
         (request (niwa-request--create
                   :session session
                   :skill-permission-rules '(("Eval" :action allow))))
         (niwa-permission-rules nil)
         outcome)
    (setf (niwa-session-permission-rules session)
          '(("Eval" :action deny)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (setq-local niwa--current-request request)
      (niwa-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)" :trust-literal-p t)
       (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))

  :doc "direct Eval allow authorizes model-generated Eval without prompting"
  (let ((niwa-permission-rules '(("Eval" :action allow)))
        outcome enqueued)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq enqueued t))) 'allow-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not enqueued))

  :doc "explicit Eval ask prompts in edits"
  (let ((niwa-permission-mode 'edits)
        (niwa-permission-rules '(("Eval" :action ask)))
        outcome enqueued)
    (niwa-pq-test--answering ((list (lambda (card)
                                         (let ((entry (plist-get card :entry)))
                                           (setq enqueued t))) 'deny-once))
                                (niwa-tool-exec-permission-check-eval-async
                                 nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny))
    (should enqueued))
  )

(niwa-deftest niwa-tool-exec-permission--bash-prompt-result ()
  ,test
  (test)
  :doc "normalizes direct, feedback, abort, and unknown Bash outcomes"
  (should (eq 'allow
              (niwa-tool-exec-permission--bash-prompt-result
               'allow nil nil "make test" nil nil)))
  (should (eq 'deny
              (niwa-tool-exec-permission--bash-prompt-result
               'deny nil nil "make test" nil nil)))
  (should (equal '(deny . "Command cancelled by user. Feedback: no")
                 (niwa-tool-exec-permission--bash-prompt-result
                  '(feedback . "no") nil nil "make test" nil nil)))
  (should (eq 'aborted
              (niwa-tool-exec-permission--bash-prompt-result
               'aborted nil nil "make test" nil nil)))
  (should (eq 'deny
              (niwa-tool-exec-permission--bash-prompt-result
               'unexpected nil nil "make test" nil nil)))
  :doc "routes scoped Bash outcomes through the canonical rule writer"
  (let ((session (niwa-session--create :name "bash-prompt")))
    (should (eq 'allow
                (niwa-tool-exec-permission--bash-prompt-result
                 'allow-session session nil "make test"
                 '("make *") nil)))
    (should (eq 'deny
                (niwa-tool-exec-permission--bash-prompt-result
                 'deny-session session nil "rm build" nil nil)))
    (should
     (equal '(("Bash" :pattern "make *" :action allow)
              ("Bash" :pattern "rm build" :action deny))
            (niwa-session-permission-rules session))))
  :doc "propagates Bash rule-write failures to the guarded permission boundary"
  (let* ((root (make-temp-file "niwa-bash-prompt-" t))
         (workspace
          (niwa-workspace--create
           :type 'project :id "bash-prompt" :root root :name "bash-prompt"))
         (file (niwa-permission-persistence-file workspace 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "invalid"))
          (should-error
            (niwa-tool-exec-permission--bash-prompt-result
             'always-allow nil workspace "make test" nil nil)))
      (delete-directory root t)))
  :doc "returns structured Bash metadata with the canonical specifier"
  (let ((result (niwa-tool-exec-permission--bash-prompt-result
                 '(deny . "reason") nil nil "make test" nil t)))
    (should (eq 'deny (plist-get result :outcome)))
    (should (equal '(deny . "reason") (plist-get result :raw-outcome)))
    (should (eq 'bash-classifier (plist-get result :via)))
    (should (eq :pattern (plist-get result :specifier-key)))
    (should (equal "make" (plist-get result :specifier-value)))))

(niwa-deftest niwa-tool-exec-permission--filesystem-paths ()
  ,test
  (test)
  :doc "a doubled leading slash in a requested grant is collapsed"
  (should (equal '((:path "/run/user/1000/gnupg/" :access read))
                 (niwa-tool-exec-permission--filesystem-paths
                  ["//run/user/1000/gnupg/"] 'read)))
  :doc "a quoted /: grant path is refused, not dropped"
  (should-error (niwa-tool-exec-permission--filesystem-paths
                 ["/:/run/user/1000"] 'read)))

(provide 'test-niwa-tool-exec-permission)

;;; test-niwa-tool-exec-permission.el ends here
