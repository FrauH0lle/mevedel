;;; test-mevedel-tool-exec-permission.el -- Tests for Bash and Eval permission adapters -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for Bash and Eval permission adapters.

;;; Code:

(require 'mevedel-tool-exec-permission)
(require 'cl-lib)
(require 'seq)
(require 'mevedel-bash-analysis)
(require 'mevedel-bash-policy)
(require 'mevedel-structs)
(require 'mevedel-execution-target)
(require 'mevedel-permission-log)
(require 'mevedel-permission-queue)
(require 'mevedel-permissions)
(require 'mevedel-telemetry)
(require 'mevedel-turn)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-tool-exec-permission-test--read-permission-log (session)
  "Read permission log entries for SESSION."
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

(mevedel-deftest mevedel-tool-exec-permission--request-permission ()
  ,test
  (test)
  :doc "uses the pipeline boundary when present"
  (let (seen)
    (mevedel-tool-exec-permission--request-permission
     (list :callback #'ignore)
     (list :permission-request
           (lambda (entry session callback)
             (setq seen (list entry session callback))))
     'session)
    (should (equal 'session (cadr seen)))
    (should (eq #'ignore (nth 2 seen))))
  :doc "falls back to direct queue admission outside the pipeline"
  (let (seen)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional session)
                 (setq seen (list entry session)))))
      (mevedel-tool-exec-permission--request-permission
       '(:kind bash) nil 'session))
    (should (equal '((:kind bash) session) seen)))
  :doc "combines pending operation and additive authority"
  (let ((cell (list nil))
        queued
        settled)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq queued entry))))
      (mevedel-tool-exec-permission--request-permission
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
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq queued entry))))
      (mevedel-tool-exec-permission--request-permission
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

(mevedel-deftest mevedel-tool-exec-permission--sandbox-request ()
  ,test
  (test)
  :doc "default request:
`mevedel-tool-exec-permission--sandbox-request' normalizes omitted authority"
  (should (equal '(:level use-default :additional-permissions nil)
                 (mevedel-tool-exec-permission--sandbox-request nil 'bash)))
  :doc "provider-shaped default request:
empty optional capabilities and an unused justification do not request authority"
  (should
   (equal
    '(:level use-default :additional-permissions nil)
    (mevedel-tool-exec-permission--sandbox-request
     '(:sandbox_permissions "use_default"
                            :additional_permissions
                            (:network :json-false :file_system (:read [] :write []))
                            :justification "Run the requested read-only workspace inspection.")
     'bash)))
  :doc "network request:
`mevedel-tool-exec-permission--sandbox-request' accepts a justified non-empty profile"
  (should
   (equal '(:level additive :additional-permissions (:network t)
                   :justification "Download dependencies?")
          (mevedel-tool-exec-permission--sandbox-request
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
      (mevedel-tool-exec-permission--sandbox-request
       `(:sandbox_permissions "with_additional_permissions"
                              :additional_permissions
                              (:file_system (:read [,read-path] :write (,write-path)))
                              :justification "Inspect and update the protected files?")
       'bash))))
  :doc "remote filesystem request:
target-native absolute and home paths qualify through the session target"
  (let* ((root (make-temp-file "mevedel-bash-remote-grant-" t))
         (native-path (file-name-concat root "resource"))
         (remote-root (format "/mevedelmock:grant:%s/" root)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("grant")
                                              (let* ((target (mevedel-execution-target-create remote-root))
                                                     (session
                                                      (mevedel-session--create
                                                       :authority-mode 'pid-lock
                                                       :execution-target target
                                                       :working-directory remote-root))
                                                     (_ (setf (mevedel-execution-target-environment target)
                                                              '(("HOME" . "/home/dev"))))
                                                     (request
                                                      (mevedel-tool-exec-permission--sandbox-request
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
      (mevedel-tool-exec-permission--sandbox-request
       `(:sandbox_permissions "with_additional_permissions"
                              :additional_permissions
                              (:file_system (:read [,path] :write [,path]))
                              :justification "Update the protected file?")
       'bash))))
  :doc "relative filesystem path:
filesystem authority requires exact absolute paths"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read ["relative/path"]))
                           :justification "Read the protected file?")
    'bash))
  :doc "empty filesystem profile:
an additive profile must contain a real capability"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:read [] :write []))
                           :justification "Allow this?")
    'bash))
  :doc "empty profile:
additive authority cannot be requested without a capability"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions nil
                           :justification "Allow this?")
    'bash))
  :doc "false network:
JSON false does not make an additive profile non-empty"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network :json-false)
                           :justification "Allow this?")
    'bash))
  :doc "missing justification:
non-default authority requires a user-facing question"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network t))
    'bash))
  :doc "default extras:
default execution rejects stray escalation arguments"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:additional_permissions (:network t)) 'bash))
  :doc "full Bash escalation:
full authority requires a justification and no additive profile"
  (should
   (equal '(:level escalated
                   :sandbox-permissions require-escalated
                   :additional-permissions nil
                   :justification "Run without confinement?")
          (mevedel-tool-exec-permission--sandbox-request
           '(:sandbox_permissions "require_escalated"
                                  :justification "  Run without confinement?  ")
           'bash)))
  :doc "provider-shaped full escalation:
an empty optional capability object does not conflict with full authority"
  (should
   (eq
    'escalated
    (plist-get
     (mevedel-tool-exec-permission--sandbox-request
      '(:sandbox_permissions "require_escalated"
                             :additional_permissions
                             (:network :json-false :file_system (:read [] :write []))
                             :justification "Run without confinement?")
      'bash)
     :level)))
  :doc "full batch Eval escalation:
batch Eval may request full authority"
  (should
   (eq 'escalated
       (plist-get
        (mevedel-tool-exec-permission--sandbox-request
         '(:sandbox_permissions "require_escalated"
                                :justification "Run batch Eval without confinement?")
         'eval 'batch)
        :level)))
  :doc "full escalation justification:
full authority rejects a missing justification"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated") 'bash))
  :doc "full escalation profile:
full authority cannot be combined with additive permissions"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated"
                           :additional_permissions (:network t)
                           :justification "Run without confinement?")
    'bash))
  :doc "full live Eval escalation:
live Eval cannot request child-confinement authority"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "require_escalated"
                           :justification "Run without confinement?")
    'eval 'live))
  :doc "live Eval:
additive child permissions are available only to batch Eval"
  (should-error
   (mevedel-tool-exec-permission--sandbox-request
    '(:sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network t)
                           :justification "Fetch package metadata?")
    'eval 'live)))

(mevedel-deftest mevedel-tool-exec-permission--additional-profile ()
  ,test
  (test)
  :doc "omits absent capabilities"
  (should-not (mevedel-tool-exec-permission--additional-profile nil nil))
  :doc "combines network and exact filesystem capabilities"
  (should
   (equal
    '(:network t
               :file-system ((:path "/tmp/input" :access read)))
    (mevedel-tool-exec-permission--additional-profile
     t '((:path "/tmp/input" :access read))))))

(mevedel-deftest mevedel-tool-exec-permission-current-context ()
  ,test
  (test)
  :doc "derives the current Bash permission facts from the active session"
  (let ((root (make-temp-file "mevedel-permission-context-" t)))
    (unwind-protect
        (let* ((workspace (mevedel-workspace--create :type 'file :root root))
               (session
                (mevedel-session--create
                 :name "context" :workspace workspace)))
          (with-temp-buffer
            (let ((context
                   (mevedel-tool-exec-permission-current-context
                    "Bash" '(:command "printf ok") session)))
              (should (eq session (plist-get context :session)))
              (should (eq workspace (plist-get context :workspace)))
              (should (eq (current-buffer) (plist-get context :buffer)))
              (should (equal "printf ok" (plist-get context :pattern))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec-permission--merge-additional-profiles ()
  ,test
  (test)
  :doc "unions capabilities and keeps the strongest exact-path access"
  (let ((path (expand-file-name "profile" temporary-file-directory)))
    (should
     (equal
      `(:network t :file-system ((:path ,path :access write)))
      (mevedel-tool-exec-permission--merge-additional-profiles
       `(:file-system ((:path ,path :access read)))
       `(:network t
                  :file-system ((:path ,path :access write)
                                (:path "relative" :access read))))))))

(mevedel-deftest mevedel-tool-exec-permission--direct-resource-grants ()
  ,test
  (test)
  :doc "returns exact grants owned directly by the session"
  (let* ((grant '(:path "/tmp/mevedel-direct" :access read))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "direct" :resource-grants (list grant))))
    (should
     (equal (list grant)
            (mevedel-tool-exec-permission--direct-resource-grants
             `(:session ,session)))))
  :doc "treats normalized resource grants as authoritative even when empty"
  (cl-letf (((symbol-function
              'mevedel-permission--load-persistent-resource-grants)
             (lambda (_workspace)
               (ert-fail "reloaded persistent resource grants"))))
    (should-not
     (mevedel-tool-exec-permission--direct-resource-grants
      '(:workspace workspace :resource-grants nil)))))

(mevedel-deftest mevedel-tool-exec-permission--remembered-additional-profile ()
  ,test
  (test)
  :doc "resolves matching direct rules whose exact grants remain sufficient"
  (let* ((path "/tmp/mevedel-remembered")
         (profile `((:path ,path :access write)))
         (rules
          `(("Bash" :pattern "make:*" :network t
             :file-system ,profile :action allow)))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "remembered" :resource-grants profile)))
    (should
     (equal
      `(:network t :file-system ,profile)
      (mevedel-tool-exec-permission--remembered-additional-profile
       "Bash" "make report"
       `(:session ,session :buckets ((:session ,@rules)))))))
  :doc "uses a portable project store as effective network and cache authority"
  (let* ((tmp-dir (make-temp-file "mevedel-profile-store-" t))
         (user-home (file-name-concat tmp-dir "home"))
         (mevedel-user-dir (file-name-concat tmp-dir "global"))
         (workspace
          (mevedel-workspace--create
           :type 'project :id "profile" :root tmp-dir :name "profile"))
         (session
          (mevedel-session--create
           :name "profile" :workspace workspace))
         (file (mevedel-permission--persistent-file workspace)))
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
                  :resource-grants ((:path "~/.npm" :access write)))
                (current-buffer)))
          (let* ((context
                  `(:session ,session :workspace ,workspace
                             :buckets
                             ((:persistent
                               ,@(mevedel-permission--load-persistent-rules
                                  workspace)))))
                 (path (expand-file-name "~/.npm")))
            (should
             (equal
              `(:network t :file-system ((:path ,path :access write)))
              (mevedel-tool-exec-permission--remembered-additional-profile
               "Bash" "npx @emacs-eask/cli test" context)))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-tool-exec-permission--apply-remembered-authority ()
  ,test
  (test)
  :doc "binds a compound command profile to the complete workload"
  (let* ((command "pwd && package fetch dependencies")
         (session (mevedel-session--create :authority-mode 'pid-lock :name "compound"))
         (request
          `(:operation-pattern ,command
                               :remember-patterns ("pwd" "package fetch:*")
                               :remember-cell ((:operation t :network t)))))
    (mevedel-tool-exec-permission--apply-remembered-authority
     'allow-session "Bash" request session nil)
    (let ((rules (mevedel-session-permission-rules session)))
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

(mevedel-deftest mevedel-tool-exec-permission-effective-sandbox-request ()
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
      (mevedel-tool-exec-permission-effective-sandbox-request
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
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:command "git status") "Bash" "git status" nil context))))
  :doc "unions remembered authority with an explicit capability delta"
  (let* ((path "/tmp/mevedel-output")
         (rules
          '(("Bash" :pattern "npx test" :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:network t :file-system ((:path ,path :access write)))
               :justification "Write the report?")
      (mevedel-tool-exec-permission-effective-sandbox-request
       `(:command "npx test"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:file_system (:write [,path]))
                  :justification "Write the report?")
       "Bash" "npx test" nil context))))
  :doc "reattaches an exact path only while its resource grant is sufficient"
  (let* ((path "/tmp/mevedel-input")
         (profile `((:path ,path :access write)))
         (rules
          `(("Bash" :pattern "make:*" :file-system ,profile
             :action allow)))
         (read-session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "read"
           :resource-grants `((:path ,path :access read))))
         (write-session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "write"
           :resource-grants `((:path ,path :access write)))))
    (should
     (equal
      '(:level use-default :additional-permissions nil)
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:session ,read-session :buckets ((:session ,@rules))))))
    (should
     (equal
      `(:level additive
               :additional-permissions (:file-system ,profile))
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:session ,write-session :buckets ((:session ,@rules)))))))
  :doc "unions matching direct profiles and keeps the strongest path access"
  (let* ((path "/tmp/mevedel-output")
         (rules
          `(("Bash" :pattern "make:*" :network t :action allow)
            ("Bash" :pattern "make:*"
             :file-system ((:path ,path :access read)) :action allow)
            ("Bash" :pattern "make:*"
             :file-system ((:path ,path :access write)) :action allow)))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "write"
           :resource-grants `((:path ,path :access write)))))
    (should
     (equal
      `(:level additive
               :additional-permissions
               (:network t :file-system ((:path ,path :access write))))
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:session ,session :buckets ((:session ,@rules)))))))
  :doc "delegated profiles and full escalation never become sticky additions"
  (let ((delegated
         '(("Bash" :pattern "make:*" :network t :action allow))))
    (should
     (equal
      '(:level use-default :additional-permissions nil)
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:command "make build") "Bash" "make build" nil
       `(:buckets ((:invocation ,@delegated))))))
    (should
     (equal
      '(:level escalated
               :sandbox-permissions require-escalated
               :additional-permissions nil
               :justification "Run directly?")
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:expression "(+ 1 2)" :mode "batch"
                     :sandbox_permissions "require_escalated"
                     :justification "Run directly?")
       "Eval" "(+ 1 2)" 'batch
       '(:buckets ((:session)))))))
  :doc "reuses an exact direct Eval profile"
  (let* ((rules
          '(("Eval" :pattern "(url-retrieve-synchronously url)"
             :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      '(:level additive :additional-permissions (:network t))
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:expression "(url-retrieve-synchronously url)" :mode "batch")
       "Eval" "(url-retrieve-synchronously url)" 'batch context))))
  :doc "does not attach a remembered child profile to live Eval"
  (let* ((rules
          '(("Eval" :pattern "(url-retrieve-synchronously url)"
             :network t :action allow)))
         (context `(:buckets ((:session ,@rules)))))
    (should
     (equal
      '(:level use-default :additional-permissions nil)
      (mevedel-tool-exec-permission-effective-sandbox-request
       '(:expression "(url-retrieve-synchronously url)" :mode "live")
       "Eval" "(url-retrieve-synchronously url)" 'live context)))))

(mevedel-deftest mevedel-tool-exec-permission--additional-authority-state ()
  ,test
  (test)
  :doc "existing read plus requested write leaves only write unresolved"
  (let* ((read '(:path "/tmp/external" :access read))
         (write '(:path "/tmp/external" :access write))
         (state
          (mevedel-tool-exec-permission--additional-authority-state
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
          (mevedel-tool-exec-permission--additional-authority-state
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
         (mevedel-tool-exec-permission--additional-authority-state
          "Bash"
          '(:additional-permissions (:network t))
          '(:mode full-auto :buckets ((:session))))))
    (should-not (plist-get state :missing))
    (should (equal '(:network t) (plist-get state :granted))))
  :doc "explicit resource deny settles before prompting"
  (let* ((write '(:path "/tmp/output" :access write))
         (state
          (mevedel-tool-exec-permission--additional-authority-state
           "Bash"
           `(:additional-permissions (:file-system (,write)))
           '(:mode full-auto
                   :buckets
                   ((:session
                     ("Bash" :path "/tmp/output" :action deny)))))))
    (should (eq 'sandbox-filesystem (plist-get state :deny-via)))))

(mevedel-deftest mevedel-tool-exec-permission--prepare-additional-authority-request ()
  ,test
  (test)
  :doc "keeps default requests unchanged"
  (let ((request '(:level use-default :additional-permissions nil)))
    (should
     (eq request
         (mevedel-tool-exec-permission--prepare-additional-authority-request
          "Bash" request nil))))
  :doc "attaches one shared approval cell to additive requests"
  (let ((request
         (mevedel-tool-exec-permission--prepare-additional-authority-request
          "Bash"
          '(:level additive :additional-permissions (:network t))
          '(:mode ask :buckets ((:session))))))
    (should (equal '(:network t)
                   (plist-get (plist-get request :authority-state)
                              :missing)))
    (should (equal '(nil) (plist-get request :approval-cell)))))

(mevedel-deftest mevedel-tool-exec-permission--log-additional-authority ()
  ,test
  (test)
  :doc "logs each capability that the invocation receives"
  (let (calls)
    (cl-letf (((symbol-function
                'mevedel-tool-exec-permission--log-permission-decision)
               (lambda (&rest args) (push args calls))))
      (mevedel-tool-exec-permission--log-additional-authority
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

(mevedel-deftest mevedel-tool-exec-permission--check-additional-permission-async ()
  ,test
  (test)
  :doc "ask Bash:
recognized command authority is followed by a once-only network prompt"
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules nil)
        entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-bash-async
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
  (let* ((root (make-temp-file "mevedel-bash-remember-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         outcome)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (should
                      (equal '(:operation t :network t)
                             (car
                              (plist-get
                               entry :remember-authority-cell))))
                     (funcall (plist-get entry :callback)
                              'allow-session))))
          (mevedel-tool-exec-permission-check-bash-async
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
            (mevedel-session-permission-rules session))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "session Bash approval:
selecting network stores qualified authority and reuses it without prompting"
  (let* ((root (make-temp-file "mevedel-bash-network-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (cl-incf prompts)
                       (setcar
                        (plist-get entry :remember-authority-cell)
                        '(:operation t :network t))
                       (funcall (plist-get entry :callback)
                                'allow-session))))
            (mevedel-tool-exec-permission-check-bash-async
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
            (mevedel-session-permission-rules session)))
          (setq outcome nil)
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (&rest _) (cl-incf prompts))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command "pwd"
                        :permission-context
                        (:session ,session :workspace ,workspace :mode ask))
             (lambda (result) (setq outcome result))))
          (should (= 1 prompts))
          (should (eq 'allow outcome)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "edits Bash:
additive network authority still prompts in edits mode"
  (let ((mevedel-permission-mode 'edits)
        (mevedel-permission-rules nil)
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq enqueued t)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-bash-async
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
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _args) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:network t)
                  :justification "Contact the service?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'allow outcome)))
  :doc "network-qualified Bash authority:
the matching command and its network request proceed without a prompt"
  (let* ((session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "network"
           :permission-mode 'ask
           :permission-rules
           '(("Bash" :pattern "npx test" :action allow)
             ("Bash" :pattern "npx test" :network t :action allow))))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
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
    (should-not enqueued)
    (should (eq 'allow outcome)))
  :doc "workspace Bash approval:
selected operation and network authority are reused by another session"
  (let* ((root (make-temp-file "mevedel-bash-workspace-network-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (first (mevedel-session-create "first" workspace))
         (second (mevedel-session-create "second" workspace))
         (mevedel--session first)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (cl-incf prompts)
                       (setcar
                        (plist-get entry :remember-authority-cell)
                        '(:operation t :network t))
                       (funcall (plist-get entry :callback)
                                'always-allow))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command "pwd"
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:network t)
                        :justification "Contact the service?"
                        :permission-context
                        (:session ,first :workspace ,workspace :mode ask))
             (lambda (result) (setq outcome result))))
          (setq outcome nil
                mevedel--session second)
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (&rest _) (cl-incf prompts))))
            (mevedel-tool-exec-permission-check-bash-async
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
      (mevedel-workspace-clear-registry)))
  :doc "sandbox off:
network authority changes no boundary and does not prompt"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "off" :sandbox-mode 'off))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _args) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:network t)
                  :justification "Contact the service?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'allow outcome)))
  :doc "full-auto protected resource:
an ungranted exact filesystem path still prompts and stores session authority"
  (let* ((root (make-temp-file "mevedel-bash-resource-" t))
         (path (file-name-concat root "secret"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (session (mevedel-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules nil)
         (mevedel-protected-paths `((,path . inaccessible)))
         entry outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (queued &optional _session)
                       (setq entry queued)
                       (setcar
                        (plist-get queued :remember-authority-cell)
                        (list :operation t
                              :file-system
                              (list (list :path path :access 'read))))
                       (funcall (plist-get queued :callback)
                                'allow-session))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command ,(format "cat %s" path)
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:file_system (:read [,path]))
                        :justification "Read the protected file?"
                        :permission-context
                        (:mode full-auto :session ,session :workspace ,workspace
                               :resource-grants nil))
             (lambda (result) (setq outcome result))))
          (should (eq 'sandbox (plist-get entry :kind)))
          (should (equal path (plist-get entry :resource-path)))
          (should (eq 'read (plist-get entry :resource-access)))
          (should (eq 'allow outcome))
          (should
           (member `(:path ,path :access read)
                   (mevedel-session-resource-grants session))))
      (delete-directory root t)))
  :doc "session resource approval:
the complete requested path profile is selected by default"
  (let* ((root (make-temp-file "mevedel-bash-resource-once-" t))
         (path (file-name-concat root "secret"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (session (mevedel-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules nil)
         (mevedel-protected-paths `((,path . inaccessible)))
         outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (should-not
                        (null
                         (plist-get
                          (car
                           (plist-get entry :remember-authority-cell))
                          :file-system)))
                       (funcall (plist-get entry :callback)
                                'allow-session))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command ,(format "cat %s" path)
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:file_system (:read [,path]))
                        :justification "Read the protected file?"
                        :permission-context
                        (:mode full-auto :session ,session :workspace ,workspace
                               :resource-grants nil))
             (lambda (result) (setq outcome result))))
          (should (eq 'allow outcome))
          (should
           (member `(:path ,path :access read)
                   (mevedel-session-resource-grants session))))
      (delete-directory root t)))
  :doc "pregranted protected resource:
an exact session grant skips only the filesystem prompt"
  (let* ((root (make-temp-file "mevedel-bash-pregrant-" t))
         (path (file-name-concat root "secret"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (grant `(:path ,path :access read))
         (session (mevedel-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto
                   :resource-grants (list grant)))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules nil)
         (mevedel-protected-paths `((,path . inaccessible)))
         enqueued outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (&rest _) (setq enqueued t))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command ,(format "cat %s" path)
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:file_system (:read [,path]))
                        :justification "Read the protected file?"
                        :permission-context
                        (:mode full-auto :session ,session :workspace ,workspace
                               :resource-grants (,grant)))
             (lambda (result) (setq outcome result))))
          (should-not enqueued)
          (should (eq 'allow outcome)))
      (delete-directory root t)))
  :doc "target replacement:
the durable Bash rule survives but its exact profile requires a fresh grant"
  (let* ((root (make-temp-file "mevedel-bash-incarnation-" t))
         (path (file-name-concat root "cache"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (profile `((:path ,path :access write)))
         (rule `("Bash" :pattern "make build"
                 :file-system ,profile :action allow))
         (session (mevedel-session-create "resource" workspace))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         (prompts 0)
         prompt-kind
         outcome)
    (setf (mevedel-session-permission-rules session) (list rule)
          (mevedel-session-resource-grants session) profile)
    (unwind-protect
        (cl-labels
            ((check ()
               (setq outcome nil)
               (mevedel-tool-exec-permission-check-bash-async
                nil
                `(:command "make build"
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:file_system (:write [,path]))
                           :justification "Write the build cache?"
                           :permission-context
                           (:mode ask :session ,session :workspace ,workspace))
                (lambda (result) (setq outcome result)))))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (cl-incf prompts)
                       (setq prompt-kind (plist-get entry :kind))
                       (funcall (plist-get entry :callback)
                                'allow-session))))
            (check)
            (should (eq 'allow outcome))
            (should (= 0 prompts))
            (mevedel-permission-invalidate-target-grants session)
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
  (let* ((root (make-temp-file "mevedel-bash-ask-grant-" t))
         (path (file-name-concat root "secret"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (grant `(:path ,path :access read))
         (session (mevedel-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto
                   :resource-grants (list grant)
                   :permission-rules
                   `(("Bash" :path ,path :action ask))))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         entry outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (queued &optional _session)
                       (setq entry queued)
                       (funcall (plist-get queued :callback) 'deny-once))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command ,(format "cat %s" path)
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:file_system (:read [,path]))
                        :justification "Read the protected file?"
                        :permission-context
                        (:mode full-auto :session ,session :workspace ,workspace
                               :session-rules (("Bash" :path ,path :action ask))
                               :resource-grants (,grant)))
             (lambda (result) (setq outcome result))))
          (should (eq 'sandbox (plist-get entry :kind)))
          (should (eq 'deny outcome)))
      (delete-directory root t)))
  :doc "resource deny:
an explicit exact path deny prevents filesystem escalation"
  (let* ((root (make-temp-file "mevedel-bash-resource-deny-" t))
         (path (file-name-concat root "secret"))
         (workspace (mevedel-workspace--create :type 'file :root root))
         (session (mevedel-session--create
                   :name "resource" :workspace workspace
                   :permission-mode 'full-auto))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules
          `(("Bash" :path ,path :action deny)))
         (mevedel-protected-paths `((,path . inaccessible)))
         enqueued outcome)
    (unwind-protect
        (progn
          (with-temp-file path (insert "secret"))
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (&rest _) (setq enqueued t))))
            (mevedel-tool-exec-permission-check-bash-async
             nil
             `(:command ,(format "cat %s" path)
                        :sandbox_permissions "with_additional_permissions"
                        :additional_permissions (:file_system (:read [,path]))
                        :justification "Read the protected file?"
                        :permission-context
                        (:mode full-auto :session ,session :workspace ,workspace))
             (lambda (result) (setq outcome result))))
          (should-not enqueued)
          (should (eq 'deny outcome)))
      (delete-directory root t)))
  :doc "explicit Bash deny:
command denial prevents network authority even in full-auto"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules '(("Bash" :pattern "pwd" :action deny)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _args) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "with_additional_permissions"
                  :additional_permissions (:network t)
                  :justification "Contact the service?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny outcome)))
  :doc "ask batch Eval:
one prompt approves both Eval and additive network authority"
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules nil)
        entries outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (push entry entries)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil
       '(:expression "(+ 1 2)"
                     :mode "batch"
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
  :doc "session batch Eval approval:
the exact expression and selected network authority are reused together"
  (let* ((root (make-temp-file "mevedel-eval-network-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         (prompts 0)
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (cl-incf prompts)
                       (setcar
                        (plist-get entry :remember-authority-cell)
                        '(:operation t :network t))
                       (funcall (plist-get entry :callback)
                                'allow-session))))
            (mevedel-tool-exec-permission-check-eval-async
             nil
             `(:expression "(+ 1 2)"
                           :mode "batch"
                           :sandbox_permissions "with_additional_permissions"
                           :additional_permissions (:network t)
                           :justification "Fetch package metadata?"
                           :permission-context
                           (:session ,session :workspace ,workspace :mode ask))
             (lambda (result) (setq outcome result))))
          (should
           (member
            '("Eval" :pattern "(+ 1 2)" :network t :action allow)
            (mevedel-session-permission-rules session)))
          (setq outcome nil)
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (&rest _) (cl-incf prompts))))
            (mevedel-tool-exec-permission-check-eval-async
             nil
             `(:expression "(+ 1 2)"
                           :mode "batch"
                           :permission-context
                           (:session ,session :workspace ,workspace :mode ask))
             (lambda (result) (setq outcome result))))
          (should (= 1 prompts))
          (should (eq 'allow outcome)))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "Bash combines multiple filesystem capabilities under one owner"
  (let* ((origin "/root/worker")
         (request (mevedel-request--create :origin origin))
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         origins
         outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (push (plist-get entry :origin) origins)
                 (with-temp-buffer
                   (funcall (plist-get entry :callback) 'allow-once)))))
      (with-temp-buffer
        (setq-local mevedel--current-request request)
        (mevedel-tool-exec-permission-check-bash-async
         nil
         '(:command "pwd"
                    :sandbox_permissions "with_additional_permissions"
                    :additional_permissions
                    (:file_system
                     (:read ["/tmp/mevedel-first" "/tmp/mevedel-second"]))
                    :justification "Inspect two requested files?")
         (lambda (result) (setq outcome result)))))
    (should (equal (list origin) (nreverse origins)))
    (should (eq 'allow outcome)))
  :doc "filesystem callback logging retains its scoped session and owner"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-scoped-permission-log-" t)))
         (origin "/root/worker")
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "main" :save-path dir :permission-mode 'ask))
         (request (mevedel-request--create
                   :session session :origin origin))
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         (mevedel-permission-log-enabled t)
         queued
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (setq queued entry))))
            (with-temp-buffer
              (setq-local mevedel--session session
                          mevedel--current-request request)
              (mevedel-tool-exec-permission-check-bash-async
               nil
               '(:command "pwd"
                          :permission-decision-metadata t
                          :sandbox_permissions "with_additional_permissions"
                          :additional_permissions
                          (:file_system (:read ["/tmp/mevedel-scoped-log"]))
                          :justification "Inspect the requested file?")
               (lambda (result) (setq outcome result)))))
          (with-temp-buffer
            (funcall (plist-get queued :callback) 'allow-once))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry
                 (cl-find
                  'sandbox-filesystem
                  (mevedel-tool-exec-permission-test--read-permission-log session)
                  :key (lambda (item) (plist-get item :via)))))
            (should (equal origin (plist-get entry :origin)))
            (should (eq 'sandbox-filesystem (plist-get entry :via)))))
      (delete-directory dir t)))
  :doc "combined Eval authority retains one scoped request owner"
  (let* ((origin "/root/worker")
         (request (mevedel-request--create :origin origin))
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         entries
         outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (push (cons (plist-get entry :kind)
                             (plist-get entry :origin))
                       entries)
                 (with-temp-buffer
                   (funcall (plist-get entry :callback) 'allow-once)))))
      (with-temp-buffer
        (setq-local mevedel--current-request request)
        (mevedel-tool-exec-permission-check-eval-async
         nil
         '(:expression "(+ 1 2)"
                       :mode "batch"
                       :sandbox_permissions "with_additional_permissions"
                       :additional_permissions (:network t)
                       :justification "Fetch package metadata?")
         (lambda (result) (setq outcome result)))))
    (should (equal (list (cons 'eval origin))
                   (nreverse entries)))
    (should (eq 'allow outcome)))
  :doc "full-auto batch Eval:
both Eval and network authority proceed without prompts"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _args) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil
       '(:expression "(+ 1 2)"
                     :mode "batch"
                     :sandbox_permissions "with_additional_permissions"
                     :additional_permissions (:network t)
                     :justification "Fetch package metadata?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'allow outcome))))

(mevedel-deftest mevedel-tool-exec-permission--check-full-escalation-async ()
  ,test
  (test)
  :doc "full-auto still asks:
full escalation prompts without a directly authored qualified rule"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should (eq 'sandbox (plist-get entry :kind)))
    (should (eq 'require-escalated
                (plist-get entry :sandbox-permissions)))
    (should (eq 'allow outcome)))
  :doc "sandbox off:
full escalation adds no boundary prompt but ordinary command authority remains"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "off" :sandbox-mode 'off))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         entries outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (push entry entries)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-bash-async
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
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules
         '(("Bash" :pattern "pwd"
            :sandbox-permissions require-escalated
            :action allow)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'allow outcome)))
  :doc "one-shot qualified allow:
an inherited escalation rule still requires one-time sandbox approval"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "one-shot-escalation"))
         (mevedel--session session)
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules
          '(("Bash" :pattern "pwd"
             :sandbox-permissions require-escalated
             :action allow)))
         entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'allow-session))))
      (mevedel-tool-exec-permission-check-bash-async
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
    (should-not (mevedel-session-permission-rules session)))
  :doc "ordinary allow is insufficient:
an unqualified command rule cannot authorize full escalation"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :pattern "pwd" :action allow)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq enqueued t)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should enqueued)
    (should (eq 'deny outcome)))
  :doc "delegated allow is insufficient:
an invocation-qualified rule cannot grant full escalation"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq enqueued t)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-bash-async
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
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :pattern "pwd" :action deny)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny outcome)))
  :doc "qualified compound deny remains final:
a denied segment cannot hide behind a broad full-escalation allow"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)
           ("Bash" :pattern "rm *"
            :sandbox-permissions require-escalated :action deny)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd && rm -rf /"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny outcome)))
  :doc "qualified nested deny remains final:
a harvested command substitution cannot hide behind a broad allow"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :sandbox-permissions require-escalated :action allow)
           ("Bash" :pattern "rm *"
            :sandbox-permissions require-escalated :action deny)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "echo $(rm -rf /)"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny outcome)))
  :doc "session approval:
the prompt stores an exact execution-level-qualified pattern rule"
  (let* ((session (mevedel-session--create :authority-mode 'pid-lock :name "full-escalation"))
         (mevedel--session session)
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil)
         outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (funcall (plist-get entry :callback) 'allow-session))))
      (mevedel-tool-exec-permission-check-bash-async
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
      (mevedel-session-permission-rules session))))
  :doc "delegated expansion:
trust-literal execution cannot use even a matching direct escalation rule"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :pattern "pwd"
            :sandbox-permissions require-escalated
            :action allow)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "pwd"
                  :trust-literal-p t
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny (car outcome))))
  :doc "reusable escalation rules:
prompts propose only literal stable Bash and Eval rules"
  (should (mevedel-tool-exec-permission--full-escalation-reusable-rule-p "Bash" "pwd"))
  (should
   (mevedel-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "rm -rf /"))
  (should-not
   (mevedel-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "echo $(pwd)"))
  (should-not
   (mevedel-tool-exec-permission--full-escalation-reusable-rule-p
    "Bash" "printf '%s' '*.tmp'"))
  (should
   (mevedel-tool-exec-permission--full-escalation-reusable-rule-p
    "Eval" "(+ 1 2)"))
  :doc "dangerous escalation prompt:
literal dangerous Bash offers reusable full-escalation authority"
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules nil)
        entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-bash-async
       nil
       '(:command "rm -rf /"
                  :sandbox_permissions "require_escalated"
                  :justification "Run without confinement?")
       (lambda (result) (setq outcome result))))
    (should (plist-get entry :include-always))
    (should (eq 'deny outcome)))
  :doc "Eval escalation prompt:
literal batch Eval offers reusable full-escalation authority"
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules nil)
        entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil
       '(:expression "(delete-file \"important\")"
                     :mode "batch"
                     :sandbox_permissions "require_escalated"
                     :justification "Run batch Eval without confinement?")
       (lambda (result) (setq outcome result))))
    (should (plist-get entry :include-always))
    (should (eq 'deny outcome)))
  :doc "batch Eval direct allow:
a broad qualified Eval rule authorizes unconfined batch execution"
  (let ((mevedel-permission-mode 'ask)
        (mevedel-permission-rules
         '(("Eval" :sandbox-permissions require-escalated :action allow)))
        enqueued outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _) (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil
       '(:expression "(+ 1 2)"
                     :mode "batch"
                     :sandbox_permissions "require_escalated"
                     :justification "Run batch Eval without confinement?")
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'allow outcome)))
  :doc "full escalation diagnostics:
the decision log identifies complete confinement bypass authority"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-full-escalation-log-" t)))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "full-escalation" :save-path dir))
         (mevedel--session session)
         (mevedel-permission-log-enabled t)
         (mevedel-permission-rules
          '(("Bash" :pattern "pwd"
             :sandbox-permissions require-escalated
             :action allow)))
         outcome)
    (unwind-protect
        (progn
          (mevedel-tool-exec-permission-check-bash-async
           nil
           `(:command "pwd"
                      :sandbox_permissions "require_escalated"
                      :justification "Run without confinement?"
                      :permission-decision-metadata t
                      :permission-context (:session ,session))
           (lambda (result) (setq outcome result)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (mevedel-tool-exec-permission-test--read-permission-log
                             session))))
            (should (eq 'sandbox-full-escalation (plist-get entry :via)))
            (should (eq 'require-escalated
                        (plist-get entry :sandbox-permissions)))))
      (delete-directory dir t))))

(mevedel-deftest mevedel-tool-exec-permission-check-bash-async ()
  ,test
  (test)
  :doc "extracts command from input and returns permission"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (mevedel-bash-dangerous-commands nil)
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "echo hello") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "outside-root resource:
default Bash requests exact additive read authority before parent traversal"
  (let* ((parent (make-temp-file "mevedel-bash-boundary-" t))
         (root (file-name-concat parent "workspace"))
         (default-directory (file-name-as-directory root))
         (mevedel-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (make-directory root)
          (mevedel-tool-exec-permission-check-bash-async
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
  (let* ((parent (make-temp-file "mevedel-bash-boundary-link-" t))
         (root (file-name-concat parent "workspace"))
         (secret (file-name-concat parent "secret"))
         (link (file-name-concat root "innocent"))
         (default-directory (file-name-as-directory root))
         (mevedel-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (make-directory root)
          (write-region "secret" nil secret nil 'silent)
          (make-symbolic-link secret link)
          (mevedel-tool-exec-permission-check-bash-async
           nil
           `(:command ,(format "cat %s" link)
                      :permission-context
                      (:mode full-auto :allowed-roots (,root) :resource-grants nil))
           (lambda (result) (setq outcome result)))
          (should (eq 'deny (car-safe outcome)))
          (should (string-match-p (regexp-quote secret) (cdr outcome))))
      (delete-directory parent t)))
  :doc "inside-root resource:
default Bash keeps bare dot inspection automatic"
  (let* ((root (make-temp-file "mevedel-bash-boundary-" t))
         (default-directory (file-name-as-directory root))
         (mevedel-permission-rules nil)
         outcome)
    (unwind-protect
        (progn
          (mevedel-tool-exec-permission-check-bash-async
           nil
           `(:command "rg -n TODO ."
                      :permission-context
                      (:mode ask :allowed-roots (,root) :resource-grants nil))
           (lambda (result) (setq outcome result)))
          (should (eq 'allow outcome)))
      (delete-directory root t)))
  :doc "returns nil when input has no command"
  (let (outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (null outcome)))
  :doc "returns deny for denied commands"
  (let ((mevedel-permission-rules
         '(("Bash" :pattern "rm*" :action deny)))
        (mevedel-bash-dangerous-commands nil)
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "rm -rf /") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "metadata mode logs sanitized Bash allow decision"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-bash-log-" t)))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "main" :save-path dir
                   :permission-mode 'ask))
         (mevedel-permission-rules
          '(("Bash" :pattern "git log:*" :action allow)))
         (mevedel-bash-dangerous-commands nil)
         (mevedel-permission-log-enabled t)
         outcome)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-exec-permission-check-bash-async
           nil '(:command "git log --oneline secret-token"
                          :permission-decision-metadata t)
           (lambda (r) (setq outcome r)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (mevedel-tool-exec-permission-test--read-permission-log
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
  (let* ((parent (mevedel-session--create :authority-mode 'pid-lock :name "parent"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "side" :audit-session parent
                   :permission-mode 'ask))
         (mevedel-permission-rules
          '(("Bash" :pattern "git log:*" :action allow)))
         (mevedel-bash-dangerous-commands nil)
         calls outcome)
    (with-temp-buffer
      (setq-local mevedel--session session)
      (cl-letf (((symbol-function 'mevedel-telemetry-record)
                 (lambda (target event &rest props)
                   (push (list target event props) calls))))
        (mevedel-tool-exec-permission-check-bash-async
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
  :doc "does not call guardian when permission resolves without prompting"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules
         '(("Bash" :pattern "echo*" :action allow)))
        (mevedel-bash-dangerous-commands nil)
        (mevedel-permission-guardian
         (lambda (_command _context _callback)
           (error "Guardian should not run")))
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "echo hello") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "interactive modes keep a guardian deny advisory"
  (dolist (mode '(ask edits))
    (let ((mevedel-permission-mode mode)
          (mevedel-permission-rules nil)
          (mevedel-bash-dangerous-commands '("sudo"))
          (mevedel-permission-guardian
           (lambda (_command _context callback)
             (funcall callback
                      '(:risk "critical"
                              :recommendation "deny"
                              :reason "Requires user judgment."))))
          outcome)
      (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                 (lambda (entry &optional _session)
                   (funcall (plist-get entry :callback) 'allow-once))))
        (mevedel-tool-exec-permission-check-bash-async
         nil '(:command "sudo ls") (lambda (result) (setq outcome result))))
      (should (eq outcome 'allow))))
  :doc "literal dangerous approval stores only the exact command"
  (let* ((workspace
          (mevedel-workspace--create
           :type 'project :id "dangerous-rule" :root temporary-file-directory
           :name "dangerous-rule" :file-cache nil))
         (session
          (mevedel-session--create
           :name "dangerous-rule" :workspace workspace
           :permission-mode 'ask))
         (mevedel-permission-rules nil)
         (mevedel-permission-guardian nil)
         ;; The resource has to sit inside the allowed root, or the exact
         ;; command never reaches the rule it is meant to store.
         (target (file-name-concat temporary-file-directory
                                   "mevedel-exact-target"))
         entry outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued &optional _session)
                 (setq entry queued)
                 (funcall (plist-get queued :callback) 'allow-session))))
      (mevedel-tool-exec-permission-check-bash-async
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
      (mevedel-session-permission-rules session))))
  :doc "full-auto allows dangerous Bash without enqueueing"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm"))
        enqueued
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "rm foo") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not enqueued))
  :doc "full-auto deny-only guardian can block suspicious Bash"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-guardian
         (lambda (_command _context callback)
           (funcall callback
                    '(:risk "critical"
                            :recommendation "deny"
                            :reason "Deletes files."))))
        enqueued
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "rm foo") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny))
    (should-not enqueued))
  :doc "request cancellation prevents a late full-auto guardian continuation"
  (let* ((request
          (mevedel-request--create
           :origin "/root"))
         (mevedel-permission-mode 'full-auto)
         (mevedel-permission-rules nil)
         (mevedel-bash-dangerous-commands '("rm"))
         guardian-callback
         outcome)
    (let ((mevedel-permission-guardian
           (lambda (_command _context callback)
             (setq guardian-callback callback))))
      (with-temp-buffer
        (setq-local mevedel--current-request request)
        (mevedel-tool-exec-permission-check-bash-async
         nil
         `(:command "rm foo"
                    :permission-context (:request ,request :mode full-auto))
         (lambda (result) (setq outcome result)))))
    (should guardian-callback)
    (should (= 1 (length (mevedel-request-cancellers request))))
    (mevedel-request-cancel request)
    (funcall guardian-callback
             '(:risk "low"
                     :recommendation "proceed"
                     :reason "Late response."))
    (should-not outcome))
  :doc "full-auto deny-only guardian timeout or invalid output allows"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-guardian
         (lambda (_command _context callback)
           (funcall callback nil)))
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "rm foo") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "full-auto deny-only guardian treats ask as advisory"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-guardian
         (lambda (_command _context callback)
           (funcall callback
                    '(:risk "high"
                            :recommendation "ask"
                            :reason "The target is ambiguous."))))
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "rm foo") (lambda (r) (setq outcome r)))
    (should (eq outcome 'allow)))
  :doc "full-auto guardian proceed, invalid output, and failure allow"
  (dolist (behavior '(proceed invalid failure))
    (let ((mevedel-permission-mode 'full-auto)
          (mevedel-permission-rules nil)
          (mevedel-bash-dangerous-commands '("rm"))
          (mevedel-permission-guardian
           (lambda (_command _context callback)
             (pcase behavior
               ('proceed
                (funcall callback
                         '(:risk "high"
                                 :recommendation "proceed"
                                 :reason "Documented cleanup.")))
               ('invalid
                (funcall callback '(:recommendation "deny")))
               ('failure
                (error "Guardian unavailable")))))
          outcome)
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "rm foo") (lambda (result) (setq outcome result)))
      (should (eq outcome 'allow))))
  :doc "full-auto deny-only guardian function timeout allows"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("rm"))
        (mevedel-permission-guardian-timeout 0.01)
        (mevedel-permission-guardian
         (lambda (_command _context _callback)
           nil))
        outcome)
    (mevedel-tool-exec-permission-check-bash-async
     nil '(:command "rm foo") (lambda (r) (setq outcome r)))
    (with-timeout (1 (error "Timed out"))
      (while (not outcome)
        (accept-process-output nil 0.01)))
    (should (eq outcome 'allow)))
  :doc "queued prompt entries preserve raw commands and add counted summary"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands nil)
        captured)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq captured entry))))
      (mevedel-tool-exec-permission-check-bash-async
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
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "prompts user and returns deny when pattern says ask and user denies"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "adds advisory guardian guidance to queued Bash prompts"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        (mevedel-permission-guardian
         (lambda (_command _context callback)
           (funcall callback
                    '(:risk "medium"
                            :recommendation "ask"
                            :reason "Uses privilege escalation."))))
        captured
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq captured entry)))
              ((symbol-function 'mevedel-permission-queue--render-head)
               (lambda (&optional _session) nil)))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (equal '(:risk medium
                           :recommendation ask
                           :reason "Uses privilege escalation.")
                   (car (plist-get captured :guardian-cell))))
    (should (eq 'done (cadr (plist-get captured :guardian-cell))))
    (funcall (plist-get captured :callback) 'deny-once)
    (should (eq outcome 'deny))
    (should (null (plist-get captured :guardian))))
  :doc "shows pending guardian guidance until a nil result marks it unavailable"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        callback
        captured
        rendered)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (setq captured entry)))
              ((symbol-function 'mevedel-permission-queue--render-head)
               (lambda (&optional _session)
                 (push (copy-sequence (plist-get captured :guardian-cell))
                       rendered))))
      (let ((mevedel-permission-guardian
             (lambda (_command _context guardian-callback)
               (setq callback guardian-callback))))
        (mevedel-tool-exec-permission-check-bash-async
         nil '(:command "sudo ls") #'ignore))
      (should (eq 'pending (cadr (plist-get captured :guardian-cell))))
      (funcall callback nil)
      (should (equal '((nil unavailable)) rendered))
      (should-not (car (plist-get captured :guardian-cell)))
      (should (eq 'unavailable
                  (cadr (plist-get captured :guardian-cell))))))
  :doc "enqueues prompts before guardian guidance completes"
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        callbacks
        enqueued
        rendered)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (push (plist-get entry :command) enqueued)))
              ((symbol-function 'mevedel-permission-queue--render-head)
               (lambda (&optional _session)
                 (push 'render rendered))))
      (let ((mevedel-permission-guardian
             (lambda (command _context callback)
               (push (cons command callback) callbacks))))
        (mevedel-tool-exec-permission-check-bash-async
         nil '(:command "sudo first") #'ignore)
        (mevedel-tool-exec-permission-check-bash-async
         nil '(:command "sudo second") #'ignore))
      (should (equal '("sudo first" "sudo second") (nreverse enqueued)))
      (funcall (cdr (assoc "sudo second" callbacks))
               '(:risk "high"
                       :recommendation "deny"
                       :reason "Second completes first."))
      (funcall (cdr (assoc "sudo first" callbacks))
               '(:risk "medium"
                       :recommendation "ask"
                       :reason "First completes later.")))
    (should (= 2 (length rendered))))
  :doc "late guardian guidance re-renders with the captured session context"
  (let* ((root (make-temp-file "mevedel-guardian-session-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (data-buffer (generate-new-buffer " *mevedel-guardian-data*"))
         (source-buffer (generate-new-buffer " *mevedel-guardian-source*"))
         (mevedel-permission-rules nil)
         (mevedel-bash-dangerous-commands '("sudo"))
         guardian-callback
         enqueue-session
         enqueue-buffer
         render-session
         render-buffer
         captured
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional session-arg)
                       (setq enqueue-session session-arg)
                       (setq enqueue-buffer (current-buffer))
                       (setq captured entry)))
                    ((symbol-function 'mevedel-permission-queue--render-head)
                     (lambda (&optional session-arg)
                       (setq render-session session-arg)
                       (setq render-buffer (current-buffer)))))
            (with-current-buffer data-buffer
              (setq-local mevedel--session session))
            (with-current-buffer source-buffer
              (setq-local mevedel--data-buffer data-buffer)
              (let ((mevedel-permission-guardian
                     (lambda (_command _context callback)
                       (setq guardian-callback callback))))
                (mevedel-tool-exec-permission-check-bash-async
                 nil '(:command "sudo ls") (lambda (r) (setq outcome r)))))
            (should guardian-callback)
            (let ((mevedel--session nil))
              (funcall guardian-callback
                       '(:risk "medium"
                               :recommendation "ask"
                               :reason "Uses privilege escalation."))))
          (should (eq enqueue-session session))
          (should (eq enqueue-buffer source-buffer))
          (should (eq render-session session))
          (should (eq render-buffer source-buffer))
          (funcall (plist-get captured :callback) 'deny-once)
          (should (eq outcome 'deny)))
      (when (buffer-live-p data-buffer)
        (kill-buffer data-buffer))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "settled prompts ignore late guardian guidance"
  (let* ((root (make-temp-file "mevedel-guardian-cancel-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (source-buffer (generate-new-buffer " *mevedel-guardian-cancel*"))
         (mevedel-permission-rules nil)
         (mevedel-bash-dangerous-commands '("sudo"))
         guardian-callback
         captured
         rendered
         outcome)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                     (lambda (entry &optional _session)
                       (setq captured entry)))
                    ((symbol-function 'mevedel-permission-queue--render-head)
                     (lambda (&optional _session)
                       (setq rendered t))))
            (with-current-buffer source-buffer
              (let ((mevedel--session session)
                    (mevedel-permission-guardian
                     (lambda (_command _context callback)
                       (setq guardian-callback callback))))
                (mevedel-tool-exec-permission-check-bash-async
                 nil '(:command "sudo ls") (lambda (r) (setq outcome r)))))
            (should guardian-callback)
            (funcall (plist-get captured :callback) 'aborted)
            (should (eq outcome 'aborted))
            (funcall guardian-callback
                     '(:risk "low"
                             :recommendation "proceed"
                             :reason "Late read-only guidance.")))
          (should-not rendered)
          (should-not (car (plist-get captured :guardian-cell))))
      (when (buffer-live-p source-buffer)
        (kill-buffer source-buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  ;; Feedback is part of the authoritative queued prompt vocabulary.
  ;; Mock the queue entry point and deliver it directly to the
  ;; adapter's callback.
  (let ((mevedel-permission-rules nil)
        (mevedel-bash-dangerous-commands '("sudo"))
        outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry &optional _session)
                 (funcall (plist-get entry :callback)
                          '(feedback . "use git instead")))))
      (mevedel-tool-exec-permission-check-bash-async
       nil '(:command "sudo ls") (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Command cancelled by user. Feedback: use git instead"
                   (cdr outcome))))
  :doc "allow-session stores the suggested reusable Bash prefix pattern"
  (let* ((root (make-temp-file "mevedel-bash-rules-" t))
         (workspace (mevedel-workspace-get-or-create
                     'file root root "test"))
         (session (mevedel-session-create "main" workspace))
         (mevedel--session session)
         (mevedel-permission-rules nil)
         (mevedel-bash-dangerous-commands nil)
         outcome)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-permission--enqueue)
                   (lambda (entry &optional _session)
                     (funcall (plist-get entry :callback)
                              'allow-session))))
          (mevedel-tool-exec-permission-check-bash-async
           nil '(:command "git describe --tags")
           (lambda (r) (setq outcome r)))
          (should (eq outcome 'allow))
          (should (member '("Bash" :pattern "git describe:*" :action allow)
                          (mevedel-session-permission-rules session))))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-tool-exec-permission--normalize-prompt-result ()
  ,test
  (test)
  :doc "normalizes stored and feedback prompt outcomes"
  (let (stored)
    (should
     (eq 'allow
         (mevedel-tool-exec-permission--normalize-prompt-result
          'allow-session
          (lambda (outcome) (setq stored outcome) 'allow)
          "Feedback: ")))
    (should (eq stored 'allow-session))
    (should
     (equal '(deny . "Feedback: no")
            (mevedel-tool-exec-permission--normalize-prompt-result
             '(feedback . "no") #'ignore "Feedback: ")))))

(mevedel-deftest mevedel-tool-exec-permission--eval-prompt-result ()
  ,test
  (test)
  :doc "normalizes direct, feedback, abort, and unknown Eval outcomes"
  (should (eq 'allow
              (mevedel-tool-exec-permission--eval-prompt-result
               'allow-once nil nil "(+ 1 2)" nil)))
  (should (eq 'deny
              (mevedel-tool-exec-permission--eval-prompt-result
               'deny-once nil nil "(+ 1 2)" nil)))
  (should (equal '(deny . "Eval cancelled by user. Feedback: no")
                 (mevedel-tool-exec-permission--eval-prompt-result
                  '(feedback . "no") nil nil "(+ 1 2)" nil)))
  (should (eq 'aborted
              (mevedel-tool-exec-permission--eval-prompt-result
               'aborted nil nil "(+ 1 2)" nil)))
  (should (eq 'deny
              (mevedel-tool-exec-permission--eval-prompt-result
               'unexpected nil nil "(+ 1 2)" nil)))
  :doc "applies stored Eval outcomes through the canonical rule writer"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "eval-prompt")))
    (should (eq 'allow
                (mevedel-tool-exec-permission--eval-prompt-result
                 'allow-session session nil "(+ 1 2)" nil)))
    (should (eq 'deny
                (mevedel-tool-exec-permission--eval-prompt-result
                 'deny-session session nil "(delete-file x)" nil)))
    (should
     (equal
      '(("Eval" :pattern "(+ 1 2)" :action allow)
        ("Eval" :pattern "(delete-file x)" :action deny))
      (mevedel-session-permission-rules session))))
  :doc "persists always-allow Eval rules to real workspace storage"
  (let* ((root (make-temp-file "mevedel-eval-prompt-" t))
         (workspace
          (mevedel-workspace--create
           :type 'project :id "eval-prompt" :root root :name "eval-prompt"))
         (session
          (mevedel-session--create
           :name "eval-prompt" :workspace workspace)))
    (unwind-protect
        (progn
          (should
           (eq 'allow
               (mevedel-tool-exec-permission--eval-prompt-result
                'always-allow session workspace "(message x)" nil)))
          (should
           (member '("Eval" :pattern "(message x)" :action allow)
                   (mevedel-permission--load-persistent-rules workspace))))
      (delete-directory root t)))
  :doc "returns structured Eval metadata when requested"
  (let ((result (mevedel-tool-exec-permission--eval-prompt-result
                 '(deny . "reason") nil nil "(+ 1 2)" t)))
    (should (eq 'deny (plist-get result :outcome)))
    (should (equal '(deny . "reason") (plist-get result :raw-outcome)))
    (should (eq 'eval-policy (plist-get result :via)))
    (should (plist-get result :logged))))

(mevedel-deftest mevedel-tool-exec-permission-check-eval-async ()
  ,test
  (test)
  :doc "returns deny when input has no expression"
  (let (outcome)
    (mevedel-tool-exec-permission-check-eval-async
     nil '(:other "value") (lambda (r) (setq outcome r)))
    (should (eq outcome 'deny)))
  :doc "metadata mode logs Eval allow without expression payload"
  (let* ((dir (file-name-as-directory
               (make-temp-file "mevedel-eval-log-" t)))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "main" :save-path dir
                   :permission-mode 'full-auto))
         (mevedel-permission-rules nil)
         (mevedel-permission-log-enabled t)
         outcome)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-exec-permission-check-eval-async
           nil '(:expression "(delete-file \"secret\")"
                             :permission-decision-metadata t)
           (lambda (r) (setq outcome r)))
          (should (eq 'allow (plist-get outcome :outcome)))
          (let ((entry (car (mevedel-tool-exec-permission-test--read-permission-log
                             session))))
            (should (eq 'permission-decision (plist-get entry :event)))
            (should (equal "Eval" (plist-get entry :tool-name)))
            (should (eq 'allow (plist-get entry :outcome)))
            (should (eq 'eval-policy (plist-get entry :via)))
            (should-not (plist-member entry :expression))))
      (delete-directory dir t)))
  :doc "returns allow when user approves"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow)))
  :doc "enqueues requested mode and preserve_ui metadata"
  (let (entry)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (queued)
                 (setq entry queued))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(delete-other-windows)"
                         :mode "live"
                         :preserve_ui :json-false)
       #'ignore))
    (should (equal "live" (plist-get entry :mode)))
    (should-not (plist-get entry :preserve-ui)))
  :doc "returns deny reason for invalid mode instead of signaling"
  (let (outcome)
    (mevedel-tool-exec-permission-check-eval-async
     nil '(:expression "(+ 1 2)" :mode "bogus")
     (lambda (r) (setq outcome r)))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (string-match-p "Unknown Eval mode" (cdr outcome))))
  :doc "refuses remote Batch Eval before prompting"
  (let* ((target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "remote-eval"
                   :execution-target target
                   :working-directory "/ssh:user@host:/srv/project/"))
         outcome
         enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil
       `(:expression "(+ 1 2)"
                     :mode "batch"
                     :permission-context (:session ,session))
       (lambda (result) (setq outcome result))))
    (should-not enqueued)
    (should (eq 'deny (car outcome)))
    (should
     (string-match-p "Batch Eval is unavailable for remote sessions"
                     (cdr outcome))))
  :doc "returns deny when user denies"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))
  :doc "feedback maps to (deny . REASON) with the historical message"
  (let (outcome)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (funcall (plist-get entry :callback)
                          '(feedback . "too dangerous")))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(delete-file \"/etc/passwd\")")
       (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should (equal "Eval cancelled by user. Feedback: too dangerous"
                   (cdr outcome))))
  :doc "trusted Eval with active allow bypasses prompt"
  (let ((mevedel-permission-rules '(("Eval" :action allow)))
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)" :trust-literal-p t)
       (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not enqueued))

  :doc "full-auto Eval bypasses prompt without active allow"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules nil)
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)")
       (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not enqueued))

  :doc "explicit context Eval uses context mode without ambient session"
  (let* ((session (mevedel-session--create
                   :authority-mode 'pid-lock
                   :name "test" :permission-mode 'full-auto))
         (context (mevedel-permission--invocation-context
                   :tool-name "Eval"
                   :session session))
         (mevedel-permission-mode 'ask)
         (mevedel-permission-rules nil))
    (should (eq 'allow
                (mevedel-tool-exec-permission--check-eval-permission
                 :permission-context context))))

  :doc "explicit Eval deny beats full-auto"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules '(("Eval" :action deny)))
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)")
       (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny))
    (should-not enqueued))

  :doc "trusted Eval without active allow denies without prompting"
  (let ((mevedel-permission-rules nil)
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (&rest _)
                 (setq enqueued t))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)" :trust-literal-p t)
       (lambda (r) (setq outcome r))))
    (should (consp outcome))
    (should (eq 'deny (car outcome)))
    (should-not enqueued))

  :doc "explicit Eval deny beats trusted allow"
  (let* ((ws (mevedel-workspace--create
              :type 'file :id "eval-deny" :root "/tmp/eval-deny"
              :name "eval-deny"
              :file-cache (mevedel-file-cache--create
                           :table (make-hash-table :test #'equal)
                           :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" ws))
         (request (mevedel-request--create
                   :session session
                   :skill-permission-rules '(("Eval" :action allow))))
         (mevedel-permission-rules nil)
         outcome)
    (setf (mevedel-session-permission-rules session)
          '(("Eval" :action deny)))
    (with-temp-buffer
      (setq-local mevedel--session session)
      (setq-local mevedel--current-request request)
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)" :trust-literal-p t)
       (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny)))

  :doc "direct Eval allow authorizes model-generated Eval without prompting"
  (let ((mevedel-permission-rules '(("Eval" :action allow)))
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (setq enqueued t)
                 (funcall (plist-get entry :callback) 'allow-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'allow))
    (should-not enqueued))

  :doc "explicit Eval ask prompts even in full-auto"
  (let ((mevedel-permission-mode 'full-auto)
        (mevedel-permission-rules '(("Eval" :action ask)))
        outcome enqueued)
    (cl-letf (((symbol-function 'mevedel-permission--enqueue)
               (lambda (entry)
                 (setq enqueued t)
                 (funcall (plist-get entry :callback) 'deny-once))))
      (mevedel-tool-exec-permission-check-eval-async
       nil '(:expression "(+ 1 2)") (lambda (r) (setq outcome r))))
    (should (eq outcome 'deny))
    (should enqueued))
  )

(mevedel-deftest mevedel-tool-exec-permission--bash-prompt-result ()
  ,test
  (test)
  :doc "normalizes direct, feedback, abort, and unknown Bash outcomes"
  (should (eq 'allow
              (mevedel-tool-exec-permission--bash-prompt-result
               'allow nil nil "make test" nil nil)))
  (should (eq 'deny
              (mevedel-tool-exec-permission--bash-prompt-result
               'deny nil nil "make test" nil nil)))
  (should (equal '(deny . "Command cancelled by user. Feedback: no")
                 (mevedel-tool-exec-permission--bash-prompt-result
                  '(feedback . "no") nil nil "make test" nil nil)))
  (should (eq 'aborted
              (mevedel-tool-exec-permission--bash-prompt-result
               'aborted nil nil "make test" nil nil)))
  (should (eq 'deny
              (mevedel-tool-exec-permission--bash-prompt-result
               'unexpected nil nil "make test" nil nil)))
  :doc "routes scoped Bash outcomes through the canonical rule writer"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "bash-prompt")))
    (should (eq 'allow
                (mevedel-tool-exec-permission--bash-prompt-result
                 'allow-session session nil "make test"
                 '("make *") nil)))
    (should (eq 'deny
                (mevedel-tool-exec-permission--bash-prompt-result
                 'deny-session session nil "rm build" nil nil)))
    (should
     (equal '(("Bash" :pattern "make *" :action allow)
              ("Bash" :pattern "rm build" :action deny))
            (mevedel-session-permission-rules session))))
  :doc "contains Bash rule-write failures in the permission result"
  (let* ((root (make-temp-file "mevedel-bash-prompt-" t))
         (workspace
          (mevedel-workspace--create
           :type 'project :id "bash-prompt" :root root :name "bash-prompt"))
         (file (mevedel-permission--persistent-file workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "invalid"))
          (should
           (string-prefix-p
            "Error: Bash rule write failed:"
            (mevedel-tool-exec-permission--bash-prompt-result
             'always-allow nil workspace "make test" nil nil))))
      (delete-directory root t)))
  :doc "returns structured Bash metadata with the canonical specifier"
  (let ((result (mevedel-tool-exec-permission--bash-prompt-result
                 '(deny . "reason") nil nil "make test" nil t)))
    (should (eq 'deny (plist-get result :outcome)))
    (should (equal '(deny . "reason") (plist-get result :raw-outcome)))
    (should (eq 'bash-classifier (plist-get result :via)))
    (should (eq :pattern (plist-get result :specifier-key)))
    (should (equal "make" (plist-get result :specifier-value)))))

(mevedel-deftest mevedel-tool-exec-permission-prompt-eval ()
  ,test
  (test)
  :doc "renders requested live mode and preserve_ui value"
  (let (content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-eval)
               (lambda (body _callback &rest _)
                 (setq content body))))
      (mevedel-tool-exec-permission-prompt-eval
       "(delete-other-windows)" #'ignore nil nil nil "live" nil))
    (should
     (string-match-p
      "Mode: live (inherently unconfined; preserve_ui: false)"
      content)))
  :doc "renders requested batch mode"
  (let (content)
    (cl-letf (((symbol-function 'mevedel-permission--prompt-async-eval)
               (lambda (body _callback &rest _)
                 (setq content body))))
      (mevedel-tool-exec-permission-prompt-eval
       "(+ 1 2)" #'ignore nil nil nil "batch" t))
    (should (string-match-p "Mode: batch" content))
    (should-not (string-match-p "preserve_ui" content))))

(provide 'test-mevedel-tool-exec-permission)

;;; test-mevedel-tool-exec-permission.el ends here
