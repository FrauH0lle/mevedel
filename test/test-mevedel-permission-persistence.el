;;; test-mevedel-permission-persistence.el -- Permission store tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests portable permission authority encoding, target-aware store reads,
;; validation, and atomic store replacement.

;;; Code:

(require 'mevedel-execution-target)
(require 'mevedel-permission-persistence)
(require 'mevedel-permissions)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Persistent rule storage

(mevedel-deftest mevedel-permission-serialize-authority ()
  ,test
  (test)
  :doc "stores remote rule and grant paths in the target-native domain"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should
     (equal
      '(:rules
        (("Read" :path "/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system ((:path "/var/cache/build" :access write))
          :action allow))
        :resource-grants ((:path "/srv/private/key" :access read)))
      (mevedel-permission-serialize-authority
       '(("Read" :path "/ssh:user@alias-a:/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system
          ((:path "/ssh:user@alias-a:/var/cache/build" :access write))
          :action allow))
       '((:path "/ssh:user@alias-a:/srv/private/key" :access read))
       target))))

  :doc "rejects authority naming another execution target"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should-not
     (mevedel-permission-serialize-authority
      '(("Read" :path "/ssh:user@alias-b:/srv/private/**" :action allow))
      nil target)))

  :doc "does not reinterpret a client absolute path as remote authority"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should-not
     (mevedel-permission-serialize-authority
      nil '((:path "/home/client/.ssh/id" :access read)) target)))

  :doc "resolves remote home syntax before persistence"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/dev")))
    (should
     (equal
      '(:rules
        (("Bash" :pattern "npm test"
          :file-system ((:path "/home/dev/.npm" :access write))
          :action allow))
        :resource-grants ((:path "/home/dev/.npm" :access write)))
      (mevedel-permission-serialize-authority
       '(("Bash" :pattern "npm test"
          :file-system ((:path "~/.npm" :access write))
          :action allow))
       '((:path "~/.npm" :access write)) target)))))

(mevedel-deftest mevedel-permission-deserialize-authority ()
  ,test
  (test)
  :doc "requalifies target-native authority through the current alias"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@alias-b:/srv/project/")))
    (should
     (equal
      '(:rules
        (("Read" :path "/ssh:user@alias-b:/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system
          ((:path "/ssh:user@alias-b:/var/cache/build" :access write))
          :action allow))
        :resource-grants
        ((:path "/ssh:user@alias-b:/srv/private/key" :access read)))
      (mevedel-permission-deserialize-authority
       '(("Read" :path "/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system ((:path "/var/cache/build" :access write))
          :action allow))
       '((:path "/srv/private/key" :access read)) target))))

  :doc "expands home-relative authority from the target environment"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/dev")))
    (should
     (equal
      '(:rules
        (("Bash" :pattern "npm test"
          :file-system ((:path "/docker:dev:/home/dev/.npm" :access write))
          :action allow))
        :resource-grants
        ((:path "/docker:dev:/home/dev/.npm" :access write)))
      (mevedel-permission-deserialize-authority
       '(("Bash" :pattern "npm test"
          :file-system ((:path "~/.npm" :access write))
          :action allow))
       '((:path "~/.npm" :access write)) target))))

  :doc "rejects durable authority containing a client-specific TRAMP prefix"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@alias-b:/srv/project/")))
    (should-not
     (mevedel-permission-deserialize-authority
      nil '((:path "/ssh:user@alias-a:/srv/private/key" :access read))
      target)))

  :doc "round-trips ordinary absolute paths for a local target"
  (let ((target (mevedel-execution-target-create "/srv/project/")))
    (should
     (equal
      '(:rules (("Read" :path "/srv/private/**" :action allow))
        :resource-grants ((:path "/srv/private/key" :access read)))
      (mevedel-permission-deserialize-authority
       '(("Read" :path "/srv/private/**" :action allow))
       '((:path "/srv/private/key" :access read)) target)))))

(mevedel-deftest mevedel-permission-persistence-save-rule ()
  ,test
  (test)
  :doc "round-trip save and load"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-rule ws "Read" 'allow)
          (mevedel-permission-persistence-save-rule ws "Edit" 'deny "/secret/*")
          (let ((rules (mevedel-permission-persistence-load-rules ws)))
            (should (= (length rules) 2))
            (should (equal (car rules) '("Read" :action allow)))
            (should (equal (cadr rules)
                           '("Edit" :path "/secret/*" :action deny)))))
      (delete-directory tmp-dir t)))
  :doc "load returns nil for missing file"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (should-not (mevedel-permission-persistence-load-rules ws))
      (delete-directory tmp-dir t)))
  :doc "round-trips an execution-level-qualified persistent rule"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-rule
           ws "Eval" 'allow nil
           :spec-key :pattern :spec-value "(message *)"
           :sandbox-permissions 'require-escalated)
          (should
           (equal
            '(("Eval" :pattern "(message *)"
                      :sandbox-permissions require-escalated
                      :action allow))
            (mevedel-permission-persistence-load-rules ws))))
      (delete-directory tmp-dir t)))
  :doc "merges global and project rules, project rules last"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (global-dir (file-name-concat tmp-dir "global/"))
         (project-dir (file-name-concat tmp-dir "project/"))
         (mevedel-user-dir global-dir)
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root project-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          ;; Write global rules
          (make-directory global-dir t)
          (with-temp-file (file-name-concat global-dir "permissions.el")
            (pp '(:rules (("Read" :action allow)
                          ("*" :path "/tmp/*" :action allow))
                  :resource-grants nil)
                (current-buffer)))
          ;; Write project rules
          (mevedel-permission-persistence-save-rule ws "Edit" 'allow "~/proj/*")
          (let ((rules (mevedel-permission-persistence-load-rules ws)))
            ;; Global rules first, then project
            (should (= (length rules) 3))
            (should (equal (nth 0 rules) '("Read" :action allow)))
            (should (equal (nth 1 rules) '("*" :path "/tmp/*" :action allow)))
            (should (equal (nth 2 rules) '("Edit" :path "~/proj/*" :action allow)))))
      (delete-directory tmp-dir t)))
  :doc "rebinds project paths through the live target but keeps global paths local"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (global-dir (file-name-concat tmp-dir "global/"))
         (project-dir (file-name-concat tmp-dir "project/"))
         (mevedel-user-dir global-dir))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("alias-a" "alias-b")
          (make-directory (file-name-concat project-dir ".mevedel") t)
          (make-directory global-dir t)
          (with-temp-file (file-name-concat global-dir "permissions.el")
            (pp '(:rules (("Read" :path "/home/client/private/**"
                           :action allow))
                  :resource-grants nil)
                (current-buffer)))
          (with-temp-file
              (file-name-concat project-dir ".mevedel/permissions.el")
            (pp '(:rules (("Read" :path "/srv/shared/**" :action allow))
                  :resource-grants nil)
                (current-buffer)))
          (let* ((root (format "/mevedelmock:alias-b:%s/" project-dir))
                 (workspace (mevedel-workspace--create
                             :type 'project :id "test" :root root
                             :name "test" :file-cache nil))
                 (target (mevedel-execution-target-create root))
                 (session (mevedel-session--create
                           :name "test" :workspace workspace
                           :execution-target target)))
            (setf (mevedel-execution-target-environment target)
                  '(("HOME" . "/home/dev")))
            (with-temp-buffer
              (setq-local mevedel--session session)
              (should
               (equal
                `(("Read" :path "/home/client/private/**" :action allow)
                  ("Read" :path
                   ,(format "/mevedelmock:alias-b:/srv/shared/**")
                   :action allow))
                (mevedel-permission-persistence-load-rules workspace))))))
      (delete-directory tmp-dir t)))
  :doc "deduplicates exact persistent rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission-persistence-save-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (let ((rules (mevedel-permission-persistence-load-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow))))))
      (delete-directory tmp-dir t)))
  :doc "preserves distinct persistent rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission-persistence-save-rule
           ws "Bash" 'deny nil
           :spec-key :pattern :spec-value "git diff:*")
          (mevedel-permission-persistence-save-rule
           ws "Bash" 'allow nil
           :spec-key :pattern :spec-value "git status:*")
          (let ((rules (mevedel-permission-persistence-load-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow)
                             ("Bash" :pattern "git diff:*"
                              :action deny)
                             ("Bash" :pattern "git status:*"
                              :action allow))))))
      (delete-directory tmp-dir t)))
  :doc "refuses to overwrite an invalid persistent store"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission-persistence-file ws)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "(:rules malformed :resource-grants nil)"))
          (should-error
           (mevedel-permission-persistence-save-rule ws "Read" 'allow)
           :type 'user-error)
          (should (equal "(:rules malformed :resource-grants nil)"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (delete-directory tmp-dir t)))
  :doc "refuses a permission store symlink without changing its target"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (outside (file-name-concat tmp-dir "outside.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission-persistence-file ws)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (write-region "(:rules nil :resource-grants nil)\n"
                        nil outside nil 'silent)
          (make-symbolic-link outside file)
          (should-error
           (mevedel-permission-persistence-save-rule ws "Read" 'allow))
          (should (file-symlink-p file))
          (should (equal "(:rules nil :resource-grants nil)\n"
                         (with-temp-buffer
                           (insert-file-contents outside)
                           (buffer-string)))))
      (delete-directory tmp-dir t)))
  :doc "refuses a symlinked workspace state directory"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (outside-dir (make-temp-file "mevedel-permissions-outside-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (state-dir (file-name-concat tmp-dir ".mevedel"))
         (outside-file (file-name-concat outside-dir "permissions.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (make-symbolic-link outside-dir state-dir)
          (should-error
           (mevedel-permission-persistence-save-rule ws "Read" 'allow))
          (should-not (file-exists-p outside-file)))
      (when (file-symlink-p state-dir)
        (delete-file state-dir))
      (delete-directory tmp-dir t)
      (delete-directory outside-dir t))))

(mevedel-deftest mevedel-permission-remove-persistent-resource-grant ()
  ,test
  (test)
  :doc "revokes one persistent grant without changing command rules"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-rule ws "Read" 'allow)
          (mevedel-permission-persistence-save-resource-grant ws path 'read)
          (should (eq 'allow
                      (mevedel-check-permission
                       "Read"
                       :tool-struct (mevedel-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (mevedel-permission-persistence-load-resource-grants
                        ws))))
          (mevedel-permission-remove-persistent-resource-grant ws path 'read)
          (should-not
           (mevedel-permission-persistence-load-resource-grants ws))
          (should (equal '(("Read" :action allow))
                         (mevedel-permission-persistence-load-rules ws)))
          (should (eq 'ask
                      (mevedel-check-permission
                       "Read"
                       :tool-struct (mevedel-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (mevedel-permission-persistence-load-resource-grants
                        ws)))))
      (delete-directory tmp-dir t))))


(mevedel-deftest mevedel-permission-persistence-load-resource-grants ()
  ,test
  (test)
  :doc "an invalid grant makes the editable store fail closed"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission-persistence-file ws)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (pp '(:rules nil
                  :resource-grants
                  ((:path "/tmp/read" :access read)
                   (:path "relative" :access write)
                   (:path "/tmp/execute" :access execute)
                   malformed))
                (current-buffer)))
          (should-not
           (mevedel-permission-persistence-load-resource-grants ws)))
      (delete-directory tmp-dir t)))
  :doc "expands home-relative exact grants in rules and authority"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (user-home (file-name-concat tmp-dir "home"))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission-persistence-file ws)))
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
          (let ((path (expand-file-name "~/.npm")))
            (should
             (equal
              `(("Bash" :pattern "npx @emacs-eask/cli *"
                 :network t
                 :file-system ((:path ,path :access write))
                 :action allow))
              (mevedel-permission-persistence-load-rules ws)))
            (should
             (equal `((:path ,path :access write))
                    (mevedel-permission-persistence-load-resource-grants
                     ws)))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-permission-validate-persistent-stores ()
  ,test
  (test)
  :doc "warns once per invalid file version and stays quiet for missing stores"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (mevedel-user-dir (file-name-concat tmp-dir "global/"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (mevedel-permission-persistence-file ws))
         (mevedel--warn-once-table (make-hash-table :test #'equal))
         warnings)
    (unwind-protect
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (&rest args) (push args warnings))))
          (mevedel-permission-validate-persistent-stores ws)
          (should-not warnings)
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "(:rules malformed :resource-grants nil)"))
          (mevedel-permission-validate-persistent-stores ws)
          (mevedel-permission-validate-persistent-stores ws)
          (should (= 1 (length warnings)))
          (with-temp-file file
            (insert "(:rules nil :resource-grants malformed)"))
          (set-file-times file (time-add (current-time) 1))
          (mevedel-permission-validate-persistent-stores ws)
          (should (= 2 (length warnings)))
          (should (string-match-p
                   "(:rules (\\.\\.\\.) :resource-grants (\\.\\.\\.))"
                   (cadar warnings))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-permission-persistence-refresh ()
  ,test
  (test)
  :doc "waits for an idle remote transport before refreshing authority"
  (let ((workspace (mevedel-workspace--create
                    :type 'project :id "remote"
                    :root "/mevedelmock:host:/srv/project/"
                    :name "remote"))
        deferred refreshed continued)
    (cl-letf (((symbol-function 'mevedel-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'mevedel-transport-run-when-idle)
               (lambda (_key _path thunk &optional _on-cancel)
                 (setq deferred thunk)
                 t))
              ((symbol-function 'mevedel-permission-validate-persistent-stores)
               (lambda (_) (setq refreshed t))))
      (mevedel-permission-persistence-refresh
       workspace (lambda () (setq continued t)))
      (should-not refreshed)
      (should-not continued)
      (funcall deferred)
      (should refreshed)
      (should continued)))

  :doc "fails closed when deferred refresh cannot be queued"
  (let ((workspace (mevedel-workspace--create
                    :type 'project :id "remote"
                    :root "/mevedelmock:host:/srv/project/"
                    :name "remote"))
        continued cancelled)
    (cl-letf (((symbol-function 'mevedel-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'mevedel-transport-run-when-idle)
               (lambda (&rest _) nil)))
      (should-not
       (mevedel-permission-persistence-refresh
        workspace
        (lambda () (setq continued t))
        (lambda () (setq cancelled t))))
      (should-not continued)
      (should cancelled))))

(mevedel-deftest mevedel-permission-persistence-save-resource-grant ()
  ,test
  (test)
  :doc "write authority promotes a persisted exact read grant"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (mevedel-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (mevedel-permission-persistence-save-resource-grant
           ws path 'read)
          (mevedel-permission-persistence-save-resource-grant
           ws path 'write)
          (should
           (equal `((:path ,path :access write))
                  (mevedel-permission-persistence-load-resource-grants ws))))
      (delete-directory tmp-dir t)))

  :doc "writes target-native paths and reloads them through the live alias"
  (let* ((tmp-dir (make-temp-file "mevedel-test-" t))
         (project-dir (file-name-concat tmp-dir "project"))
         (root (format "/mevedelmock:alias-a:%s/" project-dir))
         (workspace (mevedel-workspace--create
                     :type 'project :id "test" :root root
                     :name "test" :file-cache nil))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         (qualified "/mevedelmock:alias-a:/srv/shared/key")
         (local-file (file-name-concat project-dir
                                      ".mevedel/permissions.el"))
         target session)
    (unwind-protect
        ;; The target records its support tier at construction, so it must be
        ;; built while the mock method is supported.
        (mevedel-test--with-local-shell-tramp '("alias-a")
          (setq target (mevedel-execution-target-create root)
                session (mevedel-session--create
                         :name "test" :workspace workspace
                         :execution-target target
                         :save-path (file-name-concat
                                     root ".mevedel/sessions/test")))
          (make-directory (mevedel-session-save-path session) t)
          (puthash (mevedel-execution-target-identity target) t
                   mevedel-session-durability--disclosed-targets)
          (should
           (mevedel-session-durability-lease-acquire
            (mevedel-session-save-path session) "test" session))
          (unwind-protect
              (with-temp-buffer
                (setq-local mevedel--session session)
                (mevedel-permission-persistence-save-resource-grant
                 workspace qualified 'read)
                (should
                 (equal `((:path ,qualified :access read))
                        (mevedel-permission-persistence-load-resource-grants
                         workspace))))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session))
          (let ((raw (with-temp-buffer
                       (insert-file-contents local-file)
                       (read (current-buffer)))))
            (should
             (equal '((:path "/srv/shared/key" :access read))
                    (plist-get raw :resource-grants)))
            (should-not (string-match-p
                         "mevedelmock"
                         (with-temp-buffer
                           (insert-file-contents local-file)
                           (buffer-string))))))
      (delete-directory tmp-dir t))))

(mevedel-deftest mevedel-permission--store-file-status ()
  ,test
  (test)

  :doc "a repeat read is served from memory, never from the target"
  ;; Consulted twice per tool call, from inside gptel's curl sentinel that
  ;; TRAMP's wait loop re-runs; a read there lands on a connection that
  ;; already has a command in flight.
  (let* ((dir (make-temp-file "mevedel-store-cache-" t))
         (file (file-name-concat dir "permissions.el"))
         (reads 0))
    (unwind-protect
        (progn
          (clrhash mevedel-permission--store-cache)
          (write-region "(:rules nil :resource-grants nil)" nil file nil 'silent)
          (advice-add
           'insert-file-contents :before
           (lambda (&rest _) (setq reads (1+ reads)))
           '((name . mevedel-store-cache-count)))
          (unwind-protect
              (progn
                (should (eq 'valid (plist-get
                                    (mevedel-permission--store-file-status file)
                                    :status)))
                (should (= 1 reads))
                (mevedel-permission--store-file-status file)
                (mevedel-permission--store-file-status file)
                (should (= 1 reads)))
            (advice-remove 'insert-file-contents 'mevedel-store-cache-count)))
      (clrhash mevedel-permission--store-cache)
      (delete-directory dir t)))

  :doc "validation reads the file itself, so a hand edit is still caught"
  (let* ((dir (make-temp-file "mevedel-store-fresh-" t))
         (file (file-name-concat dir "permissions.el")))
    (unwind-protect
        (progn
          (clrhash mevedel-permission--store-cache)
          (write-region "(:rules nil :resource-grants nil)" nil file nil 'silent)
          (should (eq 'valid (plist-get
                              (mevedel-permission--store-file-status file)
                              :status)))
          (write-region "not-a-store" nil file nil 'silent)
          ;; The cache still answers valid; the uncached read does not.
          (should (eq 'valid (plist-get
                              (mevedel-permission--store-file-status file)
                              :status)))
          (should (eq 'invalid
                      (plist-get
                       (mevedel-permission--read-store-file-uncached file)
                       :status))))
      (clrhash mevedel-permission--store-cache)
      (delete-directory dir t))))

(provide 'test-mevedel-permission-persistence)
;;; test-mevedel-permission-persistence.el ends here
