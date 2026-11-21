;;; test-niwa-permission-persistence.el -- Permission store tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests portable permission authority encoding, target-aware store reads,
;; validation, and atomic store replacement.

;;; Code:

(require 'niwa-execution-target)
(require 'niwa-permission-persistence)
(require 'niwa-permissions)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-tool-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Persistent rule storage

(niwa-deftest niwa-permission-persistence-file ()
  ,test
  (test)
  :doc "selects local global and workspace stores without conflating their scopes"
  (let ((niwa-user-dir "/tmp/niwa-global/")
        (ws (niwa-workspace--create :type 'project :root "/tmp/project/")))
    (should (equal "/tmp/niwa-global/permissions.el"
                   (niwa-permission-persistence-file ws 'global)))
    (should (equal "/tmp/project/.niwa/permissions.el"
                   (niwa-permission-persistence-file ws 'workspace)))
    (should-error (niwa-permission-persistence-file ws 'other)))
  :doc "remote global discovery requires target home and never falls back locally"
  (let* ((niwa-user-dir "/tmp/client-global/")
         (ws (niwa-workspace--create
              :type 'project :root "/ssh:dev@target:/srv/project/"))
         (target (niwa-execution-target-create (niwa-workspace-root ws)))
         (session (niwa-session--create :workspace ws :execution-target target)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (should-error (niwa-permission-persistence-file ws 'global)
                    :type 'niwa-execution-target-error)
      (setf (niwa-execution-target-environment target)
            '(("HOME" . "/home/dev")))
      (should (equal "/ssh:dev@target:/home/dev/.niwa/permissions.el"
                     (niwa-permission-persistence-file ws 'global))))))

(niwa-deftest niwa-permission-serialize-authority ()
  ,test
  (test)
  :doc "stores remote rule and grant paths in the target-native domain"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should
     (equal
      '(:rules
        (("Read" :path "/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system ((:path "/var/cache/build" :access write))
          :action allow))
        :resource-grants ((:path "/srv/private/key" :access read)))
      (niwa-permission-serialize-authority
       '(("Read" :path "/ssh:user@alias-a:/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system
          ((:path "/ssh:user@alias-a:/var/cache/build" :access write))
          :action allow))
       '((:path "/ssh:user@alias-a:/srv/private/key" :access read))
       target))))

  :doc "rejects authority naming another execution target"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should-not
     (niwa-permission-serialize-authority
      '(("Read" :path "/ssh:user@alias-b:/srv/private/**" :action allow))
      nil target)))

  :doc "does not reinterpret a client absolute path as remote authority"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@alias-a:/srv/project/")))
    (should-not
     (niwa-permission-serialize-authority
      nil '((:path "/home/client/.ssh/id" :access read)) target)))

  :doc "keeps remote home syntax and abbreviates paths under the remote home"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/dev")))
    (should
     (equal
      '(:rules
        (("Bash" :pattern "npm test"
          :file-system ((:path "~/.npm" :access write))
          :action allow))
        :resource-grants ((:path "~/.npm" :access write)
                          (:path "~/.cache/x" :access read)
                          (:path "~" :access read)
                          (:path "/srv/data" :access read)))
      (niwa-permission-serialize-authority
       '(("Bash" :pattern "npm test"
          :file-system ((:path "~/.npm" :access write))
          :action allow))
       '((:path "~/.npm" :access write)
         (:path "/docker:dev:/home/dev/.cache/x" :access read)
         (:path "/docker:dev:/home/dev" :access read)
         (:path "/docker:dev:/srv/data" :access read))
       target))))

  :doc "abbreviates local paths under the home directory so the store is shareable"
  (let ((target (niwa-execution-target-create default-directory))
        (home (expand-file-name "~")))
    (should
     (equal
      `(:rules
        (("Read" :path "~/proj/*" :action allow)
         ("Edit" :path "/opt/elsewhere/*" :action allow))
        :resource-grants ((:path "~/.npm" :access write)
                          (:path "~" :access read)))
      (niwa-permission-serialize-authority
       `(("Read" :path ,(file-name-concat home "proj/*") :action allow)
         ("Edit" :path "/opt/elsewhere/*" :action allow))
       `((:path ,(file-name-concat home ".npm") :access write)
         (:path ,home :access read))
       target))))

  :doc "preserves the first path component when the home directory is root"
  (let ((process-environment (copy-sequence process-environment))
        (target (niwa-execution-target-create default-directory)))
    (setenv "HOME" "/")
    (should
     (equal '(:rules nil :resource-grants ((:path "~/cache" :access read)))
            (niwa-permission-serialize-authority
             nil '((:path "/cache" :access read)) target)))))

(niwa-deftest niwa-permission-deserialize-authority ()
  ,test
  (test)
  :doc "requalifies target-native authority through the current alias"
  (let ((target (niwa-execution-target-create
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
      (niwa-permission-deserialize-authority
       '(("Read" :path "/srv/private/**" :action allow)
         ("Bash" :pattern "make cache"
          :file-system ((:path "/var/cache/build" :access write))
          :action allow))
       '((:path "/srv/private/key" :access read)) target))))

  :doc "expands home-relative authority from the target environment"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/dev")))
    (should
     (equal
      '(:rules
        (("Bash" :pattern "npm test"
          :file-system ((:path "/docker:dev:/home/dev/.npm" :access write))
          :action allow))
        :resource-grants
        ((:path "/docker:dev:/home/dev/.npm" :access write)))
      (niwa-permission-deserialize-authority
       '(("Bash" :pattern "npm test"
          :file-system ((:path "~/.npm" :access write))
          :action allow))
       '((:path "~/.npm" :access write)) target))))

  :doc "rejects durable authority containing a client-specific TRAMP prefix"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@alias-b:/srv/project/")))
    (should-not
     (niwa-permission-deserialize-authority
      nil '((:path "/ssh:user@alias-a:/srv/private/key" :access read))
      target)))

  :doc "round-trips ordinary absolute paths for a local target"
  (let ((target (niwa-execution-target-create "/srv/project/")))
    (should
     (equal
      '(:rules (("Read" :path "/srv/private/**" :action allow))
        :resource-grants ((:path "/srv/private/key" :access read)))
      (niwa-permission-deserialize-authority
       '(("Read" :path "/srv/private/**" :action allow))
       '((:path "/srv/private/key" :access read)) target)))))

(niwa-deftest niwa-permission-persistence-save-rule ()
  ,test
  (test)
  :doc "round-trip save and load"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-rule
           ws '("Read" :action allow))
          (niwa-permission-persistence-save-rule
           ws '("Edit" :path "/secret/*" :action deny))
          (let ((rules (niwa-permission-persistence-load-rules ws)))
            (should (= (length rules) 2))
            (should (equal (car rules) '("Read" :action allow)))
            (should (equal (cadr rules)
                           '("Edit" :path "/secret/*" :action deny)))))
      (delete-directory tmp-dir t)))
  :doc "load returns nil for missing file"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (should-not (niwa-permission-persistence-load-rules ws))
      (delete-directory tmp-dir t)))
  :doc "round-trips an execution-level-qualified persistent rule"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-rule
           ws '("Eval" :pattern "(message *)"
                       :sandbox-permissions require-escalated :action allow))
          (should
           (equal
            '(("Eval" :pattern "(message *)"
                      :sandbox-permissions require-escalated
                      :action allow))
            (niwa-permission-persistence-load-rules ws))))
      (delete-directory tmp-dir t)))
  :doc "merges global and project rules, project rules last"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (global-dir (file-name-concat tmp-dir "global/"))
         (project-dir (file-name-concat tmp-dir "project/"))
         (niwa-user-dir global-dir)
         (ws (niwa-workspace--create
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
          (niwa-permission-persistence-save-rule
           ws '("Edit" :path "~/proj/*" :action allow))
          (let ((rules (niwa-permission-persistence-load-rules ws)))
            ;; Global rules first, then project
            (should (= (length rules) 3))
            (should (equal (nth 0 rules) '("Read" :action allow)))
            (should (equal (nth 1 rules) '("*" :path "/tmp/*" :action allow)))
            (should (equal (nth 2 rules) '("Edit" :path "~/proj/*" :action allow)))))
      (delete-directory tmp-dir t)))
  :doc "loads both stores from the target and ignores client global rules"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (global-dir (file-name-concat tmp-dir "global/"))
         (project-dir (file-name-concat tmp-dir "project/"))
         (target-home (file-name-concat tmp-dir "target-home"))
         (niwa-user-dir global-dir))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("alias-a" "alias-b")
          (make-directory (file-name-concat project-dir ".niwa") t)
          (make-directory global-dir t)
          (make-directory (file-name-concat target-home ".niwa") t)
          (with-temp-file (file-name-concat target-home ".niwa/permissions.el")
            (pp '(:rules (("Read" :path "~/private/**" :action allow))
                  :resource-grants ((:path "~/.npm" :access write :recursive t)))
                (current-buffer)))
          (with-temp-file (file-name-concat global-dir "permissions.el")
            (pp '(:rules (("Read" :path "/home/client/private/**"
                           :action allow))
                  :resource-grants nil)
                (current-buffer)))
          (with-temp-file
              (file-name-concat project-dir ".niwa/permissions.el")
            (pp '(:rules (("Read" :path "/srv/shared/**" :action allow))
                  :resource-grants nil)
                (current-buffer)))
          (let* ((root (format "/niwamock:alias-b:%s/" project-dir))
                 (workspace (niwa-workspace--create
                             :type 'project :id "test" :root root
                             :name "test" :file-cache nil))
                 (target (niwa-execution-target-create root))
                 (session (niwa-session--create
                           :name "test" :workspace workspace
                           :execution-target target)))
            (setf (niwa-execution-target-environment target)
                  `(("HOME" . ,target-home)))
            (with-temp-buffer
              (setq-local niwa--session session)
              (should
               (equal
                `(("Read" :path ,(format "/niwamock:alias-b:%s/private/**" target-home)
                          :action allow)
                  ("Read" :path
                   ,(format "/niwamock:alias-b:/srv/shared/**")
                   :action allow))
                (niwa-permission-persistence-load-rules workspace)))
              (should
               (equal `((:path ,(format "/niwamock:alias-b:%s/.npm" target-home)
                         :access write :recursive t))
                      (niwa-permission-persistence-load-resource-grants workspace))))))
      (delete-directory tmp-dir t)))
  :doc "deduplicates exact persistent rules"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "git diff:*" :action allow))
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "git diff:*" :action allow))
          (let ((rules (niwa-permission-persistence-load-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow))))))
      (delete-directory tmp-dir t)))
  :doc "preserves distinct persistent rules"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test"
              :file-cache (make-hash-table :test #'equal))))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "git diff:*" :action allow))
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "git diff:*" :action deny))
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "git status:*" :action allow))
          (let ((rules (niwa-permission-persistence-load-rules ws)))
            (should (equal rules
                           '(("Bash" :pattern "git diff:*"
                              :action allow)
                             ("Bash" :pattern "git diff:*"
                              :action deny)
                             ("Bash" :pattern "git status:*"
                              :action allow))))))
      (delete-directory tmp-dir t)))
  :doc "refuses to overwrite an invalid persistent store"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert "(:rules malformed :resource-grants nil)"))
          (should-error
           (niwa-permission-persistence-save-rule
            ws '("Read" :action allow))
           :type 'user-error)
          (should (equal "(:rules malformed :resource-grants nil)"
                         (with-temp-buffer
                           (insert-file-contents file)
                           (buffer-string)))))
      (delete-directory tmp-dir t)))
  :doc "refuses a permission store symlink without changing its target"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (outside (file-name-concat tmp-dir "outside.el"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (write-region "(:rules nil :resource-grants nil)\n"
                        nil outside nil 'silent)
          (make-symbolic-link outside file)
          (should-error
           (niwa-permission-persistence-save-rule
            ws '("Read" :action allow)))
          (should (file-symlink-p file))
          (should (equal "(:rules nil :resource-grants nil)\n"
                         (with-temp-buffer
                           (insert-file-contents outside)
                           (buffer-string)))))
      (delete-directory tmp-dir t)))
  :doc "refuses a symlinked workspace state directory"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (outside-dir (make-temp-file "niwa-permissions-outside-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (state-dir (file-name-concat tmp-dir ".niwa"))
         (outside-file (file-name-concat outside-dir "permissions.el"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (make-symbolic-link outside-dir state-dir)
          (should-error
           (niwa-permission-persistence-save-rule
            ws '("Read" :action allow)))
          (should-not (file-exists-p outside-file)))
      (when (file-symlink-p state-dir)
        (delete-file state-dir))
      (delete-directory tmp-dir t)
      (delete-directory outside-dir t))))

(niwa-deftest niwa-permission-remove-persistent-resource-grant ()
  ,test
  (test)
  :doc "revokes one persistent grant without changing command rules"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-rule
           ws '("Read" :action allow))
          (niwa-permission-persistence-save-resource-grant ws path 'read)
          (should (eq 'allow
                      (niwa-check-permission
                       "Read"
                       :tool-struct (niwa-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (niwa-permission-persistence-load-resource-grants
                        ws))))
          (niwa-permission-remove-persistent-resource-grant ws 'workspace path 'read)
          (should-not
           (niwa-permission-persistence-load-resource-grants ws))
          (should (equal '(("Read" :action allow))
                         (niwa-permission-persistence-load-rules ws)))
          (should (eq 'ask
                      (niwa-check-permission
                       "Read"
                       :tool-struct (niwa-tool--create
                                     :name "Read" :read-only-p t)
                       :path path :mode 'ask
                       :workspace-root "/different"
                       :resource-grants
                       (niwa-permission-persistence-load-resource-grants
                        ws)))))
      (delete-directory tmp-dir t))))


(niwa-deftest niwa-permission-persistence-load-resource-grants ()
  ,test
  (test)
  :doc "merges global grants with workspace grants without widening extent or access"
  (let* ((dir (make-temp-file "niwa-global-grants-" t))
         (niwa-user-dir (file-name-concat dir "user"))
         (ws (niwa-workspace--create :type 'project :root dir))
         (global (niwa-permission-persistence-file ws 'global))
         (exact (file-name-concat dir "exact"))
         (tree (file-name-concat dir "tree"))
         (local (file-name-concat dir "local")))
    (unwind-protect
        (progn
          (niwa-permission-persistence-write-store
           global `(:rules nil :resource-grants
                    ((:path ,exact :access read)
                     (:path ,tree :access write :recursive t))))
          (niwa-permission-persistence-save-resource-grant ws local 'read)
          (let ((grants (niwa-permission-persistence-load-resource-grants ws)))
            (should (= 3 (length grants)))
            (should (niwa-permission-rules-resource-granted-p exact 'read grants))
            (should-not (niwa-permission-rules-resource-granted-p exact 'write grants))
            (should-not (niwa-permission-rules-resource-granted-p
                         (file-name-concat exact "child") 'read grants))
            (should (niwa-permission-rules-resource-granted-p
                     (file-name-concat tree "child") 'write grants))
            (should (niwa-permission-rules-resource-granted-p local 'read grants)))
          (should (= 2 (length (plist-get
                               (niwa-permission-persistent-authority ws 'global)
                               :resource-grants)))))
      (delete-directory dir t)))
  :doc "an invalid grant makes the editable store fail closed"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
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
           (niwa-permission-persistence-load-resource-grants ws)))
      (delete-directory tmp-dir t)))
  :doc "expands home-relative exact grants in rules and authority"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (user-home (file-name-concat tmp-dir "home"))
         (niwa-user-dir (file-name-concat tmp-dir "global/"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
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
              (niwa-permission-persistence-load-rules ws)))
            (should
             (equal `((:path ,path :access write))
                    (niwa-permission-persistence-load-resource-grants
                     ws)))))
      (delete-directory tmp-dir t)))
  :doc "round-trips recursive grants in authority and rule profiles"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (pp '(:rules
                  (("Bash" :pattern "some-command *"
                    :file-system
                    ((:path "/usr/share/emacs" :access read :recursive t))
                    :action allow))
                  :resource-grants
                  ((:path "/usr/share/emacs" :access read :recursive t)
                   (:path "/tmp/exact" :access read :recursive nil)))
                (current-buffer)))
          (should
           (equal
            '(("Bash" :pattern "some-command *"
               :file-system
               ((:path "/usr/share/emacs" :access read :recursive t))
               :action allow))
            (niwa-permission-persistence-load-rules ws)))
          ;; :recursive nil is canonicalized away.
          (should
           (equal '((:path "/usr/share/emacs" :access read :recursive t)
                    (:path "/tmp/exact" :access read))
                  (niwa-permission-persistence-load-resource-grants ws))))
      (delete-directory tmp-dir t)))
  :doc "a non-boolean recursive flag makes the store fail closed"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil))
         (file (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (pp '(:rules nil
                  :resource-grants
                  ((:path "/tmp/read" :access read :recursive yes)))
                (current-buffer)))
          (should-not
           (niwa-permission-persistence-load-resource-grants ws)))
      (delete-directory tmp-dir t))))

(niwa-deftest niwa-permission-validate-persistent-stores ()
  ,test
  (test)
  :doc "warns once per invalid file version and stays quiet for missing stores"
  (dolist (scope '(global workspace))
    (let* ((tmp-dir (make-temp-file "niwa-test-" t))
           (niwa-user-dir (file-name-concat tmp-dir "global/"))
           (ws (niwa-workspace--create
		:type 'project :id "test" :root tmp-dir
		:name "test" :file-cache nil))
           (file (niwa-permission-persistence-file ws scope))
           (niwa--warn-once-table (make-hash-table :test #'equal))
           warnings)
      (unwind-protect
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (&rest args) (push args warnings))))
		   (niwa-permission-validate-persistent-stores ws)
		   (should-not warnings)
		   (make-directory (file-name-directory file) t)
		   (with-temp-file file
		     (insert "(:rules malformed :resource-grants nil)"))
		   (niwa-permission-validate-persistent-stores ws)
		   (niwa-permission-validate-persistent-stores ws)
		   (should (= 1 (length warnings)))
		   (with-temp-file file
		     (insert "(:rules nil :resource-grants malformed)"))
		   (set-file-times file (time-add (current-time) 1))
		   (niwa-permission-validate-persistent-stores ws)
		   (should (= 2 (length warnings)))
		   (should (string-match-p
			    "(:rules (\\.\\.\\.) :resource-grants (\\.\\.\\.))"
			    (cadar warnings))))
	(delete-directory tmp-dir t)))))

(niwa-deftest niwa-permission-persistence-refresh ()
  ,test
  (test)
  :doc "deferred refresh retains the originating target and cancels a dead owner"
  (let* ((dir (make-temp-file "niwa-refresh-owner-" t))
         (home (file-name-concat dir "home"))
         (owner (generate-new-buffer " *permission-refresh-owner*"))
         (workspace (niwa-workspace--create
                     :type 'project :root (format "/niwamock:owner:%s/" dir)))
         (target (niwa-execution-target-create (niwa-workspace-root workspace)))
         (session (niwa-session--create :workspace workspace :execution-target target))
         deferred observed cancelled)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("owner")
          (setf (niwa-execution-target-environment target) `(("HOME" . ,home)))
          (make-directory (file-name-concat home ".niwa") t)
          (write-region "(:rules ((\"Read\" :action deny)) :resource-grants nil)"
                        nil (file-name-concat home ".niwa/permissions.el") nil 'silent)
          (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _) t))
                    ((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (_key _path thunk &optional _on-cancel)
                       (setq deferred thunk) t)))
            (with-current-buffer owner
              (setq-local niwa--session session)
              (niwa-permission-persistence-refresh
               workspace
               (lambda ()
                 (setq observed (list (current-buffer)
                                      (niwa-permission-persistence-load-rules workspace))))
               (lambda () (setq cancelled t)))))
          ;; The transport timer may fire while an unrelated buffer is current.
          (with-temp-buffer (funcall deferred))
          (should (equal (list owner '(("Read" :action deny))) observed))
          (setq observed nil)
          (kill-buffer owner)
          (funcall deferred)
          (should cancelled)
          (should-not observed))
      (when (buffer-live-p owner) (kill-buffer owner))
      (delete-directory dir t)))
  :doc "waits for an idle remote transport before refreshing authority"
  (let ((workspace (niwa-workspace--create
                    :type 'project :id "remote"
                    :root "/niwamock:host:/srv/project/"
                    :name "remote"))
        deferred refreshed continued)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (_key _path thunk &optional _on-cancel)
                 (setq deferred thunk)
                 t))
              ((symbol-function 'niwa-permission-validate-persistent-stores)
               (lambda (_) (setq refreshed t))))
      (niwa-permission-persistence-refresh
       workspace (lambda () (setq continued t)))
      (should-not refreshed)
      (should-not continued)
      (funcall deferred)
      (should refreshed)
      (should continued)))

  :doc "delivers deferred target discovery errors without leaving a tool pending"
  (let ((workspace (niwa-workspace--create
                    :type 'project :root "/ssh:dev@target:/srv/project/"))
        deferred continued failure)
    (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (_key _path thunk &optional _on-cancel)
                 (setq deferred thunk) t)))
      (niwa-permission-persistence-refresh
       workspace (lambda () (setq continued t)) nil
       (lambda (err) (setq failure err)))
      ;; No live target environment: global HOME discovery must fail closed.
      (funcall deferred)
      (should (eq 'niwa-execution-target-error (car failure)))
      (should-not continued)))

  :doc "fails closed when deferred refresh cannot be queued"
  (let ((workspace (niwa-workspace--create
                    :type 'project :id "remote"
                    :root "/niwamock:host:/srv/project/"
                    :name "remote"))
        continued cancelled)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (&rest _) nil)))
      (should-not
       (niwa-permission-persistence-refresh
        workspace
        (lambda () (setq continued t))
        (lambda () (setq cancelled t))))
      (should-not continued)
      (should cancelled))))

(niwa-deftest niwa-permission-persistence-save-resource-grant ()
  ,test
  (test)
  :doc "write authority promotes a persisted exact read grant"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (path (file-name-concat tmp-dir "outside.el"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-resource-grant
           ws path 'read)
          (niwa-permission-persistence-save-resource-grant
           ws path 'write)
          (should
           (equal `((:path ,path :access write))
                  (niwa-permission-persistence-load-resource-grants ws))))
      (delete-directory tmp-dir t)))

  :doc "recursive grants persist and revoke independently of exact ones"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (path (file-name-concat tmp-dir "tree"))
         (ws (niwa-workspace--create
              :type 'project :id "test" :root tmp-dir
              :name "test" :file-cache nil)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-save-resource-grant
           ws path 'read)
          (niwa-permission-persistence-save-resource-grant
           ws path 'read t)
          (should
           (equal `((:path ,path :access read)
                    (:path ,path :access read :recursive t))
                  (niwa-permission-persistence-load-resource-grants ws)))
          (niwa-permission-remove-persistent-resource-grant
           ws 'workspace path 'read t)
          (should
           (equal `((:path ,path :access read))
                  (niwa-permission-persistence-load-resource-grants ws))))
      (delete-directory tmp-dir t)))

  :doc "writes target-native paths and reloads them through the live alias"
  (let* ((tmp-dir (make-temp-file "niwa-test-" t))
         (project-dir (file-name-concat tmp-dir "project"))
         (root (format "/niwamock:alias-a:%s/" project-dir))
         (workspace (niwa-workspace--create
                     :type 'project :id "test" :root root
                     :name "test" :file-cache nil))
         (niwa-session-durability--client-id (make-string 64 ?a))
         (qualified "/niwamock:alias-a:/srv/shared/key")
         (local-file (file-name-concat project-dir
                                      ".niwa/permissions.el"))
         target session)
    (unwind-protect
        ;; The target records its support tier at construction, so it must be
        ;; built while the mock method is supported.
        (niwa-test--with-local-shell-tramp '("alias-a")
          (setq target (niwa-execution-target-create root)
                session (niwa-session--create
                         :name "test" :workspace workspace
                         :execution-target target
                         :save-path (file-name-concat
                                     root ".niwa/sessions/test")))
          (make-directory (niwa-session-save-path session) t)
          (puthash (niwa-execution-target-identity target) t
                   niwa-session-durability--disclosed-targets)
          (should
           (niwa-session-durability-lease-acquire
            (niwa-session-save-path session) "test" session))
          (unwind-protect
              (with-temp-buffer
                (setq-local niwa--session session)
                (niwa-permission-persistence-save-resource-grant
                 workspace qualified 'read)
                (should
                 (equal `((:path ,qualified :access read))
                        (niwa-permission-persistence-load-resource-grants
                         workspace))))
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session))
          (let ((raw (with-temp-buffer
                       (insert-file-contents local-file)
                       (read (current-buffer)))))
            (should
             (equal '((:path "/srv/shared/key" :access read))
                    (plist-get raw :resource-grants)))
            (should-not (string-match-p
                         "niwamock"
                         (with-temp-buffer
                           (insert-file-contents local-file)
                           (buffer-string))))))
      (delete-directory tmp-dir t))))

(niwa-deftest niwa-permission--store-file-status ()
  ,test
  (test)

  :doc "a repeat read is served from memory, never from the target"
  ;; Consulted twice per tool call, from inside gptel's curl sentinel that
  ;; TRAMP's wait loop re-runs; a read there lands on a connection that
  ;; already has a command in flight.
  (let* ((dir (make-temp-file "niwa-store-cache-" t))
         (file (file-name-concat dir "permissions.el"))
         (reads 0))
    (unwind-protect
        (progn
          (clrhash niwa-permission--store-cache)
          (write-region "(:rules nil :resource-grants nil)" nil file nil 'silent)
          (advice-add
           'insert-file-contents :before
           (lambda (&rest _) (setq reads (1+ reads)))
           '((name . niwa-store-cache-count)))
          (unwind-protect
              (progn
                (should (eq 'valid (plist-get
                                    (niwa-permission--store-file-status file)
                                    :status)))
                (should (= 1 reads))
                (niwa-permission--store-file-status file)
                (niwa-permission--store-file-status file)
                (should (= 1 reads)))
            (advice-remove 'insert-file-contents 'niwa-store-cache-count)))
      (clrhash niwa-permission--store-cache)
      (delete-directory dir t)))

  :doc "validation reads the file itself, so a hand edit is still caught"
  (let* ((dir (make-temp-file "niwa-store-fresh-" t))
         (file (file-name-concat dir "permissions.el")))
    (unwind-protect
        (progn
          (clrhash niwa-permission--store-cache)
          (write-region "(:rules nil :resource-grants nil)" nil file nil 'silent)
          (should (eq 'valid (plist-get
                              (niwa-permission--store-file-status file)
                              :status)))
          (write-region "not-a-store" nil file nil 'silent)
          ;; The cache still answers valid; the uncached read does not.
          (should (eq 'valid (plist-get
                              (niwa-permission--store-file-status file)
                              :status)))
          (should (eq 'invalid
                      (plist-get
                       (niwa-permission--read-store-file-uncached file)
                       :status))))
      (clrhash niwa-permission--store-cache)
      (delete-directory dir t))))

(niwa-deftest niwa-permission-remove-persistent-rule ()
  ,test
  (test)
  :doc "global revocation preserves workspace duplicates and later external edits"
  (let* ((dir (make-temp-file "niwa-global-revoke-" t))
         (niwa-user-dir (file-name-concat dir "user"))
         (ws (niwa-workspace--create :type 'project :root dir))
         (file (niwa-permission-persistence-file ws 'global))
         (rule '("Bash" :pattern "echo *" :action allow))
         (other '("Bash" :pattern "pwd" :action allow)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-write-store
           file (list :rules (list rule) :resource-grants nil))
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "echo *" :action allow))
          (niwa-permission-validate-persistent-stores ws)
          ;; A separate editor adds a rule after the cockpit's snapshot.
          (with-temp-file file
            (prin1 (list :rules (list rule other) :resource-grants nil)
                   (current-buffer)))
          (niwa-permission-remove-persistent-rule ws 'global rule)
          (should (equal (list other)
                         (plist-get (niwa-permission-persistent-authority
                                     ws 'global) :rules)))
          (should (equal (list rule)
                         (plist-get (niwa-permission-persistent-authority
                                     ws 'workspace) :rules))))
      (delete-directory dir t))))

(niwa-deftest niwa-permission-persistence-editable-store ()
  ,test
  (test)
  :doc "refuses an invalid external edit even when the old cached store was valid"
  (let ((file (make-temp-file "niwa-editable-store-")))
    (unwind-protect
        (progn
          (write-region "(:rules nil :resource-grants nil)" nil file nil 'silent)
          (should (niwa-permission--read-store-file file))
          (write-region "(:rules malformed :resource-grants nil)" nil file nil 'silent)
          (should-error (niwa-permission-persistence-editable-store file)
                        :type 'user-error))
      (delete-file file)))
  :doc "refuses reentrant reads during a busy target operation"
  (cl-letf (((symbol-function 'niwa-transport-busy-p) (lambda (_) t))
            ((symbol-function 'niwa-permission--read-store-file-uncached)
             (lambda (&rest _) (ert-fail "Read a busy target"))))
    (should-error (niwa-permission-persistence-editable-store
                   "/ssh:dev@target:/home/dev/.niwa/permissions.el")
                  :type 'user-error)))

(niwa-deftest niwa-permission-persistence-load-safe-env-vars ()
  ,test
  (test)
  :doc "merges both stores' safe variables and keeps them when rules are saved"
  (let* ((dir (make-temp-file "niwa-safe-env-" t))
         (niwa-user-dir (file-name-concat dir "user"))
         (ws (niwa-workspace--create :type 'project :root dir))
         (global (niwa-permission-persistence-file ws 'global))
         (local (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (niwa-permission-persistence-write-store
           global '(:rules nil :resource-grants nil :safe-env-vars ("CHECK_BASE")))
          (make-directory (file-name-directory local) t)
          (with-temp-file local
            (prin1 '(:rules nil :resource-grants nil
                     :safe-env-vars ("CHECK_BASE" "MAKEFLAGS_QUIET"))
                   (current-buffer)))
          (should (equal '("CHECK_BASE" "MAKEFLAGS_QUIET")
                         (niwa-permission-persistence-load-safe-env-vars ws)))
          (niwa-permission-persistence-save-rule
           ws '("Bash" :pattern "make check" :action allow))
          (should (equal '("CHECK_BASE" "MAKEFLAGS_QUIET")
                         (plist-get (niwa-permission-persistent-authority
                                     ws 'workspace)
                                    :safe-env-vars)))
          (should (equal '(("Bash" :pattern "make check" :action allow))
                         (plist-get (niwa-permission-persistent-authority
                                     ws 'workspace)
                                    :rules))))
      (delete-directory dir t)))
  :doc "an invalid variable name disables the store and names the entry"
  (let* ((dir (make-temp-file "niwa-safe-env-bad-" t))
         (niwa-user-dir (file-name-concat dir "user"))
         (ws (niwa-workspace--create :type 'project :root dir))
         (local (niwa-permission-persistence-file ws 'workspace)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory local) t)
          (with-temp-file local
            (prin1 '(:rules (("Bash" :pattern "make" :action allow))
                     :resource-grants nil
                     :safe-env-vars ("OK" "NOT OK"))
                   (current-buffer)))
          (should-not (niwa-permission-persistence-load-safe-env-vars ws))
          (should-not (niwa-permission-persistence-load-rules ws))
          (should (string-match-p
                   ":safe-env-vars"
                   (plist-get (niwa-permission--read-store-file-uncached
                               local (niwa-permission--workspace-target ws) dir)
                              :reason))))
      (delete-directory dir t))))

(niwa-deftest niwa-permission-remove-persistent-safe-env-var ()
  ,test
  (test)
  :doc "removes one name from the selected store and keeps its other entries"
  (let* ((dir (make-temp-file "niwa-safe-env-remove-" t))
         (niwa-user-dir (file-name-concat dir "user"))
         (ws (niwa-workspace--create :type 'project :root dir))
         (rule '("Bash" :pattern "make" :action allow))
         (store `(:rules (,rule) :resource-grants nil :safe-env-vars ("A" "B"))))
    (unwind-protect
        (progn
          (niwa-permission-persistence-write-store
           (niwa-permission-persistence-file ws 'global) store)
          (niwa-permission-persistence-write-store
           (niwa-permission-persistence-file ws 'workspace) store)
          (niwa-permission-remove-persistent-safe-env-var ws 'global "A")
          (should (equal `(:rules (,rule) :resource-grants nil :safe-env-vars ("B"))
                         (niwa-permission-persistent-authority ws 'global)))
          (niwa-permission-remove-persistent-safe-env-var ws 'global "B")
          (should (equal `(:rules (,rule) :resource-grants nil)
                         (niwa-permission-persistent-authority ws 'global)))
          (should (equal store (niwa-permission-persistent-authority ws 'workspace))))
      (delete-directory dir t))))

(niwa-deftest niwa-permission-persistence-write-store ()
  ,test
  (test)
  :doc "rejects a remote change before it can be queued as apparent success"
  (let ((session (niwa-session--create :publication-active-p t)))
    (with-temp-buffer
      (setq-local niwa--session session)
      (cl-letf (((symbol-function 'niwa-session-artifacts-publish-text)
                 (lambda (&rest _) (ert-fail "Queued a permission change"))))
        (should-error
         (niwa-permission-persistence-write-store
          "/ssh:dev@target:/home/dev/.niwa/permissions.el"
          '(:rules nil :resource-grants nil))
         :type 'user-error)))))

(provide 'test-niwa-permission-persistence)
;;; test-niwa-permission-persistence.el ends here
