;;; test-niwa-plugin-registry.el -- Plugin registry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests plugin discovery, manifest trust, and durable consent state.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-cockpit)
(require 'niwa-execution-target)
(require 'niwa-plugin-registry)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-plugin-test-support"))

(defvar niwa--session)
(defvar niwa--workspace)
(defvar niwa-plugins-test--read-eval-ran nil)

(niwa-deftest niwa-plugins-list
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-" t)))
           (extra-dir (file-name-as-directory
                       (make-temp-file "niwa-plugins-extra-" t)))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (niwa-plugin-extra-roots nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory extra-dir t)))
  ,test
  (test)
  :doc "owns discovery in the plugin registry module"
  (should (string-suffix-p
           "niwa-plugin-registry"
           (file-name-sans-extension
            (symbol-file 'niwa-plugins-list 'defun))))

  :doc "discovers Codex plugin manifests and resolves manifest paths"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"version\":\"1.2.3\",\"description\":\"Demo plugin\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}")
    (let ((plugin (car (niwa-plugins-list))))
      (should (equal "demo" (niwa-plugin-name plugin)))
      (should (equal "1.2.3" (niwa-plugin-version plugin)))
      (should (equal "Demo plugin" (niwa-plugin-description plugin)))
      (should (equal (file-name-as-directory (expand-file-name root))
                     (niwa-plugin-root plugin)))
      (should (equal (file-name-concat root "skills")
                     (niwa-plugin-skills-dir plugin)))
      (should (equal (file-name-concat root "hooks" "hooks.json")
                     (niwa-plugin-hooks-file plugin)))
      (should (equal (list (list :file (file-name-concat
                                        root "hooks" "hooks.json")))
                     (niwa-plugin-hooks plugin)))))

  :doc "uses default hooks/hooks.json when the manifest omits hooks"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((plugin (car (niwa-plugins-list))))
      (should (equal (file-name-concat root "hooks" "hooks.json")
                     (niwa-plugin-hooks-file plugin)))
      (should (equal (list (list :file (file-name-concat
                                        root "hooks" "hooks.json")))
                     (niwa-plugin-hooks plugin)))))

  :doc "ignores default hook files that escape the plugin root"
  (let* ((root (niwa-plugins-test--plugin-root user-dir "repo"))
         (outside (make-temp-file "niwa-plugins-outside-hook-"))
         (hook-link (file-name-concat root "hooks" "hooks.json")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory hook-link) t)
          (make-symbolic-link outside hook-link)
          (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (let ((plugin (car (niwa-plugins-list))))
            (should (equal "demo" (niwa-plugin-name plugin)))
            (should-not (niwa-plugin-hooks-file plugin))
            (should-not (niwa-plugin-hooks plugin))))
      (delete-file outside)))

  :doc "discovers nested GitHub install paths below the install dir"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal '("demo")
                   (mapcar #'niwa-plugin-name
                           (niwa-plugins-list)))))

  :doc "discovers plugin roots from extra roots without descending into plugins"
  (let* ((direct (file-name-concat extra-dir "direct"))
         (nested (file-name-concat extra-dir "group" "nested"))
         (hidden (file-name-concat direct "child")))
    (niwa-plugins-test--write-manifest direct "{\"name\":\"direct\"}")
    (niwa-plugins-test--write-manifest nested "{\"name\":\"nested\"}")
    (niwa-plugins-test--write-manifest hidden "{\"name\":\"hidden\"}")
    (let ((niwa-plugin-extra-roots (list extra-dir direct)))
      (should (equal '("direct" "nested")
                     (mapcar #'niwa-plugin-name
                             (niwa-plugins-list))))))

  :doc "deduplicates extra-root symlink aliases by true name"
  (let* ((root (niwa-plugins-test--github-plugin-root
                user-dir "owner" "repo"))
         (link (file-name-concat extra-dir "repo-link")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-symbolic-link root link)
    (let ((niwa-plugin-extra-roots (list link)))
      (should (equal '("demo")
                     (mapcar #'niwa-plugin-name
                             (niwa-plugins-list))))))

  :doc "keeps the highest-precedence duplicate and reports shadowed sources"
  (let ((root-a (niwa-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (niwa-plugins-test--plugin-root user-dir "repo-b")))
    (niwa-plugins-test--write-manifest root-a "{\"name\":\"demo\"}")
    (niwa-plugins-test--write-manifest root-b "{\"name\":\"demo\"}")
    (let ((plugin (car (niwa-plugins-list))))
      (should (equal "demo" (niwa-plugin-name plugin)))
      (should (equal (file-name-as-directory (expand-file-name root-a))
                     (niwa-plugin-root plugin)))
      (should (= 1 (length (niwa-plugin-shadowed plugin))))))

  :doc "applies workspace/global resource root precedence"
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "niwa-plugins-precedence-" t)))
         (workspace (niwa-plugins-test--workspace workspace-root))
         (workspace-niwa (file-name-concat
                             workspace-root ".niwa" "plugins" "repo"))
         (workspace-agents (file-name-concat
                            workspace-root ".agents" "plugins" "repo"))
         (global-niwa (file-name-concat
                          niwa-user-dir "plugins" "repo"))
         (global-agents (file-name-concat
                         (niwa-plugins-dir) "repo")))
    (unwind-protect
        (progn
          (dolist (root (list workspace-niwa workspace-agents
                              global-niwa global-agents))
            (niwa-plugins-test--write-manifest
             root "{\"name\":\"demo\"}"))
          (let ((plugin (car (niwa-plugins-list workspace))))
            (should (equal (file-name-as-directory
                            (expand-file-name workspace-niwa))
                           (niwa-plugin-root plugin)))
            (should
             (equal (mapcar #'niwa-plugin-root
                            (niwa-plugin-shadowed plugin))
                    (mapcar (lambda (root)
                              (file-name-as-directory
                               (expand-file-name root)))
                            (list workspace-agents
                                  global-niwa
                                  global-agents))))))
      (delete-directory workspace-root t)))

  :doc "falls back to plugin directory basename and nil optional fields"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest root "{}")
    (let ((plugin (car (niwa-plugins-list))))
      (should (equal "repo" (niwa-plugin-name plugin)))
      (should-not (niwa-plugin-version plugin))
      (should-not (niwa-plugin-description plugin))
      (should-not (niwa-plugin-skills-dir plugin))
      (should-not (niwa-plugin-hooks-file plugin))))

  :doc "rejects unsafe manifest plugin names"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"../x\"}")
    (should-not (niwa-plugins-list)))

  :doc "rejects plugin names reserved for local skill sources"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"user\"}")
    (should-not (niwa-plugins-list)))

  :doc "ignores manifest paths that escape the plugin root"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"skills\":\"../skills\",\"hooks\":\"/tmp/hooks.json\"}")
    (let ((plugin (car (niwa-plugins-list))))
      (should (equal "demo" (niwa-plugin-name plugin)))
      (should-not (niwa-plugin-skills-dir plugin))
      (should-not (niwa-plugin-hooks-file plugin))))

  :doc "ignores symlinked manifest paths that escape the plugin root"
  (let* ((root (niwa-plugins-test--plugin-root user-dir "repo"))
         (outside (make-temp-file "niwa-plugins-outside-" t))
         (link (file-name-concat root "skills")))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-symbolic-link outside link)
          (niwa-plugins-test--write-manifest
           root
           "{\"name\":\"demo\",\"skills\":\"skills\"}")
          (let ((plugin (car (niwa-plugins-list))))
            (should (equal "demo" (niwa-plugin-name plugin)))
            (should-not (niwa-plugin-skills-dir plugin))))
      (delete-directory outside t))))


(niwa-deftest niwa-plugins--collect
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-collect-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-collect-ws-" t)))
           (workspace (niwa-plugins-test--workspace workspace-root))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (niwa-plugin-extra-roots nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "`niwa-plugins--collect' selects duplicate winners and keeps metadata errors separate"
  (let ((root-a (niwa-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (niwa-plugins-test--plugin-root user-dir "repo-b"))
        (bad-root (niwa-plugins-test--plugin-root user-dir "bad"))
        (broken-root (niwa-plugins-test--plugin-root user-dir "broken")))
    (niwa-plugins-test--write-manifest root-a "{\"name\":\"demo\"}")
    (niwa-plugins-test--write-manifest root-b "{\"name\":\"demo\"}")
    (niwa-plugins-test--write-manifest bad-root "{\"name\":\"../x\"}")
    (niwa-plugins-test--write-manifest broken-root "{")
    (let* ((collection (niwa-plugins--collect workspace))
           (winners (plist-get collection :winners))
           (errors (plist-get collection :errors))
           (plugin (car winners)))
      (should (equal '("demo")
                     (mapcar #'niwa-plugin-name winners)))
      (should (equal (file-name-as-directory (expand-file-name root-a))
                     (niwa-plugin-root plugin)))
      (should (= 1 (length (niwa-plugin-shadowed plugin))))
      (should (= 2 (length errors)))
      (should (= 2 (plist-get collection :error-count)))
      (should (equal '("demo")
                     (mapcar #'niwa-plugin-name
                             (niwa-plugins-list workspace))))
      (let ((items (niwa-plugins-items workspace)))
        (should (= 3 (length items)))
        (should (cl-some #'niwa-plugin-error-p items)))))

  :doc "`niwa-plugins--collect' counts winners only and enabled plugins by active source"
  (let ((one-root (niwa-plugins-test--plugin-root user-dir "repo-one"))
        (two-root (niwa-plugins-test--plugin-root user-dir "repo-two"))
        (two-shadow (niwa-plugins-test--plugin-root user-dir "repo-two-shadow"))
        (bad-root (niwa-plugins-test--plugin-root user-dir "bad"))
        (local-one (file-name-concat
                    workspace-root ".niwa" "plugins" "repo-one")))
    (niwa-plugins-test--write-manifest one-root "{\"name\":\"one\"}")
    (niwa-plugins-test--write-manifest two-root "{\"name\":\"two\"}")
    (niwa-plugins-test--write-manifest two-shadow "{\"name\":\"two\"}")
    (niwa-plugins-test--write-manifest bad-root "{\"name\":\"../x\"}")
    (niwa-plugins-enable "one" workspace)
    (let ((collection (niwa-plugins--collect workspace)))
      (should (= 1 (plist-get collection :enabled-count)))
      (should (= 2 (plist-get collection :total-count)))
      (should (= 1 (plist-get collection :error-count)))
      (should (equal "1/2" (niwa-plugins-count-label workspace)))
      (should (equal '("one")
                     (mapcar #'niwa-plugin-name
                             (niwa-plugins-enabled workspace)))))
    (niwa-plugins-test--write-manifest local-one "{\"name\":\"one\"}")
    (let ((collection (niwa-plugins--collect workspace)))
      (should (= 0 (plist-get collection :enabled-count)))
      (should (= 2 (plist-get collection :total-count)))
      (should (= 1 (plist-get collection :error-count)))
      (should (equal "0/2" (niwa-plugins-count-label workspace)))
      (should-not (niwa-plugins-enabled workspace)))))


;;
;;; State

(niwa-deftest niwa-plugins--write-state ()
  ,test
  (test)
  :doc "a write that dies after truncating leaves the previous state"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plugins-local-state-" t)))
         (workspace (niwa-plugins-test--workspace root))
         (real (symbol-function 'write-region)))
    (unwind-protect
        (progn
          (niwa-plugins--write-state '(("demo" :enabled t)) workspace)
          (cl-letf (((symbol-function 'write-region)
                     (lambda (_start _end filename &rest _)
                       ;; A write that truncates its target and then dies,
                       ;; as ENOSPC or a killed Emacs would.
                       (funcall real "" nil filename nil 'silent)
                       (error "Disk full"))))
            (should-error
             (niwa-plugins--write-state '(("other" :enabled t)) workspace)))
          (should (equal '(("demo" :enabled t))
                         (niwa-plugins--read-state workspace))))
      (delete-directory root t)))

  :doc "a completed write leaves no staging file beside the state"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plugins-local-state-" t)))
         (workspace (niwa-plugins-test--workspace root)))
    (unwind-protect
        (progn
          (niwa-plugins--write-state '(("demo" :enabled t)) workspace)
          (should (equal '("plugins.el")
                         (directory-files
                          (niwa-workspace-state-dir workspace) nil
                          directory-files-no-dot-files-regexp))))
      (delete-directory root t)))

  :doc "remote plugin state refuses to write without its live session"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plugins-remote-state-" t)))
         (remote-root (concat "/niwamock:plugins:" root))
         (workspace (niwa-plugins-test--workspace remote-root))
         (niwa--session nil))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("plugins")
          (should-error
           (niwa-plugins--write-state
            '(("demo" :enabled t)) workspace)
           :type 'user-error)
          (should-not
           (file-exists-p (file-name-concat root ".niwa" "plugins.el"))))
      (delete-directory root t)))

  :doc "remote plugin state discloses, leases, and publishes on the target"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plugins-remote-state-" t)))
         (remote-root (concat "/niwamock:plugins:" root))
         ;; Disclosure, lease, and publication are the portable authority,
         ;; which only a project workspace carries.
         (workspace (niwa-workspace--create
                     :type 'project :id remote-root :root remote-root
                     :name "test"))
         (session nil)
         (niwa--session nil)
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         prompts)
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("plugins")
          ;; The mock method only resolves inside this block, so the
          ;; session has to be built here for a usable target.
          (setq session (niwa-session-create "main" workspace remote-root)
                niwa--session session)
          (setf (niwa-execution-target-readiness
                 (niwa-session-execution-target session))
                '(:status ready))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (prompt)
                             (push prompt prompts)
                             t)))
                  (niwa-plugins--write-state
                   '(("demo" :enabled t)) workspace))
                (should (= 1 (length prompts)))
                (should (niwa-session-save-path session))
                (should (eq 'owned
                            (plist-get (niwa-session-lease session)
                                       :state)))
                (should
                 (equal '(("demo" :enabled t))
                        (niwa-plugins-test--read-state workspace))))
            (when (niwa-session-save-path session)
              (niwa-session-durability-lease-release
               (niwa-session-save-path session) session))))
      (delete-directory root t))))

(niwa-deftest niwa-plugins-enabled
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-state-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-workspace-" t)))
           (session (niwa-plugins-test--session workspace-root))
           (workspace (niwa-session-workspace session))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins")))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "keeps discovered plugins disabled by default in each workspace"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (niwa-plugins-find "demo" workspace))
    (should-not (niwa-plugins-enabled workspace)))

  :doc "keeps activation isolated between workspaces"
  (let* ((root (niwa-plugins-test--plugin-root user-dir "repo"))
         (other-root (file-name-as-directory
                      (make-temp-file "niwa-plugins-other-ws-" t)))
         (other-workspace (niwa-plugins-test--workspace other-root)))
    (unwind-protect
        (progn
          (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (niwa-plugins-enable "demo" workspace)
          (should (equal '("demo")
                         (mapcar #'niwa-plugin-name
                                 (niwa-plugins-enabled workspace))))
          (should-not (niwa-plugins-enabled other-workspace)))
      (delete-directory other-root t)))

  :doc "reports enabled/total plugin count"
  (let ((root-a (niwa-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (niwa-plugins-test--plugin-root user-dir "repo-b")))
    (niwa-plugins-test--write-manifest root-a "{\"name\":\"one\"}")
    (niwa-plugins-test--write-manifest root-b "{\"name\":\"two\"}")
    (niwa-plugins-enable "one" workspace)
    (should (equal "1/2" (niwa-plugins-count-label workspace)))
    (should (equal "0/0" (niwa-plugins-count-label nil))))

  :doc "does not move activation when a higher-precedence duplicate appears"
  (let* ((global-root (niwa-plugins-test--plugin-root user-dir "repo"))
         (local-root (file-name-concat
                      workspace-root ".niwa" "plugins" "repo")))
    (niwa-plugins-test--write-manifest global-root "{\"name\":\"demo\"}")
    (niwa-plugins-enable "demo" workspace)
    (niwa-plugins-test--write-manifest local-root "{\"name\":\"demo\"}")
    (should-not (niwa-plugins-enabled workspace))
    (should (niwa-plugins-active-shadowed-source
             (niwa-plugins-find "demo" workspace) workspace)))

  :doc "requires confirmation before switching activation to the winning duplicate"
  (let* ((global-root (niwa-plugins-test--plugin-root user-dir "repo"))
         (local-root (file-name-concat
                      workspace-root ".niwa" "plugins" "repo")))
    (niwa-plugins-test--write-manifest global-root "{\"name\":\"demo\"}")
    (niwa-plugins-enable "demo" workspace)
    (niwa-plugins-test--write-manifest local-root "{\"name\":\"demo\"}")
    (let (prompts)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   nil)))
        (should-not (niwa-plugins-enable "demo" workspace)))
      (should (= 1 (length prompts)))
      (should (string-match-p "Switch plugin demo activation"
                              (car prompts))))
    (let ((state (niwa-plugins-test--state-plist workspace "demo")))
      (should (niwa-plugins-same-root-p
               global-root (plist-get state :source-root) workspace)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (niwa-plugins-enable "demo" workspace)))
    (let ((state (niwa-plugins-test--state-plist workspace "demo")))
      (should (plist-get state :enabled))
      (should (niwa-plugins-same-root-p
               local-root (plist-get state :source-root) workspace))))

  :doc "hook consent summary includes identity, handlers, and runtime data"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo"))
        prompts)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo hi\"}]}]}}"))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"version\":\"1.2.3\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 t)))
      (should (niwa-plugins-enable "demo" workspace)))
    (let ((prompt (car prompts)))
      (should (string-match-p "Enable plugin demo hooks\\?" prompt))
      (should (string-match-p "Version: 1.2.3" prompt))
      (should (string-match-p "Events: PreToolUse" prompt))
      (should (string-match-p
               "Handlers: PreToolUse \\[Bash\\]: command echo hi"
               prompt))
      (should (string-match-p
               (regexp-quote (niwa-plugins-plugin-data-dir
                              "demo" workspace))
               prompt))))

  :doc "binds hook consent to matcher scope while ignoring group order"
  (let* ((root (niwa-plugins-test--plugin-root user-dir "repo"))
         (hooks-file (file-name-concat root "hooks" "hooks.json")))
    (make-directory (file-name-directory hooks-file) t)
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (with-temp-file hooks-file
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Read\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo demo\"}]}]}}"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (niwa-plugins-enable "demo" workspace)))
    (let ((exact-fingerprint
           (plist-get (niwa-plugins-test--state-plist workspace "demo")
                      :hooks-fingerprint)))
      (dolist (matcher '("*" "B.*"))
        (with-temp-file hooks-file
          (insert (format
                   (concat "{\"hooks\":{\"PreToolUse\":[{\"matcher\":%S,"
                           "\"hooks\":[{\"type\":\"command\","
                           "\"command\":\"echo demo\"}]}]}}")
                   matcher)))
        (let ((plugin (niwa-plugins-find "demo" workspace)))
          (should-not
           (equal exact-fingerprint
                  (niwa-plugins--hook-fingerprint plugin workspace)))
          (should-not (niwa-plugins-hooks-enabled-p plugin workspace)))))
    (with-temp-file hooks-file
      (insert (concat
               "{\"hooks\":{\"PreToolUse\":["
               "{\"matcher\":\"Read\",\"hooks\":[{\"type\":\"command\","
               "\"command\":\"echo read\"}]},"
               "{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\","
               "\"command\":\"echo bash\"}]}]}}")))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (niwa-plugins-enable "demo" workspace)))
    (let ((ordered-fingerprint
           (plist-get (niwa-plugins-test--state-plist workspace "demo")
                      :hooks-fingerprint)))
      (with-temp-file hooks-file
        (insert (concat
                 "{\"hooks\":{\"PreToolUse\":["
                 "{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\","
                 "\"command\":\"echo bash\"}]},"
                 "{\"matcher\":\"Read\",\"hooks\":[{\"type\":\"command\","
                 "\"command\":\"echo read\"}]}]}}")))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should (equal ordered-fingerprint
                       (niwa-plugins--hook-fingerprint plugin workspace)))
        (should (niwa-plugins-hooks-enabled-p plugin workspace)))))

  :doc "keeps project activation and hook consent across equivalent TRAMP aliases"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-plugins-alias-" t)))
         (root-a (concat "/niwamock:plugin-a:" root))
         (root-b (concat "/niwamock:plugin-b:" root))
         (workspace-a (niwa-plugins-test--workspace root-a))
         (workspace-b (niwa-plugins-test--workspace root-b))
         session-a
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("plugin-a" "plugin-b")
          (setq session-a (niwa-session-create "main" workspace-a root-a))
          (unwind-protect
              (let* ((plugin-root
                      (file-name-concat
                       root-a ".niwa" "plugins" "repo"))
                     (hooks-file
                      (file-name-concat plugin-root "hooks" "hooks.json")))
                (make-directory (file-name-directory hooks-file) t)
                (with-temp-file hooks-file
                  (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                          "\"hooks\":[{\"type\":\"command\","
                          "\"command\":\"echo alias\"}]}]}}"))
                (niwa-plugins-test--write-manifest
                 plugin-root
                 "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
                (let ((niwa--session session-a))
                  (cl-letf (((symbol-function 'yes-or-no-p)
                             (lambda (_prompt) t)))
                    (should (niwa-plugins-enable "demo" workspace-a))))
                (let* ((state (niwa-plugins-test--state-plist
                               workspace-a "demo"))
                       (fingerprint (plist-get state :hooks-fingerprint))
                       (enabled-b (niwa-plugins-enabled workspace-b))
                       (plugin-b (car enabled-b)))
                  (should (equal ".niwa/plugins/repo/"
                                 (plist-get state :source-root)))
                  (should (stringp fingerprint))
                  (should (equal '("demo")
                                 (mapcar #'niwa-plugin-name enabled-b)))
                  (should (niwa-plugin-hooks-enabled-p plugin-b))
                  (should (niwa-plugins-hooks-enabled-p
                           plugin-b workspace-b))
                  (should-not (niwa-plugins-hooks-stale-p
                               plugin-b workspace-b))
                  (should-not
                   (niwa-plugins-pending-consent workspace-b))
                  (should (equal fingerprint
                                 (niwa-plugins--hook-fingerprint
                                  plugin-b workspace-b)))))
            (when (niwa-session-save-path session-a)
              (niwa-session-durability-lease-release
               (niwa-session-save-path session-a) session-a))))
      (delete-directory root t)))

)

(niwa-deftest niwa-plugins--read-state
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-read-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-read-ws-" t)))
           (workspace (niwa-plugins-test--workspace workspace-root))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (niwa-plugins-test--read-eval-ran nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "does not evaluate read-time forms from the state file"
  (progn
    (make-directory (file-name-directory (niwa-plugins-state-file workspace)) t)
    (with-temp-file (niwa-plugins-state-file workspace)
      (insert "#.(setq niwa-plugins-test--read-eval-ran t)"))
    (should-not (niwa-plugins--read-state workspace))
    (should-not niwa-plugins-test--read-eval-ran)))

(niwa-deftest niwa-plugins-plugin-data-dir
  (:vars* ((workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-root-ws-" t)))
           (workspace (niwa-plugins-test--workspace workspace-root)))
   :after-each (delete-directory workspace-root t))
  ,test
  (test)
  :doc "locates runtime data by workspace and plugin name"
  (should (equal (file-name-concat workspace-root ".niwa"
                                  "state" "plugin-data" "demo")
                 (niwa-plugins-plugin-data-dir "demo" workspace)))

  :doc "expands the workspace root before locating runtime data"
  (let* ((root "~/niwa-plugins-ws/")
         (workspace (niwa-plugins-test--workspace root)))
    (should (equal (file-name-concat (expand-file-name root) ".niwa"
                                     "state" "plugin-data" "demo")
                   (niwa-plugins-plugin-data-dir "demo" workspace)))))
(provide 'test-niwa-plugin-registry)
;;; test-niwa-plugin-registry.el ends here
