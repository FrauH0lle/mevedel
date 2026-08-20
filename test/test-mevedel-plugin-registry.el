;;; test-mevedel-plugin-registry.el -- Plugin registry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests plugin discovery, manifest trust, and durable consent state.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-cockpit)
(require 'mevedel-execution-target)
(require 'mevedel-plugin-registry)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-plugin-test-support"))

(defvar mevedel--session)
(defvar mevedel--workspace)
(defvar mevedel-plugins-test--read-eval-ran nil)

(mevedel-deftest mevedel-plugins-list
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-" t)))
           (extra-dir (file-name-as-directory
                       (make-temp-file "mevedel-plugins-extra-" t)))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (mevedel-plugin-extra-roots nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory extra-dir t)))
  ,test
  (test)
  :doc "owns discovery in the plugin registry module"
  (should (string-suffix-p
           "mevedel-plugin-registry"
           (file-name-sans-extension
            (symbol-file 'mevedel-plugins-list 'defun))))

  :doc "discovers Codex plugin manifests and resolves manifest paths"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"version\":\"1.2.3\",\"description\":\"Demo plugin\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}")
    (let ((plugin (car (mevedel-plugins-list))))
      (should (equal "demo" (mevedel-plugin-name plugin)))
      (should (equal "1.2.3" (mevedel-plugin-version plugin)))
      (should (equal "Demo plugin" (mevedel-plugin-description plugin)))
      (should (equal (file-name-as-directory (expand-file-name root))
                     (mevedel-plugin-root plugin)))
      (should (equal (file-name-concat root "skills")
                     (mevedel-plugin-skills-dir plugin)))
      (should (equal (file-name-concat root "hooks" "hooks.json")
                     (mevedel-plugin-hooks-file plugin)))
      (should (equal (list (list :file (file-name-concat
                                        root "hooks" "hooks.json")))
                     (mevedel-plugin-hooks plugin)))))

  :doc "uses default hooks/hooks.json when the manifest omits hooks"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((plugin (car (mevedel-plugins-list))))
      (should (equal (file-name-concat root "hooks" "hooks.json")
                     (mevedel-plugin-hooks-file plugin)))
      (should (equal (list (list :file (file-name-concat
                                        root "hooks" "hooks.json")))
                     (mevedel-plugin-hooks plugin)))))

  :doc "ignores default hook files that escape the plugin root"
  (let* ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (outside (make-temp-file "mevedel-plugins-outside-hook-"))
         (hook-link (file-name-concat root "hooks" "hooks.json")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory hook-link) t)
          (make-symbolic-link outside hook-link)
          (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (let ((plugin (car (mevedel-plugins-list))))
            (should (equal "demo" (mevedel-plugin-name plugin)))
            (should-not (mevedel-plugin-hooks-file plugin))
            (should-not (mevedel-plugin-hooks plugin))))
      (delete-file outside)))

  :doc "discovers nested GitHub install paths below the install dir"
  (let ((root (mevedel-plugins-test--github-install-root "owner" "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal '("demo")
                   (mapcar #'mevedel-plugin-name
                           (mevedel-plugins-list)))))

  :doc "discovers plugin roots from extra roots without descending into plugins"
  (let* ((direct (file-name-concat extra-dir "direct"))
         (nested (file-name-concat extra-dir "group" "nested"))
         (hidden (file-name-concat direct "child")))
    (mevedel-plugins-test--write-manifest direct "{\"name\":\"direct\"}")
    (mevedel-plugins-test--write-manifest nested "{\"name\":\"nested\"}")
    (mevedel-plugins-test--write-manifest hidden "{\"name\":\"hidden\"}")
    (let ((mevedel-plugin-extra-roots (list extra-dir direct)))
      (should (equal '("direct" "nested")
                     (mapcar #'mevedel-plugin-name
                             (mevedel-plugins-list))))))

  :doc "deduplicates extra-root symlink aliases by true name"
  (let* ((root (mevedel-plugins-test--github-plugin-root
                user-dir "owner" "repo"))
         (link (file-name-concat extra-dir "repo-link")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-symbolic-link root link)
    (let ((mevedel-plugin-extra-roots (list link)))
      (should (equal '("demo")
                     (mapcar #'mevedel-plugin-name
                             (mevedel-plugins-list))))))

  :doc "keeps the highest-precedence duplicate and reports shadowed sources"
  (let ((root-a (mevedel-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (mevedel-plugins-test--plugin-root user-dir "repo-b")))
    (mevedel-plugins-test--write-manifest root-a "{\"name\":\"demo\"}")
    (mevedel-plugins-test--write-manifest root-b "{\"name\":\"demo\"}")
    (let ((plugin (car (mevedel-plugins-list))))
      (should (equal "demo" (mevedel-plugin-name plugin)))
      (should (equal (file-name-as-directory (expand-file-name root-a))
                     (mevedel-plugin-root plugin)))
      (should (= 1 (length (mevedel-plugin-shadowed plugin))))))

  :doc "applies workspace/global resource root precedence"
  (let* ((workspace-root (file-name-as-directory
                          (make-temp-file "mevedel-plugins-precedence-" t)))
         (workspace (mevedel-plugins-test--workspace workspace-root))
         (workspace-mevedel (file-name-concat
                             workspace-root ".mevedel" "plugins" "repo"))
         (workspace-agents (file-name-concat
                            workspace-root ".agents" "plugins" "repo"))
         (global-mevedel (file-name-concat
                          mevedel-user-dir "plugins" "repo"))
         (global-agents (file-name-concat
                         (mevedel-plugins-dir) "repo")))
    (unwind-protect
        (progn
          (dolist (root (list workspace-mevedel workspace-agents
                              global-mevedel global-agents))
            (mevedel-plugins-test--write-manifest
             root "{\"name\":\"demo\"}"))
          (let ((plugin (car (mevedel-plugins-list workspace))))
            (should (equal (file-name-as-directory
                            (expand-file-name workspace-mevedel))
                           (mevedel-plugin-root plugin)))
            (should
             (equal (mapcar #'mevedel-plugin-root
                            (mevedel-plugin-shadowed plugin))
                    (mapcar (lambda (root)
                              (file-name-as-directory
                               (expand-file-name root)))
                            (list workspace-agents
                                  global-mevedel
                                  global-agents))))))
      (delete-directory workspace-root t)))

  :doc "falls back to plugin directory basename and nil optional fields"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{}")
    (let ((plugin (car (mevedel-plugins-list))))
      (should (equal "repo" (mevedel-plugin-name plugin)))
      (should-not (mevedel-plugin-version plugin))
      (should-not (mevedel-plugin-description plugin))
      (should-not (mevedel-plugin-skills-dir plugin))
      (should-not (mevedel-plugin-hooks-file plugin))))

  :doc "rejects unsafe manifest plugin names"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"../x\"}")
    (should-not (mevedel-plugins-list)))

  :doc "rejects plugin names reserved for local skill sources"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"user\"}")
    (should-not (mevedel-plugins-list)))

  :doc "ignores manifest paths that escape the plugin root"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"skills\":\"../skills\",\"hooks\":\"/tmp/hooks.json\"}")
    (let ((plugin (car (mevedel-plugins-list))))
      (should (equal "demo" (mevedel-plugin-name plugin)))
      (should-not (mevedel-plugin-skills-dir plugin))
      (should-not (mevedel-plugin-hooks-file plugin))))

  :doc "ignores symlinked manifest paths that escape the plugin root"
  (let* ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (outside (make-temp-file "mevedel-plugins-outside-" t))
         (link (file-name-concat root "skills")))
    (unwind-protect
        (progn
          (make-directory root t)
          (make-symbolic-link outside link)
          (mevedel-plugins-test--write-manifest
           root
           "{\"name\":\"demo\",\"skills\":\"skills\"}")
          (let ((plugin (car (mevedel-plugins-list))))
            (should (equal "demo" (mevedel-plugin-name plugin)))
            (should-not (mevedel-plugin-skills-dir plugin))))
      (delete-directory outside t))))


(mevedel-deftest mevedel-plugins--collect
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-collect-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-collect-ws-" t)))
           (workspace (mevedel-plugins-test--workspace workspace-root))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (mevedel-plugin-extra-roots nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "`mevedel-plugins--collect' selects duplicate winners and keeps metadata errors separate"
  (let ((root-a (mevedel-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (mevedel-plugins-test--plugin-root user-dir "repo-b"))
        (bad-root (mevedel-plugins-test--plugin-root user-dir "bad"))
        (broken-root (mevedel-plugins-test--plugin-root user-dir "broken")))
    (mevedel-plugins-test--write-manifest root-a "{\"name\":\"demo\"}")
    (mevedel-plugins-test--write-manifest root-b "{\"name\":\"demo\"}")
    (mevedel-plugins-test--write-manifest bad-root "{\"name\":\"../x\"}")
    (mevedel-plugins-test--write-manifest broken-root "{")
    (let* ((collection (mevedel-plugins--collect workspace))
           (winners (plist-get collection :winners))
           (errors (plist-get collection :errors))
           (plugin (car winners)))
      (should (equal '("demo")
                     (mapcar #'mevedel-plugin-name winners)))
      (should (equal (file-name-as-directory (expand-file-name root-a))
                     (mevedel-plugin-root plugin)))
      (should (= 1 (length (mevedel-plugin-shadowed plugin))))
      (should (= 2 (length errors)))
      (should (= 2 (plist-get collection :error-count)))
      (should (equal '("demo")
                     (mapcar #'mevedel-plugin-name
                             (mevedel-plugins-list workspace))))
      (let ((items (mevedel-plugins-items workspace)))
        (should (= 3 (length items)))
        (should (cl-some #'mevedel-plugin-error-p items)))))

  :doc "`mevedel-plugins--collect' counts winners only and enabled plugins by active source"
  (let ((one-root (mevedel-plugins-test--plugin-root user-dir "repo-one"))
        (two-root (mevedel-plugins-test--plugin-root user-dir "repo-two"))
        (two-shadow (mevedel-plugins-test--plugin-root user-dir "repo-two-shadow"))
        (bad-root (mevedel-plugins-test--plugin-root user-dir "bad"))
        (local-one (file-name-concat
                    workspace-root ".mevedel" "plugins" "repo-one")))
    (mevedel-plugins-test--write-manifest one-root "{\"name\":\"one\"}")
    (mevedel-plugins-test--write-manifest two-root "{\"name\":\"two\"}")
    (mevedel-plugins-test--write-manifest two-shadow "{\"name\":\"two\"}")
    (mevedel-plugins-test--write-manifest bad-root "{\"name\":\"../x\"}")
    (mevedel-plugins-enable "one" workspace)
    (let ((collection (mevedel-plugins--collect workspace)))
      (should (= 1 (plist-get collection :enabled-count)))
      (should (= 2 (plist-get collection :total-count)))
      (should (= 1 (plist-get collection :error-count)))
      (should (equal "1/2" (mevedel-plugins-count-label workspace)))
      (should (equal '("one")
                     (mapcar #'mevedel-plugin-name
                             (mevedel-plugins-enabled workspace)))))
    (mevedel-plugins-test--write-manifest local-one "{\"name\":\"one\"}")
    (let ((collection (mevedel-plugins--collect workspace)))
      (should (= 0 (plist-get collection :enabled-count)))
      (should (= 2 (plist-get collection :total-count)))
      (should (= 1 (plist-get collection :error-count)))
      (should (equal "0/2" (mevedel-plugins-count-label workspace)))
      (should-not (mevedel-plugins-enabled workspace)))))


;;
;;; State

(mevedel-deftest mevedel-plugins--write-state ()
  ,test
  (test)
  :doc "remote plugin state refuses to write without its live session"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-plugins-remote-state-" t)))
         (remote-root (concat "/mevedelmock:plugins:" root))
         (workspace (mevedel-plugins-test--workspace remote-root))
         (mevedel--session nil))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("plugins")
          (should-error
           (mevedel-plugins--write-state
            '(("demo" :enabled t)) workspace)
           :type 'user-error)
          (should-not
           (file-exists-p (file-name-concat root ".mevedel" "plugins.el"))))
      (delete-directory root t)))

  :doc "remote plugin state discloses, leases, and publishes on the target"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-plugins-remote-state-" t)))
         (remote-root (concat "/mevedelmock:plugins:" root))
         ;; Disclosure, lease, and publication are the portable authority,
         ;; which only a project workspace carries.
         (workspace (mevedel-workspace--create
                     :type 'project :id remote-root :root remote-root
                     :name "test"))
         (session nil)
         (mevedel--session nil)
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         prompts)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("plugins")
          ;; The mock method only resolves inside this block, so the
          ;; session has to be built here for a usable target.
          (setq session (mevedel-session-create "main" workspace remote-root)
                mevedel--session session)
          (setf (mevedel-execution-target-readiness
                 (mevedel-session-execution-target session))
                '(:status ready))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (prompt)
                             (push prompt prompts)
                             t)))
                  (mevedel-plugins--write-state
                   '(("demo" :enabled t)) workspace))
                (should (= 1 (length prompts)))
                (should (mevedel-session-save-path session))
                (should (eq 'owned
                            (plist-get (mevedel-session-lease session)
                                       :state)))
                (should
                 (equal '(("demo" :enabled t))
                        (mevedel-plugins-test--read-state workspace))))
            (when (mevedel-session-save-path session)
              (mevedel-session-durability-lease-release
               (mevedel-session-save-path session) session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-plugins-enabled
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-state-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-workspace-" t)))
           (session (mevedel-plugins-test--session workspace-root))
           (workspace (mevedel-session-workspace session))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins")))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "keeps discovered plugins disabled by default in each workspace"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (mevedel-plugins-find "demo" workspace))
    (should-not (mevedel-plugins-enabled workspace)))

  :doc "keeps activation isolated between workspaces"
  (let* ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (other-root (file-name-as-directory
                      (make-temp-file "mevedel-plugins-other-ws-" t)))
         (other-workspace (mevedel-plugins-test--workspace other-root)))
    (unwind-protect
        (progn
          (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (mevedel-plugins-enable "demo" workspace)
          (should (equal '("demo")
                         (mapcar #'mevedel-plugin-name
                                 (mevedel-plugins-enabled workspace))))
          (should-not (mevedel-plugins-enabled other-workspace)))
      (delete-directory other-root t)))

  :doc "reports enabled/total plugin count"
  (let ((root-a (mevedel-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (mevedel-plugins-test--plugin-root user-dir "repo-b")))
    (mevedel-plugins-test--write-manifest root-a "{\"name\":\"one\"}")
    (mevedel-plugins-test--write-manifest root-b "{\"name\":\"two\"}")
    (mevedel-plugins-enable "one" workspace)
    (should (equal "1/2" (mevedel-plugins-count-label workspace)))
    (should (equal "0/0" (mevedel-plugins-count-label nil))))

  :doc "does not move activation when a higher-precedence duplicate appears"
  (let* ((global-root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (local-root (file-name-concat
                      workspace-root ".mevedel" "plugins" "repo")))
    (mevedel-plugins-test--write-manifest global-root "{\"name\":\"demo\"}")
    (mevedel-plugins-enable "demo" workspace)
    (mevedel-plugins-test--write-manifest local-root "{\"name\":\"demo\"}")
    (should-not (mevedel-plugins-enabled workspace))
    (should (mevedel-plugins-active-shadowed-source
             (mevedel-plugins-find "demo" workspace) workspace)))

  :doc "requires confirmation before switching activation to the winning duplicate"
  (let* ((global-root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (local-root (file-name-concat
                      workspace-root ".mevedel" "plugins" "repo")))
    (mevedel-plugins-test--write-manifest global-root "{\"name\":\"demo\"}")
    (mevedel-plugins-enable "demo" workspace)
    (mevedel-plugins-test--write-manifest local-root "{\"name\":\"demo\"}")
    (let (prompts)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (push prompt prompts)
                   nil)))
        (should-not (mevedel-plugins-enable "demo" workspace)))
      (should (= 1 (length prompts)))
      (should (string-match-p "Switch plugin demo activation"
                              (car prompts))))
    (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
      (should (mevedel-plugins-same-root-p
               global-root (plist-get state :source-root) workspace)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (mevedel-plugins-enable "demo" workspace)))
    (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
      (should (plist-get state :enabled))
      (should (mevedel-plugins-same-root-p
               local-root (plist-get state :source-root) workspace))))

  :doc "hook consent summary includes identity, handlers, and runtime data"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
        prompts)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo hi\"}]}]}}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"version\":\"1.2.3\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (prompt)
                 (push prompt prompts)
                 t)))
      (should (mevedel-plugins-enable "demo" workspace)))
    (let ((prompt (car prompts)))
      (should (string-match-p "Enable plugin demo hooks\\?" prompt))
      (should (string-match-p "Version: 1.2.3" prompt))
      (should (string-match-p "Events: PreToolUse" prompt))
      (should (string-match-p
               "Handlers: PreToolUse \\[Bash\\]: command echo hi"
               prompt))
      (should (string-match-p
               (regexp-quote (mevedel-plugins-plugin-data-dir
                              "demo" workspace))
               prompt))))

  :doc "binds hook consent to matcher scope while ignoring group order"
  (let* ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (hooks-file (file-name-concat root "hooks" "hooks.json")))
    (make-directory (file-name-directory hooks-file) t)
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (with-temp-file hooks-file
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Read\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo demo\"}]}]}}"))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (mevedel-plugins-enable "demo" workspace)))
    (let ((exact-fingerprint
           (plist-get (mevedel-plugins-test--state-plist workspace "demo")
                      :hooks-fingerprint)))
      (dolist (matcher '("*" "B.*"))
        (with-temp-file hooks-file
          (insert (format
                   (concat "{\"hooks\":{\"PreToolUse\":[{\"matcher\":%S,"
                           "\"hooks\":[{\"type\":\"command\","
                           "\"command\":\"echo demo\"}]}]}}")
                   matcher)))
        (let ((plugin (mevedel-plugins-find "demo" workspace)))
          (should-not
           (equal exact-fingerprint
                  (mevedel-plugins--hook-fingerprint plugin workspace)))
          (should-not (mevedel-plugins-hooks-enabled-p plugin workspace)))))
    (with-temp-file hooks-file
      (insert (concat
               "{\"hooks\":{\"PreToolUse\":["
               "{\"matcher\":\"Read\",\"hooks\":[{\"type\":\"command\","
               "\"command\":\"echo read\"}]},"
               "{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\","
               "\"command\":\"echo bash\"}]}]}}")))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (mevedel-plugins-enable "demo" workspace)))
    (let ((ordered-fingerprint
           (plist-get (mevedel-plugins-test--state-plist workspace "demo")
                      :hooks-fingerprint)))
      (with-temp-file hooks-file
        (insert (concat
                 "{\"hooks\":{\"PreToolUse\":["
                 "{\"matcher\":\"Bash\",\"hooks\":[{\"type\":\"command\","
                 "\"command\":\"echo bash\"}]},"
                 "{\"matcher\":\"Read\",\"hooks\":[{\"type\":\"command\","
                 "\"command\":\"echo read\"}]}]}}")))
      (let ((plugin (mevedel-plugins-find "demo" workspace)))
        (should (equal ordered-fingerprint
                       (mevedel-plugins--hook-fingerprint plugin workspace)))
        (should (mevedel-plugins-hooks-enabled-p plugin workspace)))))

  :doc "keeps project activation and hook consent across equivalent TRAMP aliases"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-plugins-alias-" t)))
         (root-a (concat "/mevedelmock:plugin-a:" root))
         (root-b (concat "/mevedelmock:plugin-b:" root))
         (workspace-a (mevedel-plugins-test--workspace root-a))
         (workspace-b (mevedel-plugins-test--workspace root-b))
         (session-a (mevedel-session-create "main" workspace-a root-a))
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("plugin-a" "plugin-b")
          (unwind-protect
              (let* ((plugin-root
                      (file-name-concat
                       root-a ".mevedel" "plugins" "repo"))
                     (hooks-file
                      (file-name-concat plugin-root "hooks" "hooks.json")))
                (make-directory (file-name-directory hooks-file) t)
                (with-temp-file hooks-file
                  (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                          "\"hooks\":[{\"type\":\"command\","
                          "\"command\":\"echo alias\"}]}]}}"))
                (mevedel-plugins-test--write-manifest
                 plugin-root
                 "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
                (let ((mevedel--session session-a))
                  (cl-letf (((symbol-function 'yes-or-no-p)
                             (lambda (_prompt) t)))
                    (should (mevedel-plugins-enable "demo" workspace-a))))
                (let* ((state (mevedel-plugins-test--state-plist
                               workspace-a "demo"))
                       (fingerprint (plist-get state :hooks-fingerprint))
                       (enabled-b (mevedel-plugins-enabled workspace-b))
                       (plugin-b (car enabled-b)))
                  (should (equal ".mevedel/plugins/repo/"
                                 (plist-get state :source-root)))
                  (should (stringp fingerprint))
                  (should (equal '("demo")
                                 (mapcar #'mevedel-plugin-name enabled-b)))
                  (should (mevedel-plugin-hooks-enabled-p plugin-b))
                  (should (mevedel-plugins-hooks-enabled-p
                           plugin-b workspace-b))
                  (should-not (mevedel-plugins-hooks-stale-p
                               plugin-b workspace-b))
                  (should-not
                   (mevedel-plugins-pending-consent workspace-b))
                  (should (equal fingerprint
                                 (mevedel-plugins--hook-fingerprint
                                  plugin-b workspace-b)))))
            (when (mevedel-session-save-path session-a)
              (mevedel-session-durability-lease-release
               (mevedel-session-save-path session-a) session-a))))
      (delete-directory root t)))

)

(mevedel-deftest mevedel-plugins--read-state
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-read-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-read-ws-" t)))
           (workspace (mevedel-plugins-test--workspace workspace-root))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (mevedel-plugins-test--read-eval-ran nil))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "does not evaluate read-time forms from the state file"
  (progn
    (make-directory (file-name-directory (mevedel-plugins-state-file workspace)) t)
    (with-temp-file (mevedel-plugins-state-file workspace)
      (insert "#.(setq mevedel-plugins-test--read-eval-ran t)"))
    (should-not (mevedel-plugins--read-state workspace))
    (should-not mevedel-plugins-test--read-eval-ran)))

(mevedel-deftest mevedel-plugins-plugin-root
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-root-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-root-ws-" t)))
           (workspace (mevedel-plugins-test--workspace workspace-root))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins")))
   :after-each (progn
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal (file-name-as-directory (expand-file-name root))
                   (mevedel-plugins-plugin-root "demo")))
    (should (equal (file-name-concat workspace-root ".mevedel"
                                     "plugin-data" "demo")
                   (mevedel-plugins-plugin-data-dir "demo" workspace))))
  (let* ((root "~/mevedel-plugins-ws/")
         (workspace (mevedel-plugins-test--workspace root)))
    (should (equal (file-name-concat (expand-file-name root) ".mevedel"
                                     "plugin-data" "demo")
                   (mevedel-plugins-plugin-data-dir "demo" workspace)))))
(provide 'test-mevedel-plugin-registry)
;;; test-mevedel-plugin-registry.el ends here
