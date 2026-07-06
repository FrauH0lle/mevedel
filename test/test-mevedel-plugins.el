;;; test-mevedel-plugins.el --- Tests for mevedel-plugins.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-cockpit)
(require 'mevedel-menu)
(require 'mevedel-plugins)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(defvar mevedel--session)
(defvar mevedel--workspace)


;;
;;; Helpers

(defun mevedel-plugins-test--plugin-root (user-dir repo)
  "Return test plugin root for REPO under USER-DIR."
  (file-name-concat user-dir "plugins" repo))

(defun mevedel-plugins-test--github-plugin-root (user-dir owner repo)
  "Return GitHub install plugin root for OWNER and REPO under USER-DIR."
  (file-name-concat user-dir "plugins" "github.com" owner repo))

(defun mevedel-plugins-test--github-install-root (owner repo)
  "Return new-install plugin root for OWNER and REPO."
  (file-name-concat (mevedel-plugins-dir) "github.com" owner repo))

(defun mevedel-plugins-test--write-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun mevedel-plugins-test--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'test :id root :root root :name "test"))

(defun mevedel-plugins-test--session (root)
  "Return a test session rooted at ROOT."
  (let ((workspace (mevedel-plugins-test--workspace root)))
    (mevedel-session-create "main" workspace root)))

(defun mevedel-plugins-test--slash (session args)
  "Run `/plugin' ARGS as SESSION."
  (let ((mevedel--session session))
    (mevedel-plugins-slash-command args)))

(defun mevedel-plugins-test--read-state (workspace)
  "Read test plugin state under WORKSPACE."
  (let ((file (mevedel-plugins-state-file workspace)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun mevedel-plugins-test--state-plist (workspace name)
  "Return persisted test state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (mevedel-plugins-test--read-state workspace))))

(defun mevedel-plugins-test--plugin-line (plugin &optional workspace)
  "Return one compact state line for PLUGIN in WORKSPACE."
  (let ((shadowed (mevedel-plugin-shadowed plugin)))
    (format "%s%s enabled:%s hooks:%s events:%s skills:%d source:%s%s"
            (mevedel-plugin-name plugin)
            (if-let* ((version (mevedel-plugin-version plugin)))
                (format " %s" version)
              "")
            (if (mevedel-plugins--enabled-p plugin workspace) "on" "off")
            (mevedel-plugins--hooks-status plugin workspace)
            (if-let* ((events (mevedel-plugins--hook-rule-events plugin)))
                (string-join events ",")
              "none")
            (mevedel-plugins--skill-count plugin)
            (mevedel-plugins--plugin-source-label plugin)
            (if shadowed
                (format " shadowed:%d" (length shadowed))
              ""))))

(defun mevedel-plugins-test--list-string (&optional workspace)
  "Return rendered plugin rows for WORKSPACE."
  (let ((plugins (mevedel-plugins-list workspace)))
    (if plugins
        (mapconcat
         (lambda (plugin)
           (string-join
            (cons (mevedel-plugins-test--plugin-line plugin workspace)
                  (mevedel-plugins--shadowed-lines plugin workspace))
            "\n"))
         plugins
         "\n")
      "No plugins installed.")))

(defvar mevedel-plugins-test--read-eval-ran nil)
(defvar mevedel-plugins-test--owner-buffers nil)

(defun mevedel-plugins-test--context
    (workspace view-buffer data-buffer &optional origin-buffer)
  "Return a plugin cockpit context for WORKSPACE."
  (list :view-buffer view-buffer
        :data-buffer data-buffer
        :origin-buffer (or origin-buffer view-buffer)
        :workspace workspace))

(defun mevedel-plugins-test--list-open (workspace)
  "Open the plugin cockpit for WORKSPACE with live owner buffers."
  (let ((view-buffer (generate-new-buffer " *plugin-test-view*"))
        (data-buffer (generate-new-buffer " *plugin-test-data*")))
    (push view-buffer mevedel-plugins-test--owner-buffers)
    (push data-buffer mevedel-plugins-test--owner-buffers)
    (mevedel-plugins-list-open
     (mevedel-plugins-test--context
      workspace view-buffer data-buffer view-buffer))))


;;
;;; Discovery

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
         (global-mevedel (mevedel-plugins-test--plugin-root
                          user-dir "repo"))
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
      (let ((items (mevedel-plugins-list--items workspace)))
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
    (should-not (mevedel-plugins-enabled workspace))
    (should (string-match-p "demo enabled:off hooks:none"
                            (mevedel-plugins-test--list-string workspace))))

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
    (should (string-match-p
             (regexp-quote "shadowed active:")
             (mevedel-plugins-test--list-string workspace))))

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
      (should (mevedel-plugins--same-root-p
               global-root (plist-get state :source-root))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (mevedel-plugins-enable "demo" workspace)))
    (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
      (should (plist-get state :enabled))
      (should (mevedel-plugins--same-root-p
               local-root (plist-get state :source-root)))))

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
      (should (string-match-p "Handlers: command echo hi" prompt))
      (should (string-match-p
               (regexp-quote (mevedel-plugins-plugin-data-dir
                              "demo" workspace))
               prompt))))

  :doc "persists enable and hook toggles"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin demo."
                     (mevedel-plugins-test--slash
                      session "enable demo")))
      (should (equal "Disabled plugin demo."
                     (mevedel-plugins-test--slash session "disable demo")))
      (should-not (mevedel-plugins-enabled workspace))
      (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (mevedel-plugins--same-root-p
                 root (plist-get state :source-root))))
      (should (equal "Enabled plugin demo."
                     (mevedel-plugins-test--slash
                      session "enable demo")))
      (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
        (should (plist-get state :enabled))
        (should (plist-get state :hooks-enabled))
        (should (mevedel-plugins--same-root-p
                 root (plist-get state :source-root)))
        (should (stringp (plist-get state :hooks-fingerprint))))
      (should (string-match-p "demo enabled:on hooks:on"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "supports plan-compatible hook on and off aliases"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin demo."
                     (mevedel-plugins-test--slash session "enable demo")))
      (should (string-match-p "demo enabled:on hooks:on"
                              (mevedel-plugins-test--list-string workspace)))
      (should (equal "Disabled hooks for plugin demo."
                     (mevedel-plugins-test--slash session "hooks demo off")))
      (should (string-match-p "demo enabled:on hooks:off"
                              (mevedel-plugins-test--list-string workspace)))
      (should (equal "Enabled hooks for plugin demo."
                     (mevedel-plugins-test--slash session "hooks demo on")))
      (should (string-match-p "demo enabled:on hooks:on"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "enabling hooks for plugin named superpowers does not queue bootstrap"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"superpowers\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin superpowers."
                     (mevedel-plugins-test--slash
                      session "enable superpowers"))))
    (should-not (mevedel-session-hook-context-pending session))))

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


;;
;;; Plugin list buffer

(mevedel-deftest mevedel-plugins-list-buffer
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-buffer-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-buffer-ws-" t)))
           (workspace (mevedel-plugins-test--workspace workspace-root))
           (mevedel-user-dir user-dir)
           (mevedel-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins")))
   :after-each (progn
                 (when-let* ((buffer (get-buffer
                                      mevedel-plugins-list-buffer-name)))
                   (kill-buffer buffer))
                 (when-let* ((buffer (get-buffer
                                      "*mevedel plugin details*")))
                   (kill-buffer buffer))
                 (when-let* ((buffer (get-buffer
                                      mevedel-plugins-help-buffer-name)))
                   (kill-buffer buffer))
                 (mapc (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer)))
                       mevedel-plugins-test--owner-buffers)
                 (setq mevedel-plugins-test--owner-buffers nil)
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "selects the displayed plugin cockpit window"
  (let ((buffer (mevedel-plugins-test--list-open workspace)))
    (should (eq buffer (window-buffer (selected-window)))))

  :doc "renders visible plugin rows and details"
  (let ((shadow-root (file-name-concat (mevedel-plugins-dir) "shadow"))
        (winning-root (mevedel-plugins-test--plugin-root user-dir "winner")))
    (mevedel-plugins-test--write-manifest shadow-root "{\"name\":\"demo\"}")
    (mevedel-plugins-enable "demo" workspace)
    (make-directory (file-name-concat winning-root "hooks") t)
    (with-temp-file (file-name-concat winning-root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo row\"}]}]}}"))
    (mevedel-plugins-test--write-manifest
     winning-root
     "{\"name\":\"demo\",\"version\":\"1.0\",\"hooks\":\"hooks/hooks.json\"}")
    (mevedel-plugins-test--list-open workspace)
    (with-current-buffer mevedel-plugins-list-buffer-name
      (let ((rows (mevedel-test-tabulated-entries-cells)))
        (should (= 1 (length rows)))
        (should (equal (cdr (assoc "demo" rows))
                       (list "*" "demo" "1.0" "off" "off" "0"
                             (abbreviate-file-name
                              (file-name-as-directory
                               (expand-file-name winning-root)))))))
      (should (equal "Enable plugin"
                     (mevedel-plugins-list--activation-label workspace)))
      (mevedel-plugins-list-details))
    (with-current-buffer "*mevedel plugin details*"
      (let ((details (buffer-string)))
        (should (string-match-p "Name:     demo" details))
        (should (string-match-p "Version: 1.0" details))
        (should (string-match-p "Events:   PreToolUse" details))
        (should (string-match-p "Manifest:" details))
        (should (string-match-p "Shadowed sources:" details))
        (should (string-match-p "shadowed active:" details))
        (should (string-match-p "Handlers: command echo row" details))
        (should (string-match-p
                 (regexp-quote (mevedel-plugins-plugin-data-dir
                                "demo" workspace))
                 details)))))

  :doc "renders an empty plugin list without error"
  (progn
    (mevedel-plugins-test--list-open workspace)
    (with-current-buffer mevedel-plugins-list-buffer-name
      (should-not tabulated-list-entries)
      (should (string-match-p "0/0 enabled"
                              (mevedel-cockpit-surface-header-line)))))

  :doc "opens generated plugin cockpit help"
  (progn
    (mevedel-plugins-list-help)
    (with-current-buffer mevedel-plugins-help-buffer-name
      (should (string-match-p "RET  Show selected plugin details"
                              (buffer-string)))
      (should (string-match-p "e    Enable or disable selected plugin"
                              (buffer-string)))
      (should (string-match-p "/plugin enable NAME"
                              (buffer-string)))))

  :doc "renders malformed manifests as visible error rows"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "bad")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"../x\"}")
    (should-not (mevedel-plugins-list workspace))
    (mevedel-plugins-test--list-open workspace)
    (with-current-buffer mevedel-plugins-list-buffer-name
      (let* ((rows (mevedel-test-tabulated-entries-cells))
             (root (file-name-as-directory (expand-file-name root))))
        (should (= 1 (length rows)))
        (should (equal (cdr (assoc (concat "error:" root) rows))
                       (list "!" "../x" "" "error" "" ""
                             (abbreviate-file-name root)))))
      (mevedel-plugins-list-details))
    (with-current-buffer "*mevedel plugin details*"
      (let ((details (buffer-string)))
        (should (string-match-p "Plugin metadata error" details))
        (should (string-match-p "Unsafe plugin name: ../x" details))
        (should (string-match-p (regexp-quote root) details)))))

  :doc "dispatches adaptive activation and hook override actions at point"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo action\"}]}]}}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (mevedel-plugins-test--list-open workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer mevedel-plugins-list-buffer-name
        (let ((list-workspace
               (mevedel-plugins-list--workspace
                (mevedel-cockpit-surface-context))))
          (mevedel-plugins-list-toggle-enabled)
          (should (equal "Disable plugin"
                         (mevedel-plugins-list--activation-label
                          list-workspace)))
          (should (mevedel-plugins--enabled-p
                   (mevedel-plugins--find "demo" list-workspace)
                   list-workspace))
          (mevedel-plugins-list-toggle-hooks)
          (should (equal "off"
                         (mevedel-plugins--hooks-status
                          (mevedel-plugins--find "demo" list-workspace)
                          list-workspace)))
          (mevedel-plugins-list-toggle-hooks)
          (should (equal "on"
                         (mevedel-plugins--hooks-status
                          (mevedel-plugins--find "demo" list-workspace)
                          list-workspace)))
          (mevedel-plugins-list-toggle-enabled)
          (should (equal "Enable plugin"
                         (mevedel-plugins-list--activation-label
                          list-workspace)))))))

  :doc "dispatches update and remove actions at point"
  (let ((root (mevedel-plugins-test--github-install-root "owner" "repo"))
        calls)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (mevedel-plugins-test--list-open workspace)
    (cl-letf ((mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (with-current-buffer mevedel-plugins-list-buffer-name
        (mevedel-plugins-list-update)))
    (should (equal (list (list (file-name-as-directory
                                (expand-file-name root))
                               (list "pull" "--ff-only")))
                   calls))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer mevedel-plugins-list-buffer-name
        (mevedel-plugins-list-remove)))
    (should-not (file-exists-p root)))

  :doc "installs, refreshes, and selects the newly installed plugin"
  (let (calls)
    (mevedel-plugins-test--list-open workspace)
    (cl-letf ((mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (mevedel-plugins-test--write-manifest
                    dest "{\"name\":\"fresh\"}"))
                 (list 0 ""))))
      (with-current-buffer mevedel-plugins-list-buffer-name
        (mevedel-plugins-list-install "owner/fresh")
        (should (assoc "fresh"
                       (mevedel-test-tabulated-entries-cells)))))
    (should (= 1 (length calls))))

  :doc "mutation actions require live owners before side effects"
  (let ((root (mevedel-plugins-test--github-install-root "owner" "repo"))
        (view-buffer (generate-new-buffer " *plugin-action-view*"))
        (data-buffer (generate-new-buffer " *plugin-action-data*"))
        calls)
    (unwind-protect
        (progn
          (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (mevedel-plugins-list-open
           (mevedel-plugins-test--context
            workspace view-buffer data-buffer view-buffer))
          (kill-buffer data-buffer)
          (cl-letf ((mevedel-plugins-git-executor
                     (lambda (_directory _args)
                       (push t calls)
                       (list 0 ""))))
            (with-current-buffer mevedel-plugins-list-buffer-name
              (should-error (mevedel-plugins-list-toggle-enabled)
                            :type 'user-error)
              (should-error (mevedel-plugins-list-update)
                            :type 'user-error)
              (should-error (mevedel-plugins-list-install "owner/fresh")
                            :type 'user-error)))
          (should-not calls)
          (should-not (mevedel-plugins-enabled workspace)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))


;;
;;; Slash command

(mevedel-deftest mevedel-plugins-slash-command
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-slash-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "mevedel-plugins-slash-ws-" t)))
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
  :doc "returns user-facing strings for bad input"
  (should (equal "Unknown plugin: missing."
                 (mevedel-plugins-test--slash session "enable missing")))
  (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                         "GitHub repository.")
                 (mevedel-plugins-test--slash
                  session "install https://example.com/x.git")))
  (let ((mevedel-plugins-git-executor
         (lambda (_directory _args)
           (ert-fail "git should not run for unsafe plugin target"))))
    (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                           "GitHub repository.")
                   (mevedel-plugins-test--slash
                    session "install owner/.."))))

  :doc "stateful commands require a current workspace before side effects"
  (let ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
        calls
        (mevedel--session nil)
        (mevedel--workspace nil))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((mevedel-plugins-git-executor
           (lambda (_directory _args)
             (push t calls)
             (list 0 ""))))
      (should (equal "No current workspace for plugin state."
                     (mevedel-plugins-slash-command "enable demo")))
      (should (equal "No current workspace for plugin state."
                     (mevedel-plugins-slash-command "update demo")))
      (should (equal "No current workspace for plugin state."
                     (mevedel-plugins-slash-command "remove demo"))))
    (should-not calls)
    (should (file-exists-p root)))

  :doc "installs a GitHub plugin with stubbed git and leaves it disabled"
  (let (calls)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "install should not prompt for hooks")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (make-directory (file-name-concat dest "hooks") t)
                   (with-temp-file (file-name-concat dest "hooks" "hooks.json")
                     (insert "{}"))
                   (mevedel-plugins-test--write-manifest
                    dest
                    "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}"))
                 (list 0 ""))))
      (should (equal "Installed plugin demo."
                     (mevedel-plugins-test--slash
                      session "install owner/repo")))
      (should (equal (list (list (mevedel-plugins-dir)
                                 (list "clone" "--depth" "1"
                                       "https://github.com/owner/repo.git"
                                       (mevedel-plugins-test--github-install-root
                                        "owner" "repo"))))
                     calls))
      (should (string-match-p "demo enabled:off hooks:off"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "blank and list forms route through the plugin cockpit"
  (let (areas)
    (cl-letf (((symbol-function 'mevedel-menu-open)
               (lambda (area)
                 (push area areas))))
      (dolist (args '("" "list"))
        (should-not (mevedel-plugins-test--slash session args))))
    (should (equal '(plugins plugins) areas)))

  :doc "blank and list forms require a live cockpit-capable buffer"
  (with-temp-buffer
    (should-error (mevedel-plugins-slash-command "") :type 'user-error))

  :doc "hook-only enable requires an enabled plugin"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "disabled hook override should not prompt"))))
      (should (equal "Plugin demo is not enabled."
                     (mevedel-plugins-test--slash
                      session "hooks demo on")))))

  :doc "updates an installed plugin by manifest name"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory root t)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((mevedel-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (mevedel-plugins-test--slash session "update demo")))
      (should (equal (list (list (file-name-as-directory
                                  (expand-file-name root))
                                 (list "pull" "--ff-only")))
                     calls))))

  :doc "install refuses an existing GitHub destination without running git"
  (let ((root (mevedel-plugins-test--github-install-root "owner" "repo")))
    (make-directory root t)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((mevedel-plugins-git-executor
           (lambda (_directory _args)
             (ert-fail "install should not run git for existing destinations"))))
      (should (equal "Plugin demo is already installed; use /plugin update demo."
                     (mevedel-plugins-test--slash
                      session "install owner/repo")))))

  :doc "install refuses existing manifestless destination without suggesting update"
  (let ((root (mevedel-plugins-test--github-install-root "owner" "repo")))
    (make-directory root t)
    (let ((mevedel-plugins-git-executor
           (lambda (_directory _args)
             (ert-fail "install should not run git for existing destinations"))))
      (should
       (equal
        (format (concat "Plugin path %s already exists, but no Codex plugin "
                        "manifest was found; fix or remove it before "
                        "installing owner/repo.")
                root)
        (mevedel-plugins-test--slash session "install owner/repo")))))

  :doc "manual updates preserve existing hook consent without prompting"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo demo\"}]}]}}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (mevedel-plugins-enable "demo" workspace))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (with-temp-file (file-name-concat
                                  root "hooks" "hooks.json")
                   (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                           "\"hooks\":[{\"type\":\"command\","
                           "\"command\":\"echo demo\","
                           "\"statusMessage\":\"updated\"}]}]}}"))
                 (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (mevedel-plugins-test--slash session "update demo")))
      (should (string-match-p "demo enabled:on hooks:on"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "manual updates invalidate changed hook consent"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (mevedel-plugins-enable "demo" workspace))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (mevedel-plugins-git-executor
               (lambda (_directory _args)
                 (with-temp-file (file-name-concat
                                  root "hooks" "hooks.json")
                   (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
                           "\"hooks\":[{\"type\":\"command\","
                           "\"command\":\"echo changed\"}]}]}}"))
                 (list 0 ""))))
      (should (equal (concat "Updated plugin demo. "
                             "Hook consent is pending; open /plugin "
                             "to review.")
                     (mevedel-plugins-test--slash session "update demo")))
      (should (equal (concat "plugin hook consent pending for demo; "
                             "open /plugin to review")
                     (mevedel-plugins-pending-consent-message workspace)))
      (let (warnings messages)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &optional level _buffer-name)
                     (push (list type message level) warnings)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          (should (mevedel-plugins-notify-pending-consent workspace)))
        (should (equal 'mevedel (caar warnings)))
        (should (string-match-p "plugin hook consent pending for demo"
                                (cadar warnings)))
        (should (member (concat "mevedel: plugin hook consent pending "
                                "for demo; open /plugin to review")
                        messages)))
      (should (string-match-p "demo enabled:on hooks:needs-consent"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "manual updates preserve disabled plugin state"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (mevedel-plugins-enable "demo" workspace))
    (mevedel-plugins-disable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (mevedel-plugins-test--slash session "update demo")))
      (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (mevedel-plugins--same-root-p
                 root (plist-get state :source-root))))
      (should (string-match-p "demo enabled:off hooks:off"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "manual updates preserve prior state when manifest name changes"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory root t)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"old-name\"}")
    (mevedel-plugins-disable "old-name" workspace)
    (let ((mevedel-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (mevedel-plugins-test--write-manifest
              root
              "{\"name\":\"new-name\"}")
             (list 0 ""))))
      (should (equal "Updated plugin new-name."
                     (mevedel-plugins-test--slash
                      session "update old-name")))
      (let ((state (mevedel-plugins-test--state-plist workspace "new-name")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (mevedel-plugins--same-root-p
                 root (plist-get state :source-root))))
      (should (string-match-p "new-name enabled:off hooks:none"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "remove deletes managed plugin root and matching activation state"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (mevedel-plugins-plugin-data-dir "demo" workspace))
         refreshes)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (with-temp-file (file-name-concat data-dir "cache")
      (insert "cached"))
    (mevedel-plugins-enable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'mevedel-skills-rescan)
               (lambda () (push t refreshes))))
      (should (equal "Removed plugin demo."
                     (mevedel-plugins-test--slash session "remove demo"))))
    (should-not (file-exists-p root))
    (should (file-exists-p data-dir))
    (should-not (mevedel-plugins-test--read-state workspace))
    (should (= 1 (length refreshes))))

  :doc "uninstall is an alias for remove"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (mevedel-plugins-plugin-data-dir "demo" workspace)))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Removed plugin demo."
                     (mevedel-plugins-test--slash
                      session "uninstall demo"))))
    (should-not (file-exists-p root))
    (should (file-exists-p data-dir)))

  :doc "remove cancellation leaves plugin root, data, and state intact"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (mevedel-plugins-plugin-data-dir "demo" workspace)))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (mevedel-plugins-disable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (should (equal "Remove cancelled for plugin demo."
                     (mevedel-plugins-test--slash session "remove demo"))))
    (should (file-exists-p root))
    (should (file-exists-p data-dir))
    (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
      (should-not (plist-get state :enabled))
      (should-not (plist-get state :hooks-enabled))
      (should (mevedel-plugins--same-root-p
               root (plist-get state :source-root)))))

  :doc "remove reports unknown plugins"
  (should (equal "Unknown plugin: missing."
                 (mevedel-plugins-test--slash session "remove missing")))

  :doc "update refuses extra-root plugins"
  (let* ((extra-dir (file-name-as-directory
                     (make-temp-file "mevedel-plugins-extra-update-" t)))
         (root (file-name-concat extra-dir "repo"))
         calls)
    (unwind-protect
        (let ((mevedel-plugin-extra-roots (list extra-dir))
              (mevedel-plugins-git-executor
               (lambda (_directory _args)
                 (push t calls)
                 (list 0 ""))))
          (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (should (string-match-p
                   "Plugin demo is not managed by mevedel; update .* manually\\."
                   (mevedel-plugins-test--slash
                    session "update demo")))
          (should-not calls))
      (delete-directory extra-dir t)))

  :doc "remove refuses extra-root plugins"
  (let* ((extra-dir (file-name-as-directory
                     (make-temp-file "mevedel-plugins-extra-remove-" t)))
         (root (file-name-concat extra-dir "repo")))
    (unwind-protect
        (let ((mevedel-plugin-extra-roots (list extra-dir)))
          (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt)
                       (ert-fail "extra-root removal should not prompt"))))
            (should (string-match-p
                     "Plugin demo is not managed by mevedel; remove .* manually\\."
                     (mevedel-plugins-test--slash
                      session "remove demo"))))
          (should (file-exists-p root)))
      (delete-directory extra-dir t)))

  :doc "update refuses workspace-local plugins"
  (let ((root (file-name-concat
               workspace-root ".mevedel" "plugins" "repo"))
        calls)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((mevedel-plugins-git-executor
           (lambda (_directory _args)
             (push t calls)
             (list 0 ""))))
      (should (string-match-p
               "Plugin demo is not managed by mevedel; update .* manually\\."
               (mevedel-plugins-test--slash session "update demo")))
      (should-not calls)))

  :doc "remove refuses workspace-local plugins"
  (let ((root (file-name-concat
               workspace-root ".mevedel" "plugins" "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "workspace-local removal should not prompt"))))
      (should (string-match-p
               "Plugin demo is not managed by mevedel; remove .* manually\\."
               (mevedel-plugins-test--slash session "remove demo"))))
    (should (file-exists-p root)))

  :doc "reload returns a user-facing string"
  (should (equal "Plugin registry reloaded. No active session skills to refresh."
                 (mevedel-plugins-slash-command "reload")))

  :doc "reload reports unexpected refresh failures"
  (let (warnings)
	      (cl-letf (((symbol-function 'mevedel-skills-rescan)
	               (lambda ()
	                 (signal 'error '("refresh broke"))))
              ((symbol-function 'display-warning)
               (lambda (type message &optional level _buffer-name)
                 (push (list type message level) warnings))))
      (should (equal "Plugin registry reload failed: refresh broke."
                     (mevedel-plugins-test--slash session "reload")))
      (should (equal '((mevedel "Plugin registry refresh failed: refresh broke" :warning))
                     warnings))))

  :doc "refreshes current session skills after plugin mutations"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
        refreshes)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'mevedel-skills-rescan)
               (lambda ()
                 (push t refreshes)))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              (mevedel-plugins-git-executor
               (lambda (_directory args)
                 (when (equal (car args) "clone")
                   (let ((dest (car (last args))))
                     (make-directory dest t)
                     (mevedel-plugins-test--write-manifest
                      dest
                      "{\"name\":\"fresh\"}")))
                 (list 0 ""))))
      (mevedel-plugins-test--slash session "enable demo")
      (mevedel-plugins-test--slash session "hooks demo off")
      (mevedel-plugins-test--slash session "hooks demo on")
      (mevedel-plugins-test--slash session "disable demo")
      (mevedel-plugins-test--slash session "install owner/fresh")
      (mevedel-plugins-test--slash session "update demo")
      (mevedel-plugins-test--slash session "reload")
      (should (= 7 (length refreshes)))))

  :doc "git executor failures return a user-facing string"
	  (let ((mevedel-plugins-git-executor
	         (lambda (_directory _args)
	           (signal 'error '("git is missing")))))
    (should (equal "Failed to install plugin owner/repo: git is missing"
                   (mevedel-plugins-test--slash
                    session "install owner/repo"))))

  :doc "fresh installs without hooks do not prompt and leave plugin disabled"
  (let (calls)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "hookless install should not prompt")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (mevedel-plugins-test--write-manifest
                    dest
                    "{\"name\":\"demo\"}"))
                 (list 0 ""))))
      (should (equal "Installed plugin demo."
                     (mevedel-plugins-test--slash
                      session "install owner/repo")))
      (should (string-match-p "demo enabled:off hooks:none"
                              (mevedel-plugins-test--list-string workspace)))))

  :doc "fresh installs fail when the clone lacks a manifest"
  (let ((mevedel-plugins-git-executor
         (lambda (_directory args)
           (make-directory (car (last args)) t)
           (list 0 ""))))
    (should (equal "Failed to install plugin owner/repo: no Codex plugin manifest found."
                   (mevedel-plugins-test--slash
                    session "install owner/repo")))))


(provide 'test-mevedel-plugins)
;;; test-mevedel-plugins.el ends here
