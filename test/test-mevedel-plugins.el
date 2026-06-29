;;; test-mevedel-plugins.el --- Tests for mevedel-plugins.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-plugins)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Helpers

(defun mevedel-plugins-test--plugin-root (user-dir repo)
  "Return test plugin root for REPO under USER-DIR."
  (file-name-concat user-dir "plugins" repo))

(defun mevedel-plugins-test--github-plugin-root (user-dir owner repo)
  "Return GitHub install plugin root for OWNER and REPO under USER-DIR."
  (file-name-concat user-dir "plugins" "github.com" owner repo))

(defun mevedel-plugins-test--write-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun mevedel-plugins-test--read-state (user-dir)
  "Read test plugin state under USER-DIR."
  (with-temp-buffer
    (insert-file-contents (file-name-concat user-dir "plugins.el"))
    (read (current-buffer))))

(defvar mevedel-plugins-test--read-eval-ran nil)


;;
;;; Discovery

(mevedel-deftest mevedel-plugins-list
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-" t)))
           (extra-dir (file-name-as-directory
                       (make-temp-file "mevedel-plugins-extra-" t)))
           (mevedel-user-dir user-dir)
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
  (let ((root (mevedel-plugins-test--github-plugin-root
               user-dir "owner" "repo")))
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

  :doc "drops duplicate manifest names from discovery"
  (let ((root-a (mevedel-plugins-test--plugin-root user-dir "repo-a"))
        (root-b (mevedel-plugins-test--plugin-root user-dir "repo-b")))
    (mevedel-plugins-test--write-manifest root-a "{\"name\":\"demo\"}")
    (mevedel-plugins-test--write-manifest root-b "{\"name\":\"demo\"}")
    (should-not (mevedel-plugins-list)))

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


;;
;;; State

(mevedel-deftest mevedel-plugins-enabled
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-state-" t)))
           (mevedel-user-dir user-dir))
   :after-each (delete-directory user-dir t))
  ,test
  (test)
  :doc "enables discovered plugins by default and leaves hooks disabled"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal '("demo")
                   (mapcar #'mevedel-plugin-name
                           (mevedel-plugins-enabled))))
    (should (equal "demo skills:on hooks:off"
                   (mevedel-plugins-slash-command "list"))))

  :doc "persists enable and hook toggles"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal "Enabled hooks for plugin demo."
                   (mevedel-plugins-slash-command "hooks enable demo")))
    (should (equal "Disabled plugin demo."
                   (mevedel-plugins-slash-command "disable demo")))
    (should-not (mevedel-plugins-enabled))
    (should (equal '(("demo" :enabled nil :hooks-enabled nil))
                   (mevedel-plugins-test--read-state user-dir)))
    (should (equal "Enabled hooks for plugin demo."
                   (mevedel-plugins-slash-command "hooks enable demo")))
    (should (equal '(("demo" :enabled t :hooks-enabled t))
                   (mevedel-plugins-test--read-state user-dir)))
    (should (equal "demo skills:on hooks:on"
                   (mevedel-plugins-slash-command "list"))))

  :doc "supports plan-compatible hook on and off aliases"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal "Enabled hooks for plugin demo."
                   (mevedel-plugins-slash-command "hooks demo on")))
    (should (equal "demo skills:on hooks:on"
                   (mevedel-plugins-slash-command "list")))
    (should (equal "Disabled hooks for plugin demo."
                   (mevedel-plugins-slash-command "hooks demo off")))
    (should (equal "demo skills:on hooks:off"
                   (mevedel-plugins-slash-command "list"))))

  :doc "enabling hooks for plugin named superpowers does not queue bootstrap"
  (let* ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
         (session (mevedel-session--create :name "main")))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"superpowers\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}")
    (with-temp-buffer
      (setq-local mevedel--session session)
      (should (equal "Enabled hooks for plugin superpowers."
                     (mevedel-plugins-slash-command "hooks superpowers on")))
      (should-not (mevedel-session-hook-context-pending session)))))

(mevedel-deftest mevedel-plugins--read-state
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-read-" t)))
           (mevedel-user-dir user-dir)
           (mevedel-plugins-test--read-eval-ran nil))
   :after-each (delete-directory user-dir t))
  ,test
  (test)
  :doc "does not evaluate read-time forms from the state file"
  (progn
    (with-temp-file (file-name-concat user-dir "plugins.el")
      (insert "#.(setq mevedel-plugins-test--read-eval-ran t)"))
    (should-not (mevedel-plugins--read-state))
    (should-not mevedel-plugins-test--read-eval-ran)))

(mevedel-deftest mevedel-plugins-plugin-root
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-root-" t)))
           (mevedel-user-dir user-dir))
   :after-each (delete-directory user-dir t))
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal (file-name-as-directory (expand-file-name root))
                   (mevedel-plugins-plugin-root "demo")))
    (should (equal (file-name-concat user-dir "plugin-data" "demo")
                   (mevedel-plugins-plugin-data-dir "demo")))))


;;
;;; Slash command

(mevedel-deftest mevedel-plugins-slash-command
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "mevedel-plugins-slash-" t)))
           (mevedel-user-dir user-dir))
   :after-each (delete-directory user-dir t))
  ,test
  (test)
  :doc "returns user-facing strings for bad input"
  (should (string-match-p "Usage:" (mevedel-plugins-slash-command "")))
  (should (equal "Unknown plugin: missing."
                 (mevedel-plugins-slash-command "enable missing")))
  (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                         "GitHub repository.")
                 (mevedel-plugins-slash-command "install https://example.com/x.git")))
  (let ((mevedel-plugins-git-executor
         (lambda (_directory _args)
           (ert-fail "git should not run for unsafe plugin target"))))
    (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                           "GitHub repository.")
                   (mevedel-plugins-slash-command "install owner/.."))))

  :doc "installs a GitHub plugin with stubbed git and prompts for hooks"
  (let (calls)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
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
                     (mevedel-plugins-slash-command "install owner/repo")))
      (should (equal (list (list (mevedel-plugins-dir)
                                 (list "clone" "--depth" "1"
                                       "https://github.com/owner/repo.git"
                                       (file-name-concat
                                        user-dir "plugins" "github.com"
                                        "owner" "repo"))))
                     calls))
      (should (equal "demo skills:on hooks:on"
                     (mevedel-plugins-slash-command "list")))))

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
                     (mevedel-plugins-slash-command "update demo")))
      (should (equal (list (list (file-name-as-directory
                                  (expand-file-name root))
                                 (list "pull" "--ff-only")))
                     calls))))

  :doc "install refuses an existing GitHub destination without running git"
  (let ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo")))
    (make-directory root t)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((mevedel-plugins-git-executor
           (lambda (_directory _args)
             (ert-fail "install should not run git for existing destinations"))))
      (should (equal "Plugin demo is already installed; use /plugin update demo."
                     (mevedel-plugins-slash-command "install owner/repo")))))

  :doc "install refuses existing manifestless destination without suggesting update"
  (let ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo")))
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
        (mevedel-plugins-slash-command "install owner/repo")))))

  :doc "manual updates preserve existing hook consent without prompting"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (mevedel-plugins-enable-hooks "demo")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (mevedel-plugins-slash-command "update demo")))
      (should (equal "demo skills:on hooks:on"
                     (mevedel-plugins-slash-command "list")))))

  :doc "manual updates preserve disabled plugin state"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (mevedel-plugins-enable-hooks "demo")
    (mevedel-plugins-disable "demo")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (mevedel-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (mevedel-plugins-slash-command "update demo")))
      (should (equal '(("demo" :enabled nil :hooks-enabled nil))
                     (mevedel-plugins-test--read-state user-dir)))
      (should (equal "demo skills:off hooks:off"
                     (mevedel-plugins-slash-command "list")))))

  :doc "manual updates preserve prior state when manifest name changes"
  (let* ((root (mevedel-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory root t)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"old-name\"}")
    (mevedel-plugins-disable "old-name")
    (let ((mevedel-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (mevedel-plugins-test--write-manifest
              root
              "{\"name\":\"new-name\"}")
             (list 0 ""))))
      (should (equal "Updated plugin new-name."
                     (mevedel-plugins-slash-command "update old-name")))
      (should (equal '(("new-name" :enabled nil :hooks-enabled nil))
                     (mevedel-plugins-test--read-state user-dir)))
      (should (equal "new-name skills:off hooks:off"
                     (mevedel-plugins-slash-command "list")))))

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
                     (mevedel-plugins-slash-command "reload")))
      (should (equal '((mevedel "Plugin registry refresh failed: refresh broke" :warning))
                     warnings))))

  :doc "refreshes current session skills after plugin mutations"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo"))
        refreshes)
    (mevedel-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (cl-letf (((symbol-function 'mevedel-skills-rescan)
               (lambda ()
                 (push t refreshes)))
              (mevedel-plugins-git-executor
               (lambda (_directory args)
                 (when (equal (car args) "clone")
                   (let ((dest (car (last args))))
                     (make-directory dest t)
                     (mevedel-plugins-test--write-manifest
                      dest
                      "{\"name\":\"fresh\"}")))
                 (list 0 ""))))
      (mevedel-plugins-slash-command "enable demo")
      (mevedel-plugins-slash-command "disable demo")
      (mevedel-plugins-slash-command "hooks demo on")
      (mevedel-plugins-slash-command "hooks demo off")
      (mevedel-plugins-slash-command "install owner/fresh")
      (mevedel-plugins-slash-command "update demo")
      (mevedel-plugins-slash-command "reload")
      (should (= 7 (length refreshes)))))

  :doc "git executor failures return a user-facing string"
	  (let ((mevedel-plugins-git-executor
	         (lambda (_directory _args)
	           (signal 'error '("git is missing")))))
    (should (equal "Failed to install plugin owner/repo: git is missing"
                   (mevedel-plugins-slash-command "install owner/repo"))))

  :doc "fresh installs without hooks do not prompt and leave hooks disabled"
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
                     (mevedel-plugins-slash-command "install owner/repo")))
      (should (equal "demo skills:on hooks:off"
                     (mevedel-plugins-slash-command "list")))))

  :doc "fresh installs fail when the clone lacks a manifest"
  (let ((mevedel-plugins-git-executor
         (lambda (_directory args)
           (make-directory (car (last args)) t)
           (list 0 ""))))
    (should (equal "Failed to install plugin owner/repo: no Codex plugin manifest found."
                   (mevedel-plugins-slash-command "install owner/repo")))))


(provide 'test-mevedel-plugins)
;;; test-mevedel-plugins.el ends here
