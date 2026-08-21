;;; test-mevedel-plugin-ui.el -- Plugin cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the plugin cockpit and /plugin command integration.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mevedel-cockpit)
(require 'mevedel-menu)
(require 'mevedel-plugin-lifecycle)
(require 'mevedel-plugin-registry)
(require 'mevedel-plugin-ui)
(require 'mevedel-plugins)
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
(defvar mevedel-plugins-test--owner-buffers nil)

(defun mevedel-plugins-test--plugin-line (plugin &optional workspace)
  "Return one compact state line for PLUGIN in WORKSPACE."
  (let ((shadowed (mevedel-plugin-shadowed plugin)))
    (format "%s%s enabled:%s hooks:%s events:%s skills:%d source:%s%s"
            (mevedel-plugin-name plugin)
            (if-let* ((version (mevedel-plugin-version plugin)))
                (format " %s" version)
              "")
            (if (mevedel-plugins-enabled-p plugin workspace) "on" "off")
            (mevedel-plugins-hooks-status plugin workspace)
            (if-let* ((events (mevedel-plugins-hook-rule-events plugin)))
                (string-join events ",")
              "none")
            (mevedel-plugins-skill-count plugin)
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
;;; Plugin list buffer

(mevedel-deftest mevedel-plugins-list-open
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
        (winning-root (file-name-concat
                       workspace-root ".mevedel" "plugins" "winner")))
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
      (should-not (mevedel-plugins-enabled-p
                   (mevedel-plugins-find "demo" workspace) workspace))
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-list-details)))
    (with-current-buffer "*mevedel plugin details*"
      (let ((details (buffer-string)))
        (should (string-match-p "Name:     demo" details))
        (should (string-match-p "Version: 1.0" details))
        (should (string-match-p "Events:   PreToolUse" details))
        (should (string-match-p "Manifest:" details))
        (should (string-match-p "Shadowed sources:" details))
        (should (string-match-p "shadowed active:" details))
        (should (string-match-p
                 "Handlers: PreToolUse \\[Bash\\]: command echo row"
                 details))
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
    ;; `with-help-window' explains its own keys in the echo area.
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-list-help))
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
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-list-details)))
    (with-current-buffer "*mevedel plugin details*"
      (let ((details (buffer-string)))
        (should (string-match-p "Plugin metadata error" details))
        (should (string-match-p "Unsafe plugin name: ../x" details))
        (should (string-match-p
                 (regexp-quote (abbreviate-file-name root))
                 details)))))

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
          (mevedel-test--with-captured-messages nil
            (mevedel-plugins-list-toggle-enabled))
          (should (mevedel-plugins-enabled-p
                   (mevedel-plugins-find "demo" list-workspace)
                   list-workspace))
          (mevedel-test--with-captured-messages nil
            (mevedel-plugins-list-toggle-hooks))
          (should (equal "off"
                         (mevedel-plugins-hooks-status
                          (mevedel-plugins-find "demo" list-workspace)
                          list-workspace)))
          (mevedel-test--with-captured-messages nil
            (mevedel-plugins-list-toggle-hooks))
          (should (equal "on"
                         (mevedel-plugins-hooks-status
                          (mevedel-plugins-find "demo" list-workspace)
                          list-workspace)))
          (mevedel-test--with-captured-messages nil
            (mevedel-plugins-list-toggle-enabled))
          (should-not (mevedel-plugins-enabled-p
                       (mevedel-plugins-find "demo" list-workspace)
                       list-workspace))))))

  :doc "every cockpit mutation refreshes its owner without changing its draft"
  (let ((plugin-user-dir user-dir)
        (state-file (mevedel-plugins-state-file workspace)))
    (require 'mevedel-skills-core)
    (require 'mevedel-view)
    (require 'mevedel-view-composer)
    (dolist (scenario '(enable disable hooks update remove install reload))
      (let* ((root (mevedel-plugins-test--plugin-root
                    plugin-user-dir "repo"))
             (skill-dir (file-name-concat root "skills" "alpha"))
             (fresh-root (mevedel-plugins-test--github-install-root
                          "owner" "fresh"))
             (draft "> plugin action\nsecond line")
             (point-offset 6)
             (session (mevedel-session-create
                       "main" workspace workspace-root)))
        (unwind-protect
            (progn
              (make-directory skill-dir t)
              (with-temp-file (file-name-concat skill-dir "SKILL.md")
                (insert (concat "---\nname: alpha\n"
                                "description: Alpha skill\n---\nAlpha body\n")))
              (when (eq scenario 'hooks)
                (make-directory (file-name-concat root "hooks") t)
                (with-temp-file (file-name-concat root "hooks" "hooks.json")
                  (insert (concat "{\"hooks\":{\"PreToolUse\":[{"
                                  "\"matcher\":\"Bash\",\"hooks\":[{"
                                  "\"type\":\"command\","
                                  "\"command\":\"echo action\"}]}]}}"))))
              (mevedel-plugins-test--write-manifest
               root
               (if (eq scenario 'hooks)
                   "{\"name\":\"demo\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}"
                 "{\"name\":\"demo\",\"skills\":\"skills\"}"))
              (unless (eq scenario 'enable)
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_prompt) t)))
                  (mevedel-plugins-enable "demo" workspace)))
              (mevedel-view-test--with-buffers
                (with-current-buffer data-buf
                  (setq-local mevedel--session session))
                (when (memq scenario '(disable remove))
                  (mevedel-test--with-captured-messages nil
                    (with-current-buffer data-buf
                      (mevedel-skills-rescan))))
                (with-current-buffer view-buf
                  (mevedel-view-test--insert-composer-draft
                   draft point-offset))
                (mevedel-plugins-list-open
                 (mevedel-cockpit-context-for-buffer view-buf))
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_prompt) t))
                          (mevedel-plugins-git-executor
                           (lambda (_directory args)
                             (pcase scenario
                               ('update
                                (delete-directory skill-dir t)
                                (let ((beta-dir
                                       (file-name-concat
                                        root "skills" "beta")))
                                  (make-directory beta-dir t)
                                  (with-temp-file
                                      (file-name-concat beta-dir "SKILL.md")
                                    (insert (concat
                                             "---\nname: beta\n"
                                             "description: Beta skill\n"
                                             "---\nBeta body\n")))))
                               ('install
                                (let ((destination (car (last args))))
                                  (make-directory destination t)
                                  (mevedel-plugins-test--write-manifest
                                   destination "{\"name\":\"fresh\"}"))))
                             (list 0 ""))))
                  (with-current-buffer mevedel-plugins-list-buffer-name
                    (mevedel-test--with-captured-messages nil
                      (pcase scenario
                        ((or 'enable 'disable)
                         (mevedel-plugins-list-toggle-enabled))
                        ('hooks (mevedel-plugins-list-toggle-hooks))
                        ('update (mevedel-plugins-list-update))
                        ('remove (mevedel-plugins-list-remove))
                        ('install (mevedel-plugins-list-install "owner/fresh"))
                        ('reload (mevedel-plugins-list-reload))))))
                (let ((names (mapcar #'mevedel-skill-name
                                     (mevedel-session-skills session))))
                  (pcase scenario
                    ('update
                     (should (member "demo:beta" names))
                     (should-not (member "demo:alpha" names)))
                    ((or 'disable 'remove)
                     (should-not (member "demo:alpha" names)))
                    (_
                     (should (member "demo:alpha" names)))))
                (with-current-buffer view-buf
                  (should (equal draft
                                 (buffer-substring-no-properties
                                  (mevedel-view--input-start) (point-max))))
                  (should (= (+ (mevedel-view--input-start) point-offset)
                             (point))))))
          (when (file-directory-p root)
            (delete-directory root t))
          (when (file-directory-p fresh-root)
            (delete-directory fresh-root t))
          (when (file-exists-p state-file)
            (delete-file state-file))))))

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
        (mevedel-test--with-captured-messages nil
          (mevedel-plugins-list-update))))
    (should (equal (list (list (file-name-as-directory
                                (expand-file-name root))
                               (list "pull" "--ff-only")))
                   calls))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer mevedel-plugins-list-buffer-name
        (mevedel-test--with-captured-messages nil
          (mevedel-plugins-list-remove))))
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
        (mevedel-test--with-captured-messages nil
          (mevedel-plugins-list-install "owner/fresh"))
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
      ;; The clone lands in a staging sibling and is published only once its
      ;; manifest validates, so nothing unusable is ever discoverable.
      (let ((dest (mevedel-plugins-test--github-install-root "owner" "repo"))
            (args (cadr (car calls))))
        (should (equal 1 (length calls)))
        (should (equal (mevedel-plugins-dir) (car (car calls))))
        (should (equal (list "clone" "--depth" "1"
                             "https://github.com/owner/repo.git")
                       (butlast args)))
        (should (mevedel-plugins-staging-name-p (car (last args))))
        (should (file-directory-p dest))
        (should-not (mevedel-plugins-test--staging-leftovers dest)))
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

  :doc "persists plugin enable and disable commands"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin demo."
                     (mevedel-plugins-test--slash session "enable demo")))
      (should (equal "Disabled plugin demo."
                     (mevedel-plugins-test--slash session "disable demo")))
      (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (mevedel-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
      (should (equal "Enabled plugin demo."
                     (mevedel-plugins-test--slash session "enable demo")))
      (let ((state (mevedel-plugins-test--state-plist workspace "demo")))
        (should (plist-get state :enabled))
        (should (plist-get state :hooks-enabled))
        (should (stringp (plist-get state :hooks-fingerprint))))))

  :doc "normalizes both plan-compatible hook spellings"
  (let ((root (mevedel-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (mevedel-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (mevedel-plugins-test--slash session "enable demo")
      (should (equal "Disabled hooks for plugin demo."
                     (mevedel-plugins-test--slash
                      session "hooks disable demo")))
      (should-not
       (plist-get (mevedel-plugins-test--state-plist workspace "demo")
                  :hooks-enabled))
      (should (equal "Enabled hooks for plugin demo."
                     (mevedel-plugins-test--slash
                      session "hooks enable demo")))
      (should
       (plist-get (mevedel-plugins-test--state-plist workspace "demo")
                  :hooks-enabled))))

  :doc "does not treat a superpowers hook toggle as skill bootstrap"
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
    (should-not (mevedel-session-hook-context-pending session)))

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

  :doc "updates and removes a managed global mevedel plugin in place"
  (let* ((root (file-name-concat
                mevedel-user-dir "plugins" "owner" "repo"))
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
                     calls)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Removed plugin demo."
                     (mevedel-plugins-test--slash session "remove demo"))))
    (should-not (file-exists-p root)))

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
        (should (mevedel-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
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
        (should (mevedel-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
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
      (should (mevedel-plugins-same-root-p
               root (plist-get state :source-root) workspace))))

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



(provide 'test-mevedel-plugin-ui)
;;; test-mevedel-plugin-ui.el ends here
