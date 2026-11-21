;;; test-niwa-plugin-ui.el -- Plugin cockpit tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the plugin cockpit and /plugin command integration.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-cockpit)
(require 'niwa-menu)
(require 'niwa-plugin-lifecycle)
(require 'niwa-plugin-registry)
(require 'niwa-plugin-ui)
(require 'niwa-plugins)
(require 'niwa-session-persistence)
(require 'niwa-skills-core)
(require 'niwa-structs)
(require 'niwa-view)
(require 'niwa-view-composer)
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
(defvar niwa-plugins-test--owner-buffers nil)

(defun niwa-plugins-test--context
    (workspace view-buffer data-buffer &optional origin-buffer)
  "Return a plugin cockpit context for WORKSPACE."
  (list :view-buffer view-buffer
        :data-buffer data-buffer
        :origin-buffer (or origin-buffer view-buffer)
        :workspace workspace))

(defun niwa-plugins-test--list-open (workspace)
  "Open the plugin cockpit for WORKSPACE with live owner buffers."
  (let ((view-buffer (generate-new-buffer " *plugin-test-view*"))
        (data-buffer (generate-new-buffer " *plugin-test-data*")))
    (push view-buffer niwa-plugins-test--owner-buffers)
    (push data-buffer niwa-plugins-test--owner-buffers)
    (niwa-plugins-list-open
     (niwa-plugins-test--context
      workspace view-buffer data-buffer view-buffer))))


;;
;;; Plugin list buffer

(niwa-deftest niwa-plugins-list-open
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-buffer-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-buffer-ws-" t)))
           (workspace (niwa-plugins-test--workspace workspace-root))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins")))
   :after-each (progn
                 (when-let* ((buffer (get-buffer
                                      niwa-plugins-list-buffer-name)))
                   (kill-buffer buffer))
                 (when-let* ((buffer (get-buffer
                                      "*niwa plugin details*")))
                   (kill-buffer buffer))
                 (when-let* ((buffer (get-buffer
                                      niwa-plugins-help-buffer-name)))
                   (kill-buffer buffer))
                 (mapc (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer)))
                       niwa-plugins-test--owner-buffers)
                 (setq niwa-plugins-test--owner-buffers nil)
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "selects the displayed plugin cockpit window"
  (let ((buffer (niwa-plugins-test--list-open workspace)))
    (should (eq buffer (window-buffer (selected-window)))))

  :doc "renders visible plugin rows and details"
  (let ((shadow-root (file-name-concat (niwa-plugins-dir) "shadow"))
        (winning-root (file-name-concat
                       workspace-root ".niwa" "plugins" "winner")))
    (niwa-plugins-test--write-manifest shadow-root "{\"name\":\"demo\"}")
    (niwa-plugins-enable "demo" workspace)
    (make-directory (file-name-concat winning-root "hooks") t)
    (with-temp-file (file-name-concat winning-root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo row\"}]}]}}"))
    (niwa-plugins-test--write-manifest
     winning-root
     "{\"name\":\"demo\",\"version\":\"1.0\",\"hooks\":\"hooks/hooks.json\"}")
    (niwa-plugins-test--list-open workspace)
    (with-current-buffer niwa-plugins-list-buffer-name
      (let ((rows (niwa-test-tabulated-entries-cells)))
        (should (= 1 (length rows)))
        (should (equal (cdr (assoc "demo" rows))
                       (list "*" "demo" "1.0" "off" "off" "0"
                             (abbreviate-file-name
                              (file-name-as-directory
                               (expand-file-name winning-root)))))))
      (should-not (niwa-plugins-enabled-p
                   (niwa-plugins-find "demo" workspace) workspace))
      (niwa-test--with-captured-messages nil
        (niwa-plugins-list-details)))
    (with-current-buffer "*niwa plugin details*"
      (let ((details (buffer-string)))
		       (should (string-match-p "Name +demo" details))
		       (should (string-match-p "Version +1.0" details))
		       (should (string-match-p "Events +PreToolUse" details))
		       (should (string-match-p "Manifest" details))
		       (should (string-match-p "Shadowed sources" details))
        (should (string-match-p "shadowed active:" details))
        (should (string-match-p
                 "Handlers: PreToolUse \\[Bash\\]: command echo row"
                 details))
        (should (string-match-p
                 (regexp-quote (niwa-plugins-plugin-data-dir
                                "demo" workspace))
                 details)))))

  :doc "renders an empty plugin list without error"
  (progn
    (niwa-plugins-test--list-open workspace)
    (with-current-buffer niwa-plugins-list-buffer-name
      (should-not tabulated-list-entries)
      (should (string-match-p "0/0 enabled"
                              (niwa-cockpit-surface-header-line)))))

  :doc "opens generated plugin cockpit help"
  (progn
    ;; `with-help-window' explains its own keys in the echo area.
    (niwa-test--with-captured-messages nil
      (niwa-plugins-list-help))
    (with-current-buffer niwa-plugins-help-buffer-name
      (should (string-match-p "RET  Show selected plugin details"
                              (buffer-string)))
      (should (string-match-p "e    Enable or disable selected plugin"
                              (buffer-string)))
      (should (string-match-p "/plugin enable NAME"
                              (buffer-string)))))

  :doc "renders malformed manifests as visible error rows"
  (let ((root (niwa-plugins-test--plugin-root user-dir "bad")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"../x\"}")
    (should-not (niwa-plugins-list workspace))
    (niwa-plugins-test--list-open workspace)
    (with-current-buffer niwa-plugins-list-buffer-name
      (let* ((rows (niwa-test-tabulated-entries-cells))
             (root (file-name-as-directory (expand-file-name root))))
        (should (= 1 (length rows)))
        (should (equal (cdr (assoc (concat "error:" root) rows))
                       (list "!" "../x" "" "error" "" ""
                             (abbreviate-file-name root)))))
      (niwa-test--with-captured-messages nil
        (niwa-plugins-list-details)))
    (with-current-buffer "*niwa plugin details*"
      (let ((details (buffer-string)))
        (should (string-match-p "Plugin metadata error" details))
        (should (string-match-p "Unsafe plugin name: ../x" details))
        (should (string-match-p
                 (regexp-quote (abbreviate-file-name root))
                 details)))))

  :doc "dispatches adaptive activation and hook override actions at point"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo action\"}]}]}}"))
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (niwa-plugins-test--list-open workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer niwa-plugins-list-buffer-name
        (let ((list-workspace
               (niwa-cockpit-context-workspace
                (niwa-cockpit-surface-context))))
          (niwa-test--with-captured-messages nil
            (niwa-plugins-list-toggle-enabled))
          (should (niwa-plugins-enabled-p
                   (niwa-plugins-find "demo" list-workspace)
                   list-workspace))
          (niwa-test--with-captured-messages nil
            (niwa-plugins-list-toggle-hooks))
          (should (equal "off"
                         (niwa-plugins-hooks-status
                          (niwa-plugins-find "demo" list-workspace)
                          list-workspace)))
          (niwa-test--with-captured-messages nil
            (niwa-plugins-list-toggle-hooks))
          (should (equal "on"
                         (niwa-plugins-hooks-status
                          (niwa-plugins-find "demo" list-workspace)
                          list-workspace)))
          (niwa-test--with-captured-messages nil
            (niwa-plugins-list-toggle-enabled))
          (should-not (niwa-plugins-enabled-p
                       (niwa-plugins-find "demo" list-workspace)
                       list-workspace))))))

  :doc "every cockpit mutation refreshes its owner without changing its draft"
  (let ((plugin-user-dir user-dir)
        (state-file (niwa-plugins-state-file workspace)))
    (dolist (scenario '(enable disable hooks update remove install reload))
      (let* ((root (niwa-plugins-test--plugin-root
                    plugin-user-dir "repo"))
             (skill-dir (file-name-concat root "skills" "alpha"))
             (fresh-root (niwa-plugins-test--github-install-root
                          "owner" "fresh"))
             (draft "> plugin action\nsecond line")
             (point-offset 6)
             (session (niwa-session-create
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
              (niwa-plugins-test--write-manifest
               root
               (if (eq scenario 'hooks)
                   "{\"name\":\"demo\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}"
                 "{\"name\":\"demo\",\"skills\":\"skills\"}"))
              (unless (eq scenario 'enable)
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_prompt) t)))
                  (niwa-plugins-enable "demo" workspace)))
              (niwa-view-test--with-buffers
                (with-current-buffer data-buf
                  (setq-local niwa--session session))
                (when (memq scenario '(disable remove))
                  (niwa-test--with-captured-messages nil
                    (with-current-buffer data-buf
                      (niwa-skills-rescan))))
                (with-current-buffer view-buf
                  (niwa-view-test--insert-composer-draft
                   draft point-offset))
                (niwa-plugins-list-open
                 (niwa-cockpit-context-for-buffer view-buf))
                (cl-letf (((symbol-function 'yes-or-no-p)
                           (lambda (_prompt) t))
                          (niwa-plugins-git-executor
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
                                  (niwa-plugins-test--write-manifest
                                   destination "{\"name\":\"fresh\"}"))))
                             (list 0 ""))))
                  (with-current-buffer niwa-plugins-list-buffer-name
                    (niwa-test--with-captured-messages nil
                      (pcase scenario
                        ((or 'enable 'disable)
                         (niwa-plugins-list-toggle-enabled))
                        ('hooks (niwa-plugins-list-toggle-hooks))
                        ('update (niwa-plugins-list-update))
                        ('remove (niwa-plugins-list-remove))
                        ('install (niwa-plugins-list-install "owner/fresh"))
                        ('reload (niwa-plugins-list-reload))))))
                (let ((names (mapcar #'niwa-skill-name
                                     (niwa-session-skills session))))
                  (pcase scenario
                    ('update
                     (should (member "demo:beta" names))
                     (should-not (member "demo:alpha" names)))
                    ((or 'disable 'remove)
                     (should-not (member "demo:alpha" names)))
                    (_
                     (should (member "demo:alpha" names)))))
                (with-current-buffer view-buf
                  (should (equal draft (niwa-view-composer-draft-text nil t)))
                  (should (= point-offset
                             (niwa-view-test--composer-point-offset))))))
          (when (file-directory-p root)
            (delete-directory root t))
          (when (file-directory-p fresh-root)
            (delete-directory fresh-root t))
          (when (file-exists-p state-file)
            (delete-file state-file))))))

  :doc "dispatches update and remove actions at point"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo"))
        calls)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (niwa-plugins-test--list-open workspace)
    (cl-letf ((niwa-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (with-current-buffer niwa-plugins-list-buffer-name
        (niwa-test--with-captured-messages nil
          (niwa-plugins-list-update))))
    (should (equal (list (list (file-name-as-directory
                                (expand-file-name root))
                               (list "pull" "--ff-only")))
                   calls))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (with-current-buffer niwa-plugins-list-buffer-name
        (niwa-test--with-captured-messages nil
          (niwa-plugins-list-remove))))
    (should-not (file-exists-p root)))

  :doc "installs, refreshes, and selects the newly installed plugin"
  (let (calls)
    (niwa-plugins-test--list-open workspace)
    (cl-letf ((niwa-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (niwa-plugins-test--write-manifest
                    dest "{\"name\":\"fresh\"}"))
                 (list 0 ""))))
      (with-current-buffer niwa-plugins-list-buffer-name
        (niwa-test--with-captured-messages nil
          (niwa-plugins-list-install "owner/fresh"))
        (should (assoc "fresh"
                       (niwa-test-tabulated-entries-cells)))))
    (should (= 1 (length calls))))

  :doc "failed updates stay raw until the native cockpit presents them"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo"))
        (raw "Failed to update plugin demo: rejected")
        (niwa-plugins-git-executor (lambda (&rest _) '(1 "rejected")))
        echoed)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (should (equal raw (niwa-plugins-update "demo" workspace)))
    (with-temp-buffer
      (setq-local niwa--workspace workspace)
      (should (equal raw (niwa-plugins-slash-command "update demo"))))
    (niwa-plugins-test--list-open workspace)
    (with-current-buffer niwa-plugins-list-buffer-name
      (niwa-test--with-captured-messages echoed (niwa-plugins-list-update)))
    (should (equal (concat "niwa [" workspace-root "]: " raw "\n") echoed)))

  :doc "mutation actions require live owners before side effects"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo"))
        (view-buffer (generate-new-buffer " *plugin-action-view*"))
        (data-buffer (generate-new-buffer " *plugin-action-data*"))
        calls)
    (unwind-protect
        (progn
          (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (niwa-plugins-list-open
           (niwa-plugins-test--context
            workspace view-buffer data-buffer view-buffer))
          (kill-buffer data-buffer)
          (cl-letf ((niwa-plugins-git-executor
                     (lambda (_directory _args)
                       (push t calls)
                       (list 0 ""))))
            (with-current-buffer niwa-plugins-list-buffer-name
              (should-error (niwa-plugins-list-toggle-enabled)
                            :type 'user-error)
              (should-error (niwa-plugins-list-update)
                            :type 'user-error)
              (should-error (niwa-plugins-list-install "owner/fresh")
                            :type 'user-error)))
          (should-not calls)
          (should-not (niwa-plugins-enabled workspace)))
      (when (buffer-live-p view-buffer) (kill-buffer view-buffer))
      (when (buffer-live-p data-buffer) (kill-buffer data-buffer)))))


;;
;;; Slash command

(niwa-deftest niwa-plugins-slash-command
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-slash-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-slash-ws-" t)))
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
  :doc "returns user-facing strings for bad input"
  (should (equal "Unknown plugin: missing."
                 (niwa-plugins-test--slash session "enable missing")))
  (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                         "GitHub repository.")
                 (niwa-plugins-test--slash
                  session "install https://example.com/x.git")))
  (let ((niwa-plugins-git-executor
         (lambda (_directory _args)
           (ert-fail "git should not run for unsafe plugin target"))))
    (should (equal (concat "Invalid plugin target: use OWNER/REPO or a "
                           "GitHub repository.")
                   (niwa-plugins-test--slash
                    session "install owner/.."))))

  :doc "stateful commands require a current workspace before side effects"
  (let ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
        calls
        (niwa--session nil)
        (niwa--workspace nil))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((niwa-plugins-git-executor
           (lambda (_directory _args)
             (push t calls)
             (list 0 ""))))
      (should (equal "No current workspace for plugin state."
                     (niwa-plugins-slash-command "enable demo")))
      (should (equal "No current workspace for plugin state."
                     (niwa-plugins-slash-command "update demo")))
      (should (equal "No current workspace for plugin state."
                     (niwa-plugins-slash-command "remove demo"))))
    (should-not calls)
    (should (file-exists-p root)))

  :doc "installs a GitHub plugin with stubbed git and leaves it disabled"
  (let (calls)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "install should not prompt for hooks")))
              (niwa-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (make-directory (file-name-concat dest "hooks") t)
                   (with-temp-file (file-name-concat dest "hooks" "hooks.json")
                     (insert "{}"))
                   (niwa-plugins-test--write-manifest
                    dest
                    "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}"))
                 (list 0 ""))))
      (should (equal "Installed plugin demo."
                     (niwa-plugins-test--slash
                      session "install owner/repo")))
      ;; The clone lands in a staging sibling and is published only once its
      ;; manifest validates, so nothing unusable is ever discoverable.
      (let ((dest (niwa-plugins-test--github-install-root "owner" "repo"))
            (args (cadr (car calls))))
        (should (equal 1 (length calls)))
        (should (equal (niwa-plugins-dir) (car (car calls))))
        (should (equal (list "clone" "--depth" "1"
                             "https://github.com/owner/repo.git")
                       (butlast args)))
        (should (niwa-plugins-staging-name-p (car (last args))))
        (should (file-directory-p dest))
        (should-not (niwa-plugins-test--staging-leftovers dest)))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should-not (niwa-plugins-enabled-p plugin workspace))
        (should (equal "off"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "blank and list forms route through the plugin cockpit"
  (let (areas)
    (cl-letf (((symbol-function 'niwa-menu-open)
               (lambda (area)
                 (push area areas))))
      (dolist (args '("" "list"))
        (should-not (niwa-plugins-test--slash session args))))
    (should (equal '(plugins plugins) areas)))

  :doc "blank and list forms require a live cockpit-capable buffer"
  (with-temp-buffer
    (should-error (niwa-plugins-slash-command "") :type 'user-error))

  :doc "hook-only enable requires an enabled plugin"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "disabled hook override should not prompt"))))
      (should (equal "Plugin demo is not enabled."
                     (niwa-plugins-test--slash
                      session "hooks demo on")))))

  :doc "persists plugin enable and disable commands"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin demo."
                     (niwa-plugins-test--slash session "enable demo")))
      (should (equal "Disabled plugin demo."
                     (niwa-plugins-test--slash session "disable demo")))
      (let ((state (niwa-plugins-test--state-plist workspace "demo")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (niwa-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
      (should (equal "Enabled plugin demo."
                     (niwa-plugins-test--slash session "enable demo")))
      (let ((state (niwa-plugins-test--state-plist workspace "demo")))
        (should (plist-get state :enabled))
        (should (plist-get state :hooks-enabled))
        (should (stringp (plist-get state :hooks-fingerprint))))))

  :doc "normalizes both plan-compatible hook spellings"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (niwa-plugins-test--slash session "enable demo")
      (should (equal "Disabled hooks for plugin demo."
                     (niwa-plugins-test--slash
                      session "hooks disable demo")))
      (should-not
       (plist-get (niwa-plugins-test--state-plist workspace "demo")
                  :hooks-enabled))
      (should (equal "Enabled hooks for plugin demo."
                     (niwa-plugins-test--slash
                      session "hooks enable demo")))
      (should
       (plist-get (niwa-plugins-test--state-plist workspace "demo")
                  :hooks-enabled))))

  :doc "does not treat a superpowers hook toggle as skill bootstrap"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"superpowers\",\"skills\":\"skills\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Enabled plugin superpowers."
                     (niwa-plugins-test--slash
                      session "enable superpowers"))))
    (should-not (niwa-session-hook-context-pending session)))

  :doc "updates an installed plugin by manifest name"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory root t)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((niwa-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (niwa-plugins-test--slash session "update demo")))
      (should (equal (list (list (file-name-as-directory
                                  (expand-file-name root))
                                 (list "pull" "--ff-only")))
                     calls))))

  :doc "updates and removes a managed global niwa plugin in place"
  (let* ((root (file-name-concat
                niwa-user-dir "plugins" "owner" "repo"))
         calls)
    (make-directory root t)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((niwa-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (niwa-plugins-test--slash session "update demo")))
      (should (equal (list (list (file-name-as-directory
                                  (expand-file-name root))
                                 (list "pull" "--ff-only")))
                     calls)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Removed plugin demo."
                     (niwa-plugins-test--slash session "remove demo"))))
    (should-not (file-exists-p root)))

  :doc "install refuses an existing GitHub destination without running git"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo")))
    (make-directory root t)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((niwa-plugins-git-executor
           (lambda (_directory _args)
             (ert-fail "install should not run git for existing destinations"))))
      (should (equal "Plugin demo is already installed; use /plugin update demo."
                     (niwa-plugins-test--slash
                      session "install owner/repo")))))

  :doc "install refuses existing manifestless destination without suggesting update"
  (let ((root (niwa-plugins-test--github-install-root "owner" "repo")))
    (make-directory root t)
    (let ((niwa-plugins-git-executor
           (lambda (_directory _args)
             (ert-fail "install should not run git for existing destinations"))))
      (should
       (equal
        (format (concat "Plugin path %s already exists, but no Codex plugin "
                        "manifest was found; fix or remove it before "
                        "installing owner/repo.")
                root)
        (niwa-plugins-test--slash session "install owner/repo")))))

  :doc "manual updates preserve existing hook consent without prompting"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Bash\","
              "\"hooks\":[{\"type\":\"command\","
              "\"command\":\"echo demo\"}]}]}}"))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (niwa-plugins-enable "demo" workspace))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (niwa-plugins-git-executor
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
                     (niwa-plugins-test--slash session "update demo")))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should (niwa-plugins-enabled-p plugin workspace))
        (should (equal "on"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "manual updates invalidate changed hook consent"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo")))
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (niwa-plugins-enable "demo" workspace))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (niwa-plugins-git-executor
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
                     (niwa-plugins-test--slash session "update demo")))
      (should (equal (concat "plugin hook consent pending for demo; "
                             "open /plugin to review")
                     (niwa-plugins-pending-consent-message workspace)))
      (let (warnings messages)
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type message &optional level _buffer-name)
                     (push (list type message level) warnings)))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (push (apply #'format format-string args) messages))))
          ;; First notification warns; the repeat demotes to *Messages*.
          (should (niwa-plugins-notify-pending-consent workspace))
          (should (niwa-plugins-notify-pending-consent workspace)))
        (should (= 1 (length warnings)))
        (should (equal 'niwa (caar warnings)))
        (should (string-match-p "plugin hook consent pending for demo"
                                (cadar warnings)))
        (should (member (concat "niwa [" (niwa-workspace-root workspace)
                                "]: plugin hook consent pending "
                                "for demo; open /plugin to review")
                        messages)))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should (niwa-plugins-enabled-p plugin workspace))
        (should (equal "needs-consent"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "manual updates preserve disabled plugin state"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root
     "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (niwa-plugins-enable "demo" workspace))
    (niwa-plugins-disable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "update should not prompt for hook consent")))
              (niwa-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (list 0 ""))))
      (should (equal "Updated plugin demo."
                     (niwa-plugins-test--slash session "update demo")))
      (let ((state (niwa-plugins-test--state-plist workspace "demo")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (niwa-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should-not (niwa-plugins-enabled-p plugin workspace))
        (should (equal "off"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "manual updates preserve prior state when manifest name changes"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         calls)
    (make-directory root t)
    (niwa-plugins-test--write-manifest root "{\"name\":\"old-name\"}")
    (niwa-plugins-disable "old-name" workspace)
    (let ((niwa-plugins-git-executor
           (lambda (directory args)
             (push (list directory args) calls)
             (niwa-plugins-test--write-manifest
              root
              "{\"name\":\"new-name\"}")
             (list 0 ""))))
      (should (equal "Updated plugin new-name."
                     (niwa-plugins-test--slash
                      session "update old-name")))
      (let ((state (niwa-plugins-test--state-plist workspace "new-name")))
        (should-not (plist-get state :enabled))
        (should-not (plist-get state :hooks-enabled))
        (should (niwa-plugins-same-root-p
                 root (plist-get state :source-root) workspace)))
      (let ((plugin (niwa-plugins-find "new-name" workspace)))
        (should-not (niwa-plugins-enabled-p plugin workspace))
        (should (equal "none"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "remove deletes managed plugin root and matching activation state"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (niwa-plugins-plugin-data-dir "demo" workspace))
         refreshes)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (with-temp-file (file-name-concat data-dir "cache")
      (insert "cached"))
    (niwa-plugins-enable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'niwa-skills-rescan)
               (lambda () (push t refreshes))))
      (should (equal "Removed plugin demo."
                     (niwa-plugins-test--slash session "remove demo"))))
    (should-not (file-exists-p root))
    (should (file-exists-p data-dir))
    (should-not (niwa-plugins-test--read-state workspace))
    (should (= 1 (length refreshes))))

  :doc "uninstall is an alias for remove"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (niwa-plugins-plugin-data-dir "demo" workspace)))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t)))
      (should (equal "Removed plugin demo."
                     (niwa-plugins-test--slash
                      session "uninstall demo"))))
    (should-not (file-exists-p root))
    (should (file-exists-p data-dir)))

  :doc "remove cancellation leaves plugin root, data, and state intact"
  (let* ((root (niwa-plugins-test--github-plugin-root user-dir "owner" "repo"))
         (data-dir (niwa-plugins-plugin-data-dir "demo" workspace)))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (make-directory data-dir t)
    (niwa-plugins-disable "demo" workspace)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) nil)))
      (should (equal "Remove cancelled for plugin demo."
                     (niwa-plugins-test--slash session "remove demo"))))
    (should (file-exists-p root))
    (should (file-exists-p data-dir))
    (let ((state (niwa-plugins-test--state-plist workspace "demo")))
      (should-not (plist-get state :enabled))
      (should-not (plist-get state :hooks-enabled))
      (should (niwa-plugins-same-root-p
               root (plist-get state :source-root) workspace))))

  :doc "remove reports unknown plugins"
  (should (equal "Unknown plugin: missing."
                 (niwa-plugins-test--slash session "remove missing")))

  :doc "update refuses extra-root plugins"
  (let* ((extra-dir (file-name-as-directory
                     (make-temp-file "niwa-plugins-extra-update-" t)))
         (root (file-name-concat extra-dir "repo"))
         calls)
    (unwind-protect
        (let ((niwa-plugin-extra-roots (list extra-dir))
              (niwa-plugins-git-executor
               (lambda (_directory _args)
                 (push t calls)
                 (list 0 ""))))
          (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (should (string-match-p
                   "Plugin demo is not managed by niwa; update .* manually\\."
                   (niwa-plugins-test--slash
                    session "update demo")))
          (should-not calls))
      (delete-directory extra-dir t)))

  :doc "remove refuses extra-root plugins"
  (let* ((extra-dir (file-name-as-directory
                     (make-temp-file "niwa-plugins-extra-remove-" t)))
         (root (file-name-concat extra-dir "repo")))
    (unwind-protect
        (let ((niwa-plugin-extra-roots (list extra-dir)))
          (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt)
                       (ert-fail "extra-root removal should not prompt"))))
            (should (string-match-p
                     "Plugin demo is not managed by niwa; remove .* manually\\."
                     (niwa-plugins-test--slash
                      session "remove demo"))))
          (should (file-exists-p root)))
      (delete-directory extra-dir t)))

  :doc "update refuses workspace-local plugins"
  (let ((root (file-name-concat
               workspace-root ".niwa" "plugins" "repo"))
        calls)
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (let ((niwa-plugins-git-executor
           (lambda (_directory _args)
             (push t calls)
             (list 0 ""))))
      (should (string-match-p
               "Plugin demo is not managed by niwa; update .* manually\\."
               (niwa-plugins-test--slash session "update demo")))
      (should-not calls)))

  :doc "remove refuses workspace-local plugins"
  (let ((root (file-name-concat
               workspace-root ".niwa" "plugins" "repo")))
    (niwa-plugins-test--write-manifest root "{\"name\":\"demo\"}")
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "workspace-local removal should not prompt"))))
      (should (string-match-p
               "Plugin demo is not managed by niwa; remove .* manually\\."
               (niwa-plugins-test--slash session "remove demo"))))
    (should (file-exists-p root)))

  :doc "reload returns a user-facing string"
  (should (equal "Plugin registry reloaded. No active session skills to refresh."
                 (niwa-plugins-slash-command "reload")))

  :doc "reload reports unexpected refresh failures"
  (let (warnings)
    (cl-letf (((symbol-function 'niwa-skills-rescan)
               (lambda ()
                 (signal 'error '("refresh broke"))))
              ((symbol-function 'display-warning)
               (lambda (type message &optional level _buffer-name)
                 (push (list type message level) warnings))))
      (should (equal "Plugin registry reload failed: refresh broke."
                     (niwa-plugins-test--slash session "reload")))
      (should (equal (list (list 'niwa
                                (format "niwa [%s]: Plugin registry refresh failed: refresh broke"
                                        (niwa-workspace-root workspace))
                                :warning))
                     warnings))))

  :doc "refreshes current session skills after plugin mutations"
  (let ((root (niwa-plugins-test--plugin-root user-dir "repo"))
        refreshes)
    (make-directory (file-name-concat root "hooks") t)
    (with-temp-file (file-name-concat root "hooks" "hooks.json")
      (insert "{}"))
    (niwa-plugins-test--write-manifest
     root "{\"name\":\"demo\",\"hooks\":\"hooks/hooks.json\"}")
    (cl-letf (((symbol-function 'niwa-skills-rescan)
               (lambda ()
                 (push t refreshes)))
              ((symbol-function 'yes-or-no-p)
               (lambda (_prompt) t))
              (niwa-plugins-git-executor
               (lambda (_directory args)
                 (when (equal (car args) "clone")
                   (let ((dest (car (last args))))
                     (make-directory dest t)
                     (niwa-plugins-test--write-manifest
                      dest
                      "{\"name\":\"fresh\"}")))
                 (list 0 ""))))
      (niwa-plugins-test--slash session "enable demo")
      (niwa-plugins-test--slash session "hooks demo off")
      (niwa-plugins-test--slash session "hooks demo on")
      (niwa-plugins-test--slash session "disable demo")
      (niwa-plugins-test--slash session "install owner/fresh")
      (niwa-plugins-test--slash session "update demo")
      (niwa-plugins-test--slash session "reload")
      (should (= 7 (length refreshes)))))

  :doc "git executor failures return a user-facing string"
  (let ((niwa-plugins-git-executor
         (lambda (_directory _args)
           (signal 'error '("git is missing")))))
    (should (equal "Failed to install plugin owner/repo: git is missing"
                   (niwa-plugins-test--slash
                    session "install owner/repo"))))

  :doc "fresh installs without hooks do not prompt and leave plugin disabled"
  (let (calls)
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (_prompt)
                 (ert-fail "hookless install should not prompt")))
              (niwa-plugins-git-executor
               (lambda (directory args)
                 (push (list directory args) calls)
                 (let ((dest (car (last args))))
                   (make-directory dest t)
                   (niwa-plugins-test--write-manifest
                    dest
                    "{\"name\":\"demo\"}"))
                 (list 0 ""))))
      (should (equal "Installed plugin demo."
                     (niwa-plugins-test--slash
                      session "install owner/repo")))
      (let ((plugin (niwa-plugins-find "demo" workspace)))
        (should-not (niwa-plugins-enabled-p plugin workspace))
        (should (equal "none"
                       (niwa-plugins-hooks-status plugin workspace))))))

  :doc "fresh installs fail when the clone lacks a manifest"
  (let ((niwa-plugins-git-executor
         (lambda (_directory args)
           (make-directory (car (last args)) t)
           (list 0 ""))))
    (should (equal "Failed to install plugin owner/repo: no Codex plugin manifest found."
                   (niwa-plugins-test--slash
                    session "install owner/repo")))))



(niwa-deftest niwa-plugins-list-check-updates
  (:vars* ((user-dir (file-name-as-directory
                      (make-temp-file "niwa-plugins-upd-ui-" t)))
           (workspace-root (file-name-as-directory
                            (make-temp-file "niwa-plugins-upd-ws-" t)))
           (workspace (niwa-plugins-test--workspace workspace-root))
           (niwa-user-dir user-dir)
           (niwa-plugin-install-directory
            (file-name-concat user-dir ".agents" "plugins"))
           (niwa-plugins--update-cache :unloaded)
           (niwa-plugins--update-check-busy nil))
   :after-each (progn
                 (when-let* ((buffer (get-buffer
                                      niwa-plugins-list-buffer-name)))
                   (kill-buffer buffer))
                 (mapc (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer)))
                       niwa-plugins-test--owner-buffers)
                 (setq niwa-plugins-test--owner-buffers nil)
                 (delete-directory user-dir t)
                 (delete-directory workspace-root t)))
  ,test
  (test)
  :doc "the action runs an on-demand check and shows its result in the rows"
  (let ((plugin-root (niwa-plugins-test--github-install-root "o" "demo"))
        (args nil))
    (niwa-plugins-test--write-manifest plugin-root "{\"name\":\"demo\"}")
    (make-directory (file-name-concat plugin-root ".git") t)
    (niwa-plugins-test--list-open workspace)
    (cl-letf (((symbol-function 'niwa-plugins-check-updates)
               (lambda (&optional on-demand done)
                 (setq args on-demand)
                 (niwa-plugins--update-set-entry
                  plugin-root (list :local "aaa" :remote "bbb"))
                 (funcall done))))
      (with-current-buffer niwa-plugins-list-buffer-name
        (let ((before (niwa-test-tabulated-entries-cells)))
          (should-not (string-match-p "update available"
                                      (car (last (cdar before)))))
          (niwa-test--with-captured-messages nil
            (niwa-plugins-list-check-updates))
          (should (eq t args))
          (should (string-match-p
                   "update available\\'"
                   (car (last (cdar (niwa-test-tabulated-entries-cells)))))))))
    ;; A failed check is a row note, not a message.
    (niwa-plugins--update-set-entry plugin-root (list :error "offline"))
    (with-current-buffer niwa-plugins-list-buffer-name
      (niwa-plugins-list-refresh)
      (should (string-match-p
               "check failed\\'"
               (car (last (cdar (niwa-test-tabulated-entries-cells)))))))))

(niwa-deftest niwa-plugins-message-local-result ()
  ,test
  (test)
  :doc "attributes known failures without changing status and cancellation messages"
  (progn
  (dolist (raw '("Failed to install plugin x: rejected" "Invalid plugin target: x"
                 "Unknown plugin: x." "Plugin path /x already exists."
                 "Plugin x is not managed by niwa; update it manually."
                 "Plugin x is not enabled." "No current workspace for plugin state."
                 "Plugin registry reload failed: rejected."))
    (let (echoed)
      (niwa-test--with-captured-messages echoed
        (niwa-plugins-message-local-result "/owner/" raw))
      (should (equal (concat "niwa [/owner/]: " raw "\n") echoed))))
  (dolist (raw '("Installed plugin x." "Updated plugin x." "Removed plugin x."
                 "Remove cancelled for plugin x." "Enable cancelled for plugin x."
                 "Plugin x is already installed; use /plugin update x."
                 "Plugin x declares no hooks." "Usage: /plugin list"))
    (let (echoed)
      (niwa-test--with-captured-messages echoed
        (niwa-plugins-message-local-result "/owner/" raw))
      (should (equal (concat raw "\n") echoed))))))

(provide 'test-niwa-plugin-ui)
;;; test-niwa-plugin-ui.el ends here
