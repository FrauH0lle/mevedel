;;; mevedel-plugin-ui.el -- Plugin cockpit and slash command -*- lexical-binding: t -*-

;;; Commentary:

;; Renders the plugin cockpit and implements the local /plugin command by
;; consuming registry and lifecycle operations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'tabulated-list))

;; `dired'
(declare-function dired "dired" (dirname &optional switches))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-workspace
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-format-header
                  "mevedel-cockpit" (name scope state))
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-show-help
                  "mevedel-cockpit" (buffer text))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-details
                  "mevedel-cockpit" ())
(declare-function mevedel-cockpit-surface-key-help-text
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-menu'
(declare-function mevedel-menu-open "mevedel-menu" (area))

;; `mevedel-plugin-lifecycle'
(declare-function mevedel-plugins-install
                  "mevedel-plugin-lifecycle" (target))
(declare-function mevedel-plugins-remove
                  "mevedel-plugin-lifecycle" (name &optional workspace))
(declare-function mevedel-plugins-update
                  "mevedel-plugin-lifecycle" (name &optional workspace))

;; `mevedel-plugin-registry'
(declare-function mevedel-plugin-description
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-error-manifest
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-error-message
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-error-p
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-error-root
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-hooks
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-name
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-p
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-root
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-shadowed
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-skills-dir
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-version
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugins-active-shadowed-source
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-disable
                  "mevedel-plugin-registry" (plugin-name &optional workspace))
(declare-function mevedel-plugins-disable-hooks
                  "mevedel-plugin-registry" (plugin-name &optional workspace))
(declare-function mevedel-plugins-enable
                  "mevedel-plugin-registry" (plugin-name &optional workspace))
(declare-function mevedel-plugins-enable-hooks
                  "mevedel-plugin-registry" (plugin-name &optional workspace))
(declare-function mevedel-plugins-enabled-p
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-find
                  "mevedel-plugin-registry" (name &optional workspace))
(declare-function mevedel-plugins-hook-consent-summary
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-hook-rule-events
                  "mevedel-plugin-registry" (plugin))
(declare-function mevedel-plugins-hooks-enabled-p
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-hooks-status
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-item-name
                  "mevedel-plugin-registry" (item))
(declare-function mevedel-plugins-items
                  "mevedel-plugin-registry" (&optional workspace))
(declare-function mevedel-plugins-manifest-file
                  "mevedel-plugin-registry" (root))
(declare-function mevedel-plugins-pending-consent
                  "mevedel-plugin-registry" (&optional workspace))
(declare-function mevedel-plugins-plugin-data-dir
                  "mevedel-plugin-registry"
                  (plugin-name &optional workspace))
(declare-function mevedel-plugins-skill-count
                  "mevedel-plugin-registry" (plugin))

;; `mevedel-plugins'
(declare-function mevedel-plugins-current-workspace "mevedel-plugins" ())
(declare-function mevedel-plugins-refresh-session
                  "mevedel-plugins" (&optional context))

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())

;;; Plugin list buffer

(defconst mevedel-plugins-list-buffer-name "*mevedel plugins*"
  "Name of the plugin management buffer.")

(defconst mevedel-plugins-help-buffer-name "*mevedel plugin help*"
  "Name of the plugin cockpit help buffer.")

(defun mevedel-plugins-pending-consent-message (&optional workspace)
  "Return a user-facing pending hook consent message for WORKSPACE."
  (require 'mevedel-plugin-registry)
  (when-let* ((pending (mevedel-plugins-pending-consent workspace)))
    (format "plugin hook consent pending for %s; open /plugin to review"
            (string-join (mapcar #'mevedel-plugin-name pending) ", "))))

(defun mevedel-plugins-notify-pending-consent (&optional workspace)
  "Warn when WORKSPACE has enabled plugins with pending hook consent."
  (when-let* ((message (mevedel-plugins-pending-consent-message workspace)))
    (display-warning 'mevedel
                     (concat "Mevedel " message ".")
                     :warning)
    (message "mevedel: %s" message)
    t))

(defun mevedel-plugins--reload (&optional context)
  "Reload plugin-visible skills for CONTEXT's session when possible."
  (require 'mevedel-plugins)
  (let ((result (mevedel-plugins-refresh-session context)))
    (cond
     ((eq result t)
      "Plugin registry reloaded. Refreshed current session skills.")
     ((stringp result)
      (format "Plugin registry reload failed: %s." result))
     (t
      "Plugin registry reloaded. No active session skills to refresh."))))

(defun mevedel-plugins--with-refresh (message)
  "Refresh current session skills and return MESSAGE."
  (require 'mevedel-plugins)
  (mevedel-plugins-refresh-session)
  message)

(defun mevedel-plugins--plugin-source-label (plugin)
  "Return a compact source label for PLUGIN."
  (abbreviate-file-name (mevedel-plugin-root plugin)))

(defun mevedel-plugins-list--item-id (item)
  "Return stable tabulated-list id for plugin cockpit ITEM."
  (cond
   ((mevedel-plugin-p item) (mevedel-plugin-name item))
   ((mevedel-plugin-error-p item)
    (concat "error:" (mevedel-plugin-error-root item)))))

(defun mevedel-plugins--state-marker (plugin &optional workspace)
  "Return the compact table state marker for PLUGIN in WORKSPACE."
  (cond
   ((equal (mevedel-plugins-hooks-status plugin workspace) "needs-consent")
    (propertize "!" 'face 'warning))
   ((mevedel-plugin-shadowed plugin)
    (propertize "*" 'face 'warning))
   (t "")))

(defun mevedel-plugins--status-cell (status)
  "Return propertized compact STATUS text."
  (pcase status
    ("on" (propertize status 'face 'success))
    ("needs-consent" (propertize status 'face 'warning))
    ((or "off" "none") (propertize status 'face 'shadow))
    (_ status)))

(defun mevedel-plugins--shadowed-lines (plugin &optional workspace)
  "Return shadowed-source lines for PLUGIN in WORKSPACE."
  (let ((active-shadow (mevedel-plugins-active-shadowed-source
                        plugin workspace)))
    (mapcar
     (lambda (shadow)
       (format "  shadowed%s: %s"
               (if (eq shadow active-shadow) " active" "")
               (mevedel-plugins--plugin-source-label shadow)))
     (mevedel-plugin-shadowed plugin))))

(defun mevedel-plugins-list--error-entry (error)
  "Return a `tabulated-list-mode' row for plugin metadata ERROR."
  (list (mevedel-plugins-list--item-id error)
        (vector
         (propertize "!" 'face 'warning)
         (propertize (mevedel-plugins-item-name error) 'face 'warning)
         ""
         (propertize "error" 'face 'error)
         ""
         ""
         (abbreviate-file-name (mevedel-plugin-error-root error)))))

(defun mevedel-plugins-list--entry (item context)
  "Return a `tabulated-list-mode' row for plugin cockpit ITEM in CONTEXT."
  (if (mevedel-plugin-error-p item)
      (mevedel-plugins-list--error-entry item)
    (let* ((workspace (mevedel-plugins-list--workspace context))
           (plugin item)
           (name (mevedel-plugin-name plugin))
           (enabled (if (mevedel-plugins-enabled-p plugin workspace)
                        "on"
                      "off"))
           (hooks (mevedel-plugins-hooks-status plugin workspace)))
      (list name
            (vector
             (mevedel-plugins--state-marker plugin workspace)
             name
             (or (mevedel-plugin-version plugin) "")
             (mevedel-plugins--status-cell enabled)
             (mevedel-plugins--status-cell hooks)
             (number-to-string (mevedel-plugins-skill-count plugin))
             (mevedel-plugins--plugin-source-label plugin))))))

(defun mevedel-plugins-list--workspace (context)
  "Return CONTEXT's plugin cockpit workspace."
  (mevedel-cockpit-context-workspace context))

(defun mevedel-plugins-list--root-label (context)
  "Return CONTEXT's plugin cockpit workspace root label."
  (if-let* ((workspace (mevedel-plugins-list--workspace context)))
      (abbreviate-file-name
       (mevedel-workspace-root workspace))
    "no workspace"))

(defun mevedel-plugins-list--selected-item ()
  "Return the selected plugin cockpit item, or nil."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-selected t))

(defun mevedel-plugins-list--plugin-at-point ()
  "Return the plugin at point, or signal a user error."
  (let ((item (mevedel-plugins-list--selected-item)))
    (cond
     ((mevedel-plugin-p item) item)
     ((mevedel-plugin-error-p item)
      (user-error "Plugin metadata cannot be read: %s"
                  (mevedel-plugin-error-message item)))
     (t
      (user-error "No plugin on this line")))))

(defun mevedel-plugins-list--selected-name ()
  "Return the selected plugin name, or signal a user error."
  (mevedel-plugin-name (mevedel-plugins-list--plugin-at-point)))

(defun mevedel-plugins-list--header-line (items context)
  "Return the plugin cockpit header line for ITEMS and CONTEXT."
  (require 'mevedel-cockpit)
  (let ((total 0)
        (enabled 0)
        (hooks 0)
        (workspace (mevedel-plugins-list--workspace context)))
    (dolist (item items)
      (when (mevedel-plugin-p item)
        (setq total (1+ total))
        (when (mevedel-plugins-enabled-p item workspace)
          (setq enabled (1+ enabled)))
        (when (mevedel-plugins-hooks-enabled-p item workspace)
          (setq hooks (1+ hooks)))))
    (mevedel-cockpit-format-header
     "plugins"
     (mevedel-plugins-list--root-label context)
     (format "%d/%d enabled · %d with hooks" enabled total hooks))))

(defun mevedel-plugins-list--collect (context)
  "Return plugin cockpit items for CONTEXT."
  (require 'mevedel-plugin-registry)
  (mevedel-plugins-items
   (mevedel-plugins-list--workspace context)))

(defun mevedel-plugins-list-refresh ()
  "Refresh the current plugin management buffer."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh))

(defun mevedel-plugins-list--refresh-preserving (name)
  "Refresh the current plugin cockpit, preserving plugin NAME when possible."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-refresh name))

(defun mevedel-plugins-list-toggle-enabled ()
  "Toggle activation for the plugin at point."
  (interactive)
  (require 'mevedel-plugin-registry)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (plugin (mevedel-plugins-list--plugin-at-point))
         (workspace (mevedel-plugins-list--workspace
                     context))
         (name (mevedel-plugin-name plugin)))
    (if (mevedel-plugins-enabled-p plugin workspace)
        (progn
          (mevedel-plugins-disable name workspace)
          (message "mevedel: disabled plugin %s" name))
      (if (mevedel-plugins-enable name workspace)
          (message "mevedel: enabled plugin %s" name)
        (message "mevedel: enable cancelled for plugin %s" name)))
    (mevedel-plugins-refresh-session context)
    (mevedel-plugins-list--refresh-preserving name)))

(defun mevedel-plugins-list-toggle-hooks ()
  "Toggle hooks for the plugin at point."
  (interactive)
  (require 'mevedel-plugin-registry)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (selected (mevedel-plugins-list--plugin-at-point))
         (workspace (mevedel-plugins-list--workspace
                     context))
         (name (mevedel-plugin-name selected)))
    (cond
     ((not (mevedel-plugin-hooks selected))
      (message "mevedel: plugin %s declares no hooks" name))
     ((not (mevedel-plugins-enabled-p
            selected workspace))
      (message "mevedel: plugin %s is not enabled" name))
     ((mevedel-plugins-hooks-enabled-p
       selected workspace)
      (mevedel-plugins-disable-hooks
       name workspace)
      (message "mevedel: disabled hooks for plugin %s" name))
     ((mevedel-plugins-enable-hooks
       name workspace)
      (message "mevedel: enabled hooks for plugin %s" name))
     (t
      (message "mevedel: hook enable cancelled for plugin %s" name)))
    (mevedel-plugins-refresh-session context)
    (mevedel-plugins-list--refresh-preserving name)))

(defun mevedel-plugins-list-update ()
  "Update the plugin at point."
  (interactive)
  (require 'mevedel-plugin-lifecycle)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (name (mevedel-plugins-list--selected-name))
         (workspace (mevedel-plugins-list--workspace
                     context))
         (message (mevedel-plugins-update
                   name
                   workspace)))
    (mevedel-plugins-refresh-session context)
    (mevedel-plugins-list--refresh-preserving name)
    (message "%s" message)))

(defun mevedel-plugins-list-remove ()
  "Remove the plugin at point."
  (interactive)
  (require 'mevedel-plugin-lifecycle)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (name (mevedel-plugins-list--selected-name))
         (workspace (mevedel-plugins-list--workspace
                     context))
         (message (mevedel-plugins-remove
                   name
                   workspace)))
    (mevedel-plugins-refresh-session context)
    (mevedel-plugins-list--refresh-preserving name)
    (message "%s" message)))

(defun mevedel-plugins-list--installed-name (message)
  "Return the plugin name from install MESSAGE when it is present."
  (and (string-match "\\`Installed plugin \\([^ ]+\\)\\." message)
       (match-string 1 message)))

(defun mevedel-plugins-list-install (target)
  "Install plugin TARGET and refresh the cockpit."
  (interactive (list (read-string "Install plugin OWNER/REPO: ")))
  (require 'mevedel-cockpit)
  (require 'mevedel-plugin-lifecycle)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (selected (tabulated-list-get-id))
         (message (mevedel-plugins-install target))
         (name (mevedel-plugins-list--installed-name message)))
    (mevedel-plugins-refresh-session context)
    (mevedel-plugins-list--refresh-preserving (or name selected))
    (message "%s" message)))

(defun mevedel-plugins-list-reload ()
  "Reload plugin-visible session skills and refresh the cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (require 'mevedel-plugins)
  (let* ((context (mevedel-cockpit-surface-context))
         (name (tabulated-list-get-id))
         (message (mevedel-plugins--reload context)))
    (mevedel-plugins-list--refresh-preserving name)
    (message "%s" message)))

(defun mevedel-plugins-list-open-source ()
  "Open the selected plugin source directory in Dired."
  (interactive)
  (let* ((item (or (mevedel-plugins-list--selected-item)
                   (user-error "No plugin on this line")))
         (root (cond
                ((mevedel-plugin-p item) (mevedel-plugin-root item))
                ((mevedel-plugin-error-p item)
                 (mevedel-plugin-error-root item)))))
    (unless (and root (file-directory-p root))
      (user-error "Plugin source is not readable: %s" root))
    (dired root)))

(defun mevedel-plugins-list--detail-text (plugin context)
  "Return detail text for PLUGIN in CONTEXT."
  (let* ((name (mevedel-plugin-name plugin))
         (workspace (mevedel-plugins-list--workspace context))
         (enabled (if (mevedel-plugins-enabled-p plugin workspace)
                      "enabled"
                    "disabled"))
         (hooks (mevedel-plugins-hooks-status plugin workspace))
         (events (mevedel-plugins-hook-rule-events plugin))
         (skills (mevedel-plugins-skill-count plugin))
         (shadowed (mevedel-plugin-shadowed plugin)))
    (string-join
     (delq nil
           (list
            (format "Name:     %s" name)
            (format "Version:  %s"
                    (or (mevedel-plugin-version plugin) "unspecified"))
            (when-let* ((description (mevedel-plugin-description plugin)))
              (format "Description: %s" description))
            (format "Status:   %s" enabled)
            (format "Hooks:    %s%s"
                    hooks
                    (if (equal hooks "needs-consent")
                        " (pending hook consent)"
                      ""))
            (format "Events:   %s"
                    (if events (string-join events ", ") "none"))
            (format "Skills:   %d%s"
                    skills
                    (if-let* ((dir (mevedel-plugin-skills-dir plugin)))
                        (format " from %s" (abbreviate-file-name dir))
                      ""))
            (format "Source:   %s"
                    (abbreviate-file-name (mevedel-plugin-root plugin)))
            (format "Manifest: %s"
                    (abbreviate-file-name
                     (mevedel-plugins-manifest-file
                      (mevedel-plugin-root plugin))))
            (when workspace
              (format "Data:     %s"
                      (abbreviate-file-name
                       (mevedel-plugins-plugin-data-dir name workspace))))
            (when shadowed
              (string-join
               (cons "Shadowed sources:"
                     (mevedel-plugins--shadowed-lines plugin workspace))
               "\n"))
            (when (mevedel-plugin-hooks plugin)
              (concat "Hook consent summary:\n"
                      (mevedel-plugins-hook-consent-summary
                       plugin workspace)))))
     "\n")))

(defun mevedel-plugins-list--error-detail-text (error)
  "Return detail text for plugin metadata ERROR."
  (string-join
   (list
    "Plugin metadata error"
    ""
    (format "Name:     %s" (mevedel-plugins-item-name error))
    (format "Source:   %s"
            (abbreviate-file-name (mevedel-plugin-error-root error)))
    (format "Manifest: %s"
            (abbreviate-file-name (mevedel-plugin-error-manifest error)))
    (format "Error:    %s" (mevedel-plugin-error-message error)))
   "\n"))

(defun mevedel-plugins-list-details ()
  "Show details for the plugin at point."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-surface-details))

(defconst mevedel-plugins-list--surface
  `(:buffer-name ,mevedel-plugins-list-buffer-name
    :label "plugin cockpit"
    :row-label "plugin"
    :mode mevedel-plugins-list-mode
    :format [("State" 5 nil)
             ("Name" 24 t)
             ("Version" 12 t)
             ("Enabled" 8 t)
             ("Hooks" 14 t)
             ("Skills" 7 t)
             ("Source" 0 t)]
    :sort-key ("Name" . nil)
    :collect mevedel-plugins-list--collect
    :entry mevedel-plugins-list--entry
    :header mevedel-plugins-list--header-line
    :details mevedel-plugins-list--details-text
    :details-buffer "*mevedel plugin details*"
    :help-buffer ,mevedel-plugins-help-buffer-name
    :help-function mevedel-plugins-list--help-text
    :keys (("e" "Enable or disable selected plugin"
            mevedel-plugins-list-toggle-enabled)
           ("h" "Toggle hooks for selected plugin"
            mevedel-plugins-list-toggle-hooks)
           ("+" "Install GitHub plugin by OWNER/REPO"
            mevedel-plugins-list-install)
           ("u" "Update selected plugin"
            mevedel-plugins-list-update)
           ("r" "Reload plugin-visible session skills"
            mevedel-plugins-list-reload)
           ("x" "Remove selected managed plugin"
            mevedel-plugins-list-remove)
           ("o" "Open selected plugin source in Dired"
            mevedel-plugins-list-open-source)))
  "Cockpit surface spec for the plugin list.")

(defun mevedel-plugins-list--help-text (&optional _context)
  "Return help text for the plugin cockpit."
  (string-join
   (list
    "mevedel plugin cockpit"
    ""
    "Keys"
    (mevedel-cockpit-surface-key-help-text mevedel-plugins-list--surface)
    ""
    "Slash equivalents"
    "/plugin enable NAME, /plugin disable NAME"
    "/plugin hooks NAME on, /plugin hooks NAME off"
    "/plugin install OWNER/REPO, /plugin update NAME"
    "/plugin reload, /plugin remove NAME, /plugin uninstall NAME"
    "")
   "\n"))

(defun mevedel-plugins-list-help ()
  "Open plugin cockpit help."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-show-help
   mevedel-plugins-help-buffer-name
   (mevedel-plugins-list--help-text)))

(defun mevedel-plugins-list-quit ()
  "Quit the plugin cockpit and return to the main session cockpit."
  (interactive)
  (require 'mevedel-cockpit)
  (mevedel-cockpit-quit "plugin cockpit"))

(defun mevedel-plugins-list--details-text (item context)
  "Return detail text for plugin cockpit ITEM in CONTEXT."
  (concat
   (if (mevedel-plugin-error-p item)
       (mevedel-plugins-list--error-detail-text item)
     (mevedel-plugins-list--detail-text item context))
   "\n"))

(define-derived-mode mevedel-plugins-list-mode tabulated-list-mode
  "mevedel-plugins"
  "Major mode for managing mevedel plugins."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-plugins-list--surface))

(defun mevedel-plugins-list-open (&optional context)
  "Open the plugin management buffer for CONTEXT."
  (require 'mevedel-cockpit)
  (require 'mevedel-plugin-registry)
  (let ((context (or context (mevedel-cockpit-current-context))))
    (mevedel-cockpit-open-surface mevedel-plugins-list--surface context)))


;;
;;; Slash command

(defconst mevedel-plugins--usage
  (concat "Usage: /plugin list | enable NAME | disable NAME | "
          "hooks enable NAME | hooks disable NAME | hooks NAME on | "
          "hooks NAME off | install TARGET | update NAME | "
          "remove NAME | uninstall NAME | reload")
  "Usage text for `/plugin'.")

(defun mevedel-plugins--known-or-message (name &optional workspace)
  "Return installed plugin NAME, or a user-facing error string."
  (or (mevedel-plugins-find name workspace)
      (format "Unknown plugin: %s." name)))

(defun mevedel-plugins-slash-command (args)
  "Run local `/plugin' command from ARGS.
Return a user-facing result string."
  (require 'mevedel-plugin-lifecycle)
  (require 'mevedel-plugin-registry)
  (require 'mevedel-plugins)
  (let* ((parts (split-string (string-trim (or args "")) "[ \t\n]+" t))
         (parts (pcase parts
                  (`("hooks" "enable" ,name) `("hooks" ,name "on"))
                  (`("hooks" "disable" ,name) `("hooks" ,name "off"))
                  (_ parts)))
         (workspace (mevedel-plugins-current-workspace)))
    (pcase parts
      ((or `() `("list"))
       (require 'mevedel-menu)
       (mevedel-menu-open 'plugins)
       nil)
      ((and (or `("enable" ,_)
                `("disable" ,_)
                `("hooks" ,_ "on")
                `("hooks" ,_ "off")
                `("update" ,_)
                `("remove" ,_)
                `("uninstall" ,_))
            (guard (not workspace)))
       "No current workspace for plugin state.")
      (`("enable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (if (mevedel-plugins-enable name workspace)
               (mevedel-plugins--with-refresh
                (format "Enabled plugin %s." name))
             (format "Enable cancelled for plugin %s." name)))))
      (`("disable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable name workspace)
           (mevedel-plugins--with-refresh
            (format "Disabled plugin %s." name)))))
      (`("hooks" ,name "on")
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (cond
            ((not (mevedel-plugin-hooks plugin))
             (format "Plugin %s declares no hooks." name))
            ((not (mevedel-plugins-enabled-p plugin workspace))
             (format "Plugin %s is not enabled." name))
            ((mevedel-plugins-enable-hooks name workspace)
             (mevedel-plugins--with-refresh
              (format "Enabled hooks for plugin %s." name)))
            (t
             (format "Hook enable cancelled for plugin %s." name))))))
      (`("hooks" ,name "off")
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable-hooks name workspace)
           (mevedel-plugins--with-refresh
            (format "Disabled hooks for plugin %s." name)))))
      (`("install" ,target)
       (mevedel-plugins--with-refresh
        (mevedel-plugins-install target)))
      (`("update" ,name)
       (mevedel-plugins--with-refresh
        (mevedel-plugins-update name workspace)))
      ((or `("remove" ,name) `("uninstall" ,name))
       (mevedel-plugins--with-refresh
        (mevedel-plugins-remove name workspace)))
      (`("reload") (mevedel-plugins--reload))
      (_ mevedel-plugins--usage))))



(provide 'mevedel-plugin-ui)
;;; mevedel-plugin-ui.el ends here
