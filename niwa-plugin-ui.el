;;; niwa-plugin-ui.el -- Plugin cockpit and slash command -*- lexical-binding: t -*-

;;; Commentary:

;; Renders the plugin cockpit and implements the local /plugin command by
;; consuming registry and lifecycle operations.

;;; Code:

(require 'niwa-report)

(require 'subr-x)
(require 'tabulated-list)
(require 'niwa-cockpit)
(require 'niwa-plugin-registry)
(require 'niwa-plugins)
(require 'niwa-utilities)

;; `dired'
(declare-function dired "dired" (dirname &optional switches))

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-format-header
                  "niwa-cockpit" (name scope state))
(declare-function niwa-cockpit-open-surface
                  "niwa-cockpit" (surface &optional context))
(declare-function niwa-cockpit-quit "niwa-cockpit" (&optional label))
(declare-function niwa-cockpit-setup-tabulated-surface
                  "niwa-cockpit" (surface))
(declare-function niwa-cockpit-show-help
                  "niwa-cockpit" (buffer text))
(declare-function niwa-cockpit-surface-context
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-details
                  "niwa-cockpit" ())
(declare-function niwa-cockpit-surface-key-help-text
                  "niwa-cockpit" (&optional surface))
(declare-function niwa-cockpit-surface-refresh
                  "niwa-cockpit" (&optional selected-id))
(declare-function niwa-cockpit-surface-selected
                  "niwa-cockpit" (&optional no-error))

;; `niwa-menu'
(declare-function niwa-menu-open "niwa-menu" (area))
(autoload 'niwa-menu-open "niwa-menu" nil t)

;; `niwa-plugin-lifecycle'
(declare-function niwa-plugins-install
                  "niwa-plugin-lifecycle" (target))
(declare-function niwa-plugins-remove
                  "niwa-plugin-lifecycle" (name &optional workspace))
(declare-function niwa-plugins-update
                  "niwa-plugin-lifecycle" (name &optional workspace))
(declare-function niwa-plugins-check-updates
                  "niwa-plugin-lifecycle" (&optional on-demand done))
(autoload 'niwa-plugins-install "niwa-plugin-lifecycle")
(autoload 'niwa-plugins-remove "niwa-plugin-lifecycle")
(autoload 'niwa-plugins-update "niwa-plugin-lifecycle")
(autoload 'niwa-plugins-check-updates "niwa-plugin-lifecycle")

;; `niwa-plugin-registry'
(declare-function niwa-plugin-description
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-error-manifest
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-error-message
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-error-p
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-error-root
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-hooks
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-name
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-p
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-root
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-shadowed
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-skills-dir
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-version
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugins-active-shadowed-source
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-disable
                  "niwa-plugin-registry" (plugin-name &optional workspace))
(declare-function niwa-plugins-disable-hooks
                  "niwa-plugin-registry" (plugin-name &optional workspace))
(declare-function niwa-plugins-enable
                  "niwa-plugin-registry" (plugin-name &optional workspace))
(declare-function niwa-plugins-enable-hooks
                  "niwa-plugin-registry" (plugin-name &optional workspace))
(declare-function niwa-plugins-enabled-p
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-find
                  "niwa-plugin-registry" (name &optional workspace))
(declare-function niwa-plugins-hook-consent-summary
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-hook-rule-events
                  "niwa-plugin-registry" (plugin))
(declare-function niwa-plugins-hooks-enabled-p
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-hooks-status
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-item-name
                  "niwa-plugin-registry" (item))
(declare-function niwa-plugins-items
                  "niwa-plugin-registry" (&optional workspace))
(declare-function niwa-plugins-manifest-file
                  "niwa-plugin-registry" (root))
(declare-function niwa-plugins-pending-consent
                  "niwa-plugin-registry" (&optional workspace))
(declare-function niwa-plugins-update-available-p
                  "niwa-plugin-registry" (plugin))
(declare-function niwa-plugins-update-check-error
                  "niwa-plugin-registry" (plugin))
(declare-function niwa-plugins-plugin-data-dir
                  "niwa-plugin-registry"
                  (plugin-name &optional workspace))
(declare-function niwa-plugins-skill-count
                  "niwa-plugin-registry" (plugin))

;; `niwa-plugins'
(declare-function niwa-plugins-current-workspace "niwa-plugins" ())
(declare-function niwa-plugins-refresh-session
                  "niwa-plugins" (&optional context))

;; `niwa-structs'
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())

;;; Plugin list buffer

(defconst niwa-plugins-list-buffer-name "*niwa plugins*"
  "Name of the plugin management buffer.")

(defconst niwa-plugins-help-buffer-name "*niwa plugin help*"
  "Name of the plugin cockpit help buffer.")

(defun niwa-plugins-pending-consent-message (&optional workspace)
  "Return a user-facing pending hook consent message for WORKSPACE."
  (when-let* ((pending (niwa-plugins-pending-consent workspace)))
    (format "plugin hook consent pending for %s; open /plugin to review"
            (string-join (mapcar #'niwa-plugin-name pending) ", "))))

(defun niwa-plugins-notify-pending-consent (&optional workspace)
  "Warn when WORKSPACE has enabled plugins with pending hook consent.
The warning is raised once per workspace and message; later session
starts log it quietly to *Messages*."
  (when-let* ((message (niwa-plugins-pending-consent-message workspace)))
    (niwa--warn-once
     (niwa-diagnostic-scope (or workspace (current-buffer)))
     (list 'plugin-pending-consent
           (and workspace (niwa-workspace-root workspace))
           message)
     "%s" message)
    t))

(defun niwa-plugins-message-local-result (scope result)
  "Present plugin RESULT locally under SCOPE, attributing only known failures."
  (if (or (cl-some (lambda (prefix) (string-prefix-p prefix result))
                  '("Failed to install plugin " "Failed to update plugin "
                    "Invalid plugin target:" "Unknown plugin:" "Plugin path "
                    "No current workspace for plugin state."
                    "Plugin registry reload failed:"))
          (string-match-p "\\`Plugin .+\\(?: is not managed by niwa;\\| is not enabled[.]\\'\\)" result))
      (niwa-diagnostic-message scope "%s" result)
    (message "%s" result)))

(defun niwa-plugins--reload (&optional context)
  "Reload plugin-visible skills for CONTEXT's session when possible."
  (let ((result (niwa-plugins-refresh-session context)))
    (cond
     ((eq result t)
      "Plugin registry reloaded. Refreshed current session skills.")
     ((stringp result)
      (format "Plugin registry reload failed: %s." result))
     (t
      "Plugin registry reloaded. No active session skills to refresh."))))

(defun niwa-plugins--with-refresh (message)
  "Refresh current session skills and return MESSAGE."
  (niwa-plugins-refresh-session)
  message)

(defun niwa-plugins--plugin-source-label (plugin)
  "Return a compact source label for PLUGIN."
  (abbreviate-file-name (niwa-plugin-root plugin)))

(defun niwa-plugins-list--item-id (item)
  "Return stable tabulated-list id for plugin cockpit ITEM."
  (cond
   ((niwa-plugin-p item) (niwa-plugin-name item))
   ((niwa-plugin-error-p item)
    (concat "error:" (niwa-plugin-error-root item)))))

(defun niwa-plugins--state-marker (plugin &optional workspace)
  "Return the compact table state marker for PLUGIN in WORKSPACE."
  (cond
   ((equal (niwa-plugins-hooks-status plugin workspace) "needs-consent")
    (propertize "!" 'face 'warning))
   ((niwa-plugin-shadowed plugin)
    (propertize "*" 'face 'warning))
   (t "")))

(defun niwa-plugins--status-cell (status)
  "Return propertized compact STATUS text."
  (pcase status
    ("on" (propertize status 'face 'success))
    ("needs-consent" (propertize status 'face 'warning))
    ((or "off" "none") (propertize status 'face 'shadow))
    (_ status)))

(defun niwa-plugins--update-note (plugin)
  "Return the cached update-check note for PLUGIN, or nil."
  (cond
   ((niwa-plugins-update-available-p plugin)
    (propertize "update available" 'face 'warning))
   ((niwa-plugins-update-check-error plugin)
    (propertize "check failed" 'face 'shadow))))

(defun niwa-plugins--source-cell (plugin)
  "Return PLUGIN's source label followed by its update note, if any."
  (if-let* ((note (niwa-plugins--update-note plugin)))
      (concat (niwa-plugins--plugin-source-label plugin) "  " note)
    (niwa-plugins--plugin-source-label plugin)))

(defun niwa-plugins--shadowed-lines (plugin &optional workspace)
  "Return shadowed-source lines for PLUGIN in WORKSPACE."
  (let ((active-shadow (niwa-plugins-active-shadowed-source
                        plugin workspace)))
    (mapcar
     (lambda (shadow)
       (format "  shadowed%s: %s"
               (if (eq shadow active-shadow) " active" "")
               (niwa-plugins--plugin-source-label shadow)))
     (niwa-plugin-shadowed plugin))))

(defun niwa-plugins-list--error-entry (error)
  "Return a `tabulated-list-mode' row for plugin metadata ERROR."
  (list (niwa-plugins-list--item-id error)
        (vector
         (propertize "!" 'face 'warning)
         (propertize (niwa-plugins-item-name error) 'face 'warning)
         ""
         (propertize "error" 'face 'error)
         ""
         ""
         (abbreviate-file-name (niwa-plugin-error-root error)))))

(defun niwa-plugins-list--entry (item context)
  "Return a `tabulated-list-mode' row for plugin cockpit ITEM in CONTEXT."
  (if (niwa-plugin-error-p item)
      (niwa-plugins-list--error-entry item)
    (let* ((workspace (niwa-cockpit-context-workspace context))
           (plugin item)
           (name (niwa-plugin-name plugin))
           (enabled (if (niwa-plugins-enabled-p plugin workspace)
                        "on"
                      "off"))
           (hooks (niwa-plugins-hooks-status plugin workspace)))
      (list name
            (vector
             (niwa-plugins--state-marker plugin workspace)
             name
             (or (niwa-plugin-version plugin) "")
             (niwa-plugins--status-cell enabled)
             (niwa-plugins--status-cell hooks)
             (number-to-string (niwa-plugins-skill-count plugin))
             (niwa-plugins--source-cell plugin))))))

(defun niwa-plugins-list--root-label (context)
  "Return CONTEXT's plugin cockpit workspace root label."
  (if-let* ((workspace (niwa-cockpit-context-workspace context)))
      (abbreviate-file-name
       (niwa-workspace-root workspace))
    "no workspace"))

(defun niwa-plugins-list--selected-item ()
  "Return the selected plugin cockpit item, or nil."
  (niwa-cockpit-surface-selected t))

(defun niwa-plugins-list--plugin-at-point ()
  "Return the plugin at point, or signal a user error."
  (let ((item (niwa-plugins-list--selected-item)))
    (cond
     ((niwa-plugin-p item) item)
     ((niwa-plugin-error-p item)
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope
        (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
       "Plugin metadata cannot be read: %s"
                  (niwa-plugin-error-message item)))
     (t
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope
        (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
       "No plugin on this line")))))

(defun niwa-plugins-list--selected-name ()
  "Return the selected plugin name, or signal a user error."
  (niwa-plugin-name (niwa-plugins-list--plugin-at-point)))

(defun niwa-plugins-list--header-line (items context)
  "Return the plugin cockpit header line for ITEMS and CONTEXT."
  (let ((total 0)
        (enabled 0)
        (hooks 0)
        (workspace (niwa-cockpit-context-workspace context)))
    (dolist (item items)
      (when (niwa-plugin-p item)
        (setq total (1+ total))
        (when (niwa-plugins-enabled-p item workspace)
          (setq enabled (1+ enabled)))
        (when (niwa-plugins-hooks-enabled-p item workspace)
          (setq hooks (1+ hooks)))))
    (niwa-cockpit-format-header
     "plugins"
     (niwa-plugins-list--root-label context)
     (format "%d/%d enabled · %d with hooks" enabled total hooks))))

(defun niwa-plugins-list--collect (context)
  "Return plugin cockpit items for CONTEXT."
  (niwa-plugins-items
   (niwa-cockpit-context-workspace context)))

(defun niwa-plugins-list-refresh ()
  "Refresh the current plugin management buffer."
  (interactive)
  (niwa-cockpit-surface-refresh))

(defun niwa-plugins-list-toggle-enabled ()
  "Toggle activation for the plugin at point."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (plugin (niwa-plugins-list--plugin-at-point))
         (workspace (niwa-cockpit-context-workspace
                     context))
         (name (niwa-plugin-name plugin)))
    (if (niwa-plugins-enabled-p plugin workspace)
        (progn
          (niwa-plugins-disable name workspace)
          (message "niwa: disabled plugin %s" name))
      (if (niwa-plugins-enable name workspace)
          (message "niwa: enabled plugin %s" name)
        (message "niwa: enable cancelled for plugin %s" name)))
    (niwa-plugins-refresh-session context)
    (niwa-cockpit-surface-refresh name)))

(defun niwa-plugins-list-toggle-hooks ()
  "Toggle hooks for the plugin at point."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (selected (niwa-plugins-list--plugin-at-point))
         (workspace (niwa-cockpit-context-workspace
                     context))
         (name (niwa-plugin-name selected)))
    (cond
     ((not (niwa-plugin-hooks selected))
      (message "niwa: plugin %s declares no hooks" name))
     ((not (niwa-plugins-enabled-p
            selected workspace))
      (message "niwa: plugin %s is not enabled" name))
     ((niwa-plugins-hooks-enabled-p
       selected workspace)
      (niwa-plugins-disable-hooks
       name workspace)
      (message "niwa: disabled hooks for plugin %s" name))
     ((niwa-plugins-enable-hooks
       name workspace)
      (message "niwa: enabled hooks for plugin %s" name))
     (t
      (message "niwa: hook enable cancelled for plugin %s" name)))
    (niwa-plugins-refresh-session context)
    (niwa-cockpit-surface-refresh name)))

(defun niwa-plugins-list-update ()
  "Update the plugin at point."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (name (niwa-plugins-list--selected-name))
         (workspace (niwa-cockpit-context-workspace
                     context))
         (scope (niwa-diagnostic-scope workspace))
         (message (niwa-plugins-update
                   name
                   workspace)))
    (niwa-plugins-refresh-session context)
    (niwa-cockpit-surface-refresh name)
    (niwa-plugins-message-local-result scope message)))

(defun niwa-plugins-list-check-updates ()
  "Check managed plugins for upstream updates now, ignoring the interval.
The check runs in the background; the cockpit refreshes in place when it
finishes, keeping the selected row."
  (interactive)
  (let ((buffer (current-buffer)))
    (niwa-plugins-check-updates
     t (lambda ()
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (niwa-cockpit-surface-refresh)))))
    (message "niwa: checking plugins for updates...")))

(defun niwa-plugins-list-remove ()
  "Remove the plugin at point."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (name (niwa-plugins-list--selected-name))
         (workspace (niwa-cockpit-context-workspace
                     context))
         (scope (niwa-diagnostic-scope workspace))
         (message (niwa-plugins-remove
                   name
                   workspace)))
    (niwa-plugins-refresh-session context)
    (niwa-cockpit-surface-refresh name)
    (niwa-plugins-message-local-result scope message)))

(defun niwa-plugins-list--installed-name (message)
  "Return the plugin name from install MESSAGE when it is present."
  (and (string-match "\\`Installed plugin \\([^ ]+\\)\\." message)
       (match-string 1 message)))

(defun niwa-plugins-list-install (target)
  "Install plugin TARGET and refresh the cockpit."
  (interactive (list (read-string "Install plugin OWNER/REPO: ")))
  (let* ((context (niwa-cockpit-surface-context))
         (selected (tabulated-list-get-id))
         (scope (niwa-diagnostic-scope (niwa-cockpit-context-workspace context)))
         (message (niwa-plugins-install target))
         (name (niwa-plugins-list--installed-name message)))
    (niwa-plugins-refresh-session context)
    (niwa-cockpit-surface-refresh (or name selected))
    (niwa-plugins-message-local-result scope message)))

(defun niwa-plugins-list-reload ()
  "Reload plugin-visible session skills and refresh the cockpit."
  (interactive)
  (let* ((context (niwa-cockpit-surface-context))
         (name (tabulated-list-get-id))
         (scope (niwa-diagnostic-scope (niwa-cockpit-context-workspace context)))
         (message (niwa-plugins--reload context)))
    (niwa-cockpit-surface-refresh name)
    (niwa-plugins-message-local-result scope message)))

(defun niwa-plugins-list-open-source ()
  "Open the selected plugin source directory in Dired."
  (interactive)
  (let* ((item (or (niwa-plugins-list--selected-item)
                   (niwa-diagnostic-user-error
                    (niwa-diagnostic-scope
                     (niwa-cockpit-context-workspace
                      (niwa-cockpit-surface-context)))
                    "No plugin on this line")))
         (root (cond
                ((niwa-plugin-p item) (niwa-plugin-root item))
                ((niwa-plugin-error-p item)
                 (niwa-plugin-error-root item)))))
    (unless (and root (file-directory-p root))
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope
        (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
       "Plugin source is not readable: %s" root))
    (dired root)))

(defun niwa-plugins-list--detail-report (plugin context)
  "Return the complete information report for PLUGIN in CONTEXT."
  (let* ((name (niwa-plugin-name plugin))
         (workspace (niwa-cockpit-context-workspace context))
         (hooks (niwa-plugins-hooks-status plugin workspace))
         (events (niwa-plugins-hook-rule-events plugin)))
    (list :title "Plugin" :subtitle name :identity (niwa-plugin-root plugin)
          :sections
          (list
           (list :id 'plugin :title "Plugin"
                 :body (niwa-report-fields
                        (list "Name" name)
                        (list "Version" (or (niwa-plugin-version plugin) "unspecified"))
                        (when-let* ((description (niwa-plugin-description plugin))) (list "Description" description))
                        (list "Status" (if (niwa-plugins-enabled-p plugin workspace) "enabled" "disabled"))))
           (list :id 'capabilities :title "Hooks & skills"
                 :body (niwa-report-fields
                        (list "Hooks" (concat hooks (if (equal hooks "needs-consent") " (pending hook consent)" "")))
                        (list "Events" (if events (string-join events ", ") "none"))
                        (list "Skills" (format "%d%s" (niwa-plugins-skill-count plugin)
                                               (if-let* ((dir (niwa-plugin-skills-dir plugin)))
                                                   (format " from %s" (abbreviate-file-name dir)) "")))))
           (list :id 'locations :title "Locations"
                 :body (niwa-report-fields
                        (list "Source" (abbreviate-file-name (niwa-plugin-root plugin)))
                        (list "Manifest" (abbreviate-file-name (niwa-plugins-manifest-file (niwa-plugin-root plugin))))
                        (when workspace (list "Data" (abbreviate-file-name (niwa-plugins-plugin-data-dir name workspace))))))
           (list :id 'shadowed :title "Shadowed sources" :folded (and (niwa-plugin-shadowed plugin) t)
                 :body (if (niwa-plugin-shadowed plugin)
                           (string-join (niwa-plugins--shadowed-lines plugin workspace) "\n")
                         "No shadowed sources."))
           (list :id 'consent :title "Hook consent summary"
                 :body (if (niwa-plugin-hooks plugin)
                           (niwa-plugins-hook-consent-summary plugin workspace)
                         "No plugin hooks."))))))

(defun niwa-plugins-list--error-detail-report (error)
  "Return the information report for plugin metadata ERROR."
  (list :title "Plugin metadata error" :subtitle (niwa-plugins-item-name error)
        :sections
        (list
         (list :id 'error :title "Error" :body (propertize (niwa-plugin-error-message error) 'face 'error))
         (list :id 'source :title "Source"
               :body (niwa-report-fields
                      (list "Name" (niwa-plugins-item-name error))
                      (list "Source" (abbreviate-file-name (niwa-plugin-error-root error)))
                      (list "Manifest" (abbreviate-file-name (niwa-plugin-error-manifest error))))))))

(defun niwa-plugins-list-details ()
  "Show details for the plugin at point."
  (interactive)
  (niwa-cockpit-surface-details))

(defconst niwa-plugins-list--surface
  `(:buffer-name ,niwa-plugins-list-buffer-name
    :label "plugin cockpit"
    :row-label "plugin"
    :mode niwa-plugins-list-mode
    :format [("State" 5 nil)
             ("Name" 24 t)
             ("Version" 12 t)
             ("Enabled" 8 t)
             ("Hooks" 14 t)
             ("Skills" 7 t)
             ("Source" 0 t)]
    :sort-key ("Name" . nil)
    :collect niwa-plugins-list--collect
    :entry niwa-plugins-list--entry
    :header niwa-plugins-list--header-line
    :details niwa-plugins-list--details-report
    :details-buffer "*niwa plugin details*"
    :help-buffer ,niwa-plugins-help-buffer-name
    :help-function niwa-plugins-list--help-report
    :keys (("e" "Enable or disable selected plugin"
            niwa-plugins-list-toggle-enabled)
           ("h" "Toggle hooks for selected plugin"
            niwa-plugins-list-toggle-hooks)
           ("+" "Install GitHub plugin by OWNER/REPO"
            niwa-plugins-list-install)
           ("u" "Update selected plugin"
            niwa-plugins-list-update)
           ("U" "Check managed plugins for updates"
            niwa-plugins-list-check-updates)
           ("r" "Reload plugin-visible session skills"
            niwa-plugins-list-reload)
           ("x" "Remove selected managed plugin"
            niwa-plugins-list-remove)
           ("o" "Open selected plugin source in Dired"
            niwa-plugins-list-open-source)))
  "Cockpit surface spec for the plugin list.")

(defun niwa-plugins-list--help-report (&optional _context)
  "Return the complete cockpit help report."
  (list :title "Plugin help"
        :sections (list (list :id 'keys :title "Keys"
                              :body (niwa-cockpit-surface-key-help-text niwa-plugins-list--surface))
			(list :id 'slash :title "Slash equivalents" :body "/plugin enable NAME, /plugin disable NAME\n/plugin hooks NAME on, /plugin hooks NAME off\n/plugin install OWNER/REPO, /plugin update NAME\n/plugin reload, /plugin remove NAME, /plugin uninstall NAME"))))

(defun niwa-plugins-list-help ()
  "Open plugin cockpit help."
  (interactive)
  (niwa-cockpit-show-help
   niwa-plugins-help-buffer-name
   (niwa-plugins-list--help-report)))

(defun niwa-plugins-list-quit ()
  "Close the plugin cockpit and restore the invoking layout."
  (interactive)
  (niwa-cockpit-quit "plugin cockpit"))

(defun niwa-plugins-list--details-report (item context)
  "Return the information report for plugin cockpit ITEM in CONTEXT."
  (if (niwa-plugin-error-p item)
      (niwa-plugins-list--error-detail-report item)
    (niwa-plugins-list--detail-report item context)))

(define-derived-mode niwa-plugins-list-mode tabulated-list-mode
  "niwa-plugins"
  "Major mode for managing niwa plugins."
  (niwa-cockpit-setup-tabulated-surface
   niwa-plugins-list--surface))

(defun niwa-plugins-list-open (&optional context)
  "Open the plugin management buffer for CONTEXT."
  (let ((context (or context (niwa-cockpit-current-context))))
    (niwa-cockpit-open-surface niwa-plugins-list--surface context)))


;;
;;; Slash command

(defconst niwa-plugins--usage
  (concat "Usage: /plugin list | enable NAME | disable NAME | "
          "hooks enable NAME | hooks disable NAME | hooks NAME on | "
          "hooks NAME off | install TARGET | update NAME | "
          "remove NAME | uninstall NAME | reload")
  "Usage text for `/plugin'.")

(defun niwa-plugins--known-or-message (name &optional workspace)
  "Return installed plugin NAME, or a user-facing error string."
  (or (niwa-plugins-find name workspace)
      (format "Unknown plugin: %s." name)))

(defun niwa-plugins-slash-command (args)
  "Run local `/plugin' command from ARGS.
Return a user-facing result string."
  (let* ((parts (split-string (string-trim (or args "")) "[ \t\n]+" t))
         (parts (pcase parts
                  (`("hooks" "enable" ,name) `("hooks" ,name "on"))
                  (`("hooks" "disable" ,name) `("hooks" ,name "off"))
                  (_ parts)))
         (workspace (niwa-plugins-current-workspace)))
    (pcase parts
      ((or `() `("list"))
       (niwa-menu-open 'plugins)
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
       (let ((plugin (niwa-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (if (niwa-plugins-enable name workspace)
               (niwa-plugins--with-refresh
                (format "Enabled plugin %s." name))
             (format "Enable cancelled for plugin %s." name)))))
      (`("disable" ,name)
       (let ((plugin (niwa-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (niwa-plugins-disable name workspace)
           (niwa-plugins--with-refresh
            (format "Disabled plugin %s." name)))))
      (`("hooks" ,name "on")
       (let ((plugin (niwa-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (cond
            ((not (niwa-plugin-hooks plugin))
             (format "Plugin %s declares no hooks." name))
            ((not (niwa-plugins-enabled-p plugin workspace))
             (format "Plugin %s is not enabled." name))
            ((niwa-plugins-enable-hooks name workspace)
             (niwa-plugins--with-refresh
              (format "Enabled hooks for plugin %s." name)))
            (t
             (format "Hook enable cancelled for plugin %s." name))))))
      (`("hooks" ,name "off")
       (let ((plugin (niwa-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (niwa-plugins-disable-hooks name workspace)
           (niwa-plugins--with-refresh
            (format "Disabled hooks for plugin %s." name)))))
      (`("install" ,target)
       (niwa-plugins--with-refresh
        (niwa-plugins-install target)))
      (`("update" ,name)
       (niwa-plugins--with-refresh
        (niwa-plugins-update name workspace)))
      ((or `("remove" ,name) `("uninstall" ,name))
       (niwa-plugins--with-refresh
        (niwa-plugins-remove name workspace)))
      (`("reload") (niwa-plugins--reload))
      (_ niwa-plugins--usage))))



(provide 'niwa-plugin-ui)
;;; niwa-plugin-ui.el ends here
