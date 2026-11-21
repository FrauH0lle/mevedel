;;; niwa-plugin-registry.el -- Plugin discovery and trust -*- lexical-binding: t -*-

;;; Commentary:

;; Discovers Codex-style manifests, validates plugin-relative paths, and owns
;; workspace-local enablement plus executable-hook consent state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'niwa-structs)

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(autoload 'niwa-cockpit-context-session "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")

;; `niwa-hooks'
(declare-function niwa-hooks-invalidate-config "niwa-hooks" ())
(declare-function niwa-hooks-read-config-file
                  "niwa-hooks" (file &optional content))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-publish-text
                  "niwa-session-artifacts"
                  (session path content &optional coding))
(autoload 'niwa-session-artifacts-publish-text
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa-user-dir)

;; `niwa-utilities'
(declare-function niwa--write-file-atomically
                  "niwa-utilities" (path content &optional coding mode))
(autoload 'niwa--write-file-atomically "niwa-utilities")

;; `niwa-workspace'
(declare-function niwa-workspace-state-dir
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")

;; `subr'
(defvar read-eval)

(defcustom niwa-plugin-extra-roots nil
  "Additional local Codex plugin roots to scan.
Each entry may be a direct plugin root or a directory containing plugin
roots below it."
  :type '(repeat directory)
  :group 'niwa)

(defconst niwa-plugins--default-install-directory
  (file-name-concat "~" ".agents" "plugins")
  "Default value for `niwa-plugin-install-directory'.")

(defcustom niwa-plugin-install-directory
  niwa-plugins--default-install-directory
  "Directory where `/plugin install' writes new plugins."
  :type 'directory
  :group 'niwa)

(defcustom niwa-plugins-update-check-interval nil
  "Days between background checks for upstream plugin updates, or nil.
Nil (the default) never checks.  A number enables an asynchronous
`git ls-remote' against each managed plugin's origin once that many
days have passed since the last check, started on an idle timer after
the registry is first used.  A check only notifies; it never updates a
plugin.  The cockpit's \"Check for updates\" action ignores this setting."
  :type '(choice (const :tag "Never check" nil)
                 (number :tag "Days between checks"))
  :group 'niwa)


;;
;;; Structs

(cl-defstruct (niwa-plugin (:constructor niwa-plugin--create))
  "Installed Codex-style plugin manifest."
  name
  version
  description
  root
  skills-dir
  hooks-file
  hooks
  shadowed
  enabled-p
  hooks-enabled-p)

(cl-defstruct (niwa-plugin-error (:constructor niwa-plugin-error--create))
  "Plugin root whose manifest metadata could not be used."
  name
  root
  manifest
  message)

;;
;;; Paths

(defun niwa-plugins--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory
   (expand-file-name (substitute-in-file-name directory))))

(defun niwa-plugins-dir ()
  "Return the global directory where new plugins are installed."
  (niwa-plugins--normalize-directory niwa-plugin-install-directory))

(defun niwa-plugins--global-niwa-dir ()
  "Return the global niwa-specific plugin directory."
  (niwa-plugins--normalize-directory
   (file-name-concat niwa-user-dir "plugins")))

(defun niwa-plugins-state-file (&optional workspace)
  "Return the persistent plugin state file for WORKSPACE.
Return nil when WORKSPACE is nil."
  (when workspace
    (file-name-concat (niwa-workspace-state-dir workspace) "plugins.el")))

(defun niwa-plugins--workspace-plugins-dir (workspace resource-dir)
  "Return WORKSPACE plugin directory under RESOURCE-DIR."
  (file-name-concat (niwa-workspace-root workspace)
                    resource-dir
                    "plugins"))

(defun niwa-plugins-plugin-data-dir (plugin-name &optional workspace)
  "Return persistent data directory for PLUGIN-NAME in WORKSPACE."
  (unless workspace
    (error "No workspace for plugin data"))
  (file-name-concat (niwa-workspace-state-dir workspace)
                    "state" "plugin-data"
                    plugin-name))


;;
;;; Manifest discovery

(defun niwa-plugins-manifest-file (root)
  "Return plugin manifest path under ROOT."
  (file-name-concat root ".codex-plugin" "plugin.json"))

(defun niwa-plugins--string-or-nil (value)
  "Return VALUE if it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun niwa-plugins--json-string (key alist)
  "Return string field named by symbol KEY from parsed JSON ALIST."
  (niwa-plugins--string-or-nil
   (alist-get key alist)))

(defun niwa-plugins--safe-name-p (name)
  "Return non-nil when NAME is safe for plugin identifiers."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9_.-]+\\'" name)
       (not (member name '("." ".." "user" "project" "managed" "bundled")))))

(defun niwa-plugins--path-in-root-p (path root)
  "Return non-nil when PATH stays inside ROOT."
  (let ((path (file-truename path))
        (root (file-name-as-directory (file-truename root))))
    (string-prefix-p root (file-name-as-directory path))))

(defun niwa-plugins--resolve-manifest-path (root path)
  "Resolve manifest PATH relative to plugin ROOT."
  (when-let* ((path (niwa-plugins--string-or-nil path))
              ((not (file-name-absolute-p path)))
              (expanded (expand-file-name path root))
              ((not (file-symlink-p expanded)))
              ((niwa-plugins--path-in-root-p expanded root)))
    expanded))

(defun niwa-plugins--normalize-manifest-hooks (root value missing)
  "Normalize manifest hook VALUE for plugin ROOT.
Return a list of plists, each with a `:file' path."
  (cond
   ((eq value missing)
    (when-let* ((default (niwa-plugins--resolve-manifest-path
                          root "hooks/hooks.json"))
                ((file-readable-p default)))
      (list (list :file default))))
   ((stringp value)
    (when-let* ((file (niwa-plugins--resolve-manifest-path root value)))
      (list (list :file file))))))

(defun niwa-plugins--first-hook-file (hooks)
  "Return the first file path in normalized HOOKS."
  (cl-some (lambda (entry) (plist-get entry :file)) hooks))

(defun niwa-plugins--read-manifest-result (root &optional workspace)
  "Read plugin manifest under ROOT.
Return a `niwa-plugin' or `niwa-plugin-error', or nil when ROOT
does not contain a readable Codex plugin manifest."
  (let ((file (niwa-plugins-manifest-file root)))
    (when (file-readable-p file)
      (let* ((root (file-name-as-directory (expand-file-name root)))
             (fallback-name
              (file-name-nondirectory (directory-file-name root))))
        (condition-case err
            (let* ((json (with-temp-buffer
                           (insert-file-contents file)
                           (json-parse-buffer
                            :object-type 'alist
                            :array-type 'list
                            :null-object nil
                            :false-object nil)))
                   (name (or (niwa-plugins--json-string 'name json)
                             fallback-name))
                   (missing (make-symbol "missing"))
                   (skills (niwa-plugins--json-string 'skills json))
                   (hooks (niwa-plugins--normalize-manifest-hooks
                           root
                           (alist-get 'hooks json missing)
                           missing)))
              (if (niwa-plugins--safe-name-p name)
                  (niwa-plugin--create
                   :name name
                   :version (niwa-plugins--json-string 'version json)
                   :description (niwa-plugins--json-string
                                 'description json)
                   :root root
                   :skills-dir (niwa-plugins--resolve-manifest-path
                                root skills)
                   :hooks-file (niwa-plugins--first-hook-file hooks)
                   :hooks hooks
                   :enabled-p (niwa-plugins--state-enabled-p
                               name workspace root)
                   :hooks-enabled-p (niwa-plugins--state-hooks-enabled-p
                                     name workspace root hooks))
                (niwa-plugin-error--create
                 :name name
                 :root root
                 :manifest file
                 :message (format "Unsafe plugin name: %s" name))))
          (error
           (niwa-plugin-error--create
            :name fallback-name
            :root root
            :manifest file
            :message (error-message-string err))))))))

(defun niwa-plugins-read-manifest (root &optional workspace)
  "Read plugin manifest under ROOT.
Return nil when ROOT does not contain a usable Codex plugin manifest."
  (let ((entry (niwa-plugins--read-manifest-result root workspace)))
    (and (niwa-plugin-p entry) entry)))

(defun niwa-plugins--canonical-root (root)
  "Return deterministic directory form for plugin ROOT."
  (file-name-as-directory (expand-file-name root)))

(defconst niwa-plugins-staging-prefix ".niwa-staging-"
  "Filename prefix marking an in-flight plugin clone.
A clone is staged beside its destination so publishing it is a
same-filesystem rename, which puts it inside the tree discovery walks.
Discovery skips these names so a half-built clone is never adopted.")

(defun niwa-plugins-staging-name-p (path)
  "Return non-nil when PATH names an in-flight plugin clone."
  (string-prefix-p niwa-plugins-staging-prefix
                   (file-name-nondirectory (directory-file-name path))))

(defun niwa-plugins--collect-roots-under (root)
  "Return plugin roots at or below ROOT.
Do not descend into a directory once it is recognized as a plugin root."
  (let (roots)
    (cl-labels
        ((walk (dir)
           (when (file-directory-p dir)
             (let ((dir (niwa-plugins--canonical-root dir)))
               (cond
                ((file-readable-p (niwa-plugins-manifest-file dir))
                 (push dir roots))
                (t
                 (dolist (entry (directory-files
                                  dir t directory-files-no-dot-files-regexp))
                   (when (and (file-directory-p entry)
                              (not (file-symlink-p entry))
                              (not (niwa-plugins-staging-name-p entry)))
                     (walk entry)))))))))
      (walk root))
    (nreverse roots)))

(defun niwa-plugins--root-sources (&optional workspace)
  "Return plugin root containers in precedence order for WORKSPACE."
  (append
   (when workspace
     (list (niwa-plugins--workspace-plugins-dir workspace ".niwa")
           (niwa-plugins--workspace-plugins-dir workspace ".agents")))
   (list (niwa-plugins--global-niwa-dir)
         (niwa-plugins-dir))
   niwa-plugin-extra-roots))

(defun niwa-plugins--plugin-roots (&optional workspace)
  "Return deduplicated plugin roots in precedence order for WORKSPACE."
  (let ((seen (make-hash-table :test #'equal))
        roots)
    (dolist (root (niwa-plugins--root-sources workspace))
      (dolist (plugin-root (niwa-plugins--collect-roots-under root))
        (let ((key (file-name-as-directory (file-truename plugin-root))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push plugin-root roots)))))
    (nreverse roots)))

(defun niwa-plugins--select-duplicate-names (plugins)
  "Select highest-precedence entries from PLUGINS.
Shadowed entries are retained in the winner's `shadowed' slot."
  (let ((by-name (make-hash-table :test #'equal))
        kept)
    (dolist (plugin plugins)
      (push plugin (gethash (niwa-plugin-name plugin) by-name)))
    (maphash
     (lambda (_name matches)
       (let* ((matches (nreverse matches))
              (winner (car matches)))
         (setf (niwa-plugin-shadowed winner) (cdr matches))
         (push winner kept)))
     by-name)
    (sort kept
          (lambda (a b)
            (string< (niwa-plugin-name a)
                     (niwa-plugin-name b))))))

(defun niwa-plugins--collect (&optional workspace)
  "Return discovered plugin metadata for WORKSPACE."
  (niwa-plugins--schedule-update-check)
  (let (plugins errors)
    (dolist (root (niwa-plugins--plugin-roots workspace))
      (let ((item (niwa-plugins--read-manifest-result root workspace)))
        (cond
         ((niwa-plugin-p item) (push item plugins))
         ((niwa-plugin-error-p item) (push item errors)))))
    (let* ((winners (niwa-plugins--select-duplicate-names
                     (nreverse plugins)))
           (errors (nreverse errors))
           (enabled-count
            (cl-count-if
             (lambda (plugin)
               (niwa-plugins-enabled-p plugin workspace))
             winners)))
      (list :winners winners
            :errors errors
            :enabled-count enabled-count
            :total-count (length winners)
            :error-count (length errors)))))

(defun niwa-plugins-list (&optional workspace)
  "Return installed Codex-style plugins.
State slots are resolved for WORKSPACE when provided."
  (plist-get (niwa-plugins--collect workspace) :winners))

(defun niwa-plugins-item-name (item)
  "Return display name for plugin cockpit ITEM."
  (cond
   ((niwa-plugin-p item) (niwa-plugin-name item))
   ((niwa-plugin-error-p item)
    (or (niwa-plugin-error-name item) "metadata error"))))

(defun niwa-plugins-items (&optional workspace)
  "Return plugin cockpit items for WORKSPACE.
Items include usable plugin manifests and visible metadata errors."
  (let ((collection (niwa-plugins--collect workspace)))
    (sort (append (plist-get collection :winners)
                  (plist-get collection :errors))
          (lambda (a b)
            (string< (niwa-plugins-item-name a)
                     (niwa-plugins-item-name b))))))

(defun niwa-plugins-find (name &optional workspace)
  "Return installed plugin named NAME, or nil."
  (cl-find name (niwa-plugins-list workspace)
           :key #'niwa-plugin-name :test #'equal))


;;
;;; Persistent state

(defun niwa-plugins--read-state (&optional workspace)
  "Read persistent plugin state for WORKSPACE."
  (when-let* ((file (niwa-plugins-state-file workspace))
              ((file-readable-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((read-eval nil))
            (let ((state (read (current-buffer))))
              (and (listp state) state))))
      (error nil))))

(defun niwa-plugins--state-session (workspace)
  "Return the live session authorized to mutate WORKSPACE plugin state."
  (let ((session
         (or (and (boundp 'niwa--session) niwa--session)
             (ignore-errors
               (niwa-cockpit-context-session
                (niwa-cockpit-current-context))))))
    (and session
         (eq workspace (niwa-session-workspace session))
         session)))

(defun niwa-plugins--state-text (state)
  "Return the durable plugin STATE file contents."
  (with-temp-buffer
    (insert ";; Niwa plugin state\n")
    (insert ";; Auto-generated, safe to edit\n\n")
    (pp state (current-buffer))
    (buffer-string)))

(defun niwa-plugins--write-state (state &optional workspace)
  "Persist plugin STATE for WORKSPACE."
  (let* ((file (or (niwa-plugins-state-file workspace)
                   (error "No workspace for plugin state")))
         (content (niwa-plugins--state-text state)))
    (if (file-remote-p file)
        (let ((session (or (niwa-plugins--state-session workspace)
                           (user-error
                            "Remote plugin state requires its live session"))))
          (niwa-session-artifacts-publish-text
           session file content 'utf-8-unix))
      ;; The file is the whole record of every plugin's activation and
      ;; hook consent, and `niwa-plugins--read-state' reads a
      ;; truncated file as nil, so a write that died in place would
      ;; silently disable everything.
      (niwa--write-file-atomically file content))))

(defun niwa-plugins--state-plist (name &optional workspace)
  "Return persisted state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (niwa-plugins--read-state workspace))))

(defun niwa-plugins-source-root (root &optional workspace)
  "Return the durable source identity for ROOT in WORKSPACE.
Project sources use workspace-relative paths; other sources stay absolute."
  (when root
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (workspace-root
            (and workspace
                 (file-name-as-directory
                  (expand-file-name (niwa-workspace-root workspace)))))
           (relative
            (and workspace-root
                 (equal (file-remote-p root)
                        (file-remote-p workspace-root))
                 (file-relative-name root workspace-root))))
      (if (and relative
               (not (file-name-absolute-p relative))
               (not (equal relative ".."))
               (not (string-prefix-p "../" relative)))
          relative
        root))))

(defun niwa-plugins-managed-roots ()
  "Return directories whose plugins niwa may update or remove."
  (delete-dups
   (mapcar #'niwa-plugins-source-root
           (list (niwa-plugins--global-niwa-dir)
                 (niwa-plugins-dir)))))

(defun niwa-plugins-same-root-p (a b &optional workspace)
  "Return non-nil when source roots A and B identify the same directory.
Relative identities are requalified through the live WORKSPACE root."
  (when (and a b)
    (let ((base (and workspace (niwa-workspace-root workspace))))
      (equal (file-name-as-directory (expand-file-name a base))
             (file-name-as-directory (expand-file-name b base))))))

(defun niwa-plugins--hook-rules-from-file (file)
  "Return normalized hook rules from FILE."
  (when (and (file-readable-p file)
             (or (fboundp 'niwa-hooks-read-config-file)
                 (require 'niwa-hooks nil t)))
    (niwa-hooks-read-config-file file)))

(defun niwa-plugins--hook-rules-from-hooks (hooks)
  "Return normalized hook rules declared by HOOKS."
  (let (rules)
    (dolist (entry hooks rules)
      (when-let* ((file (plist-get entry :file)))
        (setq rules
              (append rules
                      (niwa-plugins--hook-rules-from-file file)))))))

(defun niwa-plugins--hook-surface-from-hooks (hooks &optional workspace)
  "Return the hook consent surface declared by HOOKS in WORKSPACE."
  (let (items)
    (dolist (entry hooks)
      (when-let* ((file (plist-get entry :file)))
        (let ((source (niwa-plugins-source-root file workspace)))
          (push (list source) items)
          (dolist (rule (niwa-plugins--hook-rules-from-file file))
            (let ((event (symbol-name (car rule))))
              (dolist (group (cdr-safe rule))
                (let ((matcher (plist-get group :matcher)))
                  (dolist (handler (plist-get group :hooks))
                    (pcase (plist-get handler :type)
                      ('command
                       (push (list source event matcher 'command
                                   (plist-get handler :command))
                             items))
                      ('elisp
                       (push (list source event matcher 'elisp
                                   (plist-get handler :function))
                             items)))))))))))
    (sort (delete-dups items)
          (lambda (a b)
            (string< (prin1-to-string a)
                     (prin1-to-string b))))))

(defun niwa-plugins--hook-fingerprint-from-hooks
    (hooks &optional workspace)
  "Return a consent fingerprint for normalized plugin HOOKS in WORKSPACE."
  (when-let* ((surface (niwa-plugins--hook-surface-from-hooks
                        hooks workspace)))
    (secure-hash 'sha256 (prin1-to-string surface))))

(defun niwa-plugins--hook-fingerprint (plugin &optional workspace)
  "Return the current hook consent fingerprint for PLUGIN in WORKSPACE."
  (niwa-plugins--hook-fingerprint-from-hooks
   (niwa-plugin-hooks plugin)
   workspace))

(defun niwa-plugins--state-enabled-p (name &optional workspace root)
  "Return non-nil when plugin NAME from ROOT is enabled in WORKSPACE."
  (let ((state (niwa-plugins--state-plist name workspace)))
    (and (plist-get state :enabled)
         (niwa-plugins-same-root-p
          root
          (plist-get state :source-root)
          workspace))))

(defun niwa-plugins--state-hooks-enabled-p
    (name &optional workspace root hooks)
  "Return non-nil when plugin NAME from ROOT has hooks enabled in WORKSPACE."
  (let* ((state (niwa-plugins--state-plist name workspace))
         (fingerprint (niwa-plugins--hook-fingerprint-from-hooks
                       hooks workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (niwa-plugins--state-enabled-p name workspace root)
         (equal fingerprint (plist-get state :hooks-fingerprint)))))

(defun niwa-plugins-enabled-p (plugin &optional workspace)
  "Return non-nil when PLUGIN is enabled in WORKSPACE."
  (niwa-plugins--state-enabled-p
   (niwa-plugin-name plugin)
   workspace
   (niwa-plugin-root plugin)))

(defun niwa-plugins-hooks-enabled-p (plugin &optional workspace)
  "Return non-nil when hooks are enabled for PLUGIN in WORKSPACE."
  (niwa-plugins--state-hooks-enabled-p
   (niwa-plugin-name plugin)
   workspace
   (niwa-plugin-root plugin)
   (niwa-plugin-hooks plugin)))

(defun niwa-plugins--write-state-entry (name plist &optional workspace)
  "Persist plugin NAME state PLIST for WORKSPACE."
  (let* ((state (copy-tree (niwa-plugins--read-state workspace)))
         (entry (assoc name state)))
    (if entry
        (setcdr entry plist)
      (push (cons name plist) state))
    (niwa-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b))))
     workspace)
    ;; Enablement decides which plugin hooks resolve, so the memoized
    ;; hook configuration is stale from here.  Guarded rather than
    ;; required: hooks depends on plugins, not the other way around.
    (when (fboundp 'niwa-hooks-invalidate-config)
      (niwa-hooks-invalidate-config))))

(defun niwa-plugins-remove-state-for-source
    (name root &optional workspace)
  "Remove NAME state only when it points at source ROOT."
  (when-let* ((state (niwa-plugins--read-state workspace))
              (entry (assoc name state))
              ((niwa-plugins-same-root-p
                root
                (plist-get (cdr entry) :source-root)
                workspace)))
    (niwa-plugins--write-state
     (cl-remove name (copy-tree state)
                :key #'car
                :test #'equal)
     workspace)))

(defun niwa-plugins-transfer-state-entry
    (old-name new-name plist &optional workspace)
  "Move plugin state PLIST from OLD-NAME to NEW-NAME in WORKSPACE."
  (let (state)
    (dolist (entry (copy-tree (niwa-plugins--read-state workspace)))
      (unless (member (car entry) (list old-name new-name))
        (push entry state)))
    (push (cons new-name plist) state)
    (niwa-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b))))
     workspace)))

(defun niwa-plugins-enabled (&optional workspace)
  "Return installed plugins enabled in WORKSPACE."
  (cl-remove-if-not
   (lambda (plugin) (niwa-plugins-enabled-p plugin workspace))
   (plist-get (niwa-plugins--collect workspace) :winners)))

(defun niwa-plugins-count-label (&optional workspace)
  "Return enabled/total plugin count label for WORKSPACE."
  (if workspace
      (let ((collection (niwa-plugins--collect workspace)))
        (format "%d/%d"
                (plist-get collection :enabled-count)
                (plist-get collection :total-count)))
    "0/0"))

(defun niwa-plugins-state-entry (plugin &optional workspace)
  "Return persisted state plist for PLUGIN in WORKSPACE, with defaults applied."
  (let* ((state (copy-sequence
                 (or (niwa-plugins--state-plist
                      (niwa-plugin-name plugin) workspace)
                     '(:enabled nil :hooks-enabled nil)))))
    (setq state
          (plist-put state
                     :source-root
                     (niwa-plugins-source-root
                      (niwa-plugin-root plugin)
                      workspace)))
    state))

(defun niwa-plugins--hook-rules (plugin)
  "Return normalized hook rules declared by PLUGIN."
  (niwa-plugins--hook-rules-from-hooks
   (niwa-plugin-hooks plugin)))

(defun niwa-plugins-hook-rule-events (plugin)
  "Return event names declared by PLUGIN hooks."
  (sort
   (delete-dups
    (delq nil
          (mapcar (lambda (rule)
                    (and (consp rule)
                         (symbol-name (car rule))))
                  (niwa-plugins--hook-rules plugin))))
   #'string<))

(defun niwa-plugins--hook-rule-handlers (plugin)
  "Return concise handler descriptions declared by PLUGIN hooks."
  (let (handlers)
    (dolist (rule (niwa-plugins--hook-rules plugin))
      (dolist (group (cdr-safe rule))
        (let ((scope (format "%s [%s]"
                             (car rule)
                             (or (plist-get group :matcher) "*"))))
          (dolist (handler (plist-get group :hooks))
            (pcase (plist-get handler :type)
              ('command
               (push (format "%s: command %s"
                             scope (plist-get handler :command))
                     handlers))
              ('elisp
               (push (format "%s: elisp %s"
                             scope (plist-get handler :function))
                     handlers)))))))
    (sort (delete-dups handlers) #'string<)))

(defun niwa-plugins-hook-consent-summary (plugin &optional workspace)
  "Return a concise hook consent summary for PLUGIN in WORKSPACE."
  (let ((events (niwa-plugins-hook-rule-events plugin))
        (handlers (niwa-plugins--hook-rule-handlers plugin))
        (skills (niwa-plugins-skill-count plugin)))
    (string-join
     (delq nil
           (list
            (format "Enable plugin %s hooks?" (niwa-plugin-name plugin))
            (format "Version: %s"
                    (or (niwa-plugin-version plugin) "unspecified"))
            (format "Source: %s" (niwa-plugin-root plugin))
            (when (> skills 0)
              (format "Skills: %d from %s"
                      skills
                      (niwa-plugin-skills-dir plugin)))
            (when events
              (format "Events: %s" (string-join events ", ")))
            (when handlers
              (format "Handlers: %s" (string-join handlers ", ")))
            (when workspace
              (format "Runtime data: %s"
                      (niwa-plugins-plugin-data-dir
                       (niwa-plugin-name plugin) workspace)))))
     "\n")))

(defun niwa-plugins--ensure-hook-consent (plugin &optional workspace)
  "Return non-nil when PLUGIN hooks may be enabled in WORKSPACE."
  (let* ((fingerprint (niwa-plugins--hook-fingerprint plugin workspace))
         (state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace)))
    (or (not fingerprint)
        (equal fingerprint (plist-get state :hooks-fingerprint))
        (yes-or-no-p
         (concat (niwa-plugins-hook-consent-summary plugin workspace)
                 " ")))))

(defun niwa-plugins--ensure-source-switch-consent
    (plugin &optional workspace)
  "Return non-nil when PLUGIN may replace an active source binding."
  (let* ((state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace))
         (source-root (plist-get state :source-root)))
    (or (not (plist-get state :enabled))
        (niwa-plugins-same-root-p
         (niwa-plugin-root plugin) source-root workspace)
        (yes-or-no-p
         (format "Switch plugin %s activation from %s to %s? "
                 (niwa-plugin-name plugin)
                 source-root
                 (niwa-plugin-root plugin))))))

(defun niwa-plugins--write-enabled-state
    (plugin hooks-enabled &optional workspace)
  "Persist PLUGIN as enabled, with HOOKS-ENABLED in WORKSPACE."
  (let ((fingerprint (niwa-plugins--hook-fingerprint plugin workspace)))
    (niwa-plugins--write-state-entry
     (niwa-plugin-name plugin)
     (list :enabled t
           :hooks-enabled (and hooks-enabled fingerprint t)
           :source-root (niwa-plugins-source-root
                         (niwa-plugin-root plugin)
                         workspace)
           :hooks-fingerprint (and hooks-enabled fingerprint))
     workspace)))

(defun niwa-plugins-enable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as enabled in WORKSPACE."
  (when-let* ((plugin (niwa-plugins-find plugin-name workspace)))
    (if (and (niwa-plugins--ensure-source-switch-consent
              plugin workspace)
             (niwa-plugins--ensure-hook-consent plugin workspace))
        (progn
          (niwa-plugins--write-enabled-state
           plugin
           (niwa-plugin-hooks plugin)
           workspace)
          t)
      nil)))

(defun niwa-plugins-disable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as disabled in WORKSPACE."
  (let ((plugin (niwa-plugins-find plugin-name workspace)))
    (niwa-plugins--write-state-entry
     plugin-name
     (list :enabled nil
           :hooks-enabled nil
           :source-root (and plugin
                             (niwa-plugins-source-root
                              (niwa-plugin-root plugin)
                              workspace))
           :hooks-fingerprint nil)
     workspace)))

(defun niwa-plugins-enable-hooks (plugin-name &optional workspace)
  "Persist hooks as enabled for PLUGIN-NAME in WORKSPACE."
  (when-let* ((plugin (niwa-plugins-find plugin-name workspace)))
    (and (niwa-plugin-hooks plugin)
         (niwa-plugins-enabled-p plugin workspace)
         (niwa-plugins--ensure-hook-consent plugin workspace)
         (progn
           (niwa-plugins--write-enabled-state plugin t workspace)
           t))))

(defun niwa-plugins-disable-hooks (plugin-name &optional workspace)
  "Persist hooks as disabled for PLUGIN-NAME in WORKSPACE."
  (let* ((plugin (niwa-plugins-find plugin-name workspace))
         (state (copy-sequence (or (niwa-plugins--state-plist
                                    plugin-name workspace)
                                   '(:enabled nil :hooks-enabled nil)))))
    (when plugin
      (setq state
            (plist-put state
                       :source-root
                       (niwa-plugins-source-root
                        (niwa-plugin-root plugin)
                        workspace))))
    (setq state (plist-put state :hooks-enabled nil))
    (niwa-plugins--write-state-entry plugin-name state workspace)))


;;
;;; Registry summaries

(defun niwa-plugins-active-shadowed-source
    (plugin &optional workspace)
  "Return shadowed active source for PLUGIN in WORKSPACE, or nil."
  (let ((state (niwa-plugins--state-plist
                (niwa-plugin-name plugin) workspace)))
    (and (plist-get state :enabled)
         (not (niwa-plugins-same-root-p
               (niwa-plugin-root plugin)
               (plist-get state :source-root)
               workspace))
         (cl-find-if
          (lambda (shadow)
            (niwa-plugins-same-root-p
             (niwa-plugin-root shadow)
             (plist-get state :source-root)
             workspace))
          (niwa-plugin-shadowed plugin)))))

(defun niwa-plugins-skill-count (plugin)
  "Return number of SKILL.md files exposed by PLUGIN."
  (if-let* ((dir (niwa-plugin-skills-dir plugin))
            ((file-directory-p dir)))
      (length (directory-files-recursively dir "\\`SKILL\\.md\\'"))
    0))

(defun niwa-plugins-hooks-stale-p (plugin &optional workspace)
  "Return non-nil when PLUGIN hook consent is stale in WORKSPACE."
  (let* ((state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace))
         (fingerprint (niwa-plugins--hook-fingerprint plugin workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (niwa-plugins-same-root-p
          (niwa-plugin-root plugin)
          (plist-get state :source-root)
          workspace)
         (not (equal fingerprint
                     (plist-get state :hooks-fingerprint))))))

(defun niwa-plugins-hooks-status (plugin &optional workspace)
  "Return user-facing hook status for PLUGIN in WORKSPACE."
  (cond
   ((not (niwa-plugin-hooks plugin)) "none")
   ((niwa-plugins-hooks-enabled-p plugin workspace) "on")
   ((niwa-plugins-hooks-stale-p plugin workspace) "needs-consent")
   (t "off")))

(defun niwa-plugins-pending-consent (&optional workspace)
  "Return enabled plugins whose executable hooks need consent in WORKSPACE."
  (cl-remove-if-not
   (lambda (plugin) (niwa-plugins-hooks-stale-p plugin workspace))
   (niwa-plugins-enabled workspace)))


;;
;;; Update-check state

;; The check itself runs in `niwa-plugin-lifecycle'; this section owns the
;; setting, the idle scheduling and the cached results.  The state is global
;; (managed plugins are global), so it lives beside the other user-level
;; files rather than in a workspace's `plugins.el'.

(declare-function niwa-plugins-check-updates
                  "niwa-plugin-lifecycle" (&optional on-demand done))
(autoload 'niwa-plugins-check-updates "niwa-plugin-lifecycle")

(defconst niwa-plugins--update-check-idle-delay 30
  "Seconds of idle time before a scheduled update check starts.")

(defvar niwa-plugins--update-cache :unloaded
  "Cached update-check state plist, or `:unloaded' before the first read.
Holds `:last-check' (`float-time') and `:plugins', an alist of source
root to a plist of `:local', `:remote', `:error' and `:notified'.")

(defvar niwa-plugins--update-check-timer nil
  "Pending idle timer for a scheduled update check, or nil.")

(defvar niwa-plugins--update-check-busy nil
  "Non-nil while an update check is running.")

(defun niwa-plugins--update-state-file ()
  "Return the global update-check state file."
  (file-name-concat niwa-user-dir "plugin-update-check.el"))

(defun niwa-plugins--update-state ()
  "Return the update-check state, reading it once per session."
  (when (eq niwa-plugins--update-cache :unloaded)
    (setq niwa-plugins--update-cache
          (let ((file (niwa-plugins--update-state-file)))
            (and (file-readable-p file)
                 (condition-case nil
                     (with-temp-buffer
                       (insert-file-contents file)
                       (let ((read-eval nil))
                         (let ((state (read (current-buffer))))
                           (and (listp state) state))))
                   (error nil))))))
  niwa-plugins--update-cache)

(defun niwa-plugins--update-save (state)
  "Cache and persist update-check STATE."
  (setq niwa-plugins--update-cache state)
  (niwa--write-file-atomically
   (niwa-plugins--update-state-file)
   (with-temp-buffer
     (insert ";; Niwa plugin update-check state\n")
     (insert ";; Auto-generated, safe to delete\n\n")
     (pp state (current-buffer))
     (buffer-string))))

(defun niwa-plugins--update-entry-for-root (root)
  "Return the cached update-check plist for source ROOT, or nil."
  (cdr (assoc (niwa-plugins-source-root root)
              (plist-get (niwa-plugins--update-state) :plugins))))

(defun niwa-plugins--update-set-entry (root plist)
  "Cache PLIST for source ROOT, keeping its `:notified' commit."
  (let* ((state (copy-tree (niwa-plugins--update-state)))
         (key (niwa-plugins-source-root root))
         (plugins (assoc-delete-all key (plist-get state :plugins))))
    (when-let* (((not (plist-member plist :notified)))
                (notified
                 (plist-get (niwa-plugins--update-entry-for-root root)
                            :notified)))
      (setq plist (plist-put (copy-sequence plist) :notified notified)))
    (niwa-plugins--update-save
     (plist-put state :plugins (cons (cons key plist) plugins)))))

(defun niwa-plugins--update-mark-notified (root)
  "Record that the cached upstream commit of ROOT was announced."
  (when-let* ((entry (niwa-plugins--update-entry-for-root root)))
    (niwa-plugins--update-set-entry
     root (plist-put (copy-sequence entry) :notified
                     (plist-get entry :remote)))))

(defun niwa-plugins-forget-update (root)
  "Drop the cached update-check result for ROOT after it was updated."
  (when (niwa-plugins--update-entry-for-root root)
    (let ((state (copy-tree (niwa-plugins--update-state))))
      (niwa-plugins--update-save
       (plist-put state :plugins
                  (assoc-delete-all (niwa-plugins-source-root root)
                                    (plist-get state :plugins)))))))

(defun niwa-plugins-update-available-p (plugin)
  "Return the upstream commit when the cache says PLUGIN has an update."
  (let ((entry (niwa-plugins--update-entry-for-root
                (niwa-plugin-root plugin))))
    (and entry
         (not (plist-get entry :error))
         (plist-get entry :remote)
         (not (equal (plist-get entry :remote) (plist-get entry :local)))
         (plist-get entry :remote))))

(defun niwa-plugins-update-check-error (plugin)
  "Return the cached update-check failure message for PLUGIN, or nil."
  (plist-get (niwa-plugins--update-entry-for-root
              (niwa-plugin-root plugin))
             :error))

(defun niwa-plugins--update-check-due-p ()
  "Return non-nil when the interval setting asks for a check now."
  (and (numberp niwa-plugins-update-check-interval)
       (> niwa-plugins-update-check-interval 0)
       (let ((last (plist-get (niwa-plugins--update-state) :last-check)))
         (or (not (numberp last))
             (>= (- (float-time) last)
                 (* niwa-plugins-update-check-interval 86400))))))

(defun niwa-plugins--run-scheduled-update-check ()
  "Start the check an idle timer was set for, if it is still due."
  (setq niwa-plugins--update-check-timer nil)
  (when (niwa-plugins--update-check-due-p)
    (niwa-plugins-check-updates)))

(defun niwa-plugins--schedule-update-check ()
  "Schedule an idle update check when the interval says one is due.
Does nothing with the default nil interval, while a check is pending or
running, and never touches the network itself."
  (when (and (not niwa-plugins--update-check-timer)
             (not niwa-plugins--update-check-busy)
             (niwa-plugins--update-check-due-p))
    (setq niwa-plugins--update-check-timer
          (run-with-idle-timer
           niwa-plugins--update-check-idle-delay nil
           #'niwa-plugins--run-scheduled-update-check))))



(provide 'niwa-plugin-registry)

;;; niwa-plugin-registry.el ends here
