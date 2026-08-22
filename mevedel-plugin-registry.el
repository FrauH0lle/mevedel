;;; mevedel-plugin-registry.el -- Plugin discovery and trust -*- lexical-binding: t -*-

;;; Commentary:

;; Discovers Codex-style manifests, validates plugin-relative paths, and owns
;; workspace-local enablement plus executable-hook consent state.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())

;; `mevedel-hooks'
(declare-function mevedel-hooks-invalidate-config "mevedel-hooks" ())
(declare-function mevedel-hooks-read-config-file
                  "mevedel-hooks" (file &optional content))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-publish-text
                  "mevedel-session-artifacts"
                  (session path content &optional coding))

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel-user-dir)

;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir
                  "mevedel-workspace" (workspace))

;; `subr'
(defvar read-eval)

(defcustom mevedel-plugin-extra-roots nil
  "Additional local Codex plugin roots to scan.
Each entry may be a direct plugin root or a directory containing plugin
roots below it."
  :type '(repeat directory)
  :group 'mevedel)

(defconst mevedel-plugins--default-install-directory
  (file-name-concat "~" ".agents" "plugins")
  "Default value for `mevedel-plugin-install-directory'.")

(defcustom mevedel-plugin-install-directory
  mevedel-plugins--default-install-directory
  "Directory where `/plugin install' writes new plugins."
  :type 'directory
  :group 'mevedel)


;;
;;; Structs

(cl-defstruct (mevedel-plugin (:constructor mevedel-plugin--create))
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

(cl-defstruct (mevedel-plugin-error (:constructor mevedel-plugin-error--create))
  "Plugin root whose manifest metadata could not be used."
  name
  root
  manifest
  message)

;;
;;; Paths

(defun mevedel-plugins--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory
   (expand-file-name (substitute-in-file-name directory))))

(defun mevedel-plugins-dir ()
  "Return the global directory where new plugins are installed."
  (mevedel-plugins--normalize-directory mevedel-plugin-install-directory))

(defun mevedel-plugins--global-mevedel-dir ()
  "Return the global mevedel-specific plugin directory."
  (require 'mevedel-structs)
  (mevedel-plugins--normalize-directory
   (file-name-concat mevedel-user-dir "plugins")))

(defun mevedel-plugins-state-file (&optional workspace)
  "Return the persistent plugin state file for WORKSPACE.
Return nil when WORKSPACE is nil."
  (require 'mevedel-structs)
  (require 'mevedel-workspace)
  (when workspace
    (file-name-concat (mevedel-workspace-state-dir workspace) "plugins.el")))

(defun mevedel-plugins--workspace-plugins-dir (workspace resource-dir)
  "Return WORKSPACE plugin directory under RESOURCE-DIR."
  (require 'mevedel-structs)
  (file-name-concat (mevedel-workspace-root workspace)
                    resource-dir
                    "plugins"))

(defun mevedel-plugins-plugin-data-dir (plugin-name &optional workspace)
  "Return persistent data directory for PLUGIN-NAME in WORKSPACE."
  (require 'mevedel-structs)
  (require 'mevedel-workspace)
  (unless workspace
    (error "No workspace for plugin data"))
  (file-name-concat (mevedel-workspace-state-dir workspace)
                    "plugin-data"
                    plugin-name))


;;
;;; Manifest discovery

(defun mevedel-plugins-manifest-file (root)
  "Return plugin manifest path under ROOT."
  (file-name-concat root ".codex-plugin" "plugin.json"))

(defun mevedel-plugins--string-or-nil (value)
  "Return VALUE if it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun mevedel-plugins--json-string (key alist)
  "Return string field KEY from parsed JSON ALIST."
  (mevedel-plugins--string-or-nil
   (or (alist-get key alist nil nil #'equal)
       (alist-get (intern key) alist))))

(defun mevedel-plugins--json-value (key alist missing)
  "Return field KEY from parsed JSON ALIST, or MISSING."
  (let ((value (alist-get key alist missing nil #'equal)))
    (if (eq value missing)
        (alist-get (intern key) alist missing)
      value)))

(defun mevedel-plugins--safe-name-p (name)
  "Return non-nil when NAME is safe for plugin identifiers."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9_.-]+\\'" name)
       (not (member name '("." ".." "user" "project" "managed" "bundled")))))

(defun mevedel-plugins--path-in-root-p (path root)
  "Return non-nil when PATH stays inside ROOT."
  (let ((path (file-truename path))
        (root (file-name-as-directory (file-truename root))))
    (or (equal (file-name-as-directory path) root)
        (string-prefix-p root (file-name-as-directory path)))))

(defun mevedel-plugins--resolve-manifest-path (root path)
  "Resolve manifest PATH relative to plugin ROOT."
  (when-let* ((path (mevedel-plugins--string-or-nil path))
              ((not (file-name-absolute-p path)))
              (expanded (expand-file-name path root))
              ((not (file-symlink-p expanded)))
              ((mevedel-plugins--path-in-root-p expanded root)))
    expanded))

(defun mevedel-plugins--normalize-manifest-hooks (root value missing)
  "Normalize manifest hook VALUE for plugin ROOT.
Return a list of plists, each with a `:file' path."
  (cond
   ((eq value missing)
    (when-let* ((default (mevedel-plugins--resolve-manifest-path
                          root "hooks/hooks.json"))
                ((file-readable-p default)))
      (list (list :file default))))
   ((stringp value)
    (when-let* ((file (mevedel-plugins--resolve-manifest-path root value)))
      (list (list :file file))))))

(defun mevedel-plugins--first-hook-file (hooks)
  "Return the first file path in normalized HOOKS."
  (catch 'file
    (dolist (entry hooks)
      (when-let* ((file (plist-get entry :file)))
        (throw 'file file)))))

(defun mevedel-plugins--read-manifest-result (root &optional workspace)
  "Read plugin manifest under ROOT.
Return a `mevedel-plugin' or `mevedel-plugin-error', or nil when ROOT
does not contain a readable Codex plugin manifest."
  (require 'cl-lib)
  (require 'json)
  (require 'mevedel-structs)
  (let ((file (mevedel-plugins-manifest-file root)))
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
                   (name (or (mevedel-plugins--json-string "name" json)
                             fallback-name))
                   (missing (make-symbol "missing"))
                   (skills (mevedel-plugins--json-string "skills" json))
                   (hooks (mevedel-plugins--normalize-manifest-hooks
                           root
                           (mevedel-plugins--json-value
                            "hooks" json missing)
                           missing)))
              (if (mevedel-plugins--safe-name-p name)
                  (mevedel-plugin--create
                   :name name
                   :version (mevedel-plugins--json-string "version" json)
                   :description (mevedel-plugins--json-string
                                 "description" json)
                   :root root
                   :skills-dir (mevedel-plugins--resolve-manifest-path
                                root skills)
                   :hooks-file (mevedel-plugins--first-hook-file hooks)
                   :hooks hooks
                   :enabled-p (mevedel-plugins--state-enabled-p
                               name workspace root)
                   :hooks-enabled-p (mevedel-plugins--state-hooks-enabled-p
                                     name workspace root hooks))
                (mevedel-plugin-error--create
                 :name name
                 :root root
                 :manifest file
                 :message (format "Unsafe plugin name: %s" name))))
          (error
           (mevedel-plugin-error--create
            :name fallback-name
            :root root
            :manifest file
            :message (error-message-string err))))))))

(defun mevedel-plugins-read-manifest (root &optional workspace)
  "Read plugin manifest under ROOT.
Return nil when ROOT does not contain a usable Codex plugin manifest."
  (let ((entry (mevedel-plugins--read-manifest-result root workspace)))
    (and (mevedel-plugin-p entry) entry)))

(defun mevedel-plugins--canonical-root (root)
  "Return deterministic directory form for plugin ROOT."
  (file-name-as-directory (expand-file-name root)))

(defconst mevedel-plugins-staging-prefix ".mevedel-staging-"
  "Filename prefix marking an in-flight plugin clone.
A clone is staged beside its destination so publishing it is a
same-filesystem rename, which puts it inside the tree discovery walks.
Discovery skips these names so a half-built clone is never adopted.")

(defun mevedel-plugins-staging-name-p (path)
  "Return non-nil when PATH names an in-flight plugin clone."
  (string-prefix-p mevedel-plugins-staging-prefix
                   (file-name-nondirectory (directory-file-name path))))

(defun mevedel-plugins--collect-roots-under (root)
  "Return plugin roots at or below ROOT.
Do not descend into a directory once it is recognized as a plugin root."
  (let (roots)
    (cl-labels
        ((walk (dir)
           (when (file-directory-p dir)
             (let ((dir (mevedel-plugins--canonical-root dir)))
               (cond
                ((file-readable-p (mevedel-plugins-manifest-file dir))
                 (push dir roots))
                (t
                 (dolist (entry (directory-files
                                  dir t directory-files-no-dot-files-regexp))
                   (when (and (file-directory-p entry)
                              (not (file-symlink-p entry))
                              (not (mevedel-plugins-staging-name-p entry)))
                     (walk entry)))))))))
      (walk root))
    (nreverse roots)))

(defun mevedel-plugins--root-sources (&optional workspace)
  "Return plugin root containers in precedence order for WORKSPACE."
  (append
   (when workspace
     (list (mevedel-plugins--workspace-plugins-dir workspace ".mevedel")
           (mevedel-plugins--workspace-plugins-dir workspace ".agents")))
   (list (mevedel-plugins--global-mevedel-dir)
         (mevedel-plugins-dir))
   mevedel-plugin-extra-roots))

(defun mevedel-plugins--plugin-roots (&optional workspace)
  "Return deduplicated plugin roots in precedence order for WORKSPACE."
  (let ((seen (make-hash-table :test #'equal))
        roots)
    (dolist (root (mevedel-plugins--root-sources workspace))
      (dolist (plugin-root (mevedel-plugins--collect-roots-under root))
        (let ((key (file-name-as-directory (file-truename plugin-root))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push plugin-root roots)))))
    (nreverse roots)))

(defun mevedel-plugins--select-duplicate-names (plugins)
  "Select highest-precedence entries from PLUGINS.
Shadowed entries are retained in the winner's `shadowed' slot."
  (let ((by-name (make-hash-table :test #'equal))
        kept)
    (dolist (plugin plugins)
      (push plugin (gethash (mevedel-plugin-name plugin) by-name)))
    (maphash
     (lambda (_name matches)
       (let* ((matches (nreverse matches))
              (winner (car matches)))
         (setf (mevedel-plugin-shadowed winner) (cdr matches))
         (push winner kept)))
     by-name)
    (sort kept
          (lambda (a b)
            (string< (mevedel-plugin-name a)
                     (mevedel-plugin-name b))))))

(defun mevedel-plugins--collect (&optional workspace)
  "Return discovered plugin metadata for WORKSPACE."
  (let (plugins errors)
    (dolist (root (mevedel-plugins--plugin-roots workspace))
      (let ((item (mevedel-plugins--read-manifest-result root workspace)))
        (cond
         ((mevedel-plugin-p item) (push item plugins))
         ((mevedel-plugin-error-p item) (push item errors)))))
    (let* ((winners (mevedel-plugins--select-duplicate-names
                     (nreverse plugins)))
           (errors (nreverse errors))
           (enabled-count
            (cl-count-if
             (lambda (plugin)
               (mevedel-plugins-enabled-p plugin workspace))
             winners)))
      (list :winners winners
            :errors errors
            :enabled-count enabled-count
            :total-count (length winners)
            :error-count (length errors)))))

(defun mevedel-plugins-list (&optional workspace)
  "Return installed Codex-style plugins.
State slots are resolved for WORKSPACE when provided."
  (plist-get (mevedel-plugins--collect workspace) :winners))

(defun mevedel-plugins-item-name (item)
  "Return display name for plugin cockpit ITEM."
  (cond
   ((mevedel-plugin-p item) (mevedel-plugin-name item))
   ((mevedel-plugin-error-p item)
    (or (mevedel-plugin-error-name item) "metadata error"))))

(defun mevedel-plugins-items (&optional workspace)
  "Return plugin cockpit items for WORKSPACE.
Items include usable plugin manifests and visible metadata errors."
  (let ((collection (mevedel-plugins--collect workspace)))
    (sort (append (plist-get collection :winners)
                  (plist-get collection :errors))
          (lambda (a b)
            (string< (mevedel-plugins-item-name a)
                     (mevedel-plugins-item-name b))))))

(defun mevedel-plugins-find (name &optional workspace)
  "Return installed plugin named NAME, or nil."
  (catch 'found
    (dolist (plugin (mevedel-plugins-list workspace) nil)
      (when (equal name (mevedel-plugin-name plugin))
        (throw 'found plugin)))))

(defun mevedel-plugins-plugin-root (plugin-name)
  "Return root directory for installed PLUGIN-NAME, or nil."
  (when-let* ((plugin (mevedel-plugins-find plugin-name)))
    (mevedel-plugin-root plugin)))


;;
;;; Persistent state

(defun mevedel-plugins--read-state (&optional workspace)
  "Read persistent plugin state for WORKSPACE."
  (when-let* ((file (mevedel-plugins-state-file workspace))
              ((file-readable-p file)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((read-eval nil))
            (let ((state (read (current-buffer))))
              (and (listp state) state))))
      (error nil))))

(defun mevedel-plugins--state-session (workspace)
  "Return the live session authorized to mutate WORKSPACE plugin state."
  (require 'mevedel-cockpit)
  (require 'mevedel-structs)
  (let ((session
         (or (and (boundp 'mevedel--session) mevedel--session)
             (ignore-errors
               (mevedel-cockpit-context-session
                (mevedel-cockpit-current-context))))))
    (and session
         (eq workspace (mevedel-session-workspace session))
         session)))

(defun mevedel-plugins--state-text (state)
  "Return the durable plugin STATE file contents."
  (with-temp-buffer
    (insert ";; Mevedel plugin state\n")
    (insert ";; Auto-generated, safe to edit\n\n")
    (pp state (current-buffer))
    (buffer-string)))

(defun mevedel-plugins--write-state (state &optional workspace)
  "Persist plugin STATE for WORKSPACE."
  (require 'mevedel-session-artifacts)
  (let* ((file (or (mevedel-plugins-state-file workspace)
                   (error "No workspace for plugin state")))
         (content (mevedel-plugins--state-text state)))
    (if (file-remote-p file)
        (let ((session (or (mevedel-plugins--state-session workspace)
                           (user-error
                            "Remote plugin state requires its live session"))))
          (mevedel-session-artifacts-publish-text
           session file content 'utf-8-unix))
      (make-directory (file-name-directory file) t)
      ;; Replace the file through a same-directory rename.  It is the whole
      ;; record of every plugin's activation and hook consent, and
      ;; `mevedel-plugins--read-state' reads a truncated file as nil, so a
      ;; write that died in place would silently disable everything.
      (let ((tmp (make-temp-file
                  (expand-file-name ".mevedel-plugins-"
                                    (file-name-directory file)))))
        (unwind-protect
            (progn
              ;; `make-temp-file' creates 0600, which would become the state
              ;; file's mode; keep what an ordinary write would have produced.
              (set-file-modes tmp (default-file-modes))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region content nil tmp nil 'silent))
              (rename-file tmp file t))
          (when (file-exists-p tmp)
            (delete-file tmp)))))))

(defun mevedel-plugins--state-plist (name &optional workspace)
  "Return persisted state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (mevedel-plugins--read-state workspace))))

(defun mevedel-plugins-source-root (root &optional workspace)
  "Return the durable source identity for ROOT in WORKSPACE.
Project sources use workspace-relative paths; other sources stay absolute."
  (require 'mevedel-structs)
  (when root
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (workspace-root
            (and workspace
                 (file-name-as-directory
                  (expand-file-name (mevedel-workspace-root workspace)))))
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

(defun mevedel-plugins-managed-roots ()
  "Return directories whose plugins mevedel may update or remove."
  (delete-dups
   (mapcar #'mevedel-plugins-source-root
           (list (mevedel-plugins--global-mevedel-dir)
                 (mevedel-plugins-dir)))))

(defun mevedel-plugins-same-root-p (a b &optional workspace)
  "Return non-nil when source roots A and B identify the same directory.
Relative identities are requalified through the live WORKSPACE root."
  (require 'mevedel-structs)
  (when (and a b)
    (let ((base (and workspace (mevedel-workspace-root workspace))))
      (equal (file-name-as-directory (expand-file-name a base))
             (file-name-as-directory (expand-file-name b base))))))

(defun mevedel-plugins--hook-rules-from-file (file)
  "Return normalized hook rules from FILE."
  (when (and (file-readable-p file)
             (require 'mevedel-hooks nil t))
    (mevedel-hooks-read-config-file file)))

(defun mevedel-plugins--hook-rules-from-hooks (hooks)
  "Return normalized hook rules declared by HOOKS."
  (let (rules)
    (dolist (entry hooks rules)
      (when-let* ((file (plist-get entry :file)))
        (setq rules
              (append rules
                      (mevedel-plugins--hook-rules-from-file file)))))))

(defun mevedel-plugins--hook-surface-from-hooks (hooks &optional workspace)
  "Return the hook consent surface declared by HOOKS in WORKSPACE."
  (let (items)
    (dolist (entry hooks)
      (when-let* ((file (plist-get entry :file)))
        (let ((source (mevedel-plugins-source-root file workspace)))
          (push (list source) items)
          (dolist (rule (mevedel-plugins--hook-rules-from-file file))
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

(defun mevedel-plugins--hook-fingerprint-from-hooks
    (hooks &optional workspace)
  "Return a consent fingerprint for normalized plugin HOOKS in WORKSPACE."
  (when-let* ((surface (mevedel-plugins--hook-surface-from-hooks
                        hooks workspace)))
    (secure-hash 'sha256 (prin1-to-string surface))))

(defun mevedel-plugins--hook-fingerprint (plugin &optional workspace)
  "Return the current hook consent fingerprint for PLUGIN in WORKSPACE."
  (mevedel-plugins--hook-fingerprint-from-hooks
   (mevedel-plugin-hooks plugin)
   workspace))

(defun mevedel-plugins--state-enabled-p (name &optional workspace root)
  "Return non-nil when plugin NAME from ROOT is enabled in WORKSPACE."
  (let ((state (mevedel-plugins--state-plist name workspace)))
    (and (plist-get state :enabled)
         (mevedel-plugins-same-root-p
          root
          (plist-get state :source-root)
          workspace))))

(defun mevedel-plugins--state-hooks-enabled-p
    (name &optional workspace root hooks)
  "Return non-nil when plugin NAME from ROOT has hooks enabled in WORKSPACE."
  (let* ((state (mevedel-plugins--state-plist name workspace))
         (fingerprint (mevedel-plugins--hook-fingerprint-from-hooks
                       hooks workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (mevedel-plugins--state-enabled-p name workspace root)
         (equal fingerprint (plist-get state :hooks-fingerprint)))))

(defun mevedel-plugins-enabled-p (plugin &optional workspace)
  "Return non-nil when PLUGIN is enabled in WORKSPACE."
  (mevedel-plugins--state-enabled-p
   (mevedel-plugin-name plugin)
   workspace
   (mevedel-plugin-root plugin)))

(defun mevedel-plugins-hooks-enabled-p (plugin &optional workspace)
  "Return non-nil when hooks are enabled for PLUGIN in WORKSPACE."
  (mevedel-plugins--state-hooks-enabled-p
   (mevedel-plugin-name plugin)
   workspace
   (mevedel-plugin-root plugin)
   (mevedel-plugin-hooks plugin)))

(defun mevedel-plugins--write-state-entry (name plist &optional workspace)
  "Persist plugin NAME state PLIST for WORKSPACE."
  (let* ((state (copy-tree (mevedel-plugins--read-state workspace)))
         (entry (assoc name state)))
    (if entry
        (setcdr entry plist)
      (push (cons name plist) state))
    (mevedel-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b))))
     workspace)
    ;; Enablement decides which plugin hooks resolve, so the memoized
    ;; hook configuration is stale from here.  Guarded rather than
    ;; required: hooks depends on plugins, not the other way around.
    (when (fboundp 'mevedel-hooks-invalidate-config)
      (mevedel-hooks-invalidate-config))))

(defun mevedel-plugins-remove-state-for-source
    (name root &optional workspace)
  "Remove NAME state only when it points at source ROOT."
  (require 'cl-lib)
  (when-let* ((state (mevedel-plugins--read-state workspace))
              (entry (assoc name state))
              ((mevedel-plugins-same-root-p
                root
                (plist-get (cdr entry) :source-root)
                workspace)))
    (mevedel-plugins--write-state
     (cl-remove name (copy-tree state)
                :key #'car
                :test #'equal)
     workspace)))

(defun mevedel-plugins-transfer-state-entry
    (old-name new-name plist &optional workspace)
  "Move plugin state PLIST from OLD-NAME to NEW-NAME in WORKSPACE."
  (let (state)
    (dolist (entry (copy-tree (mevedel-plugins--read-state workspace)))
      (unless (member (car entry) (list old-name new-name))
        (push entry state)))
    (push (cons new-name plist) state)
    (mevedel-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b))))
     workspace)))

(defun mevedel-plugins-enabled (&optional workspace)
  "Return installed plugins enabled in WORKSPACE."
  (let (enabled)
    (dolist (plugin (plist-get (mevedel-plugins--collect workspace) :winners)
                    (nreverse enabled))
      (when (mevedel-plugins-enabled-p plugin workspace)
        (push plugin enabled)))))

(defun mevedel-plugins-count-label (&optional workspace)
  "Return enabled/total plugin count label for WORKSPACE."
  (if workspace
      (let ((collection (mevedel-plugins--collect workspace)))
        (format "%d/%d"
                (plist-get collection :enabled-count)
                (plist-get collection :total-count)))
    "0/0"))

(defun mevedel-plugins-state-entry (plugin &optional workspace)
  "Return persisted state plist for PLUGIN in WORKSPACE, with defaults applied."
  (let* ((state (copy-sequence
                 (or (mevedel-plugins--state-plist
                      (mevedel-plugin-name plugin) workspace)
                     '(:enabled nil :hooks-enabled nil)))))
    (setq state
          (plist-put state
                     :source-root
                     (mevedel-plugins-source-root
                      (mevedel-plugin-root plugin)
                      workspace)))
    state))

(defun mevedel-plugins--hook-rules (plugin)
  "Return normalized hook rules declared by PLUGIN."
  (mevedel-plugins--hook-rules-from-hooks
   (mevedel-plugin-hooks plugin)))

(defun mevedel-plugins-hook-rule-events (plugin)
  "Return event names declared by PLUGIN hooks."
  (sort
   (delete-dups
    (delq nil
          (mapcar (lambda (rule)
                    (and (consp rule)
                         (symbol-name (car rule))))
                  (mevedel-plugins--hook-rules plugin))))
   #'string<))

(defun mevedel-plugins--hook-rule-handlers (plugin)
  "Return concise handler descriptions declared by PLUGIN hooks."
  (let (handlers)
    (dolist (rule (mevedel-plugins--hook-rules plugin))
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

(defun mevedel-plugins-hook-consent-summary (plugin &optional workspace)
  "Return a concise hook consent summary for PLUGIN in WORKSPACE."
  (let ((events (mevedel-plugins-hook-rule-events plugin))
        (handlers (mevedel-plugins--hook-rule-handlers plugin))
        (skills (mevedel-plugins-skill-count plugin)))
    (string-join
     (delq nil
           (list
            (format "Enable plugin %s hooks?" (mevedel-plugin-name plugin))
            (format "Version: %s"
                    (or (mevedel-plugin-version plugin) "unspecified"))
            (format "Source: %s" (mevedel-plugin-root plugin))
            (when (> skills 0)
              (format "Skills: %d from %s"
                      skills
                      (mevedel-plugin-skills-dir plugin)))
            (when events
              (format "Events: %s" (string-join events ", ")))
            (when handlers
              (format "Handlers: %s" (string-join handlers ", ")))
            (when workspace
              (format "Runtime data: %s"
                      (mevedel-plugins-plugin-data-dir
                       (mevedel-plugin-name plugin) workspace)))))
     "\n")))

(defun mevedel-plugins--ensure-hook-consent (plugin &optional workspace)
  "Return non-nil when PLUGIN hooks may be enabled in WORKSPACE."
  (let* ((fingerprint (mevedel-plugins--hook-fingerprint plugin workspace))
         (state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace)))
    (or (not fingerprint)
        (equal fingerprint (plist-get state :hooks-fingerprint))
        (yes-or-no-p
         (concat (mevedel-plugins-hook-consent-summary plugin workspace)
                 " ")))))

(defun mevedel-plugins--ensure-source-switch-consent
    (plugin &optional workspace)
  "Return non-nil when PLUGIN may replace an active source binding."
  (let* ((state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace))
         (source-root (plist-get state :source-root)))
    (or (not (plist-get state :enabled))
        (mevedel-plugins-same-root-p
         (mevedel-plugin-root plugin) source-root workspace)
        (yes-or-no-p
         (format "Switch plugin %s activation from %s to %s? "
                 (mevedel-plugin-name plugin)
                 source-root
                 (mevedel-plugin-root plugin))))))

(defun mevedel-plugins--write-enabled-state
    (plugin hooks-enabled &optional workspace)
  "Persist PLUGIN as enabled, with HOOKS-ENABLED in WORKSPACE."
  (let ((fingerprint (mevedel-plugins--hook-fingerprint plugin workspace)))
    (mevedel-plugins--write-state-entry
     (mevedel-plugin-name plugin)
     (list :enabled t
           :hooks-enabled (and hooks-enabled fingerprint t)
           :source-root (mevedel-plugins-source-root
                         (mevedel-plugin-root plugin)
                         workspace)
           :hooks-fingerprint (and hooks-enabled fingerprint))
     workspace)))

(defun mevedel-plugins-enable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as enabled in WORKSPACE."
  (when-let* ((plugin (mevedel-plugins-find plugin-name workspace)))
    (if (and (mevedel-plugins--ensure-source-switch-consent
              plugin workspace)
             (mevedel-plugins--ensure-hook-consent plugin workspace))
        (progn
          (mevedel-plugins--write-enabled-state
           plugin
           (mevedel-plugin-hooks plugin)
           workspace)
          t)
      nil)))

(defun mevedel-plugins-disable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as disabled in WORKSPACE."
  (let ((plugin (mevedel-plugins-find plugin-name workspace)))
    (mevedel-plugins--write-state-entry
     plugin-name
     (list :enabled nil
           :hooks-enabled nil
           :source-root (and plugin
                             (mevedel-plugins-source-root
                              (mevedel-plugin-root plugin)
                              workspace))
           :hooks-fingerprint nil)
     workspace)))

(defun mevedel-plugins-enable-hooks (plugin-name &optional workspace)
  "Persist hooks as enabled for PLUGIN-NAME in WORKSPACE."
  (when-let* ((plugin (mevedel-plugins-find plugin-name workspace)))
    (and (mevedel-plugin-hooks plugin)
         (mevedel-plugins-enabled-p plugin workspace)
         (mevedel-plugins--ensure-hook-consent plugin workspace)
         (progn
           (mevedel-plugins--write-enabled-state plugin t workspace)
           t))))

(defun mevedel-plugins-disable-hooks (plugin-name &optional workspace)
  "Persist hooks as disabled for PLUGIN-NAME in WORKSPACE."
  (let* ((plugin (mevedel-plugins-find plugin-name workspace))
         (state (copy-sequence (or (mevedel-plugins--state-plist
                                    plugin-name workspace)
                                   '(:enabled nil :hooks-enabled nil)))))
    (when plugin
      (setq state
            (plist-put state
                       :source-root
                       (mevedel-plugins-source-root
                        (mevedel-plugin-root plugin)
                        workspace))))
    (setq state (plist-put state :hooks-enabled nil))
    (mevedel-plugins--write-state-entry plugin-name state workspace)))


;;
;;; Registry summaries

(defun mevedel-plugins-active-shadowed-source
    (plugin &optional workspace)
  "Return shadowed active source for PLUGIN in WORKSPACE, or nil."
  (require 'cl-lib)
  (let ((state (mevedel-plugins--state-plist
                (mevedel-plugin-name plugin) workspace)))
    (and (plist-get state :enabled)
         (not (mevedel-plugins-same-root-p
               (mevedel-plugin-root plugin)
               (plist-get state :source-root)
               workspace))
         (cl-find-if
          (lambda (shadow)
            (mevedel-plugins-same-root-p
             (mevedel-plugin-root shadow)
             (plist-get state :source-root)
             workspace))
          (mevedel-plugin-shadowed plugin)))))

(defun mevedel-plugins-skill-count (plugin)
  "Return number of SKILL.md files exposed by PLUGIN."
  (if-let* ((dir (mevedel-plugin-skills-dir plugin))
            ((file-directory-p dir)))
      (length (directory-files-recursively dir "\\`SKILL\\.md\\'"))
    0))

(defun mevedel-plugins-hooks-stale-p (plugin &optional workspace)
  "Return non-nil when PLUGIN hook consent is stale in WORKSPACE."
  (let* ((state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace))
         (fingerprint (mevedel-plugins--hook-fingerprint plugin workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (mevedel-plugins-same-root-p
          (mevedel-plugin-root plugin)
          (plist-get state :source-root)
          workspace)
         (not (equal fingerprint
                     (plist-get state :hooks-fingerprint))))))

(defun mevedel-plugins-hooks-status (plugin &optional workspace)
  "Return user-facing hook status for PLUGIN in WORKSPACE."
  (cond
   ((not (mevedel-plugin-hooks plugin)) "none")
   ((mevedel-plugins-hooks-enabled-p plugin workspace) "on")
   ((mevedel-plugins-hooks-stale-p plugin workspace) "needs-consent")
   (t "off")))

(defun mevedel-plugins-pending-consent (&optional workspace)
  "Return enabled plugins whose executable hooks need consent in WORKSPACE."
  (let (pending)
    (dolist (plugin (mevedel-plugins-enabled workspace) (nreverse pending))
      (when (mevedel-plugins-hooks-stale-p plugin workspace)
        (push plugin pending)))))



(provide 'mevedel-plugin-registry)

;;; mevedel-plugin-registry.el ends here
