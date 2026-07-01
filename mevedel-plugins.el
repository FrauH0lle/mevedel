;;; mevedel-plugins.el -- Codex-style plugin discovery -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal Codex-style plugin support.  Plugins are installed under
;; `~/.agents/plugins' by default and each plugin root contains
;; `.codex-plugin/plugin.json'.  This module discovers manifests, persists
;; workspace-local enabled/hook state, and implements the local `/plugin'
;; command body.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)
(require 'subr-x)
(require 'mevedel-structs)

;; `mevedel-hooks'
(declare-function mevedel-hooks--read-config-file "mevedel-hooks" (file))

;; `mevedel-skills'
(declare-function mevedel-skills-rescan "mevedel-skills" ())

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--workspace)

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

;;
;;; Paths

(defun mevedel-plugins--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory
   (expand-file-name (substitute-in-file-name directory))))

(defun mevedel-plugins-dir ()
  "Return the global directory where new plugins are installed."
  (mevedel-plugins--normalize-directory mevedel-plugin-install-directory))

(defun mevedel-plugins--legacy-dir ()
  "Return the legacy global mevedel plugin directory."
  (mevedel-plugins--normalize-directory
   (file-name-concat mevedel-user-dir "plugins")))

(defun mevedel-plugins--current-workspace ()
  "Return the current chat workspace, if available."
  (or (and (boundp 'mevedel--session)
           mevedel--session
           (mevedel-session-workspace mevedel--session))
      (and (boundp 'mevedel--workspace)
           mevedel--workspace)))

(defun mevedel-plugins-state-file (&optional workspace)
  "Return the persistent plugin state file for WORKSPACE.
Return nil when WORKSPACE is nil."
  (when workspace
    (file-name-concat (mevedel-workspace-state-dir workspace) "plugins.el")))

(defun mevedel-plugins--workspace-plugins-dir (workspace resource-dir)
  "Return WORKSPACE plugin directory under RESOURCE-DIR."
  (file-name-concat (mevedel-workspace-root workspace)
                    resource-dir
                    "plugins"))

(defun mevedel-plugins-plugin-data-dir (plugin-name &optional workspace)
  "Return persistent data directory for PLUGIN-NAME in WORKSPACE."
  (unless workspace
    (error "No workspace for plugin data"))
  (file-name-concat (mevedel-workspace-state-dir workspace)
                    "plugin-data"
                    plugin-name))


;;
;;; Manifest discovery

(defun mevedel-plugins--manifest-file (root)
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

(defun mevedel-plugins--read-manifest (root &optional workspace)
  "Read plugin manifest under ROOT.
Return nil when ROOT does not contain a readable Codex plugin manifest."
  (let ((file (mevedel-plugins--manifest-file root)))
    (when (file-readable-p file)
      (condition-case nil
          (let* ((root (file-name-as-directory (expand-file-name root)))
                 (json (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer
                          :object-type 'alist
                          :array-type 'list
                          :null-object nil
                          :false-object nil)))
                 (fallback-name
                  (file-name-nondirectory (directory-file-name root)))
                 (name (or (mevedel-plugins--json-string "name" json)
                           fallback-name))
                 (missing (make-symbol "missing"))
                 (skills (mevedel-plugins--json-string "skills" json))
                 (hooks (mevedel-plugins--normalize-manifest-hooks
                         root
                         (mevedel-plugins--json-value
                          "hooks" json missing)
                         missing)))
            (when (mevedel-plugins--safe-name-p name)
              (mevedel-plugin--create
               :name name
               :version (mevedel-plugins--json-string "version" json)
               :description (mevedel-plugins--json-string "description" json)
               :root root
               :skills-dir (mevedel-plugins--resolve-manifest-path root skills)
               :hooks-file (mevedel-plugins--first-hook-file hooks)
               :hooks hooks
               :enabled-p (mevedel-plugins--state-enabled-p
                            name workspace root)
               :hooks-enabled-p (mevedel-plugins--state-hooks-enabled-p
                                  name workspace root hooks))))
        (error nil)))))

(defun mevedel-plugins--canonical-root (root)
  "Return deterministic directory form for plugin ROOT."
  (file-name-as-directory (expand-file-name root)))

(defun mevedel-plugins--collect-roots-under (root)
  "Return plugin roots at or below ROOT.
Do not descend into a directory once it is recognized as a plugin root."
  (let (roots)
    (cl-labels
        ((walk (dir)
           (when (file-directory-p dir)
             (let ((dir (mevedel-plugins--canonical-root dir)))
               (cond
                ((file-readable-p (mevedel-plugins--manifest-file dir))
                 (push dir roots))
                (t
                 (dolist (entry (directory-files
                                  dir t directory-files-no-dot-files-regexp))
                   (when (and (file-directory-p entry)
                              (not (file-symlink-p entry)))
                     (walk entry)))))))))
      (walk root))
    (nreverse roots)))

(defun mevedel-plugins--root-sources (&optional workspace)
  "Return plugin root containers in precedence order for WORKSPACE."
  (append
   (when workspace
     (list (mevedel-plugins--workspace-plugins-dir workspace ".mevedel")
           (mevedel-plugins--workspace-plugins-dir workspace ".agents")))
   (list (mevedel-plugins--legacy-dir)
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

(defun mevedel-plugins-list (&optional workspace)
  "Return installed Codex-style plugins.
State slots are resolved for WORKSPACE when provided."
  (let (plugins)
    (dolist (root (mevedel-plugins--plugin-roots workspace))
      (when-let* ((plugin (mevedel-plugins--read-manifest
                           root workspace)))
        (push plugin plugins)))
    (mevedel-plugins--select-duplicate-names (nreverse plugins))))

(defun mevedel-plugins--find (name &optional workspace)
  "Return installed plugin named NAME, or nil."
  (catch 'found
    (dolist (plugin (mevedel-plugins-list workspace) nil)
      (when (equal name (mevedel-plugin-name plugin))
        (throw 'found plugin)))))

(defun mevedel-plugins-plugin-root (plugin-name)
  "Return root directory for installed PLUGIN-NAME, or nil."
  (when-let* ((plugin (mevedel-plugins--find plugin-name)))
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

(defun mevedel-plugins--write-state (state &optional workspace)
  "Persist plugin STATE for WORKSPACE."
  (let ((file (or (mevedel-plugins-state-file workspace)
                  (error "No workspace for plugin state"))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";; Mevedel plugin state\n")
      (insert ";; Auto-generated, safe to edit\n\n")
      (pp state (current-buffer)))))

(defun mevedel-plugins--state-plist (name &optional workspace)
  "Return persisted state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (mevedel-plugins--read-state workspace))))

(defun mevedel-plugins--source-root (root)
  "Return persisted source-root form for ROOT."
  (and root
       (file-name-as-directory (expand-file-name root))))

(defun mevedel-plugins--same-root-p (a b)
  "Return non-nil when source roots A and B identify the same directory."
  (and a b
       (equal (mevedel-plugins--source-root a)
              (mevedel-plugins--source-root b))))

(defun mevedel-plugins--hook-rules-from-file (file)
  "Return normalized hook rules from FILE."
  (when (and (file-readable-p file)
             (require 'mevedel-hooks nil t))
    (mevedel-hooks--read-config-file file)))

(defun mevedel-plugins--hook-rules-from-hooks (hooks)
  "Return normalized hook rules declared by HOOKS."
  (let (rules)
    (dolist (entry hooks rules)
      (when-let* ((file (plist-get entry :file)))
        (setq rules
              (append rules
                      (mevedel-plugins--hook-rules-from-file file)))))))

(defun mevedel-plugins--hook-surface-from-hooks (hooks)
  "Return the hook consent surface declared by HOOKS."
  (let (items)
    (dolist (entry hooks)
      (when-let* ((file (plist-get entry :file)))
        (let ((source (mevedel-plugins--source-root file)))
          (push (list source) items)
          (dolist (rule (mevedel-plugins--hook-rules-from-file file))
            (let ((event (symbol-name (car rule))))
              (push (list source event) items)
              (dolist (group (cdr-safe rule))
                (dolist (handler (plist-get group :hooks))
                  (pcase (plist-get handler :type)
                    ('command
                     (push (list source event 'command
                                 (plist-get handler :command))
                           items))
                    ('elisp
                     (push (list source event 'elisp
                                 (plist-get handler :function))
                           items))))))))))
    (sort (delete-dups items)
          (lambda (a b)
            (string< (prin1-to-string a)
                     (prin1-to-string b))))))

(defun mevedel-plugins--hook-fingerprint-from-hooks (hooks)
  "Return a consent fingerprint for normalized plugin HOOKS."
  (when-let* ((surface (mevedel-plugins--hook-surface-from-hooks hooks)))
    (secure-hash 'sha256 (prin1-to-string surface))))

(defun mevedel-plugins--hook-fingerprint (plugin)
  "Return the current hook consent fingerprint for PLUGIN."
  (mevedel-plugins--hook-fingerprint-from-hooks
   (mevedel-plugin-hooks plugin)))

(defun mevedel-plugins--state-enabled-p (name &optional workspace root)
  "Return non-nil when plugin NAME from ROOT is enabled in WORKSPACE."
  (let ((state (mevedel-plugins--state-plist name workspace)))
    (and (plist-get state :enabled)
         (mevedel-plugins--same-root-p
          root
          (plist-get state :source-root)))))

(defun mevedel-plugins--state-hooks-enabled-p
    (name &optional workspace root hooks)
  "Return non-nil when plugin NAME from ROOT has hooks enabled in WORKSPACE."
  (let* ((state (mevedel-plugins--state-plist name workspace))
         (fingerprint (mevedel-plugins--hook-fingerprint-from-hooks hooks)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (mevedel-plugins--state-enabled-p name workspace root)
         (equal fingerprint (plist-get state :hooks-fingerprint)))))

(defun mevedel-plugins--enabled-p (plugin &optional workspace)
  "Return non-nil when PLUGIN is enabled in WORKSPACE."
  (mevedel-plugins--state-enabled-p
   (mevedel-plugin-name plugin)
   workspace
   (mevedel-plugin-root plugin)))

(defun mevedel-plugins--hooks-enabled-p (plugin &optional workspace)
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
     workspace)))

(defun mevedel-plugins--remove-state-for-source
    (name root &optional workspace)
  "Remove NAME state only when it points at source ROOT."
  (when-let* ((state (mevedel-plugins--read-state workspace))
              (entry (assoc name state))
              ((mevedel-plugins--same-root-p
                root
                (plist-get (cdr entry) :source-root))))
    (mevedel-plugins--write-state
     (cl-remove name (copy-tree state)
                :key #'car
                :test #'equal)
     workspace)))

(defun mevedel-plugins-enabled (&optional workspace)
  "Return installed plugins enabled in WORKSPACE."
  (let (enabled)
    (dolist (plugin (mevedel-plugins-list workspace) (nreverse enabled))
      (when (mevedel-plugins--enabled-p plugin workspace)
        (push plugin enabled)))))

(defun mevedel-plugins-skill-dirs (&optional workspace)
  "Return enabled plugin skill directories as source-tagged entries.
Only plugins enabled in WORKSPACE are returned."
  (let (entries)
    (dolist (plugin (mevedel-plugins-enabled workspace) (nreverse entries))
      (when-let* ((dir (mevedel-plugin-skills-dir plugin))
                  ((file-directory-p dir)))
        (push (cons (file-name-as-directory (expand-file-name dir))
                    (cons 'plugin (mevedel-plugin-name plugin)))
              entries)))))

(defun mevedel-plugins--state-entry-for-plugin (plugin &optional workspace)
  "Return persisted state plist for PLUGIN in WORKSPACE, with defaults applied."
  (let* ((state (copy-sequence
                 (or (mevedel-plugins--state-plist
                      (mevedel-plugin-name plugin) workspace)
                     '(:enabled nil :hooks-enabled nil)))))
    (setq state
          (plist-put state
                     :source-root
                     (mevedel-plugins--source-root
                      (mevedel-plugin-root plugin))))
    state))

(defun mevedel-plugins--hook-rules (plugin)
  "Return normalized hook rules declared by PLUGIN."
  (mevedel-plugins--hook-rules-from-hooks
   (mevedel-plugin-hooks plugin)))

(defun mevedel-plugins--hook-rule-events (plugin)
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
        (dolist (handler (plist-get group :hooks))
          (pcase (plist-get handler :type)
            ('command
             (push (format "command %s" (plist-get handler :command))
                   handlers))
            ('elisp
             (push (format "elisp %s" (plist-get handler :function))
                   handlers))))))
    (sort (delete-dups handlers) #'string<)))

(defun mevedel-plugins--hook-consent-summary (plugin &optional workspace)
  "Return a concise hook consent summary for PLUGIN in WORKSPACE."
  (let ((events (mevedel-plugins--hook-rule-events plugin))
        (handlers (mevedel-plugins--hook-rule-handlers plugin))
        (skills (mevedel-plugins--skill-count plugin)))
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
  (let* ((fingerprint (mevedel-plugins--hook-fingerprint plugin))
         (state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace)))
    (or (not fingerprint)
        (equal fingerprint (plist-get state :hooks-fingerprint))
        (yes-or-no-p
         (concat (mevedel-plugins--hook-consent-summary plugin workspace)
                 " ")))))

(defun mevedel-plugins--ensure-source-switch-consent
    (plugin &optional workspace)
  "Return non-nil when PLUGIN may replace an active source binding."
  (let* ((state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace))
         (source-root (plist-get state :source-root)))
    (or (not (plist-get state :enabled))
        (mevedel-plugins--same-root-p
         (mevedel-plugin-root plugin) source-root)
        (yes-or-no-p
         (format "Switch plugin %s activation from %s to %s? "
                 (mevedel-plugin-name plugin)
                 source-root
                 (mevedel-plugin-root plugin))))))

(defun mevedel-plugins--write-enabled-state
    (plugin hooks-enabled &optional workspace)
  "Persist PLUGIN as enabled, with HOOKS-ENABLED in WORKSPACE."
  (let ((fingerprint (mevedel-plugins--hook-fingerprint plugin)))
    (mevedel-plugins--write-state-entry
     (mevedel-plugin-name plugin)
     (list :enabled t
           :hooks-enabled (and hooks-enabled fingerprint t)
           :source-root (mevedel-plugins--source-root
                         (mevedel-plugin-root plugin))
           :hooks-fingerprint (and hooks-enabled fingerprint))
     workspace)))

(defun mevedel-plugins-enable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as enabled in WORKSPACE."
  (when-let* ((plugin (mevedel-plugins--find plugin-name workspace)))
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
  (let ((plugin (mevedel-plugins--find plugin-name workspace)))
    (mevedel-plugins--write-state-entry
     plugin-name
     (list :enabled nil
           :hooks-enabled nil
           :source-root (and plugin
                             (mevedel-plugins--source-root
                              (mevedel-plugin-root plugin)))
           :hooks-fingerprint nil)
     workspace)))

(defun mevedel-plugins-enable-hooks (plugin-name &optional workspace)
  "Persist hooks as enabled for PLUGIN-NAME in WORKSPACE."
  (when-let* ((plugin (mevedel-plugins--find plugin-name workspace)))
    (and (mevedel-plugin-hooks plugin)
         (mevedel-plugins--enabled-p plugin workspace)
         (mevedel-plugins--ensure-hook-consent plugin workspace)
         (progn
           (mevedel-plugins--write-enabled-state plugin t workspace)
           t))))

(defun mevedel-plugins-disable-hooks (plugin-name &optional workspace)
  "Persist hooks as disabled for PLUGIN-NAME in WORKSPACE."
  (let* ((plugin (mevedel-plugins--find plugin-name workspace))
         (state (copy-sequence (or (mevedel-plugins--state-plist
                                    plugin-name workspace)
                                   '(:enabled nil :hooks-enabled nil)))))
    (when plugin
      (setq state
            (plist-put state
                       :source-root
                       (mevedel-plugins--source-root
                        (mevedel-plugin-root plugin)))))
    (setq state (plist-put state :hooks-enabled nil))
    (mevedel-plugins--write-state-entry plugin-name state workspace)))


;;
;;; Git install/update

(defun mevedel-plugins--process-git (directory args)
  "Run git ARGS in DIRECTORY.
Return (STATUS OUTPUT)."
  (let ((default-directory directory))
    (with-temp-buffer
      (list (apply #'process-file "git" nil (list (current-buffer) t) nil args)
            (string-trim (buffer-string))))))

(defvar mevedel-plugins-git-executor #'mevedel-plugins--process-git
  "Function called to run git.
It receives DIRECTORY and ARGS, and returns (STATUS OUTPUT).")

(defun mevedel-plugins--github-target-p (target)
  "Return non-nil when TARGET is a supported OWNER/REPO GitHub target."
  (and (stringp target)
       (string-match
        "\\`\\([A-Za-z0-9_.-]+\\)/\\([A-Za-z0-9_.-]+\\)\\'" target)
       (not (member (match-string 1 target) '("." "..")))
       (not (member (match-string 2 target) '("." "..")))))

(defun mevedel-plugins--repo-name (target)
  "Return repository name from GitHub TARGET."
  (cadr (split-string target "/" t)))

(defun mevedel-plugins--install-target-repo (target)
  "Return GitHub OWNER/REPO for install TARGET, or nil."
  (and (mevedel-plugins--github-target-p target) target))

(defun mevedel-plugins--github-owner (target)
  "Return owner name from GitHub TARGET."
  (car (split-string target "/" t)))

(defun mevedel-plugins--github-install-dir (target)
  "Return install directory for GitHub TARGET."
  (file-name-concat (mevedel-plugins-dir)
                    "github.com"
                    (mevedel-plugins--github-owner target)
                    (mevedel-plugins--repo-name target)))

(defun mevedel-plugins--git (directory args)
  "Run git ARGS in DIRECTORY through `mevedel-plugins-git-executor'."
  (condition-case err
      (let ((result (funcall mevedel-plugins-git-executor directory args)))
        (if (and (consp result) (integerp (car result)))
            result
          (list 1 (format "%s" result))))
    (error (list 1 (error-message-string err)))))

(defun mevedel-plugins--transfer-state-entry
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

(defun mevedel-plugins--install (target)
  "Install GitHub plugin TARGET."
  (let ((repo (mevedel-plugins--install-target-repo target)))
    (if (not (mevedel-plugins--github-target-p repo))
        "Invalid plugin target: use OWNER/REPO or a GitHub repository."
      (let* ((dest (mevedel-plugins--github-install-dir repo))
             (present (file-directory-p dest))
             (args (list "clone" "--depth" "1"
                         (format "https://github.com/%s.git" repo)
                         dest))
             status
             output)
        (if present
            (if-let* ((plugin (mevedel-plugins--read-manifest dest)))
                (let ((name (mevedel-plugin-name plugin)))
                  (format
                   "Plugin %s is already installed; use /plugin update %s."
                   name name))
              (format
               (concat "Plugin path %s already exists, but no Codex plugin "
                       "manifest was found; fix or remove it before "
                       "installing %s.")
               dest target))
          (make-directory (file-name-directory dest) t)
          (pcase-let ((`(,git-status ,git-output)
                       (mevedel-plugins--git (mevedel-plugins-dir) args)))
            (setq status git-status
                  output git-output))
          (if (not (zerop status))
              (format "Failed to install plugin %s: %s"
                      target
                      (if (string-empty-p output) "git failed" output))
            (if-let* ((plugin (mevedel-plugins--read-manifest dest)))
                (format "Installed plugin %s." (mevedel-plugin-name plugin))
              (format "Failed to install plugin %s: no Codex plugin manifest found."
                      target))))))))

(defun mevedel-plugins--update (name &optional workspace)
  "Update installed plugin NAME with git pull.
Preserve plugin state in WORKSPACE."
  (if-let* ((plugin (mevedel-plugins--find name workspace)))
      (let* ((root (mevedel-plugin-root plugin))
             (previous-state (mevedel-plugins--state-entry-for-plugin
                              plugin workspace)))
        (if (not (mevedel-plugins--managed-root-p root))
            (format "Plugin %s is not managed by mevedel; update %s manually."
                    name root)
          (pcase-let* ((`(,status ,output)
                        (mevedel-plugins--git root '("pull" "--ff-only"))))
            (if (not (zerop status))
                (format "Failed to update plugin %s: %s"
                        name
                        (if (string-empty-p output) "git failed" output))
              (if-let* ((updated (mevedel-plugins--read-manifest
                                  root workspace)))
                  (let ((updated-name (mevedel-plugin-name updated)))
                    (mevedel-plugins--transfer-state-entry
                     (mevedel-plugin-name plugin)
                     updated-name
                     previous-state
                     workspace)
                    (format "Updated plugin %s." updated-name))
                (format (concat "Failed to update plugin %s: "
                                "no Codex plugin manifest found.")
                        name))))))
    (format "Unknown plugin: %s." name)))

(defun mevedel-plugins--managed-root-directories ()
  "Return directories that mevedel may update or remove."
  (delete-dups
   (mapcar #'mevedel-plugins--source-root
           (list (mevedel-plugins--legacy-dir)
                 (mevedel-plugins-dir)))))

(defun mevedel-plugins--managed-root-p (root)
  "Return non-nil when ROOT is below a mevedel-managed global root."
  (let ((root (mevedel-plugins--source-root root)))
    (cl-some
     (lambda (managed)
       (and (not (equal root managed))
            (string-prefix-p managed root)))
     (mevedel-plugins--managed-root-directories))))

(defun mevedel-plugins--remove (name &optional workspace)
  "Remove managed installed plugin NAME.
Workspace runtime data is retained."
  (if-let* ((plugin (mevedel-plugins--find name workspace)))
      (let ((root (mevedel-plugin-root plugin)))
        (cond
         ((not (mevedel-plugins--managed-root-p root))
          (format "Plugin %s is not managed by mevedel; remove %s manually."
                  name root))
         ((not (yes-or-no-p
                (format "Remove plugin %s? Runtime data will be kept. "
                        name)))
          (format "Remove cancelled for plugin %s." name))
         (t
          (delete-directory root t)
          (mevedel-plugins--remove-state-for-source
           name root workspace)
          (format "Removed plugin %s." name))))
    (format "Unknown plugin: %s." name)))

(defun mevedel-plugins--refresh-current-session ()
  "Refresh current session skills when `mevedel-skills' is available."
  (when (fboundp 'mevedel-skills-rescan)
    (condition-case err
        (progn
          (mevedel-skills-rescan)
          t)
      (user-error nil)
      (error
       (let ((message (error-message-string err)))
         (display-warning
          'mevedel
          (format "Plugin registry refresh failed: %s" message)
          :warning)
         message)))))

(defun mevedel-plugins--reload ()
  "Reload plugin-visible skills for the current session when possible."
  (let ((result (mevedel-plugins--refresh-current-session)))
    (cond
     ((eq result t)
      "Plugin registry reloaded. Refreshed current session skills.")
     ((stringp result)
      (format "Plugin registry reload failed: %s." result))
     (t
      "Plugin registry reloaded. No active session skills to refresh."))))

(defun mevedel-plugins--with-refresh (message)
  "Refresh current session skills and return MESSAGE."
  (mevedel-plugins--refresh-current-session)
  message)


;;
;;; Plugin list buffer

(defconst mevedel-plugins-list-buffer-name "*mevedel plugins*"
  "Name of the plugin management buffer.")

(defvar-local mevedel-plugins-list--workspace nil
  "Workspace rendered in the current plugin management buffer.")

(defun mevedel-plugins--skill-count (plugin)
  "Return number of SKILL.md files exposed by PLUGIN."
  (if-let* ((dir (mevedel-plugin-skills-dir plugin))
            ((file-directory-p dir)))
      (length (directory-files-recursively dir "\\`SKILL\\.md\\'"))
    0))

(defun mevedel-plugins--hooks-stale-p (plugin &optional workspace)
  "Return non-nil when PLUGIN hook consent is stale in WORKSPACE."
  (let* ((state (mevedel-plugins--state-plist
                 (mevedel-plugin-name plugin) workspace))
         (fingerprint (mevedel-plugins--hook-fingerprint plugin)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (mevedel-plugins--same-root-p
          (mevedel-plugin-root plugin)
          (plist-get state :source-root))
         (not (equal fingerprint
                     (plist-get state :hooks-fingerprint))))))

(defun mevedel-plugins--hooks-status (plugin &optional workspace)
  "Return user-facing hook status for PLUGIN in WORKSPACE."
  (cond
   ((not (mevedel-plugin-hooks plugin)) "none")
   ((mevedel-plugins--hooks-enabled-p plugin workspace) "on")
   ((mevedel-plugins--hooks-stale-p plugin workspace) "needs-consent")
   (t "off")))

(defun mevedel-plugins--active-shadowed-source (plugin &optional workspace)
  "Return shadowed active source for PLUGIN in WORKSPACE, or nil."
  (let ((state (mevedel-plugins--state-plist
                (mevedel-plugin-name plugin) workspace)))
    (and (plist-get state :enabled)
         (not (mevedel-plugins--same-root-p
               (mevedel-plugin-root plugin)
               (plist-get state :source-root)))
         (cl-find-if
          (lambda (shadow)
            (mevedel-plugins--same-root-p
             (mevedel-plugin-root shadow)
             (plist-get state :source-root)))
          (mevedel-plugin-shadowed plugin)))))

(defun mevedel-plugins--plugin-source-label (plugin)
  "Return a compact source label for PLUGIN."
  (abbreviate-file-name (mevedel-plugin-root plugin)))

(defun mevedel-plugins--plugin-line (plugin &optional workspace)
  "Return one management-buffer line for PLUGIN in WORKSPACE."
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

(defun mevedel-plugins--shadowed-lines (plugin &optional workspace)
  "Return shadowed-source lines for PLUGIN in WORKSPACE."
  (let ((active-shadow (mevedel-plugins--active-shadowed-source
                        plugin workspace)))
    (mapcar
     (lambda (shadow)
       (format "  shadowed%s: %s"
               (if (eq shadow active-shadow) " active" "")
               (mevedel-plugins--plugin-source-label shadow)))
     (mevedel-plugin-shadowed plugin))))

(defun mevedel-plugins-list-refresh ()
  "Refresh the current plugin management buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (workspace mevedel-plugins-list--workspace))
    (erase-buffer)
    (insert (format "mevedel plugins%s\n\n"
                    (if workspace
                        (format " for %s" (mevedel-workspace-root workspace))
                      "")))
    (let ((plugins (mevedel-plugins-list workspace)))
      (if plugins
          (dolist (plugin plugins)
            (let ((start (point)))
              (insert (mevedel-plugins--plugin-line plugin workspace) "\n")
              (add-text-properties
               start (point)
               `(mevedel-plugin-name ,(mevedel-plugin-name plugin)
                 mouse-face highlight))
              (dolist (line (mevedel-plugins--shadowed-lines
                             plugin workspace))
                (insert line "\n"))))
        (insert "No plugins installed.\n")))
    (goto-char (point-min))
    (forward-line 2)))

(defun mevedel-plugins-list--name-at-point ()
  "Return plugin name at point in a management buffer."
  (or (get-text-property (point) 'mevedel-plugin-name)
      (save-excursion
        (beginning-of-line)
        (get-text-property (point) 'mevedel-plugin-name))
      (user-error "No plugin on this line")))

(defun mevedel-plugins-list-enable ()
  "Enable the plugin at point."
  (interactive)
  (let ((name (mevedel-plugins-list--name-at-point)))
    (if (mevedel-plugins-enable name mevedel-plugins-list--workspace)
        (message "mevedel: enabled plugin %s" name)
      (message "mevedel: enable cancelled for plugin %s" name))
    (mevedel-plugins--refresh-current-session)
    (mevedel-plugins-list-refresh)))

(defun mevedel-plugins-list-disable ()
  "Disable the plugin at point."
  (interactive)
  (let ((name (mevedel-plugins-list--name-at-point)))
    (mevedel-plugins-disable name mevedel-plugins-list--workspace)
    (message "mevedel: disabled plugin %s" name)
    (mevedel-plugins--refresh-current-session)
    (mevedel-plugins-list-refresh)))

(defun mevedel-plugins-list-toggle-hooks ()
  "Toggle hooks for the plugin at point."
  (interactive)
  (let* ((name (mevedel-plugins-list--name-at-point))
         (plugin (mevedel-plugins--find
                  name mevedel-plugins-list--workspace)))
    (cond
     ((not (mevedel-plugin-hooks plugin))
      (message "mevedel: plugin %s declares no hooks" name))
     ((not (mevedel-plugins--enabled-p
            plugin mevedel-plugins-list--workspace))
      (message "mevedel: plugin %s is not enabled" name))
     ((mevedel-plugins--hooks-enabled-p
       plugin mevedel-plugins-list--workspace)
      (mevedel-plugins-disable-hooks
       name mevedel-plugins-list--workspace)
      (message "mevedel: disabled hooks for plugin %s" name))
     ((mevedel-plugins-enable-hooks
       name mevedel-plugins-list--workspace)
      (message "mevedel: enabled hooks for plugin %s" name))
     (t
      (message "mevedel: hook enable cancelled for plugin %s" name)))
    (mevedel-plugins--refresh-current-session)
    (mevedel-plugins-list-refresh)))

(defun mevedel-plugins-list-update ()
  "Update the plugin at point."
  (interactive)
  (let ((message (mevedel-plugins--update
                  (mevedel-plugins-list--name-at-point)
                  mevedel-plugins-list--workspace)))
    (mevedel-plugins--refresh-current-session)
    (mevedel-plugins-list-refresh)
    (message "%s" message)))

(defun mevedel-plugins-list-remove ()
  "Remove the plugin at point."
  (interactive)
  (let ((message (mevedel-plugins--remove
                  (mevedel-plugins-list--name-at-point)
                  mevedel-plugins-list--workspace)))
    (mevedel-plugins--refresh-current-session)
    (mevedel-plugins-list-refresh)
    (message "%s" message)))

(defun mevedel-plugins-list-details ()
  "Show details for the plugin at point."
  (interactive)
  (let* ((name (mevedel-plugins-list--name-at-point))
         (workspace mevedel-plugins-list--workspace)
         (plugin (mevedel-plugins--find
                  name workspace)))
    (with-help-window "*mevedel plugin details*"
      (princ (mevedel-plugins--plugin-line
              plugin workspace))
      (princ "\n\n")
      (when-let* ((description (mevedel-plugin-description plugin)))
        (princ description)
        (princ "\n\n"))
      (when (mevedel-plugin-hooks plugin)
        (princ (mevedel-plugins--hook-consent-summary
                plugin workspace))
        (princ "\n")))))

(defvar mevedel-plugins-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'mevedel-plugins-list-refresh)
    (define-key map (kbd "e") #'mevedel-plugins-list-enable)
    (define-key map (kbd "d") #'mevedel-plugins-list-disable)
    (define-key map (kbd "h") #'mevedel-plugins-list-toggle-hooks)
    (define-key map (kbd "u") #'mevedel-plugins-list-update)
    (define-key map (kbd "x") #'mevedel-plugins-list-remove)
    (define-key map (kbd "RET") #'mevedel-plugins-list-details)
    map)
  "Keymap for `mevedel-plugins-list-mode'.")

(define-derived-mode mevedel-plugins-list-mode special-mode "mevedel-plugins"
  "Major mode for managing mevedel plugins.")

(defun mevedel-plugins-list-open (&optional workspace)
  "Open the plugin management buffer for WORKSPACE."
  (let ((buffer (get-buffer-create mevedel-plugins-list-buffer-name)))
    (with-current-buffer buffer
      (mevedel-plugins-list-mode)
      (setq mevedel-plugins-list--workspace workspace)
      (mevedel-plugins-list-refresh))
    (display-buffer buffer)
    buffer))


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
  (or (mevedel-plugins--find name workspace)
      (format "Unknown plugin: %s." name)))

(defun mevedel-plugins-slash-command (args)
  "Run local `/plugin' command from ARGS.
Return a user-facing result string."
  (let ((parts (split-string (string-trim (or args "")) "[ \t\n]+" t))
        (workspace (mevedel-plugins--current-workspace)))
    (pcase parts
      ((or `() `("list"))
       (mevedel-plugins-list-open workspace)
       nil)
      ((and (or `("enable" ,_)
                `("disable" ,_)
                `("hooks" "enable" ,_)
                `("hooks" "disable" ,_)
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
      (`("hooks" "enable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (cond
            ((not (mevedel-plugin-hooks plugin))
             (format "Plugin %s declares no hooks." name))
            ((not (mevedel-plugins--enabled-p plugin workspace))
             (format "Plugin %s is not enabled." name))
            ((mevedel-plugins-enable-hooks name workspace)
             (mevedel-plugins--with-refresh
              (format "Enabled hooks for plugin %s." name)))
            (t
             (format "Hook enable cancelled for plugin %s." name))))))
      (`("hooks" "disable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable-hooks name workspace)
           (mevedel-plugins--with-refresh
            (format "Disabled hooks for plugin %s." name)))))
      (`("hooks" ,name "on")
       (let ((plugin (mevedel-plugins--known-or-message name workspace)))
         (if (stringp plugin)
             plugin
           (cond
            ((not (mevedel-plugin-hooks plugin))
             (format "Plugin %s declares no hooks." name))
            ((not (mevedel-plugins--enabled-p plugin workspace))
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
        (mevedel-plugins--install target)))
      (`("update" ,name)
       (mevedel-plugins--with-refresh
        (mevedel-plugins--update name workspace)))
      ((or `("remove" ,name) `("uninstall" ,name))
       (mevedel-plugins--with-refresh
        (mevedel-plugins--remove name workspace)))
      (`("reload") (mevedel-plugins--reload))
      (_ mevedel-plugins--usage))))


(provide 'mevedel-plugins)

;;; mevedel-plugins.el ends here
