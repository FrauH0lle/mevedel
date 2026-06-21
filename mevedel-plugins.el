;;; mevedel-plugins.el -- Codex-style plugin discovery -*- lexical-binding: t -*-

;;; Commentary:

;; Minimal Codex-style plugin support. Plugins are installed under
;; `mevedel-user-dir'/plugins and each plugin root contains
;; .codex-plugin/plugin.json. This module discovers manifests, persists
;; enabled/hook state, and implements the local `/plugin' command body.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)
(require 'subr-x)
(require 'mevedel-structs)

;; `mevedel-skills'
(declare-function mevedel-skills-rescan "mevedel-skills" ())

;; `subr'
(defvar read-eval)

(defcustom mevedel-plugin-extra-roots nil
  "Additional local Codex plugin roots to scan.
Each entry may be a direct plugin root or a directory containing plugin
roots below it."
  :type '(repeat directory)
  :group 'mevedel)

(defconst mevedel-plugins--default-install-directory
  (file-name-concat mevedel-user-dir "plugins")
  "Default value for `mevedel-plugin-install-directory'.")

(defconst mevedel-plugins--default-data-directory
  (file-name-concat mevedel-user-dir "plugin-data")
  "Default value for `mevedel-plugin-data-directory'.")

(defcustom mevedel-plugin-install-directory
  mevedel-plugins--default-install-directory
  "Directory containing installed Codex plugins."
  :type 'directory
  :group 'mevedel)

(defcustom mevedel-plugin-data-directory
  mevedel-plugins--default-data-directory
  "Directory where plugin hooks store runtime data."
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
  enabled-p
  hooks-enabled-p)


;;
;;; Native plugin hooks

(defconst mevedel-plugins--superpowers-tool-mapping
  "Mevedel Tool Mapping

- Read a file: Read
- Create or edit a file: Write or Edit
- Run shell: Bash
- Search contents: Grep
- Find files: Glob
- Fetch URL: WebFetch
- Search web: WebSearch
- Invoke skill: Skill tool or /skill-name
- Dispatch a subagent: Agent
- Send message: SendMessage
- Task tracking: TaskCreate, TaskUpdate, TaskList, TaskGet"
  "Mevedel-native tool mapping for the Superpowers bootstrap.")

(defun mevedel-plugins--superpowers-bootstrap-context (plugin)
  "Return Superpowers bootstrap context for PLUGIN, or nil.
The context is built from PLUGIN's bundled using-superpowers skill."
  (let ((skill-file (file-name-concat
                     (mevedel-plugin-root plugin)
                     "skills" "using-superpowers" "SKILL.md")))
    (when (file-readable-p skill-file)
      (with-temp-buffer
        (insert "<EXTREMELY-IMPORTANT>\n")
        (insert "You have superpowers in mevedel. Use the bundled ")
        (insert "using-superpowers skill instructions below, adapted ")
        (insert "to mevedel's native tools.\n\n")
        (insert-file-contents skill-file)
        (goto-char (point-max))
        (unless (bolp)
          (insert "\n"))
        (insert "\n")
        (insert mevedel-plugins--superpowers-tool-mapping)
        (insert "\n</EXTREMELY-IMPORTANT>\n")
        (buffer-string)))))

(defun mevedel-plugins-superpowers-bootstrap-hook (event)
  "Return Superpowers bootstrap context for a `SessionStart' EVENT."
  (let* ((handler (plist-get event :hook-handler))
         (root (or (plist-get handler :plugin-root)
                   (plist-get event :plugin-root)))
         (name (or (plist-get handler :plugin-name)
                   (plist-get event :plugin-name)
                   "superpowers")))
    (when root
      (when-let* ((context
                   (mevedel-plugins--superpowers-bootstrap-context
                    (mevedel-plugin--create :name name :root root))))
        (list :additional-context context)))))

(defun mevedel-plugins-native-hook-rules (plugin)
  "Return mevedel-native hook rules for PLUGIN, or nil."
  (when (equal (mevedel-plugin-name plugin) "superpowers")
    '((SessionStart
       ((:matcher "startup|resume|clear"
         :hooks ((:type elisp
                  :function mevedel-plugins-superpowers-bootstrap-hook))))))))


;;
;;; Paths

(defun mevedel-plugins-dir ()
  "Return the directory containing installed plugins."
  (file-name-as-directory
   (expand-file-name
    (if (equal mevedel-plugin-install-directory
               mevedel-plugins--default-install-directory)
        (file-name-concat mevedel-user-dir "plugins")
      mevedel-plugin-install-directory))))

(defun mevedel-plugins-state-file ()
  "Return the persistent plugin state file."
  (file-name-concat mevedel-user-dir "plugins.el"))

(defun mevedel-plugins-plugin-data-dir (plugin-name)
  "Return persistent data directory for PLUGIN-NAME."
  (file-name-concat
   (file-name-as-directory
    (expand-file-name
     (if (equal mevedel-plugin-data-directory
                mevedel-plugins--default-data-directory)
         (file-name-concat mevedel-user-dir "plugin-data")
       mevedel-plugin-data-directory)))
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

(defun mevedel-plugins--read-manifest (root)
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
               :enabled-p (mevedel-plugins--state-enabled-p name)
               :hooks-enabled-p (mevedel-plugins--state-hooks-enabled-p name))))
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
    roots))

(defun mevedel-plugins--plugin-roots ()
  "Return deduplicated plugin roots from install and extra roots."
  (let ((seen (make-hash-table :test #'equal))
        roots)
    (dolist (root (cons (mevedel-plugins-dir) mevedel-plugin-extra-roots))
      (dolist (plugin-root (mevedel-plugins--collect-roots-under root))
        (let ((key (file-name-as-directory (file-truename plugin-root))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push plugin-root roots)))))
    (sort roots #'string<)))

(defun mevedel-plugins--drop-duplicate-names (plugins)
  "Drop every plugin whose manifest name appears more than once."
  (let ((counts (make-hash-table :test #'equal))
        dropped
        kept)
    (dolist (plugin plugins)
      (cl-incf (gethash (mevedel-plugin-name plugin) counts 0)))
    (dolist (plugin plugins)
      (let ((name (mevedel-plugin-name plugin)))
        (if (> (gethash name counts 0) 1)
            (push name dropped)
          (push plugin kept))))
    (when dropped
      (display-warning
       'mevedel
       (format "Ignoring duplicate plugin name(s): %s"
               (mapconcat #'identity
                          (sort (delete-dups dropped) #'string<)
                          ", "))
       :warning))
    (nreverse kept)))

(defun mevedel-plugins-list ()
  "Return installed Codex-style plugins."
  (let (plugins)
    (dolist (root (mevedel-plugins--plugin-roots))
      (when-let* ((plugin (mevedel-plugins--read-manifest root)))
        (push plugin plugins)))
    (mevedel-plugins--drop-duplicate-names
     (sort plugins
           (lambda (a b)
             (string< (mevedel-plugin-name a)
                      (mevedel-plugin-name b)))))))

(defun mevedel-plugins--find (name)
  "Return installed plugin named NAME, or nil."
  (catch 'found
    (dolist (plugin (mevedel-plugins-list) nil)
      (when (equal name (mevedel-plugin-name plugin))
        (throw 'found plugin)))))

(defun mevedel-plugins-plugin-root (plugin-name)
  "Return root directory for installed PLUGIN-NAME, or nil."
  (when-let* ((plugin (mevedel-plugins--find plugin-name)))
    (mevedel-plugin-root plugin)))


;;
;;; Persistent state

(defun mevedel-plugins--read-state ()
  "Read persistent plugin state."
  (let ((file (mevedel-plugins-state-file)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((read-eval nil))
              (let ((state (read (current-buffer))))
                (and (listp state) state))))
        (error nil)))))

(defun mevedel-plugins--write-state (state)
  "Persist plugin STATE."
  (let ((file (mevedel-plugins-state-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert ";; Mevedel plugin state\n")
      (insert ";; Auto-generated, safe to edit\n\n")
      (pp state (current-buffer)))))

(defun mevedel-plugins--state-plist (name)
  "Return persisted state plist for plugin NAME."
  (cdr (assoc name (mevedel-plugins--read-state))))

(defun mevedel-plugins--state-enabled-p (name)
  "Return non-nil when plugin NAME is enabled by persisted state."
  (let ((entry (mevedel-plugins--state-plist name)))
    (if (plist-member entry :enabled)
        (plist-get entry :enabled)
      t)))

(defun mevedel-plugins--state-hooks-enabled-p (name)
  "Return non-nil when plugin NAME has hooks enabled by persisted state."
  (and (plist-get (mevedel-plugins--state-plist name) :hooks-enabled) t))

(defun mevedel-plugins--enabled-p (plugin)
  "Return non-nil when PLUGIN is enabled."
  (mevedel-plugins--state-enabled-p (mevedel-plugin-name plugin)))

(defun mevedel-plugins--hooks-enabled-p (plugin)
  "Return non-nil when hooks are enabled for PLUGIN."
  (mevedel-plugins--state-hooks-enabled-p (mevedel-plugin-name plugin)))

(defun mevedel-plugins--current-session ()
  "Return the current buffer's mevedel session, or nil."
  (and (boundp 'mevedel--session) mevedel--session))

(defun mevedel-plugins--enqueue-superpowers-bootstrap (plugin)
  "Append PLUGIN's Superpowers bootstrap context to the current session."
  (when (equal (mevedel-plugin-name plugin) "superpowers")
    (when-let* ((session (mevedel-plugins--current-session))
                (context (mevedel-plugins--superpowers-bootstrap-context plugin)))
      (unless (member context (mevedel-session-hook-context-pending session))
        (setf (mevedel-session-hook-context-pending session)
              (append (mevedel-session-hook-context-pending session)
                      (list context)))))))

(defun mevedel-plugins--enqueue-enabled-superpowers-bootstrap ()
  "Append enabled Superpowers bootstrap context to the current session."
  (when-let* ((plugin (mevedel-plugins--find "superpowers"))
              ((mevedel-plugins--hooks-enabled-p plugin)))
    (mevedel-plugins--enqueue-superpowers-bootstrap plugin)))

(defun mevedel-plugins--set-state (name key value)
  "Set plugin NAME state KEY to VALUE."
  (let* ((state (copy-tree (mevedel-plugins--read-state)))
         (entry (assoc name state))
         (plist (copy-sequence (or (cdr entry)
                                   '(:enabled t :hooks-enabled nil)))))
    (setq plist (plist-put plist key value))
    (if entry
        (setcdr entry plist)
      (push (cons name plist) state))
    (mevedel-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b)))))))

(defun mevedel-plugins-enabled ()
  "Return installed plugins whose state is enabled."
  (let (enabled)
    (dolist (plugin (mevedel-plugins-list) (nreverse enabled))
      (when (mevedel-plugins--enabled-p plugin)
        (push plugin enabled)))))

(defun mevedel-plugins-skill-dirs ()
  "Return enabled plugin skill directories as source-tagged entries."
  (let (entries)
    (dolist (plugin (mevedel-plugins-enabled) (nreverse entries))
      (when-let* ((dir (mevedel-plugin-skills-dir plugin))
                  ((file-directory-p dir)))
        (push (cons (file-name-as-directory (expand-file-name dir))
                    (cons 'plugin (mevedel-plugin-name plugin)))
              entries)))))

(defun mevedel-plugins-enable (plugin-name)
  "Persist PLUGIN-NAME as enabled."
  (mevedel-plugins--set-state plugin-name :enabled t))

(defun mevedel-plugins-disable (plugin-name)
  "Persist PLUGIN-NAME as disabled."
  (mevedel-plugins--set-state plugin-name :hooks-enabled nil)
  (mevedel-plugins--set-state plugin-name :enabled nil))

(defun mevedel-plugins-enable-hooks (plugin-name)
  "Persist hooks as enabled for PLUGIN-NAME."
  (mevedel-plugins-enable plugin-name)
  (mevedel-plugins--set-state plugin-name :hooks-enabled t)
  (when-let* ((plugin (mevedel-plugins--find plugin-name)))
    (mevedel-plugins--enqueue-superpowers-bootstrap plugin)))

(defun mevedel-plugins-disable-hooks (plugin-name)
  "Persist hooks as disabled for PLUGIN-NAME."
  (mevedel-plugins--set-state plugin-name :hooks-enabled nil))


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

(defun mevedel-plugins--state-entry-for-plugin (plugin)
  "Return persisted state plist for PLUGIN, with defaults applied."
  (let ((entry (mevedel-plugins--state-plist (mevedel-plugin-name plugin))))
    (list :enabled (if (plist-member entry :enabled)
                       (plist-get entry :enabled)
                     t)
          :hooks-enabled (and (plist-get entry :hooks-enabled) t))))

(defun mevedel-plugins--transfer-state-entry (old-name new-name plist)
  "Move plugin state PLIST from OLD-NAME to NEW-NAME."
  (let (state)
    (dolist (entry (copy-tree (mevedel-plugins--read-state)))
      (unless (member (car entry) (list old-name new-name))
        (push entry state)))
    (push (cons new-name plist) state)
    (mevedel-plugins--write-state
     (sort state (lambda (a b) (string< (car a) (car b)))))))

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
                (let ((name (mevedel-plugin-name plugin)))
                  (mevedel-plugins-enable name)
                  (if (and (mevedel-plugin-hooks plugin)
                           (yes-or-no-p
                            (format "Enable hooks for plugin %s? " name)))
                      (mevedel-plugins-enable-hooks name)
                    (mevedel-plugins-disable-hooks name))
                  (format "Installed plugin %s." name))
              (format "Failed to install plugin %s: no Codex plugin manifest found."
                      target))))))))

(defun mevedel-plugins--update (name)
  "Update installed plugin NAME with git pull."
  (if-let* ((plugin (mevedel-plugins--find name)))
      (let* ((root (mevedel-plugin-root plugin))
             (previous-state (mevedel-plugins--state-entry-for-plugin
                              plugin))
             (result (mevedel-plugins--git root '("pull" "--ff-only")))
             (status (car result))
             (output (cadr result)))
        (if (not (zerop status))
            (format "Failed to update plugin %s: %s"
                    name
                    (if (string-empty-p output) "git failed" output))
          (if-let* ((updated (mevedel-plugins--read-manifest root)))
              (let ((updated-name (mevedel-plugin-name updated)))
                (mevedel-plugins--transfer-state-entry
                 (mevedel-plugin-name plugin)
                 updated-name
                 previous-state)
                (format "Updated plugin %s." updated-name))
            (format "Failed to update plugin %s: no Codex plugin manifest found."
                    name))))
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
    (mevedel-plugins--enqueue-enabled-superpowers-bootstrap)
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
;;; Slash command

(defconst mevedel-plugins--usage
  (concat "Usage: /plugin list | enable NAME | disable NAME | "
          "hooks enable NAME | hooks disable NAME | hooks NAME on | "
          "hooks NAME off | install TARGET | update NAME | reload")
  "Usage text for `/plugin'.")

(defun mevedel-plugins--list-string ()
  "Return user-facing plugin list."
  (let ((plugins (mevedel-plugins-list)))
    (if plugins
        (mapconcat
         (lambda (plugin)
           (format "%s%s skills:%s hooks:%s"
                   (mevedel-plugin-name plugin)
                   (if-let* ((version (mevedel-plugin-version plugin)))
                       (format " %s" version)
                     "")
                   (if (mevedel-plugins--enabled-p plugin)
                       "on"
                     "off")
                   (if (mevedel-plugins--hooks-enabled-p plugin)
                       "on"
                     "off")))
         plugins
         "\n")
      "No plugins installed.")))

(defun mevedel-plugins--known-or-message (name)
  "Return installed plugin NAME, or a user-facing error string."
  (or (mevedel-plugins--find name)
      (format "Unknown plugin: %s." name)))

(defun mevedel-plugins-slash-command (args)
  "Run local `/plugin' command from ARGS.
Return a user-facing result string."
  (let ((parts (split-string (string-trim (or args "")) "[ \t\n]+" t)))
    (pcase parts
      (`("list") (mevedel-plugins--list-string))
      (`("enable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-enable name)
           (mevedel-plugins--with-refresh
            (format "Enabled plugin %s." name)))))
      (`("disable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable name)
           (mevedel-plugins--with-refresh
            (format "Disabled plugin %s." name)))))
      (`("hooks" "enable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-enable-hooks name)
           (mevedel-plugins--with-refresh
            (format "Enabled hooks for plugin %s." name)))))
      (`("hooks" "disable" ,name)
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable-hooks name)
           (mevedel-plugins--with-refresh
            (format "Disabled hooks for plugin %s." name)))))
      (`("hooks" ,name "on")
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-enable-hooks name)
           (mevedel-plugins--with-refresh
            (format "Enabled hooks for plugin %s." name)))))
      (`("hooks" ,name "off")
       (let ((plugin (mevedel-plugins--known-or-message name)))
         (if (stringp plugin)
             plugin
           (mevedel-plugins-disable-hooks name)
           (mevedel-plugins--with-refresh
            (format "Disabled hooks for plugin %s." name)))))
      (`("install" ,target)
       (mevedel-plugins--with-refresh
        (mevedel-plugins--install target)))
      (`("update" ,name)
       (mevedel-plugins--with-refresh
        (mevedel-plugins--update name)))
      (`("reload") (mevedel-plugins--reload))
      (_ mevedel-plugins--usage))))


(provide 'mevedel-plugins)

;;; mevedel-plugins.el ends here
