;;; mevedel-system.el -- System prompt -*- lexical-binding: t -*-

;;; Commentary:

;; System prompts are assembled from ordered profiles.  Profiles choose
;; reusable named components or inline file/text components; their list
;; order is the rendered order.  Dynamic components receive request-time
;; workspace, directory, session, and buffer context.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)

;; `gptel'
(declare-function gptel--model-name "ext:gptel" (model))
(declare-function gptel--parse-tools "ext:gptel-request" (backend tools))
(declare-function gptel-backend-name "ext:gptel" (cl-x) t)
(declare-function gptel-tool-args "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-description "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-system-prompt)
(defvar gptel-tools)

;; `mevedel-goal'
(declare-function mevedel-goal-active-context "mevedel-goal" (session))

;; `mevedel-resource'
(declare-function mevedel-resource-completion-metadata
                  "mevedel-resource" (context))

;; `mevedel-skills-prompt'
(declare-function mevedel-skills-prompt-section
                  "mevedel-skills-prompt" (session &optional buffer))

;; `mevedel-structs'
(declare-function mevedel-session-deferred-pending "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-p "mevedel-structs" (cl-x))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-reasoning-effort "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-sandbox-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-all "mevedel-tool-registry" ())
(declare-function mevedel-tool-gptel-tool "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-prompt-source "mevedel-tool-registry" (cl-x) t)

;; `mevedel-utilities'
(declare-function mevedel--environment-info-string "mevedel-utilities"
                  (&optional workspace working-directory))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

(defvar mevedel-system--source-dir
  (let* ((lib (or load-file-name buffer-file-name))
         (el-file (if (and lib (string-suffix-p ".elc" lib))
                      (substring lib 0 -1)
                    lib)))
    (file-name-directory (file-truename el-file)))
  "Directory containing the mevedel source files.")

(defcustom mevedel-memory-dirs
  '(".mevedel/memory/"
    ".agents/memory/"
    "~/.mevedel/memory/"
    "~/.agents/memory/")
  "Directories scanned for persistent memory indexes.

Relative paths are resolved against the current workspace root.  The
default order is local mevedel, local shared agents, global mevedel,
then global shared agents."
  :type '(repeat directory)
  :group 'mevedel)

(defun mevedel-system--prompt-path (relative-path)
  "Return the absolute prompt path for RELATIVE-PATH."
  (expand-file-name relative-path mevedel-system--source-dir))

(defun mevedel-system-render-template (template replacements)
  "Return TEMPLATE with `{{NAME}}' placeholders replaced.
REPLACEMENTS is an alist of (NAME . VALUE), where NAME is a string."
  (with-temp-buffer
    (insert template)
    (dolist (replacement replacements)
      (goto-char (point-min))
      (let ((placeholder (format "{{%s}}" (car replacement)))
            (value (or (cdr replacement) "")))
        (while (search-forward placeholder nil t)
          (replace-match value t t))))
    (buffer-string)))

(defun mevedel-system--read-prompt-file (relative-path)
  "Read RELATIVE-PATH from the mevedel prompt directory."
  (let ((path (mevedel-system--prompt-path relative-path)))
    (unless (file-readable-p path)
      (error "Prompt file not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun mevedel-system-render-prompt-file (relative-path &optional replacements)
  "Return prompt file RELATIVE-PATH with REPLACEMENTS applied."
  (mevedel-system-render-template
   (mevedel-system--read-prompt-file relative-path)
   replacements))

;;
;;; Prompt component and profile registries

(cl-defstruct (mevedel-system-context
               (:constructor mevedel-system-context--create))
  "Request-time context passed to prompt component producers."
  workspace
  working-directory
  session
  refresh-buffer)

(cl-defstruct (mevedel-system-prompt-component
               (:constructor mevedel-system-prompt-component--create))
  "A named system prompt component."
  name
  file
  text
  producer
  cache
  cache-key)

(defvar mevedel-system--prompt-components nil
  "Alist mapping names to reusable prompt components.")

(defvar mevedel-system--prompt-profiles nil
  "Alist mapping names to ordered prompt profile plists.")

(defvar mevedel-system--prompt-component-cache (make-hash-table :test #'equal)
  "Memoized prompt component values keyed by component name and cache key.")

(defconst mevedel-system--prompt-cache-miss
  (make-symbol "mevedel-prompt-cache-miss")
  "Sentinel for missing prompt component cache entries.")

(defun mevedel-system--register-prompt-component (name props)
  "Register reusable prompt component NAME from PROPS."
  (let* ((sources (delq nil
                        (mapcar (lambda (key)
                                  (and (plist-member props key) key))
                                '(:file :text :producer))))
         (file (plist-get props :file))
         (text (plist-get props :text))
         (producer (plist-get props :producer))
         (cache (plist-get props :cache))
         (cache-key (plist-get props :cache-key)))
    (unless (= (length sources) 1)
      (error "Prompt component requires exactly one source: %s" name))
    (when (and (plist-member props :file) (not (stringp file)))
      (error "Prompt component :file must be a string: %s" name))
    (when (and (plist-member props :text) (not (stringp text)))
      (error "Prompt component :text must be a string: %s" name))
    (when (and (plist-member props :producer) (not (functionp producer)))
      (error "Prompt component :producer must be a function: %s" name))
    (unless (memq cache '(nil global keyed))
      (error "Invalid prompt component cache mode: %s" cache))
    (when (and (eq cache 'keyed)
               (not (functionp cache-key)))
      (error "Keyed prompt component requires :cache-key: %s" name))
    (mevedel-system-clear-prompt-component-cache name)
    (setf (alist-get name mevedel-system--prompt-components)
          (mevedel-system-prompt-component--create
           :name name
           :file file
           :text text
           :producer producer
           :cache cache
           :cache-key cache-key)))
  name)

(defmacro mevedel-define-prompt-component (name &rest props)
  "Define reusable prompt component NAME from PROPS.

Exactly one of `:file', `:text', or `:producer' is required.  Producers
receive a `mevedel-system-context'.  `:cache' may be `global', `keyed',
or nil; keyed components also require a `:cache-key' function."
  `(mevedel-system--register-prompt-component
    ',name
    (list ,@props)))

(defun mevedel-system--register-prompt-profile (name props)
  "Register ordered prompt profile NAME from PROPS."
  (setf (alist-get name mevedel-system--prompt-profiles) props)
  name)

(defmacro mevedel-define-prompt-profile (name &rest props)
  "Define prompt profile NAME from PROPS.

`:workspace-aware' declares whether the profile requires explicit
`workspace-config' and `environment' components.  `:components' is the
ordered component list."
  `(mevedel-system--register-prompt-profile
    ',name
    (list ,@props)))

(defun mevedel-system-clear-prompt-component-cache (&optional name)
  "Clear memoized prompt component values.

When NAME is nil, clear all prompt component cache entries."
  (if (null name)
      (clrhash mevedel-system--prompt-component-cache)
    (let (keys)
      (maphash
       (lambda (key _value)
         (when (eq (car-safe key) name)
           (push key keys)))
       mevedel-system--prompt-component-cache)
      (dolist (key keys)
        (remhash key mevedel-system--prompt-component-cache)))))

(defun mevedel-system--component-cache-key (component context)
  "Return cache key for COMPONENT and CONTEXT, or nil when uncached."
  (let ((cache (mevedel-system-prompt-component-cache component))
        (name (mevedel-system-prompt-component-name component)))
    (pcase cache
      ('global (list name :global))
      ('keyed (list name
                    (funcall (mevedel-system-prompt-component-cache-key
                              component)
                             context)))
      ('nil nil))))

(defun mevedel-system--render-component-value (component context)
  "Return COMPONENT's uncached value for CONTEXT."
  (cond
   ((mevedel-system-prompt-component-file component)
    (mevedel-system-render-prompt-file
     (mevedel-system-prompt-component-file component)))
   ((mevedel-system-prompt-component-producer component)
    (funcall (mevedel-system-prompt-component-producer component) context))
   (t (mevedel-system-prompt-component-text component))))

(defun mevedel-system--render-component (component context)
  "Return rendered COMPONENT for CONTEXT."
  (let ((key (mevedel-system--component-cache-key component context)))
    (if (not key)
        (mevedel-system--render-component-value component context)
      (let ((cached (gethash key mevedel-system--prompt-component-cache
                             mevedel-system--prompt-cache-miss)))
        (if (not (eq cached mevedel-system--prompt-cache-miss))
            cached
          (let ((value (mevedel-system--render-component-value
                        component context)))
            (puthash key value mevedel-system--prompt-component-cache)
            value))))))


;;
;;; Dynamic section helpers

(defun mevedel-system--workspace-root (workspace)
  "Return WORKSPACE's root, or the current workspace root."
  (mevedel-workspace-root (or workspace (mevedel-workspace))))

(defun mevedel-system--file-cache-key (file)
  "Return metadata cache key for FILE."
  (let ((expanded (expand-file-name file)))
    (if (file-exists-p expanded)
        (let ((attrs (file-attributes expanded)))
          (list :file (file-truename expanded)
                :mtime (file-attribute-modification-time attrs)
                :size (file-attribute-size attrs)))
      (list :missing expanded))))

(defun mevedel-system--memory-root (workspace dir)
  "Return metadata plist for memory DIR in WORKSPACE."
  (let* ((global (or (file-name-absolute-p dir)
                     (string-prefix-p "~" dir)))
         (path (directory-file-name
                (expand-file-name (substitute-in-file-name dir))))
         (family (cond
                  ((string-match-p "\\(?:\\`\\|/\\)\\.mevedel/memory\\'" path)
                   "mevedel")
                  ((string-match-p "\\(?:\\`\\|/\\)\\.agents/memory\\'" path)
                   "agents")))
         (label (string-join
                 (delq nil
                       (list (if global "Global" "Local")
                             family
                             "memory"))
                 " "))
         (root (file-name-as-directory
                (expand-file-name
                 dir
                 (and (not global)
                      (mevedel-system--workspace-root workspace)))))
         (file (file-name-concat root "MEMORY.md")))
    (list :dir root
          :file file
          :label label)))

(defun mevedel-system--memory-roots (workspace)
  "Return configured memory root metadata for WORKSPACE."
  (mapcar (lambda (dir) (mevedel-system--memory-root workspace dir))
          mevedel-memory-dirs))

(defun mevedel-system--memory-files (workspace)
  "Return configured memory index files for WORKSPACE."
  (mapcar (lambda (root) (plist-get root :file))
          (mevedel-system--memory-roots workspace)))

(defun mevedel-system--human-time-age (time)
  "Return a short human age string for TIME."
  (let* ((seconds (max 0 (float-time (time-subtract (current-time) time))))
         (days (floor (/ seconds 86400))))
    (cond
     ((zerop days) "today")
     ((= days 1) "yesterday")
     (t (format "%d days ago" days)))))

(defun mevedel-system--current-date ()
  "Return today's date for prompt cache keys."
  (format-time-string "%Y-%m-%d"))

(defun mevedel-system--memory-updated-header (memory-file)
  "Return last-updated metadata for MEMORY-FILE."
  (let* ((attrs (file-attributes memory-file))
         (mtime (file-attribute-modification-time attrs)))
    (format "<!-- Last updated: %s (%s) -->"
            (format-time-string "%Y-%m-%d" mtime)
            (mevedel-system--human-time-age mtime))))

(defun mevedel-system--read-memory-index (memory-file)
  "Return the first 200 lines from MEMORY-FILE."
  (string-join
   (with-temp-buffer
     (insert-file-contents memory-file)
     (cl-loop repeat 200
              unless (eobp)
              collect (prog1 (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))
                        (forward-line 1))))
   "\n"))

(defun mevedel-system--memory-root-content (root)
  "Return prompt content for memory ROOT, or nil when its index is absent."
  (let ((file (plist-get root :file)))
    (when (file-exists-p file)
      (string-join
       (list (format "### %s" (plist-get root :label))
             (format "Directory: %s" (plist-get root :dir))
             (mevedel-system--memory-updated-header file)
             (mevedel-system--read-memory-index file))
       "\n"))))

(defun mevedel-system--memory-roots-description (workspace)
  "Return configured memory roots for WORKSPACE as prompt text."
  (mapconcat
   (lambda (root)
     (format "- %s: %s"
             (plist-get root :label)
             (plist-get root :dir)))
   (mevedel-system--memory-roots workspace)
   "\n"))

(defun mevedel-system--memory-content (workspace)
  "Return merged WORKSPACE memory index content, or an empty notice."
  (let ((sections (delq nil
                        (mapcar #'mevedel-system--memory-root-content
                                (mevedel-system--memory-roots workspace)))))
    (if sections
        (string-join sections "\n\n")
      "Your memory indexes are currently empty. As you complete tasks, save
durable memories in separate topic files and link them from MEMORY.md.
Anything linked from MEMORY.md can be discovered in future conversations.")))

(defconst mevedel-system--memory-prompt
  (lambda (&optional workspace)
    (mevedel-system-render-prompt-file
     "prompts/system/memory-policy.md"
     `(("MEMORY_ROOTS" . ,(mevedel-system--memory-roots-description
                           workspace))
       ("MEMORY_CONTENT" . ,(mevedel-system--memory-content workspace)))))
  "Function returning the dynamic persistent memory prompt.")

(defun mevedel-system--memory-cache-key (context)
  "Return cache key for the memory prompt section in CONTEXT."
  (list
   :files (mapcar #'mevedel-system--file-cache-key
                  (mevedel-system--memory-files
                   (mevedel-system-context-workspace context)))
   :date (mevedel-system--current-date)))

(defun mevedel-system--working-directory
    (workspace working-directory &optional session)
  "Return effective WORKING-DIRECTORY for WORKSPACE."
  (file-name-as-directory
   (expand-file-name
    (or working-directory
        (and (mevedel-session-p session)
             (eq workspace (mevedel-session-workspace session))
             (mevedel-session-working-directory session))
        (mevedel-system--workspace-root workspace)))))

(defun mevedel-system--workspace-config-files (workspace &optional working-directory)
  "Return layered workspace instruction files for WORKSPACE.

Files are ordered from workspace root to WORKING-DIRECTORY.  Within a
single directory, AGENTS.local.md is loaded after AGENTS.md when
present."
  (when-let* ((workspace-root (and workspace (mevedel-workspace-root workspace))))
    (let* ((root (file-name-as-directory (expand-file-name workspace-root)))
           (cwd (mevedel-system--working-directory workspace working-directory))
           (cwd (if (file-in-directory-p cwd root) cwd root))
           (dirs nil)
           (cursor cwd))
      (while (and cursor (file-in-directory-p cursor root))
        (push cursor dirs)
        (setq cursor
             (unless (equal (file-name-as-directory cursor) root)
               (file-name-directory
                (directory-file-name cursor)))))
      (apply #'append
             (mapcar
              (lambda (dir)
                (let* ((agents-md (expand-file-name "AGENTS.md" dir))
                       (local-md (expand-file-name "AGENTS.local.md" dir)))
                  (delq nil
                        (list (and (file-readable-p agents-md)
                                   agents-md)
                              (and (file-readable-p local-md)
                                   local-md)))))
              dirs)))))

(defun mevedel-system--workspace-config-content (workspace &optional working-directory)
  "Return guidance for WORKSPACE and WORKING-DIRECTORY, or nil."
  (when-let* ((files (mevedel-system--workspace-config-files
                     workspace working-directory)))
    (string-join
     (mapcar
      (lambda (file)
        (concat "### " file "\n\n"
                (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string))))
      files)
     "\n\n")))

(defun mevedel-system--workspace-config-prompt (workspace &optional working-directory)
  "Return config prompt for WORKSPACE and WORKING-DIRECTORY, or nil."
  (when-let* ((content (mevedel-system--workspace-config-content
                       workspace working-directory)))
    (concat "## Workspace Configuration\n\n"
            "The following configuration files apply to the session, "
            "ordered from broadest to closest scope:\n\n"
            content)))

(defun mevedel-system--workspace-config-cache-key (context)
  "Return cache key for the workspace configuration section in CONTEXT."
  (or
   (mapcar #'mevedel-system--file-cache-key
           (mevedel-system--workspace-config-files
            (mevedel-system-context-workspace context)
            (mevedel-system-context-working-directory context)))
   (list :none
         (and (mevedel-system-context-workspace context)
              (mevedel-workspace-root
               (mevedel-system-context-workspace context)))
         (mevedel-system-context-working-directory context))))

(defun mevedel-system--environment-prompt (workspace &optional working-directory)
  "Return dynamic environment prompt for WORKSPACE and WORKING-DIRECTORY."
  (concat "## Environment\n\n"
          "Here is useful information about the environment you are running in:\n<env>\n"
          (mevedel--environment-info-string workspace working-directory)
          "\n</env>"))

(defun mevedel-system--session-matches-context-p (session context)
  "Return non-nil when SESSION matches CONTEXT's workspace and cwd."
  (let* ((workspace (mevedel-system-context-workspace context))
         (session-workspace (mevedel-session-workspace session))
         (context-root (and workspace
                            (file-name-as-directory
                             (expand-file-name
                              (mevedel-workspace-root workspace)))))
         (session-root (and session-workspace
                            (file-name-as-directory
                             (expand-file-name
                              (mevedel-workspace-root session-workspace)))))
         (context-dir (file-name-as-directory
                       (expand-file-name
                        (mevedel-system-context-working-directory context))))
         (session-dir (file-name-as-directory
                       (expand-file-name
                        (mevedel-session-working-directory session)))))
    (and (or (null workspace)
             (eq workspace session-workspace)
             (equal context-root session-root))
         (equal context-dir session-dir))))

(defun mevedel-system--context-session (context)
  "Return the mevedel session that should provide dynamic prompt context."
  (let ((session (mevedel-system-context-session context)))
    (and (mevedel-session-p session)
         (mevedel-system--session-matches-context-p session context)
         session)))

(defun mevedel-system--resource-metadata (context)
  "Return resource metadata for CONTEXT, or nil when unavailable."
  (when-let* ((session (mevedel-system--context-session context)))
    (condition-case nil
        (progn
          (require 'mevedel-agent-control nil t)
          (when (require 'mevedel-resource nil t)
            (mevedel-resource-completion-metadata
             (list :session session
                   :workspace (mevedel-system-context-workspace context)))))
      (error nil))))

(defun mevedel-system--resource-roster (context)
  "Return the compact resource roster usable in CONTEXT."
  (let* ((session (mevedel-system--context-session context))
         (metadata (mevedel-system--resource-metadata context))
         (retained (catch 'found
                     (dolist (entry (plist-get metadata :agents))
                       (when (plist-get entry :record)
                         (throw 'found t)))))
         (memory (catch 'found
                   (dolist (entry (plist-get metadata :memory-roots))
                     (when (file-directory-p
                            (plist-get (plist-get entry :root) :dir))
                       (throw 'found t)))))
         (lines nil))
    (when session
      (push (concat "- `local://` - shared durable space for the parent and "
                    "retained agents; use it for durable notes, findings, "
                    "contracts, and handoffs.")
            lines))
    (when session
      (push (concat "- `artifact://` - session-owned persisted tool and "
                    "execution output namespace; read available results as "
                    "evidence.")
            lines))
    (when (plist-get metadata :skills)
      (push "- `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]` - an enabled, discoverable skill package; read its files when needed." lines))
    (when retained
      (push "- `agent://` - retained agent results for this session." lines)
      (push "- `history://` - retained agent conversation history for this session." lines))
    (when memory
      (push "- `memory://` - existing configured persistent-memory roots." lines))
    (when (plist-get metadata :mcp-servers)
      (push "- `mcp://` - configured MCP servers and their resources." lines))
    (if lines
        (string-join (nreverse lines) "\n")
      "No resource address families are currently available in this request.")))

(defun mevedel-system--tool-orchestration-prompt (context)
  "Return tool orchestration guidance rendered for CONTEXT."
  (mevedel-system-render-prompt-file
   "prompts/system/tool-orchestration.md"
   `(("RESOURCE_ROSTER" .
      ,(mevedel-system--resource-roster context)))))

(defun mevedel-system--skills-prompt (context)
  "Return dynamic skills prompt text for CONTEXT, or nil."
  (when (require 'mevedel-skills-prompt nil t)
    (when-let* ((session (mevedel-system--context-session context)))
      (mevedel-skills-prompt-section
       session
       (mevedel-system-context-refresh-buffer context)))))

(defun mevedel-system--join-parts (&rest parts)
  "Join nonblank prompt PARTS with stable section spacing."
  (string-join
   (delq nil
         (mapcar (lambda (part)
                   (when (and (stringp part)
                              (not (string-blank-p part)))
                     (string-trim-right part)))
                 parts))
   "\n\n"))

(defun mevedel-system--make-context
    (workspace working-directory &optional session refresh-buffer)
  "Return normalized prompt context."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (working-directory
          (mevedel-system--working-directory
           workspace working-directory session)))
    (mevedel-system-context--create
     :workspace workspace
     :working-directory working-directory
     :session session
     :refresh-buffer refresh-buffer)))

(mevedel-define-prompt-component workspace-config
  :cache 'keyed
  :cache-key #'mevedel-system--workspace-config-cache-key
  :producer (lambda (context)
              (mevedel-system--workspace-config-prompt
               (mevedel-system-context-workspace context)
               (mevedel-system-context-working-directory context))))

(mevedel-define-prompt-component memory
  :cache 'keyed
  :cache-key #'mevedel-system--memory-cache-key
  :producer (lambda (context)
              (funcall mevedel-system--memory-prompt
                       (mevedel-system-context-workspace context))))

(mevedel-define-prompt-component environment
  :producer (lambda (context)
              (mevedel-system--environment-prompt
               (mevedel-system-context-workspace context)
               (mevedel-system-context-working-directory context))))

(mevedel-define-prompt-component skills
  :producer #'mevedel-system--skills-prompt)

(mevedel-define-prompt-component active-goal
  :producer (lambda (context)
              (when (fboundp 'mevedel-goal-active-context)
                (when-let* ((session (mevedel-system-context-session context)))
                  (mevedel-goal-active-context session)))))

(mevedel-define-prompt-component main-role
  :file "prompts/system/base.md")

(mevedel-define-prompt-component tutor-role
  :file "prompts/system/tutor.md")

(mevedel-define-prompt-component main-tone
  :file "prompts/tones/main.md")

(mevedel-define-prompt-component report-tone
  :file "prompts/tones/report.md")

(mevedel-define-prompt-component tutor-tone
  :file "prompts/tones/tutor.md")

(mevedel-define-prompt-component tool-orchestration
  :producer #'mevedel-system--tool-orchestration-prompt)

(mevedel-define-prompt-component bash-guardian-role
  :file "prompts/permissions/bash-guardian-system.md")

(mevedel-define-prompt-profile main
  :workspace-aware t
  :components '(main-role
                main-tone
                tool-orchestration
                workspace-config
                memory
                environment
                skills
                active-goal))

(mevedel-define-prompt-profile tutor
  :workspace-aware t
  :components '(tutor-role
                tutor-tone
                tool-orchestration
                workspace-config
                memory
                environment
                skills
                active-goal))

(mevedel-define-prompt-profile bash-guardian
  :workspace-aware t
  :components '(bash-guardian-role workspace-config environment))


;;
;;; System prompt builder

(defun mevedel-system--profile (profile)
  "Return PROFILE's plist, resolving a registered profile symbol."
  (cond
   ((symbolp profile)
    (or (alist-get profile mevedel-system--prompt-profiles)
        (error "Unknown prompt profile: %s" profile)))
   ((and (listp profile) (keywordp (car profile))) profile)
   (t (error "Malformed prompt profile: %S" profile))))

(defun mevedel-system--inline-component (entry)
  "Return a transient component for inline profile ENTRY."
  (unless (and (listp entry)
               (= (length entry) 3)
               (symbolp (car entry))
               (memq (cadr entry) '(:file :text)))
    (error "Malformed inline prompt component: %S" entry))
  (let ((value (caddr entry)))
    (unless (stringp value)
      (error "Inline prompt component value must be a string: %S" entry))
    (mevedel-system-prompt-component--create
     :name (car entry)
     :file (and (eq (cadr entry) :file) value)
     :text (and (eq (cadr entry) :text) value))))

(defun mevedel-system--profile-components (profile)
  "Validate PROFILE and return its ordered component objects."
  (let* ((profile (mevedel-system--profile profile))
         (workspace-aware (plist-get profile :workspace-aware))
         (entries (plist-get profile :components))
         names
         components)
    (unless (and (plist-member profile :workspace-aware)
                 (memq workspace-aware '(nil t))
                 (plist-member profile :components)
                 (listp entries))
      (error "Malformed prompt profile: %S" profile))
    (dolist (entry entries)
      (let* ((component
              (cond
               ((symbolp entry)
                (or (alist-get entry mevedel-system--prompt-components)
                    (error "Unknown prompt component: %s" entry)))
               (t (mevedel-system--inline-component entry))))
             (name (mevedel-system-prompt-component-name component)))
        (when (memq name names)
          (error "Duplicate prompt component: %s" name))
        (push name names)
        (push component components)))
    (when workspace-aware
      (dolist (required '(workspace-config environment))
        (unless (memq required entries)
          (error "Workspace-aware profile requires component: %s"
                 required))))
    (nreverse components)))

(cl-defun mevedel-system--profile-state
  (profile &key workspace working-directory session refresh-buffer)
  "Return rendered component state for PROFILE and request context."
  (let ((components (mevedel-system--profile-components profile))
        (context (mevedel-system--make-context
                  workspace working-directory session refresh-buffer)))
    (mapcar
     (lambda (component)
       (let* ((key (mevedel-system--component-cache-key component context))
              (cached
               (and key
                    (not (eq
                          (gethash key mevedel-system--prompt-component-cache
                                   mevedel-system--prompt-cache-miss)
                          mevedel-system--prompt-cache-miss))))
              (value (mevedel-system--render-component component context)))
         (list :component component :value value :cached cached)))
     components)))

(cl-defun mevedel-system-build-prompt
    (profile &key workspace working-directory session refresh-buffer)
  "Build the system prompt selected by PROFILE.

PROFILE is a registered profile symbol or an anonymous profile plist.
Components render in their listed order.  Blank values are omitted.
WORKSPACE, WORKING-DIRECTORY, SESSION, and REFRESH-BUFFER supply
request-time context to dynamic components."
  (apply
   #'mevedel-system--join-parts
   (mapcar (lambda (state) (plist-get state :value))
           (mevedel-system--profile-state
            profile
            :workspace workspace
            :working-directory working-directory
            :session session
            :refresh-buffer refresh-buffer))))

(defun mevedel-system--estimated-tokens (string)
  "Return a rough 4-chars-per-token estimate for STRING."
  (/ (+ (length string) 3) 4))

(cl-defun mevedel-system-prompt-component-report
    (profile &key workspace working-directory session refresh-buffer)
  "Return ordered audit data for PROFILE in the supplied context."
  (mapcar
   (lambda (state)
     (let* ((component (plist-get state :component))
            (value (plist-get state :value)))
       (list
        :name (mevedel-system-prompt-component-name component)
        :source (cond
                 ((mevedel-system-prompt-component-file component) 'file)
                 ((mevedel-system-prompt-component-producer component)
                  'producer)
                 (t 'text))
        :cache (mevedel-system-prompt-component-cache component)
        :cached (plist-get state :cached)
        :source-detail
        (cond
         ((mevedel-system-prompt-component-file component)
          (mevedel-system--prompt-path
           (mevedel-system-prompt-component-file component)))
         ((mevedel-system-prompt-component-producer component)
          (let ((producer
                 (mevedel-system-prompt-component-producer component)))
            (if (symbolp producer) (symbol-name producer) "dynamic producer")))
         (t "inline text"))
        :chars (if (stringp value) (length value) 0)
        :bytes (if (stringp value) (string-bytes value) 0)
        :estimated-tokens (if (stringp value)
                              (mevedel-system--estimated-tokens value)
                            0)
        :omitted (or (not (stringp value)) (string-blank-p value))
        :value value)))
   (mevedel-system--profile-state
    profile
    :workspace workspace
    :working-directory working-directory
    :session session
    :refresh-buffer refresh-buffer)))


;;
;;; Effective prompt inspector

(defvar-local mevedel-system--prompt-inspector-data-buffer nil
  "Live mevedel data buffer rendered by this prompt inspector.")

(defun mevedel-system--prompt-inspector-data-buffer ()
  "Return the active mevedel data buffer for prompt inspection."
  (let ((buffer
         (cond
          ((and (buffer-live-p mevedel-system--prompt-inspector-data-buffer)
                mevedel-system--prompt-inspector-data-buffer))
          ((and (boundp 'mevedel--data-buffer)
                (buffer-live-p mevedel--data-buffer))
           mevedel--data-buffer)
          ((bound-and-true-p mevedel--session) (current-buffer)))))
    (unless (and (buffer-live-p buffer)
                 (buffer-local-value 'mevedel--session buffer))
      (user-error "Not in a live mevedel session"))
    buffer))

(defun mevedel-system--prompt-profile-for-preset (preset)
  "Return the built-in prompt profile for PRESET, or nil."
  (pcase preset
    ('mevedel-tutor 'tutor)
    ((or 'mevedel-discuss 'mevedel-implement) 'main)
    (_ nil)))

(defun mevedel-system--effective-system-prompt ()
  "Return the current buffer's evaluated gptel system prompt."
  (let ((prompt (and (boundp 'gptel-system-prompt) gptel-system-prompt)))
    (condition-case err
        (cond ((functionp prompt) (funcall prompt))
              ((stringp prompt) prompt)
              (t ""))
      (error (format "[prompt unavailable: %s]" (error-message-string err))))))

(defun mevedel-system--tool-prompt-source (tool)
  "Return a human-readable prompt provenance label for gptel TOOL."
  (require 'mevedel-tool-registry)
  (if-let* ((native
             (cl-find tool (mevedel-tool-all)
                      :key #'mevedel-tool-gptel-tool :test #'eq)))
      (let ((source (mevedel-tool-prompt-source native)))
        (pcase (plist-get source :kind)
          ('file (format "prompt file %s" (plist-get source :path)))
          ('inline "inline native prompt")
          ('description "native short-description fallback")
          ('wrapped (format "wrapped gptel tool %s/%s"
                            (plist-get source :category)
                            (plist-get source :name)))
          (_ "native prompt provenance unavailable")))
    "external gptel tool"))

(defun mevedel-system--provider-tool-schema (backend tools)
  "Return provider-serialized tool schema text for BACKEND and TOOLS."
  (when (and backend tools)
    (condition-case nil
        (progn
          (require 'json)
          (json-encode (gptel--parse-tools backend tools)))
      (error nil))))

(defun mevedel-system--insert-effective-prompt-report (data-buffer)
  "Insert the effective prompt report for DATA-BUFFER."
  (let ((target (current-buffer)))
    (with-current-buffer data-buffer
      (let* ((session mevedel--session)
             (preset (mevedel-session-preset-name session))
             (profile (mevedel-system--prompt-profile-for-preset preset))
             (prompt (mevedel-system--effective-system-prompt))
             (tools (delete-dups
                     (append (and (boundp 'gptel-tools) gptel-tools)
                             (mevedel-session-deferred-pending session)
                             nil)))
             (schema (mevedel-system--provider-tool-schema
                      (and (boundp 'gptel-backend) gptel-backend) tools))
             (components
              (and profile
                   (mevedel-system-prompt-component-report
                    profile
                    :workspace (mevedel-session-workspace session)
                    :working-directory
                    (mevedel-session-working-directory session)
                    :session session
                    :refresh-buffer data-buffer)))
             (backend
              (if (and (boundp 'gptel-backend) gptel-backend)
                  (condition-case nil (gptel-backend-name gptel-backend)
                    (error (format "%S" gptel-backend)))
                "none"))
             (model
              (if (and (boundp 'gptel-model) gptel-model)
                  (condition-case nil (gptel--model-name gptel-model)
                    (error (format "%S" gptel-model)))
                "none")))
        (with-current-buffer target
          (insert "* Effective Prompt\n\n")
          (insert (format "Preset: %s\nProfile: %s\nBackend: %s\nModel: %s\n"
                          (or preset "none") (or profile "unknown") backend model))
          (insert (format "Reasoning effort: %s\nWorking directory: %s\n"
                          (or (mevedel-session-reasoning-effort session) "default")
                          (mevedel-session-working-directory session)))
          (insert (format "Permission mode: %s\nSandbox mode: %s\n"
                          (mevedel-session-permission-mode session)
                          (mevedel-session-sandbox-mode session)))
          (insert "External instructions: no separately exposed external instruction channel\n\n")
          (insert "* Ordered Components\n\n")
          (if components
              (dolist (component components)
                (insert (format "** %s\nSource: %s (%s)\nCache: %s; hit: %s\nSize: %d chars, %d bytes, ~%d tokens; omitted: %s\n\n%s\n\n"
                                (plist-get component :name)
                                (plist-get component :source-detail)
                                (plist-get component :source)
                                (or (plist-get component :cache) "none")
                                (if (plist-get component :cached) "yes" "no")
                                (plist-get component :chars)
                                (plist-get component :bytes)
                                (plist-get component :estimated-tokens)
                                (if (plist-get component :omitted) "yes" "no")
                                (or (plist-get component :value) ""))))
            (insert "Profile unknown; component breakdown unavailable.\n\n"))
          (insert "* Exact Final System Prompt\n\n" prompt "\n\n")
          (insert "* Effective Next-Request Tools\n\n")
          (if tools
              (dolist (tool tools)
                (let* ((description (or (gptel-tool-description tool) ""))
                       (schema-text (format "%S" (gptel-tool-args tool))))
                  (insert (format "** %s/%s\nProvenance: %s\nDescription: %d chars, %d bytes, ~%d tokens\nSchema: %d chars, %d bytes, ~%d tokens\n\n%s\n\nSchema definition:\n%s\n\n"
                                  (or (gptel-tool-category tool) "uncategorized")
                                  (gptel-tool-name tool)
                                  (mevedel-system--tool-prompt-source tool)
                                  (length description) (string-bytes description)
                                  (mevedel-system--estimated-tokens description)
                                  (length schema-text) (string-bytes schema-text)
                                  (mevedel-system--estimated-tokens schema-text)
                                  description schema-text))))
            (insert "No tools are effective for the next request.\n\n"))
          (insert "* Totals\n\n")
          (if schema
              (insert (format "Provider tool schema: %d chars, %d bytes, ~%d tokens\nEstimated total: %d chars, ~%d tokens\n"
                              (length schema) (string-bytes schema)
                              (mevedel-system--estimated-tokens schema)
                              (+ (length prompt) (length schema))
                              (mevedel-system--estimated-tokens
                               (concat prompt schema))))
            (insert (format "Provider tool schema: estimate unavailable\nEstimated total: %d system-prompt chars, ~%d tokens (tool schema excluded)\n"
                            (length prompt)
                            (mevedel-system--estimated-tokens prompt)))))))))

(defun mevedel-inspect-effective-prompt ()
  "Display the current session's effective prompt and tool report."
  (interactive)
  (let* ((data-buffer (mevedel-system--prompt-inspector-data-buffer))
         (buffer (get-buffer-create "*mevedel effective prompt*")))
    (with-current-buffer buffer
      (special-mode)
      (outline-minor-mode 1)
      (setq-local mevedel-system--prompt-inspector-data-buffer data-buffer)
      (setq-local revert-buffer-function
                  (lambda (_ignore-auto _noconfirm)
                    (mevedel-inspect-effective-prompt)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (mevedel-system--insert-effective-prompt-report data-buffer)
        (goto-char (point-min))))
    (display-buffer buffer)
    buffer))

(provide 'mevedel-system)
;;; mevedel-system.el ends here
