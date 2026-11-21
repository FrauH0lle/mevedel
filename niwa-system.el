;;; niwa-system.el -- System prompt -*- lexical-binding: t -*-

;;; Commentary:

;; System prompts are assembled from ordered profiles.  Profiles choose
;; reusable named components or inline file/text components; their list
;; order is the rendered order.  Dynamic components receive request-time
;; workspace, directory, session, and buffer context.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-report)
(require 'niwa-structs)

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel-request))

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
(defvar gptel-reasoning-effort)
(defvar gptel-system-prompt)
(defvar gptel-tools)

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-goal'
(declare-function niwa-goal-active-context "niwa-goal" (session))

;; `niwa-journal-index'
(declare-function niwa-journal-index-prompt "niwa-journal-index" (workspace))
(autoload 'niwa-journal-index-prompt "niwa-journal-index")

;; `niwa-resource'
(declare-function niwa-resource-completion-metadata
                  "niwa-resource" (context &optional scheme))
(autoload 'niwa-resource-completion-metadata "niwa-resource")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer request invocation))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-sandbox'
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(autoload 'niwa-sandbox-mode-effective "niwa-sandbox")

;; `niwa-skills-invoke'
(declare-function niwa-skills-request-model-policy "niwa-skills-invoke" ())
(autoload 'niwa-skills-request-model-policy "niwa-skills-invoke")

;; `niwa-skills-prompt'
(declare-function niwa-skills-prompt-section
                  "niwa-skills-prompt" (session &optional buffer))
(autoload 'niwa-skills-prompt-section "niwa-skills-prompt")

;; `niwa-structs'
(declare-function niwa-session-execution-target "niwa-structs" (cl-x) t)
(declare-function niwa-session-p "niwa-structs" (cl-x))
(declare-function niwa-session-permission-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-preset-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-reasoning-effort "niwa-structs" (cl-x) t)
(declare-function niwa-session-sandbox-mode "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-tool-registry'
(declare-function niwa-tool-for-gptel "niwa-tool-registry" (provider))
(declare-function niwa-tool-prompt-source "niwa-tool-registry" (cl-x) t)
(autoload 'niwa-tool-for-gptel "niwa-tool-registry")
(autoload 'niwa-tool-prompt-source "niwa-tool-registry")

;; `niwa-tools'
(declare-function niwa-tools-listing-section "niwa-tools" ())
(autoload 'niwa-tools-listing-section "niwa-tools")

;; `niwa-utilities'
(declare-function niwa--environment-info-string "niwa-utilities"
                  (&optional workspace working-directory execution-target))
(declare-function niwa-library-source-directory "niwa-utilities" (file))
(autoload 'niwa-library-source-directory "niwa-utilities")

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))

(defvar niwa-system--source-dir
  (niwa-library-source-directory (or load-file-name buffer-file-name))
  "Directory containing the niwa source files.")

(defcustom niwa-memory-dirs
  `(".niwa/memory/"
    ".agents/memory/"
    ,(file-name-concat niwa-user-dir "memory/")
    "~/.agents/memory/")
  "Directories scanned for persistent memory indexes.

Relative paths are resolved against the current workspace root.  The
default order is local niwa, local shared agents, global niwa,
then global shared agents."
  :type '(repeat directory)
  :group 'niwa)

(defun niwa-system--prompt-path (relative-path)
  "Return the absolute prompt path for RELATIVE-PATH."
  (expand-file-name relative-path niwa-system--source-dir))

(defun niwa-system-render-template (template replacements)
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

(defun niwa-system--read-prompt-file (relative-path)
  "Read RELATIVE-PATH from the niwa prompt directory."
  (let ((path (niwa-system--prompt-path relative-path)))
    (unless (file-readable-p path)
      (error "Prompt file not found: %s" path))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun niwa-system-render-prompt-file (relative-path &optional replacements)
  "Return prompt file RELATIVE-PATH with REPLACEMENTS applied."
  (niwa-system-render-template
   (niwa-system--read-prompt-file relative-path)
   replacements))

;;
;;; Prompt component and profile registries

(cl-defstruct (niwa-system-context
               (:constructor niwa-system-context--create))
  "Request-time context passed to prompt component producers."
  workspace
  working-directory
  session
  refresh-buffer)

(cl-defstruct (niwa-system-prompt-component
               (:constructor niwa-system-prompt-component--create))
  "A named system prompt component."
  name
  file
  text
  producer
  cache
  cache-key)

(defvar niwa-system--prompt-components nil
  "Alist mapping names to reusable prompt components.")

(defvar niwa-system--prompt-profiles nil
  "Alist mapping names to ordered prompt profile plists.")

(defvar niwa-system--prompt-component-cache (make-hash-table :test #'equal)
  "Memoized prompt component values keyed by component name and cache key.")

(defconst niwa-system--prompt-cache-miss
  (make-symbol "niwa-prompt-cache-miss")
  "Sentinel for missing prompt component cache entries.")

(defun niwa-system--register-prompt-component (name props)
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
    (niwa-system-clear-prompt-component-cache name)
    (setf (alist-get name niwa-system--prompt-components)
          (niwa-system-prompt-component--create
           :name name
           :file file
           :text text
           :producer producer
           :cache cache
           :cache-key cache-key)))
  name)

(defmacro niwa-define-prompt-component (name &rest props)
  "Define reusable prompt component NAME from PROPS.

Exactly one of `:file', `:text', or `:producer' is required.  Producers
receive a `niwa-system-context'.  `:cache' may be `global', `keyed',
or nil; keyed components also require a `:cache-key' function."
  `(niwa-system--register-prompt-component
    ',name
    (list ,@props)))

(defun niwa-system--register-prompt-profile (name props)
  "Register ordered prompt profile NAME from PROPS."
  (setf (alist-get name niwa-system--prompt-profiles) props)
  name)

(defmacro niwa-define-prompt-profile (name &rest props)
  "Define prompt profile NAME from PROPS.

`:workspace-aware' declares whether the profile requires explicit
`workspace-config' and `environment' components.  `:components' is the
ordered component list."
  `(niwa-system--register-prompt-profile
    ',name
    (list ,@props)))

(defun niwa-system-clear-prompt-component-cache (&optional name)
  "Clear memoized prompt component values.

When NAME is nil, clear all prompt component cache entries."
  (if (null name)
      (clrhash niwa-system--prompt-component-cache)
    (maphash
     (lambda (key _value)
       (when (eq (car-safe key) name)
         (remhash key niwa-system--prompt-component-cache)))
     niwa-system--prompt-component-cache)))

(defun niwa-system--component-cache-key (component context)
  "Return cache key for COMPONENT and CONTEXT, or nil when uncached."
  (let ((cache (niwa-system-prompt-component-cache component))
        (name (niwa-system-prompt-component-name component)))
    (pcase cache
      ('global (list name :global))
      ('keyed (list name
                    (funcall (niwa-system-prompt-component-cache-key
                              component)
                             context)))
      ('nil nil))))

(defun niwa-system--render-component-value (component context)
  "Return COMPONENT's uncached value for CONTEXT."
  (cond
   ((niwa-system-prompt-component-file component)
    (niwa-system-render-prompt-file
     (niwa-system-prompt-component-file component)))
   ((niwa-system-prompt-component-producer component)
    (funcall (niwa-system-prompt-component-producer component) context))
   (t (niwa-system-prompt-component-text component))))

(defun niwa-system--render-component (component context)
  "Return rendered COMPONENT for CONTEXT."
  (let ((key (niwa-system--component-cache-key component context)))
    (if (not key)
        (niwa-system--render-component-value component context)
      (let ((cached (gethash key niwa-system--prompt-component-cache
                             niwa-system--prompt-cache-miss)))
        (if (not (eq cached niwa-system--prompt-cache-miss))
            cached
          (let ((value (niwa-system--render-component-value
                        component context)))
            (puthash key value niwa-system--prompt-component-cache)
            value))))))


;;
;;; Dynamic section helpers

(defun niwa-system--workspace-root (workspace)
  "Return WORKSPACE's root, or the current workspace root."
  (niwa-workspace-root (or workspace (niwa-workspace))))

(defun niwa-system--file-cache-key (file)
  "Return metadata cache key for FILE.

One `file-attributes' answers existence, mtime, and size together.  Asking
`file-exists-p' and `file-truename' as well cost three target round trips
per file where one does, and this runs for every memory and workspace
configuration file on every send -- while the connection is the one the
request itself needs.

The key is keyed on the expanded name rather than the true name.  It exists
to notice that a file changed, which mtime and size already report; sharing
one entry between two paths that symlink to the same file was never worth a
round trip per file to discover."
  (let* ((expanded (expand-file-name file))
         (attrs (file-attributes expanded)))
    (if attrs
        (list :file expanded
              :mtime (file-attribute-modification-time attrs)
              :size (file-attribute-size attrs))
      (list :missing expanded))))

(defun niwa-system--memory-root (workspace dir)
  "Return metadata plist for memory DIR in WORKSPACE."
  (let* ((global (or (file-name-absolute-p dir)
                     (string-prefix-p "~" dir)))
         (path (directory-file-name
                (expand-file-name (substitute-in-file-name dir))))
         (family (cond
                  ((or (and global
                            (equal path (expand-file-name "memory" niwa-user-dir)))
                       (string-match-p "\\(?:\\`\\|/\\)\\.niwa/memory\\'" path))
                   "niwa")
                  ((string-match-p "\\(?:\\`\\|/\\)\\.agents/memory\\'" path)
                   "agents")))
         (label (string-join
                 (delq nil
                       (list (if global "Global" "Local")
                             family
                             "memory"))
                 " "))
         (alias (and family
                     (concat (if global "global" "local") "-" family)))
         (root (file-name-as-directory
                (expand-file-name
                 dir
                 (and (not global)
                      (niwa-system--workspace-root workspace)))))
         (file (file-name-concat root "MEMORY.md")))
    (list :dir root
          :file file
          :label label
          :alias alias)))

(defun niwa-system--memory-roots (workspace)
  "Return configured memory root metadata for WORKSPACE."
  (mapcar (lambda (dir) (niwa-system--memory-root workspace dir))
          niwa-memory-dirs))

(defun niwa-system--memory-files (workspace)
  "Return configured memory index files for WORKSPACE."
  (mapcar (lambda (root) (plist-get root :file))
          (niwa-system--memory-roots workspace)))

(defun niwa-system--memory-updated-header (memory-file)
  "Return last-updated metadata for MEMORY-FILE."
  (let* ((attrs (file-attributes memory-file))
         (mtime (file-attribute-modification-time attrs)))
    (format "<!-- Last updated: %s -->"
            (format-time-string "%Y-%m-%d" mtime))))

(defun niwa-system--read-memory-index (memory-file)
  "Return the first 200 lines from MEMORY-FILE."
  (with-temp-buffer
    (insert-file-contents memory-file)
    (forward-line 200)
    (string-remove-suffix
     "\n" (buffer-substring-no-properties (point-min) (point)))))

(defun niwa-system--memory-root-content (root)
  "Return prompt content for memory ROOT, or nil when its index is absent."
  (let ((file (plist-get root :file)))
    (when (file-exists-p file)
      (string-join
       (list (format "### %s" (plist-get root :label))
             (format "Directory: %s" (plist-get root :dir))
             (niwa-system--memory-updated-header file)
             (niwa-system--read-memory-index file))
       "\n"))))

(defun niwa-system--memory-roots-description (workspace)
  "Return configured memory roots for WORKSPACE as prompt text."
  (mapconcat
   (lambda (root)
     (format "- %s: %s"
             (plist-get root :label)
             (plist-get root :dir)))
   (niwa-system--memory-roots workspace)
   "\n"))

(defun niwa-system--memory-content (workspace)
  "Return merged WORKSPACE memory index content, or an empty notice."
  (let ((sections (delq nil
                        (mapcar #'niwa-system--memory-root-content
                                (niwa-system--memory-roots workspace)))))
    (if sections
        (string-join sections "\n\n")
      "Your memory indexes are currently empty.")))

(defun niwa-system--memory-context-prompt (workspace)
  "Return configured memory roots and index contents for WORKSPACE."
  (niwa-system-render-prompt-file
   "prompts/system/memory-context.md"
   `(("MEMORY_ROOTS" . ,(niwa-system--memory-roots-description workspace))
     ("MEMORY_CONTENT" . ,(niwa-system--memory-content workspace)))))

(defun niwa-system--memory-cache-key (context)
  "Return cache key for the memory prompt section in CONTEXT."
  (list
   :files (mapcar #'niwa-system--file-cache-key
                  (niwa-system--memory-files
                   (niwa-system-context-workspace context)))
   :template (niwa-system--file-cache-key
              (file-name-concat niwa-system--source-dir
                                "prompts/system/memory-context.md"))))

(defun niwa-system--working-directory
    (workspace working-directory &optional session)
  "Return effective WORKING-DIRECTORY for WORKSPACE."
  (file-name-as-directory
   (expand-file-name
    (or working-directory
        (and (niwa-session-p session)
             (eq workspace (niwa-session-workspace session))
             (niwa-session-working-directory session))
        (niwa-system--workspace-root workspace)))))

(defun niwa-system-workspace-config-files (workspace &optional working-directory)
  "Return layered workspace instruction files for WORKSPACE.

Files are ordered from workspace root to WORKING-DIRECTORY, or from the
validated agent checkout's instruction root for an external worktree.  Within a
single directory, AGENTS.local.md is loaded after AGENTS.md when
present."
  (when-let* ((workspace-root (and workspace (niwa-workspace-root workspace))))
    (let* ((root (file-name-as-directory (expand-file-name workspace-root)))
           (cwd (niwa-system--working-directory workspace working-directory))
           (root (if (and niwa--conversation-instruction-root
                          (string-prefix-p niwa--conversation-instruction-root cwd))
                     niwa--conversation-instruction-root
                   root))
           ;; Containment by name, not by `file-in-directory-p': that
           ;; resolves both arguments with `file-truename', which on a remote
           ;; workspace is a round trip per directory level, once per send.
           ;; Both names are already expanded and slash-terminated here.
           (cwd (if (string-prefix-p root (file-name-as-directory cwd))
                    cwd
                  root))
           (dirs nil)
           (cursor cwd))
      (while (and cursor
                  (string-prefix-p root (file-name-as-directory cursor)))
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

(defun niwa-system--workspace-config-content (workspace &optional working-directory)
  "Return guidance for WORKSPACE and WORKING-DIRECTORY, or nil."
  (when-let* ((files (niwa-system-workspace-config-files
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

(defun niwa-system--workspace-config-prompt (workspace &optional working-directory)
  "Return config prompt for WORKSPACE and WORKING-DIRECTORY, or nil."
  (when-let* ((content (niwa-system--workspace-config-content
                       workspace working-directory)))
    (concat "## Workspace Configuration\n\n"
            "The following configuration files apply to the session, "
            "ordered from broadest to closest scope:\n\n"
            content)))

(defun niwa-system--workspace-config-cache-key (context)
  "Return cache key for the workspace configuration section in CONTEXT."
  (or
   (mapcar #'niwa-system--file-cache-key
           (niwa-system-workspace-config-files
            (niwa-system-context-workspace context)
            (niwa-system-context-working-directory context)))
   (list :none
         (and (niwa-system-context-workspace context)
              (niwa-workspace-root
               (niwa-system-context-workspace context)))
         (niwa-system-context-working-directory context))))

(defun niwa-system--environment-prompt
    (workspace &optional working-directory execution-target)
  "Return prompt for WORKSPACE, WORKING-DIRECTORY, and EXECUTION-TARGET."
  (concat "## Environment\n\n"
          "Here is useful information about the environment you are running in:\n<env>\n"
          (niwa--environment-info-string
           workspace working-directory execution-target)
          "\n</env>"))

(defun niwa-system--session-matches-context-p (session context)
  "Return non-nil when SESSION matches CONTEXT's workspace and cwd."
  (let* ((workspace (niwa-system-context-workspace context))
         (session-workspace (niwa-session-workspace session))
         (context-root (and workspace
                            (file-name-as-directory
                             (expand-file-name
                              (niwa-workspace-root workspace)))))
         (session-root (and session-workspace
                            (file-name-as-directory
                             (expand-file-name
                              (niwa-workspace-root session-workspace)))))
         (context-dir (file-name-as-directory
                       (expand-file-name
                        (niwa-system-context-working-directory context))))
         (session-dir (file-name-as-directory
                       (expand-file-name
                        (niwa-conversation-working-directory
                         session (niwa-system-context-refresh-buffer context))))))
    (and (or (null workspace)
             (eq workspace session-workspace)
             (equal context-root session-root))
         (equal context-dir session-dir))))

(defun niwa-system--context-session (context)
  "Return the niwa session that should provide dynamic prompt context."
  (let ((session (niwa-system-context-session context)))
    (and (niwa-session-p session)
         (niwa-system--session-matches-context-p session context)
         session)))

(defun niwa-system--resource-metadata (context)
  "Return resource metadata for CONTEXT, or nil when unavailable."
  (when-let* ((session (niwa-system--context-session context)))
    (condition-case nil
        (niwa-resource-completion-metadata
         (list :session session
               :workspace (niwa-system-context-workspace context)))
      (error nil))))

(defun niwa-system--resource-roster (context)
  "Return the compact resource roster usable in CONTEXT."
  (let* ((session (niwa-system--context-session context))
         (metadata (niwa-system--resource-metadata context))
         (retained (seq-some (lambda (entry) (plist-get entry :record))
                             (plist-get metadata :agents)))
         (memory (plist-get metadata :memory-roots))
         (lines nil))
    (push (concat "- `niwa://` - packaged documentation for Niwa; use "
                  "Read, Glob, or Grep. This family is available without a "
                  "session and exposes no package source code.")
          lines)
    (when session
      (push (concat "- `work://` - working files; use Read, Glob, Grep, and "
                    "ApplyPatch. `work://plans/` is session-owned. "
                    "`work://shared/` shares working notes and handoffs between the "
                    "main agent, subagents, and later sessions in the same workspace, "
                    "including its niwa-created worktree sessions. Note contents "
                    "are not automatically added to conversations: discover and read "
                    "them with Read/Glob/Grep, and give the receiving agent the note's "
                    "address when handing off work. Use this address rather than a "
                    "cwd-relative .niwa/shared/ path. Search first "
                    "and update relevant existing files. Use Markdown, one topic per "
                    "file; choose filenames and folders as needed. No note header "
                    "or lifecycle labels are required. Keep actionable work in "
                    "the tracker/backlog, not only in temporary notes. "
                    "Notes and attachments reach age-readiness after 14 days "
                    "without modification by default; reading does not reset age. "
                    "Directory Read shows generated age-readiness rows, not mutation "
                    "authority; capture/application safety checks remain pending. "
                    "Shared files are workspace-owned, machine-local and "
                    "temporary: memory review extracts lasting lessons from quiet "
                    "files, then removes them. Writes require ordinary workspace "
                    "edit authority.")
            lines))
    (when session
      (push (concat "- `artifact://` - session-owned persisted tool and "
                    "execution output namespace; read available results as "
                    "evidence.")
            lines))
    (when (plist-get metadata :skills)
      (push (concat "- `skill://` - enabled, discoverable skill packages. "
                    "Bare listings show readable aliases and exact hashed "
                    "addresses. Aliases are `skill://local-niwa/SKILL`, "
                    "`skill://local-agents/SKILL`, "
                    "`skill://global-niwa/SKILL`, "
                    "`skill://global-agents/SKILL`, "
                    "`skill://bundled/SKILL`, `skill://managed/SKILL`, and "
                    "`skill://plugin/PLUGIN/SKILL`, each with optional "
                    "descendants; each resolves to an exact "
                    "`skill://NAME@SOURCE-KEY` locator.")
            lines))
    (when retained
      (push "- `agent://` - retained agent results for this session." lines))
    (when (plist-get metadata :notes-p)
      (push (concat "- `notes://` - read-only marked-code notes in this session's workspace. "
                    "Use Read to list display numbers and UUID addresses; "
                    "Read, Glob and Grep accept returned notes://UUID.md addresses. "
                    "These are distinct from working files under work://shared/; "
                    "reading a note does not send it or start a conversation.")
            lines))
    (when (or (plist-get metadata :saved-history-p)
              (seq-some (lambda (entry) (plist-get entry :history-p))
                        (plist-get metadata :agents)))
      (push (concat "- `history://` - conversation history. "
                    (when (seq-some (lambda (entry) (plist-get entry :history-p))
                                    (plist-get metadata :agents))
                      (concat
                       "Read/Grep `history://root` for the main conversation or `history://root/PATH` for a retained agent conversation. "
                       "In scoped conversations, retrieve relevant earlier decisions here when the request needs broader context. "))
                    (when (plist-get metadata :saved-history-p)
                      "Read/Glob/Grep `history://saved` for saved workspace conversations; narrow by the session/segment paths returned there."))
            lines))
    (when memory
      (push (concat "- `memory://` - configured persistent-memory "
                    "roots. `memory://root` reads the ordered union; a topic "
                    "is `memory://ROOT-KEY/RELATIVE-PATH`, where ROOT-KEY is "
                    "the readable key listings and search results disclose, "
                    "such as `local-niwa` or `global-agents`. Use ApplyPatch for explicit "
                    "root descendants with normal edit permissions; `memory://root` "
                    "and root-only addresses are not writable.")
            lines))
    (when (plist-get metadata :journal)
      (push (concat "- `memory://journal/` - published workspace digests; use Read, "
                    "Glob, or Grep. These read-only records are dated evidence, "
                    "not current instructions or permission to resume work.")
            lines))
    (when (plist-get metadata :shared-items)
      (push (concat "- `shared://` - the project's shared whiteboards and documents, marking those attached to this session. "
                    "Read `shared://` to list them and `shared://ID` for one line per "
                    "element or block, each with the hash SharedEdit names it by; Grep "
                    "searches them. `shared://library` lists whiteboard element libraries.")
            lines))
    (when (plist-get metadata :mcp-servers)
      (push "- `mcp://` - configured MCP servers and their resources." lines))
    (string-join (nreverse lines) "\n")))

(defun niwa-system--tool-orchestration-prompt (context)
  "Return tool orchestration guidance rendered for CONTEXT."
  (ignore context)
  (niwa-system-render-prompt-file "prompts/system/tool-orchestration.md"))

(defun niwa-system--skills-prompt (context)
  "Return dynamic skills prompt text for CONTEXT, or nil."
  (when-let* ((session (niwa-system--context-session context)))
    (niwa-skills-prompt-section
     session
     (niwa-system-context-refresh-buffer context))))

(defun niwa-system--tools-prompt (context)
  "Return the specialist tool listing for CONTEXT's conversation, or nil.
The refresh buffer identifies the conversation, so an agent sees its own
catalog rather than its root session's."
  (when-let* (((niwa-system--context-session context))
              (buffer (niwa-system-context-refresh-buffer context))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (niwa-tools-listing-section))))

(defun niwa-system--join-parts (&rest parts)
  "Join nonblank prompt PARTS with stable section spacing."
  (string-join
   (delq nil
         (mapcar (lambda (part)
                   (when (and (stringp part)
                              (not (string-blank-p part)))
                     (string-trim-right part)))
                 parts))
   "\n\n"))

(defun niwa-system--make-context
    (workspace working-directory &optional session refresh-buffer)
  "Return normalized prompt context."
  (let* ((workspace (or workspace (niwa-workspace)))
         (working-directory
          (niwa-system--working-directory
           workspace working-directory session)))
    (niwa-system-context--create
     :workspace workspace
     :working-directory working-directory
     :session session
     :refresh-buffer refresh-buffer)))

(niwa-define-prompt-component workspace-config
  :cache 'keyed
  :cache-key #'niwa-system--workspace-config-cache-key
  :producer (lambda (context)
              (niwa-system--workspace-config-prompt
               (niwa-system-context-workspace context)
               (niwa-system-context-working-directory context))))

(niwa-define-prompt-component memory-policy
  :file "prompts/system/memory-policy.md")

(niwa-define-prompt-component memory-save-policy
  :file "prompts/system/memory-save-policy.md")

(niwa-define-prompt-component memory
  :cache 'keyed
  :cache-key #'niwa-system--memory-cache-key
  :producer (lambda (context)
              (niwa-system--memory-context-prompt
               (niwa-system-context-workspace context))))

(niwa-define-prompt-component journal
  :producer (lambda (context)
              (niwa-journal-index-prompt
               (niwa-system-context-workspace context))))

(niwa-define-prompt-component environment
  :producer (lambda (context)
              (let ((session (niwa-system--context-session context)))
                (niwa-system--environment-prompt
                 (niwa-system-context-workspace context)
                 (niwa-system-context-working-directory context)
                 (and session
                      (niwa-session-execution-target session))))))

(niwa-define-prompt-component skills
  :producer #'niwa-system--skills-prompt)

(niwa-define-prompt-component tools
  :producer #'niwa-system--tools-prompt)

(niwa-define-prompt-component active-goal
  :producer (lambda (context)
              (when (fboundp 'niwa-goal-active-context)
                (when-let* ((session (niwa-system--context-session context)))
                  (niwa-goal-active-context session)))))

(niwa-define-prompt-component goal-policy
  :producer (lambda (context)
              (when-let* ((session (niwa-system--context-session context))
                          ((niwa-goal-active-context session)))
                (niwa-system-render-prompt-file "prompts/goals/policy.md"))))

(defconst niwa-system-retained-components
  '(workspace-config memory journal environment tools skills active-goal goal-policy resources)
  "Named observations delivered in conversation history for retained requests.")

(defconst niwa-system-note-context-components
  '(workspace-config memory environment tools skills resources)
  "Automatic observations included in independent note conversations.
Journal, active-goal and goal-policy belong to other conversation context.")

(defun niwa-system-context-components (&optional session)
  "Return automatic context components for SESSION, defaulting to this session."
  (if (and (setq session (or session niwa--session))
           (niwa-session-note-conversation session))
      niwa-system-note-context-components
    niwa-system-retained-components))

(niwa-define-prompt-component resources
  :producer #'niwa-system--resource-roster)

(niwa-define-prompt-component skill-policy
  :file "prompts/system/skill-policy.md")

(niwa-define-prompt-component main-role
  :file "prompts/system/base.md")

(niwa-define-prompt-component task-policy
  :file "prompts/system/task-policy.md")

(niwa-define-prompt-component main-tone
  :file "prompts/tones/main.md")

(niwa-define-prompt-component report-tone
  :file "prompts/tones/report.md")

(niwa-define-prompt-component tool-orchestration
  :producer #'niwa-system--tool-orchestration-prompt)

(niwa-define-prompt-component permission-review-role
  :file "prompts/permissions/approval-review-system.md")

(niwa-define-prompt-component buddy-role
  :file "prompts/system/buddy.md")

(niwa-define-prompt-component buddy-guide-role
  :file "prompts/system/buddy-guide.md")

(niwa-define-prompt-profile main
  :workspace-aware t
  :components '(main-role
                task-policy
                main-tone
                memory-policy
                memory-save-policy
                tool-orchestration
                resources
                skill-policy
                workspace-config
                memory
                journal
                environment
                tools
                skills
                active-goal
                goal-policy))

(niwa-define-prompt-profile permission-review
  :workspace-aware nil
  :components '(permission-review-role))

(niwa-define-prompt-profile buddy
  :workspace-aware t
  :components '(buddy-role memory-policy workspace-config memory environment))

(niwa-define-prompt-profile buddy-guide
  :workspace-aware t
  :components '(buddy-guide-role memory-policy workspace-config memory environment))


;;
;;; System prompt builder

(defun niwa-system--profile (profile)
  "Return PROFILE's plist, resolving a registered profile symbol."
  (cond
   ((symbolp profile)
    (or (alist-get profile niwa-system--prompt-profiles)
        (error "Unknown prompt profile: %s" profile)))
   ((and (listp profile) (keywordp (car profile))) profile)
   (t (error "Malformed prompt profile: %S" profile))))

(defun niwa-system--inline-component (entry)
  "Return a transient component for inline profile ENTRY."
  (unless (and (listp entry)
               (= (length entry) 3)
               (symbolp (car entry))
               (memq (cadr entry) '(:file :text)))
    (error "Malformed inline prompt component: %S" entry))
  (let ((value (caddr entry)))
    (unless (stringp value)
      (error "Inline prompt component value must be a string: %S" entry))
    (niwa-system-prompt-component--create
     :name (car entry)
     :file (and (eq (cadr entry) :file) value)
     :text (and (eq (cadr entry) :text) value))))

(defun niwa-system--profile-components (profile)
  "Validate PROFILE and return its ordered component objects."
  (let* ((profile (niwa-system--profile profile))
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
                (or (alist-get entry niwa-system--prompt-components)
                    (error "Unknown prompt component: %s" entry)))
               (t (niwa-system--inline-component entry))))
             (name (niwa-system-prompt-component-name component)))
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

(cl-defun niwa-system--profile-state
  (profile &key workspace working-directory session refresh-buffer retained)
  "Return rendered component state for PROFILE and request context."
  (let ((components (niwa-system--profile-components profile))
        (context (niwa-system--make-context
                  workspace working-directory session refresh-buffer)))
    (mapcar
     (lambda (component)
       (let* ((key (niwa-system--component-cache-key component context))
              (cached
               (and key
                    (not (eq
                          (gethash key niwa-system--prompt-component-cache
                                   niwa-system--prompt-cache-miss)
                          niwa-system--prompt-cache-miss))))
              (value (niwa-system--render-component component context)))
         (list :component component :value value :cached cached)))
     (if retained
         (cl-remove-if
          (lambda (component)
            (let ((name (niwa-system-prompt-component-name component)))
              (and (memq name niwa-system-retained-components)
                   (eq component (alist-get name niwa-system--prompt-components)))))
          components)
       components))))

(cl-defun niwa-system-build-prompt
    (profile &key workspace working-directory session refresh-buffer retained)
  "Build the system prompt selected by PROFILE.

PROFILE is a registered profile symbol or an anonymous profile plist.
Components render in their listed order.  Blank values are omitted.
WORKSPACE, WORKING-DIRECTORY, SESSION, and REFRESH-BUFFER supply
request-time context to dynamic components.  RETAINED moves changing components
to conversation delivery, leaving only the stable system contract."
  (apply
   #'niwa-system--join-parts
   (mapcar (lambda (state) (plist-get state :value))
           (niwa-system--profile-state
            profile
            :workspace workspace
            :working-directory working-directory
            :session session
            :refresh-buffer refresh-buffer
            :retained retained))))

(defun niwa-system--estimated-tokens (string)
  "Return a rough 4-chars-per-token estimate for STRING."
  (/ (+ (length string) 3) 4))

(cl-defun niwa-system-prompt-component-report
    (profile &key workspace working-directory session refresh-buffer)
  "Return ordered audit data for PROFILE in the supplied context."
  (mapcar
   (lambda (state)
     (let* ((component (plist-get state :component))
            (value (plist-get state :value)))
       (list
        :name (niwa-system-prompt-component-name component)
        :source (cond
                 ((niwa-system-prompt-component-file component) 'file)
                 ((niwa-system-prompt-component-producer component)
                  'producer)
                 (t 'text))
        :cache (niwa-system-prompt-component-cache component)
        :cached (plist-get state :cached)
        :source-detail
        (cond
         ((niwa-system-prompt-component-file component)
          (niwa-system--prompt-path
           (niwa-system-prompt-component-file component)))
         ((niwa-system-prompt-component-producer component)
          (let ((producer
                 (niwa-system-prompt-component-producer component)))
            (if (symbolp producer) (symbol-name producer) "dynamic producer")))
         (t "inline text"))
        :chars (if (stringp value) (length value) 0)
        :bytes (if (stringp value) (string-bytes value) 0)
        :estimated-tokens (if (stringp value)
                              (niwa-system--estimated-tokens value)
                            0)
        :omitted (or (not (stringp value)) (string-blank-p value))
        :value value)))
   (niwa-system--profile-state
    profile
    :workspace workspace
    :working-directory working-directory
    :session session
    :refresh-buffer refresh-buffer)))


;;
;;; Effective prompt inspector

(defvar-local niwa-system--prompt-inspector-data-buffer nil
  "Live niwa data buffer rendered by this prompt inspector.")

(defun niwa-system--prompt-inspector-data-buffer ()
  "Return the active niwa data buffer for prompt inspection."
  (let ((buffer
         (cond
          ((and (buffer-live-p niwa-system--prompt-inspector-data-buffer)
                niwa-system--prompt-inspector-data-buffer))
          ((and (boundp 'niwa--data-buffer)
                (buffer-live-p niwa--data-buffer))
           niwa--data-buffer)
          ((bound-and-true-p niwa--session) (current-buffer)))))
    (unless (and (buffer-live-p buffer)
                 (buffer-local-value 'niwa--session buffer))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not in a live niwa session"))
    buffer))

(defun niwa-system--prompt-profile-for-preset (preset)
  "Return the built-in prompt profile for PRESET, or nil."
  (pcase preset
    ('niwa-implement 'main)
    (_ nil)))

(defun niwa-system--effective-system-prompt ()
  "Return the current buffer's evaluated gptel system prompt."
  (let ((prompt (and (boundp 'gptel-system-prompt) gptel-system-prompt)))
    (condition-case err
        (cond ((functionp prompt) (funcall prompt))
              ((stringp prompt) prompt)
              (t ""))
      (error (format "[prompt unavailable: %s]" (error-message-string err))))))

(defun niwa-system--tool-prompt-source (tool)
  "Return a human-readable prompt provenance label for gptel TOOL."
  (if-let* ((native (niwa-tool-for-gptel tool)))
      (let ((source (niwa-tool-prompt-source native)))
        (pcase (plist-get source :kind)
          ('file (format "prompt file %s" (plist-get source :path)))
          ('inline "inline native prompt")
          ('description "native short-description fallback")
          ('wrapped (format "wrapped gptel tool %s/%s"
                            (plist-get source :category)
                            (plist-get source :name)))
          (_ "native prompt provenance unavailable")))
    "external gptel tool"))

(defun niwa-system--provider-tool-schema (backend tools)
  "Return provider-serialized tool schema text for BACKEND and TOOLS."
  (when (and backend tools)
    (condition-case nil
        (gptel--json-encode (gptel--parse-tools backend tools))
      (error nil))))

(defun niwa-system--effective-prompt-report (data-buffer)
  "Return the exact effective prompt report for DATA-BUFFER."
  (with-current-buffer data-buffer
    (let* ((session niwa--session)
           (policy (niwa-skills-request-model-policy))
           (gptel-backend (plist-get policy :backend))
           (gptel-model (plist-get policy :model))
           (effort (plist-get policy :effort))
           (gptel-reasoning-effort effort)
           (permission-mode (niwa-permission-mode-effective session data-buffer))
           (sandbox-mode (niwa-sandbox-mode-effective session permission-mode))
           (preset (niwa-session-preset-name session))
           (profile (niwa-system--prompt-profile-for-preset preset))
           (prompt (niwa-system--effective-system-prompt))
           (tools (and (boundp 'gptel-tools) gptel-tools))
           (schema (niwa-system--provider-tool-schema
                    (and (boundp 'gptel-backend) gptel-backend) tools))
           (components
            (and profile
                 (niwa-system-prompt-component-report
                  profile
                  :workspace (niwa-session-workspace session)
                  :working-directory
                  (niwa-session-working-directory session)
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
      (list :title "Effective Prompt" :identity session
            :diagnostic-scope (niwa-diagnostic-scope session)
            :refresh (lambda () (niwa-system--effective-prompt-report data-buffer))
            :validate (lambda ()
                        (unless (buffer-live-p data-buffer)
                          (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Prompt source buffer is no longer live")))
            :sections
            (list
             (list :id 'context :title "Context" :body (with-temp-buffer
							 (insert (format "Preset: %s\nProfile: %s\nBackend: %s\nModel: %s\n"
									 (or preset "none") (or profile "unknown") backend model))
							 (insert (format "Reasoning effort: %s\nWorking directory: %s\n"
                                         (or effort "default")
									 (niwa-session-working-directory session)))
							 (insert (format "Permission mode: %s\nEffective sandbox: %s\nSandbox preference: %s\n"
									 permission-mode
                                                                 sandbox-mode
									 (niwa-session-sandbox-mode session)))
							 (insert "External instructions: no separately exposed external instruction channel\n\n")
							 (buffer-string)))
             (list :id 'components :title "Ordered Components" :folded (and components t)
                   :body (with-temp-buffer
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
			   (buffer-string)))
             (list :id 'prompt :title "Exact Final System Prompt" :body prompt :mode 'markdown-mode)
             (list :id 'tools :title "Effective Next-Request Tools" :folded (and tools t)
                   :body (with-temp-buffer
			   (if tools
			       (dolist (tool tools)
				 (let* ((description (or (gptel-tool-description tool) ""))
					(schema-text (format "%S" (gptel-tool-args tool))))
				   (insert (format "** %s/%s\nProvenance: %s\nDescription: %d chars, %d bytes, ~%d tokens\nSchema: %d chars, %d bytes, ~%d tokens\n\n%s\n\nSchema definition:\n%s\n\n"
						   (or (gptel-tool-category tool) "uncategorized")
						   (gptel-tool-name tool)
						   (niwa-system--tool-prompt-source tool)
						   (length description) (string-bytes description)
						   (niwa-system--estimated-tokens description)
						   (length schema-text) (string-bytes schema-text)
						   (niwa-system--estimated-tokens schema-text)
						   description schema-text))))
			     (insert "No tools are effective for the next request.\n\n"))
			   (buffer-string)))
             (list :id 'totals :title "Totals" :body (with-temp-buffer
						       (if schema
							   (insert (format "Provider tool schema: %d chars, %d bytes, ~%d tokens\nEstimated total: %d chars, ~%d tokens\n"
									   (length schema) (string-bytes schema)
									   (niwa-system--estimated-tokens schema)
									   (+ (length prompt) (length schema))
									   (niwa-system--estimated-tokens
									    (concat prompt schema))))
							 (insert (format "Provider tool schema: estimate unavailable\nEstimated total: %d system-prompt chars, ~%d tokens (tool schema excluded)\n"
									 (length prompt)
									 (niwa-system--estimated-tokens prompt))))
						       (buffer-string))))))))

(defun niwa-inspect-effective-prompt ()
  "Display the current session's effective prompt and tool report."
  (interactive)
  (let* ((data-buffer (niwa-system--prompt-inspector-data-buffer))
         (buffer (niwa-report-show "*niwa effective prompt*"
                                      (niwa-system--effective-prompt-report data-buffer))))
    (with-current-buffer buffer
      (setq-local niwa-system--prompt-inspector-data-buffer data-buffer
                  revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                           (niwa-report-refresh))))
    buffer))

(provide 'niwa-system)
;;; niwa-system.el ends here
