;;; mevedel-system.el -- System prompt -*- lexical-binding: t -*-

;;; Commentary:

;; System prompt assembly.  The full prompt is composed from several
;; string constants (tone, task protocol, tool usage guidance,
;; delegation rules) plus dynamic sections built at request time:
;; persistent memory (from `.mevedel/memory/MEMORY.md'), environment
;; info, and the workspace-level AGENTS.md / CLAUDE.md if present.
;; A separate tutor-base-prompt drives the tutoring preset.
;;
;; Prompt sections are registered as named producers so static, keyed,
;; and per-request parts have a single assembly path.  This keeps the
;; public system prompt API stable while making the prompt shape auditable
;; and ready for provider-specific multi-part messages.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)

;; `gptel-agent'
(declare-function gptel-agent-read-file "ext:gptel-agent" (agent-file &optional templates metadata-only))

;; `mevedel-utilities'
(declare-function mevedel--environment-info-string "mevedel-utilities"
                  (&optional workspace working-directory))

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-workspace'
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

(defvar mevedel-system--source-dir
  (let* ((lib (or load-file-name buffer-file-name))
         (el-file (if (and lib (string-suffix-p ".elc" lib))
                      (substring lib 0 -1)
                    lib)))
    (file-name-directory (file-truename el-file)))
  "Directory containing the mevedel source files.")

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

(defun mevedel-system-render-agent-prompt-file (relative-path &optional replacements)
  "Return agent prompt file RELATIVE-PATH with REPLACEMENTS applied.

This uses gptel-agent's Markdown/Org parser so mevedel agent files share
the same template behavior as native gptel-agent definitions."
  (let ((path (mevedel-system--prompt-path relative-path)))
    (unless (file-readable-p path)
      (error "Prompt file not found: %s" path))
    (or
     (when (and (member (file-name-extension path) '("md" "org"))
                (require 'gptel-agent nil t))
       (when-let* ((entry (gptel-agent-read-file path replacements))
                   (plist (cdr entry)))
         (plist-get plist :system)))
     (mevedel-system-render-template
      (mevedel-system--read-prompt-file relative-path)
      replacements))))

(defconst mevedel-system--tone-prompt
  (mevedel-system-render-prompt-file "prompts/system/tone.md")
  "Static tone prompt shared by main, tutor, and agent prompts.")

(defconst mevedel-system--base-prompt
  (mevedel-system-render-prompt-file
   "prompts/system/base.md"
   `(("TONE_PROMPT" . ,mevedel-system--tone-prompt)))
  "Static base prompt for normal mevedel sessions.")

(defconst mevedel-system--tutor-base-prompt
  (mevedel-system-render-prompt-file
   "prompts/system/tutor.md"
   `(("TONE_PROMPT" . ,mevedel-system--tone-prompt)))
  "Static base prompt for tutor mevedel sessions.")


;;
;;; Prompt section registry

(cl-defstruct (mevedel-system-context
               (:constructor mevedel-system-context--create))
  "Request-time context passed to prompt section producers."
  base-prompt
  workspace
  working-directory)

(cl-defstruct (mevedel-system-prompt-section
               (:constructor mevedel-system-prompt-section--create))
  "A named prompt section.

NAME is the section identifier.  ORDER controls assembly order.  PRODUCER
is called with a `mevedel-system-context' and returns a string or nil.
CACHE is nil, `global', `keyed', or t.  `global' and t share one cached
value across contexts.  `keyed' caches per value returned by CACHE-KEY."
  name
  order
  producer
  cache
  cache-key)

(defvar mevedel-system--prompt-sections nil
  "Registered prompt sections.")

(defvar mevedel-system--prompt-section-cache (make-hash-table :test #'equal)
  "Memoized prompt section values keyed by section name and cache key.")

(defconst mevedel-system--prompt-cache-miss (make-symbol "mevedel-prompt-cache-miss")
  "Sentinel for missing prompt section cache entries.")

(defun mevedel-system--register-prompt-section (name props)
  "Register prompt section NAME with PROPS."
  (let ((producer (plist-get props :producer))
        (cache (plist-get props :cache))
        (cache-key (plist-get props :cache-key)))
    (unless (functionp producer)
      (error "Prompt section :producer must be a function"))
    (when (and (eq cache 'keyed)
               (not (functionp cache-key)))
      (error "Keyed prompt section requires :cache-key"))
    (mevedel-system-clear-prompt-section-cache name)
    (setq mevedel-system--prompt-sections
          (cons
           (mevedel-system-prompt-section--create
            :name name
            :order (or (plist-get props :order) 100)
            :producer producer
            :cache cache
            :cache-key cache-key)
           (let (sections)
             (dolist (section mevedel-system--prompt-sections
                              (nreverse sections))
               (unless (eq name (mevedel-system-prompt-section-name section))
                 (push section sections)))))))
  name)

(defmacro mevedel-define-prompt-section (name &rest props)
  "Define prompt section NAME.

Recognized PROPS:

- `:order' integer ordering key.
- `:producer' function called with a `mevedel-system-context'.
- `:cache' nil, `global', `keyed', or t.
- `:cache-key' function called with context for keyed sections."
  `(mevedel-system--register-prompt-section
    ',name
    (list ,@props)))

(defun mevedel-system-clear-prompt-section-cache (&optional name)
  "Clear memoized prompt section values.

When NAME is nil, clear all prompt section cache entries."
  (if (null name)
      (clrhash mevedel-system--prompt-section-cache)
    (let (keys)
      (maphash
       (lambda (key _value)
         (when (eq (car-safe key) name)
           (push key keys)))
       mevedel-system--prompt-section-cache)
      (dolist (key keys)
        (remhash key mevedel-system--prompt-section-cache)))))

(defun mevedel-system--prompt-sections-sorted ()
  "Return prompt sections sorted by ascending order."
  (sort (copy-sequence mevedel-system--prompt-sections)
        (lambda (a b)
          (< (mevedel-system-prompt-section-order a)
             (mevedel-system-prompt-section-order b)))))

(defun mevedel-system--section-cache-key (section context)
  "Return cache key for SECTION and CONTEXT, or nil when uncached."
  (let ((cache (mevedel-system-prompt-section-cache section))
        (name (mevedel-system-prompt-section-name section)))
    (pcase cache
      ('global (list name :global))
      ('keyed (list name
                    (funcall (mevedel-system-prompt-section-cache-key section)
                             context)))
      ('nil nil)
      (_ (and cache (list name :global))))))

(defun mevedel-system--render-section (section context)
  "Return rendered SECTION for CONTEXT."
  (let ((key (mevedel-system--section-cache-key section context)))
    (if (null key)
        (funcall (mevedel-system-prompt-section-producer section) context)
      (let ((cached (gethash key mevedel-system--prompt-section-cache
                             mevedel-system--prompt-cache-miss)))
        (if (not (eq cached mevedel-system--prompt-cache-miss))
            cached
          (let ((value (funcall (mevedel-system-prompt-section-producer section)
                                context)))
            (puthash key value mevedel-system--prompt-section-cache)
            value))))))

(defun mevedel-system-prompt-section-report (&optional base-prompt workspace working-directory)
  "Return audit data for prompt sections under the current context."
  (let* ((context (mevedel-system--make-context
                   (or base-prompt mevedel-system--base-prompt)
                   workspace working-directory)))
    (mapcar
     (lambda (section)
       (let* ((key (mevedel-system--section-cache-key section context))
              (cached (and key
                           (not (eq (gethash key mevedel-system--prompt-section-cache
                                             mevedel-system--prompt-cache-miss)
                                    mevedel-system--prompt-cache-miss))))
              (value (mevedel-system--render-section section context)))
         (list :name (mevedel-system-prompt-section-name section)
               :order (mevedel-system-prompt-section-order section)
               :cache (mevedel-system-prompt-section-cache section)
               :cached cached
               :chars (if (stringp value) (length value) 0))))
     (mevedel-system--prompt-sections-sorted))))


;;
;;; Dynamic section helpers

(defun mevedel-system--workspace-root (workspace)
  "Return WORKSPACE's root, or the current workspace root."
  (mevedel-workspace--root (or workspace (mevedel-workspace))))

(defun mevedel-system--file-cache-key (file)
  "Return metadata cache key for FILE."
  (let ((expanded (expand-file-name file)))
    (if (file-exists-p expanded)
        (let ((attrs (file-attributes expanded)))
          (list :file (file-truename expanded)
                :mtime (file-attribute-modification-time attrs)
                :size (file-attribute-size attrs)))
      (list :missing expanded))))

(defun mevedel-system--memory-file (workspace)
  "Return WORKSPACE memory file path."
  (file-name-concat
   (mevedel-system--workspace-root workspace)
   ".mevedel" "memory" "MEMORY.md"))

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

(defun mevedel-system--memory-content (workspace)
  "Return WORKSPACE memory index content, or an empty notice."
  (let ((memory-file (mevedel-system--memory-file workspace)))
    (if (file-exists-p memory-file)
        (concat
         (mevedel-system--memory-updated-header memory-file)
         "\n"
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
      "Your MEMORY.md index is currently empty. As you complete tasks, save
durable memories in separate topic files and link them from MEMORY.md.
Anything linked from MEMORY.md can be discovered in future conversations.")))

(defconst mevedel-system--memory-prompt
  (lambda (&optional workspace)
    (let ((root (mevedel-system--workspace-root workspace)))
      (mevedel-system-render-prompt-file
       "prompts/system/memory-policy.md"
       `(("MEMORY_DIR" . ,(file-name-concat root ".mevedel" "memory"))
         ("MEMORY_CONTENT" . ,(mevedel-system--memory-content workspace))))))
  "Function returning the dynamic persistent memory prompt.")

(defun mevedel-system--memory-cache-key (context)
  "Return cache key for the memory prompt section."
  (list
   :file (mevedel-system--file-cache-key
          (mevedel-system--memory-file
           (mevedel-system-context-workspace context)))
   :date (mevedel-system--current-date)))

(defun mevedel-system--working-directory (workspace working-directory)
  "Return the effective working directory for WORKSPACE."
  (file-name-as-directory
   (expand-file-name
    (or working-directory
        (and (boundp 'mevedel--session)
             mevedel--session
             (eq workspace (mevedel-session-workspace mevedel--session))
             (mevedel-session-working-directory mevedel--session))
        (mevedel-system--workspace-root workspace)))))

(defun mevedel-system--workspace-config-files (workspace &optional working-directory)
  "Return layered workspace instruction files for WORKSPACE.

Files are ordered from workspace root to WORKING-DIRECTORY.  Within a
single directory, AGENTS.md wins over CLAUDE.md, and AGENTS.local.md is
loaded after the shared file when present."
  (when-let* ((workspace-root (and workspace (mevedel-workspace--root workspace))))
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
                       (claude-md (expand-file-name "CLAUDE.md" dir))
                       (local-md (expand-file-name "AGENTS.local.md" dir))
                       (shared-md (cond
                                   ((file-readable-p agents-md) agents-md)
                                   ((file-readable-p claude-md) claude-md))))
                  (delq nil
                        (list shared-md
                              (and (file-readable-p local-md)
                                   local-md)))))
              dirs)))))

(defun mevedel-system--workspace-config-content (workspace &optional working-directory)
  "Return layered AGENTS/CLAUDE workspace guidance for WORKSPACE, or nil."
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
  "Return the workspace configuration prompt for WORKSPACE, or nil."
  (when-let* ((content (mevedel-system--workspace-config-content
                       workspace working-directory)))
    (concat "## Workspace Configuration\n\n"
            "The following configuration files apply to the session, "
            "ordered from broadest to closest scope:\n\n"
            content)))

(defun mevedel-system--workspace-config-cache-key (context)
  "Return cache key for the workspace configuration prompt section."
  (or
   (mapcar #'mevedel-system--file-cache-key
           (mevedel-system--workspace-config-files
            (mevedel-system-context-workspace context)
            (mevedel-system-context-working-directory context)))
   (list :none
         (and (mevedel-system-context-workspace context)
              (mevedel-workspace--root
               (mevedel-system-context-workspace context)))
         (mevedel-system-context-working-directory context))))

(defun mevedel-system--environment-prompt (workspace &optional working-directory)
  "Return the dynamic environment prompt for WORKSPACE."
  (concat "## Environment\n\n"
          "Here is useful information about the environment you are running in:\n<env>\n"
          (mevedel--environment-info-string workspace working-directory)
          "\n</env>"))

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

(defun mevedel-system--make-context (base-prompt workspace working-directory)
  "Return normalized prompt context for BASE-PROMPT."
  (let* ((workspace (or workspace (mevedel-workspace)))
         (working-directory
          (mevedel-system--working-directory workspace working-directory)))
    (mevedel-system-context--create
     :base-prompt base-prompt
     :workspace workspace
     :working-directory working-directory)))

(mevedel-define-prompt-section base
  :order 10
  :cache 'keyed
  :cache-key (lambda (context)
               (mevedel-system-context-base-prompt context))
  :producer (lambda (context)
              (mevedel-system-context-base-prompt context)))

(mevedel-define-prompt-section workspace-config
  :order 20
  :cache 'keyed
  :cache-key #'mevedel-system--workspace-config-cache-key
  :producer (lambda (context)
              (mevedel-system--workspace-config-prompt
               (mevedel-system-context-workspace context)
               (mevedel-system-context-working-directory context))))

(mevedel-define-prompt-section memory
  :order 30
  :cache 'keyed
  :cache-key #'mevedel-system--memory-cache-key
  :producer (lambda (context)
              (funcall mevedel-system--memory-prompt
                       (mevedel-system-context-workspace context))))

(mevedel-define-prompt-section environment
  :order 40
  :producer (lambda (context)
              (mevedel-system--environment-prompt
               (mevedel-system-context-workspace context)
               (mevedel-system-context-working-directory context))))


;;
;;; System prompt builder

(defun mevedel-system-render-sections
    (base-prompt &optional workspace working-directory)
  "Return rendered prompt sections for BASE-PROMPT.

WORKSPACE and WORKING-DIRECTORY are normalized the same way as
`mevedel-system-build-prompt'."
  (let ((context (mevedel-system--make-context
                  base-prompt workspace working-directory)))
    (mapcar
     (lambda (section)
       (mevedel-system--render-section section context))
     (mevedel-system--prompt-sections-sorted))))

(defun mevedel-system-render-named-sections
    (base-prompt section-names &optional workspace working-directory)
  "Return rendered prompt SECTION-NAMES for BASE-PROMPT.

SECTION-NAMES is a list of prompt section symbols.  Sections are still
rendered in their registered order; unknown names are ignored."
  (let ((context (mevedel-system--make-context
                  base-prompt workspace working-directory))
        (wanted (copy-sequence section-names)))
    (delq nil
          (mapcar
           (lambda (section)
             (when (memq (mevedel-system-prompt-section-name section) wanted)
               (mevedel-system--render-section section context)))
           (mevedel-system--prompt-sections-sorted)))))

(defun mevedel-system-build-prompt (base-prompt &optional workspace working-directory)
  "Build the full request-time system prompt.

WORKSPACE specifies the workspace context for configuration, memory, and
environment sections.  If nil, use the current buffer's workspace.
WORKING-DIRECTORY specifies the session cwd for layered instructions and
environment data.
Static content is emitted first and dynamic content last to improve
provider prefix-cache reuse."
  (apply #'mevedel-system--join-parts
         (mevedel-system-render-sections
          base-prompt workspace working-directory)))

(cl-defun mevedel-system-build-agent-prompt
    (base-prompt &key workspace working-directory
                 (workspace-config t) (memory t) (environment t))
  "Build a system prompt for an agent from BASE-PROMPT.

The agent prompt is always emitted as the `base' section.  The keyword
flags control whether the normal dynamic sections are appended.  This
lets utility agents keep a narrow identity prompt while still receiving
environment details."
  (let ((sections (append '(base)
                          (when workspace-config '(workspace-config))
                          (when memory '(memory))
                          (when environment '(environment)))))
    (apply #'mevedel-system--join-parts
           (mevedel-system-render-named-sections
            base-prompt sections workspace working-directory))))

(provide 'mevedel-system)
;;; mevedel-system.el ends here
