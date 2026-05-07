;;; mevedel-system.el -- System prompt -*- lexical-binding: t -*-

;;; Commentary:

;; System prompt assembly.  The full prompt is composed from several
;; string constants (tone, task protocol, tool usage guidance,
;; delegation rules) plus dynamic sections built at request time:
;; persistent memory (from `.mevedel/memory/MEMORY.md'), environment
;; info, and the workspace-level AGENTS.md / CLAUDE.md if present.
;; A separate tutor-base-prompt drives the tutoring preset.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)

;; `gptel-agent'
(declare-function gptel-agent-read-file "ext:gptel-agent" (agent-file &optional templates metadata-only))

;; `mevedel-utilities'
(declare-function mevedel--environment-info-string "mevedel-utilities" (&optional workspace))

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

(defun mevedel-system--workspace-root (workspace)
  "Return WORKSPACE's root, or the current workspace root."
  (mevedel-workspace--root (or workspace (mevedel-workspace))))

(defun mevedel-system--memory-content (workspace)
  "Return the first 200 lines of WORKSPACE memory, or an empty notice."
  (let ((memory-file (file-name-concat
                      (mevedel-system--workspace-root workspace)
                      ".mevedel" "memory" "MEMORY.md")))
    (if (file-exists-p memory-file)
        (string-join
         (with-temp-buffer
           (insert-file-contents memory-file)
           (cl-loop repeat 200
                    unless (eobp)
                    collect (prog1 (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))
                              (forward-line 1))))
         "\n")
      "Your MEMORY.md is currently empty. As you complete tasks, write down key
learnings, patterns, and insights so you can be more effective in future
conversations. Anything saved in MEMORY.md will be included in your
system prompt next time.")))

(defconst mevedel-system--memory-prompt
  (lambda (&optional workspace)
    (let ((root (mevedel-system--workspace-root workspace)))
      (mevedel-system-render-prompt-file
       "prompts/system/memory-policy.md"
       `(("MEMORY_DIR" . ,(file-name-concat root ".mevedel" "memory"))
         ("MEMORY_CONTENT" . ,(mevedel-system--memory-content workspace))))))
  "Function returning the dynamic persistent memory prompt.")

(defun mevedel-system--workspace-config-content (workspace)
  "Return AGENTS.md or CLAUDE.md content for WORKSPACE, or nil."
  (when-let* ((workspace-root (and workspace (mevedel-workspace--root workspace))))
    (let ((agents-md (expand-file-name "AGENTS.md" workspace-root))
          (claude-md (expand-file-name "CLAUDE.md" workspace-root)))
      (cond
       ((file-readable-p agents-md)
        (with-temp-buffer
          (insert-file-contents agents-md)
          (buffer-string)))
       ((file-readable-p claude-md)
        (with-temp-buffer
          (insert-file-contents claude-md)
          (buffer-string)))))))

(defun mevedel-system--workspace-config-prompt (workspace)
  "Return the workspace configuration prompt for WORKSPACE, or nil."
  (when-let* ((content (mevedel-system--workspace-config-content workspace)))
    (concat "## Workspace Configuration\n\n"
            "The following configuration was found in the workspace root:\n\n"
            content)))

(defun mevedel-system--environment-prompt (workspace)
  "Return the dynamic environment prompt for WORKSPACE."
  (concat "## Environment\n\n"
          "Here is useful information about the environment you are running in:\n<env>\n"
          (mevedel--environment-info-string workspace)
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


;;
;;; System prompt builder

(defun mevedel-system-build-prompt (base-prompt &optional workspace)
  "Build the full request-time system prompt.

WORKSPACE specifies the workspace context for configuration, memory, and
environment sections.  If nil, use the current buffer's workspace.
Static content is emitted first and dynamic content last to improve
provider prefix-cache reuse."
  (let ((workspace (or workspace (mevedel-workspace))))
    (mevedel-system--join-parts
     base-prompt
     (mevedel-system--workspace-config-prompt workspace)
     (funcall mevedel-system--memory-prompt workspace)
     (mevedel-system--environment-prompt workspace))))

(provide 'mevedel-system)
;;; mevedel-system.el ends here
