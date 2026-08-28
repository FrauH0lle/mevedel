;;; mevedel-tool-fs.el -- File system tool registration -*- lexical-binding: t -*-

;;; Commentary:

;; Shared file-system tool primitives and Read, Glob, and Grep registration.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'mevedel-execution-target)
(require 'mevedel-resource)
(require 'mevedel-structs)
(require 'subr-x)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))

;; `mevedel-resource'
(declare-function mevedel-resource-address-like-p
                  "mevedel-resource" (value))
(declare-function mevedel-resource-current-attempt
                  "mevedel-resource" (address))
(declare-function mevedel-resource-encode-component
                  "mevedel-resource" (value))
(declare-function mevedel-resource-parse-address
                  "mevedel-resource" (address))
(declare-function mevedel-resource-within-root-p
                  "mevedel-resource" (path root))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read "mevedel-tool-fs-read" (args))
(declare-function mevedel-tool-fs-read-render
                  "mevedel-tool-fs-read"
                  (name args result _render-data))

;; `mevedel-tool-fs-search'
(declare-function mevedel-tool-fs-search-glob
                  "mevedel-tool-fs-search" (callback args))
(declare-function mevedel-tool-fs-search-grep
                  "mevedel-tool-fs-search" (callback args))
(declare-function mevedel-tool-fs-search-render-glob
                  "mevedel-tool-fs-search"
                  (name args result _render-data))
(declare-function mevedel-tool-fs-search-render-grep
                  "mevedel-tool-fs-search"
                  (name args result _render-data))

;; `mevedel-workspace'
(defvar mevedel--workspace)

(defun mevedel-tool-fs-current-workspace-root ()
  "Return the current workspace root visible to the renderer, or nil."
  (or (and (boundp 'mevedel--workspace)
           mevedel--workspace
           (ignore-errors
             (mevedel-workspace-root mevedel--workspace)))
      (and (boundp 'mevedel--session)
           mevedel--session
           (ignore-errors
             (mevedel-workspace-root
              (mevedel-session-workspace mevedel--session))))))

(defun mevedel-tool-fs-display-path (path)
  "Return PATH as a compact display path for tool headers."
  (or (and (stringp path)
           (mevedel-resource-address-like-p path)
           path)
      (when (and (stringp path)
                 (not (string-empty-p path)))
        (let* ((root (mevedel-tool-fs-current-workspace-root))
               (expanded-root (and root (expand-file-name root)))
               (base (or (and (boundp 'mevedel--session)
                              mevedel--session
                              (ignore-errors
                                (mevedel-session-working-directory
                                 mevedel--session)))
                         default-directory))
               (full-path (expand-file-name path base)))
          (when (and expanded-root
                     (file-in-directory-p
                      full-path (file-name-as-directory expanded-root)))
            (file-relative-name full-path expanded-root))))
      (and path (file-name-nondirectory path))
      "?"))

(defun mevedel-tool-fs-model-path (path)
  "Return PATH in the current session's model-visible path domain."
  (if-let* ((session (bound-and-true-p mevedel--session))
            (target (mevedel-session-execution-target session)))
      (mevedel-execution-target-native-path target path)
    path))

(defun mevedel-tool-fs-visible-path (path &optional resource-address)
  "Return model-visible PATH for the active resource operation.

Resource handlers keep their resolved path in local variables, but every
path that crosses the handler result boundary is the authored address."
  (or resource-address
      (mevedel-tool-fs-model-path path)))

(defun mevedel-tool-fs-resource-attempt (address)
  "Return the prepared attempt for authored ADDRESS, or nil for a path."
  (when (and (stringp address)
             (mevedel-resource-address-like-p address))
    (mevedel-resource-current-attempt address)))

(defun mevedel-tool-fs-resource-rg-exclusions (address)
  "Return resource-specific ripgrep glob arguments for ADDRESS."
  (when (stringp address)
    (cond
     ((string-prefix-p "artifact://" address)
      '("--glob=!**/.mevedel-pending-executions/**"))
     ((string-prefix-p "mevedel://" address)
      '("--glob=**/*.md")))))

(defun mevedel-tool-fs-resource-child-address
    (address root path &optional address-prefix)
  "Return the logical child of ADDRESS for private PATH below ROOT.

ADDRESS is parsed with the canonical resource parser; ROOT and PATH are
only used to compute a relative component and are never returned.  When
ADDRESS-PREFIX is non-nil, use it as an already canonical logical prefix;
this is used for the dynamic memory union whose root itself is not an
addressable locator."
  (when (and address root path)
    (let* ((root (expand-file-name root))
           (path (expand-file-name path root)))
      (when (mevedel-resource-within-root-p path root)
        (let* ((parsed (and (not address-prefix)
                            (mevedel-resource-parse-address address)))
               (scheme (and parsed (plist-get parsed :scheme)))
               (components (and parsed
                                (copy-sequence
                                 (plist-get parsed :components))))
               (relative (file-relative-name path root))
               (relative-components
                (unless (string= relative ".")
                  (file-name-split relative))))
          (unless (string= relative ".")
            (if address-prefix
                (setq components relative-components)
              (setq components (append components relative-components))))
          (if address-prefix
              (concat address-prefix
                      (unless (string= relative ".")
                        (concat "/"
                                (mapconcat
                                 #'mevedel-resource-encode-component
                                 components "/"))))
            (if (eq (plist-get parsed :locator-class) 'alias)
                (concat (plist-get parsed :canonical)
                        (when relative-components
                          (concat
                           "/"
                           (mapconcat #'mevedel-resource-encode-component
                                      relative-components "/"))))
              (concat (symbol-name scheme) "://"
                      (if (eq scheme 'skill)
                          (concat (mevedel-resource-encode-component
                                   (plist-get parsed :name))
                                  "@" (plist-get parsed :source-key)
                                  (if components
                                      (concat "/"
                                              (mapconcat
                                               #'mevedel-resource-encode-component
                                               components
                                               "/"))
                                    ""))
                        (mapconcat #'mevedel-resource-encode-component
                                   components "/"))))))))))

(defun mevedel-tool-fs-strip-system-reminders (result)
  "Return RESULT without a trailing appended system-reminder block."
  (if (and (stringp result)
           (string-search "<system-reminder>" result))
      (replace-regexp-in-string
       "\n\n<system-reminder>\n\\(?:.\\|\n\\)*?</system-reminder>\\'"
       "" result t)
    result))

(defun mevedel-tool-fs-handler-result (result)
  "Return RESULT as a canonical native handler envelope."
  (if (and (proper-list-p result) (plist-member result :result))
      result
    (list :result result)))

;; `mevedel-utilities'
(declare-function mevedel--executable-find
                  "mevedel-utilities" (name &optional remote))
(autoload 'mevedel--executable-find "mevedel-utilities")

(defun mevedel-tool-fs-executable-find (name path)
  "Find executable NAME in PATH's execution target.

Cached through `mevedel--executable-find': Glob, Grep and Read each probe
for `rg' on every call, and on a remote target an uncached probe walks the
whole PATH from inside gptel's curl sentinel."
  (mevedel--executable-find name (file-remote-p path)))

(defun mevedel-tool-fs--register ()
  "Register file system tools for mevedel."
  (require 'gptel)
  (require 'mevedel-tool-fs-read)
  (require 'mevedel-tool-fs-search)

  (mevedel-define-tool
    :name "Glob"
    :description "Fast file pattern matching tool that works with any codebase size."
    :prompt-file "prompts/tools/glob.md"
    :handler #'mevedel-tool-fs-search-glob
    :args ((pattern string :required
                   "The glob pattern to match files against.")
           (path path-or-resource :optional
                 "The directory to search in. If not specified, the session working directory will be used. Relative paths are resolved from the session working directory."))
    :async-p t
    :read-only-p t
    :max-result-size 30000
    :groups (read)
    :get-path (lambda (args) (plist-get args :path))
    :renderer #'mevedel-tool-fs-search-render-glob)

  (mevedel-define-tool
    :name "Read"
    :description "Read a file from the local filesystem."
    :prompt-file "prompts/tools/read.md"
    :handler #'mevedel-tool-fs-read
    :args ((file_path path-or-resource :required "Absolute or relative path to the file to read, or a canonical resource address. Relative paths are resolved from the session working directory.")
           (offset integer :optional
                  "Text-file line number to start reading from. Do not provide for images or PDFs.")
           (limit integer :optional
                  "Text-file number of lines to read. Do not provide for images or PDFs.")
           (pages string :optional
                  "PDF-only page selector. Use for specific or large PDFs. Supports forms like \"3\", \"1-5\", and \"3-\". Each request is capped at 20 pages.")
           (max_width integer :optional
                      "Image/PDF-page-image maximum width in pixels. Requires ImageMagick.")
           (max_height integer :optional
                       "Image/PDF-page-image maximum height in pixels. Requires ImageMagick.")
           (max_tokens integer :optional
                       "Image/PDF-page-image approximate compression target. Requires ImageMagick."))
    :read-only-p t
    :groups (read)
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-fs-read-render)

  (mevedel-define-tool
    :name "Grep"
    :description "Search file contents using ripgrep."
    :prompt-file "prompts/tools/grep.md"
    :handler #'mevedel-tool-fs-search-grep
    :args ((pattern string :required
                   "The regular expression pattern to search for in file contents.")
           (path path-or-resource :optional
                 "File or directory to search in (rg PATH). Defaults to the session working directory. Relative paths are resolved from the session working directory. An explicitly selected ignored path is searched.")
           (glob string :optional
                 "Explicit inclusion glob for files (e.g. \"*.el\", \"*.{ts,tsx}\") -- maps to rg --glob and may select otherwise ignored files.")
           (output_mode string :optional
                        "Output mode: \"content\" shows matching lines (supports -A/-B/-C context, -n line numbers, head_limit), \"files_with_matches\" shows file paths (supports head_limit), \"count\" shows match counts (supports head_limit). Defaults to \"files_with_matches\".")
           (head_limit integer :optional
                      "Limit output to first N lines/entries. Defaults to 250 when unspecified. Pass 0 for unlimited (use sparingly).")
           (offset integer :optional
                  "Skip first N lines/entries before applying head_limit. Defaults to 0.")
           (context integer :optional
                   "Number of lines to show before and after each match (rg -C). Requires output_mode: \"content\".")
           (-A integer :optional
               "Number of lines to show after each match (rg -A). Requires output_mode: \"content\".")
           (-B integer :optional
               "Number of lines to show before each match (rg -B). Requires output_mode: \"content\".")
           (-C integer :optional
               "Alias for context.")
           (-i boolean :optional
               "Case insensitive search (rg -i).")
           (-n boolean :optional
               "Show line numbers in output (rg -n). Requires output_mode: \"content\". Defaults to true.")
           (type string :optional
                 "File type to search (rg --type). Common types: elisp, js, py, rust, go, java, etc.")
           (multiline boolean :optional
                     "Enable multiline mode where . matches newlines and patterns can span lines (rg -U --multiline-dotall). Default: false."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (read)
    :get-path (lambda (args) (plist-get args :path))
    :renderer #'mevedel-tool-fs-search-render-grep))

(provide 'mevedel-tool-fs)

;;; mevedel-tool-fs.el ends here
