;;; niwa-tool-fs.el -- File system tool registration -*- lexical-binding: t -*-

;;; Commentary:

;; Shared file-system tool primitives and Read, Glob, and Grep registration.

;;; Code:

(eval-when-compile
  (require 'niwa-tool-registry))

(require 'niwa-execution-target)
(require 'niwa-resource)
(require 'niwa-structs)
(require 'subr-x)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `niwa-execution-target'
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))

;; `niwa-resource'
(declare-function niwa-resource-address-like-p
                  "niwa-resource" (value))
(declare-function niwa-resource-current-attempt
                  "niwa-resource" (address))
(declare-function niwa-resource-encode-component
                  "niwa-resource" (value))
(declare-function niwa-resource-parse-address
                  "niwa-resource" (address))
(declare-function niwa-resource-within-root-p
                  "niwa-resource" (path root))

;; `niwa-structs'
(declare-function niwa-session-execution-target "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(declare-function niwa-workspace-root "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-tool-fs-read'
(declare-function niwa-tool-fs-read "niwa-tool-fs-read" (callback args))
(declare-function niwa-tool-fs-read-render
                  "niwa-tool-fs-read"
                  (name args result _render-data))

;; `niwa-tool-fs-search'
(declare-function niwa-tool-fs-search-glob
                  "niwa-tool-fs-search" (callback args))
(declare-function niwa-tool-fs-search-grep
                  "niwa-tool-fs-search" (callback args))
(declare-function niwa-tool-fs-search-render-glob
                  "niwa-tool-fs-search"
                  (name args result _render-data))
(declare-function niwa-tool-fs-search-render-grep
                  "niwa-tool-fs-search"
                  (name args result _render-data))

(autoload 'niwa-tool-fs-read "niwa-tool-fs-read")
(autoload 'niwa-tool-fs-read-render "niwa-tool-fs-read")
(autoload 'niwa-tool-fs-search-glob "niwa-tool-fs-search")
(autoload 'niwa-tool-fs-search-grep "niwa-tool-fs-search")
(autoload 'niwa-tool-fs-search-render-glob "niwa-tool-fs-search")
(autoload 'niwa-tool-fs-search-render-grep "niwa-tool-fs-search")

;; `niwa-utilities'
(declare-function niwa--executable-find
                  "niwa-utilities" (name &optional remote))
(autoload 'niwa--executable-find "niwa-utilities")

;; `niwa-workspace'
(defvar niwa--workspace)

(defun niwa-tool-fs-current-workspace-root ()
  "Return the current workspace root visible to the renderer, or nil."
  (or (and (boundp 'niwa--workspace)
           niwa--workspace
           (ignore-errors
             (niwa-workspace-root niwa--workspace)))
      (and (boundp 'niwa--session)
           niwa--session
           (ignore-errors
             (niwa-workspace-root
              (niwa-session-workspace niwa--session))))))

(defun niwa-tool-fs-display-path (path)
  "Return PATH as a compact display path for tool headers.

Decides containment by name.  This runs from a tool renderer, and
renderers run from the redraw path, where TRAMP yields to timers and
process sentinels between its send and its read: a stat issued here
lands inside whatever remote command the redraw interrupted, and TRAMP
refuses the nested call -- the renderer then fails outright and the row
loses its header.  `file-in-directory-p\' looks like a string test but
resolves both arguments with `file-truename\', which is a round trip
apiece on a remote workspace.

Both names are absolute and expanded here, so a prefix comparison
answers the same question for every path that is genuinely under the
root.  It differs only for a path reaching the root through a symlink,
which then shows as its full name rather than a relative one -- a
cosmetic loss in a header, against a renderer that cannot fail."
  (or (and (stringp path)
           (niwa-resource-address-like-p path)
           path)
      (when (and (stringp path)
                 (not (string-empty-p path)))
        (let* ((root (niwa-tool-fs-current-workspace-root))
               (expanded-root (and root (expand-file-name root)))
               (base (niwa-conversation-working-directory))
               (full-path (expand-file-name path base)))
          (when (and expanded-root
                     (string-prefix-p (file-name-as-directory expanded-root)
                                      full-path))
            (file-relative-name full-path expanded-root))))
      (and path (file-name-nondirectory path))
      "?"))

(defun niwa-tool-fs-model-path (path)
  "Return PATH in the current session's model-visible path domain."
  (if-let* ((session (bound-and-true-p niwa--session))
            (target (niwa-session-execution-target session)))
      (niwa-execution-target-native-path target path)
    path))

(defun niwa-tool-fs-visible-path (path &optional resource-address)
  "Return model-visible PATH for the active resource operation.

Resource handlers keep their resolved path in local variables, but every
path that crosses the handler result boundary is the authored address."
  (or resource-address
      (niwa-tool-fs-model-path path)))

(defun niwa-tool-fs-resource-attempt (address)
  "Return the prepared attempt for authored ADDRESS, or nil for a path."
  (when (and (stringp address)
             (niwa-resource-address-like-p address))
    (niwa-resource-current-attempt address)))

(defun niwa-tool-fs-resource-rg-exclusions (address)
  "Return resource-specific ripgrep glob arguments for ADDRESS."
  (when (stringp address)
    (cond
     ((string-prefix-p "artifact://" address)
      ;; Glob uses --iglob for its include pattern.  Ripgrep gives those
      ;; patterns precedence over --glob, so exclusions must use it too.
      '("--iglob=!**/.niwa-pending-executions/**"
        "--iglob=!**/.niwa-spool-*"))
     ((string-prefix-p "niwa://" address)
      '("--glob=**/*.md")))))

(defun niwa-tool-fs-resource-child-address
    (address root path &optional address-prefix)
  "Return the logical child of ADDRESS for private PATH below ROOT.

ADDRESS is parsed with the canonical resource parser; ROOT and PATH are
only used to compute a relative component and are never returned.  When
ADDRESS-PREFIX is non-nil, use it as an already canonical logical prefix;
this is used for the dynamic memory union whose root itself is not an
addressable locator."
  (when (and address root path)
    (let* ((root (expand-file-name root))
           ;; Target processes emit native absolute paths, not TRAMP names.
           ;; Qualify them before containment checks, never after them.
           (path (if (and (file-remote-p root) (not (file-remote-p path))
                          (file-name-absolute-p path))
                     (concat (file-remote-p root) path)
                   path))
           (path (expand-file-name path root)))
      (when (niwa-resource-within-root-p path root)
        (let* ((parsed (and (not address-prefix)
                            (niwa-resource-parse-address address)))
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
                                 #'niwa-resource-encode-component
                                 components "/"))))
            (if (eq (plist-get parsed :locator-class) 'alias)
                (concat (plist-get parsed :canonical)
                        (when relative-components
                          (concat
                           "/"
                           (mapconcat #'niwa-resource-encode-component
                                      relative-components "/"))))
              (concat (symbol-name scheme) "://"
                      (if (eq scheme 'skill)
                          (concat (niwa-resource-encode-component
                                   (plist-get parsed :name))
                                  "@" (plist-get parsed :source-key)
                                  (if components
                                      (concat "/"
                                              (mapconcat
                                               #'niwa-resource-encode-component
                                               components
                                               "/"))
                                    ""))
                        (mapconcat #'niwa-resource-encode-component
                                   components "/"))))))))))

(defun niwa-tool-fs-strip-system-reminders (result)
  "Return RESULT without a trailing appended system-reminder block."
  (if (and (stringp result)
           (string-search "<system-reminder>" result))
      (replace-regexp-in-string
       "\n\n<system-reminder>\n\\(?:.\\|\n\\)*?</system-reminder>\\'"
       "" result t)
    result))

(defun niwa-tool-fs-handler-result (result)
  "Return RESULT as a canonical native handler envelope."
  (if (and (proper-list-p result) (plist-member result :result))
      result
    (list :result result)))

(defun niwa-tool-fs-executable-find (name path)
  "Find executable NAME in PATH's execution target.

Cached through `niwa--executable-find': Glob, Grep and Read each probe
for `rg' on every call, and on a remote target an uncached probe walks the
whole PATH from inside gptel's curl sentinel."
  (niwa--executable-find name (file-remote-p path)))

(defun niwa-tool-fs--register ()
  "Register file system tools for niwa."
  (require 'gptel)

  (niwa-define-tool
    :name "Glob"
    :description "Find paths matching a glob, with bounded results and search time."
    :prompt-file "prompts/tools/glob.md"
    :handler #'niwa-tool-fs-search-glob
    :args ((pattern string :required
                   "The glob pattern to match files against.")
           (path path-or-resource :optional
                 "The directory to search in. If not specified, the session working directory will be used. Relative paths are resolved from the session working directory."))
    :async-p t
    :read-only-p t
    :max-result-size 30000
    :groups (read)
    :get-path (lambda (args) (plist-get args :path))
    :renderer '((success . niwa-tool-fs-search-render-glob)))

  (niwa-define-tool
    :name "Read"
    :description "Read supported text or media from a file or resource address."
    :prompt-file "prompts/tools/read.md"
    :handler #'niwa-tool-fs-read
    :async-p t
    :args ((file_path path-or-resource :required "Absolute or relative path to the file to read, or a canonical resource address. Relative paths are resolved from the session working directory.")
           (offset integer :optional
                  "Text-file line number to start reading from. Do not provide for images or PDFs.")
           (limit integer :optional
                  "Text-file number of lines to read. Do not provide for images or PDFs.")
           (char_offset integer :optional
                        "Zero-based character offset within the selected text line (default 0). A positive value selects one line; limit must be 1 or omitted. Returns at most 2000 characters with continuation guidance. Do not provide for images or PDFs.")
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
    :renderer '((success . niwa-tool-fs-read-render)))

  (niwa-define-tool
    :name "Grep"
    :description "Search file contents using ripgrep."
    :prompt-file "prompts/tools/grep.md"
    :handler #'niwa-tool-fs-search-grep
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
    :renderer '((success . niwa-tool-fs-search-render-grep))))

(provide 'niwa-tool-fs)

;;; niwa-tool-fs.el ends here
