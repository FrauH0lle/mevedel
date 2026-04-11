;;; mevedel-tool-fs.el -- File system tools -*- lexical-binding: t -*-

;;; Commentary:

;; File system tool implementations: Read, Glob, Grep, Write, Edit, Insert,
;; MkDir.  Also provides diff generation utilities and file snapshotting used
;; by the inline/buffer preview system.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline")
(declare-function mevedel-pipeline--positional-to-plist "mevedel-pipeline")

;; `mevedel-tool-registry'
(declare-function mevedel-tool-register "mevedel-tool-registry")

;; `diff-mode'
(declare-function diff-beginning-of-hunk "diff-mode" (&optional try-harder))
(declare-function diff-filename-drop-dir "diff-mode" (file))
(declare-function diff-hunk-file-names "diff-mode" (&optional old))
(declare-function diff-hunk-next "diff-mode" (&optional count))
(declare-function diff-setup-buffer-type "diff-mode" ())

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-chat'
(defvar mevedel--diff-preview-buffer-name)

;; `mevedel-tool-ui'
(declare-function mevedel-tools--request-access "mevedel-tool-ui" (root reason &optional buffer))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))
(declare-function mevedel-pipeline--positional-to-plist "mevedel-pipeline"
                  (arg-values arg-specs))

;; `mevedel-preview-mode'
(declare-function mevedel-preview-mode-add-preview "mevedel-preview-mode" t t)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))

(defvar mevedel--workspace)

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-file-state'
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind &optional offset limit))
(declare-function mevedel-session-read-is-duplicate-p
                  "mevedel-file-state" (session path offset limit))


;;
;;; Diff Utilities

(defun mevedel-tools--generate-diff (original modified filepath &optional labels-real)
  "Generate unified diff between ORIGINAL and MODIFIED content for FILEPATH.

When LABELS-REAL is nil (the default), an empty or nil ORIGINAL /
MODIFIED is labeled `/dev/null' to mimic git's new/deleted-file display.

When LABELS-REAL is non-nil, both sides are always labeled `a/FILEPATH'
and `b/FILEPATH'.  Use this when the diff will be fed to ediff's
patching machinery, which needs a real source path to resolve."
  (with-temp-buffer
    (let ((orig-file (make-temp-file "mevedel-orig-"))
          (mod-file (make-temp-file "mevedel-mod-")))
      (unwind-protect
          (progn
            (with-temp-file orig-file (when original (insert original)))
            (with-temp-file mod-file (when modified (insert modified)))
            (call-process "diff" nil t nil
                          "-u"
                          "--label" (if (or labels-real
                                            (and original (not (string-empty-p original))))
                                        (concat "a/" filepath)
                                      "/dev/null")
                          "--label" (if (or labels-real
                                            (and modified (not (string-empty-p modified))))
                                        (concat "b/" filepath)
                                      "/dev/null")
                          orig-file mod-file)
            (buffer-string))
        (when (file-exists-p orig-file) (delete-file orig-file))
        (when (file-exists-p mod-file) (delete-file mod-file))))))

(defun mevedel-tools--setup-diff-buffer (temp-file real-path workspace root
                                                   &optional chat-buffer final-callback
                                                   user-modified original-window-config
                                                   labels-real)
  "Setup diff buffer with content and full configuration.

Creates and configures `mevedel--diff-preview-buffer-name' with:
- Diff content between REAL-PATH and TEMP-FILE
- Read-only diff-mode with truncated lines
- Proper buffer-local variables for workspace context
- Header line with file path and action hints

Arguments:
- TEMP-FILE: Path to file with proposed changes
- REAL-PATH: Path to actual file
- WORKSPACE: Workspace identifier
- ROOT: Workspace root directory
- CHAT-BUFFER: Optional chat buffer reference
- FINAL-CALLBACK: Optional callback function
- USER-MODIFIED: Optional flag for user modifications
- ORIGINAL-WINDOW-CONFIG: Optional saved window configuration
- LABELS-REAL: When non-nil, always label both sides of the diff
  with `a/REL-PATH' / `b/REL-PATH' and skip the `new file mode' /
  `deleted file mode' headers.  Use this when the diff needs to drive
  ediff's patching machinery, which requires a real source path.

Returns the configured diff buffer."
  (let* ((rel-path (file-relative-name real-path root))
         (original-content (when (file-exists-p real-path)
                             (with-temp-buffer
                               (insert-file-contents real-path)
                               (buffer-string))))
         (modified-content (with-temp-buffer
                             (insert-file-contents temp-file)
                             (buffer-string)))
         (diff (mevedel-tools--generate-diff original-content modified-content
                                             rel-path labels-real))
         (diff-buffer (generate-new-buffer mevedel--diff-preview-buffer-name)))
    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))
        ;; Add file mode lines for new or deleted files, but only when
        ;; labels-real is nil -- otherwise we pretend the file already
        ;; existed so ediff can patch it.
        (unless labels-real
          (cond
           ;; New file: original-content is nil/empty, modified-content is
           ;; non-nil.
           ((and (or (not original-content) (string-empty-p original-content))
                 (and modified-content (not (string-empty-p modified-content))))
            (insert "new file mode 100644\n"))
           ;; Deleted file: original-content is non-nil, modified-content is
           ;; nil/empty.
           ((and (and original-content (not (string-empty-p original-content)))
                 (or (not modified-content) (string-empty-p modified-content)))
            (insert "deleted file mode 100644\n"))))
        (insert diff)
        (diff-mode)
        ;; Always use read-only mode for safety
        (read-only-mode 1)
        ;; Always truncate lines for better diff readability
        (setq-local truncate-lines t)
        ;; Re-detect patch type (i.e. 'git) now that buffer is populated
        (when (derived-mode-p 'diff-mode)
          (diff-setup-buffer-type))

        ;; Always set these buffer-local variables
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--temp-file temp-file
                    mevedel--real-path real-path
                    mevedel--chat-buffer chat-buffer
                    mevedel--final-callback final-callback
                    mevedel--user-modified user-modified
                    mevedel--original-window-config original-window-config)

        (goto-char (point-min))))
    diff-buffer))


;;
;;; File Snapshotting

(defvar-local mevedel--request-file-snapshots nil
  "Alist of (FILEPATH . ORIGINAL-CONTENT) tracking files modified.

Each entry stores the original state of a file before any modifications
in the current request. ORIGINAL-CONTENT is nil if the file didn't exist
before the request. Cleared when request completes.")

(defun mevedel--snapshot-file-if-needed (filepath)
  "Capture original state of FILEPATH before first modification in request.
Does nothing if FILEPATH has already been snapshotted in this request.
Stores nil for ORIGINAL-CONTENT if file doesn't exist yet."
  (when (and filepath (stringp filepath))
    (let ((abs-path (expand-file-name filepath)))
      (unless (assoc abs-path mevedel--request-file-snapshots)
        (push (cons abs-path
                    (when (file-exists-p abs-path)
                      (with-temp-buffer
                        (insert-file-contents abs-path)
                        (buffer-string))))
              mevedel--request-file-snapshots)))))


;;
;;; File Reading

(defconst mevedel-tool-fs--binary-extensions
  '("png" "jpg" "jpeg" "gif" "bmp" "ico" "webp" "tiff" "tif"
    "mp4" "mov" "avi" "mkv" "webm" "wmv" "flv"
    "mp3" "wav" "ogg" "flac" "aac" "m4a" "wma"
    "zip" "tar" "gz" "bz2" "7z" "rar" "xz" "tgz" "iso"
    "exe" "dll" "so" "dylib" "bin" "o" "a" "obj"
    "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"
    "ttf" "otf" "woff" "woff2" "eot"
    "pyc" "class" "jar" "wasm"
    "sqlite" "sqlite3" "db"
    "psd" "sketch" "blend"
    "dat" "data")
  "File extensions that indicate binary content.")

(defconst mevedel-tool-fs--blocked-device-paths
  '("/dev/zero" "/dev/random" "/dev/urandom" "/dev/full"
    "/dev/stdin" "/dev/tty" "/dev/console"
    "/dev/stdout" "/dev/stderr"
    "/dev/fd/0" "/dev/fd/1" "/dev/fd/2")
  "Device paths that would block or produce infinite output.")

(defun mevedel-tool-fs--binary-extension-p (filename)
  "Return non-nil if FILENAME has a binary file extension."
  (member (downcase (or (file-name-extension filename) ""))
          mevedel-tool-fs--binary-extensions))

(defun mevedel-tool-fs--blocked-device-p (filename)
  "Return non-nil if FILENAME is a blocked device path."
  (or (member filename mevedel-tool-fs--blocked-device-paths)
      (and (string-prefix-p "/proc/" filename)
           (or (string-suffix-p "/fd/0" filename)
               (string-suffix-p "/fd/1" filename)
               (string-suffix-p "/fd/2" filename)))))

(defun mevedel-tool-fs--add-line-numbers (start-line)
  "Add line numbers starting at START-LINE and truncate long lines.
Operates on current buffer.  Lines longer than 2000 characters are
truncated with a [...] marker."
  (goto-char (point-min))
  (let ((line-num start-line)
        (max-line (+ start-line (count-lines (point-min) (point-max))))
        (width))
    (setq width (length (number-to-string max-line)))
    (while (not (eobp))
      (let ((line-end (line-end-position)))
        (when (> (- line-end (point)) 2000)
          (delete-region (+ (point) 2000) line-end)
          (insert " [...]")))
      (insert (format (format "%%%dd\t" width) line-num))
      (forward-line 1)
      (setq line-num (1+ line-num)))))

(defun mevedel-tool-fs--read-file (args)
  "Read file contents.
ARGS is a plist with :file_path and optional :offset, :limit."
  (let* ((filename (plist-get args :file_path))
         (offset (plist-get args :offset))
         (limit (plist-get args :limit)))

    ;; Pre-read validation
    (unless (file-readable-p filename)
      (error "File %s is not readable" filename))
    (when (file-directory-p filename)
      (error "Cannot read directory %s as file" filename))
    (when (file-symlink-p filename)
      (setq filename (file-truename filename)))
    (when (mevedel-tool-fs--binary-extension-p filename)
      (let ((ext (file-name-extension filename)))
        (error "Cannot read binary file (type: .%s): %s" ext filename)))
    (when (mevedel-tool-fs--blocked-device-p filename)
      (error "Cannot read %s: device file would block or produce infinite output"
             filename))

    (if (and (bound-and-true-p mevedel--session)
             (mevedel-session-read-is-duplicate-p
              mevedel--session filename offset limit))
        (format "File %s unchanged since last read.  Reuse the previous contents."
                filename)
      (when (bound-and-true-p mevedel--session)
        (mevedel-session-record-file-access
         mevedel--session filename 'read offset limit))

      ;; Normalize: offset defaults to 1, limit defaults to 2000
      (let ((start-line (max 1 (or offset 1)))
            (num-lines (or limit 2000)))

        (if (and (not offset) (not limit))
            ;; Full file read
            (let ((file-size (file-attribute-size (file-attributes filename))))
              (when (> file-size (* 512 1024))
                (error "File is too large (> 512 KB).  Use offset and limit to read specific portions"))
              (with-temp-buffer
                (insert-file-contents filename)
                (mevedel-tool-fs--add-line-numbers 1)
                (buffer-substring-no-properties (point-min) (point-max))))

          ;; Range read with chunked loading for large files
          (let* ((file-size (file-attribute-size (file-attributes filename)))
                 (chunk-size (min file-size (* 512 1024)))
                 (byte-offset 0)
                 (lines-to-skip (1- start-line))
                 (lines-to-read num-lines))
            (with-temp-buffer
              ;; Skip to start-line by reading chunks
              (while (and (> lines-to-skip 0)
                          (< byte-offset file-size))
                (insert-file-contents
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))
                (setq lines-to-skip (forward-line lines-to-skip))
                (when (eobp)
                  (when (/= (line-beginning-position) (line-end-position))
                    (cl-incf lines-to-skip))
                  (delete-region (point-min) (line-beginning-position))))

              (delete-region (point-min) (point))

              ;; Read lines-to-read lines
              (cl-block nil
                (while (> lines-to-read 0)
                  (setq lines-to-read (forward-line lines-to-read))
                  (when (and (eobp)
                             (/= (line-beginning-position) (line-end-position)))
                    (cl-incf lines-to-read))
                  (if (= lines-to-read 0)
                      (delete-region (point) (point-max))
                    (if (>= byte-offset file-size)
                        (cl-return)
                      (insert-file-contents
                       filename nil byte-offset (+ byte-offset chunk-size))
                      (setq byte-offset (+ byte-offset chunk-size))))))

              (mevedel-tool-fs--add-line-numbers start-line)
              (buffer-substring-no-properties (point-min) (point-max)))))))))

(defun mevedel-tool-fs--glob (callback args)
  "Find files matching a glob pattern using ripgrep.
CALLBACK receives the result string.  ARGS is a plist with :pattern
and optional :path, :depth."
  (let* ((pattern (plist-get args :pattern))
         (path (plist-get args :path))
         (depth (plist-get args :depth)))
    (when (string-empty-p pattern)
      (error "Pattern must not be empty"))
    (unless (executable-find "rg")
      (error "`ripgrep` not installed"))
    (setq path (expand-file-name (substitute-in-file-name (or path "."))))
    (unless (and (file-readable-p path) (file-directory-p path))
      (error "Path %s is not a readable directory" path))
    (with-temp-buffer
      (let* ((rg-args (list "--files" "--hidden" "--color=never"
                            "--follow" "--sort" "modified"
                            "--iglob" pattern))
             (rg-args (if (natnump depth)
                          (nconc rg-args (list "--max-depth"
                                               (number-to-string depth)))
                        rg-args))
             (rg-args (nconc rg-args (list path)))
             (exit-code (apply #'call-process "rg" nil t nil rg-args)))
        (cond
         ((= exit-code 0) nil)
         ((= exit-code 1)
          (erase-buffer)
          (insert "No files found matching pattern"))
         (t
          (goto-char (point-min))
          (insert (format "Error: glob failed (exit code %d)\n\n" exit-code)))))
      (funcall callback (buffer-string)))))

(defun mevedel-tool-fs--grep (callback args)
  "Search file contents with ripgrep.
CALLBACK receives the result string. ARGS is a plist with :pattern and
optional :path, :glob, :output_mode, :head_limit, :offset, :-i, :-n,
:type, :multiline, :context, :-A, :-B, :-C."
  (let* ((pattern (plist-get args :pattern))
         (path (plist-get args :path))
         (file-glob (plist-get args :glob))
         (output-mode (or (plist-get args :output_mode) "files_with_matches"))
         (head-limit (let ((v (plist-get args :head_limit)))
                       (cond ((null v) 250)
                             ((= v 0) nil)
                             (t v))))
         (offset (or (plist-get args :offset) 0))
         (case-fold (plist-get args :-i))
         (line-numbers (let ((v (plist-get args :-n)))
                         (if (null v)
                             (equal output-mode "content")
                           (not (eq v :json-false)))))
         (file-type (plist-get args :type))
         (multiline (and (plist-get args :multiline)
                         (not (eq (plist-get args :multiline) :json-false))))
         (ctx-after (plist-get args :-A))
         (ctx-before (plist-get args :-B))
         (ctx-around (or (plist-get args :-C) (plist-get args :context))))
    (unless (executable-find "rg")
      (error "`ripgrep` not installed"))
    (setq path (expand-file-name (substitute-in-file-name (or path "."))))
    (unless (file-readable-p path)
      (error "Path %s is not readable" path))
    (with-temp-buffer
      (let ((rg-args nil))
        ;; Output mode flags
        (pcase output-mode
          ("content"
           (when line-numbers (push "--line-number" rg-args))
           (push "--heading" rg-args)
           (when ctx-after (push (format "-A%d" ctx-after) rg-args))
           (when ctx-before (push (format "-B%d" ctx-before) rg-args))
           (when ctx-around (push (format "-C%d" ctx-around) rg-args))
           (push "--max-count=1000" rg-args))
          ("files_with_matches"
           (push "--files-with-matches" rg-args)
           (push "--sort=modified" rg-args))
          ("count"
           (push "--count" rg-args)))
        ;; Common flags
        (when case-fold (push "-i" rg-args))
        (when multiline (push "-U" rg-args) (push "--multiline-dotall" rg-args))
        (when file-glob (push (format "--glob=%s" file-glob) rg-args))
        (when file-type (push (format "--type=%s" file-type) rg-args))
        ;; Pattern and path
        (push "-e" rg-args)
        (push pattern rg-args)
        (push path rg-args)
        (setq rg-args (nreverse rg-args))
        (let ((exit-code (apply #'call-process "rg" nil '(t t) nil rg-args)))
          (cond
           ((= exit-code 0) nil)
           ((= exit-code 1)
            (erase-buffer)
            (insert "No matches found"))
           (t
            (goto-char (point-min))
            (insert (format "Error: search failed (exit code %d)\n\n"
                            exit-code))))))
      ;; Apply offset and head_limit
      (when (or (> offset 0) head-limit)
        (goto-char (point-min))
        (let ((total-lines (count-lines (point-min) (point-max))))
          (when (> offset 0)
            (forward-line offset)
            (delete-region (point-min) (point))
            (cl-decf total-lines offset))
          (when (and head-limit (> total-lines head-limit))
            (goto-char (point-min))
            (forward-line head-limit)
            (delete-region (point) (point-max))
            (goto-char (point-max))
            (insert (format "\n... Results truncated (limit: %d, offset: %d)"
                            head-limit offset)))))
      (funcall callback (buffer-string)))))


;;
;;; Pipeline-compatible handlers

(defun mevedel-tool-fs--write (callback args)
  "Write a file to the local filesystem.
CALLBACK receives the result string.  ARGS is a plist with :file_path
and :content."
  (let* ((file-path (plist-get args :file_path))
         (content (plist-get args :content))
         (full-path (expand-file-name file-path))
         (dir (file-name-directory full-path)))
    (unless (stringp file-path)
      (error "Parameter file_path is required"))
    (unless (stringp content)
      (error "Parameter content is required"))
    ;; Ensure parent directory exists
    (unless (file-directory-p dir)
      (condition-case err
          (make-directory dir t)
        (error (error "Could not create directory %s: %s"
                      dir (error-message-string err)))))
    (require 'mevedel-preview-mode)
    (condition-case err
        (let* ((temp-file (make-temp-file "mevedel-write-" nil nil content))
               (original-content (when (file-exists-p full-path)
                                   (with-temp-buffer
                                     (insert-file-contents full-path)
                                     (buffer-string)))))
          (mevedel-preview-mode-add-preview
           :temp-file temp-file
           :original-content original-content
           :path full-path
           :callback callback
           :tool-name "Write"
           ;; Write replaces the entire file -- no need for diff-apply
           :apply-fn (lambda ()
                       (copy-file temp-file full-path t))))
      (error
       (funcall callback (format "Error writing file %s: %s"
                                 file-path (error-message-string err)))))))

(defun mevedel-tool-fs--edit (callback args)
  "Edit a file by performing exact string replacement.
CALLBACK receives the result string.  ARGS is a plist with :file_path,
:old_string, :new_string, and optionally :replace_all."
  (let* ((file-path (plist-get args :file_path))
         (old-string (plist-get args :old_string))
         (new-string (plist-get args :new_string))
         (replace-all (plist-get args :replace_all))
         (full-path (expand-file-name file-path)))
    (unless (stringp file-path)
      (error "Parameter file_path is required"))
    (unless (stringp old-string)
      (error "Parameter old_string is required"))
    (unless (stringp new-string)
      (error "Parameter new_string is required"))
    (unless (file-exists-p full-path)
      (error "File does not exist: %s" file-path))
    (when (file-directory-p full-path)
      (error "Cannot edit a directory: %s" file-path))
    (when (string= old-string new-string)
      (error "old_string and new_string must be different"))
    (require 'mevedel-preview-mode)
    (condition-case err
        (let* ((temp-file (make-temp-file "mevedel-edit-"))
               (original-content (with-temp-buffer
                                   (insert-file-contents full-path)
                                   (buffer-string))))
          ;; Copy original to temp file
          (with-temp-file temp-file
            (insert original-content))
          ;; Apply replacement to temp file
          (mevedel-tool-fs--apply-string-replacement
           temp-file old-string new-string replace-all
           (lambda (success-or-error)
             (if (stringp success-or-error)
                 ;; Error -- clean up and report
                 (progn
                   (delete-file temp-file)
                   (funcall callback success-or-error))
               ;; Success -- show diff and confirm
               (mevedel-preview-mode-add-preview
                :temp-file temp-file
                :original-content original-content
                :path full-path
                :callback callback
                :tool-name "Edit")))))
      (error
       (funcall callback (format "Error editing file %s: %s"
                                 file-path (error-message-string err)))))))

(defun mevedel-tool-fs--apply-string-replacement (temp-file old-string new-string replace-all callback)
  "Apply string replacement to TEMP-FILE.
Replace OLD-STRING with NEW-STRING.  When REPLACE-ALL is non-nil and
not :json-false, replace all occurrences; otherwise require a unique
match.  Calls CALLBACK with t on success or an error string on failure."
  (condition-case err
      (let (success)
        (with-temp-buffer
          (insert-file-contents temp-file)
          (goto-char (point-min))
          (if (and replace-all (not (eq replace-all :json-false)))
              ;; Replace all occurrences
              (if (search-forward old-string nil t)
                  (progn
                    (replace-match new-string t t)
                    (while (search-forward old-string nil t)
                      (replace-match new-string t t))
                    (write-region nil nil temp-file nil 'silent)
                    (setq success t))
                (funcall callback
                         (format "Error: Could not find old_string in file: %s"
                                 (truncate-string-to-width old-string 40))))
            ;; Single unique replacement
            (if (search-forward old-string nil t)
                (if (save-excursion (search-forward old-string nil t))
                    (funcall callback
                             "Error: old_string is not unique in the file. Use replace_all or provide more context")
                  ;; Unique match found
                  (replace-match new-string t t)
                  ;; When new-string is empty and old-string didn't end with
                  ;; newline but was followed by one, strip trailing newline
                  (when (and (string-empty-p new-string)
                             (not (string-suffix-p "\n" old-string))
                             (eq (char-after) ?\n))
                    (delete-char 1))
                  (write-region nil nil temp-file nil 'silent)
                  (setq success t))
              (funcall callback
                       (format "Error: Could not find old_string in file: %s"
                               (truncate-string-to-width old-string 40))))))
        (when success
          (funcall callback success)))
    (error
     (funcall callback (format "Error: %s" (error-message-string err))))))


(defun mevedel-tool-fs--mkdir (callback args)
  "Create a directory at the given path.
CALLBACK receives the result string.  ARGS is a plist with :path."
  (let* ((path (plist-get args :path))
         (full-path (expand-file-name path)))
    (unless (stringp path)
      (error "Parameter path is required"))
    (condition-case err
        (progn
          (make-directory full-path t)
          (funcall callback (format "Directory created: %s" full-path)))
      (error
       (funcall callback (format "Error creating directory %s: %s"
                                 path (error-message-string err)))))))


;;
;;; Register Tools

(defun mevedel-tool-fs--register ()
  "Register file system tools for mevedel."

  (mevedel-define-tool
    :name "Glob"
    :description "Fast file pattern matching tool that works with any codebase size."
    :prompt-file "tools/glob.md"
    :handler #'mevedel-tool-fs--glob
    :args ((pattern string :required
                   "The glob pattern to match files against.")
           (path string :optional
                 "The directory to search in. If not specified, the current working directory will be used. Must be a valid directory path if provided.")
           (depth integer :optional
                 "Limit directory depth of search, 1 or higher. Defaults to no limit."))
    :async-p t
    :read-only-p t
    :groups (read)
    :get-path (lambda (args) (plist-get args :path)))

  (mevedel-define-tool
    :name "Read"
    :description "Read a file from the local filesystem."
    :prompt-file "tools/read.md"
    :handler #'mevedel-tool-fs--read-file
    :args ((file_path string :required "The absolute path to the file to read.")
           (offset integer :optional
                  "The line number to start reading from. Only provide if the file is too large to read at once.")
           (limit integer :optional
                 "The number of lines to read. Only provide if the file is too large to read at once."))
    :read-only-p t
    :groups (read)
    :get-path (lambda (args) (plist-get args :file_path)))

  (mevedel-define-tool
    :name "Grep"
    :description "Search file contents using ripgrep."
    :prompt-file "tools/grep.md"
    :handler #'mevedel-tool-fs--grep
    :args ((pattern string :required
                   "The regular expression pattern to search for in file contents.")
           (path string :optional
                 "File or directory to search in (rg PATH). Defaults to current working directory.")
           (glob string :optional
                 "Glob pattern to filter files (e.g. \"*.el\", \"*.{ts,tsx}\") -- maps to rg --glob.")
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
    :groups (read)
    :get-path (lambda (args) (plist-get args :path)))

  (mevedel-define-tool
    :name "MkDir"
    :description "Create a new directory at the given path."
    :prompt-file "tools/mkdir.md"
    :handler #'mevedel-tool-fs--mkdir
    :args ((path string :required
                 "The path of the directory to create."))
    :async-p t
    :groups (edit)
    :get-path (lambda (args) (plist-get args :path)))

  (mevedel-define-tool
    :name "Write"
    :description "Write a file to the local filesystem."
    :prompt-file "tools/write.md"
    :handler #'mevedel-tool-fs--write
    :args ((file_path string :required
                      "The absolute path to the file to write (must be absolute, not relative).")
           (content string :required
                   "The content to write to the file."))
    :async-p t
    :groups (edit)
    :get-path (lambda (args) (plist-get args :file_path)))

  (mevedel-define-tool
    :name "Edit"
    :description "Performs exact string replacements in files."
    :prompt-file "tools/edit.md"
    :handler #'mevedel-tool-fs--edit
    :args ((file_path string :required
                      "The absolute path to the file to modify.")
           (old_string string :required
                       "The text to replace.")
           (new_string string :required
                       "The text to replace it with (must be different from old_string).")
           (replace_all boolean :optional
                        "Replace all occurrences of old_string (default false)."))
    :async-p t
    :groups (edit)
    :get-path (lambda (args) (plist-get args :file_path)))

)

(provide 'mevedel-tool-fs)
;;; mevedel-tool-fs.el ends here
