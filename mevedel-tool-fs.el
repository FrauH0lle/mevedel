;;; mevedel-tool-fs.el -- File system tools -*- lexical-binding: t -*-

;;; Commentary:

;; File system tool implementations: Read, Glob, Grep, Write, Edit, Insert,
;; MkDir.  Also provides diff generation utilities and file snapshotting used
;; by the inline/buffer preview system.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `diff-mode'
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

(declare-function diff-setup-buffer-type "diff-mode" ())

;; `mevedel-chat'
(defvar mevedel--diff-preview-buffer-name)

;; Circular: mevedel-tool-fs <-> mevedel-preview-mode
(declare-function mevedel-preview-mode-add-preview "mevedel-preview-mode" t t)
(defvar mevedel-inline-preview-threshold)

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))

;; `mevedel-preview-mode'
(declare-function mevedel-preview-mode--effective-mode
                  "mevedel-preview-mode" ())

;; `mevedel-structs'
(defvar mevedel--workspace)
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

(defun mevedel-tool-fs--setup-diff-buffer (temp-file real-path workspace root
                                                     &optional chat-buffer labels-real)
  "Build a diff preview buffer comparing REAL-PATH to TEMP-FILE.

Creates and configures `mevedel--diff-preview-buffer-name' with the
unified diff, diff-mode, read-only, and the buffer-local variables that
`mevedel-ediff-patch' needs to resolve the source file and workspace.

Arguments:
- TEMP-FILE: Path to file with proposed changes
- REAL-PATH: Path to actual file
- WORKSPACE: Workspace identifier
- ROOT: Workspace root directory
- CHAT-BUFFER: Optional chat buffer reference
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
           ((and (or (not original-content) (string-empty-p original-content))
                 (and modified-content (not (string-empty-p modified-content))))
            (insert "new file mode 100644\n"))
           ((and (and original-content (not (string-empty-p original-content)))
                 (or (not modified-content) (string-empty-p modified-content)))
            (insert "deleted file mode 100644\n"))))
        (insert diff)
        (diff-mode)
        (read-only-mode 1)
        (setq-local truncate-lines t)
        (when (derived-mode-p 'diff-mode)
          (diff-setup-buffer-type))

        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--temp-file temp-file
                    mevedel--real-path real-path
                    mevedel--data-buffer chat-buffer)

        (goto-char (point-min))))
    diff-buffer))


;;
;;; Diff renderer (Edit / Write)

(defun mevedel-tool-fs--count-diff-changes (patch)
  "Return a cons `(ADDED . REMOVED)' of changed lines in unified diff PATCH.
Lines starting with `+' (but not `+++') count as added; lines starting
with `-' (but not `---') count as removed.  Non-string PATCH yields
`(0 . 0)'."
  (let ((added 0) (removed 0))
    (when (stringp patch)
      (dolist (line (split-string patch "\n"))
        (when (> (length line) 0)
          (let ((first (aref line 0)))
            (cond
             ((and (eq first ?+) (not (string-prefix-p "+++" line)))
              (cl-incf added))
             ((and (eq first ?-) (not (string-prefix-p "---" line)))
              (cl-incf removed)))))))
    (cons added removed)))

(defun mevedel-tool-fs--render-diff-summary (name args result render-data)
  "Build a collapsible diff rendering plist for Edit/Write results.

NAME is the tool name (\"Edit\" or \"Write\").  ARGS is the tool arg
plist.  RESULT is the LLM-facing result string (unused; accepted for
interface compatibility).  RENDER-DATA must be a plist of the form
  (:kind diff :patch PATCH :path PATH :rel-path REL)
emitted by `mevedel-preview-mode--apply-overlay'.

Returns a rendering plist `(:header :body :body-mode
:initially-collapsed-p)' or nil when RENDER-DATA is absent or malformed,
so the view falls back to the default one-liner.

The diff is kept collapsed under `default' / `plan' permission modes,
where the user already inspected and approved the change via the
interactive preview overlay.  Under the auto-apply modes
\(`accept-edits' / `trust-all') there was no preview, so the summary
starts expanded up to the window-height threshold so the user can see
what landed."
  (ignore result)
  (when (and (listp render-data)
             (eq (plist-get render-data :kind) 'diff)
             (stringp (plist-get render-data :patch)))
    (let* ((patch (plist-get render-data :patch))
           (rel-path (plist-get render-data :rel-path))
           (abs-path (or (plist-get render-data :path)
                         (plist-get args :file_path)))
           (shown (or rel-path
                      (and abs-path (file-name-nondirectory abs-path))
                      ""))
           (counts (mevedel-tool-fs--count-diff-changes patch))
           (header (format "%s: %s (+%d -%d)"
                           (or name "Edit")
                           shown
                           (car counts)
                           (cdr counts)))
           (auto-apply-p
            (memq (mevedel-preview-mode--effective-mode)
                  '(accept-edits trust-all))))
      (list :header header
            :body patch
            :body-mode 'diff-mode
            :initially-collapsed-p
            (if auto-apply-p
                (mevedel-view-collapse-by-height-p patch)
              t)))))

(defun mevedel-tool-fs--mode-for-file (path)
  "Return the major-mode symbol `auto-mode-alist' selects for PATH, or nil.
The returned mode is only used to fontify a temp buffer for read-only
display; modes that fail to load or error fall back to text verbatim
via `mevedel-view--fontify-as'."
  (when (and path (stringp path) (not (string-empty-p path)))
    (let ((mode (assoc-default path auto-mode-alist #'string-match)))
      (cond
       ((null mode) nil)
       ((symbolp mode) mode)
       ;; `auto-mode-alist' entries may be `(MODE . t)' pairs
       ((and (consp mode) (symbolp (car mode))) (car mode))
       (t nil)))))

(defun mevedel-tool-fs--render-read (name args result _render-data)
  "Rendering plist for the Read tool.
NAME is \"Read\".  ARGS carries `:file_path'.  RESULT is the line-numbered
file content.  Header shows the file basename and line count; body
fontifies as the file's natural mode when detectable from extension."
  (when (stringp result)
    (let* ((path (plist-get args :file_path))
           (shown (or (and path (file-name-nondirectory path)) "?"))
           (lines (length (split-string result "\n"))))
      (list :header (format "%s: %s (%d lines)" (or name "Read") shown lines)
            :body result
            :body-mode (mevedel-tool-fs--mode-for-file path)
            :initially-collapsed-p t))))

(defun mevedel-tool-fs--render-grep (name args result _render-data)
  "Rendering plist for the Grep tool.
NAME is \"Grep\".  ARGS carries `:pattern' (the search regex).  RESULT
is the raw matches output.  Header shows the pattern and match count
\(one line per match); body fontifies as `grep-mode' for file:line
coloring.  `grep-mode' is autoloaded; `mevedel-view--fontify-as' falls
back to text verbatim if activation fails."
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (matches (length (seq-filter (lambda (l) (not (string-empty-p l)))
                                        (split-string result "\n")))))
      (list :header (format "%s: %s (%d matches)"
                            (or name "Grep") pattern matches)
            :body result
            :body-mode 'grep-mode
            :initially-collapsed-p t))))

(defun mevedel-tool-fs--render-glob (name args result _render-data)
  "Rendering plist for the Glob tool.
NAME is \"Glob\".  ARGS carries `:pattern'.  RESULT is a newline-separated
list of matching files.  Header shows pattern and file count."
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (files (length (seq-filter (lambda (l) (not (string-empty-p l)))
                                      (split-string result "\n")))))
      (list :header (format "%s: %s (%d files)"
                            (or name "Glob") pattern files)
            :body result
            :body-mode nil
            :initially-collapsed-p t))))


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

(defconst mevedel-tool-fs--grep-max-output-bytes (* 200 1024)
  "Hard cap on Grep tool output size in bytes.
Prevents catastrophic context overflow when searches hit files with
very long lines (e.g. JSON log files where a single match line can be
50KB+).  After line-count truncation, output exceeding this limit is
cut at the last complete line and a guidance message is appended.")

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

(defun mevedel-tool-fs--list-directory (path &optional max-entries)
  "List files under directory PATH, respecting .gitignore.

Uses `rg --files --hidden --follow' so the listing is gitignore-aware
and stable across runs (entries are sorted by path).  Returns a cons
cell (ENTRIES . TRUNCATED-P) where ENTRIES is a list of paths relative
to PATH and TRUNCATED-P is non-nil if the listing was capped at
MAX-ENTRIES (defaulting to 1000).  Signals an error if rg is missing,
PATH is not a readable directory, or rg exits with an unexpected code."
  (let ((max (or max-entries 1000)))
    (unless (executable-find "rg")
      (error "`ripgrep' not installed"))
    (unless (and (file-directory-p path) (file-readable-p path))
      (error "%s is not a readable directory" path))
    (with-temp-buffer
      (let* ((default-directory (file-name-as-directory path))
             (exit (call-process "rg" nil t nil
                                 "--files" "--hidden" "--follow"
                                 "--sort" "path" ".")))
        (cond
         ((= exit 0)
          (let* ((raw (split-string (buffer-string) "\n" t))
                 (all (mapcar (lambda (s)
                                (if (string-prefix-p "./" s) (substring s 2) s))
                              raw))
                 (truncated (> (length all) max))
                 (entries (if truncated (seq-take all max) all)))
            (cons entries truncated)))
         ((= exit 1) (cons nil nil))
         (t (error "rg exited with code %d listing %s" exit path)))))))

(defun mevedel-tool-fs--slurp-file-contents (path &optional offset limit)
  "Read file PATH with OFFSET and LIMIT, applying Read-tool safety checks.

Validates readability, rejects directories, binary files, and blocking
device paths.  Resolves symlinks.  For full-file reads (both OFFSET and
LIMIT nil), enforces the 512 KB size cap.  For range reads, OFFSET
defaults to 1 and LIMIT to 2000 lines.

Returns the content string with line numbers; signals an error on any
validation failure.  Callers that want graceful degradation should wrap
in `condition-case'."
  (unless (file-readable-p path)
    (error "File %s is not readable" path))
  (when (file-directory-p path)
    (error "Cannot read directory %s as file" path))
  (when (file-symlink-p path)
    (setq path (file-truename path)))
  (when (mevedel-tool-fs--binary-extension-p path)
    (let ((ext (file-name-extension path)))
      (error "Cannot read binary file (type: .%s): %s" ext path)))
  (when (mevedel-tool-fs--blocked-device-p path)
    (error "Cannot read %s: device file would block or produce infinite output"
           path))
  (if (zerop (file-attribute-size (file-attributes path)))
      ""
    (let ((start-line (max 1 (or offset 1)))
          (num-lines (or limit 2000)))
      (if (and (not offset) (not limit))
          (let ((file-size (file-attribute-size (file-attributes path))))
            (when (> file-size (* 512 1024))
              (error "File is too large (> 512 KB).  Use offset and limit to read specific portions"))
            (with-temp-buffer
              (insert-file-contents path)
              (mevedel-tool-fs--add-line-numbers 1)
              (buffer-substring-no-properties (point-min) (point-max))))
        (let* ((file-size (file-attribute-size (file-attributes path)))
               (chunk-size (min file-size (* 512 1024)))
               (byte-offset 0)
               (lines-to-skip (1- start-line))
               (lines-to-read num-lines))
          (with-temp-buffer
            (while (and (> lines-to-skip 0)
                        (< byte-offset file-size))
              (insert-file-contents
               path nil byte-offset (+ byte-offset chunk-size))
              (setq byte-offset (+ byte-offset chunk-size))
              (setq lines-to-skip (forward-line lines-to-skip))
              (when (eobp)
                (when (/= (line-beginning-position) (line-end-position))
                  (cl-incf lines-to-skip))
                (delete-region (point-min) (line-beginning-position))))
            (delete-region (point-min) (point))
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
                     path nil byte-offset (+ byte-offset chunk-size))
                    (setq byte-offset (+ byte-offset chunk-size))))))
            (mevedel-tool-fs--add-line-numbers start-line)
            (buffer-substring-no-properties (point-min) (point-max))))))))

(defun mevedel-tool-fs--read-file (args)
  "Read file contents.
ARGS is a plist with :file_path and optional :offset, :limit."
  (let* ((filename (plist-get args :file_path))
         (offset (plist-get args :offset))
         (limit (plist-get args :limit)))
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
    (cond
     ((and (bound-and-true-p mevedel--session)
           (mevedel-session-read-is-duplicate-p
            mevedel--session filename offset limit))
      (format "File %s unchanged since last read.  Reuse the previous contents."
              filename))
     ((zerop (file-attribute-size (file-attributes filename)))
      (when (bound-and-true-p mevedel--session)
        (mevedel-session-record-file-access
         mevedel--session filename 'read offset limit))
      (format "<system-reminder>\n\
File %s exists but is empty (0 bytes). This is the actual file \
content, not a read failure.\n</system-reminder>" filename))
     (t
      (let ((content (mevedel-tool-fs--slurp-file-contents filename offset limit)))
        (when (bound-and-true-p mevedel--session)
          (mevedel-session-record-file-access
           mevedel--session filename 'read offset limit))
        content)))))

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
         (case-fold (mevedel-tool-truthy-p (plist-get args :-i)))
         (line-numbers (let ((v (plist-get args :-n)))
                         (if (null v)
                             (equal output-mode "content")
                           (mevedel-tool-truthy-p v))))
         (file-type (plist-get args :type))
         (multiline (mevedel-tool-truthy-p (plist-get args :multiline)))
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
           (push "--max-count=1000" rg-args)
           ;; Truncate long lines to prevent log files and other
           ;; long-line sources from blowing up tool result size.
           (push "--max-columns=2000" rg-args)
           (push "--max-columns-preview" rg-args))
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
      ;; Hard cap on total output size to prevent context overflow.
      ;; Even with line-count and per-line truncation, results can be
      ;; dangerously large (e.g. log files with many short matches).
      (when (> (buffer-size) mevedel-tool-fs--grep-max-output-bytes)
        (goto-char (point-min))
        (forward-char mevedel-tool-fs--grep-max-output-bytes)
        ;; Truncate at the last complete line within the budget.
        (beginning-of-line)
        (delete-region (point) (point-max))
        (goto-char (point-max))
        (insert (format "\n... Output truncated at %dK byte limit. \
Narrow your search with :glob, :type, or a more specific :pattern."
                        (/ mevedel-tool-fs--grep-max-output-bytes 1024))))
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
Replace OLD-STRING with NEW-STRING.  When REPLACE-ALL is truthy (per
`mevedel-tool-truthy-p'), replace all occurrences; otherwise require a
unique match.  Calls CALLBACK with t on success or an error string on
failure."
  (condition-case err
      (let (success)
        (with-temp-buffer
          (insert-file-contents temp-file)
          (goto-char (point-min))
          (if (mevedel-tool-truthy-p replace-all)
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
    :max-result-size 30000
    :groups (read)
    :get-path (lambda (args) (plist-get args :path))
    :renderer #'mevedel-tool-fs--render-glob)

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
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-fs--render-read)

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
    :max-result-size 20000
    :groups (read)
    :get-path (lambda (args) (plist-get args :path))
    :renderer #'mevedel-tool-fs--render-grep)

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
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-fs--render-diff-summary)

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
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-fs--render-diff-summary)

)

(provide 'mevedel-tool-fs)
;;; mevedel-tool-fs.el ends here
