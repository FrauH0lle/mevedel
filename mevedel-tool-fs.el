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
(declare-function mevedel-tool-string-arg "mevedel-tool-registry"
                  (args key &optional default))
(declare-function mevedel-tool-integer-arg "mevedel-tool-registry"
                  (args key &optional default))

(declare-function diff-setup-buffer-type "diff-mode" ())

;; `mevedel-chat'
(defvar mevedel--diff-preview-buffer-name)

;; Circular: mevedel-tool-fs <-> mevedel-preview-mode
(declare-function mevedel-preview-mode-add-preview "mevedel-preview-mode" t t)

;; `mevedel-structs'
(defvar mevedel--workspace)
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--agent-invocation)
(defvar mevedel--data-buffer)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-file-snapshots "mevedel-structs" (cl-x) t)

;; `mevedel-file-state'
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind &optional offset limit))
(declare-function mevedel-session-read-is-duplicate-p
                  "mevedel-file-state" (session path offset limit))

;; `gptel-request'
(declare-function gptel--model-capable-p "ext:gptel-request"
                  (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request"
                  (mime &optional model))
(declare-function gptel-anthropic-p "ext:gptel-anthropic" (cl-x) t)
(declare-function gptel-bedrock-p "ext:gptel-bedrock" (cl-x) t)
(defvar gptel-backend)

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--tool-results-dir
                  "mevedel-pipeline" (session buffer))


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
unified diff, `diff-mode', read-only, and the buffer-local variables that
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

The diff starts collapsed in every permission mode so Edit/Write
results do not expand the transcript by default."
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
                           (cdr counts))))
      (list :header header
            :body patch
            :body-mode 'diff-mode
            :initially-collapsed-p t))))

(defun mevedel-tool-fs--mode-for-file (path)
  "Return the major-mode symbol `auto-mode-alist' would select for PATH, or nil.
The returned mode is only used to fontify a temp buffer for read-only
display; modes that fail to load or error fall back to text verbatim
via `mevedel-view--fontify-as'."
  (when (and path
             (stringp path)
             (not (string-empty-p path))
             (not (mevedel-tool-fs--media-mime-type path)))
    (let ((mode (assoc-default path auto-mode-alist #'string-match)))
      (cond
       ((null mode) nil)
       ((symbolp mode) mode)
       ;; `auto-mode-alist' entries may be `(MODE . t)' pairs
       ((and (consp mode) (symbolp (car mode))) (car mode))
       (t nil)))))

(defun mevedel-tool-fs--current-workspace-root ()
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

(defun mevedel-tool-fs--display-path (path)
  "Return PATH as a compact display path for tool headers."
  (or (when (and (stringp path)
                 (not (string-empty-p path)))
        (let* ((root (mevedel-tool-fs--current-workspace-root))
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

(defun mevedel-tool-fs--strip-system-reminders (result)
  "Return RESULT without a trailing appended system-reminder block."
  (if (stringp result)
      (replace-regexp-in-string
       "\n\n<system-reminder>\n\\(?:.\\|\n\\)*?</system-reminder>\\'"
       "" result t)
    result))

(defun mevedel-tool-fs--render-read (name args result _render-data)
  "Rendering plist for the Read tool.
NAME is \"Read\".  ARGS carries `:file_path'.  RESULT is the line-numbered
file content.  Header shows the file basename and line count; body
fontifies as the file's natural mode when detectable from extension."
  (when (stringp result)
    (let* ((path (plist-get args :file_path))
           (shown (mevedel-tool-fs--display-path path))
           (visible (mevedel-tool-fs--strip-system-reminders result))
           (lines (length (split-string visible "\n"))))
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
           (visible (mevedel-tool-fs--strip-system-reminders result))
           (matches (if (or (string-prefix-p "No matches found" visible)
                            (string-prefix-p "Error:" visible))
                        0
                      (length (seq-filter (lambda (l) (not (string-empty-p l)))
                                          (split-string visible "\n"))))))
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
           (lines (seq-filter (lambda (l) (not (string-empty-p l)))
                              (split-string result "\n")))
           (files (if (or (string-prefix-p "No files found" result)
                          (string-prefix-p "Error:" result))
                      0
                    (length (seq-filter
                             (lambda (line)
                               (not (string-prefix-p "... Results truncated" line)))
                             lines)))))
      (list :header (format "%s: %s (%d files)"
                            (or name "Glob") pattern files)
            :body result
            :body-mode nil
            :initially-collapsed-p t))))

(defun mevedel-tool-fs--render-mkdir (name args result render-data)
  "Rendering plist for the MkDir tool.
NAME is the tool display name.  ARGS carries `:path'.
RESULT is the tool result string.
RENDER-DATA carries normalized path metadata when available.
Header shows the directory path with a created, exists, or error suffix."
  (when (stringp result)
    (let* ((rel-path (plist-get render-data :rel-path))
           (raw-path (or rel-path
                         (plist-get render-data :path)
                         (plist-get args :path)
                         "?"))
           (raw-path (if (and (stringp rel-path)
                              (not (string-search "/" rel-path)))
                         (concat "./" rel-path)
                       raw-path))
           (shown (if (stringp raw-path)
                      (file-name-as-directory raw-path)
                    "?"))
           (status
            (cond
             ((string-prefix-p "Error" result) "error")
             ((and (listp render-data)
                   (eq (plist-get render-data :kind) 'mkdir))
              (if (plist-get render-data :created) "created" "exists"))
             ((string-match-p "already exists" result) "exists")
             (t "created"))))
      (list :header (format "%s: %s (%s)"
                            (or name "MkDir")
                            shown
                            status)
            :body nil
            :body-mode nil
            :expandable-p nil
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
Stores nil for ORIGINAL-CONTENT if file doesn't exist yet.

Populates both the legacy buffer-local alist
`mevedel--request-file-snapshots' (consumed by
`mevedel--generate-final-patch') and the active request struct's
`:file-snapshots' hash table (consumed by the session-persistence
file-history store)."
  (when (and filepath (stringp filepath))
    (let ((abs-path (expand-file-name filepath)))
      (unless (assoc abs-path mevedel--request-file-snapshots)
        (let ((original (when (file-exists-p abs-path)
                          (with-temp-buffer
                            (insert-file-contents abs-path)
                            (buffer-string)))))
          (push (cons abs-path original) mevedel--request-file-snapshots)
          (when (and (boundp 'mevedel--current-request)
                     mevedel--current-request)
            (let ((ht (mevedel-request-file-snapshots mevedel--current-request)))
              (when (hash-table-p ht)
                (puthash abs-path original ht)))))))))


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

(defconst mevedel-tool-fs--media-mime-by-extension
  '(("pdf" . "application/pdf")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp"))
  "Read-tool media MIME types keyed by lowercase file extension.")

(defconst mevedel-tool-fs--media-max-bytes (* 10 1024 1024)
  "Maximum media file size Read will base64-encode directly.")

(defconst mevedel-tool-fs--pdf-pages-max-base64-chars (* 10 1024 1024)
  "Maximum aggregate base64 payload size for one PDF page extraction.")

(defconst mevedel-tool-fs--read-max-pages 20
  "Maximum number of PDF pages a single Read call may extract.")

(defconst mevedel-tool-fs--large-attachment-reminder-bytes (* 1024 1024)
  "Minimum PDF attachment size that gets bounded-page guidance.")

(defun mevedel-tool-fs--agent-context-p ()
  "Return non-nil when the current tool call is inside a sub-agent.

Sub-agents share the parent session for permissions, but their LLM
context is separate.  A parent-session Read dedup entry therefore
must not suppress content inside a fresh agent transcript, and an
agent Read must not poison the parent's later Read calls."
  (and (boundp 'mevedel--agent-invocation)
       mevedel--agent-invocation))

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

(defconst mevedel-tool-fs--glob-default-head-limit 100
  "Default number of file paths returned by one Glob call.")

(defun mevedel-tool-fs--binary-extension-p (filename)
  "Return non-nil if FILENAME has a binary file extension."
  (member (downcase (or (file-name-extension filename) ""))
          mevedel-tool-fs--binary-extensions))

(defun mevedel-tool-fs--media-mime-type (filename)
  "Return supported media MIME type for FILENAME, or nil."
  (cdr (assoc (downcase (or (file-name-extension filename) ""))
              mevedel-tool-fs--media-mime-by-extension)))

(defun mevedel-tool-fs--image-media-p (filename)
  "Return non-nil when FILENAME is a supported image media file."
  (let ((mime (mevedel-tool-fs--media-mime-type filename)))
    (and mime (string-prefix-p "image/" mime))))

(defun mevedel-tool-fs--pdf-media-p (filename)
  "Return non-nil when FILENAME is a supported PDF media file."
  (equal (mevedel-tool-fs--media-mime-type filename) "application/pdf"))

(defun mevedel-tool-fs--blocked-device-p (filename)
  "Return non-nil if FILENAME is a blocked device path."
  (or (member filename mevedel-tool-fs--blocked-device-paths)
      (and (string-prefix-p "/proc/" filename)
           (or (string-suffix-p "/fd/0" filename)
               (string-suffix-p "/fd/1" filename)
               (string-suffix-p "/fd/2" filename)))))

(defun mevedel-tool-fs--native-media-backend-p ()
  "Return non-nil when the active backend supports media in tool results."
  (and (boundp 'gptel-backend)
       gptel-backend
       (or (and (fboundp 'gptel-anthropic-p)
                (gptel-anthropic-p gptel-backend))
           (and (fboundp 'gptel-bedrock-p)
                (gptel-bedrock-p gptel-backend)))))

(defun mevedel-tool-fs--ensure-media-capable (mime)
  "Signal an error unless the current gptel model supports MIME media input."
  (unless (and (fboundp 'gptel--model-capable-p)
               (gptel--model-capable-p 'media))
    (error "Current model does not support media input"))
  (when (and mime (fboundp 'gptel--model-mime-capable-p)
             (not (gptel--model-mime-capable-p mime)))
    (error "Current model does not support media type %s" mime)))

(defun mevedel-tool-fs--file-bytes-prefix-p (path prefix)
  "Return non-nil when PATH begins with byte string PREFIX."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (length prefix))
    (string-equal (buffer-string) prefix)))

(defun mevedel-tool-fs--file-bytes-at-p (path offset expected)
  "Return non-nil when PATH has EXPECTED byte string at OFFSET."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil offset (+ offset (length expected)))
    (string-equal (buffer-string) expected)))

(defun mevedel-tool-fs--valid-media-file-p (path mime)
  "Return non-nil when PATH contents match supported media MIME."
  (and (> (file-attribute-size (file-attributes path)) 0)
       (pcase mime
         ("application/pdf"
          (mevedel-tool-fs--file-bytes-prefix-p path "%PDF-"))
         ("image/png"
          (mevedel-tool-fs--file-bytes-prefix-p
           path (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)))
         ("image/jpeg"
          (mevedel-tool-fs--file-bytes-prefix-p
           path (unibyte-string #xff #xd8 #xff)))
         ("image/gif"
          (or (mevedel-tool-fs--file-bytes-prefix-p path "GIF87a")
              (mevedel-tool-fs--file-bytes-prefix-p path "GIF89a")))
         ("image/webp"
          (and (mevedel-tool-fs--file-bytes-prefix-p path "RIFF")
               (mevedel-tool-fs--file-bytes-at-p path 8 "WEBP")))
         (_ nil))))

(defun mevedel-tool-fs--validate-media-file (path mime)
  "Signal an error unless PATH contents match supported media MIME."
  (unless (> (file-attribute-size (file-attributes path)) 0)
    (error "Media file is empty: %s" path))
  (unless (mevedel-tool-fs--valid-media-file-p path mime)
    (error "File contents do not match media type %s: %s" mime path)))

(defun mevedel-tool-fs--parse-pages (pages)
  "Parse Read PAGES string into a cons cell (START . END).

Valid forms are \"3\", \"1-5\", and \"3-\".  Open-ended ranges are
capped to `mevedel-tool-fs--read-max-pages'.  Signals an error for
invalid forms or ranges over the per-request page limit."
  (unless (and (stringp pages) (not (string-empty-p pages)))
    (error "Parameter pages must be a non-empty string"))
  (let ((trimmed (string-trim pages))
        start end)
    (cond
     ((string-match "\\`\\([0-9]+\\)\\'" trimmed)
      (setq start (string-to-number (match-string 1 trimmed))
            end start))
     ((string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" trimmed)
      (setq start (string-to-number (match-string 1 trimmed))
            end (string-to-number (match-string 2 trimmed))))
     ((string-match "\\`\\([0-9]+\\)-\\'" trimmed)
      (setq start (string-to-number (match-string 1 trimmed))
            end (+ start (1- mevedel-tool-fs--read-max-pages))))
     (t
      (error "Invalid pages value %S; use forms like \"3\", \"1-5\", or \"3-\""
             pages)))
    (when (< start 1)
      (error "PDF page numbers start at 1"))
    (when (< end start)
      (error "PDF page range must not end before it starts"))
    (when (> (1+ (- end start)) mevedel-tool-fs--read-max-pages)
      (error "PDF page range exceeds %d pages" mevedel-tool-fs--read-max-pages))
    (cons start end)))

(defun mevedel-tool-fs--base64-file (path &optional max-bytes)
  "Return base64 contents of PATH after enforcing MAX-BYTES."
  (let ((size (file-attribute-size (file-attributes path)))
        (cap (or max-bytes mevedel-tool-fs--media-max-bytes)))
    (when (> size cap)
      (error "Media file is too large (%d bytes > %d bytes): %s"
             size cap path))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (base64-encode-region (point-min) (point-max) :no-line-break)
      (buffer-string))))

(defun mevedel-tool-fs--tool-results-dir ()
  "Return a writable directory for Read-generated media artifacts."
  (let* ((buffer (if (and (boundp 'mevedel--data-buffer)
                          mevedel--data-buffer
                          (buffer-live-p mevedel--data-buffer))
                     mevedel--data-buffer
                   (current-buffer)))
         (dir (and (bound-and-true-p mevedel--session)
                   (fboundp 'mevedel-pipeline--tool-results-dir)
                   (mevedel-pipeline--tool-results-dir
                    mevedel--session buffer))))
    (unless dir
      (setq dir (file-name-concat temporary-file-directory
                                  "mevedel-tool-results")))
    (make-directory dir t)
    dir))

(defun mevedel-tool-fs--imagemagick-command ()
  "Return an ImageMagick executable name, preferring magick over convert."
  (or (executable-find "magick")
      (executable-find "convert")))

(defun mevedel-tool-fs--call-process-capturing-output (program &rest args)
  "Call PROGRAM with ARGS, returning (EXIT-CODE . OUTPUT)."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process program nil (list t t) nil args)))
      (cons exit-code (string-trim (buffer-string))))))

(defun mevedel-tool-fs--file-size (path)
  "Return PATH's size in bytes, or nil when unavailable."
  (ignore-errors
    (file-attribute-size (file-attributes path))))

(defun mevedel-tool-fs--format-byte-size (bytes)
  "Return BYTES formatted for a compact reminder."
  (cond
   ((not (numberp bytes)) "unknown size")
   ((>= bytes (* 1024 1024))
    (format "%.1f MB" (/ bytes 1048576.0)))
   ((>= bytes 1024)
    (format "%.1f KB" (/ bytes 1024.0)))
   (t (format "%d bytes" bytes))))

(defun mevedel-tool-fs--open-ended-pages-p (pages)
  "Return non-nil when PAGES is an open-ended PDF page range."
  (and (stringp pages)
       (string-match-p "\\`[[:space:]]*[0-9]+-[[:space:]]*\\'" pages)))

(defun mevedel-tool-fs--pdf-page-count (path)
  "Return PDF PATH's page count when `pdfinfo' can determine it."
  (when (executable-find "pdfinfo")
    (pcase-let ((`(,exit-code . ,output)
                 (mevedel-tool-fs--call-process-capturing-output
                  "pdfinfo" path)))
      (when (and (zerop exit-code)
                 (string-match "^Pages:[[:space:]]+\\([0-9]+\\)" output))
        (string-to-number (match-string 1 output))))))

(defun mevedel-tool-fs--large-pdf-p (path)
  "Return non-nil when PDF PATH should get bounded-page guidance."
  (and (mevedel-tool-fs--pdf-media-p path)
       (let ((page-count (mevedel-tool-fs--pdf-page-count path))
             (size (mevedel-tool-fs--file-size path)))
         (or (and page-count (> page-count mevedel-tool-fs--read-max-pages))
             (and size
                  (> size mevedel-tool-fs--large-attachment-reminder-bytes))))))

(defun mevedel-tool-fs--format-large-pdf-reminder (path)
  "Return model-visible guidance for a large PDF at PATH."
  (let* ((page-count (mevedel-tool-fs--pdf-page-count path))
         (size (mevedel-tool-fs--file-size path))
         (details (delq nil
                        (list (and size
                                   (format "%s"
                                           (mevedel-tool-fs--format-byte-size
                                            size)))
                              (and page-count
                                   (format "%d pages" page-count))))))
    (format "PDF `%s` is large%s. Prefer bounded `Read(file_path=\"%s\", pages=\"START-END\")` requests for relevant pages instead of rereading or reattaching the whole document. Each PDF page request is capped at %d pages; use page selectors like \"1-5\" or \"6-\" when you need the next chunk."
            path
            (if details
                (format " (%s)" (string-join details ", "))
              "")
            path
            mevedel-tool-fs--read-max-pages)))

(defun mevedel-tool-fs--append-system-reminder (result body)
  "Append BODY as a system reminder to RESULT.
RESULT may be a string or a plist carrying `:result'."
  (let ((block (format "\n\n<system-reminder>\n%s\n</system-reminder>"
                       body)))
    (cond
     ((and (listp result)
           (plist-member result :result)
           (stringp (plist-get result :result)))
      (plist-put (copy-sequence result)
                 :result
                 (concat (plist-get result :result) block)))
     ((stringp result) (concat result block))
     (t result))))

(defun mevedel-tool-fs--bounded-pdf-page-range (path pages)
  "Return requested PAGES for PATH, bounded by actual page count when known."
  (let ((range (mevedel-tool-fs--parse-pages pages)))
    (if-let* ((page-count (mevedel-tool-fs--pdf-page-count path)))
        (let ((start (car range)))
          (when (< page-count start)
            (error "PDF page range starts after last page (%d)" page-count))
          (cons start (min (cdr range) page-count)))
      range)))

(defun mevedel-tool-fs--media-transform-requested-p (args)
  "Return non-nil when ARGS requests image resizing or compression."
  (or (plist-get args :max_width)
      (plist-get args :max_height)
      (plist-get args :max_tokens)))

(defun mevedel-tool-fs--positive-integer-or-nil (value name)
  "Validate VALUE as a positive integer for NAME, allowing nil."
  (when value
    (unless (and (integerp value) (> value 0))
      (error "Parameter %s must be a positive integer" name)))
  value)

(defun mevedel-tool-fs--maybe-transform-media (path args)
  "Return media PATH, optionally transformed per Read ARGS.

The returned value is a cons cell (PATH . MIME).  If ARGS contains
`:max_width', `:max_height', or `:max_tokens', ImageMagick is required."
  (let ((mime (mevedel-tool-fs--media-mime-type path)))
    (if (not (mevedel-tool-fs--media-transform-requested-p args))
        (cons path mime)
      (let* ((max-width (mevedel-tool-fs--positive-integer-or-nil
                         (plist-get args :max_width) "max_width"))
             (max-height (mevedel-tool-fs--positive-integer-or-nil
                          (plist-get args :max_height) "max_height"))
             (max-tokens (mevedel-tool-fs--positive-integer-or-nil
                          (plist-get args :max_tokens) "max_tokens"))
             (cmd (mevedel-tool-fs--imagemagick-command)))
        (unless cmd
          (error "ImageMagick not installed; install `magick' or `convert' to use max_width, max_height, or max_tokens"))
        (let* ((output-ext (if max-tokens "jpg"
                             (downcase (or (file-name-extension path) "png"))))
               (output-mime (if max-tokens "image/jpeg" mime))
               (output (make-temp-file
                        (file-name-concat
                         (mevedel-tool-fs--tool-results-dir)
                         "Read-image-")
                        nil (concat "." output-ext)))
               (resize (cond
                        ((and max-width max-height)
                         (format "%dx%d>" max-width max-height))
                        (max-width (format "%dx>" max-width))
                        (max-height (format "x%d>" max-height))))
               (target-kb (and max-tokens
                               (max 1 (/ (* max-tokens 150) 1024))))
               (im-args (append (list path "-auto-orient")
                                (when resize (list "-resize" resize))
                                (list "-strip")
                                (if max-tokens
                                    (list "-quality" "85"
                                          "-define"
                                          (format "jpeg:extent=%dkb" target-kb))
                                  (list "-quality" "85"))
                                (list output))))
          (pcase-let ((`(,exit-code . ,process-output)
                       (apply #'mevedel-tool-fs--call-process-capturing-output
                              cmd im-args)))
            (unless (zerop exit-code)
              (error "ImageMagick failed while preparing media file%s"
                     (if (string-empty-p process-output)
                         ""
                       (concat ": " process-output)))))
          (cons output output-mime))))))

(defun mevedel-tool-fs--format-media-result (path mime base64 &optional source)
  "Format a model-visible media envelope for PATH, MIME, and BASE64.
SOURCE is the original source path when PATH points to a generated file."
  (let ((size (file-attribute-size (file-attributes path))))
    (concat "<media-file>\n"
            (format "path: %s\n" path)
            (when source (format "source: %s\n" source))
            (format "mime_type: %s\n" mime)
            (format "size_bytes: %d\n" size)
            "encoding: base64\n"
            "data:\n"
            base64
            "\n</media-file>")))

(defun mevedel-tool-fs--media-read-result (text media)
  "Return Read media TEXT with MEDIA side-channel data."
  (list :result text :media media))

(defun mevedel-tool-fs--media-dedup-key (args)
  "Return a stable key for media Read options in ARGS."
  (list :media
        :pages (plist-get args :pages)
        :max-width (plist-get args :max_width)
        :max-height (plist-get args :max_height)
        :max-tokens (plist-get args :max_tokens)))

(defun mevedel-tool-fs--media-result-mime (path args)
  "Return the MIME type expected from a media Read of PATH with ARGS."
  (let ((mime (mevedel-tool-fs--media-mime-type path)))
    (cond
     ((and (equal mime "application/pdf") (plist-get args :pages))
      (if (plist-get args :max_tokens) "image/jpeg" "image/png"))
     ((and mime (string-prefix-p "image/" mime)
           (plist-get args :max_tokens))
      "image/jpeg")
     (t mime))))

(defun mevedel-tool-fs--text-range-requested-p (offset limit)
  "Return non-nil when OFFSET or LIMIT asks for a text line range.
Treat zero as absent because some model tool calls supply optional
integer defaults even when the user did not request a text range.
Treat LIMIT 2000 as absent because it is Read's documented default
text limit and may be sent by models as a defaulted optional value."
  (or (and offset (not (equal offset 0)))
      (and limit (not (member limit '(0 2000))))))

(defun mevedel-tool-fs--normalize-read-args (args)
  "Return ARGS normalized for Read handling."
  (let ((normalized (copy-sequence args)))
    (when-let* ((file-path (plist-get normalized :file_path)))
      (when (stringp file-path)
        (plist-put normalized :file_path
                   (expand-file-name (substitute-in-file-name file-path)))))
    (when (equal (plist-get normalized :pages) "")
      (plist-put normalized :pages nil))
    (dolist (key '(:offset :limit :max_width :max_height :max_tokens))
      (when (equal (plist-get normalized key) 0)
        (plist-put normalized key nil)))
    normalized))

(defun mevedel-tool-fs--normalize-media-args (args)
  "Return ARGS normalized for media Read handling."
  (mevedel-tool-fs--normalize-read-args args))

(defun mevedel-tool-fs--read-media-file (path args)
  "Read supported media PATH according to ARGS."
  (let ((mime (mevedel-tool-fs--media-mime-type path)))
    (cond
     ((equal mime "application/pdf")
      (if-let* ((pages (plist-get args :pages)))
          (mevedel-tool-fs--read-pdf-pages path pages args)
        (when (mevedel-tool-fs--media-transform-requested-p args)
          (error "'max_width', 'max_height', and 'max_tokens' are only supported for image files and PDF page images"))
        (mevedel-tool-fs--ensure-media-capable mime)
        (mevedel-tool-fs--validate-media-file path mime)
        (let* ((base64 (mevedel-tool-fs--base64-file path))
               (media (list (list :path path :mime "application/pdf"
                                  :kind 'document
                                  :data base64))))
          (mevedel-tool-fs--media-read-result
           (mevedel-tool-fs--format-media-result
            path "application/pdf" base64)
           media))))
     ((and mime (string-prefix-p "image/" mime))
      (mevedel-tool-fs--validate-media-file path mime)
      (let* ((prepared (mevedel-tool-fs--maybe-transform-media path args))
             (prepared-path (car prepared))
             (prepared-mime (cdr prepared))
             (_ (mevedel-tool-fs--ensure-media-capable prepared-mime))
             (_ (mevedel-tool-fs--validate-media-file
                 prepared-path prepared-mime))
             (base64 (mevedel-tool-fs--base64-file prepared-path))
             (media (list (list :path prepared-path :mime prepared-mime
                                :kind 'image
                                :data base64
                                :source (unless (equal prepared-path path)
                                          path)))))
        (mevedel-tool-fs--media-read-result
         (mevedel-tool-fs--format-media-result
          prepared-path prepared-mime base64
          (unless (equal prepared-path path) path))
         media)))
     (t
      (error "Unsupported media file type: %s" path)))))

(defun mevedel-tool-fs--read-pdf-pages (path pages args)
  "Render PDF PATH PAGES to images according to ARGS.
Return a media result plist."
  (let ((range (mevedel-tool-fs--bounded-pdf-page-range path pages)))
    (unless (executable-find "pdftoppm")
      (error "'pdftoppm' not installed; install 'poppler-utils' to read PDF pages as images"))
    (mevedel-tool-fs--validate-media-file path "application/pdf")
    (mevedel-tool-fs--ensure-media-capable nil)
    (let ((results nil)
          (media nil)
          (total-base64-chars 0))
      (dotimes (i (1+ (- (cdr range) (car range))))
        (let* ((page (+ (car range) i))
               (prefix (make-temp-name
                        (file-name-concat
                         (mevedel-tool-fs--tool-results-dir)
                         (format "Read-pdf-page-%d-" page))))
               (output (concat prefix ".png")))
          (pcase-let ((`(,exit-code . ,process-output)
                       (mevedel-tool-fs--call-process-capturing-output
                        "pdftoppm"
                        "-f" (number-to-string page)
                        "-l" (number-to-string page)
                        "-singlefile"
                        "-png" path prefix)))
            (unless (zerop exit-code)
              (error "'pdftoppm' failed while rendering page %d of %s%s"
                     page path
                     (if (string-empty-p process-output)
                         ""
                       (concat ": " process-output)))))
          (let* ((prepared (mevedel-tool-fs--maybe-transform-media output args))
                 (prepared-path (car prepared))
                 (prepared-mime (cdr prepared))
                 (_ (mevedel-tool-fs--ensure-media-capable prepared-mime))
                 (_ (mevedel-tool-fs--validate-media-file
                     prepared-path prepared-mime))
                 (base64 (mevedel-tool-fs--base64-file prepared-path)))
            (setq total-base64-chars (+ total-base64-chars (length base64)))
            (when (> total-base64-chars
                     mevedel-tool-fs--pdf-pages-max-base64-chars)
              (error "Rendered PDF pages exceed aggregate media size limit (%d chars)"
                     mevedel-tool-fs--pdf-pages-max-base64-chars))
            (push (mevedel-tool-fs--format-media-result
                   prepared-path prepared-mime base64 path)
                  results)
            (push (list :path prepared-path :mime prepared-mime
                        :kind 'image :data base64 :source path :page page)
                  media))))
      (mevedel-tool-fs--media-read-result
       (mapconcat #'identity (nreverse results) "\n\n")
       (nreverse media)))))

(defun mevedel-tool-fs--edit-distance (a b)
  "Return Levenshtein edit distance between strings A and B."
  (let* ((a (downcase a))
         (b (downcase b))
         (m (length a))
         (n (length b))
         (prev (make-vector (1+ n) 0))
         (curr (make-vector (1+ n) 0)))
    (dotimes (j (1+ n))
      (aset prev j j))
    (dotimes (i m)
      (aset curr 0 (1+ i))
      (dotimes (j n)
        (aset curr (1+ j)
              (min (1+ (aref curr j))
                   (1+ (aref prev (1+ j)))
                   (+ (aref prev j)
                      (if (= (aref a i) (aref b j)) 0 1)))))
      (cl-rotatef prev curr))
    (aref prev n)))

(defun mevedel-tool-fs--missing-file-suggestions (path)
  "Return up to three nearby file suggestions for missing PATH."
  (let* ((dir (or (file-name-directory path) default-directory))
         (name (file-name-nondirectory path))
         (base (file-name-base name))
         (candidates nil))
    (when (file-directory-p dir)
      (dolist (entry (directory-files dir t directory-files-no-dot-files-regexp))
        (when (file-regular-p entry)
          (let* ((entry-name (file-name-nondirectory entry))
                 (entry-base (file-name-base entry-name)))
            (when (string-equal (downcase base) (downcase entry-base))
              (push (cons 0 entry) candidates))
            (push (cons (mevedel-tool-fs--edit-distance name entry-name)
                        entry)
                  candidates)))))
    ;; If an absolute path skipped the current working directory, prefer
    ;; candidates under `default-directory' that preserve the requested tail.
    (when (file-name-absolute-p path)
      (let* ((cwd (file-name-as-directory (expand-file-name default-directory)))
             (parent (file-name-directory (directory-file-name cwd)))
             (tail (and parent (file-relative-name path parent))))
        (dolist (candidate (delq nil
                                 (list (expand-file-name name cwd)
                                       (and tail (expand-file-name tail cwd)))))
          (when (file-regular-p candidate)
            (push (cons -1 candidate) candidates)))))
    (seq-take
     (delete-dups
      (mapcar #'cdr
              (sort candidates
                    (lambda (a b)
                      (if (= (car a) (car b))
                          (string< (cdr a) (cdr b))
                        (< (car a) (car b)))))))
     3)))

(defun mevedel-tool-fs--format-missing-file-error (path)
  "Return a friendly missing-file error for PATH."
  (let ((suggestions (mevedel-tool-fs--missing-file-suggestions path)))
    (concat (format "File %s does not exist" path)
            (when suggestions
              (concat ". Did you mean:\n"
                      (mapconcat (lambda (candidate)
                                   (format "- %s" candidate))
                                 suggestions "\n"))))))

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
         (t (error "`rg' exited with code %d listing %s" exit path)))))))

(defun mevedel-tool-fs--slurp-file-contents (path &optional offset limit)
  "Return file PATH content with OFFSET and LIMIT.
Apply Read-tool safety validation before reading.

Validate readability, reject directories, binary files, and blocking
device paths, and resolve symlinks.  For full-file reads (both OFFSET and
LIMIT nil), enforce the 512 KB size cap.  For range reads, default
OFFSET to 1 and LIMIT to 2000 lines.

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
ARGS is a plist with :file_path and optional :offset, :limit, :pages,
:max_width, :max_height, and :max_tokens."
  (let* ((args (mevedel-tool-fs--normalize-read-args args))
         (filename (plist-get args :file_path))
         (offset (plist-get args :offset))
         (limit (plist-get args :limit)))
    (unless (file-exists-p filename)
      (error "%s" (mevedel-tool-fs--format-missing-file-error filename)))
    (unless (file-readable-p filename)
      (error "File %s is not readable" filename))
    (when (file-directory-p filename)
      (error "Cannot read directory %s as file" filename))
    (when (file-symlink-p filename)
      (setq filename (file-truename filename)))
    (when (mevedel-tool-fs--blocked-device-p filename)
      (error "Cannot read %s: device file would block or produce infinite output"
             filename))
    (if (mevedel-tool-fs--media-mime-type filename)
        (progn
          (when (mevedel-tool-fs--text-range-requested-p offset limit)
            (error "Offset and limit are only supported for text files"))
          (unless (or (mevedel-tool-fs--pdf-media-p filename)
                      (null (plist-get args :pages)))
            (error "Parameter pages is only supported for PDF files"))
          (when (and (mevedel-tool-fs--pdf-media-p filename)
                     (null (plist-get args :pages))
                     (mevedel-tool-fs--media-transform-requested-p args))
            (error "`max_width', `max_height', and `max_tokens' are only supported for image files and PDF page images"))
          (mevedel-tool-fs--ensure-media-capable
           (mevedel-tool-fs--media-result-mime filename args))
          (let ((dedup-key (mevedel-tool-fs--media-dedup-key args)))
            (cond
             ((and (bound-and-true-p mevedel--session)
                   (not (mevedel-tool-fs--agent-context-p))
                   (mevedel-session-read-is-duplicate-p
                    mevedel--session filename dedup-key nil))
              (format "File %s unchanged since last read.  Reuse the previous contents."
                      filename))
             (t
              (condition-case err
                  (let ((result (mevedel-tool-fs--read-media-file
                                 filename args)))
                    (when (and (mevedel-tool-fs--pdf-media-p filename)
                               (null (plist-get args :pages))
                               (mevedel-tool-fs--large-pdf-p filename))
                      (setq result
                            (mevedel-tool-fs--append-system-reminder
                             result
                             (mevedel-tool-fs--format-large-pdf-reminder
                              filename))))
                    (when (and (bound-and-true-p mevedel--session)
                               (not (mevedel-tool-fs--agent-context-p)))
                      (mevedel-session-record-file-access
                       mevedel--session filename 'read dedup-key nil))
                    result)
                (error
                 (let ((message (error-message-string err)))
                   (if (and (mevedel-tool-fs--pdf-media-p filename)
                            (null (plist-get args :pages))
                            (string-match-p "Media file is too large"
                                            message))
                       (error "%s%s"
                              message
                              (mevedel-tool-fs--append-system-reminder
                               ""
                               (mevedel-tool-fs--format-large-pdf-reminder
                                filename)))
                     (signal (car err) (cdr err))))))))))
      (when (plist-get args :pages)
        (error "Parameter pages is only supported for PDF files"))
      (when (mevedel-tool-fs--media-transform-requested-p args)
        (error "`max_width', `max_height', and `max_tokens' are only supported for image files and PDF page images"))
      (when (mevedel-tool-fs--binary-extension-p filename)
        (let ((ext (file-name-extension filename)))
          (error "Cannot read binary file (type: .%s): %s" ext filename)))
      (cond
       ((and (bound-and-true-p mevedel--session)
             (not (mevedel-tool-fs--agent-context-p))
             (mevedel-session-read-is-duplicate-p
              mevedel--session filename offset limit))
        (format "File %s unchanged since last read.  Reuse the previous contents."
                filename))
       ((zerop (file-attribute-size (file-attributes filename)))
        (when (and (bound-and-true-p mevedel--session)
                   (not (mevedel-tool-fs--agent-context-p)))
          (mevedel-session-record-file-access
           mevedel--session filename 'read offset limit))
        (format "<system-reminder>\n\
File %s exists but is empty (0 bytes). This is the actual file \
content, not a read failure.\n</system-reminder>" filename))
       (t
        (let ((content (mevedel-tool-fs--slurp-file-contents
                        filename offset limit)))
          (when (and (bound-and-true-p mevedel--session)
                     (not (mevedel-tool-fs--agent-context-p)))
            (mevedel-session-record-file-access
             mevedel--session filename 'read offset limit))
          content))))))

(defun mevedel-tool-fs--glob (callback args)
  "Find files matching a glob pattern using ripgrep.
CALLBACK receives the result string.  ARGS is a plist with :pattern
and optional :path."
  (let* ((pattern (plist-get args :pattern))
         (path (plist-get args :path)))
    (when (string-empty-p pattern)
      (error "Pattern must not be empty"))
    (unless (executable-find "rg")
      (error "`ripgrep` not installed"))
    (setq path (expand-file-name (substitute-in-file-name (or path "."))))
    (unless (and (file-readable-p path) (file-directory-p path))
      (error "Path %s is not a readable directory" path))
    (let* ((rg-args (list "--files" "--hidden" "--color=never"
                          "--follow" "--sort" "modified"
                          "--iglob" pattern
                          path))
           (output-buffer (generate-new-buffer " *mevedel-glob*")))
      (condition-case err
          (make-process
           :name "mevedel-glob"
           :buffer output-buffer
           :stderr output-buffer
           :command (cons "rg" rg-args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((exit-code (process-exit-status process))
                     result)
                 (when (buffer-live-p output-buffer)
                   (with-current-buffer output-buffer
                     (cond
                      ((= exit-code 0) nil)
                      ((= exit-code 1)
                       (erase-buffer)
                       (insert "No files found matching pattern"))
                      (t
                       (goto-char (point-min))
                       (insert (format "Error: glob failed (exit code %d)\n\n"
                                       exit-code))))
                     (when (and (not (string-prefix-p "No files found"
                                                      (buffer-string)))
                                (not (string-prefix-p "Error:"
                                                      (buffer-string))))
                       (goto-char (point-min))
                       (let ((total-lines (count-lines (point-min)
                                                       (point-max))))
                         (when (> total-lines
                                  mevedel-tool-fs--glob-default-head-limit)
                           (forward-line
                            mevedel-tool-fs--glob-default-head-limit)
                           (delete-region (point) (point-max))
                           (goto-char (point-max))
                           (insert
                            (format "\n... Results truncated (limit: %d). Narrow your search with :path or a more specific :pattern."
                                    mevedel-tool-fs--glob-default-head-limit)))))
                     (setq result (buffer-string))))
                 (when (buffer-live-p output-buffer)
                   (kill-buffer output-buffer))
                 (funcall callback (or result ""))))))
        (error
         (when (buffer-live-p output-buffer)
           (kill-buffer output-buffer))
         (error "%s" (error-message-string err)))))))

(defun mevedel-tool-fs--grep (callback args)
  "Search file contents with ripgrep.
CALLBACK receives the result string. ARGS is a plist with :pattern and
optional :path, :glob, :output_mode, :head_limit, :offset, :-i, :-n,
:type, :multiline, :context, :-A, :-B, :-C."
  (let* ((pattern (plist-get args :pattern))
         (path (mevedel-tool-string-arg args :path "."))
         (file-glob (mevedel-tool-string-arg args :glob))
         (output-mode (mevedel-tool-string-arg
                       args :output_mode "files_with_matches"))
         (head-limit (let ((v (plist-get args :head_limit)))
                       (cond ((null v) 250)
                             ((and (integerp v) (= v 0)) nil)
                             ((integerp v) v)
                             (t 250))))
         (offset (or (mevedel-tool-integer-arg args :offset) 0))
         (case-fold (mevedel-tool-truthy-p (plist-get args :-i)))
         (line-numbers (let ((v (plist-get args :-n)))
                         (if (null v)
                             (equal output-mode "content")
                           (mevedel-tool-truthy-p v))))
         (file-type (mevedel-tool-string-arg args :type))
         (multiline (mevedel-tool-truthy-p (plist-get args :multiline)))
         (ctx-after (mevedel-tool-integer-arg args :-A))
         (ctx-before (mevedel-tool-integer-arg args :-B))
         (ctx-around (or (mevedel-tool-integer-arg args :-C)
                         (mevedel-tool-integer-arg args :context))))
    (unless (executable-find "rg")
      (error "`ripgrep` not installed"))
    (setq path (expand-file-name (substitute-in-file-name path)))
    (unless (file-readable-p path)
      (error "Path %s is not readable" path))
    (let ((rg-args nil)
          (output-buffer (generate-new-buffer " *mevedel-grep*")))
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
      (condition-case err
          (make-process
           :name "mevedel-grep"
           :buffer output-buffer
           :stderr output-buffer
           :command (cons "rg" rg-args)
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (let ((exit-code (process-exit-status process))
                     result)
                 (when (buffer-live-p output-buffer)
                   (with-current-buffer output-buffer
                     (cond
                      ((= exit-code 0) nil)
                      ((= exit-code 1)
                       (erase-buffer)
                       (insert "No matches found"))
                      (t
                       (goto-char (point-min))
                       (insert
                        (format "Error: search failed (exit code %d)\n\n"
                                exit-code))))
                     ;; Apply offset and head_limit
                     (when (or (> offset 0) head-limit)
                       (goto-char (point-min))
                       (let ((total-lines (count-lines (point-min)
                                                       (point-max))))
                         (when (> offset 0)
                           (forward-line offset)
                           (delete-region (point-min) (point))
                           (cl-decf total-lines offset))
                         (when (and head-limit (> total-lines head-limit))
                           (goto-char (point-min))
                           (forward-line head-limit)
                           (delete-region (point) (point-max))
                           (goto-char (point-max))
                           (insert
                            (format "\n... Results truncated (limit: %d, offset: %d)"
                                    head-limit offset)))))
                     ;; Hard cap on total output size to prevent
                     ;; context overflow.  Even with line-count and
                     ;; per-line truncation, results can be
                     ;; dangerously large.
                     (when (> (buffer-size)
                              mevedel-tool-fs--grep-max-output-bytes)
                       (goto-char (point-min))
                       (forward-char
                        mevedel-tool-fs--grep-max-output-bytes)
                       ;; Truncate at the last complete line within
                       ;; the budget.
                       (beginning-of-line)
                       (delete-region (point) (point-max))
                       (goto-char (point-max))
                       (insert
                        (format "\n... Output truncated at %dK byte limit. \
Narrow your search with :glob, :type, or a more specific :pattern."
                                (/ mevedel-tool-fs--grep-max-output-bytes
                                   1024))))
                     (setq result (buffer-string))))
                 (when (buffer-live-p output-buffer)
                   (kill-buffer output-buffer))
                 (funcall callback (or result ""))))))
        (error
         (when (buffer-live-p output-buffer)
           (kill-buffer output-buffer))
         (error "%s" (error-message-string err)))))))


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
      (error "`old_string' and `new_string' must be different"))
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
CALLBACK receives the result string or a plist carrying render-data.
ARGS is a plist with :path."
  (let ((path (plist-get args :path)))
    (unless (stringp path)
      (error "Parameter path is required"))
    (condition-case err
        (let* ((full-path (expand-file-name path))
               (existed (file-directory-p full-path))
               (root (and (boundp 'mevedel--workspace)
                          mevedel--workspace
                          (ignore-errors
                            (mevedel-workspace-root mevedel--workspace))))
               (rel-path (and root
                              (ignore-errors
                                (file-relative-name full-path root)))))
          (make-directory full-path t)
          (funcall callback
                   (list :result
                         (if existed
                             (format "Directory already exists: %s" full-path)
                           (format "Directory created: %s" full-path))
                         :render-data
                         (list :kind 'mkdir
                               :created (not existed)
                               :path full-path
                               :rel-path rel-path))))
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
                 "The directory to search in. If not specified, the session working directory will be used. Relative paths are resolved from the session working directory."))
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
    :args ((file_path string :required "Absolute or relative path to the file to read. Relative paths are resolved from the session working directory.")
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
    :renderer #'mevedel-tool-fs--render-read)

  (mevedel-define-tool
    :name "Grep"
    :description "Search file contents using ripgrep."
    :prompt-file "tools/grep.md"
    :handler #'mevedel-tool-fs--grep
    :args ((pattern string :required
                   "The regular expression pattern to search for in file contents.")
           (path string :optional
                 "File or directory to search in (rg PATH). Defaults to the session working directory. Relative paths are resolved from the session working directory.")
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
                 "The path of the directory to create. Relative paths are resolved from the session working directory."))
    :async-p t
    :groups (edit)
    :get-path (lambda (args) (plist-get args :path))
    :renderer #'mevedel-tool-fs--render-mkdir)

  (mevedel-define-tool
    :name "Write"
    :description "Write a file to the local filesystem."
    :prompt-file "tools/write.md"
    :handler #'mevedel-tool-fs--write
    :args ((file_path string :required
                      "Absolute or relative path to the file to write. Relative paths are resolved from the session working directory.")
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
                      "Absolute or relative path to the file to modify. Relative paths are resolved from the session working directory.")
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
