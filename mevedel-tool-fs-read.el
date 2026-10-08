;;; mevedel-tool-fs-read.el -- Read text and media resources -*- lexical-binding: t -*-

;;; Commentary:

;; Text decoding, bounded Read behavior, and media conversion.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'mevedel-agents)
(require 'mevedel-execution-target)
(require 'mevedel-file-state)
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-structs)
(require 'mevedel-tool-fs)
(require 'mevedel-utilities)
(require 'xml)

;; `gptel-request'
(declare-function gptel--model-capable-p "ext:gptel-request"
                  (cap &optional model))
(declare-function gptel--model-mime-capable-p "ext:gptel-request"
                  (mime &optional model))

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-path "mevedel-agents" (cl-x) t)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-file-state'
(declare-function mevedel-session-read-is-duplicate-p
                  "mevedel-file-state" (session path offset limit))
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind &optional offset limit))

;; `mevedel-history-search'
(declare-function mevedel-history-search-start
                  "mevedel-history-search" (callback args descriptor))
(autoload 'mevedel-history-search-start "mevedel-history-search")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-tool-results-dir
                  "mevedel-pipeline"
                  (session buffer &optional request existing-only-p))
(defvar mevedel-pipeline--active-call-source)

;; `mevedel-resource'
(declare-function mevedel-resource-execute
                  "mevedel-resource" (attempt &optional executor options))

;; `mevedel-request'
(defvar mevedel--current-request)

;; `mevedel-reminders'
(declare-function mevedel-reminders-queue-turn-event
                  "mevedel-reminders" (buffer key body &optional commit))
(autoload 'mevedel-reminders-queue-turn-event "mevedel-reminders")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts" (session logical
                                                  &optional committed-only))

;; `mevedel-tool-editing'
(declare-function mevedel-tool-editing-view "mevedel-tool-editing" (components callback))
(autoload 'mevedel-tool-editing-view "mevedel-tool-editing")

;; `mevedel-structs'
(declare-function mevedel-session-execution-target "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace-instruction-hashes
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-workspace-config-files
                  "mevedel-system" (workspace &optional working-directory))
(autoload 'mevedel-system-workspace-config-files "mevedel-system")

;; `mevedel-tool-fs'
(declare-function mevedel-tool-fs-display-path "mevedel-tool-fs" (path))
(declare-function mevedel-tool-fs-executable-find "mevedel-tool-fs" (name path))
(declare-function mevedel-tool-fs-handler-result "mevedel-tool-fs" (result))
(declare-function mevedel-tool-fs-model-path "mevedel-tool-fs" (path))
(declare-function mevedel-tool-fs-resource-attempt "mevedel-tool-fs" (address))
(declare-function mevedel-tool-fs-resource-child-address
                  "mevedel-tool-fs"
                  (address root path &optional address-prefix))
(declare-function mevedel-tool-fs-resource-rg-exclusions
                  "mevedel-tool-fs" (address))
(declare-function mevedel-tool-fs-strip-system-reminders
                  "mevedel-tool-fs" (result))
(declare-function mevedel-tool-fs-visible-path
                  "mevedel-tool-fs" (path &optional resource-address))

;; `mevedel-execution'
(declare-function mevedel-execution-run-helper
                  "mevedel-execution"
                  (name command read-paths writable-roots &rest keys))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-handler-resumable "mevedel-pipeline" (function))

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())

;; `mevedel-utilities'
(declare-function mevedel-start-helper-capturing-output
                  "mevedel-utilities"
                  (callback name command read-paths &optional writable-roots session))

(defvar mevedel-tool-fs-read--resource-address nil
  "Authored resource address for the current Read operation.")

(defvar mevedel-tool-fs-read--scratch-directory nil
  "A caller's scratch directory for derived media, deleted by that caller.")

(defvar mevedel-tool-fs-read--local-media-copy nil
  "Dynamically scoped remote path and local copy for one media read.")

(defvar mevedel-tool-fs-read--fail nil
  "Function settling the current Read with an error raised after a wait.
Before the first helper an error signals to the pipeline as a synchronous
handler's does; a continuation has no such caller, so the helper boundary
hands its errors here.")

(defvar mevedel-tool-fs-read--cleanups nil
  "Cell whose car lists cleanups the current Read runs when it settles.")

(defconst mevedel-tool-fs-read--continuation-variables
  '(mevedel-tool-fs-read--fail
    mevedel-tool-fs-read--cleanups
    mevedel-tool-fs-read--resource-address
    mevedel-tool-fs-read--local-media-copy
    mevedel--session
    temporary-file-directory
    default-directory)
  "Read state a helper continuation restores.")

(defun mevedel-tool-fs-read--run (body deliver)
  "Run Read BODY and call DELIVER exactly once with its result.
BODY receives the continuation that settles the Read with a result.  An
error BODY signals before its first helper propagates to the caller; one
raised in a helper continuation settles the Read as an error result.
Either way the Read's temporary media copies are released."
  (let* ((cleanups (list nil))
         (settled nil)
         (release (lambda ()
                    (dolist (cleanup (car cleanups))
                      (ignore-errors (funcall cleanup)))
                    (setcar cleanups nil)))
         (settle (lambda (value)
                   (unless settled
                     (setq settled t)
                     (funcall release)
                     (funcall deliver value))))
         (mevedel-tool-fs-read--cleanups cleanups)
         (mevedel-tool-fs-read--fail
          (lambda (err)
            (funcall settle
                     (list :result (format "Error: %s"
                                           (mevedel-resource-error-message err))
                           :status 'error)))))
    (condition-case err
        (funcall body settle)
      (error
       (unless settled
         (setq settled t)
         (funcall release))
       (signal (car err) (cdr err))))))

(defun mevedel-tool-fs-read--helper (k name command read-paths &optional writable-roots)
  "Start helper COMMAND as NAME, then call K with its EXIT-CODE and OUTPUT.
K continues the current Read in its original buffer, pipeline context and
Read state rather than waiting here: a wait would re-enter the event loop
from gptel's process callbacks, where quitting is inhibited.  A helper that
could not run, and any error K raises, settle the Read as an error."
  (let ((values (mapcar (lambda (symbol) (and (boundp symbol) (symbol-value symbol)))
                        mevedel-tool-fs-read--continuation-variables)))
    (mevedel-start-helper-capturing-output
     (mevedel-pipeline-handler-resumable
      (lambda (exit-code output error)
        (cl-progv mevedel-tool-fs-read--continuation-variables values
          (condition-case err
              (if error
                  (signal (car error) (cdr error))
                (funcall k exit-code output))
            (error
             (if mevedel-tool-fs-read--fail
                 (funcall mevedel-tool-fs-read--fail err)
               (signal (car err) (cdr err))))))))
     name command read-paths writable-roots)))

(defun mevedel-tool-fs-read--helper-now
    (name command read-paths &optional writable-roots)
  "Run helper COMMAND as NAME to completion, returning (EXIT-CODE . OUTPUT).
READ-PATHS and WRITABLE-ROOTS authorize the helper's file access.
Only for mention expansion, which still builds its text synchronously."
  (let* ((result (mevedel-execution-run-helper
                  name command read-paths writable-roots
                  :session (bound-and-true-p mevedel--session)
                  :owner (mevedel-current-origin)))
         (error-data (plist-get result :error)))
    (when error-data
      (signal (car error-data) (cdr error-data)))
    (cons (plist-get result :exit-code) (plist-get result :output))))

(defun mevedel-tool-fs-read--visible-path (path)
  "Return PATH in the current Read operation's visible domain."
  (mevedel-tool-fs-visible-path path mevedel-tool-fs-read--resource-address))

(defvar-local mevedel-tool-fs-read--mode-cache nil
  "Display mode memo: copied `auto-mode-alist' and a bounded path table.")

(defun mevedel-tool-fs-read--mode-for-file (path)
  "Return the major-mode symbol `auto-mode-alist' would select for PATH, or nil.
The returned mode is only used to fontify a temp buffer for read-only
display; modes that fail to load or error fall back to text verbatim
via `mevedel-view--fontify-as'."
  (when (and path
             (stringp path)
             (not (string-empty-p path))
             (not (mevedel-tool-fs-read-media-mime-type path)))
    (unless (and mevedel-tool-fs-read--mode-cache
                 (equal auto-mode-alist (car mevedel-tool-fs-read--mode-cache)))
      (setq mevedel-tool-fs-read--mode-cache
            (cons (mapcar (lambda (entry)
                            (cons (if (stringp (car entry)) (copy-sequence (car entry)) (car entry))
                                  (copy-tree (cdr entry))))
                          auto-mode-alist)
                  (make-hash-table :test #'equal))))
    (let* ((table (cdr mevedel-tool-fs-read--mode-cache))
           (cached (gethash path table)))
      (if cached (car cached)
        (let* ((mode (assoc-default path auto-mode-alist #'string-match-p))
               (result (cond ((symbolp mode) mode)
                             ((and (consp mode) (symbolp (car mode))) (car mode)))))
          (when (>= (hash-table-count table) 128) (clrhash table))
          (puthash (copy-sequence path) (list result) table)
          result)))))

(defun mevedel-tool-fs-read--line-count (text)
  "Return the display line count for TEXT without allocating line strings."
  (let ((lines 1)
        (pos 0))
    (while (setq pos (string-search "\n" text pos))
      (setq lines (1+ lines))
      (setq pos (1+ pos)))
    lines))

(defun mevedel-tool-fs-read-render (name args result _render-data)
  "Rendering plist for the Read tool.
NAME is \"Read\".  ARGS carries `:file_path'.  RESULT is the line-numbered
file content.  Header shows the file basename and line count; body
fontifies as the file's natural mode when detectable from extension."
  (when (and (stringp result)
             (not (string-match-p "\\`[ \t\n]*Error:" result)))
    ;; Renderer output is disposable UI state rebuilt on each rerender.
    ;; Hashing the payload to look up a line count of the same payload
    ;; saved nothing, so the header is simply computed.
    (let ((path (plist-get args :file_path))
          (visible (mevedel-tool-fs-strip-system-reminders result)))
      (list :header (format "%s: %s (%d lines)" (or name "Read")
                            (mevedel-tool-fs-display-path path)
                            (mevedel-tool-fs-read--line-count visible))
            :body result
            :body-mode (mevedel-tool-fs-read--mode-for-file path)
            :initially-collapsed-p t))))

(defconst mevedel-tool-fs-read--binary-extensions
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

(defconst mevedel-tool-fs-read--media-mime-by-extension
  '(("pdf" . "application/pdf")
    ("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp"))
  "Read-tool media MIME types keyed by lowercase file extension.")

(defconst mevedel-tool-fs-read-media-max-bytes (* 10 1024 1024)
  "Maximum media file size Read will base64-encode directly.")

(defconst mevedel-tool-fs-read--remote-media-copy-max-bytes (* 100 1024 1024)
  "Maximum remote media size copied to a local converter input.")

(defconst mevedel-tool-fs-read--pdf-pages-max-base64-chars (* 10 1024 1024)
  "Maximum aggregate base64 payload size for one PDF page extraction.")

(defconst mevedel-tool-fs-read--default-limit 2000
  "Default maximum number of text lines returned by one Read call.")

(defconst mevedel-tool-fs-read--max-output-chars (* 50 1024)
  "Maximum number of characters returned by one text Read call.")

(defconst mevedel-tool-fs-read--max-pages 20
  "Maximum number of PDF pages a single Read call may extract.")

(defconst mevedel-tool-fs-read--large-attachment-reminder-bytes (* 1024 1024)
  "Minimum PDF attachment size that gets bounded-page guidance.")

(defun mevedel-tool-fs-read--dedup-p ()
  "Return non-nil when this Read may consult and record duplicate-read state.

Resource reads always return their contents.  Sub-agents have separate
transcripts, and ToolCall exposes only its final value to the model, so
neither may reuse or poison the parent conversation's Read history."
  (and (bound-and-true-p mevedel--session)
       (not mevedel-tool-fs-read--resource-address)
       (not mevedel--agent-invocation)
       (not (eq (bound-and-true-p mevedel-pipeline--active-call-source) 'ptc))))

(defconst mevedel-tool-fs-read--blocked-device-paths
  '("/dev/zero" "/dev/random" "/dev/urandom" "/dev/full"
    "/dev/stdin" "/dev/tty" "/dev/console"
    "/dev/stdout" "/dev/stderr"
    "/dev/fd/0" "/dev/fd/1" "/dev/fd/2")
  "Device paths that would block or produce infinite output.")

(defun mevedel-tool-fs-read--binary-extension-p (filename)
  "Return non-nil if FILENAME has a binary file extension."
  (member (downcase (or (file-name-extension filename) ""))
          mevedel-tool-fs-read--binary-extensions))

(defun mevedel-tool-fs-read-media-mime-type (filename)
  "Return supported media MIME type for FILENAME, or nil."
  (cdr (assoc (downcase (or (file-name-extension filename) ""))
              mevedel-tool-fs-read--media-mime-by-extension)))

(defun mevedel-tool-fs-read-pdf-media-p (filename)
  "Return non-nil when FILENAME is a supported PDF media file."
  (equal (mevedel-tool-fs-read-media-mime-type filename) "application/pdf"))

(defun mevedel-tool-fs-read--blocked-device-p (filename)
  "Return non-nil if FILENAME is a blocked device path."
  (or (member filename mevedel-tool-fs-read--blocked-device-paths)
      (and (string-prefix-p "/proc/" filename)
           (or (string-suffix-p "/fd/0" filename)
               (string-suffix-p "/fd/1" filename)
               (string-suffix-p "/fd/2" filename)))))

(defun mevedel-tool-fs-read--ensure-media-capable (mime)
  "Signal an error unless the current gptel model supports MIME media input."
  (require 'gptel-request)
  (unless (and (fboundp 'gptel--model-capable-p)
               (gptel--model-capable-p 'media))
    (error "Current model does not support media input"))
  (when (and mime (fboundp 'gptel--model-mime-capable-p)
             (not (gptel--model-mime-capable-p mime)))
    (error "Current model does not support media type %s" mime)))

(defun mevedel-tool-fs-read--pdf-page-images-only-p ()
  "Return non-nil when the current model takes page images but not PDFs."
  (and (fboundp 'gptel--model-mime-capable-p)
       (not (gptel--model-mime-capable-p "application/pdf"))
       (gptel--model-mime-capable-p "image/png")))

(defun mevedel-tool-fs-read--file-bytes-at-p (path offset expected)
  "Return non-nil when PATH has EXPECTED byte string at OFFSET."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil offset (+ offset (length expected)))
    (string-equal (buffer-string) expected)))

(defun mevedel-tool-fs-read--valid-media-file-p (path mime)
  "Return non-nil when PATH contents match supported media MIME."
  (and (> (file-attribute-size (file-attributes path)) 0)
       (pcase mime
         ("application/pdf"
          (mevedel-tool-fs-read--file-bytes-at-p path 0 "%PDF-"))
         ("image/png"
          (mevedel-tool-fs-read--file-bytes-at-p
           path 0 (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)))
         ("image/jpeg"
          (mevedel-tool-fs-read--file-bytes-at-p
           path 0 (unibyte-string #xff #xd8 #xff)))
         ("image/gif"
          (or (mevedel-tool-fs-read--file-bytes-at-p path 0 "GIF87a")
              (mevedel-tool-fs-read--file-bytes-at-p path 0 "GIF89a")))
         ("image/webp"
          (and (mevedel-tool-fs-read--file-bytes-at-p path 0 "RIFF")
               (mevedel-tool-fs-read--file-bytes-at-p path 8 "WEBP")))
         (_ nil))))

(defun mevedel-tool-fs-read--validate-media-file (path mime &optional display-path)
  "Signal an error unless PATH contents match supported media MIME.
Use DISPLAY-PATH in model-visible errors when non-nil."
  (unless (> (file-attribute-size (file-attributes path)) 0)
    (error "Media file is empty: %s" (or display-path path)))
  (unless (mevedel-tool-fs-read--valid-media-file-p path mime)
    (error "File contents do not match media type %s: %s"
           mime (or display-path path))))

(defun mevedel-tool-fs-read--parse-pages (pages)
  "Parse Read PAGES string into a cons cell (START . END).

Valid forms are \"3\", \"1-5\", and \"3-\".  Open-ended ranges are
capped to `mevedel-tool-fs-read--max-pages'.  Signals an error for
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
            end (+ start (1- mevedel-tool-fs-read--max-pages))))
     (t
      (error "Invalid pages value %S; use forms like \"3\", \"1-5\", or \"3-\""
             pages)))
    (when (< start 1)
      (error "PDF page numbers start at 1"))
    (when (< end start)
      (error "PDF page range must not end before it starts"))
    (when (> (1+ (- end start)) mevedel-tool-fs-read--max-pages)
      (error "PDF page range exceeds %d pages" mevedel-tool-fs-read--max-pages))
    (cons start end)))

(defun mevedel-tool-fs-read--base64-file (path &optional max-bytes display-path)
  "Return base64 contents of PATH after enforcing MAX-BYTES.
Use DISPLAY-PATH in model-visible errors when non-nil."
  (let ((size (file-attribute-size (file-attributes path)))
        (cap (or max-bytes mevedel-tool-fs-read-media-max-bytes)))
    (when (> size cap)
      (error "Media file is too large (%d bytes > %d bytes): %s"
             size cap (or display-path path)))
    (with-temp-buffer
      (insert-file-contents-literally path)
      (base64-encode-region (point-min) (point-max) :no-line-break)
      (buffer-string))))

(defun mevedel-tool-fs-read--with-local-media-source (path function)
  "Call FUNCTION with PATH available as a local converter input.
Within a Read the temporary copy lives until the Read settles, because
FUNCTION may continue from helper callbacks after it returns; otherwise it
is deleted before returning."
  (cond
   ((not (file-remote-p path))
    (funcall function path))
   ((equal path (car-safe mevedel-tool-fs-read--local-media-copy))
    (funcall function (cdr mevedel-tool-fs-read--local-media-copy)))
   (t
    (let ((size (file-attribute-size (file-attributes path))))
      (when (> size mevedel-tool-fs-read--remote-media-copy-max-bytes)
        (error "Remote media file is too large (%d bytes > %d bytes): %s"
               size mevedel-tool-fs-read--remote-media-copy-max-bytes
               (mevedel-tool-fs-model-path path)))
      (let ((directory (make-temp-file "mevedel-media-" t))
            (deferred mevedel-tool-fs-read--cleanups))
        (when deferred
          (push (lambda () (delete-directory directory t)) (car deferred)))
        (unwind-protect
            (let* ((temporary-file-directory
                    (file-name-as-directory directory))
                   (mevedel-tool-fs-read--scratch-directory directory)
                   (default-directory temporary-file-directory)
                   (mevedel--session nil)
                   (local (file-name-concat
                           directory (file-name-nondirectory path))))
              (copy-file path local t)
              (let ((mevedel-tool-fs-read--local-media-copy (cons path local)))
                (funcall function local)))
          (unless deferred
            (ignore-errors (delete-directory directory t)))))))))

(defvar mevedel-tool-fs-read--sessionless-results nil
  "This Emacs's private directory for media read outside a session.")

(defun mevedel-tool-fs-read--delete-sessionless-results ()
  "Delete media read outside a session; nothing retains it past this Emacs."
  (when mevedel-tool-fs-read--sessionless-results
    (ignore-errors (delete-directory mevedel-tool-fs-read--sessionless-results t))
    (setq mevedel-tool-fs-read--sessionless-results nil)))

(defun mevedel-tool-fs-read--tool-results-dir ()
  "Return a writable directory for Read-generated media artifacts.
A session keeps them with its tool results.  Without one they go to the
caller's `mevedel-tool-fs-read--scratch-directory', or else to a private
directory that this Emacs deletes when it exits."
  (let* ((buffer (if (and (boundp 'mevedel--data-buffer)
                          mevedel--data-buffer
                          (buffer-live-p mevedel--data-buffer))
                     mevedel--data-buffer
                   (current-buffer)))
         (dir (and (bound-and-true-p mevedel--session)
                   (fboundp 'mevedel-pipeline-tool-results-dir)
                   (mevedel-pipeline-tool-results-dir
                    mevedel--session buffer))))
    (cond
     (dir (make-directory dir t))
     (mevedel-tool-fs-read--scratch-directory
      (setq dir (file-name-concat mevedel-tool-fs-read--scratch-directory
                                  "mevedel-tool-results"))
      (make-directory dir t))
     (t
      (unless (and mevedel-tool-fs-read--sessionless-results
                   (file-directory-p mevedel-tool-fs-read--sessionless-results))
        (setq mevedel-tool-fs-read--sessionless-results
              (make-temp-file "mevedel-tool-results-" t))
        (add-hook 'kill-emacs-hook #'mevedel-tool-fs-read--delete-sessionless-results))
      (setq dir mevedel-tool-fs-read--sessionless-results)))
    dir))

(defun mevedel-tool-fs-read--imagemagick-command ()
  "Return an ImageMagick executable name, preferring magick over convert."
  (or (executable-find "magick")
      (executable-find "convert")))

(defun mevedel-tool-fs-read--file-size (path)
  "Return PATH's size in bytes, or nil when unavailable."
  (ignore-errors
    (file-attribute-size (file-attributes path))))

(defun mevedel-tool-fs-read--pdfinfo-page-count (exit-code output)
  "Return the page count `pdfinfo' reported in OUTPUT with EXIT-CODE, or nil."
  (and (eql 0 exit-code)
       (string-match "^Pages:[[:space:]]+\\([0-9]+\\)" output)
       (string-to-number (match-string 1 output))))

(defun mevedel-tool-fs-read--pdf-page-count (path k)
  "Call K with PDF PATH's page count, or nil when `pdfinfo' cannot tell."
  (mevedel-tool-fs-read--with-local-media-source
   path
   (lambda (local)
     (if (not (executable-find "pdfinfo"))
         (funcall k nil)
       (mevedel-tool-fs-read--helper
        (lambda (exit-code output)
          (funcall k (mevedel-tool-fs-read--pdfinfo-page-count exit-code output)))
        "mevedel-pdfinfo" (list "pdfinfo" local) (list local))))))

(defun mevedel-tool-fs-read-pdf-page-count-now (path)
  "Return PDF PATH's page count when `pdfinfo' can determine it.
Synchronous, for mention expansion; the Read tool waits asynchronously."
  (mevedel-tool-fs-read--with-local-media-source
   path
   (lambda (local)
     (when (executable-find "pdfinfo")
       (pcase-let ((`(,exit-code . ,output)
                    (mevedel-tool-fs-read--helper-now
                     "mevedel-pdfinfo" (list "pdfinfo" local) (list local))))
         (mevedel-tool-fs-read--pdfinfo-page-count exit-code output))))))

(defun mevedel-tool-fs-read-pdf-page-images-now (path page-count)
  "Return PNG bytes for PDF PATH's leading pages, one string per page.
PAGE-COUNT is PATH's page count.  At most `mevedel-tool-fs-read--max-pages'
pages are rendered.  Synchronous, for mention expansion; the Read tool
renders pages asynchronously."
  (unless page-count
    (error "Cannot determine the page count of %s"
           (mevedel-tool-fs-read--visible-path path)))
  (unless (executable-find "pdftoppm")
    (error "'pdftoppm' not installed; install 'poppler-utils' to read PDF pages as images"))
  (mevedel-tool-fs-read--with-local-media-source
   path
   (lambda (local)
     (let ((directory (make-temp-file "mevedel-pdf-pages-" t))
           (total 0)
           pages)
       (unwind-protect
           (dotimes (index (min page-count mevedel-tool-fs-read--max-pages))
             (let* ((page (1+ index))
                    (prefix (file-name-concat directory (format "page-%d" page))))
               (pcase-let ((`(,exit-code . ,output)
                            (mevedel-tool-fs-read--helper-now
                             "mevedel-pdftoppm"
                             (list "pdftoppm"
                                   "-f" (number-to-string page)
                                   "-l" (number-to-string page)
                                   "-singlefile" "-png" local prefix)
                             (list local) (list directory))))
                 (unless (eql 0 exit-code)
                   (error "'pdftoppm' failed while rendering page %d of %s%s"
                          page (mevedel-tool-fs-read--visible-path path)
                          (if (string-empty-p output) "" (concat ": " output)))))
               (let ((bytes (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally (concat prefix ".png"))
                              (buffer-string))))
                 (when (> (cl-incf total (length bytes))
                          mevedel-tool-fs-read-media-max-bytes)
                   (error "Rendered PDF pages exceed media size limit (%d bytes)"
                          mevedel-tool-fs-read-media-max-bytes))
                 (push bytes pages))))
         (delete-directory directory t))
       (nreverse pages)))))

(defun mevedel-tool-fs-read-large-pdf-p (path page-count)
  "Return non-nil when PDF PATH should get bounded-page guidance.
PAGE-COUNT is PATH's page count, or nil when unknown."
  (and (mevedel-tool-fs-read-pdf-media-p path)
       (let ((size (mevedel-tool-fs-read--file-size path)))
         (or (and page-count (> page-count mevedel-tool-fs-read--max-pages))
             (and size
                  (> size mevedel-tool-fs-read--large-attachment-reminder-bytes))))))

(defun mevedel-tool-fs-read-format-large-pdf-reminder (path page-count)
  "Return model-visible guidance for a large PDF at PATH.
PAGE-COUNT is PATH's page count, or nil when unknown."
  (let* ((shown (mevedel-tool-fs-read--visible-path path))
         (size (mevedel-tool-fs-read--file-size path))
         (details (delq nil
                        (list (and size
                                   (file-size-human-readable size))
                              (and page-count
                                   (format "%d pages" page-count))))))
    (format "PDF `%s` is large%s. Prefer bounded `Read(file_path=\"%s\", pages=\"START-END\")` requests for relevant pages instead of rereading or reattaching the whole document. Each PDF page request is capped at %d pages; use page selectors like \"1-5\" or \"6-\" when you need the next chunk."
            shown
            (if details
                (format " (%s)" (string-join details ", "))
              "")
            shown
            mevedel-tool-fs-read--max-pages)))

(defun mevedel-tool-fs-read--append-system-reminder (result body)
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

(defun mevedel-tool-fs-read--bounded-pdf-page-range (pages page-count)
  "Return requested PAGES bounded by PAGE-COUNT when it is known."
  (let ((range (mevedel-tool-fs-read--parse-pages pages)))
    (if page-count
        (let ((start (car range)))
          (when (< page-count start)
            (error "PDF page range starts after last page (%d)" page-count))
          (cons start (min (cdr range) page-count)))
      range)))

(defun mevedel-tool-fs-read--media-transform-requested-p (args)
  "Return non-nil when ARGS requests image resizing or compression."
  (or (plist-get args :max_width)
      (plist-get args :max_height)
      (plist-get args :max_tokens)))

(defun mevedel-tool-fs-read--positive-integer-or-nil (value name)
  "Validate VALUE as a positive integer for NAME, allowing nil."
  (when value
    (unless (and (integerp value) (> value 0))
      (error "Parameter %s must be a positive integer" name)))
  value)

(defun mevedel-tool-fs-read--maybe-transform-media (path args k)
  "Call K with media PATH, optionally transformed per Read ARGS.

K receives a cons cell (PATH . MIME).  If ARGS contains `:max_width',
`:max_height', or `:max_tokens', ImageMagick is required."
  (let ((mime (mevedel-tool-fs-read-media-mime-type path)))
    (if (not (mevedel-tool-fs-read--media-transform-requested-p args))
        (funcall k (cons path mime))
      (let* ((max-width (mevedel-tool-fs-read--positive-integer-or-nil
                         (plist-get args :max_width) "max_width"))
             (max-height (mevedel-tool-fs-read--positive-integer-or-nil
                          (plist-get args :max_height) "max_height"))
             (max-tokens (mevedel-tool-fs-read--positive-integer-or-nil
                          (plist-get args :max_tokens) "max_tokens"))
             (cmd (mevedel-tool-fs-read--imagemagick-command)))
        (unless cmd
          (error "Cannot resize/compress this image because ImageMagick is unavailable. Install `magick' or `convert', or retry Read without max_width, max_height, and max_tokens to read the original image"))
        (let* ((output-ext (if max-tokens "jpg"
                             (downcase (or (file-name-extension path) "png"))))
               (output-mime (if max-tokens "image/jpeg" mime))
               (output (make-temp-file
                        (file-name-concat
                         (mevedel-tool-fs-read--tool-results-dir)
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
          (mevedel-tool-fs-read--helper
           (lambda (exit-code process-output)
             (unless (eql 0 exit-code)
               (error "ImageMagick failed while preparing media file%s"
                      (if (string-empty-p process-output)
                          ""
                        (concat ": " process-output))))
             (funcall k (cons output output-mime)))
           "mevedel-imagemagick" (cons cmd im-args) (list path)
           (list (file-name-directory output))))))))

(defun mevedel-tool-fs-read--format-media-result
    (path mime base64 &optional source display-path)
  "Format a model-visible media envelope for PATH, MIME, and BASE64.
SOURCE is the original source path when PATH points to a generated file.
DISPLAY-PATH replaces the physical PATH in model-visible text."
  (let ((size (file-attribute-size (file-attributes path))))
    (concat "<media-file>\n"
            (format "path: %s\n" (or display-path path))
            (when (and source (not (equal source display-path)))
              (format "source: %s\n" source))
            (format "mime_type: %s\n" mime)
            (format "size_bytes: %d\n" size)
            "encoding: base64\n"
            "data:\n"
            base64
            "\n</media-file>")))

(defun mevedel-tool-fs-read--media-read-result (text media)
  "Return Read media TEXT with MEDIA side-channel data."
  (list :result text :media media))

(defun mevedel-tool-fs-read--media-dedup-key (args)
  "Return a stable key for media Read options in ARGS."
  (list :media
        :pages (plist-get args :pages)
        :max-width (plist-get args :max_width)
        :max-height (plist-get args :max_height)
        :max-tokens (plist-get args :max_tokens)))

(defun mevedel-tool-fs-read--media-result-mime (path args)
  "Return the MIME type expected from a media Read of PATH with ARGS."
  (let ((mime (mevedel-tool-fs-read-media-mime-type path)))
    (cond
     ((and (equal mime "application/pdf") (plist-get args :pages))
      (if (plist-get args :max_tokens) "image/jpeg" "image/png"))
     ((and mime (string-prefix-p "image/" mime)
           (plist-get args :max_tokens))
      "image/jpeg")
     (t mime))))

(defun mevedel-tool-fs-read--normalize-read-args (args)
  "Return ARGS normalized for Read handling."
  (let ((normalized (copy-sequence args)))
    (when-let* ((file-path (plist-get normalized :file_path)))
      (when (stringp file-path)
        (plist-put normalized :file_path (expand-file-name file-path))))
    (when (equal (plist-get normalized :pages) "")
      (plist-put normalized :pages nil))
    (dolist (key '(:offset :limit :max_width :max_height :max_tokens))
      (when (equal (plist-get normalized key) 0)
        (plist-put normalized key nil)))
    ;; A stale line number or model arithmetic near the top of a file
    ;; goes negative; answering it with content, or with an empty
    ;; success, reads as the file rather than as the range.  The media
    ;; keys keep their own validator, which owns their message.
    (dolist (key '(:offset :limit))
      (let ((value (plist-get normalized key)))
        (when (and (numberp value) (< value 0))
          (error "Parameter %s must be a positive integer"
                 (substring (symbol-name key) 1)))))
    normalized))

(defun mevedel-tool-fs-read--media-file (path args k)
  "Read supported media PATH according to ARGS, then call K with the result."
  (let ((mime (mevedel-tool-fs-read-media-mime-type path))
        (model-path (mevedel-tool-fs-read--visible-path path)))
    (cond
     ((equal mime "application/pdf")
      (if-let* ((pages (plist-get args :pages)))
          (mevedel-tool-fs-read--pdf-pages path pages args k)
        (when (mevedel-tool-fs-read--media-transform-requested-p args)
          (error "'max_width', 'max_height', and 'max_tokens' are only supported for image files and PDF page images"))
        (mevedel-tool-fs-read--ensure-media-capable mime)
        (mevedel-tool-fs-read--with-local-media-source
         path
         (lambda (source)
           (mevedel-tool-fs-read--validate-media-file source mime model-path)
           (let* ((base64 (mevedel-tool-fs-read--base64-file
                           source nil model-path))
                  (media (list (list :path model-path
                                     :mime "application/pdf"
                                     :kind 'document
                                     :data base64))))
             (funcall k (mevedel-tool-fs-read--media-read-result
                         (mevedel-tool-fs-read--format-media-result
                          source "application/pdf" base64 nil model-path)
                         media)))))))
     ((and mime (string-prefix-p "image/" mime))
      (mevedel-tool-fs-read--validate-media-file path mime model-path)
      (funcall
       (if (mevedel-tool-fs-read--media-transform-requested-p args)
           #'mevedel-tool-fs-read--with-local-media-source
         (lambda (input function) (funcall function input)))
       path
       (lambda (converter-path)
         (mevedel-tool-fs-read--maybe-transform-media
          converter-path args
          (lambda (prepared)
            (let* ((prepared-path (car prepared))
                   (prepared-mime (cdr prepared))
                   (transformed (not (equal prepared-path converter-path)))
                   (_ (mevedel-tool-fs-read--ensure-media-capable prepared-mime))
                   (_ (mevedel-tool-fs-read--validate-media-file
                       prepared-path prepared-mime model-path))
                   (base64 (mevedel-tool-fs-read--base64-file
                            prepared-path nil model-path))
                   (media (list (append
                                 (list :path model-path :mime prepared-mime
                                       :kind 'image :data base64)
                                 (and transformed (list :source model-path))))))
              (funcall k (mevedel-tool-fs-read--media-read-result
                          (mevedel-tool-fs-read--format-media-result
                           prepared-path prepared-mime base64
                           (and transformed model-path) model-path)
                          media))))))))
     (t
      (error "Unsupported media file type: %s" model-path)))))

(defun mevedel-tool-fs-read--pdf-pages (path pages args k)
  "Render PDF PATH PAGES to images according to ARGS.
Call K with a media result plist once every page is rendered."
  (let ((model-path (mevedel-tool-fs-read--visible-path path)))
    (mevedel-tool-fs-read--validate-media-file path "application/pdf" model-path)
    (mevedel-tool-fs-read--ensure-media-capable nil)
    (mevedel-tool-fs-read--with-local-media-source
     path
     (lambda (converter-path)
       (mevedel-tool-fs-read--pdf-page-count
        converter-path
        (lambda (page-count)
          (let ((range (mevedel-tool-fs-read--bounded-pdf-page-range
                        pages page-count))
                (results nil)
                (media nil)
                (total-base64-chars 0))
            (unless (executable-find "pdftoppm")
              (error "'pdftoppm' not installed; install 'poppler-utils' to read PDF pages as images"))
            (cl-labels
                ((render (page)
                   (if (> page (cdr range))
                       (funcall k (mevedel-tool-fs-read--media-read-result
                                   (concat
                                    (mapconcat #'identity (nreverse results) "\n\n")
                                    (when (and page-count (< (cdr range) page-count))
                                      (format "\n\nRendered pages %d-%d of %d. Use Read(file_path=%S, pages=\"%d-\") to continue."
                                              (car range) (cdr range) page-count
                                              model-path (1+ (cdr range)))))
                                   (nreverse media)))
                     (let* ((prefix (make-temp-name
                                     (file-name-concat
                                      (mevedel-tool-fs-read--tool-results-dir)
                                      (format "Read-pdf-page-%d-" page))))
                            (output (concat prefix ".png")))
                       (mevedel-tool-fs-read--helper
                        (lambda (exit-code process-output)
                          (unless (eql 0 exit-code)
                            (error "'pdftoppm' failed while rendering page %d of %s%s"
                                   page model-path
                                   (if (string-empty-p process-output)
                                       ""
                                     (concat ": " process-output))))
                          (mevedel-tool-fs-read--maybe-transform-media
                           output args
                           (lambda (prepared)
                             (let* ((prepared-path (car prepared))
                                    (prepared-mime (cdr prepared))
                                    (_ (mevedel-tool-fs-read--ensure-media-capable
                                        prepared-mime))
                                    (_ (mevedel-tool-fs-read--validate-media-file
                                        prepared-path prepared-mime model-path))
                                    (base64 (mevedel-tool-fs-read--base64-file
                                             prepared-path nil model-path)))
                               (setq total-base64-chars
                                     (+ total-base64-chars (length base64)))
                               (when (> total-base64-chars
                                        mevedel-tool-fs-read--pdf-pages-max-base64-chars)
                                 (error "Rendered PDF pages exceed aggregate media size limit (%d chars)"
                                        mevedel-tool-fs-read--pdf-pages-max-base64-chars))
                               (push (mevedel-tool-fs-read--format-media-result
                                      prepared-path prepared-mime base64
                                      model-path model-path)
                                     results)
                               (push (list :path model-path :mime prepared-mime
                                           :kind 'image :data base64 :source model-path
                                           :page page)
                                     media)
                               (render (1+ page))))))
                        "mevedel-pdftoppm"
                        (list "pdftoppm"
                              "-f" (number-to-string page)
                              "-l" (number-to-string page)
                              "-singlefile"
                              "-png" converter-path prefix)
                        (list converter-path)
                        (list (file-name-directory output)))))))
              (render (car range))))))))))

(defun mevedel-tool-fs-read--missing-file-suggestions (path)
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
            (push (cons (string-distance (downcase name)
                                         (downcase entry-name))
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

(defun mevedel-tool-fs-read--format-missing-file-error (path)
  "Return a friendly missing-file error for PATH."
  (let ((suggestions (unless mevedel-tool-fs-read--resource-address
                       (mevedel-tool-fs-read--missing-file-suggestions path))))
    (concat (format "File %s does not exist"
                    (mevedel-tool-fs-read--visible-path path))
            (when suggestions
              (concat ". Did you mean:\n"
                      (mapconcat (lambda (candidate)
                                   (format "- %s"
                                           (mevedel-tool-fs-read--visible-path
                                            candidate)))
                                 suggestions "\n"))))))

(defun mevedel-tool-fs-read--add-line-numbers (start-line)
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

(defun mevedel-tool-fs-read--truncate-buffer-to-lines (max-lines)
  "Truncate current buffer to MAX-LINES and return non-nil if truncated."
  (goto-char (point-min))
  (forward-line max-lines)
  (unless (eobp)
    (delete-region (point) (point-max))
    t))

(defun mevedel-tool-fs-read--next-line-number ()
  "Return the next original line number after current numbered buffer."
  (save-excursion
    (goto-char (point-max))
    (when (and (bolp) (> (point) (point-min)))
      (backward-char))
    (beginning-of-line)
    (if (looking-at "[[:space:]]*\\([0-9]+\\)\t")
        (1+ (string-to-number (match-string 1)))
      1)))

(defun mevedel-tool-fs-read--continuation-hint (path next-line)
  "Return continuation guidance for PATH starting at NEXT-LINE."
  (format "\n\n... Read output truncated. Use Read(file_path=%S, offset=%d, limit=%d) to continue, or use Grep for targeted searches."
          (mevedel-tool-fs-read--visible-path path)
          next-line mevedel-tool-fs-read--default-limit))

(defun mevedel-tool-fs-read--finalize-read-buffer (path start-line line-truncated-next)
  "Line-number and bound current text Read buffer.
PATH is used in continuation guidance.  START-LINE is the first line
number.  LINE-TRUNCATED-NEXT is the next line when an upstream line cap
already truncated the buffer, or nil.  Return the model-visible string."
  (mevedel-tool-fs-read--add-line-numbers start-line)
  (let (char-truncated-next)
    (when (> (buffer-size) mevedel-tool-fs-read--max-output-chars)
      (goto-char (+ (point-min) mevedel-tool-fs-read--max-output-chars))
      (beginning-of-line)
      (when (= (point) (point-min))
        (end-of-line))
      (delete-region (point) (point-max))
      (setq char-truncated-next (mevedel-tool-fs-read--next-line-number)))
    (let ((next (or char-truncated-next line-truncated-next))
          (content (buffer-substring-no-properties (point-min) (point-max))))
      (if next
          (concat content (mevedel-tool-fs-read--continuation-hint path next))
        content))))

(defun mevedel-tool-fs-read--list-directory-command (path)
  "Return the `rg' command listing directory PATH, after checking PATH."
  (unless (mevedel-tool-fs-executable-find "rg" path)
    (error "'rg' not installed on execution target"))
  (unless (and (file-directory-p path) (file-readable-p path))
    (error "%s is not a readable directory"
           (mevedel-tool-fs-read--visible-path path)))
  (append (list "rg" "--files" "--hidden")
          (mevedel-tool-fs-resource-rg-exclusions
           mevedel-tool-fs-read--resource-address)
          (list "--sort" "path" path)))

(defun mevedel-tool-fs-read--directory-listing (path exit output max-entries)
  "Return (ENTRIES . TRUNCATED-P) from `rg' OUTPUT and EXIT listing PATH."
  (let ((max (or max-entries 1000)))
    (cond
     ((eql exit 0)
      (let* ((raw (split-string output "\n" t))
             (model-root (mevedel-tool-fs-model-path path))
             (all (mapcar (lambda (s)
                            (setq s (replace-regexp-in-string "\\\\" "/" s))
                            (file-relative-name s model-root))
                          raw))
             (truncated (> (length all) max))
             (entries (if truncated (seq-take all max) all)))
        (cons entries truncated)))
     ((eql exit 1) (cons nil nil))
     (t (error "`rg' exited with code %s listing %s" exit
               (mevedel-tool-fs-read--visible-path path))))))

(defun mevedel-tool-fs-read-list-directory (path k &optional max-entries)
  "List files under directory PATH, respecting .gitignore, then call K.

Uses `rg --files --hidden' without following descendant symbolic links, so the
listing is gitignore-aware and stays within PATH.  Entries are sorted by path.
K receives a cons cell (ENTRIES . TRUNCATED-P) where ENTRIES is a list of
paths relative to PATH and TRUNCATED-P is non-nil if the listing was capped
at MAX-ENTRIES (defaulting to 1000).  Signals an error if rg is missing,
PATH is not a readable directory, or rg exits with an unexpected code."
  (mevedel-tool-fs-read--helper
   (lambda (exit output)
     (funcall k (mevedel-tool-fs-read--directory-listing
                 path exit output max-entries)))
   "mevedel-list-directory"
   (mevedel-tool-fs-read--list-directory-command path)
   (list path)))

(defun mevedel-tool-fs-read-list-directory-now (path &optional max-entries)
  "Return PATH's listing as for `mevedel-tool-fs-read-list-directory'.
Synchronous, for mention expansion; the Read tool waits asynchronously."
  (pcase-let ((`(,exit . ,output)
               (mevedel-tool-fs-read--helper-now
                "mevedel-list-directory"
                (mevedel-tool-fs-read--list-directory-command path)
                (list path))))
    (mevedel-tool-fs-read--directory-listing path exit output max-entries)))

(defun mevedel-tool-fs-read-slurp-file-contents
    (path &optional offset limit display-path)
  "Return file PATH content with OFFSET and LIMIT.
Apply Read-tool safety validation before reading.

DISPLAY-PATH, when non-nil, is used in model-facing continuation text.

Validate readability, reject directories, binary files, and blocking
device paths, and resolve symlinks.  For full-file reads (both OFFSET and
LIMIT nil), enforce the 512 KB size cap and return at most
`mevedel-tool-fs-read--default-limit' lines.  For range reads, default
OFFSET to 1 and LIMIT to `mevedel-tool-fs-read--default-limit' lines.

Returns the bounded content string with line numbers; signals an error on
any validation failure.  Callers that want graceful degradation should
wrap in `condition-case'."
  (unless (file-readable-p path)
    (error "File %s is not readable" (mevedel-tool-fs-read--visible-path path)))
  (when (file-directory-p path)
    (error "Cannot read directory %s as file"
           (mevedel-tool-fs-read--visible-path path)))
  (when (mevedel-tool-fs-read--binary-extension-p path)
    (let ((ext (file-name-extension path)))
      (error "Cannot read binary file (type: .%s): %s"
             ext (mevedel-tool-fs-read--visible-path path))))
  (when (mevedel-tool-fs-read--blocked-device-p path)
    (error "Cannot read %s: device file would block or produce infinite output"
           (mevedel-tool-fs-read--visible-path path)))
  (if (zerop (file-attribute-size (file-attributes path)))
      ""
    (let ((start-line (max 1 (or offset 1)))
          (num-lines (or limit mevedel-tool-fs-read--default-limit)))
      (if (and (not offset) (not limit))
          (let ((file-size (file-attribute-size (file-attributes path))))
            (when (> file-size (* 512 1024))
              (error "File is too large (> 512 KB).  Use offset and limit to read specific portions"))
            (with-temp-buffer
              (insert-file-contents path)
              (let ((line-truncated-p
                     (mevedel-tool-fs-read--truncate-buffer-to-lines num-lines)))
                (mevedel-tool-fs-read--finalize-read-buffer
                 (or display-path path)
                 1 (and line-truncated-p (1+ num-lines))))))
        (let* ((file-size (file-attribute-size (file-attributes path)))
               (chunk-size (min file-size (* 512 1024)))
               (coding
                (with-temp-buffer
                  (insert-file-contents path nil 0 chunk-size)
                  last-coding-system-used))
               (text-coding
                (coding-system-change-eol-conversion coding 'unix))
               (signature (encode-coding-string "" text-coding))
               (encoded-newlines
                (mapcar
                 (lambda (newline)
                   (substring (encode-coding-string newline text-coding)
                              (length signature)))
                 (if (eq (coding-system-eol-type coding) 2)
                     '("\r" "\n")
                   '("\n"))))
               (newline-width
                (apply #'max (mapcar #'length encoded-newlines)))
               (byte-offset 0)
               (buffer-start-offset 0)
               (lines-to-skip (1- start-line))
               (lines-to-read num-lines)
               line-truncated-next)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (cl-labels
                ((insert-chunk
                  ()
                  (let ((chunk-end
                         (min file-size (+ byte-offset chunk-size))))
                    (goto-char (point-max))
                    (insert-file-contents-literally
                     path nil byte-offset chunk-end)
                    (setq byte-offset chunk-end)))
                 (next-newline
                  ()
                  (catch 'found
                    (while t
                      (let ((start (point)) found)
                        (dolist (newline encoded-newlines)
                          (goto-char start)
                          (let (candidate match)
                            (while
                                (and (setq match
                                           (search-forward newline nil t))
                                     (not
                                      (zerop
                                       (mod
                                        (- (+ buffer-start-offset
                                              (- (match-beginning 0)
                                                 (point-min)))
                                           (length signature))
                                        newline-width))))
                              (goto-char (1+ (match-beginning 0)))
                              (setq match nil))
                            (when match
                              (setq candidate match))
                            (when (and candidate
                                       (or (null found)
                                           (< candidate found)))
                              (setq found candidate))))
                        (when found
                          (goto-char found)
                          (throw 'found found)))
                      (when (>= byte-offset file-size)
                        (throw 'found nil))
                      (let ((resume
                             (max (point-min)
                                  (- (point-max)
                                     (1- newline-width)))))
                        (insert-chunk)
                        (goto-char resume))))))
              (let ((range-found t))
                (cl-block skip
                  (dotimes (_ lines-to-skip)
                    (unless (next-newline)
                      (setq range-found nil)
                      (cl-return-from skip))
                    (let ((deleted (- (point) (point-min))))
                      (delete-region (point-min) (point))
                      (cl-incf buffer-start-offset deleted))
                    (goto-char (point-min))))
                (when (and range-found
                           (> lines-to-skip 0)
                           (= (point-min) (point-max))
                           (>= byte-offset file-size))
                  ;; The skip consumed the final newline exactly and no
                  ;; bytes remain: the offset names the first line past
                  ;; the end, which is the one a stale continuation hint
                  ;; hands the model.  An empty buffer with bytes still
                  ;; unread just means the next chunk has not arrived.
                  (setq range-found nil))
                (if (not range-found)
                    (error "Read offset %d starts after the last line of %s"
                           start-line
                           (mevedel-tool-fs-read--visible-path
                            (or display-path path)))
                  (let (selected-end)
                    (cl-block read
                      (while (> lines-to-read 0)
                        (if (next-newline)
                            (progn
                              (cl-decf lines-to-read)
                              (setq selected-end (point)))
                          (when (> (point-max) (point-min))
                            (setq selected-end (point-max)))
                          (cl-return-from read))))
                    (if selected-end
                        (progn
                          (when (or (< selected-end (point-max))
                                    (< byte-offset file-size))
                            (setq line-truncated-next
                                  (+ start-line num-lines)))
                          (delete-region selected-end (point-max)))
                      (erase-buffer))))))
            (when (> (point-max) (point-min))
              (goto-char (point-min))
              (unless (= start-line 1)
                (insert signature))
              (set-buffer-multibyte t)
              (let ((coding-system-for-read coding))
                (decode-coding-inserted-region
                 (point-min) (point-max) path)))
            (mevedel-tool-fs-read--finalize-read-buffer
             (or display-path path)
             start-line line-truncated-next)))))))

(defun mevedel-tool-fs-read--session-artifact-context (path)
  "Return `(SESSION . LOGICAL)' when PATH is in a remote session store."
  (when-let* ((session (bound-and-true-p mevedel--session))
              (target (mevedel-session-execution-target session))
              ((mevedel-execution-target-remote-p target))
              (save-path (mevedel-session-save-path session)))
    (let* ((root (file-name-as-directory (expand-file-name save-path)))
           (path (expand-file-name path)))
      (when (string-prefix-p root path)
        (cons session (substring path (length root)))))))

(defun mevedel-tool-fs-read-session-artifact
    (session logical path offset limit)
  "Read SESSION artifact LOGICAL under model-facing PATH.

OFFSET and LIMIT select the requested line range.  Remote fixed-path caches
are deliberately ignored."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (when (mevedel-tool-fs-read--binary-extension-p path)
    (error "Cannot read binary session artifact: %s"
           (mevedel-tool-fs-model-path path)))
  (let* ((bytes (progn
                  (mevedel-session-artifacts-read-artifact
                   session logical)))
         (suffix (when-let* ((extension (file-name-extension path)))
                   (concat "." extension)))
         (temporary (make-temp-file "mevedel-session-artifact-" nil suffix)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert bytes)
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max)
                            temporary nil 'silent)))
          (if (string-empty-p bytes)
              (format "<system-reminder>\n\
File %s exists but is empty (0 bytes). This is the actual file \
content, not a read failure.\n</system-reminder>"
                      (mevedel-tool-fs-model-path path))
            (mevedel-tool-fs-read-slurp-file-contents
             temporary offset limit path)))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun mevedel-tool-fs-read--file (args k)
  "Read file contents, then call K with the result.
ARGS is a plist with :file_path and optional :offset, :limit, :pages,
:max_width, :max_height, and :max_tokens.  Text reads call K before
returning; media reads may continue from helper callbacks."
  (cl-block mevedel-tool-fs-read--file
    (let* ((args (mevedel-tool-fs-read--normalize-read-args args))
           (filename (plist-get args :file_path))
           (offset (plist-get args :offset))
           (limit (plist-get args :limit)))
      (when-let* ((artifact
                  (mevedel-tool-fs-read--session-artifact-context filename)))
        (when (plist-get args :pages)
          (error "Parameter pages is only supported for PDF files"))
        (when (mevedel-tool-fs-read--media-transform-requested-p args)
          (error "`max_width', `max_height', and `max_tokens' are only supported for image files and PDF page images"))
        (cl-return-from mevedel-tool-fs-read--file
          (funcall k (mevedel-tool-fs-read-session-artifact
                      (car artifact) (cdr artifact) filename offset limit))))
    (unless (file-exists-p filename)
      (error "%s" (mevedel-tool-fs-read--format-missing-file-error filename)))
    (unless (file-readable-p filename)
      (error "File %s is not readable"
             (mevedel-tool-fs-read--visible-path filename)))
    (when (file-directory-p filename)
      (error "Cannot read directory %s as file"
             (mevedel-tool-fs-read--visible-path filename)))
    (when (mevedel-tool-fs-read--blocked-device-p filename)
      (error "Cannot read %s: device file would block or produce infinite output"
             (mevedel-tool-fs-read--visible-path filename)))
    (if (mevedel-tool-fs-read-media-mime-type filename)
        (progn
          ;; A model without document input reads a whole PDF as its
          ;; leading page images; the result says how to continue.
          (when (and (mevedel-tool-fs-read-pdf-media-p filename)
                     (null (plist-get args :pages))
                     (mevedel-tool-fs-read--pdf-page-images-only-p))
            (setq args (plist-put args :pages "1-")))
          ;; Strict tool schemas make models fill every field, so a media
          ;; read arrives with whatever offset and limit they chose.  A line
          ;; range means nothing for media; ignore it rather than fail.
          (unless (or (mevedel-tool-fs-read-pdf-media-p filename)
                      (null (plist-get args :pages)))
            (error "Parameter pages is only supported for PDF files"))
          (when (and (mevedel-tool-fs-read-pdf-media-p filename)
                     (null (plist-get args :pages))
                     (mevedel-tool-fs-read--media-transform-requested-p args))
            (error "`max_width', `max_height', and `max_tokens' are only supported for image files and PDF page images"))
          (mevedel-tool-fs-read--ensure-media-capable
           (mevedel-tool-fs-read--media-result-mime filename args))
          (let ((dedup-key (mevedel-tool-fs-read--media-dedup-key args)))
            (cond
             ((and (mevedel-tool-fs-read--dedup-p)
                   (mevedel-session-read-is-duplicate-p
                    mevedel--session filename dedup-key nil))
              (funcall k (format "File %s unchanged since last read.  Reuse the previous contents."
                                 (mevedel-tool-fs-read--visible-path filename))))
             (t
              (let ((whole-pdf (and (mevedel-tool-fs-read-pdf-media-p filename)
                                    (null (plist-get args :pages)))))
                (cl-labels
                    ((record (result)
                       (when (mevedel-tool-fs-read--dedup-p)
                         (mevedel-session-record-file-access
                          mevedel--session filename 'read dedup-key nil))
                       (funcall k result))
                     (read-media ()
                       (condition-case err
                           (mevedel-tool-fs-read--media-file
                            filename args
                            (lambda (result)
                              (if (not whole-pdf)
                                  (record result)
                                (mevedel-tool-fs-read--pdf-page-count
                                 filename
                                 (lambda (page-count)
                                   (record
                                    (if (mevedel-tool-fs-read-large-pdf-p
                                         filename page-count)
                                        (mevedel-tool-fs-read--append-system-reminder
                                         result
                                         (mevedel-tool-fs-read-format-large-pdf-reminder
                                          filename page-count))
                                      result)))))))
                         (error
                          (let ((message (error-message-string err)))
                            (if (and whole-pdf
                                     (string-match-p "Media file is too large"
                                                     message))
                                (mevedel-tool-fs-read--pdf-page-count
                                 filename
                                 (lambda (page-count)
                                   (error "%s%s"
                                          message
                                          (mevedel-tool-fs-read--append-system-reminder
                                           ""
                                           (mevedel-tool-fs-read-format-large-pdf-reminder
                                            filename page-count)))))
                              (signal (car err) (cdr err))))))))
                  (if (and whole-pdf (file-remote-p filename))
                      (mevedel-tool-fs-read--with-local-media-source
                       filename (lambda (_source) (read-media)))
                    (read-media))))))))
      (when (plist-get args :pages)
        (error "Parameter pages is only supported for PDF files"))
      (when (mevedel-tool-fs-read--media-transform-requested-p args)
        (error "`max_width', `max_height', and `max_tokens' are only supported for image files and PDF page images"))
      (when (mevedel-tool-fs-read--binary-extension-p filename)
        (let ((ext (file-name-extension filename)))
          (error "Cannot read binary file (type: .%s): %s" ext
                 (mevedel-tool-fs-read--visible-path filename))))
      (funcall
       k
       (cond
        ((and (mevedel-tool-fs-read--dedup-p)
              (mevedel-session-read-is-duplicate-p
               mevedel--session filename offset limit))
         (format "File %s unchanged since last read.  Reuse the previous contents."
                 (mevedel-tool-fs-read--visible-path filename)))
        ((zerop (file-attribute-size (file-attributes filename)))
         (when (mevedel-tool-fs-read--dedup-p)
           (mevedel-session-record-file-access
            mevedel--session filename 'read offset limit))
         (format "<system-reminder>\n\
File %s exists but is empty (0 bytes). This is the actual file \
content, not a read failure.\n</system-reminder>"
                 (mevedel-tool-fs-read--visible-path filename)))
        (t
         (let ((content (mevedel-tool-fs-read-slurp-file-contents
                         filename offset limit)))
           (when (mevedel-tool-fs-read--dedup-p)
             (mevedel-session-record-file-access
              mevedel--session filename 'read offset limit))
           content))))))))

(defun mevedel-tool-fs-read--resource-directory (path address k)
  "Call K with a logical listing for resource ADDRESS rooted at PATH."
  (condition-case err
      (mevedel-tool-fs-read-list-directory
       path
       (lambda (listing)
         (pcase-let ((`(,entries . ,truncated) listing))
           (funcall k (mevedel-tool-fs-read--resource-listing
                       path address entries truncated)))))
    (error
     (error "Cannot read resource %s: %s"
            address (error-message-string err)))))

(defun mevedel-tool-fs-read--resource-listing (path address entries truncated)
  "Return resource ADDRESS's listing of ENTRIES under PATH, TRUNCATED or not."
  (let ((result
               (mapconcat
                (lambda (entry)
                  (mevedel-tool-fs-resource-child-address
                   address path (expand-file-name entry path)))
                entries "\n")))
          (if truncated
              (concat result
                      (unless (string-empty-p result) "\n")
                      (format "... Results truncated (limit: 1000). Use Read(file_path=%S) for a narrower resource."
                              address))
            (if (string-empty-p result)
                (format "No files found under %s" address)
              result))))

(defun mevedel-tool-fs-read--virtual-text (text args address)
  "Read virtual TEXT with the normal bounded text Read behavior."
  (let* ((args (mevedel-tool-fs-read--normalize-read-args args))
         (offset (plist-get args :offset))
         (limit (plist-get args :limit)))
    (when (or (plist-get args :pages)
              (mevedel-tool-fs-read--media-transform-requested-p args))
      (error "Virtual resources only support text Read options"))
    (unless (stringp text)
      (error "Internal resource error: %s did not return text" address))
    (if (string-empty-p text)
        ""
      (when (and (null offset) (null limit)
                 (> (string-bytes text) (* 512 1024)))
        (error "Resource is too large (> 512 KB). Use offset and limit to read specific portions"))
      (with-temp-buffer
        (insert text)
        (let ((start-line (max 1 (or offset 1)))
              (num-lines (or limit mevedel-tool-fs-read--default-limit)))
          (goto-char (point-min))
          (when (> start-line 1)
            (when (or (not (zerop (forward-line (1- start-line))))
                      (eobp))
              (error "Read offset %d starts after the last line of %s"
                     start-line address))
            (delete-region (point-min) (point)))
          (let ((line-truncated-p
                 (mevedel-tool-fs-read--truncate-buffer-to-lines num-lines)))
            (mevedel-tool-fs-read--finalize-read-buffer
             address start-line
             (and line-truncated-p (+ start-line num-lines)))))))))

(defun mevedel-tool-fs-read--shared (args descriptor address k)
  "Read the shared item view DESCRIPTOR names at ADDRESS, then call K.
The session's editing host answers asynchronously; K continues in the
Read's own context, as a helper continuation does.  Text pages like other
virtual text with ARGS, and an image goes through ordinary media delivery
from a private copy the Read releases."
  (let ((values (mapcar (lambda (symbol) (and (boundp symbol) (symbol-value symbol)))
                        mevedel-tool-fs-read--continuation-variables)))
    (mevedel-tool-editing-view
     (plist-get descriptor :shared-view)
     (mevedel-pipeline-handler-resumable
      (lambda (view)
        (cl-progv mevedel-tool-fs-read--continuation-variables values
          (condition-case err
              (cond
               ((plist-get view :error)
                (error "Cannot read %s: %s" address (plist-get view :error)))
               ((plist-get view :text)
                (funcall k (mevedel-tool-fs-read--virtual-text
                            (plist-get view :text) args address)))
               (t
                (let ((file (make-temp-file
                             "mevedel-shared-" nil
                             (pcase (plist-get view :mime)
                               ("image/jpeg" ".jpg") ("image/webp" ".webp") (_ ".png")))))
                  (push (lambda () (delete-file file)) (car mevedel-tool-fs-read--cleanups))
                  (let ((coding-system-for-write 'no-conversion))
                    (with-temp-file file
                      (set-buffer-multibyte nil)
                      (insert (base64-decode-string (plist-get view :data)))))
                  (let ((mevedel-tool-fs-read--resource-address address)
                        (read-args (copy-sequence args)))
                    (mevedel-tool-fs-read--file (plist-put read-args :file_path file) k)))))
            (error
             (if mevedel-tool-fs-read--fail
                 (funcall mevedel-tool-fs-read--fail err)
               (signal (car err) (cdr err)))))))))))

(defun mevedel-tool-fs-read--resource-path (args path address k)
  "Read authorized PATH for ADDRESS with bounded Read ARGS, then call K."
  (cond
   ((and (listp path) (plist-member path :shared-view))
    (mevedel-tool-fs-read--shared args path address k))
   ((and (listp path) (plist-get path :virtual))
    (funcall k (mevedel-tool-fs-read--virtual-text
                (plist-get path :result) args address)))
   (t
    (unless (and (stringp path) (file-exists-p path))
      (error "File not found: %s" address))
    (when (and (string-prefix-p "memory://" address)
               (mevedel-tool-fs-read-media-mime-type path))
      (error "Memory resources only support text reads"))
    (let ((mevedel-tool-fs-read--resource-address address))
      (if (file-directory-p path)
          (mevedel-tool-fs-read--resource-directory path address k)
        (let ((read-args (copy-sequence args)))
          (plist-put read-args :file_path path)
          (mevedel-tool-fs-read--file read-args k)))))))

(defun mevedel-tool-fs-read (callback args)
  "Read ARGS and deliver a canonical handler envelope to CALLBACK.
Saved history returns a canceller while preparation continues asynchronously."
  (let* ((address (plist-get args :file_path))
         (attempt (mevedel-tool-fs-resource-attempt address)))
    (if attempt
        (mevedel-resource-execute
         attempt
         (lambda (path authored)
           (if (and (listp path) (plist-member path :history-workspace))
               (mevedel-history-search-start callback args path)
             (mevedel-tool-fs-read--run
              (lambda (k) (mevedel-tool-fs-read--resource-path args path authored k))
              (lambda (result)
                (funcall callback (mevedel-tool-fs-handler-result result)))))))
      (mevedel-tool-fs-read--run
       (lambda (k) (mevedel-tool-fs-read--file args k))
       (lambda (result)
         ;; Instructions follow a successful read, which a failed one
         ;; would not have reached before reads could wait for helpers.
         (unless (eq 'error (plist-get (mevedel-tool-fs-handler-result result) :status))
           (mevedel-tool-fs-read--queue-workspace-instructions
            (plist-get (mevedel-tool-fs-read--normalize-read-args args) :file_path)))
         (funcall callback (mevedel-tool-fs-handler-result result)))))))

(defun mevedel-tool-fs-read--workspace-instruction-owner ()
  "Return the canonical conversation owner for the current Read."
  (or (and mevedel--agent-invocation
           (mevedel-agent-invocation-path mevedel--agent-invocation))
      "/root"))

(defun mevedel-tool-fs-read--instruction-hashes (scope)
  "Return acknowledged instruction hashes for session or request SCOPE."
  (if (mevedel-request-p scope)
      (mevedel-request-workspace-instruction-hashes scope)
    (mevedel-session-workspace-instruction-hashes scope)))

(defun mevedel-tool-fs-read--workspace-instruction (scope owner file)
  "Render instruction FILE for SCOPE and OWNER with a deferred hash commit.
A removed file explicitly withdraws its earlier instructions.  Its empty-content
hash keeps that absence restorable until new instructions are discovered."
  (let* ((present (file-exists-p file))
         (content (if present (with-temp-buffer (insert-file-contents file) (buffer-string)) ""))
         (hash (secure-hash 'sha256 content))
         (key (list owner file)))
    (list :hash hash
          :body (if present
                    (format
                     (concat "The following host-loaded path-scoped workspace instructions apply. "
                             "These contents replace earlier instructions from this file; other paths are unchanged. "
                             "More deeply nested instructions override broader ones on conflict:"
                             "\n\n<workspace-instructions path=\"%s\">\n%s\n</workspace-instructions>")
                     (xml-escape-string file) content)
                  (format "Path-scoped instruction file %s is no longer present. Its earlier instructions no longer apply; other path scopes are unchanged."
                          file))
          :commit (lambda ()
                    (let ((hashes (mevedel-tool-fs-read--instruction-hashes scope)))
                      (setf (alist-get key hashes nil nil #'equal) hash)
                      (if (mevedel-request-p scope)
                          (setf (mevedel-request-workspace-instruction-hashes scope) hashes)
                        (setf (mevedel-session-workspace-instruction-hashes scope) hashes)))))))

(defun mevedel-tool-fs-read-workspace-context (scope owner)
  "Prepare current path instructions already learned by OWNER in SCOPE.
SCOPE is the session, or the request for a fresh isolated conversation.
Return reminder entries and deferred commits, ordered broadest scope first and
AGENTS.local.md after AGENTS.md.  Do not acknowledge them until delivery."
  (let ((files (cl-loop for (key . _hash) in (mevedel-tool-fs-read--instruction-hashes scope)
                        when (equal owner (car key)) collect (cadr key)))
        entries commits)
    (setq files
          (sort (delete-dups files)
                (lambda (a b)
                  (let ((a-dir (file-name-directory a)) (b-dir (file-name-directory b)))
                    (if (equal a-dir b-dir)
                        (let ((a-local (equal (file-name-nondirectory a) "AGENTS.local.md"))
                              (b-local (equal (file-name-nondirectory b) "AGENTS.local.md")))
                          (if (eq a-local b-local) (string-lessp a b) (not a-local)))
                      (string-lessp a-dir b-dir))))))
    (dolist (file files)
      (let ((instruction (mevedel-tool-fs-read--workspace-instruction scope owner file)))
        (push (list :type (cons 'workspace-instructions file) :body (plist-get instruction :body)
                    :hash (plist-get instruction :hash))
              entries)
        (push (plist-get instruction :commit) commits)))
    (list :entries (nreverse entries) :commits (nreverse commits))))

(defun mevedel-tool-fs-read--queue-workspace-instructions (path)
  "Queue newly applicable workspace instructions after reading PATH."
  (when-let* ((session (bound-and-true-p mevedel--session))
              (workspace (mevedel-session-workspace session))
              ((file-exists-p path))
              (path (file-truename path))
              (target-dir (file-name-directory path))
              (cwd (file-name-as-directory
                    (file-truename
                     (mevedel-session-working-directory session))))
              ((file-in-directory-p target-dir cwd)))
    (let ((baseline
           (mapcar #'file-truename
                   (mevedel-system-workspace-config-files workspace cwd)))
          (owner (mevedel-tool-fs-read--workspace-instruction-owner))
          (scope (if (and (not mevedel--agent-invocation)
                          (bound-and-true-p mevedel--current-request)
                          (or (mevedel-request-directive-uuid mevedel--current-request)
                              (plist-get (mevedel-request-context mevedel--current-request)
                                         :mevedel-native-isolated)))
                     mevedel--current-request
                   session)))
      ;; Queue one event per instruction file, broadest scope first, so a
      ;; shared ancestor read via several sibling directories in the same
      ;; turn coalesces into a single delivery.
      (dolist (file (mevedel-system-workspace-config-files
                     workspace target-dir))
        (let ((file (file-truename file)))
          (unless (or (equal file path) (member file baseline))
            (let* ((instruction (mevedel-tool-fs-read--workspace-instruction scope owner file))
                   (hash (plist-get instruction :hash))
                   (key (list owner file)))
              (unless (equal hash
                             (alist-get
                              key
                              (mevedel-tool-fs-read--instruction-hashes scope)
                              nil nil #'equal))
                (mevedel-reminders-queue-turn-event
                 (current-buffer)
                 (cons 'workspace-instructions file)
                 (plist-get instruction :body)
                 (plist-get instruction :commit))))))))))

(provide 'mevedel-tool-fs-read)

;;; mevedel-tool-fs-read.el ends here
