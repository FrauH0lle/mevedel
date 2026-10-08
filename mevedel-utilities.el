;;; mevedel-utilities.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the package-version command and shared helpers that do not belong to
;; any single mevedel module: colour tinting for overlay styling, tag-query
;; prefix/infix conversion, environment-info string assembly for system
;; prompts, synthetic transcript insertion and response-marker handling, and
;; various text and path manipulation utilities.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'timer)

;; `diff'
(declare-function diff-no-select "diff"
                  (old new &optional switches no-async buf))

;; `gptel'
(declare-function gptel--display-reasoning-stream "ext:gptel" (text info))
(defvar gptel-include-tool-results)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-prefix-alist)
(defvar gptel-response-separator)

;; `mevedel-execution'
(declare-function mevedel-execution-start-helper
                  "mevedel-execution"
                  (callback name command read-paths writable-roots &rest keys))
(autoload 'mevedel-execution-start-helper "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-label
                  "mevedel-execution-target" (target &optional directory))
(declare-function mevedel-execution-target-readiness
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-label "mevedel-execution-target")

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))
(autoload 'mevedel-mention-bindings-ranges "mevedel-mention-bindings")

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-run-program
                  "mevedel-session-control-fs" (operations &optional lock-directory))
(autoload 'mevedel-session-control-fs-physical-path "mevedel-session-control-fs")
(autoload 'mevedel-session-control-fs-run-program "mevedel-session-control-fs")

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-restore-ignored-properties
                  "mevedel-transcript" (start end))
(autoload 'mevedel-transcript-restore-ignored-properties "mevedel-transcript")

;; `mevedel-transport'
(defvar mevedel-transport--held-timers)
(defvar mevedel-transport--suspended-timers)

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `org'
(declare-function org-mode "org" ())
(autoload 'org-mode "org")
(defvar org-element-cache-persistent)
(defvar org-inhibit-startup)
(defvar org-mode-hook)

;; `org-indent'
(declare-function org-indent-mode "org-indent" (&optional arg))
(autoload 'org-indent-mode "org-indent")

;; `outline'
(defvar outline-mode-hook)

;; `saveplace'
(defvar save-place-mode)

;; `subr'
(defvar read-eval)

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-flush-file-properties "tramp-cache" (key file))


;;
;;; Package metadata

;;;###autoload
(defun mevedel-library-source-directory (file)
  "Return the canonical source directory associated with library FILE.
For bytecode, prefer an existing sibling .el file: package managers may
symlink source into a build directory while compiling a separate .elc there.
Without that source sibling, keep FILE's own directory.  This locates package
data, not resource descendants; it does not relax their containment checks."
  (let ((source (and (string-suffix-p ".elc" file)
                     (substring file 0 -1))))
    (file-name-directory
     (file-truename (if (and source (file-regular-p source)) source file)))))

;;;###autoload
(defun mevedel-version (&optional here message)
  "Return the current version of mevedel.

Interactively, or when MESSAGE is non-nil, show it in echo area.  With
prefix argument, or when HERE is non-nil, insert it at point."
  (interactive (list (or current-prefix-arg 'interactive)))
  (let ((version "v0.5.0"))
    (cond
     ((or message (called-interactively-p 'any))
      (message "mevedel %s" version))
     (here (insert (format "mevedel %s" version)))
     (t version))))


;;
;;; External helpers

(defun mevedel-start-helper-capturing-output
    (callback name command read-paths &optional writable-roots session)
  "Start helper COMMAND as NAME and call CALLBACK with its settlement.
CALLBACK receives EXIT-CODE, OUTPUT and ERROR; ERROR is condition data when
the helper failed to run or its owner was torn down, and nil otherwise.
READ-PATHS and WRITABLE-ROOTS declare the helper's filesystem boundary.
OUTPUT is passed unchanged so callers may interpret its whitespace.
CALLBACK runs exactly once, and may run before this returns: a refused
launch settles immediately.  Return the helper's idempotent cancellation
function, or nil when it could not start.

Callers continue from CALLBACK rather than waiting: a wait would re-enter
the event loop from wherever it was called, including process callbacks
that inhibit quitting, and the helper's own settlement needs that loop."
  (let (settled)
    (cl-flet ((settle (result)
                (unless settled
                  (setq settled t)
                  (funcall callback (plist-get result :exit-code)
                           (plist-get result :output)
                           (plist-get result :error)))))
      (let (start-error cancel)
        (condition-case err
            (setq cancel
                  (mevedel-execution-start-helper
                   #'settle name command read-paths writable-roots
                   :session (or session (bound-and-true-p mevedel--session))
                   :owner (mevedel-current-origin)
                   :teardown-callback
                   (lambda ()
                     (settle '(:error (error "Helper owner was torn down"))))))
          ;; An error after settlement was raised by CALLBACK itself, run
          ;; synchronously by a refused launch; it is not a failed start.
          (error (if settled
                     (signal (car err) (cdr err))
                   (setq start-error err))))
        (when start-error (settle (list :error start-error)))
        cancel))))

(defun mevedel--diff-label (side filepath)
  "Return the unified-diff label for SIDE of FILEPATH.
SIDE is \"a\" or \"b\".  Those prefixes are a git-patch convention for a
repository-relative path; prepending one to an absolute path spells it
`a//home/user/file', so an absolute FILEPATH is labelled as itself."
  (if (file-name-absolute-p filepath)
      filepath
    (concat side "/" filepath)))

(defun mevedel-generate-diff
    (original modified filepath callback &optional labels-real)
  "Diff ORIGINAL against MODIFIED for FILEPATH, then call CALLBACK.
CALLBACK receives the unified DIFF and ERROR: DIFF is a string, empty when
the contents match, and nil exactly when ERROR carries the failure's
condition data.  When LABELS-REAL is nil, empty content is labelled
`/dev/null'.  Otherwise both sides carry FILEPATH, prefixed `a/' and `b/'
when it is relative.  Spool Unicode as UTF-8 while preserving literal cache
bytes, without interactive coding-system selection.  Failing to spool the
snapshots signals instead.  Return the helper's cancellation function, or
nil when it could not start."
  (let* ((orig-file (make-temp-file "mevedel-orig-"))
         (mod-file (make-temp-file "mevedel-mod-"))
         (remove (lambda ()
                   (dolist (file (list orig-file mod-file))
                     (when (file-exists-p file) (delete-file file))))))
    (condition-case err
        ;; Cached literal reads contain eight-bit characters.  Explicit
        ;; UTF-8 writes preserve those bytes and encode decoded text.
        (let ((coding-system-for-write 'utf-8-unix))
          (with-temp-file orig-file (when original (insert original)))
          (with-temp-file mod-file (when modified (insert modified))))
      (error
       (funcall remove)
       (signal (car err) (cdr err))))
    ;; Both sides are local snapshots, so the diff runs beside Emacs even
    ;; under an ambient remote session.
    (let ((mevedel--session nil))
      (mevedel-start-helper-capturing-output
       (lambda (_exit-code output error)
         (funcall remove)
         (funcall callback
                  (and (not error)
                       (cond ((string-empty-p output) "")
                             ((string-suffix-p "\n" output) output)
                             (t (concat output "\n"))))
                  error))
       "mevedel-diff"
       (list "diff" "-u"
             "--label" (if (or labels-real
                               (and original (not (string-empty-p original))))
                           (mevedel--diff-label "a" filepath)
                         "/dev/null")
             "--label" (if (or labels-real
                               (and modified (not (string-empty-p modified))))
                           (mevedel--diff-label "b" filepath)
                         "/dev/null")
             orig-file mod-file)
       (list orig-file mod-file)))))



;;
;;; Executable lookup

(defvar mevedel--executable-cache (make-hash-table :test #'equal)
  "Resolved executables, keyed by (REMOTE . NAME); REMOTE is nil when local.

`executable-find' walks PATH with a `file-directory-p' and a
`file-executable-p' per entry, so one remote lookup is many round trips.
The callers ask per tool call and per spawn, from inside gptel's curl
sentinel -- which TRAMP re-runs from the wait loop of a command already in
flight, putting the nested lookup on a busy connection.

An executable does not appear or vanish under a running session, so the
answer is cached for the process.  Clear this to pick up a target whose
tooling changed mid-session.")

(defun mevedel--executable-find (name &optional remote)
  "Return NAME's path on REMOTE, or locally when REMOTE is nil.

Cached; see `mevedel--executable-cache'.  A negative result is cached too,
because a missing tool is looked up just as often as a present one."
  (let* ((key (cons remote name))
         (cached (gethash key mevedel--executable-cache 'miss)))
    (if (eq cached 'miss)
        (puthash key
                 (if remote (executable-find name remote) (executable-find name))
                 mevedel--executable-cache)
      cached)))


;;
;;; Display text

(defun mevedel--unified-diff (original current &optional context-lines)
  "Return the unified diff hunks between ORIGINAL and CURRENT text, or nil.
CONTEXT-LINES defaults to diff's three lines of context."
  (let ((original-buffer (generate-new-buffer " *mevedel-diff-original*"))
        (current-buffer (generate-new-buffer " *mevedel-diff-current*"))
        (output-buffer (generate-new-buffer " *mevedel-diff-output*")))
    (unwind-protect
        (progn
          (with-current-buffer original-buffer (insert original))
          (with-current-buffer current-buffer (insert current))
          (diff-no-select original-buffer current-buffer
                          (if context-lines (format "-U%d" context-lines) "-u")
                          t output-buffer)
          (with-current-buffer output-buffer
            (goto-char (point-min))
            (when (re-search-forward "^@@" nil t)
              (beginning-of-line)
              ;; `diff-no-select' appends its own completion notice; the
              ;; hunks end where that notice begins.
              (let ((start (point))
                    (end (if (re-search-forward "^Diff finished" nil t)
                             (match-beginning 0)
                           (point-max))))
                (string-trim-right
                 (buffer-substring-no-properties start end))))))
      (kill-buffer original-buffer)
      (kill-buffer current-buffer)
      (kill-buffer output-buffer))))

(defun mevedel--truncate-bytes (text limit marker)
  "Return TEXT within LIMIT UTF-8 bytes, ending in MARKER when truncated.
Truncation never splits a multibyte character.  MARKER must fit LIMIT."
  (if (<= (string-bytes text) limit)
      text
    (let ((available (- limit (string-bytes marker))))
      (when (< available 0) (error "Byte budget cannot hold its omission marker"))
      (concat (decode-coding-string (string-limit text available nil 'utf-8) 'utf-8)
              marker))))

(defun mevedel--duration-label (seconds)
  "Return a compact whole-second label for SECONDS, such as \"12s\".
Longer spans read \"3m 05s\" or \"1h 02m\".  Elapsed time shown to people
keeps one-second resolution: tenths only flicker, and keeping them
current would cost a redraw several times a second."
  (let ((total (max 0 (floor (or seconds 0)))))
    (cond
     ((< total 60) (format "%ds" total))
     ((< total 3600) (format "%dm %02ds" (/ total 60) (% total 60)))
     (t (format "%dh %02dm" (/ total 3600) (% (/ total 60) 60))))))

(defun mevedel--truncate-display (text width &optional ellipsis)
  "Return TEXT truncated to WIDTH columns, ending with ELLIPSIS.

Returns TEXT untouched when it already fits, which is what makes this
worth having.  `truncate-string-to-width\' ends its scan by running
`aref\' off the end of the string and catching the `args-out-of-range\'
itself, so a string shorter than WIDTH -- the ordinary case for a tool
label -- always raises one internally.  Nothing escapes, but TRAMP\'s
`signal-hook-function\' logs every signal raised inside a handler frame,
so on a remote workspace each label rendered during a target operation
left a line like

  Args out of range: \"Args out of range\", \"git status 12\"

in *Messages*, where it reads like a defect and is not one."
  (when (stringp text)
    (if (<= (string-width text) width)
        text
      (truncate-string-to-width text width nil nil (or ellipsis "...")))))

(defun mevedel--ordered-completion-table (displays category)
  "Return a completion table over DISPLAYS that preserves their order.
CATEGORY is exposed as completion metadata for completion UI integrations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . ,category)
          (display-sort-function . identity)
          (cycle-sort-function . identity))
      (complete-with-action action displays string pred))))

;;
;;; Diagnostics

(defvar mevedel--warn-once-table (make-hash-table :test #'equal)
  "Keys already surfaced by `mevedel--warn-once'.")

(defun mevedel--warn-once (key format &rest args)
  "Emit a mevedel `display-warning' once per KEY; demote repeats.

KEY is compared with `equal'.  The first call per KEY raises a
`display-warning' under category `mevedel'; later calls log the
formatted text to *Messages* with `inhibit-message' bound so the
echo area stays untouched.  FORMAT and ARGS are as for `format'."
  (let ((text (apply #'format format args)))
    (if (gethash key mevedel--warn-once-table)
        (let ((inhibit-message t))
          (message "mevedel: %s" text))
      (puthash key t mevedel--warn-once-table)
      (display-warning 'mevedel text :warning))))

(defun mevedel--warn-once-reset-site (site)
  "Forget every plain or composite `mevedel--warn-once' key for SITE."
  (let (stale)
    (maphash (lambda (key _)
               (when (or (eq key site)
                         (eq (car-safe key) site))
                 (push key stale)))
             mevedel--warn-once-table)
    (dolist (key stale)
      (remhash key mevedel--warn-once-table))))


;;
;;; Plain data

(defun mevedel--diagnostic-value (value)
  "Return a disk-log-safe representation of VALUE without mutating it.
Keep ordinary Lisp data; stringify runtime objects.  Keyword lists retain
the last value for duplicate keys and supply nil for a missing final value."
  (cond
   ((or (null value)
        (keywordp value)
        (symbolp value)
        (stringp value)
        (numberp value))
    value)
   ((and (listp value) (keywordp (car-safe value)))
    (let (out)
      (while value
        (let ((key (pop value))
              (val (pop value)))
          (setq out
                (plist-put out key
                           (mevedel--diagnostic-value val)))))
      out))
   ((consp value)
    (cons (mevedel--diagnostic-value (car value))
          (mevedel--diagnostic-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'mevedel--diagnostic-value
                     (append value nil))))
   (t
   (format "%S" value))))

(defun mevedel--diagnostic-entry-text (entry)
  "Return sanitized diagnostic ENTRY in its durable line format."
  (let ((print-length nil)
        (print-level nil)
        (print-quoted t))
    (concat (prin1-to-string (mevedel--diagnostic-value entry)) "\n")))

(defun mevedel--plain-data-p (value)
  "Return non-nil when VALUE contains only read-safe data."
  (cond
   ((or (null value) (stringp value) (numberp value) (symbolp value)) t)
   ((functionp value) nil)
   ((consp value)
    (and (mevedel--plain-data-p (car value))
         (mevedel--plain-data-p (cdr value))))
   ((vectorp value)
   (cl-every #'mevedel--plain-data-p value))
   (t nil)))


;;
;;; Transcript buffers

(defcustom mevedel-transcript-disabled-minor-modes
  '(org-indent-mode
    flycheck-mode
    flymake-mode
    jinx-mode
    ws-butler-mode
    undo-tree-mode
    hl-line-mode)
  "Minor modes to disable in generated mevedel transcript buffers.

Mevedel data buffers and sub-agent transcript buffers are authoritative
storage for gptel, not the primary user editing surface.  Disabling
visual/checking/history modes there keeps generated model and tool-output
insertion from running expensive editor hooks.

A backstop rather than the mechanism: `mevedel--transcript-org-mode\' now
sets the mode without running the hooks these modes enter through, so a
buffer prepared that way should never have had one enabled.  It stays for a
mode that some other path switches on."
  :type '(repeat symbol)
  :group 'mevedel)

(defun mevedel--call-with-bare-transcript-mode (function)
  "Call FUNCTION without transcript mode hooks or file-local variables.

These buffers are authoritative storage for gptel, not an editing surface.
They need Org\'s syntax, and none of what a person\'s Org setup layers on
top: every minor mode attached there runs on each model and tool insertion,
and on a remote workspace several of them reach the target.

The known Org derived-mode hook chain and the global after-mode hooks are
dynamically empty, so the same boundary also applies when FUNCTION creates and
visits a new buffer.  Local Variables are disabled at that visit boundary.

Persistent Org element-cache loading is disabled for generated storage.
`org-inhibit-startup\' covers what remains, because Org\'s startup block
runs in the mode body: inline images, LaTeX previews, `org-num-mode\', and
`org-indent-mode\'."
  ;; Finish deferred user configuration before binding its hooks.  Otherwise
  ;; a cold Org load can install hooks inside the suppression boundary, run
  ;; them on storage, and lose their global registration when it unwinds.
  (require 'org)
  (let ((after-change-major-mode-hook nil)
        (change-major-mode-after-body-hook nil)
        (enable-local-variables nil)
        (find-file-hook nil)
        (font-lock-mode-hook nil)
        (org-element-cache-persistent nil)
        (org-inhibit-startup t)
        (org-mode-hook nil)
        (outline-mode-hook nil)
        (text-mode-hook nil))
    (funcall function)))

(defun mevedel--transcript-org-mode ()
  "Enable bare Org mode in a generated transcript buffer.

The transcript is mevedel's canonical record, so its shape does not
follow the user's gptel chat-buffer settings: no prompt or response
prefixes, and every tool call and result stays in the buffer."
  (setq-local change-major-mode-hook nil)
  (mevedel--call-with-bare-transcript-mode #'org-mode)
  (setq-local org-element-cache-persistent nil
              gptel-prompt-prefix-alist nil
              gptel-response-prefix-alist nil
              gptel-include-tool-results t))

(defun mevedel--optimize-transcript-buffer ()
  "Apply buffer-local performance settings for generated transcript buffers."
  (dolist (mode mevedel-transcript-disabled-minor-modes)
    (when (and (symbolp mode)
               (fboundp mode)
               (boundp mode)
               (symbol-value mode))
      (ignore-errors
        (funcall mode -1))))
  (when (boundp 'undo-tree-auto-save-history)
    (setq-local undo-tree-auto-save-history nil))
  ;; Generated storage nobody edits: each streamed chunk and tool result
  ;; would otherwise stay on an undo list until the buffer dies.
  (buffer-disable-undo)
  (mevedel--forget-place))

(defun mevedel--forget-place ()
  "Keep the current buffer out of `save-place-alist'.

Persisted mevedel buffers visit internal state files, not documents the
user opened, so recording point in them is noise.  On a remote execution
target it is worse than noise: the resulting `/ssh:' entry makes a later
Emacs touch the target host while merely initializing `saveplace'.
Clearing the buffer-local mode also drops any entry a previous session
already recorded, because `save-place-to-alist' deletes on kill."
  (setq-local save-place-mode nil))


;;
;;; General helpers

(defun mevedel--file-truename (file)
  "Return truename for FILE, or nil when it cannot be resolved."
  (ignore-errors (file-truename file)))

(defun mevedel--file-long-name (file)
  "Return Windows long name for FILE, or nil when unavailable."
  (and (fboundp 'w32-long-file-name)
       (or (ignore-errors (funcall 'w32-long-file-name file))
           (let ((directory-name (directory-file-name file)))
             (and (not (string= directory-name file))
                  (ignore-errors
                    (funcall 'w32-long-file-name directory-name)))))))

(defun mevedel--file-macos-var-alias (file)
  "Return FILE with macOS /private/var and /var aliases swapped."
  (cond
   ((not (eq system-type 'darwin)) nil)
   ((string-prefix-p "/System/Volumes/Data/private/var/" file)
    (concat "/var/" (substring file
                               (length "/System/Volumes/Data/private/var/"))))
   ((string-prefix-p "/System/Volumes/Data/var/" file)
    (concat "/var/" (substring file
                               (length "/System/Volumes/Data/var/"))))
   ((string-prefix-p "/private/var/" file)
    (concat "/var/" (substring file (length "/private/var/"))))
   ((string-prefix-p "/var/" file)
    (concat "/private/var/" (substring file (length "/var/"))))))

(defun mevedel--file-name-prefix-p (file directory)
  "Return non-nil when FILE is textually under DIRECTORY."
  (let* ((file (directory-file-name file))
         (directory (file-name-as-directory
                     (directory-file-name directory)))
         (ignore-case
          (or (memq system-type '(windows-nt ms-dos cygwin))
              (ignore-errors
                (file-name-case-insensitive-p directory)))))
    (string-prefix-p directory file ignore-case)))

(defun mevedel--file-name-candidates (file)
  "Return alias-tolerant absolute path candidates for FILE.

A target path gets its expanded name and nothing else.  Every alias this
resolves is a local-filesystem concept -- macOS `/private/var\', Windows
8.3 short names -- and none of them can apply to a name on another host,
so the `file-truename\' that would discover them is a round trip that
cannot change the answer.

It is also a round trip on the worst possible path.  The directory walks
below call `mevedel--same-file-p\' once per ancestor, and those run during
tool dispatch, inside gptel\'s curl sentinel: TRAMP\'s wait loop re-runs
that sentinel, and the nested call issues a command on a connection that
already has one in flight."
  (if-let* ((expanded (expand-file-name file))
            ((file-remote-p expanded)))
      (list (directory-file-name expanded))
    (mevedel--local-file-name-candidates file)))

(defun mevedel--local-file-name-candidates (file)
  "Return alias-tolerant absolute path candidates for local FILE."
  (let* ((expanded (expand-file-name file))
         (true (mevedel--file-truename expanded))
         (long (mevedel--file-long-name expanded))
         (true-long (and true (mevedel--file-long-name true)))
         candidates)
    (dolist (candidate (list expanded true long true-long))
      (when candidate
        (push candidate candidates)
        (when-let* ((alias (mevedel--file-macos-var-alias candidate)))
          (push alias candidates))))
    (delete-dups
     (mapcar #'directory-file-name
             (nreverse candidates)))))

(defun mevedel--same-file-p (file-a file-b)
  "Return non-nil when FILE-A and FILE-B name the same file.

The comparison accepts expanded names, truenames, and matching basenames
whose parent directories are equal.  The parent fallback covers generated
files before their first save, where the file itself may not yet exist but
its containing directory does."
  (let ((candidates-a (mevedel--file-name-candidates file-a))
        (candidates-b (mevedel--file-name-candidates file-b)))
    (or (cl-some (lambda (a)
                   (or (member a candidates-b)
                       ;; `file-equal-p' resolves both names on the target.
                       ;; For a remote pair the candidate lists are already
                       ;; the expanded names, so it can only repeat the
                       ;; comparison just made -- at a round trip apiece.
                       (and (not (file-remote-p a))
                            (not (file-remote-p (car candidates-b)))
                            (cl-some (lambda (b)
                                       (ignore-errors (file-equal-p a b)))
                                     candidates-b))))
                 candidates-a)
        (let* ((da (car candidates-a))
               (db (car candidates-b))
               (parent-a (file-name-directory da))
               (parent-b (file-name-directory db))
               (name-a (file-name-nondirectory da))
               (name-b (file-name-nondirectory db)))
          (and (not (string= da parent-a))
               (not (string= db parent-b))
               (string= name-a name-b)
               (mevedel--same-file-p parent-a parent-b))))))

(defun mevedel--file-in-directory-p (file directory)
  "Return non-nil when FILE is under DIRECTORY, tolerating path aliases."
  (let* ((file (expand-file-name file))
         (directory (file-name-as-directory (expand-file-name directory)))
         (file-candidates (mevedel--file-name-candidates file))
         (directory-candidates (mevedel--file-name-candidates directory))
         (cursor file)
         (found nil))
    (or (file-in-directory-p file directory)
        (cl-some
         (lambda (file-candidate)
           (cl-some
            (lambda (directory-candidate)
              (or (mevedel--file-name-prefix-p
                   file-candidate directory-candidate)
                  (file-in-directory-p
                   file-candidate
                   (file-name-as-directory directory-candidate))))
            directory-candidates))
         file-candidates)
        (progn
          (while (and (not found)
                      cursor
                      (not (string= cursor
                                    (file-name-directory
                                     (directory-file-name cursor)))))
            (when (mevedel--same-file-p cursor directory)
              (setq found t))
            (setq cursor (file-name-directory
                          (directory-file-name cursor))))
          found))))

(defun mevedel--file-relative-name-or-absolute (file directory)
  "Return FILE relative to DIRECTORY, or absolute FILE when outside.

Alias spellings such as /var vs /private/var and Windows 8.3 names are
accepted when Emacs can prove the directories are the same."
  (let* ((file (expand-file-name file))
         (directory (file-name-as-directory (expand-file-name directory)))
         (file-candidates (mevedel--file-name-candidates file))
         (directory-candidates (mevedel--file-name-candidates directory))
         (cursor file)
         parts
         found)
    (cond
     ((mevedel--file-name-prefix-p file directory)
      (file-relative-name file directory))
     ((catch 'relative
        (dolist (file-candidate file-candidates)
          (dolist (directory-candidate directory-candidates)
            (when (mevedel--file-name-prefix-p
                   file-candidate directory-candidate)
              (throw 'relative
                     (file-relative-name
                      file-candidate
                      (file-name-as-directory directory-candidate))))))))
     ((progn
        (while (and (not found)
                    cursor
                    (not (string= cursor
                                  (file-name-directory
                                   (directory-file-name cursor)))))
          (if (mevedel--same-file-p cursor directory)
              (setq found t)
            (push (file-name-nondirectory (directory-file-name cursor))
                  parts)
            (setq cursor (file-name-directory
                          (directory-file-name cursor)))))
        found)
      (string-join parts "/"))
     (t file))))

(defun mevedel--timer-pending-p (timer)
  "Return non-nil when TIMER is armed and still due to fire.

A timer object proves nothing by itself: `run-at-time' pushes onto the
current binding of `timer-list', and TRAMP let-binds that list to nil
around every remote command, so a timer created from a process filter or
hook inside that window is discarded with the binding and never fires.
Only presence on `timer-list', on a list an enclosing
`mevedel-transport-with-exclusive-connection' suspended, or being held by
`mevedel-transport-run-at-time' until TRAMP returns, means the timer is
actually scheduled.  The triggered flag is no evidence either way: Emacs
sets it on a repeating timer while that timer's own function runs."
  (and (timerp timer)
       (or (memq timer timer-list)
           (memq timer (bound-and-true-p mevedel-transport--suspended-timers))
           (memq timer (bound-and-true-p mevedel-transport--held-timers)))
       t))

(defvar mevedel--coalesced-timers nil
  "Timers whose due callbacks share a host wakeup.")
(defvar mevedel--coalesced-timer nil
  "The single host timer delivering coalesced callbacks.")
(defvar mevedel--coalesced-timer-dispatching nil
  "Non-nil while coalesced callbacks are being delivered.")

(defun mevedel--ui-timer-pending-p (timer)
  "Return whether UI-host TIMER is queued, even during TRAMP suspension."
  (and (timerp timer)
       (or (memq timer mevedel--coalesced-timers)
           (mevedel--timer-pending-p timer)
           (memq timer (default-toplevel-value 'timer-list)))))

(defun mevedel--ui-timer-cancel (timer)
  "Remove UI-host TIMER from coalesced, current and top-level queues.
TRAMP's temporary `timer-list' binding hides the outer list from
`cancel-timer'.  Only remove TIMER by identity, leaving other timers alone."
  (when (timerp timer)
    (cancel-timer timer)
    (let ((outer (default-toplevel-value 'timer-list)))
      (when (memq timer outer)
        (set-default-toplevel-value 'timer-list (delq timer outer))))
    (when (memq timer mevedel--coalesced-timers)
      (setq mevedel--coalesced-timers (delq timer mevedel--coalesced-timers))
      (mevedel--coalesced-timer-arm))))

(defun mevedel--ui-timer-activate (timer &optional coalesced)
  "Activate UI-host TIMER despite TRAMP suspension.
TIMER's time and function must be set.  With COALESCED, due callbacks share
one host wakeup; repeating timers use integral clock multiples and skip
missed ticks.  Otherwise use Emacs's ordinary timer delivery."
  (if coalesced
      (progn
        ;; One timer without a time would break every later arm.
        (unless (timer--high-seconds timer)
          (error "Coalesced timer has no time: %S" (timer--function timer)))
        (cl-pushnew timer mevedel--coalesced-timers :test #'eq)
        (mevedel--coalesced-timer-arm))
    (let ((timer-list (default-toplevel-value 'timer-list)))
      (timer-activate timer)
      (set-default-toplevel-value 'timer-list timer-list))))

(defun mevedel--run-periodic-timer (period function &rest arguments)
  "Run FUNCTION with ARGUMENTS every PERIOD seconds, sharing UI wakeups.
The first call is at the next integral multiple of PERIOD.  Missed calls
are skipped after a stall.  Use `mevedel--ui-timer-cancel' to stop the timer.
This is for periodic observation, not deadlines or animation frames."
  (unless (and (numberp period) (> period 0))
    (error "Timer period must be positive"))
  (let ((timer (timer-create)))
    (timer-set-time timer (timer-next-integral-multiple-of-time nil period) period)
    (timer-set-function timer function arguments)
    (mevedel--ui-timer-activate timer t)
    timer))

(defun mevedel--coalesced-timer-arm ()
  "Arm one host timer for the earliest pending coalesced callback."
  (unless mevedel--coalesced-timer-dispatching
    (let ((due (cl-loop with earliest = nil
                        for timer in mevedel--coalesced-timers
                        for time = (timer--time timer)
                        when (or (null earliest) (time-less-p time earliest))
                        do (setq earliest time)
                        finally return earliest)))
      (unless (and due (timerp mevedel--coalesced-timer)
                   (equal due (timer--time mevedel--coalesced-timer))
                   (mevedel--ui-timer-pending-p mevedel--coalesced-timer))
        (mevedel--ui-timer-cancel mevedel--coalesced-timer)
        (setq mevedel--coalesced-timer nil)
        (when due
          (let ((timer (timer-create)))
            (timer-set-time timer due)
            (timer-set-function timer #'mevedel--coalesced-timer-tick)
            (setq mevedel--coalesced-timer timer)
            (mevedel--ui-timer-activate timer)))))))

(defun mevedel--coalesced-timer-call (timer)
  "Invoke TIMER while preserving the caller's current buffer.
This boundary also allows telemetry to time each callback individually."
  ;; Rearm before delivery so self-cancellation is authoritative.
  ;; A callback can also cancel or postpone another due member.
  (if-let* ((period (timer--repeat-delay timer)))
      (timer-set-time
       timer (timer-next-integral-multiple-of-time nil period) period)
    (setq mevedel--coalesced-timers
          (delq timer mevedel--coalesced-timers)))
  (save-current-buffer
    (apply (timer--function timer) (timer--args timer))))

(defun mevedel--coalesced-timer-tick ()
  "Deliver due callbacks together, yielding for input or after 25 ms."
  (unless mevedel--coalesced-timer-dispatching
    (mevedel--ui-timer-cancel mevedel--coalesced-timer)
    (setq mevedel--coalesced-timer nil)
    (unwind-protect
        (let* ((mevedel--coalesced-timer-dispatching t)
               (now (current-time))
               (until (time-add now 0.025))
               delivered)
          (catch 'yield
            (dolist (timer (copy-sequence mevedel--coalesced-timers))
              (when (and (memq timer mevedel--coalesced-timers)
                         (not (time-less-p now (timer--time timer))))
                ;; Bound a batch between callbacks.  An individual callback
                ;; still owns its own responsiveness; unlike ordinary timers,
                ;; its siblings cannot run while it waits for input or output.
                (when (and delivered
                           (or (input-pending-p) (time-less-p until nil)))
                  (throw 'yield nil))
                (setq delivered t)
                (condition-case-unless-debug err
                    (mevedel--coalesced-timer-call timer)
                  (error (message "Error running periodic callback: %s"
                                  (error-message-string err))))))))
      (mevedel--coalesced-timer-arm))))

(defun mevedel--cycle-list-around (element list)
  "Cycle list LIST around ELEMENT.

If ELEMENT is found in LIST, returns a list with ELEMENT as the head and
the rest of the list rotated around it.  Otherwise, returns the LIST."
  (if-let* ((element-tail (member element list)))
      (append element-tail
              (cl-loop for elt in list
                       while (not (eq elt element))
                       collect elt))
    list))

(defun mevedel--clamped-integer (value default minimum maximum)
  "Coerce VALUE to an integer clamped between MINIMUM and MAXIMUM.
Floats round and numeric strings parse; anything else, including nil,
falls back to DEFAULT.  Models frequently send tuning parameters like
timeouts as floats, strings, or out-of-range numbers; such values are
never worth failing a tool call over."
  (let ((number
         (cond ((integerp value) value)
               ((numberp value) (round value))
               ((and (stringp value)
                     (string-match-p
                      "\\`[[:space:]]*-?[0-9]+\\(\\.[0-9]*\\)?[[:space:]]*\\'"
                      value))
                (round (string-to-number value)))
               (t default))))
    (min maximum (max minimum number))))

(defun mevedel--raw-byte-char-p (char)
  "Return non-nil when CHAR is an Emacs raw byte character."
  (eq (char-charset char) 'eight-bit))

(defun mevedel--invalid-message-char-p (char)
  "Return non-nil when CHAR is not a Unicode scalar value."
  (or (mevedel--raw-byte-char-p char)
      (<= #xd800 char #xdfff)
      (> char #x10ffff)))

(defun mevedel--escape-invalid-message-chars (text)
  "Return TEXT with non-Unicode characters rendered as hex byte escapes."
  (let ((start 0)
        (index 0)
        parts)
    (while (< index (length text))
      (if (mevedel--invalid-message-char-p (aref text index))
          (progn
            (when (< start index)
              (push (substring text start index) parts))
            (let ((char (aref text index)))
              (push
               (if (mevedel--raw-byte-char-p char)
                   (format "\\x%02X" (logand char #xff))
                 (mapconcat
                  (lambda (byte) (format "\\x%02X" byte))
                  (encode-coding-string (string char) 'utf-8-unix t) ""))
               parts))
            (setq index (1+ index)
                  start index))
        (setq index (1+ index))))
    (when (< start index)
      (push (substring text start index) parts))
    (apply #'concat (nreverse parts))))

(defun mevedel--head-tail-preview-parts
    (head tail total-length &optional preview-size)
  "Return a newline-aware preview plist from bounded HEAD and TAIL.

TOTAL-LENGTH is the complete character count.  HEAD and TAIL must contain at
least PREVIEW-SIZE prefix and suffix characters respectively when the complete
text is oversized.  The result contains `:text', `:head', `:tail', and the
exact `:omitted-chars'."
  (let ((preview-size (or preview-size 2000)))
    (if (<= total-length preview-size)
        (let ((text (substring head 0 total-length)))
          (list :text text :head text :tail "" :omitted-chars 0))
      (let* ((head-budget (/ preview-size 2))
             (tail-budget (- preview-size head-budget))
             (head-newline
              (cl-position ?\n head :from-end t :end head-budget))
             (head-end
              (if (and head-newline
                       (>= head-newline (/ head-budget 2)))
                  (1+ head-newline)
                head-budget))
             (tail-window (substring tail (- (length tail) tail-budget)))
             (tail-newline
              (cl-position ?\n tail-window
                           :end (min tail-budget (/ tail-budget 2))))
             (tail-cut (if tail-newline (1+ tail-newline) 0))
             (tail-start (+ (- total-length tail-budget) tail-cut))
             (head-text (substring head 0 head-end))
             (tail-text (substring tail-window tail-cut))
             (omitted (- tail-start head-end)))
        (list
         :text
         (concat head-text
                 (unless (eq ?\n (aref head-text (1- (length head-text))))
                   "\n")
                 (format
                  "[mevedel: tool output truncated; omitted %d chars]\n"
                  omitted)
                 tail-text)
         :head head-text
         :tail tail-text
         :omitted-chars omitted)))))

(defun mevedel--trim-tool-result (text)
  "Trim TEXT's blank edges, preserving first-line indentation.
Only newlines are trimmed on the left: leading spaces are significant
alignment, e.g. the right-aligned line numbers Read prepends."
  (string-trim (or text "") "[\n\r]+"))

(defun mevedel--normalize-message-text (text)
  "Return TEXT with raw UTF-8 byte sequences decoded for display/storage.

This repairs strings where valid UTF-8 bytes reached Emacs as raw
`eight-bit' characters.  Invalid bytes, surrogate code points, and characters
beyond Unicode's maximum are kept visible as `\\xNN' byte escapes.  Normal
ASCII and Unicode text, including text properties on unaffected ranges, is
preserved."
  (if (or (not (stringp text))
          ;; Equal byte and character counts imply ASCII in a multibyte
          ;; string.  Unibyte characters also cannot be invalid code points.
          (= (length text) (string-bytes text))
          ;; Raw-byte characters lie above Unicode's maximum.  Search in C
          ;; instead of allocating Lisp iteration state for every character.
          (not (string-match-p "[^\0-\uD7FF\uE000-\U0010FFFF]" text)))
      text
    (mevedel--escape-invalid-message-chars
     (let ((start 0)
           (index 0)
           parts)
       (while (< index (length text))
         (if (mevedel--raw-byte-char-p (aref text index))
             (let ((raw-start index))
               (when (< start index)
                 (push (substring text start index) parts))
               (while (and (< index (length text))
                           (mevedel--raw-byte-char-p (aref text index)))
                 (setq index (1+ index)))
               (push
                (decode-coding-string
                 (encode-coding-string
                  (substring text raw-start index) 'raw-text)
                 'utf-8-unix t)
                parts)
               (setq start index))
           (setq index (1+ index))))
       (when (< start index)
         (push (substring text start index) parts))
       (apply #'concat (nreverse parts))))))

(defun mevedel--color-name-to-rgb (color-name)
  "Return RGB components for COLOR-NAME.
Batch Emacs reports the default face as unspecified, but mevedel only
needs the usual light-background defaults there."
  (or (color-name-to-rgb color-name)
      ;; Batch has no frame colors; use Emacs' default light frame.
      (pcase color-name
        ("unspecified-fg" (color-name-to-rgb "black"))
        ("unspecified-bg" (color-name-to-rgb "white"))
        (_ (error "Unknown color: %s" color-name)))))

(defun mevedel--tint (source-color-name tint-color-name &optional intensity)
  "Return hex string color of SOURCE-COLOR-NAME tinted with TINT-COLOR-NAME.

INTENSITY controls the tinting intensity, where 0 means no tinting and 1
means that the resulting color is the same as the TINT-COLOR-NAME color."
  (let* ((tint (mevedel--color-name-to-rgb tint-color-name))
         (color (mevedel--color-name-to-rgb source-color-name))
         (result (cl-mapcar (lambda (color tint)
                              (+ (* (- 1.0 intensity) color)
                                 (* intensity tint)))
                            color
                            tint)))
    (apply #'color-rgb-to-hex `(,@result 2))))

(defun mevedel--environment-info-string
    (&optional workspace working-directory execution-target)
  "Return formatted environment information for WORKSPACE.

WORKSPACE defaults to current `mevedel-workspace'.  WORKING-DIRECTORY
overrides the workspace root.  EXECUTION-TARGET supplies cached target
readiness facts; remote directories are never probed here.
The string includes:
- Execution target (local or TRAMP method and destination)
- Working directory
- Platform (operating system type)
- OS version
- Emacs version
- Current date"
  (let* ((dir (file-name-as-directory
               (or working-directory
                   (mevedel-workspace-root
                    (or workspace (mevedel-workspace))))))
         (default-directory dir)
         (remote (if execution-target
                     (mevedel-execution-target-remote-p execution-target)
                   (file-remote-p dir)))
         (readiness
          (and execution-target
               (mevedel-execution-target-readiness execution-target)))
         (process-line
          (lambda (program &rest args)
            (when (and (not remote) (executable-find program))
              (with-temp-buffer
                (when (zerop (apply #'process-file
                                    program nil t nil args))
                  (string-trim (buffer-string)))))))
         (os-name
          (or (plist-get readiness :operating-system)
              (ignore-errors (funcall process-line "uname" "-s"))))
         (os-version
          (or (plist-get readiness :operating-system-version)
              (ignore-errors (funcall process-line "uname" "-r"))
              (and (not remote) system-configuration)
              "unknown"))
         (platform
          (if os-name
              (downcase os-name)
            (if remote
                "unknown"
              (pcase system-type
                ('gnu/linux "linux")
                ('darwin "darwin")
                ('windows-nt "windows")
                ('cygwin "cygwin")
                ('berkeley-unix "bsd")
                (_ (symbol-name system-type))))))
         (display-directory
          (or (file-remote-p dir 'localname 'never)
              (expand-file-name dir)))
         (date (format-time-string "%Y-%m-%d")))
    (format "Execution target: %s\nWorking directory: %s\nPlatform: %s\nOS Version: %s\nEmacs version: %s\nToday's date: %s"
            (mevedel-execution-target-label execution-target dir)
            display-directory
            platform
            os-version
            emacs-version
            date)))

(defun mevedel--fill-label-string (string &optional prefix-string padding buffer)
  "Fill STRING into its label.

If PREFIX-STRING is not nil, whitespace padding is added at the start of
every newline in STRING so that it aligns visually under PREFIX-STRING.

If PADDING is non-nil, then pad the entire string from the left with it.

If BUFFER is provided, STRING will be wrapped to not overflow the fill
column of BUFFER.  Wrapping will attempt to respect word boundaries and
only hyphenate words as a last resort if a word is too long to fit on a
line by itself."
  (let* ((paragraph-padding (if prefix-string
                                (make-string (length prefix-string) ? )
                              ""))
         (padding-fill-column (if buffer
                                  (- (with-current-buffer buffer
                                       fill-column)
                                     (if (null padding) 0 (length padding))
                                     (length paragraph-padding))
                                nil)))
    (when (< padding-fill-column (length prefix-string))
      (setq padding-fill-column nil))
    (with-temp-buffer
      (when (and fill-column padding-fill-column)
        (let ((fill-column padding-fill-column))
          (insert string " ") ; The whitespace is so that large words at the EOB will be wrapped.
          (goto-char (point-min))
          (catch 'search-end
            (while t
              (beginning-of-line)
              (let ((beg (point)))
                (let (best-col-pos
                      (lineno (line-number-at-pos beg)))
                  (while (and (= (line-number-at-pos (point)) lineno)
                              (< (current-column) fill-column))
                    (setq best-col-pos (point))
                    (condition-case nil
                        (re-search-forward "\\s-+")
                      (error
                       (throw 'search-end nil))))
                  (goto-char best-col-pos)
                  (let ((eol-col (save-excursion (end-of-line) (current-column))))
                    (if (>= eol-col fill-column)
                        (progn
                          (when (bolp)
                            (forward-char (1- fill-column))
                            (insert "-"))
                          (save-excursion
                            (end-of-line)
                            (unless (>= (current-column) fill-column)
                              (delete-char 1)
                              (insert " ")))
                          (insert "\n"))
                      (forward-line)))))))))
      (goto-char (point-min))
      (insert prefix-string)
      (forward-line)
      (beginning-of-line)
      (while (not (eobp))
        (when padding
          (insert padding))
        (insert paragraph-padding)
        (beginning-of-line)
        (forward-line))
      (string-trim (buffer-string)))))

(defun mevedel--clear-user-turn-gptel-properties (start end)
  "Clear inherited properties from START to END.
Preserve atomic mention bindings and live structural producer provenance."
  (let* ((inhibit-read-only t)
         (text (buffer-substring start end))
         (bindings (mevedel-mention-bindings-ranges
                    text)))
    (set-text-properties start end nil)
    (dolist (range bindings)
      (mevedel-mention-bindings-set
       (+ start (plist-get range :start))
       (+ start (plist-get range :end))
       (plist-get range :binding)))
    (dolist (property '(mevedel-hook-audit mevedel-render-data))
      (let ((position 0))
        (while (< position (length text))
          (let ((next (next-single-property-change
                       position property text (length text))))
            (when (eq t (get-text-property position property text))
              (add-text-properties
               (+ start position) (+ start next) (list property t)))
            (setq position next))))))
  (mevedel-transcript-restore-ignored-properties start end))

(defun mevedel--insert-user-turn (input)
  "Insert user INPUT at point after the configured gptel separator.
Clear inherited transcript properties while retaining mention bindings and
structural provenance.  Return the start of INPUT, leaving point after its
trailing newline.  The caller owns request admission and response markers."
  (let ((start (point)))
    (insert gptel-response-separator)
    (unless (bolp) (insert "\n"))
    (let ((body-start (point)))
      (insert input "\n")
      (mevedel--clear-user-turn-gptel-properties start (point))
      body-start)))

(defconst mevedel--render-data-open "<!-- mevedel-render-data -->"
  "Opening delimiter for internal render-data side-channel blocks.")

(defconst mevedel--render-data-close "<!-- /mevedel-render-data -->"
  "Closing delimiter for internal render-data side-channel blocks.")

(defconst mevedel--hook-audit-open "<!-- mevedel-hook-audit -->"
  "Opening delimiter for internal hook audit side-channel blocks.")

(defconst mevedel--hook-audit-close "<!-- /mevedel-hook-audit -->"
  "Closing delimiter for internal hook audit side-channel blocks.")

(autoload 'mevedel--strip-hook-audit-blocks "mevedel-transcript-audit")
(autoload 'mevedel--hook-prompt-rewrite-audit-record
  "mevedel-transcript-audit")
(autoload 'mevedel--read-hook-audit-record "mevedel-transcript-audit")
(autoload 'mevedel--format-hook-audit-record "mevedel-transcript-audit")

(defun mevedel--insert-user-role-block-at-marker (block &optional marker)
  "Insert synthetic user-role BLOCK at MARKER or `point-max'.

The inserted text is transcript content, not assistant output, so any
inherited gptel response properties are cleared.  When MARKER is live
in the current buffer, it is advanced to the end of the inserted block
so later response insertion happens after the synthetic user turn."
  (when (and (stringp block)
             (not (string-empty-p block)))
    (let ((start nil))
      (save-excursion
        (if (and (markerp marker)
                 (marker-position marker)
                 (eq (marker-buffer marker) (current-buffer)))
            (goto-char marker)
          (goto-char (point-max)))
        (unless (bolp)
          (insert "\n"))
        (unless (or (bobp)
                    (save-excursion
                      (forward-line -1)
                      (looking-at-p "[ \t]*$")))
          (insert "\n"))
        (setq start (point))
        (insert block)
        (unless (bolp)
          (insert "\n"))
        (mevedel--clear-user-turn-gptel-properties start (point))
        (when (and (markerp marker)
                   (marker-position marker)
                   (eq (marker-buffer marker) (current-buffer)))
          (set-marker marker (point)))
        (cons start (point))))))

(defun mevedel--live-buffer-marker-p (marker buffer)
  "Return non-nil when MARKER points into BUFFER."
  (and (markerp marker)
       (marker-position marker)
       (eq (marker-buffer marker) buffer)))

(defun mevedel--active-response-marker (info buffer)
  "Return INFO's active response insertion marker for BUFFER."
  (let ((tracking (plist-get info :tracking-marker))
        (position (plist-get info :position)))
    (cond
     ((mevedel--live-buffer-marker-p tracking buffer) tracking)
     ((mevedel--live-buffer-marker-p position buffer) position))))

(defun mevedel--split-open-reasoning-before-user-input (info)
  "Close INFO's open reasoning block before injecting user input."
  (when (eq (plist-get info :reasoning-block) 'in)
    (unwind-protect
        (gptel--display-reasoning-stream t info)
      ;; gptel moves and locks the marker, unless a stream hook signals.
      (when-let* ((tracking (plist-get info :tracking-marker)))
        (set-marker-insertion-type tracking nil)))
    (when-let* ((marker (plist-get info :reasoning-marker)))
      (set-marker marker nil))
    (plist-put info :reasoning-marker nil)
    (plist-put info :reasoning-block nil)))

(defun mevedel--apply-face-to-match (regex string face)
  "Apply FACE as a text property to the REGEX match in STRING.

If FACE is nil, removes the face property from the REGEX match in
STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (if face
          (add-text-properties (match-beginning 0) (match-end 0) `(face ,face))
        (remove-text-properties (match-beginning 0) (match-end 0) '(face nil))))
    (buffer-string)))

(defun mevedel--restore-overlay (buffer overlay-start overlay-end properties)
  "Helper function to restore an instruction overlay in BUFFER.

Uses PROPERTIES, OVERLAY-START, and OVERLAY-END to recreate the overlay."
  (let ((new-ov (make-overlay overlay-start overlay-end buffer)))
    (cl-loop for (prop value) on properties by #'cddr
             do (overlay-put new-ov prop value))
    new-ov))

(defun mevedel--delimiting-markdown-backticks (string)
  "Return a string containing the appropriate code block backticks for STRING."
  (let ((backticks "```"))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks "`")))
    backticks))

(defun mevedel--overlay-region-info (overlay)
  "Return region span information of OVERLAY in its buffer.

Returns three values, first being the region line & column span string
in the buffer, and the second being the content of the span itself."
  (let ((beg (overlay-start overlay))
        (end (overlay-end overlay)))
    (cl-labels ((pos-bol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (bolp)))
                (pos-eol-p (pos)
                  (save-excursion
                    (goto-char pos)
                    (eolp)))
                (pos-lineno (pos)
                  (line-number-at-pos pos))
                (pos-colno (pos)
                  (save-excursion
                    (goto-char pos)
                    (current-column))))
      (with-current-buffer (overlay-buffer overlay)
        (without-restriction
          (unless (= beg end)
            (when (pos-eol-p beg)
              (cl-incf beg))
            (when (pos-bol-p end)
              (cl-decf end)))
          (if (= beg end (point-min))
              (cl-values "beginning of the buffer" "")
            (let ((beg-lineno (pos-lineno beg))
                  (end-lineno (pos-lineno end))
                  (beg-colno (pos-colno beg))
                  (end-colno (pos-colno end)))
              ;; Ensure positions are in correct order for display
              (when (> beg-lineno end-lineno)
                (cl-rotatef beg-lineno end-lineno))
              (cl-values (format "line%s %s"
                                 (if (/= beg-lineno end-lineno) "s" "")
                                 (if (/= beg-lineno end-lineno)
                                     (format "%d%s-%d%s"
                                             beg-lineno
                                             (if (pos-bol-p beg)
                                                 ""
                                               (format ":%d" beg-colno))
                                             end-lineno
                                             (if (pos-eol-p end)
                                                 ""
                                               (format ":%d" end-colno)))
                                   (format "%s%s"
                                           beg-lineno
                                           (if (and (pos-bol-p beg) (pos-eol-p end))
                                               ""
                                             (if (= beg-colno end-colno)
                                                 (format ", column %d" beg-colno)
                                               (format ", columns %d-%s"
                                                       beg-colno
                                                       (if (pos-eol-p end)
                                                           "eol"
                                                         (format "%d" end-colno))))))))
                         (buffer-substring-no-properties beg end)))))))))

(defun mevedel--multiline-string-p (str)
  "Check if STR contain multiple lines."
  (string-match-p "\n" str))

(defun mevedel--tag-query-prefix-from-infix (query)
  "Transform the tag QUERY to prefix notation for Lisp.

Signals an error when the query is malformed."
  (cl-labels
      ((operator-p (token)
         (memq token '(and or not)))
       (combine (operator reversed-operands)
         (let ((operands (nreverse reversed-operands)))
           (if (cdr operands)
               (cons operator operands)
             (car operands))))
       (parse-primary (tokens)
         (unless (consp tokens)
           (user-error "Operator not followed by an expression"))
         (let ((token (car tokens)))
           (when (operator-p token)
             (user-error "Unexpected operator: %s" token))
           (cons (if (listp token) (parse-list token) token)
                 (cdr tokens))))
       (parse-not (tokens)
         (if (eq (car-safe tokens) 'not)
             (let* ((parsed (parse-primary (cdr tokens)))
                    (operand (car parsed)))
               (unless operand
                 (user-error "'not' requires an expression"))
               (cons (list 'not operand) (cdr parsed)))
           (parse-primary tokens)))
       (parse-and-tail (tokens operands)
         (cond
          ((null tokens) (cons (combine 'and operands) nil))
          ((eq (car tokens) 'or)
           (cons (combine 'and operands) tokens))
          (t
           (let* ((explicit-p (eq (car tokens) 'and))
                  (parsed (parse-not (if explicit-p
                                        (cdr tokens)
                                      tokens)))
                  (operand (car parsed)))
             (unless operand
               (user-error "'and' requires an expression"))
             (parse-and-tail (cdr parsed) (cons operand operands))))))
       (parse-and (tokens)
         (let* ((parsed (parse-not tokens))
                (operand (car parsed)))
           (when (and (null operand) (cdr parsed))
             (user-error "Empty group cannot be combined"))
           (parse-and-tail (cdr parsed) (list operand))))
       (parse-or-tail (tokens operands)
         (if (null tokens)
             (cons (combine 'or operands) nil)
           (unless (eq (car tokens) 'or)
             (user-error "Unexpected tag query token: %s" (car tokens)))
           (let* ((parsed (parse-and (cdr tokens)))
                  (operand (car parsed)))
             (unless operand
               (user-error "'or' requires an expression"))
             (parse-or-tail (cdr parsed) (cons operand operands)))))
       (parse-or (tokens)
         (let* ((parsed (parse-and tokens))
                (operand (car parsed)))
           (parse-or-tail (cdr parsed) (list operand))))
       (parse-list (tokens)
         (if (null tokens)
             nil
           (unless (proper-list-p tokens)
             (user-error "Malformed tag query"))
           (car (parse-or tokens)))))
    (cond
     ((listp query) (parse-list query))
     ((operator-p query) (user-error "Unexpected operator: %s" query))
     (t query))))

(defun mevedel--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (replace-regexp-in-string "^" "> " input-string))



(defmacro mevedel--with-gc-batched (&rest body)
  "Run BODY with garbage collection deferred until after it returns.

Allocation-heavy asynchronous work -- transcript redraws, persistence
transactions -- otherwise runs at whatever `gc-cons-threshold' the
user's idle GC tuning left behind; a profiled unattended session sat at
the 800KB default and paid dozens of ~135ms collections inside single
redraws and save transactions.  Raising the threshold for the dynamic
extent of BODY trades those for one collection at the next allocation
after BODY.

A collection triggers at whichever of `gc-cons-threshold' and
`gc-cons-percentage' times the live heap is larger.  On a multi-hour
session heap the percentage term wins by a wide margin, so raising only
the threshold left a profiled stream-cleanup path collecting inside its
batched section.  Both are raised, and neither is ever lowered."
  (declare (indent 0) (debug t))
  `(let ((gc-cons-threshold (max gc-cons-threshold (* 64 1024 1024)))
         (gc-cons-percentage (max gc-cons-percentage 0.5)))
     ,@body))

(defcustom mevedel-gc-cons-threshold-while-busy (* 64 1024 1024)
  "Lowest `gc-cons-threshold', in bytes, while a mevedel request runs.

A request allocates continuously in timers and process callbacks, which
the command loop never sees.  Idle-oriented tuning such as gcmh lowers the
threshold after its idle collection and raises it again only before the
next command, so an unattended request collected about every ten megabytes:
a pause of 150 ms every few seconds on a large session.  While any root or
agent request runs, the threshold is kept at least this high; the previous
value returns when the last one ends.  Nil leaves the threshold alone."
  :type '(choice (const :tag "Leave the threshold alone" nil)
                 (natnum :tag "Bytes"))
  :group 'mevedel)

(defcustom mevedel-gc-cons-threshold-while-typing (* 256 1024 1024)
  "Lowest `gc-cons-threshold', in bytes, while typing during mevedel work.

While any request or transcript rebuild holds the busy threshold and input
arrived within the last second, collection waits for this much allocation
instead.  A collection on a long session's heap takes about 200 ms, and a
refresh or a busy request allocates enough to reach the busy threshold in
the middle of typing.  Once input pauses, the busy threshold returns and
the pending collection runs while nobody is typing.  Nil keeps the busy
threshold throughout."
  :type '(choice (const :tag "Keep the busy threshold" nil)
                 (natnum :tag "Bytes"))
  :group 'mevedel)

(defconst mevedel--gc-typing-pause 1.0
  "Seconds without input after which a pending collection may run.")

(defconst mevedel--gc-settlement-grace 30
  "Seconds an ended request keeps the busy threshold for its settlement tail.")

(defvar mevedel--gc-holds (make-hash-table :test #'eq)
  "Keys holding the busy collection threshold, to their liveness predicate.")

(defvar mevedel--gc-timer nil
  "Timer keeping the busy threshold while holds exist.")

(defvar mevedel--gc-restore nil
  "(PREVIOUS . APPLIED) once mevedel raised `gc-cons-threshold'.")

(defun mevedel--gc-maintain ()
  "Apply or release the busy collection threshold for the current holds.
Holds whose predicate fails are dropped, so an ended request that skipped
its release cannot keep the threshold raised.  Re-applying every second
also outlasts tuning that lowers the threshold after an idle collection."
  (maphash (lambda (key live-p)
             (unless (ignore-errors (funcall live-p))
               (remhash key mevedel--gc-holds)))
           mevedel--gc-holds)
  (let ((floor (mevedel--gc-floor)))
    (if (and floor (> (hash-table-count mevedel--gc-holds) 0))
        (progn
          (if (not mevedel--gc-restore)
              (setq mevedel--gc-restore (cons gc-cons-threshold nil))
            ;; Still our value: follow the floor down as well as up, so a
            ;; pause in typing lets the deferred collection run.
            (when (eql gc-cons-threshold (cdr mevedel--gc-restore))
              (setq gc-cons-threshold
                    (max floor (car mevedel--gc-restore)))))
          (when (< gc-cons-threshold floor)
            (setq gc-cons-threshold floor))
          (setcdr mevedel--gc-restore gc-cons-threshold)
          (add-hook 'pre-command-hook #'mevedel--gc-note-input)
          (unless (timerp mevedel--gc-timer)
            (setq mevedel--gc-timer (mevedel--run-periodic-timer 1 #'mevedel--gc-maintain))))
      (when (timerp mevedel--gc-timer)
        (mevedel--ui-timer-cancel mevedel--gc-timer))
      (setq mevedel--gc-timer nil)
      (remove-hook 'pre-command-hook #'mevedel--gc-note-input)
      (when mevedel--gc-restore
        ;; Someone else chose a value meanwhile; theirs stands.
        (when (eql gc-cons-threshold (cdr mevedel--gc-restore))
          (setq gc-cons-threshold (car mevedel--gc-restore)))
        (setq mevedel--gc-restore nil)))))

(defun mevedel--gc-typing-p ()
  "Return non-nil when input arrived within `mevedel--gc-typing-pause'."
  (let ((idle (current-idle-time)))
    (or (null idle) (< (float-time idle) mevedel--gc-typing-pause))))

(defun mevedel--gc-floor ()
  "Return the collection threshold floor for the current input activity."
  (when-let* ((busy mevedel-gc-cons-threshold-while-busy))
    (if (and mevedel-gc-cons-threshold-while-typing (mevedel--gc-typing-p))
        (max busy mevedel-gc-cons-threshold-while-typing)
      busy)))

(defun mevedel--gc-note-input ()
  "Raise the collection floor as soon as input resumes during held work."
  (when (and mevedel-gc-cons-threshold-while-typing
             (< gc-cons-threshold mevedel-gc-cons-threshold-while-typing))
    (mevedel--gc-maintain)))

(defun mevedel--gc-hold (key live-p)
  "Keep the busy collection threshold while KEY holds and LIVE-P is non-nil.
A batch Emacs keeps its own collection behaviour."
  (unless noninteractive
    (puthash key live-p mevedel--gc-holds)
    (mevedel--gc-maintain)))

(defun mevedel--gc-release (key)
  "Release KEY's hold on the busy collection threshold."
  (when (gethash key mevedel--gc-holds)
    (remhash key mevedel--gc-holds)
    (mevedel--gc-maintain)))

(defmacro mevedel--with-gc-busy (&rest body)
  "Run BODY holding the busy collection threshold.
For lifecycle transactions that copy a session outside any request: a
large Save As allocated 350 MB and collected 21 times."
  (declare (indent 0) (debug t))
  (let ((key (make-symbol "busy")))
    `(let ((,key (list 'busy)))
       (mevedel--gc-hold ,key #'always)
       (unwind-protect (progn ,@body)
         (mevedel--gc-release ,key)))))

(defun mevedel--write-remote-file-atomically (path content coding mode)
  "Replace remote PATH with CONTENT in one pinned target program, or return nil.

The target program writes a temporary file beside PATH, sets MODE on it and
renames it over PATH, which is the same same-directory replacement TRAMP's
file operations perform in about twenty round trips.  Nil means the program
refused -- a symlinked leaf or parent spelling, a missing directory, a busy
transport -- and the caller falls back to those file operations, which also
own creating missing parents.  CODING and MODE follow
`mevedel--write-file-atomically'."
  (condition-case nil
      (let* ((bytes (if (eq coding 'no-conversion)
                        content
                      (encode-coding-string content (or coding 'utf-8-unix))))
             (result
              (car (mevedel-session-control-fs-run-program
                    (list (list :op 'write-mode
                                :path (mevedel-session-control-fs-physical-path path)
                                :content (concat (format "%o\n" mode) bytes)
                                :coding 'no-conversion))))))
        (when (eq 'ok (plist-get result :status))
          ;; The replacement bypassed TRAMP, whose cached attributes for PATH
          ;; and its directory would otherwise describe the old file.
          (let ((vec (tramp-dissect-file-name path)))
            (tramp-flush-file-properties vec (tramp-file-name-localname vec)))
          t))
    (error nil)))

(defun mevedel--write-file-atomically (path content &optional coding mode)
  "Replace PATH with string CONTENT through a same-directory rename.

The temp file is created in PATH's own directory so the final
`rename-file' stays on one filesystem -- and, for a remote PATH, on one
host -- which is what makes the replacement atomic: a reader sees the
old complete file or the new complete file, never a truncated one.

CODING is the coding system to write with; nil means `utf-8-unix', and
`no-conversion' writes CONTENT as literal bytes.  MODE non-nil sets the
result's file modes; nil applies `default-file-modes', matching what an
ordinary write would have produced (`make-temp-file' creates 0600,
which must not leak into the destination).  PATH's parent directory is
created when missing.  A remote PATH is replaced in one target program when
the target allows it."
  (unless (and (file-remote-p path)
               (mevedel--write-remote-file-atomically
                path content coding (or mode (default-file-modes))))
    (mevedel--write-file-atomically-1 path content coding mode)))

(defun mevedel--write-file-atomically-1 (path content coding mode)
  "Replace PATH with CONTENT through TRAMP or local file operations.
CODING and MODE follow `mevedel--write-file-atomically'."
  (let ((directory (file-name-directory (expand-file-name path))))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (file-name-concat directory ".mevedel-write-"))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (when (eq coding 'no-conversion)
                (set-buffer-multibyte nil))
              (insert content)
              (let ((coding-system-for-write (or coding 'utf-8-unix)))
                (write-region (point-min) (point-max) temporary
                              nil 'silent)))
            (set-file-modes temporary (or mode (default-file-modes)))
            (rename-file temporary path t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))


(provide 'mevedel-utilities)

;;; mevedel-utilities.el ends here.
