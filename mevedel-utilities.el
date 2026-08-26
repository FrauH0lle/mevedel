;;; mevedel-utilities.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the package-version command and shared helpers that do not belong to
;; any single mevedel module: colour tinting for overlay styling, tag-query
;; prefix/infix conversion, environment-info string assembly for system
;; prompts, and various text and path manipulation utilities.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; `color'
(declare-function color-name-to-rgb "color" (color &optional frame))
(declare-function color-rgb-to-hex "color" (red green blue &optional digits))

;; `gptel'
(defvar gptel-default-mode)

;; `mevedel-execution'
(declare-function mevedel-execution-run-helper
                  "mevedel-execution"
                  (name command read-paths writable-roots &rest keys))
(autoload 'mevedel-execution-run-helper "mevedel-execution")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-readiness
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-ranges
                  "mevedel-mention-bindings" (text))
(declare-function mevedel-mention-bindings-set
                  "mevedel-mention-bindings"
                  (start end binding &optional object))
(autoload 'mevedel-mention-bindings-ranges "mevedel-mention-bindings")

;; `mevedel-structs'
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-restore-ignored-properties
                  "mevedel-transcript" (start end))
(autoload 'mevedel-transcript-restore-ignored-properties "mevedel-transcript")

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())
(autoload 'mevedel-current-origin "mevedel-turn")

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `org'
(declare-function org-mode "org" ())
(autoload 'org-mode "org")

;; `org-indent'
(declare-function org-indent-mode "org-indent" (&optional arg))
(autoload 'org-indent-mode "org-indent")

;; `saveplace'
(defvar save-place-mode)

;; `subr'
(defvar read-eval)


;;
;;; Package metadata

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

(defun mevedel-run-helper-capturing-output
    (name command read-paths &optional writable-roots session)
  "Run helper COMMAND as NAME, returning (EXIT-CODE . OUTPUT).
READ-PATHS and WRITABLE-ROOTS declare the helper's filesystem boundary.
OUTPUT is returned unchanged so callers may interpret its whitespace."
  (let* ((result (mevedel-execution-run-helper
                  name command read-paths writable-roots
                  :session (or session (bound-and-true-p mevedel--session))
                  :owner (mevedel-current-origin)))
         (error-data (plist-get result :error)))
    (when error-data
      (signal (car error-data) (cdr error-data)))
    (cons (plist-get result :exit-code)
          (plist-get result :output))))

(defun mevedel-generate-diff
    (original modified filepath &optional labels-real)
  "Generate unified diff between ORIGINAL and MODIFIED for FILEPATH.
When LABELS-REAL is nil, empty content is labelled `/dev/null'.  Otherwise
both sides use real `a/FILEPATH' and `b/FILEPATH' labels."
  (with-temp-buffer
    (let ((orig-file (make-temp-file "mevedel-orig-"))
          (mod-file (make-temp-file "mevedel-mod-")))
      (unwind-protect
          (progn
            (with-temp-file orig-file (when original (insert original)))
            (with-temp-file mod-file (when modified (insert modified)))
            (let* ((mevedel--session nil)
                   (output
                    (cdr
                     (mevedel-run-helper-capturing-output
                      "mevedel-diff"
                      (list "diff" "-u"
                            "--label" (if (or labels-real
                                              (and original
                                                   (not (string-empty-p
                                                         original))))
                                          (concat "a/" filepath)
                                        "/dev/null")
                            "--label" (if (or labels-real
                                              (and modified
                                                   (not (string-empty-p
                                                         modified))))
                                          (concat "b/" filepath)
                                        "/dev/null")
                            orig-file mod-file)
                      (list orig-file mod-file)))))
              (cond ((string-empty-p output) "")
                    ((string-suffix-p "\n" output) output)
                    (t (concat output "\n")))))
        (when (file-exists-p orig-file) (delete-file orig-file))
        (when (file-exists-p mod-file) (delete-file mod-file))))))


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
insertion from running expensive editor hooks."
  :type '(repeat symbol)
  :group 'mevedel)

(defun mevedel--transcript-org-mode ()
  "Enable Org mode without activating its frame-wide indentation redraw."
  (if (fboundp 'org-indent-mode)
      (cl-letf (((symbol-function 'org-indent-mode) #'ignore))
        (org-mode))
    (org-mode)))

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
  "Return alias-tolerant absolute path candidates for FILE."
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
                       (cl-some (lambda (b)
                                  (ignore-errors (file-equal-p a b)))
                                candidates-b)))
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

(defun mevedel--escape-raw-byte-chars (text)
  "Return TEXT with raw byte characters rendered as printable hex escapes."
  (let ((start 0)
        (index 0)
        parts)
    (while (< index (length text))
      (if (mevedel--raw-byte-char-p (aref text index))
          (progn
            (when (< start index)
              (push (substring text start index) parts))
            (push (format "\\x%02X" (logand (aref text index) #xff))
                  parts)
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
`eight-bit' characters, which cannot be written as `utf-8-unix'.  Any
remaining invalid raw bytes are kept visible as `\\xNN' escapes.  Normal
ASCII and Unicode text, including text properties on unaffected ranges,
is preserved."
  (if (or (not (stringp text))
          (not (cl-some #'mevedel--raw-byte-char-p text)))
      text
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
               (mevedel--escape-raw-byte-chars
                (decode-coding-string
                 (encode-coding-string
                  (substring text raw-start index) 'raw-text)
                 'utf-8-unix t))
               parts)
              (setq start index))
          (setq index (1+ index))))
      (when (< start index)
        (push (substring text start index) parts))
      (apply #'concat (nreverse parts)))))

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
    (format "Working directory: %s\nPlatform: %s\nOS Version: %s\nEmacs version: %s\nToday's date: %s"
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
  (let ((backticks (if (eq gptel-default-mode 'markdown-mode)
                       "~~~"
                     "```")))
    (while (string-match-p backticks string)
      (setq backticks (concat backticks
                              (if (eq gptel-default-mode 'markdown-mode)
                                  "~"
                                "`"))))
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
after BODY."
  (declare (indent 0) (debug t))
  `(let ((gc-cons-threshold (max gc-cons-threshold (* 64 1024 1024))))
     ,@body))

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
created when missing."
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
