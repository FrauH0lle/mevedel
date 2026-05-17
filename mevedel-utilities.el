;;; mevedel-utilities.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared helpers that do not belong to any single mevedel module:
;; colour tinting for overlay styling, tag-query prefix/infix
;; conversion, ediff-based patch review glue, environment-info
;; string assembly for system prompts, and various text and path
;; manipulation utilities.

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'ediff-init))

;; `ediff-ptch'
(declare-function ediff-dispatch-file-patching-job "ediff-ptch" (patch-buf filename &optional startup-hooks))
(declare-function ediff-get-patch-buffer "ediff-ptch" (&optional arg patch-buf))
(defvar ediff-backup-extension)
(defvar ediff-patch-map)

;; `ediff-util'
(declare-function ediff-compute-custom-diffs-maybe "ediff-util")
(declare-function ediff-next-difference "ediff-util" (&optional arg))

;; `gptel'
(defvar gptel-default-mode)

;; `mevedel-preview-mode'
(defvar mevedel-preview-mode--current-overlay)

;; `mevedel-tool-fs'
(defvar mevedel--real-path)

;; `mevedel-workspace'
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))


(defun mevedel--cycle-list-around (element list)
  "Cycle list LIST around ELEMENT.

If ELEMENT is found in LIST, returns a list with ELEMENT as the head and
the rest of the list rotated around it. Otherwise, returns the LIST."
  (if-let* ((element-tail (member element list)))
      (append element-tail
              (cl-loop for elt in list
                       while (not (eq elt element))
                       collect elt))
    list))

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

(defun mevedel--normalize-message-text (text)
  "Return TEXT with raw UTF-8 byte runs decoded for message display/storage.

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

(defun mevedel--tint (source-color-name tint-color-name &optional intensity)
  "Return hex string color of SOURCE-COLOR-NAME tinted with TINT-COLOR-NAME.

INTENSITY controls the tinting intensity, where 0 means no tinting and 1
means that the resulting color is the same as the TINT-COLOR-NAME color."
  (let* ((tint (color-name-to-rgb tint-color-name))
         (color (color-name-to-rgb source-color-name))
         (result (cl-mapcar (lambda (color tint)
                              (+ (* (- 1.0 intensity) color)
                                 (* intensity tint)))
                            color
                            tint)))
    ;; HACK 2025-09-30: Otherwise tests fail as they are not interactive and I
    ;;   guess then there no colors
    (apply 'color-rgb-to-hex `(,@(if noninteractive
                                     (list 1.0 1.0 1.0)
                                   result)
                               2))))

(defun mevedel--environment-info-string (&optional workspace working-directory)
  "Return a formatted string containing environment information.

WORKSPACE defaults to current `mevedel-workspace'. The string includes:
- Working directory
- Git repository status
- Platform (operating system type)
- OS version
- Emacs version
- Current date"
  (let* ((dir (or working-directory
                  (mevedel-workspace--root
                   (or workspace (mevedel-workspace)))))
         (default-directory dir)
         (is-git-repo (and (executable-find "git")
                           (= 0 (call-process "git" nil nil nil
                                              "rev-parse" "--git-dir"))))
         (os-version
          (or (and (executable-find "uname")
                   (ignore-errors (car (process-lines "uname" "-r"))))
              system-configuration))
         (platform (pcase system-type
                     ('gnu/linux "linux")
                     ('darwin "darwin")
                     ('windows-nt "windows")
                     ('cygwin "cygwin")
                     ('berkeley-unix "bsd")
                     (_ (symbol-name system-type))))
         (date (format-time-string "%Y-%m-%d")))
    (format "Working directory: %s\nIs directory a git repo: %s\nPlatform: %s\nOS Version: %s\nEmacs version: %s\nToday's date: %s"
            (expand-file-name dir)
            (if is-git-repo "Yes" "No")
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
column of BUFFER. Wrapping will attempt to respect word boundaries and
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
        (remove-text-properties
         start (point)
         '(gptel nil response nil invisible nil front-sticky nil))
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
  (cl-labels ((operatorp (elm)
                (member elm '(and or not)))
              (unary-op-p (elm)
                (eq elm 'not))
              (binary-op-p (elm)
                (member elm '(and or)))
              (expressionp (elm)
                (or (atomp elm) (listp elm)))
              (atomp (elm)
                (and (not (listp elm)) (not (operatorp elm))))
              (expand-implicit-and-ops (expr)
                (let ((result '()))
                  (dolist (elm expr)
                    (let ((prev (car result)))
                      (cond
                       ((binary-op-p elm)
                        (cond
                         ((binary-op-p prev)
                          (user-error "Consecutive binary operators: %s, %s" prev elm))
                         ((not (expressionp prev))
                          (user-error "Binary operator follows operator: %s, %s" prev elm))))
                       ((unary-op-p elm)
                        (cond
                         ((unary-op-p prev)
                          (user-error "Consecutive unary operator: %s" prev)))))
                      (when (and (not (binary-op-p elm)) prev (not (operatorp prev)))
                        (push 'and result))
                      (push elm result)))
                  (cond
                   ((operatorp (car result))
                    (user-error "Operator not followed by any expression: %s" (car result)))
                   ((binary-op-p (car (last result)))
                    (user-error "Binary operator not following any expression: %s" (car (last result)))))
                  (nreverse result)))
              (aux (elm)
                (pcase elm
                  ((pred atomp) elm)
                  ((pred expressionp)
                   (let ((expanded-expr (expand-implicit-and-ops elm))
                         (toplevel-op nil)
                         (operator nil)
                         (multiplicative-exprs ())
                         (operatorless-arg nil)
                         (args ())
                         (negate-next-expr nil))
                     (dolist (elm expanded-expr)
                       (pcase elm
                         ((pred expressionp)
                          (if (null operator)
                              (if (not negate-next-expr)
                                  (setq operatorless-arg (aux elm))
                                (setq operatorless-arg `(not ,(aux elm)))
                                (setq negate-next-expr nil))
                            (cl-symbol-macrolet ((dst (if (eq operator 'and)
                                                          multiplicative-exprs
                                                        args)))
                              (when operatorless-arg
                                (push operatorless-arg dst)
                                (setq operatorless-arg nil))
                              (if (not negate-next-expr)
                                  (push (aux elm) dst)
                                (push `(not ,(aux elm)) dst)
                                (setq negate-next-expr nil)))))
                         ((pred operatorp)
                          (if (unary-op-p elm)
                              (setq negate-next-expr t)
                            (unless (eq toplevel-op 'or)
                              (setq toplevel-op elm))
                            (setq operator elm)
                            (unless (eq operator 'and)
                              (when multiplicative-exprs
                                (push `(and ,@(nreverse multiplicative-exprs)) args)
                                (setq multiplicative-exprs ())))))))
                     (if operatorless-arg
                         operatorless-arg
                       (if args
                           (progn
                             (when multiplicative-exprs
                               (push `(and ,@multiplicative-exprs) args))
                             `(,toplevel-op ,@(nreverse args)))
                         (when multiplicative-exprs
                           `(and ,@(nreverse multiplicative-exprs))))))))))
    (aux query)))

(defun mevedel--markdown-enquote (input-string)
  "Add Markdown blockquote to each line in INPUT-STRING."
  (let ((lines (split-string input-string "\n")))
    (mapconcat (lambda (line) (concat "> " line)) lines "\n")))



;;
;;; Directive overlay and diff sync

;; The complexity is high enough to live in its own file.
(require 'mevedel-diff-apply)


;;
;;; Ediff

(defvar mevedel--old-patch-buffer-name " *mevedel-original-patch*"
  "Name of the buffer storing the original patch.")
(defvar mevedel--new-patch-buffer-name " *mevedel-modified-patch*"
  "Name of the buffer storing the modified patch.")
(defvar mevedel--ediff-custom-diff-buffer " *mevedel-ediff-custom-diff*"
  "Name of the buffer storing the current patch.")

(defvar mevedel--original-patch-string nil
  "String containing the original patch content before `ediff' session.")
(defvar mevedel--ediff-in-progress-p nil
  "Non-nil when a `ediff' patch editing session is in progress.")
(defvar mevedel--ediff-saved-wconf nil
  "Save current window configuration for later restoration.")
(defvar mevedel--ediff-finished-hook nil
  "Hook run after ediff session completes and cleanup is done.")
(defvar mevedel--current-ediff-patch-buffer nil
  "The diff buffer driving the in-flight ediff session.
Set by `mevedel-ediff-patch' so that ediff callbacks target the
correct buffer instead of looking up the canonical preview name,
which is ambiguous when multiple previews coexist.")


(defun mevedel--cleanup-ediff-session ()
  "Clean up after an ediff patch editing session.

Resets state variables, restores window configuration, removes ediff
hooks, and kills temporary patch buffers."
  (setq mevedel--ediff-in-progress-p nil)
  (when (window-configuration-p mevedel--ediff-saved-wconf)
    (set-window-configuration mevedel--ediff-saved-wconf))
  (setq mevedel--ediff-saved-wconf nil)
  (remove-hook 'ediff-quit-session-group-hook #'mevedel--cleanup-ediff-session)
  (remove-hook 'ediff-quit-hook #'mevedel--cleanup-ediff-session)
  (remove-hook 'ediff-startup-hook #'mevedel--store-old-ediff-patch)
  (remove-hook 'ediff-startup-hook #'mevedel--setup-ediff-session)
  (remove-hook 'ediff-quit-hook #'mevedel--create-patch-from-ediff)
  (dolist (buf (list mevedel--old-patch-buffer-name
                     mevedel--new-patch-buffer-name
                     mevedel--ediff-custom-diff-buffer))
    (kill-buffer buf))
  (setq mevedel--current-ediff-patch-buffer nil)
  ;; Run hook for tools that want to be notified after ediff completes
  (run-hooks 'mevedel--ediff-finished-hook)
  (setq mevedel--ediff-finished-hook nil))


(defun mevedel--setup-ediff-session ()
  "Set up the ediff session by moving to the first difference.

This function is called during ediff startup to ensure the session
begins with the cursor positioned at the first detected difference
between the files being compared."
  ;; Move to the first difference to start the ediff session
  (ediff-next-difference))

(defun mevedel-ediff-patch ()
  "Start an ediff session to review and modify the current patch.

Operates on the current buffer, which must be a mevedel diff preview
buffer with the buffer-local `mevedel--real-path' set.  Saves the
current window configuration and launches an ediff patching job that
targets `mevedel--real-path' directly, bypassing `diff-find-file-name'
(which is fragile on freshly-generated unified diffs and would crash on
new-file stubs).  Binds `mevedel--current-ediff-patch-buffer' so the
quit-hook callbacks can identify the right diff buffer even when
multiple previews coexist and share the canonical buffer name."
  (interactive)
  (let ((patch-buf (current-buffer)))
    (unless (and (buffer-live-p patch-buf)
                 (buffer-local-boundp 'mevedel--real-path patch-buf))
      (user-error "Not in a mevedel diff preview buffer"))
    (setq mevedel--current-ediff-patch-buffer patch-buf)
    (setq mevedel--ediff-saved-wconf (current-window-configuration))
    (with-current-buffer patch-buf
      (goto-char (point-min))
      (require 'ediff-ptch)
      (let ((source-file (expand-file-name mevedel--real-path)))
        (setq patch-buf (ediff-get-patch-buffer nil patch-buf))
        (ediff-with-current-buffer patch-buf
          (if (< (length ediff-patch-map) 2)
              (add-hook 'ediff-quit-hook #'mevedel--cleanup-ediff-session 99)
            (add-hook 'ediff-quit-session-group-hook
                      #'mevedel--cleanup-ediff-session 99)))
        (add-hook 'ediff-startup-hook #'mevedel--store-old-ediff-patch)
        (add-hook 'ediff-startup-hook #'mevedel--setup-ediff-session)
        (add-hook 'ediff-quit-hook #'mevedel--create-patch-from-ediff)
        (setq mevedel--ediff-in-progress-p t)
        (ediff-dispatch-file-patching-job patch-buf source-file)))))

(defun mevedel--create-patch-from-ediff ()
  "Create and apply an updated patch from an ediff session.

This function is called as part of the ediff-quit-hook to generate a new
patch based on changes made during the ediff session and update the
original patch file with the new content."
  (when mevedel--ediff-in-progress-p
    (let* ((new-patch-buf (get-buffer-create mevedel--new-patch-buffer-name t))
           (file-a (buffer-file-name ediff-buffer-A))
           (file-b (buffer-file-name ediff-buffer-B))
           (patch-buffer mevedel--current-ediff-patch-buffer))

      ;; Generate the new patch content based on ediff changes
      (mevedel--create-ediff-custom-patch new-patch-buf)

      ;; Update the main patch buffer by replacing the original patch content
      ;; with the newly generated patch from ediff
      (when (and patch-buffer
                 (buffer-live-p patch-buffer)
                 mevedel--original-patch-string)
        (with-current-buffer patch-buffer
          (let ((inhibit-read-only t)
                (new-content (with-current-buffer new-patch-buf
                               (string-trim
                                (buffer-substring-no-properties (point-min) (point-max))))))
            (save-excursion
              (goto-char (point-min))
              ;; Locate and replace the original patch string with new content
              (when (search-forward mevedel--original-patch-string nil t)
                (replace-match new-content t t)
                (message "Patch updated in %s" (buffer-name patch-buffer)))))))

      ;; Update the temp file with the user's ediff modifications so that
      ;; return-to-inline-preview can regenerate a clean diff buffer.
      (when-let* ((ov mevedel-preview-mode--current-overlay)
                  (temp-file (overlay-get ov 'mevedel--temp-file)))
        (let ((user-content (with-current-buffer ediff-buffer-B
                              (buffer-substring-no-properties
                               (point-min) (point-max)))))
          (with-temp-file temp-file
            (insert user-content))))

      ;; Finalize the ediff session by removing read-only protection and
      ;; restoring the original file with the modified version
      (with-current-buffer ediff-buffer-A
        (read-only-mode -1)
        (rename-file file-a file-b t)
        (set-visited-file-name file-b t t))

      ;; Clean up buffer names: Ediff creates unique buffer names by suffixing
      ;; the original buffer (B) with <2>. We remove the duplicate buffer and
      ;; restore the original name
      (let ((orig-buffer-name (buffer-name ediff-buffer-B)))
        (kill-buffer ediff-buffer-B)
        (with-current-buffer ediff-buffer-A
          (rename-buffer orig-buffer-name))))))

(defun mevedel--store-old-ediff-patch ()
  "Store the original patch state before starting an ediff session.

This captures the current diff as a string to allow restoration later if
needed during the ediff process."
  (when mevedel--ediff-in-progress-p
    ;; Create or get the buffer for storing the old patch
    (let* ((old-patch-buf (get-buffer-create mevedel--old-patch-buffer-name t)))
      ;; Generate the custom patch content and store it as a trimmed string
      (setq mevedel--original-patch-string
            (with-current-buffer (mevedel--create-ediff-custom-patch old-patch-buf)
              (string-trim
               (buffer-substring-no-properties (point-min) (point-max))))))))

(defun mevedel--create-ediff-custom-patch (buffer)
  "Create a custom unified diff patch from an active ediff session.

The patch is generated in BUFFER and formatted to match git's diff
format with proper a/ and b/ path prefixes for the workspace root
directory."
  (let* (;; Get the base directory from the diff buffer (set by
         ;; setup-diff-buffer to the correct root, even for files
         ;; outside the workspace).
         (base-dir (if-let* ((patch-buf mevedel--current-ediff-patch-buffer)
                             ((buffer-live-p patch-buf)))
                       (buffer-local-value 'default-directory patch-buf)
                     default-directory))
         ;; Get file paths for both ediff buffers
         (file-a (buffer-file-name ediff-buffer-A))
         (file-b (buffer-file-name ediff-buffer-B))
         ;; Remove backup extensions from file paths for clean diff display
         (file-a-no-backup-ext (string-remove-suffix ediff-backup-extension file-a))
         (file-b-no-backup-ext (string-remove-suffix ediff-backup-extension file-b))
         ;; Create buffer for storing custom diff output
         (ediff-custom-diff-buffer (get-buffer-create mevedel--ediff-custom-diff-buffer t))
         (orig-content (with-current-buffer ediff-buffer-A
                         (buffer-substring-no-properties (point-min) (point-max))))
         (new-content (with-current-buffer ediff-buffer-B
                        (buffer-substring-no-properties (point-min) (point-max))))
         ;; Build diff options with proper labels and relative paths
         (ediff-custom-diff-options (concat "-c" " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 orig-content)
                                                " /dev/null"
                                              (concat " a/" (file-relative-name file-a-no-backup-ext base-dir)))

                                            " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 new-content)
                                                " /dev/null"
                                              (concat " b/" (file-relative-name file-b-no-backup-ext base-dir))))))

    ;; Ensure we're operating within an ediff control buffer context
    (ediff-barf-if-not-control-buffer)
    ;; Ensure custom diffs are computed and available
    (ediff-compute-custom-diffs-maybe)

    (with-current-buffer buffer
      ;; Clear the buffer to prepare for new patch content
      (erase-buffer)
      ;; Insert standard git diff header with relative file paths
      (insert (format "diff --git a/%s b/%s\n"
                      (file-relative-name file-a-no-backup-ext base-dir)
                      (file-relative-name file-b-no-backup-ext base-dir)))
      ;; Add file mode lines for new or deleted files.
      (cond
       ;; New file
       ((and (string-empty-p orig-content) (not (string-empty-p new-content)))
        (insert "new file mode 100644\n"))
       ;; Deleted file
       ((and (not (string-empty-p orig-content)) (string-empty-p new-content))
        (insert "deleted file mode 100644\n")))
      ;; Insert and convert diff content from context format to unified format
      (insert (with-current-buffer ediff-custom-diff-buffer
                (diff-context->unified (point-min) (point-max))
                (buffer-substring-no-properties (point-min) (point-max))))
      ;; Normalize file paths in the diff output to ensure git-compatible
      ;; format. This step ensures consistency even if the diff command
      ;; generates different paths
      (goto-char (point-min))
      ;; Replace the --- line to use git's a/ prefix format
      (when (re-search-forward (concat "^--- " (regexp-quote file-a)) nil t)
        (replace-match (concat "--- a/" (file-relative-name file-a-no-backup-ext base-dir))))
      ;; Replace the +++ line to use git's b/ prefix format
      (goto-char (point-min))
      (when (re-search-forward (concat "^\\+\\+\\+ " (regexp-quote file-b)) nil t)
        (replace-match (concat "+++ b/" (file-relative-name file-b-no-backup-ext base-dir)))))
    ;; Return the buffer containing the formatted patch
    buffer))

(provide 'mevedel-utilities)

;;; mevedel-utilities.el ends here.
