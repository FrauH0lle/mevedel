;;; mevedel-utilities.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'ediff-init))

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

(defun mevedel--pos-bol-p (pos buffer)
  "Return nil if POS is not a beginning of a line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (= pos (pos-bol)))))

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
    (mapc (lambda (prop)
            (overlay-put new-ov prop (plist-get properties prop)))
          properties)
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
  "Check if STR contains multiple lines."
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

(defun mevedel--markdown-code-blocks (text)
  "Extract Markdown code block contents from TEXT.

Returns a list with the blocks in the order they were found."
  (let ((blocks '())
        (pos 0)
        (regex "```\\(.*\\)?\n\\([[:ascii:][:nonascii:]]*?\\)\n```"))
    (while (string-match regex text pos)
      (let ((block (match-string 2 text)))
        (setq blocks (append blocks (list block)))
        (setq pos (match-end 0))))
    blocks))


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
  "Non-nil when a `ediff' patch editing session is in progress.")

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
    (kill-buffer buf)))

(defun mevedel--setup-ediff-session ()
  "Set up the ediff session by moving to the first difference.

This function is called during ediff startup to ensure the session
begins with the cursor positioned at the first detected difference
between the files being compared."
  ;; Move to the first difference to start the ediff session
  (ediff-next-difference))

(defun mevedel-ediff-patch ()
  "Start an ediff session to review and modify the current patch.

This function retrieves the patch buffer from the current workspace,
saves the current window configuration, and launches an ediff session
for interactive patch editing. It sets up necessary hooks to handle
patch creation, cleanup, and session management."
  (interactive)
  (let ((patch-buf (mevedel--patch-buffer)))
    ;; Ensure we have a patch buffer to work with
    (unless patch-buf
      (user-error "No patch buffer found"))

    ;; Save current window configuration for later restoration
    (setq mevedel--ediff-saved-wconf (current-window-configuration))

    (with-current-buffer patch-buf
      (goto-char (point-min))
      ;; From `ediff-patch-file'
      ;; Initialize patch processing based on ediff-patch-file logic
      (let (source-dir source-file)
        (require 'ediff-ptch)

        ;; Get the proper patch buffer for ediff processing
        (setq patch-buf
              (ediff-get-patch-buffer
               nil
               (and patch-buf (get-buffer patch-buf))))

        ;; Determine the source directory from the patch or fallback to
        ;; workspace root
        (setq source-dir (if-let* ((dir (file-name-directory
                                         (diff-filename-drop-dir (car (diff-hunk-file-names t))))))
                             (expand-file-name dir (mevedel--project-root))
                           (mevedel--project-root)))

        ;; Construct the source file path
        (setq source-file
              (file-name-concat source-dir (file-name-nondirectory (diff-find-file-name t t))))

        (ediff-with-current-buffer patch-buf
          ;; Set up cleanup hooks based on whether we have single or multiple
          ;; patches
          (if (< (length ediff-patch-map) 2)
              (add-hook 'ediff-quit-hook #'mevedel--cleanup-ediff-session 99)
            (add-hook 'ediff-quit-session-group-hook #'mevedel--cleanup-ediff-session 99)))

        ;; Set up startup hooks for patch storage and session setup
        (add-hook 'ediff-startup-hook #'mevedel--store-old-ediff-patch)
        (add-hook 'ediff-startup-hook #'mevedel--setup-ediff-session)

        ;; Set up quit hook to create updated patch from ediff changes
        (add-hook 'ediff-quit-hook #'mevedel--create-patch-from-ediff)

        ;; Mark ediff session as in progress and start the patching job
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
           (patch-buffer (mevedel--patch-buffer)))

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
  (let* (;; Get the workspace root directory for relative path calculations
         (base-dir (mevedel--project-root))
         ;; Get file paths for both ediff buffers
         (file-a (buffer-file-name ediff-buffer-A))
         (file-b (buffer-file-name ediff-buffer-B))
         ;; Remove backup extensions from file paths for clean diff display
         (file-a-no-backup-ext (string-remove-suffix ediff-backup-extension file-a))
         (file-b-no-backup-ext (string-remove-suffix ediff-backup-extension file-b))
         ;; Create buffer for storing custom diff output
         (ediff-custom-diff-buffer (get-buffer-create mevedel--ediff-custom-diff-buffer t))
         ;; Build diff options with proper labels and relative paths
         (ediff-custom-diff-options (concat "-c" " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 (with-current-buffer ediff-buffer-A
                                                   (buffer-substring-no-properties (point-min) (point-max))))
                                                " /dev/null"
                                              (concat " a/" (file-relative-name file-a-no-backup-ext base-dir)))

                                            " --label"
                                            ;; Use /dev/null for empty buffers,
                                            ;; otherwise use relative path
                                            (if (string-empty-p
                                                 (with-current-buffer ediff-buffer-B
                                                   (buffer-substring-no-properties (point-min) (point-max))))
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
