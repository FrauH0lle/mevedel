;;; mevedel-preview-mode.el -- Inline diff preview -*- lexical-binding: t -*-

;;; Commentary:

;; Inline and buffer-based diff preview system for mevedel tool results.
;; Shows proposed file changes for user approval with approve/reject/edit/feedback
;; workflow.  Small diffs are displayed as overlays inline in the chat buffer;
;; larger diffs open in a separate buffer.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel'
(defvar gptel-display-buffer-action)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-diff-apply'
(declare-function mevedel-diff-apply-buffer "mevedel-diff-apply" ())

;; `mevedel-tool-fs'
(declare-function mevedel-tools--generate-diff "mevedel-tool-fs" (original modified filepath))
(declare-function mevedel-tools--setup-diff-buffer "mevedel-tool-fs"
                  (temp-file real-path workspace root
                             &optional chat-buffer final-callback
                             user-modified original-window-config))

;; `mevedel-utilities'
(declare-function mevedel-ediff-patch "mevedel-utilities" ())
(defvar mevedel--ediff-finished-hook)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))
(defvar mevedel--workspace)

;; `org-src'
(declare-function org-escape-code-in-region "org-src" (beg end))


;;
;;; Customization

(defcustom mevedel-inline-preview-threshold 0.8
  "Ratio of chat buffer height to use for inline preview threshold.

Diffs with fewer lines than this ratio times the chat buffer height
will be displayed inline as overlays in the chat buffer. Larger diffs
will be displayed in a separate buffer.

Set to 0 to always use separate buffer, or 1.0 to always prefer inline
display (when possible)."
  :type 'number
  :group 'mevedel)


;;
;;; Variables

(defvar mevedel-tools--current-inline-preview-overlay nil
  "Stores the current inline preview overlay during ediff session.")

(defvar-local mevedel--apply-fn nil
  "Buffer-local apply function for the separate-buffer preview path.
When non-nil, called instead of `mevedel-diff-apply-buffer' on approve.")


;;
;;; Inline preview

(defun mevedel-tools--show-changes-and-confirm (temp-file original-content real-path final-callback
                                                          &optional tool-name apply-fn)
  "Show diff between ORIGINAL-CONTENT and TEMP-FILE, ask user to confirm.

TEMP-FILE - path to file with proposed changes
ORIGINAL-CONTENT - original file content
REAL-PATH - path to real file
FINAL-CALLBACK - callback to return final result to LLM
TOOL-NAME - optional tool name for display (e.g., \"Edit\", \"Write\", \"Insert\")
APPLY-FN - optional function to apply changes on approve.  Called with
  no arguments in the diff-buffer context.  When nil, defaults to
  `mevedel-diff-apply-buffer' (overlay-preserving patch).  Tools that
  create new files or do full replacements should pass a function that
  writes TEMP-FILE content to REAL-PATH directly."
  ;; Validate that we're running in the chat buffer context (tools should be called by gptel from chat buffer)
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel-tools--show-changes-and-confirm' must be called from chat buffer context"))
  (let* ((chat-buffer (current-buffer))
         ;; The file we are editing can be in the main workspace root or
         ;; in another allowed one.  Fall back to the file's directory if
         ;; outside known roots (the pipeline already checked permissions).
         (root (or (mevedel-workspace--file-in-allowed-roots-p real-path chat-buffer)
                   (file-name-directory (expand-file-name real-path))))
         (workspace (mevedel-workspace chat-buffer))
         (rel-path (file-relative-name real-path root))
         (diff-buffer (mevedel-tools--setup-diff-buffer
                       temp-file real-path workspace root
                       chat-buffer
                       final-callback
                       nil  ; user-modified
                       (current-window-configuration)))
         (diff (with-current-buffer diff-buffer (buffer-string))))

    ;; Decide whether to use inline or separate buffer display
    (if (mevedel-tools--should-use-inline-preview-p diff chat-buffer)
        ;; Use inline preview
        (mevedel-tools--show-inline-preview diff temp-file original-content
                                            real-path final-callback
                                            chat-buffer workspace root rel-path
                                            tool-name diff-buffer apply-fn)
      ;; Use separate buffer (existing behavior)
      (with-current-buffer diff-buffer
        ;; Set helpful header line
        (setq header-line-format
              (concat
               (propertize (format " Proposed changes to %s. -- " rel-path) 'face 'success)
               (propertize "Choose: (a)pprove, (r)eject, (e)dit, (f)eedback and reject" 'face 'help-key-binding)))
        (when apply-fn
          (setq-local mevedel--apply-fn apply-fn)))

      ;; REVIEW 2026-03-07: Maybe change to `display-buffer'
      ;; Show the diff buffer and prompt
      (pop-to-buffer diff-buffer)
      (mevedel-tools--prompt-for-changes diff-buffer))))

(defun mevedel-tools--should-use-inline-preview-p (diff-string chat-buffer)
  "Return t if DIFF-STRING should be displayed inline in CHAT-BUFFER.

Compares the number of lines in DIFF-STRING against
`mevedel-inline-preview-threshold' times the visible height of
CHAT-BUFFER's window."
  (and (> mevedel-inline-preview-threshold 0)
       (let* ((diff-lines (with-temp-buffer
                            (insert diff-string)
                            (count-lines (point-min) (point-max))))
              (chat-window (get-buffer-window chat-buffer))
              (chat-height (and chat-window (window-height chat-window))))
         (and chat-height
              (<= diff-lines (* chat-height mevedel-inline-preview-threshold))))))

(defun mevedel-tools--show-inline-preview (diff-string temp-file _original-content
                                                       real-path final-callback
                                                       chat-buffer workspace root rel-path
                                                       &optional tool-name diff-buffer apply-fn)
  "Show DIFF-STRING as an inline overlay in CHAT-BUFFER.

Arguments:
- DIFF-STRING: The unified diff to display
- TEMP-FILE: Path to temporary file with proposed changes
- _ORIGINAL-CONTENT: Original file content (unused, for signature compatibility)
- REAL-PATH: Actual file path
- FINAL-CALLBACK: Async callback to return result
- CHAT-BUFFER: The chat buffer context
- WORKSPACE: Current workspace
- ROOT: Workspace root directory
- REL-PATH: Relative path for display
- TOOL-NAME: Optional tool name for display (e.g., \"Edit\", \"Write\", \"Insert\")
- DIFF-BUFFER: Optional diff buffer to associate with the overlay
- APPLY-FN: Optional function to apply changes
  (see `mevedel-tools--show-changes-and-confirm')"
  (let ((ov (mevedel-tools--create-inline-preview-overlay
             diff-string temp-file real-path final-callback
             chat-buffer workspace root rel-path
             nil nil tool-name diff-buffer apply-fn)))
    ;; Position cursor at the overlay
    (with-current-buffer chat-buffer
      (goto-char (overlay-start ov)))))

(defun mevedel-tools--create-inline-preview-overlay (diff-string temp-file real-path
                                                                 final-callback chat-buffer
                                                                 workspace root rel-path
                                                                 &optional user-modified position tool-name diff-buffer apply-fn)
  "Create an inline preview overlay in CHAT-BUFFER at POSITION.

Arguments:
- DIFF-STRING: The unified diff to display
- TEMP-FILE: Path to temporary file with proposed changes
- REAL-PATH: Path to actual file
- FINAL-CALLBACK: Async callback to return result
- CHAT-BUFFER: The chat buffer context
- WORKSPACE: Current workspace
- ROOT: Workspace root directory
- REL-PATH: Relative path for display
- USER-MODIFIED: Optional flag indicating user edits (shows warning)
- POSITION: Optional position to insert at (defaults to point-max)
- TOOL-NAME: Optional tool name for display (e.g., \"Edit\", \"Write\",
  \"Insert\")
- DIFF-BUFFER: Optional diff buffer to associate with the overlay

Returns the created overlay."
  (with-current-buffer chat-buffer
    (goto-char (or position (point-max)))
    (let ((start (point)))
      ;; Insert header
      (insert "\n")
      (insert (concat
               (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t))
               (if tool-name
                   (concat
                    (propertize (format "%s: " tool-name) 'font-lock-face 'font-lock-escape-face)
                    "Proposed changes to "
                    (propertize (format "%s\n" rel-path) 'font-lock-face 'font-lock-constant-face))
                 (concat
                  "Proposed changes to "
                  (propertize (format "%s\n" rel-path) 'font-lock-face 'font-lock-constant-face)))
               "\n"))

      (when user-modified
        (insert (propertize "[Modified via ediff]\n" 'face 'warning)))

      ;; Insert diff content
      (let ((diff-start (point)))
        (insert diff-string)
        (insert "\n")
        ;; Apply syntax highlighting to diff content
        (gptel-agent--fontify-block 'diff-mode diff-start (point))
        ;; Apply background color
        (font-lock-append-text-property
         start (point) 'font-lock-face (gptel-agent--block-bg))
        (when (derived-mode-p 'org-mode)
          (org-escape-code-in-region start (1- (point)))))

      (insert (propertize "Keys: " 'font-lock-face 'help-key-binding))
      (insert (propertize "RET" 'font-lock-face 'help-key-binding))
      (insert " approve  ")
      (insert (propertize "q" 'font-lock-face 'help-key-binding))
      (insert " reject  ")
      (insert (propertize "e" 'font-lock-face 'help-key-binding))
      (insert " edit  ")
      (insert (propertize "f" 'font-lock-face 'help-key-binding))
      (insert " feedback  ")
      (insert (propertize "TAB" 'font-lock-face 'help-key-binding))
      (insert " toggle\n")
      (insert (propertize "\n" 'font-lock-face '(:inherit font-lock-string-face :underline t :extend t)))

      ;; Create overlay with context
      (let ((ov (mevedel-tools--confirm-overlay start (point) t)))
        (overlay-put ov 'mevedel--temp-file temp-file)
        (overlay-put ov 'mevedel--real-path real-path)
        (overlay-put ov 'mevedel--final-callback final-callback)
        (overlay-put ov 'mevedel--user-modified user-modified)
        (overlay-put ov 'mevedel--chat-buffer chat-buffer)
        (overlay-put ov 'mevedel--workspace workspace)
        (overlay-put ov 'mevedel--root root)
        (when tool-name
          (overlay-put ov 'mevedel--tool-name tool-name))
        (when diff-buffer
          (overlay-put ov 'mevedel--diff-buffer diff-buffer))
        (when apply-fn
          (overlay-put ov 'mevedel--apply-fn apply-fn))
        ov))))

(defun mevedel-tools--confirm-overlay (from to &optional no-hide)
  "Set up tool call preview overlay FROM TO.

If NO-HIDE is non-nil, don't hide the overlay body by default."
  (let ((ov (make-overlay from to nil t)))
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'mevedel-inline-preview t)
    (overlay-put ov 'priority 10)
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo
                 (concat "Approval requested: "
                         (propertize "Keys: C-c C-c approve  C-c C-k reject  C-c C-e edit  C-c C-f feedback  TAB toggle\n"
                                     'face 'help-key-binding)))
    (overlay-put ov 'keymap
                 (define-keymap
                   ;; Mouse support
                   "<mouse-1>" #'mevedel-tools--dispatch-inline-preview
                   ;; Navigation keys (simple, less likely to conflict)
                   "n"        #'mevedel-tools--next-preview-overlay
                   "p"        #'mevedel-tools--previous-preview-overlay
                   "<tab>"    #'mevedel-tools--cycle-overlay
                   "TAB"      #'mevedel-tools--cycle-overlay
                   ;; Action keys
                   "C-c C-c"  #'mevedel-tools--approve-inline-preview
                   "C-c C-k"  #'mevedel-tools--reject-inline-preview
                   "C-c C-e"  #'mevedel-tools--edit-inline-preview
                   "C-c C-f"  #'mevedel-tools--feedback-inline-preview
                   "RET"      #'mevedel-tools--approve-inline-preview
                   "<return>" #'mevedel-tools--approve-inline-preview
                   "a"        #'mevedel-tools--approve-inline-preview
                   "r"        #'mevedel-tools--reject-inline-preview
                   "q"        #'mevedel-tools--reject-inline-preview
                   "C-g"      #'mevedel-tools--reject-inline-preview
                   "e"        #'mevedel-tools--edit-inline-preview
                   "f"        #'mevedel-tools--feedback-inline-preview))
    (unless no-hide
      (mevedel-tools--cycle-overlay ov))
    ov))

(defun mevedel-tools--cycle-overlay (ov)
  "Cycle tool call preview overlay OV between collapsed and expanded."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'mevedel-inline-preview))))
  (when ov
    (save-excursion
      (goto-char (overlay-start ov))
      (let ((line-end (line-end-position))
            (end      (overlay-end ov)))
        (pcase-let ((`(,value . ,hide-ov)
                     (get-char-property-and-overlay line-end 'invisible)))
          (if (and hide-ov (eq value t))
              (delete-overlay hide-ov)
            (unless hide-ov (setq hide-ov (make-overlay line-end (1- end) nil t)))
            (overlay-put hide-ov 'evaporate t)
            (overlay-put hide-ov 'invisible t)
            (overlay-put hide-ov 'before-string " ▼")))))))

(defun mevedel-tools--dispatch-inline-preview ()
  "Prompt user for action on inline preview via mouse click."
  (interactive)
  (let ((choice (read-char-choice
                 "Action: (a)pprove, (r)eject, (e)dit, (f)eedback: "
                 '(?a ?r ?e ?f))))
    (pcase choice
      (?a (call-interactively #'mevedel-tools--approve-inline-preview))
      (?r (call-interactively #'mevedel-tools--reject-inline-preview))
      (?e (call-interactively #'mevedel-tools--edit-inline-preview))
      (?f (call-interactively #'mevedel-tools--feedback-inline-preview)))))

(defun mevedel-tools--approve-inline-preview ()
  "Approve the inline preview at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback))
              (chat-buffer (overlay-get ov 'mevedel--chat-buffer))
              (workspace (overlay-get ov 'mevedel--workspace))
              (root (overlay-get ov 'mevedel--root)))
    (let ((user-modified (overlay-get ov 'mevedel--user-modified)))
      ;; Apply changes
      (condition-case err
          (progn
            (let ((apply-fn (overlay-get ov 'mevedel--apply-fn))
                  (diff-buffer (overlay-get ov 'mevedel--diff-buffer)))
              (if (or user-modified (not apply-fn))
                  ;; User-modified: always use diff-apply (the diff buffer
                  ;; has the updated patch from ediff, while apply-fn
                  ;; would use stale temp-file content).
                  ;; No apply-fn: default overlay-preserving diff.
                  (with-current-buffer diff-buffer
                    (mevedel-diff-apply-buffer))
                (funcall apply-fn))
              (when (buffer-live-p diff-buffer)
                (kill-buffer diff-buffer)))

            (delete-file temp-file)
            (funcall final-callback
                     (if user-modified
                         (format "Changes approved and applied to %s, but the user edited the diff before approving. The user's edits are FINAL and authoritative -- do NOT revert or overwrite them. Read the file to see what was actually applied." real-path)
                       (format "Changes approved and applied to %s" real-path)))
            ;; Clean up overlay
            (let ((start (overlay-start ov))
                  (end (overlay-end ov)))
              (delete-overlay ov)
              (delete-region start end)))
        (error
         (funcall final-callback
                  (format "Error applying changes: %s" (error-message-string err)))
         (let ((start (overlay-start ov))
               (end (overlay-end ov)))
           (delete-overlay ov)
           (delete-region start end)))))))

(defun mevedel-tools--reject-inline-preview ()
  "Reject the inline preview at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback)))
    (delete-file temp-file)
    ;; Call callback with rejection message before cleanup
    (funcall final-callback
             (format "Changes to %s were rejected by user" real-path))
    (let ((start (overlay-start ov))
          (end (overlay-end ov)))
      (delete-overlay ov)
      (delete-region start end))
    ;; Abort entire execution
    (mevedel-abort)))

(defun mevedel-tools--feedback-inline-preview ()
  "Reject the inline preview at point with feedback."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (final-callback (overlay-get ov 'mevedel--final-callback)))
    (let ((feedback (read-string "What should be changed? ")))
      (funcall final-callback
               (format "Changes rejected by user. User feedback: %s" feedback))
      (delete-file temp-file)
      (let ((start (overlay-start ov))
            (end (overlay-end ov)))
        (delete-overlay ov)
        (delete-region start end)))))

(defun mevedel-tools--edit-inline-preview ()
  "Edit the inline preview at point using ediff."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (chat-buffer (overlay-get ov 'mevedel--chat-buffer))
              (workspace (overlay-get ov 'mevedel--workspace))
              (root (overlay-get ov 'mevedel--root)))
    ;; Mark as user-modified
    (overlay-put ov 'mevedel--user-modified t)
    ;; Store overlay in a variable for the hook to find
    (setq mevedel-tools--current-inline-preview-overlay ov)
    ;; Kill any existing diff buffer so the new one gets the canonical
    ;; name that mevedel-ediff-patch and its hooks look up.
    (when-let* ((old-buf (overlay-get ov 'mevedel--diff-buffer)))
      (when (buffer-live-p old-buf)
        (kill-buffer old-buf)))
    ;; Create and setup diff buffer for ediff
    (let ((diff-buffer (mevedel-tools--setup-diff-buffer
                        temp-file real-path workspace root chat-buffer)))
      ;; Store the new diff buffer in the overlay so the return hook can find it
      (overlay-put ov 'mevedel--diff-buffer diff-buffer)
      ;; Add one-shot hook to return to inline preview after ediff
      (add-hook 'mevedel--ediff-finished-hook
                #'mevedel-tools--return-to-inline-preview)
      (with-current-buffer diff-buffer
        (mevedel-ediff-patch)))))

(defun mevedel-tools--return-to-inline-preview ()
  "Return to inline preview after ediff session completes.

Updates the inline preview with any changes made during the ediff session.
The temp file has been updated by `mevedel--create-patch-from-ediff' with
the user's ediff modifications, so we regenerate the diff buffer from
scratch for a clean, well-formed diff."
  (remove-hook 'mevedel--ediff-finished-hook
               #'mevedel-tools--return-to-inline-preview)
  (when-let ((ov mevedel-tools--current-inline-preview-overlay)
             (chat-buffer (overlay-get ov 'mevedel--chat-buffer)))
    (with-current-buffer chat-buffer
      (let* ((overlay-start (overlay-start ov))
             (overlay-end (overlay-end ov))
             (temp-file (overlay-get ov 'mevedel--temp-file))
             (real-path (overlay-get ov 'mevedel--real-path))
             (final-callback (overlay-get ov 'mevedel--final-callback))
             (workspace (overlay-get ov 'mevedel--workspace))
             (root (overlay-get ov 'mevedel--root))
             (tool-name (overlay-get ov 'mevedel--tool-name))
             (apply-fn (overlay-get ov 'mevedel--apply-fn))
             (rel-path (file-relative-name real-path root))
             ;; Kill old diff buffer and regenerate from the updated temp
             ;; file vs the (restored) real file on disk.
             (old-diff-buffer (overlay-get ov 'mevedel--diff-buffer))
             (new-diff-buffer (progn
                                (when (and old-diff-buffer
                                          (buffer-live-p old-diff-buffer))
                                  (kill-buffer old-diff-buffer))
                                (mevedel-tools--setup-diff-buffer
                                 temp-file real-path workspace root
                                 chat-buffer)))
             (updated-diff (with-current-buffer new-diff-buffer
                             (buffer-string))))

        ;; Delete the old overlay and its region
        (delete-overlay ov)
        (delete-region overlay-start overlay-end)

        ;; Recreate the preview with updated content at the same position
        (mevedel-tools--create-inline-preview-overlay
         updated-diff temp-file real-path final-callback
         chat-buffer workspace root rel-path
         t  ; user-modified = t
         overlay-start
         tool-name new-diff-buffer apply-fn)

        ;; Show the chat buffer to the user
        (display-buffer chat-buffer gptel-display-buffer-action)
        (goto-char overlay-start))

      (setq mevedel-tools--current-inline-preview-overlay nil))))

(defun mevedel-tools--next-preview-overlay ()
  "Jump to the next mevedel inline preview overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (end (overlay-end ov)))
    (when (get-char-property end 'mevedel-inline-preview)
      (goto-char end))))

(defun mevedel-tools--previous-preview-overlay ()
  "Jump to the previous mevedel inline preview overlay."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (start (overlay-start ov)))
    (goto-char start)
    (when-let ((prev-ov (cdr (get-char-property-and-overlay
                              (1- start) 'mevedel-inline-preview))))
      (goto-char (overlay-start prev-ov)))))


;;
;;; Buffer preview

(defun mevedel-tools--prompt-for-changes (diff-buffer)
  "Prompt user to approve/reject/edit change in DIFF-BUFFER.

DIFF-BUFFER must have the buffer-local variables set by
`mevedel-tools--setup-diff-buffer'."
  (unless (buffer-live-p diff-buffer)
    (error "No diff-preview buffer found"))

  ;; REVIEW 2026-03-07: Maybe change to `display-buffer'
  (pop-to-buffer diff-buffer)

  (let ((choice (read-char-choice
                 "Apply changes? (a)pprove, (r)eject, (e)dit, (f)eedback: "
                 '(?a ?r ?e ?f)))
        (temp-file (buffer-local-value 'mevedel--temp-file diff-buffer))
        (real-path (buffer-local-value 'mevedel--real-path diff-buffer))
        (final-callback (buffer-local-value 'mevedel--final-callback diff-buffer))
        (user-modified (buffer-local-value 'mevedel--user-modified diff-buffer))
        (original-wconf (buffer-local-value 'mevedel--original-window-config diff-buffer)))

    (pcase choice
      (?a
       ;; Approved - apply changes
       (let ((apply-fn (buffer-local-value 'mevedel--apply-fn diff-buffer)))
         (if (or user-modified (not apply-fn))
             ;; User-modified: always use diff-apply (updated patch).
             ;; No apply-fn: default overlay-preserving diff.
             (with-current-buffer diff-buffer
               (mevedel-diff-apply-buffer))
           (funcall apply-fn)))
       ;; Note: Patch buffer will be updated with final diffs at request end
       (funcall final-callback
                (if user-modified
                    (format "Changes approved and applied to %s, but the user edited the diff before approving. The user's edits are FINAL and authoritative -- do NOT revert or overwrite them. Read the file to see what was actually applied." real-path)
                  (format "Changes approved and applied to %s" real-path)))
       ;; Cleanup and restore window config
       (kill-buffer diff-buffer)
       (delete-file temp-file)
       (when (window-configuration-p original-wconf)
         (set-window-configuration original-wconf)))
      (?r
       ;; Rejected without feedback
       (funcall final-callback
                "Changes rejected by user. No feedback provided.")
       ;; Cleanup and restore window config
       (kill-buffer diff-buffer)
       (delete-file temp-file)
       (when (window-configuration-p original-wconf)
         (set-window-configuration original-wconf))
       (mevedel-abort))
      (?e
       ;; Run `ediff' on patch - set up hook to return here after ediff
       ;; Do NOT restore window config - let ediff manage windows
       (with-current-buffer diff-buffer
         (setq-local mevedel--user-modified t))
       ;; Add one-shot hook to return to confirmation after ediff;
       ;; use a closure to capture the specific diff-buffer.
       (let ((captured-buf diff-buffer))
         (add-hook 'mevedel--ediff-finished-hook
                   (lambda () (mevedel-tools--prompt-for-changes captured-buf))))
       (with-current-buffer diff-buffer
         (mevedel-ediff-patch)))
      (?f
       ;; Rejected with feedback
       (let ((feedback (read-string "What should be changed? ")))
         (funcall final-callback
                  (format "Changes rejected by user. User feedback: %s" feedback)))
       ;; Cleanup and restore window config
       (kill-buffer diff-buffer)
       (delete-file temp-file)
       (when (window-configuration-p original-wconf)
         (set-window-configuration original-wconf))))))

(provide 'mevedel-preview-mode)
;;; mevedel-preview-mode.el ends here
