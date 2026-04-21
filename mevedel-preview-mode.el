;;; mevedel-preview-mode.el -- Inline diff preview -*- lexical-binding: t -*-

;;; Commentary:

;; Inline and buffer-based diff preview system for mevedel tool results.
;; Shows proposed file changes for user approval with approve/reject/edit/feedback
;; workflow.  Small diffs are displayed as overlays inline in the chat buffer;
;; larger diffs open in a separate buffer.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; `gptel-agent-tools'
(declare-function gptel-agent--fontify-block "ext:gptel-agent-tools" (path-or-mode start end))
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `gptel'
(defvar gptel-display-buffer-action)

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-diff-apply'
(declare-function mevedel-diff-apply-buffer "mevedel-diff-apply" ())

;; `mevedel-file-state'
(declare-function mevedel-session-record-file-access
                  "mevedel-file-state" (session path kind))

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--view-buffer)
(defvar mevedel--data-buffer)
(declare-function mevedel-request-cancel-fn "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(defvar mevedel-view--input-marker)

;; `mevedel-tool-fs'
(declare-function mevedel-tool-fs--setup-diff-buffer "mevedel-tool-fs"
                  (temp-file real-path workspace root
                             &optional chat-buffer labels-real))

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
  "Ratio of chat buffer height to use for initial preview expansion.

All previews are displayed inline in the chat buffer. Diffs with fewer
lines than this ratio times the visible height of the chat buffer start
expanded; larger diffs start collapsed and can be toggled open with
`TAB'.

Set to 0 to start every preview collapsed, or 1.0 to expand previews up
to the full chat window height."
  :type 'number
  :group 'mevedel)

(defvar mevedel-preview-mode--current-overlay nil
  "Stores the current preview overlay during an ediff session.")


;;
;;; Variables

(defvar-local mevedel-preview-mode--pending nil
  "List of pending inline preview overlays in the current chat buffer.
Ordered oldest first.  Each entry is an overlay with the
`mevedel-inline-preview' property.")


;;
;;; Minor mode

(defvar mevedel-preview-mode-map
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix "n" #'mevedel-preview-mode-next)
    (define-key prefix "p" #'mevedel-preview-mode-previous)
    (define-key prefix "a" #'mevedel-preview-mode-approve-all)
    (define-key prefix "r" #'mevedel-preview-mode-reject-all)
    (define-key map (kbd "C-c p") prefix)
    map)
  "Keymap for `mevedel-preview-mode'.
Bindings live under the `C-c p' prefix so they don't shadow ordinary
typing in a chat buffer.")

(defun mevedel-preview-mode--lighter ()
  "Return the mode-line lighter string for `mevedel-preview-mode'."
  (if mevedel-preview-mode--pending
      (format " Preview[%d]" (length mevedel-preview-mode--pending))
    " Preview"))

(define-minor-mode mevedel-preview-mode
  "Minor mode managing inline diff previews in a mevedel chat buffer.

Activated automatically when a tool produces a diff preview, deactivated
when the last pending preview is resolved.  Tracks pending overlays in
`mevedel-preview-mode--pending' and exposes navigation and batch
operations under the `\\[mevedel-preview-mode-next]' prefix.

\\{mevedel-preview-mode-map}"
  :init-value nil
  :lighter (:eval (mevedel-preview-mode--lighter))
  :keymap mevedel-preview-mode-map
  (unless mevedel-preview-mode
    (setq mevedel-preview-mode--pending nil)))

(defun mevedel-preview-mode--register (overlay)
  "Register OVERLAY as a pending preview in its chat buffer.
Activates `mevedel-preview-mode' if not already active and installs
`mevedel-preview-mode-dismiss-all' as the active request's cancel-fn."
  (with-current-buffer (overlay-buffer overlay)
    (unless mevedel-preview-mode
      (mevedel-preview-mode 1))
    (setq mevedel-preview-mode--pending
          (append mevedel-preview-mode--pending (list overlay)))
    ;; Access the request from the data buffer (where gptel runs),
    ;; not the view buffer (where overlays live).
    (let* ((data-buf (or (and (boundp 'mevedel--data-buffer)
                              mevedel--data-buffer
                              (buffer-live-p mevedel--data-buffer)
                              mevedel--data-buffer)
                         (current-buffer)))
           (request (buffer-local-value 'mevedel--current-request data-buf)))
      (when (and request (null (mevedel-request-cancel-fn request)))
        (let ((buf (current-buffer)))
          (setf (mevedel-request-cancel-fn request)
                (lambda ()
                  (when (buffer-live-p buf)
                    (with-current-buffer buf
                      (mevedel-preview-mode-dismiss-all))))))))
    (force-mode-line-update)))

(defun mevedel-preview-mode--unregister (overlay)
  "Unregister OVERLAY from the pending preview list.
Deactivates `mevedel-preview-mode' if the list becomes empty."
  (when (buffer-live-p (overlay-buffer overlay))
    (with-current-buffer (overlay-buffer overlay)
      (setq mevedel-preview-mode--pending
            (delq overlay mevedel-preview-mode--pending))
      (force-mode-line-update)
      (unless mevedel-preview-mode--pending
        (mevedel-preview-mode -1)))))


;;
;;; Inline preview

(cl-defun mevedel-preview-mode-add-preview (&key temp-file path callback
                                                 apply-fn tool-name
                                                 &allow-other-keys)
  "Add a diff preview for the changes staged in TEMP-FILE.

Keyword arguments:
  :TEMP-FILE  path to a temporary file holding the proposed content
              (required)
  :PATH       the real file path being modified (required)
  :CALLBACK   function of one string argument, invoked with the final
              tool result (approved / rejected / error) (required)
  :APPLY-FN   optional thunk invoked to apply the changes when the
              user approves.  Called with no arguments in the diff
              buffer's context.  Defaults to `mevedel-diff-apply-buffer'
              (overlay-preserving patch).  Tools that create new files
              or do full replacements should pass a function that
              writes TEMP-FILE content to PATH directly.
  :TOOL-NAME  optional display tag (e.g. \"Write\", \"Edit\") shown in
              the preview header.

The rendered unified diff is derived internally from TEMP-FILE versus
the current contents of PATH; callers do not pre-compute it.  This is
the single public entry point for tool handlers that need user
confirmation of a file change.  Activates `mevedel-preview-mode' in the
current chat buffer on first call and registers the overlay in the
pending list."
  (unless temp-file
    (error ":temp-file is required"))
  (unless path
    (error ":path is required"))
  (unless callback
    (error ":callback is required"))
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel-preview-mode-add-preview' must be called from chat buffer context"))
  (let* ((data-buffer (current-buffer))
         (chat-buffer (or (and (boundp 'mevedel--view-buffer)
                               mevedel--view-buffer
                               (buffer-live-p mevedel--view-buffer)
                               mevedel--view-buffer)
                          data-buffer))
         (root (or (mevedel-workspace--file-in-allowed-roots-p path data-buffer)
                   (file-name-directory (expand-file-name path))))
         (workspace (mevedel-workspace data-buffer))
         (rel-path (file-relative-name path root))
         (diff-buffer (mevedel-tool-fs--setup-diff-buffer
                       temp-file path workspace root data-buffer))
         (diff (with-current-buffer diff-buffer (buffer-string))))
    (mevedel-preview-mode--show-inline-preview
     diff temp-file path callback
     chat-buffer workspace root rel-path
     :tool-name tool-name
     :diff-buffer diff-buffer
     :apply-fn apply-fn
     :collapsed (mevedel-preview-mode--should-collapse-p diff chat-buffer))))

(defun mevedel-preview-mode--should-collapse-p (diff-string chat-buffer)
  "Return non-nil when DIFF-STRING should start collapsed in CHAT-BUFFER.

Compares the number of lines in DIFF-STRING against
`mevedel-inline-preview-threshold' times the visible height of
CHAT-BUFFER's window.  When the window height is unavailable, previews
start expanded."
  (if (<= mevedel-inline-preview-threshold 0)
      t
    (let* ((diff-lines (with-temp-buffer
                         (insert diff-string)
                         (count-lines (point-min) (point-max))))
           (chat-window (get-buffer-window chat-buffer))
           (chat-height (and chat-window (window-height chat-window))))
      (and chat-height
           (> diff-lines (* chat-height mevedel-inline-preview-threshold))))))

(cl-defun mevedel-preview-mode--show-inline-preview (diff-string temp-file real-path final-callback
                                                                 chat-buffer workspace root rel-path
                                                                 &key tool-name diff-buffer apply-fn
                                                                 user-modified position collapsed)
  "Show DIFF-STRING as a preview overlay in CHAT-BUFFER."
  (let ((ov (mevedel-preview-mode--create-overlay
             diff-string temp-file real-path final-callback
             chat-buffer workspace root rel-path
             :user-modified user-modified
             :position position
             :tool-name tool-name
             :diff-buffer diff-buffer
             :apply-fn apply-fn
             :collapsed collapsed)))
    (with-current-buffer chat-buffer
      (goto-char (overlay-start ov)))))

(cl-defun mevedel-preview-mode--create-overlay (diff-string temp-file real-path
                                                            final-callback chat-buffer
                                                            workspace root rel-path
                                                            &key user-modified position tool-name
                                                            diff-buffer apply-fn collapsed)
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
    (goto-char (or position
                   (and (boundp 'mevedel-view--input-marker)
                        mevedel-view--input-marker)
                   (point-max)))
    (let ((start (point))
          (inhibit-read-only t))
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
      (let ((ov (mevedel-preview-mode--setup-overlay start (point) collapsed)))
        (overlay-put ov 'mevedel--temp-file temp-file)
        (overlay-put ov 'mevedel--real-path real-path)
        (overlay-put ov 'mevedel--final-callback final-callback)
        (overlay-put ov 'mevedel--user-modified user-modified)
        ;; Store the data buffer (where session and workspace live),
        ;; not the view buffer (where the overlay is displayed).
        (overlay-put ov 'mevedel--data-buffer
                     (or (and (boundp 'mevedel--data-buffer)
                              (buffer-local-value 'mevedel--data-buffer
                                                  chat-buffer))
                         chat-buffer))
        (overlay-put ov 'mevedel--workspace workspace)
        (overlay-put ov 'mevedel--root root)
        (when tool-name
          (overlay-put ov 'mevedel--tool-name tool-name))
        (when diff-buffer
          (overlay-put ov 'mevedel--diff-buffer diff-buffer))
        (when apply-fn
          (overlay-put ov 'mevedel--apply-fn apply-fn))
        (mevedel-preview-mode--register ov)
        ov))))

(defun mevedel-preview-mode--setup-overlay (from to &optional collapsed)
  "Set up a preview overlay FROM TO.

When COLLAPSED is non-nil, start with the diff body hidden."
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
                   "<mouse-1>" #'mevedel-preview-mode-dispatch
                   "n"        #'mevedel-preview-mode-next
                   "p"        #'mevedel-preview-mode-previous
                   "<tab>"    #'mevedel-preview-mode-toggle-overlay
                   "TAB"      #'mevedel-preview-mode-toggle-overlay
                   "C-c C-c"  #'mevedel-preview-mode-approve
                   "C-c C-k"  #'mevedel-preview-mode-reject
                   "C-c C-e"  #'mevedel-preview-mode-edit
                   "C-c C-f"  #'mevedel-preview-mode-feedback
                   "RET"      #'mevedel-preview-mode-approve
                   "<return>" #'mevedel-preview-mode-approve
                   "a"        #'mevedel-preview-mode-approve
                   "r"        #'mevedel-preview-mode-reject
                   "q"        #'mevedel-preview-mode-reject
                   "C-g"      #'mevedel-preview-mode-reject
                   "e"        #'mevedel-preview-mode-edit
                   "f"        #'mevedel-preview-mode-feedback))
    (when collapsed
      (mevedel-preview-mode-toggle-overlay ov))
    ov))

(defun mevedel-preview-mode-toggle-overlay (ov)
  "Toggle preview overlay OV between collapsed and expanded."
  (interactive (list (cdr (get-char-property-and-overlay
                           (point) 'mevedel-inline-preview))))
  (when ov
    (save-excursion
      (goto-char (overlay-start ov))
      (let ((line-end (line-end-position))
            (end (overlay-end ov)))
        (pcase-let ((`(,value . ,hide-ov)
                     (get-char-property-and-overlay line-end 'invisible)))
          (if (and hide-ov (eq value t))
              (delete-overlay hide-ov)
            (unless hide-ov
              (setq hide-ov (make-overlay line-end (1- end) nil t)))
            (overlay-put hide-ov 'evaporate t)
            (overlay-put hide-ov 'invisible t)
            (overlay-put hide-ov 'before-string " ▼")))))))

(defun mevedel-preview-mode-dispatch ()
  "Prompt user for action on preview via mouse click."
  (interactive)
  (let ((choice (read-char-choice
                 "Action: (a)pprove, (r)eject, (e)dit, (f)eedback: "
                 '(?a ?r ?e ?f))))
    (pcase choice
      (?a (call-interactively #'mevedel-preview-mode-approve))
      (?r (call-interactively #'mevedel-preview-mode-reject))
      (?e (call-interactively #'mevedel-preview-mode-edit))
      (?f (call-interactively #'mevedel-preview-mode-feedback)))))

(defun mevedel-preview-mode--cleanup-overlay (ov)
  "Delete OV and its region, remove its temp file, unregister from the mode.
If `mevedel--ediff-created-stub' is set on the overlay (we created an
empty file at `real-path' to make ediff work on a new-file preview),
delete that stub as part of cleanup.  The approve path clears the flag
before calling here so that successfully-applied content is preserved."
  (let ((temp-file (overlay-get ov 'mevedel--temp-file))
        (real-path (overlay-get ov 'mevedel--real-path))
        (stub-p (overlay-get ov 'mevedel--ediff-created-stub))
        (start (overlay-start ov))
        (end (overlay-end ov)))
    (when (and temp-file (file-exists-p temp-file))
      (ignore-errors (delete-file temp-file)))
    (when (and stub-p real-path (file-exists-p real-path))
      (ignore-errors (delete-file real-path)))
    (mevedel-preview-mode--unregister ov)
    (delete-overlay ov)
    (when (and start end)
      (let ((inhibit-read-only t))
        (delete-region start end)))))

(defun mevedel-preview-mode--apply-overlay (ov)
  "Apply the changes recorded on preview overlay OV.
Returns the result string to deliver to `final-callback'.  Signals on
failure; callers should wrap in `condition-case'."
  (let* ((user-modified (overlay-get ov 'mevedel--user-modified))
         (real-path (overlay-get ov 'mevedel--real-path))
         (apply-fn (overlay-get ov 'mevedel--apply-fn))
         (diff-buffer (overlay-get ov 'mevedel--diff-buffer))
         (chat-buffer (overlay-get ov 'mevedel--data-buffer)))
    (if (or user-modified (not apply-fn))
        ;; User-modified: always use diff-apply (the diff buffer has the
        ;; updated patch from ediff, while apply-fn would use stale
        ;; temp-file content).  No apply-fn: default overlay-preserving
        ;; diff.
        (with-current-buffer diff-buffer
          (mevedel-diff-apply-buffer))
      (funcall apply-fn))
    ;; If we created an empty stub at real-path to enable ediff on a
    ;; new-file preview, the stub now holds the approved content --
    ;; clear the flag so `cleanup-overlay' doesn't delete it.
    (overlay-put ov 'mevedel--ediff-created-stub nil)
    (when (buffer-live-p diff-buffer)
      (kill-buffer diff-buffer))
    (when-let* ((session (and (buffer-live-p chat-buffer)
                              (buffer-local-value 'mevedel--session
                                                  chat-buffer))))
      (mevedel-session-record-file-access session real-path 'modify))
    (if user-modified
        (format "Changes approved and applied to %s, but the user edited the diff before approving. The user's edits are FINAL and authoritative -- do NOT revert or overwrite them. Read the file to see what was actually applied." real-path)
      (format "Changes approved and applied to %s" real-path))))

(defun mevedel-preview-mode--approve-overlay (ov)
  "Approve OV: apply changes, fire callback, clean up.
Does not invoke `mevedel-abort'."
  (let ((final-callback (overlay-get ov 'mevedel--final-callback))
        (result nil))
    (condition-case err
        (setq result (mevedel-preview-mode--apply-overlay ov))
      (error
       (setq result (format "Error applying changes: %s"
                            (error-message-string err)))))
    (mevedel-preview-mode--cleanup-overlay ov)
    (when final-callback
      (funcall final-callback result))))

(defun mevedel-preview-mode--reject-overlay (ov &optional feedback)
  "Reject OV: fire callback with rejection message, clean up.
If FEEDBACK is non-nil, include it in the rejection message.  Does not
invoke `mevedel-abort'."
  (let ((final-callback (overlay-get ov 'mevedel--final-callback))
        (real-path (overlay-get ov 'mevedel--real-path)))
    (when final-callback
      (funcall final-callback
               (if feedback
                   (format "Changes rejected by user. User feedback: %s"
                           feedback)
                 (format "Changes to %s were rejected by user"
                         real-path))))
    (mevedel-preview-mode--cleanup-overlay ov)))

(defun mevedel-preview-mode-approve ()
  "Approve the inline preview at point."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview))))
    (mevedel-preview-mode--approve-overlay ov)))

(defun mevedel-preview-mode-reject ()
  "Reject the inline preview at point and abort the request."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview))))
    (mevedel-preview-mode--reject-overlay ov)
    (mevedel-abort)))

(defun mevedel-preview-mode-feedback ()
  "Reject the inline preview at point with feedback."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview))))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel-preview-mode--reject-overlay ov feedback))))

(defun mevedel-preview-mode-edit ()
  "Edit the inline preview at point using ediff."
  (interactive)
  (when-let* ((ov (cdr (get-char-property-and-overlay
                        (point) 'mevedel-inline-preview)))
              (temp-file (overlay-get ov 'mevedel--temp-file))
              (real-path (overlay-get ov 'mevedel--real-path))
              (chat-buffer (overlay-get ov 'mevedel--data-buffer))
              (workspace (overlay-get ov 'mevedel--workspace))
              (root (overlay-get ov 'mevedel--root)))
    ;; Ediff-based patch editing needs the target file to exist on
    ;; disk (ediff-dispatch-file-patching-job loads it as buffer A).
    ;; For a brand-new file there is nothing on disk yet, so create an
    ;; empty stub at real-path and flag the overlay -- `cleanup-overlay'
    ;; deletes the stub if the user rejects, and `apply-overlay' clears
    ;; the flag so approve keeps the stub (now holding real content).
    ;; Also ensure the parent directory exists.
    (unless (file-exists-p real-path)
      (let ((parent (file-name-directory (expand-file-name real-path))))
        (unless (file-directory-p parent)
          (make-directory parent t)))
      (with-temp-file real-path)
      (overlay-put ov 'mevedel--ediff-created-stub t))
    (overlay-put ov 'mevedel--user-modified t)
    (setq mevedel-preview-mode--current-overlay ov)
    ;; Kill any existing diff buffer so the new one gets the canonical
    ;; name that mevedel-ediff-patch and its hooks look up.
    (when-let* ((old-buf (overlay-get ov 'mevedel--diff-buffer)))
      (when (buffer-live-p old-buf)
        (kill-buffer old-buf)))
    ;; Create and setup diff buffer for ediff.  Force real labels so the
    ;; patch header points at real-path -- ediff's patching dispatcher
    ;; resolves its source file from the diff header and chokes on
    ;; `/dev/null'.
    (let ((diff-buffer (mevedel-tool-fs--setup-diff-buffer
                        temp-file real-path workspace root chat-buffer t)))
      (overlay-put ov 'mevedel--diff-buffer diff-buffer)
      (add-hook 'mevedel--ediff-finished-hook
                #'mevedel-preview-mode--return-after-ediff)
      (with-current-buffer diff-buffer
        (mevedel-ediff-patch)))))

(defun mevedel-preview-mode--return-after-ediff ()
  "Return to inline preview after ediff session completes.

Updates the inline preview with any changes made during the ediff session.
The temp file has been updated by `mevedel--create-patch-from-ediff' with
the user's ediff modifications, so we regenerate the diff buffer from
scratch for a clean, well-formed diff."
  (remove-hook 'mevedel--ediff-finished-hook
               #'mevedel-preview-mode--return-after-ediff)
  (when-let ((ov mevedel-preview-mode--current-overlay)
             (data-buffer (overlay-get ov 'mevedel--data-buffer))
             (view-buffer (overlay-buffer ov)))
    (with-current-buffer view-buffer
      (let* ((overlay-start (overlay-start ov))
             (overlay-end (overlay-end ov))
             (temp-file (overlay-get ov 'mevedel--temp-file))
             (real-path (overlay-get ov 'mevedel--real-path))
             (final-callback (overlay-get ov 'mevedel--final-callback))
             (workspace (overlay-get ov 'mevedel--workspace))
             (root (overlay-get ov 'mevedel--root))
             (tool-name (overlay-get ov 'mevedel--tool-name))
             (apply-fn (overlay-get ov 'mevedel--apply-fn))
             (stub-p (overlay-get ov 'mevedel--ediff-created-stub))
             (rel-path (file-relative-name real-path root))
             ;; Kill old diff buffer and regenerate from the updated temp
             ;; file vs the (restored) real file on disk.
             (old-diff-buffer (overlay-get ov 'mevedel--diff-buffer))
             (new-diff-buffer (progn
                                (when (and old-diff-buffer
                                          (buffer-live-p old-diff-buffer))
                                  (kill-buffer old-diff-buffer))
                                (mevedel-tool-fs--setup-diff-buffer
                                 temp-file real-path workspace root
                                 data-buffer stub-p)))
             (updated-diff (with-current-buffer new-diff-buffer
                             (buffer-string)))
             (inhibit-read-only t))

        (mevedel-preview-mode--unregister ov)
        (delete-overlay ov)
        (delete-region overlay-start overlay-end)

        (let ((new-ov (mevedel-preview-mode--create-overlay
                       updated-diff temp-file real-path final-callback
                       view-buffer workspace root rel-path
                       :user-modified t
                       :position overlay-start
                       :tool-name tool-name
                       :diff-buffer new-diff-buffer
                       :apply-fn apply-fn
                       :collapsed (mevedel-preview-mode--should-collapse-p
                                   updated-diff view-buffer))))
          (when stub-p
            (overlay-put new-ov 'mevedel--ediff-created-stub t)))

        (display-buffer view-buffer gptel-display-buffer-action)
        (goto-char overlay-start))

      (setq mevedel-preview-mode--current-overlay nil))))



;;
;;; Mode commands (navigation, batch ops, dismiss)

(defun mevedel-preview-mode-next ()
  "Jump to the next pending preview in this buffer.
If already on a preview, advance to the one after it; otherwise jump to
the first pending preview."
  (interactive)
  (when mevedel-preview-mode--pending
    (let* ((current (cdr (get-char-property-and-overlay
                          (point) 'mevedel-inline-preview)))
           (tail (when current (cdr (memq current mevedel-preview-mode--pending))))
           (target (or (car tail) (car mevedel-preview-mode--pending))))
      (when target
        (goto-char (overlay-start target))))))

(defun mevedel-preview-mode-previous ()
  "Jump to the previous pending preview in this buffer.
If already on a preview, go to the one before it; otherwise jump to
the last pending preview."
  (interactive)
  (when mevedel-preview-mode--pending
    (let* ((current (cdr (get-char-property-and-overlay
                          (point) 'mevedel-inline-preview)))
           (idx (and current (seq-position mevedel-preview-mode--pending current)))
           (target (cond
                    ((null idx)
                     (car (last mevedel-preview-mode--pending)))
                    ((zerop idx)
                     (car (last mevedel-preview-mode--pending)))
                    (t (nth (1- idx) mevedel-preview-mode--pending)))))
      (when target
        (goto-char (overlay-start target))))))

(defun mevedel-preview-mode-approve-all ()
  "Approve every pending preview in this buffer."
  (interactive)
  (dolist (ov (copy-sequence mevedel-preview-mode--pending))
    (when (overlay-buffer ov)
      (mevedel-preview-mode--approve-overlay ov))))

(defun mevedel-preview-mode-reject-all ()
  "Reject every pending preview in this buffer, then abort the request."
  (interactive)
  (let ((had-pending (and mevedel-preview-mode--pending t)))
    (dolist (ov (copy-sequence mevedel-preview-mode--pending))
      (when (overlay-buffer ov)
        (mevedel-preview-mode--reject-overlay ov)))
    (when had-pending
      (mevedel-abort))))

(defun mevedel-preview-mode-dismiss-all ()
  "Clean up every pending preview without firing any callback.
Intended as the cancel-fn for an aborting request: overlays disappear,
temp files are removed, but tool continuations are not invoked (the
request is already being torn down)."
  (interactive)
  (dolist (ov (copy-sequence mevedel-preview-mode--pending))
    (when (overlay-buffer ov)
      (mevedel-preview-mode--cleanup-overlay ov))))

(provide 'mevedel-preview-mode)
;;; mevedel-preview-mode.el ends here
