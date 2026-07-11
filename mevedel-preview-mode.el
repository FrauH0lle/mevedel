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

;; `mevedel-permissions'
(defvar mevedel-permission-mode)
(declare-function mevedel-permission-mode-transition
                  "mevedel-permissions"
                  (mode &optional prompt display-text hook-context))

;; `mevedel-structs'
(defvar mevedel--session)
(defvar mevedel--current-request)
(defvar mevedel--view-buffer)
(defvar mevedel--data-buffer)
(declare-function mevedel-request-push-canceller "mevedel-structs" (request canceller))
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)

;; `mevedel-view'
(declare-function mevedel-view--interaction-anchor "mevedel-view" ())
(declare-function mevedel-view--interaction-register "mevedel-view" (descriptor))
(declare-function mevedel-view--interaction-unregister "mevedel-view" (id))

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

All previews are displayed inline in the chat buffer.  Diffs with fewer
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

(defvar-local mevedel-preview-mode--canceller-registered-for nil
  "Request struct registered for preview dismiss cancellation.
This is the `mevedel-request' struct we registered the dismiss canceller
onto, or nil.  Used so only the first preview per request pushes a thunk
onto the composable cancellers list; subsequent overlays in the same
request do not double-register.")

(defvar mevedel-preview-mode--interaction-id-counter 0
  "Monotonic id counter for preview interaction-zone descriptors.")


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
Activates `mevedel-preview-mode' if not already active and pushes
a dismiss thunk onto the active request's cancellers list the first
time a preview is registered for that request."
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
      (when (and request
                 (not (eq request mevedel-preview-mode--canceller-registered-for)))
        (let ((buf (current-buffer)))
          (mevedel-request-push-canceller
           request
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (mevedel-preview-mode-dismiss-all))))))
        (setq mevedel-preview-mode--canceller-registered-for request)))
    ;; Killing the chat buffer outside `mevedel-abort' must still
    ;; settle the pending callbacks.  Idempotent -- `add-hook' is a
    ;; no-op when the function is already installed.
    (add-hook 'kill-buffer-hook #'mevedel-preview-mode-dismiss-all nil t)
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

(defun mevedel-preview-mode--interaction-anchor ()
  "Return the insertion anchor for a preview overlay.
Use the view interaction-zone anchor when the view module is loaded;
otherwise fall back to the end of the current buffer so preview mode
remains usable in tests and minimal chat buffers."
  (if (fboundp 'mevedel-view--interaction-anchor)
      (mevedel-view--interaction-anchor)
    (point-max)))

(defun mevedel-preview-mode--view-interaction-buffer-p (buffer)
  "Return non-nil when BUFFER can host preview interaction descriptors."
  (and (buffer-live-p buffer)
       (fboundp 'mevedel-view--interaction-register)
       (fboundp 'mevedel-view--interaction-unregister)
       (with-current-buffer buffer
         (derived-mode-p 'mevedel-view-mode))))

(defun mevedel-preview-mode--next-interaction-id ()
  "Return a fresh interaction descriptor id for a preview."
  (setq mevedel-preview-mode--interaction-id-counter
        (1+ mevedel-preview-mode--interaction-id-counter))
  (list 'preview mevedel-preview-mode--interaction-id-counter))

(defun mevedel-preview-mode--effective-mode ()
  "Return the effective permission mode for the current buffer.
Prefers the session's `permission-mode' slot; falls back to the
buffer-local or global `mevedel-permission-mode', then to `default'."
  (or (and (boundp 'mevedel--session)
           mevedel--session
           (mevedel-session-permission-mode mevedel--session))
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'default))

(cl-defun mevedel-preview-mode-add-preview (&key temp-file path callback
                                                 apply-fn tool-name
                                                 &allow-other-keys)
  "Add a diff preview for the proposed edits staged in TEMP-FILE.

Keyword arguments:
  :TEMP-FILE  path to a temporary file holding the proposed content
              (required)
  :PATH       the real file path being modified (required)
  :CALLBACK   function of one argument, invoked with the final tool
              result.  On success the argument is a plist
              `(:result STR :render-data (:kind diff ...))'; on rejection
              or error it is a plain string.  (required)
  :APPLY-FN   optional thunk invoked to apply the changes.  Called with
              no arguments in the diff buffer's context.  Defaults to
              `mevedel-diff-apply-buffer' (overlay-preserving patch).
              Tools that create new files or do full replacements should
              pass a function that writes TEMP-FILE content to PATH
              directly.
  :TOOL-NAME  optional display tag (e.g. \"Write\", \"Edit\") shown in
              the preview header.

The rendered unified diff is derived internally from TEMP-FILE versus
the current contents of PATH; callers do not pre-compute it.  This is
the single public entry point for tool handlers that need user
confirmation of a file change.

Behavior depends on the effective permission mode: under `accept-edits'
or `trust-all' the change is applied immediately without an interactive
overlay (see `mevedel-preview-mode--auto-apply').  Otherwise an inline
preview is shown and `mevedel-preview-mode' is activated in the current
chat buffer."
  (unless temp-file
    (error ":temp-file is required"))
  (unless path
    (error ":path is required"))
  (unless callback
    (error ":callback is required"))
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel-preview-mode-add-preview' must be called from chat buffer context"))
  (pcase (mevedel-preview-mode--effective-mode)
    ((or 'accept-edits 'trust-all)
     (mevedel-preview-mode--auto-apply
      temp-file path callback apply-fn tool-name))
    (_
     (mevedel-preview-mode--show-interactive
      temp-file path callback apply-fn tool-name))))

(defun mevedel-preview-mode--show-interactive (temp-file path callback apply-fn tool-name)
  "Show an interactive inline preview for TEMP-FILE vs PATH.
Called by `mevedel-preview-mode-add-preview' when the effective
permission mode requires user confirmation.  Sets up the diff buffer,
inserts the inline overlay, and registers it with `mevedel-preview-mode'.
CALLBACK is delivered the final plist after approve/reject; APPLY-FN and
TOOL-NAME carry through to the overlay."
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
    (mevedel-preview-mode--create-overlay
     diff temp-file path callback
     chat-buffer workspace root rel-path
     :tool-name tool-name
     :diff-buffer diff-buffer
     :apply-fn apply-fn
     :collapsed (mevedel-preview-mode--should-collapse-p diff chat-buffer))))

(defun mevedel-preview-mode--auto-apply (temp-file path callback apply-fn _tool-name)
  "Apply TEMP-FILE to PATH without an interactive overlay.
Computes the unified diff for the renderer side-channel, runs APPLY-FN
\(or the default overlay-preserving diff apply when APPLY-FN is nil),
records the file access, cleans up the diff buffer and temp file, and
fires CALLBACK with a `(:result :render-data)' plist carrying the patch
text.  Errors during apply are reported to CALLBACK as a plain error
string so the LLM still sees a descriptive failure."
  (let* ((data-buffer (current-buffer))
         (root (or (mevedel-workspace--file-in-allowed-roots-p path data-buffer)
                   (file-name-directory (expand-file-name path))))
         (workspace (mevedel-workspace data-buffer))
         (rel-path (ignore-errors (file-relative-name path root)))
         (diff-buffer (mevedel-tool-fs--setup-diff-buffer
                       temp-file path workspace root data-buffer))
         (patch (with-current-buffer diff-buffer (buffer-string)))
         (err-string nil))
    (unwind-protect
        (condition-case err
            (progn
              (if apply-fn
                  (funcall apply-fn)
                (with-current-buffer diff-buffer
                  (mevedel-diff-apply-buffer)))
              (when-let* ((session (buffer-local-value 'mevedel--session
                                                       data-buffer)))
                (mevedel-session-record-file-access session path 'modify)))
          (error
           (setq err-string (error-message-string err))))
      (when (buffer-live-p diff-buffer)
        (kill-buffer diff-buffer))
      (when (and temp-file (file-exists-p temp-file))
        (ignore-errors (delete-file temp-file))))
    (funcall callback
             (if err-string
                 (format "Error auto-applying changes to %s: %s"
                         path err-string)
               (list :result (format "Changes auto-applied to %s" path)
                     :render-data (list :kind 'diff
                                        :patch patch
                                        :path path
                                        :rel-path rel-path))))))

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

(defun mevedel-preview-mode--body-string (diff-string rel-path &optional user-modified)
  "Return propertized body for DIFF-STRING at REL-PATH.

USER-MODIFIED adds the edited marker when non-nil."
  (with-temp-buffer
    (when user-modified
      (insert (propertize "[Modified via ediff]\n" 'face 'warning)))
    (let ((diff-start (point)))
      (insert diff-string)
      (insert "\n")
      (gptel-agent--fontify-block 'diff-mode diff-start (point))
      (insert (mevedel-preview-mode--controls-body rel-path))
      (font-lock-append-text-property
       (point-min) (point) 'font-lock-face (gptel-agent--block-bg))
      (buffer-string))))

(defun mevedel-preview-mode--create-interaction-overlay
    (diff-string temp-file real-path final-callback chat-buffer workspace root
                 rel-path user-modified _position tool-name diff-buffer apply-fn
                 collapsed)
  "Create preview overlay for DIFF-STRING in CHAT-BUFFER's interaction zone.

TEMP-FILE, REAL-PATH, FINAL-CALLBACK, WORKSPACE, ROOT, REL-PATH,
USER-MODIFIED, TOOL-NAME, DIFF-BUFFER, APPLY-FN, and COLLAPSED describe
the preview and its callbacks."
  (with-current-buffer chat-buffer
    (let* ((id (mevedel-preview-mode--next-interaction-id))
           (prefix (if user-modified (length "[Modified via ediff]\n") 0))
           (body (mevedel-preview-mode--body-string
                  diff-string rel-path user-modified))
           (diff-body-start prefix)
           (diff-body-end (min (length body) (+ prefix 1 (length diff-string))))
           (overlay
            (mevedel-view--interaction-register
             (list :kind 'preview
                   :id id
                   :count 1
                   :body body
                   :priority 300
                   :keymap (mevedel-preview-mode--keymap)
                   :help-echo (mevedel-preview-mode--help-echo)))))
      (mevedel-preview-mode--apply-overlay-properties
       overlay collapsed diff-body-start diff-body-end)
      (overlay-put overlay 'evaporate nil)
      (overlay-put overlay 'mevedel--interaction-id id)
      (overlay-put overlay 'mevedel--temp-file temp-file)
      (overlay-put overlay 'mevedel--real-path real-path)
      (overlay-put overlay 'mevedel--rel-path rel-path)
      (overlay-put overlay 'mevedel--final-callback final-callback)
      (overlay-put overlay 'mevedel--user-modified user-modified)
      (overlay-put overlay 'mevedel--data-buffer
                   (or (and (boundp 'mevedel--data-buffer)
                            (buffer-local-value 'mevedel--data-buffer
                                                chat-buffer))
                       chat-buffer))
      (overlay-put overlay 'mevedel--workspace workspace)
      (overlay-put overlay 'mevedel--root root)
      (when tool-name
        (overlay-put overlay 'mevedel--tool-name tool-name))
      (when diff-buffer
        (overlay-put overlay 'mevedel--diff-buffer diff-buffer))
      (when apply-fn
        (overlay-put overlay 'mevedel--apply-fn apply-fn))
      (mevedel-preview-mode--register overlay)
      overlay)))

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
  (if (mevedel-preview-mode--view-interaction-buffer-p chat-buffer)
      (mevedel-preview-mode--create-interaction-overlay
       diff-string temp-file real-path final-callback chat-buffer workspace root
       rel-path user-modified position tool-name diff-buffer apply-fn collapsed)
    (with-current-buffer chat-buffer
      (save-excursion
        (goto-char (or position
                       (mevedel-preview-mode--interaction-anchor)))
        (let ((start (point))
              (inhibit-read-only t)
              diff-body-start-offset
              diff-body-end-offset)
          (when user-modified
            (insert (propertize "[Modified via ediff]\n" 'face 'warning)))

          ;; Insert diff content
          (let ((diff-start (point)))
            (insert diff-string)
            (insert "\n")
            ;; Apply syntax highlighting to diff content
            (gptel-agent--fontify-block 'diff-mode diff-start (point))
            (when (derived-mode-p 'org-mode)
              (org-escape-code-in-region start (1- (point))))
            (setq diff-body-start-offset (- diff-start start)
                  diff-body-end-offset (- (point) start)))
          (insert (mevedel-preview-mode--controls-body rel-path))
          ;; Apply background color to the full preview region: diff body
          ;; plus the controls row.  Syntax fontification above remains
          ;; scoped to the actual diff.
          (font-lock-append-text-property
           start (point) 'font-lock-face (gptel-agent--block-bg))
          (add-text-properties
           start (point)
           '(read-only t front-sticky nil rear-nonsticky t))

          ;; Create overlay with context
          (let ((ov (mevedel-preview-mode--apply-overlay-properties
                     (make-overlay start (point) nil t) collapsed
                     diff-body-start-offset diff-body-end-offset)))
            (overlay-put ov 'mevedel--temp-file temp-file)
            (overlay-put ov 'mevedel--real-path real-path)
            (overlay-put ov 'mevedel--rel-path rel-path)
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
            ov))))))

(defun mevedel-preview-mode--keymap ()
  "Return the keymap shared by raw and interaction-zone previews."
  (define-keymap
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
    "f"        #'mevedel-preview-mode-feedback
    "S"        #'mevedel-preview-mode-approve-and-trust))

(defun mevedel-preview-mode--help-echo ()
  "Return the help echo string shared by preview overlays."
  (concat "Approval requested: "
          (propertize "Keys: C-c C-c approve  C-c C-k reject  C-c C-e edit  C-c C-f feedback  S trust-rest  TAB toggle\n"
                      'face 'help-key-binding)))

(defun mevedel-preview-mode--controls-body (rel-path)
  "Return the interaction-zone controls body for preview REL-PATH."
  (concat
   "\n"
   (propertize "\n" 'font-lock-face
               '(:inherit font-lock-string-face :underline t :extend t))
   "Proposed changes to "
   (propertize (format "%s\n" rel-path)
               'font-lock-face 'font-lock-constant-face)
   (propertize "Keys: " 'font-lock-face 'help-key-binding)
   (propertize "RET" 'font-lock-face 'help-key-binding)
   " approve  "
   (propertize "q" 'font-lock-face 'help-key-binding)
   " reject  "
   (propertize "e" 'font-lock-face 'help-key-binding)
   " edit  "
   (propertize "f" 'font-lock-face 'help-key-binding)
   " feedback  "
   (propertize "S" 'font-lock-face 'help-key-binding)
   " trust-rest  "
   (propertize "TAB" 'font-lock-face 'help-key-binding)
   " toggle\n"
   (propertize "\n" 'font-lock-face
               '(:inherit font-lock-string-face :underline t :extend t))))

(defun mevedel-preview-mode--apply-overlay-properties
    (ov &optional collapsed body-start-offset body-end-offset)
  "Apply common preview properties to OV.
When COLLAPSED is non-nil, start with the diff body hidden.
BODY-START-OFFSET and BODY-END-OFFSET bound the diff content inside OV."
  (overlay-put ov 'evaporate t)
  (overlay-put ov 'mevedel-inline-preview t)
  (overlay-put ov 'read-only t)
  ;; Interaction-zone stacking: previews 300 > plan 200 >
  ;; permission 100.  Bumped from the legacy 10 so previews render
  ;; above plan / permission overlays at the same anchor.
  (overlay-put ov 'priority 300)
  (overlay-put ov 'mevedel--diff-body-start-offset body-start-offset)
  (overlay-put ov 'mevedel--diff-body-end-offset body-end-offset)
  (overlay-put ov 'help-echo (mevedel-preview-mode--help-echo))
  (overlay-put ov 'keymap (mevedel-preview-mode--keymap))
  (when collapsed
    (mevedel-preview-mode-toggle-overlay ov))
  ov)

(defun mevedel-preview-mode-toggle-overlay (ov)
  "Toggle preview overlay OV between collapsed and expanded.

When collapsed, hides only the diff body (the range stamped as
offsets when the overlay was built) so the header and key-hint rows
stay visible -- they are the cues that tell the user there is a
pending approval."
  (interactive (list (mevedel-preview-mode--overlay-at-point)))
  (when ov
    (let* ((ov-start (overlay-start ov))
           (ov-end (overlay-end ov))
           (body-start-offset (overlay-get ov 'mevedel--diff-body-start-offset))
           (body-end-offset (overlay-get ov 'mevedel--diff-body-end-offset))
           (hide-from (min ov-end (+ ov-start body-start-offset)))
           (hide-to (min ov-end (+ ov-start body-end-offset))))
      (pcase-let ((`(,value . ,hide-ov)
                   (get-char-property-and-overlay hide-from 'invisible)))
        (if (and hide-ov (eq value t))
            (delete-overlay hide-ov)
          (unless hide-ov
            (setq hide-ov (make-overlay hide-from hide-to nil t)))
          (overlay-put hide-ov 'evaporate t)
          (overlay-put hide-ov 'invisible t)
          (overlay-put hide-ov 'before-string
                       (propertize " ▼ TAB to expand\n"
                                   'face 'help-key-binding)))))))

(defun mevedel-preview-mode--overlay-at-point ()
  "Return the preview overlay targeted by point."
  (cdr (get-char-property-and-overlay (point) 'mevedel-inline-preview)))

(defun mevedel-preview-mode--cleanup-overlay (ov)
  "Delete OV and its region, remove its temp file, unregister from the mode.
If `mevedel--ediff-created-stub' is set on the overlay (we created an
empty file at `real-path' to make ediff work on a new-file preview),
delete that stub and its still-empty created parent directories as part
of cleanup.  The approve path clears the flag before calling here so
that successfully-applied content is preserved."
  (let ((temp-file (overlay-get ov 'mevedel--temp-file))
        (real-path (overlay-get ov 'mevedel--real-path))
        (stub-p (overlay-get ov 'mevedel--ediff-created-stub))
        (created-dir (overlay-get ov 'mevedel--ediff-created-directory))
        (existing-dir (overlay-get ov 'mevedel--ediff-existing-directory))
        (interaction-id (overlay-get ov 'mevedel--interaction-id))
        (buffer (overlay-buffer ov))
        (start (overlay-start ov))
        (end (overlay-end ov)))
    (when (and temp-file (file-exists-p temp-file))
      (ignore-errors (delete-file temp-file)))
    (when (and stub-p real-path (file-exists-p real-path))
      (ignore-errors (delete-file real-path)))
    (while (and stub-p
                created-dir
                (not (equal created-dir existing-dir))
                (ignore-errors (directory-empty-p created-dir)))
      (ignore-errors (delete-directory created-dir))
      (setq created-dir
            (file-name-directory (directory-file-name created-dir))))
    (mevedel-preview-mode--unregister ov)
    (if (and interaction-id buffer (buffer-live-p buffer)
             (fboundp 'mevedel-view--interaction-unregister))
        (with-current-buffer buffer
          (mevedel-view--interaction-unregister interaction-id))
      (delete-overlay ov)
      (when (and start end)
        (let ((inhibit-read-only t))
          (delete-region start end))))))

(defun mevedel-preview-mode--apply-overlay (ov)
  "Apply the proposed edits recorded on preview overlay OV.
Returns a plist `(:result STR :render-data (:kind diff ...))'  suitable
for `final-callback'; the pipeline splits the plist into LLM-facing
result and side-channel render-data.  Signals on failure; callers should
wrap in `condition-case'."
  (let* ((user-modified (overlay-get ov 'mevedel--user-modified))
         (real-path (overlay-get ov 'mevedel--real-path))
         (root (overlay-get ov 'mevedel--root))
         (apply-fn (overlay-get ov 'mevedel--apply-fn))
         (diff-buffer (overlay-get ov 'mevedel--diff-buffer))
         (chat-buffer (overlay-get ov 'mevedel--data-buffer))
         (patch (when (buffer-live-p diff-buffer)
                  (with-current-buffer diff-buffer (buffer-string)))))
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
    (let* ((result-str
            (if user-modified
                (format "Changes approved and applied to %s, but the user edited the diff before approving. The user's edits are FINAL and authoritative -- do NOT revert or overwrite them. Read the file to see what was actually applied." real-path)
              (format "Changes approved and applied to %s" real-path)))
           (rel-path (and root real-path
                          (ignore-errors (file-relative-name real-path root)))))
      (list :result result-str
            :render-data (list :kind 'diff
                               :patch (or patch "")
                               :path real-path
                               :rel-path rel-path)))))

(defun mevedel-preview-mode--approve-overlay (ov)
  "Approve OV, apply proposed edits, fire callback, and clean up.
On success, the callback receives the `(:result :render-data)' plist
from `mevedel-preview-mode--apply-overlay'; on error it receives a plain
error string.  The pipeline splits either shape.  Does not invoke
`mevedel-abort'."
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
  (when-let* ((ov (mevedel-preview-mode--overlay-at-point)))
    (mevedel-preview-mode--approve-overlay ov)))

(defun mevedel-preview-mode-approve-and-trust ()
  "Approve every pending preview in this buffer and flip mode to `accept-edits'.
Drains the pending list by running `--approve-overlay' on each overlay
\(which applies its change, fires its callback, and cleans up), then sets
the buffer-local `mevedel-permission-mode' on the associated data buffer
and the session's `permission-mode' slot so subsequent edits from the
current turn and future turns auto-apply.  Shell commands continue to
prompt -- the intent is scoped to edits, not blanket trust."
  (interactive)
  (let* ((pending (copy-sequence mevedel-preview-mode--pending))
         (count (length pending))
         (first-ov (car pending))
         (data-buffer (and first-ov
                           (overlay-get first-ov 'mevedel--data-buffer))))
    (dolist (ov pending)
      (when (overlay-buffer ov)
        (mevedel-preview-mode--approve-overlay ov)))
    (when (buffer-live-p data-buffer)
      (with-current-buffer data-buffer
        (if mevedel--session
            (progn
              (require 'mevedel-permissions)
              (mevedel-permission-mode-transition 'accept-edits))
          (setq-local mevedel-permission-mode 'accept-edits))))
    (message "accept-edits on. Applied %d pending edit%s. Shell commands still prompt."
             count (if (= count 1) "" "s"))))

(defun mevedel-preview-mode-reject ()
  "Reject the inline preview at point and abort the request.

Ordering is load-bearing: fire the rejection callback first so the
FSM advances out of TOOL (the tool sees the rejection result), then
`mevedel-abort' so any follow-up turn the FSM might have launched
is cancelled.  Reject-then-abort is intentional for edit rejection --
unlike a generic permission `deny', rejecting a Write/Edit overlay
expresses \"stop the whole sequence,\" not \"this one tool failed\"."
  (interactive)
  (when-let* ((ov (mevedel-preview-mode--overlay-at-point)))
    (mevedel-preview-mode--reject-overlay ov)
    (mevedel-abort)))

(defun mevedel-preview-mode-feedback ()
  "Reject the inline preview at point with feedback."
  (interactive)
  (when-let* ((ov (mevedel-preview-mode--overlay-at-point)))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel-preview-mode--reject-overlay ov feedback))))

(defun mevedel-preview-mode--prepare-ediff-target (ov real-path)
  "Create and record a temporary Ediff target for OV at REAL-PATH."
  (unless (file-exists-p real-path)
    (let* ((parent (file-name-directory (expand-file-name real-path)))
           (existing-dir parent))
      (while (not (file-exists-p existing-dir))
        (setq existing-dir
              (file-name-directory (directory-file-name existing-dir))))
      (unless (file-directory-p parent)
        (make-directory parent t)
        (overlay-put ov 'mevedel--ediff-created-directory parent)
        (overlay-put ov 'mevedel--ediff-existing-directory existing-dir)))
    (with-temp-file real-path)
    (overlay-put ov 'mevedel--ediff-created-stub t)))

(defun mevedel-preview-mode-edit ()
  "Edit the inline preview at point using ediff."
  (interactive)
  (when-let* ((ov (mevedel-preview-mode--overlay-at-point))
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
    (mevedel-preview-mode--prepare-ediff-target ov real-path)
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
             (created-dir
              (overlay-get ov 'mevedel--ediff-created-directory))
             (existing-dir
              (overlay-get ov 'mevedel--ediff-existing-directory))
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
        (if-let* ((interaction-id (overlay-get ov 'mevedel--interaction-id))
                  ((fboundp 'mevedel-view--interaction-unregister)))
            (mevedel-view--interaction-unregister interaction-id)
          (delete-overlay ov)
          (delete-region overlay-start overlay-end))

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
            (overlay-put new-ov 'mevedel--ediff-created-stub t)
            (overlay-put new-ov 'mevedel--ediff-created-directory created-dir)
            (overlay-put new-ov 'mevedel--ediff-existing-directory existing-dir)))

        (display-buffer view-buffer gptel-display-buffer-action))

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
  "Settle every pending preview overlay with `Error: aborted'.

Used as: (a) the canceller thunk pushed onto the request's cancellers
list when the first preview is registered, (b) the buffer-local
`kill-buffer-hook' entry installed alongside.  Each overlay's final
callback IS the tool callback (preview-mode is the degenerate case
where the UI/tool layers collapse), so firing it with a tool-result
string is the only way to advance an FSM parked in TOOL on this
preview.  Earlier behavior silently dropped the callback and stranded
the FSM."
  (interactive)
  (dolist (ov (copy-sequence mevedel-preview-mode--pending))
    (when (overlay-buffer ov)
      (let ((final-callback (overlay-get ov 'mevedel--final-callback)))
        (mevedel-preview-mode--cleanup-overlay ov)
        (when final-callback
          (funcall final-callback "Error: aborted"))))))

(provide 'mevedel-preview-mode)
;;; mevedel-preview-mode.el ends here
