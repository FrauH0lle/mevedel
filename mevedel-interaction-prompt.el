;;; mevedel-interaction-prompt.el -- Shared interaction prompts -*- lexical-binding: t -*-

;;; Commentary:

;; Shared overlay lifecycle and visual primitives for user interactions.
;; Domain-specific Ask, access, permission, and plan prompts build on this
;; boundary while retaining ownership of their own outcomes and rendering.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel-agent-tools'
(declare-function gptel-agent--block-bg "ext:gptel-agent-tools" ())

;; `mevedel-structs'
(declare-function mevedel-current-origin "mevedel-structs" ())
(declare-function mevedel-request-push-canceller
                  "mevedel-structs" (request canceller))

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-register
                  "mevedel-view-interaction" (descriptor))
(declare-function mevedel-view--interaction-target-buffer
                  "mevedel-view-interaction" (&optional data-buffer))
(declare-function mevedel-view--interaction-unregister
                  "mevedel-view-interaction" (id))
(defvar mevedel-view--interaction-overlays)

(defvar-local mevedel--prompt-overlays nil
  "List of pending mevedel-user-request overlays in this buffer.
Each carries a `mevedel--callback' overlay property -- a one-arg
thunk receiving `approve' / `deny' / (feedback . TEXT) / `aborted'.")

(defvar-local mevedel--prompt-canceller-registered-for nil
  "The `mevedel-request' structs we registered dismiss cancellers onto.
Only the first overlay per request pushes a canceller onto that
request's cancellers list.")

(defun mevedel--prompt--data-buffer (&optional buffer)
  "Return the data buffer reachable from `current-buffer', else nil.
Prefer the `mevedel--data-buffer' back-pointer set on view and derived
buffers before accepting the current buffer.  View buffers also carry
`mevedel--session', but their agent registry and active request state
live on the data buffer.

When BUFFER is non-nil, resolve from that buffer instead of the
current one."
  (let* ((cur (or buffer (current-buffer)))
         (db (and (buffer-live-p cur)
                  (ignore-errors
                    (buffer-local-value 'mevedel--data-buffer cur)))))
    (or (and db (buffer-live-p db)
             (buffer-local-value 'mevedel--session db)
             db)
        (and (buffer-live-p cur)
             (ignore-errors
               (buffer-local-value 'mevedel--session cur))
             cur))))

(defun mevedel--prompt--registered-for-p (request)
  "Return non-nil when this buffer already registered REQUEST."
  (memq request mevedel--prompt-canceller-registered-for))

(defun mevedel--prompt--mark-registered-for (request)
  "Record that this buffer has registered a canceller for REQUEST."
  (unless (mevedel--prompt--registered-for-p request)
    (push request mevedel--prompt-canceller-registered-for)))

(defun mevedel--prompt--register-canceller (&optional source-buffer overlay)
  "Push the prompt-dismiss thunk onto the active request's cancellers list.

Idempotent per request: subsequent overlays in the same request do
not push a duplicate.  Also installs `mevedel--prompt-dismiss-all' on
the buffer's `kill-buffer-hook' so killing the chat buffer settles
every pending overlay with `aborted'.

SOURCE-BUFFER, when non-nil, is used to find the owning request.
This matters when a sub-agent prompt is rendered in the parent view:
the overlay lives in the parent view, but the active request belongs
to the agent data buffer.  OVERLAY, when non-nil, is tagged with the
owning request so shared view buffers only cancel request-local
prompts during request teardown."
  (let ((prompt-buffer (current-buffer))
        (source-buffer (or source-buffer (current-buffer))))
    (when-let* ((data-buf (mevedel--prompt--data-buffer source-buffer))
                (request (buffer-local-value 'mevedel--current-request
                                             data-buf)))
      (with-current-buffer prompt-buffer
        (when (overlayp overlay)
          (overlay-put overlay 'mevedel--owning-request request))
        (unless (mevedel--prompt--registered-for-p request)
          (let ((buf prompt-buffer))
            (mevedel-request-push-canceller
             request
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (mevedel--prompt-dismiss-request request))))))
          (mevedel--prompt--mark-registered-for request)))))
  (add-hook 'kill-buffer-hook #'mevedel--prompt-dismiss-all nil t))

(defun mevedel--prompt--settle (overlay outcome)
  "Settle OVERLAY's callback exactly once with OUTCOME.

The `mevedel-settled' overlay property gates this.  Remove OVERLAY
from the buffer's pending list and interaction zone before firing
the stored callback."
  (when (and (overlayp overlay)
             (not (overlay-get overlay 'mevedel-settled)))
    (overlay-put overlay 'mevedel-settled t)
    (let ((cb (overlay-get overlay 'mevedel--callback))
          (interaction-id (overlay-get overlay 'mevedel-view-interaction-id))
          (buf (overlay-buffer overlay))
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (unless (buffer-live-p buf)
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (and interaction-id
                 (boundp 'mevedel-view--interaction-overlays)
                 (hash-table-p mevedel-view--interaction-overlays)
                 (not (eq overlay
                          (gethash interaction-id
                                   mevedel-view--interaction-overlays))))
        (setq cb nil)
        (display-warning
         'mevedel
         "Stale interaction prompt activation ignored"
         :warning))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq mevedel--prompt-overlays
                (delq overlay mevedel--prompt-overlays))
          (when (and interaction-id
                     (fboundp 'mevedel-view--interaction-unregister))
            (mevedel-view--interaction-unregister interaction-id))
          (let ((inhibit-read-only t))
            (delete-overlay overlay)
            (when (and (not interaction-id)
                       start end (>= end start) (not (eq start end)))
              (ignore-errors (delete-region start end))))))
      (when cb
        (funcall cb outcome)))))

(defun mevedel--prompt--overlay-at-point (property)
  "Return prompt overlay at point carrying PROPERTY.
Falls back to the `mevedel-view-interaction-overlay' text property
set on materialized interaction-zone descriptor text."
  (or (cdr (get-char-property-and-overlay (point) property))
      (let ((ov (get-text-property (point) 'mevedel-view-interaction-overlay)))
        (and (overlayp ov) (overlay-get ov property) ov))))

(defun mevedel--prompt-dismiss-request (request)
  "Settle pending prompt overlays owned by REQUEST with `aborted'."
  (dolist (overlay (copy-sequence mevedel--prompt-overlays))
    (when (eq request (overlay-get overlay 'mevedel--owning-request))
      (mevedel--prompt--settle overlay 'aborted))))

(defun mevedel--prompt-dismiss-all ()
  "Settle every pending prompt overlay in this buffer with `aborted'."
  (dolist (overlay (copy-sequence mevedel--prompt-overlays))
    (mevedel--prompt--settle overlay 'aborted)))

(defun mevedel--approve-request ()
  "Approve the prompt overlay at point."
  (interactive)
  (when-let* ((overlay
               (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle overlay 'approve)))

(defun mevedel--deny-request ()
  "Deny the prompt overlay at point without aborting the request."
  (interactive)
  (when-let* ((overlay
               (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (mevedel--prompt--settle overlay 'deny)))

(defun mevedel--feedback-request ()
  "Settle the prompt overlay at point with feedback text."
  (interactive)
  (when-let* ((overlay
               (mevedel--prompt--overlay-at-point 'mevedel-user-request)))
    (let ((feedback (read-string "What should be changed? ")))
      (mevedel--prompt--settle overlay (cons 'feedback feedback)))))

(defun mevedel--prompt-framed-body (content face)
  "Return CONTENT inside the standard interaction prompt frame.
FACE is inherited by the top and bottom rule lines."
  (let ((body
         (concat
          "\n"
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t))
          content
          (propertize "\n" 'font-lock-face
                      `(:inherit ,face :underline t :extend t)))))
    (font-lock-append-text-property
     0 (length body) 'font-lock-face (gptel-agent--block-bg) body)
    body))

(defun mevedel--prompt-key (key)
  "Return propertized KEY for prompt key-help rows."
  (propertize key 'font-lock-face 'help-key-binding))

(defun mevedel--prompt-attribution-line (origin)
  "Return a standard attribution line for canonical ORIGIN."
  (if (or (null origin) (equal origin "/root"))
      ""
    (concat
     (propertize (format "from %s" origin)
                 'font-lock-face 'mevedel-view-attribution)
     "\n")))

(defun mevedel--prompt-user-with-overlay
    (title content question help-echo-text callback)
  "Display a confirmation overlay and settle CALLBACK exactly once.

CALLBACK receives `approve', `deny', `(feedback . TEXT)', or
`aborted'.  TITLE is the heading text, CONTENT describes the
request, QUESTION is the final question, and HELP-ECHO-TEXT is
optional hover text."
  (let* ((source-buffer (current-buffer))
         (origin (mevedel-current-origin))
         (target-buffer
          (if (fboundp 'mevedel-view--interaction-target-buffer)
              (mevedel-view--interaction-target-buffer
               (mevedel--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (id (list :request (gensym "request-")))
         (body
          (mevedel--prompt-framed-body
           (concat
            (propertize (format "%s\n" title)
                        'font-lock-face '(:inherit bold :inherit warning))
            (mevedel--prompt-attribution-line origin)
            "\n"
            content
            "\n\n"
            (propertize (format "%s\n\n" question) 'font-lock-face 'bold)
            (propertize "Keys: " 'font-lock-face 'help-key-binding)
            (mevedel--prompt-key "a")
            " approve  "
            (mevedel--prompt-key "d")
            " deny  "
            (mevedel--prompt-key "f")
            " feedback\n")
           'warning))
         (keymap
          (define-keymap
            "y"        #'mevedel--approve-request
            "a"        #'mevedel--approve-request
            "RET"      #'mevedel--approve-request
            "<return>" #'mevedel--approve-request
            "C-c C-c"  #'mevedel--approve-request
            "n"        #'mevedel--deny-request
            "d"        #'mevedel--deny-request
            "q"        #'mevedel--deny-request
            "C-c C-k"  #'mevedel--deny-request
            "C-g"      #'mevedel--deny-request
            "f"        #'mevedel--feedback-request))
         overlay)
    (with-current-buffer target-buffer
      (setq overlay
            (mevedel-view--interaction-register
             (list :kind 'request
                   :id id
                   :origin origin
                   :count 0
                   :body body
                   :priority 150
                   :keymap keymap
                   :help-echo
                   (or help-echo-text
                       (concat title ": "
                               (propertize
                                "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                                'face 'help-key-binding)))
                   :activate callback)))
      (overlay-put overlay 'mevedel-user-request t)
      (overlay-put overlay 'mevedel--callback callback)
      (cl-pushnew overlay mevedel--prompt-overlays :test #'eq)
      (mevedel--prompt--register-canceller source-buffer overlay))
    overlay))

(provide 'mevedel-interaction-prompt)

;;; mevedel-interaction-prompt.el ends here
