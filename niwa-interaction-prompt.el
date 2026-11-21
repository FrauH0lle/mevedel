;;; niwa-interaction-prompt.el -- Shared interaction prompts -*- lexical-binding: t -*-

;; Copyright (C) 2025 Karthik Chikmagalur
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Background face selection adapted from gptel-agent-tools.el.

;;; Commentary:

;; Shared overlay lifecycle and visual primitives for user interactions.
;; Domain-specific Ask, access, permission, and plan prompts build on this
;; boundary while retaining ownership of their own outcomes and rendering.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())
(declare-function niwa-request-push-canceller
                  "niwa-turn" (request canceller))
(autoload 'niwa-current-origin "niwa-turn")
(autoload 'niwa-request-push-canceller "niwa-turn")

;; `niwa-utilities'
(declare-function niwa--warn-once
                  "niwa-utilities" (scope key format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa-diagnostic-scope "niwa-utilities")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-register
                  "niwa-view-interaction" (descriptor))
(declare-function niwa-view--interaction-target-buffer
                  "niwa-view-interaction" (&optional data-buffer))
(declare-function niwa-view--interaction-unregister
                  "niwa-view-interaction" (id))
(defvar niwa-view--interaction-overlays)

(defvar-local niwa--prompt-overlays nil
  "List of pending niwa-user-request overlays in this buffer.
Each carries a `niwa--callback' overlay property -- a one-arg
thunk receiving `approve' / `deny' / (feedback . TEXT) / `aborted'.")

(defvar niwa-interaction-prompt-created-hook nil
  "Run with a prompt OVERLAY after its interaction is registered.

An overlay carrying a `niwa--remote' descriptor may be presented on
another surface, such as a collaboration guest.  The descriptor is a
plist:

  :body      plain text describing the request
  :body-kind \"text\" (default) or \"diff\"
  :options   ordered list of (OUTCOME . LABEL); OUTCOME is a settle
             value or a zero-arg function
  :feedback  non-nil allows free-text feedback -- t settles with
             (feedback . TEXT), a function receives TEXT and owns the
             whole flow
  :questions zero-arg function returning the questionnaire as
             JSON-safe alists with current answers
  :answer    function receiving the complete answer list atomically")

(defvar niwa-interaction-prompt-settled-hook nil
  "Run with a prompt OVERLAY after it settled through any path.")

(defun niwa--prompt-announce (overlay)
  "Run the creation hook for prompt OVERLAY."
  (run-hook-with-args 'niwa-interaction-prompt-created-hook overlay))

(defvar-local niwa--prompt-canceller-registered-for nil
  "Weak-key set of requests with a registered prompt canceller.
Only the first overlay per request pushes a canceller onto that request,
without making the view buffer retain completed requests.")

(defun niwa--prompt--data-buffer (&optional buffer)
  "Return the data buffer reachable from `current-buffer', else nil.
Prefer the `niwa--data-buffer' back-pointer set on view and derived
buffers before accepting the current buffer.  View buffers also carry
`niwa--session', but their agent registry and active request state
live on the data buffer.

When BUFFER is non-nil, resolve from that buffer instead of the
current one."
  (let* ((cur (or buffer (current-buffer)))
         (db (and (buffer-live-p cur)
                  (ignore-errors
                    (buffer-local-value 'niwa--data-buffer cur)))))
    (or (and db (buffer-live-p db)
             (buffer-local-value 'niwa--session db)
             db)
        (and (buffer-live-p cur)
             (ignore-errors
               (buffer-local-value 'niwa--session cur))
             cur))))

(defun niwa--prompt--registered-for-p (request)
  "Return non-nil when this buffer already registered REQUEST."
  (and (hash-table-p niwa--prompt-canceller-registered-for)
       (gethash request niwa--prompt-canceller-registered-for)))

(defun niwa--prompt--mark-registered-for (request)
  "Record that this buffer has registered a canceller for REQUEST."
  (unless (hash-table-p niwa--prompt-canceller-registered-for)
    (setq niwa--prompt-canceller-registered-for
          (make-hash-table :test #'eq :weakness 'key)))
  (puthash request t niwa--prompt-canceller-registered-for))

(defun niwa--prompt--register-canceller (&optional source-buffer overlay)
  "Push the prompt-dismiss thunk onto the active request's cancellers list.

Idempotent per request: subsequent overlays in the same request do
not push a duplicate.  Also installs `niwa--prompt-dismiss-all' on
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
    (when-let* ((data-buf (niwa--prompt--data-buffer source-buffer))
                (request (buffer-local-value 'niwa--current-request
                                             data-buf)))
      (with-current-buffer prompt-buffer
        (when (overlayp overlay)
          (overlay-put overlay 'niwa--owning-request request)
          (overlay-put overlay 'niwa-diagnostic-scope
                       (niwa-diagnostic-scope data-buf)))
        (unless (niwa--prompt--registered-for-p request)
          (let ((buf prompt-buffer))
            (niwa-request-push-canceller
             request
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (niwa--prompt-dismiss-request request))))))
          (niwa--prompt--mark-registered-for request)))))
  (add-hook 'kill-buffer-hook #'niwa--prompt-dismiss-all nil t))

(defun niwa--prompt--settle (overlay outcome)
  "Settle OVERLAY's callback exactly once with OUTCOME.

The `niwa-settled' overlay property gates this.  Remove OVERLAY
from the buffer's pending list and interaction zone before firing
the stored callback."
  (when (and (overlayp overlay)
             (not (overlay-get overlay 'niwa-settled)))
    (overlay-put overlay 'niwa-settled t)
    (let ((cb (overlay-get overlay 'niwa--callback))
          (interaction-id (overlay-get overlay 'niwa-view-interaction-id))
          (buf (overlay-buffer overlay))
          (start (overlay-start overlay))
          (end (overlay-end overlay)))
      (unless (buffer-live-p buf)
        (setq cb nil)
        (niwa--warn-once (or (overlay-get overlay 'niwa-diagnostic-scope) 'unavailable) 'interaction-prompt-stale
                            "Stale interaction prompt activation ignored"))
      (when (and interaction-id
                 (boundp 'niwa-view--interaction-overlays)
                 (hash-table-p niwa-view--interaction-overlays)
                 (not (eq overlay
                          (gethash interaction-id
                                   niwa-view--interaction-overlays))))
        (setq cb nil)
        (niwa--warn-once (or (overlay-get overlay 'niwa-diagnostic-scope)
                               (niwa-diagnostic-scope buf)) 'interaction-prompt-stale
                            "Stale interaction prompt activation ignored"))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (setq niwa--prompt-overlays
                (delq overlay niwa--prompt-overlays))
          (when (and interaction-id
                     (fboundp 'niwa-view--interaction-unregister))
            (niwa-view--interaction-unregister interaction-id))
          (let ((inhibit-read-only t))
            (delete-overlay overlay)
            (when (and (not interaction-id)
                       start end (>= end start) (not (eq start end)))
              (ignore-errors (delete-region start end))))))
      (run-hook-with-args 'niwa-interaction-prompt-settled-hook overlay)
      (when cb
        (funcall cb outcome)))))

(defun niwa--prompt--overlay-at-point (property)
  "Return prompt overlay at point carrying PROPERTY.
Falls back to the `niwa-view-interaction-overlay' text property
set on materialized interaction-zone descriptor text."
  (or (cdr (get-char-property-and-overlay (point) property))
      (let ((ov (get-text-property (point) 'niwa-view-interaction-overlay)))
        (and (overlayp ov) (overlay-get ov property) ov))))

(defun niwa--prompt-dismiss-request (request)
  "Settle pending prompt overlays owned by REQUEST with `aborted'."
  (dolist (overlay (copy-sequence niwa--prompt-overlays))
    (when (eq request (overlay-get overlay 'niwa--owning-request))
      (niwa--prompt--settle overlay 'aborted))))

(defun niwa--prompt-dismiss-all ()
  "Settle every pending prompt overlay in this buffer with `aborted'."
  (dolist (overlay (copy-sequence niwa--prompt-overlays))
    (niwa--prompt--settle overlay 'aborted)))

(defun niwa--approve-request ()
  "Approve the prompt overlay at point."
  (interactive)
  (when-let* ((overlay
               (niwa--prompt--overlay-at-point 'niwa-user-request)))
    (niwa--prompt--settle overlay 'approve)))

(defun niwa--deny-request ()
  "Deny the prompt overlay at point without aborting the request."
  (interactive)
  (when-let* ((overlay
               (niwa--prompt--overlay-at-point 'niwa-user-request)))
    (niwa--prompt--settle overlay 'deny)))

(defun niwa--feedback-request ()
  "Settle the prompt overlay at point with feedback text."
  (interactive)
  (when-let* ((overlay
               (niwa--prompt--overlay-at-point 'niwa-user-request)))
    (let ((feedback (read-string "What should be changed? ")))
      (niwa--prompt--settle overlay (cons 'feedback feedback)))))

(defun niwa--prompt-framed-body (content face)
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
     0 (length body) 'font-lock-face (niwa--prompt-block-face) body)
    body))

(defun niwa--prompt-block-face ()
  "Return a background face for framed prompt content."
  (cond
   ((derived-mode-p 'org-mode) 'org-block)
   ((derived-mode-p 'markdown-mode) 'markdown-code-face)
   (t `(:background ,(face-attribute 'mode-line-inactive :background)
         :extend t))))

(defun niwa--prompt-key (key)
  "Return propertized KEY for prompt key-help rows."
  (propertize key 'font-lock-face 'help-key-binding
              'niwa-prompt-focus (equal key "RET")))

(defun niwa--prompt-attribution-line (origin)
  "Return a standard attribution line for canonical ORIGIN."
  (if (or (null origin) (equal origin "/root"))
      ""
    (concat
     (propertize (format "from %s" origin)
                 'font-lock-face 'niwa-view-attribution)
     "\n")))

(defun niwa--prompt-user-with-overlay
    (title content question help-echo-text callback &optional host-only
           audience)
  "Display a confirmation overlay and settle CALLBACK exactly once.

CALLBACK receives `approve', `deny', `(feedback . TEXT)', or
`aborted'.  TITLE is the heading text, CONTENT describes the
request, QUESTION is the final question, and HELP-ECHO-TEXT is
optional hover text.

With HOST-ONLY non-nil the overlay carries no `niwa--remote'
descriptor and so is never mirrored to a collaboration guest.  Use it
for a decision no guest may make.

AUDIENCE narrows the guests a mirrored interaction reaches below the
writable default.  Nil reaches every writable guest; `owner' reaches
only owner-link guests."
  (unless (memq audience '(nil owner))
    (error "Unknown collaboration audience: %S" audience))
  (let* ((source-buffer (current-buffer))
         (origin (niwa-current-origin))
         (target-buffer
          (if (fboundp 'niwa-view--interaction-target-buffer)
              (niwa-view--interaction-target-buffer
               (niwa--prompt--data-buffer source-buffer))
            (error "No live view for queued prompt")))
         (id (list :request (gensym "request-")))
         (body
          (niwa--prompt-framed-body
           (concat
            (propertize (format "%s\n" title)
                        'font-lock-face '(:inherit bold :inherit warning))
            (niwa--prompt-attribution-line origin)
            "\n"
            content
            "\n\n"
            (propertize (format "%s\n\n" question) 'font-lock-face 'bold)
            (propertize "Keys: " 'font-lock-face 'help-key-binding)
            (niwa--prompt-key "RET")
            " approve  "
            (niwa--prompt-key "d")
            " deny  "
            (niwa--prompt-key "f")
            " feedback\n")
           'warning))
         (keymap
          (define-keymap
            "y"        #'niwa--approve-request
            "a"        #'niwa--approve-request
            "RET"      #'niwa--approve-request
            "<return>" #'niwa--approve-request
            "C-c C-c"  #'niwa--approve-request
            "n"        #'niwa--deny-request
            "d"        #'niwa--deny-request
            "q"        #'niwa--deny-request
            "C-c C-k"  #'niwa--deny-request
            "C-g"      #'niwa--deny-request
            "f"        #'niwa--feedback-request))
         overlay)
    (with-current-buffer target-buffer
      (setq overlay
            (niwa-view--interaction-register
             (list :kind 'request
                   :id id
                   :origin origin
                   :count 0
                   :body body
                   :keymap keymap
                   :help-echo
                   (or help-echo-text
                       (concat title ": "
                               (propertize
                                "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                                'face 'help-key-binding)))
                   :activate callback)))
      (overlay-put overlay 'niwa-user-request t)
      (overlay-put overlay 'niwa--callback callback)
      (unless host-only
        (overlay-put overlay 'niwa--remote
                     (append
                      (list :body (substring-no-properties
                                   (format "%s\n\n%s\n\n%s"
                                           title content question))
                            :options '((approve . "Approve")
                                       (deny . "Deny"))
                            :feedback t)
                      (when audience (list :audience audience)))))
      (cl-pushnew overlay niwa--prompt-overlays :test #'eq)
      (niwa--prompt--register-canceller source-buffer overlay)
      (niwa--prompt-announce overlay))
    overlay))

(provide 'niwa-interaction-prompt)

;;; niwa-interaction-prompt.el ends here
