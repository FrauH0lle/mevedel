;;; mevedel-cockpit.el -- Shared cockpit surface helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Small helpers for session-owned tabulated cockpit surfaces.  Resource
;; modules still own their rows, actions, keymaps, and details; this module
;; only keeps the repeated `tabulated-list-mode' shell behavior in one place.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-print "tabulated-list" (&optional remember-pos update))
(defvar tabulated-list-entries)

;; `transient'
(defvar transient--original-buffer)

(defvar-local mevedel-cockpit--context nil
  "Context plist that owns the current cockpit surface.")

(defun mevedel-cockpit--buffer-session (buffer)
  "Return BUFFER's local session, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (boundp 'mevedel--session) mevedel--session))))

(defun mevedel-cockpit--make-context (view-buffer data-buffer origin-buffer)
  "Return a cockpit context for VIEW-BUFFER, DATA-BUFFER, and ORIGIN-BUFFER."
  (let* ((session (mevedel-cockpit--buffer-session data-buffer))
         (workspace (and session (mevedel-session-workspace session))))
    (list :view-buffer view-buffer
          :data-buffer data-buffer
          :origin-buffer origin-buffer
          :session session
          :workspace workspace)))

(defun mevedel-cockpit-context-for-buffer (buffer)
  "Return BUFFER's cockpit context, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       (mevedel-cockpit--context
        mevedel-cockpit--context)
       ((and (derived-mode-p 'mevedel-view-mode)
             (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer))
        (mevedel-cockpit--make-context buffer mevedel--data-buffer buffer))
       ((and (boundp 'mevedel--view-buffer)
             (buffer-live-p mevedel--view-buffer)
             (with-current-buffer mevedel--view-buffer
               (and (derived-mode-p 'mevedel-view-mode)
                    (boundp 'mevedel--data-buffer)
                    (eq mevedel--data-buffer buffer))))
        (mevedel-cockpit--make-context mevedel--view-buffer buffer buffer))))))

(defun mevedel-cockpit-current-context ()
  "Return the cockpit context for the current command."
  (or (mevedel-cockpit-context-for-buffer (current-buffer))
      (and (boundp 'transient--original-buffer)
           (buffer-live-p transient--original-buffer)
           (mevedel-cockpit-context-for-buffer transient--original-buffer))
      (user-error "No mevedel session cockpit here")))

(defun mevedel-cockpit-context-view-buffer (&optional context)
  "Return CONTEXT's live view buffer, or nil."
  (let ((buffer (plist-get (or context (mevedel-cockpit-current-context))
                           :view-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun mevedel-cockpit-context-data-buffer (&optional context)
  "Return CONTEXT's live data buffer, or nil."
  (let ((buffer (plist-get (or context (mevedel-cockpit-current-context))
                           :data-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun mevedel-cockpit-context-origin-buffer (&optional context)
  "Return CONTEXT's live origin buffer, or nil."
  (let ((buffer (plist-get (or context (mevedel-cockpit-current-context))
                           :origin-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun mevedel-cockpit-context-session (&optional context)
  "Return CONTEXT's session, or nil."
  (let ((context (or context (mevedel-cockpit-current-context))))
    (or (plist-get context :session)
        (mevedel-cockpit--buffer-session
         (mevedel-cockpit-context-data-buffer context)))))

(defun mevedel-cockpit-context-workspace (&optional context)
  "Return CONTEXT's workspace, or nil."
  (let ((context (or context (mevedel-cockpit-current-context))))
    (or (plist-get context :workspace)
        (when-let* ((session (mevedel-cockpit-context-session context)))
          (mevedel-session-workspace session)))))

(defun mevedel-cockpit-call-in-view (context function &rest args)
  "Call FUNCTION with ARGS in CONTEXT's view buffer."
  (if-let* ((buffer (mevedel-cockpit-context-view-buffer context)))
      (with-current-buffer buffer
        (apply function args))
    (user-error "No live mevedel session for this cockpit surface")))

(defun mevedel-cockpit-call-in-data (context function &rest args)
  "Call FUNCTION with ARGS in CONTEXT's data buffer."
  (if-let* ((buffer (mevedel-cockpit-context-data-buffer context)))
      (with-current-buffer buffer
        (apply function args))
    (user-error "No live mevedel session for this cockpit surface")))

(defun mevedel-cockpit-data-buffer ()
  "Return the live data buffer that owns the current cockpit, or nil."
  (when-let* ((context (condition-case nil
                           (mevedel-cockpit-current-context)
                         (user-error nil))))
    (mevedel-cockpit-context-data-buffer context)))

(defun mevedel-cockpit-require-owner (&optional label context)
  "Signal a user error unless the current cockpit has live owners.
LABEL is a user-facing surface label used in the error message.
CONTEXT defaults to `mevedel-cockpit-current-context'."
  (let ((context (condition-case nil
                     (or context (mevedel-cockpit-current-context))
                   (user-error nil))))
    (unless (and context
                 (mevedel-cockpit-context-view-buffer context)
                 (mevedel-cockpit-context-data-buffer context))
      (user-error "No live mevedel session for this %s"
                  (or label "cockpit surface"))))
  t)

(defun mevedel-cockpit--return-buffer ()
  "Return the best live owner buffer for quitting this cockpit."
  (let ((context (condition-case nil
                     (mevedel-cockpit-current-context)
                   (user-error nil))))
    (or (and context (mevedel-cockpit-context-origin-buffer context))
        (and context (mevedel-cockpit-context-view-buffer context))
        (and context (mevedel-cockpit-context-data-buffer context)))))

(defun mevedel-cockpit--require-open-context (context label)
  "Return CONTEXT when it has live view and data buffers.
LABEL is used in the owner error."
  (let ((context (or context (mevedel-cockpit-current-context))))
    (unless (and (mevedel-cockpit-context-view-buffer context)
                 (mevedel-cockpit-context-data-buffer context))
      (user-error "No live mevedel session for this %s"
                  (or label "cockpit surface")))
    context))

(defun mevedel-cockpit-goto-id (id)
  "Move point to tabulated row ID, or to the first row when absent."
  (require 'tabulated-list)
  (goto-char (point-min))
  (catch 'found
    (when id
      (while (not (eobp))
        (when (equal (tabulated-list-get-id) id)
          (throw 'found t))
        (forward-line 1)))
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (tabulated-list-get-id)))
      (forward-line 1))))

(defun mevedel-cockpit-refresh-tabulated (entries &optional selected-id)
  "Render tabulated ENTRIES, preserving SELECTED-ID or current row."
  (require 'tabulated-list)
  (let ((selected (or selected-id (tabulated-list-get-id))))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)
    (mevedel-cockpit-goto-id selected)
    (force-mode-line-update t)))

(defun mevedel-cockpit-selected (items id-function)
  "Return selected item from ITEMS using ID-FUNCTION to match row id."
  (require 'tabulated-list)
  (when-let* ((id (tabulated-list-get-id)))
    (cl-find-if
     (lambda (item)
       (equal id (funcall id-function item)))
     items)))

(defun mevedel-cockpit-quit (&optional label)
  "Kill the current cockpit and return to the main session cockpit.
LABEL is a user-facing surface label used in the dead-owner error."
  (interactive)
  (let ((buffer (current-buffer))
        (return-buffer (mevedel-cockpit--return-buffer)))
    (when return-buffer
      (when-let* ((window (display-buffer return-buffer)))
        (select-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (unless return-buffer
      (user-error "No live mevedel session for this %s"
                  (or label "cockpit surface")))
    (with-current-buffer return-buffer
      (require 'mevedel-menu)
      (mevedel-menu))))

(defun mevedel-cockpit-open-tabulated
    (buffer-name mode refresh context &optional setup label)
  "Open a session-owned tabulated cockpit buffer.
BUFFER-NAME is created or reused, MODE initializes the buffer, REFRESH
renders rows, and SETUP runs after CONTEXT is recorded.  LABEL is a
user-facing surface label used in owner errors."
  (setq context (mevedel-cockpit--require-open-context context label))
  (require 'tabulated-list)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode)
      (setq mevedel-cockpit--context context)
      (when setup
        (funcall setup))
      (funcall refresh))
    (when-let* ((window (display-buffer buffer)))
      (select-window window))
    buffer))

(provide 'mevedel-cockpit)

;;; mevedel-cockpit.el ends here
