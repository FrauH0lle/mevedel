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

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-print "tabulated-list" (&optional remember-pos update))
(defvar tabulated-list-entries)

(defvar-local mevedel-cockpit--view-buffer nil
  "View buffer that owns the current cockpit surface.")

(defvar-local mevedel-cockpit--data-buffer nil
  "Data buffer that owns the current cockpit surface.")

(defvar-local mevedel-cockpit--origin-buffer nil
  "Buffer that launched the current cockpit surface.")

(defun mevedel-cockpit-data-buffer ()
  "Return the live data buffer that owns the current cockpit, or nil."
  (and (buffer-live-p mevedel-cockpit--data-buffer)
       mevedel-cockpit--data-buffer))

(defun mevedel-cockpit-require-owner (&optional label)
  "Signal a user error unless the current cockpit has live owners.
LABEL is a user-facing surface label used in the error message."
  (unless (and (buffer-live-p mevedel-cockpit--view-buffer)
               (buffer-live-p mevedel-cockpit--data-buffer))
    (user-error "No live mevedel session for this %s"
                (or label "cockpit surface")))
  t)

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

(defun mevedel-cockpit--return-buffer ()
  "Return the best live owner buffer for quitting this cockpit."
  (or (and (buffer-live-p mevedel-cockpit--origin-buffer)
           mevedel-cockpit--origin-buffer)
      (and (buffer-live-p mevedel-cockpit--view-buffer)
           mevedel-cockpit--view-buffer)
      (and (buffer-live-p mevedel-cockpit--data-buffer)
           mevedel-cockpit--data-buffer)))

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
    (buffer-name mode refresh view-buffer data-buffer origin-buffer
                 &optional setup label)
  "Open a session-owned tabulated cockpit buffer.
BUFFER-NAME is created or reused, MODE initializes the buffer, REFRESH
renders rows, and SETUP runs after owner buffers are recorded.  VIEW-BUFFER,
DATA-BUFFER, and ORIGIN-BUFFER record the owning session pair.  LABEL is a
user-facing surface label used in owner errors."
  (unless (and (buffer-live-p view-buffer)
               (buffer-live-p data-buffer))
    (user-error "No live mevedel session for this %s"
                (or label "cockpit surface")))
  (require 'tabulated-list)
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode)
      (setq mevedel-cockpit--view-buffer view-buffer
            mevedel-cockpit--data-buffer data-buffer
            mevedel-cockpit--origin-buffer origin-buffer)
      (when setup
        (funcall setup))
      (funcall refresh))
    (when-let* ((window (display-buffer buffer)))
      (select-window window))
    buffer))

(provide 'mevedel-cockpit)

;;; mevedel-cockpit.el ends here
