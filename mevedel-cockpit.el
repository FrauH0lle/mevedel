;;; mevedel-cockpit.el -- Shared cockpit surface helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared ownership and tabulated surface machinery for session-owned cockpit
;; buffers.  Resource modules provide domain-specific item collection,
;; rendering, and actions; this module owns the common surface lifecycle.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'tabulated-list)

;; `mevedel-menu'
(declare-function mevedel-menu "mevedel-menu" ())

;; `mevedel-report'
(declare-function mevedel-report-show "mevedel-report" (buffer report &optional noselect))

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

(defvar-local mevedel-cockpit--surface nil
  "Surface plist that describes the current tabulated cockpit.")

(defvar-local mevedel-cockpit--items nil
  "Items cached for the current tabulated cockpit render.")

(defvar-local mevedel-cockpit--row-items nil
  "Hash table mapping rendered row ids to cached items.")

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

(defun mevedel-cockpit-workspace-context (workspace &optional origin-buffer)
  "Return a cockpit context for WORKSPACE alone, without a session.
ORIGIN-BUFFER is where quitting returns.  Only surfaces that do not
require a session accept it."
  (list :workspace workspace :origin-buffer origin-buffer))

(defun mevedel-cockpit--live-context-p (context)
  "Return non-nil when CONTEXT's owners are live.
A session context needs its view and data buffers; a workspace context
from `mevedel-cockpit-workspace-context' owns nothing that can die."
  (and context
       (if (plist-member context :data-buffer)
           (and (mevedel-cockpit-context-view-buffer context)
                (mevedel-cockpit-context-data-buffer context))
         (plist-get context :workspace))))

(defun mevedel-cockpit-require-owner (&optional label context)
  "Signal a user error unless the current cockpit has live owners.
LABEL is a user-facing surface label used in the error message.
CONTEXT defaults to `mevedel-cockpit-current-context'."
  (let ((context (condition-case nil
                     (or context (mevedel-cockpit-current-context))
                   (user-error nil))))
    (unless (mevedel-cockpit--live-context-p context)
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
    (unless (mevedel-cockpit--live-context-p context)
      (user-error "No live mevedel session for this %s"
                  (or label "cockpit surface")))
    context))

(defun mevedel-cockpit-goto-id (id)
  "Move point to tabulated row ID, or to the first row when absent."
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

(defun mevedel-cockpit--current-surface ()
  "Return the current cockpit surface plist."
  (or mevedel-cockpit--surface
      (user-error "No cockpit surface here")))

(defun mevedel-cockpit--surface-label (&optional surface)
  "Return SURFACE's user-facing label."
  (or (plist-get (or surface (mevedel-cockpit--current-surface)) :label)
      "cockpit surface"))

(defun mevedel-cockpit-surface-context (&optional surface)
  "Return the checked cockpit context for SURFACE."
  (let* ((surface (or surface (mevedel-cockpit--current-surface)))
         (label (mevedel-cockpit--surface-label surface))
         (context (mevedel-cockpit-current-context)))
    (mevedel-cockpit-require-owner label context)
    (when (and (plist-get surface :require-session)
               (not (mevedel-cockpit-context-session context)))
      (user-error "No mevedel session in this buffer"))
    context))

(defun mevedel-cockpit-surface-items ()
  "Return the current cockpit surface's cached items."
  mevedel-cockpit--items)

(defun mevedel-cockpit--row-entry (surface item context)
  "Return ITEM's tabulated row for SURFACE and CONTEXT."
  (let ((entry-function (plist-get surface :entry)))
    (unless entry-function
      (error "Cockpit surface has no entry function"))
    (let ((entry (funcall entry-function item context)))
      (puthash (car entry) item mevedel-cockpit--row-items)
      entry)))

(defun mevedel-cockpit--fit-format (format entries padding width)
  "Return FORMAT with column widths fitted to ENTRIES within WIDTH.
FORMAT's widths are the widths a column keeps when space is tight.  A
column narrower than that shrinks to its widest cell or header, with room
for a sort indicator.  Space left over grows columns whose cells would
be truncated, while the last column, which takes whatever remains, keeps
up to a third of the line.  PADDING is `tabulated-list-padding'."
  (let* ((count (length format))
         (last (1- count))
         (natural
          (vconcat
           (cl-loop
            for n below count
            for column = (aref format n)
            collect
            (apply #'max (+ (string-width (car column))
                            (if (nth 2 column) 2 0))
                   (mapcar (lambda (entry)
                             (let ((cell (aref (cadr entry) n)))
                               (cond ((stringp cell) (string-width cell))
                                     ((eq (car-safe cell) 'image) 1)
                                     (t (string-width (car cell))))))
                           entries)))))
         (widths (vconcat (cl-loop for n below last
                                   collect (min (aref natural n)
                                                (nth 1 (aref format n))))))
         (slack (- width padding
                   (min (aref natural last) (/ (- width padding) 3))
                   (cl-loop for n below last
                            sum (+ (aref widths n)
                                   (or (plist-get (nthcdr 3 (aref format n))
                                                  :pad-right)
                                       1)))))
         (grew t))
    (while (and (> slack 0) grew)
      (setq grew nil)
      (dotimes (n last)
        (when (and (> slack 0) (< (aref widths n) (aref natural n)))
          (cl-incf (aref widths n))
          (cl-decf slack)
          (setq grew t))))
    (vconcat (cl-loop for n below count
                      for (name width sort . props) = (aref format n)
                      collect `(,name ,(if (< n last) (aref widths n) width)
                                      ,sort ,@props)))))

(defun mevedel-cockpit--fit-columns (window)
  "Fit the cockpit columns to WINDOW.  Return non-nil when they changed."
  (when (window-live-p window)
    (let ((format (mevedel-cockpit--fit-format
                   (plist-get mevedel-cockpit--surface :format)
                   tabulated-list-entries tabulated-list-padding
                   (window-max-chars-per-line window))))
      (unless (equal format tabulated-list-format)
        (setq tabulated-list-format format)
        (tabulated-list-init-header)
        t))))

(defun mevedel-cockpit--refit (window)
  "Reprint the cockpit when WINDOW's new size changes its columns."
  (when (mevedel-cockpit--fit-columns window)
    (tabulated-list-print t)))

(defun mevedel-cockpit-surface-refresh (&optional selected-id)
  "Refresh the current tabulated cockpit, preserving SELECTED-ID."
  (interactive)
  (let* ((surface (mevedel-cockpit--current-surface))
         (context (mevedel-cockpit-surface-context surface))
         (collect-function (plist-get surface :collect))
         (selected (or selected-id (tabulated-list-get-id))))
    (unless collect-function
      (error "Cockpit surface has no collect function"))
    (setq mevedel-cockpit--items (funcall collect-function context)
          mevedel-cockpit--row-items (make-hash-table :test #'equal)
          tabulated-list-entries
          (mapcar
           (lambda (item)
             (mevedel-cockpit--row-entry surface item context))
           mevedel-cockpit--items))
    (mevedel-cockpit--fit-columns (get-buffer-window nil t))
    (tabulated-list-print t)
    (unless (equal selected (tabulated-list-get-id))
      (mevedel-cockpit-goto-id selected))
    (force-mode-line-update t)
    mevedel-cockpit--items))

(defun mevedel-cockpit-surface-selected (&optional no-error)
  "Return the selected cockpit surface item.
When NO-ERROR is non-nil, return nil instead of signaling when no row
item is selected."
  (mevedel-cockpit-surface-context)
  (let* ((surface (mevedel-cockpit--current-surface))
         (row-label (or (plist-get surface :row-label) "item"))
         (id (tabulated-list-get-id))
         (sentinel (list nil))
         (item (and id
                    mevedel-cockpit--row-items
                    (gethash id mevedel-cockpit--row-items sentinel))))
    (cond
     ((and item (not (eq item sentinel))) item)
     (no-error nil)
     (t (user-error "No %s on this line" row-label)))))

(defun mevedel-cockpit-show-help (buffer report)
  "Show structured REPORT in information BUFFER."
  (require 'mevedel-report)
  (mevedel-report-show buffer report))

(defun mevedel-cockpit--generated-help-report (surface)
  "Return generated key help as a report for SURFACE."
  (list :title (format "mevedel %s" (mevedel-cockpit--surface-label surface))
        :sections (list (list :id 'keys :title "Keys"
                              :body (mevedel-cockpit-surface-key-help-text surface)))))

(defun mevedel-cockpit-surface-help ()
  "Open help for the current cockpit surface."
  (interactive)
  (let* ((surface (mevedel-cockpit--current-surface))
         (context (condition-case nil
                      (mevedel-cockpit-current-context)
                    (user-error nil)))
         (function (plist-get surface :help-function))
         (text (or (and function (funcall function context))
                   (plist-get surface :help-report)
                   (mevedel-cockpit--generated-help-report surface)))
         (buffer (or (plist-get surface :help-buffer)
                     "*mevedel cockpit help*")))
    (mevedel-cockpit-show-help buffer text)))

(defun mevedel-cockpit--detail-report (surface context id item)
  "Build ITEM's report for SURFACE, retaining its CONTEXT and row ID."
  (let* ((report (funcall (plist-get surface :details) item context))
         (check (plist-get report :validate)))
    (plist-put report :identity (list id context (plist-get report :identity)))
    (plist-put report :validate
               (lambda ()
                 (mevedel-cockpit-require-owner (mevedel-cockpit--surface-label surface) context)
                 (when check (funcall check))))
    (plist-put report :refresh
               (lambda ()
                 (mevedel-cockpit-require-owner (mevedel-cockpit--surface-label surface) context)
                 (let* ((items (funcall (plist-get surface :collect) context))
                        (fresh (seq-find
                                (lambda (row)
                                  (equal id (car (mevedel-cockpit--row-entry surface row context))))
                                items)))
                   (unless fresh (user-error "Inspected item is no longer available"))
                   (mevedel-cockpit--detail-report surface context id fresh))))
    report))

(defun mevedel-cockpit-surface-details ()
  "Open details for the selected cockpit surface item."
  (interactive)
  (let* ((surface (mevedel-cockpit--current-surface))
         (context (mevedel-cockpit-surface-context surface))
         (function (plist-get surface :details))
         (buffer (or (plist-get surface :details-buffer)
                     "*mevedel cockpit details*"))
         (item (mevedel-cockpit-surface-selected)))
    (unless function
      (user-error "No details for this %s"
                  (mevedel-cockpit--surface-label surface)))
    (mevedel-cockpit-show-help buffer
                               (mevedel-cockpit--detail-report surface context
                                                               (tabulated-list-get-id) item))))

(defun mevedel-cockpit-format-header (name scope state)
  "Return the shared cockpit header line for NAME, SCOPE, and STATE.
NAME identifies the surface, SCOPE the session or root it belongs to, and
STATE its counts and condition.  The trailing pointer replaces per-surface
key listings; the keys themselves belong in \\`?' help."
  (concat (propertize (format "mevedel: %s" name)
                      'face 'font-lock-function-name-face)
          (format "  %s  %s    " (or scope "") (or state ""))
          (propertize "? keys" 'face 'shadow)))

(defun mevedel-cockpit-surface-header-line ()
  "Return the header line for the current cockpit surface."
  (when-let* ((surface mevedel-cockpit--surface)
              (function (plist-get surface :header)))
    (let ((context (condition-case nil
                       (mevedel-cockpit-current-context)
                     (user-error nil))))
      (funcall function mevedel-cockpit--items context))))

(defun mevedel-cockpit--key-command (binding)
  "Return BINDING's command.
BINDING may be (KEY . COMMAND) or (KEY LABEL COMMAND)."
  (if (and (consp (cdr binding))
           (stringp (cadr binding)))
      (nth 2 binding)
    (cdr binding)))

(defun mevedel-cockpit--key-help-label (binding)
  "Return BINDING's help label, or nil."
  (and (consp (cdr binding))
       (stringp (cadr binding))
       (cadr binding)))

(defun mevedel-cockpit-surface-key-help-text (&optional surface)
  "Return formatted key help lines for SURFACE.
SURFACE defaults to the current cockpit surface.  Surface key entries without
a help label are still valid bindings, but are omitted from generated help."
  (let* ((surface (or surface (mevedel-cockpit--current-surface)))
         (row-label (or (plist-get surface :row-label) "item"))
         (keys (append
                (list
                 (list "RET" (format "Show selected %s details" row-label))
                 '("g" "Refresh table")
                 '("?" "Show this help")
                 '("q" "Back to the main session cockpit"))
                (plist-get surface :keys))))
    (mapconcat
     #'identity
     (delq nil
           (mapcar
            (lambda (binding)
              (when-let* ((label (mevedel-cockpit--key-help-label binding)))
                (format "%-4s %s" (car binding) label)))
            keys))
     "\n")))

(defun mevedel-cockpit--make-surface-keymap (keys)
  "Return a cockpit keymap with default commands and extra KEYS."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'mevedel-cockpit-surface-refresh)
    (define-key map (kbd "?") #'mevedel-cockpit-surface-help)
    (define-key map (kbd "q") #'mevedel-cockpit-surface-quit)
    (define-key map (kbd "RET") #'mevedel-cockpit-surface-details)
    (dolist (binding keys)
      (define-key map (kbd (car binding))
                  (mevedel-cockpit--key-command binding)))
    map))

(defun mevedel-cockpit-setup-tabulated-surface (surface)
  "Initialize the current tabulated buffer from SURFACE."
  (setq mevedel-cockpit--surface surface
        tabulated-list-format (plist-get surface :format)
        tabulated-list-padding (or (plist-get surface :padding) 2)
        tabulated-list-sort-key (plist-get surface :sort-key))
  (when (plist-get surface :header)
    (setq header-line-format '(:eval (mevedel-cockpit-surface-header-line))))
  (use-local-map
   (mevedel-cockpit--make-surface-keymap (plist-get surface :keys)))
  (tabulated-list-init-header)
  ;; Also runs when a window first shows the buffer, which is how a
  ;; surface rendered before display gets its first fit.
  (add-hook 'window-size-change-functions #'mevedel-cockpit--refit nil t)
  (hl-line-mode 1))

(defun mevedel-cockpit-quit (&optional label)
  "Kill the current cockpit and return to the main session cockpit.
LABEL is a user-facing surface label used in the dead-owner error."
  (interactive)
  (let* ((buffer (current-buffer))
         (session-p (plist-member mevedel-cockpit--context :data-buffer))
         (return-buffer (mevedel-cockpit--return-buffer)))
    (when return-buffer
      (when-let* ((window (display-buffer return-buffer)))
        (select-window window)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (cond
     ;; A workspace context has no session cockpit to return to.
     ((not session-p))
     ((not return-buffer)
      (user-error "No live mevedel session for this %s"
                  (or label "cockpit surface")))
     (t
      (with-current-buffer return-buffer
        (require 'mevedel-menu)
        (mevedel-menu))))))

(defun mevedel-cockpit-surface-quit ()
  "Quit the current cockpit surface."
  (interactive)
  (mevedel-cockpit-quit (mevedel-cockpit--surface-label)))

(defun mevedel-cockpit-open-surface (surface &optional context)
  "Open tabulated cockpit SURFACE for CONTEXT."
  (let ((label (mevedel-cockpit--surface-label surface)))
    (setq context (mevedel-cockpit--require-open-context context label))
    (when (and (plist-get surface :require-session)
               (not (mevedel-cockpit-context-session context)))
      (user-error "No mevedel session in this buffer")))
  ;; One buffer name per surface, so a second session would otherwise
  ;; take the first session's buffer by overwriting its context -- and
  ;; with it the only handle the surface had on what it still owes that
  ;; session.  Kill it instead, so whatever it owns is released by its
  ;; own teardown.
  (when-let* ((existing (get-buffer (plist-get surface :buffer-name)))
              (previous (buffer-local-value 'mevedel-cockpit--context
                                            existing))
              ((not (and (eq (mevedel-cockpit-context-session previous)
                             (mevedel-cockpit-context-session context))
                         (eq (mevedel-cockpit-context-workspace previous)
                             (mevedel-cockpit-context-workspace context))))))
    (kill-buffer existing))
  (let ((buffer (get-buffer-create (plist-get surface :buffer-name))))
    (with-current-buffer buffer
      (funcall (plist-get surface :mode))
      (setq mevedel-cockpit--surface surface)
      (setq mevedel-cockpit--context context)
      (when-let* ((setup (plist-get surface :setup)))
        (funcall setup context))
      (mevedel-cockpit-surface-refresh))
    (when-let* ((window (display-buffer buffer)))
      (select-window window))
    buffer))

(provide 'mevedel-cockpit)

;;; mevedel-cockpit.el ends here
