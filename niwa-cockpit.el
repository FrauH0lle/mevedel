;;; niwa-cockpit.el -- Shared cockpit surface helpers -*- lexical-binding: t -*-

;;; Commentary:

;; Shared ownership and tabulated surface machinery for session-owned cockpit
;; buffers.  Resource modules provide domain-specific item collection,
;; rendering, and actions; this module owns the common surface lifecycle.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'tabulated-list)
(require 'transient)
(require 'niwa-utilities)
(require 'niwa-window)

;; `niwa-report'
(declare-function niwa-report-show "niwa-report" (buffer report &optional noselect))

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa-composer--view)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-print "tabulated-list" (&optional remember-pos update))
(defvar tabulated-list-entries)

;; `transient'
(defvar transient--original-buffer)

(defvar-local niwa-cockpit--context nil
  "Context plist that owns the current cockpit surface.")

(defvar-local niwa-cockpit--surface nil
  "Surface plist that describes the current tabulated cockpit.")

(defvar-local niwa-cockpit--items nil
  "Items cached for the current tabulated cockpit render.")

(defvar-local niwa-cockpit--row-items nil
  "Hash table mapping rendered row ids to cached items.")

(defconst niwa-cockpit--menu-prefixes
  '(niwa-menu--top niwa-menu--navigate niwa-menu--mode
    niwa-menu--model-selection niwa-menu--goal niwa-menu--control
    niwa-menu--share niwa-menu--share-item niwa-menu--preset
    niwa-worktree-status)
  "Transient prefixes that can explicitly own a cockpit opening.")

(defun niwa-cockpit-menu-context ()
  "Return the explicit context of the invoking niwa menu, or nil."
  (when (memq transient-current-command niwa-cockpit--menu-prefixes)
    (plist-get (transient-scope) :niwa-context)))

(defun niwa-cockpit-menu-return-function (&optional context)
  "Return a menu continuation for CONTEXT, only when opened from a menu.
Without CONTEXT, use the invoking transient's explicit context."
  (let* ((context (or context (niwa-cockpit-menu-context)))
         (origin (plist-get context :origin-buffer))
         (return (plist-get context :return-menu)))
    (when (and return
               (memq transient-current-command niwa-cockpit--menu-prefixes))
      (lambda ()
        (when (and (buffer-live-p origin)
                   (niwa-cockpit--live-context-p context))
          (with-current-buffer origin
            (funcall return)))))))

(defun niwa-cockpit--buffer-session (buffer)
  "Return BUFFER's local session, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (and (boundp 'niwa--session) niwa--session))))

(defun niwa-cockpit--make-context (view-buffer data-buffer origin-buffer)
  "Return a cockpit context for VIEW-BUFFER, DATA-BUFFER, and ORIGIN-BUFFER."
  (let* ((session (niwa-cockpit--buffer-session data-buffer))
         (workspace (and session (niwa-session-workspace session))))
    (list :view-buffer view-buffer
          :data-buffer data-buffer
          :origin-buffer origin-buffer
          :session session
          :workspace workspace)))

(defun niwa-cockpit-context-for-buffer (buffer)
  "Return BUFFER's cockpit context, or nil."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cond
       (niwa-cockpit--context
        niwa-cockpit--context)
       ((and (bound-and-true-p niwa-composer--view)
             (buffer-live-p niwa-composer--view)
             (buffer-live-p niwa--data-buffer))
        (niwa-cockpit--make-context
         niwa-composer--view niwa--data-buffer buffer))
       ((and (derived-mode-p 'niwa-view-mode)
             (boundp 'niwa--data-buffer)
             (buffer-live-p niwa--data-buffer))
        (niwa-cockpit--make-context buffer niwa--data-buffer buffer))
       ((and (boundp 'niwa--view-buffer)
             (buffer-live-p niwa--view-buffer)
             (with-current-buffer niwa--view-buffer
               (and (derived-mode-p 'niwa-view-mode)
                    (boundp 'niwa--data-buffer)
                    (eq niwa--data-buffer buffer))))
        (niwa-cockpit--make-context niwa--view-buffer buffer buffer))))))

(defun niwa-cockpit-diagnostic-scope (&optional context)
  "Return diagnostic scope from explicit CONTEXT or the invoking command.
Read retained ownership only; never discover a workspace or require live owners."
  (let ((context (or context (niwa-cockpit-menu-context)
                     (niwa-cockpit-context-for-buffer (current-buffer))
                     (and (boundp 'transient--original-buffer)
                          (niwa-cockpit-context-for-buffer transient--original-buffer)))))
    (niwa-diagnostic-scope
     (if context
         (or (plist-get context :workspace) (plist-get context :session)
             'unavailable)
       (current-buffer)))))

(defun niwa-cockpit-current-context ()
  "Return the cockpit context for the current command."
  (or (niwa-cockpit-menu-context)
      (niwa-cockpit-context-for-buffer (current-buffer))
      (and (boundp 'transient--original-buffer)
           (buffer-live-p transient--original-buffer)
           (niwa-cockpit-context-for-buffer transient--original-buffer))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "No niwa session cockpit here")))

(defun niwa-cockpit-context-view-buffer (&optional context)
  "Return CONTEXT's live view buffer, or nil."
  (let ((buffer (plist-get (or context (niwa-cockpit-current-context))
                           :view-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun niwa-cockpit-context-data-buffer (&optional context)
  "Return CONTEXT's live data buffer, or nil."
  (let ((buffer (plist-get (or context (niwa-cockpit-current-context))
                           :data-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun niwa-cockpit-context-origin-buffer (&optional context)
  "Return CONTEXT's live origin buffer, or nil."
  (let ((buffer (plist-get (or context (niwa-cockpit-current-context))
                           :origin-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun niwa-cockpit-context-session (&optional context)
  "Return CONTEXT's session, or nil."
  (let ((context (or context (niwa-cockpit-current-context))))
    (or (plist-get context :session)
        (niwa-cockpit--buffer-session
         (niwa-cockpit-context-data-buffer context)))))

(defun niwa-cockpit-context-workspace (&optional context)
  "Return CONTEXT's workspace, or nil."
  (let ((context (or context (niwa-cockpit-current-context))))
    (or (plist-get context :workspace)
        (when-let* ((session (niwa-cockpit-context-session context)))
          (niwa-session-workspace session)))))

(defun niwa-cockpit-call-in-view (context function &rest args)
  "Call FUNCTION with ARGS in CONTEXT's view buffer."
  (if-let* ((buffer (niwa-cockpit-context-view-buffer context)))
      (with-current-buffer buffer
        (apply function args))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                 "No live niwa session for this cockpit surface")))

(defun niwa-cockpit-call-in-data (context function &rest args)
  "Call FUNCTION with ARGS in CONTEXT's data buffer."
  (if-let* ((buffer (niwa-cockpit-context-data-buffer context)))
      (with-current-buffer buffer
        (apply function args))
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                 "No live niwa session for this cockpit surface")))

(defun niwa-cockpit-data-buffer ()
  "Return the live data buffer that owns the current cockpit, or nil."
  (when-let* ((context (condition-case nil
                           (niwa-cockpit-current-context)
                         (user-error nil))))
    (niwa-cockpit-context-data-buffer context)))

(defun niwa-cockpit-workspace-context (workspace &optional origin-buffer)
  "Return a cockpit context for WORKSPACE alone, without a session.
ORIGIN-BUFFER is where quitting returns.  Only surfaces that do not
require a session accept it."
  (list :workspace workspace :origin-buffer origin-buffer))

(defun niwa-cockpit--live-context-p (context)
  "Return non-nil when CONTEXT's owners are live.
A session context needs its view and data buffers; a workspace context
from `niwa-cockpit-workspace-context' owns nothing that can die."
  (and context
       (if (plist-member context :data-buffer)
           (and (niwa-cockpit-context-view-buffer context)
                (niwa-cockpit-context-data-buffer context))
         (plist-get context :workspace))))

(defun niwa-cockpit-require-owner (&optional label context)
  "Signal a user error unless the current cockpit has live owners.
LABEL is a user-facing surface label used in the error message.
CONTEXT defaults to `niwa-cockpit-current-context'."
  (let ((context (condition-case nil
                     (or context (niwa-cockpit-current-context))
                   (user-error nil))))
    (unless (niwa-cockpit--live-context-p context)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                  "No live niwa session for this %s"
                  (or label "cockpit surface"))))
  t)

(defun niwa-cockpit--require-open-context (context label)
  "Return CONTEXT when it has live view and data buffers.
LABEL is used in the owner error."
  (let ((context (or context (niwa-cockpit-current-context))))
    (unless (niwa-cockpit--live-context-p context)
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                  "No live niwa session for this %s"
                  (or label "cockpit surface")))
    context))

(defun niwa-cockpit-goto-id (id)
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

(defun niwa-cockpit--current-surface ()
  "Return the current cockpit surface plist."
  (or niwa-cockpit--surface
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                   "No cockpit surface here")))

(defun niwa-cockpit--surface-label (&optional surface)
  "Return SURFACE's user-facing label."
  (or (plist-get (or surface (niwa-cockpit--current-surface)) :label)
      "cockpit surface"))

(defun niwa-cockpit-surface-context (&optional surface)
  "Return the checked cockpit context for SURFACE."
  (let* ((surface (or surface (niwa-cockpit--current-surface)))
         (label (niwa-cockpit--surface-label surface))
         (context (niwa-cockpit-current-context)))
    (niwa-cockpit-require-owner label context)
    (when (and (plist-get surface :require-session)
               (not (niwa-cockpit-context-session context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa session in this buffer"))
    context))

(defun niwa-cockpit-surface-items ()
  "Return the current cockpit surface's cached items."
  niwa-cockpit--items)

(defun niwa-cockpit--row-entry (surface item context)
  "Return ITEM's tabulated row for SURFACE and CONTEXT."
  (let ((entry-function (plist-get surface :entry)))
    (unless entry-function
      (error "Cockpit surface has no entry function"))
    (let ((entry (funcall entry-function item context)))
      (puthash (car entry) item niwa-cockpit--row-items)
      entry)))

(defun niwa-cockpit--fit-format (format entries padding width)
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

(defun niwa-cockpit--fit-columns (window)
  "Fit the cockpit columns to WINDOW.  Return non-nil when they changed."
  (when (window-live-p window)
    (let ((format (niwa-cockpit--fit-format
                   (plist-get niwa-cockpit--surface :format)
                   tabulated-list-entries tabulated-list-padding
                   (window-max-chars-per-line window))))
      (unless (equal format tabulated-list-format)
        (setq tabulated-list-format format)
        (tabulated-list-init-header)
        t))))

(defun niwa-cockpit--refit (window)
  "Reprint the cockpit when WINDOW's new size changes its columns."
  (when (niwa-cockpit--fit-columns window)
    (tabulated-list-print t)))

(defun niwa-cockpit-surface-refresh (&optional selected-id)
  "Refresh the current tabulated cockpit, preserving SELECTED-ID."
  (interactive)
  (let* ((surface (niwa-cockpit--current-surface))
         (context (niwa-cockpit-surface-context surface))
         (collect-function (plist-get surface :collect))
         (selected (or selected-id (tabulated-list-get-id))))
    (unless collect-function
      (error "Cockpit surface has no collect function"))
    (setq niwa-cockpit--items (funcall collect-function context)
          niwa-cockpit--row-items (make-hash-table :test #'equal)
          tabulated-list-entries
          (mapcar
           (lambda (item)
             (niwa-cockpit--row-entry surface item context))
           niwa-cockpit--items))
    (niwa-cockpit--fit-columns (get-buffer-window nil t))
    (tabulated-list-print t)
    (unless (equal selected (tabulated-list-get-id))
      (niwa-cockpit-goto-id selected))
    (force-mode-line-update t)
    niwa-cockpit--items))

(defun niwa-cockpit-surface-selected (&optional no-error)
  "Return the selected cockpit surface item.
When NO-ERROR is non-nil, return nil instead of signaling when no row
item is selected."
  (niwa-cockpit-surface-context)
  (let* ((surface (niwa-cockpit--current-surface))
         (row-label (or (plist-get surface :row-label) "item"))
         (id (tabulated-list-get-id))
         (sentinel (list nil))
         (item (and id
                    niwa-cockpit--row-items
                    (gethash id niwa-cockpit--row-items sentinel))))
    (cond
     ((and item (not (eq item sentinel))) item)
     (no-error nil)
     (t (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                     "No %s on this line" row-label)))))

(defun niwa-cockpit-show-help (buffer report)
  "Show structured REPORT in information BUFFER."
  (require 'niwa-report)
  (niwa-report-show buffer report))

(defun niwa-cockpit--generated-help-report (surface)
  "Return generated key help as a report for SURFACE."
  (list :title (format "niwa %s" (niwa-cockpit--surface-label surface))
        :sections (list (list :id 'keys :title "Keys"
                              :body (niwa-cockpit-surface-key-help-text surface)))))

(defun niwa-cockpit-surface-help ()
  "Open help for the current cockpit surface."
  (interactive)
  (let* ((surface (niwa-cockpit--current-surface))
         (context (condition-case nil
                      (niwa-cockpit-current-context)
                    (user-error nil)))
         (function (plist-get surface :help-function))
         (text (or (and function (funcall function context))
                   (plist-get surface :help-report)
                   (niwa-cockpit--generated-help-report surface)))
         (buffer (or (plist-get surface :help-buffer)
                     "*niwa cockpit help*")))
    (niwa-cockpit-show-help buffer text)))

(defun niwa-cockpit--detail-report (surface context id item)
  "Build ITEM's report for SURFACE, retaining its CONTEXT and row ID."
  (let* ((report (funcall (plist-get surface :details) item context))
         (check (plist-get report :validate)))
    (plist-put report :identity (list id context (plist-get report :identity)))
    (plist-put report :validate
               (lambda ()
                 (niwa-cockpit-require-owner (niwa-cockpit--surface-label surface) context)
                 (when check (funcall check))))
    (plist-put report :refresh
               (lambda ()
                 (niwa-cockpit-require-owner (niwa-cockpit--surface-label surface) context)
                 (let* ((items (funcall (plist-get surface :collect) context))
                        (fresh (seq-find
                                (lambda (row)
                                  (equal id (car (niwa-cockpit--row-entry surface row context))))
                                items)))
                   (unless fresh
                     (niwa-diagnostic-user-error
                      (niwa-cockpit-diagnostic-scope context)
                      "Inspected item is no longer available"))
                   (niwa-cockpit--detail-report surface context id fresh))))
    report))

(defun niwa-cockpit-surface-details ()
  "Open details for the selected cockpit surface item."
  (interactive)
  (let* ((surface (niwa-cockpit--current-surface))
         (context (niwa-cockpit-surface-context surface))
         (function (plist-get surface :details))
         (buffer (or (plist-get surface :details-buffer)
                     "*niwa cockpit details*"))
         (item (niwa-cockpit-surface-selected)))
    (unless function
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                  "No details for this %s"
                  (niwa-cockpit--surface-label surface)))
    (niwa-cockpit-show-help buffer
                               (niwa-cockpit--detail-report surface context
                                                               (tabulated-list-get-id) item))))

(defun niwa-cockpit-number-cell (text number)
  "Return cell TEXT that `niwa-cockpit-sort-by-number' orders by NUMBER."
  (propertize text 'niwa-cockpit-number number))

(defun niwa-cockpit-sort-by-number (column)
  "Return a predicate ordering rows by the numbers behind COLUMN's cells.
Tabulated lists compare displayed text, which sorts 1m 05s before 9s,
10 before 9 and 1.5M before 200k."
  (lambda (a b)
    (< (get-text-property 0 'niwa-cockpit-number (aref (cadr a) column))
       (get-text-property 0 'niwa-cockpit-number (aref (cadr b) column)))))

(defun niwa-cockpit-format-header (name scope state)
  "Return the shared cockpit header line for NAME, SCOPE, and STATE.
NAME identifies the surface, SCOPE the session or root it belongs to, and
STATE its counts and condition.  The trailing pointer replaces per-surface
key listings; the keys themselves belong in \\`?' help."
  (concat (propertize (format "niwa: %s" name)
                      'face 'font-lock-function-name-face)
          (format "  %s  %s    " (or scope "") (or state ""))
          (propertize "? keys" 'face 'shadow)))

(defun niwa-cockpit-surface-header-line ()
  "Return the header line for the current cockpit surface."
  (when-let* ((surface niwa-cockpit--surface)
              (function (plist-get surface :header)))
    (let ((context (condition-case nil
                       (niwa-cockpit-current-context)
                     (user-error nil))))
      (funcall function niwa-cockpit--items context))))

(defun niwa-cockpit--key-command (binding)
  "Return BINDING's command.
BINDING may be (KEY . COMMAND) or (KEY LABEL COMMAND)."
  (if (and (consp (cdr binding))
           (stringp (cadr binding)))
      (nth 2 binding)
    (cdr binding)))

(defun niwa-cockpit--key-help-label (binding)
  "Return BINDING's help label, or nil."
  (and (consp (cdr binding))
       (stringp (cadr binding))
       (cadr binding)))

(defun niwa-cockpit-surface-key-help-text (&optional surface)
  "Return formatted key help lines for SURFACE.
SURFACE defaults to the current cockpit surface.  Surface key entries without
a help label are still valid bindings, but are omitted from generated help."
  (let* ((surface (or surface (niwa-cockpit--current-surface)))
         (row-label (or (plist-get surface :row-label) "item"))
         (keys (append
                (list
                 (list "RET" (format "Show selected %s details" row-label))
                 '("g" "Refresh table")
                 '("?" "Show this help")
                 '("q" "Close and return to the previous layout"))
                (plist-get surface :keys))))
    (mapconcat
     #'identity
     (delq nil
           (mapcar
            (lambda (binding)
              (when-let* ((label (niwa-cockpit--key-help-label binding)))
                (format "%-4s %s" (car binding) label)))
            keys))
     "\n")))

(defun niwa-cockpit--make-surface-keymap (keys)
  "Return a cockpit keymap with default commands and extra KEYS."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'niwa-cockpit-surface-refresh)
    (define-key map (kbd "?") #'niwa-cockpit-surface-help)
    (define-key map (kbd "q") #'niwa-cockpit-surface-quit)
    (define-key map (kbd "RET") #'niwa-cockpit-surface-details)
    (dolist (binding keys)
      (define-key map (kbd (car binding))
                  (niwa-cockpit--key-command binding)))
    map))

(defun niwa-cockpit-setup-tabulated-surface (surface)
  "Initialize the current tabulated buffer from SURFACE."
  (setq niwa-cockpit--surface surface
        tabulated-list-format (plist-get surface :format)
        tabulated-list-padding (or (plist-get surface :padding) 2)
        tabulated-list-sort-key (plist-get surface :sort-key))
  (when (plist-get surface :header)
    (setq header-line-format '(:eval (niwa-cockpit-surface-header-line))))
  (use-local-map
   (niwa-cockpit--make-surface-keymap (plist-get surface :keys)))
  (tabulated-list-init-header)
  ;; Also runs when a window first shows the buffer, which is how a
  ;; surface rendered before display gets its first fit.
  (add-hook 'window-size-change-functions #'niwa-cockpit--refit nil t)
  (hl-line-mode 1))

(defun niwa-cockpit-quit (&optional label)
  "Close the current cockpit and restore its invoking window layout.
LABEL identifies the surface.  Return to the menu only for menu entry."
  (interactive)
  (ignore label)
  (niwa-window-quit))

(defun niwa-cockpit-surface-quit ()
  "Quit the current cockpit surface."
  (interactive)
  (niwa-cockpit-quit (niwa-cockpit--surface-label)))

(defun niwa-cockpit-open-surface (surface &optional context)
  "Open tabulated cockpit SURFACE for CONTEXT."
  (let ((label (niwa-cockpit--surface-label surface)))
    (setq context (niwa-cockpit--require-open-context context label))
    (when (and (plist-get surface :require-session)
               (not (niwa-cockpit-context-session context)))
      (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope context)
                                   "No niwa session in this buffer")))
  ;; One buffer name per surface, so a second session would otherwise
  ;; take the first session's buffer by overwriting its context -- and
  ;; with it the only handle the surface had on what it still owes that
  ;; session.  Kill it instead, so whatever it owns is released by its
  ;; own teardown.
  (when-let* ((existing (get-buffer (plist-get surface :buffer-name)))
              (previous (buffer-local-value 'niwa-cockpit--context
                                            existing))
              ((not (and (eq (niwa-cockpit-context-session previous)
                             (niwa-cockpit-context-session context))
                         (eq (niwa-cockpit-context-workspace previous)
                             (niwa-cockpit-context-workspace context))))))
    (kill-buffer existing))
  (let ((buffer (get-buffer-create (plist-get surface :buffer-name))))
    (with-current-buffer buffer
      (funcall (plist-get surface :mode))
      (setq niwa-cockpit--surface surface)
      (setq niwa-cockpit--context context)
      (when-let* ((setup (plist-get surface :setup)))
        (funcall setup context))
      (niwa-cockpit-surface-refresh))
    (niwa-window-display-buffer
     buffer nil (niwa-cockpit-menu-return-function context))
    buffer))

(provide 'niwa-cockpit)

;;; niwa-cockpit.el ends here
