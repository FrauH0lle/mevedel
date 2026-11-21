;;; niwa-report.el --- Read-only information panels -*- lexical-binding: t -*-

;;; Commentary:

;; Display domain-owned reports as named sections.  Owners supply exact content
;; and section identities; this module owns only presentation and navigation.
;; Reports are plists with :title, :subtitle and :sections.  Each section has
;; :id, :title and :body, with optional :mode and :folded presentation hints.

;;; Code:

(require 'button)
(require 'niwa-cockpit)
(require 'seq)
(require 'niwa-view-fontify)
(require 'outline)

(defvar-local niwa-report--report nil "Report currently being inspected.")
(defvar-local niwa-report--folds nil "Section fold choices in this report.")
(defvar-local niwa-report--selected nil "Selected section identity.")
(defvar-local niwa-report--body-start 1 "Start of the currently displayed source body.")
(defvar-local niwa-report--positions nil "Reading positions by section identity.")
(defvar-local niwa-report--origin nil "Buffer from which this panel was opened.")
(defvar-local niwa-report--identity nil "Owner and item identity of this panel.")
(defvar-local niwa-report--index nil "Section index buffer owned by this panel.")
(defvar-local niwa-report--unfollow nil "Detach this inspector from its live source.")
(defvar-local niwa-report--parent nil "Reading buffer owning this section index.")
(defvar-local niwa-report--scope 'unavailable "Retained diagnostic ownership.")

(defun niwa-report-fields (&rest rows)
  "Format ROWS of (LABEL VALUE) as aligned, visually wrapped information."
  (mapconcat
   (lambda (row)
     (if (null row) ""
       (concat (propertize (format "%-16s " (car row)) 'face 'shadow)
               (propertize (format "%s" (cadr row))
                           'wrap-prefix (make-string 17 ?\s))
               "\n")))
   rows ""))

(defvar-keymap niwa-report-mode-map
  :parent special-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "n" #'niwa-report-next
  "p" #'niwa-report-previous
  "g" #'niwa-report-refresh
  "q" #'niwa-report-quit
  "RET" #'push-button)

(define-derived-mode niwa-report-mode special-mode "MevInfo"
  "Read an information panel; RET toggles the section at point."
  (setq-local truncate-lines nil
              truncate-partial-width-windows nil
              word-wrap t
              buffer-invisibility-spec '(t)
              header-line-format "RET toggle · TAB next · S-TAB previous · g refresh · q close"))

(defun niwa-report--owner ()
  "Return the reading buffer for this panel or section index."
  (unless (derived-mode-p 'niwa-report-mode)
    (niwa-diagnostic-user-error (niwa-cockpit-diagnostic-scope)
                                 "No information panel in this buffer"))
  (if (buffer-live-p niwa-report--parent)
      niwa-report--parent
    (current-buffer)))

(defun niwa-report-next (&optional previous)
  "Move to the next section, or the previous one when PREVIOUS is non-nil."
  (interactive)
  (let ((owner (niwa-report--owner)))
    (if (with-current-buffer owner
          (plist-get niwa-report--report :navigator))
        (with-current-buffer owner
          (let* ((ids (mapcar (lambda (section) (plist-get section :id))
                              (plist-get niwa-report--report :sections)))
                 (offset (seq-position ids niwa-report--selected #'equal)))
            (when ids
              (niwa-report-select-section
               (nth (mod (+ (or offset 0) (if previous -1 1)) (length ids)) ids)))))
      (if previous (backward-button 1 t) (forward-button 1 t)))))

(defun niwa-report-previous ()
  "Move to the previous section."
  (interactive)
  (niwa-report-next t))

(defun niwa-report--unavailable (error-data)
  "Clear disclosed content after ERROR-DATA invalidates this inspector."
  (setq niwa-report--report
        (list :title "Report unavailable" :sections nil
              :refresh (plist-get niwa-report--report :refresh)))
  (niwa-report--kill-index)
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (insert (propertize "* Report unavailable\n\n" 'face 'error)
            (niwa-diagnostic-format niwa-report--scope "%s"
                                       (error-message-string error-data)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun niwa-report-select-section (id)
  "Read the section identified by ID without changing its owner."
  (with-current-buffer (niwa-report--owner)
    (when-let* ((check (plist-get niwa-report--report :validate)))
      (condition-case err (funcall check)
        (error
         (niwa-report--unavailable err)
         (signal (car err) (cdr err)))))
    (unless (seq-find (lambda (section) (equal id (plist-get section :id)))
                      (plist-get niwa-report--report :sections))
      (niwa-diagnostic-user-error niwa-report--scope
                                   "Section is no longer available"))
    (niwa-report--save-position)
    (setq niwa-report--selected id)
    (niwa-report--render)
    (niwa-report--render-index)))

(defun niwa-report-refresh ()
  "Refresh this report through its original owner without moving focus."
  (interactive)
  (with-current-buffer (niwa-report--owner)
    (let ((origin niwa-report--origin)
          (refresh (plist-get niwa-report--report :refresh)))
      (unless (and refresh (buffer-live-p origin))
        (niwa-diagnostic-user-error niwa-report--scope
                                     "No live source to refresh this report"))
      (condition-case err
          (niwa-report-render (with-current-buffer origin (funcall refresh)) origin)
        (error (niwa-report--unavailable err) (signal (car err) (cdr err)))))))

(defun niwa-report--kill-index ()
  "Release the index buffer and windows owned by this reading buffer."
  (when (buffer-live-p niwa-report--index)
    (let ((index niwa-report--index))
      (setq niwa-report--index nil)
      (dolist (window (get-buffer-window-list index nil t))
        (when (window-parameter window 'niwa-report-index)
          (unless (one-window-p t (window-frame window))
            (delete-window window))))
      (kill-buffer index))))

(defun niwa-report-quit ()
  "Close this inspector and its section index, returning to its owner."
  (interactive)
  (let ((buffer (niwa-report--owner)))
    (with-current-buffer buffer (niwa-report--kill-index))
    (niwa-window-quit buffer)))

(defun niwa-report--toggle (button)
  "Toggle the section identified by BUTTON."
  (let* ((inhibit-read-only t)
         (id (button-get button 'niwa-report-section))
         (overlay (button-get button 'niwa-report-overlay))
         (folded (not (overlay-get overlay 'invisible))))
    (puthash id folded niwa-report--folds)
    (overlay-put overlay 'invisible folded)
    (button-put button 'display
                (concat (button-label button) (if folded "  ▸" "  ▾")))))

(defun niwa-report--fontify (text mode)
  "Fontify exact TEXT in MODE for a read-only panel without font lock."
  (let ((text (copy-sequence (niwa-view--fontify-as text mode)))
        (position 0))
    (while (< position (length text))
      (let ((end (next-single-property-change position 'font-lock-face text (length text))))
        (when-let* ((face (get-text-property position 'font-lock-face text)))
          (put-text-property position end 'face face text))
        (setq position end)))
    text))

(defun niwa-report--insert-section (section)
  "Insert SECTION with its explicit heading and exact body."
  (let* ((id (plist-get section :id))
         (folded (gethash id niwa-report--folds
                          (plist-get section :folded)))
         (heading (point))
         (button (insert-text-button
                  (concat "** " (plist-get section :title))
                  'face 'outline-2 'follow-link t
                  'niwa-report-section id
                  'action #'niwa-report--toggle))
         (start (point)))
    (insert "\n\n"
            (niwa-report--fontify
             (or (plist-get section :body) "") (plist-get section :mode)))
    (unless (bolp) (insert "\n"))
    (let ((overlay (make-overlay start (point) nil t nil)))
      (overlay-put overlay 'niwa-report-section id)
      (overlay-put overlay 'invisible folded)
      (overlay-put overlay 'isearch-open-invisible
                   (lambda (hidden)
                     (when (overlay-get hidden 'invisible)
                       (niwa-report--toggle (button-at (1- (overlay-start hidden)))))))
      (button-put button 'niwa-report-overlay overlay)
      (button-put button 'display
                  (concat (button-label button) (if folded "  ▸" "  ▾"))))
    (add-text-properties heading (point) (list 'niwa-report-section id))
    (insert "\n")))

(defun niwa-report--save-position ()
  "Retain source-relative point and scroll position for the selected section."
  (when niwa-report--positions
    (puthash niwa-report--selected
             (cons (- (point) niwa-report--body-start)
                   (when-let* ((window (get-buffer-window (current-buffer))))
                     (- (window-start window) niwa-report--body-start)))
             niwa-report--positions)))

(defun niwa-report--render ()
  "Render the current report without changing its source strings."
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (setq niwa-report--body-start 1)
    (insert (propertize (concat "* " (plist-get niwa-report--report :title))
                        'face 'outline-1)
            "\n")
    (when-let* ((subtitle (plist-get niwa-report--report :subtitle)))
      (insert (propertize subtitle 'face 'shadow) "\n"))
    (insert "\n")
    (dolist (section (plist-get niwa-report--report :sections))
      (if (plist-get niwa-report--report :navigator)
          (when (equal (plist-get section :id) niwa-report--selected)
            (insert (propertize (concat "** " (plist-get section :title))
                                'face 'outline-2) "\n\n")
            (setq niwa-report--body-start (point))
            (insert (niwa-report--fontify (or (plist-get section :body) "")
                                             (plist-get section :mode))))
        (niwa-report--insert-section section)))
    (let ((position (gethash niwa-report--selected niwa-report--positions)))
      (goto-char (if position
                     (max (point-min) (min (point-max) (+ niwa-report--body-start (car position))))
                   (point-min)))
      (when-let* ((start (cdr position))
                  (window (get-buffer-window (current-buffer))))
        (set-window-start window (max (point-min) (min (point-max) (+ niwa-report--body-start start))) t)))
    (set-buffer-modified-p nil)))

(defun niwa-report--render-index ()
  "Render the section index of the current reading buffer."
  (when (buffer-live-p niwa-report--index)
    (let ((owner (current-buffer))
          (sections (plist-get niwa-report--report :sections))
          (selected niwa-report--selected)
          selected-position)
      (with-current-buffer niwa-report--index
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Sections\n\n" 'face 'outline-1))
          (dolist (section sections)
            (let ((id (plist-get section :id)))
              (when (equal id selected) (setq selected-position (point)))
              (insert-text-button
               (plist-get section :title)
               'face (if (equal id selected) 'highlight 'link)
               'follow-link t
               'action (lambda (_)
                         (with-current-buffer owner
                           (niwa-report-select-section id))))
              (insert "\n\n")))
          (goto-char (or selected-position (point-min)))
          (when-let* ((window (get-buffer-window (current-buffer))))
            (set-window-point window (point)))
          (set-buffer-modified-p nil))))))

(defun niwa-report--display-index (window)
  "Display this panel's section index beside or above reading WINDOW."
  (when (plist-get niwa-report--report :navigator)
    (unless (buffer-live-p niwa-report--index)
      (let ((owner (current-buffer)))
        (setq niwa-report--index
              (generate-new-buffer (concat (buffer-name) " sections")))
        (with-current-buffer niwa-report--index
          (niwa-report-mode)
          (setq niwa-report--parent owner
                header-line-format "RET read · n/p section · q close"))))
    (unless (get-buffer-window niwa-report--index)
      (let ((index-window
             (condition-case nil
                 (if (>= (window-total-width window) 100)
                     (split-window window -32 'left)
                   (split-window window -7 'above))
               (error nil))))
        (when index-window
          (set-window-buffer index-window niwa-report--index)
          (set-window-parameter index-window 'niwa-report-index (current-buffer)))))
    (add-hook 'window-size-change-functions #'niwa-report--resize nil t)
    (setq header-line-format "n/p section · g refresh · q close")
    (niwa-report--render-index)))

(defun niwa-report--resize (window)
  "Adapt the section index when reading WINDOW changes size."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (plist-get niwa-report--report :navigator))
    (let ((index (and (buffer-live-p niwa-report--index)
                      (get-buffer-window niwa-report--index (window-frame window)))))
      (when (and (window-live-p index)
                 (eq (window-parent index) (window-parent window)))
        (let* ((side (= (cadr (window-edges index)) (cadr (window-edges window))))
               (width (if side (+ (window-total-width index) (window-total-width window))
                        (window-total-width window))))
          (unless (eq side (>= width 100))
            (let ((selected (eq (selected-window) index)))
              (delete-window index)
              (niwa-report--display-index window)
              (when selected
                (when-let* ((new (get-buffer-window niwa-report--index)))
                  (select-window new))))))))))

(defun niwa-report-follow-source (source report-function)
  "Follow SOURCE changes in this inspector using REPORT-FUNCTION.
Coalesce source events; closing the inspector leaves SOURCE untouched."
  (when niwa-report--unfollow (funcall niwa-report--unfollow))
  (let ((target (current-buffer)) timer change closed cleanup refresh)
    (setq refresh (lambda () (plist-put (funcall report-function) :refresh refresh))
          change
          (lambda (&rest _)
            (unless timer
              (setq timer
                    (run-at-time
                     0.1 nil
                     (lambda ()
                       (setq timer nil)
                       (when (and (buffer-live-p target) (buffer-live-p source))
                         (with-current-buffer target
                           (condition-case err
                               (niwa-report-render (funcall refresh))
                             (error
                              (niwa-report--unavailable err))))))))))
          closed
          (lambda ()
            (when (buffer-live-p target)
              (with-current-buffer target
                (niwa-report-render
                 (list :title "Information panel" :identity source
                       :sections (list (list :id 'closed :title "Source closed"
                                             :body "The source buffer has closed. Reopen this report from its owner to inspect a current source.")))))))
          cleanup
          (lambda ()
            (when timer (cancel-timer timer) (setq timer nil))
            (when (buffer-live-p target)
              (with-current-buffer target
                (remove-hook 'kill-buffer-hook cleanup t)))
            (when (buffer-live-p source)
              (with-current-buffer source
                (remove-hook 'after-change-functions change t)
                (remove-hook 'kill-buffer-hook closed t)))))
    (setq niwa-report--unfollow cleanup
          niwa-report--report (plist-put niwa-report--report :refresh refresh))
    (with-current-buffer source
      (add-hook 'after-change-functions change nil t)
      (add-hook 'kill-buffer-hook closed nil t))
    (add-hook 'kill-buffer-hook cleanup nil t)))

(defun niwa-report-render (report &optional origin)
  "Render REPORT in the current buffer without displaying or selecting it.
ORIGIN identifies the live caller; omission retains this inspector's owner.
REPORT's optional :diagnostic-scope is a captured root, `global' or
`unavailable'; it overrides ORIGIN's diagnostic ownership."
  (unless (and (stringp (plist-get report :title))
               (listp (plist-get report :sections)))
    (error "Invalid information report"))
  (let* ((scope (or (plist-get report :diagnostic-scope)
                   (if (buffer-live-p origin)
                       (with-current-buffer origin (niwa-cockpit-diagnostic-scope))
                     (if niwa-report--origin niwa-report--scope
                       (niwa-cockpit-diagnostic-scope)))))
         (origin (or origin niwa-report--origin (current-buffer)))
         (identity (list origin (plist-get report :identity) (plist-get report :title))))
    (setq niwa-report--scope scope)
    (condition-case err
        (when-let* ((check (plist-get report :validate))) (funcall check))
      (error
       (niwa-report--unavailable err)
       (signal (car err) (cdr err))))
    (if (equal identity niwa-report--identity)
        (niwa-report--save-position)
      (niwa-report--kill-index)
      (when niwa-report--unfollow (funcall niwa-report--unfollow))
      (funcall (or (plist-get report :mode) #'niwa-report-mode))
      (setq niwa-report--folds (make-hash-table :test #'equal)
            niwa-report--positions (make-hash-table :test #'equal)
            niwa-report--selected (plist-get report :initial)))
    (setq niwa-report--scope scope
          niwa-report--report report
          niwa-report--identity identity
          niwa-report--origin origin)
    (unless (seq-find (lambda (section)
                        (equal (plist-get section :id) niwa-report--selected))
                      (plist-get report :sections))
      (setq niwa-report--selected
            (plist-get (car (plist-get report :sections)) :id)))
    (add-hook 'kill-buffer-hook #'niwa-report--kill-index nil t)
    (niwa-report--render)
    (unless (plist-get report :navigator) (niwa-report--kill-index))
    (niwa-report--render-index)
    (current-buffer)))

(defun niwa-report-show (buffer report &optional noselect)
  "Display structured REPORT in BUFFER, returning the buffer.
NOSELECT displays the report without moving focus."
  (let ((origin (or niwa-report--origin (current-buffer)))
        (target (get-buffer-create buffer)))
    (with-current-buffer target (niwa-report-render report origin))
    (let ((window (niwa-window-display-buffer
                   target noselect (niwa-cockpit-menu-return-function))))
      (when (window-live-p window)
        (with-current-buffer target (niwa-report--display-index window))))
    target))

(provide 'niwa-report)
;;; niwa-report.el ends here
