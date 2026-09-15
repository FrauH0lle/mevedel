;;; mevedel-report.el --- Read-only information panels -*- lexical-binding: t -*-

;;; Commentary:

;; Display domain-owned reports as named sections.  Owners supply exact content
;; and section identities; this module owns only presentation and navigation.
;; Reports are plists with :title, :subtitle and :sections.  Each section has
;; :id, :title and :body, with optional :mode and :folded presentation hints.

;;; Code:

(require 'button)
(require 'seq)
(require 'mevedel-view-fontify)
(require 'outline)

(defvar-local mevedel-report--report nil "Report currently being inspected.")
(defvar-local mevedel-report--folds nil "Section fold choices in this report.")
(defvar-local mevedel-report--selected nil "Selected section identity.")
(defvar-local mevedel-report--body-start 1 "Start of the currently displayed source body.")
(defvar-local mevedel-report--positions nil "Reading positions by section identity.")
(defvar-local mevedel-report--origin nil "Buffer from which this panel was opened.")
(defvar-local mevedel-report--identity nil "Owner and item identity of this panel.")
(defvar-local mevedel-report--index nil "Section index buffer owned by this panel.")
(defvar-local mevedel-report--unfollow nil "Detach this inspector from its live source.")
(defvar-local mevedel-report--parent nil "Reading buffer owning this section index.")

(defun mevedel-report-fields (&rest rows)
  "Format ROWS of (LABEL VALUE) as aligned, visually wrapped information."
  (mapconcat
   (lambda (row)
     (if (null row) ""
       (concat (propertize (format "%-16s " (car row)) 'face 'shadow)
               (propertize (format "%s" (cadr row))
                           'wrap-prefix (make-string 17 ?\s))
               "\n")))
   rows ""))

(defvar-keymap mevedel-report-mode-map
  :parent special-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "n" #'mevedel-report-next
  "p" #'mevedel-report-previous
  "g" #'mevedel-report-refresh
  "q" #'mevedel-report-quit
  "RET" #'push-button)

(define-derived-mode mevedel-report-mode special-mode "MevInfo"
  "Read an information panel; RET toggles the section at point."
  (setq-local truncate-lines nil
              truncate-partial-width-windows nil
              word-wrap t
              buffer-invisibility-spec '(t)
              header-line-format "RET toggle · TAB next · S-TAB previous · g refresh · q close"))

(defun mevedel-report--owner ()
  "Return the reading buffer for this panel or section index."
  (unless (derived-mode-p 'mevedel-report-mode)
    (user-error "No information panel in this buffer"))
  (if (buffer-live-p mevedel-report--parent)
      mevedel-report--parent
    (current-buffer)))

(defun mevedel-report-next (&optional previous)
  "Move to the next section, or the previous one when PREVIOUS is non-nil."
  (interactive)
  (let ((owner (mevedel-report--owner)))
    (if (with-current-buffer owner
          (plist-get mevedel-report--report :navigator))
        (with-current-buffer owner
          (let* ((ids (mapcar (lambda (section) (plist-get section :id))
                              (plist-get mevedel-report--report :sections)))
                 (offset (seq-position ids mevedel-report--selected #'equal)))
            (when ids
              (mevedel-report-select-section
               (nth (mod (+ (or offset 0) (if previous -1 1)) (length ids)) ids)))))
      (if previous (backward-button 1 t) (forward-button 1 t)))))

(defun mevedel-report-previous ()
  "Move to the previous section."
  (interactive)
  (mevedel-report-next t))

(defun mevedel-report--unavailable (error-data)
  "Clear disclosed content after ERROR-DATA invalidates this inspector."
  (setq mevedel-report--report
        (list :title "Report unavailable" :sections nil
              :refresh (plist-get mevedel-report--report :refresh)))
  (mevedel-report--kill-index)
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (insert (propertize "* Report unavailable\n\n" 'face 'error)
            (error-message-string error-data))
    (goto-char (point-min))
    (set-buffer-modified-p nil)))

(defun mevedel-report-select-section (id)
  "Read the section identified by ID without changing its owner."
  (with-current-buffer (mevedel-report--owner)
    (when-let* ((check (plist-get mevedel-report--report :validate)))
      (condition-case err (funcall check)
        (error
         (mevedel-report--unavailable err)
         (signal (car err) (cdr err)))))
    (unless (seq-find (lambda (section) (equal id (plist-get section :id)))
                      (plist-get mevedel-report--report :sections))
      (user-error "Section is no longer available"))
    (mevedel-report--save-position)
    (setq mevedel-report--selected id)
    (mevedel-report--render)
    (mevedel-report--render-index)))

(defun mevedel-report-refresh ()
  "Refresh this report through its original owner without moving focus."
  (interactive)
  (with-current-buffer (mevedel-report--owner)
    (let ((origin mevedel-report--origin)
          (refresh (plist-get mevedel-report--report :refresh)))
      (unless (and refresh (buffer-live-p origin))
        (user-error "No live source to refresh this report"))
      (condition-case err
          (mevedel-report-render (with-current-buffer origin (funcall refresh)) origin)
        (error (mevedel-report--unavailable err) (signal (car err) (cdr err)))))))

(defun mevedel-report--kill-index ()
  "Release the index buffer and windows owned by this reading buffer."
  (when (buffer-live-p mevedel-report--index)
    (let ((index mevedel-report--index))
      (setq mevedel-report--index nil)
      (dolist (window (get-buffer-window-list index nil t))
        (when (window-parameter window 'mevedel-report-index)
          (unless (one-window-p t (window-frame window))
            (delete-window window))))
      (kill-buffer index))))

(defun mevedel-report-quit ()
  "Close this inspector and its section index, returning to its owner."
  (interactive)
  (let* ((buffer (mevedel-report--owner))
         (origin (buffer-local-value 'mevedel-report--origin buffer)))
    (with-current-buffer buffer (mevedel-report--kill-index))
    (when-let* ((window (get-buffer-window buffer)))
      (quit-window nil window))
    (kill-buffer buffer)
    (when (buffer-live-p origin)
      (pop-to-buffer origin))))

(defun mevedel-report--toggle (button)
  "Toggle the section identified by BUTTON."
  (let* ((inhibit-read-only t)
         (id (button-get button 'mevedel-report-section))
         (overlay (button-get button 'mevedel-report-overlay))
         (folded (not (overlay-get overlay 'invisible))))
    (puthash id folded mevedel-report--folds)
    (overlay-put overlay 'invisible folded)
    (button-put button 'display
                (concat (button-label button) (if folded "  ▸" "  ▾")))))

(defun mevedel-report--fontify (text mode)
  "Fontify exact TEXT in MODE for a read-only panel without font lock."
  (let ((text (copy-sequence (mevedel-view--fontify-as text mode)))
        (position 0))
    (while (< position (length text))
      (let ((end (next-single-property-change position 'font-lock-face text (length text))))
        (when-let* ((face (get-text-property position 'font-lock-face text)))
          (put-text-property position end 'face face text))
        (setq position end)))
    text))

(defun mevedel-report--insert-section (section)
  "Insert SECTION with its explicit heading and exact body."
  (let* ((id (plist-get section :id))
         (folded (gethash id mevedel-report--folds
                          (plist-get section :folded)))
         (heading (point))
         (button (insert-text-button
                  (concat "** " (plist-get section :title))
                  'face 'outline-2 'follow-link t
                  'mevedel-report-section id
                  'action #'mevedel-report--toggle))
         (start (point)))
    (insert "\n\n"
            (mevedel-report--fontify
             (or (plist-get section :body) "") (plist-get section :mode)))
    (unless (bolp) (insert "\n"))
    (let ((overlay (make-overlay start (point) nil t nil)))
      (overlay-put overlay 'mevedel-report-section id)
      (overlay-put overlay 'invisible folded)
      (overlay-put overlay 'isearch-open-invisible
                   (lambda (hidden)
                     (when (overlay-get hidden 'invisible)
                       (mevedel-report--toggle (button-at (1- (overlay-start hidden)))))))
      (button-put button 'mevedel-report-overlay overlay)
      (button-put button 'display
                  (concat (button-label button) (if folded "  ▸" "  ▾"))))
    (add-text-properties heading (point) (list 'mevedel-report-section id))
    (insert "\n")))

(defun mevedel-report--save-position ()
  "Retain source-relative point and scroll position for the selected section."
  (when mevedel-report--positions
    (puthash mevedel-report--selected
             (cons (- (point) mevedel-report--body-start)
                   (when-let* ((window (get-buffer-window (current-buffer))))
                     (- (window-start window) mevedel-report--body-start)))
             mevedel-report--positions)))

(defun mevedel-report--render ()
  "Render the current report without changing its source strings."
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer)
    (setq mevedel-report--body-start 1)
    (insert (propertize (concat "* " (plist-get mevedel-report--report :title))
                        'face 'outline-1)
            "\n")
    (when-let* ((subtitle (plist-get mevedel-report--report :subtitle)))
      (insert (propertize subtitle 'face 'shadow) "\n"))
    (insert "\n")
    (dolist (section (plist-get mevedel-report--report :sections))
      (if (plist-get mevedel-report--report :navigator)
          (when (equal (plist-get section :id) mevedel-report--selected)
            (insert (propertize (concat "** " (plist-get section :title))
                                'face 'outline-2) "\n\n")
            (setq mevedel-report--body-start (point))
            (insert (mevedel-report--fontify (or (plist-get section :body) "")
                                             (plist-get section :mode))))
        (mevedel-report--insert-section section)))
    (let ((position (gethash mevedel-report--selected mevedel-report--positions)))
      (goto-char (if position
                     (max (point-min) (min (point-max) (+ mevedel-report--body-start (car position))))
                   (point-min)))
      (when-let* ((start (cdr position))
                  (window (get-buffer-window (current-buffer))))
        (set-window-start window (max (point-min) (min (point-max) (+ mevedel-report--body-start start))) t)))
    (set-buffer-modified-p nil)))

(defun mevedel-report--render-index ()
  "Render the section index of the current reading buffer."
  (when (buffer-live-p mevedel-report--index)
    (let ((owner (current-buffer))
          (sections (plist-get mevedel-report--report :sections))
          (selected mevedel-report--selected)
          selected-position)
      (with-current-buffer mevedel-report--index
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
                           (mevedel-report-select-section id))))
              (insert "\n\n")))
          (goto-char (or selected-position (point-min)))
          (when-let* ((window (get-buffer-window (current-buffer))))
            (set-window-point window (point)))
          (set-buffer-modified-p nil))))))

(defun mevedel-report--display-index (window)
  "Display this panel's section index beside or above reading WINDOW."
  (when (plist-get mevedel-report--report :navigator)
    (unless (buffer-live-p mevedel-report--index)
      (let ((owner (current-buffer)))
        (setq mevedel-report--index
              (generate-new-buffer (concat (buffer-name) " sections")))
        (with-current-buffer mevedel-report--index
          (mevedel-report-mode)
          (setq mevedel-report--parent owner
                header-line-format "RET read · n/p section · q close"))))
    (unless (get-buffer-window mevedel-report--index)
      (let ((index-window
             (condition-case nil
                 (if (>= (window-total-width window) 100)
                     (split-window window -32 'left)
                   (split-window window -7 'above))
               (error nil))))
        (when index-window
          (set-window-buffer index-window mevedel-report--index)
          (set-window-parameter index-window 'mevedel-report-index (current-buffer)))))
    (add-hook 'window-size-change-functions #'mevedel-report--resize nil t)
    (setq header-line-format "n/p section · g refresh · q close")
    (mevedel-report--render-index)))

(defun mevedel-report--resize (window)
  "Adapt the section index when reading WINDOW changes size."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (plist-get mevedel-report--report :navigator))
    (let ((index (and (buffer-live-p mevedel-report--index)
                      (get-buffer-window mevedel-report--index (window-frame window)))))
      (when (and (window-live-p index)
                 (eq (window-parent index) (window-parent window)))
        (let* ((side (= (cadr (window-edges index)) (cadr (window-edges window))))
               (width (if side (+ (window-total-width index) (window-total-width window))
                        (window-total-width window))))
          (unless (eq side (>= width 100))
            (let ((selected (eq (selected-window) index)))
              (delete-window index)
              (mevedel-report--display-index window)
              (when selected
                (when-let* ((new (get-buffer-window mevedel-report--index)))
                  (select-window new))))))))))

(defun mevedel-report-follow-source (source report-function)
  "Follow SOURCE changes in this inspector using REPORT-FUNCTION.
Coalesce source events; closing the inspector leaves SOURCE untouched."
  (when mevedel-report--unfollow (funcall mevedel-report--unfollow))
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
                               (mevedel-report-render (funcall refresh))
                             (error
                              (mevedel-report--unavailable err))))))))))
          closed
          (lambda ()
            (when (buffer-live-p target)
              (with-current-buffer target
                (mevedel-report-render
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
    (setq mevedel-report--unfollow cleanup
          mevedel-report--report (plist-put mevedel-report--report :refresh refresh))
    (with-current-buffer source
      (add-hook 'after-change-functions change nil t)
      (add-hook 'kill-buffer-hook closed nil t))
    (add-hook 'kill-buffer-hook cleanup nil t)))

(defun mevedel-report-render (report &optional origin)
  "Render REPORT in the current buffer without displaying or selecting it.
ORIGIN identifies the live caller; omission retains this inspector's owner."
  (unless (and (stringp (plist-get report :title))
               (listp (plist-get report :sections)))
    (error "Invalid information report"))
  (let* ((origin (or origin mevedel-report--origin (current-buffer)))
         (identity (list origin (plist-get report :identity) (plist-get report :title))))
    (condition-case err
        (when-let* ((check (plist-get report :validate))) (funcall check))
      (error
       (mevedel-report--unavailable err)
       (signal (car err) (cdr err))))
    (if (equal identity mevedel-report--identity)
        (mevedel-report--save-position)
      (mevedel-report--kill-index)
      (when mevedel-report--unfollow (funcall mevedel-report--unfollow))
      (funcall (or (plist-get report :mode) #'mevedel-report-mode))
      (setq mevedel-report--folds (make-hash-table :test #'equal)
            mevedel-report--positions (make-hash-table :test #'equal)
            mevedel-report--selected (plist-get report :initial)))
    (setq mevedel-report--report report
          mevedel-report--identity identity
          mevedel-report--origin origin)
    (unless (seq-find (lambda (section)
                        (equal (plist-get section :id) mevedel-report--selected))
                      (plist-get report :sections))
      (setq mevedel-report--selected
            (plist-get (car (plist-get report :sections)) :id)))
    (add-hook 'kill-buffer-hook #'mevedel-report--kill-index nil t)
    (mevedel-report--render)
    (unless (plist-get report :navigator) (mevedel-report--kill-index))
    (mevedel-report--render-index)
    (current-buffer)))

(defun mevedel-report-show (buffer report &optional noselect)
  "Display structured REPORT in BUFFER, returning the buffer.
NOSELECT displays the report without moving focus."
  (let ((origin (or mevedel-report--origin (current-buffer)))
        (target (get-buffer-create buffer)))
    (with-current-buffer target (mevedel-report-render report origin))
    (let ((window (if noselect (display-buffer target)
                    (pop-to-buffer target) (selected-window))))
      (when (window-live-p window)
        (with-current-buffer target (mevedel-report--display-index window))))
    target))

(provide 'mevedel-report)
;;; mevedel-report.el ends here
