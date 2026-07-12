;;; mevedel-gptel-bridge.el -- gptel-menu bridge for mevedel -*- lexical-binding: t -*-

;;; Commentary:

;; Bridge from the mevedel session cockpit to gptel's transient menu.  The
;; view buffer is user-facing, but gptel state belongs to the paired data
;; buffer, so view-launched gptel menus temporarily run from the data buffer
;; and restore the view after nested prompt-edit flows finish.

;;; Code:

;; `gptel-transient'
(declare-function gptel--edit-directive "ext:gptel-transient"
                  (&optional sym &rest args))
(declare-function gptel-menu "ext:gptel-transient" ())
(defvar gptel--set-buffer-locally)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-origin-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-view-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())

;; `transient'
(defvar transient--prefix)
(defvar transient-post-exit-hook)

(defvar mevedel-gptel-bridge--return-view-buffer nil
  "View buffer to restore after a gptel transient exits.")

(defvar mevedel-gptel-bridge--return-data-buffer nil
  "Data buffer that may need replacing with the view after transient exit.")

(defvar mevedel-gptel-bridge--return-window nil
  "Window that launched a gptel transient from a mevedel view.")

(defvar mevedel-gptel-bridge--return-window-snapshot nil
  "Window/buffer pairs captured before a view-launched gptel command.")

(defun mevedel-gptel-bridge--active-p ()
  "Return non-nil while a view-launched gptel bridge is restoring."
  (and mevedel-gptel-bridge--return-view-buffer
       (buffer-live-p mevedel-gptel-bridge--return-view-buffer)))

(defun mevedel-gptel-bridge--clear-return-state ()
  "Clear pending gptel transient view restoration state."
  (remove-hook 'transient-post-exit-hook
               #'mevedel-gptel-bridge--return-to-view)
  (setq mevedel-gptel-bridge--return-view-buffer nil
        mevedel-gptel-bridge--return-data-buffer nil
        mevedel-gptel-bridge--return-window nil
        mevedel-gptel-bridge--return-window-snapshot nil))

(defun mevedel-gptel-bridge--prompt-edit-active-p ()
  "Return non-nil while gptel's prompt edit buffer is displayed."
  (when-let* ((buffer (get-buffer "*gptel-prompt*")))
    (or (eq (current-buffer) buffer)
        (get-buffer-window buffer t))))

(defun mevedel-gptel-bridge--window-snapshot ()
  "Return live frame windows paired with their current buffers."
  (mapcar (lambda (window)
            (cons window (window-buffer window)))
          (window-list nil 'no-minibuf)))

(defun mevedel-gptel-bridge--launch-window (view-buffer)
  "Return the window that should be restored to VIEW-BUFFER."
  (cond
   ((eq (window-buffer (selected-window)) view-buffer)
    (selected-window))
   ((get-buffer-window view-buffer t))
   (t (selected-window))))

(defun mevedel-gptel-bridge--restore-window-buffers ()
  "Restore window buffers after a view-launched gptel command."
  (let ((view-buffer mevedel-gptel-bridge--return-view-buffer)
        (data-buffer mevedel-gptel-bridge--return-data-buffer)
        (origin-window mevedel-gptel-bridge--return-window))
    (when (and view-buffer data-buffer
               (buffer-live-p view-buffer)
               (buffer-live-p data-buffer))
      (dolist (entry mevedel-gptel-bridge--return-window-snapshot)
        (let ((window (car entry))
              (buffer (cdr entry)))
          (when (and (window-live-p window)
                     (not (eq window origin-window))
                     (memq (window-buffer window)
                           (list data-buffer view-buffer))
                     (buffer-live-p buffer))
            (ignore-errors
              (set-window-buffer window buffer)))))
      (when (window-live-p origin-window)
        (ignore-errors
          (set-window-buffer origin-window view-buffer))
        (select-window origin-window)))))

(defun mevedel-gptel-bridge--return-to-view ()
  "Restore the launching mevedel view after a gptel transient exits."
  (let ((view-buffer mevedel-gptel-bridge--return-view-buffer)
        (data-buffer mevedel-gptel-bridge--return-data-buffer))
    (cond
     ((not (and view-buffer data-buffer
                (buffer-live-p view-buffer)
                (buffer-live-p data-buffer)))
      (mevedel-gptel-bridge--clear-return-state))
     ((mevedel-gptel-bridge--prompt-edit-active-p)
      nil)
     (t
      (mevedel-gptel-bridge--restore-window-buffers)
      (mevedel-gptel-bridge--clear-return-state)))))

(defun mevedel-gptel-bridge--schedule-return-to-view
    (view-buffer data-buffer)
  "Schedule restoration of VIEW-BUFFER after gptel transient exit."
  (when (and view-buffer data-buffer
             (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (unless (and (eq mevedel-gptel-bridge--return-view-buffer view-buffer)
                 (eq mevedel-gptel-bridge--return-data-buffer data-buffer)
                 (window-live-p mevedel-gptel-bridge--return-window))
      (setq mevedel-gptel-bridge--return-view-buffer view-buffer
            mevedel-gptel-bridge--return-data-buffer data-buffer
            mevedel-gptel-bridge--return-window
            (mevedel-gptel-bridge--launch-window view-buffer)
            mevedel-gptel-bridge--return-window-snapshot
            (mevedel-gptel-bridge--window-snapshot)))
    (add-hook 'transient-post-exit-hook
              #'mevedel-gptel-bridge--return-to-view)))

(defun mevedel-gptel-bridge--edit-directive-args (args)
  "Return ARGS with a bridge-restoring `:callback' wrapper when needed."
  (if (not mevedel-gptel-bridge--return-view-buffer)
      args
    (let* ((leading (and args (not (keywordp (car args)))))
           (sym (and leading (car args)))
           (plist (if leading (cdr args) args))
           (callback (plist-get plist :callback))
           (data-buffer mevedel-gptel-bridge--return-data-buffer)
           (wrapped-callback
            (lambda (message)
              (mevedel-gptel-bridge--restore-window-buffers)
              (if callback
                  (if (buffer-live-p data-buffer)
                      (with-current-buffer data-buffer
                        (funcall callback message))
                    (funcall callback message))
                (mevedel-gptel-bridge--clear-return-state)))))
      (setq plist (plist-put (copy-sequence plist)
                             :callback wrapped-callback))
      (if leading
          (cons sym plist)
        plist))))

(defun mevedel-gptel-bridge--edit-directive-advice (orig-fn &rest args)
  "Wrap gptel directive edit callback while the bridge is active."
  (let* ((args (mevedel-gptel-bridge--edit-directive-args args))
         (leading (and args (not (keywordp (car args)))))
         (sym (and leading (car args)))
         (plist (if leading (cdr args) args))
         (callback (plist-get plist :callback)))
    (when callback
      (setq plist
            (plist-put
             (copy-sequence plist)
             :callback
             (lambda (message)
               (unwind-protect
                   (funcall callback message)
                 (unless (bound-and-true-p transient--prefix)
                   (mevedel-gptel-bridge--return-to-view)
                   (mevedel-gptel-bridge--cleanup-advice)))))))
    (apply orig-fn (if leading (cons sym plist) plist))))

(defun mevedel-gptel-bridge--cleanup-advice ()
  "Remove temporary gptel bridge advice after final transient exit."
  (unless (mevedel-gptel-bridge--active-p)
    (remove-hook 'transient-post-exit-hook
                 #'mevedel-gptel-bridge--cleanup-advice)
    (when (fboundp 'gptel--edit-directive)
      (advice-remove 'gptel--edit-directive
                     #'mevedel-gptel-bridge--edit-directive-advice))))

(defun mevedel-gptel-bridge--install-advice ()
  "Install temporary advice needed by the explicit gptel bridge."
  (require 'gptel-transient)
  (unless (advice-member-p
           #'mevedel-gptel-bridge--edit-directive-advice
           'gptel--edit-directive)
    (advice-add 'gptel--edit-directive
                :around #'mevedel-gptel-bridge--edit-directive-advice))
  (add-hook 'transient-post-exit-hook
            #'mevedel-gptel-bridge--cleanup-advice 90))

;;;###autoload
(defun mevedel-gptel-bridge-open (&optional context)
  "Open `gptel-menu' for cockpit CONTEXT's data buffer."
  (interactive)
  (require 'gptel-transient)
  (require 'mevedel-cockpit)
  (let* ((context (or context (mevedel-cockpit-current-context)))
         (origin (or (mevedel-cockpit-context-origin-buffer context)
                     (current-buffer)))
         (view-buffer (mevedel-cockpit-context-view-buffer context))
         (data-buffer (mevedel-cockpit-context-data-buffer context))
         (view-origin-p (eq origin view-buffer))
         (window (and view-origin-p
                      (or (get-buffer-window view-buffer t)
                          (selected-window)))))
    (unless (and view-buffer data-buffer)
      (user-error "No mevedel session cockpit here"))
    (with-current-buffer data-buffer
      (setq-local gptel--set-buffer-locally t))
    (if view-origin-p
        (let ((setup-ok nil))
          (mevedel-gptel-bridge--schedule-return-to-view
           view-buffer data-buffer)
          (mevedel-gptel-bridge--install-advice)
          (unwind-protect
              (progn
                (when (window-live-p window)
                  (set-window-buffer window data-buffer))
                (with-selected-window
                    (if (window-live-p window) window (selected-window))
                  (with-current-buffer data-buffer
                    (call-interactively #'gptel-menu)))
                (setq setup-ok t))
            (unless setup-ok
              (mevedel-gptel-bridge--return-to-view)
              (mevedel-gptel-bridge--cleanup-advice))))
      (with-current-buffer data-buffer
        (call-interactively #'gptel-menu)))))

(provide 'mevedel-gptel-bridge)

;;; mevedel-gptel-bridge.el ends here
