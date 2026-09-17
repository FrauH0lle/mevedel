;;; mevedel-gptel-bridge.el -- gptel-menu bridge for mevedel -*- lexical-binding: t -*-

;;; Commentary:

;; Bridge from the mevedel session cockpit to gptel's transient menu.  The
;; view buffer is user-facing, but gptel state belongs to the paired data
;; buffer, so view-launched gptel menus temporarily run from the data buffer
;; and restore the view after nested prompt-edit flows finish.

;;; Code:

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--fsm-last)

;; `gptel-transient'
(declare-function gptel--edit-directive "ext:gptel-transient"
                  (&optional sym &rest args))
(declare-function gptel-menu "ext:gptel-transient" ())
(defvar gptel--set-buffer-locally)

;; `mevedel-agent-conversation'
(defvar mevedel--agent-invocation)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-origin-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-view-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context
                  "mevedel-cockpit" ())
(autoload 'mevedel-cockpit-context-data-buffer "mevedel-cockpit")
(autoload 'mevedel-cockpit-context-origin-buffer "mevedel-cockpit")
(autoload 'mevedel-cockpit-context-view-buffer "mevedel-cockpit")
(autoload 'mevedel-cockpit-current-context "mevedel-cockpit")

;; `mevedel-structs'
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-view-composer'
(declare-function mevedel-view--send-root
                  "mevedel-view-composer" (&optional steering-input))
(autoload 'mevedel-view--send-root "mevedel-view-composer")
(defvar mevedel-view--side-conversation-p)

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

(defun mevedel-gptel-bridge--steering-buffer (&optional info)
  "Return the mevedel data buffer owning INFO or the current gptel request."
  (let* ((info (or info (and (bound-and-true-p gptel--fsm-last)
                             (gptel-fsm-info gptel--fsm-last))))
         (buffer (or (plist-get info :buffer) (current-buffer))))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'mevedel--session buffer))
      buffer)))

(defun mevedel-gptel-bridge--steer-advice (orig-fn &rest args)
  "Route ORIG-FN's native steering through the owning mevedel composer.
Unrelated gptel requests pass ARGS to ORIG-FN unchanged."
  (if-let* ((buffer (mevedel-gptel-bridge--steering-buffer)))
      (with-current-buffer buffer
        (when (bound-and-true-p mevedel--agent-invocation)
          (user-error "Use FollowupAgent or SendMessage to steer a retained agent"))
        (unless (buffer-live-p mevedel--view-buffer)
          (user-error "Open the mevedel session view to steer"))
        (let ((request mevedel--current-request)
              (view mevedel--view-buffer))
          (unless request
            (user-error "No active root turn to steer"))
          (with-current-buffer view
            (when mevedel-view--side-conversation-p
              (user-error "Steering is unavailable in a side conversation"))
            (let ((input (read-string "Steering instructions for this turn: ")))
              (unless (string-blank-p input)
                (unless (and (buffer-live-p buffer) (buffer-live-p view)
                             (eq request (buffer-local-value
                                          'mevedel--current-request buffer)))
                  (user-error "The root turn changed while entering steering"))
                (mevedel-view--send-root input))))))
    (apply orig-fn args)))

(defun mevedel-gptel-bridge--tool-steer-advice
    (orig-fn &optional calls overlay info)
  "Keep ORIG-FN's CALLS rejection out of managed mevedel requests.
OVERLAY and INFO identify the original gptel confirmation UI."
  (if (mevedel-gptel-bridge--steering-buffer
       (or info (and (overlayp overlay) (overlay-get overlay 'info))))
      (user-error "Use mevedel's permission feedback or composer to steer")
    (funcall orig-fn calls overlay info)))

(defun mevedel-gptel-bridge-install ()
  "Route native gptel steering through mevedel's accepted-input path."
  (dolist (command '(gptel-send--steer gptel--suffix-steer))
    (advice-add command :around #'mevedel-gptel-bridge--steer-advice))
  (advice-add 'gptel--steer-tool-calls
              :around #'mevedel-gptel-bridge--tool-steer-advice))

(defun mevedel-gptel-bridge-uninstall ()
  "Remove mevedel's native gptel steering routing."
  (dolist (command '(gptel-send--steer gptel--suffix-steer))
    (advice-remove command #'mevedel-gptel-bridge--steer-advice))
  (advice-remove 'gptel--steer-tool-calls
                 #'mevedel-gptel-bridge--tool-steer-advice))

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
  "Schedule restoration of VIEW-BUFFER, paired with DATA-BUFFER.

VIEW-BUFFER is restored after the gptel transient exits.  Only one
restoration is pending at a time, so scheduling a different view also
restores whichever view is pending already: its own exit hook will never
run once this state replaces it."
  (when (and view-buffer data-buffer
             (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (let ((same-pair (and (eq mevedel-gptel-bridge--return-view-buffer
                              view-buffer)
                          (eq mevedel-gptel-bridge--return-data-buffer
                              data-buffer))))
      (unless (and same-pair
                   (window-live-p mevedel-gptel-bridge--return-window))
        ;; The launch window has to be resolved before the pending view is
        ;; handed its windows back, because restoring reselects the window
        ;; that view was launched from.
        (let ((window (mevedel-gptel-bridge--launch-window view-buffer)))
          (unless same-pair
            (mevedel-gptel-bridge--restore-window-buffers))
          (setq mevedel-gptel-bridge--return-view-buffer view-buffer
                mevedel-gptel-bridge--return-data-buffer data-buffer
                mevedel-gptel-bridge--return-window window
                mevedel-gptel-bridge--return-window-snapshot
                (mevedel-gptel-bridge--window-snapshot)))))
    (add-hook 'transient-post-exit-hook
              #'mevedel-gptel-bridge--return-to-view)))

(defun mevedel-gptel-bridge--edit-directive-advice (orig-fn &rest args)
  "Wrap gptel directive edit callback while the bridge is active."
  (let* ((leading (and args (not (keywordp (car args)))))
         (sym (and leading (car args)))
         (plist (if leading (cdr args) args))
         (callback (plist-get plist :callback))
         (data-buffer mevedel-gptel-bridge--return-data-buffer)
         (restore-p
          (memq (current-buffer)
                (list mevedel-gptel-bridge--return-view-buffer data-buffer))))
    (when (or callback restore-p)
      (setq plist
            (plist-put
             (copy-sequence plist)
             :callback
             (lambda (message)
               (unwind-protect
                   (if restore-p
                       (progn
                         (mevedel-gptel-bridge--restore-window-buffers)
                         (if callback
                             (if (buffer-live-p data-buffer)
                                 (with-current-buffer data-buffer
                                   (funcall callback message))
                               (funcall callback message))
                           (mevedel-gptel-bridge--clear-return-state)))
                     ;; Unrelated edits keep their own callback buffer.
                     (funcall callback message))
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
