;;; niwa-gptel-bridge.el -- gptel-menu bridge for niwa -*- lexical-binding: t -*-

;;; Commentary:

;; Bridge from the niwa session cockpit to gptel's transient menu.  The
;; view buffer is user-facing, but gptel state belongs to the paired data
;; buffer, so view-launched gptel menus temporarily run from the data buffer
;; and restore the view after nested prompt-edit flows finish.

;;; Code:

(require 'niwa-utilities)

;; `gptel'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--fsm-last)
(defvar gptel-backend)

;; `gptel-transient'
(declare-function gptel--edit-directive "ext:gptel-transient"
                  (&optional sym &rest args))
(declare-function gptel-menu "ext:gptel-transient" ())
(defvar gptel--set-buffer-locally)

;; `niwa-agent-conversation'
(defvar niwa--agent-invocation)

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-data-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-origin-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-context-view-buffer
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(autoload 'niwa-cockpit-context-data-buffer "niwa-cockpit")
(autoload 'niwa-cockpit-context-origin-buffer "niwa-cockpit")
(autoload 'niwa-cockpit-context-view-buffer "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")

;; `niwa-engine'
(declare-function niwa-engine-external-p "niwa-engine" (backend))
(autoload 'niwa-engine-external-p "niwa-engine")

;; `niwa-structs'
(defvar niwa--current-request)
(defvar niwa--data-buffer)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-view-composer'
(declare-function niwa-view--send-root
                  "niwa-view-composer" (&optional steering-input))
(autoload 'niwa-view--send-root "niwa-view-composer")
(defvar niwa-view--side-conversation-p)

;; `transient'
(defvar transient--prefix)
(defvar transient-post-exit-hook)

(defvar niwa-gptel-bridge--return-view-buffer nil
  "View buffer to restore after a gptel transient exits.")

(defvar niwa-gptel-bridge--return-data-buffer nil
  "Data buffer that may need replacing with the view after transient exit.")

(defvar niwa-gptel-bridge--return-window nil
  "Window that launched a gptel transient from a niwa view.")

(defvar niwa-gptel-bridge--return-window-snapshot nil
  "Window/buffer pairs captured before a view-launched gptel command.")

(defun niwa-gptel-bridge--steering-buffer (&optional info)
  "Return the niwa data buffer owning INFO or the current gptel request."
  (let* ((info (or info (and (bound-and-true-p gptel--fsm-last)
                             (gptel-fsm-info gptel--fsm-last))))
         (buffer (or (plist-get info :buffer) (current-buffer))))
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'niwa--session buffer))
      buffer)))

(defun niwa-gptel-bridge--steer-advice (orig-fn &rest args)
  "Route ORIG-FN's native steering through the owning niwa composer.
Unrelated gptel requests pass ARGS to ORIG-FN unchanged."
  (if-let* ((buffer (niwa-gptel-bridge--steering-buffer)))
      (with-current-buffer buffer
        (when (bound-and-true-p niwa--agent-invocation)
          (niwa-diagnostic-user-error (niwa-diagnostic-scope buffer)
                                         "Use SendMessage to steer a retained agent"))
        (unless (buffer-live-p niwa--view-buffer)
          (niwa-diagnostic-user-error (niwa-diagnostic-scope buffer)
                                         "Open the niwa session view to steer"))
        (let ((request niwa--current-request)
              (view niwa--view-buffer)
              (scope (niwa-diagnostic-scope buffer)))
          (unless request
            (niwa-diagnostic-user-error scope "No active root turn to steer"))
          (with-current-buffer view
            (when niwa-view--side-conversation-p
              (niwa-diagnostic-user-error scope "Steering is unavailable in a side conversation"))
            (let ((input (read-string "Steering instructions for this turn: ")))
              (unless (string-blank-p input)
                (unless (and (buffer-live-p buffer) (buffer-live-p view)
                             (eq request (buffer-local-value
                                          'niwa--current-request buffer)))
                  (niwa-diagnostic-user-error scope "The root turn changed while entering steering"))
                (niwa-view--send-root input))))))
    (apply orig-fn args)))

(defun niwa-gptel-bridge--tool-steer-advice
    (orig-fn &optional calls overlay info)
  "Keep ORIG-FN's CALLS rejection out of managed niwa requests.
OVERLAY and INFO identify the original gptel confirmation UI."
  (if-let* ((buffer (niwa-gptel-bridge--steering-buffer
                    (or info (and (overlayp overlay) (overlay-get overlay 'info))))))
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope buffer)
       "Use niwa's permission feedback or composer to steer")
    (funcall orig-fn calls overlay info)))

(defun niwa-gptel-bridge-install ()
  "Route native gptel steering through niwa's accepted-input path."
  (advice-add 'gptel-menu :before #'niwa-gptel-bridge--assert-menu-backend)
  (dolist (command '(gptel-send--steer gptel--suffix-steer))
    (advice-add command :around #'niwa-gptel-bridge--steer-advice))
  (advice-add 'gptel--steer-tool-calls
              :around #'niwa-gptel-bridge--tool-steer-advice))

(defun niwa-gptel-bridge-uninstall ()
  "Remove niwa's native gptel steering routing."
  (advice-remove 'gptel-menu #'niwa-gptel-bridge--assert-menu-backend)
  (dolist (command '(gptel-send--steer gptel--suffix-steer))
    (advice-remove command #'niwa-gptel-bridge--steer-advice))
  (advice-remove 'gptel--steer-tool-calls
                 #'niwa-gptel-bridge--tool-steer-advice))

(defun niwa-gptel-bridge--assert-menu-backend (&rest _args)
  "Reject gptel request controls for an external session engine."
  (with-current-buffer (if (and (boundp 'niwa--data-buffer)
                               (buffer-live-p niwa--data-buffer))
                          niwa--data-buffer (current-buffer))
    (when (and (bound-and-true-p niwa--session)
               (niwa-engine-external-p gptel-backend))
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope (current-buffer))
       "The gptel request controls are unavailable for this engine; use niwa's model menu for model and effort, or its tools menu"))))

(defun niwa-gptel-bridge--active-p ()
  "Return non-nil while a view-launched gptel bridge is restoring."
  (and niwa-gptel-bridge--return-view-buffer
       (buffer-live-p niwa-gptel-bridge--return-view-buffer)))

(defun niwa-gptel-bridge--clear-return-state ()
  "Clear pending gptel transient view restoration state."
  (remove-hook 'transient-post-exit-hook
               #'niwa-gptel-bridge--return-to-view)
  (setq niwa-gptel-bridge--return-view-buffer nil
        niwa-gptel-bridge--return-data-buffer nil
        niwa-gptel-bridge--return-window nil
        niwa-gptel-bridge--return-window-snapshot nil))

(defun niwa-gptel-bridge--prompt-edit-active-p ()
  "Return non-nil while gptel's prompt edit buffer is displayed."
  (when-let* ((buffer (get-buffer "*gptel-prompt*")))
    (or (eq (current-buffer) buffer)
        (get-buffer-window buffer t))))

(defun niwa-gptel-bridge--window-snapshot ()
  "Return live frame windows paired with their current buffers."
  (mapcar (lambda (window)
            (cons window (window-buffer window)))
          (window-list nil 'no-minibuf)))

(defun niwa-gptel-bridge--launch-window (view-buffer)
  "Return the window that should be restored to VIEW-BUFFER."
  (cond
   ((eq (window-buffer (selected-window)) view-buffer)
    (selected-window))
   ((get-buffer-window view-buffer t))
   (t (selected-window))))

(defun niwa-gptel-bridge--restore-window-buffers ()
  "Restore window buffers after a view-launched gptel command."
  (let ((view-buffer niwa-gptel-bridge--return-view-buffer)
        (data-buffer niwa-gptel-bridge--return-data-buffer)
        (origin-window niwa-gptel-bridge--return-window))
    (when (and view-buffer data-buffer
               (buffer-live-p view-buffer)
               (buffer-live-p data-buffer))
      (dolist (entry niwa-gptel-bridge--return-window-snapshot)
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

(defun niwa-gptel-bridge--return-to-view ()
  "Restore the launching niwa view after a gptel transient exits."
  (let ((view-buffer niwa-gptel-bridge--return-view-buffer)
        (data-buffer niwa-gptel-bridge--return-data-buffer))
    (cond
     ((not (and view-buffer data-buffer
                (buffer-live-p view-buffer)
                (buffer-live-p data-buffer)))
      (niwa-gptel-bridge--clear-return-state))
     ((niwa-gptel-bridge--prompt-edit-active-p)
      nil)
     (t
      (niwa-gptel-bridge--restore-window-buffers)
      (niwa-gptel-bridge--clear-return-state)))))

(defun niwa-gptel-bridge--schedule-return-to-view
    (view-buffer data-buffer)
  "Schedule restoration of VIEW-BUFFER, paired with DATA-BUFFER.

VIEW-BUFFER is restored after the gptel transient exits.  Only one
restoration is pending at a time, so scheduling a different view also
restores whichever view is pending already: its own exit hook will never
run once this state replaces it."
  (when (and view-buffer data-buffer
             (buffer-live-p view-buffer)
             (buffer-live-p data-buffer))
    (let ((same-pair (and (eq niwa-gptel-bridge--return-view-buffer
                              view-buffer)
                          (eq niwa-gptel-bridge--return-data-buffer
                              data-buffer))))
      (unless (and same-pair
                   (window-live-p niwa-gptel-bridge--return-window))
        ;; The launch window has to be resolved before the pending view is
        ;; handed its windows back, because restoring reselects the window
        ;; that view was launched from.
        (let ((window (niwa-gptel-bridge--launch-window view-buffer)))
          (unless same-pair
            (niwa-gptel-bridge--restore-window-buffers))
          (setq niwa-gptel-bridge--return-view-buffer view-buffer
                niwa-gptel-bridge--return-data-buffer data-buffer
                niwa-gptel-bridge--return-window window
                niwa-gptel-bridge--return-window-snapshot
                (niwa-gptel-bridge--window-snapshot)))))
    (add-hook 'transient-post-exit-hook
              #'niwa-gptel-bridge--return-to-view)))

(defun niwa-gptel-bridge--edit-directive-advice (orig-fn &rest args)
  "Wrap gptel directive edit callback while the bridge is active."
  (let* ((leading (and args (not (keywordp (car args)))))
         (sym (and leading (car args)))
         (plist (if leading (cdr args) args))
         (callback (plist-get plist :callback))
         (data-buffer niwa-gptel-bridge--return-data-buffer)
         (restore-p
          (memq (current-buffer)
                (list niwa-gptel-bridge--return-view-buffer data-buffer))))
    (when (or callback restore-p)
      (setq plist
            (plist-put
             (copy-sequence plist)
             :callback
             (lambda (message)
               (unwind-protect
                   (if restore-p
                       (progn
                         (niwa-gptel-bridge--restore-window-buffers)
                         (if callback
                             (if (buffer-live-p data-buffer)
                                 (with-current-buffer data-buffer
                                   (funcall callback message))
                               (funcall callback message))
                           (niwa-gptel-bridge--clear-return-state)))
                     ;; Unrelated edits keep their own callback buffer.
                     (funcall callback message))
                 (unless (bound-and-true-p transient--prefix)
                   (niwa-gptel-bridge--return-to-view)
                   (niwa-gptel-bridge--cleanup-advice)))))))
    (apply orig-fn (if leading (cons sym plist) plist))))

(defun niwa-gptel-bridge--cleanup-advice ()
  "Remove temporary gptel bridge advice after final transient exit."
  (unless (niwa-gptel-bridge--active-p)
    (remove-hook 'transient-post-exit-hook
                 #'niwa-gptel-bridge--cleanup-advice)
    (when (fboundp 'gptel--edit-directive)
      (advice-remove 'gptel--edit-directive
                     #'niwa-gptel-bridge--edit-directive-advice))))

(defun niwa-gptel-bridge--install-advice ()
  "Install temporary advice needed by the explicit gptel bridge."
  (unless (advice-member-p
           #'niwa-gptel-bridge--edit-directive-advice
           'gptel--edit-directive)
    (advice-add 'gptel--edit-directive
                :around #'niwa-gptel-bridge--edit-directive-advice))
  (add-hook 'transient-post-exit-hook
            #'niwa-gptel-bridge--cleanup-advice 90))

;;;###autoload
(defun niwa-gptel-bridge-open (&optional context)
  "Open `gptel-menu' for cockpit CONTEXT's data buffer."
  (interactive)
  (require 'gptel-transient)
  (let* ((context (or context (niwa-cockpit-current-context)))
         (origin (or (niwa-cockpit-context-origin-buffer context)
                     (current-buffer)))
         (view-buffer (niwa-cockpit-context-view-buffer context))
         (data-buffer (niwa-cockpit-context-data-buffer context))
         (view-origin-p (eq origin view-buffer))
         (window (and view-origin-p
                      (or (get-buffer-window view-buffer t)
                          (selected-window)))))
    (unless (and view-buffer data-buffer)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope origin)
                                     "No niwa session cockpit here"))
    (with-current-buffer data-buffer
      (niwa-gptel-bridge--assert-menu-backend)
      (setq-local gptel--set-buffer-locally t))
    (if view-origin-p
        (let ((setup-ok nil))
          (niwa-gptel-bridge--schedule-return-to-view
           view-buffer data-buffer)
          (niwa-gptel-bridge--install-advice)
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
              (niwa-gptel-bridge--return-to-view)
              (niwa-gptel-bridge--cleanup-advice))))
      (with-current-buffer data-buffer
        (call-interactively #'gptel-menu)))))

(provide 'niwa-gptel-bridge)

;;; niwa-gptel-bridge.el ends here
