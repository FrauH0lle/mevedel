;;; mevedel-view-control-transfer.el -- View control-transfer UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns cooperative control-transfer polling, presentation, commands, and
;; view registration.  The generic interaction owner supplies redraw and
;; drain callbacks; durable lease and transfer state remain session-owned.

;;; Code:

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-framed-body
                  "mevedel-interaction-prompt" (content face))
(declare-function mevedel--prompt-key "mevedel-interaction-prompt" (key))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-save
                  "mevedel-session-artifacts"
                  (session buffer &optional settled force))

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer--follow-published
                  "mevedel-session-control-transfer"
                  (session buffer &optional force))
(declare-function mevedel-session-control-transfer-acquire
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-decide
                  "mevedel-session-control-transfer" (session decision))
(declare-function mevedel-session-control-transfer-descriptor
                  "mevedel-session-control-transfer" (session read-only-p))
(declare-function mevedel-session-control-transfer-drain-blocker
                  "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-poll
                  "mevedel-session-control-transfer"
                  (session buffer read-only-p))
(declare-function mevedel-session-control-transfer-register-drain
                  "mevedel-session-control-transfer" (session predicate))
(declare-function mevedel-session-control-transfer-register-observer
                  "mevedel-session-control-transfer" (session observer))
(declare-function mevedel-session-control-transfer-register-presentation
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-register-root-buffer
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-request
                  "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-unregister-drain
                  "mevedel-session-control-transfer" (session predicate))
(declare-function mevedel-session-control-transfer-unregister-observer
                  "mevedel-session-control-transfer" (session observer))
(declare-function mevedel-session-control-transfer-unregister-presentation
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-unregister-root-buffer
                  "mevedel-session-control-transfer" (session buffer))
(defvar mevedel-session-follow-published)

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability"
                  (session-dir &optional session))
(declare-function mevedel-session-durability-lease-state
                  "mevedel-session-durability" (session-dir))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-apply-read-only-mode
                  "mevedel-session-persistence" (buf &optional reason))
(defvar mevedel-session--read-only-mode)
(defvar mevedel-session--save-failed)

;; `mevedel-structs'
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--session)

;; `mevedel-view-agent'
(declare-function mevedel-view-reset-agent-ephemeral-state
                  "mevedel-view-agent" (&optional view-buffer))
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-history'
(declare-function mevedel-view-history-load "mevedel-view-history"
                  (&optional session))
(declare-function mevedel-view-history-save "mevedel-view-history"
                  (&optional view-buffer))

;; `mevedel-view-render'
(declare-function mevedel-view--full-rerender "mevedel-view-render"
                  (&optional transcript-buffer source-changed-p))
(declare-function mevedel-view--rebase-data-sources "mevedel-view-render"
                  (mapping))


;;
;;; Customization

(defcustom mevedel-view-control-transfer-poll-seconds 5
  "Seconds between cooperative control-transfer polls in a view.

Each poll reads the durable lease head, the target clock, and the transfer
mailbox.  On a remote target that is several synchronous round trips through
the one connection every interval, competing with the work the user asked
for, so the interval trades handoff latency against connection time.
`mevedel-view-control-transfer-remote-poll-seconds' governs that case."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-control-transfer-remote-poll-seconds 30
  "Seconds between control-transfer polls when the session lives on a target.

The poll costs nothing worth counting locally and several synchronous round
trips remotely, so the two cases do not want the same cadence.  Connection
time is not the only cost: every command in flight is a window in which a
process sentinel belonging to some other package can issue its own remote
operation on the same connection, which TRAMP then refuses as a reentrant
call.  Polling twelve times a minute holds that window open for no reason.

Only handoff latency is traded away.  A control transfer requested from
another client is noticed within this interval instead of within five
seconds; nothing else observes the cadence."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-view-control-transfer-active-poll-seconds 2
  "Seconds between control-transfer polls while a transfer is in flight.

The idle intervals buy back connection time from a session nobody is trying
to hand over.  A transfer in flight is the opposite case: three separate
waits compose -- the owner noticing the request, the grant deadline, and the
requester noticing the release fence -- so an idle cadence turns a 30 second
handoff into minutes of polling latency.  This interval applies only while a
request is outstanding, and both sides return to their idle cadence as soon
as it settles."
  :type 'number
  :group 'mevedel)

(defun mevedel-view--control-transfer-poll-seconds (session)
  "Return the control-transfer poll interval to use for SESSION."
  (cond
   ((and session
         (memq (plist-get (mevedel-session-control-transfer session) :state)
               '(requested quiescing)))
    mevedel-view-control-transfer-active-poll-seconds)
   ((and session
         (file-remote-p (or (mevedel-session-save-path session)
                            (mevedel-session-working-directory session)
                            "")))
    mevedel-view-control-transfer-remote-poll-seconds)
   (t mevedel-view-control-transfer-poll-seconds)))


;;
;;; State

(defvar-local mevedel-view--control-transfer-timer nil
  "Timer polling cooperative lease-transfer records for this view.")

(defvar-local mevedel-view--control-transfer-drain-session nil
  "Session owning this view's registered control-transfer drain.")

(defvar-local mevedel-view--control-transfer-drain-token nil
  "Registered control-transfer drain predicate for this view.")

(defvar-local mevedel-view--control-transfer-rebuild-function nil
  "Function that asks the generic interaction owner to rebuild this view.")

(defvar-local mevedel-view--session-observer-session nil
  "Session whose semantic events this view observes.")

(defvar-local mevedel-view--session-observer nil
  "Unregister token for this view's semantic session observer.")

(defun mevedel-view--control-transfer-rebuild ()
  "Ask the current view's interaction owner to rebuild."
  (when (functionp mevedel-view--control-transfer-rebuild-function)
    (funcall mevedel-view--control-transfer-rebuild-function)))

(defun mevedel-view--control-transfer-schedule (view)
  "Arm VIEW's next control-transfer poll at the currently apt interval.

The interval is chosen per tick rather than once at setup, because a
transfer in flight wants a cadence the idle session does not."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when (timerp mevedel-view--control-transfer-timer)
        (cancel-timer mevedel-view--control-transfer-timer))
      (let ((interval
             (mevedel-view--control-transfer-poll-seconds
              (and (boundp 'mevedel--data-buffer)
                   (buffer-live-p mevedel--data-buffer)
                   (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer)))))
        (setq-local mevedel-view--control-transfer-timer
                    (run-at-time interval nil
                                 #'mevedel-view--control-transfer-refresh
                                 view))))))

(defun mevedel-view-control-transfer--session-event (view event &rest args)
  "Apply semantic session EVENT to VIEW, when it remains live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (pcase event
        ('save-history
         (require 'mevedel-view-history)
         (mevedel-view-history-save))
        ('load-history
         (require 'mevedel-view-history)
         (mevedel-view-history-load (car args)))
        ('rerender
         (require 'mevedel-view-render)
         (mevedel-view--full-rerender))
        ('rebase-data-sources
         (require 'mevedel-view-render)
         (mevedel-view--rebase-data-sources (car args)))
        ('reset-agent-ephemeral-state
         (require 'mevedel-view-agent)
         (mevedel-view-reset-agent-ephemeral-state))
        ('refresh-status
         (when-let ((data (mevedel-view--control-transfer-data-buffer)))
           (with-current-buffer data
             (when (and buffer-file-name (file-exists-p buffer-file-name))
               (setq buffer-file-truename (file-truename buffer-file-name))
               (set-visited-file-modtime))
             (when (boundp 'mevedel-session--save-failed)
               (setq mevedel-session--save-failed nil)))
           (force-mode-line-update t)))
        ('rename
         (let* ((new-data-name (car args))
                (new-view-name
                 (if (string-match "\\*$" new-data-name)
                     (replace-match ":view*" t t new-data-name)
                   (concat new-data-name ":view"))))
           (rename-buffer new-view-name t)))))))

(defun mevedel-view--control-transfer-data-buffer ()
  "Return the live data buffer for the current view, or nil."
  (and (boundp 'mevedel--data-buffer)
       (buffer-live-p mevedel--data-buffer)
       mevedel--data-buffer))

(defun mevedel-view--control-transfer-refresh (&optional view)
  "Poll transfer state and refresh VIEW's interaction zone.

The timer passes its owning view explicitly; it must not depend on the
ambient current buffer, which may be an unrelated buffer when a timer fires."
  (when (buffer-live-p (or view (current-buffer)))
    (require 'mevedel-session-control-transfer)
    (with-current-buffer (or view (current-buffer))
      (when-let ((data (mevedel-view--control-transfer-data-buffer)))
        (let* ((session (buffer-local-value 'mevedel--session data))
               (read-only-p
                (with-current-buffer data
                  (bound-and-true-p mevedel-session--read-only-mode))))
          (when session
            (when (and read-only-p
                       (ignore-errors
                         (mevedel-session-control-transfer-poll
                          session data t)))
              (require 'mevedel-view-render)
              (mevedel-view--full-rerender))
            (when (not read-only-p)
              (ignore-errors
                (mevedel-session-control-transfer-poll
                 session data nil))))
          (mevedel-view--control-transfer-rebuild)))
      (mevedel-view--control-transfer-schedule (current-buffer)))))


;;
;;; Commands and presentation

(defun mevedel-view-control-transfer-grant ()
  "Grant the currently displayed cooperative control-transfer request."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-decide mevedel--session 'grant))
    (mevedel-view--control-transfer-rebuild)))

(defun mevedel-view-control-transfer-keep ()
  "Reject the currently displayed cooperative control-transfer request."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-decide mevedel--session 'reject))
    (mevedel-view--control-transfer-rebuild)))

(defun mevedel-view-control-transfer-request ()
  "Request control of the current read-only portable session."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-request mevedel--session))
    (message "mevedel: control-transfer request recorded; wait for release")
    (mevedel-view--control-transfer-rebuild)))

(defun mevedel-view--control-transfer-session ()
  "Return the current view's (DATA-BUFFER . SESSION), or signal."
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (let ((session (buffer-local-value 'mevedel--session data)))
      (unless session (user-error "No active mevedel session"))
      (cons data session))))

;;;###autoload
(defun mevedel-take-control ()
  "Take control of the session shown in the current view.

An unheld or expired lease is acquired directly -- there is no owner to ask,
and the lease layer already confirms an expired takeover.  A lease a live
client holds is requested instead; that client grants it automatically once
`mevedel-session-transfer-prompt-timeout' passes, then finishes its work and
releases, so control arrives without anyone sitting at the other machine."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (require 'mevedel-session-durability)
  (pcase-let ((`(,data . ,session) (mevedel-view--control-transfer-session)))
    (unless (buffer-local-value 'mevedel-session--read-only-mode data)
      (user-error "This session is already writable here"))
    (if (eq 'foreign
            (mevedel-session-durability-lease-state
             (mevedel-session-save-path session)))
        (mevedel-view-control-transfer-request)
      (mevedel-session-control-transfer-acquire session data)
      (require 'mevedel-view-render)
      (mevedel-view--full-rerender)
      (mevedel-view--control-transfer-rebuild))))

;;;###autoload
(defun mevedel-release-control ()
  "Hand this session's lease back and keep watching it read-only.

The session is saved and published before the lease goes, so whoever takes
it next starts from the work done here.  Live work blocks the release for
the same reason a granted transfer waits for it."
  (interactive)
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-codec)
  (require 'mevedel-session-artifacts)
  (require 'mevedel-session-control-transfer)
  (require 'mevedel-session-durability)
  (pcase-let ((`(,data . ,session) (mevedel-view--control-transfer-session)))
    (when (buffer-local-value 'mevedel-session--read-only-mode data)
      (user-error "This session is already read-only here"))
    (when-let ((blocker
                (mevedel-session-control-transfer-drain-blocker session)))
      (user-error "Cannot release control while %s is outstanding" blocker))
    (mevedel-session-artifacts-save session data t)
    (mevedel-session-durability-lease-release
     (mevedel-session-save-path session) session)
    (mevedel-session-persistence-apply-read-only-mode
     data "control released; following the new owner from here")
    (mevedel-view--control-transfer-rebuild)))

;;;###autoload
(defun mevedel-toggle-follow ()
  "Toggle whether this non-owner view follows the owner's published turns."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (pcase-let ((`(,data . ,_session) (mevedel-view--control-transfer-session)))
    (let ((enabled
           (with-current-buffer data
             (setq-local mevedel-session-follow-published
                         (not mevedel-session-follow-published)))))
      (mevedel-view--control-transfer-rebuild)
      (message "mevedel: following published turns %s"
               (if enabled "on" "off")))))

;;;###autoload
(defun mevedel-refresh-session ()
  "Re-read the owner's newest published state into this read-only view.

This is what following does on a timer, run now and regardless of whether
this view follows."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (pcase-let ((`(,data . ,session) (mevedel-view--control-transfer-session)))
    (unless (buffer-local-value 'mevedel-session--read-only-mode data)
      (user-error "This session is writable here; there is nothing to follow"))
    (if (mevedel-session-control-transfer--follow-published session data t)
        (progn
          (require 'mevedel-view-render)
          (mevedel-view--full-rerender)
          (mevedel-view--control-transfer-rebuild)
          (message "mevedel: advanced to the owner's newest published state"))
      (message "mevedel: already at the owner's newest published state"))))

(defun mevedel-view--control-transfer-body (descriptor)
  "Return the rendered interaction body for control-transfer DESCRIPTOR."
  (require 'mevedel-interaction-prompt)
  (let* ((attention (plist-get descriptor :attention))
         (detail (plist-get descriptor :detail))
         (keys (plist-get descriptor :keys)))
    (mevedel--prompt-framed-body
     (concat
      (propertize (plist-get descriptor :title)
                  'font-lock-face
                  (if attention
                      '(:inherit warning :weight bold)
                    '(:inherit mevedel-view-header :weight bold)))
      "\n"
      (when detail
        (concat (propertize detail 'font-lock-face 'shadow) "\n"))
      (when keys
        (concat
         (mapconcat (lambda (entry)
                      (concat (mevedel--prompt-key (car entry))
                              " " (cdr entry)))
                    keys "   ")
         "\n")))
     (if attention 'warning 'shadow))))

(defvar-keymap mevedel-view--control-transfer-status-map
  :doc "Inert keymap for a control-transfer status line.")

(defvar-keymap mevedel-view--control-transfer-map
  :doc "Keymap for cooperative lease-transfer controls."
  "g" #'mevedel-view-control-transfer-grant
  "k" #'mevedel-view-control-transfer-keep)

(defvar-keymap mevedel-view--control-transfer-request-map
  :doc "Keymap for requesting cooperative control from read-only mode."
  "r" #'mevedel-view-control-transfer-request)

(defun mevedel-view-control-transfer-current-descriptor ()
  "Return the current view's complete control-transfer descriptor, or nil."
  (require 'mevedel-session-control-transfer)
  (when-let* ((data (mevedel-view--control-transfer-data-buffer))
              (session (buffer-local-value 'mevedel--session data))
              (descriptor
               (mevedel-session-control-transfer-descriptor
                session
                (buffer-local-value 'mevedel-session--read-only-mode data))))
    (append descriptor
            (list :id 'control-transfer
                  :body (mevedel-view--control-transfer-body descriptor)
                  :keymap
                  (pcase (plist-get descriptor :action)
                    ('grant mevedel-view--control-transfer-map)
                    ('request mevedel-view--control-transfer-request-map)
                    (_ mevedel-view--control-transfer-status-map))
                  :origin "/root"))))


;;
;;; View lifecycle

(defun mevedel-view-control-transfer-teardown ()
  "Remove transfer registrations and cancel the current view's poll timer."
  (when (and mevedel-view--session-observer-session
             mevedel-view--session-observer)
    (mevedel-session-control-transfer-unregister-observer
     mevedel-view--session-observer-session
     mevedel-view--session-observer)
    (setq mevedel-view--session-observer-session nil
          mevedel-view--session-observer nil))
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer)
             (not mevedel-view--agent-transcript-p))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer)))
      (when session
        (mevedel-session-control-transfer-unregister-presentation
         session (current-buffer))
        (mevedel-session-control-transfer-unregister-root-buffer
         session mevedel--data-buffer))))
  (when (and mevedel-view--control-transfer-drain-session
             mevedel-view--control-transfer-drain-token)
    (mevedel-session-control-transfer-unregister-drain
     mevedel-view--control-transfer-drain-session
     mevedel-view--control-transfer-drain-token)
    (setq mevedel-view--control-transfer-drain-session nil
          mevedel-view--control-transfer-drain-token nil))
  (when (timerp mevedel-view--control-transfer-timer)
    (cancel-timer mevedel-view--control-transfer-timer)
    (setq mevedel-view--control-transfer-timer nil))
  (setq mevedel-view--control-transfer-rebuild-function nil))

(defun mevedel-view-control-transfer-initialize (rebuild-function drain-predicate)
  "Initialize transfer UI using REBUILD-FUNCTION and DRAIN-PREDICATE."
  (require 'mevedel-session-control-transfer)
  (mevedel-view-control-transfer-teardown)
  (setq-local mevedel-view--control-transfer-rebuild-function rebuild-function)
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer)
             (not mevedel-view--agent-transcript-p))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer))
          (view (current-buffer)))
      (when session
        (mevedel-session-control-transfer-register-root-buffer
         session mevedel--data-buffer)
        (mevedel-session-control-transfer-register-presentation session view)
        (setq-local
         mevedel-view--session-observer-session session
         mevedel-view--session-observer
         (mevedel-session-control-transfer-register-observer
          session
          (lambda (event &rest args)
            (apply #'mevedel-view-control-transfer--session-event
                   view event args)))
         mevedel-view--control-transfer-drain-session session
         mevedel-view--control-transfer-drain-token
         (mevedel-session-control-transfer-register-drain
          session drain-predicate))))
    (mevedel-view--control-transfer-schedule (current-buffer))))

(provide 'mevedel-view-control-transfer)

;;; mevedel-view-control-transfer.el ends here
