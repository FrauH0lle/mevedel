;;; niwa-view-control-transfer.el -- View control-transfer UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns cooperative control-transfer polling, presentation, commands, and
;; view registration.  The generic interaction owner supplies redraw and
;; drain callbacks; durable lease and transfer state remain session-owned.

;;; Code:

;; `niwa-execution'
(declare-function niwa-execution-stop-services "niwa-execution" (session))
(autoload 'niwa-execution-stop-services "niwa-execution")

;; `niwa-interaction-prompt'
(declare-function niwa--prompt-framed-body
                  "niwa-interaction-prompt" (content face))
(declare-function niwa--prompt-key "niwa-interaction-prompt" (key))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-save
                  "niwa-session-artifacts"
                  (session buffer &optional settled force))
(autoload 'niwa-session-artifacts-save "niwa-session-artifacts")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer--follow-published
                  "niwa-session-control-transfer"
                  (session buffer &optional force))
(declare-function niwa-session-control-transfer-acquire
                  "niwa-session-control-transfer" (session buffer))
(declare-function niwa-session-control-transfer-decide
                  "niwa-session-control-transfer" (session decision))
(declare-function niwa-session-control-transfer-descriptor
                  "niwa-session-control-transfer" (session read-only-p))
(declare-function niwa-session-control-transfer-drain-blocker
                  "niwa-session-control-transfer" (session))
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(autoload 'niwa-session-codec-portable-authority-p "niwa-session-codec")
(declare-function niwa-session-control-transfer-poll
                  "niwa-session-control-transfer"
                  (session buffer read-only-p))
(declare-function niwa-session-control-transfer-register-drain
                  "niwa-session-control-transfer" (session predicate))
(declare-function niwa-session-control-transfer-register-observer
                  "niwa-session-control-transfer" (session observer))
(declare-function niwa-session-control-transfer-register-root-buffer
                  "niwa-session-control-transfer" (session buffer))
(declare-function niwa-session-control-transfer-request
                  "niwa-session-control-transfer" (session))
(declare-function niwa-session-control-transfer-take-over
                  "niwa-session-control-transfer" (session buffer))
(declare-function niwa-session-control-transfer-unregister-drain
                  "niwa-session-control-transfer" (session predicate))
(declare-function niwa-session-control-transfer-unregister-observer
                  "niwa-session-control-transfer" (session observer))
(declare-function niwa-session-control-transfer-unregister-root-buffer
                  "niwa-session-control-transfer" (session buffer))
(defvar niwa-session-follow-published)

;; `niwa-session-durability'
(declare-function niwa-session-durability-lease-release
                  "niwa-session-durability"
                  (session-dir &optional session))
(declare-function niwa-session-durability-lease-state
                  "niwa-session-durability" (session-dir))
(autoload 'niwa-session-durability-lease-release
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-state
  "niwa-session-durability")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-apply-read-only-mode
                  "niwa-session-persistence" (buf &optional reason))
(autoload 'niwa-session-persistence-apply-read-only-mode
  "niwa-session-persistence")
(defvar niwa-session--read-only-mode)
(defvar niwa-session--save-failed)

;; `niwa-structs'
(declare-function niwa-session-control-transfer
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--session)

;; `niwa-view-execution'
(declare-function niwa-view-execution-reset "niwa-view-execution" ())

;; `niwa-view-history'
(declare-function niwa-view-history-load "niwa-view-history"
                  (&optional session))
(declare-function niwa-view-history-save "niwa-view-history"
                  (&optional view-buffer))
(autoload 'niwa-view-history-load "niwa-view-history")
(autoload 'niwa-view-history-save "niwa-view-history")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(autoload 'niwa-view-rerender "niwa-view")

;; `niwa-view-render'
(declare-function niwa-view--rebase-data-sources "niwa-view-render"
                  (mapping))
(autoload 'niwa-view--rebase-data-sources "niwa-view-render")

(require 'niwa-interaction-prompt)
(require 'niwa-session-control-transfer)
(require 'niwa-structs)


;;
;;; Customization

(defcustom niwa-view-control-transfer-poll-seconds 5
  "Seconds between cooperative control-transfer polls in a view.

Each poll reads the durable lease head, the target clock, and the transfer
mailbox.  On a remote target that is several synchronous round trips through
the one connection every interval, competing with the work the user asked
for, so the interval trades handoff latency against connection time.
`niwa-view-control-transfer-remote-poll-seconds' governs that case."
  :type 'number
  :group 'niwa)

(defcustom niwa-view-control-transfer-remote-poll-seconds 30
  "Seconds between control-transfer polls when the session lives on a target.

A local owner skips the target program while its request mailbox is
unchanged, and a remote poll costs several synchronous round trips, so the
two cases do not want the same cadence.  Connection
time is not the only cost: every command in flight is a window in which a
process sentinel belonging to some other package can issue its own remote
operation on the same connection, which TRAMP then refuses as a reentrant
call.  Polling twelve times a minute holds that window open for no reason.

Only handoff latency is traded away.  A control transfer requested from
another client is noticed within this interval instead of within five
seconds; nothing else observes the cadence."
  :type 'number
  :group 'niwa)

(defcustom niwa-view-control-transfer-active-poll-seconds 2
  "Seconds between control-transfer polls while a transfer is in flight.

The idle intervals buy back connection time from a session nobody is trying
to hand over.  A transfer in flight is the opposite case: three separate
waits compose -- the owner noticing the request, the grant deadline, and the
requester noticing the release fence -- so an idle cadence turns a 30 second
handoff into minutes of polling latency.  This interval applies only while a
request is outstanding, and both sides return to their idle cadence as soon
as it settles."
  :type 'number
  :group 'niwa)

(defun niwa-view--control-transfer-poll-seconds (session)
  "Return the control-transfer poll interval to use for SESSION."
  (cond
   ((and session
         (memq (plist-get (niwa-session-control-transfer session) :state)
               '(requested quiescing)))
    niwa-view-control-transfer-active-poll-seconds)
   ((and session
         (file-remote-p (or (niwa-session-save-path session)
                            (niwa-session-working-directory session)
                            "")))
    niwa-view-control-transfer-remote-poll-seconds)
   (t niwa-view-control-transfer-poll-seconds)))


;;
;;; State

(defvar-local niwa-view--control-transfer-timer nil
  "Timer polling cooperative lease-transfer records for this view.")

(defvar-local niwa-view--control-transfer-torn-down-p nil
  "Non-nil once this view's transfer machinery has been torn down.
Teardown runs from the kill hook while the buffer is still live, and a
session event delivered during it must not re-arm the poll timer: that
timer would outlive the buffer as a stray wakeup holding a dead view.")

(defvar-local niwa-view--control-transfer-drain-session nil
  "Session owning this view's registered control-transfer drain.")

(defvar-local niwa-view--control-transfer-drain-token nil
  "Registered control-transfer drain predicate for this view.")

(defvar-local niwa-view--control-transfer-descriptor 'unset
  "Transfer descriptor this view's interaction zone last showed.")

(defvar-local niwa-view--control-transfer-rebuild-function nil
  "Function that asks the generic interaction owner to rebuild this view.")

(defvar-local niwa-view--session-observer-session nil
  "Session whose semantic events this view observes.")

(defvar-local niwa-view--session-observer nil
  "Unregister token for this view's semantic session observer.")

(defun niwa-view--control-transfer-rebuild ()
  "Ask the current view's interaction owner to rebuild."
  (when (functionp niwa-view--control-transfer-rebuild-function)
    (funcall niwa-view--control-transfer-rebuild-function)))

(defun niwa-view--control-transfer-schedule (view)
  "Arm VIEW's next control-transfer poll at the currently apt interval.

The interval is chosen per tick rather than once at setup, because a
transfer in flight wants a cadence the idle session does not."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when (timerp niwa-view--control-transfer-timer)
        (cancel-timer niwa-view--control-transfer-timer))
      ;; Only portable sessions transfer control; another session's poll
      ;; signals, and re-arming it woke the view every few seconds forever.
      (unless (or niwa-view--control-transfer-torn-down-p
                  (and-let* ((data (and (boundp 'niwa--data-buffer)
                                        (buffer-live-p niwa--data-buffer)
                                        niwa--data-buffer))
                             (session (buffer-local-value 'niwa--session data)))
                    (condition-case nil
                        (not (niwa-session-codec-portable-authority-p session))
                      (error nil))))
        (let ((interval
               (niwa-view--control-transfer-poll-seconds
                (and (boundp 'niwa--data-buffer)
                     (buffer-live-p niwa--data-buffer)
                     (buffer-local-value 'niwa--session
                                         niwa--data-buffer)))))
          (setq-local niwa-view--control-transfer-timer
                      (run-at-time interval nil
                                   #'niwa-view--control-transfer-refresh
                                   view)))))))

(defun niwa-view-control-transfer--session-event (view event &rest args)
  "Apply semantic session EVENT to VIEW, when it remains live."
  (when (buffer-live-p view)
    (with-current-buffer view
      (pcase event
        ('save-history
         (niwa-view-history-save))
        ('load-history
         (niwa-view-history-load (car args)))
        ('rerender
         (niwa-view-rerender))
        ('rebase-data-sources
         (niwa-view--rebase-data-sources (car args)))
        ('reset-status-panel
         (niwa-view-execution-reset))
        ('refresh-status
         (when-let* ((data (niwa-view--control-transfer-data-buffer)))
           (with-current-buffer data
             (when (and buffer-file-name (file-exists-p buffer-file-name))
               (setq buffer-file-truename (file-truename buffer-file-name))
               (set-visited-file-modtime))
             (when (boundp 'niwa-session--save-failed)
               (setq niwa-session--save-failed nil)))
           (force-mode-line-update t)))
        ('rename
         (let* ((new-data-name (car args))
                (new-view-name
                 (if (string-match "\\*$" new-data-name)
                     (replace-match ":view*" t t new-data-name)
                   (concat new-data-name ":view"))))
           (rename-buffer new-view-name t)))))))

(defun niwa-view--control-transfer-data-buffer ()
  "Return the live data buffer for the current view, or nil."
  (and (boundp 'niwa--data-buffer)
       (buffer-live-p niwa--data-buffer)
       niwa--data-buffer))

(defun niwa-view--control-transfer-refresh (&optional view)
  "Poll transfer state and refresh VIEW's interaction zone.

The timer passes its owning view explicitly; it must not depend on the
ambient current buffer, which may be an unrelated buffer when a timer fires."
  (when (buffer-live-p (or view (current-buffer)))
    (with-current-buffer (or view (current-buffer))
      (when-let* ((data (niwa-view--control-transfer-data-buffer)))
        (let* ((session (buffer-local-value 'niwa--session data))
               (read-only-p
                (with-current-buffer data
                  (bound-and-true-p niwa-session--read-only-mode))))
          (when session
            (when (and read-only-p
                       (ignore-errors
                         (niwa-session-control-transfer-poll
                          session data t)))
              (niwa-view-rerender))
            (when (not read-only-p)
              (ignore-errors
                (niwa-session-control-transfer-poll
                 session data nil))))
          (setq read-only-p
                (buffer-local-value 'niwa-session--read-only-mode data))
          ;; The zone shows only this descriptor; rebuilding it unchanged
          ;; every poll allocated about 180 KB per view every five seconds.
          (let ((descriptor
                 (and session
                      (niwa-session-control-transfer-descriptor
                       session read-only-p))))
            (unless (equal descriptor niwa-view--control-transfer-descriptor)
              (setq niwa-view--control-transfer-descriptor descriptor)
              (niwa-view--control-transfer-rebuild)))))
      (niwa-view--control-transfer-schedule (current-buffer)))))


;;
;;; Commands and presentation

(defun niwa-view-control-transfer-grant ()
  "Grant the currently displayed cooperative control-transfer request."
  (interactive)
  (let ((data (niwa-view--control-transfer-data-buffer)))
    (unless data (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session"))
    (with-current-buffer data
      (niwa-session-control-transfer-decide niwa--session 'grant))
    (niwa-view--control-transfer-rebuild)))

(defun niwa-view-control-transfer-keep ()
  "Reject the currently displayed cooperative control-transfer request."
  (interactive)
  (let ((data (niwa-view--control-transfer-data-buffer)))
    (unless data (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session"))
    (with-current-buffer data
      (niwa-session-control-transfer-decide niwa--session 'reject))
    (niwa-view--control-transfer-rebuild)))

(defun niwa-view-control-transfer-request ()
  "Request control of the current read-only portable session."
  (interactive)
  (let ((data (niwa-view--control-transfer-data-buffer)))
    (unless data (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session"))
    (with-current-buffer data
      (niwa-session-control-transfer-request niwa--session))
    (message "niwa: control-transfer request recorded; wait for release")
    (niwa-view--control-transfer-rebuild)))

(defun niwa-view--control-transfer-session ()
  "Return the current view's (DATA-BUFFER . SESSION), or signal."
  (let ((data (niwa-view--control-transfer-data-buffer)))
    (unless data (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session"))
    (let ((session (buffer-local-value 'niwa--session data)))
      (unless session (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No active niwa session"))
      (cons data session))))

;;;###autoload
(defun niwa-take-control ()
  "Take control of the session shown in the current view.

An unheld or expired lease is acquired directly -- there is no owner to ask,
and the lease layer already confirms an expired takeover.  A lease a live
client holds is requested instead; that client grants it automatically once
`niwa-session-transfer-prompt-timeout' passes, then finishes its work and
releases, so control arrives without anyone sitting at the other machine."
  (interactive)
  (pcase-let ((`(,data . ,session) (niwa-view--control-transfer-session)))
    (unless (buffer-local-value 'niwa-session--read-only-mode data)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This session is already writable here"))
    (if (eq 'foreign
            (niwa-session-durability-lease-state
             (niwa-session-save-path session)))
      (niwa-view-control-transfer-request)
      (niwa-session-control-transfer-acquire session data)
      (niwa-view-rerender)
      (niwa-view--control-transfer-rebuild))))

(defun niwa-view-control-transfer-take-over ()
  "Confirm taking the current read-only session from its live holder."
  (interactive)
  (pcase-let ((`(,data . ,session) (niwa-view--control-transfer-session)))
    (unless (buffer-local-value 'niwa-session--read-only-mode data)
      (niwa-diagnostic-user-error
       (niwa-diagnostic-scope session) "This session is already writable here"))
    (niwa-session-control-transfer-take-over session data)
    (niwa-view-rerender)
    (niwa-view--control-transfer-rebuild)))

;;;###autoload
(defun niwa-release-control ()
  "Hand this session's lease back and keep watching it read-only.

The session is saved and published before the lease goes, so whoever takes
it next starts from the work done here.  Live work blocks the release for
the same reason a granted transfer waits for it."
  (interactive)
  (pcase-let ((`(,data . ,session) (niwa-view--control-transfer-session)))
    (when (buffer-local-value 'niwa-session--read-only-mode data)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This session is already read-only here"))
    (when-let* ((blocker
                (niwa-session-control-transfer-drain-blocker session)))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Cannot release control while %s is outstanding" blocker))
    (niwa-session-artifacts-save session data t)
    (niwa-execution-stop-services session)
    (niwa-session-durability-lease-release
     (niwa-session-save-path session) session)
    (niwa-session-persistence-apply-read-only-mode
     data "control released; following the new owner from here")
    (niwa-view--control-transfer-rebuild)))

;;;###autoload
(defun niwa-toggle-follow ()
  "Toggle whether this non-owner view follows the owner's published turns."
  (interactive)
  (pcase-let ((`(,data . ,_session) (niwa-view--control-transfer-session)))
    (let ((enabled
           (with-current-buffer data
             (setq-local niwa-session-follow-published
                         (not niwa-session-follow-published)))))
      (niwa-view--control-transfer-rebuild)
      (message "niwa: following published turns %s"
               (if enabled "on" "off")))))

;;;###autoload
(defun niwa-refresh-session ()
  "Re-read the owner's newest published state into this read-only view.

This is what following does on a timer, run now and regardless of whether
this view follows."
  (interactive)
  (pcase-let ((`(,data . ,session) (niwa-view--control-transfer-session)))
    (unless (buffer-local-value 'niwa-session--read-only-mode data)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This session is writable here; there is nothing to follow"))
    (if (niwa-session-control-transfer--follow-published session data t)
        (progn
          (niwa-view-rerender)
          (niwa-view--control-transfer-rebuild)
          (message "niwa: advanced to the owner's newest published state"))
      (message "niwa: already at the owner's newest published state"))))

(defun niwa-view--control-transfer-body (descriptor)
  "Return the rendered interaction body for control-transfer DESCRIPTOR."
  (let* ((attention (plist-get descriptor :attention))
         (detail (plist-get descriptor :detail))
         (keys (plist-get descriptor :keys)))
    (niwa--prompt-framed-body
     (concat
      (propertize (plist-get descriptor :title)
                  'font-lock-face
                  (if attention
                      '(:inherit warning :weight bold)
                    '(:inherit niwa-view-header :weight bold)))
      "\n"
      (when detail
        (concat (propertize detail 'font-lock-face 'shadow) "\n"))
      (when keys
        (concat
         (mapconcat (lambda (entry)
                      (concat (niwa--prompt-key (car entry))
                              " " (cdr entry)))
                    keys "   ")
         "\n")))
     (if attention 'warning 'shadow))))

(defvar-keymap niwa-view--control-transfer-status-map
  :doc "Inert keymap for a control-transfer status line.")

(defvar-keymap niwa-view--control-transfer-take-over-map
  :doc "Keymap for taking over while a cooperative request waits."
  "b" #'niwa-view-control-transfer-take-over)

(defvar-keymap niwa-view--control-transfer-map
  :doc "Keymap for cooperative lease-transfer controls."
  "g" #'niwa-view-control-transfer-grant
  "k" #'niwa-view-control-transfer-keep)

(defvar-keymap niwa-view--control-transfer-request-map
  :doc "Keymap for taking control from read-only mode."
  "r" #'niwa-take-control
  "b" #'niwa-view-control-transfer-take-over)

(defun niwa-view-control-transfer-current-descriptor ()
  "Return the current view's complete control-transfer descriptor, or nil."
  (when-let* ((data (niwa-view--control-transfer-data-buffer))
              (session (buffer-local-value 'niwa--session data))
              (descriptor
               (niwa-session-control-transfer-descriptor
                session
                (buffer-local-value 'niwa-session--read-only-mode data))))
    (append descriptor
            (list :id 'control-transfer
                  :body (niwa-view--control-transfer-body descriptor)
                  :keymap
                  (pcase (plist-get descriptor :action)
                    ('grant niwa-view--control-transfer-map)
                    ('request niwa-view--control-transfer-request-map)
                    ('status (if (plist-get descriptor :keys)
                                 niwa-view--control-transfer-take-over-map
                               niwa-view--control-transfer-status-map))
                    (_ niwa-view--control-transfer-status-map))
                  :origin "/root"))))


;;
;;; View lifecycle

(defun niwa-view-control-transfer-stop-polling ()
  "Stop the current view's polling while retaining its registrations.
Only initialization may re-arm polling after this call.  A closing view
can still use its root registration to seal journal work before teardown."
  (when (timerp niwa-view--control-transfer-timer)
    (cancel-timer niwa-view--control-transfer-timer))
  (setq niwa-view--control-transfer-timer nil
        niwa-view--control-transfer-torn-down-p t))

(defun niwa-view-control-transfer-teardown ()
  "Remove transfer registrations and cancel the current view's poll timer."
  (when (and niwa-view--session-observer-session
             niwa-view--session-observer)
    (niwa-session-control-transfer-unregister-observer
     niwa-view--session-observer-session
     niwa-view--session-observer)
    (setq niwa-view--session-observer-session nil
          niwa-view--session-observer nil))
  (when (and (boundp 'niwa--data-buffer)
             (buffer-live-p niwa--data-buffer)
             (not niwa-view--agent-transcript-p))
    (let ((session (buffer-local-value 'niwa--session
                                       niwa--data-buffer)))
      (when session
        (niwa-session-control-transfer-unregister-root-buffer
         session niwa--data-buffer))))
  (when (and niwa-view--control-transfer-drain-session
             niwa-view--control-transfer-drain-token)
    (niwa-session-control-transfer-unregister-drain
     niwa-view--control-transfer-drain-session
     niwa-view--control-transfer-drain-token)
    (setq niwa-view--control-transfer-drain-session nil
          niwa-view--control-transfer-drain-token nil))
  (niwa-view-control-transfer-stop-polling)
  (setq niwa-view--control-transfer-rebuild-function nil))

(defun niwa-view-control-transfer-initialize (rebuild-function drain-predicate)
  "Initialize transfer UI using REBUILD-FUNCTION and DRAIN-PREDICATE."
  (niwa-view-control-transfer-teardown)
  ;; Initialization is the one legitimate re-arm after a teardown.
  (setq-local niwa-view--control-transfer-torn-down-p nil)
  (setq-local niwa-view--control-transfer-descriptor 'unset)
  (setq-local niwa-view--control-transfer-rebuild-function rebuild-function)
  (when (and (boundp 'niwa--data-buffer)
             (buffer-live-p niwa--data-buffer)
             (not niwa-view--agent-transcript-p))
    (let ((session (buffer-local-value 'niwa--session
                                       niwa--data-buffer))
          (view (current-buffer)))
      (when session
        (niwa-session-control-transfer-register-root-buffer
         session niwa--data-buffer)
        (setq-local
         niwa-view--session-observer-session session
         niwa-view--session-observer
         (niwa-session-control-transfer-register-observer
          session
          (lambda (event &rest args)
            (apply #'niwa-view-control-transfer--session-event
                   view event args)))
         niwa-view--control-transfer-drain-session session
         niwa-view--control-transfer-drain-token
         (niwa-session-control-transfer-register-drain
          session drain-predicate))))
    (niwa-view--control-transfer-schedule (current-buffer))))

(provide 'niwa-view-control-transfer)

;;; niwa-view-control-transfer.el ends here
