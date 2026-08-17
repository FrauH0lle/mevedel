;;; mevedel-view-interaction.el --- View interaction-zone UI -*- lexical-binding: t -*-

;;; Commentary:

;; Owns interaction descriptor registration, ordering, anchoring, and redraw.
;; Domain prompt modules retain callback settlement and outcome semantics.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-block-turn
		  "mevedel-agent-control" (session path activity))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-parent-data-buffer
		  "mevedel-agents" (cl-x) t)

;; `mevedel-interaction-prompt'
(defvar mevedel--prompt-overlays)

;; `mevedel-permission-queue'
(declare-function mevedel-permission-queue--render-head
		  "mevedel-permission-queue" (&optional session))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-effective
		  "mevedel-permissions"
		  (&optional session data-buffer surface-buffer))

;; `mevedel-plan-mode'
(declare-function mevedel-plan-approval-render "mevedel-plan-mode"
		  (&optional session))

;; `mevedel-structs'
(declare-function mevedel-request-p "mevedel-structs" (cl-x))
(declare-function mevedel-request-set-active-work-paused
                  "mevedel-structs" (request paused &optional now))
(declare-function mevedel-session-pending-plan-approval
		  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-permission-queue "mevedel-structs"
		  (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-control-transfer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x))
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--data-buffer)
(defvar mevedel--session)
(defvar mevedel--view-buffer)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record "mevedel-telemetry"
		  (session event &rest props))

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text "mevedel-utilities"
		  (text))

;; `mevedel-view'
(declare-function mevedel-view--display-fragment-keymap "mevedel-view"
			  (&rest maps))
(declare-function mevedel-view--full-rerender "mevedel-view" nil)
(declare-function mevedel-view--status-anchor "mevedel-view" nil)
(declare-function mevedel-view--zone-separator "mevedel-view" (label))
(defvar mevedel-view--interaction-marker)

;; `mevedel-view-agent'
(defvar mevedel-view--agent-transcript-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
		  "mevedel-view-composer" nil)
(declare-function mevedel-view--pending-inputs-render
                  "mevedel-view-composer" (&optional session))
(declare-function mevedel-view--session "mevedel-view-composer" nil)
(defvar mevedel-view--prompt-hook-pending)
(defvar mevedel-session--read-only-mode)

;; `mevedel-session-control-transfer'
(declare-function mevedel-session-control-transfer--follow-published
                  "mevedel-session-control-transfer"
                  (session buffer &optional force))
(declare-function mevedel-session-control-transfer-acquire
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-decide
                  "mevedel-session-control-transfer" (session decision))
(declare-function mevedel-session-control-transfer-drain-blocker
                  "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-descriptor
                  "mevedel-session-control-transfer" (session read-only-p))
(declare-function mevedel-session-control-transfer-poll
                  "mevedel-session-control-transfer"
                  (session buffer read-only-p))
(declare-function mevedel-session-control-transfer-register-drain
                  "mevedel-session-control-transfer" (session predicate))
(declare-function mevedel-session-control-transfer-register-observer
                  "mevedel-session-control-transfer" (session observer))
(declare-function mevedel-session-control-transfer-request
                  "mevedel-session-control-transfer" (session))
(declare-function mevedel-session-control-transfer-unregister-drain
                  "mevedel-session-control-transfer" (session predicate))
(declare-function mevedel-session-control-transfer-unregister-observer
                  "mevedel-session-control-transfer" (session observer))
(declare-function mevedel-session-control-transfer-register-root-buffer
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-register-presentation
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-unregister-root-buffer
                  "mevedel-session-control-transfer" (session buffer))
(declare-function mevedel-session-control-transfer-unregister-presentation
                  "mevedel-session-control-transfer" (session buffer))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-lease-release
                  "mevedel-session-durability"
                  (session-dir &optional session))
(declare-function mevedel-session-durability-lease-state
                  "mevedel-session-durability" (session-dir))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--apply-read-only-mode
                  "mevedel-session-persistence" (buffer &optional reason))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence"
                  (session buffer &optional settled))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-framed-body
                  "mevedel-interaction-prompt" (content face))
(declare-function mevedel--prompt-key "mevedel-interaction-prompt" (key))

;; `mevedel-view-render'
(declare-function mevedel-view--debug-log "mevedel-view-render"
                  (event &rest data))
(declare-function mevedel-view--rebase-data-sources "mevedel-view-render"
                  (mapping))
(declare-function mevedel-view--debug-state "mevedel-view-render"
                  (&optional data-buf start end))

;; `mevedel-view-history'
(declare-function mevedel-view-history-load "mevedel-view-history"
                  (&optional session))
(declare-function mevedel-view-history-save "mevedel-view-history"
                  (&optional view-buffer))

;; `mevedel-view-stream'
(declare-function mevedel-view--render-request-progress
                  "mevedel-view-stream" nil)
(declare-function mevedel-view--request-progress-region-start
		  "mevedel-view-stream" nil)

;; `mevedel-view-zone'
(declare-function mevedel-view-zone-fragment-bounds
		  "mevedel-view-zone" (namespace id))
(declare-function mevedel-view-zone-reconcile "mevedel-view-zone"
		  (namespace start end fragments))
(declare-function mevedel-view-zone-region "mevedel-view-zone"
		  (namespace))

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

(defvar-local mevedel-view--interaction-descriptors nil
  "Hash table of live interaction-zone descriptors keyed by descriptor id.")

(defvar-local mevedel-view--interaction-overlays nil
  "Hash table of live interaction-zone overlays keyed by descriptor id.")

(defvar-local mevedel-view--interaction-telemetry-opened nil
  "Hash table of interaction lifecycle metadata retained across redraws.")

(defvar-local mevedel-view--control-transfer-timer nil
  "Timer polling cooperative lease-transfer records for this view.")

(defvar-local mevedel-view--control-transfer-drain-session nil
  "Session owning this view's registered control-transfer drain.")

(defvar-local mevedel-view--control-transfer-drain-token nil
  "Registered control-transfer drain predicate for this view.")

(defvar-local mevedel-view--session-observer-session nil
  "Session whose semantic events this view observes.")

(defvar-local mevedel-view--session-observer nil
  "Unregister token for this view's semantic session observer.")

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

(defun mevedel-view-interaction--session-event (view event &rest args)
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
         (mevedel-view--full-rerender))
        ('rebase-data-sources
         (require 'mevedel-view-render)
         (mevedel-view--rebase-data-sources (car args)))
        ('reset-agent-ephemeral-state
         (when (fboundp 'mevedel-view-reset-agent-ephemeral-state)
           (mevedel-view-reset-agent-ephemeral-state)))
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
              ;; Acquiring control and advancing to a newer publication both
              ;; replace the transcript.  The coordinator enables writes only
              ;; after the target incarnation check and committed restore.
              (mevedel-view--full-rerender))
            (when (not read-only-p)
              (ignore-errors
                (mevedel-session-control-transfer-poll
                 session data nil))))
          (mevedel-view--interaction-rebuild)))
      (mevedel-view--control-transfer-schedule (current-buffer)))))

(defun mevedel-view-control-transfer-grant ()
  "Grant the currently displayed cooperative control-transfer request."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-decide mevedel--session 'grant))
    (mevedel-view--interaction-rebuild)))

(defun mevedel-view-control-transfer-keep ()
  "Reject the currently displayed cooperative control-transfer request."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-decide mevedel--session 'reject))
    (mevedel-view--interaction-rebuild)))

(defun mevedel-view-control-transfer-request ()
  "Request control of the current read-only portable session."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (let ((data (mevedel-view--control-transfer-data-buffer)))
    (unless data (user-error "No active mevedel session"))
    (with-current-buffer data
      (mevedel-session-control-transfer-request mevedel--session))
    (message "mevedel: control-transfer request recorded; wait for release")
    (mevedel-view--interaction-rebuild)))

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
      (mevedel-view--full-rerender)
      (mevedel-view--interaction-rebuild))))

;;;###autoload
(defun mevedel-release-control ()
  "Hand this session's lease back and keep watching it read-only.

The session is saved and published before the lease goes, so whoever takes
it next starts from the work done here.  Live work blocks the release for
the same reason a granted transfer waits for it."
  (interactive)
  (require 'mevedel-session-control-transfer)
  (require 'mevedel-session-durability)
  (require 'mevedel-session-persistence)
  (pcase-let ((`(,data . ,session) (mevedel-view--control-transfer-session)))
    (when (buffer-local-value 'mevedel-session--read-only-mode data)
      (user-error "This session is already read-only here"))
    (when-let ((blocker
                (mevedel-session-control-transfer-drain-blocker session)))
      (user-error "Cannot release control while %s is outstanding" blocker))
    (mevedel-session-persistence-save session data t)
    (mevedel-session-durability-lease-release
     (mevedel-session-save-path session) session)
    (mevedel-session-persistence--apply-read-only-mode
     data "Control released; this session is now read-only here")
    (mevedel-view--interaction-rebuild)
    (message "mevedel: control released; following the new owner")))

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
      (mevedel-view--interaction-rebuild)
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
          (mevedel-view--full-rerender)
          (mevedel-view--interaction-rebuild)
          (message "mevedel: advanced to the owner's newest published state"))
      (message "mevedel: already at the owner's newest published state"))))

(defun mevedel-view--control-transfer-body (descriptor)
  "Return the rendered interaction body for control-transfer DESCRIPTOR.

Control transfer is one of the few things in the view that takes the session
away from the user, so it is framed like a permission prompt rather than
printed as a line of transcript.  The states waiting on a person are framed
in `warning\='; the ones merely reporting protocol progress are not, because
a permanent read-only banner in the same colour as a live decision teaches
the user to stop seeing both."
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
  "k" #'mevedel-view-control-transfer-keep
  "r" #'mevedel-view-control-transfer-request
  "RET" #'mevedel-view-control-transfer-grant
  "<return>" #'mevedel-view-control-transfer-grant
  "<mouse-1>" #'mevedel-view-control-transfer-grant)

(defvar-keymap mevedel-view--control-transfer-request-map
  :doc "Keymap for requesting cooperative control from read-only mode."
  "r" #'mevedel-view-control-transfer-request
  "RET" #'mevedel-view-control-transfer-request
  "<return>" #'mevedel-view-control-transfer-request
  "<mouse-1>" #'mevedel-view-control-transfer-request)

(defun mevedel-view-interaction-teardown ()
  "Cancel timers owned by the current view before it is destroyed."
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
    (setq mevedel-view--control-transfer-timer nil)))

(defvar-keymap mevedel-view--pending-plan-map
  :doc "Keymap on the pending Plan segment in the interaction counter."
  "RET" #'mevedel-view--show-pending-plan
  "<return>" #'mevedel-view--show-pending-plan
  "<mouse-1>" #'mevedel-view--show-pending-plan
  "<mouse-2>" #'mevedel-view--show-pending-plan)


(defun mevedel-view-interaction-initialize ()
  "Initialize interaction descriptor state in the current view buffer."
  (require 'mevedel-session-control-transfer)
  (setq-local mevedel-view--interaction-descriptors
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-overlays
              (make-hash-table :test #'equal))
  (setq-local mevedel-view--interaction-telemetry-opened
              (make-hash-table :test #'equal))
  (mevedel-view-interaction-teardown)
  (when (and (boundp 'mevedel--data-buffer)
             (buffer-live-p mevedel--data-buffer)
             (not mevedel-view--agent-transcript-p))
    (let ((session (buffer-local-value 'mevedel--session
                                       mevedel--data-buffer))
          (view (current-buffer)))
      (when session
        (mevedel-session-control-transfer-register-root-buffer
         session mevedel--data-buffer)
        (mevedel-session-control-transfer-register-presentation
         session view)
        (setq-local mevedel-view--session-observer-session session
                    mevedel-view--session-observer
                    (mevedel-session-control-transfer-register-observer
                     session
                     (let ((view (current-buffer)))
                       (lambda (event &rest args)
                         (apply #'mevedel-view-interaction--session-event
                                view event args))))
                    mevedel-view--control-transfer-drain-session session
                    mevedel-view--control-transfer-drain-token
                    (mevedel-session-control-transfer-register-drain
                     session
                     (lambda ()
                       (mevedel-view-interaction-transfer-drain-p view))))))
    (mevedel-view--control-transfer-schedule (current-buffer))))

(defun mevedel-view--interaction-telemetry-close (id)
  "Record and forget telemetry lifetime ID."
  (when-let* ((metadata
               (and (hash-table-p mevedel-view--interaction-telemetry-opened)
                    (gethash id mevedel-view--interaction-telemetry-opened))))
    (when-let* ((session (mevedel-view--session))
                ((fboundp 'mevedel-telemetry-record)))
      (mevedel-telemetry-record
       session 'interaction-closed
       :interaction-id id
       :kind (plist-get metadata :kind)
       :origin (plist-get metadata :origin)
       :permission-mode-base (mevedel-session-permission-mode session)
       :permission-mode-effective
       (and (fboundp 'mevedel-permission-mode-effective)
            (mevedel-permission-mode-effective
             session mevedel--data-buffer (current-buffer)))
       :duration-ms
       (round (* 1000.0
                 (- (float-time) (plist-get metadata :opened-at))))))
    (remhash id mevedel-view--interaction-telemetry-opened)))

(defun mevedel-view-interaction-pending-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a pending user interaction.
VIEW-BUFFER defaults to the current buffer."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p mevedel-view--prompt-hook-pending)
               (and (hash-table-p mevedel-view--interaction-descriptors)
                    (> (hash-table-count
                        mevedel-view--interaction-descriptors)
                       0)))))))

(defun mevedel-view-interaction-transfer-drain-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER has a prompt that must drain.

The transfer control itself is deliberately excluded: it remains visible
while the owner waits for the other, settlement-blocking interactions."
  (let ((view (or view-buffer (current-buffer))))
    (and (buffer-live-p view)
         (with-current-buffer view
           (or (bound-and-true-p mevedel-view--prompt-hook-pending)
               (and (hash-table-p mevedel-view--interaction-descriptors)
                    (catch 'pending
                      (maphash
                       (lambda (_id descriptor)
                         (when (mevedel-view--interaction-pauses-active-work-p
                                descriptor)
                           (throw 'pending t)))
                       mevedel-view--interaction-descriptors)
                      nil)))))))

(defun mevedel-view--interaction-pauses-active-work-p (descriptor)
  "Return non-nil when interaction DESCRIPTOR pauses active work."
  (if (plist-member descriptor :active-work-paused)
      (plist-get descriptor :active-work-paused)
    (memq (plist-get descriptor :kind)
          '(ask permission plan preview request))))

(defun mevedel-view--interaction-active-work-paused-p ()
  "Return non-nil when the current view awaits actionable user input."
  (or
   (and (hash-table-p mevedel-view--interaction-descriptors)
        (catch 'paused
          (maphash
           (lambda (_id descriptor)
             (when (mevedel-view--interaction-pauses-active-work-p descriptor)
               (throw 'paused t)))
           mevedel-view--interaction-descriptors)
          nil))
   (when-let* ((session (mevedel-view--session))
               (entry (mevedel-session-pending-plan-approval session)))
     (plist-get entry :hidden))))

(defun mevedel-view-interaction-blocking-p (&optional view-buffer)
  "Return non-nil when VIEW-BUFFER awaits input outside pending-input UI."
  (let ((view (or view-buffer (current-buffer))))
    (and
     (buffer-live-p view)
     (with-current-buffer view
       (or
        (bound-and-true-p mevedel-view--prompt-hook-pending)
        (mevedel-view--interaction-active-work-paused-p))))))


;;
;;; Target view

(defun mevedel-view--interaction-target-buffer (&optional data-buffer)
  "Return the live view buffer that should host queued interactions.
DATA-BUFFER, when non-nil, is the chat/data buffer whose
`mevedel--view-buffer' binding should be consulted.  Signals when
there is no live non-transcript view.  Queue renderers catch this
as a render failure and abort the visible head rather than
silently placing controls in a data buffer."
  (cl-labels
      ((live-interaction-view-p (view)
         (and view
              (buffer-live-p view)
              (with-current-buffer view
                (and (not (bound-and-true-p
                           mevedel-view--agent-transcript-p))
                     (boundp 'mevedel-view--interaction-marker)
                     (markerp mevedel-view--interaction-marker)
                     (eq (marker-buffer mevedel-view--interaction-marker)
                         view)))))
       (view-for-data-buffer (buf &optional seen)
         (when (and buf
                    (buffer-live-p buf)
                    (not (memq buf seen)))
           (or (let ((view (buffer-local-value 'mevedel--view-buffer
                                               buf)))
                 (and (live-interaction-view-p view) view))
               (when-let* ((inv (and (boundp 'mevedel--agent-invocation)
                                     (buffer-local-value
                                      'mevedel--agent-invocation buf)))
                           ((mevedel-agent-invocation-p inv))
                           (parent (mevedel-agent-invocation-parent-data-buffer
                                    inv)))
                 (view-for-data-buffer parent (cons buf seen)))))))
    (or (and (live-interaction-view-p (current-buffer))
             (current-buffer))
        (view-for-data-buffer data-buffer)
        (view-for-data-buffer (current-buffer))
        (and (boundp 'mevedel--view-buffer)
             (live-interaction-view-p mevedel--view-buffer)
             mevedel--view-buffer)
        (error "No live view for queued prompt"))))



;;
;;; Rendering and lifecycle

(defun mevedel-view--interaction-sync-active-work-pause ()
  "Synchronize request timing with visible blocking interactions."
  (when-let* (((buffer-live-p mevedel--data-buffer))
              (request
               (buffer-local-value 'mevedel--current-request
                                   mevedel--data-buffer))
              ((mevedel-request-p request)))
    (let ((paused (mevedel-view--interaction-active-work-paused-p)))
      (mevedel-request-set-active-work-paused request paused)
      (when (fboundp 'mevedel-view--render-request-progress)
        (mevedel-view--render-request-progress)))))

(defun mevedel-view--interaction-plural (n singular plural)
  "Return N followed by SINGULAR or PLURAL."
  (format "%d %s" n (if (= n 1) singular plural)))

(defun mevedel-view--interaction-count-label ()
  "Return the composite interaction-zone counter label, or nil."
  (let ((previews 0)
        (plans 0)
        (requests 0)
        (asks 0)
        (permissions 0)
        (pending-inputs 0)
        parts)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (_id descriptor)
         (let ((count (or (plist-get descriptor :count) 0)))
           (pcase (plist-get descriptor :kind)
             ('preview (cl-incf previews count))
             ('plan (cl-incf plans count))
             ('request (cl-incf requests (max 1 count)))
             ('ask (cl-incf asks (max 1 count)))
             ('permission (cl-incf permissions count))
             ('pending-input
              (cl-incf pending-inputs count)))))
       mevedel-view--interaction-descriptors))
    (let ((session (mevedel-view--session)))
      (when session
        (when (mevedel-session-pending-plan-approval session)
          (setq plans (max plans 1)))
        (setq permissions
              (max permissions
                   (length (mevedel-session-permission-queue session))))
        (setq pending-inputs
              (max
               pending-inputs
               (+ (length (mevedel-session-pending-steering session))
                  (length
                   (mevedel-session-pending-follow-ups session)))))))
    (setq parts
          (delq nil
                (list
                 (when (> previews 0)
                   (mevedel-view--interaction-plural
                    previews "preview" "previews"))
                 (when (> plans 0)
                   (mevedel-view--interaction-plural plans "plan" "plans"))
                 (when (> requests 0)
                   (mevedel-view--interaction-plural
                    requests "request" "requests"))
                 (when (> asks 0)
                   (mevedel-view--interaction-plural asks "question" "questions"))
                 (when (> permissions 0)
                   (mevedel-view--interaction-plural
                    permissions "permission" "permissions"))
                 (when (> pending-inputs 0)
                   (mevedel-view--interaction-plural
                    pending-inputs "input" "inputs")))))
    (when parts
      (concat (string-join parts " · ") " pending"))))

(defun mevedel-view--interaction-kind-priority (kind)
  "Return the stable interaction overlay priority for KIND."
  (pcase kind
    ('preview 300)
    ('plan 200)
    ((or 'request 'ask) 150)
    ('permission 100)
    ('pending-input 80)
    (_ 50)))

(defun mevedel-view--interaction-preserve-on-rebuild-p (descriptor)
  "Return non-nil when DESCRIPTOR owns direct prompt state.
Direct request and preview prompts carry callbacks that are not represented
by a session queue.  Normal view rebuilds must keep them alive; explicit
clear/teardown paths still remove them."
  (memq (plist-get descriptor :kind) '(preview request ask)))

(defun mevedel-view--interaction-body (descriptor overlay)
  "Return DESCRIPTOR's body with standard interaction text properties.
OVERLAY is stored on the text as the descriptor's callback handle."
  (let* ((body (copy-sequence
                (mevedel--normalize-message-text
                 (or (plist-get descriptor :body) ""))))
         (map (mevedel-view--display-fragment-keymap
               (plist-get descriptor :keymap)))
         (help (plist-get descriptor :help-echo))
         (kind (plist-get descriptor :kind))
         (id (plist-get descriptor :id))
         (body-properties-owned
          (plist-get descriptor :body-properties-owned))
         (read-only (if (plist-member descriptor :read-only)
                        (plist-get descriptor :read-only)
                      t)))
    (add-text-properties
     0 (length body)
     `(mevedel-view-interaction-kind ,kind
       mevedel-view-interaction-id ,id
       mevedel-view-interaction-overlay ,overlay)
     body)
    (unless body-properties-owned
      (add-text-properties
       0 (length body)
       `(read-only ,read-only front-sticky nil rear-nonsticky t)
       body))
    (when (and map (not body-properties-owned))
      (add-text-properties 0 (length body) `(keymap ,map) body))
    (when help
      (add-text-properties 0 (length body) `(help-echo ,help) body))
    body))

(defun mevedel-view--interaction-region-end ()
  "Return the end boundary for fragment-managed interaction text."
  (let ((progress-start (mevedel-view--request-progress-region-start))
        (input-pos (mevedel-view--input-marker-position)))
    (or (and progress-start
             (or (not input-pos) (<= progress-start input-pos))
             progress-start)
        input-pos
        (point-max))))

(defun mevedel-view--interaction-descriptor-pairs ()
  "Return live interaction descriptor pairs sorted by display priority."
  (let (pairs)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (push (cons id descriptor) pairs))
       mevedel-view--interaction-descriptors))
    (sort pairs
          (lambda (a b)
            (> (or (plist-get (cdr a) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr a) :kind)))
               (or (plist-get (cdr b) :priority)
                   (mevedel-view--interaction-kind-priority
                    (plist-get (cdr b) :kind))))))))

(defun mevedel-view--interaction-apply-overlay-properties
    (overlay descriptor)
  "Apply DESCRIPTOR metadata to interaction OVERLAY."
  (let ((kind (plist-get descriptor :kind))
        (id (plist-get descriptor :id))
        (origin (plist-get descriptor :origin)))
    (overlay-put overlay 'evaporate nil)
    (overlay-put overlay 'mevedel-view-interaction-kind kind)
    (overlay-put overlay 'mevedel-view-interaction-id id)
    (overlay-put overlay 'mevedel-view-interaction-origin origin)
    (overlay-put overlay 'priority
                 (or (plist-get descriptor :priority)
                     (mevedel-view--interaction-kind-priority kind)))
    (overlay-put overlay 'read-only
                 (if (plist-member descriptor :read-only)
                     (plist-get descriptor :read-only)
                   t))
    (if-let* ((map (plist-get descriptor :keymap)))
        (overlay-put overlay 'keymap map)
      (overlay-put overlay 'keymap nil))
    (if-let* ((help (plist-get descriptor :help-echo)))
        (overlay-put overlay 'help-echo help)
      (overlay-put overlay 'help-echo nil))
    (if (plist-member descriptor :entry)
        (overlay-put overlay 'mevedel-view-interaction-entry
                     (plist-get descriptor :entry))
      (overlay-put overlay 'mevedel-view-interaction-entry nil))
    (if-let* ((activate (plist-get descriptor :activate)))
        (overlay-put overlay 'mevedel-view-interaction-activate activate)
      (overlay-put overlay 'mevedel-view-interaction-activate nil))
    overlay))

(defun mevedel-view--interaction-overlay-for (id descriptor)
  "Return live callback overlay for ID and DESCRIPTOR."
  (let ((overlay (and (hash-table-p mevedel-view--interaction-overlays)
                      (gethash id mevedel-view--interaction-overlays))))
    (unless (and (overlayp overlay) (overlay-buffer overlay))
      (let ((anchor (mevedel-view--interaction-anchor)))
        (setq overlay (make-overlay anchor anchor (current-buffer) nil t))))
    (when (hash-table-p mevedel-view--interaction-overlays)
      (puthash id overlay mevedel-view--interaction-overlays))
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    overlay))

(defun mevedel-view--show-pending-plan (&optional event)
  "Show and focus the current session's pending Plan approval from EVENT."
  (interactive (list last-nonmenu-event))
  (when (mouse-event-p event)
    (mouse-set-point event))
  (when-let* ((session (mevedel-view--session))
              (entry (mevedel-session-pending-plan-approval session))
              (id (plist-get entry :interaction-id)))
    (let ((bounds (mevedel-view-zone-fragment-bounds 'interaction id)))
      (unless bounds
        (plist-put entry :hidden nil)
        (require 'mevedel-plan-mode)
        (mevedel-plan-approval-render session)
        (setq bounds
              (mevedel-view-zone-fragment-bounds 'interaction id)))
      (when bounds
        (goto-char (plist-get bounds :start))))))

(defun mevedel-view--interaction-separator-fragment (label)
  "Return the non-navigatable interaction separator fragment for LABEL."
  (let ((body (mevedel-view--zone-separator label)))
    (when (and (mevedel-view--session)
               (mevedel-session-pending-plan-approval
                (mevedel-view--session))
               (string-match "\\b[0-9]+ plans?\\b" body))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face link
         keymap ,mevedel-view--pending-plan-map
         mouse-face highlight
         follow-link t
         help-echo "Show pending plan"
         mevedel-view-pending-plan t)
       body))
    (list :namespace 'interaction
          :id :separator
          :priority 1000
          :body body
          :navigatable nil)))

(defun mevedel-view--interaction-fragment (id descriptor)
  "Return a fragment plist for interaction DESCRIPTOR ID."
  (let* ((overlay (mevedel-view--interaction-overlay-for id descriptor))
         (body (mevedel-view--interaction-body descriptor overlay))
         (fragment (list :namespace 'interaction
                         :id id
                         :priority (or (plist-get descriptor :priority)
                                       (mevedel-view--interaction-kind-priority
                                        (plist-get descriptor :kind)))
                         :body body
                         :keymap (mevedel-view--display-fragment-keymap
                                  (plist-get descriptor :keymap))
                         :help-echo (plist-get descriptor :help-echo)
                         :entry (plist-get descriptor :entry)
                         :activate (plist-get descriptor :activate)
                         :body-properties-owned
                         (plist-get descriptor :body-properties-owned)
                         :navigatable (and (or (plist-get descriptor :activate)
                                               (plist-get descriptor :keymap))
                                           t))))
    (when (plist-member descriptor :read-only)
      (setq fragment (plist-put fragment :read-only
                                (plist-get descriptor :read-only))))
    fragment))

(defun mevedel-view--interaction-delete-overlay (overlay)
  "Release OVERLAY's retained-agent activity token and delete it."
  (when (overlayp overlay)
    (when-let* ((release
                 (overlay-get overlay 'mevedel-agent-activity-release)))
      (overlay-put overlay 'mevedel-agent-activity-release nil)
      (funcall release))
    (delete-overlay overlay)))

(defun mevedel-view--interaction-delete-stale-overlays ()
  "Delete descriptor overlays whose descriptors are no longer live."
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash
     (lambda (id overlay)
       (unless (and (hash-table-p mevedel-view--interaction-descriptors)
                    (gethash id mevedel-view--interaction-descriptors))
         (mevedel-view--interaction-delete-overlay overlay)
         (remhash id mevedel-view--interaction-overlays)))
     mevedel-view--interaction-overlays)))

(defun mevedel-view--interaction-sync-overlays (pairs)
  "Move descriptor callback overlays for PAIRS to fragment bounds."
  (dolist (pair pairs)
    (pcase-let* ((`(,id . ,descriptor) pair)
                 (overlay (and (hash-table-p mevedel-view--interaction-overlays)
                               (gethash id mevedel-view--interaction-overlays)))
                 (bounds (mevedel-view-zone-fragment-bounds
                          'interaction id)))
      (when (and (overlayp overlay) bounds)
        (move-overlay overlay
                      (plist-get bounds :start)
                      (plist-get bounds :end)
                      (current-buffer))
        (mevedel-view--interaction-apply-overlay-properties
         overlay descriptor)))))

(defun mevedel-view--interaction-render ()
  "Render interaction-zone fragments and descriptor callback overlays."
  (require 'mevedel-view-zone)
  (let* ((label (mevedel-view--interaction-count-label))
         (pairs (mevedel-view--interaction-descriptor-pairs))
         (render-p (or label pairs
                       (mevedel-view-zone-region 'interaction))))
    (mevedel-view--interaction-delete-stale-overlays)
    (when render-p
      (let* ((start (mevedel-view--interaction-anchor))
             (end (max start (mevedel-view--interaction-region-end)))
             (fragments
              (append
               (when label
                 (list (mevedel-view--interaction-separator-fragment label)))
               (mapcar
                (lambda (pair)
                  (pcase-let ((`(,id . ,descriptor) pair))
                    (mevedel-view--interaction-fragment id descriptor)))
                pairs))))
        (mevedel-view-zone-reconcile 'interaction start end fragments)
        (mevedel-view--interaction-sync-overlays pairs)))))

(defun mevedel-view--interaction-rebuild ()
  "Rebuild interaction-zone descriptors from live preview and queue state.
This deletes only interaction UI overlays and never settles callbacks."
  (unless mevedel-view--agent-transcript-p
    (require 'mevedel-session-control-transfer)
    (unwind-protect
        (progn
          (mevedel-view--interaction-clear-for-rebuild)
          (when-let* ((session (mevedel-view--session)))
            (when-let ((descriptor
                        (mevedel-session-control-transfer-descriptor
                         session
                         ;; Read-only mode lives on the data buffer.  Asking
                         ;; the view for it always answered nil, so both
                         ;; clients rendered the owner's side of a transfer.
                         (when-let ((data
                                     (mevedel-view--control-transfer-data-buffer)))
                           (buffer-local-value 'mevedel-session--read-only-mode
                                               data)))))
              (mevedel-view--interaction-register
               (append descriptor
                       (list :id 'control-transfer
                             :body
                             (mevedel-view--control-transfer-body descriptor)
                             :keymap
                             (pcase (plist-get descriptor :action)
                               ('grant mevedel-view--control-transfer-map)
                               ('request
                                mevedel-view--control-transfer-request-map)
                               ;; A pending request is a status line; there
                               ;; is nothing for the user to press on it.
                               (_ mevedel-view--control-transfer-status-map))
                             :origin "/root"))))
            (when (mevedel-session-pending-plan-approval session)
              (require 'mevedel-plan-mode)
              (mevedel-plan-approval-render session))
            (when (mevedel-session-permission-queue session)
              (require 'mevedel-permission-queue)
              (mevedel-permission-queue--render-head session))
            (when (or (mevedel-session-pending-steering session)
                      (mevedel-session-pending-follow-ups session)
                      (mevedel-session-pending-input-failure-paused session))
              (mevedel-view--pending-inputs-render session)))
          (when (hash-table-p mevedel-view--interaction-telemetry-opened)
            (let (closed)
              (maphash
               (lambda (id _metadata)
                 (unless
                     (and (hash-table-p
                           mevedel-view--interaction-descriptors)
                          (gethash id mevedel-view--interaction-descriptors))
                   (push id closed)))
               mevedel-view--interaction-telemetry-opened)
              (dolist (id closed)
                (mevedel-view--interaction-telemetry-close id))))
          (mevedel-view--interaction-render))
      (mevedel-view--interaction-sync-active-work-pause))))

(defun mevedel-view--interaction-register (descriptor)
  "Register DESCRIPTOR in the interaction zone and return its overlay."
  (unless (hash-table-p mevedel-view--interaction-descriptors)
    (setq mevedel-view--interaction-descriptors
          (make-hash-table :test #'equal)))
  (unless (hash-table-p mevedel-view--interaction-overlays)
    (setq mevedel-view--interaction-overlays
          (make-hash-table :test #'equal)))
  (let* ((id (plist-get descriptor :id))
         (anchor (mevedel-view--interaction-anchor))
         (existing-overlay
          (and (hash-table-p mevedel-view--interaction-overlays)
               (gethash id mevedel-view--interaction-overlays)))
         (overlay (or existing-overlay
                      (make-overlay anchor anchor (current-buffer) nil t))))
    (mevedel-view--debug-log
     'interaction-register-begin
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (mevedel-view--debug-state mevedel--data-buffer))
    (unless (hash-table-p mevedel-view--interaction-telemetry-opened)
      (setq mevedel-view--interaction-telemetry-opened
            (make-hash-table :test #'equal)))
    (unless (gethash id mevedel-view--interaction-telemetry-opened)
      (puthash id
               (list :opened-at (float-time)
                     :kind (plist-get descriptor :kind)
                     :origin (plist-get descriptor :origin))
               mevedel-view--interaction-telemetry-opened)
      (when-let* ((session (mevedel-view--session))
                  ((fboundp 'mevedel-telemetry-record)))
        (mevedel-telemetry-record
         session 'interaction-opened
         :interaction-id id
         :kind (plist-get descriptor :kind)
         :origin (plist-get descriptor :origin)
         :permission-mode-base (mevedel-session-permission-mode session)
         :permission-mode-effective
         (and (fboundp 'mevedel-permission-mode-effective)
              (mevedel-permission-mode-effective
               session mevedel--data-buffer (current-buffer)))
         :active-work-paused
         (and (mevedel-view--interaction-pauses-active-work-p descriptor) t)
         :pending-count
         (and (hash-table-p mevedel-view--interaction-descriptors)
              (1+ (hash-table-count
                   mevedel-view--interaction-descriptors))))))
    (unless existing-overlay
      (let ((origin (plist-get descriptor :origin))
            (kind (plist-get descriptor :kind))
            (session (mevedel-view--session)))
        (when (and session origin
                   (not (equal origin "/root"))
                   (not (eq kind 'permission)))
          (require 'mevedel-agent-control)
          (overlay-put
           overlay 'mevedel-agent-activity-release
           (mevedel-agent-control-block-turn
            session origin 'interaction-blocked)))))
    (puthash id descriptor mevedel-view--interaction-descriptors)
    (puthash id overlay mevedel-view--interaction-overlays)
    (mevedel-view--interaction-apply-overlay-properties overlay descriptor)
    (mevedel-view--interaction-sync-active-work-pause)
    (mevedel-view--interaction-render)
    (mevedel-view--debug-log
     'interaction-register-end
     :interaction-id id
     :kind (plist-get descriptor :kind)
     :state (mevedel-view--debug-state mevedel--data-buffer))
    overlay))

(defun mevedel-view--interaction-unregister (id)
  "Remove interaction-zone descriptor ID and its overlay."
  (mevedel-view--interaction-telemetry-close id)
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (remhash id mevedel-view--interaction-descriptors))
  (mevedel-view--interaction-sync-active-work-pause)
  (when (hash-table-p mevedel-view--interaction-overlays)
    (when-let* ((overlay (gethash id mevedel-view--interaction-overlays)))
      (mevedel-view--interaction-delete-overlay overlay)
      (when (and (boundp 'mevedel--prompt-overlays)
                 (listp mevedel--prompt-overlays))
        (setq mevedel--prompt-overlays
              (delq overlay mevedel--prompt-overlays)))
      (remhash id mevedel-view--interaction-overlays))
    (mevedel-view--interaction-render)))

(defun mevedel-view--interaction-clear-for-rebuild ()
  "Delete rebuild-owned interaction UI while preserving direct prompt UI."
  (let (remove-ids)
    (when (hash-table-p mevedel-view--interaction-descriptors)
      (maphash
       (lambda (id descriptor)
         (unless (mevedel-view--interaction-preserve-on-rebuild-p descriptor)
           (push id remove-ids)))
       mevedel-view--interaction-descriptors))
    (dolist (id remove-ids)
      (when (hash-table-p mevedel-view--interaction-overlays)
        (when-let* ((overlay (gethash id
                                      mevedel-view--interaction-overlays)))
          (mevedel-view--interaction-delete-overlay overlay))
        (remhash id mevedel-view--interaction-overlays))
      (when (hash-table-p mevedel-view--interaction-descriptors)
        (remhash id mevedel-view--interaction-descriptors))))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (let* ((id (and (overlayp ov)
                        (overlay-get ov 'mevedel-view-interaction-id)))
               (descriptor
                (and id
                     (hash-table-p mevedel-view--interaction-descriptors)
                     (gethash id mevedel-view--interaction-descriptors))))
          (cond
           ((not (and (overlayp ov) (overlay-buffer ov))))
           ((and (eq (overlay-buffer ov) (current-buffer))
                 id
                 (not (mevedel-view--interaction-preserve-on-rebuild-p
                       descriptor)))
            (mevedel-view--interaction-delete-overlay ov))
           (t
            (push ov live)))))
      (setq mevedel--prompt-overlays (nreverse live))))
  (mevedel-view--interaction-render))

(defun mevedel-view--interaction-clear ()
  "Delete all interaction-zone overlays without firing callbacks."
  (when (hash-table-p mevedel-view--interaction-descriptors)
    (clrhash mevedel-view--interaction-descriptors))
  (mevedel-view--interaction-sync-active-work-pause)
  (mevedel-view--interaction-render)
  (when (hash-table-p mevedel-view--interaction-overlays)
    (maphash (lambda (_id overlay)
               (mevedel-view--interaction-delete-overlay overlay))
             mevedel-view--interaction-overlays)
    (clrhash mevedel-view--interaction-overlays))
  (when (and (boundp 'mevedel--prompt-overlays)
             (listp mevedel--prompt-overlays))
    (let (live)
      (dolist (ov mevedel--prompt-overlays)
        (cond
         ((not (and (overlayp ov) (overlay-buffer ov))))
         ((and (eq (overlay-buffer ov) (current-buffer))
               (overlay-get ov 'mevedel-view-interaction-id))
          (mevedel-view--interaction-delete-overlay ov))
         (t
          (push ov live))))
      (setq mevedel--prompt-overlays (nreverse live)))))


(defun mevedel-view--interaction-anchor ()
  "Return the buffer position to anchor an interaction-zone overlay.
View buffers require a live `mevedel-view--interaction-marker'.  If its
position has drifted outside the status/input boundaries, repair it to the
current status anchor.  Non-view buffers use `(point-max)' so tool rendering
can still build isolated fragments."
  (if (not (derived-mode-p 'mevedel-view-mode))
      (point-max)
    (unless (and (markerp mevedel-view--interaction-marker)
                 (eq (marker-buffer mevedel-view--interaction-marker)
                     (current-buffer))
                 (marker-position mevedel-view--interaction-marker))
      (error "View interaction marker is not live"))
    (let* ((input-pos (mevedel-view--input-marker-position))
           (status-pos (mevedel-view--status-anchor))
           (interaction-pos (marker-position
                             mevedel-view--interaction-marker)))
      (if (and (>= interaction-pos status-pos)
               (or (not input-pos) (<= interaction-pos input-pos)))
          interaction-pos
        (let ((anchor (if input-pos
                          (min status-pos input-pos)
                        status-pos)))
          (set-marker mevedel-view--interaction-marker anchor)
          anchor)))))


(provide 'mevedel-view-interaction)

;;; mevedel-view-interaction.el ends here
