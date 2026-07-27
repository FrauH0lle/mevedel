;;; mevedel-pending-inputs.el --- Pending Inputs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Session-owned inspection and composer editing for same-turn steering and
;; queued follow-ups.  Opening the cockpit pauses automatic delivery; queue
;; edits retain the selected entry until a validated replacement is ready.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'tabulated-list))

;; `gptel-request'
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-session
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-context-view-buffer
                  "mevedel-cockpit" (&optional context))
(declare-function mevedel-cockpit-current-context "mevedel-cockpit" ())
(declare-function mevedel-cockpit-open-surface
                  "mevedel-cockpit" (surface &optional context))
(declare-function mevedel-cockpit-quit "mevedel-cockpit" (&optional label))
(declare-function mevedel-cockpit-setup-tabulated-surface
                  "mevedel-cockpit" (surface))
(declare-function mevedel-cockpit-surface-context
                  "mevedel-cockpit" (&optional surface))
(declare-function mevedel-cockpit-surface-refresh
                  "mevedel-cockpit" (&optional selected-id))
(declare-function mevedel-cockpit-surface-selected
                  "mevedel-cockpit" (&optional no-error))

;; `mevedel-mention-bindings'
(declare-function mevedel-mention-bindings-copy-text
                  "mevedel-mention-bindings" (text))

;; `mevedel-structs'
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-pending-input-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(defvar mevedel--current-request)

;; `mevedel-view-composer'
(declare-function mevedel-view--bind-input-mentions
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--composer-snapshot
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--pending-input-preview
                  "mevedel-view-composer" (input))
(declare-function mevedel-view--pending-input-text
                  "mevedel-view-composer" (entry))
(declare-function mevedel-view--pop-dropped-file-grants-for-input
                  "mevedel-view-composer" (input session))
(declare-function mevedel-view--prepare-steering-entry
                  "mevedel-view-composer" (submission request))
(declare-function mevedel-view--restore-composer-snapshot
                  "mevedel-view-composer" (snapshot session &optional force))
(declare-function mevedel-view--schedule-late-follow-up-drain
                  "mevedel-view-composer" ())
(declare-function mevedel-view--submit-planned-input
                  "mevedel-view-composer"
                  (input &optional before-send on-block dispatch after-insert))
(declare-function mevedel-view-refresh-input-prompt
                  "mevedel-view-composer" ())
(defvar mevedel--data-buffer)
(defvar mevedel-view--pending-input-edit)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

;; `tabulated-list'
(declare-function tabulated-list-mode "tabulated-list" ())

(defconst mevedel-pending-inputs-buffer-name "*mevedel pending inputs*"
  "Name of the Pending Inputs cockpit buffer.")

(defun mevedel-pending-inputs--session (&optional context)
  "Return the Pending Inputs session for CONTEXT."
  (or (mevedel-cockpit-context-session
       (or context (mevedel-cockpit-surface-context)))
      (user-error "No mevedel session in this buffer")))

(defun mevedel-pending-inputs--collect (context)
  "Collect pending-input row projections for CONTEXT."
  (let ((session (mevedel-pending-inputs--session context))
        rows)
    (dolist (category '(steering follow-up))
      (let ((position 0)
            (entries
             (if (eq category 'steering)
                 (mevedel-session-pending-steering session)
               (mevedel-session-pending-follow-ups session))))
        (dolist (entry entries)
          (push (list :id (plist-get entry :id)
                      :category category
                      :position (cl-incf position)
                      :state (or (plist-get entry :state) 'pending)
                      :entry entry)
                rows))))
    (nreverse rows)))

(defun mevedel-pending-inputs--entry (item _context)
  "Return the tabulated row for pending-input ITEM."
  (let* ((entry (plist-get item :entry))
         (category (plist-get item :category))
         (id (plist-get item :id)))
    (list
     id
     (vector
      (if (eq category 'steering) "Steering" "Follow-up")
      (number-to-string (plist-get item :position))
      (capitalize (symbol-name (plist-get item :state)))
      (mevedel-view--pending-input-preview
       (mevedel-view--pending-input-text entry))))))

(defun mevedel-pending-inputs--header (items context)
  "Return the cockpit header for ITEMS and CONTEXT."
  (format "%d pending input%s%s"
          (length items)
          (if (= 1 (length items)) "" "s")
          (if (mevedel-session-pending-input-paused
               (mevedel-pending-inputs--session context))
              " · delivery paused"
            "")))

(defun mevedel-pending-inputs--details (item _context)
  "Return full text for pending-input ITEM."
  (mevedel-view--pending-input-text (plist-get item :entry)))

(defun mevedel-pending-inputs--setup (context)
  "Pause delivery before rendering the cockpit for CONTEXT."
  (let ((session (mevedel-pending-inputs--session context))
        (view (mevedel-cockpit-context-view-buffer context)))
    (mevedel-session-set-pending-input-paused session t)
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))))

(defun mevedel-pending-inputs--selected ()
  "Return the selected pending-input row projection."
  (mevedel-cockpit-surface-selected))

(defun mevedel-pending-inputs--replace
    (session category id replacement)
  "Replace pending entry ID in SESSION CATEGORY with REPLACEMENT."
  (let* ((entries
          (if (eq category 'steering)
              (mevedel-session-pending-steering session)
            (mevedel-session-pending-follow-ups session)))
         (position
          (cl-position-if
           (lambda (entry) (equal id (plist-get entry :id)))
           entries)))
    (unless position
      (user-error "Pending input is no longer queued"))
    (let ((updated (copy-sequence entries)))
      (setcar (nthcdr position updated) replacement)
      (mevedel-session-set-pending-inputs session category updated))
    replacement))

(defun mevedel-pending-inputs--restore-entry-metadata
    (prepared original)
  "Copy stable queue metadata from ORIGINAL to PREPARED."
  (dolist (key '(:id :category :queued-at-time :queued-at-turn
                 :queued-at-goal-id :state))
    (when (plist-member original key)
      (setq prepared (plist-put prepared key (plist-get original key)))))
  prepared)

(defun mevedel-pending-inputs--return-to-cockpit (state)
  "Restore the suspended draft and return to the cockpit for STATE."
  (let* ((context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (cockpit (plist-get state :cockpit-buffer))
         (id (plist-get state :id)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (setq mevedel-view--pending-input-edit nil)
        (mevedel-view--restore-composer-snapshot
         (plist-get state :composer-snapshot) session t)
        (mevedel-view-refresh-input-prompt)
        (mevedel-view--interaction-rebuild)))
    (if (buffer-live-p cockpit)
        (progn
          (with-current-buffer cockpit
            (mevedel-cockpit-surface-refresh id))
          (when-let* ((window (display-buffer cockpit)))
            (select-window window)))
      (when (buffer-live-p view)
        (when-let* ((window (display-buffer view)))
          (select-window window))))))

(defun mevedel-pending-inputs-edit ()
  "Edit the selected pending input in its owning view composer."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (category (plist-get item :category))
         (position (plist-get item :position))
         (cockpit (current-buffer)))
    (unless (buffer-live-p view)
      (user-error "No live owning view"))
    (with-current-buffer view
      (require 'mevedel-mention-bindings)
      (when mevedel-view--pending-input-edit
        (user-error "A pending-input edit is already active"))
      (let* ((snapshot (mevedel-view--composer-snapshot session))
             (input (mevedel-mention-bindings-copy-text
                     (plist-get entry :input)))
             (state
              (list
               :id (plist-get entry :id)
               :category category
               :label
               (format "%s %d"
                       (if (eq category 'steering)
                           "steering"
                         "follow-up")
                       position)
               :entry entry
               :context context
               :cockpit-buffer cockpit
               :saving nil
               :composer-snapshot snapshot)))
        (setq mevedel-view--pending-input-edit state)
        (mevedel-view--restore-composer-snapshot
         (list :text input
               :point-offset (length input)
               :dropped-file-grants
               (copy-sequence
                (plist-get entry :dropped-file-grants)))
         session t)
        (mevedel-view-refresh-input-prompt)))
    (when-let* ((window (display-buffer view)))
      (select-window window))))

(defun mevedel-pending-inputs--finish-replacement
    (state replacement)
  "Replace STATE's entry with REPLACEMENT and finish the edit."
  (let* ((context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (original (plist-get state :entry))
         (replacement
          (mevedel-pending-inputs--restore-entry-metadata
           replacement original)))
    (mevedel-pending-inputs--replace
     session
     (plist-get state :category)
     (plist-get state :id)
     replacement)
    (mevedel-pending-inputs--return-to-cockpit state)
    replacement))

(defun mevedel-pending-inputs--save-follow-up (state input session)
  "Save follow-up edit STATE using bound INPUT in SESSION."
  (let* ((original (copy-sequence (plist-get state :entry)))
         (grants
          (mevedel-view--pop-dropped-file-grants-for-input input session))
         (replacement (plist-put original :input input)))
    (setq replacement
          (plist-put replacement :dropped-file-grants grants)
          replacement (plist-put replacement :submission nil))
    (mevedel-pending-inputs--finish-replacement state replacement)))

(defun mevedel-pending-inputs--save-steering (state input request)
  "Prepare and save steering edit STATE from INPUT for REQUEST."
  (setf (plist-get state :saving) t)
  (mevedel-view--submit-planned-input
   input nil
   (lambda ()
     (when (eq state mevedel-view--pending-input-edit)
       (setf (plist-get state :saving) nil)
       (message "mevedel: pending-input edit was not accepted")))
   (lambda (submission)
     (when (eq state mevedel-view--pending-input-edit)
       (if-let* ((prepared
                  (mevedel-view--prepare-steering-entry
                   submission request)))
           (mevedel-pending-inputs--finish-replacement state prepared)
         (setf (plist-get state :saving) nil))))))

(defun mevedel-pending-inputs-save-edit ()
  "Validate and save the active pending-input composer edit."
  (interactive)
  (unless mevedel-view--pending-input-edit
    (user-error "No pending-input edit is active"))
  (let* ((state mevedel-view--pending-input-edit)
         (context (plist-get state :context))
         (session (mevedel-pending-inputs--session context))
         (category (plist-get state :category)))
    (when (plist-get state :saving)
      (user-error "Pending-input preparation is still running"))
    (condition-case err
        (let ((input (mevedel-view--bind-input-mentions session)))
          (when (string-empty-p input)
            (user-error "Pending input must not be empty"))
          (when (string-match-p "\\`[ \t]*[/]" input)
            (user-error "Slash commands cannot be pending input"))
          (if (eq category 'follow-up)
              (mevedel-pending-inputs--save-follow-up
               state input session)
            (let ((request
                   (and (buffer-live-p mevedel--data-buffer)
                        (buffer-local-value
                         'mevedel--current-request mevedel--data-buffer))))
              (unless (and request
                           (equal (mevedel-request-id request)
                                  (plist-get
                                   (plist-get state :entry)
                                   :request-id)))
                (user-error "Steering turn is no longer active"))
              (mevedel-pending-inputs--save-steering
               state input request))))
      (error
       (message "mevedel: pending-input edit failed: %s"
                (error-message-string err))))))

(defun mevedel-pending-inputs-cancel-edit ()
  "Cancel the active pending-input composer edit."
  (interactive)
  (unless mevedel-view--pending-input-edit
    (user-error "No pending-input edit is active"))
  (let ((state mevedel-view--pending-input-edit))
    (mevedel-pending-inputs--return-to-cockpit state)
    (message "mevedel: pending-input edit cancelled")))

(defun mevedel-pending-inputs-quit ()
  "Close the cockpit and resume eligible automatic delivery."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (editing
          (and (buffer-live-p view)
               (buffer-local-value
                'mevedel-view--pending-input-edit view))))
    (when editing
      (user-error "Save or cancel the pending-input edit first"))
    (mevedel-session-set-pending-input-paused session nil)
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))
    (when-let* ((data-buffer
                 (and (buffer-live-p view)
                      (buffer-local-value 'mevedel--data-buffer view)))
                ((buffer-live-p data-buffer))
                (request
                 (buffer-local-value
                  'mevedel--current-request data-buffer))
                (fsm (and request (mevedel-request-fsm request)))
                ((plist-get (gptel-fsm-info fsm)
                            :mevedel-pending-input-hold)))
      (gptel--fsm-transition fsm 'WAIT))
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--schedule-late-follow-up-drain)))
    (mevedel-cockpit-quit "Pending Inputs cockpit")))

(defconst mevedel-pending-inputs--surface
  `(:buffer-name ,mevedel-pending-inputs-buffer-name
    :label "Pending Inputs cockpit"
    :row-label "pending input"
    :mode mevedel-pending-inputs-mode
    :format [("Category" 12 nil)
             ("#" 4 nil)
             ("State" 12 nil)
             ("Preview" 0 nil)]
    :require-session t
    :collect mevedel-pending-inputs--collect
    :entry mevedel-pending-inputs--entry
    :header mevedel-pending-inputs--header
    :details mevedel-pending-inputs--details
    :details-buffer "*mevedel pending input*"
    :setup mevedel-pending-inputs--setup
    :keys (("RET" "Edit selected pending input" mevedel-pending-inputs-edit)
           ("e" "Edit selected pending input" mevedel-pending-inputs-edit)
           ("q" "Close and resume eligible delivery" mevedel-pending-inputs-quit)))
  "Cockpit surface spec for pending input.")

(define-derived-mode mevedel-pending-inputs-mode tabulated-list-mode
  "mevedel-pending-inputs"
  "Major mode for inspecting and editing pending input."
  (require 'mevedel-cockpit)
  (mevedel-cockpit-setup-tabulated-surface
   mevedel-pending-inputs--surface))

(defun mevedel-pending-inputs-open (&optional context-or-event)
  "Open the Pending Inputs cockpit for CONTEXT-OR-EVENT."
  (interactive
   (list (and (mouse-event-p last-nonmenu-event)
              last-nonmenu-event)))
  (when (mouse-event-p context-or-event)
    (mouse-set-point context-or-event)
    (setq context-or-event nil))
  (require 'mevedel-cockpit)
  (let* ((context (or context-or-event
                      (mevedel-cockpit-current-context)))
         (session (mevedel-pending-inputs--session context)))
    (unless (or (mevedel-session-pending-steering session)
                (mevedel-session-pending-follow-ups session))
      (user-error "No pending input"))
    (mevedel-cockpit-open-surface
     mevedel-pending-inputs--surface context)))

(provide 'mevedel-pending-inputs)
;;; mevedel-pending-inputs.el ends here
