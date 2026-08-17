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
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-data-buffer
                  "mevedel-cockpit" (&optional context))
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

;; `mevedel-prompt-submission'
(declare-function mevedel-prompt-submission-restore
                  "mevedel-prompt-submission" (submission))

;; `mevedel-structs'
(declare-function mevedel-goal-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-fsm "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session--set-dropped-file-grants
                  "mevedel-structs" (session paths))
(declare-function mevedel-session-dropped-file-grants
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-goal "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-failure-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-input-paused
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-inputs
                  "mevedel-structs" (session category))
(declare-function mevedel-session-pending-steering
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-set-pending-input-failure-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-input-paused
                  "mevedel-structs" (session paused))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)

;; `mevedel-view-composer'
(declare-function mevedel-view--bind-input-mentions
                  "mevedel-view-composer" (session))
(declare-function mevedel-view--composer-snapshot
                  "mevedel-view-composer" (session))
(declare-function mevedel-view-composer-scope-label
                  "mevedel-view-composer" (&optional scope))
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

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-assert-new-mutation-authority
                  "mevedel-session-persistence" (session))
(defvar mevedel--data-buffer)
(defvar mevedel-view--pending-input-edit)

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())

;; `tabulated-list'
(declare-function tabulated-list-get-id "tabulated-list" ())
(declare-function tabulated-list-mode "tabulated-list" ())
(declare-function tabulated-list-put-tag
                  "tabulated-list" (tag &optional advance))

(defconst mevedel-pending-inputs-buffer-name "*mevedel pending inputs*"
  "Name of the Pending Inputs cockpit buffer.")

(defvar-local mevedel-pending-inputs--marked-ids nil
  "Pending-input identities marked for deletion.")

(defvar-local mevedel-pending-inputs--converting-id nil
  "Pending-input identity undergoing asynchronous conversion.")

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
            (entries (mevedel-session-pending-inputs session category)))
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
      (pcase (plist-get item :state)
        ('failed-turn "Needs review")
        (state (capitalize (symbol-name state))))
      (concat
       (when-let* ((scope (plist-get entry :scope)))
         (format "[◆ %s] " (mevedel-view-composer-scope-label scope)))
       (when-let* ((guest (plist-get entry :guest-name)))
         (format "[⇄ %s] " guest))
       (mevedel-view--pending-input-preview
        (mevedel-view--pending-input-text entry)))))))

(defun mevedel-pending-inputs--header (items context)
  "Return the cockpit header for ITEMS and CONTEXT."
  (format "%d pending input%s%s"
          (length items)
          (if (= 1 (length items)) "" "s")
          (let ((session (mevedel-pending-inputs--session context)))
            (concat
             (when (mevedel-session-pending-input-failure-paused session)
               " · failure recovery required")
             (when (mevedel-session-pending-input-paused session)
               " · delivery paused")))))

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

(defun mevedel-pending-inputs--refresh (&optional selected-id)
  "Refresh live pending-input UI, preserving SELECTED-ID and deletion marks."
  (let* ((context (mevedel-cockpit-surface-context))
         (view (mevedel-cockpit-context-view-buffer context)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))
    (mevedel-cockpit-surface-refresh selected-id)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (member (tabulated-list-get-id)
                      mevedel-pending-inputs--marked-ids)
          (tabulated-list-put-tag "D"))
        (forward-line 1)))))

(defun mevedel-pending-inputs-refresh ()
  "Refresh the live Pending Inputs cockpit."
  (interactive)
  (mevedel-pending-inputs--refresh (tabulated-list-get-id)))

(defun mevedel-pending-inputs--replace
    (session category id replacement)
  "Replace pending entry ID in SESSION CATEGORY with REPLACEMENT."
  (let* ((entries (mevedel-session-pending-inputs session category))
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

(defun mevedel-pending-inputs--move-between
    (session source target id replacement)
  "Move ID from SESSION SOURCE to TARGET tail as REPLACEMENT."
  (let ((source-entries (mevedel-session-pending-inputs session source)))
    (unless (cl-find id source-entries
                     :key (lambda (entry) (plist-get entry :id))
                     :test #'equal)
      (user-error "Pending input is no longer queued"))
    (mevedel-session-set-pending-inputs
     session source
     (cl-remove id source-entries
                :key (lambda (entry) (plist-get entry :id))
                :test #'equal))
    (mevedel-session-set-pending-inputs
     session target
     (append (mevedel-session-pending-inputs session target)
             (list replacement)))
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

(defun mevedel-pending-inputs--save-failed-steering (state input session)
  "Save failed steering edit STATE as review-only INPUT in SESSION."
  (let* ((original (plist-get state :entry))
         (grants
          (mevedel-view--pop-dropped-file-grants-for-input input session))
         replacement)
    (when-let* ((submission (plist-get original :submission)))
      (mevedel-prompt-submission-restore submission))
    (cl-loop for (key value) on original by #'cddr
             unless (memq key '(:model-input :transcript-payload
                                :hook-audits :request-context :submission))
             do (setq replacement
                      (append replacement (list key value))))
    (setq replacement
          (plist-put replacement :input input)
          replacement
          (plist-put replacement :dropped-file-grants grants))
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
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-assert-new-mutation-authority session)
    (when (plist-get state :saving)
      (user-error "Pending-input preparation is still running"))
    (condition-case err
        (let ((input (mevedel-view--bind-input-mentions session)))
          (when (string-empty-p input)
            (user-error "Pending input must not be empty"))
          (when (string-match-p "\\`[ \t]*[/]" input)
            (user-error "Slash commands cannot be pending input"))
          (cond
           ((eq category 'follow-up)
            (mevedel-pending-inputs--save-follow-up
             state input session))
           ((eq (plist-get (plist-get state :entry) :state)
                'failed-turn)
            (mevedel-pending-inputs--save-failed-steering
             state input session))
           (t
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
               state input request)))))
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

(defun mevedel-pending-inputs--move (offset)
  "Move selected pending input by OFFSET inside its category."
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (category (plist-get item :category))
         (id (plist-get item :id))
         (entries (copy-sequence
                   (mevedel-session-pending-inputs session category)))
         (index (cl-position id entries
                             :key (lambda (entry) (plist-get entry :id))
                             :test #'equal))
         (target (and index (+ index offset))))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-assert-new-mutation-authority session)
    (unless (and target (>= target 0) (< target (length entries)))
      (user-error "Pending input is already at the category boundary"))
    (cl-rotatef (nth index entries) (nth target entries))
    (mevedel-session-set-pending-inputs session category entries)
    (mevedel-pending-inputs--refresh id)))

(defun mevedel-pending-inputs-move-up ()
  "Move the selected pending input up within its category."
  (interactive)
  (mevedel-pending-inputs--move -1))

(defun mevedel-pending-inputs-move-down ()
  "Move the selected pending input down within its category."
  (interactive)
  (mevedel-pending-inputs--move 1))

(defun mevedel-pending-inputs--current-request (context)
  "Return CONTEXT's current root request, or nil."
  (when-let* ((data-buffer
               (mevedel-cockpit-context-data-buffer context))
              ((buffer-live-p data-buffer)))
    (buffer-local-value 'mevedel--current-request data-buffer)))

(defun mevedel-pending-inputs--copy-input (entry)
  "Return ENTRY's propertized input copy."
  (require 'mevedel-mention-bindings)
  (mevedel-mention-bindings-copy-text (plist-get entry :input)))

(defun mevedel-pending-inputs--conversion-finished
    (cockpit context selected-id)
  "Refresh COCKPIT and CONTEXT after converting SELECTED-ID."
  (if (buffer-live-p cockpit)
      (with-current-buffer cockpit
        (setq mevedel-pending-inputs--converting-id nil)
        (mevedel-pending-inputs--refresh selected-id))
    (when-let* ((view (mevedel-cockpit-context-view-buffer context)))
      (with-current-buffer view
        (mevedel-view--interaction-rebuild)))))

(defun mevedel-pending-inputs-make-steering ()
  "Convert the selected follow-up to steering after full preparation."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id))
         (request (mevedel-pending-inputs--current-request context))
         (fsm (and request (mevedel-request-fsm request)))
         (view (mevedel-cockpit-context-view-buffer context))
         (cockpit (current-buffer))
         (original-grants
          (copy-sequence
           (mevedel-session-dropped-file-grants session)))
         (entry-grants
          (copy-sequence (plist-get entry :dropped-file-grants)))
         (staged-grants
          (cl-set-difference entry-grants original-grants :test #'equal))
         (restore-grants
          (lambda ()
            (let ((current
                   (mevedel-session-dropped-file-grants session)))
              (mevedel-session--set-dropped-file-grants
               session
               (append
                original-grants
                (cl-set-difference
                 current
                 (append original-grants staged-grants)
                 :test #'equal)))))))
    (require 'mevedel-session-persistence)
    (mevedel-session-persistence-assert-new-mutation-authority session)
    (unless (eq (plist-get item :category) 'follow-up)
      (user-error "Pending input is already steering"))
    (when (plist-get entry :scope)
      (user-error "Directive follow-ups cannot be converted to steering"))
    (when mevedel-pending-inputs--converting-id
      (user-error "Pending-input conversion is still running"))
    (unless (and fsm
                 (not (memq (gptel-fsm-state fsm) '(DONE ERRS ABRT))))
      (user-error "No steerable root turn is active"))
    (unless (buffer-live-p view)
      (user-error "No live owning view"))
    (setq mevedel-pending-inputs--converting-id id)
    (mevedel-session--set-dropped-file-grants
     session (append staged-grants original-grants))
    (with-current-buffer view
      (condition-case err
          (mevedel-view--submit-planned-input
           (mevedel-pending-inputs--copy-input entry)
           nil
           (lambda ()
             (funcall restore-grants)
             (when (buffer-live-p cockpit)
               (with-current-buffer cockpit
                 (setq mevedel-pending-inputs--converting-id nil)))
             (message "mevedel: follow-up remains unchanged"))
           (lambda (submission)
             (unwind-protect
                 (if-let* ((prepared
                            (mevedel-view--prepare-steering-entry
                             submission request)))
                     (condition-case conversion-err
                         (let ((replacement
                                (mevedel-pending-inputs--restore-entry-metadata
                                 prepared entry)))
                           (setq replacement
                                 (plist-put replacement :category 'steering)
                                 replacement
                                 (plist-put replacement :state 'pending))
                           (mevedel-pending-inputs--move-between
                            session 'follow-up 'steering id replacement)
                           (mevedel-pending-inputs--conversion-finished
                            cockpit context id))
                       (error
                        (mevedel-prompt-submission-restore submission)
                        (when (buffer-live-p cockpit)
                          (with-current-buffer cockpit
                            (setq mevedel-pending-inputs--converting-id nil)))
                        (message "mevedel: follow-up remains unchanged: %s"
                                 (error-message-string conversion-err))))
                   (when (buffer-live-p cockpit)
                     (with-current-buffer cockpit
                       (setq mevedel-pending-inputs--converting-id nil))))
               (funcall restore-grants))))
        (error
         (funcall restore-grants)
         (setq mevedel-pending-inputs--converting-id nil)
         (signal (car err) (cdr err)))))))

(defun mevedel-pending-inputs-make-follow-up ()
  "Convert the selected steering entry to a follow-up at the target tail."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (item (mevedel-pending-inputs--selected))
         (entry (plist-get item :entry))
         (id (plist-get item :id)))
    (unless (eq (plist-get item :category) 'steering)
      (user-error "Pending input is already a follow-up"))
    (when-let* ((submission (plist-get entry :submission)))
      (mevedel-prompt-submission-restore submission))
    (let ((replacement
           (list
            :id id
            :category 'follow-up
            :input (mevedel-pending-inputs--copy-input entry)
            :dropped-file-grants
            (copy-sequence (plist-get entry :dropped-file-grants))
            :queued-at-time (float-time)
            :queued-at-goal-id
            (when-let* ((goal (mevedel-session-goal session)))
              (mevedel-goal-id goal))
            :queued-at-turn
            (or (mevedel-session-turn-count session) 0)
            :state 'pending)))
      (mevedel-pending-inputs--move-between
       session 'steering 'follow-up id replacement)
      (mevedel-pending-inputs--refresh id))))

(defun mevedel-pending-inputs-mark-delete ()
  "Mark the selected pending input for deletion."
  (interactive)
  (let ((id (plist-get (mevedel-pending-inputs--selected) :id)))
    (cl-pushnew id mevedel-pending-inputs--marked-ids :test #'equal)
    (tabulated-list-put-tag "D" t)))

(defun mevedel-pending-inputs-unmark ()
  "Remove the deletion mark from the selected pending input."
  (interactive)
  (let ((id (plist-get (mevedel-pending-inputs--selected) :id)))
    (setq mevedel-pending-inputs--marked-ids
          (delete id mevedel-pending-inputs--marked-ids))
    (tabulated-list-put-tag " " t)))

(defun mevedel-pending-inputs--discard (entries)
  "Release reserved prompt context owned by ENTRIES."
  (dolist (entry entries)
    (when-let* ((submission (plist-get entry :submission)))
      (mevedel-prompt-submission-restore submission))))

(defun mevedel-pending-inputs-execute-deletions ()
  "Confirm and delete every marked pending input."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (steering
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     mevedel-pending-inputs--marked-ids))
           (mevedel-session-pending-steering session)))
         (follow-ups
          (cl-remove-if-not
           (lambda (entry)
             (member (plist-get entry :id)
                     mevedel-pending-inputs--marked-ids))
           (mevedel-session-pending-follow-ups session))))
    (unless (or steering follow-ups)
      (user-error "No pending input is marked for deletion"))
    (when (yes-or-no-p
           (format "Delete %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-session-set-pending-inputs
       session 'steering
       (cl-remove-if
        (lambda (entry) (memq entry steering))
        (mevedel-session-pending-steering session)))
      (mevedel-session-set-pending-inputs
       session 'follow-up
       (cl-remove-if
        (lambda (entry) (memq entry follow-ups))
        (mevedel-session-pending-follow-ups session)))
      (setq mevedel-pending-inputs--marked-ids nil)
      (mevedel-pending-inputs--refresh))))

(defun mevedel-pending-inputs-clear ()
  "Confirm and clear all pending input in the owning root session."
  (interactive)
  (let* ((context (mevedel-cockpit-current-context))
         (session (mevedel-pending-inputs--session context))
         (steering (mevedel-session-pending-steering session))
         (follow-ups (mevedel-session-pending-follow-ups session))
         (view (mevedel-cockpit-context-view-buffer context)))
    (unless (or steering follow-ups)
      (user-error "No pending input"))
    (when (and (buffer-live-p view)
               (buffer-local-value 'mevedel-view--pending-input-edit view))
      (user-error "Save or cancel the pending-input edit first"))
    (when (yes-or-no-p
           (format "Clear %d steering and %d follow-up pending input%s? "
                   (length steering) (length follow-ups)
                   (if (= 1 (+ (length steering) (length follow-ups)))
                       ""
                     "s")))
      (mevedel-pending-inputs--discard (append steering follow-ups))
      (mevedel-session-set-pending-inputs session 'steering nil)
      (mevedel-session-set-pending-inputs session 'follow-up nil)
      (when (buffer-live-p view)
        (with-current-buffer view
          (mevedel-view--interaction-rebuild)))
      (when (derived-mode-p 'mevedel-pending-inputs-mode)
        (setq mevedel-pending-inputs--marked-ids nil)
        (mevedel-cockpit-surface-refresh))
      (message "mevedel: cleared pending input"))))

(defun mevedel-pending-inputs-resume-after-failure ()
  "Clear failure pause after all failed-turn steering is resolved."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (failed
          (cl-remove-if-not
           (lambda (entry)
             (eq (plist-get entry :state) 'failed-turn))
           (mevedel-session-pending-steering session))))
    (unless (mevedel-session-pending-input-failure-paused session)
      (user-error "Pending input is not paused after a turn failure"))
    (when failed
      (user-error "Resolve %d failed-turn steering message%s first"
                  (length failed) (if (= 1 (length failed)) "" "s")))
    (mevedel-session-set-pending-input-failure-paused session nil)
    (mevedel-pending-inputs--refresh)
    (message "mevedel: pending-input failure pause cleared")))

(defun mevedel-pending-inputs-quit ()
  "Close the cockpit and resume eligible automatic delivery."
  (interactive)
  (let* ((context (mevedel-cockpit-surface-context))
         (session (mevedel-pending-inputs--session context))
         (view (mevedel-cockpit-context-view-buffer context))
         (failure-paused
          (mevedel-session-pending-input-failure-paused session))
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
    (unless failure-paused
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
        (gptel--fsm-transition fsm 'WAIT)))
    (when (and (not failure-paused) (buffer-live-p view))
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
           ("M-<up>" "Move up within category" mevedel-pending-inputs-move-up)
           ("M-<down>" "Move down within category"
            mevedel-pending-inputs-move-down)
           ("s" "Convert to steering" mevedel-pending-inputs-make-steering)
           ("f" "Convert to follow-up" mevedel-pending-inputs-make-follow-up)
           ("d" "Mark for deletion" mevedel-pending-inputs-mark-delete)
           ("u" "Unmark deletion" mevedel-pending-inputs-unmark)
           ("x" "Delete marked pending input"
            mevedel-pending-inputs-execute-deletions)
           ("R" "Resume after failure" mevedel-pending-inputs-resume-after-failure)
           ("C-c C-q" "Clear all pending input" mevedel-pending-inputs-clear)
           ("g" "Refresh live pending input" mevedel-pending-inputs-refresh)
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
                (mevedel-session-pending-follow-ups session)
                (mevedel-session-pending-input-failure-paused session))
      (user-error "No pending input"))
    (mevedel-cockpit-open-surface
     mevedel-pending-inputs--surface context)))

(provide 'mevedel-pending-inputs)
;;; mevedel-pending-inputs.el ends here
