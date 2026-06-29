;;; mevedel-tool-plan.el -- Planning tool -*- lexical-binding: t -*-

;;; Commentary:

;; Conversational Plan-mode support.  The model emits
;; <proposed_plan>...</proposed_plan>; mevedel extracts the block,
;; persists the latest plan as a session artifact, and displays it
;; inline with interactive controls for the user.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)
(require 'mevedel-utilities)

;; `mevedel-queue'
(declare-function mevedel-queue--entry-metadata-get "mevedel-queue"
                  (entry key))
(declare-function mevedel-queue--entry-metadata-put "mevedel-queue"
                  (entry key value))

;; `mevedel-chat'
(declare-function mevedel--implement-plan "mevedel-chat" (action-plist))
(declare-function mevedel-session-plan-queue "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-plan-metadata "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-queued-user-messages
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel-plans-directory)
(defvar mevedel--session)
(defvar mevedel-permission-mode)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(declare-function gptel-send "ext:gptel" (&optional arg))

;; `mevedel-permissions'
(declare-function mevedel-permission-mode-normalize
                  "mevedel-permissions" (mode))
(declare-function mevedel-permission-mode-set-raw
                  "mevedel-permissions" (mode))
(declare-function mevedel-permission-mode-apply-auto-lifecycle
                  "mevedel-permissions"
                  (previous-mode target-mode &optional session))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-ensure-files
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))

;; `mevedel-skills'
(declare-function mevedel-skills--refresh-view-input-prompt "mevedel-skills" ())

;; `mevedel-reminders'
(declare-function mevedel-session-ensure-reminder
                  "mevedel-reminders" (session reminder))
(declare-function mevedel-session-remove-reminder
                  "mevedel-reminders" (session type))
(declare-function mevedel-reminders-make-plan-mode "mevedel-reminders" ())
(declare-function mevedel-reminders-make-plan-mode-reentry
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-plan-mode-exit
                  "mevedel-reminders" ())
(declare-function mevedel-reminders-make-plan-reference
                  "mevedel-reminders" ())

;; `mevedel-tool-ui'
(declare-function mevedel--prompt--settle "mevedel-tool-ui" (overlay outcome))

;; `mevedel-view'
(declare-function mevedel-view--interaction-anchor "mevedel-view" ())
(declare-function mevedel-view--interaction-register "mevedel-view"
                  (descriptor))
(declare-function mevedel-view--interaction-target-buffer "mevedel-view"
                  (&optional data-buffer))
(declare-function mevedel-view--fontify-as "mevedel-view" (text mode))
(declare-function mevedel-view--begin-external-turn
                  "mevedel-view"
                  (display-text data-turn-start &optional kind hook-context))
(declare-function mevedel-view--clear-input "mevedel-view" ())
(declare-function mevedel-view--input-start "mevedel-view" ())
(defvar mevedel--view-buffer)


;;
;;; Planning

(defconst mevedel-plan-mode--open-tag "<proposed_plan>"
  "Opening tag for Plan-mode proposed plans.")

(defconst mevedel-plan-mode--close-tag "</proposed_plan>"
  "Closing tag for Plan-mode proposed plans.")

(defconst mevedel-plan-mode--relative-plan-path
  (file-name-concat "plans" "current.md")
  "Relative path of the current Plan-mode artifact under a session dir.")

(defun mevedel-plan-mode-extract-proposed-plan (text)
  "Return the last proposed-plan body found in TEXT, or nil.
Only exact line-oriented `<proposed_plan>' blocks are recognized."
  (let ((case-fold-search nil)
        found)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan-mode--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((body-start (point)))
          (when (re-search-forward
                 (concat "^" (regexp-quote mevedel-plan-mode--close-tag)
                         "[ \t]*$")
                 nil t)
            (setq found
                  (string-trim-right
                   (buffer-substring-no-properties
                    body-start (match-beginning 0)))))))
      found)))

(defun mevedel-plan-mode-strip-proposed-plans (text)
  "Return TEXT with proposed-plan blocks removed."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              (concat "^" (regexp-quote mevedel-plan-mode--open-tag)
                      "[ \t]*\n")
              nil t)
        (let ((start (match-beginning 0)))
          (if (re-search-forward
               (concat "^" (regexp-quote mevedel-plan-mode--close-tag)
                       "[ \t]*\n?")
               nil t)
              (delete-region start (match-end 0))
            (delete-region start (point-max)))
          (goto-char start)))
      (string-trim (buffer-string)))))

(defun mevedel-plan-mode--metadata (&optional session)
  "Return SESSION's plan metadata plist."
  (mevedel-session-plan-metadata (or session mevedel--session)))

(defun mevedel-plan-mode--metadata-put (session key value)
  "Set KEY to VALUE in SESSION's plan metadata."
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata (plist-put metadata key value))
    (setf (mevedel-session-plan-metadata session) metadata)
    metadata))

(defun mevedel-plan-mode--plan-hash (plan-markdown)
  "Return a stable hash for PLAN-MARKDOWN."
  (secure-hash 'sha256 (string-trim-right (or plan-markdown ""))))

(defun mevedel-plan-mode-known-proposed-plan-p (plan-markdown &optional session)
  "Return non-nil if PLAN-MARKDOWN was presented in SESSION."
  (when-let* ((session (or session mevedel--session))
              (hashes (plist-get (mevedel-session-plan-metadata session)
                                 :presented-plan-hashes)))
    (member (mevedel-plan-mode--plan-hash plan-markdown) hashes)))

(defun mevedel-plan-mode--workspace-plan-path (session)
  "Return a workspace-level plan artifact path for non-persistent SESSION."
  (when-let* ((workspace (mevedel-session-workspace session))
              (root (mevedel-workspace-root workspace)))
    (let* ((plans-dir (if (and (boundp 'mevedel-plans-directory)
                               mevedel-plans-directory)
                          mevedel-plans-directory
                        (file-name-concat ".mevedel" "plans")))
           (dir (if (file-name-absolute-p plans-dir)
                    plans-dir
                  (expand-file-name plans-dir root)))
           (filename (format "plan-%s.md"
                             (format-time-string "%Y%m%d-%H%M%S"))))
      (file-name-concat dir filename))))

(defun mevedel-plan-mode-current-plan-path (&optional session buffer)
  "Return the session-local current plan path for SESSION.
Materializes the session directory when needed.  BUFFER defaults to the
current data buffer."
  (let* ((session (or session mevedel--session))
         (buffer (or buffer (current-buffer)))
         (save-path (or (mevedel-session-save-path session)
                        (progn
                          (require 'mevedel-session-persistence)
                          (mevedel-session-persistence-ensure-files
                           session buffer)))))
    (if save-path
        (file-name-concat save-path mevedel-plan-mode--relative-plan-path)
      (mevedel-plan-mode--workspace-plan-path session))))

(defun mevedel-plan-mode--metadata-plan-path (session)
  "Return SESSION's recorded plan artifact path, when available."
  (let ((metadata (mevedel-session-plan-metadata session)))
    (or (when-let* ((save-path (mevedel-session-save-path session))
                    (path (or (plist-get metadata :path)
                              mevedel-plan-mode--relative-plan-path)))
          (file-name-concat save-path path))
        (plist-get metadata :absolute-path))))

(defun mevedel-plan-mode--write-current-plan (plan-markdown session buffer)
  "Write PLAN-MARKDOWN to SESSION's current plan artifact for BUFFER.
Returns the absolute path."
  (let ((path (mevedel-plan-mode-current-plan-path session buffer))
        (plan-markdown (mevedel--normalize-message-text plan-markdown)))
    (unless path
      (error "Could not materialize session plan path"))
    (make-directory (file-name-directory path) t)
    (write-region plan-markdown nil path nil 'silent)
    (let ((turn (or (mevedel-session-turn-count session) 0))
          (metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                       nil)))
          (hash (mevedel-plan-mode--plan-hash plan-markdown)))
      (setq metadata (plist-put metadata :path
                                mevedel-plan-mode--relative-plan-path))
      (setq metadata (plist-put metadata :absolute-path path))
      (setq metadata (plist-put metadata :status 'presented))
      (setq metadata (plist-put metadata :updated-turn turn))
      (setq metadata (plist-put metadata :updated-at
                                (format-time-string "%FT%H-%M-%S")))
      (setq metadata
            (plist-put metadata :presented-plan-hashes
                       (cl-remove-duplicates
                        (cons hash
                              (plist-get metadata :presented-plan-hashes))
                        :test #'equal)))
      (setq metadata (plist-put metadata :verification-pending nil))
      (setq metadata (plist-put metadata :approved-turn nil))
      (setq metadata (plist-put metadata :approved-at nil))
      (setf (mevedel-session-plan-metadata session) metadata))
    path))

(defun mevedel-plan-mode--archive-accepted-plan (plan-path session)
  "Copy PLAN-PATH to a timestamped accepted-plan artifact for SESSION.
Returns a plist with `:path' and `:absolute-path' keys."
  (unless (and plan-path (file-exists-p plan-path))
    (error "Accepted plan artifact does not exist"))
  (let* ((dir (file-name-directory plan-path))
         (timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (archive-path (file-name-concat
                        dir (format "accepted-%s.md" timestamp)))
         (index 1))
    (while (file-exists-p archive-path)
      (setq archive-path
            (file-name-concat dir (format "accepted-%s-%d.md"
                                          timestamp index)))
      (setq index (1+ index)))
    (copy-file plan-path archive-path)
    (list :path (when-let* ((save-path (mevedel-session-save-path session)))
                  (file-relative-name archive-path save-path))
          :absolute-path archive-path)))

(defun mevedel-plan-mode--current-plan-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan-mode--metadata-plan-path session)))
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (mevedel--normalize-message-text (buffer-string)))))))

(defun mevedel-plan-mode--current-plan-exists-p (&optional session)
  "Return non-nil when SESSION has a current plan artifact on disk."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan-mode--metadata-plan-path session)))
      (and path (file-exists-p path)))))

(defun mevedel-plan-mode--set-mode (mode)
  "Set permission MODE for current session and refresh the view prompt."
  (require 'mevedel-permissions)
  (mevedel-permission-mode-set-raw mode)
  (when (fboundp 'mevedel-skills--refresh-view-input-prompt)
    (mevedel-skills--refresh-view-input-prompt)))

(defun mevedel-plan-mode--effective-permission-mode (&optional session)
  "Return the effective permission mode for SESSION."
  (or (and session (mevedel-session-permission-mode session))
      (and (boundp 'mevedel-permission-mode) mevedel-permission-mode)
      'default))

(defun mevedel-plan-mode-enter (&optional prompt display-text hook-context)
  "Enter conversational Plan mode.
When PROMPT is non-empty, send it as the first Plan-mode user turn.
DISPLAY-TEXT and HOOK-CONTEXT control the visible turn when PROMPT
contains model-only context."
  (unless (bound-and-true-p mevedel--session)
    (user-error "No mevedel session in this buffer"))
  (let ((previous
         (mevedel-plan-mode--effective-permission-mode mevedel--session)))
    (unless (eq previous 'plan)
      (mevedel-plan-mode--metadata-put
       mevedel--session :previous-permission-mode previous))
    (require 'mevedel-reminders)
    (mevedel-plan-mode--set-mode 'plan)
    (mevedel-permission-mode-apply-auto-lifecycle
     previous 'plan mevedel--session)
    (mevedel-session-remove-reminder mevedel--session 'plan-mode-exit)
    (mevedel-session-ensure-reminder
     mevedel--session (mevedel-reminders-make-plan-mode))
    (when (mevedel-plan-mode--current-plan-exists-p mevedel--session)
      (mevedel-session-ensure-reminder
       mevedel--session (mevedel-reminders-make-plan-mode-reentry)))
    (message "mevedel: plan mode on")
    (when (and prompt (not (string-blank-p prompt)))
      (mevedel-plan-mode--insert-and-send prompt display-text hook-context))))

(defun mevedel-plan-mode-exit (&optional target-mode)
  "Exit Plan mode and restore TARGET-MODE or the recorded prior mode."
  (when (bound-and-true-p mevedel--session)
    (require 'mevedel-reminders)
    (require 'mevedel-permissions)
    (let* ((previous
            (mevedel-plan-mode--effective-permission-mode mevedel--session))
           (metadata (mevedel-session-plan-metadata mevedel--session))
           (restore (or target-mode
                        (plist-get metadata :previous-permission-mode)
                        'default)))
      (setq restore (mevedel-permission-mode-normalize restore))
      (when (eq restore 'plan)
        (setq restore 'default))
      (mevedel-plan-mode--metadata-put
       mevedel--session :previous-permission-mode nil)
      (mevedel-session-remove-reminder mevedel--session 'plan-mode)
      (mevedel-session-remove-reminder mevedel--session 'plan-mode-reentry)
      (mevedel-plan-mode--set-mode restore)
      (mevedel-permission-mode-apply-auto-lifecycle
       previous restore mevedel--session)
      (mevedel-session-ensure-reminder
       mevedel--session (mevedel-reminders-make-plan-mode-exit))
      restore)))

(defun mevedel-plan-mode-restore-reminders (&optional session)
  "Restore Plan-mode reminders for resumed SESSION when it is in Plan mode."
  (let ((session (or session mevedel--session)))
    (when (and session
               (eq (mevedel-session-permission-mode session) 'plan))
      (require 'mevedel-reminders)
      (mevedel-session-remove-reminder session 'plan-mode-exit)
      (mevedel-session-ensure-reminder
       session (mevedel-reminders-make-plan-mode))
      (when (mevedel-plan-mode--current-plan-exists-p session)
        (mevedel-session-ensure-reminder
         session (mevedel-reminders-make-plan-mode-reentry))))))

(defun mevedel-plan-mode--insert-and-send
    (prompt &optional display-text hook-context)
  "Insert PROMPT as a user turn in the current data buffer and send it.
DISPLAY-TEXT is shown in the view instead of PROMPT.  HOOK-CONTEXT is
shown as a collapsed hook-context disclosure."
  (goto-char (point-max))
  (let ((user-turn-start (point)))
    (insert gptel-response-separator)
    (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
      (unless (and (>= (point) (+ (point-min) (length prefix)))
                   (string= (buffer-substring-no-properties
                             (- (point) (length prefix)) (point))
                            prefix))
        (unless (bolp) (insert "\n"))
        (insert prefix)))
    (insert prompt "\n")
    (mevedel--clear-user-turn-gptel-properties user-turn-start (point)))
  (let ((data-turn-start (copy-marker (point) nil)))
    (when-let* ((view (and (boundp 'mevedel--view-buffer)
                           mevedel--view-buffer))
                ((buffer-live-p view))
                ((fboundp 'mevedel-view--begin-external-turn)))
      (with-current-buffer view
        (mevedel-view--begin-external-turn
         (or display-text prompt) data-turn-start nil hook-context))))
  (gptel-send))

(defun mevedel-plan-queue--current-session ()
  "Resolve the session struct that owns the plan approval FIFO."
  (mevedel-queue--current-session))

(defvar mevedel-plan-queue--spec
  (mevedel-queue-spec--create
   :name 'plan-queue
   :get (lambda (session) (mevedel-session-plan-queue session))
   :set (lambda (session queue)
          (setf (mevedel-session-plan-queue session) queue))
   :render #'mevedel-plan-queue--render-entry
   :settle (lambda (entry outcome)
             (when-let* ((callback (plist-get entry :callback)))
               (funcall callback outcome)))
   :entry-origin (lambda (entry) (plist-get entry :origin)))
  "Shared FIFO spec for plan approval confirmations.")

(defun mevedel-plan-queue--get (&optional session)
  "Return SESSION's plan queue."
  (when-let* ((sess (or session (mevedel-plan-queue--current-session))))
    (mevedel-queue--get mevedel-plan-queue--spec sess)))

(defun mevedel-plan-queue--enqueue (entry)
  "Append plan approval ENTRY to the session FIFO and render the head."
  (mevedel-queue--enqueue mevedel-plan-queue--spec entry))

(defun mevedel-plan-queue--render-head (&optional session)
  "Render the current head of SESSION's plan approval FIFO."
  (mevedel-queue--render-head mevedel-plan-queue--spec
                              (or session
                                  (mevedel-plan-queue--current-session))))

(defun mevedel-plan-mode--ensure-implementation-allowed (entry outcome)
  "Signal when ENTRY may not be implemented with OUTCOME yet."
  (when (and (mevedel-plan-mode--approval-action outcome)
             (mevedel-session-queued-user-messages
              (plist-get entry :session)))
    (user-error
     "Resolve queued messages before implementing the plan (edit or clear the queued message batch)")))

(defun mevedel-plan-queue--on-head-outcome (entry outcome)
  "Settle plan approval ENTRY with OUTCOME and render the next head."
  (mevedel-plan-mode--ensure-implementation-allowed entry outcome)
  (mevedel-queue--pop mevedel-plan-queue--spec entry outcome))

(defun mevedel-plan-queue-abort-all (&optional session)
  "Flush SESSION's plan FIFO, firing `aborted' on every entry."
  (mevedel-queue--abort-all mevedel-plan-queue--spec 'aborted session))

(defun mevedel-plan-queue-sweep-agent (origin &optional session)
  "Abort queued SESSION plans whose `:origin' matches ORIGIN."
  (mevedel-queue--sweep-origin
   mevedel-plan-queue--spec origin 'aborted session))

(defconst mevedel-plan-mode--implementation-modes
  '(default accept-edits trust-all)
  "Plan implementation permission modes cycled by the approval prompt.")

(defun mevedel-plan-mode--implementation-mode-label (mode)
  "Return compact display label for implementation MODE."
  (pcase mode
    ('accept-edits "edit")
    ('trust-all "auto")
    (_ "default")))

(defun mevedel-plan-mode--next-implementation-mode (mode)
  "Return the next implementation mode after MODE."
  (let* ((modes mevedel-plan-mode--implementation-modes)
         (tail (memq mode modes)))
    (or (cadr tail) (car modes))))

(defun mevedel-plan-queue--entry-implementation-mode (entry)
  "Return ENTRY's selected implementation permission mode."
  (or (mevedel-queue--entry-metadata-get entry :implementation-mode)
      'default))

(defun mevedel-plan-queue--cycle-entry-implementation-mode (entry)
  "Cycle ENTRY's implementation mode and rerender the plan prompt."
  (mevedel-queue--entry-metadata-put
   entry :implementation-mode
   (mevedel-plan-mode--next-implementation-mode
    (mevedel-plan-queue--entry-implementation-mode entry)))
  (mevedel-plan-queue--render-entry entry))

(defun mevedel-plan-queue--keys-line (implementation-mode)
  "Return the plan approval key help line for IMPLEMENTATION-MODE."
  (concat
   (propertize "Keys: " 'font-lock-face 'help-key-binding)
   (propertize "RET" 'font-lock-face 'help-key-binding)
   " implement  "
   (propertize "I" 'font-lock-face 'help-key-binding)
   " implement (clear context)  "
   (propertize "TAB" 'font-lock-face 'help-key-binding)
   "/"
   (propertize "m" 'font-lock-face 'help-key-binding)
   (format " mode: %s  "
           (mevedel-plan-mode--implementation-mode-label
            implementation-mode))
   (propertize "f" 'font-lock-face 'help-key-binding)
   " feedback draft  "
   (propertize "q" 'font-lock-face 'help-key-binding)
   " cancel\n"))

(defun mevedel-plan-queue--display-body (plan-markdown)
  "Return PLAN-MARKDOWN fontified for the plan approval interaction zone."
  (if (fboundp 'mevedel-view--fontify-as)
      (mevedel-view--fontify-as plan-markdown 'markdown-mode)
    plan-markdown))

(defun mevedel-plan-queue--render-entry (entry)
  "Render plan approval queue ENTRY in the interaction zone."
  (let ((plan-markdown (plist-get entry :body))
        (chat-buffer (plist-get entry :chat-buffer))
        overlay)
    (cl-labels
        ((settle (sym)
           (when overlay
             (mevedel--prompt--settle overlay sym)))
         (implementation-outcome (action)
           (list :action action
                 :mode (mevedel-plan-queue--entry-implementation-mode
                        entry)))
         (implement-plan ()
           (interactive)
           (let ((outcome (implementation-outcome 'implement)))
             (mevedel-plan-mode--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (implement-plan-clear ()
           (interactive)
           (let ((outcome (implementation-outcome 'implement-clear)))
             (mevedel-plan-mode--ensure-implementation-allowed entry outcome)
             (settle outcome)))
         (cycle-implementation-mode ()
           (interactive)
           (mevedel-plan-queue--cycle-entry-implementation-mode entry))
         (reject-plan-feedback ()
           (interactive)
           (settle 'feedback-draft))
         (abort-plan ()
           (interactive) (settle 'aborted)))
      (let ((target-buf
             (if (fboundp 'mevedel-view--interaction-target-buffer)
                 (mevedel-view--interaction-target-buffer chat-buffer)
               (error "No live view for queued prompt"))))
        (mevedel-queue--entry-metadata-put entry :view-buffer target-buf)
        (with-current-buffer target-buf
          (let* ((keymap (make-sparse-keymap))
                 (interaction-id (or (mevedel-queue--entry-metadata-get
                                      entry :interaction-id)
                                     (let ((id (list :plan (gensym "plan-"))))
                                       (mevedel-queue--entry-metadata-put
                                        entry :interaction-id id)
                                       id)))
                 (body
                  (concat
                   "\n"
                   (mevedel-plan-queue--display-body plan-markdown)
                   "\n\n"
                   (mevedel-plan-queue--keys-line
                    (mevedel-plan-queue--entry-implementation-mode entry))
                   (propertize
                    "\n" 'font-lock-face
                    '(:inherit font-lock-string-face :underline t :extend t)))))
            (define-key keymap (kbd "RET") #'implement-plan)
            (define-key keymap (kbd "<return>") #'implement-plan)
            (define-key keymap (kbd "i") #'implement-plan)
            (define-key keymap (kbd "C-c C-c") #'implement-plan)
            (define-key keymap (kbd "I") #'implement-plan-clear)
            (define-key keymap (kbd "TAB") #'cycle-implementation-mode)
            (define-key keymap (kbd "<tab>") #'cycle-implementation-mode)
            (define-key keymap (kbd "m") #'cycle-implementation-mode)
            (define-key keymap (kbd "f") #'reject-plan-feedback)
            (define-key keymap (kbd "q") #'abort-plan)
            (define-key keymap (kbd "C-c C-k") #'abort-plan)
            (define-key keymap (kbd "C-g") #'abort-plan)
            (setq overlay
                  (mevedel-view--interaction-register
                   (list :kind 'plan
                         :id interaction-id
                         :count (length (mevedel-plan-queue--get
                                         (plist-get entry :session)))
                         :body body
                         :priority 200
                         :keymap keymap
                         :help-echo nil
                         :entry entry
                         :activate
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
                            entry outcome)))))
            (overlay-put overlay 'mevedel-plan t)
            (overlay-put overlay 'mevedel-user-request t)
            (overlay-put overlay 'mevedel--callback
                         (lambda (outcome)
                           (mevedel-plan-queue--on-head-outcome
                            entry outcome)))
            (overlay-put overlay 'keymap keymap)
            (when (and (overlay-buffer overlay)
                       (= (point) (overlay-start overlay)))
              (when-let* ((buf-win (get-buffer-window target-buf)))
                (with-selected-window buf-win
                  (recenter-top-bottom 1))))))))))

(defun mevedel-plan-mode--mark-approved
    (session plan-path accepted-plan)
  "Mark SESSION's current plan artifact at PLAN-PATH as approved.
ACCEPTED-PLAN is metadata for the archived accepted-plan artifact."
  (let ((metadata (copy-sequence (or (mevedel-session-plan-metadata session)
                                     nil))))
    (setq metadata
          (plist-put metadata :path mevedel-plan-mode--relative-plan-path))
    (setq metadata (plist-put metadata :status 'approved))
    (setq metadata (plist-put metadata :approved-turn
                              (or (mevedel-session-turn-count session) 0)))
    (setq metadata (plist-put metadata :approved-at
                              (format-time-string "%FT%H-%M-%S")))
    (setq metadata (plist-put metadata :verification-pending t))
    (setq metadata (plist-put metadata :absolute-path plan-path))
    (setq metadata (plist-put metadata :accepted-path
                              (plist-get accepted-plan :path)))
    (setq metadata (plist-put metadata :accepted-absolute-path
                              (plist-get accepted-plan :absolute-path)))
    (setf (mevedel-session-plan-metadata session) metadata)
    (require 'mevedel-reminders)
    (mevedel-session-remove-reminder session 'plan-reference)
    (mevedel-session-ensure-reminder
     session (mevedel-reminders-make-plan-reference))))

(defun mevedel-plan-mode--save-session-state (session buffer)
  "Persist SESSION state from BUFFER when session persistence is enabled."
  (require 'mevedel-session-persistence)
  (condition-case err
      (mevedel-session-persistence-save session buffer)
    (error
     (display-warning
      'mevedel
      (format "Could not persist plan state: %S" err)
      :warning))))

(defun mevedel-plan-mode--mark-rejected (session)
  "Mark SESSION's current plan as rejected."
  (mevedel-plan-mode--metadata-put session :status 'rejected)
  (mevedel-plan-mode--metadata-put session :verification-pending nil))

(defun mevedel-plan-mode--feedback-draft-text (plan-path &optional feedback)
  "Return editable Plan feedback draft text referencing PLAN-PATH.
When FEEDBACK is non-nil, prefill it in the feedback section."
  (format
   "Plan feedback:\n\n%s\n\nRevise the proposed plan to address the feedback. Treat the current plan artifact as reference-only: read it if needed, but do not edit it. When the revised plan is decision-complete, emit exactly one full replacement <proposed_plan> block.\n\nCurrent plan artifact: %s"
   (or feedback "")
   (or plan-path "latest plan artifact")))

(defun mevedel-plan-mode--insert-feedback-draft
    (chat-buffer plan-path &optional feedback)
  "Insert an editable Plan feedback draft for PLAN-PATH in CHAT-BUFFER's view.
When FEEDBACK is non-nil, prefill it in the feedback section."
  (let ((target-buffer
         (if (fboundp 'mevedel-view--interaction-target-buffer)
             (mevedel-view--interaction-target-buffer chat-buffer)
           (error "No live view for Plan feedback draft"))))
    (unless (buffer-live-p target-buffer)
      (error "No live view for Plan feedback draft"))
    (with-current-buffer target-buffer
      (mevedel-view--clear-input)
      (goto-char (mevedel-view--input-start))
      (let ((start (point)))
        (insert (mevedel-plan-mode--feedback-draft-text plan-path feedback))
        (goto-char start))
      (when (search-forward "Plan feedback:\n\n" nil t)
        (goto-char (match-end 0)))
      (when-let* ((window (get-buffer-window target-buffer)))
        (select-window window)))))

(defun mevedel-plan-mode-clear-verification-pending (&optional session)
  "Clear SESSION's approved-plan verification pending flag."
  (when-let* ((session (or session (and (boundp 'mevedel--session)
                                        mevedel--session))))
    (mevedel-plan-mode--metadata-put session :verification-pending nil)))

(defun mevedel-plan-mode--approval-action (outcome)
  "Return implementation action represented by OUTCOME, or nil."
  (cond
   ((memq outcome '(implement implement-clear)) outcome)
   ((and (consp outcome)
         (memq (plist-get outcome :action) '(implement implement-clear)))
    (plist-get outcome :action))))

(defun mevedel-plan-mode--approval-implementation-mode (outcome)
  "Return implementation permission mode represented by OUTCOME."
  (let ((mode (and (consp outcome) (plist-get outcome :mode))))
    (if (memq mode '(accept-edits trust-all))
        mode
      'default)))

(defun mevedel-plan-mode--approval-callback
    (plan-markdown chat-buffer outcome)
  "Handle OUTCOME for a proposed PLAN-MARKDOWN in CHAT-BUFFER."
  (setq plan-markdown (mevedel--normalize-message-text plan-markdown))
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (if-let* ((action (mevedel-plan-mode--approval-action outcome)))
          (let* ((path (mevedel-plan-mode--write-current-plan
	               plan-markdown mevedel--session chat-buffer))
                (accepted-plan
                 (mevedel-plan-mode--archive-accepted-plan
                  path mevedel--session))
                (implementation-mode
                 (mevedel-plan-mode--approval-implementation-mode outcome)))
	    (mevedel-plan-mode--mark-approved
             mevedel--session path accepted-plan)
	    (mevedel-plan-mode-exit)
            (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
	    (mevedel--implement-plan
	     (list :action action
                   :plan-file path
                   :plan-markdown plan-markdown
                   :permission-mode implementation-mode)))
        (pcase outcome
        (`(feedback . ,text)
         (let ((path (mevedel-plan-mode--metadata-plan-path mevedel--session)))
           (mevedel-plan-mode--mark-rejected mevedel--session)
           (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
           (mevedel-plan-mode--insert-feedback-draft chat-buffer path text)))
        ('feedback-draft
         (let ((path (mevedel-plan-mode--metadata-plan-path mevedel--session)))
           (mevedel-plan-mode--mark-rejected mevedel--session)
           (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
           (mevedel-plan-mode--insert-feedback-draft chat-buffer path)))
        ('aborted
         (mevedel-plan-mode--metadata-put
          mevedel--session :status 'cancelled)
         (mevedel-plan-mode--metadata-put
          mevedel--session :verification-pending nil)
         (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
         (message "mevedel: plan approval cancelled"))
        (_
         (message "mevedel: unknown plan outcome %S" outcome)))))))

(defun mevedel-plan-mode--approval-entry
    (plan-markdown chat-buffer session)
  "Return a plan approval queue entry for PLAN-MARKDOWN in CHAT-BUFFER SESSION."
  (list :body plan-markdown
        :chat-buffer chat-buffer
        :origin "main"
        :session session
        :callback
        (lambda (outcome)
          (mevedel-plan-mode--approval-callback
           plan-markdown chat-buffer outcome))))

(defun mevedel-plan-mode-present (plan-markdown &optional chat-buffer)
  "Present PLAN-MARKDOWN for approval in CHAT-BUFFER.
The latest presented plan is persisted to the session-local plan
artifact before the approval prompt is displayed."
  (setq plan-markdown (mevedel--normalize-message-text plan-markdown))
  (let ((chat-buffer (or chat-buffer (current-buffer))))
    (with-current-buffer chat-buffer
      (mevedel-tools--validate-params
          nil mevedel-plan-mode-present
        (plan-markdown (stringp . "string")))
      (unless (string-blank-p plan-markdown)
        (mevedel-plan-mode--write-current-plan
         plan-markdown mevedel--session chat-buffer)
        (mevedel-plan-queue--enqueue
         (mevedel-plan-mode--approval-entry
          plan-markdown chat-buffer mevedel--session))))))

(defun mevedel-plan-mode-restore-pending-approval
    (&optional session chat-buffer)
  "Restore pending approval for SESSION in CHAT-BUFFER if needed."
  (let* ((chat-buffer (or chat-buffer (current-buffer)))
         (session (or session (and (boundp 'mevedel--session)
                                   mevedel--session)))
         (metadata (and session (mevedel-session-plan-metadata session))))
    (when (and session
               (eq (plist-get metadata :status) 'presented)
               (null (mevedel-session-plan-queue session)))
      (when-let* ((plan-markdown
                   (mevedel-plan-mode--current-plan-body session))
                  ((not (string-blank-p plan-markdown))))
        (mevedel-plan-queue--enqueue
         (mevedel-plan-mode--approval-entry
          plan-markdown chat-buffer session))))))

(defun mevedel-plan-mode--response-text (start end)
  "Return response text between START and END in the current buffer."
  (mevedel--normalize-message-text
   (buffer-substring-no-properties start end)))

(defun mevedel-plan-mode--post-response (start end)
  "Detect `<proposed_plan>' blocks between START and END."
  (when (and (bound-and-true-p mevedel--session)
             (eq (mevedel-session-permission-mode mevedel--session) 'plan))
    (when-let* ((plan (mevedel-plan-mode-extract-proposed-plan
                       (mevedel-plan-mode--response-text start end))))
      (mevedel-plan-mode-present plan (current-buffer)))))


(provide 'mevedel-tool-plan)
;;; mevedel-tool-plan.el ends here
