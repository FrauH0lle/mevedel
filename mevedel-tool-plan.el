;;; mevedel-tool-plan.el -- Planning tool -*- lexical-binding: t -*-

;;; Commentary:

;; Conversational Plan-mode support.  The model emits
;; <proposed_plan>...</proposed_plan>; mevedel extracts the block,
;; persists the latest plan as a session artifact, and displays it
;; inline with interactive controls for the user.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))
(require 'mevedel-queue)

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
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel-plans-directory)
(defvar mevedel--session)
(defvar mevedel-permission-mode)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(declare-function gptel-send "ext:gptel" (&optional arg))

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
  "Write PLAN-MARKDOWN to SESSION's current plan artifact.
Returns the absolute path."
  (let ((path (mevedel-plan-mode-current-plan-path session buffer)))
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

(defun mevedel-plan-mode--current-plan-body (&optional session)
  "Return SESSION's current plan artifact contents, or nil."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan-mode--metadata-plan-path session)))
      (when (and path (file-exists-p path))
        (with-temp-buffer
          (insert-file-contents path)
          (buffer-string))))))

(defun mevedel-plan-mode--current-plan-exists-p (&optional session)
  "Return non-nil when SESSION has a current plan artifact on disk."
  (when-let* ((session (or session mevedel--session)))
    (let ((path (mevedel-plan-mode--metadata-plan-path session)))
      (and path (file-exists-p path)))))

(defun mevedel-plan-mode--set-mode (mode)
  "Set permission MODE for current session and refresh the view prompt."
  (setopt mevedel-permission-mode mode)
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
    (let* ((metadata (mevedel-session-plan-metadata mevedel--session))
           (restore (or target-mode
                        (plist-get metadata :previous-permission-mode)
                        'default)))
      (when (eq restore 'plan)
        (setq restore 'default))
      (mevedel-plan-mode--metadata-put
       mevedel--session :previous-permission-mode nil)
      (mevedel-session-remove-reminder mevedel--session 'plan-mode)
      (mevedel-session-remove-reminder mevedel--session 'plan-mode-reentry)
      (mevedel-plan-mode--set-mode restore)
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
  (insert gptel-response-separator)
  (when-let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist)))
    (unless (and (>= (point) (+ (point-min) (length prefix)))
                 (string= (buffer-substring-no-properties
                           (- (point) (length prefix)) (point))
                          prefix))
      (unless (bolp) (insert "\n"))
      (insert prefix)))
  (insert prompt "\n")
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

(defun mevedel-plan-queue--set (queue &optional session)
  "Set SESSION's plan queue to QUEUE."
  (when-let* ((sess (or session (mevedel-plan-queue--current-session))))
    (mevedel-queue--set mevedel-plan-queue--spec sess queue)))

(defun mevedel-plan-queue--enqueue (entry)
  "Append plan approval ENTRY to the session FIFO and render the head."
  (mevedel-queue--enqueue mevedel-plan-queue--spec entry))

(defun mevedel-plan-queue--render-head (&optional session)
  "Render the current head of SESSION's plan approval FIFO."
  (mevedel-queue--render-head mevedel-plan-queue--spec
                              (or session
                                  (mevedel-plan-queue--current-session))))

(defun mevedel-plan-queue--on-head-outcome (entry outcome)
  "Settle plan approval ENTRY with OUTCOME and render the next head."
  (mevedel-queue--pop mevedel-plan-queue--spec entry outcome))

(defun mevedel-plan-queue-abort-all (&optional session)
  "Flush SESSION's plan FIFO, firing `aborted' on every entry."
  (mevedel-queue--abort-all mevedel-plan-queue--spec 'aborted session))

(defun mevedel-plan-queue-sweep-agent (origin &optional session)
  "Abort queued plans whose `:origin' matches ORIGIN."
  (mevedel-queue--sweep-origin
   mevedel-plan-queue--spec origin 'aborted session))

(defun mevedel-plan-queue--keys-line ()
  "Return the plan approval key help line."
  (concat
   (propertize "Keys: " 'font-lock-face 'help-key-binding)
   (propertize "RET" 'font-lock-face 'help-key-binding)
   " implement  "
   (propertize "I" 'font-lock-face 'help-key-binding)
   " implement (clear context)  "
   (propertize "f" 'font-lock-face 'help-key-binding)
   " feedback  "
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
         (implement-plan ()
           (interactive) (settle 'implement))
         (implement-plan-clear ()
           (interactive) (settle 'implement-clear))
         (reject-plan-feedback ()
           (interactive)
           (let ((feedback (read-string "Feedback on this plan: ")))
             (settle (cons 'feedback feedback))))
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
                   (mevedel-plan-queue--keys-line)
                   (propertize
                    "\n" 'font-lock-face
                    '(:inherit font-lock-string-face :underline t :extend t)))))
            (define-key keymap (kbd "RET") #'implement-plan)
            (define-key keymap (kbd "<return>") #'implement-plan)
            (define-key keymap (kbd "i") #'implement-plan)
            (define-key keymap (kbd "C-c C-c") #'implement-plan)
            (define-key keymap (kbd "I") #'implement-plan-clear)
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

(defun mevedel-plan-mode--mark-approved (session plan-path)
  "Mark SESSION's current plan artifact at PLAN-PATH as approved."
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

(defun mevedel-plan-mode-clear-verification-pending (&optional session)
  "Clear SESSION's approved-plan verification pending flag."
  (when-let* ((session (or session (and (boundp 'mevedel--session)
                                        mevedel--session))))
    (mevedel-plan-mode--metadata-put session :verification-pending nil)))

(defun mevedel-plan-mode--approval-callback
    (plan-markdown chat-buffer outcome)
  "Handle OUTCOME for a proposed PLAN-MARKDOWN in CHAT-BUFFER."
  (when (buffer-live-p chat-buffer)
    (with-current-buffer chat-buffer
      (pcase outcome
	 ((or 'implement 'implement-clear)
	 (let ((path (mevedel-plan-mode--write-current-plan
	              plan-markdown mevedel--session chat-buffer)))
	   (mevedel-plan-mode--mark-approved mevedel--session path)
	   (mevedel-plan-mode-exit)
           (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
	   (mevedel--implement-plan
	    (list :action outcome
                  :plan-file path
                  :plan-markdown plan-markdown))))
        (`(feedback . ,text)
         (mevedel-plan-mode--mark-rejected mevedel--session)
         (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
         (mevedel-plan-mode--insert-and-send
          (format
           "Plan feedback:\n\n%s\n\nOriginal proposed plan:\n\n%s\n\nRevise the plan to address the feedback. When the revised plan is decision-complete, emit exactly one <proposed_plan> block."
           text plan-markdown)))
        ('aborted
         (mevedel-plan-mode--metadata-put
          mevedel--session :status 'cancelled)
         (mevedel-plan-mode--metadata-put
          mevedel--session :verification-pending nil)
         (mevedel-plan-mode--save-session-state mevedel--session chat-buffer)
         (message "mevedel: plan approval cancelled"))
        (_
         (message "mevedel: unknown plan outcome %S" outcome))))))

(defun mevedel-plan-mode--approval-entry
    (plan-markdown chat-buffer session)
  "Return a plan approval queue entry for PLAN-MARKDOWN."
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
  "Restore a presented plan approval prompt for SESSION if needed."
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
  (buffer-substring-no-properties start end))

(defun mevedel-plan-mode--post-response (start end)
  "Detect `<proposed_plan>' blocks in completed Plan-mode responses."
  (when (and (bound-and-true-p mevedel--session)
             (eq (mevedel-session-permission-mode mevedel--session) 'plan))
    (when-let* ((plan (mevedel-plan-mode-extract-proposed-plan
                       (mevedel-plan-mode--response-text start end))))
      (mevedel-plan-mode-present plan (current-buffer)))))


(provide 'mevedel-tool-plan)
;;; mevedel-tool-plan.el ends here
