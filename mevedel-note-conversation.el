;;; mevedel-note-conversation.el -- Isolated note sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Explicit sends create ordinary durable sessions, never forks of a chat.
;; The normal view owns the read-only transcript, composer, requests and tools.
;; Notes retain only the session identity in the workspace instruction store.

;;; Code:

(require 'cl-lib)
(require 'mevedel-chat)
(require 'mevedel-note-frame)
(require 'mevedel-file-state)
(require 'mevedel-mentions)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-presets)
(require 'mevedel-reminders)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view-composer)

;; `mevedel-note-change'
(declare-function mevedel-note-change--note "mevedel-note-change" (&optional note))
(autoload 'mevedel-note-apply-change "mevedel-note-change" nil t)
(autoload 'mevedel-note-revert-change "mevedel-note-change" nil t)
(autoload 'mevedel-note-merge-change "mevedel-note-merge" nil t)

;; `mevedel-note-settings'
(declare-function mevedel-note-settings-apply "mevedel-note-settings" (settings data))

;; `gptel'
(defvar gptel--known-presets)

(defvar-keymap mevedel-note-conversation-mode-map
  :doc "Note conversation actions; ordinary characters remain composer input."
  "C-c C-c" #'mevedel-view-send
  "C-c C-a" #'mevedel-note-apply-change
  "C-c C-b" #'mevedel-note-revert-change
  "C-c C-g" #'mevedel-note-merge-change
  "C-c C-v" #'mevedel-note-show-sent
  "C-c C-z" #'mevedel-note-conversation-close)

(define-key mevedel-note-conversation-mode-map
            [remap mevedel-pending-inputs-save-edit]
            #'mevedel-note-conversation-send-reply)

(defvar-local mevedel-note-conversation--origin nil
  "Invoking window and point to restore when the conversation closes.")

(define-minor-mode mevedel-note-conversation-mode
  "Note conversation controls on an ordinary session view."
  :lighter "" :interactive nil)

(defun mevedel-note-conversation-attention-state (properties)
  "Project cached conversation work for PROPERTIES, a plist or live overlay."
  (when-let* ((id (if (overlayp properties)
                     (overlay-get properties 'mevedel-note-session-id)
                   (plist-get properties 'mevedel-note-session-id)))
              (data (mevedel-session-control-transfer-root-buffer-for-id id)))
    (and (mevedel-turn-busy-p data) 'working)))

(add-hook 'mevedel-notes-attention-state-functions
          #'mevedel-note-conversation-attention-state)

(defun mevedel-note-conversation-send-reply ()
  "Send the note composer, or save an explicitly opened pending-input edit."
  (interactive)
  (if mevedel-view--pending-input-edit
      (mevedel-pending-inputs-save-edit)
    (mevedel-view-send)))

(defun mevedel-note-conversation-close ()
  "Close this note's frame or window, keeping its conversation and draft."
  (interactive)
  (let ((origin mevedel-note-conversation--origin))
    (if (and (frame-live-p mevedel-note-frame--frame)
             (eq (current-buffer) mevedel-note-frame--view-buffer))
        (mevedel-note-frame-close)
      (quit-window nil))
    (when (window-live-p (car origin))
      (select-window (car origin))
      (when (eq (window-buffer (car origin)) (marker-buffer (cdr origin)))
        (goto-char (cdr origin))))))

(defun mevedel-note-conversation-record-input (session data)
  "Retain each fully assembled provider input DATA for SESSION's inspection.
DATA contains request text and tool schemas, never transport credentials."
  (when (mevedel-session-note-conversation session)
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (let ((directory (file-name-concat (mevedel-session-save-path session) "note-input")))
      (make-directory directory t)
      (let* ((last (car (last (directory-files directory nil "\\`[0-9]+\\.json\\'"))))
             (index (1+ (if last (string-to-number last) 0)))
             (file (file-name-concat directory (format "%08d.json" index))))
        (mevedel--write-file-atomically file (gptel--json-encode data) 'utf-8-unix #o600)))))

(defun mevedel-note-conversation--record-gptel-input ()
  "Retain each gptel input after all context injection has finished."
  (when mevedel--current-request
    (mevedel-note-conversation-record-input
     mevedel--session
     (plist-get (mevedel-engine-info (mevedel-engine-owner mevedel--current-request)) :data))))

(defun mevedel-note-conversation-setup ()
  "Install note-specific controls and isolation on fresh and restored sessions."
  (setq-local gptel-context nil gptel-use-context nil)
  (when mevedel--session
    (setf (mevedel-session-reminders mevedel--session)
          (cl-remove 'edited-file (mevedel-session-reminders mevedel--session)
                     :key #'mevedel-reminder-type)))
  (add-hook 'gptel-post-request-hook #'mevedel-note-conversation--record-gptel-input nil t)
  (when (buffer-live-p mevedel--view-buffer)
    (with-current-buffer mevedel--view-buffer (mevedel-note-conversation-mode 1)))
  (mevedel-note-conversation-refresh-hint))

(defun mevedel-note-conversation-refresh-hint (&optional note)
  "Refresh the cached composer hint when NOTE's prepared-change state changes.
Without NOTE, restore this session's hint during note conversation setup."
  (let* ((id (and note (overlay-get note 'mevedel-note-session-id)))
         (data (if id (mevedel-session-control-transfer-root-buffer-for-id id)
                 (and mevedel--session
                      (mevedel-session-note-conversation mevedel--session)
                      (current-buffer))))
         (session (and data (buffer-local-value 'mevedel--session data)))
         (view (and data (buffer-local-value 'mevedel--view-buffer data))))
    (when (and session (mevedel-session-note-conversation session)
               (buffer-live-p view))
      (let* ((waiting
              (or (and note (overlay-get note 'mevedel-note-prepared-change))
                (cl-some
                 (lambda (entry)
                   (let ((properties (plist-get entry :properties)))
                     (and (equal (mevedel-session-session-id session)
                                 (plist-get properties 'mevedel-note-session-id))
                          (plist-get properties 'mevedel-note-prepared-change))))
                 (mevedel-notes-open (mevedel-session-workspace session)))))
             (hint (propertize
                    (if waiting "C-c C-z close · C-c C-a apply · C-c C-g merge"
                      "C-c C-z close") 'face 'shadow)))
        (with-current-buffer view
          (unless (equal hint mevedel-view--input-hint)
            (setq-local mevedel-view--input-hint hint)
            (mevedel-view-refresh-input-prompt)))))))

(defun mevedel-note-show-sent ()
  "Show every exact provider input retained for this note conversation."
  (interactive)
  (let* ((data mevedel--data-buffer)
         (session (buffer-local-value 'mevedel--session data))
         (directory (file-name-concat (mevedel-session-save-path session) "note-input"))
         (report (get-buffer-create (format "*Note sent: %s*" (mevedel-session-session-id session)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope session)
      (with-current-buffer report
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (file (directory-files directory t "\\`[0-9]+\\.json\\'"))
            (unless (bobp) (insert "\n\n"))
            (insert-file-contents file)
            (goto-char (point-max)))
          (special-mode)))
      (display-buffer report)
      report)))

(defun mevedel-note-conversation--literal (text)
  "Keep captured TEXT inert without changing ordinary at-sign code syntax."
  (let ((tokens (append (mapcar #'car mevedel-mention-handlers)
                        (mapcar (lambda (preset)
                                  (concat "@" (regexp-quote (format "%s" (car preset))) "\\_>"))
                                gptel--known-presets))))
    (propertize
     (replace-regexp-in-string
      (concat "\\(^\\|[[:space:]]\\)\\(" (mapconcat #'identity tokens "\\|") "\\)")
      "\\1\\\\\\2" text)
     'mevedel-mention-literal t)))

(defun mevedel-note-conversation--tree (note)
  "Return NOTE's request tree with each parent and excluded conversation."
  (let* ((start (overlay-start note)) (end (overlay-end note))
         (session-id (overlay-get note 'mevedel-note-session-id))
         (children (make-hash-table :test #'eq))
         (parents (list note)))
    (dolist (item (sort (mevedel--wholly-contained-instructions (overlay-buffer note) start end)
                        :key (lambda (ov) (- (overlay-end ov) (overlay-start ov)))
                        :reverse t))
      (when (and (eq 'note (overlay-get item 'mevedel-instruction-type))
                 (or (> (overlay-start item) start) (< (overlay-end item) end)))
        ;; The most recently seen containing range is the narrowest parent.
        (let ((parent (cl-find-if (lambda (ov)
                                   (and (<= (overlay-start ov) (overlay-start item))
                                        (>= (overlay-end ov) (overlay-end item))
                                        (or (< (overlay-start ov) (overlay-start item))
                                            (> (overlay-end ov) (overlay-end item)))))
                                 parents)))
          (push item (gethash parent children))
          (push item parents))))
    (cl-labels ((walk (item parent)
                  (let* ((id (overlay-get item 'mevedel-note-session-id))
                         (excluded (and parent id (not (equal session-id id)))))
                    (cons (list :note item :parent parent :excluded excluded)
                          (unless excluded
                            (mapcan (lambda (child) (walk child item))
                                    (sort (gethash item children) :key #'overlay-start)))))))
      (walk note nil))))

(defun mevedel-note-conversation--roots (notes)
  "Return NOTES without duplicates or descendants covered by another selection."
  (cl-remove-if
   (lambda (note)
     (cl-some (lambda (parent)
                (and (not (eq parent note))
                     (cl-find note
                              (mevedel-note-conversation--tree parent)
                              :key (lambda (entry)
                                     (unless (plist-get entry :excluded) (plist-get entry :note))))))
              notes))
   (delete-dups (copy-sequence notes))))

(defun mevedel-note-conversation--block (note session &optional parent)
  "Capture NOTE's authored text, code and explicit attachments for SESSION.
If PARENT is non-nil, name it instead of repeating the shared code.
Escape mention syntax in the frozen result so later requests, reloads and
compaction cannot reinterpret captured code or attached prose as new input.
Only note/ref mentions are frozen here; other authored mentions use the
ordinary request-time transform, where media has an active request owner."
  (let ((expansion
         (let ((mevedel-mention-handlers
                (cl-remove-if-not (lambda (handler) (memq (cadr handler) '(note ref)))
                                  mevedel-mention-handlers)))
           (mevedel-mentions-expand-user-input
            (mevedel-mentions-prepare-user-input
             (or (overlay-get note 'mevedel-note-text) "") session)
            session t))))
    (with-current-buffer (overlay-buffer note)
      (format "%s\n\n%s%s\n\n%s"
              (if parent
                  (format "Note %s at line %d, inside Note %s"
                          (overlay-get note 'mevedel-id)
                          (line-number-at-pos (overlay-start note))
                          (overlay-get parent 'mevedel-id))
                (format "Note %s at %s:%d" (overlay-get note 'mevedel-id) buffer-file-name
                        (line-number-at-pos (overlay-start note))))
              (plist-get expansion :text)
              (if parent ""
                (format "\n\nNoted code at send time:\n```\n%s\n```"
                        (mevedel-note-conversation--literal
                         (buffer-substring-no-properties (overlay-start note) (overlay-end note)))))
              (mevedel-note-conversation--literal
               (mapconcat (lambda (item) (plist-get item :body))
                          (plist-get expansion :reminder-items) "\n\n"))))))

(defun mevedel-note-conversation--prompt (note session)
  "Build the exact generated request for NOTE and its descendants in SESSION."
  (mevedel-note-conversation--render (mevedel-note-conversation--tree note) session))

(defun mevedel-note-conversation--render (tree session)
  "Render a note request TREE for SESSION."
  (mapconcat
   (lambda (entry)
     (let ((item (plist-get entry :note)) (parent (plist-get entry :parent)))
       (if (plist-get entry :excluded)
           (with-current-buffer (overlay-buffer item)
             (format "Note %s at line %d, inside Note %s, has its own conversation and is not part of this request."
                     (overlay-get item 'mevedel-id) (line-number-at-pos (overlay-start item))
                     (overlay-get parent 'mevedel-id)))
         (mevedel-note-conversation--block item session parent))))
   tree
   "\n\n"))

;;;###autoload
(defun mevedel-note-preview-prompt (&optional note)
  "Show the generated send text for NOTE without sending or creating a session.
Provider policy, request-time mentions and tools are assembled only on send."
  (interactive)
  (setq note (or note (mevedel-notes--at-point)))
  (let* ((workspace (mevedel--instruction-buffer-workspace (overlay-buffer note)))
         (session (mevedel-session-create
                   "Note preview" workspace
                   (with-current-buffer (overlay-buffer note) default-directory)))
         (text (mevedel-note-conversation--prompt note session))
         (buffer (get-buffer-create "*mevedel-note-preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (special-mode)
        (visual-line-mode 1)))
    (pop-to-buffer buffer)
    buffer))

(cl-defun mevedel-note-conversation--send-notes (notes &key no-display defer-write)
  "Send or reopen NOTES in one session and return its view.
NO-DISPLAY leaves windows and focus alone.  DEFER-WRITE leaves the store
sync to the caller's batch."
  (unless notes (user-error "Choose at least one note"))
  (require 'mevedel-note-settings)
  (let* ((note (car notes))
         (workspace (when (and (overlayp note) (overlay-buffer note))
                      (mevedel--instruction-buffer-workspace (overlay-buffer note)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (dolist (item notes)
        (unless (and workspace (overlayp item) (overlay-buffer item)
                     (eq (overlay-get item 'mevedel-instruction-type) 'note)
                     (equal (mevedel--instruction-workspace-key workspace)
                            (mevedel--instruction-workspace-key
                             (mevedel--instruction-buffer-workspace (overlay-buffer item)))))
          (user-error "Send notes from one workspace together")))
      (let* ((origin (unless no-display (cons (selected-window) (copy-marker (point)))))
             (id (overlay-get note 'mevedel-note-session-id))
             (_ (unless (cl-every
                         (lambda (item) (equal id (overlay-get item 'mevedel-note-session-id)))
                         notes)
                  (user-error "Notes already belong to different conversations; send them separately")))
             (data (if id
                       (or (mevedel-session-control-transfer-root-buffer-for-id id)
                           (mevedel-session-persistence-resume-id workspace id)
                           (user-error "Note conversation is unavailable: %s" id))
                     (mevedel--chat-buffer nil t workspace
                                           (with-current-buffer (overlay-buffer note)
                                             (file-name-directory buffer-file-name)))))
             (session (buffer-local-value 'mevedel--session data))
             (initial (or (not id)
                          (and (zerop (or (mevedel-session-turn-count session) 0))
                               (cl-some (lambda (item)
                                          (eq 'unsent (overlay-get item 'mevedel-note-state)))
                                        notes))))
             (tree (and initial
                        (cl-delete-duplicates
                         (mapcan #'mevedel-note-conversation--tree
                                 (mevedel-note-conversation--roots notes))
                         :key (lambda (entry)
                                (unless (plist-get entry :excluded) (plist-get entry :note)))
                         :test (lambda (a b) (and a (eq a b))) :from-end t)))
             (members (if initial
                          (cl-loop for entry in tree unless (plist-get entry :excluded)
                                   collect (plist-get entry :note))
                        notes)))
        (dolist (item members)
          (when (eq 'buddy (overlay-get item 'mevedel-note-author))
            (mevedel-note-keep item)))
        (condition-case err
            (with-current-buffer data
              (if id
                  (mevedel-note-conversation-setup)
                (setf (mevedel-session-note-conversation session) t)
                (let ((settings (overlay-get note 'mevedel-note-settings)))
                  (unless (plist-member settings :preset)
                    (mevedel-preset-apply 'mevedel-implement data))
                  (mevedel-note-settings-apply settings data))
                (let ((mevedel-instruction-store--inhibit
                       (if defer-write t mevedel-instruction-store--inhibit)))
                  (mevedel-session-artifacts-save session data nil t)))
              (when initial
                (mevedel--submit-generated-turn
                 (mevedel-note-conversation--render tree session))))
          ((error quit)
           (unless id (mevedel-chat-discard-buffers data t))
           (signal (car err) (cdr err))))
        (when (or initial (cl-some (lambda (item)
                                     (not (eq 'sent (overlay-get item 'mevedel-note-state))))
                                   notes))
          (mevedel-note-mark-sent members (mevedel-session-session-id session))
          (unless defer-write (mevedel-instruction-store-flush t)))
        (let ((view (buffer-local-value 'mevedel--view-buffer data)))
          (unless no-display
            (with-current-buffer view
              (when mevedel-note-conversation--origin
                (set-marker (cdr mevedel-note-conversation--origin) nil))
              (setq-local mevedel-note-conversation--origin origin))
            (mevedel-note-frame-display note view t)
            (with-current-buffer view (goto-char (point-max))))
          view)))))

;;;###autoload
(cl-defun mevedel-note-send (&optional note &key no-display defer-write)
  "Send NOTE in its own session, or reopen its existing conversation.
Default to the note at point.  Return the conversation view.
NO-DISPLAY leaves focus alone; DEFER-WRITE lets a batch caller sync the store."
  (interactive)
  (mevedel-note-conversation--send-notes (list (or note (mevedel-notes--at-point)))
                                       :no-display no-display :defer-write defer-write))

;;;###autoload
(cl-defun mevedel-note-send-batch (notes &key no-display defer-write)
  "Send NOTES together in one conversation and return its view.
An already sent batch reopens its session.  Mixing existing conversations
refuses rather than losing either thread.  Interactively, choose loaded notes.
NO-DISPLAY leaves focus alone; DEFER-WRITE lets a batch caller sync the store."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let* ((choices (cl-loop for note in (mevedel--all-instructions)
                              when (eq (overlay-get note 'mevedel-instruction-type) 'note)
                              collect (cons (number-to-string (overlay-get note 'mevedel-id)) note)))
            (completion-extra-properties
             (list :annotation-function
                   (lambda (id)
                     (concat " " (mevedel-notes--display-text (cdr (assoc id choices))))))))
       (list (mapcar (lambda (choice) (cdr (assoc choice choices)))
                     (completing-read-multiple "Send notes: " choices nil t))))))
  (mevedel-note-conversation--send-notes (delete-dups (copy-sequence notes))
                                       :no-display no-display :defer-write defer-write))

(provide 'mevedel-note-conversation)
;;; mevedel-note-conversation.el ends here
