;;; niwa-note-conversation.el -- Isolated note sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Explicit sends create ordinary durable sessions, never forks of a chat.
;; The normal view owns the read-only transcript, composer, requests and tools.
;; Notes retain only the session identity in the workspace instruction store.

;;; Code:

(require 'cl-lib)
(require 'niwa-chat)
(require 'niwa-note-frame)
(require 'niwa-file-state)
(require 'niwa-mentions)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-presets)
(require 'niwa-reminders)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-transfer)
(require 'niwa-session-persistence)
(require 'niwa-structs)
(require 'niwa-view-composer)

;; `niwa-note-change'
(declare-function niwa-note-change--note "niwa-note-change" (&optional note))
(autoload 'niwa-note-apply-change "niwa-note-change" nil t)
(autoload 'niwa-note-revert-change "niwa-note-change" nil t)
(autoload 'niwa-note-merge-change "niwa-note-merge" nil t)

;; `niwa-note-settings'
(declare-function niwa-note-settings-apply "niwa-note-settings" (settings data))

;; `gptel'
(defvar gptel--known-presets)

(defvar-keymap niwa-note-conversation-mode-map
  :doc "Note conversation actions; ordinary characters remain composer input."
  "C-c C-c" #'niwa-note-conversation-send-reply
  "C-c C-a" #'niwa-note-apply-change
  "C-c C-b" #'niwa-note-revert-change
  "C-c C-g" #'niwa-note-merge-change
  "C-c C-v" #'niwa-note-show-sent
  "C-c C-z" #'niwa-note-conversation-close)

(define-key niwa-note-conversation-mode-map
            [remap niwa-pending-inputs-save-edit]
            #'niwa-note-conversation-send-reply)

(defvar-local niwa-note-conversation--origin nil
  "Invoking window and point to restore when the conversation closes.")

(define-minor-mode niwa-note-conversation-mode
  "Note conversation controls in its transcript and composer."
  :lighter "" :interactive nil)

(defun niwa-note-conversation-attention-state (properties)
  "Project cached conversation work for PROPERTIES, a plist or live overlay."
  (when-let* ((id (if (overlayp properties)
                     (overlay-get properties 'niwa-note-session-id)
                   (plist-get properties 'niwa-note-session-id)))
              (data (niwa-session-control-transfer-root-buffer-for-id id)))
    (and (niwa-turn-busy-p data) 'working)))

(add-hook 'niwa-notes-attention-state-functions
          #'niwa-note-conversation-attention-state)

(defun niwa-note-conversation-send-reply ()
  "Send the note composer, or save an explicitly opened pending-input edit."
  (interactive)
  (with-current-buffer (or niwa-composer--view (current-buffer))
    (if niwa-view--pending-input-edit
        (niwa-pending-inputs-save-edit)
      (niwa-view-send))))

(defun niwa-note-conversation-close ()
  "Close this note's frame or window, keeping its conversation and draft."
  (interactive)
  (let* ((view (or niwa-composer--view (current-buffer)))
         (origin (buffer-local-value 'niwa-note-conversation--origin view)))
    (if (and (frame-live-p niwa-note-frame--frame)
             (eq view niwa-note-frame--view-buffer))
        (niwa-note-frame-close)
      (quit-window nil))
    (when (window-live-p (car origin))
      (select-window (car origin))
      (when (eq (window-buffer (car origin)) (marker-buffer (cdr origin)))
        (goto-char (cdr origin))))))

(defun niwa-note-conversation-record-input (session data)
  "Retain each fully assembled provider input DATA for SESSION's inspection.
DATA contains request text and tool schemas, never transport credentials."
  (when (niwa-session-note-conversation session)
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (let ((directory (file-name-concat (niwa-session-save-path session) "note-input")))
      (make-directory directory t)
      (let* ((last (car (last (directory-files directory nil "\\`[0-9]+\\.json\\'"))))
             (index (1+ (if last (string-to-number last) 0)))
             (file (file-name-concat directory (format "%08d.json" index))))
        (niwa--write-file-atomically file (gptel--json-encode data) 'utf-8-unix #o600)))))

(defun niwa-note-conversation--record-gptel-input ()
  "Retain each gptel input after all context injection has finished."
  (when niwa--current-request
    (niwa-note-conversation-record-input
     niwa--session
     (plist-get (niwa-engine-info (niwa-engine-owner niwa--current-request)) :data))))

(defun niwa-note-conversation-setup ()
  "Install note-specific controls and isolation on fresh and restored sessions."
  (setq-local gptel-context nil gptel-use-context nil)
  (when niwa--session
    (setf (niwa-session-reminders niwa--session)
          (cl-remove 'edited-file (niwa-session-reminders niwa--session)
                     :key #'niwa-reminder-type)))
  (add-hook 'gptel-post-request-hook #'niwa-note-conversation--record-gptel-input nil t)
  (when (buffer-live-p niwa--view-buffer)
    (with-current-buffer niwa--view-buffer
      (niwa-note-conversation-mode 1)
      (when-let* ((composer (niwa-view-composer-buffer)))
        (with-current-buffer composer (niwa-note-conversation-mode 1)))))
  (niwa-note-conversation-refresh-hint))

(defun niwa-note-conversation-refresh-hint (&optional note)
  "Refresh the cached composer hint when NOTE's prepared-change state changes.
Without NOTE, restore this session's hint during note conversation setup."
  (let* ((id (and note (overlay-get note 'niwa-note-session-id)))
         (data (if id (niwa-session-control-transfer-root-buffer-for-id id)
                 (and niwa--session
                      (niwa-session-note-conversation niwa--session)
                      (current-buffer))))
         (session (and data (buffer-local-value 'niwa--session data)))
         (view (and data (buffer-local-value 'niwa--view-buffer data))))
    (when (and session (niwa-session-note-conversation session)
               (buffer-live-p view))
      (let* ((waiting
              (or (and note (overlay-get note 'niwa-note-prepared-change))
                (cl-some
                 (lambda (entry)
                   (let ((properties (plist-get entry :properties)))
                     (and (equal (niwa-session-session-id session)
                                 (plist-get properties 'niwa-note-session-id))
                          (plist-get properties 'niwa-note-prepared-change))))
                 (niwa-notes-open (niwa-session-workspace session)))))
             (hint (propertize
                    (if waiting "C-c C-z close · C-c C-a apply · C-c C-g merge"
                      "C-c C-z close") 'face 'shadow)))
        (with-current-buffer view
          (unless (equal hint niwa-view--input-hint)
            (setq-local niwa-view--input-hint hint)
            (niwa-view-refresh-input-prompt)))))))

(defun niwa-note-show-sent ()
  "Show every exact provider input retained for this note conversation."
  (interactive)
  (let* ((data niwa--data-buffer)
         (session (buffer-local-value 'niwa--session data))
         (directory (file-name-concat (niwa-session-save-path session) "note-input"))
         (report (get-buffer-create (format "*Note sent: %s*" (niwa-session-session-id session)))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope session)
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

(defun niwa-note-conversation--literal (text)
  "Keep captured TEXT inert without changing ordinary at-sign code syntax."
  (let ((tokens (append (mapcar #'car niwa-mention-handlers)
                        (mapcar (lambda (preset)
                                  (concat "@" (regexp-quote (format "%s" (car preset))) "\\_>"))
                                gptel--known-presets))))
    (propertize
     (replace-regexp-in-string
      (concat "\\(^\\|[[:space:]]\\)\\(" (mapconcat #'identity tokens "\\|") "\\)")
      "\\1\\\\\\2" text)
     'niwa-mention-literal t)))

(defun niwa-note-conversation--tree (note)
  "Return NOTE's request tree with each parent and excluded conversation."
  (let* ((start (overlay-start note)) (end (overlay-end note))
         (session-id (overlay-get note 'niwa-note-session-id))
         (children (make-hash-table :test #'eq))
         (parents (list note)))
    (dolist (item (sort (niwa--wholly-contained-instructions (overlay-buffer note) start end)
                        :key (lambda (ov) (- (overlay-end ov) (overlay-start ov)))
                        :reverse t))
      (when (and (eq 'note (overlay-get item 'niwa-instruction-type))
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
                  (let* ((id (overlay-get item 'niwa-note-session-id))
                         (excluded (and parent id (not (equal session-id id)))))
                    (cons (list :note item :parent parent :excluded excluded)
                          (unless excluded
                            (mapcan (lambda (child) (walk child item))
                                    (sort (gethash item children) :key #'overlay-start)))))))
      (walk note nil))))

(defun niwa-note-conversation--batch-tree (notes)
  "Combine NOTES' request trees without resending covered descendants."
  (let ((trees (mapcar #'niwa-note-conversation--tree
                       (delete-dups (copy-sequence notes))))
        (covered (make-hash-table :test #'eq))
        (included (make-hash-table :test #'eq)))
    (dolist (tree trees)
      (dolist (entry (cdr tree))
        (unless (plist-get entry :excluded)
          (puthash (plist-get entry :note) t covered))))
    (cl-loop for tree in trees
             unless (gethash (plist-get (car tree) :note) covered)
             nconc (cl-loop for entry in tree
                            for note = (unless (plist-get entry :excluded)
                                         (plist-get entry :note))
                            unless (and note (gethash note included))
                            collect entry and do (when note (puthash note t included))))))

(defun niwa-note-conversation--block (note session &optional parent)
  "Capture NOTE's authored text, code and explicit attachments for SESSION.
If PARENT is non-nil, name it instead of repeating the shared code.
Escape mention syntax in the frozen result so later requests, reloads and
compaction cannot reinterpret captured code or attached prose as new input.
Only note/ref mentions are frozen here; other authored mentions use the
ordinary request-time transform, where media has an active request owner."
  (let ((expansion
         (let ((niwa-mention-handlers
                (cl-remove-if-not (lambda (handler) (memq (cadr handler) '(note ref)))
                                  niwa-mention-handlers)))
           (niwa-mentions-expand-user-input
            (niwa-mentions-prepare-user-input
             (or (overlay-get note 'niwa-note-text) "") session)
            session t))))
    (with-current-buffer (overlay-buffer note)
      (format "%s\n\n%s%s\n\n%s"
              (if parent
                  (format "Note %s at line %d, inside Note %s"
                          (overlay-get note 'niwa-id)
                          (line-number-at-pos (overlay-start note))
                          (overlay-get parent 'niwa-id))
                (format "Note %s at %s:%d" (overlay-get note 'niwa-id) buffer-file-name
                        (line-number-at-pos (overlay-start note))))
              (plist-get expansion :text)
              (if parent ""
                (format "\n\nNoted code at send time:\n```\n%s\n```"
                        (niwa-note-conversation--literal
                         (buffer-substring-no-properties (overlay-start note) (overlay-end note)))))
              (niwa-note-conversation--literal
               (mapconcat (lambda (item) (plist-get item :body))
                          (plist-get expansion :reminder-items) "\n\n"))))))

(defun niwa-note-conversation--prompt (note session)
  "Build the exact generated request for NOTE and its descendants in SESSION."
  (niwa-note-conversation--render (niwa-note-conversation--tree note) session))

(defun niwa-note-conversation--render (tree session)
  "Render a note request TREE for SESSION."
  (mapconcat
   (lambda (entry)
     (let ((item (plist-get entry :note)) (parent (plist-get entry :parent)))
       (if (plist-get entry :excluded)
           (with-current-buffer (overlay-buffer item)
             (format "Note %s at line %d, inside Note %s, has its own conversation and is not part of this request."
                     (overlay-get item 'niwa-id) (line-number-at-pos (overlay-start item))
                     (overlay-get parent 'niwa-id)))
         (niwa-note-conversation--block item session parent))))
   tree
   "\n\n"))

;;;###autoload
(defun niwa-note-preview-prompt (&optional note)
  "Show the generated send text for NOTE without sending or creating a session.
Provider policy, request-time mentions and tools are assembled only on send."
  (interactive)
  (setq note (or note (niwa-notes--at-point)))
  (let* ((workspace (niwa--instruction-buffer-workspace (overlay-buffer note)))
         (session (niwa-session-create
                   "Note preview" workspace
                   (with-current-buffer (overlay-buffer note) default-directory)))
         (text (niwa-note-conversation--prompt note session))
         (buffer (get-buffer-create "*niwa-note-preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min))
        (special-mode)
        (visual-line-mode 1)))
    (pop-to-buffer buffer)
    buffer))

(cl-defun niwa-note-conversation--send-notes (notes &key no-display defer-write)
  "Send or reopen NOTES in one session and return its view.
NO-DISPLAY leaves windows and focus alone.  DEFER-WRITE leaves the store
sync to the caller's batch."
  (unless notes (user-error "Choose at least one note"))
  (require 'niwa-note-settings)
  (let* ((note (car notes))
         (workspace (when (and (overlayp note) (overlay-buffer note))
                      (niwa--instruction-buffer-workspace (overlay-buffer note)))))
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (dolist (item notes)
        (unless (and workspace (overlayp item) (overlay-buffer item)
                     (eq (overlay-get item 'niwa-instruction-type) 'note)
                     (equal (niwa--instruction-workspace-key workspace)
                            (niwa--instruction-workspace-key
                             (niwa--instruction-buffer-workspace (overlay-buffer item)))))
          (user-error "Send notes from one workspace together")))
      (let* ((origin (unless no-display (cons (selected-window) (copy-marker (point)))))
             (id (overlay-get note 'niwa-note-session-id))
             (_ (unless (cl-every
                         (lambda (item) (equal id (overlay-get item 'niwa-note-session-id)))
                         notes)
                  (user-error "Notes already belong to different conversations; send them separately")))
             (data (if id
                       (or (niwa-session-control-transfer-root-buffer-for-id id)
                           (niwa-session-persistence-resume-id workspace id)
                           (user-error "Note conversation is unavailable: %s" id))
                     (niwa--chat-buffer nil t workspace
                                           (with-current-buffer (overlay-buffer note)
                                             (file-name-directory buffer-file-name)))))
             (session (buffer-local-value 'niwa--session data))
             (initial (or (not id)
                          (and (zerop (or (niwa-session-turn-count session) 0))
                               (cl-some (lambda (item)
                                          (eq 'unsent (overlay-get item 'niwa-note-state)))
                                        notes))))
             (tree (and initial (niwa-note-conversation--batch-tree notes)))
             (members (if initial
                          (cl-loop for entry in tree unless (plist-get entry :excluded)
                                   collect (plist-get entry :note))
                        notes)))
        (dolist (item members)
          (when (eq 'buddy (overlay-get item 'niwa-note-author))
            (niwa-note-keep item)))
        (condition-case err
            (with-current-buffer data
              (if id
                  (niwa-note-conversation-setup)
                (setf (niwa-session-note-conversation session) t)
                (let ((settings (overlay-get note 'niwa-note-settings)))
                  (unless (plist-member settings :preset)
                    (niwa-preset-apply 'niwa-implement data))
                  (niwa-note-settings-apply settings data))
                (let ((niwa-instruction-store--inhibit
                       (if defer-write t niwa-instruction-store--inhibit)))
                  (niwa-session-artifacts-save session data nil t)))
              (when initial
                (niwa--submit-generated-turn
                 (niwa-note-conversation--render tree session))))
          ((error quit)
           (unless id (niwa-chat-discard-buffers data t))
           (signal (car err) (cdr err))))
        (when (or initial (cl-some (lambda (item)
                                     (not (eq 'sent (overlay-get item 'niwa-note-state))))
                                   notes))
          (niwa-note-mark-sent members (niwa-session-session-id session))
          (unless defer-write (niwa-instruction-store-flush t)))
        (let ((view (buffer-local-value 'niwa--view-buffer data)))
          (unless no-display
            (with-current-buffer view
              (when niwa-note-conversation--origin
                (set-marker (cdr niwa-note-conversation--origin) nil))
              (setq-local niwa-note-conversation--origin origin))
            (niwa-note-frame-display note view t)
            (with-current-buffer view (goto-char (point-max))))
          view)))))

;;;###autoload
(cl-defun niwa-note-send (&optional note &key no-display defer-write)
  "Send NOTE in its own session, or reopen its existing conversation.
Default to the note at point.  Return the conversation view.
NO-DISPLAY leaves focus alone; DEFER-WRITE lets a batch caller sync the store."
  (interactive)
  (niwa-note-conversation--send-notes (list (or note (niwa-notes--at-point)))
                                       :no-display no-display :defer-write defer-write))

;;;###autoload
(cl-defun niwa-note-send-batch (notes &key no-display defer-write)
  "Send NOTES together in one conversation and return its view.
An already sent batch reopens its session.  Mixing existing conversations
refuses rather than losing either thread.  Interactively, choose loaded notes.
NO-DISPLAY leaves focus alone; DEFER-WRITE lets a batch caller sync the store."
  (interactive
   (progn
     (niwa--instruction-activate-buffer)
     (let* ((choices (cl-loop for note in (niwa--all-instructions)
                              when (eq (overlay-get note 'niwa-instruction-type) 'note)
                              collect (cons (number-to-string (overlay-get note 'niwa-id)) note)))
            (completion-extra-properties
             (list :annotation-function
                   (lambda (id)
                     (concat " " (niwa-notes--display-text (cdr (assoc id choices))))))))
       (list (mapcar (lambda (choice) (cdr (assoc choice choices)))
                     (completing-read-multiple "Send notes: " choices nil t))))))
  (niwa-note-conversation--send-notes (delete-dups (copy-sequence notes))
                                       :no-display no-display :defer-write defer-write))

(provide 'niwa-note-conversation)
;;; niwa-note-conversation.el ends here
