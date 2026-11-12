;;; mevedel-note-conversation.el -- Isolated note sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Explicit sends create ordinary durable sessions, never forks of a chat.
;; The normal view owns the read-only transcript, composer, requests and tools.
;; Notes retain only the session identity in the workspace instruction store.

;;; Code:

(require 'cl-lib)
(require 'mevedel-chat)
(require 'mevedel-directive-frame)
(require 'mevedel-file-state)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-presets)
(require 'mevedel-reminders)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-persistence)
(require 'mevedel-structs)
(require 'mevedel-view-composer)

(defvar-keymap mevedel-note-conversation-mode-map
  :doc "Note conversation actions; ordinary characters remain composer input."
  "C-c C-c" #'mevedel-view-send
  "C-c C-v" #'mevedel-note-show-sent
  "C-c C-q" #'mevedel-note-conversation-close
  "C-c C-z" #'mevedel-note-conversation-close)

(define-key mevedel-note-conversation-mode-map
            [remap mevedel-pending-inputs-save-edit]
            #'mevedel-note-conversation-send-reply)

(defvar-local mevedel-note-conversation--origin nil
  "Invoking window and point to restore when the conversation closes.")

(define-minor-mode mevedel-note-conversation-mode
  "Note conversation controls on an ordinary session view."
  :lighter "" :interactive nil)

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
    (if (and (frame-live-p mevedel-directive-frame--frame)
             (eq (current-buffer) mevedel-directive-frame--view-buffer))
        (mevedel-directive-frame-close)
      (quit-window nil))
    (when (window-live-p (car origin))
      (select-window (car origin))
      (when (eq (window-buffer (car origin)) (marker-buffer (cdr origin)))
        (goto-char (cdr origin))))))

(defun mevedel-note-conversation-record-input (session data)
  "Retain SESSION's first fully assembled provider input DATA for inspection.
DATA contains request text and tool schemas, never transport credentials."
  (when (eq (mevedel-session-preset-name session) 'mevedel-note)
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (let ((file (file-name-concat (mevedel-session-save-path session) "note-input.json")))
      (unless (file-exists-p file)
        (mevedel--write-file-atomically file (gptel--json-encode data) 'utf-8-unix #o600)))))

(defun mevedel-note-conversation--record-gptel-input ()
  "Retain the initial gptel input after all context injection has finished."
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
    (with-current-buffer mevedel--view-buffer (mevedel-note-conversation-mode 1))))

(defun mevedel-note-show-sent ()
  "Show the exact initial provider input retained for this note conversation."
  (interactive)
  (let* ((data mevedel--data-buffer)
         (session (buffer-local-value 'mevedel--session data))
         (file (file-name-concat (mevedel-session-save-path session) "note-input.json"))
         (report (get-buffer-create (format "*Note sent: %s*" (mevedel-session-session-id session)))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope session)
      (with-current-buffer report
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents file)
          (special-mode)))
      (display-buffer report)
      report)))

(defun mevedel-note-conversation--prompt (note)
  "Capture NOTE's authored text and current code as a request."
  (with-current-buffer (overlay-buffer note)
    (format "Note %s at %s:%d\n\n%s\n\nNoted code at send time:\n```\n%s\n```"
            (overlay-get note 'mevedel-id) buffer-file-name
            (line-number-at-pos (overlay-start note))
            (or (overlay-get note 'mevedel-note-text) "")
            (buffer-substring-no-properties (overlay-start note) (overlay-end note)))))

(defun mevedel-note-conversation--send-notes (notes)
  "Send or reopen NOTES in one session and return its focused view."
  (unless notes (user-error "Choose at least one note"))
  (let* ((note (car notes))
         (workspace (mevedel--instruction-buffer-workspace (overlay-buffer note))))
    (mevedel-with-diagnostic-errors (mevedel-diagnostic-scope workspace)
      (dolist (item notes)
        (unless (and (overlayp item) (overlay-buffer item)
                     (eq (overlay-get item 'mevedel-instruction-type) 'note)
                     (equal (mevedel--instruction-workspace-key workspace)
                            (mevedel--instruction-workspace-key
                             (mevedel--instruction-buffer-workspace (overlay-buffer item)))))
          (user-error "Send notes from one workspace together")))
      (let* ((origin (cons (selected-window) (copy-marker (point))))
             (id (overlay-get note 'mevedel-note-session-id))
             (_ (unless (cl-every
                         (lambda (item) (equal id (overlay-get item 'mevedel-note-session-id)))
                         notes)
                  (user-error "Notes already belong to different conversations; send them separately")))
             (prompt (unless id (mapconcat #'mevedel-note-conversation--prompt notes "\n\n")))
             (data (if id
                       (or (cdr (cl-find-if
                                 (lambda (entry)
                                   (equal id (mevedel-session-session-id
                                              (buffer-local-value 'mevedel--session (cdr entry)))))
                                 (mevedel--workspace-sessions workspace)))
                           (mevedel-session-persistence-resume-id workspace id)
                           (user-error "Note conversation is unavailable: %s" id))
                     (mevedel--chat-buffer nil t workspace
                                           (with-current-buffer (overlay-buffer note)
                                             (file-name-directory buffer-file-name)))))
             (session (buffer-local-value 'mevedel--session data)))
        (with-current-buffer data
          (if id
              (mevedel-note-conversation-setup)
            (mevedel-preset-apply 'mevedel-note data)
            (mevedel-session-artifacts-save session data nil t)
            (mevedel--submit-generated-turn prompt)))
        (unless id
          (dolist (item notes)
            (overlay-put item 'mevedel-note-session-id (mevedel-session-session-id session))
            (overlay-put item 'mevedel-note-state 'sent)
            (mevedel-notes--render item))
          (mevedel-instruction-store-touch workspace)
          (mevedel-instruction-store-flush t))
        (let ((view (buffer-local-value 'mevedel--view-buffer data)))
          (with-current-buffer view
            (when mevedel-note-conversation--origin
              (set-marker (cdr mevedel-note-conversation--origin) nil))
            (setq-local mevedel-note-conversation--origin origin))
          (mevedel-directive-frame-display note view t)
          (with-current-buffer view (goto-char (point-max)))
          view)))))

;;;###autoload
(defun mevedel-note-send (&optional note)
  "Send NOTE in its own session, or reopen its existing conversation.
Default to the note at point.  Return the focused conversation view."
  (interactive)
  (mevedel-note-conversation--send-notes (list (or note (mevedel-notes--at-point)))))

;;;###autoload
(defun mevedel-note-send-batch (notes)
  "Send NOTES together in one conversation and return its focused view.
An already sent batch reopens its session.  Mixing existing conversations
refuses rather than losing either thread.  Interactively, choose loaded notes."
  (interactive
   (progn
     (mevedel--instruction-activate-buffer)
     (let ((choices (cl-loop for note in (mevedel--all-instructions)
                             when (eq (overlay-get note 'mevedel-instruction-type) 'note)
                             collect (cons (format "%s: %s" (overlay-get note 'mevedel-id)
                                                   (mevedel-notes--display-text note)) note))))
       (list (mapcar (lambda (choice) (cdr (assoc choice choices)))
                     (completing-read-multiple "Send notes: " choices nil t))))))
  (mevedel-note-conversation--send-notes (delete-dups (copy-sequence notes))))

(provide 'mevedel-note-conversation)
;;; mevedel-note-conversation.el ends here
