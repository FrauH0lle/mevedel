;;; mevedel-view-segments.el -- Historical session segment inspection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the ephemeral buffers, cursor state, read-only composer presentation,
;; and navigation commands used to inspect archived session segments.  Durable
;; segment bytes and descriptors remain owned by `mevedel-session-artifacts'.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'mevedel-structs)

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-collect-prompts
                  "mevedel-session-artifacts" (buffer &optional preview-length))
(declare-function mevedel-session-artifacts-read-segment
                  "mevedel-session-artifacts" (session number))
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-segments
                  "mevedel-session-artifacts" (session live-buffer))
(declare-function mevedel-session-artifacts-session-chats
                  "mevedel-session-artifacts" (session))
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'mevedel-session-artifacts-collect-prompts "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-transcript-segment
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-segments "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-session-chats "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-transcript-segments
  "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--data-buffer)
(defvar mevedel--view-buffer)

;; `mevedel-view'
(declare-function mevedel-view--jump-to-pinned-prompt
                  "mevedel-view" (position &optional event))
(defvar mevedel-view--side-conversation-p)

;; `mevedel-view-composer'
(declare-function mevedel-view--input-marker-position
                  "mevedel-view-composer" ())
(declare-function mevedel-view-composer-session-fork-armed-p
                  "mevedel-view-composer" ())
(declare-function mevedel-view-composer-set-historical-visible
                  "mevedel-view-composer" (visible))

;; `mevedel-view-render'
(declare-function mevedel-view-render-capture-segment-state
                  "mevedel-view-render" ())
(declare-function mevedel-view-render-mutate
                  "mevedel-view-render" (key function &optional replacement cleanup))
(autoload 'mevedel-view-render-mutate "mevedel-view-render")
(declare-function mevedel-view-render-project-segment
                  "mevedel-view-render" (data-buffer state direction))
(declare-function mevedel-view--expand-turn "mevedel-view-render" ())
(declare-function mevedel-view-render-toggle-user-input
                  "mevedel-view-render" ())
(autoload 'mevedel-view-render-capture-segment-state "mevedel-view-render")
(autoload 'mevedel-view-render-project-segment "mevedel-view-render")
(defvar mevedel-view-render--batch)


;;
;;; State

(defvar-local mevedel-view-segments--number nil
  "Archived segment number currently projected in this view.")

(defvar-local mevedel-view-segments--buffer nil
  "Read-only transcript buffer currently projected in this view.")

(defvar-local mevedel-view-segments--states nil
  "Cursor and disclosure state keyed by archived segment number.")

(defvar-local mevedel-view-segments--live-state nil
  "Cursor and window state captured before historical inspection.")

(defun mevedel-view-historical-segment-p ()
  "Return non-nil when this view projects an archived session segment."
  (and mevedel-view-segments--number
       (buffer-live-p mevedel-view-segments--buffer)))

(defun mevedel-view-segments-current-number ()
  "Return the archived segment projected by this view, or nil."
  (and (mevedel-view-historical-segment-p)
       mevedel-view-segments--number))

(defun mevedel-view-segments-display-buffer ()
  "Return the transcript buffer currently projected by this view."
  (if (mevedel-view-historical-segment-p)
      mevedel-view-segments--buffer
    mevedel--data-buffer))

(defun mevedel-view-segments--session ()
  "Return the live session associated with this view."
  (and (buffer-live-p mevedel--data-buffer)
       (buffer-local-value 'mevedel--session mevedel--data-buffer)))

(defun mevedel-view-segments--entries (session)
  "Return this view's segment descriptors, with a numbered current entry.
Agent views inspect their own numbered compaction archives, never the parent
session's root segments."
  (if mevedel-view--agent-transcript-p
      (let* ((entries (mevedel-session-artifacts-transcript-segments
                       session mevedel--data-buffer t))
             (latest (1+ (or (cl-loop for entry in entries
                                     maximize (or (plist-get entry :number) 0))
                             0))))
        (mapcar (lambda (entry)
                  (if (plist-get entry :current-p)
                      (plist-put (copy-sequence entry) :number latest)
                    entry))
                entries))
    (mevedel-session-artifacts-segments session mevedel--data-buffer)))

(defun mevedel-view-segments--cleanup ()
  "Kill the archived segment buffer owned by the current view."
  (when (buffer-live-p mevedel-view-segments--buffer)
    (kill-buffer mevedel-view-segments--buffer))
  (setq mevedel-view-segments--buffer nil
        mevedel-view-segments--number nil))

(defun mevedel-view-segments-initialize ()
  "Initialize historical segment inspection in the current view."
  (setq-local mevedel-view-segments--states (make-hash-table :test #'eql))
  (add-hook 'kill-buffer-hook #'mevedel-view-segments--cleanup nil t))


;;
;;; Presentation

(defvar-keymap mevedel-view-segments--latest-map
  :doc "Keymap for the historical segment Latest action."
  "RET" #'mevedel-view-return-to-latest-segment
  "<mouse-1>" #'mevedel-view-return-to-latest-segment)

(defun mevedel-view-segments-banner ()
  "Return the archived segment banner for the current view."
  (let* ((session (buffer-local-value 'mevedel--session
                                      mevedel--data-buffer))
         (current (plist-get (car (last (mevedel-view-segments--entries session)))
                             :number))
         (latest
          (propertize
           "[Latest]"
           'keymap mevedel-view-segments--latest-map
           'mouse-face 'highlight
           'help-echo "Return to the live session segment")))
    (propertize
     (format "Viewing archived %ssegment %d of %d - read-only %s\n\n"
             (if mevedel-view--agent-transcript-p "agent " "")
             mevedel-view-segments--number current latest)
     'read-only t
     'font-lock-face 'shadow
     'mevedel-view-historical-banner t)))


;;
;;; Navigation

(defun mevedel-view-segments--show (number direction)
  "Project session segment NUMBER, landing according to DIRECTION."
  (mevedel-view-render-mutate
   'source
   (lambda () (mevedel-view-segments--show-now number direction)) t))

(defun mevedel-view-segments--prompt-entry (buffer source-pos)
  "Verify that SOURCE-POS is a canonical user prompt position in BUFFER."
  (unless (and (integerp source-pos)
               (cl-find source-pos
                        (mevedel-session-artifacts-collect-prompts buffer)
                        :key (lambda (entry) (plist-get entry :pos))))
    (user-error "No user prompt at source position %s" source-pos)))

(defun mevedel-view-segments--show-now (number direction &optional source-pos)
  "Switch to NUMBER in DIRECTION with projection ownership held.
When SOURCE-POS is non-nil, verify its prompt before replacing the view."
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (entries (mevedel-view-segments--entries session))
         (current (plist-get (car (last entries)) :number))
         (entry (cl-find number entries
                         :key (lambda (item) (plist-get item :number)))))
    (when (and mevedel-view--agent-transcript-p
               (not (and entry (eq (plist-get entry :status) 'readable))))
      (user-error "Unreadable transcript segment: %s" number))
    (if (= number current)
        (mevedel-view-segments--return-now)
      (let* ((new-buffer
              (if mevedel-view--agent-transcript-p
                  (mevedel-session-artifacts-read-transcript-segment
                   session entry)
                (mevedel-session-artifacts-read-segment session number)))
             (old-number mevedel-view-segments--number)
             (old-buffer mevedel-view-segments--buffer)
             (old-state (mevedel-view-render-capture-segment-state))
             (target-state (gethash number mevedel-view-segments--states))
             (view-buffer (current-buffer)))
        (when source-pos
          (condition-case err
              (mevedel-view-segments--prompt-entry new-buffer source-pos)
            (error
             (kill-buffer new-buffer)
             (signal (car err) (cdr err)))))
        (with-current-buffer new-buffer
          (setq-local mevedel--view-buffer view-buffer))
        (if old-number
            (puthash old-number old-state mevedel-view-segments--states)
          (setq mevedel-view-segments--live-state old-state))
        (setq mevedel-view-segments--number number
              mevedel-view-segments--buffer new-buffer)
        (condition-case err
            (progn
              (mevedel-view-composer-set-historical-visible t)
              (mevedel-view-render-project-segment
               new-buffer target-state direction)
              (unless (mevedel-view-composer-session-fork-armed-p)
                (mevedel-view-composer-set-historical-visible nil))
              (when (and (buffer-live-p old-buffer)
                         (not (eq old-buffer new-buffer)))
                (kill-buffer old-buffer)))
          (error
           (setq mevedel-view-segments--number old-number
                 mevedel-view-segments--buffer old-buffer)
           (unwind-protect
               (progn
                 (when (buffer-live-p new-buffer)
                   (kill-buffer new-buffer))
                 (mevedel-view-composer-set-historical-visible t)
                 (mevedel-view-render-project-segment
                  (and old-number old-buffer) old-state nil)
                 (when (and old-number
                            (not (mevedel-view-composer-session-fork-armed-p)))
                   (mevedel-view-composer-set-historical-visible nil)))
             (when mevedel-view--agent-transcript-p
               (setq buffer-read-only t)))
           (signal (car err) (cdr err))))))))

(defun mevedel-view-segments--prompt-header (buffer source-pos)
  "Find the rendered header for BUFFER's prompt at SOURCE-POS."
  (let ((pos (point-min))
        (limit (mevedel-view--input-marker-position))
        found)
    (while (and (< pos limit) (not found))
      (let* ((context (get-text-property pos 'mevedel-view-turn-context))
             (turn (plist-get context :turn)))
        (when (and (eq (plist-get context :data) buffer)
                   (eq (plist-get turn :role) 'user)
                   (or (and (<= (plist-get turn :start) source-pos)
                            (< source-pos (plist-get turn :end)))
                       ;; A directive's indexed position is its boundary,
                       ;; before the user body and thus before the turn start.
                       (eql source-pos
                            (plist-get (plist-get turn :directive) :start)))
                   (memq (get-text-property pos 'mevedel-view-type)
                         '(turn-header turn-summary)))
          (setq found pos)))
      (unless found
        (setq pos (or (next-single-property-change
                       pos 'mevedel-view-type nil limit)
                      limit))))
    found))

(defun mevedel-view-segments--reveal-prompt (position window)
  "Reveal prompt header at POSITION, focusing WINDOW when available."
  (if (and (window-live-p window)
           (eq (window-buffer window) (current-buffer)))
      (progn
        (select-window window)
        (mevedel-view--jump-to-pinned-prompt position))
    (goto-char position)
    (when (get-text-property (point) 'mevedel-view-stash)
      (mevedel-view--expand-turn)
      (goto-char position))
    (forward-line 1)
    (when (and (eq (get-text-property (point) 'mevedel-view-type)
                   'user-input-summary)
               (get-text-property (point) 'mevedel-view-collapsed))
      (mevedel-view-render-toggle-user-input))
    (goto-char position)))

(defun mevedel-view-segments-jump-to-prompt (segment source-pos &optional window)
  "Open archived SEGMENT and reveal the user prompt at SOURCE-POS.
Called in the owning view buffer.  WINDOW is the clicked view window, if any.
An invalid prompt or unreadable archive leaves the current projection intact."
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (latest (or (mevedel-session-current-segment session) 1)))
    (unless (and (integerp segment) (<= 1 segment) (< segment latest))
      (user-error "Unknown archived session segment: %s" segment))
    (mevedel-view-render-mutate
     'source
     (lambda ()
       (if (eql segment (mevedel-view-segments-current-number))
           (progn
             (mevedel-view-segments--prompt-entry
              mevedel-view-segments--buffer source-pos)
             ;; A scheduled batch can still show the old source or a placeholder.
             ;; Project synchronously before resolving any rendered position.
             (when mevedel-view-render--batch
               (mevedel-view-render-project-segment
                mevedel-view-segments--buffer
                (mevedel-view-render-capture-segment-state) nil)))
         (mevedel-view-segments--show-now segment 'backward source-pos))
       (when (eql segment (mevedel-view-segments-current-number))
         (let ((position (mevedel-view-segments--prompt-header
                          mevedel-view-segments--buffer source-pos)))
           (unless position
             (user-error "Archived prompt is not rendered at %s" source-pos))
           (mevedel-view-segments--reveal-prompt
            position (or window (get-buffer-window (current-buffer) t))))))
     t)))

(defun mevedel-view-previous-segment ()
  "Show the previous session segment without changing session state."
  (interactive)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (entries (mevedel-view-segments--entries session))
         (current (or mevedel-view-segments--number
                      (plist-get (car (last entries)) :number)))
         (target (if mevedel-view--agent-transcript-p
                     (cl-loop for entry in entries
                              for number = (plist-get entry :number)
                              when (< number current) maximize number)
                   (1- current))))
    (if (or (null target) (< target 1))
        (message "mevedel: oldest segment")
      (mevedel-view-segments--show target 'backward))))

(defun mevedel-view-next-segment ()
  "Show the next session segment without changing session state."
  (interactive)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (entries (mevedel-view-segments--entries session))
         (latest (plist-get (car (last entries)) :number))
         (current (or mevedel-view-segments--number latest))
         (target (if mevedel-view--agent-transcript-p
                     (cl-loop for entry in entries
                              for number = (plist-get entry :number)
                              when (> number current) minimize number)
                   (1+ current))))
    (if (or (null target) (> target latest))
        (message "mevedel: latest segment")
      (mevedel-view-segments--show target 'forward))))

(defun mevedel-view-segments--candidate (segment displayed &optional chats-p)
  "Return completion text for SEGMENT, marking DISPLAYED.
CHATS-P names each segment's chat, for a session `/clear' divided."
  (let ((number (plist-get segment :number))
        (status (plist-get segment :status))
        (preview (or (plist-get segment :preview) "(no user message)"))
        (chat (plist-get segment :chat)))
    (format "%s S%d  %-10s  %s%s"
            (if (= number displayed) "●" " ")
            number
            (if (plist-get segment :current-p)
                "current"
              (symbol-name status))
            (if (and chats-p chat)
                (format "Chat %d · %s  "
                        (plist-get chat :number)
                        (truncate-string-to-width
                         (or (plist-get chat :title) "") 32 nil nil "…"))
              "")
            preview)))

(defun mevedel-view-go-to-segment (&optional number)
  "Choose and display this transcript's segment NUMBER.

Interactively, offer every canonical segment, including missing and
unreadable entries in root transcripts.  Agent views offer their own
readable compaction archives instead of the parent session's segments."
  (interactive)
  (let* ((session (or (mevedel-view-segments--session)
                      (user-error "Active view has no mevedel session")))
         (segments (mevedel-view-segments--entries session))
         (latest (plist-get (car (last segments)) :number))
         (displayed (or mevedel-view-segments--number latest))
         (chats-p (cl-some (lambda (segment)
                             (> (or (plist-get (plist-get segment :chat) :number) 1)
                                1))
                           segments))
         (choices
          (mapcar
           (lambda (segment)
             (cons (mevedel-view-segments--candidate segment displayed chats-p)
                   (plist-get segment :number)))
           segments))
         (target
          (or number
              (cdr
               (assoc
                (completing-read "Go to segment: " choices nil t)
                choices)))))
    (unless (and (integerp target) (<= 1 target) (<= target latest)
                 (or (not mevedel-view--agent-transcript-p)
                     (cl-find target segments
                              :key (lambda (entry) (plist-get entry :number)))))
      (user-error "Unknown session segment: %s" target))
    (unless (= target displayed)
      (mevedel-view-segments--show
       target (if (< target displayed) 'backward 'forward)))))

(defun mevedel-view-segments--chats ()
  "Return the live session's chats.
Agent transcripts and side conversations have none of their own."
  (when-let* (((not mevedel-view--agent-transcript-p))
              ((not mevedel-view--side-conversation-p))
              (session (mevedel-view-segments--session)))
    (mevedel-session-artifacts-session-chats session)))

(defun mevedel-view-go-to-chat (&optional number)
  "Show chat NUMBER of this session, choosing it interactively.
A chat is the run of segments between two `/clear's.  An earlier chat
opens at its last segment, read-only like any archived segment; the
current chat returns to the live segment."
  (interactive)
  (let* ((chats (or (mevedel-view-segments--chats)
                    (user-error "Active view has no mevedel session")))
         (choices
          (mapcar
           (lambda (chat)
             (cons (format "Chat %d  %-40s  %s"
                           (plist-get chat :number)
                           (truncate-string-to-width
                            (or (plist-get chat :title) "") 40 nil nil "…")
                           (if (plist-get chat :current-p)
                               "current"
                             (format "S%d–S%d"
                                     (plist-get chat :first-segment)
                                     (plist-get chat :last-segment))))
                   chat))
           chats))
         (chat
          (if number
              (or (cl-find number chats
                           :key (lambda (chat) (plist-get chat :number)))
                  (user-error "Unknown chat: %s" number))
            (if (cdr chats)
                (cdr (assoc (completing-read "Go to chat: " choices nil t)
                            choices))
              (user-error "This session has one chat; /clear starts another")))))
    (mevedel-view-go-to-segment (plist-get chat :last-segment))))

(defvar-keymap mevedel-view-segments--chats-map
  :doc "Keymap for the earlier-chats line of a view."
  "RET" #'mevedel-view-go-to-chat
  "<mouse-1>" #'mevedel-view-go-to-chat)

(defun mevedel-view-segments-chats-line ()
  "Return the line naming this session's earlier chats, or nil.
The newest earlier chat comes first; the line opens the chat picker."
  (when-let* ((chats (mevedel-view-segments--chats))
              (earlier (reverse (seq-remove (lambda (chat)
                                              (plist-get chat :current-p))
                                            chats))))
    (propertize
     (format "◂ %d earlier chat%s in this session: %s\n\n"
             (length earlier) (if (cdr earlier) "s" "")
             (truncate-string-to-width
              (mapconcat (lambda (chat) (plist-get chat :title)) earlier " · ")
              100 nil nil "…"))
     'read-only t
     ;; History starts right after this line: point there must keep the
     ;; history's own keymap and stay editable for the renderer.
     'rear-nonsticky t
     'font-lock-face 'shadow
     'keymap mevedel-view-segments--chats-map
     'mouse-face 'highlight
     'help-echo "Show an earlier chat of this session"
     'mevedel-view-chats-line t)))

(defun mevedel-view-return-to-latest-segment (&optional _event)
  "Return from archived inspection to the live session segment."
  (interactive)
  (mevedel-view-render-mutate 'source #'mevedel-view-segments--return-now t))

(defun mevedel-view-segments--return-now ()
  "Return to the live source while holding projection ownership."
  (when (mevedel-view-historical-segment-p)
    (let ((old-number mevedel-view-segments--number)
          (old-buffer mevedel-view-segments--buffer)
          (old-state (mevedel-view-render-capture-segment-state))
          (live-state mevedel-view-segments--live-state))
      (puthash old-number old-state mevedel-view-segments--states)
      (setq mevedel-view-segments--number nil
            mevedel-view-segments--buffer nil)
      (condition-case err
          (progn
            (mevedel-view-composer-set-historical-visible t)
            (mevedel-view-render-project-segment
             mevedel--data-buffer live-state 'forward)
            ;; Agent inspection has no editable live composer.  The root-view
            ;; composer unlock above is only needed while replacing its source.
            (when mevedel-view--agent-transcript-p
              (setq buffer-read-only t))
            (when (buffer-live-p old-buffer)
              (kill-buffer old-buffer))
            (setq mevedel-view-segments--live-state nil))
        (error
         (setq mevedel-view-segments--number old-number
               mevedel-view-segments--buffer old-buffer)
         (unwind-protect
             (progn
               (mevedel-view-render-project-segment old-buffer old-state nil)
               (mevedel-view-composer-set-historical-visible nil))
           (when mevedel-view--agent-transcript-p
             (setq buffer-read-only t)))
         (signal (car err) (cdr err)))))))

(provide 'mevedel-view-segments)
;;; mevedel-view-segments.el ends here
