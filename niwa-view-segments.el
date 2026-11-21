;;; niwa-view-segments.el -- Historical session segment inspection -*- lexical-binding: t -*-

;;; Commentary:

;; Owns the ephemeral buffers, cursor state, read-only composer presentation,
;; and navigation commands used to inspect archived session segments.  Durable
;; segment bytes and descriptors remain owned by `niwa-session-artifacts'.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'niwa-structs)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-collect-prompts
                  "niwa-session-artifacts" (buffer &optional preview-length))
(declare-function niwa-session-artifacts-read-segment
                  "niwa-session-artifacts" (session number))
(declare-function niwa-session-artifacts-read-transcript-segment
                  "niwa-session-artifacts" (session descriptor))
(declare-function niwa-session-artifacts-segments
                  "niwa-session-artifacts" (session live-buffer))
(declare-function niwa-session-artifacts-session-chats
                  "niwa-session-artifacts" (session))
(declare-function niwa-session-artifacts-transcript-segments
                  "niwa-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'niwa-session-artifacts-collect-prompts "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-transcript-segment
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segments "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-session-chats "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-transcript-segments
  "niwa-session-artifacts")

;; `niwa-structs'
(declare-function niwa-session-current-segment
                  "niwa-structs" (cl-x) t)
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format-string &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

;; `niwa-view'
(declare-function niwa-view--jump-to-pinned-prompt
                  "niwa-view" (position &optional event))
(defvar niwa-view--side-conversation-p)

;; `niwa-view-composer'
(declare-function niwa-view-composer-session-fork-armed-p
                  "niwa-view-composer" ())
(declare-function niwa-view-composer-set-historical-visible
                  "niwa-view-composer" (visible))

;; `niwa-view-render'
(declare-function niwa-view-render-capture-segment-state
                  "niwa-view-render" ())
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(autoload 'niwa-view-render-mutate "niwa-view-render")
(declare-function niwa-view-render-project-segment
                  "niwa-view-render" (data-buffer state direction))
(declare-function niwa-view--expand-turn "niwa-view-render" ())
(declare-function niwa-view-render-toggle-user-input
                  "niwa-view-render" ())
(autoload 'niwa-view-render-capture-segment-state "niwa-view-render")
(autoload 'niwa-view-render-project-segment "niwa-view-render")
(defvar niwa-view-render--batch)

;; `niwa-view-viewport'
(declare-function niwa-view-transcript-tail "niwa-view-viewport" ())


;;
;;; State

(defvar-local niwa-view-segments--number nil
  "Archived segment number currently projected in this view.")

(defvar-local niwa-view-segments--buffer nil
  "Read-only transcript buffer currently projected in this view.")

(defvar-local niwa-view-segments--states nil
  "Cursor and disclosure state keyed by archived segment number.")

(defvar-local niwa-view-segments--live-state nil
  "Cursor and window state captured before historical inspection.")

(defun niwa-view-historical-segment-p ()
  "Return non-nil when this view projects an archived session segment."
  (and niwa-view-segments--number
       (buffer-live-p niwa-view-segments--buffer)))

(defun niwa-view-segments-current-number ()
  "Return the archived segment projected by this view, or nil."
  (and (niwa-view-historical-segment-p)
       niwa-view-segments--number))

(defun niwa-view-segments-display-buffer ()
  "Return the transcript buffer currently projected by this view."
  (if (niwa-view-historical-segment-p)
      niwa-view-segments--buffer
    niwa--data-buffer))

(defun niwa-view-segments--session ()
  "Return the live session associated with this view."
  (and (buffer-live-p niwa--data-buffer)
       (buffer-local-value 'niwa--session niwa--data-buffer)))

(defun niwa-view-segments--entries (session)
  "Return this view's segment descriptors, with a numbered current entry.
Agent views inspect their own numbered compaction archives, never the parent
session's root segments."
  (if niwa-view--agent-transcript-p
      (let* ((entries (niwa-session-artifacts-transcript-segments
                       session niwa--data-buffer t))
             (latest (1+ (or (cl-loop for entry in entries
                                     maximize (or (plist-get entry :number) 0))
                             0))))
        (mapcar (lambda (entry)
                  (if (plist-get entry :current-p)
                      (plist-put (copy-sequence entry) :number latest)
                    entry))
                entries))
    (niwa-session-artifacts-segments session niwa--data-buffer)))

(defun niwa-view-segments--cleanup ()
  "Kill the archived segment buffer owned by the current view."
  (when (buffer-live-p niwa-view-segments--buffer)
    (kill-buffer niwa-view-segments--buffer))
  (setq niwa-view-segments--buffer nil
        niwa-view-segments--number nil))

(defun niwa-view-segments-initialize ()
  "Initialize historical segment inspection in the current view."
  (setq-local niwa-view-segments--states (make-hash-table :test #'eql))
  (add-hook 'kill-buffer-hook #'niwa-view-segments--cleanup nil t))


;;
;;; Presentation

(defvar-keymap niwa-view-segments--latest-map
  :doc "Keymap for the historical segment Latest action."
  "RET" #'niwa-view-return-to-latest-segment
  "<mouse-1>" #'niwa-view-return-to-latest-segment)

(defun niwa-view-segments-banner ()
  "Return the archived segment banner for the current view."
  (let* ((session (buffer-local-value 'niwa--session
                                      niwa--data-buffer))
         (current (plist-get (car (last (niwa-view-segments--entries session)))
                             :number))
         (latest
          (propertize
           "[Latest]"
           'keymap niwa-view-segments--latest-map
           'mouse-face 'highlight
           'help-echo "Return to the live session segment")))
    (propertize
     (format "Viewing archived %ssegment %d of %d - read-only %s\n\n"
             (if niwa-view--agent-transcript-p "agent " "")
             niwa-view-segments--number current latest)
     'read-only t
     'font-lock-face 'shadow
     'niwa-view-historical-banner t)))


;;
;;; Navigation

(defun niwa-view-segments--show (number direction)
  "Project session segment NUMBER, landing according to DIRECTION."
  (niwa-view-render-mutate
   'source
   (lambda () (niwa-view-segments--show-now number direction)) t))

(defun niwa-view-segments--prompt-entry (buffer source-pos)
  "Verify that SOURCE-POS is a canonical user prompt position in BUFFER."
  (unless (and (integerp source-pos)
               (cl-find source-pos
                        (niwa-session-artifacts-collect-prompts buffer)
                        :key (lambda (entry) (plist-get entry :pos))))
    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No user prompt at source position %s" source-pos)))

(defun niwa-view-segments--show-now (number direction &optional source-pos)
  "Switch to NUMBER in DIRECTION with projection ownership held.
When SOURCE-POS is non-nil, verify its prompt before replacing the view."
  (let* ((session (or (niwa-view-segments--session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
         (entries (niwa-view-segments--entries session))
         (current (plist-get (car (last entries)) :number))
         (entry (cl-find number entries
                         :key (lambda (item) (plist-get item :number)))))
    (when (and niwa-view--agent-transcript-p
               (not (and entry (eq (plist-get entry :status) 'readable))))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unreadable transcript segment: %s" number))
    (if (= number current)
        (niwa-view-segments--return-now)
      (let* ((new-buffer
              (if niwa-view--agent-transcript-p
                  (niwa-session-artifacts-read-transcript-segment
                   session entry)
                (niwa-session-artifacts-read-segment session number)))
             (old-number niwa-view-segments--number)
             (old-buffer niwa-view-segments--buffer)
             (old-state (niwa-view-render-capture-segment-state))
             (target-state (gethash number niwa-view-segments--states))
             (view-buffer (current-buffer)))
        (when source-pos
          (condition-case err
              (niwa-view-segments--prompt-entry new-buffer source-pos)
            (error
             (kill-buffer new-buffer)
             (signal (car err) (cdr err)))))
        (with-current-buffer new-buffer
          (setq-local niwa--view-buffer view-buffer))
        (if old-number
            (puthash old-number old-state niwa-view-segments--states)
          (setq niwa-view-segments--live-state old-state))
        (setq niwa-view-segments--number number
              niwa-view-segments--buffer new-buffer)
        (condition-case err
            (progn
              (niwa-view-composer-set-historical-visible t)
              (niwa-view-render-project-segment
               new-buffer target-state direction)
              (unless (niwa-view-composer-session-fork-armed-p)
                (niwa-view-composer-set-historical-visible nil))
              (when (and (buffer-live-p old-buffer)
                         (not (eq old-buffer new-buffer)))
                (kill-buffer old-buffer)))
          (error
           (setq niwa-view-segments--number old-number
                 niwa-view-segments--buffer old-buffer)
           (unwind-protect
               (progn
                 (when (buffer-live-p new-buffer)
                   (kill-buffer new-buffer))
                 (niwa-view-composer-set-historical-visible t)
                 (niwa-view-render-project-segment
                  (and old-number old-buffer) old-state nil)
                 (when (and old-number
                            (not (niwa-view-composer-session-fork-armed-p)))
                   (niwa-view-composer-set-historical-visible nil)))
             (when niwa-view--agent-transcript-p
               (setq buffer-read-only t)))
           (signal (car err) (cdr err))))))))

(defun niwa-view-segments--prompt-header (buffer source-pos)
  "Find the rendered header for BUFFER's prompt at SOURCE-POS."
  (let ((pos (point-min))
        (limit (niwa-view-transcript-tail))
        found)
    (while (and (< pos limit) (not found))
      (let* ((context (get-text-property pos 'niwa-view-turn-context))
             (turn (plist-get context :turn)))
        (when (and (eq (plist-get context :data) buffer)
                   (eq (plist-get turn :role) 'user)
                   (<= (plist-get turn :start) source-pos)
                   (< source-pos (plist-get turn :end))
                   (memq (get-text-property pos 'niwa-view-type)
                         '(turn-header turn-summary)))
          (setq found pos)))
      (unless found
        (setq pos (or (next-single-property-change
                       pos 'niwa-view-type nil limit)
                      limit))))
    found))

(defun niwa-view-segments--reveal-prompt (position window)
  "Reveal prompt header at POSITION, focusing WINDOW when available."
  (if (and (window-live-p window)
           (eq (window-buffer window) (current-buffer)))
      (progn
        (select-window window)
        (niwa-view--jump-to-pinned-prompt position))
    (goto-char position)
    (when (get-text-property (point) 'niwa-view-stash)
      (niwa-view--expand-turn)
      (goto-char position))
    (forward-line 1)
    (when (and (eq (get-text-property (point) 'niwa-view-type)
                   'user-input-summary)
               (get-text-property (point) 'niwa-view-collapsed))
      (niwa-view-render-toggle-user-input))
    (goto-char position)))

(defun niwa-view-segments-jump-to-prompt (segment source-pos &optional window)
  "Open archived SEGMENT and reveal the user prompt at SOURCE-POS.
Called in the owning view buffer.  WINDOW is the clicked view window, if any.
An invalid prompt or unreadable archive leaves the current projection intact."
  (let* ((session (or (niwa-view-segments--session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
         (latest (or (niwa-session-current-segment session) 1)))
    (unless (and (integerp segment) (<= 1 segment) (< segment latest))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unknown archived session segment: %s" segment))
    (niwa-view-render-mutate
     'source
     (lambda ()
       (if (eql segment (niwa-view-segments-current-number))
           (progn
             (niwa-view-segments--prompt-entry
              niwa-view-segments--buffer source-pos)
             ;; A scheduled batch can still show the old source or a placeholder.
             ;; Project synchronously before resolving any rendered position.
             (when niwa-view-render--batch
               (niwa-view-render-project-segment
                niwa-view-segments--buffer
                (niwa-view-render-capture-segment-state) nil)))
         (niwa-view-segments--show-now segment 'backward source-pos))
       (when (eql segment (niwa-view-segments-current-number))
         (let ((position (niwa-view-segments--prompt-header
                          niwa-view-segments--buffer source-pos)))
           (unless position
             (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Archived prompt is not rendered at %s" source-pos))
           (niwa-view-segments--reveal-prompt
            position (or window (get-buffer-window (current-buffer) t))))))
     t)))

(defun niwa-view-previous-segment ()
  "Show the previous session segment without changing session state."
  (interactive)
  (let* ((session (or (niwa-view-segments--session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
         (entries (niwa-view-segments--entries session))
         (current (or niwa-view-segments--number
                      (plist-get (car (last entries)) :number)))
         (target (if niwa-view--agent-transcript-p
                     (cl-loop for entry in entries
                              for number = (plist-get entry :number)
                              when (< number current) maximize number)
                   (1- current))))
    (if (or (null target) (< target 1))
        (message "niwa: oldest segment")
      (niwa-view-segments--show target 'backward))))

(defun niwa-view-next-segment ()
  "Show the next session segment without changing session state."
  (interactive)
  (let* ((session (or (niwa-view-segments--session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
         (entries (niwa-view-segments--entries session))
         (latest (plist-get (car (last entries)) :number))
         (current (or niwa-view-segments--number latest))
         (target (if niwa-view--agent-transcript-p
                     (cl-loop for entry in entries
                              for number = (plist-get entry :number)
                              when (> number current) minimize number)
                   (1+ current))))
    (if (or (null target) (> target latest))
        (message "niwa: latest segment")
      (niwa-view-segments--show target 'forward))))

(defun niwa-view-segments--candidate (segment displayed &optional chats-p)
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

(defun niwa-view-go-to-segment (&optional number)
  "Choose and display this transcript's segment NUMBER.

Interactively, offer every canonical segment, including missing and
unreadable entries in root transcripts.  Agent views offer their own
readable compaction archives instead of the parent session's segments."
  (interactive)
  (let* ((session (or (niwa-view-segments--session)
                      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
         (segments (niwa-view-segments--entries session))
         (latest (plist-get (car (last segments)) :number))
         (displayed (or niwa-view-segments--number latest))
         (chats-p (cl-some (lambda (segment)
                             (> (or (plist-get (plist-get segment :chat) :number) 1)
                                1))
                           segments))
         (choices
          (mapcar
           (lambda (segment)
             (cons (niwa-view-segments--candidate segment displayed chats-p)
                   (plist-get segment :number)))
           segments))
         (target
          (or number
              (cdr
               (assoc
                (completing-read "Go to segment: " choices nil t)
                choices)))))
    (unless (and (integerp target) (<= 1 target) (<= target latest)
                 (or (not niwa-view--agent-transcript-p)
                     (cl-find target segments
                              :key (lambda (entry) (plist-get entry :number)))))
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unknown session segment: %s" target))
    (unless (= target displayed)
      (niwa-view-segments--show
       target (if (< target displayed) 'backward 'forward)))))

(defun niwa-view-segments--chats ()
  "Return the live session's chats.
Agent transcripts and side conversations have none of their own."
  (when-let* (((not niwa-view--agent-transcript-p))
              ((not niwa-view--side-conversation-p))
              (session (niwa-view-segments--session)))
    (niwa-session-artifacts-session-chats session)))

(defun niwa-view-go-to-chat (&optional number)
  "Show chat NUMBER of this session, choosing it interactively.
A chat is the run of segments between two `/clear's.  An earlier chat
opens at its last segment, read-only like any archived segment; the
current chat returns to the live segment."
  (interactive)
  (let* ((chats (or (niwa-view-segments--chats)
                    (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Active view has no niwa session")))
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
                  (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Unknown chat: %s" number))
            (if (cdr chats)
                (cdr (assoc (completing-read "Go to chat: " choices nil t)
                            choices))
              (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "This session has one chat; /clear starts another")))))
    (niwa-view-go-to-segment (plist-get chat :last-segment))))

(defvar-keymap niwa-view-segments--chats-map
  :doc "Keymap for the earlier-chats line of a view."
  "RET" #'niwa-view-go-to-chat
  "<mouse-1>" #'niwa-view-go-to-chat)

(defun niwa-view-segments-chats-line ()
  "Return the line naming this session's earlier chats, or nil.
The newest earlier chat comes first; the line opens the chat picker."
  (when-let* ((chats (niwa-view-segments--chats))
              (earlier (reverse (seq-remove (lambda (chat)
                                              (plist-get chat :current-p))
                                            chats))))
    (propertize
     (concat
      (propertize
       (format "◂ %d earlier chat%s in this session: %s"
               (length earlier) (if (cdr earlier) "s" "")
               (truncate-string-to-width
                (mapconcat (lambda (chat) (plist-get chat :title)) earlier " · ")
                100 nil nil "…"))
       'keymap niwa-view-segments--chats-map
       'mouse-face 'highlight
       'help-echo "Show an earlier chat of this session")
      "\n\n")
     'read-only t
     'front-sticky '(read-only)
     ;; History starts right after this line: point there must keep the
     ;; history's own keymap and stay editable for the renderer.
     'rear-nonsticky t
     'font-lock-face 'shadow
     'niwa-view-chats-line t)))

(defun niwa-view-return-to-latest-segment (&optional _event)
  "Return from archived inspection to the live session segment."
  (interactive)
  (niwa-view-render-mutate 'source #'niwa-view-segments--return-now t))

(defun niwa-view-segments--return-now ()
  "Return to the live source while holding projection ownership."
  (when (niwa-view-historical-segment-p)
    (let ((old-number niwa-view-segments--number)
          (old-buffer niwa-view-segments--buffer)
          (old-state (niwa-view-render-capture-segment-state))
          (live-state niwa-view-segments--live-state))
      (puthash old-number old-state niwa-view-segments--states)
      (setq niwa-view-segments--number nil
            niwa-view-segments--buffer nil)
      (condition-case err
          (progn
            (niwa-view-composer-set-historical-visible t)
            (niwa-view-render-project-segment
             niwa--data-buffer live-state 'forward)
            ;; Agent inspection has no editable live composer.  The root-view
            ;; composer unlock above is only needed while replacing its source.
            (when niwa-view--agent-transcript-p
              (setq buffer-read-only t))
            (when (buffer-live-p old-buffer)
              (kill-buffer old-buffer))
            (setq niwa-view-segments--live-state nil))
        (error
         (setq niwa-view-segments--number old-number
               niwa-view-segments--buffer old-buffer)
         (unwind-protect
             (progn
               (niwa-view-render-project-segment old-buffer old-state nil)
               (niwa-view-composer-set-historical-visible nil))
           (when niwa-view--agent-transcript-p
             (setq buffer-read-only t)))
         (signal (car err) (cdr err)))))))

(provide 'niwa-view-segments)
;;; niwa-view-segments.el ends here
