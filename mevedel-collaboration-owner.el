;;; mevedel-collaboration-owner.el --- Owner collaboration authorities -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the browser owner-link authorities: changing permission mode and
;; creating isolated sessions for authenticated writable guests.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `mevedel-chat'
(declare-function mevedel--chat-buffer
                  "mevedel-chat"
                  (session-name &optional create workspace working-directory))
(declare-function mevedel--display-chat-buffer "mevedel-chat" (chat-buffer))
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--chat-buffer "mevedel-chat")
(autoload 'mevedel--display-chat-buffer "mevedel-chat")
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--publish-status
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-buffer
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration--start
                  "mevedel-collaboration" (session data-buffer))
(declare-function mevedel-collaboration--stop-internal
                  "mevedel-collaboration" (room reason))
(defvar mevedel-collaboration--duplicate-prompt-window)

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--audience-peers-for
                  "mevedel-collaboration-guest" (room audience))
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt-user-with-overlay
                  "mevedel-interaction-prompt"
                  (title content question help-echo-text callback
                         &optional host-only audience))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'mevedel-view-enqueue-external-follow-up "mevedel-pending-inputs")

;; `mevedel-permission-mode'
(declare-function mevedel-permission-mode-transition
                  "mevedel-permission-mode" (mode))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-sanitize
                  "mevedel-session-artifacts" (name))

;; `mevedel-structs'
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)


;;
;;; Owner-link authorities

(defconst mevedel-collaboration--max-session-name-chars 48
  "Longest session name a guest may ask for.")

(defun mevedel-collaboration--owner (room peer)
  "Return PEER\='s guest plist when it holds ROOM\='s owner token, else nil."
  (when-let* ((guest (mevedel-collaboration--guest room peer))
              ((plist-get guest :owner)))
    guest))

(defun mevedel-collaboration--handle-set-mode (room peer frame)
  "Set the session permission mode from owner PEER\='s FRAME.

Only an owner-link guest may do this.  It is a typed frame rather than
the `mode\=' slash command, which stays refused for every guest: that
refusal protects the command allowlist from becoming an escalation path."
  (when-let* ((guest (mevedel-collaboration--owner room peer))
              (requested (mevedel-collaboration--guest-text
                          (plist-get frame :mode)))
              (mode (car (member requested '("ask" "edits" "full-auto"))))
              (data-buffer (mevedel-collaboration--room-data-buffer room)))
    (with-current-buffer data-buffer
      (mevedel-permission-mode-transition (intern mode)))
    (message "mevedel: permission mode set to %s by owner %s"
             mode (plist-get guest :name))
    (mevedel-collaboration--publish-status room)))

(defun mevedel-collaboration--new-session-reply
    (room peer request-id name &rest keys)
  "Send PEER in ROOM a new-session REQUEST-ID outcome for NAME and KEYS."
  (mevedel-collaboration--transport-send
   (plist-get room :transport) peer
   (append (list :t "new-session" :reqId request-id :name name) keys)))

(defun mevedel-collaboration--discard-created-session (room buffer)
  "Stop ROOM and discard BUFFER after guest-session creation failed."
  (when room
    (ignore-errors
      (mevedel-collaboration--stop-internal room 'start-failed)))
  (when (buffer-live-p buffer)
    (let ((view-buffer (buffer-local-value 'mevedel--view-buffer buffer)))
      (dolist (candidate (list buffer view-buffer))
        (when (buffer-live-p candidate)
          (let* ((hooks (buffer-local-value 'kill-buffer-hook candidate))
                 (safe-hooks
                  (lambda ()
                    (let ((kill-buffer-hook hooks))
                      (run-hook-wrapped
                       'kill-buffer-hook
                       (lambda (hook)
                         (let ((kill-buffer-hook nil))
                           (ignore-errors (funcall hook)))
                         nil))))))
            (with-current-buffer candidate
              (set-buffer-modified-p nil)
              (let ((kill-buffer-query-functions nil)
                    (kill-buffer-hook (list safe-hooks)))
                (ignore-errors (kill-buffer candidate)))))
          (when (buffer-live-p candidate)
            (with-current-buffer candidate
              (let ((kill-buffer-hook nil)
                    (kill-buffer-query-functions nil))
                (ignore-errors (kill-buffer candidate))))))))))

(defun mevedel-collaboration--offer-room (room peer name link)
  "Tell PEER in ROOM that it may join the room NAME at LINK.

Unlike a reply, this answers no request of PEER's own, so it travels as
its own frame: pairing it with a request the receiver did not make is
how one guest's approval lands on another guest's pending card."
  (mevedel-collaboration--transport-send
   (plist-get room :transport) peer
   (list :t "room" :name name :link link)))

(defun mevedel-collaboration--offer-room-to-owners (room except name link)
  "Offer the room NAME at LINK to ROOM's owner guests other than EXCEPT.

An owner may create a session outright and may approve someone else's,
so a new room in the same workspace is one it can already reach; being
told is the difference between reaching it and knowing it exists.  The
requester is skipped because its own reply already carried a link."
  (dolist (owner-peer (mevedel-collaboration--audience-peers-for
                       room 'owner))
    (unless (eql owner-peer except)
      (mevedel-collaboration--offer-room room owner-peer name link))))

(defun mevedel-collaboration--create-guest-session
    (room peer guest request-id name prompt)
  "Create session NAME for GUEST and reply to PEER's REQUEST-ID.

The new room shares only ROOM's workspace and working directory.  GUEST
receives the same authority tier it already holds, and ROOM's other
owner guests are offered an owner link to it.  PROMPT, when given,
is queued only after the new room starts; a failed start or enqueue
discards the partial session and reports failure without stopping ROOM."
  (let* ((session (plist-get room :session))
         (workspace (and session (mevedel-session-workspace session)))
         (directory (and session (mevedel-session-working-directory session))))
    (cond
     ((not (and workspace directory))
      (mevedel-collaboration--new-session-reply
       room peer request-id name :ok :json-false
       :message "This room has no workspace"))
     ((assoc name (mevedel--workspace-sessions workspace))
      (mevedel-collaboration--new-session-reply
       room peer request-id name :ok :json-false
       :message (format "A session named %s already exists" name)))
     (t
      (let (buffer new-room failure)
        (condition-case err
            (let* ((created-buffer
                    (mevedel--chat-buffer name t workspace directory))
                   (new-session
                    (buffer-local-value 'mevedel--session created-buffer)))
              (setq buffer created-buffer
                    new-room
                    (mevedel-collaboration--start new-session created-buffer))
              (when (and prompt
                         (not (mevedel-view-enqueue-external-follow-up
                               created-buffer prompt
                               :guest-name (plist-get guest :name)
                               :guest-id (plist-get guest :guest-id))))
                (error "Could not queue the approved first prompt")))
          (error (setq failure (error-message-string err))))
        (if failure
            (progn
              (mevedel-collaboration--discard-created-session new-room buffer)
              (mevedel-collaboration--new-session-reply
               room peer request-id name :ok :json-false
               :message (format "Session could not be created: %s" failure)))
          (if (mevedel-collaboration--new-session-reply
               room peer request-id name :ok t
               :link (plist-get new-room
                                (if (plist-get guest :owner)
                                    :link-owner
                                  :link-full)))
              (progn
                ;; Offering is incidental too: an owner that cannot be
                ;; told still has a room the requester can reach.
                (ignore-errors
                  (mevedel-collaboration--offer-room-to-owners
                   room peer name (plist-get new-room :link-owner)))
                ;; Presentation is incidental to the protocol transaction: a
                ;; display failure must not revoke the room whose link was sent.
                (ignore-errors (mevedel--display-chat-buffer buffer))
                (message "mevedel: session %s created for guest %s"
                         name (plist-get guest :name)))
            ;; Without delivery the requester cannot reach the room and a
            ;; retry would collide with its hidden session name.
            (mevedel-collaboration--discard-created-session
             new-room buffer))))))))

(defun mevedel-collaboration--ask-host-new-session
    (room peer guest request-id name prompt)
  "Ask the host to approve GUEST's REQUEST-ID for a session named NAME.

The prompt reaches Emacs and ROOM's owner-link guests.  An owner may
create a session outright, so approving someone else's request is no new
authority; and only a non-owner ever gets here, because an owner's own
request never becomes a question.  Restricting the audience to owners
therefore already excludes the requester, whatever browser it shares
with an owner.  The callback acts only while the original ROOM and
authenticated GUEST still own DATA-BUFFER and PEER.

GUEST is latched for the wait: a person answers this, and a guest that
can stack requests while nobody is at the keyboard hands the host a pile
of prompts for the same session, of which every approval after the first
fails on the name that the first one took."
  (if-let* ((data-buffer (mevedel-collaboration--room-data-buffer room)))
      (with-current-buffer data-buffer
        (plist-put guest :pending-new-session (cons request-id name))
        (mevedel--prompt-user-with-overlay
         "New session requested"
         (concat
          (format "Guest:  %s\n" (plist-get guest :name))
          (format "Name:   %s\n" name)
          (format "Prompt: %s\n"
                  (or prompt "(none -- the session starts empty)")))
         (format "Create session \"%s\" for %s?"
                 name (plist-get guest :name))
         nil
         (lambda (outcome)
           (when (and (eq room (mevedel-collaboration--room-for-buffer
                                data-buffer))
                      (eq guest (mevedel-collaboration--guest room peer)))
             (plist-put guest :pending-new-session nil)
             (pcase outcome
               ('approve
                (mevedel-collaboration--create-guest-session
                 room peer guest request-id name prompt))
               (`(feedback . ,text)
                (mevedel-collaboration--new-session-reply
                 room peer request-id name :ok :json-false :message text))
               (_
                (mevedel-collaboration--new-session-reply
                 room peer request-id name :ok :json-false
                 :message "The host declined this request")))))
         nil
         'owner))
    (mevedel-collaboration--new-session-reply
     room peer request-id name :ok :json-false
     :message "This room has no session")))

(defun mevedel-collaboration--handle-new-session (room peer frame)
  "Act on writable guest PEER's request in FRAME for a new session."
  (when-let* ((guest (mevedel-collaboration--guest room peer))
              ((plist-get guest :writable))
              (request-id (plist-get frame :reqId))
              ((mevedel-collaboration--request-id-p request-id))
              (raw (mevedel-collaboration--guest-text (plist-get frame :name)))
              (name (mevedel-session-artifacts-sanitize
                     (truncate-string-to-width
                      raw mevedel-collaboration--max-session-name-chars)))
              ((string-match-p "[A-Za-z0-9]" name)))
    (let* ((prompt (mevedel-collaboration--guest-text
                    (plist-get frame :prompt)))
           (waiting (plist-get guest :pending-new-session))
           (last (plist-get guest :last-new-session))
           (now (float-time)))
      (cond
       ;; A retransmitted request is the same request, not a second
       ;; decision that should refuse the first one.
       ((and waiting (eql request-id (car waiting))) nil)
       ;; One question to a person at a time.  A second request cannot
       ;; be answered usefully anyway: approving both fails the later
       ;; one on the name the earlier one took.
       (waiting
        (mevedel-collaboration--new-session-reply
         room peer request-id name :ok :json-false
         :message (format "Your request for %s is still waiting"
                          (cdr waiting))))
       ((and last
             (eql request-id (car last))
             (< (- now (cdr last))
                mevedel-collaboration--duplicate-prompt-window))
        nil)
       (t
        (plist-put guest :last-new-session (cons request-id now))
        (if (plist-get guest :owner)
            (mevedel-collaboration--create-guest-session
             room peer guest request-id name prompt)
          (mevedel-collaboration--ask-host-new-session
           room peer guest request-id name prompt)))))))

(provide 'mevedel-collaboration-owner)
;;; mevedel-collaboration-owner.el ends here
