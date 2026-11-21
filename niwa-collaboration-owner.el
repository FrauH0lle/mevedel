;;; niwa-collaboration-owner.el --- Owner collaboration authorities -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the browser owner-link authorities: changing permission mode and
;; creating isolated sessions for authenticated writable guests, including
;; Conversation Forks of the shared session.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `niwa-chat'
(declare-function niwa--chat-buffer
                  "niwa-chat"
                  (session-name &optional create workspace working-directory))
(declare-function niwa--display-chat-buffer "niwa-chat" (chat-buffer))
(declare-function niwa--ensure-chat-preset "niwa-chat" (chat-buffer))
(declare-function niwa--workspace-sessions "niwa-chat" (workspace))
(declare-function niwa-chat-discard-buffers
                  "niwa-chat" (data-buffer &optional discard-modifications))
(autoload 'niwa--chat-buffer "niwa-chat")
(autoload 'niwa--display-chat-buffer "niwa-chat")
(autoload 'niwa--ensure-chat-preset "niwa-chat")
(autoload 'niwa--workspace-sessions "niwa-chat")
(autoload 'niwa-chat-discard-buffers "niwa-chat")

;; `niwa-collaboration'
(declare-function niwa-collaboration--guest
                  "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--guest-link
                  "niwa-collaboration" (room guest))
(declare-function niwa-collaboration--guest-text
                  "niwa-collaboration" (value))
(declare-function niwa-collaboration--publish-status
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-data-buffer
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-for-buffer
                  "niwa-collaboration" (data-buffer))
(declare-function niwa-collaboration--start
                  "niwa-collaboration" (session data-buffer))

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--save-guest-files
                  "niwa-collaboration-guest" (data-buffer images))
(autoload 'niwa-collaboration--save-guest-files "niwa-collaboration-guest")
(declare-function niwa-collaboration--stop-internal
                  "niwa-collaboration" (room reason))
(defvar niwa-collaboration--duplicate-prompt-window)

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--audience-peers-for
                  "niwa-collaboration-guest" (room audience))
(declare-function niwa-collaboration--request-id-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--room-workspace
                  "niwa-collaboration-guest" (room))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-failed-turn
                  "niwa-gptel-retry" (buffer failure-id))
(autoload 'niwa-gptel-retry-failed-turn "niwa-gptel-retry")

;; `niwa-interaction-prompt'
(declare-function niwa--prompt-user-with-overlay
                  "niwa-interaction-prompt"
                  (title content question help-echo-text callback
                         &optional host-only audience))

;; `niwa-models'
(declare-function niwa-model-candidates "niwa-models" ())
(declare-function niwa-model-resolve-provider
                  "niwa-models" (spec &optional noerror))
(declare-function niwa-model-set-session-provider
                  "niwa-models" (session provider &optional buffer))
(declare-function niwa-model-set-session-effort
                  "niwa-models" (session effort &optional buffer))
(declare-function niwa-model-supported-efforts "niwa-models" (model))
(autoload 'niwa-model-candidates "niwa-models")
(autoload 'niwa-model-resolve-provider "niwa-models")
(autoload 'niwa-model-set-session-provider "niwa-models")
(autoload 'niwa-model-set-session-effort "niwa-models")
(autoload 'niwa-model-supported-efforts "niwa-models")

;; `niwa-pending-inputs'
(declare-function niwa-view-enqueue-external-follow-up
                  "niwa-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'niwa-view-enqueue-external-follow-up "niwa-pending-inputs")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-transition
                  "niwa-permission-mode" (mode))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-sanitize
                  "niwa-session-artifacts" (name))

;; `niwa-session-fork'
(declare-function niwa-session-fork-create
                  "niwa-session-fork" (buffer target type))
(autoload 'niwa-session-fork-create "niwa-session-fork")

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-rewind
                  "niwa-session-rewind"
                  (buffer target &optional boundary scope confirm-function complete))
(autoload 'niwa-session-rewind-rewind "niwa-session-rewind")

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-turn-count "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(defvar niwa--session)


;;
;;; Owner-link authorities

(defconst niwa-collaboration--max-session-name-chars 48
  "Longest session name a guest may ask for.")

(defun niwa-collaboration--owner (room peer)
  "Return PEER\='s guest plist when it holds ROOM\='s owner token, else nil."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((plist-get guest :owner)))
    guest))

(defun niwa-collaboration--handle-set-mode (room peer frame)
  "Set the session permission mode from owner PEER\='s FRAME.

Only an owner-link guest may do this.  It is a typed frame rather than
the `mode\=' slash command, which stays refused for every guest: that
refusal protects the command allowlist from becoming an escalation path."
  (when-let* ((guest (niwa-collaboration--owner room peer))
              (requested (niwa-collaboration--guest-text
                          (plist-get frame :mode)))
              (mode (car (member requested '("ask" "edits" "full-auto"))))
              (data-buffer (niwa-collaboration--room-data-buffer room)))
    (with-current-buffer data-buffer
      (niwa-permission-mode-transition (intern mode)))
    (message "niwa: permission mode set to %s by owner %s"
             mode (plist-get guest :name))
    (niwa-collaboration--publish-status room)))

(defun niwa-collaboration--handle-session-models (room peer)
  "Send PEER the current model choices when it can request a session in ROOM.
Each model that declares reasoning efforts lists their names under its
label in the frame\='s efforts object."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((if (plist-get room :session)
                   (plist-get guest :writable)
                 (plist-get guest :owner))))
    (let ((candidates (niwa-model-candidates))
          (efforts (make-hash-table :test #'equal)))
      (dolist (candidate candidates)
        (when-let* ((names (niwa-model-supported-efforts
                            (plist-get (cdr candidate) :model))))
          (puthash (car candidate) (vconcat (mapcar #'symbol-name names))
                   efforts)))
      (niwa-collaboration--transport-send
       (plist-get room :transport) peer
       (list :t "session-models" :models (vconcat (mapcar #'car candidates))
             :efforts efforts)))))

(defun niwa-collaboration--new-session-reply
    (room peer request-id name &rest keys)
  "Send PEER in ROOM a new-session REQUEST-ID outcome for NAME and KEYS."
  (niwa-collaboration--transport-send
   (plist-get room :transport) peer
   (append (list :t "new-session" :reqId request-id :name name) keys)))

(defun niwa-collaboration--discard-created-session (room buffer)
  "Stop ROOM and discard BUFFER after guest-session creation failed."
  (when room
    (ignore-errors
      (niwa-collaboration--stop-internal room 'start-failed)))
  (niwa-chat-discard-buffers buffer t))

(defun niwa-collaboration--offer-room (room peer name link)
  "Tell PEER in ROOM that it may join the room NAME at LINK.

Unlike a reply, this answers no request of PEER's own, so it travels as
its own frame: pairing it with a request the receiver did not make is
how one guest's approval lands on another guest's pending card."
  (niwa-collaboration--transport-send
   (plist-get room :transport) peer
   (list :t "room" :name name :link link)))

(defun niwa-collaboration--offer-room-to-owners (room except name link)
  "Offer the room NAME at LINK to ROOM's owner guests other than EXCEPT.

An owner may create a session outright and may approve someone else's,
so a new room in the same workspace is one it can already reach; being
told is the difference between reaching it and knowing it exists.  The
requester is skipped because its own reply already carried a link."
  (dolist (owner-peer (niwa-collaboration--audience-peers-for
                       room 'owner))
    (unless (eql owner-peer except)
      (niwa-collaboration--offer-room room owner-peer name link))))

(defun niwa-collaboration--create-guest-session
    (room peer guest request-id name prompt &optional images model effort)
  "Create session NAME for GUEST and reply to PEER's REQUEST-ID.
A nil NAME creates an unnamed session; replies and offers then carry the
name it starts with, its id, until it is titled from its first prompt.

The new room shares only ROOM's workspace and working directory, or a
lobby's workspace and root.  GUEST receives the same authority tier it
already holds, and ROOM's other owner guests are offered an owner link
to it.  The session gets the default chat preset and then MODEL, a
\"BACKEND:MODEL\" label, when given: the preset may name a model of its
own, which would otherwise replace the guest's choice.  EFFORT, a
reasoning effort symbol MODEL supports, follows the model.  PROMPT, when
given, is queued with attachment IMAGES only after the new room starts;
a failed start, save or enqueue discards the partial session and
reports failure without stopping ROOM."
  (let* ((session (plist-get room :session))
         (workspace (niwa-collaboration--room-workspace room))
         (directory (or (plist-get room :directory)
                        (and session
                             (niwa-session-working-directory session)))))
    (cond
     ((not (and workspace directory))
      (niwa-collaboration--new-session-reply
       room peer request-id name :ok :json-false
       :message "This room has no workspace"))
     ((and name (assoc name (niwa--workspace-sessions workspace)))
      (niwa-collaboration--new-session-reply
       room peer request-id name :ok :json-false
       :message (format "A session named %s already exists" name)))
     (t
      (let (buffer new-room failure)
        (condition-case err
            (let* ((created-buffer
                    (setq buffer
                          (niwa--chat-buffer name t workspace directory)))
                   (new-session
                    (buffer-local-value 'niwa--session created-buffer)))
              (setq name (niwa-session-name new-session))
              (niwa--ensure-chat-preset created-buffer)
              (when model
                (niwa-model-set-session-provider
                 new-session (niwa-model-resolve-provider model)
                 created-buffer))
              (when effort
                (niwa-model-set-session-effort
                 new-session effort created-buffer))
              (setq new-room
                    (niwa-collaboration--start new-session created-buffer))
              (when prompt
                (let ((paths (niwa-collaboration--save-guest-files
                              created-buffer images)))
                  (unless (niwa-view-enqueue-external-follow-up
                           created-buffer prompt
                           :guest-name (plist-get guest :name)
                           :guest-id (plist-get guest :guest-id)
                           :paths paths)
                    (dolist (path paths) (ignore-errors (delete-file path)))
                    (error "Could not queue the approved first prompt")))))
          (error (setq failure (error-message-string err))))
        (if failure
            (progn
              (niwa-collaboration--discard-created-session new-room buffer)
              (niwa-collaboration--new-session-reply
               room peer request-id name :ok :json-false
               :message (format "Session could not be created: %s" failure)))
          (if (niwa-collaboration--new-session-reply
               room peer request-id name :ok t
               :link (niwa-collaboration--guest-link new-room guest))
              (progn
                ;; Offering is incidental too: an owner that cannot be
                ;; told still has a room the requester can reach.
                (ignore-errors
                  (niwa-collaboration--offer-room-to-owners
                   room peer name (plist-get new-room :link-owner)))
                ;; Presentation is incidental to the protocol transaction: a
                ;; display failure must not revoke the room whose link was sent.
                (ignore-errors (niwa--display-chat-buffer buffer))
                (message "niwa: session %s created for guest %s"
                         name (plist-get guest :name)))
            ;; Without delivery the requester cannot reach the room and a
            ;; retry would collide with its hidden session name.
            (niwa-collaboration--discard-created-session
             new-room buffer))))))))

(defun niwa-collaboration--ask-host-new-session
    (room peer guest request-id name prompt &optional images model effort)
  "Ask the host to approve GUEST's REQUEST-ID for a session named NAME.
PROMPT and its attachment IMAGES start the session once approved, on
MODEL and its reasoning EFFORT when given.  A nil NAME asks for an
unnamed session.

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
  (if-let* ((data-buffer (niwa-collaboration--room-data-buffer room)))
      (with-current-buffer data-buffer
        (plist-put guest :pending-new-session (cons request-id name))
        (niwa--prompt-user-with-overlay
         "New session requested"
         (concat
          (format "Guest:  %s\n" (plist-get guest :name))
          (format "Name:   %s\n"
                  (or name "(none -- titled from the first prompt)"))
          (format "Model:  %s\n" (or model "default"))
          (when effort
            (format "Effort: %s\n" effort))
          (format "Prompt: %s\n"
                  (or prompt "(none -- the session starts empty)"))
          (when (> (length images) 0)
            (format "Files:  %d attached\n" (length images))))
         (if name
             (format "Create session \"%s\" for %s?"
                     name (plist-get guest :name))
           (format "Create a new session for %s?" (plist-get guest :name)))
         nil
         (lambda (outcome)
           (when (and (eq room (niwa-collaboration--room-for-buffer
                                data-buffer))
                      (eq guest (niwa-collaboration--guest room peer)))
             (plist-put guest :pending-new-session nil)
             (pcase outcome
               ('approve
                (niwa-collaboration--create-guest-session
                 room peer guest request-id name prompt images model effort))
               (`(feedback . ,text)
                (niwa-collaboration--new-session-reply
                 room peer request-id name :ok :json-false :message text))
               (_
                (niwa-collaboration--new-session-reply
                 room peer request-id name :ok :json-false
                 :message "The host declined this request")))))
         nil
         'owner))
    (niwa-collaboration--new-session-reply
     room peer request-id name :ok :json-false
     :message "This room has no session")))

(defun niwa-collaboration--handle-new-session (room peer frame)
  "Act on writable guest PEER's request in FRAME for a new session.
FRAME may name the session's model; one this host has not registered
refuses the request rather than falling back to the default.  FRAME may
also name a reasoning effort, which needs a model that supports it; the
default model is only known once its preset applies.  A request
without a usable name creates an unnamed session, which is titled from
its first prompt like one started in Emacs without a name."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((plist-get guest :writable))
              (request-id (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p request-id)))
    (let* ((raw (niwa-collaboration--guest-text (plist-get frame :name)))
           (sanitized (and raw
                           (niwa-session-artifacts-sanitize
                            (truncate-string-to-width
                             raw niwa-collaboration--max-session-name-chars))))
           (name (and sanitized (string-match-p "[A-Za-z0-9]" sanitized)
                      sanitized))
           (images (plist-get frame :images))
           (prompt (or (niwa-collaboration--guest-text
                        (plist-get frame :prompt))
                       ;; Files alone still say what the session is for.
                       (and images "See the attached file.")))
           (model (niwa-collaboration--guest-text (plist-get frame :model)))
           (provider (and model (cdr (assoc model (niwa-model-candidates)))))
           (effort-name (niwa-collaboration--guest-text
                         (plist-get frame :effort)))
           (effort (and effort-name provider
                        ;; Never intern browser text: a declared effort
                        ;; is already an interned symbol.
                        (car (memq (intern-soft effort-name)
                                   (niwa-model-supported-efforts
                                    (plist-get provider :model))))))
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
        (niwa-collaboration--new-session-reply
         room peer request-id name :ok :json-false
         :message (if (cdr waiting)
                      (format "Your request for %s is still waiting"
                              (cdr waiting))
                    "Your request for a new session is still waiting")))
       ((and last
             (eql request-id (car last))
             (< (- now (cdr last))
                niwa-collaboration--duplicate-prompt-window))
        nil)
       ;; Only offered labels: probing an arbitrary name could register it.
       ((and model (not provider))
        (niwa-collaboration--new-session-reply
         room peer request-id name :ok :json-false
         :message (format "Unknown model %s" model)))
       ((and effort-name (not effort))
        (niwa-collaboration--new-session-reply
         room peer request-id name :ok :json-false
         :message (if model
                      (format "Model %s has no reasoning effort %s"
                              model effort-name)
                    "Choose a model to set its reasoning effort")))
       (t
        (plist-put guest :last-new-session (cons request-id now))
        (if (plist-get guest :owner)
            (niwa-collaboration--create-guest-session
             room peer guest request-id name prompt images model effort)
          (niwa-collaboration--ask-host-new-session
           room peer guest request-id name prompt images model effort)))))))

(defvar niwa-collaboration--max-fork-outcomes 32
  "Settled fork outcomes kept per admitted connection for retries.")

(defun niwa-collaboration--fork-ledger (room peer)
  "Return PEER's fork ledger in ROOM, creating it when absent.
The ledger is a plist with `:outcomes', an alist from request id to
`pending' or the reply keys that settled it, newest first, and
`:forgotten', the highest request id dropped from that bounded alist.
ROOM keys it by PEER rather than storing
it on the guest plist, which a repeated hello replaces; the guest
lifecycle drops it when PEER leaves or the relay connection goes down,
so it lasts exactly as long as the admitted connection."
  (let ((ledgers (or (plist-get room :fork-ledgers)
                     (let ((table (make-hash-table :test #'eql)))
                       (plist-put room :fork-ledgers table)
                       table))))
    (or (gethash peer ledgers)
        (puthash peer (list :forgotten -1 :outcomes nil) ledgers))))

(defun niwa-collaboration--fork-settle (room peer ledger request-id keys)
  "Record KEYS as REQUEST-ID's outcome in LEDGER and send them to PEER in ROOM.
KEYS are the `new-session' reply fields, starting with `:name'."
  (let ((outcomes (plist-get ledger :outcomes)))
    (setf (alist-get request-id outcomes) keys)
    (when (> (length outcomes) niwa-collaboration--max-fork-outcomes)
      (dolist (dropped (nthcdr niwa-collaboration--max-fork-outcomes outcomes))
        (plist-put ledger :forgotten
                   (max (car dropped) (plist-get ledger :forgotten))))
      (setq outcomes (seq-take outcomes niwa-collaboration--max-fork-outcomes)))
    (plist-put ledger :outcomes outcomes))
  (apply #'niwa-collaboration--new-session-reply
         room peer request-id (plist-get keys :name) keys))

(defun niwa-collaboration--create-fork (data-buffer fork-point)
  "Fork the session in DATA-BUFFER at FORK-POINT and start the child's room.
Return (NEW-ROOM NAME FAILURE).  The child buffer is owned here: when
its room does not start, its live buffer, view and lock are discarded,
while the published fork stays saved for the host to open."
  (let (child-buffer child new-room failure)
    (condition-case err
        (setq child-buffer (niwa-session-fork-create
                            data-buffer (list :fork-point-id fork-point)
                            'conversation))
      (error (setq failure (format "Fork could not be created: %s"
                                   (error-message-string err)))))
    (when child-buffer
      (setq child (buffer-local-value 'niwa--session child-buffer))
      (unwind-protect
          (condition-case err
              (setq new-room (niwa-collaboration--start child child-buffer))
            (error (setq failure (error-message-string err))))
        (if new-room
            ;; Like a session created for a guest, the host sees it open.
            (ignore-errors (niwa--display-chat-buffer child-buffer))
          (niwa-chat-discard-buffers child-buffer t)
          (setq failure
                (format "Fork saved as session %s, but its room could not start: %s.  Open it in Emacs with M-x niwa"
                        (niwa-session-session-id child)
                        (or failure "the room did not start"))))))
    (list new-room (and child (niwa-session-name child)) failure)))

(defun niwa-collaboration--handle-fork (room peer frame)
  "Fork ROOM's session at FRAME's fork point for owner PEER.

This is the browser's Conversation Fork: a new session whose transcript
ends at the assistant response that carries FRAME's `forkPoint', in the
same working directory, with the source session left unchanged.  It is
an owner-link authority, like creating a session outright, because the
child copies the session's configuration and history.  The fork shares
the `new-session' reply, so the requester's waiting notice settles with
an owner link to the child's room; other owners are offered it too.

Each request id forks at most once per admitted connection.  A retry of
a settled request receives its recorded outcome again and a retry while
it runs is ignored.  The ledger keeps a bounded number of outcomes, so
an id at or below the highest one it has dropped may be a repeat it no
longer remembers: that is refused as stale rather than performed.

A fork that published but whose room could not start stays a saved
session: discarding it would lose the only copy of a state the
requester asked to keep.  The refusal names it and how to open it."
  (when-let* ((guest (niwa-collaboration--owner room peer))
              (request-id (plist-get frame :reqId))
              ((niwa-collaboration--request-id-p request-id)))
    (let* ((ledger (niwa-collaboration--fork-ledger room peer))
           (known (assq request-id (plist-get ledger :outcomes))))
      (cond
       ((eq (cdr known) 'pending) nil)
       (known
        (apply #'niwa-collaboration--new-session-reply
               room peer request-id (plist-get (cdr known) :name) (cdr known)))
       ((<= request-id (plist-get ledger :forgotten))
        (niwa-collaboration--new-session-reply
         room peer request-id nil :ok :json-false
         :message "This fork request is stale and was not repeated"))
       (t
        (plist-put ledger :outcomes
                   (cons (cons request-id 'pending)
                         (plist-get ledger :outcomes)))
        (let ((fork-point (niwa-collaboration--guest-text
                           (plist-get frame :forkPoint)))
              (data-buffer (niwa-collaboration--room-data-buffer room))
              keys)
          (unwind-protect
              (setq
               keys
               (if (not (and fork-point data-buffer (plist-get room :session)))
                   (list :name nil :ok :json-false
                         :message "Choose a settled assistant response to fork")
                 (pcase-let ((`(,new-room ,name ,failure)
                              (niwa-collaboration--create-fork
                               data-buffer fork-point)))
                   (if (not new-room)
                       (list :name name :ok :json-false :message failure)
                     (ignore-errors
                       (niwa-collaboration--offer-room-to-owners
                        room peer name (plist-get new-room :link-owner)))
                     (message "niwa: session forked as %s by owner %s"
                              name (plist-get guest :name))
                     (list :name name :ok t
                           :link (niwa-collaboration--guest-link
                                  new-room guest))))))
            ;; A non-local exit still settles the id, so its retry is
            ;; answered rather than ignored forever.
            (niwa-collaboration--fork-settle
             room peer ledger request-id
             (or keys (list :name nil :ok :json-false
                            :message "The fork was interrupted"))))))))))

(defun niwa-collaboration--handle-history-action (room peer frame)
  "Apply owner PEER's conversation history action in ROOM from FRAME.
The browser confirms before sending; host validation never opens a prompt."
  (when (niwa-collaboration--owner room peer)
    (let ((buffer (niwa-collaboration--room-data-buffer room))
          (request-id (plist-get frame :reqId)))
      (when (niwa-collaboration--request-id-p request-id)
        (cl-labels ((reply (_result &optional diagnosis)
                      (ignore-errors
                        (niwa-collaboration--transport-send
                         (plist-get room :transport) peer
                         (append (list :t "history-action" :reqId request-id
                                       :ok (if diagnosis :json-false t))
                                 (when diagnosis (list :message diagnosis)))))))
         (condition-case err
            (progn
              (unless (and buffer (plist-get room :session))
                (user-error "This room has no session"))
              (pcase (plist-get frame :t)
                ("rewind"
                 (unless (and (integerp (plist-get frame :turnCount))
                              (= (plist-get frame :turnCount)
                                 (or (niwa-session-turn-count
                                      (plist-get room :session)) 0)))
                   (user-error "The conversation changed; review Rewind again"))
                 (let ((point-id (niwa-collaboration--guest-text
                                  (plist-get frame :forkPoint))))
                   (unless point-id
                     (user-error "Choose a settled assistant response to rewind"))
                   (niwa-session-rewind-rewind
                    buffer (list :fork-point-id point-id) 'after 'conversation
                    (lambda (_impact) t) #'reply)))
                ("retry-turn"
                 (let ((failure-id (niwa-collaboration--guest-text
                                    (plist-get frame :failureId))))
                   (unless failure-id (user-error "Choose a failed turn to retry"))
                   (niwa-gptel-retry-failed-turn buffer failure-id)
                   (reply t)))
                (_ (user-error "Unknown history action"))))
          (error
           (reply nil (error-message-string err)))))))))

(provide 'niwa-collaboration-owner)
;;; niwa-collaboration-owner.el ends here
