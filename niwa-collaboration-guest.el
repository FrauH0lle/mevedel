;;; niwa-collaboration-guest.el --- collaboration guest protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Handles untrusted guest protocol frames for a live collaboration room:
;; authentication, Web Push registration, prompt and attachment intake,
;; retraction, abort, and relay peer lifecycle.  Room ownership, projection,
;; outbound publication, and public commands remain in the collaboration
;; facade.

;;; Code:


(autoload 'niwa-collaboration-recovery-handle "niwa-collaboration-recovery")
(autoload 'niwa-collaboration-recovery-send "niwa-collaboration-recovery")
(declare-function niwa-collaboration-recovery-handle "niwa-collaboration-recovery" (room peer frame))
(declare-function niwa-collaboration-recovery-send "niwa-collaboration-recovery" (room peer))

(eval-when-compile
  (require 'cl-lib))

(require 'json)
(require 'niwa-utilities)

;; `niwa-collaboration'
;; `niwa-tool-fs-read'
(declare-function niwa-tool-fs-read--binary-extension-p "niwa-tool-fs-read" (filename))
(autoload 'niwa-tool-fs-read--binary-extension-p "niwa-tool-fs-read")

(declare-function niwa-collaboration--base64url-decode
                  "niwa-collaboration" (string))
(declare-function niwa-collaboration--guest
                  "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--guest-text
                  "niwa-collaboration" (value))
(declare-function niwa-collaboration--publish-queue
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--publish-status
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--queue-position
                  "niwa-collaboration" (room entry))
(declare-function niwa-collaboration--room-data-buffer
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--room-for-buffer
                  "niwa-collaboration" (data-buffer))
(declare-function niwa-collaboration--room-for-overlay
                  "niwa-collaboration" (overlay))
(declare-function niwa-collaboration--room-for-session
                  "niwa-collaboration" (session))
(declare-function niwa-collaboration--room-list
                  "niwa-collaboration" ())
(declare-function niwa-collaboration--sanitize-guest-id
                  "niwa-collaboration" (value))
(declare-function niwa-collaboration--sanitize-guest-name
                  "niwa-collaboration" (name))
(declare-function niwa-collaboration--send-queue-state
                  "niwa-collaboration" (room peer guest &optional force))
(declare-function niwa-collaboration--status-frame
                  "niwa-collaboration" (room))
(declare-function niwa-collaboration--skip-oversize
                  "niwa-collaboration" (room send))
(declare-function niwa-collaboration--send-guarded
                  "niwa-collaboration" (room peer frame))
(defvar niwa-collaboration--attachment-extensions)
(defvar niwa-collaboration--duplicate-prompt-window)
(defvar niwa-collaboration--max-attachment-bytes)
(defvar niwa-collaboration--max-prompt-attachments)
(defvar niwa-collaboration--max-prompt-bytes)
(defvar niwa-collaboration-guest-skills)
(defvar niwa-collaboration-remote-interactions)
(defvar niwa-collaboration-unsafe-guest-commands)

(defvar niwa-collaboration-needs-host-message)

;; `niwa-collaboration-agent'
(declare-function niwa-collaboration--agents-frame
                  "niwa-collaboration-agent" (room))
(declare-function niwa-collaboration--handle-fetch-agent
                  "niwa-collaboration-agent" (room peer frame))
(declare-function niwa-collaboration--handle-execution-result-get
                  "niwa-collaboration-agent" (room peer frame))

;; `niwa-collaboration-artifact'
(declare-function niwa-collaboration--handle-artifact-get
                  "niwa-collaboration-artifact" (room peer frame))
(declare-function niwa-collaboration--handle-artifact-delete
                  "niwa-collaboration-artifact" (room peer frame))
(declare-function niwa-collaboration--handle-store-action
                  "niwa-collaboration-artifact" (room peer frame))
(declare-function niwa-collaboration--handle-store-list
                  "niwa-collaboration-artifact" (room peer frame))
(declare-function niwa-collaboration--store-frame
                  "niwa-collaboration-artifact" (room &optional rows))

;; `niwa-collaboration-background'
(declare-function niwa-collaboration--background-frame
                  "niwa-collaboration-background" (room))
(declare-function niwa-collaboration--handle-background-locate
                  "niwa-collaboration-background" (room peer frame))
(declare-function niwa-collaboration--handle-background-stop
                  "niwa-collaboration-background" (room peer frame))
(autoload 'niwa-collaboration--background-frame "niwa-collaboration-background")
(autoload 'niwa-collaboration--handle-background-stop "niwa-collaboration-background")
(autoload 'niwa-collaboration--handle-background-locate "niwa-collaboration-background")

;; `niwa-collaboration-discussions'
(declare-function niwa-collaboration-discussions-handle
                  "niwa-collaboration-discussions" (room peer frame))

;; `niwa-collaboration-files'
(declare-function niwa-collaboration-files-handle-upload
                  "niwa-collaboration-files" (owner peer frame root))

;; `niwa-collaboration-history'
(declare-function niwa-collaboration--handle-history-get
                  "niwa-collaboration-history" (room peer frame))
(declare-function niwa-collaboration--publish-history
                  "niwa-collaboration-history" (room &optional peer))
(autoload 'niwa-collaboration--handle-history-get
  "niwa-collaboration-history")
(autoload 'niwa-collaboration--publish-history
  "niwa-collaboration-history")

;; `niwa-collaboration-lobby'
(declare-function niwa-collaboration-lobby--find
                  "niwa-collaboration-lobby" (workspace))

;; `niwa-collaboration-presence'
(declare-function niwa-collaboration-presence-handle
                  "niwa-collaboration-presence" (room peer frame))
(declare-function niwa-collaboration-presence-publish
                  "niwa-collaboration-presence" (room))
(declare-function niwa-collaboration-presence-state
                  "niwa-collaboration-presence" (frame))

;; `niwa-collaboration-owner'
(declare-function niwa-collaboration--handle-new-session
                  "niwa-collaboration-owner" (room peer frame))
(declare-function niwa-collaboration--handle-session-models
                  "niwa-collaboration-owner" (room peer))
(declare-function niwa-collaboration--handle-set-mode
                  "niwa-collaboration-owner" (room peer frame))
(declare-function niwa-collaboration--handle-history-action
                  "niwa-collaboration-owner" (room peer frame))
(autoload 'niwa-collaboration--handle-history-action "niwa-collaboration-owner")
(declare-function niwa-collaboration--owner
                  "niwa-collaboration-owner" (room peer))
(autoload 'niwa-collaboration--handle-new-session
  "niwa-collaboration-owner")
(declare-function niwa-collaboration--handle-fork
                  "niwa-collaboration-owner" (room peer frame))
(autoload 'niwa-collaboration--handle-fork "niwa-collaboration-owner")
(autoload 'niwa-collaboration--handle-session-models
  "niwa-collaboration-owner")
(autoload 'niwa-collaboration--handle-set-mode
  "niwa-collaboration-owner")
(autoload 'niwa-collaboration--owner "niwa-collaboration-owner")

;; `niwa-collaboration-goal'
(declare-function niwa-collaboration--goal-frame
                  "niwa-collaboration-goal" (room))
(autoload 'niwa-collaboration--goal-frame "niwa-collaboration-goal")

;; `niwa-collaboration-projection'
(declare-function niwa-collaboration--canonical-records
                  "niwa-collaboration-projection" (data-buffer))
(declare-function niwa-collaboration--json-record
                  "niwa-collaboration-projection" (record))
(defvar niwa-collaboration--protocol-version)
(declare-function niwa-collaboration--protocol-error "niwa-collaboration-transport" ())

;; `niwa-collaboration-task'
(declare-function niwa-collaboration--tasks-frame
                  "niwa-collaboration-task" (room))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-control
                  "niwa-collaboration-transport" (transport control))
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))
(defvar niwa-collaboration--max-frame-json-bytes)

;; `niwa-interaction-prompt'
(declare-function niwa--prompt--settle
                  "niwa-interaction-prompt" (overlay outcome))
(declare-function niwa--prompt-user-with-overlay
                  "niwa-interaction-prompt"
                  (title content question help-echo-text callback
                         &optional host-only audience))

;; `niwa-models'
(declare-function niwa-model-candidates "niwa-models" ())
(autoload 'niwa-model-candidates "niwa-models")

;; `niwa-pending-inputs'
(declare-function niwa-pending-inputs--set-queues
                  "niwa-pending-inputs" (session &rest replacements))
(declare-function niwa-view--refused-question "niwa-pending-inputs" (err))
(declare-function niwa-view-enqueue-external-follow-up
                  "niwa-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'niwa-view--refused-question "niwa-pending-inputs")
(autoload 'niwa-view-enqueue-external-follow-up "niwa-pending-inputs")

;; `niwa-skills-core'
(declare-function niwa-skill-argument-hint
                  "niwa-skills-core" (skill))
(declare-function niwa-skill-name "niwa-skills-core" (skill))

;; `niwa-skills-ui'
(declare-function niwa-skills-user-visible-skills
                  "niwa-skills-ui" (session &optional inline-only))
(autoload 'niwa-skills-user-visible-skills "niwa-skills-ui")
(defvar niwa-slash-commands)

;; `niwa-sleep'
(declare-function niwa-sleep-update "niwa-sleep" (&rest _))
(autoload 'niwa-sleep-update "niwa-sleep")

;; `niwa-structs'
(declare-function niwa-session-pending-follow-ups
                  "niwa-structs" (session))
(declare-function niwa-session-set-pending-inputs
                  "niwa-structs" (session category entries))
(declare-function niwa-session-workspace "niwa-structs" (session))
(declare-function niwa-session-working-directory
                  "niwa-structs" (session))
(declare-function niwa-workspace-id "niwa-structs" (cl-x))
(declare-function niwa-workspace-name "niwa-structs" (cl-x))
(declare-function niwa-workspace-root "niwa-structs" (cl-x))
(declare-function niwa-workspace-type "niwa-structs" (cl-x))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-sanitize
                  "niwa-session-artifacts" (name))

;; `niwa-view'
(declare-function niwa-view--abort-data-buffer
                  "niwa-view" (data-buffer))
(autoload 'niwa-view--abort-data-buffer "niwa-view")

;; `niwa-view-composer'
(declare-function niwa-view-abort "niwa-view-composer" ())
(declare-function niwa-view-invocation-kind
                  "niwa-view-composer" (name &optional session))
(autoload 'niwa-view-abort "niwa-view-composer" nil t)
(autoload 'niwa-view-invocation-kind "niwa-view-composer")

;; `niwa-view-input-files'
(declare-function niwa-view--media-dir "niwa-view-input-files" ())
(autoload 'niwa-view--media-dir "niwa-view-input-files")

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" ())
(autoload 'niwa-view--interaction-rebuild "niwa-view-interaction")


;;
;;; Guest protocol primitives

(defun niwa-collaboration--request-id-p (value)
  "Return non-nil when VALUE is a bounded browser request id."
  (and (integerp value) (<= 0 value #x1fffffffffffff)))


;;
;;; Snapshot delivery

(defun niwa-collaboration--snapshot-frame-overhead ()
  "Return the encoded bytes a snapshot frame costs before its records.
Measured with an empty record array and the longer `final' spelling, so a
chunk that turns out not to be the last one cannot overflow.  A JSON array
adds one separator per record after the first, which is what the record
sizes alone never accounted for."
  (string-bytes
   (json-encode
    (list :t "snapshot-chunk"
          :records (vconcat nil)
          :final :json-false))))

(defun niwa-collaboration--records-frame (meta chunk final)
  "Return META with its record array CHUNK and last-chunk flag FINAL."
  (append meta (list :final (if final t :json-false) :records (vconcat chunk))))

(defun niwa-collaboration--snapshot-chunks (records &optional overhead)
  "Split RECORDS into chunks each within the encoded wire bound.
Send each list with `niwa-collaboration--records-frame'.
The bound belongs to the frame that goes on the wire, not to the records
in it.  A record too large to travel in a frame of its own is dropped:
emitting a frame the relay must refuse costs the host connection, and the
relay collects the room with it, so one oversized record would end the
session for every guest.  OVERHEAD is the encoded bytes the carrying
frame costs before its records; it defaults to the snapshot frame's.
No records still make one empty chunk: a receiver waits for a final one."
  (let* ((overhead (or overhead
                       (niwa-collaboration--snapshot-frame-overhead)))
         (limit niwa-collaboration--max-frame-json-bytes)
         chunks current (size 0))
    (dolist (record records)
      (let* ((object (niwa-collaboration--json-record record))
             (bytes (string-bytes (json-encode object))))
        (unless (> (+ overhead bytes) limit)
          ;; One separator for every record after the first in the chunk.
          (when (and current
                     (> (+ overhead size 1 bytes) limit))
            (push (nreverse current) chunks)
            (setq current nil size 0))
          (push object current)
          (setq size (+ size bytes (if (cdr current) 1 0))))))
    (when (or current (null chunks))
      (push (nreverse current) chunks))
    (nreverse chunks)))

(defconst niwa-collaboration--command-hints
  '(("plan" . "[prompt]")
    ("goal" . "[objective]")
    ("compact" . "[instructions]")
    ("stop" . "[execution]")
    ("review" . "uncommitted | HEAD | branch:NAME | commit:REV | instructions")
    ("verify" . "uncommitted | HEAD | branch:NAME | commit:REV | instructions"))
  "Argument hints for the local slash commands a guest may invoke.
Only commands whose arguments a guest can meaningfully supply appear;
anything absent is offered as an argument-less button.")

(defconst niwa-collaboration--invocation-defaults
  '(("review" . "uncommitted")
    ("verify" . "uncommitted"))
  "Arguments substituted when a guest invokes a command with none.
Without an argument these commands open a target picker in the host's
minibuffer, which a guest can neither see nor answer.")

(defun niwa-collaboration--guest-role (guest)
  "Return the link tier of GUEST: `owner\=', `full\=', or nil for a viewer."
  (cond ((plist-get guest :owner) 'owner)
        ((plist-get guest :writable) 'full)))

(defun niwa-collaboration--role-policy (role)
  "Return the `niwa-collaboration-guest-skills\=' value for ROLE.
An owner link without an entry of its own inherits the `full\=' entry."
  (cdr (or (assq role niwa-collaboration-guest-skills)
           (and (eq role 'owner)
                (assq 'full niwa-collaboration-guest-skills)))))

(defun niwa-collaboration--policy-admits-p (policy name)
  "Return non-nil when POLICY admits NAME.
POLICY has the shape of a `global-minor-mode\=' mode list: t, nil, or
a list read left to right until an element decides, with an implicit
nil at the end."
  (cond
   ((eq policy t) t)
   ((consp policy)
    (catch 'decided
      (dolist (element policy nil)
        (pcase element
          ('t (throw 'decided t))
          ('nil (throw 'decided nil))
          ((pred stringp)
           (when (equal element name) (throw 'decided t)))
          (`(not . ,names)
           (when (member name names) (throw 'decided nil)))))))))

(defun niwa-collaboration--guest-invocable-p (name role)
  "Return non-nil when a guest holding ROLE may invoke NAME at all."
  (and (stringp name)
       (not (member name niwa-collaboration-unsafe-guest-commands))
       (niwa-collaboration--policy-admits-p
        (niwa-collaboration--role-policy role) name)))

(defun niwa-collaboration--guest-skills-admitted-p (names role session)
  "Return non-nil when ROLE may apply selected skill NAMES in SESSION."
  (and (proper-list-p names) names (<= (length names) 6)
       (= (length names) (length (delete-dups (copy-sequence names))))
       (cl-every
        (lambda (name)
          (and (niwa-collaboration--guest-invocable-p name role)
               (eq (niwa-view-invocation-kind name session) 'skill)))
        names)))

(defun niwa-collaboration--guest-roster (room guest)
  "Return the invocations ROOM offers GUEST as JSON-safe descriptors.

Every local slash command and user-invocable skill the guest\='s tier
admits is listed, commands first.  Each entry carries the name, which
namespace it belongs to, and its argument hint, because a guest button
has to render the right sigil and say whether the invocation wants
arguments.  A name that resolves to neither namespace is dropped rather
than offered as a button that cannot work."
  (when-let* ((data-buffer (niwa-collaboration--room-data-buffer room))
              (view-buffer (buffer-local-value 'niwa--view-buffer
                                               data-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (let ((session (plist-get room :session))
            (role (niwa-collaboration--guest-role guest))
            roster)
        (dolist (name (delete-dups
                       (append
                        (mapcar #'car niwa-slash-commands)
                        (mapcar #'niwa-skill-name
                                (niwa-skills-user-visible-skills session)))))
          (when (niwa-collaboration--guest-invocable-p name role)
            (when-let* ((kind (niwa-view-invocation-kind name session)))
              (push (append
                     (list (cons "name" name)
                           (cons "kind" (symbol-name kind)))
                     (when-let* ((hint (niwa-collaboration--invocation-hint
                                        name kind session)))
                       (list (cons "hint" hint))))
                    roster))))
        (nreverse roster)))))

(defun niwa-collaboration--invocation-hint (name kind session)
  "Return the argument hint for invocation NAME of KIND, or nil."
  (pcase kind
    ('skill
     (when-let* ((skill (cl-find name
                                 (niwa-skills-user-visible-skills session)
                                 :key #'niwa-skill-name :test #'equal)))
       (niwa-skill-argument-hint skill)))
    ('command
     (cdr (assoc name niwa-collaboration--command-hints)))))

(defun niwa-collaboration--model-labels ()
  "Return the models a guest may pick for a new session, as labels."
  (vconcat (mapcar #'car (niwa-model-candidates))))

(defun niwa-collaboration--room-workspace (room)
  "Return ROOM's workspace, or nil.
A lobby has no session; it carries its workspace instead."
  (or (plist-get room :workspace)
      (when-let* ((session (plist-get room :session)))
        (niwa-session-workspace session))))

(defun niwa-collaboration--workspace-key (room)
  "Return an opaque key naming ROOM's workspace on this host, or nil.
A browser keeps the rooms it was given under it, so a room lists only
its own workspace's rooms; the key reveals no path."
  (when-let* ((workspace (niwa-collaboration--room-workspace room)))
    (substring (secure-hash 'sha256
                            (format "%s\0%s\0%s" (system-name)
                                    (niwa-workspace-type workspace)
                                    (niwa-workspace-id workspace)))
               0 32)))

(defun niwa-collaboration--project-name (workspace)
  "Return WORKSPACE's name as browser headers show it."
  (or (niwa-workspace-name workspace)
      (file-name-nondirectory
       (directory-file-name (niwa-workspace-root workspace)))))

(defun niwa-collaboration--lobby-field (room guest)
  "Return what GUEST in ROOM learns of its workspace's lobby.
An owner gets the running lobby's owner link, the way back to the
project from any of its rooms.  Other guests learn only whether a lobby
runs, so a view or full room link never reveals the workspace's other
sessions; their browser may hold a lobby link of its own.  The value is
the link, t, or `:json-false' when no lobby runs.  Nothing here starts
one."
  (if-let* (((featurep 'niwa-collaboration-lobby))
            ;; A lobby carries its workspace itself and needs no way back.
            ((not (plist-get room :workspace)))
            (workspace (niwa-collaboration--room-workspace room))
            (lobby (niwa-collaboration-lobby--find workspace)))
      (if (plist-get guest :owner) (plist-get lobby :link-owner) t)
    :json-false))

(defun niwa-collaboration--announce-lobby (workspace)
  "Tell every guest of WORKSPACE's rooms whether its lobby runs now.
One guest's failed send does not keep the others from hearing it."
  (let ((root (niwa-workspace-root workspace)))
    (dolist (room (niwa-collaboration--room-list))
      (when-let* ((transport (plist-get room :transport))
                  ;; A lobby needs no word about itself.
                  ((not (plist-get room :workspace)))
                  (own (niwa-collaboration--room-workspace room))
                  ((equal root (niwa-workspace-root own))))
        (maphash (lambda (peer guest)
                   (ignore-errors
                     (niwa-collaboration--transport-send
                      transport peer
                      (list :t "lobby-state"
                            :lobby (niwa-collaboration--lobby-field
                                    room guest)))))
                 (plist-get room :guests))))))

(defun niwa-collaboration--send-snapshot (room peer)
  "Send ROOM's welcome and chunked snapshot to guest PEER."
  (let* ((transport (plist-get room :transport))
         (guest (niwa-collaboration--guest room peer))
         (workspace (niwa-collaboration--room-workspace room))
         (records (plist-get room :records))
         ;; Guests joining while the records are unchanged share their
         ;; encoding; each publish drops it.
         (cached (plist-get room :snapshot-chunks))
         (chunks (if (and cached (eq records (car cached)))
                     (cdr cached)
                   (let ((chunks (niwa-collaboration--snapshot-chunks records)))
                     (plist-put room :snapshot-chunks (cons records chunks))
                     chunks))))
    (niwa-collaboration--transport-send
     transport peer
     (append
      (list :t "welcome"
            :proto niwa-collaboration--protocol-version
            :readOnly (if (plist-get guest :writable) :json-false t)
            :workspace (niwa-collaboration--workspace-key room)
            :lobby (niwa-collaboration--lobby-field room guest)
            ;; Count what is actually sent: a record too large for a frame of
            ;; its own is dropped, and promising it would leave the guest
            ;; waiting for a chunk that never arrives.
            :recordCount (apply #'+ (mapcar #'length chunks)))
      (when workspace
        (list :project (niwa-collaboration--project-name workspace)))
      ;; The host-curated roster is the guest's whole discovery
      ;; surface; a view link gets none, having no way to use it.
      (when-let* (((plist-get guest :writable))
                  (roster (niwa-collaboration--guest-roster room guest)))
        (list :commands (vconcat roster)))
      ;; Only a writable guest can ask for a session to choose one for.
      (when (plist-get guest :writable)
        (list :models (niwa-collaboration--model-labels)))))
    (cl-loop for rest on chunks do
             (niwa-collaboration--send-guarded
              room peer
              (niwa-collaboration--records-frame
               (list :t "snapshot-chunk") (car rest) (null (cdr rest)))))))


;;
;;; Remote interactions

(defvar niwa-collaboration--ui-request-counter 0
  "Monotonic id source for ui-request frames within this Emacs process.")

(defvar niwa-collaboration-remote-guest nil
  "Display name of the guest whose answer is being applied, or nil.
Bound around a ui-response handler so downstream effects -- such as a
plan revision request queued by remote feedback -- can attribute their
output to the answering guest.")

(defun niwa-collaboration--writable-peers (room)
  "Return the peer ids of ROOM's writable guests."
  (let (peers)
    (maphash (lambda (peer guest)
               (when (plist-get guest :writable)
                 (push peer peers)))
             (plist-get room :guests))
    (nreverse peers)))

(defun niwa-collaboration--audience-peer-p (room peer audience)
  "Return non-nil when PEER in ROOM belongs to AUDIENCE.

AUDIENCE narrows an interaction below the writable default.  `owner'
restricts it to owner-link guests.  A nil AUDIENCE is every writable
guest, the ordinary case."
  (let ((guest (niwa-collaboration--guest room peer)))
    (and guest
         (pcase audience
           ('nil t)
           ('owner (plist-get guest :owner))
           (_ nil)))))

(defun niwa-collaboration--audience-peers-for (room audience)
  "Return ROOM's writable peers that belong to AUDIENCE."
  (if (null audience)
      (niwa-collaboration--writable-peers room)
    (seq-filter (lambda (peer)
                  (niwa-collaboration--audience-peer-p
                   room peer audience))
                (niwa-collaboration--writable-peers room))))

(defun niwa-collaboration--audience-peers (room overlay)
  "Return ROOM's peers that may see OVERLAY's interaction."
  (niwa-collaboration--audience-peers-for
   room (plist-get (overlay-get overlay 'niwa--remote) :audience)))

(defun niwa-collaboration--ui-request-frame (request-id overlay)
  "Return the ui-request frame for OVERLAY under REQUEST-ID."
  (let ((remote (overlay-get overlay 'niwa--remote)))
    (append
     (list :t "ui-request"
           :reqId request-id
           :body (or (plist-get remote :body) "")
           :bodyKind (or (plist-get remote :body-kind) "text")
           :options
           (vconcat
            (cl-loop for (_outcome . label) in (plist-get remote :options)
                     for index from 0
                     collect `(("id" . ,index) ("label" . ,label))))
           :allowFeedback
           (if (plist-get remote :feedback) t :json-false))
     (when-let* ((notice (plist-get remote :notice)))
       (list :notice notice))
     (when (plist-get remote :options-owner-only)
       (list :optionsOwnerOnly t))
     (when-let* ((input (plist-get remote :option-input)))
       (list :optionInput input))
     ;; A cancel handler settles just this interaction -- for the Ask
     ;; questionnaire, the run continues -- so the guest gets a Dismiss.
     (when (plist-get remote :cancel)
       (list :allowCancel t))
     ;; A questionnaire travels structurally; the guest answers all
     ;; questions atomically through the :answers response field.
     (when-let* ((questions (plist-get remote :questions)))
       (list :questions (vconcat (funcall questions)))))))

(defun niwa-collaboration--on-prompt-created (overlay)
  "Present prompt OVERLAY to the active room's writable guests.

A re-render of the same interaction -- the permission queue redraws its
head on every selection change -- reuses the existing request id, so a
guest sees one card updated in place instead of an accumulating pile."
  (when-let* ((room (niwa-collaboration--room-for-overlay overlay))
              (remote (overlay-get overlay 'niwa--remote)))
    (when niwa-collaboration-remote-interactions
      (let* ((requests (plist-get room :ui-requests))
             (interaction-id
              (overlay-get overlay 'niwa-view-interaction-id))
             (existing-id
              (and interaction-id
                   (catch 'found
                     (maphash
                      (lambda (id tracked)
                        (when (and (overlayp tracked)
                                   (equal interaction-id
                                          (overlay-get
                                           tracked
                                           'niwa-view-interaction-id)))
                          (throw 'found id)))
                      requests)
                     nil)))
             (request-id
              (or existing-id
                  (cl-incf niwa-collaboration--ui-request-counter)))
             (peers (niwa-collaboration--audience-peers room overlay)))
        (puthash request-id overlay requests)
        (let ((frame (niwa-collaboration--ui-request-frame
                      request-id overlay)))
          (dolist (peer peers)
            (niwa-collaboration--transport-send
             (plist-get room :transport) peer frame)))
        (unless existing-id
          ;; Waking a guest for a decision it is not shown is noise, so
          ;; the push follows the same audience the frame did.
          (if (plist-get remote :audience)
              (niwa-collaboration--push-guests
               room (delq nil
                          (mapcar (lambda (peer)
                                    (plist-get
                                     (niwa-collaboration--guest room peer)
                                     :guest-id))
                                  peers)))
            (niwa-collaboration--push-writable-guests
             room (overlay-get overlay 'niwa-lobby-woken))))))))

(defun niwa-collaboration--on-prompt-settled (overlay)
  "Dismiss OVERLAY's ui-request from every guest surface.
Every room is searched rather than the overlay's buffer resolved: a
settled overlay may already be deleted, and a deleted overlay no longer
knows where it lived."
  (dolist (room (niwa-collaboration--room-list))
    (let ((requests (plist-get room :ui-requests)))
      (maphash
       (lambda (request-id tracked)
         (when (eq tracked overlay)
           (remhash request-id requests)
           (dolist (peer (niwa-collaboration--writable-peers room))
             (niwa-collaboration--transport-send
              (plist-get room :transport) peer
              (list :t "ui-request-end" :reqId request-id)))))
       requests))))

(defun niwa-collaboration--send-ui-requests (room peer)
  "Send ROOM's active ui-requests to writable guest PEER.

A narrowed interaction is withheld here as well as on creation: a guest
must not collect a decision it may not see by reconnecting."
  (let ((requests (plist-get room :ui-requests))
        ids)
    (maphash (lambda (request-id _overlay) (push request-id ids)) requests)
    (dolist (request-id (sort ids #'<))
      (let* ((overlay (gethash request-id requests))
             (audience (plist-get (overlay-get overlay 'niwa--remote)
                                  :audience)))
        (when (or (null audience)
                  (niwa-collaboration--audience-peer-p
                   room peer audience))
          (niwa-collaboration--send-guarded
           room peer
           (niwa-collaboration--ui-request-frame request-id overlay)))))))

(defun niwa-collaboration-retire-permission-requests (session entries)
  "Retire SESSION's expired permission ENTRIES from guest surfaces.
Remove registry entries before sending frames, without invoking any answer
callback.  Each affected guest receives one nonterminal batch notice."
  ;; Expiring a local queue must not load the optional sharing subsystem.
  (when (featurep 'niwa-collaboration)
    (dolist (room (niwa-collaboration--room-list))
      (when (eq session (plist-get room :session))
        (let ((requests (plist-get room :ui-requests))
              retired audience)
          (maphash
           (lambda (request-id overlay)
             (when (memq (overlay-get overlay 'niwa-view-interaction-entry)
                         entries)
               (let ((peers (niwa-collaboration--audience-peers room overlay)))
                 (push (cons request-id peers) retired)
                 (dolist (peer peers)
                   (cl-pushnew peer audience)))
               (remhash request-id requests)))
           requests)
          (dolist (request retired)
            (dolist (peer (cdr request))
              (niwa-collaboration--transport-send
               (plist-get room :transport) peer
               (list :t "ui-request-end" :reqId (car request)))))
          (dolist (peer audience)
            (niwa-collaboration--transport-send
             (plist-get room :transport) peer
             (list :t "notice" :message
                   (format "%d permission %s expired; nothing was approved."
                           (length entries)
                           (if (= 1 (length entries)) "request" "requests"))))))))))

(defun niwa-collaboration--questionnaire-answers (answers)
  "Normalize ANSWERS atomically within the shared guest input budget.
Each answer is text or a list of selected texts.  Return nil for invalid
input; the questionnaire handler validates the shape for each question."
  (when (and (listp answers) answers)
    (catch 'invalid
      (let ((bytes 0))
        (cl-labels
            ((text (value)
               (unless (and (stringp value)
                            (<= (string-bytes value)
                                niwa-collaboration--max-prompt-bytes))
                 (throw 'invalid nil))
               (let ((trimmed (string-trim value)))
                 (cl-incf bytes (string-bytes trimmed))
                 trimmed)))
          (let ((normalized
                 (mapcar (lambda (answer)
                           (cond
                            ((stringp answer) (text answer))
                            ((listp answer) (mapcar #'text answer))
                            (t (throw 'invalid nil))))
                         answers)))
            (when (<= bytes niwa-collaboration--max-prompt-bytes)
              normalized)))))))

(defun niwa-collaboration--handle-ui-response (room peer frame)
  "Settle the ui-request answered by writable guest PEER through FRAME.

The first answer -- from Emacs or any guest -- wins; the shared settle
already guards exactly-once, and a request no longer in the registry gets
a nonterminal notice.  A function option runs in the prompt's buffer so an
answer can execute the same path the host key binding would."
  (let ((guest (niwa-collaboration--guest room peer))
        (request-id (plist-get frame :reqId)))
    (when (and guest
               (plist-get guest :writable)
               niwa-collaboration-remote-interactions
               (integerp request-id))
      (unless (gethash request-id (plist-get room :ui-requests))
        (niwa-collaboration--transport-send
         (plist-get room :transport) peer
         (list :t "notice" :message
               "Request is no longer pending; nothing was approved.")))
      (when-let* ((overlay (gethash request-id (plist-get room :ui-requests)))
                  ;; Seeing a narrowed interaction and answering it are
                  ;; the same authority, so the audience is rechecked
                  ;; here: a request id is guessable, the audience is not.
                  ((niwa-collaboration--audience-peer-p
                    room peer
                    (plist-get (overlay-get overlay 'niwa--remote)
                               :audience))))
        (let* ((remote (overlay-get overlay 'niwa--remote))
               (options (plist-get remote :options))
               (feedback (plist-get frame :feedback))
               (option (plist-get frame :option))
               (answers (plist-get frame :answers))
               (feedback-handler (plist-get remote :feedback))
               (answer-handler (plist-get remote :answer))
               (cancel-handler (plist-get remote :cancel))
               (outcome
                (cond
                 ;; A cancel settles just this interaction through the
                 ;; handler the prompt offered; nothing else is touched.
                 ((and (eq (plist-get frame :cancel) t)
                       (functionp cancel-handler))
                  cancel-handler)
                 ;; Single answers are strings; multi-select answers are
                 ;; string lists.  Blank strings/empty lists mean no preference.
                 ((and answers
                       (functionp answer-handler)
                       (when-let* ((normalized
                                    (niwa-collaboration--questionnaire-answers
                                     answers)))
                         (lambda () (funcall answer-handler normalized)))))
                 ((and feedback-handler
                       (niwa-collaboration--guest-text feedback))
                  ;; A function handler owns the whole feedback flow, for
                  ;; prompts whose feedback is not a plain settle outcome.
                  (let ((text (niwa-collaboration--guest-text feedback)))
                    (if (functionp feedback-handler)
                        (lambda () (funcall feedback-handler text))
                      (cons 'feedback text))))
                 ((and (natnump option) (nth option options)
                       (or (not (plist-get remote :options-owner-only))
                           (plist-get guest :owner)))
                  (let ((handler (car (nth option options))))
                    (if (plist-get remote :option-input)
                        (let ((value (plist-get frame :value)))
                          (unless (and (stringp value)
                                       (<= (string-bytes value)
                                           niwa-collaboration--max-prompt-bytes))
                            (user-error "Option input must be bounded text"))
                          (lambda () (funcall handler (string-trim value))))
                      handler))))))
          (when (and outcome (buffer-live-p (overlay-buffer overlay)))
            (message "niwa: interaction answered by guest %s"
                     (plist-get guest :name))
            (with-current-buffer (overlay-buffer overlay)
              (let ((niwa-collaboration-remote-guest
                     (plist-get guest :name)))
                ;; The frame boundary reports failures to the answering peer,
                ;; with owner-only diagnostics, like every other guest action.
                (if (functionp outcome)
                    (funcall outcome)
                  (niwa--prompt--settle overlay outcome))))))))))


;;
;;; Inbound guest frames

(defun niwa-collaboration--admit-hello (room peer frame)
  "Register PEER in ROOM from its hello FRAME and return its guest plist.
A protocol mismatch is answered with an error frame and returns nil.
Authority comes only from the tokens FRAME proves it holds."
  (let ((proto (plist-get frame :proto)))
    (if (not (equal proto niwa-collaboration--protocol-version))
        (progn
          (niwa-collaboration--transport-send
           (plist-get room :transport) peer (niwa-collaboration--protocol-error))
          nil)
      (let* ((name (niwa-collaboration--sanitize-guest-name
                    (plist-get frame :name)))
             (claimed (niwa-collaboration--base64url-decode
                       (plist-get frame :writeToken)))
             (writable (and claimed
                            (equal claimed (plist-get room :write-token))))
             (claimed-owner (niwa-collaboration--base64url-decode
                             (plist-get frame :ownerToken)))
             ;; Owner authority is never granted on its own: the owner
             ;; link contains the write token, so a peer claiming one
             ;; without the other is a forgery attempt, not a tier.
             (owner (and writable claimed-owner
                         (equal claimed-owner (plist-get room :owner-token))))
             (guest (append
                     (list :name name :writable writable :owner owner
                           :ready t
                           :guest-id (niwa-collaboration--sanitize-guest-id
                                      (plist-get frame :guestId)))
                     (condition-case nil
                         (niwa-collaboration-presence-state frame)
                       (error nil)))))
        (puthash peer guest (plist-get room :guests))
        (niwa-sleep-update)
        guest))))

(defun niwa-collaboration--handle-hello (room peer frame)
  "Register guest PEER from its hello FRAME and send the snapshot.
Each step stands alone: a frame too large to send costs the guest only
that step."
  (when-let* ((guest (niwa-collaboration--admit-hello room peer frame)))
    (let ((transport (plist-get room :transport)))
      (dolist (step
               (list
                (lambda () (niwa-collaboration--send-snapshot room peer))
                (lambda () (niwa-collaboration--publish-history room peer))
                ;; Queue and busy state travel only on change, so a joining
                ;; guest is told the current ones directly.
                (lambda () (niwa-collaboration--send-queue-state room peer guest t))
                (lambda ()
                  (niwa-collaboration--transport-send
                   transport peer (niwa-collaboration--status-frame room)))
                ;; The roster broadcast is latched on change, so a joining
                ;; guest is told the current one directly -- an empty roster
                ;; included, because a reconnecting viewer must clear stale rows.
                (lambda ()
                  (niwa-collaboration--transport-send
                   transport peer (niwa-collaboration--agents-frame room)))
                (lambda ()
                  (niwa-collaboration--transport-send
                   transport peer (niwa-collaboration--background-frame room)))
                ;; The task list is latched the same way.
                (lambda ()
                  (niwa-collaboration--transport-send
                   transport peer (niwa-collaboration--tasks-frame room)))
                (lambda ()
                  (niwa-collaboration--transport-send
                   transport peer (niwa-collaboration--goal-frame room)))
                ;; The store's artifacts attached to the session join the
                ;; sidebar.  A store fault costs the guest only this listing.
                (lambda ()
                  (condition-case err
                      (niwa-collaboration--transport-send
                       transport peer (niwa-collaboration--store-frame room))
                    (error (niwa-diagnostic-message
                            (niwa-diagnostic-scope
                             (niwa-collaboration--room-workspace room))
                            "Artifact listing failed: %s"
                            (error-message-string err)))))
                (lambda ()
                  (when (and (plist-get guest :writable)
                             niwa-collaboration-remote-interactions)
                    (niwa-collaboration--send-ui-requests room peer)))
                (lambda () (niwa-collaboration-presence-publish room))
                ;; Last, so an optional recovery fault cannot cost the owner
                ;; the frames above.
                (lambda ()
                  (when (plist-get guest :owner)
                    (niwa-collaboration-recovery-send room peer)))))
        (niwa-collaboration--skip-oversize room step)))))

(defconst niwa-collaboration--max-push-endpoint-bytes 2048
  "Maximum encoded bytes accepted for a browser push endpoint.")

(defun niwa-collaboration--push-endpoint-p (value)
  "Return non-nil when VALUE is a bounded HTTPS push endpoint."
  (and (stringp value)
       (<= (string-bytes value)
           niwa-collaboration--max-push-endpoint-bytes)
       (string-match-p "\\`https://[^[:space:][:cntrl:]#]+\\'" value)))

(defun niwa-collaboration--handle-push-subscription (room peer frame &optional persist)
  "Forward authenticated PEER's push subscription FRAME for ROOM.
Before a mutation, call PERSIST with the guest id and new endpoint
\(nil for removal).  A failed write must not change the live subscription."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((plist-get guest :ready))
              (guest-id (plist-get guest :guest-id)))
    (pcase (plist-get frame :t)
      ("push-subscribe"
       (when-let* ((endpoint (plist-get frame :endpoint))
                   ((niwa-collaboration--push-endpoint-p endpoint)))
         (when persist (funcall persist guest-id endpoint))
         (puthash guest-id
                  (list :endpoint endpoint
                        :writable (plist-get guest :writable))
                  (plist-get room :push-guests))
         (niwa-collaboration--transport-control
          (plist-get room :transport)
          (list :t "push-subscribe" :peer peer :guestId guest-id
                :endpoint endpoint
                :active (if (eq t (plist-get frame :active))
                            t :json-false)))))
      ("push-unsubscribe"
       (when persist (funcall persist guest-id nil))
       (remhash guest-id (plist-get room :push-guests))
       (niwa-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-unsubscribe" :guestId guest-id)))
      ("push-state"
       (niwa-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-state" :peer peer :guestId guest-id
              :active (if (eq t (plist-get frame :active))
                          t :json-false)))))))

(defun niwa-collaboration--push-guests (room guest-ids)
  "Ask ROOM's relay to wake subscribed GUEST-IDS."
  (when guest-ids
    (niwa-collaboration--transport-control
     (plist-get room :transport)
     (list :t "push" :guestIds (vconcat guest-ids)))))

(defun niwa-collaboration--push-writable-guests (room &optional already-woken)
  "Wake writable subscribers of ROOM not in ALREADY-WOKEN's ids or endpoints."
  (let (guest-ids)
    (maphash (lambda (guest-id subscription)
               (when (and (plist-get subscription :writable)
                          (not (member guest-id already-woken))
                          (not (member (plist-get subscription :endpoint) already-woken)))
                 (push guest-id guest-ids)))
             (plist-get room :push-guests))
    (niwa-collaboration--push-guests room guest-ids)))

(defun niwa-collaboration--push-audience (owner room kind)
  "Wake subscribers to ROOM and the lobby of session or workspace OWNER for KIND.
`all' means user-turn completion; other kinds target writable devices.
Return the lobby guest ids and endpoints selected for a wake."
  (let* ((workspace (if (niwa-session-p owner) (niwa-session-workspace owner) owner))
         (lobby (and workspace (fboundp 'niwa-collaboration-lobby--find)
                     (niwa-collaboration-lobby--find workspace)))
         (all (eq kind 'all))
         (room-sent
          (and room (if all
                        (niwa-collaboration--transport-control
                         (plist-get room :transport) (list :t "push"))
                      (niwa-collaboration--push-writable-guests room))))
         seen woken)
    (when lobby
      ;; A browser has one guest id but a distinct Push endpoint per room.
      (when-let* (room-sent (subs (plist-get room :push-guests)))
        (maphash (lambda (id sub)
                   (when (or all (plist-get sub :writable))
                     (push id seen)
                     (when-let* ((endpoint (plist-get sub :endpoint)))
                       (push endpoint seen)))) subs))
      (let (ids)
        (when-let* ((subs (plist-get lobby :push-guests)))
          (maphash (lambda (id sub)
                     (unless (or (member id seen) (member (plist-get sub :endpoint) seen))
                       (push id ids)
                       (push id woken)
                       (when-let* ((endpoint (plist-get sub :endpoint)))
                         (push endpoint woken)))) subs))
        (unless (niwa-collaboration--push-guests lobby ids)
          (setq woken nil)))
      (maphash (lambda (peer guest)
                 (when (and (plist-get guest :ready) (plist-get guest :writable)
                            (not (member (plist-get guest :guest-id) seen)))
                   (niwa-collaboration--transport-send
                    (plist-get lobby :transport) peer
                    (list :t "notice" :kind (if all "completion" (symbol-name kind))))))
               (plist-get lobby :guests)))
    woken))

(defun niwa-collaboration-push-notice (owner kind)
  "For KIND, wake writable subscribers of the session or workspace OWNER.
Permission cards reach room guests on display, not on admission."
  (let* ((room (and (niwa-session-p owner) (not (eq kind 'permission))
                    (niwa-collaboration--room-for-session owner)))
         (woken (niwa-collaboration--push-audience owner room kind)))
    (when room
      (maphash (lambda (peer guest)
                 (when (and (plist-get guest :ready) (plist-get guest :writable))
                   (niwa-collaboration--transport-send
                    (plist-get room :transport) peer
                    (list :t "notice" :kind (symbol-name kind)))))
               (plist-get room :guests)))
    woken))

(defun niwa-collaboration--restore-push-subscriptions (room)
  "Restore ROOM's Web Push endpoints after a relay reconnect."
  (when-let* ((subscriptions (plist-get room :push-guests)))
    (maphash
     (lambda (guest-id subscription)
       (niwa-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-subscribe" :peer 0 :guestId guest-id
              :endpoint (plist-get subscription :endpoint)
              :active :json-false)))
     subscriptions)))

(cl-defun niwa-collaboration--handle-prompt (room peer frame)
  "Queue the prompt in FRAME from writable guest PEER as a follow-up.

The prompt enters the ordinary pending-input queue: delivered when the
session is idle, queued behind a running request, paused while the
Pending Inputs cockpit is open.  The guest name is attribution only and
never enters model-visible context.

FRAME may carry an `:invoke\=' naming an allowlisted command or skill,
in which case the text is that invocation\='s arguments rather than a
prompt.  The name travels as its own field and is validated here: guest
text is never scanned for a sigil, so a pasted log line cannot invoke
anything.  `:skills' carries explicitly selected names applied together
with literal text, mutually exclusive with `:invoke'."
  (let* ((guest (niwa-collaboration--guest room peer))
         (role (niwa-collaboration--guest-role guest))
         (invoke (plist-get frame :invoke))
         (skills (plist-get frame :skills))
         (text (plist-get frame :text)))
    (when (and invoke
               (not (niwa-collaboration--guest-invocable-p invoke role)))
      (cl-return-from niwa-collaboration--handle-prompt))
    (when (or (and invoke skills)
              (and (plist-member frame :skills)
                   (not (niwa-collaboration--guest-skills-admitted-p
                         skills role (plist-get room :session)))))
      (cl-return-from niwa-collaboration--handle-prompt))
    ;; A bare invocation of a command that would otherwise prompt on the
    ;; host gets the argument the host would have picked first.
    (when (and invoke (or (null text) (string-blank-p text)))
      (setq text (or (cdr (assoc invoke
                                 niwa-collaboration--invocation-defaults))
                     text)))
    (when (and guest
               (plist-get guest :writable)
               ;; An invocation may carry no arguments at all; a plain
               ;; prompt still has to say something.
               (or (and (or invoke skills) (or (null text) (stringp text)))
                   (niwa-collaboration--guest-text text)))
      ;; The prompt frame may carry a fresher display name than the hello
      ;; did; the badge should show what the guest typed.
      (when (stringp (plist-get frame :name))
        (plist-put guest :name (niwa-collaboration--sanitize-guest-name
                                (plist-get frame :name))))
      ;; Drop a byte-identical repeat inside the duplicate window: a
      ;; double-fired client submit, not a second question.  Prompts
      ;; carrying attachments are never deduplicated -- consecutive
      ;; sends legitimately reuse the same placeholder text.
      (let ((last (plist-get guest :last-prompt))
            ;; An invocation and a prompt with the same text are
            ;; different sends, so the latch keys on both.
            (dedup-key (list invoke skills text))
            (now (float-time)))
        (when (and last
                   (not (plist-get frame :images))
                   (equal (car last) dedup-key)
                   (< (- now (cdr last))
                      niwa-collaboration--duplicate-prompt-window))
          (cl-return-from niwa-collaboration--handle-prompt))
        (let* ((data-buffer (niwa-collaboration--room-data-buffer room))
               (view-buffer (and data-buffer
                                 (buffer-local-value 'niwa--view-buffer
                                                     data-buffer))))
          (when (buffer-live-p view-buffer)
            ;; Attachments ride the same pipeline as clipboard images
            ;; pasted in Emacs: saved under the session media directory,
            ;; then mentioned and read-granted by the queue seam.  Read
            ;; decides text or media from the extension, so nothing here
            ;; has to.
            ;; Both are durable, and the queue seam still refuses a prompt
            ;; whose session view is not live, so neither outlives a prompt
            ;; that was not queued.
            (let ((paths
                   (condition-case err
                       (with-current-buffer view-buffer
                         (niwa-collaboration--save-guest-attachments
                          (plist-get frame :images)))
                     (error
                      ;; A failed media write is this prompt's problem, not
                      ;; the room's: letting it reach the frame handler
                      ;; tears the session down for every guest.
                      (niwa-diagnostic-warning
                       (niwa-diagnostic-scope (niwa-collaboration--room-workspace room))
                       'niwa
                       (format "Guest attachment could not be saved: %s"
                               (error-message-string err))
                       :warning)
                      (cl-return-from
                          niwa-collaboration--handle-prompt))))
                  (queued nil))
              ;; Latch before the enqueue, which redraws and can therefore
              ;; re-enter, and give the latch back when nothing was queued.
              (plist-put guest :last-prompt (cons dedup-key now))
              (unwind-protect
                  (progn
                    (setq queued
                          (niwa-view-enqueue-external-follow-up
                           data-buffer (or text "")
                           :guest-name (plist-get guest :name)
                           :guest-id (plist-get guest :guest-id)
                           :paths paths
                           :invoke invoke
                           :skills skills
                           :guest-role role))
                    (when queued
                      (niwa-collaboration--transport-send
                       (plist-get room :transport) peer
                       (append
                        (list :t "queued")
                        (when-let* ((id (plist-get queued :id)))
                          (list :id id))
                        (when-let* ((position
                                     (niwa-collaboration--queue-position
                                      room queued)))
                          (list :position position))))))
                (unless queued
                  (plist-put guest :last-prompt nil)
                  (dolist (path paths)
                    (when (file-exists-p path)
                      (ignore-errors (delete-file path)))))))))))))


(defun niwa-collaboration--utf8-text-p (bytes)
  "Return non-nil when unibyte BYTES are UTF-8 text without NUL bytes."
  (and (not (string-search "\0" bytes))
       (not (string-match-p "[\200-\377]"
                            (decode-coding-string bytes 'utf-8 t)))))

(defun niwa-collaboration--attachment-extension (image bytes)
  "Return the extension guest attachment IMAGE with BYTES is saved under.
A typed attachment takes its type's extension.  A plain-text one -- a
source or markup file -- keeps its own extension when that is short and
not one Read treats as binary, else `txt', and must be UTF-8 text.
Return nil for anything else."
  (let ((mime (plist-get image :mime))
        (own (plist-get image :extension)))
    (if (and (equal mime "text/plain") own)
        (and (niwa-collaboration--utf8-text-p bytes)
             (if (and (stringp own)
                      (string-match-p "\\`[a-z0-9]\\{1,12\\}\\'" own)
                      (not (niwa-tool-fs-read--binary-extension-p
                            (concat "attachment." own))))
                 own
               "txt"))
      (cdr (assoc mime niwa-collaboration--attachment-extensions)))))

(defun niwa-collaboration--save-guest-attachments (images)
  "Save valid guest attachments IMAGES under the session media directory.
IMAGES is the decoded frame list of (:mime STRING :data BASE64) plists;
a plain-text one may name its own `:extension'.  Return the saved
absolute paths.  Runs in the view buffer.  Anything invalid -- unknown
type, undecodable data, non-text bytes, or a set over the byte budget --
drops the whole set rather than attaching a partial one."
  (when (and images (listp images)
             (<= (length images)
                 niwa-collaboration--max-prompt-attachments))
    (catch 'invalid
      (let ((total 0)
            (decoded nil))
        (dolist (image images)
          (let* ((bytes (and (stringp (plist-get image :data))
                             (condition-case nil
                                 (base64-decode-string
                                  (plist-get image :data))
                               (error nil))))
                 (extension (and bytes (niwa-collaboration--attachment-extension
                                        image bytes))))
            (unless (and extension (> (length bytes) 0))
              (throw 'invalid nil))
            (cl-incf total (length bytes))
            (when (> total niwa-collaboration--max-attachment-bytes)
              (throw 'invalid nil))
            (push (cons extension bytes) decoded)))
        (let ((dir (niwa-view--media-dir))
              (stamp (format-time-string "%Y%m%d-%H%M%S"))
              (n 0)
              (complete nil)
              paths)
          (unwind-protect
              (progn
                (dolist (entry (nreverse decoded))
                  (let ((path nil))
                    ;; `excl' makes the name its own claim: testing first and
                    ;; writing after leaves a window another writer can take,
                    ;; and remote media I/O can yield inside it.
                    (while (null path)
                      (let ((candidate
                             (file-name-concat
                              dir (format "guest-%s-%d.%s" stamp
                                          (cl-incf n) (car entry))))
                            (coding-system-for-write 'binary))
                        (condition-case nil
                            (progn
                              (write-region (cdr entry) nil candidate nil
                                            'silent nil 'excl)
                              (setq path candidate))
                          (file-already-exists nil))))
                    (push path paths)))
                (setq complete t)
                (nreverse paths))
            ;; A set is attached whole or not at all, so a set that failed
            ;; part way through takes its own files with it.
            (unless complete
              (dolist (path paths)
                (when (file-exists-p path)
                  (ignore-errors (delete-file path)))))))))))

(defun niwa-collaboration--save-guest-files (data-buffer images)
  "Save guest attachment IMAGES for DATA-BUFFER's session; return their paths.
IMAGES is a frame list or vector of (:mime STRING :data BASE64) plists.
Return nil when there are none, and signal when a set cannot be attached
whole, so a question never queues without the files its sender chose.
The caller deletes the paths when its own enqueue fails."
  (when (and images (> (length images) 0))
    (let ((view (and (buffer-live-p data-buffer)
                     (buffer-local-value 'niwa--view-buffer data-buffer))))
      (or (and (buffer-live-p view)
               (with-current-buffer view
                 (niwa-collaboration--save-guest-attachments
                  (append images nil))))
          (error "The attachments could not be attached; check their type and size")))))

(defun niwa-collaboration--handle-retract (room peer frame)
  "Remove the pending entry FRAME names when guest PEER queued it.

Authority is per entry: the id must belong to an entry this guest's
stable id queued, so no guest can delete another guest's or the host's
pending input.  The entry's attachment files leave with it, mirroring
the failed-enqueue cleanup."
  (let ((guest (niwa-collaboration--guest room peer))
        (id (plist-get frame :id)))
    (when (and guest
               (plist-get guest :writable)
               (plist-get guest :guest-id)
               (integerp id))
      (when-let* ((session (plist-get room :session))
                  (entries (niwa-session-pending-follow-ups session))
                  (entry (cl-find-if
                          (lambda (candidate)
                            (and (equal id (plist-get candidate :id))
                                 (equal (plist-get guest :guest-id)
                                        (plist-get candidate :guest-id))
                                 ;; The drain is delivering it: the files
                                 ;; are about to be read mid-turn, so it
                                 ;; is no longer the guest's to take back.
                                 (not (plist-get candidate :delivering))))
                          entries)))
        (niwa-pending-inputs--set-queues
         session 'follow-up (remq entry entries))
        (dolist (path (plist-get entry :guest-paths))
          (when (file-exists-p path)
            (ignore-errors (delete-file path))))
        (when-let* ((data-buffer (niwa-collaboration--room-data-buffer
                                  room))
                    (view-buffer (buffer-local-value 'niwa--view-buffer
                                                     data-buffer))
                    ((buffer-live-p view-buffer)))
          (with-current-buffer view-buffer
            (niwa-view--interaction-rebuild)))
        (niwa-collaboration--publish-queue room)))))

(defun niwa-collaboration--handle-abort (room peer)
  "Abort the running request for writable guest PEER."
  (let ((guest (niwa-collaboration--guest room peer)))
    (when (and guest (plist-get guest :writable))
      (when-let* ((data-buffer (niwa-collaboration--room-data-buffer room)))
        (let ((view-buffer
               (buffer-local-value 'niwa--view-buffer data-buffer)))
          (if (buffer-live-p view-buffer)
              (with-current-buffer view-buffer
                (niwa-view-abort))
            (niwa-view--abort-data-buffer data-buffer)))))))

(defun niwa-collaboration--on-frame (data-buffer peer frame)
  "Dispatch decoded guest FRAME from PEER for DATA-BUFFER's room."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (niwa-collaboration--guard-guest-frame
     room peer frame #'niwa-collaboration--dispatch-frame)))

(defun niwa-collaboration--guard-guest-frame (room peer frame dispatch)
  "Call DISPATCH with ROOM, PEER and FRAME, answering its failure to PEER.
ROOM is a session room or a lobby.

Failure isolation mirrors the gptel observers: a fault in guest input
handling preserves the room, warns the host with the error and tells only
the sending guest.  A `user-error' tells an owner-link guest its text and
anyone else a generic sentence.  Guests act while nobody may be at the
keyboard, so frames run with `inhibit-interaction': a step that would ask
in Emacs is refused to the sending guest instead of waiting for an
answer, and only the host's warning names the question."
  (condition-case fault
      (condition-case err
          (let ((inhibit-interaction t))
            (funcall dispatch room peer frame))
        (user-error
         (niwa-collaboration--transport-send
          (plist-get room :transport) peer
          (list :t "notice" :message
                (if (niwa-collaboration--owner room peer)
                    (error-message-string err)
                  "The host could not complete this action; the owner can review session status"))))
        (inhibited-interaction
         (niwa-collaboration--transport-send
          (plist-get room :transport) peer
          (list :t "notice" :message niwa-collaboration-needs-host-message))
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope (niwa-collaboration--room-workspace room)) 'niwa
          (format "A guest's %s request needed a decision in Emacs and was refused%s"
                  (plist-get frame :t) (niwa-view--refused-question err)))))
    (error
     (condition-case nil
         (progn
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope (niwa-collaboration--room-workspace room))
            'niwa (format "A guest's %s frame failed: %s"
                             (plist-get frame :t) (error-message-string fault))
            :warning)
           (niwa-collaboration--transport-send
            (plist-get room :transport) peer
            (list :t "notice" :message "The host could not handle this action")))
       (error nil)))))

(defun niwa-collaboration--dispatch-shared-frame (room peer frame root)
  "Handle guest FRAME from PEER if its type is one rooms and lobbies share.
ROOM is a session room or a lobby; ROOT is the workspace root that file
uploads land in.  Return `handled' or `unhandled', never a handler's
value, so each kind dispatches its own types first and falls through."
  (pcase (plist-get frame :t)
    ("viewing" (niwa-collaboration-presence-handle room peer frame) 'handled)
    ("new-session"
     (niwa-collaboration--handle-new-session room peer frame) 'handled)
    ("session-models"
     (niwa-collaboration--handle-session-models room peer) 'handled)
    ("file-upload"
     (niwa-collaboration-files-handle-upload room peer frame root) 'handled)
    ;; The workspace's artifacts, reached by store identity.
    ("artifact-get"
     (niwa-collaboration--handle-artifact-get room peer frame) 'handled)
    ("artifact-delete"
     (niwa-collaboration--handle-artifact-delete room peer frame) 'handled)
    ("discussion"
     (niwa-collaboration-discussions-handle room peer frame) 'handled)
    ("store-list"
     (niwa-collaboration--handle-store-list room peer frame) 'handled)
    ("store-action"
     (niwa-collaboration--handle-store-action room peer frame) 'handled)
    (_ 'unhandled)))

(defun niwa-collaboration--dispatch-frame (room peer frame)
  "Handle guest FRAME from PEER in session ROOM.
Types a lobby shares fall through to
`niwa-collaboration--dispatch-shared-frame', whose status this
returns; for the room's own types the value is unspecified."
  (pcase (plist-get frame :t)
    ("recovery" (niwa-collaboration-recovery-handle room peer frame))
    ("hello" (niwa-collaboration--handle-hello room peer frame))
    ("set-name"
     (when-let* ((guest (niwa-collaboration--guest room peer))
                 ((stringp (plist-get frame :name))))
       (plist-put guest :name
                  (niwa-collaboration--sanitize-guest-name
                   (plist-get frame :name)))
       (niwa-collaboration-presence-publish room)))
    ((or "push-subscribe" "push-unsubscribe" "push-state")
     (niwa-collaboration--handle-push-subscription
      room peer frame))
    ("prompt" (niwa-collaboration--handle-prompt room peer frame))
    ("abort" (niwa-collaboration--handle-abort room peer))
    ("background-stop"
     (niwa-collaboration--handle-background-stop room peer frame))
    ("background-locate"
     (niwa-collaboration--handle-background-locate room peer frame))
    ("fetch-agent"
     (niwa-collaboration--handle-fetch-agent room peer frame))
    ("execution-result-get"
     (niwa-collaboration--handle-execution-result-get room peer frame))
    ("history-get"
     (niwa-collaboration--handle-history-get room peer frame))
    ((or "editing" "editing-presence")
     (niwa-collaboration-editing-handle room peer frame))
    ("retract" (niwa-collaboration--handle-retract room peer frame))
    ("ui-response"
     (niwa-collaboration--handle-ui-response room peer frame))
    ("set-mode"
     (niwa-collaboration--handle-set-mode room peer frame))
    ("fork" (niwa-collaboration--handle-fork room peer frame))
    ((or "rewind" "retry-turn")
     (niwa-collaboration--handle-history-action room peer frame))
    (_
     (niwa-collaboration--dispatch-shared-frame
      room peer frame
      (when-let* ((workspace (niwa-collaboration--room-workspace room)))
        (niwa-workspace-root workspace))))))

(defun niwa-collaboration--on-control (data-buffer event peer)
  "Handle relay control EVENT for PEER in DATA-BUFFER's room."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (pcase event
      ;; A joined peer becomes a guest only through its hello frame.
      ('peer-joined nil)
      ('peer-left
       (niwa-collaboration-editing-depart room peer)
       (when (gethash peer (plist-get room :guests))
         (plist-put room :last-guest-departure (float-time)))
       (remhash peer (plist-get room :guests))
       ;; Fork request ids are scoped to the admitted connection.
       (when-let* ((ledgers (plist-get room :fork-ledgers)))
         (remhash peer ledgers))
       (niwa-collaboration-presence-publish room)))))

(defun niwa-collaboration--on-state (data-buffer state)
  "Track relay transport STATE for DATA-BUFFER's room."
  (when-let* ((room (niwa-collaboration--room-for-buffer data-buffer)))
    (pcase state
      ;; The relay garbage-collects the room with the host connection, so
      ;; a drop invalidated every guest; they rejoin and re-hello against
      ;; the re-created room.
      ('down
       (when (> (hash-table-count (plist-get room :guests)) 0)
         (plist-put room :last-guest-departure (float-time)))
       (maphash (lambda (peer _guest) (niwa-collaboration-editing-depart room peer))
                (plist-get room :guests))
       (clrhash (plist-get room :guests))
       (when-let* ((ledgers (plist-get room :fork-ledgers)))
         (clrhash ledgers))
       (niwa-collaboration-presence-publish room)
       ;; The links and QR are handed out before the async dial settles.
       ;; A dial that has never succeeded -- wrong relay URL or a missing or
       ;; stale configured host token -- would otherwise retry forever
       ;; with the user none the wiser that the share is dead.
       (unless (or (plist-get room :was-open)
                   (plist-get room :dial-warned))
         (setq room (plist-put room :dial-warned t))
         (niwa-diagnostic-warning
          (niwa-diagnostic-scope (niwa-collaboration--room-workspace room)) 'niwa
          (concat "Collaboration relay dial failing; the share is not "
                  "live. Check `niwa-collaboration-relay-url' and "
                  "the relay's optional host-token configuration.")
          :warning)))
      ('open
       (setq room (plist-put room :was-open t))
       (niwa-collaboration--restore-push-subscriptions room))
      ('stopped nil))))

;; The dispatch above is the cold feature boundary for the two read-only
;; guest extensions.  Neither extension requires this module back.
(require 'niwa-collaboration-agent)
(require 'niwa-collaboration-artifact)
(require 'niwa-collaboration-discussions)
(require 'niwa-collaboration-editing)
(require 'niwa-collaboration-files)
(require 'niwa-collaboration-presence)

(provide 'niwa-collaboration-guest)
;;; niwa-collaboration-guest.el ends here
