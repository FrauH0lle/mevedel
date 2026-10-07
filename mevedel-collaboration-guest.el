;;; mevedel-collaboration-guest.el --- collaboration guest protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Handles untrusted guest protocol frames for a live collaboration room:
;; authentication, Web Push registration, prompt and attachment intake,
;; retraction, abort, and relay peer lifecycle.  Room ownership, projection,
;; outbound publication, and public commands remain in the collaboration
;; facade.

;;; Code:


(autoload 'mevedel-collaboration-recovery-handle "mevedel-collaboration-recovery")
(autoload 'mevedel-collaboration-recovery-send "mevedel-collaboration-recovery")
(declare-function mevedel-collaboration-recovery-handle "mevedel-collaboration-recovery" (room peer frame))
(declare-function mevedel-collaboration-recovery-send "mevedel-collaboration-recovery" (room peer &optional backend))

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `mevedel-collaboration'
;; `mevedel-tool-fs-read'
(declare-function mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read" (filename))
(autoload 'mevedel-tool-fs-read--binary-extension-p "mevedel-tool-fs-read")

(declare-function mevedel-collaboration--base64url-decode
                  "mevedel-collaboration" (string))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-queue
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish-status
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--queue-position
                  "mevedel-collaboration" (room entry))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-buffer
                  "mevedel-collaboration" (data-buffer))
(declare-function mevedel-collaboration--room-for-overlay
                  "mevedel-collaboration" (overlay))
(declare-function mevedel-collaboration--room-list
                  "mevedel-collaboration" ())
(declare-function mevedel-collaboration--sanitize-guest-id
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--sanitize-guest-name
                  "mevedel-collaboration" (name))
(declare-function mevedel-collaboration--send-queue-state
                  "mevedel-collaboration" (room peer guest &optional force))
(declare-function mevedel-collaboration--status-frame
                  "mevedel-collaboration" (room))
(defvar mevedel-collaboration--attachment-extensions)
(defvar mevedel-collaboration--duplicate-prompt-window)
(defvar mevedel-collaboration--max-attachment-bytes)
(defvar mevedel-collaboration--max-prompt-attachments)
(defvar mevedel-collaboration--max-prompt-bytes)
(defvar mevedel-collaboration-guest-skills)
(defvar mevedel-collaboration-remote-interactions)
(defvar mevedel-collaboration-unsafe-guest-commands)

(defvar mevedel-collaboration-needs-host-message)

;; `mevedel-collaboration-agent'
(declare-function mevedel-collaboration--agents-frame
                  "mevedel-collaboration-agent" (room))
(declare-function mevedel-collaboration--handle-fetch-agent
                  "mevedel-collaboration-agent" (room peer frame))
(declare-function mevedel-collaboration--handle-execution-result-get
                  "mevedel-collaboration-agent" (room peer frame))

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration--handle-artifact-comment
                  "mevedel-collaboration-artifact-comments" (room peer frame))
(declare-function mevedel-collaboration--handle-artifact-get
                  "mevedel-collaboration-artifact" (room peer frame))
(declare-function mevedel-collaboration--handle-artifact-delete
                  "mevedel-collaboration-artifact" (room peer frame))

;; `mevedel-collaboration-files'
(declare-function mevedel-collaboration-files-handle-upload
                  "mevedel-collaboration-files" (owner peer frame root))

;; `mevedel-collaboration-history'
(declare-function mevedel-collaboration--handle-history-get
                  "mevedel-collaboration-history" (room peer frame))
(declare-function mevedel-collaboration--publish-history
                  "mevedel-collaboration-history" (room &optional peer))
(autoload 'mevedel-collaboration--handle-history-get
  "mevedel-collaboration-history")
(autoload 'mevedel-collaboration--publish-history
  "mevedel-collaboration-history")

;; `mevedel-collaboration-owner'
(declare-function mevedel-collaboration--owner "mevedel-collaboration-owner" (room peer))
(declare-function mevedel-collaboration--handle-new-session
                  "mevedel-collaboration-owner" (room peer frame))
(declare-function mevedel-collaboration--handle-set-mode
                  "mevedel-collaboration-owner" (room peer frame))
(autoload 'mevedel-collaboration--handle-new-session
  "mevedel-collaboration-owner")
(autoload 'mevedel-collaboration--handle-set-mode
  "mevedel-collaboration-owner")

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
(defvar mevedel-collaboration--protocol-version)

;; `mevedel-collaboration-task'
(declare-function mevedel-collaboration--tasks-frame
                  "mevedel-collaboration-task" (room))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-control
                  "mevedel-collaboration-transport" (transport control))
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(defvar mevedel-collaboration--max-frame-json-bytes)

;; `mevedel-interaction-prompt'
(declare-function mevedel--prompt--settle
                  "mevedel-interaction-prompt" (overlay outcome))
(declare-function mevedel--prompt-user-with-overlay
                  "mevedel-interaction-prompt"
                  (title content question help-echo-text callback
                         &optional host-only audience))

;; `mevedel-models'
(declare-function mevedel-model-candidates "mevedel-models" ())
(autoload 'mevedel-model-candidates "mevedel-models")

;; `mevedel-pending-inputs'
(declare-function mevedel-pending-inputs--set-queues
                  "mevedel-pending-inputs" (session &rest replacements))
(declare-function mevedel-view--refused-question "mevedel-pending-inputs" (err))
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs"
                  (data-buffer text &rest keys))
(autoload 'mevedel-view--refused-question "mevedel-pending-inputs")
(autoload 'mevedel-view-enqueue-external-follow-up "mevedel-pending-inputs")

;; `mevedel-skills-core'
(declare-function mevedel-skill-argument-hint
                  "mevedel-skills-core" (skill))
(declare-function mevedel-skill-name "mevedel-skills-core" (skill))

;; `mevedel-skills-ui'
(declare-function mevedel-skills-user-visible-skills
                  "mevedel-skills-ui" (session &optional inline-only))
(autoload 'mevedel-skills-user-visible-skills "mevedel-skills-ui")
(defvar mevedel-slash-commands)

;; `mevedel-structs'
(declare-function mevedel-directive-id "mevedel-structs" (record))
(declare-function mevedel-session-pending-follow-ups
                  "mevedel-structs" (session))
(declare-function mevedel-session-set-pending-inputs
                  "mevedel-structs" (session category entries))
(declare-function mevedel-session-workspace "mevedel-structs" (session))
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (session))
(declare-function mevedel-workspace-directives "mevedel-structs" (workspace))
(declare-function mevedel-workspace-id "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x))
(declare-function mevedel-workspace-type "mevedel-structs" (cl-x))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-sanitize
                  "mevedel-session-artifacts" (name))

;; `mevedel-view'
(declare-function mevedel-view--abort-data-buffer
                  "mevedel-view" (data-buffer))
(autoload 'mevedel-view--abort-data-buffer "mevedel-view")

;; `mevedel-view-composer'
(declare-function mevedel-view-abort "mevedel-view-composer" ())
(declare-function mevedel-view-invocation-kind
                  "mevedel-view-composer" (name &optional session))
(autoload 'mevedel-view-abort "mevedel-view-composer" nil t)
(autoload 'mevedel-view-invocation-kind "mevedel-view-composer")

;; `mevedel-view-input-files'
(declare-function mevedel-view--media-dir "mevedel-view-input-files" ())
(autoload 'mevedel-view--media-dir "mevedel-view-input-files")

;; `mevedel-view-interaction'
(declare-function mevedel-view--interaction-rebuild
                  "mevedel-view-interaction" ())
(autoload 'mevedel-view--interaction-rebuild "mevedel-view-interaction")


;;
;;; Guest protocol primitives

(defun mevedel-collaboration--request-id-p (value)
  "Return non-nil when VALUE is a bounded browser request id."
  (and (integerp value) (<= 0 value #x1fffffffffffff)))


;;
;;; Snapshot delivery

(defun mevedel-collaboration--snapshot-frame-overhead ()
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

(defun mevedel-collaboration--snapshot-chunks (records &optional overhead)
  "Split RECORDS into lists of JSON records each under the wire bound.
The bound belongs to the frame that goes on the wire, not to the records
in it.  A record too large to travel in a frame of its own is dropped:
emitting a frame the relay must refuse costs the host connection, and the
relay collects the room with it, so one oversized record would end the
session for every guest.  OVERHEAD is the encoded bytes the carrying
frame costs before its records; it defaults to the snapshot frame's."
  (let* ((overhead (or overhead
                       (mevedel-collaboration--snapshot-frame-overhead)))
         (limit mevedel-collaboration--max-frame-json-bytes)
         chunks current (size 0))
    (dolist (record records)
      (let* ((json (mevedel-collaboration--json-record record))
             (bytes (string-bytes
                     (json-encode json))))
        (unless (> (+ overhead bytes) limit)
          ;; One separator for every record after the first in the chunk.
          (when (and current
                     (> (+ overhead size 1 bytes) limit))
            (push (nreverse current) chunks)
            (setq current nil size 0))
          (push json current)
          (setq size (+ size bytes (if (cdr current) 1 0))))))
    (when current
      (push (nreverse current) chunks))
    (nreverse chunks)))

(defconst mevedel-collaboration--command-hints
  '(("plan" . "[prompt]")
    ("goal" . "[objective]")
    ("compact" . "[instructions]")
    ("stop" . "[execution]")
    ("review" . "uncommitted | HEAD | branch:NAME | commit:REV | instructions")
    ("verify" . "uncommitted | HEAD | branch:NAME | commit:REV | instructions"))
  "Argument hints for the local slash commands a guest may invoke.
Only commands whose arguments a guest can meaningfully supply appear;
anything absent is offered as an argument-less button.")

(defconst mevedel-collaboration--invocation-defaults
  '(("review" . "uncommitted")
    ("verify" . "uncommitted"))
  "Arguments substituted when a guest invokes a command with none.
Without an argument these commands open a target picker in the host's
minibuffer, which a guest can neither see nor answer.")

(defun mevedel-collaboration--guest-role (guest)
  "Return the link tier of GUEST: `owner\=', `full\=', or nil for a viewer."
  (cond ((plist-get guest :owner) 'owner)
        ((plist-get guest :writable) 'full)))

(defun mevedel-collaboration--role-policy (role)
  "Return the `mevedel-collaboration-guest-skills\=' value for ROLE.
An owner link without an entry of its own inherits the `full\=' entry."
  (cdr (or (assq role mevedel-collaboration-guest-skills)
           (and (eq role 'owner)
                (assq 'full mevedel-collaboration-guest-skills)))))

(defun mevedel-collaboration--policy-admits-p (policy name)
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

(defun mevedel-collaboration--guest-invocable-p (name role)
  "Return non-nil when a guest holding ROLE may invoke NAME at all."
  (and (stringp name)
       (not (member name mevedel-collaboration-unsafe-guest-commands))
       (mevedel-collaboration--policy-admits-p
        (mevedel-collaboration--role-policy role) name)))

(defun mevedel-collaboration--guest-skills-admitted-p (names role session)
  "Return non-nil when ROLE may apply selected skill NAMES in SESSION."
  (and (proper-list-p names) names (<= (length names) 6)
       (= (length names) (length (delete-dups (copy-sequence names))))
       (cl-every
        (lambda (name)
          (and (mevedel-collaboration--guest-invocable-p name role)
               (eq (mevedel-view-invocation-kind name session) 'skill)))
        names)))

(defun mevedel-collaboration--guest-roster (room guest)
  "Return the invocations ROOM offers GUEST as JSON-safe descriptors.

Every local slash command and user-invocable skill the guest\='s tier
admits is listed, commands first.  Each entry carries the name, which
namespace it belongs to, and its argument hint, because a guest button
has to render the right sigil and say whether the invocation wants
arguments.  A name that resolves to neither namespace is dropped rather
than offered as a button that cannot work."
  (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer room))
              (view-buffer (buffer-local-value 'mevedel--view-buffer
                                               data-buffer))
              ((buffer-live-p view-buffer)))
    (with-current-buffer view-buffer
      (let ((session (plist-get room :session))
            (role (mevedel-collaboration--guest-role guest))
            roster)
        (dolist (name (delete-dups
                       (append
                        (mapcar #'car mevedel-slash-commands)
                        (mapcar #'mevedel-skill-name
                                (mevedel-skills-user-visible-skills session)))))
          (when (mevedel-collaboration--guest-invocable-p name role)
            (when-let* ((kind (mevedel-view-invocation-kind name session)))
              (push (append
                     (list (cons "name" name)
                           (cons "kind" (symbol-name kind)))
                     (when-let* ((hint (mevedel-collaboration--invocation-hint
                                        name kind session)))
                       (list (cons "hint" hint))))
                    roster))))
        (nreverse roster)))))

(defun mevedel-collaboration--invocation-hint (name kind session)
  "Return the argument hint for invocation NAME of KIND, or nil."
  (pcase kind
    ('skill
     (when-let* ((skill (cl-find name
                                 (mevedel-skills-user-visible-skills session)
                                 :key #'mevedel-skill-name :test #'equal)))
       (mevedel-skill-argument-hint skill)))
    ('command
     (cdr (assoc name mevedel-collaboration--command-hints)))))

(defun mevedel-collaboration--model-labels ()
  "Return the models a guest may pick for a new session, as labels."
  (vconcat (mapcar #'car (mevedel-model-candidates))))

(defun mevedel-collaboration--room-workspace (room)
  "Return ROOM's workspace, or nil.
A lobby has no session; it carries its workspace instead."
  (or (plist-get room :workspace)
      (when-let* ((session (plist-get room :session)))
        (mevedel-session-workspace session))))

(defun mevedel-collaboration--workspace-key (room)
  "Return an opaque key naming ROOM's workspace on this host, or nil.
A browser keeps the rooms it was given under it, so a room lists only
its own workspace's rooms; the key reveals no path."
  (when-let* ((workspace (mevedel-collaboration--room-workspace room)))
    (substring (secure-hash 'sha256
                            (format "%s\0%s\0%s" (system-name)
                                    (mevedel-workspace-type workspace)
                                    (mevedel-workspace-id workspace)))
               0 32)))

(defun mevedel-collaboration--send-snapshot (room peer)
  "Send ROOM's welcome and chunked snapshot to guest PEER."
  (let* ((transport (plist-get room :transport))
         (guest (mevedel-collaboration--guest room peer))
         (records (plist-get room :records))
         (chunks (or (mevedel-collaboration--snapshot-chunks records)
                     (list nil))))
    (mevedel-collaboration--transport-send
     transport peer
     (append
      (list :t "welcome"
            :proto mevedel-collaboration--protocol-version
            :readOnly (if (plist-get guest :writable) :json-false t)
            :workspace (mevedel-collaboration--workspace-key room)
            ;; Count what is actually sent: a record too large for a frame of
            ;; its own is dropped, and promising it would leave the guest
            ;; waiting for a chunk that never arrives.
            :recordCount (apply #'+ (mapcar #'length chunks)))
      ;; The host-curated roster is the guest's whole discovery
      ;; surface; a view link gets none, having no way to use it.
      (when-let* (((plist-get guest :writable))
                  (roster (mevedel-collaboration--guest-roster room guest)))
        (list :commands (vconcat roster)))
      ;; Only a writable guest can ask for a session to choose one for.
      (when (plist-get guest :writable)
        (list :models (mevedel-collaboration--model-labels)))))
    (cl-loop for rest on chunks do
             (mevedel-collaboration--transport-send
              transport peer
              (list :t "snapshot-chunk"
                    :records (vconcat (car rest))
                    :final (if (cdr rest) :json-false t))))))


;;
;;; Remote interactions

(defvar mevedel-collaboration--ui-request-counter 0
  "Monotonic id source for ui-request frames within this Emacs process.")

(defvar mevedel-collaboration-remote-guest nil
  "Display name of the guest whose answer is being applied, or nil.
Bound around a ui-response handler so downstream effects -- such as a
plan revision request queued by remote feedback -- can attribute their
output to the answering guest.")

(defun mevedel-collaboration--writable-peers (room)
  "Return the peer ids of ROOM's writable guests."
  (let (peers)
    (maphash (lambda (peer guest)
               (when (plist-get guest :writable)
                 (push peer peers)))
             (plist-get room :guests))
    (nreverse peers)))

(defun mevedel-collaboration--audience-peer-p (room peer audience)
  "Return non-nil when PEER in ROOM belongs to AUDIENCE.

AUDIENCE narrows an interaction below the writable default.  `owner'
restricts it to owner-link guests.  A nil AUDIENCE is every writable
guest, the ordinary case."
  (let ((guest (mevedel-collaboration--guest room peer)))
    (and guest
         (pcase audience
           ('nil t)
           ('owner (plist-get guest :owner))
           (_ nil)))))

(defun mevedel-collaboration--audience-peers-for (room audience)
  "Return ROOM's writable peers that belong to AUDIENCE."
  (if (null audience)
      (mevedel-collaboration--writable-peers room)
    (seq-filter (lambda (peer)
                  (mevedel-collaboration--audience-peer-p
                   room peer audience))
                (mevedel-collaboration--writable-peers room))))

(defun mevedel-collaboration--audience-peers (room overlay)
  "Return ROOM's peers that may see OVERLAY's interaction."
  (mevedel-collaboration--audience-peers-for
   room (plist-get (overlay-get overlay 'mevedel--remote) :audience)))

(defun mevedel-collaboration--ui-request-frame (request-id overlay)
  "Return the ui-request frame for OVERLAY under REQUEST-ID."
  (let ((remote (overlay-get overlay 'mevedel--remote)))
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
     ;; A cancel handler settles just this interaction -- for the Ask
     ;; questionnaire, the run continues -- so the guest gets a Dismiss.
     (when (plist-get remote :cancel)
       (list :allowCancel t))
     ;; A questionnaire travels structurally; the guest answers all
     ;; questions atomically through the :answers response field.
     (when-let* ((questions (plist-get remote :questions)))
       (list :questions (vconcat (funcall questions)))))))

(defun mevedel-collaboration--on-prompt-created (overlay)
  "Present prompt OVERLAY to the active room's writable guests.

A re-render of the same interaction -- the permission queue redraws its
head on every selection change -- reuses the existing request id, so a
guest sees one card updated in place instead of an accumulating pile."
  (when-let* ((room (mevedel-collaboration--room-for-overlay overlay))
              (remote (overlay-get overlay 'mevedel--remote)))
    (when mevedel-collaboration-remote-interactions
      (let* ((requests (plist-get room :ui-requests))
             (interaction-id
              (overlay-get overlay 'mevedel-view-interaction-id))
             (existing-id
              (and interaction-id
                   (catch 'found
                     (maphash
                      (lambda (id tracked)
                        (when (and (overlayp tracked)
                                   (equal interaction-id
                                          (overlay-get
                                           tracked
                                           'mevedel-view-interaction-id)))
                          (throw 'found id)))
                      requests)
                     nil)))
             (request-id
              (or existing-id
                  (cl-incf mevedel-collaboration--ui-request-counter)))
             (peers (mevedel-collaboration--audience-peers room overlay)))
        (puthash request-id overlay requests)
        (let ((frame (mevedel-collaboration--ui-request-frame
                      request-id overlay)))
          (dolist (peer peers)
            (mevedel-collaboration--transport-send
             (plist-get room :transport) peer frame)))
        (unless existing-id
          ;; Waking a guest for a decision it is not shown is noise, so
          ;; the push follows the same audience the frame did.
          (if (plist-get remote :audience)
              (mevedel-collaboration--push-guests
               room (delq nil
                          (mapcar (lambda (peer)
                                    (plist-get
                                     (mevedel-collaboration--guest room peer)
                                     :guest-id))
                                  peers)))
            (mevedel-collaboration--push-writable-guests room)))))))

(defun mevedel-collaboration--on-prompt-settled (overlay)
  "Dismiss OVERLAY's ui-request from every guest surface.
Every room is searched rather than the overlay's buffer resolved: a
settled overlay may already be deleted, and a deleted overlay no longer
knows where it lived."
  (dolist (room (mevedel-collaboration--room-list))
    (let ((requests (plist-get room :ui-requests)))
      (maphash
       (lambda (request-id tracked)
         (when (eq tracked overlay)
           (remhash request-id requests)
           (dolist (peer (mevedel-collaboration--writable-peers room))
             (mevedel-collaboration--transport-send
              (plist-get room :transport) peer
              (list :t "ui-request-end" :reqId request-id)))))
       requests))))

(defun mevedel-collaboration--send-ui-requests (room peer)
  "Send ROOM's active ui-requests to writable guest PEER.

A narrowed interaction is withheld here as well as on creation: a guest
must not collect a decision it may not see by reconnecting."
  (let ((requests (plist-get room :ui-requests))
        ids)
    (maphash (lambda (request-id _overlay) (push request-id ids)) requests)
    (dolist (request-id (sort ids #'<))
      (let* ((overlay (gethash request-id requests))
             (audience (plist-get (overlay-get overlay 'mevedel--remote)
                                  :audience)))
        (when (or (null audience)
                  (mevedel-collaboration--audience-peer-p
                   room peer audience))
          (mevedel-collaboration--transport-send
           (plist-get room :transport) peer
           (mevedel-collaboration--ui-request-frame request-id overlay)))))))

(defun mevedel-collaboration--handle-ui-response (room peer frame)
  "Settle the ui-request answered by writable guest PEER through FRAME.

The first answer -- from Emacs or any guest -- wins; the shared settle
already guards exactly-once, and a request no longer in the registry is
ignored silently.  A function option runs in the prompt's buffer so an
answer can execute the same path the host key binding would."
  (let ((guest (mevedel-collaboration--guest room peer))
        (request-id (plist-get frame :reqId)))
    (when (and guest
               (plist-get guest :writable)
               mevedel-collaboration-remote-interactions
               (integerp request-id))
      (when-let* ((overlay (gethash request-id (plist-get room :ui-requests)))
                  ;; Seeing a narrowed interaction and answering it are
                  ;; the same authority, so the audience is rechecked
                  ;; here: a request id is guessable, the audience is not.
                  ((mevedel-collaboration--audience-peer-p
                    room peer
                    (plist-get (overlay-get overlay 'mevedel--remote)
                               :audience))))
        (let* ((remote (overlay-get overlay 'mevedel--remote))
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
                 ;; A questionnaire response is submitted atomically.  A
                 ;; blank string is the Ask tool's explicit no-preference.
                 ((and answers
                       (functionp answer-handler)
                       (listp answers)
                       (let ((trimmed
                              (mapcar
                               (lambda (answer)
                                 (and
                                  (stringp answer)
                                  (<= (string-bytes answer)
                                      mevedel-collaboration--max-prompt-bytes)
                                  (string-trim answer)))
                               answers)))
                         (and (not (memq nil trimmed))
                              ;; Every answer lands in one tool result, so
                              ;; the set shares the budget its parts pass.
                              (<= (apply #'+ (mapcar #'string-bytes trimmed))
                                  mevedel-collaboration--max-prompt-bytes)
                              (lambda ()
                                (funcall answer-handler trimmed))))))
                 ((and feedback-handler
                       (mevedel-collaboration--guest-text feedback))
                  ;; A function handler owns the whole feedback flow, for
                  ;; prompts whose feedback is not a plain settle outcome.
                  (let ((text (mevedel-collaboration--guest-text feedback)))
                    (if (functionp feedback-handler)
                        (lambda () (funcall feedback-handler text))
                      (cons 'feedback text))))
                 ((and (integerp option) (nth option options))
                  (car (nth option options))))))
          (when (and outcome (buffer-live-p (overlay-buffer overlay)))
            (message "mevedel: interaction answered by guest %s"
                     (plist-get guest :name))
            (with-current-buffer (overlay-buffer overlay)
              (let ((mevedel-collaboration-remote-guest
                     (plist-get guest :name)))
                (condition-case err
                    (if (functionp outcome)
                        (funcall outcome)
                      (mevedel--prompt--settle overlay outcome))
                  (user-error
                   (message "mevedel: remote answer rejected: %s"
                            (error-message-string err))))))))))))


;;
;;; Inbound guest frames

(defun mevedel-collaboration--admit-hello (room peer frame)
  "Register PEER in ROOM from its hello FRAME and return its guest plist.
A protocol mismatch is answered with an error frame and returns nil.
Authority comes only from the tokens FRAME proves it holds."
  (let ((proto (plist-get frame :proto)))
    (if (not (equal proto mevedel-collaboration--protocol-version))
        (progn
          (mevedel-collaboration--transport-send
           (plist-get room :transport) peer
           (list :t "error"
                 :message (format "protocol mismatch: host speaks %d"
                                  mevedel-collaboration--protocol-version)))
          nil)
      (let* ((name (mevedel-collaboration--sanitize-guest-name
                    (plist-get frame :name)))
             (claimed (mevedel-collaboration--base64url-decode
                       (plist-get frame :writeToken)))
             (writable (and claimed
                            (equal claimed (plist-get room :write-token))))
             (claimed-owner (mevedel-collaboration--base64url-decode
                             (plist-get frame :ownerToken)))
             ;; Owner authority is never granted on its own: the owner
             ;; link contains the write token, so a peer claiming one
             ;; without the other is a forgery attempt, not a tier.
             (owner (and writable claimed-owner
                         (equal claimed-owner (plist-get room :owner-token))))
             (guest (list :name name :writable writable :owner owner
                          :ready t
                          :guest-id (mevedel-collaboration--sanitize-guest-id
                                     (plist-get frame :guestId)))))
        (puthash peer guest (plist-get room :guests))
        guest))))

(defun mevedel-collaboration--handle-hello (room peer frame)
  "Register guest PEER from its hello FRAME and send the snapshot."
  (when-let* ((guest (mevedel-collaboration--admit-hello room peer frame)))
    (mevedel-collaboration--send-snapshot room peer)
    (mevedel-collaboration--publish-history room peer)
    ;; Queue and busy state travel only on change, so a joining
    ;; guest is told the current ones directly.
    (mevedel-collaboration--send-queue-state room peer guest t)
    (mevedel-collaboration--transport-send
     (plist-get room :transport) peer
     (mevedel-collaboration--status-frame room))
    (when (plist-get guest :owner)
      (mevedel-collaboration-recovery-send room peer))
    ;; The roster broadcast is latched on change, so a joining guest
    ;; is told the current one directly -- an empty roster included,
    ;; because a reconnecting viewer must clear stale rows.
    (mevedel-collaboration--transport-send
     (plist-get room :transport) peer
     (mevedel-collaboration--agents-frame room))
    ;; The task list is latched the same way.
    (mevedel-collaboration--transport-send
     (plist-get room :transport) peer
     (mevedel-collaboration--tasks-frame room))
    (when (and (plist-get guest :writable)
               mevedel-collaboration-remote-interactions)
      (mevedel-collaboration--send-ui-requests room peer))))

(defconst mevedel-collaboration--max-push-endpoint-bytes 2048
  "Maximum encoded bytes accepted for a browser push endpoint.")

(defun mevedel-collaboration--push-endpoint-p (value)
  "Return non-nil when VALUE is a bounded HTTPS push endpoint."
  (and (stringp value)
       (<= (string-bytes value)
           mevedel-collaboration--max-push-endpoint-bytes)
       (string-match-p "\\`https://[^[:space:][:cntrl:]#]+\\'" value)))

(defun mevedel-collaboration--handle-push-subscription (room peer frame)
  "Forward authenticated PEER's push subscription FRAME for ROOM."
  (when-let* ((guest (mevedel-collaboration--guest room peer))
              ((plist-get guest :ready))
              (guest-id (plist-get guest :guest-id)))
    (pcase (plist-get frame :t)
      ("push-subscribe"
       (when-let* ((endpoint (plist-get frame :endpoint))
                   ((mevedel-collaboration--push-endpoint-p endpoint)))
         (puthash guest-id
                  (list :endpoint endpoint
                        :writable (plist-get guest :writable))
                  (plist-get room :push-guests))
         (mevedel-collaboration--transport-control
          (plist-get room :transport)
          (list :t "push-subscribe" :peer peer :guestId guest-id
                :endpoint endpoint
                :active (if (eq t (plist-get frame :active))
                            t :json-false)))))
      ("push-unsubscribe"
       (remhash guest-id (plist-get room :push-guests))
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-unsubscribe" :guestId guest-id)))
      ("push-state"
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-state" :peer peer :guestId guest-id
              :active (if (eq t (plist-get frame :active))
                          t :json-false)))))))

(defun mevedel-collaboration--push-guests (room guest-ids)
  "Ask ROOM's relay to wake subscribed GUEST-IDS."
  (when guest-ids
    (mevedel-collaboration--transport-control
     (plist-get room :transport)
     (list :t "push" :guestIds (vconcat guest-ids)))))

(defun mevedel-collaboration--push-writable-guests (room)
  "Wake every notification-subscribed writable guest of ROOM."
  (let (guest-ids)
    (maphash (lambda (guest-id subscription)
               (when (plist-get subscription :writable)
                 (push guest-id guest-ids)))
             (plist-get room :push-guests))
    (mevedel-collaboration--push-guests room guest-ids)))

(defun mevedel-collaboration--restore-push-subscriptions (room)
  "Restore ROOM's Web Push endpoints after a relay reconnect."
  (when-let* ((subscriptions (plist-get room :push-guests)))
    (maphash
     (lambda (guest-id subscription)
       (mevedel-collaboration--transport-control
        (plist-get room :transport)
        (list :t "push-subscribe" :peer 0 :guestId guest-id
              :endpoint (plist-get subscription :endpoint)
              :active :json-false)))
     subscriptions)))

(cl-defun mevedel-collaboration--handle-prompt (room peer frame)
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
  (let* ((guest (mevedel-collaboration--guest room peer))
         (role (mevedel-collaboration--guest-role guest))
         (invoke (plist-get frame :invoke))
         (skills (plist-get frame :skills))
         (text (plist-get frame :text)))
    (when (and invoke
               (not (mevedel-collaboration--guest-invocable-p invoke role)))
      (cl-return-from mevedel-collaboration--handle-prompt))
    (when (or (and invoke skills)
              (and (plist-member frame :skills)
                   (not (mevedel-collaboration--guest-skills-admitted-p
                         skills role (plist-get room :session)))))
      (cl-return-from mevedel-collaboration--handle-prompt))
    ;; A bare invocation of a command that would otherwise prompt on the
    ;; host gets the argument the host would have picked first.
    (when (and invoke (or (null text) (string-blank-p text)))
      (setq text (or (cdr (assoc invoke
                                 mevedel-collaboration--invocation-defaults))
                     text)))
    (when (and guest
               (plist-get guest :writable)
               ;; An invocation may carry no arguments at all; a plain
               ;; prompt still has to say something.
               (or (and (or invoke skills) (or (null text) (stringp text)))
                   (mevedel-collaboration--guest-text text)))
      ;; The prompt frame may carry a fresher display name than the hello
      ;; did; the badge should show what the guest typed.
      (when (stringp (plist-get frame :name))
        (plist-put guest :name (mevedel-collaboration--sanitize-guest-name
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
                      mevedel-collaboration--duplicate-prompt-window))
          (cl-return-from mevedel-collaboration--handle-prompt))
        (let* ((data-buffer (mevedel-collaboration--room-data-buffer room))
               (view-buffer (and data-buffer
                                 (buffer-local-value 'mevedel--view-buffer
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
                         (mevedel-collaboration--save-guest-attachments
                          (plist-get frame :images)))
                     (error
                      ;; A failed media write is this prompt's problem, not
                      ;; the room's: letting it reach the frame handler
                      ;; tears the session down for every guest.
                      (display-warning
                       'mevedel
                       (format "Guest attachment could not be saved: %s"
                               (error-message-string err))
                       :warning)
                      (cl-return-from
                          mevedel-collaboration--handle-prompt))))
                  (queued nil))
              ;; Latch before the enqueue, which redraws and can therefore
              ;; re-enter, and give the latch back when nothing was queued.
              (plist-put guest :last-prompt (cons dedup-key now))
              (unwind-protect
                  (progn
                    (setq queued
                          (mevedel-view-enqueue-external-follow-up
                           data-buffer (or text "")
                           :guest-name (plist-get guest :name)
                           :guest-id (plist-get guest :guest-id)
                           :paths paths
                           :invoke invoke
                           :skills skills
                           :guest-role role
                           :directive-id
                           (unless (or invoke skills)
                             (mevedel-collaboration--guest-directive-id
                              room frame))))
                    (when queued
                      (mevedel-collaboration--transport-send
                       (plist-get room :transport) peer
                       (append
                        (list :t "queued")
                        (when-let* ((id (plist-get queued :id)))
                          (list :id id))
                        (when-let* ((position
                                     (mevedel-collaboration--queue-position
                                      room queued)))
                          (list :position position))))))
                (unless queued
                  (plist-put guest :last-prompt nil)
                  (dolist (path paths)
                    (when (file-exists-p path)
                      (ignore-errors (delete-file path)))))))))))))

(defun mevedel-collaboration--guest-directive-id (room frame)
  "Return the directive id FRAME asks ROOM to scope its prompt to, or nil.

The viewer sends the id its transcript filter is showing, which lists
directives with activity in this room's session.  An id for a directive
the workspace no longer has, or one bound to another session, yields
nil, so the prompt goes to main chat instead of failing.  A room grants
its own session only: a directive bound elsewhere would run, and maybe
restore, another session."
  (when-let* ((id (plist-get frame :directive))
              ((stringp id))
              (session (plist-get room :session))
              (workspace (mevedel-session-workspace session))
              (directive (cl-find id (mevedel-workspace-directives workspace)
                                  :key #'mevedel-directive-id :test #'equal))
              ((equal (mevedel-directive-session-id directive)
                      (mevedel-session-session-id session))))
    id))

(defun mevedel-collaboration--utf8-text-p (bytes)
  "Return non-nil when unibyte BYTES are UTF-8 text without NUL bytes."
  (and (not (string-search "\0" bytes))
       (not (string-match-p "[\200-\377]"
                            (decode-coding-string bytes 'utf-8 t)))))

(defun mevedel-collaboration--attachment-extension (image bytes)
  "Return the extension guest attachment IMAGE with BYTES is saved under.
A typed attachment takes its type's extension.  A plain-text one -- a
source or markup file -- keeps its own extension when that is short and
not one Read treats as binary, else `txt', and must be UTF-8 text.
Return nil for anything else."
  (let ((mime (plist-get image :mime))
        (own (plist-get image :extension)))
    (if (and (equal mime "text/plain") own)
        (and (mevedel-collaboration--utf8-text-p bytes)
             (if (and (stringp own)
                      (string-match-p "\\`[a-z0-9]\\{1,12\\}\\'" own)
                      (not (mevedel-tool-fs-read--binary-extension-p
                            (concat "attachment." own))))
                 own
               "txt"))
      (cdr (assoc mime mevedel-collaboration--attachment-extensions)))))

(defun mevedel-collaboration--save-guest-attachments (images)
  "Save valid guest attachments IMAGES under the session media directory.
IMAGES is the decoded frame list of (:mime STRING :data BASE64) plists;
a plain-text one may name its own `:extension'.  Return the saved
absolute paths.  Runs in the view buffer.  Anything invalid -- unknown
type, undecodable data, non-text bytes, or a set over the byte budget --
drops the whole set rather than attaching a partial one."
  (when (and images (listp images)
             (<= (length images)
                 mevedel-collaboration--max-prompt-attachments))
    (catch 'invalid
      (let ((total 0)
            (decoded nil))
        (dolist (image images)
          (let* ((bytes (and (stringp (plist-get image :data))
                             (condition-case nil
                                 (base64-decode-string
                                  (plist-get image :data))
                               (error nil))))
                 (extension (and bytes (mevedel-collaboration--attachment-extension
                                        image bytes))))
            (unless (and extension (> (length bytes) 0))
              (throw 'invalid nil))
            (cl-incf total (length bytes))
            (when (> total mevedel-collaboration--max-attachment-bytes)
              (throw 'invalid nil))
            (push (cons extension bytes) decoded)))
        (let ((dir (mevedel-view--media-dir))
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

(defun mevedel-collaboration--save-guest-files (data-buffer images)
  "Save guest attachment IMAGES for DATA-BUFFER's session; return their paths.
IMAGES is a frame list or vector of (:mime STRING :data BASE64) plists.
Return nil when there are none, and signal when a set cannot be attached
whole, so a question never queues without the files its sender chose.
The caller deletes the paths when its own enqueue fails."
  (when (and images (> (length images) 0))
    (let ((view (and (buffer-live-p data-buffer)
                     (buffer-local-value 'mevedel--view-buffer data-buffer))))
      (or (and (buffer-live-p view)
               (with-current-buffer view
                 (mevedel-collaboration--save-guest-attachments
                  (append images nil))))
          (error "The attachments could not be attached; check their type and size")))))

(defun mevedel-collaboration--handle-retract (room peer frame)
  "Remove the pending entry FRAME names when guest PEER queued it.

Authority is per entry: the id must belong to an entry this guest's
stable id queued, so no guest can delete another guest's or the host's
pending input.  The entry's attachment files leave with it, mirroring
the failed-enqueue cleanup."
  (let ((guest (mevedel-collaboration--guest room peer))
        (id (plist-get frame :id)))
    (when (and guest
               (plist-get guest :writable)
               (plist-get guest :guest-id)
               (integerp id))
      (when-let* ((session (plist-get room :session))
                  (entries (mevedel-session-pending-follow-ups session))
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
        (mevedel-pending-inputs--set-queues
         session 'follow-up (remq entry entries))
        (dolist (path (plist-get entry :guest-paths))
          (when (file-exists-p path)
            (ignore-errors (delete-file path))))
        (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer
                                  room))
                    (view-buffer (buffer-local-value 'mevedel--view-buffer
                                                     data-buffer))
                    ((buffer-live-p view-buffer)))
          (with-current-buffer view-buffer
            (mevedel-view--interaction-rebuild)))
        (mevedel-collaboration--publish-queue room)))))

(defun mevedel-collaboration--handle-abort (room peer)
  "Abort the running request for writable guest PEER."
  (let ((guest (mevedel-collaboration--guest room peer)))
    (when (and guest (plist-get guest :writable))
      (when-let* ((data-buffer (mevedel-collaboration--room-data-buffer room)))
        (let ((view-buffer
               (buffer-local-value 'mevedel--view-buffer data-buffer)))
          (if (buffer-live-p view-buffer)
              (with-current-buffer view-buffer
                (mevedel-view-abort))
            (mevedel-view--abort-data-buffer data-buffer)))))))

(defun mevedel-collaboration--on-frame (data-buffer peer frame)
  "Dispatch decoded guest FRAME from PEER for DATA-BUFFER's room.

Failure isolation mirrors the gptel observers: a fault in guest input
handling preserves the room and reports a safe notice.  Guests act
while nobody may be at the keyboard, so frames run with
`inhibit-interaction': a step that would ask in Emacs is refused to the
sending guest instead of waiting for an answer."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (condition-case nil
        (condition-case err
            (let ((inhibit-interaction t))
              (mevedel-collaboration--dispatch-frame room peer frame))
          (user-error
           (mevedel-collaboration--transport-send
            (plist-get room :transport) peer
            (list :t "notice" :message
                  (if (mevedel-collaboration--owner room peer)
                      (error-message-string err)
                    "The host could not complete this action; the owner can review session status"))))
          (inhibited-interaction
           (mevedel-collaboration--transport-send
            (plist-get room :transport) peer
            (list :t "notice" :message mevedel-collaboration-needs-host-message))
           (display-warning
            'mevedel
            (format "A guest's %s request needed a decision in Emacs and was refused%s"
                    (plist-get frame :t) (mevedel-view--refused-question err)))))
      (error (mevedel-collaboration--observer-failure room)))))

(defun mevedel-collaboration--dispatch-frame (room peer frame)
  "Handle guest FRAME from PEER in ROOM."
  (pcase (plist-get frame :t)
    ("recovery" (mevedel-collaboration-recovery-handle room peer frame))
    ("hello" (mevedel-collaboration--handle-hello room peer frame))
    ("set-name"
     (when-let* ((guest (mevedel-collaboration--guest room peer))
                 ((stringp (plist-get frame :name))))
       (plist-put guest :name
                  (mevedel-collaboration--sanitize-guest-name
                   (plist-get frame :name)))))
    ((or "push-subscribe" "push-unsubscribe" "push-state")
     (mevedel-collaboration--handle-push-subscription
      room peer frame))
    ("prompt" (mevedel-collaboration--handle-prompt room peer frame))
    ("abort" (mevedel-collaboration--handle-abort room peer))
    ("fetch-agent"
     (mevedel-collaboration--handle-fetch-agent room peer frame))
    ("execution-result-get"
     (mevedel-collaboration--handle-execution-result-get room peer frame))
    ("artifact-get"
     (mevedel-collaboration--handle-artifact-get room peer frame))
    ("artifact-delete"
     (mevedel-collaboration--handle-artifact-delete room peer frame))
    ("artifact-comment"
     (mevedel-collaboration--handle-artifact-comment room peer frame))
    ("file-upload"
     (mevedel-collaboration-files-handle-upload
      room peer frame
      (mevedel-workspace-root
       (mevedel-session-workspace (plist-get room :session)))))
    ("history-get"
     (mevedel-collaboration--handle-history-get room peer frame))
    ((or "editing" "editing-presence")
     (mevedel-collaboration-editing-handle room peer frame))
    ("retract" (mevedel-collaboration--handle-retract room peer frame))
    ("ui-response"
     (mevedel-collaboration--handle-ui-response room peer frame))
    ("set-mode"
     (mevedel-collaboration--handle-set-mode room peer frame))
    ("new-session"
     (mevedel-collaboration--handle-new-session room peer frame))))

(defun mevedel-collaboration--on-control (data-buffer event peer)
  "Handle relay control EVENT for PEER in DATA-BUFFER's room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (pcase event
      ;; A joined peer becomes a guest only through its hello frame.
      ('peer-joined nil)
      ('peer-left
       (mevedel-collaboration-editing-depart room peer)
       (remhash peer (plist-get room :guests))))))

(defun mevedel-collaboration--on-state (data-buffer state)
  "Track relay transport STATE for DATA-BUFFER's room."
  (when-let* ((room (mevedel-collaboration--room-for-buffer data-buffer)))
    (pcase state
      ;; The relay garbage-collects the room with the host connection, so
      ;; a drop invalidated every guest; they rejoin and re-hello against
      ;; the re-created room.
      ('down
       (maphash (lambda (peer _guest) (mevedel-collaboration-editing-depart room peer))
                (plist-get room :guests))
       (clrhash (plist-get room :guests))
       ;; The links and QR are handed out before the async dial settles.
       ;; A dial that has never succeeded -- wrong relay URL or a missing or
       ;; stale configured host token -- would otherwise retry forever
       ;; with the user none the wiser that the share is dead.
       (unless (or (plist-get room :was-open)
                   (plist-get room :dial-warned))
         (setq room (plist-put room :dial-warned t))
         (display-warning
          'mevedel
          (concat "Collaboration relay dial failing; the share is not "
                  "live. Check `mevedel-collaboration-relay-url' and "
                  "the relay's optional host-token configuration.")
          :warning)))
      ('open
       (setq room (plist-put room :was-open t))
       (mevedel-collaboration--restore-push-subscriptions room))
      ('stopped nil))))

;; The dispatch above is the cold feature boundary for the two read-only
;; guest extensions.  Neither extension requires this module back.
(require 'mevedel-collaboration-agent)
(require 'mevedel-collaboration-artifact)
(require 'mevedel-collaboration-artifact-comments)
(require 'mevedel-collaboration-editing)
(require 'mevedel-collaboration-files)

(provide 'mevedel-collaboration-guest)
;;; mevedel-collaboration-guest.el ends here
