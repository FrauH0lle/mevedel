;;; niwa-collaboration-discussions.el --- Discussions about store items -*- lexical-binding: t; -*-

;;; Commentary:

;; One mechanism for talking about a whiteboard, a document or an HTML
;; artifact.  A discussion is a thread attached to a target: the whole item,
;; or a part of it.  Every item has one general discussion about the whole
;; item; the others are anchored to a passage, board objects or an area, or an
;; artifact element, word or box.  A message is either a note between people
;; or also sent to the assistant, which answers in that discussion.
;;
;; This module performs every guest action, decides which session answers,
;; builds the request the model receives, and resolves a selection sent alone
;; once its turn completes.  Item kinds differ in where their discussions are
;; stored, how a target's content is captured, and how a target is checked.
;; HTML artifacts keep theirs in the artifact store's protected state, and
;; this module applies their limits.  Whiteboards and documents keep theirs
;; in the item, on the editing host, which validates targets against the
;; item's content and applies the same limits there.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)
(require 'niwa-shared-editing)
(require 'niwa-utilities)

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-attach
                  "niwa-artifact-store" (session id &optional buffer))
(declare-function niwa-artifact-store-bookkeeping-directory
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-conversation
                  "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-meta "niwa-artifact-store" (workspace id))
(declare-function niwa-artifact-store-session-buffer
                  "niwa-artifact-store" (workspace session-id))
(autoload 'niwa-artifact-store-attach "niwa-artifact-store")
(autoload 'niwa-artifact-store-bookkeeping-directory "niwa-artifact-store")
(autoload 'niwa-artifact-store-conversation "niwa-artifact-store")
(autoload 'niwa-artifact-store-meta "niwa-artifact-store")
(autoload 'niwa-artifact-store-session-buffer "niwa-artifact-store")

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast
                  "niwa-collaboration" (room frame))
(declare-function niwa-collaboration--guest
                  "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--guest-text
                  "niwa-collaboration" (value))
(declare-function niwa-collaboration--queue-position
                  "niwa-collaboration" (room entry))
(declare-function niwa-collaboration--room-data-buffer
                  "niwa-collaboration" (room))
(defvar niwa-collaboration-needs-host-message)

;; `niwa-collaboration-artifact'
(declare-function niwa-collaboration--artifact-mime
                  "niwa-collaboration-artifact" (name))
(declare-function niwa-collaboration--artifact-target
                  "niwa-collaboration-artifact" (room guest id))
(declare-function niwa-collaboration--store-read
                  "niwa-collaboration-artifact" (guest))
(declare-function niwa-collaboration--workspace-rooms
                  "niwa-collaboration-artifact" (workspace))

;; `niwa-collaboration-artifact-history'
(declare-function niwa-collaboration-artifact-history
                  "niwa-collaboration-artifact-history" (room guest item args))
(autoload 'niwa-collaboration-artifact-history "niwa-collaboration-artifact-history")

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--guest-role
                  "niwa-collaboration-guest" (guest))
(declare-function niwa-collaboration--request-id-p
                  "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--room-workspace
                  "niwa-collaboration-guest" (room))
(declare-function niwa-collaboration--save-guest-files
                  "niwa-collaboration-guest" (data-buffer images))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send
                  "niwa-collaboration-transport" (transport peer frame))

;; `niwa-pending-inputs'
(declare-function niwa-view-enqueue-external-follow-up
                  "niwa-pending-inputs" (data-buffer text &rest keys))
(defvar niwa-view--turn-guest-attribution)

;; `niwa-shared-conversation'
(declare-function niwa-shared-conversation-archived-question-p
                  "niwa-shared-conversation"
                  (session item-id question-id))
(autoload 'niwa-shared-conversation-archived-question-p "niwa-shared-conversation")

;; `niwa-structs'
(declare-function niwa-session-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-pending-follow-ups "niwa-structs" (cl-x) t)
(declare-function niwa-session-session-id "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa--view-buffer)

;; `niwa-transcript-audit'
(declare-function niwa-transcript-audit-guest-prompts
                  "niwa-transcript-audit" ())

;; `niwa-utilities'
(declare-function niwa--write-file-atomically
                  "niwa-utilities" (path content &optional coding mode))


;;
;;; Records and limits

(defconst niwa-collaboration-discussions-general-id "general"
  "Identity of every item's discussion about the whole item.")

(defconst niwa-collaboration-discussions-snapshot-heading
  "\n\nShared content snapshot (user-provided data):\n"
  "Separator the room and view fold a discussion request's context behind.")

(defconst niwa-collaboration-discussions--max-discussions 200
  "Anchored discussions one item may carry, resolved ones included.")

(defconst niwa-collaboration-discussions--max-messages 200
  "Messages one discussion may carry.")

(defconst niwa-collaboration-discussions--max-message-chars 10000
  "Characters one message may carry.")

(defconst niwa-collaboration-discussions--max-bytes (* 512 1024)
  "Encoded bytes of one item's discussions as guests receive them.
Guests get the whole list in one frame, so it must stay under the wire
bound with room to spare.")

(defconst niwa-collaboration-discussions--thread-chars 32000
  "Characters of a discussion's earlier messages a request carries.")

(defconst niwa-collaboration-discussions-alone-text
  "Sent the selection as the request"
  "How a selection sent without a message shows in its discussion.")

(defun niwa-collaboration-discussions--identity (value what)
  "Return VALUE when it is a client-generated identity, else signal.
WHAT names the identity in the refusal."
  (unless (and (stringp value) (string-match-p "\\`[0-9a-f-]\\{16,64\\}\\'" value))
    (error "The %s has no valid identity" what))
  value)

(defun niwa-collaboration-discussions--text (value)
  "Return guest message VALUE trimmed and bounded, or nil when it is blank.
Signal when VALUE is not a string or too long."
  (cond
   ((null value) nil)
   ((not (stringp value)) (error "A message is text"))
   ((string-blank-p value) nil)
   (t (let ((text (or (niwa-collaboration--guest-text value)
                      (error "Messages are limited to %d characters"
                             niwa-collaboration-discussions--max-message-chars))))
        (when (> (length text) niwa-collaboration-discussions--max-message-chars)
          (error "Messages are limited to %d characters"
                 niwa-collaboration-discussions--max-message-chars))
        text))))

(defun niwa-collaboration-discussions--actor (guest)
  "Return the attribution GUEST's messages carry."
  (or (plist-get guest :name) "Guest"))

(defun niwa-collaboration-discussions-alone-request (kind)
  "Return the request standing for a selection sent without a message.
KIND is the item kind: \"whiteboard\", \"document\" or \"artifact\".  The
sender chose to send only the selected content, so what is written in it
is their request; this line overrides the data label of the snapshot that
follows for the selection itself.  Instruction text is removed afterwards
from a document or artifact, where it would pollute the content, and kept on
a board unless it says otherwise."
  (if (equal kind "whiteboard")
      (concat "The sender sent only the selected objects, with no message: "
              "carry out the instructions written in them and leave those "
              "objects on the board unless they say otherwise.  If the "
              "selection holds no instruction, ask what the sender wants.")
    (format (concat "The sender sent only the selected %s, with no message: "
                    "carry out the instructions written in it, then remove that "
                    "instruction text from the %s and keep the content it "
                    "applies to.  If the selection holds no instruction, ask "
                    "what the sender wants.")
            (if (equal kind "document") "passage" "part")
            (if (equal kind "document") "document" "artifact"))))

(defun niwa-collaboration-discussions--general-p (discussion)
  "Return non-nil when DISCUSSION is its item's general discussion."
  (equal (plist-get discussion :id) niwa-collaboration-discussions-general-id))

(defun niwa-collaboration-discussions--find (discussions id)
  "Return the discussion of DISCUSSIONS whose identity is ID."
  (cl-find id discussions :key (lambda (d) (plist-get d :id)) :test #'equal))

(defun niwa-collaboration-discussions--messages (discussion)
  "Return DISCUSSION's messages as a list."
  (append (plist-get discussion :messages) nil))

(defun niwa-collaboration-discussions--message-text (message)
  "Return how MESSAGE reads in a thread."
  (if (eq (plist-get message :alone) t)
      niwa-collaboration-discussions-alone-text
    (plist-get message :text)))

(defun niwa-collaboration-discussions--thread (discussion)
  "Return DISCUSSION's messages as request text, newest kept within bounds."
  (let ((lines nil) (size 0))
    (catch 'full
      (dolist (message (reverse (niwa-collaboration-discussions--messages discussion)))
        (let ((line (format "- %s: %s" (plist-get message :actor)
                            (niwa-collaboration-discussions--message-text message))))
          (when (> (+ size (length line)) niwa-collaboration-discussions--thread-chars)
            (throw 'full nil))
          (setq size (+ size 1 (length line)))
          (push line lines))))
    (concat "Discussion:\n" (string-join lines "\n"))))

(defun niwa-collaboration-discussions-public (discussions)
  "Return DISCUSSIONS as guests receive them, without stored excerpts."
  (vconcat
   (mapcar
    (lambda (discussion)
      (append
       (list :id (plist-get discussion :id)
             :target (plist-get discussion :target)
             :quote (or (plist-get discussion :quote) "")
             :resolved (if (eq (plist-get discussion :resolved) t) t :json-false)
             :messages
             (vconcat
              (mapcar (lambda (message)
                        (append (list :id (plist-get message :id)
                                      :actor (plist-get message :actor)
                                      :text (or (plist-get message :text) ""))
                                (when (eq (plist-get message :alone) t)
                                  (list :alone t))))
                      (niwa-collaboration-discussions--messages discussion))))
       (when-let* ((by (plist-get discussion :resolvedBy))) (list :resolvedBy by))
       ;; The session that answers the discussion, for its chip.
       (when-let* ((session (plist-get discussion :session)))
         (list :session session :sessionName (plist-get discussion :sessionName)))))
    discussions)))


;;
;;; HTML artifacts

(defconst niwa-collaboration-discussions--artifact-limits
  '(:selector 1000 :label 512 :quote 512 :text 4000 :html 8000)
  "Character bounds on the guest-reported parts of an artifact target.")

(defun niwa-collaboration-discussions--artifact-string (value key &optional empty-ok)
  "Return VALUE when it is a string within KEY's bound.
Unless EMPTY-OK, an empty string is rejected too."
  (and (stringp value)
       (or empty-ok (not (string-empty-p value)))
       (<= (length value)
           (plist-get niwa-collaboration-discussions--artifact-limits key))
       value))

(defun niwa-collaboration-discussions--artifact-anchor (anchor)
  "Return a host-built copy of guest artifact ANCHOR, or nil when malformed.
Only known fields survive, each bounded, so the target the host stores and
retains in the transcript is data it checked rather than whatever the guest
sent."
  (when (and (consp anchor) (keywordp (car anchor)))
    (let ((selector (niwa-collaboration-discussions--artifact-string
                     (plist-get anchor :selector) :selector))
          (label (niwa-collaboration-discussions--artifact-string
                  (plist-get anchor :label) :label t))
          (kind (plist-get anchor :kind))
          (sig (plist-get anchor :sig))
          (quote-text (niwa-collaboration-discussions--artifact-string
                       (plist-get anchor :quote) :quote))
          (start (plist-get anchor :start))
          (region (plist-get anchor :region))
          (count (plist-get anchor :count)))
      (when selector
        (let ((clean (list :kind (if (member kind '("word" "selection" "box" "element"))
                                     kind "element")
                           :selector selector
                           :label (or label ""))))
          (when (and (consp sig) (keywordp (car sig))
                     (stringp (plist-get sig :tag))
                     (string-match-p "\\`[a-z][a-z0-9-]\\{0,31\\}\\'"
                                     (plist-get sig :tag)))
            (setq clean
                  (append clean
                          (list :sig
                                (append (list :tag (plist-get sig :tag))
                                        (when (and (stringp (plist-get sig :h))
                                                   (string-match-p
                                                    "\\`[0-9a-f]\\{16\\}\\'"
                                                    (plist-get sig :h)))
                                          (list :h (plist-get sig :h))))))))
          (when quote-text
            (setq clean (append clean
                                (list :quote quote-text
                                      :start (if (and (natnump start)
                                                      (<= start (* 16 1024 1024)))
                                                 start 0)))))
          (when (and (consp region) (keywordp (car region)))
            (let ((values (mapcar (lambda (key) (plist-get region key))
                                  '(:x0 :y0 :x1 :y1))))
              (when (and (cl-every (lambda (value)
                                     (and (numberp value) (<= 0 value 1)))
                                   values)
                         (< (nth 0 values) (nth 2 values))
                         (< (nth 1 values) (nth 3 values)))
                (setq clean
                      (append clean
                              (list :region (list :x0 (nth 0 values)
                                                  :y0 (nth 1 values)
                                                  :x1 (nth 2 values)
                                                  :y1 (nth 3 values)))
                              (when (and (natnump count) (<= count 10000))
                                (list :count count)))))))
          clean)))))

(defun niwa-collaboration-discussions--artifact-context (context)
  "Return the bounded text and HTML excerpt guest CONTEXT reports, or nil."
  (when (and (consp context) (keywordp (car context)))
    (let ((text (niwa-collaboration-discussions--artifact-string
                 (plist-get context :text) :text))
          (html (niwa-collaboration-discussions--artifact-string
                 (plist-get context :html) :html)))
      (when (or text html)
        (append (when text (list :text text)) (when html (list :html html)))))))

(defun niwa-collaboration-discussions--artifact-capture (record discussion)
  "Return the model-facing capture of DISCUSSION's target on artifact RECORD.
The artifact is named by file, not pasted in; the excerpts are what the
guest's browser rendered for the target."
  (let* ((target (plist-get discussion :target))
         (context (plist-get discussion :context))
         (region (plist-get target :region)))
    (string-join
     (delq nil
           (append
            (list
             (format "Artifact %s" (plist-get record :artifact))
             (when-let* ((path (plist-get record :artifact-path)))
               (format "File: %s" path)))
            (if (equal (plist-get target :kind) "whole")
                (list "Scope: the whole artifact")
              (list
               (unless (string-empty-p (or (plist-get target :label) ""))
                 (format "Target: %s" (plist-get target :label)))
               (format "Selector in the rendered page: %s" (plist-get target :selector))
               (when-let* ((quote-text (plist-get target :quote)))
                 (format "Selected text: %S" quote-text))
               (when region
                 (format "Box within the target: x %s-%s, y %s-%s of its width and height%s"
                         (plist-get region :x0) (plist-get region :x1)
                         (plist-get region :y0) (plist-get region :y1)
                         (if-let* ((count (plist-get target :count)))
                             (format " (%d element%s covered)" count
                                     (if (= count 1) "" "s"))
                           "")))
               (when-let* ((text (plist-get context :text)))
                 (concat "Target text:\n" text))
               (when-let* ((html (plist-get context :html)))
                 (concat "Target HTML as rendered (scripts may have changed it):\n"
                         "```html\n" html "\n```"))))))
     "\n")))

(defun niwa-collaboration-discussions--artifact-file (workspace id &optional name)
  "Return the discussions file of WORKSPACE's store artifact ID.
NAME selects another file in the artifact's protected state."
  (file-name-concat (niwa-artifact-store-bookkeeping-directory workspace id)
                    (or name "discussions.json")))

(defun niwa-collaboration-discussions--artifact-read (workspace id)
  "Return the stored discussions of WORKSPACE's artifact ID, oldest first.
A missing file is an artifact nobody has discussed yet."
  (let ((file (niwa-collaboration-discussions--artifact-file workspace id)))
    (when (file-exists-p file)
      (append (plist-get (niwa-shared-editing--parse
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string)))
                         :discussions)
              nil))))

(defun niwa-collaboration-discussions--artifact-write (workspace id discussions)
  "Store and publish DISCUSSIONS for WORKSPACE's artifact ID, within bounds.
The comment file of the earlier comment mechanism is removed on the way."
  (let ((public (niwa-collaboration-discussions-public discussions)))
    (when (> (string-bytes (niwa-shared-editing--json public))
             niwa-collaboration-discussions--max-bytes)
      (error "This artifact has too many discussions; resolve and shorten some first"))
    (niwa--write-file-atomically
     (niwa-collaboration-discussions--artifact-file workspace id)
     (niwa-shared-editing--json (list :discussions (vconcat discussions))))
    (let ((obsolete (niwa-collaboration-discussions--artifact-file
                     workspace id "comments.json")))
      (when (file-exists-p obsolete) (delete-file obsolete)))
    (niwa-collaboration-discussions--publish
     workspace (concat "artifact:" id) public)))

(defun niwa-collaboration-discussions--publish (workspace item public)
  "Tell every guest of WORKSPACE that ITEM now has PUBLIC discussions.
Discussions belong to the item, so every room and the lobby see them."
  (let ((frame (list :t "discussions" :item item :discussions public)))
    (dolist (room (niwa-collaboration--workspace-rooms workspace))
      (niwa-collaboration--broadcast room frame))))


;;
;;; Items

(defun niwa-collaboration-discussions--item (room guest id)
  "Return the item frame ID names for GUEST in ROOM, or signal.
ID is \"artifact:STORE-ID\" or a card published to GUEST.  The result names
the item's kind, store identity, item identity, title and record.  A
whiteboard's or document's item identity is its store id, as its room
conversation knows it; an HTML artifact's is \"artifact:STORE-ID\"."
  (let* ((record (niwa-collaboration--artifact-target room guest id))
         (store (plist-get record :store))
         (workspace (niwa-collaboration--room-workspace room)))
    (unless (and record store (not (plist-get record :missing)))
      (error "This artifact is not published"))
    (if (plist-get record :item)
        (let ((meta (niwa-artifact-store-meta workspace store)))
          (list :kind (symbol-name (plist-get meta :kind)) :store store :item store
                :title (or (plist-get meta :title) store) :record record
                :workspace workspace :shared t))
      (unless (equal (niwa-collaboration--artifact-mime (plist-get record :artifact))
                     "text/html")
        (error "Only HTML artifacts, whiteboards and documents take discussions"))
      (unless (equal (plist-get record :artifact)
                     (plist-get (niwa-collaboration--artifact-target
                                 room guest (concat "artifact:" store))
                                :artifact))
        (error "Discussions are kept on an artifact's main file"))
      (list :kind "artifact" :store store :item (concat "artifact:" store)
            :title (plist-get record :artifact) :record record
            :workspace workspace))))

(defun niwa-collaboration-discussions--load (item)
  "Return ITEM's stored discussions."
  (niwa-collaboration-discussions--artifact-read
   (plist-get item :workspace) (plist-get item :store)))

(defun niwa-collaboration-discussions--save (item discussions)
  "Store and publish ITEM's DISCUSSIONS."
  (niwa-collaboration-discussions--artifact-write
   (plist-get item :workspace) (plist-get item :store) discussions))

(defun niwa-collaboration-discussions--replace (discussions discussion)
  "Return DISCUSSIONS with DISCUSSION in place of the one sharing its id."
  (mapcar (lambda (d) (if (equal (plist-get d :id) (plist-get discussion :id)) discussion d))
          discussions))


;;
;;; Assistant requests

(defun niwa-collaboration-discussions--question-known-p (data-buffer item question-id)
  "Return non-nil when QUESTION-ID is already queued or delivered in DATA-BUFFER.
ITEM is the attribution's item identity.  A retried send then succeeds
without queueing the message twice, also when the request was delivered
before compaction moved it into an archived segment.  Reading archives costs
time proportional to the session's age, so callers ask only when the
request may be a retry."
  (let ((known (lambda (shared)
                 (and (plist-get shared :discussionId)
                      (equal (plist-get shared :itemId) item)
                      (equal (plist-get shared :questionId) question-id)))))
    (with-current-buffer data-buffer
      (or (cl-some (lambda (entry) (funcall known (plist-get entry :shared-question)))
                   (niwa-session-pending-follow-ups niwa--session))
          (cl-some (lambda (attribution)
                     (funcall known (plist-get (cdr attribution) :shared)))
                   (niwa-transcript-audit-guest-prompts))
          (niwa-shared-conversation-archived-question-p
           niwa--session item question-id)))))

(defun niwa-collaboration-discussions-answering-buffer (room item thread-session)
  "Return the data buffer of the session that answers about store ITEM.
A room answers in its own session only: its links speak for that session,
never another.  The lobby, which may open any session, keeps a discussion
with THREAD-SESSION, the session answering it, or else uses the item's
dedicated session."
  (let ((workspace (niwa-collaboration--room-workspace room)))
    (condition-case nil
        (cond
         ((plist-get room :session)
          (niwa-collaboration--room-data-buffer room))
         ((and thread-session
               (niwa-artifact-store-session-buffer workspace thread-session)))
         (t (niwa-artifact-store-conversation workspace item)))
      (inhibited-interaction
       (error "%s" niwa-collaboration-needs-host-message)))))

(defun niwa-collaboration-discussions--target-label (discussion)
  "Return the label naming DISCUSSION's target in a folded request."
  (let ((target (plist-get discussion :target)))
    (if (equal (plist-get target :kind) "whole") "Whole item"
      (let ((label (or (and (not (string-empty-p (or (plist-get target :label) "")))
                            (plist-get target :label))
                       (car (split-string (or (plist-get discussion :quote) "") "\n" t))
                       "Selection")))
        (truncate-string-to-width label 120 nil nil "…")))))

(defun niwa-collaboration-discussions--send
    (room guest item discussion message question-id images &optional capture version fresh)
  "Queue MESSAGE of DISCUSSION about ITEM for the answering session.
QUESTION-ID identifies the request; IMAGES are the sender's attachment
frames.  CAPTURE is what the editing host captured of a whiteboard's or
document's target: (:prompt TEXT :png BASE64 :revision N :quote Q); an HTML
artifact's target is captured here.  VERSION, when non-nil, is the message
id a new request must still have as MESSAGE's; a known request is answered
as queued regardless.  FRESH means this action just stored the message
QUESTION-ID names, so its request cannot be known yet.  Return the reply
fields: the answering session and queue state."
  (let* ((kind (plist-get item :kind))
         (data-buffer (niwa-collaboration-discussions-answering-buffer
                       room (plist-get item :store) (plist-get discussion :session)))
         (session (buffer-local-value 'niwa--session data-buffer))
         (answering (list :session (niwa-session-session-id session)
                          :sessionName (niwa-session-name session))))
    (niwa-artifact-store-attach session (plist-get item :store) data-buffer)
    (if (and (not fresh)
             (niwa-collaboration-discussions--question-known-p
              data-buffer (plist-get item :item) question-id))
        (append (list :queued t :questionId question-id) answering)
      (when (and version (not (equal version (plist-get message :id))))
        (error "Discussion changed.  Review it before sending again"))
      (let* ((alone (eq (plist-get message :alone) t))
             (text (if alone
                       (niwa-collaboration-discussions-alone-request kind)
                     (plist-get message :text)))
             (general (niwa-collaboration-discussions--general-p discussion))
             (input (concat text niwa-collaboration-discussions-snapshot-heading
                            (if capture (plist-get capture :prompt)
                              (niwa-collaboration-discussions--artifact-capture
                               (plist-get item :record) discussion))
                            (unless general
                              (concat "\n" (niwa-collaboration-discussions--thread
                                            discussion)))))
             (shared (append (list :kind kind
                                   :itemId (plist-get item :item)
                                   :title (plist-get item :title)
                                   :questionId question-id
                                   :discussionId (plist-get discussion :id)
                                   :messageId (plist-get message :id)
                                   :scope (if general "whole" "selection")
                                   :targetLabel (niwa-collaboration-discussions--target-label
                                                 discussion)
                                   :text text)
                             (when alone (list :alone t))
                             (when capture
                               (list :revision (plist-get capture :revision)
                                     :quote (truncate-string-to-width
                                             (or (plist-get capture :quote) "") 2000
                                             nil nil "…")))))
             paths queued)
        (unwind-protect
            (progn
              ;; Acquire both sets inside the cleanup boundary: saving the
              ;; sender's files can fail after the board snapshot was saved.
              (when-let* ((png (plist-get capture :png)))
                (setq paths (niwa-collaboration--save-guest-files
                             data-buffer (list (list :mime "image/png" :data png)))))
              (setq paths (append paths (niwa-collaboration--save-guest-files
                                        data-buffer images)))
              (setq queued
                  (niwa-view-enqueue-external-follow-up
                   data-buffer input
                   :guest-name (plist-get guest :name)
                   :guest-id (plist-get guest :guest-id)
                   :guest-role (niwa-collaboration--guest-role guest)
                   :paths paths
                   :shared-question shared)))
          ;; Files of a message that was not queued leave with it.
          (unless queued
            (dolist (path paths) (ignore-errors (delete-file path)))))
        (unless queued (error "The session cannot accept a message right now"))
        (append (list :queued t :questionId question-id)
                answering
                (when-let* (((eq data-buffer (plist-get room :data-buffer)))
                            (position (niwa-collaboration--queue-position
                                       room queued)))
                  (list :position position)))))))

(defun niwa-collaboration-discussions--answered (discussions id answer)
  "Return DISCUSSIONS with discussion ID recording the session ANSWER names.
Return nil when nothing changes."
  (when-let* ((discussion (niwa-collaboration-discussions--find discussions id))
              (session (plist-get answer :session))
              ((not (and (equal session (plist-get discussion :session))
                         (equal (plist-get answer :sessionName)
                                (plist-get discussion :sessionName))))))
    (niwa-collaboration-discussions--replace
     discussions
     (plist-put (plist-put (copy-sequence discussion) :session session)
                :sessionName (plist-get answer :sessionName)))))


;;
;;; Whiteboards and documents

;; Their discussions live in the item, on the editing host, which checks each
;; target against the item's content and keeps the anchors current.

(defun niwa-collaboration-discussions--shared-call (item request authorize callback)
  "Run editing REQUEST on ITEM's host and call CALLBACK with its result.
AUTHORIZE, when non-nil, must still permit the operation.  CALLBACK
receives the result plist, or (:error MESSAGE)."
  (niwa-shared-editing-call
   (plist-get item :workspace)
   (append (list :id (plist-get item :store)) request)
   (lambda (reply)
     (funcall callback (if (plist-get reply :error)
                           (list :error (plist-get reply :error))
                         (plist-get reply :result))))
   authorize))

(defun niwa-collaboration-discussions--shared-target (target)
  "Return the whiteboard or document target guest TARGET names, or signal."
  (pcase (plist-get target :kind)
    ("whole" (list :kind "whole"))
    ("passage" (list :kind "passage" :range (plist-get target :range)))
    ("objects" (append (list :kind "objects"
                             :selection (or (plist-get target :selection) []))
                       (when (plist-get target :region)
                         (list :region (plist-get target :region)))))
    (_ (error "The discussed part could not be identified"))))

(defun niwa-collaboration-discussions--shared-capture (target)
  "Return the question read fields that capture TARGET as it now stands."
  (pcase (plist-get target :kind)
    ("whole" (list :whole t))
    ("passage" (list :range (plist-get target :range)))
    (_ (append (list :selection (plist-get target :selection))
               (when (plist-get target :region)
                 (list :region (plist-get target :region)))))))

(defun niwa-collaboration-discussions--shared-ask
    (room guest item id question-id version target expected images authorize callback
          &optional fresh)
  "Send ITEM's discussion ID at VERSION to the assistant, then call CALLBACK.
QUESTION-ID identifies the request.  TARGET names the discussed part as the
sender reviewed it and EXPECTED that content's hash; a discussion about the
whole item has none.  IMAGES are the sender's attachment frames.
AUTHORIZE, when non-nil, must still permit the operation.  FRESH means the
host just stored the message QUESTION-ID names."
  (let ((room-buffer (and (plist-get room :session)
                          (niwa-collaboration--room-data-buffer room)))
        known)
    ;; This may run in the host's reply, outside the caller's refusal handler:
    ;; history that cannot settle a retry is the sender's refusal.
    (condition-case err
        (setq known (and room-buffer (not fresh)
                         (niwa-collaboration-discussions--question-known-p
                          room-buffer (plist-get item :item) question-id)))
      (error (setq known (list :error (error-message-string err)))))
    (cond
     ((plist-get known :error) (funcall callback known))
     (known (funcall callback (list :discussionId id :queued t :questionId question-id)))
     (t
      (niwa-collaboration-discussions--shared-call
       item (append (list :action "read" :question t :image t :imageMax 1024
                          :discussionId id :version version)
                    (niwa-collaboration-discussions--shared-capture target)
                    (when expected (list :expected expected)))
       authorize
       (lambda (result)
         (funcall
          callback
          (condition-case err
              (if (plist-get result :error) result
                (let* ((discussions (append (plist-get result :discussions) nil))
                       (discussion (or (niwa-collaboration-discussions--find discussions id)
                                       (error "That discussion no longer exists")))
                       (message (car (last (niwa-collaboration-discussions--messages
                                            discussion))))
                       (answer (niwa-collaboration-discussions--send
                                room guest item discussion message question-id images
                                (list :prompt (plist-get result :prompt)
                                      :png (plist-get result :png)
                                      :revision (plist-get result :revision)
                                      :quote (plist-get result :quote))
                                nil fresh)))
                  (when (niwa-collaboration-discussions--answered discussions id answer)
                    (niwa-collaboration-discussions--shared-call
                     item (list :action "discussion-answered" :discussionId id
                                :session (plist-get answer :session)
                                :sessionName (plist-get answer :sessionName)
                                :opId (niwa-collaboration-discussions--op-id)
                                :actor (niwa-collaboration-discussions--actor guest))
                     authorize #'ignore))
                  (append (list :discussionId id) answer)))
            (error (list :error (error-message-string err)))))))))))

(defun niwa-collaboration-discussions--shared-reviewed (args)
  "Return the target ARGS name for an existing discussion, as reviewed.
The sender names the parts of the discussion's target that still exist;
the host checks them against the stored target."
  (cond
   ((eq (plist-get args :whole) t) (list :kind "whole"))
   ((plist-get args :range) (list :kind "passage" :range (plist-get args :range)))
   (t (append (list :kind "objects" :selection (or (plist-get args :selection) []))
              (when (plist-get args :region) (list :region (plist-get args :region)))))))

(defun niwa-collaboration-discussions--shared-perform
    (room guest item args authorize callback)
  "Perform discussion ARGS on whiteboard or document ITEM; call CALLBACK.
AUTHORIZE, when non-nil, must still permit the operation."
  (let* ((action (plist-get args :action))
         (actor (niwa-collaboration-discussions--actor guest))
         (to-assistant (eq (plist-get args :toAssistant) t))
         (then-ask
          (lambda (id message-id target)
            (lambda (result)
              (cond
               ((plist-get result :error) (funcall callback result))
               ((not to-assistant)
                (funcall callback (list :discussionId id :messageId message-id
                                        :discussions (plist-get result :discussions))))
               (t (niwa-collaboration-discussions--shared-ask
                   room guest item id message-id message-id target
                   (plist-get args :expected) (plist-get args :images) authorize
                   (lambda (answer)
                     (funcall callback
                              (if (plist-get answer :error) answer
                                (append (list :messageId message-id) answer))))
                   ;; The host replays an operation it already recorded.
                   (not (eq (plist-get result :replayed) t)))))))))
    (pcase action
      ("list"
       (niwa-collaboration--store-read guest)
       (niwa-collaboration-discussions--shared-call
        item (list :action "read") authorize
        (lambda (result)
          (funcall callback
                   (if (plist-get result :error) result
                     (list :item (plist-get item :item)
                           :discussions (or (plist-get result :discussions) [])))))))
      ("post"
       (let* ((target (niwa-collaboration-discussions--shared-target
                       (plist-get args :target)))
              (whole (equal (plist-get target :kind) "whole"))
              (message-id (niwa-collaboration-discussions--identity
                           (plist-get args :messageId) "message"))
              (id (if whole niwa-collaboration-discussions-general-id
                    (niwa-collaboration-discussions--identity
                     (plist-get args :discussionId) "discussion")))
              (text (niwa-collaboration-discussions--text (plist-get args :text)))
              (alone (and (not text) to-assistant (not whole))))
         (unless (or text alone)
           (error (if whole "A message is required"
                    "A message is required, or send the selection to the assistant")))
         (niwa-collaboration-discussions--shared-call
          item (append (list :action "discussion-post" :discussionId id :messageId message-id
                             :target target :text (or text "") :alone (if alone t :json-false)
                             :opId message-id :actor actor)
                       (when (plist-get args :expected)
                         (list :expected (plist-get args :expected))))
          authorize (funcall then-ask id message-id target))))
      ("reply"
       (let ((id (or (plist-get args :discussionId) (error "Name the discussion to reply to")))
             (message-id (niwa-collaboration-discussions--identity
                          (plist-get args :messageId) "message"))
             (text (or (niwa-collaboration-discussions--text (plist-get args :text))
                       (error "A message is required"))))
         (niwa-collaboration-discussions--shared-call
          item (list :action "discussion-reply" :discussionId id :messageId message-id
                     :text text :opId message-id :actor actor)
          authorize
          ;; A reply sends the discussion's target as the sender reviewed it.
          (funcall then-ask id message-id
                   (niwa-collaboration-discussions--shared-reviewed args)))))
      ("resolve"
       (let ((id (plist-get args :discussionId))
             (resolved (if (eq (plist-get args :resolved) t) t :json-false)))
         (niwa-collaboration-discussions--shared-call
          item (list :action "discussion-resolve" :discussionId id :resolved resolved
                     :opId (niwa-collaboration-discussions--op-id) :actor actor)
          authorize
          (lambda (result)
            (funcall callback
                     (if (plist-get result :error) result
                       (list :discussionId id :resolved resolved
                             :discussions (plist-get result :discussions))))))))
      ("ask"
       (niwa-collaboration-discussions--shared-ask
        room guest item (plist-get args :discussionId)
        (niwa-collaboration-discussions--identity (plist-get args :questionId) "question")
        (plist-get args :version)
        (niwa-collaboration-discussions--shared-reviewed args)
        (plist-get args :expected) nil authorize callback)))))


;;
;;; Actions

(defun niwa-collaboration-discussions--op-id ()
  "Return a fresh operation identity for a host-made discussion change."
  (substring (secure-hash 'sha256 (format "%s%s" (current-time) (random t))) 0 40))

(defun niwa-collaboration-discussions--append (discussion message)
  "Return DISCUSSION with MESSAGE added, or DISCUSSION when it already has it."
  (let ((messages (niwa-collaboration-discussions--messages discussion)))
    (if (cl-find (plist-get message :id) messages
                 :key (lambda (m) (plist-get m :id)) :test #'equal)
        discussion
      (when (eq (plist-get discussion :resolved) t)
        (error "Reopen this discussion before adding to it"))
      (when (>= (length messages) niwa-collaboration-discussions--max-messages)
        (error "This discussion has reached its %d message limit"
               niwa-collaboration-discussions--max-messages))
      (plist-put (copy-sequence discussion) :messages
                 (vconcat messages (list message))))))

(defun niwa-collaboration-discussions--message (guest id text alone)
  "Return GUEST's message ID with TEXT, or sent alone when ALONE."
  (append (list :id id :actor (niwa-collaboration-discussions--actor guest)
                :text (or text "") :created (format-time-string "%FT%TZ" nil t))
          (when alone (list :alone t))))

(defun niwa-collaboration-discussions--target (item target)
  "Return ITEM's host-checked copy of guest TARGET, or signal."
  (cond
   ((and (consp target) (equal (plist-get target :kind) "whole"))
    (list :kind "whole"))
   ((equal (plist-get item :kind) "artifact")
    (or (niwa-collaboration-discussions--artifact-anchor target)
        (error "The discussed part could not be identified")))
   (t (error "Unknown discussion target"))))

(defun niwa-collaboration-discussions--post (room guest item args)
  "Start or continue a discussion of ITEM as ARGS ask; return reply fields.
A whole-item target adds to the item's general discussion."
  (let* ((target (niwa-collaboration-discussions--target item (plist-get args :target)))
         (whole (equal (plist-get target :kind) "whole"))
         (to-assistant (eq (plist-get args :toAssistant) t))
         (message-id (niwa-collaboration-discussions--identity
                      (plist-get args :messageId) "message"))
         (id (if whole niwa-collaboration-discussions-general-id
               (niwa-collaboration-discussions--identity
                (plist-get args :discussionId) "discussion")))
         (text (niwa-collaboration-discussions--text (plist-get args :text)))
         (alone (and (not text) to-assistant (not whole))))
    (unless (or text alone)
      (error (if whole "A message is required"
               "A message is required, or send the selection to the assistant")))
    (let* ((discussions (niwa-collaboration-discussions--load item))
           (existing (niwa-collaboration-discussions--find discussions id))
           (message (niwa-collaboration-discussions--message guest message-id text alone))
           (discussion
            (cond
             (existing (niwa-collaboration-discussions--append existing message))
             (t
              (unless whole
                (when (>= (cl-count-if-not #'niwa-collaboration-discussions--general-p
                                           discussions)
                          niwa-collaboration-discussions--max-discussions)
                  (error "This item has reached its %d discussion limit"
                         niwa-collaboration-discussions--max-discussions)))
              (append (list :id id :target target
                            :quote (if whole "" (or (plist-get target :quote)
                                                    (plist-get target :label) ""))
                            :messages (vector message)
                            :resolved :json-false
                            :created (plist-get message :created))
                      (when-let* (((not whole))
                                  (context (niwa-collaboration-discussions--artifact-context
                                            (plist-get args :context))))
                        (list :context context)))))))
      (unless (eq discussion existing)
        (setq discussions (if existing
                              (niwa-collaboration-discussions--replace discussions discussion)
                            (append discussions (list discussion))))
        (niwa-collaboration-discussions--save item discussions))
      (append (list :discussionId id :messageId message-id)
              (when to-assistant
                (niwa-collaboration-discussions--ask-now
                 room guest item discussions id message-id
                 (plist-get args :images) message-id (not (eq discussion existing))))))))

(defun niwa-collaboration-discussions--ask-now
    (room guest item discussions id question-id images &optional version fresh)
  "Send discussion ID of ITEM's DISCUSSIONS as it stands; return reply fields.
QUESTION-ID identifies the request; the discussion's latest message is its
request.  IMAGES are the sender's attachment frames.  VERSION, when non-nil,
is the message the sender asks about, which must still be the latest: a
retried message never sends a newer one under its identity.  FRESH means
this action just stored that message."
  (let* ((discussion (or (niwa-collaboration-discussions--find discussions id)
                         (error "That discussion no longer exists")))
         (message (car (last (niwa-collaboration-discussions--messages discussion))))
         (answer (progn
                   (when (eq (plist-get discussion :resolved) t)
                     (error "Reopen this discussion before asking the assistant"))
                   (unless message (error "This discussion has no message to send"))
                   (niwa-collaboration-discussions--send
                    room guest item discussion message question-id images nil version fresh)))
         (answered (niwa-collaboration-discussions--answered discussions id answer)))
    (when answered (niwa-collaboration-discussions--save item answered))
    answer))

(defun niwa-collaboration-discussions--reply (room guest item args)
  "Add a message to an existing discussion of ITEM as ARGS ask."
  (let* ((id (or (plist-get args :discussionId) (error "Name the discussion to reply to")))
         (message-id (niwa-collaboration-discussions--identity
                      (plist-get args :messageId) "message"))
         (text (or (niwa-collaboration-discussions--text (plist-get args :text))
                   (error "A message is required")))
         (discussions (niwa-collaboration-discussions--load item))
         (discussion (or (niwa-collaboration-discussions--find discussions id)
                         (error "That discussion no longer exists")))
         (updated (niwa-collaboration-discussions--append
                   discussion (niwa-collaboration-discussions--message
                               guest message-id text nil))))
    (unless (eq updated discussion)
      (setq discussions (niwa-collaboration-discussions--replace discussions updated))
      (niwa-collaboration-discussions--save item discussions))
    (append (list :discussionId id :messageId message-id)
            (when (eq (plist-get args :toAssistant) t)
              (niwa-collaboration-discussions--ask-now
               room guest item discussions id message-id (plist-get args :images)
               message-id (not (eq updated discussion)))))))

(defun niwa-collaboration-discussions--resolve (item args actor)
  "Resolve or reopen ITEM's discussion as ARGS ask, attributed to ACTOR."
  (let* ((id (plist-get args :discussionId))
         (discussions (niwa-collaboration-discussions--load item))
         (discussion (or (niwa-collaboration-discussions--find discussions id)
                         (error "That discussion no longer exists")))
         (resolved (if (eq (plist-get args :resolved) t) t :json-false)))
    (when (niwa-collaboration-discussions--general-p discussion)
      (error "The discussion about the whole item stays open"))
    (unless (eq (plist-get discussion :resolved) resolved)
      (niwa-collaboration-discussions--save
       item (niwa-collaboration-discussions--replace
             discussions
             (plist-put (plist-put (copy-sequence discussion) :resolved resolved)
                        :resolvedBy (and (eq resolved t) actor)))))
    (list :discussionId id :resolved resolved)))

(defun niwa-collaboration-discussions-perform
    (room guest args callback &optional authorize)
  "Perform GUEST's discussion ARGS in ROOM and call CALLBACK with the reply.
The reply is a plist of fields for the sender, or (:error MESSAGE) when the
action is refused.  CALLBACK is called exactly once: at once for an HTML
artifact, once its editing host answers for a whiteboard or document.
The room must remain live and the link must still permit the action when
that host runs it.  AUTHORIZE, when non-nil, adds the caller's continuing
authority check."
  (let ((answered nil))
    (cl-flet ((reply (value)
                (unless answered
                  (setq answered t)
                  (funcall callback value))))
      (condition-case err
          (let* ((action (plist-get args :action))
                 (item (progn
                         (unless (member action '("list" "history" "post" "reply" "resolve" "ask"))
                           (error "Unknown discussion action"))
                         (niwa-collaboration-discussions--item
                          room guest (plist-get args :item))))
                 (caller-authorize authorize)
                 (authorize
                  (lambda ()
                    (and (memq room (niwa-collaboration--workspace-rooms
                                     (plist-get item :workspace)))
                         (or (member action '("list" "history")) (plist-get guest :writable))
                         (or (null caller-authorize) (funcall caller-authorize))))))
            (unless (or (member action '("list" "history")) (plist-get guest :writable))
              (error "This link can view the item but not discuss it"))
            (unless (funcall authorize) (error "Discussion authority ended"))
            (if (equal action "history")
                (reply (niwa-collaboration-artifact-history room guest item args))
              (if (plist-get item :shared)
                  (niwa-collaboration-discussions--shared-perform
                   room guest item args authorize #'reply)
                (reply
                 (pcase action
                   ("list"
                    (niwa-collaboration--store-read guest)
                    (list :item (plist-get item :item)
                          :discussions (niwa-collaboration-discussions-public
                                        (niwa-collaboration-discussions--load item))))
                   ("post" (niwa-collaboration-discussions--post room guest item args))
                   ("reply" (niwa-collaboration-discussions--reply room guest item args))
                   ("resolve" (niwa-collaboration-discussions--resolve
                               item args (niwa-collaboration-discussions--actor guest)))
                   ("ask"
                    (let ((id (plist-get args :discussionId)))
                      (append (list :discussionId id)
                              (niwa-collaboration-discussions--ask-now
                               room guest item (niwa-collaboration-discussions--load item) id
                               (niwa-collaboration-discussions--identity
                                (plist-get args :questionId) "question")
                               nil)))))))))
        (error (reply (list :error (error-message-string err))))))))

(defun niwa-collaboration-discussions-handle (room peer frame)
  "Perform guest PEER's discussion FRAME in ROOM and answer the sender.
Every refusal is answered to the sender; none reaches the room's frame
handler, whose failure would stop the share for everyone."
  (let ((guest (niwa-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (niwa-collaboration-discussions-perform
       room guest frame
       (lambda (reply)
         (niwa-collaboration--transport-send
          (plist-get room :transport) peer
          (append (list :t "discussion" :reqId req-id) reply)))
       ;; A whiteboard's or document's host runs later: the guest must still
       ;; be in the room.
       (lambda () (eq guest (niwa-collaboration--guest room peer)))))))


;;
;;; Selections sent alone

(defun niwa-collaboration-discussions-turn-settled (outcome)
  "Resolve the selection-alone discussion the current buffer's turn served.
OUTCOME is the turn's terminal outcome; only a completed turn resolves.  A
selection sent alone is a task rather than a conversation, so once it is
done nothing remains to discuss -- unless someone wrote in the discussion
meanwhile, which keeps it open.  The turn's attribution is consumed either
way."
  (when-let* ((attribution (and (boundp 'niwa-view--turn-guest-attribution)
                                niwa-view--turn-guest-attribution))
              (shared (prog1 (plist-get attribution :shared)
                        (setq niwa-view--turn-guest-attribution nil)))
              ((eq outcome 'success))
              ((eq (plist-get shared :alone) t))
              (item (plist-get shared :itemId))
              (session (bound-and-true-p niwa--session)))
    (let* ((workspace (niwa-session-workspace session))
           (scope (niwa-diagnostic-scope workspace))
           (id (plist-get shared :discussionId))
           (report (lambda (text)
                     (niwa-diagnostic-message scope "Could not resolve the discussion: %s" text))))
      (condition-case err
          (if (not (string-prefix-p "artifact:" item))
              ;; A whiteboard's or document's discussion lives on its host.
              (niwa-shared-editing-call
               workspace (list :action "discussion-resolve" :id item :discussionId id
                               :resolved t :actor "Assistant" :ifAlone t
                               :opId (niwa-collaboration-discussions--op-id))
               (lambda (reply)
                 (when-let* ((text (plist-get reply :error)))
                   (funcall report text))))
            (let* ((store (substring item 9))
                   (discussions (niwa-collaboration-discussions--artifact-read
                                 workspace store))
                   (discussion (niwa-collaboration-discussions--find discussions id)))
              (when (and discussion (not (eq (plist-get discussion :resolved) t))
                         (cl-every (lambda (message) (eq (plist-get message :alone) t))
                                   (niwa-collaboration-discussions--messages discussion)))
                (niwa-collaboration-discussions--artifact-write
                 workspace store
                 (niwa-collaboration-discussions--replace
                  discussions
                  (plist-put (plist-put (copy-sequence discussion) :resolved t)
                             :resolvedBy "Assistant"))))))
        (error (funcall report (error-message-string err)))))))

(add-hook 'niwa-turn-settled-functions #'niwa-collaboration-discussions-turn-settled)

(provide 'niwa-collaboration-discussions)
;;; niwa-collaboration-discussions.el ends here
