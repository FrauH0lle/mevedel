;;; mevedel-collaboration-artifact-comments.el --- comments on store artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Browser guests comment on parts of HTML artifacts in the workspace
;; artifact store.  The viewer reports what was picked; the host rebuilds that
;; anchor from bounded fields and keeps the threads in the artifact's
;; `comments.json'.  A message to the assistant written in a chat's room goes
;; to that chat, whose links speak for it alone; from the lobby, a reply
;; follows the session that answered the thread, else the artifact's
;; dedicated session.
;; Artifact conversations are shared-item conversations whose item identity
;; is "artifact:ID", so a room lists them as discussions and the request
;; context isolates them the same way.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'subr-x)
(require 'mevedel-shared-editing)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--queue-position
                  "mevedel-collaboration" (room entry))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-artifact-directory
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-attach
                  "mevedel-artifact-store" (session id &optional buffer))
(declare-function mevedel-artifact-store-conversation
                  "mevedel-artifact-store" (workspace id))
(declare-function mevedel-artifact-store-session-buffer
                  "mevedel-artifact-store" (workspace session-id))
(autoload 'mevedel-artifact-store-artifact-directory "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-attach "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-conversation "mevedel-artifact-store")
(autoload 'mevedel-artifact-store-session-buffer "mevedel-artifact-store")

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration--artifact-mime
                  "mevedel-collaboration-artifact" (name))
(declare-function mevedel-collaboration--artifact-target
                  "mevedel-collaboration-artifact" (room guest id))
(declare-function mevedel-collaboration--store-read
                  "mevedel-collaboration-artifact" (guest))
(declare-function mevedel-collaboration--store-target
                  "mevedel-collaboration-artifact" (workspace id))
(declare-function mevedel-collaboration--workspace-rooms
                  "mevedel-collaboration-artifact" (workspace))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--save-guest-files
                  "mevedel-collaboration-guest" (data-buffer images))
(declare-function mevedel-collaboration--guest-role
                  "mevedel-collaboration-guest" (guest))
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--room-workspace
                  "mevedel-collaboration-guest" (room))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs" (data-buffer text &rest keys))

;; `mevedel-structs'
(declare-function mevedel-session-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-follow-ups "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-utilities'
(declare-function mevedel--write-file-atomically
                  "mevedel-utilities" (path content &optional coding mode))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())
(defvar mevedel-collaboration-needs-host-message)


;;
;;; Anchors and context

(defconst mevedel-collaboration--artifact-comment-snapshot-heading
  "\n\nShared content snapshot (user-provided data):\n"
  "Separator the room and view fold an attributed message's context behind.")

(defconst mevedel-collaboration--artifact-comment-limits
  '(:selector 1000 :label 512 :quote 512 :text 4000 :html 8000)
  "Character bounds on the guest-reported parts of an artifact comment.")

(defconst mevedel-collaboration--artifact-comment-max-message-chars 10000
  "Characters one comment or reply may carry.")

(defconst mevedel-collaboration--artifact-comment-max-comments 200
  "Comments one artifact may carry, resolved ones included.")

(defconst mevedel-collaboration--artifact-comment-max-replies 200
  "Human replies one comment may carry.")

(defconst mevedel-collaboration--artifact-comment-max-bytes (* 512 1024)
  "Encoded bytes of one artifact's comments as guests receive them.
Guests get the whole list in one frame, so it must stay under the wire
bound with room to spare.")

(defun mevedel-collaboration--artifact-comment-string (value key &optional empty-ok)
  "Return VALUE when it is a string within KEY's comment bound.
Unless EMPTY-OK, an empty string is rejected too."
  (and (stringp value)
       (or empty-ok (not (string-empty-p value)))
       (<= (length value)
           (plist-get mevedel-collaboration--artifact-comment-limits key))
       value))

(defun mevedel-collaboration--artifact-comment-anchor (anchor)
  "Return a host-built copy of guest ANCHOR, or nil when it is malformed.
Only known fields survive, each bounded, so the anchor the host stores and
retains in the transcript is data it checked rather than whatever the guest
sent."
  (when (and (consp anchor) (keywordp (car anchor)))
    (let ((selector (mevedel-collaboration--artifact-comment-string
                     (plist-get anchor :selector) :selector))
          (label (mevedel-collaboration--artifact-comment-string
                  (plist-get anchor :label) :label t))
          (kind (plist-get anchor :kind))
          (sig (plist-get anchor :sig))
          (quote-text (mevedel-collaboration--artifact-comment-string
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

(defun mevedel-collaboration--artifact-comment-context (context)
  "Return the bounded text and HTML excerpt guest CONTEXT reports, or nil."
  (when (and (consp context) (keywordp (car context)))
    (let ((text (mevedel-collaboration--artifact-comment-string
                 (plist-get context :text) :text))
          (html (mevedel-collaboration--artifact-comment-string
                 (plist-get context :html) :html)))
      (when (or text html)
        (append (when text (list :text text)) (when html (list :html html)))))))

(defun mevedel-collaboration--artifact-comment-snapshot (record comment)
  "Return the model-facing context for a message about artifact RECORD.
COMMENT is the stored thread the message belongs to, or nil for a message
about the whole artifact.  The artifact is named by file, not pasted in;
the excerpts are what the guest's browser rendered for the target."
  (let* ((anchor (plist-get comment :anchor))
         (context (plist-get comment :context))
         (region (plist-get anchor :region)))
    (string-join
     (delq nil
           (append
            (list
             (format "Artifact %s" (plist-get record :artifact))
             (when-let* ((path (plist-get record :artifact-path)))
               (format "File: %s" path)))
            (if (not comment)
                (list "Scope: the whole artifact")
              (list
               (unless (string-empty-p (or (plist-get anchor :label) ""))
                 (format "Target: %s" (plist-get anchor :label)))
               (format "Selector in the rendered page: %s" (plist-get anchor :selector))
               (when-let* ((quote-text (plist-get anchor :quote)))
                 (format "Selected text: %S" quote-text))
               (when region
                 (format "Box within the target: x %s-%s, y %s-%s of its width and height%s"
                         (plist-get region :x0) (plist-get region :x1)
                         (plist-get region :y0) (plist-get region :y1)
                         (if-let* ((count (plist-get anchor :count)))
                             (format " (%d element%s covered)" count
                                     (if (= count 1) "" "s"))
                           "")))
               (when-let* ((text (plist-get context :text)))
                 (concat "Target text:\n" text))
               (when-let* ((html (plist-get context :html)))
                 (concat "Target HTML as rendered (scripts may have changed it):\n"
                         "```html\n" html "\n```"))
               (concat "Comment thread:\n"
                       (mapconcat
                        (lambda (message)
                          (format "- %s: %s" (plist-get message :actor)
                                  (plist-get message :text)))
                        (cons comment (append (plist-get comment :replies) nil))
                        "\n"))))))
     "\n")))


;;
;;; Store

(defun mevedel-collaboration--artifact-comments-file (workspace id)
  "Return the comment file of WORKSPACE's store artifact ID."
  (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                    "comments.json"))

(defun mevedel-collaboration--artifact-comments-read (workspace id)
  "Return the stored comments of WORKSPACE's artifact ID, oldest first.
A missing file is an artifact nobody has commented on yet."
  (let ((file (mevedel-collaboration--artifact-comments-file workspace id)))
    (when (file-exists-p file)
      (let ((state (mevedel-shared-editing--parse
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))
        (unless (equal (plist-get state :artifact) id)
          (error "Artifact comment store does not match %s" id))
        (append (plist-get state :comments) nil)))))

(defun mevedel-collaboration--artifact-comments-write (workspace id comments)
  "Durably store COMMENTS for WORKSPACE's artifact ID."
  (mevedel--write-file-atomically
   (mevedel-collaboration--artifact-comments-file workspace id)
   (mevedel-shared-editing--json (list :artifact id :comments (vconcat comments)))))

(defun mevedel-collaboration--artifact-comments-public (comments)
  "Return COMMENTS as guests receive them, without stored excerpts."
  (vconcat
   (mapcar (lambda (comment)
             (list :id (plist-get comment :id)
                   :actor (plist-get comment :actor)
                   :text (plist-get comment :text)
                   :anchor (plist-get comment :anchor)
                   :resolved (if (eq (plist-get comment :resolved) t) t :json-false)
                   ;; The session that answers the thread, for its chip.
                   :session (plist-get comment :session)
                   :sessionName (plist-get comment :session-name)
                   :replies (vconcat
                             (mapcar (lambda (reply)
                                       (list :id (plist-get reply :id)
                                             :actor (plist-get reply :actor)
                                             :text (plist-get reply :text)))
                                     (plist-get comment :replies)))))
           comments)))

(defun mevedel-collaboration--artifact-comments-publish (workspace id comments)
  "Tell every guest of WORKSPACE that its artifact ID now has COMMENTS.
Comments belong to the artifact, so every room and the lobby see them."
  (let ((frame (list :t "artifact-comments" :artifact id
                     :comments (mevedel-collaboration--artifact-comments-public
                                comments))))
    (dolist (room (mevedel-collaboration--workspace-rooms workspace))
      (mevedel-collaboration--broadcast room frame))))


;;
;;; Assistant requests

(defun mevedel-collaboration--artifact-question-known-p (data-buffer question-id)
  "Return non-nil when QUESTION-ID is already queued or delivered in DATA-BUFFER.
A retried send then succeeds without queueing the message twice."
  (let ((known (lambda (shared)
                 (and (equal (plist-get shared :kind) "artifact")
                      (equal (plist-get shared :questionId) question-id)))))
    (with-current-buffer data-buffer
      (or (cl-some (lambda (entry) (funcall known (plist-get entry :shared-question)))
                   (mevedel-session-pending-follow-ups mevedel--session))
          (cl-some (lambda (attribution)
                     (funcall known (plist-get (cdr attribution) :shared)))
                   (mevedel-transcript-audit-guest-prompts))))))

(defun mevedel-collaboration--artifact-answering-buffer (room id thread-session)
  "Return the data buffer of the session that answers about artifact ID.
A room answers in its own session only: its links speak for that session,
never another.  The lobby, which may open any session, keeps a thread
with THREAD-SESSION, the session answering it, or else uses the
artifact's dedicated session."
  (let ((workspace (mevedel-collaboration--room-workspace room)))
    (condition-case nil
        (cond
         ((plist-get room :session)
          (mevedel-collaboration--room-data-buffer room))
         ((and thread-session
               (mevedel-artifact-store-session-buffer workspace thread-session)))
         (t (mevedel-artifact-store-conversation workspace id)))
      (inhibited-interaction
       (error "%s" mevedel-collaboration-needs-host-message)))))

(defun mevedel-collaboration--artifact-ask
    (room guest record question-id text comment &optional images)
  "Queue GUEST's TEXT about store artifact RECORD and return the reply fields.
QUESTION-ID identifies the request; COMMENT is the thread it belongs to, or
nil for a message about the whole artifact.  IMAGES are the sender's
attachment frames.  The answering session, chosen by
`mevedel-collaboration--artifact-answering-buffer', becomes attached to the
artifact; the reply's `:session' and `:sessionName' name it."
  (let* ((id (plist-get record :store))
         (data-buffer (mevedel-collaboration--artifact-answering-buffer
                       room id (plist-get comment :session)))
         (session (buffer-local-value 'mevedel--session data-buffer))
         (answering (list :session (mevedel-session-session-id session)
                          :sessionName (mevedel-session-name session))))
    (mevedel-artifact-store-attach session id data-buffer)
    (if (mevedel-collaboration--artifact-question-known-p data-buffer question-id)
        (append (list :queued t :questionId question-id) answering)
      (let* ((paths (mevedel-collaboration--save-guest-files data-buffer images))
             (name (plist-get record :artifact))
             (shared (append (list :kind "artifact"
                                   :itemId (concat "artifact:" id)
                                   :title name
                                   :artifact name
                                   :questionId question-id
                                   :scope (if comment "selection" "whole")
                                   :text text)
                             (when comment
                               (list :commentId (plist-get comment :id)
                                     :anchor (plist-get comment :anchor)))))
             queued)
        (unwind-protect
            (setq queued
                  (mevedel-view-enqueue-external-follow-up
                   data-buffer
                   (concat text mevedel-collaboration--artifact-comment-snapshot-heading
                           (mevedel-collaboration--artifact-comment-snapshot
                            record comment))
                   :guest-name (plist-get guest :name)
                   :guest-id (plist-get guest :guest-id)
                   :guest-role (mevedel-collaboration--guest-role guest)
                   :paths paths
                   :shared-question shared))
          ;; Files of a message that was not queued leave with it.
          (unless queued
            (dolist (path paths) (ignore-errors (delete-file path)))))
        (unless queued (error "The session cannot accept a message right now"))
        (append (list :queued t :questionId question-id)
                answering
                (when-let* (((eq data-buffer (plist-get room :data-buffer)))
                            (position (mevedel-collaboration--queue-position
                                       room queued)))
                  (list :position position)))))))


;;
;;; Guest actions

(defun mevedel-collaboration--artifact-comment-id (value)
  "Return VALUE when it is a valid client-generated identity, else signal."
  (unless (and (stringp value) (string-match-p "\\`[0-9a-f-]\\{16,64\\}\\'" value))
    (error "The comment has no valid identity"))
  value)

(defun mevedel-collaboration--artifact-comment-message (value)
  "Return guest message VALUE trimmed and bounded, else signal."
  (let ((text (mevedel-collaboration--guest-text value)))
    (unless text (error "A message is required"))
    (when (> (length text) mevedel-collaboration--artifact-comment-max-message-chars)
      (error "Messages are limited to %d characters"
             mevedel-collaboration--artifact-comment-max-message-chars))
    text))

(defun mevedel-collaboration--artifact-comment-actor (guest)
  "Return the attribution GUEST's comments carry."
  (or (plist-get guest :name) "Guest"))

(defun mevedel-collaboration--artifact-comments-update (workspace id comments)
  "Store and publish COMMENTS for WORKSPACE's artifact ID, within bounds."
  (when (> (string-bytes (mevedel-shared-editing--json
                          (mevedel-collaboration--artifact-comments-public comments)))
           mevedel-collaboration--artifact-comment-max-bytes)
    (error "This artifact has too many comments; resolve and shorten some first"))
  (mevedel-collaboration--artifact-comments-write workspace id comments)
  (mevedel-collaboration--artifact-comments-publish workspace id comments))

(defun mevedel-collaboration--artifact-comment-answered (comment answer)
  "Return COMMENT recording the session the ASK reply fields name.
Return nil when ANSWER changes nothing, e.g. without an assistant
message."
  (when-let* ((session (plist-get answer :session))
              ((not (and (equal session (plist-get comment :session))
                         (equal (plist-get answer :sessionName)
                                (plist-get comment :session-name))))))
    (plist-put (plist-put (copy-sequence comment) :session session)
               :session-name (plist-get answer :sessionName))))

(defun mevedel-collaboration--artifact-comment-action (room guest frame)
  "Perform GUEST's artifact comment FRAME in ROOM and return the reply fields.
FRAME's `:id' names a card or a store artifact; comments belong to the
store artifact.  Signal an error with a message for the guest when the
action is refused."
  (let* ((action (plist-get frame :action))
         (workspace (mevedel-collaboration--room-workspace room))
         (named (mevedel-collaboration--artifact-target room guest (plist-get frame :id)))
         (id (plist-get named :store))
         (record (and id (mevedel-collaboration--store-target workspace id))))
    (unless (member action '("list" "post" "reply" "resolve" "ask"))
      (error "Unknown artifact comment action"))
    (unless (and record (not (plist-get record :missing)))
      (error "This artifact is not published"))
    (unless (equal (plist-get named :artifact) (plist-get record :artifact))
      (error "Comments are kept on an artifact's main file"))
    (unless (equal (mevedel-collaboration--artifact-mime (plist-get record :artifact))
                   "text/html")
      (error "Only HTML artifacts take comments"))
    (unless (or (equal action "list") (plist-get guest :writable))
      (error "This link can view the artifact but not comment on it"))
    (when (equal action "list")
      (mevedel-collaboration--store-read guest))
    (let* ((comments (mevedel-collaboration--artifact-comments-read workspace id))
           (find (lambda (comment-id)
                   (cl-find comment-id comments :key (lambda (c) (plist-get c :id))
                            :test #'equal)))
           (replace (lambda (comment)
                      (mapcar (lambda (c) (if (equal (plist-get c :id) (plist-get comment :id))
                                              comment c))
                              comments)))
           (to-assistant (eq (plist-get frame :toAssistant) t)))
      (pcase action
        ("list"
         (list :artifact id
               :comments (mevedel-collaboration--artifact-comments-public comments)))
        ("ask"
         (mevedel-collaboration--artifact-ask
          room guest record
          (mevedel-collaboration--artifact-comment-id (plist-get frame :questionId))
          (mevedel-collaboration--artifact-comment-message (plist-get frame :text))
          nil (plist-get frame :images)))
        ("post"
         (let* ((comment-id (mevedel-collaboration--artifact-comment-id
                             (plist-get frame :commentId)))
                (existing (funcall find comment-id))
                (comment
                 (or existing
                     (let ((text (mevedel-collaboration--artifact-comment-message
                                  (plist-get frame :text)))
                           (anchor (or (mevedel-collaboration--artifact-comment-anchor
                                        (plist-get frame :anchor))
                                       (error "The commented part could not be identified"))))
                       (when (>= (length comments)
                                 mevedel-collaboration--artifact-comment-max-comments)
                         (error "This artifact already has %d comments"
                                mevedel-collaboration--artifact-comment-max-comments))
                       (append (list :id comment-id
                                     :actor (mevedel-collaboration--artifact-comment-actor guest)
                                     :text text :anchor anchor :resolved :json-false
                                     :replies [])
                               (when-let* ((context (mevedel-collaboration--artifact-comment-context
                                                     (plist-get frame :context))))
                                 (list :context context))))))
                answer answered)
           (unless existing
             (setq comments (append comments (list comment)))
             (mevedel-collaboration--artifact-comments-update workspace id comments))
           (when to-assistant
             (setq answer (mevedel-collaboration--artifact-ask
                           room guest record comment-id (plist-get comment :text) comment))
             (when (setq answered (mevedel-collaboration--artifact-comment-answered
                                   comment answer))
               (mevedel-collaboration--artifact-comments-update
                workspace id (funcall replace answered))))
           (append (list :commentId comment-id) answer)))
        ("reply"
         (let* ((comment (or (funcall find (plist-get frame :commentId))
                             (error "That comment no longer exists")))
                (reply-id (mevedel-collaboration--artifact-comment-id
                           (plist-get frame :replyId)))
                (replies (append (plist-get comment :replies) nil))
                (existing (cl-find reply-id replies :key (lambda (r) (plist-get r :id))
                                   :test #'equal))
                (reply (or existing
                           (list :id reply-id
                                 :actor (mevedel-collaboration--artifact-comment-actor guest)
                                 :text (mevedel-collaboration--artifact-comment-message
                                        (plist-get frame :text)))))
                answer answered)
           (when (eq (plist-get comment :resolved) t)
             (error "Reopen this comment before replying"))
           (unless existing
             (when (>= (length replies) mevedel-collaboration--artifact-comment-max-replies)
               (error "This comment already has %d replies"
                      mevedel-collaboration--artifact-comment-max-replies))
             (setq comment (plist-put (copy-sequence comment) :replies
                                      (vconcat replies (list reply))))
             (setq comments (funcall replace comment))
             (mevedel-collaboration--artifact-comments-update workspace id comments))
           (when to-assistant
             (setq answer (mevedel-collaboration--artifact-ask
                           room guest record reply-id (plist-get reply :text) comment))
             (when (setq answered (mevedel-collaboration--artifact-comment-answered
                                   comment answer))
               (mevedel-collaboration--artifact-comments-update
                workspace id (funcall replace answered))))
           (append (list :commentId (plist-get comment :id) :replyId reply-id)
                   answer)))
        ("resolve"
         (let* ((comment (or (funcall find (plist-get frame :commentId))
                             (error "That comment no longer exists")))
                (resolved (if (eq (plist-get frame :resolved) t) t :json-false)))
           (unless (eq (plist-get comment :resolved) resolved)
             (mevedel-collaboration--artifact-comments-update
              workspace id
              (funcall replace (plist-put (copy-sequence comment) :resolved resolved))))
           (list :commentId (plist-get comment :id)
                 :resolved (if (eq resolved t) t :json-false))))))))

(defun mevedel-collaboration--handle-artifact-comment (room peer frame)
  "Perform guest PEER's artifact comment FRAME in ROOM and answer the sender.
Every refusal is answered to the sender; none reaches the room's frame
handler, whose failure would stop the share for everyone."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append (list :t "artifact-comment" :reqId req-id)
               (condition-case err
                   (mevedel-collaboration--artifact-comment-action room guest frame)
                 (error (list :error (error-message-string err)))))))))

(provide 'mevedel-collaboration-artifact-comments)
;;; mevedel-collaboration-artifact-comments.el ends here
