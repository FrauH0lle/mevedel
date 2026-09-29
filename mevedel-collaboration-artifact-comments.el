;;; mevedel-collaboration-artifact-comments.el --- comments on session artifacts -*- lexical-binding: t; -*-

;;; Commentary:

;; Browser guests comment on parts of HTML artifacts.  The viewer reports
;; what was picked; the host rebuilds that anchor from bounded fields,
;; keeps the comment thread in a per-artifact store beside the shared
;; items, and, when a message is sent to the assistant, queues it into the
;; artifact's own conversation.  Artifact conversations are shared-item
;; conversations whose item identity is "artifact:NAME", so the room lists
;; them as discussions and the request context isolates them the same way.

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

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration--artifact-mime
                  "mevedel-collaboration-artifact" (name))
(declare-function mevedel-collaboration--artifact-record
                  "mevedel-collaboration-artifact" (room guest id))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--guest-role
                  "mevedel-collaboration-guest" (guest))
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs" (data-buffer text &rest keys))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts" (session logical &optional committed-only))

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p
                  "mevedel-session-codec" (session))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir &optional head names))

;; `mevedel-structs'
(declare-function mevedel-session-pending-follow-ups "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())


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
             (format "Session artifact %s" (plist-get record :artifact))
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

(defun mevedel-collaboration--artifact-comment-logical (name)
  "Return the session-relative comment store path for artifact NAME."
  (file-name-concat "artifacts" "shared-editing" "artifact-comments"
                    (concat (substring (secure-hash 'sha256 name) 0 32) ".json")))

(defun mevedel-collaboration--artifact-comments-stored-p (session logical)
  "Return non-nil when SESSION holds the comment store at LOGICAL."
  (when-let* ((root (mevedel-session-save-path session)))
    (if (mevedel-session-codec-portable-authority-p session)
        (assoc logical (plist-get (or (mevedel-session-publication session)
                                      (mevedel-session-publication-read root))
                                  :artifacts))
      (file-exists-p (file-name-concat root logical)))))

(defun mevedel-collaboration--artifact-comments-read (session name)
  "Return artifact NAME's stored comments in SESSION, oldest first.
A missing store is an artifact nobody has commented on yet."
  (let* ((logical (mevedel-collaboration--artifact-comment-logical name))
         (text (and (mevedel-collaboration--artifact-comments-stored-p session logical)
                    (mevedel-session-artifacts-read-artifact session logical t))))
    (when text
      (let ((state (mevedel-shared-editing--parse
                    (decode-coding-string text 'utf-8-unix))))
        (unless (equal (plist-get state :artifact) name)
          (error "Artifact comment store does not match %s" name))
        (append (plist-get state :comments) nil)))))

(defun mevedel-collaboration--artifact-comments-write (room name comments)
  "Durably store COMMENTS for artifact NAME in ROOM's session."
  (let ((content (mevedel-shared-editing--json
                  (list :artifact name :comments (vconcat comments)))))
    (with-current-buffer (mevedel-collaboration--room-data-buffer room)
      (mevedel-shared-editing-commit-file
       (plist-get room :session)
       (mevedel-collaboration--artifact-comment-logical name) content))))

(defun mevedel-collaboration--artifact-comments-public (comments)
  "Return COMMENTS as guests receive them, without stored excerpts."
  (vconcat
   (mapcar (lambda (comment)
             (list :id (plist-get comment :id)
                   :actor (plist-get comment :actor)
                   :text (plist-get comment :text)
                   :anchor (plist-get comment :anchor)
                   :resolved (if (eq (plist-get comment :resolved) t) t :json-false)
                   :replies (vconcat
                             (mapcar (lambda (reply)
                                       (list :id (plist-get reply :id)
                                             :actor (plist-get reply :actor)
                                             :text (plist-get reply :text)))
                                     (plist-get comment :replies)))))
           comments)))

(defun mevedel-collaboration--artifact-comments-publish (room name comments)
  "Tell every guest in ROOM that artifact NAME now has COMMENTS."
  (mevedel-collaboration--broadcast
   room (list :t "artifact-comments" :artifact name
              :comments (mevedel-collaboration--artifact-comments-public comments))))


;;
;;; Assistant requests

(defun mevedel-collaboration--artifact-question-known-p (room question-id)
  "Return non-nil when QUESTION-ID is already queued or delivered in ROOM.
A retried send then succeeds without queueing the message twice."
  (let ((known (lambda (shared)
                 (and (equal (plist-get shared :kind) "artifact")
                      (equal (plist-get shared :questionId) question-id))))
        (buffer (mevedel-collaboration--room-data-buffer room)))
    (or (cl-some (lambda (entry) (funcall known (plist-get entry :shared-question)))
                 (mevedel-session-pending-follow-ups (plist-get room :session)))
        (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (cl-some (lambda (attribution)
                          (funcall known (plist-get (cdr attribution) :shared)))
                        (mevedel-transcript-audit-guest-prompts)))))))

(defun mevedel-collaboration--artifact-ask (room guest record question-id text comment)
  "Queue GUEST's TEXT about artifact RECORD into its conversation in ROOM.
QUESTION-ID identifies the request; COMMENT is the thread it belongs to, or
nil for a message about the whole artifact.  Return the reply fields."
  (if (mevedel-collaboration--artifact-question-known-p room question-id)
      (list :queued t :questionId question-id)
    (let* ((name (plist-get record :artifact))
           (shared (append (list :kind "artifact"
                                 :itemId (concat "artifact:" name)
                                 :title name
                                 :artifact name
                                 :questionId question-id
                                 :scope (if comment "selection" "whole")
                                 :text text)
                           (when comment
                             (list :commentId (plist-get comment :id)
                                   :anchor (plist-get comment :anchor)))))
           (queued (mevedel-view-enqueue-external-follow-up
                    (mevedel-collaboration--room-data-buffer room)
                    (concat text mevedel-collaboration--artifact-comment-snapshot-heading
                            (mevedel-collaboration--artifact-comment-snapshot
                             record comment))
                    :guest-name (plist-get guest :name)
                    :guest-id (plist-get guest :guest-id)
                    :guest-role (mevedel-collaboration--guest-role guest)
                    :shared-question shared)))
      (unless queued (error "The session cannot accept a message right now"))
      (append (list :queued t :questionId question-id)
              (when-let* ((position (mevedel-collaboration--queue-position room queued)))
                (list :position position))))))


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

(defun mevedel-collaboration--artifact-comments-update (room name comments)
  "Store and publish COMMENTS for artifact NAME in ROOM, within bounds."
  (when (> (string-bytes (mevedel-shared-editing--json
                          (mevedel-collaboration--artifact-comments-public comments)))
           mevedel-collaboration--artifact-comment-max-bytes)
    (error "This artifact has too many comments; resolve and shorten some first"))
  (mevedel-collaboration--artifact-comments-write room name comments)
  (mevedel-collaboration--artifact-comments-publish room name comments))

(defun mevedel-collaboration--artifact-comment-action (room guest frame)
  "Perform GUEST's artifact comment FRAME in ROOM and return the reply fields.
Signal an error with a message for the guest when the action is refused."
  (let* ((action (plist-get frame :action))
         (record (mevedel-collaboration--artifact-record
                  room guest (plist-get frame :id)))
         (name (plist-get record :artifact))
         (session (plist-get room :session)))
    (unless (member action '("list" "post" "reply" "resolve" "ask"))
      (error "Unknown artifact comment action"))
    (unless (and record (not (plist-get record :missing)))
      (error "This artifact is not published"))
    (unless (equal (mevedel-collaboration--artifact-mime name) "text/html")
      (error "Only HTML artifacts take comments"))
    (unless (or (equal action "list") (plist-get guest :writable))
      (error "This link can view the artifact but not comment on it"))
    (let* ((comments (mevedel-collaboration--artifact-comments-read session name))
           (find (lambda (id) (cl-find id comments :key (lambda (c) (plist-get c :id))
                                       :test #'equal)))
           (to-assistant (eq (plist-get frame :toAssistant) t)))
      (pcase action
        ("list"
         (list :artifact name
               :comments (mevedel-collaboration--artifact-comments-public comments)))
        ("ask"
         (mevedel-collaboration--artifact-ask
          room guest record
          (mevedel-collaboration--artifact-comment-id (plist-get frame :questionId))
          (mevedel-collaboration--artifact-comment-message (plist-get frame :text))
          nil))
        ("post"
         (let* ((id (mevedel-collaboration--artifact-comment-id (plist-get frame :commentId)))
                (existing (funcall find id))
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
                       (append (list :id id
                                     :actor (mevedel-collaboration--artifact-comment-actor guest)
                                     :text text :anchor anchor :resolved :json-false
                                     :replies [])
                               (when-let* ((context (mevedel-collaboration--artifact-comment-context
                                                     (plist-get frame :context))))
                                 (list :context context)))))))
           (unless existing
             (mevedel-collaboration--artifact-comments-update
              room name (append comments (list comment))))
           (append (list :commentId id)
                   (when to-assistant
                     (mevedel-collaboration--artifact-ask
                      room guest record id (plist-get comment :text) comment)))))
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
                                        (plist-get frame :text))))))
           (when (eq (plist-get comment :resolved) t)
             (error "Reopen this comment before replying"))
           (unless existing
             (when (>= (length replies) mevedel-collaboration--artifact-comment-max-replies)
               (error "This comment already has %d replies"
                      mevedel-collaboration--artifact-comment-max-replies))
             (setq comment (plist-put (copy-sequence comment) :replies
                                      (vconcat replies (list reply))))
             (mevedel-collaboration--artifact-comments-update
              room name (mapcar (lambda (c) (if (equal (plist-get c :id) (plist-get comment :id))
                                                comment c))
                                comments)))
           (append (list :commentId (plist-get comment :id) :replyId reply-id)
                   (when to-assistant
                     (mevedel-collaboration--artifact-ask
                      room guest record reply-id (plist-get reply :text) comment)))))
        ("resolve"
         (let* ((comment (or (funcall find (plist-get frame :commentId))
                             (error "That comment no longer exists")))
                (resolved (if (eq (plist-get frame :resolved) t) t :json-false)))
           (unless (eq (plist-get comment :resolved) resolved)
             (let ((changed (plist-put (copy-sequence comment) :resolved resolved)))
               (mevedel-collaboration--artifact-comments-update
                room name (mapcar (lambda (c) (if (eq c comment) changed c)) comments))))
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
