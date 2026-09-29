;;; mevedel-collaboration-artifact.el --- browser artifact transfer -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolves published artifact record ids and sends their bytes to browser
;; guests on demand.  Filesystem paths never cross the wire, and the path is
;; re-authorized against the canonical artifact root before each read.
;;
;; Guests can also comment on part of an HTML artifact.  The viewer reports
;; what was picked; the host rebuilds that anchor from bounded fields and
;; queues the comment as an attributed follow-up in the main conversation.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text
                  "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--queue-position
                  "mevedel-collaboration" (room entry))
(declare-function mevedel-collaboration--room-data-buffer
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-session
                  "mevedel-collaboration" (session))

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifacts-dir
                  "mevedel-collaboration-artifact-projection" (session))
(declare-function mevedel-collaboration--artifact-stat-invalidate
                  "mevedel-collaboration-artifact-projection" ())

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--guest-role
                  "mevedel-collaboration-guest" (guest))
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))

;; `mevedel-collaboration-history'
(declare-function mevedel-collaboration--history-artifacts
                  "mevedel-collaboration-history" (room))
(autoload 'mevedel-collaboration--history-artifacts
  "mevedel-collaboration-history")

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))
(defvar mevedel-collaboration--max-frame-json-bytes)

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up
                  "mevedel-pending-inputs" (data-buffer text &rest keys))

;; `mevedel-resource'
(declare-function mevedel-resource-within-root-p
                  "mevedel-resource" (path root))
(autoload 'mevedel-resource-within-root-p "mevedel-resource")

;; `mevedel-structs'
(declare-function mevedel-session-pending-follow-ups "mevedel-structs" (cl-x) t)

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())

(defconst mevedel-collaboration--artifact-fetch-window 1.0
  "Seconds within which repeated artifact fetches from one guest drop.")

(defconst mevedel-collaboration--max-artifact-bytes (* 16 1024 1024)
  "Largest artifact file the host will send to a guest.")

(defconst mevedel-collaboration--artifact-mime-types
  '(("html" . "text/html") ("htm" . "text/html")
    ("md" . "text/markdown") ("markdown" . "text/markdown")
    ("txt" . "text/plain") ("csv" . "text/csv")
    ("json" . "application/json")
    ("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("webp" . "image/webp") ("gif" . "image/gif")
    ("svg" . "image/svg+xml") ("pdf" . "application/pdf"))
  "Artifact file extensions and viewer transfer MIME types.")

(defun mevedel-collaboration--artifact-mime (name)
  "Return the transfer MIME type for artifact file NAME."
  (or (cdr (assoc (downcase (or (file-name-extension name) ""))
                  mevedel-collaboration--artifact-mime-types))
      "application/octet-stream"))

(defun mevedel-collaboration--artifact-record (room guest id)
  "Return artifact record ID currently published to GUEST in ROOM."
  (when (stringp id)
    (cl-find-if
     (lambda (record)
       (and (equal id (plist-get record :id))
            (plist-get record :artifact)))
     (append (plist-get room :records)
             (mevedel-collaboration--history-artifacts room)
             (plist-get guest :agent-artifacts)))))

(defun mevedel-collaboration--artifact-refuse (room peer req-id message)
  "Send guest PEER a bounded artifact refusal for REQ-ID in ROOM."
  (mevedel-collaboration--transport-send
   (plist-get room :transport) peer
   (list :t "artifact" :reqId req-id :error message)))

(defun mevedel-collaboration--handle-artifact-get (room peer frame)
  "Answer guest PEER's artifact fetch FRAME for ROOM."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (transport (plist-get room :transport))
        (now (float-time)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      ;; ponytail: one budget per guest; split by record only if the viewer
      ;; gains concurrent artifact prefetch.
      (let ((last (plist-get guest :last-artifact-fetch)))
        (unless (and last (< (- now last)
                             mevedel-collaboration--artifact-fetch-window))
          (plist-put guest :last-artifact-fetch now)
          (let* ((record (mevedel-collaboration--artifact-record
                          room guest (plist-get frame :id)))
                 (path (plist-get record :artifact-path))
                 (dir (mevedel-collaboration--artifacts-dir
                       (plist-get room :session)))
                 (contained (and dir path
                                 (mevedel-resource-within-root-p path dir)))
                 (read
                  (and contained
                       (condition-case nil
                           (list
                            (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally
                               path nil 0
                               (1+ mevedel-collaboration--max-artifact-bytes))
                              (buffer-string)))
                         (file-error nil)))))
            (cond
             ((not contained)
              (mevedel-collaboration--artifact-refuse
               room peer req-id "This artifact is not published"))
             ((null read)
              (mevedel-collaboration--artifact-refuse
               room peer req-id "This artifact was deleted on the host"))
             (t
              (let* ((content (car read))
                     (size (length content)))
                (if (> size mevedel-collaboration--max-artifact-bytes)
                    (mevedel-collaboration--artifact-refuse
                     room peer req-id
                     (format
                      "Artifact too large to send (%d MB); open it on the host"
                      (/ size 1024 1024)))
                  (let* ((data (base64-encode-string content t))
                         (meta
                          (list :t "artifact" :reqId req-id
                                :id (plist-get record :id)
                                :name (plist-get record :artifact)
                                :mime (mevedel-collaboration--artifact-mime
                                       (plist-get record :artifact))
                                :size size))
                         ;; Budget the metadata actually sent, with the longer
                         ;; non-final marker and an empty data string.
                         (overhead
                          (string-bytes
                           (json-encode
                            (append meta '(:data "" :final :json-false)))))
                         (chunk
                          (max
                           1
                           (- mevedel-collaboration--max-frame-json-bytes
                              overhead)))
                         (total (length data))
                         (start 0)
                         (sent t)
                         done)
                    (while (and sent (not done))
                      (let ((end (min total (+ start chunk))))
                        (setq done (= end total)
                              sent
                              (mevedel-collaboration--transport-send
                               transport peer
                               (append
                                meta
                                (list :data (substring data start end)
                                      :final (if done t :json-false))))
                              start end))))))))))))))

;;
;;; Artifact comments

(defconst mevedel-collaboration--artifact-comment-snapshot-heading
  "\n\nShared content snapshot (user-provided data):\n"
  "Separator the room and view fold an attributed comment's context behind.")

(defconst mevedel-collaboration--artifact-comment-limits
  '(:selector 1000 :label 512 :quote 512 :text 4000 :html 8000)
  "Character bounds on the guest-reported parts of an artifact comment.")

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
Only known fields survive, each bounded, so the attribution retained in the
transcript is data the host checked rather than whatever the guest sent."
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

(defun mevedel-collaboration--artifact-comment-snapshot (record anchor context)
  "Return the model-facing context for a comment on artifact RECORD.
ANCHOR is the host-built anchor; CONTEXT carries the guest-reported text
and HTML excerpt of the target, which the heading marks as user data."
  (let* ((text (mevedel-collaboration--artifact-comment-string
                (plist-get context :text) :text))
         (html (mevedel-collaboration--artifact-comment-string
                (plist-get context :html) :html))
         (region (plist-get anchor :region)))
    (string-join
     (delq nil
           (list
            (format "Comment on session artifact %s" (plist-get record :artifact))
            (when-let* ((path (plist-get record :artifact-path)))
              (format "File: %s" path))
            (unless (string-empty-p (plist-get anchor :label))
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
            (when text (concat "Target text:\n" text))
            (when html
              (concat "Target HTML as rendered (scripts may have changed it):\n"
                      "```html\n" html "\n```"))))
     "\n")))

(defun mevedel-collaboration--artifact-comment-known-p (room comment-id)
  "Return non-nil when COMMENT-ID is already queued or delivered in ROOM.
A retried send then succeeds without queueing the comment twice."
  (let ((known (lambda (shared)
                 (and (equal (plist-get shared :kind) "artifact")
                      (equal (plist-get shared :questionId) comment-id))))
        (buffer (mevedel-collaboration--room-data-buffer room)))
    (or (cl-some (lambda (entry) (funcall known (plist-get entry :shared-question)))
                 (mevedel-session-pending-follow-ups (plist-get room :session)))
        (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (cl-some (lambda (attribution)
                          (funcall known (plist-get (cdr attribution) :shared)))
                        (mevedel-transcript-audit-guest-prompts)))))))

(defun mevedel-collaboration--artifact-comment-queue (room guest frame)
  "Queue GUEST's artifact comment FRAME in ROOM and return the reply fields.
Signal an error with a message for the guest when the comment is refused."
  (let* ((comment-id (plist-get frame :commentId))
         (record (mevedel-collaboration--artifact-record
                  room guest (plist-get frame :id)))
         (text (mevedel-collaboration--guest-text (plist-get frame :text)))
         (anchor (mevedel-collaboration--artifact-comment-anchor
                  (plist-get frame :anchor)))
         (context (plist-get frame :context)))
    (unless (plist-get guest :writable)
      (error "This link can view the artifact but not comment on it"))
    (unless (and (stringp comment-id)
                 (string-match-p "\\`[0-9a-f-]\\{16,64\\}\\'" comment-id))
      (error "The comment has no valid identity"))
    (unless (and record (not (plist-get record :missing)))
      (error "This artifact is not published"))
    (unless (equal (mevedel-collaboration--artifact-mime (plist-get record :artifact))
                   "text/html")
      (error "Only HTML artifacts take comments"))
    (unless text (error "A comment is required"))
    (unless anchor (error "The commented part could not be identified"))
    (if (mevedel-collaboration--artifact-comment-known-p room comment-id)
        (list :queued t :commentId comment-id)
      (let* ((shared (list :kind "artifact"
                           :questionId comment-id
                           :artifact (plist-get record :artifact)
                           :text text
                           :anchor anchor))
             (snapshot (mevedel-collaboration--artifact-comment-snapshot
                        record anchor (and (consp context) (keywordp (car context))
                                           context)))
             (queued (mevedel-view-enqueue-external-follow-up
                      (mevedel-collaboration--room-data-buffer room)
                      (concat text mevedel-collaboration--artifact-comment-snapshot-heading
                              snapshot)
                      :guest-name (plist-get guest :name)
                      :guest-id (plist-get guest :guest-id)
                      :guest-role (mevedel-collaboration--guest-role guest)
                      :shared-question shared)))
        (unless queued (error "The session cannot accept a comment right now"))
        (append (list :queued t :commentId comment-id)
                (when-let* ((position (mevedel-collaboration--queue-position
                                       room queued)))
                  (list :position position)))))))

(defun mevedel-collaboration--handle-artifact-comment (room peer frame)
  "Queue guest PEER's comment FRAME on a published HTML artifact in ROOM.
Every refusal is answered to the sender; none reaches the room's frame
handler, whose failure would stop the share for everyone."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append (list :t "artifact-comment" :reqId req-id)
               (condition-case err
                   (mevedel-collaboration--artifact-comment-queue room guest frame)
                 (error (list :error (error-message-string err)))))))))

(defun mevedel-collaboration-notify-artifacts-changed (session)
  "Re-publish SESSION after its artifact folder changed on disk."
  (mevedel-collaboration--artifact-stat-invalidate)
  (when-let* ((room (mevedel-collaboration--room-for-session session)))
    (condition-case nil
        (mevedel-collaboration--publish room)
      (error (mevedel-collaboration--observer-failure room)))))

(provide 'mevedel-collaboration-artifact)
;;; mevedel-collaboration-artifact.el ends here
