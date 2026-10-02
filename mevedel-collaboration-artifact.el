;;; mevedel-collaboration-artifact.el --- browser artifact transfer -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolves published artifact record ids and sends their bytes to browser
;; guests on demand.  Filesystem paths never cross the wire, and the path is
;; re-authorized against the canonical artifact root before each read.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--publish
                  "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-session
                  "mevedel-collaboration" (session))

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifacts-dir
                  "mevedel-collaboration-artifact-projection" (session))
(declare-function mevedel-collaboration--artifact-stat-invalidate
                  "mevedel-collaboration-artifact-projection" ())

;; `mevedel-collaboration-guest'
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

;; `mevedel-collaboration-artifact-comments'
(declare-function mevedel-collaboration--artifact-comment-logical
                  "mevedel-collaboration-artifact-comments" (name))
(autoload 'mevedel-collaboration--artifact-comment-logical
  "mevedel-collaboration-artifact-comments")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-artifacts-dir
                  "mevedel-session-artifacts" (save-path))
(declare-function mevedel-session-artifacts-delete-files
                  "mevedel-session-artifacts" (session paths))
(autoload 'mevedel-session-artifacts-artifacts-dir "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-delete-files "mevedel-session-artifacts")

;; `mevedel-resource'
(declare-function mevedel-resource-within-root-p
                  "mevedel-resource" (path root))
(autoload 'mevedel-resource-within-root-p "mevedel-resource")


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
                  (mevedel-collaboration--send-chunked
                   transport peer
                   (list :t "artifact" :reqId req-id
                         :id (plist-get record :id)
                         :name (plist-get record :artifact)
                         :mime (mevedel-collaboration--artifact-mime
                                (plist-get record :artifact))
                         :size size)
                   content)))))))))))

(defun mevedel-collaboration--send-chunked (transport peer meta content)
  "Send unibyte CONTENT to PEER through TRANSPORT as base64 chunk frames.
Every frame carries the plist META, a `:data' slice, and `:final', true
on the last one; each stays under the wire bound.  Return non-nil when
every frame was written."
  (let* ((data (base64-encode-string content t))
         ;; Budget the metadata actually sent, with the longer non-final
         ;; marker and an empty data string.
         (overhead (string-bytes
                    (json-encode
                     (append meta '(:data "" :final :json-false)))))
         (chunk (max 1 (- mevedel-collaboration--max-frame-json-bytes
                          overhead)))
         (total (length data))
         (start 0)
         (sent t)
         done)
    (while (and sent (not done))
      (let ((end (min total (+ start chunk))))
        (setq done (= end total)
              sent (mevedel-collaboration--transport-send
                    transport peer
                    (append meta
                            (list :data (substring data start end)
                                  :final (if done t :json-false))))
              start end)))
    sent))

(defun mevedel-collaboration-delete-artifact (session name)
  "Delete SESSION's artifact NAME with its comments and update its room.
NAME is the path relative to the artifacts folder, as cards and the
artifacts cockpit show it.  Whiteboards and documents keep their state
below `shared-editing/' and are deleted as items, not as files."
  (let ((directory (mevedel-session-artifacts-artifacts-dir
                    (or (mevedel-session-save-path session)
                        (error "This session has no artifacts folder")))))
    (when (string-prefix-p "shared-editing/" name)
      (error "Delete whiteboards and documents from Shared work"))
    (mevedel-session-artifacts-delete-files
     session
     (list (expand-file-name name directory)
           (file-name-concat (mevedel-session-save-path session)
                             (mevedel-collaboration--artifact-comment-logical name))))
    ;; Only a loaded collaboration can have a room to tell.
    (when (featurep 'mevedel-collaboration)
      (mevedel-collaboration-notify-artifacts-changed session))))

(defun mevedel-collaboration--handle-artifact-delete (room peer frame)
  "Delete the published artifact FRAME names for writable guest PEER in ROOM.
The file comes from the host's own record, never from the frame.  Every
refusal is answered to the sender."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (mevedel-collaboration--transport-send
       (plist-get room :transport) peer
       (append
        (list :t "artifact-delete" :reqId req-id)
        (condition-case err
            (let ((record (mevedel-collaboration--artifact-record
                           room guest (plist-get frame :id))))
              (unless (plist-get guest :writable)
                (error "This link can view artifacts but not delete them"))
              (unless (and record (not (plist-get record :missing)))
                (error "This artifact is no longer on the host"))
              (mevedel-collaboration-delete-artifact
               (plist-get room :session) (plist-get record :artifact))
              (list :ok t :artifact (plist-get record :artifact)))
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
