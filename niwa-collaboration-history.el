;;; niwa-collaboration-history.el --- Browser access to archived segments -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuses the session's archived transcripts as read-only browser history.
;; Only artifact metadata is cached; transcript bodies are projected on demand.
;; Compaction therefore changes model context without unpublishing artifacts.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'niwa-structs)
(require 'niwa-transport)
(require 'niwa-collaboration-projection)

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast "niwa-collaboration" (room frame))
(declare-function niwa-collaboration--guest "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--room-for-session "niwa-collaboration" (session))
;; `niwa-collaboration-artifact-projection'
(declare-function niwa-collaboration--artifact-title
                  "niwa-collaboration-artifact-projection" (session relative))
;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--request-id-p "niwa-collaboration-guest" (value))
(declare-function niwa-collaboration--snapshot-chunks "niwa-collaboration-guest" (records &optional overhead))
(declare-function niwa-collaboration--records-frame "niwa-collaboration-guest" (meta chunk final))
;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send "niwa-collaboration-transport" (transport peer frame))
(declare-function niwa-collaboration--send-guarded "niwa-collaboration" (room peer frame))
;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-read-segment "niwa-session-artifacts" (session number))
(declare-function niwa-session-artifacts-session-chats "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-read-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-session-chats "niwa-session-artifacts")

(defun niwa-collaboration--history-records (room number)
  "Project archived segment NUMBER in ROOM without activating it."
  (let* ((session (plist-get room :session))
         (live (plist-get room :data-buffer))
         (buffer (niwa-session-artifacts-read-segment session number))
         (completions (niwa-collaboration--session-bash-completions
                       session live number)))
    (unwind-protect
        (with-current-buffer buffer
          ;; Inspection buffers do not inherit session authority. Projection
          ;; needs the session solely to resolve its published artifact paths.
          (setq-local niwa--session session)
          (mapcar (lambda (record)
                    (plist-put record :id
                               (format "history-%d-%s" number (plist-get record :id))))
                  (niwa-collaboration--canonical-records
                   buffer live completions)))
      (kill-buffer buffer))))

(defun niwa-collaboration--history-artifacts (room)
  "Return artifacts published by ROOM's archived transcript records.
Cache immutable segment metadata, never a second transcript store."
  (when-let* ((session (plist-get room :session))
              (path (niwa-session-save-path session)))
    (let* ((current (or (niwa-session-current-segment session) 1))
           (source (plist-get room :history-source))
           (archives (and (equal path (car source))
                          (<= (or (cdr source) 1) current)
                          (plist-get room :history-archives))))
      (cl-loop for number from 1 below current do
               (unless (assq number archives)
                 (push
                  (cons number
                        (condition-case nil
                            (niwa-collaboration--history-catalog-records
                             (niwa-collaboration--history-records room number))
                          ;; A missing archive does not stop a live room. Its
                          ;; disclosure fetch reports the failure with Retry.
                          (error nil)))
                  archives)))
      (plist-put room :history-source (cons path current))
      (plist-put room :history-archives archives)
      (cl-loop for number from 1 below current append
               (mapcar
                (lambda (record)
                  (let ((stat (niwa-collaboration--artifact-stat
                               (plist-get record :artifact-path))))
                    (append record
                            (when-let* ((title (niwa-collaboration--artifact-title
                                               session (plist-get record :artifact))))
                              (list :title title))
                            (if (null (car stat)) '(:missing t)
                              (list :size (car stat) :artifactRevision (cdr stat))))))
                (cdr (assq number archives)))))))

(defun niwa-collaboration--history-catalog-records (records)
  "Extract only published artifact identities and paths from RECORDS."
  (cl-loop for record in records when (plist-get record :artifact)
           collect (append (list :id (plist-get record :id)
                                 :artifact (plist-get record :artifact)
                                 :artifact-path (plist-get record :artifact-path))
                           (when (plist-member record :store)
                             (list :store (plist-get record :store))))))

(defun niwa-collaboration--publish-history (room &optional peer)
  "Publish ROOM's history index when changed, or directly to joining PEER."
  (let* ((session (plist-get room :session))
         (current (or (and session (niwa-session-current-segment session)) 1))
         (artifacts (niwa-collaboration--history-artifacts room))
         ;; Chat descriptors share the bounded records stream with artifacts,
         ;; rather than repeating an unbounded index in every chunk's metadata.
         (records (append artifacts
                          (when session
                            (mapcar
                             (lambda (chat)
                               (list :kind "chat" :number (plist-get chat :number)
                                     :firstSegment (plist-get chat :first-segment)
                                     :lastSegment (plist-get chat :last-segment)
                                     :title (plist-get chat :title)))
                             (niwa-session-artifacts-session-chats session)))))
         (index (cons current records))
         (meta (list :t "history-index" :currentSegment current)))
    (when (or peer (not (equal index (plist-get room :history-index))))
      (unless peer (plist-put room :history-index index))
      (let* ((overhead (string-bytes (json-encode (append meta '(:records [] :final :json-false)))))
             (chunks (niwa-collaboration--snapshot-chunks records overhead)))
        (cl-loop for rest on chunks do
                 (let ((frame (niwa-collaboration--records-frame
                               meta (car rest) (null (cdr rest)))))
                   (if peer
                       (niwa-collaboration--send-guarded room peer frame)
                     (niwa-collaboration--broadcast room frame))))))
    artifacts))

(defun niwa-collaboration--handle-history-get (room peer frame)
  "Send a bounded projection of an archived segment to authenticated PEER."
  (let* ((guest (niwa-collaboration--guest room peer))
         (session (plist-get room :session))
         (number (plist-get frame :segment))
         (req-id (plist-get frame :reqId))
         (meta (list :t "history" :reqId req-id :segment number))
         (transport (plist-get room :transport)))
    (when (and guest (niwa-collaboration--request-id-p req-id))
      (cond
       ((not (and session (integerp number) (< 0 number)
                  (< number (or (niwa-session-current-segment session) 1))))
        (niwa-collaboration--transport-send
         transport peer (list :t "history" :reqId req-id :error "Unknown archived segment")))
       ((let ((last (plist-get guest :last-history-fetch)))
          (and last (< (- (float-time) last) 1)))
        (niwa-collaboration--transport-send
         transport peer (append meta '(:error "Please wait a moment, then retry."))))
       (t
        (plist-put guest :last-history-fetch (float-time))
        (let ((fetch
               (lambda ()
		 (condition-case nil
		     (let* ((records (niwa-collaboration--history-records room number))
			    (overhead (string-bytes (json-encode (append meta '(:records [] :final :json-false)))))
			    (chunks (niwa-collaboration--snapshot-chunks records overhead)))
		       ;; A successful retry can recover metadata from an archive that
		       ;; was unavailable when this room first built its index.
		       (let ((archives (plist-get room :history-archives)))
			 (setf (alist-get number archives)
			       (niwa-collaboration--history-catalog-records records))
			 (plist-put room :history-archives archives))
		       (niwa-collaboration--publish-history room)
		       (cl-loop for rest on chunks do
				(niwa-collaboration--transport-send
				 transport peer
				 (niwa-collaboration--records-frame
				  meta (car rest) (null (cdr rest))))))
		   (error
		    (niwa-collaboration--transport-send
		     transport peer (append meta '(:error "This archived segment could not be read. Retry when the host is available."))))))))
          ;; Websocket filters can arrive while another remote read owns
          ;; TRAMP's connection.  Keep the rate limit at admission and recheck
          ;; the room and guest before the deferred disclosure.
          (let ((path (niwa-session-save-path session)))
            (if (niwa-transport-busy-p path)
                (niwa-transport-run-when-idle
                 (list 'collaboration-history (plist-get room :room-id) peer req-id) path
                 (lambda ()
                   (when (and (eq room (niwa-collaboration--room-for-session session))
                              (eq guest (niwa-collaboration--guest room peer)))
                     (funcall fetch))))
              (funcall fetch)))))))))

(provide 'niwa-collaboration-history)
;;; niwa-collaboration-history.el ends here
