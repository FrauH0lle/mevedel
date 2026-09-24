;;; mevedel-collaboration-history.el --- Browser access to archived segments -*- lexical-binding: t; -*-

;;; Commentary:
;; Reuses the session's archived transcripts as read-only browser history.
;; Only artifact metadata is cached; transcript bodies are projected on demand.
;; Compaction therefore changes model context without unpublishing artifacts.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'mevedel-structs)
(require 'mevedel-collaboration-projection)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest "mevedel-collaboration" (room peer))
;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--request-id-p "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--snapshot-chunks "mevedel-collaboration-guest" (records &optional overhead))
;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send "mevedel-collaboration-transport" (transport peer frame))
;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-segment "mevedel-session-artifacts" (session number))
(autoload 'mevedel-session-artifacts-read-segment "mevedel-session-artifacts")

(defun mevedel-collaboration--history-records (room number)
  "Project archived segment NUMBER in ROOM without activating it."
  (let* ((session (plist-get room :session))
         (buffer (mevedel-session-artifacts-read-segment session number)))
    (unwind-protect
        (with-current-buffer buffer
          ;; Inspection buffers do not inherit session authority. Projection
          ;; needs the session solely to resolve its published artifact paths.
          (setq-local mevedel--session session)
          (mapcar (lambda (record)
                    (plist-put record :id
                               (format "history-%d-%s" number (plist-get record :id))))
                  (mevedel-collaboration--canonical-records buffer)))
      (kill-buffer buffer))))

(defun mevedel-collaboration--history-artifacts (room)
  "Return artifacts published by ROOM's archived transcript records.
Cache immutable segment metadata, never a second transcript store."
  (when-let* ((session (plist-get room :session))
              (path (mevedel-session-save-path session)))
    (let* ((current (or (mevedel-session-current-segment session) 1))
           (source (plist-get room :history-source))
           (archives (and (equal path (car source))
                          (<= (or (cdr source) 1) current)
                          (plist-get room :history-archives))))
      (cl-loop for number from 1 below current do
               (unless (assq number archives)
                 (push
                  (cons number
                        (condition-case nil
                            (mevedel-collaboration--history-catalog-records
                             (mevedel-collaboration--history-records room number))
                          ;; A missing archive does not stop a live room. Its
                          ;; disclosure fetch reports the failure with Retry.
                          (error nil)))
                  archives)))
      (plist-put room :history-source (cons path current))
      (plist-put room :history-archives archives)
      (cl-loop for number from 1 below current append
               (mapcar
                (lambda (record)
                  (let ((stat (mevedel-collaboration--artifact-stat
                               (plist-get record :artifact-path))))
                    (append record (if (cdr stat) '(:missing t) (list :size (car stat))))))
                (cdr (assq number archives)))))))

(defun mevedel-collaboration--history-catalog-records (records)
  "Extract only published artifact identities and paths from RECORDS."
  (cl-loop for record in records when (plist-get record :artifact)
           collect (list :id (plist-get record :id)
                         :artifact (plist-get record :artifact)
                         :artifact-path (plist-get record :artifact-path))))

(defun mevedel-collaboration--publish-history (room &optional peer)
  "Publish ROOM's history index when changed, or directly to joining PEER."
  (let* ((session (plist-get room :session))
         (current (or (and session (mevedel-session-current-segment session)) 1))
         (records (mevedel-collaboration--history-artifacts room))
         (index (cons current records))
         (meta (list :t "history-index" :currentSegment current)))
    (when (or peer (not (equal index (plist-get room :history-index))))
      (unless peer (plist-put room :history-index index))
      (let* ((overhead (string-bytes (json-encode (append meta '(:records [] :final :json-false)))))
             (chunks (or (mevedel-collaboration--snapshot-chunks records overhead) (list nil))))
        (cl-loop for rest on chunks do
                 (let ((frame (append meta (list :records (vconcat (car rest))
                                                :final (if (cdr rest) :json-false t)))))
                   (if peer
                       (mevedel-collaboration--transport-send (plist-get room :transport) peer frame)
                     (mevedel-collaboration--broadcast room frame))))))
    records))

(defun mevedel-collaboration--handle-history-get (room peer frame)
  "Send a bounded projection of an archived segment to authenticated PEER."
  (let* ((guest (mevedel-collaboration--guest room peer))
         (session (plist-get room :session))
         (number (plist-get frame :segment))
         (req-id (plist-get frame :reqId))
         (meta (list :t "history" :reqId req-id :segment number))
         (transport (plist-get room :transport)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      (cond
       ((not (and session (integerp number) (< 0 number)
                  (< number (or (mevedel-session-current-segment session) 1))))
        (mevedel-collaboration--transport-send
         transport peer (list :t "history" :reqId req-id :error "Unknown archived segment")))
       ((let ((last (plist-get guest :last-history-fetch)))
          (and last (< (- (float-time) last) 1)))
        (mevedel-collaboration--transport-send
         transport peer (append meta '(:error "Please wait a moment, then retry."))))
       (t
        (plist-put guest :last-history-fetch (float-time))
        (condition-case nil
            (let* ((records (mevedel-collaboration--history-records room number))
                   (overhead (string-bytes (json-encode (append meta '(:records [] :final :json-false)))))
                   (chunks (or (mevedel-collaboration--snapshot-chunks records overhead) (list nil))))
              ;; A successful retry can recover metadata from an archive that
              ;; was unavailable when this room first built its index.
              (let ((archives (plist-get room :history-archives)))
                (setf (alist-get number archives)
                      (mevedel-collaboration--history-catalog-records records))
                (plist-put room :history-archives archives))
              (mevedel-collaboration--publish-history room)
              (cl-loop for rest on chunks do
                       (mevedel-collaboration--transport-send
                        transport peer
                        (append meta (list :records (vconcat (car rest))
                                           :final (if (cdr rest) :json-false t))))))
          (error
           (mevedel-collaboration--transport-send
            transport peer (append meta '(:error "This archived segment could not be read. Retry when the host is available."))))))))))

(provide 'mevedel-collaboration-history)
;;; mevedel-collaboration-history.el ends here
