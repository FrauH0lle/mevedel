;;; mevedel-collaboration-artifact-history.el --- Item answer history -*- lexical-binding: t; -*-

;;; Commentary:
;; Read-only canonical HTML discussion history.  Opening history never resumes
;; a session or acquires its authority.  A room reads only its own session;
;; the lobby follows the selected discussion's answering session.  Source
;; identities cache bounded projections, not another durable conversation store.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'mevedel-structs)
(require 'mevedel-shared-conversation)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-codec)
(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-view-interaction)

;; `mevedel-artifact-store'
(declare-function mevedel-artifact-store-meta "mevedel-artifact-store" (workspace id))

;; `mevedel-chat'
(declare-function mevedel--workspace-sessions "mevedel-chat" (workspace))
(autoload 'mevedel--workspace-sessions "mevedel-chat")

;; `mevedel-collaboration-artifact'
(declare-function mevedel-collaboration--store-read "mevedel-collaboration-artifact" (guest))

;; `mevedel-collaboration-discussions'
(declare-function mevedel-collaboration-discussions--load "mevedel-collaboration-discussions" (item))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-list-sessions "mevedel-session-persistence" (workspace &optional cached))
(autoload 'mevedel-session-persistence-list-sessions "mevedel-session-persistence")

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read "mevedel-session-publication" (session-dir &optional head names))
(autoload 'mevedel-session-publication-read "mevedel-session-publication")

(defun mevedel-collaboration-artifact-history--source (room item discussion)
  "Return (SESSION . LIVE-BUFFER) for ITEM's DISCUSSION in ROOM, or nil.
Cold sessions use minimal metadata solely for disposable transcript inspection."
  (if-let* ((session (plist-get room :session)))
      (cons session (plist-get room :data-buffer))
    (let* ((workspace (plist-get item :workspace))
           (meta (mevedel-artifact-store-meta workspace (plist-get item :store)))
           (ids (delete-dups (delq nil (list (plist-get discussion :session)
                                            (plist-get meta :dedicated-session)))))
           (live-sessions (and ids (mevedel--workspace-sessions workspace))))
      (catch 'found
        (dolist (id ids)
          (if-let* ((live (cl-find-if
                          (lambda (entry)
                            (equal id (mevedel-session-session-id
                                       (buffer-local-value 'mevedel--session (cdr entry)))))
                          live-sessions)))
              (throw 'found (cons (buffer-local-value 'mevedel--session (cdr live)) (cdr live)))
            (let* ((matches (lambda (entry)
                              (equal id (plist-get (plist-get entry :summary) :session-id))))
                   (entry (or (cl-find-if matches (mevedel-session-persistence-list-sessions workspace t))
                              (cl-find-if matches (mevedel-session-persistence-list-sessions workspace))))
                   (path (plist-get entry :save-path)))
              (when path
                (let* ((publication (and (plist-get entry :publication)
                                         (or (mevedel-session-publication-read path)
                                             (error "The answering session is unavailable"))))
                       (sidecar (mevedel-session-codec-read
                                 (or (plist-get publication :sidecar)
                                     (file-name-concat path "session.meta.el"))))
                       (number (plist-get sidecar :current-segment)))
                  (mevedel-session-codec-validate-current-sidecar sidecar)
                  (unless (and (equal (plist-get sidecar :version) mevedel-session-codec-format-version)
                               (equal id (plist-get sidecar :session-id))
                               (integerp number) (> number 0))
                    (error "The answering session is unavailable"))
                  ;; Inspection needs neither restored permissions nor execution
                  ;; targets. Full deserialization may ask about relocated authority.
                  (throw 'found
                         (cons (mevedel-session--create
                                :session-id id :workspace workspace :save-path path
                                :current-segment number :publication publication
                                :authority-mode (plist-get sidecar :authority-mode)
                                :working-directory (mevedel-workspace-root workspace))
                               nil)))))))
        (when ids (error "The answering session is unavailable"))))))

(defun mevedel-collaboration-artifact-history--identity (session live)
  "Return constant-size source identities for SESSION and optional LIVE.
Finalized archives are immutable.  Publication heads identify portable archive
changes; the session directory timestamp catches PID archive replacement,
addition and deletion.  Only the mutable current segment needs its own probe."
  (let* ((path (mevedel-session-save-path session))
         (number (or (mevedel-session-current-segment session) 1)))
    (list path number
          (and (buffer-live-p live)
               (with-current-buffer live (list live (buffer-chars-modified-tick))))
          (plist-get (mevedel-session-publication session) :head)
          (and path (file-attribute-modification-time (file-attributes path)))
          (mevedel-session-artifacts-transcript-segment-identity
           session (list :number number)))))

(defun mevedel-collaboration-artifact-history--project (session live item)
  "Return bounded canonical history for ITEM in SESSION, preferring LIVE.
Keep archive-read diagnostics local; browser errors name only logical segments."
  (cl-flet ((unavailable (err &optional number)
              (message "mevedel: artifact history read failed: %S" err)
              (if number
                  (user-error "Session history segment %d is unavailable" number)
                (user-error "An answering-session transcript segment is unavailable"))))
    (let* ((number (or (mevedel-session-current-segment session) 1))
           (cold (unless (buffer-live-p live)
                   (condition-case err
                       (mevedel-session-artifacts-read-segment session number)
                     (error (unavailable err number))))))
      (unwind-protect
          (let* ((history
                  (condition-case err
                      (mevedel-shared-conversation-history
                       session item :live-buffer (or cold live)
                       :limit mevedel-shared-conversation--history-limit)
                    (error (unavailable err))))
                 (turns (reverse (plist-get history :turns))))
            (with-temp-buffer
              (delay-mode-hooks (org-mode))
              (setq-local mevedel--session session)
              (dolist (turn turns) (insert (plist-get turn :text) "\n"))
              (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
                (dolist (record records)
                  (plist-put record :id (format "item-history:%s:%s"
                                               (mevedel-session-session-id session)
                                               (plist-get record :id))))
                (list :conversation (vconcat records)
                      :conversationTruncated (if (plist-get history :truncated) t :json-false)))))
        (when cold (kill-buffer cold))))))

(defun mevedel-collaboration-artifact-history (room guest item args)
  "Read ITEM's selected discussion history in ROOM for GUEST using ARGS.
Keep at most four bounded projections per room.  An unchanged source avoids
archive reads and projection; a matching digest also avoids retransmission."
  (mevedel-collaboration--store-read guest)
  (when (plist-get item :shared) (error "Open this item's editor to read its history"))
  (let* ((discussion-id (or (plist-get args :discussionId) "general"))
         (discussion (cl-find discussion-id (mevedel-collaboration-discussions--load item)
                              :key (lambda (d) (plist-get d :id)) :test #'equal)))
    (unless (or discussion (equal discussion-id "general")) (error "No such discussion"))
    (let* ((source (mevedel-collaboration-artifact-history--source room item discussion))
           (session (car source)) (live (cdr source))
           (key (list (and session (mevedel-session-session-id session)) (plist-get item :item)))
           (identity (and session (mevedel-collaboration-artifact-history--identity session live)))
           (cache (plist-get room :artifact-conversations))
           (entry (assoc key cache))
           (result (if (and entry (equal identity (cadr entry))) (caddr entry)
                     (let* ((result (if session
                                        (mevedel-collaboration-artifact-history--project
                                         session live (plist-get item :item))
                                      (list :conversation [] :conversationTruncated :json-false)))
                            (encoded
                             (progn
                               ;; Canonical records also carry host-only lookup
                               ;; fields. Use the room's wire allowlist here too.
                               (setq result
                                     (plist-put
                                      result :conversation
                                      (vconcat
                                       (mapcar #'mevedel-collaboration--json-record
                                               (append (plist-get result :conversation) nil)))))
                               (json-encode result))))
                       (when (> (string-bytes encoded)
                                (- mevedel-collaboration--max-frame-json-bytes 2048))
                         (error "This item's history is too large to display"))
                       (setq result (append result (list :conversationDigest
                                                        (secure-hash 'sha256 encoded))))
                       (setq cache (cons (list key identity result) (delete entry cache)))
                       (when (> (length cache) 4) (setcdr (nthcdr 3 cache) nil))
                       (plist-put room :artifact-conversations cache)
                       result)))
           ;; Interaction registration does not modify the data transcript.
           ;; Keep its small signature outside the cached transcript projection.
           (decision-state
            (when (and session (not (plist-get room :session))
                       (equal (mevedel-session-dedicated-artifact session) (plist-get item :store))
                       (buffer-live-p live))
              (with-current-buffer live
                (when (and (buffer-live-p mevedel--view-buffer)
                           (mevedel-view-interaction-pending-p mevedel--view-buffer))
                  (with-current-buffer mevedel--view-buffer
                    (list (truncate-string-to-width (mevedel-session-name session) 160 nil nil "...")
                          mevedel-view--interaction-arrivals
                          (and (hash-table-p mevedel-view--interaction-descriptors)
                               (sort (mapcar #'prin1-to-string
                                             (hash-table-keys mevedel-view--interaction-descriptors))
                                     #'string<))
                          (and (bound-and-true-p mevedel-view--prompt-hook-pending) t)))))))
           (digest (if decision-state
                       (secure-hash 'sha256 (concat (plist-get result :conversationDigest)
                                                   (prin1-to-string decision-state)))
                     (plist-get result :conversationDigest))))
      (append (list :item (plist-get item :item) :discussionId discussion-id)
              (if (equal (plist-get args :digest) digest)
                  (list :conversationUnchanged t
                        :conversationDigest digest)
                (append (list :conversationDecision
                              (and decision-state (list :pending t :title (car decision-state))))
                        (plist-put (copy-sequence result) :conversationDigest digest)))))))

(provide 'mevedel-collaboration-artifact-history)
;;; mevedel-collaboration-artifact-history.el ends here
