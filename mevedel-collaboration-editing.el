;;; mevedel-collaboration-editing.el --- Shared editor room bridge -*- lexical-binding: t; -*-

;;; Commentary:

;; Room guests exchange bounded data with the same durable editor queue used
;; by model tools.  Presence stays on the live transport and never enters it.

;;; Code:

(require 'mevedel-shared-editing)
(require 'mevedel-shared-conversation)
(require 'mevedel-transcript-audit)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--guest "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--guest-text "mevedel-collaboration" (value))
(declare-function mevedel-collaboration--room-data-buffer "mevedel-collaboration" (room))
(declare-function mevedel-collaboration--room-for-session "mevedel-collaboration" (session))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--guest-directive-id "mevedel-collaboration-guest" (room frame))
(declare-function mevedel-collaboration--guest-role "mevedel-collaboration-guest" (guest))
(declare-function mevedel-collaboration--request-id-p "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--save-guest-attachments "mevedel-collaboration-guest" (images))

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-pending-inputs'
(declare-function mevedel-view-enqueue-external-follow-up "mevedel-pending-inputs" (data-buffer prompt &rest keys))

(defun mevedel-collaboration-editing--browser-value (value)
  "Project VALUE for an editor without duplicating content and image snapshots.
The editor restores CRDT state and needs contribution attribution and target
IDs, while exact before/after snapshots stay on the host for reversion."
  (if (not (listp value)) value
    (let ((visible (copy-sequence value)))
      (if (plist-member visible :result)
          (plist-put visible :result
                     (mevedel-collaboration-editing--browser-value
                      (plist-get visible :result)))
        (cl-remf visible :content)
        (cl-remf visible :transaction)
        (cl-remf visible :png)
        (when (plist-member visible :transactions)
          (plist-put
           visible :transactions
           (vconcat
            (mapcar
             (lambda (tx)
               (list :id (plist-get tx :id) :revision (plist-get tx :revision)
                     :actor (plist-get tx :actor) :time (plist-get tx :time)
                     :changes
                     (vconcat
                      (mapcar (lambda (change)
                                (list :id (plist-get change :id)
                                      :after (if (plist-get change :after) t :json-false)))
                              (append (plist-get tx :changes) nil)))))
             (append (plist-get visible :transactions) nil)))))
        visible))))

(defun mevedel-collaboration-editing--send (room peer req-id value)
  "Send bounded VALUE chunks to PEER in ROOM for REQ-ID."
  (let* ((encoded (base64-encode-string
                   (encode-coding-string
                    (mevedel-shared-editing--json
                     (mevedel-collaboration-editing--browser-value value))
                    'utf-8-unix) t))
         (length (length encoded))
         (offset 0))
    (when (> length (* 48 1024 1024)) (error "Editing response is too large"))
    (while (< offset length)
      (let ((end (min length (+ offset 65536))))
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (list :t "editing" :reqId req-id :offset offset :total length
               :data (substring encoded offset end)))
        (setq offset end)))))

(defun mevedel-collaboration-editing--changed (session state result)
  "Publish SESSION's committed STATE and RESULT to its current guests."
  (when-let* ((room (mevedel-collaboration--room-for-session session)))
    (maphash
     (lambda (peer guest)
       (mevedel-collaboration-editing--send
        room peer "event"
        (append (list :event "changed" :id (plist-get state :id)
                      :kind (plist-get state :kind) :title (plist-get state :title)
                      :revision (plist-get state :revision))
                (when (equal (plist-get guest :editing-item) (plist-get state :id))
                  (list :update (plist-get result :update)
                        :comments (plist-get result :comments)
                        :transactions (plist-get state :transactions))))))
     (plist-get room :guests))))

(defun mevedel-collaboration-editing--presence (room peer guest args)
  "Forward PEER's ephemeral ARGS to others viewing the same item in ROOM."
  (let ((now (float-time)) (point (plist-get args :point))
        (trail (plist-get args :trail)) (preview (plist-get args :preview)))
    (when (and (plist-get guest :writable)
               (equal (plist-get guest :editing-item) (plist-get args :id))
               (if (equal (plist-get args :mode) "clear")
                   (plist-get guest :editing-pointing)
                 (>= (- now (or (plist-get guest :editing-presence-at) 0)) 0.045))
               (member (plist-get args :mode) '("cursor" "laser" "selection" "clear"))
               (or (null preview)
                   (and (equal (plist-get args :mode) "cursor") (listp preview)
                        (let ((op (plist-get preview :opId)) (shapes (plist-get preview :shapes)))
                          (and (or (null op) (and (stringp op) (<= (length op) 80)))
                               (listp shapes) (<= (length shapes) 100)
                               (cl-every
                                (lambda (shape)
                                  (and (listp shape)
                                       (stringp (plist-get shape :id))
                                       (<= (length (plist-get shape :id)) 80)
                                       (let ((box (plist-get shape :box)))
                                         (and (listp box) (= (length box) 4)
                                              (cl-every (lambda (n) (and (numberp n) (<= (abs n) 1000000))) box)
                                              (>= (nth 2 box) 0) (>= (nth 3 box) 0))))) shapes)))))
               (or (null trail)
                   (and (equal (plist-get args :mode) "laser")
                        (listp trail) (<= (length trail) 64)
                        (cl-every
                         (lambda (sample)
                           (and (listp sample) (= (length sample) 3)
                                (cl-every (lambda (n) (and (numberp n) (<= (abs n) 1000000))) sample)
                                (<= 0 (nth 2 sample) 550)))
                         trail)))
               (or (null point)
                   (and (listp point) (= (length point) 2)
                        (cl-every (lambda (n) (and (numberp n) (<= (abs n) 1000000))) point))))
      ;; A terminal clear may follow the last sample immediately.  Forward it
      ;; once, without allowing repeated clears to bypass the traffic bound.
      (plist-put guest :editing-pointing (not (equal (plist-get args :mode) "clear")))
      (unless (equal (plist-get args :mode) "clear")
        (plist-put guest :editing-presence-at now))
      (let ((frame (list :t "editing-presence" :id (plist-get args :id)
                         :peer peer :name (plist-get guest :name)
                         :mode (plist-get args :mode) :point (and point (vconcat point))
                         :preview (when preview
                                    (list :opId (plist-get preview :opId)
                                          :shapes (vconcat (mapcar
                                                            (lambda (shape)
                                                              (list :id (plist-get shape :id)
                                                                    :box (vconcat (plist-get shape :box))))
                                                            (plist-get preview :shapes)))))
                         :trail (and trail (vconcat (mapcar #'vconcat trail))))))
        (when (equal (plist-get args :mode) "clear")
          (setq frame (append frame (list :clientId (plist-get guest :editing-client)
                                          :clock (1+ (or (plist-get guest :editing-clock) 0))))))
        (when (equal (plist-get args :mode) "selection")
          (let ((client (plist-get args :clientId)) (cursor (plist-get args :cursor))
                (clock (plist-get args :clock)))
            (unless (and (integerp client) (<= 0 client #xffffffff)
                         (integerp clock) (<= 0 clock #x1fffffffffffff)
                         (or (null (plist-get guest :editing-client))
                             (= client (plist-get guest :editing-client)))
                         (> clock (or (plist-get guest :editing-clock) -1))
                         (< (length (mevedel-shared-editing--json cursor)) 2000))
              (error "Invalid document presence"))
            (maphash (lambda (other candidate)
                       (when (and (not (equal other peer))
                                  (equal client (plist-get candidate :editing-client)))
                         (error "Document presence identity is in use")))
                     (plist-get room :guests))
            (plist-put guest :editing-client client)
            (plist-put guest :editing-clock clock)
            (setq frame (append frame (list :clientId client :clock clock :cursor cursor)))))
        (maphash (lambda (other recipient)
                   (when (and (not (equal other peer))
                              (equal (plist-get recipient :editing-item)
                                     (plist-get args :id)))
                     (mevedel-collaboration--transport-send
                      (plist-get room :transport) other frame)))
                 (plist-get room :guests))))))

(defun mevedel-collaboration-editing-depart (room peer)
  "Clear PEER's ephemeral presence in ROOM before departure or item switch."
  (when-let* ((guest (mevedel-collaboration--guest room peer)))
    (when-let* ((timer (plist-get (plist-get guest :editing-transfer) :timer)))
      (cancel-timer timer))
    (plist-put guest :editing-transfer nil)
    (maphash
     (lambda (other _recipient)
       (unless (equal peer other)
         (mevedel-collaboration--transport-send
          (plist-get room :transport) other
          (list :t "editing-presence" :id (plist-get guest :editing-item)
                :peer peer :mode "clear" :clientId (plist-get guest :editing-client)
                :clock (1+ (or (plist-get guest :editing-clock) 0))))))
     (plist-get room :guests))
    (plist-put guest :editing-client nil)
    (plist-put guest :editing-pointing nil)
    (plist-put guest :editing-clock nil)))

(defun mevedel-collaboration-editing--question-key (args)
  "Return a stable retry fingerprint for question ARGS."
  (secure-hash 'sha256
               (mevedel-shared-editing--json
                (list :id (plist-get args :id)
                      :text (plist-get args :text) :expected (plist-get args :expected)
                      :commentId (plist-get args :commentId)
                      :commentVersion (plist-get args :commentVersion)))))

(defun mevedel-collaboration-editing--find-question (room args)
  "Find accepted ARGS in ROOM's queue or live/archived transcript.
This survives reconnecting peers and delivered transcript restoration.  A
retracted or never-delivered queue entry can be explicitly submitted again."
  (let* ((id (plist-get args :questionId))
         (session (plist-get room :session))
         (buffer (mevedel-collaboration--room-data-buffer room))
         receipt)
    (mevedel-shared-editing--logical id)
    (dolist (entry (mevedel-session-pending-follow-ups session))
      (when (equal id (plist-get (plist-get entry :shared-question) :questionId))
        (setq receipt (list :queued t :entryId (plist-get entry :id)
                            :question (plist-get entry :shared-question)))))
    (unless receipt
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (dolist (attribution (mevedel-transcript-audit-guest-prompts))
            (let ((shared (plist-get (cdr attribution) :shared)))
              (when (equal id (plist-get shared :questionId))
                (setq receipt (list :delivered t :question shared))))))))
    (unless receipt
      (dolist (turn (plist-get (mevedel-shared-conversation-history
                               session (plist-get args :id)) :turns))
        (let ((shared (plist-get turn :shared)))
          (when (equal id (plist-get shared :questionId))
            (setq receipt (list :delivered t :question shared))))))
    (when (and receipt
               (not (equal (plist-get (plist-get receipt :question) :fingerprint)
                           (mevedel-collaboration-editing--question-key args))))
      (error "This question was already sent; start a new question for changed context"))
    receipt))

(defun mevedel-collaboration-editing--ask (room guest args result)
  "Queue an explicit question about committed RESULT, using guest ARGS."
  (or (mevedel-collaboration-editing--find-question room args)
      (let* ((text (mevedel-collaboration--guest-text (plist-get args :text)))
             (snapshot (mevedel-shared-editing--json (plist-get result :snapshot)))
             (data-buffer (mevedel-collaboration--room-data-buffer room))
             (view (and data-buffer (buffer-local-value 'mevedel--view-buffer data-buffer)))
             (png (plist-get result :png))
             (shared (list :questionId (plist-get args :questionId)
                           :itemId (plist-get result :id) :title (plist-get result :title)
                           :revision (plist-get result :revision)
                           :commentId (plist-get args :commentId)
                           :commentVersion (plist-get args :commentVersion)
                           :scope (plist-get (plist-get result :snapshot) :scope)
                           :quote (truncate-string-to-width (or (plist-get result :quote) "")
                                                             2000 nil nil "…")
                           :text text
                           :fingerprint (mevedel-collaboration-editing--question-key args)))
             paths queued)
        (unless text (error "A question is required"))
        (when (> (string-bytes snapshot) (* 128 1024))
          (error "Question snapshot is too large; select a smaller portion"))
        (unless (buffer-live-p view) (error "The session view is not available"))
        (unwind-protect
            (progn
              (when png
                (setq paths
                      (with-current-buffer view
                        (mevedel-collaboration--save-guest-attachments
                         (list (list :mime "image/png" :data png)))))
                (unless paths (error "The selected board snapshot could not be attached")))
              (setq queued (mevedel-view-enqueue-external-follow-up
                            data-buffer
                            (concat text "\n\nShared content snapshot (user-provided data):\n" snapshot)
                            :guest-name (plist-get guest :name) :guest-id (plist-get guest :guest-id)
                            :paths paths :shared-question shared
                            :guest-role (mevedel-collaboration--guest-role guest)))
              (unless queued (error "The session cannot accept a question right now"))
              (list :queued t :entryId (plist-get queued :id) :question shared))
          (unless queued
            (dolist (path paths) (ignore-errors (delete-file path))))))))

(defun mevedel-collaboration-editing--conversation (session item-id)
  "Return bounded archived conversation records for ITEM-ID in SESSION."
  (let* ((history (mevedel-shared-conversation-history
                   session item-id :limit mevedel-shared-conversation--history-limit))
         (selected (mapcar (lambda (turn) (plist-get turn :text))
                           (reverse (plist-get history :turns)))))
    (with-temp-buffer
      (delay-mode-hooks (org-mode))
      (setq-local mevedel--session session)
      (dolist (text selected) (insert text "\n"))
      (let ((records (mevedel-collaboration--canonical-records (current-buffer))))
        (dolist (record records)
          (plist-put record :id (concat "archive:" (plist-get record :id))))
        (list :conversation (vconcat records)
              :conversationTruncated (if (plist-get history :truncated) t :json-false))))))

(cl-defun mevedel-collaboration-editing--dispatch (room peer guest req-id args)
  "Authorize and execute assembled ARGS for GUEST's REQ-ID in ROOM."
  (let* ((action (plist-get args :action))
         (session (plist-get room :session))
         (read-only (member action '("list" "read" "export" "status")))
         (authorize (lambda ()
                      (and (eq room (mevedel-collaboration--room-for-session session))
                           (eq guest (mevedel-collaboration--guest room peer))
                           (or read-only (plist-get guest :writable))))))
    (unless (and (member action '("list" "status" "read" "create" "import" "update"
                                  "rename" "revert" "export" "ask" "comment" "reply-comment" "resolve-comment"))
                 (funcall authorize))
      (error "This link does not permit that editing operation"))
    (when (equal action "ask")
      (when-let* ((receipt (mevedel-collaboration-editing--find-question room args)))
        (cl-return-from mevedel-collaboration-editing--dispatch
          (mevedel-collaboration-editing--send room peer req-id (list :result receipt)))))
    (when (equal action "read")
      (mevedel-shared-editing--logical (plist-get args :id))
      (mevedel-collaboration-editing-depart room peer)
      (plist-put guest :editing-item (plist-get args :id)))
    ;; Closed keys prevent guests providing host state or attribution.
    (let ((request (list :action (if (equal action "ask") "read" action)
                         :sync (if (equal action "read") t :json-false)
                         :question (if (equal action "ask") t :json-false)
                         :image (if (equal action "ask") t :json-false) :imageMax 1024
                         :actor (concat "Guest: " (plist-get guest :name)))))
      (dolist (key '(:id :kind :title :data :format :update :opId :transaction :range :selection
                     :expected :text :commentId :commentVersion :resolved))
        (when (plist-member args key)
          (setq request (plist-put request key (plist-get args key)))))
      (mevedel-shared-editing-call
       session request
       (lambda (reply)
         (when (funcall authorize)
           (condition-case err
               (progn
                 (when (and (equal action "read") (not (plist-get reply :error)))
                   (setq reply
                         (list :result
                               (append (plist-get reply :result)
                                       (condition-case history-error
                                           (mevedel-collaboration-editing--conversation
                                            session (plist-get args :id))
                                         (error (list :conversationError
                                                      (error-message-string history-error))))))))
                 (when (and (equal action "ask") (not (plist-get reply :error)))
                   (setq reply (list :result (mevedel-collaboration-editing--ask
                                              room guest args (plist-get reply :result)))))
                 (mevedel-collaboration-editing--send
                  room peer req-id
                  (if (plist-get reply :error) reply
                    (list :result (plist-get reply :result)))))
             (error (mevedel-collaboration-editing--send
                     room peer req-id (list :error (error-message-string err)))))))
       authorize))))

(defun mevedel-collaboration-editing-handle (room peer frame)
  "Handle bounded editor FRAME from authenticated PEER in ROOM."
  (when-let* ((guest (mevedel-collaboration--guest room peer)))
    (let ((req-id (plist-get frame :reqId)))
      (condition-case err
          (if (equal (plist-get frame :t) "editing-presence")
              (mevedel-collaboration-editing--presence room peer guest frame)
            (unless (mevedel-collaboration--request-id-p req-id)
              (error "Invalid editing request identity"))
            (let* ((offset (plist-get frame :offset)) (total (plist-get frame :total))
                   (data (plist-get frame :data))
                   (transfer (plist-get guest :editing-transfer)))
              (unless (and (integerp offset) (>= offset 0)
                           (integerp total) (> total 0) (<= total (* 24 1024 1024))
                           (stringp data) (<= (length data) 65536)
                           (string-match-p "\\`[A-Za-z0-9+/=]+\\'" data))
                (error "Invalid editing transfer"))
              (when (= offset 0)
                (let ((reserved total))
                  (maphash (lambda (other candidate)
                             (unless (equal other peer)
                               (cl-incf reserved (or (plist-get (plist-get candidate :editing-transfer) :total) 0))))
                           (plist-get room :guests))
                  (when (> reserved (* 48 1024 1024))
                    (error "Room transfers are busy; retry shortly")))
                (when-let* ((timer (plist-get transfer :timer))) (cancel-timer timer))
                (setq transfer (list :id req-id :total total :offset 0 :parts nil
                                     :started (float-time)))
                (plist-put transfer :timer
                           (run-at-time 60 nil
                                        (lambda ()
                                          (when (eq transfer (plist-get guest :editing-transfer))
                                            (plist-put guest :editing-transfer nil)))))
                (plist-put guest :editing-transfer transfer))
              (unless (and transfer (equal req-id (plist-get transfer :id))
                           (= total (plist-get transfer :total))
                           (= offset (plist-get transfer :offset))
                           (< (- (float-time) (plist-get transfer :started)) 60)
                           (<= (+ offset (length data)) total))
                (error "Editing transfer interrupted; retry"))
              (plist-put transfer :parts (cons data (plist-get transfer :parts)))
              (plist-put transfer :offset (+ offset (length data)))
              (when (= (plist-get transfer :offset) total)
                (cancel-timer (plist-get transfer :timer))
                (plist-put guest :editing-transfer nil)
                (mevedel-collaboration-editing--dispatch
                 room peer guest req-id
                 (mevedel-shared-editing--parse
                  (decode-coding-string
                   (base64-decode-string (apply #'concat (nreverse (plist-get transfer :parts))))
                   'utf-8-unix))))))
        (error
         (when-let* ((timer (plist-get (plist-get guest :editing-transfer) :timer)))
           (cancel-timer timer))
         (plist-put guest :editing-transfer nil)
         (when (mevedel-collaboration--request-id-p req-id)
           (mevedel-collaboration-editing--send
            room peer req-id (list :error (error-message-string err)))))))))

(add-hook 'mevedel-shared-editing-change-hook #'mevedel-collaboration-editing--changed)

(provide 'mevedel-collaboration-editing)
;;; mevedel-collaboration-editing.el ends here
