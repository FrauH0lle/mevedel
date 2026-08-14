;;; mevedel-collaboration.el --- read-only browser collaboration -*- lexical-binding: t; -*-

;;; Commentary:

;; Owns the process-wide room, public commands, and gptel lifecycle hooks.
;; Canonical projection and loopback WebSocket transport live in the focused
;; collaboration projection and transport modules.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(defvar gptel-post-tool-call-functions)
(defvar gptel-pre-tool-call-functions)

;; `mevedel-chat'
(defvar mevedel-session-end-hook)

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection" (data-buffer))
(declare-function mevedel-collaboration--json-record
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--json-string
                  "mevedel-collaboration-projection" (object))
(declare-function mevedel-collaboration--pending-tool-match
                  "mevedel-collaboration-projection" (info pending))
(declare-function mevedel-collaboration--project-records
                  "mevedel-collaboration-projection" (room))
(declare-function mevedel-collaboration--record
                  "mevedel-collaboration-projection" (id kind &rest fields))
(declare-function mevedel-collaboration--record-without-revision
                  "mevedel-collaboration-projection" (record))
(declare-function mevedel-collaboration--reuse-record-ids
                  "mevedel-collaboration-projection" (old new))
(declare-function mevedel-collaboration--stable-record-id
                  "mevedel-collaboration-projection"
                  (kind text &optional occurrence))
(declare-function mevedel-collaboration--tool-call-key
                  "mevedel-collaboration-projection" (info))
(declare-function mevedel-collaboration--tool-records
                  "mevedel-collaboration-projection" (records))
(declare-function mevedel-collaboration--tool-result-fields
                  "mevedel-collaboration-projection" (result))
(defvar mevedel-collaboration--protocol-version)

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--finish-stop
                  "mevedel-collaboration-transport" (guest server reason))
(declare-function mevedel-collaboration--preupgrade-stop
                  "mevedel-collaboration-transport" (server))
(declare-function mevedel-collaboration--ws-start
                  "mevedel-collaboration-transport" (handler port))
(declare-function mevedel-collaboration--guest-close
                  "mevedel-collaboration-transport" (guest &optional reason))
(declare-function mevedel-collaboration--guest-send
                  "mevedel-collaboration-transport" (guest object))
(declare-function mevedel-collaboration--guest-send-immediate
                  "mevedel-collaboration-transport" (guest object))
(declare-function mevedel-collaboration--request-handler
                  "mevedel-collaboration-transport" (request))
(declare-function mevedel-collaboration--room-pending-auth
                  "mevedel-collaboration-transport" (room))
(declare-function mevedel-collaboration--snapshot-chunks
                  "mevedel-collaboration-transport" (records))
(declare-function mevedel-collaboration--web-server-slot
                  "mevedel-collaboration-transport" (object slot))

;; `mevedel-structs'
(declare-function mevedel-session-session-id "mevedel-structs" (session))

;; `url-parse'
(declare-function url-filename "url-parse" (url-struct))
(declare-function url-generic-parse-url "url-parse" (url))
(declare-function url-host "url-parse" (url-struct))
(declare-function url-password "url-parse" (url-struct))
(declare-function url-target "url-parse" (url-struct))
(declare-function url-type "url-parse" (url-struct))
(declare-function url-user "url-parse" (url-struct))

;; `web-server'
(declare-function ws-start "web-server" (handlers port &optional log-buffer &rest network-args))
(declare-function ws-stop "web-server" (server))

;;
;;; Customization and state

(defcustom mevedel-collaboration-public-base-url nil
  "HTTPS origin exposed by an operator-managed collaboration tunnel.

When nil, collaboration links use the local loopback origin and are for local
testing only.  The value is an origin, not a path: for example,
`https://collab.example.net'.  Mevedel does not start or manage a tunnel."
  :type '(choice (const :tag "Local loopback only" nil)
                 (string :tag "HTTPS origin"))
  :group 'mevedel)

(defconst mevedel-collaboration--max-snapshot-bytes (* 8 1024 1024))
(defconst mevedel-collaboration--publish-delay 0.1)

(defvar mevedel-collaboration--room nil
  "The one process-wide live collaboration room, or nil.")

;;
;;; Small data helpers

(defun mevedel-collaboration--room-data-buffer (&optional room)
  "Return the live data buffer for ROOM, or nil."
  (let ((buffer (plist-get (or room mevedel-collaboration--room)
                           :data-buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun mevedel-collaboration--current-data-buffer ()
  "Return the data buffer associated with the current command context."
  (cond
   ((and (boundp 'mevedel--data-buffer)
         (buffer-live-p mevedel--data-buffer))
    mevedel--data-buffer)
   ((and (boundp 'mevedel--session)
         mevedel--session
         (local-variable-p 'mevedel--session)
         (not (local-variable-p 'mevedel--view-buffer)))
    (current-buffer))
   ((and (boundp 'mevedel--view-buffer)
         (buffer-live-p mevedel--view-buffer))
    (with-current-buffer mevedel--view-buffer
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           mevedel--data-buffer)))))

(defun mevedel-collaboration--session-label (session data-buffer)
  "Return a safe display label for SESSION and DATA-BUFFER."
  (or (and (fboundp 'mevedel-session-session-id)
           session
           (mevedel-session-session-id session))
      (and (buffer-live-p data-buffer) (buffer-name data-buffer))
      "session"))

(defun mevedel-collaboration--normalize-public-origin ()
  "Return the configured public origin, or nil.
Signal `user-error' for a configured URL that is not an HTTPS origin."
  (let ((value (and (stringp mevedel-collaboration-public-base-url)
                    (string-trim-right
                     mevedel-collaboration-public-base-url "/"))))
    (when (and value (not (string-empty-p value)))
      (require 'url-parse)
      (let* ((url (url-generic-parse-url value))
             (type (url-type url))
             (host (url-host url))
             (filename (url-filename url))
             (target (url-target url))
             (user (url-user url))
             (password (url-password url)))
        (unless (and (equal type "https")
                     host
                     (null user)
                     (null password)
                     (or (null filename) (equal filename ""))
                     (null target))
          (user-error
           "'mevedel-collaboration-public-base-url' must be an HTTPS origin")))
      value)))

(defun mevedel-collaboration--random-bytes (count)
  "Return COUNT bytes from the operating system random source."
  (condition-case error-data
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally "/dev/urandom" nil nil count)
        (let ((bytes (buffer-string)))
          (if (= (string-bytes bytes) count)
              bytes
            (error "Short read from OS random source"))))
    (error
     (user-error "Cannot start collaboration: OS random source unavailable (%s)"
                 (error-message-string error-data)))))

(defun mevedel-collaboration--random-token ()
  "Return an opaque URL-safe 256-bit bearer token."
  (replace-regexp-in-string
   "=+$" ""
   (replace-regexp-in-string
    "/" "_"
    (replace-regexp-in-string
     "\\+" "-"
     (base64-encode-string
      (mevedel-collaboration--random-bytes 32) t)))))

;;
;;; Room publication

(defun mevedel-collaboration--publish (&optional room)
  "Publish changed records from ROOM to its authenticated guest."
  (setq room (or room mevedel-collaboration--room))
  (when room
    (let* ((old (plist-get room :records))
           (new (mevedel-collaboration--reuse-record-ids
                 old (mevedel-collaboration--project-records room)))
           (old-by-id (make-hash-table :test #'equal))
           (new-by-id (make-hash-table :test #'equal))
           changed removed)
      (dolist (record old)
        (puthash (plist-get record :id) record old-by-id))
      (dolist (record new)
        (let* ((id (plist-get record :id))
               (previous (gethash id old-by-id))
               (revision (if previous
                             (or (plist-get previous :revision) 0)
                           0)))
          (unless (and previous
                       (equal (mevedel-collaboration--record-without-revision
                               previous)
                              (mevedel-collaboration--record-without-revision
                               record)))
            (setq revision (1+ revision))
            (push (setq record (plist-put record :revision revision)) changed))
          (puthash id record new-by-id)))
      (dolist (record old)
        (unless (gethash (plist-get record :id) new-by-id)
          (push (plist-get record :id) removed)))
      (setq changed (nreverse changed)
            removed (nreverse removed))
      (setq new (mapcar (lambda (record)
                          (or (gethash (plist-get record :id) new-by-id)
                              record))
                        new))
      (setq room (plist-put room :records new))
      (setq mevedel-collaboration--room room)
      (when-let ((guest (plist-get room :guest)))
        (when (plist-get guest :authenticated)
          (dolist (record changed)
            (mevedel-collaboration--guest-send
             guest `(("type" . "record")
                     ("version" . ,mevedel-collaboration--protocol-version)
                     ("record" . ,(mevedel-collaboration--json-record record)))))
          (when removed
            (mevedel-collaboration--guest-send
             guest `(("type" . "remove")
                     ("version" . ,mevedel-collaboration--protocol-version)
                     ("ids" . ,removed)))))))))

(defun mevedel-collaboration--publish-timer ()
  "Run the coalesced collaboration publication timer."
  (let ((room mevedel-collaboration--room))
    (when room
      (setq mevedel-collaboration--room
            (plist-put room :publish-timer nil))
      (mevedel-collaboration--publish room))))

(defun mevedel-collaboration--safe-accepted-prompt (data-buffer)
  "Publish DATA-BUFFER immediately after an accepted prompt is inserted.

This observer is failure-isolated so a collaboration viewer cannot block the
request or prompt transaction."
  (condition-case nil
      (when-let ((room mevedel-collaboration--room))
        (when (eq data-buffer (plist-get room :data-buffer))
          (mevedel-collaboration--publish room)))
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--schedule-publish ()
  "Coalesce assistant stream updates for the active room."
  (when-let ((room mevedel-collaboration--room))
    (unless (plist-get room :publish-timer)
      (setq mevedel-collaboration--room
            (plist-put room :publish-timer
                       (run-at-time mevedel-collaboration--publish-delay nil
                                    #'mevedel-collaboration--publish-timer))))))

;;
;;; Room lifecycle

(defun mevedel-collaboration--snapshot-size (records)
  "Return the serialized size in bytes of RECORDS."
  (string-bytes
   (mevedel-collaboration--json-string
    `(("type" . "snapshot")
      ("version" . ,mevedel-collaboration--protocol-version)
      ("records" . ,(mapcar #'mevedel-collaboration--json-record records))))))

(defun mevedel-collaboration--stop-internal (&optional reason)
  "Stop the active room and all associated processes and timers."
  (when-let ((room mevedel-collaboration--room))
    ;; Clear the authority before any teardown operation can signal.  The
    ;; local ROOM still supplies the process and guest to close below.
    (setq mevedel-collaboration--room nil)
    (remove-hook 'kill-emacs-hook #'mevedel-collaboration--stop-for-emacs)
    (when-let ((data-buffer (plist-get room :data-buffer)))
      (when (buffer-live-p data-buffer)
        (with-current-buffer data-buffer
          (remove-hook 'gptel-pre-tool-call-functions
                       #'mevedel-collaboration--safe-pre-tool)
          (remove-hook 'gptel-post-tool-call-functions
                       #'mevedel-collaboration--safe-post-tool))))
    (when-let ((timer (plist-get room :publish-timer)))
      (cancel-timer timer))
    (let ((guest (plist-get room :guest))
          (pending (plist-get room :pending-auth))
          (server (plist-get room :server)))
      (dolist (pending-guest pending)
        (mevedel-collaboration--guest-close pending-guest 'room-stopped))
      (if (and guest (plist-get guest :authenticated)
               (not (eq reason 'emacs-exit)))
          (progn
            (mevedel-collaboration--guest-send-immediate
             guest `(("type" . "status")
                     ("version" . ,mevedel-collaboration--protocol-version)
                     ("status" . "ended")))
            ;; `ws-stop' deletes request processes, so give the tiny final
            ;; frame one event-loop turn before closing the guest and server.
            (let ((timer (run-at-time 0.05 nil
                                      #'mevedel-collaboration--finish-stop
                                      guest server reason)))
              (setf (plist-get guest :close-timer) timer)
              (setf (plist-get guest :close-server) server)
              (when-let ((process (plist-get guest :process)))
                (process-put process 'mevedel-collaboration-guest guest))))
        (mevedel-collaboration--finish-stop guest server reason)))))

(defun mevedel-collaboration--stop-for-buffer ()
  "Stop sharing when the owning data buffer is killed."
  (when (eq (current-buffer)
            (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--stop-internal 'data-buffer-killed)))

(defun mevedel-collaboration--stop-for-session ()
  "Stop sharing from a data buffer's SessionEnd hook."
  (mevedel-collaboration--stop-for-buffer))

(defun mevedel-collaboration--stop-for-emacs ()
  "Stop sharing before Emacs exits."
  (mevedel-collaboration--stop-internal 'emacs-exit))

(cl-defun mevedel-collaboration--start (session data-buffer)
  "Start a room for SESSION and DATA-BUFFER and return its link.

The early return below needs the block a `cl-defun' establishes; a plain
`defun' would signal `no-catch' instead of returning the live link."
  (when (plist-get mevedel-collaboration--room :server)
    (let ((room mevedel-collaboration--room))
      (if (eq session (plist-get room :session))
          (cl-return-from mevedel-collaboration--start
            (plist-get room :link))
        (user-error "Collaboration already belongs to session %s"
                    (plist-get room :session-label)))))
  (let* ((public-origin (mevedel-collaboration--normalize-public-origin))
         (room-id (substring (mevedel-collaboration--random-token) 0 16))
         (token (mevedel-collaboration--random-token))
         (local-origin nil)
         (records (mevedel-collaboration--canonical-records data-buffer)))
    (when (> (mevedel-collaboration--snapshot-size records)
             mevedel-collaboration--max-snapshot-bytes)
      (user-error "Initial collaboration snapshot exceeds 8 MiB"))
    ;; Validate that every staged snapshot message fits the wire limit before
    ;; opening a listener.  A guest can reconnect for a fresh snapshot, but a
    ;; room that cannot produce one is not useful.
    (mevedel-collaboration--snapshot-chunks records)
    ;; Only this feature needs the listener, so a missing package is a setup
    ;; answer rather than a load failure out of a slash command.
    (unless (require 'web-server nil t)
      (user-error "Collaboration requires the 'web-server' package; install it first"))
    (let ((server nil))
      (condition-case error-data
          (progn
            (setq server
                  (mevedel-collaboration--ws-start
                   #'mevedel-collaboration--request-handler t))
            (let* ((process (mevedel-collaboration--web-server-slot
                             server 'process))
                   (port (process-contact process :service)))
              (setq local-origin (format "http://127.0.0.1:%s" port))
              (setq mevedel-collaboration--room
                    (list :server server
                          :session session
                          :data-buffer data-buffer
                          :session-label
                          (mevedel-collaboration--session-label
                           session data-buffer)
                          :room-id room-id
                          :token token
                          :origins (append (list local-origin)
                                           (and public-origin
                                                (list public-origin)))
                          :local-origin local-origin
                          :public-origin public-origin
                          :link (format "%s/index.html#%s.%s"
                                        (or public-origin local-origin)
                                        room-id token)
                          :records records
                          :pending-tools nil
                          :tool-call-occurrences
                          (make-hash-table :test #'equal)
                          :guest nil
                          :pending-auth nil
                          :publish-timer nil))
              (with-current-buffer data-buffer
                (add-hook 'kill-buffer-hook
                          #'mevedel-collaboration--stop-for-buffer nil t))
              (with-current-buffer data-buffer
                (add-hook 'mevedel-session-end-hook
                          #'mevedel-collaboration--stop-for-session nil t))
              (with-current-buffer data-buffer
                (add-hook 'gptel-pre-tool-call-functions
                          #'mevedel-collaboration--safe-pre-tool nil t)
                (add-hook 'gptel-post-tool-call-functions
                          #'mevedel-collaboration--safe-post-tool nil t))
              (add-hook 'kill-emacs-hook
                        #'mevedel-collaboration--stop-for-emacs)
              (kill-new (plist-get mevedel-collaboration--room :link))
              (plist-get mevedel-collaboration--room :link)))
        (error
         (condition-case nil
             (if (and mevedel-collaboration--room
                      (eq (plist-get mevedel-collaboration--room :server)
                          server))
                 (mevedel-collaboration--stop-internal 'start-failed)
               (when server (ws-stop server)))
           (error nil))
         (signal (car error-data) (cdr error-data)))))))

(defun mevedel-collaboration-view ()
  "Start or redisplay the one read-only browser room."
  (interactive)
  (require 'mevedel-collaboration-projection)
  (require 'mevedel-collaboration-transport)
  (let* ((data-buffer (mevedel-collaboration--current-data-buffer))
         (session (and data-buffer
                       (with-current-buffer data-buffer
                         (and (boundp 'mevedel--session)
                              mevedel--session))))
         (room mevedel-collaboration--room))
    (unless (and data-buffer session)
      (user-error "No active mevedel session in this buffer"))
    (cond
     ((and room (eq session (plist-get room :session)))
      (kill-new (plist-get room :link))
      (message "mevedel: collaboration link copied to kill ring")
      (plist-get room :link))
     (room
      (user-error "Collaboration already belongs to session %s"
                  (plist-get room :session-label)))
     ((not
       (yes-or-no-p
        (concat
         "Share visible prompts, responses, paths, and tool results, which may "
         "contain credentials or secrets, with a browser? "
         (if mevedel-collaboration-public-base-url
             "The configured tunnel observes the plaintext local hop. "
           "The generated link is local-only. "))))
      (user-error "Collaboration not started"))
     (t
      (let ((link (mevedel-collaboration--start session data-buffer)))
        (message "mevedel: collaboration link copied to kill ring")
        link)))))

(defun mevedel-collaboration-stop ()
  "Stop the active read-only browser room."
  (interactive)
  (if mevedel-collaboration--room
      (progn
        (mevedel-collaboration--stop-internal 'user-stop)
        (message "mevedel: collaboration stopped"))
    (message "mevedel: collaboration is not active")))

(defun mevedel-collaboration-status ()
  "Report active collaboration status without exposing its token."
  (interactive)
  (if-let ((room mevedel-collaboration--room))
      (message "mevedel: collaboration active for %s; local %s%s; %s"
               (plist-get room :session-label)
               (plist-get room :local-origin)
               (if-let ((public (plist-get room :public-origin)))
                   (format ", public %s" public)
                 "")
               (cond
                ((plist-get room :guest) "viewer authenticated")
                ((mevedel-collaboration--room-pending-auth room)
                 "viewer authenticating")
                (t "no viewer connected")))
    (message "mevedel: collaboration inactive")))


;;
;;; gptel and lifecycle hooks

(defun mevedel-collaboration--pre-tool (info)
  "Publish a running tool record for gptel tool-call INFO."
  (when-let ((room mevedel-collaboration--room))
    (when (eq (current-buffer) (plist-get room :data-buffer))
      (let* ((name (format "%s" (plist-get info :name)))
             (call-key (mevedel-collaboration--tool-call-key info))
             (pending (plist-get room :pending-tools))
             existing)
        (dolist (entry pending)
          (when (and (null existing)
                     (equal (plist-get entry :call-key) call-key)
                     (equal (plist-get entry :status) "running"))
            (setq existing entry)))
        (unless existing
          (let* ((canonical
                  (mevedel-collaboration--canonical-records
                   (plist-get room :data-buffer)))
                 (occurrences (plist-get room :tool-call-occurrences))
                 (occurrence (gethash call-key occurrences 0))
                 (explicit-id (or (plist-get info :id)
                                  (plist-get info :call-id)
                                  (plist-get info :tool-call-id)
                                  (plist-get info :tool_call_id)))
                 (id (if explicit-id
                         (format "tool-%s" explicit-id)
                       (mevedel-collaboration--stable-record-id
                        "tool" call-key occurrence)))
                 (entry (mevedel-collaboration--record
                         id "tool"
                         :revision 0
                         :name name
                         :status "running"
                         :summary name
                         :result ""
                         :truncated nil
                         :pending t
                         :identity-fixed t
                         :call-key call-key
                         :baseline-tool-count
                         (length (mevedel-collaboration--tool-records canonical))
                         :baseline-record-count (length canonical))))
            (puthash call-key (1+ occurrence) occurrences)
            (setq room (plist-put room :pending-tools
                                  (append pending (list entry))))
            (setq mevedel-collaboration--room room)))
        ;; Tool start is intentionally published immediately.  A short tool
        ;; still has a truthful completion transition, while a long tool is
        ;; visible as running before it produces a result.
        (mevedel-collaboration--publish room))))
  nil)

(defun mevedel-collaboration--post-tool (info)
  "Publish the settled result for gptel tool-call INFO."
  (when-let ((room mevedel-collaboration--room))
    (when (eq (current-buffer) (plist-get room :data-buffer))
      (let* ((pending (plist-get room :pending-tools))
             entry)
        (dolist (candidate pending)
          (when (and (null entry)
                     (equal (plist-get candidate :status) "running")
                     (mevedel-collaboration--pending-tool-match
                      info candidate))
            (setq entry candidate)))
        (when entry
          (let ((fields (mevedel-collaboration--tool-result-fields
                         (plist-get info :result))))
            (dolist (key '(:status :result :truncated))
              (setf (plist-get entry key) (plist-get fields key)))
            (mevedel-collaboration--publish room))))))
  nil)

(defun mevedel-collaboration--safe-pre-tool (info)
  "Run the live tool-start observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--pre-tool info)
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--safe-post-tool (info)
  "Run the live tool-settlement observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-tool info)
    (error (mevedel-collaboration--observer-failure)))
  nil)

(defun mevedel-collaboration--observer-failure ()
  "Stop the room after an observer failure without affecting the request."
  (condition-case nil
      (mevedel-collaboration--stop-internal 'observer-failure)
    (error nil))
  (condition-case nil
      (display-warning
       'mevedel "Live collaboration stopped after an observer failure" :warning)
    (error nil)))

(defun mevedel-collaboration--post-stream ()
  "Schedule a coalesced publication after gptel inserts response text."
  (when (eq (current-buffer) (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--schedule-publish)))

(defun mevedel-collaboration--post-response (_start _end)
  "Publish the settled response for the active data buffer."
  (when (eq (current-buffer) (mevedel-collaboration--room-data-buffer))
    (mevedel-collaboration--schedule-publish)))

(defun mevedel-collaboration--safe-post-stream ()
  "Run the stream observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-stream)
    (error (mevedel-collaboration--observer-failure))))

(defun mevedel-collaboration--safe-post-response (start end)
  "Run the response observer without signaling into gptel."
  (condition-case nil
      (mevedel-collaboration--post-response start end)
    (error (mevedel-collaboration--observer-failure))))

(provide 'mevedel-collaboration)
;;; mevedel-collaboration.el ends here
