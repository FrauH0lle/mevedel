;;; mevedel-collaboration-agent.el --- browser agent sharing -*- lexical-binding: t; -*-

;;; Commentary:

;; Publishes the retained-agent roster and serves projected live and archived agent
;; transcripts to collaboration guests.  Agent lookup is registry-only; cold
;; conversation loading runs outside guest frame handlers.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'json)

;; `mevedel-agent-control'
(declare-function mevedel-agent-control-record-model-metadata
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-role "mevedel-agent-control" (record))
(declare-function mevedel-agent-record-settled-outcome
                  "mevedel-agent-control" (record))

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-ensure-conversation
                  "mevedel-agent-persistence"
                  (session record root-buffer &optional readonly-p))
(autoload 'mevedel-agent-persistence-ensure-conversation
  "mevedel-agent-persistence")

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast
                  "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest
                  "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--observer-failure
                  "mevedel-collaboration" (room &optional err))
(declare-function mevedel-collaboration--room-for-buffer
                  "mevedel-collaboration" (buffer))
(declare-function mevedel-collaboration--room-for-session
                  "mevedel-collaboration" (session))
(declare-function mevedel-collaboration--schedule-publish
                  "mevedel-collaboration" (room))

;; `mevedel-collaboration-guest'
(declare-function mevedel-collaboration--request-id-p
                  "mevedel-collaboration-guest" (value))
(declare-function mevedel-collaboration--records-frame "mevedel-collaboration-guest"
                  (meta chunk final))
(declare-function mevedel-collaboration--snapshot-chunks
                  "mevedel-collaboration-guest" (records &optional overhead))

;; `mevedel-collaboration-projection'
(declare-function mevedel-collaboration--canonical-records
                  "mevedel-collaboration-projection"
                  (data-buffer &optional completion-buffer completions cache own-path))
(declare-function mevedel-collaboration--forwarded-execution
                  "mevedel-collaboration-projection" (segment &optional events))
(declare-function mevedel-collaboration--truncate-bytes
                  "mevedel-collaboration-projection" (string limit))
(autoload 'mevedel-collaboration--canonical-records
  "mevedel-collaboration-projection")
(autoload 'mevedel-collaboration--forwarded-execution
  "mevedel-collaboration-projection")
(autoload 'mevedel-collaboration--truncate-bytes
  "mevedel-collaboration-projection")

;; `mevedel-execution'
(declare-function mevedel-execution-list-user "mevedel-execution" (session))
(autoload 'mevedel-execution-list-user "mevedel-execution")

;; `mevedel-resource'
(declare-function mevedel-resource-visit-path "mevedel-resource"
                  (address &optional context))
(autoload 'mevedel-resource-visit-path "mevedel-resource")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))

;; `mevedel-collaboration-transport'
(declare-function mevedel-collaboration--transport-send
                  "mevedel-collaboration-transport" (transport peer frame))

;; `mevedel-structs'
(declare-function mevedel-session-agent-registry "mevedel-structs" (session))
(declare-function mevedel-session-save-path "mevedel-structs" (session))
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript-segments "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-buffer-spans
                  "mevedel-transcript-audit" (&optional type start end))
(declare-function mevedel-transcript-audit-records
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-for-tool
                  "mevedel-tool-render-data" (buffer tool-use-id))

;; `mevedel-transport'
(declare-function mevedel-transport-busy-p "mevedel-transport" (&optional path))
(declare-function mevedel-transport-run-at-time "mevedel-transport" (seconds function &rest args))
(declare-function mevedel-transport-run-when-idle "mevedel-transport"
                  (key path thunk &optional on-cancel delay))
(autoload 'mevedel-transport-run-at-time "mevedel-transport")

;; `mevedel-view-agent'
(declare-function mevedel-view--agent-record-status
                  "mevedel-view-agent" (record))

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-locate-source "mevedel-view-audit"
                  (session live-buffer tool-id &optional agent-transcript-p))
(autoload 'mevedel-view-audit-locate-source "mevedel-view-audit")

(defconst mevedel-collaboration--agent-fetch-window 1.0
  "Minimum seconds between current or archive agent fetches from one guest.")
(defconst mevedel-collaboration--execution-result-limit 50000
  "Maximum bytes of execution output evidence in one guest response.")

(defun mevedel-collaboration--agent-rows (room)
  "Return ROOM's guest-visible retained agent rows, sorted by path.
An active agent carries its live status; a settled one carries its
terminal outcome (done, errored, or interrupted), so its retained
transcript stays reachable from the viewer's finished-agents list."
  ;; ponytail: every retained agent travels in one frame; rows are a few
  ;; dozen fixed bytes each, so bound or paginate only if registries grow
  ;; to thousands.
  (when-let* ((session (plist-get room :session)))
    (let (rows)
      (dolist (pair (mevedel-session-agent-registry session))
        (let* ((record (cdr pair))
               (metadata (mevedel-agent-control-record-model-metadata record))
               (outcome (mevedel-agent-record-settled-outcome record))
               (status
                (or (mevedel-view--agent-record-status record)
                    (pcase outcome
                      ('completed 'done)
                      ((or 'errored 'interrupted) outcome)))))
          (when status
            (push (append
                   (list (cons "path" (car pair))
                         (cons "status" (symbol-name status)))
                   (when-let* ((role (mevedel-agent-record-role record)))
                     (list (cons "role" (format "%s" role))))
                   (cl-loop for key in '(:backend :model :effort)
                            for value = (plist-get metadata key)
                            when value collect
                            (cons (substring (symbol-name key) 1) value)))
                  rows))))
      (sort rows (lambda (left right)
                   (string-lessp (cdr (assoc "path" left))
                                 (cdr (assoc "path" right))))))))

(defun mevedel-collaboration--agents-frame (room)
  "Return ROOM's retained agent roster frame."
  (list :t "agents"
        :agents (vconcat (mevedel-collaboration--agent-rows room))))

(defun mevedel-collaboration--publish-agents (room)
  "Broadcast ROOM's retained agent roster when it has changed."
  (let ((frame (mevedel-collaboration--agents-frame room)))
    (unless (equal frame (plist-get room :agents))
      (plist-put room :agents frame)
      (mevedel-collaboration--broadcast room frame))))

(defun mevedel-collaboration-notify-agents-changed (session)
  "Schedule SESSION's publication after retained agent state changed."
  (when-let* ((room (mevedel-collaboration--room-for-session session)))
    (condition-case err
        (mevedel-collaboration--schedule-publish room)
      (error (mevedel-collaboration--observer-failure room err)))))

(defun mevedel-collaboration--agent-frame-overhead (req-id path &optional segment)
  "Return encoded agent-frame overhead for REQ-ID, PATH and optional SEGMENT."
  (string-bytes
   (json-encode
    (append (list :t "agent" :reqId req-id :path path
                  :digest (make-string 64 ?0) :records (vconcat nil)
                  :final :json-false)
            (when segment (list :segment segment))))))

(defun mevedel-collaboration--agent-record (room path)
  "Return the registry record for canonical PATH in ROOM, or nil."
  (when-let* ((session (plist-get room :session))
              ((stringp path)))
    (cdr (assoc path (mevedel-session-agent-registry session)))))

(defun mevedel-collaboration--agent-conversation (room path)
  "Return the live conversation buffer for canonical PATH in ROOM, or nil."
  (when-let* ((record (mevedel-collaboration--agent-record room path))
              (buffer (mevedel-agent-record-conversation-buffer record))
              ((buffer-live-p buffer)))
    buffer))

(defun mevedel-collaboration--set-agent-load (room path state)
  "Record agent PATH's load STATE in ROOM, in place; nil clears it."
  ;; `plist-put' extends the shared room plist in place; a `setf' on
  ;; `plist-get' would only rebind the local variable.
  (plist-put room :agent-loads
             (let ((loads (assoc-delete-all path (plist-get room :agent-loads))))
               (if state (cons (cons path state) loads) loads))))

(defun mevedel-collaboration--queue-agent-load (room path)
  "Queue loading the retained conversation of agent PATH for ROOM.
A settled agent of a resumed session stays on disk until first opened.
Load it from a timer, never inside the guest's frame handler.  Return
non-nil while the load is pending, nil for an unknown or failed agent."
  (when (mevedel-collaboration--agent-record room path)
    (pcase (alist-get path (plist-get room :agent-loads) nil nil #'equal)
      ('failed nil)
      ('pending t)
      (_ (mevedel-collaboration--set-agent-load room path 'pending)
         (mevedel-transport-run-at-time 0 #'mevedel-collaboration--load-agent room path)
         t))))

(defun mevedel-collaboration--load-agent (room path)
  "Load agent PATH's conversation for ROOM, as the host's own open does.
A failure is remembered, so later guest polls are refused, not retried."
  (let ((data (plist-get room :data-buffer)))
    (when (eq room (mevedel-collaboration--room-for-buffer data))
      (mevedel-collaboration--set-agent-load
       room path
       (condition-case nil
           (progn
             (mevedel-agent-persistence-ensure-conversation
              (plist-get room :session)
              (mevedel-collaboration--agent-record room path)
              data
              (with-current-buffer data
                (bound-and-true-p mevedel-session--read-only-mode)))
             nil)
         (error 'failed)))
      ;; Current-transcript viewers poll, but an immutable archive is a
      ;; one-shot disclosure.  Finish its admitted request after hydration.
      (let* ((waiters (plist-get room :agent-fetch-waiters))
             (ready (cl-remove-if-not
                     (lambda (entry) (equal path (plist-get entry :path))) waiters)))
        (plist-put room :agent-fetch-waiters
                   (cl-set-difference waiters ready :test #'eq))
        (dolist (entry ready)
          (funcall (plist-get entry :resume)))))))

(defun mevedel-collaboration--agent-fetch-records (room live-buffer path segment)
  "Project PATH's LIVE-BUFFER or child archive SEGMENT for ROOM.
SEGMENT is nil for the current transcript.  Archive buffers are disposable;
only canonical child descriptors can select them, never root history."
  (let* ((session (plist-get room :session))
         (descriptor
          (when segment
            (cl-find-if
             (lambda (entry)
               (and (equal segment (plist-get entry :number))
                    (not (plist-get entry :current-p))
                    (eq (plist-get entry :status) 'readable)))
             (mevedel-session-artifacts-transcript-segments session live-buffer t))))
         (buffer
          (if segment
              (if descriptor
                  (mevedel-session-artifacts-read-transcript-segment session descriptor)
                (error "Unknown archived agent segment"))
            live-buffer)))
    (unwind-protect
        (with-current-buffer buffer
          ;; Inspection buffers carry no authority; projection needs only the
          ;; session's published artifact paths and this child's owner path.
          (when segment (setq-local mevedel--session session))
          (mapcar
           (lambda (record)
             (let ((copy (if (or segment (plist-get record :artifact))
                             (copy-sequence record) record)))
               (when (plist-get record :artifact)
                 (plist-put copy :id
                            (concat "agent-artifact-"
                                    (secure-hash 'sha256
                                                 (concat path "\0" (plist-get record :id))))))
               (when segment
                 (plist-put copy :id
                            (format "agent-history-%d-%s" segment (plist-get copy :id))))
               copy))
           (mevedel-collaboration--canonical-records buffer nil nil nil path)))
      (when (and segment (buffer-live-p buffer) (not (eq buffer live-buffer)))
        (kill-buffer buffer)))))

(defun mevedel-collaboration--reply-agent-records (room peer guest frame records)
  "Send projected agent RECORDS for guest PEER's FRAME in ROOM.
Retain full artifact lookup metadata in GUEST, not on the wire."
  (let* ((req-id (plist-get frame :reqId))
         (path (plist-get frame :path))
         (segment (plist-get frame :segment))
         (transport (plist-get room :transport))
         (chunks (mevedel-collaboration--snapshot-chunks
                  records (mevedel-collaboration--agent-frame-overhead req-id path segment)))
         (digest (secure-hash 'sha256
                              (mapconcat #'identity (apply #'append chunks) "\n")))
         (meta (append (list :t "agent" :reqId req-id :path path :digest digest)
                       (when segment (list :segment segment)))))
    (plist-put guest :agent-artifacts
               (cl-remove-if-not (lambda (record) (plist-get record :artifact)) records))
    (if (equal digest (plist-get frame :known))
        (mevedel-collaboration--transport-send
         transport peer (append meta '(:unchanged t)))
      (cl-loop for rest on chunks do
               (mevedel-collaboration--transport-send
                transport peer
                (mevedel-collaboration--records-frame
                 meta (car rest) (null (cdr rest))))))))

(defun mevedel-collaboration--handle-fetch-agent (room peer frame)
  "Answer guest PEER's agent-transcript fetch FRAME for ROOM.
Optional positive integer `:segment' selects this child's compact archive."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (path (plist-get frame :path))
        (segment (plist-get frame :segment))
        (transport (plist-get room :transport))
        (now (float-time)))
    (when (and guest (mevedel-collaboration--request-id-p req-id))
      ;; Live polls must not consume a one-shot archive disclosure's budget.
      (let* ((archive-p (plist-member frame :segment))
             (slot (if archive-p :last-agent-archive-fetch :last-agent-fetch))
             (last (plist-get guest slot))
             (throttled (and last (< (- now last)
                                    mevedel-collaboration--agent-fetch-window))))
        (when (and throttled archive-p)
          ;; Archive opens do not poll, so silence would leave them loading
          ;; forever.  Keep the last successful artifact lookup authority.
          (mevedel-collaboration--transport-send
           transport peer
           (list :t "agent" :reqId req-id :path path :segment segment
                 :error "Please wait a moment, then retry.")))
        (unless throttled
          (plist-put guest slot now)
          (cl-labels
              ((refuse (message)
                 (plist-put guest :agent-artifacts nil)
                 (mevedel-collaboration--transport-send
                  transport peer
                  (append (list :t "agent" :reqId req-id :error message)
                          (when (plist-member frame :segment)
                            (list :path path :segment segment)))))
               (fetch ()
                 (condition-case err
                     (if-let* ((buffer (mevedel-collaboration--agent-conversation room path)))
                         (mevedel-collaboration--reply-agent-records
                          room peer guest frame
                          (mevedel-collaboration--agent-fetch-records room buffer path segment))
                       ;; Cold conversation hydration stays outside the handler.
                       ;; Current viewers poll; archives finish this one request.
                       (if (mevedel-collaboration--queue-agent-load room path)
                           (when segment
                             ;; At most one pending archive disclosure per peer.
                             (plist-put room :agent-fetch-waiters
                                        (cons (list :path path :peer peer :resume #'resume)
                                              (cl-remove-if
                                               (lambda (entry)
                                                 (equal peer (plist-get entry :peer)))
                                               (plist-get room :agent-fetch-waiters)))))
                         (refuse "This agent's transcript is not available")))
                   (error (if segment
                              (refuse "This agent's archived segment could not be read")
                            (signal (car err) (cdr err))))))
               (resume ()
                 (when (and (eq room (mevedel-collaboration--room-for-session
                                     (plist-get room :session)))
                            (eq guest (mevedel-collaboration--guest room peer)))
                   (fetch))))
            (cond
             ((and (plist-member frame :segment)
                   (not (and (integerp segment) (> segment 0))))
              (refuse "Invalid archived agent segment: expected a positive integer"))
             ((and segment
                   (mevedel-transport-busy-p
                    (mevedel-session-save-path (plist-get room :session))))
              (mevedel-transport-run-when-idle
               (list 'collaboration-agent-history (plist-get room :room-id) peer req-id)
               (mevedel-session-save-path (plist-get room :session))
               #'resume))
             (t (fetch)))))))))

(defun mevedel-collaboration--find-transcript-evidence (session live-buffer find)
  "Run FIND on SESSION's readable source-backed segments of LIVE-BUFFER.
Return the newest matching value.  Archive buffers are always released."
  (when (and session (buffer-live-p live-buffer))
    (catch 'found
      (dolist (descriptor (reverse
                           (mevedel-session-artifacts-transcript-segments
                            session live-buffer)))
        (when (eq (plist-get descriptor :status) 'readable)
          (let ((buffer (if (plist-get descriptor :current-p) live-buffer
                          (condition-case nil
                              (mevedel-session-artifacts-read-transcript-segment
                               session descriptor)
                            (error nil)))))
            (when (buffer-live-p buffer)
              (unwind-protect
                  (with-current-buffer buffer
                    (save-restriction
                      (widen)
                      (when-let* ((value (funcall find)))
                        (throw 'found value))))
                (unless (eq buffer live-buffer) (kill-buffer buffer))))))))))

(defun mevedel-collaboration--forwarded-evidence (room owner id)
  "Return validated parent mailbox evidence for OWNER and execution ID.
Only a matching forwarded completion in this room authorizes a fetch."
  (mevedel-collaboration--find-transcript-evidence
   (plist-get room :session) (plist-get room :data-buffer)
   (lambda ()
     (cl-some
      (lambda (segment)
        (when (eq (car segment) 'mailbox)
          (let ((record (mevedel-collaboration--forwarded-execution segment)))
            (when (and (equal owner (plist-get record :owner))
                       (equal id (plist-get record :execution-id)))
              record))))
      (mevedel-transcript-segments (point-min) (point-max))))))

(defun mevedel-collaboration--local-execution (room owner id)
  "Return (EVIDENCE . BUFFER) for a trusted local OWNER/ID in ROOM.
EVIDENCE is a terminal breadcrumb or source-backed live registry snapshot.
The receiving transcript must itself be the root or the registered child;
a client cannot authorize a result by naming another agent or a path."
  (when-let* ((buffer (if (equal owner "/root")
                         (plist-get room :data-buffer)
                       (mevedel-collaboration--agent-conversation room owner)))
              ((buffer-live-p buffer))
              (session (buffer-local-value 'mevedel--session buffer))
              (record
               (or
                (when-let* ((facts
                             (cl-find-if
                              (lambda (candidate)
                                (and (plist-get candidate :yielded)
                                     (equal owner (plist-get candidate :owner))
                                     (equal id (plist-get candidate :execution-id))))
                              (mevedel-execution-list-user (plist-get room :session))))
                            ((mevedel-view-audit-locate-source
                              session buffer (plist-get facts :tool-use-id)
                              (not (equal owner "/root")))))
                  (list :owner owner :execution-id id :live t
                        :tool-use-id (plist-get facts :tool-use-id) :facts facts))
                (mevedel-collaboration--find-transcript-evidence
                 session buffer
                 (lambda ()
                   (cl-some
                    (lambda (span)
                      (let ((audit (plist-get span :record)))
                        (when (and (equal owner (plist-get audit :owner))
                                   (equal id (plist-get audit :execution-id)))
                          audit)))
                    (reverse (mevedel-transcript-audit-buffer-spans
                              'execution-breadcrumb))))))))
    (cons record buffer)))

(defun mevedel-collaboration--execution-row-facts (buffer tool-id id)
  "Find TOOL-ID's terminal Bash render data for ID in BUFFER's segments."
  (when (and (stringp tool-id) (buffer-live-p buffer))
    (mevedel-collaboration--find-transcript-evidence
     (buffer-local-value 'mevedel--session buffer) buffer
     (lambda ()
       (let ((data (mevedel-tool-render-data-for-tool
                    (current-buffer) tool-id)))
         (when (and (equal (plist-get data :execution-id) id)
                    (memq (plist-get data :state)
                          '(completed interrupted lost)))
           data))))))

(defun mevedel-collaboration--terminal-execution-facts
    (buffer id &optional tool-id)
  "Find trusted terminal audit facts for ID in BUFFER's source segments.
When TOOL-ID is supplied by an authorized local breadcrumb, require its
matching tool-use id; nested ToolCall Bash children have no separate row."
  (when (buffer-live-p buffer)
    (mevedel-collaboration--find-transcript-evidence
     (buffer-local-value 'mevedel--session buffer) buffer
     (lambda ()
       (cl-some
        (lambda (span)
          (let* ((record (plist-get span :record))
                 (facts (plist-get record :render-data)))
            (when (and (memq (plist-get record :type)
                             '(execution-completion execution-archive))
                       (equal id (plist-get facts :execution-id))
                       (or (null tool-id)
                           (equal tool-id (plist-get record :tool-use-id))))
              facts)))
        (reverse (mevedel-transcript-audit-buffer-spans)))))))

(defun mevedel-collaboration--child-execution-facts (room owner id)
  "Return source-backed terminal facts for OWNER's ID when the child is live.
Compaction segments are searched as well as the current buffer; a cold child
has no resident transcript, so its forwarded parent delivery is the fallback."
  (when-let* ((session (plist-get room :session))
              (entry (assoc owner (mevedel-session-agent-registry session)))
              (buffer (mevedel-agent-record-conversation-buffer (cdr entry)))
              ((buffer-live-p buffer))
              ((buffer-local-value 'mevedel--session buffer)))
    (or (mevedel-collaboration--terminal-execution-facts buffer id)
        (when-let* ((breadcrumb (car (mevedel-collaboration--local-execution
                                      room owner id))))
          (or (mevedel-collaboration--execution-row-facts
               buffer (plist-get breadcrumb :tool-use-id) id)
              (plist-get breadcrumb :facts))))))

(defun mevedel-collaboration--retained-execution-output (session facts)
  "Return bounded retained output from canonical FACTS and SESSION, or nil.
Only trusted source-backed child/local facts supply a path; never accept a
guest-supplied artifact address or read paths from a forwarded mailbox."
  (let* ((path (plist-get facts :output-path))
         (physical (and (stringp path) (string-prefix-p "artifact://" path)
                        (condition-case nil
                            (mevedel-resource-visit-path path (list :session session))
                          (error nil)))))
    (or (and (stringp physical) (file-regular-p physical)
             (file-readable-p physical)
             (with-temp-buffer
               (insert-file-contents physical nil 0
                                     (min (1+ mevedel-collaboration--execution-result-limit)
                                          (file-attribute-size
                                           (file-attributes physical))))
               (buffer-string)))
        (plist-get facts :execution-output))))

(defun mevedel-collaboration--handle-execution-result-get (room peer frame)
  "Serve bounded, read-only execution evidence to registered guest PEER.
OWNER and identity must occur together in a source-backed live registry entry,
trusted local breadcrumb or the parent's forwarded mailbox.  No guest-supplied
path grants authority."
  (let ((guest (mevedel-collaboration--guest room peer))
        (req-id (plist-get frame :reqId))
        (owner (plist-get frame :owner))
        (id (plist-get frame :executionId)))
    (when (and guest (mevedel-collaboration--request-id-p req-id)
               (stringp owner) (<= (string-bytes owner) 256)
               (stringp id) (<= (string-bytes id) 256))
      (let* ((now (float-time))
             (recent (plist-get guest :last-execution-result-fetch))
             (throttled (and recent
                             (< (- now recent)
                                mevedel-collaboration--agent-fetch-window)))
             (_ (unless throttled
                  (plist-put guest :last-execution-result-fetch now)))
             (forwarded (unless throttled
                          (mevedel-collaboration--forwarded-evidence room owner id)))
             (local (unless (or throttled forwarded)
                      (mevedel-collaboration--local-execution room owner id)))
             (child-buffer (or (cdr local)
                               (and forwarded
                                    (mevedel-collaboration--agent-conversation
                                     room owner))))
             (child-session (and (buffer-live-p child-buffer)
                                 (buffer-local-value 'mevedel--session child-buffer)))
             (facts (or (and forwarded
                             (mevedel-collaboration--child-execution-facts
                              room owner id))
                        (and local
                             (or (mevedel-collaboration--execution-row-facts
                                  child-buffer (plist-get (car local) :tool-use-id) id)
                                 (and (stringp (plist-get (car local) :tool-use-id))
                                      (mevedel-collaboration--terminal-execution-facts
                                       child-buffer id
                                       (plist-get (car local) :tool-use-id)))
                                 (plist-get (car local) :facts)))))
             (canonical-output (and facts (or child-session
                                               (plist-get room :session))
                                    (mevedel-collaboration--retained-execution-output
                                     (or child-session (plist-get room :session)) facts)))
             (mailbox-output (and forwarded
                                  (plist-get (plist-get forwarded :facts)
                                             :execution-output)))
             (output (or canonical-output
                         (and (stringp mailbox-output)
                              (string-remove-prefix "\n" mailbox-output))))
             (bounded (and (stringp output)
                           (mevedel-collaboration--truncate-bytes
                            output mevedel-collaboration--execution-result-limit))))
        (mevedel-collaboration--transport-send
         (plist-get room :transport) peer
         (list :t "execution-result" :reqId req-id
               :owner owner :executionId id
               :source (cond (throttled "missing")
                             ((and (stringp canonical-output)
                                   (plist-get (car local) :live)) "live")
                             ((stringp canonical-output) "child")
                             ((stringp output) "forwarded")
                             (t "missing"))
               :output (or bounded "")
               :truncated (and (stringp output) (not (equal output bounded)))
               :error (cond (throttled "Retry result fetch shortly.")
                            ((not (stringp output))
                             "Original execution row and retained output are unavailable."))))))))

(provide 'mevedel-collaboration-agent)
;;; mevedel-collaboration-agent.el ends here
