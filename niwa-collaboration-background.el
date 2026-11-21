;;; niwa-collaboration-background.el --- Host background-work panel -*- lexical-binding: t; -*-

;;; Commentary:

;; Browser presentation of the native background panel's semantic rows.
;; Native collection and stable ordering are shared; selection, focus and
;; finished-row grace are local to each browser.  A bounded room-local terminal
;; cache bridges passive terminal events and reconnects, never owning a process
;; or consuming output.  Typed stops resolve current host authority again.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'niwa-agent-control)
(require 'niwa-view-execution)

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-ensure-conversation
                  "niwa-agent-persistence" (session record root-buffer &optional readonly-p))
(autoload 'niwa-agent-persistence-ensure-conversation "niwa-agent-persistence")

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x) t)
(declare-function niwa-agent-invocation-parent-tool-use-id "niwa-agents" (cl-x) t)

;; `niwa-collaboration'
(declare-function niwa-collaboration--broadcast "niwa-collaboration" (room frame))
(declare-function niwa-collaboration--guest "niwa-collaboration" (room peer))
(declare-function niwa-collaboration--room-for-session "niwa-collaboration" (session))
(declare-function niwa-collaboration--schedule-publish "niwa-collaboration" (room))

;; `niwa-collaboration-guest'
(declare-function niwa-collaboration--request-id-p "niwa-collaboration-guest" (value))

;; `niwa-collaboration-transport'
(declare-function niwa-collaboration--transport-send "niwa-collaboration-transport" (transport peer frame))

;; `niwa-execution'
(defvar niwa-execution-event-functions)

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(declare-function niwa-transport-run-at-time "niwa-transport" (seconds function &rest args))
(declare-function niwa-transport-run-when-idle "niwa-transport" (key path function &optional on-cancel))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-run-at-time "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-view-audit'
(declare-function niwa-view-audit-execution-record "niwa-view-audit"
                  (session live execution-id &optional agent-transcript-p))
(declare-function niwa-view-audit-locate-source "niwa-view-audit"
                  (session live-buffer tool-id &optional agent-transcript-p))
(autoload 'niwa-view-audit-execution-record "niwa-view-audit")
(autoload 'niwa-view-audit-locate-source "niwa-view-audit")

(defconst niwa-collaboration--background-row-limit 128
  "Maximum background rows in a frame and retained terminal cache.")
(defconst niwa-collaboration--background-frame-bytes (* 64 1024)
  "Maximum encoded JSON bytes in a background-work frame.")
(defconst niwa-collaboration--background-terminal-seconds 30
  "Seconds terminal evidence remains available for browser reconnects.
Browsers own their ten-second visible grace, independently of this cache.")
(defconst niwa-collaboration--background-line-limit 512
  "Maximum characters in one output preview row.")

(defun niwa-collaboration--background-key (entry)
  "Encode ENTRY's native identity as a stable browser key."
  (json-encode (vconcat (car entry))))

(defun niwa-collaboration--background-preview (data)
  "Return bounded three-row output and explicit availability facts for DATA."
  (let* ((terminal (plist-get data :terminal-p))
         (output (plist-get data (if terminal :whole-output :output-tail)))
         ;; Events and live snapshots are bounded at their producer; cap again
         ;; before line splitting so projection never expands an unbounded value.
         (bounded (and (stringp output) (substring output (max 0 (- (length output) 4096)))))
         (lines (and bounded (split-string (string-trim-right bounded) "\n")))
         (available (and (stringp output)
                         (not (and (string-empty-p output)
                                   (or (eq (plist-get data :state) 'lost)
                                       (> (or (plist-get data :output-bytes) 0) 0))))))
         (truncated (or (plist-get data :output-preview-truncated-p)
                        (plist-get data :whole-output-truncated-p)
                        (and output (> (length output) 4096))
                        (> (length lines) 3)
                        (cl-some (lambda (line) (> (length line) niwa-collaboration--background-line-limit))
                                 lines)))
         (rows (cond ((not available) (list "Output unavailable."))
                     ((string-empty-p output) (list (if terminal "No output." "No output yet.")))
                     (t (last lines 3)))))
    (list :preview
          (vconcat (cl-loop for index below 3 for line = (or (nth index rows) "")
                            collect (replace-regexp-in-string
                                     "[\r\t]" " "
                                     (substring line 0 (min (length line) niwa-collaboration--background-line-limit)))))
          :previewAvailable (if available t :json-false)
          :previewTruncated (if truncated t :json-false)
          :previewLabel
          (concat "Preview (not full output)"
                  (cond
                   ((or (eq (plist-get data :termination) 'output-limit)
                        (> (or (plist-get data :omitted-output-bytes) 0) 0))
                    " — output was truncated; retained output may omit content")
                   (truncated " — truncated"))))))

(defun niwa-collaboration--background-row (session entry order)
  "Project SESSION's native ENTRY at ORDER into an allowlisted row."
  (let* ((agent (eq (nth 1 entry) 'agent))
         (data (nth 2 entry))
         (owner (if agent (plist-get data :path) (plist-get data :owner)))
         (metadata (or (plist-get data :model-metadata)
                       (and (not (plist-get data :terminal-p))
                            (niwa-agent-control-model-metadata session owner))))
         (status (or (plist-get data :status) (plist-get data :state) 'unknown)))
    (append
     (list :key (niwa-collaboration--background-key entry)
           :kind (if agent "agent" (symbol-name (or (plist-get data :kind) 'bash)))
           :owner owner :order order
           :startedAt (niwa-view-execution--start-time data)
           :status (symbol-name status)
           :statusLabel (if agent
                            (pcase status ('done "Finished")
                              (_ (capitalize (symbol-name status))))
                          (niwa-view-execution--state-label data))
           :description (let ((text (or (plist-get data :description)
                                        (plist-get data :command) owner "")))
                          (substring text 0 (min 1024 (length text))))
           :terminal (if (plist-get data :terminal-p) t :json-false))
     (if agent (list :path owner :parentPath (plist-get data :parent-path)
                     :depth (or (plist-get data :depth) 0))
       (append (list :id (plist-get data :execution-id)
                     :outputBytes (or (plist-get data :output-bytes) 0)
                     :outputLines (or (plist-get data :output-lines) 0)
                     :omittedOutputBytes (or (plist-get data :omitted-output-bytes) 0)
                     :exitCode (plist-get data :exit-code)
                     :outcome (when-let* ((value (plist-get data :outcome)))
                                (symbol-name value))
                     :termination (when-let* ((value (plist-get data :termination)))
                                    (symbol-name value))
                     :eventCount (plist-get data :event-count)
                     :expiresAt (plist-get data :expires-at))
               (niwa-collaboration--background-preview data)))
     (when-let* ((id (plist-get data :tool-use-id))) (list :toolUseId id))
     (cl-loop for key in '(:backend :model :effort)
              for value = (plist-get metadata key)
              when value append (list key value)))))

(defun niwa-collaboration--background-retain (room entry)
  "Retain terminal ENTRY in ROOM with a bounded lifetime and count.
Duplicate evidence may refresh facts, but never the original completion age."
  (let* ((cache (plist-get room :background-terminal))
         (previous (cl-find (car entry) cache
                            :key (lambda (item) (car (cdr item))) :test #'equal)))
    (plist-put room :background-terminal
               (seq-take
                (cons (cons (if previous (car previous) (float-time)) entry)
                      (cl-remove (car entry) cache
                                 :key (lambda (item) (car (cdr item))) :test #'equal))
                niwa-collaboration--background-row-limit))))

(defun niwa-collaboration--background-frame (room)
  "Return ROOM's bounded background-work frame, including terminal evidence."
  (let* ((session (plist-get room :session))
         (active (mapcar
                  (lambda (entry)
                    (let* ((data (copy-sequence (nth 2 entry)))
                           (metadata (or (plist-get data :model-metadata)
                                         (niwa-agent-control-model-metadata
                                          session (plist-get data :owner)))))
                      (list (car entry) (nth 1 entry)
                            (plist-put data :model-metadata metadata))))
                  (niwa-view-execution-collect session)))
         (known (plist-get room :background-known))
         (now (float-time))
         (starts (or (plist-get room :background-starts)
                     (let ((table (make-hash-table :test #'equal)))
                       (plist-put room :background-starts table) table))))
    ;; Agents have retained registry outcomes rather than execution events.
    ;; Preserve the last active native row's effective model and start facts.
    (dolist (entry known)
      (when (and (eq (nth 1 entry) 'agent)
                 (not (assoc (car entry) active)))
        (let* ((data (copy-sequence (nth 2 entry)))
               (record (cdr (assoc (plist-get data :path) (niwa-session-agent-registry session))))
               (outcome (and record (niwa-agent-record-settled-outcome record))))
          (when outcome
            (setq data (plist-put data :terminal-p t)
                  data (plist-put data :status (if (eq outcome 'completed) 'done outcome)))
            (niwa-collaboration--background-retain room (list (car entry) 'agent data))))))
    (let* ((terminal (cl-remove-if
                      (lambda (item)
                        (or (> (- now (car item)) niwa-collaboration--background-terminal-seconds)
                            (assoc (car (cdr item)) active)))
                      (plist-get room :background-terminal)))
           (all (append active (mapcar #'cdr terminal)))
           (ordered (niwa-view-execution--ordered all starts))
           ;; Admit newest native-ordered live rows before terminal evidence,
           ;; then restore their mixed native order.  Capping before ordering
           ;; would retain the execution registry's oldest entries instead.
           (chosen (seq-take
                    (append (cl-remove-if (lambda (entry) (plist-get (nth 2 entry) :terminal-p)) ordered)
                            (cl-remove-if-not (lambda (entry) (plist-get (nth 2 entry) :terminal-p)) ordered))
                    niwa-collaboration--background-row-limit))
           (entries (cl-remove-if-not (lambda (entry) (memq entry chosen)) ordered))
           (rows (cl-loop for entry in entries for order from 0
                          ;; Native ordering preserves the cached entry objects.
                          for cached = (rassq entry terminal)
                          collect (append
                                   (niwa-collaboration--background-row session entry order)
                                   (when cached
                                     (list :terminalAgeSeconds (max 0.0 (- now (car cached))))))))
           ;; Reserve the small frame envelope and JSON array separators.
           (remaining (- niwa-collaboration--background-frame-bytes 256))
           admitted)
      (plist-put room :background-terminal terminal)
      (plist-put room :background-known
                 (cl-remove-if (lambda (entry) (plist-get (nth 2 entry) :terminal-p)) entries))
      (dolist (row (append (cl-remove-if (lambda (row) (eq t (plist-get row :terminal))) rows)
                          (cl-remove-if-not (lambda (row) (eq t (plist-get row :terminal))) rows)))
        (let ((bytes (1+ (string-bytes (json-encode row)))))
          (when (<= bytes remaining)
            (setq remaining (- remaining bytes))
            (push row admitted))))
      (list :t "background-work"
            ;; Retain the original native ordering after priority admission.
            :rows (vconcat (cl-remove-if-not (lambda (row) (memq row admitted)) rows))
            :truncated (if (> (length all) (length admitted)) t :json-false)))))

(defun niwa-collaboration--publish-background (room)
  "Broadcast ROOM's background rows only when their visible facts change."
  (let* ((frame (niwa-collaboration--background-frame room))
         ;; Age informs initial browser grace, not a changing visible fact.
         ;; Joining guests construct a fresh frame with the current ages.
         (facts (plist-put
                 (copy-sequence frame) :rows
                 (vconcat
                  (mapcar (lambda (row)
                            (let ((copy (copy-sequence row)))
                              (cl-remf copy :terminalAgeSeconds)
                              copy))
                          (plist-get frame :rows))))))
    (unless (equal facts (plist-get room :background-frame))
      (plist-put room :background-frame facts)
      (niwa-collaboration--broadcast room frame))))

(defun niwa-collaboration--background-event (event)
  "Observe passive execution EVENT, retaining terminal facts before removal."
  (when-let* ((room (niwa-collaboration--room-for-session (plist-get event :session))))
    (when (eq (plist-get event :type) 'terminal)
      (let* ((facts (plist-get event :facts))
             (owner (plist-get event :owner))
             (key (list 'execution owner (plist-get facts :execution-id)))
             (previous (nth 2 (assoc key (plist-get room :background-known))))
             (data (append (list :owner owner :terminal-p t
                                 :started-at (or (plist-get previous :started-at)
                                                 (plist-get event :started-at))
                                 :tool-use-id (plist-get event :tool-use-id)
                                 :model-metadata
                                 (or (plist-get previous :model-metadata)
                                     (niwa-agent-control-model-metadata (plist-get event :session) owner))
                                 :whole-output (plist-get event :whole-output)
                                 :whole-output-truncated-p (plist-get event :whole-output-truncated-p))
                           facts previous)))
        (when (plist-get facts :execution-id)
          (niwa-collaboration--background-retain room (list key 'execution data)))))
    (niwa-collaboration--schedule-publish room)))

(defun niwa-collaboration--handle-background-stop (room peer frame)
  "Handle writable guest PEER's typed stop FRAME in ROOM.
Keys are presentation identity only; the current scoped host registry proves
the target.  The native user-authority API rechecks liveness at the action."
  (let* ((guest (niwa-collaboration--guest room peer))
         (session (plist-get room :session))
         (path (plist-get frame :path))
         (owner (plist-get frame :owner))
         (id (plist-get frame :id))
         (key (plist-get frame :key)))
    (unless (and guest (plist-get guest :writable))
      (user-error "Background control requires a full or owner link"))
    (let ((entry
           (if path
               (progn
                 (unless (and (stringp path) (not (equal path "/root"))
                              (not id) (not owner)
                              (assoc path (niwa-session-agent-registry session)))
                   (user-error "Agent is no longer available"))
                 (list (list 'agent path) 'agent (list :path path)))
             (let ((snapshot
                    (and (stringp id) (stringp owner)
                         (cl-find-if
                          (lambda (item)
                            (and (plist-get item :yielded)
                                 (equal owner (plist-get item :owner))
                                 (equal id (plist-get item :execution-id))))
                          (niwa-view-execution--live session)))))
               (unless snapshot (user-error "Execution is no longer available"))
               (list (niwa-view-execution--key 'execution snapshot) 'execution snapshot)))))
      (when (and key (not (equal key (niwa-collaboration--background-key entry))))
        (user-error "Background identity does not match"))
      (if path (niwa-agent-control-interrupt-user session path)
        (niwa-execution-stop-user session id)))))

(defun niwa-collaboration--background-source-buffer (room owner)
  "Return ROOM's read-only-inspectable receiving transcript for known OWNER.
Called outside guest filters so cold retained-agent hydration is safe."
  (if (equal owner "/root")
      (plist-get room :data-buffer)
    (when-let* ((session (plist-get room :session))
                (record (cdr (assoc owner (niwa-session-agent-registry session)))))
      (or (let ((buffer (niwa-agent-record-conversation-buffer record)))
            (and (buffer-live-p buffer) buffer))
          (progn
            (niwa-agent-persistence-ensure-conversation
             session record (plist-get room :data-buffer) t)
            (niwa-agent-record-conversation-buffer record))))))

(defun niwa-collaboration--background-source (room frame)
  "Locate FRAME's background source in ROOM using native audit lookup.
Only host registries or source-backed execution evidence supply tool identity;
neither a client tool ID nor a filesystem path is accepted as authority."
  (let* ((session (plist-get room :session))
         (path (plist-get frame :path))
         (owner (plist-get frame :owner))
         (id (plist-get frame :id))
         tool-id)
    (if path
        (let* ((record (and (stringp path) (not owner) (not id)
                            (not (equal path "/root"))
                            (cdr (assoc path (niwa-session-agent-registry session)))))
               (invocation (and record (niwa-agent-record-invocation record))))
          (unless record (user-error "Agent source is unavailable"))
          (setq owner (or (niwa-agent-record-parent-path record) "/root")
                tool-id (and (niwa-agent-invocation-p invocation)
                             (niwa-agent-invocation-parent-tool-use-id invocation))))
      (unless (and (stringp owner) (stringp id)
                   (<= (string-bytes owner) 256) (<= (string-bytes id) 256)
                   (or (equal owner "/root")
                       (assoc owner (niwa-session-agent-registry session))))
        (user-error "Execution source is unavailable")))
    (let* ((buffer (niwa-collaboration--background-source-buffer room owner))
           (agent-p (not (equal owner "/root"))))
      (unless (buffer-live-p buffer) (user-error "Source transcript is unavailable"))
      (unless path
        (let* ((snapshot (cl-find-if
                          (lambda (item) (and (equal owner (plist-get item :owner))
                                              (equal id (plist-get item :execution-id))))
                          (niwa-view-execution--live session)))
               (cached (assoc (list 'execution owner id)
                              (mapcar #'cdr (plist-get room :background-terminal)))))
          (setq tool-id
                (or (plist-get snapshot :tool-use-id)
                    (plist-get (nth 2 cached) :tool-use-id)
                    (plist-get (niwa-view-audit-execution-record session buffer id agent-p)
                               :tool-use-id)))))
      (let ((source (niwa-view-audit-locate-source session buffer tool-id agent-p)))
        (unless source (user-error "Original source row is unavailable"))
        (append (list :owner owner :toolUseId tool-id
                      :current (if (plist-get source :current-p) t :json-false))
                (when path (list :path path))
                (unless (plist-get source :current-p)
                  (list :segment (plist-get source :number))))))))

(defun niwa-collaboration--handle-background-locate (room peer frame)
  "Answer authenticated PEER's read-only background source FRAME in ROOM.
Archive I/O and cold child hydration run outside the websocket filter, with
the room and guest rechecked after transport deferral."
  (when-let* ((guest (niwa-collaboration--guest room peer))
              ((niwa-collaboration--request-id-p (plist-get frame :reqId))))
    (let* ((session (plist-get room :session))
           (meta (list :t "background-source" :reqId (plist-get frame :reqId)))
           (send (lambda (reply)
                   (niwa-collaboration--transport-send
                    (plist-get room :transport) peer (append meta reply))))
           (last (plist-get guest :last-background-locate))
           (now (float-time)))
      (if (and last (< (- now last) 1.0))
          (funcall send '(:unavailable "Retry source lookup shortly."))
        (plist-put guest :last-background-locate now)
        (niwa-transport-run-at-time
         0
         (lambda ()
           (let ((lookup
                  (lambda ()
                    (when (and (eq room (niwa-collaboration--room-for-session session))
                               (eq guest (niwa-collaboration--guest room peer)))
                      (funcall send
                               (condition-case nil
                                   (niwa-collaboration--background-source room frame)
                                 (error '(:unavailable "Original source row is unavailable."))))))))
             (if (niwa-transport-busy-p (niwa-session-save-path session))
                 (niwa-transport-run-when-idle
                  (list 'background-locate (plist-get room :room-id) peer (plist-get frame :reqId))
                  (niwa-session-save-path session) lookup)
               (funcall lookup)))))))))

(add-hook 'niwa-execution-event-functions #'niwa-collaboration--background-event)

(provide 'niwa-collaboration-background)
;;; niwa-collaboration-background.el ends here
