;;; mevedel-collaboration-background.el --- Host background-work panel -*- lexical-binding: t; -*-

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
(require 'mevedel-agent-control)
(require 'mevedel-view-execution)

;; `mevedel-collaboration'
(declare-function mevedel-collaboration--broadcast "mevedel-collaboration" (room frame))
(declare-function mevedel-collaboration--guest "mevedel-collaboration" (room peer))
(declare-function mevedel-collaboration--room-for-session "mevedel-collaboration" (session))
(declare-function mevedel-collaboration--schedule-publish "mevedel-collaboration" (room))

;; `mevedel-execution'
(defvar mevedel-execution-event-functions)

(defconst mevedel-collaboration--background-row-limit 128
  "Maximum background rows in a frame and retained terminal cache.")
(defconst mevedel-collaboration--background-frame-bytes (* 64 1024)
  "Maximum encoded JSON bytes in a background-work frame.")
(defconst mevedel-collaboration--background-terminal-seconds 30
  "Seconds terminal evidence remains available for browser reconnects.
Browsers own their ten-second visible grace, independently of this cache.")
(defconst mevedel-collaboration--background-line-limit 512
  "Maximum characters in one output preview row.")

(defun mevedel-collaboration--background-key (entry)
  "Encode ENTRY's native identity as a stable browser key."
  (json-encode (vconcat (car entry))))

(defun mevedel-collaboration--background-preview (data)
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
                        (cl-some (lambda (line) (> (length line) mevedel-collaboration--background-line-limit))
                                 lines)))
         (rows (cond ((not available) (list "Output unavailable."))
                     ((string-empty-p output) (list (if terminal "No output." "No output yet.")))
                     (t (last lines 3)))))
    (list :preview
          (vconcat (cl-loop for index below 3 for line = (or (nth index rows) "")
                            collect (replace-regexp-in-string
                                     "[\r\t]" " "
                                     (substring line 0 (min (length line) mevedel-collaboration--background-line-limit)))))
          :previewAvailable (if available t :json-false)
          :previewTruncated (if truncated t :json-false)
          :previewLabel
          (concat "Preview (not full output)"
                  (cond
                   ((or (eq (plist-get data :termination) 'output-limit)
                        (> (or (plist-get data :omitted-output-bytes) 0) 0))
                    " — output was truncated; retained output may omit content")
                   (truncated " — truncated"))))))

(defun mevedel-collaboration--background-row (session entry order)
  "Project SESSION's native ENTRY at ORDER into an allowlisted row."
  (let* ((agent (eq (nth 1 entry) 'agent))
         (data (nth 2 entry))
         (owner (if agent (plist-get data :path) (plist-get data :owner)))
         (metadata (or (plist-get data :model-metadata)
                       (and (not (plist-get data :terminal-p))
                            (mevedel-agent-control-model-metadata session owner))))
         (status (or (plist-get data :status) (plist-get data :state) 'unknown)))
    (append
     (list :key (mevedel-collaboration--background-key entry)
           :kind (if agent "agent" (symbol-name (or (plist-get data :kind) 'bash)))
           :owner owner :order order
           :startedAt (mevedel-view-execution--start-time data)
           :status (symbol-name status)
           :statusLabel (if agent
                            (pcase status ('done "Finished")
                              (_ (capitalize (symbol-name status))))
                          (mevedel-view-execution--state-label data))
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
               (mevedel-collaboration--background-preview data)))
     (when-let* ((id (plist-get data :tool-use-id))) (list :toolUseId id))
     (cl-loop for key in '(:backend :model :effort)
              for value = (plist-get metadata key)
              when value append (list key value)))))

(defun mevedel-collaboration--background-retain (room entry)
  "Retain terminal ENTRY in ROOM with a bounded lifetime and count."
  (plist-put room :background-terminal
             (seq-take
              (cons (cons (float-time) entry)
                    (cl-remove (car entry) (plist-get room :background-terminal)
                               :key (lambda (item) (car (cdr item))) :test #'equal))
              mevedel-collaboration--background-row-limit)))

(defun mevedel-collaboration--background-frame (room)
  "Return ROOM's bounded background-work frame, including terminal evidence."
  (let* ((session (plist-get room :session))
         (active (mapcar
                  (lambda (entry)
                    (let* ((data (copy-sequence (nth 2 entry)))
                           (metadata (or (plist-get data :model-metadata)
                                         (mevedel-agent-control-model-metadata
                                          session (plist-get data :owner)))))
                      (list (car entry) (nth 1 entry)
                            (plist-put data :model-metadata metadata))))
                  (mevedel-view-execution-collect session)))
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
               (record (cdr (assoc (plist-get data :path) (mevedel-session-agent-registry session))))
               (outcome (and record (mevedel-agent-record-settled-outcome record))))
          (when outcome
            (setq data (plist-put data :terminal-p t)
                  data (plist-put data :status (if (eq outcome 'completed) 'done outcome)))
            (mevedel-collaboration--background-retain room (list (car entry) 'agent data))))))
    (let* ((terminal (cl-remove-if
                      (lambda (item)
                        (or (> (- now (car item)) mevedel-collaboration--background-terminal-seconds)
                            (assoc (car (cdr item)) active)))
                      (plist-get room :background-terminal)))
           (all (append active (mapcar #'cdr terminal)))
           (ordered (mevedel-view-execution--ordered all starts))
           ;; Admit newest native-ordered live rows before terminal evidence,
           ;; then restore their mixed native order.  Capping before ordering
           ;; would retain the execution registry's oldest entries instead.
           (chosen (seq-take
                    (append (cl-remove-if (lambda (entry) (plist-get (nth 2 entry) :terminal-p)) ordered)
                            (cl-remove-if-not (lambda (entry) (plist-get (nth 2 entry) :terminal-p)) ordered))
                    mevedel-collaboration--background-row-limit))
           (entries (cl-remove-if-not (lambda (entry) (memq entry chosen)) ordered))
           (rows (cl-loop for entry in entries for order from 0
                          collect (mevedel-collaboration--background-row session entry order)))
           ;; Reserve the small frame envelope and JSON array separators.
           (remaining (- mevedel-collaboration--background-frame-bytes 256))
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

(defun mevedel-collaboration--publish-background (room)
  "Broadcast ROOM's background rows only when their visible facts change."
  (let ((frame (mevedel-collaboration--background-frame room)))
    (unless (equal frame (plist-get room :background-frame))
      (plist-put room :background-frame frame)
      (mevedel-collaboration--broadcast room frame))))

(defun mevedel-collaboration--background-event (event)
  "Observe passive execution EVENT, retaining terminal facts before removal."
  (when-let* ((room (mevedel-collaboration--room-for-session (plist-get event :session))))
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
                                     (mevedel-agent-control-model-metadata (plist-get event :session) owner))
                                 :whole-output (plist-get event :whole-output)
                                 :whole-output-truncated-p (plist-get event :whole-output-truncated-p))
                           facts previous)))
        (when (plist-get facts :execution-id)
          (mevedel-collaboration--background-retain room (list key 'execution data)))))
    (mevedel-collaboration--schedule-publish room)))

(defun mevedel-collaboration--handle-background-stop (room peer frame)
  "Handle writable guest PEER's typed stop FRAME in ROOM.
Keys are presentation identity only; the current scoped host registry proves
the target.  The native user-authority API rechecks liveness at the action."
  (let* ((guest (mevedel-collaboration--guest room peer))
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
                              (assoc path (mevedel-session-agent-registry session)))
                   (user-error "Agent is no longer available"))
                 (list (list 'agent path) 'agent (list :path path)))
             (let ((snapshot
                    (and (stringp id) (stringp owner)
                         (cl-find-if
                          (lambda (item)
                            (and (plist-get item :yielded)
                                 (equal owner (plist-get item :owner))
                                 (equal id (plist-get item :execution-id))))
                          (mevedel-view-execution--live session)))))
               (unless snapshot (user-error "Execution is no longer available"))
               (list (mevedel-view-execution--key 'execution snapshot) 'execution snapshot)))))
      (when (and key (not (equal key (mevedel-collaboration--background-key entry))))
        (user-error "Background identity does not match"))
      (if path (mevedel-agent-control-interrupt-user session path)
        (mevedel-execution-stop-user session id)))))

(add-hook 'mevedel-execution-event-functions #'mevedel-collaboration--background-event)

(provide 'mevedel-collaboration-background)
;;; mevedel-collaboration-background.el ends here
