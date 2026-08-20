;;; mevedel-execution-transcript.el -- Durable execution transcript state -*- lexical-binding: t -*-

;;; Commentary:

;; Owns durable Bash execution render data, compaction archive records, and
;; terminal reconciliation.  Live progress presentation remains in
;; mevedel-view-stream.el; segment publication remains in
;; mevedel-session-artifacts.el.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x))

;; `mevedel-execution'
(declare-function mevedel-execution-sandbox-summary-class
                  "mevedel-execution" (summary))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-publish-transcript-state
                  "mevedel-session-artifacts"
                  (session root-buffer transcript-path content &optional coding))
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds
                  "mevedel-session-artifacts" ())

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-write-current-buffer-atomically
                  "mevedel-session-persistence" (path))

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--session)

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-for-tool
                  "mevedel-tool-render-data" (buffer tool-use-id))
(declare-function mevedel-tool-render-data-update
                  "mevedel-tool-render-data" (buffer tool-use-id updates))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(declare-function mevedel-transcript-audit-records
                  "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-audit-spans
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-transcript-restore'
(declare-function mevedel-transcript-restore-properties
                  "mevedel-transcript-restore" (&optional only-if-missing))

;; `org'
(declare-function org-mode "org" (&optional arg))
(defvar org-agenda-file-menu-enabled)

(defvar-local mevedel-execution-transcript--pending-terminals nil
  "Tool render-data updates waiting for their transcript rows.")

(defvar-local mevedel-execution-transcript--archived-rows nil
  "Execution tool-use ids whose rows were explicitly removed by compaction.")

(defconst mevedel-execution-transcript--pending-terminal-limit 64
  "Maximum render-data updates retained while rows are unavailable.")

(defun mevedel-execution-transcript--status (facts)
  "Return visual status for terminal execution FACTS."
  (if (memq (plist-get facts :outcome)
            '(success no-match different false))
      'success
    'error))

(defun mevedel-execution-transcript-terminal-render-data (event)
  "Return durable terminal render data projected from EVENT."
  (let* ((facts (copy-tree (plist-get event :facts)))
         (summary
          (copy-tree
           (plist-get (plist-get event :observation) :sandbox-summary)))
         (render-data
          (append
           facts
           (list :status (mevedel-execution-transcript--status facts)
                 :live-execution-p nil
                 :sandbox-facts
                 (copy-tree
                  (plist-get (plist-get event :observation) :sandbox-facts))
                 :execution-output
                 (copy-sequence (or (plist-get event :whole-output) ""))))))
    (if (and summary
             (progn
               (require 'mevedel-execution)
               (mevedel-execution-sandbox-summary-class summary)))
        (plist-put render-data :sandbox-summary summary)
      render-data)))

(defun mevedel-execution-transcript--replace-archived-record
    (tool-use-id replacement)
  "Replace current buffer's archived TOOL-USE-ID with REPLACEMENT."
  (require 'cl-lib)
  (require 'mevedel-transcript-audit)
  (when-let* ((span
               (cl-find-if
                (lambda (candidate)
                  (equal
                   (plist-get (plist-get candidate :record) :tool-use-id)
                   tool-use-id))
                (mevedel-transcript-audit-spans
                 (buffer-substring (point-min) (point-max))
                 'execution-archive))))
    (let ((begin (+ (point-min) (plist-get span :start)))
          (end (+ (point-min) (plist-get span :end))))
      (delete-region begin end)
      (goto-char begin)
      (insert (mevedel--format-hook-audit-record replacement))
      t)))

(defun mevedel-execution-transcript--completion-record-p
    (tool-use-id &optional expected)
  "Return non-nil when the current buffer completed TOOL-USE-ID.
When EXPECTED is non-nil, require the durable record to equal it."
  (require 'cl-lib)
  (require 'mevedel-transcript-audit)
  (cl-some
   (lambda (record)
     (and (equal (plist-get record :tool-use-id) tool-use-id)
          (or (null expected) (equal record expected))))
   (mevedel-transcript-audit-records
    (buffer-substring (point-min) (point-max))
    'execution-completion)))

(defun mevedel-execution-transcript--record-archived-terminal
    (data-buffer event render-data)
  "Record terminal EVENT in DATA-BUFFER after its original row was archived.
RENDER-DATA is retained in the hidden transcript audit record."
  (require 'mevedel-session-persistence)
  (require 'mevedel-session-artifacts)
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (save-restriction
        (widen)
        (let* ((inhibit-read-only t)
               (modified-p (buffer-modified-p))
               (tool-use-id (plist-get event :tool-use-id))
               (path buffer-file-name)
               (replacement
                (list :type 'execution-completion
                      :tool-use-id tool-use-id
                      :owner (plist-get event :owner)
                      :render-data render-data))
               (marker-table mevedel-execution-transcript--archived-rows))
          (when path
            (if (file-remote-p path)
                (let* ((session (or mevedel--session
                                    (error "Remote transcript has no session")))
                       (save-path (mevedel-session-save-path session))
                       (logical (file-relative-name path save-path))
                       (root-buffer
                        (if (bound-and-true-p mevedel--agent-invocation)
                            (mevedel-agent-invocation-parent-data-buffer
                             mevedel--agent-invocation)
                          data-buffer))
                       (coding (or buffer-file-coding-system 'utf-8-unix)))
                  (with-temp-buffer
                    (setq buffer-file-coding-system coding)
                    (insert
                     (decode-coding-string
                      (mevedel-session-artifacts-read-artifact
                       session logical)
                      coding))
                    (let ((org-agenda-file-menu-enabled nil))
                      (org-mode))
                    (require 'mevedel-transcript-restore)
                    (mevedel-transcript-restore-properties)
                    (unless
                        (or
                         (mevedel-execution-transcript--replace-archived-record
                          tool-use-id replacement)
                         (mevedel-execution-transcript--completion-record-p
                          tool-use-id replacement))
                      (error "Persisted execution record missing: %s"
                             tool-use-id))
                    (mevedel-session-artifacts-stabilize-gptel-bounds)
                    (mevedel-session-artifacts-publish-transcript-state
                     session root-buffer path
                     (buffer-substring-no-properties (point-min) (point-max))
                     coding)))
              (with-temp-buffer
                (insert-file-contents path)
                (let ((org-agenda-file-menu-enabled nil))
                  (org-mode))
                (require 'mevedel-transcript-restore)
                (mevedel-transcript-restore-properties)
                (unless
                    (or
                     (mevedel-execution-transcript--replace-archived-record
                      tool-use-id replacement)
                     (mevedel-execution-transcript--completion-record-p
                      tool-use-id replacement))
                  (error "Persisted execution record missing: %s" tool-use-id))
                (mevedel-session-artifacts-stabilize-gptel-bounds)
                (mevedel-session-persistence-write-current-buffer-atomically
                 path)))
            ;; The atomic rename changed the visited file.  Refresh the
            ;; buffer's baseline before applying the same replacement in
            ;; memory, otherwise Emacs may report a spurious file-supersession
            ;; conflict.
            (set-visited-file-modtime))
          (save-excursion
            (unless
                (or (mevedel-execution-transcript--replace-archived-record
                     tool-use-id replacement)
                    (mevedel-execution-transcript--completion-record-p
                     tool-use-id replacement))
              (error "Archived execution record missing: %s" tool-use-id)))
          (when path
            (mevedel-session-artifacts-stabilize-gptel-bounds))
          (set-buffer-modified-p modified-p)
          (when (hash-table-p marker-table)
            (remhash tool-use-id marker-table)))))))

(defun mevedel-execution-transcript--archived-render-data
    (data-buffer tool-use-id)
  "Return archived render data for TOOL-USE-ID in DATA-BUFFER."
  (require 'cl-lib)
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (save-restriction
        (widen)
        (require 'mevedel-transcript-audit)
        (when-let* ((record
                     (cl-find-if
                      (lambda (candidate)
                        (equal (plist-get candidate :tool-use-id)
                               tool-use-id))
                      (mevedel-transcript-audit-records
                       (buffer-substring
                        (point-min) (point-max))
                       'execution-archive))))
          (copy-tree (plist-get record :render-data)))))))

(defun mevedel-execution-transcript-prepare-archive
    (data-buffer tool-use-ids)
  "Return a compaction plan for TOOL-USE-IDS removed from DATA-BUFFER."
  (require 'mevedel-tool-render-data)
  (let (live completed)
    (dolist (tool-use-id tool-use-ids)
      (when-let* ((render-data
                   (or (mevedel-tool-render-data-for-tool
                        data-buffer tool-use-id)
                       (mevedel-execution-transcript--archived-render-data
                        data-buffer tool-use-id)))
                  ((or (plist-get render-data :execution-id)
                       (plist-get render-data :live-execution-p))))
        (if (plist-get render-data :live-execution-p)
            (push (cons tool-use-id render-data) live)
          (push (cons tool-use-id render-data) completed))))
    (list :live (nreverse live) :completed (nreverse completed))))

(defun mevedel-execution-transcript-archive-text (plan)
  "Return durable hidden transcript records for archive PLAN."
  (require 'mevedel-transcript-audit)
  (concat
   (mapconcat
    (lambda (entry)
      (mevedel--format-hook-audit-record
       (list :type 'execution-archive
             :tool-use-id (car entry)
             :render-data (cdr entry))))
    (plist-get plan :live)
    "")
   (mapconcat
    (lambda (entry)
      (mevedel--format-hook-audit-record
       (list :type 'execution-completion
             :tool-use-id (car entry)
             :render-data (cdr entry))))
    (plist-get plan :completed)
    "")))

(defun mevedel-execution-transcript-commit-archive
    (data-buffer plan)
  "Commit execution row archive PLAN after DATA-BUFFER compaction succeeds."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when-let* ((live (plist-get plan :live)))
        (unless (hash-table-p mevedel-execution-transcript--archived-rows)
          (setq-local mevedel-execution-transcript--archived-rows
                      (make-hash-table :test #'equal)))
        (dolist (entry live)
          (puthash (car entry) t
                   mevedel-execution-transcript--archived-rows))))))

(defun mevedel-execution-transcript--archived-row-p
    (data-buffer tool-use-id)
  "Return non-nil when DATA-BUFFER archived TOOL-USE-ID."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (and (hash-table-p mevedel-execution-transcript--archived-rows)
           (gethash tool-use-id
                    mevedel-execution-transcript--archived-rows)))))

(defun mevedel-execution-transcript--pending-table (data-buffer)
  "Return DATA-BUFFER's pending terminal table, creating it if needed."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (unless (hash-table-p mevedel-execution-transcript--pending-terminals)
        (setq-local mevedel-execution-transcript--pending-terminals
                    (make-hash-table :test #'equal)))
      mevedel-execution-transcript--pending-terminals)))

(defun mevedel-execution-transcript-pending-render-data
    (data-buffer tool-use-id)
  "Return pending render data for TOOL-USE-ID in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (and (hash-table-p mevedel-execution-transcript--pending-terminals)
           (copy-tree
            (plist-get
             (gethash tool-use-id
                      mevedel-execution-transcript--pending-terminals)
             :render-data))))))

(defun mevedel-execution-transcript-store-pending-terminal
    (data-buffer event render-data)
  "Retain EVENT's RENDER-DATA until its durable row is ready."
  (when-let* ((table
               (mevedel-execution-transcript--pending-table data-buffer)))
    (let ((tool-use-id (plist-get event :tool-use-id)))
      (when (and (not (gethash tool-use-id table))
                 (>= (hash-table-count table)
                     mevedel-execution-transcript--pending-terminal-limit))
        (let (evicted)
          (maphash (lambda (key _value)
                     (unless evicted (setq evicted key)))
                   table)
          (when evicted
            (remhash evicted table)
            (display-warning
             'mevedel
             (format "Discarding stale render-data update for tool %s" evicted)
             :warning))))
      (puthash tool-use-id
               (list :event event :render-data render-data)
               table))))

(defun mevedel-execution-transcript-retry-pending-terminals (data-buffer)
  "Persist pending tool updates whose rows exist in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (hash-table-p mevedel-execution-transcript--pending-terminals)
        (require 'mevedel-tool-render-data)
        (let (settled)
          (maphash
           (lambda (tool-use-id pending)
             (let ((event (plist-get pending :event))
                   (render-data (plist-get pending :render-data)))
               (cond
                ((mevedel-tool-render-data-update
                  data-buffer tool-use-id render-data)
                 (push tool-use-id settled))
                ((and (eq (plist-get event :type) 'terminal)
                      (mevedel-execution-transcript--archived-row-p
                       data-buffer tool-use-id))
                 (condition-case err
                     (progn
                       (mevedel-execution-transcript--record-archived-terminal
                        data-buffer event render-data)
                       (push tool-use-id settled))
                   (error
                    (display-warning
                     'mevedel
                     (format "Could not persist archived execution %s: %s"
                             tool-use-id (error-message-string err))
                     :warning)))))))
           mevedel-execution-transcript--pending-terminals)
          (dolist (tool-use-id settled)
            (remhash tool-use-id
                     mevedel-execution-transcript--pending-terminals)))))))

(defun mevedel-execution-transcript-retry-terminals (&rest _args)
  "Persist pending tool updates in the current data buffer."
  (mevedel-execution-transcript-retry-pending-terminals (current-buffer))
  nil)

(defun mevedel-execution-transcript-handle-event (event)
  "Persist terminal Bash EVENT in its authoritative transcript row.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (let ((type (plist-get event :type))
        (tool-use-id (plist-get event :tool-use-id))
        (data-buffer (plist-get event :data-buffer)))
    (when (and (eq type 'terminal) tool-use-id)
      (require 'mevedel-tool-render-data)
      (let ((render-data
             (mevedel-execution-transcript-terminal-render-data event)))
        (cond
         ((mevedel-tool-render-data-update
           data-buffer tool-use-id render-data)
          (when-let* ((table
                       (mevedel-execution-transcript--pending-table
                        data-buffer)))
            (remhash tool-use-id table)))
         ((mevedel-execution-transcript--archived-row-p
           data-buffer tool-use-id)
          (condition-case err
              (mevedel-execution-transcript--record-archived-terminal
               data-buffer event render-data)
            (error
             (mevedel-execution-transcript-store-pending-terminal
              data-buffer event render-data)
             (display-warning
              'mevedel
              (format "Could not persist archived execution %s: %s"
                      tool-use-id (error-message-string err))
              :warning))))
         (t
          (mevedel-execution-transcript-store-pending-terminal
           data-buffer event render-data))))))
  nil)

(provide 'mevedel-execution-transcript)

;;; mevedel-execution-transcript.el ends here
