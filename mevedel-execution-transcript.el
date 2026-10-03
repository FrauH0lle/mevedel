;;; mevedel-execution-transcript.el -- Durable execution transcript state -*- lexical-binding: t -*-

;;; Commentary:

;; Owns durable Bash execution render data, compaction archive records, and
;; terminal reconciliation.  Live progress presentation remains in
;; mevedel-view-stream.el; segment publication remains in
;; mevedel-session-artifacts.el.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-execution-telemetry)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-audit)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x))

;; `mevedel-execution-telemetry'
(declare-function mevedel-execution-telemetry-sandbox-summary-class
                  "mevedel-execution-telemetry" (summary))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-publish-transcript-state
                  "mevedel-session-artifacts"
                  (session root-buffer transcript-path content &optional coding))
(declare-function mevedel-session-artifacts-read-artifact
                  "mevedel-session-artifacts"
                  (session logical &optional committed-only))
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-stabilize-gptel-bounds
                  "mevedel-session-artifacts" ())
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts" (session live-buffer))
(autoload 'mevedel-session-artifacts-publish-transcript-state
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-artifact
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-read-transcript-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-stabilize-gptel-bounds
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-transcript-segments "mevedel-session-artifacts")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p
                  "mevedel-session-codec" (session))
(autoload 'mevedel-session-codec-portable-authority-p
  "mevedel-session-codec")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-write-current-buffer-atomically
                  "mevedel-session-persistence" (path))
(autoload 'mevedel-session-persistence-write-current-buffer-atomically
  "mevedel-session-persistence")

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
(autoload 'mevedel-transcript-restore-properties "mevedel-transcript-restore")

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

;; `org'
(declare-function org-mode "org" (&optional arg))
(defvar org-agenda-file-menu-enabled)

(defvar-local mevedel-execution-transcript--pending-terminals nil
  "Tool render-data updates waiting for their transcript rows.")

(defvar-local mevedel-execution-transcript--completion-index nil
  "(TICK . TABLE) mapping tool-use ids to their newest recorded render data.")

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
           (list :status (if (plist-get (plist-get event :observation) :error)
                             'error
                           (mevedel-execution-transcript--status facts))
                 :live-execution-p nil
                 :output-preview-truncated-p
                 (plist-get event :whole-output-truncated-p)
                 :sandbox-facts
                 (copy-tree
                  (plist-get (plist-get event :observation) :sandbox-facts))
                 :execution-error
                 (copy-tree (plist-get (plist-get event :observation) :error))
                 :execution-output
                 (copy-sequence (or (plist-get event :whole-output) ""))))))
    (if (and summary
             (mevedel-execution-telemetry-sandbox-summary-class summary))
        (plist-put render-data :sandbox-summary summary)
      render-data)))

(defun mevedel-execution-transcript--breadcrumb-present-p (record)
  "Return non-nil when this receiving transcript already contains RECORD.
Completion retries after compaction must not create a second breadcrumb in
the new segment of the same transcript."
  (cl-labels ((present (buffer)
                (with-current-buffer buffer
                  (save-restriction
                    (widen)
                    (cl-some
                     (lambda (existing)
                       (and (equal (plist-get existing :execution-id)
                                   (plist-get record :execution-id))
                            (equal (plist-get existing :owner)
                                   (plist-get record :owner))))
                     (mevedel-transcript-audit-records
                      (buffer-substring (point-min) (point-max))
                      'execution-breadcrumb))))))
    (or (present (current-buffer))
        (when mevedel--session
          (catch 'found
            (dolist (descriptor (mevedel-session-artifacts-transcript-segments
                                 mevedel--session (current-buffer)))
              (when (and (eq (plist-get descriptor :status) 'readable)
                         (not (plist-get descriptor :current-p)))
                (when-let* ((older (condition-case nil
                                      (mevedel-session-artifacts-read-transcript-segment
                                       mevedel--session descriptor)
                                    (error nil))))
                  (unwind-protect
                      (when (present older) (throw 'found t))
                    (kill-buffer older))))))))))

(defun mevedel-execution-transcript--persist-terminal-record
    (record &optional terminal-data)
  "Persist RECORD beside the current transcript without saving unsent edits.
The visited buffer remains authoritative in memory.  Update only its last
published file with the hidden record and, when TERMINAL-DATA is provided,
the matching original Bash row; do not publish an unsent in-memory draft."
  (when-let* ((path buffer-file-name)
              ((file-exists-p path)))
    (let* ((session (or mevedel--session
                        (error "Transcript has no session")))
           (portable (mevedel-session-codec-portable-authority-p session))
           (root-buffer
            (and portable
                 (if (bound-and-true-p mevedel--agent-invocation)
                     (mevedel-agent-invocation-parent-data-buffer
                      mevedel--agent-invocation)
                   (current-buffer))))
           (coding (or buffer-file-coding-system 'utf-8-unix)))
      (with-temp-buffer
        (if portable
            (progn
              (setq buffer-file-coding-system coding)
              (insert
               (decode-coding-string
                (mevedel-session-artifacts-read-artifact
                 session
                 (file-relative-name
                  path (mevedel-session-save-path session)))
                coding)))
          (insert-file-contents path))
        (let ((org-agenda-file-menu-enabled nil)) (org-mode))
        (mevedel-transcript-restore-properties)
        (when terminal-data
          (mevedel-tool-render-data-update
           (current-buffer) (plist-get record :tool-use-id) terminal-data))
        (unless (if (eq (plist-get record :type) 'execution-breadcrumb)
                    (mevedel-execution-transcript--breadcrumb-present-p record)
                  (mevedel-execution-transcript--completion-record-p
                   (plist-get record :tool-use-id) record))
          (goto-char (point-max))
          (insert (mevedel--format-hook-audit-record record))
          (mevedel-session-artifacts-stabilize-gptel-bounds)
          (if portable
              (mevedel-session-artifacts-publish-transcript-state
               session root-buffer path
               (buffer-substring-no-properties (point-min) (point-max))
               coding)
            (mevedel-session-persistence-write-current-buffer-atomically
             path))))
      (set-visited-file-modtime)
      t)))

(defun mevedel-execution-transcript--record-breadcrumb (event)
  "Record one chronological completion for terminal yielded EVENT.
The event's execution identity is absent for foreground commands.  This
record carries no output copy: the tool row and retained artifact own it."
  (when-let* ((data-buffer (plist-get event :data-buffer))
              ((buffer-live-p data-buffer))
              (facts (plist-get event :facts))
              (execution-id (plist-get facts :execution-id))
              (tool-use-id (plist-get event :tool-use-id)))
    (with-current-buffer data-buffer
      (save-restriction
        (widen)
        (let* ((inhibit-read-only t)
               (modified-p (buffer-modified-p))
               (persisted-p nil)
               (record (list :type 'execution-breadcrumb
                             :execution-id execution-id
                             :tool-use-id tool-use-id
                             :owner (plist-get event :owner)
                             :command (plist-get facts :command)
                             :facts (copy-tree facts)
                             :source-target buffer-file-name
                             :emitted-at (plist-get event :emitted-at))))
          (unless (mevedel-execution-transcript--breadcrumb-present-p record)
            (setq persisted-p
                  (condition-case err
                      (mevedel-execution-transcript--persist-terminal-record
                       record
                       (mevedel-execution-transcript-terminal-render-data event))
                    (error
                     (display-warning
                      'mevedel
                      (format "Could not publish execution completion %s: %s"
                              execution-id (error-message-string err))
                      :warning)
                     nil)))
            (save-excursion
              (goto-char (point-max))
              (insert (mevedel--format-hook-audit-record record)))
            (set-buffer-modified-p (or modified-p (not persisted-p)))))))))

(defun mevedel-execution-transcript--replace-archived-record
    (tool-use-id replacement)
  "Replace current buffer's archived TOOL-USE-ID with REPLACEMENT.
When an earlier failed live publication already retained REPLACEMENT, remove
only the stale archive so a later retry does not leave two completions."
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
      (unless (mevedel-execution-transcript--completion-record-p
               tool-use-id replacement)
        (goto-char begin)
        (insert (mevedel--format-hook-audit-record replacement)))
      t)))

(defun mevedel-execution-transcript--completion-record-p
    (tool-use-id &optional expected)
  "Return non-nil when the current buffer completed TOOL-USE-ID.
When EXPECTED is non-nil, require the durable record to equal it."
  (cl-some
   (lambda (record)
     (and (equal (plist-get record :tool-use-id) tool-use-id)
          (or (null expected) (equal record expected))))
   (mevedel-transcript-audit-records
    (buffer-substring (point-min) (point-max))
    'execution-completion)))

(defun mevedel-execution-transcript--nested-tool-use-id-p (tool-use-id)
  "Return non-nil for a ToolCall child TOOL-USE-ID.
The PTC driver assigns child ids by appending a slash and call number to
the parent's tool-use id; unlike the parent, these have no gptel segment."
  (and (stringp tool-use-id)
       (string-match-p "/[0-9]+\\'" tool-use-id)))

(defun mevedel-execution-transcript--record-nested-terminal
    (data-buffer event render-data)
  "Keep nested EVENT's terminal RENDER-DATA durable without a child tool row.
The enclosing ToolCall row holds the child's initial output; the hidden
completion holds its updated bounded output when it settles after the call."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (save-restriction
        (widen)
        (let* ((inhibit-read-only t)
               (modified-p (buffer-modified-p))
               (tool-use-id (plist-get event :tool-use-id))
               (record (list :type 'execution-completion
                             :tool-use-id tool-use-id
                             :owner (plist-get event :owner)
                             :render-data render-data)))
          (unless (mevedel-execution-transcript--completion-record-p
                   tool-use-id record)
            (let ((persisted-p
                   (condition-case err
                       (mevedel-execution-transcript--persist-terminal-record
                        record)
                     (error
                      (display-warning
                       'mevedel
                       (format "Could not publish nested execution %s: %s"
                               tool-use-id (error-message-string err))
                       :warning)
                      nil))))
              (save-excursion
                (goto-char (point-max))
                (insert (mevedel--format-hook-audit-record record)))
              (set-buffer-modified-p (or modified-p (not persisted-p))))))))))

(defun mevedel-execution-transcript--record-archived-terminal
    (data-buffer event render-data)
  "Record terminal EVENT in DATA-BUFFER after its original row was archived.
RENDER-DATA is retained in the hidden transcript audit record."
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
            (let* ((session (or mevedel--session
                                (error "Transcript has no session")))
                   (portable
                    (mevedel-session-codec-portable-authority-p session))
                   (root-buffer
                    (and portable
                         (if (bound-and-true-p mevedel--agent-invocation)
                             (mevedel-agent-invocation-parent-data-buffer
                              mevedel--agent-invocation)
                           data-buffer)))
                   (coding (or buffer-file-coding-system 'utf-8-unix)))
              (with-temp-buffer
                (if portable
                    (progn
                      (setq buffer-file-coding-system coding)
                      (insert
                       (decode-coding-string
                        (mevedel-session-artifacts-read-artifact
                         session
                         (file-relative-name
                          path (mevedel-session-save-path session)))
                        coding)))
                  (insert-file-contents path))
                (let ((org-agenda-file-menu-enabled nil))
                  (org-mode))
                (mevedel-transcript-restore-properties)
                (unless
                    (or
                     (mevedel-execution-transcript--replace-archived-record
                      tool-use-id replacement)
                     (mevedel-execution-transcript--completion-record-p
                      tool-use-id replacement))
                  (error "Persisted execution record missing: %s" tool-use-id))
                (mevedel-session-artifacts-stabilize-gptel-bounds)
                (if portable
                    (mevedel-session-artifacts-publish-transcript-state
                     session root-buffer path
                     (buffer-substring-no-properties (point-min) (point-max))
                     coding)
                  (mevedel-session-persistence-write-current-buffer-atomically
                   path))))
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

(defun mevedel-execution-transcript-prepare-archive
    (data-buffer tool-use-ids)
  "Return a compaction plan for TOOL-USE-IDS removed from DATA-BUFFER.
Inspect archived records once per call, only when a live row is missing."
  (let (archived live completed)
    (dolist (tool-use-id tool-use-ids)
      (when-let* ((render-data
                   (or (mevedel-tool-render-data-for-tool
                        data-buffer tool-use-id)
                       (when (buffer-live-p data-buffer)
                         (unless archived
                           (setq archived (make-hash-table :test #'equal))
                           (with-current-buffer data-buffer
                             (save-restriction
                               (widen)
                               (dolist (record
                                        (mevedel-transcript-audit-records
                                         (buffer-substring (point-min) (point-max))))
                                 (when (memq (plist-get record :type)
                                             '(execution-archive execution-completion))
                                   (let ((id (plist-get record :tool-use-id)))
                                     (when (or (eq (plist-get record :type)
                                                   'execution-completion)
                                               (not (gethash id archived)))
                                       (puthash id record archived))))))))
                         (copy-tree
                          (plist-get (gethash tool-use-id archived)
                                     :render-data)))))
                  ((or (plist-get render-data :execution-id)
                       (plist-get render-data :live-execution-p))))
        (if (plist-get render-data :live-execution-p)
            (push (cons tool-use-id render-data) live)
          (push (cons tool-use-id render-data) completed))))
    (list :live (nreverse live) :completed (nreverse completed))))

(defun mevedel-execution-transcript-archive-text (plan)
  "Return durable hidden transcript records for archive PLAN."
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
  "Return unsettled or retained completion data for TOOL-USE-ID in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (or (and (hash-table-p mevedel-execution-transcript--pending-terminals)
               (copy-tree
                (plist-get
                 (gethash tool-use-id
                          mevedel-execution-transcript--pending-terminals)
                 :render-data)))
          (copy-tree
           (gethash tool-use-id
                    (mevedel-execution-transcript--completion-index)))))))

(defun mevedel-execution-transcript--completion-index ()
  "Return the current buffer's newest completion render data by tool-use id.
A view asks once per Bash row on every streamed update; parsing the whole
transcript for each question made long turns quadratic.  Rebuild only
after the text changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick (car mevedel-execution-transcript--completion-index))
      (let ((table (make-hash-table :test #'equal)))
        (save-restriction
          (widen)
          ;; Later records replace earlier ones: the newest completion wins.
          (dolist (record (mevedel-transcript-audit-records
                           (buffer-substring (point-min) (point-max))
                           'execution-completion))
            (puthash (plist-get record :tool-use-id)
                     (plist-get record :render-data) table)))
        (setq mevedel-execution-transcript--completion-index
              (cons tick table))))
    (cdr mevedel-execution-transcript--completion-index)))

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
            (mevedel--warn-once
             'execution-transcript-eviction
             "Discarding stale render-data update for tool %s" evicted))))
      (puthash tool-use-id
               (list :event event :render-data render-data)
               table))))

(defun mevedel-execution-transcript-retry-pending-terminals (data-buffer)
  "Persist pending tool updates whose rows exist in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (hash-table-p mevedel-execution-transcript--pending-terminals)
        (let (settled)
          (maphash
           (lambda (tool-use-id pending)
             (let ((event (plist-get pending :event))
                   (render-data (plist-get pending :render-data)))
               ;; One unsettled entry must not abort the sweep, and this
               ;; runs before the caller releases its turn.
               (condition-case err
                   (cond
                    ((mevedel-tool-render-data-update
                      data-buffer tool-use-id render-data)
                     (push tool-use-id settled))
                    ((and (eq (plist-get event :type) 'terminal)
                          (mevedel-execution-transcript--archived-row-p
                           data-buffer tool-use-id))
                     (mevedel-execution-transcript--record-archived-terminal
                      data-buffer event render-data)
                     (push tool-use-id settled))
                    ((and (eq (plist-get event :type) 'terminal)
                          (mevedel-execution-transcript--nested-tool-use-id-p
                           tool-use-id))
                     (mevedel-execution-transcript--record-nested-terminal
                      data-buffer event render-data)
                     (push tool-use-id settled)))
                 (error
                  (display-warning
                   'mevedel
                   (format "Could not settle pending execution %s: %s"
                           tool-use-id (error-message-string err))
                   :warning)))))
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
             ;; If publication fails, a later ordinary transcript save still
             ;; carries the terminal truth, even without another event or a
             ;; pending-table retry.  Keep the retry for prompt disk recovery.
             (when (buffer-live-p data-buffer)
               (with-current-buffer data-buffer
                 (save-restriction
                   (widen)
                   (let ((inhibit-read-only t)
                         (replacement
                          (list :type 'execution-completion
                                :tool-use-id tool-use-id
                                :owner (plist-get event :owner)
                                :render-data render-data)))
                     (unless (or (condition-case nil
                                     (mevedel-execution-transcript--replace-archived-record
                                      tool-use-id replacement)
                                   (error nil))
                                 (mevedel-execution-transcript--completion-record-p
                                  tool-use-id replacement))
                       (save-excursion
                         (goto-char (point-max))
                         (insert (mevedel--format-hook-audit-record replacement))))
                     (set-buffer-modified-p t)))))
             (mevedel-execution-transcript-store-pending-terminal
              data-buffer event render-data)
             (display-warning
              'mevedel
              (format "Could not persist archived execution %s: %s"
                      tool-use-id (error-message-string err))
              :warning))))
         ((mevedel-execution-transcript--nested-tool-use-id-p tool-use-id)
          (condition-case err
              (mevedel-execution-transcript--record-nested-terminal
               data-buffer event render-data)
            (error
             (mevedel-execution-transcript-store-pending-terminal
              data-buffer event render-data)
             (display-warning
              'mevedel
              (format "Could not retain nested execution %s: %s"
                      tool-use-id (error-message-string err))
              :warning))))
         (t
          (mevedel-execution-transcript-store-pending-terminal
           data-buffer event render-data)))
        (condition-case err
            (mevedel-execution-transcript--record-breadcrumb event)
          (error
           (display-warning
            'mevedel
            (format "Could not persist execution completion %s: %s"
                    tool-use-id (error-message-string err))
            :warning))))))
  nil)

(provide 'mevedel-execution-transcript)

;;; mevedel-execution-transcript.el ends here
