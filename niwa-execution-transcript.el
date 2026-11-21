;;; niwa-execution-transcript.el -- Durable execution transcript state -*- lexical-binding: t -*-

;;; Commentary:

;; Owns durable Bash execution render data, compaction archive records, and
;; terminal reconciliation.  Live progress presentation remains in
;; niwa-view-stream.el; segment publication remains in
;; niwa-session-artifacts.el.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-execution-telemetry)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-audit)
(require 'niwa-utilities)

;; `niwa-execution-telemetry'
(declare-function niwa-execution-telemetry-sandbox-summary-class
                  "niwa-execution-telemetry" (summary))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-artifact-present-p
                  "niwa-session-artifacts" (session logical &optional committed-only))
(declare-function niwa-session-artifacts-publish-transcript-state
                  "niwa-session-artifacts"
                  (session root-buffer transcript-path content checkpoint
                           &optional coding prepared-sidecar))
(declare-function niwa-session-artifacts-read-artifact
                  "niwa-session-artifacts"
                  (session logical &optional committed-only))
(declare-function niwa-session-artifacts-read-transcript-segment
                  "niwa-session-artifacts" (session descriptor))
(declare-function niwa-session-artifacts-stabilize-gptel-bounds
                  "niwa-session-artifacts" ())
(declare-function niwa-session-artifacts-transcript-segment-identity
                  "niwa-session-artifacts" (session descriptor))
(declare-function niwa-session-artifacts-transcript-segments
                  "niwa-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'niwa-session-artifacts-artifact-present-p
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-publish-transcript-state
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-artifact
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-read-transcript-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-stabilize-gptel-bounds
  "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-transcript-segments "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-transcript-segment-identity
  "niwa-session-artifacts")

;; `niwa-session-codec'
(declare-function niwa-session-codec-portable-authority-p
                  "niwa-session-codec" (session))
(autoload 'niwa-session-codec-portable-authority-p
  "niwa-session-codec")

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-autosave-soon
                  "niwa-session-persistence" (buffer))
(declare-function niwa-session-persistence-checkpoints-active-p
                  "niwa-session-persistence" ())
(autoload 'niwa-session-persistence-autosave-soon
  "niwa-session-persistence")
(autoload 'niwa-session-persistence-checkpoints-active-p
  "niwa-session-persistence")

;; `niwa-structs'
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(defvar niwa--agent-invocation)
(defvar niwa--session)

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-for-tool
                  "niwa-tool-render-data" (buffer tool-use-id))
(declare-function niwa-tool-render-data-update
                  "niwa-tool-render-data" (buffer tool-use-id updates))

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(declare-function niwa-transcript-audit-records
                  "niwa-transcript-audit" (text &optional type))
(declare-function niwa-transcript-audit-spans
                  "niwa-transcript-audit" (text &optional type))

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties
                  "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-utilities'
(declare-function niwa--insert-at-active-response
                  "niwa-utilities" (buffer text))
(declare-function niwa--transcript-org-mode "niwa-utilities" ())

;; `org'
(defvar org-agenda-file-menu-enabled)

(defvar-local niwa-execution-transcript--pending-terminals nil
  "Tool render-data updates waiting for their transcript rows.")

(defvar-local niwa-execution-transcript--completion-index nil
  "(TICK . TABLE) mapping tool-use ids to their newest recorded render data.")

(defvar-local niwa-execution-transcript--archived-update-cache nil
  "Last published archived updates, keyed by exact tool-use id.
Each value is (DESCRIPTOR IDENTITY UPDATES); identity guards external changes.")

(defvar-local niwa-execution-transcript--archived-rows nil
  "Execution tool-use ids whose rows were explicitly removed by compaction.")

(defun niwa-execution-transcript--status (facts)
  "Return visual status for terminal execution FACTS."
  (cond
   ((memq (plist-get facts :outcome) '(success no-match different false))
    'success)
   ;; A requested stop is neutral; the outcome fact stays `failure'.
   ((memq (plist-get facts :termination) '(stopped owner-stopped interrupted))
    'cancelled)
   (t 'error)))

(defun niwa-execution-transcript-terminal-render-data (event)
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
                           (niwa-execution-transcript--status facts))
                 :live-execution-p nil
                 :output-preview-truncated-p
                 (plist-get event :whole-output-truncated-p)
                 :sandbox-facts
                 (copy-tree
                  (plist-get (plist-get event :observation) :sandbox-facts))
                 :permission-approvals
                 (copy-tree
                  (plist-get (plist-get event :observation) :permission-approvals))
                 :execution-error
                 (copy-tree (plist-get (plist-get event :observation) :error))
                 :execution-output
                 (copy-sequence (or (plist-get event :whole-output) ""))))))
    (if (and summary
             (niwa-execution-telemetry-sandbox-summary-class summary))
        (plist-put render-data :sandbox-summary summary)
      render-data)))

(defvar niwa-execution-transcript--segment-breadcrumbs
  (make-hash-table :test #'equal)
  "Archived segment content identity -> its breadcrumb keys.")

(defconst niwa-execution-transcript--segment-breadcrumbs-limit 256
  "Maximum number of archived segments whose breadcrumb keys are retained.")

(defun niwa-execution-transcript--breadcrumb-keys (buffer)
  "Return (OWNER . EXECUTION-ID) for each completion breadcrumb in BUFFER."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (mapcar (lambda (record)
                (cons (plist-get record :owner) (plist-get record :execution-id)))
              (niwa-transcript-audit-records
               (buffer-substring (point-min) (point-max))
               'execution-breadcrumb)))))

(defun niwa-execution-transcript--segment-breadcrumb-keys (session descriptor)
  "Return breadcrumb keys of SESSION's archived segment DESCRIPTOR, or nil.
Keys are cached by the segment's content identity, a published hash or file
stat, rather than reread and parsed for every completion."
  (let* ((identity (niwa-session-artifacts-transcript-segment-identity
                    session descriptor))
         (cached (if identity
                     (gethash identity niwa-execution-transcript--segment-breadcrumbs
                              :missing)
                   :missing)))
    (if (not (eq cached :missing))
        cached
      (when-let* ((older (condition-case nil
                             (niwa-session-artifacts-read-transcript-segment
                              session descriptor)
                           (error nil))))
        (let ((keys (unwind-protect
                        (niwa-execution-transcript--breadcrumb-keys older)
                      (kill-buffer older))))
          (when identity
            (when (>= (hash-table-count niwa-execution-transcript--segment-breadcrumbs)
                      niwa-execution-transcript--segment-breadcrumbs-limit)
              (clrhash niwa-execution-transcript--segment-breadcrumbs))
            (puthash identity keys niwa-execution-transcript--segment-breadcrumbs))
          keys)))))

(defun niwa-execution-transcript--breadcrumb-present-p (record)
  "Return non-nil when this receiving transcript already contains RECORD.
Completion retries after compaction must not create a second breadcrumb in
the new segment of the same transcript."
  (let ((key (cons (plist-get record :owner) (plist-get record :execution-id))))
    (or (member key (niwa-execution-transcript--breadcrumb-keys (current-buffer)))
        (when niwa--session
          (cl-some (lambda (descriptor)
                     (and (eq (plist-get descriptor :status) 'readable)
                          (not (plist-get descriptor :current-p))
                          (member key (niwa-execution-transcript--segment-breadcrumb-keys
                                       niwa--session descriptor))))
                   (niwa-session-artifacts-transcript-segments
                    niwa--session (current-buffer)))))))

(defun niwa-execution-transcript--persist-terminal-record
    (record &optional terminal-data)
  "Persist RECORD beside the current transcript without saving unsent edits.
The visited buffer remains authoritative in memory.  Update only its last
published file with the hidden record and, when TERMINAL-DATA is provided,
the matching original Bash row; do not publish an unsent in-memory draft."
  (when-let* ((path buffer-file-name))
    (let* ((session (or niwa--session
                        (error "Transcript has no session")))
           (relative (file-relative-name
                      path (niwa-session-save-path session)))
           (coding (or buffer-file-coding-system 'utf-8-unix)))
      (when (and (niwa-session-codec-portable-authority-p session)
                 (niwa-session-artifacts-artifact-present-p
                  session relative))
        (with-temp-buffer
          (setq buffer-file-coding-system coding)
          (insert
           (decode-coding-string
            (niwa-session-artifacts-read-artifact session relative)
            coding))
          ;; Bare: the user's Org hooks cost 45 of 80 ms on a 2.9 MB copy.
          (let ((org-agenda-file-menu-enabled nil)) (niwa--transcript-org-mode))
          (niwa-transcript-restore-properties)
          (when terminal-data
            (niwa-tool-render-data-update
             (current-buffer) (plist-get record :tool-use-id) terminal-data))
          (unless (if (eq (plist-get record :type) 'execution-breadcrumb)
                      (niwa-execution-transcript--breadcrumb-present-p record)
                    (niwa-execution-transcript--completion-record-p
                     (plist-get record :tool-use-id) record))
            (goto-char (point-max))
            (insert (niwa--format-hook-audit-record record))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (niwa-session-artifacts-publish-transcript-state
             session (niwa-session-root-buffer session) path
             (buffer-substring-no-properties (point-min) (point-max))
             ;; These are committed bytes plus an audit, not a freeze of
             ;; the receiving live conversation.  Retain pending mail.
             nil coding)))
        (set-visited-file-modtime)
        t))))

(defun niwa-execution-transcript--record-breadcrumb (event)
  "Record one chronological completion for terminal yielded EVENT.
The event's execution identity is absent for foreground commands.  This
record carries no output copy: the tool row and retained artifact own it.
While the transcript's turn runs, the record goes where its response
currently ends, so the turn's later output follows it; an idle transcript
receives it at its end.

The record reaches disk with the transcript's next checkpoint, like the
running turn's own output: the in-flight checkpoint while a request runs,
or one queued here otherwise.  Publishing it at once re-read,
re-parsed and republished the whole transcript for one line, a 225 ms
stall per yielded command on a long session."
  (when-let* ((data-buffer (plist-get event :data-buffer))
              ((buffer-live-p data-buffer))
              (facts (plist-get event :facts))
              (execution-id (plist-get facts :execution-id))
              (tool-use-id (plist-get event :tool-use-id)))
    (with-current-buffer data-buffer
      (save-restriction
        (widen)
        (let* ((inhibit-read-only t)
               (record (list :type 'execution-breadcrumb
                             :execution-id execution-id
                             :tool-use-id tool-use-id
                             :owner (plist-get event :owner)
                             :command (plist-get facts :command)
                             :facts (copy-tree facts)
                             :source-target buffer-file-name
                             :emitted-at (plist-get event :emitted-at))))
          (unless (niwa-execution-transcript--breadcrumb-present-p record)
            (let* ((text (niwa--format-hook-audit-record record))
                   (active-position
                    (niwa--insert-at-active-response data-buffer text)))
              (unless active-position
                (save-excursion
                  (goto-char (point-max))
                  (insert text)))
              (set-buffer-modified-p t)
              ;; In-flight checkpoints carry it.  Without them an idle
              ;; editor may receive no input, and so no auto-save, for hours.
              (when (and niwa--session
                         (niwa-session-save-path niwa--session)
                         (not (niwa-session-persistence-checkpoints-active-p)))
                (niwa-session-persistence-autosave-soon data-buffer)))))))))

(defun niwa-execution-transcript--replace-archived-record
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
                (niwa-transcript-audit-spans
                 (buffer-substring (point-min) (point-max))
                 'execution-archive))))
    (let ((begin (+ (point-min) (plist-get span :start)))
          (end (+ (point-min) (plist-get span :end))))
      (delete-region begin end)
      (unless (niwa-execution-transcript--completion-record-p
               tool-use-id replacement)
        (goto-char begin)
        (insert (niwa--format-hook-audit-record replacement)))
      t)))

(defun niwa-execution-transcript--completion-record-p
    (tool-use-id &optional expected)
  "Return non-nil when the current buffer completed TOOL-USE-ID.
When EXPECTED is non-nil, require the durable record to equal it."
  (cl-some
   (lambda (record)
     (and (equal (plist-get record :tool-use-id) tool-use-id)
          (or (null expected) (equal record expected))))
   (niwa-transcript-audit-records
    (buffer-substring (point-min) (point-max))
    'execution-completion)))

(defun niwa-execution-transcript--nested-tool-use-id-p (tool-use-id)
  "Return non-nil for a ToolCall child TOOL-USE-ID.
The PTC driver assigns child ids by appending a slash and call number to
the parent's tool-use id; unlike the parent, these have no gptel segment."
  (and (stringp tool-use-id)
       (string-match-p "/[0-9]+\\'" tool-use-id)))

(defun niwa-execution-transcript--record-nested-terminal
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
          (unless (niwa-execution-transcript--completion-record-p
                   tool-use-id record)
            (let ((persisted-p
                   (condition-case err
                       (niwa-execution-transcript--persist-terminal-record
                        record)
                     (error
                      (niwa-diagnostic-warning
                       (niwa-diagnostic-scope (or (plist-get event :session) data-buffer))
                       'niwa
                       (format "Could not publish nested execution %s: %s"
                               tool-use-id (error-message-string err))
                       :warning)
                      nil))))
              (save-excursion
                (goto-char (point-max))
                (insert (niwa--format-hook-audit-record record)))
              (set-buffer-modified-p (or modified-p (not persisted-p))))))))))

(defun niwa-execution-transcript--record-archived-terminal
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
               (durable
                (and path
                     (niwa-session-codec-portable-authority-p
                      (or niwa--session
                          (error "Transcript has no session")))))
               (replacement
                (list :type 'execution-completion
                      :tool-use-id tool-use-id
                      :owner (plist-get event :owner)
                      :render-data render-data))
               (marker-table niwa-execution-transcript--archived-rows))
          (when durable
            (let ((session niwa--session)
                  (coding (or buffer-file-coding-system 'utf-8-unix)))
              (with-temp-buffer
                (setq buffer-file-coding-system coding)
                (insert
                 (decode-coding-string
                  (niwa-session-artifacts-read-artifact
                   session
                   (file-relative-name
                    path (niwa-session-save-path session)))
                  coding))
                (let ((org-agenda-file-menu-enabled nil))
                  (niwa--transcript-org-mode))
                (niwa-transcript-restore-properties)
                (unless
                    (or
                     (niwa-execution-transcript--replace-archived-record
                      tool-use-id replacement)
                     (niwa-execution-transcript--completion-record-p
                      tool-use-id replacement))
                  (error "Persisted execution record missing: %s" tool-use-id))
                (niwa-session-artifacts-stabilize-gptel-bounds)
                (niwa-session-artifacts-publish-transcript-state
                 session (niwa-session-root-buffer session) path
                 (buffer-substring-no-properties (point-min) (point-max))
                 ;; Only enrich the committed archive; do not checkpoint
                 ;; unsaved live fields or retire pending consumption.
                 nil coding)))
            ;; The atomic rename changed the visited file.  Refresh the
            ;; buffer's baseline before applying the same replacement in
            ;; memory, otherwise Emacs may report a spurious file-supersession
            ;; conflict.
            (set-visited-file-modtime))
          (save-excursion
            (unless
                (or (niwa-execution-transcript--replace-archived-record
                     tool-use-id replacement)
                    (niwa-execution-transcript--completion-record-p
                     tool-use-id replacement))
              (error "Archived execution record missing: %s" tool-use-id)))
          (when path
            (niwa-session-artifacts-stabilize-gptel-bounds))
          ;; Without a durable workspace only an ordinary save keeps the row.
          (set-buffer-modified-p (or modified-p (and path (not durable))))
          (when (hash-table-p marker-table)
            (remhash tool-use-id marker-table)))))))

(defun niwa-execution-transcript-prepare-archive
    (data-buffer tool-use-ids)
  "Return a compaction plan for TOOL-USE-IDS removed from DATA-BUFFER.
Inspect archived records once per call, only when a live row is missing."
  (niwa-execution-transcript-retry-pending-terminals data-buffer)
  (let (archived live completed)
    (dolist (tool-use-id tool-use-ids)
      (when-let* ((render-data
                   (or (niwa-tool-render-data-for-tool
                        data-buffer tool-use-id)
                       (when (buffer-live-p data-buffer)
                         (unless archived
                           (setq archived (make-hash-table :test #'equal))
                           (with-current-buffer data-buffer
                             (save-restriction
                               (widen)
                               (dolist (record
                                        (niwa-transcript-audit-records
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
        ;; Public execution facts exist even without a view.  A renderer's
        ;; progress flag is not the authority for command completion.
        (if (memq (plist-get render-data :state) '(queued running stopping))
            (push (cons tool-use-id render-data) live)
          (push (cons tool-use-id render-data) completed))))
    (list :live (nreverse live) :completed (nreverse completed))))

(defun niwa-execution-transcript-archive-text (plan)
  "Return durable hidden transcript records for archive PLAN."
  (concat
   (mapconcat
    (lambda (entry)
      (niwa--format-hook-audit-record
       (list :type 'execution-archive
             :tool-use-id (car entry)
             :render-data (cdr entry))))
    (plist-get plan :live)
    "")
   (mapconcat
    (lambda (entry)
      (niwa--format-hook-audit-record
       (list :type 'execution-completion
             :tool-use-id (car entry)
             :render-data (cdr entry))))
    (plist-get plan :completed)
    "")))

(defun niwa-execution-transcript-commit-archive
    (data-buffer plan)
  "Commit execution row archive PLAN after DATA-BUFFER compaction succeeds."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when-let* ((live (plist-get plan :live)))
        (unless (hash-table-p niwa-execution-transcript--archived-rows)
          (setq-local niwa-execution-transcript--archived-rows
                      (make-hash-table :test #'equal)))
        (dolist (entry live)
          (puthash (car entry) t
                   niwa-execution-transcript--archived-rows))))))

(defun niwa-execution-transcript--archived-row-p
    (data-buffer tool-use-id)
  "Return non-nil when DATA-BUFFER archived TOOL-USE-ID."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (and (hash-table-p niwa-execution-transcript--archived-rows)
           (gethash tool-use-id
                    niwa-execution-transcript--archived-rows)))))

(defun niwa-execution-transcript--pending-table (data-buffer)
  "Return DATA-BUFFER's pending terminal table, creating it if needed."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (unless (hash-table-p niwa-execution-transcript--pending-terminals)
        (setq-local niwa-execution-transcript--pending-terminals
                    (make-hash-table :test #'equal)))
      niwa-execution-transcript--pending-terminals)))

(defun niwa-execution-transcript-pending-render-data
    (data-buffer tool-use-id)
  "Return unsettled or retained completion data for TOOL-USE-ID in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (or (and (hash-table-p niwa-execution-transcript--pending-terminals)
               (copy-tree
                (plist-get
                 (gethash tool-use-id
                          niwa-execution-transcript--pending-terminals)
                 :render-data)))
          (copy-tree
           (gethash tool-use-id
                    (niwa-execution-transcript--completion-index)))))))

(defun niwa-execution-transcript--completion-index ()
  "Return the current buffer's newest completion render data by tool-use id.
A view asks once per Bash row on every streamed update; parsing the whole
transcript for each question made long turns quadratic.  Rebuild only
after the text changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick (car niwa-execution-transcript--completion-index))
      (let ((table (make-hash-table :test #'equal)))
        (save-restriction
          (widen)
          ;; Later records replace earlier ones: the newest completion wins.
          (dolist (record (niwa-transcript-audit-records
                           (buffer-substring (point-min) (point-max))
                           'execution-completion))
            (puthash (plist-get record :tool-use-id)
                     (plist-get record :render-data) table)))
        (setq niwa-execution-transcript--completion-index
              (cons tick table))))
    (cdr niwa-execution-transcript--completion-index)))

(defun niwa-execution-transcript-store-pending-terminal
    (data-buffer event render-data)
  "Retain EVENT's RENDER-DATA until its durable row is ready."
  (when-let* ((table
               (niwa-execution-transcript--pending-table data-buffer)))
    (let ((tool-use-id (plist-get event :tool-use-id)))
      (puthash tool-use-id
               (list :event event :render-data render-data)
               table))))

(defun niwa-execution-transcript--update-archived-render-data
    (data-buffer tool-use-id updates)
  "Merge UPDATES into TOOL-USE-ID's archived row from DATA-BUFFER.
Search only this receiving transcript's history, newest first.  Summary
updates can outlive their Agent row after compaction; never discard them or
substitute the root transcript for an agent's history.  Return non-nil only
after publishing the updated archive."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when-let* ((session niwa--session)
                  (save-path (niwa-session-save-path session)))
        (catch 'updated
          (let ((cached (and niwa-execution-transcript--archived-update-cache
                             (gethash tool-use-id
                                      niwa-execution-transcript--archived-update-cache))))
            (when (and cached (equal updates (nth 2 cached))
                       (nth 1 cached)
                       (equal (nth 1 cached)
                              (niwa-session-artifacts-transcript-segment-identity
                               session (car cached))))
              (throw 'updated t)))
          (dolist (descriptor
                   (reverse
                    (niwa-session-artifacts-transcript-segments
                     session data-buffer niwa--agent-invocation)))
            (when (and (eq (plist-get descriptor :status) 'readable)
                       (not (plist-get descriptor :current-p)))
              (let ((archive
                     (niwa-session-artifacts-read-transcript-segment
                      session descriptor)))
                (unwind-protect
                    (when (niwa-tool-render-data-update
                           archive tool-use-id updates)
                      (with-current-buffer archive
                        (let ((inhibit-read-only t))
                          (niwa-session-artifacts-stabilize-gptel-bounds))
                        (let ((path
                               (expand-file-name
                                (or (plist-get descriptor :logical)
                                    (format "segment-%04d.chat.org"
                                            (plist-get descriptor :number)))
                                save-path))
                              (coding (or buffer-file-coding-system 'utf-8-unix)))
                          (niwa-session-artifacts-publish-transcript-state
                           session (niwa-session-root-buffer session) path
                           (buffer-substring-no-properties (point-min) (point-max))
                           nil coding)))
                      (unless niwa-execution-transcript--archived-update-cache
                        (setq niwa-execution-transcript--archived-update-cache
                              (make-hash-table :test #'equal)))
                      (puthash tool-use-id
                               (list descriptor
                                     (niwa-session-artifacts-transcript-segment-identity
                                      session descriptor)
                                     (copy-tree updates))
                               niwa-execution-transcript--archived-update-cache)
                      (throw 'updated t))
                  (when (buffer-live-p archive)
                    (with-current-buffer archive (set-buffer-modified-p nil))
                    (kill-buffer archive)))))))))))

(defun niwa-execution-transcript-retry-pending-terminals (data-buffer)
  "Persist pending tool updates whose rows exist in DATA-BUFFER."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (when (hash-table-p niwa-execution-transcript--pending-terminals)
        (let (settled)
          (maphash
           (lambda (tool-use-id pending)
             (let ((event (plist-get pending :event))
                   (render-data (plist-get pending :render-data)))
               ;; One unsettled entry must not abort the sweep, and this
               ;; runs before the caller releases its turn.
               (condition-case err
                   (cond
                    ((niwa-tool-render-data-update
                      data-buffer tool-use-id render-data)
                     (push tool-use-id settled))
                    ((and (not (eq (plist-get event :type) 'terminal))
                          (niwa-execution-transcript--update-archived-render-data
                           data-buffer tool-use-id render-data))
                     (push tool-use-id settled))
                    ((and (eq (plist-get event :type) 'terminal)
                          (niwa-execution-transcript--archived-row-p
                           data-buffer tool-use-id))
                     (niwa-execution-transcript--record-archived-terminal
                      data-buffer event render-data)
                     (push tool-use-id settled))
                    ((and (eq (plist-get event :type) 'terminal)
                          (niwa-execution-transcript--nested-tool-use-id-p
                           tool-use-id))
                     (niwa-execution-transcript--record-nested-terminal
                      data-buffer event render-data)
                     (push tool-use-id settled)))
                 (error
                  (niwa-diagnostic-warning
                   (niwa-diagnostic-scope (or (plist-get event :session) data-buffer))
                   'niwa
                   (format "Could not settle pending execution %s: %s"
                           tool-use-id (error-message-string err))
                   :warning)))))
           niwa-execution-transcript--pending-terminals)
          (dolist (tool-use-id settled)
            (remhash tool-use-id
                     niwa-execution-transcript--pending-terminals)))))))

(defun niwa-execution-transcript-retry-terminals (&rest _args)
  "Persist pending tool updates in the current data buffer."
  (niwa-execution-transcript-retry-pending-terminals (current-buffer))
  nil)

(defun niwa-execution-transcript-handle-event (event)
  "Persist terminal Bash EVENT in its authoritative transcript row.
Always return nil; only the mailbox sink may acknowledge durable delivery."
  (let ((type (plist-get event :type))
        (tool-use-id (plist-get event :tool-use-id))
        (data-buffer (plist-get event :data-buffer)))
    (when (and (eq type 'terminal) tool-use-id)
      (let ((render-data
             (niwa-execution-transcript-terminal-render-data event)))
        (cond
         ((niwa-tool-render-data-update
           data-buffer tool-use-id render-data)
          (when-let* ((table
                       (niwa-execution-transcript--pending-table
                        data-buffer)))
            (remhash tool-use-id table)))
         ((niwa-execution-transcript--archived-row-p
           data-buffer tool-use-id)
          (condition-case err
              (niwa-execution-transcript--record-archived-terminal
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
                                     (niwa-execution-transcript--replace-archived-record
                                      tool-use-id replacement)
                                   (error nil))
                                 (niwa-execution-transcript--completion-record-p
                                  tool-use-id replacement))
                       (save-excursion
                         (goto-char (point-max))
                         (insert (niwa--format-hook-audit-record replacement))))
                     (set-buffer-modified-p t)))))
             (niwa-execution-transcript-store-pending-terminal
              data-buffer event render-data)
             (niwa-diagnostic-warning
              (niwa-diagnostic-scope (or (plist-get event :session) data-buffer))
              'niwa
              (format "Could not persist archived execution %s: %s"
                      tool-use-id (error-message-string err))
              :warning))))
         ((niwa-execution-transcript--nested-tool-use-id-p tool-use-id)
          (condition-case err
              (niwa-execution-transcript--record-nested-terminal
               data-buffer event render-data)
            (error
             (niwa-execution-transcript-store-pending-terminal
              data-buffer event render-data)
             (niwa-diagnostic-warning
              (niwa-diagnostic-scope (or (plist-get event :session) data-buffer))
              'niwa
              (format "Could not retain nested execution %s: %s"
                      tool-use-id (error-message-string err))
              :warning))))
         (t
          (niwa-execution-transcript-store-pending-terminal
           data-buffer event render-data)))
        (condition-case err
            (niwa-execution-transcript--record-breadcrumb event)
          (error
           (niwa-diagnostic-warning
            (niwa-diagnostic-scope (or (plist-get event :session) data-buffer))
            'niwa
            (format "Could not persist execution completion %s: %s"
                    tool-use-id (error-message-string err))
            :warning))))))
  nil)

(provide 'niwa-execution-transcript)

;;; niwa-execution-transcript.el ends here
