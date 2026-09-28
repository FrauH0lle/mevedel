;;; mevedel-collaboration-projection.el --- canonical collaboration projection -*- lexical-binding: t; -*-

;;; Commentary:

;; Reconstructs the allowlisted collaboration records from the authoritative
;; data buffer and tracks live tool records until canonical settlement.

;;; Code:

;; `mevedel-agent-control'
(declare-function mevedel-agent-record-conversation-buffer
                  "mevedel-agent-control" (record))

;; `mevedel-collaboration-artifact-projection'
(declare-function mevedel-collaboration--artifact-fields
                  "mevedel-collaboration-artifact-projection" (render-data))

;; `mevedel-execution'
(declare-function mevedel-execution-list-user
                  "mevedel-execution" (session))

;; `mevedel-execution-transcript'
(declare-function mevedel-execution-transcript-pending-render-data
                  "mevedel-execution-transcript" (data-buffer tool-use-id))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-read-transcript-segment
                  "mevedel-session-artifacts" (session descriptor))
(declare-function mevedel-session-artifacts-transcript-segments
                  "mevedel-session-artifacts" (session live-buffer))
(autoload 'mevedel-session-artifacts-read-transcript-segment "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-transcript-segments "mevedel-session-artifacts")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-lease-owned-p
                  "mevedel-session-durability" (session))

;; `mevedel-session-publication'
(declare-function mevedel-session-publication-read
                  "mevedel-session-publication" (session-dir &optional head names))
(declare-function mevedel-session-publication-uncommitted-artifact
                  "mevedel-session-publication" (session logical))

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-for-call "mevedel-tool-registry" (name))

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-direct-call
                  "mevedel-tool-render-data" (name data))
(declare-function mevedel-tool-render-data-extract
                  "mevedel-tool-render-data"
                  (result-string &optional session expected-tool-use-id allow-payload-tool-use-id))

;; `mevedel-transcript'
(declare-function mevedel-transcript--mailbox-any-block-at-point
                  "mevedel-transcript" (limit))
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (text))
(declare-function mevedel-transcript-audit-buffer-spans
                  "mevedel-transcript-audit" (&optional type start end))
(declare-function mevedel-transcript-audit-guest-prompts
                  "mevedel-transcript-audit" ())
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))

;; `mevedel-utilities'
(declare-function mevedel--trim-tool-result "mevedel-utilities" (text))

;; `mevedel-view-audit'
(declare-function mevedel-view-audit-mailbox-breadcrumb
                  "mevedel-view-audit" (text sender))
(autoload 'mevedel-view-audit-mailbox-breadcrumb "mevedel-view-audit")

;; `mevedel-view-render'
(declare-function mevedel-view--generic-tool-rendering
                  "mevedel-view-render" (name args result &optional collapsed-only render-data))
(declare-function mevedel-view--invoke-renderer
                  "mevedel-view-render" (tool render-data args result))
(declare-function mevedel-view--rendering-status
                  "mevedel-view-render" (rendering &optional render-data))
(declare-function mevedel-view--sandbox-summary-line
                  "mevedel-view-render" (summary))
(declare-function mevedel-view--tool-call-parse
                  "mevedel-view-render" (data-buf start end &optional raw))
(declare-function mevedel-view--user-turn-text
                  "mevedel-view-render" (segments data-buf))
(declare-function mevedel-view--visible-response-text
                  "mevedel-view-render" (text))

(require 'mevedel-collaboration-artifact-projection)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)
(require 'mevedel-view-render)

(defvar-local mevedel-collaboration--execution-prior-cache nil
  "Derived archive identity cache for this live transcript only.
The source descriptor signature is checked before each use; this is never
used to authorize a guest result fetch or persisted as execution state.")

(defconst mevedel-collaboration--protocol-version 3)
(defconst mevedel-collaboration--max-record-text-bytes
  (/ (- (* 1 1024 1024) 4096) 6)
  "Raw text bytes one projected record carries.
The wire bound applies to the encoded frame, where escaping expands one byte
at most six-fold, so bounding the raw text keeps every record sendable
without measuring an encoding that would only be rejected later.  The
reserve covers the record's own keys and the frame around it, which the
six-fold worst case alone does not.")
(defconst mevedel-collaboration--max-tool-result-bytes 50000)
(defconst mevedel-collaboration--tool-error-regexp
  "\\(?:Error:\\|blocked by\\|<tool_call_error>\\)"
  "Result text marking a settled tool call as failed.")


;;
;;; Canonical records


(defun mevedel-collaboration--truncate-bytes (string limit)
  "Return STRING no longer than LIMIT bytes, with a truncation marker."
  (if (<= (string-bytes string) limit)
      string
    (let ((marker "\n[truncated]")
          (max (max 0 (- limit (string-bytes "\n[truncated]"))))
          (low 0)
          (high (length string)))
      ;; Search by character index so a multibyte character is never split.
      ;; Each candidate is measured in encoded bytes, keeping large rejected
      ;; HTTP and transcript payloads linearithmic instead of quadratic.
      (while (< low high)
        (let ((mid (ceiling (+ low high) 2)))
          (if (<= (string-bytes (substring string 0 mid)) max)
              (setq low mid)
            (setq high (1- mid)))))
      (concat (substring string 0 low) marker))))

(defun mevedel-collaboration--clean-response (text)
  "Return the visible, text-only form of assistant TEXT.
Audit blocks are stripped defensively: a hidden record swallowed into a
response span must never reach the wire as visible text."
  (let ((visible (mevedel-view--visible-response-text
                  (mevedel--strip-hook-audit-blocks text))))
    (unless (stringp visible)
      (error "Canonical response projection failed"))
    (string-trim visible)))

(defun mevedel-collaboration--clean-user (segment data-buffer)
  "Return visible user text for SEGMENT from DATA-BUFFER."
  (let ((visible (mevedel-view--user-turn-text (list segment) data-buffer)))
    (unless (stringp visible)
      (error "Canonical user projection failed"))
    (string-trim visible)))

(defun mevedel-collaboration--record (id kind &rest fields)
  "Build a projection record with ID, KIND, and FIELDS."
  (append (list :id id :kind kind) fields))

(defun mevedel-collaboration--stable-record-id (kind text &optional occurrence)
  "Return a content identity for KIND and TEXT.

Transcript segments do not carry durable positions for user or response
turns.  A digest gives newly opened rooms an ID independent of buffer
positions; OCCURRENCE distinguishes repeated identical records.  Publication
reconciliation below keeps the identity stable while an assistant response
grows in the existing room."
  (format "%s-%s" kind
          (substring (secure-hash
                      'sha256
                      (format "%s\0%s\0%d" kind text (or occurrence 0)))
                     0 24)))

(defun mevedel-collaboration--reuse-record-ids (old new)
  "Reuse room-local IDs from OLD for the ordered records in NEW.

The canonical transcript parser exposes no stable hook identity for a growing
response.  Matching the ordered role streams lets a replacement retain its
room-local ID without using a numeric buffer position or a guessed tool-call
key."
  (let ((by-kind (make-hash-table :test #'eq)))
    (dolist (record old)
      (let ((kind (plist-get record :kind)))
        (puthash kind
                 (append (gethash kind by-kind)
                         (unless (plist-get record :pending)
                           (list record)))
                 by-kind)))
    (mapcar
     (lambda (record)
       (if (plist-get record :identity-fixed)
           record
         (let* ((kind (plist-get record :kind))
                (candidates (gethash kind by-kind))
                (previous (car candidates)))
           (when previous
             (puthash kind (cdr candidates) by-kind)
             (setq record (plist-put record :id
                                     (plist-get previous :id))))
           record)))
     new)))

(defun mevedel-collaboration--record-without-revision (record)
  "Return RECORD without its transport-local revision."
  (let ((copy (copy-sequence record)))
    (setq copy (plist-put copy :revision nil))
    copy))

(defun mevedel-collaboration--json-record (record)
  "Return JSON-safe alist representation of RECORD.
An artifact record's `:artifact-path' stays host-side: guests address
an artifact only by its record id, never by a filesystem path."
  (let (out)
    (dolist (key '(:id :kind :revision :text :name :status :summary :result
                       :truncated :guest :directive :detail :diff
                       :artifact :size :missing :presentation :shared
                       :execution))
      (when (plist-member record key)
        (push (cons (substring (symbol-name key) 1)
                    (plist-get record key))
              out)))
    (nreverse out)))


;;
;;; Canonical projection

(defun mevedel-collaboration--tool-detail (args)
  "Return a bounded one-line viewer summary of tool ARGS, or nil.
The detail is the tool's primary operand -- a command, path, pattern, or
query -- so a collapsed tool row says what the call did."
  (let ((value (and (listp args)
                    (cl-loop for key in '(:command :file_path :pattern
                                          :query :url :path :name)
                             for found = (plist-get args key)
                             when (stringp found) return found))))
    (when value
      (mevedel-collaboration--truncate-bytes
       (car (split-string value "\n")) 200))))

(defun mevedel-collaboration--tool-extras (name args)
  "Return the optional :detail and :diff record fields for NAME with ARGS.
Shared by the canonical tool record and the live pending record so both
carry the same operand summary and, for ApplyPatch, the authored patch."
  (append
   (when-let* ((detail (mevedel-collaboration--tool-detail args)))
     (list :detail detail))
   (when-let* (((equal (format "%s" name) "ApplyPatch"))
              (patch (plist-get args :patch))
              ((stringp patch)))
     (list :diff (mevedel-collaboration--truncate-bytes
                  patch mevedel-collaboration--max-tool-result-bytes)))))

(defun mevedel-collaboration--routine-poll-p (name args data status)
  "Return non-nil for a successful, empty-input WriteStdin observation.
Do not discard failed control operations, even when the requested input was
empty.  A process exiting unsuccessfully is not a failed control operation."
  (and (equal name "WriteStdin")
       (equal (or (plist-get args :chars) "") "")
       (or (eq (plist-get data :control-succeeded-p) t)
           (and (not (plist-member data :control-succeeded-p))
                (not (memq status '(error denied cancelled blocked failed)))
                (not (memq (plist-get data :status)
                           '(error denied cancelled blocked failed)))))))

(defun mevedel-collaboration--execution-output (data)
  "Return retained whole execution output from DATA when available.
An empty string is meaningful: a completed command may produce no output."
  (when (and (plist-member data :execution-output)
             (stringp (plist-get data :execution-output)))
    (plist-get data :execution-output)))

(defun mevedel-collaboration--live-bash-data (parsed)
  "Return PARSED with the owner's live Bash tail when its process is running.
The snapshot is bounded by the execution owner; terminal render data replaces
it once the command settles.  No polling row becomes a second output owner."
  (let* ((data (plist-get parsed :render-data))
         (id (plist-get data :execution-id))
         (live (and id mevedel--session
                    (fboundp 'mevedel-execution-list-user)
                    (cl-find id (mevedel-execution-list-user mevedel--session)
                             :key (lambda (record)
                                    (plist-get record :execution-id))
                             :test #'equal))))
    (if (and live (eq (plist-get live :state) 'running))
        (let ((copy (copy-sequence parsed)))
          (plist-put copy :render-data
                     (plist-put
                      (plist-put (copy-sequence data) :status 'running)
                      :execution-output
                      (or (plist-get live :output-tail) ""))))
      parsed)))

(defun mevedel-collaboration--missing-bash-data (data completions)
  "Mark archived running DATA unknown if later evidence has a gap.
COMPLETIONS records a missing later segment under :gap.  Do not infer a
terminal outcome while the execution owner still reports a running process."
  (if (and (hash-table-p completions)
           (gethash :gap completions)
           (eq (plist-get data :state) 'running)
           (not (and (plist-get data :execution-id)
                     mevedel--session
                     (fboundp 'mevedel-execution-list-user)
                     (cl-find-if
                      (lambda (execution)
                        (and (equal (plist-get execution :execution-id)
                                    (plist-get data :execution-id))
                             (eq (plist-get execution :state) 'running)))
                      (mevedel-execution-list-user mevedel--session)))))
      (plist-put (plist-put (copy-sequence data) :state 'unknown)
                 :status 'warning)
    data))

(defun mevedel-collaboration--direct-facts (data direct)
  "Return DIRECT's authoritative child facts rather than a stale wrapper status.
The direct-call helper copies the child's result status into render data; a
terminal Bash event or failed control operation may have newer facts."
  (let ((child-facts (plist-get (car (plist-get data :calls)) :render-data)))
    (if (or (memq (plist-get child-facts :status)
                  '(error denied cancelled blocked failed))
            (and (equal (plist-get direct :tool) "Bash")
                 (memq (plist-get child-facts :state)
                       '(completed interrupted lost unknown))))
        child-facts
      (plist-get direct :render-data))))

(defun mevedel-collaboration--reconcile-ptc-children
    (data-buffer parsed &optional completion-buffer completions)
  "Return PARSED with pending terminal facts for its nested Bash children.
PTC children use their own tool-use ids, but have no separate gptel segment;
their completion may therefore be retained in the execution transcript's
pending table rather than patched into the outer ToolCall render data.
COMPLETION-BUFFER and COMPLETIONS supply later facts for an archived segment.
Keep the raw parsed model value and transcript metadata unchanged."
  (cl-labels
      ((reconcile (data depth)
         (if (or (> depth 16) (not (eq (plist-get data :kind) 'ptc)))
             data
           (let* ((changed nil)
                  (children
                  (mapcar
                   (lambda (child)
                     (let* ((prior (plist-get child :render-data))
                            (nested (and (equal (plist-get child :tool) "ToolCall")
                                         (reconcile prior (1+ depth))))
                            (terminal
                             (and (equal (plist-get child :tool) "Bash")
                                  (stringp (plist-get child :id))
                                  (or (and (hash-table-p completions)
                                           (gethash (plist-get child :id) completions))
                                      (and (buffer-live-p completion-buffer)
                                           (mevedel-execution-transcript-pending-render-data
                                            completion-buffer (plist-get child :id)))
                                      (mevedel-execution-transcript-pending-render-data
                                       data-buffer (plist-get child :id)))))
                            (missing (and (equal (plist-get child :tool) "Bash")
                                          (not terminal)
                                          (mevedel-collaboration--missing-bash-data
                                           prior completions))))
                       (if (or terminal (and missing (not (eq prior missing)))
                               (and nested (not (eq prior nested))))
                           (progn
                             (setq changed t)
                             (let ((copy (plist-put
                                          (copy-sequence child) :render-data
                                          (or terminal missing nested))))
                               (if (or terminal (and missing (not (eq prior missing))))
                                   (plist-put copy :status
                                              (plist-get (or terminal missing) :status))
                                 copy)))
                         child)))
                   (plist-get data :calls))))
             (if changed
                 (plist-put (copy-sequence data) :calls children)
               data)))))
    (let* ((data (plist-get parsed :render-data))
           (updated (and (equal (plist-get parsed :name) "ToolCall")
                         (reconcile data 0))))
      (if (and updated (not (eq data updated)))
          (plist-put (copy-sequence parsed) :render-data updated)
        parsed))))


(defun mevedel-collaboration-tool-presentation (parsed)
  "Return a bounded browser presentation tree for PARSED.
Execution identity and provider results remain unchanged.  Only display fields
are exported, with one shared text/structure budget across the entire tree."
  (let ((remaining mevedel-collaboration--max-tool-result-bytes)
        truncated)
    (cl-labels
        ((text (value &optional limit)
           (when (stringp value)
             (let* ((size (min remaining (or limit remaining)))
                    (bounded (if (< size 16) ""
                               (mevedel-collaboration--truncate-bytes value size))))
               (when (< (length bounded) (length value)) (setq truncated t))
               (setq remaining (- remaining (string-bytes bounded)))
               bounded)))
         (node (name args result data id depth &optional batch status)
           (if (or (mevedel-collaboration--routine-poll-p name args data status)
                   (when-let* ((direct
                                (mevedel-tool-render-data-direct-call name data)))
                     (mevedel-collaboration--routine-poll-p
                      (plist-get direct :tool) (plist-get direct :args)
                      (mevedel-collaboration--direct-facts data direct)
                      (plist-get direct :status))))
               nil
             (if (or (< remaining 512) (> depth 16))
               (progn (setq truncated t) nil)
             ;; Reserve structural JSON keys per node as well as its strings.
             (setq remaining (- remaining 256))
             (let* ((direct (mevedel-tool-render-data-direct-call name data))
                    (name (if direct (plist-get direct :tool) name))
                    (args (if direct (plist-get direct :args) args))
                    (data (if direct
                              (mevedel-collaboration--direct-facts data direct)
                            data))
                    (data (if (equal name "Bash")
                              (plist-get
                               (mevedel-collaboration--live-bash-data
                                (list :render-data data))
                               :render-data)
                            data))
                    (result (or (and (equal name "Bash")
                                     (mevedel-collaboration--execution-output data))
                                (and direct (equal name "Bash")
                                     (eq (plist-get data :state) 'unknown)
                                     (plist-get direct :result))
                                result))
                    (tool (and (stringp name) (mevedel-tool-for-call name)))
                    (rendering
                     (or (and tool (mevedel-view--invoke-renderer tool data args result))
                         (mevedel-view--generic-tool-rendering name args result nil data)))
                    (sandbox-line
                     (when-let* ((summary (or (plist-get rendering :sandbox-summary)
                                              (plist-get data :sandbox-summary)))
                                 (line (mevedel-view--sandbox-summary-line summary)))
                       (mevedel-collaboration--truncate-bytes
                        (string-trim (substring-no-properties line)) 280)))
                    (header (plist-get rendering :header))
                    (header (if sandbox-line
                                (concat (mevedel-collaboration--truncate-bytes
                                         header 700)
                                        " · " sandbox-line)
                              header))
                    (status
                     (pcase (or (and (memq (plist-get data :status) '(denied cancelled))
                                     (plist-get data :status))
                                (mevedel-view--rendering-status rendering data))
                       ('error "failed") ('denied "denied") ('cancelled "cancelled")
                       ('running "running") ('warning "warning") (_ "completed")))
                    (row (list :id (text id 200) :name (text name 200)
                               :detail (text (mevedel-collaboration--tool-detail args) 200)
                               :header (text header 1000)
                               :status status
                               :collapsed (if (plist-get rendering :initially-collapsed-p)
                                              t :json-false)
                               :format (pcase (plist-get rendering :body-mode)
                                         ((or 'markdown-mode 'gfm-mode 'org-mode) "markdown")
                                         ('diff-mode "diff") (_ "text"))
                               :body (text (plist-get rendering :body))))
                    children attachments)
               (when batch (setq row (plist-put row :batch (text (format "%s" batch) 200))))
               (when-let* ((diff (plist-get (mevedel-collaboration--tool-extras name args) :diff)))
                 (setq row (plist-put row :diff (text diff))))
               (cl-loop for attachment in (plist-get data :attachments)
                        while (>= remaining 512) do
                        (let ((body (cdr (assoc attachment (plist-get data :attachment-bodies)))))
                          (push (node "Skill dependency" (list :name attachment)
                                      (or body "Delivered dependency body unavailable in this transcript.")
                                      (list :status 'success)
                                      (concat "attachment:" attachment) (1+ depth))
                                attachments)
                          (when (car attachments)
                            (setf (plist-get (car attachments) :format) "markdown"))))
               (cl-loop for child in (plist-get rendering :child-calls)
                        while (>= remaining 512) do
                        (push (node (plist-get child :tool) (plist-get child :args)
                                    (plist-get child :result)
                                    (let ((facts (copy-sequence
                                                  (plist-get child :render-data))))
                                      (if (and (equal (plist-get child :tool) "Bash")
                                               (memq (plist-get facts :state)
                                               '(completed interrupted lost unknown)))
                                          facts
                                        (plist-put facts :status
                                                   (plist-get child :status))))
                                    (plist-get child :id) (1+ depth)
                                    (plist-get child :batch) (plist-get child :status))
                              children))
               (when (or (< (length attachments) (length (plist-get data :attachments)))
                         (< (length children) (length (plist-get rendering :child-calls))))
                 (setq truncated t))
               (when attachments (setq row (plist-put row :attachments (vconcat (delq nil (nreverse attachments))))))
               (when children (setq row (plist-put row :children (vconcat (delq nil (nreverse children))))))
               row)))))
      (condition-case nil
          (let ((row (node (plist-get parsed :name) (plist-get parsed :args)
                           (plist-get parsed :result) (plist-get parsed :render-data)
                           "root" 0)))
            (when truncated (setq row (plist-put row :truncated t)))
            row)
        (error nil)))))

(defun mevedel-collaboration--tool-record (parsed raw &optional occurrence)
  "Return an allowlisted tool record from PARSED and transcript RAW text."
  (let* ((name (plist-get parsed :name))
         (result (or (plist-get parsed :result) ""))
         (tool-use-id (plist-get parsed :tool-use-id)))
    (unless (stringp name)
      (error "Canonical tool projection failed"))
    (unless (or (mevedel-collaboration--routine-poll-p
                 name (plist-get parsed :args) (plist-get parsed :render-data)
                 (or (plist-get (plist-get parsed :render-data) :status)
                     (and (string-match-p mevedel-collaboration--tool-error-regexp
                                          result)
                          'error)))
                (when-let* ((direct (mevedel-tool-render-data-direct-call
                                     name (plist-get parsed :render-data))))
                  (mevedel-collaboration--routine-poll-p
                   (plist-get direct :tool) (plist-get direct :args)
                   (mevedel-collaboration--direct-facts
                    (plist-get parsed :render-data) direct)
                   (plist-get direct :status))))
      (let* ((id (if tool-use-id
                   (format "tool-%s" tool-use-id)
                 (mevedel-collaboration--stable-record-id
                  "tool" raw occurrence)))
           (result (mevedel--trim-tool-result
                    (if (stringp result) result "")))
           (presentation (mevedel-collaboration-tool-presentation parsed))
           (status (or (plist-get presentation :status)
                       (if (string-match-p mevedel-collaboration--tool-error-regexp result)
                           "failed" "completed")))
           (result (mevedel-collaboration--truncate-bytes
                    (or (and (equal name "Bash")
                             (mevedel-collaboration--execution-output
                              (plist-get parsed :render-data)))
                        result)
                    mevedel-collaboration--max-tool-result-bytes))
           (truncated (string-suffix-p "\n[truncated]" result)))
      (apply #'mevedel-collaboration--record
             id "tool"
             :revision 0
             :name (format "%s" name)
             :status status
             :summary (format "%s" name)
             :result result
             :truncated (and truncated t)
             :identity-fixed (and tool-use-id t)
             :presentation presentation
             (mevedel-collaboration--tool-extras
              name (plist-get parsed :args)))))))

(defun mevedel-collaboration--tool-segment-records
    (data-buffer segment &optional occurrence completion-buffer render-end completions)
  "Return canonical records for tool SEGMENT in DATA-BUFFER.
A settled ApplyPatch may produce several artifact cards.  A patch touching
only artifact destinations reuses its ordinary tool record as the first card;
a mixed patch retains the ordinary row and adds child cards.
COMPLETION-BUFFER may contain later completion evidence for archived Bash.
RENDER-END includes a following, separately classified hidden metadata block."
  (with-current-buffer data-buffer
    (let* ((start (cadr segment))
           (end (caddr segment))
           (parsed (mevedel-view--tool-call-parse
                    data-buffer start (or render-end end)))
           (terminal (and (equal (plist-get parsed :name) "Bash")
                          (plist-get parsed :tool-use-id)
                          (or (and (hash-table-p completions)
                                   (gethash (plist-get parsed :tool-use-id)
                                            completions))
                              (and (buffer-live-p completion-buffer)
                                   (mevedel-execution-transcript-pending-render-data
                                    completion-buffer (plist-get parsed :tool-use-id)))
                              (mevedel-execution-transcript-pending-render-data
                               data-buffer (plist-get parsed :tool-use-id)))))
           (parsed (if terminal
                       (plist-put (copy-sequence parsed) :render-data terminal)
                     (if (equal (plist-get parsed :name) "Bash")
                         (let ((live (mevedel-collaboration--live-bash-data parsed)))
                           (plist-put (copy-sequence live) :render-data
                                      (mevedel-collaboration--missing-bash-data
                                       (plist-get live :render-data) completions)))
                       parsed)))
           (parsed (mevedel-collaboration--reconcile-ptc-children
                    data-buffer parsed completion-buffer completions))
           (base (mevedel-collaboration--tool-record
                  parsed (buffer-substring-no-properties start end)
                  occurrence))
           (direct (mevedel-tool-render-data-direct-call
                    (plist-get parsed :name) (plist-get parsed :render-data)))
           (effective-name (if direct (plist-get direct :tool)
                             (plist-get parsed :name)))
           (effective-data (if direct (plist-get direct :render-data)
                             (plist-get parsed :render-data)))
           (files (and (equal effective-name "ApplyPatch")
                       (equal (plist-get base :status) "completed")
                       (mevedel-collaboration--artifact-fields
                        effective-data)))
           (all-files (and (eq (plist-get effective-data :kind)
                               'patch)
                           (plist-get effective-data :files)))
           (pure (and files (= (length files) (length all-files))))
           cards)
      (dolist (fields files)
        (let* ((relative (plist-get fields :artifact))
               (first (null cards))
               (record (if (and pure first)
                           (copy-sequence base)
                         (list :id (format "%s-artifact-%s"
                                           (plist-get base :id)
                                           (substring
                                            (secure-hash 'sha256 relative)
                                            0 12))
                               :kind "tool"
                               :revision 0
                               :name "Artifact"
                               :status "completed"
                               :summary "Artifact"
                               :result ""
                               :truncated nil
                               :identity-fixed t
                               :artifact-child t))))
          (setq record (append record fields))
          (push record cards)))
      (setq cards (nreverse cards))
      (if pure cards (delq nil (cons base cards))))))

(defun mevedel-collaboration--directive-at (ranges position)
  "Return the directive id owning POSITION per directive RANGES, or nil."
  (cl-loop for range in ranges
           when (and (<= (plist-get range :start) position)
                     (< position (plist-get range :end)))
           return (plist-get range :directive-id)))

(defun mevedel-collaboration--directive-ranges ()
  "Return the current buffer's directive turn ranges, or nil.
A malformed audit grammar degrades to untagged records instead of
failing the whole projection."
  (ignore-errors (mevedel-transcript-buffer-directive-ranges t)))

(defun mevedel-collaboration--attribute-guest-prompts (user-starts)
  "Attach guest names to user records per attribution positions.
USER-STARTS is an ordered list of (SEGMENT-START . RECORD).  Each guest
attribution record names the last user turn starting at or before it.
Starts are the stable anchor: segment repair can grow a user turn's end
over the audit blocks that follow it, or reclassify trailing text, but
the turn always begins before its own attribution block."
  (dolist (attribution (mevedel-transcript-audit-guest-prompts))
    (let (owner)
      (dolist (entry user-starts)
        (when (<= (car entry) (car attribution))
          (setq owner (cdr entry))))
      (when owner
        (plist-put owner :guest (plist-get (cdr attribution) :name))
        (when-let* ((shared (plist-get (cdr attribution) :shared)))
          (plist-put owner :shared shared))))))

(defun mevedel-collaboration--execution-ids-in-buffer (buffer ids)
  "Add trusted execution identities in BUFFER to hash table IDS."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (dolist (span (mevedel-transcript-audit-buffer-spans
                     'execution-breadcrumb))
        (when-let* ((record (plist-get span :record))
                    (owner (plist-get record :owner))
                    (id (plist-get record :execution-id))
                    ((stringp owner)) ((stringp id)))
          (puthash (plist-get
                    (mevedel-collaboration--forwarded-execution-record
                     record) :id)
                   t ids)))
      (dolist (segment (mevedel-transcript-segments (point-min) (point-max)))
        (when (eq (car segment) 'mailbox)
          (when-let* ((completion
                       (mevedel-collaboration--forwarded-execution segment))
                      (owner (plist-get completion :owner))
                      (id (plist-get completion :execution-id))
                      ((stringp owner)) ((stringp id)))
            (puthash (plist-get
                      (mevedel-collaboration--forwarded-execution-record
                       completion) :id)
                     t ids))))))
  ids)

(defun mevedel-collaboration--execution-archive-signature (session descriptor)
  "Return source fingerprint for archived DESCRIPTOR in SESSION."
  (let* ((number (plist-get descriptor :number))
         (logical (or (plist-get descriptor :logical)
                      (format "segment-%04d.chat.org" number)))
         (portable (mevedel-session-codec-portable-authority-p session))
         (source (if portable
                     (and (mevedel-session-durability-lease-owned-p session)
                          (mevedel-session-publication-uncommitted-artifact
                           session logical))
                   (plist-get descriptor :path)))
         (publication (and portable
                           (or (mevedel-session-publication session)
                               (mevedel-session-publication-read
                                (mevedel-session-save-path session)))))
         (published (and publication
                         (cdr (assoc logical
                                     (plist-get publication :artifacts)))))
         (attributes (and source (file-attributes source 'string))))
    (list number logical (plist-get descriptor :status)
          (if source
              ;; Do not include atime: reading an archive can change it and
              ;; would immediately invalidate the memo on every publication.
              (list source
                    (and attributes
                         (list (file-attribute-modification-time attributes)
                               (file-attribute-status-change-time attributes)
                               (file-attribute-size attributes)
                               (file-attribute-inode-number attributes))))
            (and published (plist-get published :sha256))))))

(defun mevedel-collaboration--prior-execution-ids
    (session live-buffer &optional target-number)
  "Return execution identities before TARGET-NUMBER in SESSION.
When TARGET-NUMBER is nil, scan readable history preceding LIVE-BUFFER.
Unreadable archives do not suppress later source-backed breadcrumbs."
  (let* ((descriptors
          (when (and session (buffer-live-p live-buffer))
            (cl-loop for descriptor in
                     (mevedel-session-artifacts-transcript-segments
                      session live-buffer)
                     until (or (plist-get descriptor :current-p)
                               (and target-number
                                    (equal target-number
                                           (plist-get descriptor :number))))
                     collect descriptor)))
         (signature (mapcar (lambda (descriptor)
                              (mevedel-collaboration--execution-archive-signature
                               session descriptor))
                            descriptors))
         (cache (and (not target-number) (buffer-live-p live-buffer)
                     (buffer-local-value
                      'mevedel-collaboration--execution-prior-cache live-buffer)))
         (prior (if (and cache (eq session (plist-get cache :session))
                         (equal signature (plist-get cache :signature)))
                    (plist-get cache :ids)
                  (let ((ids (make-hash-table :test #'equal))
                        (read-ok t))
                    (dolist (descriptor descriptors)
                      (when (eq (plist-get descriptor :status) 'readable)
                        (let ((buffer
                               (condition-case nil
                                   (mevedel-session-artifacts-read-transcript-segment
                                    session descriptor)
                                 (error nil))))
                          (if (not (buffer-live-p buffer))
                              (setq read-ok nil)
                            (unwind-protect
                                (mevedel-collaboration--execution-ids-in-buffer
                                 buffer ids)
                              (kill-buffer buffer))))))
                    (when (and read-ok (not target-number))
                      (with-current-buffer live-buffer
                        (setq mevedel-collaboration--execution-prior-cache
                              (list :session session :signature signature
                                    :ids ids))))
                    ids))))
    prior))

(defun mevedel-collaboration--session-bash-completions
    (session live-buffer target-number)
  "Return terminal facts after TARGET-NUMBER in SESSION.
Read later source-backed segments chronologically, including LIVE-BUFFER.
Flag missing or unreadable later segments under :gap so a stale running
row does not imply success.  This table lives only for one history fetch."
  (let ((completions (make-hash-table :test #'equal))
        (later nil))
    (puthash :prior-executions
             (mevedel-collaboration--prior-execution-ids
              session live-buffer target-number)
             completions)
    (when (and session (buffer-live-p live-buffer))
      (dolist (descriptor (mevedel-session-artifacts-transcript-segments
                           session live-buffer))
        (if (equal target-number (plist-get descriptor :number))
            (setq later t)
          (when later
            (unless (eq (plist-get descriptor :status) 'readable)
              (puthash :gap t completions))
            (when (eq (plist-get descriptor :status) 'readable)
              (let ((buffer
                     (if (plist-get descriptor :current-p)
                         live-buffer
                       (condition-case nil
                           (mevedel-session-artifacts-read-transcript-segment
                            session descriptor)
                         (error (puthash :gap t completions) nil)))))
                (when (buffer-live-p buffer)
                  (unwind-protect
                      (with-current-buffer buffer
                        (save-restriction
                          (widen)
                          (dolist (audit (mevedel-transcript-audit-records
                                          (buffer-substring (point-min) (point-max))
                                          'execution-completion))
                            (when-let* ((id (plist-get audit :tool-use-id))
                                        (facts (plist-get audit :render-data)))
                              (puthash id facts completions)))))
                    (unless (eq buffer live-buffer)
                      (kill-buffer buffer))))))))))
    completions))

(defun mevedel-collaboration--forwarded-execution (segment)
  "Return the native completion for EXECUTION mailbox SEGMENT, or nil.
The structural mailbox parser supplies its bounds; malformed or ordinary
mailboxes are not guest completion records."
  (save-excursion
    (goto-char (cadr segment))
    (when (search-forward "<" (caddr segment) t)
      (backward-char)
      (when-let* ((block (mevedel-transcript--mailbox-any-block-at-point
                          (caddr segment)))
                  ((eq (plist-get block :kind) 'agent-message))
                  (open (buffer-substring-no-properties
                         (plist-get block :open-start)
                         (plist-get block :open-end)))
                  ((string-match-p "\\_<type=\"EXECUTION\"" open)))
        (mevedel-view-audit-mailbox-breadcrumb
         (buffer-substring-no-properties
          (plist-get block :body-start) (plist-get block :body-end))
         (plist-get block :id))))))

(defun mevedel-collaboration--forwarded-execution-record (completion)
  "Project trusted COMPLETION as an output-free linked guest breadcrumb."
  (let* ((owner (plist-get completion :owner))
         (id (plist-get completion :execution-id))
         (facts (plist-get completion :facts))
         (outcome (plist-get facts :outcome))
         (termination (plist-get facts :termination))
         (status (cond ((memq termination '(stopped interrupted signaled
                                          cancelled owner-stopped)) "cancelled")
                       ((memq outcome '(success no-match different false))
                        "completed")
                       ((null outcome) "warning")
                       (t "failed")))
         (command (mevedel-collaboration--truncate-bytes
                   (car (split-string (or (plist-get completion :command)
                                          (plist-get facts :command) "Bash") "\n")) 200)))
    (mevedel-collaboration--record
     (concat "forwarded-execution-"
             (substring (secure-hash 'sha256 (concat owner "\0" id)) 0 24))
     "execution" :revision 0 :identity-fixed t :status status
     :execution (list :owner owner :id id :command command
                      :exitCode (plist-get facts :exit-code)))))

(defun mevedel-collaboration--canonical-records
    (data-buffer &optional completion-buffer completions)
  "Return allowlisted records reconstructed from DATA-BUFFER.
Records inside a directive turn carry that directive's id so a viewer
can filter the transcript to one directive client-side; user records
attributed to a collaboration guest carry that guest's name.
COMPLETION-BUFFER and COMPLETIONS supply later terminal evidence for archived
segments."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (let ((ranges (mevedel-collaboration--directive-ranges))
            (segments (mevedel-transcript-segments (point-min) (point-max)))
            (following-render-data (make-hash-table :test #'eql))
            (breadcrumbs (mevedel-transcript-audit-buffer-spans
                          'execution-breadcrumb))
            (forwarded-executions (make-hash-table :test #'equal))
            (prior-executions (and completions
                                   (gethash :prior-executions completions)))
            (prior-ready (and completions t))
            records user-starts (occurrences (make-hash-table :test #'equal)))
        (cl-labels
            ((add-execution (completion directive)
               (when-let* ((owner (plist-get completion :owner))
                           (id (plist-get completion :execution-id))
                           ((stringp owner)) ((stringp id))
                           (record (mevedel-collaboration--forwarded-execution-record
                                    completion))
                           (key (plist-get record :id))
                           ((not (gethash key forwarded-executions)))
                           (_ (progn
                                (unless prior-ready
                                  (setq prior-ready t)
                                  (when-let* ((session (bound-and-true-p mevedel--session))
                                            ;; Agent fetch projects only the
                                            ;; current child, not its archives.
                                            ((not (and (stringp buffer-file-name)
                                                       (string-match-p
                                                        "/agents/.*\\.chat\\.org\\'"
                                                        buffer-file-name))))
                                            ((not (cl-some
                                                   (lambda (entry)
                                                     (eq data-buffer
                                                         (mevedel-agent-record-conversation-buffer
                                                          (cdr entry))))
                                                   (mevedel-session-agent-registry
                                                    session)))))
                                    (setq prior-executions
                                          (mevedel-collaboration--prior-execution-ids
                                           session data-buffer))))
                                t))
                           ((not (and prior-executions
                                      (gethash key prior-executions)))))
                 (puthash key t forwarded-executions)
                 (when directive
                   (setq record (plist-put record :directive directive)))
                 (push record records))))
        ;; On reload gptel classifies the hidden render-data block separately
        ;; from its preceding tool row.  Include it when parsing the row, but
        ;; leave the raw model-visible result and transcript segments intact.
        (dolist (segment segments)
          (when (eq (car segment) 'render-data)
            (puthash (cadr segment) (caddr segment) following-render-data)))
        (dolist (segment segments)
          (while (and breadcrumbs (< (plist-get (car breadcrumbs) :start)
                                     (cadr segment)))
            (let* ((span (pop breadcrumbs))
                   (record (plist-get span :record)))
              (add-execution record
                             (mevedel-collaboration--directive-at
                              ranges (plist-get span :start)))))
          (let ((directive (mevedel-collaboration--directive-at
                            ranges (cadr segment))))
            (cond
             ((memq (car segment) '(user response))
              (let* ((userp (eq (car segment) 'user))
                     (kind (if userp "user" "assistant"))
                     (text (if userp
                               (mevedel-collaboration--clean-user
                                segment data-buffer)
                             (mevedel-collaboration--clean-response
                              (buffer-substring
                               (cadr segment) (caddr segment))))))
                (unless (string-empty-p text)
                  (let* ((key (list kind text))
                         (occurrence (gethash key occurrences 0)))
                    (puthash key (1+ occurrence) occurrences)
                    (push (apply
                           #'mevedel-collaboration--record
                           (mevedel-collaboration--stable-record-id
                            kind text occurrence) kind
                           :revision 0
                           :text (mevedel-collaboration--truncate-bytes
                                  text
                                  mevedel-collaboration--max-record-text-bytes)
                           (when directive (list :directive directive)))
                          records)
                    (when userp
                      (push (cons (cadr segment) (car records))
                            user-starts))))))
             ((memq (car segment) '(ignored render-data))
              (let ((summary (cdr (mevedel-tool-render-data-extract
                                   (buffer-substring
                                    (cadr segment) (caddr segment))))))
                (when (and (eq (plist-get summary :kind) 'request-summary)
                           (eq (plist-get summary :outcome) 'error))
                  (let* ((text (concat "Assistant request failed. "
                                       (or (plist-get summary :message)
                                           "Retry from the host or send a follow-up.")))
                         (key (list "failure" text))
                         (occurrence (gethash key occurrences 0)))
                    (puthash key (1+ occurrence) occurrences)
                    (push (mevedel-collaboration--record
                           (mevedel-collaboration--stable-record-id
                            "failure" text occurrence)
                           "assistant" :revision 0 :status "failed"
                           :text (mevedel-collaboration--truncate-bytes text 2000))
                          records)))))
             ((eq (car segment) 'mailbox)
              (when-let* ((completion (mevedel-collaboration--forwarded-execution
                                       segment)))
                (add-execution completion directive)))
             ((eq (car segment) 'tool)
              (let* ((start (cadr segment))
                     (end (caddr segment))
                     (raw (buffer-substring-no-properties start end))
                     (key (list "tool" raw))
                     (occurrence (gethash key occurrences 0)))
                (puthash key (1+ occurrence) occurrences)
                (dolist (record (mevedel-collaboration--tool-segment-records
                                 data-buffer segment occurrence completion-buffer
                                 (gethash end following-render-data) completions))
                  (when directive
                    (setq record (plist-put record :directive directive)))
                  (push record records)))))))
        (dolist (span breadcrumbs)
          (add-execution (plist-get span :record)
                         (mevedel-collaboration--directive-at
                          ranges (plist-get span :start))))
        (mevedel-collaboration--attribute-guest-prompts (nreverse user-starts))
        (nreverse records))))))

(defun mevedel-collaboration--tool-records (records)
  "Return the tool records in RECORDS, preserving their order."
  (let (tools)
    (dolist (record records)
      (when (and (equal (plist-get record :kind) "tool")
                 (not (plist-get record :artifact-child)))
        (push record tools)))
    (nreverse tools)))

(defun mevedel-collaboration--tool-call-fingerprint (info)
  "Return a bounded stable fingerprint for tool-call INFO."
  (let ((print-level 4)
        (print-length 32)
        (print-circle t))
    (prin1-to-string (plist-get info :args))))

(defun mevedel-collaboration--tool-call-key (info)
  "Return the matching key for a gptel tool-call INFO plist."
  (or (and (plist-get info :id)
           (format "id:%s" (plist-get info :id)))
      (and (plist-get info :call-id)
           (format "id:%s" (plist-get info :call-id)))
      (and (plist-get info :tool-call-id)
           (format "id:%s" (plist-get info :tool-call-id)))
      (and (plist-get info :tool_call_id)
           (format "id:%s" (plist-get info :tool_call_id)))
      (format "call:%s\0%s"
              (plist-get info :name)
              (mevedel-collaboration--tool-call-fingerprint info))))

(defun mevedel-collaboration--tool-result-fields (result)
  "Return status, bounded RESULT, and truncation for tool RESULT."
  (let* ((result (if (stringp result) result (format "%s" (or result ""))))
         (result (mevedel--trim-tool-result result))
         (status (if (string-match-p
                      mevedel-collaboration--tool-error-regexp result)
                     "failed"
                   "completed"))
         (bounded (mevedel-collaboration--truncate-bytes
                   result mevedel-collaboration--max-tool-result-bytes)))
    (list :status status
          :result bounded
          :truncated (and (string-suffix-p "\n[truncated]" bounded) t))))

(defun mevedel-collaboration--pending-tool-match (info pending)
  "Return non-nil when INFO matches pending tool record PENDING."
  (and (equal (plist-get pending :call-key)
              (mevedel-collaboration--tool-call-key info))
       (equal (plist-get pending :name)
              (format "%s" (plist-get info :name)))))

(defun mevedel-collaboration--suppressed-tool-landed-p (data-buffer entry)
  "Return non-nil when a hidden canonical poll has settled ENTRY.
Only consider tool segments inserted after ENTRY began, and require the same
call key; a previous identical poll must not consume a later pending call."
  (when (buffer-live-p data-buffer)
    (with-current-buffer data-buffer
      (cl-some
       (lambda (segment)
         (when (and (eq (car segment) 'tool)
                    (>= (cadr segment)
                        (or (plist-get entry :baseline-buffer-end)
                            (point-min))))
           (let ((parsed (mevedel-view--tool-call-parse
                          data-buffer (cadr segment) (caddr segment))))
             (and parsed
                  (equal (plist-get entry :call-key)
                         (mevedel-collaboration--tool-call-key parsed))
                  (null (mevedel-collaboration--tool-record
                         parsed
                         (buffer-substring-no-properties
                          (cadr segment) (caddr segment))))))))
       (mevedel-transcript-segments (point-min) (point-max))))))

(defun mevedel-collaboration--project-records (room)
  "Return the current semantic projection for ROOM.

Pending tool records are a live-only projection until their settled
canonical transcript record appears.  A pending record's identity is copied
onto that canonical record, so a viewer updates one card from running through
completion instead of seeing a duplicate tool card."
  (let* ((canonical (mevedel-collaboration--canonical-records
                     (plist-get room :data-buffer)))
         (pending (plist-get room :pending-tools))
         (canonical-tools (mevedel-collaboration--tool-records canonical))
         (claimed nil)
         (remaining nil))
    (dolist (entry pending)
      (let* ((status (plist-get entry :status))
             (baseline (min (length canonical-tools)
                            (max 0 (or (plist-get entry :baseline-tool-count)
                                       0))))
             (candidates (nthcdr baseline canonical-tools))
             exact candidate)
        ;; Prefer the exact settled twin, but fall back to the first
        ;; unclaimed same-name record at or after this entry's baseline:
        ;; the canonical transcript is authoritative once a record lands
        ;; there, and an unmatched pending would otherwise duplicate it
        ;; forever -- as a stuck "running" card when the settlement info
        ;; missed the pending entry, or as a completed twin when the
        ;; transcript-formatted result text diverges from the raw result.
        (dolist (record candidates)
          (unless (memq record claimed)
            (when (equal (plist-get record :name) (plist-get entry :name))
              (unless candidate (setq candidate record))
              (when (and (null exact)
                         (equal (plist-get record :status) status)
                         (equal (plist-get record :result)
                                (plist-get entry :result)))
                (setq exact record)))))
        (setq candidate (or exact candidate))
        (if candidate
            (let ((index (cl-position candidate canonical :test #'eq)))
              (push candidate claimed)
              (setf (nth index canonical)
                    (plist-put (plist-put candidate :id
                                          (plist-get entry :id))
                               :identity-fixed t)))
          (unless (and (not (equal status "running"))
                       (mevedel-collaboration--suppressed-tool-landed-p
                        (plist-get room :data-buffer) entry))
            (push entry remaining)))))
    (setq remaining (nreverse remaining))
    ;; A room plist is never empty, so this mutates in place and the
    ;; room registry keeps pointing at the same object.
    (setq room (plist-put room :pending-tools remaining))
    (let ((pending-at nil)
          (length (length canonical))
          output)
      (dolist (entry remaining)
        (let* ((baseline (min length
                              (max 0 (or (plist-get entry
                                                    :baseline-record-count)
                                         length))))
               (cell (assq baseline pending-at)))
          (if cell
              (setcdr cell (append (cdr cell) (list entry)))
            (push (cons baseline (list entry)) pending-at))))
      (dotimes (index (1+ length))
        (dolist (entry (cdr (assq index pending-at)))
          (push entry output))
        (when (< index length)
          (push (nth index canonical) output)))
      (nreverse output))))


(provide 'mevedel-collaboration-projection)
;;; mevedel-collaboration-projection.el ends here
