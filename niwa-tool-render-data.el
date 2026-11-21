;;; niwa-tool-render-data.el -- Tool render-data side channel -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool render-data serialization, provider scrubbing, transcript
;; mutation, and stale execution reconciliation.  Pipeline sequencing and
;; render-transform/attachment steps remain in niwa-pipeline.el.

;;; Code:

;; Required at load time, not lazily per call: the extraction and strip
;; helpers here run per transcript segment on every streaming redraw, and
;; even a satisfied `require' scans `features' each call.
(require 'cl-lib)
(require 'subr-x)
(require 'niwa-structs)
(require 'niwa-tool-media)
(require 'niwa-transcript-audit)
(require 'niwa-history)
(require 'niwa-utilities)

;; `gptel'
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)

;; `niwa-execution-transcript'
(declare-function niwa-execution-transcript-retry-pending-terminals
                  "niwa-execution-transcript" (data-buffer))
(autoload 'niwa-execution-transcript-retry-pending-terminals
  "niwa-execution-transcript")

;; `niwa-structs'
(declare-function niwa-session-save-path "niwa-structs" (cl-x) t)
(defvar niwa--session)

;; `niwa-tool-media'
(declare-function niwa-tool-media-add-to-provider-result
                  "niwa-tool-media" (backend parsed media-by-index))
(declare-function niwa-tool-media-extract
                  "niwa-tool-media"
                  (result-string &optional tool-results-dir expected-tool-use-id
                                 allow-payload-tool-use-id session))
(declare-function niwa-tool-media-prepare-tool-result
                  "niwa-tool-media"
                  (backend tool-call tool-results-dir &optional session))
(declare-function niwa-tool-media-strip-blocks
                  "niwa-tool-media" (string))

;; `niwa-transcript-audit'
(declare-function niwa--format-hook-audit-record
                  "niwa-transcript-audit" (record))
(declare-function niwa--strip-hook-audit-blocks
                  "niwa-transcript-audit" (string))
(declare-function niwa-transcript-audit-spans
                  "niwa-transcript-audit" (text &optional type))

;; `niwa-utilities'
(declare-function niwa--normalize-message-text
                  "niwa-utilities" (text))

;; `subr'
(defvar read-eval)

(defconst niwa-tool-render-data-open "<!-- niwa-render-data -->"
  "Opening delimiter marking a hidden render-data side-channel block.
Emitted inside tool results so the view buffer interpreter can extract
the serialized render-data without re-running the tool.")

(defconst niwa-tool-render-data-close "<!-- /niwa-render-data -->"
  "Closing delimiter marking the end of a render-data side-channel block.")

(defun niwa-tool-render-data-direct-call (name data)
  "Return the settled direct child of tool NAME with render DATA, or nil.
Envelope failures and live programs retain their own presentation.  The child
is display metadata only; it never replaces the provider's call identity."
  (when (and (equal name "ToolCall")
             (eq (plist-get data :kind) 'ptc)
             (stringp (plist-get data :direct-tool))
             (not (plist-get data :live-p))
             (memq (plist-get data :outcome) '(completed tool-error)))
    (let* ((calls (plist-get data :calls))
           (child (and (consp calls) (null (cdr calls)) (car calls))))
      (when (and child
                 (equal (plist-get child :tool) (plist-get data :direct-tool))
                 (not (and (eq (plist-get data :status) 'error)
                           (not (eq (plist-get child :status) 'error)))))
        (let ((facts (plist-get child :render-data)))
          ;; A persisted child call's status predates terminal or reload facts.
          (plist-put (copy-sequence child) :render-data
                     (if (memq (plist-get facts :state)
                               '(completed interrupted lost archived))
                         facts
                       (plist-put (copy-sequence facts)
                                  :status (plist-get child :status)))))))))

(defun niwa-tool-render-data--plain (value)
  "Return VALUE with text properties stripped from all contained strings."
  (cond
   ((stringp value)
    (niwa--normalize-message-text (substring-no-properties value)))
   ((consp value)
    (cons (niwa-tool-render-data--plain (car value))
          (niwa-tool-render-data--plain (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'niwa-tool-render-data--plain value)))
   (t value)))

(defun niwa-tool-render-data-size (data)
  "Return the printed size of DATA after stripping text properties."
  (length (prin1-to-string (niwa-tool-render-data--plain data))))

(defun niwa-tool-render-data-format
    (render-data &optional tool-use-id)
  "Return the serialized side-channel block for RENDER-DATA.
When TOOL-USE-ID is non-nil, bind the block to that tool call.
The returned string is propertized `invisible' = t so the data buffer
hides it as a best-effort display courtesy.

The block survives verbatim into the chat buffer (which feeds the view
parser and persistence).  An `:around' advice on
`gptel--parse-tool-results' -- installed by
`niwa-tool-render-data-install-provider-adapter' -- strips the block at
the single chokepoint where tool result strings become the LLM-bound
API message, without touching the callback that drives chat-buffer
display."
  (let ((data (niwa-tool-render-data--plain render-data)))
    (if tool-use-id
        (setq data
              (plist-put data :niwa-tool-use-id tool-use-id))
      (cl-remf data :niwa-tool-use-id))
    (propertize
     (concat "\n" niwa-tool-render-data-open "\n"
             (let ((print-level nil)
                   (print-length nil)
                   (print-circle t))
               (prin1-to-string data))
             "\n" niwa-tool-render-data-close "\n")
     'invisible t
     'gptel 'niwa-render-data
     'niwa-render-data t)))

(defun niwa-tool-render-data--delimiter-at-p (delimiter source position limit)
  "Return non-nil when DELIMITER occurs in SOURCE at POSITION before LIMIT.
Nil SOURCE denotes the current buffer.  Do not copy surrounding payloads."
  (let ((end (+ position (length delimiter))))
    (and (<= end limit)
         (if source
             (eq t (compare-strings delimiter nil nil source position end))
           (save-excursion
             (goto-char position)
             (looking-at-p (regexp-quote delimiter)))))))

(defun niwa-tool-render-data--read-block (source start limit)
  "Decode a complete render-data block in SOURCE at START before LIMIT.
Nil SOURCE denotes the current buffer.  Return (BEGIN END DATA), including
optional framing newlines, or nil.  Delimiters inside the Lisp object are
data, not boundaries.  This function validates syntax, not provenance."
  (when (niwa-tool-render-data--delimiter-at-p
         niwa-tool-render-data-open source start limit)
    (condition-case nil
        (let* ((read-eval nil)
               (payload-start (+ start (length niwa-tool-render-data-open)))
               (parsed (if source
                           (read-from-string source payload-start limit)
                         (goto-char payload-start)
                         (let ((data (read (current-buffer))))
                           (cons data (point)))))
               (data (car parsed))
               (cursor (cdr parsed)))
          (when (and (consp data)
                     (proper-list-p data)
                     (zerop (% (length data) 2))
                     (cl-loop for tail on data by #'cddr
                              always (keywordp (car tail))))
            (while (and (< cursor limit)
                        (memq (if source (aref source cursor) (char-after cursor))
                              '(?\s ?\t ?\r ?\n)))
              (cl-incf cursor))
            (when (niwa-tool-render-data--delimiter-at-p
                   niwa-tool-render-data-close source cursor limit)
              (let ((end (+ cursor (length niwa-tool-render-data-close)))
                    (minimum (if source 0 (point-min))))
                (list (if (and (> start minimum)
                               (eq (if source (aref source (1- start))
                                     (char-before start)) ?\n))
                          (1- start) start)
                      (if (and (< end limit)
                               (eq (if source (aref source end)
                                     (char-after end)) ?\n))
                          (1+ end) end)
                      data)))))
      (error nil))))

(defun niwa-tool-render-data-block-at (start &optional limit)
  "Return the complete render-data block opening at START before LIMIT.
The result is (BEGIN END DATA), in current-buffer coordinates, or nil.
Preserve point, match data and narrowing; read directly without copying the
buffer.  Callers must independently establish ownership or provenance."
  (save-match-data
    (save-excursion
      (save-restriction
        (narrow-to-region (point-min) (min (or limit (point-max)) (point-max)))
        (niwa-tool-render-data--read-block nil start (point-max))))))

(defun niwa-tool-render-data-blocks (string)
  "Return valid render-data blocks found in STRING, oldest first.
Each entry is `(BEGIN END DATA)'.  Invalid marker-looking text is skipped.
BEGIN and END include the formatter's optional surrounding newlines."
  (when (stringp string)
    (let ((search-start 0)
          blocks open)
      (while (setq open
                   (string-search niwa-tool-render-data-open
                                  string search-start))
        (let ((block (niwa-tool-render-data--read-block string open (length string))))
          (if block
              (progn
                (push block blocks)
                (setq search-start (cadr block)))
            (setq search-start (+ open (length niwa-tool-render-data-open))))))
      (nreverse blocks))))

(defun niwa-tool-render-data-trusted-range-p
    (start end &optional object)
  "Return non-nil when START..END is trusted render data in OBJECT.
OBJECT is a string or buffer and defaults to the current buffer."
  (let ((source (or object (current-buffer))))
    (while (and (< start end)
                (memq (if (stringp source)
                          (aref source (1- end))
                        (with-current-buffer source
                          (char-after (1- end))))
                      '(?\s ?\t ?\r ?\n)))
      (setq end (1- end)))
    (and (< start end)
         (or
          (and (eq t (get-text-property start 'niwa-render-data source))
               (= end (next-single-property-change
                       start 'niwa-render-data source end)))
          (and (eq 'niwa-render-data
                   (get-text-property start 'gptel source))
               (= end (next-single-property-change
                       start 'gptel source end)))))))

(defun niwa-tool-render-data--owner-p (data expected-tool-use-id)
  "Return non-nil when DATA belongs to EXPECTED-TOOL-USE-ID.
Nil EXPECTED-TOOL-USE-ID selects only unbound render data."
  (if expected-tool-use-id
      (and (plist-member data :niwa-tool-use-id)
           (equal expected-tool-use-id
                  (plist-get data :niwa-tool-use-id)))
    (not (plist-member data :niwa-tool-use-id))))

(defun niwa-tool-render-data--authorized-p
    (block string expected-tool-use-id)
  "Return non-nil when BLOCK in STRING has the expected authority."
  (and (niwa-tool-render-data--owner-p
        (caddr block) expected-tool-use-id)
       (or expected-tool-use-id
           (niwa-tool-render-data-trusted-range-p
            (car block) (cadr block) string))))

(defun niwa-tool-render-data-without-owner (data)
  "Return a copy of DATA without its internal owner field."
  (let ((plain (copy-sequence data)))
    (cl-remf plain :niwa-tool-use-id)
    plain))

(defun niwa-tool-render-data-strip
    (string &optional expected-tool-use-id)
  "Remove render-data owned by EXPECTED-TOOL-USE-ID from STRING.
Nil EXPECTED-TOOL-USE-ID removes only unbound blocks."
  (if-let* ((blocks
             (cl-remove-if-not
              (lambda (block)
                (niwa-tool-render-data--authorized-p
                 block string expected-tool-use-id))
              (niwa-tool-render-data-blocks string))))
      (let ((cursor 0)
            parts)
        (dolist (block blocks)
          (push (substring string cursor (car block)) parts)
          (setq cursor (cadr block)))
        (push (substring string cursor) parts)
        (apply #'concat (nreverse parts)))
    string))

(defun niwa-tool-render-data-strip-non-media
    (string &optional expected-tool-use-id)
  "Remove trusted non-media side channels from STRING.
Render data must belong to EXPECTED-TOOL-USE-ID; nil selects unbound data.
Non-string tool results pass through unchanged."
  (if (stringp string)
      (niwa--strip-hook-audit-blocks
       (niwa-tool-render-data-strip
        string expected-tool-use-id))
    string))

(defun niwa-tool-render-data-find-agent-block (agent-id)
  "Return bounds of the first render-data block for AGENT-ID.

The return value is (BEG . END), or nil when no block has a matching
`:agent-id'.
Searches the current buffer from `point-min'.  Used by the
background handle patch path to locate the block whose hidden
plist should be updated when a sub-agent's status changes."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (found)
        (while (and (not found)
                    (search-forward niwa-tool-render-data-open nil t))
          (when-let* ((block (niwa-tool-render-data-block-at (match-beginning 0))))
            (pcase-let ((`(,block-beg ,block-end ,plist) block))
              (goto-char block-end)
              (when (and (eq (plist-get plist :kind) 'collaboration-event)
                         (eq (plist-get plist :event) 'started)
                         (if (plist-member plist :niwa-tool-use-id)
                             (niwa-tool-render-data-call-range-p
                              plist block-beg block-end)
                           (niwa-tool-render-data-trusted-range-p
                            block-beg block-end))
                         (equal (plist-get plist :agent-id) agent-id))
                (setq found (cons block-beg block-end))))))
        found))))

(defun niwa-tool-render-data-patch-block (beg end new-plist)
  "Replace the render-data block between BEG and END with NEW-PLIST.
Preserves the surrounding text and the hidden-block delimiters.
The new block is formatted via
`niwa-tool-render-data-format' so it stays
round-trippable through `niwa-tool-render-data-extract'.

Inherits the `gptel' text property of the surrounding text onto the
inserted block.  Without this inheritance, the block becomes a hole in
the gptel-property run that delimits the tool segment; the view
buffer's `extract-segments' then splits the tool segment in two and
the LLM-invisible block leaks into the visible body of the tool
result."
  (save-excursion
    (save-restriction
      (widen)
      (let ((surrounding-gptel
             (or (and (consp (get-text-property beg 'gptel))
                      (eq (car (get-text-property beg 'gptel)) 'tool)
                      (get-text-property beg 'gptel))
                 (and (> beg (point-min))
                      (get-text-property (1- beg) 'gptel))
                 (and (< end (point-max))
                      (get-text-property end 'gptel))))
            (tool-use-id
             (when-let* ((block (save-excursion
                                 (goto-char beg)
                                 (skip-chars-forward "\n" end)
                                 (niwa-tool-render-data-block-at (point) end))))
               (plist-get (caddr block) :niwa-tool-use-id))))
        (goto-char beg)
        (delete-region beg end)
        (let ((block
               (niwa-tool-render-data-format
                new-plist tool-use-id)))
          (when surrounding-gptel
            (setq block (propertize block 'gptel surrounding-gptel)))
          (insert block))))))

(defun niwa-tool-render-data--display-results-advice
    (orig-fun tool-results info)
  "Render TOOL-RESULTS from INFO's authoritative call records via ORIG-FUN.

gptel's display callback omits call IDs and its renderer recovers them by
name, confusing repeated calls to one tool.  For niwa conversations,
render each completed call with its own ID, arguments and result from
`:tool-use'.  Restrict that lookup to the current call while reusing the
original renderer and mutable insertion state.  Display order follows the
model's call order, independently of asynchronous completion order."
  (if (not (when-let* ((buffer (plist-get info :buffer))
                      ((buffer-live-p buffer)))
             (buffer-local-value 'niwa--session buffer)))
      (funcall orig-fun tool-results info)
    (let ((calls (plist-get info :tool-use)))
      (unwind-protect
          (dolist (call calls)
            (let* ((name (plist-get call :name))
                   (tool (car (cl-find name tool-results :test #'equal
                                       :key (lambda (entry)
                                              (gptel-tool-name (car entry)))))))
              (unless tool
                (error "Missing display result for tool: %s" name))
              (plist-put info :tool-use (list call))
              (funcall orig-fun
                       (list (list tool (plist-get call :args)
                                   (plist-get call :result)))
                       info)))
        (plist-put info :tool-use calls))
      ;; Terminal events precede insertion.  Agent turns can loop through
      ;; hundreds of batches (and compaction) before post-response hooks run.
      ;; Settle at the durable insertion boundary, even without an open view.
      (niwa-execution-transcript-retry-pending-terminals
       (plist-get info :buffer))
      (niwa-history-record-tool-batch info))))

(defun niwa-tool-render-data-segment-bounds (tool-use-id)
  "Return current-buffer bounds carrying TOOL-USE-ID, or nil."
  (let ((target (cons 'tool tool-use-id))
        (position (point-min))
        bounds)
    (while (and (< position (point-max)) (not bounds))
      (let* ((value (get-text-property position 'gptel))
             (next (or (next-single-property-change
                        position 'gptel nil (point-max))
                       (point-max))))
        (when (equal value target)
          (let ((end next))
            (while (and (< end (point-max))
                        (equal (get-text-property end 'gptel) target))
              (setq end (or (next-single-property-change
                             end 'gptel nil (point-max))
                            (point-max))))
            (setq bounds (cons position end))))
        (setq position next)))
    bounds))

(defun niwa-tool-render-data-call-range-p (data beg end)
  "Return non-nil when DATA at BEG..END belongs to its tool segment."
  (when-let* ((tool-use-id (plist-get data :niwa-tool-use-id))
              ((stringp tool-use-id)))
    (let ((target (cons 'tool tool-use-id))
          (positions (list beg (and (> beg (point-min)) (1- beg))))
          authorized)
      (while (and positions (not authorized))
        (when-let* ((position (pop positions))
                    ((< position (point-max)))
                    ((equal target (get-text-property position 'gptel))))
          (let ((next (or (next-single-property-change
                           position 'gptel nil (point-max))
                          (point-max))))
            (setq authorized
                  (or (and (= position beg) (<= end next))
                      (and (memq next (list beg (1+ beg)))
                           (niwa-tool-render-data-trusted-range-p
                            (max beg next) end)))))))
      authorized)))

(defun niwa-tool-render-data--find-block
    (beg end &optional expected-tool-use-id)
  "Return a matching (BEGIN END DATA) block inside BEG..END, or nil.
The block must belong to EXPECTED-TOOL-USE-ID; nil selects unbound data."
  (save-excursion
    (goto-char beg)
    (let (found)
      (while (and (not found)
                  (search-forward niwa-tool-render-data-open end t))
        (when-let* ((block (niwa-tool-render-data-block-at (match-beginning 0) end)))
          (goto-char (cadr block))
          (when (niwa-tool-render-data--owner-p (caddr block) expected-tool-use-id)
            (setq found block))))
      found)))

(defun niwa-tool-render-data--next-segment-start (position tool-use-id)
  "Return the first different tool segment after POSITION.
TOOL-USE-ID identifies property runs that still belong to the current tool."
  (let ((cursor position)
        found)
    (while (and (< cursor (point-max)) (not found))
      (let ((property (get-text-property cursor 'gptel)))
        (when (and (consp property)
                   (eq (car property) 'tool)
                   (not (equal (cdr property) tool-use-id)))
          (setq found cursor)))
      (unless found
        (setq cursor
              (or (next-single-property-change
                   cursor 'gptel nil (point-max))
                  (point-max)))))
    found))

(defun niwa-tool-render-data--plist-merge (base updates)
  "Return a copy of BASE with each key in UPDATES replaced."
  (let ((merged (copy-tree base)))
    (while updates
      (setq merged (plist-put merged (pop updates) (pop updates))))
    merged))

(defun niwa-tool-render-data--nested-call-location (buffer tool-use-id)
  "Return (ROOT-ID ROOT-DATA CALLS-TAIL) for TOOL-USE-ID in BUFFER, or nil.
The car of CALLS-TAIL is the matching child in ROOT-DATA.
Only follow the exact numbered child ancestry assigned by the PTC driver.
Each intermediate child must be a ToolCall with PTC render data; arbitrary
matching ids elsewhere in a payload are not tool identities.  The traversal
is iterative and bounded by the number of components in TOOL-USE-ID."
  (when (stringp tool-use-id)
    (let ((root tool-use-id) path)
      (while (string-match "/[1-9][0-9]*\\'" root)
        (push root path)
        (setq root (substring root 0 (match-beginning 0))))
      (when (and path (not (string-empty-p root)))
        (let* ((data (niwa-tool-render-data-for-tool buffer root))
               (parent data)
               tail)
          (while (and path (proper-list-p parent)
                      (eq (plist-get parent :kind) 'ptc)
                      (proper-list-p (plist-get parent :calls)))
            (let ((id (pop path)))
              (setq tail
                    (member-if
                     (lambda (call)
                       (and (proper-list-p call)
                            (equal (plist-get call :id) id)))
                     (plist-get parent :calls))
                    parent
                    (and (equal (plist-get (car tail) :tool) "ToolCall")
                         (plist-get (car tail) :render-data)))))
          (when (and tail (null path))
            (list root data tail)))))))

(defun niwa-tool-render-data-update
    (buffer tool-use-id updates)
  "Merge UPDATES into TOOL-USE-ID's hidden render data in BUFFER.

Return non-nil when the authoritative tool segment was found and updated.
For a numbered ToolCall child, merge into that child's render data inside
the enclosing real tool segment, preserving its siblings and envelope.
The side channel remains inside the segment's `gptel' property run, so it is
persisted with the transcript while the provider scrubber keeps it model-hidden."
  (when (and (buffer-live-p buffer)
             (stringp tool-use-id)
             (listp updates))
    (or (with-current-buffer buffer
          (save-restriction
            (widen)
            (when-let* ((segment
                         (niwa-tool-render-data-segment-bounds tool-use-id)))
              (let* ((beg (car segment))
                     (end (cdr segment))
                     (search-end
                      (or (niwa-tool-render-data--next-segment-start
                           end tool-use-id)
                          (point-max)))
                     (block
                      (niwa-tool-render-data--find-block
                       beg search-end tool-use-id))
                     (existing
                      (and block
                           (niwa-tool-render-data-without-owner (caddr block))))
                     (render-data
                      (niwa-tool-render-data--plist-merge existing updates))
                     (inhibit-modification-hooks t)
                     (inhibit-read-only t))
                (if block
                    (niwa-tool-render-data-patch-block
                     (car block) (cadr block) render-data)
                  (goto-char end)
                  (insert
                   (propertize
                    (niwa-tool-render-data-format
                     render-data tool-use-id)
                    'gptel (get-text-property beg 'gptel))))
                t))))
        (when-let* ((location
                     (niwa-tool-render-data--nested-call-location
                      buffer tool-use-id)))
          (let* ((tail (nth 2 location))
                 (child (car tail)))
            (setcar tail
                    (niwa-tool-render-data--plist-merge
                     child
                     (list :render-data
                           (niwa-tool-render-data--plist-merge
                            (plist-get child :render-data) updates))))
            (niwa-tool-render-data-update
             buffer (car location)
             (list :calls (plist-get (cadr location) :calls))))))))

(defun niwa-tool-render-data-for-tool (buffer tool-use-id)
  "Return TOOL-USE-ID's hidden render data in BUFFER, or nil.
Numbered ToolCall child ids select only their exact nested call metadata."
  (when (and (buffer-live-p buffer) (stringp tool-use-id))
    (or (with-current-buffer buffer
          (save-restriction
            (widen)
            (when-let* ((segment
                         (niwa-tool-render-data-segment-bounds tool-use-id))
                        (search-end
                         (or (niwa-tool-render-data--next-segment-start
                              (cdr segment) tool-use-id)
                             (point-max)))
                        (block
                         (niwa-tool-render-data--find-block
                          (car segment) search-end tool-use-id)))
              (niwa-tool-render-data-without-owner (caddr block)))))
        (when-let* ((location
                     (niwa-tool-render-data--nested-call-location
                      buffer tool-use-id)))
          (plist-get (car (nth 2 location)) :render-data)))))

(defun niwa-tool-render-data--reconcile-running
    (data successor-execution-ids &optional lost-monitor)
  "Return (REPAIRED-DATA . COUNT) for stale executions in DATA.
ToolCall render data may contain other ToolCalls and Bash calls; repair their
children without changing successful siblings or unrelated outer metadata.
SUCCESSOR-EXECUTION-IDS identifies records with newer facts.  Call
LOST-MONITOR with the repaired facts for each monitor actually lost."
  (let ((count 0)
        (repaired data))
    (when (and (listp data)
               (or (plist-get data :execution-id)
                   (plist-get data :live-execution-p))
               (or (eq (plist-get data :state) 'running)
                   (plist-get data :live-execution-p)))
      (setq repaired
            (niwa-tool-render-data--plist-merge
             data
             (if (member (plist-get data :execution-id)
                         successor-execution-ids)
                 '(:state archived :status nil :live-execution-p nil
                          :termination compacted :outcome nil)
               '(:state lost :status error :live-execution-p nil
                        :termination lost :outcome failure)))
            count 1)
      (when (and lost-monitor (eq (plist-get repaired :kind) 'monitor)
                 (eq (plist-get repaired :state) 'lost))
        (funcall lost-monitor repaired)))
    (when (and (listp data) (eq (plist-get data :kind) 'ptc))
      (let ((child-count 0) calls)
        (dolist (call (plist-get data :calls))
          (pcase-let ((`(,child . ,repaired-count)
                       (niwa-tool-render-data--reconcile-running
                        (plist-get call :render-data)
                        successor-execution-ids lost-monitor)))
            (cl-incf child-count repaired-count)
            (push (if (> repaired-count 0)
                      (niwa-tool-render-data--plist-merge
                       call (list :render-data child))
                    call)
                  calls)))
        (when (> child-count 0)
          (setq repaired
                (niwa-tool-render-data--plist-merge
                 repaired (list :calls (nreverse calls))))
          (cl-incf count child-count))))
    (cons repaired count)))

(defun niwa-tool-render-data-reconcile-lost-executions
    (buffer &optional successor-execution-ids lost-monitor)
  "Mark stale running execution records in BUFFER as lost.

This repairs transcript state after resume or fork.  It never attempts to
reattach an operating-system process.  Records named by
SUCCESSOR-EXECUTION-IDS are marked archived because a newer segment owns their
terminal truth.  Call LOST-MONITOR with each lost monitor's repaired facts.
Return the number of repaired executions."
  (let (records archived-records)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward niwa-tool-render-data-open nil t)
              (when-let* ((block (niwa-tool-render-data-block-at (match-beginning 0))))
                (pcase-let* ((`(,begin ,end ,stored) block)
                            (data
                             (and (niwa-tool-render-data-call-range-p
                                   stored begin end)
                                  (niwa-tool-render-data-without-owner stored)))
                            (repair
                             (and data
                                  (niwa-tool-render-data--reconcile-running
                                   data successor-execution-ids lost-monitor))))
                  (goto-char end)
                  (when (and repair (> (cdr repair) 0))
                    (push (list begin end repair) records))))))
          (dolist (record records)
            (pcase-let ((`(,begin ,end ,repair) record))
              (niwa-tool-render-data-patch-block
               begin end (car repair))))
          (setq archived-records
                (cl-loop for span in (niwa-transcript-audit-spans
                                      (buffer-substring (point-min) (point-max))
                                      'execution-archive)
                         for repair =
                         (niwa-tool-render-data--reconcile-running
                          (plist-get (plist-get span :record) :render-data)
                          successor-execution-ids lost-monitor)
                         when (> (cdr repair) 0)
                         collect (cons span repair)))
          (dolist (entry (reverse archived-records))
            (let* ((span (car entry))
                   (repair (cdr entry))
                   (record (plist-get span :record))
                   (begin (+ (point-min) (plist-get span :start)))
                   (end (+ (point-min) (plist-get span :end))))
              (setq record
                    (plist-put
                     record :type 'execution-completion))
              (setq record
                    (plist-put
                     record :render-data (car repair)))
              (delete-region begin end)
              (goto-char begin)
              (insert (niwa--format-hook-audit-record record))))))
      (+ (cl-loop for record in records sum (cdr (nth 2 record)))
         (cl-loop for entry in archived-records sum (cdr (cdr entry)))))))

(defun niwa-tool-render-data--provider-advice (orig-fun backend tool-use)
  "Strip render-data blocks from TOOL-USE results for BACKEND via ORIG-FUN.

Wraps `gptel--parse-tool-results' (a `cl-defgeneric' with per-backend
methods in gptel-openai.el, gptel-anthropic.el, ...) which is the sole
point at which `:result' strings are copied into the API-shaped
tool_result message.  Both request paths funnel through it:

- Tool-follow-up requests (`gptel--handle-tool-result' ->
  `gptel--parse-tool-results' -> `gptel--inject-prompt').
- User-initiated requests that re-parse the chat buffer
  (`gptel--parse-buffer' calls `gptel--parse-tool-results' on each
  stored tool-call region).

The advice temporarily substitutes cleaned strings into the `:result'
slot of each tool-call plist, calls ORIG-FUN so the backend method
builds its message from the scrubbed values, then restores the original
`:result' values.  Everything downstream that consumes `:tool-use' or
`:tool-result' for display (the gptel callback feeding the chat buffer,
the view parser, persistence) keeps seeing the full block."
  (let ((saved nil))
    (unwind-protect
        (let* ((media-by-index nil)
               (session (bound-and-true-p niwa--session))
               (tool-results-dir
                (when-let* ((save-path (and session
                                           (niwa-session-save-path session))))
                  (file-name-concat save-path "tool-results"))))
          (dolist (tc tool-use)
            (let* ((orig (plist-get tc :result))
                   (clean-tc (copy-sequence tc))
                   (prepared
                    (progn
                      (when (stringp orig)
                        (plist-put
                         clean-tc :result
                         (niwa-tool-render-data-strip-non-media
                          orig (plist-get tc :id))))
                      (niwa-tool-media-prepare-tool-result
                       backend clean-tc tool-results-dir session)))
                   (llm-result (car prepared))
                   (native-media (cdr prepared)))
              (push native-media media-by-index)
              (when (and llm-result (not (equal orig llm-result)))
                (push (cons tc orig) saved)
                (plist-put tc :result llm-result))))
          (niwa-tool-media-add-to-provider-result
           backend
           (funcall orig-fun backend tool-use)
           (nreverse media-by-index)))
      (dolist (entry saved)
        (plist-put (car entry) :result (cdr entry))))))

(defun niwa-tool-render-data-install-provider-adapter ()
  "Install gptel interop advice for tool-result continuation paths."
  (require 'gptel-request)
  (niwa-history-install)
  (advice-add 'gptel--parse-tool-results :around
              #'niwa-tool-render-data--provider-advice)
  (advice-add 'gptel--display-tool-results :around
              #'niwa-tool-render-data--display-results-advice))

(defun niwa-tool-render-data-uninstall-provider-adapter ()
  "Remove gptel interop advice for tool-result continuation paths."
  (niwa-history-uninstall)
  (advice-remove 'gptel--parse-tool-results
                 #'niwa-tool-render-data--provider-advice)
  (advice-remove 'gptel--display-tool-results
                 #'niwa-tool-render-data--display-results-advice))

(defun niwa-tool-render-data-extract
    (result-string &optional session expected-tool-use-id
                   allow-payload-tool-use-id)
  "Return (VISIBLE-PART . RENDER-DATA) parsed from RESULT-STRING.
VISIBLE-PART is the tool result with the side-channel block stripped.
RENDER-DATA is the plist deserialized from inside the block, or
nil when no valid block is present.  Unparseable payloads are treated as
absent: the original string is returned verbatim in VISIBLE-PART.
SESSION, EXPECTED-TOOL-USE-ID, and ALLOW-PAYLOAD-TOOL-USE-ID
control trusted side-channel lookup."
  (let ((tool-results-dir
         (when-let* ((save-path (and session
                                     (niwa-session-save-path session))))
           (file-name-concat save-path "tool-results"))))
    (if (not (stringp result-string))
        (cons result-string nil)
      (let* ((blocks
              (cl-remove-if-not
               (lambda (block)
                 (niwa-tool-render-data--authorized-p
                  block result-string expected-tool-use-id))
               (niwa-tool-render-data-blocks result-string)))
             (block (car (last blocks)))
             (visible
              (if block
                  (concat (substring result-string 0 (car block))
                          (substring result-string (cadr block)))
                result-string))
             (media-visible
              (car (niwa-tool-media-extract
                    (niwa-tool-media-strip-blocks visible)
                    tool-results-dir
                    expected-tool-use-id
                    allow-payload-tool-use-id
                    session))))
        (cons (if block (string-trim-right media-visible) media-visible)
              (and block
                   (niwa-tool-render-data-without-owner
                    (caddr block))))))))

(provide 'niwa-tool-render-data)
;;; niwa-tool-render-data.el ends here
