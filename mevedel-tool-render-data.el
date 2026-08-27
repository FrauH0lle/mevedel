;;; mevedel-tool-render-data.el -- Tool render-data side channel -*- lexical-binding: t -*-

;;; Commentary:

;; Owns tool render-data serialization, provider scrubbing, transcript
;; mutation, and stale execution reconciliation.  Pipeline sequencing and
;; render-transform/attachment steps remain in mevedel-pipeline.el.

;;; Code:

;; Required at load time, not lazily per call: the extraction and strip
;; helpers here run per transcript segment on every streaming redraw, and
;; even a satisfied `require' scans `features' each call.
(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-structs)
(require 'mevedel-tool-media)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)

;; `mevedel-structs'
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

;; `mevedel-tool-media'
(declare-function mevedel-tool-media-add-to-provider-result
                  "mevedel-tool-media" (backend parsed media-by-index))
(declare-function mevedel-tool-media-extract
                  "mevedel-tool-media"
                  (result-string &optional tool-results-dir expected-tool-use-id
                                 allow-payload-tool-use-id session))
(declare-function mevedel-tool-media-prepare-tool-result
                  "mevedel-tool-media"
                  (backend tool-call tool-results-dir &optional session))
(declare-function mevedel-tool-media-strip-blocks
                  "mevedel-tool-media" (string))

;; `mevedel-transcript-audit'
(declare-function mevedel--format-hook-audit-record
                  "mevedel-transcript-audit" (record))
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (string))
(declare-function mevedel-transcript-audit-spans
                  "mevedel-transcript-audit" (text &optional type))

;; `mevedel-utilities'
(declare-function mevedel--normalize-message-text
                  "mevedel-utilities" (text))

;; `subr'
(defvar read-eval)

(defconst mevedel-tool-render-data-open "<!-- mevedel-render-data -->"
  "Opening delimiter marking a hidden render-data side-channel block.
Emitted inside tool results so the view buffer interpreter can extract
the serialized render-data without re-running the tool.")

(defconst mevedel-tool-render-data-close "<!-- /mevedel-render-data -->"
  "Closing delimiter marking the end of a render-data side-channel block.")

(defun mevedel-tool-render-data--plain (value)
  "Return VALUE with text properties stripped from all contained strings."
  (cond
   ((stringp value)
    (mevedel--normalize-message-text (substring-no-properties value)))
   ((consp value)
    (cons (mevedel-tool-render-data--plain (car value))
          (mevedel-tool-render-data--plain (cdr value))))
   ((vectorp value)
    (apply #'vector
           (mapcar #'mevedel-tool-render-data--plain value)))
   (t value)))

(defun mevedel-tool-render-data-size (data)
  "Return the printed size of DATA after stripping text properties."
  (length (prin1-to-string (mevedel-tool-render-data--plain data))))

(defun mevedel-tool-render-data-format
    (render-data &optional tool-use-id)
  "Return the serialized side-channel block for RENDER-DATA.
When TOOL-USE-ID is non-nil, bind the block to that tool call.
The returned string is propertized `invisible' = t so the data buffer
hides it as a best-effort display courtesy.

The block survives verbatim into the chat buffer (which feeds the view
parser and persistence).  An `:around' advice on
`gptel--parse-tool-results' -- installed by
`mevedel-tool-render-data-install-provider-adapter' -- strips the block at
the single chokepoint where tool result strings become the LLM-bound
API message, without touching the callback that drives chat-buffer
display."
  (let ((data (mevedel-tool-render-data--plain render-data)))
    (setq data (copy-sequence data))
    (if tool-use-id
        (setq data
              (plist-put data :mevedel-tool-use-id tool-use-id))
      (cl-remf data :mevedel-tool-use-id))
    (propertize
     (concat "\n" mevedel-tool-render-data-open "\n"
             (let ((print-level nil)
                   (print-length nil)
                   (print-circle t))
               (prin1-to-string data))
             "\n" mevedel-tool-render-data-close "\n")
     'invisible t
     'gptel 'mevedel-render-data
     'mevedel-render-data t)))

(defun mevedel-tool-render-data--read-payload (payload)
  "Read one render-data plist from PAYLOAD or return a failure sentinel."
  (condition-case nil
      (let* ((read-eval nil)
             (parsed (read-from-string payload))
             (data (car parsed))
             (end (cdr parsed))
             (rest (substring payload end)))
        (if (and (consp data)
                 (string-blank-p rest)
                 (proper-list-p data)
                 (zerop (% (length data) 2))
                 (cl-loop for tail on data by #'cddr
                          always (keywordp (car tail))))
            data
          :mevedel-parse-failed))
    (error :mevedel-parse-failed)))

(defun mevedel-tool-render-data-blocks (string)
  "Return valid render-data blocks found in STRING, oldest first.
Each entry is `(BEGIN END DATA)'.  Invalid marker-looking text is skipped.
BEGIN and END include the formatter's optional surrounding newlines."
  (when (stringp string)
    (let ((search-start 0)
          blocks open)
      (while (setq open
                   (string-search mevedel-tool-render-data-open
                                  string search-start))
        (let* ((payload-start
                (+ open (length mevedel-tool-render-data-open)))
               (close
                (string-search mevedel-tool-render-data-close
                               string payload-start)))
          (if (not close)
              (setq search-start payload-start)
            (let* ((payload
                    (string-trim
                     (substring string payload-start close)))
                   (data
                    (mevedel-tool-render-data--read-payload payload))
                   (next-open
                    (string-search mevedel-tool-render-data-open
                                   string payload-start))
                   (close-end
                    (+ close (length mevedel-tool-render-data-close))))
              (cond
               ((not (eq data :mevedel-parse-failed))
                (let ((begin
                       (if (and (> open 0)
                                (eq (aref string (1- open)) ?\n))
                           (1- open)
                         open))
                      (end
                       (if (and (< close-end (length string))
                                (eq (aref string close-end) ?\n))
                           (1+ close-end)
                         close-end)))
                  (push (list begin end data) blocks))
                (setq search-start close-end))
               ((and next-open (< next-open close))
                (setq search-start next-open))
               (t
                (setq search-start close-end)))))))
      (nreverse blocks))))

(defun mevedel-tool-render-data-trusted-range-p
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
          (and (eq t (get-text-property start 'mevedel-render-data source))
               (= end (next-single-property-change
                       start 'mevedel-render-data source end)))
          (and (eq 'mevedel-render-data
                   (get-text-property start 'gptel source))
               (= end (next-single-property-change
                       start 'gptel source end)))))))

(defun mevedel-tool-render-data--owner-p (data expected-tool-use-id)
  "Return non-nil when DATA belongs to EXPECTED-TOOL-USE-ID.
Nil EXPECTED-TOOL-USE-ID selects only unbound render data."
  (if expected-tool-use-id
      (and (plist-member data :mevedel-tool-use-id)
           (equal expected-tool-use-id
                  (plist-get data :mevedel-tool-use-id)))
    (not (plist-member data :mevedel-tool-use-id))))

(defun mevedel-tool-render-data--authorized-p
    (block string expected-tool-use-id)
  "Return non-nil when BLOCK in STRING has the expected authority."
  (and (mevedel-tool-render-data--owner-p
        (caddr block) expected-tool-use-id)
       (or expected-tool-use-id
           (mevedel-tool-render-data-trusted-range-p
            (car block) (cadr block) string))))

(defun mevedel-tool-render-data-without-owner (data)
  "Return a copy of DATA without its internal owner field."
  (let ((plain (copy-sequence data)))
    (cl-remf plain :mevedel-tool-use-id)
    plain))

(defun mevedel-tool-render-data-strip
    (string &optional expected-tool-use-id)
  "Remove render-data owned by EXPECTED-TOOL-USE-ID from STRING.
Nil EXPECTED-TOOL-USE-ID removes only unbound blocks."
  (if-let* ((blocks
             (cl-remove-if-not
              (lambda (block)
                (mevedel-tool-render-data--authorized-p
                 block string expected-tool-use-id))
              (mevedel-tool-render-data-blocks string))))
      (let ((cursor 0)
            parts)
        (dolist (block blocks)
          (push (substring string cursor (car block)) parts)
          (setq cursor (cadr block)))
        (push (substring string cursor) parts)
        (apply #'concat (nreverse parts)))
    string))

(defun mevedel-tool-render-data-strip-non-media
    (string &optional expected-tool-use-id)
  "Remove trusted non-media side channels from STRING.
Render data must belong to EXPECTED-TOOL-USE-ID; nil selects unbound data."
  (mevedel--strip-hook-audit-blocks
   (mevedel-tool-render-data-strip
    string expected-tool-use-id)))

(defun mevedel-tool-render-data-find-agent-block (agent-id)
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
                    (search-forward mevedel-tool-render-data-open nil t))
          (let* ((open-beg (match-beginning 0))
                 (block-beg (if (and (> open-beg (point-min))
                                     (eq (char-before open-beg) ?\n))
                                (1- open-beg)
                              open-beg)))
            (if (not (search-forward mevedel-tool-render-data-close nil t))
                (goto-char (point-max))
              (let* ((close-end (match-end 0))
                     (block-end (if (and (< close-end (point-max))
                                         (eq (char-after close-end) ?\n))
                                    (1+ close-end)
                                  close-end))
                     (raw (buffer-substring-no-properties block-beg block-end))
                     (block (car (mevedel-tool-render-data-blocks raw)))
                     (plist (and block (caddr block))))
                (when (and (eq (plist-get plist :kind)
                               'collaboration-event)
                           (eq (plist-get plist :event) 'started)
                           (if (plist-member plist :mevedel-tool-use-id)
                               (mevedel-tool-render-data-call-range-p
                                plist block-beg block-end)
                             (mevedel-tool-render-data-trusted-range-p
                              block-beg block-end))
                           (equal (plist-get plist :agent-id) agent-id))
                  (setq found (cons block-beg block-end)))))))
        found))))

(defun mevedel-tool-render-data-patch-block (beg end new-plist)
  "Replace the render-data block between BEG and END with NEW-PLIST.
Preserves the surrounding text and the hidden-block delimiters.
The new block is formatted via
`mevedel-tool-render-data-format' so it stays
round-trippable through `mevedel-tool-render-data-extract'.

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
             (or (and (> beg (point-min))
                      (get-text-property (1- beg) 'gptel))
                 (and (< end (point-max))
                      (get-text-property end 'gptel))))
            (tool-use-id
             (when-let* ((block
                          (car
                           (mevedel-tool-render-data-blocks
                            (buffer-substring-no-properties beg end)))))
               (plist-get (caddr block) :mevedel-tool-use-id))))
        (goto-char beg)
        (delete-region beg end)
        (let ((block
               (mevedel-tool-render-data-format
                new-plist tool-use-id)))
          (when surrounding-gptel
            (setq block (propertize block 'gptel surrounding-gptel)))
          (insert block))))))

(defun mevedel-tool-render-data--tool-property-bounds (position)
  "Return (START END . ID) for the tool property run covering POSITION.

The run holding POSITION is the answer while the transcript is live, where
gptel propertized the whole tool payload -- render-data block included --
as one run.  After a restore the grammar splits the payload, so the tool
run is the one ending where POSITION\='s run begins."
  (let ((candidates (list position)))
    (let ((start (or (previous-single-property-change
                      (min (1+ position) (point-max)) 'gptel)
                     (point-min))))
      (when (> start (point-min))
        (push (1- start) candidates)))
    (catch 'found
      (dolist (candidate candidates)
        (let ((value (get-text-property candidate 'gptel)))
          (when (and (consp value) (eq (car value) 'tool))
            (throw 'found
                   (list (or (previous-single-property-change
                              (min (1+ candidate) (point-max)) 'gptel)
                             (point-min))
                         (or (next-single-property-change candidate 'gptel)
                             (point-max))
                         (cdr value)))))))))

(defun mevedel-tool-render-data-repair-owner-properties
    (&optional beg end)
  "Restamp tool ids in BEG..END from mevedel\='s own render-data owners.

gptel resolves the id it stamps on a tool block by tool name -- a
`cl-find-if' over `:tool-use' matching only `:name' -- so two calls to the
same tool in one turn both receive the first match\='s id.  The block whose
id was taken then disagrees with the owner mevedel wrote inside it, its
render data reads as unauthorized, and the raw side-channel text renders as
the tool\='s output.

mevedel knows the true id: it stamped it into the block itself.  Only a
block mevedel wrote carries the `mevedel-render-data' property, so a tool
result that merely contains the delimiters cannot steer the id and the
authority check keeps its meaning.  Return how many runs were restamped."
  (let ((position (or beg (point-min)))
        (limit (or end (point-max)))
        (repaired 0))
    (while (< position limit)
      (let ((next (or (next-single-property-change
                       position 'mevedel-render-data nil limit)
                      limit)))
        (when (eq t (get-text-property position 'mevedel-render-data))
          (when-let* ((block (car (mevedel-tool-render-data-blocks
                                   (buffer-substring-no-properties
                                    position next))))
                      (owner (plist-get (caddr block) :mevedel-tool-use-id))
                      ((stringp owner))
                      (bounds (mevedel-tool-render-data--tool-property-bounds
                               position))
                      ((not (equal owner (caddr bounds)))))
            (put-text-property (car bounds) (cadr bounds)
                               'gptel (cons 'tool owner))
            (cl-incf repaired)))
        (setq position next)))
    repaired))

(defun mevedel-tool-render-data--display-results-advice
    (orig-fun tool-results info)
  "Call ORIG-FUN, then repair ids in the TOOL-RESULTS inserted for INFO."
  (let* ((boundary (or (plist-get info :tool-marker)
                       (plist-get info :tracking-marker)
                       (plist-get info :position)))
         (start (and (markerp boundary)
                     (marker-buffer boundary)
                     (copy-marker boundary nil))))
    (unwind-protect
        (prog1 (funcall orig-fun tool-results info)
          (when-let* ((start-buffer (and start (marker-buffer start)))
                      ((buffer-live-p start-buffer))
                      (end (plist-get info :tool-marker))
                      ((markerp end))
                      ((eq start-buffer (marker-buffer end))))
            (with-current-buffer start-buffer
              (let ((inhibit-read-only t))
                (mevedel-tool-render-data-repair-owner-properties
                 start end)))))
      (when start
        (set-marker start nil)))))

(defun mevedel-tool-render-data-segment-bounds (tool-use-id)
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

(defun mevedel-tool-render-data-call-range-p (data beg end)
  "Return non-nil when DATA at BEG..END belongs to its tool segment."
  (when-let* ((tool-use-id (plist-get data :mevedel-tool-use-id))
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
                           (mevedel-tool-render-data-trusted-range-p
                            (max beg next) end)))))))
      authorized)))

(defun mevedel-tool-render-data--block-bounds
    (beg end &optional expected-tool-use-id)
  "Return matching render-data bounds inside BEG..END, or nil.
The block must belong to EXPECTED-TOOL-USE-ID; nil selects unbound data."
  (when-let* ((block
               (cl-find-if
                (lambda (candidate)
                  (mevedel-tool-render-data--owner-p
                   (caddr candidate) expected-tool-use-id))
                (mevedel-tool-render-data-blocks
                 (buffer-substring-no-properties beg end)))))
    (cons (+ beg (car block)) (+ beg (cadr block)))))

(defun mevedel-tool-render-data--next-segment-start (position tool-use-id)
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

(defun mevedel-tool-render-data--plist-merge (base updates)
  "Return a copy of BASE with each key in UPDATES replaced."
  (let ((merged (copy-tree base)))
    (while updates
      (setq merged (plist-put merged (pop updates) (pop updates))))
    merged))

(defun mevedel-tool-render-data-update
    (buffer tool-use-id updates)
  "Merge UPDATES into TOOL-USE-ID's hidden render data in BUFFER.

Return non-nil when the authoritative tool segment was found and updated.
The side channel remains inside the segment's `gptel' property run, so it is
persisted with the transcript while the provider scrubber keeps it model-hidden."
  (when (and (buffer-live-p buffer)
             (stringp tool-use-id)
             (listp updates))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (when-let* ((segment
                     (mevedel-tool-render-data-segment-bounds tool-use-id)))
          (let* ((beg (car segment))
                 (end (cdr segment))
                 (search-end
                  (or (mevedel-tool-render-data--next-segment-start
                       end tool-use-id)
                      (point-max)))
                 (block
                  (mevedel-tool-render-data--block-bounds
                   beg search-end tool-use-id))
                 (existing
                  (and block
                       (cdr
                        (mevedel-tool-render-data-extract
                         (buffer-substring-no-properties
                          (car block) (cdr block))
                         nil tool-use-id))))
                 (render-data
                  (mevedel-tool-render-data--plist-merge existing updates))
                 (inhibit-modification-hooks t)
                 (inhibit-read-only t))
            (if block
                (mevedel-tool-render-data-patch-block
                 (car block) (cdr block) render-data)
              (goto-char end)
              (insert
               (propertize
                (mevedel-tool-render-data-format
                 render-data tool-use-id)
                'gptel (get-text-property beg 'gptel))))
            t))))))

(defun mevedel-tool-render-data-for-tool (buffer tool-use-id)
  "Return TOOL-USE-ID's hidden render data in BUFFER, or nil."
  (when (and (buffer-live-p buffer) (stringp tool-use-id))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (when-let* ((segment
                     (mevedel-tool-render-data-segment-bounds tool-use-id))
                    (block
                     (mevedel-tool-render-data--block-bounds
                      (car segment) (cdr segment) tool-use-id)))
          (cdr
           (mevedel-tool-render-data-extract
            (buffer-substring-no-properties (car block) (cdr block))
            nil tool-use-id)))))))

(defun mevedel-tool-render-data-reconcile-lost-executions
    (buffer &optional successor-execution-ids)
  "Mark stale running Bash render records in BUFFER as lost.

This repairs transcript state after resume or fork.  It never attempts to
reattach an operating-system process.  Records named by
SUCCESSOR-EXECUTION-IDS are marked archived because a newer segment owns their
terminal truth.  Return the number of repaired records."
  (let (records archived-records)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (point-min))
            (while (search-forward mevedel-tool-render-data-open nil t)
              (let* ((open-begin (match-beginning 0))
                     (begin
                      (if (and (> open-begin (point-min))
                               (eq (char-before open-begin) ?\n))
                          (1- open-begin)
                        open-begin)))
                (when (search-forward mevedel-tool-render-data-close nil t)
                  (let* ((close-end (match-end 0))
                         (end
                          (if (and (< close-end (point-max))
                                   (eq (char-after close-end) ?\n))
                              (1+ close-end)
                            close-end))
                         (block
                          (car
                           (mevedel-tool-render-data-blocks
                            (buffer-substring-no-properties begin end))))
                         (stored (and block (caddr block)))
                         (data
                          (and (mevedel-tool-render-data-call-range-p
                                stored begin end)
                               (mevedel-tool-render-data-without-owner
                                stored))))
                    (when (and data
                               (or (plist-get data :execution-id)
                                   (plist-get data :live-execution-p))
                               (or (eq (plist-get data :state) 'running)
                                   (plist-get data :live-execution-p)))
                      (push (list begin end data) records)))))))
          (dolist (record records)
            (pcase-let ((`(,begin ,end ,data) record))
              (mevedel-tool-render-data-patch-block
               begin end
               (mevedel-tool-render-data--plist-merge
                data
                (if (member (plist-get data :execution-id)
                            successor-execution-ids)
                    '(:state archived :status nil :live-execution-p nil
                             :termination compacted :outcome nil)
                  '(:state lost :status error :live-execution-p nil
                           :termination lost :outcome failure))))))
          (setq archived-records
                (cl-remove-if-not
                 (lambda (span)
                   (let* ((record (plist-get span :record))
                          (data (plist-get record :render-data)))
                     (and (or (eq (plist-get data :state) 'running)
                              (plist-get data :live-execution-p))
                          data)))
                 (mevedel-transcript-audit-spans
                  (buffer-substring (point-min) (point-max))
                  'execution-archive)))
          (dolist (span (reverse archived-records))
            (let* ((record (plist-get span :record))
                   (data (plist-get record :render-data))
                   (successor-p
                    (member (plist-get data :execution-id)
                            successor-execution-ids))
                   (begin (+ (point-min) (plist-get span :start)))
                   (end (+ (point-min) (plist-get span :end))))
              (setq record
                    (plist-put
                     record :type 'execution-completion))
              (setq record
                    (plist-put
                     record :render-data
                     (mevedel-tool-render-data--plist-merge
                      data
                      (if successor-p
                          '(:state archived :status nil
                                   :live-execution-p nil
                                   :termination compacted :outcome nil)
                        '(:state lost :status error
                                 :live-execution-p nil
                                 :termination lost :outcome failure)))))
              (delete-region begin end)
              (goto-char begin)
              (insert (mevedel--format-hook-audit-record record))))))
      (+ (length records) (length archived-records)))))

(defun mevedel-tool-render-data--provider-advice (orig-fun backend tool-use)
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
               (session (bound-and-true-p mevedel--session))
               (tool-results-dir
                (when-let ((save-path (and session
                                           (mevedel-session-save-path session))))
                  (file-name-concat save-path "tool-results"))))
          (dolist (tc tool-use)
            (let* ((orig (plist-get tc :result))
                   (clean-tc (copy-sequence tc))
                   (prepared
                    (progn
                      (when (stringp orig)
                        (plist-put
                         clean-tc :result
                         (mevedel-tool-render-data-strip-non-media
                          orig (plist-get tc :id))))
                      (mevedel-tool-media-prepare-tool-result
                       backend clean-tc tool-results-dir session)))
                   (llm-result (car prepared))
                   (native-media (cdr prepared)))
              (push native-media media-by-index)
              (when (and llm-result (not (equal orig llm-result)))
                (push (cons tc orig) saved)
                (plist-put tc :result llm-result))))
          (mevedel-tool-media-add-to-provider-result
           backend
           (funcall orig-fun backend tool-use)
           (nreverse media-by-index)))
      (dolist (entry saved)
        (plist-put (car entry) :result (cdr entry))))))

(defun mevedel-tool-render-data-install-provider-adapter ()
  "Install gptel interop advice for tool-result continuation paths."
  (require 'gptel-request)
  (advice-add 'gptel--parse-tool-results :around
              #'mevedel-tool-render-data--provider-advice)
  (advice-add 'gptel--display-tool-results :around
              #'mevedel-tool-render-data--display-results-advice))

(defun mevedel-tool-render-data-uninstall-provider-adapter ()
  "Remove gptel interop advice for tool-result continuation paths."
  (advice-remove 'gptel--parse-tool-results
                 #'mevedel-tool-render-data--provider-advice)
  (advice-remove 'gptel--display-tool-results
                 #'mevedel-tool-render-data--display-results-advice))

(defun mevedel-tool-render-data-extract
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
                                     (mevedel-session-save-path session))))
           (file-name-concat save-path "tool-results"))))
    (if (not (stringp result-string))
        (cons result-string nil)
      (let* ((blocks
              (cl-remove-if-not
               (lambda (block)
                 (mevedel-tool-render-data--authorized-p
                  block result-string expected-tool-use-id))
               (mevedel-tool-render-data-blocks result-string)))
             (block (car (last blocks)))
             (visible
              (if block
                  (concat (substring result-string 0 (car block))
                          (substring result-string (cadr block)))
                result-string))
             (media-visible
              (car (mevedel-tool-media-extract
                    (mevedel-tool-media-strip-blocks visible)
                    tool-results-dir
                    expected-tool-use-id
                    allow-payload-tool-use-id
                    session))))
        (cons (if block (string-trim-right media-visible) media-visible)
              (and block
                   (mevedel-tool-render-data-without-owner
                    (caddr block))))))))

(provide 'mevedel-tool-render-data)
;;; mevedel-tool-render-data.el ends here
