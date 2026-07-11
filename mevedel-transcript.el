;;; mevedel-transcript.el --- Transcript span classification -*- lexical-binding: t -*-

;;; Commentary:

;; Read-only helpers for classifying the gptel data buffer transcript into
;; structural spans.  Callers decide how to render, persist, or compact the
;; spans.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

(defun mevedel-transcript--skip-leading-properties-drawer (pos)
  "Return POS advanced past a leading `:PROPERTIES:' drawer, if any.

gptel-org stores per-buffer state (preset, model, backend, system
prompt, bounds) in an org `:PROPERTIES:' drawer at the top of the
data buffer.  The drawer has no `gptel' text property, so the segment
extractor would classify it as a user turn and render its raw text in
the view on a full rerender (session resume, compaction, manual
refresh).  Skip past it so the rendered view starts at real content."
  (save-excursion
    (goto-char pos)
    (if (and (looking-at-p ":PROPERTIES:$")
             (re-search-forward "^:END:[ \t]*\n" nil t))
        (point)
      pos)))

(defun mevedel-transcript--skip-leading-summary-block (pos)
  "Return POS advanced past a leading compaction summary block, if any."
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t\n")
    (if (and (looking-at-p "#\\+begin_summary\\b")
             (re-search-forward "^#\\+end_summary[^\n]*\n?" nil t))
        (progn
          (skip-chars-forward " \t\n")
          (point))
      pos)))

(defun mevedel-transcript--after-leading-system-reminders ()
  "Return first position after leading system-reminder blocks."
  (save-excursion
    (goto-char (point-min))
    (while (looking-at "<system-reminder>\n")
      (if (search-forward "\n</system-reminder>" nil t)
          (when (looking-at "\n\n")
            (forward-char 2))
        (goto-char (point-max))))
    (point)))

(defun mevedel-transcript-prompt-transform-start ()
  "Return the start of the last user prompt in a transform buffer."
  (let ((pos (point-max)))
    (while (and (> pos (point-min))
                (let ((prop (get-text-property (1- pos) 'gptel)))
                  (cond
                   ((eq prop 'ignore)
                    (setq pos (or (previous-single-property-change
                                   (1- pos) 'gptel nil (point-min))
                                  (point-min))))
                   ((and (null prop)
                         (memq (char-before pos) '(?\s ?\t ?\n ?\r)))
                    (setq pos (1- pos)))
                   (t nil)))))
    (if-let* ((boundary (and (> pos (point-min))
                             (previous-single-property-change
                              (1- pos) 'gptel nil (point-min))))
              ((> boundary (point-min))))
        boundary
      (mevedel-transcript--after-leading-system-reminders))))

(defun mevedel-transcript--classify-gptel-prop (prop)
  "Classify a `gptel' text property value PROP into a segment type symbol."
  (pcase prop
    ('nil 'user)
    ('response 'response)
    ('ignore 'ignore)
    (`(tool . ,_id) 'tool)
    (_ 'response)))


(defun mevedel-transcript--org-scaffolding-only-text-p (text)
  "Return non-nil when TEXT is blank or only org begin/end marker lines."
  (and (stringp text)
       (with-temp-buffer
         (insert text)
         (goto-char (point-min))
         (catch 'not-scaffolding
           (while (not (eobp))
             (let ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
               (unless (or (string-empty-p (string-trim line))
                           (string-match-p
                            "\\`[ \t]*#\\+\\(?:begin\\|end\\)_[[:alnum:]_-]+\\b.*\\'"
                            line))
                 (throw 'not-scaffolding nil)))
             (forward-line 1))
           t))))

(defun mevedel-transcript--scaffolding-only-p (data-buf seg-start seg-end)
  "Return non-nil if DATA-BUF region [SEG-START, SEG-END] is org-only glue.
A segment is org-only when it contains nothing but `#+begin_...' /
`#+end_...' marker lines, blank lines, and whitespace.  Classification
uses this to avoid treating restored org marker gaps as assistant prose."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min seg-start pmax)))
             (e (max pmin (min seg-end pmax)))
             (text (and (< s e)
                        (buffer-substring-no-properties s e))))
        (or (null text)
            (mevedel-transcript--org-scaffolding-only-text-p text))))))

(defun mevedel-transcript--org-block-depth-before (pos block-re)
  "Return nesting depth before POS for org blocks matching BLOCK-RE.

BLOCK-RE should match the suffix after `#+begin_' / `#+end_', for
example `tool\\|reasoning'."
  (save-excursion
    (save-restriction
      (widen)
      (let ((depth 0)
            (regexp (format "^#\\+\\(begin\\|end\\)_\\(?:%s\\)\\b"
                            block-re)))
        (goto-char (point-min))
        (while (re-search-forward regexp pos t)
          (if (equal (match-string 1) "begin")
              (cl-incf depth)
            (setq depth (max 0 (1- depth)))))
        depth))))

(defun mevedel-transcript--user-prompt-start (pos next prop)
  "Return the real PROP prompt start in [POS, NEXT), or nil.

Nil-`gptel' regions can contain a mixture of org block glue and the
next user prompt, for example `#+end_tool' / reasoning text /
`#+end_reasoning' / user text.  Scan line-wise so callers agree on the
first non-empty line outside gptel-owned org blocks."
  (when (null prop)
    (save-excursion
      (save-restriction
        (widen)
        (let ((tool-depth
               (mevedel-transcript--org-block-depth-before
                pos "tool\\|reasoning"))
              (summary-depth
               (mevedel-transcript--org-block-depth-before pos "summary")))
          (goto-char pos)
          (catch 'found
            (while (< (point) next)
              (let* ((line-start (point))
                     (line-end (min next (line-end-position)))
                     (line (buffer-substring-no-properties
                            line-start line-end)))
                (cond
                 ((string-match-p
                   "\\`[ \t]*#\\+begin_\\(?:tool\\|reasoning\\)\\b"
                   line)
                  (cl-incf tool-depth))
                 ((string-match-p
                   "\\`[ \t]*#\\+end_\\(?:tool\\|reasoning\\)\\b"
                   line)
                  (setq tool-depth (max 0 (1- tool-depth))))
                 ((string-match-p "\\`[ \t]*#\\+begin_summary\\b" line)
                  (cl-incf summary-depth))
                 ((string-match-p "\\`[ \t]*#\\+end_summary\\b" line)
                  (setq summary-depth (max 0 (1- summary-depth))))
                 ((and (= tool-depth 0)
                       (= summary-depth 0)
                       (not (string-empty-p (string-trim line)))
                       (not (string-match-p
                             "\\`[ \t]*#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\|summary\\)\\b"
                             line)))
                  (throw 'found line-start)))
                (forward-line 1)))
            nil))))))


(defun mevedel-transcript--queued-user-message-batch-control-text (text)
  "Return TEXT trimmed of leading org tool-close glue.
Generated queued batches can be inserted immediately after a tool block,
leaving unpropertized `#+end_tool' marker text in the same nil-`gptel'
segment.  Strip only that trailing-tool structural glue so ordinary prose
containing queued-message XML is still rendered literally."
  (when (stringp text)
    (with-temp-buffer
      (insert (string-trim text))
      (goto-char (point-min))
      (while (looking-at "#\\+end_tool\\b[^\n]*\n?")
        (delete-region (match-beginning 0) (match-end 0))
        (skip-chars-forward " \t\r\n"))
      (string-trim (buffer-string)))))

(defun mevedel-transcript--queued-user-message-batch-items-from-text (text)
  "Return generated queued user-message items parsed from TEXT, or nil.
TEXT must consist only of leading org tool-close glue, the generated
optional system reminder, and one complete queued-message batch.  Literal
examples embedded in user text are not treated as control markup."
  (when (stringp text)
    (with-temp-buffer
      (insert (mevedel-transcript--queued-user-message-batch-control-text text))
      (goto-char (point-min))
      (when (looking-at "<system-reminder>")
        (goto-char (match-end 0))
        (if (search-forward "</system-reminder>" nil t)
            (skip-chars-forward " \t\r\n")
          (goto-char (point-max))))
      (when (looking-at
             "<queued-user-message-batch[[:space:]][^>]*count=\"\\([0-9]+\\)\"[^>]*>")
        (let ((count (string-to-number (match-string 1)))
              (body-start (match-end 0)))
          (goto-char body-start)
          (when (search-forward "</queued-user-message-batch>" nil t)
            (let ((body-end (match-beginning 0)))
              (skip-chars-forward " \t\r\n")
              (when (= (point) (point-max))
                (mevedel-transcript--queued-user-message-items-from-body
                 (buffer-substring-no-properties body-start body-end)
                 count)))))))))

(defun mevedel-transcript--queued-user-message-items-from-body (body count)
  "Return queued user-message items parsed from BODY matching COUNT."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let (items ok)
      (setq ok t)
      (while ok
        (skip-chars-forward " \t\r\n")
        (cond
         ((eobp)
          (setq ok nil))
         ((looking-at
           "<queued-user-message[[:space:]][^>]*index=\"\\([0-9]+\\)\"[^>]*>")
          (let ((index (string-to-number (match-string 1)))
                (body-start (match-end 0)))
            (goto-char body-start)
            (if (search-forward "</queued-user-message>" nil t)
                (push (cons index
                            (string-trim
                             (buffer-substring-no-properties
                              body-start (match-beginning 0))))
                      items)
              (setq ok :invalid))))
         (t
          (setq ok :invalid))))
      (when (and (not (eq ok :invalid))
                 (= count (length items))
                 (> count 0))
        (mapcar #'cdr (sort items (lambda (a b) (< (car a) (car b)))))))))


(defun mevedel-transcript--extract-segments (start end)
  "Extract segments from the data buffer between START and END.
Returns a list of (TYPE DATA-START DATA-END) where TYPE is one of
`user', `response', `tool', or `ignore'.  Walks forward through
text property changes on the `gptel' property.

START and END are first expanded to whole `gptel' property runs.  This
matters for incremental re-rendering via `gptel-post-response-functions':
those hooks may report a changed region that begins in the middle of an
existing tool or response segment.  Without boundary expansion, the
first extracted tool segment can start after the leading newline and
opening paren of the tool plist, so reparsing sees `:name ...' instead
of `(:name ...)'."
  (let (segments seg-start seg-type)
    (save-excursion
      (setq start (or (previous-single-property-change (min (1+ start) (point-max))
                                                       'gptel nil (point-min))
                      (point-min))
            end (or (next-single-property-change end 'gptel nil (point-max))
                    (point-max)))
      (setq start (mevedel-transcript--skip-leading-properties-drawer start))
      (setq start (mevedel-transcript--skip-leading-summary-block start))
      (goto-char start)
      (setq seg-start start
            seg-type (mevedel-transcript--classify-gptel-prop
                      (get-text-property start 'gptel)))
      (while (< (point) end)
        (let ((next (next-single-property-change (point) 'gptel nil end)))
          (goto-char next)
          (when (< next end)
            ;; Property changed before end -- push the completed segment
            ;; and start a new one.
            (push (list seg-type seg-start next) segments)
            (setq seg-start next
                  seg-type (mevedel-transcript--classify-gptel-prop
                            (get-text-property next 'gptel))))))
      ;; Push the final (or only) segment.
      (when (< seg-start end)
        (push (list seg-type seg-start end) segments)))
    (setq segments (nreverse segments))
    (mevedel-transcript--split-queued-user-message-batch-segments
     (mevedel-transcript--repair-response-fragment-segments
      (mevedel-transcript--split-structural-user-response-prefixes
       (mevedel-transcript--normalize-tool-block-segments segments start end))))))

(defun mevedel-transcript--split-queued-user-message-batch-segments (segments)
  "Split generated queued-message batch suffixes out of user SEGMENTS."
  (let (out)
    (dolist (seg segments (nreverse out))
      (pcase-let ((`(,type ,seg-start ,seg-end) seg))
        (if (not (eq type 'user))
            (push seg out)
          (let (split-start)
            (save-excursion
              (goto-char seg-start)
              (while (and (not split-start)
                          (search-forward "<system-reminder>" seg-end t))
                (let ((candidate (match-beginning 0)))
                  (when (mevedel-transcript--queued-user-message-batch-items-from-text
                         (buffer-substring-no-properties candidate seg-end))
                    (setq split-start candidate)))))
            (if split-start
                (progn
                  (when (< seg-start split-start)
                    (push (list type seg-start split-start) out))
                  (push (list type split-start seg-end) out))
              (push seg out))))))))

(defun mevedel-transcript--org-tool-blocks-overlapping (segments start end)
  "Return org tool block bounds from SEGMENTS overlapping START..END.
Bounds include the `#+begin_tool' and `#+end_tool' marker lines.  The
view uses these structural anchors to repair stale restored
`GPTEL_BOUNDS' that split a single tool block across several property
runs."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+begin_tool\\b" end t)
        (let* ((block-start (match-beginning 0))
               (marker-end (match-end 0))
               (block-end
                (and (mevedel-transcript--org-tool-block-start-p block-start)
                     (mevedel-transcript--tool-block-end-from-start block-start)))
               (overlap-p
                (and block-end
                     (or (mevedel-transcript--tool-block-overlaps-tool-segment-p
                          segments block-start block-end)
                         (mevedel-transcript--tool-block-inside-ignore-segment-p
                          segments block-start block-end)))))
          (when (and block-end (not overlap-p))
            (when-let* ((min-end
                         (mevedel-transcript--first-tool-segment-start-after
                          segments block-start end))
                        (retry-min
                         (or (mevedel-transcript--tool-block-start-before
                              min-end block-end)
                             min-end)))
              (when-let* ((retry-end
                            (mevedel-transcript--tool-block-retry-end
                             block-start nil block-end retry-min)))
                (setq block-end retry-end)
                (setq overlap-p
                      (and block-end
                           (mevedel-transcript--tool-block-overlaps-tool-segment-p
                            segments block-start block-end))))))
          (if (and block-end
                   (< block-start end)
                   (> block-end start)
                   overlap-p)
              (progn
                (push (cons block-start block-end) blocks)
                (goto-char (max block-end marker-end)))
            (goto-char (min (1+ block-start) (point-max)))))))
    (sort blocks
          (lambda (a b) (< (car a) (car b))))))

(defun mevedel-transcript--tool-block-overlaps-tool-segment-p
    (segments block-start block-end)
  "Return non-nil when SEGMENTS overlap BLOCK-START..BLOCK-END."
  (let (found)
    (while (and segments (not found))
      (let ((seg (car segments)))
        (setq found
              (and (eq (car seg) 'tool)
                   (< (cadr seg) block-end)
                   (> (caddr seg) block-start))))
      (setq segments (cdr segments)))
    found))

(defun mevedel-transcript--tool-block-inside-ignore-segment-p
    (segments block-start block-end)
  "Return non-nil when an ignore entry in SEGMENTS spans BLOCK-START..BLOCK-END."
  (let (found)
    (while (and segments (not found))
      (let ((seg (car segments)))
        (setq found
              (and (eq (car seg) 'ignore)
                   (<= (cadr seg) block-start)
                   (<= block-end (caddr seg)))))
      (setq segments (cdr segments)))
    found))

(defun mevedel-transcript--first-tool-segment-start-after (segments pos limit)
  "Return the first tool segment start in SEGMENTS after POS and before LIMIT."
  (let (found)
    (while (and segments (not found))
      (let ((seg (car segments)))
        (when (and (eq (car seg) 'tool)
                   (> (cadr seg) pos)
                   (< (cadr seg) limit))
          (setq found (cadr seg))))
      (setq segments (cdr segments)))
    found))

(defun mevedel-transcript--org-tool-block-start-p (pos)
  "Return non-nil when POS is at a persisted org tool block start.
Literal `#+begin_tool' text can appear inside tool output.  A real
persisted tool block is followed by the serialized `(:name ...)' tool
plist, so use that as the structural discriminator instead of text
properties, which can be stale after restoring `GPTEL_BOUNDS'."
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (skip-chars-forward " \t\n")
    (looking-at-p "(\\s-*:name\\_>")))

(defun mevedel-transcript--tool-block-bounds-for-run (seg-start seg-end
                                                          &optional limit)
  "Return recovered org tool block bounds for a tool run.
SEG-START and SEG-END are the bounds of an actual `gptel' tool
property run.  Recovery is anchored to that run rather than to every
marker-looking line in the buffer, so transcript-like text inside a
tool result does not create nested fake tool blocks.  LIMIT, when
non-nil, is the start of the next actual tool run."
  (when-let* ((block-start
               (mevedel-transcript--tool-block-start-for-run seg-start seg-end
                                                       limit))
              (block-end
               (mevedel-transcript--tool-block-end-from-start block-start limit
                                                        seg-start)))
    (when (and (< block-start seg-end)
               (> block-end seg-start))
      (cons block-start block-end))))

(defun mevedel-transcript--tool-block-start-for-run (seg-start seg-end
                                                         &optional limit)
  "Return the structural `#+begin_tool' for tool run SEG-START..SEG-END.
LIMIT bounds the backward search when non-nil."
  (let (found)
    (save-excursion
      (goto-char seg-end)
      (while (re-search-backward "^#\\+begin_tool\\b" nil t)
        (let ((candidate (point)))
          (when (and (< candidate seg-end)
                     (mevedel-transcript--org-tool-block-start-p candidate))
            (let ((block-end
                   (mevedel-transcript--tool-block-end-from-start candidate limit)))
              (when (and block-end
                         (or (> block-end seg-start)
                             (let ((retry-min
                                    (or (mevedel-transcript--tool-block-start-before
                                         seg-start block-end)
                                        seg-start)))
                               (mevedel-transcript--tool-block-retry-end
                                candidate limit block-end retry-min))))
                (when (<= block-end seg-start)
                  (let ((retry-min
                         (or (mevedel-transcript--tool-block-start-before
                              seg-start block-end)
                             seg-start)))
                    (setq block-end
                          (mevedel-transcript--tool-block-retry-end
                           candidate limit block-end retry-min))))
                (when (and block-end (> block-end seg-start))
                  ;; Keep scanning backward and retain the earliest
                  ;; containing block.  That rejects nested transcript text
                  ;; when stale bounds start after the nested marker.
                  (setq found candidate))))))))
    found))

(defun mevedel-transcript--tool-block-retry-end (block-start limit block-end
                                                       retry-min)
  "Return an extended structural close for BLOCK-START, or nil.
LIMIT bounds the search when non-nil.  BLOCK-END is the normal close
for BLOCK-START and RETRY-MIN is a later
marker-looking block start where the restored tool run begins.  Recovery
is accepted only when the outer block extends beyond that later block's
own first close, which distinguishes nested-looking tool text from a
completed earlier block followed by a real tool call."
  (when (mevedel-transcript--tool-block-retry-gap-p block-end retry-min)
    (let ((retry-end
           (mevedel-transcript--tool-block-end-from-start block-start limit
                                                    retry-min))
          (inner-close
           (mevedel-transcript--first-tool-close-after retry-min limit)))
      (when (and retry-end inner-close (> retry-end inner-close))
        retry-end))))

(defun mevedel-transcript--tool-block-start-before (pos limit)
  "Return the nearest structural tool block start before POS after LIMIT."
  (let (found)
    (save-excursion
      (goto-char pos)
      (while (and (not found)
                  (re-search-backward "^#\\+begin_tool\\b" limit t))
        (when (mevedel-transcript--org-tool-block-start-p (point))
          (setq found (point)))))
    found))

(defun mevedel-transcript--tool-block-retry-gap-p (block-end min-end)
  "Return non-nil when BLOCK-END..MIN-END resemble tool output.
This gate is used only after the normal structural close for a block
falls before the restored tool run.  It permits the recovery case where
a literal close marker appears inside tool output before a nested-looking
marker, while preventing an earlier completed tool block from swallowing
the next real tool call."
  (and block-end
       min-end
       (< block-end min-end)
       (let ((gap (buffer-substring-no-properties block-end min-end)))
         (and (string-match-p "[^ \t\n]" gap)
              (mevedel-transcript--range-has-gptel-prop-p
               block-end min-end '(tool))
              (not (mevedel-transcript--range-has-gptel-prop-p
                    block-end min-end '(response ignore)))))))

(defun mevedel-transcript--first-tool-close-after (pos &optional limit)
  "Return the first non-response `#+end_tool' marker end after POS.
LIMIT bounds the search when non-nil."
  (let (found)
    (save-excursion
      (goto-char pos)
      (while (and (not found)
                  (re-search-forward "^#\\+end_tool[^\n]*\n?" limit t))
        (let ((marker-start (match-beginning 0))
              (marker-end (match-end 0)))
          (unless (eq (mevedel-transcript--classify-gptel-prop
                       (get-text-property marker-start 'gptel))
                      'response)
            (setq found marker-end)))))
    found))

(defun mevedel-transcript--range-has-gptel-prop-p (start end types)
  "Return non-nil when START..END contain a `gptel' property in TYPES."
  (let (found)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not found))
        (let ((type (mevedel-transcript--classify-gptel-prop
                     (get-text-property (point) 'gptel))))
          (when (memq type types)
            (setq found t))
          (goto-char (or (next-single-property-change (point) 'gptel nil end)
                         end)))))
    found))

(defun mevedel-transcript--blank-gap-p (start end)
  "Return non-nil if START..END is only whitespace."
  (or (>= start end)
      (string-empty-p
       (string-trim
        (buffer-substring-no-properties start end)))))

(defconst mevedel-transcript--mailbox-block-specs
  '((agent-result
     :open "<agent-result\\s-+[^>]*agent-id=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-result>")
    (agent-message
     :open "<agent-message\\s-+[^>]*from=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-message>"))
  "Structural mailbox block regexes.

The open regex captures the agent id in match group 1.")

(defun mevedel-transcript--mailbox-spec (kind key)
  "Return mailbox KIND spec value for KEY."
  (plist-get (cdr (assq kind mevedel-transcript--mailbox-block-specs)) key))

(defun mevedel-transcript--mailbox-open-at-point (kind limit)
  "Return mailbox open metadata for KIND at point before LIMIT.
The returned plist contains `:kind', `:id', `:open-start', and
`:open-end'."
  (let ((regex (mevedel-transcript--mailbox-spec kind :open)))
    (when (and regex
               (<= (point) limit)
               (looking-at regex)
               (<= (match-end 0) limit))
      (list :kind kind
            :id (match-string-no-properties 1)
            :open-start (match-beginning 0)
            :open-end (match-end 0)))))

(defun mevedel-transcript--mailbox-close-line-regexp (close-tag)
  "Return a line-oriented regexp for CLOSE-TAG."
  (concat "^[ \t]*" (regexp-quote close-tag) "[ \t]*\\(?:\n\\|\\'\\)"))

(defun mevedel-transcript--mailbox-nested-open-regexp (close-tag)
  "Return a broad nested mailbox opener regexp for CLOSE-TAG."
  (pcase close-tag
    ("</agent-result>" "<agent-result\\(?:\\s-\\|>\\)")
    ("</agent-message>" "<agent-message\\(?:\\s-\\|>\\)")))

(defun mevedel-transcript--mailbox-structural-close-at (start close-re)
  "Return close bounds when START is on a structural CLOSE-RE line."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (when (looking-at close-re)
      (cons (match-beginning 0) (match-end 0)))))

(defun mevedel-transcript--mailbox-find-close (open-regexp close-tag limit)
  "Return structural close bounds for CLOSE-TAG before LIMIT.
OPEN-REGEXP matches normal nested mailbox openings of the same kind.
The return value is a cons cell `(BODY-END . CLOSE-END)'.  Close
markers are accepted only as standalone lines; matching nested openings
first keeps literal nested mailbox examples from ending the outer card."
  (let* ((close-re (mevedel-transcript--mailbox-close-line-regexp close-tag))
         (loose-close-re (regexp-quote close-tag))
         (nested-open-re (mevedel-transcript--mailbox-nested-open-regexp close-tag))
         (event-re (concat "\\(?:" open-regexp "\\)"
                           (when nested-open-re
                             (concat "\\|\\(?:" nested-open-re "\\)"))
                           "\\|\\(?:" loose-close-re "\\)"))
         (depth 0))
    (catch 'done
      (while (re-search-forward event-re limit t)
        (let ((start (match-beginning 0)))
          (cond
           ((mevedel-transcript--mailbox-structural-close-at start close-re)
            (if (> depth 0)
                (cl-decf depth)
              (throw 'done
                     (mevedel-transcript--mailbox-structural-close-at
                      start close-re))))
           ((save-excursion
              (goto-char start)
              (looking-at loose-close-re))
            (when (> depth 0)
              (cl-decf depth)))
           (t
            (cl-incf depth)))))
      nil)))

(defun mevedel-transcript--mailbox-block-at-point (kind limit)
  "Return mailbox block metadata for KIND at point before LIMIT.
The returned plist includes open metadata plus `:body-start',
`:body-end', and `:close-end'."
  (let ((open (mevedel-transcript--mailbox-open-at-point kind limit))
        close)
    (when open
      (save-excursion
        (goto-char (plist-get open :open-end))
        (setq close
              (mevedel-transcript--mailbox-find-close
               (mevedel-transcript--mailbox-spec kind :open)
               (mevedel-transcript--mailbox-spec kind :close)
               limit)))
      (when close
        (append open
                (list :body-start (plist-get open :open-end)
                      :body-end (car close)
                      :close-end (cdr close)))))))

(defun mevedel-transcript--mailbox-any-block-at-point (limit)
  "Return mailbox block metadata at point before LIMIT, or nil."
  (or (mevedel-transcript--mailbox-block-at-point 'agent-result limit)
      (mevedel-transcript--mailbox-block-at-point 'agent-message limit)))

(defun mevedel-transcript--mailbox-start-in-range-p (start end)
  "Return non-nil if a mailbox opening tag appears in START..END."
  (save-excursion
    (goto-char start)
    (or (re-search-forward "<agent-result\\(?:\\s-\\|>\\)" end t)
        (progn
          (goto-char start)
          (re-search-forward "<agent-message\\(?:\\s-\\|>\\)" end t)))))

(defun mevedel-transcript--tool-block-gap-crosses-boundary-p (start end)
  "Return non-nil when START..END crosses non-tool conversation content."
  (and (< start end)
       (or (mevedel-transcript--range-has-gptel-prop-p start end
                                                 '(response ignore))
           (mevedel-transcript--mailbox-start-in-range-p start end)
           (save-excursion
             (goto-char start)
             (re-search-forward
              "^\\(?:<system-reminder>\\|<queued-user-message-batch\\_>\\|<hook-context>\\)"
              end t)))))

(defun mevedel-transcript--tool-block-truncated-before-p (start end)
  "Return non-nil if a mevedel truncation marker appears in START..END."
  (and (< start end)
       (save-excursion
         (goto-char start)
         (re-search-forward
          "\\[mevedel: tool output truncated; omitted [0-9]+ chars\\]"
          end t))))

(defun mevedel-transcript--tool-block-call-readable-before-p (start end)
  "Return non-nil when START..END begins with a readable tool call plist."
  (and (< start end)
       (condition-case nil
           (save-restriction
             (narrow-to-region start end)
             (save-excursion
               (goto-char start)
               (forward-line 1)
               (skip-chars-forward " \t\n")
               (let ((sexp (read (current-buffer))))
                 (and (listp sexp)
                      (stringp (plist-get sexp :name))))))
         (error nil))))

(defun mevedel-transcript--tool-block-end-from-start (block-start &optional limit
                                                            min-end)
  "Return the structural close for the tool block at BLOCK-START.
LIMIT bounds the search when non-nil.
The close is the last non-response `#+end_tool' marker before the next
structural `#+begin_tool' that appears after at least one close marker.
That separates adjacent persisted tools while preserving marker-looking
text inside a tool result.  If MIN-END is non-nil, ignore closes at or
before MIN-END when deciding whether a following begin marker starts the
next persisted tool."
  (let (last-close done)
    (save-excursion
      (goto-char block-start)
      (forward-line 1)
      (while (and (not done)
                  (re-search-forward "^#\\+\\(begin_tool\\b\\|end_tool[^\n]*\n?\\)"
                                     limit t))
        (let ((marker-start (match-beginning 0))
              (marker-end (match-end 0)))
          (cond
           ((and (not last-close)
                 (save-excursion
                   (goto-char marker-start)
                   (looking-at-p "^#\\+begin_tool\\b"))
                 (mevedel-transcript--org-tool-block-start-p marker-start)
                 (mevedel-transcript--range-has-gptel-prop-p
                  block-start marker-start '(tool))
                 (not (mevedel-transcript--same-tool-run-before-p
                       marker-start block-start))
                 (or (mevedel-transcript--tool-block-truncated-before-p
                      block-start marker-start)
                     (not (mevedel-transcript--tool-block-call-readable-before-p
                           block-start marker-start))))
            (setq last-close marker-start
                  done t))
           ((and last-close
                 (or (not min-end) (> last-close min-end))
                 (save-excursion
                   (goto-char marker-start)
                   (looking-at-p "^#\\+begin_tool\\b"))
                 (mevedel-transcript--org-tool-block-start-p marker-start)
                 (or (not (mevedel-transcript--same-tool-run-before-p
                           marker-start block-start))
                     (mevedel-transcript--tool-block-gap-crosses-boundary-p
                      last-close marker-start)
                     (mevedel-transcript--blank-gap-p
                      last-close marker-start)))
            (setq done t))
           ((save-excursion
              (goto-char marker-start)
              (looking-at-p "^#\\+end_tool"))
            (let ((marker-type
                   (mevedel-transcript--classify-gptel-prop
                    (get-text-property marker-start 'gptel))))
              (unless (and (eq marker-type 'response)
                           (or (mevedel-transcript--range-has-gptel-prop-p
                                (or last-close block-start)
                                marker-start '(response ignore))
                               (not (or
                                     (mevedel-transcript--range-has-gptel-prop-p
                                      (or last-close block-start)
                                      marker-start '(tool))
                                     (and last-close
                                          (mevedel-transcript--gap-body-text-p
                                           last-close marker-start))))))
                (setq last-close marker-end))))))))
    last-close))

(defun mevedel-transcript--same-tool-run-before-p (pos limit)
  "Return non-nil if POS has the same tool prop as prior text after LIMIT."
  (let ((prop (get-text-property pos 'gptel))
        found)
    (when (eq (mevedel-transcript--classify-gptel-prop prop) 'tool)
      (save-excursion
        (goto-char pos)
        (while (and (> (point) limit) (not found))
          (backward-char 1)
          (unless (memq (char-after) '(?\s ?\t ?\n ?\r))
            (setq found (equal (get-text-property (point) 'gptel)
                               prop))))))
    found))

(defun mevedel-transcript--gap-body-text-p (start end)
  "Return non-nil when START..END resemble unclassified tool body text."
  (and (< start end)
       (not (mevedel-transcript--range-has-gptel-prop-p
             start end '(response ignore)))
       (string-match-p "[^ \t\n]"
                       (buffer-substring-no-properties start end))))

(defun mevedel-transcript--normalize-tool-block-segments (segments start end)
  "Return SEGMENTS with overlapping org tool blocks made canonical.
START and END are the requested data-buffer range.  Each org tool
block overlapping that range becomes one `tool' segment covering the
whole block; property runs that only contain pieces of the block marker
or tool sexp are dropped.  Text outside tool blocks keeps its original
classification."
  (let ((blocks (mevedel-transcript--org-tool-blocks-overlapping segments start end))
        out)
    (dolist (block blocks)
      (let ((block-start (car block))
            (block-end (cdr block)))
        (while (and segments (<= (caddr (car segments)) block-start))
          (push (pop segments) out))
        (when (and segments (< (cadr (car segments)) block-start))
          (let ((seg (car segments)))
            (push (list (car seg) (cadr seg) block-start) out)))
        (while (and segments (< (cadr (car segments)) block-end))
          (let ((seg (car segments)))
            (setq segments (cdr segments))
            (when (> (caddr seg) block-end)
              (setq segments
                    (cons (list (if (eq (car seg) 'tool)
                                    'user
                                  (car seg))
                                block-end (caddr seg))
                          segments)))))
        (push (list 'tool block-start block-end) out)))
    (nconc (nreverse out) segments)))

(defun mevedel-transcript--leading-structural-user-tail-start (start end)
  "Return assistant-tail start after leading structural user blocks.
START..END is a nil-`gptel' segment.  Returns nil unless the segment
begins with one or more complete mailbox/reminder blocks followed by
non-whitespace prose."
  (let ((patterns '(("<system-reminder>" . "</system-reminder>")
                    ("<queued-user-message-batch\\_>" . "</queued-user-message-batch>")
                    ("<hook-context>" . "</hook-context>")))
        last-block-end done)
    (save-excursion
      (goto-char start)
      (while (and (not done) (< (point) end))
        (skip-chars-forward " \t\n\r" end)
        (let (matched)
          (cond
           ((setq matched
                  (mevedel-transcript--mailbox-any-block-at-point end))
            (goto-char (plist-get matched :close-end))
            (setq last-block-end (point)
                  matched t))
           (t
            (dolist (pattern patterns)
              (when (and (not matched)
                         (looking-at-p (car pattern)))
                (setq matched t)
                (if (search-forward (cdr pattern) end t)
                    (progn
                      (when (and (< (point) end)
                                 (eq (char-after) ?\n))
                        (forward-char 1))
                      (setq last-block-end (point)))
                  (setq done t))))))
          (unless matched
            (setq done t))))
      (when last-block-end
        (goto-char last-block-end)
        (skip-chars-forward " \t\n\r" end)
        (when (and (< (point) end)
                   (string-match-p
                    "[^ \t\n\r]"
                    (buffer-substring-no-properties (point) end)))
          (point))))))

(defun mevedel-transcript--response-continuation-gap-p (start end)
  "Return non-nil if START..END is a response prefix gap."
  (and (< start end)
       (not (memq (char-before end) '(?\n ?\r)))
       (string-match-p
        "[^ \t\n\r]"
        (buffer-substring-no-properties start end))))

(defun mevedel-transcript--split-structural-user-response-prefixes (segments)
  "Split assistant response prefixes out of structural user SEGMENTS.
Stale restored bounds can leave an `<agent-result>' block and the first
characters of the following assistant text in one nil-property segment,
with the next segment marked `response'.  Split that mid-line prefix so
the normal response merger can keep the assistant turn intact."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let* ((seg (car rest))
             (next (cadr rest))
             (tail-start
              (and (eq (car seg) 'user)
                   (eq (car-safe next) 'response)
                   (mevedel-transcript--leading-structural-user-tail-start
                    (cadr seg) (caddr seg)))))
        (if (and tail-start
                 (< tail-start (caddr seg))
                 (mevedel-transcript--response-continuation-gap-p
                  tail-start (caddr seg)))
            (progn
              (when (< (cadr seg) tail-start)
                (push (list 'user (cadr seg) tail-start) out))
              (push (list 'response tail-start (caddr seg)) out))
          (push seg out)))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun mevedel-transcript--repair-response-fragment-segments (segments)
  "Return SEGMENTS with stale response fragments reclassified.
Older or externally edited transcripts can restore `GPTEL_BOUNDS' a
few characters into an assistant response, leaving the leading text as
a nil-property `user' segment between a tool and a response.  Such
fragments should render as part of the assistant response, not as a
fake thinking block or user turn."
  (let (converted prev-type rest)
    (setq rest (mevedel-transcript--merge-adjacent-segments segments '(user)))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (next-type (car-safe (cadr rest)))
             (prev-seg (car converted))
             (convert-p
              (and (eq type 'user)
                   (or (and (memq prev-type '(tool ignore))
                            (eq next-type 'response)
                            (mevedel-transcript--response-fragment-segment-p
                             seg (cadr rest)))
                       (mevedel-transcript--response-continuation-segment-p
                        prev-seg seg (cadr rest))))))
        (push (if convert-p
                  (list 'response (cadr seg) (caddr seg))
                seg)
              converted)
        (setq prev-type (if convert-p 'response type))
        (setq rest (cdr rest))))
    (mevedel-transcript--merge-adjacent-segments (nreverse converted) '(response))))

(defun mevedel-transcript--response-fragment-segment-p (seg next-seg)
  "Return non-nil when SEG resembles a stale prefix of NEXT-SEG.
This is deliberately conservative so a real user prompt in a
`tool -> user -> response' sequence does not get swallowed into the
assistant turn."
  (and next-seg
       (let* ((text (buffer-substring-no-properties (cadr seg) (caddr seg)))
              (trimmed (string-trim text))
              (next-text
               (buffer-substring-no-properties (cadr next-seg)
                                               (caddr next-seg)))
              (next-trimmed (string-trim-left next-text)))
         (and (not (string-empty-p trimmed))
              (<= (length text) 120)
              (not (string-match-p "\n" text))
              (not (string-match-p "\\`\\(?:\\*+\\|#+\\|[-+*]\\)[ \t]"
                                   trimmed))
              (not (mevedel-transcript--scaffolding-only-p
                    (current-buffer) (cadr seg) (caddr seg)))
              (not (string-empty-p next-trimmed))
              (let ((ch (aref next-trimmed 0)))
                (or (and (>= ch ?a) (<= ch ?z))
                    (memq ch '(?, ?. ?\; ?: ?\) ?\] ?\}))))))))

(defun mevedel-transcript--response-continuation-segment-p (prev-seg seg next-seg)
  "Return non-nil when SEG continues PREV-SEG and NEXT-SEG mid-line."
  (and prev-seg
       next-seg
       (eq (car prev-seg) 'response)
       (eq (car seg) 'user)
       (eq (car next-seg) 'response)
       (< (cadr seg) (caddr seg))
       (> (cadr seg) (point-min))
       (not (memq (char-before (cadr seg)) '(?\n ?\r)))
       (mevedel-transcript--response-continuation-text-p
        (buffer-substring-no-properties (cadr seg) (caddr seg)))))

(defun mevedel-transcript--response-continuation-text-p (text)
  "Return non-nil when TEXT can be repaired as response continuation."
  (let ((trimmed (string-trim text)))
    (and (not (string-empty-p trimmed))
         ;; A trailing newline can be same-line response glue, but a
         ;; new nonblank line may be the next real prompt.
         (not (string-match-p "[\n\r][ \t\r\n]*[^ \t\r\n]" text))
         (not (string-match-p "\\`\\(?:\\*+\\|#+\\|[-+*]\\)[ \t]"
                              trimmed)))))

(defun mevedel-transcript--merge-adjacent-segments (segments types)
  "Merge contiguous SEGMENTS whose type is a member of TYPES."
  (let (out)
    (dolist (seg segments)
      (let ((prev (car out)))
        (if (and prev
                 (memq (car prev) types)
                 (eq (car prev) (car seg))
                 (= (caddr prev) (cadr seg)))
            (setcar out (list (car seg) (cadr prev) (caddr seg)))
          (push seg out))))
    (nreverse out)))


(provide 'mevedel-transcript)
;;; mevedel-transcript.el ends here
