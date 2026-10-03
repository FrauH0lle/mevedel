;;; mevedel-transcript.el --- Transcript span classification -*- lexical-binding: t -*-

;;; Commentary:

;; Canonical classification and property restoration for the gptel data
;; buffer transcript.  Callers decide how to render, persist, or compact the
;; resulting structural spans.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)

;; `mevedel-ptc-interpreter'
(declare-function mevedel-ptc-state-direct-tool "mevedel-ptc-interpreter" (state))
(autoload 'mevedel-ptc-close "mevedel-ptc-interpreter")
(autoload 'mevedel-ptc-start "mevedel-ptc-interpreter")

;; `mevedel-tool-media'
(declare-function mevedel-tool-media-extract
                  "mevedel-tool-media"
                  (result-string &optional tool-results-dir
                                 expected-tool-use-id
                                 allow-payload-tool-use-id))
(autoload 'mevedel-tool-media-extract "mevedel-tool-media")

;; `mevedel-tool-render-data'
(declare-function mevedel-tool-render-data-trusted-range-p
                  "mevedel-tool-render-data" (start end &optional object))

;; `mevedel-transcript-audit'
(declare-function mevedel-transcript-audit-buffer-spans
                  "mevedel-transcript-audit" (&optional type start end))
(declare-function mevedel-transcript-audit-trusted-range-p
                  "mevedel-transcript-audit" (start end &optional object))

;; `org'
(declare-function org-entry-get
                  "ext:org" (pom property &optional inherit literal-nil))

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

(defun mevedel-transcript-leading-property (name)
  "Return property NAME's value from the current buffer's leading drawer.
Read the top-level `:PROPERTIES:' drawer directly: `org-entry-get' at the
buffer start builds Org element context, which took 11 ms per save on a long
transcript to read one small value."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at-p ":PROPERTIES:$")
        (let ((end (save-excursion
                     (and (re-search-forward "^:END:[ \t]*$" nil t)
                          (match-beginning 0)))))
          (when (and end
                     (re-search-forward
                      (concat "^:" (regexp-quote name) ":[ \t]+\\(.*?\\)[ \t]*$")
                      end t))
            (match-string-no-properties 1)))))))

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
    (while-let ((range (mevedel-transcript--system-reminder-range-at-point
                        (point-max))))
      (goto-char (nth 3 range))
      (skip-chars-forward " \t\r\n"))
    (point)))

(defun mevedel-transcript-prompt-transform-start ()
  "Return the start of the last user prompt in a transform buffer."
  (let ((pos (point-max)))
    (while (and (> pos (point-min))
                (let ((prop (get-text-property (1- pos) 'gptel)))
                  (cond
                   ((memq prop '(ignore mevedel-render-data
                                 mevedel-hook-audit))
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
    ((or 'ignore 'mevedel-render-data 'mevedel-hook-audit) 'ignored)
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

(defun mevedel-transcript--counted-tool-marker-p (marker-pos)
  "Return non-nil when the `#+begin_tool' at MARKER-POS is structural.
A genuine tool block's marker either carries a `gptel' text property (a
live response or a normalized transcript) or is followed by the
serialized `(:name ...)' tool plist.  Literal `#+begin_tool' typed
inside a user prompt has neither; counting it would hide every later
user prompt from the prompt index and from compaction turn boundaries."
  (or (get-text-property marker-pos 'gptel)
      (mevedel-transcript--org-tool-block-start-p marker-pos)))

(defun mevedel-transcript--org-block-depth-before
    (pos block-re &optional start depth)
  "Return nesting depth before POS for org blocks matching BLOCK-RE.

BLOCK-RE should match the suffix after `#+begin_' / `#+end_', for
example `tool\\|reasoning'.  START and DEPTH resume a pass that already
counted up to START, so a caller walking a transcript forward pays for
each region once instead of rescanning from the beginning per turn.

The marker is recognized with leading whitespace allowed, which is the
rule the span walk below already applies and the spelling gptel's own org
support accepts."
  (save-excursion
    (save-restriction
      (widen)
      (let ((depth (or depth 0))
            (regexp (format "^[ \t]*#\\+\\(begin\\|end\\)_\\(%s\\)\\b"
                            block-re)))
        (goto-char (or start (point-min)))
        (while (re-search-forward regexp pos t)
          (cond
           ((equal (match-string 1) "end")
            (setq depth (max 0 (1- depth))))
           ((or (not (equal (match-string 2) "tool"))
                (mevedel-transcript--counted-tool-marker-p
                 (match-beginning 0)))
            (cl-incf depth))))
        depth))))

(defun mevedel-transcript-prompt-scan-state ()
  "Return carried state for a forward pass of prompt-start scans.
A caller that walks segments in order threads one of these through
`mevedel-transcript--user-prompt-start' so the block-depth prefix is
counted once for the pass rather than once per turn."
  (list (point-min) 0 0))

(defun mevedel-transcript--user-prompt-start (pos next prop &optional state)
  "Return the real PROP prompt start in [POS, NEXT), or nil.

Nil-`gptel' regions can contain a mixture of org block glue and the
next user prompt, for example `#+end_tool' / reasoning text /
`#+end_reasoning' / user text.  Scan line-wise so callers agree on the
first non-empty line outside gptel-owned org blocks.

STATE is optional carried state from `mevedel-transcript-prompt-scan-state'.
A caller that walks segments forward threads one through, which counts the
block-depth prefix once for the pass; without it each call counts from the
beginning of the buffer."
  (when (null prop)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((from (if state (nth 0 state) (point-min)))
               (tool-depth
                (mevedel-transcript--org-block-depth-before
                 pos "tool\\|reasoning" from (and state (nth 1 state))))
               (summary-depth
                (mevedel-transcript--org-block-depth-before
                 pos "summary" from (and state (nth 2 state)))))
          (when state
            (setf (nth 0 state) pos
                  (nth 1 state) tool-depth
                  (nth 2 state) summary-depth))
          (goto-char pos)
          (catch 'found
            (while (< (point) next)
              (let* ((line-start (point))
                     (line-end (min next (line-end-position)))
                     (line (buffer-substring-no-properties
                            line-start line-end)))
                (cond
                 ((string-match
                   "\\`[ \t]*#\\+begin_\\(tool\\|reasoning\\)\\b"
                   line)
                  (when (or (not (equal (match-string 1 line) "tool"))
                            (mevedel-transcript--counted-tool-marker-p
                             line-start))
                    (cl-incf tool-depth)))
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


;;
;;; Canonical structure

(defun mevedel-transcript--tool-id-in-range (start end)
  "Return the first gptel tool id in START..END, or nil."
  (let ((pos start)
        id)
    (while (and (< pos end) (not id))
      (let ((prop (get-text-property pos 'gptel)))
        (when (and (consp prop) (eq (car prop) 'tool))
          (setq id (cdr prop))))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    id))

(defun mevedel-transcript--tool-bound-id (start end)
  "Return a persisted tool id overlapping START..END, or nil."
  (when (derived-mode-p 'org-mode)
    (when-let* ((raw (org-entry-get (point-min) "GPTEL_BOUNDS"))
                (bounds (condition-case nil (read raw) (error nil))))
      (catch 'found
        (dolist (range (alist-get 'tool bounds))
          (when (and (integerp (car-safe range))
                     (integerp (cadr range))
                     (stringp (caddr range))
                     (not (string-empty-p (caddr range)))
                     (< (car range) end)
                     (> (cadr range) start))
            (throw 'found (caddr range))))))))

(defun mevedel-transcript--org-tool-block-parts (start end)
  "Return readable and scaffold subranges for the tool block START..END."
  (save-excursion
    (goto-char start)
    (when (looking-at-p "#\\+begin_tool\\b")
      (forward-line 1)
      (skip-chars-forward " \t\n" end)
      (let ((tool-start (point)))
        (when (and (< tool-start end)
                   (looking-at-p "(\\s-*:name\\_>"))
          (condition-case nil
              (progn
                ;; Scan the plist with Lisp syntax.  Org gives backslash
                ;; symbol syntax rather than escape syntax, so an escaped
                ;; quote inside a serialized argument would close the string
                ;; early, desynchronize the scan, and make a well-formed tool
                ;; block look malformed.
                (with-syntax-table emacs-lisp-mode-syntax-table
                  (forward-sexp 1))
                (let ((sexp-end (point)))
                  (when (re-search-forward "^#\\+end_tool[^\n]*\n?" end t)
                    (let ((tool-end (match-beginning 0))
                          (suffix-end (match-end 0)))
                      (when (<= sexp-end tool-end)
                        (list :prefix-start start
                              :prefix-end tool-start
                              :tool-start tool-start
                              :tool-end tool-end
                              :suffix-start tool-end
                              :suffix-end suffix-end))))))
            (error nil)))))))

(defvar mevedel-transcript--control-lines nil
  "Candidate control-line positions shared within one structural scan.")

(defconst mevedel-transcript--control-line-regexp
  (concat "^\\(?:#\\+\\(?:begin_\\|end_\\)"
          "\\|<\\(?:/?\\(?:system-reminder\\|hook-context"
          "\\|task-background\\|agent-message\\|agent-result\\)"
          "\\|!-- /?mevedel-\\)\\|\\*+ <\\|:PROMPT:\\|:END:\\)")
  "Candidate prefix regexp shared by direct and resumable scans.")

(defun mevedel-transcript--control-line-positions ()
  "Return a vector of candidate control lines in the accessible buffer.
Index prefixes only; the existing parsers still validate each complete marker."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let (positions)
        (while (re-search-forward mevedel-transcript--control-line-regexp nil t)
          (push (match-beginning 0) positions))
        (vconcat (nreverse positions))))))

(defconst mevedel-transcript--control-kinds
  '((begin-reasoning . "#\\+begin_reasoning\\b")
    (end-reasoning . "#\\+end_reasoning")
    (begin-tool . "#\\+begin_tool\\b")
    (end-tool . "#\\+end_tool")
    (reminder . "\\(?:\\*+ \\)?</?system-reminder>")
    (mailbox . "\\(?:\\*+ \\)?<agent-\\(?:result\\|message\\)")
    (hook-context . "</?hook-context>")
    (task-background . "</?task-background>")
    (render-data . "<!-- /?mevedel-render-data -->")
    (hook-audit . "<!-- /?mevedel-hook-audit -->")
    (prompt . ":PROMPT:")
    (end . ":END:"))
  "Mutually exclusive control line kinds, as line-start prefix regexps.")

(defconst mevedel-transcript--control-kind-regexp
  (let ((group 0))
    (mapconcat (lambda (kind)
                 (format "\\(?%d:%s\\)" (setq group (1+ group)) (cdr kind)))
               mevedel-transcript--control-kinds "\\|"))
  "Classify an indexed control line by the group of its kind.")

(defconst mevedel-transcript--control-search-kinds
  '(("^#\\+begin_reasoning\\b" begin-reasoning)
    ("^#\\+end_reasoning[^\n]*\n?" end-reasoning)
    ("^#\\+begin_tool\\b" begin-tool)
    ("^#\\+end_tool[^\n]*\n?" end-tool)
    ("^#\\+\\(begin_tool\\b\\|end_tool[^\n]*\n?\\)" begin-tool end-tool)
    ("^\\(?:\\*+ \\)?<system-reminder>[ \t]*$" reminder)
    ("^\\(?:\\(?:\\*+ \\)?<system-reminder>[ \t]*\\|</system-reminder>[ \t]*\\)\\(?:\n\\|\\'\\)"
     reminder)
    ("^\\(?:\\*+ \\)?<\\(?:agent-result\\|agent-message\\)\\(?:\\s-\\|>\\)" mailbox)
    ("^<hook-context>[ \t]*$" hook-context)
    ("^</hook-context>[ \t]*\n?" hook-context)
    ("^<task-background>[ \t]*$" task-background)
    ("^</task-background>[ \t]*\n?" task-background)
    ("^<!-- mevedel-render-data -->[ \t]*$" render-data)
    ("^<!-- /mevedel-render-data -->[ \t]*\\(?:\n\\(?:[ \t\r]*\n\\)*\\)?"
     render-data)
    ("^:PROMPT:[ \t]*$" prompt)
    ("^:END:[ \t]*\n?" end)
    ("^<!-- mevedel-hook-audit -->[ \t]*$" hook-audit)
    ("^<!-- /mevedel-hook-audit -->[ \t]*\\(?:\n[ \t\r]*\\)*" hook-audit))
  "Control searches whose every match starts a line of one of the listed kinds.
Other searches visit every indexed line.")

(defvar mevedel-transcript--control-line-kinds
  (make-hash-table :test #'eq :weakness 'key)
  "Control line indexes to their lines grouped by search regexp.
Every structural scan runs each control search over the whole transcript.
Testing each search's regexp at every indexed line cost a pass per search;
classifying each line once lets a search visit only lines of its kinds.")

(defun mevedel-transcript--control-search-lines (regexp)
  "Return the indexed control lines REGEXP can match, in source order."
  (if-let* ((kinds (cdr (assoc regexp mevedel-transcript--control-search-kinds))))
      (let ((table
             (or (gethash mevedel-transcript--control-lines
                          mevedel-transcript--control-line-kinds)
                 (let ((grouped (make-hash-table :test #'equal)))
                   (save-excursion
                     (let (by-kind)
                       (cl-loop for position across mevedel-transcript--control-lines
                                do (goto-char position)
                                (when (looking-at mevedel-transcript--control-kind-regexp)
                                  (let ((kind (cl-loop for index from 1
                                                       for entry in mevedel-transcript--control-kinds
                                                       when (match-beginning index)
                                                       return (car entry))))
                                    (push position (alist-get kind by-kind)))))
                       (pcase-dolist (`(,kind . ,positions) by-kind)
                         (puthash kind (vconcat (nreverse positions)) grouped))))
                   (puthash mevedel-transcript--control-lines grouped
                            mevedel-transcript--control-line-kinds)))))
        (or (gethash regexp table)
            (puthash regexp
                     (if (cdr kinds)
                         (vconcat (sort (apply #'append
                                               (mapcar (lambda (kind)
                                                         (append (gethash kind table) nil))
                                                       kinds))
                                        #'<))
                       (or (gethash (car kinds) table) []))
                     table)))
    mevedel-transcript--control-lines))

(defun mevedel-transcript--search-control-line (regexp limit)
  "Search forward for control REGEXP before LIMIT, returning its end or nil.
Use the current structural scan's line index when available.  Preserve native
search bounds and match data, including a match ending partway through a line."
  (if (not mevedel-transcript--control-lines)
      (re-search-forward regexp limit t)
    (let* ((origin (point))
           (limit (or limit (point-max)))
           (lines (mevedel-transcript--control-search-lines regexp))
           (low 0) (high (length lines)) found)
      (while (< low high)
        (let ((middle (/ (+ low high) 2)))
          (if (< (aref lines middle) origin)
              (setq low (1+ middle))
            (setq high middle))))
      (while (and (< low (length lines))
                  (< (aref lines low) limit)
                  (not found))
        (goto-char (aref lines low))
        (when (looking-at-p regexp)
          (setq found (re-search-forward regexp limit t)))
        (setq low (1+ low)))
      (unless found (goto-char origin))
      found)))

(defun mevedel-transcript--system-reminder-range-at-point (limit)
  "Return complete system-reminder bounds at point before LIMIT.
The result is `(START BODY-START BODY-END END)'.  Literal complete reminders
nested inside the outer body do not close it early."
  (when (looking-at "^\\(?:\\*+ \\)?<system-reminder>[ \t]*$")
    (let ((origin (point))
          (start (match-beginning 0))
          (body-start (progn (forward-line 1) (point)))
          (depth 1)
          body-end
          block-end)
      (while (and (> depth 0)
                  (mevedel-transcript--search-control-line
                   "^\\(?:\\(?:\\*+ \\)?<system-reminder>[ \t]*\\|</system-reminder>[ \t]*\\)\\(?:\n\\|\\'\\)"
                   limit))
        (if (save-excursion
              (goto-char (match-beginning 0))
              (looking-at "\\(?:\\*+ \\)?<system-reminder>"))
            (cl-incf depth)
          (cl-decf depth)
          (when (zerop depth)
            (setq body-end (match-beginning 0)
                  block-end (match-end 0)))))
      (if block-end
          (list start body-start body-end block-end)
        (goto-char origin)
        nil))))

(defun mevedel-transcript--structure-priority (range)
  "Return overlay priority for structural RANGE."
  (pcase (car range)
    ('tool 40)
    ((or 'mailbox 'reminder 'hook-context 'prompt) 30)
    ('reasoning 20)
    ((or 'render-data 'ignored) 10)
    (_ 0)))

(defun mevedel-transcript--property-priority (range)
  "Return property-application priority for structural RANGE."
  (pcase (car range)
    ((or 'render-data 'ignored) 40)
    ('tool 30)
    ('reasoning 20)
    (_ 10)))

(defun mevedel-transcript--base-segment-type (start base-segments)
  "Return the type of the raw property run in BASE-SEGMENTS holding START."
  (car-safe
   (cl-find-if
    (lambda (seg) (and (<= (cadr seg) start) (< start (caddr seg))))
    base-segments)))

(defun mevedel-transcript--control-prefix-p (range base-segments accepted)
  "Return non-nil when RANGE follows only ACCEPTED control structure.
BASE-SEGMENTS delimit the containing raw property run."
  (let* ((start (cadr range))
         (base (cl-find-if
                (lambda (seg)
                  (and (<= (cadr seg) start) (< start (caddr seg))))
                base-segments))
         (cursor (and base (cadr base)))
         ok)
    (setq ok (numberp cursor))
    ;; Generated control blocks reach the transcript in their own
    ;; `ignore' run (or unpropertized, once normalized), never inside
    ;; gptel's streamed `response' run.  So a response run gets the same
    ;; head-of-run proof user prose gets: markup the model merely quoted
    ;; stays prose instead of collapsing into a control row.
    (when (and base (not (memq (car base) '(user response))))
      (setq cursor start))
    (dolist (prior (sort (copy-sequence accepted)
                         (lambda (a b) (< (cadr a) (cadr b)))))
      (when (and ok
                 (< (cadr prior) start)
                 (> (caddr prior) cursor))
        (when (and (> (cadr prior) cursor)
                   (string-match-p
                    "[^ \t\r\n]"
                    (buffer-substring-no-properties
                     cursor (min start (cadr prior)))))
          (setq ok nil))
        (setq cursor (max cursor (min start (caddr prior))))))
    (and ok
         (not (string-match-p
               "[^ \t\r\n]"
               (buffer-substring-no-properties cursor start))))))

(defun mevedel-transcript--mailbox-control-context-p (range base-segments)
  "Return non-nil when mailbox RANGE is outside an Org user heading.
BASE-SEGMENTS supplies the raw property span containing RANGE."
  (let* ((start (cadr range))
         (base (cl-find-if
                (lambda (seg)
                  (and (<= (cadr seg) start) (< start (caddr seg))))
                base-segments)))
    (and base
         (not (string-match-p
               "^\\*+ "
               (buffer-substring-no-properties (cadr base) start))))))

(defun mevedel-transcript--unparseable-tool-ranges
    (start end base-segments tool-ranges)
  "Return stale tool blocks in START..END absent from TOOL-RANGES.
BASE-SEGMENTS supplies the raw property spans used to identify closed
blocks that still carry stale tool properties."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (mevedel-transcript--search-control-line "^#\\+begin_tool\\b" end)
        (let ((block-start (match-beginning 0)))
          ;; Candidates advance in source order. Earlier disjoint property
          ;; runs and validated blocks cannot prove this or any later block.
          (while (and base-segments
                      (<= (caddr (car base-segments)) block-start))
            (setq base-segments (cdr base-segments)))
          (while (and tool-ranges
                      (<= (caddr (car tool-ranges)) block-start))
            (setq tool-ranges (cdr tool-ranges)))
          (if (mevedel-transcript--search-control-line "^#\\+end_tool[^\n]*\n?" end)
              (let ((block-end (match-end 0)))
                (when (and
                       (mevedel-transcript--tool-block-overlaps-tool-segment-p
                        base-segments block-start block-end)
                       (not (when-let* ((range (car tool-ranges)))
                              (and (<= (cadr range) block-start)
                                   (<= block-end (caddr range))))))
                  (push (list 'ignored block-start block-end) ranges)))
            (goto-char (1+ block-start))))))
    (nreverse ranges)))

(defconst mevedel-transcript--opaque-payload-types
  '(render-data reasoning mailbox prompt)
  "Control ranges whose payload is authored text rather than structure.
A control marker that starts inside one of these is quoted content, so
`mevedel-transcript--structural-ranges' refuses to carve it out.  Tool
blocks are left out because they already win by overlay priority and
legitimately neighbour their own render-data.")

(defconst mevedel-transcript--self-proving-types
  '(tool render-data ignored)
  "Control ranges carrying their own proof of being generated.
A tool range is validated against the raw `gptel' tool property runs,
and render-data and hook-audit ranges against their trust hash, so these
stay valid inside a container payload: backends really do nest a tool
call inside a reasoning block, and a tool block really does carry its
own render-data.")

(defconst mevedel-transcript--range-scans
  '((reasoning "^#\\+begin_reasoning\\b" "^#\\+end_reasoning[^\n]*\n?")
    (mailbox)
    (reminder)
    (hook-context "^<hook-context>[ \t]*$" "^</hook-context>[ \t]*\n?")
    (task-background "^<task-background>[ \t]*$" "^</task-background>[ \t]*\n?")
    (render-data "^<!-- mevedel-render-data -->[ \t]*$"
                 "^<!-- /mevedel-render-data -->[ \t]*\\(?:\n\\(?:[ \t\r]*\n\\)*\\)?")
    (prompt "^:PROMPT:[ \t]*$" "^:END:[ \t]*\n?")
    (ignored "^<!-- mevedel-hook-audit -->[ \t]*$"
             "^<!-- /mevedel-hook-audit -->[ \t]*\\(?:\n[ \t\r]*\\)*"))
  "Canonical control searches in structural precedence order.")

(defun mevedel-transcript--next-raw-range (spec pos end)
  "Return (NEXT . RANGE) for the next SPEC candidate from POS to END.
RANGE is nil when a malformed opener was skipped.  Return nil only when
there are no more candidates.  Search for closes against the complete
source, not a work-slice boundary."
  (save-excursion
    (goto-char pos)
    (pcase (car spec)
      ('reminder
       (when (mevedel-transcript--search-control-line
              "^\\(?:\\*+ \\)?<system-reminder>[ \t]*$" end)
         (goto-char (match-beginning 0))
         (let ((start (point))
               (range (mevedel-transcript--system-reminder-range-at-point end)))
           (if range
               (cons (nth 3 range) (list 'reminder start (nth 3 range)))
             (cons (1+ start) nil)))))
      ('mailbox
       (when (mevedel-transcript--search-control-line
              "^\\(?:\\*+ \\)?<\\(?:agent-result\\|agent-message\\)\\(?:\\s-\\|>\\)"
              end)
         (let ((start (match-beginning 0)))
           (goto-char start)
           (search-forward "<" end)
           (backward-char 1)
           (let ((block (mevedel-transcript--mailbox-any-block-at-point end)))
             (if block
                 (cons (plist-get block :close-end)
                       (list 'mailbox start (plist-get block :close-end)))
               (cons (1+ start) nil))))))
      (_
       (when (mevedel-transcript--search-control-line (cadr spec) end)
         (let ((start (match-beginning 0))
               (next (match-end 0)))
           (if (mevedel-transcript--search-control-line (caddr spec) end)
               (let ((finish (match-end 0)))
                 (cons finish (list (car spec) start finish)))
             (cons next nil))))))))

(defun mevedel-transcript--raw-ranges (start end)
  "Return canonical candidate ranges in START..END before validation."
  (let (ranges)
    (dolist (spec mevedel-transcript--range-scans)
      (let ((pos start) next)
        (while (setq next (mevedel-transcript--next-raw-range spec pos end))
          (setq pos (car next))
          (when (cdr next)
            (push (cdr next) ranges)))))
    (nreverse ranges)))

(defun mevedel-transcript--structural-ranges (start end base-segments
                                                    &optional prepared-ranges)
  "Return canonical control ranges in START..END.
BASE-SEGMENTS are raw `gptel' property runs used to validate persisted
tool blocks.  Each result is `(TYPE START END VALUE...)'.
PREPARED-RANGES, when non-nil, is a cons whose cdr holds candidates
collected by a resumable scan of this same source."
  (let* ((mevedel-transcript--control-lines
          (or mevedel-transcript--control-lines
              (mevedel-transcript--control-line-positions)))
         (ranges (if prepared-ranges (cdr prepared-ranges)
                   (mevedel-transcript--raw-ranges start end))))
    (let (tool-ranges)
      (dolist (block (mevedel-transcript--org-tool-blocks-overlapping
                      base-segments start end))
        (push (list 'tool (car block) (cdr block)
                    (or (mevedel-transcript--tool-id-in-range
                         (car block) (cdr block))
                        (mevedel-transcript--tool-bound-id
                         (car block) (cdr block))
                        ""))
              tool-ranges))
      (setq tool-ranges (nreverse tool-ranges))
      (setq ranges
            (append ranges tool-ranges
                    (mevedel-transcript--unparseable-tool-ranges
                     start end base-segments tool-ranges))))
    (let ((base-segments base-segments)
          accepted payloads)
      (dolist (range (sort ranges (lambda (a b) (< (cadr a) (cadr b)))))
        ;; Both lists advance in source order.  Prefixes before this range
        ;; cannot contain it and need not be searched again.
        (while (and base-segments
                    (<= (caddr (car base-segments)) (cadr range)))
          (setq base-segments (cdr base-segments)))
        (let ((audit-p
               (and (eq (car range) 'ignored)
                    (save-excursion
                      (goto-char (cadr range))
                      (looking-at-p "<!-- mevedel-hook-audit -->"))))
              (render-p (eq (car range) 'render-data))
              ;; Generated control blocks reach the transcript in their
              ;; own run, never inside gptel's streamed `response' run,
              ;; so markup found there is prose the model quoted.  Tool
              ;; blocks are exempt: they are already validated against
              ;; the raw tool property runs.
              (quoted-in-response-p
               (and (eq (mevedel-transcript--base-segment-type
                         (cadr range) base-segments)
                        'response)
                    (not (eq (car range) 'tool))
                    (not (mevedel-transcript--control-prefix-p
                          range base-segments accepted)))))
          (when (or (and (not quoted-in-response-p)
                         (not (memq (car range) '(mailbox reminder)))
                         (not audit-p)
                         (not render-p))
                    (and audit-p
                         (save-excursion
                           (goto-char (cadr range))
                           (when (search-forward
                                  "<!-- /mevedel-hook-audit -->"
                                  (caddr range) t)
                             (mevedel-transcript-audit-trusted-range-p
                              (cadr range) (point)))))
                    (and render-p
                         (mevedel-tool-render-data-trusted-range-p
                          (cadr range) (caddr range)))
                  (and (eq (car range) 'mailbox)
                       (mevedel-transcript--mailbox-control-context-p
                        range base-segments))
                  (and (eq (car range) 'mailbox)
                       (cl-find-if (lambda (prior)
                                     (eq (car prior) 'mailbox))
                                   accepted))
                  (and (not audit-p)
                       (not render-p)
                       (mevedel-transcript--control-prefix-p
                        range base-segments accepted)))
            ;; Container payloads carry authored text: a render-data
            ;; block quotes a whole prepared prompt, reasoning and
            ;; mailbox bodies are model- and agent-authored, and a
            ;; prompt drawer holds the user's directive.  A marker
            ;; starting inside one is quoted data, not transcript
            ;; structure.  Ranges are visited in start order and a
            ;; container never starts after its own contents, so the
            ;; enclosing payload is already recorded here.
            (unless (and (not (memq (car range)
                                    mevedel-transcript--self-proving-types))
                         (cl-find-if
                          (lambda (span)
                            (and (> (cadr range) (car span))
                                 (< (cadr range) (cdr span))))
                          payloads))
              (when (memq (car range)
                          mevedel-transcript--opaque-payload-types)
                (push (cons (cadr range) (caddr range)) payloads))
              (push range accepted)))))
      (setq ranges (nreverse accepted)))
    (sort ranges
          (lambda (a b)
            (let ((pa (mevedel-transcript--structure-priority a))
                  (pb (mevedel-transcript--structure-priority b)))
              (if (= pa pb)
                  (< (cadr a) (cadr b))
                (< pa pb)))))))

(defun mevedel-transcript--overlay-range (segments range)
  "Overlay canonical RANGE on ordered role SEGMENTS.
Reuse the untouched suffix without modifying SEGMENTS."
  (let ((start (cadr range))
        (end (caddr range))
        (segment (list (car range) (cadr range) (caddr range)))
        out inserted)
    (while (and segments (< (cadr (car segments)) end))
      (let* ((seg (pop segments))
             (seg-start (cadr seg))
             (seg-end (caddr seg)))
        (cond
         ((<= seg-end start)
          (push seg out))
         (t
          (when (< seg-start start)
            (push (list (car seg) seg-start start) out))
          (unless inserted
            (push segment out)
            (setq inserted t))
          (when (> seg-end end)
            (push (list (if (and (eq (car range) 'tool)
                                 (eq (car seg) 'tool))
                            'user
                          (car seg))
                        end seg-end)
                  out))))))
    (unless inserted
      (push segment out))
    (nconc (nreverse out) segments)))

(defun mevedel-transcript--overlay-ranges (segments ranges)
  "Overlay structural RANGES, in precedence order, on ordered SEGMENTS.
Advance through the unchanged prefix while range starts increase; restart
when a later precedence class returns to earlier source.  Metadata inside
a tool remains part of that tool.  Do not modify either input list."
  (let* ((head (cons nil (copy-sequence segments)))
         (cursor head)
         (previous-start 0))
    (dolist (range ranges)
      (let ((start (cadr range)))
        (when (< start previous-start)
          (setq cursor head))
        (setq previous-start start)
        (while (and (cdr cursor) (<= (caddr (cadr cursor)) start))
          (setq cursor (cdr cursor)))
        (let ((segment (cadr cursor)))
          (unless (and (memq (car range) '(render-data ignored))
                       (eq (car segment) 'tool)
                       (<= (cadr segment) start)
                       (<= (caddr range) (caddr segment)))
            (setcdr cursor
                    (mevedel-transcript--overlay-range (cdr cursor) range))))))
    (cdr head)))

(defun mevedel-transcript--property-segments (start end)
  "Return raw `gptel' property segments in START..END."
  (let (segments seg-start seg-type)
    (save-excursion
      (setq start (or (previous-single-property-change
                       (min (1+ start) (point-max)) 'gptel nil (point-min))
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
            (push (list seg-type seg-start next) segments)
            (setq seg-start next
                  seg-type (mevedel-transcript--classify-gptel-prop
                            (get-text-property next 'gptel))))))
      (when (< seg-start end)
        (push (list seg-type seg-start end) segments)))
    (nreverse segments)))

(cl-defstruct (mevedel-transcript--scan
               (:constructor mevedel-transcript--make-scan))
  "One full-source canonical segmentation pass."
  buffer tick low high start end phase pos type properties controls
  control-lines raw-index raw-pos raw-ranges result)

(defun mevedel-transcript-scan-start (start end)
  "Start a resumable canonical scan between START and END in this buffer.
Capture the effective narrowing; later steps restore that restriction even
when the caller widened the source buffer.  Callers must discard a job
after any source text or property change."
  (let* ((low (point-min))
         (high (point-max))
         (from (or (previous-single-property-change
                    (min (1+ start) high) 'gptel nil low)
                   low)))
    (setq from (mevedel-transcript--skip-leading-properties-drawer from)
          from (mevedel-transcript--skip-leading-summary-block from))
    (mevedel-transcript--make-scan
     :buffer (current-buffer) :tick (buffer-modified-tick)
     :low low :high high :start start :end end
     :phase 'properties :pos from :type (mevedel-transcript--classify-gptel-prop
                                        (get-text-property from 'gptel))
     :properties nil :controls nil)))

(defun mevedel-transcript-scan-cancel (job)
  "Release retained intermediate data in transcript scan JOB."
  (setf (mevedel-transcript--scan-phase job) 'cancelled
        (mevedel-transcript--scan-buffer job) nil
        (mevedel-transcript--scan-properties job) nil
        (mevedel-transcript--scan-controls job) nil
        (mevedel-transcript--scan-control-lines job) nil
        (mevedel-transcript--scan-raw-ranges job) nil
        (mevedel-transcript--scan-result job) nil)
  nil)

(defun mevedel-transcript-scan-result (job)
  "Return JOB's canonical segments once it is complete, or nil."
  (when (eq (mevedel-transcript--scan-phase job) 'done)
    (mevedel-transcript--scan-result job)))

(defun mevedel-transcript--scan-properties-step (job)
  "Advance JOB by at most 128 complete gptel property runs."
  (let ((pos (mevedel-transcript--scan-pos job))
        (to (or (next-single-property-change
                 (mevedel-transcript--scan-end job) 'gptel nil (point-max))
                (point-max)))
        (count 0))
    (while (and (< pos to) (< count 128))
      (let ((next (next-single-property-change pos 'gptel nil to)))
        (push (list (mevedel-transcript--scan-type job) pos next)
              (mevedel-transcript--scan-properties job))
        (setq pos next)
        (when (< pos to)
          (setf (mevedel-transcript--scan-type job)
                (mevedel-transcript--classify-gptel-prop
                 (get-text-property pos 'gptel))))
        (cl-incf count)))
    (setf (mevedel-transcript--scan-pos job) pos)
    (when (>= pos to)
      (setf (mevedel-transcript--scan-properties job)
            (nreverse (mevedel-transcript--scan-properties job))
            (mevedel-transcript--scan-start job)
            (if (mevedel-transcript--scan-properties job)
                (cadr (car (mevedel-transcript--scan-properties job)))
              (mevedel-transcript--scan-start job))
            (mevedel-transcript--scan-end job)
            (if (mevedel-transcript--scan-properties job) to
              (mevedel-transcript--scan-end job))
            (mevedel-transcript--scan-pos job) (point-min)
            (mevedel-transcript--scan-phase job) 'controls))))

(defun mevedel-transcript--scan-controls-step (job)
  "Index a bounded source span and at most 128 control prefixes for JOB."
  (save-excursion
    (goto-char (mevedel-transcript--scan-pos job))
    (let* ((count 0)
           (limit (min (point-max) (+ (point) 16384)))
           (finished nil))
      (while (and (< count 128)
                  (re-search-forward mevedel-transcript--control-line-regexp
                                     limit t))
        (push (match-beginning 0) (mevedel-transcript--scan-controls job))
        (cl-incf count))
      ;; Search limits may cut across the fixed candidate prefix.  Revisit
      ;; the final 64 characters on the next step, without repeating matches
      ;; already found in this one.  Every indexed match ends before LIMIT.
      (setq finished (and (< count 128) (= limit (point-max))))
      (setf (mevedel-transcript--scan-pos job)
            (if (= count 128) (point)
              (max (point) (min limit
                                (max (mevedel-transcript--scan-pos job)
                                     (- limit 64))))))
      (when finished
        (setf (mevedel-transcript--scan-control-lines job)
              (vconcat (nreverse (mevedel-transcript--scan-controls job)))
              (mevedel-transcript--scan-controls job) nil
              (mevedel-transcript--scan-pos job)
              (mevedel-transcript--scan-start job)
              (mevedel-transcript--scan-raw-pos job)
              (mevedel-transcript--scan-start job)
              (mevedel-transcript--scan-phase job) 'raw)))))

(defun mevedel-transcript--scan-raw-step (job)
  "Collect at most 32 complete control candidates for JOB."
  (let* ((spec (nth (or (mevedel-transcript--scan-raw-index job) 0)
                    mevedel-transcript--range-scans))
         (pos (mevedel-transcript--scan-raw-pos job))
         (count 0)
         next)
    (if (null spec)
        (setf (mevedel-transcript--scan-phase job) 'finish)
      (while (and (< count 32)
                  (setq next (mevedel-transcript--next-raw-range
                              spec pos (mevedel-transcript--scan-end job))))
        (setq pos (car next))
        (when (cdr next)
          (push (cdr next) (mevedel-transcript--scan-raw-ranges job)))
        (cl-incf count))
      (setf (mevedel-transcript--scan-raw-pos job) pos)
      (unless next
        (setf (mevedel-transcript--scan-raw-index job)
              (1+ (or (mevedel-transcript--scan-raw-index job) 0))
              (mevedel-transcript--scan-raw-pos job)
              (mevedel-transcript--scan-start job))))))

(defun mevedel-transcript-scan-step (job)
  "Advance JOB by one cooperative unit; return non-nil when complete.
All steps run in JOB's original source buffer and effective narrowing.
Tool-block recovery, trust validation, and final neighbor repairs are
currently atomic stages; a single exceptionally large block can exceed
the caller's timer budget."
  (pcase (mevedel-transcript--scan-phase job)
    ('done t)
    ('cancelled (error "Transcript scan cancelled"))
    (_
     (let ((source (mevedel-transcript--scan-buffer job)))
       (unless (buffer-live-p source)
         (error "Transcript scan source is dead"))
       (with-current-buffer source
         (unless (eql (buffer-modified-tick) (mevedel-transcript--scan-tick job))
           (error "Transcript scan source changed"))
         (save-excursion
           (save-restriction
             (widen)
             (narrow-to-region (mevedel-transcript--scan-low job)
                               (mevedel-transcript--scan-high job))
             (let ((mevedel-transcript--control-lines
                    (mevedel-transcript--scan-control-lines job)))
               (pcase (mevedel-transcript--scan-phase job)
                 ('properties (mevedel-transcript--scan-properties-step job))
                 ('controls (mevedel-transcript--scan-controls-step job))
                 ('raw (mevedel-transcript--scan-raw-step job))
                 ('finish
                  (let* ((segments (mevedel-transcript--scan-properties job))
                         (start (mevedel-transcript--scan-start job))
                         (end (mevedel-transcript--scan-end job))
                         (ranges (mevedel-transcript--structural-ranges
                                  start end segments
                                  (cons t (nreverse
                                           (mevedel-transcript--scan-raw-ranges
                                            job)))))
                         (result (mevedel-transcript--finish-segments
                                  segments ranges start end)))
                    (setf (mevedel-transcript--scan-result job) result
                          (mevedel-transcript--scan-phase job) 'done
                          (mevedel-transcript--scan-properties job) nil
                          (mevedel-transcript--scan-controls job) nil
                          (mevedel-transcript--scan-raw-ranges job) nil
                          (mevedel-transcript--scan-control-lines job) nil))))))))
       (eq (mevedel-transcript--scan-phase job) 'done)))))

(defun mevedel-transcript-segments (start end)
  "Return canonical transcript segments between START and END.
Each segment is `(TYPE START END)'.  Structural control ranges override
stale `gptel' runs; incomplete controls remain ordinary transcript text."
  (let ((job (mevedel-transcript-scan-start start end)))
    (unwind-protect
        (progn
          (while (not (mevedel-transcript-scan-step job)))
          (mevedel-transcript-scan-result job))
      (mevedel-transcript-scan-cancel job))))

(defun mevedel-transcript--finish-segments (segments ranges scan-start scan-end)
  "Apply RANGES and canonical neighbor repairs to SEGMENTS in source bounds."
  (let ((segments (mevedel-transcript--overlay-ranges segments ranges)))
    (dolist (span
             (mevedel-transcript-audit-buffer-spans 'fork-point scan-start scan-end))
      (let ((prompt-start (plist-get span :end))
            prompt-end)
        (dolist (segment segments)
          (when (and (>= (cadr segment) prompt-start)
                     (memq (car segment)
                           '(response tool reasoning mailbox reminder
                             hook-context render-data prompt))
                     (or (null prompt-end) (< (cadr segment) prompt-end)))
            (setq prompt-end (cadr segment))))
        (when (and prompt-end
                   (string-match-p
                    "[^ \t\r\n]"
                    (buffer-substring-no-properties prompt-start prompt-end)))
          (setq segments
                (mevedel-transcript--overlay-range
                 segments (list 'user prompt-start prompt-end))))))
    (mevedel-transcript--merge-adjacent-segments
     (mevedel-transcript--repair-response-fragment-segments
      (mevedel-transcript--repair-orphan-mailbox-tail-segments
       (mevedel-transcript--repair-mailbox-prose-segments
        (mevedel-transcript--absorb-structural-whitespace segments))))
     '(ignored))))

(defun mevedel-transcript--whitespace-segment-p (seg)
  "Return non-nil if SEG is entirely whitespace."
  (not (string-match-p
        "[^ \t\r\n]"
        (buffer-substring-no-properties (cadr seg) (caddr seg)))))

(defun mevedel-transcript--absorb-structural-whitespace (segments)
  "Attach whitespace-only property fragments to adjacent control SEGMENTS."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let ((seg (car rest))
            (next (cadr rest)))
        (if (and next
                 (memq (car seg) '(user ignored))
                 (mevedel-transcript--whitespace-segment-p seg)
                 (memq (car next)
                       '(tool reasoning mailbox reminder hook-context
                         render-data prompt ignored)))
            (progn
              (setcar (cdr rest)
                      (list (car next) (cadr seg) (caddr next)))
              (setq rest (cdr rest)))
          (push seg out)
          (setq rest (cdr rest)))))
    (nreverse out)))

(defun mevedel-transcript--repair-mailbox-prose-segments (segments)
  "Classify plain prose between mailbox SEGMENTS as assistant response."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let ((seg (car rest)))
        (push (if (and (eq (car seg) 'user)
                       (eq (car-safe (car out)) 'mailbox)
                       (eq (car-safe (cadr rest)) 'mailbox)
                       (string-match-p
                        "[^ \t\r\n]"
                        (buffer-substring-no-properties
                         (cadr seg) (caddr seg))))
                  (list 'response (cadr seg) (caddr seg))
                seg)
              out))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun mevedel-transcript--repair-orphan-mailbox-tail-segments (segments)
  "Classify orphan agent-result tails in SEGMENTS as ignored control text."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let* ((seg (car rest))
             (text (and (eq (car seg) 'user)
                        (buffer-substring-no-properties
                         (cadr seg) (caddr seg)))))
        (push (if (and text
                       (eq (car-safe (car out)) 'tool)
                       (eq (car-safe (cadr rest)) 'reasoning)
                       (string-match-p
                        "^[ \t]*</agent-result>[ \t]*\\(?:\r?\n\\)?\\'"
                        text))
                  (list 'ignored (cadr seg) (caddr seg))
                seg)
              out))
      (setq rest (cdr rest)))
    (nreverse out)))

(defun mevedel-transcript--clear-gptel-properties (start end)
  "Clear stale gptel-related text properties from START to END."
  (remove-text-properties
   start end
   '(gptel nil response nil invisible nil front-sticky nil)))

(defun mevedel-transcript--apply-structural-properties (ranges)
  "Apply canonical `gptel' properties for structural RANGES."
  (dolist (range (sort (copy-sequence ranges)
                       (lambda (a b)
                         (< (mevedel-transcript--property-priority a)
                            (mevedel-transcript--property-priority b)))))
    (pcase-let ((`(,type ,start ,end . ,values) range))
      (mevedel-transcript--clear-gptel-properties start end)
      (pcase type
        ('tool
         (if-let* ((parts (mevedel-transcript--org-tool-block-parts
                           start end)))
             (progn
               (put-text-property
                (plist-get parts :prefix-start)
                (plist-get parts :prefix-end) 'gptel 'ignore)
               (put-text-property
                (plist-get parts :tool-start)
                (plist-get parts :tool-end)
                'gptel (cons 'tool (or (car values) "")))
               (put-text-property
                (plist-get parts :suffix-start)
                (plist-get parts :suffix-end) 'gptel 'ignore))
           (put-text-property start end 'gptel 'ignore)))
        ('render-data
         (add-text-properties
          start end '(gptel mevedel-render-data mevedel-render-data t)))
        ('ignored
         (if (save-excursion
               (goto-char start)
               (looking-at-p "<!-- mevedel-hook-audit -->"))
             (add-text-properties
              start end '(gptel mevedel-hook-audit mevedel-hook-audit t))
           (put-text-property start end 'gptel 'ignore)))
        ((or 'reasoning 'prompt)
         (put-text-property start end 'gptel 'ignore))))))

(defconst mevedel-transcript--response-continuation-max-gap 160
  "Maximum structural-to-response prefix size repaired after restore.")

(defun mevedel-transcript--structural-gap-prop-p (prop type)
  "Return non-nil when PROP is stale structural state after TYPE."
  (or (null prop)
      (pcase type
        ('tool
         (or (eq prop 'tool)
             (and (consp prop) (eq (car prop) 'tool))))
        ((or 'reasoning 'prompt)
         (eq prop 'ignore))
        ('render-data
         (eq prop 'mevedel-render-data))
        ('ignored
         (memq prop '(ignore mevedel-hook-audit)))
        (_ nil))))

(defun mevedel-transcript--first-nonblank-pos (start end)
  "Return the first non-whitespace position in START..END, or nil."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\n\r" end)
    (when (< (point) end)
      (point))))

(defun mevedel-transcript--response-continuation-range (start type)
  "Return a stale response prefix range after structural START and TYPE."
  (let ((limit (min (point-max)
                    (+ start mevedel-transcript--response-continuation-max-gap)))
        (pos start)
        prefix-start response-start done)
    (while (and (< pos limit) (not response-start) (not done))
      (let* ((prop (get-text-property pos 'gptel))
             (next (or (next-single-property-change pos 'gptel nil limit)
                       limit)))
        (cond
         ((eq prop 'response)
          (setq response-start pos))
         ((mevedel-transcript--structural-gap-prop-p prop type)
          (unless prefix-start
            (setq prefix-start
                  (mevedel-transcript--first-nonblank-pos pos next))))
         (t
          (setq done t)))
        (setq pos next)))
    (when (and prefix-start
               response-start
               (< prefix-start response-start)
               (not (memq (char-before response-start) '(?\n ?\r)))
               (null (mevedel-transcript--structural-ranges
                      prefix-start response-start
                      (mevedel-transcript--property-segments
                       prefix-start response-start)))
               (mevedel-transcript--response-continuation-text-p
                (buffer-substring-no-properties prefix-start response-start)))
      (cons prefix-start response-start))))

(defun mevedel-transcript--repair-response-continuation-properties (ranges)
  "Repair stale response prefixes immediately after structural RANGES."
  (dolist (range ranges)
    (pcase-let ((`(,type ,_start ,end . ,_) range))
      (when (< end (point-max))
        (when-let* ((repair
                     (mevedel-transcript--response-continuation-range
                      end type)))
          (add-text-properties
           (car repair) (cdr repair)
           '(gptel response front-sticky (gptel))))))))

(defvar-local mevedel-transcript--normalized-tick nil
  "Modification tick after the last successful whole-buffer normalization.
Text and property edits both invalidate this derived state.")

(defun mevedel-transcript-normalize-properties ()
  "Normalize structural transcript properties in the current Org buffer.
Reuse normalization until buffer text or properties change."
  (when (and (derived-mode-p 'org-mode)
             (not (eql mevedel-transcript--normalized-tick
                       (buffer-modified-tick))))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (with-silent-modifications
            (let* ((drawer-end
                    (mevedel-transcript--skip-leading-properties-drawer
                     (point-min)))
                   (base (mevedel-transcript--property-segments
                          (point-min) (point-max)))
                   (ranges (mevedel-transcript--structural-ranges
                            (point-min) (point-max) base)))
              (when (> drawer-end (point-min))
                (mevedel-transcript--clear-gptel-properties
                 (point-min) drawer-end))
              (mevedel-transcript--apply-structural-properties ranges)
              (mevedel-transcript--repair-response-continuation-properties
               ranges))))))
    (setq mevedel-transcript--normalized-tick (buffer-modified-tick))))

(defun mevedel-transcript-restore-ignored-properties (start end)
  "Restore ignored side-channel properties within START..END."
  (save-match-data
    (let* ((base (mevedel-transcript--property-segments start end))
           (ranges (mevedel-transcript--structural-ranges start end base)))
      (mevedel-transcript--apply-structural-properties
       (cl-remove-if-not (lambda (range)
                           (memq (car range) '(render-data ignored)))
                         ranges)))))



(defun mevedel-transcript--org-tool-blocks-overlapping (segments start end)
  "Return org tool block bounds from SEGMENTS overlapping START..END.
Bounds include the `#+begin_tool' and `#+end_tool' marker lines.  The
view uses these structural anchors to repair stale restored
`GPTEL_BOUNDS' that split a single tool block across several property
runs."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (mevedel-transcript--search-control-line "^#\\+begin_tool\\b" end)
        ;; Keep the containing property run, but discard completed prefixes.
        ;; Raw property segments are disjoint and ordered by source position.
        (while (and segments
                    (<= (caddr (car segments)) (match-beginning 0)))
          (setq segments (cdr segments)))
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
  "Return non-nil when ordered SEGMENTS overlap BLOCK-START..BLOCK-END."
  (catch 'overlap
    (while (and segments (< (cadr (car segments)) block-end))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'tool) (> (caddr seg) block-start))
          (throw 'overlap t))))))

(defun mevedel-transcript--tool-block-inside-ignore-segment-p
    (segments block-start block-end)
  "Return non-nil when ordered SEGMENTS has an ignore entry spanning the block.
The block is bounded by BLOCK-START and BLOCK-END."
  (catch 'contains
    (while (and segments (<= (cadr (car segments)) block-start))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'ignored) (<= block-end (caddr seg)))
          (throw 'contains t))))))

(defun mevedel-transcript--first-tool-segment-start-after (segments pos limit)
  "Return the first tool start in ordered SEGMENTS after POS and before LIMIT."
  (catch 'start
    (while (and segments (< (cadr (car segments)) limit))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'tool) (> (cadr seg) pos))
          (throw 'start (cadr seg)))))))

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

(defvar mevedel-transcript--tool-block-index nil
  "Buffer-keyed canonical boundary indexes scoped to one projection.")

(defun mevedel-transcript--tool-block-bounds-for-run (beg end &optional limit)
  "Return canonical tool bounds overlapping BEG..END, respecting LIMIT.
Within a projection use a lazily built index, invalidated by text or property
changes and narrowing.  Incomplete structural ranges use anchored recovery,
bounded by the preceding validated block so a miss never rescans that history."
  (if (not mevedel-transcript--tool-block-index)
      (mevedel-transcript--recover-tool-block-bounds beg end limit)
    (let* ((key (list (buffer-modified-tick) (point-min) (point-max)))
           (cached (gethash (current-buffer) mevedel-transcript--tool-block-index)))
      (unless (equal key (car cached))
        (setq cached
              (cons key (vconcat (mevedel-transcript--org-tool-blocks-overlapping
                                 (mevedel-transcript--property-segments (point-min) (point-max))
                                 (point-min) (point-max)))))
        (puthash (current-buffer) cached mevedel-transcript--tool-block-index))
      (let* ((blocks (cdr cached)) (lo 0) (hi (length blocks)))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi) 2)))
            (if (<= (cdr (aref blocks mid)) beg) (setq lo (1+ mid)) (setq hi mid))))
        (let ((block (and (< lo (length blocks)) (aref blocks lo))))
          (if (and block (< (car block) end) (or (not limit) (<= (cdr block) limit)))
              block
            (save-excursion
              (save-restriction
                (when (> lo 0)
                  (narrow-to-region (cdr (aref blocks (1- lo))) (point-max)))
                (mevedel-transcript--recover-tool-block-bounds beg end limit)))))))))

(defun mevedel-transcript--recover-tool-block-bounds (seg-start seg-end
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
       (save-excursion
         (save-match-data
           (goto-char block-end)
           (and (re-search-forward "[^ \t\n]" min-end t)
                (mevedel-transcript--range-has-gptel-prop-p
                 block-end min-end '(tool))
                (not (mevedel-transcript--range-has-gptel-prop-p
                      block-end min-end '(response ignored))))))))

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
     :open "<agent-result\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-result>")
    (agent-message
     :open "<agent-message\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-message>"))
  "Structural mailbox block regexes.

The open regex captures the canonical sender path in match group 1.")

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
                                                 '(response ignored))
           (mevedel-transcript--mailbox-start-in-range-p start end)
           (save-excursion
             (goto-char start)
             (re-search-forward
              "^\\(?:<system-reminder>\\|<hook-context>\\)"
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
           ;; Narrowing clamps point into the region, and the restriction
           ;; is lifted before an inner `save-excursion' could undo that.
           ;; Callers scan forward from point, so the outer form is what
           ;; keeps this predicate from rewinding their loop.
           (save-excursion
             (save-restriction
               (narrow-to-region start end)
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
                  (mevedel-transcript--search-control-line "^#\\+\\(begin_tool\\b\\|end_tool[^\n]*\n?\\)"
                                     limit))
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
                                marker-start '(response ignored))
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
             start end '(response ignored)))
       (string-match-p "[^ \t\n]"
                       (buffer-substring-no-properties start end))))


(defun mevedel-transcript--repair-response-fragment-segments (segments)
  "Return SEGMENTS with stale response fragments reclassified.
Older or externally edited transcripts can restore `GPTEL_BOUNDS' a
few characters into or before the end of an assistant response, leaving
response text as nil-property `user' or `ignored' segments.  Such
fragments should render as part of the assistant response, not as fake
thinking blocks or user turns."
  (let (converted prev-type rest)
    (setq rest (mevedel-transcript--merge-adjacent-segments segments '(user)))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (next-type (car-safe (cadr rest)))
             (prev-seg (car converted))
             (following (cdr rest))
             (render-data-after-tail-p
              (progn
                (while (and following
                            (memq (caar following) '(user ignored)))
                  (setq following (cdr following)))
                (eq (caar following) 'render-data)))
             (convert-p
              (or (and (eq prev-type 'response)
                       (memq type '(user ignored))
                       render-data-after-tail-p
                       ;; Audits emitted after the final response are
                       ;; records, not a stale tail of its text.
                       (not (string-match-p
                             "\\`[ \t\r\n]*<!-- mevedel-hook-audit -->"
                             (buffer-substring-no-properties
                              (cadr seg) (caddr seg)))))
                  (and (eq type 'user)
                       (or (and (eq prev-type 'response)
                                (eq next-type 'ignored)
                                ;; Guest attribution follows a real prompt too.
                                (not (string-match-p
                                      "\\`[ \t\r\n]*\\*+ "
                                      (buffer-substring-no-properties
                                       (cadr seg) (caddr seg))))
                                (string-match-p
                                 "\\`[ \t\r\n]*<!-- mevedel-hook-audit -->"
                                 (buffer-substring-no-properties
                                  (cadr (cadr rest))
                                  (caddr (cadr rest)))))
                       (and (memq prev-type
                                  '(tool reasoning mailbox reminder
                                    render-data ignored))
                            (eq next-type 'response)
                            (mevedel-transcript--response-fragment-segment-p
                             seg (cadr rest)))
                       (mevedel-transcript--response-continuation-segment-p
                        prev-seg seg (cadr rest)))))))
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


;;
;;; Context-summary evidence

(defun mevedel-transcript--summary-truncation-marker (omitted)
  "Return the evidence marker for OMITTED tool-result characters."
  (format "\n[mevedel: tool output truncated; omitted %d chars]\n" omitted))

(defun mevedel-transcript--summary-truncate-string (string limit)
  "Return tool argument STRING shortened to LIMIT characters."
  (if (> (length string) limit)
      (concat (substring string 0 limit)
              (format "\n[mevedel: string argument truncated; omitted %d chars]"
                      (- (length string) limit)))
    string))

(defun mevedel-transcript--summary-truncate-args (value limit)
  "Return tool argument VALUE with strings shortened to LIMIT characters."
  (cond
   ((stringp value)
    (mevedel-transcript--summary-truncate-string value limit))
   ((vectorp value)
    (vconcat
     (mapcar (lambda (item)
               (mevedel-transcript--summary-truncate-args item limit))
             value)))
   ((consp value)
    (cons (mevedel-transcript--summary-truncate-args (car value) limit)
          (mevedel-transcript--summary-truncate-args (cdr value) limit)))
   (t value)))

(defun mevedel-transcript--summary-tool-sexp-start (text)
  "Return the readable tool-call plist start in TEXT, or nil."
  (if (string-prefix-p "#+begin_tool" text)
      (when-let* ((header-end (string-match "\n" text)))
        (let ((pos (1+ header-end)))
          (while (and (< pos (length text))
                      (memq (aref text pos) '(?\s ?\t ?\n)))
            (setq pos (1+ pos)))
          (when (and (string-match "(\\s-*:name\\_>" text pos)
                     (= (match-beginning 0) pos))
            pos)))
    (when (string-match "\\`[ \t\n]*(\\s-*:name\\_>" text)
      (match-beginning 0))))

(defun mevedel-transcript--summary-media-placeholder (item)
  "Return a textual context-summary placeholder for native media ITEM."
  (format "[media: %s; MIME %s%s]"
          (or (plist-get item :kind) "unknown")
          (or (plist-get item :mime) "unknown")
          (if-let* ((path (plist-get item :path)))
              (format "; path %s" path)
            "")))

(defun mevedel-transcript--instruction-tool-p (form)
  "Return non-nil if transcript call FORM delivers a Skill instruction body.
Interpret no guest code.  Validate a ToolCall expression using the closed
parser so quoted names or composed results are not mistaken for instructions."
  (or (equal (plist-get form :name) "Skill")
      (and (equal (plist-get form :name) "ToolCall")
           (condition-case nil
               (let ((state (mevedel-ptc-start
                             (plist-get (plist-get form :args) :expression)
                             '("Skill") '("Skill"))))
                 (unwind-protect
                     (equal "Skill" (mevedel-ptc-state-direct-tool state))
                   (mevedel-ptc-close state)))
             (error nil)))))

(defun mevedel-transcript--summary-tool-parts
    (text cap tool-results-dir tool-id)
  "Return neutral call/result evidence parts for tool TEXT.
CAP bounds ordinary result characters; Skill instruction results stay complete.
TOOL-RESULTS-DIR and TOOL-ID resolve
trusted native media metadata without returning payload bytes."
  (let* ((extracted
          (mevedel-tool-media-extract
           text tool-results-dir tool-id t))
         (visible (car extracted))
         (media (cdr extracted))
         (sexp-start (mevedel-transcript--summary-tool-sexp-start visible))
         call result)
    (when sexp-start
      (condition-case nil
          (let* ((read-result (read-from-string visible sexp-start))
                 (form (car read-result))
                 (form-end (cdr read-result)))
            (when (and (listp form) (stringp (plist-get form :name)))
              (setq call
                    (prin1-to-string
                     (mevedel-transcript--summary-truncate-args
                      form (max 80 (min 800 cap)))))
              (let* ((close (string-match "\n#\\+end_tool[^\n]*\n?\\'"
                                          visible form-end))
                     (body (string-trim
                            (substring visible form-end
                                       (or close (length visible))))))
                (setq result
                      ;; Skill results carry authored guidance, also after
                      ;; cold restore has lost the live invocation records.
                      (if (and (not (mevedel-transcript--instruction-tool-p form))
                               (> (length body) cap))
                          (concat
                           (substring body 0 cap)
                           (mevedel-transcript--summary-truncation-marker
                            (- (length body) cap)))
                        body)))))
        (error nil)))
    (list
     (or call "(unparseable tool-call metadata)")
     (string-join
      (delq nil
            (append
             (and result (not (string-empty-p result)) (list result))
             (mapcar #'mevedel-transcript--summary-media-placeholder media)))
      "\n"))))

(defun mevedel-transcript--summary-evidence-item (provenance text)
  "Return one neutral evidence item labelled with PROVENANCE and TEXT."
  (format "--- evidence item; provenance: %s ---\n%s\n--- end evidence item ---"
          provenance (string-trim text)))

(cl-defun mevedel-transcript-project-evidence
    (ranges &key (tool-output-max 8000) tool-results-dir skill-provenance)
  "Project selected transcript RANGES into frozen neutral evidence.

RANGES is an ordered list of buffer position conses.  TOOL-OUTPUT-MAX bounds
ordinary tool results while preserving readable call metadata.  Skill results
retain their complete instruction text as evidence.  TOOL-RESULTS-DIR
allows native media references to resolve to textual kind, MIME, and path
placeholders.  SKILL-PROVENANCE is the already selected list of prior skill
invocation descriptions."
  (unless (and (listp ranges)
               (seq-every-p
                (lambda (range)
                  (and (consp range)
                       (integer-or-marker-p (car range))
                       (integer-or-marker-p (cdr range))
                       (<= (car range) (cdr range))))
                ranges))
    (error "Invalid transcript evidence ranges"))
  (mevedel-transcript-project-segments
   (cl-loop for (begin . finish) in ranges
            append (cl-loop for (type start end) in
                            (mevedel-transcript-segments begin finish)
                            collect (list type (max begin start) (min finish end))))
   :tool-output-max tool-output-max
   :tool-results-dir tool-results-dir
   :skill-provenance skill-provenance))

(cl-defun mevedel-transcript-project-segments
    (segments &key (tool-output-max 8000) tool-results-dir skill-provenance)
  "Format already classified SEGMENTS from the current buffer as evidence.

SEGMENTS uses the canonical (TYPE START END) representation returned by
`mevedel-transcript-segments'. Callers may select spans before formatting;
this function never classifies them again. TOOL-OUTPUT-MAX bounds ordinary
tool results, TOOL-RESULTS-DIR resolves media, and SKILL-PROVENANCE records
previously selected skill invocations. These options have the same meaning
as in `mevedel-transcript-project-evidence'."
  (unless (natnump tool-output-max)
    (error "Tool output maximum must be non-negative"))
  (let (items)
    (dolist (segment segments)
      (pcase-let ((`(,type ,start ,end) segment))
        (unless (or (>= start end)
                    (eq type 'render-data))
          (let ((text (buffer-substring start end)))
            (unless (string-blank-p (substring-no-properties text))
              (pcase type
                ('ignored
                 (dolist (record (mevedel-transcript-audit-records
                                  text 'injected-reminders))
                   (dolist (entry (plist-get record :items))
                     (push (mevedel-transcript--summary-evidence-item
                            "system-reminder" (plist-get entry :body))
                           items))))
                ('tool
                 (pcase-let
                     ((`(,call ,result)
                       (mevedel-transcript--summary-tool-parts
                        text tool-output-max tool-results-dir
                        (mevedel-transcript--tool-id-in-range start end))))
                   (push
                    (mevedel-transcript--summary-evidence-item
                     "tool-call" call)
                    items)
                   (push
                    (mevedel-transcript--summary-evidence-item
                     "tool-result" result)
                    items)))
                (_
                 (push
                  (mevedel-transcript--summary-evidence-item
                   (pcase type
                     ((or 'user 'prompt) "user")
                     ('response "assistant")
                     ('reasoning "reasoning")
                     ('mailbox "agent-message")
                     ('reminder "system-reminder")
                     ('hook-context "hook-context")
                     ('task-background "task-background")
                     (_ (symbol-name type)))
                   text)
                  items))))))))
    (dolist (skill skill-provenance)
      (unless (stringp skill)
        (error "Skill provenance must be text"))
      (push (mevedel-transcript--summary-evidence-item
             "skill-invocation" skill)
            items))
    ;; Tool result bodies keep their `gptel' text properties through
    ;; media extraction and formatting.  The evidence string is later
    ;; inserted into gptel's prompt buffer, where a stale `(tool . id)'
    ;; span makes the provider parser `read' arbitrary result text as a
    ;; tool-call plist and fail.  Frozen evidence is plain text.
    (substring-no-properties
     (mapconcat #'identity (nreverse items) "\n\n"))))

(provide 'mevedel-transcript)
;;; mevedel-transcript.el ends here
