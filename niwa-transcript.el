;;; niwa-transcript.el --- Transcript span classification -*- lexical-binding: t -*-

;;; Commentary:

;; Canonical classification and property restoration for the gptel data
;; buffer transcript.  Callers decide how to render, persist, or compact the
;; resulting structural spans.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-audit)
(require 'niwa-transcript-source)
(require 'niwa-utilities)

;; `niwa-ptc-interpreter'
(declare-function niwa-ptc-state-direct-tool "niwa-ptc-interpreter" (state))
(autoload 'niwa-ptc-close "niwa-ptc-interpreter")
(autoload 'niwa-ptc-start "niwa-ptc-interpreter")

;; `niwa-tool-media'
(declare-function niwa-tool-media-extract
                  "niwa-tool-media"
                  (result-string &optional tool-results-dir
                                 expected-tool-use-id
                                 allow-payload-tool-use-id))
(autoload 'niwa-tool-media-extract "niwa-tool-media")

;; `niwa-tool-render-data'
(declare-function niwa-tool-render-data-trusted-range-p
                  "niwa-tool-render-data" (start end &optional object))

;; `niwa-transcript-audit'
(declare-function niwa-transcript-audit-buffer-spans
                  "niwa-transcript-audit" (&optional type start end))
(declare-function niwa-transcript-audit-trusted-range-p
                  "niwa-transcript-audit" (start end &optional object))

;; `org'
(declare-function org-entry-get
                  "ext:org" (pom property &optional inherit literal-nil))

(defun niwa-transcript--skip-leading-properties-drawer (pos)
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

(defun niwa-transcript-leading-property (name)
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

(defun niwa-transcript--skip-leading-summary-block (pos)
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

(defun niwa-transcript-summary-source (scan-start &optional agent-p)
  "Return (SOURCE . NEXT) for the compaction summary near SCAN-START.
SOURCE is the summary's (START . END) range in the current buffer, or nil
when there is none.  NEXT is where ordinary transcript content begins.
A leading summary starts at SCAN-START.  With AGENT-P, one behind the
agent's task anchor counts too, found by walking `gptel' ignore runs:
only a producer-owned wrapper grants ownership, never the role tag alone.
The walk is a full-history step, so streaming paths must not call it."
  (let* ((after-summary (niwa-transcript--skip-leading-summary-block scan-start))
         (source (and (> after-summary scan-start)
                      (prog1 (cons scan-start after-summary)
                        (setq scan-start after-summary)))))
    (when (and (not source) agent-p)
      (save-excursion
        (let ((position scan-start))
          (while (and (not source) (< position (point-max)))
            (when (eq (get-text-property position 'gptel) 'ignore)
              (goto-char position)
              (when (looking-at "#\\+begin_summary niwa-role=compaction-summary[ \t]*$")
                (let ((end (niwa-transcript--skip-leading-summary-block position)))
                  (when (> end position)
                    (setq source (cons position end))))))
            (setq position (next-single-property-change
                            position 'gptel nil (point-max)))))))
    (cons source scan-start)))

(defun niwa-transcript-summary-body (source)
  "Return the text of summary range SOURCE without its wrapper lines.
SOURCE is (START . END) in the current buffer, as markers or positions.  The
result is what the model received: the handoff preface plus the summary."
  (let ((text (buffer-substring-no-properties (car source) (cdr source))))
    (string-trim
     (replace-regexp-in-string
      "\\`[ \t\n]*#\\+begin_summary[^\n]*\n\\|\n?#\\+end_summary[ \t\n]*\\'" ""
      text))))

(defun niwa-transcript--after-leading-system-reminders ()
  "Return first position after leading system-reminder blocks."
  (save-excursion
    (goto-char (point-min))
    (while-let ((range (niwa-transcript--system-reminder-range-at-point
                        (point-max))))
      (goto-char (nth 3 range))
      (skip-chars-forward " \t\r\n"))
    (point)))

(defun niwa-transcript-prompt-transform-start ()
  "Return the start of the last user prompt in a transform buffer."
  (let ((pos (point-max)))
    (while (and (> pos (point-min))
                (let ((prop (get-text-property (1- pos) 'gptel)))
                  (cond
                   ((memq prop '(ignore niwa-render-data
                                 niwa-hook-audit))
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
      (niwa-transcript--after-leading-system-reminders))))

(defun niwa-transcript--classify-gptel-prop (prop)
  "Classify a `gptel' text property value PROP into a segment type symbol."
  (pcase prop
    ('nil 'user)
    ('response 'response)
    ((or 'ignore 'niwa-render-data 'niwa-hook-audit) 'ignored)
    (`(tool . ,_id) 'tool)
    (_ 'response)))


(defun niwa-transcript--org-scaffolding-only-text-p (text)
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

(defun niwa-transcript--scaffolding-only-p (data-buf seg-start seg-end)
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
            (niwa-transcript--org-scaffolding-only-text-p text))))))

(defun niwa-transcript--counted-tool-marker-p (marker-pos)
  "Return non-nil when the `#+begin_tool' at MARKER-POS is structural.
A genuine tool block's marker either carries a `gptel' text property (a
live response or a normalized transcript) or is followed by the
serialized `(:name ...)' tool plist.  Literal `#+begin_tool' typed
inside a user prompt has neither; counting it would hide every later
user prompt from the prompt index and from compaction turn boundaries."
  (or (get-text-property marker-pos 'gptel)
      (niwa-transcript--org-tool-block-start-p marker-pos)))

(defun niwa-transcript--org-block-depths-before
    (pos &optional start tool-depth summary-depth)
  "Return (TOOL-DEPTH . SUMMARY-DEPTH) of org block nesting before POS.

TOOL-DEPTH counts tool and reasoning blocks, SUMMARY-DEPTH summary blocks.
START, TOOL-DEPTH and SUMMARY-DEPTH resume a pass that already counted up
to START, so a caller walking a transcript forward pays for each region
once instead of rescanning from the beginning per turn.  Both depths come
from one regexp pass: counting them separately read a long transcript
twice on every save.

The marker is recognized with leading whitespace allowed, which is the
rule the span walk below already applies and the spelling gptel's own org
support accepts."
  (save-excursion
    (save-restriction
      (widen)
      (let ((tool (or tool-depth 0))
            (summary (or summary-depth 0))
            (regexp (concat
                     "^[ \t]*#\\+\\(begin\\|end\\)_\\(tool\\|reasoning\\|summary\\)\\b"
                     "\\|^<!-- niwa-render-data -->")))
        (goto-char (or start (point-min)))
        (while (re-search-forward regexp pos t)
          (if (not (match-beginning 1))
              (when-let* ((block (niwa-transcript--render-data-block-at
                                 (match-beginning 0))))
                (goto-char (min pos (cadr block))))
            (let ((end (equal (match-string 1) "end"))
                  (kind (match-string 2)))
              (if (equal kind "summary")
                  (setq summary (if end (max 0 (1- summary)) (1+ summary)))
                (cond
                 (end (setq tool (max 0 (1- tool))))
                 ((or (not (equal kind "tool"))
                      (niwa-transcript--counted-tool-marker-p
                       (match-beginning 0)))
                  (cl-incf tool)))))))
        (cons tool summary)))))

(defun niwa-transcript-prompt-scan-state ()
  "Return carried state for a forward pass of prompt-start scans.
A caller that walks segments in order threads one of these through
`niwa-transcript--user-prompt-start' so the block-depth prefix is
counted once for the pass rather than once per turn."
  (list (point-min) 0 0))

(defun niwa-transcript--user-prompt-start (pos next prop &optional state)
  "Return the real PROP prompt start in [POS, NEXT), or nil.

Nil-`gptel' regions can contain a mixture of org block glue and the
next user prompt, for example `#+end_tool' / reasoning text /
`#+end_reasoning' / user text.  Scan line-wise so callers agree on the
first non-empty line outside gptel-owned org blocks.

STATE is optional carried state from `niwa-transcript-prompt-scan-state'.
A caller that walks segments forward threads one through, which counts the
block-depth prefix once for the pass; without it each call counts from the
beginning of the buffer."
  (when (null prop)
    (save-excursion
      (save-restriction
        (widen)
        (let* ((from (if state (nth 0 state) (point-min)))
               (depths (niwa-transcript--org-block-depths-before
                        pos from (and state (nth 1 state)) (and state (nth 2 state))))
               (tool-depth (car depths))
               (summary-depth (cdr depths)))
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
                            (niwa-transcript--counted-tool-marker-p
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

(defun niwa-transcript--tool-id-in-range (start end)
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

(defun niwa-transcript--tool-bound-id (start end)
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

(defun niwa-transcript--org-tool-block-parts (start end)
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
                  ;; END is the canonical block boundary.  A quoted close in
                  ;; serialized metadata is part of the result, not scaffold.
                  (goto-char end)
                  (when (re-search-backward "^#\\+end_tool[^\n]*\n?" sexp-end t)
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

(defvar niwa-transcript--control-lines nil
  "Candidate control-line positions shared within one structural scan.")

(defvar niwa-transcript--render-data-blocks nil
  "Complete decoded metadata blocks shared within one structural scan.")

(defun niwa-transcript--render-data-block-at (position &optional limit literal)
  "Return the authorized render-data block at POSITION before LIMIT.
Use the codec's complete payload boundaries.  A tool-owned block must match
its surrounding call, including a saved metadata run that ends partway
through the payload; an unbound block still needs producer provenance.
With LITERAL, return a valid block without checking its authority."
  (when-let* ((block
              (or (when-let* ((memo niwa-transcript--render-data-blocks)
                              (cached (gethash position memo))
                              ((<= (cadr cached)
                                   (min (or limit (point-max)) (point-max)))))
                    cached)
                  (save-excursion
                    (goto-char position)
                    (when (looking-at-p "<!-- niwa-render-data -->")
                      (niwa-tool-render-data-block-at position limit))))))
    ;; A bounded read may omit the optional final newline.  Do not replace
    ;; the complete-source entry with that shorter representation.
    (when (and niwa-transcript--render-data-blocks
               (or (null limit) (>= limit (point-max))))
      (puthash position block niwa-transcript--render-data-blocks))
    (pcase-let ((`(,begin ,end ,data) block))
      (cond
       (literal block)
       ((plist-member data :niwa-tool-use-id)
          (let* ((owner (plist-get data :niwa-tool-use-id))
                 (target (cons 'tool owner))
                 (context begin))
            (when (eq (get-text-property context 'gptel) 'niwa-render-data)
              (setq context
                    (max (point-min)
                         (1- (or (previous-single-property-change
                                  (1+ context) 'gptel nil (point-min))
                                 (point-min))))))
            (when (and (stringp owner)
                       (or (niwa-tool-render-data-call-range-p data begin end)
                           ;; Recovery extends an existing metadata run, never
                           ;; literal user text merely adjacent to a tool.
                           (and (eq (get-text-property position 'gptel)
                                    'niwa-render-data)
                                (or (equal target (get-text-property context 'gptel))
                                    (and (> context (point-min))
                                         (equal target (get-text-property
                                                        (1- context) 'gptel)))))))
              block)))
       ((niwa-tool-render-data-trusted-range-p position end)
        block)))))

(defconst niwa-transcript--control-line-regexp
  (concat "^\\(?:#\\+\\(?:begin_\\|end_\\)"
          "\\|<\\(?:/?\\(?:system-reminder\\|hook-context"
          "\\|task-background\\|agent-message\\|agent-result\\)"
          "\\|!-- /?niwa-\\)\\|:PROMPT:\\|:END:\\)")
  "Candidate prefix regexp shared by direct and resumable scans.")

(defun niwa-transcript--control-line-positions (&optional start)
  "Return a vector of candidate control lines in the accessible buffer.
Begin at START when resuming an unchanged prefix.  Index prefixes only;
the existing parsers still validate each complete marker."
  (save-excursion
    (save-match-data
      (goto-char (or start (point-min)))
      (let (positions)
        (while (re-search-forward niwa-transcript--control-line-regexp nil t)
          (let ((start (match-beginning 0)))
            (push start positions)
            (when-let* ((block (niwa-transcript--render-data-block-at start)))
              (goto-char (cadr block)))))
        (vconcat (nreverse positions))))))

(defconst niwa-transcript--control-kinds
  '((begin-reasoning . "#\\+begin_reasoning\\b")
    (end-reasoning . "#\\+end_reasoning")
    (begin-tool . "#\\+begin_tool\\b")
    (end-tool . "#\\+end_tool")
    (reminder . "</?system-reminder>")
    (mailbox . "<agent-\\(?:result\\|message\\)")
    (hook-context . "</?hook-context>")
    (task-background . "</?task-background>")
    (render-data . "<!-- /?niwa-render-data -->")
    (hook-audit . "<!-- /?niwa-hook-audit -->")
    (prompt . ":PROMPT:")
    (end . ":END:"))
  "Mutually exclusive control line kinds, as line-start prefix regexps.")

(defconst niwa-transcript--control-kind-regexp
  (let ((group 0))
    (mapconcat (lambda (kind)
                 (format "\\(?%d:%s\\)" (setq group (1+ group)) (cdr kind)))
               niwa-transcript--control-kinds "\\|"))
  "Classify an indexed control line by the group of its kind.")

(defconst niwa-transcript--control-search-kinds
  '(("^#\\+begin_reasoning\\b" begin-reasoning)
    ("^#\\+end_reasoning[^\n]*\n?" end-reasoning)
    ("^#\\+begin_tool\\b" begin-tool)
    ("^#\\+end_tool[^\n]*\n?" end-tool)
    ("^#\\+\\(begin_tool\\b\\|end_tool[^\n]*\n?\\)" begin-tool end-tool)
    ("^<system-reminder>[ \t]*$" reminder)
    ("^\\(?:<system-reminder>[ \t]*\\|</system-reminder>[ \t]*\\)\\(?:\n\\|\\'\\)"
     reminder)
    ("^<\\(?:agent-result\\|agent-message\\)\\(?:\\s-\\|>\\)" mailbox)
    ("^<hook-context>[ \t]*$" hook-context)
    ("^</hook-context>[ \t]*\n?" hook-context)
    ("^<task-background>[ \t]*$" task-background)
    ("^</task-background>[ \t]*\n?" task-background)
    ("^<!-- niwa-render-data -->[ \t]*$" render-data)
    ("^<!-- /niwa-render-data -->[ \t]*\\(?:\n\\(?:[ \t\r]*\n\\)*\\)?"
     render-data)
    ("^:PROMPT:[ \t]*$" prompt)
    ("^:END:[ \t]*\n?" end)
    ("^<!-- niwa-hook-audit -->[ \t]*$" hook-audit)
    ("^<!-- /niwa-hook-audit -->[ \t]*\\(?:\n[ \t\r]*\\)*" hook-audit))
  "Control searches whose every match starts a line of one of the listed kinds.
Other searches visit every indexed line.")

(defvar niwa-transcript--control-line-kinds
  (make-hash-table :test #'eq :weakness 'key)
  "Control line indexes to their lines grouped by search regexp.
Every structural scan runs each control search over the whole transcript.
Testing each search's regexp at every indexed line cost a pass per search;
classifying each line once lets a search visit only lines of its kinds.")

(defun niwa-transcript--control-search-lines (regexp)
  "Return the indexed control lines REGEXP can match, in source order."
  (if-let* ((kinds (cdr (assoc regexp niwa-transcript--control-search-kinds))))
      (let ((table
             (or (gethash niwa-transcript--control-lines
                          niwa-transcript--control-line-kinds)
                 (let ((grouped (make-hash-table :test #'equal)))
                   (save-excursion
                     (let (by-kind)
                       (cl-loop for position across niwa-transcript--control-lines
                                do (goto-char position)
                                (when (looking-at niwa-transcript--control-kind-regexp)
                                  (let ((kind (cl-loop for index from 1
                                                       for entry in niwa-transcript--control-kinds
                                                       when (match-beginning index)
                                                       return (car entry))))
                                    (push position (alist-get kind by-kind)))))
                       (pcase-dolist (`(,kind . ,positions) by-kind)
                         (puthash kind (vconcat (nreverse positions)) grouped))))
                   (puthash niwa-transcript--control-lines grouped
                            niwa-transcript--control-line-kinds)))))
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
    niwa-transcript--control-lines))

(defun niwa-transcript--search-control-line (regexp limit)
  "Search forward for control REGEXP before LIMIT, returning its end or nil.
Use the current structural scan's line index when available.  Preserve native
search bounds and match data, including a match ending partway through a line."
  (if (not niwa-transcript--control-lines)
      (re-search-forward regexp limit t)
    (let* ((origin (point))
           (limit (or limit (point-max)))
           (lines (niwa-transcript--control-search-lines regexp))
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

(defun niwa-transcript--system-reminder-range-at-point (limit)
  "Return complete system-reminder bounds at point before LIMIT.
The result is `(START BODY-START BODY-END END)'.  Literal complete reminders
nested inside the outer body do not close it early."
  (when (looking-at "^<system-reminder>[ \t]*$")
    (let ((origin (point))
          (start (match-beginning 0))
          (body-start (progn (forward-line 1) (point)))
          (depth 1)
          body-end
          block-end)
      (while (and (> depth 0)
                  (niwa-transcript--search-control-line
                   "^\\(?:<system-reminder>[ \t]*\\|</system-reminder>[ \t]*\\)\\(?:\n\\|\\'\\)"
                   limit))
        (if (save-excursion
              (goto-char (match-beginning 0))
              (looking-at "<system-reminder>"))
            (cl-incf depth)
          (cl-decf depth)
          (when (zerop depth)
            (setq body-end (match-beginning 0)
                  block-end (match-end 0)))))
      (if block-end
          (list start body-start body-end block-end)
        (goto-char origin)
        nil))))

(defun niwa-transcript--structure-priority (range)
  "Return overlay priority for structural RANGE."
  (pcase (car range)
    ('tool 40)
    ((or 'mailbox 'reminder 'hook-context 'prompt) 30)
    ('reasoning 20)
    ((or 'render-data 'ignored) 10)
    (_ 0)))

(defun niwa-transcript--property-priority (range)
  "Return property-application priority for structural RANGE."
  (pcase (car range)
    ((or 'render-data 'ignored) 40)
    ('tool 30)
    ('reasoning 20)
    (_ 10)))

(defun niwa-transcript--base-segment-type (start base-segments)
  "Return the type of the raw property run in BASE-SEGMENTS holding START."
  (car-safe
   (cl-find-if
    (lambda (seg) (and (<= (cadr seg) start) (< start (caddr seg))))
    base-segments)))

(defun niwa-transcript--control-prefix-p (range base-segments accepted)
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

(defun niwa-transcript--mailbox-control-context-p (range base-segments)
  "Return non-nil when mailbox RANGE is outside an Org heading's text.
BASE-SEGMENTS supplies the raw property span containing RANGE.  A mailbox
after an agent task heading or a heading line in the same span is quoted
data, not delivered structure."
  (let* ((start (cadr range))
         (base (cl-find-if
                (lambda (seg)
                  (and (<= (cadr seg) start) (< start (caddr seg))))
                base-segments)))
    (and base
         (not (string-match-p
               "^\\*+ "
               (buffer-substring-no-properties (cadr base) start))))))

(defun niwa-transcript--unparseable-tool-ranges
    (start end base-segments tool-ranges)
  "Return stale tool blocks in START..END absent from TOOL-RANGES.
BASE-SEGMENTS supplies the raw property spans used to identify closed
blocks that still carry stale tool properties."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (niwa-transcript--search-control-line "^#\\+begin_tool\\b" end)
        (let ((block-start (match-beginning 0)))
          ;; Candidates advance in source order. Earlier disjoint property
          ;; runs and validated blocks cannot prove this or any later block.
          (while (and base-segments
                      (<= (caddr (car base-segments)) block-start))
            (setq base-segments (cdr base-segments)))
          (while (and tool-ranges
                      (<= (caddr (car tool-ranges)) block-start))
            (setq tool-ranges (cdr tool-ranges)))
          (if (niwa-transcript--search-control-line "^#\\+end_tool[^\n]*\n?" end)
              (let ((block-end (match-end 0)))
                (when (and
                       (niwa-transcript--tool-block-overlaps-tool-segment-p
                        base-segments block-start block-end)
                       (not (when-let* ((range (car tool-ranges)))
                              (and (<= (cadr range) block-start)
                                   (<= block-end (caddr range))))))
                  (push (list 'ignored block-start block-end) ranges)))
            (goto-char (1+ block-start))))))
    (nreverse ranges)))

(defconst niwa-transcript--opaque-payload-types
  '(render-data reasoning mailbox prompt)
  "Control ranges whose payload is authored text rather than structure.
A control marker that starts inside one of these is quoted content, so
`niwa-transcript--structural-ranges' refuses to carve it out.  Tool
blocks are left out because they already win by overlay priority and
legitimately neighbour their own render-data.")

(defconst niwa-transcript--self-proving-types
  '(tool render-data ignored)
  "Control ranges carrying their own proof of being generated.
A tool range is validated against the raw `gptel' tool property runs,
and render-data and hook-audit ranges against their trust hash, so these
stay valid inside a container payload: backends really do nest a tool
call inside a reasoning block, and a tool block really does carry its
own render-data.")

(defconst niwa-transcript--range-scans
  '((reasoning "^#\\+begin_reasoning\\b" "^#\\+end_reasoning[^\n]*\n?")
    (mailbox)
    (reminder)
    (hook-context "^<hook-context>[ \t]*$" "^</hook-context>[ \t]*\n?")
    (task-background "^<task-background>[ \t]*$" "^</task-background>[ \t]*\n?")
    (render-data "^<!-- niwa-render-data -->[ \t]*$"
                 "^<!-- /niwa-render-data -->[ \t]*\\(?:\n\\(?:[ \t\r]*\n\\)*\\)?")
    (prompt "^:PROMPT:[ \t]*$" "^:END:[ \t]*\n?")
    (ignored "^<!-- niwa-hook-audit -->[ \t]*$"
             "^<!-- /niwa-hook-audit -->[ \t]*\\(?:\n[ \t\r]*\\)*"))
  "Canonical control searches in structural precedence order.")

(defun niwa-transcript--next-raw-range (spec pos end)
  "Return (NEXT . RANGE) for the next SPEC candidate from POS to END.
RANGE is nil when a malformed opener was skipped.  Return nil only when
there are no more candidates.  Search for closes against the complete
source, not a work-slice boundary."
  (save-excursion
    (goto-char pos)
    (pcase (car spec)
      ('reminder
       (when (niwa-transcript--search-control-line
              "^<system-reminder>[ \t]*$" end)
         (goto-char (match-beginning 0))
         (let ((start (point))
               (range (niwa-transcript--system-reminder-range-at-point end)))
           (if range
               (cons (nth 3 range) (list 'reminder start (nth 3 range)))
             (cons (1+ start) nil)))))
      ('mailbox
       (when (niwa-transcript--search-control-line
              "^<\\(?:agent-result\\|agent-message\\)\\(?:\\s-\\|>\\)"
              end)
         (let ((start (match-beginning 0)))
           (goto-char start)
           (search-forward "<" end)
           (backward-char 1)
           (let ((block (niwa-transcript--mailbox-any-block-at-point end)))
             (if block
                 (cons (plist-get block :close-end)
                       (list 'mailbox start (plist-get block :close-end)))
               (cons (1+ start) nil))))))
      ('render-data
       (when (niwa-transcript--search-control-line (cadr spec) end)
         (let* ((start (match-beginning 0))
                (block (niwa-transcript--render-data-block-at start end t)))
           (if block
               (progn
                 (goto-char (cadr block))
                 ;; Blank separator lines belong to metadata rather than the
                 ;; enclosing tool's readable body, as with other controls.
                 (while (and (< (point) end)
                             (looking-at "[ \t\r]*\n")
                             (<= (match-end 0) end))
                   (goto-char (match-end 0)))
                 (cons (point) (list 'render-data start (point))))
             (cons (1+ start) nil)))))
      (_
       (when (niwa-transcript--search-control-line (cadr spec) end)
         (let ((start (match-beginning 0))
               (next (match-end 0)))
           (if (niwa-transcript--search-control-line (caddr spec) end)
               (let ((finish (match-end 0)))
                 (cons finish (list (car spec) start finish)))
             (cons next nil))))))))

(defun niwa-transcript--raw-ranges (start end)
  "Return canonical candidate ranges in START..END before validation."
  (let (ranges)
    (dolist (spec niwa-transcript--range-scans)
      (let ((pos start) next)
        (while (setq next (niwa-transcript--next-raw-range spec pos end))
          (setq pos (car next))
          (when (cdr next)
            (push (cdr next) ranges)))))
    (nreverse ranges)))

(defun niwa-transcript--structural-ranges (start end base-segments
                                                    &optional prepared-ranges
                                                    retained-tools)
  "Return canonical control ranges in START..END.
BASE-SEGMENTS are raw `gptel' property runs used to validate persisted
tool blocks.  Each result is `(TYPE START END VALUE...)'.
PREPARED-RANGES, when non-nil, is a cons whose cdr holds candidates
collected by a resumable scan of this same source.  RETAINED-TOOLS holds
validated tool ranges before START when resuming an unchanged prefix."
  (let* ((niwa-transcript--render-data-blocks
          (or niwa-transcript--render-data-blocks
              (make-hash-table :test #'eql)))
         (niwa-transcript--control-lines
          (or niwa-transcript--control-lines
              (niwa-transcript--control-line-positions)))
         (ranges (if prepared-ranges (cdr prepared-ranges)
                   (niwa-transcript--raw-ranges start end))))
    (let (tool-ranges)
      (dolist (block (niwa-transcript--org-tool-blocks-overlapping
                      base-segments start end))
        (push (list 'tool (car block) (cdr block)
                    (or (niwa-transcript--tool-id-in-range
                         (car block) (cdr block))
                        (niwa-transcript--tool-bound-id
                         (car block) (cdr block))
                        ""))
              tool-ranges))
      (setq tool-ranges (append retained-tools (nreverse tool-ranges)))
      (setq ranges
            (append ranges tool-ranges
                    (niwa-transcript--unparseable-tool-ranges
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
                      (looking-at-p "<!-- niwa-hook-audit -->"))))
              (render-p (eq (car range) 'render-data))
              ;; Generated control blocks reach the transcript in their
              ;; own run, never inside gptel's streamed `response' run,
              ;; so markup found there is prose the model quoted.  Tool
              ;; blocks are exempt: they are already validated against
              ;; the raw tool property runs.
              (quoted-in-response-p
               (and (eq (niwa-transcript--base-segment-type
                         (cadr range) base-segments)
                        'response)
                    (not (eq (car range) 'tool))
                    (not (niwa-transcript--control-prefix-p
                          range base-segments accepted)))))
          (when (or (and (not quoted-in-response-p)
                         (not (memq (car range) '(mailbox reminder)))
                         (not audit-p)
                         (not render-p))
                    (and audit-p
                         (save-excursion
                           (goto-char (cadr range))
                           (when (search-forward
                                  "<!-- /niwa-hook-audit -->"
                                  (caddr range) t)
                             (niwa-transcript-audit-trusted-range-p
                              (cadr range) (point)))))
                    (and render-p
                         (niwa-transcript--render-data-block-at
                          (cadr range) (caddr range)))
                  (and (eq (car range) 'mailbox)
                       (niwa-transcript--mailbox-control-context-p
                        range base-segments))
                  (and (eq (car range) 'mailbox)
                       (cl-find-if (lambda (prior)
                                     (eq (car prior) 'mailbox))
                                   accepted))
                  (and (not audit-p)
                       (not render-p)
                       (niwa-transcript--control-prefix-p
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
                                    niwa-transcript--self-proving-types))
                         (cl-find-if
                          (lambda (span)
                            (and (> (cadr range) (car span))
                                 (< (cadr range) (cdr span))))
                          payloads))
              (when (memq (car range)
                          niwa-transcript--opaque-payload-types)
                (push (cons (cadr range) (caddr range)) payloads))
              (push range accepted)))))
      (setq ranges (nreverse accepted)))
    (sort ranges
          (lambda (a b)
            (let ((pa (niwa-transcript--structure-priority a))
                  (pb (niwa-transcript--structure-priority b)))
              (if (= pa pb)
                  (< (cadr a) (cadr b))
                (< pa pb)))))))

(defun niwa-transcript--overlay-range (segments range)
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

(defun niwa-transcript--overlay-ranges (segments ranges)
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
                    (niwa-transcript--overlay-range (cdr cursor) range))))))
    (cdr head)))

(defun niwa-transcript--property-segments (start end)
  "Return raw `gptel' property segments in START..END."
  (let (segments seg-start seg-type)
    (save-excursion
      (setq start (or (previous-single-property-change
                       (min (1+ start) (point-max)) 'gptel nil (point-min))
                      (point-min))
            end (or (next-single-property-change end 'gptel nil (point-max))
                    (point-max)))
      (setq start (niwa-transcript--skip-leading-properties-drawer start))
      (setq start (niwa-transcript--skip-leading-summary-block start))
      (goto-char start)
      (setq seg-start start
            seg-type (niwa-transcript--classify-gptel-prop
                      (get-text-property start 'gptel)))
      (while (< (point) end)
        (let ((next (next-single-property-change (point) 'gptel nil end)))
          (goto-char next)
          (when (< next end)
            (push (list seg-type seg-start next) segments)
            (setq seg-start next
                  seg-type (niwa-transcript--classify-gptel-prop
                            (get-text-property next 'gptel))))))
      (when (< seg-start end)
        (push (list seg-type seg-start end) segments)))
    (nreverse segments)))

(cl-defstruct (niwa-transcript--scan
               (:constructor niwa-transcript--make-scan))
  "One full-source canonical segmentation pass."
  buffer tick low high start end phase pos type properties controls
  control-lines render-data-blocks raw-index raw-pos raw-ranges result)

(defun niwa-transcript-scan-start (start end)
  "Start a resumable canonical scan between START and END in this buffer.
Capture the effective narrowing; later steps restore that restriction even
when the caller widened the source buffer.  Callers must discard a job
after any source text or property change."
  (let* ((low (point-min))
         (high (point-max))
         (from (or (previous-single-property-change
                    (min (1+ start) high) 'gptel nil low)
                   low)))
    (setq from (niwa-transcript--skip-leading-properties-drawer from)
          from (niwa-transcript--skip-leading-summary-block from))
    (niwa-transcript--make-scan
     :buffer (current-buffer) :tick (buffer-modified-tick)
     :low low :high high :start start :end end
     :phase 'properties :pos from :type (niwa-transcript--classify-gptel-prop
                                        (get-text-property from 'gptel))
     :properties nil :controls nil
     :render-data-blocks (make-hash-table :test #'eql))))

(defun niwa-transcript-scan-cancel (job)
  "Release retained intermediate data in transcript scan JOB."
  (setf (niwa-transcript--scan-phase job) 'cancelled
        (niwa-transcript--scan-buffer job) nil
        (niwa-transcript--scan-properties job) nil
        (niwa-transcript--scan-controls job) nil
        (niwa-transcript--scan-control-lines job) nil
        (niwa-transcript--scan-render-data-blocks job) nil
        (niwa-transcript--scan-raw-ranges job) nil
        (niwa-transcript--scan-result job) nil)
  nil)

(defun niwa-transcript-scan-result (job)
  "Return JOB's canonical segments once it is complete, or nil."
  (when (eq (niwa-transcript--scan-phase job) 'done)
    (niwa-transcript--scan-result job)))

(defun niwa-transcript--scan-properties-step (job)
  "Advance JOB by at most 128 complete gptel property runs."
  (let ((pos (niwa-transcript--scan-pos job))
        (type (niwa-transcript--scan-type job))
        (properties (niwa-transcript--scan-properties job))
        (to (or (next-single-property-change
                 (niwa-transcript--scan-end job) 'gptel nil (point-max))
                (point-max)))
        (count 0))
    (while (and (< pos to) (< count 128))
      (let ((next (next-single-property-change pos 'gptel nil to)))
        (push (list type pos next) properties)
        (setq pos next)
        (when (< pos to)
          (setq type
                (niwa-transcript--classify-gptel-prop
                 (get-text-property pos 'gptel))))
        (cl-incf count)))
    (when (>= pos to) (setq properties (reverse properties)))
    ;; Commit the lists and their cursors together.  An input abort before this
    ;; point can repeat the unit without duplicating runs or reversing old state.
    (let ((inhibit-quit t))
      (setf (niwa-transcript--scan-pos job) pos
            (niwa-transcript--scan-type job) type
            (niwa-transcript--scan-properties job) properties)
      (when (>= pos to)
        (setf (niwa-transcript--scan-start job)
              (if properties
                  (cadr (car properties))
                (niwa-transcript--scan-start job))
              (niwa-transcript--scan-end job)
              (if properties to
                (niwa-transcript--scan-end job))
              (niwa-transcript--scan-pos job) (point-min)
              (niwa-transcript--scan-phase job) 'controls)))))

(defun niwa-transcript--scan-controls-step (job)
  "Index a bounded source span and at most 128 control prefixes for JOB."
  (save-excursion
    (goto-char (niwa-transcript--scan-pos job))
    (let* ((count 0)
           (controls (niwa-transcript--scan-controls job))
           (limit (min (point-max) (+ (point) 16384)))
           (finished nil))
      (while (and (< count 128)
                  (< (point) limit)
                  (re-search-forward niwa-transcript--control-line-regexp
                                     limit t))
        (let ((start (match-beginning 0)))
          (push start controls)
          (when-let* ((block (niwa-transcript--render-data-block-at start)))
            (goto-char (cadr block))))
        (cl-incf count))
      ;; Search limits may cut across the fixed candidate prefix.  Revisit
      ;; the final 64 characters on the next step, without repeating matches
      ;; already found in this one.  Every indexed match ends before LIMIT.
      (setq finished (and (< count 128) (= limit (point-max))))
      (let ((pos (if (= count 128) (point)
                   (max (point) (min limit
                                     (max (niwa-transcript--scan-pos job)
                                          (- limit 64))))))
            (lines (when finished (vconcat (reverse controls)))))
        (let ((inhibit-quit t))
          (setf (niwa-transcript--scan-pos job) pos
                (niwa-transcript--scan-controls job) controls)
          (when finished
            (setf (niwa-transcript--scan-control-lines job) lines
                  (niwa-transcript--scan-controls job) nil
                  (niwa-transcript--scan-pos job)
                  (niwa-transcript--scan-start job)
                  (niwa-transcript--scan-raw-pos job)
                  (niwa-transcript--scan-start job)
                  (niwa-transcript--scan-phase job) 'raw)))))))

(defun niwa-transcript--scan-raw-step (job)
  "Collect at most 32 complete control candidates for JOB."
  (let* ((spec (nth (or (niwa-transcript--scan-raw-index job) 0)
                    niwa-transcript--range-scans))
         (pos (niwa-transcript--scan-raw-pos job))
         (ranges (niwa-transcript--scan-raw-ranges job))
         (count 0)
         next)
    (if (null spec)
        (setf (niwa-transcript--scan-phase job) 'finish)
      (while (and (< count 32)
                  (setq next (niwa-transcript--next-raw-range
                              spec pos (niwa-transcript--scan-end job))))
        (setq pos (car next))
        (when (cdr next)
          (push (cdr next) ranges))
        (cl-incf count))
      (let ((inhibit-quit t))
        (setf (niwa-transcript--scan-raw-pos job) pos
              (niwa-transcript--scan-raw-ranges job) ranges)
        (unless next
          (setf (niwa-transcript--scan-raw-index job)
                (1+ (or (niwa-transcript--scan-raw-index job) 0))
                (niwa-transcript--scan-raw-pos job)
                (niwa-transcript--scan-start job)))))))

(defun niwa-transcript-scan-step (job)
  "Advance JOB by one cooperative unit; return non-nil when complete.
All steps run in JOB's original source buffer and effective narrowing.
Input may abandon a step before its state commit; retrying it is safe.
Individual decodes and other non-quitting primitives can still exceed
the caller's timer budget."
  (pcase (niwa-transcript--scan-phase job)
    ('done t)
    ('cancelled (error "Transcript scan cancelled"))
    (_
     (let ((source (niwa-transcript--scan-buffer job)))
       (unless (buffer-live-p source)
         (error "Transcript scan source is dead"))
       (with-current-buffer source
         (unless (eql (buffer-modified-tick) (niwa-transcript--scan-tick job))
           (error "Transcript scan source changed"))
         (save-excursion
           (save-restriction
             (widen)
             (narrow-to-region (niwa-transcript--scan-low job)
                               (niwa-transcript--scan-high job))
             (let ((niwa-transcript--control-lines
                    (niwa-transcript--scan-control-lines job))
                   (niwa-transcript--render-data-blocks
                    (niwa-transcript--scan-render-data-blocks job)))
               (pcase (niwa-transcript--scan-phase job)
                 ('properties (niwa-transcript--scan-properties-step job))
                 ('controls (niwa-transcript--scan-controls-step job))
                 ('raw (niwa-transcript--scan-raw-step job))
                 ('finish
                  (let* ((segments (niwa-transcript--scan-properties job))
                         (start (niwa-transcript--scan-start job))
                         (end (niwa-transcript--scan-end job))
                         (ranges (niwa-transcript--structural-ranges
                                  start end segments
                                  (cons t (reverse
                                           (niwa-transcript--scan-raw-ranges
                                            job)))))
                         (result (niwa-transcript--finish-segments
                                  segments ranges start end)))
                    (let ((inhibit-quit t))
                      (setf (niwa-transcript--scan-result job) result
                            (niwa-transcript--scan-phase job) 'done
                            (niwa-transcript--scan-properties job) nil
                            (niwa-transcript--scan-controls job) nil
                            (niwa-transcript--scan-raw-ranges job) nil
                            (niwa-transcript--scan-render-data-blocks job) nil
                            (niwa-transcript--scan-control-lines job) nil)))))))))
       (eq (niwa-transcript--scan-phase job) 'done)))))

(defun niwa-transcript-segments (start end)
  "Return canonical transcript segments between START and END.
Each segment is `(TYPE START END)'.  Structural control ranges override
stale `gptel' runs; incomplete controls remain ordinary transcript text."
  (let ((job (niwa-transcript-scan-start start end)))
    (unwind-protect
        (progn
          (while (not (niwa-transcript-scan-step job)))
          (niwa-transcript-scan-result job))
      (niwa-transcript-scan-cancel job))))

(defconst niwa-transcript--response-continuation-max-gap 160
  "Maximum structural-to-response prefix size repaired after restore.")

(cl-defstruct (niwa-transcript-cache
               (:constructor niwa-transcript-cache-create)
               (:copier nil))
  "Derived segmentation of one accessible transcript, never persisted.
SOURCE proves the unchanged prefix through paired modification hooks; an
unobserved edit requires full derivation.  ORIGIN is the last body start, whose
marker follows known metadata writes.  BLOCKED is the earliest unresolved
opener or orphan tool property run."
  buffer low high limit origin mode tick source raw ranges blocked segments restart ranges-only-p)

(defun niwa-transcript--cache-rebase (cache delta)
  "Shift CACHE's body coordinates by a known leading metadata DELTA."
  (dolist (slot '(raw ranges segments))
    (let ((shifted
           (mapcar (lambda (range)
                     (append (list (car range) (+ delta (cadr range))
                                   (+ delta (caddr range)))
                             (cdddr range)))
                   (pcase slot
                     ('raw (niwa-transcript-cache-raw cache))
                     ('ranges (niwa-transcript-cache-ranges cache))
                     ('segments (niwa-transcript-cache-segments cache))))))
      (pcase slot
        ('raw (setf (niwa-transcript-cache-raw cache) shifted))
        ('ranges (setf (niwa-transcript-cache-ranges cache) shifted))
        ('segments (setf (niwa-transcript-cache-segments cache) shifted)))))
  (when (niwa-transcript-cache-blocked cache)
    (cl-incf (niwa-transcript-cache-blocked cache) delta))
  (when (niwa-transcript-cache-restart cache)
    (cl-incf (niwa-transcript-cache-restart cache) delta))
  (cl-incf (niwa-transcript-cache-high cache) delta)
  (cl-incf (niwa-transcript-cache-origin cache) delta))

(defun niwa-transcript--cache-restart (cache change)
  "Return CACHE's last closed control before CHANGE, or the buffer start.
An unresolved opener or a raw container crossing that point moves the
restart back: a later close can change even a previously rejected range.
Without a tool close in the changed suffix, an old tool cannot extend.  Resume
at the changed line instead, retaining any preceding control whose bounded
response-prefix repair can still reach the change."
  (let ((start (point-min))
        (limit (min change (or (niwa-transcript-cache-blocked cache) change)))
        last-tool moved independent-tail line-start)
    (save-excursion
      (goto-char (min limit (point-max)))
      ;; Opener validity can depend on trailing whitespace anywhere on the
      ;; line.  An edit far from the prefix can turn a rejected opener into a
      ;; valid one, so a fixed-size lookbehind cannot prove this boundary.
      (setq line-start (line-beginning-position))
      (goto-char line-start)
      (setq independent-tail
            (not (re-search-forward "^#\\+end_tool" nil t))))
    (dolist (range (niwa-transcript-cache-ranges cache))
      (when (and (< (cadr range) limit) (< (caddr range) limit))
        (setq start (max start (cadr range)))
        (when (eq (car range) 'tool)
          (setq last-tool (max (or last-tool (point-min)) (cadr range))))))
    ;; Tool recovery chooses the last close before the next genuine opener,
    ;; even across other controls.  Keep that final tool's lookahead live.
    (if independent-tail
        (progn
          (setq start line-start)
          (dolist (range (niwa-transcript-cache-ranges cache))
            (when (and (<= (caddr range) limit)
                       (>= (caddr range)
                           (- limit niwa-transcript--response-continuation-max-gap)))
              (setq start (min start (cadr range))))))
      (when last-tool (setq start (min start last-tool))))
    (setq moved t)
    (while moved
      (setq moved nil)
      (dolist (range (append (niwa-transcript-cache-raw cache)
                            (niwa-transcript-cache-ranges cache)))
        (when (and (< (cadr range) start) (> (caddr range) start))
          (setq start (cadr range) moved t))))
    start))

(defun niwa-transcript--cached-parse (cache origin ranges-only)
  "Return full canonical segmentation, retaining unchanged structure in CACHE.
Resume control searches at the last closed control before the first source
change.  Validate against the complete raw property runs and apply neighbor
repairs to the joined result, preserving the full parser's context.  The
cache is valid only for its source buffer, restriction and major mode.
ORIGIN, when supplied, is the body start after known leading metadata.
RANGES-ONLY derives normalization ranges including leading summary controls,
instead of the conversation segments that exclude the inherited summary."
  (let* ((origin (or origin (point-min)))
         (tick (buffer-modified-tick))
         (proof (niwa-transcript-cache-source cache))
         (same (and (eq (current-buffer) (niwa-transcript-cache-buffer cache))
                    (eql (point-min) (niwa-transcript-cache-low cache))
                    (eql (point-max)
                         (and (markerp (niwa-transcript-cache-limit cache))
                              (marker-position
                               (niwa-transcript-cache-limit cache))))
                    (eq major-mode (niwa-transcript-cache-mode cache))
                    (eq ranges-only (niwa-transcript-cache-ranges-only-p cache))
                    proof
                    (eql origin (marker-position
                                 (niwa-transcript-source-origin proof)))))
         (dirty (and same (niwa-transcript-source-change proof)))
         (delta (if same (- origin (niwa-transcript-cache-origin cache)) 0))
         (old (and same (or dirty
                            (= (point-max) (+ delta
                                             (niwa-transcript-cache-high cache)))))))
    (unless old
      (when proof (niwa-transcript-source-release proof))
      (when (markerp (niwa-transcript-cache-limit cache))
        (set-marker (niwa-transcript-cache-limit cache) nil))
      (setf (niwa-transcript-cache-limit cache) (copy-marker (point-max) t))
      (setq proof (niwa-transcript-source-create origin))
      ;; Establish ownership before derivation, which may quit or signal.  A
      ;; failed attempt remains invalid and its next attempt releases this proof.
      (setf (niwa-transcript-cache-source cache) proof
            (niwa-transcript-cache-buffer cache) nil))
    (when (and old (/= delta 0))
      (niwa-transcript--cache-rebase cache delta))
    (if (and old (null dirty))
        (niwa-transcript-cache-segments cache)
      (let* ((change (and old (+ origin dirty)))
             (base (niwa-transcript--property-segments (point-min) (point-max)))
             (low (if (and base (not ranges-only))
                      (cadr (car base)) (point-min)))
             (old (and old (>= change low) old))
             (start (if old (max low (niwa-transcript--cache-restart cache change))
                      low))
             (niwa-transcript--render-data-blocks (make-hash-table :test #'eql))
             (niwa-transcript--control-lines
              (niwa-transcript--control-line-positions start))
             (raw (and old (cl-remove-if-not
                            (lambda (range) (<= (caddr range) start))
                            (niwa-transcript-cache-raw cache))))
             (prefix (and old (cl-remove-if-not
                               (lambda (range) (<= (caddr range) start))
                               (niwa-transcript-cache-ranges cache))))
             (blocked (and old (niwa-transcript-cache-blocked cache)
                           (< (niwa-transcript-cache-blocked cache) start)
                           (niwa-transcript-cache-blocked cache))))
        (dolist (spec niwa-transcript--range-scans)
          (let ((pos start) next)
            (while (setq next (niwa-transcript--next-raw-range spec pos (point-max)))
              (if (cdr next) (push (cdr next) raw)
                (setq blocked (min (or blocked pos) pos)))
              (setq pos (car next)))))
        (let* ((ranges (niwa-transcript--structural-ranges
                        start (point-max) base
                        (cons t (append (cl-remove-if
                                         (lambda (range) (eq (car range) 'tool)) prefix)
                                        (cl-remove-if
                                         (lambda (range) (<= (caddr range) start)) raw)))
                        (cl-remove-if-not (lambda (range) (eq (car range) 'tool)) prefix)))
               (segments (if ranges-only ranges
                           (niwa-transcript--finish-segments
                            base ranges (point-min) (point-max))))
               (tools (sort (cl-remove-if-not (lambda (range) (eq (car range) 'tool))
                                              (copy-sequence ranges))
                            (lambda (a b) (< (cadr a) (cadr b))))))
          ;; An incomplete or unrecognized tool opener can acquire its first
          ;; valid close later, even when it was not a canonical tool before.
          (save-excursion
            (cl-loop for position across niwa-transcript--control-lines do
                     (goto-char position)
                     (when (looking-at-p "#\\+begin_tool\\b")
                       (while (and tools (<= (caddr (car tools)) position))
                         (setq tools (cdr tools)))
                       (unless (and tools (<= (cadr (car tools)) position))
                         (setq blocked (min (or blocked position) position))))))
          ;; Orphan tool properties can make recovery extend an earlier close
          ;; when a later tool run arrives.  Do not freeze that earlier state.
          (let ((tools (sort (cl-remove-if-not
                              (lambda (range) (eq (car range) 'tool))
                              (copy-sequence ranges))
                             (lambda (a b) (< (cadr a) (cadr b))))))
            (dolist (segment base)
              (when (eq (car segment) 'tool)
                (while (and tools (<= (caddr (car tools)) (cadr segment)))
                  (setq tools (cdr tools)))
                (unless (and tools (<= (cadr (car tools)) (cadr segment))
                             (>= (caddr (car tools)) (caddr segment)))
                  (setq blocked (min (or blocked (cadr segment)) (cadr segment)))))))
          (niwa-transcript-source-accept proof)
          (setf (niwa-transcript-cache-buffer cache) (current-buffer)
                (niwa-transcript-cache-low cache) (point-min)
                (niwa-transcript-cache-high cache) (point-max)
                (niwa-transcript-cache-origin cache) origin
                (niwa-transcript-cache-mode cache) major-mode
                (niwa-transcript-cache-tick cache) tick
                (niwa-transcript-cache-source cache) proof
                (niwa-transcript-cache-ranges-only-p cache) ranges-only
                (niwa-transcript-cache-raw cache) raw
                (niwa-transcript-cache-ranges cache) ranges
                (niwa-transcript-cache-blocked cache) blocked
                (niwa-transcript-cache-segments cache) segments
                (niwa-transcript-cache-restart cache) start)
          segments)))))

(defun niwa-transcript-cached-segments (cache &optional origin)
  "Return canonical segments, reusing CACHE's hook-proven unchanged prefix.
ORIGIN is the body start after known leading metadata, or `point-min'."
  (niwa-transcript--cached-parse cache origin nil))

(defun niwa-transcript-cached-ranges (cache &optional origin)
  "Return normalization ranges, reusing CACHE's hook-proven unchanged prefix.
ORIGIN is the body start after known leading metadata, or `point-min'."
  (niwa-transcript--cached-parse cache origin t))

(defvar niwa-transcript--prepared-audit-spans nil
  "Nil, or (t . SPANS) from the current source's proven audit derivation.
The prompt-index consumer supplies canonical absolute spans while finishing
the same source, avoiding a second whole-buffer audit scan.")

(defun niwa-transcript--finish-segments (segments ranges scan-start scan-end)
  "Apply RANGES and canonical neighbor repairs to SEGMENTS in source bounds."
  (let ((segments (niwa-transcript--overlay-ranges segments ranges))
        repaired)
    (dolist (span
             (if niwa-transcript--prepared-audit-spans
                 (cl-remove-if-not
                  (lambda (span)
                    (eq (plist-get (plist-get span :record) :type) 'fork-point))
                  (cdr niwa-transcript--prepared-audit-spans))
               (niwa-transcript-audit-buffer-spans 'fork-point scan-start scan-end)))
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
          (setq repaired t
                segments
                (niwa-transcript--overlay-range
                 segments (list 'user prompt-start prompt-end))))))
    ;; Repair stale prompt properties without changing authoritative controls
    ;; inside that prefix, such as hidden injection records.
    (when repaired
      (setq segments (niwa-transcript--overlay-ranges segments ranges)))
    (niwa-transcript--merge-adjacent-segments
     (niwa-transcript--repair-response-fragment-segments
      (niwa-transcript--repair-orphan-mailbox-tail-segments
       (niwa-transcript--repair-mailbox-prose-segments
        (niwa-transcript--absorb-structural-whitespace segments))))
     '(ignored))))

(defun niwa-transcript--whitespace-segment-p (seg)
  "Return non-nil if SEG is entirely whitespace."
  (save-excursion
    (goto-char (cadr seg))
    (skip-chars-forward " \t\r\n" (caddr seg))
    (= (point) (caddr seg))))

(defun niwa-transcript--absorb-structural-whitespace (segments)
  "Attach whitespace-only property fragments to adjacent control SEGMENTS."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let ((seg (car rest))
            (next (cadr rest)))
        (if (and next
                 (memq (car seg) '(user ignored))
                 (memq (car next)
                       '(tool reasoning mailbox reminder hook-context
                         render-data prompt ignored))
                 (niwa-transcript--whitespace-segment-p seg))
            (progn
              (setcar (cdr rest)
                      (list (car next) (cadr seg) (caddr next)))
              (setq rest (cdr rest)))
          (push seg out)
          (setq rest (cdr rest)))))
    (nreverse out)))

(defun niwa-transcript--repair-mailbox-prose-segments (segments)
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

(defun niwa-transcript--repair-orphan-mailbox-tail-segments (segments)
  "Classify orphan agent-result tails in SEGMENTS as ignored control text."
  (let (out rest)
    (setq rest segments)
    (while rest
      (let* ((seg (car rest))
             (text (and (eq (car seg) 'user)
                        (eq (car-safe (car out)) 'tool)
                        (eq (car-safe (cadr rest)) 'reasoning)
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

(defun niwa-transcript--clear-gptel-properties (start end)
  "Clear stale gptel-related text properties from START to END."
  (remove-text-properties
   start end
   '(gptel nil response nil invisible nil front-sticky nil)))

(defun niwa-transcript--apply-structural-properties (ranges)
  "Apply canonical `gptel' properties for structural RANGES."
  (dolist (range (sort (copy-sequence ranges)
                       (lambda (a b)
                         (< (niwa-transcript--property-priority a)
                            (niwa-transcript--property-priority b)))))
    (pcase-let ((`(,type ,start ,end . ,values) range))
      (niwa-transcript--clear-gptel-properties start end)
      (pcase type
        ('tool
         (if-let* ((parts (niwa-transcript--org-tool-block-parts
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
          start end '(gptel niwa-render-data niwa-render-data t)))
        ('ignored
         (if (save-excursion
               (goto-char start)
               (looking-at-p "<!-- niwa-hook-audit -->"))
             (add-text-properties
              start end '(gptel niwa-hook-audit niwa-hook-audit t))
           (put-text-property start end 'gptel 'ignore)))
        ((or 'reasoning 'prompt)
         (put-text-property start end 'gptel 'ignore))))))

(defun niwa-transcript--structural-gap-prop-p (prop type)
  "Return non-nil when PROP is stale structural state after TYPE."
  (or (null prop)
      (pcase type
        ('tool
         (or (eq prop 'tool)
             (and (consp prop) (eq (car prop) 'tool))))
        ((or 'reasoning 'prompt)
         (eq prop 'ignore))
        ('render-data
         (eq prop 'niwa-render-data))
        ('ignored
         (memq prop '(ignore niwa-hook-audit)))
        (_ nil))))

(defun niwa-transcript--first-nonblank-pos (start end)
  "Return the first non-whitespace position in START..END, or nil."
  (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\n\r" end)
    (when (< (point) end)
      (point))))

(defun niwa-transcript--response-continuation-range (start type)
  "Return a stale response prefix range after structural START and TYPE."
  (let ((limit (min (point-max)
                    (+ start niwa-transcript--response-continuation-max-gap)))
        (pos start)
        prefix-start response-start done)
    (while (and (< pos limit) (not response-start) (not done))
      (let* ((prop (get-text-property pos 'gptel))
             (next (or (next-single-property-change pos 'gptel nil limit)
                       limit)))
        (cond
         ((eq prop 'response)
          (setq response-start pos))
         ((niwa-transcript--structural-gap-prop-p prop type)
          (unless prefix-start
            (setq prefix-start
                  (niwa-transcript--first-nonblank-pos pos next))))
         (t
          (setq done t)))
        (setq pos next)))
    (when (and prefix-start
               response-start
               (< prefix-start response-start)
               (not (memq (char-before response-start) '(?\n ?\r)))
               (null (niwa-transcript--structural-ranges
                      prefix-start response-start
                      (niwa-transcript--property-segments
                       prefix-start response-start)))
               (niwa-transcript--response-continuation-text-p
                (buffer-substring-no-properties prefix-start response-start)))
      (cons prefix-start response-start))))

(defun niwa-transcript--repair-response-continuation-properties (ranges)
  "Repair stale response prefixes immediately after structural RANGES."
  (dolist (range ranges)
    (pcase-let ((`(,type ,_start ,end . ,_) range))
      (when (< end (point-max))
        (when-let* ((repair
                     (niwa-transcript--response-continuation-range
                      end type)))
          (add-text-properties
           (car repair) (cdr repair)
           '(gptel response front-sticky (gptel))))))))

(defvar-local niwa-transcript--normalized-tick nil
  "Modification tick after the last successful whole-buffer normalization.
Text and property edits both invalidate this derived state.")

(defvar-local niwa-transcript--normalization-cache nil
  "Hook-proven structural ranges for the current buffer's normalization.")

(defun niwa-transcript-normalize-properties ()
  "Normalize structural transcript properties in the current Org buffer.
Reapply only the suffix after the last proven closed control.  Unobserved edits
require a full pass; known leading metadata writes only rebase coordinates."
  (when (and (derived-mode-p 'org-mode)
             (not (eql niwa-transcript--normalized-tick
                       (buffer-modified-tick))))
    (save-match-data
      (save-excursion
        (save-restriction
          (widen)
          (let* ((drawer-end
                  (niwa-transcript--skip-leading-properties-drawer
                   (point-min)))
                 (cache (or niwa-transcript--normalization-cache
                            (setq niwa-transcript--normalization-cache
                                  (niwa-transcript-cache-create))))
                 (proof (niwa-transcript-cache-source cache))
                 (changed (or (null proof)
                              (not (eq (marker-buffer
                                        (niwa-transcript-source-origin proof))
                                       (current-buffer)))
                              (not (eql drawer-end
                                        (marker-position
                                         (niwa-transcript-source-origin proof))))
                              (niwa-transcript-source-change proof)))
                 (ranges (niwa-transcript-cached-ranges cache drawer-end))
                 (restart (niwa-transcript-cache-restart cache))
                 (suffix (and changed
                              (cl-remove-if
                               (lambda (range) (<= (caddr range) restart))
                               ranges))))
            (with-silent-modifications
              (when (> drawer-end (point-min))
                (niwa-transcript-source-call-with-metadata-change
                 (lambda ()
                   (niwa-transcript-source-call-with-change
                    (point-min) drawer-end
                    (lambda ()
                      (niwa-transcript--clear-gptel-properties
                       (point-min) drawer-end))))))
              (when suffix
                (niwa-transcript-source-call-with-change
                 (max restart drawer-end) (point-max)
                 (lambda ()
                   (niwa-transcript--apply-structural-properties suffix)
                   (niwa-transcript--repair-response-continuation-properties
                    suffix)))))
            ;; Normalization is an idempotent property repair.  Its own ranges
            ;; stay valid; every other derived consumer observes the suffix.
            (niwa-transcript-source-accept
             (niwa-transcript-cache-source cache))))))
    (setq niwa-transcript--normalized-tick (buffer-modified-tick))))

(defun niwa-transcript-restore-ignored-properties (start end)
  "Restore ignored side-channel properties within START..END."
  (save-match-data
    (let* ((base (niwa-transcript--property-segments start end))
           (ranges (niwa-transcript--structural-ranges start end base)))
      (niwa-transcript--apply-structural-properties
       (cl-remove-if-not (lambda (range)
                           (memq (car range) '(render-data ignored)))
                         ranges)))))



(defun niwa-transcript--org-tool-blocks-overlapping (segments start end)
  "Return org tool block bounds from SEGMENTS overlapping START..END.
Bounds include the `#+begin_tool' and `#+end_tool' marker lines.  The
view uses these structural anchors to repair stale restored
`GPTEL_BOUNDS' that split a single tool block across several property
runs."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (niwa-transcript--search-control-line "^#\\+begin_tool\\b" end)
        ;; Keep the containing property run, but discard completed prefixes.
        ;; Raw property segments are disjoint and ordered by source position.
        (while (and segments
                    (<= (caddr (car segments)) (match-beginning 0)))
          (setq segments (cdr segments)))
        (let* ((block-start (match-beginning 0))
               (marker-end (match-end 0))
               (block-end
                (and (niwa-transcript--org-tool-block-start-p block-start)
                     (niwa-transcript--tool-block-end-from-start block-start)))
               (overlap-p
                (and block-end
                     (or (niwa-transcript--tool-block-overlaps-tool-segment-p
                          segments block-start block-end)
                         (niwa-transcript--tool-block-inside-ignore-segment-p
                          segments block-start block-end)))))
          (when (and block-end (not overlap-p))
            (when-let* ((min-end
                         (niwa-transcript--first-tool-segment-start-after
                          segments block-start end))
                        (retry-min
                         (or (niwa-transcript--tool-block-start-before
                              min-end block-end)
                             min-end)))
              (when-let* ((retry-end
                            (niwa-transcript--tool-block-retry-end
                             block-start nil block-end retry-min)))
                (setq block-end retry-end)
                (setq overlap-p
                      (and block-end
                           (niwa-transcript--tool-block-overlaps-tool-segment-p
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

(defun niwa-transcript--tool-block-overlaps-tool-segment-p
    (segments block-start block-end)
  "Return non-nil when ordered SEGMENTS overlap BLOCK-START..BLOCK-END."
  (catch 'overlap
    (while (and segments (< (cadr (car segments)) block-end))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'tool) (> (caddr seg) block-start))
          (throw 'overlap t))))))

(defun niwa-transcript--tool-block-inside-ignore-segment-p
    (segments block-start block-end)
  "Return non-nil when ordered SEGMENTS has an ignore entry spanning the block.
The block is bounded by BLOCK-START and BLOCK-END."
  (catch 'contains
    (while (and segments (<= (cadr (car segments)) block-start))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'ignored) (<= block-end (caddr seg)))
          (throw 'contains t))))))

(defun niwa-transcript--first-tool-segment-start-after (segments pos limit)
  "Return the first tool start in ordered SEGMENTS after POS and before LIMIT."
  (catch 'start
    (while (and segments (< (cadr (car segments)) limit))
      (let ((seg (pop segments)))
        (when (and (eq (car seg) 'tool) (> (cadr seg) pos))
          (throw 'start (cadr seg)))))))

(defun niwa-transcript--org-tool-block-start-p (pos)
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

(defvar niwa-transcript--tool-block-index nil
  "Buffer-keyed canonical boundary indexes scoped to one projection.")

(defun niwa-transcript--tool-block-bounds-for-run (beg end &optional limit)
  "Return canonical tool bounds overlapping BEG..END, respecting LIMIT.
Within a projection use a lazily built index, invalidated by text or property
changes and narrowing.  Incomplete structural ranges use anchored recovery,
bounded by the preceding validated block so a miss never rescans that history."
  (if (not niwa-transcript--tool-block-index)
      (niwa-transcript--recover-tool-block-bounds beg end limit)
    (let* ((key (list (buffer-modified-tick) (point-min) (point-max)))
           (cached (gethash (current-buffer) niwa-transcript--tool-block-index)))
      (unless (equal key (car cached))
        (setq cached
              (cons key (vconcat (niwa-transcript--org-tool-blocks-overlapping
                                 (niwa-transcript--property-segments (point-min) (point-max))
                                 (point-min) (point-max)))))
        (puthash (current-buffer) cached niwa-transcript--tool-block-index))
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
                (niwa-transcript--recover-tool-block-bounds beg end limit)))))))))

(defun niwa-transcript--recover-tool-block-bounds (seg-start seg-end
                                                          &optional limit)
  "Return recovered org tool block bounds for a tool run.
SEG-START and SEG-END are the bounds of an actual `gptel' tool
property run.  Recovery is anchored to that run rather than to every
marker-looking line in the buffer, so transcript-like text inside a
tool result does not create nested fake tool blocks.  LIMIT, when
non-nil, is the start of the next actual tool run."
  (when-let* ((block-start
               (niwa-transcript--tool-block-start-for-run seg-start seg-end
                                                       limit))
              (block-end
               (niwa-transcript--tool-block-end-from-start block-start limit
                                                        seg-start)))
    (when (and (< block-start seg-end)
               (> block-end seg-start))
      (cons block-start block-end))))

(defun niwa-transcript--tool-block-start-for-run (seg-start seg-end
                                                         &optional limit)
  "Return the structural `#+begin_tool' for tool run SEG-START..SEG-END.
LIMIT bounds the backward search when non-nil."
  (let (found)
    (save-excursion
      (goto-char seg-end)
      (while (re-search-backward "^#\\+begin_tool\\b" nil t)
        (let ((candidate (point)))
          (when (and (< candidate seg-end)
                     (niwa-transcript--org-tool-block-start-p candidate))
            (let ((block-end
                   (niwa-transcript--tool-block-end-from-start candidate limit)))
              (when (and block-end
                         (or (> block-end seg-start)
                             (let ((retry-min
                                    (or (niwa-transcript--tool-block-start-before
                                         seg-start block-end)
                                        seg-start)))
                               (niwa-transcript--tool-block-retry-end
                                candidate limit block-end retry-min))))
                (when (<= block-end seg-start)
                  (let ((retry-min
                         (or (niwa-transcript--tool-block-start-before
                              seg-start block-end)
                             seg-start)))
                    (setq block-end
                          (niwa-transcript--tool-block-retry-end
                           candidate limit block-end retry-min))))
                (when (and block-end (> block-end seg-start))
                  ;; Keep scanning backward and retain the earliest
                  ;; containing block.  That rejects nested transcript text
                  ;; when stale bounds start after the nested marker.
                  (setq found candidate))))))))
    found))

(defun niwa-transcript--tool-block-retry-end (block-start limit block-end
                                                       retry-min)
  "Return an extended structural close for BLOCK-START, or nil.
LIMIT bounds the search when non-nil.  BLOCK-END is the normal close
for BLOCK-START and RETRY-MIN is a later
marker-looking block start where the restored tool run begins.  Recovery
is accepted only when the outer block extends beyond that later block's
own first close, which distinguishes nested-looking tool text from a
completed earlier block followed by a real tool call."
  (when (niwa-transcript--tool-block-retry-gap-p block-end retry-min)
    (let ((retry-end
           (niwa-transcript--tool-block-end-from-start block-start limit
                                                    retry-min))
          (inner-close
           (niwa-transcript--first-tool-close-after retry-min limit)))
      (when (and retry-end inner-close (> retry-end inner-close))
        retry-end))))

(defun niwa-transcript--tool-block-start-before (pos limit)
  "Return the nearest structural tool block start before POS after LIMIT."
  (let (found)
    (save-excursion
      (goto-char pos)
      (while (and (not found)
                  (re-search-backward "^#\\+begin_tool\\b" limit t))
        (when (niwa-transcript--org-tool-block-start-p (point))
          (setq found (point)))))
    found))

(defun niwa-transcript--tool-block-retry-gap-p (block-end min-end)
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
                (niwa-transcript--range-has-gptel-prop-p
                 block-end min-end '(tool))
                (not (niwa-transcript--range-has-gptel-prop-p
                      block-end min-end '(response ignored))))))))

(defun niwa-transcript--first-tool-close-after (pos &optional limit)
  "Return the first non-response `#+end_tool' marker end after POS.
LIMIT bounds the search when non-nil."
  (let (found)
    (save-excursion
      (goto-char pos)
      (while (and (not found)
                  (re-search-forward "^#\\+end_tool[^\n]*\n?" limit t))
        (let ((marker-start (match-beginning 0))
              (marker-end (match-end 0)))
          (unless (eq (niwa-transcript--classify-gptel-prop
                       (get-text-property marker-start 'gptel))
                      'response)
            (setq found marker-end)))))
    found))

(defun niwa-transcript--range-has-gptel-prop-p (start end types)
  "Return non-nil when START..END contain a `gptel' property in TYPES."
  (let (found)
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not found))
        (let ((type (niwa-transcript--classify-gptel-prop
                     (get-text-property (point) 'gptel))))
          (when (memq type types)
            (setq found t))
          (goto-char (or (next-single-property-change (point) 'gptel nil end)
                         end)))))
    found))

(defun niwa-transcript--blank-gap-p (start end)
  "Return non-nil if START..END is only whitespace."
  (or (>= start end)
      (string-empty-p
       (string-trim
        (buffer-substring-no-properties start end)))))

(defconst niwa-transcript--mailbox-block-specs
  '((agent-result
     :open "<agent-result\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-result>")
    (agent-message
     :open "<agent-message\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
     :close "</agent-message>"))
  "Structural mailbox block regexes.

The open regex captures the canonical sender path in match group 1.")

(defun niwa-transcript--mailbox-spec (kind key)
  "Return mailbox KIND spec value for KEY."
  (plist-get (cdr (assq kind niwa-transcript--mailbox-block-specs)) key))

(defun niwa-transcript--mailbox-open-at-point (kind limit)
  "Return mailbox open metadata for KIND at point before LIMIT.
The returned plist contains `:kind', `:id', `:open-start', and
`:open-end'.  Agent results also carry `:outcome', one of `completed',
`errored', `interrupted', or nil for missing or unknown metadata."
  (let ((regex (niwa-transcript--mailbox-spec kind :open)))
    (when (and regex
               (<= (point) limit)
               (looking-at regex)
               (<= (match-end 0) limit))
      (let ((open (list :kind kind
                        :id (match-string-no-properties 1)
                        :open-start (match-beginning 0)
                        :open-end (match-end 0))))
        (when (eq kind 'agent-result)
          (setq open
                (plist-put
                 open :outcome
                 (save-match-data
                   (save-excursion
                     (when (re-search-forward
                            "\\s-outcome=\"\\([^\"]*\\)\""
                            (plist-get open :open-end) t)
                       (cdr (assoc (match-string-no-properties 1)
                                   '(("completed" . completed)
                                     ("errored" . errored)
                                     ("interrupted" . interrupted))))))))))
        open))))

(defun niwa-transcript--mailbox-close-line-regexp (close-tag)
  "Return a line-oriented regexp for CLOSE-TAG."
  (concat "^[ \t]*" (regexp-quote close-tag) "[ \t]*\\(?:\n\\|\\'\\)"))

(defun niwa-transcript--mailbox-nested-open-regexp (close-tag)
  "Return a broad nested mailbox opener regexp for CLOSE-TAG."
  (pcase close-tag
    ("</agent-result>" "<agent-result\\(?:\\s-\\|>\\)")
    ("</agent-message>" "<agent-message\\(?:\\s-\\|>\\)")))

(defun niwa-transcript--mailbox-structural-close-at (start close-re)
  "Return close bounds when START is on a structural CLOSE-RE line."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (when (looking-at close-re)
      (cons (match-beginning 0) (match-end 0)))))

(defun niwa-transcript--mailbox-find-close (open-regexp close-tag limit)
  "Return structural close bounds for CLOSE-TAG before LIMIT.
OPEN-REGEXP matches normal nested mailbox openings of the same kind.
The return value is a cons cell `(BODY-END . CLOSE-END)'.  Close
markers are accepted only as standalone lines; matching nested openings
first keeps literal nested mailbox examples from ending the outer card."
  (let* ((close-re (niwa-transcript--mailbox-close-line-regexp close-tag))
         (loose-close-re (regexp-quote close-tag))
         (nested-open-re (niwa-transcript--mailbox-nested-open-regexp close-tag))
         (event-re (concat "\\(?:" open-regexp "\\)"
                           (when nested-open-re
                             (concat "\\|\\(?:" nested-open-re "\\)"))
                           "\\|\\(?:" loose-close-re "\\)"))
         (depth 0))
    (catch 'done
      (while (re-search-forward event-re limit t)
        (let ((start (match-beginning 0)))
          (cond
           ((niwa-transcript--mailbox-structural-close-at start close-re)
            (if (> depth 0)
                (cl-decf depth)
              (throw 'done
                     (niwa-transcript--mailbox-structural-close-at
                      start close-re))))
           ((save-excursion
              (goto-char start)
              (looking-at loose-close-re))
            (when (> depth 0)
              (cl-decf depth)))
           (t
            (cl-incf depth)))))
      nil)))

(defun niwa-transcript--mailbox-block-at-point (kind limit)
  "Return mailbox block metadata for KIND at point before LIMIT.
The returned plist includes open metadata plus `:body-start',
`:body-end', and `:close-end'."
  (let ((open (niwa-transcript--mailbox-open-at-point kind limit))
        close)
    (when open
      (save-excursion
        (goto-char (plist-get open :open-end))
        (setq close
              (niwa-transcript--mailbox-find-close
               (niwa-transcript--mailbox-spec kind :open)
               (niwa-transcript--mailbox-spec kind :close)
               limit)))
      (when close
        (append open
                (list :body-start (plist-get open :open-end)
                      :body-end (car close)
                      :close-end (cdr close)))))))

(defun niwa-transcript--mailbox-any-block-at-point (limit)
  "Return mailbox block metadata at point before LIMIT, or nil."
  (or (niwa-transcript--mailbox-block-at-point 'agent-result limit)
      (niwa-transcript--mailbox-block-at-point 'agent-message limit)))

(defun niwa-transcript--mailbox-start-in-range-p (start end)
  "Return non-nil if a mailbox opening tag appears in START..END."
  (save-excursion
    (goto-char start)
    (or (re-search-forward "<agent-result\\(?:\\s-\\|>\\)" end t)
        (progn
          (goto-char start)
          (re-search-forward "<agent-message\\(?:\\s-\\|>\\)" end t)))))

(defun niwa-transcript--tool-block-gap-crosses-boundary-p (start end)
  "Return non-nil when START..END crosses non-tool conversation content."
  (and (< start end)
       (or (niwa-transcript--range-has-gptel-prop-p start end
                                                 '(response ignored))
           (niwa-transcript--mailbox-start-in-range-p start end)
           (save-excursion
             (goto-char start)
             (re-search-forward
              "^\\(?:<system-reminder>\\|<hook-context>\\)"
              end t)))))

(defun niwa-transcript--tool-block-truncated-before-p (start end)
  "Return non-nil if a niwa truncation marker appears in START..END."
  (and (< start end)
       (save-excursion
         (goto-char start)
         (re-search-forward
          "\\[niwa: tool output truncated; omitted [0-9]+ chars\\]"
          end t))))

(defun niwa-transcript--tool-block-call-readable-before-p (start end)
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

(defun niwa-transcript--tool-block-end-from-start (block-start &optional limit
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
                  (niwa-transcript--search-control-line "^#\\+\\(begin_tool\\b\\|end_tool[^\n]*\n?\\)"
                                     limit))
        (let ((marker-start (match-beginning 0))
              (marker-end (match-end 0)))
          (cond
           ((and (not last-close)
                 (save-excursion
                   (goto-char marker-start)
                   (looking-at-p "^#\\+begin_tool\\b"))
                 (niwa-transcript--org-tool-block-start-p marker-start)
                 (niwa-transcript--range-has-gptel-prop-p
                  block-start marker-start '(tool))
                 (not (niwa-transcript--same-tool-run-before-p
                       marker-start block-start))
                 (or (niwa-transcript--tool-block-truncated-before-p
                      block-start marker-start)
                     (not (niwa-transcript--tool-block-call-readable-before-p
                           block-start marker-start))))
            (setq last-close marker-start
                  done t))
           ((and last-close
                 (or (not min-end) (> last-close min-end))
                 (save-excursion
                   (goto-char marker-start)
                   (looking-at-p "^#\\+begin_tool\\b"))
                 (niwa-transcript--org-tool-block-start-p marker-start)
                 (or (not (niwa-transcript--same-tool-run-before-p
                           marker-start block-start))
                     (niwa-transcript--tool-block-gap-crosses-boundary-p
                      last-close marker-start)
                     (niwa-transcript--blank-gap-p
                      last-close marker-start)))
            (setq done t))
           ((save-excursion
              (goto-char marker-start)
              (looking-at-p "^#\\+end_tool"))
            (let ((marker-type
                   (niwa-transcript--classify-gptel-prop
                    (get-text-property marker-start 'gptel))))
              (unless (and (eq marker-type 'response)
                           (or (niwa-transcript--range-has-gptel-prop-p
                                (or last-close block-start)
                                marker-start '(response ignored))
                               (not (or
                                     (niwa-transcript--range-has-gptel-prop-p
                                      (or last-close block-start)
                                      marker-start '(tool))
                                     (and last-close
                                          (niwa-transcript--gap-body-text-p
                                           last-close marker-start))))))
                (setq last-close marker-end))))))))
    last-close))

(defun niwa-transcript--same-tool-run-before-p (pos limit)
  "Return non-nil if POS has the same tool prop as prior text after LIMIT."
  (let ((prop (get-text-property pos 'gptel))
        found)
    (when (eq (niwa-transcript--classify-gptel-prop prop) 'tool)
      (save-excursion
        (goto-char pos)
        (while (and (> (point) limit) (not found))
          (backward-char 1)
          (unless (memq (char-after) '(?\s ?\t ?\n ?\r))
            (setq found (equal (get-text-property (point) 'gptel)
                               prop))))))
    found))

(defun niwa-transcript--gap-body-text-p (start end)
  "Return non-nil when START..END resemble unclassified tool body text."
  (and (< start end)
       (not (niwa-transcript--range-has-gptel-prop-p
             start end '(response ignored)))
       (string-match-p "[^ \t\n]"
                       (buffer-substring-no-properties start end))))


(defun niwa-transcript--own-line-prompt-p (seg)
  "Return non-nil when user SEG is a prompt rather than a stale response tail.
A prompt starts on its own line after the response separator; a stale
tail left by drifted `GPTEL_BOUNDS' continues the response's last line."
  (and (eq (car seg) 'user)
       (save-excursion
         (goto-char (cadr seg))
         (skip-chars-forward " \t" (caddr seg))
         (and (< (point) (caddr seg)) (eq (char-after) ?\n)
              (not (niwa-transcript--whitespace-segment-p seg))))))

(defun niwa-transcript--repair-response-fragment-segments (segments)
  "Return SEGMENTS with stale response fragments reclassified.
Older or externally edited transcripts can restore `GPTEL_BOUNDS' a
few characters into or before the end of an assistant response, leaving
response text as nil-property `user' or `ignored' segments.  Such
fragments should render as part of the assistant response, not as fake
thinking blocks or user turns."
  (let (converted prev-type rest)
    (setq rest (niwa-transcript--merge-adjacent-segments segments '(user)))
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
                             "\\`[ \t\r\n]*<!-- niwa-hook-audit -->"
                             (buffer-substring-no-properties
                              (cadr seg) (caddr seg))))
                       ;; A generated prompt precedes its display record.
                       (not (niwa-transcript--own-line-prompt-p seg)))
                  (and (eq type 'user)
                       (or (and (eq prev-type 'response)
                                (eq next-type 'ignored)
                                ;; Guest attribution follows a real prompt too.
                                (not (niwa-transcript--own-line-prompt-p seg))
                                (string-match-p
                                 "\\`[ \t\r\n]*<!-- niwa-hook-audit -->"
                                 (buffer-substring-no-properties
                                  (cadr (cadr rest))
                                  (caddr (cadr rest)))))
                       (and (memq prev-type
                                  '(tool reasoning mailbox reminder
                                    render-data ignored))
                            (eq next-type 'response)
                            (niwa-transcript--response-fragment-segment-p
                             seg (cadr rest)))
                       (niwa-transcript--response-continuation-segment-p
                        prev-seg seg (cadr rest)))))))
        (push (if convert-p
                  (list 'response (cadr seg) (caddr seg))
                seg)
              converted)
        (setq prev-type (if convert-p 'response type))
        (setq rest (cdr rest))))
    (niwa-transcript--merge-adjacent-segments (nreverse converted) '(response))))

(defun niwa-transcript--response-fragment-segment-p (seg next-seg)
  "Return non-nil when SEG resembles a stale prefix of NEXT-SEG.
This is deliberately conservative so a real user prompt in a
`tool -> user -> response' sequence does not get swallowed into the
assistant turn."
  (and next-seg
       (<= (- (caddr seg) (cadr seg)) 120)
       (let* ((text (buffer-substring-no-properties (cadr seg) (caddr seg)))
              (trimmed (string-trim text))
              (next-first
               (save-excursion
                 (goto-char (cadr next-seg))
                 (skip-chars-forward " \t\n\r" (caddr next-seg))
                 (and (< (point) (caddr next-seg)) (char-after)))))
         (and (not (string-empty-p trimmed))
              (<= (length text) 120)
              (not (string-match-p "\n" text))
              (not (string-match-p "\\`\\(?:\\*+\\|#+\\|[-+*]\\)[ \t]"
                                   trimmed))
              (not (niwa-transcript--scaffolding-only-p
                    (current-buffer) (cadr seg) (caddr seg)))
              next-first
              (let ((ch next-first))
                (or (and (>= ch ?a) (<= ch ?z))
                    (memq ch '(?, ?. ?\; ?: ?\) ?\] ?\}))))))))

(defun niwa-transcript--response-continuation-segment-p (prev-seg seg next-seg)
  "Return non-nil when SEG continues PREV-SEG and NEXT-SEG mid-line."
  (and prev-seg
       next-seg
       (eq (car prev-seg) 'response)
       (eq (car seg) 'user)
       (eq (car next-seg) 'response)
       (< (cadr seg) (caddr seg))
       (> (cadr seg) (point-min))
       (not (memq (char-before (cadr seg)) '(?\n ?\r)))
       (niwa-transcript--response-continuation-text-p
        (buffer-substring-no-properties (cadr seg) (caddr seg)))))

(defun niwa-transcript--response-continuation-text-p (text)
  "Return non-nil when TEXT can be repaired as response continuation."
  (let ((trimmed (string-trim text)))
    (and (not (string-empty-p trimmed))
         ;; A trailing newline can be same-line response glue, but a
         ;; new nonblank line may be the next real prompt.
         (not (string-match-p "[\n\r][ \t\r\n]*[^ \t\r\n]" text))
         (not (string-match-p "\\`\\(?:\\*+\\|#+\\|[-+*]\\)[ \t]"
                              trimmed)))))

(defun niwa-transcript--merge-adjacent-segments (segments types)
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

(defun niwa-transcript--summary-truncation-marker (omitted)
  "Return the evidence marker for OMITTED tool-result characters."
  (format "\n[niwa: tool output truncated; omitted %d chars]\n" omitted))

(defun niwa-transcript--summary-truncate-string (string limit)
  "Return tool argument STRING shortened to LIMIT characters."
  (if (> (length string) limit)
      (concat (substring string 0 limit)
              (format "\n[niwa: string argument truncated; omitted %d chars]"
                      (- (length string) limit)))
    string))

(defun niwa-transcript--summary-truncate-args (value limit)
  "Return tool argument VALUE with strings shortened to LIMIT characters."
  (cond
   ((stringp value)
    (niwa-transcript--summary-truncate-string value limit))
   ((vectorp value)
    (vconcat
     (mapcar (lambda (item)
               (niwa-transcript--summary-truncate-args item limit))
             value)))
   ((consp value)
    (cons (niwa-transcript--summary-truncate-args (car value) limit)
          (niwa-transcript--summary-truncate-args (cdr value) limit)))
   (t value)))

(defun niwa-transcript--summary-tool-sexp-start (text)
  "Return the readable tool-call plist start in TEXT, or nil."
  ;; Canonical tool spans can include the preceding response separator.
  (if (string-match "\\`[ \t\r\n]*#\\+begin_tool\\b[^\n]*\n" text)
      (let ((pos (match-end 0)))
        (while (and (< pos (length text))
                    (memq (aref text pos) '(?\s ?\t ?\r ?\n)))
          (setq pos (1+ pos)))
        (when (and (string-match "(\\s-*:name\\_>" text pos)
                   (= (match-beginning 0) pos))
          pos))
    (when (string-match "\\`[ \t\n]*(\\s-*:name\\_>" text)
      (match-beginning 0))))

(defun niwa-transcript--summary-media-placeholder (item)
  "Return a textual context-summary placeholder for native media ITEM."
  (format "[media: %s; MIME %s%s]"
          (or (plist-get item :kind) "unknown")
          (or (plist-get item :mime) "unknown")
          (if-let* ((path (plist-get item :path)))
              (format "; path %s" path)
            "")))

(defun niwa-transcript--instruction-tool-p (form)
  "Return non-nil if transcript call FORM delivers a Skill instruction body.
Interpret no guest code.  Validate a ToolCall expression using the closed
parser so quoted names or composed results are not mistaken for instructions."
  (or (equal (plist-get form :name) "Skill")
      (and (equal (plist-get form :name) "ToolCall")
           (condition-case nil
               (let ((state (niwa-ptc-start
                             (plist-get (plist-get form :args) :expression)
                             '("Skill") '("Skill"))))
                 (unwind-protect
                     (equal "Skill" (niwa-ptc-state-direct-tool state))
                   (niwa-ptc-close state)))
             (error nil)))))

(defun niwa-transcript--summary-tool-parts
    (text cap tool-results-dir tool-id)
  "Return neutral call/result evidence parts for tool TEXT.
CAP bounds ordinary result characters; Skill instruction results stay complete.
TOOL-RESULTS-DIR and TOOL-ID resolve
trusted native media metadata without returning payload bytes."
  (let* ((extracted
          (niwa-tool-media-extract
           text tool-results-dir tool-id t))
         (visible (car extracted))
         (media (cdr extracted))
         (sexp-start (niwa-transcript--summary-tool-sexp-start visible))
         call result)
    (when sexp-start
      (condition-case nil
          (let* ((read-result (read-from-string visible sexp-start))
                 (form (car read-result))
                 (form-end (cdr read-result)))
            (when (and (listp form) (stringp (plist-get form :name)))
              (setq call
                    (prin1-to-string
                     (niwa-transcript--summary-truncate-args
                      form (max 80 (min 800 cap)))))
              (let* ((close (string-match "\n#\\+end_tool[^\n]*\n?\\'"
                                          visible form-end))
                     (body (string-trim
                            (substring visible form-end
                                       (or close (length visible))))))
                (setq result
                      ;; Skill results carry authored guidance, also after
                      ;; cold restore has lost the live invocation records.
                      (if (and (not (niwa-transcript--instruction-tool-p form))
                               (> (length body) cap))
                          (concat
                           (substring body 0 cap)
                           (niwa-transcript--summary-truncation-marker
                            (- (length body) cap)))
                        body)))))
        (error nil)))
    (list
     (or call "(unparseable tool-call metadata)")
     (string-join
      (delq nil
            (append
             (and result (not (string-empty-p result)) (list result))
             (mapcar #'niwa-transcript--summary-media-placeholder media)))
      "\n"))))

(defun niwa-transcript--summary-evidence-item (provenance text)
  "Return one neutral evidence item labelled with PROVENANCE and TEXT."
  (format "--- evidence item; provenance: %s ---\n%s\n--- end evidence item ---"
          provenance (string-trim text)))

(cl-defun niwa-transcript-project-evidence
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
  (niwa-transcript-project-segments
   (cl-loop for (begin . finish) in ranges
            append (cl-loop for (type start end) in
                            (niwa-transcript-segments begin finish)
                            collect (list type (max begin start) (min finish end))))
   :tool-output-max tool-output-max
   :tool-results-dir tool-results-dir
   :skill-provenance skill-provenance))

(cl-defun niwa-transcript-project-segments
    (segments &key (tool-output-max 8000) tool-results-dir skill-provenance)
  "Format already classified SEGMENTS from the current buffer as evidence.

SEGMENTS uses the canonical (TYPE START END) representation returned by
`niwa-transcript-segments'. Callers may select spans before formatting;
this function never classifies them again. TOOL-OUTPUT-MAX bounds ordinary
tool results, TOOL-RESULTS-DIR resolves media, and SKILL-PROVENANCE records
previously selected skill invocations. These options have the same meaning
as in `niwa-transcript-project-evidence'."
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
                 (dolist (record (niwa-transcript-audit-records
                                  text 'injected-reminders))
                   (dolist (entry (plist-get record :items))
                     (push (niwa-transcript--summary-evidence-item
                            "system-reminder" (plist-get entry :body))
                           items))))
                ('tool
                 (pcase-let
                     ((`(,call ,result)
                       (niwa-transcript--summary-tool-parts
                        text tool-output-max tool-results-dir
                        (niwa-transcript--tool-id-in-range start end))))
                   (push
                    (niwa-transcript--summary-evidence-item
                     "tool-call" call)
                    items)
                   (push
                    (niwa-transcript--summary-evidence-item
                     "tool-result" result)
                    items)))
                (_
                 (push
                  (niwa-transcript--summary-evidence-item
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
      (push (niwa-transcript--summary-evidence-item
             "skill-invocation" skill)
            items))
    ;; Tool result bodies keep their `gptel' text properties through
    ;; media extraction and formatting.  The evidence string is later
    ;; inserted into gptel's prompt buffer, where a stale `(tool . id)'
    ;; span makes the provider parser `read' arbitrary result text as a
    ;; tool-call plist and fail.  Frozen evidence is plain text.
    (substring-no-properties
     (mapconcat #'identity (nreverse items) "\n\n"))))

(provide 'niwa-transcript)
;;; niwa-transcript.el ends here
