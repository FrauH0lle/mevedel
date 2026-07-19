;;; mevedel-transcript.el --- Transcript span classification -*- lexical-binding: t -*-

;;; Commentary:

;; Canonical classification and property restoration for the gptel data
;; buffer transcript.  Callers decide how to render, persist, or compact the
;; resulting structural spans.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'subr-x)

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
    ('ignore 'ignored)
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
                (forward-sexp 1)
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

(defun mevedel-transcript--delimited-ranges (type open close start end)
  "Return complete TYPE ranges delimited by OPEN and CLOSE in START..END."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward open end t)
        (let ((block-start (match-beginning 0))
              (next (match-end 0)))
          (if (re-search-forward close end t)
              (let ((block-end (match-end 0)))
                (push (list type block-start block-end) ranges)
                (goto-char block-end))
            (goto-char next)))))
    (nreverse ranges)))

(defun mevedel-transcript--mailbox-ranges (start end)
  "Return complete mailbox ranges in START..END."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward
              "^\\(?:\\*+ \\)?<\\(?:agent-result\\|agent-message\\)\\(?:\\s-\\|>\\)"
              end t)
        (let ((line-start (match-beginning 0)))
          (goto-char line-start)
          (search-forward "<" end t)
          (backward-char 1)
          (if-let* ((block
                     (mevedel-transcript--mailbox-any-block-at-point end)))
              (progn
                (push (list 'mailbox line-start
                            (plist-get block :close-end))
                      ranges)
                (goto-char (plist-get block :close-end)))
            (goto-char (1+ line-start))))))
    (nreverse ranges)))

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
    (when (and base (not (eq (car base) 'user)))
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

(defun mevedel-transcript--range-inside-tool-segment-p (range segments)
  "Return non-nil when RANGE is contained by a raw tool entry in SEGMENTS."
  (cl-find-if
   (lambda (seg)
     (and (eq (car seg) 'tool)
          (<= (cadr seg) (cadr range))
          (<= (caddr range) (caddr seg))))
   segments))

(defun mevedel-transcript--unparseable-tool-ranges
    (start end base-segments tool-ranges)
  "Return stale tool blocks in START..END absent from TOOL-RANGES.
BASE-SEGMENTS supplies the raw property spans used to identify closed
blocks that still carry stale tool properties."
  (let (ranges)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^#\\+begin_tool\\b" end t)
        (let ((block-start (match-beginning 0)))
          (if (re-search-forward "^#\\+end_tool[^\n]*\n?" end t)
              (let ((block-end (match-end 0)))
                (when (and
                       (cl-find-if
                        (lambda (seg)
                          (and (eq (car seg) 'tool)
                               (< (cadr seg) block-end)
                               (> (caddr seg) block-start)))
                        base-segments)
                       (not (cl-find-if
                             (lambda (range)
                               (and (<= (cadr range) block-start)
                                    (<= block-end (caddr range))))
                             tool-ranges)))
                  (push (list 'ignored block-start block-end) ranges)))
            (goto-char (1+ block-start))))))
    (nreverse ranges)))

(defun mevedel-transcript--structural-ranges (start end base-segments)
  "Return canonical control ranges in START..END.
BASE-SEGMENTS are raw `gptel' property runs used to validate persisted
tool blocks.  Each result is `(TYPE START END VALUE...)'."
  (let ((ranges
         (append
          (mevedel-transcript--delimited-ranges
           'reasoning "^#\\+begin_reasoning\\b" "^#\\+end_reasoning[^\n]*\n?"
           start end)
          (mevedel-transcript--mailbox-ranges start end)
          (mevedel-transcript--delimited-ranges
           'reminder "^\\(?:\\*+ \\)?<system-reminder>[ \t]*$"
           "^</system-reminder>[ \t]*\n?" start end)
          (mevedel-transcript--delimited-ranges
           'hook-context "^<hook-context>[ \t]*$"
           "^</hook-context>[ \t]*\n?" start end)
          (mevedel-transcript--delimited-ranges
           'render-data "^<!-- mevedel-render-data -->[ \t]*$"
           "^<!-- /mevedel-render-data -->[ \t]*\\(?:\n\\(?:[ \t\r]*\n\\)*\\)?"
           start end)
          (mevedel-transcript--delimited-ranges
           'prompt "^:PROMPT:[ \t]*$" "^:END:[ \t]*\n?" start end)
          (mevedel-transcript--delimited-ranges
           'ignored "^<!-- mevedel-hook-audit -->[ \t]*$"
           "^<!-- /mevedel-hook-audit -->[ \t]*\\(?:\n[ \t\r]*\\)*"
           start end))))
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
    (let (accepted)
      (dolist (range (sort ranges (lambda (a b) (< (cadr a) (cadr b)))))
        (when (or (not (memq (car range) '(mailbox reminder)))
                  (and (eq (car range) 'mailbox)
                       (mevedel-transcript--mailbox-control-context-p
                        range base-segments))
                  (and (eq (car range) 'mailbox)
                       (cl-find-if (lambda (prior)
                                     (eq (car prior) 'mailbox))
                                   accepted))
                  (mevedel-transcript--control-prefix-p
                   range base-segments accepted))
          (push range accepted)))
      (setq ranges (nreverse accepted)))
    (sort ranges
          (lambda (a b)
            (let ((pa (mevedel-transcript--structure-priority a))
                  (pb (mevedel-transcript--structure-priority b)))
              (if (= pa pb)
                  (< (cadr a) (cadr b))
                (< pa pb)))))))

(defun mevedel-transcript--overlay-range (segments range)
  "Overlay canonical RANGE on role SEGMENTS."
  (let ((start (cadr range))
        (end (caddr range))
        (segment (list (car range) (cadr range) (caddr range)))
        out inserted)
    (dolist (seg segments)
      (let ((seg-start (cadr seg))
            (seg-end (caddr seg)))
        (cond
         ((or (<= seg-end start) (>= seg-start end))
          (when (and (not inserted) (>= seg-start end))
            (push segment out)
            (setq inserted t))
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
    (nreverse out)))

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

(defun mevedel-transcript-segments (start end)
  "Return canonical transcript segments between START and END.
Each segment is `(TYPE START END)'.  TYPE is `user',
`response', `tool', `reasoning', `mailbox', `reminder',
`hook-context', `render-data', `prompt', or
`ignored'.  Structural control ranges override stale `gptel' property
runs and incomplete control text remains ordinary transcript text."
  (let* ((segments (mevedel-transcript--property-segments start end))
         (scan-start (if segments (cadr (car segments)) start))
         (scan-end (if segments (caddr (car (last segments))) end)))
    (dolist (range (mevedel-transcript--structural-ranges
                    scan-start scan-end segments))
      (unless (and (memq (car range) '(render-data ignored))
                   (mevedel-transcript--range-inside-tool-segment-p
                    range segments))
        (setq segments (mevedel-transcript--overlay-range segments range))))
    (mevedel-transcript--merge-adjacent-segments
     (mevedel-transcript--repair-response-fragment-segments
      (mevedel-transcript--repair-mailbox-prose-segments
       (mevedel-transcript--absorb-structural-whitespace segments)))
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
        ((or 'reasoning 'render-data 'prompt 'ignored)
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
        ((or 'reasoning 'render-data 'prompt 'ignored)
         (eq prop 'ignore))
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

(defun mevedel-transcript-normalize-properties ()
  "Normalize structural transcript properties in the current Org buffer."
  (when (derived-mode-p 'org-mode)
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
               ranges))))))))

(defun mevedel-transcript-restore-ignored-properties (start end)
  "Restore ignored side-channel properties within START..END."
  (save-match-data
    (let* ((base (mevedel-transcript--property-segments start end))
           (ranges (mevedel-transcript--structural-ranges start end base))
           ignored)
      (dolist (range ranges)
        (when (memq (car range) '(render-data ignored))
          (push range ignored)))
      (mevedel-transcript--apply-structural-properties (nreverse ignored)))))



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
              (and (eq (car seg) 'ignored)
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
                    block-end min-end '(response ignored)))))))

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
                   (or (and (memq prev-type
                                  '(tool reasoning mailbox reminder
                                    render-data ignored))
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
