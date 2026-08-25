;;; mevedel-compact-evidence.el -- Compaction transcript evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Owns transcript projection, turn boundaries, and structure-safe tool output
;; truncation for compaction.  It does not mutate a target or own request state.

;;; Code:

(require 'cl-lib)
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'subr-x)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel--request-alist)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-require-path
                  "mevedel-agents" (invocation))
(defvar mevedel-agent-task-path-property)

;; `mevedel-compact-estimation'
(declare-function mevedel-compact-estimation-usable-tokens
                  "mevedel-compact-estimation" ())
(autoload 'mevedel-compact-estimation-usable-tokens
  "mevedel-compact-estimation")

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-segment-summary-bounds
                  "mevedel-session-artifacts" ())
(declare-function mevedel-session-artifacts-strip-summary-handoff-prefix
                  "mevedel-session-artifacts" (summary))
(autoload 'mevedel-session-artifacts-segment-summary-bounds
  "mevedel-session-artifacts")
(autoload 'mevedel-session-artifacts-strip-summary-handoff-prefix
  "mevedel-session-artifacts")

;; `mevedel-structs'
(declare-function mevedel-session-invoked-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-tool-results-directory
                  "mevedel-structs" (session))
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-agent-path
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-args
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-name
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-origin
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-role
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-turn
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--agent-invocation)
(defvar mevedel--session)

;; `mevedel-transcript'
(declare-function mevedel-transcript--user-prompt-start
                  "mevedel-transcript" (pos next prop &optional state))
(declare-function mevedel-transcript-prompt-scan-state
                  "mevedel-transcript" ())
(declare-function mevedel-transcript-project-evidence
                  "mevedel-transcript" (ranges &rest keys))
(declare-function mevedel-transcript-segments
                  "mevedel-transcript" (start end))

;; `mevedel-transcript-audit'
(declare-function mevedel--strip-hook-audit-blocks
                  "mevedel-transcript-audit" (text))
(declare-function mevedel-transcript-audit-records
                  "mevedel-transcript-audit" (text &optional type))
(declare-function mevedel-transcript-buffer-directive-ranges
                  "mevedel-transcript-audit" (&optional allow-open))
(declare-function mevedel-transcript-exclude-directive-turns
                  "mevedel-transcript-audit" (&optional fsm))

;; `org'
(declare-function org-find-property "org" (property &optional value))

(defcustom mevedel-compact-evidence-tail-turns 2
  "Target number of recent complete turns to preserve verbatim."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-evidence-tail-budget 0.25
  "Maximum fraction of usable context reserved for preserved tail."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (<= 0.0 value)
                  (<= value 1.0)))))
  :group 'mevedel)

(defcustom mevedel-compact-evidence-tail-tool-output-max 4000
  "Per-tool-result character cap inside the preserved tail."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-evidence-body-tool-output-max 8000
  "Per-tool-result character cap inside the compaction request body."
  :type 'natnum
  :group 'mevedel)

(defun mevedel-compact-evidence--tool-output-prop-p (prop)
  "Return non-nil if PROP is a gptel tool output span."
  (and (consp prop) (eq (car prop) 'tool)))

(defun mevedel-compact-evidence--truncation-marker (omitted)
  "Return the marker inserted when OMITTED tool-output chars are removed."
  (format "\n[mevedel: tool output truncated; omitted %d chars]\n"
          omitted))

(defun mevedel-compact-evidence--string-arg-marker (omitted)
  "Return the marker inserted when OMITTED string-argument chars are removed."
  (format "\n[mevedel: string argument truncated; omitted %d chars]"
          omitted))

(defun mevedel-compact-evidence--truncate-string-arg (string limit)
  "Return STRING shortened to LIMIT chars, preserving a truncation marker."
  (if (and (integerp limit) (> (length string) limit))
      (concat (substring string 0 limit)
              (mevedel-compact-evidence--string-arg-marker
               (- (length string) limit)))
    string))

(defun mevedel-compact-evidence--truncate-tool-args (value limit)
  "Return VALUE with nested string arguments shortened to LIMIT chars."
  (cond
   ((stringp value)
    (mevedel-compact-evidence--truncate-string-arg value limit))
   ((vectorp value)
    (vconcat (mapcar (lambda (item)
                       (mevedel-compact-evidence--truncate-tool-args item limit))
                     value)))
   ((consp value)
    (cons (mevedel-compact-evidence--truncate-tool-args (car value) limit)
          (mevedel-compact-evidence--truncate-tool-args (cdr value) limit)))
   (t value)))

(defun mevedel-compact-evidence--tool-arg-limit (cap)
  "Return the string argument retention limit for tool CAP."
  (max 80 (min 800 (or cap 800))))

(defun mevedel-compact-evidence--truncate-tool-header (text limit)
  "Return TEXT with a trailing readable org tool header shortened to LIMIT."
  (if (string-match "#\\+begin_tool[[:space:]]+" text)
      (condition-case nil
          (let* ((form-start (match-end 0))
                 (read-result (read-from-string text form-start))
                 (form (car read-result))
                 (form-end (cdr read-result))
                 (trailing (substring text form-end)))
            (if (and (consp form)
                     (string-match-p "\\`[[:space:]]*\\'" trailing))
                (concat (substring text 0 form-start)
                        (prin1-to-string
                         (mevedel-compact-evidence--truncate-tool-args form limit))
                        trailing)
              text))
        (error text))
    text))

(defun mevedel-compact-evidence--escape-tool-body-markers (text)
  "Return TEXT with org tool markers escaped for use inside tool bodies."
  (replace-regexp-in-string "^#\\+\\(begin\\|end\\)_tool" "# +\\1_tool" text
                            nil nil))

(defun mevedel-compact-evidence--tool-output-close (text)
  "Return the trailing org tool close marker in TEXT, or nil."
  (when (string-match "\n#\\+end_tool[^\n]*\n?\\'" text)
    (cons (match-beginning 0) (match-end 0))))

(defun mevedel-compact-evidence--raw-tool-truncation (text cap)
  "Return TEXT truncated to CAP chars with the standard omitted marker."
  (concat (substring text 0 cap)
          (mevedel-compact-evidence--truncation-marker (- (length text) cap))))

(defun mevedel-compact-evidence--truncate-tool-body (body cap)
  "Return BODY truncated to CAP chars with the standard omitted marker."
  (if (and (integerp cap) (> (length body) cap))
      (concat (mevedel-compact-evidence--escape-tool-body-markers
               (substring body 0 cap))
              (mevedel-compact-evidence--truncation-marker (- (length body) cap)))
    (mevedel-compact-evidence--escape-tool-body-markers body)))

(defun mevedel-compact-evidence--structural-tool-span (text cap)
  "Return a structurally safe compacted org tool span for TEXT.
CAP is the maximum retained body size.  Return nil when TEXT is not
parseable as a persisted org tool span."
  (when-let* ((sexp-start (string-match "(\\s-*:name\\_>" text)))
    (condition-case nil
        (let* ((read-result (read-from-string text sexp-start))
               (sexp (car read-result))
               (sexp-end (cdr read-result)))
          (when (and (listp sexp) (stringp (plist-get sexp :name)))
            (let* ((arg-limit (mevedel-compact-evidence--tool-arg-limit cap))
                   (safe-sexp
                    (mevedel-compact-evidence--truncate-tool-args sexp arg-limit))
                   (prefix (mevedel-compact-evidence--truncate-tool-header
                            (substring text 0 sexp-start) arg-limit))
                   (suffix (substring text sexp-end))
                   (close (mevedel-compact-evidence--tool-output-close suffix))
                   (body-end (or (car close) (length suffix)))
                   (body (substring suffix 0 body-end))
                   (close-text (and close
                                    (substring suffix (car close) (cdr close))))
                   (trailing (if close
                                 (substring suffix (cdr close))
                               "")))
              (concat prefix
                      (prin1-to-string safe-sexp)
                      (mevedel-compact-evidence--truncate-tool-body body cap)
                      close-text
                      trailing))))
      (error nil))))

(defun mevedel-compact-evidence--tool-sexp-start (text)
  "Return the readable tool sexp start in TEXT, or nil.
For org tool blocks, only accept the sexp immediately after the
`#+begin_tool' header and whitespace.  For raw tool spans, require the
span itself to start with the sexp, allowing leading whitespace."
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

(defun mevedel-compact-evidence--tool-subranges (text)
  "Return parseable tool subranges for compacted org tool TEXT.

The returned plist has `:tool-start', `:tool-end', `:prefix-start',
`:prefix-end', `:suffix-start', and `:suffix-end'.  The tool range starts
at the readable `(:name ...)' sexp and excludes org scaffolding."
  (when-let* ((sexp-start (mevedel-compact-evidence--tool-sexp-start text)))
    (condition-case nil
        (let* ((read-result (read-from-string text sexp-start))
               (sexp (car read-result))
               (sexp-end (cdr read-result)))
          (when (and (listp sexp) (stringp (plist-get sexp :name)))
            (let* ((close (string-match "\n#\\+end_tool[^\n]*\n?" text sexp-end))
                   (tool-end (or close (length text)))
                   (suffix-end (if close (match-end 0) (length text))))
              (list :prefix-start 0
                    :prefix-end sexp-start
                    :tool-start sexp-start
                    :tool-end tool-end
                    :suffix-start tool-end
                    :suffix-end suffix-end))))
      (error nil))))

(defun mevedel-compact-evidence--propertize-tool-span (text prop no-properties)
  "Return TEXT with PROP restored unless NO-PROPERTIES is non-nil.
When TEXT is an org tool block, restore PROP only on the readable
`(:name ...)' sexp and result body so provider parsers can `read' the
range directly."
  (unless no-properties
    (remove-text-properties 0 (length text) '(gptel nil) text)
    (if-let* ((parts (mevedel-compact-evidence--tool-subranges text)))
        (progn
          (add-text-properties (plist-get parts :prefix-start)
                               (plist-get parts :prefix-end)
                               '(gptel ignore)
                               text)
          (add-text-properties (plist-get parts :tool-start)
                               (plist-get parts :tool-end)
                               `(gptel ,prop)
                               text)
          (add-text-properties (plist-get parts :suffix-start)
                               (plist-get parts :suffix-end)
                               '(gptel ignore)
                               text))
      (add-text-properties 0 (length text)
                           (if (or (string-prefix-p "#+begin_tool" text)
                                   (string-match-p "\\`[ \\t\\n]*(" text))
                               '(gptel ignore)
                             `(gptel ,prop))
                           text)))
  text)

(defun mevedel-compact-evidence--tool-span-with-output-cap (text prop cap no-properties)
  "Return compacted tool span TEXT with PROP preserved when appropriate.
CAP limits the visible result body.  When NO-PROPERTIES is non-nil, return
plain text."
  (mevedel-compact-evidence--propertize-tool-span
   (or (mevedel-compact-evidence--structural-tool-span text cap)
       (mevedel-compact-evidence--raw-tool-truncation text cap))
   prop no-properties))

(defun mevedel-compact-evidence-region-with-tool-output-cap (beg end cap
                                                         &optional no-properties)
  "Return text from BEG to END, truncating each tool output span to CAP.

When NO-PROPERTIES is non-nil, strip text properties from copied text."
  (let ((pos beg)
        (parts nil))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'gptel nil end))
             (prop (get-text-property pos 'gptel))
             (tool-output-p (mevedel-compact-evidence--tool-output-prop-p prop))
             (span-len (- next pos))
             (text-fn (if no-properties
                          #'buffer-substring-no-properties
                        #'buffer-substring))
             (text (funcall text-fn pos next)))
        (push (cond
               ((and tool-output-p
                     (integerp cap)
                     (> span-len cap))
                (mevedel-compact-evidence--tool-span-with-output-cap
                 (substring-no-properties text) prop cap no-properties))
               ((and tool-output-p (integerp cap))
                (if-let* ((compacted
                           (mevedel-compact-evidence--structural-tool-span
                            (substring-no-properties text) cap)))
                    (mevedel-compact-evidence--propertize-tool-span
                     compacted prop no-properties)
                  (mevedel-compact-evidence--propertize-tool-span
                   (substring-no-properties text) prop no-properties)))
               ((and (integerp cap)
                     (null prop)
                     (< next end)
                     (mevedel-compact-evidence--tool-output-prop-p
                      (get-text-property next 'gptel)))
                (mevedel-compact-evidence--truncate-tool-header
                 text (mevedel-compact-evidence--tool-arg-limit cap)))
               (t text))
              parts)
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(defun mevedel-compact-evidence--skill-provenance
    (session preserved-tail-turns &optional agent-path)
  "Return skill provenance selected for SESSION's compacted prefix.
PRESERVED-TAIL-TURNS is the number of newest turns excluded from it.
When AGENT-PATH is non-nil, include only invocations from that conversation."
  (if-let* ((session session)
            (records (mevedel-session-invoked-skills session)))
      (let ((cutoff (- (or (mevedel-session-turn-count session) 0)
                       (max 0 (or preserved-tail-turns 0)))))
        (mapcar
         (lambda (rec)
           (format "$%s%s (role: %s, origin: %s, turn: %s)"
                   (mevedel-skill-invocation-record-name rec)
                   (let ((args (mevedel-skill-invocation-record-args rec)))
                     (if (and args (not (string-empty-p args)))
                         (concat " " args)
                       ""))
                   (or (mevedel-skill-invocation-record-role rec) "?")
                   (or (mevedel-skill-invocation-record-origin rec) "?")
                   (or (mevedel-skill-invocation-record-turn rec) "?")))
         (seq-filter
          (lambda (rec)
            (and (<= (or (mevedel-skill-invocation-record-turn rec) 0)
                     cutoff)
                 (or (null agent-path)
                     (equal agent-path
                            (mevedel-skill-invocation-record-agent-path
                             rec)))))
          records)))
    nil))

(defun mevedel-compact-evidence-buffer-active-p (buf)
  "Return non-nil if BUF has an active gptel request."
  (cl-find-if
   (lambda (entry)
     (eq (thread-first (cadr entry)
                       (gptel-fsm-info)
                       (plist-get :buffer))
         buf))
   gptel--request-alist))

(defun mevedel-compact-evidence-find-boundary ()
  "Find the compaction boundary in the current buffer.
Return the position just after the last response, or nil if no response
exists."
  (let (boundary)
    (dolist (seg (mevedel-transcript-segments (point-min) (point-max)))
      (when (eq (car seg) 'response)
        (setq boundary (caddr seg))))
    boundary))

(defun mevedel-compact-evidence-turn-starts-before (limit &optional body-start)
  "Return complete turn start positions before LIMIT, oldest first.

User-authored text after the previous assistant response begins a turn.
Tool-call/result spans between assistant response chunks do not create a
new turn.  BODY-START defaults to the main-session body start."
  (let ((after-response t)
        ;; The segments arrive in order, so one carried state counts the
        ;; block-depth prefix once for the whole pass.
        (scan-state (mevedel-transcript-prompt-scan-state))
        starts)
    (dolist (seg (mevedel-transcript-segments
                  (or body-start (mevedel-compact-evidence-body-start)) limit))
      (let ((seg-end (min (caddr seg) limit)))
        (when (< (cadr seg) seg-end)
          (pcase (car seg)
            ('response
             (setq after-response t))
            ('user
             (when-let* ((prompt-start
                          (and after-response
                               (mevedel-transcript--user-prompt-start
                                (cadr seg) seg-end nil scan-state))))
               (push prompt-start starts)
               (setq after-response nil)))))))
    (nreverse starts)))

(defun mevedel-compact-evidence-context-snapshot (context)
  "Return the current effective conversation selected by CONTEXT.

CONTEXT is `all', `none', or a positive integer.  The snapshot copies
text properties because gptel uses them to distinguish user, response, and
tool spans.  Positive limits retain the anchored summary, when present, and
the requested number of most recent live turns.  This function reads only the
current buffer, so turns already rotated into compacted segments cannot be
reconstructed into a child conversation."
  (pcase context
    ('none "")
    ('all (buffer-substring (point-min) (point-max)))
    ((pred (lambda (value) (and (integerp value) (> value 0))))
     (let* ((agent-p (bound-and-true-p mevedel--agent-invocation))
            (summary (if agent-p
                         (mevedel-compact-evidence-agent-summary-bounds)
                       (mevedel-compact-evidence--summary-bounds)))
            (body-start (or (plist-get summary :end) (point-min)))
            (starts (mevedel-compact-evidence-turn-starts-before
                     (point-max) body-start))
            (count (length starts))
            (tail-start (if (<= count context)
                            body-start
                          (nth (- count context) starts))))
       (concat (and summary
                    (buffer-substring (point-min) body-start))
               (buffer-substring tail-start (point-max)))))
    (_ (error "Invalid normalized agent context: %S" context))))

(defun mevedel-compact-evidence-summary-context-evidence (tool-use-id)
  "Return frozen parent evidence excluding TOOL-USE-ID's tool segment."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (agent-path
          (if-let* ((invocation (and (boundp 'mevedel--agent-invocation)
                                     mevedel--agent-invocation)))
              (mevedel-agent-invocation-require-path invocation)
            "/root"))
         ranges)
    (dolist (segment (mevedel-transcript-segments (point-min) (point-max)))
      (unless (and (eq (car segment) 'tool)
                   (or (equal (cadddr segment) tool-use-id)
                       (equal (get-text-property (cadr segment) 'gptel)
                              (cons 'tool tool-use-id))))
        (push (cons (cadr segment) (caddr segment)) ranges)))
    (mevedel-transcript-project-evidence
     (nreverse ranges)
     :tool-output-max mevedel-compact-evidence-body-tool-output-max
     :tool-results-dir (mevedel-session-tool-results-directory session)
     :skill-provenance
     (mevedel-compact-evidence--skill-provenance session 0 agent-path))))

(defun mevedel-compact-evidence--directive-ranges ()
  "Return complete directive ranges using current-buffer positions."
  (mevedel-transcript-buffer-directive-ranges))

(defun mevedel-compact-evidence--regions-without-directives (regions)
  "Return REGIONS with complete directive turns removed."
  (let ((directives (mevedel-compact-evidence--directive-ranges))
        result)
    (dolist (region regions)
      (let ((cursor (car region))
            (end (cdr region)))
        (dolist (directive directives)
          (let ((directive-start (plist-get directive :start))
                (directive-end (plist-get directive :end)))
            (when (and (< directive-start end) (> directive-end cursor))
              (when (< cursor directive-start)
                (push (cons cursor (min directive-start end)) result))
              (setq cursor (max cursor directive-end)))))
        (when (< cursor end)
          (push (cons cursor end) result))))
    (nreverse result)))

(defun mevedel-compact-evidence-tail-start (limit aggressive &optional body-start)
  "Return tail start before LIMIT, or LIMIT when AGGRESSIVE.
The tail starts after the response preceding the preserved recent turns.
If keeping `mevedel-compact-evidence-tail-turns' turns would exceed
`mevedel-compact-evidence-tail-budget', older preserved turns are dropped.
BODY-START defaults to the main-session body start."
  (if aggressive
      limit
    (let* ((body-start (or body-start (mevedel-compact-evidence-body-start)))
           (starts (mevedel-compact-evidence-turn-starts-before limit body-start))
           (count (length starts))
           (max-turns (max 0 mevedel-compact-evidence-tail-turns))
           (budget-chars
            (* 4 (round (* mevedel-compact-evidence-tail-budget
                           (mevedel-compact-estimation-usable-tokens)))))
           (turns (min max-turns count))
           start)
      (cl-labels
          ((start-for (n)
             (if (or (zerop n) (zerop count))
                 limit
               (max body-start
                    (if (<= count n)
                        body-start
                      (nth (- count n) starts))))))
        (setq start (start-for turns))
        (while (and (> turns 1)
                    (> (- limit start) budget-chars))
          (cl-decf turns)
          (setq start (start-for turns))))
      (or (cl-loop for range in (mevedel-compact-evidence--directive-ranges)
                   when (and (> start (plist-get range :start))
                             (< start (plist-get range :end)))
                   return (plist-get range :start))
          start))))

(defun mevedel-compact-evidence-pending-text-from-prompt-buffer ()
  "Return pending request text from the current prompt buffer.

The prompt buffer may already contain expanded mentions and injected
system reminders, so source-buffer positions are not reliable here."
  (when-let* ((start (mevedel-compact-evidence-find-boundary)))
    (buffer-substring start (point-max))))

(defun mevedel-compact-evidence-prefix-before-pending (pending-text)
  "Return current buffer text before the reattached PENDING-TEXT."
  (let ((boundary (mevedel-compact-evidence-find-boundary)))
    (unless boundary
      (when (and pending-text
                 (not (string-empty-p pending-text))
                 (string-suffix-p
                  (substring-no-properties pending-text)
                  (buffer-substring-no-properties (point-min) (point-max))))
        (setq boundary (- (point-max) (length pending-text)))))
    (when boundary
      (buffer-substring (point-min) boundary))))

(defun mevedel-compact-evidence--marker-range-live-p (start end)
  "Return non-nil when START and END delimit a live marker range."
  (and (markerp start)
       (markerp end)
       (marker-buffer start)
       (marker-buffer end)
       (<= (marker-position start) (marker-position end))))

(defun mevedel-compact-evidence-rebuild-prompt-buffer
    (prompt-buffer source-buffer source-pending-text
                   prompt-history-start prompt-pending-start)
  "Rebuild PROMPT-BUFFER after SOURCE-BUFFER has been compacted.

SOURCE-PENDING-TEXT identifies the pending prompt in SOURCE-BUFFER.
PROMPT-HISTORY-START and PROMPT-PENDING-START delimit the old transcript
span.  Only that span is replaced, so prompt transforms that ran while
compaction was in flight remain in place."
  (let ((compacted-prefix
         (with-current-buffer source-buffer
           (mevedel-compact-evidence-prefix-before-pending source-pending-text))))
    (with-current-buffer prompt-buffer
      (let ((inhibit-read-only t))
        (if (and compacted-prefix
                 (mevedel-compact-evidence--marker-range-live-p
                  prompt-history-start prompt-pending-start))
            (progn
              (delete-region prompt-history-start prompt-pending-start)
              (goto-char prompt-history-start)
              (insert compacted-prefix))
          (erase-buffer)
          (insert-buffer-substring source-buffer))
        (mevedel-transcript-exclude-directive-turns)))))

(defun mevedel-compact-evidence--system-reminder-block (body)
  "Return BODY wrapped as a model-visible system reminder block."
  (format "<system-reminder>\n%s\n</system-reminder>" body))

(defun mevedel-compact-evidence-insert-current-request-reminder (body)
  "Insert reminder BODY before the pending request in the current buffer."
  (when (and (stringp body) (not (string-empty-p body)))
    (save-excursion
      (goto-char (or (mevedel-compact-evidence-find-boundary) (point-min)))
      (let ((start (point)))
        (insert "\n" (mevedel-compact-evidence--system-reminder-block body) "\n")
        (remove-text-properties
         start (point)
         '(gptel nil response nil invisible nil front-sticky nil))))))

(defun mevedel-compact-evidence--summary-bounds ()
  "Return plist bounds for the leading summary block, or nil.
The plist contains `:begin', `:body-begin', `:body-end' and `:end'."
  (mevedel-session-artifacts-segment-summary-bounds))

(defun mevedel-compact-evidence-previous-summary ()
  "Return the leading compaction summary body, or nil."
  (when-let* ((bounds (mevedel-compact-evidence--summary-bounds)))
    (mevedel-session-artifacts-strip-summary-handoff-prefix
     (string-trim
      (mevedel--strip-hook-audit-blocks
       (buffer-substring
        (plist-get bounds :body-begin)
        (plist-get bounds :body-end)))))))

(defun mevedel-compact-evidence-body-start ()
  "Return the position after the leading summary block, if present."
  (if-let* ((bounds (mevedel-compact-evidence--summary-bounds)))
      (plist-get bounds :end)
    (point-min)))

(defun mevedel-compact-evidence-agent-task-heading (&optional invocation)
  "Return INVOCATION's own persisted task-heading position, or nil.

The path property distinguishes the current task from ancestor task headings
copied into the transcript by a context fork.  INVOCATION defaults to the
current agent buffer's invocation."
  (when-let* ((invocation (or invocation
                              (bound-and-true-p mevedel--agent-invocation)))
              (path (mevedel-agent-invocation-require-path invocation)))
    (require 'org)
    (org-find-property mevedel-agent-task-path-property path)))

(defun mevedel-compact-evidence-agent-summary-bounds (&optional invocation)
  "Return INVOCATION's anchored agent-summary bounds, or nil."
  (save-excursion
    (when-let* ((task-heading
                 (mevedel-compact-evidence-agent-task-heading invocation)))
      (goto-char task-heading)
      (when (re-search-forward "^#\\+begin_summary\\b.*$" nil t)
        (let ((begin (match-beginning 0))
              (body-begin (match-end 0)))
          (when (re-search-forward "^#\\+end_summary\\b.*$" nil t)
            (list :begin begin
                  :body-begin (1+ body-begin)
                  :body-end (match-beginning 0)
                  :end (match-end 0))))))))

(defun mevedel-compact-evidence-archived-tool-use-ids (begin end)
  "Return live tool-use ids removed within BEGIN..END.
This includes both concrete tool rows and durable execution archives carried
forward by an earlier compaction."
  (let ((position begin)
        ids)
    (while (< position end)
      (let ((property (get-text-property position 'gptel)))
        (when (and (consp property)
                   (eq (car property) 'tool)
                   (stringp (cdr property)))
          (cl-pushnew (cdr property) ids :test #'equal)))
      (setq position
            (or (next-single-property-change position 'gptel nil end)
                end)))
    (dolist (record
             (mevedel-transcript-audit-records
              (buffer-substring begin end)
              'execution-archive))
      (when-let* ((tool-use-id (plist-get record :tool-use-id)))
        (cl-pushnew tool-use-id ids :test #'equal)))
    (nreverse ids)))

(defun mevedel-compact-evidence-current-tool-batch-start (info body-start)
  "Return the start of INFO's current transcript tool batch.
Only search from BODY-START so inherited parent history cannot become the
pending continuation."
  (let ((ids
         (delq nil
               (mapcar
                (lambda (call)
                  (and (listp call) (plist-get call :id)))
                (plist-get info :tool-use))))
        (batch-start body-start)
        found)
    (dolist (segment
             (mevedel-transcript-segments body-start (point-max)))
      (when (and (not found) (eq (car segment) 'tool))
        (if (cl-some
             (lambda (id)
               (member id
                       (mevedel-compact-evidence-archived-tool-use-ids
                        (cadr segment) (caddr segment))))
             ids)
            (setq found batch-start)
          (setq batch-start (caddr segment)))))
    found))

(defun mevedel-compact-evidence-select (target limit aggressive)
  "Return TARGET's projected evidence selection ending at LIMIT.
AGGRESSIVE selects whether to preserve the ordinary recent-turn tail."
  (let* ((body-start (plist-get target :body-start))
         (tail-start
          (mevedel-compact-evidence-tail-start limit aggressive body-start))
         (compact-end (max body-start tail-start))
         (history-regions
          (mevedel-compact-evidence--regions-without-directives
           (append (plist-get target :history-prefix-regions)
                   (list (cons body-start compact-end)))))
         (preserved-tail-turns
          (mevedel-compact-evidence-preserved-tail-turn-count
           tail-start limit aggressive))
         (content
          (mevedel-transcript-project-evidence
           history-regions
           :tool-output-max mevedel-compact-evidence-body-tool-output-max
           :tool-results-dir (plist-get target :tool-results-dir)
           :skill-provenance
           (mevedel-compact-evidence--skill-provenance
            (plist-get target :prompt-session)
            preserved-tail-turns
            (plist-get target :skill-agent-path)))))
    (list :content content
          :history-regions history-regions
          :preserved-tail-turns preserved-tail-turns
          :tail-start tail-start)))

(defun mevedel-compact-evidence-preserved-tail-turn-count (tail-start limit aggressive)
  "Return the number of complete user-authored requests in retained tail.
TAIL-START and LIMIT delimit the retained tail.  AGGRESSIVE means no
tail is retained."
  (if aggressive
      0
    (length
     (cl-remove-if-not
      (lambda (start) (>= start tail-start))
      (mevedel-compact-evidence-turn-starts-before limit)))))

(provide 'mevedel-compact-evidence)

;;; mevedel-compact-evidence.el ends here
