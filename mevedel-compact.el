;;; mevedel-compact.el --- Chat compaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Summarises older portions of a chat session to reduce token usage.
;; Persisted sessions rotate to a new on-disk segment whose first block
;; is an anchored compaction summary followed by a preserved recent
;; tail.  Non-persisted manual compaction falls back to the legacy
;; in-buffer ignored-summary block.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel nil t))

;; `gptel'
(defvar gptel-mode)
(defvar gptel--markdown-block-map)
(defvar gptel-model)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel--request-params)
(declare-function gptel-mode "ext:gptel" (&optional arg))
(declare-function gptel-markdown-cycle-block "ext:gptel" ())
(declare-function gptel--update-status "ext:gptel" (msg &optional face))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request")
(declare-function gptel-request "ext:gptel-request")
(declare-function gptel--merge-plists "ext:gptel-request" (&rest plists))
(declare-function gptel--model-request-params "ext:gptel-request" (model))
(declare-function gptel-backend-request-params "ext:gptel-request" (backend))
(defvar gptel--request-alist)

;; `mevedel'
(declare-function mevedel--active-chat-buffer "mevedel-chat" (&optional workspace))
(declare-function mevedel-view--full-rerender "mevedel-view" ())
(declare-function mevedel-view--stop-spinner "mevedel-view" ())
(declare-function mevedel-view--update-spinner "mevedel-view" (status))
(defvar mevedel--view-buffer)
(defvar mevedel--session)

;; `mevedel-mentions'
(declare-function mevedel--transform-expand-mentions "mevedel-mentions" (fsm))

;; `mevedel-reminders'
(declare-function mevedel-reminders--transform "mevedel-reminders" (fsm))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-rotate-segment
                  "mevedel-session-persistence" (session buffer summary
                                                         &rest keys))
(declare-function mevedel-session-persistence--segment-summary-bounds
                  "mevedel-session-persistence" ())
(declare-function mevedel-session-persistence--segment-path
                  "mevedel-session-persistence" (session-dir segment))
(declare-function mevedel-session-current-segment
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(defvar mevedel-session-persistence)
(defvar mevedel-session--read-only-mode)

(defcustom mevedel-compact-context-limit nil
  "Override model context window in tokens.

When nil, mevedel reads the active `gptel-model' `:context-window'
property and falls back to 200000 tokens when the model does not expose
one."
  :type '(choice (const :tag "Use gptel context window" nil)
          (natnum :tag "Token count"))
  :group 'mevedel)

(defcustom mevedel-compact-token-threshold 0.80
  "Estimated token threshold for auto-compaction.
Can be either the number of tokens as an integer or a float between 0
and 1, used as a fraction of usable context."
  :type '(choice (natnum :tag "Absolute token count")
          (float :tag "Fraction of usable context"))
  :group 'mevedel)

(defcustom mevedel-compact-reserve-tokens 20000
  "Token headroom reserved below the model context window."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-auto t
  "Whether mevedel automatically compacts persisted sessions."
  :type 'boolean
  :group 'mevedel)

(defcustom mevedel-compact-tail-turns 2
  "Target number of recent complete turns to preserve verbatim."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-tail-budget 0.25
  "Maximum fraction of usable context reserved for preserved tail."
  :type '(restricted-sexp
          :tag "Fraction of usable context"
          :match-alternatives
          ((lambda (value)
             (and (floatp value)
                  (<= 0.0 value)
                  (<= value 1.0)))))
  :group 'mevedel)

(defcustom mevedel-compact-tail-tool-output-max 4000
  "Per-tool-result character cap inside the preserved tail."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-body-tool-output-max 8000
  "Per-tool-result character cap inside the compaction request body."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-compact-warn-on-completion t
  "Whether to show a one-shot accuracy message after compaction."
  :type 'boolean
  :group 'mevedel)

(defvar-local mevedel--known-token-baseline nil
  "Last known non-compaction request token baseline plist.")

(defvar-local mevedel--compaction-in-flight nil
  "Non-nil while a compaction request is active in this buffer.")

(defvar-local mevedel--compact-auto-disabled nil
  "Non-nil when auto-compaction is disabled for this session.")

(defvar-local mevedel--compact-failure-count 0
  "Consecutive compaction failures in this session.")

(defvar-local mevedel--compact-warning-shown nil
  "Non-nil after showing the post-compaction accuracy warning.")

(defvar-local mevedel--compact-auto-ineligible-warning-shown nil
  "Non-nil after warning that auto-compaction cannot run here.")

(defun mevedel--file-local-variables-start ()
  "Return position where file-local variables block begins, or nil.
Searches forward from beginning of buffer for the first Local Variables
block."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "Local Variables:" nil t)
      (line-beginning-position))))

(defsubst mevedel--model-context-window (&optional model)
  "Return MODEL's context window in tokens.
MODEL defaults to `gptel-model'.  gptel stores `:context-window' in
thousands of tokens, sometimes as a float."
  (when-let* ((m (or model gptel-model))
              (kt (get m :context-window)))
    (round (* kt 1000))))

(defun mevedel--compact-context-limit ()
  "Return the effective context window in tokens."
  (or mevedel-compact-context-limit
      (mevedel--model-context-window)
      200000))

(defun mevedel--compact-model-max-output-tokens ()
  "Return the effective configured max-output token count, or 0."
  (or gptel-max-tokens
      (when (and (bound-and-true-p gptel-backend)
                 (fboundp 'gptel--merge-plists)
                 (fboundp 'gptel-backend-request-params)
                 (fboundp 'gptel--model-request-params))
        (let ((params (gptel--merge-plists
                       gptel--request-params
                       (gptel-backend-request-params gptel-backend)
                       (gptel--model-request-params gptel-model))))
          (or (plist-get params :max_tokens)
              (plist-get params :maxOutputTokens)
              (plist-get params :max_output_tokens)
              (plist-get params :num_predict))))
      0))

(defun mevedel--compact-usable-tokens ()
  "Return usable context tokens after response reserve."
  (max 1 (- (mevedel--compact-context-limit)
            (max mevedel-compact-reserve-tokens
                 (or (mevedel--compact-model-max-output-tokens) 0)))))

(defun mevedel--compact-threshold-tokens ()
  "Return the effective compaction threshold in tokens."
  (let ((threshold mevedel-compact-token-threshold)
        (usable (mevedel--compact-usable-tokens)))
    (cond
     ((integerp threshold) threshold)
     ((floatp threshold) (round (* threshold usable)))
     (t (round (* 0.80 usable))))))

(defun mevedel--compact-token-usage-count (tokens)
  "Return total prompt/output TOKENS from a gptel token-usage plist."
  (when (listp tokens)
    (+ (or (plist-get tokens :input) 0)
       (or (plist-get tokens :cached) 0)
       (or (plist-get tokens :cache-read) 0)
       (or (plist-get tokens :cache_read) 0)
       (or (plist-get tokens :output) 0))))

(defun mevedel--compact-token-usage-input (tokens)
  "Return input/cached prompt tokens from a gptel token-usage plist."
  (when (listp tokens)
    (+ (or (plist-get tokens :input) 0)
       (or (plist-get tokens :cached) 0)
       (or (plist-get tokens :cache-read) 0)
       (or (plist-get tokens :cache_read) 0))))

(defun mevedel--compact-record-token-baseline (fsm)
  "Record API-reported token usage from FSM for future estimates.

Compaction requests are ignored so the summary request's own token
count never becomes the baseline for the chat buffer."
  (when-let* ((info (and fsm (gptel-fsm-info fsm)))
              ((not (plist-get (plist-get info :context)
                               :mevedel-compaction)))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer))
              (tokens (or (plist-get info :tokens-full)
                          (plist-get info :tokens)))
              (count (mevedel--compact-token-usage-count tokens)))
    (with-current-buffer chat-buffer
      (setq mevedel--known-token-baseline
            (list :tokens count
                  :input-tokens (mevedel--compact-token-usage-input tokens)
                  :output-tokens (or (plist-get tokens :output) 0)
                  :position (copy-marker (point-max)))))))

(defun mevedel--estimate-tokens ()
  "Estimate the number of tokens in the current buffer.
Only counts text not marked with the `gptel' property `ignore' and
excludes file-local variables block."
  (if-let* ((baseline mevedel--known-token-baseline)
            (tokens (plist-get baseline :tokens))
            (position (plist-get baseline :position))
            ((integer-or-marker-p position))
            (pos (if (markerp position) (marker-position position) position))
            ((<= (point-min) pos))
            ((<= pos (point-max))))
      (+ tokens (/ (- (point-max) pos) 4))
    (let ((pos (point-min))
          (total 0)
          (flv-start (mevedel--file-local-variables-start)))
      (while (< pos (point-max))
        (let* ((next (next-single-property-change pos 'gptel nil (point-max)))
               (prop (get-text-property pos 'gptel)))
          (unless (eq prop 'ignore)
            ;; Only count if not in file-local-variables region
            (when (or (null flv-start) (< pos flv-start))
              (setq total (+ total (- (if (and flv-start (> next flv-start))
                                          flv-start
                                        next)
                                      pos)))))
          (setq pos next)))
      (/ total 4))))

(defun mevedel--compact-estimate-buffer-tokens (buffer)
  "Return a fresh token estimate for BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let (mevedel--known-token-baseline)
        (mevedel--estimate-tokens)))))

(defun mevedel--compact-tool-output-prop-p (prop)
  "Return non-nil when PROP marks a gptel tool output span."
  (and (consp prop) (eq (car prop) 'tool)))

(defun mevedel--compact-truncation-marker (omitted)
  "Return the marker inserted when OMITTED tool-output chars are removed."
  (format "\n[mevedel: tool output truncated; omitted %d chars]\n"
          omitted))

(defun mevedel--compact-region-with-tool-output-cap (beg end cap
                                                         &optional no-properties)
  "Return text from BEG to END, truncating each tool output span to CAP.

When NO-PROPERTIES is non-nil, strip text properties from copied text."
  (let ((pos beg)
        (parts nil))
    (while (< pos end)
      (let* ((next (next-single-property-change pos 'gptel nil end))
             (prop (get-text-property pos 'gptel))
             (tool-output-p (mevedel--compact-tool-output-prop-p prop))
             (span-len (- next pos))
             (take-len (if (and tool-output-p
                                (integerp cap)
                                (> span-len cap))
                           cap
                         span-len))
             (text-fn (if no-properties
                          #'buffer-substring-no-properties
                        #'buffer-substring)))
        (push (funcall text-fn pos (+ pos take-len)) parts)
        (when (< take-len span-len)
          (push (mevedel--compact-truncation-marker
                 (- span-len take-len))
                parts))
        (setq pos next)))
    (apply #'concat (nreverse parts))))

(declare-function mevedel-session-invoked-skills "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-name
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-args
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-trigger
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-skill-invocation-record-turn
                  "mevedel-structs" (cl-x) t)

(defun mevedel--compact-skills-section (session)
  "Return the anchored `Skills Invoked' section body for SESSION."
  (if-let* ((session session)
            (records (mevedel-session-invoked-skills session)))
      (mapconcat
       (lambda (rec)
         (format "- /%s%s (trigger: %s, turn: %s)"
                 (mevedel-skill-invocation-record-name rec)
                 (let ((args (mevedel-skill-invocation-record-args rec)))
                   (if (and args (not (string-empty-p args)))
                       (concat " " args)
                     ""))
                 (or (mevedel-skill-invocation-record-trigger rec) "?")
                 (or (mevedel-skill-invocation-record-turn rec) "?")))
       records "\n")
    "- (none)"))

(defun mevedel--compact-prompt (&optional previous-summary instructions session)
  "Return the anchored compaction system prompt.
PREVIOUS-SUMMARY selects update mode when non-nil.  INSTRUCTIONS are
manual user instructions.  SESSION supplies invoked skill records."
  (concat
   "CRITICAL: Respond with TEXT ONLY. Do NOT call any tools.\n"
   "- Do NOT use Read, Bash, Grep, Glob, Edit, Write, or ANY other tool.\n"
   "- You already have all the context you need in the conversation above.\n"
   "- Tool calls will be REJECTED and will waste your only turn -- you will fail the task.\n"
   "- Your entire response must be plain text matching the structure below.\n\n"
   (if previous-summary
       (concat
        "Update the anchored summary below using the conversation history above.\n"
        "The previous summary is authoritative retained context from older compacted history. "
        "Do NOT replace it with only the recent conversation.\n"
        "Your output must merge BOTH sources: keep still-true details from the previous summary, "
        "remove stale or contradicted details, and add new facts from the recent conversation.\n"
        "If the recent conversation is a separate completed task, preserve the older completed "
        "task under Progress/Critical Context/Relevant Files instead of dropping it.\n\n"
        "<previous-summary>\n" previous-summary "\n</previous-summary>\n\n")
     "Create a new anchored summary from the conversation history above.\n\n")
   "Output exactly this Markdown structure and keep the section order unchanged.\n\n"
   "## Goal\n- [single-sentence task summary]\n\n"
   "## Constraints & Preferences\n- [user constraints, preferences, specs, or \"(none)\"]\n\n"
   "## Progress\n### Done\n- [completed work or \"(none)\"]\n\n"
   "### In Progress\n- [current work or \"(none)\"]\n\n"
   "### Blocked\n- [blockers or \"(none)\"]\n\n"
   "## Key Decisions\n- [decision and why, or \"(none)\"]\n\n"
   "## Next Steps\n- [ordered next actions or \"(none)\"]\n\n"
   "## Critical Context\n- [important technical facts, errors, open questions, or \"(none)\"]\n\n"
   "## Relevant Files\n- [path: why it matters, or \"(none)\"]\n\n"
   "## Skills Invoked\n" (mevedel--compact-skills-section session) "\n\n"
   (when (and instructions (not (string-blank-p instructions)))
     (concat "## Additional Instructions\n" instructions "\n\n"))
   "Rules:\n"
   "- Keep every section, even when empty.\n"
   "- Use terse bullets, not prose paragraphs.\n"
   (when previous-summary
     "- Do not discard previous-summary details merely because they are not repeated in the recent conversation.\n")
   "- Preserve exact file paths, commands, error strings, and identifiers when known.\n"
   "- Do not mention the summary process or that context was compacted.\n"))

(defun mevedel--compact-buffer-active-p (buf)
  "Return non-nil if BUF has an active gptel request."
  (cl-find-if
   (lambda (entry)
     (eq (thread-first (cadr entry)
                       (gptel-fsm-info)
                       (plist-get :buffer))
         buf))
   gptel--request-alist))

(defun mevedel--compact-find-boundary ()
  "Find the compaction boundary in the current buffer.
Walks backward from the end to find the end of the last response.
Everything up to that point will be compacted. Returns the position just
after the last response, or nil if no response exists."
  (let ((pos (point-max)))
    (while (and pos (not (eq (get-text-property pos 'gptel) 'response)))
      (setq pos (previous-single-property-change pos 'gptel)))
    ;; pos is now inside the last response region (or nil).
    ;; Find where this response region ends.
    (when pos
      (next-single-property-change pos 'gptel nil (point-max)))))

(defun mevedel--compact-response-ends-before (limit)
  "Return response end positions before LIMIT, oldest first."
  (let ((pos (point-min))
        ends)
    (while (< pos limit)
      (let* ((next (next-single-property-change pos 'gptel nil limit))
             (prop (get-text-property pos 'gptel)))
        (when (eq prop 'response)
          (push next ends))
        (setq pos next)))
    (nreverse (delete-dups ends))))

(defun mevedel--compact-user-authored-span-p (beg end)
  "Return non-nil when BEG..END looks like user-authored prompt text."
  (let ((prop (get-text-property beg 'gptel)))
    (and (not (memq prop '(response ignore)))
         (not (and (consp prop) (eq (car prop) 'tool)))
         (not (string-blank-p
               (buffer-substring-no-properties beg end))))))

(defun mevedel--compact-turn-starts-before (limit)
  "Return complete turn start positions before LIMIT, oldest first.

A turn starts at user-authored text after the previous assistant
response.  Tool-call/result spans between assistant response chunks do
not create a new turn."
  (let ((pos (mevedel--compact-body-start))
        (after-response t)
        starts)
    (while (< pos limit)
      (let* ((next (next-single-property-change pos 'gptel nil limit))
             (prop (get-text-property pos 'gptel)))
        (cond
         ((eq prop 'response)
          (setq after-response t))
         ((and after-response
               (mevedel--compact-user-authored-span-p pos next))
          (push pos starts)
          (setq after-response nil)))
        (setq pos next)))
    (nreverse starts)))

(defun mevedel--compact-tail-start (limit aggressive)
  "Return tail start before LIMIT, or LIMIT when AGGRESSIVE.
The tail starts after the response preceding the preserved recent turns.
If keeping `mevedel-compact-tail-turns' turns would exceed
`mevedel-compact-tail-budget', older preserved turns are dropped."
  (if aggressive
      limit
    (let* ((starts (mevedel--compact-turn-starts-before limit))
           (count (length starts))
           (body-start (mevedel--compact-body-start))
           (max-turns (max 0 mevedel-compact-tail-turns))
           (budget-chars
            (* 4 (round (* mevedel-compact-tail-budget
                           (mevedel--compact-usable-tokens)))))
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
      start)))

(defun mevedel--compact-pending-text-from-prompt-buffer ()
  "Return pending request text from the current prompt buffer.

The prompt buffer may already contain expanded mentions and injected
system reminders, so source-buffer positions are not reliable here."
  (when-let* ((start (mevedel--compact-find-boundary)))
    (buffer-substring start (point-max))))

(defun mevedel--compact-prefix-before-pending (pending-text)
  "Return current buffer text before the reattached PENDING-TEXT."
  (let ((boundary (mevedel--compact-find-boundary)))
    (unless boundary
      (when (and pending-text
                 (not (string-empty-p pending-text))
                 (string-suffix-p
                  (substring-no-properties pending-text)
                  (buffer-substring-no-properties (point-min) (point-max))))
        (setq boundary (- (point-max) (length pending-text)))))
    (when boundary
      (buffer-substring (point-min) boundary))))

(defun mevedel--compact-marker-range-live-p (start end)
  "Return non-nil when START and END delimit a live marker range."
  (and (markerp start)
       (markerp end)
       (marker-buffer start)
       (marker-buffer end)
       (<= (marker-position start) (marker-position end))))

(defun mevedel--compact-rebuild-prompt-buffer
    (prompt-buffer source-buffer source-pending-text
                   prompt-history-start prompt-pending-start)
  "Rebuild PROMPT-BUFFER after SOURCE-BUFFER has been compacted.

Only the old transcript span is replaced, so prompt transforms that
ran while compaction was in flight remain in place."
  (let ((compacted-prefix
         (with-current-buffer source-buffer
           (mevedel--compact-prefix-before-pending source-pending-text))))
    (with-current-buffer prompt-buffer
      (let ((inhibit-read-only t))
        (if (and compacted-prefix
                 (mevedel--compact-marker-range-live-p
                  prompt-history-start prompt-pending-start))
            (progn
              (delete-region prompt-history-start prompt-pending-start)
              (goto-char prompt-history-start)
              (insert compacted-prefix))
          (erase-buffer)
          (insert-buffer-substring source-buffer))))))

(defun mevedel--compact-summary-bounds ()
  "Return plist bounds for the leading summary block, or nil.
The plist contains `:begin', `:body-begin', `:body-end' and `:end'."
  (require 'mevedel-session-persistence)
  (mevedel-session-persistence--segment-summary-bounds))

(defun mevedel--compact-previous-summary ()
  "Return the leading compaction summary body, or nil."
  (when-let* ((bounds (mevedel--compact-summary-bounds)))
    (string-trim
     (buffer-substring-no-properties
      (plist-get bounds :body-begin)
      (plist-get bounds :body-end)))))

(defun mevedel--compact-body-start ()
  "Return the position after the leading summary block, if present."
  (if-let* ((bounds (mevedel--compact-summary-bounds)))
      (plist-get bounds :end)
    (point-min)))

(defun mevedel--compact-current-persisted-p ()
  "Return non-nil when current buffer can use segment rotation."
  (and (boundp 'mevedel--session)
       mevedel--session
       (mevedel-session-save-path mevedel--session)
       buffer-file-name
       (progn
         (require 'mevedel-session-persistence)
         (string=
          (expand-file-name buffer-file-name)
          (expand-file-name
           (mevedel-session-persistence--segment-path
            (mevedel-session-save-path mevedel--session)
            (mevedel-session-current-segment mevedel--session)))))))

(defun mevedel--compact-can-rotate-p ()
  "Return non-nil when compaction may rotate the persisted segment."
  (and (bound-and-true-p mevedel-session-persistence)
       (mevedel--compact-current-persisted-p)))

(defun mevedel--compact-auto-eligible-p ()
  "Return non-nil when automatic compaction may run in this buffer."
  (and mevedel-compact-auto
       (not mevedel--compact-auto-disabled)
       (not mevedel--compaction-in-flight)
       (not (bound-and-true-p mevedel-session--read-only-mode))
       (mevedel--compact-can-rotate-p)))

(defun mevedel--compact-auto-ineligible-reason ()
  "Return a short reason automatic compaction cannot run, or nil."
  (cond
   ((not mevedel-compact-auto) "auto-compaction is disabled")
   (mevedel--compact-auto-disabled "auto-compaction is disabled after repeated failures")
   (mevedel--compaction-in-flight "compaction is already in progress")
   ((bound-and-true-p mevedel-session--read-only-mode) "session is read-only")
   ((not (bound-and-true-p mevedel-session-persistence)) "session persistence is disabled")
   ((not (and (boundp 'mevedel--session)
              mevedel--session
              (mevedel-session-save-path mevedel--session)))
    "session is not materialized on disk")
   ((not (mevedel--compact-current-persisted-p))
    "current buffer is not the active persisted segment")))

(defun mevedel--compact-should-compact-p (&optional token-estimate)
  "Return non-nil when estimated tokens exceed the configured threshold."
  (let ((over-threshold (>= (or token-estimate
                                (mevedel--estimate-tokens))
                            (mevedel--compact-threshold-tokens))))
    (cond
     ((not over-threshold) nil)
     ((mevedel--compact-auto-eligible-p) t)
     ((not mevedel--compact-auto-ineligible-warning-shown)
      (setq mevedel--compact-auto-ineligible-warning-shown t)
      (display-warning
       'mevedel
       (format "Auto-compaction skipped: %s"
               (or (mevedel--compact-auto-ineligible-reason)
                   "session is not eligible"))
       :warning)
      nil)
     (t nil))))

(defun mevedel--compact-apply (boundary summary &optional tail-text pending-text)
  "Apply compaction to the current buffer.

Implements **split-on-compact**: when the session is materialized
on disk, we finalize the current segment file and start a new one
whose content is SUMMARY followed by TAIL-TEXT and PENDING-TEXT.
Falls back to the legacy in-place approach when the session has no
`save-path' (persistence disabled).

BOUNDARY is unused in the segment-rotation path; it is preserved
for the legacy fallback."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (cond
     ;; Split-on-compact path: rotate to a new segment file.
     ((and session
           (mevedel--compact-can-rotate-p))
      (remove-text-properties 0 (length summary)
                              '(gptel nil face nil) summary)
      (mevedel-session-persistence-rotate-segment
       session (current-buffer) summary
       :tail-text tail-text
       :pending-text pending-text))
     ;; Legacy path: in-place ignore marking (no on-disk persistence).
     (t
      (mevedel--compact-apply-legacy boundary summary)))))

(defun mevedel--compact-apply-legacy (boundary summary)
  "Legacy in-place compaction: mark content before BOUNDARY as ignored.

Used when no on-disk session exists (`mevedel-session-persistence' is
nil or the session has not yet been materialized).  Inserts SUMMARY at
BOUNDARY wrapped in a folded summary block."
  (let ((inhibit-read-only t))
    (put-text-property (point-min) boundary 'gptel 'ignore)
    (put-text-property (point-min) boundary 'face 'shadow)
    (save-excursion
      (goto-char boundary)
      (let ((sep (format "\n\n--- Conversation compacted at %s ---\n\n"
                         (format-time-string "%Y-%m-%d %H:%M"))))
        (remove-text-properties 0 (length summary) '(gptel nil face nil) summary)
        (insert
         (propertize sep 'gptel 'ignore)
         (if (derived-mode-p 'org-mode)
             (propertize "#+begin_summary\n" 'gptel 'ignore)
           (propertize "``` summary\n" 'gptel 'ignore
                       'keymap gptel--markdown-block-map))
         summary
         (if (derived-mode-p 'org-mode)
             (concat "\n" (propertize "#+end_summary\n" 'gptel 'ignore))
           (concat "\n" (propertize "```\n" 'gptel 'ignore
                                    'keymap gptel--markdown-block-map))))
        (ignore-errors
          (if (derived-mode-p 'org-mode)
              (save-excursion
                (search-backward "#+begin_summary" boundary t)
                (when (looking-at "^#+begin_summary")
                  (org-cycle)))
            (save-excursion
              (when (re-search-backward "^```" boundary t)
                (gptel-markdown-cycle-block)))))))))

(cl-defun mevedel--compact-run
    (&key aggressive instructions pending-start callback auto)
  "Run compaction in the current chat buffer.
AGGRESSIVE drops the preserved tail.  INSTRUCTIONS are manual summary
instructions.  PENDING-START marks an inserted-but-unsent prompt region.
CALLBACK receives (ERR) when compaction settles.  AUTO marks an
auto-compaction call."
  (let ((chat-buffer (current-buffer))
        (tokens-before (mevedel--estimate-tokens))
        (settled nil))
    (cl-labels
        ((finish (err)
           (unless settled
             (setq settled t)
             (when (buffer-live-p chat-buffer)
               (with-current-buffer chat-buffer
                 (setq-local mevedel--compaction-in-flight nil)))
             (when callback (funcall callback err)))))
      (when mevedel--compaction-in-flight
        (user-error "Compaction already in progress"))
      (when (bound-and-true-p mevedel-session--read-only-mode)
        (user-error "Session is read-only"))
      (when (and (not pending-start)
                 (mevedel--compact-buffer-active-p chat-buffer))
        (user-error "Cannot compact while a request is active"))
      (let* ((limit (or pending-start (mevedel--compact-find-boundary))))
        (unless limit
          (if auto
              (cl-return-from mevedel--compact-run
                (when callback (funcall callback :skip)))
            (user-error "Not enough conversation content to compact")))
        (let* ((body-start (mevedel--compact-body-start))
               (tail-start (mevedel--compact-tail-start limit aggressive))
               (compact-end (max body-start tail-start))
               (old-content
                (mevedel--compact-region-with-tool-output-cap
                 body-start compact-end
                 mevedel-compact-body-tool-output-max
                 t))
               (tail-text (unless aggressive
                            (mevedel--compact-region-with-tool-output-cap
                             tail-start limit
                             mevedel-compact-tail-tool-output-max)))
               (pending-text (when pending-start
                               (buffer-substring pending-start (point-max))))
               (previous-summary (mevedel--compact-previous-summary))
               (boundary-marker (copy-marker compact-end))
               (system-prompt
                (mevedel--compact-prompt previous-summary
                                         instructions
                                         mevedel--session))
               (attempt 0)
               (max-attempts 3))
          (when (string-blank-p old-content)
            (if auto
                (cl-return-from mevedel--compact-run
                  (when callback (funcall callback :skip)))
              (user-error "Not enough conversation content to compact")))
          (setq mevedel--compaction-in-flight t)
          (message "mevedel: compacting (%dk -> ...)"
                   (/ tokens-before 1000))
          (when-let* ((vb mevedel--view-buffer)
                      (_ (buffer-live-p vb)))
            (with-current-buffer vb
              (mevedel-view--update-spinner "Compacting...")))
          (require 'gptel)
          (cl-labels
              ((fail (err retryable)
                 (if (and retryable (< attempt max-attempts))
                     (let ((delay (expt 2 (1- attempt))))
                       (message "mevedel: compaction failed, retrying in %ss (%s)"
                                delay err)
                       (run-at-time
                        delay nil
                        (lambda ()
                          (when (buffer-live-p chat-buffer)
                            (with-current-buffer chat-buffer
                              (send-request))))))
                   (cl-incf mevedel--compact-failure-count)
                   (display-warning 'mevedel err :warning)
                   (finish err)))
               (send-request ()
                 (cl-incf attempt)
                 (let ((old-use-tools gptel-use-tools)
                       (old-tools gptel-tools))
                   (unwind-protect
                       (condition-case err
                           (progn
                             (setq-local gptel-use-tools nil)
                             (setq-local gptel-tools nil)
                             (gptel-with-preset 'gptel-default
                               (gptel-request old-content
                                 :system system-prompt
                                 :buffer chat-buffer
                                 :stream nil
                                 :transforms nil
                                 :context (list :mevedel-compaction t)
                                 :callback
                                 (lambda (response info)
                                   (with-current-buffer chat-buffer
                                     (pcase response
                                       ('nil
                                        (fail
                                         (format "Compaction failed: %s"
                                                 (or (plist-get info :error)
                                                     (plist-get info :status)
                                                     "unknown error"))
                                         t))
                                       ('abort
                                        (fail "Compaction aborted" nil))
                                       ((pred stringp)
                                        (condition-case err
                                            (progn
                                              (mevedel--compact-apply
                                               boundary-marker response
                                               tail-text pending-text)
                                              (set-marker boundary-marker nil)
                                              (setq mevedel--known-token-baseline nil)
                                              (setq mevedel--compact-failure-count 0)
                                              (when-let* ((vb mevedel--view-buffer)
                                                          (_ (buffer-live-p vb)))
                                                (with-current-buffer vb
                                                  (mevedel-view--full-rerender)
                                                  (unless auto
                                                    (mevedel-view--stop-spinner))))
                                              (message
                                               "mevedel: compaction complete (%dk -> %dk tokens, %d turns preserved)"
                                               (/ tokens-before 1000)
                                               (/ (mevedel--estimate-tokens) 1000)
                                               (if aggressive 0 mevedel-compact-tail-turns))
                                              (when (and mevedel-compact-warn-on-completion
                                                         (not mevedel--compact-warning-shown))
                                                (setq mevedel--compact-warning-shown t)
                                                (message
                                                 "mevedel: long threads with multiple compactions can reduce model accuracy; consider starting a new session for unrelated work"))
                                              (finish nil))
                                          (error
                                           (fail (format "%s" err) nil))))))))))
                         (error
                          (fail (format "%s" err) t)))
                     (setq-local gptel-use-tools old-use-tools)
                     (setq-local gptel-tools old-tools)))))
            (send-request)))))))

;;;###autoload
(defun mevedel-compact (&optional aggressive instructions)
  "Compact the current mevedel chat buffer.
With prefix argument AGGRESSIVE, compact without preserving a recent
tail.  INSTRUCTIONS is an optional string of manual summary guidance."
  (interactive "P")
  (let* ((chat-buffer
          (cond
           ((and (bound-and-true-p gptel-mode) (bound-and-true-p mevedel--workspace))
            (current-buffer))
           (t (mevedel--active-chat-buffer)))))
    (unless (and chat-buffer (buffer-live-p chat-buffer))
      (user-error "No mevedel chat buffer found"))
    (with-current-buffer chat-buffer
      (mevedel--compact-run
       :aggressive aggressive
       :instructions instructions))))

(defun mevedel--compact-transform-auto (continue fsm)
  "Prompt transform that runs auto-compaction before request realization.
CONTINUE is gptel's async transform continuation.  FSM is the request
state machine."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (source-buffer (and (listp info) (plist-get info :buffer)))
         (prompt-buffer (current-buffer))
         (effective-backend gptel-backend)
         (effective-model gptel-model)
         (effective-max-tokens gptel-max-tokens)
         (effective-request-params gptel--request-params)
         (context (and (listp info) (plist-get info :context))))
    (if (or (and (listp context) (plist-get context :mevedel-compaction))
            (not (buffer-live-p source-buffer)))
        (funcall continue)
      (with-current-buffer source-buffer
        (cond
         (mevedel--compaction-in-flight
          (user-error "Compaction already in progress"))
         ((not (let ((gptel-backend effective-backend)
                     (gptel-model effective-model)
                     (gptel-max-tokens effective-max-tokens)
                     (gptel--request-params effective-request-params))
                 (mevedel--compact-should-compact-p
                  (mevedel--compact-estimate-buffer-tokens
                   prompt-buffer))))
          (funcall continue))
         (t
          (let ((pending-start (mevedel--compact-find-boundary)))
            (if (not pending-start)
                (funcall continue)
              (let ((source-pending-text
                     (buffer-substring pending-start (point-max)))
                    (prompt-history-start
                     (when (buffer-live-p prompt-buffer)
                       (with-current-buffer prompt-buffer
                         (copy-marker (point-min) t))))
                    (prompt-pending-start
                     (when (buffer-live-p prompt-buffer)
                       (with-current-buffer prompt-buffer
                         (when-let* ((start (mevedel--compact-find-boundary)))
                           (copy-marker start nil))))))
                (mevedel--compact-run
                 :pending-start pending-start
                 :auto t
                 :callback
                 (lambda (err)
                   (unwind-protect
                       (cond
                        ((eq err :skip)
                         (funcall continue))
                        (err
                         (when (>= mevedel--compact-failure-count 3)
                           (setq mevedel--compact-auto-disabled t))
                         (display-warning
                          'mevedel
                          (format
                           (if mevedel--compact-auto-disabled
                               "Auto-compaction disabled after repeated failures; request not sent: %s"
                             "Auto-compaction failed; request not sent: %s")
                           err)
                          :warning)
                         (when (fboundp 'gptel--update-status)
                           (gptel--update-status " Compaction failed" 'error))
                         (when-let* ((vb mevedel--view-buffer)
                                     (_ (buffer-live-p vb)))
                           (with-current-buffer vb
                             (mevedel-view--stop-spinner))))
                        (t
                         (when (and (buffer-live-p prompt-buffer)
                                    (buffer-live-p source-buffer))
                           (with-current-buffer source-buffer
                             (when-let* ((marker (plist-get info :position)))
                               (set-marker marker (point-max) source-buffer)))
                           (when-let* ((vb mevedel--view-buffer)
                                       (_ (buffer-live-p vb)))
                             (with-current-buffer vb
                               (mevedel-view--update-spinner "Thinking...")))
                           (mevedel--compact-rebuild-prompt-buffer
                            prompt-buffer source-buffer source-pending-text
                            prompt-history-start prompt-pending-start))
                         (funcall continue)))
                     (when (markerp prompt-history-start)
                       (set-marker prompt-history-start nil))
                     (when (markerp prompt-pending-start)
                       (set-marker prompt-pending-start nil))))))))))))))

(provide 'mevedel-compact)
;;; mevedel-compact.el ends here
