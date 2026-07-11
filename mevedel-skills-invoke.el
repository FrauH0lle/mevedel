;;; mevedel-skills-invoke.el -- Skill invocation and dispatch -*- lexical-binding: t -*-

;;; Commentary:

;; Owns skill acceptance and preparation: request-scoped overrides, argument
;; substitution, body injections, invocation records, inline attachments,
;; direct fork response insertion and terminal handoff, model-facing handlers,
;; and skill-specific leading dispatch.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-agents))

(require 'mevedel-structs)
(require 'mevedel-tool-registry)
(require 'mevedel-models)
(require 'mevedel-skills-core)
(require 'mevedel-turn)

;; `gptel'
(declare-function gptel--update-status "ext:gptel" (msg &optional face))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-post-response-functions)
(defvar gptel-prompt-prefix-alist)
(defvar gptel-response-separator)
(defvar gptel-system-prompt)

;; `mevedel-agent-exec'
(defvar mevedel--agent-invocation)
(defvar mevedel-agent-exec--agents)

;; `mevedel-agent-runtime'
;; Use `t' for the arglist: cl-defun with &key keywords confuses the
;; byte-compiler's arity check.
(declare-function mevedel-agent-runtime-dispatch "mevedel-agent-runtime" t t)

;; `mevedel-agents'
(declare-function mevedel-agent--create "mevedel-agents" (&rest args))
(declare-function mevedel-agent-get "mevedel-agents" (name))
(declare-function mevedel-agent-invocation-hook-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-effort-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-model-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))

;; `mevedel-mentions'
(declare-function mevedel-mentions-replace-with-placeholder
                  "mevedel-mentions" (start end placeholder))

;; `mevedel-models'
(declare-function mevedel-model-apply-provider-to-info
                  "mevedel-models" (info provider))
(declare-function mevedel-model-parse-selector "mevedel-models" (value))
(declare-function mevedel-model-resolve-selector
                  "mevedel-models" (selector &optional noerror))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--format-render-data-block
                  "mevedel-pipeline" (render-data))
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))

;; `mevedel-structs'
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-hook-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-effort-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-model-override
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-invoked-skills
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-session-id "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-turn-count "mevedel-structs" (cl-x) t)
(defvar mevedel--current-directive-uuid)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-tool-exec'
(declare-function mevedel-tool-exec--register "mevedel-tool-exec" ())

;; `mevedel-tool-registry'
(declare-function mevedel-tool-get "mevedel-tool-registry"
                  (name &optional category))

;; `mevedel-tools'
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-terminal-mailbox "mevedel-tools" (fsm))
(defvar mevedel-tools--current-fsm)

;; `mevedel-transcript'
(declare-function mevedel-transcript-prompt-transform-start
                  "mevedel-transcript" ())
(declare-function mevedel-transcript-restore-ignored-properties
                  "mevedel-transcript" (start end))

;; `mevedel-turn'
(declare-function mevedel--complete-turn "mevedel-turn" (fsm))

;; `mevedel-utilities'
(declare-function mevedel--clear-user-turn-gptel-properties
                  "mevedel-utilities" (start end))
(declare-function mevedel--format-hook-audit-record
                  "mevedel-utilities" (record))
(declare-function mevedel--hook-prompt-rewrite-audit-record
                  "mevedel-utilities"
                  (event original submitted &optional reason))

;; `text-property-search'
(declare-function prop-match-end "text-property-search" (match))
(declare-function text-property-search-backward
                  "text-property-search"
                  (property &optional value predicate not-current))


;;
;;; Request-scoped skill context

(defun mevedel-skills--activate-request-context (request rules hooks)
  "Append skill-scoped RULES and HOOKS to REQUEST."
  (when rules
    (setf (mevedel-request-skill-permission-rules request)
          (append (mevedel-request-skill-permission-rules request)
                  rules)))
  (when hooks
    (setf (mevedel-request-hook-rules request)
          (append (mevedel-request-hook-rules request)
                  hooks))))

(defvar-local mevedel-skills--pending-request-context nil
  "Buffer-local pending request context for the next mevedel-request.

A plist of the form
  (:permission-rules RULES :model MODEL :effort EFFORT
   :hook-rules HOOKS :invoked-skills SKILLS)

populated by user-dispatched skill invocation before `gptel-send'
fires.  Drained into the new `mevedel-request' slots by the
WAIT-state begin handler in `mevedel-presets.el' (see also
`mevedel-skills--drain-pending-context').

Cleared on drain.  Cleared by an `unwind-protect' in the slash
or skill dispatch path if `gptel-send' aborts before request creation.")

(put 'mevedel-skills--pending-request-context 'permanent-local t)

(defvar-local mevedel-skills--pending-inline-attachments nil
  "Prepared inline `$skill' attachments for the next prompt transform.

Each entry is a plist with :name, :body, :skill, and :arguments.
The stash lives on the chat/data buffer.  It is populated before
`gptel-send' and consumed by
`mevedel-skills--transform-expand-inline-attachments' in gptel's
temporary prompt buffer.")

(put 'mevedel-skills--pending-inline-attachments 'permanent-local t)

(defcustom mevedel-skills-max-recursion-depth 4
  "Maximum nesting depth for skill invocations.
A skill body that invokes another skill increments the depth;
exceeding this limit fails the inner invocation with an error
outcome.  Default of 4 allows skill A -> B -> C -> D before failing."
  :type 'integer
  :group 'mevedel)

(defvar mevedel-skills--invoke-depth 0
  "Dynamic depth of nested `mevedel-skills-invoke' calls.
Let-bound around each invocation so the depth naturally pops on
control-flow exit (return, error, throw, abort).")

(defun mevedel-skills--current-invocation ()
  "Return the active sub-agent invocation, or nil.
Reads the buffer-local `mevedel--agent-invocation' set by
`mevedel-agent-exec--allocate-agent-buffer' on agent buffers;
returns nil when called outside any sub-agent."
  (and (boundp 'mevedel--agent-invocation)
       mevedel--agent-invocation))

(defun mevedel-skills--current-request ()
  "Return the active request struct, or nil."
  (and (boundp 'mevedel--current-request)
       mevedel--current-request))

(defun mevedel-skills--current-model-override ()
  "Return the active skill model override, or nil.
Checks the active sub-agent invocation first (innermost wins), then
the request struct.  Used by the WAIT-state apply handler to swap
`info :backend' and `info :model' on the next gptel iteration."
  (or (when-let* ((inv (mevedel-skills--current-invocation)))
        (mevedel-agent-invocation-skill-model-override inv))
      (when-let* ((req (mevedel-skills--current-request)))
        (mevedel-request-skill-model-override req))))

(defun mevedel-skills--pre-realize-model-override ()
  "Return the model selector visible before gptel realizes request data.

Checks active invocation/request overrides first, then the pending
slash/inline skill stash that has not yet been drained into a request."
  (or (mevedel-skills--current-model-override)
      (plist-get mevedel-skills--pending-request-context :model)))

(defun mevedel-skills--model-selector (skill)
  "Return SKILL's parsed model selector, or nil."
  (when-let* ((model (mevedel-skill-model skill)))
    (mevedel-model-parse-selector model)))

(defun mevedel-skills--drain-pending-context (request)
  "Drain `mevedel-skills--pending-request-context' (buffer-local) into REQUEST.

After this call the buffer-local stash is nil.  No-op when no stash
is present.

The stash plist keys map onto the request slots:

- :permission-rules -> `mevedel-request-skill-permission-rules'
- :model            -> `mevedel-request-skill-model-override'
- :effort           -> `mevedel-request-skill-effort-override'
- :hook-rules       -> `mevedel-request-hook-rules'
- :invoked-skills   -> appended to `mevedel-session-invoked-skills'
                       on the request's session"
  (when-let* ((ctx mevedel-skills--pending-request-context))
    (when-let* ((rules (plist-get ctx :permission-rules)))
      (setf (mevedel-request-skill-permission-rules request) rules))
    (when-let* ((model (plist-get ctx :model)))
      (setf (mevedel-request-skill-model-override request) model))
    (when-let* ((effort (plist-get ctx :effort)))
      (setf (mevedel-request-skill-effort-override request) effort))
    (when-let* ((hooks (plist-get ctx :hook-rules)))
      (setf (mevedel-request-hook-rules request) hooks))
    (when-let* ((skills (plist-get ctx :invoked-skills))
                (session mevedel--session))
      (setf (mevedel-session-invoked-skills session)
            (append (mevedel-session-invoked-skills session) skills)))
    (setq-local mevedel-skills--pending-request-context nil)))

(defun mevedel-skills--transform-apply-model-override (fsm)
  "Pre-realize transform: apply skill model overrides to prompt locals.

FSM is the active gptel request state machine.

gptel realizes request payloads from the temp prompt buffer's
buffer-local `gptel-backend' and `gptel-model'.  Applying the override
here lets cross-backend skill pins build backend-correct request data.
Post-realize model-side overrides remain handled by
`mevedel-skills--apply-overrides-handler'."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (when-let* ((override (with-current-buffer chat-buffer
                              (mevedel-skills--pre-realize-model-override)))
                  (provider (mevedel-model-resolve-selector override t)))
        (setq-local gptel-backend (plist-get provider :backend))
        (setq-local gptel-model (plist-get provider :model))))))

(defun mevedel-skills--apply-overrides-handler (fsm)
  "WAIT-state handler: apply post-realize skill model overrides to FSM info.

Reads the active model override (from sub-agent invocation or
request) and mutates `info :backend' and `info :model' so the
upcoming gptel-request fires with the override.  No-op when no
override is in effect.

This is a post-realize safety rail for model-side skill invocations
that arrive during an already-running tool loop.  Same-backend model
swaps are allowed; cross-backend swaps are rejected by
`mevedel-model-apply-provider-to-info' because `info' :data is already
backend-specific.

  Effort is parsed and stored on the same slot but not applied here
  -- gptel does not yet expose an effort knob.  When it does, this
  handler will mutate the corresponding info key the same way."
  (let* ((info (gptel-fsm-info fsm))
         (chat-buffer (plist-get info :buffer)))
    (when (and chat-buffer (buffer-live-p chat-buffer))
      (with-current-buffer chat-buffer
        (when-let* ((override (mevedel-skills--current-model-override))
                    (provider (mevedel-model-resolve-selector override t)))
          (setf (gptel-fsm-info fsm)
                (mevedel-model-apply-provider-to-info info provider)))))))


;;
;;; Argument tokenization

(defun mevedel-skills--parse-arguments (arguments)
  "Parse ARGUMENTS into a list of tokens, shell-style.
Returns nil when ARGUMENTS is nil or blank.  Falls back to
whitespace splitting when shell parsing fails (unbalanced quotes
etc.).  Empty tokens that can fall out of leading/trailing
whitespace are filtered.  Ports the parsing half of ccs's
argumentSubstitution.ts."
  (cond
   ((null arguments) nil)
   ((not (stringp arguments)) nil)
   ((string-blank-p arguments) nil)
   (t
    (cl-remove-if #'string-empty-p
                  (condition-case nil
                      (split-string-and-unquote arguments)
                    (error
                     (split-string arguments "[ \t\n]+" t)))))))


;;
;;; Variable substitution

(defconst mevedel-skills--non-author-text-property
  'mevedel-skills-non-author-text
  "Text property set on content not written literally in SKILL.md.")

(defconst mevedel-skills--literal-placeholder-property
  'mevedel-skills-literal-placeholder
  "Text property set on escaped placeholders that must stay literal.")

(defvar mevedel-skills--substitution-made-p nil
  "Non-nil when skill variable substitution replaced text.")

(defun mevedel-skills--word-char-p (ch)
  "Return non-nil when CH is a word character (`[A-Za-z0-9_]')."
  (and ch
       (or (and (>= ch ?a) (<= ch ?z))
           (and (>= ch ?A) (<= ch ?Z))
           (and (>= ch ?0) (<= ch ?9))
           (eq ch ?_))))

(defun mevedel-skills--mark-non-author-text (value)
  "Return VALUE marked as text not written literally in SKILL.md."
  (let ((copy (copy-sequence (or value ""))))
    (add-text-properties
     0 (length copy)
     (list mevedel-skills--non-author-text-property t)
     copy)
    copy))

(defun mevedel-skills--property-range-p (text start end property)
  "Return non-nil when TEXT has PROPERTY anywhere from START to END."
  (let ((pos start)
        found)
    (while (and (< pos end) (not found))
      (if (get-text-property pos property text)
          (setq found t)
        (setq pos (or (next-single-property-change pos property text end)
                      end))))
    found))

(defun mevedel-skills--non-author-range-p (text start end)
  "Return non-nil when TEXT has any non-author content from START to END."
  (mevedel-skills--property-range-p
   text start end mevedel-skills--non-author-text-property))

(defun mevedel-skills--literal-placeholder-range-p (text start end)
  "Return non-nil when TEXT has literal placeholder content from START to END."
  (mevedel-skills--property-range-p
   text start end mevedel-skills--literal-placeholder-property))

(defun mevedel-skills--protected-substitution-range-p (text start end)
  "Return non-nil when TEXT from START to END must not be substituted."
  (or (mevedel-skills--non-author-range-p text start end)
      (mevedel-skills--literal-placeholder-range-p text start end)))

(defun mevedel-skills--author-ranges-p (text &rest ranges)
  "Return non-nil when every range in RANGES is author-written in TEXT.
RANGES is a flat list of START/END pairs."
  (let ((author-p t))
    (while (and ranges author-p)
      (let ((start (pop ranges))
            (end (pop ranges)))
        (when (mevedel-skills--non-author-range-p text start end)
          (setq author-p nil))))
    author-p))

(defun mevedel-skills--replace-match-with-non-author (value)
  "Replace the current match with VALUE marked as non-author text."
  (let ((start (match-beginning 0))
        (end (match-end 0)))
    (setq mevedel-skills--substitution-made-p t)
    (delete-region start end)
    (goto-char start)
    (insert (mevedel-skills--mark-non-author-text value))))

(defconst mevedel-skills--literal-variable-placeholders
  '("${CLAUDE_SESSION_ID}"
    "${CLAUDE_SKILL_DIR}"
    "${CLAUDE_EFFORT}"
    "${MEVEDEL_SESSION_ID}"
    "${MEVEDEL_SKILL_DIR}"
    "${MEVEDEL_EFFORT}")
  "Literal skill variable placeholders supported by substitution.")

(defun mevedel-skills--placeholder-end-at-point (argument-names)
  "Return placeholder end at point, or nil when point is not at one.
ARGUMENT-NAMES is the list of named skill arguments."
  (or (cl-loop for placeholder in mevedel-skills--literal-variable-placeholders
               when (looking-at (regexp-quote placeholder))
               return (match-end 0))
      (when (looking-at "\\$ARGUMENTS\\(\\[[0-9]+\\]\\)?")
        (match-end 0))
      (when (looking-at "\\$\\([0-9]+\\)")
        (let ((end (match-end 0)))
          (unless (mevedel-skills--word-char-p (char-after end))
            end)))
      (cl-loop for name in argument-names
               for target = (concat "$" name)
               for end = (+ (point) (length target))
               when (and (looking-at (regexp-quote target))
                         (not (eq (char-after end) ?\[))
                         (not (mevedel-skills--word-char-p
                               (char-after end))))
               return end)))

(defun mevedel-skills--protect-escaped-placeholders (text argument-names)
  "Return TEXT with escaped placeholders made literal.
ARGUMENT-NAMES is the list of named skill arguments.
A backslash before a recognized placeholder, such as `\\$ARGUMENTS',
is removed and the placeholder is protected from substitution."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "\\$" nil t)
      (let ((slash (match-beginning 0))
            (dollar (1- (point))))
        (goto-char dollar)
        (let ((end (mevedel-skills--placeholder-end-at-point argument-names)))
          (if end
              (progn
                (delete-region slash (1+ slash))
                (add-text-properties
                 slash (1- end)
                 (list mevedel-skills--literal-placeholder-property t))
                (goto-char (1- end)))
            (goto-char (1+ dollar))))))
    (buffer-string)))

(defun mevedel-skills--substitute-named (text name value)
  "Replace `$NAME' with VALUE in TEXT, strict word-boundary matching.

Skips `$NAME[...]' (indexed access form) and `$NAMEident' (longer
identifier).  Emulates ccs's `\\=$NAME(?![\\=[\\=w])' regex.
Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let ((target (concat "$" name)))
        (while (search-forward target nil t)
          (let ((next (char-after)))
            (cond
             ;; Followed by [ -> indexed-access form, skip
             ((eq next ?\[) nil)
             ;; Followed by word char -> longer identifier, skip
             ((mevedel-skills--word-char-p next) nil)
             ((mevedel-skills--protected-substitution-range-p
               (buffer-string) (match-beginning 0) (match-end 0))
              nil)
             (t
              (mevedel-skills--replace-match-with-non-author value))))))
      (buffer-string))))

(defun mevedel-skills--substitute-shorthand (text parsed-args)
  "Replace `$N' shorthand with PARSED-ARGS[N] (zero-based) in TEXT.

Strict word-boundary: `$1' followed by a word char (e.g. `$1foo') is
not substituted.  `$ARGUMENTS' starts with `A' (a word char following
the `$') so this regex naturally skips it.  Indices out of range are
substituted with the empty string.  Case-sensitive."
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "\\$\\([0-9]+\\)" nil t)
        (let ((next (char-after)))
          (unless (or (mevedel-skills--word-char-p next)
                      (mevedel-skills--protected-substitution-range-p
                       (buffer-string) (match-beginning 0) (match-end 0)))
            (let* ((idx (string-to-number (match-string 1)))
                   (val (or (nth idx parsed-args) "")))
              (mevedel-skills--replace-match-with-non-author val)))))
      (buffer-string))))

(defun mevedel-skills--substitute-regexp
    (text regexp replacement-fn &optional author-only-p)
  "Replace REGEXP matches in TEXT with non-author replacement text.
REPLACEMENT-FN is called before the match is deleted, so it may use
`match-string' to inspect subgroups in the current buffer.
When AUTHOR-ONLY-P is non-nil, skip matches that overlap non-author text."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (unless (or (mevedel-skills--literal-placeholder-range-p
                   (buffer-string) (match-beginning 0) (match-end 0))
                  (and author-only-p
                       (mevedel-skills--non-author-range-p
                        (buffer-string) (match-beginning 0) (match-end 0))))
        (mevedel-skills--replace-match-with-non-author
         (funcall replacement-fn))))
    (buffer-string)))

(defun mevedel-skills--substitute-vars (text arguments session skill)
  "Return TEXT with skill placeholders expanded.

Algorithm follows ccs's `argumentSubstitution.ts' for unescaped
placeholders, with the mevedel extension that a backslash before a
recognized placeholder keeps it literal.  Substitution order
\=(zero-based throughout):

1. Named arguments from SKILL's `argument-names' slot, mapping
   ARGUMENT-NAMES[i] -> PARSED-ARGS[i].
2. `$ARGUMENTS[N]'.
3. `$N' shorthand.
4. `$ARGUMENTS' (the raw argument string).
5. `${CLAUDE_SESSION_ID}', `${CLAUDE_SKILL_DIR}', `${CLAUDE_EFFORT}',
   and their `${MEVEDEL_*}' aliases.  These are substituted after the
   placeholder-substituted check below.

If ARGUMENTS is non-empty AND none of steps 1-4 substituted
anything, append `\\nARGUMENTS: <raw>' so the body still receives
the user's input.

Named-argument matching uses strict word-boundary semantics so
`$foo' does not match `$foo[0]' or `$foobar'.  Numeric-only
argument names are filtered out at scan time
\\=(see `mevedel-skills--parse-argument-names') so they cannot
shadow `$0'/`$1' shorthand."
  (let* ((session-id (or (and session (mevedel-session-session-id session))
                         (and session (mevedel-session-name session))
                         ""))
         (skill-dir (or (and skill (mevedel-skill-source-dir skill)) ""))
         (effort (or (and skill
                          (mevedel-skill-effort skill)
                          (symbol-name (mevedel-skill-effort skill)))
                     ""))
         (argument-names (and skill (mevedel-skill-argument-names skill)))
         (raw-args arguments)
         (parsed-args (mevedel-skills--parse-arguments raw-args))
         (full (or raw-args ""))
         (mevedel-skills--substitution-made-p nil)
         (result (mevedel-skills--protect-escaped-placeholders
                  text argument-names)))
    ;; 1. Named arguments.
    (cl-loop for name in argument-names
             for i from 0
             for value = (or (nth i parsed-args) "")
             do (setq result
                      (mevedel-skills--substitute-named result name value)))
    ;; 2. $ARGUMENTS[N].
    (setq result
          (mevedel-skills--substitute-regexp
           result
           "\\$ARGUMENTS\\[\\([0-9]+\\)\\]"
           (lambda ()
             (or (nth (string-to-number (match-string 1))
                      parsed-args)
                 ""))))
    ;; 3. $N shorthand.
    (setq result (mevedel-skills--substitute-shorthand result parsed-args))
    ;; 4. $ARGUMENTS (full).
    (setq result
          (mevedel-skills--substitute-regexp
           result "\\$ARGUMENTS" (lambda () full)))
    ;; Decide append-fallback BEFORE the mevedel-specific ${...} subs
    ;; so they don't influence the "no placeholder substituted" check.
    (let ((args-substituted mevedel-skills--substitution-made-p))
      ;; 5. Claude-compatible and mevedel-native literal variables.
      (dolist (var `(("${CLAUDE_SESSION_ID}" . ,session-id)
                     ("${CLAUDE_SKILL_DIR}" . ,skill-dir)
                     ("${CLAUDE_EFFORT}" . ,effort)
                     ("${MEVEDEL_SESSION_ID}" . ,session-id)
                     ("${MEVEDEL_SKILL_DIR}" . ,skill-dir)
                     ("${MEVEDEL_EFFORT}" . ,effort)))
        (setq result
              (mevedel-skills--substitute-regexp
               result (regexp-quote (car var))
               (lambda () (cdr var)) t)))
      ;; 6. Append-fallback: only when args were supplied AND non-empty
      ;; AND nothing was substituted.
      (when (and (not args-substituted)
                 (stringp raw-args)
                 (not (string-empty-p raw-args)))
        (setq result
              (concat result "\n\nARGUMENTS: "
                      (mevedel-skills--mark-non-author-text raw-args)))))
    result))


;;
;;; Body injections

(define-error 'mevedel-skills-shell-abort
  "Skill body shell expansion failed; skill must abort.")

(defun mevedel-skills--injection-outcome-error-p (result)
  "Return non-nil when pipeline RESULT means body injection failed."
  (and (stringp result)
       (or (string-prefix-p "Error:" result)
           (string-prefix-p "Command failed with exit code" result)
           (string-prefix-p "Failed to start process:" result))))

(defun mevedel-skills--run-shell-command-async (command marker callback)
  "Run COMMAND through the Bash tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original shell-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Bash"))
                  (progn
                    (require 'mevedel-tool-exec)
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Bash"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append (mevedel-tool-args tool)
                    '((trust-literal-p boolean :optional
                                       "Internal trusted skill input.")))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason shell-failure
                         :message "Bash tool is not registered.")))
     (t
      (condition-case err
          (progn
            (unless (fboundp 'mevedel-tools--current-deferred-context)
              (require 'mevedel-tools))
            (mevedel-pipeline-run-tool
             tool
             (lambda (result)
               (cond
                ((and (stringp result)
                      (string-prefix-p "Error: Permission denied" result))
                 (funcall callback
                          `(:status error :reason permission-denied
                                    :message ,(format "Shell expansion %s denied: %s"
                                                      marker result))))
                ((mevedel-skills--injection-outcome-error-p result)
                 (funcall callback
                          `(:status error :reason shell-failure
                                    :message ,(format "Shell expansion %s failed: %s"
                                                      marker result))))
                (t
                 (funcall callback
                          `(:status ok :output ,(string-trim-right
                                                 (or result "")))))))
             (list :command command :trust-literal-p t)))
        (error
         (funcall callback
                  `(:status error :reason shell-failure
                            :message ,(format "Shell expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills--run-elisp-expression-async (expression marker callback)
  "Run EXPRESSION through the Eval tool pipeline, then call CALLBACK.

CALLBACK receives either \\=(:status ok :output STRING) or
\\=(:status error :reason SYMBOL :message STRING).  MARKER is the
original elisp-injection marker used in diagnostics."
  (let ((tool (or (ignore-errors (mevedel-tool-get "Eval"))
                  (progn
                    (require 'mevedel-tool-exec)
                    (mevedel-tool-exec--register)
                    (ignore-errors (mevedel-tool-get "Eval"))))))
    (when tool
      (setq tool (copy-mevedel-tool tool))
      (setf (mevedel-tool-args tool)
            (append
             (mevedel-tool-args tool)
             '((trust-literal-p boolean :optional
                                "Internal trusted skill input.")
               (result-format string :optional
                              "Internal skill result format.")))))
    (cond
     ((null tool)
      (funcall callback
               `(:status error :reason elisp-failure
                         :message "Eval tool is not registered.")))
     (t
      (condition-case err
          (progn
            (unless (fboundp 'mevedel-tools--current-deferred-context)
              (require 'mevedel-tools))
            (mevedel-pipeline-run-tool
             tool
             (lambda (result)
               (cond
                ((and (stringp result)
                      (string-prefix-p "Error: Permission denied" result))
                 (funcall callback
                          `(:status error :reason permission-denied
                                    :message ,(format "Elisp expansion %s denied: %s"
                                                      marker result))))
                ((mevedel-skills--injection-outcome-error-p result)
                 (funcall callback
                          `(:status error :reason elisp-failure
                                    :message ,(format "Elisp expansion %s failed: %s"
                                                      marker result))))
                (t
                 (funcall callback
                          `(:status ok :output ,(string-trim-right
                                                 (or result "")))))))
             (list :expression expression
                   :trust-literal-p t
                   :result-format "injection")))
        (error
         (funcall callback
                  `(:status error :reason elisp-failure
                            :message ,(format "Elisp expansion %s errored: %s"
                                              marker
                                              (error-message-string err))))))))))

(defun mevedel-skills--ranges-overlap-p (ranges start end)
  "Return non-nil when any range in RANGES overlaps START to END."
  (let (found)
    (while (and ranges (not found))
      (let ((range (pop ranges)))
        (when (and (< start (cdr range))
                   (< (car range) end))
          (setq found t))))
    found))

(defun mevedel-skills--markdown-injection-fence-opener-p (line marker)
  "Return non-nil when LINE and MARKER open a body-injection fence."
  (and (string= marker "```")
       (or (string= line "```!")
           (string-match-p "\\````!el[ \t]*\\'" line))))

(defun mevedel-skills--markdown-authored-fence-close-end (text close-re start)
  "Return end of the next CLOSE-RE match in TEXT after START."
  (let ((search start)
        close-end)
    (while (and (not close-end)
                (string-match close-re text search))
      (if (mevedel-skills--author-ranges-p
           text (match-beginning 0) (match-end 0))
          (setq close-end (match-end 0))
        (setq search (match-end 0))))
    close-end))

(defun mevedel-skills--markdown-code-fence-ranges (text)
  "Return ordinary Markdown code-fence ranges in TEXT.
Body-injection fences are deliberately excluded so they remain
active skill syntax."
  (let ((ranges nil)
        (pos 0)
        (len (length text)))
    (while (and (< pos len)
                (string-match "\\(^\\|\n\\)\\(```+\\)[^\n]*\\(\n\\|\\'\\)"
                              text pos))
      (let* ((line-start (+ (match-beginning 0)
                            (length (match-string 1 text))))
             (marker (match-string 2 text))
             (line-end (if (string= (match-string 3 text) "\n")
                           (1- (match-end 0))
                         (match-end 0)))
             (line (substring text line-start line-end))
             (body-start (match-end 0))
             (close-re (concat "\\(^\\|\n\\)"
                               (regexp-quote marker)
                               "\\(\n\\|\\'\\)"))
             (close-end (mevedel-skills--markdown-authored-fence-close-end
                         text close-re body-start)))
        (if (mevedel-skills--markdown-injection-fence-opener-p line marker)
            (setq pos (or close-end len))
          (if close-end
              (progn
                (push (cons line-start close-end) ranges)
                (setq pos close-end))
            (push (cons line-start len) ranges)
            (setq pos len)))))
    (nreverse ranges)))

(defun mevedel-skills--injection-inline-marker-start (text position)
  "Return the injection marker start before TEXT's backtick at POSITION."
  (cond
   ((and (> position 0)
         (= (aref text (1- position)) ?!))
    (1- position))
   ((and (>= position 3)
         (string= (substring text (- position 3) position) "!el"))
    (- position 3))))

(defun mevedel-skills--injection-inline-span-end (text position line-end)
  "Return end of authored inline injection at POSITION, or nil.
POSITION must point at the opening backtick.  LINE-END is the
exclusive end of the current line."
  (when-let* ((marker-start
               (mevedel-skills--injection-inline-marker-start text position))
              ((mevedel-skills--author-ranges-p
                text marker-start (1+ position))))
    (let ((search (1+ position))
          span-end)
      (while (and (not span-end)
                  (string-match "`" text search)
                  (<= (match-end 0) line-end))
        (let ((close-start (match-beginning 0))
              (close-end (match-end 0)))
          (if (mevedel-skills--author-ranges-p text close-start close-end)
              (setq span-end close-end)
            (setq search close-end))))
      span-end)))

(defun mevedel-skills--markdown-inline-code-ranges (text fence-ranges)
  "Return Markdown inline code-span ranges in TEXT outside FENCE-RANGES."
  (let ((ranges nil)
        (line-start 0)
        (len (length text)))
    (while (< line-start len)
      (let* ((line-end (or (string-match "\n" text line-start) len))
             (pos line-start))
        (while (and (< pos line-end)
                    (string-match "`+" text pos))
          (let* ((run-start (match-beginning 0))
                 (run-end (match-end 0))
                 (run (match-string 0 text)))
            (cond
             ((or (>= run-start line-end)
                  (mevedel-skills--ranges-overlap-p
                   fence-ranges run-start run-end))
              (setq pos run-end))
             ((if-let* ((injection-end
                         (mevedel-skills--injection-inline-span-end
                          text run-start line-end)))
                  (setq pos injection-end)))
             ((and (string-match (regexp-quote run) text run-end)
                   (<= (match-end 0) line-end))
              (push (cons run-start (match-end 0)) ranges)
              (setq pos (match-end 0)))
             (t
              (setq pos run-end)))))
        (setq line-start (if (< line-end len) (1+ line-end) len))))
    (nreverse ranges)))

(defun mevedel-skills--markdown-code-ranges (text)
  "Return Markdown code ranges in TEXT that should not run as injections."
  (let* ((fence-ranges (mevedel-skills--markdown-code-fence-ranges text))
         (inline-ranges (mevedel-skills--markdown-inline-code-ranges
                         text fence-ranges)))
    (sort (append fence-ranges inline-ranges)
          (lambda (a b) (< (car a) (car b))))))

(defun mevedel-skills--injection-match (text)
  "Return the next body-injection match in TEXT.

The return value is a plist with :start, :end, :command, and
:marker, or nil when TEXT contains no injection marker."
  (let ((matches nil)
        (markdown-code-ranges (mevedel-skills--markdown-code-ranges text)))
    (cl-labels
        ((scan-inline
          (opener kind payload-key)
          (let ((pos 0)
                (opener-re (regexp-quote opener))
                (len (length text)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills--author-ranges-p
                            text start body-start)
                           (not (mevedel-skills--ranges-overlap-p
                                 markdown-code-ranges start body-start)))
                  (while (and (not done)
                              (string-match "`" text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-end 0)))
                      (cond
                       ((string-match-p
                         "\n" (substring text body-start close-start))
                        (setq done t))
                       ((and (mevedel-skills--author-ranges-p
                              text close-start close-end)
                             (not (mevedel-skills--ranges-overlap-p
                                   markdown-code-ranges close-start close-end)))
                        (let ((payload (substring text body-start
                                                  close-start)))
                          (push (list :kind kind
                                      :start start
                                      :end close-end
                                      payload-key payload
                                      :marker (format "%s%s`" opener payload))
                                matches))
                        (setq done t))
                       (t
                        (setq search close-end))))))
                (setq pos (min len (max (1+ start) body-start)))))))
         (scan-fenced
          (opener-regexp kind payload-key marker)
          (let ((pos 0)
                (opener-re (concat "\\(^\\|\n\\)" opener-regexp)))
            (while (string-match opener-re text pos)
              (let* ((start (match-beginning 0))
                     (prefix-start (match-beginning 1))
                     (prefix-end (match-end 1))
                     (prefix (match-string 1 text))
                     (marker-start (+ start (length prefix)))
                     (body-start (match-end 0))
                     (search body-start)
                     (done nil))
                (when (and (mevedel-skills--author-ranges-p
                            text prefix-start prefix-end marker-start body-start)
                           (not (mevedel-skills--ranges-overlap-p
                                 markdown-code-ranges marker-start body-start)))
                  (while (and (not done)
                              (string-match "\n```\\(\n\\|\\'\\)"
                                            text search))
                    (let ((close-start (match-beginning 0))
                          (close-end (match-beginning 1))
                          (suffix-start (match-beginning 1))
                          (suffix-end (match-end 1)))
                      (if (and (mevedel-skills--author-ranges-p
                                text close-start close-end suffix-start suffix-end)
                               (not (mevedel-skills--ranges-overlap-p
                                     markdown-code-ranges close-start close-end)))
                          (let ((payload (substring text body-start
                                                    close-start)))
                            (push (list :kind kind
                                        :start start
                                        :end (match-end 0)
                                        payload-key payload
                                        :marker marker
                                        :prefix prefix
                                        :suffix (match-string 1 text))
                                  matches)
                            (setq done t))
                        (setq search (match-end 0))))))
                (setq pos (max (1+ start) body-start)))))))
      (scan-fenced (regexp-quote "```!\n")
                   'shell :command "(fenced shell block)")
      (scan-fenced "```!el[ \t]*\n"
                   'elisp :expression "(fenced elisp block)")
      (scan-inline "!`" 'shell :command)
      (scan-inline "!el`" 'elisp :expression)
      (car (sort matches
                 (lambda (a b)
                   (< (plist-get a :start)
                      (plist-get b :start))))))))

(defun mevedel-skills--run-body-injections-async (text callback)
  "Replace skill body injection markers in TEXT, then call CALLBACK.

CALLBACK receives either \\=(:status ok :body STRING) or
\\=(:status error :reason SYMBOL :message STRING).

Supported markers:
- !`COMMAND`          inline: run COMMAND, substitute stdout
- ```!\\nSCRIPT\\n``` fenced block: run SCRIPT as a shell script
- !el`EXPRESSION`     inline: evaluate EXPRESSION, substitute result
- ```!el\\nEXPR\\n``` fenced block: evaluate EXPR, substitute result

Each command/expression goes through its normal tool pipeline with
`:trust-literal-p t', so permission checking, execution, and
oversized-result persistence stay aligned with normal tool
execution."
  (if-let* ((match (mevedel-skills--injection-match text)))
      (let ((start (plist-get match :start))
            (end (plist-get match :end))
            (kind (plist-get match :kind))
            (marker (plist-get match :marker))
            (prefix (or (plist-get match :prefix) ""))
            (suffix (or (plist-get match :suffix) ""))
            (origin-buffer (current-buffer)))
        (funcall
         (pcase kind
           ('shell #'mevedel-skills--run-shell-command-async)
           ('elisp #'mevedel-skills--run-elisp-expression-async))
         (or (plist-get match :command)
             (plist-get match :expression))
         marker
         (lambda (outcome)
           (if (not (buffer-live-p origin-buffer))
               (funcall callback
                        `(:status error :reason aborted
                                  :message "Skill buffer was killed during body injection expansion."))
             (with-current-buffer origin-buffer
               (pcase (plist-get outcome :status)
                 ('ok
                  (mevedel-skills--run-body-injections-async
                   (concat (substring text 0 start)
                           prefix
                           (mevedel-skills--mark-non-author-text
                            (plist-get outcome :output))
                           suffix
                           (substring text end))
                   callback))
                 (_
                  (funcall callback outcome))))))))
    (funcall callback `(:status ok :body ,(substring-no-properties text)))))


;;
;;; Unified skill invocation API

(defun mevedel-skills--display-event (display-callback event)
  "Funcall DISPLAY-CALLBACK with EVENT, ignoring errors.
DISPLAY-CALLBACK may be nil; EVENT is a lifecycle event plist
\\=."
  (when display-callback
    (condition-case err
        (funcall display-callback event)
      (error
       (display-warning
        'mevedel
        (format "Skill display-callback error: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-skills--invoke-error (skill reason message
                                           callback display-callback)
  "Emit SKILL error event, then deliver REASON and MESSAGE to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event error :skill ,skill-name
              :reason ,reason :message ,message))
    (funcall callback
             `(:status error :reason ,reason :message ,message))))

(defun mevedel-skills--run-expansion-hook
    (skill arguments prompt trigger session callback)
  "Run `UserPromptExpansion' for a user SKILL expansion.
ARGUMENTS is the raw user argument string.  SESSION supplies workspace
context.  CALLBACK receives (PROMPT DECISION).  Non-user TRIGGER values
skip the hook and call CALLBACK with PROMPT and nil."
  (if (not (eq trigger 'user-skill))
      (funcall callback prompt nil)
    (require 'mevedel-hooks)
    (let* ((workspace (and session (mevedel-session-workspace session)))
           (request (and (boundp 'mevedel--current-request)
                         mevedel--current-request)))
      (mevedel-hooks-run-event
       'UserPromptExpansion
       (mevedel-hooks-event-plist
        'UserPromptExpansion session workspace
        :skill-name (mevedel-skill-name skill)
        :arguments arguments
        :prompt prompt)
       (lambda (decision)
         (let* ((updated (plist-get decision :updated-input))
                (context (mevedel-hooks-additional-context-string
                          decision 'UserPromptExpansion))
                (prompt (if (stringp updated) updated prompt)))
           (when (and context (not (string-empty-p context)))
             (setq prompt (concat prompt "\n\n" context)))
           (funcall callback prompt decision)))
       session workspace request nil))))

(defun mevedel-skills--invoke-done (skill outcome callback display-callback)
  "Emit SKILL done event, then deliver OUTCOME to CALLBACK.
DISPLAY-CALLBACK receives the lifecycle event when non-nil."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (mevedel-skills--display-event
     display-callback
     `(:event done :skill ,skill-name))
    (funcall callback outcome)))

(defun mevedel-skills--safe-hook-decision (event decision)
  "Return plist-shaped hook DECISION for EVENT, or nil.

Hook runners normally sanitize their callback value, but callers can
stub them in tests and older compiled code may still deliver malformed
values.  Skill dispatch reads prompt decisions synchronously, so keep
that boundary defensive."
  (if (and (listp decision)
           (or (null decision)
               (keywordp (car-safe decision))))
      decision
    (display-warning
     'mevedel
     (format "Ignoring malformed %s hook decision: %S" event decision)
     :warning)
    nil))

(defun mevedel-skills--prompt-rewrite-audit-record (original decision)
  "Return a `UserPromptExpansion' rewrite audit record, or nil."
  (when-let* ((updated (plist-get decision :updated-input))
              ((stringp updated)))
    (mevedel--hook-prompt-rewrite-audit-record
     'UserPromptExpansion original updated
     (mevedel-hooks-decision-reason decision))))

(defun mevedel-skills-inline-display-text (name arguments)
  "Return the compact view text for inline skill NAME and ARGUMENTS."
  (if (and arguments (not (string-empty-p arguments)))
      (format "$%s %s" name arguments)
    (format "$%s" name)))

(defun mevedel-skills-format-inline-render-data (skill arguments)
  "Return hidden render-data for an expanded inline user SKILL.

ARGUMENTS is the raw user argument string.

The block is ignored by gptel and consumed by `mevedel-view' so the
data buffer keeps the expanded prompt while the view can show the
original `$skill' invocation compactly."
  (let* ((name (mevedel-skill-name skill))
         (data (list :kind 'inline-skill
                     :name name
                     :arguments arguments
                     :display-text
                     (mevedel-skills-inline-display-text
                      name arguments)))
         (block (propertize
                 (progn
                   (require 'mevedel-pipeline)
                   (mevedel-pipeline--format-render-data-block data))
                 'gptel 'ignore)))
    block))

(defun mevedel-skills--insert-inline-user-skill-render-data
    (skill arguments)
  "Insert hidden render-data for SKILL with raw user ARGUMENTS."
  (insert (mevedel-skills-format-inline-render-data skill arguments)))

(defun mevedel-skills-format-inline-attachment-render-data (attachments)
  "Return hidden render-data for inline skill ATTACHMENTS."
  (let ((data (list :kind 'inline-skill-attachments
                    :skills (mapcar
                             (lambda (attachment)
                               (list :name (plist-get attachment :name)))
                             attachments))))
    (propertize
     (progn
       (require 'mevedel-pipeline)
       (mevedel-pipeline--format-render-data-block data))
     'gptel 'ignore)))

(defun mevedel-skills--inline-attachment-reminder (attachment)
  "Return system-reminder body for prepared inline skill ATTACHMENT."
  (format "Skill `$%s` was attached by an inline user mention. Follow these instructions for this turn:\n\n%s"
          (plist-get attachment :name)
          (or (plist-get attachment :body) "")))

(defun mevedel-skills--replace-inline-attachment-mentions
    (attachments start end)
  "Replace prompt mentions for prepared ATTACHMENTS between START and END.
Return attachments that were actually referenced, preserving first use."
  (let* ((text (buffer-substring-no-properties start end))
         (by-name (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         replacements
         used)
    (dolist (attachment attachments)
      (puthash (plist-get attachment :name) attachment by-name))
    (dolist (token
             (mevedel-skills--scan-skill-tokens
              text (lambda (candidate) (gethash candidate by-name))))
      (let ((name (plist-get token :name))
            (attachment (plist-get token :value)))
        (push (list (+ start (plist-get token :start))
                    (+ start (plist-get token :end))
                    (format "[skill:%s -- attached]" name))
              replacements)
        (unless (gethash name seen)
          (puthash name t seen)
          (push attachment used))))
    (require 'mevedel-mentions)
    (dolist (replacement (sort replacements
                               (lambda (a b) (> (car a) (car b)))))
      (mevedel-mentions-replace-with-placeholder
       (nth 0 replacement) (nth 1 replacement) (nth 2 replacement)))
    (nreverse used)))

(defun mevedel-skills--transform-expand-inline-attachments (fsm)
  "Expand prepared inline `$skill' attachments into prompt context."
  (when-let* ((chat-buffer (and fsm (plist-get (gptel-fsm-info fsm) :buffer)))
              ((buffer-live-p chat-buffer))
              (attachments
               (buffer-local-value
                'mevedel-skills--pending-inline-attachments chat-buffer)))
    (with-current-buffer chat-buffer
      (setq-local mevedel-skills--pending-inline-attachments nil))
    (require 'mevedel-transcript)
    (let* ((start (copy-marker (mevedel-transcript-prompt-transform-start) nil))
           (used (mevedel-skills--replace-inline-attachment-mentions
                  attachments (marker-position start) (point-max))))
      (when used
        (goto-char start)
        (dolist (attachment used)
          (let ((insert-start (point)))
            (insert "<system-reminder>\n"
                    (mevedel-skills--inline-attachment-reminder attachment)
                    "\n</system-reminder>\n\n")
            (remove-text-properties
             insert-start (point)
             '(gptel nil response nil invisible nil front-sticky nil))))))))

(cl-defun mevedel-skills--activate-context
    (trigger &key permission-rules model effort hook-rules invoked-skill)
  "Apply skill-scoped overrides to the active context.

TRIGGER selects the install path:

- `user-skill': append onto the buffer-local pending stash
  (`mevedel-skills--pending-request-context'); drained at request
  begin by the WAIT-state begin handler in `mevedel-presets.el'.
  Used because user skill dispatch fires before the `mevedel-request'
  has been created.
- `model-skill' / `internal': mutate the active sub-agent
  invocation (innermost) or request directly.

PERMISSION-RULES is a list of parsed mevedel rules to append.
MODEL is a selector plist or nil.  EFFORT is a symbol or nil (currently
inert).  HOOK-RULES is a list of normalized hook rules.  INVOKED-SKILL
is a `mevedel-skill-invocation-record' to record on the session for
compaction/replay.

Emits one-time per-invocation `display-warning' notices when MODEL
or EFFORT is set so skill authors know:
- model: the override is being installed; verify the provider or tier is
  configured with gptel.
- effort: stored but currently has no observable effect (gptel
  does not yet expose an effort knob)."
  (when model
    (display-warning
     'mevedel
     (format "Skill model override %S installed; verify the provider or tier is configured with gptel."
             model)
     :debug))
  (when effort
    (display-warning
     'mevedel
     (format "Skill effort override %S stored but currently inert (gptel does not yet expose an effort knob)."
             effort)
     :debug))
  (cond
   ((eq trigger 'user-skill)
    (let ((existing mevedel-skills--pending-request-context))
      (when permission-rules
        (setq existing
              (plist-put existing :permission-rules
                         (append (plist-get existing :permission-rules)
                                 permission-rules))))
      (when model
        (setq existing (plist-put existing :model model)))
      (when effort
        (setq existing (plist-put existing :effort effort)))
      (when hook-rules
        (setq existing
              (plist-put existing :hook-rules
                         (append (plist-get existing :hook-rules)
                                 hook-rules))))
      (when invoked-skill
        (setq existing
              (plist-put existing :invoked-skills
                         (append (plist-get existing :invoked-skills)
                                 (list invoked-skill)))))
      (setq-local mevedel-skills--pending-request-context existing)))
   (t
    (let ((req (mevedel-skills--current-request))
          (inv (mevedel-skills--current-invocation)))
      ;; Permission rules accumulate on the innermost slot.
      (when permission-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-skill-permission-rules inv)
                (append (mevedel-agent-invocation-skill-permission-rules inv)
                        permission-rules)))
         (req
          (mevedel-skills--activate-request-context
           req permission-rules nil))))
      ;; Model and effort overwrite (last-writer-wins).
      (when model
        (cond
         (inv (setf (mevedel-agent-invocation-skill-model-override inv) model))
         (req (setf (mevedel-request-skill-model-override req) model))))
      (when effort
        (cond
         (inv (setf (mevedel-agent-invocation-skill-effort-override inv) effort))
         (req (setf (mevedel-request-skill-effort-override req) effort))))
      (when hook-rules
        (cond
         (inv
          (setf (mevedel-agent-invocation-hook-rules inv)
                (append (mevedel-agent-invocation-hook-rules inv)
                        hook-rules)))
	 (req
	  (mevedel-skills--activate-request-context
	   req nil hook-rules))))
      ;; Record on the session.
      (when invoked-skill
        (when-let* ((session (and (boundp 'mevedel--session) mevedel--session)))
          (setf (mevedel-session-invoked-skills session)
                (append (mevedel-session-invoked-skills session)
                        (list invoked-skill)))))))))

(cl-defun mevedel-skills--invoke-inline
    (skill arguments callback &key trigger display-callback)
  "Inline-context invocation.

SKILL is the skill to run.  ARGUMENTS is the raw user/model argument
string.  CALLBACK receives the final outcome.  TRIGGER records the
invocation source.  DISPLAY-CALLBACK receives lifecycle events.

Preparation order matches the body-injection section:
  1. Load body
  2. Substitute variables
  3. Activate skill-scoped permission rules (so allowed-tools is
     in effect during body injection expansion)
  4. Expand shell/elisp injections
  5. Build invocation record
  6. Activate model/effort + record"
  (let* ((skill-name (mevedel-skill-name skill))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (body (mevedel-skill-load-body skill)))
    (cond
     ((null body)
      (mevedel-skills--invoke-error
       skill 'load-failure
       (format "Skill %s could not be loaded: %s"
               skill-name
               (or (mevedel-skill-source-file skill) "unknown source"))
       callback display-callback))
     (t
      (let* ((substituted (mevedel-skills--substitute-vars
                           body arguments session skill))
             (rules (mevedel-skill-allowed-tool-rules skill))
             (model (mevedel-skills--model-selector skill))
             (effort (mevedel-skill-effort skill))
             (hooks (mevedel-skill-hooks skill))
             (temporary-request-p nil))
        ;; Step 3: activate permission rules before body injection expansion.
        (mevedel-skills--activate-context
         trigger :permission-rules rules :hook-rules hooks)
        ;; Slash expansion happens before the real request exists.
        ;; Install a short-lived request so tool pipeline permission
        ;; checks can see this skill's allowed-tools while body
        ;; injections are being prepared.
        (when (and (eq trigger 'user-skill)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules
                       :hook-rules hooks)))
	(mevedel-skills--run-body-injections-async
	 substituted
	 (lambda (injection-outcome)
	   (pcase (plist-get injection-outcome :status)
	     ('ok
	      (mevedel-skills--run-expansion-hook
	       skill arguments (plist-get injection-outcome :body)
	       trigger session
	       (lambda (expanded decision)
                 (setq decision
                       (mevedel-skills--safe-hook-decision
                        'UserPromptExpansion decision))
	         (when temporary-request-p
	           (setq-local mevedel--current-request nil))
	         (if (and (plist-member decision :continue)
	                  (not (plist-get decision :continue)))
	             (progn
	               (when (eq trigger 'user-skill)
	                 (setq-local mevedel-skills--pending-request-context nil))
	               (mevedel-skills--invoke-error
	                skill 'hook-blocked
	                (or (plist-get decision :stop-reason)
	                    "UserPromptExpansion hook stopped skill")
	                callback display-callback))
                   (let* ((hook-audits
                           (delq nil
                                 (list
                                  (mevedel-skills--prompt-rewrite-audit-record
                                   (plist-get injection-outcome :body)
                                   decision))))
                          (record
                           (mevedel-skill-invocation-record--create
                            :name skill-name
                            :args arguments
                            :trigger trigger
                            :turn (and session
                                       (mevedel-session-turn-count session))
                            :source-path (mevedel-skill-source-file skill)
                            :prepared-body expanded))
                          (ctx (list :permission-rules rules
                                     :model model
                                     :effort effort
                                     :hook-rules hooks
                                     :invoked-skills (list record))))
                     ;; Rules already activated above; do not append them twice.
                     (mevedel-skills--activate-context
                      trigger :model model :effort effort
                      :invoked-skill record)
                     (mevedel-skills--invoke-done
                      skill
                      `(:status ok :kind inline
                                :body ,expanded
                                :arguments ,arguments
                                :hook-audits ,hook-audits
                                :request-context ,ctx)
                      callback display-callback))))))
	     (_
	      (when (eq trigger 'user-skill)
	        (setq-local mevedel-skills--pending-request-context nil))
	      (when temporary-request-p
	        (setq-local mevedel--current-request nil))
	      (mevedel-skills--invoke-error
	       skill
	       (plist-get injection-outcome :reason)
	       (plist-get injection-outcome :message)
	       callback display-callback))))))))))

(defun mevedel-skills--build-parent-inherited-agent (skill)
  "Build a synthetic `mevedel-agent' for SKILL with no `agent' field.

Captures the calling buffer's current gptel state at spawn time
and returns a `mevedel-agent' struct named `skill:<skill-name>'.
The agent inherits the parent's system prompt directly; tools are
inherited via the request-locals snapshot captured by
`mevedel-agent-exec--run' at dispatch time, which carries the
calling buffer's `gptel-tools' through to the spawned agent
buffer.

Side effect: the synthetic agent is also registered (or refreshed)
in the buffer-local `mevedel-agent-exec--agents' alist so the
spawn path can resolve it the same way it resolves named agents.
Registration is keyed on the `skill:<skill-name>' identifier."
  (let* ((skill-name (mevedel-skill-name skill))
         (agent-name (concat "skill:" skill-name))
         (parent-system (and (boundp 'gptel-system-prompt)
                             gptel-system-prompt))
         (agent
          (mevedel-agent--create
           :name agent-name
           :description (or (mevedel-skill-description skill)
                            (format "Parent-inherited fork of skill %s"
                                    skill-name))
           :tools nil
           :system-prompt (or parent-system "")
           :max-turns nil
           :reminders nil)))
    (let ((spec (mevedel-agent-to-gptel-spec agent))
          (existing (and (boundp 'mevedel-agent-exec--agents)
                         mevedel-agent-exec--agents)))
      (setq-local mevedel-agent-exec--agents
                  (cons spec
                        (cl-remove agent-name existing
                                   :key #'car :test #'equal)))
      ;; Some tests dynamically bind this special variable; keep the
      ;; dynamic binding in sync with the buffer-local value.
      (setq mevedel-agent-exec--agents
            (buffer-local-value 'mevedel-agent-exec--agents
                                (current-buffer))))
    agent))

(defun mevedel-skills--build-fork-agent (skill)
  "Return a `mevedel-agent' struct to use for SKILL's fork dispatch.

If SKILL declares an `agent' field, look it up in the registry
and return that agent.  Returns nil for unknown agent names so
the caller can produce an `unknown-agent' outcome.

If SKILL does not declare an `agent' field, build a synthetic
parent-inherited agent via
`mevedel-skills--build-parent-inherited-agent'.  The synthetic
agent's name is `skill:<skill-name>'; system prompt is
snapshotted from the calling buffer's `gptel-system-prompt';
tools propagate through the spawn path's request-locals capture."
  (let ((agent-name (mevedel-skill-agent skill)))
    (cond
     ((and (stringp agent-name) (not (string-empty-p agent-name)))
      (mevedel-agent-get agent-name))
     (t
      (mevedel-skills--build-parent-inherited-agent skill)))))

(cl-defun mevedel-skills--invoke-fork-direct
    (skill arguments callback &key trigger display-callback
           additional-context description on-invocation)
  "Direct fork dispatch via `mevedel-agent-runtime-dispatch'.  Async outcome.

SKILL is the skill to run.  ARGUMENTS is the raw user/model argument
string.  CALLBACK receives the final outcome.  TRIGGER records the
invocation source.  DISPLAY-CALLBACK receives lifecycle events.
ADDITIONAL-CONTEXT and DESCRIPTION are passed through to the spawned
agent task.  ON-INVOCATION is called with the agent invocation object.

Builds the target `mevedel-agent' via
`mevedel-skills--build-fork-agent', then dispatches the substituted
skill body as the agent's task prompt.  The agent runs foreground;
the parent FSM parks until the agent returns.

The outcome callback fires when the agent completes (potentially
much later than this function returns).  Suitable for callers
that already operate async (e.g., the `Skill' tool handler)."
  (let* ((skill-name (mevedel-skill-name skill))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (agent (mevedel-skills--build-fork-agent skill)))
    (if (null agent)
        ;; --build-fork-agent only returns nil for an unknown named
        ;; agent; the parent-inherited path always synthesizes a
        ;; struct.
        (mevedel-skills--invoke-error
         skill 'unknown-agent
         (format "Skill '%s' references unknown agent '%s'"
                 skill-name (mevedel-skill-agent skill))
         callback display-callback)
      (let* ((body (or (mevedel-skill-load-body skill) ""))
             (substituted (mevedel-skills--substitute-vars
                           body arguments session skill))
             (description (or description
                              (mevedel-skill-description skill)
                              skill-name))
             (rules (mevedel-skill-allowed-tool-rules skill))
             (model (mevedel-skills--model-selector skill))
             (effort (mevedel-skill-effort skill))
             (hooks (mevedel-skill-hooks skill))
             (temporary-request-p nil))
        (cond
         ((not (eq trigger 'user-skill))
          (mevedel-skills--activate-context
           trigger :permission-rules rules :hook-rules hooks))
         ((bound-and-true-p mevedel--current-request)
          (mevedel-skills--activate-request-context
           mevedel--current-request rules hooks)))
        (when (and (eq trigger 'user-skill)
                   (not (bound-and-true-p mevedel--current-request)))
          (setq temporary-request-p t)
          (setq-local mevedel--current-request
                      (mevedel-request--create
                       :session session
                       :file-snapshots (make-hash-table :test #'equal)
                       :skill-permission-rules rules
                       :hook-rules hooks)))
        (mevedel-skills--run-body-injections-async
         substituted
         (lambda (injection-outcome)
           (pcase (plist-get injection-outcome :status)
             ('ok
              (mevedel-skills--run-expansion-hook
               skill arguments (plist-get injection-outcome :body)
               trigger session
               (lambda (prepared decision)
                 (setq decision
                       (mevedel-skills--safe-hook-decision
                        'UserPromptExpansion decision))
                 (when temporary-request-p
                   (setq-local mevedel--current-request nil))
                 (if (and (plist-member decision :continue)
                          (not (plist-get decision :continue)))
                     (mevedel-skills--invoke-error
                      skill 'hook-blocked
                      (or (plist-get decision :stop-reason)
                          "UserPromptExpansion hook stopped skill")
                      callback display-callback)
                   (let* ((hook-audits
                           (delq nil
                                 (list
                                  (mevedel-skills--prompt-rewrite-audit-record
                                   (plist-get injection-outcome :body)
                                   decision))))
                          (prepared
                           (if (and (stringp additional-context)
                                    (not (string-empty-p additional-context)))
                               (concat prepared "\n\n" additional-context)
                             prepared))
                          (record
                           (mevedel-skill-invocation-record--create
                            :name skill-name
                            :args arguments
                            :trigger trigger
                            :turn (and session
                                       (mevedel-session-turn-count session))
                            :source-path (mevedel-skill-source-file skill)
                            :prepared-body prepared)))
                     (when session
                       (setf (mevedel-session-invoked-skills session)
                             (append (mevedel-session-invoked-skills session)
                                     (list record))))
                     (unless (fboundp 'mevedel-agent-runtime-dispatch)
                       (require 'mevedel-agent-runtime))
                     (condition-case err
                         (mevedel-agent-runtime-dispatch
                          (lambda (response)
                            ;; `mevedel-agent-runtime-dispatch' may deliver either a bare
                            ;; string or a plist with transcript render data.
                            (let* ((wrapped-p
                                    (and (listp response)
                                         (plist-member response :result)))
                                   (result-str (if wrapped-p
                                                   (plist-get response :result)
                                                 response))
                                   (render-data
                                    (and wrapped-p
                                         (plist-get response :render-data)))
                                   (transcript-agent-id
                                    (and wrapped-p
                                         (plist-get render-data :agent-id))))
                              (mevedel-skills--invoke-done
                               skill
                               `(:status ok :kind fork
                                         :result ,result-str
                                         :agent-id
                                         ,(or transcript-agent-id
                                              (mevedel-agent-name agent))
                                         :hook-audits ,hook-audits
                                         :render-data ,render-data)
                               callback display-callback)))
                          agent description prepared
                          :parent-context
                          (mevedel-tools--current-deferred-context)
                          :parent-fsm mevedel-tools--current-fsm
                          :message-handler #'mevedel-tools--handle-message-inject
                          :terminal-handler
                          #'mevedel-tools--handle-terminal-mailbox
                          :skill-permission-rules rules
                          :skill-model-override model
                          :skill-effort-override effort
                          :skill-hook-rules hooks
                          :on-invocation on-invocation)
                       (error
                        (mevedel-skills--invoke-error
                         skill 'agent-dispatch-failed
                         (error-message-string err)
                         callback display-callback))))))))
             (_
              (when temporary-request-p
                (setq-local mevedel--current-request nil))
              (mevedel-skills--invoke-error
               skill
               (plist-get injection-outcome :reason)
               (plist-get injection-outcome :message)
               callback display-callback)))))))))

(cl-defun mevedel-skills-invoke
    (skill arguments callback &key trigger display-callback
           additional-context description on-invocation skip-gates)
  "Invoke SKILL with ARGUMENTS through the unified skill API.

CALLBACK is invoked with a normalized invocation outcome plist:

  (:status ok    :kind inline :body BODY :request-context CTX)
  (:status ok    :kind fork   :result RESULT :agent-id ID
                  :render-data DATA)
  (:status error :reason REASON :message MESSAGE)

TRIGGER is `user-skill', `model-skill', or `internal' and
determines the blocking model implicitly: `user-skill' blocks
chat input; `model-skill' blocks the parent tool call.

DISPLAY-CALLBACK is an optional lifecycle event sink that
receives `agent-progress' (fork only), `done', and `error'
events.

ADDITIONAL-CONTEXT is appended to fork-skill agent prompts after body
injections have prepared the prompt.

DESCRIPTION overrides the task description for fork skills.
ON-INVOCATION is forwarded to `mevedel-agent-runtime-dispatch' for fork skills.
SKIP-GATES bypasses user-disabled/user-invocable/model-invocable gates
for first-class local commands that own their dispatch semantics.

Recursion depth is tracked via the dynamic let-bound
`mevedel-skills--invoke-depth'.
Crossing `mevedel-skills-max-recursion-depth' yields a
`recursion-limit-exceeded' error outcome.

Inline and fork contexts are callback-driven.  Inline invocation
calls CALLBACK with a prepared body; fork invocation dispatches a
foreground agent and calls CALLBACK when that agent returns."
  (let ((skill-name (and skill (mevedel-skill-name skill))))
    (cond
     ((not (mevedel-skill-p skill))
      (mevedel-skills--invoke-error
       skill 'unknown-skill
       "Invalid skill struct"
       callback display-callback))
     ;; Recursion guard.
     ((>= mevedel-skills--invoke-depth mevedel-skills-max-recursion-depth)
      (mevedel-skills--invoke-error
       skill 'recursion-limit-exceeded
       (format "Skill recursion limit exceeded (max %d) invoking '%s'"
               mevedel-skills-max-recursion-depth skill-name)
       callback display-callback))
     ;; User-disabled skill gating.
     ((and (not skip-gates)
           (not (mevedel-skills--skill-enabled-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (if (eq trigger 'user-skill)
           (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                   skill-name skill-name skill-name)
         (format "Skill '%s' is disabled" skill-name))
       callback display-callback))
     ;; User-slash gating.
     ((and (not skip-gates)
           (eq trigger 'user-skill)
           (not (mevedel-skill-user-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not user-invocable" skill-name)
       callback display-callback))
     ;; Model-side gating.
     ((and (not skip-gates)
           (eq trigger 'model-skill)
           (not (mevedel-skill-model-invocable-p skill)))
      (mevedel-skills--invoke-error
       skill 'disabled
       (format "Skill '%s' is not model-invocable" skill-name)
       callback display-callback))
     (t
      (let ((mevedel-skills--invoke-depth
             (1+ mevedel-skills--invoke-depth)))
        (pcase (mevedel-skill-context skill)
          ('inline
           (mevedel-skills--invoke-inline
            skill arguments callback
            :trigger trigger :display-callback display-callback))
          ('fork
           (mevedel-skills--invoke-fork-direct
            skill arguments callback
            :trigger trigger :display-callback display-callback
            :additional-context additional-context
            :description description
            :on-invocation on-invocation))
          (other
           (mevedel-skills--invoke-error
            skill 'unknown-skill
            (format "Skill '%s' has unsupported context: %S"
                    skill-name other)
            callback display-callback))))))))


;;
;;; Skill tool handler

(defun mevedel-skills--render-skill-tool (name args result _render-data)
  "Return rendering plist for NAME, ARGS, and RESULT from the Skill tool."
  (when (stringp result)
    (let* ((skill-name (or (plist-get args :name) "?"))
           (lines (length (split-string result "\n" t))))
      (list :header (format "%s: %s (%d %s)"
                            (or name "Skill")
                            skill-name
                            lines
                            (if (= lines 1) "line" "lines"))
            :body result
            :body-mode 'markdown-mode
            :status (and (string-prefix-p "Error:" result) 'error)
            :initially-collapsed-p t))))

(defun mevedel-skills--invoke-handler (callback args)
  "Pipeline handler for the `Skill' tool.

CALLBACK is the async tool callback.  ARGS is a plist with :name
and optional :arguments.

Routes through `mevedel-skills-invoke' with `model-skill' trigger
and projects the outcome plist to a tool-result string: success
returns the body; error returns a `Error: ' prefixed message."
  (let* ((name (plist-get args :name))
         (arguments (plist-get args :arguments))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (skill (and session (mevedel-session-get-skill session name)))
         (return (lambda (result)
                   (funcall callback (list :result result)))))
    (cond
     ((not (stringp name))
      (funcall return "Error: Skill name is required."))
     ((not session)
      (funcall return "Error: No active mevedel session."))
     ((not skill)
      (funcall return (format "Error: Unknown skill '%s'." name)))
     (t
      (mevedel-skills-invoke
       skill arguments
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (funcall return
                     (or (plist-get outcome :body)
                         (plist-get outcome :result)
                         (format "Skill '%s' produced no body." name))))
           ('error
            (funcall return
                     (format "Error: %s"
                             (or (plist-get outcome :message)
                                 "skill invocation failed"))))))
       :trigger 'model-skill)))))

(defconst mevedel-skills--list-tool-limit 25
  "Maximum entries returned by the ListSkills tool without narrowing.")

(defun mevedel-skills--model-visible-p (skill &optional active-only)
  "Return non-nil when SKILL may be shown to the model.
When ACTIVE-ONLY is non-nil, dormant path-scoped skills are excluded."
  (and (mevedel-skill-model-invocable-p skill)
       (mevedel-skills--skill-enabled-p skill)
       (or (not active-only)
           (mevedel-skill-active-p skill))))

(defun mevedel-skills--skill-matches-query-p (skill query)
  "Return non-nil when SKILL matches QUERY."
  (or (not (and (stringp query) (not (string-empty-p query))))
      (let ((case-fold-search t)
            (needle (regexp-quote query)))
        (cl-some
         (lambda (value)
           (and (stringp value) (string-match-p needle value)))
         (list (mevedel-skill-name skill)
               (mevedel-skill-display-name skill)
               (mevedel-skill-description skill))))))

(defun mevedel-skills--format-list-tool-result
    (skills omitted &optional mark-dormant)
  "Format SKILLS for the ListSkills tool, noting OMITTED entries."
  (if (null skills)
      "No model-invocable skills match."
    (let ((body
           (mapconcat (lambda (skill)
                        (mevedel-skills--listing-describe
                         skill
                         (and mark-dormant
                              (mevedel-skills--model-visible-p skill)
                              (mevedel-skill-path-patterns skill)
                              (not (mevedel-skill-active-p skill)))))
                      skills "\n")))
      (if (> omitted 0)
          (concat body
                  (format "\n\n%d more skill(s) omitted; use query to narrow."
                          omitted))
        body))))

(defun mevedel-skills--list-handler (callback args)
  "Pipeline handler for the `ListSkills' tool.
CALLBACK is the async tool callback.  ARGS is a plist with optional :query."
  (let* ((query (plist-get args :query))
         (session (and (boundp 'mevedel--session) mevedel--session))
         (return (lambda (result)
                   (funcall callback (list :result result)))))
    (cond
     ((not session)
      (funcall return "Error: No active mevedel session."))
     ((and query (not (stringp query)))
      (funcall return "Error: query must be a string."))
     (t
      (when (buffer-live-p (current-buffer))
        (mevedel-skills--ensure-fresh (current-buffer) session))
      (let* ((narrowed (and (stringp query)
                            (not (string-empty-p (string-trim query)))))
             (pool (if narrowed
                       (cl-remove-if-not
                        #'mevedel-skills--model-visible-p
                        (mevedel-session-skills session))
                     (mevedel-skills--listing-candidates session)))
             (matches
              (cl-remove-if-not
               (lambda (skill)
                 (mevedel-skills--skill-matches-query-p skill query))
               pool))
             (shown (cl-subseq matches 0
                                (min (length matches)
                                     mevedel-skills--list-tool-limit)))
             (omitted (max 0 (- (length matches) (length shown)))))
        (funcall return
                 (mevedel-skills--format-list-tool-result
                  shown omitted narrowed)))))))


;;
;;; User skill dispatch

(defun mevedel-skills--current-prompt-region ()
  "Return (START . END) of the pending prompt text in the chat buffer.

Locates the start of the current user prompt by preferring gptel's
`gptel' text property -- which marks prior LLM responses even when the
user has disabled the prompt prefix -- and falling back to the last
occurrence of the configured prompt prefix.  If neither boundary is
present the whole buffer is treated as the pending prompt.  END is
always `point-max'.  Returns nil only for an empty buffer."
  (save-excursion
    (goto-char (point-max))
    (let* ((match (text-property-search-backward 'gptel nil nil t))
           (prefix (alist-get major-mode gptel-prompt-prefix-alist))
           (has-prefix (and prefix (not (string-empty-p prefix))))
           start)
      (cond
       (match (setq start (prop-match-end match)))
       ((and has-prefix
             (progn (goto-char (point-max))
                    (search-backward prefix nil t)))
        (setq start (+ (point) (length prefix))))
       ((< (point-min) (point-max))
        (setq start (point-min))))
      (when (and start has-prefix)
        (save-excursion
          (goto-char start)
          (when (looking-at-p (regexp-quote prefix))
            (setq start (+ start (length prefix))))))
      (when start
        (cons start (point-max))))))


(defun mevedel-skills--parse-prefixed-line (text prefix)
  "Parse TEXT for a leading PREFIX command line.
Returns (NAME ARGS OFFSET) when TEXT starts (after optional leading
whitespace) with PREFIX followed by an identifier, or nil otherwise.
NAME is the command name; ARGS is the rest of TEXT after the
command name (the remainder of the first line plus every
subsequent line, joined and trimmed); OFFSET is the 0-based
character position of PREFIX within TEXT."
  (let* ((trimmed (string-trim-left text))
         (offset (- (length text) (length trimmed))))
    (when (and (> (length trimmed) 1) (eq (aref trimmed 0) prefix))
      (let* ((line-end (or (string-match "\n" trimmed) (length trimmed)))
             (line (substring trimmed 1 line-end))
             (rest (substring trimmed line-end))
             (space (string-match "[ \t]" line))
             (name (if space (substring line 0 space) line))
             (first-line-args (if space (substring line space) ""))
             (args (string-trim (concat first-line-args rest))))
        (when (string-match-p "\\`[A-Za-z0-9_.:-]+\\'" name)
          (list name args offset))))))

(defun mevedel-skills--parse-skill-line (text)
  "Parse TEXT for a leading `$skill [args]' line.

User skill invocations take the user's prompt body as ARGS, and prompt
bodies are naturally multi-line."
  (mevedel-skills--parse-prefixed-line text ?$))

(defun mevedel-skills--escaped-position-p (text pos)
  "Return non-nil when POS in TEXT is escaped by an odd backslash run."
  (let ((i (1- pos))
        (count 0))
    (while (and (>= i 0)
                (eq (aref text i) ?\\))
      (cl-incf count)
      (cl-decf i))
    (cl-oddp count)))

(defun mevedel-skills--inline-skill-boundary-p (text pos)
  "Return non-nil when a `$skill' mention may start at POS in TEXT."
  (or (= pos 0)
      (not (string-match-p
            "[[:alnum:]_]"
            (char-to-string (aref text (1- pos)))))))

(defun mevedel-skills--quote-delimiter-p (text pos)
  "Return non-nil when POS is a quote delimiter in TEXT."
  (and (not (mevedel-skills--escaped-position-p text pos))
       (let ((quote (aref text pos)))
         (and (memq quote '(?\" ?\'))
              (or (eq quote ?\")
                  (not (and (mevedel-skills--word-char-p
                             (and (> pos 0) (aref text (1- pos))))
                            (mevedel-skills--word-char-p
                             (and (< (1+ pos) (length text))
                                  (aref text (1+ pos)))))))))))

(defun mevedel-skills--quoted-inline-skill-p (text start end)
  "Return non-nil when TEXT START..END is inside single/double quotes."
  (let ((line-start (or (and (> start 0)
                             (string-match-p "\n"
                                             (substring text 0 start))
                             (1+ (cl-position ?\n text
                                               :end start
                                               :from-end t)))
                        0))
        (line-end (or (cl-position ?\n text :start end)
                      (length text)))
        quoted)
    (dolist (quote '(?\" ?\') quoted)
      (let ((open nil)
            (pos (1- start)))
        (while (and (not open) (>= pos line-start))
          (when (and (eq (aref text pos) quote)
                     (mevedel-skills--quote-delimiter-p text pos))
            (setq open pos))
          (cl-decf pos))
        (when open
          (setq pos end)
          (while (and (not quoted) (< pos line-end))
            (when (and (eq (aref text pos) quote)
                       (mevedel-skills--quote-delimiter-p text pos))
              (setq quoted t))
            (cl-incf pos)))))))

(defun mevedel-skills--skill-token-live-p
    (text start end first-nonspace code-ranges &optional allow-root)
  "Return non-nil when TEXT START..END is a live `$skill' token.
When ALLOW-ROOT is nil, exclude the leading root skill invocation."
  (and (or allow-root (not (= start first-nonspace)))
       (mevedel-skills--inline-skill-boundary-p text start)
       (not (mevedel-skills--escaped-position-p text start))
       (not (mevedel-skills--quoted-inline-skill-p text start end))
       (not (mevedel-skills--ranges-overlap-p
             code-ranges start end))))

(defun mevedel-skills--resolve-inline-skill-name (raw-name lookup)
  "Resolve RAW-NAME with LOOKUP, trimming trailing punctuation if needed.
Return (NAME . VALUE), preferring an exact match.  Only punctuation
that is also allowed in skill names is trimmed, so `$foobar' does not
fall back to `$foo'."
  (let ((name raw-name)
        found)
    (while (and (not found) (> (length name) 0))
      (when-let* ((value (funcall lookup name)))
        (setq found (cons name value)))
      (unless found
        (let ((last (aref name (1- (length name)))))
          (if (memq last '(?. ?: ?-))
              (setq name (substring name 0 -1))
            (setq name "")))))
    found))

(defun mevedel-skills--scan-skill-tokens (text lookup &optional allow-root)
  "Return live `$skill' tokens in TEXT resolved through LOOKUP.
Each token has `:start', `:end', `:name', and `:value'.  When
ALLOW-ROOT is non-nil, include the leading root invocation."
  (let ((pos 0)
        (first-nonspace (or (string-match-p "\\S-" text) -1))
        (code-ranges (mevedel-skills--markdown-code-ranges text))
        tokens)
    (while (string-match "\\$\\([A-Za-z0-9_.:-]+\\)" text pos)
      (let* ((start (match-beginning 0))
             (resolved
              (mevedel-skills--resolve-inline-skill-name
               (match-string 1 text) lookup))
             (name (car-safe resolved))
             (end (and name (+ start 1 (length name)))))
        (setq pos (match-end 0))
        (when (and resolved
                   (mevedel-skills--skill-token-live-p
                    text start end first-nonspace code-ranges allow-root))
          (push (list :start start :end end :name name
                      :value (cdr resolved))
                tokens))))
    (nreverse tokens)))

(defun mevedel-skills--inline-skill-mentions (text session)
  "Return live inline `$skill' mentions in TEXT for SESSION.

Unknown `$foo' text is ignored.  Known but unavailable skills return
a single plist with :error and :message so callers can block the send."
  (let ((tokens
         (and session
              (mevedel-skills--scan-skill-tokens
               text
               (lambda (candidate)
                 (mevedel-session-get-skill session candidate)))))
        (seen (make-hash-table :test #'equal))
        mentions
        problem)
    (while (and tokens (not problem))
      (let* ((token (pop tokens))
             (start (plist-get token :start))
             (end (plist-get token :end))
             (name (plist-get token :name))
             (skill (plist-get token :value)))
        (cond
           ((not (mevedel-skills--skill-enabled-p skill))
            (setq problem
                  (list :error 'disabled
                        :message
                        (format "Skill $%s is disabled. Enable it with /skills enable %s or escape it as \\$%s."
                                name name name))))
           ((not (mevedel-skill-user-invocable-p skill))
            (setq problem
                  (list :error 'not-user-invocable
                        :message
                        (format "Skill $%s is not user-invocable. Escape it as \\$%s to send it literally."
                                name name))))
           ((eq (mevedel-skill-context skill) 'fork)
            (setq problem
                  (list :error 'fork-inline
                        :message
                        (format "Fork skill $%s must be invoked at the start of the prompt. Use $%s ..., or escape, quote, or code-span it to send literal text."
                                name name))))
           ((not (gethash (mevedel-skill-name skill) seen))
            (puthash (mevedel-skill-name skill) t seen)
            (push (list :name (mevedel-skill-name skill)
                        :start start
                        :end end
                        :skill skill)
                  mentions)))))
    (or problem (nreverse mentions))))

(defun mevedel-skills--ensure-fresh-line ()
  "Leave point at the start of an empty line with a blank line above.
Called after deleting a slash-command region so the next insertion or
cursor rest position is visually separated from the preceding response.
Skipped when the slash command was preceded solely by the prompt prefix,
since in that case the prefix should remain on its own line and the body
should inline with it."
  (unless (bolp) (insert "\n"))
  (unless (save-excursion
            (forward-line -1)
              (looking-at-p "^[ \t]*$"))
    (insert "\n")))

(defun mevedel-skills--insert-fork-result (outcome)
  "Insert fork skill OUTCOME as an assistant response in the data buffer.

The current buffer must be the data buffer.  This path is used when a
fork skill suppresses the main `gptel-send'; it records the
foreground agent's final result as the assistant side of that turn and
runs the normal post-response hooks so the view and persistence layers
observe the completed response."
  (require 'mevedel-utilities)
  (require 'mevedel-transcript)
  (let* ((render-data (plist-get outcome :render-data))
         (hook-audits (plist-get outcome :hook-audits))
         (result (or (plist-get outcome :result)
                     "Fork skill produced no result."))
         (result (if render-data
                     (progn
                       (require 'mevedel-pipeline)
                       (concat result
                               (mevedel-pipeline--format-render-data-block
                                render-data)))
                   result)))
    (unless (bound-and-true-p mevedel--current-request)
      (when (bound-and-true-p mevedel--session)
        (mevedel-request-begin mevedel--session
                               (and (boundp 'mevedel--current-directive-uuid)
                                    mevedel--current-directive-uuid))))
    (goto-char (point-max))
    (when-let* ((synthetic (plist-get outcome :synthetic-user-message)))
      (let ((user-turn-start (point)))
        (unless (bolp) (insert "\n"))
        (insert synthetic)
        (unless (bolp) (insert "\n"))
        (mevedel--clear-user-turn-gptel-properties
         user-turn-start (point))))
    (when hook-audits
      (insert (mapconcat #'mevedel--format-hook-audit-record
                         hook-audits "")))
    (unless (bolp) (insert "\n"))
    (insert gptel-response-separator)
    (let ((start (point)))
      (insert result)
      (unless (eq (char-before) ?\n)
        (insert "\n"))
      (let ((end (point)))
        (add-text-properties start end '(gptel response))
        (mevedel-transcript-restore-ignored-properties start end)
        (condition-case err
            (run-hook-with-args 'gptel-post-response-functions start end)
          (error
           (display-warning
            'mevedel
            (format "Fork post-response hook failed: %s"
                    (error-message-string err))
            :warning)))
        (mevedel-transcript-restore-ignored-properties start end)
        (mevedel--complete-turn
         (gptel-make-fsm :info (list :buffer (current-buffer))))
        (gptel--update-status " Ready" 'success)))))

(defun mevedel-skills--command-delete-context (command-pos)
  "Return deletion context for a command starting at COMMAND-POS.
The result is a plist with :delete-start and :after-prefix."
  (let* ((prefix (alist-get major-mode gptel-prompt-prefix-alist))
         (has-prefix (and prefix (not (string-empty-p prefix))))
         (line-start (save-excursion
                       (goto-char command-pos)
                       (line-beginning-position)))
         (before-command
          (buffer-substring-no-properties line-start command-pos))
         (after-prefix (and has-prefix (equal before-command prefix)))
         (delete-start
          (cond
           (after-prefix command-pos)
           ((string-match-p "\\`[ \t]*\\'" before-command) line-start)
           (t command-pos))))
    (list :delete-start delete-start :after-prefix after-prefix)))

(defun mevedel-skills--handle-user-skill-outcome
    (skill outcome delete-start region-end after-prefix continue-fn)
  "Apply user skill OUTCOME in the current data buffer.

SKILL is the invoked user skill.  DELETE-START and REGION-END bound
the original `$skill' text.  AFTER-PREFIX means the command followed
the prompt prefix.

CONTINUE-FN, when non-nil, resumes the original `gptel-send' after an
inline body has been inserted.  Fork outcomes suppress that send and
insert their result when the foreground agent finishes."
  (pcase (plist-get outcome :status)
    ('ok
     (pcase (plist-get outcome :kind)
       ('inline
        (delete-region delete-start region-end)
        (unless after-prefix
          (mevedel-skills--ensure-fresh-line))
        (let ((body (or (plist-get outcome :body)
                        (format "Skill '%s' produced no body."
                                (mevedel-skill-name skill)))))
          (insert body)
          (mevedel-skills--insert-inline-user-skill-render-data
           skill (plist-get outcome :arguments)))
        (when continue-fn
          (funcall continue-fn))
        'skill)
       ('fork
        (message "Skill '%s' dispatched; waiting for agent result..."
                 (mevedel-skill-name skill))
        (mevedel-skills--insert-fork-result outcome)
        'skill)
       (_
        (message "Skill '%s' returned an unsupported outcome: %S"
                 (mevedel-skill-name skill) outcome)
        'unknown)))
    (_
     (message "Skill '%s' failed: %s"
              (mevedel-skill-name skill)
              (plist-get outcome :message))
     'unknown)))
(defun mevedel-skills--dispatch-skill-command (&optional continue-fn)
  "Parse and dispatch a `$skill' invocation in the current chat buffer.

Returns:
- `skill'   a skill was expanded into the prompt region; caller
            should proceed with the send if CONTINUE-FN was nil.
- `unknown' a named skill existed but could not be invoked; caller
            should abort the send.
- nil       no known `$skill' present; caller should proceed as usual."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (text (buffer-substring-no-properties (car region) (cdr region)))
              (parsed (mevedel-skills--parse-skill-line text)))
    (let* ((name (nth 0 parsed))
           (args (nth 1 parsed))
           (skill-pos (+ (car region) (nth 2 parsed)))
           (skill (and mevedel--session
                       (mevedel-session-get-skill mevedel--session name))))
      (when skill
        (let* ((delete-context
                (mevedel-skills--command-delete-context skill-pos))
               (delete-start (plist-get delete-context :delete-start))
               (after-prefix (plist-get delete-context :after-prefix))
               (buffer (current-buffer))
               (region-end (cdr region))
               (dispatch-result 'skill))
          (mevedel-skills-invoke
           skill args
           (lambda (outcome)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq dispatch-result
                       (mevedel-skills--handle-user-skill-outcome
                        skill outcome delete-start region-end
                        after-prefix continue-fn)))))
           :trigger 'user-skill)
          dispatch-result)))))

(defun mevedel-skills--prepare-inline-attachments
    (mentions callback &optional prepared)
  "Prepare inline skill MENTIONS, then call CALLBACK.
CALLBACK receives (:status ok :attachments LIST) or an error plist.
PREPARED is the accumulator used for sequential async preparation."
  (if (null mentions)
      (funcall callback
               (list :status 'ok :attachments (nreverse prepared)))
    (let* ((mention (car mentions))
           (skill (plist-get mention :skill))
           (name (plist-get mention :name)))
      (mevedel-skills-invoke
       skill ""
       (lambda (outcome)
         (pcase (plist-get outcome :status)
           ('ok
            (if (eq (plist-get outcome :kind) 'inline)
                (mevedel-skills--prepare-inline-attachments
                 (cdr mentions) callback
                 (cons (list :name name
                             :skill skill
                             :arguments ""
                             :body (plist-get outcome :body))
                       prepared))
              (funcall callback
                       (list :status 'error
                             :reason 'unsupported-context
                             :message
                             (format "Skill $%s cannot be attached inline."
                                     name)))))
           (_
            (funcall callback outcome))))
       :trigger 'user-skill))))

(defun mevedel-skills--prepare-inline-attachments-for-text
    (text session callback)
  "Prepare inline `$skill' attachments in TEXT for SESSION.
CALLBACK receives the prepared outcome.  Return `skill' when async
preparation took ownership, `unknown' when a known skill blocks the
send, or nil when TEXT has no inline attachments."
  (when-let* ((mentions (mevedel-skills--inline-skill-mentions
                         text session)))
    (cond
     ((plist-get mentions :error)
      (message "%s" (plist-get mentions :message))
      'unknown)
     (t
      (mevedel-skills--prepare-inline-attachments mentions callback)
      'skill))))

(defun mevedel-skills--stage-inline-attachments (attachments)
  "Store prepared inline ATTACHMENTS and return their render-data block."
  (setq-local mevedel-skills--pending-inline-attachments attachments)
  (mevedel-skills-format-inline-attachment-render-data attachments))

(defun mevedel-skills--clear-pending-inline-attachments ()
  "Clear pending inline attachment request state in the current buffer."
  (setq-local mevedel-skills--pending-request-context nil)
  (setq-local mevedel-skills--pending-inline-attachments nil))

(defun mevedel-skills--dispatch-inline-attachments (&optional continue-fn)
  "Prepare inline `$skill' attachments in the current prompt.
Return `skill' when preparation took ownership of continuing the send,
`unknown' when a known skill blocks the send, or nil when no inline
attachments exist."
  (when-let* ((region (mevedel-skills--current-prompt-region))
              (session (and (bound-and-true-p mevedel--session)
                            mevedel--session))
              ((not (bound-and-true-p
                     mevedel-skills--pending-inline-attachments)))
              (text (buffer-substring-no-properties (car region) (cdr region))))
    (let ((buffer (current-buffer)))
      (mevedel-skills--prepare-inline-attachments-for-text
       text session
       (lambda (outcome)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (pcase (plist-get outcome :status)
               ('ok
                (goto-char (point-max))
                (insert
                 (mevedel-skills--stage-inline-attachments
                  (plist-get outcome :attachments)))
                (when continue-fn
                  (funcall continue-fn)))
               (_
                (mevedel-skills--clear-pending-inline-attachments)
                (message "Inline skill failed: %s"
                         (or (plist-get outcome :message)
                             "unknown error")))))))))))


;;
;;; Shared model listing primitives

(defcustom mevedel-skills-listing-max-entry-chars 1536
  "Maximum characters per skill entry in the model-facing skills roster.

Entries longer than this are truncated with an ellipsis so a single
verbose description cannot starve the rest of the listing.  The
default cap is 1,536 chars."
  :type 'integer
  :group 'mevedel)

(defconst mevedel-skills--source-priority
  '((project . mevedel)
    (project . agents)
    (user . mevedel)
    (user . agents)
    bundled
    managed
    plugin)
  "Source priority for model-facing skills roster ordering.")

(defun mevedel-skills--source-priority-key (skill)
  "Return ordering key for SKILL in model-facing rosters."
  (or (cl-position
       (cond
        ((and (mevedel-skills--ordinary-skill-p skill)
              (mevedel-skill-source-family skill))
         (cons (mevedel-skill-source skill)
               (mevedel-skill-source-family skill)))
        ((eq (mevedel-skill-source skill) 'project)
         '(project . mevedel))
        ((eq (mevedel-skill-source skill) 'user)
         '(user . mevedel))
        (t
         (mevedel-skill-source skill)))
       mevedel-skills--source-priority
       :test #'equal)
      most-positive-fixnum))

(defun mevedel-skills--truncate-text (text limit)
  "Return TEXT truncated to LIMIT characters, using `...' when possible."
  (let ((text (or text "")))
    (cond
     ((<= limit 0) "")
     ((<= (length text) limit) text)
     ((<= limit 3) (substring text 0 limit))
     (t (concat (substring text 0 (- limit 3)) "...")))))

(defun mevedel-skills--entry-base (skill &optional dormant)
  "Return the roster line prefix for SKILL.
When DORMANT is non-nil, mark the skill as dormant path-scoped."
  (format "- %s%s:"
          (mevedel-skill-name skill)
          (if dormant " [dormant path-scoped]" "")))

(defun mevedel-skills--entry-description (skill &optional dormant)
  "Return SKILL's description capped for a single roster entry."
  (let* ((base (mevedel-skills--entry-base skill dormant))
         (limit (max 0 (- mevedel-skills-listing-max-entry-chars
                          (length base)
                          1))))
    (mevedel-skills--truncate-text
     (or (mevedel-skill-description skill) "")
     limit)))

(defun mevedel-skills--listing-describe (skill &optional dormant)
  "Return a one-line entry for SKILL.

Format:
  - name: description

`mevedel-skills-listing-max-entry-chars' (1,536 by default) caps entries by
truncation with an ellipsis so a single verbose skill cannot starve
the rest of the listing.  When DORMANT is non-nil, mark the skill as
dormant path-scoped for `ListSkills(query)' output."
  (concat (mevedel-skills--entry-base skill dormant)
          " "
          (mevedel-skills--entry-description skill dormant)))

(defun mevedel-skills--listing-candidates (session)
  "Return SESSION's model-invocable, currently active skills.

Sorted by configured resource precedence so budget pressure drops
global/bundled/plugin entries before local resource entries."
  (let ((candidates
         (cl-remove-if-not
          (lambda (s)
            (mevedel-skills--model-visible-p s t))
          (mevedel-session-skills session))))
    (cl-sort (copy-sequence candidates)
             (lambda (a b)
               (< (mevedel-skills--source-priority-key a)
                  (mevedel-skills--source-priority-key b))))))


(provide 'mevedel-skills-invoke)
;;; mevedel-skills-invoke.el ends here
