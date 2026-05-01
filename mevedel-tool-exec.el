;;; mevedel-tool-exec.el -- Bash and Eval tools -*- lexical-binding: t -*-

;;; Commentary:

;; Bash command execution with permission system and Eval tool for elisp
;; evaluation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-permissions'
(declare-function mevedel-permission--collect-buckets
                  "mevedel-permissions"
                  (invocation-rules request-rules
                                    session-rules persistent-rules))
(declare-function mevedel-permission--any-deny "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--first-non-nil-action
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name skip-keys))
(declare-function mevedel-permission--rules-action "mevedel-permissions"
                  (rules tool-name &rest keys))
(declare-function mevedel-permission--load-persistent-rules "mevedel-permissions"
                  (workspace))
(declare-function mevedel-permission--plan-mode-skip-keys
                  "mevedel-permissions" (mode read-only-p))
(defvar mevedel-permission-rules)
(defvar mevedel-permission-mode)

;; `mevedel-structs'
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-tool-ui'
(declare-function mevedel-permission--enqueue "mevedel-permission-queue" (entry))
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions" t t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)
(declare-function mevedel-permission--build-attribution-line
                  "mevedel-tool-ui" (origin))
(declare-function mevedel-permission--prompt-async-eval
                  "mevedel-tool-ui" (content cont &optional count entry))

;; `mevedel-view'
(declare-function mevedel-view-collapse-by-height-p "mevedel-view" (body))


;;
;;; Permission queue helpers

(defun mevedel-tool-exec--current-origin ()
  "Return the queue entry origin for the current call site.
Resolves the canonical agent-id by reading the buffer-local
`mevedel--agent-invocation' (set in sub-agent buffers at
allocation time); falls back to \"main\" for main-thread
dispatches.  Used to populate the queue entry's `:origin' so
per-agent terminal sweep can match entries owned by an agent
that has unwound."
  (or (and-let* ((inv (and (boundp 'mevedel--agent-invocation)
                           mevedel--agent-invocation))
                 ((mevedel-agent-invocation-p inv)))
        (mevedel-agent-invocation-agent-id inv))
      "main"))

(defun mevedel-tool-exec--dangerous-command-p (command)
  "Return non-nil if COMMAND parses to any binary in
`mevedel-bash-dangerous-commands'.
Used by the queue entry's `:dangerous' flag so the eventual Bash
render-head can warn prominently."
  (when-let* ((extraction (mevedel-tools--extract-commands command))
              (commands (car extraction)))
    (cl-some (lambda (cmd) (member cmd mevedel-bash-dangerous-commands))
             commands)))


;;
;;; Command Execution

(defcustom mevedel-bash-dangerous-commands
  '("rm" "sudo" "dd" "mkfs" "fdisk" "parted"
    "chmod" "chown" "chgrp" "chattr"
    "kill" "pkill" "killall"
    "curl" "wget" "nc" "ncat" "telnet"
    "ssh" "scp" "rsync" "sftp"
    "iptables" "systemctl" "service"
    "reboot" "shutdown" "poweroff" "halt")
  "Commands that always require explicit confirmation.
Even if a rule in `mevedel-permission-rules' would allow these
commands, they will still trigger a confirmation prompt due to their
potential for system modification, data loss, or external network
access."
  :type '(repeat string)
  :group 'mevedel)

(defcustom mevedel-bash-fail-safe-on-complex-syntax t
  "When non-nil, always ask for permission when complex syntax is detected.
Complex syntax includes: variable expansion ($VAR, ${VAR}), eval, exec,
nested quotes, here-documents, and other constructs that cannot be
reliably parsed.

When nil, the system will attempt to extract commands from complex
syntax, which may miss dangerous commands hidden in variable expansions
or other dynamic constructs.

Recommended value: t (fail-safe behavior)."
  :type 'boolean
  :group 'mevedel)

(defun mevedel-tools--quotes-balanced-p (str)
  "Return t if quotes in STR are properly balanced, nil otherwise.
Handles single quotes, double quotes, and backslash escaping."
  (let ((in-single nil)
        (in-double nil)
        (escaped nil)
        (i 0)
        (len (length str)))
    (catch 'unbalanced
      (while (< i len)
        (let ((c (aref str i)))
          (cond
           ;; Handle escape sequences
           (escaped
            (setq escaped nil))

           ;; Backslash starts escape
           ((eq c ?\\)
            (setq escaped t))

           ;; Single quote toggle (only outside double quotes)
           ((and (eq c ?') (not in-double))
            (setq in-single (not in-single)))

           ;; Double quote toggle (only outside single quotes)
           ((and (eq c ?\") (not in-single))
            (setq in-double (not in-double))))
          (setq i (1+ i))))

      ;; Quotes are balanced if we're not currently inside any quotes
      ;; and not in an escaped state
      (and (not in-single) (not in-double) (not escaped)))))

(defun mevedel-tools--contains-complex-syntax-p (str)
  "Return t if STR's syntax is too complex to parse safely, nil otherwise.

Complex syntax includes:
- Variable expansion: $VAR, ${VAR}
- Eval or exec commands
- Here documents
- Brace expansion
- Unbalanced quotes"
  (or
   ;; Variable expansion (but not command substitution which we handle)
   (and (string-match-p "\\$[{A-Za-z_]" str) t)

   ;; Eval or exec
   (and (string-match-p "\\b\\(eval\\|exec\\)\\b" str) t)

   ;; Here documents
   (and (string-match-p "<<-?\\s-*['\"]?\\w" str) t)

   ;; Brace expansion that could hide commands
   (and (string-match-p "{[^}]*,[^}]*}" str) t)

   ;; Unmatched quotes
   (not (mevedel-tools--quotes-balanced-p str))))

(defun mevedel-tools--split-command-chain (str)
  "Split STR on command separators, respecting quotes.
Handles: && || ; | and newlines.
Returns list of command segments."
  (let ((result '())
        (current "")
        (in-single nil)
        (in-double nil)
        (escaped nil)
        (i 0)
        (len (length str)))
    (while (< i len)
      (let ((c (aref str i))
            (next (when (< (1+ i) len) (aref str (1+ i)))))
        (cond
         ;; Handle escape sequences
         (escaped
          (setq current (concat current (char-to-string c)))
          (setq escaped nil))

         ;; Backslash starts escape
         ((eq c ?\\)
          (setq current (concat current (char-to-string c)))
          (setq escaped t))

         ;; Single quote toggle (only outside double quotes)
         ((and (eq c ?') (not in-double))
          (setq current (concat current (char-to-string c)))
          (setq in-single (not in-single)))

         ;; Double quote toggle (only outside single quotes)
         ((and (eq c ?\") (not in-single))
          (setq current (concat current (char-to-string c)))
          (setq in-double (not in-double)))

         ;; Handle separators outside quotes
         ((and (not in-single) (not in-double))
          (cond
           ;; && separator
           ((and (eq c ?&) (eq next ?&))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current "")
            (setq i (1+ i))) ; skip next &

           ;; || separator
           ((and (eq c ?|) (eq next ?|))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current "")
            (setq i (1+ i))) ; skip next |

           ;; Single | (pipe)
           ((and (eq c ?|) (not (eq next ?|)))
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; ; separator
           ((eq c ?\;)
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; Newline separator
           ((eq c ?\n)
            (when (> (length (string-trim current)) 0)
              (push (string-trim current) result))
            (setq current ""))

           ;; Regular character
           (t (setq current (concat current (char-to-string c))))))

         ;; Inside quotes - accumulate
         (t (setq current (concat current (char-to-string c)))))
        (setq i (1+ i))))

    ;; Add final segment
    (when (> (length (string-trim current)) 0)
      (push (string-trim current) result))

    (nreverse result)))

(defun mevedel-tools--extract-substitutions (str)
  "Extract command substitutions from STR: $(...) and `...`.
Returns list of substitution contents.
Handles nested $(...)."
  (let ((result '()))
    ;; Extract $(...)
    (let ((pos 0))
      (while (string-match "\\$(" str pos)
        (let ((start (match-end 0))
              (depth 1)
              (i (match-end 0)))
          (while (and (< i (length str)) (> depth 0))
            (let ((c (aref str i)))
              (cond
               ((eq c ?\() (setq depth (1+ depth)))
               ((eq c ?\)) (setq depth (1- depth))))
              (setq i (1+ i))))
          (when (= depth 0)
            (push (substring str start (1- i)) result)
            (setq pos i)))))

    ;; Extract `...` (backticks)
    (let ((pos 0))
      (while (string-match "`\\([^`]*\\)`" str pos)
        (push (match-string 1 str) result)
        (setq pos (match-end 0))))

    (nreverse result)))

(defun mevedel-tools--extract-command-name (segment)
  "Extract the command name from SEGMENT.
Handles prefixes like sudo, env, and paths like /bin/cmd.
Returns command name string or nil."
  (when (and segment (> (length segment) 0))
    (condition-case nil
        (let* ((words (split-string-and-unquote segment))
               ;; Skip variable assignments (VAR=value)
               (words (seq-drop-while
                       (lambda (w)
                         (string-match-p "^[A-Za-z_][A-Za-z0-9_]*=" w))
                       words))
               (first-word (car words)))
          (when first-word
            (cond
             ;; sudo, doas, su - return the actual command after the prefix
             ((member first-word '("sudo" "doas" "su"))
              (or (cadr words) first-word))

             ;; nice with optional -n flag
             ((string-equal first-word "nice")
              (let ((rest (cdr words)))
                (or (if (and rest (string-equal (car rest) "-n"))
                        (nth 3 words)  ; skip nice, -n, and value (4th element)
                      (cadr words))    ; just skip nice (2nd element)
                    "nice")))

             ;; timeout - skip timeout and its duration argument
             ((string-equal first-word "timeout")
              (or (caddr words) "timeout"))  ; skip timeout and duration

             ;; nohup, time - next word is the actual command
             ((member first-word '("nohup" "time"))
              (or (cadr words) first-word))

             ;; env with args - find first non-assignment
             ((string-equal first-word "env")
              (or (car (seq-drop-while
                        (lambda (w) (string-match-p "=" w))
                        (cdr words)))
                  "env"))

             ;; Absolute/relative path - extract basename
             ((string-match-p "/" first-word)
              (file-name-nondirectory first-word))

             ;; Regular command
             (t first-word))))
      (error nil))))

(defun mevedel-tools--extract-commands (command-string)
  "Extract all command names from COMMAND-STRING.
Returns (COMMANDS . UNPARSEABLE) where:
- COMMANDS is a list of extracted command names
- UNPARSEABLE is t if complex syntax was detected, nil otherwise."
  (let ((commands '())
        (unparseable nil))

    ;; Check for complex syntax and mark it, but continue extraction
    ;; The check-bash-permission function will decide what to do based on fail-safe setting
    (when (mevedel-tools--contains-complex-syntax-p command-string)
      (setq unparseable t))

    ;; Split on command separators
    (dolist (segment (mevedel-tools--split-command-chain command-string))
      (let ((segment-commands '())
            (words (condition-case nil
                       (split-string-and-unquote segment)
                     (error nil))))

        ;; Build commands in correct order: sudo (if present), main cmd, substitutions

        ;; 1. Check if segment starts with sudo/doas/su - add the prefix
        (when (and words (member (car words) '("sudo" "doas" "su")))
          (setq segment-commands (append segment-commands (list (car words)))))

        ;; 2. Extract and add the main command name
        (when-let ((cmd (mevedel-tools--extract-command-name segment)))
          (setq segment-commands (append segment-commands (list cmd))))

        ;; 3. Extract and recursively process command substitutions
        (dolist (subst (mevedel-tools--extract-substitutions segment))
          (let ((sub-result (mevedel-tools--extract-commands subst)))
            (setq segment-commands (append segment-commands (car sub-result)))
            (when (cdr sub-result)
              (setq unparseable t))))

        ;; Add all segment commands to main commands list
        (setq commands (append commands segment-commands))))

    (cons commands unparseable)))

(defun mevedel-tools--bash-buckets ()
  "Return the bucket alist visible to Bash, innermost-first.

Includes the request-scoped skill rule buckets so a skill's
`allowed-tools: [Bash(...)]' grants are honored by the Bash
permission check; without this, skill rules silently failed for
the Bash tool path because Bash had its own flattened resolver."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (cond
                     (session (mevedel-session-workspace session))
                     ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (request (and (boundp 'mevedel--current-request)
                       mevedel--current-request))
         (invocation (and (boundp 'mevedel--agent-invocation)
                          mevedel--agent-invocation))
         (invocation-rules
          (and invocation
               (mevedel-agent-invocation-skill-permission-rules invocation)))
         (request-rules
          (and request
               (mevedel-request-skill-permission-rules request)))
         (session-rules (when session
                          (mevedel-session-permission-rules session)))
         (persistent (when workspace
                       (mevedel-permission--load-persistent-rules workspace))))
    (mevedel-permission--collect-buckets
     invocation-rules request-rules session-rules persistent)))

(defun mevedel-tools--bash-effective-rules ()
  "Return the merged permission-rule list visible to Bash.

Flattened helper kept for callers that don't need bucket
precedence (e.g. legacy callers, tests).  New code should use
`mevedel-tools--bash-buckets' to preserve innermost-first
allow/ask resolution."
  (let ((buckets (mevedel-tools--bash-buckets)))
    (apply #'append (mapcar #'cdr buckets))))

(cl-defun mevedel-tools--bash-bucket-action
    (buckets command &key skip-keys)
  "Two-pass bucket resolution for COMMAND against BUCKETS.

Pass 1 (deny absolute): if any bucket yields `deny', returns
`deny'.  Pass 2 (allow/ask innermost-first): walks buckets and
returns the first non-nil bucket action.  Returns nil when no
bucket matches the command pattern.

SKIP-KEYS is forwarded to the allow/ask pass; under plan mode
the pipeline asks the generic resolver to suppress the
`:invocation' and `:request' buckets, and Bash must honor the
same suppression so skill `allowed-tools' grants cannot bypass
plan mode.  Deny resolution is absolute and ignores SKIP-KEYS.

Mirrors `mevedel-check-permission' for the Bash tool's per-
command precedence so skill rules win over session rules in
allow/ask resolution but session denies still win over skill
allows."
  (cond
   ((mevedel-permission--any-deny buckets "Bash" nil command nil nil)
    'deny)
   (t
    (mevedel-permission--first-non-nil-action
     buckets "Bash" nil command nil nil skip-keys))))

(cl-defun mevedel-tools--check-bash-permission (command &key trust-literal-p)
  "Decide `allow', `deny', or `ask' for COMMAND against permission rules.

Rules come from invocation, request, session, persistent, and
defcustom buckets (in that innermost-first order) and are
matched via `:pattern'.

Default behavior: the fail-safe and dangerous-command checks
take precedence -- unparseable syntax and dangerous-blocklisted
commands always downgrade to `ask'.  Otherwise the full command
is tested first, then each extracted sub-command for defence in
depth.  Within the results, `deny' wins over `ask' which wins
over `allow'.  If nothing matches, `ask' is returned so trust-
all mode never auto-approves unknown bash invocations.

When TRUST-LITERAL-P is non-nil (skill body shell expansion
path), the dangerous-commands blocklist and the fail-safe-
complex-syntax check are SKIPPED.  Skill body shell expansions
(`!`...`' and ` ```! ` blocks) set this flag so author-written
literal commands are not treated as LLM-generated invocations.
Explicit deny rules and protected-path guards still apply --
the flag only relaxes the heuristic overlays that exist to
catch hallucinated shell from the model.

Bucket-aware: the skill buckets are consulted on the same
innermost-first order as the main permission resolver, so a
skill's `allowed-tools: [Bash(gh *)]' grants `gh' calls
without requiring a session-level rule."
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction)))

    (when (and unparseable
               mevedel-bash-fail-safe-on-complex-syntax
               (not trust-literal-p))
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    (when (null commands)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    (let* ((buckets (mevedel-tools--bash-buckets))
           (session (and (boundp 'mevedel--session) mevedel--session))
           (mode (or (and session (mevedel-session-permission-mode session))
                     mevedel-permission-mode))
           (skip-keys (mevedel-permission--plan-mode-skip-keys mode nil))
           (has-operators (string-match-p "&&\\|||\\||\\|;\\|\n" command))
           (full-action (mevedel-tools--bash-bucket-action
                         buckets command :skip-keys skip-keys))
           (dangerous-p (and (not trust-literal-p)
                             (seq-some
                              (lambda (cmd)
                                (member cmd mevedel-bash-dangerous-commands))
                              commands))))

      (cond
       ;; Full command matched and no operators: trust an explicit deny/ask
       ;; even if dangerous; only an allow is downgraded by the blocklist.
       ((and full-action (not has-operators))
        (cond
         ((memq full-action '(deny ask)) full-action)
         (dangerous-p 'ask)
         (t full-action)))
       (t
        ;; Check each extracted sub-command for defence in depth.  Explicit
        ;; deny wins over the dangerous blocklist; otherwise dangerous
        ;; downgrades allow/nil to ask.
        (let ((actions (if full-action (list full-action) nil)))
          (dolist (cmd commands)
            (push (mevedel-tools--bash-bucket-action
                   buckets cmd :skip-keys skip-keys)
                  actions))
          (cond
           ((memq 'deny actions) 'deny)
           (dangerous-p 'ask)
           ((memq nil actions) 'ask)
           ((memq 'ask actions) 'ask)
           (t 'allow))))))))


;;
;;; Eval Prompt UI

(defcustom mevedel-eval-expression-display-limit 20
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel--prompt-user-for-eval
    (expression callback &optional origin count entry)
  "Display the Eval-permission overlay; deliver UI outcome to CALLBACK.

CALLBACK is invoked once with one of `allow-once', `deny-once',
(feedback . TEXT), or `aborted'.  Long expressions are truncated in
the display and can be toggled with TAB.  ORIGIN, when non-main,
renders the same attribution line used by generic and Bash permission
prompts.  COUNT is the permission queue depth for the composite
interaction-zone counter."
  (let* ((lines (split-string expression "\n"))
         (long-p (> (length lines) mevedel-eval-expression-display-limit))
         (display-expr (if long-p
                           (concat
                            (string-join
                             (seq-take lines mevedel-eval-expression-display-limit)
                             "\n")
                            "\n"
                            (propertize
                             (format "... (%d more lines, press TAB to expand)"
                                     (- (length lines) mevedel-eval-expression-display-limit))
                             'font-lock-face 'shadow))
                         expression))
         (content (concat
                   "The LLM is requesting permission to evaluate elisp.\n\n"
                   (when (and origin
                              (not (equal origin "main"))
                              (fboundp 'mevedel-permission--build-attribution-line))
                     (mevedel-permission--build-attribution-line origin))
                   (propertize "Expression:\n" 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n\n" display-expr)
                               'font-lock-face 'font-lock-string-face))))
    (if (fboundp 'mevedel-permission--prompt-async-eval)
        (mevedel-permission--prompt-async-eval content callback count entry)
      (display-warning
       'mevedel
       "Eval permission UI unavailable"
       :warning)
      (funcall callback 'aborted))))


;;
;;; Eval permission adapter

(defun mevedel-tool-exec--eval-check-permission-async (_tool-struct input cont)
  "Async permission check for the Eval tool.

Routes the prompt through the session permission queue rather
than calling `mevedel--prompt-user-for-eval' directly.  The
queue's render-head dispatches to the specialized Eval UI via
`mevedel-permission-queue--render-eval'.  CONT receives the same
slot vocabulary as before: `allow', `deny', `(deny . REASON)',
`aborted' -- feedback text shaped into the existing
\"Eval cancelled by user. Feedback: TEXT\" form so LLM-visible
denial parity with the sync slot is preserved."
  (let ((expression (plist-get input :expression)))
    (cond
     ((null expression) (funcall cont 'deny))
     (t
      (mevedel-permission--enqueue
       (list :kind 'eval
             :expression expression
             :origin (mevedel-tool-exec--current-origin)
             :callback
             (lambda (outcome)
               (pcase outcome
                 ('allow-once (funcall cont 'allow))
                 ('deny-once  (funcall cont 'deny))
                 (`(feedback . ,text)
                  (funcall cont
                           (cons 'deny
                                 (format "Eval cancelled by user. Feedback: %s"
                                         text))))
                 ('aborted (funcall cont 'aborted))
                 (_        (funcall cont 'deny))))))))))


;;
;;; Bash Prompt UI

(defun mevedel-tool-exec--check-permission-async (_tool-struct input cont)
  "Async permission check for the Bash tool.

Pattern matching first: when `mevedel-tools--check-bash-permission'
yields a final decision the slot returns it directly.  Trust-literal
shell-expansion path also returns directly (no prompt).  When the
classifier yields `ask' the request enters the session permission
queue; the queue's render-head dispatches to the Bash-specific
overlay via `mevedel-permission-queue--render-bash' when the
entry becomes the head.  CONT receives the same slot vocabulary
as before: `allow' / `deny' / `(deny . REASON)' / `aborted'.
Feedback is shaped into the existing
\"Command cancelled by user. Feedback: TEXT\" form for LLM-visible
parity with the sync slot."
  (let ((command (plist-get input :command))
        (trust-literal-p (plist-get input :trust-literal-p)))
    (cond
     ((null command) (funcall cont nil))
     (t
      (let ((decision (mevedel-tools--check-bash-permission
                       command :trust-literal-p trust-literal-p)))
        (cond
         ((not (eq decision 'ask))
          (funcall cont decision))
         (trust-literal-p
          (funcall
           cont
           (cons 'deny
                 "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies.")))
         (t
          (let* ((session (and (boundp 'mevedel--session) mevedel--session))
                 (workspace (and session
                                 (mevedel-session-workspace session))))
            (mevedel-permission--enqueue
             (list :kind 'bash
                   :command command
                   :dangerous (mevedel-tool-exec--dangerous-command-p command)
                   :workspace workspace
                   :include-always (not (null workspace))
                   :origin (mevedel-tool-exec--current-origin)
                   :callback
                 (lambda (outcome)
                   (pcase outcome
                     ;; Route 5-button outcomes through
                     ;; --apply-prompt-result so allow-session /
                     ;; always-allow create the pattern rule before
                     ;; we settle the slot.  The function collapses
                     ;; each outcome to 'allow / 'deny.
                     ((or 'allow-once 'allow-session 'always-allow
                          'deny-once 'deny-session)
                      (condition-case err
                          (let ((collapsed
                                 (mevedel-permission--apply-prompt-result
                                  outcome "Bash" session workspace nil
                                  :spec-key :pattern
                                  :spec-value command)))
                            (funcall cont collapsed))
                        (error
                         (funcall cont
                                  (format "Error: Bash rule write failed: %S"
                                          err)))))
                     ('allow   (funcall cont 'allow))
                     ('deny    (funcall cont 'deny))
                     (`(deny . ,reason)
                      (funcall cont (cons 'deny reason)))
                     (`(feedback . ,text)
                      (funcall cont
                               (cons 'deny
                                     (format "Command cancelled by user. Feedback: %s"
                                             text))))
                     ('aborted (funcall cont 'aborted))
                     (_        (funcall cont 'deny))))))))))))))


;;
;;; Output size guard

(defconst mevedel-tool-exec--max-output-bytes (* 512 1024)
  "Hard cap on Bash/Eval tool output size in bytes.
Output exceeding this limit is truncated with a notice appended.")

(defun mevedel-tool-exec--truncate-output (output)
  "Truncate OUTPUT string if it exceeds the byte limit.
Returns OUTPUT unchanged when within budget, or a truncated copy
with a notice when it exceeds `mevedel-tool-exec--max-output-bytes'."
  (if (<= (length output) mevedel-tool-exec--max-output-bytes)
      output
    (concat
     (substring output 0 mevedel-tool-exec--max-output-bytes)
     (format "\n\n... Output truncated (%dK of %dK bytes shown)."
             (/ mevedel-tool-exec--max-output-bytes 1024)
             (/ (length output) 1024)))))


;;
;;; Bash

(defun mevedel-tool-exec--bash (callback args)
  "Execute a Bash command and return its output.
CALLBACK receives the result string.  ARGS is a plist with :command."
  (let ((command (plist-get args :command)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (condition-case err
        (let* ((output-buffer (generate-new-buffer " *mevedel-bash*"))
               (proc (make-process
                      :name "mevedel-bash"
                      :buffer output-buffer
                      :command (list "bash" "-c" command)
                      :connection-type 'pipe
                      :sentinel
                      (lambda (process _event)
                        (condition-case sentinel-err
                            (when (memq (process-status process) '(exit signal))
                              (let* ((exit-code (process-exit-status process))
                                     (output (mevedel-tool-exec--truncate-output
                                              (with-current-buffer (process-buffer process)
                                                (buffer-string)))))
                                (kill-buffer (process-buffer process))
                                (funcall callback
                                         (if (zerop exit-code)
                                             output
                                           (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
                                                   exit-code output)))))
                          (error
                           (kill-buffer (process-buffer process))
                           (funcall callback
                                    (format "Error in sentinel: %s" sentinel-err))))))))
          proc)
      (error
       (funcall callback (format "Failed to start process: %s" err))
       nil))))


;;
;;; Eval

(defun mevedel-tool-exec--eval (callback args)
  "Evaluate an Elisp expression and return the result.
CALLBACK receives the result string.  ARGS is a plist with :expression."
  (let ((expression (plist-get args :expression)))
    (unless (stringp expression)
      (error "Parameter expression is required"))
    (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
          (result nil) (output nil))
      (unwind-protect
          (condition-case err
              (progn
                (setq result (eval (read expression) t))
                (when (> (buffer-size standard-output) 0)
                  (setq output (mevedel-tool-exec--truncate-output
                                (with-current-buffer standard-output
                                  (buffer-string)))))
                (funcall callback
                         (mevedel-tool-exec--truncate-output
                          (concat
                           (format "Result:\n%S" result)
                           (and output (format "\n\nSTDOUT:\n%s" output))))))
            ((error user-error)
             (funcall callback
                      (concat
                       (format "Error: Eval failed with error %S: %S"
                               (car err) (cdr err))
                       (and output (format "\n\nSTDOUT:\n%s" output))))))
        (kill-buffer standard-output)))))


;;
;;; Renderers

(defun mevedel-tool-exec--render-bash (name args result _render-data)
  "Rendering plist for the Bash tool.
NAME is \"Bash\".  ARGS carries `:command'.  RESULT is stdout/stderr.
Header shows a truncated first line of the command; body fontifies as
`sh-mode'."
  (when (stringp result)
    (let* ((cmd (or (plist-get args :command) ""))
           (first-line (car (split-string cmd "\n"))))
      (list :header (format "%s: %s" (or name "Bash") first-line)
            :body result
            :body-mode 'sh-mode
            :initially-collapsed-p t))))


;;
;;; Tool registration

(defun mevedel-tool-exec--register ()
  "Register Bash and Eval tools."

  (mevedel-define-tool
    :name "Bash"
    :description "Execute Bash commands."
    :prompt-file "tools/bash.md"
    :handler #'mevedel-tool-exec--bash
    :args ((command string :required
                   "The Bash command to execute. Can include pipes and standard shell operators."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--check-permission-async
    :get-pattern (lambda (input) (plist-get input :command))
    :renderer #'mevedel-tool-exec--render-bash)

  (mevedel-define-tool
    :name "Eval"
    :description "Evaluate an Elisp expression and return the result."
    :prompt-file "tools/eval.md"
    :handler #'mevedel-tool-exec--eval
    :args ((expression string :required "A single elisp sexp to evaluate."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--eval-check-permission-async))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
