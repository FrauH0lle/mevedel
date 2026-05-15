;;; mevedel-tool-exec.el -- Bash and Eval tools -*- lexical-binding: t -*-

;;; Commentary:

;; Bash command execution with permission system and Eval tool for elisp
;; evaluation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'subr-x)

;; `gptel'
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

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
(declare-function mevedel-permission--path-protected-p
                  "mevedel-permissions" (path))
(defvar mevedel-permission-rules)
(defvar mevedel-permission-mode)
(defvar mevedel-protected-paths)

;; `mevedel-structs'
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-tool-ui'
(declare-function mevedel-permission--enqueue "mevedel-permission-queue"
                  (entry &optional session))
(declare-function mevedel-permission-queue--current-session
                  "mevedel-permission-queue" ())
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))
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

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))


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

(defcustom mevedel-permission-guardian nil
  "Whether to annotate Bash permission prompts with risk guidance.

When nil, permission prompts are rendered without guardian guidance.
When t, mevedel asks the current gptel model for advisory-only Bash
risk classification while an `ask' prompt is pending.

A function value is useful for custom classifiers and tests.  It is
called as (FUNCTION COMMAND CONTEXT CALLBACK), where CONTEXT is a plist
with parser metadata and CALLBACK accepts either nil or a plist:

  (:risk low|medium|high|critical
   :recommendation allow-once|ask|deny
   :reason \"short explanation\")

The result is never used for enforcement.  Explicit deny, plan mode,
protected-path policy, and the user's decision remain authoritative."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Use gptel reviewer" t)
                 function)
  :group 'mevedel)

(defcustom mevedel-permission-guardian-timeout 20
  "Seconds to wait before giving up on Bash guardian guidance."
  :type 'number
  :group 'mevedel)

(defconst mevedel-tool-exec--bash-safe-env-vars
  '("GOEXPERIMENT" "GOOS" "GOARCH" "CGO_ENABLED" "GO111MODULE"
    "RUST_BACKTRACE" "RUST_LOG"
    "NODE_ENV"
    "PYTHONUNBUFFERED" "PYTHONDONTWRITEBYTECODE"
    "PYTEST_DISABLE_PLUGIN_AUTOLOAD" "PYTEST_DEBUG"
    "LANG" "LANGUAGE" "LC_ALL" "LC_CTYPE" "LC_TIME" "CHARSET"
    "TERM" "COLORTERM" "NO_COLOR" "FORCE_COLOR" "TZ"
    "LS_COLORS" "LSCOLORS" "GREP_COLOR" "GREP_COLORS" "GCC_COLORS"
    "TIME_STYLE" "BLOCK_SIZE" "BLOCKSIZE")
  "Environment variables safe to skip before suggesting Bash prefix rules.")

(defconst mevedel-tool-exec--bash-never-prefix-commands
  '("sh" "bash" "zsh" "fish" "csh" "tcsh" "ksh" "dash"
    "env" "xargs"
    "nice" "stdbuf" "nohup" "timeout" "time"
    "doas" "pkexec" "su")
  "Shells and wrappers that must not be generalized to prefix rules.")

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

(defun mevedel-tool-exec--dedupe-strings (strings)
  "Return STRINGS without duplicates, preserving first occurrence order."
  (let (seen result)
    (dolist (s strings)
      (when (and (stringp s)
                 (not (string-empty-p s))
                 (not (member s seen)))
        (push s seen)
        (push s result)))
    (nreverse result)))

(defun mevedel-tool-exec--bash-segment-words (segment)
  "Return shell words parsed from SEGMENT, or nil when parsing fails."
  (condition-case nil
      (split-string-and-unquote segment)
    (error nil)))

(defun mevedel-tool-exec--bash-env-assignment-p (word)
  "Return non-nil when WORD is a leading shell env assignment."
  (and (stringp word)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" word)))

(defun mevedel-tool-exec--bash-safe-env-assignment-p (word)
  "Return non-nil when WORD is a safe env assignment for prefix matching."
  (and (mevedel-tool-exec--bash-env-assignment-p word)
       (member (car (split-string word "=" t))
               mevedel-tool-exec--bash-safe-env-vars)))

(defun mevedel-tool-exec--bash-strip-safe-env-assignments (words)
  "Strip safe leading env assignments from WORDS.

Return nil if a leading env assignment is not known safe.  This
avoids saving prefix rules that will not match later permission
checks, and avoids hiding environment-controlled behavior behind a
general rule."
  (catch 'unsafe
    (while (and words
                (mevedel-tool-exec--bash-env-assignment-p (car words)))
      (unless (mevedel-tool-exec--bash-safe-env-assignment-p (car words))
        (throw 'unsafe nil))
      (setq words (cdr words)))
    words))

(defun mevedel-tool-exec--bash-subcommand-token-p (word)
  "Return non-nil when WORD looks like a stable shell subcommand."
  (and (stringp word)
       (string-match-p
        "\\`[[:lower:]][[:lower:][:digit:]]*\\(?:-[[:lower:][:digit:]]+\\)*\\'"
        word)))

(defun mevedel-tool-exec--bash-command-never-prefix-p (command)
  "Return non-nil when COMMAND should not get a broad prefix rule."
  (member command
          (append mevedel-bash-dangerous-commands
                  mevedel-tool-exec--bash-never-prefix-commands)))

(defun mevedel-tool-exec--bash-prefix-for-segment (segment)
  "Return a stable command prefix for Bash SEGMENT, or nil.

The heuristic follows Claude Code's low-maintenance shape: derive
`command subcommand' generically, only when the second token looks
like a subcommand rather than a flag, path, file name, or number.
Dangerous commands and shell/wrapper commands are not generalized."
  (let* ((words (mevedel-tool-exec--bash-segment-words segment))
         (words (and words
                     (mevedel-tool-exec--bash-strip-safe-env-assignments
                      words)))
         (command (car words))
         (subcommand (cadr words)))
    (when (and command
               subcommand
               (not (mevedel-tool-exec--bash-command-never-prefix-p command))
               (mevedel-tool-exec--bash-subcommand-token-p subcommand))
      (string-join (list command subcommand) " "))))

(defun mevedel-tool-exec--bash-allow-pattern-for-segment (segment)
  "Return the reusable allow pattern suggested for Bash SEGMENT.

Simple `command subcommand ...' invocations are generalized to
Claude Code-style prefix rules such as `git log:*'.  Segments that
do not have a stable subcommand, or that start with a dangerous
command/wrapper, stay exact."
  (let* ((trimmed (string-trim segment))
         (prefix (mevedel-tool-exec--bash-prefix-for-segment trimmed)))
    (if prefix
        (concat prefix ":*")
      trimmed)))

(defun mevedel-tool-exec--bash-allow-patterns (command)
  "Return reusable allow patterns to store when approving COMMAND.

Compound commands produce one pattern per command segment.  This
avoids saving a brittle whole-chain string such as
`pwd && git log --oneline' when the useful reusable rule is
`git log:*'."
  (mevedel-tool-exec--dedupe-strings
   (mapcar #'mevedel-tool-exec--bash-allow-pattern-for-segment
           (mevedel-tools--split-command-chain command))))

(defun mevedel-tool-exec--effective-permission-mode ()
  "Return the effective permission mode for the current tool call."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (or (and session (mevedel-session-permission-mode session))
        mevedel-permission-mode)))

(defun mevedel-tool-exec--effective-trust-p (trust-literal-p &optional mode)
  "Return non-nil when heuristic permission prompts should be bypassed.
Trusted skill literals and `trust-all' mode share this predicate for
skipping Bash/Eval heuristic prompts.  Explicit deny rules and protected
paths are checked separately before callers use this result."
  (or trust-literal-p
      (eq (or mode (mevedel-tool-exec--effective-permission-mode))
          'trust-all)))

(defun mevedel-tool-exec--bash-path-word (word)
  "Return the literal path part of shell WORD, or nil.
Handles common redirection spellings such as `>/tmp/file',
`2>>log', and `< ~/.ssh/config' after shell word splitting.  File
descriptor duplication forms like `2>&1' are ignored."
  (when (stringp word)
    (let ((path word))
      (when (string-match
             "\\`[0-9]*\\(?:<<-?\\|<>\\|>>\\|>\\|<\\)\\(.+\\)\\'"
             path)
        (setq path (match-string 1 path)))
      (unless (or (string-empty-p path)
                  (string-prefix-p "&" path))
        path))))

(defun mevedel-tool-exec--bash-literal-path-tokens (command)
  "Return literal shell words from COMMAND that look path-like.
The scan is intentionally shallow: it catches obvious tokens such as
`.git/config', `secrets/key', and `/home/me/.gnupg' without trying to
evaluate variables or globs.  Command substitution bodies are scanned
recursively as literal text."
  (let (seen paths)
    (cl-labels
        ((add-path (path)
           (when (and (stringp path)
                      (not (member path seen)))
             (push path seen)
             (push path paths)))
         (path-like-p (path)
           (and (not (string-prefix-p "-" path))
                (or (string-prefix-p "~/" path)
                    (string-prefix-p "/" path)
                    (string-prefix-p "./" path)
                    (string-prefix-p "../" path)
                    (string-match-p "/" path)
                    (string-match-p "\\(?:\\`\\|/\\)\\.[^/]+\\(?:/\\|\\'\\)"
                                    path))))
         (scan (value)
           (dolist (segment (mevedel-tools--split-command-chain value))
             (dolist (word (or (mevedel-tool-exec--bash-segment-words segment)
                               nil))
               (when-let* ((path (mevedel-tool-exec--bash-path-word word)))
                 (when (path-like-p path)
                   (add-path path))))
             (dolist (subst (mevedel-tools--extract-substitutions segment))
               (scan subst)))))
      (scan command))
    (nreverse paths)))

(defun mevedel-tool-exec--bash-protected-path-p (command)
  "Return non-nil when COMMAND contains an obvious protected path token."
  (cl-some
   (lambda (path)
     (or (mevedel-permission--path-protected-p path)
         ;; Directory roots such as `.git' may be protected by a
         ;; `**/.git/**' policy even when the literal token has no child.
         (mevedel-permission--path-protected-p
          (file-name-as-directory path))
         (cl-some
          (lambda (name)
            (and (cl-some (lambda (pattern)
                            (string-match-p
                             (concat "\\." (regexp-quote name)
                                     "\\(?:/\\|\\'\\)")
                             pattern))
                          mevedel-protected-paths)
                 (string-match-p
                  (concat "\\(?:\\`\\|/\\)\\." (regexp-quote name)
                          "\\(?:/\\|\\'\\)")
                  path)))
          '("git" "ssh" "gnupg"))))
   (mevedel-tool-exec--bash-literal-path-tokens command)))

(defun mevedel-tool-exec--bash-deny-candidates (command)
  "Return Bash strings that explicit deny rules should check.
Includes the whole command, top-level command-chain segments, and
command substitutions recursively.  This preserves argument-sensitive
deny patterns such as `rm *' inside `$(...)' without making a generic
deny on bare extracted command names override an explicit allow for the
full command."
  (let (seen result)
    (cl-labels
        ((add (value)
           (when (and (stringp value)
                      (not (string-empty-p (string-trim value)))
                      (not (member value seen)))
             (push value seen)
             (push value result)))
         (walk (value)
           (add value)
           (dolist (segment (mevedel-tools--split-command-chain value))
             (add segment)
             (dolist (subst (mevedel-tools--extract-substitutions segment))
               (walk subst)))))
      (walk command))
    (nreverse result)))

(defun mevedel-tool-exec--bash-explicit-deny-p (buckets command)
  "Return non-nil when any explicit Bash deny covers COMMAND."
  (or
   (cl-some
    (lambda (candidate)
      (mevedel-permission--any-deny
       buckets "Bash" nil candidate nil nil))
    (mevedel-tool-exec--bash-deny-candidates command))
   (mevedel-tool-exec--bash-extracted-command-deny-p buckets command)))

(defun mevedel-tool-exec--bash-extracted-command-deny-p (buckets command)
  "Return non-nil when a patterned deny matches an extracted Bash command.

This preserves hard denies such as `(\"Bash\" :pattern \"rm\" :action
deny)' for `rm /tmp/foo' and `echo $(rm /tmp/foo)' in `trust-all'
mode.  Only specifier-carrying `:pattern' rules are considered here, so
a generic unqualified Bash deny does not override a more specific allow
for the full command."
  (cl-some
   (lambda (cmd)
     (cl-some
      (lambda (entry)
        (eq (mevedel-permission--rules-action
             (seq-filter
              (lambda (rule)
                (plist-member (cdr rule) :pattern))
              (cdr entry))
             "Bash" :pattern cmd)
            'deny))
      buckets))
   (car (mevedel-tools--extract-commands command))))

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

(cl-defun mevedel-tools--check-bash-permission
    (command &key trust-literal-p ignore-effective-trust-p)
  "Decide `allow', `deny', or `ask' for COMMAND against permission rules.

Rules come from invocation, request, session, persistent, and
defcustom buckets (in that innermost-first order) and are
matched via `:pattern'.

Default behavior: the fail-safe and dangerous-command checks take
precedence -- unparseable syntax and dangerous-blocklisted commands
downgrade to `ask'.  Otherwise the full command is tested first, then
each extracted sub-command for defence in depth.  Within the results,
`deny' wins over `ask' which wins over `allow'.  If nothing matches,
`ask' is returned unless the effective permission mode is `trust-all'.
In `plan' mode, prompt-requiring `ask' outcomes are hard-denied after
explicit non-skill allow rules have had their chance to decide.

When TRUST-LITERAL-P is non-nil (skill body shell expansion
path), the dangerous-commands blocklist and the fail-safe-
complex-syntax check are SKIPPED.  Skill body shell expansions
(`!`...`' and ` ```! ` blocks) set this flag so author-written
literal commands are not treated as LLM-generated invocations.
Explicit deny rules and protected-path guards still apply --
the flag only relaxes the heuristic overlays that exist to
catch hallucinated shell from the model.

In `trust-all' mode, explicit deny rules and protected path tokens still
win, then unknown, dangerous, and complex Bash invocations are allowed.
When IGNORE-EFFECTIVE-TRUST-P is non-nil, `trust-all' is ignored; this
is used by the guardian to decide whether a command would have been
suspicious under the normal classifier.

Bucket-aware: the skill buckets are consulted on the same
innermost-first order as the main permission resolver, so a
skill's `allowed-tools: [Bash(gh *)]' grants `gh' calls
without requiring a session-level rule."
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction))
         (buckets (mevedel-tools--bash-buckets))
         (mode (mevedel-tool-exec--effective-permission-mode))
         (effective-trust-p
          (and (not ignore-effective-trust-p)
               (mevedel-tool-exec--effective-trust-p trust-literal-p mode)))
         (trust-all-p (and effective-trust-p (eq mode 'trust-all)))
         (plan-mode-p (eq mode 'plan)))
    (when (mevedel-tool-exec--bash-explicit-deny-p buckets command)
      (cl-return-from mevedel-tools--check-bash-permission 'deny))

    (when (mevedel-tool-exec--bash-protected-path-p command)
      (cl-return-from mevedel-tools--check-bash-permission
        (if plan-mode-p 'deny 'ask)))

    (when (and unparseable
               mevedel-bash-fail-safe-on-complex-syntax
               (not effective-trust-p))
      (cl-return-from mevedel-tools--check-bash-permission
        (if plan-mode-p 'deny 'ask)))

    (let* ((commands (or commands '()))
           (skip-keys (mevedel-permission--plan-mode-skip-keys mode nil))
           (has-operators (string-match-p "&&\\|||\\||\\|;\\|\n" command))
           (segments (mevedel-tools--split-command-chain command))
           (has-nested-commands (> (length commands) (length segments)))
           (full-action (mevedel-tools--bash-bucket-action
                         buckets command :skip-keys skip-keys))
           (dangerous-p (and (not trust-literal-p)
                             (not effective-trust-p)
                             (seq-some
                              (lambda (cmd)
                                (member cmd mevedel-bash-dangerous-commands))
                              commands))))

      (when (and trust-all-p
                 (not (eq full-action 'deny)))
        (cl-return-from mevedel-tools--check-bash-permission 'allow))

      (when (null commands)
        (cl-return-from mevedel-tools--check-bash-permission
          (if plan-mode-p 'deny 'ask)))

      (let ((decision
             (cond
              ;; Full command matched and no operators: trust an explicit
              ;; deny/ask even if dangerous; only an allow is downgraded by
              ;; the blocklist.
              ((and full-action (not has-operators) (not has-nested-commands))
               (cond
                ((memq full-action '(deny ask)) full-action)
                (dangerous-p 'ask)
                (t full-action)))
              (t
               ;; Check each extracted sub-command for defence in depth.
               ;; Explicit deny wins over the dangerous blocklist; otherwise
               ;; dangerous downgrades allow/nil to ask.
               (let ((actions (if full-action (list full-action) nil)))
                 (dolist (segment segments)
                   (let* ((segment-action
                           (mevedel-tools--bash-bucket-action
                            buckets segment :skip-keys skip-keys))
                          (segment-commands
                           (car (mevedel-tools--extract-commands segment)))
                          (commands-to-check
                           (if segment-action
                               (cdr segment-commands)
                             segment-commands))
                          (command-actions
                           (mapcar
                            (lambda (cmd)
                              (mevedel-tools--bash-bucket-action
                               buckets cmd :skip-keys skip-keys))
                            commands-to-check)))
                     (when segment-action
                       (push segment-action actions))
                     (dolist (action command-actions)
                       (push action actions))))
                 (cond
                  ((memq 'deny actions) 'deny)
                  (dangerous-p 'ask)
                  ((memq nil actions) 'ask)
                  ((memq 'ask actions) 'ask)
                  (t 'allow)))))))
        (if (and plan-mode-p (eq decision 'ask))
            'deny
          decision)))))


;;
;;; Bash guardian guidance

(defun mevedel-tool-exec--bash-guardian-symbol (value allowed)
  "Return VALUE as a normalized symbol when it is in ALLOWED."
  (let* ((string (cond
                  ((symbolp value) (symbol-name value))
                  ((stringp value) value)))
         (symbol (and string
                      (intern
                       (replace-regexp-in-string
                        "_" "-"
                        (downcase (string-trim string)))))))
    (and (memq symbol allowed) symbol)))

(defun mevedel-tool-exec--bash-guardian-truncate (string limit)
  "Return STRING capped at LIMIT characters."
  (let ((string (string-trim (or string ""))))
    (if (> (length string) limit)
        (concat (substring string 0 limit) "...")
      string)))

(defun mevedel-tool-exec--bash-guardian-normalize (guidance)
  "Return normalized Bash guardian GUIDANCE plist, or nil."
  (when (listp guidance)
    (let* ((risk (mevedel-tool-exec--bash-guardian-symbol
                  (plist-get guidance :risk)
                  '(low medium high critical)))
           (recommendation (mevedel-tool-exec--bash-guardian-symbol
                            (plist-get guidance :recommendation)
                            '(allow-once ask deny)))
           (reason (plist-get guidance :reason)))
      (when (and risk recommendation (stringp reason)
                 (not (string-empty-p (string-trim reason))))
        (list :risk risk
              :recommendation recommendation
              :reason (mevedel-tool-exec--bash-guardian-truncate reason 240))))))

(defun mevedel-tool-exec--bash-guardian-json-range (text)
  "Return the first likely JSON object substring in TEXT, or nil."
  (when-let* ((start (string-match "{" text)))
    (let ((i (1- (length text)))
          end)
      (while (and (>= i start) (not end))
        (when (eq (aref text i) ?\})
          (setq end i))
        (setq i (1- i)))
      (and end (substring text start (1+ end))))))

(defun mevedel-tool-exec--bash-guardian-parse (response)
  "Parse guardian RESPONSE into normalized guidance, or nil."
  (when (stringp response)
    (when-let* ((json (mevedel-tool-exec--bash-guardian-json-range response)))
      (condition-case nil
          (mevedel-tool-exec--bash-guardian-normalize
           (progn
             (require 'json)
             (json-parse-string json
                                :object-type 'plist
                                :array-type 'list
                                :null-object nil
                                :false-object nil)))
        (error nil)))))

(defun mevedel-tool-exec--bash-guardian-context-string (context)
  "Return CONTEXT formatted for the Bash guardian prompt."
  (string-join
   (delq nil
         (list
          (format "Dangerous command detected: %s"
                  (if (plist-get context :dangerous) "yes" "no"))
          (format "Complex or unparseable syntax: %s"
                  (if (plist-get context :unparseable) "yes" "no"))
          (when-let* ((commands (plist-get context :commands)))
            (format "Detected commands: %s"
                    (string-join commands ", ")))
          (when-let* ((patterns (plist-get context :allow-patterns)))
            (format "Suggested allow patterns: %s"
                    (string-join patterns ", ")))))
   "\n"))

(defun mevedel-tool-exec--bash-guardian-model-async (command context callback)
  "Ask gptel for advisory-only Bash risk guidance.
CALLBACK receives normalized guidance or nil."
  (if (not (require 'gptel nil t))
      (funcall callback nil)
    (let ((done nil)
          timer)
      (cl-labels
          ((finish (guidance)
             (unless done
               (setq done t)
               (when timer
                 (cancel-timer timer))
               (funcall callback guidance))))
        (setq timer
              (run-at-time
               mevedel-permission-guardian-timeout nil
               (lambda ()
                 (finish nil))))
        (condition-case nil
            (let ((gptel-use-tools nil)
                  (gptel-tools nil)
                  (gptel-use-context nil)
                  (prompt
                   (progn
                     (require 'mevedel-system)
                     (mevedel-system-render-prompt-file
                      "prompts/permissions/bash-guardian.md"
                      `(("COMMAND" . ,command)
                        ("CONTEXT" . ,(mevedel-tool-exec--bash-guardian-context-string
                                        context)))))))
              (gptel-request
                prompt
                :buffer (current-buffer)
                :stream nil
                :transforms nil
                :callback
                (lambda (response _info)
                  (cond
                   ((stringp response)
                    (finish
                     (mevedel-tool-exec--bash-guardian-parse response)))
                   ((or (null response) (eq response 'abort))
                    (finish nil))))))
          (error
           (finish nil)))))))

(defun mevedel-tool-exec--bash-guardian-classify-async
    (command context callback)
  "Return optional guardian guidance for COMMAND.
CALLBACK receives nil or a normalized guidance plist."
  (cond
   ((null mevedel-permission-guardian)
    (funcall callback nil))
   ((functionp mevedel-permission-guardian)
    (let ((done nil)
          timer)
      (cl-labels
          ((finish (guidance)
             (unless done
               (setq done t)
               (when timer
                 (cancel-timer timer))
               (funcall callback
                        (mevedel-tool-exec--bash-guardian-normalize
                         guidance)))))
        (setq timer
              (run-at-time
               mevedel-permission-guardian-timeout nil
               (lambda ()
                 (finish nil))))
        (condition-case nil
            (funcall mevedel-permission-guardian command context #'finish)
          (error
           (finish nil))))))
   (t
    (mevedel-tool-exec--bash-guardian-model-async
     command context callback))))

(defun mevedel-tool-exec--bash-trust-all-guardian-needed-p (command)
  "Return non-nil when COMMAND should get deny-only guardian review.
This is only for `trust-all' mode.  The guardian is consulted when the
normal classifier would have asked, avoiding latency for routine allowed
commands while still giving the optional guardian a chance to veto
suspicious Bash."
  (and mevedel-permission-guardian
       (eq (mevedel-tool-exec--effective-permission-mode) 'trust-all)
       (eq (mevedel-tools--check-bash-permission
            command :ignore-effective-trust-p t)
           'ask)))

(defun mevedel-tool-exec--bash-guardian-context (command)
  "Return guardian context plist for COMMAND."
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction)))
    (list :dangerous (mevedel-tool-exec--dangerous-command-p command)
          :commands commands
          :unparseable unparseable
          :allow-patterns (mevedel-tool-exec--bash-allow-patterns command))))

(defun mevedel-tool-exec--bash-deny-only-guardian-async (command cont)
  "Run deny-only guardian review for COMMAND, then call CONT.
Guardian deny recommendations become `deny'.  Timeout, failure, invalid
output, and non-deny recommendations allow by default."
  (mevedel-tool-exec--bash-guardian-classify-async
   command
   (mevedel-tool-exec--bash-guardian-context command)
   (lambda (guardian)
     (funcall cont
              (if (eq (plist-get guardian :recommendation) 'deny)
                  'deny
                'allow)))))


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

(defun mevedel-tools--eval-buckets ()
  "Return the bucket alist visible to Eval, innermost-first."
  (mevedel-tools--bash-buckets))

(cl-defun mevedel-tools--check-eval-permission
    (&key trust-literal-p)
  "Decide `allow', `deny', or `ask' for an Eval invocation.

Normal model-requested Eval asks unless an explicit deny rule applies
or the effective permission mode is `trust-all'.  When TRUST-LITERAL-P
is non-nil, as with author-written skill body injections, an active
allow rule for Eval may bypass the prompt.  Deny rules still win
absolutely, and plan mode suppresses skill-bucket allows because Eval is
not read-only.  In plan mode, Eval is denied unless an explicit
non-skill allow rule returns an earlier `allow' decision."
  (let* ((buckets (mevedel-tools--eval-buckets))
         (mode (mevedel-tool-exec--effective-permission-mode))
         (skip-keys (mevedel-permission--plan-mode-skip-keys mode nil))
         (action (mevedel-permission--first-non-nil-action
                  buckets "Eval" nil nil nil nil skip-keys)))
    (cond
     ((mevedel-permission--any-deny buckets "Eval" nil nil nil nil)
      'deny)
     ((eq mode 'trust-all)
      'allow)
     ((eq mode 'plan)
      (if (eq action 'allow) 'allow 'deny))
     (trust-literal-p
      (or action 'ask))
     (t 'ask))))

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
  (let ((expression (plist-get input :expression))
        (trust-literal-p (plist-get input :trust-literal-p)))
    (cond
     ((null expression) (funcall cont 'deny))
     (t
      (pcase (mevedel-tools--check-eval-permission
              :trust-literal-p trust-literal-p)
        ('allow
         (funcall cont 'allow))
        ('deny
         (funcall cont 'deny))
        (_
         (if trust-literal-p
             (funcall
              cont
              (cons 'deny
                    "Elisp expansion requires a pre-approved Eval rule; no prompt is shown while preparing skill bodies."))
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
                      (_        (funcall cont 'deny)))))))))))))


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
    (if (null command)
        (funcall cont nil)
      (let ((decision (mevedel-tools--check-bash-permission
                       command :trust-literal-p trust-literal-p)))
        (cond
         ((not (eq decision 'ask))
          (if (and (eq decision 'allow)
                   (mevedel-tool-exec--bash-trust-all-guardian-needed-p
                    command))
              (mevedel-tool-exec--bash-deny-only-guardian-async
               command cont)
            (funcall cont decision)))
         (trust-literal-p
          (funcall
           cont
           (cons 'deny
                 "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies.")))
         (t
          (let* ((source-buffer (current-buffer))
                 (session (or (and (boundp 'mevedel--session)
                                   mevedel--session)
                              (mevedel-permission-queue--current-session)))
                 (guardian-pending t)
                 (workspace (and session
                                 (mevedel-session-workspace session)))
                 (extraction (mevedel-tools--extract-commands command))
                 (commands (car extraction))
                 (unparseable (cdr extraction))
                 (allow-patterns
                  (mevedel-tool-exec--bash-allow-patterns command))
                 (dangerous
                  (mevedel-tool-exec--dangerous-command-p command))
                 (guardian-cell
                  (list nil (and mevedel-permission-guardian 'pending)))
                 (entry
                  (list :kind 'bash
                        :command command
                        :dangerous dangerous
                        :commands commands
                        :unparseable unparseable
                        :allow-patterns allow-patterns
                        :guardian-cell guardian-cell
                        :workspace workspace
                        :include-always (not (null workspace))
                        :origin (mevedel-tool-exec--current-origin)
                        :callback
                        (lambda (outcome)
                          (setq guardian-pending nil)
                          (pcase outcome
                            ;; Route 5-button outcomes through
                            ;; --apply-bash-prompt-result so allow-session /
                            ;; always-allow create the suggested pattern rule
                            ;; before we settle the slot.  The function
                            ;; collapses each outcome to 'allow / 'deny.
                            ((or 'allow-once 'allow-session 'always-allow
                                 'deny-once 'deny-session)
                             (condition-case err
                                 (let ((collapsed
                                        (mevedel-tool-exec--apply-bash-prompt-result
                                         outcome session workspace command
                                         allow-patterns)))
                                   (funcall cont collapsed))
                               (error
                                (funcall cont
                                         (format "Error: Bash rule write failed: %S"
                                                 err)))))
                            ('allow (funcall cont 'allow))
                            ('deny (funcall cont 'deny))
                            (`(deny . ,reason)
                             (funcall cont (cons 'deny reason)))
                            (`(feedback . ,text)
                             (funcall cont
                                      (cons 'deny
                                            (format "Command cancelled by user. Feedback: %s"
                                                    text))))
                            ('aborted (funcall cont 'aborted))
                            (_ (funcall cont 'deny)))))))
            (if (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (mevedel-permission--enqueue entry session))
              (mevedel-permission--enqueue entry session))
            (mevedel-tool-exec--bash-guardian-classify-async
             command
             (list :dangerous dangerous
                   :commands commands
                   :unparseable unparseable
                   :allow-patterns allow-patterns
                   :workspace workspace)
             (lambda (guardian)
               (when guardian-pending
                 (let ((was-pending (eq (cadr guardian-cell) 'pending)))
                   (setcar guardian-cell guardian)
                   (when was-pending
                     (setcar (cdr guardian-cell)
                             (if guardian 'done 'unavailable)))
                   (when (or guardian was-pending)
                     ;; Replace the pending placeholder in-place with
                     ;; either guidance or an unavailable note.
                     (when (buffer-live-p source-buffer)
                       (with-current-buffer source-buffer
                         (mevedel-permission-queue--render-head
                          session)))))))))))))))

(defun mevedel-tool-exec--apply-bash-prompt-result
    (outcome session workspace command allow-patterns)
  "Apply Bash permission prompt OUTCOME and return `allow' or `deny'.

Session/permanent allow outcomes store ALLOW-PATTERNS as Bash
`:pattern' rules instead of saving COMMAND verbatim.  Deny-session
stays exact to avoid broad negative rules from a single rejection."
  (pcase outcome
    ('allow-once 'allow)
    ((or 'allow-session 'always-allow)
     (dolist (pattern (or allow-patterns (list command)))
       (mevedel-permission--apply-prompt-result
        outcome "Bash" session workspace nil
        :spec-key :pattern
        :spec-value pattern))
     'allow)
    ('deny-once 'deny)
    ('deny-session
     (mevedel-permission--apply-prompt-result
      outcome "Bash" session workspace nil
      :spec-key :pattern
      :spec-value command)
     'deny)
    (_ 'deny)))


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

(defun mevedel-tool-exec--default-directory ()
  "Return the cwd Bash/Eval handlers should use.

Prefer the active session working directory when available, then the
workspace root.  Fall back to the caller's `default-directory' for
direct non-workspace uses."
  (let* ((session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (cond
                     (session (mevedel-session-workspace session))
                     ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (session-dir (and session
                           (ignore-errors
                             (mevedel-session-working-directory session))))
         (root (and workspace
                    (ignore-errors
                      (mevedel-workspace-root workspace)))))
    (file-name-as-directory (or session-dir root default-directory))))

(defun mevedel-tool-exec--bash (callback args)
  "Execute a Bash command and return its output.
CALLBACK receives the result string.  ARGS is a plist with :command."
  (let ((command (plist-get args :command)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (condition-case err
        (let* ((output-buffer (generate-new-buffer " *mevedel-bash*"))
               (workdir (mevedel-tool-exec--default-directory))
               (proc (let ((default-directory workdir))
                       (with-current-buffer output-buffer
                         (setq-local default-directory workdir))
                       (make-process
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
                                      (format "Error in sentinel: %s" sentinel-err)))))))))
          proc)
      (error
       (funcall callback (format "Failed to start process: %s" err))
       nil))))


;;
;;; Eval

(defun mevedel-tool-exec--eval (callback args)
  "Evaluate an Elisp expression and return the result.
CALLBACK receives the result string.  ARGS is a plist with :expression."
  (let ((expression (plist-get args :expression))
        (result-format (plist-get args :result-format)))
    (unless (stringp expression)
      (error "Parameter expression is required"))
    (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
          (result nil) (output nil))
      (unwind-protect
          (condition-case err
              (let ((default-directory
                      (mevedel-tool-exec--default-directory)))
                (setq result (eval (read expression) t))
                (when (> (buffer-size standard-output) 0)
                  (setq output (mevedel-tool-exec--truncate-output
                                (with-current-buffer standard-output
                                  (buffer-string)))))
                (funcall callback
                         (mevedel-tool-exec--truncate-output
                          (if (eq result-format 'injection)
                              (concat
                               (format "%S" result)
                               (and output (format "\n\nSTDOUT:\n%s" output)))
                            (concat
                             (format "Result:\n%S" result)
                             (and output (format "\n\nSTDOUT:\n%s" output)))))))
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
                   "The Bash command to execute from the session working directory. Can include pipes and standard shell operators."))
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
    :args ((expression string :required "A single elisp sexp to evaluate with default-directory set to the session working directory."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--eval-check-permission-async))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
