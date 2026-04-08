;;; mevedel-tool-exec.el -- Bash and Eval tools -*- lexical-binding: t -*-

;;; Commentary:

;; Bash command execution with permission system and Eval tool for elisp
;; evaluation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-chat'
(declare-function mevedel-abort "mevedel-chat" (&optional buf))

;; `mevedel-tool-ui'
(declare-function mevedel--prompt-user-with-overlay "mevedel-tool-ui" (title content question &optional help-echo-text))
(declare-function mevedel-tools--request-access "mevedel-tool-ui" (root reason &optional buffer))

;; `mevedel-workspace'
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))


;;
;;; Bash Prompt UI

(defun mevedel--prompt-user-for-bash-command (command)
  "Prompt user for permission to execute COMMAND in the chat buffer.
Returns one of:
- t if approved
- nil if denied
- (feedback . TEXT) if user provides feedback

Displays an overlay showing the command and extracted sub-commands."
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction))
         (content (concat
                   "The LLM is requesting permission to execute a bash command.\n\n"
                   (propertize "Command: " 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n\n" command) 'font-lock-face 'font-lock-string-face)
                   (when commands
                     (concat
                      (propertize "Detected commands: " 'font-lock-face 'font-lock-escape-face)
                      (propertize (mapconcat #'identity commands ", ")
                                  'font-lock-face 'font-lock-constant-face)
                      "\n\n"))
                   (when unparseable
                     (propertize "\u26a0 Warning: Command contains complex syntax that could not be fully parsed.\n\n"
                                 'font-lock-face 'warning)))))
    (mevedel--prompt-user-with-overlay
     "Bash Command Execution Request"
     content
     "Execute this command?"
     (concat "Bash command execution: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding)))))


;;
;;; Command Execution

(defcustom mevedel-bash-permissions
  '(;; Default: ask for everything not explicitly allowed/denied
    ("*" . ask)

    ;; File inspection (read-only)
    ("ls*" . allow)
    ("cat*" . allow)
    ("head*" . allow)
    ("tail*" . allow)
    ("less*" . allow)
    ("more*" . allow)
    ("file*" . allow)
    ("stat*" . allow)
    ("wc*" . allow)
    ("du*" . allow)
    ("df*" . allow)

    ;; Directory operations (read-only)
    ("pwd*" . allow)
    ("cd*" . allow)

    ;; Text processing (read-only)
    ("grep*" . allow)
    ("egrep*" . allow)
    ("fgrep*" . allow)
    ("rg*" . allow)
    ("ag*" . allow)
    ("awk*" . allow)
    ("cut*" . allow)
    ("sort*" . allow)
    ("uniq*" . allow)
    ("tr*" . allow)
    ("diff*" . allow)

    ;; File search (read-only)
    ("find*" . allow)
    ("which*" . allow)
    ("whereis*" . allow)
    ("type*" . allow)

    ;; Version control (read operations)
    ("git status*" . allow)
    ("git log*" . allow)
    ("git diff*" . allow)
    ("git show*" . allow)
    ("git branch*" . allow)
    ("git tag*" . allow)
    ("git remote*" . allow)
    ("git ls-files*" . allow)
    ("git config --get*" . allow)
    ("git config --list*" . allow)

    ;; Process inspection (read-only)
    ("ps*" . allow)
    ("pgrep*" . allow)

    ;; System information (read-only)
    ("uname*" . allow)
    ("hostname*" . allow)
    ("whoami*" . allow)
    ("id*" . allow)
    ("date*" . allow)
    ("uptime*" . allow)
    ("printenv*" . allow)

    ;; Echo (safe output)
    ("echo*" . allow)
    ("printf*" . allow))
  "Permission settings for bash commands.
Each entry is (PATTERN . ACTION) where PATTERN is a shell glob and
ACTION is one of the symbols `allow`, `deny`, or `ask'. Later entries
override earlier ones.

This default configuration allows common read-only operations used in
development workflows. Dangerous commands are still caught by
`mevedel-bash-dangerous-commands' even if patterns would allow them.

IMPORTANT: Put specific patterns LAST since later entries override
earlier ones. Example: ((\"*\" . deny) (\"ls*\" . allow)) denies
everything except ls."
  :type '(repeat (cons (string :tag "Glob pattern")
                       (choice :tag "Action" (const allow) (const deny) (const ask))))
  :group 'mevedel)

(defcustom mevedel-bash-dangerous-commands
  '("rm" "sudo" "dd" "mkfs" "fdisk" "parted"
    "chmod" "chown" "chgrp" "chattr"
    "kill" "pkill" "killall"
    "curl" "wget" "nc" "ncat" "telnet"
    "ssh" "scp" "rsync" "sftp"
    "iptables" "systemctl" "service"
    "reboot" "shutdown" "poweroff" "halt")
  "Commands that always require explicit confirmation.
Even if a pattern in `mevedel-bash-permissions' would allow these
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

(defun mevedel-tools--permission-action (command permissions)
  "Return the action for COMMAND given PERMISSIONS.
Returns (ACTION . MATCHED-PATTERN) cons cell."
  (let ((action 'ask)
        (matched-pattern nil))
    (dolist (entry permissions)
      (let ((pattern (car entry))
            (value (let ((val (cdr entry)))
                     (cond
                      ((memq val '(allow deny ask)) val)
                      ((and (stringp val) (not (string-empty-p val)))
                       (pcase (intern (downcase val))
                         ('allow 'allow)
                         ('deny 'deny)
                         ('ask 'ask)
                         (_ 'ask)))
                      (t 'ask)))))
        (when (and (stringp pattern)
                   (mevedel-tools--match-pattern pattern command))
          (setq action value)
          (setq matched-pattern pattern))))
    (cons action matched-pattern)))

(defun mevedel-tools--match-pattern (pattern command)
  "Return non-nil when COMMAND matches shell glob PATTERN."
  (condition-case nil
      (string-match-p (wildcard-to-regexp pattern) command)
    (error nil)))

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

(defun mevedel-tools--remove-substitutions (str)
  "Remove command substitutions from STR, replacing with placeholder.
Returns cleaned string with substitutions removed."
  (let ((result str))
    ;; Remove $(...) - use simple regex replacement
    (while (string-match "\\$(([^)]*)" result)
      (setq result (replace-match "__SUBST__" t t result)))

    ;; Remove backticks
    (while (string-match "`[^`]*`" result)
      (setq result (replace-match "__SUBST__" t t result)))

    result))

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

(cl-defun mevedel-tools--check-bash-permission (command)
  "Check if COMMAND is allowed based on permission rules.
Extracts all commands from COMMAND string (including commands in chains,
pipes, and substitutions) and checks each against permission rules and
the dangerous command blocklist.

Returns one of the symbols:
- `allow': Command is allowed to execute
- `deny': Command is denied
- `ask': User should be prompted for confirmation"
  (let* ((extraction (mevedel-tools--extract-commands command))
         (commands (car extraction))
         (unparseable (cdr extraction)))

    ;; If unparseable and fail-safe is enabled, always ask
    (when (and unparseable mevedel-bash-fail-safe-on-complex-syntax)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    ;; If no commands were extracted, ask for safety
    (when (null commands)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

    ;; Check the full command string first
    (let* ((full-result (mevedel-tools--permission-action command mevedel-bash-permissions))
           (full-action (car full-result))
           (full-pattern (cdr full-result))
           ;; Check if command contains shell operators (chains, pipes, etc.)
           (has-operators (string-match-p "&&\\|||\\||\\|;\\|\n" command))
           ;; Specific match: non-generic pattern AND no shell operators
           (specific-match (and full-pattern
                                (not (member full-pattern '("*" "**")))
                                (not has-operators))))

      ;; If full command matched a SPECIFIC pattern AND has no operators, trust
      ;; that match (this handles "git status", "git log args", etc.)
      (if specific-match
          ;; Specific match: only check dangerous blocklist, don't check
          ;; extracted commands
          (if (and (eq full-action 'allow)
                   (seq-some (lambda (cmd) (member cmd mevedel-bash-dangerous-commands))
                             commands))
              'ask
            full-action)

        ;; Otherwise: check ALL extracted commands for defense-in-depth
        (let ((actions (list full-action)))
          ;; Check each extracted command against patterns
          (dolist (cmd commands)
            (push (car (mevedel-tools--permission-action cmd mevedel-bash-permissions)) actions))

          ;; Apply dangerous command blocklist
          (when (seq-some (lambda (cmd) (member cmd mevedel-bash-dangerous-commands))
                          commands)
            (push 'ask actions))

          ;; Combine with precedence: deny > ask > allow
          (cond
           ((memq 'deny actions) 'deny)
           ((memq 'ask actions) 'ask)
           (t 'allow)))))))


;;
;;; Bash permission adapter

(defun mevedel-tool-exec--check-permission (_tool-struct input)
  "Check permission for a Bash tool invocation.

Adapter for the unified permission system.  Extracts the :command
from INPUT and delegates to `mevedel-tools--check-bash-permission'.
Returns `allow', `deny', `ask', or nil."
  (when-let* ((command (plist-get input :command)))
    (mevedel-tools--check-bash-permission command)))


;;
;;; Bash

(cl-defun mevedel-tools--execute-bash (callback command)
  "Execute a bash command and return its output.

CALLBACK is the async callback function to call with results.
COMMAND is the bash command string to execute."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--execute-bash (command stringp))

  ;; Check permissions
  (let ((permission (mevedel-tools--check-bash-permission command)))
    (cond
     ;; Denied by permission rules
     ((eq permission 'deny)
      (cl-return-from mevedel-tools--execute-bash
        (funcall callback (format "Error: Command denied by permission rules: %s" command))))

     ;; Ask user for confirmation with overlay
     ((eq permission 'ask)
      (let ((result (mevedel--prompt-user-for-bash-command command)))
        (unless (eq result t)
          (cl-return-from mevedel-tools--execute-bash
            (if (consp result)
                (funcall callback
                         (format "Error: Command execution cancelled by user. Feedback: %s"
                                 (cdr result)))
              (funcall callback "Error: Command execution cancelled by user")
              (mevedel-abort))))))

     ;; Allow - proceed with execution
     ((eq permission 'allow)
      nil))) ; continue to execution

  ;; Execute command
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
                                   (output (with-current-buffer (process-buffer process)
                                             (buffer-string))))
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
     nil)))


;;
;;; Tool registration

(defun mevedel-tool-exec--register ()
  "Register Bash and Eval tools."

  (gptel-make-tool
   :name "Bash"
   :function #'mevedel-tools--execute-bash
   :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or
equivalents) available. Use this to inspect system state, run builds,
tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `Read`, `Write`, `Edit`, `Glob`,
`Grep`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `Bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'

The command will be executed in the current working directory. Output is
returned as a string. Long outputs should be filtered/limited using
pipes.

### When to use `Bash`

- System commands: git, make, compiler commands, etc.
- Commands that truly require shell execution
- Running tests or builds

### When NOT to use `Bash`

- File operations -> use dedicated file tools instead
- Finding files -> use `Glob`
- Searching contents -> use `Grep`
- Reading files -> use `Read`
- Editing files -> use `Edit`
- Writing files -> use `Write`
- Communication with user -> output text directly

### How to use `Bash`

- Commands execute in the workspace root directory
- Quote file paths with spaces using double quotes
- Chain dependent commands with && (or ; if failures are OK)

### Examples of good usage

<example>
- Building the project:
Bash(command=\"make build && make test\")
</example>

<example>
- Checking git status and staging changes:
Bash(command=\"git status && git add .\")
</example>

### Examples of bad usage

<example>
- Using echo for communication:
Bash(command=\"echo 'Processing complete'\")
<reasoning>
Should output text directly instead of using bash echo.
</reasoning>
</example>

<example>
- Reading file contents:
Bash(command=\"cat config.yml\")
<reasoning>
Should use Read tool instead for better integration.
</reasoning>
</example>
"
   :args '((:name "command"
            :type string
            :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
   :async t
   :confirm nil  ;; Permission checking handled by mevedel-tools--check-bash-permission
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "Eval"
   :function
   (lambda (expression)
     (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
           (result nil) (output nil))
       (unwind-protect
           (condition-case err
               (progn
                 (setq result (eval (read expression) t))
                 (when (> (buffer-size standard-output) 0)
                   (setq output (with-current-buffer standard-output (buffer-string))))
                 (concat
                  (format "Result:\n%S" result)
                  (and output (format "\n\nSTDOUT:\n%s" output))))
             ((error user-error)
              (concat
               (format "Error: eval failed with error %S: %S"
                       (car err) (cdr err))
               (and output (format "\n\nSTDOUT:\n%s" output)))))
         (kill-buffer standard-output))))
   :description "Evaluate Elisp `expression` and return result and any printed output.

`expression` can be anything to evaluate. It can be a function call, a
variable, a quasi-quoted expression. The only requirement is that only
the first sexp will be read and evaluated, so if you need to evaluate
multiple expressions, make one call per expression. Do not combine
expressions using `progn` etc. Just go expression by expression and try to
make standalone single expressions.

Instead of saying \"I can't calculate that\" etc, use this tool to
evaluate the result.

The return value is formated to a string using `%S`, so a string will be
returned as an escaped embedded string and literal forms will be
compatible with `read` where possible. Some forms have no printed
representation that can be read and will be represented with
`#<hash-notation>` instead.

Output from `print`, `prin1`, and `princ` is captured and returned as
STDOUT. Use `print` for diagnostic output, not `message` (which goes to
`*Messages*` buffer and is not captured).

### When to use `Eval`

- Testing elisp code snippets or expressions
- Verifying code changes work correctly
- Checking variable values or function behavior
- Demonstrating elisp functionality to users
- Calculating results instead of saying \"I can't calculate that\"
- Quickly changing user settings or checking configuration
- Exploring Emacs state or testing hypotheses

### When NOT to use `Eval`

- Multi-expression evaluations -> make one call per expression (no progn)
- Complex code that requires multiple statements -> break into individual
  expressions
- When you need to modify files -> use `Edit` instead
- For bash/shell operations -> use `Bash`

### How to use `Eval`

- Provide a single elisp expression as a string
- Can be function calls, variables, quasi-quoted expressions, or any
  valid elisp
- Only the first sexp will be read and evaluated
- Return values are formatted using `%S` (strings appear escaped, literals
  are `read`-compatible)
- Some objects without printed representation show as `#<hash-notation>`
- Make one call per expression - don't combine with progn
- Use for quick settings changes, variable checks, or demonstrations

### Examples of good usage

<example>
- Calculate sum
Eval(expression=\"(+ 1 2 3 4)\")
</example>

<example>
- Check current buffers
Eval(expression=\"(buffer-list)\")
</example>

<example>
- Change setting
Eval(expression=\"(setq tab-width 4)\")
</example>

### Examples of bad usage

<example>
Eval(expression=\"(progn (message \\\"hello\\\") (message \\\"world\\\"))\")
<reasoning>
Should make two separate Eval calls instead of using progn.
</reasoning>
</example>

<example>
Eval(expression=\"(find-file \\\"/path/to/file.txt\\\") ; Then try to edit\")
<reasoning>
Use Edit tool for file modifications, not Eval.
</reasoning>
</example>
"
   :args '(( :name "expression"
             :type string
             :description "A single elisp sexp to evaluate."))
   :category "mevedel"
   :confirm t
   :include t))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
