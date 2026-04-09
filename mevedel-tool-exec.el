;;; mevedel-tool-exec.el -- Bash and Eval tools -*- lexical-binding: t -*-

;;; Commentary:

;; Bash command execution with permission system and Eval tool for elisp
;; evaluation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline"
                  (tool callback args))
(declare-function mevedel-pipeline--positional-to-plist "mevedel-pipeline"
                  (arg-values arg-specs))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-register "mevedel-tool-registry")

;; `mevedel-tool-ui'
(declare-function mevedel--prompt-user-with-overlay "mevedel-tool-ui" (title content question &optional help-echo-text))


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
;;; Eval Prompt UI

(defcustom mevedel-eval-expression-display-limit 20
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel--prompt-user-for-eval (expression)
  "Prompt user for permission to evaluate EXPRESSION.
Returns one of:
- t if approved
- nil if denied
- (feedback . TEXT) if user provides feedback

Displays an overlay showing the expression.  Long expressions are
truncated and can be toggled with TAB."
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
                   (propertize "Expression:\n" 'font-lock-face 'font-lock-escape-face)
                   (propertize (format "%s\n\n" display-expr)
                               'font-lock-face 'font-lock-string-face))))
    (mevedel--prompt-user-with-overlay
     "Eval Expression Request"
     content
     "Evaluate this expression?"
     (concat "Eval expression: "
             (propertize "Keys: C-c C-c approve  C-c C-k deny  f feedback"
                         'face 'help-key-binding)))))


;;
;;; Eval permission adapter

(defun mevedel-tool-exec--eval-check-permission (_tool-struct input)
  "Check permission for an Eval tool invocation.

Always prompts the user with the expression to evaluate.  Elisp is
Turing-complete, so pattern-based analysis is not meaningful.

Returns `allow' or `deny'.  Never returns `ask' -- the Eval-specific
prompt handles that case directly."
  (when-let* ((expression (plist-get input :expression)))
    (let ((result (mevedel--prompt-user-for-eval expression)))
      (cond
       ((eq result t) 'allow)
       ((consp result)
        (signal 'mevedel-permission-denied
                (list (format "Eval cancelled by user. Feedback: %s"
                              (cdr result)))))
       (t 'deny)))))


;;
;;; Bash Prompt UI

(defun mevedel-tool-exec--check-permission (_tool-struct input)
  "Check permission for a Bash tool invocation.

Adapter for the unified permission system.  Extracts the :command
from INPUT and delegates to `mevedel-tools--check-bash-permission'.

When the pattern-based check returns `ask', shows the Bash-specific
prompt overlay (command text, extracted sub-commands, complex syntax
warnings) via `mevedel--prompt-user-for-bash-command' and resolves
to `allow' or `deny'.

Returns `allow', `deny', or nil.  Never returns `ask' -- the
Bash-specific prompt handles that case directly."
  (when-let* ((command (plist-get input :command)))
    (let ((decision (mevedel-tools--check-bash-permission command)))
      (if (eq decision 'ask)
          (let ((result (mevedel--prompt-user-for-bash-command command)))
            (cond
             ((eq result t) 'allow)
             ((consp result)
              (signal 'mevedel-permission-denied
                      (list (format "Command cancelled by user. Feedback: %s"
                                    (cdr result)))))
             (t 'deny)))
        decision))))


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
                  (setq output (with-current-buffer standard-output
                                 (buffer-string))))
                (funcall callback
                         (concat
                          (format "Result:\n%S" result)
                          (and output (format "\n\nSTDOUT:\n%s" output)))))
            ((error user-error)
             (funcall callback
                      (concat
                       (format "Error: Eval failed with error %S: %S"
                               (car err) (cdr err))
                       (and output (format "\n\nSTDOUT:\n%s" output))))))
        (kill-buffer standard-output)))))


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
    :check-permission #'mevedel-tool-exec--check-permission)

  (mevedel-define-tool
    :name "Eval"
    :description "Evaluate an Elisp expression and return the result."
    :prompt-file "tools/eval.md"
    :handler #'mevedel-tool-exec--eval
    :args ((expression string :required "A single elisp sexp to evaluate."))
    :async-p t
    :check-permission #'mevedel-tool-exec--eval-check-permission))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
