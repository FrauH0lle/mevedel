;;; niwa-bash-policy.el --- Argument-aware Bash command policies -*- lexical-binding: t -*-

;;; Commentary:

;; Owns conservative Bash classification, reusable permission patterns, and
;; argument matching.  Permission adapters decide and persist authority; this
;; module supplies Bash-specific policy facts.

;;; Code:

(require 'cl-lib)
(require 'niwa-agents)
(require 'niwa-bash-analysis)
(require 'niwa-execution-target)
(require 'niwa-permission-mode)
(require 'niwa-permission-persistence)
(require 'niwa-permission-rules)
(require 'niwa-permissions)
(require 'niwa-sandbox)
(require 'niwa-structs)
(require 'seq)
(require 'subr-x)

;; `niwa-agents'
(declare-function niwa-agent-invocation-skill-permission-rules
                  "niwa-agents" (cl-x) t)
(declare-function niwa-plan-read-only-p "niwa-agents"
                  (&optional session))
(defvar niwa--agent-invocation)

;; `niwa-bash-analysis'
(declare-function niwa-bash-analysis-analyze
                  "niwa-bash-analysis" (source))
(defvar niwa-bash-dangerous-commands)

;; `niwa-bash-git'
(declare-function niwa-bash-git-read-only-p "niwa-bash-git" (argv))
(autoload 'niwa-bash-git-read-only-p "niwa-bash-git")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-expand-path
                  "niwa-execution-target" (target path &optional directory))
(declare-function niwa-execution-target-native-path
                  "niwa-execution-target" (target path))

;; `niwa-permission-mode'
(defvar niwa-permission-mode)

;; `niwa-permission-persistence'
(declare-function niwa-permission-persistence-load-rules
                  "niwa-permission-persistence" (workspace))
(declare-function niwa-permission-persistence-load-safe-env-vars
                  "niwa-permission-persistence" (workspace))

;; `niwa-permission-rules'
(declare-function niwa-permission-protected-path-policy
                  "niwa-permission-rules" ())
(declare-function niwa-permission-rules-action
                  "niwa-permission-rules" (rules tool-name &rest keys))
(declare-function niwa-permission-rules-collect-buckets
                  "niwa-permission-rules"
                  (invocation-rules request-rules
                                    session-rules persistent-rules))
(declare-function niwa-permission-rules-first-action-with-bucket
                  "niwa-permission-rules"
                  (buckets tool-name path pattern domain name))
(declare-function niwa-permission-rules-path-in-allowed-roots-p
                  "niwa-permission-rules" (path roots))
(declare-function niwa-permission-rules-path-protected-p
                  "niwa-permission-rules" (path &optional target access))
(declare-function niwa-permission-rules-resource-granted-p
                  "niwa-permission-rules"
                  (path access grants &optional recursive))
(declare-function niwa-permission-rules-strip-safe-assignments
                  "niwa-permission-rules" (command names))
(defvar niwa-permission-rules-builtin-safe-env-vars)

;; `niwa-sandbox'
(declare-function niwa-sandbox-mode-effective
                  "niwa-sandbox" (&optional session permission-mode))
(defvar niwa-sandbox-intrinsic-paths)

;; `niwa-structs'
(declare-function niwa-request-skill-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-execution-target
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-mode
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-permission-rules
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-working-directory
                  "niwa-structs" (cl-x) t)
(declare-function niwa-session-workspace
                  "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--workspace)

;; `seq'
(declare-function seq-filter "seq" (pred seq))


;;
;;; Policies

(defconst niwa-bash-policy--simple-read-only-commands
  '("cat" "cd" "cut" "echo" "expr" "false" "grep" "head" "id" "ls"
    "nl" "paste" "pwd" "rev" "seq" "stat" "tail" "tr" "true" "uname"
    "wc" "which" "whoami")
  "Commands whose argument handling does not introduce child effects.")

(defconst niwa-bash-policy--unsafe-find-options
  '("-delete" "-exec" "-execdir" "-fls" "-fprint" "-fprint0"
    "-fprintf" "-ok" "-okdir")
  "Find options that mutate, execute commands, or write files.")

(defun niwa-bash-policy--base64-read-only-p (argv)
  "Return non-nil when base64 ARGV has no output-file option."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("-o" "--output"))
          (string-prefix-p "--output=" argument)
          (and (string-prefix-p "-o" argument)
               (not (string-equal argument "-o")))))
    (cdr argv))))

(defun niwa-bash-policy--find-read-only-p (argv)
  "Return non-nil when find ARGV has no effectful primary."
  (not
   (cl-some
    (lambda (argument)
      (member argument niwa-bash-policy--unsafe-find-options))
    (cdr argv))))

(defun niwa-bash-policy--rg-read-only-p (argv)
  "Return non-nil when ripgrep ARGV cannot launch helper programs."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("--hostname-bin" "--pre" "--search-zip" "-z"))
          (string-prefix-p "--hostname-bin=" argument)
          (string-prefix-p "--pre=" argument)))
    (cdr argv))))

(defun niwa-bash-policy--uniq-read-only-p (argv)
  "Return non-nil for supported uniq ARGV writing only to standard output.
Recognize ordinary GNU flags, numeric option values and optional group methods.
Allow at most INPUT and a literal `-' OUTPUT.  Options must precede operands:
POSIXLY_CORRECT can turn an option after INPUT into an output filename.  Unknown
options, missing values and contradictory grouping modes remain unclassified."
  (and (consp argv)
       (proper-list-p argv)
       (equal (car argv) "uniq")
       (cl-every (lambda (word)
                   (and (stringp word) (not (string-match-p "\0" word))))
                 argv)
       (let ((case-fold-search nil)
             (args (cdr argv)) (valid t) operands (modes "") group)
         (while (and valid args)
           (let ((word (pop args)))
             (cond
              ((equal word "--") (setq operands args args nil))
              ((or (equal word "-") (not (string-prefix-p "-" word)))
               (setq operands (cons word args) args nil))
              ((member word '("--count" "--repeated" "--unique" "--ignore-case"
                              "--zero-terminated" "--help" "--version"))
               (setq modes (concat modes (pcase word
                                           ("--count" "c")
                                           ("--repeated" "d")
                                           ("--unique" "u")
                                           (_ "")))))
              ((string-match-p
                "\\`--all-repeated\\(?:=\\(?:none\\|prepend\\|separate\\)\\)?\\'" word)
               (setq modes (concat modes "D")))
              ((string-match-p
                "\\`--group\\(?:=\\(?:separate\\|prepend\\|append\\|both\\)\\)?\\'" word)
               (setq group t))
              ((member word '("--skip-fields" "--skip-chars" "--check-chars"))
               (unless (and args (string-match-p "\\`[+]?[0-9]+\\'" (pop args)))
                 (setq valid nil)))
              ((string-match-p
                "\\`--\\(?:skip-fields\\|skip-chars\\|check-chars\\)=[+]?[0-9]+\\'" word))
              ((string-match "\\`-\\([cduDiz]*\\)\\(?:[fsw]\\([+]?[0-9]+\\)?\\)?\\'" word)
               (setq modes (concat modes (match-string 1 word)))
               (when (string-match-p "[fsw]\\'" word)
                 (unless (and args (string-match-p "\\`[+]?[0-9]+\\'" (pop args)))
                   (setq valid nil))))
              (t (setq valid nil)))))
         (and valid
              (<= (length operands) 2)
              (or (< (length operands) 2) (equal (cadr operands) "-"))
              (not (and group (string-match-p "[cduD]" modes)))
              (not (and (string-match-p "c" modes)
                        (string-match-p "D" modes)))))))

(defun niwa-bash-policy--sed-address-p (program)
  "Return non-nil when PROGRAM is one numeric print address."
  (and program
       (string-match-p "\\`[0-9]+\\(?:,[0-9]+\\)?p\\'" program)))

(defun niwa-bash-policy--sed-read-only-p (argv)
  "Return non-nil when sed ARGV is the recognized numeric print form."
  (and (<= 3 (length argv))
       (<= (length argv) 4)
       (string-equal (nth 1 argv) "-n")
       (niwa-bash-policy--sed-address-p (nth 2 argv))
       (or (= (length argv) 3)
           (string-equal (nth 3 argv) "-")
           (not (string-prefix-p "-" (nth 3 argv))))))

(defun niwa-bash-policy--awk-read-only-p (argv)
  "Return non-nil when AWK ARGV uses one effect-free inline program."
  (let ((program (nth 1 argv)))
    (and (<= 2 (length argv))
         program
         (not (string-prefix-p "-" program))
         (not (cl-some
               (lambda (argument)
                 (string-prefix-p "-" argument))
               (cddr argv)))
         (not (string-match-p
               "\\(?:@[[:space:]]*[[:alpha:]_]\\|\\_<\\(?:close\\|getline\\|system\\)\\_>\\|[>|]\\)"
               program)))))

(defun niwa-bash-policy--inspection-options-p (args flags values short)
  "Validate inspection ARGS against FLAGS, VALUES and SHORT options.
FLAGS are complete long options without operands; VALUES take an operand.
SHORT is a regexp matching one entire short option (possibly with its value).
After `--', remaining words are literal operands."
  (let ((valid t))
    (while (and valid args)
      (let ((word (pop args)))
        (cond
         ((equal word "--") (setq args nil))
         ((or (equal word "-") (not (string-prefix-p "-" word))))
         ((member word flags))
         ((member word values)
          (if args (pop args) (setq valid nil)))
         ((cl-some (lambda (option)
                     (string-prefix-p (concat option "=") word))
                   values))
         ((and short (string-match-p short word)))
         (t (setq valid nil)))))
    valid))

(defun niwa-bash-policy--inspection-read-only-p (argv)
  "Return non-nil for supported file-inspection ARGV.
These GNU utility options only read inputs and emit to standard output;
unrecognized variants remain unknown rather than inheriting name authority."
  (pcase (car argv)
    ("diff"
     (niwa-bash-policy--inspection-options-p
      (cdr argv)
      '("--normal" "--brief" "--report-identical-files" "--context"
        "--unified" "--ed" "--rcs" "--side-by-side" "--left-column"
        "--suppress-common-lines" "--show-c-function" "--recursive"
        "--new-file" "--unidirectional-new-file" "--ignore-case"
        "--ignore-all-space" "--ignore-space-change" "--ignore-blank-lines"
        "--strip-trailing-cr" "--no-dereference" "--text" "--binary"
        "--minimal" "--speed-large-files" "--help" "--version")
      '("--context" "--unified" "--width" "--label" "--exclude"
        "--exclude-from" "--from-file" "--to-file" "--ignore-matching-lines")
      "\\`-\\(?:[qscuenypPrNaiwbBtTd]+\\|[CUWILxXSF]\\(?:.*\\)\\)\\'"))
    ("cmp"
     (niwa-bash-policy--inspection-options-p
      (cdr argv) '("--silent" "--quiet" "--verbose" "--print-bytes"
                   "--help" "--version")
      '("--ignore-initial" "--bytes")
      "\\`-\\(?:[slb]+\\|[in].*\\)\\'"))
    ((or "readlink" "realpath")
     (niwa-bash-policy--inspection-options-p
      (cdr argv)
      '("--canonicalize" "--canonicalize-existing" "--canonicalize-missing"
        "--logical" "--physical" "--no-symlinks" "--strip" "--zero"
        "--no-newline" "--quiet" "--silent" "--verbose" "--help" "--version")
      '("--relative-to" "--relative-base") "\\`-[efmLPsnqvz]+\\'"))
    ("du"
     (niwa-bash-policy--inspection-options-p
      (cdr argv)
      '("--all" "--apparent-size" "--bytes" "--total" "--human-readable"
        "--si" "--count-links" "--dereference" "--no-dereference"
        "--separate-dirs" "--summarize" "--one-file-system" "--null"
        "--help" "--version")
      '("--block-size" "--max-depth" "--exclude" "--exclude-from"
        "--files0-from" "--threshold" "--time" "--time-style")
      "\\`-\\(?:[abcDhHklLmPsSx0]+\\|[BdXt].*\\)\\'"))))

(defun niwa-bash-policy--test-read-only-p (argv)
  "Return non-nil for literal file/string/integer test ARGV.
Exclude variable lookup and arithmetic expressions that Bash can evaluate
recursively, even when the original shell word is quoted."
  (let ((args (cdr argv)))
    (when (equal (car argv) "[")
      (setq args (and (equal (car (last args)) "]") (butlast args))))
    (and (or (equal (car argv) "test") (equal (car (last argv)) "]"))
         (pcase (length args)
           ((or 0 1) t)
           (2 (or (member (car args)
                          '("-b" "-c" "-d" "-e" "-f" "-g" "-h" "-k"
                            "-L" "-p" "-r" "-s" "-S" "-u" "-w" "-x"
                            "-O" "-G" "-N" "-n" "-z"))
                  (and (equal (car args) "-t")
                       (string-match-p "\\`[0-9]+\\'" (cadr args)))))
           (3 (or (member (cadr args) '("=" "==" "!=" "-ef" "-nt" "-ot"))
                  (and (member (cadr args) '("-eq" "-ne" "-lt" "-le" "-gt" "-ge"))
                       (cl-every (lambda (word)
                                   (string-match-p "\\`[+-]?[0-9]+\\'" word))
                                 (list (car args) (caddr args))))))))))


;;
;;; Public interface

;;;###autoload
(defun niwa-bash-policy-read-only-p (argv)
  "Return non-nil when parsed command ARGV has a read-only built-in policy."
  (let ((command (car argv)))
    (and command
         (not (string-match-p "/" command))
         (cond
          ((member command niwa-bash-policy--simple-read-only-commands) t)
          ((string-equal command "git")
           (niwa-bash-git-read-only-p argv))
          ((member command '("diff" "cmp" "readlink" "realpath" "du"))
           (niwa-bash-policy--inspection-read-only-p argv))
          ((member command '("test" "["))
           (niwa-bash-policy--test-read-only-p argv))
          ((string-equal command "awk")
           (niwa-bash-policy--awk-read-only-p argv))
          ((string-equal command "base64")
           (niwa-bash-policy--base64-read-only-p argv))
          ((string-equal command "find")
           (niwa-bash-policy--find-read-only-p argv))
          ((string-equal command "rg")
           (niwa-bash-policy--rg-read-only-p argv))
          ((string-equal command "sed")
           (niwa-bash-policy--sed-read-only-p argv))
          ((string-equal command "uniq")
           (niwa-bash-policy--uniq-read-only-p argv))))))

(defun niwa-bash-policy--context-directory (permission-context)
  "Return the resource directory captured by PERMISSION-CONTEXT."
  (if permission-context
      (let* ((session (plist-get permission-context :session))
             (session-dir
              (and session
                   (ignore-errors
                     (niwa-session-working-directory session))))
             (directory (plist-get permission-context :execution-directory))
             (root (plist-get permission-context :workspace-root))
             (allowed-root (car (plist-get permission-context :allowed-roots))))
        (file-name-as-directory
         (or directory session-dir root allowed-root
             (error "Permission context has no execution directory"))))
    default-directory))

(defun niwa-bash-policy--bash-resource-paths
    (command &optional analysis permission-context)
  "Return canonical literal resource paths identified in COMMAND.
Dynamic expansions remain complex and are not evaluated.  Reuse ANALYSIS when
the caller already analyzed COMMAND.  PERMISSION-CONTEXT supplies the target."
  (let ((resources
         (plist-get (or analysis (niwa-bash-analysis-analyze command))
                    :resources)))
    (when resources
      (let* ((session (plist-get permission-context :session))
             (base (niwa-bash-policy--context-directory permission-context))
             (target (or (and session
                              (niwa-session-execution-target session))
                         (and (file-remote-p base)
                              (niwa-execution-target-create base)))))
        (mapcar
         (lambda (path)
           (let ((remote-file-name-inhibit-cache t))
             (file-truename
              (if target
                  (niwa-execution-target-expand-path target path base)
                (expand-file-name path base)))))
         resources)))))

(defun niwa-bash-policy-missing-resource-paths
    (command permission-context request)
  "Return COMMAND resources lacking authority under PERMISSION-CONTEXT.
REQUEST may supply exact additive filesystem grants for this invocation."
  (let ((resources
         (and (not (memq (niwa-bash-policy-effective-permission-mode
                          permission-context)
                         '(edits full-auto)))
         (niwa-bash-policy--bash-resource-paths
          command nil permission-context))))
    (when resources
      (let* ((session (plist-get permission-context :session))
             (base (niwa-bash-policy--context-directory permission-context))
             (target (or (and session
                              (niwa-session-execution-target session))
                         (and (file-remote-p base)
                              (niwa-execution-target-create base))))
             (roots (or (plist-get permission-context :allowed-roots)
                        (and-let* ((root (plist-get permission-context
                                                    :workspace-root)))
                          (list root))
                        (list base temporary-file-directory)))
             (roots
              (let ((remote-file-name-inhibit-cache t))
                (mapcar #'file-truename roots)))
             (grants
              (append
               (plist-get permission-context :resource-grants)
               (plist-get (plist-get request :additional-permissions)
                          :file-system)))
             missing)
        (dolist (path resources)
          (unless (or (member (file-local-name path)
                              niwa-sandbox-intrinsic-paths)
                      (niwa-permission-rules-path-in-allowed-roots-p path roots)
                      (niwa-permission-rules-resource-granted-p
                       path 'read grants))
            (push (if target
                      (niwa-execution-target-native-path target path)
                    path)
                  missing)))
        (delete-dups (nreverse missing))))))


;;
;;; Command Execution

(defconst niwa-bash-policy--bash-never-prefix-commands
  '("sh" "bash" "zsh" "fish" "csh" "tcsh" "ksh" "dash"
    "env" "xargs"
    "nice" "stdbuf" "nohup" "timeout" "time"
    "doas" "pkexec" "su")
  "Shells and wrappers that must not be generalized to prefix rules.")


(defun niwa-bash-policy--dedupe-strings (strings)
  "Return STRINGS without duplicates, preserving first occurrence order."
  (delete-dups
   (seq-filter (lambda (string)
                 (and (stringp string) (not (string-empty-p string))))
               strings)))

(defun niwa-bash-policy-commands-summary (commands)
  "Return a counted, first-seen summary string for COMMANDS."
  (when-let* ((unique (niwa-bash-policy--dedupe-strings commands)))
    (string-join
     (mapcar
      (lambda (command)
        (let ((count (cl-count command commands :test #'equal)))
          (if (> count 1)
              (format "%s (%d)" command count)
            command)))
      unique)
     ", ")))

(defun niwa-bash-policy-decision-specifier-value (command)
  "Return sanitized Bash specifier metadata for COMMAND."
  (or (niwa-bash-policy-commands-summary
       (niwa-bash-policy-command-names
        (niwa-bash-analysis-analyze command)))
      "unparseable shell command"))

(defun niwa-bash-policy-command-names (analysis)
  "Return executable names from normalized Bash ANALYSIS."
  (mapcar (lambda (argv) (file-name-nondirectory (car argv)))
          (plist-get analysis :commands)))

(defun niwa-bash-policy--bash-segment-words (segment)
  "Return shell words parsed from SEGMENT, or nil when parsing fails."
  (condition-case nil
      (niwa-bash-analysis--split-command segment)
    (error nil)))

(defun niwa-bash-policy--bash-env-assignment-p (word)
  "Return non-nil when WORD is a leading shell env assignment."
  (and (stringp word)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" word)))

(defun niwa-bash-policy--bash-safe-env-assignment-p (word names)
  "Return non-nil when WORD assigns one of the safe variable NAMES."
  (and (niwa-bash-policy--bash-env-assignment-p word)
       (member (car (split-string word "=" t)) names)))

(defun niwa-bash-policy--bash-strip-safe-env-assignments
    (words permission-context)
  "Strip safe leading env assignments from WORDS.
Safe names come from `niwa-bash-policy-safe-env-vars' under
PERMISSION-CONTEXT.

Return nil if a leading env assignment is not known safe.  This
avoids saving prefix rules that will not match later permission
checks, and avoids hiding environment-controlled behavior behind a
general rule."
  (let ((names (niwa-bash-policy-safe-env-vars permission-context)))
    (catch 'unsafe
      (while (and words
                  (niwa-bash-policy--bash-env-assignment-p (car words)))
        (unless (niwa-bash-policy--bash-safe-env-assignment-p
                 (car words) names)
          (throw 'unsafe nil))
        (setq words (cdr words)))
      words)))

(defun niwa-bash-policy--bash-subcommand-token-p (word)
  "Return non-nil for stable shell subcommand WORD."
  (and (stringp word)
       (string-match-p
        "\\`[[:lower:]][[:lower:][:digit:]]*\\(?:-[[:lower:][:digit:]]+\\)*\\'"
        word)))

(defun niwa-bash-policy--bash-command-never-prefix-p (command)
  "Return non-nil when COMMAND should not get a broad prefix rule.
Executable paths retain the restrictions of their command name."
  (let ((commands (append niwa-bash-dangerous-commands
                          niwa-bash-policy--bash-never-prefix-commands)))
    (or (member command commands)
        (member (file-name-nondirectory command) commands))))

(defun niwa-bash-policy--bash-prefix-for-segment
    (segment &optional permission-context)
  "Return a stable command prefix for Bash SEGMENT, or nil.
PERMISSION-CONTEXT supplies the user's extra safe environment variables.

The heuristic follows Claude Code's low-maintenance shape: derive
`command subcommand' generically, only when the second token looks
like a subcommand rather than a flag, path, file name, or number.
Dangerous commands and shell/wrapper commands are not generalized."
  (let* ((parsed (niwa-bash-policy--bash-segment-words segment))
         (words (and parsed
                     (niwa-bash-policy--bash-strip-safe-env-assignments
                      parsed permission-context)))
         (command (car words))
         (subcommand (cadr words)))
    (when (and command
               subcommand
               ;; Prefix rules are shell text, not argv.  Decoded whitespace
               ;; or shell syntax would change the executable when rejoined.
               (string-match-p "\\`[[:alnum:]_./:@%+=,-]+\\'" command)
               (not (niwa-bash-policy--bash-command-never-prefix-p command))
               (niwa-bash-policy--bash-subcommand-token-p subcommand))
      ;; Include skipped assignments in the source check: quoted `LANG=C'
      ;; names an executable, not an environment assignment.  If decoding
      ;; changes this prefix's spelling, remember the authored segment exactly.
      (let ((source-prefix
             (string-join
              (cl-subseq parsed 0 (+ 2 (- (length parsed) (length words)))) " ")))
        (when (and (string-prefix-p source-prefix segment)
                   (or (= (length source-prefix) (length segment))
                       (memq (aref segment (length source-prefix)) '(?\s ?\t ?\n))))
          (string-join (list command subcommand) " "))))))

(defun niwa-bash-policy--bash-allow-pattern-for-segment
    (segment &optional permission-context)
  "Return the reusable allow pattern suggested for Bash SEGMENT.
PERMISSION-CONTEXT supplies the user's extra safe environment variables.

Simple `command subcommand ...' invocations are generalized to
Claude Code-style prefix rules such as `git log:*'.  Segments that
do not have a stable subcommand, or that start with a dangerous
command/wrapper, stay exact."
  (let* ((trimmed (string-trim segment))
         (prefix (niwa-bash-policy--bash-prefix-for-segment
                  trimmed permission-context)))
    (if prefix
        (concat prefix ":*")
      trimmed)))

(defun niwa-bash-policy-allow-patterns (command &optional permission-context)
  "Return reusable allow patterns to store when approving COMMAND.
Leading safe environment assignments, including the `:safe-env-vars' of
PERMISSION-CONTEXT, are skipped as rule matching skips them.

Compound commands produce one pattern per command segment.  This
avoids saving a brittle whole-chain string such as
`pwd && git log --oneline' when the useful reusable rule is
`git log:*'."
  (niwa-bash-policy--dedupe-strings
   (mapcar (lambda (segment)
             (niwa-bash-policy--bash-allow-pattern-for-segment
              segment permission-context))
           (plist-get (niwa-bash-analysis-analyze command) :segments))))

(defun niwa-bash-policy-reusable-operation-p (command)
  "Return non-nil when COMMAND can be remembered without broadening it."
  (let ((analysis (niwa-bash-analysis-analyze command)))
    (and (not (string-empty-p (string-trim command)))
         (not (plist-get analysis :complex-p))
         (not (plist-get analysis :background-p))
         (not (string-match-p "\\(?:\\*\\|\\?\\|\\[\\)" command)))))

(defun niwa-bash-policy-single-reusable-segment-p (command)
  "Return non-nil when COMMAND is one reusable segment.
Compounds, dynamic syntax, background jobs and glob characters are not."
  (and (niwa-bash-policy-reusable-operation-p command)
       (null (cdr (plist-get (niwa-bash-analysis-analyze command) :segments)))))

(defun niwa-bash-policy-stable-prefix (command)
  "Return COMMAND's literal `executable subcommand' prefix, or nil.
Only one reusable segment has a prefix.  Compounds, dynamic syntax, unsafe
environment assignments, wrappers, dangerous commands and a leading option
have none.  The executable stays literal; paths are not reduced to names."
  (when (niwa-bash-policy-single-reusable-segment-p command)
    (let* ((text (string-trim command))
           (prefix (niwa-bash-policy--bash-prefix-for-segment text)))
      ;; Evidence slices arguments after this prefix.  A shell-word prefix
      ;; with normalized quoting or whitespace is not a literal text prefix.
      (when (and prefix
                 (string-prefix-p prefix text)
                 (or (= (length prefix) (length text))
                     (memq (aref text (length prefix)) '(?\s ?\t ?\n))))
        prefix))))

(defun niwa-bash-policy-effective-permission-mode
    (&optional permission-context)
  "Return effective permission mode for PERMISSION-CONTEXT."
  (let ((session (if permission-context
                     (plist-get permission-context :session)
                   (and (boundp 'niwa--session) niwa--session))))
    (or (and permission-context
             (plist-get permission-context :mode))
        (if permission-context
            (niwa-permission-mode-for-context permission-context)
          (niwa-permission-mode-effective session)))))

(defun niwa-bash-policy-effective-sandbox-mode
    (&optional permission-context)
  "Return effective sandbox mode for PERMISSION-CONTEXT."
  (niwa-sandbox-mode-effective
   (if permission-context
       (plist-get permission-context :session)
     (and (boundp 'niwa--session) niwa--session))
   (niwa-bash-policy-effective-permission-mode permission-context)))

(defun niwa-bash-policy--bash-protected-path-p
    (command &optional analysis permission-context)
  "Return non-nil if COMMAND has an obvious protected path in ANALYSIS.
PERMISSION-CONTEXT supplies the owning execution target."
  (let ((paths (niwa-bash-policy--bash-resource-paths
                command analysis permission-context)))
    (when paths
      (let* ((session (plist-get permission-context :session))
             (base (niwa-bash-policy--context-directory permission-context))
             (target (or (and session
                              (niwa-session-execution-target session))
                         (and (file-remote-p base)
                              (niwa-execution-target-create base)))))
        (cl-some
         (lambda (path)
           (or (niwa-permission-rules-path-protected-p path target)
               ;; Directory roots such as `.git' may be protected by a
               ;; `**/.git/**' policy even when the literal token has no child.
               (niwa-permission-rules-path-protected-p
                (file-name-as-directory path) target)
               (cl-some
                (lambda (name)
                  (and (cl-some (lambda (pattern)
                                  (string-match-p
                                   (concat "\\." (regexp-quote name)
                                           "\\(?:/\\|\\'\\)")
                                   pattern))
                                (mapcar
                                 #'car
                                 (niwa-permission-protected-path-policy)))
                       (string-match-p
                        (concat "\\(?:\\`\\|/\\)\\." (regexp-quote name)
                                "\\(?:/\\|\\'\\)")
                        path)))
                '("git" "ssh" "gnupg"))))
         paths)))))

(defun niwa-bash-policy--bash-deny-candidates (command &optional analysis)
  "Return Bash strings explicit deny rules should check for COMMAND.
Includes the whole command, recognized command-chain segments, and extracted
command names.  Dangerous-name harvesting remains independent, so unsupported
syntax cannot hide a command in `niwa-bash-dangerous-commands'.  Reuse
ANALYSIS when supplied."
  (let ((analysis (or analysis (niwa-bash-analysis-analyze command))))
    (niwa-bash-policy--dedupe-strings
     (append (list command)
             (plist-get analysis :candidates)
             (niwa-bash-policy-command-names analysis)))))

(defun niwa-bash-policy--bash-deny-match-p
    (buckets candidates &optional pattern-only-p)
  "Return non-nil when BUCKETS deny one of CANDIDATES.
When PATTERN-ONLY-P is non-nil, ignore generic fallback rules."
  (cl-some
   (lambda (candidate)
     (cl-some
      (lambda (entry)
        (let ((rules (if pattern-only-p
                         (seq-filter
                          (lambda (rule)
                            (plist-member (cdr rule) :pattern))
                          (cdr entry))
                       (cdr entry))))
          (eq (niwa-permission-rules-action
               rules "Bash" :pattern candidate)
              'deny)))
      buckets))
   candidates))

(defun niwa-bash-policy-explicit-deny-p
    (buckets command &optional analysis)
  "Return non-nil when an effective Bash deny covers COMMAND.
Generic fallback denies are evaluated against the recognized top-level
segments, and against the original command when it is a single segment.  A
compound command as a whole, like harvested nested candidates, is checked
only against pattern rules: allow wildcards never span a separator, so a
generic fallback would otherwise deny every compound whose segments are each
allowed.  ANALYSIS is the normalized result for COMMAND when already known."
  (let* ((analysis (or analysis (niwa-bash-analysis-analyze command)))
         (segments (plist-get analysis :segments))
         (compound-p (cdr segments))
         (harvested (niwa-bash-policy--bash-deny-candidates command analysis)))
    (or (niwa-bash-policy--bash-deny-match-p
         buckets (if compound-p segments (cons command segments)))
        (niwa-bash-policy--bash-deny-match-p buckets harvested t))))

(defun niwa-bash-policy--context-session (permission-context)
  "Return PERMISSION-CONTEXT's session, or the ambient one without a context."
  (if permission-context
      (plist-get permission-context :session)
    (and (boundp 'niwa--session) niwa--session)))

(defun niwa-bash-policy--context-workspace (permission-context)
  "Return PERMISSION-CONTEXT's workspace, or the ambient one without a context."
  (let ((session (niwa-bash-policy--context-session permission-context)))
    (or (and session (niwa-session-workspace session))
        (if permission-context
            (plist-get permission-context :workspace)
          (and (boundp 'niwa--workspace) niwa--workspace)))))

(defun niwa-bash-policy-buckets (&optional permission-context)
  "Return Bash buckets for PERMISSION-CONTEXT, innermost-first.

Includes the request-scoped skill rule buckets so a skill's
`allowed-tools: [Bash(...)]' grants are honored by the Bash
permission check; without this, skill rules silently failed for
the Bash tool path because Bash had its own flattened resolver."
  (if (plist-member permission-context :buckets)
      (plist-get permission-context :buckets)
      (let* ((session (niwa-bash-policy--context-session permission-context))
             (workspace (niwa-bash-policy--context-workspace permission-context))
             (request
              (if permission-context
                  (plist-get permission-context :request)
                (and (boundp 'niwa--current-request)
                     niwa--current-request)))
             (invocation
              (if permission-context
                  (plist-get permission-context :invocation)
                niwa--agent-invocation))
             (invocation-rules
              (and invocation
                   (niwa-agent-invocation-skill-permission-rules
                    invocation)))
             (request-rules
              (and request
                   (niwa-request-skill-permission-rules request)))
             (session-rules (when session
                              (niwa-session-permission-rules session)))
             (persistent (when workspace
                           (niwa-permission-persistence-load-rules
                            workspace))))
        (niwa-permission-rules-collect-buckets
         invocation-rules request-rules session-rules persistent))))

(defun niwa-bash-policy-safe-env-vars (&optional permission-context)
  "Return the variables whose leading assignments rule matching ignores.
These are `niwa-permission-rules-builtin-safe-env-vars' plus the
`:safe-env-vars' of the global and workspace permission stores.  A
PERMISSION-CONTEXT with `:safe-env-vars' supplies the store entries
itself."
  (append
   niwa-permission-rules-builtin-safe-env-vars
   (if (plist-member permission-context :safe-env-vars)
       (plist-get permission-context :safe-env-vars)
     (let ((workspace (niwa-bash-policy--context-workspace permission-context)))
       (and workspace
            (niwa-permission-persistence-load-safe-env-vars workspace))))))

(defun niwa-bash-policy-rule-command (command &optional permission-context)
  "Return COMMAND as permission rules see it under PERMISSION-CONTEXT.
Leading assignments to safe variables are removed from every segment; see
`niwa-bash-policy-safe-env-vars'."
  (niwa-permission-rules-strip-safe-assignments
   command (niwa-bash-policy-safe-env-vars permission-context)))

(defun niwa-bash-policy--segments-covered-p (actions classes)
  "Return non-nil when every segment is allowed or read-only.
ACTIONS and CLASSES hold each segment's rule action and analysis class, in
segment order.  A read-only segment needs no rule."
  (cl-loop for action in actions
           for class in classes
           always (or (eq action 'allow) (eq class 'read-only))))

(defun niwa-bash-policy-profile-segments (command direct-buckets)
  "Return compound COMMAND's segments whose direct profiles it may receive.
Return them only when DIRECT-BUCKETS allow every segment that is not
read-only; otherwise, and for a single command, return nil, so one segment
never inherits another's capability.  COMMAND is already in rule form; see
`niwa-bash-policy-rule-command'."
  (let ((segments (plist-get (niwa-bash-analysis-analyze command) :segments)))
    (when (and (cdr segments)
               (niwa-bash-policy--segments-covered-p
                (mapcar (lambda (segment)
                          (car (niwa-bash-policy--bash-bucket-match
                                direct-buckets segment)))
                        segments)
                (mapcar (lambda (segment)
                          (plist-get (niwa-bash-analysis-analyze segment) :class))
                        segments)))
      segments)))

(defun niwa-bash-policy--bash-bucket-match (buckets command)
  "Return the first non-deny (ACTION . BUCKET) matching COMMAND in BUCKETS."
  (niwa-permission-rules-first-action-with-bucket
   buckets "Bash" nil command nil nil))

(defun niwa-bash-policy--bash-direct-match (buckets command)
  "Return direct user authority matching COMMAND in BUCKETS."
  (niwa-bash-policy--bash-bucket-match
   (seq-filter
    (lambda (entry) (memq (car entry) '(:session :persistent :defcustom)))
    buckets)
   command))


(cl-defun niwa-bash-policy-check-permission
    (command &key permission-context metadata-p)
  "Decide Bash permission for COMMAND and PERMISSION-CONTEXT.

Rules come from invocation, request, session, persistent, and
defcustom buckets (in that innermost-first order) and are
matched via `:pattern'.

Ask uses conservative Bash analysis.  Edits allows arbitrary commands within
required confinement after explicit ask/deny rules.  Full-auto bypasses ordinary
asks and default resource boundaries.  Hard denies and Plan remain effective.

With METADATA-P, return decision metadata with the determining cause instead
of the outcome symbol.

Bucket-aware: delegated invocation and request rules may authorize ordinary
unknown commands, but only session, persistent, and global user rules may
authorize dangerous or complex syntax."
  (let* ((analysis (niwa-bash-analysis-analyze command))
         (class (plist-get analysis :class))
         ;; Denies and mode restrictions see the command as written; rule
         ;; matching and classification ignore safe leading assignments.
         (rule-command (niwa-bash-policy-rule-command
                        command permission-context))
         (rule-analysis (if (equal rule-command command)
                            analysis
                          (niwa-bash-analysis-analyze rule-command)))
         (rule-class (plist-get rule-analysis :class))
         (segments (plist-get rule-analysis :segments))
         (buckets (niwa-bash-policy-buckets permission-context))
         (mode (niwa-bash-policy-effective-permission-mode
                permission-context))
         (full-match (niwa-bash-policy--bash-bucket-match buckets rule-command))
         (direct-match (niwa-bash-policy--bash-direct-match buckets rule-command))
         (segment-matches
          (mapcar (lambda (segment)
                    (niwa-bash-policy--bash-bucket-match buckets segment))
                  segments))
         (segment-actions (mapcar #'car segment-matches))
         (direct-segment-actions
          (mapcar
           (lambda (segment)
             (car (niwa-bash-policy--bash-direct-match buckets segment)))
           segments))
         (segment-classes
          (mapcar
           (lambda (segment)
             (plist-get (niwa-bash-analysis-analyze segment) :class))
           segments)))
    (cl-flet ((decide (outcome via)
                (if metadata-p
                    (niwa-permission--decision outcome via)
                  outcome)))
      (when (niwa-bash-policy-explicit-deny-p buckets command analysis)
        (cl-return-from niwa-bash-policy-check-permission
          (decide 'deny 'rule)))

      (when (and (let ((niwa--current-request
                       (if (plist-member permission-context :request)
                           (plist-get permission-context :request)
                         niwa--current-request))
                      (niwa--agent-invocation
                       (if (plist-member permission-context :invocation)
                           (plist-get permission-context :invocation)
                         niwa--agent-invocation)))
                   ;; Captured nil stamps must not borrow a newer turn's Plan
                   ;; authority.  The owning session's live Plan flag still applies.
                   (niwa-plan-read-only-p
                    (plist-get permission-context :session)))
                 (not (eq class 'read-only)))
        (cl-return-from niwa-bash-policy-check-permission
          (decide 'deny 'plan-mode)))

      (when (eq mode 'full-auto)
        (cl-return-from niwa-bash-policy-check-permission
          (decide 'allow 'mode)))

      (when (and (not (eq mode 'edits))
                 (not (plist-get permission-context
                                 :resource-authority-separated-p))
                 (niwa-bash-policy--bash-protected-path-p
                  command analysis permission-context))
        (cl-return-from niwa-bash-policy-check-permission
          (decide 'ask 'protected-path)))

      (when (and (plist-get permission-context :one-shot-mutations-p)
                 (not (eq class 'read-only)))
        (cl-return-from niwa-bash-policy-check-permission
          (decide 'ask 'one-shot-mutation)))

      (cond
       ((eq (car full-match) 'ask) (decide 'ask 'rule))
       ((memq 'ask segment-actions) (decide 'ask 'rule))
       ((eq mode 'edits) (decide 'allow 'mode))
       ((and (memq rule-class '(dangerous complex))
             (eq (car direct-match) 'allow))
        (decide 'allow 'rule))
       ((and (eq rule-class 'dangerous)
             segments
             (niwa-bash-policy--segments-covered-p
              direct-segment-actions segment-classes))
        (decide 'allow 'rule))
       ((memq rule-class '(dangerous complex))
        (decide 'ask 'mode))
       ((and (memq 'allow segment-actions)
             (niwa-bash-policy--segments-covered-p
              segment-actions segment-classes))
        (decide 'allow 'rule))
       ((eq rule-class 'read-only)
        (decide 'allow 'read-only))
       ((eq (car full-match) 'allow) (decide 'allow 'rule))
       (t (decide 'ask 'mode))))))


(provide 'niwa-bash-policy)

;;; niwa-bash-policy.el ends here
