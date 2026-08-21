;;; mevedel-bash-policy.el --- Argument-aware Bash command policies -*- lexical-binding: t -*-

;;; Commentary:

;; Owns conservative Bash classification, reusable permission patterns, and
;; guardian guidance.  Permission adapters decide and persist authority; this
;; module supplies Bash-specific policy facts.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))
(defvar mevedel-bash-dangerous-commands)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-expand-path
                  "mevedel-execution-target" (target path &optional directory))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-permissions'
(declare-function mevedel-permission--collect-buckets
                  "mevedel-permissions"
                  (invocation-rules request-rules
                                    session-rules persistent-rules))
(declare-function mevedel-permission--find-rules
                  "mevedel-permissions" (rules tool-name &rest keys))
(declare-function mevedel-permission--first-non-nil-action-with-bucket
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--load-persistent-rules
                  "mevedel-permissions" (workspace))
(declare-function mevedel-permission--path-in-allowed-roots-p
                  "mevedel-permissions" (path roots))
(declare-function mevedel-permission--path-protected-p
                  "mevedel-permissions" (path &optional target))
(declare-function mevedel-permission--plan-mode-p
                  "mevedel-permissions" (&optional session))
(declare-function mevedel-permission--qualified-buckets
                  "mevedel-permissions" (buckets qualifier value))
(declare-function mevedel-permission--resource-granted-p
                  "mevedel-permissions" (path access grants))
(declare-function mevedel-permission--rules-action
                  "mevedel-permissions" (rules tool-name &rest keys))
(declare-function mevedel-permission-protected-path-policy
                  "mevedel-permissions" ())
(defvar mevedel-permission-mode)

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-mode-effective
                  "mevedel-sandbox" (&optional session))
(declare-function mevedel-sandbox-pending-facts
                  "mevedel-sandbox"
                  (&optional additional-permissions sandbox-permissions mode
                             workdir))
(declare-function mevedel-sandbox-status-text
                  "mevedel-sandbox" (facts))
(defvar mevedel-sandbox-intrinsic-paths)

;; `mevedel-structs'
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace
                  "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-system'
(declare-function mevedel-system-build-prompt
                  "mevedel-system" (profile &rest keys))

;; `seq'
(declare-function seq-filter "seq" (pred seq))


;;
;;; Policies

(defconst mevedel-bash-policy--simple-read-only-commands
  '("cat" "cd" "cut" "echo" "expr" "false" "grep" "head" "id" "ls"
    "nl" "paste" "pwd" "rev" "seq" "stat" "tail" "tr" "true" "uname"
    "uniq" "wc" "which" "whoami")
  "Commands whose argument handling does not introduce child effects.")

(defconst mevedel-bash-policy--unsafe-find-options
  '("-delete" "-exec" "-execdir" "-fls" "-fprint" "-fprint0"
    "-fprintf" "-ok" "-okdir")
  "Find options that mutate, execute commands, or write files.")

(defun mevedel-bash-policy--base64-read-only-p (argv)
  "Return non-nil when base64 ARGV has no output-file option."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("-o" "--output"))
          (string-prefix-p "--output=" argument)
          (and (string-prefix-p "-o" argument)
               (not (string-equal argument "-o")))))
    (cdr argv))))

(defun mevedel-bash-policy--find-read-only-p (argv)
  "Return non-nil when find ARGV has no effectful primary."
  (not
   (cl-some
    (lambda (argument)
      (member argument mevedel-bash-policy--unsafe-find-options))
    (cdr argv))))

(defun mevedel-bash-policy--rg-read-only-p (argv)
  "Return non-nil when ripgrep ARGV cannot launch helper programs."
  (not
   (cl-some
    (lambda (argument)
      (or (member argument '("--hostname-bin" "--pre" "--search-zip" "-z"))
          (string-prefix-p "--hostname-bin=" argument)
          (string-prefix-p "--pre=" argument)))
    (cdr argv))))

(defun mevedel-bash-policy--sed-address-p (program)
  "Return non-nil when PROGRAM is one numeric print address."
  (and program
       (string-match-p "\\`[0-9]+\\(?:,[0-9]+\\)?p\\'" program)))

(defun mevedel-bash-policy--sed-read-only-p (argv)
  "Return non-nil when sed ARGV is the recognized numeric print form."
  (and (<= 3 (length argv))
       (<= (length argv) 4)
       (string-equal (nth 1 argv) "-n")
       (mevedel-bash-policy--sed-address-p (nth 2 argv))
       (or (= (length argv) 3)
           (string-equal (nth 3 argv) "-")
           (not (string-prefix-p "-" (nth 3 argv))))))

(defun mevedel-bash-policy--awk-read-only-p (argv)
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


;;
;;; Public interface

;;;###autoload
(defun mevedel-bash-policy-read-only-p (argv)
  "Return non-nil when parsed command ARGV has a read-only built-in policy."
  (require 'cl-lib)
  (let ((command (car argv)))
    (and command
         (not (string-match-p "/" command))
         (cond
          ((member command mevedel-bash-policy--simple-read-only-commands) t)
          ((string-equal command "awk")
           (mevedel-bash-policy--awk-read-only-p argv))
          ((string-equal command "base64")
           (mevedel-bash-policy--base64-read-only-p argv))
          ((string-equal command "find")
           (mevedel-bash-policy--find-read-only-p argv))
          ((string-equal command "rg")
           (mevedel-bash-policy--rg-read-only-p argv))
          ((string-equal command "sed")
           (mevedel-bash-policy--sed-read-only-p argv))))))

(defun mevedel-bash-policy--context-directory (permission-context)
  "Return the resource directory captured by PERMISSION-CONTEXT."
  (if permission-context
      (let* ((session (plist-get permission-context :session))
             (session-dir
              (and session
                   (ignore-errors
                     (mevedel-session-working-directory session))))
             (directory (plist-get permission-context :execution-directory))
             (root (plist-get permission-context :workspace-root))
             (allowed-root (car (plist-get permission-context :allowed-roots))))
        (file-name-as-directory
         (or session-dir directory root allowed-root
             (error "Permission context has no execution directory"))))
    default-directory))

(defun mevedel-bash-policy--bash-resource-paths
    (command &optional analysis permission-context)
  "Return canonical literal resource paths identified in COMMAND.
Dynamic expansions remain complex and are not evaluated.  Reuse ANALYSIS when
the caller already analyzed COMMAND.  PERMISSION-CONTEXT supplies the target."
  (let ((resources
         (plist-get (or analysis (mevedel-bash-analysis-analyze command))
                    :resources)))
    (when resources
      (let* ((session (plist-get permission-context :session))
             (base (mevedel-bash-policy--context-directory permission-context))
             (target (or (and session
                              (mevedel-session-execution-target session))
                         (and (file-remote-p base)
                              (mevedel-execution-target-create base)))))
        (mapcar
         (lambda (path)
           (let ((remote-file-name-inhibit-cache t))
             (file-truename
              (if target
                  (mevedel-execution-target-expand-path target path base)
                (expand-file-name path base)))))
         resources)))))

(defun mevedel-bash-policy-missing-resource-paths
    (command permission-context request)
  "Return COMMAND resources lacking authority under PERMISSION-CONTEXT.
REQUEST may supply exact additive filesystem grants for this invocation."
  (require 'mevedel-bash-analysis)
  (require 'mevedel-execution-target)
  (require 'mevedel-permissions)
  (require 'mevedel-sandbox)
  (require 'mevedel-structs)
  (let ((resources
         (mevedel-bash-policy--bash-resource-paths
          command nil permission-context)))
    (when resources
      (let* ((session (plist-get permission-context :session))
             (base (mevedel-bash-policy--context-directory permission-context))
             (target (or (and session
                              (mevedel-session-execution-target session))
                         (and (file-remote-p base)
                              (mevedel-execution-target-create base))))
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
                              mevedel-sandbox-intrinsic-paths)
                      (mevedel-permission--path-in-allowed-roots-p path roots)
                      (mevedel-permission--resource-granted-p
                       path 'read grants))
            (push (if target
                      (mevedel-execution-target-native-path target path)
                    path)
                  missing)))
        (delete-dups (nreverse missing))))))


;;
;;; Command Execution

(defcustom mevedel-permission-guardian nil
  "Whether to annotate Bash permission prompts with risk guidance.

When nil, permission prompts are rendered without guardian guidance.
When t, mevedel asks the current gptel model for advisory-only Bash
risk classification while an `ask' prompt is pending.

A function value is useful for custom classifiers and tests.  It is
called as (FUNCTION COMMAND CONTEXT CALLBACK), where CONTEXT contains
normalized analysis and pending confinement facts, and CALLBACK accepts
either nil or a plist:

  (:risk low|medium|high|critical
   :recommendation proceed|ask|deny
   :reason \"short explanation\")

The result never grants authority or changes deterministic Bash analysis.
Explicit denies, native-edit Goal restrictions, protected-path policy, and
the user's decision remain authoritative."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Use gptel reviewer" t)
                 function)
  :group 'mevedel)

(defcustom mevedel-permission-guardian-timeout 20
  "Seconds to wait before giving up on Bash guardian guidance."
  :type 'number
  :group 'mevedel)

(defconst mevedel-bash-policy--bash-safe-env-vars
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

(defconst mevedel-bash-policy--bash-never-prefix-commands
  '("sh" "bash" "zsh" "fish" "csh" "tcsh" "ksh" "dash"
    "env" "xargs"
    "nice" "stdbuf" "nohup" "timeout" "time"
    "doas" "pkexec" "su")
  "Shells and wrappers that must not be generalized to prefix rules.")


(defun mevedel-bash-policy--dedupe-strings (strings)
  "Return STRINGS without duplicates, preserving first occurrence order."
  (require 'seq)
  (delete-dups
   (seq-filter (lambda (string)
                 (and (stringp string) (not (string-empty-p string))))
               strings)))

(defun mevedel-bash-policy-commands-summary (commands)
  "Return a counted, first-seen summary string for COMMANDS."
  (require 'cl-lib)
  (when-let* ((unique (mevedel-bash-policy--dedupe-strings commands)))
    (string-join
     (mapcar
      (lambda (command)
        (let ((count (cl-count command commands :test #'equal)))
          (if (> count 1)
              (format "%s (%d)" command count)
            command)))
      unique)
     ", ")))

(defun mevedel-bash-policy-decision-specifier-value (command)
  "Return sanitized Bash specifier metadata for COMMAND."
  (require 'mevedel-bash-analysis)
  (or (mevedel-bash-policy-commands-summary
       (mevedel-bash-policy-command-names
        (mevedel-bash-analysis-analyze command)))
      "unparseable shell command"))

(defun mevedel-bash-policy-command-names (analysis)
  "Return executable names from normalized Bash ANALYSIS."
  (mapcar (lambda (argv) (file-name-nondirectory (car argv)))
          (plist-get analysis :commands)))

(defun mevedel-bash-policy--bash-segment-words (segment)
  "Return shell words parsed from SEGMENT, or nil when parsing fails."
  (condition-case nil
      (split-string-and-unquote segment)
    (error nil)))

(defun mevedel-bash-policy--bash-env-assignment-p (word)
  "Return non-nil when WORD is a leading shell env assignment."
  (and (stringp word)
       (string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" word)))

(defun mevedel-bash-policy--bash-safe-env-assignment-p (word)
  "Return non-nil when WORD is a safe env assignment for prefix matching."
  (and (mevedel-bash-policy--bash-env-assignment-p word)
       (member (car (split-string word "=" t))
               mevedel-bash-policy--bash-safe-env-vars)))

(defun mevedel-bash-policy--bash-strip-safe-env-assignments (words)
  "Strip safe leading env assignments from WORDS.

Return nil if a leading env assignment is not known safe.  This
avoids saving prefix rules that will not match later permission
checks, and avoids hiding environment-controlled behavior behind a
general rule."
  (catch 'unsafe
    (while (and words
                (mevedel-bash-policy--bash-env-assignment-p (car words)))
      (unless (mevedel-bash-policy--bash-safe-env-assignment-p (car words))
        (throw 'unsafe nil))
      (setq words (cdr words)))
    words))

(defun mevedel-bash-policy--bash-subcommand-token-p (word)
  "Return non-nil for stable shell subcommand WORD."
  (and (stringp word)
       (string-match-p
        "\\`[[:lower:]][[:lower:][:digit:]]*\\(?:-[[:lower:][:digit:]]+\\)*\\'"
        word)))

(defun mevedel-bash-policy--bash-command-never-prefix-p (command)
  "Return non-nil when COMMAND should not get a broad prefix rule."
  (member command
          (append mevedel-bash-dangerous-commands
                  mevedel-bash-policy--bash-never-prefix-commands)))

(defun mevedel-bash-policy--bash-prefix-for-segment (segment)
  "Return a stable command prefix for Bash SEGMENT, or nil.

The heuristic follows Claude Code's low-maintenance shape: derive
`command subcommand' generically, only when the second token looks
like a subcommand rather than a flag, path, file name, or number.
Dangerous commands and shell/wrapper commands are not generalized."
  (let* ((words (mevedel-bash-policy--bash-segment-words segment))
         (words (and words
                     (mevedel-bash-policy--bash-strip-safe-env-assignments
                      words)))
         (command (car words))
         (subcommand (cadr words)))
    (when (and command
               subcommand
               (not (mevedel-bash-policy--bash-command-never-prefix-p command))
               (mevedel-bash-policy--bash-subcommand-token-p subcommand))
      (string-join (list command subcommand) " "))))

(defun mevedel-bash-policy--bash-allow-pattern-for-segment (segment)
  "Return the reusable allow pattern suggested for Bash SEGMENT.

Simple `command subcommand ...' invocations are generalized to
Claude Code-style prefix rules such as `git log:*'.  Segments that
do not have a stable subcommand, or that start with a dangerous
command/wrapper, stay exact."
  (let* ((trimmed (string-trim segment))
         (prefix (mevedel-bash-policy--bash-prefix-for-segment trimmed)))
    (if prefix
        (concat prefix ":*")
      trimmed)))

(defun mevedel-bash-policy-allow-patterns (command)
  "Return reusable allow patterns to store when approving COMMAND.

Compound commands produce one pattern per command segment.  This
avoids saving a brittle whole-chain string such as
`pwd && git log --oneline' when the useful reusable rule is
`git log:*'."
  (require 'mevedel-bash-analysis)
  (mevedel-bash-policy--dedupe-strings
   (mapcar #'mevedel-bash-policy--bash-allow-pattern-for-segment
           (plist-get (mevedel-bash-analysis-analyze command) :segments))))

(defun mevedel-bash-policy-reusable-operation-p (command)
  "Return non-nil when COMMAND can be remembered without broadening it."
  (require 'mevedel-bash-analysis)
  (let ((analysis (mevedel-bash-analysis-analyze command)))
    (and (not (string-empty-p (string-trim command)))
         (not (plist-get analysis :complex-p))
         (not (plist-get analysis :background-p))
         (not (string-match-p "\\(?:\\*\\|\\?\\|\\[\\)" command)))))

(defun mevedel-bash-policy-effective-permission-mode
    (&optional permission-context)
  "Return effective permission mode for PERMISSION-CONTEXT."
  (require 'mevedel-structs)
  (let ((session (if permission-context
                     (plist-get permission-context :session)
                   (and (boundp 'mevedel--session) mevedel--session))))
    (or (and permission-context
             (plist-get permission-context :mode))
        (and session (mevedel-session-permission-mode session))
        mevedel-permission-mode)))

(defun mevedel-bash-policy-effective-sandbox-mode
    (&optional permission-context)
  "Return effective sandbox mode for PERMISSION-CONTEXT."
  (require 'mevedel-sandbox)
  (mevedel-sandbox-mode-effective
   (if permission-context
       (plist-get permission-context :session)
     (and (boundp 'mevedel--session) mevedel--session))))

(defun mevedel-bash-policy--bash-protected-path-p
    (command &optional analysis permission-context)
  "Return non-nil if COMMAND has an obvious protected path in ANALYSIS.
PERMISSION-CONTEXT supplies the owning execution target."
  (let ((paths (mevedel-bash-policy--bash-resource-paths
                command analysis permission-context)))
    (when paths
      (let* ((session (plist-get permission-context :session))
             (base (mevedel-bash-policy--context-directory permission-context))
             (target (or (and session
                              (mevedel-session-execution-target session))
                         (and (file-remote-p base)
                              (mevedel-execution-target-create base)))))
        (cl-some
         (lambda (path)
           (or (mevedel-permission--path-protected-p path target)
               ;; Directory roots such as `.git' may be protected by a
               ;; `**/.git/**' policy even when the literal token has no child.
               (mevedel-permission--path-protected-p
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
                                 (mevedel-permission-protected-path-policy)))
                       (string-match-p
                        (concat "\\(?:\\`\\|/\\)\\." (regexp-quote name)
                                "\\(?:/\\|\\'\\)")
                        path)))
                '("git" "ssh" "gnupg"))))
         paths)))))

(defun mevedel-bash-policy--bash-deny-candidates (command &optional analysis)
  "Return Bash strings explicit deny rules should check for COMMAND.
Includes the whole command, recognized command-chain segments, and extracted
command names.  Dangerous-name harvesting remains independent, so unsupported
syntax cannot hide a command in `mevedel-bash-dangerous-commands'.  Reuse
ANALYSIS when supplied."
  (let ((analysis (or analysis (mevedel-bash-analysis-analyze command))))
    (mevedel-bash-policy--dedupe-strings
     (append (list command)
             (plist-get analysis :candidates)
             (mevedel-bash-policy-command-names analysis)))))

(defun mevedel-bash-policy--bash-deny-match-p
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
          (eq (mevedel-permission--rules-action
               rules "Bash" :pattern candidate)
              'deny)))
      buckets))
   candidates))

(defun mevedel-bash-policy-explicit-deny-p
    (buckets command &optional analysis)
  "Return non-nil when an effective Bash deny covers COMMAND.
Generic fallback denies are evaluated against the original command and its
recognized top-level segments.  Harvested nested candidates use only pattern
rules, so a generic fallback cannot defeat a specific allow for the containing
command.  ANALYSIS is the normalized result for COMMAND when already known."
  (require 'mevedel-bash-analysis)
  (let* ((analysis (or analysis (mevedel-bash-analysis-analyze command)))
         (top-level (cons command (plist-get analysis :segments)))
         (harvested (mevedel-bash-policy--bash-deny-candidates command analysis)))
    (or (mevedel-bash-policy--bash-deny-match-p buckets top-level)
        (mevedel-bash-policy--bash-deny-match-p buckets harvested t))))

(defun mevedel-bash-policy-buckets (&optional permission-context)
  "Return Bash buckets for PERMISSION-CONTEXT, innermost-first.

Includes the request-scoped skill rule buckets so a skill's
`allowed-tools: [Bash(...)]' grants are honored by the Bash
permission check; without this, skill rules silently failed for
the Bash tool path because Bash had its own flattened resolver."
  (require 'mevedel-agents)
  (require 'mevedel-permissions)
  (require 'mevedel-structs)
  (if (plist-member permission-context :buckets)
      (plist-get permission-context :buckets)
      (let* ((session (if permission-context
                          (plist-get permission-context :session)
                        (and (boundp 'mevedel--session)
                             mevedel--session)))
             (workspace
              (if permission-context
                  (or (and session (mevedel-session-workspace session))
                      (plist-get permission-context :workspace))
                (or (and session (mevedel-session-workspace session))
                    (and (boundp 'mevedel--workspace)
                         mevedel--workspace))))
             (request
              (if permission-context
                  (plist-get permission-context :request)
                (and (boundp 'mevedel--current-request)
                     mevedel--current-request)))
             (invocation
              (if permission-context
                  (plist-get permission-context :invocation)
                (and (boundp 'mevedel--agent-invocation)
                     mevedel--agent-invocation)))
             (invocation-rules
              (and invocation
                   (mevedel-agent-invocation-skill-permission-rules
                    invocation)))
             (request-rules
              (and request
                   (mevedel-request-skill-permission-rules request)))
             (session-rules (when session
                              (mevedel-session-permission-rules session)))
             (persistent (when workspace
                           (mevedel-permission--load-persistent-rules
                            workspace))))
        (mevedel-permission--collect-buckets
         invocation-rules request-rules session-rules persistent))))

(defun mevedel-bash-policy--bash-bucket-match (buckets command)
  "Return the first non-deny (ACTION . BUCKET) matching COMMAND in BUCKETS."
  (mevedel-permission--first-non-nil-action-with-bucket
   buckets "Bash" nil command nil nil))

(defun mevedel-bash-policy--bash-direct-match (buckets command)
  "Return direct user authority matching COMMAND in BUCKETS."
  (mevedel-bash-policy--bash-bucket-match
   (seq-filter
    (lambda (entry) (memq (car entry) '(:session :persistent :defcustom)))
    buckets)
   command))


(cl-defun mevedel-bash-policy-check-permission
    (command &key trust-literal-p ignore-effective-trust-p
             permission-context)
  "Decide Bash permission for COMMAND and PERMISSION-CONTEXT.

Rules come from invocation, request, session, persistent, and
defcustom buckets (in that innermost-first order) and are
matched via `:pattern'.

Normalized Bash analysis supplies read-only, dangerous, complex, or unknown
classification.  Read-only commands run without a matching rule.  Unknown
commands need matching authority.  Dangerous and complex commands require
direct user authority rather than invocation- or request-delegated rules.
TRUST-LITERAL-P identifies a delegated skill-body call and grants no extra
authority over dangerous or complex syntax.

In `full-auto' mode, explicit deny rules and protected path tokens still
win, then unknown, dangerous, and complex Bash invocations are allowed.
When IGNORE-EFFECTIVE-TRUST-P is non-nil, `full-auto' is ignored; this
is used by the guardian to decide whether a command would have been
suspicious under the normal classifier.

Bucket-aware: delegated invocation and request rules may authorize ordinary
unknown commands, but only session, persistent, and global user rules may
authorize dangerous or complex syntax."
  (require 'cl-lib)
  (require 'mevedel-bash-analysis)
  (require 'mevedel-permissions)
  (require 'mevedel-structs)
  (require 'subr-x)
  (ignore trust-literal-p)
  (let* ((analysis (mevedel-bash-analysis-analyze command))
         (class (plist-get analysis :class))
         (segments (plist-get analysis :segments))
         (buckets (mevedel-bash-policy-buckets permission-context))
         (mode (mevedel-bash-policy-effective-permission-mode
                permission-context))
         (full-auto-p (and (not ignore-effective-trust-p)
                           (eq mode 'full-auto)))
         (full-match (mevedel-bash-policy--bash-bucket-match buckets command))
         (direct-match (mevedel-bash-policy--bash-direct-match buckets command))
         (segment-matches
          (mapcar (lambda (segment)
                    (mevedel-bash-policy--bash-bucket-match buckets segment))
                  segments))
         (segment-actions (mapcar #'car segment-matches))
         (direct-segment-actions
          (mapcar
           (lambda (segment)
             (car (mevedel-bash-policy--bash-direct-match buckets segment)))
           segments))
         (segment-classes
          (mapcar
           (lambda (segment)
             (plist-get (mevedel-bash-analysis-analyze segment) :class))
           segments)))
    (when (mevedel-bash-policy-explicit-deny-p buckets command analysis)
      (cl-return-from mevedel-bash-policy-check-permission 'deny))

    (when (and (mevedel-permission--plan-mode-p
                (plist-get permission-context :session))
               (not (eq class 'read-only)))
      (cl-return-from mevedel-bash-policy-check-permission 'deny))

    (when (and (not (plist-get permission-context
                               :resource-authority-separated-p))
               (mevedel-bash-policy--bash-protected-path-p
                command analysis permission-context))
      (cl-return-from mevedel-bash-policy-check-permission 'ask))

    (when (and (plist-get permission-context :one-shot-mutations-p)
               (not (eq class 'read-only)))
      (cl-return-from mevedel-bash-policy-check-permission 'ask))

    (cond
     ((eq (car full-match) 'ask) 'ask)
     ((memq 'deny segment-actions) 'deny)
     ((memq 'ask segment-actions) 'ask)
     ((and (memq class '(dangerous complex))
           (eq (car direct-match) 'allow))
      'allow)
     ((and (eq class 'dangerous)
           segments
           (cl-loop for action in direct-segment-actions
                    for segment-class in segment-classes
                    always (or (eq action 'allow)
                               (eq segment-class 'read-only))))
      'allow)
     ((memq class '(dangerous complex))
      (if full-auto-p 'allow 'ask))
     ((and segments (cl-every (lambda (action) (eq action 'allow))
                              segment-actions))
      'allow)
     ((eq class 'read-only)
      (if (memq 'ask segment-actions) 'ask 'allow))
     ((eq (car full-match) 'allow) 'allow)
     ((eq (car full-match) 'deny) 'deny)
     (full-auto-p 'allow)
     (t 'ask))))


;;
;;; Bash guardian guidance

(defun mevedel-bash-policy--bash-guardian-symbol (value allowed)
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

(defun mevedel-bash-policy--bash-guardian-truncate (string limit)
  "Return STRING capped at LIMIT characters."
  (let ((string (string-trim (or string ""))))
    (if (> (length string) limit)
        (concat (substring string 0 limit) "...")
      string)))

(defun mevedel-bash-policy--bash-guardian-normalize (guidance)
  "Return normalized Bash guardian GUIDANCE plist, or nil."
  (when (listp guidance)
    (let* ((risk (mevedel-bash-policy--bash-guardian-symbol
                  (plist-get guidance :risk)
                  '(low medium high critical)))
           (recommendation (mevedel-bash-policy--bash-guardian-symbol
                            (plist-get guidance :recommendation)
                            '(proceed ask deny)))
           (reason (plist-get guidance :reason)))
      (when (and risk recommendation (stringp reason)
                 (not (string-empty-p (string-trim reason))))
        (list :risk risk
              :recommendation recommendation
              :reason (mevedel-bash-policy--bash-guardian-truncate reason 240))))))

(defun mevedel-bash-policy--bash-guardian-json-range (text)
  "Return the first likely JSON object substring in TEXT, or nil."
  (when-let* ((start (string-match "{" text)))
    (let ((i (1- (length text)))
          end)
      (while (and (>= i start) (not end))
        (when (eq (aref text i) ?\})
          (setq end i))
        (setq i (1- i)))
      (and end (substring text start (1+ end))))))

(defun mevedel-bash-policy--bash-guardian-parse (response)
  "Parse guardian RESPONSE into normalized guidance, or nil."
  (when (stringp response)
    (when-let* ((json (mevedel-bash-policy--bash-guardian-json-range response)))
      (condition-case nil
          (mevedel-bash-policy--bash-guardian-normalize
           (progn
             (require 'json)
             (json-parse-string json
                                :object-type 'plist
                                :array-type 'list
                                :null-object nil
                                :false-object nil)))
        (error nil)))))

(defun mevedel-bash-policy-guardian-context-string (context)
  "Return CONTEXT formatted for the Bash guardian prompt."
  (require 'cl-lib)
  (string-join
   (delq nil
         (list
          (when-let* ((class (plist-get context :class)))
            (format "Command class: %s" class))
          (when-let* ((parser (plist-get context :parser)))
            (format "Parser: %s" parser))
          (format "Dangerous command detected: %s"
                  (if (plist-get context :dangerous) "yes" "no"))
          (format "Complex or unparseable syntax: %s"
                  (if (plist-get context :unparseable) "yes" "no"))
          (when-let* ((reasons (plist-get context :reasons)))
            (format "Analysis reasons: %s"
                    (if (cl-every #'stringp reasons)
                        (string-join reasons "; ")
                      (prin1-to-string reasons))))
          (when-let* ((resources (plist-get context :resources)))
            (format "Identified resources: %s"
                    (if (and (listp resources)
                             (cl-every #'stringp resources))
                        (string-join resources ", ")
                      (prin1-to-string resources))))
          (when-let* ((commands (or (plist-get context :commands-summary)
                                    (and-let* ((commands (plist-get context :commands)))
                                      (string-join commands ", ")))))
            (format "Detected commands: %s" commands))
          (when-let* ((level (plist-get context :sandbox-permissions)))
            (format "Requested sandbox permissions: %s" level))
          (when-let* ((additional
                       (plist-get context :additional-permissions)))
            (format "Requested additional permissions: %S" additional))
          (when-let* ((patterns
                       (plist-get context :matching-allow-patterns)))
            (format "Matching explicit allow patterns: %s"
                    (string-join patterns ", ")))
          (when-let* ((facts (plist-get context :sandbox-facts)))
            (require 'mevedel-sandbox)
            (format "Confinement: %s"
                    (mevedel-sandbox-status-text facts)))))
   "\n"))

(defun mevedel-bash-policy--bash-guardian-model-async (command context callback)
  "Ask gptel for advisory-only Bash risk guidance about COMMAND.
CONTEXT describes the classifier inputs.  CALLBACK receives normalized
guidance or nil."
  (if (not (require 'gptel nil t))
      (funcall callback nil)
    (let ((done nil)
          chunks
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
            (let* ((policy
                    (progn
                      (require 'mevedel-models)
                      (mevedel-model-resolve-workload 'guardian)))
                   (gptel-use-tools nil)
                   (gptel-tools nil)
                   (gptel-use-context nil)
                   (system-prompt
                    (progn
                      (require 'mevedel-system)
                      (mevedel-system-build-prompt
                       'bash-guardian
                       :workspace (plist-get context :workspace)
                       :working-directory
                       (plist-get context :working-directory)
                       :session (plist-get context :session))))
                   (prompt
                    (format
                     "Bash source:\n```bash\n%s\n```\n\nDeterministic analysis and confinement evidence:\n```text\n%s\n```"
                     command
                     (mevedel-bash-policy-guardian-context-string
                      context)))
                   (request-fn
                    (lambda ()
                      (gptel-request
                       prompt
                       :buffer (current-buffer)
                       :stream gptel-stream
                       :system system-prompt
                       :transforms nil
                       :callback
                       (lambda (response info)
                         (cond
                          ((and (consp response)
                                (eq (car response) 'reasoning)))
                          ((and (plist-get info :stream)
                                (stringp response))
                           (push response chunks))
                          ((eq response t)
                           (finish
                            (mevedel-bash-policy--bash-guardian-parse
                             (apply #'concat (nreverse chunks)))))
                          ((stringp response)
                           (finish
                            (mevedel-bash-policy--bash-guardian-parse response)))
                          ((or (null response) (eq response 'abort))
                           (finish nil))))))))
              (let ((gptel-backend (plist-get policy :backend))
                    (gptel-model (plist-get policy :model))
                    (gptel-reasoning-effort (plist-get policy :effort)))
                (funcall request-fn)))
          (error
           (finish nil)))))))

(defun mevedel-bash-policy-guardian-classify-async
    (command context callback)
  "Return optional guardian guidance for COMMAND and CONTEXT.
CALLBACK receives nil or a normalized guidance plist."
  (require 'gptel)
  (require 'mevedel-models)
  (require 'mevedel-system)
  (require 'subr-x)
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
                        (mevedel-bash-policy--bash-guardian-normalize
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
    (mevedel-bash-policy--bash-guardian-model-async
     command context callback))))

(defun mevedel-bash-policy-full-auto-guardian-needed-p
    (command &optional permission-context)
  "Return non-nil when COMMAND and PERMISSION-CONTEXT need guardian review.
This is only for `full-auto' mode.  The guardian is consulted when the
normal classifier would have asked, avoiding latency for routine allowed
commands while still giving the optional guardian a chance to veto
suspicious Bash."
  (and mevedel-permission-guardian
       (eq (mevedel-bash-policy-effective-permission-mode
            permission-context)
           'full-auto)
       (eq (mevedel-bash-policy-check-permission
            command :ignore-effective-trust-p t
            :permission-context permission-context)
           'ask)))

(defun mevedel-bash-policy-guardian-context
    (command &optional permission-context)
  "Return guardian context for COMMAND and PERMISSION-CONTEXT."
  (require 'cl-lib)
  (require 'mevedel-bash-analysis)
  (require 'mevedel-permissions)
  (require 'mevedel-sandbox)
  (require 'mevedel-structs)
  (let* ((session (if permission-context
                      (plist-get permission-context :session)
                    (and (boundp 'mevedel--session) mevedel--session)))
         (workspace (or (plist-get permission-context :workspace)
                        (and session (mevedel-session-workspace session))))
         (working-directory
          (and session (mevedel-session-working-directory session)))
         (analysis (mevedel-bash-analysis-analyze command))
         (commands (mevedel-bash-policy-command-names analysis))
         (buckets (mevedel-bash-policy-buckets permission-context))
         (request (plist-get permission-context :sandbox-request))
         (additional-permissions
          (plist-get request :additional-permissions))
         (sandbox-permissions
          (plist-get request :sandbox-permissions))
         (rule-buckets
          (if sandbox-permissions
              (append
               buckets
               (mevedel-permission--qualified-buckets
                buckets :sandbox-permissions sandbox-permissions))
            buckets))
         (matching-allow-patterns
          (mevedel-bash-policy--dedupe-strings
           (cl-loop
            for (_bucket . rules) in rule-buckets
            append
            (cl-loop
             for rule in
             (mevedel-permission--find-rules
              rules "Bash" :pattern command)
             for pattern = (plist-get (cdr rule) :pattern)
             when (and pattern
                       (eq (plist-get (cdr rule) :action) 'allow))
             collect pattern)))))
    (require 'mevedel-sandbox)
    (list :session session
          :workspace workspace
          :working-directory working-directory
          :analysis analysis
          :class (plist-get analysis :class)
          :dangerous (eq (plist-get analysis :class) 'dangerous)
          :commands commands
          :commands-summary (mevedel-bash-policy-commands-summary commands)
          :parser (plist-get analysis :parser)
          :reasons (plist-get analysis :reasons)
          :resources (plist-get analysis :resources)
          :unparseable (eq (plist-get analysis :class) 'complex)
          :allow-patterns (mevedel-bash-policy-allow-patterns command)
          :matching-allow-patterns matching-allow-patterns
          :additional-permissions additional-permissions
          :sandbox-permissions sandbox-permissions
          :sandbox-facts
          (mevedel-sandbox-pending-facts
           additional-permissions sandbox-permissions
           (mevedel-bash-policy-effective-sandbox-mode
            permission-context)
           working-directory))))

(provide 'mevedel-bash-policy)

;;; mevedel-bash-policy.el ends here
