;;; mevedel-tool-exec.el -- Bash and Eval tools -*- lexical-binding: t -*-

;;; Commentary:

;; Bash command execution with permission system and Eval tool for elisp
;; evaluation.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'subr-x)
(require 'mevedel-permission-log)

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `cl-seq'
(declare-function cl-count "cl-seq" (cl-item cl-seq &rest cl-keys))

;; `gptel'
(declare-function gptel-request "ext:gptel-request" (&optional prompt &rest args))
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-reasoning-effort)
(defvar gptel-tools)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar read-eval)

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-skill-permission-rules
                  "mevedel-agents" (cl-x) t)
(defvar mevedel--agent-invocation)

;; `mevedel-bash-analysis'
(declare-function mevedel-bash-analysis-analyze
                  "mevedel-bash-analysis" (source))
(defvar mevedel-bash-dangerous-commands)

;; `mevedel-models'
(declare-function mevedel-model-resolve-workload
                  "mevedel-models"
                  (workload &optional explicit-selector explicit-effort))

;; `mevedel-permission-prompt'
(declare-function mevedel-permission--build-attribution-line
                  "mevedel-permission-prompt" (origin))
(declare-function mevedel-permission--prompt-async-eval
                  "mevedel-permission-prompt"
                  (content cont &optional count entry))

;; `mevedel-permission-queue'
(declare-function mevedel-permission--enqueue "mevedel-permission-queue"
                  (entry &optional session))
(declare-function mevedel-permission-queue--current-session
                  "mevedel-permission-queue" ())
(declare-function mevedel-permission-queue--render-head
                  "mevedel-permission-queue" (&optional session))

;; `mevedel-permissions'
(declare-function mevedel-permission--apply-prompt-result
                  "mevedel-permissions" t t)
(declare-function mevedel-permission--any-deny "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--collect-buckets
                  "mevedel-permissions"
                  (invocation-rules request-rules
                                    session-rules persistent-rules))
(declare-function mevedel-permission--first-non-nil-action
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--first-non-nil-action-with-bucket
                  "mevedel-permissions"
                  (buckets tool-name path pattern domain name))
(declare-function mevedel-permission--load-persistent-rules "mevedel-permissions"
                  (workspace))
(declare-function mevedel-permission--normalize-outcome
                  "mevedel-permissions" (outcome))
(declare-function mevedel-permission--path-protected-p
                  "mevedel-permissions" (path))
(declare-function mevedel-permission--rules-action "mevedel-permissions"
                  (rules tool-name &rest keys))
(defvar mevedel-permission-mode)
(defvar mevedel-permission-rules)
(defvar mevedel-protected-paths)

;; `mevedel-structs'
(declare-function mevedel-request-skill-permission-rules
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-mode "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-permission-rules "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-working-directory "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-root "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file
                  "mevedel-system" (relative-path &optional replacements))

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

(defun mevedel-tool-exec--permission-log-session ()
  "Return the session visible to a Bash/Eval permission adapter."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (mevedel-permission-queue--current-session)))

(defun mevedel-tool-exec--permission-decision-result
    (metadata-p outcome via &rest props)
  "Return OUTCOME, or metadata when METADATA-P is non-nil."
  (if metadata-p
      (append (list :outcome (mevedel-permission--normalize-outcome outcome)
                    :raw-outcome outcome
                    :via via
                    :logged t)
              props)
    outcome))

(defun mevedel-tool-exec--log-permission-decision
    (tool-name outcome via &rest props)
  "Persist TOOL-NAME permission decision OUTCOME via VIA with PROPS."
  (when-let* ((session (mevedel-tool-exec--permission-log-session)))
    (apply #'mevedel-permission-log
           session 'permission-decision
           (append (list :tool-name tool-name
                         :origin (mevedel-tool-exec--current-origin)
                         :mode (mevedel-tool-exec--effective-permission-mode)
                         :outcome (mevedel-permission--normalize-outcome outcome)
                         :via via)
                   props))))

(defun mevedel-tool-exec--analyze-bash (command)
  "Return normalized Bash analysis for COMMAND."
  (require 'mevedel-bash-analysis)
  (mevedel-bash-analysis-analyze command))


;;
;;; Command Execution

(defcustom mevedel-bash-timeout 120
  "Maximum seconds a Bash command may run before it is terminated.

The timeout starts once the process is spawned.  Bash tool calls may
override this default with `timeout_seconds'.  Set to nil to disable
Bash command timeouts."
  :type '(choice (integer :tag "Timeout in seconds")
                 (const :tag "Disabled" nil))
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

The result is never used for enforcement.  Explicit deny, read-only Goal
phases, protected-path policy, and the user's decision remain authoritative."
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

(defun mevedel-tool-exec--bash-commands-summary (commands)
  "Return a counted, first-seen summary string for COMMANDS."
  (when-let* ((unique (mevedel-tool-exec--dedupe-strings commands)))
    (string-join
     (mapcar
      (lambda (command)
        (let ((count (cl-count command commands :test #'equal)))
          (if (> count 1)
              (format "%s (%d)" command count)
            command)))
      unique)
     ", ")))

(defun mevedel-tool-exec--bash-decision-specifier-value (command)
  "Return sanitized Bash specifier metadata for COMMAND."
  (or (mevedel-tool-exec--bash-commands-summary
       (mevedel-tool-exec--bash-command-names
        (mevedel-tool-exec--analyze-bash command)))
      "unparseable shell command"))

(defun mevedel-tool-exec--bash-command-names (analysis)
  "Return executable names from normalized Bash ANALYSIS."
  (mapcar (lambda (argv) (file-name-nondirectory (car argv)))
          (plist-get analysis :commands)))

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
  "Return non-nil for stable shell subcommand WORD."
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
           (plist-get (mevedel-tool-exec--analyze-bash command) :segments))))

(defun mevedel-tool-exec--effective-permission-mode
    (&optional permission-context)
  "Return effective permission mode for PERMISSION-CONTEXT."
  (let ((session (and (boundp 'mevedel--session) mevedel--session)))
    (or (and permission-context
             (plist-get permission-context :mode))
        (and session (mevedel-session-permission-mode session))
        mevedel-permission-mode)))

(defun mevedel-tool-exec--bash-literal-path-tokens (command &optional analysis)
  "Return literal path resources identified in COMMAND.
Dynamic expansions remain complex and are not evaluated.  Reuse ANALYSIS when
the caller already analyzed COMMAND."
  (plist-get (or analysis (mevedel-tool-exec--analyze-bash command))
             :resources))

(defun mevedel-tool-exec--bash-protected-path-p (command &optional analysis)
  "Return non-nil if COMMAND has an obvious protected path in ANALYSIS."
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
   (mevedel-tool-exec--bash-literal-path-tokens command analysis)))

(defun mevedel-tool-exec--bash-deny-candidates (command &optional analysis)
  "Return Bash strings explicit deny rules should check for COMMAND.
Includes the whole command, recognized command-chain segments, and extracted
command names.  Dangerous-name harvesting remains independent, so unsupported
syntax cannot hide a command in `mevedel-bash-dangerous-commands'.  Reuse
ANALYSIS when supplied."
  (let ((analysis (or analysis (mevedel-tool-exec--analyze-bash command))))
    (mevedel-tool-exec--dedupe-strings
     (append (list command)
             (plist-get analysis :candidates)
             (mevedel-tool-exec--bash-command-names analysis)))))

(defun mevedel-tool-exec--bash-deny-match-p
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

(defun mevedel-tool-exec--bash-explicit-deny-p
    (buckets command &optional analysis)
  "Return non-nil when an effective Bash deny covers COMMAND.
Generic fallback denies are evaluated against the original command and its
recognized top-level segments.  Harvested nested candidates use only pattern
rules, so a generic fallback cannot defeat a specific allow for the containing
command.  ANALYSIS is the normalized result for COMMAND when already known."
  (let* ((analysis (or analysis (mevedel-tool-exec--analyze-bash command)))
         (top-level (cons command (plist-get analysis :segments)))
         (harvested (mevedel-tool-exec--bash-deny-candidates command analysis)))
    (or (mevedel-tool-exec--bash-deny-match-p buckets top-level)
        (mevedel-tool-exec--bash-deny-match-p buckets harvested t))))

(defun mevedel-tools--bash-buckets (&optional permission-context)
  "Return Bash buckets for PERMISSION-CONTEXT, innermost-first.

Includes the request-scoped skill rule buckets so a skill's
`allowed-tools: [Bash(...)]' grants are honored by the Bash
permission check; without this, skill rules silently failed for
the Bash tool path because Bash had its own flattened resolver."
  (or (and permission-context (plist-get permission-context :buckets))
      (let* ((session (and (boundp 'mevedel--session) mevedel--session))
             (workspace (cond
                         (session (mevedel-session-workspace session))
                         ((and (boundp 'mevedel--workspace)
                               mevedel--workspace))))
             (request (and (boundp 'mevedel--current-request)
                           mevedel--current-request))
             (invocation (and (boundp 'mevedel--agent-invocation)
                              mevedel--agent-invocation))
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

(defun mevedel-tool-exec--bash-bucket-match (buckets command)
  "Return the first non-deny (ACTION . BUCKET) matching COMMAND in BUCKETS."
  (mevedel-permission--first-non-nil-action-with-bucket
   buckets "Bash" nil command nil nil))

(defun mevedel-tool-exec--bash-direct-match (buckets command)
  "Return direct user authority matching COMMAND in BUCKETS."
  (mevedel-tool-exec--bash-bucket-match
   (seq-filter
    (lambda (entry) (memq (car entry) '(:session :persistent :defcustom)))
    buckets)
   command))


(cl-defun mevedel-tools--check-bash-permission
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
  (ignore trust-literal-p)
  (let* ((analysis (mevedel-tool-exec--analyze-bash command))
         (class (plist-get analysis :class))
         (segments (plist-get analysis :segments))
         (buckets (mevedel-tools--bash-buckets permission-context))
         (mode (mevedel-tool-exec--effective-permission-mode
                permission-context))
         (full-auto-p (and (not ignore-effective-trust-p)
                           (eq mode 'full-auto)))
         (full-match (mevedel-tool-exec--bash-bucket-match buckets command))
         (direct-match (mevedel-tool-exec--bash-direct-match buckets command))
         (segment-matches
          (mapcar (lambda (segment)
                    (mevedel-tool-exec--bash-bucket-match buckets segment))
                  segments))
         (segment-actions (mapcar #'car segment-matches))
         (direct-segment-actions
          (mapcar
           (lambda (segment)
             (car (mevedel-tool-exec--bash-direct-match buckets segment)))
           segments))
         (segment-classes
          (mapcar
           (lambda (segment)
             (plist-get (mevedel-tool-exec--analyze-bash segment) :class))
           segments)))
    (when (mevedel-tool-exec--bash-explicit-deny-p buckets command analysis)
      (cl-return-from mevedel-tools--check-bash-permission 'deny))

    (when (mevedel-tool-exec--bash-protected-path-p command analysis)
      (cl-return-from mevedel-tools--check-bash-permission 'ask))

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
          (when-let* ((commands (or (plist-get context :commands-summary)
                                    (and-let* ((commands (plist-get context :commands)))
                                      (string-join commands ", ")))))
            (format "Detected commands: %s" commands))
          (when-let* ((patterns (plist-get context :allow-patterns)))
            (format "Suggested allow patterns: %s"
                    (string-join patterns ", ")))))
   "\n"))

(defun mevedel-tool-exec--bash-guardian-model-async (command context callback)
  "Ask gptel for advisory-only Bash risk guidance about COMMAND.
CONTEXT describes the classifier inputs.  CALLBACK receives normalized
guidance or nil."
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
        (condition-case err
            (let* ((policy
                    (progn
                      (require 'mevedel-models)
                      (mevedel-model-resolve-workload 'guardian)))
                   (gptel-use-tools nil)
                   (gptel-tools nil)
                   (gptel-use-context nil)
                   (prompt
                    (progn
                      (require 'mevedel-system)
                      (mevedel-system-render-prompt-file
                       "prompts/permissions/bash-guardian.md"
                       `(("COMMAND" . ,command)
                         ("CONTEXT" . ,(mevedel-tool-exec--bash-guardian-context-string
                                         context))))))
                   (request-fn
                    (lambda ()
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
                            (finish nil))))))))
              (let ((gptel-backend (plist-get policy :backend))
                    (gptel-model (plist-get policy :model))
                    (gptel-reasoning-effort (plist-get policy :effort)))
                (funcall request-fn)))
          (user-error
           (setq done t)
           (when timer
             (cancel-timer timer))
           (signal (car err) (cdr err)))
          (error
           (finish nil)))))))

(defun mevedel-tool-exec--bash-guardian-classify-async
    (command context callback)
  "Return optional guardian guidance for COMMAND and CONTEXT.
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

(defun mevedel-tool-exec--bash-full-auto-guardian-needed-p
    (command &optional permission-context)
  "Return non-nil when COMMAND and PERMISSION-CONTEXT need guardian review.
This is only for `full-auto' mode.  The guardian is consulted when the
normal classifier would have asked, avoiding latency for routine allowed
commands while still giving the optional guardian a chance to veto
suspicious Bash."
  (and mevedel-permission-guardian
       (eq (mevedel-tool-exec--effective-permission-mode
            permission-context)
           'full-auto)
       (eq (mevedel-tools--check-bash-permission
            command :ignore-effective-trust-p t
            :permission-context permission-context)
           'ask)))

(defun mevedel-tool-exec--bash-guardian-context (command)
  "Return guardian context plist for COMMAND."
  (let* ((analysis (mevedel-tool-exec--analyze-bash command))
         (commands (mevedel-tool-exec--bash-command-names analysis)))
    (list :class (plist-get analysis :class)
          :dangerous (eq (plist-get analysis :class) 'dangerous)
          :commands commands
          :commands-summary (mevedel-tool-exec--bash-commands-summary commands)
          :parser (plist-get analysis :parser)
          :reasons (plist-get analysis :reasons)
          :resources (plist-get analysis :resources)
          :unparseable (eq (plist-get analysis :class) 'complex)
          :allow-patterns (mevedel-tool-exec--bash-allow-patterns command))))

(defun mevedel-tool-exec--bash-deny-only-guardian-async
    (command cont &optional metadata-p)
  "Run deny-only guardian review for COMMAND and METADATA-P, then call CONT.
Guardian deny recommendations become `deny'.  Timeout, failure, invalid
output, and non-deny recommendations allow by default."
  (mevedel-tool-exec--bash-guardian-classify-async
   command
   (mevedel-tool-exec--bash-guardian-context command)
   (lambda (guardian)
     (let ((outcome (if (eq (plist-get guardian :recommendation) 'deny)
                        'deny
                      'allow)))
       (when metadata-p
         (mevedel-tool-exec--log-permission-decision
          "Bash" outcome 'bash-guardian
          :specifier-key :pattern
          :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
       (funcall cont
                (mevedel-tool-exec--permission-decision-result
                 metadata-p outcome 'bash-guardian
                 :specifier-key :pattern
                 :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))))


;;
;;; Eval Prompt UI

(defcustom mevedel-eval-expression-display-limit 20
  "Maximum number of lines to show inline in the Eval permission prompt.
Expressions longer than this are truncated with a toggle to expand."
  :type 'integer
  :group 'mevedel)

(defun mevedel--prompt-user-for-eval
    (expression callback &optional origin count entry mode preserve-ui)
  "Display Eval permission overlay for EXPRESSION and CALLBACK.

CALLBACK is invoked once with `allow-once', `deny-once', a feedback cons,
or `aborted'.  Long expressions are truncated in
the display and can be toggled with TAB.  ORIGIN, when non-main,
renders the same attribution line used by generic and Bash permission
prompts.  COUNT is the permission queue depth for the composite
interaction-zone counter.  ENTRY identifies the queued prompt.  MODE and
PRESERVE-UI describe the requested execution scope."
  (unless (fboundp 'mevedel-permission--prompt-async-eval)
    (require 'mevedel-permission-prompt))
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
                   (propertize "Mode: " 'font-lock-face 'font-lock-escape-face)
                   (format "%s" (or mode "live"))
                   (when (equal (or mode "live") "live")
                     (format " (preserve_ui: %s)"
                             (if preserve-ui "true" "false")))
                   "\n\n"
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

(cl-defun mevedel-tools--check-eval-permission
    (&key trust-literal-p permission-context)
  "Decide Eval permission for TRUST-LITERAL-P and PERMISSION-CONTEXT.

Normal model-requested Eval asks unless an explicit deny rule applies
or the effective permission mode is `full-auto'.  When TRUST-LITERAL-P
is non-nil, as with author-written skill body injections, an active
allow rule for Eval may bypass the prompt.  Deny rules still win
absolutely."
  (let* ((buckets (mevedel-tools--bash-buckets permission-context))
         (mode (mevedel-tool-exec--effective-permission-mode
                permission-context))
         (action (mevedel-permission--first-non-nil-action
                  buckets "Eval" nil nil nil nil)))
    (cond
     ((mevedel-permission--any-deny buckets "Eval" nil nil nil nil)
      'deny)
     ((eq mode 'full-auto)
      'allow)
     (trust-literal-p
      (or action 'ask))
     (t 'ask))))

(defun mevedel-tool-exec--eval-check-permission-async (_tool-struct input cont)
  "Async permission check for Eval tool INPUT.

Routes the prompt through the session permission queue rather
than calling `mevedel--prompt-user-for-eval' directly.  The
queue's render-head dispatches to the specialized Eval UI via
`mevedel-permission-queue--render-eval'.  CONT receives the same
slot vocabulary as before: `allow', `deny', `(deny . REASON)',
`aborted' -- feedback text shaped into the existing
\"Eval cancelled by user. Feedback: TEXT\" form so LLM-visible
denial parity with the sync slot is preserved."
  (let* ((expression (plist-get input :expression))
         (trust-literal-p (plist-get input :trust-literal-p))
         (permission-context (plist-get input :permission-context))
         (metadata-p (plist-get input :permission-decision-metadata))
         mode
         mode-error
         (preserve-ui (mevedel-tool-exec--eval-preserve-ui-p input)))
    (condition-case err
        (setq mode (mevedel-tool-exec--eval-mode input))
      (error (setq mode-error (error-message-string err))))
    (cond
     (mode-error
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         "Eval" (cons 'deny mode-error) 'eval-policy))
      (funcall cont
               (mevedel-tool-exec--permission-decision-result
                metadata-p (cons 'deny mode-error) 'eval-policy)))
     ((null expression)
      (when metadata-p
        (mevedel-tool-exec--log-permission-decision
         "Eval" 'deny 'eval-policy))
      (funcall cont
               (mevedel-tool-exec--permission-decision-result
                metadata-p 'deny 'eval-policy)))
     (t
      (pcase (mevedel-tools--check-eval-permission
              :trust-literal-p trust-literal-p
              :permission-context permission-context)
        ('allow
         (when metadata-p
           (mevedel-tool-exec--log-permission-decision
            "Eval" 'allow 'eval-policy))
         (funcall cont
                  (mevedel-tool-exec--permission-decision-result
                   metadata-p 'allow 'eval-policy)))
        ('deny
         (when metadata-p
           (mevedel-tool-exec--log-permission-decision
            "Eval" 'deny 'eval-policy))
         (funcall cont
                  (mevedel-tool-exec--permission-decision-result
                   metadata-p 'deny 'eval-policy)))
        (_
         (if trust-literal-p
             (let ((outcome
                    (cons 'deny
                          "Elisp expansion requires a pre-approved Eval rule; no prompt is shown while preparing skill bodies.")))
               (when metadata-p
                 (mevedel-tool-exec--log-permission-decision
                  "Eval" outcome 'eval-policy))
               (funcall
                cont
                (mevedel-tool-exec--permission-decision-result
                 metadata-p outcome 'eval-policy)))
           (when metadata-p
             (mevedel-tool-exec--log-permission-decision
              "Eval" 'ask 'eval-policy))
           (apply #'mevedel-permission--enqueue
            (list :kind 'eval
                  :expression expression
                  :mode (symbol-name mode)
                  :preserve-ui preserve-ui
                  :origin (mevedel-tool-exec--current-origin)
                  :callback
                  (lambda (outcome)
                    (pcase outcome
                      ('allow-once
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'allow 'eval-policy)))
                      ('deny-once
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'deny 'eval-policy)))
                      (`(feedback . ,text)
                       (funcall cont
                                (mevedel-tool-exec--permission-decision-result
                                 metadata-p
                                 (cons 'deny
                                       (format "Eval cancelled by user. Feedback: %s"
                                               text))
                                 'eval-policy)))
                      ('aborted
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'aborted 'eval-policy)))
                      (_
                       (funcall
                        cont
                        (mevedel-tool-exec--permission-decision-result
                         metadata-p 'deny 'eval-policy))))))
            (and (plist-get permission-context :session)
                 (list (plist-get permission-context :session)))))))))))


;;
;;; Bash Prompt UI

(defun mevedel-tool-exec--check-permission-async (_tool-struct input cont)
  "Async permission check for Bash tool INPUT.

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
        (trust-literal-p (plist-get input :trust-literal-p))
        (permission-context (plist-get input :permission-context))
        (metadata-p (plist-get input :permission-decision-metadata)))
    (if (null command)
        (funcall cont nil)
      (let ((decision (mevedel-tools--check-bash-permission
                       command :trust-literal-p trust-literal-p
                       :permission-context permission-context)))
        (cond
         ((not (eq decision 'ask))
          (if (and (eq decision 'allow)
                   (mevedel-tool-exec--bash-full-auto-guardian-needed-p
                    command permission-context))
              (mevedel-tool-exec--bash-deny-only-guardian-async
               command cont metadata-p)
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" decision 'bash-classifier
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec--permission-decision-result
              metadata-p decision 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))
         (trust-literal-p
          (let ((outcome
                 (cons 'deny
                       "Shell expansion requires a pre-approved Bash rule; no prompt is shown while preparing skill bodies.")))
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" outcome 'bash-classifier
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (funcall
             cont
             (mevedel-tool-exec--permission-decision-result
              metadata-p outcome 'bash-classifier
              :specifier-key :pattern
              :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))
         (t
          (let* ((source-buffer (current-buffer))
                 (session (or (plist-get permission-context :session)
                              (and (boundp 'mevedel--session)
                                   mevedel--session)
                              (mevedel-permission-queue--current-session)))
                 (guardian-pending t)
                 (workspace (or (plist-get permission-context :workspace)
                                (and session
                                     (mevedel-session-workspace session))))
                 (analysis (mevedel-tool-exec--analyze-bash command))
                 (command-class (plist-get analysis :class))
                 (commands (mevedel-tool-exec--bash-command-names analysis))
                 (commands-summary
                  (mevedel-tool-exec--bash-commands-summary commands))
                 (unparseable (eq command-class 'complex))
                 (allow-patterns
                  (mevedel-tool-exec--bash-allow-patterns command))
                 (dangerous (eq command-class 'dangerous))
                 (rule-creating-p
                  (not (memq command-class '(dangerous complex))))
                 (guardian-cell
                  (list nil (and mevedel-permission-guardian 'pending)))
                 (entry
                  (list :kind 'bash
                        :command command
                        :analysis analysis
                        :command-class command-class
                        :commands commands
                        :commands-summary commands-summary
                        :unparseable unparseable
                        :allow-patterns allow-patterns
                        :guardian-cell guardian-cell
                        :workspace workspace
                        :include-always (and rule-creating-p
                                             (not (null workspace)))
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
                                   (funcall
                                    cont
                                    (mevedel-tool-exec--permission-decision-result
                                     metadata-p collapsed 'bash-classifier
                                     :specifier-key :pattern
                                     :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                               (error
                                (funcall
                                 cont
                                 (mevedel-tool-exec--permission-decision-result
                                  metadata-p
                                  (format "Error: Bash rule write failed: %S" err)
                                  'bash-classifier
                                  :specifier-key :pattern
                                  :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))))
                            ('allow
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'allow 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            ('deny
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'deny 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (`(deny . ,reason)
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p (cons 'deny reason) 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (`(feedback . ,text)
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p
                               (cons 'deny
                                     (format "Command cancelled by user. Feedback: %s"
                                             text))
                               'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            ('aborted
                             (funcall
                              cont
                              (mevedel-tool-exec--permission-decision-result
                               metadata-p 'aborted 'bash-classifier
                               :specifier-key :pattern
                               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command))))
                            (_ (funcall
                                cont
                                (mevedel-tool-exec--permission-decision-result
                                 metadata-p 'deny 'bash-classifier
                                 :specifier-key :pattern
                                 :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))))))))
            (when metadata-p
              (mevedel-tool-exec--log-permission-decision
               "Bash" 'ask 'bash-classifier
               :specifier-key :pattern
               :specifier-value (mevedel-tool-exec--bash-decision-specifier-value
                                 command)))
            (if (buffer-live-p source-buffer)
                (with-current-buffer source-buffer
                  (mevedel-permission--enqueue entry session))
              (mevedel-permission--enqueue entry session))
            (mevedel-tool-exec--bash-guardian-classify-async
             command
             (list :dangerous dangerous
                   :commands commands
                   :commands-summary commands-summary
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
  "Apply Bash prompt OUTCOME for SESSION, WORKSPACE, and COMMAND.

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

(defconst mevedel-tool-exec--child-kill-delay 2
  "Seconds to wait before force-killing a timed-out child process.")

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

(defun mevedel-tool-exec--bash-timeout-seconds (args)
  "Return the effective Bash timeout in seconds for ARGS."
  (let ((override (plist-get args :timeout_seconds)))
    (cond
     ((and override (not (integerp override)))
      (error "Parameter timeout_seconds must be an integer"))
     ((and (integerp override) (<= override 0))
      (error "Parameter timeout_seconds must be positive"))
     ((null mevedel-bash-timeout) nil)
     ((integerp override) override)
     ((and (integerp mevedel-bash-timeout)
           (> mevedel-bash-timeout 0))
      mevedel-bash-timeout)
     (t
      (error "Variable mevedel-bash-timeout must be nil or a positive integer")))))

(defun mevedel-tool-exec--child-spawn-command (command)
  "Return spawn metadata for child process COMMAND.
Use `setsid' when available so timeout signals can reach descendants."
  (let ((setsid (and (not (eq system-type 'windows-nt))
                     (executable-find "setsid"))))
    (if (not setsid)
        (list :command command)
      (let ((group-file (make-temp-file "mevedel-child-group-")))
        (list
         :command
         (append
          (list setsid "-f" "-w"
                "sh" "-c"
                "printf '%s' \"$$\" > \"$1\"; shift; exec \"$@\""
                "mevedel-child" group-file)
          command)
         :group-file group-file)))))

(defun mevedel-tool-exec--child-group-id (group-file)
  "Return the positive process group id recorded in GROUP-FILE."
  (when (and group-file (file-readable-p group-file))
    (let ((id (string-to-number
               (string-trim
                (with-temp-buffer
                  (insert-file-contents group-file)
                  (buffer-string))))))
      (and (> id 0) id))))

(defun mevedel-tool-exec--signal-child-process
    (process process-group-p signal &optional pid)
  "Send SIGNAL to PROCESS, using PROCESS-GROUP-P and PID when requested.
When PID is non-nil, use it as the process group id even if PROCESS
has already exited."
  (let ((target-pid (or pid (and (processp process) (process-id process)))))
    (condition-case nil
        (if (and process-group-p (integerp target-pid) (> target-pid 0))
            (signal-process (- target-pid) signal)
          (when (processp process)
            (signal-process process signal)))
      (error
       (when (process-live-p process)
         (ignore-errors
           (signal-process process signal)))))))

(defun mevedel-tool-exec--start-child-process
    (name command workdir timeout callback)
  "Start COMMAND as NAME in WORKDIR and call CALLBACK when it settles.

CALLBACK receives a plist with :exit-code, :output, :timed-out-p,
and optional :error.  TIMEOUT is nil or a positive number of seconds.
On systems with `setsid', timeout signals cover the whole process group."
  (let (output-buffer spawn group-file timer force-timer settle-timer
                      timed-out finished exit-code process)
    (cl-labels
        ((process-output ()
           (if (buffer-live-p output-buffer)
               (with-current-buffer output-buffer
                 (buffer-string))
             ""))
         (cleanup ()
           (when (processp process)
             (set-process-query-on-exit-flag process nil)
             (when (process-live-p process)
               (ignore-errors (delete-process process))))
           (when (buffer-live-p output-buffer)
             (let ((kill-buffer-query-functions nil))
               (kill-buffer output-buffer)))
           (when group-file
             (ignore-errors (delete-file group-file))))
         (finish (status &optional error-data)
           (unless finished
             (setq finished t)
             (when (timerp timer)
               (cancel-timer timer))
             (when (timerp force-timer)
               (cancel-timer force-timer))
             (when (timerp settle-timer)
               (cancel-timer settle-timer))
             (let ((output (process-output)))
               (cleanup)
               (funcall callback
                        (list :exit-code status
                              :output output
                              :timed-out-p timed-out
                              :error error-data)))))
         (process-ended (child)
           (when (memq (process-status child) '(exit signal))
             (setq exit-code (process-exit-status child))
             ;; A timed-out process group still needs the delayed KILL even
             ;; when its leader exits promptly after TERM.
             (unless (and timed-out (timerp force-timer))
               (finish exit-code))))
         (settle-after-kill ()
           (unless finished
             (finish (or exit-code -1))))
         (force-kill ()
           (unless finished
             (let ((group-id
                    (mevedel-tool-exec--child-group-id group-file)))
               ;; Give the sentinel a bounded chance to drain the pipe after
               ;; KILL before falling back to explicit settlement.
               (setq force-timer nil
                     settle-timer
                     (run-at-time mevedel-tool-exec--child-kill-delay
                                  nil #'settle-after-kill))
               (mevedel-tool-exec--signal-child-process
                process (and group-id t) 'KILL group-id))))
         (time-out ()
           (unless finished
             (if (not (process-live-p process))
                 (finish (process-exit-status process))
               (let ((group-id
                      (mevedel-tool-exec--child-group-id group-file)))
                 (setq timed-out t)
                 (mevedel-tool-exec--signal-child-process
                  process (and group-id t) 'TERM group-id)
                 (setq force-timer
                       (run-at-time mevedel-tool-exec--child-kill-delay
                                    nil #'force-kill)))))))
      (condition-case err
          (progn
            (setq output-buffer
                  (generate-new-buffer (format " *%s*" name)))
            (setq spawn (mevedel-tool-exec--child-spawn-command command)
                  group-file (plist-get spawn :group-file))
            (setq process
                  (let ((default-directory workdir))
                    (with-current-buffer output-buffer
                      (setq-local default-directory workdir))
                    (make-process
                     :name name
                     :buffer output-buffer
                     :command (plist-get spawn :command)
                     :connection-type 'pipe
                     :noquery t
                     :sentinel
                     (lambda (child _event)
                       (process-ended child)))))
            (when timeout
              (setq timer
                    (run-at-time timeout nil #'time-out)))
            process)
        (error
         (finish -1 err)
         nil)))))

(defun mevedel-tool-exec--bash-format-result (exit-code output timed-out timeout)
  "Return Bash result text for EXIT-CODE, OUTPUT, TIMED-OUT, and TIMEOUT."
  (cond
   (timed-out
    (format "Command timed out after %ss and was terminated:\nSTDOUT+STDERR:\n%s"
            timeout output))
   ((zerop exit-code) output)
   (t
    (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s"
            exit-code output))))

(defun mevedel-tool-exec--bash (callback args)
  "Execute a Bash command and return its output.
CALLBACK receives the result envelope.  ARGS is a plist with :command
and optional :timeout_seconds."
  (let ((command (plist-get args :command)))
    (unless (stringp command)
      (error "Parameter command is required"))
    (let ((timeout (mevedel-tool-exec--bash-timeout-seconds args)))
      (mevedel-tool-exec--start-child-process
       "mevedel-bash" (list "bash" "-lc" command)
       (mevedel-tool-exec--default-directory) timeout
       (lambda (child-result)
         (let ((error-data (plist-get child-result :error)))
           (funcall
            callback
            (list
             :result
             (if error-data
                 (format "Failed to start process: %s" error-data)
               (mevedel-tool-exec--bash-format-result
                (plist-get child-result :exit-code)
                (mevedel-tool-exec--truncate-output
                 (plist-get child-result :output))
                (plist-get child-result :timed-out-p)
                timeout))))))))))


;;
;;; Eval

(defun mevedel-tool-exec--eval-mode (args)
  "Return the requested Eval execution mode from ARGS."
  (let ((mode (plist-get args :mode)))
    (cond
     ((or (null mode)
          (eq mode :json-false)
          (and (stringp mode) (string-empty-p mode))
          (equal mode "live"))
      'live)
     ((equal mode "batch") 'batch)
     (t (error "Unknown Eval mode: %s" mode)))))

(defun mevedel-tool-exec--eval-preserve-ui-p (args)
  "Return non-nil when ARGS request restoring window state."
  (not (eq (plist-get args :preserve_ui) :json-false)))

(defun mevedel-tool-exec--eval-format-result
    (result output result-format)
  "Format Eval RESULT and captured OUTPUT for RESULT-FORMAT."
  (mevedel-tool-exec--truncate-output
   (if (equal result-format "injection")
       (concat
        (format "%S" result)
        (and (not (string-empty-p (or output "")))
             (format "\n\nSTDOUT:\n%s" output)))
     (concat
      (format "Result:\n%S" result)
      (and (not (string-empty-p (or output "")))
           (format "\n\nSTDOUT:\n%s" output))))))

(defun mevedel-tool-exec--eval-format-error (err output)
  "Format Eval error ERR and captured OUTPUT."
  (concat
   (format "Error: Eval failed with error %S: %S"
           (car err) (cdr err))
   (and (not (string-empty-p (or output "")))
        (format "\n\nSTDOUT:\n%s" output))))

(defun mevedel-tool-exec--eval-live (callback expression result-format preserve-ui)
  "Evaluate EXPRESSION in the live Emacs process.
CALLBACK receives the result envelope.  RESULT-FORMAT controls
the model-facing shape.  PRESERVE-UI restores the selected frame's
window configuration after evaluation."
  (let ((standard-output (generate-new-buffer " *mevedel-eval-elisp*"))
        (window-configuration (and preserve-ui
                                   (current-window-configuration)))
        (result nil) (output nil) response)
    (unwind-protect
        (condition-case err
            (let ((default-directory
                    (mevedel-tool-exec--default-directory)))
              (setq result (eval (read expression) t))
              (when (> (buffer-size standard-output) 0)
                (setq output (mevedel-tool-exec--truncate-output
                              (with-current-buffer standard-output
                                (buffer-string)))))
              (setq response
                    (mevedel-tool-exec--eval-format-result
                     result output result-format)))
          ((error user-error)
           (when (> (buffer-size standard-output) 0)
             (setq output (mevedel-tool-exec--truncate-output
                           (with-current-buffer standard-output
                             (buffer-string)))))
           (setq response
                 (mevedel-tool-exec--eval-format-error err output))))
      (when (window-configuration-p window-configuration)
        (ignore-errors
          (set-window-configuration window-configuration)))
      (kill-buffer standard-output))
    (funcall callback (list :result response))))

(defun mevedel-tool-exec--eval-batch-script
    (expression result-file workdir load-path-value result-format)
  "Return batch Eval source for EXPRESSION writing RESULT-FILE.
WORKDIR, LOAD-PATH-VALUE, and RESULT-FORMAT configure the child Emacs."
  (concat
   ";;; -*- lexical-binding: t -*-\n"
   (prin1-to-string
    `(let ((load-path ',load-path-value)
           (default-directory ,workdir)
           (expression ,expression)
           (result-file ,result-file)
           (result-format ',result-format)
           (max-output-bytes ,mevedel-tool-exec--max-output-bytes)
           (stdout-buffer (generate-new-buffer " *mevedel-eval-batch-stdout*"))
           result output)
       (unwind-protect
           (let ((standard-output stdout-buffer)
                 (truncate-output
                  (lambda (text)
                    (if (<= (length text) max-output-bytes)
                        text
                      (concat
                       (substring text 0 max-output-bytes)
                       (format "\n\n... Output truncated (%dK of %dK bytes shown)."
                               (/ max-output-bytes 1024)
                               (/ (length text) 1024)))))))
             (condition-case err
                 (progn
                   (setq result (eval (read expression) t))
                   (setq output
                         (with-current-buffer stdout-buffer
                           (buffer-string)))
                   (with-temp-file result-file
                     (prin1 (list :status 'ok
                                  :text
                                  (funcall
                                   truncate-output
                                   (if (equal result-format "injection")
                                       (concat
                                        (format "%S" result)
                                        (and (> (length (or output "")) 0)
                                             (format "\n\nSTDOUT:\n%s" output)))
                                     (concat
                                      (format "Result:\n%S" result)
                                      (and (> (length (or output "")) 0)
                                           (format "\n\nSTDOUT:\n%s" output))))))
                            (current-buffer))))
               ((error user-error)
                (setq output
                      (and (buffer-live-p stdout-buffer)
                           (with-current-buffer stdout-buffer
                             (buffer-string))))
                (with-temp-file result-file
                  (prin1 (list :status 'error
                               :text
                               (funcall
                                truncate-output
                                (concat
                                 (format "Error: Eval failed with error %S: %S"
                                         (car err) (cdr err))
                                 (and (> (length (or output "")) 0)
                                      (format "\n\nSTDOUT:\n%s" output)))))
                         (current-buffer))))))
         (when (buffer-live-p stdout-buffer)
           (kill-buffer stdout-buffer)))))))

(defun mevedel-tool-exec--eval-read-batch-result (result-file)
  "Read the batch Eval result plist from RESULT-FILE."
  (when (file-exists-p result-file)
    (with-temp-buffer
      (insert-file-contents result-file)
      (let ((read-eval nil))
        (read (current-buffer))))))

(defun mevedel-tool-exec--eval-batch (callback expression result-format)
  "Evaluate EXPRESSION in a child Emacs process and call CALLBACK."
  (let* ((workdir (mevedel-tool-exec--default-directory))
         (script-file (make-temp-file "mevedel-eval-batch-" nil ".el"))
         (result-file (make-temp-file "mevedel-eval-result-" nil ".el"))
         (script (mevedel-tool-exec--eval-batch-script
                  expression result-file workdir load-path result-format)))
    (condition-case err
        (progn
          (with-temp-file script-file
            (insert script))
          (mevedel-tool-exec--start-child-process
           "mevedel-eval-batch"
           (list (expand-file-name invocation-name invocation-directory)
                 "-Q" "--batch" "-l" script-file)
           workdir nil
           (lambda (child-result)
             (let* ((exit-code (plist-get child-result :exit-code))
                    (diagnostics (plist-get child-result :output))
                    (payload
                     (condition-case nil
                         (mevedel-tool-exec--eval-read-batch-result
                          result-file)
                       (error nil))))
               (unwind-protect
                   (funcall
                    callback
                    (list
                     :result
                     (mevedel-tool-exec--truncate-output
                      (cond
                       ((eq (plist-get payload :status) 'ok)
                        (or (plist-get payload :text) ""))
                       ((eq (plist-get payload :status) 'error)
                        (or (plist-get payload :text) "Error: Eval failed"))
                       ((plist-get child-result :error)
                        (format "Failed to start Eval batch process: %s"
                                (plist-get child-result :error)))
                       (t
                        (format
                         "Error: Eval batch process failed with exit code %d%s"
                         exit-code
                         (if (string-empty-p (or diagnostics ""))
                             ""
                           (format ":\n%s" diagnostics))))))))
                 (ignore-errors (delete-file script-file))
                 (ignore-errors (delete-file result-file)))))))
      (error
       (ignore-errors (delete-file script-file))
       (ignore-errors (delete-file result-file))
       (funcall callback
                (list :result
                      (format "Failed to start Eval batch process: %s" err)))
       nil))))

(defun mevedel-tool-exec--eval (callback args)
  "Evaluate an Elisp expression and return the result.
CALLBACK receives the result envelope.  ARGS is a plist with :expression."
  (let ((expression (plist-get args :expression))
        (result-format (plist-get args :result-format))
        (mode (mevedel-tool-exec--eval-mode args)))
    (unless (stringp expression)
      (error "Parameter expression is required"))
    (pcase mode
      ('live
       (mevedel-tool-exec--eval-live
        callback expression result-format
        (mevedel-tool-exec--eval-preserve-ui-p args)))
      ('batch
       (mevedel-tool-exec--eval-batch callback expression result-format)))))


;;
;;; Renderers

(defun mevedel-tool-exec--render-bash (name args result _render-data)
  "Rendering plist for the Bash tool.
NAME is \"Bash\".  ARGS carries `:command'.  RESULT is stdout/stderr.
Header shows a truncated first line of the command; body fontifies as
`sh-mode'."
  (when (stringp result)
    (let* ((cmd (or (plist-get args :command) ""))
           (first-line (car (split-string cmd "\n")))
           (status (and (or (string-prefix-p "Command failed" result)
                            (string-prefix-p "Command timed out" result)
                            (string-prefix-p "Failed to start" result)
                            (string-prefix-p "Error:" result))
                        'error)))
      (list :header (format "%s: %s" (or name "Bash") first-line)
            :body result
            :body-mode 'sh-mode
            :status status
            :initially-collapsed-p t))))

(defun mevedel-tool-exec--render-eval (name args result _render-data)
  "Return rendering plist for Eval NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((expression (or (plist-get args :expression) ""))
           (first-line (car (split-string expression "\n")))
           (mode (let ((raw (plist-get args :mode)))
                   (if (or (null raw)
                           (eq raw :json-false)
                           (and (stringp raw) (string-empty-p raw)))
                       "live"
                     raw)))
           (status (and (string-prefix-p "Error:" result) 'error)))
      (list :header (format "%s: %s %s"
                            (or name "Eval")
                            mode
                            (truncate-string-to-width
                             first-line 60 nil nil "..."))
            :body result
            :body-mode 'emacs-lisp-mode
            :status status
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
                   "The Bash command to execute from the session working directory. Can include pipes and standard shell operators.")
           (timeout_seconds integer :optional
                            "Optional timeout in seconds. Defaults to `mevedel-bash-timeout' (120 seconds). Must be positive."))
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
    :args ((expression string :required "A single elisp sexp to evaluate with default-directory set to the session working directory.")
           (mode string :optional "Execution mode: live (default) evaluates in the current Emacs; batch evaluates in a child emacs --batch process."
                 :enum ["live" "batch"])
           (preserve_ui boolean :optional "In live mode, restore the current window configuration after evaluation. Defaults to true."))
    :async-p t
    :max-result-size 30000
    :groups (eval)
    :check-permission-async #'mevedel-tool-exec--eval-check-permission-async
    :renderer #'mevedel-tool-exec--render-eval))

(provide 'mevedel-tool-exec)
;;; mevedel-tool-exec.el ends here
