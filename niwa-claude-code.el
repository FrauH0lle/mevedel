;;; niwa-claude-code.el --- Claude subscription adapter setup -*- lexical-binding: t -*-

;;; Commentary:
;; Owns Claude-specific login checks, local launch isolation, and ACP metadata.
;; Subscription tokens stay with the CLI; only hard-expiry metadata is sampled.
;; Shared policy owns explicit fallbacks.

;;; Code:

(require 'niwa-utilities)

(require 'acp)
(require 'cl-lib)
(require 'button)
(require 'json)
(require 'subr-x)
(require 'niwa-claude-code-usage)
(require 'niwa-engine)
(require 'niwa-mcp)
(require 'niwa-claude-code-backend)
(require 'niwa-claude-code-maintenance)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `niwa-acp'
(declare-function niwa-acp--fail "niwa-acp" (connection error))
(declare-function niwa-acp--send "niwa-acp" (connection request success))
(declare-function niwa-acp-state "niwa-acp" (cl-x) t)

;; `niwa-acp-text'
(declare-function niwa-acp-text-request "niwa-acp-text"
                  (launch prompt callback &optional stream ready))
(autoload 'niwa-acp-text-request "niwa-acp-text")

;; `niwa-acp-workload'
(declare-function niwa-acp-workload-request "niwa-acp-workload"
                  (launch prompt tools callback before-tool boundary))
(autoload 'niwa-acp-workload-request "niwa-acp-workload")

;; `niwa-agent-exec'
(declare-function niwa-agent-exec-acknowledge-provider "niwa-agent-exec"
                  (invocation model effort))
(autoload 'niwa-agent-exec-acknowledge-provider "niwa-agent-exec")

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (object))

;; `niwa-claude-code-agent'
(declare-function niwa-claude-code-agent-next-sample
                  "niwa-claude-code-agent" (invocation &optional boundary))

;; `niwa-claude-code-context'
(declare-function niwa-claude-code-context-check "niwa-claude-code-context" (owner))
(declare-function niwa-claude-code-context-hook-fits-p "niwa-claude-code-context" (text))
(declare-function niwa-claude-code-context-next-prompt "niwa-claude-code-context" (owner))
(declare-function niwa-claude-code-context-observe
                  "niwa-claude-code-context" (owner notification))
(declare-function niwa-claude-code-context-prepare
                  "niwa-claude-code-context" (owner &optional prompt-p instructions))
(declare-function niwa-claude-code-context-restore "niwa-claude-code-context" (owner))
(autoload 'niwa-claude-code-context-check "niwa-claude-code-context")
(autoload 'niwa-claude-code-context-hook-fits-p "niwa-claude-code-context")
(autoload 'niwa-claude-code-context-next-prompt "niwa-claude-code-context")
(autoload 'niwa-claude-code-context-observe "niwa-claude-code-context")
(autoload 'niwa-claude-code-context-prepare "niwa-claude-code-context")
(autoload 'niwa-claude-code-context-restore "niwa-claude-code-context")

;; `niwa-goal'
(declare-function niwa-goal-agent-budget-notice "niwa-goal" (fsm))
(declare-function niwa-goal-charge-agent-progress "niwa-goal" (fsm))
(declare-function niwa-goal-tool-result-budget-warnings "niwa-goal" (session fsm))

;; `niwa-reminders'
(declare-function niwa-reminders-queue-turn-event "niwa-reminders" (buffer key body &optional commit))

;; `niwa-structs'
(declare-function niwa-session-root-buffer "niwa-structs" (cl-x) t)
(defvar niwa--agent-invocation)
(defvar niwa--session)

(defun niwa-claude-code-request-text (prompt system callback &optional stream ready)
  "Request isolated text for PROMPT and SYSTEM via CALLBACK, optionally STREAM.
READY runs once the native conversation started and the prompt is submitted."
  (when gptel-tools (error "Isolated text requests cannot use tools"))
  (niwa-acp-text-request
   (niwa-claude-code-launch system [] (gptel--model-name gptel-model)
                               (bound-and-true-p gptel-reasoning-effort))
   prompt callback stream ready))

(defun niwa-claude-code-request-workload (prompt system tools callback before-tool boundary)
  "Run PROMPT with SYSTEM and TOOLS via CALLBACK, BEFORE-TOOL and BOUNDARY."
  (let ((model (gptel--model-name gptel-model)) (effort (bound-and-true-p gptel-reasoning-effort)))
    (niwa-acp-workload-request
     (lambda (mcp hook)
       (plist-put (niwa-claude-code-launch system mcp model effort nil hook)
                  :control #'niwa-claude-code--workload-control))
     prompt tools callback before-tool boundary)))

(defun niwa-claude-code--option (session category)
  "Return SESSION's reported configuration option for CATEGORY."
  (cl-find category (alist-get 'configOptions session)
           :key (lambda (row) (alist-get 'category row)) :test #'equal))

(defun niwa-claude-code--check-model (backend session)
  "Cache BACKEND's SESSION catalog and return its effort option.
The caller has already validated the selected model against SESSION."
  (let* ((models (niwa-claude-code--option session "model"))
         (efforts (niwa-claude-code--option session "thought_level"))
         (rows (alist-get 'options models))
         (current (alist-get 'currentValue models))
         (existing (gptel-backend-models backend))
         (catalog (mapcar (lambda (name) (niwa-claude-code--model name existing))
                          (mapcar #'car niwa-claude-code--aliases))))
    (mapc (lambda (row)
            (let ((name (alist-get 'value row)))
              (unless (and (stringp name) (not (string-empty-p name)))
                (error "Claude returned an invalid model catalog"))
              (let ((entry (niwa-claude-code--model name (append catalog existing))))
                (put entry :description (or (alist-get 'description row) (alist-get 'name row)))
                (unless (memq entry catalog) (setq catalog (append catalog (list entry)))))))
          rows)
    (when-let* ((selected (cl-find current catalog :key #'gptel--model-name :test #'equal)))
      (let ((levels (delq nil (mapcar (lambda (option)
                                        (let ((value (alist-get 'value option)))
                                          (and (stringp value) (not (equal value "default"))
                                               (intern value))))
                                      (alist-get 'options efforts)))))
        (put selected :reasoning-effort (and levels (cons 'member levels)))))
    (setf (gptel-backend-models backend) catalog)
    efforts))

(cl-defun niwa-claude-code--prepare-session (backend model effort buffer connection session ready)
  "Discover MODEL on BACKEND and apply EFFORT before CONNECTION is ready.
SESSION contains the initial capabilities.  READY receives the acknowledged
metadata.  An unavailable selection uses the configured fallback and updates
BUFFER when it owns the root session.  A permission mode inherited from
the user's Claude settings is reset to `default': niwa owns permissions."
  (let* ((row (niwa-claude-code--option session "model"))
         (available (mapcar (lambda (item) (alist-get 'value item)) (alist-get 'options row)))
         recovered)
    (unless (and available (cl-every #'stringp available)
                 (stringp (alist-get 'currentValue row)))
      (user-error "Claude did not report model capabilities; update the connection adapter and retry"))
    (unless (member model available)
      (let* ((owner (and (buffer-live-p buffer) (buffer-local-value 'niwa--session buffer)))
             (provider (niwa-model-recover-provider
                        (format "%s:%s" (gptel-backend-name backend) model) owner t))
             (replacement (and provider (gptel--model-name (plist-get provider :model)))))
        (when (and owner provider (eq buffer (niwa-session-root-buffer owner)))
          (niwa-model-set-session-provider owner provider buffer))
        (unless (and provider (eq backend (plist-get provider :backend)) (member replacement available))
          (user-error "Claude model is unavailable; select an available provider before continuing"))
        (setq model replacement recovered t)))
    (unless (equal model (alist-get 'currentValue row))
      (unless recovered
        (user-error "Claude selected %s instead of %s; select an available model and retry"
                    (alist-get 'currentValue row) model))
      (niwa-acp--send
       connection
       (acp-make-session-set-config-option-request
        :session-id (alist-get 'sessionId session) :config-id (alist-get 'id row) :value model)
       (lambda (response)
         (if (equal model (alist-get 'currentValue (niwa-claude-code--option response "model")))
             (progn
               (setf (alist-get 'configOptions session) (alist-get 'configOptions response))
               (niwa-claude-code--prepare-session backend model effort buffer connection session ready))
           (niwa-acp--fail connection "Claude did not acknowledge the fallback model"))))
      (cl-return-from niwa-claude-code--prepare-session nil)))
  (let* ((option (niwa-claude-code--check-model backend session))
         (mode (niwa-claude-code--option session "mode"))
         (values (mapcar (lambda (row) (alist-get 'value row)) (alist-get 'options option)))
         (value (if (and effort (member (symbol-name effort) values))
                    (symbol-name effort) "default"))
         (fallback (and effort (equal value "default"))))
    (cl-labels
        ((finish ()
           (when (eq (niwa-acp-state connection) 'starting)
             (when fallback
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (eq gptel-backend backend)
                              (equal (gptel--model-name gptel-model) model)
                              (eq gptel-reasoning-effort effort))
                     (setq-local gptel-reasoning-effort nil)
                     (when (and (bound-and-true-p niwa--session)
                                (eq buffer (niwa-session-root-buffer niwa--session)))
                       (niwa-recovery-report niwa--session "effort" 'configuration
                                                "Claude reset unsupported reasoning effort to its default" nil)
                       (niwa-model-set-session-effort niwa--session nil buffer)))))
               (niwa-diagnostic-message
                (or (niwa-acp-diagnostic-scope connection) 'unavailable)
                "Claude %s does not support effort %s; using its default" model effort))
             ;; A retained child reports what Claude acknowledged, including a
             ;; recovered model or its default effort, not what it requested.
             (when-let* ((invocation
                          (and (buffer-live-p buffer)
                               (buffer-local-value 'niwa--agent-invocation buffer))))
               (niwa-agent-exec-acknowledge-provider
                invocation (intern model) (unless (equal value "default") effort)))
             (funcall ready session)))
         (configure (row value failure next)
           (niwa-acp--send
            connection
            (acp-make-session-set-config-option-request
             :session-id (alist-get 'sessionId session)
             :config-id (alist-get 'id row) :value value)
            (lambda (response)
              (when (eq (niwa-acp-state connection) 'starting)
                (let ((actual (cl-find (alist-get 'id row) (alist-get 'configOptions response)
                                       :key (lambda (row) (alist-get 'id row)) :test #'equal)))
                  (if (not (equal value (alist-get 'currentValue actual)))
                      (niwa-acp--fail connection failure)
                    (setf (alist-get 'configOptions session) (alist-get 'configOptions response))
                    (funcall next)))))))
         (apply-effort ()
           (if (not option)
               (finish)
             (unless (and (stringp (alist-get 'id option)) (member value values))
               (error "Claude did not report a usable default effort; update the adapter and retry"))
             ;; Re-read: resetting the permission mode replaced the options.
             (if (equal value (alist-get 'currentValue
                                         (niwa-claude-code--option session "thought_level")))
                 (finish)
               (configure option value "Claude did not acknowledge the selected effort" #'finish)))))
      (if (and mode (not (equal "default" (alist-get 'currentValue mode))))
          (configure mode "default" "Claude did not acknowledge niwa's permission mode"
                     #'apply-effort)
        (apply-effort)))))

(defconst niwa-claude-code--api-environment
  '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
    "CLAUDE_CODE_OAUTH_TOKEN" "CLAUDE_CODE_USE_BEDROCK"
    "CLAUDE_CODE_USE_VERTEX" "CLAUDE_CODE_USE_FOUNDRY")
  "Inherited authentication routes excluded by explicit subscription selection.")

(defvar niwa-claude-code--login-expiry-cache (make-hash-table :test #'equal)
  "Hard login expiry and modification time indexed by credentials file.")

(defun niwa-claude-code--read-login-expiry (file)
  "Return only the Unix expiry time in seconds from credentials FILE."
  (with-temp-buffer
    (insert-file-contents file)
    ;; No other credential field is kept, logged, or returned.
    (let* ((credentials (json-parse-buffer))
           (login (and (hash-table-p credentials) (gethash "claudeAiOauth" credentials)))
           (expiry (and (hash-table-p login) (gethash "refreshTokenExpiresAt" login))))
      (skip-chars-forward " \t\r\n")
      (when (and (eobp) (integerp expiry) (< 0 expiry 1000000000000000))
        (/ expiry 1000.0)))))

(defun niwa-claude-code--login-expiry ()
  "Return the local Claude login's hard expiry in Unix seconds, if exposed."
  (let* ((file (expand-file-name ".credentials.json"
                                 (or (getenv "CLAUDE_CONFIG_DIR") "~/.claude")))
         (mtime (and (not (file-remote-p file))
                     (ignore-errors
                       (when (file-regular-p file)
                         (file-attribute-modification-time
                          (file-attributes (file-truename file)))))))
         (cached (gethash file niwa-claude-code--login-expiry-cache)))
    (cond
     ((not mtime) (remhash file niwa-claude-code--login-expiry-cache) nil)
     ((equal mtime (car cached)) (cdr cached))
     (t
      (let ((expiry (ignore-errors (niwa-claude-code--read-login-expiry file))))
        (puthash file (cons mtime expiry) niwa-claude-code--login-expiry-cache)
        expiry)))))

(defvar niwa-claude-code--version-cache (make-hash-table :test #'equal)
  "Successful executable versions indexed by their resolved file identity.")

(defun niwa-claude-code--version-key (command)
  "Return COMMAND's executable identity and enclosing package metadata.
The package manifest detects npm updates whose launcher file is unchanged."
  (let* ((file (file-truename command))
         (package (locate-dominating-file (file-name-directory file) "package.json")))
    (mapcar (lambda (path)
              (when path
                (let ((attributes (file-attributes path)))
                  (list path (file-attribute-modification-time attributes)
                        (file-attribute-status-change-time attributes)
                        (file-attribute-size attributes) (file-attribute-inode-number attributes)))))
            (list file (when package (file-name-concat package "package.json"))))))

(defun niwa-claude-code--prepare-launch (checks cli environment ready failure)
  "Check executable CHECKS and current CLI login before READY.
Each check is (COMMAND MINIMUM LABEL). Cache only successful stable versions;
authentication always runs with the captured isolated ENVIRONMENT.
FAILURE receives a safe diagnostic and, for a missing sign-in or an outdated
executable, the code `unavailable'.  Return a cancellation function."
  (let (cancel-command finished)
    (cl-labels
        ((cancel ()
           (setq finished t)
           (when cancel-command (funcall cancel-command)))
         (fail (message &optional code)
           (unless finished (cancel) (funcall failure message code)))
         (next ()
           (unless finished
             (condition-case err
                 (let ((process-environment environment)
                       (default-directory temporary-file-directory))
                   (if checks
                       (pcase-let* ((`(,command ,minimum ,label) (pop checks))
                                    (key (niwa-claude-code--version-key command))
                                    (cached (gethash key niwa-claude-code--version-cache)))
                         (if (and cached (version<= minimum cached))
                             (next)
                           (setq cancel-command
                                 (niwa-claude-code--command-output-async
                                  command '("--version")
                                  (lambda (output)
                                    (unless finished
                                      (condition-case err
                                          (let ((version (niwa-claude-code--version output minimum)))
                                            (if (not version)
                                                (fail (format "%s %s or newer is required" label minimum)
                                                      'unavailable)
                                              (when (equal key (niwa-claude-code--version-key command))
                                                (puthash key version niwa-claude-code--version-cache))
                                              (next)))
                                        (error (fail (error-message-string err)))))) #'fail))))
                     (setq cancel-command
                           (niwa-claude-code--command-output-async
                            ;; Logged-out status exits 1 with valid JSON.
                            cli '("auth" "status" "--json")
                            (lambda (output)
                              (unless finished
                                (let ((status (condition-case nil
                                                  (json-parse-string output :object-type 'plist :false-object :json-false)
                                                (error (fail "Claude returned an invalid authentication status; run `claude auth status --json' in a terminal") nil))))
                                  (unless finished
                                      (if (and (eq t (plist-get status :loggedIn))
                                               (equal "claude.ai" (plist-get status :authMethod))
                                               (equal "firstParty" (plist-get status :apiProvider))
                                               (member (plist-get status :subscriptionType) '("pro" "max" "team" "enterprise")))
                                          (progn (setq finished t) (funcall ready))
                                        (fail "Claude subscription login required; run `claude auth login' and select your Claude account"
                                              'unavailable))))))
                            #'fail t))))
               (error (fail (error-message-string err)))))))
      (next)
      #'cancel)))

(defun niwa-claude-code--wait (start)
  "Synchronously wait for an explicit setup command's asynchronous check.
START receives success and failure callbacks and returns a canceller.
Return the success value; signal the failure diagnostic as a `user-error'."
  (let (done value failure)
    (let ((cancel (funcall start
                           (lambda (&optional result) (setq value result done t))
                           (lambda (message &optional _code) (setq failure message done t)))))
      (unwind-protect
          (progn
            (while (not done) (accept-process-output nil 0.02))
            (when failure (user-error "%s" failure))
            value)
        (funcall cancel)))))

;;;###autoload
(defun niwa-claude-code-setup ()
  "Check the subscription installation and display setup instructions.
Use the same preflight as a normal turn without starting a model request.
Return the setup buffer."
  (interactive)
  (let ((status (condition-case err
                    (progn
                      (niwa-mcp-bridge-file)
                      (niwa-claude-code--wait
                       (plist-get (niwa-claude-code-launch "Setup check" [] "sonnet" nil)
                                  :prepare-launch))
                      "Ready for Claude Code subscription sessions.")
                  (error (niwa-diagnostic-format 'global "%s" (error-message-string err)))))
        (buffer (get-buffer-create "*niwa Claude Code setup*")))
    (niwa-claude-code-register)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Claude Code subscription setup\n\n" status "\n\n"
                (format "1. Install Claude Code %s or newer, Node.js 22 or newer,\n"
                        niwa-claude-code--cli-version)
                "   and Python 3.8 or newer on the Emacs host. Emacs must find\n"
                "   their executables in exec-path.\n\n"
                "2. Run `claude auth login' in a terminal and select your Claude\n"
                "   Pro or Max account. Team and Enterprise accounts also work.\n\n"
                "3. Install the connection adapter, then refresh this check.\n\n"
                "4. Start a niwa session and select Claude Code:sonnet,\n"
                "   Claude Code:opus, Claude Code:fable or Claude Code:haiku in the model menu.\n"
                "   Niwa supplies the tool connection automatically.\n\n")
        (if niwa-claude-code-adapter-executable
            (progn
              (insert (format "Custom adapter: %s\n" niwa-claude-code-adapter-executable))
              (insert-text-button "Change adapter setting" 'follow-link t
                                  'action (lambda (_) (customize-variable 'niwa-claude-code-adapter-executable))))
          (insert-text-button "Install/update adapter" 'follow-link t
                              'action (lambda (_) (niwa-claude-code-install-adapter))))
        (insert "\n\n")
        (insert-text-button "Refresh setup check" 'follow-link t
                            'action (lambda (_) (niwa-claude-code-setup)))
        (insert "\n\n")
        (insert-text-button "Claude Code installation instructions" 'follow-link t
                            'action (lambda (_) (browse-url "https://code.claude.com/docs/en/setup")))
        (insert "\n\n" (format "Adapter and conversation directory: %s\n" niwa-claude-code-directory))
        (goto-char (point-min))
        (help-mode)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun niwa-claude-code-install-adapter ()
  "Check and install Claude CLI and adapter updates asynchronously.
The CLI follows its own configured release channel.
The echo area reports a check already running and the result."
  (interactive)
  (niwa-claude-code-maintenance-check t))

(defun niwa-claude-code--observe (owner notification)
  "Observe native usage and context receipts for admitted OWNER."
  (niwa-claude-code-usage-observe owner notification)
  (when (niwa-agent-invocation-p owner)
    (niwa-goal-charge-agent-progress owner))
  (niwa-claude-code-context-observe owner notification))

(defun niwa-claude-code--hook-event (event)
  "Return EVENT's hook name, rejecting hooks niwa did not configure."
  (let ((name (plist-get event :hook_event_name)))
    (unless (or (member name '("PostToolBatch" "PreToolUse"))
                (and (equal "SessionStart" name) (equal "compact" (plist-get event :source))))
      (error "Unsupported Claude hook event"))
    name))

(defun niwa-claude-code--workload-control (boundary event)
  "Return the native hook decision for an isolated workload's EVENT.
BOUNDARY returns reminder text, or nil, after each completed tool batch."
  (let ((context (when (equal "PostToolBatch" (niwa-claude-code--hook-event event))
                   (funcall boundary))))
    (when (and context (not (and (stringp context) (niwa-claude-code-context-hook-fits-p context))))
      (error "Claude hook context exceeds its supported size"))
    (if context
        (list :hookSpecificOutput (list :hookEventName "PostToolBatch" :additionalContext context))
      '(:continue t))))

(defun niwa-claude-code--control (request event)
  "Return the native hook decision for admitted REQUEST and Claude EVENT."
  (let* ((name (niwa-claude-code--hook-event event))
         (decision
          (progn
            (when (equal "PostToolBatch" name)
              (let* ((info (niwa-engine-info request))
                     (buffer (plist-get info :buffer))
                     (warnings
                      (if (niwa-request-p request)
                          (niwa-goal-tool-result-budget-warnings (niwa-request-session request) request)
                        (when-let* ((notice (niwa-goal-agent-budget-notice request)))
                          (list (plist-put notice :key 'goal-budget))))))
                (dolist (warning warnings)
                  (niwa-reminders-queue-turn-event
                   buffer (plist-get warning :key) (plist-get warning :body) (plist-get warning :commit)))))
            (when (and (niwa-agent-invocation-p request)
                       (equal "PostToolBatch" name)
                       (not (plist-get (niwa-engine-info request) :niwa-end-turn))
                       (not (plist-get (niwa-engine-info request) :niwa-cancelled)))
              (niwa-claude-code-agent-next-sample request t))
            (let ((reason (plist-get (niwa-engine-info request) :niwa-end-turn)))
              (if (or reason (if (niwa-request-p request)
                                 (niwa-request-cancelled-p request)
                               (plist-get (niwa-engine-info request) :niwa-cancelled)))
                  (list :continue :json-false
                        :stopReason (format "Niwa ended this turn: %s" (or reason 'interrupted)))
                (if-let* ((context
                           (pcase name
                             ("SessionStart" (niwa-claude-code-context-restore request))
                             ("PostToolBatch" (niwa-claude-code-context-prepare request))
                             ("PreToolUse"
                              ;; SessionStart(compact) does not reliably honor a stop.
                              ;; Stop again before tool dispatch until the full context
                              ;; can be delivered through the next native prompt.
                              (cl-loop for key in '(:niwa-claude-context-pending
                                                    :niwa-claude-restoration-pending)
                                       for pending = (plist-get (niwa-engine-info request) key)
                                       when (eq 'continuation (plist-get pending :route))
                                       return (plist-get pending :body))))))
                    (if (or (not (niwa-claude-code-context-hook-fits-p context))
                            ;; Only an undelivered continuation reaches here.
                            (equal "PreToolUse" name)
                            ;; Authored steering cannot ride hook context.
                            (and (equal "PostToolBatch" name)
                                 (eq 'continuation
                                     (plist-get (plist-get (niwa-engine-info request)
                                                           :niwa-claude-context-pending)
                                                :route))))
                        (progn
                          (when (and (niwa-agent-invocation-p request)
                                     (member name
                                             '("SessionStart" "PreToolUse")))
                            ;; SessionStart cannot stop the next sample; a denied
                            ;; tool also spent its sample.  Reserve another only
                            ;; after successful settlement, even for text alone.
                            (setf (niwa-engine-info request)
                                  (plist-put (niwa-engine-info request)
                                             :niwa-claude-continuation-sample-spent t)))
                          ;; The model reads this beside the continuation prompt;
                          ;; a "wait" here made it end the turn after steering.
                          (list :continue :json-false :stopReason "Paused only to deliver input as a prompt; the turn has not ended. Continue the work as that input directs."))
                      (list :hookSpecificOutput
                            (list :hookEventName name :additionalContext context)))
                  '(:continue t)))))))
    (when (and (equal "PreToolUse" name)
               (eq :json-false (plist-get decision :continue)))
      (setq decision
            (plist-put decision :hookSpecificOutput
                       (list :hookEventName "PreToolUse" :permissionDecision "deny"
                             :permissionDecisionReason (plist-get decision :stopReason)))))
    decision))

(defun niwa-claude-code--complete-prompt (owner outcome)
  "Account for OWNER's native OUTCOME and prepare required context continuation."
  (setq outcome (niwa-claude-code-usage-complete owner outcome))
  (when (niwa-agent-invocation-p owner)
    (niwa-goal-charge-agent-progress owner))
  (when (eq 'success (plist-get outcome :status))
    (when (and (niwa-agent-invocation-p owner)
               (plist-get (niwa-engine-info owner) :niwa-claude-continuation-sample-spent))
      (setf (niwa-engine-info owner)
            (plist-put (niwa-engine-info owner) :niwa-claude-continuation-sample-spent nil))
      (unless (or (plist-get (niwa-engine-info owner) :niwa-end-turn)
                  (plist-get (niwa-engine-info owner) :niwa-cancelled))
        (niwa-claude-code-agent-next-sample owner t)))
    (when-let* ((content (niwa-claude-code-context-next-prompt owner)))
      (let ((info (niwa-engine-info owner)))
        (setq info (plist-put info :niwa-claude-usage-base (copy-sequence (plist-get info :tokens-full))))
        (setq info (plist-put info :niwa-claude-finished-samples
                              (append (mapcar #'car (plist-get info :niwa-claude-sample-usage))
                                      (plist-get info :niwa-claude-finished-samples))))
        (setq info (plist-put info :niwa-claude-sample-id nil))
        (setq info (plist-put info :niwa-claude-prompt-usage nil))
        (setq info (plist-put info :niwa-usage-pending t))
        (setf (niwa-engine-info owner) (plist-put info :niwa-claude-sample-usage nil)))
      (setq outcome (plist-put outcome :next-prompt content))))
  (setq outcome (plist-put outcome :queued-handover
                           (and (plist-get (plist-get (niwa-engine-info owner) :niwa-claude-context-pending)
                                           :steering-text)
                                t)))
  outcome)

(defun niwa-claude-code--outcome (outcome)
  "Normalize Claude adapter prompt OUTCOME without inventing unknown usage.
The adapter resets its usage tally on prompt activation.  Cache creation is
part of gptel's normalized input; cache reads remain separate.  Quota model
rows include internal work and are not another copy of these prompt totals."
  (let* ((response (plist-get outcome :response))
         (usage (alist-get 'usage response))
         (input (alist-get 'inputTokens usage))
         (output (alist-get 'outputTokens usage))
         (cached (alist-get 'cachedReadTokens usage))
         (cache (alist-get 'cachedWriteTokens usage))
         tokens)
    (when (and (natnump input) (natnump cache))
      (setq tokens (list :input (+ input cache))))
    (when (natnump output) (setq tokens (append tokens (list :output output))))
    (when (natnump cached) (setq tokens (append tokens (list :cached cached))))
    (when (natnump cache) (setq tokens (append tokens (list :cache cache))))
    (plist-put outcome :tokens tokens)))

(defun niwa-claude-code--prompt-retry-delay (failure)
  "Return seconds to wait before re-sending a prompt that met FAILURE, or nil.
Every Claude process on the host shares one credential store.  A process that
needs a refresh while another holds the refresh lock, or left it stale by
exiting, fails before any model request; the lock clears within a minute."
  (when-let* ((message (alist-get 'message failure))
              ((stringp message))
              ((string-search "another Claude Code process is refreshing it" message)))
    30))

(defun niwa-claude-code--runtime-commands (runtime)
  "Return the local executables a Claude launch needs, given maintained RUNTIME.
The plist has :cli, :adapter, :node and :python.  Every refusal is a
`user-error' naming the missing or outdated runtime piece."
  (let ((cli (or (and (plist-get runtime :cli) (file-executable-p (plist-get runtime :cli))
                      (plist-get runtime :cli))
                 (executable-find niwa-claude-code-executable)
                 (user-error "Install Claude Code and run `claude auth login'")))
        (adapter
         (or (and niwa-claude-code-adapter-executable
                  (executable-find niwa-claude-code-adapter-executable))
             (and (not niwa-claude-code-adapter-executable)
                  (or (and (plist-get runtime :adapter) (file-executable-p (plist-get runtime :adapter))
                           (plist-get runtime :adapter))
                      (executable-find "claude-agent-acp")))
             (user-error "Run M-x niwa-claude-code-install-adapter to install the Claude connection adapter"))))
    (unless (version<= "0.15.2" acp-package-version)
      (user-error "Upgrade the Emacs acp package to 0.15.2 or newer"))
    (list :cli cli :adapter adapter
          :node (or (executable-find "node") (user-error "Install Node.js 22 or newer on the Emacs host"))
          :python (or (executable-find "python3") (user-error "Install Python 3.8 or newer on the Emacs host")))))

(defun niwa-claude-code-unavailable-reason ()
  "Return why no Claude conversation can start on this host, or nil.
Only a missing or outdated local runtime is reported here; sign-in is checked
asynchronously by each launch."
  ;; A remote state directory is a configuration error the launch reports.
  (unless (file-remote-p niwa-claude-code-directory)
    (let ((default-directory temporary-file-directory))
      (condition-case err
          (progn (niwa-claude-code--runtime-commands (niwa-claude-code-maintenance-state))
                 nil)
        (user-error (error-message-string err))))))

(defun niwa-claude-code-launch (system mcp model effort &optional session-id hook-command)
  "Prepare an authenticated Claude ACP launch for SYSTEM and MCP servers.
MODEL is the explicit Claude model name.  EFFORT is a supported effort symbol
or nil.  SESSION-ID resumes the installed CLI's history on this machine.
HOOK-COMMAND is the generated private bridge for request boundary decisions.
Return the generic ACP launch plist; no model request is made here."
  (unless (and (stringp system) (stringp model))
    (error "Claude launch requires a system prompt and model"))
  (unless (symbolp effort)
    (user-error "Claude Code does not support reasoning effort %S" effort))
  (when (file-remote-p niwa-claude-code-directory)
    (user-error "Claude Code state must be on the local machine"))
  (niwa-claude-code-maintenance-check)
  (let* ((runtime (niwa-claude-code-maintenance-state))
         (directory (expand-file-name niwa-claude-code-directory))
         (commands (niwa-claude-code--runtime-commands runtime))
         (cli (plist-get commands :cli))
         (adapter (plist-get commands :adapter))
         (process-environment (copy-sequence process-environment))
         (default-directory temporary-file-directory))
    (dolist (name niwa-claude-code--api-environment) (setenv name nil))
    (setenv "CLAUDE_CODE_EXECUTABLE" cli)
    (when (and niwa-claude-code-agent-log-directory
               (not (getenv "CLAUDE_AGENT_LOGS")))
      (setenv "CLAUDE_AGENT_LOGS" (expand-file-name niwa-claude-code-agent-log-directory)))
    ;; The adapter also resolves a model from settings before session/new
    ;; completes.  Its environment pin must agree with the SDK option.
    (setenv "ANTHROPIC_MODEL" model)
    ;; Configure effort only after discovery, including an explicit default.
    (setenv "CLAUDE_CODE_EFFORT_LEVEL" nil)
    (let* ((checks (list
                    (list (plist-get commands :node) "22.0.0" "Node.js")
                    (list (plist-get commands :python) "3.8.0" "Python")
                    (list cli niwa-claude-code--cli-version "Claude Code")
                    (list adapter niwa-claude-code--adapter-version "Claude ACP adapter")))
           (environment process-environment)
           (backend (niwa-claude-code-register))
           (buffer (current-buffer))
           (cwd (file-name-concat directory "conversations"))
           (settings '((autoMemoryEnabled . :false)))
           (options `((tools . []) (model . ,model)
                      (systemPrompt . ((type . "custom") (prompt . ,system) (snapshot . :false)))
                      (settingSources . []) (strictMcpConfig . t)
                      (includeHookEvents . t)
                      (allowedTools . ["mcp__niwa__*"])
                      (extraArgs . ((disable-slash-commands . ""))))))
      (when hook-command
        (let ((hooks `[((type . "command") (command . ,hook-command) (timeout . 35))]))
          (setq settings
                (append settings
                        `((hooks . ((PreToolUse . [((hooks . ,hooks))])
                                    (PostToolBatch . [((hooks . ,hooks))])
                                    (SessionStart . [((matcher . "compact") (hooks . ,hooks))]))))))))
      (make-directory cwd t)
      (set-file-modes cwd #o700)
      (list :command adapter :cwd cwd :environment process-environment
            :mcp mcp :session-id session-id
            :agent-name "Claude Code"
            :auth-hint "Run `claude auth login' in a terminal."
            :auth-expiry #'niwa-claude-code--login-expiry
            :auth-warn-days niwa-claude-code-login-warning-days
            :prepare-launch
            (lambda (ready failure)
              (niwa-claude-code--prepare-launch checks cli environment ready failure))
            :prepare-session
            (lambda (connection session ready)
              (niwa-claude-code--prepare-session backend model effort buffer connection session ready))
            :tool-id-field :claudecode/toolUseId
            :announces-tool-calls t
            :normalize-outcome #'niwa-claude-code--outcome
            :prompt-retry-delay #'niwa-claude-code--prompt-retry-delay
            :complete-prompt #'niwa-claude-code--complete-prompt
            :control #'niwa-claude-code--control
            :observe #'niwa-claude-code--observe
            :check-context #'niwa-claude-code-context-check
            :meta
            `((claudeCode . ((emitRawSDKMessages . t)
                             (options . ,(append options
                                                 `((settings . ,settings)))))))))))

(defun niwa-claude-code-usage-launch (model)
  "Prepare an isolated native subscription usage inspection for MODEL.
Retain ordinary launch isolation, enabling only the native-command exception.
Model configuration is unnecessary for this local command and must not update
its caller's selected model or backend catalog."
  (let* ((launch (niwa-claude-code-launch "Subscription usage inspection" [] model nil))
         (meta (copy-tree (plist-get launch :meta)))
         (options (alist-get 'options (alist-get 'claudeCode meta))))
    (setf (plist-get launch :meta) meta
          (alist-get 'extraArgs options)
          (assq-delete-all 'disable-slash-commands (alist-get 'extraArgs options))
          (plist-get launch :prepare-session) nil
          (plist-get launch :required-command) "usage"
          (plist-get launch :response-timeout) 30)
    launch))

(provide 'niwa-claude-code)
;;; niwa-claude-code.el ends here
