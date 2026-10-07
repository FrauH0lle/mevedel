;;; mevedel-claude-code.el --- Claude subscription adapter setup -*- lexical-binding: t -*-

;;; Commentary:
;; Owns Claude-specific login checks, local launch isolation, and ACP metadata.
;; Subscription credentials remain entirely inside the installed Claude CLI.
;; Credentials stay in the native CLI; shared policy owns explicit fallbacks.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'button)
(require 'json)
(require 'subr-x)
(require 'mevedel-claude-code-usage)
(require 'mevedel-engine)
(require 'mevedel-mcp)
(require 'mevedel-claude-code-backend)
(require 'mevedel-claude-code-maintenance)

;; `gptel'
(defvar gptel-reasoning-effort)

;; `mevedel-acp'
(declare-function mevedel-acp--fail "mevedel-acp" (connection error))
(declare-function mevedel-acp--send "mevedel-acp" (connection request success))
(declare-function mevedel-acp-state "mevedel-acp" (cl-x) t)

;; `mevedel-acp-text'
(declare-function mevedel-acp-text-request "mevedel-acp-text"
                  (launch prompt callback &optional stream))
(autoload 'mevedel-acp-text-request "mevedel-acp-text")

;; `mevedel-acp-workload'
(declare-function mevedel-acp-workload-request "mevedel-acp-workload"
                  (launch prompt tools callback before-tool boundary))
(autoload 'mevedel-acp-workload-request "mevedel-acp-workload")

;; `mevedel-agents'
(declare-function mevedel-agent-invocation-p "mevedel-agents" (object))

;; `mevedel-claude-code-agent'
(declare-function mevedel-claude-code-agent-next-sample
                  "mevedel-claude-code-agent" (invocation &optional boundary))

;; `mevedel-claude-code-context'
(declare-function mevedel-claude-code-context-check "mevedel-claude-code-context" (owner))
(declare-function mevedel-claude-code-context-hook-fits-p "mevedel-claude-code-context" (text))
(declare-function mevedel-claude-code-context-next-prompt "mevedel-claude-code-context" (owner))
(declare-function mevedel-claude-code-context-observe
                  "mevedel-claude-code-context" (owner notification))
(declare-function mevedel-claude-code-context-prepare
                  "mevedel-claude-code-context" (owner &optional prompt-p instructions))
(declare-function mevedel-claude-code-context-restore "mevedel-claude-code-context" (owner))
(autoload 'mevedel-claude-code-context-check "mevedel-claude-code-context")
(autoload 'mevedel-claude-code-context-hook-fits-p "mevedel-claude-code-context")
(autoload 'mevedel-claude-code-context-next-prompt "mevedel-claude-code-context")
(autoload 'mevedel-claude-code-context-observe "mevedel-claude-code-context")
(autoload 'mevedel-claude-code-context-prepare "mevedel-claude-code-context")
(autoload 'mevedel-claude-code-context-restore "mevedel-claude-code-context")

;; `mevedel-goal'
(declare-function mevedel-goal-agent-budget-notice "mevedel-goal" (fsm))
(declare-function mevedel-goal-charge-agent-progress "mevedel-goal" (fsm))
(declare-function mevedel-goal-tool-result-budget-warnings "mevedel-goal" (session fsm))

;; `mevedel-reminders'
(declare-function mevedel-reminders-queue-turn-event "mevedel-reminders" (buffer key body &optional commit))

;; `mevedel-structs'
(declare-function mevedel-session-root-buffer "mevedel-structs" (cl-x) t)
(defvar mevedel--session)

(defun mevedel-claude-code-request-text (prompt system callback &optional stream)
  "Request isolated text for PROMPT and SYSTEM via CALLBACK, optionally STREAM."
  (when gptel-tools (error "Isolated text requests cannot use tools"))
  (mevedel-acp-text-request
   (mevedel-claude-code-launch system [] (gptel--model-name gptel-model)
                               (bound-and-true-p gptel-reasoning-effort))
   prompt callback stream))

(defun mevedel-claude-code-request-workload (prompt system tools callback before-tool boundary)
  "Run PROMPT with SYSTEM and TOOLS via CALLBACK, BEFORE-TOOL and BOUNDARY."
  (let ((model (gptel--model-name gptel-model)) (effort (bound-and-true-p gptel-reasoning-effort)))
    (mevedel-acp-workload-request
     (lambda (mcp hook)
       (plist-put (mevedel-claude-code-launch system mcp model effort nil hook)
                  :control #'mevedel-claude-code--workload-control))
     prompt tools callback before-tool boundary)))

(defun mevedel-claude-code--option (session category)
  "Return SESSION's reported configuration option for CATEGORY."
  (cl-find category (alist-get 'configOptions session)
           :key (lambda (row) (alist-get 'category row)) :test #'equal))

(defun mevedel-claude-code--check-model (backend session)
  "Cache BACKEND's SESSION catalog and return its effort option.
The caller has already validated the selected model against SESSION."
  (let* ((models (mevedel-claude-code--option session "model"))
         (efforts (mevedel-claude-code--option session "thought_level"))
         (rows (alist-get 'options models))
         (current (alist-get 'currentValue models))
         (existing (gptel-backend-models backend))
         (catalog (mapcar (lambda (name) (mevedel-claude-code--model name existing))
                          (mapcar #'car mevedel-claude-code--aliases))))
    (mapc (lambda (row)
            (let ((name (alist-get 'value row)))
              (unless (and (stringp name) (not (string-empty-p name)))
                (error "Claude returned an invalid model catalog"))
              (let ((entry (mevedel-claude-code--model name (append catalog existing))))
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

(cl-defun mevedel-claude-code--prepare-session (backend model effort buffer connection session ready)
  "Discover MODEL on BACKEND and apply EFFORT before CONNECTION is ready.
SESSION contains the initial capabilities.  READY receives the acknowledged
metadata.  An unavailable selection uses the configured fallback and updates
BUFFER when it owns the root session.  A permission mode inherited from
the user's Claude settings is reset to `default': mevedel owns permissions."
  (let* ((row (mevedel-claude-code--option session "model"))
         (available (mapcar (lambda (item) (alist-get 'value item)) (alist-get 'options row)))
         recovered)
    (unless (and available (cl-every #'stringp available)
                 (stringp (alist-get 'currentValue row)))
      (user-error "Claude did not report model capabilities; update the connection adapter and retry"))
    (unless (member model available)
      (let* ((owner (and (buffer-live-p buffer) (buffer-local-value 'mevedel--session buffer)))
             (provider (mevedel-model-recover-provider
                        (format "%s:%s" (gptel-backend-name backend) model) owner t))
             (replacement (and provider (gptel--model-name (plist-get provider :model)))))
        (when (and owner provider (eq buffer (mevedel-session-root-buffer owner)))
          (mevedel-model-set-session-provider owner provider buffer))
        (unless (and provider (eq backend (plist-get provider :backend)) (member replacement available))
          (user-error "Claude model is unavailable; select an available provider before continuing"))
        (setq model replacement recovered t)))
    (unless (equal model (alist-get 'currentValue row))
      (unless recovered
        (user-error "Claude selected %s instead of %s; select an available model and retry"
                    (alist-get 'currentValue row) model))
      (mevedel-acp--send
       connection
       (acp-make-session-set-config-option-request
        :session-id (alist-get 'sessionId session) :config-id (alist-get 'id row) :value model)
       (lambda (response)
         (if (equal model (alist-get 'currentValue (mevedel-claude-code--option response "model")))
             (progn
               (setf (alist-get 'configOptions session) (alist-get 'configOptions response))
               (mevedel-claude-code--prepare-session backend model effort buffer connection session ready))
           (mevedel-acp--fail connection "Claude did not acknowledge the fallback model"))))
      (cl-return-from mevedel-claude-code--prepare-session nil)))
  (let* ((option (mevedel-claude-code--check-model backend session))
         (mode (mevedel-claude-code--option session "mode"))
         (values (mapcar (lambda (row) (alist-get 'value row)) (alist-get 'options option)))
         (value (if (and effort (member (symbol-name effort) values))
                    (symbol-name effort) "default"))
         (fallback (and effort (equal value "default"))))
    (cl-labels
        ((finish ()
           (when (eq (mevedel-acp-state connection) 'starting)
             (when fallback
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (eq gptel-backend backend)
                              (equal (gptel--model-name gptel-model) model)
                              (eq gptel-reasoning-effort effort))
                     (setq-local gptel-reasoning-effort nil)
                     (when (and (bound-and-true-p mevedel--session)
                                (eq buffer (mevedel-session-root-buffer mevedel--session)))
                       (mevedel-recovery-report mevedel--session "effort" 'configuration
                                                "Claude reset unsupported reasoning effort to its default" nil)
                       (mevedel-model-set-session-effort mevedel--session nil buffer)))))
               (message "mevedel: Claude %s does not support effort %s; using its default" model effort))
             (funcall ready session)))
         (configure (row value failure next)
           (mevedel-acp--send
            connection
            (acp-make-session-set-config-option-request
             :session-id (alist-get 'sessionId session)
             :config-id (alist-get 'id row) :value value)
            (lambda (response)
              (when (eq (mevedel-acp-state connection) 'starting)
                (let ((actual (cl-find (alist-get 'id row) (alist-get 'configOptions response)
                                       :key (lambda (row) (alist-get 'id row)) :test #'equal)))
                  (if (not (equal value (alist-get 'currentValue actual)))
                      (mevedel-acp--fail connection failure)
                    (setf (alist-get 'configOptions session) (alist-get 'configOptions response))
                    (funcall next)))))))
         (apply-effort ()
           (if (not option)
               (finish)
             (unless (and (stringp (alist-get 'id option)) (member value values))
               (error "Claude did not report a usable default effort; update the adapter and retry"))
             ;; Re-read: resetting the permission mode replaced the options.
             (if (equal value (alist-get 'currentValue
                                         (mevedel-claude-code--option session "thought_level")))
                 (finish)
               (configure option value "Claude did not acknowledge the selected effort" #'finish)))))
      (if (and mode (not (equal "default" (alist-get 'currentValue mode))))
          (configure mode "default" "Claude did not acknowledge mevedel's permission mode"
                     #'apply-effort)
        (apply-effort)))))

(defconst mevedel-claude-code--api-environment
  '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
    "CLAUDE_CODE_OAUTH_TOKEN" "CLAUDE_CODE_USE_BEDROCK"
    "CLAUDE_CODE_USE_VERTEX" "CLAUDE_CODE_USE_FOUNDRY")
  "Inherited authentication routes excluded by explicit subscription selection.")

(defvar mevedel-claude-code--version-cache (make-hash-table :test #'equal)
  "Successful executable versions indexed by their resolved file identity.")

(defun mevedel-claude-code--version-key (command)
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

(defun mevedel-claude-code--prepare-launch (checks cli environment ready failure)
  "Check executable CHECKS and current CLI login before READY.
Each check is (COMMAND MINIMUM LABEL). Cache only successful stable versions;
authentication always runs with the captured isolated ENVIRONMENT.
FAILURE receives a safe diagnostic. Return a cancellation function."
  (let (cancel-command finished)
    (cl-labels
        ((cancel ()
           (setq finished t)
           (when cancel-command (funcall cancel-command)))
         (fail (message)
           (unless finished (cancel) (funcall failure message)))
         (next ()
           (unless finished
             (condition-case err
                 (let ((process-environment environment)
                       (default-directory temporary-file-directory))
                   (if checks
                       (pcase-let* ((`(,command ,minimum ,label) (pop checks))
                                    (key (mevedel-claude-code--version-key command))
                                    (cached (gethash key mevedel-claude-code--version-cache)))
                         (if (and cached (version<= minimum cached))
                             (next)
                           (setq cancel-command
                                 (mevedel-claude-code--command-output-async
                                  command '("--version")
                                  (lambda (output)
                                    (unless finished
                                      (condition-case err
                                          (let ((version (mevedel-claude-code--version output minimum)))
                                            (if (not version)
                                                (fail (format "%s %s or newer is required" label minimum))
                                              (when (equal key (mevedel-claude-code--version-key command))
                                                (puthash key version mevedel-claude-code--version-cache))
                                              (next)))
                                        (error (fail (error-message-string err)))))) #'fail))))
                     (setq cancel-command
                           (mevedel-claude-code--command-output-async
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
                                        (fail "Claude subscription login required; run `claude auth login' and select your Claude account"))))))
                            #'fail t))))
               (error (fail (error-message-string err)))))))
      (next)
      #'cancel)))

(defun mevedel-claude-code--wait (start)
  "Synchronously wait for an explicit setup command's asynchronous check.
START receives success and failure callbacks and returns a canceller.
Return the success value; signal the failure diagnostic as a `user-error'."
  (let (done value failure)
    (let ((cancel (funcall start
                           (lambda (&optional result) (setq value result done t))
                           (lambda (message) (setq failure message done t)))))
      (unwind-protect
          (progn
            (while (not done) (accept-process-output nil 0.02))
            (when failure (user-error "%s" failure))
            value)
        (funcall cancel)))))

;;;###autoload
(defun mevedel-claude-code-setup ()
  "Check the subscription installation and display setup instructions.
Use the same preflight as a normal turn without starting a model request.
Return the setup buffer."
  (interactive)
  (let ((status (condition-case err
                    (progn
                      (mevedel-mcp-bridge-file)
                      (mevedel-claude-code--wait
                       (plist-get (mevedel-claude-code-launch "Setup check" [] "sonnet" nil)
                                  :prepare-launch))
                      "Ready for Claude Code subscription sessions.")
                  (error (error-message-string err))))
        (buffer (get-buffer-create "*mevedel Claude Code setup*")))
    (mevedel-claude-code-register)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Claude Code subscription setup\n\n" status "\n\n"
                (format "1. Install Claude Code %s or newer, Node.js 22 or newer,\n"
                        mevedel-claude-code--cli-version)
                "   and Python 3.8 or newer on the Emacs host. Emacs must find\n"
                "   their executables in exec-path.\n\n"
                "2. Run `claude auth login' in a terminal and select your Claude\n"
                "   Pro or Max account. Team and Enterprise accounts also work.\n\n"
                "3. Install the connection adapter, then refresh this check.\n\n"
                "4. Start a mevedel session and select Claude Code:sonnet,\n"
                "   Claude Code:opus, Claude Code:fable or Claude Code:haiku in the model menu.\n"
                "   Mevedel supplies the tool connection automatically.\n\n")
        (if mevedel-claude-code-adapter-executable
            (progn
              (insert (format "Custom adapter: %s\n" mevedel-claude-code-adapter-executable))
              (insert-text-button "Change adapter setting" 'follow-link t
                                  'action (lambda (_) (customize-variable 'mevedel-claude-code-adapter-executable))))
          (insert-text-button "Install/update adapter" 'follow-link t
                              'action (lambda (_) (mevedel-claude-code-install-adapter))))
        (insert "\n\n")
        (insert-text-button "Refresh setup check" 'follow-link t
                            'action (lambda (_) (mevedel-claude-code-setup)))
        (insert "\n\n")
        (insert-text-button "Claude Code installation instructions" 'follow-link t
                            'action (lambda (_) (browse-url "https://code.claude.com/docs/en/setup")))
        (insert "\n\n" (format "Adapter and conversation directory: %s\n" mevedel-claude-code-directory))
        (goto-char (point-min))
        (help-mode)))
    (display-buffer buffer)
    buffer))

;;;###autoload
(defun mevedel-claude-code-install-adapter ()
  "Check and install Claude CLI and adapter updates asynchronously.
The CLI follows its own configured release channel.
The echo area reports a check already running and the result."
  (interactive)
  (mevedel-claude-code-maintenance-check t))

(defun mevedel-claude-code--observe (owner notification)
  "Observe native usage and context receipts for admitted OWNER."
  (mevedel-claude-code-usage-observe owner notification)
  (when (mevedel-agent-invocation-p owner)
    (mevedel-goal-charge-agent-progress owner))
  (mevedel-claude-code-context-observe owner notification))

(defun mevedel-claude-code--hook-event (event)
  "Return EVENT's hook name, rejecting hooks mevedel did not configure."
  (let ((name (plist-get event :hook_event_name)))
    (unless (or (member name '("PostToolBatch" "PreToolUse"))
                (and (equal "SessionStart" name) (equal "compact" (plist-get event :source))))
      (error "Unsupported Claude hook event"))
    name))

(defun mevedel-claude-code--workload-control (boundary event)
  "Return the native hook decision for an isolated workload's EVENT.
BOUNDARY returns reminder text, or nil, after each completed tool batch."
  (let ((context (when (equal "PostToolBatch" (mevedel-claude-code--hook-event event))
                   (funcall boundary))))
    (when (and context (not (and (stringp context) (mevedel-claude-code-context-hook-fits-p context))))
      (error "Claude hook context exceeds its supported size"))
    (if context
        (list :hookSpecificOutput (list :hookEventName "PostToolBatch" :additionalContext context))
      '(:continue t))))

(defun mevedel-claude-code--control (request event)
  "Return the native hook decision for admitted REQUEST and Claude EVENT."
  (let* ((name (mevedel-claude-code--hook-event event))
         (decision
          (progn
            (when (equal "PostToolBatch" name)
              (let* ((info (mevedel-engine-info request))
                     (buffer (plist-get info :buffer))
                     (warnings
                      (if (mevedel-request-p request)
                          (mevedel-goal-tool-result-budget-warnings (mevedel-request-session request) request)
                        (when-let* ((notice (mevedel-goal-agent-budget-notice request)))
                          (list (plist-put notice :key 'goal-budget))))))
                (dolist (warning warnings)
                  (mevedel-reminders-queue-turn-event
                   buffer (plist-get warning :key) (plist-get warning :body) (plist-get warning :commit)))))
            (when (and (mevedel-agent-invocation-p request)
                       (equal "PostToolBatch" name)
                       (not (plist-get (mevedel-engine-info request) :mevedel-end-turn))
                       (not (plist-get (mevedel-engine-info request) :mevedel-cancelled)))
              (mevedel-claude-code-agent-next-sample request t))
            (let ((reason (plist-get (mevedel-engine-info request) :mevedel-end-turn)))
              (if (or reason (if (mevedel-request-p request)
                                 (mevedel-request-cancelled-p request)
                               (plist-get (mevedel-engine-info request) :mevedel-cancelled)))
                  (list :continue :json-false
                        :stopReason (format "Mevedel ended this turn: %s" (or reason 'interrupted)))
                (if-let* ((context
                           (pcase name
                             ("SessionStart" (mevedel-claude-code-context-restore request))
                             ("PostToolBatch" (mevedel-claude-code-context-prepare request))
                             ("PreToolUse"
                              ;; SessionStart(compact) does not reliably honor a stop.
                              ;; Stop again before tool dispatch until the full context
                              ;; can be delivered through the next native prompt.
                              (cl-loop for key in '(:mevedel-claude-context-pending
                                                    :mevedel-claude-restoration-pending)
                                       for pending = (plist-get (mevedel-engine-info request) key)
                                       when (eq 'continuation (plist-get pending :route))
                                       return (plist-get pending :body))))))
                    (if (not (mevedel-claude-code-context-hook-fits-p context))
                        (progn
                          (when (and (mevedel-agent-invocation-p request)
                                     (member name
                                             '("SessionStart" "PreToolUse")))
                            ;; SessionStart cannot stop the next sample; a denied
                            ;; tool also spent its sample.  Reserve another only
                            ;; after successful settlement, even for text alone.
                            (setf (mevedel-engine-info request)
                                  (plist-put (mevedel-engine-info request)
                                             :mevedel-claude-continuation-sample-spent t)))
                          (list :continue :json-false :stopReason "Mevedel will deliver the full context in a continuation prompt before further work."))
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

(defun mevedel-claude-code--complete-prompt (owner outcome)
  "Account for OWNER's native OUTCOME and prepare required context continuation."
  (setq outcome (mevedel-claude-code-usage-complete owner outcome))
  (when (mevedel-agent-invocation-p owner)
    (mevedel-goal-charge-agent-progress owner))
  (when (eq 'success (plist-get outcome :status))
    (when (and (mevedel-agent-invocation-p owner)
               (plist-get (mevedel-engine-info owner) :mevedel-claude-continuation-sample-spent))
      (setf (mevedel-engine-info owner)
            (plist-put (mevedel-engine-info owner) :mevedel-claude-continuation-sample-spent nil))
      (unless (or (plist-get (mevedel-engine-info owner) :mevedel-end-turn)
                  (plist-get (mevedel-engine-info owner) :mevedel-cancelled))
        (mevedel-claude-code-agent-next-sample owner t)))
    (when-let* ((content (mevedel-claude-code-context-next-prompt owner)))
      (let ((info (mevedel-engine-info owner)))
        (setq info (plist-put info :mevedel-claude-usage-base (copy-sequence (plist-get info :tokens-full))))
        (setq info (plist-put info :mevedel-claude-finished-samples
                              (append (mapcar #'car (plist-get info :mevedel-claude-sample-usage))
                                      (plist-get info :mevedel-claude-finished-samples))))
        (setq info (plist-put info :mevedel-claude-sample-id nil))
        (setq info (plist-put info :mevedel-claude-prompt-usage nil))
        (setq info (plist-put info :mevedel-usage-pending t))
        (setf (mevedel-engine-info owner) (plist-put info :mevedel-claude-sample-usage nil)))
      (setq outcome (plist-put outcome :next-prompt content))))
  outcome)

(defun mevedel-claude-code--outcome (outcome)
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

(defun mevedel-claude-code-launch (system mcp model effort &optional session-id hook-command)
  "Prepare an authenticated Claude ACP launch for SYSTEM and MCP servers.
MODEL is the explicit Claude model name.  EFFORT is a supported effort symbol
or nil.  SESSION-ID resumes the installed CLI's history on this machine.
HOOK-COMMAND is the generated private bridge for request boundary decisions.
Return the generic ACP launch plist; no model request is made here."
  (unless (and (stringp system) (stringp model))
    (error "Claude launch requires a system prompt and model"))
  (unless (symbolp effort)
    (user-error "Claude Code does not support reasoning effort %S" effort))
  (when (file-remote-p mevedel-claude-code-directory)
    (user-error "Claude Code state must be on the local machine"))
  (mevedel-claude-code-maintenance-check)
  (let* ((runtime (mevedel-claude-code-maintenance-state))
         (directory (expand-file-name mevedel-claude-code-directory))
         (cli (or (and (plist-get runtime :cli) (file-executable-p (plist-get runtime :cli))
                       (plist-get runtime :cli))
                  (executable-find mevedel-claude-code-executable)
                  (user-error "Install Claude Code and run `claude auth login'")))
         (adapter
          (or (and mevedel-claude-code-adapter-executable
                   (executable-find mevedel-claude-code-adapter-executable))
              (and (not mevedel-claude-code-adapter-executable)
                   (or (and (plist-get runtime :adapter) (file-executable-p (plist-get runtime :adapter))
                            (plist-get runtime :adapter))
                       (executable-find "claude-agent-acp")))
              (user-error "Run M-x mevedel-claude-code-install-adapter to install the Claude connection adapter")))
         (process-environment (copy-sequence process-environment))
         (default-directory temporary-file-directory))
    (dolist (name mevedel-claude-code--api-environment) (setenv name nil))
    (setenv "CLAUDE_CODE_EXECUTABLE" cli)
    ;; The adapter also resolves a model from settings before session/new
    ;; completes.  Its environment pin must agree with the SDK option.
    (setenv "ANTHROPIC_MODEL" model)
    ;; Configure effort only after discovery, including an explicit default.
    (setenv "CLAUDE_CODE_EFFORT_LEVEL" nil)
    (unless (version<= "0.15.2" acp-package-version)
      (user-error "Upgrade the Emacs acp package to 0.15.2 or newer"))
    (let* ((checks (list
                    (list (or (executable-find "node") (user-error "Install Node.js 22 or newer on the Emacs host"))
                          "22.0.0" "Node.js")
                    (list (or (executable-find "python3") (user-error "Install Python 3.8 or newer on the Emacs host"))
                          "3.8.0" "Python")
                    (list cli mevedel-claude-code--cli-version "Claude Code")
                    (list adapter mevedel-claude-code--adapter-version "Claude ACP adapter")))
           (environment process-environment)
           (backend (mevedel-claude-code-register))
           (buffer (current-buffer))
           (cwd (file-name-concat directory "conversations"))
           (settings '((autoMemoryEnabled . :false)))
           (options `((tools . []) (model . ,model)
                      (systemPrompt . ((type . "custom") (prompt . ,system) (snapshot . :false)))
                      (settingSources . []) (strictMcpConfig . t)
                      (includeHookEvents . t)
                      (allowedTools . ["mcp__mevedel__*"])
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
            :prepare-launch
            (lambda (ready failure)
              (mevedel-claude-code--prepare-launch checks cli environment ready failure))
            :prepare-session
            (lambda (connection session ready)
              (mevedel-claude-code--prepare-session backend model effort buffer connection session ready))
            :tool-id-field :claudecode/toolUseId
            :normalize-outcome #'mevedel-claude-code--outcome
            :complete-prompt #'mevedel-claude-code--complete-prompt
            :control #'mevedel-claude-code--control
            :observe #'mevedel-claude-code--observe
            :check-context #'mevedel-claude-code-context-check
            :meta
            `((claudeCode . ((emitRawSDKMessages . t)
                             (options . ,(append options
                                                 `((settings . ,settings)))))))))))

(defun mevedel-claude-code-usage-launch (model)
  "Prepare an isolated native subscription usage inspection for MODEL.
Retain ordinary launch isolation, enabling only the native-command exception.
Model configuration is unnecessary for this local command and must not update
its caller's selected model or backend catalog."
  (let* ((launch (mevedel-claude-code-launch "Subscription usage inspection" [] model nil))
         (meta (copy-tree (plist-get launch :meta)))
         (options (alist-get 'options (alist-get 'claudeCode meta))))
    (setf (plist-get launch :meta) meta
          (alist-get 'extraArgs options)
          (assq-delete-all 'disable-slash-commands (alist-get 'extraArgs options))
          (plist-get launch :prepare-session) nil
          (plist-get launch :required-command) "usage"
          (plist-get launch :response-timeout) 30)
    launch))

(provide 'mevedel-claude-code)
;;; mevedel-claude-code.el ends here
