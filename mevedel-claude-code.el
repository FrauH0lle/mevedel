;;; mevedel-claude-code.el --- Claude subscription adapter setup -*- lexical-binding: t -*-

;;; Commentary:
;; Owns Claude-specific login checks, local launch isolation, and ACP metadata.
;; Subscription credentials remain entirely inside the installed Claude CLI.
;; This module never reads credential files or chooses an API fallback.

;;; Code:

(require 'acp)
(require 'cl-lib)
(require 'button)
(require 'json)
(require 'subr-x)
(require 'mevedel-claude-code-usage)
(require 'mevedel-engine)
(require 'mevedel-mcp)
(require 'mevedel-models)

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

;; `mevedel-transport'
(declare-function mevedel-transport-run-at-time "mevedel-transport" (seconds function &rest args))
(autoload 'mevedel-transport-run-at-time "mevedel-transport")

(defconst mevedel-claude-code--aliases
  '(("sonnet" low medium high xhigh max)
    ("opus" low medium high xhigh max)
    ("fable" low medium high xhigh max)
    ("haiku"))
  "Documented alias effort choices before ACP reports live capabilities.
See https://code.claude.com/docs/en/model-config.")

(cl-defstruct (mevedel-claude-code-backend
               (:include gptel-backend)
               (:constructor mevedel-claude-code--make-backend)))

(cl-defmethod gptel--request-data ((_backend mevedel-claude-code-backend) _prompts)
  "Refuse HTTP dispatch for a subscription-backed conversation."
  (user-error "Claude Code requires mevedel's external conversation engine"))

(cl-defmethod mevedel-engine-external-p ((_backend mevedel-claude-code-backend)) t)

(cl-defmethod mevedel-engine-request-text
  ((_backend mevedel-claude-code-backend) prompt system callback &optional stream _context)
  (when gptel-tools (error "Isolated text requests cannot use tools"))
  (mevedel-acp-text-request
   (mevedel-claude-code-launch system [] (gptel--model-name gptel-model)
                               gptel-reasoning-effort)
   prompt callback stream))

(cl-defmethod mevedel-engine-request-workload
  ((_backend mevedel-claude-code-backend) prompt system tools callback before-tool boundary)
  (let ((model (gptel--model-name gptel-model)) (effort gptel-reasoning-effort))
    (mevedel-acp-workload-request
     (lambda (mcp hook)
       (plist-put (mevedel-claude-code-launch system mcp model effort nil hook)
                  :control #'mevedel-claude-code--workload-control))
     prompt tools callback before-tool boundary)))

(defun mevedel-claude-code--model (name &optional existing)
  "Return a backend-owned model named NAME, reusing EXISTING when possible.
Uninterned symbols keep native capabilities separate from API model metadata.
Known aliases start with documented effort choices, refreshed on connection.
Other model IDs accept configured symbols pending capability discovery."
  (or (cl-find name existing :key #'gptel--model-name :test #'equal)
      (car (gptel--process-models
            (list (list (make-symbol name) :description "Claude Code subscription"
                        :capabilities '(tool-use media)
                        :mime-types '("image/jpeg" "image/png" "image/gif" "image/webp")
                        :reasoning-effort
                        (if-let* ((alias (assoc name mevedel-claude-code--aliases)))
                            (and (cdr alias) (cons 'member (cdr alias)))
                          'symbol)))))))

(cl-defmethod mevedel-model--find-model ((_backend mevedel-claude-code-backend) model-name)
  ;; Accept persisted or configured IDs without growing
  ;; the catalog.  Each session validates against its discovered catalog
  ;; before dispatch and never chooses a fallback.
  (or (cl-call-next-method) (mevedel-claude-code--model model-name)))

(defun mevedel-claude-code--check-model (backend model session)
  "Cache BACKEND's SESSION catalog, validate MODEL and return its effort option."
  (let* ((config (alist-get 'configOptions session))
         (models (cl-find "model" config :key (lambda (row) (alist-get 'category row)) :test #'equal))
         (efforts (cl-find "thought_level" config :key (lambda (row) (alist-get 'category row)) :test #'equal))
         (rows (alist-get 'options models))
         (current (alist-get 'currentValue models))
         (existing (gptel-backend-models backend))
         (catalog (mapcar (lambda (name) (mevedel-claude-code--model name existing))
                          (mapcar #'car mevedel-claude-code--aliases))))
    (unless (and (sequencep rows) (> (length rows) 0) (stringp current))
      (user-error "Claude did not report model capabilities; update the connection adapter and retry"))
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
    (unless (cl-find model rows :key (lambda (row) (alist-get 'value row)) :test #'equal)
      (user-error "Claude model %s is unavailable; select a listed model and retry" model))
    (unless (equal model current)
      (user-error "Claude selected %s instead of %s; select an available model and retry" current model))
    efforts))

(defun mevedel-claude-code--prepare-session (backend model effort buffer connection session ready)
  "Discover MODEL on BACKEND and apply EFFORT before CONNECTION is ready.
SESSION contains the initial capabilities.  READY receives the acknowledged
metadata.  An unsupported selection uses Claude's default and resets BUFFER's
selection if it still matches this launch.  A permission mode inherited from
the user's Claude settings is reset to `default': mevedel owns permissions."
  (let* ((option (mevedel-claude-code--check-model backend model session))
         (mode (cl-find "mode" (alist-get 'configOptions session)
                        :key (lambda (row) (alist-get 'category row)) :test #'equal))
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
             (configure option value "Claude did not acknowledge the selected effort" #'finish))))
      (if (and mode (not (equal "default" (alist-get 'currentValue mode))))
          (configure mode "default" "Claude did not acknowledge mevedel's permission mode"
                     #'apply-effort)
        (apply-effort)))))

(defun mevedel-claude-code-register ()
  "Register Claude Code in the ordinary provider and workload selection."
  (if-let* ((backend (alist-get "Claude Code" gptel--known-backends nil nil #'equal))
            ((mevedel-claude-code-backend-p backend)))
      backend
    (setf (alist-get "Claude Code" gptel--known-backends nil nil #'equal)
          (mevedel-claude-code--make-backend
           :name "Claude Code" :stream t
           :models (mapcar #'mevedel-claude-code--model (mapcar #'car mevedel-claude-code--aliases))))))

(autoload 'mevedel-claude-code-send "mevedel-claude-code-session")
(autoload 'mevedel-claude-code-release-history "mevedel-claude-code-session")
(autoload 'mevedel-claude-code-recover-history "mevedel-claude-code-session" nil t)

(defcustom mevedel-claude-code-executable "claude"
  "Installed, unmodified Claude Code executable."
  :type 'string :group 'mevedel)

(defcustom mevedel-claude-code-adapter-executable nil
  "ACP adapter executable, or nil for the managed installation and PATH."
  :type '(choice (const :tag "Find installed adapter" nil) file)
  :group 'mevedel)

(defcustom mevedel-claude-code-directory
  (file-name-concat user-emacs-directory "mevedel" "claude-code")
  "Local adapter installation and neutral conversation working directory.
Keep this location stable to resume the installed CLI's retained histories."
  :type 'directory :group 'mevedel)

(defconst mevedel-claude-code--adapter-version "0.86.0"
  "Adapter release installed by the guided setup.")

(defconst mevedel-claude-code--api-environment
  '("ANTHROPIC_API_KEY" "ANTHROPIC_AUTH_TOKEN" "ANTHROPIC_BASE_URL"
    "CLAUDE_CODE_OAUTH_TOKEN" "CLAUDE_CODE_USE_BEDROCK"
    "CLAUDE_CODE_USE_VERTEX" "CLAUDE_CODE_USE_FOUNDRY")
  "Inherited authentication routes excluded by explicit subscription selection.")

(defun mevedel-claude-code--command-output-async (command args ready failure &optional any-exit)
  "Run status COMMAND with ARGS asynchronously; return its canceller.
READY receives stdout on success, or on every exit with ANY-EXIT.  FAILURE
receives a safe diagnostic.  Cancel and timeout release the child and both
private output buffers."
  (let ((check (mapconcat #'shell-quote-argument (cons (file-name-nondirectory command) args) " "))
        (stdout (generate-new-buffer " *claude-status*"))
        (stderr (generate-new-buffer " *claude-status-errors*"))
        process timer finished)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (when process
             (set-process-sentinel process #'ignore)
             (when (process-live-p process) (delete-process process)))
           (when (buffer-live-p stdout) (kill-buffer stdout))
           (when (buffer-live-p stderr) (kill-buffer stderr)))
         (cancel () (setq finished t) (cleanup))
         (fail (message)
           (unless finished
             (setq finished t)
             (cleanup)
             (funcall failure message)))
         (exited (child _event)
           (when (and (not finished) (memq (process-status child) '(exit signal)))
             (if (not (or any-exit (zerop (process-exit-status child))))
                 (fail (format "Setup check failed: %s; run it in a terminal" check))
               (let ((output (with-current-buffer stdout (string-trim (buffer-string)))))
                 (setq finished t)
                 (cleanup)
                 (funcall ready output))))))
      (condition-case nil
          (progn
            (setq timer (mevedel-transport-run-at-time
                         10 (lambda () (fail (format "Setup check timed out: %s; run it in a terminal" check)))))
            (setq process (make-process :name "mevedel-claude-status"
                                        :command (cons command args)
                                        :buffer stdout :stderr stderr
                                        :connection-type 'pipe :noquery t
                                        :coding 'utf-8-unix :sentinel #'exited))
            (when finished (cleanup)))
        (error (fail (format "Setup check failed: %s; run it in a terminal" check))))
      #'cancel)))

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

(defun mevedel-claude-code--version (output minimum)
  "Return the version reported in OUTPUT when it is at least MINIMUM."
  (and (string-match "[0-9]+\\.[0-9]+\\.[0-9]+" output)
       (version<= minimum (match-string 0 output))
       (match-string 0 output)))

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
                "1. Install Claude Code 2.1.290 or newer, Node.js 22 or newer,\n"
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
  "Offer to install the pinned ACP adapter in the configured local directory.
Run npm asynchronously with output in a compilation buffer.  Return the
installation process, or nil when the user declines.  Never install on send."
  (interactive)
  (when-let* ((process (get-process "mevedel-claude-install"))
              ((process-live-p process)))
    (user-error "An adapter installation is already running"))
  (when mevedel-claude-code-adapter-executable
    (user-error "Clear mevedel-claude-code-adapter-executable to use the managed installation"))
  (when (file-remote-p mevedel-claude-code-directory)
    (user-error "Claude Code state must be on the local machine"))
  (let* ((default-directory temporary-file-directory)
         (directory (expand-file-name mevedel-claude-code-directory))
         (package (concat "@agentclientprotocol/claude-agent-acp@" mevedel-claude-code--adapter-version))
         (npm (or (executable-find "npm") (user-error "Install Node.js 22 or newer with npm first"))))
    (let ((node (or (executable-find "node") (user-error "Install Node.js 22 or newer first"))))
      (unless (mevedel-claude-code--version
               (mevedel-claude-code--wait
                (lambda (ready failure)
                  (mevedel-claude-code--command-output-async node '("--version") ready failure)))
               "22.0.0")
        (user-error "Node.js 22.0.0 or newer is required")))
    (when (yes-or-no-p (format "Install %s and its dependencies with npm in %s? " package directory))
      (let ((buffer (get-buffer-create "*mevedel Claude adapter installation*"))
            (process-environment (copy-sequence process-environment)))
        (dolist (name mevedel-claude-code--api-environment) (setenv name nil))
        (make-directory directory t)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Installing %s in %s\n\n" package directory))
            (compilation-mode)))
        (display-buffer buffer)
        (make-process
         :name "mevedel-claude-install" :buffer buffer :connection-type 'pipe
         :command (list npm "install" "--prefix" directory "--save-exact" "--no-audit" "--no-fund" package)
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (when (buffer-live-p (process-buffer process))
               (with-current-buffer (process-buffer process)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert (if (and (eq 'exit (process-status process)) (zerop (process-exit-status process)))
                               "\nInstallation complete. Run M-x mevedel-claude-code-setup to refresh readiness.\n"
                             "\nInstallation failed. Review this output, fix the reported problem, and retry setup.\n"))))))))))))

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
  (let* ((directory (expand-file-name mevedel-claude-code-directory))
         (managed (file-name-concat directory "node_modules" ".bin" "claude-agent-acp"))
         (cli (or (executable-find mevedel-claude-code-executable)
                  (user-error "Install Claude Code and run `claude auth login'")))
         (adapter
          (or (and mevedel-claude-code-adapter-executable
                   (executable-find mevedel-claude-code-adapter-executable))
              (and (not mevedel-claude-code-adapter-executable)
                   (or (and (file-executable-p managed) managed)
                       (executable-find "claude-agent-acp")))
              (user-error "Run M-x mevedel-claude-code-setup to install the Claude connection adapter")))
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
                    (list cli "2.1.290" "Claude Code")
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

(provide 'mevedel-claude-code)
;;; mevedel-claude-code.el ends here
