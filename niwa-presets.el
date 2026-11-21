;;; niwa-presets.el -- Presets used -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the implementation gptel preset and composable preset setup.
;;
;; Each preset assembles tool lists and registers sub-agents buffer-locally at
;; request time.  This module builds the request FSM handler chain around the
;; neutral terminal transaction in `niwa-turn'.  Deferred-tool handler
;; wiring also lives here because it piggy-backs on preset setup.

;;; Code:

(require 'niwa-utilities)

(require 'niwa-recovery)

(require 'cl-lib)
(require 'niwa-structs)
(require 'niwa-turn)

(eval-when-compile
  ;; Needed for `setf' on `gptel-fsm' struct slots (native comp)
  (require 'gptel-request nil t))

;; `gptel'
(declare-function gptel--apply-preset "ext:gptel" (preset &optional setter))
(declare-function gptel--modify-value "ext:gptel" (original new-spec))
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel-make-preset "ext:gptel" (name &rest keys))
(autoload 'gptel--apply-preset "gptel")
(autoload 'gptel--modify-value "gptel")
(autoload 'gptel-get-preset "gptel")
(autoload 'gptel-make-preset "gptel")

;; `gptel-request'
(declare-function gptel--fsm-transition "ext:gptel-request" (machine &optional new-state))
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--tool-result-p "ext:gptel-request" (info))
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(defvar gptel-request--transitions)
(defvar gptel-tools)

;; `niwa-agents'
(declare-function niwa-agents--setup-for-request
                  "niwa-agents" (&optional preset-name))

;; `niwa-chat'
(declare-function niwa--generate-final-patch
                  "niwa-chat" (workspace request callback))
(declare-function niwa--replace-patch-buffer
                  "niwa-chat" (patch-content))
(autoload 'niwa--generate-final-patch "niwa-chat")

;; `niwa-compact'
(declare-function niwa--compact-handle-wait "niwa-compact" (fsm))
(autoload 'niwa--compact-handle-wait "niwa-compact")

;; `niwa-compact-estimation'
(declare-function niwa-compact-estimation-record-token-baseline
                  "niwa-compact-estimation" (fsm))
(autoload 'niwa-compact-estimation-record-token-baseline
  "niwa-compact-estimation")

;; `niwa-goal'
(declare-function niwa-goal-capture-request "niwa-goal" (fsm))

;; `niwa-implementation-policy'
(declare-function niwa-implementation-policy-assert-preparation
                  "niwa-implementation-policy" (preparation))
(autoload 'niwa-implementation-policy-assert-preparation
  "niwa-implementation-policy")
(defvar niwa-implementation-policy--admission-preparation)

;; `niwa-models'
(declare-function niwa-model-merge-tiers
                  "niwa-models" (additions current))
(declare-function niwa-model-merge-workloads
                  "niwa-models" (additions current))

;; `niwa-note-conversation'
(declare-function niwa-note-conversation-setup "niwa-note-conversation" ())
(autoload 'niwa-note-conversation-setup "niwa-note-conversation")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-assert-new-mutation-authority
                  "niwa-session-artifacts" (session))
(autoload 'niwa-session-artifacts-assert-new-mutation-authority
  "niwa-session-artifacts")

;; `niwa-skills-invoke'
(declare-function niwa-skills--drain-pending-context
                  "niwa-skills-invoke" (request))
(autoload 'niwa-skills--drain-pending-context "niwa-skills-invoke")

;; `niwa-structs'
(declare-function niwa-session-preset-name "niwa-structs" (cl-x) t)
(declare-function niwa-session-forked-from-session-id "niwa-structs" (cl-x) t)
(defvar niwa--current-request)
(defvar niwa--session)

;; `niwa-system'
(declare-function niwa-system-build-prompt
                  "niwa-system"
                  (profile &rest keys))

;; `niwa-tool-registry'
(declare-function niwa-tool-all "niwa-tool-registry" ())
(declare-function niwa-tool-category "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-read-only-p
                  "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-resolve "niwa-tool-registry" (specs))
(declare-function niwa-tool-resolve-gptel
                  "niwa-tool-registry" (specs))
(declare-function niwa-tool-summary "niwa-tool-registry" (cl-x) t)
(autoload 'niwa-tool-all "niwa-tool-registry")
(autoload 'niwa-tool-category "niwa-tool-registry")
(autoload 'niwa-tool-name "niwa-tool-registry")
(autoload 'niwa-tool-read-only-p "niwa-tool-registry")
(autoload 'niwa-tool-resolve "niwa-tool-registry")
(autoload 'niwa-tool-resolve-gptel "niwa-tool-registry")
(autoload 'niwa-tool-summary "niwa-tool-registry")

;; `niwa-tool-ptc'

;; `niwa-tools'
(declare-function niwa-tools--handle-agent-roster-inject
                  "niwa-tools" (fsm))
(declare-function niwa-tools--handle-message-inject
                  "niwa-tools" (fsm))
(declare-function niwa-tools--handle-plan-tool-filter
                  "niwa-tools" (fsm))
(declare-function niwa-tools--handle-steering-inject
                  "niwa-tools" (fsm))
(declare-function niwa-tools--pending-steering-p
                  "niwa-tools" (info))

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (&optional path))
(declare-function niwa-transport-run-when-idle
                  "niwa-transport" (key path thunk &optional on-cancel))
(autoload 'niwa-transport-busy-p "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")

;; `niwa-turn'
(declare-function niwa--complete-turn "niwa-turn" (fsm))
(declare-function niwa--fail-turn "niwa-turn" (fsm status))
(declare-function niwa--safe-fsm-handler "niwa-turn" (handler))
(declare-function niwa--turn-current-p "niwa-turn" (fsm))
(declare-function niwa--turn-hold "niwa-turn" (fsm))
(declare-function niwa--turn-release "niwa-turn" (fsm))
(declare-function niwa-request-begin
                  "niwa-turn" (session &optional options))

;; `niwa-view-stream'
(declare-function niwa-view-stream-ensure-progress-for-fsm
                  "niwa-view-stream" (fsm))

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))

;;
;;; Presets

(defcustom niwa-action-preset-alist
  '((implement . niwa-implement))
  "Alist mapping actions to presets."
  :group 'niwa
  :type '(alist :key-type symbol))

(defcustom niwa-preset-extra-tool-specs nil
  "Alist mapping preset names to extra tool specs.

Each entry is (PRESET-SYMBOL . SPEC-LIST).  SPEC-LIST uses the same
forms accepted by `niwa-define-preset''s :tools keyword (bare
symbols, (:group X), (:tool X), (:discoverable X)).  Extras are merged
into the preset's tool set every time the preset is applied:
deferred entries are appended to the session's tool-catalog and
active entries are appended to the buffer-local `gptel-tools'.

Useful for giving user-wrapped tools a home in an existing preset
without redefining it."
  :group 'niwa
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defvar niwa-preset--registry nil
  "Alist mapping preset names to niwa-specific metadata.
Each value contains the raw parent, agent, tool, and variable settings.")

(defvar niwa-preset--temporary-p nil
  "Non-nil while a preset is applied for one dynamic request only.")

(defvar niwa-preset--applying-p nil
  "Non-nil inside a preset application, including its parent applications.")

(defun niwa-preset--apply-note-layer (apply-preset preset &rest args)
  "Call APPLY-PRESET with PRESET and ARGS, keeping note isolation on top."
  (let ((outer (not niwa-preset--applying-p))
        (niwa-preset--applying-p t))
    (prog1 (apply apply-preset preset args)
      (when (and outer niwa--session
                 (niwa-session-note-conversation niwa--session))
        (niwa-note-conversation-setup)))))

(advice-add 'gptel--apply-preset :around #'niwa-preset--apply-note-layer)

(defconst niwa-preset--structural-keys
  '(:description :parents :pre :post :backend :model :system :tools :agents
    :model-tiers :model-workloads)
  "Preset keys with dedicated niwa or gptel behavior.")

(defun niwa-preset--variable-for-key (key)
  "Return the variable selected by ordinary preset KEY, or nil.
Niwa public and private variables take precedence over gptel variables."
  (let ((name (substring (symbol-name key) 1)))
    (cl-find-if
     #'boundp
     (delq nil
           (mapcar #'intern-soft
                   (list (concat "niwa-" name)
                         (concat "niwa--" name)
                         (concat "gptel-" name)
                         (concat "gptel--" name)))))))

(defun niwa-preset--parent-list (metadata)
  "Return the parents declared by preset METADATA."
  (ensure-list (plist-get metadata :parents)))

(defun niwa-preset--parents (name)
  "Return the presets NAME declares as parents.

A preset may be defined through niwa or directly through gptel, and
both declare parents the same way, so a cycle can run through either
registry."
  (niwa-preset--parent-list
   (or (alist-get name niwa-preset--registry)
       (gptel-get-preset name))))

(defun niwa-preset--parent-chain-to (name start &optional visited)
  "Return the parent chain from START up to NAME, or nil.

The chain is a list of preset names beginning at START and ending at
NAME.  VISITED is the set of presets the walk has already entered: a
preset that does not reach NAME once never does, and pruning it keeps
the walk finite over presets gptel defines directly, which it registers
without a cycle check of its own."
  (let ((visited (or visited (make-hash-table :test 'eq))))
    (cond
     ((eq start name) (list name))
     ((gethash start visited) nil)
     (t
      (puthash start t visited)
      (let ((parents (niwa-preset--parents start))
            chain)
        (while (and parents (not chain))
          (setq chain (niwa-preset--parent-chain-to
                       name (pop parents) visited)))
        (and chain (cons start chain)))))))

(defun niwa-preset--resolved-metadata (name)
  "Return inherited niwa metadata for preset NAME."
  (let* ((metadata (or (alist-get name niwa-preset--registry)
                       (user-error "Unknown niwa preset: %s" name)))
         (resolved nil))
    (dolist (parent (niwa-preset--parent-list metadata))
      (let ((parent-meta (niwa-preset--resolved-metadata parent)))
        (when (plist-member parent-meta :agents)
          (setq resolved (plist-put resolved :agents
                                    (plist-get parent-meta :agents))))
        (when (plist-member parent-meta :tool-specs)
          (setq resolved (plist-put resolved :tool-specs
                                    (plist-get parent-meta :tool-specs))))))
    (dolist (key '(:agents :tool-specs))
      (when (plist-member metadata key)
        (setq resolved (plist-put resolved key (plist-get metadata key)))))
    resolved))

(defun niwa-preset--setting-specs (name)
  "Return inherited ordinary niwa setting specs for preset NAME."
  (let* ((metadata (or (alist-get name niwa-preset--registry)
                       (user-error "Unknown niwa preset: %s" name)))
         specs)
    (dolist (parent (niwa-preset--parent-list metadata))
      (setq specs (append specs (niwa-preset--setting-specs parent))))
    (append specs (plist-get metadata :settings))))

(defun niwa-preset-resolve-settings (name)
  "Return preset NAME's resolved niwa variable alist."
  (let (resolved)
    (dolist (setting (niwa-preset--setting-specs name))
      (let* ((symbol (car setting))
             (current (if-let* ((entry (assq symbol resolved)))
                          (cdr entry)
                        (symbol-value symbol)))
             (value (gptel--modify-value current (cdr setting))))
        (setf (alist-get symbol resolved) value)))
    resolved))

(defun niwa-preset--apply-settings (name)
  "Apply preset NAME's niwa settings to the current request or session."
  (let ((settings
         (plist-get (or (alist-get name niwa-preset--registry)
                        (user-error "Unknown niwa preset: %s" name))
                    :settings)))
    (dolist (setting settings)
      (let* ((symbol (car setting))
             (value (gptel--modify-value
                     (symbol-value symbol) (cdr setting))))
        (if niwa-preset--temporary-p
            (set symbol value)
          (set (make-local-variable symbol) value))))
    (when (and (not niwa-preset--temporary-p) niwa--session)
      (setf (niwa-session-preset-name niwa--session) name)
      ;; Any applied session preset replaces a missing one.
      (niwa-recovery-clear niwa--session "preset"))))

(defun niwa-preset--post (name user-post)
  "Run USER-POST and required niwa setup for preset NAME."
  (when user-post (funcall user-post))
  (niwa-preset--apply-settings name)
  (niwa-preset--refresh-tools name)
  (niwa-preset--setup-scope name))

(defun niwa-preset--setup-scope (name)
  "Install preset NAME's agent roster and tool catalog in the current buffer.
They live in buffer, session, and tool state that no preset application
can let-bind.  The roster comes last: its Agent schema replaces the Agent
in the final `gptel-tools', extras included."
  (niwa-preset--setup-catalog name)
  (niwa-preset--setup-extras name)
  (niwa-agents--setup-for-request name))

(defun niwa-preset--restore-scope-at-request-end ()
  "Reinstall the session preset's scope when the current request ends.
A request-only preset replaces the agent roster and tool catalog, which
its tool calls read long after dispatch returns, so they cannot be
restored when the preset's dynamic extent closes.  The session preset is
read at teardown, so one selected during the request wins."
  (let ((buffer (current-buffer)))
    (niwa-request-push-canceller
     niwa--current-request
     (lambda ()
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when-let* ((session niwa--session)
                       (name (niwa-session-preset-name session))
                       ((assq name niwa-preset--registry)))
             (niwa-preset--setup-scope name))))))))

(defun niwa-preset--define (name keys)
  "Register exact preset NAME from evaluated KEYS."
  ;; Reject a cycle before anything is registered: gptel walks parents
  ;; unguarded too, and both walks recurse until the Lisp stack gives out.
  (dolist (parent (ensure-list (plist-get keys :parents)))
    (when-let* ((chain (niwa-preset--parent-chain-to name parent)))
      (user-error "Preset %s cannot inherit from %s: parent cycle %s"
                  name parent
                  (mapconcat #'symbol-name (cons name chain) " -> "))))
  (let ((raw-keys keys)
        (gptel-keys nil)
        (settings nil)
        (user-post (plist-get keys :post)))
    (while keys
      (let ((key (pop keys))
            (value (pop keys)))
        (cond
         ((eq key :agents) nil)
         ((memq key '(:model-tiers :model-workloads))
          (require 'niwa-models)
          (push (cons (if (eq key :model-tiers)
                          'niwa-model-tiers
                        'niwa-model-workloads)
                      (list :function
                            (apply-partially
                             (if (eq key :model-tiers)
                                 #'niwa-model-merge-tiers
                               #'niwa-model-merge-workloads)
                             value)))
                settings))
         ((eq key :tools)
          (setq gptel-keys
                (append gptel-keys
                        (list key
                              (plist-get
                               (niwa-tool-resolve-gptel value)
                               :active)))))
         ((memq key niwa-preset--structural-keys)
          (unless (eq key :post)
            (setq gptel-keys (append gptel-keys (list key value)))))
         (t
          (let ((symbol (niwa-preset--variable-for-key key)))
            (cond
             ((and symbol
                   (string-prefix-p "niwa-" (symbol-name symbol)))
              (push (cons symbol value) settings))
             (symbol
              (setq gptel-keys (append gptel-keys (list key value))))
             (t
              (niwa-diagnostic-warning
               'global '(niwa presets)
               (format "niwa preset %s: setting for %s not found, ignoring"
                       name key)))))))))
    (let ((metadata (list :parents (plist-get raw-keys :parents)
                          :settings (nreverse settings))))
      (when (plist-member raw-keys :agents)
        (setq metadata
              (plist-put metadata :agents (plist-get raw-keys :agents))))
      (when (plist-member raw-keys :tools)
        (setq metadata
              (plist-put metadata :tool-specs (plist-get raw-keys :tools))))
      (setf (alist-get name niwa-preset--registry) metadata))
    (apply #'gptel-make-preset name
           (append gptel-keys
                   (list :post (apply-partially
                                #'niwa-preset--post name user-post))))))

(defun niwa-preset-apply (name &optional buffer)
  "Apply niwa preset NAME buffer-locally to BUFFER and its session."
  (with-current-buffer (or buffer (current-buffer))
    (gptel--apply-preset
     name (lambda (symbol value)
            (set (make-local-variable symbol) value)))))

(defun niwa-preset-restore-session (session &optional buffer)
  "Reapply SESSION's selected preset in BUFFER.
A preset that is no longer registered blocks requests until a replacement
preset is applied."
  (when-let* ((name (niwa-session-preset-name session)))
    (if (assq name niwa-preset--registry)
        (niwa-preset-apply name buffer)
      (niwa-recovery-report session "preset" 'configuration
                               (format "Preset %s is unavailable; select a replacement" name) t))))

(defmacro niwa-with-preset (name &rest body)
  "Run BODY with niwa preset NAME applied for the current request only.
Variables are scoped to BODY.  NAME's agent roster and tool catalog stay
until the current request ends, when its teardown reinstalls the session
preset's."
  (declare (indent 1) (debug t))
  `(let ((preset ,name)
         (niwa-preset--temporary-p t))
     (niwa-preset--restore-scope-at-request-end)
     (if (and (symbolp preset)
              (assq preset niwa-preset--registry))
         (let* ((symbols (mapcar #'car
                                 (niwa-preset-resolve-settings preset)))
                (values (mapcar #'symbol-value symbols)))
           (cl-progv symbols values
             (gptel-with-preset preset ,@body)))
       (gptel-with-preset preset ,@body))))

(defmacro niwa-define-preset (name &rest keys)
  "Define a niwa preset NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description  STRING  -- preset description for selection UI
  :tools        LIST    -- tool specs for `niwa-tool-resolve-gptel'
                           (groups, (:tool X), (:discoverable X))
  :agents       LIST    -- list of agent name symbols for request-time setup
  :system       VALUE   -- system prompt (string, function, or dynamic spec)
  :model-tiers  ALIST   -- named provider/effort tier entries
  :model-workloads ALIST -- workload tier/provider/effort entries

PARENT composition and arbitrary ordinary keys follow gptel preset value
semantics.  Ordinary keys prefer `niwa-KEY' and `niwa--KEY', then
`gptel-KEY' and `gptel--KEY'.  Required request setup composes with user
`:pre' and `:post' hooks."
  (declare (indent 1))
  (let (forms)
    (while keys
      (let ((key (pop keys))
            (value (pop keys)))
        (setq forms
              (append forms
                      (list key
                            (if (memq key '(:parents :tools :agents
                                           :model-tiers :model-workloads))
                                (if (eq (car-safe value) 'quote)
                                    value
                                  `',value)
                              value))))))
    `(niwa-preset--define ',name (list ,@forms))))

;;;###autoload
(defun niwa--define-presets ()
  "Define gptel presets for niwa actions."
  ;; Full editing preset for implementation
  (niwa-define-preset niwa-implement
    :description "Full editing capabilities with patch review workflow"
    :tools ((:tool "Read") (:tool "Glob") (:tool "Grep")
            (:discoverable read)
            (:tool "ApplyPatch") (:discoverable edit) (:tool "Bash")
            (:tool "Eval")
            (:tool "WriteStdin") (:tool "Monitor") (:tool "Notify")
            (:tool "Schedule")
            (:tool "ToolSearch") (:tool "ToolCall")
            (:discoverable util)
            (:discoverable (:tool "ListExecutions"))
            (:discoverable (:tool "StopExecution"))
            (:discoverable code) (:discoverable web) (:discoverable elisp)
            (:discoverable tasks) (:discoverable agents) (:discoverable notes))
    :agents (worker explorer reviewer verifier)
    :system (lambda ()
              (niwa-system-build-prompt
               'main
               :retained t
               :session niwa--session
               :refresh-buffer (current-buffer)))))

(defun niwa-preset-prepare-request (request)
  "Capture effective model, pending skills and authority on admitted REQUEST."
  (let ((info (niwa-engine-info (niwa-engine-owner request))))
    (when (and (plist-get info :backend) (plist-get info :model))
      (setf (niwa-request-model-policy request)
            (list :backend (plist-get info :backend) :model (plist-get info :model)
                  :effort (plist-get info :reasoning-effort)))))
  (niwa-skills--drain-pending-context request))


;;
;;; Request-time preset setup

(defun niwa-preset--refresh-tools (preset-name)
  "Resolve PRESET-NAME's active tools from the current registry."
  (when-let* ((meta (niwa-preset--resolved-metadata preset-name))
              (tool-specs (plist-get meta :tool-specs)))
    (setq-local
     gptel-tools
     (plist-get (niwa-tool-resolve-gptel tool-specs) :active))))

(defun niwa-preset--setup-catalog (preset-name)
  "Refresh the discoverable set from PRESET-NAME's tool specifications.
Retain loaded/pending names until WAIT reconciles them against the complete
set, including extras.  Reapplying the same preset does not unload tools;
a different scope cannot restore a capability absent from its final set."
  (when-let* ((session niwa--session))
    (let* ((meta (niwa-preset--resolved-metadata preset-name))
           (resolved (niwa-tool-resolve (plist-get meta :tool-specs))))
      (setf (niwa-session-tool-catalog session)
            (mapcar (lambda (tool)
                      (cons (list (niwa-tool-category tool)
                                  (niwa-tool-name tool))
                            (niwa-tool-summary tool)))
                    (plist-get resolved :discoverable))))))

(defun niwa-preset--setup-extras (preset-name)
  "Merge `niwa-preset-extra-tool-specs' for PRESET-NAME into the request.

Resolves the user-declared extra specs via `niwa-tool-resolve'
and its gptel counterpart.  Active entries are appended to the
buffer-local `gptel-tools' (deduplicating against the existing
list).  Deferred entries augment the session's `tool-catalog' --
the built-in deferred specs from the preset stay in place.

Has no effect when no extras are registered for PRESET-NAME."
  (when-let* ((extras (alist-get preset-name niwa-preset-extra-tool-specs)))
    (let* ((resolved (niwa-tool-resolve extras))
           (deferred-tools (plist-get resolved :discoverable))
           (gptel-resolved (niwa-tool-resolve-gptel extras))
           (active-gptel (plist-get gptel-resolved :active)))
      ;; Append active tools to buffer-local gptel-tools, avoiding
      ;; duplicates by identity.
      (when active-gptel
        (dolist (tool active-gptel)
          (unless (memq tool gptel-tools)
            (setq-local gptel-tools (append gptel-tools (list tool))))))
      ;; Merge deferred entries into the session's tool-catalog.
      (when-let* ((session niwa--session)
                  ((not (null deferred-tools))))
        (let ((existing (niwa-session-tool-catalog session))
              (additions
               (mapcar (lambda (tool)
                         (cons (list (niwa-tool-category tool)
                                     (niwa-tool-name tool))
                               (niwa-tool-summary tool)))
                       deferred-tools)))
          (dolist (entry additions)
            (unless (cl-find (car entry) existing :key #'car :test #'equal)
              (setq existing (append existing (list entry)))))
          (setf (niwa-session-tool-catalog session) existing))))))


;;
;;; FSM handler chain builder

(defun niwa-preset--build-transitions (transitions)
  "Add niwa's turn continuation rules to copied TRANSITIONS.
A final response with pending same-turn steering continues through WAIT.
Tool results end the turn instead of sampling again once
`niwa-turn-end-at-boundary' asked for that; errors and gptel or niwa
steering still take precedence, so queued steering is delivered first."
  (when-let* ((type-entry (assq 'TYPE transitions))
              (rules (cdr type-entry)))
    (setcdr type-entry
            (append (butlast rules)
                    (list (cons #'niwa-tools--pending-steering-p 'WAIT))
                    (last rules))))
  (when-let* ((tret-entry (assq 'TRET transitions))
              (rules (cdr tret-entry)))
    (let ((index (or (cl-position #'gptel--tool-result-p rules :key #'car)
                     (1- (length rules)))))
      (setcdr tret-entry
              (append (seq-take rules index)
                      (list (cons #'niwa-tools--pending-steering-p 'WAIT)
                            (cons #'niwa-turn-end-requested-p 'DONE))
                      (seq-drop rules index)))))
  transitions)

(defun niwa-preset--final-patch-handler (fsm &optional continuation)
  "Generate and display a final patch for FSM, then call CONTINUATION."
  (let* ((continuation (or continuation #'ignore))
         (continued nil)
         (finish
          (lambda (machine)
            (unless continued
              (setq continued t)
              (funcall continuation machine)))))
    (condition-case err
        (if-let* ((info (niwa-engine-info fsm))
                  (chat-buffer (plist-get info :buffer))
                  ((buffer-live-p chat-buffer))
                  (workspace (with-current-buffer chat-buffer
                               (niwa-workspace))))
            (let ((request (or (plist-get info :niwa-request)
                               (with-current-buffer chat-buffer
                                 niwa--current-request))))
              (niwa-preset--apply-final-patch
               fsm chat-buffer workspace request finish))
          (funcall finish fsm))
      (error
       (niwa--warn-once
        (niwa-diagnostic-scope (plist-get (niwa-engine-info fsm) :buffer))
        (list 'turn-fsm-handler "niwa-preset--final-patch-handler")
        "FSM handler niwa-preset--final-patch-handler failed: %s"
        (error-message-string err))
       (funcall finish fsm)))))

(defun niwa-preset--apply-final-patch
    (fsm chat-buffer workspace request &optional continuation)
  "Generate FSM's final patch for WORKSPACE, then call CONTINUATION.
CHAT-BUFFER owns the originating conversation and REQUEST owns its changes.

This handler runs from `gptel--fsm-transition', which gptel calls inside
its curl process sentinel -- and TRAMP re-runs a pending sentinel from the
wait loop of any remote command already in flight.  Generating the patch
reads every file the turn touched, so doing it there issues commands on a
connection that is mid-conversation, and the two cross replies.

`niwa-transport-run-when-idle' runs the work inline when the transport
is free.  When it defers, callback invocation and terminal settlement wait
in CONTINUATION so the turn cannot settle before the patch exists.

REQUEST is captured here, while the turn is still live, and handed to the
generator.  Deferring moved that work past the point where settlement
clears `niwa--current-request\', so a deferred generator that re-read
the buffer-local would find nil and signal."
  (niwa--turn-hold fsm)
  (let* ((continuation (or continuation #'ignore))
         (root (ignore-errors (niwa-workspace-root workspace)))
         finished
         (settle
          (lambda (patch error)
            (unwind-protect
                (unwind-protect
                    (funcall
                     (niwa--safe-fsm-handler
                      (lambda (_machine)
                        (when error (signal (car error) (cdr error)))
                        ;; Presentation belongs only to the current request.
                        (when (and (buffer-live-p chat-buffer)
                                   (niwa--turn-current-p fsm)
                                   patch (> (length patch) 0))
                          (niwa--replace-patch-buffer patch))))
                     fsm)
                  (funcall continuation fsm))
              (niwa--turn-release fsm))))
         (generate
          (lambda ()
            (unless finished
              (setq finished t)
              ;; The diffs run as helper processes; the turn waits for
              ;; them through its hold instead of blocking the callback
              ;; this handler runs in, where quitting is inhibited.
              (let (settled)
                (condition-case err
                    (if (buffer-live-p chat-buffer)
                        (with-current-buffer chat-buffer
                          (niwa--generate-final-patch
                           workspace request
                           (lambda (patch error)
                             (unless settled
                               (setq settled t)
                               (funcall settle patch error)))))
                      (setq settled t)
                      (funcall settle nil nil))
                  (error
                   (unless settled
                     (setq settled t)
                     (funcall settle nil err))))))))
         (cancel
          (lambda ()
            (unless finished
              (setq finished t)
              (unwind-protect
                  (if (niwa-request-p fsm)
                      (progn
                        (setf (niwa-request-cancelled-p fsm) t)
                        (funcall continuation fsm))
                    (niwa-preset--settle-terminal fsm t))
                (niwa--turn-release fsm))))))
    (condition-case err
        (if (and root (file-remote-p root) (niwa-transport-busy-p root))
            (unless (niwa-transport-run-when-idle
                     (list 'final-patch fsm) root generate cancel)
              (funcall cancel))
          (funcall generate))
      ((error quit)
       (funcall cancel)
       (signal (car err) (cdr err))))))

(defun niwa-preset--settle-terminal (owner &optional cancelled-p status)
  "Invoke OWNER's request callback, then settle its terminal state.
STATUS is success, aborted or error for an external request; native gptel
owners derive it from their FSM. CANCELLED-P overrides either with aborted."
  (setq status (if cancelled-p 'aborted
                 (or status (pcase (gptel-fsm-state owner)
                              ('DONE 'success) ('ABRT 'aborted) (_ 'error)))))
  (let ((info (niwa-engine-info owner)))
    (setf (niwa-engine-info owner)
          (plist-put info :niwa-terminal-status status)))
  (when-let* ((info (niwa-engine-info owner))
              ((not (plist-get info :niwa-terminal-callback-called)))
              (request-callback (plist-get info :niwa-request-callback))
              ((functionp request-callback)))
    (setf (niwa-engine-info owner)
          (plist-put info :niwa-terminal-callback-called t))
    (funcall
     (niwa--safe-fsm-handler
      (lambda (request-owner)
        (funcall request-callback (and (eq status 'aborted) 'abort) request-owner)))
     owner))
  (funcall
   (niwa--safe-fsm-handler
    (if (eq status 'success) #'niwa--complete-turn
      (lambda (request-owner) (niwa--fail-turn request-owner status))))
   owner))

(defun niwa-preset--terminal-handler (fsm)
  "Generate FSM's patch before callback invocation and settlement."
  (let ((info (gptel-fsm-info fsm)))
    (unless (plist-get info :niwa-terminal-started)
      (setq info (plist-put info :niwa-terminal-started t))
      (when-let* ((buffer (plist-get info :buffer))
                  ((buffer-live-p buffer)))
        (setq info
              (plist-put info :niwa-response-end
                         (with-current-buffer buffer
                           (copy-marker (point-max) nil)))))
      (setf (gptel-fsm-info fsm) info)
      (if (plist-get info :niwa-preparation-rejected)
          ;; No request was admitted.  An ambient replacement request must
          ;; not supply captures, patches or cancellation for this machine.
          (niwa-preset--settle-terminal fsm)
        (niwa-preset--final-patch-handler
         fsm #'niwa-preset--settle-terminal)))))

(defun niwa-preset--reject-preparation (fsm)
  "Visibly settle stale, unadmitted FSM without touching a newer request."
  (let* ((info (gptel-fsm-info fsm))
         (buffer (plist-get info :buffer))
         (reason "Implementation context changed during preparation; submit again"))
    (plist-put info :status "Request not sent")
    (plist-put info :error (list :type "stale_implementation_preparation" :message reason))
    (plist-put info :niwa-preparation-rejected t)
    (setf (gptel-fsm-info fsm) info)
    (niwa-diagnostic-message (niwa-diagnostic-scope buffer) "Request not sent: %s" reason)
    (when (and (buffer-live-p buffer)
               (buffer-local-value 'niwa--current-request buffer))
      ;; gptel's error presentation and last-FSM handler are buffer-scoped.
      ;; A late callback must leave the replacement's UI and FSM intact.
      (setf (gptel-fsm-handlers fsm)
            (cons (list 'ERRS #'niwa-preset--settle-terminal)
                  (assq-delete-all 'ERRS (copy-sequence (gptel-fsm-handlers fsm))))))
    (gptel--fsm-transition fsm 'ERRS)))

(defun niwa-preset--guard-preparation (fsm handlers)
  "Run WAIT HANDLERS only while FSM's preparation still owns its authority.
Catch stale admission after yielding checks too.  gptel runs sibling handlers
even after a nested terminal transition, so this gate owns the whole group."
  (condition-case err
      (progn
        (when-let* ((info (gptel-fsm-info fsm))
                    ((not (plist-get info :niwa-request-begun)))
                    (preparation (plist-get info :niwa-implementation-preparation)))
          (with-current-buffer (plist-get info :buffer)
            (niwa-implementation-policy-assert-preparation preparation)))
        (mapc (lambda (handler) (funcall handler fsm)) handlers))
    (niwa-stale-implementation-preparation
     (niwa-preset--reject-preparation fsm))
    (error
     ;; Request readiness can detect a replacement admitted while it yielded
     ;; and signal before reaching policy capture.  Settle that stale machine,
     ;; but leave errors unrelated to changed preparation on their old path,
     ;; including errors after this machine's own admission recorded its id.
     (if (when-let* ((info (gptel-fsm-info fsm))
                    ((not (plist-get info :niwa-request-begun)))
                    ((not (plist-get info :niwa-request-id)))
                    (preparation (plist-get info :niwa-implementation-preparation)))
           (condition-case nil
               (with-current-buffer (plist-get info :buffer)
                 (niwa-implementation-policy-assert-preparation preparation)
                 nil)
             (niwa-stale-implementation-preparation t)))
         (niwa-preset--reject-preparation fsm)
       (signal (car err) (cdr err))))))

(defun niwa-preset--build-handlers (handlers)
  "Build the standard niwa FSM handler chain from base HANDLERS.

HANDLERS is an alist like `gptel-send--handlers'.  Returns a new
alist with niwa-specific handlers added:

  1.  Same-turn steering delivery (WAIT state handler)
  1a.  Direct-child roster refresh (WAIT state handler)
  1b.  Inbound agent-message delivery (WAIT state handler)
  1c.  Deferred tool injection (WAIT state handler)
  1d.  ToolCall effective-roster description (WAIT state handler)
  1e.  System-reminder delivery at the final provider-dispatch seam
  2.  Ordered final patch, request callback, and terminal settlement"
  ;; 1. Add the pre-sample WAIT handlers in execution order.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (append
               (list #'niwa-tools--handle-steering-inject
                     #'niwa-tools--handle-agent-roster-inject
                     #'niwa-tools--handle-message-inject
                     #'niwa-tools--handle-plan-tool-filter)
               (cdr wait-entry)))))
  ;; 1f. Begin the niwa-request on the first WAIT entry.  WAIT is
  ;; re-entered after each tool call loop, so the guard on
  ;; `:niwa-request-begun' keeps request-begin idempotent per FSM.
  ;; After `niwa-request-begin' creates the request, drain any buffer-local
  ;; `niwa-skills--pending-request-context' permission/hooks into the new
  ;; request and invocation records into the session.  Model/effort was already
  ;; consumed by the pre-realization prompt transform.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons (lambda (fsm)
                      (let* ((info (gptel-fsm-info fsm))
                             (begun (plist-get info :niwa-request-begun))
                             (chat-buffer (plist-get info :buffer)))
                        (when (and (not begun)
                                   chat-buffer
                                   (buffer-live-p chat-buffer))
                          (with-current-buffer chat-buffer
                            (when niwa--session
                              (niwa-session-artifacts-assert-new-mutation-authority
                               niwa--session)
                              (when-let* ((preparation
                                           (plist-get info :niwa-implementation-preparation)))
                                (niwa-implementation-policy-assert-preparation preparation))
                              (let ((adopt
                                     (lambda (request)
                                       (setf (niwa-request-fsm request) fsm)
                                       (plist-put info :niwa-request-id
                                                  (niwa-request-id request))
                                       (plist-put info :niwa-request request))))
                                (unless niwa--current-request
                                  (let ((niwa-implementation-policy--admission-preparation
                                         (plist-get info :niwa-implementation-preparation)))
                                    ;; Adopt at admission: a later admission
                                    ;; step's error must not read as stale.
                                    (niwa-request-begin
                                     niwa--session
                                     (list :admitted adopt))))
                                (when (and niwa--current-request
                                           (fboundp 'niwa-request-id))
                                  (funcall adopt niwa--current-request)))
                              (niwa-goal-capture-request fsm)
                              (niwa-preset-prepare-request niwa--current-request)
                              (when (fboundp
                                     'niwa-view-stream-ensure-progress-for-fsm)
                                (niwa-view-stream-ensure-progress-for-fsm
                                 fsm))))
                          (setf (gptel-fsm-info fsm)
                                (plist-put info :niwa-request-begun t)))))
                    (cdr wait-entry)))))
  ;; 1f. Continuation auto-compaction: keep all WAIT injectors ahead
  ;; of the network send, then gate the realized request immediately
  ;; before gptel fires it.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (mapcar
               (lambda (handler)
                 (if (eq handler #'gptel--handle-wait)
                     #'niwa--compact-handle-wait
                   handler))
               (cdr wait-entry)))))
  ;; Record API-reported token usage at TPRE so tool-using turns retain
  ;; their latest intermediate baseline.  Terminal recording belongs to
  ;; the successful/failure transactions below.
  (let ((tpre-entry (assq 'TPRE handlers)))
    (if tpre-entry
        (setcdr tpre-entry
                (append (cdr tpre-entry)
                        (list #'niwa-compact-estimation-record-token-baseline)))
      (push (list 'TPRE #'niwa-compact-estimation-record-token-baseline) handlers)))
  ;; gptel runs a terminal handler list synchronously.  Keep the dependent
  ;; niwa work in one handler so a deferred patch can resume the rest in
  ;; order instead of letting sibling handlers persist an empty patch.
  (unless (assq 'ABRT handlers)
    (push '(ABRT) handlers))
  (setq handlers
        (niwa--add-termination-handler
         #'niwa-preset--terminal-handler handlers))
  (setq handlers (niwa--wrap-terminal-handlers handlers))
  (when-let* ((wait-entry (assq 'WAIT handlers)))
    (let ((wait-handlers (cdr wait-entry)))
      (setcdr wait-entry
              (list (lambda (fsm)
                      (niwa-preset--guard-preparation fsm wait-handlers))))))
  handlers)


;;
;;; Termination handler utility

(defun niwa--add-termination-handler (handler handlers &optional transitions)
  "Update FSM's state HANDLERS to call HANDLER when the request terminates.

Optional argument TRANSITIONS is an alist like
`gptel-request--transitions'.

The HANDLER will receive one argument when the request terminates:
- FSM: the `gptel-fsm' struct for the request

The request is considered to have terminated when the FSM reaches a
state with no possible transition, or an explicit ABRT handler."
  (let* ((terminal-states (niwa--terminal-states handlers transitions))
         (terminal-handlers
          (cl-loop for state in terminal-states
                   for entry = (assq state handlers)
                   collect (if (member handler (cdr entry))
                               entry
                             (cons state (append (cdr entry) (list handler)))))))
    (append (cl-remove-if (lambda (entry) (member (car entry) terminal-states))
                          handlers)
            terminal-handlers)))

(defun niwa--terminal-states (handlers &optional transitions)
  "Return terminal states represented by HANDLERS and TRANSITIONS."
  (let* ((transitions (or transitions gptel-request--transitions))
         (all-states
          (cl-remove-duplicates
           (append
            (mapcar #'car transitions)
            (and (assq 'ABRT handlers) '(ABRT))
            (cl-mapcan (lambda (entry) (mapcar #'cdr (cdr entry)))
                       transitions)))))
    (cl-remove-if-not
     (lambda (state)
       (let ((entry (assq state transitions)))
         (or (null entry) (null (cdr entry)))))
     all-states)))

(defun niwa--wrap-terminal-handlers (handlers &optional transitions)
  "Wrap terminal-state HANDLERS for TRANSITIONS so one failure cannot skip cleanup."
  (let ((terminal-states (niwa--terminal-states handlers transitions)))
    (mapcar
     (lambda (entry)
       (if (memq (car entry) terminal-states)
           (cons (car entry)
                 (mapcar #'niwa--safe-fsm-handler (cdr entry)))
         entry))
     handlers)))

(provide 'niwa-presets)

;;; niwa-presets.el ends here
