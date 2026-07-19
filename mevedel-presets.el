;;; mevedel-presets.el -- Presets used -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the four gptel presets that drive a mevedel session:
;; `mevedel-discuss' (read-only exploration), `mevedel-implement'
;; (full edit access, inherits discuss), `mevedel-revise' (editing
;; with instruction context, inherits implement), and `mevedel-tutor'
;; (tutoring assistant, inherits discuss).
;;
;; Each preset assembles tool lists and registers sub-agents buffer-locally at
;; request time.  This module builds the request FSM handler chain around the
;; neutral terminal transaction in `mevedel-turn'.  Deferred-tool handler
;; wiring also lives here because it piggy-backs on preset setup.

;;; Code:

(require 'cl-lib)
(require 'mevedel-turn)

(eval-when-compile
  (require 'mevedel-structs)
  ;; Needed for `setf' on `gptel-fsm' struct slots (native comp)
  (require 'gptel-request nil t))

;; `gptel'
(declare-function gptel--apply-preset "ext:gptel" (preset &optional setter))
(declare-function gptel--modify-value "ext:gptel" (original new-spec))
(declare-function gptel-make-preset "ext:gptel" (name &rest keys))

;; `gptel-request'
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(defvar gptel-request--transitions)
(defvar gptel-tools)

;; `mevedel-agents'
(declare-function mevedel-agents--setup-for-request
                  "mevedel-agents" (&optional preset-name))

;; `mevedel-chat'
(declare-function mevedel--generate-final-patch
                  "mevedel-chat" (&optional workspace))
(declare-function mevedel--replace-patch-buffer
                  "mevedel-chat" (patch-content))
(defvar mevedel--current-directive-uuid)

;; `mevedel-compact'
(declare-function mevedel--compact-handle-wait "mevedel-compact" (fsm))
(declare-function mevedel--compact-record-token-baseline
                  "mevedel-compact" (fsm))

;; `mevedel-models'
(declare-function mevedel-model-merge-tiers
                  "mevedel-models" (additions current))
(declare-function mevedel-model-merge-workloads
                  "mevedel-models" (additions current))

;; `mevedel-overlays'
(declare-function mevedel--find-directive-by-uuid
                  "mevedel-overlays" (uuid))

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-fork-now
                  "mevedel-session-persistence" (buffer))
(defvar mevedel-session--fork-pending)

;; `mevedel-skills-invoke'
(declare-function mevedel-skills--drain-pending-context
                  "mevedel-skills-invoke" (request))

;; `mevedel-structs'
(declare-function mevedel-request-begin
                  "mevedel-structs" (session &optional directive-uuid))
(declare-function mevedel-session-preset-name "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-preset-settings "mevedel-structs" (cl-x) t)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-build-prompt
                  "mevedel-system"
                  (base-prompt &optional workspace working-directory
                               session refresh-buffer))
(defvar mevedel-system--base-prompt)
(defvar mevedel-system--tutor-base-prompt)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-category "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-resolve "mevedel-tool-registry" (specs))
(declare-function mevedel-tool-resolve-gptel
                  "mevedel-tool-registry" (specs))
(declare-function mevedel-tool-summary "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tools'
(declare-function mevedel-tools--handle-agent-roster-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-deferred-inject
                  "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-message-inject
                  "mevedel-tools" (fsm))

;; `mevedel-turn'
(declare-function mevedel--complete-turn "mevedel-turn" (fsm))
(declare-function mevedel--fail-turn "mevedel-turn" (fsm status))
(declare-function mevedel--safe-fsm-handler "mevedel-turn" (handler))

;; `mevedel-view-stream'
(declare-function mevedel-view-stream-ensure-progress-for-fsm
                  "mevedel-view-stream" (fsm))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;;
;;; Presets

(defcustom mevedel-action-preset-alist
  '((implement . mevedel-implement)
    (discuss . mevedel-discuss)
    (revise . mevedel-revise)
    (tutor . mevedel-tutor))
  "Alist mapping actions to presets."
  :group 'mevedel
  :type '(alist :key-type symbol))

(defcustom mevedel-preset-extra-tool-specs nil
  "Alist mapping preset names to extra tool specs.

Each entry is (PRESET-SYMBOL . SPEC-LIST).  SPEC-LIST uses the same
forms accepted by `mevedel-define-preset''s :tools keyword (bare
symbols, (:group X), (:tool X), (:deferred X)).  Extras are merged
into the preset's tool set every time the preset is applied:
deferred entries are appended to the session's deferred-set and
active entries are appended to the buffer-local `gptel-tools'.

Useful for giving user-wrapped tools a home in an existing preset
without redefining it."
  :group 'mevedel
  :type '(alist :key-type symbol :value-type (repeat sexp)))

(defvar mevedel-preset--registry nil
  "Alist mapping preset names to mevedel-specific metadata.
Each value contains the raw parent, agent, tool, and variable settings.")

(defvar mevedel-preset--temporary-p nil
  "Non-nil while a preset is applied for one dynamic request only.")

(defconst mevedel-preset--structural-keys
  '(:description :parents :pre :post :backend :model :system :tools :agents
    :model-tiers :model-workloads)
  "Preset keys with dedicated mevedel or gptel behavior.")

(defun mevedel-preset--variable-for-key (key)
  "Return the variable selected by ordinary preset KEY, or nil.
Mevedel public and private variables take precedence over gptel variables."
  (let ((name (substring (symbol-name key) 1)))
    (cl-find-if
     #'boundp
     (delq nil
           (mapcar #'intern-soft
                   (list (concat "mevedel-" name)
                         (concat "mevedel--" name)
                         (concat "gptel-" name)
                         (concat "gptel--" name)))))))

(defun mevedel-preset--resolved-metadata (name)
  "Return inherited mevedel metadata for preset NAME."
  (let* ((metadata (or (alist-get name mevedel-preset--registry)
                       (user-error "Unknown mevedel preset: %s" name)))
         (resolved nil))
    (dolist (parent (ensure-list (plist-get metadata :parents)))
      (let ((parent-meta (mevedel-preset--resolved-metadata parent)))
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

(defun mevedel-preset--setting-specs (name)
  "Return inherited ordinary mevedel setting specs for preset NAME."
  (let* ((metadata (or (alist-get name mevedel-preset--registry)
                       (user-error "Unknown mevedel preset: %s" name)))
         specs)
    (dolist (parent (ensure-list (plist-get metadata :parents)))
      (setq specs (append specs (mevedel-preset--setting-specs parent))))
    (append specs (plist-get metadata :settings))))

(defun mevedel-preset-resolve-settings (name)
  "Return preset NAME's resolved mevedel variable alist."
  (let (resolved)
    (dolist (setting (mevedel-preset--setting-specs name))
      (let* ((symbol (car setting))
             (current (if-let* ((entry (assq symbol resolved)))
                          (cdr entry)
                        (symbol-value symbol)))
             (value (gptel--modify-value current (cdr setting))))
        (setf (alist-get symbol resolved) value)))
    resolved))

(defun mevedel-preset--apply-settings (name)
  "Apply preset NAME's mevedel settings to the current request or session."
  (let ((settings
         (plist-get (or (alist-get name mevedel-preset--registry)
                        (user-error "Unknown mevedel preset: %s" name))
                    :settings)))
    (dolist (setting settings)
      (let* ((symbol (car setting))
             (value (gptel--modify-value
                     (symbol-value symbol) (cdr setting))))
        (if mevedel-preset--temporary-p
            (set symbol value)
          (set (make-local-variable symbol) value))))
    (when (and (not mevedel-preset--temporary-p) mevedel--session)
      (let ((resolved
             (mapcar (lambda (symbol)
                       (cons symbol (symbol-value symbol)))
                     (delete-dups
                      (mapcar #'car
                              (mevedel-preset--setting-specs name))))))
        (setf (mevedel-session-preset-name mevedel--session) name
              (mevedel-session-preset-settings mevedel--session)
              (copy-tree resolved))
        resolved))))

(defun mevedel-preset--post (name user-post)
  "Run USER-POST and required mevedel setup for preset NAME."
  (when user-post (funcall user-post))
  (mevedel-preset--apply-settings name)
  (mevedel-agents--setup-for-request name)
  (mevedel-preset--setup-deferred name)
  (mevedel-preset--setup-extras name))

(defun mevedel-preset--define (name keys)
  "Register exact preset NAME from evaluated KEYS."
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
          (require 'mevedel-models)
          (push (cons (if (eq key :model-tiers)
                          'mevedel-model-tiers
                        'mevedel-model-workloads)
                      (list :function
                            (apply-partially
                             (if (eq key :model-tiers)
                                 #'mevedel-model-merge-tiers
                               #'mevedel-model-merge-workloads)
                             value)))
                settings))
         ((eq key :tools)
          (setq gptel-keys
                (append gptel-keys
                        (list key
                              (plist-get
                               (mevedel-tool-resolve-gptel value)
                               :active)))))
         ((memq key mevedel-preset--structural-keys)
          (unless (eq key :post)
            (setq gptel-keys (append gptel-keys (list key value)))))
         (t
          (let ((symbol (mevedel-preset--variable-for-key key)))
            (cond
             ((and symbol
                   (string-prefix-p "mevedel-" (symbol-name symbol)))
              (push (cons symbol value) settings))
             (symbol
              (setq gptel-keys (append gptel-keys (list key value))))
             (t
              (display-warning
               '(mevedel presets)
               (format "mevedel preset %s: setting for %s not found, ignoring"
                       name key)))))))))
    (let ((metadata (list :parents (plist-get raw-keys :parents)
                          :settings (nreverse settings))))
      (when (plist-member raw-keys :agents)
        (setq metadata
              (plist-put metadata :agents (plist-get raw-keys :agents))))
      (when (plist-member raw-keys :tools)
        (setq metadata
              (plist-put metadata :tool-specs (plist-get raw-keys :tools))))
      (setf (alist-get name mevedel-preset--registry) metadata))
    (apply #'gptel-make-preset name
           (append gptel-keys
                   (list :post (apply-partially
                                #'mevedel-preset--post name user-post))))))

(defun mevedel-preset-apply (name &optional buffer)
  "Apply mevedel preset NAME buffer-locally to BUFFER and its session."
  (with-current-buffer (or buffer (current-buffer))
    (gptel--apply-preset
     name (lambda (symbol value)
            (set (make-local-variable symbol) value)))))

(defun mevedel-preset-restore-session (session &optional buffer)
  "Restore SESSION's saved mevedel preset settings in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (dolist (setting (mevedel-session-preset-settings session))
      (set (make-local-variable (car setting)) (cdr setting)))))

(defmacro mevedel-with-preset (name &rest body)
  "Run BODY with mevedel preset NAME applied for this request only."
  (declare (indent 1) (debug t))
  `(let ((preset ,name))
     (if (and (symbolp preset)
              (assq preset mevedel-preset--registry))
         (let* ((symbols (mapcar #'car
                                 (mevedel-preset-resolve-settings preset)))
                (values (mapcar #'symbol-value symbols))
                (mevedel-preset--temporary-p t))
           (cl-progv symbols values
             (gptel-with-preset preset ,@body)))
       (gptel-with-preset preset ,@body))))

(defmacro mevedel-define-preset (name &rest keys)
  "Define a mevedel preset NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description  STRING  -- preset description for selection UI
  :tools        LIST    -- tool specs for `mevedel-tool-resolve-gptel'
                           (bare symbols, (:group X), (:tool X), (:deferred X))
  :agents       LIST    -- list of agent name symbols for request-time setup
  :system       VALUE   -- system prompt (string, function, or dynamic spec)
  :model-tiers  ALIST   -- named provider/effort tier entries
  :model-workloads ALIST -- workload tier/provider/effort entries

PARENT composition and arbitrary ordinary keys follow gptel preset value
semantics.  Ordinary keys prefer `mevedel-KEY' and `mevedel--KEY', then
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
    `(mevedel-preset--define ',name (list ,@forms))))

;;;###autoload
(defun mevedel--define-presets ()
  "Define gptel presets for mevedel actions."
  (require 'gptel)
  (require 'mevedel-tool-registry)

  ;; Read-only preset for discussion/analysis
  (mevedel-define-preset mevedel-discuss
    :description "Read-only tools for code analysis and discussion"
    :tools (read util (:tool "Bash")
            (:tool "WriteStdin") (:tool "ListExecutions")
            (:tool "StopExecution")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (worker explorer reviewer verifier)
    :system (lambda ()
              (mevedel-system-build-prompt
               mevedel-system--base-prompt nil nil
               mevedel--session (current-buffer))))

  ;; Full editing preset for implementation
  (mevedel-define-preset mevedel-implement
    :description "Full editing capabilities with patch review workflow"
    :parents (mevedel-discuss)
    :tools (read util edit (:tool "Bash")
            (:tool "WriteStdin") (:tool "ListExecutions")
            (:tool "StopExecution")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (worker explorer reviewer verifier)
    :system (lambda ()
              (mevedel-system-build-prompt
               mevedel-system--base-prompt nil nil
               mevedel--session (current-buffer))))

  ;; Revision preset with previous patch context
  (mevedel-define-preset mevedel-revise
    :description "Revise previous implementation with full context"
    :parents (mevedel-implement)
    :system "You are revising a previous implementation. The previous patch and its context are included in the conversation. Analyze what needs to be changed and create an improved implementation.")

  ;; Tutoring preset - guides through hints, never provides solutions
  (mevedel-define-preset mevedel-tutor
    :description "Tutoring preset - guides through hints, never provides solutions"
    :parents (mevedel-discuss)
    :tools (read util (:tool "Bash")
            (:tool "WriteStdin") (:tool "ListExecutions")
            (:tool "StopExecution")
            (:tool "GetHints") (:tool "RecordHint")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (worker explorer reviewer verifier)
    :system (lambda ()
              (mevedel-system-build-prompt
               mevedel-system--tutor-base-prompt nil nil
               mevedel--session (current-buffer)))))

;;
;;; Request-time preset setup

(defun mevedel-preset--setup-deferred (preset-name)
  "Populate the current session's deferred tool set from PRESET-NAME.

Resolves the preset's tool specs and takes the `:deferred' portion
directly (no active-set exclusion).  Each entry is a pair
\((CATEGORY NAME) . SHORT-DESCRIPTION) taken from the mevedel-tool
struct, suitable for `mevedel-tools--search-deferred'.

Writes to the buffer-local session's `deferred-set' slot and clears
any prior deferred state so every request starts with a clean
lifecycle.  Has no effect when the preset has no deferred specs or
no active session is bound."
  (when-let* ((session mevedel--session)
              (meta (mevedel-preset--resolved-metadata preset-name))
              (tool-specs (plist-get meta :tool-specs)))
    (let* ((resolved (mevedel-tool-resolve tool-specs))
           (deferred (plist-get resolved :deferred)))
      (setf (mevedel-session-deferred-set session)
            (mapcar (lambda (tool)
                      (cons (list (mevedel-tool-category tool)
                                  (mevedel-tool-name tool))
                            (mevedel-tool-summary tool)))
                    deferred))
      ;; Reset lifecycle state so expiry/TTL starts fresh for this request.
      (setf (mevedel-session-deferred-pending session) nil)
      (setf (mevedel-session-deferred-injected session) nil)
      (setf (mevedel-session-deferred-used session) nil)
      (setf (mevedel-session-deferred-expired session) nil))))

(defun mevedel-preset--setup-extras (preset-name)
  "Merge `mevedel-preset-extra-tool-specs' for PRESET-NAME into the request.

Resolves the user-declared extra specs via `mevedel-tool-resolve'
and its gptel counterpart.  Active entries are appended to the
buffer-local `gptel-tools' (deduplicating against the existing
list).  Deferred entries augment the session's `deferred-set' --
the built-in deferred specs from the preset stay in place.

Has no effect when no extras are registered for PRESET-NAME."
  (when-let* ((extras (alist-get preset-name mevedel-preset-extra-tool-specs)))
    (let* ((resolved (mevedel-tool-resolve extras))
           (deferred-tools (plist-get resolved :deferred))
           (gptel-resolved (mevedel-tool-resolve-gptel extras))
           (active-gptel (plist-get gptel-resolved :active)))
      ;; Append active tools to buffer-local gptel-tools, avoiding
      ;; duplicates by identity.
      (when active-gptel
        (dolist (tool active-gptel)
          (unless (memq tool gptel-tools)
            (setq-local gptel-tools (append gptel-tools (list tool))))))
      ;; Merge deferred entries into the session's deferred-set.
      (when-let* ((session mevedel--session)
                  ((not (null deferred-tools))))
        (let ((existing (mevedel-session-deferred-set session))
              (additions
               (mapcar (lambda (tool)
                         (cons (list (mevedel-tool-category tool)
                                     (mevedel-tool-name tool))
                               (mevedel-tool-summary tool)))
                       deferred-tools)))
          (dolist (entry additions)
            (unless (cl-find (car entry) existing :key #'car :test #'equal)
              (setq existing (append existing (list entry)))))
          (setf (mevedel-session-deferred-set session) existing))))))


;;
;;; FSM handler chain builder

(defun mevedel-preset--final-patch-handler (fsm)
  "Generate and display a final patch for FSM when a workspace exists."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer))
              (workspace (with-current-buffer chat-buffer
                           (mevedel-workspace))))
    (let* ((directive-uuid (with-current-buffer chat-buffer
                             mevedel--current-directive-uuid))
           (final-patch (with-current-buffer chat-buffer
                          (mevedel--generate-final-patch workspace))))
      (when (and final-patch (> (length final-patch) 0))
        (when directive-uuid
          (when-let* ((directive (mevedel--find-directive-by-uuid
                                  directive-uuid)))
            (overlay-put directive 'mevedel-directive-patch final-patch)))
        (mevedel--replace-patch-buffer final-patch)))))

(defun mevedel-preset--build-handlers (handlers)
  "Build the standard mevedel FSM handler chain from base HANDLERS.

HANDLERS is an alist like `gptel-send--handlers'.  Returns a new
alist with mevedel-specific handlers added:

  1.  Direct-child roster refresh (WAIT state handler)
  1a.  Inbound agent-message delivery (WAIT state handler)
  1b.  Deferred tool injection (WAIT state handler)
  2.  Final patch generation (terminal state handler)
  3.  Request callback invocation (terminal state handler)
  4.  Canonical successful-turn transaction (DONE state handler only)
  5.  Failure and abort cleanup"
  ;; 1. Add the pre-sample WAIT handlers in execution order.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (append
               (list #'mevedel-tools--handle-agent-roster-inject
                     #'mevedel-tools--handle-message-inject
                     #'mevedel-tools--handle-deferred-inject)
               (cdr wait-entry)))))
  ;; 1c. Begin the mevedel-request on the first WAIT entry.  WAIT is
  ;; re-entered after each tool call loop, so the guard on
  ;; `:mevedel-request-begun' keeps request-begin idempotent per FSM.
  ;; Materialize a fork-pending rewind preview before `request-begin'
  ;; runs -- this catches direct data-buffer send paths (gptel-send,
  ;; agent invocations) that bypass `mevedel-view-send'.  After
  ;; `mevedel-request-begin' creates the request, drain any buffer-local
  ;; `mevedel-skills--pending-request-context' permission/hooks into the new
  ;; request and invocation records into the session.  Model/effort was already
  ;; consumed by the pre-realization prompt transform.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons (lambda (fsm)
                      (let* ((info (gptel-fsm-info fsm))
                             (begun (plist-get info :mevedel-request-begun))
                             (chat-buffer (plist-get info :buffer)))
                        (when (and (not begun)
                                   chat-buffer
                                   (buffer-live-p chat-buffer))
                          (with-current-buffer chat-buffer
                            (when (bound-and-true-p
                                   mevedel-session--fork-pending)
                              (require 'mevedel-session-persistence)
                              (mevedel-session-persistence-fork-now
                               chat-buffer))
                            (when mevedel--session
                              (mevedel-request-begin
                               mevedel--session
                               mevedel--current-directive-uuid)
                              ;; Drain pending stash from user skill
                              ;; invocation.
                              (when (and (boundp 'mevedel--current-request)
                                         mevedel--current-request)
                                (mevedel-skills--drain-pending-context
                                 mevedel--current-request))
                              (when (fboundp
                                     'mevedel-view-stream-ensure-progress-for-fsm)
                                (mevedel-view-stream-ensure-progress-for-fsm
                                 fsm))))
                          (setf (gptel-fsm-info fsm)
                                (plist-put info :mevedel-request-begun t)))))
                    (cdr wait-entry)))))
  ;; 1c. Continuation auto-compaction: keep all WAIT injectors ahead
  ;; of the network send, then gate the realized request immediately
  ;; before gptel fires it.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (mapcar
               (lambda (handler)
                 (if (eq handler #'gptel--handle-wait)
                     #'mevedel--compact-handle-wait
                   handler))
               (cdr wait-entry)))))
  ;; 2. Generate final patch and store in directive
  (setq handlers
        (mevedel--add-termination-handler
         #'mevedel-preset--final-patch-handler
         handlers))
  ;; 3. Run callback from instruction
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (request-callback
                        (plist-get info :mevedel-request-callback)))
             (when (functionp request-callback)
               (funcall request-callback nil fsm))))
         handlers))
  ;; Record API-reported token usage at TPRE so tool-using turns retain
  ;; their latest intermediate baseline.  Terminal recording belongs to
  ;; the successful/failure transactions below.
  (let ((tpre-entry (assq 'TPRE handlers)))
    (if tpre-entry
        (setcdr tpre-entry
                (append (cdr tpre-entry)
                        (list #'mevedel--compact-record-token-baseline)))
      (push (list 'TPRE #'mevedel--compact-record-token-baseline) handlers)))
  ;; Failure terminals retain StopFailure, skip persistence and queued
  ;; follow-up submission, and still perform all shared cleanup steps.
  (dolist (state '(ERRS ABRT))
    (let* ((entry (assq state handlers))
           (failure-status (if (eq state 'ABRT) 'aborted 'error))
           (failure-handler
            (lambda (fsm)
              (mevedel--fail-turn fsm failure-status))))
      (if entry
          (setcdr entry (append (cdr entry) (list failure-handler)))
        (push (list state failure-handler) handlers))))
  (setq handlers (mevedel--wrap-terminal-handlers handlers))
  ;; Install the internally isolated successful transaction after the
  ;; ordinary terminal handlers have been wrapped.  Keeping the named
  ;; function itself in the chain gives direct fork turns the exact same
  ;; entry point.
  (let ((done-entry (assq 'DONE handlers)))
    (if done-entry
        (unless (memq #'mevedel--complete-turn (cdr done-entry))
          (setcdr done-entry
                  (append (cdr done-entry) (list #'mevedel--complete-turn))))
      (push (list 'DONE #'mevedel--complete-turn) handlers)))
  handlers)


;;
;;; Termination handler utility

(defun mevedel--add-termination-handler (handler handlers &optional transitions)
  "Update FSM's state HANDLERS to call HANDLER when the request terminates.

Optional argument TRANSITIONS is an alist like
`gptel-request--transitions'.

The HANDLER will receive one argument when the request terminates:
- FSM: the `gptel-fsm' struct for the request

The request is considered to have terminated when the FSM reaches a
state with no possible transitions to another state."
  (let* (;; An alist of states mapped to potential next states. See
         ;; 'gptel-request--transitions'.
         (transitions (or transitions gptel-request--transitions))
         ;; Find all potential next states in one of the rules
         (all-states
          (cl-remove-duplicates
           (append
            (mapcar #'car transitions)
            (cl-mapcan (lambda (entry) (mapcar #'cdr (cdr entry))) transitions))))
         ;; Collect states that either don't appear as keys, or appear as keys
         ;; but have no possible next states. These are states which can't
         ;; transition to any other states.
         (terminal-states
          (cl-remove-if-not
           (lambda (state)
             (let ((entry (assq state transitions)))
               ;; If no entry exists or entry exists but has no transitions
               (or (null entry) (null (cdr entry)))))
           all-states))
         ;; Alist whose keys are the terminal states, and values are their new
         ;; lists of handlers
         (terminal-state-handlers
          (cl-loop
           for state in terminal-states
           for existing-entry = (assq state handlers)
           collect (if existing-entry
                       (if (member handler (cdr existing-entry))
                           ;; Handler already present, return entry unchanged
                           existing-entry
                         ;; Handler not present, add it
                         (cons state (append (cdr existing-entry) (list handler))))
                     ;; (cons state (append (cdr existing-entry) (list handler)))
                     (cons state (list handler)))))
         ;; Create a new handlers list for this FSM
         (augmented-handlers
          (append
           ;; Copy existing non-terminal handlers
           (cl-remove-if (lambda (entry) (member (car entry) terminal-states)) handlers)
           ;; Add our terminal state handlers
           terminal-state-handlers)))
    ;; Update the handlers list
    augmented-handlers))

(defun mevedel--terminal-states (handlers &optional transitions)
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

(defun mevedel--wrap-terminal-handlers (handlers &optional transitions)
  "Wrap terminal-state HANDLERS for TRANSITIONS so one failure cannot skip cleanup."
  (let ((terminal-states (mevedel--terminal-states handlers transitions)))
    (mapcar
     (lambda (entry)
       (if (memq (car entry) terminal-states)
           (cons (car entry)
                 (mapcar #'mevedel--safe-fsm-handler (cdr entry)))
         entry))
     handlers)))

(provide 'mevedel-presets)

;;; mevedel-presets.el ends here
