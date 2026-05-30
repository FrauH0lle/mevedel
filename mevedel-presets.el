;;; mevedel-presets.el -- Presets used -*- lexical-binding: t -*-

;;; Commentary:

;; Defines the four gptel presets that drive a mevedel session:
;; `mevedel-discuss' (read-only exploration), `mevedel-implement'
;; (full edit access, inherits discuss), `mevedel-revise' (editing
;; with instruction context, inherits implement), and `mevedel-tutor'
;; (tutoring assistant, inherits discuss).
;;
;; Each preset assembles tool lists, installs FSM termination
;; handlers for cleanup, and registers sub-agents buffer-locally at
;; request time.  Deferred-tool wiring (BWAIT state, handler table
;; injection) is also done here because it piggy-backs on preset
;; setup.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs)
  ;; Needed for `setf' on `gptel-fsm' struct slots (native comp)
  (require 'gptel-request nil t))

;; `gptel'
(declare-function gptel-make-preset "ext:gptel" (name &rest keys))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(defvar gptel-request--transitions)
(defvar gptel-tools)

;; `mevedel-chat'
(declare-function mevedel--generate-final-patch "mevedel-chat" (&optional workspace))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch-content))
(declare-function mevedel--implementation-permission-mode-restore
                  "mevedel-chat" ())
(defvar mevedel--current-directive-uuid)

;; `mevedel-view'
(declare-function mevedel-view--handle-queued-user-message-inject
                  "mevedel-view" (fsm))
(declare-function mevedel-view--schedule-queued-user-message-drain
                  "mevedel-view" (fsm))
(declare-function mevedel-view--ensure-request-progress-for-fsm
                  "mevedel-view" (fsm))

;; `mevedel-compact'
(declare-function mevedel--compact-record-token-baseline
                  "mevedel-compact" (fsm))
(declare-function mevedel--compact-handle-wait
                  "mevedel-compact" (fsm))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `mevedel-hooks'
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))

;; `mevedel-skills'
(declare-function mevedel-skills--apply-overrides-handler
                  "mevedel-skills" (fsm))
(declare-function mevedel-skills--drain-pending-context
                  "mevedel-skills" (request))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-deferred-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-message-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--handle-terminal-mailbox "mevedel-tools" (fsm))
(declare-function mevedel-tools--background-agents-pending-p "mevedel-tool-ui" (info))
(declare-function mevedel-tools--handle-bwait "mevedel-tool-ui" (fsm))
(declare-function mevedel-tools--bwait-injected-table "mevedel-tool-ui" (source))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-resolve "mevedel-tool-registry" (specs))
(declare-function mevedel-tool-resolve-gptel "mevedel-tool-registry" (specs))
(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-description "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-summary "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-category "mevedel-tool-registry" (cl-x) t)

;; `mevedel-tool-ui'
(declare-function mevedel--clear-pending-access-requests "mevedel-tool-ui" (&rest _))

;; `mevedel-agents'
(declare-function mevedel-agents--setup-for-request "mevedel-agents"
                  (&optional preset-name))

;; `mevedel-tool-fs'
(defvar mevedel--request-file-snapshots)

;; `mevedel-structs'
(defvar mevedel--current-request)
(declare-function mevedel-request-begin "mevedel-structs"
                  (session &optional directive-uuid))
(declare-function mevedel-request-end "mevedel-structs" ())

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence-save
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-persistence-fork-now
                  "mevedel-session-persistence" (buffer))
(defvar mevedel-session-persistence)
(defvar mevedel-session--save-failed)
(defvar mevedel-session--fork-pending)

;; `mevedel-overlays'
(declare-function mevedel--find-directive-by-uuid "mevedel-overlays" (uuid))

;; `mevedel-structs' (accessors come from eval-when-compile require above)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-build-prompt
                  "mevedel-system"
                  (base-prompt &optional workspace working-directory))
(defvar mevedel-system--base-prompt)
(defvar mevedel-system--tutor-base-prompt)


;;
;;; Presets

(defun mevedel--fsm-error-message (fsm)
  "Return a compact error message for FSM, or nil."
  (let* ((info (and fsm (gptel-fsm-info fsm)))
         (error (plist-get info :error))
         (status (plist-get info :status))
         (error-type (and (listp error) (plist-get error :type)))
         (error-message (and (listp error) (plist-get error :message))))
    (or error-message
        (and error-type status (format "%s: %s" error-type status))
        (and error-type (format "%s" error-type))
        (and status (format "%s" status)))))

(defun mevedel--run-turn-terminal-hook (fsm event status)
  "Run top-level turn terminal hook EVENT for FSM with STATUS."
  (when-let* ((info (gptel-fsm-info fsm))
              (chat-buffer (plist-get info :buffer))
              ((buffer-live-p chat-buffer)))
    (with-current-buffer chat-buffer
      (when (bound-and-true-p mevedel--session)
        (require 'mevedel-hooks)
        (let* ((workspace (mevedel-workspace))
               (request (and (boundp 'mevedel--current-request)
                             mevedel--current-request))
               (reason (and (eq event 'StopFailure)
                            (or (mevedel--fsm-error-message fsm)
                                (symbol-name status)))))
          (mevedel-hooks-run-event
           event
           (mevedel-hooks-event-plist
            event mevedel--session workspace
            :status (symbol-name status)
            :terminal-reason reason)
           #'ignore
           mevedel--session workspace request nil))))))

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
Each entry: (NAME . (:agents AGENT-LIST :tool-specs ORIGINAL-SPECS)).")

(defmacro mevedel-define-preset (name &rest keys)
  "Define a mevedel preset NAME with declarative KEYS.

KEYS is a plist with the following recognized keys:

  :description  STRING  -- preset description for selection UI
  :tools        LIST    -- tool specs for `mevedel-tool-resolve-gptel'
                           (bare symbols, (:group X), (:tool X), (:deferred X))
  :agents       LIST    -- list of agent name symbols for request-time setup
  :system       VALUE   -- system prompt (string, function, or dynamic spec)

Unlike `gptel-make-preset', this macro:
  - Has no :parents -- mevedel presets are flat by design; `:pre'/`:post'
    hooks assume no inheritance chain
  - Has no :send--handlers -- FSM handlers are injected at request time
  - Resolves :tools via `mevedel-tool-resolve-gptel' at definition time
  - Stores :agents in `mevedel-preset--registry' for request-time setup
  - Injects a `:post' hook that runs `mevedel-agents--setup-for-request'
    and `mevedel-preset--setup-deferred' so mevedel session wiring fires
    transparently on every preset application path (directives,
    `gptel-with-preset', @name expansion, transient menus)"
  (declare (indent 1))
  (let ((preset-sym (intern (concat "mevedel-" (symbol-name name)))))
    `(progn
       ;; Store mevedel-specific metadata
       (setf (alist-get ',preset-sym mevedel-preset--registry)
             (list :agents ',(plist-get keys :agents)
                   :tool-specs ',(plist-get keys :tools)))
       ;; Register with gptel
       (gptel-make-preset ',preset-sym
         :description ,(plist-get keys :description)
         :tools (let* ((resolved (mevedel-tool-resolve-gptel
                                  ',(plist-get keys :tools)))
                       (active (plist-get resolved :active)))
                  active)
         ,@(when (plist-get keys :system)
             (list :system (plist-get keys :system)))
         :post (lambda ()
                 (mevedel-agents--setup-for-request ',preset-sym)
                 (mevedel-preset--setup-deferred ',preset-sym)
                 (mevedel-preset--setup-extras ',preset-sym))))))

;;;###autoload
(defun mevedel--define-presets ()
  "Define gptel presets for mevedel actions."
  (require 'gptel)
  (require 'mevedel-tool-registry)

  ;; Read-only preset for discussion/analysis
  (mevedel-define-preset discuss
    :description "Read-only tools for code analysis and discussion"
    :tools (read util (:tool "Bash")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (explorer coordinator verifier)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--base-prompt)))

  ;; Full editing preset for implementation
  (mevedel-define-preset implement
    :description "Full editing capabilities with patch review workflow"
    :tools (read util edit (:tool "Bash")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (explorer coordinator verifier)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--base-prompt)))

  ;; Revision preset with previous patch context
  (mevedel-define-preset revise
    :description "Revise previous implementation with full context"
    :tools (read util edit (:tool "Bash")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (explorer coordinator verifier)
    :system "You are revising a previous implementation. The previous patch and its context are included in the conversation. Analyze what needs to be changed and create an improved implementation.")

  ;; Tutoring preset - guides through hints, never provides solutions
  (mevedel-define-preset tutor
    :description "Tutoring preset - guides through hints, never provides solutions"
    :tools (read util (:tool "Bash")
            (:tool "GetHints") (:tool "RecordHint")
            (:deferred (:tool "Eval"))
            (:deferred code)
            (:deferred web)
            (:deferred elisp))
    :agents (explorer coordinator verifier)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--tutor-base-prompt))))

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
              (meta (alist-get preset-name mevedel-preset--registry))
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
           (active-tools (plist-get resolved :active))
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
          (setf (mevedel-session-deferred-set session) existing)))
      ;; Silence unused-var warning when session is nil.
      (ignore active-tools))))


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

  1. Deferred tool injection (WAIT state handler)
  1a. Inbound agent-message delivery (WAIT state handler)
  2. Final patch generation (terminal state handler)
  3. Request callback invocation (terminal state handler)
  4. File snapshot and access request cleanup (terminal state handler)
  5. Session turn-count increment (terminal state handler)
  5a. Token baseline correction
  5b. Session autosave (DONE state handler only)
  5c. Turn terminal hooks
  5d. Temporary implementation permission mode restore
  6. Request cleanup
  7. BWAIT parking
  8. Terminal mailbox guard"
  ;; 1. Deferred tool injection: add to WAIT state
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons #'mevedel-tools--handle-deferred-inject
                    (cdr wait-entry)))))
  ;; 1a. Inbound message delivery: drain session mailbox into
  ;; the next request's messages.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons #'mevedel-tools--handle-message-inject
                    (cdr wait-entry)))))
  ;; 1aa. Prepared composer queue delivery: drain user prompts queued
  ;; during an active request into the next WAIT-cycle HTTP request.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons #'mevedel-view--handle-queued-user-message-inject
                    (cdr wait-entry)))))
  ;; 1b. spec: apply skill request-scoped overrides BEFORE the
  ;; gptel--handle-wait fires.  This handler runs every WAIT entry
  ;; (not just the first), so iteration 2+ pick up overrides
  ;; installed mid-turn by model-side Skill calls.  Prepended BEFORE
  ;; the begin handler so begin lands at the front of the chain
  ;; (creating the request and draining the stash); the
  ;; apply-overrides handler runs immediately after, reading the
  ;; freshly-drained slot.
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons #'mevedel-skills--apply-overrides-handler
                    (cdr wait-entry)))))
  ;; 1c. Begin the mevedel-request on the first WAIT entry.  WAIT is
  ;; re-entered after each tool call loop, so the guard on
  ;; `:mevedel-request-begun' keeps request-begin idempotent per FSM.
  ;; Materialize a fork-pending rewind preview before `request-begin'
  ;; runs -- this catches direct data-buffer send paths (gptel-send,
  ;; agent invocations) that bypass `mevedel-view-send'.  spec:
  ;; after `mevedel-request-begin' creates the request, drain any
  ;; buffer-local `mevedel-skills--pending-request-context' stash
  ;; into the new request's slots.
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
                              ;; spec: drain pending stash from
                              ;; slash-dispatched skill invocation.
                              (when (and (boundp 'mevedel--current-request)
                                         mevedel--current-request)
                                (mevedel-skills--drain-pending-context
                                 mevedel--current-request))
                              (when (fboundp
                                     'mevedel-view--ensure-request-progress-for-fsm)
                                (mevedel-view--ensure-request-progress-for-fsm
                                 fsm))))
                          (setf (gptel-fsm-info fsm)
                                (plist-put info :mevedel-request-begun t)))))
                    (cdr wait-entry)))))
  ;; 1d. Continuation auto-compaction: keep all WAIT injectors ahead
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
  ;; 4. Cleanup local vars
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer)))
             (with-current-buffer chat-buffer
               (setq mevedel--request-file-snapshots nil)
               (mevedel--clear-pending-access-requests))))
         handlers))
  ;; 5. Increment session turn count (drives reminder throttling)
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer))
                       ((buffer-live-p chat-buffer)))
             (with-current-buffer chat-buffer
               (when mevedel--session
                 (cl-incf (mevedel-session-turn-count mevedel--session))))))
         handlers))
  ;; 5a. Record API-reported token usage so pre-send compaction uses
  ;; a real baseline once gptel has one.  Compaction requests are
  ;; skipped inside the handler.
  (let ((tpre-entry (assq 'TPRE handlers)))
    (if tpre-entry
        (setcdr tpre-entry
                (append (cdr tpre-entry)
                        (list #'mevedel--compact-record-token-baseline)))
      (push (list 'TPRE #'mevedel--compact-record-token-baseline) handlers)))
  (setq handlers
        (mevedel--add-termination-handler
         #'mevedel--compact-record-token-baseline
         handlers))
  ;; 5b. Session autosave (completed-turn-boundary contract).  Only
  ;; fires when the FSM reached `DONE' so abort/error turns never land
  ;; on disk.  Runs after the turn-count bump (so `:total-turn-count'
  ;; is current) and before `mevedel-request-end' (so the request
  ;; struct's `:file-snapshots' hash is still reachable for file
  ;; history).  Skipped in read-only session mode so a restore that
  ;; opened under another host's lock cannot persist.
  (let ((done-entry (assq 'DONE handlers))
        (save-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer))
                       ((buffer-live-p chat-buffer)))
             (with-current-buffer chat-buffer
               (when (and mevedel--session
                          (bound-and-true-p mevedel-session-persistence)
                          (not (bound-and-true-p
                                mevedel-session--read-only-mode)))
                 (condition-case err
                     (progn
                       (mevedel-session-persistence-save
                        mevedel--session chat-buffer)
                       (when (bound-and-true-p mevedel-session--save-failed)
                         (setq mevedel-session--save-failed nil)
                         (force-mode-line-update)))
                   (error
                    (display-warning 'mevedel
                                     (format "Session auto-save failed: %s" err)
                                     :warning)
                    (setq-local mevedel-session--save-failed t)
                    (force-mode-line-update)))))))))
    (if done-entry
        (unless (member save-handler (cdr done-entry))
          (setcdr done-entry (append (cdr done-entry) (list save-handler))))
      (push (list 'DONE save-handler) handlers)))
  ;; 5c. Turn terminal hooks.  `Stop' is per successful top-level turn;
  ;; `StopFailure' is per aborted or errored turn.  Both run before
  ;; `mevedel-request-end' so request-scoped hook layers are still visible.
  (let ((done-entry (assq 'DONE handlers))
        (stop-handler
         (lambda (fsm)
           (mevedel--run-turn-terminal-hook fsm 'Stop 'completed))))
    (if done-entry
        (unless (member stop-handler (cdr done-entry))
          (setcdr done-entry (append (cdr done-entry) (list stop-handler))))
      (push (list 'DONE stop-handler) handlers)))
  (dolist (state '(ERRS ABRT))
    (let* ((entry (assq state handlers))
           (failure-status (if (eq state 'ABRT) 'aborted 'error))
           (failure-handler
            (lambda (fsm)
              (mevedel--run-turn-terminal-hook
               fsm 'StopFailure failure-status))))
      (if entry
          (setcdr entry (append (cdr entry) (list failure-handler)))
        (push (list state failure-handler) handlers))))
  ;; 5d. Restore temporary Plan implementation permission mode overrides.
  ;; Runs before request cleanup so any save/hook code has already seen
  ;; the implementation mode, but the session does not remain permissive
  ;; after the turn reaches a terminal state.
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer))
                       ((buffer-live-p chat-buffer)))
             (with-current-buffer chat-buffer
               (mevedel--implementation-permission-mode-restore))))
         handlers))
  ;; 6. End the mevedel-request (drains cancellers, clears buffer-local).
  ;; Placed last so earlier termination handlers still see the live
  ;; request if they need it.
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer))
                       ((buffer-live-p chat-buffer)))
             (with-current-buffer chat-buffer
               (mevedel-request-end))))
         handlers))
  ;; 6a. Queue drain.  Only successful top-level turns may submit a
  ;; queued follow-up.  The zero-delay timer lets the terminal
  ;; transition finish with `mevedel--current-request' already clear.
  (let ((done-entry (assq 'DONE handlers))
        (drain-handler #'mevedel-view--schedule-queued-user-message-drain))
    (if done-entry
        (unless (member drain-handler (cdr done-entry))
          (setcdr done-entry (append (cdr done-entry)
                                     (list drain-handler))))
      (push (list 'DONE drain-handler) handlers)))
  ;; 7. BWAIT handler: parks the FSM when background agents are running.
  (let ((bwait-entry (assq 'BWAIT handlers)))
    (if bwait-entry
        (setcdr bwait-entry
                (append (cdr bwait-entry)
                        (list #'mevedel-tools--handle-bwait)))
      (push (list 'BWAIT #'mevedel-tools--handle-bwait) handlers)))
  ;; 8. Terminal-state mailbox guard: log and clear orphaned messages
  ;; if the main session ends in DONE/ERRS/ABRT with queued results.
  (dolist (state '(DONE ERRS ABRT))
    (let ((entry (assq state handlers)))
      (if entry
          (setcdr entry
                  (append (cdr entry)
                          (list #'mevedel-tools--handle-terminal-mailbox)))
        (push (list state #'mevedel-tools--handle-terminal-mailbox) handlers))))
  (mevedel--wrap-terminal-handlers handlers))


;;
;;; BWAIT transition table injection

(defun mevedel-preset--inject-bwait-transitions (table)
  "Return TABLE with BWAIT parking state added.

Inserts a `mevedel-tools--background-agents-pending-p' predicate
before the `(t . DONE)' fallthrough in both the TYPE and TRET states.
When the predicate matches (background agents still running, no tool
calls), the FSM parks in BWAIT instead of terminating.

BWAIT is registered as a terminal-like state with no outgoing
transitions -- the background agent completion callback forces a
transition to WAIT explicitly.

Delegates to `mevedel-tools--bwait-injected-table' so the injected
copy is shared across every preset application."
  (mevedel-tools--bwait-injected-table table))


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

(defun mevedel--handler-name (handler)
  "Return a compact display name for FSM HANDLER."
  (cond
   ((symbolp handler) (symbol-name handler))
   ((byte-code-function-p handler) "#<byte-code>")
   ((functionp handler) "#<function>")
   (t (format "%S" handler))))

(defun mevedel--safe-fsm-handler (handler)
  "Return a wrapper that runs FSM HANDLER without aborting sibling handlers."
  (lambda (fsm)
    (condition-case err
        (funcall handler fsm)
      (error
       (display-warning
        'mevedel
        (format "FSM handler %s failed: %s"
                (mevedel--handler-name handler)
                (error-message-string err))
        :warning)
       nil))))

(defun mevedel--wrap-terminal-handlers (handlers &optional transitions)
  "Wrap terminal-state HANDLERS so one failure cannot skip cleanup."
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
