;;; mevedel-presets.el -- Presets used -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; `gptel'
(declare-function gptel-make-preset "ext:gptel" (name &rest keys))

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)
(declare-function gptel-tool-category "ext:gptel-request" (cl-x) t)
(defvar gptel-request--transitions)

;; `mevedel-chat'
(declare-function mevedel--generate-final-patch "mevedel-chat" (&optional workspace))
(declare-function mevedel--replace-patch-buffer "mevedel-chat" (patch-content))
(defvar mevedel--current-directive-uuid)

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))

;; `mevedel-tools'
(declare-function mevedel-tools--handle-deferred-inject "mevedel-tools" (fsm))
(declare-function mevedel-tools--setup-deferred-registry "mevedel-tools" (active-tool-paths))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-resolve-gptel "mevedel-tool-registry" (specs))

;; `mevedel-tool-ui'
(declare-function mevedel--clear-pending-access-requests "mevedel-tool-ui" (&rest _))

;; `mevedel-tool-fs'
(defvar mevedel--request-file-snapshots)

;; `mevedel-overlays'
(declare-function mevedel--find-directive-by-uuid "mevedel-overlays" (uuid))


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
  - Has no :parents -- all presets are flat
  - Has no :send--handlers -- FSM handlers are injected at request time
  - Resolves :tools via `mevedel-tool-resolve-gptel' at definition time
  - Stores :agents in `mevedel-preset--registry' for request-time setup"
  (declare (indent 1))
  (let ((preset-sym (intern (concat "mevedel-" (symbol-name name)))))
    `(progn
       ;; Store mevedel-specific metadata
       (setf (alist-get ',preset-sym mevedel-preset--registry)
             (list :agents ',( plist-get keys :agents)
                   :tool-specs ',(plist-get keys :tools)))
       ;; Register with gptel
       (gptel-make-preset ',preset-sym
         :description ,(plist-get keys :description)
         :tools (let* ((resolved (mevedel-tool-resolve-gptel
                                  ',(plist-get keys :tools)))
                       (active (plist-get resolved :active)))
                  active)
         ,@(when (plist-get keys :system)
             (list :system (plist-get keys :system)))))))

;;;###autoload
(defun mevedel--define-presets ()
  "Define gptel presets for mevedel actions."
  (require 'gptel)
  (require 'mevedel-tool-registry)

  ;; Read-only preset for discussion/analysis
  (mevedel-define-preset discuss
    :description "Read-only tools for code analysis and discussion"
    :tools (read util eval)
    :agents (codebase-analyst researcher planner introspector)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--base-prompt)))

  ;; Full editing preset for implementation
  (mevedel-define-preset implement
    :description "Full editing capabilities with patch review workflow"
    :tools (read util eval edit (:tool "CreatePlan"))
    :agents (codebase-analyst researcher planner introspector)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--base-prompt)))

  ;; Revision preset with previous patch context
  (mevedel-define-preset revise
    :description "Revise previous implementation with full context"
    :tools (read util eval edit (:tool "CreatePlan"))
    :agents (codebase-analyst researcher planner introspector)
    :system "You are revising a previous implementation. The previous patch and its context are included in the conversation. Analyze what needs to be changed and create an improved implementation.")

  ;; Tutoring preset - guides through hints, never provides solutions
  (mevedel-define-preset tutor
    :description "Tutoring preset - guides through hints, never provides solutions"
    :tools (read util eval (:tool "GetHints") (:tool "RecordHint"))
    :agents (codebase-analyst researcher planner introspector)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--tutor-base-prompt)))

  ;; Discussion preset with deferred code-tools and edit-tools
  (mevedel-define-preset discuss-deferred
    :description "Discussion preset with deferred tool loading via ToolSearch"
    :tools (read util eval (:deferred code) (:deferred edit))
    :agents (codebase-analyst researcher planner introspector)
    :system (lambda ()
              (mevedel-system-build-prompt mevedel-system--base-prompt))))

;;
;;; Request-time preset setup

(defun mevedel-preset--setup-deferred (preset-name)
  "Set up the deferred tool registry for PRESET-NAME.

Resolves the preset's tool specs and populates the deferred registry
with any (:deferred ...) entries.  Only has an effect if the preset
has deferred tool specs."
  (when-let* ((meta (alist-get preset-name mevedel-preset--registry))
              (tool-specs (plist-get meta :tool-specs)))
    (let* ((resolved (mevedel-tool-resolve-gptel tool-specs))
           (active (plist-get resolved :active)))
      (when (plist-get resolved :deferred)
        (mevedel-tools--setup-deferred-registry
         (mapcar (lambda (tool)
                   (list (gptel-tool-category tool) (gptel-tool-name tool)))
                 active))))))


;;
;;; FSM handler chain builder

(defun mevedel-preset--build-handlers (handlers)
  "Build the standard mevedel FSM handler chain from base HANDLERS.

HANDLERS is an alist like `gptel-send--handlers'.  Returns a new
alist with mevedel-specific handlers added:

  1. Deferred tool injection (WAIT state handler)
  2. Final patch generation (terminal state handler)
  3. Request callback invocation (terminal state handler)
  4. File snapshot and access request cleanup (terminal state handler)"
  ;; 1. Deferred tool injection: add to WAIT state
  (let ((wait-entry (assq 'WAIT handlers)))
    (when wait-entry
      (setcdr wait-entry
              (cons #'mevedel-tools--handle-deferred-inject
                    (cdr wait-entry)))))
  ;; 2. Generate final patch and store in directive
  (setq handlers
        (mevedel--add-termination-handler
         (lambda (fsm)
           (when-let* ((info (gptel-fsm-info fsm))
                       (chat-buffer (plist-get info :buffer)))
             (let* ((workspace (with-current-buffer chat-buffer
                                 (mevedel-workspace)))
                    (directive-uuid (with-current-buffer chat-buffer
                                     mevedel--current-directive-uuid))
                    (final-patch (with-current-buffer chat-buffer
                                   (mevedel--generate-final-patch workspace))))
               (when (and final-patch (> (length final-patch) 0))
                 (when-let* ((directive (mevedel--find-directive-by-uuid
                                         directive-uuid)))
                   (overlay-put directive 'mevedel-directive-patch final-patch))
                 (mevedel--replace-patch-buffer final-patch)))))
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

(provide 'mevedel-presets)

;;; mevedel-presets.el ends here
