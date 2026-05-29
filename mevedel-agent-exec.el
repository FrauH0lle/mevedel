;;; mevedel-agent-exec.el -- Sub-agent task runner -*- lexical-binding: t -*-

;;; Commentary:

;; Mevedel-owned sub-agent runtime.  Extracted from `gptel-agent-tools'
;; so mevedel controls the surface where private coupling used to
;; concentrate: the task dispatch function, the sub-agent FSM handler
;; table, and the agent registry.
;;
;; The extraction fixes a streaming-truncation bug present in the
;; upstream `gptel-agent--task': that function's `(pred stringp)'
;; branch fires its main callback on every streamed chunk and has no
;; `t' branch, so gptel's stream-complete signal (`gptel-request.el'
;; line 2864) is silently dropped.  Since gptel's tool-call commit
;; path locks in the first delivered value, the parent agent only
;; ever sees the first chunk of a sub-agent's final response.  The
;; runner here accumulates on string chunks and fires exactly once on
;; `t', so the full response reaches the parent.
;;
;; See specs/archive/18-sub-agent-runtime.md for the full extraction
;; rationale, scope, and regression-test plan.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'gptel)
  ;; Required for the cl-defstruct `setf' expanders of `gptel-fsm-*' slots.
  ;; Without this, `(setf (gptel-fsm-handlers ...) ...)' below does not expand
  ;; to its slot-setter at compile time and falls back to looking up a
  ;; nonexistent `(setf gptel-fsm-handlers)' function at runtime.
  (require 'gptel-request)
  ;; Required for the cl-defstruct `setf' expanders on
  ;; `mevedel-agent-invocation-*' slots referenced below.
  (require 'mevedel-agents))

(require 'mevedel-models)

;; `gptel-request'
(declare-function gptel-request "ext:gptel-request"
                  (&optional prompt &rest args))
(declare-function gptel-fsm-info "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-state "ext:gptel-request" (cl-x) t)
(declare-function gptel-fsm-handlers "ext:gptel-request" (cl-x) t)
(declare-function gptel-make-fsm "ext:gptel-request" (&rest args))
(declare-function gptel--fsm-transition "ext:gptel-request"
                  (machine &optional new-state))
(declare-function gptel--transform-add-context "ext:gptel-request" (fsm))
(declare-function gptel--display-tool-calls "ext:gptel-request" (calls info))
(defvar gptel-send--transitions)

;; `gptel'
(declare-function gptel-with-preset "ext:gptel" (name &rest body))
(declare-function gptel-get-preset "ext:gptel" (name))
(declare-function gptel--update-status "ext:gptel"
                  (msg &optional face))
(defvar gptel--fsm-last)
(defvar gptel-agent-preset)
(defvar gptel-stream)
(defvar gptel-tools)
(defvar gptel-use-tools)
(defvar gptel-use-context)
(defvar gptel-context)
(defvar gptel-backend)
(defvar gptel-temperature)
(defvar gptel-max-tokens)
(defvar gptel-cache)
(defvar gptel--request-params)
(defvar gptel-use-curl)
(defvar gptel-include-reasoning)
(defvar gptel-org-convert-response)
(defvar gptel-org-branching-context nil)
(defvar gptel-org-ignore-elements)
(defvar gptel--system-message)

;; `mevedel-compact'
(declare-function mevedel--compact-record-token-baseline
                  "mevedel-compact" (fsm))

;; `org-element'
(declare-function org-element-cache-reset "ext:org-element"
                  (&optional all no-persistence))
(defvar org-element-use-cache)
(defvar org-element-cache-persistent)

;; `mevedel-tool-ui' -- static cycle (tool-ui requires this module at
;; compile time for symbols declared below; we declare tool-ui helpers
;; here for the runtime call sites.  Both modules are fully loaded by
;; the time `mevedel-agent-exec--run' is reachable.)
(declare-function mevedel-tools--augment-agent-handlers "mevedel-tool-ui"
                  (handlers &rest rest))
(declare-function mevedel-tools--handle-message-inject "mevedel-tool-ui" (fsm))
(declare-function mevedel-tools--handle-wait-inject "mevedel-tool-ui" (fsm))
(declare-function mevedel-tools--inject-bwait-transition "mevedel-tool-ui"
                  (fsm))
(declare-function mevedel-tools--complete-background-agent "mevedel-tool-ui"
                  (invocation response))
(defvar mevedel-tools-task-debug)

;; `mevedel-tools' -- polymorphic ctx accessors (session/invocation)
(declare-function mevedel-tools--ctx-background-agents "mevedel-tools" (ctx))
(declare-function mevedel-tools--ctx-messages "mevedel-tools" (ctx))

;; `mevedel-agents' -- invocation struct
(declare-function mevedel-agent-to-gptel-spec "mevedel-agents" (agent))
(declare-function mevedel-agent-invocation-p "mevedel-agents" (cl-x))
(declare-function mevedel-agent-invocation-agent "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-name "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-buffer "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-data-buffer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-parent-session
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-agent-id
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-relative-path
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-status
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-sidecar-dirty
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-call-count
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-started-at
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-terminal-reason
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-verdict
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-description
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-activity
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-transcript-save-timer
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-start-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-render-data-end-marker
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-background-p
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-model-tier-override
                  "mevedel-agents" (cl-x) t)
(declare-function mevedel-agent-invocation-skill-model-override
                  "mevedel-agents" (cl-x) t)

;; `mevedel-models'
(declare-function mevedel-model-agent-default-selector
                  "mevedel-models" (agent-type))
(declare-function mevedel-model-resolve-selector
                  "mevedel-models" (selector &optional noerror))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--find-render-data-block-by-agent-id
                  "mevedel-pipeline" (agent-id))
(declare-function mevedel-pipeline--patch-render-data-block
                  "mevedel-pipeline" (beg end new-plist))
(declare-function mevedel-pipeline-extract-render-data
                  "mevedel-pipeline" (result-string))

;; `mevedel-view'
(declare-function mevedel-view-rerender "mevedel-view"
                  (&optional buffer))
(declare-function mevedel-view-agent-live-transcript-finalize
                  "mevedel-view" (invocation))

(defcustom mevedel-agent-transcript-save-debounce 0.5
  "Idle seconds to wait before saving a running sub-agent transcript.
Tool-result transitions schedule a save through this debounce so
busy agent tool loops do not synchronously run save hooks on every
tool boundary.  Terminal states still flush immediately."
  :type 'number
  :group 'mevedel)

(defconst mevedel-agent-exec--live-activity-limit 5
  "Maximum recent activity items mirrored into live agent metadata.")

(defvar mevedel-agent-exec--suppress-activity-rerender nil
  "Non-nil means activity recording should not schedule a view rerender.")

;; `mevedel-session-persistence'
(declare-function mevedel-session-persistence--update-transcript-entry
                  "mevedel-session-persistence" (session agent-id updates))
(declare-function mevedel-session-persistence--write-sidecar-now
                  "mevedel-session-persistence" (session buffer))
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel-session-persistence)

;; `mevedel-hooks'
(declare-function mevedel-hooks-run-event "mevedel-hooks"
                  (event event-plist callback
                         &optional session workspace request invocation))
(declare-function mevedel-hooks-event-plist "mevedel-hooks"
                  (event &optional session workspace &rest extra))
(declare-function mevedel-hooks-additional-context-string "mevedel-hooks"
                  (decision))
(defvar mevedel-hooks-command-timeout)
(defvar mevedel-hooks-command-timeout-max)

;; gptel handlers
(declare-function gptel--handle-post-insert "ext:gptel" (fsm))
(declare-function gptel--handle-abort "ext:gptel" (fsm))
(declare-function gptel--handle-error "ext:gptel" (fsm))
(declare-function gptel--handle-token-usage "ext:gptel" (fsm))
(declare-function gptel-abort "ext:gptel" (&optional buf))
(declare-function gptel-get-tool "ext:gptel-request" (path))
(declare-function gptel-tool-p "ext:gptel-request" (cl-x))
(declare-function gptel-tool-name "ext:gptel-request" (cl-x) t)

;; Live agent FSM registry, defined buffer-local in `mevedel-tool-ui'.
(defvar mevedel-tools--agents-fsm)

;; `gptel' core (stable)
(declare-function gptel--handle-wait "ext:gptel-request" (fsm))
(declare-function gptel--handle-pre-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-use "ext:gptel-request" (fsm))
(declare-function gptel--handle-post-tool "ext:gptel-request" (fsm))
(declare-function gptel--handle-tool-result "ext:gptel-request" (fsm))
(defvar gptel-model)
(defvar gptel--tool-preview-alist)

;; `gptel-agent-tools' -- still external for the helpers we have not
;; internalized yet (confirm-overlay used by the preview renderer).
(declare-function gptel-agent--confirm-overlay "ext:gptel-agent-tools"
                  (from to &optional no-hide))


;;
;;; Agent registry

(defvar-local mevedel-agent-exec--agents nil
  "Buffer-local list of sub-agent specs available in this chat buffer.

Each element is a cons `(NAME . PLIST)' where PLIST is a gptel preset
keyword list describing the sub-agent (tools, prompt, max-turns, ...).
The list is populated by `mevedel-agents--setup-for-request' from
`mevedel-agent--registry' at preset-install time and consumed by
`mevedel-agent-exec--run' when dispatching sub-agent tasks.")


;;
;;; Agent buffer
;;
;; Each sub-agent invocation runs in its own gptel buffer; that
;; buffer is the on-disk transcript when persistence is enabled.
;; The buffer carries parent-context bindings so tools dispatched
;; from inside the sub-agent see the parent session/workspace.

(defvar-local mevedel--agent-invocation nil
  "When non-nil, identifies the buffer as a sub-agent's transcript buffer.

Bound buffer-locally to the `mevedel-agent-invocation' struct that
owns this buffer.  Read by `mevedel--active-chat-buffer' and
`mevedel--workspace-sessions' (in `mevedel-chat.el') so an agent
buffer is not mistaken for a top-level chat buffer when discovering
the parent context.")

(defun mevedel-agent-exec--allocate-agent-buffer (invocation parent-data-buffer)
  "Create and configure the agent buffer for INVOCATION.

PARENT-DATA-BUFFER is the parent chat buffer.  Reads parent's
`mevedel--session' and `mevedel--workspace' and installs them
buffer-locally on the new agent buffer **by reference**, not by
copy.  This is the load-bearing contract for permission
propagation: tools dispatched from the agent buffer reach the
pipeline, which captures `mevedel--session' as the parent's
session struct, and any rule that the user accepts inside a
sub-agent prompt is written via `setf' on that same struct -- so
\"allow-session\" inside a sub-agent immediately applies to the
main agent and to every other live sub-agent sharing the same
session.  Likewise, `mevedel-permission-mode' toggles on the
parent are observable at the next sub-agent pipeline entry.

The buffer name uses the placeholder format
`*mevedel-agent-<short-id>*' until `set-visited-file-name' adopts the
transcript path; `generate-new-buffer' disambiguates same-second
collisions automatically with a `<2>' suffix.

Installs a buffer-local `kill-buffer-hook' that calls `gptel-abort'
on the buffer when the user kills it mid-stream.  Finalization is
idempotent and the dead-buffer save path is a no-op, so this drives
the FSM through ABRT cleanly.

Returns the agent buffer.  Caller is responsible for inserting the
initial task prompt and (optionally) calling `set-visited-file-name'."
  (let* ((agent-id (mevedel-agent-invocation-agent-id invocation))
         (short-id (or (and (stringp agent-id)
                            (let* ((suffix
                                    (cadr (split-string agent-id "--" t))))
                              (and (stringp suffix)
                                   (substring suffix 0
                                              (min 8 (length suffix))))))
                       "anon"))
         (buf (generate-new-buffer
               (format "*mevedel-agent-%s*" short-id)))
         (parent-session
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--session parent-data-buffer)))
         (parent-workspace
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--workspace parent-data-buffer)))
         (parent-view-buffer
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel--view-buffer parent-data-buffer)))
         (parent-agent-specs
          (and (buffer-live-p parent-data-buffer)
               (buffer-local-value 'mevedel-agent-exec--agents
                                   parent-data-buffer))))
    (with-current-buffer buf
      (let ((org-element-use-cache nil)
            (org-element-cache-persistent nil))
        (org-mode))
      (when (fboundp 'org-element-cache-reset)
        (let ((org-element-use-cache t))
          (ignore-errors
            (org-element-cache-reset nil 'no-persistence))))
      (setq-local org-element-use-cache nil)
      (setq-local org-element-cache-persistent nil)
      (setq-local gptel-org-convert-response nil)
      (setq-local gptel-org-branching-context nil)
      (setq-local gptel-org-ignore-elements '(property-drawer))
      ;; Activate gptel-mode so org property persistence and bounds
      ;; round-trip work.  If activation fails (rare; unusual configs),
      ;; abandon the buffer and signal the caller via a thrown
      ;; `mevedel-agent-buffer-setup-failed' tag so dispatch can fall
      ;; through to the legacy prompt-only path.
      (unless (require 'gptel nil t)
        (kill-buffer buf)
        (throw 'mevedel-agent-buffer-setup-failed 'no-gptel))
      (condition-case err
          (gptel-mode +1)
        (error
         (message "mevedel: gptel-mode activation failed: %S; \
falling back to legacy prompt-only path" err)
         (kill-buffer buf)
         (throw 'mevedel-agent-buffer-setup-failed err)))
      (when parent-session
        (setq-local mevedel--session parent-session))
      (when parent-workspace
        (setq-local mevedel--workspace parent-workspace))
      ;; also expose the parent's view buffer so tools that
      ;; target `mevedel--view-buffer' (permission prompts, async
      ;; questionnaires, RequestAccess, ...) surface in the parent
      ;; UI instead of inside the hidden agent buffer.
      (when (and parent-view-buffer (buffer-live-p parent-view-buffer))
        (setq-local mevedel--view-buffer parent-view-buffer))
      ;; Propagate the parent's agent registry so a sub-agent that
      ;; itself dispatches further sub-agents (e.g. coordinator
      ;; spawning explorer / verifier workers) can resolve their
      ;; specs.  Without this, `(assoc agent-type
      ;; mevedel-agent-exec--agents)' inside the sub-agent buffer
      ;; reads nil, the worker's `:tools' / `:system' preset slots
      ;; never apply, and the worker fires with default (empty)
      ;; tooling.
      (when parent-agent-specs
        (setq-local mevedel-agent-exec--agents parent-agent-specs))
      (setq-local mevedel--agent-invocation invocation)
      ;; bump the invocation's call-count on each
      ;; tool dispatch so the parent's running-handle badge reads
      ;; [running · N calls] rather than the zero-suppressed
      ;; [running].  The hook runs in the sub-agent's buffer; the
      ;; invocation reference is closed over so termination /
      ;; buffer-kill don't leave a dangling pointer.
      ;; The companion --handle-update hook patches the parent's
      ;; render-data block so the visible badge advances live.
      (let ((inv invocation))
        ;; The hook return value gets inspected by gptel for control
        ;; plist keys (`:stop', `:confirm', etc).  Wrap the body in
        ;; `prog1 nil' so the hook always returns nil; otherwise
        ;; `mevedel-view-rerender''s idle-timer return value (a
        ;; vector) would land in gptel's `plist-member' call and
        ;; signal `wrong-type-argument plistp ...'.
        (add-hook 'gptel-pre-tool-call-functions
                  (lambda (&rest args)
                    (prog1 nil
                      (when (mevedel-agent-invocation-p inv)
                        (cl-incf (mevedel-agent-invocation-call-count inv))
                        (let ((tool-name
                               (or (mevedel-agent-exec--activity-tool-name args)
                                   "Tool")))
                          (let ((mevedel-agent-exec--suppress-activity-rerender
                                 t))
                            (mevedel-agent-exec--record-activity
                             inv
                             (list :type 'tool-start
                                   :tool-name tool-name
                                   :summary (format "%s(...)" tool-name)))))
                        (mevedel-agent-exec--handle-update inv))))
                  nil t)
        (add-hook 'gptel-post-tool-call-functions
                  (lambda (&rest args)
                    (prog1 nil
                      (when (mevedel-agent-invocation-p inv)
                        (let ((tool-name
                               (or (mevedel-agent-exec--activity-tool-name args)
                                   "Tool")))
                          (let ((mevedel-agent-exec--suppress-activity-rerender
                                 t))
                            (mevedel-agent-exec--record-activity
                             inv
                             (if (mevedel-agent-exec--activity-error-p args)
                                 (list :type 'tool-error
                                       :tool-name tool-name
                                       :error (format "%s failed" tool-name))
                               (list :type 'tool-finish
                                     :tool-name tool-name
                                     :summary (format "%s done" tool-name))))))
                        (mevedel-agent-exec--handle-update inv))))
                  nil t)
        (add-hook 'gptel-post-response-functions
                  (lambda (&rest _)
                    (prog1 nil
                      (when (mevedel-agent-invocation-p inv)
                        (mevedel-agent-exec--handle-update inv))))
                  nil t))
      (add-hook 'kill-buffer-hook
                #'mevedel-agent-exec--on-buffer-kill nil t))
    buf))

(defun mevedel-agent-exec--cache-render-data-bounds (invocation beg end)
  "Cache render-data block bounds BEG and END on INVOCATION.
Markers track future in-place replacements of the same hidden block:
the start marker stays before inserted text and the end marker advances
after inserted text."
  (when (mevedel-agent-invocation-p invocation)
    (let ((start (copy-marker beg nil))
          (finish (copy-marker end t)))
      (setf (mevedel-agent-invocation-render-data-start-marker invocation)
            start)
      (setf (mevedel-agent-invocation-render-data-end-marker invocation)
            finish)
      (cons start finish))))

(defun mevedel-agent-exec--cached-render-data-bounds
    (invocation parent-buf agent-id)
  "Return cached render-data bounds for INVOCATION and AGENT-ID.
The cached marker range is accepted only when it still contains a
render-data block for AGENT-ID in PARENT-BUF.  Markers can remain
position-valid after a transcript rewrite replaces the original block;
validating the payload prevents live updates from patching unrelated
text."
  (when (mevedel-agent-invocation-p invocation)
    (let ((beg (mevedel-agent-invocation-render-data-start-marker invocation))
          (end (mevedel-agent-invocation-render-data-end-marker invocation)))
      (when (and (markerp beg)
                 (markerp end)
                 (eq (marker-buffer beg) parent-buf)
                 (eq (marker-buffer end) parent-buf)
                 (marker-position beg)
                 (marker-position end)
                 (< (marker-position beg) (marker-position end)))
        (let* ((raw (buffer-substring-no-properties beg end))
               (parsed (mevedel-pipeline-extract-render-data raw))
               (visible (car parsed))
               (plist (cdr parsed)))
          (if (and (stringp visible)
                   (string-empty-p (string-trim visible))
                   (listp plist)
                   (equal (plist-get plist :agent-id) agent-id))
              (cons beg end)
            (setf (mevedel-agent-invocation-render-data-start-marker
                   invocation)
                  nil)
            (setf (mevedel-agent-invocation-render-data-end-marker
                   invocation)
                  nil)
            nil))))))

(defun mevedel-agent-exec--render-data-bounds (invocation agent-id)
  "Return render-data block bounds for INVOCATION and AGENT-ID.
Uses cached markers when possible and falls back to scanning the current
buffer when the cache is missing or stale."
  (or (mevedel-agent-exec--cached-render-data-bounds
       invocation (current-buffer) agent-id)
      (when-let* ((bounds
                   (mevedel-pipeline--find-render-data-block-by-agent-id
                    agent-id)))
        (mevedel-agent-exec--cache-render-data-bounds
         invocation (car bounds) (cdr bounds)))))

(defun mevedel-agent-exec--handle-update (invocation)
  "Patch the parent's render-data block for INVOCATION to reflect live state.
Runs from sub-agent FSM hooks (`gptel-pre-tool-call-functions',
`gptel-post-tool-call-functions') alongside the call-count bump.

Locates the parent's `<!-- mevedel-render-data -->' block via cached
markers or `mevedel-pipeline--find-render-data-block-by-agent-id', merges
the current `:status' / `:calls' / `:elapsed' / `:reason' values
from INVOCATION onto the existing plist, writes the block back
in place via `mevedel-pipeline--patch-render-data-block', and
schedules a parent-view re-render via `mevedel-view-rerender'.

Failure modes:
- Parent buffer dead: silent no-op, one-shot warning.
- Parent buffer narrowed: save-restriction + widen guard
  (provided by the locator/patcher).
- Render-data block not found: silent no-op (segment may have
  compacted away; sidecar status is authoritative).
- Patch error: warn and continue."
  (let* ((parent-buf (mevedel-agent-invocation-parent-data-buffer invocation))
         (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (mevedel-agent-exec--sync-transcript-entry invocation)
    (when (and (bufferp parent-buf) (buffer-live-p parent-buf) agent-id)
      (condition-case err
          (with-current-buffer parent-buf
            (when-let ((bounds
                        (mevedel-agent-exec--render-data-bounds
                         invocation agent-id)))
              (let* ((beg (car bounds))
                     (end (cdr bounds))
                     (raw (buffer-substring-no-properties beg end))
                     (parsed (mevedel-pipeline-extract-render-data raw))
                     (existing (cdr parsed))
                     (status (mevedel-agent-invocation-transcript-status
                              invocation))
                     (calls (mevedel-agent-invocation-call-count invocation))
                     (started (mevedel-agent-invocation-started-at invocation))
                     (elapsed (and started
                                   (float-time
                                    (time-subtract (current-time) started))))
                     (reason (mevedel-agent-invocation-terminal-reason
                              invocation))
                     (verdict (mevedel-agent-invocation-verdict invocation))
                     (activity
                      (mevedel-agent-exec--background-activity-snapshot
                       invocation))
                     (updated (copy-sequence existing)))
                (when (listp existing)
                  (setq updated
                        (plist-put updated :status (or status 'running)))
                  (setq updated
                        (plist-put updated :calls (or calls 0)))
                  (when elapsed
                    (setq updated (plist-put updated :elapsed elapsed)))
                  (when reason
                    (setq updated (plist-put updated :reason reason)))
                  (when verdict
                    (setq updated (plist-put updated :verdict verdict)))
                  (when activity
                    (setq updated (plist-put updated :activity activity)))
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (mevedel-pipeline--patch-render-data-block
                     beg end updated)))))
            ;; Schedule a parent-view re-render so the visible
            ;; badge picks up the patched render-data.
            (when-let* ((view-buf (and (boundp 'mevedel--view-buffer)
                                       mevedel--view-buffer)))
              (when (buffer-live-p view-buf)
                (mevedel-view-rerender view-buf))))
        (error
         (display-warning
          'mevedel
          (format "handle-update for %s failed: %S" agent-id err)
          :warning))))))

(defun mevedel-agent-exec--activity-tool-name (args)
  "Best-effort extraction of a tool name from hook ARGS."
  (catch 'name
    (dolist (arg args)
      (cond
       ((and (listp arg) (plist-get arg :name))
        (throw 'name (format "%s" (plist-get arg :name))))
       ((and (listp arg) (plist-get arg :tool-name))
        (throw 'name (format "%s" (plist-get arg :tool-name))))
       ((and (fboundp 'gptel-tool-name)
             (ignore-errors (gptel-tool-name arg)))
        (throw 'name (format "%s" (gptel-tool-name arg))))))))

(defun mevedel-agent-exec--activity-error-p (args)
  "Return non-nil if hook ARGS look like a tool error."
  (cl-some
   (lambda (arg)
     (and (stringp arg)
          (string-prefix-p "Error:" arg)))
   args))

(defun mevedel-agent-exec--activity-clean-string (value)
  "Return VALUE as a one-line activity string, or nil."
  (when value
    (string-trim
     (replace-regexp-in-string "[\n\r\t ]+" " " (format "%s" value)))))

(defun mevedel-agent-exec--activity-sanitize-item (item)
  "Return sanitized activity ITEM, or nil when ITEM is not recordable."
  (when (and (listp item)
             (memq (plist-get item :type)
                   '(tool-start tool-finish tool-error waiting message status)))
    (let ((copy (copy-sequence item)))
      (dolist (key '(:tool-name :summary :from :status :error))
        (when (plist-member copy key)
          (setq copy
                (plist-put copy key
                           (mevedel-agent-exec--activity-clean-string
                            (plist-get copy key))))))
      copy)))

(defun mevedel-agent-exec--sync-transcript-entry (invocation)
  "Sync live INVOCATION metadata into its session transcript entry."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (session (mevedel-agent-invocation-parent-session invocation))
              (agent-id (mevedel-agent-invocation-agent-id invocation)))
    (let* ((started (mevedel-agent-invocation-started-at invocation))
           (elapsed (and started
                         (float-time
                          (time-subtract (current-time) started))))
           (reason (mevedel-agent-invocation-terminal-reason invocation))
           (verdict (mevedel-agent-invocation-verdict invocation))
           (activity
            (mevedel-agent-exec--background-activity-snapshot invocation))
           (updates (list :status
                          (or (mevedel-agent-invocation-transcript-status
                               invocation)
                              'running)
                          :calls
                          (or (mevedel-agent-invocation-call-count
                               invocation)
                              0)
                          :updated-at
                          (format-time-string "%FT%H-%M-%S"))))
      (when elapsed
        (setq updates (plist-put updates :elapsed elapsed)))
      (when reason
        (setq updates (plist-put updates :reason reason)))
      (when verdict
        (setq updates (plist-put updates :verdict verdict)))
      (when activity
        (setq updates (plist-put updates :activity activity)))
      (mevedel-session-persistence--update-transcript-entry
       session agent-id updates))))

(defun mevedel-agent-exec--record-activity (invocation item &optional _reserved)
  "Append ephemeral activity ITEM to INVOCATION.
Schedules a parent view rerender unless
`mevedel-agent-exec--suppress-activity-rerender' is non-nil."
  (when (and (mevedel-agent-invocation-p invocation)
             (buffer-live-p (mevedel-agent-invocation-parent-data-buffer
                             invocation)))
    (when-let* ((clean (mevedel-agent-exec--activity-sanitize-item item)))
      (let* ((item (plist-put clean :time (float-time)))
             (items (append (mevedel-agent-invocation-activity invocation)
                            (list item))))
        (setf (mevedel-agent-invocation-activity invocation) items)
        (mevedel-agent-exec--sync-transcript-entry invocation)
        (unless mevedel-agent-exec--suppress-activity-rerender
          (when-let* ((parent-buf
                       (mevedel-agent-invocation-parent-data-buffer invocation))
                      ((buffer-live-p parent-buf))
                      (view-buf (buffer-local-value 'mevedel--view-buffer
                                                    parent-buf))
                      ((buffer-live-p view-buf)))
            (mevedel-view-rerender view-buf)))))))

(defun mevedel-agent-exec--activity-snapshot (invocation &optional limit)
  "Return INVOCATION's activity list, excluding transient status items.
When LIMIT is non-nil, return only the last LIMIT entries."
  (when (mevedel-agent-invocation-p invocation)
    (let* ((items
            (cl-remove-if
             (lambda (item) (eq (plist-get item :type) 'status))
             (mevedel-agent-invocation-activity invocation)))
           (items (if (and limit (> (length items) limit))
                      (last items limit)
                    items)))
      (copy-tree items))))

(defun mevedel-agent-exec--live-activity-snapshot (invocation)
  "Return bounded recent activity for live agent handle metadata."
  (mevedel-agent-exec--activity-snapshot
   invocation mevedel-agent-exec--live-activity-limit))

(defun mevedel-agent-exec--final-activity-snapshot (invocation)
  "Return full activity for completed background rendering."
  (mevedel-agent-exec--activity-snapshot invocation))

(defun mevedel-agent-exec--background-activity-snapshot (invocation)
  "Return activity metadata for INVOCATION when it is a background agent.
Running agents publish only a bounded live preview; terminal agents
publish the full non-status activity history for persistence and
completed render-data."
  (when (and (mevedel-agent-invocation-p invocation)
             (mevedel-agent-invocation-background-p invocation))
    (if (memq (mevedel-agent-invocation-transcript-status invocation)
              '(completed error aborted))
        (mevedel-agent-exec--final-activity-snapshot invocation)
      (mevedel-agent-exec--live-activity-snapshot invocation))))

(defun mevedel-agent-exec--gptel-response-prop-p (prop)
  "Return non-nil when PROP denotes a gptel assistant response."
  (or (eq prop 'response)
      (let ((tail prop)
            found)
        (while (consp tail)
          (when (eq (car tail) 'response)
            (setq found t
                  tail nil))
          (when (consp tail)
            (setq tail (cdr tail))))
        found)))

(defun mevedel-agent-exec--final-response-text (invocation)
  "Return INVOCATION's final assistant response from its transcript.

The callback accumulator is a transport fallback and can contain
sub-agent transcript/tool material on some backends.  The transcript
buffer carries gptel text properties, so the last `response' run is
the user-facing answer to deliver to the parent."
  (when-let* (((mevedel-agent-invocation-p invocation))
              (buf (mevedel-agent-invocation-buffer invocation))
              ((buffer-live-p buf)))
    (with-current-buffer buf
      (let ((pos (point-min))
            last-start last-end)
        (while (< pos (point-max))
          (let* ((prop (get-text-property pos 'gptel))
                 (next (or (next-single-property-change
                            pos 'gptel nil (point-max))
                           (point-max))))
            (when (mevedel-agent-exec--gptel-response-prop-p prop)
              (setq last-start pos
                    last-end next))
            (setq pos next)))
        (when (and last-start last-end)
          (let ((text (string-trim
                       (buffer-substring-no-properties
                        last-start last-end))))
            (unless (string-empty-p text)
              text)))))))

(defun mevedel-agent-exec--on-buffer-kill ()
  "Buffer-local kill-buffer-hook for agent buffers.

When the user kills a live agent buffer mid-stream, drive its FSM
through ABRT.  `gptel--request-alist' entries have the shape
\(PROC FSM . CLEANUP-FN), so the FSM is `(cadr entry)'.  The
in-flight check skips the call when no live request matches the
dying buffer, otherwise `gptel-abort' would print a spurious
\"Stopped gptel request\" during routine finalization-driven kills."
  (when (and (boundp 'mevedel--agent-invocation)
             mevedel--agent-invocation
             (mevedel-agent-invocation-p mevedel--agent-invocation))
    (mevedel-agent-exec--flush-transcript-save mevedel--agent-invocation))
  (when (and (boundp 'mevedel--agent-invocation)
             mevedel--agent-invocation
             (fboundp 'gptel-abort)
             (boundp 'gptel--request-alist)
             gptel--request-alist
             (cl-some
              (lambda (entry)
                (let* ((fsm (cadr entry))
                       (info (and fsm (gptel-fsm-info fsm))))
                  (eq (and info (plist-get info :buffer))
                      (current-buffer))))
              gptel--request-alist))
    (condition-case _
        (gptel-abort (current-buffer))
      (error nil))))

(defun mevedel-agent-exec--cancel-transcript-save (invocation)
  "Cancel INVOCATION's pending debounced transcript save."
  (when (mevedel-agent-invocation-p invocation)
    (let ((timer (mevedel-agent-invocation-transcript-save-timer
                  invocation)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (mevedel-agent-invocation-transcript-save-timer invocation)
            nil))))

(defun mevedel-agent-exec--flush-transcript-save (invocation)
  "Cancel pending debounce and save INVOCATION's transcript now."
  (mevedel-agent-exec--cancel-transcript-save invocation)
  (mevedel-agent-exec--save-transcript-buffer invocation))

(defun mevedel-agent-exec--schedule-transcript-save (invocation)
  "Schedule a debounced save for INVOCATION's transcript."
  (if (and (numberp mevedel-agent-transcript-save-debounce)
           (> mevedel-agent-transcript-save-debounce 0))
      (when (mevedel-agent-invocation-p invocation)
        (mevedel-agent-exec--cancel-transcript-save invocation)
        (setf
         (mevedel-agent-invocation-transcript-save-timer invocation)
         (run-with-idle-timer
          mevedel-agent-transcript-save-debounce nil
          (lambda (inv)
            (when (mevedel-agent-invocation-p inv)
              (setf (mevedel-agent-invocation-transcript-save-timer inv)
                    nil)
              (mevedel-agent-exec--save-transcript-buffer inv)))
          invocation)))
    (mevedel-agent-exec--save-transcript-buffer invocation)))

(defun mevedel-agent-exec--save-transcript-buffer (invocation)
  "Save INVOCATION's agent buffer to its transcript file (best-effort).

Returns nil and skips when the buffer is dead, has no
`buffer-file-name', or when its `buffer-file-name' no longer
matches the recorded transcript path (e.g. user renamed via
`set-visited-file-name').  Otherwise calls `basic-save-buffer'
silently so no interactive prompt can fire from inside an FSM handler
and routine transcript autosaves do not flood `*Messages*';
`before-save-hook' still drives `gptel-org--save-state' for
`GPTEL_BOUNDS' round-tripping.

Updates `:updated-at' on the in-memory transcript entry; retries
the sidecar write when `sidecar-dirty' is set and the parent
session has fully materialized."
  (when (mevedel-agent-invocation-p invocation)
    (let ((buf (mevedel-agent-invocation-buffer invocation))
          (rel (mevedel-agent-invocation-transcript-relative-path
                invocation))
          (session (mevedel-agent-invocation-parent-session invocation))
          (parent-buf (mevedel-agent-invocation-parent-data-buffer
                       invocation)))
      (when (and buf (buffer-live-p buf))
        (with-current-buffer buf
          (when (and buffer-file-name rel session
                     (mevedel-session-save-path session)
                     (string= (expand-file-name buffer-file-name)
                              (expand-file-name
                               rel
                               (mevedel-session-save-path session))))
            (condition-case err
                (progn
                  (when (buffer-modified-p)
                    (let ((save-silently t)
                          (inhibit-message t)
                          (message-log-max nil))
                      (basic-save-buffer)))
                  (let ((now (format-time-string "%FT%H-%M-%S")))
                    (mevedel-session-persistence--update-transcript-entry
                     session
                     (mevedel-agent-invocation-agent-id invocation)
                     (list :updated-at now)))
                  ;; Retry sidecar if dirty and parent has materialized
                  ;; through its first DONE.
                  (when (and (mevedel-agent-invocation-sidecar-dirty invocation)
                             (mevedel-session-save-path session)
                             (buffer-live-p parent-buf))
                    (when (mevedel-session-persistence--write-sidecar-now
                           session parent-buf)
                      (setf (mevedel-agent-invocation-sidecar-dirty invocation)
                            nil)))
                  t)
              (error
               (message "mevedel: transcript save failed for %s: %S"
                        (mevedel-agent-invocation-agent-id invocation)
                        err)
               nil))))))))

(defun mevedel-agent-exec--prompt-heading-position ()
  "Return the buffer position of the `* Agent Task:' heading, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Agent Task:" nil t)
      (line-beginning-position))))

(defun mevedel-agent-exec--insert-injected-prompt
    (invocation block &optional position)
  "Insert BLOCK as a user-role region in INVOCATION's agent buffer.

Used by `mevedel-tools--handle-message-inject' (mailbox) and
`mevedel-tools--handle-wait-inject' (reminders) so the audit log
captures injected user-role content that `gptel--inject-prompt'
otherwise writes only to `info :data :messages'.

When POSITION is `prepend', the block is inserted just above the
`* Agent Task:' heading, so the audit log matches a
`gptel--inject-prompt' call that inserted at position 0.  Otherwise
the block is appended at point-max, matching normal mailbox delivery
after prior assistant output.

Clears any inherited `gptel'/'response' text properties from the
inserted text because these blocks are user-role prompt injections.
After insertion, triggers a transcript save so the injection is
durable before the WAIT handler fires the HTTP request.

Best-effort: failure to write to the buffer is logged and ignored
so it cannot abort the WAIT cycle.  The LLM payload is authoritative
regardless of buffer state."
  (when (and (mevedel-agent-invocation-p invocation)
             (stringp block)
             (not (string-empty-p block)))
    (let ((buf (mevedel-agent-invocation-buffer invocation)))
      (when (and buf (buffer-live-p buf))
        (condition-case err
            (with-current-buffer buf
              (let ((inhibit-read-only t)
                    start)
                (save-excursion
                  (cond
                   ;; Mirror `gptel--inject-prompt' position 0.
                   ((and (eq position 'prepend)
                         (mevedel-agent-exec--prompt-heading-position))
                    (goto-char (mevedel-agent-exec--prompt-heading-position))
                    (unless (bolp) (insert "\n"))
                    (setq start (point))
                    (insert block)
                    (unless (bolp) (insert "\n"))
                    (insert "\n"))
                   ;; Subsequent turn or no heading found: append.
                   (t
                    (goto-char (point-max))
                    (unless (bolp) (insert "\n"))
                    (unless (looking-back "\n\n" 2) (insert "\n"))
                    (setq start (point))
                    (insert block)
                    (unless (bolp) (insert "\n"))))
                  (when start
                    (remove-text-properties
                     start (point)
                     '(gptel nil response nil invisible nil front-sticky nil))))
                (mevedel-agent-exec--flush-transcript-save invocation)))
          (error
           (message "mevedel: insert-injected-prompt failed: %S" err)))))))

(defun mevedel-agent-exec--finalize (invocation status)
  "Mark INVOCATION's transcript STATUS terminal and save once more.

STATUS is one of `completed', `error', `aborted'.  Idempotent: if
the invocation already has a terminal status, returns immediately.

Patches the parent's `<!-- mevedel-render-data -->' block so the view
buffer's handle picks up the terminal status (badge transitions from
`[running]' to `✓ done' / `✗ error' / `✗ aborted')."
  (when (mevedel-agent-invocation-p invocation)
    (let ((current (mevedel-agent-invocation-transcript-status invocation)))
      (unless (memq current '(completed error aborted))
        (setf (mevedel-agent-invocation-transcript-status invocation) status)
        (let ((session (mevedel-agent-invocation-parent-session invocation))
              (parent-buf (mevedel-agent-invocation-parent-data-buffer
                           invocation))
              (now (format-time-string "%FT%H-%M-%S")))
          (when session
            (mevedel-session-persistence--update-transcript-entry
             session
             (mevedel-agent-invocation-agent-id invocation)
             (list :status status :updated-at now)))
          (mevedel-agent-exec--flush-transcript-save invocation)
          ;; Sync the new terminal status (and any captured
          ;; `terminal-reason') onto the parent's render-data block,
          ;; then trigger a parent-view rerender.  Without this, the
          ;; handle's `[running]' badge stays put even after the
          ;; sub-agent has reached DONE/ERRS/ABRT.
          (let ((mevedel-agent-exec--suppress-activity-rerender t))
            (mevedel-agent-exec--record-activity
             invocation
             (list :type 'status :status status
                   :summary (format "%s" status))))
          (when (and session
                     (mevedel-agent-invocation-background-p invocation))
            (mevedel-session-persistence--update-transcript-entry
             session
             (mevedel-agent-invocation-agent-id invocation)
             (list :activity
                   (mevedel-agent-exec--final-activity-snapshot
                    invocation))))
          (mevedel-agent-exec--handle-update invocation)
          (when (and session (buffer-live-p parent-buf))
            (mevedel-session-persistence--write-sidecar-now
             session parent-buf))
          (setf (mevedel-agent-invocation-activity invocation) nil)
          (mevedel-agent-exec--run-stop-hook invocation status)
          ;; Kill the transcript buffer if it has no windows anywhere.
          ;; Drop the kill hook before killing -- finalization has
          ;; already done the FSM teardown so the hook's gptel-abort
          ;; would either no-op or produce a spurious "Stopped"
          ;; message on a buffer whose request is gone.
          (let ((buf (mevedel-agent-invocation-buffer invocation))
                (view-kept
                 (and (fboundp 'mevedel-view-agent-live-transcript-finalize)
                      (mevedel-view-agent-live-transcript-finalize
                       invocation))))
            (when (and buf (buffer-live-p buf)
                       (not view-kept)
                       (null (get-buffer-window-list buf nil t)))
              (with-current-buffer buf
                (remove-hook 'kill-buffer-hook
                             #'mevedel-agent-exec--on-buffer-kill t))
              (condition-case _
                  (kill-buffer buf)
                (error nil)))))))))

(defun mevedel-agent-exec--error-reason-from-info (info)
  "Extract a short human reason string from gptel-fsm INFO, or nil.
Assembles a single-line reason from the HTTP `:status' and the
`:error' value (which may be a string or a plist with `:type' /
`:message').  Truncated at 200 characters."
  (when (listp info)
    (let* ((status (plist-get info :status))
           (err (plist-get info :error))
           (parts nil))
      (when (and status (stringp status) (not (string-empty-p status)))
        (push (string-trim status) parts))
      (cond
       ((stringp err)
        (push (string-trim err) parts))
       ((listp err)
        (when-let* ((type (plist-get err :type)))
          (push (string-trim (format "%s" type)) parts))
        (when-let* ((msg (plist-get err :message)))
          (push (string-trim (format "%s" msg)) parts))))
      (when parts
        (let* ((joined (mapconcat #'identity (nreverse parts) ": "))
               (max 200))
          (if (> (length joined) max)
              (concat (substring joined 0 max) "...")
            joined))))))

(defun mevedel-agent-exec--error-reason-from-fsm (fsm)
  "Extract a short human reason string from FSM's `:error' info, or nil.
Reads `gptel-fsm-info' and delegates to
`mevedel-agent-exec--error-reason-from-info'."
  (when (and fsm (fboundp 'gptel-fsm-info))
    (mevedel-agent-exec--error-reason-from-info (gptel-fsm-info fsm))))


(defun mevedel-agent-exec--task-preview-setup (arg-values _info)
  "Tool-preview renderer for the Agent tool.

Called by gptel during tool preview for each Agent call to format
the call's (TYPE DESCRIPTION PROMPT) argument list inline.  ARG-VALUES
is the positional argument list; the second plist is the tool call
info, unused here.

Delegates the confirmation-overlay wrapping to
`gptel-agent--confirm-overlay' for now -- that helper is outside the
sub-agent-runtime extraction scope and replacing it earns little."
  (pcase-let ((from (point))
              (`(,type ,desc ,prompt) arg-values))
    (insert "("
            (propertize "Agent " 'font-lock-face 'font-lock-keyword-face)
            (propertize (prin1-to-string type)
                        'font-lock-face 'font-lock-escape-face)
            " " (propertize (prin1-to-string desc)
                            'font-lock-face
                            '(:inherit font-lock-constant-face :inherit bold))
            "\n" (propertize (prin1-to-string prompt)
                             'line-prefix "  "
                             'wrap-prefix "  "
                             'font-lock-face 'font-lock-constant-face)
            ")\n\n")
    (gptel-agent--confirm-overlay from (point) t)))


;;
;;; FSM handler table

(defun mevedel-agent-exec--invocation-from-info (info)
  "Return the `mevedel-agent-invocation' recorded on INFO, or nil."
  (or (and (mevedel-agent-invocation-p
            (plist-get info :mevedel-agent-invocation))
           (plist-get info :mevedel-agent-invocation))
      (when-let* ((ov (plist-get info :context))
                  ((overlayp ov)))
        (overlay-get ov 'mevedel-agent-invocation))))

(defun mevedel-agent-exec--invocation-from-fsm (fsm)
  "Return the `mevedel-agent-invocation' for FSM, or nil."
  (when fsm
    (mevedel-agent-exec--invocation-from-info (gptel-fsm-info fsm))))

(defun mevedel-agent-exec--handle-tret-save (fsm)
  "Schedule an agent-buffer save after `gptel--handle-tool-result' returns.

Long tool loops can run many WAIT/TOOL/TRET cycles between two DONE
events.  Saving through a debounce keeps recent output durable without
running Org save machinery synchronously on every tool boundary."
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-exec--schedule-transcript-save inv)))

(defun mevedel-agent-exec--handle-wait-activity (fsm)
  "Record a sparse waiting activity item for FSM."
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (unless (eq (plist-get (car (last (mevedel-agent-invocation-activity inv)))
                           :type)
                'waiting)
      (mevedel-agent-exec--record-activity
       inv '(:type waiting :summary "waiting")))))

(defun mevedel-agent-exec--handle-done-save (fsm)
  "Run gptel's post-insert path so `gptel-post-response-functions' fires.

The current sub-agent FSM table had no DONE entry; this hook
delegates to `gptel--handle-post-insert' and then triggers an
explicit transcript save (for completeness; the post-response hook
also calls the save helper buffer-locally)."
  (when (fboundp 'gptel--handle-post-insert)
    (condition-case _ (gptel--handle-post-insert fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-exec--flush-transcript-save inv)))

(defun mevedel-agent-exec--handle-abort-save (fsm)
  "Drive gptel's abort path and finalize the transcript as `aborted'."
  (when (fboundp 'gptel--handle-abort)
    (condition-case _ (gptel--handle-abort fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (mevedel-agent-exec--finalize inv 'aborted)))

(defun mevedel-agent-exec--handle-errs-save (fsm)
  "Run gptel's error path and finalize the transcript as `error'.

`gptel-post-response-functions' fires from `gptel--handle-error',
which writes an error region into the buffer; persisting that
output is the audit log's job.

Captures a short reason (HTTP status + error type/message) onto the
invocation's `terminal-reason' slot before finalize so the parent's
render-data badge can show e.g. `✗ error · 429: rate_limit_error'."
  (when (fboundp 'gptel--handle-error)
    (condition-case _ (gptel--handle-error fsm) (error nil)))
  (when-let* ((inv (mevedel-agent-exec--invocation-from-fsm fsm)))
    (when-let* ((reason (mevedel-agent-exec--error-reason-from-fsm fsm)))
      (setf (mevedel-agent-invocation-terminal-reason inv) reason))
    (mevedel-agent-exec--finalize inv 'error)
    (when (and (mevedel-agent-invocation-background-p inv)
               (fboundp 'mevedel-tools--complete-background-agent))
      (mevedel-tools--complete-background-agent
       inv
       (format "Error: Task %s could not finish task \"%s\".

Error details: %S"
               (or (and (mevedel-agent-invocation-agent inv)
                        (mevedel-agent-name
                         (mevedel-agent-invocation-agent inv)))
                   "agent")
               (or (mevedel-agent-invocation-description inv) "")
               (plist-get (gptel-fsm-info fsm) :error))))))

(defvar mevedel-agent-exec--handlers
  `((WAIT ,#'mevedel-agent-exec--handle-wait-activity
          ,#'gptel--handle-wait)
    (TPRE ,#'gptel--handle-token-usage
          ,#'mevedel--compact-record-token-baseline
          ,#'gptel--handle-pre-tool
          ,#'gptel--fsm-transition)
    (TOOL ,#'gptel--handle-tool-use)
    (TRET ,#'gptel--handle-post-tool
          ,#'gptel--handle-tool-result
          ,#'mevedel-agent-exec--handle-tret-save)
    (DONE ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-agent-exec--handle-done-save)
    (ABRT ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-agent-exec--handle-abort-save)
    (ERRS ,#'mevedel--compact-record-token-baseline
          ,#'mevedel-agent-exec--handle-errs-save))
  "Handler table for the mevedel sub-agent FSM.

Same shape as `gptel-send--transitions': each entry is `(STATE
FN ...)' where FN is called when the FSM transitions into (or out
of) STATE.  Modelled after the upstream `gptel-agent-request--handlers'
table.

Additions:

- `TRET' gains `mevedel-agent-exec--handle-tret-save' so transcripts
  are durable across long tool loops (gptel's post-response hook
  fires only at DONE/ABRT, not TRET).
- `DONE' is added with `gptel--handle-post-insert' delegation so
  `gptel-post-response-functions' actually runs in the agent buffer.
- `ABRT' drives the transcript through finalization with status
  `aborted'.")


;;
;;; Request buffer configuration

(defun mevedel-agent-exec--force-initial-tool-use-p (agent-type invocation)
  "Return non-nil when AGENT-TYPE should be forced to use a tool first.

Coordinator invocations are only useful when they actually create
tasks and/or dispatch workers.  On the first turn, force tool use
so a text-only \"I'll do it\" response cannot terminate the
foreground skill dispatch."
  (and (equal agent-type "coordinator")
       (mevedel-agent-invocation-p invocation)
       (zerop (or (mevedel-agent-invocation-turn-count invocation) 0))))

(defun mevedel-agent-exec--clear-forced-tool-choice (fsm)
  "Remove one-shot forced tool choice from FSM request data.

`gptel-use-tools' = `force' is intentionally used only to make a
coordinator's first action be a real tool call.  Once the model has
produced tool use and the FSM reaches TPRE, remove provider-specific
force fields so later WAIT cycles can produce the final text
synthesis without being forced into another tool call."
  (let* ((info (gptel-fsm-info fsm))
         (data (plist-get info :data)))
    (when (listp data)
      ;; OpenAI / Responses / Anthropic.
      (cl-remf data :tool_choice)
      ;; Gemini and Bedrock both use :toolConfig, but with different
      ;; force keys.  Preserve any remaining config, especially
      ;; Bedrock's :tools entry.
      (when-let* ((tool-config (plist-get data :toolConfig)))
        (when (listp tool-config)
          (cl-remf tool-config :functionCallingConfig)
          (cl-remf tool-config :toolChoice)
          (if tool-config
              (plist-put data :toolConfig tool-config)
            (cl-remf data :toolConfig))))
      (plist-put info :data data))))

(defun mevedel-agent-exec--inject-sendmessage (buffer)
  "Append the SendMessage gptel-tool to BUFFER's `gptel-tools'.

Idempotent: if a tool whose name is `\"SendMessage\"' is already on
the list, this is a no-op.  Looks the tool up via `gptel-get-tool'
in the `(\"mevedel\" \"SendMessage\")' category-name pair.  Used to
hand background sub-agents the dialog channel without forcing every
agent definition to declare the tool statically -- the capability
is a property of the dispatch shape (background vs foreground),
not a per-agent decision."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((tool (and (fboundp 'gptel-get-tool)
                       (ignore-errors
                         (gptel-get-tool '("mevedel" "SendMessage"))))))
        (when (and tool
                   (not (cl-some (lambda (existing)
                                   (and (gptel-tool-p existing)
                                        (equal (gptel-tool-name existing)
                                               "SendMessage")))
                                 gptel-tools)))
          (setq-local gptel-tools (append gptel-tools (list tool))))))))

(defun mevedel-agent-exec--apply-request-locals (buffer values)
  "Apply gptel request-local VALUES to BUFFER.

VALUES is an alist of `(SYMBOL . VALUE)' captured while the agent
preset is dynamically active.  `gptel-request' builds its prompt
buffer by copying these variables from the request buffer, so an
agent-specific buffer must carry the preset values buffer-locally
before dispatch."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (dolist (entry values)
        (set (make-local-variable (car entry)) (cdr entry))))))

(defun mevedel-agent-exec--refresh-initial-transcript-state (invocation)
  "Persist INVOCATION after agent request locals have been installed."
  (when (and (mevedel-agent-invocation-p invocation)
             (mevedel-agent-invocation-transcript-relative-path invocation))
    (let ((buf (mevedel-agent-invocation-buffer invocation)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (set-buffer-modified-p t))
        (mevedel-agent-exec--flush-transcript-save invocation)))))

(defun mevedel-agent-exec--provider-for-invocation (agent-type invocation)
  "Return the resolved provider for AGENT-TYPE and INVOCATION, or nil.

Precedence is explicit Agent-tool tier, skill selector, agent default
tier, then inherit."
  (let* ((selector
          (cond
           ((and invocation
                 (mevedel-agent-invocation-model-tier-override invocation)))
           ((and invocation
                 (mevedel-agent-invocation-skill-model-override invocation)))
           (t (mevedel-model-agent-default-selector agent-type)))))
    (mevedel-model-resolve-selector selector)))


;;
;;; Task runner

(defun mevedel-agent-exec--run-start-hook-sync
    (agent-type description prompt invocation)
  "Run `SubagentStart' hooks synchronously and return their decision.
PROMPT is included in the event payload.  Command hooks still execute
through the shared async runner; this helper waits because callers need
the child FSM synchronously for abort/background bookkeeping."
  (require 'mevedel-hooks)
  (let* ((session (and (mevedel-agent-invocation-p invocation)
                       (mevedel-agent-invocation-parent-session invocation)))
         (workspace (and session (mevedel-session-workspace session)))
         (done nil)
         decision)
    (mevedel-hooks-run-event
     'SubagentStart
     (mevedel-hooks-event-plist
      'SubagentStart session workspace
      :agent-type agent-type
      :agent-id (and (mevedel-agent-invocation-p invocation)
                     (mevedel-agent-invocation-agent-id invocation))
      :description description
      :prompt prompt
      :background (and (mevedel-agent-invocation-p invocation)
                       (mevedel-agent-invocation-background-p invocation))
      :transcript-relative-path
      (and (mevedel-agent-invocation-p invocation)
           (mevedel-agent-invocation-transcript-relative-path invocation)))
     (lambda (result)
       (setq decision result
             done t))
     session workspace nil invocation)
    (while (not done)
      (accept-process-output nil 0.05))
    decision))

(defun mevedel-agent-exec--run-stop-hook (invocation status)
  "Fire `SubagentStop' hooks for INVOCATION terminal STATUS."
  (when (mevedel-agent-invocation-p invocation)
    (require 'mevedel-hooks)
    (let* ((session (mevedel-agent-invocation-parent-session invocation))
	   (workspace (and session (mevedel-session-workspace session)))
	   (agent (mevedel-agent-invocation-agent invocation))
	   (agent-type (and agent (mevedel-agent-name agent)))
	   (parent-buffer
	    (mevedel-agent-invocation-parent-data-buffer invocation))
	   (runner
	    (lambda ()
	      (mevedel-hooks-run-event
	       'SubagentStop
	       (mevedel-hooks-event-plist
	        'SubagentStop session workspace
	        :agent-type agent-type
	        :agent-id (mevedel-agent-invocation-agent-id invocation)
	        :description (mevedel-agent-invocation-description invocation)
	        :status status
	        :terminal-reason
	        (mevedel-agent-invocation-terminal-reason invocation)
	        :background (mevedel-agent-invocation-background-p invocation)
	        :transcript-relative-path
	        (mevedel-agent-invocation-transcript-relative-path
	         invocation))
	       #'ignore
	       session workspace nil invocation))))
      (if (and parent-buffer (buffer-live-p parent-buffer))
	  (with-current-buffer parent-buffer
	    (funcall runner))
	(funcall runner)))))

(defun mevedel-agent-exec--run (main-cb agent-type description prompt
                                        &optional invocation agent-buffer)
  "Dispatch a sub-agent task and route its final response to MAIN-CB.

AGENT-TYPE is the registry key (e.g. `\"explorer\"', `\"verifier\"').
DESCRIPTION is a short human-facing label shown in the agent handle.
PROMPT is the full instruction handed to the sub-agent.

Optional INVOCATION is the `mevedel-agent-invocation' associated with
this task.  When present it is stashed on the FSM info plist so
reminder/message handlers can reach it, and the FSM's WAIT state is
augmented with the mevedel message-inject and reminder-inject handlers.
The BWAIT parking state is also installed so background children keep
the FSM alive.

Optional AGENT-BUFFER is the per-invocation gptel buffer that should
hold the sub-agent's transcript.  When present, the
sub-agent request runs there with `gptel-request nil :buffer
AGENT-BUFFER'; otherwise the request runs against the parent chat
buffer at `gptel--fsm-last' (legacy / fallback path).

Callback contract.  Unlike the upstream `gptel-agent--task', which
fires MAIN-CB on every streamed chunk and drops gptel's end-of-stream
`t' signal, this runner:

  - accumulates streamed string chunks into `partial';
  - fires MAIN-CB exactly once on the `t' branch, after the sub-agent
    turn has completed and no further tool-use is pending.

When AGENT-BUFFER is supplied, the wrapper forwards each insertable
event (string chunks, tool-call / tool-result entries) to gptel's
stock callback before running the mevedel bookkeeping, so the agent
buffer reflects the event before the wrapper's `partial`
accumulator acts on it.  Terminal events (`t', nil, abort) skip the
forward step.

Returns the spawned FSM."
  (let ((decision (mevedel-agent-exec--run-start-hook-sync
                   agent-type description prompt invocation)))
    (when (and (plist-member decision :continue)
               (not (plist-get decision :continue)))
      (error "%s" (or (plist-get decision :stop-reason)
                      "SubagentStart hook stopped sub-agent")))
    (when-let* ((msg (plist-get decision :system-message)))
      (message "mevedel: %s" msg))
    (when-let* ((context (mevedel-hooks-additional-context-string decision)))
      (setq prompt (concat prompt "\n\n" context))
      (when (buffer-live-p agent-buffer)
        (with-current-buffer agent-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n" context "\n")))
        (mevedel-agent-exec--flush-transcript-save invocation))))
  (let ((force-initial-tool-use
         (mevedel-agent-exec--force-initial-tool-use-p
          agent-type invocation))
        (agent-spec
         (or (cdr (assoc agent-type mevedel-agent-exec--agents))
             (and (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-agent invocation)
                  (cdr (mevedel-agent-to-gptel-spec
                        (mevedel-agent-invocation-agent invocation))))))
        (parent-include-reasoning gptel-include-reasoning))
    (gptel-with-preset
     (nconc (list :use-tools (if force-initial-tool-use 'force t)
                  :context nil)
            (and gptel-agent-preset
                 (copy-sequence
                  (cond
                   ((symbolp gptel-agent-preset)
                    (gptel-get-preset gptel-agent-preset))
                   ((listp gptel-agent-preset)
                    gptel-agent-preset)
                   (t (error "Invalid `gptel-agent-preset': %S"
                             gptel-agent-preset)))))
            agent-spec
            (list :include-reasoning parent-include-reasoning))
     (let* ((provider (mevedel-agent-exec--provider-for-invocation
                       agent-type invocation))
            (effective-backend (or (plist-get provider :backend) gptel-backend))
            (effective-model (or (plist-get provider :model) gptel-model))
            (info (and (boundp 'gptel--fsm-last)
                       gptel--fsm-last
                       (gptel-fsm-info gptel--fsm-last)))
            (where (or (plist-get info :tracking-marker)
                       (plist-get info :position)
                       (copy-marker (point-max) nil)))
            (partial (format "%s result for task: %s\n\n"
                             (capitalize agent-type) description))
            (fsm (gptel-make-fsm :table gptel-send--transitions
                                 :handlers mevedel-agent-exec--handlers))
            (mevedel-cb (mevedel-agent-exec--make-callback
                         main-cb agent-type description where (list partial)))
            (request-locals
             `((gptel-backend . ,effective-backend)
               (gptel-model . ,effective-model)
               (gptel--system-message . ,gptel--system-message)
               (gptel-use-tools . ,gptel-use-tools)
               (gptel-tools . ,gptel-tools)
               (gptel-use-context . ,gptel-use-context)
               (gptel-context . ,gptel-context)
               (gptel-stream . ,gptel-stream)
               (gptel-use-curl . ,gptel-use-curl)
               (gptel-include-reasoning . ,gptel-include-reasoning)
               (gptel-temperature . ,gptel-temperature)
               (gptel-max-tokens . ,gptel-max-tokens)
               (gptel-cache . ,gptel-cache)
               (gptel--request-params . ,gptel--request-params))))
       (when invocation
         (setf (gptel-fsm-handlers fsm)
               (mevedel-tools--augment-agent-handlers
		(gptel-fsm-handlers fsm)
		:prepend
		`((WAIT . (,#'mevedel-tools--handle-message-inject
                           ,#'mevedel-tools--handle-wait-inject))
                  ,@(when force-initial-tool-use
                      `((TPRE . (,#'mevedel-agent-exec--clear-forced-tool-choice)))))))
         (mevedel-tools--inject-bwait-transition fsm))
       ;; Register the FSM on the parent chat buffer's registry BEFORE
       ;; dispatching gptel-request -- otherwise a racing mevedel-abort
       ;; cannot find the entry while the HTTP request is being set up.
       ;; The registration uses the invocation's agent-id; legacy
       ;; callers without an invocation skip this step.
       (when (and invocation
                  (mevedel-agent-invocation-p invocation)
                  (mevedel-agent-invocation-agent-id invocation))
         (let ((agent-id (mevedel-agent-invocation-agent-id invocation))
               (parent-buf
		(mevedel-agent-invocation-parent-data-buffer invocation)))
           (when (and parent-buf (buffer-live-p parent-buf))
             (with-current-buffer parent-buf
               (setf (alist-get agent-id mevedel-tools--agents-fsm
				nil nil #'equal)
                     fsm)))))
       (when invocation
         (setf (gptel-fsm-info fsm)
               (plist-put (gptel-fsm-info fsm)
                          :mevedel-agent-invocation invocation)))
       (gptel--update-status " Calling Agent..." 'font-lock-escape-face)
       (cond
	;; Dispatch into the per-invocation agent buffer and wrap
	;; gptel's stock :callback so insertion happens through
	;; gptel's normal dispatch.  `gptel-request' computes its
	;; default `:position' marker from `(point-marker)' in the
	;; current buffer when no explicit `:position' is supplied
	;; (gptel-request.el ~2119); without `with-current-buffer
	;; agent-buffer' the marker would live in the parent buffer
	;; and gptel's insertion path would write the response there
	;; instead of into the agent transcript.
	((buffer-live-p agent-buffer)
         (mevedel-agent-exec--apply-request-locals
          agent-buffer request-locals)
         (mevedel-agent-exec--refresh-initial-transcript-state
          invocation)
         ;; Background sub-agents run concurrently with their caller,
         ;; so SendMessage is meaningful: live mailbox routing reaches
         ;; main, parent coordinator, or the sender's own children on
         ;; their next WAIT, according to `mevedel-tools--resolve-recipient'.
         ;; For
         ;; foreground sub-agents the caller's FSM parks in TOOL until
         ;; the sub-agent terminates -- live messaging would queue
         ;; messages that only drain at end-of-sub-agent, alongside
         ;; the terminal `<agent-result>'.  Inject the SendMessage
         ;; tool only when background; idempotent when the agent's
         ;; static `:tools' already includes it (e.g. coordinator).
         (when (and invocation
                    (mevedel-agent-invocation-p invocation)
                    (mevedel-agent-invocation-background-p invocation))
           (mevedel-agent-exec--inject-sendmessage agent-buffer))
         (with-current-buffer agent-buffer
           (goto-char (point-max))
           (gptel-request nil
			  :buffer agent-buffer
			  :fsm fsm
			  :stream gptel-stream
			  :system gptel--system-message
			  :transforms (list #'gptel--transform-add-context)))
         (let* ((req-info (gptel-fsm-info fsm))
		(gptel-cb (plist-get req-info :callback))
		(wrapped
                 (mevedel-agent-exec--wrap-callback gptel-cb mevedel-cb)))
           (when invocation
             (setq req-info
                   (plist-put req-info :mevedel-agent-invocation invocation)))
           (setf (gptel-fsm-info fsm)
                 (plist-put req-info :callback wrapped)))
         fsm)
	;; Legacy / fallback path: dispatch against the parent chat
	;; buffer with our callback as :callback (no wrap-and-chain
	;; because no agent buffer needs insertion).
	(t
         (gptel-request prompt
			:fsm fsm
			:stream gptel-stream
			:transforms (list #'gptel--transform-add-context)
			:callback mevedel-cb)
         (when invocation
           (setf (gptel-fsm-info fsm)
                 (plist-put (gptel-fsm-info fsm)
                            :mevedel-agent-invocation invocation)))
         fsm))))))

(defun mevedel-agent-exec--wrap-callback (gptel-cb mevedel-cb)
  "Build the wrap-and-chain callback for the agent-buffer dispatch path.

GPTEL-CB is gptel's stock insertion callback captured from the
FSM's `:callback' info slot (typically `gptel--insert-response' or
`gptel-curl--stream-insert-response').  MEVEDEL-CB is the bookkeeping
callback returned by `mevedel-agent-exec--make-callback'.

For each event delivered by gptel:

- terminal events (response is `t', nil, or `abort'): run
  MEVEDEL-CB only.  These produce no buffer insertion; gptel's
  stock callback would be a no-op insert path.
- insertable events (string chunks, `(tool-call . ...)`,
  `(tool-result . ...)`, etc.): forward to GPTEL-CB first so the
  agent buffer reflects the event, then run MEVEDEL-CB so the
  partial accumulator and finalize gating see the post-insert
  state.

Errors from either delegate are caught so a misbehaving callback
cannot strand the FSM."
  (lambda (response &rest rest)
    (let ((terminal (memq response '(t nil abort))))
      (unless terminal
        (when gptel-cb
          (condition-case err (apply gptel-cb response rest)
            (error
             (message "mevedel: gptel insertion callback errored: %S" err)))))
      (when mevedel-cb
        (condition-case err (apply mevedel-cb response rest)
          (error
           (message "mevedel: agent bookkeeping callback errored: %S" err)))))))

(defun mevedel-agent-exec--make-callback (main-cb agent-type description
                                                  where partial-cell)
  "Return the callback used by `mevedel-agent-exec--run'.

MAIN-CB receives the final accumulated partial string exactly once.

AGENT-TYPE and DESCRIPTION decorate the error / abort messages.  WHERE
is the tracking-marker fallback for the initial `tool-call' dispatch.
PARTIAL-CELL is a one-element list holding the running accumulated
text; string chunks are concatenated into its car, and the `t'
completion branch reads it back out.

The dispatch table is:

- `nil': transport error; MAIN-CB receives a formatted error string.
- `(tool-call . CALLS)': update tracking marker and hand off to
  `gptel--display-tool-calls'.
- `(pred stringp)': accumulate into PARTIAL-CELL.  When `:stream' is
  absent from the info plist (non-streaming request), also treat the
  string as the terminal signal because gptel never sends `'t' in
  that mode -- see `gptel-curl--stream-cleanup' at gptel-request.el
  2864, which is the only site that fires `'t' and runs on the
  streaming curl sentinel only.  Both non-streaming paths
  \(`gptel--url-parse-response' at 2602 and the non-streaming branch
  of `gptel-curl--parse-response' at 3018) deliver the final text as
  one string and advance the FSM without any terminal event.
- `'t': stream complete; if no tool-use is pending, run the optional
  transformer over the partial and fire MAIN-CB once.
- `'abort': aborted; MAIN-CB receives a formatted abort string.

A per-closure `fired' latch makes all delivery branches idempotent so
streaming runs that emit one string chunk followed by `'t', and the
defensive corner cases, cannot double-fire.  MAIN-CB invocations are
wrapped in `condition-case' so a throw inside the caller (e.g. a
dead chat buffer or malformed plist) does not escape
gptel's callback chain and strand the parent's background-agent
bookkeeping."
  (cl-labels ((safe-call (cb &rest args)
                (condition-case err
                    (apply cb args)
                  (error
                   (message "mevedel agent-exec main-cb error: %S" err))))
              (terminal-ready-p (info)
                ;; A text-only `'t' event is the FSM's "this turn produced
                ;; no tool calls" signal.  But for sub-agents with
                ;; background children, an intermediate text turn (e.g.
                ;; "Waiting for the third explorer...") fires `'t' too --
                ;; the FSM then parks in BWAIT, eventually resumes WAIT
                ;; on a child completion, and produces another text turn
                ;; with the final synthesis.  Finalizing on the first
                ;; text turn would deliver the intermediate text to the
                ;; parent's tool callback, *and* set the `fired' latch,
                ;; preventing finalize from running on the actual final
                ;; turn.  The check below holds finalize until the
                ;; sub-agent's background-agents and messages mailbox
                ;; are both empty -- at that point a text-only turn is
                ;; truly final.  No invocation in the request info
                ;; (legacy callers) → ready unconditionally.
                (let* ((inv (mevedel-agent-exec--invocation-from-info info)))
                  (or (not (and inv (mevedel-agent-invocation-p inv)))
                      (and (not (mevedel-tools--ctx-background-agents inv))
                           (not (mevedel-tools--ctx-messages inv)))))))
    (let ((fired nil))
      ;; Accept &rest so the wrap-and-chain forwarder can pass through
      ;; gptel's optional `raw' third argument (see
      ;; `gptel--insert-response' / `gptel-curl--stream-insert-response')
      ;; without tripping a wrong-number-of-arguments.
      (lambda (resp info &rest _ignored)
        (let ((ov (plist-get info :context)))
          (cl-flet ((finalize ()
                      (when (bound-and-true-p mevedel-tools-task-debug)
                        (message "mevedel AGENT-EXEC FINALIZE agent=%s desc=%S \
partial-len=%d :tool-use=%S :stream=%S"
                                 agent-type description
                                 (length (or (car partial-cell) ""))
                                 (and (plist-get info :tool-use) t)
                                 (and (plist-get info :stream) t)))
                      (when (overlayp ov) (delete-overlay ov))
                      (when-let* ((transformer (plist-get info :transformer)))
                        (setcar partial-cell
                                (funcall transformer (car partial-cell))))
                      ;; Drive transcript finalization from
                      ;; the success path so a non-error completion
                      ;; lands on disk before the parent sees the
                      ;; result.
                      (let* ((inv (mevedel-agent-exec--invocation-from-info
                                   info))
                             (final-response
                              (and (mevedel-agent-invocation-p inv)
                                   (mevedel-agent-exec--final-response-text
                                    inv))))
                        (when (mevedel-agent-invocation-p inv)
                          (mevedel-agent-exec--finalize inv 'completed))
                        (safe-call main-cb
                                   (or final-response (car partial-cell))))))
            (pcase resp
              ('nil
               (unless fired
                 (setq fired t)
                 (let* ((inv (mevedel-agent-exec--invocation-from-info info)))
                   (when (mevedel-agent-invocation-p inv)
                     (when-let* ((reason
                                  (mevedel-agent-exec--error-reason-from-info
                                   info)))
                       (setf (mevedel-agent-invocation-terminal-reason inv)
                             reason))
                     (mevedel-agent-exec--finalize inv 'error)))
                 (when (overlayp ov) (delete-overlay ov))
                 (safe-call main-cb
                            (format "Error: Task %s could not finish task \"%s\".

Error details: %S"
                                    agent-type description
                                    (plist-get info :error)))))
              (`(tool-call . ,calls)
               (unless (plist-get info :tracking-marker)
                 (plist-put info :tracking-marker where))
               (gptel--display-tool-calls calls info))
              ((pred stringp)
               (setcar partial-cell (concat (car partial-cell) resp))
               ;; Non-streaming terminal: gptel removes `:stream' from
               ;; info when the request is non-streaming, and never
               ;; fires `'t' in that mode.  Treat the string as the
               ;; terminal signal for this turn provided no tool-use
               ;; is pending (tool-use turns get another string/`'t'
               ;; on the following WAIT cycle) AND the sub-agent's
               ;; background children / mailbox are drained (otherwise
               ;; this is just an intermediate text turn that BWAIT
               ;; will follow up on -- see `terminal-ready-p').
               (when (and (not fired)
                          (not (plist-get info :stream))
                          (not (plist-get info :tool-use))
                          (terminal-ready-p info))
                 (setq fired t)
                 (finalize)))
              ('t
               ;; Same gating as the stringp branch above.
               (when (and (not fired)
                          (not (plist-get info :tool-use))
                          (terminal-ready-p info))
                 (setq fired t)
                 (finalize)))
              ('abort
               (unless fired
                 (setq fired t)
                 (let* ((inv (mevedel-agent-exec--invocation-from-info info)))
                   (when (mevedel-agent-invocation-p inv)
                     (mevedel-agent-exec--finalize inv 'aborted)))
                 (when (overlayp ov) (delete-overlay ov))
                 (safe-call main-cb
                            (format "Error: Task \"%s\" was aborted by the user. \
%s could not finish."
                                    description agent-type)))))))))))


;;
;;; Preview registration

(with-eval-after-load 'gptel-agent-tools
  ;; Override the upstream entry so the Agent tool's tool-call preview
  ;; uses mevedel's renderer.  Upstream registers its version when
  ;; gptel-agent-tools loads; we stamp on top afterwards.
  (setf (alist-get "Agent" gptel--tool-preview-alist nil nil #'equal)
        #'mevedel-agent-exec--task-preview-setup))


(provide 'mevedel-agent-exec)
;;; mevedel-agent-exec.el ends here
