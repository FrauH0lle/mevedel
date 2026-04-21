;;; mevedel-pipeline.el -- Tool execution pipeline -*- lexical-binding: t -*-

;;; Commentary:

;; Sequential step-based execution engine for mevedel tools. Each tool
;; invocation runs through a standard pipeline: validate -> permission ->
;; snapshot -> handler -> persist. Tool handlers that need user confirmation
;; of a file change call `mevedel-preview-mode-add-preview' directly;
;; there is no explicit confirm step in the pipeline.
;;
;; The persist step saves oversized results to disk and replaces them
;; with a preview + file path, preventing LLM context overflow from
;; unexpectedly large tool output.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'mevedel-permissions)
(require 'mevedel-structs)

(declare-function mevedel-tool-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-handler "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-args "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-async-p "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-path "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-pattern "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-domain "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-get-name "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool-max-result-size "mevedel-tool-registry" (cl-x) t)
(declare-function mevedel-tool--validate-args "mevedel-tool-registry"
                  (tool-name args arg-specs))

(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-tool-fs'
(declare-function mevedel--snapshot-file-if-needed "mevedel-tool-fs" (filepath))

;; `mevedel-tool-ui'
(declare-function mevedel-permission--prompt "mevedel-tool-ui"
                  (tool-name &optional path include-always))

;; `mevedel-tools'
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--ctx-record-used "mevedel-tools" (ctx name))


;;
;;; Error conditions

(define-error 'mevedel-pipeline-error "Pipeline error")
(define-error 'mevedel-permission-denied "Permission denied"
              'mevedel-pipeline-error)
(define-error 'mevedel-validation-error "Validation error"
              'mevedel-pipeline-error)


;;
;;; Result persistence

(defconst mevedel-pipeline--default-max-result-size 50000
  "Global cap on tool result size in characters.
When a tool declares a `max-result-size', the effective limit is
the minimum of the tool value and this default.")

(defconst mevedel-pipeline--preview-size 2000
  "Number of characters to include in the preview when a result is persisted.")

(defun mevedel-pipeline--persist-result (result tool workspace)
  "Save RESULT to disk and return a preview string.

TOOL is the `mevedel-tool' whose result exceeded its size limit.
WORKSPACE provides the `.mevedel/' state directory.  The full result
is written to `.mevedel/tool-results/' and the return value is a
preview-sized replacement for the LLM context."
  (let* ((dir (file-name-concat (mevedel-workspace-state-dir workspace)
                                "tool-results"))
         (name (mevedel-tool-name tool))
         (_ (make-directory dir t))
         (file (make-temp-file (file-name-concat dir (concat name "-"))
                               nil ".txt"))
         (preview-end (min (length result) mevedel-pipeline--preview-size))
         ;; Cut at last newline within preview range to avoid mid-line breaks
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (write-region result nil file nil 'silent)
    (concat "<persisted-output>\n"
            (format "Output too large (%d chars). Full output saved to: %s\n\n"
                    (length result) file)
            (format "Preview (first %d chars):\n" cut)
            (substring result 0 cut)
            (if has-more "\n...\n" "\n")
            "</persisted-output>")))

(defun mevedel-pipeline--truncate-result (result tool)
  "Truncate RESULT to a preview without persisting to disk.

Used when the result exceeds the size limit but no workspace is
available for file persistence.  TOOL is used only for the tool name
in the message."
  (let* ((preview-end (min (length result) mevedel-pipeline--preview-size))
         (cut (let ((nl (cl-position ?\n result :from-end t :end preview-end)))
                (if (and nl (> nl (/ preview-end 2))) nl preview-end)))
         (has-more (< cut (length result))))
    (concat (format "Output too large (%d chars) and no workspace available \
to persist full result (tool: %s).\n\n"
                    (length result) (mevedel-tool-name tool))
            (format "Preview (first %d chars):\n" cut)
            (substring result 0 cut)
            (if has-more "\n...\n" "\n"))))


;;
;;; Pipeline runner

(defun mevedel-pipeline--run (steps callback context)
  "Run pipeline STEPS sequentially, calling CALLBACK with the result.

STEPS is a list of step functions. Each step takes (CONTEXT NEXT) where
CONTEXT is a plist of accumulated state and NEXT is a continuation
function taking an updated context plist.

Sync steps call NEXT directly. Async steps defer and call NEXT
later (e.g., after a user prompt resolves).

CALLBACK is the final async callback (from gptel). It receives either
the result string on success, or an error string prefixed with
\"Error:\" on failure.

CONTEXT is the initial plist, threaded through all steps. The :result
key holds the final value passed to CALLBACK."
  (if (null steps)
      (funcall callback (plist-get context :result))
    (let ((step (car steps))
          (rest (cdr steps)))
      (condition-case err
          (funcall step context
                   (lambda (updated-ctx)
                     (mevedel-pipeline--run rest callback updated-ctx)))
        (mevedel-validation-error
         (funcall callback (format "Error: %s" (cadr err))))
        (mevedel-permission-denied
         (funcall callback (format "Error: Permission denied%s"
                                   (if (cadr err)
                                       (format ": %s" (cadr err))
                                     ""))))
        (mevedel-pipeline-error
         (funcall callback (format "Error: %s" (cadr err))))
        (error
         (funcall callback
                  (format "Error: %s" (error-message-string err))))))))


;;
;;; Standard steps

(defun mevedel-pipeline--step-validate (context next)
  "Validate tool arguments against the arg spec.

Signals `mevedel-validation-error' on failure, calls NEXT on success.
CONTEXT must contain :tool and :args."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (err (mevedel-tool--validate-args
               (mevedel-tool-name tool)
               args
               (mevedel-tool-args tool))))
    (if err
        (signal 'mevedel-validation-error (list err))
      (funcall next context))))

(defun mevedel-pipeline--step-permission (context next)
  "Check permission for the tool invocation.

Reads session state from buffer-locals and calls
`mevedel-check-permission'. If the decision is `ask', prompts the user
via `mevedel-permission--prompt' (blocking). Dispatches the prompt
result to store rules as needed.

When the prompt fires because a path is outside the workspace
root (workspace boundary), the stored rule is tool-agnostic: it uses
\"*\" as the tool name and the path's directory as the scope. This
grants all tools access to that directory.

Signals `mevedel-permission-denied' if the final decision is `deny'.
CONTEXT must contain :tool and :args, NEXT is called on success."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (tool-name (mevedel-tool-name tool))
         (get-path-fn (mevedel-tool-get-path tool))
         (path (when get-path-fn
                 (ignore-errors (funcall get-path-fn args))))
         ;; Read session state from buffer-locals.  Session should always
         ;; exist in chat buffers; fall back to mevedel--workspace for
         ;; edge cases (e.g., tools running outside a session context).
         (session (and (boundp 'mevedel--session) mevedel--session))
         (workspace (cond
                     (session (mevedel-session-workspace session))
                     ((and (boundp 'mevedel--workspace) mevedel--workspace))))
         (workspace-root (when workspace
                           (ignore-errors
                             (mevedel-workspace-root workspace))))
         (persistent-rules (when workspace
                             (mevedel-permission--load-persistent-rules
                              workspace)))
         (session-rules (append
                         (when session
                           (mevedel-session-permission-rules session))
                         persistent-rules))
         (mode (when session (mevedel-session-permission-mode session)))
         ;; Run the decision chain
         (decision (mevedel-check-permission
                    tool-name
                    :tool-struct tool
                    :path path
                    :content args
                    :session-rules session-rules
                    :mode mode
                    :workspace-root workspace-root))
         ;; Workspace boundary: path is outside workspace root and no
         ;; explicit rule covers it.  The rule stored on prompt
         ;; approval should be tool-agnostic ("*") and directory-scoped.
         (workspace-boundary-p
          (and (eq decision 'ask)
               path workspace-root
               (not (string-prefix-p
                     (file-name-as-directory (expand-file-name workspace-root))
                     (expand-file-name path))))))
    (pcase decision
      ('allow (funcall next context))
      ('deny (signal 'mevedel-permission-denied (list tool-name)))
      ('ask
       ;; Prompt the user (blocking via recursive-edit)
       (let* ((rule-tool (if workspace-boundary-p "*" tool-name))
              (rule-path (if workspace-boundary-p
                             (concat (file-name-directory
                                      (expand-file-name path))
                                     "**")
                           path))
              (prompt-result (mevedel-permission--prompt
                              tool-name rule-path
                              (not (null workspace))))
              (final (mevedel-permission--apply-prompt-result
                      prompt-result rule-tool session workspace
                      rule-path)))
         (if (eq final 'deny)
             (signal 'mevedel-permission-denied (list tool-name))
           (funcall next context)))))))

(defun mevedel-pipeline--step-snapshot (context next)
  "Snapshot files before modification.

Extracts the path from tool args via the tool's get-path function and
snapshots it. Only included for non-read-only tools. CONTEXT must
contain :tool and :args, NEXT is called on success."
  (let* ((tool (plist-get context :tool))
         (args (plist-get context :args))
         (get-path-fn (mevedel-tool-get-path tool)))
    (when get-path-fn
      (when-let* ((path (ignore-errors (funcall get-path-fn args))))
        (mevedel--snapshot-file-if-needed path)))
    (funcall next context)))

(defun mevedel-pipeline--record-use (tool)
  "Record that TOOL was invoked on the current turn.

Pushes the tool's name onto the current deferred context's
`deferred-used' slot so that the WAIT handler can reset the TTL for
any tool the model called since the previous turn.  The context is
either a `mevedel-session' (main chat) or a
`mevedel-agent-invocation' (spawned sub-agent), resolved via
`mevedel-tools--current-deferred-context'.  The entry is stored
regardless of whether the tool is deferred; the WAIT handler filters
against the injected set."
  (when-let* ((ctx (mevedel-tools--current-deferred-context)))
    (mevedel-tools--ctx-record-used ctx (mevedel-tool-name tool))))

(defun mevedel-pipeline--step-handler (context next)
  "Run the tool handler.

For async tools (async-p is non-nil), the handler receives a callback as
its first argument followed by the args plist. For sync tools, the
handler receives just the args plist and returns the result directly.

Sets :result in CONTEXT for downstream steps, NEXT is called on success."
  (let* ((tool (plist-get context :tool))
         (handler (mevedel-tool-handler tool))
         (args (plist-get context :args)))
    (mevedel-pipeline--record-use tool)
    (if (mevedel-tool-async-p tool)
        (funcall handler
                 (lambda (result)
                   (funcall next (plist-put context :result result)))
                 args)
      (funcall next
               (plist-put context :result (funcall handler args))))))


(defun mevedel-pipeline--step-persist (context next)
  "Persist oversized tool results to disk.

If the tool has a `max-result-size' and the string result exceeds the
effective limit (the minimum of the tool value and
`mevedel-pipeline--default-max-result-size'), saves the full result to
`.mevedel/tool-results/' and replaces :result with a preview.

When no workspace is available, the result is still truncated to the
preview size to prevent context overflow -- only the file write is
skipped.

Skips entirely when the result is not a string or is an error message.
CONTEXT must contain :tool and :result.  NEXT is called with the
possibly-updated context."
  (let* ((tool (plist-get context :tool))
         (result (plist-get context :result))
         (max-size (mevedel-tool-max-result-size tool))
         (effective (when max-size
                      (min max-size mevedel-pipeline--default-max-result-size))))
    (if (or (null effective)
            (null result)
            (not (stringp result))
            (string-prefix-p "Error:" result)
            (<= (length result) effective))
        (funcall next context)
      ;; Result exceeds limit -- persist or truncate
      (let* ((session (and (boundp 'mevedel--session) mevedel--session))
             (workspace (cond
                         (session (mevedel-session-workspace session))
                         ((and (boundp 'mevedel--workspace) mevedel--workspace)))))
        (funcall next
                 (plist-put context :result
                            (if workspace
                                (mevedel-pipeline--persist-result
                                 result tool workspace)
                              (mevedel-pipeline--truncate-result
                               result tool))))))))


;;
;;; Step list builder

(defun mevedel-pipeline--build-steps (tool)
  "Build the standard step list for TOOL.

Returns a list of step functions based on TOOL's behavioral flags:
  1. validate   -- always included
  2. permission -- always included
  3. snapshot   -- skipped if read-only-p
  4. handler    -- always included
  5. persist    -- included when max-result-size is set"
  (let ((steps nil))
    (when (mevedel-tool-max-result-size tool)
      (push #'mevedel-pipeline--step-persist steps))
    (push #'mevedel-pipeline--step-handler steps)
    (unless (mevedel-tool-read-only-p tool)
      (push #'mevedel-pipeline--step-snapshot steps))
    (push #'mevedel-pipeline--step-permission steps)
    (push #'mevedel-pipeline--step-validate steps)
    steps))


;;
;;; Entry point

(defun mevedel-pipeline-run-tool (tool callback args)
  "Execute TOOL through the standard pipeline.

CALLBACK is the async result callback from gptel. ARGS is a plist of
tool arguments (e.g., (:file_path \"/foo\" :content \"bar\"))."
  (let ((steps (mevedel-pipeline--build-steps tool))
        (context (list :tool tool :args args)))
    (mevedel-pipeline--run steps callback context)))


;;
;;; Args conversion

(defun mevedel-pipeline--positional-to-plist (arg-values arg-specs)
  "Convert positional ARG-VALUES to a keyword plist using ARG-SPECS.

ARG-SPECS is the mevedel args format: ((name type ...) ...).
ARG-VALUES is a list of values in the same order.
Returns a plist like (:name1 val1 :name2 val2 ...)."
  (let ((plist nil))
    (cl-loop for spec in arg-specs
             for val in arg-values
             do (push (intern (format ":%s" (car spec))) plist)
                (push val plist))
    (nreverse plist)))

(provide 'mevedel-pipeline)
;;; mevedel-pipeline.el ends here
