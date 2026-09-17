;;; mevedel-tool-ptc.el --- Programmatic Tool Calling tool -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The model-facing Programmatic Tool Calling adapter.  This module constructs
;; the request-local callable roster and prompt, delegates execution to the
;; closed driver in `mevedel-ptc-driver', renders aggregate results, and
;; registers the ToolCall envelope with the ordinary tool registry.
;;
;; Nested calls are not provider-origin tool calls.  The envelope owns one
;; aggregate audit record listing each call, its arguments, and its outcome;
;; the view projects those children as collapsible rows without adding them
;; to provider history.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

(require 'mevedel-system)

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(declare-function gptel-tool-category "ext:gptel-request" (tool))
(declare-function gptel-tool-name "ext:gptel-request" (tool))

;; `mevedel-ptc-driver'
(declare-function mevedel-ptc-driver-run
                  "mevedel-ptc-driver" (callback script roster &optional standalone-tools))

(autoload 'mevedel-ptc-driver-run "mevedel-ptc-driver")

;; `mevedel-structs'
(declare-function mevedel-request-ptc-primitives
                  "mevedel-structs" (request))
(defvar mevedel--current-request)

;; `mevedel-tool-registry'
(declare-function mevedel-tool-args "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-call-name "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-callable-p "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-description "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-for-call "mevedel-tool-registry" (name))
(declare-function mevedel-tool-get "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-groups "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-read-only-p "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-summary "mevedel-tool-registry" (tool))

;; `mevedel-tools'
(declare-function mevedel-tools--ctx-tool-catalog "mevedel-tools" (ctx))
(declare-function mevedel-tools--current-context "mevedel-tools" ())
(defvar mevedel-tools--current-fsm)


;;;; Roster

(defcustom mevedel-ptc-composable-tools
  '("Read" "Glob" "Grep" "Bash" "ApplyPatch" "Eval"
    "XrefReferences" "XrefDefinitions" "Imenu" "Treesitter"
    "WebSearch" "WebFetch" "ListSkills"
    "TaskCreate" "TaskUpdate" "TaskNote" "TaskList" "TaskGet"
    "Agent" "FollowupAgent" "ListAgents" "InterruptAgent" "SendMessage"
    "WriteStdin" "ListExecutions" "StopExecution")
  "Tools allowed in composed expressions, subject to current capabilities.
Other available tools support standalone invocation.  Instruction-producing
and interaction tools require a model turn to consume their result."
  :type '(repeat string)
  :group 'mevedel)

(defun mevedel-tool-ptc--composable-p (name)
  "Return non-nil if expression tool NAME permits composition."
  (or (member name mevedel-ptc-composable-tools)
      (when-let* ((tool (mevedel-tool-for-call name)))
        (and (mevedel-tool-read-only-p tool)
             (memq 'elisp (mevedel-tool-groups tool))))))

(defcustom mevedel-ptc-parallelism 4
  "Maximum number of nested calls one ToolCall batch may run concurrently."
  :type 'natnum
  :group 'mevedel)

(defun mevedel-tool-ptc--active-tool-names ()
  "Return canonical names of registered tools active in the current request."
  (let* ((fsm (bound-and-true-p mevedel-tools--current-fsm))
         (tools (or (and fsm (plist-get (gptel-fsm-info fsm) :tools))
                    (bound-and-true-p gptel-tools))))
    (delq nil
          (mapcar (lambda (tool)
                    (when-let* ((registered (mevedel-tool-get
                                            (gptel-tool-name tool)
                                            (gptel-tool-category tool))))
                      (when (mevedel-tool-callable-p registered)
                        (mevedel-tool-call-name registered))))
                  tools))))

(defun mevedel-tool-ptc--catalog-tool-names ()
  "Return canonical names in the current owner's discoverable catalog."
  (when-let* ((ctx (mevedel-tools--current-context)))
    (delq nil
          (mapcar (lambda (entry)
                    (when-let* ((tool (mevedel-tool-get (cadr (car entry))
                                                       (car (car entry)))))
                      (when (mevedel-tool-callable-p tool)
                        (mevedel-tool-call-name tool))))
                  (mevedel-tools--ctx-tool-catalog ctx)))))

(defun mevedel-tool-ptc--roster ()
  "Return callable names allowed by the current role and request."
  (let ((available (delete-dups
                    (append (mevedel-tool-ptc--active-tool-names)
                            (mevedel-tool-ptc--catalog-tool-names))))
        (restriction
         (if (bound-and-true-p mevedel--current-request)
             (mevedel-request-ptc-primitives mevedel--current-request)
           :unrestricted)))
    (seq-filter (lambda (name)
                  (and (not (member name '("ToolCall" "ToolSearch")))
                       (or (eq restriction :unrestricted)
                           (member name restriction))))
                available)))

(defun mevedel-tool-ptc--arg-type-name (type)
  "Return the compact guest-facing name for argument TYPE."
  (pcase type
    ((or 'path 'path-or-resource) "string")
    ((pred symbolp) (symbol-name type))
    (_ (format "%s" type))))

(defun mevedel-tool-ptc--tool-declaration (name)
  "Return one compact ToolCall declaration for tool NAME."
  (let* ((tool (mevedel-tool-for-call name))
         (args
          (mapcar
           (lambda (spec)
             (let ((arg (format ":%s %s"
                                (car spec)
                                (mevedel-tool-ptc--arg-type-name (cadr spec)))))
               (if (eq (nth 2 spec) :required) arg (format "[%s]" arg))))
           (and tool (mevedel-tool-args tool))))
         (summary (and tool (or (mevedel-tool-summary tool)
                                (mevedel-tool-description tool)))))
    (format "- (%s%s)%s"
            name
            (if args (concat " " (string-join args " ")) "")
            (if summary (concat " - " (string-trim summary)) ""))))

(defun mevedel-tool-ptc--call-template (name)
  "Return a single-call template for NAME with required argument placeholders."
  (concat "(" name
          (mapconcat
           (lambda (spec)
             (if (not (eq (nth 2 spec) :required)) ""
               (format " :%s %s" (car spec)
                       (pcase (cadr spec)
                         ((or 'number 'integer) "0")
                         ('boolean "t")
                         ('array "[]")
                         ('object "'()")
                         (_ (prin1-to-string (format "<%s>" (car spec))))))))
           (mevedel-tool-args (mevedel-tool-for-call name)) "")
          ")"))

;;;; Driver adapter

(defun mevedel-tool-ptc--handler (callback args)
  "Run the script in ARGS through the current request's nested tool roster."
  (let ((roster (mevedel-tool-ptc--roster)))
    (mevedel-ptc-driver-run
     callback (plist-get args :expression) roster
     (seq-remove #'mevedel-tool-ptc--composable-p roster))))


;;;; Rendering

(defcustom mevedel-tool-ptc-result-collapse-line-threshold 10
  "ToolCall returned values longer than this many lines fold to a row.
The folded `Returned' row follows the nested call rows, defaults to
collapsed, and expands to the complete returned value.  Zero keeps
every returned value inline in the block body."
  :type 'integer
  :group 'mevedel)

(defun mevedel-tool-ptc--render (_name _args result render-data)
  "Render a settled ToolCall call.

The body carries only what the script returned.  Every nested call
becomes a `:child-calls' row that the view renders through that tool's
own renderer, so a nested Grep gets Grep's header and `grep-mode' body
instead of one flat dump fontified in a single mode.  A completed
script's returned value longer than
`mevedel-tool-ptc-result-collapse-line-threshold' lines moves out of
the body into a trailing collapsed `Returned' row that reuses the same
nested-row machinery."
  (when (eq (plist-get render-data :kind) 'ptc)
    (let* ((calls (plist-get render-data :calls))
           (live-p (plist-get render-data :live-p))
           (active-tool (plist-get render-data :active-tool))
           (known-total (plist-get render-data :known-total))
           (permission-waits (plist-get render-data :permission-waits))
           (outcome (plist-get render-data :outcome))
           (elapsed (plist-get render-data :elapsed-seconds))
           (error-count
            (cl-count-if (lambda (call)
                           (memq (plist-get call :status) '(error denied)))
                         calls))
           (returned-value
            (and (not live-p)
                 (if (stringp result) result (format "%S" result))))
           (fold-returned-p
            (and returned-value
                 (eq outcome 'completed)
                 (> mevedel-tool-ptc-result-collapse-line-threshold 0)
                 (> (length (split-string returned-value "\n"))
                    mevedel-tool-ptc-result-collapse-line-threshold))))
      (list :header (if live-p
                        (format "ToolCall: %d%s completed%s%s%s"
                                (or (plist-get render-data :completed-count)
                                    (length calls))
                                (if known-total (format "/%d" known-total) "")
                                (if active-tool
                                    (format ", %s active" active-tool)
                                  "")
                                (if (> error-count 0)
                                    (format ", %d failed" error-count)
                                  "")
                                (if permission-waits
                                    (format ", awaiting permission for %s"
                                            (string-join permission-waits ", "))
                                  ""))
                      (format "ToolCall: %d call%s%s%s (%s)"
                              (length calls)
                              (if (= (length calls) 1) "" "s")
                              (if (numberp elapsed)
                                  (format " \u00b7 %.1fs" elapsed)
                                "")
                              (if (> error-count 0)
                                  (format " \u00b7 %d failed" error-count)
                                "")
                              outcome))
            :body (and returned-value
                       (not fold-returned-p)
                       (concat (if (eq outcome 'completed) "Returned:\n" "")
                               returned-value))
            :child-calls (and (not live-p)
                              (append calls
                                      (when fold-returned-p
                                        (list
                                         (list :id "returned"
                                               :order (length calls)
                                               :tool "Returned"
                                               :status 'success
                                               :result returned-value)))))
            :status (if live-p 'running
                      (if (eq outcome 'completed) 'success 'error))
            :initially-collapsed-p t))))


;;;; Registration

(defun mevedel-tool-ptc--register ()
  "Register the Programmatic Tool Calling tool."
  (mevedel-define-tool
   :name "ToolCall"
   :description "Call one or more tools using the calling expressions supplied by ToolSearch."
   :summary "Call a specialist or compose tool calls in a closed Lisp expression."
   :prompt-file "prompts/tools/ptc.md"
   :handler #'mevedel-tool-ptc--handler
   :args ((expression string :required
                  "The orchestration script. See the tool description for the accepted dialect."))
   :async-p t
   :category "mevedel"
   :groups (util)
   ;; The envelope itself never modifies state; every nested call carries
   ;; its own authority, so read-only request rules deny mutating children
   ;; individually rather than the envelope wholesale.
   :read-only-p t
   :max-result-size 30000
   :renderer #'mevedel-tool-ptc--render))

(provide 'mevedel-tool-ptc)
;;; mevedel-tool-ptc.el ends here
