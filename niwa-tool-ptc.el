;;; niwa-tool-ptc.el --- Programmatic Tool Calling tool -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The model-facing Programmatic Tool Calling adapter.  This module constructs
;; the request-local callable roster and prompt, delegates execution to the
;; closed driver in `niwa-ptc-driver', renders aggregate results, and
;; registers the ToolCall envelope with the ordinary tool registry.
;;
;; Nested calls are not provider-origin tool calls.  The envelope owns one
;; aggregate audit record listing each call, its arguments, and its outcome;
;; the view projects those children as collapsible rows without adding them
;; to provider history.

;;; Code:

(require 'niwa-engine)

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-tool-registry))

(require 'niwa-system)
(require 'niwa-execution-telemetry)

;; `niwa-utilities'
(declare-function niwa--duration-label "niwa-utilities" (seconds))
(autoload 'niwa--duration-label "niwa-utilities")

;; `gptel'
(defvar gptel-tools)

;; `niwa-ptc-driver'
(declare-function niwa-ptc-driver-run
                  "niwa-ptc-driver" (callback script roster &optional standalone-tools))

(autoload 'niwa-ptc-driver-run "niwa-ptc-driver")

;; `niwa-structs'
(declare-function niwa-request-ptc-primitives
                  "niwa-structs" (request))
(defvar niwa--current-request)

;; `niwa-tool-registry'
(declare-function niwa-tool-args "niwa-tool-registry" (tool))
(declare-function niwa-tool-call-name "niwa-tool-registry" (tool))
(declare-function niwa-tool-callable-p "niwa-tool-registry" (tool))
(declare-function niwa-tool-contextual-contract "niwa-tool-registry" (tool))
(declare-function niwa-tool-description "niwa-tool-registry" (tool))
(declare-function niwa-tool-for-call "niwa-tool-registry" (name))
(declare-function niwa-tool-for-gptel "niwa-tool-registry" (provider))
(declare-function niwa-tool-get "niwa-tool-registry" (name &optional category))
(declare-function niwa-tool-groups "niwa-tool-registry" (tool))
(declare-function niwa-tool-read-only-p "niwa-tool-registry" (tool))

;; `niwa-tools'
(declare-function niwa-tools--ctx-tool-catalog "niwa-tools" (ctx))
(declare-function niwa-tools--current-context "niwa-tools" ())
(declare-function niwa-tools-visibility "niwa-tools" (&optional owner))
(defvar niwa-tools--current-engine)


;;;; Roster

(defcustom niwa-ptc-composable-tools
  '("Read" "Glob" "Grep" "Bash" "ApplyPatch" "Eval"
    "XrefReferences" "XrefDefinitions" "Imenu" "Treesitter"
    "WebSearch" "WebFetch" "ListSkills"
    "TaskWrite" "TaskList"
    "Agent" "ListAgents" "InterruptAgent" "SendMessage"
    "WriteStdin" "ListExecutions" "StopExecution")
  "Tools allowed in composed expressions, subject to current capabilities.
Other available tools support standalone invocation.  Instruction-producing
and interaction tools require a model turn to consume their result."
  :type '(repeat string)
  :group 'niwa)

(defun niwa-tool-ptc--composable-p (name)
  "Return non-nil if expression tool NAME permits composition."
  (or (member name niwa-ptc-composable-tools)
      (when-let* ((tool (niwa-tool-for-call name)))
        (and (niwa-tool-read-only-p tool)
             (memq 'elisp (niwa-tool-groups tool))))))

(defcustom niwa-ptc-parallelism 4
  "Maximum number of nested calls one ToolCall batch may run concurrently."
  :type 'natnum
  :group 'niwa)

(defun niwa-tool-ptc--native-tools ()
  "Return the current request's native tools, or the buffer's before admission."
  (let* ((fsm (bound-and-true-p niwa-tools--current-engine))
         (info (and fsm (niwa-engine-info fsm))))
    (if fsm
        (plist-get info :tools)
      (bound-and-true-p gptel-tools))))

(defun niwa-tool-ptc--active-tool-names ()
  "Return canonical names of registered tools active in the current request."
  (delq nil
        (mapcar (lambda (tool)
                  (when-let* ((registered (niwa-tool-for-gptel tool)))
                    (when (niwa-tool-callable-p registered)
                      (niwa-tool-call-name registered))))
                (niwa-tool-ptc--native-tools))))

(defun niwa-tool-ptc--catalog-tool-names ()
  "Return canonical names in the current owner's discoverable catalog."
  (when-let* ((ctx (niwa-tools--current-context)))
    (delq nil
          (mapcar (lambda (entry)
                    (when-let* ((tool (niwa-tool-get (cadr (car entry))
                                                       (car (car entry)))))
                      (when (niwa-tool-callable-p tool)
                        (niwa-tool-call-name tool))))
                  (niwa-tools--ctx-tool-catalog ctx)))))

(defun niwa-tool-ptc--roster (&optional include-hidden)
  "Return callable names allowed by the current role and request.
INCLUDE-HIDDEN retains tools hidden by transient turn visibility, for role
inheritance.  Capability membership and skill restrictions still apply."
  (let ((available (delete-dups
                    (append (niwa-tool-ptc--active-tool-names)
                            (niwa-tool-ptc--catalog-tool-names))))
        (visible (niwa-tools-visibility
                  (bound-and-true-p niwa-tools--current-engine)))
        (restriction
         (if (bound-and-true-p niwa--current-request)
             (niwa-request-ptc-primitives niwa--current-request)
           :unrestricted)))
    (seq-filter (lambda (name)
                  (and (not (member name '("ToolCall" "ToolSearch")))
                       (or include-hidden (funcall visible name))
                       (or (eq restriction :unrestricted)
                           (member name restriction))))
                available)))

(defun niwa-tool-ptc--arg-type-name (type)
  "Return the compact guest-facing name for argument TYPE."
  (pcase type
    ((or 'path 'path-or-resource) "string")
    ((pred symbolp) (symbol-name type))
    (_ (format "%s" type))))

(defun niwa-tool-ptc--tool-declaration (name)
  "Return one compact ToolCall declaration for tool NAME."
  (let* ((tool (niwa-tool-for-call name))
         (args
          (mapcar
           (lambda (spec)
             (let ((arg (format ":%s %s"
                                (car spec)
                                (niwa-tool-ptc--arg-type-name (cadr spec)))))
               (if (eq (nth 2 spec) :required) arg (format "[%s]" arg))))
           (and tool (niwa-tool-args tool))))
         (summary (and tool (or (cdr (niwa-tool-contextual-contract tool))
                                (niwa-tool-description tool)))))
    (format "- (%s%s)%s%s"
            name
            (if args (concat " " (string-join args " ")) "")
            (if summary (concat " - " (string-trim summary)) "")
            (if (niwa-tool-ptc--composable-p name) "" " [standalone only]"))))

(defun niwa-tool-ptc--call-template (name)
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
           (niwa-tool-args (niwa-tool-for-call name)) "")
          ")"))

;;;; Driver adapter

(defun niwa-tool-ptc--handler (callback args)
  "Run the script in ARGS through the current request's nested tool roster."
  (let ((roster (niwa-tool-ptc--roster)))
    (niwa-ptc-driver-run
     callback (plist-get args :expression) roster
     (seq-remove #'niwa-tool-ptc--composable-p roster))))


;;;; Rendering

(defcustom niwa-tool-ptc-result-collapse-line-threshold 10
  "ToolCall returned values longer than this many lines fold to a row.
The folded `Returned' row follows the nested call rows, defaults to
collapsed, and expands to the complete returned value.  Zero keeps
every returned value inline in the block body."
  :type 'integer
  :group 'niwa)

(defun niwa-tool-ptc--render (_name _args result render-data)
  "Render a settled ToolCall call.

The body carries only what the script returned.  Every nested call
becomes a `:child-calls' row that the view renders through that tool's
own renderer, so a nested Grep gets Grep's header and `grep-mode' body
instead of one flat dump fontified in a single mode.  A completed
script's returned value longer than
`niwa-tool-ptc-result-collapse-line-threshold' lines moves out of
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
                           (let ((status (plist-get call :status))
                                 (facts (plist-get call :render-data)))
                             (or (eq status 'denied)
                                 (and (eq status 'error)
                                      (not (and (equal (plist-get call :tool)
                                                       "WriteStdin")
                                                (memq (plist-get facts :execution-control)
                                                      '(input poll))
                                                (plist-get facts :control-succeeded-p)))))))
                         calls))
           (returned-value
            (and (not (bound-and-true-p niwa-tool-render-summary-only))
                 (not live-p)
                 (if (stringp result) result (format "%S" result))))
           (fold-returned-p
            (and returned-value
                 (eq outcome 'completed)
                 (> niwa-tool-ptc-result-collapse-line-threshold 0)
                 (> (length (split-string returned-value "\n"))
                    niwa-tool-ptc-result-collapse-line-threshold))))
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
                                  (format " \u00b7 %s"
                                          (niwa--duration-label elapsed))
                                "")
                              (if (> error-count 0)
                                  (format " \u00b7 %d failed" error-count)
                                "")
                              outcome))
            :body (and returned-value
                       (not fold-returned-p)
                       (concat (if (eq outcome 'completed) "Returned:\n" "")
                               returned-value))
            :child-calls (and (not (bound-and-true-p niwa-tool-render-summary-only))
                              (not live-p)
                              (append calls
                                      (when fold-returned-p
                                        (list
                                         (list :id "returned"
                                               :order (length calls)
                                               :tool "Returned"
                                               :status 'success
                                               :result returned-value)))))
            :status (cond
                     (live-p 'running)
                     ((not (eq outcome 'completed)) 'error)
                     ((or (> error-count 0)
                          (cl-some (lambda (call)
                                     (or (eq (plist-get call :status) 'warning)
                                         (eq (niwa-execution-telemetry-sandbox-summary-class
                                              (plist-get (plist-get call :render-data)
                                                         :sandbox-summary))
                                             'warning)))
                                   calls))
                      'warning)
                     (t 'success))
            :initially-collapsed-p t))))


;;;; Registration

(defun niwa-tool-ptc--register ()
  "Register the Programmatic Tool Calling tool."
  (niwa-define-tool
   :name "ToolCall"
   :description "Call one or more tools using the calling expressions supplied by ToolSearch."
   :summary "Call a specialist or compose tool calls in a closed Lisp expression."
   :prompt-file "prompts/tools/ptc.md"
   :handler #'niwa-tool-ptc--handler
   :args ((expression string :required
                  "The orchestration script. See the tool description for the accepted dialect."))
   :async-p t
   :category "niwa"
   :groups (util)
   ;; The envelope itself never modifies state; every nested call carries
   ;; its own authority, so read-only request rules deny mutating children
   ;; individually rather than the envelope wholesale.
   :read-only-p t
   :max-result-size 30000
   :renderer #'niwa-tool-ptc--render))

(provide 'niwa-tool-ptc)
;;; niwa-tool-ptc.el ends here
