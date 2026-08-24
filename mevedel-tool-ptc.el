;;; mevedel-tool-ptc.el --- Programmatic Tool Calling tool -*- lexical-binding: t -*-

;;; Commentary:
;;
;; The model-facing Programmatic Tool Calling adapter.  This module constructs
;; the request-local callable roster and prompt, delegates execution to the
;; closed driver in `mevedel-ptc-driver', renders aggregate results, and
;; registers the ToolScript envelope with the ordinary tool registry.
;;
;; Nested calls are not provider-origin tool calls.  The envelope owns one
;; aggregate audit record listing each call, its arguments, and its outcome;
;; the view projects those children as collapsible rows without adding them
;; to provider history.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel'
(defvar gptel-tools)

;; `gptel-request'
(declare-function gptel--make-tool "ext:gptel-request" (&rest spec))
(declare-function gptel-fsm-info "ext:gptel-request" (fsm))
(declare-function gptel-tool-args "ext:gptel-request" (tool))
(declare-function gptel-tool-async "ext:gptel-request" (tool))
(declare-function gptel-tool-category "ext:gptel-request" (tool))
(declare-function gptel-tool-confirm "ext:gptel-request" (tool))
(declare-function gptel-tool-function "ext:gptel-request" (tool))
(declare-function gptel-tool-include "ext:gptel-request" (tool))
(declare-function gptel-tool-name "ext:gptel-request" (tool))

;; `help-fns'
(declare-function help-function-arglist
                  "help-fns" (function &optional preserve-names))

;; `mevedel-ptc-driver'
(declare-function mevedel-ptc-driver-run
                  "mevedel-ptc-driver" (callback script roster))

;; `mevedel-ptc-interpreter'
(defvar mevedel-ptc-pure-primitives)

;; `mevedel-structs'
(declare-function mevedel-request-ptc-primitives
                  "mevedel-structs" (request))
(defvar mevedel--current-request)

;; `mevedel-system'
(declare-function mevedel-system-render-template
                  "mevedel-system" (template replacements))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-args "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-description "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-ensure
                  "mevedel-tool-registry" (name &optional category))
(declare-function mevedel-tool-name "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-prompt "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-summary "mevedel-tool-registry" (tool))
(defvar mevedel-tool-registry--source-dir)

;; `mevedel-tools'
(declare-function mevedel-tools--ctx-deferred-set "mevedel-tools" (ctx))
(declare-function mevedel-tools--current-deferred-context "mevedel-tools" ())
(declare-function mevedel-tools--request-data-set-tools "mevedel-tools" (info))
(defvar mevedel-tools--current-fsm)


;;;; Roster

(defconst mevedel-tool-ptc--dialect-manual-path
  (file-name-concat mevedel-tool-registry--source-dir
                    "docs" "ptc-dialect.md")
  "Absolute path to the installed ToolScript dialect manual.")

(defcustom mevedel-ptc-primitive-tools
  '("Read" "Glob" "Grep" "Bash"
    "XrefReferences" "XrefDefinitions" "Imenu" "Treesitter")
  "Tools a script may call, subject to also being active in the request.

An allowlist rather than an exclusion list, so a newly registered tool is
never a script primitive by accident.  Deliberately absent: ApplyPatch,
Eval, Ask, the Agent family, and every other interaction-owning tool.
ToolScript itself is absent, so scripts do not nest."
  :type '(repeat string)
  :group 'mevedel)

(defcustom mevedel-ptc-parallelism 4
  "Maximum number of nested calls one ToolScript batch may run concurrently."
  :type 'natnum
  :group 'mevedel)

(defvar mevedel-tool-ptc--pure-primitive-reference-cache nil
  "Cached guest signatures for the closed pure-primitive table.")

(defun mevedel-tool-ptc--format-arglist (arglist)
  "Return guest-facing arguments for Emacs ARGLIST."
  (let (optional rest rendered)
    (dolist (arg arglist (string-join (nreverse rendered) " "))
      (pcase arg
        ('&optional (setq optional t))
        ('&rest (setq rest t))
        (_
         (let ((name (downcase (symbol-name (if (consp arg) (car arg) arg)))))
           (push (cond
                  (rest (concat name "..."))
                  (optional (format "[%s]" name))
                  (t name))
                 rendered)
           (setq rest nil)))))))

(defun mevedel-tool-ptc--pure-primitive-reference ()
  "Return cached signatures for every allowed pure primitive."
  (require 'mevedel-ptc-interpreter)
  (or mevedel-tool-ptc--pure-primitive-reference-cache
      (setq mevedel-tool-ptc--pure-primitive-reference-cache
            (string-join
             (mapcar
              (lambda (entry)
                (let ((args (mevedel-tool-ptc--format-arglist
                             (help-function-arglist (cdr entry) t))))
                  (format "- (%s%s)"
                          (car entry)
                          (if (string-empty-p args) "" (concat " " args)))))
              mevedel-ptc-pure-primitives)
             "\n"))))

(defun mevedel-tool-ptc--active-tool-names ()
  "Return the names of tools active in the current request."
  (let* ((fsm (bound-and-true-p mevedel-tools--current-fsm))
         (tools (or (and fsm (plist-get (gptel-fsm-info fsm) :tools))
                    (bound-and-true-p gptel-tools))))
    (delete-dups
     (delq nil (mapcar (lambda (tool) (ignore-errors (gptel-tool-name tool)))
                       tools)))))

(defun mevedel-tool-ptc--deferred-tool-names ()
  "Return the names of tools deferred for the current request."
  (unless (fboundp 'mevedel-tools--current-deferred-context)
    (require 'mevedel-tools))
  (when-let* ((ctx (ignore-errors (mevedel-tools--current-deferred-context))))
    (delq nil (mapcar (lambda (entry) (and (consp (car entry)) (cadr (car entry))))
                      (mevedel-tools--ctx-deferred-set ctx)))))

(defun mevedel-tool-ptc--roster ()
  "Return the tool names this script may call.

Deferred tools count as available.  Deferral is a prompt-budget decision,
not an authority or availability one: the pipeline never consults it, so
a deferred tool executes normally."
  (let ((available (append (mevedel-tool-ptc--active-tool-names)
                           (mevedel-tool-ptc--deferred-tool-names)))
        (restriction
         (if (bound-and-true-p mevedel--current-request)
             (mevedel-request-ptc-primitives mevedel--current-request)
           :unrestricted)))
    (seq-filter (lambda (name)
                  (and (member name available)
                       (or (eq restriction :unrestricted)
                           (member name restriction))))
                mevedel-ptc-primitive-tools)))

(defun mevedel-tool-ptc--arg-type-name (type)
  "Return the compact guest-facing name for argument TYPE."
  (pcase type
    ((or 'path 'path-or-resource) "string")
    ((pred symbolp) (symbol-name type))
    (_ (format "%s" type))))

(defun mevedel-tool-ptc--tool-declaration (name)
  "Return one compact ToolScript declaration for tool NAME."
  (let* ((tool (mevedel-tool-ensure name))
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

(defun mevedel-tool-ptc--request-description (fsm)
  "Return the ToolScript description for FSM's effective callable roster."
  (require 'mevedel-system)
  (let* ((mevedel-tools--current-fsm fsm)
         (tool (mevedel-tool-ensure "ToolScript"))
         (roster (mevedel-tool-ptc--roster)))
    (concat (mevedel-system-render-template
             (mevedel-tool-prompt tool)
             `(("PTC_DIALECT_MANUAL_PATH" .
                ,mevedel-tool-ptc--dialect-manual-path)))
            "\n\n## Pure data operations\n\n"
            (mevedel-tool-ptc--pure-primitive-reference)
            "\n\n## Tools available in this request\n\n"
            (if roster
                (string-join (mapcar #'mevedel-tool-ptc--tool-declaration roster)
                             "\n")
              "No nested tools are available."))))

(defun mevedel-tool-ptc--handle-description (fsm)
  "WAIT-state handler: give FSM's ToolScript tool its effective nested roster."
  (let* ((info (gptel-fsm-info fsm))
         (tools (plist-get info :tools))
         (ptc (seq-find (lambda (tool)
                          (equal (gptel-tool-name tool) "ToolScript"))
                        tools)))
    (when ptc
      (let ((copy
             (gptel--make-tool
              :function (gptel-tool-function ptc)
              :name (gptel-tool-name ptc)
              :description (mevedel-tool-ptc--request-description fsm)
              :args (copy-tree (gptel-tool-args ptc) t)
              :async (gptel-tool-async ptc)
              :category (gptel-tool-category ptc)
              :confirm (gptel-tool-confirm ptc)
              :include (gptel-tool-include ptc))))
        (plist-put info :tools
                   (mapcar (lambda (tool) (if (eq tool ptc) copy tool)) tools))
        (mevedel-tools--request-data-set-tools info)))))


;;;; Driver adapter

(defun mevedel-tool-ptc--handler (callback args)
  "Run the script in ARGS through the current request's nested tool roster."
  (require 'mevedel-ptc-driver)
  (mevedel-ptc-driver-run
   callback (plist-get args :script) (mevedel-tool-ptc--roster)))


;;;; Rendering

(defun mevedel-tool-ptc--render (_name _args result render-data)
  "Render a settled ToolScript call.

The body carries only what the script returned.  Every nested call
becomes a `:child-calls' row that the view renders through that tool's
own renderer, so a nested Grep gets Grep's header and `grep-mode' body
instead of one flat dump fontified in a single mode."
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
                         calls)))
      (list :header (if live-p
                        (format "ToolScript: %d%s completed%s%s%s"
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
                      (format "ToolScript: %d call%s%s%s (%s)"
                              (length calls)
                              (if (= (length calls) 1) "" "s")
                              (if (numberp elapsed)
                                  (format " \u00b7 %.1fs" elapsed)
                                "")
                              (if (> error-count 0)
                                  (format " \u00b7 %d failed" error-count)
                                "")
                              outcome))
            :body (and (not live-p)
                       (concat (if (eq outcome 'completed) "Returned:\n" "")
                               (if (stringp result)
                                   result
                                 (format "%S" result))))
            :child-calls (and (not live-p) calls)
            :status (if live-p 'running
                      (if (eq outcome 'completed) 'success 'error))
            :initially-collapsed-p t))))


;;;; Registration

(defun mevedel-tool-ptc--register ()
  "Register the Programmatic Tool Calling tool."
  (mevedel-define-tool
   :name "ToolScript"
   :description "Run an orchestration script that calls other tools."
   :summary "Orchestrate several tool calls in one turn with a small Lisp script."
   :prompt-file "tools/ptc.md"
   :handler #'mevedel-tool-ptc--handler
   :args ((script string :required
                  "The orchestration script. See the tool description for the accepted dialect."))
   :async-p t
   :category "mevedel"
   :groups (util)
   :max-result-size 30000
   :renderer #'mevedel-tool-ptc--render))

(provide 'mevedel-tool-ptc)
;;; mevedel-tool-ptc.el ends here
