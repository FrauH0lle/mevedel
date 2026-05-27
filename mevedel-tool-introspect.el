;;; mevedel-tool-introspect.el -- Elisp introspection tools -*- lexical-binding: t -*-

;;; Commentary:

;; Wraps the 16 `gptel-agent' introspection tools as mevedel tools so
;; they flow through the pipeline (permissions, persistence, display)
;; and can be pulled in via `(:deferred elisp)' from presets and
;; agents.  The source structs in gptel's `"introspection"' category
;; are left untouched; this file registers copies under
;; `"mevedel-introspection"' whose `:function' dispatches through the
;; pipeline.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'gptel-agent-tools-introspection)
(require 'subr-x)

;; `gptel-request'
(declare-function gptel-get-tool "ext:gptel-request" (path))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--register-wrap
                  "mevedel-tool-registry" (&rest keys))
(defvar mevedel-tool--registry)


;;
;;; Permission helpers

(defun mevedel-tool-introspect--variable-value-check (_tool _input)
  "Always-ask permission for `variable_value'.

Variables can hold auth tokens and other sensitive state, so every
call prompts the user regardless of permission mode."
  'ask)

(defun mevedel-tool-introspect--library-source-check (_tool _input)
  "Always-allow permission for `library_source'.

`find-library-name' resolves paths only under `load-path', so the
access surface is bounded to libraries Emacs knows about.  Mirrors
the upstream no-permission behaviour."
  'allow)


;;
;;; Renderers

(defun mevedel-tool-introspect--primary-value (args)
  "Return the first meaningful primary value from introspection ARGS."
  (catch 'found
    (dolist (key '(:symbol :function :variable :library :feature
                   :manual :manual_name :node
                   :function_prefix :command_prefix :variable_prefix))
      (let ((value (plist-get args key)))
        (when (and (stringp value) (not (string-empty-p value)))
          (throw 'found value))))
    nil))

(defun mevedel-tool-introspect--body-kind (name)
  "Return a display kind symbol for introspection tool NAME."
  (cond
   ((member name '("function_source" "variable_source" "library_source"))
    'source)
   ((member name '("function_documentation" "variable_documentation"
                   "manual_node_contents" "symbol_manual_section"))
    'documentation)
   ((string-suffix-p "_completions" name) 'completions)
   (t 'result)))

(defun mevedel-tool-introspect--line-count (result)
  "Return non-empty line count for RESULT."
  (if (stringp result)
      (length (split-string result "\n" t))
    0))

(defun mevedel-tool-introspect--render-transform (name args result)
  "Return bounded render metadata for wrapped introspection RESULT."
  (list :kind 'introspection
        :tool name
        :target (mevedel-tool-introspect--primary-value args)
        :body-kind (mevedel-tool-introspect--body-kind name)
        :lines (mevedel-tool-introspect--line-count result)
        :chars (length result)))

(defun mevedel-tool-introspect--render (name args result render-data)
  "Rendering plist for wrapped introspection tools."
  (when (stringp result)
    (let* ((target (or (plist-get render-data :target)
                       (mevedel-tool-introspect--primary-value args)))
           (kind (or (plist-get render-data :body-kind)
                     (mevedel-tool-introspect--body-kind name)))
           (lines (or (plist-get render-data :lines)
                      (mevedel-tool-introspect--line-count result)))
           (shown-name (replace-regexp-in-string "_" " " (or name "introspect")))
           (status (and (string-prefix-p "Error:" result) 'error))
           (mode (and (memq kind '(source))
                      'emacs-lisp-mode)))
      (list :header (format "%s: %s%s (%d %s)"
                            shown-name
                            kind
                            (if target (format " %s" target) "")
                            lines
                            (if (= lines 1) "line" "lines"))
            :body result
            :body-mode mode
            :status status
            :initially-collapsed-p t))))


;;
;;; Registration

;;;###autoload
(defun mevedel-tool-introspect--register ()
  "Wrap the 16 gptel-agent introspection tools for mevedel.

Idempotent: any existing `mevedel-introspection' entries are purged
before wrapping, so repeat calls (e.g. during tests or reloads) are
safe."
  (maphash
   (lambda (key _tool)
     (when (equal (car key) "mevedel-introspection")
       (remhash key mevedel-tool--registry)))
   (copy-hash-table mevedel-tool--registry))

  ;; Existence / enumeration
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "symbol_exists"))
    :summary "Check if a symbol is interned in obarray."
    :groups (elisp)
    :read-only-p t
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "load_paths"))
    :summary "Return user load-path entries."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "features"))
    :summary "Check whether a feature is loaded or available."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  ;; Manuals
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_names"))
    :summary "List available info manuals."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_nodes"))
    :summary "List section nodes of an info manual."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_node_contents"))
    :summary "Read the contents of an info manual node."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "symbol_manual_section"))
    :summary "Find which manual section documents a symbol."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  ;; Completions
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_completions"))
    :summary "List function names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "command_completions"))
    :summary "List interactive command names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_completions"))
    :summary "List variable names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  ;; Source / documentation
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_source"))
    :summary "Read the source code for a function or macro."
    :groups (elisp)
    :read-only-p t
    :max-result-size 30000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_source"))
    :summary "Read the source code for a variable."
    :groups (elisp)
    :read-only-p t
    :max-result-size 30000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_documentation"))
    :summary "Read the docstring for a function or macro."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_documentation"))
    :summary "Read the docstring for a variable."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  ;; Library source (bounded by load-path)
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "library_source"))
    :summary "Read the source code for a library."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000
    :check-permission #'mevedel-tool-introspect--library-source-check
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render)

  ;; Variable value (sensitive -- always-ask)
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_value"))
    :summary "Return a variable's global value (always asks)."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :check-permission #'mevedel-tool-introspect--variable-value-check
    :render-transform #'mevedel-tool-introspect--render-transform
    :renderer #'mevedel-tool-introspect--render))

(provide 'mevedel-tool-introspect)
;;; mevedel-tool-introspect.el ends here
