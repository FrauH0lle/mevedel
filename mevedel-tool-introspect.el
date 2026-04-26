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
    :read-only-p t)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "load_paths"))
    :summary "Return user load-path entries."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "features"))
    :summary "Check whether a feature is loaded or available."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  ;; Manuals
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_names"))
    :summary "List available info manuals."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_nodes"))
    :summary "List section nodes of an info manual."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "manual_node_contents"))
    :summary "Read the contents of an info manual node."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "symbol_manual_section"))
    :summary "Find which manual section documents a symbol."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000)

  ;; Completions
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_completions"))
    :summary "List function names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "command_completions"))
    :summary "List interactive command names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_completions"))
    :summary "List variable names matching a prefix."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  ;; Source / documentation
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_source"))
    :summary "Read the source code for a function or macro."
    :groups (elisp)
    :read-only-p t
    :max-result-size 30000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_source"))
    :summary "Read the source code for a variable."
    :groups (elisp)
    :read-only-p t
    :max-result-size 30000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "function_documentation"))
    :summary "Read the docstring for a function or macro."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_documentation"))
    :summary "Read the docstring for a variable."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000)

  ;; Library source (bounded by load-path)
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "library_source"))
    :summary "Read the source code for a library."
    :groups (elisp)
    :read-only-p t
    :max-result-size 50000
    :check-permission #'mevedel-tool-introspect--library-source-check)

  ;; Variable value (sensitive -- always-ask)
  (mevedel-define-tool
    :wrap (gptel-get-tool '("introspection" "variable_value"))
    :summary "Return a variable's global value (always asks)."
    :groups (elisp)
    :read-only-p t
    :max-result-size 20000
    :check-permission #'mevedel-tool-introspect--variable-value-check))

(provide 'mevedel-tool-introspect)
;;; mevedel-tool-introspect.el ends here
