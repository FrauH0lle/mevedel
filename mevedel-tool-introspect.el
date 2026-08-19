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

;; `find-func'
(declare-function find-library-name "find-func" (library))

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

(defun mevedel-tool-introspect--library-source-check (_tool input)
  "Allow INPUT only when it names source inside a local `load-path'."
  (let ((name (plist-get input :library))
        source
        unsafe-predecessor-p)
    (if (and (stringp name)
             (not (string-empty-p name))
             (not (file-name-absolute-p name))
             (null (file-name-directory name))
             (progn
               (catch 'resolved
                 (dolist (directory load-path)
                   (cond
                    ((or (null directory)
                         (not (stringp directory))
                         (condition-case nil
                             (file-remote-p directory)
                           (error t)))
                     (setq unsafe-predecessor-p t))
                    ((file-directory-p directory)
                     (let* ((load-path (list directory))
                            (candidate
                             (ignore-errors (find-library-name name))))
                       (when candidate
                         (let ((canonical-directory
                                (ignore-errors (file-truename directory)))
                               (canonical-source
                                (ignore-errors (file-truename candidate))))
                           (if (and canonical-directory canonical-source
                                    (file-in-directory-p
                                     canonical-source canonical-directory))
                               (setq source canonical-source)
                             (setq unsafe-predecessor-p t))
                           (throw 'resolved nil))))))))
               (and source (not unsafe-predecessor-p))))
        'allow
      '(deny . "Library must resolve inside a local load-path entry"))))


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
  "Return bounded render metadata for introspection NAME, ARGS, and RESULT."
  (list :kind 'introspection
        :tool name
        :target (mevedel-tool-introspect--primary-value args)
        :body-kind (mevedel-tool-introspect--body-kind name)
        :lines (mevedel-tool-introspect--line-count result)
        :chars (length result)))

(defun mevedel-tool-introspect--render (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA."
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

(defconst mevedel-tool-introspect--registrations
  '(("symbol_exists" "Check if a symbol is interned in obarray." nil nil)
    ("load_paths" "Return user load-path entries." 20000 nil)
    ("features" "Check whether a feature is loaded or available." 20000 nil)
    ("manual_names" "List available info manuals." 20000 nil)
    ("manual_nodes" "List section nodes of an info manual." 20000 nil)
    ("manual_node_contents" "Read the contents of an info manual node."
     50000 nil)
    ("symbol_manual_section" "Find which manual section documents a symbol."
     50000 nil)
    ("function_completions" "List function names matching a prefix."
     20000 nil)
    ("command_completions" "List interactive command names matching a prefix."
     20000 nil)
    ("variable_completions" "List variable names matching a prefix."
     20000 nil)
    ("function_source" "Read the source code for a function or macro."
     30000 nil)
    ("variable_source" "Read the source code for a variable." 30000 nil)
    ("function_documentation" "Read the docstring for a function or macro."
     20000 nil)
    ("variable_documentation" "Read the docstring for a variable." 20000 nil)
    ("library_source" "Read the source code for a library." 50000
     mevedel-tool-introspect--library-source-check)
    ("variable_value" "Return a variable's global value (always asks)." 20000
     mevedel-tool-introspect--variable-value-check))
  "Wrapped introspection tool name, summary, result cap, and permission check.")

;;;###autoload
(defun mevedel-tool-introspect--register ()
  "Wrap the 16 `gptel-agent' introspection tools for mevedel.

Idempotent: any existing `mevedel-introspection' entries are purged
before wrapping, so repeat calls (e.g. during tests or reloads) are
safe."
  (maphash
   (lambda (key _tool)
     (when (equal (car key) "mevedel-introspection")
       (remhash key mevedel-tool--registry)))
   (copy-hash-table mevedel-tool--registry))
  (dolist (registration mevedel-tool-introspect--registrations)
    (pcase-let ((`(,name ,summary ,max-result-size ,check-permission)
                 registration))
      (mevedel-tool--register-wrap
       :source (gptel-get-tool (list "introspection" name))
       :summary summary
       :groups '(elisp)
       :read-only-p t
       :max-result-size max-result-size
       :check-permission check-permission
       :render-transform #'mevedel-tool-introspect--render-transform
       :renderer #'mevedel-tool-introspect--render))))

(provide 'mevedel-tool-introspect)
;;; mevedel-tool-introspect.el ends here
