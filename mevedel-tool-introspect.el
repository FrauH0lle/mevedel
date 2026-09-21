;;; mevedel-tool-introspect.el -- Elisp introspection tools -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Karthik Chikmagalur
;; Tool handlers adapted from gptel-agent-tools-introspection.el,
;; originally adapted from ragmacs.el by Positron Solutions.

;;; Commentary:

;; Native Emacs introspection tools with mevedel permissions, result limits,
;; persistence and rendering.  Available through the discoverable elisp group.

;;; Code:

(eval-when-compile
  (require 'mevedel-tool-registry))

(require 'info)
(require 'info-look)
(require 'find-func)
(require 'cus-edit)
(require 'orderless)
(require 'subr-x)

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--positional-to-plist
                  "mevedel-tool-registry" (raw-args specs))
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))


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
;;; Handlers

(defun mevedel-tool-introspect--manual-node-contents (manual node)
  "Return contents of NODE in Info MANUAL."
  (save-window-excursion
    (Info-goto-node (format "(%s)%s" manual node))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mevedel-tool-introspect--symbol-in-manual (symbol)
  "Return the Info documentation for SYMBOL, if it exists."
  (when-let* ((symbol (intern-soft symbol)))
    (let* (buffer
           (remember (lambda (shown alist)
                       (display-buffer-no-window (setq buffer shown) alist)))
           (display-buffer-overriding-action
            `(,remember (allow-no-window . t))))
      (info-lookup-symbol symbol #'emacs-lisp-mode)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mevedel-tool-introspect--library-source (library-name)
  "Return the source code of LIBRARY-NAME as a string."
  (with-temp-buffer
    (insert-file-contents (find-library-name library-name))
    (buffer-string)))

(defun mevedel-tool-introspect--source (symbol &optional type)
  "Return source for SYMBOL, or nil if its definition is not found.
TYPE is nil for functions or defvar for variables."
  (mevedel-tool--with-quiet-file-visit
    (when-let* ((callable (intern-soft symbol))
                (save-silently t)
                (vc-follow-symlinks t)
                (location (find-definition-noselect callable type)))
      (with-current-buffer (car location)
        (save-excursion
          (goto-char (cdr location))
          (buffer-substring-no-properties
           (point)
           (progn
             (if (null type)
                 (end-of-defun)
               (cond ((derived-mode-p 'c-mode)
                      (forward-sexp 2)
                      (forward-char))
                     ((derived-mode-p 'emacs-lisp-mode) (forward-sexp))
                     (t (error "Unexpected file mode"))))
             (point))))))))


;;
;;; Registration

;;;###autoload
(defun mevedel-tool-introspect--register ()
  "Register the 16 native Emacs introspection tools."
  (mevedel-define-tool
   :name "symbol_exists"
   :description "Check if a symbol is interned in obarray."
   :summary "Check whether a symbol is interned in the running Emacs."
   :prompt-file "prompts/tools/symbol_exists.md"
   :handler (lambda (args)
	      (list :result
		    (let ((name (plist-get args :symbol))) (intern-soft name))))
   :args ((symbol string :required
		  "A symbol that will be in `obarray' if they actually exist"))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size nil
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "load_paths"
   :description "Return user load-path entries."
   :summary "List the running Emacs's library load-path entries."
   :prompt-file "prompts/tools/load_paths.md"
   :handler (lambda (_args) (list :result (string-join load-path "\n")))
   :args nil
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "features"
   :description "Check whether a feature is loaded or available."
   :summary "Check whether an Emacs feature is loaded or available."
   :prompt-file "prompts/tools/features.md"
   :handler (lambda (args)
	      (list :result
		    (let ((feature (plist-get args :feature)))
		      (if-let* ((feature-symbol (intern-soft feature)))
			  (when (featurep feature-symbol) feature)
			(find-library-name feature)))))
   :args ((feature string :required "FEATURE to look for."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "manual_names"
   :description "List available info manuals."
   :summary "List Info manuals available to Emacs."
   :prompt-file "prompts/tools/manual_names.md"
   :handler (lambda (_args)
	      (list :result
		    (json-serialize
		     (vconcat (info--filter-manual-names (info--manual-names nil))))))
   :args nil
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "manual_nodes"
   :description "List section nodes of an info manual."
   :summary "List section nodes of an Info manual available to Emacs."
   :prompt-file "prompts/tools/manual_nodes.md"
   :handler (lambda (args)
	      (list :result
		    (let ((name (plist-get args :manual)))
		      (json-serialize
		       (vconcat (mapcar #'car (Info-build-node-completions name)))))))
   :args ((manual string :required
		  "The name of the manual.\nExamples include \"cl\", \"elisp\", or \"transient\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "manual_node_contents"
   :description "Read the contents of an info manual node."
   :summary "Read an Info manual node through Emacs."
   :prompt-file "prompts/tools/manual_node_contents.md"
   :handler (lambda (args)
	      (list :result
		    (funcall #'mevedel-tool-introspect--manual-node-contents
			     (plist-get args :manual_name) (plist-get args :node))))
   :args ((manual_name string :required
		       "The name of MANUAL.\nExamples manuals include \"cl\", \"elisp\", or \"transient\".")
	  (node string :required
		"The name of the NODE in a MANUAL.\nExample nodes from the elisp manual include \"Records\" or \"Sequences\nArrays Vectors\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 50000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "symbol_manual_section"
   :description "Find which manual section documents a symbol."
   :summary "Find the Info manual section documenting an Emacs Lisp symbol."
   :prompt-file "prompts/tools/symbol_manual_section.md"
   :handler (lambda (args)
	      (list :result
		    (funcall #'mevedel-tool-introspect--symbol-in-manual
			     (plist-get args :symbol))))
   :args ((symbol string :required
		  "Name of a SYMBOL, such as \"find-file-noselect\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 50000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "function_completions"
   :description "List function names matching an Orderless pattern."
   :summary "Find Emacs function names matching an Orderless pattern."
   :prompt-file "prompts/tools/function_completions.md"
   :handler (lambda (args)
	      (list :result
		    (let ((prefix (plist-get args :function_prefix)))
		      (string-join (orderless-filter prefix obarray #'functionp)
				   "\n"))))
   :args ((function_prefix string :required
			   "FUNCTION_PREFIX of functions you are searching for."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "command_completions"
   :description "List interactive command names matching an Orderless pattern."
   :summary "Find interactive Emacs commands matching an Orderless pattern."
   :prompt-file "prompts/tools/command_completions.md"
   :handler (lambda (args)
	      (list :result
		    (let ((prefix (plist-get args :command_prefix)))
		      (string-join (orderless-filter prefix obarray #'commandp)
				   "\n"))))
   :args ((command_prefix string :required
			  "COMMAND_PREFIX of commands you are searching for."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "variable_completions"
   :description "List variable names matching an Orderless pattern."
   :summary "Find bound Emacs variable names matching an Orderless pattern."
   :prompt-file "prompts/tools/variable_completions.md"
   :handler (lambda (args)
	      (list :result
		    (let ((prefix (plist-get args :variable_prefix)))
		      (string-join (orderless-filter prefix obarray #'boundp) "\n"))))
   :args ((variable_prefix string :required
			   "VARIABLE_PREFIX of variables you are searching for."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "function_source"
   :description "Read the source code for a function or macro."
   :summary "Locate an Emacs function or macro's source definition."
   :prompt-file "prompts/tools/function_source.md"
   :handler (lambda (args)
	      (list :result
		    (mevedel-tool-introspect--source
                     (plist-get args :function))))
   :args ((function string :required
		    "Name of a FUNCTION, such as \"find-file-noselect\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 30000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "variable_source"
   :description "Read the source code for a variable."
   :summary "Read an Emacs variable's source declaration, not its current value."
   :prompt-file "prompts/tools/variable_source.md"
   :handler (lambda (args)
	      (list :result
		    (mevedel-tool-introspect--source
                     (plist-get args :variable) 'defvar)))
   :args ((variable string :required
		    "Name of a VARIABLE, such as \"last-kbd-macro\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 30000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "function_documentation"
   :description "Read the docstring for a function or macro."
   :summary "Read a function or macro docstring from the running Emacs."
   :prompt-file "prompts/tools/function_documentation.md"
   :handler (lambda (args)
	      (list :result
		    (let ((symbol (plist-get args :function)))
		      (when-let* ((symbol (intern-soft symbol)))
			(documentation symbol)))))
   :args ((function string :required "Name of a FUNCTION, such as \"mapcar\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "variable_documentation"
   :description "Read the docstring for a variable."
   :summary "Read a variable docstring from the running Emacs."
   :prompt-file "prompts/tools/variable_documentation.md"
   :handler (lambda (args)
	      (list :result
		    (let ((symbol (plist-get args :variable)))
		      (when-let* ((symbol (intern-soft symbol)))
			(custom-variable-documentation symbol)))))
   :args ((variable string :required
		    "Name of a VARIABLE, such as \"cursor-type\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "library_source"
   :description "Read the source code for a library."
   :summary "Read library source resolved through Emacs load-path."
   :prompt-file "prompts/tools/library_source.md"
   :handler (lambda (args)
	      (list :result
		    (funcall #'mevedel-tool-introspect--library-source
			     (plist-get args :library))))
   :args ((library string :required "LIBRARY to look for."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 50000
   :check-permission #'mevedel-tool-introspect--library-source-check
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "variable_value"
   :description "Return a variable's global value (always asks)."
   :summary "Inspect an Emacs variable's current global runtime value (always asks)."
   :prompt-file "prompts/tools/variable_value.md"
   :handler (lambda (args)
	      (list :result
		    (let ((symbol (plist-get args :variable)))
		      (when-let* ((symbol (intern-soft symbol)))
			(default-value symbol)))))
   :args ((variable string :required
		    "Name of a VARIABLE, such as \"last-kbd-macro\"."))
   :category "mevedel-introspection"
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :check-permission #'mevedel-tool-introspect--variable-value-check
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render))))

(provide 'mevedel-tool-introspect)
;;; mevedel-tool-introspect.el ends here
