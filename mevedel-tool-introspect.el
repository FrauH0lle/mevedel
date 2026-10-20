;;; mevedel-tool-introspect.el -- Elisp introspection tools -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Karthik Chikmagalur
;; Tool handlers adapted from gptel-agent-tools-introspection.el,
;; originally adapted from ragmacs.el by Positron Solutions.

;;; Commentary:

;; Three native Emacs introspection tools with mevedel permissions, result
;; limits, persistence and rendering: EmacsDescribe reads one symbol's or
;; library's documentation, source, manual section or value; EmacsFind matches
;; names, features and load-path entries; EmacsManual reads Info manuals.
;; Available through the discoverable elisp group.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
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

(defun mevedel-tool-introspect--library-source-check (_tool input)
  "Allow INPUT only when it names source inside a local `load-path'."
  (let ((name (plist-get input :symbol))
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

(defun mevedel-tool-introspect--describe-kind (args)
  "Return the kind EmacsDescribe ARGS address: function, variable or library.
An omitted kind is a defined function, else a bound variable, else a library
for a source read.  Return nil when nothing matches."
  (let ((kind (plist-get args :kind))
        (symbol (mevedel-tool-introspect--symbol args)))
    (cond
     ((member kind '("function" "variable" "library")) (intern kind))
     ((and symbol (fboundp symbol)) 'function)
     ((and symbol (boundp symbol)) 'variable)
     ((member (plist-get args :aspect) '("source" "location")) 'library))))

(defun mevedel-tool-introspect--describe-check (_tool input)
  "Decide EmacsDescribe INPUT's own permission, or nil for ordinary policy.
A value read always asks: variables can hold auth tokens and other private
state.  A library source read must resolve inside a local `load-path', so it
never reads a remote or escaping file.  A library location only searches
local directories and reads no file, so ordinary policy covers it."
  (pcase (plist-get input :aspect)
    ("value" 'ask)
    ("source"
     (when (eq 'library (mevedel-tool-introspect--describe-kind input))
       (mevedel-tool-introspect--library-source-check nil input)))))

(defun mevedel-tool-introspect--present (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun mevedel-tool-introspect--symbol (args)
  "Return the existing symbol ARGS name, never interning a new one."
  (when-let* ((name (mevedel-tool-introspect--present (plist-get args :symbol))))
    (intern-soft name)))


;;
;;; Renderers

(defun mevedel-tool-introspect--primary-value (args)
  "Return the first meaningful primary value from introspection ARGS."
  (catch 'found
    (dolist (key '(:symbol :node :manual :pattern :kind))
      (let ((value (plist-get args key)))
        (when (and (stringp value) (not (string-empty-p value)))
          (throw 'found value))))
    nil))

(defun mevedel-tool-introspect--body-kind (name args)
  "Return a display kind symbol for introspection tool NAME called with ARGS."
  (pcase name
    ("EmacsDescribe"
     (pcase (plist-get args :aspect)
       ("source" 'source)
       ((or "documentation" "manual") 'documentation)
       (_ 'result)))
    ("EmacsManual" (if (plist-get args :node) 'documentation 'result))
    ("EmacsFind" 'completions)
    (_ 'result)))

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
        :body-kind (mevedel-tool-introspect--body-kind name args)
        :lines (mevedel-tool-introspect--line-count result)
        :chars (length result)))

(defun mevedel-tool-introspect--render (name args result render-data)
  "Return rendering plist for NAME using ARGS, RESULT, and RENDER-DATA."
  (when (stringp result)
    (let* ((target (or (plist-get render-data :target)
                       (mevedel-tool-introspect--primary-value args)))
           (kind (or (plist-get render-data :body-kind)
                     (mevedel-tool-introspect--body-kind name args)))
           (lines (or (plist-get render-data :lines)
                      (mevedel-tool-introspect--line-count result)))
           (shown-name (or name "introspect"))
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
  "Return contents of NODE in Info MANUAL without displaying it."
  (with-temp-buffer
    ;; Enter Info mode first so navigation does not select an Info window.
    ;; A saved window configuration would overwrite semantic reader positions
    ;; if a live view redraws while Info loads the node.
    (Info-mode)
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
TYPE is nil for functions or defvar for variables.
Missing C source configuration signals an error instead of prompting."
  (mevedel-tool--with-quiet-file-visit
    (when-let* ((callable (intern-soft symbol))
                (save-silently t)
                (vc-follow-symlinks t)
                (definition
                 (if type
                     (cons callable (or (symbol-file callable type)
                                        (help-C-file-name callable 'var)))
                   (find-function-library callable)))
                (location
                 (let ((library (cdr definition)))
                   ;; Emacs prompts for this directory even in a tool callback.
                   ;; Keep native alias resolution and source search, but never
                   ;; enter that interactive C-source configuration path.
                   (when (and library
                              (string-match-p "\\`src/.*\\.[cm]\\'" library)
                              (null find-function-C-source-directory))
                     (error "Emacs C sources unavailable; set find-function-C-source-directory"))
                   (find-function-search-for-symbol
                    (car definition) type library))))
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

(defun mevedel-tool-introspect--describe (args)
  "Return EmacsDescribe's result for ARGS."
  (let* ((name (plist-get args :symbol))
         (aspect (plist-get args :aspect))
         (kind (mevedel-tool-introspect--describe-kind args))
         (symbol (mevedel-tool-introspect--symbol args)))
    (unless (mevedel-tool-introspect--present name)
      (error "EmacsDescribe needs a symbol"))
    (list
     :result
     (cond
      ((equal aspect "manual")
       (or (mevedel-tool-introspect--symbol-in-manual name)
           (format "No Info manual section documents `%s'." name)))
      ((null kind)
       (format "No function or variable named `%s' is defined in this Emacs." name))
      ((and (eq kind 'function) (not (and symbol (fboundp symbol))))
       (format "No function named `%s' is defined in this Emacs." name))
      ((and (eq kind 'variable) (not symbol))
       (format "No variable named `%s' is known to this Emacs." name))
      ((equal aspect "documentation")
       (pcase kind
         ('function (or (documentation symbol)
                        (format "Function `%s' has no documentation." name)))
         ('variable (or (custom-variable-documentation symbol)
                        (format "Variable `%s' has no documentation." name)))
         (_ (error "Libraries have no documentation aspect; read their source"))))
      ((equal aspect "source")
       (pcase kind
         ('library (mevedel-tool-introspect--library-source name))
         (_ (or (mevedel-tool-introspect--source
                 name (and (eq kind 'variable) 'defvar))
                (format "No source located for %s `%s'." kind name)))))
      ((equal aspect "location")
       (mevedel-tool-introspect--location name symbol kind))
      ((equal aspect "value")
       (cond
        ((not (eq kind 'variable))
         (error "Only variables have a value; `%s' is a %s" name kind))
        ((boundp symbol) (default-value symbol))
        (t (format "Variable `%s' has no global value." name))))
      (t (error "Unknown EmacsDescribe aspect: %s" aspect))))))

(defun mevedel-tool-introspect--location (name symbol kind)
  "Return where NAME, interned as SYMBOL, of KIND is defined or found.
A library location also says whether its feature is loaded."
  (pcase kind
    ('library
     (let ((feature (intern-soft name)))
       (if-let* ((file (mevedel-tool-introspect--local-library-file name)))
           (format "Library `%s' is at %s; its feature is %s." name file
                   (if (and feature (featurep feature)) "loaded" "not loaded"))
         (format "Library `%s' is not found in local load-path directories%s." name
                 (if (and feature (featurep feature)) ", but its feature is loaded" "")))))
    (_
     (or (symbol-file symbol (if (eq kind 'variable) 'defvar 'defun))
         (format "No defining file is recorded for %s `%s'; it may be a C primitive or defined interactively."
                 kind name)))))

(defun mevedel-tool-introspect--local-load-path ()
  "Return existing local `load-path' directories.
Remote entries are skipped so a lookup never opens a connection."
  (seq-filter (lambda (directory)
                (and (stringp directory)
                     (not (file-remote-p directory))
                     (file-directory-p directory)))
              load-path))

(defun mevedel-tool-introspect--local-library-file (name)
  "Return the first local `load-path' file for library NAME, or nil.
Source is preferred over compiled files within one directory."
  (when (and (not (file-name-absolute-p name))
             (null (file-name-directory name)))
    (cl-loop for directory in (mevedel-tool-introspect--local-load-path)
             thereis (cl-loop for suffix in '(".el" ".el.gz" ".elc")
                              for file = (expand-file-name (concat name suffix)
                                                           directory)
                              when (file-exists-p file) return file))))

(defun mevedel-tool-introspect--library-names ()
  "Return library names found in local `load-path' directories."
  (let ((names (make-hash-table :test #'equal)))
    (dolist (directory (mevedel-tool-introspect--local-load-path))
      (dolist (file (directory-files directory nil "\\.elc?\\(?:\\.gz\\)?\\'"))
        (puthash (replace-regexp-in-string "\\.elc?\\(?:\\.gz\\)?\\'" "" file)
                 t names)))
    (hash-table-keys names)))

(defun mevedel-tool-introspect--find (args)
  "Return EmacsFind's result for ARGS: names matching an Orderless pattern."
  (let ((kind (plist-get args :kind))
        (pattern (mevedel-tool-introspect--present (plist-get args :pattern))))
    (unless pattern
      (error "EmacsFind needs a pattern"))
    (list
     :result
     (string-join
      (pcase kind
        ("function" (orderless-filter pattern obarray #'functionp))
        ("command" (orderless-filter pattern obarray #'commandp))
        ("variable" (orderless-filter pattern obarray #'boundp))
        ("library" (sort (orderless-filter
                          pattern (mevedel-tool-introspect--library-names))
                         #'string<))
        (_ (error "Unknown EmacsFind kind: %s" kind)))
      "\n"))))

(defun mevedel-tool-introspect--manual (args)
  "Return EmacsManual's result for ARGS."
  (let ((manual (mevedel-tool-introspect--present (plist-get args :manual)))
        (node (mevedel-tool-introspect--present (plist-get args :node))))
    (list
     :result
     (cond
      (node
       (unless manual (error "EmacsManual needs the manual containing node %s" node))
       (mevedel-tool-introspect--manual-node-contents manual node))
      (manual
       (json-serialize (vconcat (mapcar #'car (Info-build-node-completions manual)))))
      (t
       (json-serialize
        (vconcat (info--filter-manual-names (info--manual-names nil)))))))))


;;
;;; Registration

;;;###autoload
(defun mevedel-tool-introspect--register ()
  "Register the three native Emacs introspection tools."
  (mevedel-define-tool
   :name "EmacsDescribe"
   :description "Read a symbol's or library's documentation, source, location, Info manual section, or global value from the running Emacs."
   :summary "Read an Emacs function, variable or library's documentation, source, location, manual section or current value."
   :prompt-file "prompts/tools/emacsdescribe.md"
   :handler #'mevedel-tool-introspect--describe
   :args ((symbol string :required
                  "Exact function, variable or library name, such as \"mapcar\", \"fill-column\" or \"subr-x\".")
          (aspect string :required "What to read."
                  :enum ["documentation" "source" "location" "manual" "value"])
          (kind string :optional
                "What SYMBOL names. Omit to infer: a defined function, else a variable, else a library for source or location."
                :enum ["function" "variable" "library"]))
   :groups (elisp)
   :read-only-p t
   :max-result-size 50000
   :check-permission #'mevedel-tool-introspect--describe-check
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "EmacsFind"
   :description "Find Emacs function, command, variable or library names matching an Orderless pattern."
   :summary "Find Emacs function, command, variable or library names in the running Emacs."
   :prompt-file "prompts/tools/emacsfind.md"
   :handler #'mevedel-tool-introspect--find
   :args ((kind string :required "What to find."
                :enum ["function" "command" "variable" "library"])
          (pattern string :required
                   "Orderless pattern; space-separated components match in any order."))
   :groups (elisp)
   :read-only-p t
   :max-result-size 20000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render)))

  (mevedel-define-tool
   :name "EmacsManual"
   :description "List Info manuals, list a manual's nodes, or read one node through Emacs."
   :summary "Read Emacs Info manuals: list manuals, list a manual's nodes, or read a node."
   :prompt-file "prompts/tools/emacsmanual.md"
   :handler #'mevedel-tool-introspect--manual
   :args ((manual string :optional
                  "Info manual such as \"elisp\" or \"cl\". Omit to list manuals.")
          (node string :optional
                "Node of MANUAL such as \"Sequences Arrays Vectors\". Omit to list MANUAL's nodes."))
   :groups (elisp)
   :read-only-p t
   :max-result-size 50000
   :render-transform #'mevedel-tool-introspect--render-transform
   :renderer '((success . mevedel-tool-introspect--render))))

(provide 'mevedel-tool-introspect)
;;; mevedel-tool-introspect.el ends here
