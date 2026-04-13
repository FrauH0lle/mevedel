;;; mevedel-tool-code.el -- Code exploration tools -*- lexical-binding: t -*-

;;; Commentary:

;; Xref, Imenu, and Tree-sitter tool implementations for code navigation
;; and structural analysis.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `mevedel-pipeline'
(declare-function mevedel-pipeline-run-tool "mevedel-pipeline")
(declare-function mevedel-pipeline--positional-to-plist "mevedel-pipeline")

;; `mevedel-tool-registry'
(declare-function mevedel-tool-register "mevedel-tool-registry")

;; `cl-extra'
(declare-function cl-some "cl-extra" (cl-pred cl-seq &rest cl-rest))

;; `imenu'
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(defvar imenu--index-alist)

;; `treesit'
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-field-name "treesit" (node))
(declare-function treesit-node-text "treesit" (node &optional no-property))

;; `xref'
(declare-function xref-backend-references "xref" (backend identifier))
(declare-function xref-backend-apropos "xref" (backend pattern))
(declare-function xref-find-backend "xref")
(declare-function xref-item-location "xref" (cl-x) t)
(declare-function xref-item-summary "xref" (cl-x) t)
(declare-function xref-location-group "xref" (cl-x) t)
(declare-function xref-location-marker "xref" (cl-x) t)

;; `project'
(defvar project-current-directory-override)


;;
;;; Xref Integration

(defun mevedel-tool-code--format-xref-items (xref-items)
  "Format XREF-ITEMS as a newline-separated string of file:line: summary."
  (string-join
   (mapcar (lambda (item)
             (let* ((location (xref-item-location item))
                    (file (xref-location-group location))
                    (marker (xref-location-marker location))
                    (line (with-current-buffer (marker-buffer marker)
                            (save-excursion
                              (goto-char marker)
                              (line-number-at-pos))))
                    (summary (xref-item-summary item)))
               (format "%s:%d: %s" file line summary)))
           xref-items)
   "\n"))

(defun mevedel-tool-code--xref-references (callback args)
  "Find references to an identifier using xref.
CALLBACK receives the result string.  ARGS is a plist with :identifier
and :file_path."
  (require 'xref)
  (let* ((identifier (plist-get args :identifier))
         (file-path (plist-get args :file_path))
         (full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))
    (unless (file-exists-p full-path)
      (error "File %s does not exist" file-path))
    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend))
                ;; Prevent interactive project selection when the file
                ;; is not inside a recognized project.
                (project-current-directory-override
                 (file-name-directory full-path)))
            (unless backend
              (error "No xref backend available for %s" file-path))
            (let ((xref-items (xref-backend-references backend identifier)))
              (funcall callback
                       (if xref-items
                           (mevedel-tool-code--format-xref-items xref-items)
                         (format "No references found for '%s'" identifier)))))
        (error
         (funcall callback (format "Error searching for '%s' in %s: %s"
                                   identifier file-path
                                   (error-message-string err))))))))

(defun mevedel-tool-code--xref-definitions (callback args)
  "Find symbols matching a pattern using xref-backend-apropos.
CALLBACK receives the result string.  ARGS is a plist with :pattern
and :file_path."
  (require 'xref)
  (let* ((pattern (plist-get args :pattern))
         (file-path (plist-get args :file_path))
         (full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))
    (unless (file-exists-p full-path)
      (error "File %s does not exist" file-path))
    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend))
                ;; Prevent interactive project selection when the file
                ;; is not inside a recognized project.
                (project-current-directory-override
                 (file-name-directory full-path)))
            (cond
             ((not backend)
              (funcall callback
                       (format "No xref backend available for %s" file-path)))
             ;; Special handling for etags without tags table
             ((and (eq backend 'etags)
                   (not (or (and (boundp 'tags-file-name) tags-file-name
                                 (file-exists-p tags-file-name))
                            (and (boundp 'tags-table-list) tags-table-list
                                 (cl-some #'file-exists-p tags-table-list)))))
              (funcall callback
                       (format "No tags table available for %s" file-path)))
             (t
              (let ((xref-items (xref-backend-apropos backend pattern)))
                (funcall callback
                         (if xref-items
                             (mevedel-tool-code--format-xref-items xref-items)
                           (format "No symbols found matching pattern '%s'"
                                   pattern)))))))
        (error
         (funcall callback (format "Error searching for pattern '%s' in %s: %s"
                                   pattern file-path
                                   (error-message-string err))))))))


;;
;;; Imenu Integration

(defun mevedel-tool-code--imenu (callback args)
  "List symbols in a file using imenu.
CALLBACK receives the result string.  ARGS is a plist with :file_path."
  (require 'imenu)
  (let* ((file-path (plist-get args :file_path))
         (full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))
    (unless (file-exists-p full-path)
      (error "File %s does not exist" file-path))
    (condition-case err
        (with-current-buffer target-buffer
          (imenu--make-index-alist)
          (if imenu--index-alist
              (let ((results nil))
                (dolist (item imenu--index-alist)
                  (cond
                   ;; Skip special entries
                   ((string-match-p "^\\*" (car item)) nil)
                   ;; Simple entries (name . marker-or-position)
                   ((markerp (cdr item))
                    (push (format "%s:%d: %s" file-path
                                  (line-number-at-pos
                                   (marker-position (cdr item)))
                                  (car item))
                          results))
                   ((numberp (cdr item))
                    (push (format "%s:%d: %s" file-path
                                  (line-number-at-pos (cdr item))
                                  (car item))
                          results))
                   ;; Nested entries (category . items)
                   ((listp (cdr item))
                    (let ((category (car item)))
                      (dolist (subitem (cdr item))
                        (when (and (consp subitem)
                                   (or (markerp (cdr subitem))
                                       (numberp (cdr subitem))))
                          (push (format "%s:%d: [%s] %s" file-path
                                        (line-number-at-pos
                                         (if (markerp (cdr subitem))
                                             (marker-position (cdr subitem))
                                           (cdr subitem)))
                                        category (car subitem))
                                results)))))))
                (funcall callback
                         (if results
                             (string-join (nreverse results) "\n")
                           (format "No symbols found in %s" file-path))))
            (funcall callback
                     (format "No imenu support or no symbols found in %s"
                             file-path))))
      (error
       (funcall callback (format "Error listing symbols in %s: %s"
                                 file-path (error-message-string err)))))))


;;
;;; Tree-sitter Integration

(defun mevedel-tool-code--treesitter (callback args)
  "Get tree-sitter syntax tree information for a file.
CALLBACK receives the result string.  ARGS is a plist with :file_path
and optional :line, :column, :whole_file, :include_ancestors,
:include_children."
  (let* ((file-path (plist-get args :file_path))
         (line (plist-get args :line))
         (column (plist-get args :column))
         (whole-file (let ((v (plist-get args :whole_file)))
                       (and v (not (eq v :json-false)))))
         (include-ancestors (let ((v (plist-get args :include_ancestors)))
                              (and v (not (eq v :json-false)))))
         (include-children (let ((v (plist-get args :include_children)))
                             (and v (not (eq v :json-false)))))
         (full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))
    (unless (file-exists-p full-path)
      (error "File %s does not exist" file-path))
    (unless (treesit-available-p)
      (error "Tree-sitter is not available in this Emacs build"))
    (condition-case err
        (with-current-buffer target-buffer
          (let* ((parsers (treesit-parser-list))
                 (parser (car parsers)))
            (unless parser
              (error "No tree-sitter parser available for %s" file-path))
            (let* ((root-node (treesit-parser-root-node parser))
                   (pos (cond (whole-file nil)
                              (line (mevedel-tool-code--line-column-to-point
                                     line (or column 0)))
                              (t (point))))
                   (node (if whole-file root-node
                           (treesit-node-at pos parser))))
              (unless node
                (error "No tree-sitter node found"))
              (if whole-file
                  (funcall callback
                           (mevedel-tool-code--treesit-format-tree
                            root-node 0 20))
                (let ((results nil))
                  (push (format "Node Type: %s" (treesit-node-type node))
                        results)
                  (push (format "Range: %d-%d"
                                (treesit-node-start node)
                                (treesit-node-end node))
                        results)
                  (push (format "Text: %s"
                                (truncate-string-to-width
                                 (treesit-node-text node t)
                                 80 nil nil "..."))
                        results)
                  (when (treesit-node-check node 'named)
                    (push "Named: yes" results))
                  (when-let* ((field-name (treesit-node-field-name node)))
                    (push (format "Field: %s" field-name) results))
                  ;; Ancestors
                  (when include-ancestors
                    (push "\nAncestors:" results)
                    (let ((parent (treesit-node-parent node))
                          (level 1))
                      (while (and parent (< level 10))
                        (push (format "  %s[%d] %s (%d-%d)"
                                      (make-string level ?-)
                                      level
                                      (treesit-node-type parent)
                                      (treesit-node-start parent)
                                      (treesit-node-end parent))
                              results)
                        (setq parent (treesit-node-parent parent))
                        (cl-incf level))))
                  ;; Children
                  (when include-children
                    (push "\nChildren:" results)
                    (let ((child-count (treesit-node-child-count node))
                          (i 0))
                      (if (= child-count 0)
                          (push "  (no children)" results)
                        (while (< i (min child-count 20))
                          (when-let* ((child (treesit-node-child node i)))
                            (push (format "  [%d] %s%s (%d-%d)"
                                          i
                                          (treesit-node-type child)
                                          (if (treesit-node-check child 'named)
                                              " (named)" "")
                                          (treesit-node-start child)
                                          (treesit-node-end child))
                                  results))
                          (cl-incf i))
                        (when (> child-count 20)
                          (push (format "  ... and %d more children"
                                        (- child-count 20))
                                results)))))
                  (funcall callback
                           (string-join (nreverse results) "\n")))))))
      (error
       (funcall callback (format "Error getting tree-sitter info for %s: %s"
                                 file-path (error-message-string err)))))))

(defun mevedel-tool-code--treesit-format-tree (node level max-depth)
  "Format NODE and its children as a tree string.
LEVEL is the current indentation level.
MAX-DEPTH is the maximum depth to traverse."
  (if (or (not node) (>= level max-depth))
      ""
    (let* ((indent (make-string (* level 2) ?\s))
           (type (treesit-node-type node))
           (named (if (treesit-node-check node 'named) " (named)" ""))
           (start (treesit-node-start node))
           (end (treesit-node-end node))
           (field-name (treesit-node-field-name node))
           (field-str (if field-name (format " [%s]" field-name) ""))
           (text (treesit-node-text node t))
           (text-preview (if (and (< (length text) 40)
                                  (not (string-match-p "\n" text)))
                             (format " \"%s\"" text)
                           ""))
           (result (format "%s%s%s%s (%d-%d)%s\n"
                           indent type named field-str
                           start end text-preview))
           (child-count (treesit-node-child-count node)))
      ;; Add children
      (dotimes (i child-count)
        (when-let ((child (treesit-node-child node i)))
          (setq result (concat result
                               (mevedel-tool-code--treesit-format-tree
                                child (1+ level) max-depth)))))
      result)))

(defun mevedel-tool-code--line-column-to-point (line column)
  "Convert LINE and COLUMN to point position in current buffer.
LINE is 1-based, COLUMN is 0-based (Emacs convention)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))


;;
;;; Tool registration

(defun mevedel-tool-code--register ()
  "Register code exploration tools (Xref, Imenu, Treesitter)."

  (mevedel-define-tool
    :name "XrefReferences"
    :description "Find where a function, variable, or class is used throughout your codebase."
    :prompt-file "tools/xref-references.md"
    :handler #'mevedel-tool-code--xref-references
    :args ((identifier string :required
                       "The exact identifier to find references for (case-sensitive).")
           (file_path string :required
                      "File path to use as context for the search (affects which xref backend is used)."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path)))

  (mevedel-define-tool
    :name "XrefDefinitions"
    :description "Search for functions, variables, or classes by name pattern across your project."
    :prompt-file "tools/xref-definitions.md"
    :handler #'mevedel-tool-code--xref-definitions
    :args ((pattern string :required
                    "The pattern (substring or regex) to match symbol names.")
           (file_path string :required
                      "File path to use as context for the search."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path)))

  (mevedel-define-tool
    :name "Imenu"
    :description "Navigate and explore a file's structure by listing all its functions, classes, and variables with their locations."
    :prompt-file "tools/imenu.md"
    :handler #'mevedel-tool-code--imenu
    :args ((file_path string :required
                      "Path to the file to analyze for symbols."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path)))

  (mevedel-define-tool
    :name "Treesitter"
    :description "Get tree-sitter syntax tree information for a file."
    :prompt-file "tools/treesitter.md"
    :handler #'mevedel-tool-code--treesitter
    :args ((file_path string :required
                      "Path to the file to analyze.")
           (line integer :optional
                 "Line number (1-based).")
           (column integer :optional
                   "Column number (0-based).")
           (whole_file boolean :optional
                       "Show the entire file's syntax tree.")
           (include_ancestors boolean :optional
                              "Include parent node hierarchy.")
           (include_children boolean :optional
                             "Include child nodes."))
    :async-p t
    :read-only-p t
    :max-result-size 30000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path))))

(provide 'mevedel-tool-code)
;;; mevedel-tool-code.el ends here
