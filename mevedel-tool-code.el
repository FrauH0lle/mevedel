;;; mevedel-tool-code.el -- Code exploration tools -*- lexical-binding: t -*-

;;; Commentary:

;; Xref, Imenu, and Tree-sitter tool implementations for code navigation
;; and structural analysis.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

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
(declare-function xref-item-location "xref" (cl-x) t)
(declare-function xref-item-summary "xref" (cl-x) t)

;; `mevedel-workspace'
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))

;; `mevedel-tool-ui'
(declare-function mevedel-tools--request-access "mevedel-tool-ui" (root reason &optional buffer))


;;
;;; Xref Integration

(cl-defun mevedel-tools--xref-find-references (callback identifier file-path)
  "Find references to IDENTIFIER in the current session's project.

CALLBACK is the async callback function to return results.
IDENTIFIER is the symbol to find references for.
FILE-PATH specifies which file's buffer context to use for the search."
  (require 'xref)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--xref-find-references
    (identifier stringp)
    (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path)))
         (identifier-str (format "%s" identifier)))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
      (format "Need to read file: %s" file-path)
      mevedel-tools--xref-find-references callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--xref-find-references
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend)))
            (if (not backend)
                (format "No xref backend available for %s" file-path)
              (let ((xref-items (xref-backend-references backend identifier-str)))
                (if xref-items
                    (funcall callback
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
                  (funcall callback (format "No references found for '%s'" identifier-str))))))
        (error
         (funcall callback (format "Error searching for '%s' in %s: %s"
                                   identifier-str file-path (error-message-string err))))))))

(cl-defun mevedel-tools--xref-find-apropos (callback pattern file-path)
  "Find symbols matching PATTERN across the entire project.

CALLBACK is the async callback function to call with results.
FILE-PATH specifies which file's buffer context to use for the search.
This function uses the session context to operate in the correct
project."
  (require 'xref)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--xref-find-apropos
    (pattern stringp)
    (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path)))
         (pattern-str (format "%s" pattern)))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
      (format "Need to read file: %s" file-path)
      mevedel-tools--xref-find-apropos callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--xref-find-apropos
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (with-current-buffer target-buffer
      (condition-case err
          (let ((backend (xref-find-backend)))
            (cond
             ((not backend)
              (funcall callback (format "No xref backend available for %s" file-path)))
             ;; Special handling for etags without tags table
             ((and (eq backend 'etags)
                   (not (or (and (boundp 'tags-file-name) tags-file-name
                                 (file-exists-p tags-file-name))
                            (and (boundp 'tags-table-list) tags-table-list
                                 (cl-some #'file-exists-p tags-table-list)))))
              (funcall callback (format "No tags table available for %s" file-path)))
             (t
              (let ((xref-items (xref-backend-apropos backend pattern-str)))
                (if xref-items
                    (funcall callback
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
                  (funcall callback (format "No symbols found matching pattern '%s'" pattern-str)))))))
        (error
         (funcall callback (format "Error searching for pattern '%s' in %s: %s"
                                   pattern-str file-path (error-message-string err))))))))


;;
;;; Imenu Integration

(cl-defun mevedel-tools--imenu-list-symbols (callback file-path)
  "List all symbols in FILE-PATH using imenu.
CALLBACK is the async callback function to call with results.
Returns a list of symbols with their types and positions."
  (require 'imenu)
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--imenu-list-symbols
    (file-path stringp))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
      (format "Need to read file: %s" file-path)
      mevedel-tools--imenu-list-symbols callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--imenu-list-symbols
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (condition-case err
        (with-current-buffer target-buffer
          ;; Generate or update imenu index
          (imenu--make-index-alist)
          (if imenu--index-alist
              (let ((results '()))
                ;; Process the imenu index
                (dolist (item imenu--index-alist)
                  (cond
                   ;; Skip special entries
                   ((string-match-p "^\\*" (car item)) nil)
                   ;; Handle simple entries (name . position)
                   ((markerp (cdr item))
                    (let ((line (line-number-at-pos (marker-position (cdr item)))))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line
                                    (car item))
                            results)))
                   ;; Handle position numbers
                   ((numberp (cdr item))
                    (let ((line (line-number-at-pos (cdr item))))
                      (push (format "%s:%d: %s"
                                    file-path
                                    line
                                    (car item))
                            results)))
                   ;; Handle nested entries (category . items)
                   ((listp (cdr item))
                    (let ((category (car item)))
                      (dolist (subitem (cdr item))
                        (when (and (consp subitem)
                                   (or (markerp (cdr subitem))
                                       (numberp (cdr subitem))))
                          (let ((line (line-number-at-pos
                                       (if (markerp (cdr subitem))
                                           (marker-position (cdr subitem))
                                         (cdr subitem)))))
                            (push (format "%s:%d: [%s] %s"
                                          file-path
                                          line
                                          category
                                          (car subitem))
                                  results))))))))
                (if results
                    (funcall callback (string-join (nreverse results) "\n"))
                  (funcall callback (format "No symbols found in %s" file-path))))
            (funcall callback (format "No imenu support or no symbols found in %s" file-path))))
      (error
       (funcall callback (format "Error listing symbols in %s: %s"
                                 file-path (error-message-string err)))))))


;;
;;; Tree-sitter Integration

(defun mevedel-tools--treesit-info (callback file-path &optional line column whole_file include_ancestors include_children)
  "Get tree-sitter parse tree information for FILE-PATH.
CALLBACK is the async callback function to call with results.
Optional LINE and COLUMN specify the position (1-based line, 0-based column).
If WHOLE_FILE is non-nil, show the entire file's syntax tree.
If neither position is specified, defaults to current cursor position (point).
If INCLUDE_ANCESTORS is non-nil, include parent node hierarchy.
If INCLUDE_CHILDREN is non-nil, include child nodes."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--treesit-info
    (file-path stringp)
    (line integerp nil)
    (column integerp nil)
    (whole_file booleanp nil)
    (include_ancestors booleanp nil)
    (include_children booleanp nil))

  (let* ((full-path (expand-file-name file-path))
         (target-buffer (or (find-buffer-visiting full-path)
                            (find-file-noselect full-path))))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
      (format "Need to read file: %s" file-path)
      mevedel-tools--treesit-info callback)

    (unless (file-exists-p full-path)
      (cl-return-from mevedel-tools--treesit-info
        (funcall callback (format "File %s does not exist in the workspace" file-path))))

    (condition-case err
        (if (not (treesit-available-p))
            (funcall callback "Tree-sitter is not available in this Emacs build")
          (with-current-buffer target-buffer
            (let* ((parsers (treesit-parser-list))
                   (parser (car parsers)))
              (if (not parser)
                  (funcall callback (format "No tree-sitter parser available for %s" file-path))
                (let* ((root-node (treesit-parser-root-node parser))
                       ;; Determine position from line/column or use current point
                       (pos (cond (whole_file nil)
                                  (line (mevedel-tools--treesit-line-column-to-point
                                         line (or column 0)))
                                  ;; Use current point in the target buffer
                                  (t (point))))
                       (node (if whole_file
                                 root-node
                               (treesit-node-at pos parser)))
                       (results '()))
                  (if (not node)
                      (funcall callback "No tree-sitter node found")
                    ;; For full tree, use a different display function
                    (if whole_file
                        (mevedel-tools--treesit-format-tree root-node 0 20)
                      ;; Basic node information for specific position
                      (push (format "Node Type: %s" (treesit-node-type node)) results)
                      (push (format "Range: %d-%d"
                                    (treesit-node-start node)
                                    (treesit-node-end node)) results)
                      (push (format "Text: %s"
                                    (truncate-string-to-width
                                     (treesit-node-text node t)
                                     80 nil nil "...")) results)

                      ;; Check if node is named
                      (when (treesit-node-check node 'named)
                        (push "Named: yes" results))

                      ;; Field name if available
                      (let ((field-name (treesit-node-field-name node)))
                        (when field-name
                          (push (format "Field: %s" field-name) results)))

                      ;; Include ancestors if requested
                      (when include_ancestors
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

                      ;; Include children if requested
                      (when include_children
                        (push "\nChildren:" results)
                        (let ((child-count (treesit-node-child-count node))
                              (i 0))
                          (if (= child-count 0)
                              (push "  (no children)" results)
                            (while (< i (min child-count 20))
                              (let ((child (treesit-node-child node i)))
                                (when child
                                  (push (format "  [%d] %s%s (%d-%d)"
                                                i
                                                (treesit-node-type child)
                                                (if (treesit-node-check child 'named)
                                                    " (named)" "")
                                                (treesit-node-start child)
                                                (treesit-node-end child))
                                        results)))
                              (cl-incf i))
                            (when (> child-count 20)
                              (push (format "  ... and %d more children"
                                            (- child-count 20))
                                    results)))))

                      ;; Return formatted results
                      (funcall callback (string-join (nreverse results) "\n")))))))))
      (error
       (funcall callback (format "Error getting tree-sitter info for %s: %s"
                                 file-path (error-message-string err)))))))

(defun mevedel-tools--treesit-format-tree (node level max-depth)
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
                               (mevedel-tools--treesit-format-tree
                                child (1+ level) max-depth)))))
      result)))

(defun mevedel-tools--treesit-line-column-to-point (line column)
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

  ;; Adapted from claude-code-ide.el

  (gptel-make-tool
   :name "XrefReferences"
   :description "Find where a function, variable, or class is used throughout your
codebase. Perfect for understanding code dependencies and impact
analysis.

### When to use `XrefReferences`

- Finding all callers of a function before modifying it
- Understanding where a variable is read/written
- Impact analysis before refactoring
- Tracing data flow through a codebase
- Verifying that dead code is truly unused

### When NOT to use `XrefReferences`

- Searching for text patterns → use `Grep`
- Looking for file names → use `Glob`
- Getting an overview of a file → use `Imenu`
- Complex multi-step searches → DELEGATE

### How to use `XrefReferences`

- Provide the exact identifier name (case-sensitive)
- Specify a file in the project for context (affects which xref
  backend is used)
- Works best with language servers (LSP), TAGS files, or built-in
  elisp xref
- More precise than grep for finding actual references vs. string matches

### Examples of good usage

<example>
- Find all calls to authenticate_user function
XrefReferences(identifier=\"authenticate_user\", file_path=\"src/auth.el\")
</example>

### Examples of bad usage

<example>
XrefReferences(identifier=\"user\", file_path=\".\")
<reasoning>
Too generic, might not be indexed as expected.
Use Grep for simple text searches instead.
</reasoning>
</example>
"
   :function #'mevedel-tools--xref-find-references
   :args '((:name "identifier"
            :type string
            :description "The identifier to find references for")
           (:name "file_path"
            :type string
            :description "File path to use as context for the search"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "XrefDefinitions"
   :description "Search for functions, variables, or classes by name pattern across your
project. Helps you discover code elements when you know part of the name.

### When to use `XrefDefinitions`

- Discovering functions or variables with names matching a pattern
- Finding related symbols when you know part of the name
- Exploring API surface area by naming convention
- Locating symbol definitions by partial name

### When NOT to use `XrefDefinitions`

- Searching for specific text in files -> use `Grep`
- Finding exact symbol references/usage -> use `XrefReferences`
- Searching across many files without symbol focus -> DELEGATE
- Pattern is too vague and will return many results -> DELEGATE

### How to use `XrefDefinitions`

- Provide a pattern (substring or regex) to match symbol names
- Works with indexed symbols (LSP, TAGS, elisp definitions)
- Returns symbol definitions (not all references)
- Useful for discovering what's available in a codebase

### Examples of good usage

<example>
- Find all authentication-related symbols
XrefDefinitions(pattern=\"auth\", file_path=\".\")
</example>

<example>
- Find symbols with 'config' in name
XrefDefinitions(pattern=\"*config*\", file_path=\".\")
</example>

### Examples of bad usage

<example>
XrefDefinitions(pattern=\"error_message\", file_path=\".\")
<reasoning>
Looking for text occurrences.
Use Grep to search for text strings, not symbol definitions.
</reasoning>
</example>
"
   :function #'mevedel-tools--xref-find-apropos
   :args '((:name "pattern"
            :type string
            :description "The pattern to search for symbols")
           (:name "file_path"
            :type string
            :description "File path to use as context for the search"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Imenu"
   :description "Navigate and explore a file's structure by listing all its functions,
classes, and variables with their locations.

### When to use `Imenu`

- Getting a structural overview of a single file's organization
- Listing all functions, classes, methods in a file
- Understanding file structure before making changes
- Quickly finding what symbols are defined in a file

### When NOT to use `Imenu`

- Searching across multiple files -> use `Grep` or DELEGATE
- Finding where a symbol is used (references) -> use `XrefReferences`
- Reading actual code implementation -> use `Read`
- The file is very large and you only need specific content -> use `Read`
  with line ranges

### How to use `Imenu`

- Provide the file path to analyze
- Returns a hierarchical list of symbols (functions, classes, methods, etc.)
- Language-aware (uses major mode's imenu support)
- Useful as a first step before diving into specific functions
- Shows structure without full file content (more efficient than reading
  entire file)

### Examples of good usage

<example>
- Get overview of authentication module structure
Imenu(file_path=\"src/auth.js\")
</example>

### Examples of bad usage

<example>
Imenu(file_path=\"**/*.py\")
<reasoning>
Can't analyze multiple files.
Use Glob to find files, then Imenu on individual files.
</reasoning>
</example>

<example>
Imenu(file_path=\"README.md\")
<reasoning>
Looking for content in documentation.
Use Read to actually see the content of documentation files.
</reasoning>
</example>
"
   :function #'mevedel-tools--imenu-list-symbols
   :args '((:name "file_path"
            :type string
            :description "Path to the file to analyze for symbols"))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Treesitter"
   :description "Get tree-sitter syntax tree information for a file, including node
types, ranges, and hierarchical structure. Useful for understanding code
structure and AST analysis.

### When to use `Treesitter`

- Analyzing precise syntax structure of code
- Understanding code hierarchy and nesting
- Extracting structured information about code elements
- Working with complex syntax that needs precise parsing

### When NOT to use `Treesitter`

- Simple text search -> use `Grep`
- Just reading code -> use `Read`
- Getting a simple overview of functions -> use `Imenu` (simpler and faster)
- Language doesn't have tree-sitter support in Emacs
- You don't need detailed syntax tree information

### How to use `Treesitter`

- Provide the file path and optionally a region/range
- Only works for languages with tree-sitter grammar installed in Emacs
- Returns detailed syntax tree structure
- More detailed than Imenu but also more complex
- Best for tasks requiring precise syntactic analysis

### Examples of good usage

<example>
- Analyze syntax tree at specific location
Treesitter(file_path=\"src/complex-parser.js\", line=10, column=5)
</example>

<example>
- Understand complex YAML structure
Treesitter(file_path=\"nested-config.yaml\", whole_file=true)
</example>

### Examples of bad usage

<example>
Treesitter(file_path=\"README.md\")
<reasoning>
Simple text document.
Use Read to read documentation files.
</reasoning>
</example>
"
   :function #'mevedel-tools--treesit-info
   :args
   '((:name "file_path"
      :type string
      :description "Path to the file to analyze")
     (:name "line"
      :type number
      :optional t
      :description "Line number (1-based)")
     (:name "column"
      :type number
      :optional t
      :description "Column number (0-based)")
     (:name "whole_file"
      :type boolean
      :optional t
      :description "Show the entire file's syntax tree")
     (:name "include_ancestors"
      :type boolean
      :optional t
      :description "Include parent node hierarchy")
     (:name "include_children"
      :type boolean
      :optional t
      :description "Include child nodes"))
   :category "mevedel"
   :async t))

(provide 'mevedel-tool-code)
;;; mevedel-tool-code.el ends here
