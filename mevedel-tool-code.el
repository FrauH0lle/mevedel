;;; mevedel-tool-code.el -- Code exploration tools -*- lexical-binding: t -*-

;;; Commentary:

;; Xref, Imenu, and Tree-sitter tool implementations for code navigation
;; and structural analysis.

;;; Code:

;; `mevedel-utilities'
(declare-function mevedel--truncate-display
                  "mevedel-utilities" (text width &optional ellipsis))
(autoload 'mevedel--truncate-display "mevedel-utilities")

(require 'cl-lib)

(eval-when-compile
  (require 'mevedel-tool-registry))

;; `apropos'
(declare-function apropos-parse-pattern "apropos" (pattern &optional do-all))
(autoload 'apropos-parse-pattern "apropos")

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `imenu'
(declare-function imenu--make-index-alist "imenu" (&optional noerror))
(declare-function imenu--subalist-p "imenu" (item))
(defvar imenu--index-alist)
(autoload 'imenu--make-index-alist "imenu")
(autoload 'imenu--subalist-p "imenu")

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-native-path
                  "mevedel-execution-target" (target path))
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-native-path "mevedel-execution-target")
(autoload 'mevedel-execution-target-remote-p "mevedel-execution-target")

;; `mevedel-pipeline'
(declare-function mevedel-pipeline--positional-to-plist
                  "mevedel-pipeline" (raw-args specs))
(declare-function mevedel-pipeline-run-tool
                  "mevedel-pipeline" (tool callback args))

;; `mevedel-tool-registry'
(declare-function mevedel-tool--resolve-prompt
                  "mevedel-tool-registry" (prompt))
(declare-function mevedel-tool-register "mevedel-tool-registry" (tool))
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))
(autoload 'mevedel-tool-truthy-p "mevedel-tool-registry")

;; `project'
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-files "project" (project &optional dirs))
(defvar project-current-directory-override)
(defvar project-files-relative-names)

;; `treesit'
(declare-function treesit-available-p "treesit" ())
(declare-function treesit-parser-list "treesit" (&optional buffer language))
(declare-function treesit-parser-root-node "treesit" (parser))
(declare-function treesit-node-at "treesit" (pos &optional parser-or-lang named))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-start "treesit" (node))
(declare-function treesit-node-end "treesit" (node))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-field-name "treesit" (node))
(declare-function treesit-node-parent "treesit" (node))
(declare-function treesit-node-child "treesit" (node n &optional named))
(declare-function treesit-node-child-count "treesit" (node &optional named))
(declare-function treesit-node-check "treesit" (node property))

;; `xref'
(declare-function xref-backend-apropos "xref" (backend pattern))
(declare-function xref-backend-references "xref" (backend identifier))
(declare-function xref-find-backend "xref")
(declare-function xref-item-location "xref" (cl-x) t)
(declare-function xref-item-summary "xref" (cl-x) t)
(declare-function xref-location-group "xref" (cl-x) t)
(declare-function xref-location-line "xref" (cl-x) t)
(declare-function xref-location-marker "xref" (cl-x) t)
(declare-function xref-matches-in-files "xref" (regexp files))
(autoload 'xref-backend-apropos "xref")
(autoload 'xref-backend-references "xref")
(autoload 'xref-find-backend "xref")
(autoload 'xref-item-location "xref")
(autoload 'xref-item-summary "xref")
(autoload 'xref-location-group "xref")
(autoload 'xref-location-line "xref")
(autoload 'xref-location-marker "xref")
(autoload 'xref-matches-in-files "xref")


;;
;;; Xref Integration

(defmacro mevedel-tool-code--with-quiet-file-visit (&rest body)
  "Run BODY while suppressing interactive file-visit side effects."
  (declare (indent 0) (debug t))
  `(let ((enable-local-variables :safe)
         (find-file-hook nil)
         (hack-local-variables-hook nil))
     ,@body))

(defun mevedel-tool-code--execution-target (path)
  "Return the execution target containing absolute PATH."
  (mevedel-execution-target-create (file-name-directory path)))

(defun mevedel-tool-code--with-file-buffer (file-path callback)
  "Call CALLBACK with FILE-PATH's target-native path and visiting buffer.
CALLBACK also receives the qualified expanded file name.  If this function
opens the buffer itself, kill it afterward unless it was modified."
  (let* ((full-path (expand-file-name file-path))
         (target (mevedel-tool-code--execution-target full-path))
         (native-path
          (mevedel-execution-target-native-path target full-path)))
    (unless (file-exists-p full-path)
      (error "File %s does not exist" native-path))
    (let* ((existing-buffer (find-buffer-visiting full-path))
           (target-buffer (or existing-buffer
                              (mevedel-tool-code--with-quiet-file-visit
                                (find-file-noselect full-path)))))
      (unwind-protect
          (funcall callback native-path full-path target-buffer)
        (when (and (not existing-buffer)
                   (buffer-live-p target-buffer)
                   (not (buffer-modified-p target-buffer)))
          (kill-buffer target-buffer))))))

(defun mevedel-tool-code--xref-location-line (location)
  "Return LOCATION's line number without leaving visited files behind."
  (mevedel-tool-code--with-quiet-file-visit
    (or (xref-location-line location)
        (let ((before-buffers (buffer-list))
              marker marker-buffer line)
          (unwind-protect
              (progn
                (setq marker (xref-location-marker location)
                      marker-buffer (and (markerp marker)
                                         (marker-buffer marker)))
                (when marker-buffer
                  (with-current-buffer marker-buffer
                    (save-excursion
                      (goto-char marker)
                      (setq line (line-number-at-pos))))))
            (when (and (buffer-live-p marker-buffer)
                       (not (memq marker-buffer before-buffers))
                       (not (buffer-modified-p marker-buffer))
                       (let ((file (buffer-file-name marker-buffer))
                             (group (xref-location-group location)))
                         (and file
                              (stringp group)
                              (ignore-errors
                                (file-equal-p file group)))))
              (kill-buffer marker-buffer)))
          line))))

(defun mevedel-tool-code--format-xref-items (xref-items &optional target)
  "Format XREF-ITEMS as a newline-separated string of file:line: summary.
When TARGET is non-nil, render locations in its target-native path domain."
  (string-join
   (mapcar (lambda (item)
             (mevedel-tool-code--with-quiet-file-visit
               (let* ((location (xref-item-location item))
                      (file (xref-location-group location))
                      (file (if (and target (stringp file))
                                (mevedel-execution-target-native-path
                                 target file)
                              file))
                      (line (mevedel-tool-code--xref-location-line location))
                      (summary (xref-item-summary item)))
                 (format "%s:%s: %s" file (or line "?") summary))))
           xref-items)
   "\n"))

(defun mevedel-tool-code--remote-xref-backend-error
    (target backend operation)
  "Return a diagnostic when BACKEND cannot run OPERATION on remote TARGET."
  (when (and (mevedel-execution-target-remote-p target)
             (not (and (eq operation 'references)
                       (eq backend 'elisp))))
    (format
     "Error: Remote Xref %s with backend `%s' is not supported for remote workspaces; use Imenu, Grep, or an explicitly tested TRAMP-aware backend"
     (if (eq operation 'references) "reference search" "definition search")
     backend)))

(defun mevedel-tool-code--xref-references (callback args)
  "Find references to an identifier using xref.
CALLBACK receives the result envelope.  ARGS is a plist with :identifier
and :file_path."
  (let ((identifier (plist-get args :identifier))
        (file-path (plist-get args :file_path)))
    (mevedel-tool-code--with-file-buffer
     file-path
     (lambda (file-path full-path target-buffer)
       (with-current-buffer target-buffer
         (condition-case err
             (let* ((backend (mevedel-tool-code--with-quiet-file-visit
                               (xref-find-backend)))
                    (target (mevedel-tool-code--execution-target full-path))
                    (backend-error
                     (mevedel-tool-code--remote-xref-backend-error
                      target backend 'references))
                    ;; Prevent interactive project selection when the file
                    ;; is not inside a recognized project.
                    (project-current-directory-override
                     (file-name-directory full-path)))
               (cond
                ((not backend)
                 (error "No xref backend available for %s" file-path))
                (backend-error
                 (funcall
                  callback
                  (list :result backend-error)))
                (t
                 (let ((xref-items
                        (mevedel-tool-code--with-quiet-file-visit
                          (if (eq backend 'elisp)
                              (let ((project-files-relative-names nil))
                                (when-let* ((project (project-current t))
                                            (files (project-files project)))
                                  (let ((symbol-regexp
                                         (format "\\_<%s\\_>"
                                                 (regexp-quote identifier))))
                                    (cl-remove-if-not
                                     (lambda (item)
                                       (string-match-p
                                        symbol-regexp (xref-item-summary item)))
                                     (xref-matches-in-files
                                      (regexp-quote identifier) files)))))
                            (xref-backend-references backend identifier)))))
                   (funcall callback
                            (list :result
                                  (if xref-items
                                      (mevedel-tool-code--format-xref-items
                                       xref-items target)
                                    (format "No references found for '%s'"
                                            identifier))))))))
           (error
            (funcall callback
                     (list :result
                           (format "Error searching for '%s' in %s: %s"
                                   identifier file-path
                                   (error-message-string err)))))))))))

(defun mevedel-tool-code--xref-definitions (callback args)
  "Find symbols matching a pattern using `xref-backend-apropos'.
CALLBACK receives the result envelope.  ARGS is a plist with :pattern
and :file_path."
  (let ((pattern (plist-get args :pattern))
        (file-path (plist-get args :file_path)))
    (mevedel-tool-code--with-file-buffer
     file-path
     (lambda (file-path full-path target-buffer)
       (with-current-buffer target-buffer
         (condition-case err
             (let* ((backend (mevedel-tool-code--with-quiet-file-visit
                               (xref-find-backend)))
                    (target (mevedel-tool-code--execution-target full-path))
                    (backend-error
                     (mevedel-tool-code--remote-xref-backend-error
                      target backend 'definitions))
                    ;; Prevent interactive project selection when the file
                    ;; is not inside a recognized project.
                    (project-current-directory-override
                     (file-name-directory full-path)))
               (cond
                ((not backend)
                 (funcall callback
                          (list :result
                                (format "No xref backend available for %s"
                                        file-path))))
                (backend-error
                 (funcall
                  callback
                  (list :result backend-error)))
                ;; Special handling for etags without tags table
                ((and (eq backend 'etags)
                      (not (or (and (boundp 'tags-file-name) tags-file-name
                                    (file-exists-p tags-file-name))
                               (and (boundp 'tags-table-list) tags-table-list
                                    (cl-some #'file-exists-p tags-table-list)))))
                 (funcall callback
                          (list :result
                                (format "No tags table available for %s"
                                        file-path))))
                (t
                 (let ((xref-items (mevedel-tool-code--with-quiet-file-visit
                                      (xref-backend-apropos backend pattern))))
                   (funcall callback
                            (list :result
                                  (if xref-items
                                      (mevedel-tool-code--format-xref-items
                                       xref-items target)
                                    (format
                                     "No symbols found matching pattern '%s'"
                                     pattern))))))))
           (error
            (funcall callback
                     (list :result
                           (format
                            "Error searching for pattern '%s' in %s: %s"
                            pattern file-path
                            (error-message-string err)))))))))))


;;
;;; Imenu Integration

(defun mevedel-tool-code--imenu-leaves (index file-path &optional path)
  "Return one formatted line per Imenu leaf of INDEX located in FILE-PATH.
PATH holds the enclosing category names, outermost first.  Descend through
every nested subalist, so a leaf below more than one category is listed
rather than dropped.  Skip Imenu's special entries, which mark themselves
with a negative position rather than with a distinguished name.  Line
numbers are whole-buffer numbers, because Imenu builds its index widened
while the visiting buffer may be narrowed."
  (mapcan
   (lambda (item)
     (let* ((name (car-safe item))
            (value (cdr-safe item))
            ;; (NAME . POSITION), or Imenu's longer
            ;; (NAME POSITION FUNCTION ARGUMENTS...) item.
            (pos (if (consp value) (car value) value)))
       (cond
        ((not (stringp name)) nil)
        ((imenu--subalist-p item)
         (mevedel-tool-code--imenu-leaves
          value file-path (append path (list name))))
        ((not (integer-or-marker-p pos)) nil)
        ((< pos 0) nil)
        (t
         (list (format "%s:%d: %s%s" file-path
                       (line-number-at-pos pos t)
                       (if path
                           (format "[%s] " (string-join path " > "))
                         "")
                       name))))))
   index))

(defun mevedel-tool-code--imenu (callback args)
  "List symbols in a file using imenu.
CALLBACK receives the result envelope.  ARGS is a plist with :file_path."
  (let ((file-path (plist-get args :file_path)))
    (mevedel-tool-code--with-file-buffer
     file-path
     (lambda (file-path _full-path target-buffer)
       (condition-case err
           (with-current-buffer target-buffer
             (imenu--make-index-alist)
             (if imenu--index-alist
                 (let ((results (mevedel-tool-code--imenu-leaves
                                 imenu--index-alist file-path)))
                   (funcall callback
                            (list :result
                                  (if results
                                      (string-join results "\n")
                                    (format "No symbols found in %s"
                                            file-path)))))
               (funcall callback
                        (list :result
                              (format
                               "No imenu support or no symbols found in %s"
                               file-path)))))
         (error
          (funcall callback
                   (list :result
                         (format "Error listing symbols in %s: %s"
                                 file-path
                                 (error-message-string err))))))))))


;;
;;; Tree-sitter Integration

(defun mevedel-tool-code--treesitter (callback args)
  "Get tree-sitter syntax tree information for a file.
CALLBACK receives the result envelope.  ARGS is a plist with :file_path
and optional :line, :column, :whole_file, :include_ancestors,
:include_children."
  (let ((file-path (plist-get args :file_path))
        (line (plist-get args :line))
        (column (plist-get args :column))
        (whole-file (mevedel-tool-truthy-p (plist-get args :whole_file)))
        (include-ancestors (mevedel-tool-truthy-p
                            (plist-get args :include_ancestors)))
        (include-children (mevedel-tool-truthy-p
                           (plist-get args :include_children))))
    (mevedel-tool-code--with-file-buffer
     file-path
     (lambda (file-path _full-path target-buffer)
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
                              (list :result
                                    (mevedel-tool-code--treesit-format-tree
                                     root-node 20)))
                   (let ((results nil))
                     (push (format "Node Type: %s" (treesit-node-type node))
                           results)
                     (push (format "Range: %d-%d"
                                   (treesit-node-start node)
                                   (treesit-node-end node))
                           results)
                     (push (format "Text: %s"
                                   (mevedel--truncate-display (treesit-node-text node t)
                                    80 "..."))
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
                              (list :result
                                    (string-join (nreverse results)
                                                 "\n"))))))))
         (error
          (funcall callback
                   (list :result
                         (format
                          "Error getting tree-sitter info for %s: %s"
                          file-path (error-message-string err))))))))))

(defconst mevedel-tool-code--treesit-tree-max-chars (* 200 1024)
  "Hard cap on the characters a whole-file tree-sitter tree may occupy.
Sits well above the Treesitter tool result limit so the persisted
artifact stays useful, while a wide or generated file cannot spend
unbounded time and memory building a tree nothing can consume.")

(defun mevedel-tool-code--treesit-format-tree (node max-depth)
  "Format NODE and its children as a tree string.
MAX-DEPTH is the maximum depth to traverse.  Traversal stops once it has
built `mevedel-tool-code--treesit-tree-max-chars' characters and says so,
so a wide file cannot build an unbounded intermediate string."
  (with-temp-buffer
    (when (catch 'truncated
            (cl-labels
                ((insert-node
                   (node level)
                   (when (and node (< level max-depth))
                     (when (> (buffer-size)
                              mevedel-tool-code--treesit-tree-max-chars)
                       (throw 'truncated t))
                     ;; Read node text only when its span can fit the
                     ;; preview, so a wide file does not copy every large
                     ;; node just to reject it.
                     (let* ((start (treesit-node-start node))
                            (end (treesit-node-end node))
                            (field-name (treesit-node-field-name node))
                            (text (and (< (- end start) 40)
                                       (treesit-node-text node t))))
                       (insert (make-string (* level 2) ?\s)
                               (treesit-node-type node)
                               (if (treesit-node-check node 'named)
                                   " (named)"
                                 "")
                               (if field-name
                                   (format " [%s]" field-name)
                                 "")
                               (format " (%d-%d)" start end)
                               (if (and text (not (string-search "\n" text)))
                                   (format " \"%s\"" text)
                                 "")
                               "\n"))
                     (dotimes (i (treesit-node-child-count node))
                       (insert-node (treesit-node-child node i)
                                    (1+ level))))))
              (insert-node node 0))
            nil)
      (insert "... (tree truncated at the construction limit)\n"))
    (buffer-string)))

(defun mevedel-tool-code--line-column-to-point (line column)
  "Convert LINE and COLUMN to point position in current buffer.
LINE is 1-based, COLUMN is 0-based (Emacs convention).  Signals an error
for a coordinate the buffer does not have instead of clamping it to the
nearest position, so a stale or malformed location is repairable rather
than silently answered for somewhere else."
  (unless (and (natnump line) (> line 0))
    (error "Line must be 1 or greater: %S" line))
  (unless (natnump column)
    (error "Column must be 0 or greater: %S" column))
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (unless (and (zerop (forward-line (1- line))) (bolp))
        (error "Line %d is outside the buffer" line))
      (when (< (move-to-column column) column)
        (error "Column %d is beyond the end of line %d" column line))
      (point))))


;;
;;; Renderers

(defun mevedel-tool-code--display-file (path)
  "Return a compact display name for PATH."
  (if (and (stringp path) (not (string-empty-p path)))
      (file-name-nondirectory path)
    "?"))

(defun mevedel-tool-code--result-lines (result)
  "Return non-empty line count for RESULT."
  (if (stringp result)
      (length (split-string result "\n" t))
    0))

(defun mevedel-tool-code--location-count (result)
  "Return number of file:line style entries in RESULT."
  (if (or (not (stringp result))
          (string-prefix-p "Error" result)
          (string-prefix-p "No " result))
      0
    (mevedel-tool-code--result-lines result)))

(defun mevedel-tool-code--render-xref (name args result _render-data)
  "Return rendering plist for xref tool NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((target (or (plist-get args :identifier)
                       (plist-get args :pattern)
                       "?"))
           (file (mevedel-tool-code--display-file
                  (plist-get args :file_path)))
           (count (mevedel-tool-code--location-count result))
           (status (and (string-prefix-p "Error" result) 'error)))
      (list :header (format "%s: %s in %s (%d %s)"
                            (or name "Xref")
                            target
                            file
                            count
                            (if (= count 1) "match" "matches"))
            :body result
            :body-mode 'grep-mode
            :status status
            :initially-collapsed-p t))))

(defun mevedel-tool-code--render-imenu (name args result _render-data)
  "Return rendering plist for Imenu tool NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((file (mevedel-tool-code--display-file
                  (plist-get args :file_path)))
           (count (mevedel-tool-code--location-count result))
           (status (and (string-prefix-p "Error" result) 'error)))
      (list :header (format "%s: %s (%d %s)"
                            (or name "Imenu")
                            file
                            count
                            (if (= count 1) "symbol" "symbols"))
            :body result
            :body-mode 'grep-mode
            :status status
            :initially-collapsed-p t))))

(defun mevedel-tool-code--render-treesitter (name args result _render-data)
  "Return rendering plist for tree-sitter tool NAME with ARGS and RESULT."
  (when (stringp result)
    (let* ((file (mevedel-tool-code--display-file
                  (plist-get args :file_path)))
           (line (plist-get args :line))
           (whole-file (mevedel-tool-truthy-p
                        (plist-get args :whole_file)))
           (where (cond
                   (whole-file "whole file")
                   ((integerp line) (format "line %d" line))
                   (t "point")))
           (status (and (string-prefix-p "Error" result) 'error)))
      (list :header (format "%s: %s (%s)"
                            (or name "Treesitter")
                            file where)
            :body result
            :body-mode nil
            :status status
            :initially-collapsed-p t))))


;;
;;; Tool registration

(defun mevedel-tool-code--register ()
  "Register code exploration tools (Xref, Imenu, Treesitter)."

  (mevedel-define-tool
    :name "XrefReferences"
    :description "Find where a function, variable, or class is used throughout your codebase."
    :summary "LSP-aware symbol references, callers, and impact analysis."
    :prompt-file "prompts/tools/xref-references.md"
    :handler #'mevedel-tool-code--xref-references
    :args ((identifier string :required
                       "The exact identifier to find references for (case-sensitive).")
           (file_path path :required
                      "File path to use as context for the search (affects which xref backend is used)."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-code--render-xref)

  (mevedel-define-tool
    :name "XrefDefinitions"
    :description "Search for functions, variables, or classes by name pattern across your project."
    :summary "LSP-aware symbol definitions and name discovery."
    :prompt-file "prompts/tools/xref-definitions.md"
    :handler #'mevedel-tool-code--xref-definitions
    :args ((pattern string :required
                    "The pattern (substring or regex) to match symbol names.")
           (file_path path :required
                      "File path to use as context for the search."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-code--render-xref)

  (mevedel-define-tool
    :name "Imenu"
    :description "Navigate and explore a file's structure by listing all its functions, classes, and variables with their locations."
    :summary "Fast outline of functions, classes, and variables in one file."
    :prompt-file "prompts/tools/imenu.md"
    :handler #'mevedel-tool-code--imenu
    :args ((file_path path :required
                      "Path to the file to analyze for symbols."))
    :async-p t
    :read-only-p t
    :max-result-size 20000
    :groups (code)
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-code--render-imenu)

  (mevedel-define-tool
    :name "Treesitter"
    :description "Get tree-sitter syntax tree information for a file."
    :prompt-file "prompts/tools/treesitter.md"
    :handler #'mevedel-tool-code--treesitter
    :args ((file_path path :required
                      "Path to the file to analyze.")
           (line integer :optional
                 "Line number (1-based); must exist in the file.")
           (column integer :optional
                   "Column number (0-based); must exist on that line.")
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
    :get-path (lambda (args) (plist-get args :file_path))
    :renderer #'mevedel-tool-code--render-treesitter))

(provide 'mevedel-tool-code)
;;; mevedel-tool-code.el ends here
