;;; mevedel-tool-fs-search.el -- Glob, Grep, and resource privacy -*- lexical-binding: t -*-

;;; Commentary:

;; Search process orchestration and private resource-output rewriting.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-execution'
(declare-function mevedel-execution-start-helper
                  "mevedel-execution"
                  (callback name command read-paths writable-roots &rest keys))

;; `mevedel-resource'
(declare-function mevedel-resource-execute
                  "mevedel-resource" (attempt &optional executor options))

;; `mevedel-structs'
(defvar mevedel--session)

;; `mevedel-tool-fs'
(declare-function mevedel-tool-fs-executable-find "mevedel-tool-fs" (name path))
(declare-function mevedel-tool-fs-handler-result "mevedel-tool-fs" (result))
(declare-function mevedel-tool-fs-resource-attempt "mevedel-tool-fs" (address))
(declare-function mevedel-tool-fs-resource-child-address
                  "mevedel-tool-fs"
                  (address root path &optional address-prefix))
(declare-function mevedel-tool-fs-resource-rg-exclusions
                  "mevedel-tool-fs" (address))
(declare-function mevedel-tool-fs-strip-system-reminders
                  "mevedel-tool-fs" (result))
(declare-function mevedel-tool-fs-visible-path
                  "mevedel-tool-fs" (path &optional resource-address))

;; `mevedel-tool-registry'
(declare-function mevedel-tool-integer-arg "mevedel-tool-registry"
                  (args key &optional default))
(declare-function mevedel-tool-string-arg "mevedel-tool-registry"
                  (args key &optional default))
(declare-function mevedel-tool-truthy-p "mevedel-tool-registry" (value))

;; `mevedel-turn'
(declare-function mevedel-current-origin "mevedel-turn" ())

(defvar mevedel-tool-fs-search--resource-address nil
  "Authored resource address for the current search.")

(defvar mevedel-tool-fs-search--resource-dispatching nil
  "Non-nil while dispatching a resource attempt into a search handler.")

(defun mevedel-tool-fs-search--visible-path (path)
  "Return PATH in the current search operation's visible domain."
  (mevedel-tool-fs-visible-path path
                               mevedel-tool-fs-search--resource-address))

(defun mevedel-tool-fs-search--resource-search-roots (target)
  "Return helper roots carried by virtual resource TARGET, if any."
  (and (listp target)
       (plist-get target :virtual)
       (plist-get target :resource-search-roots)))

(defun mevedel-tool-fs-search--resource-search-candidate-end (line root)
  "Return the private path end in LINE below ROOT, or nil.

Resource searches use ripgrep's NUL path separator.  Prefer the longest
existing prefix for captured output without that separator, so a colon in a
filename is not mistaken for a line-number delimiter."
  (let* ((root (expand-file-name root))
         (directory (file-name-as-directory root)))
    (cond
     ((string= line root) (length line))
     ((and (string-prefix-p root line)
           (< (length root) (length line))
           (= (aref line (length root)) ?:))
      (length root))
     ((string-prefix-p directory line)
      (let ((position (length directory))
            candidate-end)
        (while (setq position (string-search ":" line position))
          (when (file-exists-p (substring line 0 position))
            (setq candidate-end position))
          (setq position (1+ position)))
        (or candidate-end (length line)))))))

(defun mevedel-tool-fs-search--resource-search-rewrite-path (address root path)
  "Rewrite private PATH below ROOT as a child of ADDRESS, or an error."
  (or (mevedel-tool-fs-resource-child-address address root path)
      (format "Error: Resource search returned an unsafe path for %s"
              address)))

(defun mevedel-tool-fs-search--rewrite-resource-search-output
    (output root address &optional path-list-p)
  "Replace private search paths in OUTPUT with logical ADDRESS paths.

Only path prefixes emitted by ripgrep are rewritten.  Match text and
context lines remain untouched.  PATH-LIST-P selects NUL-delimited file-list
output, used by Glob; content output keeps its match suffixes."
  (if (or (null output) (null root) (null address))
      output
    (if (string-search "\0" output)
        (if path-list-p
            (mapconcat
             (lambda (path)
               (mevedel-tool-fs-search--resource-search-rewrite-path
                address root path))
             (split-string output "\0" t) "\n")
          (let ((scan 0)
                (copy-start 0)
                pieces)
            (while-let ((nul (string-search "\0" output scan)))
              (let* ((newline (cl-position ?\n output :end nul :from-end t))
                     (line-start (if newline (1+ newline) 0))
                     (path (substring output line-start nul)))
                (push (substring output copy-start line-start) pieces)
                (push (mevedel-tool-fs-search--resource-search-rewrite-path
                       address root path)
                      pieces)
                (setq copy-start (1+ nul)
                      scan (1+ nul))))
            (push (substring output copy-start) pieces)
            (apply #'concat (nreverse pieces))))
      (let ((root (expand-file-name root))
            (lines (split-string output "\n" nil)))
        (mapconcat
         (lambda (line)
           (let* ((candidate-end
                   (mevedel-tool-fs-search--resource-search-candidate-end
                    line root))
                  (candidate (and candidate-end
                                  (substring line 0 candidate-end)))
                  (logical (and candidate
                                (mevedel-tool-fs-resource-child-address
                                 address root candidate))))
             (if logical
                 (concat logical (substring line (or candidate-end 0)))
               (if candidate
                   (format "Error: Resource search returned an unsafe path for %s"
                           address)
                 line))))
         lines "\n")))))

(defun mevedel-tool-fs-search--rewrite-resource-search-roots
    (output roots &optional path-list-p)
  "Replace private paths in multi-root resource OUTPUT.

ROOTS are plists with `:path', `:address-prefix', and optional `:label'.
The same path-prefix and containment checks as single-root rewriting apply
before a logical address is emitted."
  (if (or (null output) (null roots))
      output
    (let ((roots
           (sort (copy-sequence roots)
                 (lambda (left right)
                   (> (length (expand-file-name (plist-get left :path)))
                      (length (expand-file-name (plist-get right :path))))))))
      (if (and path-list-p (string-search "\0" output))
          (mapconcat
           (lambda (path)
             (or
              (cl-loop
               for root-data in roots
               for root = (expand-file-name (plist-get root-data :path))
               for logical =
               (mevedel-tool-fs-resource-child-address
                (or (plist-get root-data :address-prefix)
                    (plist-get root-data :address))
                root path (plist-get root-data :address-prefix))
               when logical
               return
               (concat logical
                       (when-let ((label (plist-get root-data :label)))
                         (concat "\t" label))))
              "Error: Resource search returned an unsafe path"))
           (split-string output "\0" t) "\n")
        (mapconcat
         (lambda (line)
           (or
             (cl-loop
             for root-data in roots
             for root = (expand-file-name (plist-get root-data :path))
             for candidate-end =
             (mevedel-tool-fs-search--resource-search-candidate-end line root)
             for candidate = (and candidate-end
                                  (substring line 0 candidate-end))
             for logical =
             (and candidate-end
                  (mevedel-tool-fs-resource-child-address
                   (or (plist-get root-data :address-prefix)
                       (plist-get root-data :address))
                   root candidate
                   (plist-get root-data :address-prefix)))
             if candidate-end
             return
             (if logical
                 (concat logical
                         (when-let ((label (plist-get root-data :label)))
                           (concat "\t" label))
                         (substring line candidate-end))
               (format "Error: Resource search returned an unsafe path")))
            line))
         (split-string output "\n" nil)
         "\n")))))

(defun mevedel-tool-fs-search--scrub-resource-search-output (output roots)
  "Replace physical ROOTS anywhere in resource search OUTPUT.

ROOTS are plists with `:path', `:address-prefix', or `:address'.  Longer
physical roots are replaced first so nested roots retain their own logical
addresses."
  (let ((roots
         (sort (copy-sequence roots)
               (lambda (left right)
                 (> (length (expand-file-name (plist-get left :path)))
                    (length (expand-file-name (plist-get right :path))))))))
    (dolist (root roots output)
      (let* ((physical-root
              (directory-file-name
               (expand-file-name (plist-get root :path))))
             (native-root
              (when-let ((path
                          (file-remote-p physical-root 'localname 'never)))
                (directory-file-name path)))
             (variants (delete-dups (delq nil (list physical-root native-root))))
             (address (or (plist-get root :address-prefix)
                          (plist-get root :address))))
        (dolist (variant variants)
          (setq output
                (if (string= variant "/")
                    (if (string-search variant output)
                        "Error: Resource search returned an unsafe path"
                      output)
                  (string-replace variant address output))))
        (when (cl-some (lambda (variant)
                         (string-search variant output))
                       variants)
          (setq output "Error: Resource search returned an unsafe path"))))))

(defun mevedel-tool-fs-search--rewrite-resource-handler-result
    (result root address &optional path-list-p)
  "Return RESULT with private search paths rewritten for ADDRESS.

Scrub ROOT wherever helper diagnostics include it."
  (if (and (proper-list-p result)
           (plist-member result :result)
           (stringp (plist-get result :result)))
      (let ((copy (copy-sequence result)))
        (plist-put
         copy :result
         (mevedel-tool-fs-search--scrub-resource-search-output
          (mevedel-tool-fs-search--rewrite-resource-search-output
           (plist-get result :result) root address path-list-p)
          (list (list :path root :address address))))
        copy)
    result))

(defun mevedel-tool-fs-search-render-grep (name args result _render-data)
  "Rendering plist for the Grep tool.
NAME is \"Grep\".  ARGS carries `:pattern' (the search regex).  RESULT
is the raw matches output.  Header shows the pattern and match count
\(one line per match); body fontifies as `grep-mode' for file:line
coloring.  `grep-mode' is autoloaded; `mevedel-view--fontify-as' falls
back to text verbatim if activation fails."
  (require 'mevedel-tool-fs)
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (visible (mevedel-tool-fs-strip-system-reminders result))
           (matches (if (or (string-prefix-p "No matches found" visible)
                            (string-prefix-p "Error:" visible))
                        0
                      (length (seq-filter (lambda (l) (not (string-empty-p l)))
                                          (split-string visible "\n"))))))
      (list :header (format "%s: %s (%d matches)"
                            (or name "Grep") pattern matches)
            :body result
            :body-mode 'grep-mode
            :initially-collapsed-p t))))

(defun mevedel-tool-fs-search-render-glob (name args result _render-data)
  "Rendering plist for the Glob tool.
NAME is \"Glob\".  ARGS carries `:pattern'.  RESULT is a newline-separated
list of matching files.  Header shows pattern and file count."
  (require 'mevedel-tool-fs)
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (lines (seq-filter (lambda (l) (not (string-empty-p l)))
                              (split-string result "\n")))
           (files (if (or (string-prefix-p "No files found" result)
                          (string-prefix-p "Error:" result))
                      0
                    (length (seq-filter
                             (lambda (line)
                               (not (string-prefix-p "... Results truncated" line)))
                             lines)))))
      (list :header (format "%s: %s (%d files)"
                            (or name "Glob") pattern files)
            :body result
            :body-mode nil
            :initially-collapsed-p t))))

;;
;;; Search execution

(defconst mevedel-tool-fs-search--grep-max-output-bytes (* 200 1024)
  "Hard cap on Grep tool output size in bytes.
Prevents catastrophic context overflow when searches hit files with
very long lines (e.g. JSON log files where a single match line can be
50KB+).  After line-count truncation, output exceeding this limit is
cut at the last complete line and a guidance message is appended.")

(defconst mevedel-tool-fs-search--glob-default-head-limit 100
  "Default number of file paths returned by one Glob call.")

(defconst mevedel-tool-fs-search--glob-max-output-bytes (* 30 1024)
  "Hard cap on Glob tool output size in bytes.")

(defconst mevedel-tool-fs-search--vcs-directory-names
  '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl")
  "Version-control metadata directory names excluded from searches.")

(defconst mevedel-tool-fs-search--rg-vcs-exclusions
  (mapcar (lambda (name)
            (format "--glob=!**/%s" name))
          mevedel-tool-fs-search--vcs-directory-names)
  "Ripgrep arguments excluding version-control metadata directories.")

(defcustom mevedel-tool-fs-search-timeout 20
  "Seconds before a Glob or Grep helper is terminated."
  :type 'number
  :group 'mevedel)

(defun mevedel-tool-fs-search--rg-outcome (child-result)
  "Classify CHILD-RESULT with termination facts before its exit code."
  (cond
   ((plist-get child-result :error) 'error)
   ((plist-get child-result :timed-out-p) 'timeout)
   ((plist-get child-result :output-limit-p) 'output-limit)
   ((= (plist-get child-result :exit-code) 0) 'success)
   ((= (plist-get child-result :exit-code) 1) 'no-match)
   (t 'failure)))

(defun mevedel-tool-fs-search--vcs-metadata-path-p (path)
  "Return non-nil when PATH resolves inside VCS metadata."
  (seq-some (lambda (component)
              (member component mevedel-tool-fs-search--vcs-directory-names))
            (file-name-split (file-truename path))))

(defun mevedel-tool-fs-search--truncate-output-buffer
    (maximum-size &optional guidance)
  "Truncate the current buffer at a complete line before MAXIMUM-SIZE bytes.
When GUIDANCE is non-nil, append a byte-limit message after truncation."
  (when (> (1- (position-bytes (point-max))) maximum-size)
    (let* ((notice (and guidance
                        (format "\n... Output truncated at %dK byte limit. %s"
                                (/ maximum-size 1024) guidance)))
           (content-limit (- maximum-size (string-bytes (or notice "")))))
      (if (< content-limit 0)
          (progn
            (erase-buffer)
            (insert (substring notice 0 maximum-size)))
        (goto-char (byte-to-position (1+ content-limit)))
        (beginning-of-line)
        (delete-region (point) (point-max))
        (when notice
          (goto-char (point-max))
          (insert notice))))))

(defun mevedel-tool-fs-search--finalize-glob-buffer ()
  "Bound current buffer and return the model-visible Glob result."
  (unless (save-excursion
            (goto-char (point-min))
            (looking-at-p (regexp-quote "No files found")))
    (goto-char (point-min))
    (let ((total-lines (count-lines (point-min) (point-max))))
      (when (> total-lines mevedel-tool-fs-search--glob-default-head-limit)
        (forward-line mevedel-tool-fs-search--glob-default-head-limit)
        (delete-region (point) (point-max))
        (goto-char (point-max))
        (insert
         (format "\n... Results truncated (limit: %d). Narrow your search with :path or a more specific :pattern."
                 mevedel-tool-fs-search--glob-default-head-limit)))))
  (mevedel-tool-fs-search--truncate-output-buffer
   mevedel-tool-fs-search--glob-max-output-bytes
   "Narrow your search with :path or a more specific :pattern.")
  (buffer-string))

(defun mevedel-tool-fs-search--normalize-rg-glob (root pattern)
  "Return a narrowed `(ROOT . PATTERN)' for ripgrep.
Reject absolute PATTERNs, parent traversal, and existing symlink escapes.
Return nil when the literal directory prefix does not exist."
  (when (file-name-absolute-p pattern)
    (error "Glob pattern must be relative"))
  (let* ((components (split-string pattern "/" t))
         (relative-components
          (seq-remove (lambda (component)
                        (string= component "."))
                      components))
         prefix)
    (when (member ".." relative-components)
      (error "Glob pattern must not traverse parent directories"))
    (while (and (cdr relative-components)
                (not (string-match-p "[][?*{]"
                                     (car relative-components))))
      (push (pop relative-components) prefix))
    (if (null prefix)
        (cons root pattern)
      (setq prefix (nreverse prefix))
      (unless (seq-some (lambda (component)
                          (member component
                                  mevedel-tool-fs-search--vcs-directory-names))
                        prefix)
        (let ((directory
               (expand-file-name (string-join prefix "/") root)))
          (when (and (file-directory-p directory)
                     (not (mevedel-tool-fs-search--vcs-metadata-path-p
                           directory)))
            (let ((true-root (file-name-as-directory (file-truename root)))
                  (true-directory
                   (file-name-as-directory (file-truename directory))))
              (unless (or (string= true-root true-directory)
                          (file-in-directory-p true-directory true-root))
                (error "Glob pattern escapes the search root"))
              (cons directory
                    (string-join relative-components "/")))))))))

(defun mevedel-tool-fs-search--prepend-partial-warning
    (warning result maximum-size)
  "Prepend WARNING to RESULT without exceeding MAXIMUM-SIZE.
Return RESULT unchanged when WARNING is nil."
  (if (null warning)
      result
    (with-temp-buffer
      (insert warning result)
      (mevedel-tool-fs-search--truncate-output-buffer maximum-size)
      (buffer-string))))

(defun mevedel-tool-fs-search--settle-rg-result
    (child-result operation no-match failure-guidance)
  "Insert CHILD-RESULT output and return its settlement metadata.
OPERATION names the user-facing operation.  NO-MATCH is its empty result,
and FAILURE-GUIDANCE tells the caller how to narrow a failed invocation."
  (let* ((output (or (plist-get child-result :output) ""))
         (outcome (mevedel-tool-fs-search--rg-outcome child-result))
         partial-warning pageable-p)
    (insert (replace-regexp-in-string "\r\n?" "\n" output))
    (pcase outcome
      ('error
       (erase-buffer)
       (insert (format "Error: %s failed to start: %s"
                       operation
                       (error-message-string
                        (plist-get child-result :error)))))
      ((or 'timeout 'output-limit)
       (let ((problem (if (eq outcome 'timeout)
                          "timed out"
                        "reached its output limit")))
         (if (string-empty-p output)
             (insert (format "Error: %s %s; narrow the search"
                             operation problem))
           (setq partial-warning
                 (format "Warning: %s %s; results are partial. Narrow the search.\n\n"
                         operation problem)
                 pageable-p t))))
      ('success (setq pageable-p t))
      ('no-match
       (erase-buffer)
       (insert no-match))
      ('failure
       (goto-char (point-min))
       (insert (format "Error: %s failed (exit code %d). %s\n\n"
                       operation (plist-get child-result :exit-code)
                       failure-guidance))))
    (list :partial-warning partial-warning :pageable-p pageable-p)))

(defun mevedel-tool-fs-search-glob (callback args)
  "Find files matching a glob pattern using ripgrep.
CALLBACK receives the result envelope.  ARGS is a plist with :pattern
and optional :path."
  (require 'cl-lib)
  (require 'mevedel-resource)
  (require 'mevedel-tool-fs)
  (require 'mevedel-turn)
  (let* ((address (plist-get args :path))
         (attempt (and (not mevedel-tool-fs-search--resource-dispatching)
                       (mevedel-tool-fs-resource-attempt address))))
    (if attempt
        (mevedel-resource-execute
         attempt
         (lambda (path authored)
           (if (mevedel-tool-fs-search--resource-search-roots path)
               (let ((native-args (copy-sequence args))
                     (mevedel-tool-fs-search--resource-address authored)
                     (mevedel-tool-fs-search--resource-dispatching t))
                 (plist-put native-args :resource-roots
                            (mevedel-tool-fs-search--resource-search-roots path))
                 (plist-put native-args :path nil)
                 (mevedel-tool-fs-search-glob callback native-args))
             (if (and (listp path) (plist-get path :virtual))
                 (funcall callback
                          (mevedel-tool-fs-handler-result
                           (plist-get path :result)))
               (unless (and (stringp path) (file-exists-p path))
                 (error "Resource %s is not available for Glob" authored))
               (let ((native-args (copy-sequence args))
                     (mevedel-tool-fs-search--resource-address authored)
                     (mevedel-tool-fs-search--resource-dispatching t))
                 (plist-put native-args :path path)
                 (mevedel-tool-fs-search-glob
                  (lambda (result)
                    (funcall
                     callback
                     (mevedel-tool-fs-search--rewrite-resource-handler-result
                      result path authored t)))
                  native-args))))))
      (let* ((pattern (plist-get args :pattern))
             (path (plist-get args :path))
             (resource-roots (plist-get args :resource-roots)))
        (when (string-empty-p pattern)
          (error "Pattern must not be empty"))
        (let* ((resource-paths
                (and resource-roots
                     (mapcar (lambda (root)
                               (expand-file-name (plist-get root :path)))
                             resource-roots)))
               (paths (or resource-paths
                          (list (expand-file-name (or path "."))))))
          (unless (mevedel-tool-fs-executable-find "rg" (car paths))
            (error "'rg' not installed on execution target"))
          (unless (cl-every (lambda (root)
                              (and (file-readable-p root)
                                   (or (null resource-roots)
                                       (file-directory-p root))))
                            paths)
            (error "Path %s is not a readable directory"
                   (mevedel-tool-fs-search--visible-path (car paths))))
          (if resource-roots
              (let* ((session (bound-and-true-p mevedel--session))
                     (rg-args
                      (append (list "--files" "--hidden" "--no-ignore"
                                    "--color=never" "--iglob" pattern)
                              (when (or resource-roots
                                        mevedel-tool-fs-search--resource-address)
                                (list "--null"))
                              (mevedel-tool-fs-resource-rg-exclusions
                               (plist-get args :path))
                              mevedel-tool-fs-search--rg-vcs-exclusions
                              paths)))
                (require 'mevedel-execution)
                (mevedel-execution-start-helper
                 (lambda (child-result)
                   (with-temp-buffer
                     (let* ((settlement
                             (mevedel-tool-fs-search--settle-rg-result
                              child-result "glob" "No files found matching pattern"
                              "Narrow :path or :pattern."))
                            (partial-warning
                             (plist-get settlement :partial-warning)))
                       (let ((settled (buffer-string)))
                         (erase-buffer)
                         (insert
                          (mevedel-tool-fs-search--scrub-resource-search-output
                           (mevedel-tool-fs-search--rewrite-resource-search-roots
                            settled
                            resource-roots
                            t)
                           resource-roots))
                         (let ((result (mevedel-tool-fs-search--finalize-glob-buffer)))
                           (funcall callback
                                    (mevedel-tool-fs-handler-result
                                     (mevedel-tool-fs-search--prepend-partial-warning
                                      partial-warning result
                                      mevedel-tool-fs-search--glob-max-output-bytes))))))))
                 "mevedel-glob" (cons "rg" rg-args) paths nil
                 :session session :owner (mevedel-current-origin)
                 :timeout mevedel-tool-fs-search-timeout))
            (let ((path (car paths)))
              (if-let ((normalized
                        (and (not (mevedel-tool-fs-search--vcs-metadata-path-p path))
                             (mevedel-tool-fs-search--normalize-rg-glob path pattern))))
                  (let* ((path (car normalized))
                         (pattern (cdr normalized))
                         (session (bound-and-true-p mevedel--session))
                         (rg-args
                          (append (list "--files" "--hidden" "--no-ignore"
                                        "--color=never" "--iglob" pattern)
                                  (when mevedel-tool-fs-search--resource-address
                                    (list "--null"))
                                  (mevedel-tool-fs-resource-rg-exclusions
                                   (or address mevedel-tool-fs-search--resource-address))
                                  mevedel-tool-fs-search--rg-vcs-exclusions
                                  (list path))))
                    (require 'mevedel-execution)
                    (mevedel-execution-start-helper
                     (lambda (child-result)
                       (with-temp-buffer
                         (let* ((settlement
                                 (mevedel-tool-fs-search--settle-rg-result
                                  child-result "glob" "No files found matching pattern"
                                  "Narrow :path or :pattern."))
                                (partial-warning
                                 (plist-get settlement :partial-warning)))
                           (let ((result (mevedel-tool-fs-search--finalize-glob-buffer)))
                             (funcall callback
                                      (mevedel-tool-fs-handler-result
                                       (mevedel-tool-fs-search--prepend-partial-warning
                                        partial-warning result
                                        mevedel-tool-fs-search--glob-max-output-bytes)))))))
                     "mevedel-glob" (cons "rg" rg-args) (list path) nil
                     :session session :owner (mevedel-current-origin)
                     :timeout mevedel-tool-fs-search-timeout))
                (funcall callback
                         (mevedel-tool-fs-handler-result
                          "No files found matching pattern"))))))))))

(defun mevedel-tool-fs-search-grep (callback args)
  "Search file contents with ripgrep.
CALLBACK receives the result envelope.  ARGS is a plist with :pattern and
optional :path, :glob, :output_mode, :head_limit, :offset, :-i, :-n,
:type, :multiline, :context, :-A, :-B, :-C."
  (require 'cl-lib)
  (require 'mevedel-resource)
  (require 'mevedel-tool-fs)
  (require 'mevedel-tool-registry)
  (require 'mevedel-turn)
  (let* ((address (plist-get args :path))
         (attempt (and (not mevedel-tool-fs-search--resource-dispatching)
                       (mevedel-tool-fs-resource-attempt address))))
    (if attempt
        (mevedel-resource-execute
         attempt
         (lambda (path authored)
           (if (mevedel-tool-fs-search--resource-search-roots path)
               (let ((native-args (copy-sequence args))
                     (mevedel-tool-fs-search--resource-address authored)
                     (mevedel-tool-fs-search--resource-dispatching t))
                 (plist-put native-args :resource-roots
                            (mevedel-tool-fs-search--resource-search-roots path))
                 (plist-put native-args :resource-address authored)
                 (plist-put native-args :path nil)
                 (mevedel-tool-fs-search-grep callback native-args))
             (if (and (listp path) (plist-get path :virtual))
                 (funcall callback
                          (mevedel-tool-fs-handler-result
                           (plist-get path :result)))
               (unless (and (stringp path) (file-exists-p path))
                 (error "Resource %s is not available for Grep" authored))
               (let ((native-args (copy-sequence args))
                     (mevedel-tool-fs-search--resource-address authored)
                     (mevedel-tool-fs-search--resource-dispatching t))
                 (plist-put native-args :path path)
                 (mevedel-tool-fs-search-grep
                  (lambda (result)
                    (funcall
                     callback
                     (mevedel-tool-fs-search--rewrite-resource-handler-result
                      result path authored)))
                  native-args))))))
      (let* ((pattern (plist-get args :pattern))
             (path (mevedel-tool-string-arg args :path "."))
             (resource-roots (plist-get args :resource-roots))
             (resource-address (plist-get args :resource-address))
             (file-glob (mevedel-tool-string-arg args :glob))
             (output-mode (mevedel-tool-string-arg
                           args :output_mode "files_with_matches"))
             (head-limit (let ((v (plist-get args :head_limit)))
                           (cond ((null v) 250)
                                 ((and (integerp v) (= v 0)) nil)
                                 ((and (integerp v) (< v 0))
                                  (error "Grep :head_limit must be 0 or greater"))
                                 ((integerp v) v)
                                 (t 250))))
             (offset (let ((v (or (mevedel-tool-integer-arg args :offset) 0)))
                       (when (< v 0)
                         (error "Grep :offset must be 0 or greater"))
                       v))
             (case-fold (mevedel-tool-truthy-p (plist-get args :-i)))
             (line-numbers (let ((v (plist-get args :-n)))
                             (if (null v)
                                 (equal output-mode "content")
                               (mevedel-tool-truthy-p v))))
             (file-type (mevedel-tool-string-arg args :type))
             (multiline (mevedel-tool-truthy-p (plist-get args :multiline)))
             (ctx-after (mevedel-tool-integer-arg args :-A))
             (ctx-before (mevedel-tool-integer-arg args :-B))
             (ctx-around (or (mevedel-tool-integer-arg args :-C)
                             (mevedel-tool-integer-arg args :context))))
        (setq path (expand-file-name path))
        (unless (mevedel-tool-fs-executable-find "rg" path)
          (error "'rg' not installed on execution target"))
        (let* ((resource-paths
                (and resource-roots
                     (mapcar (lambda (root)
                               (expand-file-name (plist-get root :path)))
                             resource-roots)))
               (search-roots (or resource-paths (list path))))
          (unless (cl-every (lambda (root)
                              (and (file-readable-p root)
                                   (if resource-roots
                                       (file-directory-p root)
                                     (file-exists-p root))))
                            search-roots)
            (error "Path %s is not readable"
                   (mevedel-tool-fs-search--visible-path (car search-roots))))
          (let* ((vcs-metadata-p (and (null resource-roots)
                                      (mevedel-tool-fs-search--vcs-metadata-path-p path)))
                 (normalized (and (null resource-roots)
                                  (not vcs-metadata-p)
                                  file-glob
                                  (file-directory-p path)
                                  (mevedel-tool-fs-search--normalize-rg-glob
                                   path file-glob))))
            (when vcs-metadata-p
              (setq path nil))
            (when (and path file-glob (file-directory-p path))
              (if normalized
                  (setq path (car normalized)
                        file-glob (cdr normalized))
                (setq path nil)))
            (if (null path)
                (funcall callback
                         (mevedel-tool-fs-handler-result "No matches found"))
              (let ((rg-args (list "--hidden" "--no-require-git"))
                    (session (bound-and-true-p mevedel--session)))
                ;; Output mode flags
                (pcase output-mode
                  ("content"
                   (when line-numbers (push "--line-number" rg-args))
                   (push "--heading" rg-args)
                   (when ctx-after (push (format "-A%d" ctx-after) rg-args))
                   (when ctx-before (push (format "-B%d" ctx-before) rg-args))
                   (when ctx-around (push (format "-C%d" ctx-around) rg-args))
                   (push "--max-count=1000" rg-args)
                   ;; Truncate long lines to prevent log files and other
                   ;; long-line sources from blowing up tool result size.
                   (push "--max-columns=2000" rg-args)
                   (push "--max-columns-preview" rg-args))
                  ("files_with_matches"
                   (push "--files-with-matches" rg-args))
                  ("count"
                   (push "--count" rg-args)))
                ;; Common flags
                (when case-fold (push "-i" rg-args))
                (when multiline (push "-U" rg-args) (push "--multiline-dotall" rg-args))
                (when file-glob (push (format "--glob=%s" file-glob) rg-args))
                (when file-type (push (format "--type=%s" file-type) rg-args))
                (when (or resource-roots mevedel-tool-fs-search--resource-address)
                  (push "--null" rg-args))
                (setq rg-args (nreverse rg-args))
                (setq rg-args
                      (append
                       rg-args
                       (mevedel-tool-fs-resource-rg-exclusions
                        (or resource-address mevedel-tool-fs-search--resource-address))
                       mevedel-tool-fs-search--rg-vcs-exclusions
                       (list "-e" pattern)
                       (or resource-paths (list path))))
                (require 'mevedel-execution)
                (mevedel-execution-start-helper
                 (lambda (child-result)
                   (with-temp-buffer
                     (let* ((settlement
                             (mevedel-tool-fs-search--settle-rg-result
                              child-result "search" "No matches found"
                              "Narrow :path, :glob, :type, or :pattern."))
                            (partial-warning
                             (plist-get settlement :partial-warning))
                            (pageable-output-p
                             (plist-get settlement :pageable-p)))
                       (when resource-roots
                         (let ((rewritten
                                (mevedel-tool-fs-search--scrub-resource-search-output
                                 (mevedel-tool-fs-search--rewrite-resource-search-roots
                                  (buffer-string) resource-roots)
                                 resource-roots)))
                           (erase-buffer)
                           (insert rewritten)))
                       ;; Apply offset and head_limit.
                       (when (and pageable-output-p
                                  (or (> offset 0) head-limit))
                         (goto-char (point-min))
                         (let ((total-lines (count-lines (point-min) (point-max))))
                           ;; An offset past the last output line would
                           ;; leave an empty success indistinguishable
                           ;; from no matches; answer about the range,
                           ;; and about nothing else.
                           (if (and (> offset 0) (>= offset total-lines))
                               (progn
                                 (erase-buffer)
                                 (insert
                                  (format "Offset %d starts after the last of %d output lines.  Lower :offset or repeat the search."
                                          offset total-lines)))
                             (when (> offset 0)
                               (forward-line offset)
                               (delete-region (point-min) (point))
                               (cl-decf total-lines offset))
                             (when (and head-limit (> total-lines head-limit))
                               (goto-char (point-min))
                               (forward-line head-limit)
                               (delete-region (point) (point-max))
                               (goto-char (point-max))
                               (insert
                                (format "\n... Results truncated (limit: %d, offset: %d)"
                                        head-limit offset))))))
                       ;; Bound total output even after line-count truncation.
                       (mevedel-tool-fs-search--truncate-output-buffer
                        mevedel-tool-fs-search--grep-max-output-bytes
                        "Narrow your search with :glob, :type, or a more specific :pattern.")
                       (funcall
                        callback
                        (mevedel-tool-fs-handler-result
                         (mevedel-tool-fs-search--prepend-partial-warning
                          partial-warning
                          (buffer-string)
                          mevedel-tool-fs-search--grep-max-output-bytes))))))
                 "mevedel-grep" (cons "rg" rg-args) search-roots nil
                 :session session :owner (mevedel-current-origin)
                 :timeout mevedel-tool-fs-search-timeout)))))))))

(provide 'mevedel-tool-fs-search)

;;; mevedel-tool-fs-search.el ends here
