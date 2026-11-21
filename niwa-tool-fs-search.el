;;; niwa-tool-fs-search.el -- Glob, Grep, and resource privacy -*- lexical-binding: t -*-

;;; Commentary:

;; Search process orchestration and private resource-output rewriting.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'niwa-resource)
(require 'niwa-tool-fs)
(require 'niwa-execution)
(require 'niwa-tool-registry)
(require 'niwa-turn)

;; `niwa-execution'
(declare-function niwa-execution-start-helper
                  "niwa-execution"
                  (callback name command read-paths writable-roots &rest keys))

;; `niwa-pipeline'
(declare-function niwa-pipeline-handler-resumable "niwa-pipeline" (function))
(autoload 'niwa-pipeline-handler-resumable "niwa-pipeline")

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-catalog "niwa-artifact-backend"
                  (workspace &optional detail))
(autoload 'niwa-artifact-backend-catalog "niwa-artifact-backend")

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)

;; `niwa-tool-editing'
(declare-function niwa-tool-editing-view "niwa-tool-editing" (components callback &optional search))
(autoload 'niwa-tool-editing-view "niwa-tool-editing")

;; `niwa-history-search'
(declare-function niwa-history-search-start
                  "niwa-history-search" (callback args descriptor))
(autoload 'niwa-history-search-start "niwa-history-search")

;; `niwa-resource'
(declare-function niwa-resource-error-message "niwa-resource"
                  (failure &optional address private-paths))
(declare-function niwa-resource-execute-async
                  "niwa-resource" (attempt callback executor &optional options))

;; `niwa-structs'
(defvar niwa--session)

;; `niwa-tool-fs'
(declare-function niwa-tool-fs-executable-find "niwa-tool-fs" (name path))
(declare-function niwa-tool-fs-handler-result "niwa-tool-fs" (result))
(declare-function niwa-tool-fs-resource-attempt "niwa-tool-fs" (address))
(declare-function niwa-tool-fs-resource-child-address
                  "niwa-tool-fs"
                  (address root path &optional address-prefix))
(declare-function niwa-tool-fs-resource-rg-exclusions
                  "niwa-tool-fs" (address))
(declare-function niwa-tool-fs-strip-system-reminders
                  "niwa-tool-fs" (result))
(declare-function niwa-tool-fs-visible-path
                  "niwa-tool-fs" (path &optional resource-address))

;; `niwa-tool-registry'
(declare-function niwa-tool-integer-arg "niwa-tool-registry"
                  (args key &optional default))
(declare-function niwa-tool-string-arg "niwa-tool-registry"
                  (args key &optional default))
(declare-function niwa-tool-truthy-p "niwa-tool-registry" (value))

;; `niwa-turn'
(declare-function niwa-current-origin "niwa-turn" ())

(defvar niwa-tool-fs-search--resource-address nil
  "Authored resource address for the current search.")

(defvar niwa-tool-fs-search--resource-dispatching nil
  "Non-nil while dispatching a resource attempt into a search handler.")

(defvar niwa-tool-fs-search--teardown nil
  "Optional cleanup for copied virtual documents if the helper is torn down.")

(defvar niwa-tool-fs-search--local-snapshot nil
  "Non-nil when the helper searches an authorized client-local copy.")

(defun niwa-tool-fs-search--documents (callback args descriptor operation)
  "Search validated virtual documents from DESCRIPTOR with OPERATION.
Reuse ordinary search options in ARGS and send the normal result to CALLBACK.
Only the authorized document snapshots enter the helper's read scope.  Like
remote media conversion, this processes an already transferred copy locally."
  (let* ((documents (plist-get descriptor :resource-search-documents))
         (address (plist-get descriptor :address))
         (directory (make-temp-file "niwa-resource-search-" t))
         (cleanup (lambda () (ignore-errors (delete-directory directory t)))))
    (condition-case err
        (let ((default-directory (file-name-as-directory directory))
              (niwa-tool-fs-search--local-snapshot t)
              (niwa-tool-fs-search--resource-dispatching t)
              (niwa-tool-fs-search--resource-address address)
              (niwa-tool-fs-search--teardown cleanup)
              (native (copy-sequence args)))
          (dolist (document documents)
            (unless (and (stringp (car document))
                         (not (file-name-absolute-p (car document)))
                         (cl-every (lambda (part) (not (member part '("" "." ".."))))
                                   (split-string (car document) "/"))
                         (stringp (cdr document)))
              (error "Internal resource error: invalid search document"))
            (make-directory (file-name-directory (file-name-concat directory (car document))) t)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (cdr document) nil
                            (file-name-concat directory (car document))
                            nil 'silent)))
          (plist-put native :path nil)
          (plist-put native :resource-roots
                     (list (list :path directory
                                 :address (file-name-directory address))))
          (plist-put native :resource-address address)
          (plist-put native :resource-search-maps (plist-get descriptor :resource-search-maps))
          (funcall operation
                   (lambda (result)
                     (unwind-protect (funcall callback result) (funcall cleanup)))
                   native))
      (error
       (funcall cleanup)
       (error "Could not search resource %s: %s" address
              (niwa-resource-error-message err address (list directory)))))))

(defun niwa-tool-fs-search--shared (callback args descriptor operation)
  "Grep the shared item text views DESCRIPTOR names with OPERATION and ARGS.
A bare `shared://' searches every item's expanded overview.  The editing host
answers asynchronously, so the views are fetched first, one at a time, and
then searched as virtual documents whose results CALLBACK receives."
  (let* ((components (plist-get descriptor :shared-view))
         (address (plist-get descriptor :address))
         (views (cond
                 ((null components)
                  (mapcar (lambda (item)
                            (let ((id (plist-get item :id))) (cons id (list id))))
                          (niwa-artifact-backend-catalog
                           (niwa-session-workspace niwa--session) 'items)))
                 ((or (equal (car (last components)) "view.png")
                      (equal (car (last components)) "sheet.png")
                      (equal (cadr components) "images"))
                  (error "Grep searches text; %s is an image" address))
                 (t (list (cons (car (last components)) components)))))
         (documents nil)
         (maps nil)
         (next nil))
    (setq next
          (niwa-pipeline-handler-resumable
           (lambda ()
             (if (null views)
                 (niwa-tool-fs-search--documents
                  callback args
                  (list :resource-search-documents (nreverse documents)
                        :resource-search-maps (nreverse maps) :address address)
                  operation)
               (pcase-let ((`(,_name . ,parts) (pop views)))
                 (niwa-tool-editing-view
                  parts
                  (lambda (view)
                    (if-let* ((message (plist-get view :error)))
                        (funcall callback
                                 (list :result (format "Error: Could not search %s: %s"
                                                       address message)
                                       :status 'error))
                      (mapc (lambda (document)
                              (let ((name (format "view-%d/%s" (length documents)
                                                  (file-name-nondirectory (plist-get document :address)))))
                                (push (cons name (plist-get document :text)) documents)
                                (push (cons name document) maps)))
                            (plist-get view :documents))
                      (funcall next))) t))))))
    (if views
        (funcall next)
      (funcall callback (list :result (format "No shared items to search under %s" address))))))

(defun niwa-tool-fs-search--shared-line (spans byte)
  "Map search BYTE to the physical Read line in sorted byte SPANS."
  (let ((lo 0) (hi (length spans)))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi) 2)))
        (if (<= (aref (aref spans mid) 1) byte)
            (setq lo (1+ mid))
          (setq hi mid))))
    (aref (aref spans (min lo (1- (length spans)))) 2)))

(defun niwa-tool-fs-search--shared-name (path)
  "Return the private snapshot key for ripgrep result PATH."
  (concat (file-name-nondirectory (directory-file-name (file-name-directory path)))
          "/" (file-name-nondirectory path)))

(defun niwa-tool-fs-search--shared-output
    (output maps mode numbered before after partial)
  "Map ripgrep OUTPUT with MAPS to actual Read locations.
MODE and NUMBERED select ordinary Grep output.  BEFORE and AFTER count real
Read context lines.  Files mode uses NUL-delimited paths and short-circuits
matching.  Other modes use JSON; PARTIAL allows an incomplete final event."
  (let ((hits (make-hash-table :test #'equal)) out)
    (if (equal mode "files_with_matches")
        (dolist (path (butlast (split-string output "\0")))
          (let ((name (niwa-tool-fs-search--shared-name path)))
            (unless (assoc name maps) (error "Unknown shared search document"))
            (puthash name t hits)))
      (dolist (row (split-string output "\n" t))
      (let ((event (condition-case nil
                       (json-parse-string row :object-type 'plist :array-type 'list)
                     (error (unless partial (error "Invalid shared search result"))))))
        (when (equal (plist-get event :type) "match")
          (let* ((data (plist-get event :data))
                 (path (plist-get (plist-get data :path) :text))
                 (name (niwa-tool-fs-search--shared-name path))
                 (document (cdr (assoc name maps)))
                 (spans (plist-get document :spans))
                 (base (plist-get data :absolute_offset))
                 (selected (or (gethash name hits) (make-hash-table :test #'eql))))
            (unless (and document (> (length spans) 0)) (error "Unknown shared search document"))
            (dolist (match (plist-get data :submatches))
              (let* ((start (+ base (plist-get match :start)))
                     (end (+ base (plist-get match :end)))
                     (first (niwa-tool-fs-search--shared-line spans start))
                     (last (niwa-tool-fs-search--shared-line spans (max start (1- end)))))
                (cl-loop for line from first to last do (puthash line t selected))))
            (puthash name selected hits))))))
    (dolist (entry maps)
      (when-let* ((selected (gethash (car entry) hits)))
        (let* ((document (cdr entry))
               (address (plist-get document :address))
               (lines (plist-get document :lines)))
          (pcase mode
            ("files_with_matches" (push address out))
            ("count" (push (format "%s:%d" address (hash-table-count selected)) out))
            ("content"
             (let ((visible (make-hash-table :test #'eql)) previous)
               (maphash (lambda (line _)
                          (cl-loop for n from (max 1 (- line before))
                                   to (min (length lines) (+ line after))
                                   do (puthash n t visible))) selected)
               (dolist (line (sort (hash-table-keys visible) #'<))
                 (when (and previous (> line (1+ previous)) (> (+ before after) 0))
                   (push "--" out))
                 (let ((sep (if (gethash line selected) ":" "-")))
                   (push (concat address sep (when numbered (format "%d%s" line sep))
                                 (aref lines (1- line))) out))
                 (setq previous line))))))))
    (if out (concat (string-join (nreverse out) "\n") "\n") "")))

(defun niwa-tool-fs-search--visible-path (path)
  "Return PATH in the current search operation's visible domain."
  (niwa-tool-fs-visible-path path
                               niwa-tool-fs-search--resource-address))

(defun niwa-tool-fs-search--resource-search-roots (target)
  "Return helper roots carried by virtual resource TARGET, if any."
  (and (listp target)
       (plist-get target :virtual)
       (plist-get target :resource-search-roots)))

(defun niwa-tool-fs-search--resource-search-candidate-end (line root)
  "Return the private path end in LINE below ROOT, or nil.

Resource searches use ripgrep's NUL path separator.  Prefer the longest
existing prefix for captured output without that separator, so a colon in a
filename is not mistaken for a line-number delimiter."
  (let* ((root (expand-file-name root))
         (remote (file-remote-p root))
         (root (if (and remote (not (string-prefix-p remote line)))
                   (file-local-name root) root))
         (directory (file-name-as-directory root)))
    (cond
     ((string= line root) (length line))
     ((string-prefix-p (concat root "\0") line) (length root))
     ((and (string-prefix-p directory line) (string-search "\0" line)))
     ((and (string-prefix-p root line)
           (< (length root) (length line))
           (= (aref line (length root)) ?:))
      (length root))
     ((string-prefix-p directory line)
      (let ((position (length directory))
            candidate-end)
        (while (setq position (string-search ":" line position))
          (when (file-exists-p (concat (and remote (not (file-remote-p line)) remote)
                                       (substring line 0 position)))
            (setq candidate-end position))
          (setq position (1+ position)))
        (or candidate-end (length line)))))))

(defun niwa-tool-fs-search--resource-search-rewrite-path (address root path)
  "Rewrite private PATH below ROOT as a child of ADDRESS, or an error."
  (or (niwa-tool-fs-resource-child-address address root path)
      (format "Error: Resource search returned an unsafe path for %s"
              address)))

(defun niwa-tool-fs-search--rewrite-resource-search-output
    (output root address &optional path-list-p separator)
  "Replace private search paths in OUTPUT with logical ADDRESS paths.

Only path prefixes emitted by ripgrep are rewritten.  Match text and
context lines remain untouched.  PATH-LIST-P selects NUL-delimited file-list
output, used by Glob.  SEPARATOR replaces each NUL path delimiter in other
output: a newline for headings, or a colon for counts."
  (if (or (null output) (null root) (null address))
      output
    (if (string-search "\0" output)
        (if path-list-p
            (mapconcat
             (lambda (path)
               (niwa-tool-fs-search--resource-search-rewrite-path
                address root path))
             (let ((paths (split-string output "\0" t)))
               (if (string-prefix-p "niwa://" address)
                   (cl-remove-if-not
                    (lambda (path) (string-suffix-p ".md" path))
                    paths)
                 paths))
             "\n")
          (let ((scan 0)
                (copy-start 0)
                pieces)
            (while-let ((nul (string-search "\0" output scan)))
              (let* ((newline (cl-position ?\n output :end nul :from-end t))
                     (line-start (if newline (1+ newline) 0))
                     (path (substring output line-start nul)))
                (push (substring output copy-start line-start) pieces)
                (push (niwa-tool-fs-search--resource-search-rewrite-path
                       address root path)
                      pieces)
                (push separator pieces)
                (setq copy-start (1+ nul)
                      scan (1+ nul))))
            (push (substring output copy-start) pieces)
            (apply #'concat (nreverse pieces))))
      (let ((root (expand-file-name root))
            (lines (split-string output "\n" nil)))
        (mapconcat
         (lambda (line)
           (let* ((candidate-end
                   (niwa-tool-fs-search--resource-search-candidate-end
                    line root))
                  (candidate (and candidate-end
                                  (substring line 0 candidate-end)))
                  (logical (and candidate
                                (niwa-tool-fs-resource-child-address
                                 address root candidate))))
             (if logical
                 (concat logical (substring line (or candidate-end 0)))
               (if candidate
                   (format "Error: Resource search returned an unsafe path for %s"
                           address)
                 line))))
         lines "\n")))))

(defun niwa-tool-fs-search--rewrite-resource-search-roots
    (output roots &optional path-list-p separator)
  "Replace private paths in multi-root resource OUTPUT.

ROOTS are plists with `:path', `:address-prefix', and optional `:label'.
The same path-prefix and containment checks as single-root rewriting apply
before a logical address is emitted.  PATH-LIST-P selects file lists;
otherwise SEPARATOR replaces the NUL path delimiter."
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
               (niwa-tool-fs-resource-child-address
                (or (plist-get root-data :address-prefix)
                    (plist-get root-data :address))
                root path (plist-get root-data :address-prefix))
               when logical
               return
               (concat logical
                       (when-let* ((label (plist-get root-data :label)))
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
             (niwa-tool-fs-search--resource-search-candidate-end line root)
             for candidate = (and candidate-end
                                  (substring line 0 candidate-end))
             for logical =
             (and candidate-end
                  (niwa-tool-fs-resource-child-address
                   (or (plist-get root-data :address-prefix)
                       (plist-get root-data :address))
                   root candidate
                   (plist-get root-data :address-prefix)))
             for nul-p = (and candidate-end
                              (< candidate-end (length line))
                              (= (aref line candidate-end) 0))
             if candidate-end
             return
             (if logical
                 (concat logical
                         (when-let* ((label (plist-get root-data :label)))
                           (concat "\t" label))
                         (and nul-p separator)
                         (substring line (if nul-p (1+ candidate-end) candidate-end)))
               (format "Error: Resource search returned an unsafe path")))
            line))
         (split-string output "\n" nil)
         "\n")))))

(defun niwa-tool-fs-search--scrub-resource-search-output (output roots)
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
              (when-let* ((path
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

(defun niwa-tool-fs-search-render-grep (name args result render-data)
  "Rendering plist for the Grep tool.
NAME is \"Grep\".  ARGS carries `:pattern' (the search regex).  RESULT
is the raw matches output.  Header shows the pattern and match count
\(one line per match); body fontifies as `grep-mode' for file:line
coloring.  `grep-mode' is autoloaded; `niwa-view--fontify-as' falls
back to text verbatim if activation fails."
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (visible (niwa-tool-fs-strip-system-reminders result))
           (matches (or (plist-get render-data :count)
                        (if (or (string-prefix-p "No matches found" visible)
                                (string-prefix-p "Error:" visible))
                            0
                          (length (seq-filter (lambda (l) (not (string-empty-p l)))
                                              (split-string visible "\n")))))))
      (list :header (format "%s: %s (%d matches)"
                            (or name "Grep") pattern matches)
            :body result
            :body-mode 'grep-mode
            :initially-collapsed-p t))))

(defun niwa-tool-fs-search-render-glob (name args result render-data)
  "Rendering plist for the Glob tool.
NAME is \"Glob\".  ARGS carries `:pattern'.  RESULT is a newline-separated
list of matching files.  Header shows pattern and file count."
  (when (stringp result)
    (let* ((pattern (or (plist-get args :pattern) ""))
           (lines (seq-filter (lambda (l) (not (string-empty-p l)))
                              (split-string result "\n")))
           (files (or (plist-get render-data :count)
                      (if (or (string-prefix-p "No files found" result)
                              (string-prefix-p "Error:" result))
                          0
                        (length (seq-filter
                                 (lambda (line)
                                   (not (string-prefix-p "... Results truncated" line)))
                                 lines))))))
      (list :header (format "%s: %s (%d files)"
                            (or name "Glob") pattern files)
            :body result
            :body-mode nil
            :initially-collapsed-p t))))

;;
;;; Search execution

(defconst niwa-tool-fs-search--grep-max-output-bytes (* 200 1024)
  "Hard cap on Grep tool output size in bytes.
Prevents catastrophic context overflow when searches hit files with
very long lines (e.g. JSON log files where a single match line can be
50KB+).  After line-count truncation, output exceeding this limit is
cut at the last complete line and a guidance message is appended.")

(defconst niwa-tool-fs-search--glob-default-head-limit 100
  "Default number of file paths returned by one Glob call.")

(defconst niwa-tool-fs-search--glob-max-output-bytes (* 30 1024)
  "Hard cap on Glob tool output size in bytes.")

(defconst niwa-tool-fs-search--vcs-directory-names
  '(".git" ".svn" ".hg" ".bzr" ".jj" ".sl")
  "Version-control metadata directory names excluded from searches.")

(defconst niwa-tool-fs-search--rg-vcs-exclusions
  (mapcar (lambda (name)
            (format "--glob=!**/%s" name))
          niwa-tool-fs-search--vcs-directory-names)
  "Ripgrep arguments excluding version-control metadata directories.")

(defcustom niwa-tool-fs-search-timeout 20
  "Seconds before a Glob or Grep helper is terminated."
  :type 'number
  :group 'niwa)

(defun niwa-tool-fs-search--rg-outcome (child-result)
  "Classify CHILD-RESULT with termination facts before its exit code."
  (cond
   ((plist-get child-result :error) 'error)
   ((plist-get child-result :timed-out-p) 'timeout)
   ((plist-get child-result :output-limit-p) 'output-limit)
   ((= (plist-get child-result :exit-code) 0) 'success)
   ((= (plist-get child-result :exit-code) 1) 'no-match)
   (t 'failure)))

(defun niwa-tool-fs-search--vcs-metadata-path-p (path)
  "Return non-nil when PATH resolves inside VCS metadata."
  (seq-some (lambda (component)
              (member component niwa-tool-fs-search--vcs-directory-names))
            (file-name-split (file-truename path))))

(defun niwa-tool-fs-search--truncate-output-buffer
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

(defun niwa-tool-fs-search--finalize-glob-buffer ()
  "Bound current buffer and return the model-visible Glob result."
  (unless (save-excursion
            (goto-char (point-min))
            (looking-at-p (regexp-quote "No files found")))
    (goto-char (point-min))
    (let ((total-lines (count-lines (point-min) (point-max))))
      (when (> total-lines niwa-tool-fs-search--glob-default-head-limit)
        (forward-line niwa-tool-fs-search--glob-default-head-limit)
        (delete-region (point) (point-max))
        (goto-char (point-max))
        (insert
         (format "\n... Results truncated (limit: %d). Narrow your search with :path or a more specific :pattern."
                 niwa-tool-fs-search--glob-default-head-limit)))))
  (niwa-tool-fs-search--truncate-output-buffer
   niwa-tool-fs-search--glob-max-output-bytes
   "Narrow your search with :path or a more specific :pattern.")
  (buffer-string))

(defun niwa-tool-fs-search--normalize-rg-glob (root pattern)
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
                                  niwa-tool-fs-search--vcs-directory-names))
                        prefix)
        (let ((directory
               (expand-file-name (string-join prefix "/") root)))
          (when (and (file-directory-p directory)
                     (not (niwa-tool-fs-search--vcs-metadata-path-p
                           directory)))
            (let ((true-root (file-name-as-directory (file-truename root)))
                  (true-directory
                   (file-name-as-directory (file-truename directory))))
              (unless (or (string= true-root true-directory)
                          (file-in-directory-p true-directory true-root))
                (error "Glob pattern escapes the search root"))
              (cons directory
                    (string-join relative-components "/")))))))))

(defun niwa-tool-fs-search--prepend-partial-warning
    (warning result maximum-size)
  "Prepend WARNING to RESULT without exceeding MAXIMUM-SIZE.
Return RESULT unchanged when WARNING is nil."
  (if (null warning)
      result
    (with-temp-buffer
      (insert warning result)
      (niwa-tool-fs-search--truncate-output-buffer maximum-size)
      (buffer-string))))

(defun niwa-tool-fs-search--settle-rg-result
    (child-result operation no-match failure-guidance)
  "Insert CHILD-RESULT output and return its settlement metadata.
OPERATION names the user-facing operation.  NO-MATCH is its empty result,
and FAILURE-GUIDANCE tells the caller how to narrow a failed invocation."
  (let* ((output (or (plist-get child-result :output) ""))
         (outcome (niwa-tool-fs-search--rg-outcome child-result))
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

(defun niwa-tool-fs-search--start-helper (callback name command read-paths session)
  "Start search COMMAND named NAME and deliver its result to CALLBACK.
READ-PATHS and SESSION retain the native helper's execution scope.
For resource searches, hide helper startup errors, which may name private
scratch paths outside the search roots.  Preserve ordinary path errors and
downstream callback errors."
  (let ((address niwa-tool-fs-search--resource-address)
        settled)
    (cl-labels
        ((finish (result)
           (unless settled
             (setq settled t)
             (funcall callback
                      (if (plist-get result :error)
                          (list :exit-code 2
                                :output (format (concat "Resource search could not start (%s); "
                                                        "check helper availability and retry\n")
                                                address))
                        result)))))
      (condition-case err
          (niwa-execution-start-helper
           (if address #'finish callback) name command read-paths nil
           ;; A local copy changes the helper target, not the caller's policy.
           :session (unless niwa-tool-fs-search--local-snapshot session)
           :owner (niwa-current-origin)
           :teardown-callback niwa-tool-fs-search--teardown
           :timeout niwa-tool-fs-search-timeout)
        (error
         (if (or (null address) settled)
             (signal (car err) (cdr err))
           (finish (list :error err))
           #'ignore))))))

(defun niwa-tool-fs-search--start-grep
    (callback rg-args paths read-paths file-glob file-type session)
  "Start Grep with RG-ARGS over PATHS and call CALLBACK with child results.
READ-PATHS and SESSION retain the native helper's execution scope.
Package directories first select files using FILE-GLOB and FILE-TYPE, then
admit only contained Markdown files before any content search."
  (let ((address niwa-tool-fs-search--resource-address))
    (if (not (and address (string-prefix-p "niwa://" address)
                  (cl-every #'file-directory-p paths)))
        (niwa-tool-fs-search--start-helper
         callback "niwa-grep" (cons "rg" (append rg-args paths)) read-paths session)
      (let ((deadline (+ (float-time) niwa-tool-fs-search-timeout))
            (owner (niwa-current-origin))
            (teardown niwa-tool-fs-search--teardown)
            cancelled settled cancels)
        (cl-labels
            ((finish (result)
               (unless (or cancelled settled)
                 ;; Downstream delivery owns its errors once settlement begins.
                 (setq settled t)
                 (funcall callback result)))
             (start (continuation name command)
               (condition-case err
                   (push
                    (niwa-execution-start-helper
                     (niwa-pipeline-handler-resumable
                      (lambda (result)
                        (unless (or cancelled settled)
                          (condition-case err
                              (if-let* ((failure (plist-get result :error)))
                                  (failed failure)
                                (funcall continuation result))
                            (error (failed err))))))
                     name command read-paths nil
                     :session session :owner owner :teardown-callback teardown
                     :timeout (max 0.001 (- deadline (float-time))))
                    cancels)
                 (error (failed err))))
             (failed (err)
               (if settled
                   (signal (car err) (cdr err))
                 ;; Either helper can fail before returning its canceller.
                 ;; Startup errors may name private scratch paths.
                 (reject (format (concat "Package Grep could not continue search (%s); "
                                         "check helper availability and retry")
                                 address))))
             (reject (reason)
               (finish (list :exit-code 2 :output (concat reason "\n")))))
          (start
           (lambda (result)
             (let ((outcome (niwa-tool-fs-search--rg-outcome result))
                   (output (or (plist-get result :output) "")))
               (cond
                ((memq outcome '(timeout output-limit))
                 ;; File-list output is not content and must not be paginated.
                 (finish (plist-put (copy-sequence result) :output "")))
                ((not (memq outcome '(success no-match)))
                 (reject "Package Grep could not select files; check :glob and :type"))
                ((> (string-bytes output) niwa-tool-fs-search--grep-max-output-bytes)
                 (reject "Package Grep file list is too large; narrow :path, :glob, or :type"))
                ((and (eq outcome 'success)
                      (not (string-suffix-p "\0" output)))
                 (reject "Package Grep returned an incomplete file list; narrow the search"))
                (t
                 (let ((files
                        (cl-remove-if-not
                         (lambda (file)
                           (and (string-suffix-p ".md" file)
                                (cl-some (lambda (root)
                                           (niwa-resource-within-root-p file root))
                                         paths)
                                (file-regular-p file)))
                         (split-string output "\0" t))))
                   (cond
                    ((>= (float-time) deadline)
                     (finish '(:exit-code 1 :timed-out-p t :output "")))
                    ((null files)
                     (finish '(:exit-code 1 :output "")))
                    (t
                     (start #'finish "niwa-grep"
                            (cons "rg" (append '("--with-filename") rg-args files))))))))))
           "niwa-grep-files"
           (append (list "rg" "--files" "--null" "--hidden" "--no-require-git")
                   ;; Do not add a second positive glob: rg combines them by
                   ;; union, and explicit file arguments bypass these filters.
                   (when file-glob (list (format "--glob=%s" file-glob)))
                   (when file-type (list (format "--type=%s" file-type)))
                   niwa-tool-fs-search--rg-vcs-exclusions paths)))
        (lambda ()
          (unless cancelled
            (setq cancelled t)
            (dolist (cancel cancels) (when (functionp cancel) (funcall cancel)))))))))

(defun niwa-tool-fs-search--dispatch-resource (callback args attempt operation)
  "Dispatch prepared ATTEMPT to the Glob or Grep handler OPERATION.
Preserve ARGS and return the handler's cancellation value.  CALLBACK receives
results rewritten in the authored resource domain."
  (niwa-resource-execute-async
   attempt callback
   (lambda (path authored callback)
     (cond
      ((and (listp path) (plist-member path :history-workspace))
       (niwa-history-search-start callback args path))
      ((and (listp path) (plist-member path :resource-search-documents))
       (niwa-tool-fs-search--documents callback args path operation))
      ((and (listp path) (plist-member path :shared-view))
       (niwa-tool-fs-search--shared callback args path operation))
      ((niwa-tool-fs-search--resource-search-roots path)
       (let ((native-args (copy-sequence args))
             (niwa-tool-fs-search--resource-address authored)
             (niwa-tool-fs-search--resource-dispatching t))
         (plist-put native-args :resource-roots
                    (niwa-tool-fs-search--resource-search-roots path))
         (when (eq operation #'niwa-tool-fs-search-grep)
           (plist-put native-args :resource-address authored))
         (plist-put native-args :path nil)
         (funcall operation callback native-args)))
      ((and (listp path) (plist-get path :virtual))
       (funcall callback
                (list :result (plist-get path :result)
                      :render-data (plist-get path :render-data))))
      (t
       (unless (and (stringp path) (file-exists-p path))
         (error "Search target not found: %s" authored))
       (let ((native-args (copy-sequence args))
             (niwa-tool-fs-search--resource-address authored)
             (niwa-tool-fs-search--resource-dispatching t))
         (plist-put native-args :path path)
         (funcall operation callback native-args)))))
   args))

(defun niwa-tool-fs-search-glob (callback args)
  "Find files matching a glob pattern using ripgrep.
CALLBACK receives the result envelope.  ARGS is a plist with :pattern
and optional :path."
  (let* ((address (plist-get args :path))
         (attempt (and (not niwa-tool-fs-search--resource-dispatching)
                       (niwa-tool-fs-resource-attempt address))))
    (if attempt
        (niwa-tool-fs-search--dispatch-resource
         callback args attempt #'niwa-tool-fs-search-glob)
      (let* ((pattern (plist-get args :pattern))
             (path (plist-get args :path))
             (resource-address niwa-tool-fs-search--resource-address)
             (resource-roots (plist-get args :resource-roots)))
        (when (string-empty-p pattern)
          (error "Pattern must not be empty"))
        (let* ((resource-paths
                (and resource-roots
                     (mapcar (lambda (root)
                               (expand-file-name (plist-get root :path)))
                             resource-roots)))
               (paths (or resource-paths
                          (list (expand-file-name (or path ".")))))
               (resource-root (and resource-address (car paths))))
          (unless (niwa-tool-fs-executable-find "rg" (car paths))
            (error "'rg' not installed on execution target"))
          (unless (cl-every (lambda (root)
                              (and (file-readable-p root)
                                   (or (null resource-roots)
                                       (file-directory-p root))))
                            paths)
            (error "Path %s is not a readable directory"
                   (niwa-tool-fs-search--visible-path (car paths))))
          (if resource-roots
              ;; Narrow each root by the pattern's literal directory prefix;
              ;; rg matches --iglob relative to the searched path, so an
              ;; unnarrowed prefix such as "dir/**/*.md" never matches.  Every
              ;; root yields the same narrowed pattern.
              (let (narrowed narrowed-pattern)
                (dolist (root paths)
                  (when-let* ((normalized
                               (niwa-tool-fs-search--normalize-rg-glob
                                root pattern)))
                    (push (car normalized) narrowed)
                    (setq narrowed-pattern (cdr normalized))))
                (setq paths (nreverse narrowed)
                      pattern (or narrowed-pattern pattern)))
            (let ((path (car paths)))
              (if-let* ((normalized
                         (and (not (niwa-tool-fs-search--vcs-metadata-path-p path))
                              (niwa-tool-fs-search--normalize-rg-glob path pattern))))
                  (setq paths (list (car normalized))
                        pattern (cdr normalized))
                (setq paths nil))))
          (if (null paths)
              (funcall callback
                       (niwa-tool-fs-handler-result
                        "No files found matching pattern"))
            (let* ((session (bound-and-true-p niwa--session))
                   (rg-args
                    (append (list "--files" "--hidden" "--no-ignore"
                                  "--color=never" "--iglob" pattern)
                            (when (or resource-roots
                                      niwa-tool-fs-search--resource-address)
                              (list "--null"))
                            (niwa-tool-fs-resource-rg-exclusions
                             (or niwa-tool-fs-search--resource-address
                                 address))
                            niwa-tool-fs-search--rg-vcs-exclusions
                            paths)))
              (niwa-tool-fs-search--start-helper
               (lambda (child-result)
                 (with-temp-buffer
                   (let* ((settlement
                           (niwa-tool-fs-search--settle-rg-result
                            child-result "glob" "No files found matching pattern"
                            "Narrow :path or :pattern."))
                          (partial-warning
                           (plist-get settlement :partial-warning)))
                     (when (or resource-roots resource-root)
                       (let ((settled (buffer-string)))
                         (erase-buffer)
                         (insert
                          (niwa-tool-fs-search--scrub-resource-search-output
                           (if resource-roots
                               (niwa-tool-fs-search--rewrite-resource-search-roots
                                settled resource-roots t)
                             (niwa-tool-fs-search--rewrite-resource-search-output
                              settled resource-root resource-address t))
                           (or resource-roots
                               (list (list :path resource-root :address resource-address)))))))
                     (let ((result (niwa-tool-fs-search--finalize-glob-buffer)))
                       (funcall callback
                                (niwa-tool-fs-handler-result
                                 (niwa-tool-fs-search--prepend-partial-warning
                                  partial-warning result
                                  niwa-tool-fs-search--glob-max-output-bytes)))))))
               "niwa-glob" (cons "rg" rg-args) paths session))))))))

(defun niwa-tool-fs-search-grep (callback args)
  "Search file contents with ripgrep.
CALLBACK receives the result envelope.  ARGS is a plist with :pattern and
optional :path, :glob, :output_mode, :head_limit, :offset, :-i, :-n,
:type, :multiline, :context, :-A, :-B, :-C."
  (let* ((address (plist-get args :path))
         (attempt (and (not niwa-tool-fs-search--resource-dispatching)
                       (niwa-tool-fs-resource-attempt address))))
    (if attempt
        (niwa-tool-fs-search--dispatch-resource
         callback args attempt #'niwa-tool-fs-search-grep)
      (let* ((pattern (plist-get args :pattern))
             (path (niwa-tool-string-arg args :path "."))
             (resource-roots (plist-get args :resource-roots))
             (maps (plist-get args :resource-search-maps))
             (resource-address (or (plist-get args :resource-address)
                                   niwa-tool-fs-search--resource-address))
             (file-glob (niwa-tool-string-arg args :glob))
             (output-mode (niwa-tool-string-arg
                           args :output_mode "files_with_matches"))
             (head-limit (let ((v (plist-get args :head_limit)))
                           (cond ((null v) 250)
                                 ((and (integerp v) (= v 0)) nil)
                                 ((and (integerp v) (< v 0))
                                  (error "Grep :head_limit must be 0 or greater"))
                                 ((integerp v) v)
                                 (t 250))))
             (offset (let ((v (or (niwa-tool-integer-arg args :offset) 0)))
                       (when (< v 0)
                         (error "Grep :offset must be 0 or greater"))
                       v))
             (case-fold (niwa-tool-truthy-p (plist-get args :-i)))
             (line-numbers (let ((v (plist-get args :-n)))
                             (if (null v)
                                 (equal output-mode "content")
                               (niwa-tool-truthy-p v))))
             (file-type (niwa-tool-string-arg args :type))
             (multiline (niwa-tool-truthy-p (plist-get args :multiline)))
             (ctx-after (niwa-tool-integer-arg args :-A))
             (ctx-before (niwa-tool-integer-arg args :-B))
             (ctx-around (or (niwa-tool-integer-arg args :-C)
                             (niwa-tool-integer-arg args :context))))
        (setq path (expand-file-name path))
        (unless (niwa-tool-fs-executable-find "rg" path)
          (error "'rg' not installed on execution target"))
        (let* ((resource-paths
                (and resource-roots
                     (mapcar (lambda (root)
                               (expand-file-name (plist-get root :path)))
                             resource-roots)))
               (search-roots (or resource-paths (list path)))
               (output-roots (or resource-roots
                                 (and resource-address
                                      (list (list :path path :address resource-address))))))
          (unless (cl-every (lambda (root)
                              (and (file-readable-p root)
                                   (if resource-roots
                                       (file-directory-p root)
                                     (file-exists-p root))))
                            search-roots)
            (error "Path %s is not readable"
                   (niwa-tool-fs-search--visible-path (car search-roots))))
          (let* ((vcs-metadata-p (and (null resource-roots)
                                      (niwa-tool-fs-search--vcs-metadata-path-p path)))
                 (normalized (and (null resource-roots)
                                  (not vcs-metadata-p)
                                  file-glob
                                  (file-directory-p path)
                                  (niwa-tool-fs-search--normalize-rg-glob
                                   path file-glob))))
            (when vcs-metadata-p
              (setq path nil))
            (when (and (null resource-roots) path file-glob (file-directory-p path))
              (if normalized
                  (setq path (car normalized)
                        file-glob (cdr normalized))
                (setq path nil)))
            (if (null path)
                (funcall callback
                         (niwa-tool-fs-handler-result "No matches found"))
              (let ((rg-args (list "--hidden" "--no-require-git"))
                    (session (bound-and-true-p niwa--session)))
                ;; Output mode flags
                (if maps
                    (progn
                      (if (equal output-mode "files_with_matches")
                          (progn (push "--files-with-matches" rg-args) (push "--null" rg-args))
                        (push "--json" rg-args))
                      (when (equal output-mode "content") (push "--max-count=1000" rg-args))
                      (dolist (value (list ctx-before ctx-after ctx-around))
                        (when (and value (< value 0)) (error "Grep context must be 0 or greater"))))
                  (pcase output-mode
                  ("content"
                   (when line-numbers (push "--line-number" rg-args))
                   ;; Resource pages must retain their source even when an
                   ;; offset removes earlier matches from the same file.
                   (push (if output-roots "--no-heading" "--heading") rg-args)
                   (when output-roots (push "--with-filename" rg-args))
                   (when ctx-after (push (format "-A%d" ctx-after) rg-args))
                   (when ctx-before (push (format "-B%d" ctx-before) rg-args))
                   (when ctx-around (push (format "-C%d" ctx-around) rg-args))
                   ;; Collect enough matches for the output page and one
                   ;; more to detect truncation. Headings/context only add
                   ;; output lines, so they need no additional matches.
                   (push (format "--max-count=%d"
                                 (if head-limit
                                     (min 1000 (+ offset head-limit 1))
                                   1000))
                         rg-args)
                   ;; Truncate long lines to prevent log files and other
                   ;; long-line sources from blowing up tool result size.
                   (push "--max-columns=2000" rg-args)
                   (push "--max-columns-preview" rg-args))
                  ("files_with_matches"
                   (push "--files-with-matches" rg-args))
                  ("count"
                   (push "--count" rg-args))))
                ;; Common flags
                (when case-fold (push "-i" rg-args))
                (when multiline (push "-U" rg-args) (push "--multiline-dotall" rg-args))
                (when file-glob (push (format "--glob=%s" file-glob) rg-args))
                (when file-type (push (format "--type=%s" file-type) rg-args))
                (when (and (not maps) (or resource-roots niwa-tool-fs-search--resource-address))
                  (push "--null" rg-args))
                (setq rg-args (nreverse rg-args))
                (setq rg-args
                      (append
                       rg-args
                       (niwa-tool-fs-resource-rg-exclusions
                        (or resource-address niwa-tool-fs-search--resource-address))
                       niwa-tool-fs-search--rg-vcs-exclusions
                       (list "-e" pattern)))
                (niwa-tool-fs-search--start-grep
                 (lambda (child-result)
                   (when (and maps (memq (niwa-tool-fs-search--rg-outcome child-result)
                                         '(success no-match timeout output-limit)))
                     (setq child-result
                           (plist-put
                            (copy-sequence child-result) :output
                            (niwa-tool-fs-search--shared-output
                             (or (plist-get child-result :output) "") maps output-mode line-numbers
                             (or ctx-before ctx-around 0) (or ctx-after ctx-around 0)
                             (memq (niwa-tool-fs-search--rg-outcome child-result)
                                   '(timeout output-limit))))))
                   (with-temp-buffer
                     (let* ((settlement
                             (niwa-tool-fs-search--settle-rg-result
                              child-result "search" "No matches found"
                              "Narrow :path, :glob, :type, or :pattern."))
                            (partial-warning
                             (plist-get settlement :partial-warning))
                            (pageable-output-p
                             (plist-get settlement :pageable-p))
                            range-error-p)
                       ;; Bound global content work before converting every
                       ;; collected path to a resource address. Keep one extra
                       ;; line so ordinary pagination still detects truncation.
                       (when (and output-roots pageable-output-p head-limit
                                  (equal output-mode "content"))
                         (goto-char (point-min))
                         (forward-line (+ offset head-limit 1))
                         (delete-region (point) (point-max)))
                       (when output-roots
                         (let ((rewritten
                                (niwa-tool-fs-search--scrub-resource-search-output
                                 (niwa-tool-fs-search--rewrite-resource-search-roots
                                  (buffer-string) output-roots
                                  (equal output-mode "files_with_matches")
                                  ":")
                                 output-roots)))
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
                                 (setq range-error-p t)
                                 (erase-buffer)
                                 (insert
                                  (format "Error: Offset %d starts after the last of %d output lines.  Lower :offset or repeat the search."
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
                       (niwa-tool-fs-search--truncate-output-buffer
                        niwa-tool-fs-search--grep-max-output-bytes
                        "Narrow your search with :glob, :type, or a more specific :pattern.")
                       (let ((result
                              (niwa-tool-fs-handler-result
                               (niwa-tool-fs-search--prepend-partial-warning
                                partial-warning (buffer-string)
                                niwa-tool-fs-search--grep-max-output-bytes))))
                         (when range-error-p
                           (setq result (plist-put result :status 'error))
                           (setq result (plist-put result :render-data '(:count 0))))
                         (funcall callback result)))))
                 rg-args (or resource-paths (list path)) search-roots
                 file-glob file-type session)))))))))

(provide 'niwa-tool-fs-search)

;;; niwa-tool-fs-search.el ends here
