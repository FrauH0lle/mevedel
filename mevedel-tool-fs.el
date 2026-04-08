;;; mevedel-tool-fs.el -- File system tools -*- lexical-binding: t -*-

;;; Commentary:

;; File system tool implementations: Read, Glob, Grep, Write, Edit, Insert,
;; MkDir.  Also provides diff generation utilities and file snapshotting used
;; by the inline/buffer preview system.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-tool-registry))

;; `diff-mode'
(declare-function diff-beginning-of-hunk "diff-mode" (&optional try-harder))
(declare-function diff-filename-drop-dir "diff-mode" (file))
(declare-function diff-hunk-file-names "diff-mode" (&optional old))
(declare-function diff-hunk-next "diff-mode" (&optional count))
(declare-function diff-setup-buffer-type "diff-mode" ())

;; `gptel-request'
(declare-function gptel-make-tool "ext:gptel-request" (&rest slots))

;; `mevedel-chat'
(defvar mevedel--diff-preview-buffer-name)

;; `mevedel-tool-ui'
(declare-function mevedel-tools--request-access "mevedel-tool-ui" (root reason &optional buffer))

;; `mevedel-preview-mode'
(declare-function mevedel-tools--show-changes-and-confirm "mevedel-preview-mode"
                  (temp-file original-content real-path final-callback &optional tool-name))

;; `mevedel-workspace'
(declare-function mevedel-workspace "mevedel-workspace" (&optional buffer))
(declare-function mevedel-workspace--root "mevedel-workspace" (workspace))
(declare-function mevedel-workspace--file-in-allowed-roots-p "mevedel-workspace" (file &optional buffer))

(defvar mevedel--workspace)


;;
;;; Diff Utilities

(defun mevedel-tools--generate-diff (original modified filepath)
  "Generate unified diff between ORIGINAL and MODIFIED content for FILEPATH."
  (with-temp-buffer
    (let ((orig-file (make-temp-file "mevedel-orig-"))
          (mod-file (make-temp-file "mevedel-mod-")))
      (unwind-protect
          (progn
            (with-temp-file orig-file (when original (insert original)))
            (with-temp-file mod-file (when modified (insert modified)))
            (call-process "diff" nil t nil
                          "-u"
                          "--label" (if (and original (not (string-empty-p original)))
                                        (concat "a/" filepath)
                                      "/dev/null")
                          "--label" (if (and modified (not (string-empty-p modified)))
                                        (concat "b/" filepath)
                                      "/dev/null")
                          orig-file mod-file)
            (buffer-string))
        (when (file-exists-p orig-file) (delete-file orig-file))
        (when (file-exists-p mod-file) (delete-file mod-file))))))

(defun mevedel-tools--setup-diff-buffer (temp-file real-path workspace root
                                                   &optional chat-buffer final-callback
                                                   user-modified original-window-config)
  "Setup diff buffer with content and full configuration.

Creates and configures `mevedel--diff-preview-buffer-name' with:
- Diff content between REAL-PATH and TEMP-FILE
- Read-only diff-mode with truncated lines
- Proper buffer-local variables for workspace context
- Header line with file path and action hints

Arguments:
- TEMP-FILE: Path to file with proposed changes
- REAL-PATH: Path to actual file
- WORKSPACE: Workspace identifier
- ROOT: Workspace root directory
- CHAT-BUFFER: Optional chat buffer reference
- FINAL-CALLBACK: Optional callback function
- USER-MODIFIED: Optional flag for user modifications
- ORIGINAL-WINDOW-CONFIG: Optional saved window configuration

Returns the configured diff buffer."
  (let* ((rel-path (file-relative-name real-path root))
         (original-content (when (file-exists-p real-path)
                             (with-temp-buffer
                               (insert-file-contents real-path)
                               (buffer-string))))
         (modified-content (with-temp-buffer
                             (insert-file-contents temp-file)
                             (buffer-string)))
         (diff (mevedel-tools--generate-diff original-content modified-content rel-path))
         (diff-buffer (generate-new-buffer mevedel--diff-preview-buffer-name)))
    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))
        ;; Add file mode lines for new or deleted files.
        (cond
         ;; New file: original-content is nil/empty, modified-content is
         ;; non-nil.
         ((and (or (not original-content) (string-empty-p original-content))
               (and modified-content (not (string-empty-p modified-content))))
          (insert "new file mode 100644\n"))
         ;; Deleted file: original-content is non-nil, modified-content is
         ;; nil/empty.
         ((and (and original-content (not (string-empty-p original-content)))
               (or (not modified-content) (string-empty-p modified-content)))
          (insert "deleted file mode 100644\n")))
        (insert diff)
        (diff-mode)
        ;; Always use read-only mode for safety
        (read-only-mode 1)
        ;; Always truncate lines for better diff readability
        (setq-local truncate-lines t)
        ;; Re-detect patch type (i.e. 'git) now that buffer is populated
        (when (derived-mode-p 'diff-mode)
          (diff-setup-buffer-type))

        ;; Always set these buffer-local variables
        (setq-local default-directory root
                    mevedel--workspace workspace
                    mevedel--temp-file temp-file
                    mevedel--real-path real-path
                    mevedel--chat-buffer chat-buffer
                    mevedel--final-callback final-callback
                    mevedel--user-modified user-modified
                    mevedel--original-window-config original-window-config)

        (goto-char (point-min))))
    diff-buffer))


;;
;;; File Snapshotting

(defvar-local mevedel--request-file-snapshots nil
  "Alist of (FILEPATH . ORIGINAL-CONTENT) tracking files modified.

Each entry stores the original state of a file before any modifications
in the current request. ORIGINAL-CONTENT is nil if the file didn't exist
before the request. Cleared when request completes.")

(defun mevedel--snapshot-file-if-needed (filepath)
  "Capture original state of FILEPATH before first modification in request.
Does nothing if FILEPATH has already been snapshotted in this request.
Stores nil for ORIGINAL-CONTENT if file doesn't exist yet."
  (when (and filepath (stringp filepath))
    (let ((abs-path (expand-file-name filepath)))
      (unless (assoc abs-path mevedel--request-file-snapshots)
        (push (cons abs-path
                    (when (file-exists-p abs-path)
                      (with-temp-buffer
                        (insert-file-contents abs-path)
                        (buffer-string))))
              mevedel--request-file-snapshots)))))

(defun mevedel--snapshot-files-from-diff (diff-str)
  "Extract and snapshot all files referenced in DIFF-STR.
Uses `diff-mode' to parse the diff and extract file paths.

Must be called from the mevedel chat buffer context to preserve
buffer-local snapshots."
  ;; Validate that we're running in the chat buffer context
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel--snapshot-files-from-diff' must be called from chat buffer context"))
  ;; Extract file paths in a temp buffer, but snapshot in the current buffer
  (let* ((workspace (mevedel-workspace))
         (files-to-snapshot
          (with-temp-buffer
            (insert diff-str)
            (diff-mode)
            (goto-char (point-min))
            (let ((files nil))
              (condition-case nil
                  (progn
                    (diff-beginning-of-hunk t)
                    (while (not (eobp))
                      (let ((file-names (diff-hunk-file-names)))
                        (when file-names
                          (let* ((new-file (car file-names))
                                 (old-file (cadr file-names))
                                 (ws-root (mevedel-workspace--root workspace)))
                            ;; Collect files to snapshot
                            (unless (string-match-p "dev/null" new-file)
                              (let ((filepath (expand-file-name (diff-filename-drop-dir new-file) ws-root)))
                                (push filepath files)))
                            (unless (string-match-p "dev/null" old-file)
                              (let ((filepath (expand-file-name (diff-filename-drop-dir old-file) ws-root)))
                                (push filepath files))))))
                      (condition-case nil
                          (diff-hunk-next)
                        (error (goto-char (point-max))))))
                (error nil))
              files))))
    ;; Now snapshot files in the CURRENT buffer context (not temp buffer) and
    ;; deduplicate the file list first
    (dolist (filepath (delete-dups files-to-snapshot))
      (mevedel--snapshot-file-if-needed filepath))))


;;
;;; File Reading

(cl-defun mevedel-tools--read-file-lines (callback filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME via CALLBACK."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--read-file-lines
    (filename stringp)
    (start-line integerp nil)
    (end-line integerp nil))

  (unless (file-readable-p filename)
    (cl-return-from mevedel-tools--read-file-lines
      (funcall callback (format "Error: File %s is not readable" filename))))

  (when (file-directory-p filename)
    (cl-return-from mevedel-tools--read-file-lines
      (funcall callback (format "Error: Cannot read directory %s as file" filename))))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  ;; Check directory permissions
  (mevedel-tools--check-directory-permissions filename
    (format "Need to read file: %s" filename)
    mevedel-tools--read-file-lines callback)

  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (cl-return-from mevedel-tools--read-file-lines
            (funcall callback "Error: File is too large (> 512 KB).
Please specify a line range to read"))
        (with-temp-buffer
          (insert-file-contents filename)
          ;; Add line numbers (cat -n style) and truncate long lines
          (goto-char (point-min))
          (let ((line-num 1)
                (max-lines (count-lines (point-min) (point-max))))
            ;; Calculate width for line numbers
            (let ((width (length (number-to-string max-lines))))
              (while (not (eobp))
                ;; Truncate line if longer than 2000 characters
                (let ((line-end (line-end-position)))
                  (when (> (- line-end (point)) 2000)
                    (delete-region (+ (point) 2000) line-end)
                    (insert " [...]")))
                ;; Insert line number
                (insert (format (format "%%%dd\t" width) line-num))
                (forward-line 1)
                (setq line-num (1+ line-num)))))
          (funcall callback (buffer-substring-no-properties (point-min) (point-max)))))
    ;; Handle nil start-line OR nil end-line
    (let* ((start-line (or start-line 1))
           (end-line (or end-line 2000))
           ;; Store for line numbering
           (original-start-line start-line))
      (cl-decf start-line)
      (let* ((file-size (nth 7 (file-attributes filename)))
             (chunk-size (min file-size (* 512 1024)))
             (byte-offset 0) (line-offset (- end-line start-line)))
        (with-temp-buffer
          ;; Go to start-line
          (while (and (> start-line 0)
                      (< byte-offset file-size))
            (insert-file-contents
             filename nil byte-offset (+ byte-offset chunk-size))
            (setq byte-offset (+ byte-offset chunk-size))
            (setq start-line (forward-line start-line))
            (when (eobp)
              (if (/= (line-beginning-position) (line-end-position))
                  ;; forward-line counted 1 extra line
                  (cl-incf start-line))
              (delete-region (point-min) (line-beginning-position))))

          (delete-region (point-min) (point))

          ;; Go to end-line, forward by line-offset
          (cl-block nil
            (while (> line-offset 0)
              (setq line-offset (forward-line line-offset))
              (when (and (eobp) (/= (line-beginning-position) (line-end-position)))
                ;; forward-line counted 1 extra line
                (cl-incf line-offset))
              (if (= line-offset 0)
                  (delete-region (point) (point-max))
                (if (>= byte-offset file-size)
                    (cl-return)
                  (insert-file-contents
                   filename nil byte-offset (+ byte-offset chunk-size))
                  (setq byte-offset (+ byte-offset chunk-size))))))

          ;; Add line numbers (cat -n style) and truncate long lines
          (goto-char (point-min))
          (let ((line-num original-start-line)
                (width (length (number-to-string end-line))))
            (while (not (eobp))
              ;; Truncate line if longer than 2000 characters
              (let ((line-end (line-end-position)))
                (when (> (- line-end (point)) 2000)
                  (delete-region (+ (point) 2000) line-end)
                  (insert " [...]")))
              ;; Insert line number
              (insert (format (format "%%%dd\t" width) line-num))
              (forward-line 1)
              (setq line-num (1+ line-num))))

          (funcall callback (buffer-substring-no-properties (point-min) (point-max))))))))

(cl-defun mevedel-tools--grep (callback regex path &optional glob context-lines)
  "Search for REGEX in file or directory at PATH using ripgrep.

CALLBACK is a function to call with the results.
REGEX is a PCRE-format regular expression to search for.
PATH can be a file or directory to search in.

Optional arguments:
GLOB restricts the search to files matching the glob pattern.
  Examples: \"*.el\", \"*.md\", \"*.rs\"
CONTEXT-LINES specifies the number of lines of context to show
  around each match (0-15 inclusive, defaults to 0).

Returns a string containing matches grouped by file, with line numbers
and optional context. Results are sorted by modification time and
limited to 1000 matches per file."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--grep
    (regex stringp)
    (path stringp)
    (glob stringp nil)
    (context-lines integerp nil))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--grep
      (funcall callback (format "Error: File or directory %s is not readable" path))))
  (let ((grepper (executable-find "rg")))
    (unless grepper
      (cl-return-from mevedel-tools--grep
        (funcall callback "Error: `ripgrep` not installed. This tool cannot be used")))

    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions path
      (format "Need to grep in: %s" path)
      mevedel-tools--grep callback)

    (with-temp-buffer
      (let* ((args (delq nil (list "--sort=modified"
                                   (and (natnump context-lines)
                                        (format "--context=%d" context-lines))
                                   (and glob (format "--glob=%s" glob))
                                   ;; "--files-with-matches"
                                   "--max-count=1000"
                                   "--heading" "--line-number" "-e" regex
                                   (expand-file-name (substitute-in-file-name path)))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (funcall callback (buffer-string))))))

(cl-defun mevedel-tools--glob (callback pattern &optional path depth)
  "Find files matching PATTERN.

CALLBACK is a function to call with the results.
PATTERN is a case-insensitive regex pattern to match filenames against.
PATH is the optional directory to search (defaults to current
directory).
DEPTH limits recursion depth when provided (non-negative integer).

Returns a string listing matching files with full paths, sorted by
modification time. Raises an error if PATTERN is empty, PATH is not
readable, or the `rg' executable is not found."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--glob
    (pattern stringp)
    (path stringp nil)
    (depth integerp nil))

  (when (string-empty-p pattern)
    (cl-return-from mevedel-tools--glob
      (funcall callback "Error: pattern must not be empty")))
  (if path
      (unless (and (file-readable-p path) (file-directory-p path))
        (cl-return-from mevedel-tools--glob
          (funcall callback (format "Error: path %s is not readable" path))))
    (setq path "."))
  (unless (executable-find "rg")
    (cl-return-from mevedel-tools--glob
      (funcall callback "Error: `ripgrep` not installed. This tool cannot be used")))

  ;; Check directory permissions
  (mevedel-tools--check-directory-permissions path (format "Need to find files in: %s" path)
    mevedel-tools--glob callback)

  (with-temp-buffer
    (let* ((args (list "--files" "--hidden" "--color=never"
                       "--follow" "--sort" "modified"
                       "--iglob" pattern))
           (args (if (natnump depth)
                     (nconc args (list "--max-depth" (number-to-string depth)))
                   args))
           (args (nconc args (ensure-list (expand-file-name path))))
           (exit-code (apply #'call-process "rg" nil t nil args)))
      (when (/= exit-code 0)
        (goto-char (point-min))
        (insert (format "Glob failed with exit code %d\n.STDOUT:\n\n"
                        exit-code))))
    (funcall callback (buffer-string))))


;;
;;; File Editing Tools

(cl-defun mevedel-tools--edit-files (callback path &optional old-str new-str-or-diff use-diff)
  "Edit file(s) at PATH using either string matching or unified diff.

This function supports two distinct modes of operation:

1. STRING REPLACEMENT MODE (USE-DIFF is nil or :json-false):
   - Searches for OLD-STR in the file at PATH
   - Replaces it with NEW-STR-OR-DIFF
   - Requires OLD-STR to match exactly once (uniquely) in the file
   - Only works on single files, not directories

2. DIFF/PATCH MODE (when USE-DIFF is non-nil and not :json-false):
   - Applies NEW-STR-OR-DIFF as a unified diff using the `patch` command
   - Works on both single files and directories
   - OLD-STR is ignored in this mode
   - NEW-STR-OR-DIFF can contain the diff in fenced code blocks
     (=diff or =patch)
   - Uses the -N (--forward) option to ignore already-applied patches


CALLBACK - async callback for results
PATH - file to edit
OLD-STR - text to find (string mode only)
NEW-STR-OR-DIFF - replacement text or diff
USE-DIFF - if non-nil, treat NEW-STR as diff

Workflow:
1. Check access to file, request inline if needed
2. Apply changes to temp copy of file
3. Generate diff showing actual changes
4. Show diff to user for approval
5. If approved: apply to real file and add to patch buffer
6. If rejected: optionally get feedback for LLM"
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--edit-files
    (path stringp)
    (new-str-or-diff stringp))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--edit-files
      (funcall callback (format "Error: File or directory %s is not readable" path))))

  (let* ((expanded-path (expand-file-name path)))
    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions expanded-path
      (format "Need to edit file: %s" path)
      mevedel-tools--edit-files callback)

    (mevedel-tools--edit-files-1 callback expanded-path old-str new-str-or-diff use-diff)))

(cl-defun mevedel-tools--edit-files-1 (callback path old-str new-str-or-diff use-diff)
  "Perform the actual file edit operation for PATH.

CALLBACK - async callback for results
PATH - absolute path to file to edit
OLD-STR - text to find (string mode only)
NEW-STR-OR-DIFF - replacement text or diff
USE-DIFF - if non-nil, treat NEW-STR as diff

This is the internal function that does the actual work after access has
been verified."
  ;; Verify path is valid
  (unless (and path (stringp path))
    (cl-return-from mevedel-tools--edit-files-1
      (funcall callback (format "Error: Invalid path argument: %S" path))))

  ;; Snapshot the file(s) before any modifications
  ;; Use the SAME condition as the mode determination below
  (if (or (eq use-diff :json-false) old-str)
      ;; STRING MODE: Snapshot single file
      (mevedel--snapshot-file-if-needed path)
    ;; DIFF MODE: Extract files from diff and snapshot each
    (mevedel--snapshot-files-from-diff new-str-or-diff))

  (let* ((temp-file (make-temp-file "mevedel-edit-"))
         (original-content (when (file-exists-p path)
                             (with-temp-buffer
                               (insert-file-contents path)
                               (buffer-string)))))

    (condition-case err
        (progn
          ;; Copy original to temp file (empty if file doesn't exist)
          (with-temp-file temp-file
            (when original-content
              (insert original-content)))

          ;; Apply edit to temp file (string or diff mode)
          (if (or (eq use-diff :json-false) old-str)
              ;; STRING REPLACEMENT MODE
              (progn
                (when (file-directory-p path)
                  (cl-return-from mevedel-tools--edit-files-1
                    (funcall
                     callback
                     (format "Error: String replacement is intended for single files, not directories (%s)" path))))
                (mevedel-tools--apply-string-replacement temp-file old-str new-str-or-diff
                                                         (lambda (success-or-error)
                                                           (if (stringp success-or-error)
                                                               ;; Error
                                                               (progn
                                                                 (delete-file temp-file)
                                                                 (funcall callback success-or-error))
                                                             ;; Success - show diff and confirm
                                                             (mevedel-tools--show-changes-and-confirm
                                                              temp-file original-content path callback "Edit")))))

            ;; DIFF MODE
            (mevedel-tools--apply-diff-to-temp temp-file new-str-or-diff
                                               (lambda (success-or-error)
                                                 (if (stringp success-or-error)
                                                     ;; Error
                                                     (progn
                                                       (delete-file temp-file)
                                                       (funcall callback success-or-error))
                                                   ;; Success - show diff and confirm
                                                   (mevedel-tools--show-changes-and-confirm
                                                    temp-file original-content path callback "Edit"))))))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (funcall callback (format "Error: %s" (error-message-string err)))))))

(defun mevedel-tools--apply-string-replacement (temp-file old-str new-str-or-diff callback)
  "Apply string replacement to TEMP-FILE.
Calls CALLBACK with t on success or error string on failure.
CALLBACK is the function to call with the result.
OLD-STR and NEW-STR-OR-DIFF are the replacement parameters."
  (condition-case err
      (let (success)
        (with-temp-buffer
          (insert-file-contents temp-file)
          (goto-char (point-min))
          (if (search-forward old-str nil t)
              (if (save-excursion (search-forward old-str nil t))
                  (funcall callback "Error: Match is not unique.
Consider providing more context for the replacement, or a unified diff")
                ;; Unique match found - replace it
                (replace-match (string-replace "\\" "\\\\" new-str-or-diff))
                (write-region nil nil temp-file nil 'silent)
                (setq success t))
            (funcall callback (format "Error: Could not find old_str \"%s\" in file"
                                      (truncate-string-to-width old-str 20)))))
        (when success
          (funcall callback success)))
    (error
     (funcall callback (format "Error: %s" (error-message-string err))))))

(cl-defun mevedel-tools--apply-diff-to-temp (temp-file diff callback)
  "Apply DIFF to TEMP-FILE using patch command.
Calls CALLBACK with t on success or error string on failure."
  (unless (executable-find "patch")
    (cl-return-from mevedel-tools--apply-diff-to-temp
      (funcall callback "Error: Command \"patch\" not available, cannot apply diffs.
Use string replacement instead")))

  (let* ((out-buf-name (generate-new-buffer-name "*patch-stdout*"))
         ;; Initialize to a known non-zero value
         (exit-status -1)
         (result-output ""))

    (unwind-protect
        (let ((default-directory (file-name-directory (expand-file-name temp-file)))
              (patch-options '("--forward" "--verbose")))

          (with-temp-message
              (format "Applying diff to: `%s` with options: %s"
                      temp-file patch-options)

            (with-temp-buffer
              (insert diff)
              ;; Ensure trailing newline
              (unless (eq (char-before (point-max)) ?\n)
                (goto-char (point-max))
                (insert "\n"))
              (goto-char (point-min))
              ;; Remove code fences if present
              (when (looking-at-p "^ *```\\(diff\\|patch\\)\n")
                (delete-line)
                (goto-char (point-max))
                (forward-line -1)
                (when (looking-at-p "^ *```")
                  (delete-line)))

              ;; Fix line numbers in hunk headers
              (mevedel-tools--fix-patch-headers)

              (setq exit-status
                    (apply #'call-process-region
                           (point-min) (point-max)
                           "patch" nil (list out-buf-name t)
                           nil (append patch-options (list (file-name-nondirectory temp-file)))))))
          ;; Retrieve content from buffers using their names
          (when-let* ((stdout-buf (get-buffer out-buf-name)))
            (when (buffer-live-p stdout-buf)
              (with-current-buffer stdout-buf
                (setq result-output (buffer-string)))))

          (if (= exit-status 0)
              (funcall callback t)
            (let ((err (format "Error: Failed to apply diff to %s (exit status %s).
Patch command options: %s
Patch STDOUT:\n%s"
                               temp-file exit-status patch-options
                               result-output)))
              (funcall callback err))))
      ;; Clean up
      (let ((stdout-buf-obj (get-buffer out-buf-name)))
        (when (buffer-live-p stdout-buf-obj) (kill-buffer stdout-buf-obj))))))

(defun mevedel-tools--fix-patch-headers ()
  "Fix line numbers in hunks in diff at point."
  ;; Find and process each hunk header
  (while (re-search-forward "^@@ -\\([0-9]+\\),\\([0-9]+\\) +\\+\\([0-9]+\\),\\([0-9]+\\) @@" nil t)
    (let ((hunk-start (line-beginning-position))
          (orig-line (string-to-number (match-string 1)))
          (new-line (string-to-number (match-string 3)))
          (orig-count 0)
          (new-count 0))

      ;; Count lines in this hunk until we hit the next @@ or EOF
      (goto-char hunk-start)
      (forward-line 1)
      (save-match-data
        (while (and (not (eobp))
                    (not (looking-at-p "^@@")))
          (cond
           ;; Removed lines (not ---)
           ((looking-at-p "^-[^-]")
            (cl-incf orig-count))
           ;; Added lines (not +++)
           ((looking-at-p "^\\+[^+]")
            (cl-incf new-count))
           ;; Context lines (space at start)
           ((looking-at-p "^ ")
            (cl-incf orig-count)
            (cl-incf new-count)))
          (forward-line 1)))

      ;; Replace the hunk header with corrected counts
      (goto-char hunk-start)
      (delete-line)
      (insert (format "@@ -%d,%d +%d,%d @@\n"
                      orig-line orig-count new-line new-count)))))

(cl-defun mevedel-tools--insert-in-file (callback path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

CALLBACK is a function to call with the result.
LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--insert-in-file
    (path stringp)
    (line-number integerp)
    (new-str stringp))

  (unless (file-readable-p path)
    (cl-return-from mevedel-tools--insert-in-file
      (funcall callback (format "Error: File %s is not readable" path))))

  (when (file-directory-p path)
    (cl-return-from mevedel-tools--insert-in-file
      (funcall callback (format "Error: Cannot insert into directory %s" path))))

  ;; Snapshot the file before any modifications
  (mevedel--snapshot-file-if-needed path)

  (let ((temp-file (make-temp-file "mevedel-edit-")))
    (condition-case err
        (let* ((expanded-path (expand-file-name path))
               (original-content (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string))))

          ;; Check directory permissions
          (mevedel-tools--check-directory-permissions expanded-path
            (format "Need to insert into file: %s" path)
            mevedel-tools--insert-in-file callback)

          (with-temp-file temp-file
            (insert original-content)

            (pcase line-number
              (0 (goto-char (point-min)))       ; Insert at the beginning
              (-1 (goto-char (point-max)))      ; Insert at the end
              (_ (goto-char (point-min))
                 (forward-line line-number)))   ; Insert before line N

            ;; Insert the new string
            (insert new-str)

            ;; Ensure there's a newline after the inserted text if not already present
            (unless (or (string-suffix-p "\n" new-str) (eobp))
              (insert "\n")))
          ;; Show diff and confirm
          (mevedel-tools--show-changes-and-confirm
           temp-file original-content path callback "Insert"))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (funcall callback (format "Error: %s" (error-message-string err)))))))

(cl-defun mevedel-tools--make-directory (parent name)
  "Create a directory NAME in PARENT directory.

Creates the directory and any missing parent directories. If the
directory already exists, this is a no-op and returns success.

PARENT is the parent directory path, NAME is the name of the new
directory to create."
  ;; Validate input
  (mevedel-tools--validate-params nil mevedel-tools--make-directory
    (parent stringp)
    (name stringp))
  ;; Check directory permissions
  (mevedel-tools--check-directory-permissions parent
    (format "Need to create directory in: %s" parent)
    mevedel-tools--make-directory nil)

  (condition-case errdata
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s:\n%S" name parent errdata))))

(cl-defun mevedel-tools--write-file (callback path filename content)
  "Write CONTENT to FILENAME in PATH.

CALLBACK is a function to call with the result.
PATH and FILENAME are expanded to create the full path.
CONTENT is written to the file. Returns a success message string, or
signals an error if writing fails.

PATH, FILENAME, and CONTENT must all be strings."
  ;; Validate input
  (mevedel-tools--validate-params callback mevedel-tools--write-file
    (path stringp)
    (filename stringp)
    (content stringp))

  (let* ((full-path (expand-file-name filename path)))
    ;; Check directory permissions
    (mevedel-tools--check-directory-permissions full-path
      (format "Need to create %s in directory: %s" filename path)
      mevedel-tools--write-file callback)

    ;; Snapshot the file before any modifications
    (mevedel--snapshot-file-if-needed full-path)
    ;; Access granted, proceed
    (condition-case errdata
        (let* ((temp-file (make-temp-file "mevedel-edit-" nil nil content))
               (original-content (when (file-exists-p full-path)
                                   (with-temp-buffer
                                     (insert-file-contents full-path)
                                     (buffer-string)))))
          ;; Show diff and confirm
          (mevedel-tools--show-changes-and-confirm
           temp-file original-content full-path callback "Write"))
      (error (funcall callback (format "Error: Could not write file %s:\n%S" path errdata))))))


;;
;;; Register Tools

(defun mevedel-tool-fs--register ()
  "Register file system tools for mevedel."

  (gptel-make-tool
   :name "Glob"
   :description "Recursively find files matching a provided glob pattern.

### When to use `Glob`

- Searching for files by name patterns or extensions
- You know the file pattern but not exact location
- Finding all files of a certain type
- Exploring project or directory structure

### When NOT to use `Glob`

- Searching file contents -> use `Grep`
- You know the exact file path -> use `Read`
- Doing open-ended multi-round searches -> delegate

### How to use `Glob`

- Supports standard glob patterns: `**/*.el`, `*.{el,txt}`,
  `lisp/**/*.el`. The glob applies to the basename of the file (with
  extension).
- Returns files sorted by modification time (most recent first)
- You can call multiple tools in a single response. It is always better
  to speculatively perform multiple searches in parallel if they are
  potentially useful.

### Examples of good usage

<example>
- Find all test files
Glob(pattern=\"**/*.test.js\")
</example>

<example>
- Find all config files
Glob(pattern=\"config/*.{yml,yaml,json}\")
</example>

### Examples of bad usage

<example>
- Searching for content
Glob(pattern=\"password\")
<reasoning>
Should use Grep to search file contents instead.
</reasoning>
</example>

<example>
Glob(pattern=\"/usr/local/bin/python\")
<reasoning>
Should use Read if you want to read a specific known file.
</reasoning>
</example>
"
   :function #'mevedel-tools--glob
   :args '((:name "pattern"
            :type string
            :description "Glob pattern to match, for example \"*.el\". Must not be empty.
Use \"*\" to list all files in a directory.")
           (:name "path"
            :type string
            :description "Directory to search in.  Supports relative paths and defaults to \".\""
            :optional t)
           (:name "depth"
            :description "Limit directory depth of search, 1 or higher. Defaults to no limit."
            :type integer
            :optional t))
   :category "mevedel"
   :async t)

  (gptel-make-tool
   :name "Read"
   :description "Read file contents between specified line numbers `start_line` and
`end_line`, with both ends included.

Consider using the `Grep` tool to find the right range to read first.

Reads up to 2000 lines if the line range is not provided.

Any lines longer than 2000 characters will be truncated.

Files over 512 KB in size can only be read by specifying a line range.

### When to use `Read`

- You need to examine file contents
- Before editing any file (required)
- You know the exact file path
- Understanding code structure and implementation

### When NOT to use `Read`

- Searching for files by name -> use `Glob`
- Searching file contents across multiple files -> use `Grep`

### How to use `Read`
- Default behavior reads from beginning to end
- For large files, use `start_line` and `end_line` parameters to read
  specific sections
- Recommended to read the whole file when possible
- Always read before editing - edit tools will error otherwise
- You can call multiple tools in a single response. It is always better
  to speculatively read multiple potentially useful files in parallel.

### Examples of good usage

<example>
- Reading a specific function:
Read(file_path=\"src/utils.el\", start_line=45, end_line=62)
</example>

<example>
- Examining configuration before changes:
Read(file_path=\"config/database.yml\")
</example>

### Examples of bad usage

<example>
- Trying to find all files with 'test' in the name:
Read(file_path=\"*test*\")
<reasoning>
Should use Glob(pattern=\"*test*\") instead.
</reasoning>
</example>
"
   :function #'mevedel-tools--read-file-lines
   :args '((:name "file_path"
            :type string
            :description "The path to the file to be read."
            :type string)
           (:name "start_line"
            :type integer
            :description "The line to start reading from, defaults to the start of the file"
            :optional t)
           (:name "end_line"
            :type integer
            :description "The line up to which to read, defaults to 2000 or the end of the file."
            :optional t))
   :category "mevedel"
   :async t
   :include t)

  (gptel-make-tool
   :name "Grep"
   :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by
file. Can search an individual file (if providing a file path) or a
directory. Consider using this tool to find the right line range for the
`Read` tool.

When searching directories, optionally restrict the types of files in
the search with a `glob`. Can request context lines around each match
using the `context_lines` parameters.

### When to use `Grep`

- Finding ONE specific, well-defined string/pattern in the codebase
- You know what you're looking for and where it likely is
- Verifying presence/absence of specific text
- Quick, focused searches with expected results <20 matches

### When NOT to use `Grep`

- Building code understanding or exploring unfamiliar code -> DELEGATE
- Expected to get many results (20+ matches) -> DELEGATE
- Will need follow-up searches based on results -> DELEGATE
- Searching for files by name -> use `Glob`
- Reading known file contents -> use `Read`

### How to use `Grep`

- Supports full regex syntax
- Can specify glob pattern to narrow scope
- Use `context_lines` parameter to see surrounding lines
- You can call multiple tools in a single response. It is always better
  to speculatively perform multiple focused grep searches in parallel.
- **If you find yourself doing a second grep based on first results, you
    should have delegated**

### Examples of good usage

<example>
- Find all TODO comments in Python files
Grep(regex=\"TODO|FIXME\", path=\".\", glob=\"**/*.py\")
</example>

<example>
- Find authenticate function definition
Grep(regex=\"def authenticate\", path=\".\", context_lines=3)
</example>

### Examples of bad usage

<example>
Grep(regex=\"import\")
<reasoning>
Too generic, will return many results.
Should delegate to codebase-analyst for broader exploration.
</reasoning>
</example>

<example>
Grep(regex=\"user\", glob=\"**/*\")
- Followed by more searches based on results
<reasoning>
Should delegate instead of doing multiple sequential searches.
</reasoning>
</example>
"
   :function #'mevedel-tools--grep
   :args '((:name "regex"
            :description "Regular expression to search for in file contents."
            :type string)
           (:name "path"
            :description "File or directory to search in."
            :type string)
           (:name "glob"
            :description "Optional glob to restrict file types to search for.
Only required when path is a directory.
Examples: *.md, *.rs"
            :type string
            :optional t)
           (:name "context_lines"
            :description "Number of lines of context to retrieve around each match (0-15 inclusive).
Optional, defaults to 0."
            :optional t
            :type integer
            :maximum 15))
   :async t
   :category "mevedel")

  (gptel-make-tool
   :name "MkDir"
   :description "Create a new directory with the given name in the specified parent
directory.

### When to use `MkDir`

- Creating new directories for organizing files
- Setting up directory structure for a project
- Preparing directories before writing files

### How to use `MkDir`

- Provide parent directory path and name of new directory
- Creates parent directories automatically if they don't exist (like
  mkdir -p)
- Safe to call multiple times (idempotent)

### Examples of good usage

<example>
- Create a new tests directory
MkDir(parent=\".\", name=\"tests\")
</example>

<example>
- Create nested directory structure
MkDir(parent=\"src/components\", name=\"forms\")
</example>

### Examples of bad usage

<example>
- Using for file creation
MkDir(parent=\"src\", name=\"app.js\")
<reasoning>
Use Write tool to create files, not MkDir.
</reasoning>
</example>
"
   :function #'mevedel-tools--make-directory
   :args (list '(:name "parent"
                 :type string
                 :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                 :type string
                 :description "The name of the new directory to create, e.g. testdir"))
   :category "mevedel"
   :confirm t)

  (gptel-make-tool
   :name "Write"
   :description "Create a new file with the specified content.

Overwrites an existing file, so use with care!

Consider using the more granular tools `Insert` or `Edit` first.

### When to use `Write`

- Creating new files that don't exist yet
- Completely replacing the contents of an existing file
- Generating new code or configuration files

### When NOT to use `Write`

- Modifying existing files -> use `Edit` instead (more precise and safer)
- The file already exists and you only need to change part of it -> use `Edit`
- You haven't read the file first (if it exists) -> read first, then use `Edit`

### How to use `Write`

- Will overwrite existing files completely - use with caution
- MUST use `Read` first if the file already exists (tool will error otherwise)
- Always prefer editing existing files rather than creating new ones
- Provide complete file content

### Examples of good usage

<example>
- Creating a new test file for a function:
Write(path=\"tests\", filename=\"test-user-auth.el\", content=\";;; test-user-auth.el --- Tests for user authentication\n\n(describe \"User Authentication\"\n  (it \"should validate correct password\")\n    (expect (user-auth-valid-p \"user\" \"pass\") :to-be t)))\n\")
</example>

<example>
- Generating a complete configuration file:
Write(path=\"config\", filename=\"database.yml\", content=\"development:\n  adapter: postgresql\n  database: myapp_dev\n  host: localhost\n\nproduction:\n  adapter: postgresql\n  database: myapp_prod\n  host: prod.db.example.com\n\")
</example>

### Examples of bad usage

<example>
- Trying to modify just one function in an existing file:
Write(path=\"src\", filename=\"utils.el\", content=\"(defun helper-func () ...) ; Other existing functions lost!\")
<reasoning>
Should use Edit instead to preserve other functions.
</reasoning>
</example>
"
   :function #'mevedel-tools--write-file
   :args (list '(:name "path"
                 :type string
                 :description "The directory where to create the file, \".\" is the current directory.")
               '(:name "filename"
                 :type string
                 :description "The name of the file to create.")
               '(:name "content"
                 :type string
                 :description "The content to write to the file"))
   :category "mevedel"
   :async t)

  ;; Custom Edit tool with user confirmation
  (gptel-make-tool
   :name "Edit"
   :function #'mevedel-tools--edit-files
   :description "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:

- Short replacements: Provide both `old_str` and `new_str`, in which
case `old_str` needs to exactly match one unique section of the original
file, including any whitespace. Make sure to include enough context that
the match is not ambiguous. The entire original string will be replaced
with `new str`.
- Long or involved replacements: set the `diff` parameter to true and
provide a unified diff in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch)
  and be in unified format.
- The LLM should generate the diff such that the file paths within the
  diff (e.g., '--- a/filename' '+++ b/filename') are appropriate for the
  'path'.

To simply insert text at some line, use the `Insert` tool instead.

### When to use `Edit`

- Modifying existing files with surgical precision
- Making targeted changes to code or configuration
- Replacing specific strings, functions, or sections
- Any time you need to change part of an existing file

### When NOT to use `Edit`

- Creating brand new files -> use `Write`
- You haven't read the file yet -> must read first (tool will error)

### How to use `Edit`

- MUST read the file first (required, tool will error otherwise)
- Provide exact `old_str` to match (including proper indentation from
  file content)
- Provide `new_str` as replacement (must be different from `old_str`)
- The edit will FAIL if `old_str` is not unique
- Preserve exact indentation from the file content
- Always prefer editing existing files over creating new ones

### Examples of good usage

<example>
- Updating a function signature:
Edit(path=\"src/auth.el\", old_str=\"(defun validate-user (username password)\", new_str=\"(defun validate-user (username password &optional timeout)\")
</example>

<example>
- Fixing a configuration value:
Edit(path=\"config.json\", old_str=\"\"port\": 3000\", new_str=\"\"port\": 8080\")
</example>

### Examples of bad usage

<example>
- Trying to replace all instances of a common word:
Edit(path=\"README.md\", old_str=\"user\", new_str=\"customer\")
<reasoning>
Should use diff method if this is intentional.
</reasoning>
</example>
"
   :args '((:name "path"
            :type string
            :description "File path or directory to edit")
           (:name "old_str"
            :type string
            :optional t
            :description "Original string to replace. If providing a unified diff, this should be false")
           (:name "new_str"
            :type string
            :description "Replacement text (for string mode) or unified diff (for diff mode)")
           (:name "use_diff"
            :type boolean
            :description "If true, new_str is treated as a unified diff to apply"))
   :async t
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "Insert"
   :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a
specific location with no changes to the surrounding context.

### When to use `Insert`

- When you only need to add new content to a file
- When you know the exact line number for the insertion
- For purely additive actions that don't require changing surrounding
  context

### When NOT to use `Insert`

- When you need to replace or modify existing text -> use `Edit`
- When you need to create a new file entirely -> use `Write`

### How to use `Insert`

- The `line_number` parameter specifies the line *after* which to insert
  `text`
- Use `line_number: 0` to insert at the very beginning of the file
- Use `line_number: -1` to insert at the very end of the file
- This tool is preferred over `Edit` when only insertion is required

### Examples of good usage

<example>
- Add config variable
Insert(path=\"config.js\", line_number=5, new_str=\"const API_KEY = process.env.API_KEY;\")
</example>

<example>
- Add section at end
Insert(path=\"README.md\", line_number=-1, new_str=\"\n## Contributing\nPlease fork and submit pull requests.\")
</example>

### Examples of bad usage

<example>
- Replacing existing return
Insert(path=\"script.py\", line_number=10, new_str=\"return new_value\")
<reasoning>
Use Edit to replace existing lines, not Insert.
</reasoning>
</example>

<example>
- Creating new file
Insert(path=\"new-file.txt\", line_number=0, new_str=\"content\")
<reasoning>
Use Write to create entirely new files.
</reasoning>
</example>
"
   :function #'mevedel-tools--insert-in-file
   :args '((:name "path"
            :description "Path of file to edit."
            :type string)
           (:name "line_number"
            :description "The line number at which to insert `new_str`, with
- 0 to insert at the beginning, and
- -1 to insert at the end."
            :type integer)
           (:name "new_str"
            :description "String to insert at `line_number`."
            :type string))
   :category "mevedel"
   :async t
   :include t))

(provide 'mevedel-tool-fs)
;;; mevedel-tool-fs.el ends here
