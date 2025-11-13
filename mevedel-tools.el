;;; mevedel-tools.el -- Tool definitions -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib))


;;
;;; Directory access

(defvar-local mevedel--pending-access-requests nil
  "Alist of (ROOT . STATUS) for in-flight access requests.

STATUS can be \\='pending, \\='granted, or \\='denied.

This is buffer-local per chat buffer to deduplicate access prompts
within a session.")

(defvar-local mevedel--access-request-lock nil
  "Non-nil when an access request is being processed.
This is buffer-local per chat buffer to prevent race conditions.")

(defvar-local mevedel--request-file-snapshots nil
  "Alist of (FILEPATH . ORIGINAL-CONTENT) tracking files modified.

Each entry stores the original state of a file before any modifications
in the current request. ORIGINAL-CONTENT is nil if the file didn't exist
before the request. Cleared when request completes.")

(defun mevedel-tools--request-access (root reason &optional buffer)
  "Request access to ROOT with REASON, handling concurrent requests gracefully.
Returns t if access granted, nil if denied or interrupted.

BUFFER is the chat buffer context for buffer-local state (defaults to
current buffer). This ensures we're operating on the correct session's
access grants."
  (with-current-buffer (or buffer (current-buffer))
    (let ((pending-status (alist-get root mevedel--pending-access-requests
                                     nil nil #'string=)))
      (cond
       ;; Already granted in this batch
       ((eq pending-status 'granted) t)

       ;; Already denied in this batch
       ((eq pending-status 'denied) nil)

       ;; Request is pending - wait for it with user interrupt support
       ((eq pending-status 'pending)
        (let ((result (mevedel--wait-for-access-resolution root)))
          (eq result 'granted)))

       ;; New request - acquire lock and prompt
       (t
        ;; Wait for lock with interrupt support
        (let ((got-lock
               (while-no-input
                 (while mevedel--access-request-lock
                   (sit-for 0.05))
                 t)))
          (when got-lock
            (setq mevedel--access-request-lock t)
            (unwind-protect
                (progn
                  ;; Double-check after acquiring lock
                  (let ((status (alist-get root mevedel--pending-access-requests
                                           nil nil #'string=)))
                    (if status
                        (eq status 'granted)
                      ;; Mark as pending
                      (setf (alist-get root mevedel--pending-access-requests
                                       nil nil #'string=) 'pending)

                      ;; Actually prompt user
                      (let ((granted (mevedel--prompt-user-for-access root reason)))
                        (setf (alist-get root mevedel--pending-access-requests
                                         nil nil #'string=)
                              (if granted 'granted 'denied))

                        ;; Update session tracking if granted
                        (when granted
                          (let* ((workspace-root (mevedel-workspace--root (mevedel-workspace)))
                                 (current-roots (alist-get workspace-root mevedel-workspace-additional-roots
                                                           nil nil #'equal)))
                            ;; Add root to the list if not already present
                            (unless (member root current-roots)
                              (setf (alist-get workspace-root mevedel-workspace-additional-roots
                                               nil nil #'equal)
                                    (cons root current-roots)))))

                        granted))))
              (setq mevedel--access-request-lock nil)))))))))

(defun mevedel--wait-for-access-resolution (root)
  "Wait for pending access request for ROOT to resolve.

Returns \\='granted, \\='denied, or \\='interrupted."
  (let ((result
         (while-no-input
           (while (eq (alist-get root mevedel--pending-access-requests
                                 nil nil #'string=)
                      'pending)
             ;; Check every 50ms, allow redisplay
             (sit-for 0.05))
           ;; Return the final status
           (alist-get root mevedel--pending-access-requests
                      nil nil #'string=))))
    (cond
     ;; `while-no-input' returned t (user input)
     ((eq result t) 'interrupted)
     ;; User quit with C-g
     ((null result) 'interrupted)
     ;; 'granted or 'denied
     (t result))))

(defun mevedel--prompt-user-for-access (root reason)
  "Prompt user for access to ROOT with REASON.
Returns t if granted, nil if denied."
  (yes-or-no-p
   (format "Grant LLM access to directory: %s\n\nReason: %s\n\nAllow access? "
           root reason)))

(defun mevedel--clear-pending-access-requests (&rest _)
  "Clear the pending access requests cache.
Should be called after each LLM response completes."
  (setq mevedel--pending-access-requests nil))

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
;;; Custom tools

(defvar mevedel-tools--ro-tools
  '("mevedel_glob_files" "mevedel_read_file_lines" "mevedel_grep_files"
    "mevedel_ask_user" "mevedel_request_directory_access" "mevedel_execute_bash"))
(defvar mevedel-tools--rw-tools
  '("mevedel_make_directory" "mevedel_write_file" "mevedel_edit_files" "mevedel_insert_in_file"))

;;;; Read tools

(defun mevedel--define-read-tools ()
  "Define custom read-only tools for `mevedel'."
  (gptel-make-tool
   :name "mevedel_glob_files"
   :description "Recursively find files matching a provided glob pattern.

- Supports glob patterns like \"*.md\" or \"*test*.py\".
  The glob applies to the basename of the file (with extension).
- Returns matching file paths at all depths sorted by modification time.
  Limit the depth of the search by providing the `depth` argument.
- When you are doing an open ended search that may require multiple rounds
  of globbing and grepping, use the \"task\" tool instead
- You can call multiple tools in a single response.  It is always better to
  speculatively perform multiple searches in parallel if they are potentially useful."
   :function (lambda (callback pattern &optional path depth)
               (cl-block nil
                 (when (string-empty-p pattern)
                   (cl-return
                    (funcall callback "Error: pattern must not be empty")))
                 (if path
                     (unless (and (file-readable-p path) (file-directory-p path))
                       (cl-return
                        (funcall callback (format "Error: path %s is not readable" path))))
                   (setq path "."))
                 (unless (executable-find "tree")
                   (cl-return
                    (funcall callback "Error: Executable `tree` not found.  This tool cannot be used")))
                 (let* ((full-path (expand-file-name path))
                        (file-root (mevedel--file-in-allowed-roots-p full-path)))
                   ;; No access yet, request it
                   (unless file-root
                     (let* ((requested-root (or (file-name-directory full-path)
                                                full-path))
                            (reason (format "Need to find files in: %s" path))
                            (granted (mevedel-tools--request-access requested-root reason)))
                       ;; Access denied
                       (unless granted
                         (cl-return
                          (funcall callback
                                   (format "Error: Access denied to %s. Cannot search files" requested-root))))))
                   ;; Access granted, proceed
                   (with-temp-buffer
                     (let* ((args (list "-l" "-f" "-i" "-I" ".git"
                                        "--sort=mtime" "--ignore-case"
                                        "--prune" "-P" pattern full-path))
                            (args (if (natnump depth)
                                      (nconc args '("-L" (number-to-string depth)))
                                    args))
                            (exit-code (apply #'call-process "tree" nil t nil args)))
                       (when (/= exit-code 0)
                         (goto-char (point-min))
                         (insert (format "glob_files failed with exit code %d\n.STDOUT:\n\n"
                                         exit-code))))
                     (funcall callback (buffer-string))))))
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
   :name "mevedel_read_file_lines"
   :description "Read file contents between specified line numbers `start_line` and `end_line`,
with both ends included.

Consider using the \"grep_files\" tool to find the right range to read first.

Reads the whole file if the line range is not provided.

Files over 512 KB in size can only be read by specifying a line range."
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
            :description "The line up to which to read, defaults to the end of the file."
            :optional t))
   :category "mevedel"
   ;; :confirm (lambda (_ start end) (or (not start) (not end) (> (- end start) 100)))
   :async t
   :include t)

  (gptel-make-tool
   :name "mevedel_grep_files"
   :description "Search for text in file(s) at `path`.

Use this tool to find relevant parts of files to read.

Returns a list of matches prefixed by the line number, and grouped by file.
Can search an individual file (if providing a file path) or a directory.
Consider using this tool to find the right line range for the \"read_file_lines\" tool.

When searching directories, optionally restrict the types of files in the search with a `glob`.
Can request context lines around each match using the `context_lines` parameters."
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

  ;; Tool for LLM to ask user questions during execution
  (gptel-make-tool
   :name "mevedel_ask_user"
   :function (lambda (callback question &optional options)
               "Ask the user a question and wait for response.

CALLBACK is automatically prepended for async tools.
QUESTION is the question string to ask the user.
OPTIONS is an optional array of predefined choices."
               (let ((answer (if options
                                 (let ((choice (completing-read
                                                (concat question " ")
                                                (append options '("Other"))
                                                nil nil)))
                                   (if (equal choice "Other")
                                       (read-string (concat question " (custom): "))
                                     choice))
                               (read-string (concat question " ")))))
                 (funcall callback answer)))
   :description "Ask the user a question and wait for their response.
Use this when you need clarification or user input to proceed with a task."
   :args '((:name "question"
            :type string
            :description "The question to ask the user")
           (:name "options"
            :type array
            :items (:type string)
            :optional t
            :description "Optional list of predefined choices for the user to select from"))
   :async t
   :include t
   :category "mevedel")

  ;; Tool for LLM to request access to new directories
  (gptel-make-tool
   :name "mevedel_request_directory_access"
   :function (lambda (callback directory reason)
               "Request user permission to access a directory.

CALLBACK is for async execution.
DIRECTORY is the path to request access to.
REASON explains why access is needed."
               (cl-block nil
                 (unless (and (file-readable-p directory) (file-directory-p directory))
                   (cl-return
                    (funcall callback (format "Error: directory %s is not readable" directory))))
                 (let ((expanded (expand-file-name directory)))
                   (if (yes-or-no-p
                        (format "Grant LLM access to directory: %s\n\nReason: %s\n\nAllow access? "
                                expanded reason))
                       (progn
                         (mevedel-add-project-root expanded)
                         (funcall callback
                                  (format "Access granted to %s. You can now read and write files in this directory." expanded)))
                     (funcall callback
                              (format "Access denied to %s. You cannot access files in this directory." expanded))))))
   :description "Request access to a directory outside the current allowed project roots. You must explain why you need access to this directory."
   :args '((:name "directory"
            :type string
            :description "Absolute or relative path to the directory you need to access")
           (:name "reason"
            :type string
            :description "Clear explanation of why you need access to this directory and what you plan to do there"))
   :async t
   :confirm nil  ;; Confirmation handled within the tool
   :include t
   :category "mevedel")

  (gptel-make-tool
   :name "mevedel_execute_bash"
   :function (lambda (callback command)
               "Execute a bash command and return its output.

COMMAND is the bash command string to execute."
               (with-temp-buffer
                 (let* ((exit-code (call-process "bash" nil (current-buffer) nil "-c" command))
                        (output (buffer-string)))
                   (if (zerop exit-code)
                       (funcall callback output)
                     (funcall callback (format "Command failed with exit code %d:\nSTDOUT+STDERR:\n%s" exit-code output))))))
   :description "Execute Bash commands.

This tool provides access to a Bash shell with GNU coreutils (or equivalents) available.
Use this to inspect system state, run builds, tests or other development or system administration tasks.

Do NOT use this for file operations, finding, reading or editing files.
Use the provided file tools instead: `read_file_lines`, `write_file`, `edit_files`, \
`glob_files`, `grep_files`

- Quote file paths with spaces using double quotes.
- Chain dependent commands with && (or ; if failures are OK)
- Use absolute paths instead of cd when possible
- For parallel commands, make multiple `execute_bash` calls in one message
- Run tests, check your work or otherwise close the loop to verify changes you make.

EXAMPLES:
- List files with details: 'ls -lah /path/to/dir'
- Find recent errors: 'grep -i error /var/log/app.log | tail -20'
- Check file type: 'file document.pdf'
- Count lines: 'wc -l *.txt'

The command will be executed in the current working directory.  Output is
returned as a string.  Long outputs should be filtered/limited using pipes."
   :args '((:name "command"
            :type string
            :description "The Bash command to execute.  \
Can include pipes and standard shell operators.
Example: 'ls -la | head -20' or 'grep -i error app.log | tail -50'"))
   :async t
   :confirm t
   :include t
   :category "gptel-agent"))

(defun mevedel--define-tools ()
  "Define custom mevedel tools for gptel-agent."

  (gptel-make-tool
   :name "mevedel_make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :function (lambda (parent name)
               (let* ((full-path (expand-file-name parent))
                      (file-root (mevedel--file-in-allowed-roots-p full-path)))
                 ;; No access yet, request it
                 (unless file-root
                   (let* ((requested-root (or (file-name-directory full-path)
                                              full-path))
                          (reason (format "Need to create directory in: %s" parent))
                          (granted (mevedel-tools--request-access requested-root reason)))
                     ;; Access denied
                     (unless granted
                       (error "Error: Access denied to %s. Cannot create directory" requested-root))))
                 ;; Access granted, proceed
                 (condition-case errdata
                     (progn
                       (make-directory (expand-file-name name parent) t)
                       (format "Directory %s created/verified in %s" name parent))
                   (error (format "Error creating directory %s in %s:\n%S" name parent errdata)))))
   :args (list '(:name "parent"
                 :type "string"
                 :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                 :type "string"
                 :description "The name of the new directory to create, e.g. testdir"))
   :category "mevedel"
   :confirm t)

  (gptel-make-tool
   :name "mevedel_write_file"
   :description "Create a new file with the specified content.
Overwrites an existing file, so use with care!
Consider using the more granular tools \"mevedel_insert_in_file\" or \"mevedel_edit_files\" first."
   :function (lambda (callback path filename content)
               (cl-block nil
                 (let* ((full-path (expand-file-name filename path))
                        (file-root (mevedel--file-in-allowed-roots-p full-path)))
                   ;; No access yet, request it
                   (unless file-root
                     (let* ((requested-root (or (file-name-directory full-path)
                                                full-path))
                            (reason (format "Need to create %s in directory: %s" filename path))
                            (granted (mevedel-tools--request-access requested-root reason)))
                       ;; Access denied
                       (unless granted
                         (cl-return
                          (funcall callback (format "Error: Access denied to %s. Cannot create %s in directory %s" requested-root filename path))))))
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
                          temp-file original-content full-path callback))
                     (error (funcall callback (format "Error: Could not write file %s:\n%S" path errdata)))))))
   :args (list '(:name "path"
                 :type "string"
                 :description "The directory where to create the file, \".\" is the current directory.")
               '(:name "filename"
                 :type "string"
                 :description "The name of the file to create.")
               '(:name "content"
                 :type "string"
                 :description "The content to write to the file"))
   :category "mevedel"
   :async t
   :confirm t)

  ;; Custom mevedel_edit_files tool with user confirmation
  (gptel-make-tool
   :name "mevedel_edit_files"
   :function #'mevedel-tools--edit-files
   :description "Replace text in one or more files.

To edit a single file, provide the file `path`.

For the replacement, there are two methods:
- Short replacements: Provide both `old_str` and `new_str`, in which case `old_str` \
needs to exactly match one unique section of the original file, including any whitespace.  \
Make sure to include enough context that the match is not ambiguous.  \
The entire original string will be replaced with `new str`.
- Long or involved replacements: set the `diff` parameter to true and provide a unified diff \
in `new_str`. `old_str` can be ignored.

To edit multiple files,
- provide the directory path,
- set the `diff` parameter to true
- and provide a unified diff in `new_str`.

Diff instructions:

- The diff must be provided within fenced code blocks (=diff or =patch) and be in unified format.
- The LLM should generate the diff such that the file paths within the diff \
  (e.g., '--- a/filename' '+++ b/filename') are appropriate for the 'path'.

To simply insert text at some line, use the \"mevedel_insert_in_file\" instead."
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
   :name "mevedel_insert_in_file"
   :description "Insert `new_str` after `line_number` in file at `path`.

Use this tool for purely additive actions: adding text to a file at a \
specific location with no changes to the surrounding context."
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
            :description "String to insert at `line_number`."))
   :category "mevedel"
   :async t
   :include t))


;;
;;; Custom tool implementations

;;;; Read tools

(cl-defun mevedel-tools--read-file-lines (callback filename start-line end-line)
  "Return lines START-LINE to END-LINE fom FILENAME via CALLBACK."
  (unless (file-readable-p filename)
    (cl-return-from 'mevedel-tools--read-file-lines
      (funcall callback (format "Error: File %s is not readable" filename))))

  (when (file-directory-p filename)
    (cl-return-from 'mevedel-tools--read-file-lines
      (funcall callback (format "Error: Cannot read directory %s as file" filename))))

  (when (file-symlink-p filename)
    (setq filename (file-truename filename)))

  (let* ((full-path (expand-file-name filename))
         (file-root (mevedel--file-in-allowed-roots-p full-path)))
    ;; No access yet, request it
    (unless file-root
      (let* ((requested-root (or (file-name-directory full-path)
                                 full-path))
             (reason (format "Need to read file: %s" filename))
             (granted (mevedel-tools--request-access requested-root reason)))
        ;; Access denied
        (unless granted
          (cl-return-from 'mevedel-tools--read-file-lines
            (funcall callback
                     (format "Error: Access denied to %s. Cannot read file %s" requested-root filename)))))))
  ;; Access granted, proceed
  (if (and (not start-line) (not end-line)) ;read full file
      (if (> (file-attribute-size (file-attributes filename))
             (* 512 1024))
          (cl-return-from 'mevedel-tools--read-file-lines
            (funcall callback "Error: File is too large (> 512 KB).
Please specify a line range to read"))
        (with-temp-buffer
          (insert-file-contents filename)
          (funcall callback (buffer-string))))
    ;; TODO: Handle nil start-line OR nil end-line
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
                (insert-file-contents-literally
                 filename nil byte-offset (+ byte-offset chunk-size))
                (setq byte-offset (+ byte-offset chunk-size))))))

        (funcall callback (buffer-string))))))

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
and optional context. Results are sorted by modification time."
  (unless (file-readable-p path)
    (cl-return-from 'mevedel-tools--grep
      (funcall callback (format "Error: File or directory %s is not readable" path))))
  (let ((grepper (or (executable-find "rg") (executable-find "grep"))))
    (unless grepper
      (cl-return-from 'mevedel-tools--grep
        (funcall callback "Error: ripgrep/grep not available, this tool cannot be used")))
    (let* ((full-path (expand-file-name path))
           (file-root (mevedel--file-in-allowed-roots-p full-path)))
      ;; No access yet, request it
      (unless file-root
        (let* ((requested-root (or (file-name-directory full-path)
                                   full-path))
               (reason (format "Need to grep in: %s" path))
               (granted (mevedel-tools--request-access requested-root reason)))
          ;; Access denied
          (unless granted
            (cl-return-from 'mevedel-tools--grep
              (funcall callback
                       (format "Error: Access denied to %s. Cannot grep in files" requested-root)))))))
    ;; Access granted, proceed
    (with-temp-buffer
      (let* ((args
              (cond
               ((string-suffix-p "rg" grepper)
                (delq nil (list "--sort=modified"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--glob=%s" glob))
                                ;; "--files-with-matches" "--max-count=10"
                                "--heading" "--line-number" "-e" regex
                                (expand-file-name (substitute-in-file-name path)))))
               ((string-suffix-p "grep" grepper)
                (delq nil (list "--recursive"
                                (and (natnump context-lines)
                                     (format "--context=%d" context-lines))
                                (and glob (format "--include=%s" glob))
                                "--line-number" "--regexp" regex
                                (expand-file-name (substitute-in-file-name path)))))))
             (exit-code (apply #'call-process grepper nil '(t t) nil args)))
        (when (/= exit-code 0)
          (goto-char (point-min))
          (insert (format "Error: search failed with exit-code %d.  Tool output:\n\n" exit-code)))
        (funcall callback (buffer-string))))))

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
  (unless (file-readable-p path)
    (cl-return-from 'mevedel-tools--edit-files
      (funcall callback (format "Error: File or directory %s is not readable" path))))

  (unless new-str-or-diff
    (cl-return-from 'mevedel-tools--edit-files
      (funcall callback "Required argument `new_str' missing")))
  ;; Check access and request it if needed
  (let* ((expanded-path (expand-file-name path))
         (file-root (mevedel--file-in-allowed-roots-p expanded-path)))
    ;; No access yet, request it
    (unless file-root
      (let* ((requested-root (or (file-name-directory expanded-path)
                                 expanded-path))
             (reason (format "Need to edit file: %s" path))
             (granted (mevedel-tools--request-access requested-root reason)))
        ;; Access denied
        (unless granted
          (cl-return-from 'mevedel-tools--edit-files
            (funcall callback
                     (format "Error: Access denied to %s. Cannot edit file in %s" requested-root path))))))
    ;; Access granted, proceed with edit
    (mevedel-tools--edit-files-1 callback expanded-path old-str new-str-or-diff use-diff)))

(cl-defun mevedel-tools--insert-in-file (callback path line-number new-str)
  "Insert NEW-STR at LINE-NUMBER in file at PATH.

CALLBACK is a function to call with the result.
LINE-NUMBER conventions:
- 0 inserts at the beginning of the file
- -1 inserts at the end of the file
- N > 1 inserts before line N"
  (unless (file-readable-p path)
    (cl-return-from 'mevedel-tools--insert-in-file
      (funcall callback (format "Error: File %s is not readable" path))))

  (when (file-directory-p path)
    (cl-return-from 'mevedel-tools--insert-in-file
      (funcall callback (format "Error: Cannot insert into directory %s" path))))

  ;; Snapshot the file before any modifications
  (mevedel--snapshot-file-if-needed path)

  (let ((temp-file (make-temp-file "mevedel-edit-")))
    (condition-case err
        (let* ((expanded-path (expand-file-name path))
               (file-root (mevedel--file-in-allowed-roots-p expanded-path))
               (original-content (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string))))
          ;; No access yet, request it
          (unless file-root
            (let* ((requested-root (or (file-name-directory expanded-path)
                                       expanded-path))
                   (reason (format "Need to insert into file: %s" path))
                   (granted (mevedel-tools--request-access requested-root reason)))
              ;; Access denied
              (unless granted
                (cl-return-from 'mevedel-tools--insert-in-file
                  (funcall callback
                           (format "Error: Access denied to %s. Cannot insert in file %s" requested-root path))))))

          ;; Access granted, proceed with edit
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
           temp-file original-content path callback))

      (error
       (when (file-exists-p temp-file)
         (delete-file temp-file))
       (funcall callback (format "Error: %s" (error-message-string err)))))))

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
    (cl-return-from 'mevedel-tools--edit-files-1
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
                  (cl-return-from 'mevedel-tools--edit-files-1
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
                                                              temp-file original-content path callback)))))

            ;; DIFF MODE (ELSE branch)
            (mevedel-tools--apply-diff-to-temp temp-file new-str-or-diff
                                               (lambda (success-or-error)
                                                 (if (stringp success-or-error)
                                                     ;; Error
                                                     (progn
                                                       (delete-file temp-file)
                                                       (funcall callback success-or-error))
                                                   ;; Success - show diff and confirm
                                                   (mevedel-tools--show-changes-and-confirm
                                                    temp-file original-content path callback))))))

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
    (cl-return-from 'mevedel-tools--apply-diff-to-temp
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

(defun mevedel-tools--prompt-for-changes ()
  "Prompt user to approve/reject/edit changes in *mevedel-diff-preview* buffer.
Expects buffer-local variables to be set in the diff-preview buffer."
  (let ((diff-buffer (get-buffer "*mevedel-diff-preview*")))
    (unless diff-buffer
      (error "No diff-preview buffer found"))

    (pop-to-buffer diff-buffer)

    (let ((choice (read-char-choice
                   "Apply changes? (a)pprove, (r)eject, (e)dit, (f)eedback: "
                   '(?a ?r ?e ?f)))
          (temp-file (buffer-local-value 'mevedel--temp-file diff-buffer))
          (real-path (buffer-local-value 'mevedel--real-path diff-buffer))
          (final-callback (buffer-local-value 'mevedel--final-callback diff-buffer))
          (user-modified (buffer-local-value 'mevedel--user-modified diff-buffer))
          (original-wconf (buffer-local-value 'mevedel--original-window-config diff-buffer)))

      (pcase choice
        (?a
         ;; Approved - apply changes
         (with-current-buffer diff-buffer
           (mevedel-diff-apply-buffer))
         ;; Note: Patch buffer will be updated with final diffs at request end
         (funcall final-callback
                  (if user-modified
                      (format "Changes approved and applied to %s, but were modified by user via ediff. You may need to read the relevant sections of the file to see the final applied changes." real-path)
                    (format "Changes approved and applied to %s" real-path)))
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))
        (?r
         ;; Rejected without feedback
         (funcall final-callback
                  "Changes rejected by user. No feedback provided.")
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))
        (?e
         ;; Run `ediff' on patch - set up hook to return here after ediff
         ;; Do NOT restore window config - let ediff manage windows
         (with-current-buffer diff-buffer
           (setq-local mevedel--user-modified t))
         ;; Add one-shot hook to return to confirmation after ediff
         (add-hook 'mevedel--ediff-finished-hook #'mevedel-tools--prompt-for-changes)
         (with-current-buffer diff-buffer
           (mevedel-ediff-patch)))
        (?f
         ;; Rejected with feedback
         (let ((feedback (read-string "What should be changed? ")))
           (funcall final-callback
                    (format "Changes rejected by user. User feedback: %s" feedback)))
         ;; Cleanup and restore window config
         (kill-buffer diff-buffer)
         (delete-file temp-file)
         (when (window-configuration-p original-wconf)
           (set-window-configuration original-wconf)))))))

(defun mevedel-tools--show-changes-and-confirm (temp-file original-content real-path final-callback)
  "Show diff between ORIGINAL-CONTENT and TEMP-FILE, ask user to confirm.

TEMP-FILE - path to file with proposed changes
ORIGINAL-CONTENT - original file content
REAL-PATH - path to real file
FINAL-CALLBACK - callback to return final result to LLM"
  ;; Validate that we're running in the chat buffer context (tools should be called by gptel from chat buffer)
  (unless (buffer-local-value 'mevedel--workspace (current-buffer))
    (error "`mevedel-tools--show-changes-and-confirm' must be called from chat buffer context"))
  (let* ((chat-buffer (current-buffer))
         ;; The file we are editing can be in the in the main workspace root or
         ;; in another allowed one
         (root (mevedel--file-in-allowed-roots-p real-path chat-buffer))
         (workspace (mevedel-workspace chat-buffer))
         (rel-path (file-relative-name real-path root))
         (modified-content (with-temp-buffer
                             (insert-file-contents temp-file)
                             (buffer-string)))
         (diff (mevedel-tools--generate-diff original-content modified-content rel-path))
         (diff-buffer (get-buffer-create "*mevedel-diff-preview*")))

    (with-current-buffer diff-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "diff --git a/%s b/%s\n" rel-path rel-path))
        (insert diff)
        (diff-mode)
        ;; Diffs are generally easier to interact with (e.g. press RET to jump to
        ;; a change) in read-only mode, so this seems like a reasonable default.
        (read-only-mode 1)
        ;; Diffs are visually confusing if continuation lines are displayed.
        (setq-local truncate-lines t)
        ;; Re-detect patch type (i.e. 'git) now that the buffer has been populated.
        (when (derived-mode-p 'diff-mode)
          (diff-setup-buffer-type))
        (setq header-line-format
              (concat
               (propertize (format " Proposed changes to %s. -- " rel-path) 'face 'success)
               (propertize "Choose: (a)pprove, (r)eject, (e)dit, (f)eedback and reject" 'face 'help-key-binding)))
        (setq-local default-directory root
                    mevedel--workspace workspace
                    ;; Store context for re-entry after ediff
                    mevedel--temp-file temp-file
                    mevedel--real-path real-path
                    mevedel--final-callback final-callback
                    mevedel--user-modified nil
                    ;; Store window config to restore after final action
                    mevedel--original-window-config (current-window-configuration))

        (goto-char (point-min))))

    ;; Show the diff buffer and prompt
    (pop-to-buffer diff-buffer)
    (mevedel-tools--prompt-for-changes)))

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

(provide 'mevedel-tools)
;;; mevedel-tools.el ends here
