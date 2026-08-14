;;; mevedel-session-control-fs.el -- Physical session control filesystem -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs session-control filesystem operations through a target-side
;; directory descriptor.  The descriptor pins the parent directory while the
;; relative operation runs, so a pathname swap after preflight cannot redirect
;; a lease, transfer, or recovery mutation.

;;; Code:

(eval-when-compile (require 'cl-lib))

(define-error 'mevedel-session-control-fs-conflict
  "Portable control filesystem name already exists")
(define-error 'mevedel-session-control-fs-absent
  "Portable control filesystem name does not exist")

(defvar mevedel-session-control-fs--test-pause-file nil
  "Test-only target-relative pause marker, or nil in production.

The paused script gives up after a bounded wait: a test that dies before
writing the continue marker must not leave a target-side process polling
forever.")

(defun mevedel-session-control-fs--physical-spelling (path)
  "Return PATH's expanded spelling, which control operations require literally.

Whether that spelling is physical is proved on the target, in the same process
that performs the operation: the script compares the opened parent's `pwd -P'
with this spelling and refuses a final name that is a symbolic link.  Walking
the components here instead would cost one target round trip per component on
every operation and would still be checking a name the target could change
before the operation ran."
  (unless (and (stringp path) (file-name-absolute-p path)
               (not (string-prefix-p "~" (file-local-name path))))
    (error "Portable control path must be absolute: %S" path))
  (directory-file-name (expand-file-name path)))

(defun mevedel-session-control-fs--descriptor (path)
  "Return a target-side parent descriptor specification for PATH."
  (let* ((physical (mevedel-session-control-fs--physical-spelling path))
         (parent (file-name-directory physical))
         (leaf (file-name-nondirectory physical)))
    (unless (and parent (not (string-empty-p leaf)))
      (error "Portable control parent is unavailable: %s" path))
    (list :path physical
          :parent parent
          :leaf leaf)))

(defconst mevedel-session-control-fs--script
  (concat
   "set -eu\n"
   "parent=$1\n"
   "leaf=$2\n"
   "expected_pwd=$3\n"
   "operation=$4\n"
   "pause_file=$5\n"
   "test -e \"$parent\" || exit 78\n"
   "exec 9<\"$parent\"\n"
   "cd -- /proc/self/fd/9\n"
   "test \"$(pwd -P)\" = \"$expected_pwd\"\n"
   "if test -n \"$pause_file\"; then\n"
   "  : >\"$pause_file\"\n"
   "  waited=0\n"
   "  while test ! -e \"$pause_file.continue\"; do\n"
   "    sleep 0.01\n"
   "    waited=$((waited + 1))\n"
   "    test \"$waited\" -lt 6000 || exit 79\n"
   "  done\n"
   "fi\n"
   "case \"$operation\" in\n"
   "  read)\n"
   "    test ! -L \"$leaf\"\n"
   "    test -e \"$leaf\" || exit 77\n"
   "    exec 8<\"$leaf\"\n"
   "    test ! -L \"$leaf\"\n"
   "    cat <&8\n"
   "    ;;\n"
   "  write)\n"
   "    test ! -L \"$leaf\"\n"
   "    temporary=$(mktemp -- .mevedel-control-fs-XXXXXX)\n"
   "    trap 'rm -f -- \"$temporary\"' EXIT\n"
   "    cat >\"$temporary\"\n"
   "    mv -fT -- \"$temporary\" \"$leaf\"\n"
   "    trap - EXIT\n"
   "    ;;\n"
   "  create)\n"
   "    test ! -L \"$leaf\"\n"
   "    temporary=$(mktemp -- .mevedel-control-fs-XXXXXX)\n"
   "    trap 'rm -f -- \"$temporary\"' EXIT\n"
   "    cat >\"$temporary\"\n"
   "    if test ! -d \"$leaf\" && ln -- \"$temporary\" \"$leaf\"; then\n"
   "      rm -f -- \"$temporary\"\n"
   "      trap - EXIT\n"
   "      exit 0\n"
   "    fi\n"
   "    if test -e \"$leaf\" || test -L \"$leaf\"; then\n"
   "      exit 73\n"
   "    fi\n"
   "    exit 75\n"
   "    ;;\n"
   "  mkdir)\n"
   "    test ! -L \"$leaf\"\n"
   "    if mkdir -- \"$leaf\"; then\n"
   "      :\n"
   "    elif test -d \"$leaf\"; then\n"
   "      exit 73\n"
   "    else\n"
   "      exit 75\n"
   "    fi\n"
   "    ;;\n"

   "  probe)\n"
   "    test ! -L \"$leaf\"\n"
   "    test -e \"$leaf\" || exit 77\n"
   "    printf '1\\n'\n"
   "    ;;\n"
   "  directory)\n"
   "    test ! -L \"$leaf\"\n"
   "    test -e \"$leaf\" || exit 77\n"
   "    test -d \"$leaf\"\n"
   "    printf '1\\n'\n"
   "    ;;\n"
   "  delete-file)\n"
   "    test ! -L \"$leaf\"\n"
   "    rm -f -- \"$leaf\"\n"
   "    ;;\n"
   "  delete-directory)\n"
   "    test ! -L \"$leaf\"\n"
   "    rm -rf -- \"$leaf\"\n"
   "    ;;\n"
   "  clock)\n"
   "    test ! -L \"$leaf\"\n"
   "    exec 8<\"$leaf\"\n"
   "    test ! -L \"$leaf\"\n"
   "    cd -- /proc/self/fd/8\n"
   "    temporary=$(mktemp -- .mevedel-control-clock-XXXXXX)\n"
   "    trap 'rm -f -- \"$temporary\"' EXIT\n"
   "    stat -c '%Y' -- \"$temporary\"\n"
   "    ;;\n"
   "  list)\n"
   "    test ! -L \"$leaf\"\n"
   "    test -e \"$leaf\" || exit 77\n"
   "    exec 8<\"$leaf\"\n"
   "    test ! -L \"$leaf\"\n"
   "    cd -- /proc/self/fd/8\n"
   "    for entry in ./* ./.[!.]* ./..?*; do\n"
   "      if test -L \"$entry\"; then exit 76; fi\n"
   "      test -e \"$entry\" || continue\n"
   "      printf '%s\\0' \"${entry#./}\"\n"
   "    done\n"
   "    ;;\n"
   "  *) exit 74 ;;\n"
   "esac\n")
  "Target-side descriptor-relative control filesystem script.")

(defvar mevedel-session-control-fs--programs (make-hash-table :test #'equal)
  "Resolved target `bash' and `stat' paths, keyed by TRAMP prefix.

Locating a program on a remote target costs one `test -x' per `exec-path'
entry, and the durability layer inhibits the remote file-name cache, so
resolving them per operation tripled the cost of every lease, transfer, and
recovery round trip.  A stale entry cannot mis-target an operation: the
script proves its own parent directory, and a moved interpreter fails the
operation, which drops the entry.")

(defun mevedel-session-control-fs--programs (remote)
  "Return a cons of target `bash' and `stat' paths for REMOTE."
  (let ((key (or remote "")))
    (or (gethash key mevedel-session-control-fs--programs)
        (let ((bash (executable-find "bash" remote))
              (stat (executable-find "stat" remote)))
          (unless (and bash stat)
            (error "Portable control filesystem requires bash and stat"))
          (puthash key (cons bash stat)
                   mevedel-session-control-fs--programs)))))

(defun mevedel-session-control-fs--connection-directory (path)
  "Return an always-present directory on PATH's target for process dispatch.

The scripts receive their parent directory as an explicit argument, so the
working directory only selects the target.  A deleted or never-created
parent must not turn into a `Setting current directory' failure."
  (concat (or (file-remote-p path) "") "/"))

(defun mevedel-session-control-fs--run
    (path operation &optional content coding-system)
  "Run descriptor-relative OPERATION on PATH with optional CONTENT.

One target process performs the whole operation: it opens the parent
directory, proves the opened directory is the requested physical path, and
then works only through that descriptor.  Splitting the proof into a separate
preflight would double the target round trips without narrowing the window,
because the descriptor, not an earlier observation, is the authority."
  (let* ((descriptor (mevedel-session-control-fs--descriptor path))
         (parent (plist-get descriptor :parent))
         (remote (file-remote-p parent))
         (default-directory
          (mevedel-session-control-fs--connection-directory parent))
         ;; The script resolves `stat' itself through the target PATH; this
         ;; only proves both programs are present before dispatching.
         (bash (car (mevedel-session-control-fs--programs remote))))
    (let ((input (and content (make-temp-file ".mevedel-control-fs-input-")))
          (output (generate-new-buffer " *mevedel-control-fs-output*")))
      (when (eq coding-system 'no-conversion)
        (with-current-buffer output
          (set-buffer-multibyte nil)))
      (unwind-protect
          (progn
            (when input
              (with-temp-buffer
                (insert content)
                (let ((coding-system-for-write
                       (or coding-system 'utf-8-unix)))
                  (write-region (point-min) (point-max)
                                input nil 'silent))))
            ;; `let*': the process call is one of these initializers, so a
            ;; plain `let' would run it before the coding system is bound.
            (let* ((coding-system-for-read
                    (if (string= operation "read")
                        (or coding-system 'utf-8-unix)
                      'utf-8-unix))
                   (status
                   (process-file bash input output nil
                                 "-p" "-c" mevedel-session-control-fs--script
                                 "mevedel-session-control-fs"
                                 (file-local-name parent)
                                 (plist-get descriptor :leaf)
                                 (directory-file-name
                                  (file-local-name parent))
                                 operation
                                 (or mevedel-session-control-fs--test-pause-file
                                     ""))))
              (unless (and (integerp status) (zerop status))
                (cond
                 ((and (integerp status) (= status 73))
                  (signal 'mevedel-session-control-fs-conflict (list path)))
                 ((and (integerp status) (memq status '(77 78)))
                  (signal 'mevedel-session-control-fs-absent (list path)))
                 (t
                  ;; The resolved interpreters are the only cached input to
                  ;; this call, so a failure that is not a name conflict or
                  ;; an absent name retries their lookup.
                  (remhash (or remote "")
                           mevedel-session-control-fs--programs)
                  ;; A refused or failed target operation is a filesystem
                  ;; failure: callers classify publication and recovery
                  ;; retries by that condition.
                  (with-current-buffer output
                    (signal 'file-error
                            (list "Portable control operation failed"
                                  path (string-trim (buffer-string))))))))
              (with-current-buffer output
                (buffer-string))))
        (when (and input (file-exists-p input))
          (delete-file input))
        (when (buffer-live-p output)
          (kill-buffer output))))))

(defun mevedel-session-control-fs-physical-path (path)
  "Return the absolute control spelling PATH must resolve to on the target.

Each operation proves that spelling target-side; this only rejects a path
that could never be a control path."
  (mevedel-session-control-fs--physical-spelling path))

(defun mevedel-session-control-fs-read-file
    (path &optional coding-system)
  "Read target control file PATH through its pinned parent directory.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (mevedel-session-control-fs--run path "read" nil coding-system))

(defun mevedel-session-control-fs-path-exists-p (path)
  "Return non-nil when target PATH exists as a non-symlink entry."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run path "probe")
        t)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-session-control-fs-directory-p (path)
  "Return non-nil when target PATH exists as a non-symlink directory."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run path "directory")
        t)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-session-control-fs-write-file
    (path content &optional coding-system)
  "Atomically replace target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (mevedel-session-control-fs--run path "write" content coding-system)
  t)

(defun mevedel-session-control-fs-create-file
    (path content &optional coding-system)
  "Exclusively create target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run path "create" content coding-system)
        t)
    (mevedel-session-control-fs-conflict nil)))

(defun mevedel-session-control-fs-make-directory (path &optional parents)
  "Create target control directory PATH, optionally including PARENTS.

A pinned operation can only create a name inside a directory it already
opened, so missing parents are created one component at a time, each through
its own pinned parent."
  (let ((path (mevedel-session-control-fs-physical-path path)))
    (condition-case nil
        (progn
          (mevedel-session-control-fs--run path "mkdir")
          t)
      (mevedel-session-control-fs-conflict nil)
      (mevedel-session-control-fs-absent
       (let ((parent (directory-file-name
                      (file-name-directory path))))
         (unless (and parents (not (equal parent path)))
           (signal 'mevedel-session-control-fs-absent (list path)))
         (mevedel-session-control-fs-make-directory parent t)
         (mevedel-session-control-fs-make-directory path nil))))))

(defun mevedel-session-control-fs-list-directory (directory regexp)
  "Return physical paths in DIRECTORY matching REGEXP.

An absent DIRECTORY lists nothing, so callers need no separate existence
round trip.  The directory descriptor is pinned while names are enumerated.
Symlink entries fail closed before their names can be handed to a caller."
  (let* ((directory (mevedel-session-control-fs-physical-path directory))
         ;; Names are NUL separated: a newline is a legal filename byte, and
         ;; a line-separated listing would let one crafted entry present
         ;; itself to the caller as two.
         (names (condition-case nil
                    (split-string
                     (mevedel-session-control-fs--run directory "list")
                     "\0" t)
                  (mevedel-session-control-fs-absent nil)))
         result)
    (dolist (name names (nreverse result))
      (when (string-match-p regexp name)
        (push (expand-file-name name directory) result)))))

(defun mevedel-session-control-fs-delete-file (path)
  "Delete target control file PATH without following a final symlink."
  (mevedel-session-control-fs--run path "delete-file")
  t)

(defun mevedel-session-control-fs-delete-directory (path)
  "Recursively delete target control directory PATH without following its root."
  (mevedel-session-control-fs--run path "delete-directory")
  t)

(defun mevedel-session-control-fs-target-time (directory)
  "Return target filesystem seconds from a descriptor-relative marker.

Unreadable output fails closed: a silently substituted zero would make every
stored deadline look live to this client and every deadline it writes look
expired to every other client."
  (let ((value (string-trim
                (mevedel-session-control-fs--run directory "clock"))))
    (unless (string-match-p "\\`[0-9]+\\'" value)
      (signal 'file-error
              (list "Portable control clock is unavailable" directory)))
    (string-to-number value)))

(provide 'mevedel-session-control-fs)

;;; mevedel-session-control-fs.el ends here
