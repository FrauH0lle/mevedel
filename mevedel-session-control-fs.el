;;; mevedel-session-control-fs.el -- Physical session control filesystem -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs session-control filesystem operations through a target-side
;; directory descriptor.  The descriptor pins the parent directory while the
;; relative operation runs, so a pathname swap after preflight cannot redirect
;; a lease, transfer, or recovery mutation.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Every control operation both refuses to nest and runs with timers
;; suspended, and the latter is a macro, so this is a load-time dependency
;; rather than a lazily reachable one.
(require 'mevedel-transport)

(define-error 'mevedel-session-control-fs-conflict
  "Portable control filesystem name already exists")
(define-error 'mevedel-session-control-fs-absent
  "Portable control filesystem name does not exist")
(define-error 'mevedel-session-control-fs-busy
  "Portable control filesystem target is already in use")

(defun mevedel-session-control-fs--assert-idle (path)
  "Refuse a control operation on PATH that would nest in another one.

A control operation issued while the target connection is mid-command does
not merely fail: it can consume the running command's pending output and
return an answer belonging to something else, so an absent lock reads as
present and durable state is then derived from that.  Callers reachable from
a timer, a process filter, or redisplay are expected to defer instead; this
refuses the operation outright rather than let a wrong answer through."
  (when (mevedel-transport-busy-p path)
    (signal 'mevedel-session-control-fs-busy (list path))))

(defvar mevedel-session-control-fs--test-pause-file nil
  "Test-only target-relative pause marker, or nil in production.

The paused script gives up after a bounded wait: a test that dies before
writing the continue marker must not leave a target-side process polling
forever.")

(defconst mevedel-session-control-fs--spelling-cache-limit 4096
  "How many canonical control spellings to remember before starting over.")

(defvar mevedel-session-control-fs--spelling-cache
  (make-hash-table :test #'equal)
  "Canonical spellings keyed by (PATH . REMOTE-PREFIX-OF-`default-directory').

The computation is a pure function of those two, and the durability layer asks
for the same handful of paths thousands of times per session.  The connection
prefix belongs in the key because `expand-file-name' resolves an argument
against `default-directory' through its file-name handler.")

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
  (let ((key (cons path (file-remote-p default-directory))))
    (or (gethash key mevedel-session-control-fs--spelling-cache)
        (progn
          (when (> (hash-table-count
                    mevedel-session-control-fs--spelling-cache)
                   mevedel-session-control-fs--spelling-cache-limit)
            (clrhash mevedel-session-control-fs--spelling-cache))
          (puthash key
                   (directory-file-name (expand-file-name path))
                   mevedel-session-control-fs--spelling-cache)))))

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

(defconst mevedel-session-control-fs--program-script
  (concat
   "set -eu\n"
   "pause_file=$1\n"
   "shift\n"
   ;; Every operation opens its own parent and re-proves it, so one process
   ;; carrying a program is exactly as pinned as one process per operation.
   ;;
   ;; Every check below states its own failure explicitly rather than leaning
   ;; on `set -e'.  The caller runs this function on the left of a `||', and
   ;; that suppresses errexit for everything the function does, so an implicit
   ;; guard would silently continue into the operation it was meant to refuse.
   "run_op() {\n"
   "  op=$1\n"
   "  parent=$2\n"
   "  expected=$3\n"
   "  leaf=$4\n"
   "  payload=$5\n"
   "  test -e \"$parent\" || exit 78\n"
   "  exec 9<\"$parent\" || exit 70\n"
   "  cd -- /proc/self/fd/9 || exit 70\n"
   "  test \"$(pwd -P)\" = \"$expected\" || exit 70\n"
   "  if test -n \"$pause_file\"; then\n"
   "    : >\"$pause_file\"\n"
   "    waited=0\n"
   "    while test ! -e \"$pause_file.continue\"; do\n"
   "      sleep 0.01\n"
   "      waited=$((waited + 1))\n"
   "      test \"$waited\" -lt 6000 || exit 79\n"
   "    done\n"
   "  fi\n"
   "  case \"$op\" in\n"
   "    read)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      base64 -w0 <&8 || exit 67\n"
   "      ;;\n"
   "    verify)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      observed=$(base64 -w0 <&8) || exit 67\n"
   "      test \"$observed\" = \"$payload\" || exit 72\n"
   "      ;;\n"
   "    write)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      temporary=$(mktemp -- .mevedel-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      printf '%s' \"$payload\" | base64 -d >\"$temporary\" || exit 66\n"
   "      mv -fT -- \"$temporary\" \"$leaf\" || exit 67\n"
   "      trap - EXIT\n"
   "      ;;\n"
   "    create)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      temporary=$(mktemp -- .mevedel-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      printf '%s' \"$payload\" | base64 -d >\"$temporary\" || exit 66\n"
   "      if test ! -d \"$leaf\" && ln -- \"$temporary\" \"$leaf\"; then\n"
   "        rm -f -- \"$temporary\"\n"
   "        trap - EXIT\n"
   "        exit 0\n"
   "      fi\n"
   "      if test -e \"$leaf\" || test -L \"$leaf\"; then\n"
   "        exit 73\n"
   "      fi\n"
   "      exit 75\n"
   "      ;;\n"
   "    mkdir)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      if mkdir -- \"$leaf\"; then\n"
   "        :\n"
   "      elif test -d \"$leaf\"; then\n"
   "        exit 73\n"
   "      else\n"
   "        exit 75\n"
   "      fi\n"
   "      ;;\n"
   "    probe)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      ;;\n"
   "    directory)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      test -d \"$leaf\" || exit 68\n"
   "      ;;\n"
   "    delete-file)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      rm -f -- \"$leaf\" || exit 67\n"
   "      ;;\n"
   "    delete-directory)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      rm -rf -- \"$leaf\" || exit 67\n"
   "      ;;\n"
   "    clock)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   "      temporary=$(mktemp -- .mevedel-control-clock-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      stat -c '%Y' -- \"$temporary\" | base64 -w0 || exit 67\n"
   "      ;;\n"
   "    list)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   ;; A symlink entry fails the whole listing, so it is proved absent before
   ;; any name is emitted: `exit' from inside the emitting pipeline would
   ;; leave only its own subshell and report success.
   "      for entry in ./* ./.[!.]* ./..?*; do\n"
   "        if test -L \"$entry\"; then exit 76; fi\n"
   "      done\n"
   ;; Names stay NUL separated inside the payload: a newline is a legal
   ;; filename byte, and one crafted entry must not arrive as two.
   "      { for entry in ./* ./.[!.]* ./..?*; do\n"
   "          test -e \"$entry\" || continue\n"
   "          printf '%s\\0' \"${entry#./}\"\n"
   "        done\n"
   "      } | base64 -w0\n"
   "      ;;\n"
   "    *) exit 74 ;;\n"
   "  esac\n"
   "}\n"
   ;; Diagnostics are collected target-side and shipped as one trailing
   ;; record.  Handing `process-file' a local stderr file instead would make
   ;; TRAMP create a remote temporary and copy it back on every single
   ;; program, which measured as a twelfth of a remote turn.  The record is
   ;; emitted from the EXIT trap so an early stop still carries it, and its
   ;; header is a word where an operation's is a number, so no operation can
   ;; be confused with it -- which is the separation that keeps a tool writing
   ;; to stderr from presenting itself as a result.
   "diagnostics=$(mktemp) || exit 66\n"
   "trap 'printf \"diagnostic 0\\0%s\\0\" \"$(base64 -w0 <\"$diagnostics\")\";"
   " rm -f -- \"$diagnostics\"' EXIT\n"
   "index=0\n"
   "emit() {\n"
   "  index=$((index + 1))\n"
   "  status=0\n"
   ;; The operation never reads the program's own stdin.
   "  out=$(run_op \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" "
   "</dev/null 2>>\"$diagnostics\") || status=$?\n"
   "  printf '%s %s\\0%s\\0' \"$index\" \"$status\" \"$out\"\n"
   ;; A failed operation ends the program: a compare-and-set expresses its
   ;; precondition as an earlier operation, so later ones must not run.  An
   ;; operation marked optional is one whose failure the caller expects to
   ;; interpret itself, such as ensuring a directory that already exists.
   "  if test \"$status\" -ne 0 && test \"$6\" != 1; then\n"
   "    exit 0\n"
   "  fi\n"
   "}\n"
   ;; A request arrives as arguments when it fits and on stdin when it does
   ;; not.  Arguments cost nothing, while a stdin file costs TRAMP a remote
   ;; temporary and a copy into it on every program -- but an argument list
   ;; has a size ceiling that a file does not, so both readers stay.  A field
   ;; is one argument because NUL, the framing byte, is the one byte a
   ;; filename cannot contain and so cannot be embedded in one.
   "if test \"$#\" -gt 0; then\n"
   "  while test \"$#\" -ge 6; do\n"
   "    emit \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" \"$6\"\n"
   "    shift 6\n"
   "  done\n"
   "  test \"$#\" -eq 0 || exit 71\n"
   "else\n"
   "  while IFS= read -r -d '' op; do\n"
   "    IFS= read -r -d '' parent || exit 71\n"
   "    IFS= read -r -d '' expected || exit 71\n"
   "    IFS= read -r -d '' leaf || exit 71\n"
   "    IFS= read -r -d '' payload || exit 71\n"
   "    IFS= read -r -d '' optional || exit 71\n"
   "    emit \"$op\" \"$parent\" \"$expected\" \"$leaf\" \"$payload\" \"$optional\"\n"
   "  done\n"
   "fi\n")
  "Target-side script running a whole program of pinned control operations.

Payloads are base64 so one framing carries arbitrary bytes, including the
NUL-separated listing names and content that a shell cannot pass through a
command substitution literally.  `base64' resolves through the target PATH,
like `stat'.")

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

(defconst mevedel-session-control-fs--program-verbs
  '((read . "read")
    (verify . "verify")
    (write . "write")
    (create . "create")
    (make-directory . "mkdir")
    (path-exists-p . "probe")
    (directory-p . "directory")
    (delete-file . "delete-file")
    (delete-directory . "delete-directory")
    (target-time . "clock")
    (list-directory . "list"))
  "Program operation names mapped to their target-side verbs.")

(defun mevedel-session-control-fs--program-status (code)
  "Return the result vocabulary for target exit CODE."
  (cond
   ((eq code 0) 'ok)
   ((eq code 72) 'mismatch)
   ((eq code 73) 'conflict)
   ((memq code '(77 78)) 'absent)
   (t 'failed)))

(defun mevedel-session-control-fs--program-value (op payload)
  "Decode target PAYLOAD for OP's verb into its Lisp result."
  (pcase (plist-get op :op)
    ('read (decode-coding-string payload
                                 (or (plist-get op :coding) 'utf-8-unix)))
    ('list-directory (split-string payload "\0" t))
    ('target-time
     (let ((text (string-trim payload)))
       (unless (string-match-p "\\`[0-9]+\\'" text)
         (signal 'file-error
                 (list "Portable control clock is unavailable"
                       (plist-get op :path))))
       (string-to-number text)))
    (_ nil)))

(defun mevedel-session-control-fs--program-fields (op)
  "Return OP encoded as the six request fields the target script reads."
  (let* ((verb (or (cdr (assq (plist-get op :op)
                              mevedel-session-control-fs--program-verbs))
                   (error "Unknown control program operation: %S"
                          (plist-get op :op))))
         (descriptor
          (mevedel-session-control-fs--descriptor (plist-get op :path)))
         (parent (plist-get descriptor :parent))
         (content (plist-get op :content))
         (payload
          (cond
           ((null content) "")
           ((multibyte-string-p content)
            (base64-encode-string
             (encode-coding-string
              content (or (plist-get op :coding) 'utf-8-unix))
             t))
           (t (base64-encode-string content t)))))
    (list verb
          (file-local-name parent)
          (directory-file-name (file-local-name parent))
          (plist-get descriptor :leaf)
          payload
          (if (plist-get op :optional) "1" "0"))))

(defun mevedel-session-control-fs--program-request (operations)
  "Return the NUL-framed target request encoding OPERATIONS."
  (mapconcat
   (lambda (op)
     (mapconcat #'identity
                (append (mevedel-session-control-fs--program-fields op)
                        (list ""))
                "\0"))
   operations
   ""))

(defconst mevedel-session-control-fs--argument-budget 3072
  "Largest quoted argument run, in bytes, that may travel on the command line.

The binding constraint is not the target's `ARG_MAX' -- that is megabytes.  It
is one physical line of the command TRAMP writes to the connection process:
that process talks over a pty, and a pty in canonical mode truncates past
`N_TTY_BUF_SIZE', 4 KiB.  Exceeding it does not fail cleanly.
`process-send-string' blocks inside the write, which no timer interrupts and
no timeout unwinds, so the connection is wedged for the life of the process.

Only the arguments are budgeted, because only they are one unbroken line.
`tramp-send-string' preserves newlines, so the script -- far larger than this
budget -- reaches the pty as a hundred short lines and never approaches the
ceiling.  The arguments follow the script's last line, so a kilobyte of the
4 KiB is left to it and to TRAMP's own prefix.

The size is measured after shell quoting, which is what actually lands on the
line: base64 grows about two percent, but a path holding spaces can nearly
double.  `shell-quote-argument' stands in for TRAMP's variant, which differs
only in newline handling -- and a newline can only help, because it ends the
line being measured.")

(defun mevedel-session-control-fs--program-arguments (operations)
  "Return OPERATIONS as target argument fields, or nil to use the request file.

Arguments are the cheap delivery: they ride the command line TRAMP already
sends, where a request file costs a remote temporary and a copy into it.  They
are refused for a request over `mevedel-session-control-fs--argument-budget',
and for a field carrying bytes the command line cannot represent -- TRAMP
encodes the command line with the connection coding system, while the request
file is written without conversion, so a name outside ASCII is only
byte-transparent through the file."
  (let ((fields (mapcan #'mevedel-session-control-fs--program-fields
                        operations))
        (total 0))
    (catch 'oversized
      (dolist (field fields)
        (unless (string-match-p "\\`[[:ascii:]]*\\'" field)
          (throw 'oversized nil))
        ;; One byte for the separator the command line will need anyway.
        (setq total (+ total (string-bytes (shell-quote-argument field)) 1))
        (when (> total mevedel-session-control-fs--argument-budget)
          (throw 'oversized nil)))
      fields)))

(defconst mevedel-session-control-fs--diagnostic-header "diagnostic 0"
  "Header naming the trailing record that carries target diagnostics.

An operation's header is an index and a status, both numbers, so a record
announcing itself this way cannot be mistaken for one -- and neither can
anything a tool wrote to stderr, which reaches Emacs only inside this
record's base64 payload.")

(defun mevedel-session-control-fs--take-diagnostic (records)
  "Return (DIAGNOSTIC . REMAINING) after removing the diagnostic from RECORDS."
  (let ((position (seq-position
                   records
                   mevedel-session-control-fs--diagnostic-header
                   #'equal)))
    (if (null position)
        (cons "" records)
      (let* ((payload (nth (1+ position) records))
             (text (if (and payload (not (string-empty-p payload)))
                       (condition-case nil
                           (decode-coding-string
                            (base64-decode-string payload) 'utf-8-unix)
                         (error ""))
                     "")))
        (cons (string-trim text)
              (append (seq-take records position)
                      (nthcdr (+ position 2) records)))))))

(defun mevedel-session-control-fs--program-results (operations output)
  "Return per-operation results pairing OPERATIONS with target OUTPUT.

OUTPUT carries the program's diagnostics as a trailing record; it is peeled
off here and attached to every operation that did not succeed, so a caller can
report why."
  (let* ((split (mevedel-session-control-fs--take-diagnostic
                 (split-string output "\0")))
         (diagnostic (car split))
         (records (cdr split))
         results)
    ;; Records arrive as a header and a payload per attempted operation; the
    ;; trailing element after the final separator is empty.
    (dolist (op operations)
      (let ((header (pop records))
            (payload (pop records)))
        (push
         (if (or (null header) (string-empty-p header))
             (list :op (plist-get op :op) :path (plist-get op :path)
                   :status 'skipped :value nil)
           (let* ((fields (split-string header " " t))
                  (code (string-to-number (or (nth 1 fields) "1")))
                  (status (mevedel-session-control-fs--program-status code))
                  (decoded (and (eq status 'ok)
                                (not (string-empty-p (or payload "")))
                                (base64-decode-string payload))))
             (list :op (plist-get op :op)
                   :path (plist-get op :path)
                   :status status
                   :code code
                   :value (and (eq status 'ok)
                               (mevedel-session-control-fs--program-value
                                op (or decoded "")))
                   :diagnostic (unless (eq status 'ok) (or diagnostic "")))))
         results)))
    (nreverse results)))

(defun mevedel-session-control-fs-run-program (operations)
  "Run OPERATIONS as one pinned target program and return their results.

OPERATIONS is a list of plists.  `:op' names one of
`mevedel-session-control-fs--program-verbs', `:path' is the absolute target
path it addresses, `:content' supplies bytes for a writing verb or the
expected bytes for `verify', and `:coding' selects a non-default coding
system.  `:optional' marks an operation whose failure the caller interprets
itself, such as ensuring a directory that may already exist, and which
therefore does not end the program.

Every operation opens and re-proves its own parent descriptor inside the one
process, so a program is exactly as pinned as the same operations run one at
a time.  The program stops at the first operation that does not succeed, and
its remaining operations report `skipped'; that is what lets a caller express
a compare-and-set as a `verify' followed by its writes.  Each result carries
`:status' from the shared vocabulary -- `ok', `conflict', `absent',
`mismatch', `failed', `skipped' -- so a caller reproduces the nil-versus-
signal contract of the single-operation wrappers per operation."
  (when operations
    (mevedel-session-control-fs--assert-idle
     (plist-get (car operations) :path))
    (let* ((parents
            (mapcar (lambda (op)
                      (file-remote-p
                       (plist-get
                        (mevedel-session-control-fs--descriptor
                         (plist-get op :path))
                        :parent)))
                    operations))
           (remote (car parents)))
      (dolist (other parents)
        (unless (equal other remote)
          (error "Control program crosses execution targets")))
      (let* ((default-directory
              (mevedel-session-control-fs--connection-directory
               (or remote "/")))
             (bash (car (mevedel-session-control-fs--programs remote)))
             (arguments
              (mevedel-session-control-fs--program-arguments operations))
             (input (unless arguments
                      (make-temp-file ".mevedel-control-fs-program-")))
             (output (generate-new-buffer " *mevedel-control-fs-output*")))
        (with-current-buffer output (set-buffer-multibyte nil))
        (unwind-protect
            (progn
              (when input
                (let ((coding-system-for-write 'no-conversion)
                      (request (mevedel-session-control-fs--program-request
                                operations)))
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert request)
                    (write-region (point-min) (point-max) input nil 'silent))))
              (let* ((coding-system-for-read 'no-conversion)
                     (status
                      (mevedel-transport-with-exclusive-connection
                        ;; Stderr is discarded rather than pointed at a local
                        ;; file: TRAMP would answer a local one by creating a
                        ;; remote temporary and copying it back on every
                        ;; program.  The script ships diagnostics itself, in a
                        ;; record of its own.  A bare buffer destination is
                        ;; not an option -- that leaves stderr unredirected
                        ;; into the connection buffer, which TRAMP appends to
                        ;; the output, corrupting the framing.
                        ;;
                        ;; Still exactly one target process per program: a
                        ;; request too large for the command line moves to the
                        ;; stdin file, it does not become a second call.
                        (apply
                         #'process-file
                         bash input (list output nil) nil
                         "-p" "-c"
                         mevedel-session-control-fs--program-script
                         "mevedel-session-control-fs"
                         (or mevedel-session-control-fs--test-pause-file "")
                         arguments)))
                     (text (with-current-buffer output (buffer-string))))
                (unless (and (integerp status) (zerop status))
                  ;; The resolved interpreters are the only cached input, so a
                  ;; program that failed as a whole retries their lookup.
                  (remhash (or remote "")
                           mevedel-session-control-fs--programs)
                  (signal 'file-error
                          (list "Portable control program failed"
                                (plist-get (car operations) :path)
                                ;; A program that died before its trap ran has
                                ;; no record; report what did arrive.
                                (let ((captured
                                       (car
                                        (mevedel-session-control-fs--take-diagnostic
                                         (split-string text "\0")))))
                                  (if (string-empty-p captured)
                                      (string-trim text)
                                    captured)))))
                (mevedel-session-control-fs--program-results operations text)))
          (when (and input (file-exists-p input))
            (delete-file input))
          (when (buffer-live-p output)
            (kill-buffer output)))))))

(defun mevedel-session-control-fs-program-value (result)
  "Return RESULT's decoded value, signalling the shared conditions on failure.

`conflict' and `absent' stay normal answers for the wrappers that treat them
as such, so this only raises the classifications no caller can continue past."
  (pcase (plist-get result :status)
    ('ok (plist-get result :value))
    ('conflict (signal 'mevedel-session-control-fs-conflict
                       (list (plist-get result :path))))
    ('absent (signal 'mevedel-session-control-fs-absent
                     (list (plist-get result :path))))
    (_ (signal 'file-error
               (list "Portable control operation failed"
                     (plist-get result :path)
                     (or (plist-get result :diagnostic)
                         (format "%s" (plist-get result :status))))))))

(defun mevedel-session-control-fs-physical-path (path)
  "Return the absolute control spelling PATH must resolve to on the target.

Each operation proves that spelling target-side; this only rejects a path
that could never be a control path."
  (mevedel-session-control-fs--physical-spelling path))

(defun mevedel-session-control-fs--run-1 (op path &optional content coding)
  "Run OP on PATH as a one-operation program and return its decoded value.
CONTENT and CODING are the operation's payload and coding system.  The
shared status vocabulary supplies the classification: `conflict' and
`absent' raise their conditions, everything else failed raises
`file-error' with the target's own diagnostic."
  (mevedel-session-control-fs-program-value
   (car (mevedel-session-control-fs-run-program
         (list (append (list :op op :path path)
                       (and content (list :content content))
                       (and coding (list :coding coding))))))))

(defun mevedel-session-control-fs-read-file
    (path &optional coding-system)
  "Read target control file PATH through its pinned parent directory.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (mevedel-session-control-fs--run-1 'read path nil coding-system))

(defun mevedel-session-control-fs-path-exists-p (path)
  "Return non-nil when target PATH exists as a non-symlink entry."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run-1 'path-exists-p path)
        t)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-session-control-fs-directory-p (path)
  "Return non-nil when target PATH exists as a non-symlink directory."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run-1 'directory-p path)
        t)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-session-control-fs-write-file
    (path content &optional coding-system)
  "Atomically replace target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (mevedel-session-control-fs--run-1 'write path content coding-system)
  t)

(defun mevedel-session-control-fs-create-file
    (path content &optional coding-system)
  "Exclusively create target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run-1 'create path content coding-system)
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
          (mevedel-session-control-fs--run-1 'make-directory path)
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
         (names (condition-case nil
                    (mevedel-session-control-fs--run-1
                     'list-directory directory)
                  (mevedel-session-control-fs-absent nil)))
         result)
    (dolist (name names (nreverse result))
      (when (string-match-p regexp name)
        (push (expand-file-name name directory) result)))))

(defun mevedel-session-control-fs-delete-file (path)
  "Delete target control file PATH without following a final symlink."
  (mevedel-session-control-fs--run-1 'delete-file path)
  t)

(defun mevedel-session-control-fs-delete-directory (path)
  "Recursively delete target control directory PATH without following its root."
  (mevedel-session-control-fs--run-1 'delete-directory path)
  t)

(defun mevedel-session-control-fs-target-time (directory)
  "Return target filesystem seconds from a descriptor-relative marker.

Unreadable output fails closed inside the program value decoder: a
silently substituted zero would make every stored deadline look live to
this client and every deadline it writes look expired to every other
client."
  (mevedel-session-control-fs--run-1 'target-time directory))

(provide 'mevedel-session-control-fs)

;;; mevedel-session-control-fs.el ends here
