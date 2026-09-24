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
(require 'tar-mode)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-create
                  "mevedel-execution-target" (workspace-root))
(declare-function mevedel-execution-target-direct-async-capable-p
                  "mevedel-execution-target" (target))
(autoload 'mevedel-execution-target-create "mevedel-execution-target")
(autoload 'mevedel-execution-target-direct-async-capable-p
  "mevedel-execution-target")

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
   "lock_directory=$2\n"
   "archive_reads=$3\n"
   "shift 3\n"
   "if test -n \"$lock_directory\"; then\n"
   "  exec 7<\"$lock_directory\" || exit 70\n"
   "  test \"$(cd /proc/self/fd/7 && pwd -P)\" = \"$lock_directory\" || exit 70\n"
   "  flock -x -w 20 7 || exit 79\n"
   "fi\n"
   ;; Every operation opens its own parent and re-proves it, so one process
   ;; carrying a program is exactly as pinned as one process per operation.
   ;;
   ;; Every check below states its own failure explicitly rather than leaning
   ;; on `set -e'.  The caller runs this function on the left of a `||', and
   ;; that suppresses errexit for everything the function does, so an implicit
   ;; guard would silently continue into the operation it was meant to refuse.
   "pin_parent() {\n"
   "  parent=$1\n"
   "  test -e \"$parent\" || exit 78\n"
   "  exec 9<\"$parent\" || exit 70\n"
   ;; Physical cd sets PWD from the opened directory and -e refuses an
   ;; unresolvable cwd. Avoid a pwd subshell for every individual operation.
   "  cd -P -e -- /proc/self/fd/9 || exit 70\n"
   "  test \"$PWD\" = \"$parent\" || exit 70\n"
   "  if test -n \"$pause_file\"; then\n"
   "    : >\"$pause_file\"\n"
   "    waited=0\n"
   "    while test ! -e \"$pause_file.continue\"; do\n"
   "      sleep 0.01\n"
   "      waited=$((waited + 1))\n"
   "      test \"$waited\" -lt 6000 || exit 79\n"
   "    done\n"
   "  fi\n"
   "}\n"
   ;; Read-only batches keep all proved parents open while tar transfers the
   ;; regular files. No link is dereferenced and no archive is extracted.
   "run_archive() (\n"
   "  files=()\n"
   "  while test \"$#\" -ge 5; do\n"
   "    test \"$1\" = read && test -z \"$4\" || exit 74\n"
   "    pin_parent \"$2\"\n"
   "    test ! -L \"$3\" && test -f \"$3\" || exit 69\n"
   "    exec {pin}</proc/self/fd/9 || exit 70\n"
   "    files+=(\"/proc/self/fd/$pin/$3\")\n"
   "    shift 5\n"
   "  done\n"
   "  test \"$#\" -eq 0 || exit 71\n"
   "  export TAR_OPTIONS=\n"
   "  exec tar --format=gnu --no-recursion --hard-dereference --absolute-names -cf - -- \"${files[@]}\"\n"
   ")\n"
   ;; Oversized stdin fields carry their encoded byte length.  Required
   ;; writes with a temporary destination stream exactly that many bytes;
   ;; the trailing NUL must be present before rename/link can commit them.
   ;; Optional writes and plain append decode first, preserving framing if
   ;; an optional operation fails before consuming input.
   "read_operation() {\n"
   "  IFS= read -r -d '' op || return 1\n"
   "  IFS= read -r -d '' parent || exit 71\n"
   "  IFS= read -r -d '' leaf || exit 71\n"
   "  IFS= read -r -d '' optional || exit 71\n"
   "  IFS= read -r -d '' payload_size || exit 71\n"
   "  [[ \"$payload_size\" =~ ^[0-9]+$ ]] || exit 71\n"
   "  payload_stream=0\n"
   "  payload=\n"
   "  if test \"$optional\" = 0 && test \"$payload_size\" -gt 65536 && [[ \"$op\" =~ ^(write|write-mode|append-rotating|create)$ ]]; then\n"
   "    payload_stream=$payload_size\n"
   "  else\n"
   "    IFS= read -r -N \"$payload_size\" payload || exit 71\n"
   "    IFS= read -r -d '' boundary && test -z \"$boundary\" || exit 71\n"
   "  fi\n"
   "}\n"
   "decode_payload() {\n"
   "  if test \"$payload_stream\" -gt 0; then\n"
   "    (set -o pipefail; dd bs=65536 iflag=count_bytes,fullblock count=\"$payload_stream\" status=none | base64 -d) || return 66\n"
   "    IFS= read -r -d '' boundary && test -z \"$boundary\"\n"
   "  else\n"
   "    printf '%s' \"$payload\" | base64 -d\n"
   "  fi\n"
   "}\n"
   "run_op() {\n"
   ;; The decoder owns op/parent/leaf/payload/optional for this operation.
   ;; Do not pass large payloads through shell function argument lists:
   ;; each expansion copies the field.  run_op executes in emit's subshell,
   ;; so its scratch variables cannot alter the next decoded operation.
   "  pin_parent \"$parent\"\n"
   "  case \"$op\" in\n"
   "    read)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      if test -n \"$payload\"; then\n"
   "        limit=$payload\n"
   "        [[ \"$limit\" =~ ^[0-9]+$ ]] || exit 67\n"
   "        test -f \"$leaf\" || exit 68\n"
   "        (set -o pipefail; dd if=\"$leaf\" iflag=nofollow,count_bytes,nonblock count=\"$limit\" status=none | base64 -w0) || exit 67\n"
   "      else\n"
   "        (set -o pipefail; dd if=\"$leaf\" iflag=nofollow status=none | base64 -w0) || exit 67\n"
   "      fi\n"
   "      ;;\n"
   "    verify)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      observed=$(set -o pipefail; dd if=\"$leaf\" iflag=nofollow status=none | base64 -w0) || exit 67\n"
   ;; The expected payload may arrive newline-wrapped so the command
   ;; line's physical lines stay short; the observation is unwrapped.
   "      test \"$observed\" = \"${payload//$'\\n'/}\" || exit 72\n"
   "      ;;\n"
   "    absent)\n"
   "      test ! -e \"$leaf\" && test ! -L \"$leaf\" || exit 72\n"
   "      ;;\n"
   "    verify-mode)\n"
   "      mode=$payload\n"
   "      [[ \"$mode\" =~ ^[0-7]+$ ]] || exit 67\n"
   "      test ! -L \"$leaf\" && test -f \"$leaf\" || exit 69\n"
   "      test \"$(stat -c %a -- \"$leaf\")\" = \"$mode\" || exit 72\n"
   "      ;;\n"
   "    write|write-mode)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      temporary=$(mktemp -- .mevedel-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      if test \"$op\" = write-mode; then\n"
   "        (set -o pipefail; decode_payload | {\n"
   "          IFS= read -r mode || exit 66\n"
   "          [[ \"$mode\" =~ ^[0-7]+$ ]] || exit 66\n"
   "          cat >\"$temporary\" && chmod \"$mode\" -- \"$temporary\"; }) || exit 66\n"
   "      else\n"
   "        decode_payload >\"$temporary\" || exit 66\n"
   "      fi\n"
   "      mv -fT -- \"$temporary\" \"$leaf\" || exit 67\n"
   "      trap - EXIT\n"
   "      ;;\n"
   ;; ponytail: no temp+rename, so a crash mid-append can tear one
   ;; trailing line.  Acceptable: append serves single-writer
   ;; line-oriented diagnostic streams never read at resume; upgrade to
   ;; write-to-temp + cat-merge if a consumer ever parses strictly.
   "    append-rotating)\n"
   "      test ! -L \"$leaf\" && test ! -L \"$leaf.1\" || exit 69\n"
   "      test ! -e \"$leaf\" || test -f \"$leaf\" || exit 69\n"
   "      test ! -e \"$leaf.1\" || test -f \"$leaf.1\" || exit 69\n"
   "      temporary=$(mktemp -- .mevedel-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      decode_payload >\"$temporary\" || exit 66\n"
   "      IFS= read -r limit <\"$temporary\" || exit 66\n"
   "      [[ \"$limit\" =~ ^[1-9][0-9]*$ ]] || exit 66\n"
   "      size=$(stat -c %s -- \"$temporary\") || exit 67\n"
   "      incoming=$((size - ${#limit} - 1))\n"
   "      test \"$incoming\" -le \"$limit\" || exit 67\n"
   "      size=0\n"
   "      if test -f \"$leaf\"; then size=$(stat -c %s -- \"$leaf\") || exit 67; fi\n"
   "      if test $((size + incoming)) -gt \"$limit\"; then\n"
   ;; Trim a pre-existing oversized log on first rotation, dropping the
   ;; partial first line.  Normal rotations preserve complete records.
   "        if test \"$size\" -gt \"$limit\"; then\n"
   "          (set -o pipefail; tail -c \"$limit\" -- \"$leaf\" | sed '1d' >\"$leaf.1\") || exit 67\n"
   "          : >\"$leaf\" || exit 67\n"
   "        else\n"
   "          mv -fT -- \"$leaf\" \"$leaf.1\" || exit 67\n"
   "        fi\n"
   "      fi\n"
   "      tail -n +2 -- \"$temporary\" >>\"$leaf\" || exit 67\n"
   "      rm -f -- \"$temporary\"\n"
   "      trap - EXIT\n"
   "      ;;\n"
   "    append)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      exec 8>>\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      decode_payload >&8 || exit 67\n"
   "      ;;\n"
   "    create)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      temporary=$(mktemp -- .mevedel-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      decode_payload >\"$temporary\" || exit 66\n"
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
   "      if test -d \"$leaf\"; then exit 73; fi\n"
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
   "    clock|before-time)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   "      temporary=$(mktemp -- .mevedel-control-clock-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   "      now=$(stat -c '%Y' -- \"$temporary\") || exit 67\n"
   "      if test \"$op\" = clock; then\n"
   "        printf '%s' \"$now\" | base64 -w0 || exit 67\n"
   "      else\n"
   "        deadline=$payload\n"
   "        [[ \"$deadline\" =~ ^[0-9]+$ ]] || exit 67\n"
   "        test \"$now\" -lt \"$deadline\" || exit 72\n"
   "      fi\n"
   "      ;;\n"
   "    delete-empty-directory)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      rmdir -- \"$leaf\" 2>/dev/null || exit 72\n"
   "      ;;\n"
   "    verify-latest)\n"
   "      suffix=$(decode_payload) || exit 71\n"
   "      test -n \"$suffix\" || exit 71\n"
   "      for entry in ./*\"$suffix\"; do\n"
   "        test ! -L \"$entry\" || exit 69\n"
   "        test -e \"$entry\" || continue\n"
   "        if [[ \"${entry#./}\" > \"$leaf\" ]]; then exit 72; fi\n"
   "      done\n"
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
   "    tree-size)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -d \"$leaf\" || exit 68\n"
   ;; One traversal both refuses symlinks and sums regular files: a second
   ;; `find' could run after a descendant was swapped for a link.  The
   ;; refusal leaves only the summing subshell, so `pipefail' carries it out
   ;; as the same failure as a broken traversal.
   "      (set -o pipefail\n"
   "       find -P \"./$leaf\" \\( -type l -printf 'L\\n' \\) -o \\( -type f -printf '%s\\n' \\) |\n"
   "         { total=0\n"
   "           while IFS= read -r record; do\n"
   "             test \"$record\" != L || exit 67\n"
   "             total=$((total + record))\n"
   "           done\n"
   "           printf '%s' \"$total\"; } | base64 -w0) || exit 67\n"
   "      ;;\n"
   "    *) exit 74 ;;\n"
   "  esac\n"
   "}\n"
   ;; Diagnostics are collected target-side and shipped as one trailing
   ;; record.  Handing `process-file' a local stderr file instead would make
   ;; TRAMP create a remote temporary and copy it back on every single
   ;; program, which measured as a twelfth of a remote turn.  The record is
   ;; encoded through a separate pipe so an early stop still carries it, and its
   ;; header is a word where an operation's is a number, so no operation can
   ;; be confused with it -- which is the separation that keeps a tool writing
   ;; to stderr from presenting itself as a result.
   "emit() {\n"
   "  index=$((index + 1))\n"
   "  status=0\n"
   ;; A streamed write consumes only its framed field from shared stdin.
   ;; Stream the encoded payload, then acknowledge its status.  Command
   ;; substitution would copy large manifests through a shell variable.
   "  (run_op) || status=$?\n"
   "  printf '\\0%s %s\\0' \"$index\" \"$status\"\n"
   ;; A failed operation ends the program: a caller expresses a precondition
   ;; as an earlier operation, so later ones must not run.  An
   ;; operation marked optional is one whose failure the caller expects to
   ;; interpret itself, such as ensuring a directory that already exists.
   "  if test \"$status\" -ne 0 && test \"$optional\" != 1; then\n"
   "    exit 0\n"
   "  fi\n"
   "}\n"
   ;; A request arrives as arguments when it fits and on stdin when it does
   ;; not.  Arguments cost nothing, while a stdin file costs TRAMP a remote
   ;; temporary and a copy into it on every program -- but a single argument
   ;; and the exec line have kernel size ceilings a file does not, so both
   ;; readers stay.  A field is one argument because NUL, the framing byte,
   ;; is the one byte a filename cannot contain and so cannot be embedded in
   ;; one; payload fields may carry newline-wrapped base64.
   "run_program() {\n"
   "index=0\n"
   "payload_stream=0\n"
   "if test \"$archive_reads\" = 1; then\n"
   "  if test \"$#\" -eq 0; then\n"
   "    fields=()\n"
   "    while read_operation; do fields+=(\"$op\" \"$parent\" \"$leaf\" \"$payload\" \"$optional\"); done\n"
   "    set -- \"${fields[@]}\"\n"
   "  fi\n"
   ;; Stream the encoded bytes instead of copying a multi-megabyte archive
   ;; through a shell variable. The receiver also requires successful exit.
   "  printf 'archive 0\\0'\n"
   "  archive_status=0\n"
   "  run_archive \"$@\" </dev/null | base64 -w0 || archive_status=$?\n"
   "  printf '\\0archive-status %s\\0' \"$archive_status\"\n"
   "  return 0\n"
   "fi\n"
   "if test \"$#\" -gt 0; then\n"
   "  while test \"$#\" -ge 5; do\n"
   "    op=$1; parent=$2; leaf=$3; payload=$4; optional=$5\n"
   "    emit\n"
   "    shift 5\n"
   "  done\n"
   "  test \"$#\" -eq 0 || exit 71\n"
   "else\n"
   "  while read_operation; do emit; done\n"
   "fi\n"
   "}\n"
   ;; Operation frames bypass the diagnostic pipe through descriptor 3.
   ;; Encode stderr before command substitution: shell variables cannot hold
   ;; NUL bytes.  This avoids a temporary file and its creation/removal
   ;; processes on every program, including successful read-only probes.
   "exec 3>&1\n"
   "set -o pipefail\n"
   "program_status=0\n"
   "diagnostics=$(run_program \"$@\" 2>&1 1>&3 | base64 -w0) || program_status=$?\n"
   "printf 'diagnostic 0\\0%s\\0' \"$diagnostics\"\n"
   "exit \"$program_status\"\n")
  "Target-side script running a whole program of pinned control operations.

Content payloads are base64 so one framing carries arbitrary bytes, including
NUL-separated listing names and content that a shell cannot pass through a
command substitution literally.  Numeric request fields travel as digits and
are validated by the target.  `base64' resolves through the target PATH,
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
  ;; Every remote target is readiness-gated to Linux already; a local
  ;; target never is, yet the scripts need procfs descriptor pinning and
  ;; GNU stat, mv, and base64 semantics.  Refusing here covers every
  ;; portable entry point -- lease, recovery, transfer, publication --
  ;; with one message instead of an inscrutable script exit.
  (when (and (null remote) (not (eq system-type 'gnu/linux)))
    (user-error
     (concat "Portable session storage requires a Linux host: the "
             "control filesystem needs procfs, GNU stat, and bash "
             "(this host: %s)")
     system-type))
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
    (verify-latest . "verify-latest")
    (absent . "absent")
    (before-time . "before-time")
    (write-mode . "write-mode")
    (verify-mode . "verify-mode")
    (write . "write")
    (append . "append")
    (append-rotating . "append-rotating")
    (create . "create")
    (make-directory . "mkdir")
    (path-exists-p . "probe")
    (directory-p . "directory")
    (delete-file . "delete-file")
    (delete-directory . "delete-directory")
    (delete-empty-directory . "delete-empty-directory")
    (target-time . "clock")
    (list-directory . "list")
    (tree-size . "tree-size"))
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
    ('tree-size
     (unless (string-match-p "\\`[0-9]+\\'" payload)
       (signal 'file-error
               (list "Portable control tree size is unavailable"
                     (plist-get op :path))))
     (string-to-number payload))
    (_ nil)))

(defun mevedel-session-control-fs--program-fields (op)
  "Return OP encoded as the five request fields the target script reads."
  (let* ((verb (or (cdr (assq (plist-get op :op)
                              mevedel-session-control-fs--program-verbs))
                   (error "Unknown control program operation: %S"
                          (plist-get op :op))))
         (descriptor
          (mevedel-session-control-fs--descriptor (plist-get op :path)))
         (parent (plist-get descriptor :parent))
         (content (plist-get op :content))
         ;; Wrapped base64: every 76-column line ends in a newline, so a
         ;; payload of any size never contributes an over-budget physical
         ;; line to the command TRAMP writes.  Both target readers are
         ;; newline-transparent -- decode ignores the wrapping and verify
         ;; strips it before comparing.
         (payload
          (cond
           ((plist-member op :max-bytes)
            (unless (and (eq (plist-get op :op) 'read)
                         (natnump (plist-get op :max-bytes)))
              (error "Read byte limit must be a non-negative integer"))
            (number-to-string (plist-get op :max-bytes)))
           ((null content) "")
           ((memq (plist-get op :op) '(verify-mode before-time)) content)
           ((multibyte-string-p content)
            (base64-encode-string
             (encode-coding-string
              content (or (plist-get op :coding) 'utf-8-unix))))
           (t (base64-encode-string content)))))
    ;; The parent travels once, in its physical no-trailing-slash
    ;; spelling: the script both opens it and proves it against
    ;; `pwd -P', which prints exactly that form, root included.
    (list verb
          (directory-file-name (file-local-name parent))
          (plist-get descriptor :leaf)
          payload
          (if (plist-get op :optional) "1" "0"))))

(defun mevedel-session-control-fs--insert-program-request (fields)
  "Insert FIELDS as a stdin request, with a byte length before each payload.
A trailing NUL proves a complete field before a streamed write commits.
The request is assembled in a buffer rather than as a string: a
request-sized string, and then its encoded copy, used to double every large
payload before it reached the target.  The buffer's own representation is
already the UTF-8 the target reads, raw bytes included, so it is sent
unconverted."
  (dolist (row fields)
    (let ((payload (nth 3 row)))
      (dolist (field (list (nth 0 row) (nth 1 row) (nth 2 row) (nth 4 row)
                           (number-to-string (string-bytes payload))
                           payload))
        (insert field 0)))))

(defun mevedel-session-control-fs--write-program-request (fields file)
  "Write FIELDS to FILE as a stdin request.
See `mevedel-session-control-fs--insert-program-request'."
  (with-temp-buffer
    (mevedel-session-control-fs--insert-program-request fields)
    ;; Writing unconverted streams an ASCII request, and base64 payloads
    ;; always are, without an encoded copy.
    (let ((coding-system-for-write 'no-conversion))
      (write-region nil nil file nil 'silent))))

(defconst mevedel-session-control-fs--pipe-bootstrap
  (concat "IFS= read -r -d '' script || exit 70; "
          "exec \"$0\" -p -c \"$script\" mevedel-session-control-fs \"$@\" 2>/dev/null")
  "Command line that reads the program script from stdin and runs it.

TRAMP bounds a direct-async command line by the target's PIPE_BUF, so the
script travels ahead of the request on stdin, terminated by a NUL, and this
bootstrap replaces itself with it.  `read' consumes a pipe byte by byte, so
the request that follows remains unread for the script.  Stderr is discarded
as on the request-file carrier -- the script ships its diagnostics in its own
record -- and because the wait never drains a separate stderr pipe, which a
chatty target could otherwise fill until it blocks.")

(defvar mevedel-session-control-fs--pipe-local nil
  "Test-only: carry local programs over the pipe as well.")

(defun mevedel-session-control-fs--pipe-capable-p (remote)
  "Return non-nil when REMOTE programs may stream their request over a pipe.

A direct-async process is a plain ssh channel beside the connection's shell,
so a request reaches the target without being copied through that shell as
a temporary file first -- about twenty TRAMP commands.  Targets qualify as
for direct-async Bash."
  (if remote
      (and mevedel-transport-remote-direct-async
           (fboundp 'tramp-direct-async-process-p)
           (mevedel-execution-target-direct-async-capable-p
            (mevedel-execution-target-create remote)))
    mevedel-session-control-fs--pipe-local))

(defun mevedel-session-control-fs--run-over-pipe
    (remote bash request output flags)
  "Run the program for REMOTE with BASH, streaming REQUEST's buffer on stdin.

Output goes to buffer OUTPUT and stderr is discarded, as with the request
file carrier.  FLAGS are the script's positional arguments.  Return the exit
status.  The wait accepts only this process's output and runs no timers, so
nothing can start another target operation inside it; the whole call counts
as one remote operation for `mevedel-transport-busy-p'."
  (mevedel-transport-call-as-remote-operation
   (lambda ()
     (let ((stderr (generate-new-buffer " *mevedel-control-fs-stderr*"))
           process)
       (unwind-protect
           (progn
             (setq process
                   (mevedel-transport-call-with-spawn-channel
                    remote t
                    (lambda ()
                      (make-process
                       :name "mevedel-control-fs" :buffer output
                       :stderr stderr
                       :command (append (list bash "-p" "-c"
                                              mevedel-session-control-fs--pipe-bootstrap
                                              bash)
                                        flags)
                       :connection-type 'pipe :coding 'no-conversion
                       :file-handler t :noquery t :sentinel #'ignore))))
             (process-send-string
              process
              (concat (encode-coding-string
                       mevedel-session-control-fs--program-script 'utf-8-unix)
                      "\0"))
             (with-current-buffer request
               (process-send-region process (point-min) (point-max)))
             (process-send-eof process)
             (while (or (process-live-p process)
                        (accept-process-output process 0 nil 1))
               (accept-process-output process 0.05 nil 1))
             (process-exit-status process))
         (when (process-live-p process)
           (delete-process process))
         (when-let* ((pipe (get-buffer-process stderr)))
           (delete-process pipe))
         (kill-buffer stderr))))))

(defconst mevedel-session-control-fs--argument-budget 3072
  "Largest physical line, in bytes, that the argument list may contribute.

The binding constraint is not the target's `ARG_MAX' -- that is megabytes.  It
is one physical line of the command TRAMP writes to the connection process:
that process talks over a pty, and a pty in canonical mode truncates past
`N_TTY_BUF_SIZE', 4 KiB.  Exceeding it does not fail cleanly.
`process-send-string' blocks inside the write -- timers still run while it
blocks, but nothing unwinds the write itself -- so the connection is wedged
for the life of the process.

Lines are what is budgeted, not fields: payload fields arrive as wrapped
base64, so a large payload crosses the pty as many short lines the way the
script itself does -- `tramp-send-string' preserves newlines, and a quoted
argument keeps its embedded newlines as line breaks.  Paths and verbs never
contain newlines, so any one physical line holds at most a handful of short
fields or one 76-column payload slice.  The arguments follow the script's
last line, so a kilobyte of the 4 KiB is left to it and to TRAMP's own
prefix.

The size is measured after shell quoting, which is what actually lands on the
line: a path holding spaces can nearly double.  `shell-quote-argument' stands
in for TRAMP's variant, which differs only in newline handling -- and a
newline can only help, because it ends the line being measured.")

(defconst mevedel-session-control-fs--argument-field-budget 98304
  "Largest quoted field, in bytes, that may travel as one argument.

One `execve' argument is capped by the kernel's `MAX_ARG_STRLEN', 128 KiB
on Linux, and exceeding it fails the exec outright.  The margin below it
absorbs quoting growth measured before TRAMP's own variant runs.")

(defconst mevedel-session-control-fs--argument-total-budget 524288
  "Largest quoted request, in bytes, that may travel as arguments.

`ARG_MAX' is about two megabytes less the environment; staying well under
it keeps a many-artifact program from failing the exec, and past this size
the request file's out-of-band copy is no longer the slower path.")

(defun mevedel-session-control-fs--program-arguments (fields)
  "Return FIELDS as target argument fields, or nil to use the request file.

Arguments are the cheap delivery: they ride the command line TRAMP already
sends, where a request file costs a remote temporary and a copy into it.  They
are refused when any physical line would exceed
`mevedel-session-control-fs--argument-budget', when one field or the whole
request would exceed the kernel's exec limits, and for a field carrying
bytes the command line cannot represent -- TRAMP
encodes the command line with the connection coding system, while the request
file is written without conversion, so a name outside ASCII is only
byte-transparent through the file."
  (let ((fields (apply #'append fields))
        (line 0)
        (total 0))
    (catch 'oversized
      (dolist (field fields)
        ;; Quoting cannot shrink a field.  Large payloads already require the
        ;; request file, so do not scan and quote megabytes to rediscover it.
        (when (> (string-bytes field)
                 mevedel-session-control-fs--argument-field-budget)
          (throw 'oversized nil))
        (unless (string-match-p "\\`[[:ascii:]]*\\'" field)
          (throw 'oversized nil))
        (let* ((quoted (shell-quote-argument field))
               (bytes (string-bytes quoted))
               (segments (split-string quoted "\n")))
          (when (> bytes
                   mevedel-session-control-fs--argument-field-budget)
            (throw 'oversized nil))
          (setq total (+ total bytes 1))
          (when (> total
                   mevedel-session-control-fs--argument-total-budget)
            (throw 'oversized nil))
          ;; One byte for the separator the command line will need
          ;; anyway; each embedded newline starts a fresh physical line.
          (setq line (+ line 1 (string-bytes (car segments))))
          (when (> line mevedel-session-control-fs--argument-budget)
            (throw 'oversized nil))
          (dolist (segment (cdr segments))
            (setq line (string-bytes segment))
            (when (> line mevedel-session-control-fs--argument-budget)
              (throw 'oversized nil)))))
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
  (unless (string-suffix-p "\0" output)
    (error "Incomplete control program response"))
  (let* ((split (mevedel-session-control-fs--take-diagnostic
                 (split-string output "\0")))
         (diagnostic (car split))
         (records (cdr split))
         (index 0)
         results)
    ;; Payloads stream first; only a complete trailing status authorizes
    ;; consuming their bytes.  A failed operation may have emitted a prefix.
    (dolist (op operations)
      (cl-incf index)
      (let ((payload (pop records))
            (header (pop records)))
        (push
         (if (and (null header) (or (null payload) (string-empty-p payload)))
             (list :op (plist-get op :op) :path (plist-get op :path)
                   :status 'skipped :value nil)
           (unless (and header
                        (string-match-p
                         (format "\\`%d [0-9]+\\'" index) header))
             (error "Incomplete or misordered control operation result"))
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
    (unless (or (null records) (equal records '("")))
      (error "Unexpected control operation result"))
    (nreverse results)))

(defun mevedel-session-control-fs--archive-results (operations bytes)
  "Decode a read-only archive of OPERATIONS from target BYTES.
Accept exactly one regular member per operation, in request order. Nothing
is extracted to disk. A rejected archive requires fresh ordinary reads."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((position (point-min)) results)
      (dolist (op operations)
        (let* ((header (tar-header-block-tokenize position 'utf-8-unix))
               (start (and header (tar-header-data-start header)))
               (size (and header (tar-header-size header))))
          (unless (and header (eq (plist-get op :op) 'read)
                       (null (tar-header-link-type header)) (natnump size)
                       (<= (+ start size) (point-max))
                       (string-match-p
                        (concat "\\`/proc/self/fd/[0-9]+/"
                                (regexp-quote (file-name-nondirectory (plist-get op :path))) "\\'")
                        (tar-header-name header)))
            (error "Invalid control read archive member"))
          (unless (= (tar-header-checksum header)
                     (tar-header-block-checksum
                      (buffer-substring-no-properties (- start 512) start)))
            (error "Invalid control read archive checksum"))
          (push (list :op 'read :path (plist-get op :path) :status 'ok :code 0
                      :value (mevedel-session-control-fs--program-value
                              op (buffer-substring-no-properties start (+ start size)))
                      :diagnostic nil) results)
          (setq position (tar-header-data-end header))))
      (when (tar-header-block-tokenize position 'utf-8-unix)
        (error "Unexpected control read archive member"))
      (nreverse results))))

(defun mevedel-session-control-fs-run-program (operations &optional lock-directory)
  "Run OPERATIONS as one pinned target program and return their results.

LOCK-DIRECTORY, when non-nil, holds an exclusive target-side `flock' on its
pinned directory descriptor for the entire program. Cooperating mutations must
use the same physical directory. Process death releases the lock; waiting is
bounded to 20 seconds. This requires the target's `flock' executable.

OPERATIONS is a list of plists.  `:op' names one of
`mevedel-session-control-fs--program-verbs', `:path' is the absolute target
path it addresses, `:content' supplies bytes for a writing verb or the
expected bytes for `verify', and `:coding' selects a non-default coding
system.  A `read' may carry `:max-bytes' to bound bytes on the target before
transfer or decoding.  `:optional' marks an operation whose failure the caller
interprets
itself, such as ensuring a directory that may already exist, and which
therefore does not end the program.

Every operation opens and proves its parent descriptor inside the target
process. Up to 32 independent unbounded reads may transfer together through
GNU tar while keeping those descriptors open. Unavailable tar, an unsafe
batch or an invalid archive causes a fresh ordinary read program.
No archive is extracted, and caller byte/hash validation remains unchanged.
The program stops at the first operation that does not succeed, and
its remaining operations report `skipped'; that is what lets a caller state a
precondition as a `verify' its writes depend on.  This narrows the window
between the proof and the write to two adjacent syscall sequences in one
target process, but it does not close it: nothing excludes another client
from the leaf in between, and the exclusive `create' verb is the only
atomic election primitive here.  Each result carries
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
      (when lock-directory
        (setq lock-directory (mevedel-session-control-fs-physical-path lock-directory))
        (unless (equal remote (file-remote-p lock-directory))
          (error "Control lock crosses execution targets")))
      (dolist (other parents)
        (unless (equal other remote)
          (error "Control program crosses execution targets")))
      (let* ((default-directory
              (mevedel-session-control-fs--connection-directory
               (or remote "/")))
             (bash (car (mevedel-session-control-fs--programs remote)))
             ;; Encoded once: the oversized path used to re-encode every
             ;; payload a second time for the request file.
             (fields (mapcar #'mevedel-session-control-fs--program-fields
                             operations))
             (archive-p (and (<= 2 (length operations) 32)
                             (cl-every (lambda (op)
                                         (and (eq (plist-get op :op) 'read)
                                              (not (plist-member op :max-bytes)))) operations)))
             (arguments
              (mevedel-session-control-fs--program-arguments fields))
             ;; A request too large for the command line streams over a
             ;; direct pipe where the target allows one, and otherwise
             ;; travels as an input file TRAMP copies to the target.
             (pipe (and (not arguments)
                        (mevedel-session-control-fs--pipe-capable-p remote)
                        (generate-new-buffer " *mevedel-control-fs-request*")))
             (input (unless (or arguments pipe)
                      (make-temp-file ".mevedel-control-fs-program-")))
             (flags (list (or mevedel-session-control-fs--test-pause-file "")
                          (if lock-directory (file-local-name lock-directory) "")
                          nil))
             (output (generate-new-buffer " *mevedel-control-fs-output*")))
        (with-current-buffer output (set-buffer-multibyte nil))
        (unwind-protect
            (progn
              (when pipe
                (with-current-buffer pipe
                  (mevedel-session-control-fs--insert-program-request fields)))
              (when input
                (mevedel-session-control-fs--write-program-request
                 fields input))
              (catch 'read-result
                (dotimes (_attempt 2)
                  (with-current-buffer output (erase-buffer))
                  (setcar (last flags) (if archive-p "1" "0"))
                  (let* ((coding-system-for-read 'no-conversion)
                         (status
                          (mevedel-transport-with-exclusive-connection
                           (if pipe
                               (mevedel-session-control-fs--run-over-pipe
                                remote bash pipe output flags)
                             ;; Stderr is discarded rather than pointed at a local
                             ;; file: TRAMP would answer a local one by creating a
                             ;; remote temporary and copying it back on every
                             ;; program.  The script ships diagnostics itself, in a
                             ;; record of its own.  A bare buffer destination is
                             ;; not an option -- that leaves stderr unredirected
                             ;; into the connection buffer, which TRAMP appends to
                             ;; the output, corrupting the framing.
                             ;;
                             ;; Oversized requests use stdin in the same process.
                             ;; Only a rejected archive needs an ordinary retry.
                             (apply
                              #'process-file
                              bash input (list output nil) nil
                              "-p" "-c"
                              mevedel-session-control-fs--program-script
                              "mevedel-session-control-fs"
                              (append flags arguments)))))
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
                    (when (and (not archive-p) (string-prefix-p "archive 0\0" text))
                      (error "Unexpected control read archive"))
                    (if (string-prefix-p "archive 0\0" text)
                        (condition-case nil
                            (let ((records (split-string text "\0")))
                              (unless (equal (nth 2 records) "archive-status 0")
                                (error "Control read archive transfer failed"))
                              (throw 'read-result
                                     (mevedel-session-control-fs--archive-results
                                      operations (base64-decode-string (cadr records)))))
                          (error (setq archive-p nil)))
                      (throw 'read-result
                             (mevedel-session-control-fs--program-results operations text)))))))
          (when (and input (file-exists-p input))
            (delete-file input))
          (when (buffer-live-p pipe)
            (kill-buffer pipe))
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

(defvar mevedel-session-control-fs--async-wait nil
  "Non-nil makes `mevedel-session-control-fs-run-program-async' synchronous.
Bound where the editor is about to exit, so an asynchronous program cannot
be killed with it before its result is known.")

(defun mevedel-session-control-fs-run-program-async (operations callback)
  "Run OPERATIONS as one control program, then call CALLBACK.

CALLBACK receives RESULTS, as `mevedel-session-control-fs-run-program'
returns them, and ERROR, condition data when the program failed as a whole;
exactly one is non-nil.  Where a direct pipe to the target is allowed the
program runs on its own channel and nothing waits for it: CALLBACK then runs
from a process sentinel, which can be inside another remote command, so it
must not start target I/O itself.  Elsewhere, and while
`mevedel-session-control-fs--async-wait' is non-nil, the program runs
synchronously and CALLBACK runs before this returns.  Return the process
when the program runs asynchronously, and nil otherwise.

Nothing orders an asynchronous program against other programs on the same
target: callers serialize the ones that touch the same files."
  (let ((remote (file-remote-p (plist-get (car operations) :path))))
    (if (or mevedel-session-control-fs--async-wait
            (not (mevedel-session-control-fs--pipe-capable-p remote)))
        (let (results failure)
          (condition-case err
              (setq results (mevedel-session-control-fs-run-program operations))
            (error (setq failure err)))
          (funcall callback results failure)
          nil)
      (mevedel-session-control-fs--assert-idle (or remote "/"))
      (let* ((default-directory
              (mevedel-session-control-fs--connection-directory (or remote "/")))
             (bash (car (mevedel-session-control-fs--programs remote)))
             (output (generate-new-buffer " *mevedel-control-fs-async*"))
             (stderr (generate-new-buffer " *mevedel-control-fs-async-stderr*"))
             (release (lambda ()
                        (when-let* ((pipe (get-buffer-process stderr)))
                          (delete-process pipe))
                        (dolist (buffer (list output stderr))
                          (when (buffer-live-p buffer) (kill-buffer buffer)))))
             settled process)
        (with-current-buffer output (set-buffer-multibyte nil))
        (condition-case err
            (progn
              (setq process
                    (mevedel-transport-call-with-spawn-channel
                     remote t
                     (lambda ()
                       (make-process
                        :name "mevedel-control-fs-async" :buffer output
                        :stderr stderr
                        :command (list bash "-p" "-c"
                                       mevedel-session-control-fs--pipe-bootstrap
                                       bash "" "" "0")
                        :connection-type 'pipe :coding 'no-conversion
                        :file-handler t :noquery t
                        :sentinel
                        (lambda (process _event)
                          (unless (or settled (process-live-p process))
                            (setq settled t)
                            (let ((status (process-exit-status process))
                                  (text (with-current-buffer output (buffer-string))))
                              (funcall release)
                              (if (eql 0 status)
                                  (let (results failure)
                                    (condition-case err
                                        (setq results
                                              (mevedel-session-control-fs--program-results
                                               operations text))
                                      (error (setq failure err)))
                                    (funcall callback results failure))
                                (remhash (or remote "") mevedel-session-control-fs--programs)
                                (funcall callback nil
                                         (list 'file-error
                                               "Portable control program failed"
                                               (plist-get (car operations) :path)
                                               (format "exit status %s" status)))))))))))
              (process-send-string
               process
               (concat (encode-coding-string
                        mevedel-session-control-fs--program-script 'utf-8-unix)
                       "\0"))
              (with-temp-buffer
                (mevedel-session-control-fs--insert-program-request
                 (mapcar #'mevedel-session-control-fs--program-fields operations))
                (process-send-region process (point-min) (point-max)))
              (process-send-eof process)
              process)
          (error
           (unless settled
             (setq settled t)
             (when (process-live-p process) (delete-process process))
             (funcall release)
             (funcall callback nil err))
           nil))))))

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
    (path &optional coding-system max-bytes)
  "Read target control file PATH through its pinned parent directory.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes.
MAX-BYTES, when non-nil, bounds the read on the target before transfer and
decoding.  Use `no-conversion' when a prefix may split a multibyte character."
  (mevedel-session-control-fs-program-value
   (car (mevedel-session-control-fs-run-program
         (list (append (list :op 'read :path path)
                       (and coding-system (list :coding coding-system))
                       (and max-bytes (list :max-bytes max-bytes))))))))

(defun mevedel-session-control-fs-create-or-verify (path content &optional coding-system)
  "Create target control file PATH with CONTENT, or accept an identical PATH.
Return non-nil when PATH now holds exactly CONTENT, nil when another writer
left different content.  CODING-SYSTEM defaults to UTF-8."
  (or (mevedel-session-control-fs-create-file path content coding-system)
      (equal content
             (mevedel-session-control-fs-read-file
              path coding-system (1+ (string-bytes content))))))

(defun mevedel-session-control-fs-path-exists-p (path)
  "Return non-nil when target PATH exists as a non-symlink entry."
  (condition-case nil
      (progn
        (mevedel-session-control-fs--run-1 'path-exists-p path)
        t)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-session-control-fs-paths-exist (paths)
  "Return, in PATHS order, whether each target path exists.
One program answers every path, each as for
`mevedel-session-control-fs-path-exists-p'."
  (mapcar (lambda (result)
            (pcase (plist-get result :status)
              ('ok t)
              ('absent nil)
              (_ (mevedel-session-control-fs-program-value result))))
          (mevedel-session-control-fs--optional-batch 'path-exists-p paths)))

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

(defun mevedel-session-control-fs-append-file
    (path content &optional coding-system)
  "Append CONTENT to target control file PATH, creating it when absent.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes.
Unlike `write', append works in place: a crash mid-operation can leave a
torn trailing line, which its diagnostic-stream consumers tolerate."
  (mevedel-session-control-fs--run-1 'append path content coding-system)
  t)

(defun mevedel-session-control-fs-append-rotating (path content max-bytes)
  "Append UTF-8 CONTENT to PATH, retaining one bounded PATH.1 archive.
Rotate before exceeding MAX-BYTES.  Reject an oversized single entry.
The pinned parent directory lock serializes cooperating diagnostic writers."
  (unless (and (integerp max-bytes) (> max-bytes 0))
    (error "Diagnostic byte limit must be positive"))
  (mevedel-session-control-fs-program-value
   (car (mevedel-session-control-fs-run-program
         (list (list :op 'append-rotating :path path
                     :content (concat (number-to-string max-bytes) "\n" content)))
         (directory-file-name (file-name-directory path)))))
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
its own pinned parent.  They share one program: every ancestor below the root
is an optional creation, top down, where an existing one is a conflict.
Return non-nil when PATH was created, nil when it already existed."
  (let ((path (mevedel-session-control-fs-physical-path path)))
    (condition-case nil
        (progn
          (mevedel-session-control-fs--run-1 'make-directory path)
          t)
      (mevedel-session-control-fs-conflict nil)
      (mevedel-session-control-fs-absent
       (unless parents
         (signal 'mevedel-session-control-fs-absent (list path)))
       (let ((directory path) ancestors)
         (while (let ((parent (directory-file-name
                               (file-name-directory directory))))
                  (unless (equal parent directory)
                    (push parent ancestors)
                    (setq directory parent))))
         (let* ((results
                 (mevedel-session-control-fs-run-program
                  (append (mapcar (lambda (ancestor)
                                    (list :op 'make-directory :path ancestor
                                          :optional t))
                                  (cdr ancestors))
                          (list (list :op 'make-directory :path path)))))
                (final (car (last results))))
           (pcase (plist-get final :status)
             ('ok t)
             ('conflict nil)
             ;; An ancestor that could not be created explains the failure
             ;; better than the absent parent it left behind.
             (_ (mevedel-session-control-fs-program-value
                 (or (seq-find (lambda (result)
                                 (not (memq (plist-get result :status)
                                            '(ok conflict))))
                               results)
                     final))))))))))

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

(defun mevedel-session-control-fs--optional-batch (op paths)
  "Run OP over every one of PATHS in one program, none of them required.
Each operation is independent: a failure neither stops the program nor
signals here.  Returns the per-path result plists in PATHS order, each
carrying `:path' and `:status' for the caller to classify."
  (when paths
    (mevedel-session-control-fs-run-program
     (mapcar (lambda (path) (list :op op :path path :optional t)) paths))))

(defun mevedel-session-control-fs-delete-directories (paths)
  "Recursively delete the target control directories PATHS in one program.
See `mevedel-session-control-fs--optional-batch' for the result shape."
  (mevedel-session-control-fs--optional-batch 'delete-directory paths))

(defun mevedel-session-control-fs-tree-sizes (paths)
  "Recursively size the target control directory trees PATHS in one program.
See `mevedel-session-control-fs--optional-batch' for the result shape; a
successful result also carries the tree's byte count as an integer `:value'."
  (mevedel-session-control-fs--optional-batch 'tree-size paths))

(defun mevedel-session-control-fs-target-time (directory)
  "Return target filesystem seconds from a descriptor-relative marker.

Unreadable output fails closed inside the program value decoder: a
silently substituted zero would make every stored deadline look live to
this client and every deadline it writes look expired to every other
client."
  (mevedel-session-control-fs--run-1 'target-time directory))

(provide 'mevedel-session-control-fs)

;;; mevedel-session-control-fs.el ends here
