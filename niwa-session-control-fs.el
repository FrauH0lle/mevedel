;;; niwa-session-control-fs.el -- Physical session control filesystem -*- lexical-binding: t; -*-

;;; Commentary:

;; Performs session-control filesystem operations through a target-side
;; directory descriptor.  The descriptor pins the parent directory while the
;; relative operation runs, so a pathname swap after preflight cannot redirect
;; a lease, transfer, or recovery mutation.

;;; Code:

(require 'cl-lib)
(eval-when-compile
  (require 'tramp-sh))

;; `tramp' and `tramp-sh', required on the remote paths that call these
(declare-function tramp-compute-multi-hops "tramp" (vec))
(declare-function tramp-get-connection-process "tramp" (vec))
(declare-function tramp-get-method-parameter "tramp" (vec param &optional default))
(declare-function tramp-ssh-or-plink-options "tramp-sh" (vec))

;; Every control operation both refuses to nest and runs with timers
;; suspended, and the latter is a macro, so this is a load-time dependency
;; rather than a lazily reachable one.
(require 'niwa-transport)
(require 'niwa-telemetry)
(require 'tar-mode)

;; `niwa-execution-target'
(declare-function niwa-execution-target-create
                  "niwa-execution-target" (workspace-root))
(declare-function niwa-execution-target-direct-async-capable-p
                  "niwa-execution-target" (target))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-direct-async-capable-p
  "niwa-execution-target")

;; `niwa-utilities'
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

(define-error 'niwa-session-control-fs-conflict
	      "Portable control filesystem name already exists")
(define-error 'niwa-session-control-fs-absent
	      "Portable control filesystem name does not exist")
(define-error 'niwa-session-control-fs-busy
	      "Portable control filesystem target is already in use")

(defun niwa-session-control-fs--assert-idle (path)
  "Refuse a control operation on PATH that would nest in another one.

A control operation issued while the target connection is mid-command does
not merely fail: it can consume the running command's pending output and
return an answer belonging to something else, so an absent lock reads as
present and durable state is then derived from that.  Callers reachable from
a timer, a process filter, or redisplay are expected to defer instead; this
refuses the operation outright rather than let a wrong answer through."
  (when (niwa-transport-busy-p path)
    (signal 'niwa-session-control-fs-busy (list path))))

(defvar niwa-session-control-fs--test-pause-file nil
  "Test-only target-relative pause marker, or nil in production.

The paused script gives up after a bounded wait: a test that dies before
writing the continue marker must not leave a target-side process polling
forever.")

(defvar niwa-session-control-fs--lock-budget 20
  "Seconds `niwa-session-control-fs-call-with-locks' waits for its locks.
Tests that expect a lock refusal bind a smaller value instead of waiting out
the production budget.")

(defconst niwa-session-control-fs--spelling-cache-limit 4096
  "How many canonical control spellings to remember before starting over.")

(defvar niwa-session-control-fs--spelling-cache
  (make-hash-table :test #'equal)
  "Canonical spellings keyed by (PATH . REMOTE-PREFIX-OF-`default-directory').

The computation is a pure function of those two, and the durability layer asks
for the same handful of paths thousands of times per session.  The connection
prefix belongs in the key because `expand-file-name' resolves an argument
against `default-directory' through its file-name handler.")

(defun niwa-session-control-fs--physical-spelling (path)
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
    (or (gethash key niwa-session-control-fs--spelling-cache)
        (progn
          (when (> (hash-table-count
                    niwa-session-control-fs--spelling-cache)
                   niwa-session-control-fs--spelling-cache-limit)
            (clrhash niwa-session-control-fs--spelling-cache))
          (puthash key
                   (directory-file-name (expand-file-name path))
                   niwa-session-control-fs--spelling-cache)))))

(defun niwa-session-control-fs--descriptor (path)
  "Return a target-side parent descriptor specification for PATH."
  (let* ((physical (niwa-session-control-fs--physical-spelling path))
         (parent (file-name-directory physical))
         (leaf (file-name-nondirectory physical)))
    (unless (and parent (not (string-empty-p leaf)))
      (error "Portable control parent is unavailable: %s" path))
    (list :path physical
          :parent parent
          :leaf leaf)))

(defconst niwa-session-control-fs--program-script
  (concat
   "set -eu\n"
   "take_lock() {\n"
   "  exec 7<\"$1\" || exit 70\n"
   "  test \"$(cd /proc/self/fd/7 && pwd -P)\" = \"$1\" || exit 70\n"
   "  if [[ ${3-} == report ]]; then\n"
   "    flock -xn 7 && return 0\n"
   ;; flock(1)'s PID in /proc/locks can already be gone: fd7 owns the lock.
   "    for fd in /proc/[0-9]*/fd/7; do\n"
   "      [[ $fd -ef /proc/self/fd/7 ]] || continue\n"
   "      pid=${fd#/proc/}; pid=${pid%%/*}\n"
   "      if grep -q '^lock:.*FLOCK .*WRITE' /proc/$pid/fdinfo/7 2>/dev/null; then\n"
   "        printf 'holder %s\\0' \"$pid\"; break\n"
   "      fi\n"
   "    done\n"
   "  fi\n"
   "  flock -x -w \"$2\" 7 || exit 79\n"
   "}\n"
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
   "        dd if=\"$leaf\" iflag=nofollow,count_bytes,nonblock count=\"$limit\" status=none | base64 -w0 || exit 67\n"
   "      else\n"
   "        dd if=\"$leaf\" iflag=nofollow status=none | base64 -w0 || exit 67\n"
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
   "      temporary=$(mktemp -- .niwa-control-fs-XXXXXX) || exit 66\n"
   "      trap 'rm -f -- \"$temporary\"' EXIT\n"
   ;; Authored parents may have untrusted writers.  Prove the opened inode
   ;; before reopening it for output; payload writes and chmod never follow
   ;; the mutable temporary pathname.
   "      exec 8<\"$temporary\" || exit 66\n"
   ;; Inode equality cannot prove where the descriptor was opened: a link
   ;; to an outside file can replace the temporary between open and proof.
   "      test \"$(readlink /proc/self/fd/8)\" = \"${parent%/}/$temporary\" || exit 70\n"
   "      exec 8>/proc/self/fd/8 || exit 66\n"
   "      if test \"$op\" = write-mode; then\n"
   "        (set -o pipefail; decode_payload | {\n"
   "          IFS= read -r mode || exit 66\n"
   "          [[ \"$mode\" =~ ^[0-7]+$ ]] || exit 66\n"
   "          cat >&8 && chmod \"$mode\" -- /proc/self/fd/8; }) || exit 66\n"
   "      else\n"
   "        decode_payload >&8 || exit 66\n"
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
   "      temporary=$(mktemp -- .niwa-control-fs-XXXXXX) || exit 66\n"
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
   ;; A local target receives large payloads as a staged file beside the
   ;; nearest existing ancestor instead of base64 on stdin: same filesystem,
   ;; so the rename or link into the pinned parent stays atomic.
   "    write-staged|create-staged)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   ;; The staged name is relative to the pinned parent, so a pathname
   ;; swapped after the proof cannot redirect it; `..' steps through the
   ;; opened directory's real ancestors.
   "      [[ \"$payload\" =~ ^(\\.\\./)*\\.niwa-control-fs-staged-[^/]+$ ]] || exit 71\n"
   "      test ! -L \"$payload\" && test -f \"$payload\" || exit 66\n"
   "      if test \"$op\" = write-staged; then\n"
   "        mv -fT -- \"$payload\" \"$leaf\" || exit 67\n"
   "        exit 0\n"
   "      fi\n"
   "      if test ! -d \"$leaf\" && ln -- \"$payload\" \"$leaf\"; then\n"
   "        rm -f -- \"$payload\"\n"
   "        exit 0\n"
   "      fi\n"
   "      if test -e \"$leaf\" || test -L \"$leaf\"; then\n"
   "        exit 73\n"
   "      fi\n"
   "      exit 75\n"
   "      ;;\n"
   "    create)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      temporary=$(mktemp -- .niwa-control-fs-XXXXXX) || exit 66\n"
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
   "    directory-mode)\n"
   "      [[ \"$payload\" =~ ^[0-7]+$ ]] || exit 67\n"
   "      test ! -L \"$leaf\" && test -d \"$leaf\" || exit 69\n"
   "      exec 8<\"$leaf\" || exit 70\n"
   "      test \"$(cd /proc/self/fd/8 && pwd -P)\" = \"${parent%/}/$leaf\" || exit 70\n"
   "      chmod \"$payload\" -- /proc/self/fd/8 || exit 67\n"
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
   "    observation-clock)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -d \"$leaf\" || exit 68\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   "      now=$(LC_ALL=C; printf '%s' \"${EPOCHREALTIME-}\") || exit 67\n"
   "      printf '%s' \"$now\" | base64 -w0 || exit 67\n"
   "      ;;\n"
   "    clock|before-time)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   "      temporary=$(mktemp -- .niwa-control-fs-XXXXXX) || exit 66\n"
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
   ;; Content hashes read through a descriptor proved to be the pinned leaf,
   ;; so a link swapped in after the type check cannot redirect the read.
   "    hash|verify-hash|verify-hash-snapshot)\n"
   "      if test \"$op\" = verify-hash-snapshot; then\n"
   "        expected=${payload%% *}; limit=${payload#* }\n"
   "        [[ \"$limit\" =~ ^[0-9]+$ ]] || exit 67\n"
   "        if test -z \"$expected\" && test ! -e \"$leaf\" && test ! -L \"$leaf\"; then exit 0; fi\n"
   "      fi\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      test -f \"$leaf\" || exit 68\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test \"$(readlink /proc/self/fd/8)\" = \"${parent%/}/$leaf\" || exit 70\n"
   "      sum=$(sha256sum <&8) || exit 67\n"
   "      sum=${sum%% *}\n"
   "      if test \"$op\" = hash; then\n"
   "        size=$(stat -L -c %s -- /proc/self/fd/8) || exit 67\n"
   "        printf '%s %s' \"$size\" \"$sum\" | base64 -w0 || exit 67\n"
   "      elif test \"$op\" = verify-hash; then\n"
   "        test \"$sum\" = \"$payload\" || exit 72\n"
   "      elif test \"$sum\" != \"$expected\"; then\n"
   "        size=$(stat -L -c %s -- /proc/self/fd/8) || exit 67\n"
   "        (set -o pipefail; {\n"
   "          printf '%s %s\\n' \"$size\" \"$sum\"\n"
   "          if test \"$size\" -lt \"$limit\"; then\n"
   "            dd if=/proc/self/fd/8 iflag=count_bytes count=\"$limit\" status=none || exit 67\n"
   "          fi\n"
   "        } | base64 -w0) || exit 67\n"
   "        exit 72\n"
   "      fi\n"
   "      ;;\n"
   "    verify-latest)\n"
   "      suffix=$payload\n"
   "      test -n \"$suffix\" || exit 71\n"
   "      for entry in ./*\"$suffix\"; do\n"
   "        test ! -L \"$entry\" || exit 69\n"
   "        test -e \"$entry\" || continue\n"
   "        if [[ \"${entry#./}\" > \"$leaf\" ]]; then exit 72; fi\n"
   "      done\n"
   "      ;;\n"
   ;; Proves a directory still holds exactly the entries once listed, so a
   ;; program can refuse to mutate after something was added or removed.
   ;; Both sides sort by bytes; an absent directory holds nothing.
   "    verify-list)\n"
   "      if test ! -e \"$leaf\" && test ! -L \"$leaf\"; then\n"
   "        test -z \"${payload//$'\\n'/}\" || exit 72\n"
   "        exit 0\n"
   "      fi\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      exec 8<\"$leaf\" || exit 67\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      cd -- /proc/self/fd/8 || exit 70\n"
   "      observed=$(set -o pipefail\n"
   "        for entry in ./* ./.[!.]* ./..?*; do\n"
   "          test -e \"$entry\" || test -L \"$entry\" || continue\n"
   "          printf '%s\\0' \"${entry#./}\"\n"
   "        done | LC_ALL=C sort -z | base64 -w0) || exit 67\n"
   "      test \"$observed\" = \"${payload//$'\\n'/}\" || exit 72\n"
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
   "    stat-tree)\n"
   "      test ! -L \"$leaf\" || exit 69\n"
   "      test -e \"$leaf\" || exit 77\n"
   "      test -d \"$leaf\" || exit 68\n"
   "      exec 8<\"$leaf\" || exit 70\n"
   "      cd -P -e -- /proc/self/fd/8 || exit 70\n"
   "      test \"$PWD\" = \"${parent%/}/$leaf\" || exit 70\n"
   "      export LC_ALL=C\n"
   "      (set -o pipefail; find -P . -printf '%y\\0%p\\0%D\\0%i\\0%s\\0%T@\\0%C@\\0%m\\0%U\\0%G\\0' | base64 -w0) || exit 67\n"
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
   ;; File transactions keep before-images on the locked filesystem and pinned
   ;; parent descriptors until commit.  Only successful, individually atomic
   ;; mutations enter the undo list; a failed write-mode never replaced its leaf.
   "finish_files() {\n"
   "  test -n \"$undo_directory\" || return 0\n"
   "  failed=0\n"
   "  if test \"$files_committed\" = 0; then\n"
   "    for ((i=${#undo_parents[@]}-1; i>=0; i--)); do\n"
   "      cd -- /proc/self/fd/${undo_parents[i]} || { failed=1; continue; }\n"
   "      if test \"${undo_kinds[i]}\" = mkdir; then\n"
   "        rmdir -- \"${undo_leaves[i]}\" || failed=1\n"
   "      elif test -f \"$undo_directory/${undo_indices[i]}\"; then\n"
   "        mv -fT -- \"$undo_directory/${undo_indices[i]}\" \"${undo_leaves[i]}\" || failed=1\n"
   "      else\n"
   "        rm -f -- \"${undo_leaves[i]}\" || failed=1\n"
   "      fi\n"
   "    done\n"
   "    if test \"$failed\" = 1; then\n"
   "      echo \"File batch rollback FAILED; before-images retained at $undo_directory\" >&2\n"
   "      exit 80\n"
   "    fi\n"
   "    echo 'File batch aborted; all changes rolled back' >&2\n"
   "  fi\n"
   "  rm -rf -- \"$undo_directory\"\n"
   "}\n"
   "snapshot_file() {\n"
   "  pin_parent \"$parent\"\n"
   "  test ! -L \"$leaf\" || return 69\n"
   "  exec {undo_pin}</proc/self/fd/9 || return 70\n"
   "  if test -e \"$leaf\"; then\n"
   "    if test \"$op\" = mkdir && test -d \"$leaf\"; then return 0; fi\n"
   "    test -f \"$leaf\" || return 69\n"
   "    cp -p --no-dereference --reflink=auto -- \"$leaf\" \"$undo_directory/$index\" || return 66\n"
   "    test ! -L \"$undo_directory/$index\" && test -f \"$undo_directory/$index\" || return 69\n"
   "  fi\n"
   "}\n"
   "emit() {\n"
   "  index=$((index + 1))\n"
   "  status=0\n"
   "  case \"$op\" in\n"
   "    begin-files)\n"
   "      test -n \"$lock_directory\" && test -z \"$undo_directory\" || exit 71\n"
   "      undo_directory=$(mktemp -d -- \"$lock_directory/.file-batch-XXXXXX\") || exit 66\n"
   "      trap finish_files EXIT\n"
   "      printf '\\0%s 0\\0' \"$index\"; return 0 ;;\n"
   "    commit-files) files_committed=1; printf '\\0%s 0\\0' \"$index\"; return 0 ;;\n"
   "  esac\n"
   "  if test -n \"$undo_directory\"; then\n"
   "    case \"$op\" in\n"
   "      write-mode|delete-file|mkdir) snapshot_file || status=$? ;;\n"
   "    esac\n"
   "  fi\n"
   ;; A streamed write consumes only its framed field from shared stdin.
   ;; Stream the encoded payload, then acknowledge its status.  Command
   ;; substitution would copy large manifests through a shell variable.
   "  if test \"$status\" = 0; then (trap - EXIT; run_op) || status=$?; fi\n"
   "  if test -n \"$undo_directory\" && test \"$status\" = 0; then\n"
   "    case \"$op\" in\n"
   "      write-mode|delete-file|mkdir)\n"
   "        undo_parents+=(\"$undo_pin\"); undo_leaves+=(\"$leaf\")\n"
   "        undo_kinds+=(\"$op\"); undo_indices+=(\"$index\") ;;\n"
   "    esac\n"
   "  fi\n"
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
   "undo_directory=; files_committed=0\n"
   "undo_parents=(); undo_leaves=(); undo_kinds=(); undo_indices=()\n"
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
   ;; Encode before command substitution, since shell variables drop NULs.
   ;; This preserves arbitrary target diagnostics without a temporary file.
   "control_program() {\n"
   "pause_file=$1; lock_directory=$2; archive_reads=$3; shift 3\n"
   "if [[ -n $held ]]; then\n"
   "  [[ $lock_directory == \"$held\" && $held -ef /proc/self/fd/7 ]] || exit 70\n"
   "elif [[ -n $lock_directory ]]; then take_lock \"$lock_directory\" 20; fi\n"
   "exec 3>&1\n"
   "set -o pipefail\n"
   "program_status=0\n"
   "diagnostics=$(run_program \"$@\" 2>&1 1>&3 | base64 -w0) || program_status=$?\n"
   "printf 'diagnostic 0\\0%s\\0' \"$diagnostics\"\n"
   "exit \"$program_status\"\n"
   "}\n"
   "held=\n"
   "if [[ $1 != --helper ]]; then control_program \"$@\"; fi\n"
   "held=$2\n"
   "if [[ -n $held ]]; then take_lock \"$held\" \"$3\" report; fi\n"
   "printf 'ready\\0'\n"
   "while IFS= read -r count; do\n"
   "  [[ $count == close ]] && exit 0\n"
   "  [[ $count =~ ^[0-9]+$ ]] || exit 71\n"
   "  fields=()\n"
   "  for ((i=0; i<count; i++)); do\n"
   "    IFS= read -r size || exit 71\n"
   "    [[ $size =~ ^[0-9]+$ ]] || exit 71\n"
   "    LC_ALL=C IFS= read -r -N \"$size\" field || exit 71\n"
   "    fields+=(\"$field\")\n"
   "  done\n"
   "  status=0; (control_program \"${fields[@]}\") </dev/null || status=$?\n"
   "  printf 'end %s\\0' \"$status\"\n"
   "done\n")
  "Target-side script running a whole program of pinned control operations.

Content payloads are base64 so one framing carries arbitrary bytes, including
NUL-separated listing names and content that a shell cannot pass through a
command substitution literally.  Numeric request fields travel as digits and
are validated by the target, and so does a `verify-latest' suffix, a short
file-name ending, as plain text.  `base64' resolves through the target PATH,
like `stat'.")

(defvar niwa-session-control-fs--programs (make-hash-table :test #'equal)
  "Resolved target `bash', `stat' and, remotely, `gzip' paths by TRAMP prefix.

Locating a program on a remote target costs one `test -x' per `exec-path'
entry, and the durability layer inhibits the remote file-name cache, so
resolving them per operation tripled the cost of every lease, transfer, and
recovery round trip.  A stale entry cannot mis-target an operation: the
script proves its own parent directory, and a moved interpreter fails the
operation, which drops the entry.")

(defun niwa-session-control-fs--programs (remote)
  "Return the target program paths for REMOTE.
The value is a list of `bash' and `stat' and, for a remote target only, the
optional `gzip' that `niwa-session-control-fs--script-arguments' uses."
  ;; Every remote target is readiness-gated to Linux already; a local
  ;; target never is, yet the scripts need procfs descriptor pinning and
  ;; GNU stat, mv, and base64 semantics.  Refusing here covers every
  ;; portable entry point -- lease, recovery, transfer, publication --
  ;; with one message instead of an inscrutable script exit.
  (when (and (null remote) (not (eq system-type 'gnu/linux)))
    (niwa-diagnostic-user-error 'global
     (concat "Portable session storage requires a Linux host: the "
             "control filesystem needs procfs, GNU stat, and bash "
             "(this host: %s)")
     system-type))
  (let ((key (or remote "")))
    (or (gethash key niwa-session-control-fs--programs)
        (let ((bash (executable-find "bash" remote))
              (stat (executable-find "stat" remote)))
          (unless (and bash stat)
            (error "Portable control filesystem requires bash and stat"))
          (puthash key (list bash stat
                             (and remote (executable-find "gzip" remote)))
                   niwa-session-control-fs--programs)))))

(defvar niwa-session-control-fs--compressed-script nil
  "The program script as wrapped base64 of its gzip bytes, once computed.
The symbol `none' records that local compression is unavailable.")

;; A reload installs a new program script, so discard its old encoding too.
(setq niwa-session-control-fs--compressed-script nil)

(defconst niwa-session-control-fs--compressed-bootstrap
  (concat "g=$1; s=$2; shift 2; "
          "script=$(set -o pipefail; printf %s \"$s\" | base64 -d | \"$g\" -dc) || exit 70; "
          "eval \"$script\"")
  "Command line that decompresses the program script and runs it in place.
Its first two arguments are the target `gzip' and the encoded script; the
script's own arguments follow.")

(defun niwa-session-control-fs--compressed-script ()
  "Return the encoded compressed program script, or nil when unavailable."
  (unless niwa-session-control-fs--compressed-script
    (setq niwa-session-control-fs--compressed-script
          (or (ignore-errors
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert (encode-coding-string
                           niwa-session-control-fs--program-script 'utf-8-unix))
                  (let ((default-directory temporary-file-directory))
                    (when (eql 0 (call-process-region
                                  (point-min) (point-max) "gzip" t t nil "-9nc"))
                      ;; Wrapped at 76 columns, so no physical line nears the
                      ;; pty budget `niwa-session-control-fs--argument-budget'
                      ;; describes.
                      (base64-encode-string (buffer-string))))))
              'none)))
  (and (stringp niwa-session-control-fs--compressed-script)
       niwa-session-control-fs--compressed-script))

(defun niwa-session-control-fs--script-arguments (remote programs)
  "Return the interpreter arguments that run the program script.
REMOTE is the target prefix and PROGRAMS its resolved program paths.

A remote command line crosses TRAMP's pty, where the shell-quoted script
alone is about 17 KB and pty backpressure made every program wait for it.
Where the target has `gzip', the script travels compressed at about a
fifth of that size behind a short decompressing bootstrap.  A local
command is exec'd directly and keeps the script inline, as does a target
without `gzip'."
  (let ((gzip (and remote (nth 2 programs)))
        (encoded (and remote (nth 2 programs)
                      (niwa-session-control-fs--compressed-script))))
    (if encoded
        (list "-p" "-c" niwa-session-control-fs--compressed-bootstrap
              "niwa-session-control-fs" (file-local-name gzip) encoded)
      (list "-p" "-c" niwa-session-control-fs--program-script
            "niwa-session-control-fs"))))

(defun niwa-session-control-fs--connection-directory (path)
  "Return an always-present directory on PATH's target for process dispatch.

The scripts receive their parent directory as an explicit argument, so the
working directory only selects the target.  A deleted or never-created
parent must not turn into a `Setting current directory' failure."
  (concat (or (file-remote-p path) "") "/"))

(defconst niwa-session-control-fs--program-verbs
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
    (create-staged . "create-staged")
    (write-staged . "write-staged")
    (make-directory . "mkdir")
    (directory-mode . "directory-mode")
    (path-exists-p . "probe")
    (directory-p . "directory")
    (delete-file . "delete-file")
    (delete-directory . "delete-directory")
    (delete-empty-directory . "delete-empty-directory")
    (target-time . "clock")
    (observation-time . "observation-clock")
    (list-directory . "list")
    (stat-tree . "stat-tree")
    (verify-list . "verify-list")
    (tree-size . "tree-size")
    (file-hash . "hash")
    (verify-hash . "verify-hash")
    (begin-files . "begin-files")
    (commit-files . "commit-files"))
  "Program operation names mapped to their target-side verbs.")

(defun niwa-session-control-fs--program-status (code)
  "Return the result vocabulary for target exit CODE."
  (cond
   ((eq code 0) 'ok)
   ((eq code 72) 'mismatch)
   ((eq code 73) 'conflict)
   ((memq code '(77 78)) 'absent)
   (t 'failed)))

(defun niwa-session-control-fs--program-value (op payload)
  "Decode target PAYLOAD for OP's verb into its Lisp result."
  (pcase (plist-get op :op)
    ('read (decode-coding-string payload
                                 (or (plist-get op :coding) 'utf-8-unix)))
    ((or 'list-directory 'stat-tree) (split-string payload "\0" t))
    ('observation-time
     (unless (string-match-p "\\`[0-9]+\\.[0-9]+\\'" payload)
       (signal 'file-error (list "Target observation clock is unavailable" (plist-get op :path))))
     (string-to-number payload))
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
    ((and 'verify-hash (guard (plist-member op :snapshot-bytes)))
     (unless (string-match "\\`\\([0-9]+\\) \\([0-9a-f]\\{64\\}\\)\n" payload)
       (signal 'file-error (list "Invalid control hash snapshot" (plist-get op :path))))
     (let* ((size (string-to-number (match-string 1 payload)))
            (hash (match-string 2 payload))
            (content (substring payload (match-end 0)))
            (included (< size (plist-get op :snapshot-bytes))))
       (unless (if included
                   (and (= size (length content))
                        (equal hash (secure-hash 'sha256 content)))
                 (string-empty-p content))
         (signal 'file-error (list "Inconsistent control hash snapshot" (plist-get op :path))))
       (append (list :hash hash :bytes size)
               (when included (list :content content)))))
    ('file-hash
     (unless (string-match "\\`\\([0-9]+\\) \\([0-9a-f]\\{64\\}\\)\\'" payload)
       (signal 'file-error
               (list "Portable control file hash is unavailable"
                     (plist-get op :path))))
     (list :size (string-to-number (match-string 1 payload))
           :sha256 (match-string 2 payload)))
    (_ nil)))

(defun niwa-session-control-fs--program-fields (op)
  "Return OP encoded as the five request fields the target script reads."
  (let* ((verb (or (cdr (assq (plist-get op :op)
                              niwa-session-control-fs--program-verbs))
                   (error "Unknown control program operation: %S"
                          (plist-get op :op))))
         (descriptor
          (niwa-session-control-fs--descriptor (plist-get op :path)))
         (parent (plist-get descriptor :parent))
         (content (plist-get op :content))
         ;; Wrapped base64: every 76-column line ends in a newline, so a
         ;; payload of any size never contributes an over-budget physical
         ;; line to the command TRAMP writes.  Both target readers are
         ;; newline-transparent -- decode ignores the wrapping and verify
         ;; strips it before comparing.
         (payload
          (cond
           ((and (eq (plist-get op :op) 'verify-hash)
                 (plist-member op :snapshot-bytes))
            (unless (and (natnump (plist-get op :snapshot-bytes))
                         (or (null content)
                             (and (stringp content)
                                  (string-match-p "\\`[0-9a-f]\\{64\\}\\'" content))))
              (error "Invalid expected control hash or snapshot limit"))
            (setq verb "verify-hash-snapshot")
            (format "%s %d" (or content "") (plist-get op :snapshot-bytes)))
           ((plist-member op :max-bytes)
            (unless (and (eq (plist-get op :op) 'read)
                         (natnump (plist-get op :max-bytes)))
              (error "Read byte limit must be a non-negative integer"))
            (number-to-string (plist-get op :max-bytes)))
           ((eq (plist-get op :op) 'verify-hash)
            (unless (and (stringp content) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" content))
              (error "Invalid expected control file hash"))
            content)
           ((null content) "")
           ((memq (plist-get op :op)
                  '(verify-mode directory-mode before-time verify-latest
                    write-staged create-staged))
            content)
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

(defun niwa-session-control-fs--insert-program-request (fields)
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

(defun niwa-session-control-fs--write-program-request (fields file)
  "Write FIELDS to FILE as a stdin request.
See `niwa-session-control-fs--insert-program-request'."
  (with-temp-buffer
    (niwa-session-control-fs--insert-program-request fields)
    ;; Writing unconverted streams an ASCII request, and base64 payloads
    ;; always are, without an encoded copy.
    (let ((coding-system-for-write 'no-conversion))
      (write-region nil nil file nil 'silent))))

(defconst niwa-session-control-fs--pipe-bootstrap
  (concat "IFS= read -r -d '' script || exit 70; "
          "exec \"$0\" -p -c \"$script\" niwa-session-control-fs \"$@\" 2>/dev/null")
  "Command line that reads the program script from stdin and runs it.

TRAMP bounds a direct-async command line by the target's PIPE_BUF, so the
script travels ahead of the request on stdin, terminated by a NUL, and this
bootstrap replaces itself with it.  `read' consumes a pipe byte by byte, so
the request that follows remains unread for the script.  Stderr is discarded
as on the request-file carrier -- the script ships its diagnostics in its own
record -- and because the wait never drains a separate stderr pipe, which a
chatty target could otherwise fill until it blocks.")

(defun niwa-session-control-fs--pipe-capable-p (remote)
  "Return non-nil when REMOTE programs may stream their request over a pipe.

A direct-async process is a plain ssh channel beside the connection's shell,
so a request reaches the target without being copied through that shell as
a temporary file first -- about twenty TRAMP commands.  Targets qualify as
for direct-async Bash.  A local program has no connection to bypass, so the
synchronous carrier never pipes it; the asynchronous one decides itself."
  (and remote
       niwa-transport-remote-direct-async
       (fboundp 'tramp-direct-async-process-p)
       (niwa-execution-target-direct-async-capable-p
        (niwa-execution-target-create remote))))

(defun niwa-session-control-fs--send-request-buffer (process)
  "Send the current request buffer to PROCESS in small pieces.
Large pipe writes make Emacs wait repeatedly under backpressure.  Bounded
writes preserve the existing framing and the caller's remote-operation guard."
  (let ((start (point-min))
        (limit (point-max)))
    (while (< start limit)
      (let ((end (min limit (+ start 1024))))
        (process-send-region process start end)
        (setq start end)))))

(defun niwa-session-control-fs--send-eof (process)
  "Close PROCESS's input, accepting a child that finished meanwhile."
  (when (process-live-p process)
    (condition-case err
        (process-send-eof process)
      (error
       (unless (and (not (process-live-p process))
                    (string-match-p "\\`Process .* not running:"
                                    (error-message-string err)))
         (signal (car err) (cdr err)))))))

(defun niwa-session-control-fs--run-over-pipe
    (remote bash request output flags)
  "Run the program for REMOTE with BASH, streaming REQUEST's buffer on stdin.

Output goes to buffer OUTPUT and stderr is discarded, as with the request
file carrier.  FLAGS are the script's positional arguments.  Return the exit
status.  The wait accepts only this process's output and runs no timers, so
nothing can start another target operation inside it; the whole call counts
as one remote operation for `niwa-transport-busy-p'."
  (niwa-transport-call-as-remote-operation
   (lambda ()
     (let ((stderr (generate-new-buffer " *niwa-control-fs-stderr*"))
           process)
       (unwind-protect
           (progn
             (setq process
                   (niwa-transport-call-with-spawn-channel
                    remote t
                    (lambda ()
                      (make-process
                       :name "niwa-control-fs" :buffer output
                       :stderr stderr
                       :command (append (list bash "-p" "-c"
                                              niwa-session-control-fs--pipe-bootstrap
                                              bash)
                                        flags)
                       :connection-type 'pipe :coding 'no-conversion
                       :file-handler t :noquery t :sentinel #'ignore))))
             (with-temp-buffer
               (insert (encode-coding-string
                        niwa-session-control-fs--program-script 'utf-8-unix) 0)
               (niwa-session-control-fs--send-request-buffer process))
             (with-current-buffer request
               (niwa-session-control-fs--send-request-buffer process))
             (niwa-session-control-fs--send-eof process)
             (while (or (process-live-p process)
                        (accept-process-output process 0 nil 1))
               (accept-process-output process 0.05 nil 1))
             (process-exit-status process))
         (when (process-live-p process)
           (delete-process process))
         (when-let* ((pipe (get-buffer-process stderr)))
           (delete-process pipe))
         (kill-buffer stderr))))))

(defconst niwa-session-control-fs--argument-budget 3072
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

(defconst niwa-session-control-fs--argument-field-budget 98304
  "Largest quoted field, in bytes, that may travel as one argument.

One `execve' argument is capped by the kernel's `MAX_ARG_STRLEN', 128 KiB
on Linux, and exceeding it fails the exec outright.  The margin below it
absorbs quoting growth measured before TRAMP's own variant runs.")

(defconst niwa-session-control-fs--argument-total-budget 524288
  "Largest quoted request, in bytes, that may travel as arguments.

`ARG_MAX' is about two megabytes less the environment; staying well under
it keeps a many-artifact program from failing the exec, and past this size
the request file's out-of-band copy is no longer the slower path.")

(defun niwa-session-control-fs--program-arguments (fields)
  "Return FIELDS as target argument fields, or nil to use the request file.

Arguments are the cheap delivery: they ride the command line TRAMP already
sends, where a request file costs a remote temporary and a copy into it.  They
are refused when any physical line would exceed
`niwa-session-control-fs--argument-budget', when one field or the whole
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
                 niwa-session-control-fs--argument-field-budget)
          (throw 'oversized nil))
        (unless (string-match-p "\\`[[:ascii:]]*\\'" field)
          (throw 'oversized nil))
        (let* ((quoted (shell-quote-argument field))
               (bytes (string-bytes quoted))
               (segments (split-string quoted "\n")))
          (when (> bytes
                   niwa-session-control-fs--argument-field-budget)
            (throw 'oversized nil))
          (setq total (+ total bytes 1))
          (when (> total
                   niwa-session-control-fs--argument-total-budget)
            (throw 'oversized nil))
          ;; One byte for the separator the command line will need
          ;; anyway; each embedded newline starts a fresh physical line.
          (setq line (+ line 1 (string-bytes (car segments))))
          (when (> line niwa-session-control-fs--argument-budget)
            (throw 'oversized nil))
          (dolist (segment (cdr segments))
            (setq line (string-bytes segment))
            (when (> line niwa-session-control-fs--argument-budget)
              (throw 'oversized nil)))))
      fields)))

(defconst niwa-session-control-fs--diagnostic-header "diagnostic 0"
  "Header naming the trailing record that carries target diagnostics.

An operation's header is an index and a status, both numbers, so a record
announcing itself this way cannot be mistaken for one -- and neither can
anything a tool wrote to stderr, which reaches Emacs only inside this
record's base64 payload.")

(defun niwa-session-control-fs--take-diagnostic (records)
  "Return (DIAGNOSTIC . REMAINING) after removing the diagnostic from RECORDS."
  (let ((position (seq-position
                   records
                   niwa-session-control-fs--diagnostic-header
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

(defun niwa-session-control-fs--program-results (operations output)
  "Return per-operation results pairing OPERATIONS with target OUTPUT.

OUTPUT carries the program's diagnostics as a trailing record; it is peeled
off here and attached to every operation that did not succeed, so a caller can
report why."
  (unless (string-suffix-p "\0" output)
    (error "Incomplete control program response"))
  (let* ((split (niwa-session-control-fs--take-diagnostic
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
                  (status (niwa-session-control-fs--program-status code))
                  (decoded (and (eq status 'ok)
                                (not (string-empty-p (or payload "")))
                                (base64-decode-string payload))))
             (append
              (list :op (plist-get op :op)
                    :path (plist-get op :path)
                    :status status
                    :code code
                    :value (and (eq status 'ok)
                                (not (eq (plist-get op :op) 'verify-hash))
                                (niwa-session-control-fs--program-value
                                 op (or decoded "")))
                    :diagnostic (unless (eq status 'ok) (or diagnostic "")))
              (when (and (eq status 'mismatch)
                         (eq (plist-get op :op) 'verify-hash)
                         (plist-member op :snapshot-bytes))
                (list :observed (niwa-session-control-fs--program-value
                                 op (base64-decode-string payload)))))))
         results)))
    (unless (or (null records) (equal records '("")))
      (error "Unexpected control operation result"))
    (nreverse results)))

(defun niwa-session-control-fs--archive-results (operations bytes)
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
                      :value (niwa-session-control-fs--program-value
                              op (buffer-substring-no-properties start (+ start size)))
                      :diagnostic nil) results)
          (setq position (tar-header-data-end header))))
      (when (tar-header-block-tokenize position 'utf-8-unix)
        (error "Unexpected control read archive member"))
      (nreverse results))))

(defconst niwa-session-control-fs-scratch-name-regexp
  "\\`\\.niwa-control-fs-\\(?:staged-\\)?[A-Za-z0-9]\\{6\\}\\'"
  "Reserved scratch leaves from `make-temp-file' and the shell's mktemp.
Interrupted carriers can leave these non-authoritative payloads behind.")

(defconst niwa-session-control-fs--stage-min-bytes 0
  "Smallest local payload written as a staged file instead of base64.
Every local write is staged: the script's base64 path spends three
processes on a write and four on a create, a staged one or two.")

(defvar niwa-session-control-fs--stage-local t
  "Non-nil stages large local payloads; tests bind nil to exercise carriers.")

(defun niwa-session-control-fs--inline-source (op)
  "Return OP with a `:source-file' payload read into raw `:content'.
Remote targets and small payloads travel as base64 fields."
  (if-let* ((source (plist-get op :source-file)))
      (let ((copy (copy-sequence op)))
        (cl-remf copy :source-file)
        (plist-put (plist-put copy :content
                              (with-temp-buffer
                                (set-buffer-multibyte nil)
                                (insert-file-contents-literally source)
                                (buffer-string)))
                   :coding 'no-conversion))
    op))

(defun niwa-session-control-fs--stage-payloads (operations staged)
  "Return local OPERATIONS with large writes carried by staged files.

Base64 encoding a payload into the request and decoding it on the target
cost a 260 KB sidecar write 31 ms, most of a save.  A local target shares
the editor's filesystem, so the bytes are written once to a private file
in the destination's nearest existing ancestor -- a directory the same
program creates lives on that same filesystem -- and the program renames
or links it after proving the destination's parent as before.  The
program names it relative to that pinned parent, never by pathname.  Record each
staged file in the car of STAGED for cleanup.  A payload that cannot be
staged -- an unwritable destination directory -- travels inline instead, so
the target refuses the operation itself and the program reports it: a local
staging error must not end operations that preceded it, such as a lease
commit the write rides behind."
  (mapcar
   (lambda (op)
     (let ((content (plist-get op :content))
           (source (plist-get op :source-file)))
       (if (not (and (memq (plist-get op :op) '(write create))
                     (if source
                         (>= (file-attribute-size (file-attributes source))
                             niwa-session-control-fs--stage-min-bytes)
                       (and (stringp content)
                            (>= (string-bytes content)
                                niwa-session-control-fs--stage-min-bytes)))))
           (niwa-session-control-fs--inline-source op)
         (condition-case nil
             (let* ((parent (plist-get (niwa-session-control-fs--descriptor
                                        (plist-get op :path))
                                       :parent))
                    (directory parent)
                    (up ""))
               (while (not (file-directory-p directory))
                 (setq directory (file-name-directory (directory-file-name directory))
                       up (concat up "../")))
               (let ((file (make-temp-file
                            (file-name-concat directory ".niwa-control-fs-staged-")))
                     (coding-system-for-write
                      (if (multibyte-string-p content)
                          (or (plist-get op :coding) 'utf-8-unix)
                        'no-conversion)))
                 (push file (car staged))
                 (if source
                     ;; A file-backed payload is copied by the kernel; its bytes
                     ;; never become a Lisp string.
                     (copy-file source file t)
                   (write-region content nil file nil 'silent))
                 (list :op (if (eq (plist-get op :op) 'write) 'write-staged 'create-staged)
                       :path (plist-get op :path)
                       :content (concat up (file-name-nondirectory file))
                       :optional (plist-get op :optional))))
           (file-error (niwa-session-control-fs--inline-source op))))))
   operations))

(defvar niwa-session-control-fs--measure-min-ms 20
  "Shortest synchronous program, in milliseconds, recorded as telemetry.
Pause attribution needs only programs long enough to be seen; recording
every five-second transfer poll would drown them.")

(defvar niwa-session-control-fs--helper-enabled nil
  "Non-nil inside a synchronous workflow that owns a reusable carrier.")
(defvar niwa-session-control-fs--local-helper nil
  "Process serving successive local programs, without retaining locks.")
(defvar niwa-session-control-fs--lock-holder nil
  "Target PID observed holding the lock during this startup only.")

(defmacro niwa-session-control-fs-with-reuse (&rest body)
  "Run foreground BODY with a local carrier, closing it on every exit."
  (declare (indent 0) (debug t))
  `(progn
     (when inhibit-quit (user-error "Cannot wait for a memory decision in this callback"))
     (let ((niwa-session-control-fs--helper-enabled t)
           (niwa-session-control-fs--local-helper nil))
       (unwind-protect (progn ,@body)
         (niwa-session-control-fs-lock-session-close niwa-session-control-fs--local-helper)))))

(defun niwa-session-control-fs--lock-timeout (lock)
  "Refuse acquisition of LOCK without signalling any potential holder."
  (signal 'file-error
          (list (concat "Control lock wait expired"
                        (when niwa-session-control-fs--lock-holder
                          (format "; observed holder PID %s on %s. A crashed editor may have left it; verify it still holds this lock and no editor uses it before stopping it"
                                  niwa-session-control-fs--lock-holder
                                  (or (file-remote-p lock 'host 'never) (system-name))))) lock)))

(defun niwa-session-control-fs-lock-session-close (process)
  "Release PROCESS and its private output buffer; repeated calls are safe."
  (when (processp process)
    (when (process-live-p process)
      (when (and (process-get process 'ready) (not (process-get process 'busy)))
        ;; Classic container ttys need an explicit close before disconnect.
        (ignore-errors (process-send-string process "close\n")
                       (accept-process-output process .1 nil 1)))
      (when (process-live-p process) (delete-process process)))
    (when (buffer-live-p (process-buffer process)) (kill-buffer (process-buffer process)))))

(defun niwa-session-control-fs--helper-wait (process regexp &optional deadline)
  "Wait for REGEXP at the tail of PROCESS output, bounded by DEADLINE."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (while (not (save-excursion
                  (forward-char (- (min 20 (- (point) (point-min)))))
                  (re-search-forward regexp nil t)))
      (when (and deadline (not niwa-session-control-fs--lock-holder)
                 (save-excursion (goto-char (point-min)) (looking-at "holder \\([0-9]+\\)\0")))
        (setq niwa-session-control-fs--lock-holder (match-string 1)))
      (unless (process-live-p process)
        (when (= (process-exit-status process) 79)
          (niwa-session-control-fs--lock-timeout (process-get process 'lock)))
        (signal 'file-error (list "Control helper died; refusing to continue" (process-get process 'lock))))
      (let ((remaining (and deadline (- deadline (float-time)))))
        (when (and remaining (<= remaining 0))
          (niwa-session-control-fs--lock-timeout (process-get process 'lock)))
        (accept-process-output process remaining nil 1))
      (goto-char (point-max)))
    (goto-char (match-beginning 0))))

(defun niwa-session-control-fs--helper-open (remote lock deadline)
  "Start the helper on REMOTE and acquire LOCK by absolute time DEADLINE."
  (let* ((default-directory (niwa-session-control-fs--connection-directory (or remote "/")))
         (bash (file-local-name (car (niwa-session-control-fs--programs remote))))
         (script (encode-coding-string niwa-session-control-fs--program-script 'utf-8-unix))
         (vec (and remote (tramp-dissect-file-name default-directory)))
         (container-shell (and vec (member (tramp-file-name-method vec) '("podman" "docker"))
                               (null (cdr (tramp-compute-multi-hops vec)))
                               (tramp-get-method-parameter vec 'tramp-direct-async)))
         (buffer (generate-new-buffer " *niwa-control-helper*")) process ready)
    (with-current-buffer buffer (set-buffer-multibyte nil))
    (unwind-protect
        (progn
          (setq process
                (niwa-transport-call-with-spawn-channel
                 remote container-shell
                 (lambda ()
                   ;; conmon keeps -it ttys open after client death.  Preserve
                   ;; the configured method's remaining arguments verbatim.
                   (let ((tramp-methods
                          (if container-shell
                              (cons (cl-subst "-i" "-it" (assoc (tramp-file-name-method vec) tramp-methods) :test #'equal)
                                    tramp-methods)
                            (and (boundp 'tramp-methods) tramp-methods))))
                   (make-process
                    :name "niwa-control-helper" :buffer buffer :file-handler t
                    :coding 'no-conversion :connection-type 'pipe :noquery t :sentinel #'ignore
                    :command (list bash "-p" "-c"
                                   ;; Classic TRAMP supplies a tty even for a pipe process.
                                   ;; Bash's fixed-length reads need a real pipe, not tty mode changes.
                                   (concat "exec 2>/dev/null; if test -t 0; then stty raw -echo || exit 71; "
                                           "exec 0< <(cat); fi; printf 'input\\0'; "
                                           "LC_ALL=C IFS= read -r -N \"$1\" s && "
                                           "IFS= read -r -d '' lock || exit 71; "
                                           "set -- --helper \"$lock\" \"$2\"; eval \"$s\"")
                                   "control-helper" (number-to-string (length script))
                                   (format "%.6f" (max 0 (- deadline (float-time))))))))))
          (process-put process 'lock lock)
          (process-put process 'remote remote)
          (niwa-session-control-fs--helper-wait process "\\`input\0\\'" deadline)
          (with-current-buffer buffer (erase-buffer))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert script (encode-coding-string (if lock (file-local-name lock) "") 'utf-8-unix) 0)
            (niwa-session-control-fs--send-request-buffer process))
          (niwa-session-control-fs--helper-wait process "ready\0\\'" deadline)
          (process-put process 'ready t)
          (setq ready t)
          process)
      (unless ready
        (niwa-session-control-fs-lock-session-close process)
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun niwa-session-control-fs--acquire (directory deadline)
  "Acquire DIRECTORY on its execution host by the enclosing absolute DEADLINE."
  (niwa-session-control-fs--assert-idle directory)
  (setq directory (niwa-session-control-fs-physical-path directory))
  (niwa-transport-call-as-remote-operation
   (lambda () (niwa-session-control-fs--helper-open (file-remote-p directory) directory deadline))))

(defun niwa-session-control-fs-lock-session-open (directory &optional wait)
  "Hold an exclusive lock on DIRECTORY on its host for successive calls.
WAIT bounds lock acquisition in seconds, at most twenty (the default)."
  (unless (<= 0 (or wait 20) 20) (error "Invalid control lock wait"))
  (let ((deadline (+ (float-time) (or wait 20)))
        (niwa-session-control-fs--lock-holder nil) session)
    (niwa-transport-call-with-deadline
     (or wait 20)
     (lambda () (setq session (niwa-session-control-fs--acquire directory deadline)))
     (lambda ()
       ;; A ready helper returned after the deadline must not retain the lock.
       (niwa-session-control-fs-lock-session-close session)
       (niwa-session-control-fs--lock-timeout directory)))))

(defun niwa-session-control-fs-call-with-locks (directories function)
  "Create and lock DIRECTORIES in order, then call FUNCTION with their sessions.
One budget (`niwa-session-control-fs--lock-budget', twenty seconds) covers all
startup, never the work of FUNCTION.
Release acquired locks in reverse order on every exit."
  ;; The section spans acquisition and release: the first lock is held while
  ;; later ones start, and closing a helper runs kill hooks.
  (niwa-transport-call-in-section
   (lambda ()
     (let ((deadline (+ (float-time) niwa-session-control-fs--lock-budget))
           (niwa-session-control-fs--lock-holder nil) current sessions)
       (unwind-protect
           (progn
             (niwa-transport-call-with-deadline
              niwa-session-control-fs--lock-budget
              (lambda ()
                (dolist (directory directories)
                  (setq current directory niwa-session-control-fs--lock-holder nil)
                  (niwa-session-control-fs-make-directory directory t)
                  (push (niwa-session-control-fs--acquire directory deadline) sessions)))
              (lambda () (niwa-session-control-fs--lock-timeout current)))
             (apply function (reverse sessions)))
         (mapc #'niwa-session-control-fs-lock-session-close sessions))))))

(defun niwa-session-control-fs-lock-session-run (session operations)
  "Run OPERATIONS only on SESSION, under its original directory lock."
  (unless (and (processp session) (process-live-p session) (process-get session 'lock))
    (signal 'file-error '("Control lock owner is gone; refusing to continue")))
  (niwa-session-control-fs-run-program operations session))

(defun niwa-session-control-fs--helper-request (process fields output)
  "Send FIELDS to PROCESS and collect its ordinary program reply in OUTPUT."
  (when (process-get process 'busy) (error "Control helper already in use"))
  (process-put process 'busy t)
  (let (done)
    (unwind-protect
        (progn
          (with-current-buffer (process-buffer process) (erase-buffer))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert (number-to-string (length fields)) "\n")
            (dolist (field fields)
              (when (string-search "\0" field) (error "NUL in control argument"))
              (let ((bytes (if (multibyte-string-p field) (encode-coding-string field 'utf-8-unix) field)))
                (insert (number-to-string (length bytes)) "\n" bytes)))
            (niwa-session-control-fs--send-request-buffer process))
          (niwa-session-control-fs--helper-wait process "end \\([0-9]+\\)\0\\'")
          (with-current-buffer (process-buffer process)
            (let ((status (string-to-number (match-string 1))))
              (append-to-buffer output (point-min) (point))
              (setq done t)
              status)))
      (process-put process 'busy nil)
      (unless done (niwa-session-control-fs-lock-session-close process)))))

(defun niwa-session-control-fs--pooled-helper ()
  "Return the idle local helper with exactly the current spawn environment."
  (when inhibit-quit (user-error "Cannot wait for a reusable control helper in this callback"))
  (unless (and niwa-session-control-fs--local-helper
               (process-live-p niwa-session-control-fs--local-helper)
               (= (default-file-modes) (process-get niwa-session-control-fs--local-helper 'modes))
               (eq niwa-session-control-fs--program-script
                   (process-get niwa-session-control-fs--local-helper 'script))
               (equal process-environment (process-get niwa-session-control-fs--local-helper 'environment)))
    (niwa-session-control-fs-lock-session-close niwa-session-control-fs--local-helper)
    (setq niwa-session-control-fs--local-helper
          (niwa-session-control-fs--helper-open nil nil (+ (float-time) 20)))
    (process-put niwa-session-control-fs--local-helper 'script niwa-session-control-fs--program-script)
    (process-put niwa-session-control-fs--local-helper 'modes (default-file-modes))
    (process-put niwa-session-control-fs--local-helper 'environment (copy-sequence process-environment)))
  niwa-session-control-fs--local-helper)

(defun niwa-session-control-fs-run-program (operations &optional lock-directory)
  "Run OPERATIONS as one pinned target program and return their results.

LOCK-DIRECTORY, when non-nil, holds an exclusive target-side `flock' on its
pinned directory descriptor for the entire program. Cooperating mutations must
use the same physical directory. A live lock session instead of a directory
sends this request to that exact owner without affecting reentrant calls.
Process death releases the lock; waiting is
bounded to 20 seconds. This requires the target's `flock' executable.

OPERATIONS is a list of plists.  `:op' names one of
`niwa-session-control-fs--program-verbs', `:path' is the absolute target
path it addresses, `:content' supplies bytes for a writing verb or the
expected bytes for `verify', and `:coding' selects a non-default coding
system.  A `read' may carry `:max-bytes' to bound bytes on the target before
transfer or decoding.  `:optional' marks an operation whose failure the caller
interprets
itself, such as ensuring a directory that may already exist, and which
therefore does not end the program.

A `verify-hash' with `:snapshot-bytes' also accepts nil `:content' to
require absence.  On mismatch its result carries `:observed', the checked
file's `:hash' and `:bytes', plus unibyte `:content' when strictly smaller
than that limit.  These are captured before the program releases its lock;
an absent file reports `absent' with no observation.  Other failed operations
never expose a partial payload as a value.

Every operation opens and proves its parent descriptor inside the target
process. Up to 32 independent unbounded reads may transfer together through
GNU tar while keeping those descriptors open. Unavailable tar, an unsafe
batch or an invalid archive causes a fresh ordinary read program.
No archive is extracted, and caller byte/hash validation remains unchanged.
The program stops at the first operation that does not succeed, and
its remaining operations report `skipped'; that is what lets a caller state a
precondition as a `verify' its writes depend on.  Without LOCK-DIRECTORY,
this narrows the window between proof and write but does not close it:
another client can change the leaf in between.  With LOCK-DIRECTORY,
cooperating callers serialize the entire program under the same target lock.
With a lock, `begin-files' and `commit-files' bracket a file transaction:
`write-mode', `delete-file' and newly made directories are snapshotted and
undone in reverse order if the program ends before commit.  Before-images
preserve file bytes, modes and timestamps.  Rollback failure signals an
error and retains the before-images in the lock directory for recovery.
This handles operation errors, not uncatchable process death or power loss.
Each result carries
`:status' from the shared vocabulary -- `ok', `conflict', `absent',
`mismatch', `failed', `skipped' -- so a caller reproduces the nil-versus-
signal contract of the single-operation wrappers per operation."
  ;; A synchronous program blocks the editor for its whole round trip,
  ;; so a long one is worth attributing to a pause.
  (niwa-telemetry-measure
   nil 'control-program niwa-session-control-fs--measure-min-ms
   (lambda () (niwa-session-control-fs--run-program operations lock-directory))
   :kind (plist-get (car operations) :op)
   :operation-count (length operations)))

(defun niwa-session-control-fs--run-program (operations &optional lock-directory)
  "Run OPERATIONS as `niwa-session-control-fs-run-program' documents.
LOCK-DIRECTORY is its optional target-side lock."
  (when operations
    (niwa-session-control-fs--assert-idle
     (plist-get (car operations) :path))
    (let* ((owner (and (processp lock-directory) lock-directory))
           (lock-directory (if owner (or (process-get owner 'lock) (error "Not a control lock owner")) lock-directory))
           (parents
            (mapcar (lambda (op)
                      (file-remote-p
                       (plist-get
                        (niwa-session-control-fs--descriptor
                         (plist-get op :path))
                        :parent)))
                    operations))
           (remote (car parents)))
      (when lock-directory
        (setq lock-directory (niwa-session-control-fs-physical-path lock-directory))
        (unless (equal remote (file-remote-p lock-directory))
          (error "Control lock crosses execution targets")))
      (dolist (other parents)
        (unless (equal other remote)
          (error "Control program crosses execution targets")))
      (let* ((default-directory
              (niwa-session-control-fs--connection-directory
               (or remote "/")))
             (programs (niwa-session-control-fs--programs remote))
             (bash (car programs))
             (staged (list nil))
             ;; Encoded once: the oversized path used to re-encode every
             ;; payload a second time for the request file.
             (fields (mapcar #'niwa-session-control-fs--program-fields
                             (if (or remote (not niwa-session-control-fs--stage-local))
                                 (mapcar #'niwa-session-control-fs--inline-source
                                         operations)
                               (niwa-session-control-fs--stage-payloads
                                operations staged))))
             (archive-p (and (<= 2 (length operations) 32)
                             (cl-every (lambda (op)
                                         (and (eq (plist-get op :op) 'read)
                                              (not (plist-member op :max-bytes)))) operations)))
             (arguments
              (niwa-session-control-fs--program-arguments fields))
             ;; A request too large for the command line streams over a
             ;; direct pipe where the target allows one, and otherwise
             ;; travels as an input file TRAMP copies to the target.
             (helper (or owner
                         (and niwa-session-control-fs--helper-enabled
                              (not inhibit-quit) (not remote) arguments
                              (not (and niwa-session-control-fs--local-helper
                                        (process-get niwa-session-control-fs--local-helper 'busy)))
                              (niwa-session-control-fs--pooled-helper))))
             (pipe (and (not helper) (not arguments)
                        (niwa-session-control-fs--pipe-capable-p remote)
                        (generate-new-buffer " *niwa-control-fs-request*")))
             (input (unless (or helper arguments pipe)
                      (make-temp-file ".niwa-control-fs-program-")))
             (flags (list (or niwa-session-control-fs--test-pause-file "")
                          (if lock-directory (file-local-name lock-directory) "")
                          nil))
             (output (generate-new-buffer " *niwa-control-fs-output*")))
        (with-current-buffer output (set-buffer-multibyte nil))
        (unwind-protect
            (progn
              (when pipe
                (with-current-buffer pipe
                  (niwa-session-control-fs--insert-program-request fields)))
              (when input
                (niwa-session-control-fs--write-program-request
                 fields input))
              (catch 'read-result
                (dotimes (_attempt 2)
                  (with-current-buffer output (erase-buffer))
                  (setcar (last flags) (if archive-p "1" "0"))
                  (let* ((coding-system-for-read 'no-conversion)
                         (status
                          (niwa-transport-with-exclusive-connection
                           (cond
                            (helper
                             (niwa-transport-call-as-remote-operation
                              (lambda ()
                                (niwa-session-control-fs--helper-request
                                 helper (append flags (or arguments (apply #'append fields))) output))))
                            (pipe
                               (niwa-session-control-fs--run-over-pipe
                                remote bash pipe output flags))
                            (t
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
                              (append
                               (niwa-session-control-fs--script-arguments
                                remote programs)
                               flags arguments))))))
                         (text (with-current-buffer output (buffer-string))))
                    (unless (and (integerp status) (zerop status))
                      ;; The resolved interpreters are the only cached input, so a
                      ;; program that failed as a whole retries their lookup.
                      (remhash (or remote "")
                               niwa-session-control-fs--programs)
                      (signal 'file-error
                              (list "Portable control program failed"
                                    (plist-get (car operations) :path)
                                    ;; A program that died before its trap ran has
                                    ;; no record; report what did arrive.
                                    (let ((captured
                                           (car
                                            (niwa-session-control-fs--take-diagnostic
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
                                     (niwa-session-control-fs--archive-results
                                      operations (base64-decode-string (cadr records)))))
                          (error (setq archive-p nil)))
                      (throw 'read-result
                             (niwa-session-control-fs--program-results operations text)))))))
          (when (and input (file-exists-p input))
            (delete-file input))
          ;; A program that stopped early leaves its later payloads staged.
          (dolist (file (car staged))
            (when (file-exists-p file) (delete-file file)))
          (when (buffer-live-p pipe)
            (kill-buffer pipe))
          (when (buffer-live-p output)
            (kill-buffer output)))))))

(defun niwa-session-control-fs-program-value (result)
  "Return RESULT's decoded value, signalling the shared conditions on failure.

`conflict' and `absent' stay normal answers for the wrappers that treat them
as such, so this only raises the classifications no caller can continue past."
  (pcase (plist-get result :status)
    ('ok (plist-get result :value))
    ('conflict (signal 'niwa-session-control-fs-conflict
                       (list (plist-get result :path))))
    ('absent (signal 'niwa-session-control-fs-absent
                     (list (plist-get result :path))))
    (_ (signal 'file-error
               (list "Portable control operation failed"
                     (plist-get result :path)
                     (or (plist-get result :diagnostic)
                         (format "%s" (plist-get result :status))))))))

(defvar niwa-session-control-fs--async-wait nil
  "Non-nil makes `niwa-session-control-fs-run-program-async' synchronous.
Bound where the editor is about to exit, so an asynchronous program cannot
be killed with it before its result is known.")

(defvar niwa-session-control-fs--async-jobs nil
  "Asynchronous programs whose callback has not yet run.
Each entry is a unique cell, removed when its callback is about to run.")

(defun niwa-session-control-fs-wait-async (&optional seconds)
  "Wait up to SECONDS (default 10) until every asynchronous program has delivered.
Where the editor is about to exit, this lets programs already running finish
and their callbacks run, which `niwa-session-control-fs--async-wait'
cannot do for programs launched earlier.  Return non-nil when none remain."
  (let ((deadline (+ (float-time) (or seconds 10))))
    (while (and niwa-session-control-fs--async-jobs
                (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (null niwa-session-control-fs--async-jobs)))

(defvar niwa-session-control-fs--orphaned-staged nil
  "Staged (FILE . SCOPE) payloads whose deletion failed, retried at exit.")

(defun niwa-session-control-fs--exit-cleanup ()
  "Remove staged payload files of asynchronous programs that never delivered.
Emacs exit can come before a program's delivery timer runs, or after a
cancelled one was lost; the staged files sit beside target files and must not
outlive the editor.  Files an earlier cleanup could not delete are retried.
Each failure is reported and does not stop the rest."
  (let ((files (apply #'append niwa-session-control-fs--orphaned-staged
                      (mapcar (lambda (job)
                                (mapcar (lambda (file) (cons file (cadr job)))
                                        (car (car job))))
                              niwa-session-control-fs--async-jobs))))
    (setq niwa-session-control-fs--orphaned-staged nil
          niwa-session-control-fs--async-jobs nil)
    (dolist (entry files)
      (condition-case err
          (when (file-exists-p (car entry)) (delete-file (car entry)))
        ((error quit)
         (niwa-diagnostic-warning (or (cdr entry) 'unavailable)
          'niwa
          (format "Staged control payload cleanup failed: %s"
                  (error-message-string err))
          :warning))))))

(add-hook 'kill-emacs-hook #'niwa-session-control-fs--exit-cleanup 90)

(defun niwa-session-control-fs--connected-ssh-command (remote)
  "Return a local SSH argv reusing REMOTE's live master, or nil.
Only direct SSH targets with TRAMP-managed connection sharing qualify.
ProxyCommand=false makes a vanished master fail instead of reconnecting."
  ;; REMOTE must include an absolute localname.  Even `file-remote-p' on
  ;; a bare TRAMP prefix expands the remote home with a target command.
  (when (and remote (file-remote-p remote nil t)
             niwa-transport-remote-direct-async
             (member (file-remote-p remote 'method) '("ssh" "scp"))
             (not (file-remote-p remote 'hop)))
    (require 'tramp-sh)
    (when (eq tramp-use-connection-share t)
      (let* ((vec (tramp-dissect-file-name remote))
             (options (tramp-ssh-or-plink-options vec))
             (temporary (tramp-get-connection-property
                         (tramp-get-connection-process vec) "temp-file")))
        (when (and (stringp temporary)
                   (string-match-p "ControlPath=[^ ]+" options)
                   (not (string-match-p "ControlPath=none\\(?: \\|$\\)" options)))
          (append
           (list (tramp-get-method-parameter vec 'tramp-login-program)
                 "-o" "ControlMaster=no" "-o" "ProxyCommand=false"
                 "-o" "ProxyJump=none" "-o" "BatchMode=yes")
           (split-string-and-unquote (format-spec options `((?t . ,temporary))))
           (when-let* ((user (tramp-file-name-user vec))) (list "-l" user))
           (when-let* ((port (tramp-file-name-port vec))) (list "-p" port))
           (list (tramp-file-name-host vec))))))))

(defun niwa-session-control-fs-async-busy-p (directory)
  "Return non-nil while a local asynchronous program addresses DIRECTORY.
Include completed programs awaiting callback delivery: their continuations may
still mutate storage after the target process has exited.  No target I/O runs."
  (let* ((directory (niwa-session-control-fs-physical-path directory))
         (prefix (file-name-as-directory directory)))
    (seq-some (lambda (job)
                (seq-some (lambda (path)
                            (or (equal directory path) (string-prefix-p prefix path)))
                          (nth 2 job)))
              niwa-session-control-fs--async-jobs)))

(cl-defun niwa-session-control-fs-run-program-async
    (operations callback &optional scope lock-directory connected-only)
  "Run OPERATIONS as one control program, then call CALLBACK.

CALLBACK receives RESULTS, as `niwa-session-control-fs-run-program'
returns them, and ERROR, condition data when the program failed as a whole;
exactly one is non-nil, and it is called exactly once.  A local program, and
a remote one where a direct pipe to the target is allowed, runs as its own
process and nothing waits for it.  Local writes keep the synchronous
carrier's staged payload files, which live until the process has exited and
are removed just before CALLBACK, from the delivery timer and never the
sentinel; Emacs exit removes those of programs that never delivered.

The process sentinel only decodes and stores the outcome.  CALLBACK then runs
through `niwa-transport-run-when-idle' with a zero delay: always from a
timer after the sentinel has returned, once no remote operation is in flight,
so it may start target I/O.  A disabled transport or cancelled retry still
delivers the stored outcome from a zero-second transport timer rather than
losing or inventing one.  A request that fails to launch calls CALLBACK
before this returns.

Elsewhere, and while `niwa-session-control-fs--async-wait' is non-nil, the
program runs synchronously and CALLBACK runs before this returns.  Return the
process when the program runs asynchronously, and nil otherwise.
`niwa-session-control-fs-wait-async' waits for programs in flight.

SCOPE is the captured diagnostic root, or `unavailable' for ownerless work.
LOCK-DIRECTORY serializes the whole program with synchronous and asynchronous
programs using that same physical directory, exactly as in the synchronous API.
Without it callers must serialize programs touching the same files.
CONNECTED-ONLY requires a live remote SSH master; never reconnect or use a
synchronous fallback.  An unavailable carrier delivers a `file-error'.
It carries bounded requests entirely in argv, so a stalled master cannot
block Emacs writing the program to a pipe.  Oversized requests fail closed."
  (let* ((remote (file-remote-p (plist-get (car operations) :path)))
         (ssh (and connected-only
                   (not niwa-session-control-fs--async-wait)
                   (ignore-errors
                     (niwa-session-control-fs--connected-ssh-command
                      (plist-get (car operations) :path))))))
    (when lock-directory
      (setq lock-directory (niwa-session-control-fs-physical-path lock-directory))
      (unless (equal remote (file-remote-p lock-directory))
        (error "Control lock crosses execution targets")))
    (when (and connected-only (not ssh))
      (funcall callback nil '(file-error "Connected-only SSH carrier unavailable"))
      (cl-return-from niwa-session-control-fs-run-program-async nil))
    (if (or niwa-session-control-fs--async-wait
            (null operations)
            (and (not ssh) remote
                 (not (niwa-session-control-fs--pipe-capable-p remote))))
        (let (results failure)
          (condition-case err
              (setq results (niwa-session-control-fs-run-program operations lock-directory))
            (error (setq failure err)))
          (funcall callback results failure)
          nil)
      ;; A connected-only child never touches TRAMP's shared shell.
      (unless ssh
        (niwa-session-control-fs--assert-idle
         (or (plist-get (car operations) :path) "/")))
      (let* ((default-directory
              (if ssh temporary-file-directory
                (niwa-session-control-fs--connection-directory (or remote "/"))))
             (bash (if ssh "bash" (car (niwa-session-control-fs--programs remote))))
             (path (plist-get (car operations) :path))
             (output (generate-new-buffer " *niwa-control-fs-async*"))
             (stderr (generate-new-buffer " *niwa-control-fs-async-stderr*"))
             (staged (list nil))
             (job (list staged scope
                        (mapcar (lambda (op)
                                  (niwa-session-control-fs-physical-path
                                   (plist-get op :path))) operations)))
             ;; The transport queue coalesces work under `equal' keys, and
             ;; JOB is structurally equal across programs that stage
             ;; nothing: an uninterned symbol keeps each delivery its own.
             (key (list 'niwa-session-control-fs-async (make-symbol "job")))
             (release (lambda ()
                        ;; A program that stopped early leaves its later
                        ;; payloads staged; remove them first, as the buffers'
                        ;; kill hooks are arbitrary code that may fail.  A file
                        ;; that cannot be deleted is remembered for exit
                        ;; cleanup, and the first failure is signalled after
                        ;; every step ran.
                        (let (failure)
                          (dolist (file (car staged))
                            (condition-case err
                                (when (file-exists-p file) (delete-file file))
                              ((error quit)
                               (push (cons file scope) niwa-session-control-fs--orphaned-staged)
                               (setq failure (or failure err)))))
                          ;; Every private resource is released on its own:
                          ;; one failing or quitting kill hook must not leave
                          ;; the others.
                          (condition-case err
                              (when-let* ((pipe (get-buffer-process stderr)))
                                (delete-process pipe))
                            ((error quit) (setq failure (or failure err))))
                          ;; The buffers are private: another module's kill
                          ;; query must not veto them, and a veto (a nil
                          ;; return) counts as failure like a signal does.
                          (dolist (buffer (list output stderr))
                            (when (buffer-live-p buffer)
                              (condition-case err
                                  (let ((kill-buffer-query-functions nil))
                                    (kill-buffer buffer))
                                ((error quit) (setq failure (or failure err))))
                              (when (buffer-live-p buffer)
                                (setq failure
                                      (or failure
                                          (list 'error "Control buffer was not killed")))
                                (condition-case nil
                                    (progn
                                      (with-current-buffer buffer
                                        (setq-local kill-buffer-hook nil))
                                      (let ((kill-buffer-hook nil)
                                            (kill-buffer-query-functions nil))
                                        (kill-buffer buffer)))
                                  ((error quit) nil)))))
                          (when failure (signal (car failure) (cdr failure))))))
             (deliver (lambda (results failure)
                        (setq niwa-session-control-fs--async-jobs
                              (delq job niwa-session-control-fs--async-jobs))
                        (funcall callback results failure)))
             (complete (lambda (results failure)
                         (let* ((done nil)
                                (thunk (lambda ()
                                         (unless done
                                           (setq done t)
                                          ;; Cleanup runs here, never in the
                                          ;; sentinel: killing buffers runs
                                          ;; arbitrary hooks and deleting staged
                                          ;; files is file I/O.  A failure is
                                          ;; reported and must not lose the
                                          ;; stored outcome.
                                          (condition-case cleanup
                                              (funcall release)
                                            ((error quit)
                                             (niwa-diagnostic-warning (or scope 'unavailable)
                                              'niwa
                                              (format "Control program cleanup failed: %s"
                                                      (error-message-string cleanup))
                                              :warning)))
                                           (funcall deliver results failure)))))
                           ;; Never run the callback in the sentinel, which may
                           ;; sit inside another remote command and whose
                           ;; callback may start the next target phase.  A
                           ;; disabled transport or a cancelled retry still
                           ;; delivers the stored outcome, from a timer.
                           (let ((later (lambda ()
                                          (niwa-transport-run-at-time
                                           0 thunk))))
                             (unless (niwa-transport-run-when-idle
                                      key path thunk later 0)
                               (funcall later))))))
             settled process)
        (with-current-buffer output (set-buffer-multibyte nil))
        (push job niwa-session-control-fs--async-jobs)
        (condition-case err
            (let* ((_ (dolist (op operations)
                        (unless (equal remote
                                       (file-remote-p
                                        (plist-get
                                         (niwa-session-control-fs--descriptor
                                          (plist-get op :path))
                                         :parent)))
                          (error "Control program crosses execution targets"))))
                   (fields (mapcar #'niwa-session-control-fs--program-fields
                                   (if (or remote
                                           (not niwa-session-control-fs--stage-local))
                                       (mapcar #'niwa-session-control-fs--inline-source
                                               operations)
                                     (niwa-session-control-fs--stage-payloads
                                      operations staged))))
                   ;; Connected-only and small local requests ride as
                   ;; arguments.  Only ordinary carriers may stream stdin.
                   (arguments (and (or ssh (not remote))
                                   (niwa-session-control-fs--program-arguments
                                    fields))))
              (when (and ssh (not arguments))
                (error "Connected-only request exceeds argument limits"))
              ;; Writing the request can run process filters, as it does in the
              ;; synchronous carrier: counting the dispatch as a remote operation
              ;; makes whatever they start defer instead of nesting in it.
              (niwa-transport-call-as-remote-operation
               (lambda ()
                 (setq process
                       (niwa-transport-call-with-spawn-channel
                        (unless ssh remote) t
                        (lambda ()
                          (make-process
                           :name "niwa-control-fs-async" :buffer output
                           :stderr stderr
                           :command
                           (cond
                            (ssh
                             (append ssh
                                     (list
                                      (mapconcat
                                       #'shell-quote-argument
                                       (append
                                        (list bash "-p" "-c"
                                              niwa-session-control-fs--program-script
                                              "niwa-session-control-fs"
                                              (or niwa-session-control-fs--test-pause-file "")
                                              (if lock-directory (file-local-name lock-directory) "") "0")
                                        arguments) " "))))
                            (arguments
                               (append
                                (list bash "-p" "-c"
                                      niwa-session-control-fs--program-script
                                      "niwa-session-control-fs"
                                      (or niwa-session-control-fs--test-pause-file "")
                                      (if lock-directory (file-local-name lock-directory) "") "0")
                                arguments))
                            (t (list bash "-p" "-c"
                                   niwa-session-control-fs--pipe-bootstrap
                                   bash (or niwa-session-control-fs--test-pause-file "")
                                   (if lock-directory (file-local-name lock-directory) "") "0")))
                           :connection-type 'pipe :coding 'no-conversion
                           :file-handler (not ssh) :noquery t
                           :sentinel
                           (lambda (process _event)
                             (unless (or settled (process-live-p process))
                               (setq settled t)
                               (let ((status (process-exit-status process))
                                     (text (with-current-buffer output
                                             (buffer-string))))
                                 (if (eql 0 status)
                                     (let (results failure)
                                       (condition-case err
                                           (setq results
                                                 (niwa-session-control-fs--program-results
                                                  operations text))
                                         (error (setq failure err)))
                                       (funcall complete results failure))
                                   (remhash (or remote "")
                                            niwa-session-control-fs--programs)
                                   (funcall complete nil
                                            (list 'file-error
                                                  "Portable control program failed"
                                                  path
                                                  (format "exit status %s" status)))))))))))
                 (unless arguments
                   (with-temp-buffer
                     (insert (encode-coding-string
                              niwa-session-control-fs--program-script 'utf-8-unix)
                             0)
                     (niwa-session-control-fs--send-request-buffer process))
                   (with-temp-buffer
                     (niwa-session-control-fs--insert-program-request fields)
                     (niwa-session-control-fs--send-request-buffer process))
                   (niwa-session-control-fs--send-eof process))
                 process)))
          ((error quit)
           (unless settled
             (setq settled t)
             (when (process-live-p process) (delete-process process))
             (condition-case cleanup
                 (funcall release)
               ((error quit)
                (niwa-diagnostic-warning (or scope 'unavailable)
                 'niwa
                 (format "Control program cleanup failed: %s"
                         (error-message-string cleanup))
                 :warning)))
             ;; A program that dies before its request is written failed as a
             ;; whole, just as the sentinel reports an unsuccessful exit.
             (remhash (or remote "") niwa-session-control-fs--programs)
             (funcall deliver nil
                      (if (eq (car err) 'file-error)
                          err
                        (list 'file-error "Portable control program failed"
                              path (error-message-string err)))))
           (when (eq (car err) 'quit) (signal (car err) (cdr err)))
           nil))))))

(defun niwa-session-control-fs-physical-path (path)
  "Return the absolute control spelling PATH must resolve to on the target.

Each operation proves that spelling target-side; this only rejects a path
that could never be a control path."
  (niwa-session-control-fs--physical-spelling path))

(defun niwa-session-control-fs--run-1 (op path &optional content coding)
  "Run OP on PATH as a one-operation program and return its decoded value.
CONTENT and CODING are the operation's payload and coding system.  The
shared status vocabulary supplies the classification: `conflict' and
`absent' raise their conditions, everything else failed raises
`file-error' with the target's own diagnostic."
  (niwa-session-control-fs-program-value
   (car (niwa-session-control-fs-run-program
         (list (append (list :op op :path path)
                       (and content (list :content content))
                       (and coding (list :coding coding))))))))

(defun niwa-session-control-fs-read-file
    (path &optional coding-system max-bytes)
  "Read target control file PATH through its pinned parent directory.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes.
MAX-BYTES, when non-nil, bounds the read on the target before transfer and
decoding.  Use `no-conversion' when a prefix may split a multibyte character."
  (niwa-session-control-fs-program-value
   (car (niwa-session-control-fs-run-program
         (list (append (list :op 'read :path path)
                       (and coding-system (list :coding coding-system))
                       (and max-bytes (list :max-bytes max-bytes))))))))

(defun niwa-session-control-fs-create-or-verify (path content &optional coding-system)
  "Create target control file PATH with CONTENT, or accept an identical PATH.
Return non-nil when PATH now holds exactly CONTENT, nil when another writer
left different content.  CODING-SYSTEM defaults to UTF-8."
  (or (niwa-session-control-fs-create-file path content coding-system)
      (equal content
             (niwa-session-control-fs-read-file
              path coding-system (1+ (string-bytes content))))))

(defun niwa-session-control-fs-path-exists-p (path)
  "Return non-nil when target PATH exists as a non-symlink entry."
  (condition-case nil
      (progn
        (niwa-session-control-fs--run-1 'path-exists-p path)
        t)
    (niwa-session-control-fs-absent nil)))

(defun niwa-session-control-fs-paths-exist (paths)
  "Return, in PATHS order, whether each target path exists.
One program answers every path, each as for
`niwa-session-control-fs-path-exists-p'."
  (mapcar (lambda (result)
            (pcase (plist-get result :status)
              ('ok t)
              ('absent nil)
              (_ (niwa-session-control-fs-program-value result))))
          (niwa-session-control-fs--optional-batch 'path-exists-p paths)))

(defun niwa-session-control-fs-directory-p (path)
  "Return non-nil when target PATH exists as a non-symlink directory."
  (condition-case nil
      (progn
        (niwa-session-control-fs--run-1 'directory-p path)
        t)
    (niwa-session-control-fs-absent nil)))

(defun niwa-session-control-fs-write-file
    (path content &optional coding-system)
  "Atomically replace target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (niwa-session-control-fs--run-1 'write path content coding-system)
  t)

(defun niwa-session-control-fs-append-file
    (path content &optional coding-system)
  "Append CONTENT to target control file PATH, creating it when absent.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes.
Unlike `write', append works in place: a crash mid-operation can leave a
torn trailing line, which its diagnostic-stream consumers tolerate."
  (niwa-session-control-fs--run-1 'append path content coding-system)
  t)

(defun niwa-session-control-fs-append-rotating (path content max-bytes)
  "Append UTF-8 CONTENT to PATH, retaining one bounded PATH.1 archive.
Rotate before exceeding MAX-BYTES.  Reject an oversized single entry.
The pinned parent directory lock serializes cooperating diagnostic writers."
  (unless (and (integerp max-bytes) (> max-bytes 0))
    (error "Diagnostic byte limit must be positive"))
  (niwa-session-control-fs-program-value
   (car (niwa-session-control-fs-run-program
         (list (list :op 'append-rotating :path path
                     :content (concat (number-to-string max-bytes) "\n" content)))
         (directory-file-name (file-name-directory path)))))
  t)

(defun niwa-session-control-fs-create-file
    (path content &optional coding-system)
  "Exclusively create target control file PATH with CONTENT.
CODING-SYSTEM defaults to UTF-8; use `no-conversion' for arbitrary bytes."
  (condition-case nil
      (progn
        (niwa-session-control-fs--run-1 'create path content coding-system)
        t)
    (niwa-session-control-fs-conflict nil)))

(defun niwa-session-control-fs-make-directory (path &optional parents)
  "Create target control directory PATH, optionally including PARENTS.

A pinned operation can only create a name inside a directory it already
opened, so missing parents are created one component at a time, each through
its own pinned parent.  They share one program: every ancestor below the root
is an optional creation, top down, where an existing one is a conflict.
Return non-nil when PATH was created, nil when it already existed."
  (let ((path (niwa-session-control-fs-physical-path path)))
    (condition-case nil
        (progn
          (niwa-session-control-fs--run-1 'make-directory path)
          t)
      (niwa-session-control-fs-conflict nil)
      (niwa-session-control-fs-absent
       (unless parents
         (signal 'niwa-session-control-fs-absent (list path)))
       (let ((directory path) ancestors)
         (while (let ((parent (directory-file-name
                               (file-name-directory directory))))
                  (unless (equal parent directory)
                    (push parent ancestors)
                    (setq directory parent))))
         (let* ((results
                 (niwa-session-control-fs-run-program
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
             (_ (niwa-session-control-fs-program-value
                 (or (seq-find (lambda (result)
                                 (not (memq (plist-get result :status)
                                            '(ok conflict))))
                               results)
                     final))))))))))

(defun niwa-session-control-fs-list-directory (directory regexp)
  "Return physical paths in DIRECTORY matching REGEXP.

An absent DIRECTORY lists nothing, so callers need no separate existence
round trip.  The directory descriptor is pinned while names are enumerated.
Symlink entries fail closed before their names can be handed to a caller."
  (let* ((directory (niwa-session-control-fs-physical-path directory))
         (names (condition-case nil
                    (niwa-session-control-fs--run-1
                     'list-directory directory)
                  (niwa-session-control-fs-absent nil)))
         result)
    (dolist (name names (nreverse result))
      (when (string-match-p regexp name)
        (push (expand-file-name name directory) result)))))

(defun niwa-session-control-fs-listing-proof (directory paths)
  "Return an operation proving DIRECTORY still holds exactly PATHS.
PATHS are every entry `niwa-session-control-fs-list-directory' returned
for DIRECTORY.  The operation fails as `mismatch' once an entry was added or
removed, so a later program can make an unchanged listing its precondition."
  (list :op 'verify-list :path (niwa-session-control-fs-physical-path directory)
        :content (mapconcat (lambda (name) (concat name "\0"))
                            (sort (mapcar #'file-name-nondirectory paths) #'string<)
                            "")))

(defun niwa-session-control-fs-delete-file (path)
  "Delete target control file PATH without following a final symlink."
  (niwa-session-control-fs--run-1 'delete-file path)
  t)

(defun niwa-session-control-fs-delete-directory (path)
  "Recursively delete target control directory PATH without following its root."
  (niwa-session-control-fs--run-1 'delete-directory path)
  t)

(defun niwa-session-control-fs--optional-batch (op paths)
  "Run OP over every one of PATHS in one program, none of them required.
Each operation is independent: a failure neither stops the program nor
signals here.  Returns the per-path result plists in PATHS order, each
carrying `:path' and `:status' for the caller to classify."
  (when paths
    (niwa-session-control-fs-run-program
     (mapcar (lambda (path) (list :op op :path path :optional t)) paths))))

(defun niwa-session-control-fs-delete-directories (paths)
  "Recursively delete the target control directories PATHS in one program.
See `niwa-session-control-fs--optional-batch' for the result shape."
  (niwa-session-control-fs--optional-batch 'delete-directory paths))

(defun niwa-session-control-fs-tree-sizes (paths)
  "Recursively size the target control directory trees PATHS in one program.
See `niwa-session-control-fs--optional-batch' for the result shape; a
successful result also carries the tree's byte count as an integer `:value'."
  (niwa-session-control-fs--optional-batch 'tree-size paths))

(defun niwa-session-control-fs-file-hash (path)
  "Return regular file PATH's (:size BYTES :sha256 HEX) observed on its target.
The bytes are hashed there and never transferred.  A missing PATH signals
`niwa-session-control-fs-absent'; links and other types are file errors."
  (niwa-session-control-fs--run-1 'file-hash path))

(defun niwa-session-control-fs-target-time (directory)
  "Return target filesystem seconds from a descriptor-relative marker.

Unreadable output fails closed inside the program value decoder: a
silently substituted zero would make every stored deadline look live to
this client and every deadline it writes look expired to every other
client."
  (niwa-session-control-fs--run-1 'target-time directory))

(defun niwa-session-control-fs-observation-time (directory)
  "Return target host seconds without writing inside DIRECTORY.
This read-only clock is for age observations, not lease ownership.  The
directory and its parent are descriptor-proved by the native program;
missing or linked paths and invalid clock output fail closed."
  (niwa-session-control-fs--run-1 'observation-time directory))

(provide 'niwa-session-control-fs)

;;; niwa-session-control-fs.el ends here
