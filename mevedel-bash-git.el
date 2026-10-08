;;; mevedel-bash-git.el --- Git inspection policy and hardened argv -*- lexical-binding: t -*-

;;; Commentary:

;; Argument-aware inspection policy for Git 2.56.  Classification alone does
;; not authorize an ordinary Git launch: callers must execute the argv returned
;; by `mevedel-bash-git-prepare', and must fail closed if preparation fails.
;; Shell parsing, quoting, resource permissions and confinement belong to the
;; caller.  This module neither parses shell text nor grants filesystem access.
;;
;; Keep normal repository, user and system config, overriding only unsafe
;; inspection behavior.  Built-in command names cannot be replaced by aliases.
;; Signature-bearing formats are excluded because they invoke verification
;; even when log.showSignature is false.  Batch cat-file modes and interactive
;; stdin modes are deliberately outside this bounded argument interface.
;; Worktree status/diff have a runtime guard: active clean/process filters or
;; gitlinks are refused before inspection.  Unused driver definitions are safe.
;; A private temporary metadata view freezes effective config and the index.
;; Resolved includes and filter commands are absent from its launch config, so
;; a concurrent source config/attribute edit cannot enable new helpers after
;; the guard.  Objects/refs remain read-through; inspection is not an atomic
;; repository snapshot.  Worktree files and attributes remain live: concurrent
;; edits may change reported comparisons, but cannot enable filter/submodule
;; helpers in the final process.  Temporary metadata is removed on ordinary
;; exit and caught signals; an uncatchable kill can leave a private temp view.
;;
;; The privileged Bash launcher removes every inherited GIT_* environment
;; entry at the actual execution target, then installs only fixed inspection
;; controls.  GNU env's NUL-delimited enumeration also covers names that Bash
;; cannot import as shell variables.  This is not a defense against an untrusted
;; Git/env executable, PATH, dynamic loader or Git bug;
;; ordinary child confinement and trusted executable resolution still apply.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst mevedel-bash-git--diff-flags
  '("-p" "--patch" "-s" "--no-patch" "-u" "--raw" "--patch-with-raw"
    "--patch-with-stat" "--numstat" "--shortstat" "--stat" "--summary"
    "--compact-summary" "--name-only" "--name-status" "--check" "--binary"
    "--full-index" "--no-color" "--color" "--no-ext-diff" "--no-textconv"
    "--no-renames" "--exit-code" "--quiet" "-z" "--relative" "--text" "-a"
    "--ignore-space-at-eol" "--ignore-cr-at-eol" "--ignore-space-change"
    "--ignore-all-space" "--ignore-blank-lines" "-b" "-w" "--minimal"
    "--patience" "--histogram" "--no-prefix" "--default-prefix"
    "-M" "--find-renames" "-C" "--find-copies")
  "Exact diff display options which do not take values.")

(defconst mevedel-bash-git--history-flags
  '("--oneline" "--graph" "--all" "--branches" "--tags" "--remotes"
    "--first-parent" "--no-merges" "--merges" "--reverse" "--topo-order"
    "--date-order" "--author-date-order" "--boundary" "--root"
    "--no-walk" "--do-walk" "--decorate" "--no-decorate"
    "--no-show-signature" "--use-mailmap" "--no-use-mailmap"
    "--notes" "--no-notes" "--follow" "--full-history" "--simplify-merges"
    "--dense" "--sparse" "--left-right" "--cherry-pick" "--cherry-mark"
    "--ancestry-path" "--abbrev-commit" "--no-abbrev-commit"
    "--regexp-ignore-case" "-i" "--extended-regexp" "-E" "--fixed-strings"
    "-F" "--all-match" "--invert-grep")
  "Exact history traversal/display options which do not take values.")

(defconst mevedel-bash-git--flags
  '(("status" "--short" "-s" "--branch" "-b" "--show-stash" "--porcelain"
     "--long" "-z" "--null" "--no-renames" "--renames" "--no-ahead-behind"
     "--ahead-behind" "--verbose" "-v" "--ignored" "-u"
     "-uno" "-unormal" "-uall" "--untracked-files")
    ("diff" "--cached" "--staged")
    ("log") ("show")
    ("rev-parse" "--verify" "--quiet" "-q" "--short" "--sq" "--not"
     "--revs-only" "--no-revs" "--flags" "--no-flags" "--symbolic"
     "--symbolic-full-name" "--abbrev-ref" "--all" "--branches" "--tags"
     "--remotes" "--show-toplevel" "--show-prefix" "--show-cdup" "--git-dir"
     "--absolute-git-dir" "--git-common-dir" "--is-inside-git-dir"
     "--is-inside-work-tree" "--is-bare-repository" "--is-shallow-repository"
     "--show-object-format" "--show-ref-format" "--local-env-vars"
     "--end-of-options")
    ("ls-files" "-z" "-t" "-v" "-f" "-c" "--cached" "-d" "--deleted"
     "-o" "--others" "-i" "--ignored" "-s" "--stage" "-u" "--unmerged"
     "-k" "--killed" "-m" "--modified" "--resolve-undo" "--directory"
     "--no-empty-directory" "--eol" "--deduplicate" "--exclude-standard"
     "--error-unmatch" "--full-name" "--abbrev" "--debug" "--sparse")
    ("ls-tree" "-d" "-r" "-t" "-l" "-z" "--name-only" "--name-status"
     "--object-only" "--full-name" "--full-tree" "--abbrev")
    ("cat-file" "-e" "-p" "-t" "-s")
    ("merge-base" "-a" "--all" "--octopus" "--is-ancestor" "--independent"
     "--fork-point")
    ("for-each-ref" "--ignore-case" "--omit-empty" "--include-root-refs")
    ("branch" "--list" "--show-current" "-a" "--all" "-r" "--remotes"
     "-v" "-vv" "--verbose" "--no-color" "--color" "--no-column"
     "--no-abbrev" "--ignore-case" "--omit-empty"))
  "Per-command exact flag whitelist; no abbreviations or short clusters.")

(defconst mevedel-bash-git--values
  '(("status" ("--porcelain" enum "1" "2" "v1" "v2")
     ("--untracked-files" enum "no" "normal" "all")
     ("--ignored" enum "traditional" "matching" "no")
     ("--ignore-submodules" enum "none" "untracked" "dirty" "all"))
    ("diff")
    ("log" ("-n" number) ("--max-count" number) ("--skip" number)
     ("--since" text) ("--after" text) ("--until" text) ("--before" text)
     ("--author" text) ("--committer" text) ("--grep" text)
     ("--format" pretty) ("--pretty" pretty) ("--date" text)
     ("--decorate" enum "short" "full" "auto" "no"))
    ("show" ("--format" pretty) ("--pretty" pretty) ("--date" text))
    ("rev-parse" ("--short" number) ("--abbrev-ref" enum "strict" "loose")
     ("--path-format" enum "absolute" "relative") ("--git-path" operand)
     ("--resolve-git-dir" operand) ("--disambiguate" hex)
     ("--show-object-format" enum "storage" "input" "output"))
    ("ls-files" ("-x" text) ("--exclude" text) ("--with-tree" operand)
     ("--abbrev" number) ("--format" format))
    ("ls-tree" ("--abbrev" number) ("--format" format))
    ("cat-file") ("merge-base")
    ("for-each-ref" ("--count" number) ("--sort" sort) ("--format" format)
     ("--points-at" operand) ("--merged" operand) ("--no-merged" operand)
     ("--contains" operand) ("--no-contains" operand) ("--exclude" text))
    ("branch" ("--format" format) ("--sort" sort) ("--abbrev" number)
     ("--color" enum "always" "auto" "never") ("--points-at" operand)
     ("--merged" operand) ("--no-merged" operand)
     ("--contains" operand) ("--no-contains" operand)))
  "Options with mandatory values, either separate or joined by equals.")

(defconst mevedel-bash-git--diff-values
  '(("-U" number) ("--unified" number) ("--inter-hunk-context" number)
    ("--abbrev" number) ("--stat-width" number) ("--stat-name-width" number)
    ("--stat-count" number) ("--color" enum "always" "auto" "never")
    ("--diff-algorithm" enum "myers" "minimal" "patience" "histogram")
    ("--ignore-submodules" enum "none" "untracked" "dirty" "all")
    ("--submodule" enum "short" "log") ("--relative" text)
    ("--src-prefix" text) ("--dst-prefix" text) ("--line-prefix" text)
    ("-S" text) ("-G" text))
  "Safe valued options shared by diff, log and show.")

(defconst mevedel-bash-git--environment-guard
  "mapfile -d '' -t inherited < <(env -0) || exit $?
wait \"$!\" || exit $?
unset_args=(-u PAGER -u SSH_ASKPASS)
for entry in \"${inherited[@]}\"; do
  case \"$entry\" in GIT_*=*) unset_args+=(-u \"${entry%%=*}\");; esac
done
exec env \"${unset_args[@]}\" \\
  GIT_OPTIONAL_LOCKS=0 GIT_NO_LAZY_FETCH=1 GIT_NO_REPLACE_OBJECTS=1 \\
  GIT_TERMINAL_PROMPT=0 GIT_PAGER=cat \\
  GIT_TRACE2=0 GIT_TRACE2_EVENT=0 GIT_TRACE2_PERF=0 \"$@\""
  "Remove target Git environment entries before executing literal arguments.
Trace2 ignores command-line config overrides, so its fixed environment values
must override global and system trace targets.  Enumeration failure is fatal.")

(defconst mevedel-bash-git--worktree-guard
  "n=$1
temporary_root=$2
shift 2
git=(\"${@:1:n}\")
args=(\"${@:n+1}\")
set -o pipefail
gitdir=$(\"${git[@]}\" rev-parse --absolute-git-dir && printf '.') || exit $?
gitdir=${gitdir%.}
gitdir=${gitdir%$'\\n'}
common=$(\"${git[@]}\" rev-parse --path-format=absolute --git-common-dir && printf '.') || exit $?
common=${common%.}
common=${common%$'\\n'}
worktree=$(\"${git[@]}\" rev-parse --show-toplevel && printf '.') || exit $?
worktree=${worktree%.}
worktree=${worktree%$'\\n'}
case \"$gitdir$common$worktree\" in *$'\\n'*) exit 126 ;; esac
if [ -z \"$temporary_root\" ]; then temporary_root=${TMPDIR:-/tmp}; fi
tmp=$(mktemp -d \"$temporary_root/mevedel-git-inspection.XXXXXXXXXX\") || exit $?
trap 'rm -rf -- \"$tmp\"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
trap 'exit 129' HUP
shadow=$tmp/git
mkdir -- \"$shadow\" || exit $?
raw=$tmp/effective-config
: > \"$raw\"
: > \"$shadow/config\"
quote_config () {
  local text=$1
  text=${text//\\\\/\\\\\\\\}
  text=${text//\\\"/\\\\\\\"}
  text=${text//$'\\n'/\\\\n}
  text=${text//$'\\t'/\\\\t}
  text=${text//$'\\b'/\\\\b}
  printf '%s' \"$text\"
}
emit_config () {
  local key=$1 present=$2 value=$3 section name subsection
  section=${key%%.*}
  name=${key##*.}
  if [ \"$key\" = \"$section.$name\" ]; then
    printf '[%s]\\n' \"$section\"
  else
    subsection=${key#*.}
    subsection=${subsection%.*}
    printf '[%s \"%s\"]\\n' \"$section\" \"$(quote_config \"$subsection\")\"
  fi
  if [ \"$present\" -eq 1 ]; then
    printf ' %s = \"%s\"\\n' \"$name\" \"$(quote_config \"$value\")\"
  else
    printf ' %s\\n' \"$name\"
  fi
}
\"${git[@]}\" config --null --list --includes |
  while IFS= read -r -d '' entry; do
    key=${entry%%$'\\n'*}
    value=''
    present=0
    if [[ $entry == *$'\\n'* ]]; then value=${entry#*$'\\n'}; present=1; fi
    case \"${key,,}\" in include.path|includeif.*.path) continue ;; esac
    emit_config \"$key\" \"$present\" \"$value\" >> \"$raw\" || exit $?
    case \"${key,,}\" in filter.*.clean|filter.*.smudge|filter.*.process|filter.*.required) continue ;; esac
    emit_config \"$key\" \"$present\" \"$value\" >> \"$shadow/config\" || exit $?
  done || exit $?
for name in objects refs packed-refs reftable shallow logs info; do
  if [ -e \"$common/$name\" ]; then
    ln -s -- \"$common/$name\" \"$shadow/$name\" || exit $?
  fi
done
cp -L -- \"$gitdir/HEAD\" \"$shadow/HEAD\" || exit $?
if [ -f \"$gitdir/index\" ]; then cp -L -- \"$gitdir/index\" \"$shadow/index\" || exit $?; fi
for shared in \"$gitdir\"/sharedindex.* \"$common\"/sharedindex.*; do
  if [ -f \"$shared\" ]; then cp -L -- \"$shared\" \"$shadow/\" || exit $?; fi
done
export GIT_DIR=\"$shadow\" GIT_COMMON_DIR=\"$shadow\" GIT_WORK_TREE=\"$worktree\"
export GIT_CONFIG_SYSTEM=/dev/null GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
\"${git[@]}\" config --file \"$raw\" --get-regexp '^filter\\..*\\.(clean|process)$' >/dev/null
rc=$?
if [ \"$rc\" -eq 0 ]; then
  previous=''
  \"${git[@]}\" ls-files --cached --others --exclude-standard -z |
    \"${git[@]}\" check-attr -z --stdin filter |
    while IFS= read -r -d '' path && IFS= read -r -d '' attr && IFS= read -r -d '' value; do
      case \"$value\" in unspecified|unset) continue ;; esac
      if [ \"$value\" = \"$previous\" ]; then continue; fi
      for key in \"filter.$value.clean\" \"filter.$value.process\"; do
        \"${git[@]}\" config --file \"$raw\" --get \"$key\" >/dev/null
        rc=$?
        if [ \"$rc\" -eq 0 ]; then
          printf '%s\\n' 'Git inspection refused: configured worktree filters may execute helpers' >&2
          exit 126
        elif [ \"$rc\" -ne 1 ]; then
          exit \"$rc\"
        fi
      done
      previous=$value
    done || exit $?
elif [ \"$rc\" -ne 1 ]; then
  exit \"$rc\"
fi
index=$(\"${git[@]}\" ls-files --stage) || exit $?
tree=''
\"${git[@]}\" rev-parse --verify --quiet HEAD >/dev/null
rc=$?
if [ \"$rc\" -eq 0 ]; then
  tree=$(\"${git[@]}\" ls-tree -r HEAD) || exit $?
elif [ \"$rc\" -ne 1 ]; then
  exit \"$rc\"
fi
if [[ $index =~ (^|$'\\n')160000\\  || $tree =~ (^|$'\\n')160000\\  ]]; then
  printf '%s\\n' 'Git inspection refused: submodule inspection may execute repository helpers' >&2
  exit 126
fi
\"${git[@]}\" \"${args[@]}\"
exit $?"
  "Private metadata/config snapshot and guard; authored input stays argv.")


;;; Validation

(defun mevedel-bash-git--value-p (value rule)
  "Return whether VALUE satisfies option value RULE."
  (and (stringp value) (not (string-empty-p value))
       (pcase (car rule)
         ('number (and (string-match-p "\\`[0-9]+\\'" value)
                       (<= (string-to-number value) 1000000)))
         ('hex (string-match-p "\\`[0-9a-fA-F]+\\'" value))
         ('enum (member value (cdr rule)))
         ('operand (not (string-prefix-p "-" value)))
         ('text t)
         ('sort (not (string-match-p "signature" (downcase value))))
         ('pretty
          (and (mevedel-bash-git--value-p value '(format))
               (or (member value '("oneline" "short" "medium" "full" "fuller"
                                   "reference" "email" "mboxrd" "raw"))
                   (string-prefix-p "format:" value)
                   (string-prefix-p "tformat:" value)
                   (string-match-p "%" value))))
         ('format (not (or (string-match-p "%G" value)
                           (string-match-p "%([*]?signature" value)))))))

(defun mevedel-bash-git--parse (argv)
  "Validate Git ARGV, returning (GLOBAL COMMAND ARGS), or signal an error."
  (unless (and (proper-list-p argv) (equal (car argv) "git")
               (cl-every (lambda (arg)
                           (and (stringp arg)
                                (not (string-prefix-p "~" arg))
                                (not (string-match-p "\0" arg)))) argv))
    (error "Invalid Git argument vector"))
  (let ((rest (cdr argv)) global)
    (while (and rest (string-prefix-p "-" (car rest)))
      (let ((arg (pop rest)))
        (cond
         ((member arg '("--no-pager" "-P")) (push arg global))
         ((equal arg "-C")
          (unless (and rest (not (string-empty-p (car rest))))
            (error "Missing Git directory argument"))
          (push arg global) (push (pop rest) global))
         (t (error "Unsupported Git global option: %s" arg)))))
    (let* ((command (pop rest))
           (args rest)
           (diff-p (member command '("diff" "log" "show")))
           (flags (append (cdr (assoc command mevedel-bash-git--flags))
                          (when diff-p mevedel-bash-git--diff-flags)
                          (when (member command '("log" "show"))
                            mevedel-bash-git--history-flags)))
           (values (append (cdr (assoc command mevedel-bash-git--values))
                           (when diff-p mevedel-bash-git--diff-values)))
           operands seen-flags literal)
      (unless (assoc command mevedel-bash-git--flags)
        (error "Unsupported Git inspection command: %s" command))
      (while rest
        (let ((arg (pop rest)))
          (cond
           (literal (push arg operands))
           ((or (equal arg "--")
                (and (equal command "rev-parse")
                     (equal arg "--end-of-options")))
            (setq literal t))
           ((member arg flags) (push arg seen-flags))
           ((and (member command '("log" "show"))
                 (string-match-p "\\`-[0-9]+\\'" arg))
            (unless (mevedel-bash-git--value-p (substring arg 1) '(number))
              (error "Unbounded Git history count")))
           ((string-prefix-p "-" arg)
            (let* ((equals (string-match "=" arg))
                   (name (if equals (substring arg 0 equals) arg))
                   (rule (cdr (assoc name values)))
                   (value (when rule
                            (if equals (substring arg (1+ equals)) (pop rest)))))
              ;; Support common -n10/-U3 without accepting arbitrary clusters.
              (when (and (not rule)
                         (string-match "\\`\\(-[nU]\\)\\([0-9]+\\)\\'" arg))
                (setq rule (cdr (assoc (match-string 1 arg) values))
                      value (match-string 2 arg)))
              (unless (and rule (mevedel-bash-git--value-p value rule))
                (error "Unsupported Git inspection option: %s" arg))))
           (t (push arg operands)))))
      (setq operands (nreverse operands))
      (pcase command
        ("branch"
         (unless (or (member "--list" seen-flags)
                     (and (equal args '("--show-current")) (not operands)))
           (error "Git branch inspection requires explicit listing")))
        ("cat-file"
         (unless (or (and (= (length seen-flags) 1) (= (length operands) 1))
                     (and (null seen-flags) (= (length operands) 2)
                          (member (car operands) '("blob" "tree" "commit" "tag"))))
           (error "Unsupported Git cat-file mode")))
        ("ls-tree"
         (unless operands (error "Missing Git tree operand")))
        ("merge-base"
         (unless (if (member "--is-ancestor" seen-flags)
                     (= (length operands) 2)
                   (if (member "--fork-point" seen-flags)
                       (memq (length operands) '(1 2))
                     (if (member "--independent" seen-flags)
                         operands
                       (>= (length operands) 2))))
           (error "Missing or excess Git merge-base operands"))))
      (list (nreverse global) command args))))


;;; Public interface

(defun mevedel-bash-git-read-only-p (argv)
  "Return non-nil if ARGV is a supported Git inspection argument list.
This classification is valid only when launched through
`mevedel-bash-git-prepare', with ordinary resource permissions enforced."
  (condition-case nil
      (progn (mevedel-bash-git--parse argv) t)
    (error nil)))

(defun mevedel-bash-git-prepare (argv &optional temporary-root)
  "Return a complete hardened executable argument list for Git ARGV.
Signal an error if ARGV is not supported; never fall back to ordinary Git.
The returned list starts with privileged Bash and is suitable for direct argv
execution or individual shell quoting.  ARGV is not mutated.  All inherited
GIT_* environment entries are cleared at the target before fixed inspection
controls are installed.  Normal config files are
still read, but inspection helpers, tracing, optional index writes, object
replacement, implicit fetches and prompts are disabled.  Worktree routes
refuse active clean/process filters and gitlinks at launch, rather than
silently changing comparisons or executing submodule helpers.  An unused
filter definition alone does not cause refusal.  TEMPORARY-ROOT, when non-nil,
is the trusted absolute native target directory for private temporary metadata;
it must already be available to the child.  It is passed separately from ARGV
and never used as a Git operand.  Nil retains the target's TMPDIR or /tmp
default.
See Commentary for the execution-environment trust boundary."
  (unless (or (null temporary-root)
              (and (stringp temporary-root)
                   (string-prefix-p "/" temporary-root)
                   (not (string-match-p "\0" temporary-root))))
    (error "Invalid native temporary directory"))
  (pcase-let* ((`(,global ,command ,args) (mevedel-bash-git--parse argv))
               (git-prefix
                (append
                 '("git" "--no-pager" "--no-replace-objects" "--no-lazy-fetch"
                   "--no-optional-locks" "-c" "core.fsmonitor=false"
                   "-c" "core.untrackedCache=false" "-c" "log.showSignature=false"
                   "-c" "format.pretty=medium" "-c" "branch.sort=refname"
                   "-c" "diff.submodule=short" "-c" "diff.autoRefreshIndex=false"
                   "-c" "status.submoduleSummary=false")
                 global))
               (command-args
                (append (list command)
                        (when (member command '("diff" "log" "show"))
                          '("--no-ext-diff" "--no-textconv"))
                        (when (member command '("log" "show"))
                          '("--no-show-signature"))
                        ;; branch.sort is multi-valued: adding one safe key
                        ;; does not stop earlier signature keys being evaluated.
                        (when (equal command "branch")
                          '("--no-sort" "--sort=refname"))
                        (if (member command '("status" "diff"))
                            (let ((separator (or (cl-position "--" args :test #'equal)
                                                 (length args))))
                              (append (cl-subseq args 0 separator)
                                      '("--ignore-submodules=all")
                                      (nthcdr separator args)))
                          args))))
    (append
     (list "bash" "--noprofile" "--norc" "-p" "-c"
           mevedel-bash-git--environment-guard "mevedel-git-environment")
     (when (member command '("status" "diff"))
       (list "bash" "--noprofile" "--norc" "-p" "-c"
             mevedel-bash-git--worktree-guard "mevedel-git-inspection"
             (number-to-string (length git-prefix)) (or temporary-root "")))
     git-prefix command-args)))

(provide 'mevedel-bash-git)
;;; mevedel-bash-git.el ends here
