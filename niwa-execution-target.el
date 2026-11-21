;;; niwa-execution-target.el -- Workspace execution targets -*- lexical-binding: t -*-

;;; Commentary:

;; Derives the one filesystem and command authority owned by a session.
;; Callers use target-native paths for model-facing data and qualified paths
;; for Emacs filesystem operations.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; Probe commands wait on the shared connection with timers suspended,
;; and the suspension is a macro, so this is a load-time dependency.
(require 'niwa-transport)

;; `niwa-sandbox'
(declare-function niwa-sandbox-invalidate-probe-cache
                  "niwa-sandbox" (&optional workdir))
(declare-function niwa-sandbox-probe "niwa-sandbox" (&optional workdir))
(autoload 'niwa-sandbox-invalidate-probe-cache "niwa-sandbox")
(autoload 'niwa-sandbox-probe "niwa-sandbox")

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-get-connection-process "tramp" (vec))
(autoload 'tramp-dissect-file-name "tramp")
(autoload 'tramp-get-connection-process "tramp")

(define-error 'niwa-execution-target-error "Execution target error")

(defconst niwa-execution-target--hard-dependencies '(rg bash setsid)
  "Programs required for remote workspace execution.")

(defconst niwa-execution-target--optional-capabilities
  '(bwrap gio inotifywait)
  "Programs whose availability enables optional target workflows.")

(defconst niwa-execution-target--dependency-packages
  '((rg . "ripgrep") (bash . "bash") (setsid . "util-linux"))
  "Package supplying each required target program.")

(defconst niwa-execution-target--package-managers
  '(("apt-get" . "apt-get install -y %s")
    ("dnf" . "dnf install -y %s")
    ("apk" . "apk add %s")
    ("pacman" . "pacman -S --noconfirm %s")
    ("zypper" . "zypper install -y %s"))
  "Install command template for each recognized package manager.")

(defconst niwa-execution-target--probe-timeout 10
  "Maximum seconds spent in one readiness probe.")

(cl-defstruct (niwa-execution-target
               (:constructor niwa-execution-target--create)
               (:copier nil))
  "One workspace filesystem and command authority."
  workspace-root
  native-root
  prefix
  method
  identity
  support-tier
  environment
  readiness
  connection-process
  incarnation
  observed-incarnation
  incarnation-changed-p)

(defun niwa-execution-target--prefix (path)
  "Return PATH's complete TRAMP prefix, including any hop."
  (when-let* ((native (file-remote-p path 'localname 'never)))
    (unless (string-suffix-p native path)
      (error "Cannot identify execution target for path: %s" path))
    (substring path 0 (- (length path) (length native)))))

(defun niwa-execution-target--support-tier (method hop)
  "Return support tier for TRAMP METHOD and HOP."
  (cond
   (hop 'experimental)
   ((memq method '(ssh scp sshx scpx docker podman)) 'supported)
   ((eq method 'kubernetes) 'experimental)
   (method 'unsupported)
   (t 'supported)))

(defun niwa-execution-target--identity (root method hop)
  "Return a structured identity for ROOT, METHOD, and HOP."
  (if (null method)
      '(:method local)
    (let ((user (file-remote-p root 'user 'never))
          (host (file-remote-p root 'host 'never)))
      (append (list :method method)
              (and user (list :user user))
              (and host (list :host host))
              (and hop (list :hop hop))))))

(defun niwa-execution-target--parse-environment-entry (entry)
  "Parse one NAME=VALUE environment ENTRY."
  (when-let* ((separator (string-search "=" entry)))
    (cons (substring entry 0 separator)
          (substring entry (1+ separator)))))

(defun niwa-execution-target--read-local-identity (path)
  "Return the trimmed contents of local identity file PATH, or nil."
  (condition-case nil
      (when (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents-literally path)
          (let ((value (string-trim (buffer-string))))
            (and (not (string-empty-p value)) value))))
    (error nil)))

(defun niwa-execution-target--local-pid1-start-time ()
  "Return stable PID 1 start ticks from `/proc', or nil."
  (condition-case nil
      (when-let* ((line (niwa-execution-target--read-local-identity
                         "/proc/1/stat"))
                  (match (and (string-match "\\(.*\\)) [A-Z] " line)
                              (match-end 1)))
                  (fields (split-string
                           (substring line (1+ match)) "[ \t]+" t)))
        ;; FIELD 3 (state) is the first element.  FIELD 22 (starttime) is
        ;; element 19 when the split starts at that state field.
        (nth 19 fields))
    (error nil)))

(defun niwa-execution-target--normalize-incarnation-field (value)
  "Return VALUE in the canonical single-line incarnation form."
  (replace-regexp-in-string
   "[\r\n]" "" (string-trim (or value ""))))

(defun niwa-execution-target--normalize-hostname (hostname)
  "Return canonical HOSTNAME bytes for an incarnation payload."
  (let ((value (downcase
                (niwa-execution-target--normalize-incarnation-field
                 hostname))))
    (while (string-suffix-p "." value)
      (setq value (substring value 0 -1)))
    value))

(defun niwa-execution-target--local-hostname ()
  "Return the local kernel hostname used by the target probe, or nil.

The rest of the local payload comes from this machine's `/proc', so the
hostname must too: a remote `default-directory' would mix two machines into
one fingerprint and read as a replacement.  `/proc/sys/kernel/hostname' is
the nodename `uname -n' prints, in this process's UTS namespace, without the
process `uname' cost on every incarnation check; `uname' remains the
fallback where procfs does not offer it."
  (or (when-let* ((raw (niwa-execution-target--read-local-identity
                        "/proc/sys/kernel/hostname")))
        ;; Decoded as `uname's output is, so a fingerprint keeps its bytes.
        (decode-coding-string raw (or (car default-process-coding-system)
                                      'undecided)))
      (condition-case nil
          (with-temp-buffer
            (let ((default-directory "/"))
              (unless (zerop (process-file "uname" nil t nil "-n"))
                (error "'uname' failed")))
            (let ((value (string-trim (buffer-string))))
              (and (not (string-empty-p value)) value)))
        (error nil))))

(defun niwa-execution-target--incarnation-payload
    (boot machine pid1-start hostname)
  "Return canonical incarnation bytes for BOOT, MACHINE, PID1-START, HOSTNAME."
  (format "boot=%s\nmachine=%s\npid1-start=%s\nhostname=%s\n"
          (niwa-execution-target--normalize-incarnation-field boot)
          (niwa-execution-target--normalize-incarnation-field machine)
          (niwa-execution-target--normalize-incarnation-field pid1-start)
          (niwa-execution-target--normalize-hostname hostname)))

(defun niwa-execution-target--local-incarnation ()
  "Return a stable fingerprint for this local host incarnation.

The boot id or PID 1 start time is required to distinguish a reboot or
recreated runtime.  Machine id and hostname may salt that strong observation,
but never form an authority identity on their own."
  (let ((boot (niwa-execution-target--read-local-identity
               "/proc/sys/kernel/random/boot_id"))
        (machine (niwa-execution-target--read-local-identity
                  "/etc/machine-id"))
        (pid1-start (niwa-execution-target--local-pid1-start-time))
        (hostname (niwa-execution-target--local-hostname)))
    (when (or (and boot (not (string-empty-p boot)))
              (and pid1-start (not (string-empty-p pid1-start))))
      (secure-hash
       'sha256
       (niwa-execution-target--incarnation-payload
        boot machine pid1-start hostname)))))

(defun niwa-execution-target-create (workspace-root)
  "Derive an execution target from WORKSPACE-ROOT.

This is side-effect free and never opens a TRAMP connection."
  (unless (and (stringp workspace-root)
               (file-name-absolute-p workspace-root))
    (error "Workspace root must be an absolute path"))
  (let* ((prefix (niwa-execution-target--prefix workspace-root))
         (method-name (file-remote-p workspace-root 'method 'never))
         (method (and method-name (intern method-name)))
         (hop (file-remote-p workspace-root 'hop 'never))
         (native-root (if prefix
                          (file-name-as-directory
                           (expand-file-name
                            (file-remote-p workspace-root 'localname 'never)
                            "/"))
                        (file-name-as-directory
                         (expand-file-name workspace-root))))
         (root (if prefix (concat prefix native-root) native-root)))
    (niwa-execution-target--create
     :workspace-root root
     :native-root native-root
     :prefix prefix
     :method method
     :identity (niwa-execution-target--identity
                workspace-root method hop)
     :support-tier (niwa-execution-target--support-tier method hop)
     :incarnation (and (null prefix)
                       (niwa-execution-target--local-incarnation))
     :environment
     (unless prefix
       (delq nil
             (mapcar #'niwa-execution-target--parse-environment-entry
                     process-environment))))))

(defun niwa-execution-target-remote-p (target)
  "Return non-nil when TARGET is remote through TRAMP.

A nil TARGET is the local environment, so it is not remote."
  (and target (niwa-execution-target-prefix target) t))

(defun niwa-execution-target-supported-p (target)
  "Return non-nil when TARGET has the supported tier."
  (eq 'supported (niwa-execution-target-support-tier target)))

(defun niwa-execution-target-direct-async-capable-p (target)
  "Return non-nil when TARGET can host direct-async process spawns.

Single-hop ssh and scp connections qualify.  The container methods
carry the direct-async parameter too, but their per-spawn client exec
allocates a tty and prints its own notices ahead of the command --
carriage returns and interleaved client output corrupt the group
marker protocol -- so they keep the classic spawn.  A hop disqualifies
the whole connection.  This is a pure capability statement; whether an
individual spawn uses it is the execution layer's decision."
  (and target
       (niwa-execution-target-remote-p target)
       (memq (niwa-execution-target-method target) '(ssh scp))
       (not (plist-get (niwa-execution-target-identity target) :hop))))

(defun niwa-execution-target-label (target &optional directory)
  "Return a compact user-facing identity for TARGET.
When TARGET is nil, derive the identity from DIRECTORY, which defaults to
`default-directory'.  This only parses the path; it never probes the target.
For multi-hop paths, identify the final destination."
  (let* ((identity
          (if target
              (niwa-execution-target-identity target)
            (let* ((directory (or directory default-directory))
                   (method (file-remote-p directory 'method 'never)))
              (niwa-execution-target--identity
               directory (and method (intern method)) nil))))
         (method (plist-get identity :method))
         (user (plist-get identity :user))
         (host (plist-get identity :host)))
    (if (eq method 'local)
        "local"
      (format "%s%s"
              method
              (if host
                  (format ":%s%s" (if user (concat user "@") "") host)
                "")))))

(defun niwa-execution-target-native-path (target path)
  "Return PATH in TARGET's native path domain.

Signal `niwa-execution-target-error' when PATH explicitly names a
different execution target."
  (unless (stringp path)
    (signal 'niwa-execution-target-error
            (list "Path must be a string")))
  (when (file-name-quoted-p path 'top)
    (signal 'niwa-execution-target-error
            (list (format "Quoted paths are not allowed: %s" path))))
  (if-let* ((path-prefix (niwa-execution-target--prefix path)))
      (if (equal path-prefix (niwa-execution-target-prefix target))
          (file-remote-p path 'localname 'never)
        (signal 'niwa-execution-target-error
                (list (format "Path names another execution target: %s"
                              path))))
    path))

(defun niwa-execution-target--path-domain-key (path)
  "Return PATH's normalized local or remote filesystem domain key."
  (if-let* ((method (file-remote-p path 'method 'never)))
      (let* ((raw-host (file-remote-p path 'host 'never))
             (hop (file-remote-p path 'hop 'never))
             (explicit-port
              (and raw-host
                   (string-match "\\`\\(.*\\)#\\([0-9]+\\)\\'" raw-host)
                   (match-string 2 raw-host)))
             (host (if explicit-port (match-string 1 raw-host) raw-host))
             (ssh-family-p
              (member method '("ssh" "scp" "sshx" "scpx"))))
        (list method
              (file-remote-p path 'user 'never)
              (and host (if ssh-family-p (downcase host) host))
              (or explicit-port (and ssh-family-p "22"))
              (mapcar
               (lambda (entry)
                 (niwa-execution-target--path-domain-key
                  (concat "/" entry ":/")))
               (split-string (or hop "") "|" t))))
    'local))

(defun niwa-execution-target-same-path-domain-p (left right)
  "Return non-nil when LEFT and RIGHT name the same filesystem domain."
  (equal (niwa-execution-target--path-domain-key left)
         (niwa-execution-target--path-domain-key right)))

(defun niwa-execution-target--environment-value (target name)
  "Return NAME from TARGET's probed environment or signal an error."
  (if-let* ((entry (assoc name
                          (niwa-execution-target-environment target))))
      (cdr entry)
    (signal 'niwa-execution-target-error
            (list (format "Unknown target environment variable: %s" name)))))

(defun niwa-execution-target--expand-path-text (target path)
  "Expand TARGET environment and home syntax in native PATH."
  (let ((expanded
         (replace-regexp-in-string
          "\\$\\(?:{\\([[:alpha:]_][[:alnum:]_]*\\)}\\|\\([[:alpha:]_][[:alnum:]_]*\\)\\)"
          (lambda (reference)
            (niwa-execution-target--environment-value
             target
             (if (string-prefix-p "${" reference)
                 (substring reference 2 -1)
               (substring reference 1))))
          path t t)))
    (cond
     ((equal expanded "~")
      (niwa-execution-target--environment-value target "HOME"))
     ((string-prefix-p "~/" expanded)
      (file-name-concat
       (niwa-execution-target--environment-value target "HOME")
       (substring expanded 2)))
     ((and (niwa-execution-target-remote-p target)
           (string-prefix-p "~" expanded))
      (signal 'niwa-execution-target-error
              (list (format "Unsupported target home path: %s" path))))
     (t expanded))))

(defun niwa-execution-target-expand-path (target path &optional directory)
  "Return PATH expanded and qualified for TARGET.

Relative PATH resolves against DIRECTORY or TARGET's workspace root.
Environment variables and `~' resolve from TARGET's probed environment.
DIRECTORY may be target-native or already qualified for TARGET."
  (let* ((native-path
          (niwa-execution-target--expand-path-text
           target (niwa-execution-target-native-path target path)))
         (native-directory
          (niwa-execution-target--expand-path-text
           target
           (niwa-execution-target-native-path
            target
            (or directory (niwa-execution-target-native-root target)))))
         (native-directory
          (if (file-name-absolute-p native-directory)
              native-directory
            (expand-file-name
             native-directory
             (niwa-execution-target-native-root target))))
         (resolved (niwa-expand-file-name native-path native-directory)))
    (concat (niwa-execution-target-prefix target) resolved)))

(defun niwa-execution-target--process-output (target program &rest args)
  "Run fixed PROGRAM ARGS on TARGET and return its output."
  (let ((default-directory
         (niwa-execution-target-workspace-root target))
        ;; TRAMP starts from the target shell environment.  Sending the
        ;; client's environment would leak client state, while local targets
        ;; must retain their ordinary process environment.
        (process-environment
         (unless (niwa-execution-target-remote-p target)
           process-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (current-buffer) nil
                           args)))
        (unless (and (integerp status) (zerop status))
          (error "Target probe command failed: %s" program))
        (buffer-string)))))

(defconst niwa-execution-target--virtualization-script
  "if [ -f /.dockerenv ]; then echo docker
elif [ -f /run/.containerenv ]; then echo podman
elif grep -qi microsoft /proc/version 2>/dev/null; then echo wsl
elif command -v systemd-detect-virt >/dev/null 2>&1; then
  systemd-detect-virt 2>/dev/null || true
fi"
  "POSIX shell script printing the target's container, VM or WSL kind.
Container and WSL markers are file tests; `systemd-detect-virt' is
consulted only when present.  The script exits zero and prints nothing
or \"none\" when no virtualization is detected.")

(defun niwa-execution-target--probe-virtualization (target)
  "Return TARGET's container, VM or WSL kind as a string, or nil.
Run once per readiness probe, so the result is cached with readiness.
A failed or unexpected answer yields nil and never blocks readiness."
  (when-let* ((output
               (ignore-errors
                 (niwa-execution-target--process-output
                  target "sh" "-c"
                  niwa-execution-target--virtualization-script)))
              (kind (string-trim (car (split-string output "\n")))))
    (and (string-match-p "\\`[a-z0-9-]\\{1,32\\}\\'" kind)
         (not (equal kind "none"))
         kind)))

(defun niwa-execution-target--live-connection (target)
  "Return TARGET's live TRAMP connection process, or nil."
  (when (niwa-execution-target-remote-p target)
    (let ((process
           (tramp-get-connection-process
            (tramp-dissect-file-name
             (niwa-execution-target-workspace-root target)))))
      (and (process-live-p process) process))))

(defun niwa-execution-target--probe-incarnation (target bash)
  "Return TARGET's Linux/container incarnation fingerprint using BASH."
  (let ((output
         (niwa-execution-target--process-output
          target bash "-c"
          (concat
           "boot=; "
           "if test -r /proc/sys/kernel/random/boot_id; then "
           "IFS= read -r boot </proc/sys/kernel/random/boot_id || true; "
           "fi; "
           "machine=; "
           "if test -r /etc/machine-id; then "
           "IFS= read -r machine </etc/machine-id || true; fi; "
           "start=; "
           "if IFS= read -r line </proc/1/stat; then "
           "rest=${line##*) }; set -- $rest; start=${20}; "
           "fi; "
           "hostname=$(uname -n 2>/dev/null || true); "
           "test -n \"$boot\" || test -n \"$start\" || exit 72; "
           "printf 'boot=%s\\nmachine=%s\\npid1-start=%s\\nhostname=%s\\n' "
           "\"$boot\" \"$machine\" \"$start\" \"$hostname\""))))
    (when (string-empty-p output)
      (error "Target incarnation probe returned no identity"))
    (let (boot machine pid1-start hostname)
      (dolist (line (split-string output "\n" t))
        (when (string-match
               "\\`\\(boot\\|machine\\|pid1-start\\|hostname\\)=\\(.*\\)\\'"
               line)
          (pcase (match-string 1 line)
            ("boot" (setq boot (match-string 2 line)))
            ("machine" (setq machine (match-string 2 line)))
            ("pid1-start" (setq pid1-start (match-string 2 line)))
            ("hostname" (setq hostname (match-string 2 line))))))
      (unless (or (not (string-empty-p (or boot "")))
                  (not (string-empty-p (or pid1-start ""))))
        (error "Target incarnation probe returned no identity"))
      (secure-hash
       'sha256
       (niwa-execution-target--incarnation-payload
        boot machine pid1-start hostname)))))

(defun niwa-execution-target--record-incarnation (target incarnation)
  "Stage observed INCARNATION on TARGET and detect replacement.

The acknowledged `incarnation' remains unchanged while a replacement is
pending, so incidental persistence cannot pair the new target identity with
authority granted to the old target."
  (let ((baseline (niwa-execution-target-incarnation target)))
    (cond
     ((null baseline)
      (setf (niwa-execution-target-incarnation target) incarnation
            (niwa-execution-target-observed-incarnation target) nil
            (niwa-execution-target-incarnation-changed-p target) nil))
     ((niwa-execution-target-incarnation-changed-p target)
      (setf (niwa-execution-target-observed-incarnation target)
            incarnation))
     ((not (equal baseline incarnation))
      (setf (niwa-execution-target-observed-incarnation target)
            incarnation
            (niwa-execution-target-incarnation-changed-p target) t))))
  target)

(defun niwa-execution-target-refresh-incarnation (target)
  "Refresh TARGET's local incarnation observation when it is local.

Remote targets refresh their identity as part of readiness probing; local
targets do not need the full command probe merely to detect replacement."
  (unless (niwa-execution-target-remote-p target)
    (let ((incarnation (niwa-execution-target--local-incarnation)))
      (unless incarnation
        (error "Local target incarnation is unavailable"))
      (niwa-execution-target--record-incarnation target incarnation)))
  target)

(defun niwa-execution-target-observe-incarnation (target)
  "Refresh TARGET's incarnation observation with one target command.

Mutation admission only needs to know whether the target was replaced.  A
full readiness probe re-derives environment, capabilities, and sandbox facts
that are fixed for the life of the connection, at a cost of roughly fifteen
synchronous round trips; this observation costs one.  TARGET must already
carry a readiness result, because the fingerprint command runs through the
Bash capability that probe established."
  (if (niwa-execution-target-remote-p target)
      (let ((bash (niwa-execution-target-capability target 'bash)))
        (unless bash
          (error "Target readiness has not established a Bash capability"))
        (niwa-execution-target--record-incarnation
         target
         (niwa-transport-call-with-deadline
          niwa-execution-target--probe-timeout
          (lambda () (niwa-execution-target--probe-incarnation target bash))
          (lambda ()
            (signal 'niwa-execution-target-error
                    (list "Target incarnation probe timed out"))))))
    (niwa-execution-target-refresh-incarnation target))
  target)

(defun niwa-execution-target-restore-incarnation (target incarnation)
  "Restore persisted INCARNATION while retaining TARGET's live observation.

For a local target, TARGET already carries the current host fingerprint from
construction.  A mismatch remains pending until the caller revokes prior
grants and publishes the replacement profile."
  (unless (and (stringp incarnation) (not (string-blank-p incarnation)))
    (error "Invalid target incarnation: %S" incarnation))
  (when (and (not (niwa-execution-target-remote-p target))
             (null (niwa-execution-target-incarnation target)))
    (error "Local target incarnation is unavailable"))
  (let ((observed (niwa-execution-target-incarnation target)))
    (setf (niwa-execution-target-incarnation target) incarnation
          (niwa-execution-target-observed-incarnation target)
          (and observed (unless (equal observed incarnation) observed))
          (niwa-execution-target-incarnation-changed-p target)
          (and observed (not (equal observed incarnation)))))
  target)

(defun niwa-execution-target-prepare-incarnation-acknowledgement (target)
  "Make TARGET's staged incarnation serializable without acknowledging it.

Call this only after revoking exact authority associated with the previous
incarnation.  `incarnation-changed-p' remains non-nil until the resulting
sidecar publication commits."
  (let ((observed (niwa-execution-target-observed-incarnation target)))
    (unless (and (niwa-execution-target-incarnation-changed-p target)
                 (stringp observed)
                 (not (string-blank-p observed)))
      (error "Target incarnation replacement is not staged"))
    (setf (niwa-execution-target-incarnation target) observed))
  target)

(defun niwa-execution-target-acknowledge-incarnation (target)
  "Acknowledge TARGET's observed incarnation replacement."
  (when (niwa-execution-target-incarnation-changed-p target)
    (unless (equal (niwa-execution-target-incarnation target)
                   (niwa-execution-target-observed-incarnation target))
      (error "Target incarnation replacement is not prepared"))
    (setf (niwa-execution-target-observed-incarnation target) nil
          (niwa-execution-target-incarnation-changed-p target) nil))
  target)

(defun niwa-execution-target--parse-environment (output)
  "Parse NUL-delimited env OUTPUT into an alist."
  (delq nil
        (mapcar #'niwa-execution-target--parse-environment-entry
                (split-string output "\0" t))))

(defun niwa-execution-target--probe-capabilities (target)
  "Return TARGET's available program capabilities as an alist."
  (let ((default-directory
         (niwa-execution-target-workspace-root target))
        (remote (niwa-execution-target-prefix target))
        capabilities)
    (dolist (name (append niwa-execution-target--hard-dependencies
                          niwa-execution-target--optional-capabilities))
      (when-let* ((path (executable-find (symbol-name name) remote)))
        (push (cons name
                    (niwa-execution-target-native-path target path))
              capabilities)))
    (nreverse capabilities)))

(defun niwa-execution-target--install-hint (target missing)
  "Return the command installing MISSING programs on TARGET, or nil.
Only a recognized package manager present on the target yields a hint;
an unrecognized one leaves the readiness diagnostic naming the programs."
  (let ((default-directory
         (niwa-execution-target-workspace-root target))
        (remote (niwa-execution-target-prefix target)))
    (when-let* ((packages
                 (delete-dups
                  (delq nil
                        (mapcar
                         (lambda (name)
                           (alist-get
                            name
                            niwa-execution-target--dependency-packages))
                         missing))))
                (template
                 (cdr (seq-find
                       (lambda (entry) (executable-find (car entry) remote))
                       niwa-execution-target--package-managers))))
      (format template (string-join packages " ")))))

(defun niwa-execution-target--probe-dependency-behavior
    (target capabilities)
  "Return CAPABILITIES that cannot satisfy TARGET's required behavior."
  (delq
   nil
   (mapcar
    (lambda (name)
      (when-let* ((program (alist-get name capabilities)))
        (when (or (eq name 'bash) (alist-get 'bash capabilities))
          (condition-case nil
              (progn
                (pcase name
                  ('rg
                   (niwa-execution-target--process-output
                    target (alist-get 'bash capabilities) "-c"
                    (concat
                     "set -eu; "
                     "root=$(mktemp -d \"${TMPDIR:-/tmp}/niwa-rg.XXXXXX\"); "
                     "trap 'rm -rf \"$root\"' EXIT; "
                     "printf 'Needle\\nother\\n' > \"$root/.probe.txt\"; "
                   "files=$(\"$1\" --files --hidden --no-ignore --follow "
                   "--sort path "
                   "--color=never --iglob '*.txt' --glob='!**/.git' \"$root\"); "
                     "case \"$files\" in *'.probe.txt'*) ;; *) exit 80;; esac; "
                     "content=$(\"$1\" --hidden --no-require-git "
                     "--line-number --heading -A1 -B1 -C1 --max-count=1000 "
                     "--max-columns=2000 --max-columns-preview -i -U "
                     "--multiline-dotall --glob='*.txt' --type=txt "
                     "-e needle \"$root\"); "
                     "case \"$content\" in *Needle*) ;; *) exit 81;; esac; "
                     "\"$1\" --hidden --no-require-git --files-with-matches "
                     "-e Needle \"$root\" >/dev/null; "
                     "\"$1\" --hidden --no-require-git --count "
                     "-e Needle \"$root\" >/dev/null")
                    "niwa-rg-probe" program))
                  ('bash
                   (niwa-execution-target--process-output
                    target program "-c"
                    "test -n \"$BASH_VERSION\" && test -n \"$BASHPID\""))
                  ('setsid
                   (niwa-execution-target--process-output
                    target program "-f" "-w" "bash" "-c" "exit 0")))
                nil)
            (error name)))))
    niwa-execution-target--hard-dependencies)))

(defun niwa-execution-target--apply-sandbox-readiness
    (target readiness sandbox-mode)
  "Return READINESS with TARGET's effective SANDBOX-MODE facts."
  (let ((readiness (copy-sequence readiness)))
    (when sandbox-mode
      (setq readiness (plist-put readiness :sandbox-mode sandbox-mode)))
    (when (and (niwa-execution-target-remote-p target)
               (eq 'ready (plist-get readiness :status)))
      (pcase sandbox-mode
        ('off
         (setq readiness (plist-put readiness :sandbox-status 'off)))
        ((or 'best-effort 'required)
         (let ((availability
                (niwa-sandbox-probe
                 (niwa-execution-target-workspace-root target))))
           (if (plist-get availability :available)
               (setq readiness
                     (plist-put readiness :sandbox-status 'bubblewrap))
             (setq readiness
                   (plist-put readiness :sandbox-status 'unavailable))
             (setq readiness
                   (plist-put readiness :sandbox-reason
                              (plist-get availability :reason)))
             (when (eq sandbox-mode 'required)
               (setq readiness (plist-put readiness :status 'blocked))
               (setq readiness
                     (plist-put readiness :reason 'sandbox-unavailable))))))))
    readiness))

(defun niwa-execution-target-probe
    (target &optional refresh sandbox-mode)
  "Return TARGET readiness, probing its live connection when needed.

The fixed probe result is cached for TARGET's live TRAMP connection.  A
reconnect or non-nil REFRESH discards it and the matching Bubblewrap cache.
  SANDBOX-MODE records the session's effective confinement requirement."
  (let* ((remote (niwa-execution-target-remote-p target))
         (connection (and remote
                          (niwa-execution-target--live-connection target)))
         (reconnected
          (and (niwa-execution-target-readiness target)
               (not (eq connection
                        (niwa-execution-target-connection-process target)))))
         (refresh (or refresh reconnected)))
    (unless remote
      (niwa-execution-target-refresh-incarnation target))
    (when (and remote
               (or refresh
                   (null (niwa-execution-target-readiness target))))
      (niwa-sandbox-invalidate-probe-cache
       (niwa-execution-target-workspace-root target)))
    (if (and (not refresh)
             (niwa-execution-target-readiness target)
             (eq sandbox-mode
                 (plist-get (niwa-execution-target-readiness target)
                            :sandbox-mode)))
        (niwa-execution-target-readiness target)
      (setf (niwa-execution-target-environment target) nil
            (niwa-execution-target-readiness target)
            (niwa-execution-target--apply-sandbox-readiness
             target
             (if (eq 'unsupported
                     (niwa-execution-target-support-tier target))
                 '(:status blocked :reason unsupported-target)
               (condition-case err
                   ;; Every probe command waits on the shared connection;
                   ;; a foreign timer that starts its own remote operation
                   ;; mid-wait consumes a probe reply and both readers
                   ;; desync -- a flycheck stat has been observed parsing
                   ;; this probe's `env -0' dump.  One suspension covers
                   ;; the whole probe without blocking its own deadline.
                   (niwa-transport-call-with-deadline
                    niwa-execution-target--probe-timeout
                    (lambda ()
                     (let* ((environment
                             (niwa-execution-target--parse-environment
                              (niwa-execution-target--process-output
                               target "env" "-0")))
                            (operating-system
                             (string-trim
                              (niwa-execution-target--process-output
                               target "uname" "-s")))
                            (operating-system-version
                             (string-trim
                              (niwa-execution-target--process-output
                               target "uname" "-r")))
                            (virtualization
                             (niwa-execution-target--probe-virtualization
                              target))
                            (capabilities
                             (niwa-execution-target--probe-capabilities target))
                            (missing
                             (delq nil
                                   (mapcar
                                    (lambda (name)
                                      (unless (assq name capabilities) name))
                                    niwa-execution-target--hard-dependencies)))
                            (incompatible
                             (niwa-execution-target--probe-dependency-behavior
                              target capabilities))
                            (install-hint
                             (and missing
                                  (niwa-execution-target--install-hint
                                   target missing)))
                            (home (cdr (assoc "HOME" environment)))
                            (missing-home
                             (or (null home) (string-empty-p home)))
                            (unsupported-os
                             (and (niwa-execution-target-remote-p target)
                                  (not (equal operating-system "Linux"))))
                            (incarnation
                             (and remote
                                  (null missing)
                                  (null incompatible)
                                  (not unsupported-os)
                                  (niwa-execution-target--probe-incarnation
                                   target (alist-get 'bash capabilities)))))
                       (setf (niwa-execution-target-environment target)
                             environment)
                       (when incarnation
                         (niwa-execution-target--record-incarnation
                          target incarnation))
                       (list :status (if (or missing-home missing incompatible
                                             unsupported-os)
                                         'blocked
                                       'ready)
                             :reason (cond
                                      (unsupported-os
                                       'unsupported-operating-system)
                                      (missing-home 'missing-environment)
                                      (missing 'missing-dependencies)
                                      (incompatible 'incompatible-dependencies))
                             :error
                             (and missing-home
                                  (mapconcat
                                   #'identity
                                   (delq
                                    nil
                                    (list
                                     "Target environment does not define a non-empty HOME"
                                     (and missing
                                          (format
                                           "missing required target programs: %s"
                                           (mapconcat #'symbol-name missing ", ")))
                                     (and incompatible
                                          (format
                                           "incompatible target programs: %s"
                                           (mapconcat #'symbol-name
                                                      incompatible ", ")))))
                                   "; "))
                             :operating-system operating-system
                             :operating-system-version operating-system-version
                             :virtualization virtualization
                             :capabilities capabilities
                             :missing-dependencies missing
                             :install-hint install-hint
                             :incompatible-dependencies incompatible
                             :incarnation incarnation)))
                    (lambda ()
                      (signal 'niwa-execution-target-error
                              (list "Target readiness probe timed out"))))
                 (error
                  (list :status 'blocked
                        :reason 'probe-failed
                        :error (error-message-string err)))))
             sandbox-mode))
      (when remote
        (setf (niwa-execution-target-connection-process target)
              (niwa-execution-target--live-connection target)))
      (niwa-execution-target-readiness target))))

(defun niwa-execution-target-ready-p (target)
  "Return non-nil when TARGET's last readiness probe succeeded."
  (eq 'ready
      (plist-get (niwa-execution-target-readiness target) :status)))

(defun niwa-execution-target-capability (target name)
  "Return executable path for capability NAME in TARGET's native domain."
  (cdr (assq name
             (plist-get (niwa-execution-target-readiness target)
                        :capabilities))))

(defun niwa-execution-target-missing-dependencies (target)
  "Return hard dependencies missing from TARGET's last probe."
  (plist-get (niwa-execution-target-readiness target)
             :missing-dependencies))

(defun niwa-execution-target-readiness-message (target)
  "Return a user-facing summary of TARGET's readiness."
  (let* ((readiness (niwa-execution-target-readiness target))
         (reason (plist-get readiness :reason))
         (retry "run M-x niwa-retry-target-readiness after fixing it"))
    (pcase (plist-get readiness :status)
      ('ready
       (pcase (plist-get readiness :sandbox-status)
         ('unavailable
          (format
           "ready; sandbox unavailable, best-effort execution will be unconfined: %s"
           (or (plist-get readiness :sandbox-reason) "unknown reason")))
         ('off "ready; sandbox disabled")
         (_ "ready")))
      ('blocked
       (format
        "%s%s; %s"
        (pcase reason
          ('missing-dependencies
           (concat
            (format "missing required target programs: %s"
                    (mapconcat #'symbol-name
                               (plist-get readiness :missing-dependencies)
                               ", "))
            (when-let* ((incompatible
                         (plist-get readiness :incompatible-dependencies)))
              (format "; incompatible target programs: %s"
                      (mapconcat #'symbol-name incompatible ", ")))))
          ('incompatible-dependencies
           (format "incompatible required target programs: %s"
                   (mapconcat
                    #'symbol-name
                    (plist-get readiness :incompatible-dependencies)
                    ", ")))
          ('unsupported-operating-system
           (format "unsupported target operating system: %s (Linux required)"
                   (or (plist-get readiness :operating-system) "unknown")))
          ('unsupported-target "unsupported TRAMP target method")
          ('missing-environment
           (or (plist-get readiness :error)
               "target environment is incomplete"))
          ('probe-failed
           (format "target probe failed: %s"
                   (or (plist-get readiness :error) "unknown error")))
          ('sandbox-unavailable
           (format "required target sandbox unavailable: %s"
                   (or (plist-get readiness :sandbox-reason)
                       "unknown reason")))
          (_ "target readiness blocked"))
        (if-let* ((hint (plist-get readiness :install-hint)))
            (format "; install them with: %s" hint)
          "")
        retry))
      (_ (format "not probed; %s" retry)))))

(provide 'niwa-execution-target)

;;; niwa-execution-target.el ends here
