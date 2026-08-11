;;; mevedel-execution-target.el -- Workspace execution targets -*- lexical-binding: t -*-

;;; Commentary:

;; Derives the one filesystem and command authority owned by a session.
;; Callers use target-native paths for model-facing data and qualified paths
;; for Emacs filesystem operations.

;;; Code:

(eval-when-compile (require 'cl-lib))

;; `mevedel-sandbox'
(declare-function mevedel-sandbox-invalidate-probe-cache
                  "mevedel-sandbox" (&optional workdir))
(declare-function mevedel-sandbox-probe "mevedel-sandbox" (&optional workdir))

;; `tramp'
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-get-connection-process "tramp" (vec))

(define-error 'mevedel-execution-target-error "Execution target error")

(defconst mevedel-execution-target--hard-dependencies '(rg bash setsid)
  "Programs required for remote workspace execution.")

(defconst mevedel-execution-target--optional-capabilities
  '(bwrap gio inotifywait)
  "Programs whose availability enables optional target workflows.")

(defconst mevedel-execution-target--probe-timeout 10
  "Maximum seconds spent in one readiness probe.")

(cl-defstruct (mevedel-execution-target
               (:constructor mevedel-execution-target--create)
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

(defun mevedel-execution-target--prefix (path)
  "Return PATH's complete TRAMP prefix, including any hop."
  (when-let* ((native (file-remote-p path 'localname 'never)))
    (unless (string-suffix-p native path)
      (error "Cannot identify execution target for path: %s" path))
    (substring path 0 (- (length path) (length native)))))

(defun mevedel-execution-target--support-tier (method hop)
  "Return support tier for TRAMP METHOD and HOP."
  (cond
   (hop 'experimental)
   ((memq method '(ssh docker podman)) 'supported)
   ((eq method 'kubernetes) 'experimental)
   (method 'unsupported)
   (t 'supported)))

(defun mevedel-execution-target--identity (root method hop)
  "Return a structured identity for ROOT, METHOD, and HOP."
  (if (null method)
      '(:method local)
    (let ((user (file-remote-p root 'user 'never))
          (host (file-remote-p root 'host 'never)))
      (append (list :method method)
              (and user (list :user user))
              (and host (list :host host))
              (and hop (list :hop hop))))))

(defun mevedel-execution-target--parse-environment-entry (entry)
  "Parse one NAME=VALUE environment ENTRY."
  (when-let* ((separator (string-search "=" entry)))
    (cons (substring entry 0 separator)
          (substring entry (1+ separator)))))

(defun mevedel-execution-target-create (workspace-root)
  "Derive an execution target from WORKSPACE-ROOT.

This is side-effect free and never opens a TRAMP connection."
  (unless (and (stringp workspace-root)
               (file-name-absolute-p workspace-root))
    (error "Workspace root must be an absolute path"))
  (let* ((prefix (mevedel-execution-target--prefix workspace-root))
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
    (mevedel-execution-target--create
     :workspace-root root
     :native-root native-root
     :prefix prefix
     :method method
     :identity (mevedel-execution-target--identity
                workspace-root method hop)
     :support-tier (mevedel-execution-target--support-tier method hop)
     :environment
     (unless prefix
       (delq nil
             (mapcar #'mevedel-execution-target--parse-environment-entry
                     process-environment))))))

(defun mevedel-execution-target-remote-p (target)
  "Return non-nil when TARGET is remote through TRAMP."
  (and (mevedel-execution-target-prefix target) t))

(defun mevedel-execution-target-supported-p (target)
  "Return non-nil when TARGET has the supported tier."
  (eq 'supported (mevedel-execution-target-support-tier target)))

(defun mevedel-execution-target-label (target)
  "Return a compact user-facing identity for TARGET."
  (let* ((identity (mevedel-execution-target-identity target))
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

(defun mevedel-execution-target-native-path (target path)
  "Return PATH in TARGET's native path domain.

Signal `mevedel-execution-target-error' when PATH explicitly names a
different execution target."
  (unless (stringp path)
    (signal 'mevedel-execution-target-error
            (list "Path must be a string")))
  (if-let* ((path-prefix (mevedel-execution-target--prefix path)))
      (if (equal path-prefix (mevedel-execution-target-prefix target))
          (file-remote-p path 'localname 'never)
        (signal 'mevedel-execution-target-error
                (list (format "Path names another execution target: %s"
                              path))))
    path))

(defun mevedel-execution-target--environment-value (target name)
  "Return NAME from TARGET's probed environment or signal an error."
  (if-let* ((entry (assoc name
                          (mevedel-execution-target-environment target))))
      (cdr entry)
    (signal 'mevedel-execution-target-error
            (list (format "Unknown target environment variable: %s" name)))))

(defun mevedel-execution-target--expand-path-text (target path)
  "Expand TARGET environment and home syntax in native PATH."
  (let ((expanded
         (replace-regexp-in-string
          "\\$\\(?:{\\([[:alpha:]_][[:alnum:]_]*\\)}\\|\\([[:alpha:]_][[:alnum:]_]*\\)\\)"
          (lambda (reference)
            (mevedel-execution-target--environment-value
             target
             (if (string-prefix-p "${" reference)
                 (substring reference 2 -1)
               (substring reference 1))))
          path t t)))
    (cond
     ((equal expanded "~")
      (mevedel-execution-target--environment-value target "HOME"))
     ((string-prefix-p "~/" expanded)
      (file-name-concat
       (mevedel-execution-target--environment-value target "HOME")
       (substring expanded 2)))
     ((and (mevedel-execution-target-remote-p target)
           (string-prefix-p "~" expanded))
      (signal 'mevedel-execution-target-error
              (list (format "Unsupported target home path: %s" path))))
     (t expanded))))

(defun mevedel-execution-target-expand-path (target path &optional directory)
  "Return PATH expanded and qualified for TARGET.

Relative PATH resolves against DIRECTORY or TARGET's workspace root.
Environment variables and `~' resolve from TARGET's probed environment.
DIRECTORY may be target-native or already qualified for TARGET."
  (let* ((native-path
          (mevedel-execution-target--expand-path-text
           target (mevedel-execution-target-native-path target path)))
         (native-directory
          (mevedel-execution-target--expand-path-text
           target
           (mevedel-execution-target-native-path
            target
            (or directory (mevedel-execution-target-native-root target)))))
         (native-directory
          (if (file-name-absolute-p native-directory)
              native-directory
            (expand-file-name
             native-directory
             (mevedel-execution-target-native-root target))))
         (resolved (expand-file-name native-path native-directory)))
    (concat (mevedel-execution-target-prefix target) resolved)))

(defun mevedel-execution-target--process-output (target program &rest args)
  "Run fixed PROGRAM ARGS on TARGET and return its output."
  (let ((default-directory
         (mevedel-execution-target-workspace-root target))
        ;; TRAMP starts from the target shell environment.  Sending the
        ;; client's environment would leak client state, while local targets
        ;; must retain their ordinary process environment.
        (process-environment
         (unless (mevedel-execution-target-remote-p target)
           process-environment)))
    (with-temp-buffer
      (let ((status (apply #'process-file program nil (current-buffer) nil
                           args)))
        (unless (and (integerp status) (zerop status))
          (error "Target probe command failed: %s" program))
        (buffer-string)))))

(defun mevedel-execution-target--live-connection (target)
  "Return TARGET's live TRAMP connection process, or nil."
  (when (mevedel-execution-target-remote-p target)
    (require 'tramp)
    (let ((process
           (tramp-get-connection-process
            (tramp-dissect-file-name
             (mevedel-execution-target-workspace-root target)))))
      (and (process-live-p process) process))))

(defun mevedel-execution-target--probe-incarnation (target bash)
  "Return TARGET's Linux/container incarnation fingerprint using BASH."
  (let ((output
         (mevedel-execution-target--process-output
          target bash "-c"
          (concat
           "IFS= read -r boot </proc/sys/kernel/random/boot_id || exit 70; "
           "machine=; "
           "if test -r /etc/machine-id; then "
           "IFS= read -r machine </etc/machine-id || true; fi; "
           "IFS= read -r line </proc/1/stat || exit 71; "
           "rest=${line##*) }; set -- $rest; start=${20}; "
           "test -n \"$boot\" && test -n \"$start\" || exit 72; "
           "printf '%s\\n%s\\n%s\\n' \"$boot\" \"$machine\" \"$start\""))))
    (when (string-empty-p output)
      (error "Target incarnation probe returned no identity"))
    (secure-hash 'sha256 output)))

(defun mevedel-execution-target--record-incarnation (target incarnation)
  "Stage observed INCARNATION on TARGET and detect replacement.

The acknowledged `incarnation' remains unchanged while a replacement is
pending, so incidental persistence cannot pair the new target identity with
authority granted to the old target."
  (let ((baseline (mevedel-execution-target-incarnation target)))
    (cond
     ((null baseline)
      (setf (mevedel-execution-target-incarnation target) incarnation
            (mevedel-execution-target-observed-incarnation target) nil
            (mevedel-execution-target-incarnation-changed-p target) nil))
     ((mevedel-execution-target-incarnation-changed-p target)
      (setf (mevedel-execution-target-observed-incarnation target)
            incarnation))
     ((not (equal baseline incarnation))
      (setf (mevedel-execution-target-observed-incarnation target)
            incarnation
            (mevedel-execution-target-incarnation-changed-p target) t))))
  target)

(defun mevedel-execution-target-seed-incarnation (target incarnation)
  "Seed TARGET with persisted INCARNATION without detecting replacement."
  (unless (and (stringp incarnation) (not (string-blank-p incarnation)))
    (error "Invalid target incarnation: %S" incarnation))
  (setf (mevedel-execution-target-incarnation target) incarnation
        (mevedel-execution-target-observed-incarnation target) nil
        (mevedel-execution-target-incarnation-changed-p target) nil)
  target)

(defun mevedel-execution-target-prepare-incarnation-acknowledgement (target)
  "Make TARGET's staged incarnation serializable without acknowledging it.

Call this only after revoking exact authority associated with the previous
incarnation.  `incarnation-changed-p' remains non-nil until the resulting
sidecar publication commits."
  (let ((observed (mevedel-execution-target-observed-incarnation target)))
    (unless (and (mevedel-execution-target-incarnation-changed-p target)
                 (stringp observed)
                 (not (string-blank-p observed)))
      (error "Target incarnation replacement is not staged"))
    (setf (mevedel-execution-target-incarnation target) observed))
  target)

(defun mevedel-execution-target-acknowledge-incarnation (target)
  "Acknowledge TARGET's observed incarnation replacement."
  (when (mevedel-execution-target-incarnation-changed-p target)
    (unless (equal (mevedel-execution-target-incarnation target)
                   (mevedel-execution-target-observed-incarnation target))
      (error "Target incarnation replacement is not prepared"))
    (setf (mevedel-execution-target-observed-incarnation target) nil
          (mevedel-execution-target-incarnation-changed-p target) nil))
  target)

(defun mevedel-execution-target--parse-environment (output)
  "Parse NUL-delimited env OUTPUT into an alist."
  (delq nil
        (mapcar #'mevedel-execution-target--parse-environment-entry
                (split-string output "\0" t))))

(defun mevedel-execution-target--probe-capabilities (target)
  "Return TARGET's available program capabilities as an alist."
  (let ((default-directory
         (mevedel-execution-target-workspace-root target))
        (remote (mevedel-execution-target-prefix target))
        capabilities)
    (dolist (name (append mevedel-execution-target--hard-dependencies
                          mevedel-execution-target--optional-capabilities))
      (when-let* ((path (executable-find (symbol-name name) remote)))
        (push (cons name
                    (mevedel-execution-target-native-path target path))
              capabilities)))
    (nreverse capabilities)))

(defun mevedel-execution-target--probe-dependency-behavior
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
                   (mevedel-execution-target--process-output
                    target (alist-get 'bash capabilities) "-c"
                    (concat
                     "set -eu; "
                     "root=$(mktemp -d \"${TMPDIR:-/tmp}/mevedel-rg.XXXXXX\"); "
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
                    "mevedel-rg-probe" program))
                  ('bash
                   (mevedel-execution-target--process-output
                    target program "-c"
                    "test -n \"$BASH_VERSION\" && test -n \"$BASHPID\""))
                  ('setsid
                   (mevedel-execution-target--process-output
                    target program "-f" "-w" "bash" "-c" "exit 0")))
                nil)
            (error name)))))
    mevedel-execution-target--hard-dependencies)))

(defun mevedel-execution-target--apply-sandbox-readiness
    (target readiness sandbox-mode)
  "Return READINESS with TARGET's effective SANDBOX-MODE facts."
  (let ((readiness (copy-sequence readiness)))
    (when sandbox-mode
      (setq readiness (plist-put readiness :sandbox-mode sandbox-mode)))
    (when (and (mevedel-execution-target-remote-p target)
               (eq 'ready (plist-get readiness :status)))
      (pcase sandbox-mode
        ('off
         (setq readiness (plist-put readiness :sandbox-status 'off)))
        ((or 'best-effort 'required)
         (require 'mevedel-sandbox)
         (let ((availability
                (mevedel-sandbox-probe
                 (mevedel-execution-target-workspace-root target))))
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

(defun mevedel-execution-target-probe
    (target &optional refresh sandbox-mode)
  "Return TARGET readiness, probing its live connection when needed.

The fixed probe result is cached for TARGET's live TRAMP connection.  A
reconnect or non-nil REFRESH discards it and the matching Bubblewrap cache.
SANDBOX-MODE records the session's effective confinement requirement."
  (let* ((remote (mevedel-execution-target-remote-p target))
         (connection (and remote
                          (mevedel-execution-target--live-connection target)))
         (reconnected
          (and (mevedel-execution-target-readiness target)
               (not (eq connection
                        (mevedel-execution-target-connection-process target)))))
         (refresh (or refresh reconnected)))
    (when (and refresh remote)
      (require 'mevedel-sandbox)
      (mevedel-sandbox-invalidate-probe-cache
       (mevedel-execution-target-workspace-root target)))
    (if (and (not refresh)
             (mevedel-execution-target-readiness target)
             (eq sandbox-mode
                 (plist-get (mevedel-execution-target-readiness target)
                            :sandbox-mode)))
        (mevedel-execution-target-readiness target)
      (setf (mevedel-execution-target-environment target) nil
            (mevedel-execution-target-readiness target)
            (mevedel-execution-target--apply-sandbox-readiness
             target
             (if (eq 'unsupported
                     (mevedel-execution-target-support-tier target))
                 '(:status blocked :reason unsupported-target)
               (condition-case err
                   (with-timeout
                       (mevedel-execution-target--probe-timeout
                        (signal 'mevedel-execution-target-error
                                (list "Target readiness probe timed out")))
                     (let* ((environment
                             (mevedel-execution-target--parse-environment
                              (mevedel-execution-target--process-output
                               target "env" "-0")))
                            (operating-system
                             (string-trim
                              (mevedel-execution-target--process-output
                               target "uname" "-s")))
                            (capabilities
                             (mevedel-execution-target--probe-capabilities target))
                            (missing
                             (delq nil
                                   (mapcar
                                    (lambda (name)
                                      (unless (assq name capabilities) name))
                                    mevedel-execution-target--hard-dependencies)))
                            (incompatible
                             (mevedel-execution-target--probe-dependency-behavior
                              target capabilities))
                            (unsupported-os
                             (and (mevedel-execution-target-remote-p target)
                                  (not (equal operating-system "Linux"))))
                            (incarnation
                             (and remote
                                  (null missing)
                                  (null incompatible)
                                  (not unsupported-os)
                                  (mevedel-execution-target--probe-incarnation
                                   target (alist-get 'bash capabilities)))))
                       (setf (mevedel-execution-target-environment target)
                             environment)
                       (when incarnation
                         (mevedel-execution-target--record-incarnation
                          target incarnation))
                       (list :status (if (or missing incompatible unsupported-os)
                                         'blocked
                                       'ready)
                             :reason (cond
                                      (unsupported-os
                                       'unsupported-operating-system)
                                      (missing 'missing-dependencies)
                                      (incompatible 'incompatible-dependencies))
                             :operating-system operating-system
                             :capabilities capabilities
                             :missing-dependencies missing
                             :incompatible-dependencies incompatible
                             :incarnation incarnation)))
                 (error
                  (list :status 'blocked
                        :reason 'probe-failed
                        :error (error-message-string err)))))
             sandbox-mode))
      (when remote
        (setf (mevedel-execution-target-connection-process target)
              (mevedel-execution-target--live-connection target)))
      (mevedel-execution-target-readiness target))))

(defun mevedel-execution-target-ready-p (target)
  "Return non-nil when TARGET's last readiness probe succeeded."
  (eq 'ready
      (plist-get (mevedel-execution-target-readiness target) :status)))

(defun mevedel-execution-target-capability (target name)
  "Return executable path for capability NAME in TARGET's native domain."
  (cdr (assq name
             (plist-get (mevedel-execution-target-readiness target)
                        :capabilities))))

(defun mevedel-execution-target-missing-dependencies (target)
  "Return hard dependencies missing from TARGET's last probe."
  (plist-get (mevedel-execution-target-readiness target)
             :missing-dependencies))

(defun mevedel-execution-target-readiness-message (target)
  "Return a user-facing summary of TARGET's readiness."
  (let* ((readiness (mevedel-execution-target-readiness target))
         (reason (plist-get readiness :reason))
         (retry "run M-x mevedel-retry-target-readiness after fixing it"))
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
        "%s; %s"
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
          ('probe-failed
           (format "target probe failed: %s"
                   (or (plist-get readiness :error) "unknown error")))
          ('sandbox-unavailable
           (format "required target sandbox unavailable: %s"
                   (or (plist-get readiness :sandbox-reason)
                       "unknown reason")))
          (_ "target readiness blocked"))
        retry))
      (_ (format "not probed; %s" retry)))))

(provide 'mevedel-execution-target)

;;; mevedel-execution-target.el ends here
