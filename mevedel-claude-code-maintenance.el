;;; mevedel-claude-code-maintenance.el --- Checked Claude runtime updates -*- lexical-binding: t -*-
;;; Commentary:
;; Native Claude owns its installer.  Adapter versions are staged separately;
;; checked runtime paths are selected only for new invocations.  Conversation
;; storage and active processes never move with an update.  Reading the
;; published state is cheap and side-effect free, also without Claude.  The
;; bounded status-command runner is shared with launch preflight.
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'mevedel-recovery)
(require 'mevedel-acp)
(require 'mevedel-claude-code-backend)

;; `mevedel-transport'
(autoload 'mevedel-transport-run-at-time "mevedel-transport")
(declare-function mevedel-transport-run-at-time "mevedel-transport" (seconds function &rest args))

(defcustom mevedel-claude-code-auto-update t
  "Maintain the native Claude CLI and managed ACP adapter automatically."
  :type 'boolean :group 'mevedel)
(defvar mevedel-claude-code-maintenance--jobs (make-hash-table :test #'equal))
(defvar mevedel-claude-code-maintenance-changed-hook nil)
(autoload 'mevedel-readiness-runtime-changed "mevedel-readiness")
(add-hook 'mevedel-claude-code-maintenance-changed-hook #'mevedel-readiness-runtime-changed)
(defvar mevedel-claude-code-maintenance--timer nil)

(defun mevedel-claude-code--command-output-async (command args ready failure &optional any-exit timeout)
  "Run status COMMAND with ARGS asynchronously; return its canceller.
READY receives stdout on success, or on every exit with ANY-EXIT.  FAILURE
receives a safe diagnostic.  TIMEOUT defaults to 10 seconds.  Cancel and
timeout release the child and both private output buffers."
  (let ((check (mapconcat #'shell-quote-argument (cons (file-name-nondirectory command) args) " "))
        (stdout (generate-new-buffer " *claude-status*"))
        (stderr (generate-new-buffer " *claude-status-errors*"))
        process timer finished)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (when process
             (set-process-sentinel process #'ignore)
             (when (process-live-p process) (delete-process process)))
           (when (buffer-live-p stdout) (kill-buffer stdout))
           (when (buffer-live-p stderr) (kill-buffer stderr)))
         (cancel () (setq finished t) (cleanup))
         (fail (message)
           (unless finished
             (setq finished t)
             (cleanup)
             (funcall failure message)))
         (exited (child _event)
           (when (and (not finished) (memq (process-status child) '(exit signal)))
             (if (not (or any-exit (zerop (process-exit-status child))))
                 (fail (format "Setup check failed: %s; run it in a terminal" check))
               (let ((output (with-current-buffer stdout (string-trim (buffer-string)))))
                 (setq finished t)
                 (cleanup)
                 (funcall ready output))))))
      (condition-case nil
          (progn
            (setq timer (mevedel-transport-run-at-time
                         (or timeout 10) (lambda () (fail (format "Setup check timed out: %s; run it in a terminal" check)))))
            (setq process (make-process :name "mevedel-claude-status"
                                        :command (cons command args)
                                        :buffer stdout :stderr stderr
                                        :connection-type 'pipe :noquery t
                                        :coding 'utf-8-unix :sentinel #'exited))
            (when finished (cleanup)))
        (error (fail (format "Setup check failed: %s; run it in a terminal" check))))
      #'cancel)))

(defun mevedel-claude-code-maintenance-state ()
  "Read the configured installation's last checked runtime and safe status."
  (let ((file (file-name-concat mevedel-claude-code-directory "runtime.json")))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((state (json-parse-buffer :object-type 'plist :null-object nil :false-object nil)))
              (when (and (listp state)
                         (equal (plist-get state :configuredCli) mevedel-claude-code-executable)
                         (equal (plist-get state :configuredAdapter) mevedel-claude-code-adapter-executable)
                         (member (plist-get state :status) '("ready" "checking" "failed"))
                         (or (null (plist-get state :checked)) (numberp (plist-get state :checked)))
                         (cl-every (lambda (key) (let ((value (plist-get state key)))
                                                   (or (null value) (stringp value))))
                                   '(:cli :adapter :previousCli :previousAdapter :rejectedCli :rejectedAdapter :message)))
                state)))
        (error nil)))))

(defun mevedel-claude-code-maintenance--write (state)
  "Atomically publish checked installation STATE without changing conversations."
  (setq state (plist-put state :configuredCli mevedel-claude-code-executable)
        state (plist-put state :configuredAdapter mevedel-claude-code-adapter-executable))
  (let* ((directory (expand-file-name mevedel-claude-code-directory))
         (file (file-name-concat directory "runtime.json"))
         (temporary (make-temp-file (file-name-concat directory ".runtime-"))))
    (unwind-protect
        (progn
          (with-temp-file temporary (insert (json-serialize state :null-object nil)))
          (set-file-modes temporary #o600)
          (rename-file temporary file t))
      (when (file-exists-p temporary) (delete-file temporary)))
    (when (equal "ready" (plist-get state :status))
      (condition-case nil (mevedel-claude-code-maintenance--prune state) (file-error nil)))
    (run-hook-wrapped 'mevedel-claude-code-maintenance-changed-hook
                      (lambda (observer)
                        (condition-case nil (funcall observer state) (error nil)) nil))))

(defun mevedel-claude-code-maintenance--prune (state)
  "Remove obsolete managed versions, preserving STATE and active launches."
  (let ((keep (mapcar (lambda (key) (plist-get state key))
                      '(:cli :adapter :previousCli :previousAdapter :rejectedCli :rejectedAdapter))))
    (dolist (process (process-list))
      (when (process-live-p process)
        (let ((launch (process-get process 'mevedel-acp-launch)))
          (push (plist-get launch :command) keep)
          (dolist (entry (plist-get launch :environment))
            (when (string-prefix-p "CLAUDE_CODE_EXECUTABLE=" entry)
              (push (substring entry (length "CLAUDE_CODE_EXECUTABLE=")) keep))))))
    (dolist (kind '("executables" "runtimes"))
      (let ((root (file-name-concat mevedel-claude-code-directory kind)))
        (when (file-directory-p root)
          (dolist (path (directory-files root t "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'"))
            (unless (cl-some (lambda (used)
                               (and (stringp used)
                                    (or (equal used path) (file-in-directory-p used path)))) keep)
              (if (file-directory-p path) (delete-directory path t) (delete-file path)))))))))

(defun mevedel-claude-code-maintenance--pin (cli directory)
  "Keep CLI available under DIRECTORY if the native installer prunes versions."
  (let* ((source (file-truename cli))
         (target (file-name-concat directory "executables" (file-name-nondirectory source))))
    (make-directory (file-name-directory target) t)
    (unless (file-exists-p target)
      (condition-case nil (add-name-to-file source target)
        (file-error (copy-file source target))))
    target))

(defun mevedel-claude-code-maintenance-check (&optional force)
  "Check stable updates asynchronously when due, or now with FORCE.
Return current state.  Concurrent sessions and hosts share an installation
lock.  A forced check reports a running check and its result in the echo area."
  (when (and mevedel-claude-code-auto-update (not mevedel-claude-code-maintenance--timer))
    (setq mevedel-claude-code-maintenance--timer
          (run-at-time 3600 3600 #'mevedel-claude-code-maintenance-check)))
  (let* ((directory (expand-file-name mevedel-claude-code-directory))
         (lock (file-name-concat directory "runtime-update"))
         (state (mevedel-claude-code-maintenance-state))
         (last (or (plist-get state :checked) 0))
         (interval (if (equal (plist-get state :status) "failed") 3600 86400)))
    (when (and (or force mevedel-claude-code-auto-update)
               (not (file-remote-p directory))
               (or force (> (- (float-time) last) interval)))
      (if (or (gethash directory mevedel-claude-code-maintenance--jobs) (file-locked-p lock))
          (when force (message "mevedel: a Claude runtime update check is already running"))
        (make-directory directory t)
        ;; `lock-file' does nothing while `create-lockfiles' is nil.
        (let ((create-lockfiles t)) (lock-file lock))
        (when force (message "mevedel: checking Claude runtime updates..."))
        (let* ((cli (executable-find mevedel-claude-code-executable))
               (npm (executable-find "npm"))
               (old (copy-sequence state))
               (configured-cli mevedel-claude-code-executable)
               (configured-adapter mevedel-claude-code-adapter-executable)
               cancel-command timer connection stopped candidate adapter stage)
          (cl-labels
              ((clean ()
                 (when timer (cancel-timer timer))
                 (when connection (mevedel-acp-close connection))
                 (when cancel-command (funcall cancel-command))
                 (unlock-file lock)
                 (remhash directory mevedel-claude-code-maintenance--jobs))
               (cancel () (setq stopped t) (clean))
               (finish (message)
                 (unless stopped
                   (setq stopped t)
                   (clean)
                   (let ((mevedel-claude-code-directory directory)
                         (mevedel-claude-code-executable configured-cli)
                         (mevedel-claude-code-adapter-executable configured-adapter))
                     (mevedel-claude-code-maintenance--write
                      (append (list :status (if message "failed" "ready")
                                    :message (or message "Claude runtime is up to date")
                                    :checked (float-time)
                                    :cli (if message (plist-get old :cli) candidate)
                                    :adapter (if message (plist-get old :adapter) adapter)
                                    :previousCli (plist-get old :cli)
                                    :previousAdapter (plist-get old :adapter)
                                    :rejectedCli (and message candidate)
                                    :rejectedAdapter (and message adapter)) nil)))
                   (when force (message "mevedel: %s" (or message "Claude runtime is up to date")))))
               (run (command next)
                 (setq cancel-command
                       (let ((default-directory temporary-file-directory))
                         (mevedel-claude-code--command-output-async
                          (car command) (cdr command)
                          (lambda (output)
                            (condition-case nil
                                (funcall next output)
                              (error (finish "Runtime validation failed; the previous runtime remains selected"))))
                          (lambda (_)
                            (finish "Claude update failed; the previous runtime remains selected. Check the native installer and npm on the host."))
                          nil 300))))
               (validate-adapter ()
                 (if (and (not force) (equal candidate (plist-get old :rejectedCli))
                          (equal adapter (plist-get old :rejectedAdapter)))
                     (finish "This runtime was rejected previously; waiting for a newer release or an explicit update check")
                   (run (list adapter "--version")
                        (lambda (output)
                          (if (not (mevedel-claude-code--version output mevedel-claude-code--adapter-version))
                              (finish "Adapter version is unsupported; the previous runtime remains selected")
                            (let ((process-environment (copy-sequence process-environment)))
                              (setenv "CLAUDE_CODE_EXECUTABLE" candidate)
                              (setq connection
                                    (mevedel-acp-open
                                     (list :command adapter :cwd directory
                                           :environment process-environment :initialize-only t)
                                     (lambda (checked)
                                       (if (eq t (alist-get 'loadSession (mevedel-acp-capabilities checked)))
                                           (finish nil)
                                         (finish "Adapter cannot retain conversations; previous runtime remains selected")))
                                     (lambda (_message)
                                       (finish "Adapter protocol validation failed; previous runtime remains selected"))))))))))
               (check-cli (next)
                 (run (list candidate "--version")
                      (lambda (output)
                        (unless (mevedel-claude-code--version output mevedel-claude-code--cli-version)
                          (error "Unsupported Claude version"))
                        (funcall next))))
               (install-adapter ()
                 (if configured-adapter
                     (progn (setq adapter (executable-find configured-adapter))
                            (if adapter (validate-adapter)
                              (finish "The custom ACP adapter is missing; update its externally managed installation")))
                   (if (not npm) (finish "Install Node.js 22 and npm on the host to update the managed adapter")
                     (run (list npm "view" "@agentclientprotocol/claude-agent-acp" "dist-tags.latest" "--json")
                          (lambda (output)
                            (let ((version (json-parse-string output)))
                              (unless (and (stringp version)
                                           (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" version))
                                (error "Invalid stable adapter version"))
                              (setq stage (file-name-concat directory "runtimes" version)
                                    adapter (file-name-concat stage "node_modules" ".bin" "claude-agent-acp"))
                              (if (file-executable-p adapter) (validate-adapter)
                                (run (list npm "install" "--prefix" stage "--save-exact" "--no-audit" "--no-fund"
                                           (concat "@agentclientprotocol/claude-agent-acp@" version))
                                     (lambda (_) (validate-adapter)))))))))))
            (puthash directory #'cancel mevedel-claude-code-maintenance--jobs)
            (setq timer (mevedel-transport-run-at-time
                         300 (lambda () (finish "Claude update timed out; the previous runtime remains selected"))))
            (condition-case nil
                (cond
                 ((not cli) (finish "Install native Claude Code on the host before enabling automatic updates"))
                 ;; Native installer paths are versioned.  Do not replace a package-manager binary.
                 ((not (string-match-p "/claude/versions/" (file-truename cli)))
                  (setq candidate (file-truename cli))
                  (check-cli #'install-adapter))
                 (t
                  ;; Preserve the checked executable before the native installer can prune it.
                  (unless (plist-get old :cli)
                    (setq old (plist-put old :cli (mevedel-claude-code-maintenance--pin cli directory))))
                  (unless (plist-get old :adapter)
                    (setq old (plist-put old :adapter
                                         (or mevedel-claude-code-adapter-executable
                                             (executable-find "claude-agent-acp")))))
                  (let ((mevedel-claude-code-directory directory)
                        (mevedel-claude-code-executable configured-cli)
                        (mevedel-claude-code-adapter-executable configured-adapter))
                    (mevedel-claude-code-maintenance--write
                     (append (list :status "checking" :message "Checking stable Claude updates")
                             (cl-loop for (key value) on old by #'cddr
                                      unless (memq key '(:status :message)) append (list key value)))))
                  (run (list cli "install" "stable")
                       (lambda (_)
                         (setq candidate (mevedel-claude-code-maintenance--pin cli directory))
                         (check-cli #'install-adapter)))))
              (error (finish "Runtime update preparation failed; previous runtime remains selected")))))))
    state))

(defun mevedel-claude-code-maintenance-stop ()
  "Cancel every managed update and release its installation lock."
  (when mevedel-claude-code-maintenance--timer
    (cancel-timer mevedel-claude-code-maintenance--timer)
    (setq mevedel-claude-code-maintenance--timer nil))
  (let (jobs)
    (maphash (lambda (_directory cancel) (push cancel jobs)) mevedel-claude-code-maintenance--jobs)
    (mapc #'funcall jobs)))
(add-hook 'kill-emacs-hook #'mevedel-claude-code-maintenance-stop)

(provide 'mevedel-claude-code-maintenance)
;;; mevedel-claude-code-maintenance.el ends here
