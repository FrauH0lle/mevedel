;;; niwa-plugin-lifecycle.el -- Plugin Git lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Installs, updates, and removes niwa plugins through Git and the
;; plugin registry's validated manifest and durable state operations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'niwa-plugin-registry)

;; `niwa-plugin-registry'
(declare-function niwa-plugin-name
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-root
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugins-dir "niwa-plugin-registry" ())
(declare-function niwa-plugins-find
                  "niwa-plugin-registry" (name &optional workspace))
(declare-function niwa-plugins-list
                  "niwa-plugin-registry" (&optional workspace))
(declare-function niwa-plugins-forget-update
                  "niwa-plugin-registry" (root))
(declare-function niwa-plugins--update-entry-for-root
                  "niwa-plugin-registry" (root))
(declare-function niwa-plugins--update-mark-notified
                  "niwa-plugin-registry" (root))
(declare-function niwa-plugins--update-save
                  "niwa-plugin-registry" (state))
(declare-function niwa-plugins--update-set-entry
                  "niwa-plugin-registry" (root plist))
(declare-function niwa-plugins--update-state
                  "niwa-plugin-registry" ())
(declare-function niwa-plugins-hooks-stale-p
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-managed-roots
                  "niwa-plugin-registry" ())
(declare-function niwa-plugins-read-manifest
                  "niwa-plugin-registry" (root &optional workspace))
(declare-function niwa-plugins-remove-state-for-source
                  "niwa-plugin-registry"
                  (name root &optional workspace))
(declare-function niwa-plugins-source-root
                  "niwa-plugin-registry" (root &optional workspace))
(declare-function niwa-plugins-state-entry
                  "niwa-plugin-registry" (plugin &optional workspace))
(declare-function niwa-plugins-transfer-state-entry
                  "niwa-plugin-registry"
                  (old-name new-name plist &optional workspace))

;;; Git install/update

(defun niwa-plugins--process-git (directory args)
  "Run git ARGS in DIRECTORY.
Return (STATUS OUTPUT)."
  (let ((default-directory directory))
    (with-temp-buffer
      (list (apply #'process-file "git" nil (list (current-buffer) t) nil args)
            (string-trim (buffer-string))))))

(defvar niwa-plugins-git-executor #'niwa-plugins--process-git
  "Function called to run git.
It receives DIRECTORY and ARGS, and returns (STATUS OUTPUT).")

(defun niwa-plugins--github-target-p (target)
  "Return non-nil when TARGET is a supported OWNER/REPO GitHub target."
  (and (stringp target)
       (string-match
        "\\`\\([A-Za-z0-9_.-]+\\)/\\([A-Za-z0-9_.-]+\\)\\'" target)
       (not (member (match-string 1 target) '("." "..")))
       (not (member (match-string 2 target) '("." "..")))))

(defun niwa-plugins--git (directory args)
  "Run git ARGS in DIRECTORY through `niwa-plugins-git-executor'."
  (condition-case err
      (let ((result (funcall niwa-plugins-git-executor directory args)))
        (if (and (consp result) (integerp (car result)))
            result
          (list 1 (format "%s" result))))
    (error (list 1 (error-message-string err)))))

(defun niwa-plugins--make-staging-directory (dest)
  "Create and return a staging directory for a plugin clone bound for DEST.
A sibling of DEST, so publishing it is a same-filesystem rename, and
uniquely named, so two concurrent installs of one repository cannot clone
into each other's staging tree or delete it on the way out."
  (let ((staging
         (make-temp-name
          (expand-file-name niwa-plugins-staging-prefix
                            (file-name-directory (directory-file-name dest))))))
    ;; Not `make-temp-file': its 0700 would become the published plugin's
    ;; mode.  Creating the name here also fails if it somehow exists.
    (make-directory staging)
    staging))

(defun niwa-plugins-install (target)
  "Install GitHub plugin TARGET."
  (if (not (niwa-plugins--github-target-p target))
      "Invalid plugin target: use OWNER/REPO or a GitHub repository."
    (let* ((dest (file-name-concat (niwa-plugins-dir) "github.com" target))
           (present (file-directory-p dest)))
      (if present
          (if-let* ((plugin (niwa-plugins-read-manifest dest)))
              (let ((name (niwa-plugin-name plugin)))
                (format
                 "Plugin %s is already installed; use /plugin update %s."
                 name name))
            (format
             (concat "Plugin path %s already exists, but no Codex plugin "
                     "manifest was found; fix or remove it before "
                     "installing %s.")
             dest target))
        (make-directory (file-name-directory dest) t)
        ;; Clone into a staging sibling and publish only what validates:
        ;; the destination is the discovery tree, so a rejected clone left
        ;; there is enableable, unremovable through the cockpit, and turns
        ;; every retry into the already-exists dead end.
        (let ((staging (niwa-plugins--make-staging-directory dest)))
          (unwind-protect
              (pcase-let ((`(,status ,output)
                           (niwa-plugins--git
                            (niwa-plugins-dir)
                            (list "clone" "--depth" "1"
                                  (format "https://github.com/%s.git" target)
                                  staging))))
                (let ((plugin (and (zerop status)
                                   (niwa-plugins-read-manifest staging))))
                  (cond
                   ((not (zerop status))
                    (format "Failed to install plugin %s: %s"
                            target
                            (if (string-empty-p output) "git failed" output)))
                   ((not plugin)
                    (format
                     (concat "Failed to install plugin %s: no Codex plugin "
                             "manifest found.")
                     target))
                   ;; The already-installed guard ran before the clone, and
                   ;; it only tests for a directory.
                   ((file-exists-p dest)
                    (format
                     (concat "Plugin path %s already exists; remove it "
                             "before installing %s.")
                     dest target))
                   (t
                    (rename-file staging dest)
                    (format "Installed plugin %s."
                            (niwa-plugin-name plugin))))))
            (when (file-directory-p staging)
              (delete-directory staging t))))))))

(defun niwa-plugins-update (name &optional workspace)
  "Update installed plugin NAME with git pull.
Preserve plugin state in WORKSPACE."
  (if-let* ((plugin (niwa-plugins-find name workspace)))
      (let* ((root (niwa-plugin-root plugin))
             (previous-state (niwa-plugins-state-entry
                              plugin workspace)))
        (if (not (niwa-plugins--managed-root-p root))
            (format "Plugin %s is not managed by niwa; update %s manually."
                    name root)
          (pcase-let* ((`(,status ,output)
                        (niwa-plugins--git root '("pull" "--ff-only"))))
            (if (not (zerop status))
                (format "Failed to update plugin %s: %s"
                        name
                        (if (string-empty-p output) "git failed" output))
              (if-let* ((updated (niwa-plugins-read-manifest
                                  root workspace)))
                  (let ((updated-name (niwa-plugin-name updated)))
                    (niwa-plugins-forget-update root)
                    (niwa-plugins-transfer-state-entry
                     (niwa-plugin-name plugin)
                     updated-name
                     previous-state
                     workspace)
                    (if (niwa-plugins-hooks-stale-p updated workspace)
                        (format (concat "Updated plugin %s. "
                                        "Hook consent is pending; "
                                        "open /plugin to review.")
                                updated-name)
                      (format "Updated plugin %s." updated-name)))
                (format (concat "Failed to update plugin %s: "
                                "no Codex plugin manifest found.")
                        name))))))
    (format "Unknown plugin: %s." name)))

(defun niwa-plugins--managed-root-p (root)
  "Return non-nil when ROOT is below a niwa-managed global root."
  (let ((root (niwa-plugins-source-root root)))
    (cl-some
     (lambda (managed)
       (and (not (equal root managed))
            (string-prefix-p managed root)))
     (niwa-plugins-managed-roots))))


;;; Update check

(defconst niwa-plugins--update-check-timeout 60
  "Seconds before a hung `git' call of the update check is killed.")

(defconst niwa-plugins--update-check-environment
  '("GIT_TERMINAL_PROMPT=0" "GIT_ASKPASS=false"
    "SSH_ASKPASS_REQUIRE=never" "GCM_INTERACTIVE=never")
  "Environment entries that keep an unattended update check from prompting.")

(defun niwa-plugins--process-git-async (directory args callback)
  "Run git ARGS in DIRECTORY without blocking; call CALLBACK once.
CALLBACK receives (STATUS OUTPUT), like the synchronous executor returns.
The environment and arguments switch off git's terminal prompt, askpass
programs, ssh askpass and Git Credential Manager's interactive mode, so an
unattended check should not raise a credential dialog.  A call that outlives
`niwa-plugins--update-check-timeout' is killed and reported failed."
  (let* ((default-directory directory)
         (process-environment
          (append niwa-plugins--update-check-environment
                  process-environment))
         (buffer (generate-new-buffer " *niwa-plugin-git*" t))
         ;; Diagnostics (GIT_TRACE, warnings) must not mix into the
         ;; stdout that callers parse, so stderr gets its own buffer.
         (stderr (generate-new-buffer " *niwa-plugin-git-stderr*" t))
         (process (make-process
                   :name "niwa-plugin-git" :buffer buffer
                   :stderr stderr
                   :command (append '("git" "-c" "credential.interactive=never")
                                    args)
                   :connection-type 'pipe :noquery t))
         (timer (run-at-time niwa-plugins--update-check-timeout nil
                             #'delete-process process)))
    (set-process-sentinel
     process
     (lambda (process _event)
       (unless (process-live-p process)
         (cancel-timer timer)
         (let ((output (with-current-buffer buffer
                         (string-trim (buffer-string)))))
           (kill-buffer buffer)
           (kill-buffer stderr)
           (funcall callback (process-exit-status process) output)))))))

(defvar niwa-plugins-async-git-executor #'niwa-plugins--process-git-async
  "Function called to run git for the update check without blocking.
It receives DIRECTORY, ARGS and CALLBACK, and later calls CALLBACK once
with (STATUS OUTPUT).")

(defun niwa-plugins--git-async (directory args callback)
  "Run git ARGS in DIRECTORY through `niwa-plugins-async-git-executor'.
A failure to start git reaches CALLBACK as a failed result.  If CALLBACK
signals, the check is no longer running, so later checks can start."
  (let* ((called nil)
         (wrapped
          (lambda (&rest result)
            (setq called t)
            (let ((ok nil))
              (unwind-protect
                  (prog1 (apply callback result) (setq ok t))
                (unless ok
                  (setq niwa-plugins--update-check-busy nil)))))))
    (condition-case err
        (funcall niwa-plugins-async-git-executor directory args wrapped)
      (error
       (if called
           (signal (car err) (cdr err))
         (funcall wrapped 1 (error-message-string err)))))))

(defun niwa-plugins--update-targets ()
  "Return roots of installed managed plugins that are Git clones."
  (cl-loop for plugin in (niwa-plugins-list)
           for root = (niwa-plugin-root plugin)
           when (and (niwa-plugins--managed-root-p root)
                     (file-exists-p (expand-file-name ".git" root)))
           collect root))

(defun niwa-plugins--object-id-p (string)
  "Return non-nil when STRING is a full SHA-1 or SHA-256 object id."
  (and (stringp string)
       (string-match-p "\\`\\(?:[0-9a-f]\\{40\\}\\|[0-9a-f]\\{64\\}\\)\\'" string)))

(defun niwa-plugins--check-root (root done)
  "Compare ROOT's origin HEAD with its local HEAD, cache it, call DONE."
  ;; Only a fixed reason is stored: git's own output can carry credential
  ;; URLs, and the state file is plain text.
  (cl-flet ((fail (reason)
              (niwa-plugins--update-set-entry root (list :error reason))
              (funcall done)))
    (niwa-plugins--git-async
     root '("ls-remote" "origin" "HEAD")
     (lambda (status output)
       (let ((remote (car (split-string output "[ \t\n]" t))))
         (if (or (not (zerop status))
                 (not (niwa-plugins--object-id-p remote)))
             (fail (if (zerop status)
                       "unexpected git output"
                     (format "git exited with status %s" status)))
           (niwa-plugins--git-async
            root '("rev-parse" "HEAD")
            (lambda (status output)
              (if (or (not (zerop status))
                      (not (niwa-plugins--object-id-p output)))
                  (fail (if (zerop status)
                            "unexpected git output"
                          (format "git exited with status %s" status)))
                (niwa-plugins--update-set-entry
                 root (list :local output :remote remote))
                (funcall done))))))))))

(defun niwa-plugins--finish-update-check (on-demand targets done)
  "Announce what the check of TARGETS found, then call DONE.
ON-DEMAND is non-nil for a user-requested check.
Each upstream commit is announced once: automatic checks stay silent for
an already announced commit, and report only plugins newly seen."
  (setq niwa-plugins--update-check-busy nil)
  (let (fresh available)
    (dolist (root targets)
      (let ((entry (niwa-plugins--update-entry-for-root root)))
        (when (and entry (not (plist-get entry :error))
                   (not (equal (plist-get entry :remote)
                               (plist-get entry :local))))
          (push root available)
          (unless (equal (plist-get entry :notified)
                         (plist-get entry :remote))
            (push root fresh)))))
    (cond
     (on-demand
      (message "niwa: checked %d plugin%s, %d with updates available"
               (length targets) (if (= (length targets) 1) "" "s")
               (length available)))
     (fresh
      (message "niwa: updates available for %d plugin%s; /plugin to review"
               (length fresh) (if (= (length fresh) 1) "" "s"))))
    (mapc #'niwa-plugins--update-mark-notified
          (if on-demand available fresh))
    (when done (funcall done))))

(defun niwa-plugins-check-updates (&optional on-demand done)
  "Check managed plugins for upstream commits without blocking Emacs.
Run `git ls-remote origin HEAD' for one plugin at a time and cache the
comparison with local HEAD.  Return nil immediately; a check already
running makes this a no-op.  ON-DEMAND says a user asked, which reports
the outcome even when nothing is new.  DONE, when non-nil, is called with
no arguments after the results are cached."
  (unless niwa-plugins--update-check-busy
    (setq niwa-plugins--update-check-busy t)
    (condition-case err
        (let* ((targets (niwa-plugins--update-targets))
               (queue (copy-sequence targets)))
          (niwa-plugins--update-save
           (plist-put (copy-tree (niwa-plugins--update-state))
                      :last-check (float-time)))
          (cl-labels
              ((next ()
                 (if queue
                     (niwa-plugins--check-root (pop queue) #'next)
                   (niwa-plugins--finish-update-check
                    on-demand targets done))))
            (next)))
      (error
       (setq niwa-plugins--update-check-busy nil)
       (signal (car err) (cdr err)))))
  nil)

(defun niwa-plugins-remove (name &optional workspace)
  "Remove managed installed plugin NAME.
Workspace runtime data is retained."
  (if-let* ((plugin (niwa-plugins-find name workspace)))
      (let ((root (niwa-plugin-root plugin)))
        (cond
         ((not (niwa-plugins--managed-root-p root))
          (format "Plugin %s is not managed by niwa; remove %s manually."
                  name root))
         ((not (yes-or-no-p
                (format "Remove plugin %s? Runtime data will be kept. "
                        name)))
          (format "Remove cancelled for plugin %s." name))
         (t
          (delete-directory root t)
          (niwa-plugins-remove-state-for-source
           name root workspace)
          (format "Removed plugin %s." name))))
    (format "Unknown plugin: %s." name)))

(provide 'niwa-plugin-lifecycle)
;;; niwa-plugin-lifecycle.el ends here
