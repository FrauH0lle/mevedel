;;; mevedel-plugin-lifecycle.el -- Plugin Git lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Installs, updates, and removes mevedel plugins through Git and the
;; plugin registry's validated manifest and durable state operations.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-plugin-registry)

;; `mevedel-plugin-registry'
(declare-function mevedel-plugin-name
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-root
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugins-dir "mevedel-plugin-registry" ())
(declare-function mevedel-plugins-find
                  "mevedel-plugin-registry" (name &optional workspace))
(declare-function mevedel-plugins-list
                  "mevedel-plugin-registry" (&optional workspace))
(declare-function mevedel-plugins-forget-update
                  "mevedel-plugin-registry" (root))
(declare-function mevedel-plugins--update-entry-for-root
                  "mevedel-plugin-registry" (root))
(declare-function mevedel-plugins--update-mark-notified
                  "mevedel-plugin-registry" (root))
(declare-function mevedel-plugins--update-save
                  "mevedel-plugin-registry" (state))
(declare-function mevedel-plugins--update-set-entry
                  "mevedel-plugin-registry" (root plist))
(declare-function mevedel-plugins--update-state
                  "mevedel-plugin-registry" ())
(declare-function mevedel-plugins-hooks-stale-p
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-managed-roots
                  "mevedel-plugin-registry" ())
(declare-function mevedel-plugins-read-manifest
                  "mevedel-plugin-registry" (root &optional workspace))
(declare-function mevedel-plugins-remove-state-for-source
                  "mevedel-plugin-registry"
                  (name root &optional workspace))
(declare-function mevedel-plugins-source-root
                  "mevedel-plugin-registry" (root &optional workspace))
(declare-function mevedel-plugins-state-entry
                  "mevedel-plugin-registry" (plugin &optional workspace))
(declare-function mevedel-plugins-transfer-state-entry
                  "mevedel-plugin-registry"
                  (old-name new-name plist &optional workspace))

;;; Git install/update

(defun mevedel-plugins--process-git (directory args)
  "Run git ARGS in DIRECTORY.
Return (STATUS OUTPUT)."
  (let ((default-directory directory))
    (with-temp-buffer
      (list (apply #'process-file "git" nil (list (current-buffer) t) nil args)
            (string-trim (buffer-string))))))

(defvar mevedel-plugins-git-executor #'mevedel-plugins--process-git
  "Function called to run git.
It receives DIRECTORY and ARGS, and returns (STATUS OUTPUT).")

(defun mevedel-plugins--github-target-p (target)
  "Return non-nil when TARGET is a supported OWNER/REPO GitHub target."
  (and (stringp target)
       (string-match
        "\\`\\([A-Za-z0-9_.-]+\\)/\\([A-Za-z0-9_.-]+\\)\\'" target)
       (not (member (match-string 1 target) '("." "..")))
       (not (member (match-string 2 target) '("." "..")))))

(defun mevedel-plugins--git (directory args)
  "Run git ARGS in DIRECTORY through `mevedel-plugins-git-executor'."
  (condition-case err
      (let ((result (funcall mevedel-plugins-git-executor directory args)))
        (if (and (consp result) (integerp (car result)))
            result
          (list 1 (format "%s" result))))
    (error (list 1 (error-message-string err)))))

(defun mevedel-plugins--make-staging-directory (dest)
  "Create and return a staging directory for a plugin clone bound for DEST.
A sibling of DEST, so publishing it is a same-filesystem rename, and
uniquely named, so two concurrent installs of one repository cannot clone
into each other's staging tree or delete it on the way out."
  (let ((staging
         (make-temp-name
          (expand-file-name mevedel-plugins-staging-prefix
                            (file-name-directory (directory-file-name dest))))))
    ;; Not `make-temp-file': its 0700 would become the published plugin's
    ;; mode.  Creating the name here also fails if it somehow exists.
    (make-directory staging)
    staging))

(defun mevedel-plugins-install (target)
  "Install GitHub plugin TARGET."
  (if (not (mevedel-plugins--github-target-p target))
      "Invalid plugin target: use OWNER/REPO or a GitHub repository."
    (let* ((dest (file-name-concat (mevedel-plugins-dir) "github.com" target))
           (present (file-directory-p dest)))
      (if present
          (if-let* ((plugin (mevedel-plugins-read-manifest dest)))
              (let ((name (mevedel-plugin-name plugin)))
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
        (let ((staging (mevedel-plugins--make-staging-directory dest)))
          (unwind-protect
              (pcase-let ((`(,status ,output)
                           (mevedel-plugins--git
                            (mevedel-plugins-dir)
                            (list "clone" "--depth" "1"
                                  (format "https://github.com/%s.git" target)
                                  staging))))
                (let ((plugin (and (zerop status)
                                   (mevedel-plugins-read-manifest staging))))
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
                            (mevedel-plugin-name plugin))))))
            (when (file-directory-p staging)
              (delete-directory staging t))))))))

(defun mevedel-plugins-update (name &optional workspace)
  "Update installed plugin NAME with git pull.
Preserve plugin state in WORKSPACE."
  (if-let* ((plugin (mevedel-plugins-find name workspace)))
      (let* ((root (mevedel-plugin-root plugin))
             (previous-state (mevedel-plugins-state-entry
                              plugin workspace)))
        (if (not (mevedel-plugins--managed-root-p root))
            (format "Plugin %s is not managed by mevedel; update %s manually."
                    name root)
          (pcase-let* ((`(,status ,output)
                        (mevedel-plugins--git root '("pull" "--ff-only"))))
            (if (not (zerop status))
                (format "Failed to update plugin %s: %s"
                        name
                        (if (string-empty-p output) "git failed" output))
              (if-let* ((updated (mevedel-plugins-read-manifest
                                  root workspace)))
                  (let ((updated-name (mevedel-plugin-name updated)))
                    (mevedel-plugins-forget-update root)
                    (mevedel-plugins-transfer-state-entry
                     (mevedel-plugin-name plugin)
                     updated-name
                     previous-state
                     workspace)
                    (if (mevedel-plugins-hooks-stale-p updated workspace)
                        (format (concat "Updated plugin %s. "
                                        "Hook consent is pending; "
                                        "open /plugin to review.")
                                updated-name)
                      (format "Updated plugin %s." updated-name)))
                (format (concat "Failed to update plugin %s: "
                                "no Codex plugin manifest found.")
                        name))))))
    (format "Unknown plugin: %s." name)))

(defun mevedel-plugins--managed-root-p (root)
  "Return non-nil when ROOT is below a mevedel-managed global root."
  (let ((root (mevedel-plugins-source-root root)))
    (cl-some
     (lambda (managed)
       (and (not (equal root managed))
            (string-prefix-p managed root)))
     (mevedel-plugins-managed-roots))))


;;; Update check

(defconst mevedel-plugins--update-check-timeout 60
  "Seconds before a hung `git' call of the update check is killed.")

(defconst mevedel-plugins--update-check-environment
  '("GIT_TERMINAL_PROMPT=0" "GIT_ASKPASS=false"
    "SSH_ASKPASS_REQUIRE=never" "GCM_INTERACTIVE=never")
  "Environment entries that keep an unattended update check from prompting.")

(defun mevedel-plugins--process-git-async (directory args callback)
  "Run git ARGS in DIRECTORY without blocking; call CALLBACK once.
CALLBACK receives (STATUS OUTPUT), like the synchronous executor returns.
The environment and arguments switch off git's terminal prompt, askpass
programs, ssh askpass and Git Credential Manager's interactive mode, so an
unattended check should not raise a credential dialog.  A call that outlives
`mevedel-plugins--update-check-timeout' is killed and reported failed."
  (let* ((default-directory directory)
         (process-environment
          (append mevedel-plugins--update-check-environment
                  process-environment))
         (buffer (generate-new-buffer " *mevedel-plugin-git*" t))
         ;; Diagnostics (GIT_TRACE, warnings) must not mix into the
         ;; stdout that callers parse, so stderr gets its own buffer.
         (stderr (generate-new-buffer " *mevedel-plugin-git-stderr*" t))
         (process (make-process
                   :name "mevedel-plugin-git" :buffer buffer
                   :stderr stderr
                   :command (append '("git" "-c" "credential.interactive=never")
                                    args)
                   :connection-type 'pipe :noquery t))
         (timer (run-at-time mevedel-plugins--update-check-timeout nil
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

(defvar mevedel-plugins-async-git-executor #'mevedel-plugins--process-git-async
  "Function called to run git for the update check without blocking.
It receives DIRECTORY, ARGS and CALLBACK, and later calls CALLBACK once
with (STATUS OUTPUT).")

(defun mevedel-plugins--git-async (directory args callback)
  "Run git ARGS in DIRECTORY through `mevedel-plugins-async-git-executor'.
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
                  (setq mevedel-plugins--update-check-busy nil)))))))
    (condition-case err
        (funcall mevedel-plugins-async-git-executor directory args wrapped)
      (error
       (if called
           (signal (car err) (cdr err))
         (funcall wrapped 1 (error-message-string err)))))))

(defun mevedel-plugins--update-targets ()
  "Return roots of installed managed plugins that are Git clones."
  (cl-loop for plugin in (mevedel-plugins-list)
           for root = (mevedel-plugin-root plugin)
           when (and (mevedel-plugins--managed-root-p root)
                     (file-exists-p (expand-file-name ".git" root)))
           collect root))

(defun mevedel-plugins--object-id-p (string)
  "Return non-nil when STRING is a full SHA-1 or SHA-256 object id."
  (and (stringp string)
       (string-match-p "\\`\\(?:[0-9a-f]\\{40\\}\\|[0-9a-f]\\{64\\}\\)\\'" string)))

(defun mevedel-plugins--check-root (root done)
  "Compare ROOT's origin HEAD with its local HEAD, cache it, call DONE."
  ;; Only a fixed reason is stored: git's own output can carry credential
  ;; URLs, and the state file is plain text.
  (cl-flet ((fail (reason)
              (mevedel-plugins--update-set-entry root (list :error reason))
              (funcall done)))
    (mevedel-plugins--git-async
     root '("ls-remote" "origin" "HEAD")
     (lambda (status output)
       (let ((remote (car (split-string output "[ \t\n]" t))))
         (if (or (not (zerop status))
                 (not (mevedel-plugins--object-id-p remote)))
             (fail (if (zerop status)
                       "unexpected git output"
                     (format "git exited with status %s" status)))
           (mevedel-plugins--git-async
            root '("rev-parse" "HEAD")
            (lambda (status output)
              (if (or (not (zerop status))
                      (not (mevedel-plugins--object-id-p output)))
                  (fail (if (zerop status)
                            "unexpected git output"
                          (format "git exited with status %s" status)))
                (mevedel-plugins--update-set-entry
                 root (list :local output :remote remote))
                (funcall done))))))))))

(defun mevedel-plugins--finish-update-check (on-demand targets done)
  "Announce what the check of TARGETS found, then call DONE.
ON-DEMAND is non-nil for a user-requested check.
Each upstream commit is announced once: automatic checks stay silent for
an already announced commit, and report only plugins newly seen."
  (setq mevedel-plugins--update-check-busy nil)
  (let (fresh available)
    (dolist (root targets)
      (let ((entry (mevedel-plugins--update-entry-for-root root)))
        (when (and entry (not (plist-get entry :error))
                   (not (equal (plist-get entry :remote)
                               (plist-get entry :local))))
          (push root available)
          (unless (equal (plist-get entry :notified)
                         (plist-get entry :remote))
            (push root fresh)))))
    (cond
     (on-demand
      (message "mevedel: checked %d plugin%s, %d with updates available"
               (length targets) (if (= (length targets) 1) "" "s")
               (length available)))
     (fresh
      (message "mevedel: updates available for %d plugin%s; /plugin to review"
               (length fresh) (if (= (length fresh) 1) "" "s"))))
    (mapc #'mevedel-plugins--update-mark-notified
          (if on-demand available fresh))
    (when done (funcall done))))

(defun mevedel-plugins-check-updates (&optional on-demand done)
  "Check managed plugins for upstream commits without blocking Emacs.
Run `git ls-remote origin HEAD' for one plugin at a time and cache the
comparison with local HEAD.  Return nil immediately; a check already
running makes this a no-op.  ON-DEMAND says a user asked, which reports
the outcome even when nothing is new.  DONE, when non-nil, is called with
no arguments after the results are cached."
  (unless mevedel-plugins--update-check-busy
    (setq mevedel-plugins--update-check-busy t)
    (condition-case err
        (let* ((targets (mevedel-plugins--update-targets))
               (queue (copy-sequence targets)))
          (mevedel-plugins--update-save
           (plist-put (copy-tree (mevedel-plugins--update-state))
                      :last-check (float-time)))
          (cl-labels
              ((next ()
                 (if queue
                     (mevedel-plugins--check-root (pop queue) #'next)
                   (mevedel-plugins--finish-update-check
                    on-demand targets done))))
            (next)))
      (error
       (setq mevedel-plugins--update-check-busy nil)
       (signal (car err) (cdr err)))))
  nil)

(defun mevedel-plugins-remove (name &optional workspace)
  "Remove managed installed plugin NAME.
Workspace runtime data is retained."
  (if-let* ((plugin (mevedel-plugins-find name workspace)))
      (let ((root (mevedel-plugin-root plugin)))
        (cond
         ((not (mevedel-plugins--managed-root-p root))
          (format "Plugin %s is not managed by mevedel; remove %s manually."
                  name root))
         ((not (yes-or-no-p
                (format "Remove plugin %s? Runtime data will be kept. "
                        name)))
          (format "Remove cancelled for plugin %s." name))
         (t
          (delete-directory root t)
          (mevedel-plugins-remove-state-for-source
           name root workspace)
          (format "Removed plugin %s." name))))
    (format "Unknown plugin: %s." name)))

(provide 'mevedel-plugin-lifecycle)
;;; mevedel-plugin-lifecycle.el ends here
