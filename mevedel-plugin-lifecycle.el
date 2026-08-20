;;; mevedel-plugin-lifecycle.el -- Plugin Git lifecycle -*- lexical-binding: t -*-

;;; Commentary:

;; Installs, updates, and removes mevedel plugins through Git and the
;; plugin registry's validated manifest and durable state operations.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; `mevedel-plugin-registry'
(declare-function mevedel-plugin-name
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-root
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugins-dir "mevedel-plugin-registry" ())
(declare-function mevedel-plugins-find
                  "mevedel-plugin-registry" (name &optional workspace))
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

(defun mevedel-plugins--repo-name (target)
  "Return repository name from GitHub TARGET."
  (cadr (split-string target "/" t)))

(defun mevedel-plugins--install-target-repo (target)
  "Return GitHub OWNER/REPO for install TARGET, or nil."
  (and (mevedel-plugins--github-target-p target) target))

(defun mevedel-plugins--github-owner (target)
  "Return owner name from GitHub TARGET."
  (car (split-string target "/" t)))

(defun mevedel-plugins--github-install-dir (target)
  "Return install directory for GitHub TARGET."
  (file-name-concat (mevedel-plugins-dir)
                    "github.com"
                    (mevedel-plugins--github-owner target)
                    (mevedel-plugins--repo-name target)))

(defun mevedel-plugins--git (directory args)
  "Run git ARGS in DIRECTORY through `mevedel-plugins-git-executor'."
  (condition-case err
      (let ((result (funcall mevedel-plugins-git-executor directory args)))
        (if (and (consp result) (integerp (car result)))
            result
          (list 1 (format "%s" result))))
    (error (list 1 (error-message-string err)))))

(defun mevedel-plugins-install (target)
  "Install GitHub plugin TARGET."
  (require 'mevedel-plugin-registry)
  (let ((repo (mevedel-plugins--install-target-repo target)))
    (if (not (mevedel-plugins--github-target-p repo))
        "Invalid plugin target: use OWNER/REPO or a GitHub repository."
      (let* ((dest (mevedel-plugins--github-install-dir repo))
             (present (file-directory-p dest))
             (args (list "clone" "--depth" "1"
                         (format "https://github.com/%s.git" repo)
                         dest))
             status
             output)
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
          (pcase-let ((`(,git-status ,git-output)
                       (mevedel-plugins--git (mevedel-plugins-dir) args)))
            (setq status git-status
                  output git-output))
          (if (not (zerop status))
              (format "Failed to install plugin %s: %s"
                      target
                      (if (string-empty-p output) "git failed" output))
            (if-let* ((plugin (mevedel-plugins-read-manifest dest)))
                (format "Installed plugin %s." (mevedel-plugin-name plugin))
              (format "Failed to install plugin %s: no Codex plugin manifest found."
                      target))))))))

(defun mevedel-plugins-update (name &optional workspace)
  "Update installed plugin NAME with git pull.
Preserve plugin state in WORKSPACE."
  (require 'mevedel-plugin-registry)
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
  (require 'cl-lib)
  (let ((root (mevedel-plugins-source-root root)))
    (cl-some
     (lambda (managed)
       (and (not (equal root managed))
            (string-prefix-p managed root)))
     (mevedel-plugins-managed-roots))))

(defun mevedel-plugins-remove (name &optional workspace)
  "Remove managed installed plugin NAME.
Workspace runtime data is retained."
  (require 'mevedel-plugin-registry)
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
