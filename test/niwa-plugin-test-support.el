;;; niwa-plugin-test-support.el -- Shared plugin fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-filesystem fixtures for plugin owner tests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `niwa-plugin-registry'
(declare-function niwa-plugins-dir "niwa-plugin-registry" ())
(declare-function niwa-plugins-state-file
                  "niwa-plugin-registry" (&optional workspace))

;; `niwa-plugin-ui'
(declare-function niwa-plugins-slash-command "niwa-plugin-ui" (args))

;; `niwa-structs'
(declare-function niwa-session-create
                  "niwa-structs"
                  (name workspace &optional working-directory))
(declare-function niwa-workspace--create
                  "niwa-structs" (&rest keys))
(defvar niwa--session)

(defun niwa-plugins-test--plugin-root (user-dir repo)
  "Return test plugin root for REPO under USER-DIR."
  (file-name-concat user-dir ".agents" "plugins" repo))

(defun niwa-plugins-test--github-plugin-root (user-dir owner repo)
  "Return GitHub install plugin root for OWNER and REPO under USER-DIR."
  (file-name-concat user-dir ".agents" "plugins" "github.com" owner repo))

(defun niwa-plugins-test--github-install-root (owner repo)
  "Return new-install plugin root for OWNER and REPO."
  (file-name-concat (niwa-plugins-dir) "github.com" owner repo))

(defun niwa-plugins-test--staging-leftovers (dest)
  "Return in-flight clone directories left beside DEST."
  (let ((dir (file-name-directory (directory-file-name dest))))
    (and (file-directory-p dir)
         (seq-filter #'niwa-plugins-staging-name-p
                     (directory-files dir nil
                                      directory-files-no-dot-files-regexp)))))

(defun niwa-plugins-test--write-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun niwa-plugins-test--workspace (root)
  "Return a test workspace rooted at ROOT."
  (niwa-workspace--create
   :type 'file :id root :root root :name "test"))

(defun niwa-plugins-test--session (root)
  "Return a test session rooted at ROOT."
  (let ((workspace (niwa-plugins-test--workspace root)))
    (niwa-session-create "main" workspace root)))

(defun niwa-plugins-test--slash (session args)
  "Run `/plugin' ARGS as SESSION."
  (let ((niwa--session session))
    (niwa-test--with-captured-messages nil
      (niwa-plugins-slash-command args))))

(defun niwa-plugins-test--read-state (workspace)
  "Read test plugin state under WORKSPACE."
  (let ((file (niwa-plugins-state-file workspace)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun niwa-plugins-test--state-plist (workspace name)
  "Return persisted test state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (niwa-plugins-test--read-state workspace))))

(provide 'niwa-plugin-test-support)
;;; niwa-plugin-test-support.el ends here
