;;; mevedel-plugin-test-support.el -- Shared plugin fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-filesystem fixtures for plugin owner tests.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

;; `mevedel-plugin-registry'
(declare-function mevedel-plugins-dir "mevedel-plugin-registry" ())
(declare-function mevedel-plugins-state-file
                  "mevedel-plugin-registry" (&optional workspace))

;; `mevedel-plugin-ui'
(declare-function mevedel-plugins-slash-command "mevedel-plugin-ui" (args))

;; `mevedel-structs'
(declare-function mevedel-session-create
                  "mevedel-structs"
                  (name workspace &optional working-directory))
(declare-function mevedel-workspace--create
                  "mevedel-structs" (&rest keys))
(defvar mevedel--session)

(defun mevedel-plugins-test--plugin-root (user-dir repo)
  "Return test plugin root for REPO under USER-DIR."
  (file-name-concat user-dir ".agents" "plugins" repo))

(defun mevedel-plugins-test--github-plugin-root (user-dir owner repo)
  "Return GitHub install plugin root for OWNER and REPO under USER-DIR."
  (file-name-concat user-dir ".agents" "plugins" "github.com" owner repo))

(defun mevedel-plugins-test--github-install-root (owner repo)
  "Return new-install plugin root for OWNER and REPO."
  (file-name-concat (mevedel-plugins-dir) "github.com" owner repo))

(defun mevedel-plugins-test--staging-leftovers (dest)
  "Return in-flight clone directories left beside DEST."
  (let ((dir (file-name-directory (directory-file-name dest))))
    (and (file-directory-p dir)
         (seq-filter #'mevedel-plugins-staging-name-p
                     (directory-files dir nil
                                      directory-files-no-dot-files-regexp)))))

(defun mevedel-plugins-test--write-manifest (root json)
  "Write plugin manifest JSON under ROOT."
  (make-directory (file-name-concat root ".codex-plugin") t)
  (with-temp-file (file-name-concat root ".codex-plugin" "plugin.json")
    (insert json)))

(defun mevedel-plugins-test--workspace (root)
  "Return a test workspace rooted at ROOT."
  (mevedel-workspace--create
   :type 'file :id root :root root :name "test"))

(defun mevedel-plugins-test--session (root)
  "Return a test session rooted at ROOT."
  (let ((workspace (mevedel-plugins-test--workspace root)))
    (mevedel-session-create "main" workspace root)))

(defun mevedel-plugins-test--slash (session args)
  "Run `/plugin' ARGS as SESSION."
  (let ((mevedel--session session))
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-slash-command args))))

(defun mevedel-plugins-test--read-state (workspace)
  "Read test plugin state under WORKSPACE."
  (let ((file (mevedel-plugins-state-file workspace)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (read (current-buffer))))))

(defun mevedel-plugins-test--state-plist (workspace name)
  "Return persisted test state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (mevedel-plugins-test--read-state workspace))))

(provide 'mevedel-plugin-test-support)
;;; mevedel-plugin-test-support.el ends here
