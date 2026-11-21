;;; niwa-plugins.el -- Plugin session integration facade -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the narrow session boundary shared by plugin registry, lifecycle,
;; and UI owners: resolving the active workspace and refreshing session skills.

;;; Code:

(require 'niwa-structs)
(require 'niwa-utilities)

;; `niwa-cockpit'
(declare-function niwa-cockpit-call-in-data
                  "niwa-cockpit" (context function &rest args))
(declare-function niwa-cockpit-context-workspace
                  "niwa-cockpit" (&optional context))
(autoload 'niwa-cockpit-call-in-data "niwa-cockpit")
(autoload 'niwa-cockpit-context-workspace "niwa-cockpit")

;; `niwa-plugin-registry'
(declare-function niwa-plugin-name
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugin-skills-dir
                  "niwa-plugin-registry" (cl-x) t)
(declare-function niwa-plugins-enabled
                  "niwa-plugin-registry" (&optional workspace))
(autoload 'niwa-plugins-enabled "niwa-plugin-registry")

;; `niwa-skills-core'
(declare-function niwa-skills-rescan "niwa-skills-core" ())

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)
(defvar niwa--session)
(defvar niwa--workspace)

(defun niwa-plugins-current-workspace ()
  "Return the current chat workspace, if available."
  (or (and (boundp 'niwa--session)
           niwa--session
           (niwa-session-workspace niwa--session))
      (and (boundp 'niwa--workspace)
           niwa--workspace)))

(defun niwa-plugins-refresh-session (&optional context)
  "Refresh CONTEXT's session skills when `niwa-skills' is available.
Without CONTEXT, refresh the session owned by the current buffer."
  (when (fboundp 'niwa-skills-rescan)
    (let ((scope (niwa-diagnostic-scope
                  (if context (niwa-cockpit-context-workspace context)
                    (current-buffer)))))
      (condition-case err
          (progn
            (if context
                (niwa-cockpit-call-in-data
                 context #'niwa-skills-rescan)
              (niwa-skills-rescan))
            t)
        (user-error nil)
        (error
         (let ((message (error-message-string err)))
           (niwa--warn-once
            scope
            'plugin-registry-refresh
            "Plugin registry refresh failed: %s" message)
           message))))))

(defun niwa-plugins-skill-dirs (&optional workspace)
  "Return enabled plugin skill directories as source-tagged entries.
Only plugins enabled in WORKSPACE are returned."
  (let (entries)
    (dolist (plugin (niwa-plugins-enabled workspace) (nreverse entries))
      (when-let* ((dir (niwa-plugin-skills-dir plugin))
                  ((file-directory-p dir)))
        (push (cons (file-name-as-directory (expand-file-name dir))
                    (cons 'plugin (niwa-plugin-name plugin)))
              entries)))))

(provide 'niwa-plugins)
;;; niwa-plugins.el ends here
