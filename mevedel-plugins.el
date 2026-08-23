;;; mevedel-plugins.el -- Plugin session integration facade -*- lexical-binding: t -*-

;;; Commentary:

;; Provides the narrow session boundary shared by plugin registry, lifecycle,
;; and UI owners: resolving the active workspace and refreshing session skills.

;;; Code:

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-call-in-data
                  "mevedel-cockpit" (context function &rest args))

;; `mevedel-plugin-registry'
(declare-function mevedel-plugin-name
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugin-skills-dir
                  "mevedel-plugin-registry" (cl-x) t)
(declare-function mevedel-plugins-enabled
                  "mevedel-plugin-registry" (&optional workspace))

;; `mevedel-skills-core'
(declare-function mevedel-skills-rescan "mevedel-skills-core" ())

;; `mevedel-structs'
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(defvar mevedel--session)
(defvar mevedel--workspace)

;; `mevedel-utilities'
(declare-function mevedel--warn-once
                  "mevedel-utilities" (key format &rest args))

(defun mevedel-plugins-current-workspace ()
  "Return the current chat workspace, if available."
  (require 'mevedel-structs)
  (or (and (boundp 'mevedel--session)
           mevedel--session
           (mevedel-session-workspace mevedel--session))
      (and (boundp 'mevedel--workspace)
           mevedel--workspace)))

(defun mevedel-plugins-refresh-session (&optional context)
  "Refresh CONTEXT's session skills when `mevedel-skills' is available.
Without CONTEXT, refresh the session owned by the current buffer."
  (when (fboundp 'mevedel-skills-rescan)
    (condition-case err
        (progn
          (if context
              (progn
                (require 'mevedel-cockpit)
                (mevedel-cockpit-call-in-data
                 context #'mevedel-skills-rescan))
            (mevedel-skills-rescan))
          t)
      (user-error nil)
      (error
       (let ((message (error-message-string err)))
         (mevedel--warn-once
          'plugin-registry-refresh
          "Plugin registry refresh failed: %s" message)
         message)))))

(defun mevedel-plugins-skill-dirs (&optional workspace)
  "Return enabled plugin skill directories as source-tagged entries.
Only plugins enabled in WORKSPACE are returned."
  (require 'mevedel-plugin-registry)
  (let (entries)
    (dolist (plugin (mevedel-plugins-enabled workspace) (nreverse entries))
      (when-let* ((dir (mevedel-plugin-skills-dir plugin))
                  ((file-directory-p dir)))
        (push (cons (file-name-as-directory (expand-file-name dir))
                    (cons 'plugin (mevedel-plugin-name plugin)))
              entries)))))

(provide 'mevedel-plugins)
;;; mevedel-plugins.el ends here
