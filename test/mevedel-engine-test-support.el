;;; mevedel-engine-test-support.el --- Admitted engine fixture -*- lexical-binding: t -*-

;;; Commentary:
;; Real published sessions for external request and transport tests.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))

(defmacro mevedel-engine-test--with-session (&rest body)
  "Run BODY in a temporary published session with an admitted request."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-mcp-tools-" t))
          (workspace (test-mevedel-session-persistence--make-workspace root))
          (session (mevedel-session-create "mcp-tools" workspace))
          (buffer (generate-new-buffer " *mcp-tools*")))
     (unwind-protect
         (with-current-buffer buffer
           (mevedel-chat-prepare-transcript-buffer)
           (setq-local mevedel--session session mevedel--workspace workspace
                       default-directory (file-name-as-directory root))
           (setf (mevedel-session-permission-mode session) 'full-auto)
           (mevedel-session-set-root-buffer session buffer)
           (mevedel-session-artifacts-ensure-files session buffer)
           (let ((request (mevedel-request-begin session))) ,@body))
       (test-mevedel-session-persistence--release-and-kill buffer session)
       (mevedel-workspace-clear-registry)
       (delete-directory root t))))

(defun mevedel-engine-test--evidence ()
  "Return decoded transcript evidence from the current fixture buffer."
  (mevedel-transcript-project-evidence (list (cons (point-min) (point-max)))))

(defun mevedel-engine-test--count-evidence (regexp)
  "Count REGEXP in decoded evidence, including delivered reminder records."
  (let ((text (mevedel-engine-test--evidence)))
    (with-temp-buffer
      (insert text)
      (how-many regexp (point-min) (point-max)))))

(provide 'mevedel-engine-test-support)
;;; mevedel-engine-test-support.el ends here
