;;; mevedel-journal-test-support.el -- Journal integration fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-session fixtures for capture and processing tests.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))
(require 'mevedel-journal-capture)
(require 'mevedel-compact-run)
(require 'mevedel-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-compact-test-support"))

(defun mevedel-test-journal-capture--with-session (function)
  "Call FUNCTION with a real temporary SESSION and BUFFER."
  (cl-destructuring-bind (workspace . directory)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *journal-capture-session*"))
          (session (mevedel-session-create "capture" workspace))
          (mevedel-journal-enabled t))
      (unwind-protect
          (mevedel-skills-test--with-model-backends
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--session session
                          gptel-backend (gptel-get-backend "Fast")
                          gptel-model 'fast-model)
              (gptel-mode 1)
              (funcall function session buffer)))
        (let ((mevedel-journal-enabled nil))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (when (mevedel-session-save-path session)
          (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session))
        (delete-directory directory t)
        (mevedel-workspace-clear-registry)))))

(defun mevedel-test-journal-capture--turn (session buffer prompt response)
  "Complete and auto-save PROMPT and RESPONSE for SESSION in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((start (point)))
      (insert "\n" prompt "\n")
      (remove-text-properties start (point) '(gptel nil)))
    (insert (propertize (concat response "\n") 'gptel 'response))
    (cl-incf (mevedel-session-turn-count session))
    (mevedel--run-turn-steps (gptel-make-fsm :info (list :buffer buffer)) '(mevedel--turn-save mevedel--turn-checkpoint))))

(defun mevedel-test-journal-capture--with-portable-session (function)
  "Call FUNCTION with an isolated portable session and its root buffer."
  (let ((kill-emacs-hook kill-emacs-hook)
        (mevedel-session-durability--disclosed-targets (make-hash-table :test #'equal)))
    (mevedel-test-journal-capture--with-session
     (lambda (session buffer)
       (setf (mevedel-workspace-type (mevedel-session-workspace session)) 'project
             (mevedel-session-authority-mode session) 'portable)
       (puthash (mevedel-execution-target-identity (mevedel-session-execution-target session))
                t mevedel-session-durability--disclosed-targets)
       (funcall function session buffer)))))

(provide 'mevedel-journal-test-support)
;;; mevedel-journal-test-support.el ends here
