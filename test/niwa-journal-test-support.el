;;; niwa-journal-test-support.el -- Journal integration fixtures -*- lexical-binding: t -*-

;;; Commentary:

;; Shared real-session fixtures for capture and processing tests.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-journal-capture)
(require 'niwa-compact-run)
(require 'niwa-compact-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-compact-test-support"))

(defun niwa-test-journal-capture--with-session (function)
  "Call FUNCTION with a real temporary SESSION and BUFFER."
  (cl-destructuring-bind (workspace . directory)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *journal-capture-session*"))
          (session (niwa-session-create "capture" workspace))
          (niwa-journal-enabled t))
      (unwind-protect
          (niwa-skills-test--with-model-backends
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session
                          gptel-backend (gptel-get-backend "Fast")
                          gptel-model 'fast-model)
              (gptel-mode 1)
              (funcall function session buffer)))
        (let ((niwa-journal-enabled nil))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (when (niwa-session-save-path session)
          (niwa-session-persistence-lock-release (niwa-session-save-path session) session))
        (delete-directory directory t)
        (niwa-workspace-clear-registry)))))

(defun niwa-test-journal-capture--turn (session buffer prompt response)
  "Complete and auto-save PROMPT and RESPONSE for SESSION in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-max))
    (let ((start (point)))
      (insert "\n" prompt "\n")
      (remove-text-properties start (point) '(gptel nil)))
    (insert (propertize (concat response "\n") 'gptel 'response))
    (cl-incf (niwa-session-turn-count session))
    (niwa--run-turn-steps (gptel-make-fsm :info (list :buffer buffer)) '(niwa--turn-save niwa--turn-checkpoint))))

(defun niwa-test-journal-capture--with-portable-session (function)
  "Call FUNCTION with an isolated portable session and its root buffer."
  (let ((kill-emacs-hook kill-emacs-hook)
        (niwa-session-durability--disclosed-targets (make-hash-table :test #'equal)))
    (niwa-test-journal-capture--with-session
     (lambda (session buffer)
       (setf (niwa-workspace-type (niwa-session-workspace session)) 'project
             (niwa-session-authority-mode session) 'portable)
       (puthash (niwa-execution-target-identity (niwa-session-execution-target session))
                t niwa-session-durability--disclosed-targets)
       (funcall function session buffer)))))

(provide 'niwa-journal-test-support)
;;; niwa-journal-test-support.el ends here
