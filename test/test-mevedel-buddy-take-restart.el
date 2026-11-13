;;; test-mevedel-buddy-take-restart.el --- Taken-note restart checks -*- lexical-binding: t -*-

;;; Commentary:
;; Separate writer and reader Emacsen share only a real store and XDG state.
;; SSH runs opt in through the existing notes acceptance fixture variables.

;;; Code:

(require 'tramp-sh)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-take-restart-test--cold (directory config form)
  "Run FORM in a cold Emacs, sharing SSH through DIRECTORY with CONFIG."
  (with-temp-buffer
    (let ((process
           (make-process
            :name "take-restart" :buffer (current-buffer) :noquery t
            :connection-type 'pipe :sentinel #'ignore
            :command
            (list (expand-file-name invocation-name invocation-directory)
                  "--batch" "-Q" "--eval"
                  (prin1-to-string
                   `(progn
                      (setq load-path ',load-path
                            native-comp-enable-subr-trampolines nil
                            native-comp-jit-compilation nil
                            mevedel-buddy-idle-delay 3600)
                      (require 'cl-lib)
                      (require 'tramp-sh)
                      (setq tramp-histfile-override t
                            tramp-persistency-file-name nil
                            tramp-use-connection-share t
                            tramp-ssh-controlmaster-options
                            ,(format "-F %s -o ControlMaster=auto -o ControlPath=%s -o ControlPersist=no"
                                     config (file-name-concat directory "ssh.%%C")))
                      (let ((inhibit-message t)) ,form)))))))
      (unwind-protect
          (with-timeout (90 (ert-fail "Taken-note restart child timed out"))
            (while (process-live-p process) (accept-process-output process 0.05)))
        (when (process-live-p process) (delete-process process))
        (while (process-live-p process) (accept-process-output process 0.05)))
      (accept-process-output process 0)
      (ert-info ((buffer-string))
        (should (equal 0 (process-exit-status process)))
        (should (equal "" (buffer-string)))))))

(mevedel-deftest mevedel-note-take/restart
  (:quiet t)
  (let* ((remote ,remote)
         (base (and remote (or (getenv "MEVEDEL_TEST_SSH_ROOT")
                               (ert-skip "MEVEDEL_TEST_SSH_ROOT is not set"))))
         (config (if remote (or (getenv "MEVEDEL_TEST_SSH_CONFIG")
                                (ert-skip "MEVEDEL_TEST_SSH_CONFIG is not set"))
                   "/dev/null"))
         (directory (make-temp-file "take-restart-" t))
         (root (file-name-as-directory
                (if remote (file-name-concat base (file-name-nondirectory directory)) directory)))
         (file (file-name-concat root "source.el"))
         (mevedel-test--write-roots (cons root mevedel-test--write-roots))
         (tramp-histfile-override t)
         (tramp-persistency-file-name nil)
         (tramp-use-connection-share t)
         (tramp-ssh-controlmaster-options
          (format "-F %s -o ControlMaster=auto -o ControlPath=%s -o ControlPersist=no"
                  config (file-name-concat directory "ssh.%%C")))
         owned)
    (unwind-protect
        (progn
          (when remote (should-not (file-exists-p root)))
          (setq owned t)
          (make-directory (file-name-concat root ".mevedel") t)
          (write-region "source code\n" nil file nil 'silent)
          (mevedel-take-restart-test--cold
           directory config
           `(progn
              (require 'mevedel)
              (with-current-buffer (find-file-noselect ,file)
                (mevedel-buddy-set-level 'quiet)
                (mevedel-note-take (mevedel-note-create "Keep watching after restart"))
                (mevedel-instruction-store-flush t))))
          (mevedel-take-restart-test--cold
           directory config
           `(progn
              (require 'mevedel-notes-activation)
              (when (featurep 'mevedel-buddy) (error "Reader was not cold"))
              (let ((reads 0))
                (advice-add 'mevedel-session-control-fs-read-file :before
                            (lambda (path &rest _)
                              (when (string-suffix-p "/buddy-level" path) (cl-incf reads))))
                (with-current-buffer (find-file-noselect ,file)
                  (goto-char (point-max)) (insert ";; user edit\n") (goto-char 3)
                  (let ((deadline (+ (float-time) 20)))
                    (while (and (not (seq-some
                                      (lambda (overlay)
                                        (string-search "paused"
                                                       (or (overlay-get overlay 'after-string) "")))
                                      (overlays-in (point-min) (point-max))))
                                (< (float-time) deadline))
                      (accept-process-output nil 0.02)))
                  (when (or (bound-and-true-p mevedel-buddy-mode)
                            (featurep 'mevedel-buddy))
                    (error "Restored take started Buddy"))
                  (unless (seq-some
                           (lambda (overlay)
                             (string-search "paused"
                                            (or (overlay-get overlay 'after-string) "")))
                           (overlays-in (point-min) (point-max)))
                    (error "Taken label did not restore"))
                  (unless (and (equal "source code\n;; user edit\n" (buffer-string))
                               (= 3 (point)) (buffer-modified-p))
                    (error "Restore disturbed the user's source or point"))
                  (unless (zerop reads) (error "Restore read the dial %s times" reads))
                  (mevedel-note-take)
                  (unless (bound-and-true-p mevedel-buddy-mode)
                    (error "Taking again did not resume Buddy"))
                  (set-buffer-modified-p nil)
                  (kill-buffer))))))
      (when owned
        (when remote
          (tramp-flush-directory-properties (tramp-dissect-file-name root)
                                            (file-remote-p root 'localname)))
        (delete-directory root t))
      (when remote (tramp-cleanup-all-connections))
      (when (file-directory-p directory) (delete-directory directory t))))
  (remote)
  :doc "a cold local visit pauses a taken watch without dial I/O or lost edits"
  nil
  :doc "a cold SSH callback pauses a taken watch without dial I/O or lost edits"
  t)

(provide 'test-mevedel-buddy-take-restart)
;;; test-mevedel-buddy-take-restart.el ends here
