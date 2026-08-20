;;; test-mevedel-session-owner-load.el --- Cold session owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split session owners without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-session-owners/cold-load ()
  ,test
  (test)
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "mevedel"))))
        (emacs (expand-file-name invocation-name invocation-directory))
        (cases
         '((facade
            (progn
              (require 'mevedel-session-persistence)
              (let ((directory (make-temp-file "mevedel-owner-load-" t)))
                (unwind-protect
                    (mevedel-session-persistence-allocate-session-id
                     "cold" directory)
                  (delete-directory directory t)))
              (unless (featurep 'mevedel-session-artifacts)
                (error "Facade did not load its artifact owner"))))
           (artifacts
            (progn
              (require 'mevedel-structs)
              (require 'mevedel-session-artifacts)
              (let ((session
                     (mevedel-session--create
                      :authority-mode 'pid-lock
                      :save-path temporary-file-directory
                      :session-id "cold-artifacts"
                      :current-segment 1)))
                (mevedel-session-artifacts-artifact-present-p
                 session "missing")
                (with-temp-buffer
                  (org-mode)
                  (insert "* Segment\n")
                  (mevedel-session-artifacts--insert-segment-header session)))
              (unless (featurep 'mevedel-session-codec)
                (error "Artifacts did not load the codec"))
              (unless (featurep 'mevedel-utilities)
                (error "Artifacts did not load Utilities"))))
           (codec
            (progn
              (require 'mevedel-session-codec)
              (require 'mevedel-structs)
              (require 'mevedel-workspace)
              (require 'mevedel-workspace-identity)
              (let ((directory (make-temp-file "mevedel-codec-load-" t)))
                (unwind-protect
                    (let* ((workspace
                            (mevedel-workspace-get-or-create
                             'project "cold-codec" directory "cold-codec"))
                           (_ (mevedel-workspace-identity-ensure directory))
                           (session (mevedel-session-create "cold" workspace))
                           (sidecar (mevedel-session-codec-serialize session))
                           (path (file-name-concat directory
                                                  "session.meta.el")))
                      (mevedel-session-codec-authority-mode-for-path
                       directory session)
                      (mevedel-session-codec-write path sidecar)
                      (unless (equal sidecar
                                     (mevedel-session-codec-read path))
                        (error "Codec did not round-trip sidecar data"))
                      (unless
                          (mevedel-session-p
                           (plist-get
                            (mevedel-session-codec-deserialize
                             sidecar workspace)
                            :session))
                        (error "Codec did not deserialize a session"))
                      (when (featurep 'mevedel-session-persistence)
                        (error "Codec loaded the persistence facade")))
                  (mevedel-workspace-clear-registry)
                  (delete-directory directory t)))))
           (rewind
            (progn
              (require 'mevedel-structs)
              (require 'mevedel-session-rewind)
              (mevedel-session-rewind--rewind-candidate
               (mevedel-session--create)
               '(:cum-turn 1 :segment 1))
              (when (featurep 'mevedel-session-fork)
                (error "Rewind loaded Fork"))))
           (fork
            (progn
              (require 'mevedel-structs)
              (require 'mevedel-session-fork)
              (mevedel-session-fork-clone-session
               (mevedel-session--create)
               'fork
               :current-segment 1
               :forked-from-turn 1)
              (unless (featurep 'mevedel-session-rewind)
                (error "Fork did not load Rewind")))))))
    (dolist (case cases)
      (with-temp-buffer
        (ert-info ((format "cold owner: %s" (car case)))
          (should
           (= 0
              (call-process emacs nil t nil
                            "--batch" "-Q" "-L" root
                            "--eval" (prin1-to-string (cadr case)))))
          (should (string-empty-p (string-trim (buffer-string)))))))))

(provide 'test-mevedel-session-owner-load)
;;; test-mevedel-session-owner-load.el ends here
