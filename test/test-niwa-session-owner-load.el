;;; test-niwa-session-owner-load.el --- Cold session owner tests -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies the split session owners without loading the niwa umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-session-owners/cold-load ()
  ,test
  (test)
  (let ((root
         (file-name-as-directory
          (file-name-directory (locate-library "niwa"))))
        (emacs (expand-file-name invocation-name invocation-directory))
        (gptel-root (file-name-directory (locate-library "gptel-request")))
        (cases
         '((facade
            (progn
              (require 'niwa-session-persistence)
              (let ((directory (make-temp-file "niwa-owner-load-" t)))
                (unwind-protect
                    (progn
                      (niwa-session-persistence-allocate-session-id
                       directory)
                      (with-temp-buffer
                        (insert "cold")
                        (niwa-session-persistence-write-current-buffer-atomically
                         (file-name-concat directory "segment.chat.org"))))
                  (delete-directory directory t)))
              (unless (featurep 'niwa-session-artifacts)
                (error "Facade did not load its artifact owner"))))
           (artifacts
            (progn
              (require 'niwa-structs)
              (require 'niwa-session-artifacts)
              (let ((session
                     (niwa-session--create
                      :save-path temporary-file-directory
                      :session-id "cold-artifacts"
                      :current-segment 1)))
                (niwa-session-artifacts-artifact-present-p
                 session "missing")
                (with-temp-buffer
                  (org-mode)
                  (insert "* Segment\n")
                  (niwa-session-artifacts--insert-segment-header session)
                  (niwa-session-artifacts-collect-prompts
                   (current-buffer))))
              (unless (featurep 'niwa-session-publication)
                (error "Artifact resolution did not load its publication owner"))
              (unless (featurep 'niwa-utilities)
                (error "Artifacts did not load Utilities"))))
           (codec
            (progn
              (require 'niwa-session-codec)
              (require 'niwa-structs)
              (require 'niwa-workspace)
              (require 'niwa-workspace-identity)
              (let ((directory (make-temp-file "niwa-codec-load-" t)))
                (unwind-protect
                    (let* ((workspace
                            (niwa-workspace-get-or-create
                             'project "cold-codec" directory "cold-codec"))
                           (_ (niwa-workspace-identity-ensure directory))
                           (session (niwa-session-create "cold" workspace))
                           (sidecar (niwa-session-codec-serialize session))
                           (path (file-name-concat directory
                                                  "session.meta.el")))
                      (niwa-session-codec-authority-mode-for-path
                       directory session)
                      (niwa-session-codec-write path sidecar)
                      (unless (equal sidecar
                                     (niwa-session-codec-read path))
                        (error "Codec did not round-trip sidecar data"))
                      (unless
                          (niwa-session-p
                           (plist-get
                            (niwa-session-codec-deserialize
                             sidecar workspace)
                            :session))
                        (error "Codec did not deserialize a session"))
                      (when (featurep 'niwa-session-persistence)
                        (error "Codec loaded the persistence facade")))
                  (niwa-workspace-clear-registry)
                  (delete-directory directory t)))))
           (rewind
            (progn
              (require 'niwa-structs)
              (require 'niwa-session-rewind)
              (niwa-session-rewind--rewind-candidate
               (niwa-session--create)
               '(:cum-turn 1 :segment 1))
              (when (featurep 'niwa-session-fork)
                (error "Rewind loaded Fork"))))
           (fork
            (progn
              (require 'niwa-structs)
              (require 'niwa-session-fork)
              (niwa-session-fork-clone-session
               (niwa-session--create)
               'fork
               :current-segment 1
               :forked-from-turn 1)
              (unless (featurep 'niwa-session-rewind)
                (error "Fork did not load Rewind")))))))
    (dolist (case cases)
      (with-temp-buffer
        (ert-info ((format "cold owner: %s" (car case)))
          (should
           (= 0
              (call-process emacs nil t nil
                            "--batch" "-Q" "-L" root "-L" gptel-root
                            "--eval" (prin1-to-string (cadr case)))))
          (should (string-empty-p (string-trim (buffer-string)))))))))

(provide 'test-niwa-session-owner-load)
;;; test-niwa-session-owner-load.el ends here
