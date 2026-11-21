;;; test-niwa-view-segments-owner-load.el -- Cold segment owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies historical segment inspection without loading the niwa umbrella.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-view-segments/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and projects a real archived segment"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "niwa"))))
         (compiled-root (make-temp-file "niwa-view-segments-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owner "niwa-view-segments.el"))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root owner)
                     (file-name-concat compiled-root owner))
          (let ((byte-compile-verbose nil))
            (byte-compile-file (file-name-concat compiled-root owner)))
          (with-temp-buffer
            (should
             (= 0
                (call-process
                 emacs nil t nil
                 "--batch" "-Q" "-L" compiled-root "-L" root
                 "--eval"
                 (prin1-to-string
                  '(progn
                     (require 'niwa-session-artifacts)
                     (require 'niwa-structs)
                     (require 'niwa-workspace-identity)
                     (require 'niwa-view-segments)
                     (let* ((directory
                             (make-temp-file "niwa-segment-cold-" t))
                            (data (generate-new-buffer " *segment-cold-data*"))
                            (view (generate-new-buffer " *segment-cold-view*"))
                            (workspace (niwa-workspace--create
                                        :type 'file :id directory :root directory :name "cold"))
                            (session
                             (niwa-session--create
                              :authority-mode 'portable :workspace workspace
                              :working-directory (file-name-as-directory directory)
                              :execution-target (niwa-execution-target-create directory)
                              :name "cold"
                              :save-path (file-name-as-directory directory)
                              :current-segment 2
                              :prompt-index
                              '((1 . ((:cum-turn 1 :preview "archived")))
                                (2 . ((:cum-turn 2 :preview "live")))))))
                       (unwind-protect
                           (progn
                             (with-temp-file
                                 (niwa-session-artifacts-segment-path
                                  directory 1)
                               (insert
                                ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                                "Archived prompt\n"))
                             (niwa-workspace-identity-ensure directory)
                             (unless (niwa-session-durability-lease-acquire
                                      directory "cold fixture" session)
                               (error "Could not lease cold fixture"))
                             (niwa-session-publication-publish
                              session
                              (list (list :path (niwa-session-artifacts-segment-path directory 1)
                                          :content ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\nArchived prompt\n")
                                    (list :path (file-name-concat directory "session.meta.el")
                                          :content (niwa-session-artifacts-printed-value
                                                    (niwa-session-codec-serialize session))
                                          :commit-marker t)))
                             (with-current-buffer data
                               (org-mode)
                               (setq-local niwa--session session)
                               (insert "Live prompt\n"))
                             (require 'niwa-view)
                             (niwa-view--setup view data)
                             (with-current-buffer view
                               (niwa-view-go-to-segment 1)
                               (unless (and (niwa-view-historical-segment-p)
                                            (string-search
                                             "Archived prompt"
                                             (buffer-string)))
                                 (error "Archived segment was not projected")))
                             (when (featurep 'niwa)
                               (error "Owner loaded the niwa umbrella")))
                         (when (buffer-live-p view)
                           (kill-buffer view))
                         (when (buffer-live-p data)
                           (kill-buffer data))
                         (niwa-session-durability-lease-release directory session)
                         (delete-directory directory t))))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-niwa-view-segments-owner-load)
;;; test-niwa-view-segments-owner-load.el ends here
