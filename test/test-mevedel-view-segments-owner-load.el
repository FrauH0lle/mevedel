;;; test-mevedel-view-segments-owner-load.el -- Cold segment owner test -*- lexical-binding: t -*-

;;; Commentary:

;; Verifies historical segment inspection without loading the mevedel umbrella.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-view-segments/cold-load ()
  ,test
  (test)
  :doc "loads the compiled owner and projects a real archived segment"
  (let* ((root
          (file-name-as-directory
           (file-name-directory (locate-library "mevedel"))))
         (compiled-root (make-temp-file "mevedel-view-segments-owner-" t))
         (emacs (expand-file-name invocation-name invocation-directory))
         (owner "mevedel-view-segments.el"))
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
                     (require 'mevedel-session-artifacts)
                     (require 'mevedel-structs)
                     (require 'mevedel-view-segments)
                     (let* ((directory
                             (make-temp-file "mevedel-segment-cold-" t))
                            (data (generate-new-buffer " *segment-cold-data*"))
                            (view (generate-new-buffer " *segment-cold-view*"))
                            (session
                             (mevedel-session--create
                              :authority-mode 'pid-lock
                              :name "cold"
                              :save-path (file-name-as-directory directory)
                              :current-segment 2
                              :prompt-index
                              '((1 . ((:cum-turn 1 :preview "archived")))
                                (2 . ((:cum-turn 2 :preview "live")))))))
                       (unwind-protect
                           (progn
                             (with-temp-file
                                 (mevedel-session-artifacts-segment-path
                                  directory 1)
                               (insert
                                ":PROPERTIES:\n:GPTEL_BOUNDS: nil\n:END:\n\n"
                                "Archived prompt\n"))
                             (with-current-buffer data
                               (org-mode)
                               (setq-local mevedel--session session)
                               (insert "Live prompt\n"))
                             (require 'mevedel-view)
                             (mevedel-view--setup view data)
                             (with-current-buffer view
                               (mevedel-view-go-to-segment 1)
                               (unless (and (mevedel-view-historical-segment-p)
                                            (string-search
                                             "Archived prompt"
                                             (buffer-string)))
                                 (error "Archived segment was not projected")))
                             (when (featurep 'mevedel)
                               (error "Owner loaded the mevedel umbrella")))
                         (when (buffer-live-p view)
                           (kill-buffer view))
                         (when (buffer-live-p data)
                           (kill-buffer data))
                         (delete-directory directory t))))))))
            (should (string-empty-p (string-trim (buffer-string))))))
      (delete-directory compiled-root t))))

(provide 'test-mevedel-view-segments-owner-load)
;;; test-mevedel-view-segments-owner-load.el ends here
