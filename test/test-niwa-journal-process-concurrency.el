;;; test-niwa-journal-process-concurrency.el -- Independent digest clients -*- lexical-binding: t -*-

;;; Commentary:

;; Real Emacs processes share a sealed capture and race workspace admission.
;; The accepted model callback is controlled by real barrier files.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-process)

(niwa-deftest niwa-journal-process-next/concurrency ()
  ,test
  (test)
  :doc "two independent clients admit one request and publish one capture result"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Saved decisive evidence")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (barriers (make-temp-file "niwa-journal-race-" t))
            (runner (file-name-concat barriers "runner.el"))
            (deadline (+ (float-time) 30))
            processes buffers)
       (unwind-protect
           (progn
             (with-temp-file runner
               (insert ";;; -*- lexical-binding: t -*-\n")
               (prin1
                `(progn
                   (setq load-path ',load-path)
                   (require 'niwa-journal-process)
                   (require 'gptel-openai)
                   (gptel-make-openai "Fast" :key "unused" :models '(fast-model))
                   (let* ((worker (pop command-line-args-left))
                          (workspace (niwa-workspace--create :root ,root :id ,root :type 'project))
                          (start (file-name-concat ,barriers "start"))
                          (release (file-name-concat ,barriers "release"))
                          (stop (+ (float-time) 25)))
                     (write-region "ready" nil (file-name-concat ,barriers (concat "ready-" worker)) nil 'silent)
                     (while (and (not (file-exists-p start)) (< (float-time) stop))
                       (accept-process-output nil 0.01))
                     (unless (file-exists-p start) (error "Start barrier timed out"))
                     (cl-letf (((symbol-function 'gptel-request)
                                (lambda (_prompt &rest args)
                                  (write-region "request" nil (file-name-concat ,barriers (concat "request-" worker)) nil 'silent)
                                  (while (and (not (file-exists-p release)) (< (float-time) stop))
                                    (accept-process-output nil 0.01))
                                  (unless (file-exists-p release) (error "Release barrier timed out"))
                                  (funcall (plist-get args :callback)
                                           (format "## Done\n- Observed: worker %s accepted saved evidence.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none" worker)
                                           nil))))
                       (niwa-journal-process-next workspace))
                     (write-region "done" nil (file-name-concat ,barriers (concat "done-" worker)) nil 'silent)))
                (current-buffer)))
             (dotimes (index 2)
               (let* ((output (generate-new-buffer " *journal-race-child*"))
                      (process (make-process
                                :name (format "journal-race-%s" index) :buffer output :noquery t
                                :sentinel #'ignore
                                :command (list (file-name-concat invocation-directory invocation-name)
                                               "-Q" "--batch" "-l" runner (number-to-string index)))))
                 (push output buffers)
                 (push process processes)))
             (while (and (< (length (directory-files barriers nil "\\`ready-")) 2)
                         (cl-every #'process-live-p processes) (< (float-time) deadline))
               (accept-process-output nil 0.01))
             (should (= 2 (length (directory-files barriers nil "\\`ready-"))))
             (write-region "start" nil (file-name-concat barriers "start") nil 'silent)
             (while (and (or (null (directory-files barriers nil "\\`request-"))
                             (null (directory-files barriers nil "\\`done-")))
                         (< (float-time) deadline))
               (accept-process-output nil 0.01))
             (should (= 1 (length (directory-files barriers nil "\\`request-"))))
             (should (= 1 (length (directory-files barriers nil "\\`done-"))))
             (write-region "release" nil (file-name-concat barriers "release") nil 'silent)
             (while (and (cl-some #'process-live-p processes) (< (float-time) deadline))
               (accept-process-output nil 0.01))
             (dolist (process processes)
               (should-not (process-live-p process))
               (should (= 0 (process-exit-status process))))
             (let ((entries (niwa-journal-store-entries root)))
               (should (= 1 (length entries)))
               (should (equal (plist-get capture :id) (plist-get (car entries) :capture-id))))
             (should (= 1 (length (directory-files barriers nil "\\`request-"))))
             (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
             (should-not (niwa-journal-capture-list workspace)))
         (dolist (process processes) (when (process-live-p process) (delete-process process)))
         (dolist (output buffers) (when (buffer-live-p output) (kill-buffer output)))
         (delete-directory barriers t))))))

(provide 'test-niwa-journal-process-concurrency)
;;; test-niwa-journal-process-concurrency.el ends here
