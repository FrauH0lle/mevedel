;;; test-mevedel-journal-process-concurrency.el -- Independent digest clients -*- lexical-binding: t -*-

;;; Commentary:

;; Real Emacs processes share a sealed capture and race workspace admission.
;; The accepted model callback is controlled by real barrier files.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-process)

(mevedel-deftest mevedel-journal-process-next/concurrency ()
  ,test
  (test)
  :doc "two independent clients admit one request and publish one capture result"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Saved decisive evidence")
     (let* ((workspace (mevedel-session-workspace session))
            (root (mevedel-workspace-root workspace))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (barriers (make-temp-file "mevedel-journal-race-" t))
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
                   (require 'mevedel-journal-process)
                   (require 'gptel-openai)
                   (gptel-make-openai "Fast" :key "unused" :models '(fast-model))
                   (let* ((worker (pop command-line-args-left))
                          (workspace (mevedel-workspace--create :root ,root :id ,root :type 'project))
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
                       (mevedel-journal-process-next workspace))
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
             (let ((entries (mevedel-journal-store-entries root)))
               (should (= 1 (length entries)))
               (should (equal (plist-get capture :id) (plist-get (car entries) :capture-id))))
             (should (= 1 (length (directory-files barriers nil "\\`request-"))))
             (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
             (should-not (mevedel-journal-capture-list workspace)))
         (dolist (process processes) (when (process-live-p process) (delete-process process)))
         (dolist (output buffers) (when (buffer-live-p output) (kill-buffer output)))
         (delete-directory barriers t))))))

(provide 'test-mevedel-journal-process-concurrency)
;;; test-mevedel-journal-process-concurrency.el ends here
