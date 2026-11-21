;;; test-niwa-agent-runtime-owner-load.el --- Cold runtime compilation tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise provider interruption after compiling without preloaded gptel.

;;; Code:

(require 'bytecomp)

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-agent-runtime/cold-compile ()
		 ,test
		 (test)
		 :doc "cold-compiled interruption aborts and restores provider callbacks"
		 (let* ((root (file-name-directory (locate-library "niwa-agent-runtime")))
			(compiled-root (make-temp-file "niwa-runtime-compile-" t))
			(source (file-name-concat compiled-root "niwa-agent-runtime.el"))
			(emacs (expand-file-name invocation-name invocation-directory)))
		   (unwind-protect
		       (progn
			 (copy-file (file-name-concat root "niwa-agent-runtime.el") source)
			 ;; Source loading this dependency evaluates its compile-time
			 ;; gptel require and accidentally supplies the runtime's expanders.
			 (let ((system-source
				(file-name-concat compiled-root "niwa-system.el"))
			       (byte-compile-verbose nil))
			   (copy-file (file-name-concat root "niwa-system.el") system-source)
			   (byte-compile-file system-source))
			 ;; Compilation must also be cold: helpers loads gptel-request here.
			 ;; The child inherits Eask's isolated HOME and dependency paths.
			 (with-temp-buffer
			   (let ((status
				  (call-process
				   emacs nil t nil "--batch" "-Q"
				   "--eval"
				   (prin1-to-string
				    `(progn
				       (setq load-path ',(cons compiled-root load-path))
				       (require 'bytecomp)
				       (when (featurep 'gptel-request)
					 (error "gptel-request must start unloaded"))
				       (let ((byte-compile-verbose nil)
					     (byte-compile-error-on-warn t))
					 (unless (byte-compile-file ,source)
					   (error "Runtime compilation failed")))
				       (load ,(concat source "c") nil t)
				       (load ,(file-name-concat
					       root "test/test-niwa-agent-runtime.el") nil t)
				       (dolist (name '(niwa-agent-runtime-interrupt/test@1
						       niwa-agent-runtime-interrupt/test@2))
					 (let ((result (ert-run-test (ert-get-test name))))
					   (unless (ert-test-passed-p result)
					     (signal
					      (car (ert-test-result-with-condition-condition
						    result))
					      (cdr (ert-test-result-with-condition-condition
						    result)))))))))))
			     (ert-info ((buffer-string))
				       (should (equal 0 status))
				       (should (equal "" (buffer-string)))))))
		     (delete-directory compiled-root t))))

(provide 'test-niwa-agent-runtime-owner-load)
;;; test-niwa-agent-runtime-owner-load.el ends here
