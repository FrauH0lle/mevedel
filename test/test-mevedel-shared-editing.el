;;; test-mevedel-shared-editing.el --- Shared editing acceptance -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-shared-editing)

(mevedel-deftest mevedel-shared-editing-call
		 (:doc "Commits a board through the host helper and reopens durable state")
		 (let* ((directory (make-temp-file "mevedel-editing-" t))
			(session (mevedel-session--create :save-path directory
							  :authority-mode 'pid-lock))
			(buffer (generate-new-buffer " *editing-test*")))
		   (unwind-protect
		       (with-current-buffer buffer
			 (setq-local mevedel--session session)
			 (setf (mevedel-session-root-buffer session) buffer)
			 (let (reply)
			   (mevedel-shared-editing-call
			    session '(:action "create" :id "board1" :kind "whiteboard"
					      :title "Architecture" :actor "Alice" :opId "one")
			    (lambda (result) (setq reply result)))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (not reply) (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should-not (plist-get reply :error))
			   (should (= 1 (plist-get (plist-get reply :result) :revision)))
			   (should (equal "Architecture"
					  (plist-get (car (mevedel-shared-editing-list session))
						     :title)))
			   (mevedel-shared-editing-stop)
			   (setq reply nil)
			   (mevedel-shared-editing-call
			    session '(:action "read" :id "board1")
			    (lambda (result) (setq reply result)))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (not reply) (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (equal "Architecture"
					  (plist-get (plist-get reply :result) :title)))))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer (mevedel-shared-editing-stop))
		       (kill-buffer buffer))
		     (delete-directory directory t))))

(mevedel-deftest mevedel-shared-editing-stop
		 (:doc "Killing a buffer after commit settles once and stops its private helper")
		 (let* ((directory (make-temp-file "mevedel-editing-stop-" t))
			(session (mevedel-session--create :save-path directory :authority-mode 'pid-lock))
			(buffer (generate-new-buffer " *editing-stop-test*"))
			(calls 0) reply runtime
			(mevedel-shared-editing-change-hook
			 (list (lambda (&rest _) (kill-buffer buffer)))))
		   (unwind-protect
		       (progn
			 (with-current-buffer buffer
			   (setq-local mevedel--session session)
			   (setf (mevedel-session-root-buffer session) buffer)
			   (mevedel-shared-editing-call
			    session '(:action "create" :id "committed" :kind "whiteboard"
					      :title "Committed" :actor "Alice" :opId "one")
			    (lambda (result) (cl-incf calls) (setq reply result)))
			   (setq runtime mevedel-shared-editing--runtime))
			 (let ((deadline (+ (float-time) 10)))
			   (while (and (not reply) (< (float-time) deadline))
			     (accept-process-output nil 0.05)))
			 (should (= calls 1))
			 (should-not (plist-get reply :error))
			 (should-not (buffer-live-p buffer))
			 (should-not (process-live-p (plist-get runtime :process)))
			 (should (= 1 (plist-get (car (mevedel-shared-editing-list session)) :revision))))
		     (when runtime (mevedel-shared-editing-stop runtime))
		     (when (buffer-live-p buffer) (kill-buffer buffer))
		     (delete-directory directory t))))

(mevedel-deftest mevedel-shared-editing--parse
  (:doc "Preserves empty mark attributes, arrays, nulls and false through exact patch reads")
  (let* ((json "{\"before\":null,\"marks\":[{\"type\":\"bold\",\"attrs\":{}}],\"empty\":[],\"flag\":false}")
         (value (mevedel-shared-editing--parse json)))
    (should (null (plist-get value :before)))
    (should (hash-table-p (plist-get (aref (plist-get value :marks) 0) :attrs)))
    (should (equal json (mevedel-shared-editing--json value)))))

(mevedel-deftest mevedel-shared-editing--json
  (:doc "Unicode JSON remains text when nested in model or browser messages")
  (let* ((value (list :title (string #x2014 #x03bb #x1f331) :empty nil :flag :json-false))
         (text (mevedel-shared-editing--json value))
         (envelope (json-serialize (list :text text))))
    (should (multibyte-string-p text))
    (should (equal (plist-get (json-parse-string envelope :object-type 'plist) :text) text))
    (should (equal value (mevedel-shared-editing--parse text)))))

(provide 'test-mevedel-shared-editing)
;;; test-mevedel-shared-editing.el ends here
