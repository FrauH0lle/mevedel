;;; test-mevedel-tool-editing.el --- Shared editing tool authority -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-tools)
(require 'mevedel-pipeline)
(require 'mevedel-tool-editing)

(mevedel-deftest mevedel-tool-editing--call
		 (:doc "The real tool pipeline commits authorized edits and fences cancelled work" :quiet t)
		 (let* ((directory (make-temp-file "mevedel-editing-tools-" t))
			(workspace (mevedel-workspace--create :root directory))
			(session (mevedel-session--create :save-path directory
							  :workspace workspace :working-directory directory
							  :permission-mode 'full-auto :authority-mode 'pid-lock))
			(buffer (generate-new-buffer " *editing-tools-test*"))
			(mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
		   (unwind-protect
		       (with-current-buffer buffer
			 (setq-local mevedel--session session)
			 (setf (mevedel-session-root-buffer session) buffer)
			 (mevedel-tool-editing--register)
			 (let ((tool (mevedel-tool-get "SharedCreate")) reply (calls 0))
			   (funcall
			    (mevedel-pipeline-run-tool-outcome
			     tool (lambda (value) (cl-incf calls) (setq reply value))
			     '(:kind "whiteboard" :title "Cancelled")))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (or (plist-get mevedel-shared-editing--runtime :active)
					     (plist-get mevedel-shared-editing--runtime :queue))
					 (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'cancelled (plist-get reply :reason)))
			   (should (= calls 1))
			   (should-not (mevedel-shared-editing-list session))
			   (setq reply nil)
			   (mevedel-pipeline-run-tool-outcome
			    tool (lambda (value) (setq reply value))
			    '(:kind "document" :title "Accepted"))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (not reply) (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'success (plist-get reply :status)))
			   (should (= 1 (length (mevedel-shared-editing-list session))))
			   (let (cancel
				 (mevedel-shared-editing-change-hook nil))
			     (setq reply nil calls 0
				   mevedel-shared-editing-change-hook
				   (list (lambda (&rest _) (funcall cancel) (mevedel-shared-editing-stop))))
			     (setq cancel
				   (mevedel-pipeline-run-tool-outcome
				    tool (lambda (value) (cl-incf calls) (setq reply value))
				    '(:kind "whiteboard" :title "Committed before cancellation")))
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (not reply) (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should (eq 'success (plist-get reply :status)))
			     (should (= 1 calls))
			     (should (= 2 (length (mevedel-shared-editing-list session)))))
			   (dolist (restriction '(plan deny read-only))
			     (setq reply nil)
			     (setf (mevedel-session-plan-mode session) (eq restriction 'plan)
				   (mevedel-session-permission-rules session)
				   (when (eq restriction 'deny) '(("SharedCreate" :action deny))))
			     (setq buffer-read-only (eq restriction 'read-only))
			     (mevedel-pipeline-run-tool-outcome tool (lambda (value) (setq reply value))
								'(:kind "whiteboard" :title "Refused"))
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (not reply) (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should (memq (plist-get reply :status) '(error denied)))
			     (should (= 2 (length (mevedel-shared-editing-list session)))))
			   (setq buffer-read-only nil)))
		     (when (buffer-live-p buffer)
		       (with-current-buffer buffer (mevedel-shared-editing-stop))
		       (kill-buffer buffer))
		     (delete-directory directory t))))

(mevedel-deftest mevedel-tool-editing--register
  (:doc "Shared tools serialize through the provider schema with correct optional arguments")
  (let ((mevedel-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil))
    (mevedel-tool-editing--register)
    (dolist (spec '(("SharedRead" [])
                    ("SharedCreate" ["kind" "title"])
                    ("SharedEdit" ["id" "action"])))
      (let* ((tool (mevedel-tool-gptel-tool (mevedel-tool-get (car spec))))
             (encoded (json-serialize (gptel--parse-tools nil (list tool))
                                      :null-object nil :false-object :json-false))
             (decoded (json-parse-string encoded :object-type 'plist :array-type 'array))
             (parameters (plist-get (plist-get (aref decoded 0) :function) :parameters))
             (properties (plist-get parameters :properties)))
        (should (equal (plist-get parameters :required) (cadr spec)))
        (cl-loop for (_name argument) on properties by #'cddr
                 do (should (stringp (plist-get argument :description))))
        (pcase (car spec)
          ("SharedRead"
           (should (equal (plist-get (plist-get (plist-get properties :selection) :items) :type)
                          "string")))
          ("SharedCreate"
           (should (equal (plist-get (plist-get properties :kind) :enum)
                          ["whiteboard" "document"])))
          ("SharedEdit"
           (should (equal (plist-get (plist-get properties :changes) :items) '(:type "object")))
           (should (equal (plist-get (plist-get properties :action) :enum)
                          ["patch" "rename" "revert"]))))))))

(mevedel-deftest mevedel-tool-editing--restore-nulls
  (:doc "Lower gptel's lossless null marker in model-supplied args before host encoding")
  (let ((shape (list :type "rect" :box [0 0 10 10] :text "hi")))
    (should (equal (mevedel-tool-editing--restore-nulls
                    (list :id "a" :before :null :after shape))
                   (list :id "a" :before nil :after shape)))
    (should (equal (mevedel-tool-editing--restore-nulls
                    (list :changes (vconcat (list (list :id "a" :before :null :after shape)))))
                   (list :changes (vconcat (list (list :id "a" :before nil :after shape))))))
    (should (equal (mevedel-tool-editing--restore-nulls "null") "null"))
    (should (equal (mevedel-tool-editing--restore-nulls (list :a 1 :b [1 2] :c :json-false))
                   (list :a 1 :b [1 2] :c :json-false)))
    (should (equal (mevedel-tool-editing--restore-nulls
                    '(:box (710 143 85 1) :points ((1 2) (3 4))))
                   '(:box [710 143 85 1] :points [[1 2] [3 4]])))
    (should (eq (mevedel-tool-editing--restore-nulls (make-symbol ":null")) nil))
    (should (eq (mevedel-tool-editing--restore-nulls :null) nil))))

(provide 'test-mevedel-tool-editing)
;;; test-mevedel-tool-editing.el ends here
