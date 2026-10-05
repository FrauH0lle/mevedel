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

(mevedel-deftest mevedel-tool-editing--edit
  (:doc "The model lists library items with a sheet and inserts one into a board" :quiet t)
  (let* ((directory (make-temp-file "mevedel-editing-library-tools-" t))
         (workspace (mevedel-workspace--create :root directory))
         (session (mevedel-session--create :save-path directory
                                           :workspace workspace :working-directory directory
                                           :permission-mode 'full-auto :authority-mode 'pid-lock))
         (buffer (generate-new-buffer " *editing-library-tools-test*"))
         (mevedel-shared-library-directory (file-name-concat directory "libraries"))
         (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-root-buffer session) buffer)
          (mevedel-tool-editing--register)
          (cl-labels ((run (name args)
                        (let (reply)
                          (mevedel-pipeline-run-tool-outcome
                           (mevedel-tool-get name) (lambda (value) (setq reply value)) args)
                          (let ((deadline (+ (float-time) 10)))
                            (while (and (not reply) (< (float-time) deadline))
                              (accept-process-output nil 0.05)))
                          (should (eq 'success (plist-get reply :status)))
                          reply)))
            ;; Version 1 libraries hold bare element lists without item ids.
            (make-directory mevedel-shared-library-directory t)
            (with-temp-file (file-name-concat mevedel-shared-library-directory "Legacy.excalidrawlib")
              (insert "{\"type\":\"excalidrawlib\",\"version\":1,\"library\":[[{\"id\":\"r\",\"type\":\"rectangle\",\"x\":0,\"y\":0,\"width\":80,\"height\":40}]]}"))
            (let* ((listed (run "SharedRead" '(:library t)))
                   (items (append (plist-get (mevedel-shared-editing--parse (plist-get listed :result)) :items) nil))
                   (legacy (plist-get (cl-find "Legacy" items :key (lambda (i) (plist-get i :library)) :test #'equal) :ref)))
              (should (member "Built-in/builtin-database" (mapcar (lambda (i) (plist-get i :ref)) items)))
              (should (plist-get listed :media))
              (let ((board (mevedel-shared-editing--parse
                            (plist-get (run "SharedCreate" '(:kind "whiteboard" :title "Stores")) :result))))
                (let ((inserted (mevedel-shared-editing--parse
                                 (plist-get (run "SharedEdit" (list :id (plist-get board :id) :action "insert"
                                                                    :item "Built-in/builtin-database" :x 40 :y 60))
                                            :result))))
                  (should (= 4 (length (plist-get inserted :inserted))))
                  (should (= 40 (apply #'min (mapcar (lambda (e) (plist-get e :x))
                                                     (append (plist-get inserted :content) nil))))))
                (should (= 1 (length (plist-get (mevedel-shared-editing--parse
                                                 (plist-get (run "SharedEdit" (list :id (plist-get board :id) :action "insert"
                                                                                    :item legacy :x 300 :y 60))
                                                            :result))
                                                :inserted))))
                ;; The model colours the canvas and sees the board on it.
                (let ((tinted (run "SharedEdit" (list :id (plist-get board :id) :action "background"
                                                      :background "#fffce8"))))
                  (should (equal "#fffce8" (plist-get (mevedel-shared-editing--parse (plist-get tinted :result))
                                                      :background)))
                  (should (plist-get tinted :media)))
                (should-not (plist-get (mevedel-shared-editing--parse
                                        (plist-get (run "SharedEdit" (list :id (plist-get board :id) :action "background"
                                                                           :background ""))
                                                   :result))
                                       :background))))))
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
                          ["patch" "insert" "rename" "background" "revert"]))))))))

(mevedel-deftest mevedel-tool-editing--summaries
  (:doc "ToolSearch finds the shared tools by what people call shared items")
  (let ((mevedel-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil)
        (session (mevedel-skills-test--make-session)))
    (mevedel-tool-editing--register)
    (setf (mevedel-session-tool-catalog session)
          (mapcar (lambda (name)
                    (let ((tool (mevedel-tool-get name)))
                      (cons (list (mevedel-tool-category tool) name)
                            (mevedel-tool-summary tool))))
                  '("SharedRead" "SharedCreate" "SharedEdit")))
    (let ((found (lambda (query)
                   (mapcar #'cadar (mevedel-tools--search-catalog session query)))))
      (should (member "SharedCreate" (funcall found "whiteboard")))
      (should (equal '("SharedRead" "SharedCreate" "SharedEdit")
                     (funcall found "whiteboard")))
      (should (equal '("SharedRead" "SharedCreate" "SharedEdit")
                     (funcall found "shared")))
      (should (member "SharedCreate" (funcall found "document"))))))

(mevedel-deftest mevedel-tool-editing--restore-nulls
  (:doc "Lower gptel's lossless null marker in model-supplied args before host encoding")
  (let ((shape (list :type "rectangle" :x 0 :y 0 :width 10 :height 10)))
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
