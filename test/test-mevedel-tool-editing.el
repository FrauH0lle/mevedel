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
(require 'mevedel-shared-library)

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

(defmacro mevedel-tool-editing-test--with-session (&rest body)
  "Run BODY in a live session buffer with shared tools, then clean up."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "mevedel-editing-tools-" t))
          (workspace (mevedel-workspace--create :root directory))
          (session (mevedel-session--create :save-path directory
                                            :workspace workspace :working-directory directory
                                            :permission-mode 'full-auto :authority-mode 'pid-lock))
          (buffer (generate-new-buffer " *editing-tools-test*"))
          (mevedel-shared-library-directory (file-name-concat directory "libraries"))
          (mevedel-tool--registry (copy-hash-table mevedel-tool--registry)))
     (unwind-protect
         (with-current-buffer buffer
           (setq-local mevedel--session session)
           (setf (mevedel-session-root-buffer session) buffer)
           (mevedel-tools-register)
           (require 'gptel-request)
           (cl-letf (((symbol-function 'gptel--model-capable-p)
                      (lambda (capability &optional _model) (eq capability 'media)))
                     ((symbol-function 'gptel--model-mime-capable-p)
                      (lambda (_mime &optional _model) t)))
           (cl-labels ((run (name args &optional status)
                         (let (reply)
                           (mevedel-pipeline-run-tool-outcome
                            (mevedel-tool-get name) (lambda (value) (setq reply value)) args)
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (unless (eq (or status 'success) (plist-get reply :status))
                             (ert-fail (list name args (plist-get reply :status) (plist-get reply :result))))
                           reply))
                       (text (reply)
                         (let ((result (plist-get reply :result)))
                           (if (stringp result) result (format "%S" result))))
                       (parse (reply) (mevedel-shared-editing--parse (plist-get reply :result))))
             ,@body)))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (mevedel-shared-editing-stop))
         (kill-buffer buffer))
       (delete-directory directory t))))

(mevedel-deftest mevedel-tool-editing--edit
  (:doc "The model lists library items through shared://library and inserts one into a board" :quiet t)
  (mevedel-tool-editing-test--with-session
    ;; Version 1 libraries hold bare element lists without item ids.
    (make-directory mevedel-shared-library-directory t)
    (with-temp-file (file-name-concat mevedel-shared-library-directory "Legacy.excalidrawlib")
      (insert "{\"type\":\"excalidrawlib\",\"version\":1,\"library\":[[{\"id\":\"r\",\"type\":\"rectangle\",\"x\":0,\"y\":0,\"width\":80,\"height\":40}]]}"))
    (let* ((listing (text (run "Read" '(:file_path "shared://library"))))
           (legacy (and (string-match "\\(Legacy/[^ ]+\\) ·" listing) (match-string 1 listing))))
      (should (string-search "Built-in/builtin-database · " listing))
      (should legacy)
      (should (plist-get (run "Read" '(:file_path "shared://library/Legacy/sheet.png")) :media))
      (should (string-match-p "No element library Missing"
                              (text (run "Read" '(:file_path "shared://library/Missing") 'error))))
      (let* ((board (parse (run "SharedCreate" '(:kind "whiteboard" :title "Stores"))))
             (id (plist-get board :id))
             (inserted (parse (run "SharedEdit" (list :id id :action "insert"
                                                      :item "Built-in/builtin-database" :x 40 :y 60)))))
        (should (equal (concat "shared://" id) (plist-get board :address)))
        (should (= 4 (length (plist-get inserted :inserted))))
        (should (= 40 (apply #'min (mapcar (lambda (line)
                                              (plist-get (mevedel-shared-editing--parse (substring line 13)) :x))
                                            (append (plist-get inserted :changed) nil)))))
        (should (= 1 (length (plist-get (parse (run "SharedEdit" (list :id id :action "insert"
                                                                       :item legacy :x 300 :y 60)))
                                        :inserted))))
        ;; The model colours the canvas and sees the board on it.
        (let ((tinted (run "SharedEdit" (list :id id :action "background" :background "#fffce8"))))
          (should (equal "#fffce8" (plist-get (parse tinted) :background)))
          (should (plist-get tinted :media)))
        (should-not (plist-get (parse (run "SharedEdit" (list :id id :action "background" :background "")))
                               :background))))))

(mevedel-deftest mevedel-tool-editing-view
  (:doc "Read and Grep serve shared:// views; SharedEdit names targets by their hash" :quiet t)
  (mevedel-tool-editing-test--with-session
    (should (string-search "No shared whiteboards or documents"
                           (text (run "Read" '(:file_path "shared://")))))
    (let* ((board (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plan"))))
           (id (plist-get board :id))
           (address (concat "shared://" id)))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "api" :after (list :type "rectangle" :x 0 :y 0
                                                                            :width 100 :height 50))
                                               (list :id "api-label" :after
                                                     (list :type "text" :x 0 :y 0 :width 0 :height 0
                                                           :text "Billing API" :containerId "api")))))
      (should (string-search (format "%s\twhiteboard \"Plan\" · revision 2" address)
                             (text (run "Read" '(:file_path "shared://")))))
      (let* ((overview (text (run "Read" (list :file_path address))))
             (hash (and (string-match "\\([0-9a-f]\\{12\\}\\) {\"id\":\"api\"" overview)
                        (match-string 1 overview))))
        (should (string-search "whiteboard \"Plan\" · revision 2 · 2 elements" overview))
        (should hash)
        (should (string-search "Billing API" (text (run "Grep" (list :pattern "Billing" :path address
                                                                      :output_mode "content")))))
        (should (string-search address (text (run "Grep" (list :pattern "Billing" :path "shared://")))))
        (should (string-match-p (concat "hash " hash)
                                (text (run "Read" (list :file_path (concat address "/elements/api"))))))
        (should (plist-get (run "Read" (list :file_path (concat address "/view.png"))) :media))
        (let ((edited (parse (run "SharedEdit" (list :id id :action "patch"
                                                     :changes (vector (list :id "api" :hash hash
                                                                            :set (list :strokeColor "#e03131"))))))))
          (should (= 3 (plist-get edited :revision)))
          (should (string-search "#e03131" (aref (plist-get edited :changed) 0))))
        ;; The hash the model read is stale after its own edit.
        (let ((stale (run "SharedEdit" (list :id id :action "patch"
                                             :changes (vector (list :id "api" :hash hash :set (list :x 5))))
                          'error)))
          (should (equal "stale" (plist-get (parse stale) :code)))
          (should (string-search "#e03131" (text stale))))
        (should (string-search "Shared item not found"
                               (text (run "Read" '(:file_path "shared://missing") 'error))))
        (should (string-search "Unknown shared:// address"
                               (text (run "Read" (list :file_path (concat address "/nope")) 'error))))))))

(mevedel-deftest mevedel-tool-editing--register
  (:doc "Shared tools serialize through the provider schema with correct optional arguments")
  (let ((mevedel-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil))
    (mevedel-tool-editing--register)
    (dolist (spec '(("SharedCreate" ["kind" "title"])
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
                  '("SharedCreate" "SharedEdit")))
    (let ((found (lambda (query)
                   (mapcar #'cadar (mevedel-tools--search-catalog session query)))))
      (should (member "SharedCreate" (funcall found "whiteboard")))
      (should (equal '("SharedCreate" "SharedEdit") (funcall found "whiteboard")))
      (should (equal '("SharedCreate" "SharedEdit") (funcall found "shared")))
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
