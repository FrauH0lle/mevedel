;;; test-niwa-tool-editing.el --- Shared editing tool authority -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared editing tests through the real host and existing public seams.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-tools)
(require 'niwa-pipeline)
(require 'niwa-tool-editing)
(require 'niwa-shared-editing)
(require 'niwa-shared-library)

(niwa-deftest niwa-tool-editing--call
		 (:doc "The real tool pipeline commits authorized edits and fences cancelled work" :quiet t)
		 (let* ((directory (make-temp-file "niwa-editing-tools-" t))
			(workspace (niwa-workspace--create :root directory))
			(session (niwa-session--create :save-path directory
							  :workspace workspace :working-directory directory
							  :permission-mode 'full-auto ))
			(buffer (generate-new-buffer " *editing-tools-test*"))
			(niwa-shared-editing--runtimes (make-hash-table :test #'equal))
			(niwa-artifact-lease--held (make-hash-table :test #'equal))
			(niwa-tool--registry (copy-hash-table niwa-tool--registry)))
		   (unwind-protect
		       (with-current-buffer buffer
			 (setq-local niwa--session session)
			 (setf (niwa-session-root-buffer session) buffer)
			 (niwa-tool-editing--register)
			 (let ((tool (niwa-tool-get "SharedCreate")) reply (calls 0))
			   (funcall
			    (niwa-pipeline-run-tool-outcome
			     tool (lambda (value) (cl-incf calls) (setq reply value))
			     '(:kind "whiteboard" :title "Cancelled")))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (or (plist-get (niwa-shared-editing--runtime workspace) :active)
					     (plist-get (niwa-shared-editing--runtime workspace) :queue))
					 (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'cancelled (plist-get reply :reason)))
			   (should (= calls 1))
			   (should-not (niwa-shared-editing-list workspace))
			   (setq reply nil)
			   (niwa-pipeline-run-tool-outcome
			    tool (lambda (value) (setq reply value))
			    '(:kind "document" :title "Accepted"))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (not reply) (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'success (plist-get reply :status)))
			   (should (= 1 (length (niwa-shared-editing-list workspace))))
			   ;; The model's item is attached to its session.
			   (should (equal (mapcar (lambda (item) (plist-get item :id)) (niwa-shared-editing-list workspace))
					  (niwa-session-attached-artifacts session)))
			   (let (cancel
				 (niwa-artifact-backend-change-hook nil))
			     (setq reply nil calls 0
				   niwa-artifact-backend-change-hook
				   (list (lambda (&rest _) (funcall cancel) (niwa-shared-editing-stop))))
			     (setq cancel
				   (niwa-pipeline-run-tool-outcome
				    tool (lambda (value) (cl-incf calls) (setq reply value))
				    '(:kind "whiteboard" :title "Committed before cancellation")))
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (not reply) (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should (eq 'success (plist-get reply :status)))
			     (should (= 1 calls))
			     (should (= 2 (length (niwa-shared-editing-list workspace)))))
				   ;; A blank title is refused before anything is queued.
				   (setq reply nil)
				   (niwa-pipeline-run-tool-outcome tool (lambda (value) (setq reply value))
								      '(:kind "document" :title "   "))
				   (let ((deadline (+ (float-time) 10)))
				     (while (and (not reply) (< (float-time) deadline))
				       (accept-process-output nil 0.05)))
				   (should (eq 'error (plist-get reply :status)))
				   (should (string-match-p "1 to 200 characters" (format "%s" (plist-get reply :result))))
				   (should (= 2 (length (niwa-shared-editing-list workspace))))
			   ;; Item leases, not the session's buffer, decide who may edit.
			   (dolist (restriction '(plan deny))
			     (setq reply nil)
			     (setf (niwa-session-plan-mode session) (eq restriction 'plan)
				   (niwa-session-permission-rules session)
				   (when (eq restriction 'deny) '(("SharedCreate" :action deny))))
			     (setq buffer-read-only (eq restriction 'read-only))
			     (niwa-pipeline-run-tool-outcome tool (lambda (value) (setq reply value))
								'(:kind "whiteboard" :title "Refused"))
			     (let ((deadline (+ (float-time) 10)))
			       (while (and (not reply) (< (float-time) deadline))
				 (accept-process-output nil 0.05)))
			     (should (memq (plist-get reply :status) '(error denied)))
			     (should (= 2 (length (niwa-shared-editing-list workspace)))))
			   (setq buffer-read-only nil)))
		     (niwa-shared-editing-stop)
		     (niwa-artifact-lease-release-all)
		     (when (buffer-live-p buffer) (kill-buffer buffer))
		     (delete-directory directory t))))

(defmacro niwa-tool-editing-test--with-session (&rest body)
  "Run BODY in a live session buffer with shared tools, then clean up."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "niwa-editing-tools-" t))
          (workspace (niwa-workspace--create :root directory))
          (session (niwa-session--create :save-path directory
                                            :workspace workspace :working-directory directory
                                            :permission-mode 'full-auto ))
          (buffer (generate-new-buffer " *editing-tools-test*"))
          (niwa-shared-editing--runtimes (make-hash-table :test #'equal))
          (niwa-artifact-lease--held (make-hash-table :test #'equal))
          (niwa-shared-library-directory (file-name-concat directory "libraries"))
          (niwa-tool--registry (copy-hash-table niwa-tool--registry)))
     (unwind-protect
         (with-current-buffer buffer
           (setq-local niwa--session session)
           (setf (niwa-session-root-buffer session) buffer)
           (niwa-tools-register)
           (require 'gptel-request)
           (cl-letf (((symbol-function 'gptel--model-capable-p)
                      (lambda (capability &optional _model) (eq capability 'media)))
                     ((symbol-function 'gptel--model-mime-capable-p)
                      (lambda (_mime &optional _model) t)))
           (cl-labels ((run (name args &optional status)
                         (let (reply)
                           (niwa-pipeline-run-tool-outcome
                            (niwa-tool-get name) (lambda (value) (setq reply value)) args)
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (unless (eq (or status 'success) (plist-get reply :status))
                             (ert-fail (list name args (plist-get reply :status) (plist-get reply :result))))
                           reply))
                       (text (reply)
                         (let ((result (plist-get reply :result)))
                           (if (stringp result) result (format "%S" result))))
                       (parse (reply) (niwa-shared-editing--parse (plist-get reply :result))))
             ,@body)))
       (niwa-shared-editing-stop)
       (niwa-artifact-lease-release-all)
       (when (buffer-live-p buffer) (kill-buffer buffer))
       (delete-directory directory t))))

(niwa-deftest niwa-tool-editing--edit
  (:doc "The model lists library items through shared://library and inserts one into a board" :quiet t)
  (niwa-tool-editing-test--with-session
    ;; Version 1 libraries hold bare element lists without item ids.
    (make-directory niwa-shared-library-directory t)
    (with-temp-file (file-name-concat niwa-shared-library-directory "Legacy.excalidrawlib")
      (insert "{\"type\":\"excalidrawlib\",\"version\":1,\"library\":[[{\"id\":\"r\",\"type\":\"rectangle\",\"x\":0,\"y\":0,\"width\":80,\"height\":40}]]}"))
    (with-temp-file (file-name-concat niwa-shared-library-directory "Unsupported.excalidrawlib")
      (insert "{\"type\":\"excalidrawlib\",\"version\":99,\"libraryItems\":[]}"))
    (let* ((listing (text (run "Read" '(:file_path "shared://library"))))
           (legacy (and (string-match "\\(Legacy/[^ ]+\\) ·" listing) (match-string 1 listing))))
      (should (string-search "Built-in/builtin-database · " listing))
      (should (string-match-p "Unsupported.*Not an Excalidraw library" listing))
      (should legacy)
      (should (plist-get (run "Read" '(:file_path "shared://library/Legacy/sheet.png")) :media))
      ;; One item previews at its ref.
      (should (plist-get (run "Read" (list :file_path (format "shared://library/%s/view.png" legacy)))
                         :media))
      (should (string-match-p "No element library Missing"
                              (text (run "Read" '(:file_path "shared://library/Missing") 'error))))
      (let* ((board (parse (run "SharedCreate" '(:kind "whiteboard" :title "Stores"))))
             (id (plist-get board :id))
             (inserted (parse (run "SharedEdit" (list :id id :action "insert"
                                                      :item "Built-in/builtin-database" :x 40 :y 60)))))
        (should (equal (concat "shared://" id) (plist-get board :address)))
        (should (= 4 (length (plist-get inserted :inserted))))
        (should (= 40 (apply #'min (mapcar (lambda (line)
                                              (plist-get (niwa-shared-editing--parse (substring line 13)) :x))
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

(niwa-deftest niwa-tool-editing-view
  (:doc "Read and Grep serve shared:// views; SharedEdit names targets by their hash" :quiet t)
  (niwa-tool-editing-test--with-session
    (should (string-search "No shared whiteboards or documents"
                           (text (run "Read" '(:file_path "shared://")))))
    (setq-local niwa--current-request (niwa-request--create :session session))
    (let* ((board (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plan"))))
           (id (plist-get board :id))
           (address (concat "shared://" id)))
      ;; Created and changed items are versioned when the turn settles; a
      ;; no-op edit is not.
      (should (equal (list (cons workspace id))
                     (niwa-request-edited-artifacts niwa--current-request)))
      (setf (niwa-request-edited-artifacts niwa--current-request) nil)
      (run "SharedEdit" (list :id id :action "rename" :title "Plan"))
      (should-not (niwa-request-edited-artifacts niwa--current-request))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "api" :after (list :type "rectangle" :x 0 :y 0
                                                                            :width 100 :height 50))
                                               (list :id "api-label" :after
                                                     (list :type "text" :x 0 :y 0 :width 0 :height 0
                                                           :text "Billing API" :containerId "api")))))
      (should (equal (list (cons workspace id))
                     (niwa-request-edited-artifacts niwa--current-request)))
      (should (string-search (format "%s\twhiteboard \"Plan\"" address)
                             (text (run "Read" '(:file_path "shared://")))))
      (let* ((overview (text (run "Read" (list :file_path address))))
             (hash (and (string-match "\\([0-9a-f]\\{12\\}\\) {\"id\":\"api\"" overview)
                        (match-string 1 overview))))
        (should (string-search "whiteboard \"Plan\" · revision 3 · 2 elements" overview))
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
          (should (= 4 (plist-get edited :revision)))
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

(niwa-deftest niwa-tool-editing-view-long-lines
  (:doc "Read recovers long document text without physical line truncation" :quiet t)
  (niwa-tool-editing-test--with-session
    (let* ((created (parse (run "SharedCreate" '(:kind "document" :title "Long text"))))
           (id (plist-get created :id))
           (address (concat "shared://" id))
           (tail "TAIL-SENTINEL")
           (written (concat (make-string 3000 ?x) tail)))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "long" :after
                                                     (list :type "paragraph"
                                                           :content (vector (list :type "text" :text written)))))))
      (let ((overview (text (run "Read" (list :file_path address))))
            (full (text (run "Read" (list :file_path (concat address "/elements/long"))))))
        (should (string-search (concat "Read " address "/elements/long") overview))
        (should (string-search tail full))
        (should-not (string-search "[line truncated" full))))))

(niwa-deftest niwa-tool-editing-search-long-lines
  (:doc "Grep searches full text beyond overview previews and rendering chunks" :quiet t)
  (niwa-tool-editing-test--with-session
    (setf (niwa-session-sandbox-mode session) 'off)
    (setq-local niwa--current-request (niwa-request--create :session session))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Search long text"))) :id))
           (address (concat "shared://" id))
           (needle "CROSSBOUNDARYMARKER"))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "long" :after
                                                     (list :type "paragraph" :content
                                                           (vector (list :type "text" :text
                                                                         (concat (make-string 3185 ?x) needle))))))))
      (should-not (string-search needle (text (run "Read" (list :file_path address)))))
      (let ((start (symbol-function 'niwa-execution-start-helper)) commands)
        (cl-letf (((symbol-function 'niwa-execution-start-helper)
                   (lambda (&rest args)
                     (push (nth 2 args) commands)
                     (apply start args))))
          (dolist (path (list "shared://" address (concat address "/elements/long")))
            (should (string-search
                     path (text (run "Grep" (list :path path :pattern needle
                                                 :output_mode "files_with_matches")))))))
        (dolist (command commands)
          (should (member "--files-with-matches" command))
          (should-not (member "--json" command)))))))

(niwa-deftest niwa-tool-editing-search-files-bounded
  (:doc "File-only shared matching short-circuits within a small helper output budget" :quiet t)
  (niwa-tool-editing-test--with-session
    (setf (niwa-session-sandbox-mode session) 'off)
    (setq-local niwa--current-request (niwa-request--create :session session))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Bounded files"))) :id))
           (path (concat "shared://" id "/elements/large")))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "large" :after
                                                     (list :type "paragraph" :content
                                                           (vector (list :type "text" :text (make-string 100000 ?x))))))))
      (let* ((niwa-execution-process-output-limit (* 256 1024))
             (hits (text (run "Grep" (list :path path :pattern "x{10}" :output_mode "files_with_matches")))))
        (should (string-search path hits))
        (should-not (string-search "partial" hits))))))

(niwa-deftest niwa-tool-editing-search-read-location
  (:doc "Grep locations read the actual matching long shared content" :quiet t)
  (niwa-tool-editing-test--with-session
    (setf (niwa-session-sandbox-mode session) 'off)
    (setq-local niwa--current-request (niwa-request--create :session session))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Locations"))) :id))
           (address (concat "shared://" id "/elements/long"))
           (needle "LOCATION-NEEDLE"))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "long" :after
                                                     (list :type "paragraph" :content
                                                           (vector (list :type "text" :text
                                                                         (concat (make-string 7000 ?x) needle))))))))
      (let ((hits (text (run "Grep" (list :path address :pattern needle :output_mode "content")))))
        (should (string-match (concat (regexp-quote address) ":\\([0-9]+\\):") hits))
        (let ((line (string-to-number (match-string 1 hits))))
          (should (string-search needle (text (run "Read" (list :file_path address
                                                                :offset line :limit 1))))))))))

(niwa-deftest niwa-tool-editing-search-read-context
  (:doc "Shared search maps later rows, context, modes and overview hits to Read" :quiet t)
  (niwa-tool-editing-test--with-session
    (setf (niwa-session-sandbox-mode session) 'off)
    (setq-local niwa--current-request (niwa-request--create :session session))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Later rows"))) :id))
           (address (concat "shared://" id)))
      (cl-labels
          ((host (args)
             (let (reply)
               (niwa-shared-editing-call workspace args (lambda (value) (setq reply value)))
               (let ((deadline (+ (float-time) 10)))
                 (while (and (not reply) (< (float-time) deadline)) (accept-process-output nil 0.05)))
               (should-not (plist-get reply :error))
               (should reply)))
           (verify (path needle)
             (let ((hits (text (run "Grep" (list :path path :pattern needle :output_mode "content"
                                                :context 1)))))
               (ert-info ((format "Grep %S at %s" needle path))
                 (should (string-match "\\(shared://[^[:space:]]+\\):\\([0-9]+\\):" hits)))
               (let ((target (match-string 1 hits)) (line (string-to-number (match-string 2 hits))))
                 (should (string-search needle (text (run "Read" (list :file_path target :offset line :limit 1)))))
                 (should (string-search (format "%s-%d-" target (1- line)) hits))))))
        (host (list :action "discussion-post" :id id :opId "long-message" :messageId "long-message"
                    :actor "Alice" :target '(:kind "whole") :text (make-string 7000 ?x)))
        (host (list :action "discussion-post" :id id :opId "later-message" :messageId "later-message"
                    :actor "Alice" :target '(:kind "whole") :text "LATER-DISCUSSION-NEEDLE"))
        (verify (concat address "/discussions") "LATER-DISCUSSION-NEEDLE")
        (run "SharedEdit" (list :id id :action "rename" :title "SHORT-HISTORY-NEEDLE"))
        (run "SharedEdit" (list :id id :action "patch"
                                :changes (vconcat
                                          (cl-loop for n below 100 collect
                                                   (list :id (format "history-object-%04d" n)
                                                         :after '(:type "paragraph"))))))
        (verify (concat address "/history") "SHORT-HISTORY-NEEDLE")
        (run "SharedEdit" (list :id id :action "patch"
                                :changes (vector (list :id "long" :after
                                                       (list :type "paragraph" :content
                                                             (vector (list :type "text" :text
                                                                           (concat (make-string 7000 ?x) "OVERVIEW-NEEDLE"))))))))
        (verify address "OVERVIEW-NEEDLE")
        (let ((path (concat address "/elements/long")))
          (should (string-search path (text (run "Grep" (list :path address :pattern "OVERVIEW-NEEDLE")))))
          (should (string-search path (text (run "Grep" (list :path address :pattern "OVERVIEW-NEEDLE"
                                                              :glob "long")))))
          (should (string-search (concat path ":1")
                                 (text (run "Grep" (list :path address :pattern "OVERVIEW-NEEDLE" :output_mode "count")))))
          (let ((hits (text (run "Grep" (list :path path :pattern "OVERVIEW-NEEDLE"
                                             :output_mode "content" :context 1 :-A 3 :-B 2)))))
            (should (string-match (concat (regexp-quote path) ":\\([0-9]+\\):") hits))
            (let ((line (string-to-number (match-string 1 hits))))
              (should (string-search (format "%s-%d-" path (+ line 3)) hits))
              (should (string-search (format "%s-%d-" path (- line 2)) hits))))
          (should (string-search "OVERVIEW-NEEDLE"
                                 (text (run "Grep" (list :path path :pattern "OVERVIEW-NEEDLE"
                                                        :output_mode "content" :-n :json-false))))))))))

(defconst niwa-tool-editing-test--png-base64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A valid 1x1 PNG.")

(defconst niwa-tool-editing-test--jpeg-base64
  "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDABALDA4MChAODQ4SERATGCgaGBYWGDEjJR0oOjM9PDkzODdASFxOQERXRTc4UG1RV19iZ2hnPk1xeXBkeFxlZ2P/2wBDARESEhgVGC8aGi9jQjhCY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2P/wAARCAADAAQDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDnaKKK88+vP//Z"
  "A valid 4x3 JPEG.")

(defconst niwa-tool-editing-test--webp-base64
  "UklGRiIAAABXRUJQVlA4IBYAAAAwAQCdASoBAAEADsD+JaQAA3AAAAAA"
  "A valid 1x1 WebP.")

(defun niwa-tool-editing-test--write (path content)
  "Write the unibyte CONTENT to PATH, creating its directory; return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert content)))
  path)

(defun niwa-tool-editing-test--png (path)
  "Write the sample PNG to PATH and return PATH."
  (niwa-tool-editing-test--write
   path (base64-decode-string niwa-tool-editing-test--png-base64)))

(defun niwa-tool-editing-test--bytes (path)
  "Return PATH's content as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun niwa-tool-editing-test--file-id (edited)
  "Return the fileId in the first changed line of the parsed edit result EDITED."
  (let ((line (aref (plist-get edited :changed) 0)))
    (and (string-match "\"fileId\":\"\\([0-9a-f]\\{40\\}\\)\"" line)
         (match-string 1 line))))

(niwa-deftest niwa-tool-editing--image
  (:doc "SharedEdit image brings a file or readable resource image into a board or document" :quiet t)
  ,test
  (test)
  :doc "puts a workspace file on a board, where it reads back as an image"
  (niwa-tool-editing-test--with-session
    (let* ((file (niwa-tool-editing-test--png (file-name-concat directory "plot.png")))
           (link (file-name-concat directory "link.png"))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plots"))) :id))
           (edit (run "SharedEdit" (list :id id :action "image" :source file :x 40 :y 50 :width 200)))
           (line (aref (plist-get (parse edit) :changed) 0))
           (file-id (niwa-tool-editing-test--file-id (parse edit))))
      (should file-id)
      (should (string-search "\"type\":\"image\"" line))
      (should (string-search "\"x\":40" line))
      (should (string-search "\"width\":200" line))
      (should (plist-get edit :media))
      (should (plist-get (run "Read" (list :file_path (format "shared://%s/images/%s" id file-id)))
                         :media))
      ;; A symlink to an image is read through to the image.
      (make-symbolic-link file link)
      (should (equal file-id
                     (niwa-tool-editing-test--file-id
                      (parse (run "SharedEdit" (list :id id :action "image" :source link
                                                     :x 0 :y 0))))))))
  :doc "stores JPEG and WebP images as they are, from an artifact:// address or a path"
  (niwa-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Photos"))) :id))
           (webp (niwa-tool-editing-test--write
                  (file-name-concat directory "pic.webp")
                  (base64-decode-string niwa-tool-editing-test--webp-base64))))
      (niwa-tool-editing-test--write
       (file-name-concat directory "tool-results" "photo.jpg")
       (base64-decode-string niwa-tool-editing-test--jpeg-base64))
      (cl-loop for (source mime) in `(("artifact://photo.jpg" "image/jpeg") (,webp "image/webp"))
               for file-id = (niwa-tool-editing-test--file-id
                              (parse (run "SharedEdit" (list :id id :action "image" :source source
                                                             :x 0 :y 0))))
               do (should file-id)
               (should (equal mime (plist-get
                                    (car (plist-get
                                          (run "Read" (list :file_path (format "shared://%s/images/%s" id file-id)))
                                          :media))
                                    :mime))))))
  :doc "puts a work:// image after a chosen document block, described, and a second at the end"
  (niwa-tool-editing-test--with-session
    (niwa-tool-editing-test--png (file-name-concat directory "local" "plot.png"))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Report"))) :id))
           (first-block
            (let ((overview (text (run "Read" (list :file_path (concat "shared://" id))))))
              (should (string-match "\"id\":\"\\([^\"]+\\)\"" overview))
              (match-string 1 overview)))
           (head (parse (run "SharedEdit" (list :id id :action "image" :source "work://plot.png"
                                                :alt "Plot" :afterId first-block)))))
      (should (string-search "\"alt\":\"Plot\"" (aref (plist-get head :changed) 0)))
      (run "SharedEdit" (list :id id :action "image" :source "work://plot.png"))
      (let ((overview (text (run "Read" (list :file_path (concat "shared://" id))))))
        (should (= 2 (with-temp-buffer
                       (insert overview)
                       (how-many (regexp-quote (format "shared://%s/images/" id)) (point-min)))))
        (should (< (string-match "\"type\":\"paragraph\"" overview)
                   (string-match "\"type\":\"image\"" overview))))))
  :doc "copies an image between shared items by address, keeping its content-addressed file"
  (niwa-tool-editing-test--with-session
    (let* ((file (niwa-tool-editing-test--png (file-name-concat directory "plot.png")))
           (one (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "One"))) :id))
           (two (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Two"))) :id))
           (file-id (niwa-tool-editing-test--file-id
                     (parse (run "SharedEdit" (list :id one :action "image" :source file :x 0 :y 0)))))
           (copied (parse (run "SharedEdit" (list :id two :action "image" :x 0 :y 0
                                                  :source (format "shared://%s/images/%s" one file-id)))))
           (snapshot (parse (run "SharedEdit" (list :id two :action "image" :x 300 :y 0
                                                    :source (format "shared://%s/view.png" one))))))
      (should (equal file-id (niwa-tool-editing-test--file-id copied)))
      (should-not (equal file-id (niwa-tool-editing-test--file-id snapshot)))))
  :doc "explains what is wrong with a source that is no usable image and changes nothing"
  (niwa-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Refusals"))) :id))
           (refuse (lambda (source)
                     (text (run "SharedEdit" (list :id id :action "image" :source source :x 0 :y 0)
                                'error))))
           (notes (niwa-tool-editing-test--write
                   (file-name-concat directory "local" "notes.txt") "plain text"))
           (huge (niwa-tool-editing-test--write
                  (file-name-concat directory "huge.png") (make-string 4800000 ?x)))
           (gif (niwa-tool-editing-test--write
                 (file-name-concat directory "anim.gif") "GIF89a\1\0\1\0")))
      (should (string-search "File not found" (funcall refuse (file-name-concat directory "missing.png"))))
      (should (string-search "is a directory" (funcall refuse directory)))
      (should (string-search "Not a supported image" (funcall refuse notes)))
      (should (string-search "Not a supported image" (funcall refuse "work://notes.txt")))
      (should (string-search "Not a supported image" (funcall refuse gif)))
      (should (string-search "limited to" (funcall refuse huge)))
      (should (string-search "not an image: it reads as text"
                             (funcall refuse (concat "shared://" id))))
      (should (string-search "needs source"
                             (text (run "SharedEdit" (list :id id :action "image") 'error))))
      (should (string-search "0 elements" (text (run "Read" (list :file_path (concat "shared://" id))))))))
  :doc "leaves the item unchanged when reading the source is denied or the session is planning"
  (niwa-tool-editing-test--with-session
    (let* ((file (niwa-tool-editing-test--png (file-name-concat directory "secret.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Guarded"))) :id))
           (attempt (lambda ()
                      (run "SharedEdit" (list :id id :action "image" :source file :x 0 :y 0)
                           'error))))
      (setf (niwa-session-permission-rules session)
            (list (list "*" :path file :action 'deny)))
      (funcall attempt)
      (setf (niwa-session-permission-rules session) nil
            (niwa-session-plan-mode session) t)
      (funcall attempt)
      (setf (niwa-session-plan-mode session) nil)
      (should (string-search "0 elements" (text (run "Read" (list :file_path (concat "shared://" id)))))))))

(niwa-deftest niwa-tool-editing--save-image
  (:doc "SharedSaveImage writes a shared image to a new file" :quiet t)
  ,test
  (test)
  :doc "saves an embedded image and the board as drawn, creating folders and never overwriting"
  (niwa-tool-editing-test--with-session
    (setq-local niwa--current-request
                (niwa-request--create :session session
                                         :file-snapshots (make-hash-table :test #'equal)))
    (let* ((file (niwa-tool-editing-test--png (file-name-concat directory "plot.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plots"))) :id))
           (file-id (niwa-tool-editing-test--file-id
                     (parse (run "SharedEdit" (list :id id :action "image" :source file :x 0 :y 0)))))
           (source (format "shared://%s/images/%s" id file-id))
           (out (file-name-concat directory "exports" "saved.png"))
           (view (file-name-concat directory "exports" "board.png")))
      (should (string-search "Saved" (text (run "SharedSaveImage" (list :source source :file_path out)))))
      (should (equal (niwa-tool-editing-test--bytes file)
                     (niwa-tool-editing-test--bytes out)))
      ;; Rewind restores the absence the snapshot recorded.
      (should (null (gethash out (niwa-request-file-snapshots niwa--current-request) 'unrecorded)))
      (should (string-search "already exists"
                             (text (run "SharedSaveImage" (list :source source :file_path out) 'error))))
      (should (equal (niwa-tool-editing-test--bytes file)
                     (niwa-tool-editing-test--bytes out)))
      (run "SharedSaveImage" (list :source (format "shared://%s/view.png" id) :file_path view))
      (should (equal "\211PNG" (substring (niwa-tool-editing-test--bytes view) 0 4)))))
  :doc "saves a document's image by the key its source shows"
  (niwa-tool-editing-test--with-session
    (let* ((file (niwa-tool-editing-test--png (file-name-concat directory "plot.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Report"))) :id))
           (out (file-name-concat directory "from-doc.png")))
      (run "SharedEdit" (list :id id :action "image" :source file))
      (let ((overview (text (run "Read" (list :file_path (concat "shared://" id))))))
        (should (string-match (format "shared://%s/images/\\([^\"]+\\)\"" id) overview))
        (run "SharedSaveImage" (list :source (format "shared://%s/images/%s" id (match-string 1 overview))
                                     :file_path out)))
      (should (equal (niwa-tool-editing-test--bytes file)
                     (niwa-tool-editing-test--bytes out)))))
  :doc "refuses sources it cannot serve, missing images, and a denied destination without writing"
  (niwa-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Empty"))) :id))
           (out (file-name-concat directory "never.png"))
           (refuse (lambda (source)
                     (text (run "SharedSaveImage" (list :source source :file_path out) 'error)))))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse "work://plot.png")))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse "/etc/hostname")))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse (concat "shared://" id))))
      (should (string-search "No image" (funcall refuse (format "shared://%s/images/missing" id))))
      (setf (niwa-session-permission-rules session)
            (list (list "*" :path out :action 'deny)))
      (should (funcall refuse (format "shared://%s/view.png" id)))
      (should-not (file-exists-p out)))))

(niwa-deftest niwa-tool-editing--register
  (:doc "Shared tools serialize through the provider schema with correct optional arguments")
  (let ((niwa-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil))
    (niwa-tool-editing--register)
    (dolist (spec '(("SharedCreate" ["kind" "title"])
                    ("SharedEdit" ["id" "action"])
                    ("SharedSaveImage" ["source" "file_path"])))
      (let* ((tool (niwa-tool-gptel-tool (niwa-tool-get (car spec))))
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
                          ["patch" "insert" "image" "rename" "background" "revert"]))
           (let ((source (plist-get properties :source)))
             (should (stringp (plist-get source :description)))
             (should-not (plist-member source :access)))))))))

(niwa-deftest niwa-tool-editing--summaries
  (:doc "ToolSearch finds the shared tools by what people call shared items")
  (let ((niwa-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil)
        (session (niwa-skills-test--make-session)))
    (niwa-tool-editing--register)
    (setf (niwa-session-tool-catalog session)
          (mapcar (lambda (name)
                    (let ((tool (niwa-tool-get name)))
                      (cons (list (niwa-tool-category tool) name)
                            (niwa-tool-summary tool))))
                  '("SharedCreate" "SharedEdit")))
    (let ((found (lambda (query)
                   (mapcar #'cadar (niwa-tools--search-catalog session query)))))
      (should (member "SharedCreate" (funcall found "whiteboard")))
      (should (equal '("SharedCreate" "SharedEdit") (funcall found "whiteboard")))
      (should (equal '("SharedCreate" "SharedEdit") (funcall found "shared")))
      (should (member "SharedCreate" (funcall found "document"))))))

(niwa-deftest niwa-tool-editing--restore-nulls
  (:doc "Lower gptel's lossless null marker in model-supplied args before host encoding")
  (let ((shape (list :type "rectangle" :x 0 :y 0 :width 10 :height 10)))
    (should (equal (niwa-tool-editing--restore-nulls
                    (list :id "a" :before :null :after shape))
                   (list :id "a" :before nil :after shape)))
    (should (equal (niwa-tool-editing--restore-nulls
                    (list :changes (vconcat (list (list :id "a" :before :null :after shape)))))
                   (list :changes (vconcat (list (list :id "a" :before nil :after shape))))))
    (should (equal (niwa-tool-editing--restore-nulls "null") "null"))
    (should (equal (niwa-tool-editing--restore-nulls (list :a 1 :b [1 2] :c :json-false))
                   (list :a 1 :b [1 2] :c :json-false)))
    (should (equal (niwa-tool-editing--restore-nulls
                    '(:box (710 143 85 1) :points ((1 2) (3 4))))
                   '(:box [710 143 85 1] :points [[1 2] [3 4]])))
    (should (eq (niwa-tool-editing--restore-nulls (make-symbol ":null")) nil))
    (should (eq (niwa-tool-editing--restore-nulls :null) nil))))

(provide 'test-niwa-tool-editing)
;;; test-niwa-tool-editing.el ends here
