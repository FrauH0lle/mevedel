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
(require 'mevedel-shared-editing)
(require 'mevedel-shared-library)

(mevedel-deftest mevedel-tool-editing--call
		 (:doc "The real tool pipeline commits authorized edits and fences cancelled work" :quiet t)
		 (let* ((directory (make-temp-file "mevedel-editing-tools-" t))
			(workspace (mevedel-workspace--create :root directory))
			(session (mevedel-session--create :save-path directory
							  :workspace workspace :working-directory directory
							  :permission-mode 'full-auto :authority-mode 'pid-lock))
			(buffer (generate-new-buffer " *editing-tools-test*"))
			(mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
			(mevedel-artifact-lease--held (make-hash-table :test #'equal))
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
			     (while (and (or (plist-get (mevedel-shared-editing--runtime workspace) :active)
					     (plist-get (mevedel-shared-editing--runtime workspace) :queue))
					 (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'cancelled (plist-get reply :reason)))
			   (should (= calls 1))
			   (should-not (mevedel-shared-editing-list workspace))
			   (setq reply nil)
			   (mevedel-pipeline-run-tool-outcome
			    tool (lambda (value) (setq reply value))
			    '(:kind "document" :title "Accepted"))
			   (let ((deadline (+ (float-time) 10)))
			     (while (and (not reply) (< (float-time) deadline))
			       (accept-process-output nil 0.05)))
			   (should (eq 'success (plist-get reply :status)))
			   (should (= 1 (length (mevedel-shared-editing-list workspace))))
			   ;; The model's item is attached to its session.
			   (should (equal (mevedel-shared-editing-ids workspace)
					  (mevedel-session-attached-artifacts session)))
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
			     (should (= 2 (length (mevedel-shared-editing-list workspace)))))
				   ;; A blank title is refused before anything is queued.
				   (setq reply nil)
				   (mevedel-pipeline-run-tool-outcome tool (lambda (value) (setq reply value))
								      '(:kind "document" :title "   "))
				   (let ((deadline (+ (float-time) 10)))
				     (while (and (not reply) (< (float-time) deadline))
				       (accept-process-output nil 0.05)))
				   (should (eq 'error (plist-get reply :status)))
				   (should (string-match-p "1 to 200 characters" (format "%s" (plist-get reply :result))))
				   (should (= 2 (length (mevedel-shared-editing-list workspace))))
			   ;; Item leases, not the session's buffer, decide who may edit.
			   (dolist (restriction '(plan deny))
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
			     (should (= 2 (length (mevedel-shared-editing-list workspace)))))
			   (setq buffer-read-only nil)))
		     (mevedel-shared-editing-stop)
		     (mevedel-artifact-lease-release-all)
		     (when (buffer-live-p buffer) (kill-buffer buffer))
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
          (mevedel-shared-editing--runtimes (make-hash-table :test #'equal))
          (mevedel-artifact-lease--held (make-hash-table :test #'equal))
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
       (mevedel-shared-editing-stop)
       (mevedel-artifact-lease-release-all)
       (when (buffer-live-p buffer) (kill-buffer buffer))
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
    (setq-local mevedel--current-request (mevedel-request--create :session session))
    (let* ((board (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plan"))))
           (id (plist-get board :id))
           (address (concat "shared://" id)))
      ;; Created and changed items are versioned when the turn settles; a
      ;; no-op edit is not.
      (should (equal (list (cons workspace id))
                     (mevedel-request-edited-artifacts mevedel--current-request)))
      (setf (mevedel-request-edited-artifacts mevedel--current-request) nil)
      (run "SharedEdit" (list :id id :action "rename" :title "Plan"))
      (should-not (mevedel-request-edited-artifacts mevedel--current-request))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "api" :after (list :type "rectangle" :x 0 :y 0
                                                                            :width 100 :height 50))
                                               (list :id "api-label" :after
                                                     (list :type "text" :x 0 :y 0 :width 0 :height 0
                                                           :text "Billing API" :containerId "api")))))
      (should (equal (list (cons workspace id))
                     (mevedel-request-edited-artifacts mevedel--current-request)))
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

(mevedel-deftest mevedel-tool-editing-view-long-lines
  (:doc "Read recovers long document text without physical line truncation" :quiet t)
  (mevedel-tool-editing-test--with-session
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

(mevedel-deftest mevedel-tool-editing-search-long-lines
  (:doc "Grep searches full text beyond overview previews and rendering chunks" :quiet t)
  (mevedel-tool-editing-test--with-session
    (setf (mevedel-session-sandbox-mode session) 'off)
    (setq-local mevedel--current-request (mevedel-request--create :session session))
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Search long text"))) :id))
           (address (concat "shared://" id))
           (needle "CROSSBOUNDARYMARKER"))
      (run "SharedEdit" (list :id id :action "patch"
                              :changes (vector (list :id "long" :after
                                                     (list :type "paragraph" :content
                                                           (vector (list :type "text" :text
                                                                         (concat (make-string 3185 ?x) needle))))))))
      (should-not (string-search needle (text (run "Read" (list :file_path address)))))
      (dolist (path (list "shared://" address (concat address "/elements/long")))
        (should (string-search
                 path (text (run "Grep" (list :path path :pattern needle
                                             :output_mode "files_with_matches")))))))))

(defconst mevedel-tool-editing-test--png-base64
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg=="
  "A valid 1x1 PNG.")

(defconst mevedel-tool-editing-test--jpeg-base64
  "/9j/4AAQSkZJRgABAQAAAQABAAD/2wBDABALDA4MChAODQ4SERATGCgaGBYWGDEjJR0oOjM9PDkzODdASFxOQERXRTc4UG1RV19iZ2hnPk1xeXBkeFxlZ2P/2wBDARESEhgVGC8aGi9jQjhCY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2NjY2P/wAARCAADAAQDASIAAhEBAxEB/8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQAAAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJicoKSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKTlJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo6erx8vP09fb3+Pn6/8QAHwEAAwEBAQEBAQEBAQAAAAAAAAECAwQFBgcICQoL/8QAtREAAgECBAQDBAcFBAQAAQJ3AAECAxEEBSExBhJBUQdhcRMiMoEIFEKRobHBCSMzUvAVYnLRChYkNOEl8RcYGRomJygpKjU2Nzg5OkNERUZHSElKU1RVVldYWVpjZGVmZ2hpanN0dXZ3eHl6goOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4uPk5ebn6Onq8vP09fb3+Pn6/9oADAMBAAIRAxEAPwDnaKKK88+vP//Z"
  "A valid 4x3 JPEG.")

(defconst mevedel-tool-editing-test--webp-base64
  "UklGRiIAAABXRUJQVlA4IBYAAAAwAQCdASoBAAEADsD+JaQAA3AAAAAA"
  "A valid 1x1 WebP.")

(defun mevedel-tool-editing-test--write (path content)
  "Write the unibyte CONTENT to PATH, creating its directory; return PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert content)))
  path)

(defun mevedel-tool-editing-test--png (path)
  "Write the sample PNG to PATH and return PATH."
  (mevedel-tool-editing-test--write
   path (base64-decode-string mevedel-tool-editing-test--png-base64)))

(defun mevedel-tool-editing-test--bytes (path)
  "Return PATH's content as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defun mevedel-tool-editing-test--file-id (edited)
  "Return the fileId in the first changed line of the parsed edit result EDITED."
  (let ((line (aref (plist-get edited :changed) 0)))
    (and (string-match "\"fileId\":\"\\([0-9a-f]\\{40\\}\\)\"" line)
         (match-string 1 line))))

(mevedel-deftest mevedel-tool-editing--image
  (:doc "SharedEdit image brings a file or readable resource image into a board or document" :quiet t)
  ,test
  (test)
  :doc "puts a workspace file on a board, where it reads back as an image"
  (mevedel-tool-editing-test--with-session
    (let* ((file (mevedel-tool-editing-test--png (file-name-concat directory "plot.png")))
           (link (file-name-concat directory "link.png"))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plots"))) :id))
           (edit (run "SharedEdit" (list :id id :action "image" :source file :x 40 :y 50 :width 200)))
           (line (aref (plist-get (parse edit) :changed) 0))
           (file-id (mevedel-tool-editing-test--file-id (parse edit))))
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
                     (mevedel-tool-editing-test--file-id
                      (parse (run "SharedEdit" (list :id id :action "image" :source link
                                                     :x 0 :y 0))))))))
  :doc "stores JPEG and WebP images as they are, from an artifact:// address or a path"
  (mevedel-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Photos"))) :id))
           (webp (mevedel-tool-editing-test--write
                  (file-name-concat directory "pic.webp")
                  (base64-decode-string mevedel-tool-editing-test--webp-base64))))
      (mevedel-tool-editing-test--write
       (file-name-concat directory "tool-results" "photo.jpg")
       (base64-decode-string mevedel-tool-editing-test--jpeg-base64))
      (cl-loop for (source mime) in `(("artifact://photo.jpg" "image/jpeg") (,webp "image/webp"))
               for file-id = (mevedel-tool-editing-test--file-id
                              (parse (run "SharedEdit" (list :id id :action "image" :source source
                                                             :x 0 :y 0))))
               do (should file-id)
               (should (equal mime (plist-get
                                    (car (plist-get
                                          (run "Read" (list :file_path (format "shared://%s/images/%s" id file-id)))
                                          :media))
                                    :mime))))))
  :doc "puts a work:// image after a chosen document block, described, and a second at the end"
  (mevedel-tool-editing-test--with-session
    (mevedel-tool-editing-test--png (file-name-concat directory "local" "plot.png"))
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
  (mevedel-tool-editing-test--with-session
    (let* ((file (mevedel-tool-editing-test--png (file-name-concat directory "plot.png")))
           (one (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "One"))) :id))
           (two (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Two"))) :id))
           (file-id (mevedel-tool-editing-test--file-id
                     (parse (run "SharedEdit" (list :id one :action "image" :source file :x 0 :y 0)))))
           (copied (parse (run "SharedEdit" (list :id two :action "image" :x 0 :y 0
                                                  :source (format "shared://%s/images/%s" one file-id)))))
           (snapshot (parse (run "SharedEdit" (list :id two :action "image" :x 300 :y 0
                                                    :source (format "shared://%s/view.png" one))))))
      (should (equal file-id (mevedel-tool-editing-test--file-id copied)))
      (should-not (equal file-id (mevedel-tool-editing-test--file-id snapshot)))))
  :doc "explains what is wrong with a source that is no usable image and changes nothing"
  (mevedel-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Refusals"))) :id))
           (refuse (lambda (source)
                     (text (run "SharedEdit" (list :id id :action "image" :source source :x 0 :y 0)
                                'error))))
           (notes (mevedel-tool-editing-test--write
                   (file-name-concat directory "local" "notes.txt") "plain text"))
           (huge (mevedel-tool-editing-test--write
                  (file-name-concat directory "huge.png") (make-string 4800000 ?x)))
           (gif (mevedel-tool-editing-test--write
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
  (mevedel-tool-editing-test--with-session
    (let* ((file (mevedel-tool-editing-test--png (file-name-concat directory "secret.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Guarded"))) :id))
           (attempt (lambda ()
                      (run "SharedEdit" (list :id id :action "image" :source file :x 0 :y 0)
                           'error))))
      (setf (mevedel-session-permission-rules session)
            (list (list "*" :path file :action 'deny)))
      (funcall attempt)
      (setf (mevedel-session-permission-rules session) nil
            (mevedel-session-plan-mode session) t)
      (funcall attempt)
      (setf (mevedel-session-plan-mode session) nil)
      (should (string-search "0 elements" (text (run "Read" (list :file_path (concat "shared://" id)))))))))

(mevedel-deftest mevedel-tool-editing--save-image
  (:doc "SharedSaveImage writes a shared image to a new file" :quiet t)
  ,test
  (test)
  :doc "saves an embedded image and the board as drawn, creating folders and never overwriting"
  (mevedel-tool-editing-test--with-session
    (setq-local mevedel--current-request
                (mevedel-request--create :session session
                                         :file-snapshots (make-hash-table :test #'equal)))
    (let* ((file (mevedel-tool-editing-test--png (file-name-concat directory "plot.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Plots"))) :id))
           (file-id (mevedel-tool-editing-test--file-id
                     (parse (run "SharedEdit" (list :id id :action "image" :source file :x 0 :y 0)))))
           (source (format "shared://%s/images/%s" id file-id))
           (out (file-name-concat directory "exports" "saved.png"))
           (view (file-name-concat directory "exports" "board.png")))
      (should (string-search "Saved" (text (run "SharedSaveImage" (list :source source :file_path out)))))
      (should (equal (mevedel-tool-editing-test--bytes file)
                     (mevedel-tool-editing-test--bytes out)))
      ;; Rewind restores the absence the snapshot recorded.
      (should (null (gethash out (mevedel-request-file-snapshots mevedel--current-request) 'unrecorded)))
      (should (string-search "already exists"
                             (text (run "SharedSaveImage" (list :source source :file_path out) 'error))))
      (should (equal (mevedel-tool-editing-test--bytes file)
                     (mevedel-tool-editing-test--bytes out)))
      (run "SharedSaveImage" (list :source (format "shared://%s/view.png" id) :file_path view))
      (should (equal "\211PNG" (substring (mevedel-tool-editing-test--bytes view) 0 4)))))
  :doc "saves a document's image by the key its source shows"
  (mevedel-tool-editing-test--with-session
    (let* ((file (mevedel-tool-editing-test--png (file-name-concat directory "plot.png")))
           (id (plist-get (parse (run "SharedCreate" '(:kind "document" :title "Report"))) :id))
           (out (file-name-concat directory "from-doc.png")))
      (run "SharedEdit" (list :id id :action "image" :source file))
      (let ((overview (text (run "Read" (list :file_path (concat "shared://" id))))))
        (should (string-match (format "shared://%s/images/\\([^\"]+\\)\"" id) overview))
        (run "SharedSaveImage" (list :source (format "shared://%s/images/%s" id (match-string 1 overview))
                                     :file_path out)))
      (should (equal (mevedel-tool-editing-test--bytes file)
                     (mevedel-tool-editing-test--bytes out)))))
  :doc "refuses sources it cannot serve, missing images, and a denied destination without writing"
  (mevedel-tool-editing-test--with-session
    (let* ((id (plist-get (parse (run "SharedCreate" '(:kind "whiteboard" :title "Empty"))) :id))
           (out (file-name-concat directory "never.png"))
           (refuse (lambda (source)
                     (text (run "SharedSaveImage" (list :source source :file_path out) 'error)))))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse "work://plot.png")))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse "/etc/hostname")))
      (should (string-search "SharedSaveImage reads shared://" (funcall refuse (concat "shared://" id))))
      (should (string-search "No image" (funcall refuse (format "shared://%s/images/missing" id))))
      (setf (mevedel-session-permission-rules session)
            (list (list "*" :path out :action 'deny)))
      (should (funcall refuse (format "shared://%s/view.png" id)))
      (should-not (file-exists-p out)))))

(mevedel-deftest mevedel-tool-editing--register
  (:doc "Shared tools serialize through the provider schema with correct optional arguments")
  (let ((mevedel-tool--registry (make-hash-table :test #'equal))
        (gptel--known-tools nil))
    (mevedel-tool-editing--register)
    (dolist (spec '(("SharedCreate" ["kind" "title"])
                    ("SharedEdit" ["id" "action"])
                    ("SharedSaveImage" ["source" "file_path"])))
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
                          ["patch" "insert" "image" "rename" "background" "revert"]))
           (let ((source (plist-get properties :source)))
             (should (stringp (plist-get source :description)))
             (should-not (plist-member source :access)))))))))

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
