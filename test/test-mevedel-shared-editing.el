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

(mevedel-deftest mevedel-shared-editing--send
  (:doc "Large helper requests use small UTF-8 writes without changing JSON framing")
  (let* ((args (list :title (concat (make-string 1020 ?x) "λ 🌱")
                     :data (make-string 10000 ?é)))
         (expected (concat (mevedel-shared-editing--json args) "\n"))
         chunks)
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (_process chunk)
                 (should (<= (string-bytes (encode-coding-string chunk 'utf-8-unix)) 4096))
                 (push chunk chunks))))
      (mevedel-shared-editing--send nil args))
    (should (> (length chunks) 1))
    (should (equal expected (apply #'concat (nreverse chunks))))))

(mevedel-deftest mevedel-shared-editing--process
  (:doc "Helper replies frame fragmented Unicode, multiple lines, and bound unfinished bytes")
  (let ((buffer (generate-new-buffer " *editing-framing*")) replies)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel-shared-editing--runtime
                      (list :active (list :requestId 1 :callback #'ignore)))
          (let* ((process (mevedel-shared-editing--process buffer))
                 (filter (process-filter process))
                 (line "{\"requestId\":1,\"result\":{\"title\":\"λ 🌱\"}}\n"))
            (cl-letf (((symbol-function 'mevedel-shared-editing--accept)
                       (lambda (_buffer _job reply) (push reply replies))))
              (funcall filter process (substring line 0 12))
              (should-not replies)
              (funcall filter process (concat (substring line 12) line))
              (sleep-for 0.01)
              (should (= (length replies) 2))
              (should (equal (plist-get (plist-get (car replies) :result) :title) "λ 🌱"))
              (should-not (process-get process :partial))
              (should (= (process-get process :partial-bytes) 0)))
            ;; A UTF-8 fragment can exceed the byte limit before any newline.
            (process-put process :partial-bytes (1- (* 64 1024 1024)))
            (funcall filter process "λ")
            (should-not (process-live-p process))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (mevedel-shared-editing-stop))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-shared-editing-call
		 ()
  ,test (test)
  :doc "Commits a board through the host helper and reopens durable state"
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
		     (delete-directory directory t)))
  :doc "Availability is optional, read-only, and recovers after runtime and resource repair"
  (let* ((directory (make-temp-file "mevedel-editing-status-" t))
         (resources mevedel-shared-editing--directory)
         (node mevedel-shared-editing-node-program)
         (session (mevedel-session--create))
         (buffer (generate-new-buffer " *editing-status-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-root-buffer session) buffer)
          (insert "> Host draft\nsecond line")
          (setq buffer-read-only t)
          (cl-labels ((call (action)
                       (let (reply)
                         (mevedel-shared-editing-call
                          session (list :action action) (lambda (value) (setq reply value)))
                         (let ((deadline (+ (float-time) 10)))
                           (while (and (not reply) (< (float-time) deadline))
                             (accept-process-output nil 0.05)))
                         (should reply)
                         reply)))
            (let ((mevedel-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (string-match-p "Install Node" (plist-get (call "status") :error)))
              (should (equal [] (plist-get (call "list") :result))))
            (should (eq t (plist-get (plist-get (call "status") :result) :available)))
            (let ((mevedel-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error))))
            (copy-file (file-name-concat resources "host.bundle.mjs")
                       (file-name-concat directory "host.bundle.mjs"))
            (let ((mevedel-shared-editing--directory directory))
              (should (string-match-p "resources" (plist-get (call "status") :error)))
              (dolist (file '("resvg.wasm" "font.ttf"))
                (copy-file (file-name-concat resources file)
                           (file-name-concat directory file)))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            ;; A configured runtime change also invalidates a live helper.
            (let ((mevedel-shared-editing-node-program
                   (file-name-concat directory "missing-node")))
              (should (plist-get (call "status") :error)))
            (let ((mevedel-shared-editing-node-program node))
              (should (eq t (plist-get (plist-get (call "status") :result) :available))))
            (should-not (mevedel-session-save-path session))
            (should (equal (buffer-string) "> Host draft\nsecond line"))
            (should (equal (sort (directory-files directory nil "^[^.]") #'string<)
                           '("font.ttf" "host.bundle.mjs" "resvg.wasm")))))
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
