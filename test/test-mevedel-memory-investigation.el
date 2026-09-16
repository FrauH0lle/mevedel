;;; test-mevedel-memory-investigation.el -- Read-only review tools -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the real bounded investigation surface without a model request.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-scope)
(require 'mevedel-memory-investigation)
(require 'mevedel-system)
(require 'mevedel-sandbox)

(defun mevedel-test-investigation--call (state operation args)
  "Run OPERATION with ARGS in STATE and wait for its bounded result."
  (let (result done)
    (mevedel-memory-investigation-call
     state operation args (lambda (text) (setq result text done t)))
    (with-timeout (5 (error "Investigation did not settle"))
      (while (not done) (accept-process-output nil 0.01)))
    result))

(mevedel-deftest mevedel-memory-investigation-call ()
		 ,test
		 (test)
		 :doc "Read uses frozen memory and admitted journal evidence while source stays read-only"
		 (let* ((directory (make-temp-file "mevedel-investigation-" t))
			(workspace (mevedel-workspace--create :root directory))
			(mevedel-memory-dirs '("memory"))
			(topic (file-name-concat directory "memory" "topic.md"))
			state)
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory topic))
			 (with-temp-file topic (insert "Captured lesson.\n"))
			 (with-temp-file (file-name-concat directory "source.el") (insert ";; Current source.\n"))
			 (let* ((scope (mevedel-memory-scope-capture workspace))
				(id (caar (plist-get scope :roots))))
			   (setq state (mevedel-memory-investigation-create
					scope '((:file "digest.md" :id "digest-id" :session "session-id"
						       :created "2026-09-07T12:00:00Z" :body "Admitted evidence."))
					(lambda () t) (lambda (_reason) (ert-fail "Unexpected exhaustion"))))
			   (with-temp-file topic (insert "Changed after capture.\n"))
			   (should (string-match-p "Captured lesson" (mevedel-test-investigation--call
								      state 'read (list :root id :path "topic.md"))))
			   (should (string-match-p "Current source" (mevedel-test-investigation--call
								     state 'read '(:root "workspace" :path "source.el"))))
			   (should (string-match-p "Admitted evidence" (mevedel-test-investigation--call
									state 'read '(:root "journal" :path "digest.md"))))
			   (dolist (args '((:root "workspace" :path "../escape")
					   (:root "workspace" :path ".mevedel/state/journal/private.json")
					   (:root "journal" :path "unselected.md")
					   (:root "missing" :path "topic.md")))
			     (should (string-prefix-p "Error:" (mevedel-test-investigation--call state 'read args))))))
		     (when state (mevedel-memory-investigation-stop state))
		     (delete-directory directory t)))
		 :doc "Glob and Grep reuse ordinary search over admitted nested snapshots without leaking private paths"
		 (let* ((directory (make-temp-file "mevedel-investigation-search-" t))
			(workspace (mevedel-workspace--create :root directory))
			(mevedel-memory-dirs '("memory"))
			(topic (file-name-concat directory "memory" "nested" "topic.md"))
			(mevedel-sandbox-mode 'off)
			state)
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory topic) t)
			 (with-temp-file topic (insert "Captured unique lesson.\n"))
			 (make-directory (file-name-concat directory "src"))
			 (with-temp-file (file-name-concat directory "src" "code.el") (insert ";; Unique source.\n"))
			 (make-directory (file-name-concat directory ".mevedel" "state" "journal") t)
			 (with-temp-file (file-name-concat directory ".mevedel" "state" "journal" "secret.el")
			   (insert "Unique PRIVATE.\n"))
			 (let* ((scope (mevedel-memory-scope-capture workspace))
				(id (caar (plist-get scope :roots))))
			   (setq state (mevedel-memory-investigation-create scope nil (lambda () t)
									    (lambda (_) (ert-fail "Unexpected limit"))))
			   (should (string-match-p "nested/topic.md"
						   (mevedel-test-investigation--call state 'glob
										     (list :root id :path "." :pattern "*.md"))))
			   (let ((result (mevedel-test-investigation--call state 'grep
									   (list :root id :path "nested" :pattern "unique"))))
			     (should (string-match-p "Captured unique lesson" result))
			     (should-not (string-match-p "mevedel-memory-search-\\|/tmp/" result)))
			   (let ((result (mevedel-test-investigation--call state 'grep
									   '(:root "workspace" :path "." :pattern "Unique"))))
			     (should (string-match-p "src/code.el" result))
			     (should-not (string-match-p "PRIVATE\\|secret.el\\|Captured unique" result)))
			   (should (string-prefix-p "Error:" (mevedel-test-investigation--call
							      state 'grep '(:root "workspace" :path "memory" :pattern "."))))))
		     (when state (mevedel-memory-investigation-stop state))
		     (mevedel-execution-teardown-all)
		     (delete-directory directory t)))
		 :doc "source search marks oversized evidence partial and remains usable on a remote workspace"
		 (let ((directory (make-temp-file "mevedel-investigation-remote-" t))
		       (mevedel-memory-dirs nil)
		       (mevedel-sandbox-mode 'off)
		       state)
		   (unwind-protect
		       (mevedel-test--with-local-shell-tramp '("memory-investigation")
							     (let* ((remote (format "/mevedelmock:memory-investigation:%s/" directory))
								    (workspace (mevedel-workspace--create :root remote)))
							       (with-temp-file (file-name-concat directory "small.el") (insert ";; Source needle.\n"))
							       (with-temp-file (file-name-concat directory "huge.el") (insert (make-string 600000 ?x)))
							       (setq state (mevedel-memory-investigation-create
									    (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore))
							       (should (string-match-p "Source needle" (mevedel-test-investigation--call
													state 'read '(:root "workspace" :path "small.el"))))
							       (should (string-prefix-p
                                                        "Error:"
                                                        (mevedel-test-investigation--call
                                                         state 'read '(:root "workspace" :path "huge.el" :offset 1 :limit 1))))
                                                       (let ((text (mevedel-test-investigation--call state 'grep
													     '(:root "workspace" :path "." :pattern "needle"))))
								 (should (string-prefix-p "[Partial source snapshot" text))
								 (should (string-match-p "small.el" text))
								 (should-not (string-match-p "mevedelmock:\\|mevedel-memory-search-" text)))))
		     (when state (mevedel-memory-investigation-stop state))
		     (mevedel-execution-teardown-all)
		     (delete-directory directory t)))

  :doc "search callbacks can launch follow-ups after empty or asynchronous results"
  (let* ((directory (make-temp-file "mevedel-investigation-followup-" t))
         (default-directory (file-name-as-directory directory))
         (workspace (mevedel-workspace--create :root directory))
         (mevedel-memory-dirs nil)
         (mevedel-sandbox-mode 'off)
         (state (mevedel-memory-investigation-create
                 (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore)))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.el")
            (insert ";; Source evidence.\n"))
          (dolist (case '((glob :root "workspace" :path "." :pattern ".mevedel/**")
                         (glob :root "workspace" :path "." :pattern "*.el")
                         (grep :root "workspace" :path "." :pattern "Absent")
                         (grep :root "workspace" :path "." :pattern "Source"
                               :glob "missing/*.el")))
            (let (done failure result callback-directory)
              (mevedel-memory-investigation-call
               state (car case) (cdr case)
               (lambda (text)
                 (setq result text callback-directory default-directory)
                 (condition-case err
                     (should (= 0 (process-file "true" nil nil nil)))
                   (error (setq failure err)))
                 (setq done t)))
              (with-timeout (5 (ert-fail "Investigation did not settle"))
                (while (not done) (accept-process-output nil 0.01)))
              (should result)
              (should-not (string-prefix-p "Error:" result))
              (should-not failure)
              (should (equal callback-directory default-directory))
              (should-not (mevedel-memory-investigation-active state)))))
      (mevedel-memory-investigation-stop state)
      (mevedel-execution-teardown-all)
      (delete-directory directory t)))

  :doc "bounds UTF-8 results, total returned text, and calls; exhaustion retires the owner once"
		 (let* ((directory (make-temp-file "mevedel-investigation-limits-" t))
			(workspace (mevedel-workspace--create :root directory))
			(mevedel-memory-dirs nil)
			state reason (failures 0))
		   (unwind-protect
		       (progn
			 (with-temp-file (file-name-concat directory "large.txt")
			   (dotimes (_ 100) (insert (make-string 100 ?\u754c) "\n")))
			 (let ((scope (mevedel-memory-scope-capture workspace)))
			   (setq state (mevedel-memory-investigation-create
					scope nil (lambda () t) (lambda (value) (setq reason value) (cl-incf failures))))
			   (dotimes (_ 8)
			     (let ((text (mevedel-test-investigation--call state 'read
									   '(:root "workspace" :path "large.txt"))))
			       (should (<= (string-bytes text) 8192))
			       (should (string-match-p "truncated" text))
			       (should-not (cl-some #'mevedel--invalid-message-char-p text))))
			   (mevedel-memory-investigation-call state 'read '(:root "workspace" :path "large.txt")
							      (lambda (_) (ert-fail "Output beyond budget")))
			   (should (equal reason "Tool output budget exhausted"))
			   (should (= failures 1))
			   (mevedel-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Retired callback")))
			   (should (= failures 1))
			   (setq state (mevedel-memory-investigation-create scope nil (lambda () t)
									    (lambda (value) (setq reason value))))
			   (should (= 64 mevedel-memory-investigation--max-calls))
			   (should (= 65536 mevedel-memory-investigation--max-bytes))
			   (dotimes (_ 64)
			     (mevedel-test-investigation--call state 'read '(:root "unknown" :path "a.md")))
			   (should (= 64 (mevedel-memory-investigation-calls state)))
			   (should-not (mevedel-memory-investigation-stopped state))
			   (mevedel-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Call beyond budget")))
			   (should (equal reason "Tool call budget exhausted"))
			   (should (= 64 (mevedel-memory-investigation-calls state)))
			   (should (mevedel-memory-investigation-stopped state))))
		     (when state (mevedel-memory-investigation-stop state))
		     (delete-directory directory t))))

(mevedel-deftest mevedel-memory-investigation--deliver ()
  ,test
  (test)
  :doc "accepts exactly 64 KiB of bounded results and rejects one more byte without charging it"
  (let* ((delivered 0) (failures 0)
         (state (mevedel-memory-investigation-create
                 nil nil (lambda () t)
                 (lambda (reason)
                   (should (equal "Tool output budget exhausted" reason))
                   (cl-incf failures)))))
    (unwind-protect
        (progn
          (dotimes (_ 8)
            (mevedel-memory-investigation--deliver
             state (lambda (text) (should (= 8192 (string-bytes text))) (cl-incf delivered))
             (make-string 8192 ?x)))
          (should (= 8 delivered))
          (should (= 65536 (mevedel-memory-investigation-bytes state)))
          (should-not (mevedel-memory-investigation-stopped state))
          (dotimes (_ 2)
            (mevedel-memory-investigation--deliver
             state (lambda (_) (ert-fail "Delivery beyond aggregate budget")) "x"))
          (should (= 1 failures))
          (should (= 65536 (mevedel-memory-investigation-bytes state)))
          (should (mevedel-memory-investigation-stopped state)))
      (mevedel-memory-investigation-stop state))))

(mevedel-deftest mevedel-memory-investigation-tools ()
		 ,test
		 (test)
		 :doc "exposes only scoped asynchronous Read/Glob/Grep without changing the global tool registry"
		 (let* ((state (mevedel-memory-investigation-create nil nil (lambda () t) #'ignore))
			(known (copy-tree gptel--known-tools)))
		   (unwind-protect
		       (let ((tools (mevedel-memory-investigation-tools state)))
			 (should (equal '("Read" "Glob" "Grep") (mapcar #'gptel-tool-name tools)))
			 (should (cl-every #'gptel-tool-async tools))
                         (cl-mapc
                          (lambda (tool description)
                            (should (string-prefix-p description
                                                     (gptel-tool-description tool))))
                          tools '("Read one relative file" "Find relative paths"
                                  "Search text with a regular expression"))
			 (should (equal known gptel--known-tools))
			 (let (result)
			   (funcall (gptel-tool-function (car tools)) (lambda (text) (setq result text))
				    "unknown" "missing.md" nil nil)
			   (should (string-prefix-p "Error:" result))))
		     (mevedel-memory-investigation-stop state))))

(mevedel-deftest mevedel-memory-investigation-stop ()
		 ,test
		 (test)
		 :doc "stops a live search child, removes its snapshot, and suppresses late delivery"
		 (let* ((directory (make-temp-file "mevedel-investigation-cancel-" t))
			(workspace (mevedel-workspace--create :root directory))
			(mevedel-memory-dirs nil)
			(bin (file-name-concat directory "bin"))
			(marker (file-name-concat directory "started"))
			(mevedel-sandbox-mode 'off)
			state delivered)
		   (unwind-protect
		       (progn
			 (make-directory bin)
			 (with-temp-file (file-name-concat bin "rg")
			   (insert "#!/bin/sh\nfor value do :; done\nprintf '%s' \"$value\" > "
				   (shell-quote-argument marker) "\nexec sleep 30\n"))
			 (set-file-modes (file-name-concat bin "rg") #o755)
			 (setq state (mevedel-memory-investigation-create
				      (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore))
			 (let ((exec-path (cons bin exec-path)))
			   (mevedel-memory-investigation-call state 'glob
							      '(:root "workspace" :path "." :pattern "*")
							      (lambda (_) (setq delivered t))))
			 (with-timeout (5 (error "Search helper did not start"))
			   (while (not (file-exists-p marker)) (accept-process-output nil 0.01)))
			 (let ((snapshot (with-temp-buffer (insert-file-contents marker) (buffer-string))))
			   (should (file-directory-p snapshot))
			   (mevedel-memory-investigation-stop state)
			   (should-not (file-exists-p snapshot)))
			 (with-timeout (5 (error "Search child did not stop"))
			   (while (cl-some (lambda (process) (and (string-prefix-p "mevedel-glob" (process-name process))
								  (process-live-p process)))
					   (process-list))
			     (accept-process-output nil 0.01)))
			 (should-not delivered)
			 (mevedel-memory-investigation-call state 'read '(:root "workspace" :path "started")
							    (lambda (_) (setq delivered t)))
			 (should-not delivered))
		     (when state (mevedel-memory-investigation-stop state))
		     (mevedel-execution-teardown-all)
		     (delete-directory directory t))))

(provide 'test-mevedel-memory-investigation)
;;; test-mevedel-memory-investigation.el ends here
