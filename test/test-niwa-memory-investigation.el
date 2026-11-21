;;; test-niwa-memory-investigation.el -- Read-only review tools -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise the real bounded investigation surface without a model request.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-memory-scope)
(require 'niwa-memory-investigation)
(require 'niwa-system)
(require 'niwa-sandbox)

(defun niwa-test-investigation--call (state operation args)
  "Run OPERATION with ARGS in STATE and wait for its bounded result."
  (let (result done)
    (niwa-memory-investigation-call
     state operation args (lambda (text) (setq result text done t)))
    (with-timeout (5 (error "Investigation did not settle"))
      (while (not done) (accept-process-output nil 0.01)))
    result))

(niwa-deftest niwa-memory-investigation-call ()
		 ,test
		 (test)
		 :doc "Read uses frozen memory and admitted journal evidence while source stays read-only"
		 (let* ((directory (make-temp-file "niwa-investigation-" t))
			(workspace (niwa-workspace--create :root directory))
			(niwa-memory-dirs '("memory"))
			(topic (file-name-concat directory "memory" "topic.md"))
			state)
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory topic))
			 (with-temp-file topic (insert "Captured lesson.\n"))
			 (with-temp-file (file-name-concat directory "source.el") (insert ";; Current source.\n"))
			 (let* ((scope (niwa-memory-scope-capture workspace))
				(id (caar (plist-get scope :roots))))
			   (setq state (niwa-memory-investigation-create
					scope '((:file "digest.md" :id "digest-id" :session "session-id"
						       :created "2026-09-07T12:00:00Z" :body "Admitted evidence."))
					(lambda () t) (lambda (_reason) (ert-fail "Unexpected exhaustion"))))
			   (with-temp-file topic (insert "Changed after capture.\n"))
			   (should (string-match-p "Captured lesson" (niwa-test-investigation--call
								      state 'read (list :root id :path "topic.md"))))
			   (should (string-match-p "Current source" (niwa-test-investigation--call
								     state 'read '(:root "workspace" :path "source.el"))))
			   (should (string-match-p "Admitted evidence" (niwa-test-investigation--call
									state 'read '(:root "journal" :path "digest.md"))))
			   (dolist (args '((:root "workspace" :path "../escape")
					   (:root "workspace" :path ".niwa/state/journal/private.json")
					   (:root "journal" :path "unselected.md")
					   (:root "missing" :path "topic.md")))
			     (should (string-prefix-p "Error:" (niwa-test-investigation--call state 'read args))))
			   (should (string-match-p "Current source" (niwa-test-investigation--call
								     state 'read '(:root "workspace" :path "./source.el"))))
				   ;; Delivery remains observable without retaining source bodies.
				   (let ((summary (niwa-memory-investigation-summary state)))
				     (should (= 8 (length (plist-get summary :calls))))
				     (should-not (string-search "Current source" (prin1-to-string state))))))
		     (when state (niwa-memory-investigation-stop state))
		     (delete-directory directory t)))
		 :doc "Glob and Grep reuse ordinary search over admitted nested snapshots without leaking private paths"
		 (let* ((directory (make-temp-file "niwa-investigation-search-" t))
			(workspace (niwa-workspace--create :root directory))
			(niwa-memory-dirs '("memory"))
			(topic (file-name-concat directory "memory" "nested" "topic.md"))
			(niwa-sandbox-mode 'off)
			state)
		   (unwind-protect
		       (progn
			 (make-directory (file-name-directory topic) t)
			 (with-temp-file topic (insert "Captured unique lesson.\n"))
			 (make-directory (file-name-concat directory "src"))
			 (with-temp-file (file-name-concat directory "src" "code.el") (insert ";; Unique source.\n"))
			 (make-directory (file-name-concat directory ".niwa" "state" "journal") t)
			 (with-temp-file (file-name-concat directory ".niwa" "state" "journal" "secret.el")
			   (insert "Unique PRIVATE.\n"))
			 (let* ((scope (niwa-memory-scope-capture workspace))
				(id (caar (plist-get scope :roots))))
			   (setq state (niwa-memory-investigation-create scope nil (lambda () t)
									    (lambda (_) (ert-fail "Unexpected limit"))))
			   (should (string-match-p "nested/topic.md"
						   (niwa-test-investigation--call state 'glob
										     (list :root id :path "." :pattern "*.md"))))
			   (let ((result (niwa-test-investigation--call state 'grep
									   (list :root id :path "nested" :pattern "unique"))))
			     (should (string-match-p "Captured unique lesson" result))
			     (should-not (string-match-p "niwa-memory-search-\\|/tmp/" result)))
			   (let ((result (niwa-test-investigation--call state 'grep
									   '(:root "workspace" :path "." :pattern "Unique"))))
			     (should (string-match-p "src/code.el" result))
			     (should-not (string-match-p "PRIVATE\\|secret.el\\|Captured unique" result)))
			   (should (string-prefix-p "Error:" (niwa-test-investigation--call
							      state 'grep '(:root "workspace" :path "memory" :pattern "."))))))
		     (when state (niwa-memory-investigation-stop state))
		     (niwa-execution-teardown-all)
		     (delete-directory directory t)))
		 :doc "source search marks oversized evidence partial and remains usable on a remote workspace"
		 (let ((directory (make-temp-file "niwa-investigation-remote-" t))
		       (niwa-memory-dirs nil)
		       (niwa-sandbox-mode 'off)
		       state)
		   (unwind-protect
		       (niwa-test--with-local-shell-tramp '("memory-investigation")
							     (let* ((remote (format "/niwamock:memory-investigation:%s/" directory))
								    (workspace (niwa-workspace--create :root remote)))
							       (with-temp-file (file-name-concat directory "small.el") (insert ";; Source needle.\n"))
							       (with-temp-file (file-name-concat directory "huge.el") (insert (make-string 600000 ?x)))
							       (setq state (niwa-memory-investigation-create
									    (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
							       (should (string-match-p "Source needle" (niwa-test-investigation--call
													state 'read '(:root "workspace" :path "small.el"))))
							       (should (string-prefix-p
                                                        "Error:"
                                                        (niwa-test-investigation--call
                                                         state 'read '(:root "workspace" :path "huge.el" :offset 1 :limit 1))))
                                                       (let ((text (niwa-test-investigation--call state 'grep
													     '(:root "workspace" :path "." :pattern "needle"))))
								 (should (string-prefix-p "[Partial source snapshot" text))
								 (should (string-match-p "small.el" text))
								 (should-not (string-match-p "niwamock:\\|niwa-memory-search-" text)))))
		     (when state (niwa-memory-investigation-stop state))
		     (niwa-execution-teardown-all)
		     (delete-directory directory t)))

  :doc "search callbacks can launch follow-ups after empty or asynchronous results"
  (let* ((directory (make-temp-file "niwa-investigation-followup-" t))
         (default-directory (file-name-as-directory directory))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs nil)
         (niwa-sandbox-mode 'off)
         (state (niwa-memory-investigation-create
                 (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore)))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat directory "source.el")
            (insert ";; Source evidence.\n"))
          (dolist (case '((glob :root "workspace" :path "." :pattern ".niwa/**")
                         (glob :root "workspace" :path "." :pattern "*.el")
                         (grep :root "workspace" :path "." :pattern "Absent")
                         (grep :root "workspace" :path "." :pattern "Source"
                               :glob "missing/*.el")))
            (let (done failure result callback-directory)
              (niwa-memory-investigation-call
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
              (should-not (niwa-memory-investigation-active state)))))
      (niwa-memory-investigation-stop state)
      (niwa-execution-teardown-all)
      (delete-directory directory t)))

  :doc "bounds UTF-8 results, total returned text, and calls; exhaustion retires the owner once"
		 (let* ((directory (make-temp-file "niwa-investigation-limits-" t))
			(workspace (niwa-workspace--create :root directory))
			(niwa-memory-dirs nil)
			state reason (failures 0))
		   (unwind-protect
		       (progn
			 (with-temp-file (file-name-concat directory "large.txt")
			   (dotimes (_ 100) (insert (make-string 100 ?\u754c) "\n")))
			 (let ((scope (niwa-memory-scope-capture workspace)))
			   (setq state (niwa-memory-investigation-create
					scope nil (lambda () t) (lambda (value) (setq reason value) (cl-incf failures))))
			   (dotimes (_ 8)
			     (let ((text (niwa-test-investigation--call state 'read
									   '(:root "workspace" :path "large.txt"))))
			       (should (<= (string-bytes text) 8192))
			       (should (string-match-p "truncated" text))
			       (should-not (cl-some #'niwa--invalid-message-char-p text))))
			   (niwa-memory-investigation-call state 'read '(:root "workspace" :path "large.txt")
							      (lambda (_) (ert-fail "Output beyond budget")))
			   (should (equal reason "Tool output budget exhausted"))
			   (should (= failures 1))
			   (niwa-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Retired callback")))
			   (should (= failures 1))
			   ;; A truncated Read names a delivered line to continue from,
			   ;; so following it skips nothing.
			   (let* ((state (niwa-memory-investigation-create scope nil (lambda () t) #'ignore))
				  (first (niwa-test-investigation--call
					  state 'read '(:root "workspace" :path "large.txt")))
				  (line (and (string-match "Read again with offset \\([0-9]+\\) to continue" first)
					     (string-to-number (match-string 1 first))))
				  (next (niwa-test-investigation--call
					 state 'read (list :root "workspace" :path "large.txt" :offset line))))
			     (should (<= (string-bytes first) 8192))
			     (should (> line 1))
			     (should (string-match-p (format "\n *%d\t" line) first))
			     (should (string-match-p (format "\\` *%d\t" line) next)))
			   (setq state (niwa-memory-investigation-create scope nil (lambda () t)
									    (lambda (value) (setq reason value))))
			   (should (= 64 niwa-memory-investigation--max-calls))
			   (should (= 65536 niwa-memory-investigation--max-bytes))
			   (dotimes (_ 64)
			     (niwa-test-investigation--call state 'read '(:root "unknown" :path "a.md")))
			   (should (= 64 (niwa-memory-investigation-calls state)))
			   (should-not (niwa-memory-investigation-stopped state))
			   (niwa-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Call beyond budget")))
			   (should (equal reason "Tool call budget exhausted"))
			   (should (= 64 (niwa-memory-investigation-calls state)))
			   (should (niwa-memory-investigation-stopped state))))
		     (when state (niwa-memory-investigation-stop state))
		     (delete-directory directory t))))

(niwa-deftest niwa-memory-investigation--git ()
  ,test
  (test)
  :doc "Git history stays read-only and bounded without a retained body accumulator"
  (let* ((directory (file-name-as-directory (file-truename (make-temp-file "niwa-investigation-git-" t))))
         (workspace (niwa-workspace--create :root directory))
         (niwa-memory-dirs nil)
         (git (lambda (&rest args)
                (with-temp-buffer
                  (unless (zerop (apply #'call-process "git" nil t nil "-C" directory args))
                    (error "'git' %S failed: %s" args (buffer-string))))))
         state)
    (unwind-protect
        (progn
          (funcall git "init" "-q")
          (with-temp-file (file-name-concat directory "a.el") (insert ";; a\n"))
          (funcall git "add" "a.el")
          (funcall git "-c" "user.name=T" "-c" "user.email=t@example.invalid"
                   "commit" "-q" "-m" "fix(view): Integrate the visited-row anchor" "-m" "Resolves findings 1B and 2.")
          (let ((head (with-temp-buffer
                        (call-process "git" nil t nil "-C" directory "rev-parse" "--short" "HEAD")
                        (string-trim (buffer-string))))
                (before (with-temp-buffer
                          (call-process "git" nil t nil "-C" directory "status" "--porcelain")
                          (buffer-string))))
            (setq state (niwa-memory-investigation-create
                         (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
            (should (string-match-p "visited-row anchor"
                                    (niwa-test-investigation--call
                                     state 'git '(:command "log" :pattern "visited.row"))))
            (let ((shown (niwa-test-investigation--call state 'git (list :command "show" :pattern head))))
              (should (string-match-p "Resolves findings 1B and 2" shown))
              (should (string-match-p (format "Commit %s contained in current HEAD: yes" head) shown)))
            ;; A hash given to log is shown; a commit off HEAD says so.
            (funcall git "checkout" "-q" "-b" "side")
            (funcall git "-c" "user.name=T" "-c" "user.email=t@example.invalid"
                     "commit" "-q" "--allow-empty" "-m" "Side experiment")
            (let ((side (with-temp-buffer
                          (call-process "git" nil t nil "-C" directory "rev-parse" "--short" "HEAD")
                          (string-trim (buffer-string)))))
              (funcall git "checkout" "-q" "-")
              (let ((shown (niwa-test-investigation--call state 'git (list :command "log" :pattern side :path " "))))
                (should (string-match-p "Side experiment" shown))
                (should (string-match-p (format "Commit %s contained in current HEAD: no" side) shown))))
            (setq before (with-temp-buffer
                           (call-process "git" nil t nil "-C" directory "status" "--porcelain")
                           (buffer-string)))
            (should (equal "No matching commits."
                           (niwa-test-investigation--call
                            state 'git '(:command "log" :pattern "never-mentioned"))))
            (dolist (args '((:command "show" :pattern "HEAD; rm -rf .")
                            (:command "show" :pattern "--output=x")
                            (:command "diff" :pattern "x")
                            (:command "log" :pattern "x" :path "../outside")))
              (should (string-prefix-p "Error:" (niwa-test-investigation--call state 'git args))))
            ;; Git results reach the caller without a retained full-text copy.
            (should-not (string-search "Resolves findings 1B and 2" (prin1-to-string state)))
            (should (equal before (with-temp-buffer
                                    (call-process "git" nil t nil "-C" directory "status" "--porcelain")
                                    (buffer-string))))))
      (when state (niwa-memory-investigation-stop state))
      (delete-directory directory t))))

(niwa-deftest niwa-memory-investigation--deliver ()
  ,test
  (test)
  :doc "accepts exactly 64 KiB of bounded results and rejects one more byte without charging it"
  (let* ((delivered 0) (failures 0)
         (state (niwa-memory-investigation-create
                 nil nil (lambda () t)
                 (lambda (reason)
                   (should (equal "Tool output budget exhausted" reason))
                   (cl-incf failures)))))
    (unwind-protect
        (progn
          (dotimes (_ 8)
            (niwa-memory-investigation--deliver
             state (lambda (text) (should (= 8192 (string-bytes text))) (cl-incf delivered))
             (make-string 8192 ?x)))
          (should (= 8 delivered))
          (should (= 65536 (niwa-memory-investigation-bytes state)))
          (should-not (niwa-memory-investigation-stopped state))
          (dotimes (_ 2)
            (niwa-memory-investigation--deliver
             state (lambda (_) (ert-fail "Delivery beyond aggregate budget")) "x"))
          (should (= 1 failures))
          (should (= 65536 (niwa-memory-investigation-bytes state)))
          (should (niwa-memory-investigation-stopped state)))
      (niwa-memory-investigation-stop state))))

(niwa-deftest niwa-memory-investigation-tools ()
		 ,test
		 (test)
		 :doc "exposes only scoped asynchronous Read/Glob/Grep without changing the global tool registry"
		 (let* ((state (niwa-memory-investigation-create nil nil (lambda () t) #'ignore))
			(known (copy-tree gptel--known-tools)))
		   (unwind-protect
		       (let ((tools (niwa-memory-investigation-tools state)))
			 (should (equal '("Read" "Glob" "Grep" "Git") (mapcar #'gptel-tool-name tools)))
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
		     (niwa-memory-investigation-stop state))))

(niwa-deftest niwa-memory-investigation-stop ()
		 ,test
		 (test)
		 :doc "stops a live search child, removes its snapshot, and suppresses late delivery"
		 (let* ((directory (make-temp-file "niwa-investigation-cancel-" t))
			(workspace (niwa-workspace--create :root directory))
			(niwa-memory-dirs nil)
			(bin (file-name-concat directory "bin"))
			(marker (file-name-concat directory "started"))
			(niwa-sandbox-mode 'off)
			state delivered)
		   (unwind-protect
		       (progn
			 (make-directory bin)
			 (with-temp-file (file-name-concat bin "rg")
			   (insert "#!/bin/sh\nfor value do :; done\nprintf '%s' \"$value\" > "
				   (shell-quote-argument marker) "\nexec sleep 30\n"))
			 (set-file-modes (file-name-concat bin "rg") #o755)
			 (setq state (niwa-memory-investigation-create
				      (niwa-memory-scope-capture workspace) nil (lambda () t) #'ignore))
			 (let ((exec-path (cons bin exec-path)))
			   (niwa-memory-investigation-call state 'glob
							      '(:root "workspace" :path "." :pattern "*")
							      (lambda (_) (setq delivered t))))
			 (with-timeout (5 (error "Search helper did not start"))
			   (while (not (file-exists-p marker)) (accept-process-output nil 0.01)))
			 (let ((snapshot (with-temp-buffer (insert-file-contents marker) (buffer-string))))
			   (should (file-directory-p snapshot))
			   (niwa-memory-investigation-stop state)
			   (should-not (file-exists-p snapshot)))
			 (with-timeout (5 (error "Search child did not stop"))
			   (while (cl-some (lambda (process) (and (string-prefix-p "niwa-glob" (process-name process))
								  (process-live-p process)))
					   (process-list))
			     (accept-process-output nil 0.01)))
			 (should-not delivered)
			 (niwa-memory-investigation-call state 'read '(:root "workspace" :path "started")
							    (lambda (_) (setq delivered t)))
			 (should-not delivered))
		     (when state (niwa-memory-investigation-stop state))
		     (niwa-execution-teardown-all)
		     (delete-directory directory t))))

(provide 'test-niwa-memory-investigation)
;;; test-niwa-memory-investigation.el ends here
