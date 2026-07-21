;;; test-mevedel-telemetry.el --- Tests for session telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the durable, redacted session telemetry stream.

;;; Code:

(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'profiler)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-telemetry--session (root)
  "Return a test session rooted below ROOT."
  (let* ((workspace
          (mevedel-workspace--create
           :type 'test :id root :root root :name "telemetry"
           :file-cache (mevedel-file-cache--create
                        :table (make-hash-table :test #'equal)
                        :order nil :total-bytes 0)))
         (session (mevedel-session-create "main" workspace)))
    (setf (mevedel-session-session-id session) "telemetry-test"
          (mevedel-session-turn-count session) 7)
    session))

(defun test-mevedel-telemetry--read (file)
  "Return all Lisp values stored one per line in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (entries)
      (condition-case nil
          (while t
            (push (read (current-buffer)) entries))
        (end-of-file))
      (nreverse entries))))

(mevedel-deftest mevedel-telemetry-record
  (:doc "buffers safe enriched events until session materialization")
  (let* ((root (make-temp-file "mevedel-telemetry-" t))
         (session (test-mevedel-telemetry--session root))
         (goal (mevedel-goal--create
                :id "goal-1" :status 'active :phase 'implementing :cycle 2)))
    (unwind-protect
        (progn
          (setf (mevedel-session-goal session) goal)
          (mevedel-telemetry-record
           session 'permission-decision
           :request-id "request-1"
           :outcome 'allow
           :command "SECRET raw command"
           :safe-note "bounded\nvalue")
          (should (= 1 (length (mevedel-session-telemetry-pending session))))
          (setf (mevedel-session-save-path session) root)
          (mevedel-telemetry-flush session)
          (let* ((entries (test-mevedel-telemetry--read
                           (file-name-concat root "telemetry-log.el")))
                 (entry (car entries)))
            (should (= 1 (length entries)))
            (should (= 1 (plist-get entry :schema-version)))
            (should (eq 'permission-decision (plist-get entry :event)))
            (should (equal "telemetry-test" (plist-get entry :session-id)))
            (should (= 7 (plist-get entry :turn)))
            (should (equal "goal-1" (plist-get entry :goal-id)))
            (should (eq 'implementing (plist-get entry :goal-phase)))
            (should (= 2 (plist-get entry :goal-cycle)))
            (should (numberp (plist-get entry :elapsed-ms)))
            (should (stringp (plist-get entry :time)))
            (should (equal "bounded\nvalue" (plist-get entry :safe-note)))
            (should-not (plist-member entry :command))
            (should-not (string-match-p
                         "SECRET"
                         (with-temp-buffer
                           (prin1 entry (current-buffer))
                           (buffer-string))))
            (with-temp-buffer
              (insert-file-contents
               (file-name-concat root "telemetry-log.el"))
              (should (= 1 (count-lines (point-min) (point-max))))))
          (should-not (mevedel-session-telemetry-pending session)))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-finish
  (:doc "pairs asynchronous span events and records elapsed duration")
  (let* ((root (make-temp-file "mevedel-telemetry-span-" t))
         (session (test-mevedel-telemetry--session root)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) root)
          (let ((span (mevedel-telemetry-start
                       session 'provider-request :request-id "request-1")))
            (mevedel-telemetry-finish span :outcome 'success))
          (pcase-let ((`(,start ,finish)
                       (test-mevedel-telemetry--read
                        (file-name-concat root "telemetry-log.el"))))
            (should (eq 'start (plist-get start :stage)))
            (should (eq 'finish (plist-get finish :stage)))
            (should (equal (plist-get start :span-id)
                           (plist-get finish :span-id)))
            (should (equal "request-1" (plist-get finish :request-id)))
            (should (eq 'success (plist-get finish :outcome)))
            (should (>= (plist-get finish :duration-ms) 0))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--guard-prompt
  (:doc "blocks and records hidden prompts without retaining prompt text")
  (let* ((root (make-temp-file "mevedel-telemetry-prompt-" t))
         (session (test-mevedel-telemetry--session root))
         (secret "SECRET prompt body must not be retained")
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry-profiler-fail-on-prompt t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) root)
          (should-error
           (mevedel-telemetry--guard-prompt
            'y-or-n-p (lambda (&rest _) t) secret)
           :type 'user-error)
          (let* ((entries (test-mevedel-telemetry--read
                           (file-name-concat root "telemetry-log.el")))
                 (entry (car entries))
                 (printed (prin1-to-string entry)))
            (should (= 1 (length entries)))
            (should (eq 'interactive-prompt-opened
                        (plist-get entry :event)))
            (should (eq 'y-or-n-p (plist-get entry :prompt-function)))
            (should (= (length secret) (plist-get entry :prompt-chars)))
            (should (stringp (plist-get entry :prompt-hash)))
            (should (plist-get entry :blocked))
            (should-not (string-match-p "SECRET" printed))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-profiler-directory
  (:doc "isolates artifacts below the active profiler run directory")
  (let* ((root (make-temp-file "mevedel-telemetry-directory-" t))
         (session (test-mevedel-telemetry--session root))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry--profiler-run-id "run-test"))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) root)
          (should
           (equal (file-name-concat root "diagnostics" "run-test")
                  (mevedel-telemetry-profiler-directory session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--install-prompt-guard
  (:doc "advises each synchronous prompt with its own function identity")
  (let* ((root (make-temp-file "mevedel-telemetry-prompt-advice-" t))
         (session (test-mevedel-telemetry--session root))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry-profiler-fail-on-prompt t))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) root)
          (mevedel-telemetry--install-prompt-guard)
          (should-error (y-or-n-p "SECRET advised prompt") :type 'user-error)
          (let ((entry
                 (car (test-mevedel-telemetry--read
                       (file-name-concat root "telemetry-log.el")))))
            (should (eq 'y-or-n-p (plist-get entry :prompt-function)))
            (should-not (string-match-p "SECRET" (prin1-to-string entry)))))
      (mevedel-telemetry--remove-prompt-guard)
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-current-session
  (:doc "finds sessions directly, through data buffers, and through agents")
  (let* ((root (make-temp-file "mevedel-telemetry-current-" t))
         (session (test-mevedel-telemetry--session root))
         (data (generate-new-buffer " *telemetry-data*"))
         (view (generate-new-buffer " *telemetry-view*")))
    (unwind-protect
        (progn
          (with-current-buffer data (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--data-buffer data)
            (should (eq session (mevedel-telemetry-current-session)))))
      (kill-buffer data)
      (kill-buffer view)
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-path
  (:doc "returns nil before materialization and the configured path after it")
  (let* ((root (make-temp-file "mevedel-telemetry-path-" t))
         (session (test-mevedel-telemetry--session root)))
    (unwind-protect
        (progn
          (should-not (mevedel-telemetry-path session))
          (setf (mevedel-session-save-path session) root)
          (should (equal (file-name-concat root mevedel-telemetry-file-name)
                         (mevedel-telemetry-path session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--monotonic-now
  (:doc "returns a numeric monotonic-clock reading")
  (let ((first (mevedel-telemetry--monotonic-now))
        (second (mevedel-telemetry--monotonic-now)))
    (should (numberp first))
    (should (>= second first))))

(mevedel-deftest mevedel-telemetry--truncate-string
  (:doc "bounds strings without altering short values")
  (let ((mevedel-telemetry-max-string-length 3))
    (should (equal "abc" (mevedel-telemetry--truncate-string "abcdef")))
    (should (equal "ab" (mevedel-telemetry--truncate-string "ab")))))

(mevedel-deftest mevedel-telemetry--take-bounded
  (:doc "takes at most the requested number of list elements")
  (should (equal '(a b :truncated)
                 (mevedel-telemetry--take-bounded '(a b c) 2)))
  (should (equal '(a b) (mevedel-telemetry--take-bounded '(a b) 2))))

(mevedel-deftest mevedel-telemetry--safe-props
  (:doc "rejects payload and envelope keys while bounding nested values")
  (let ((safe (mevedel-telemetry--safe-props
               '(:command "secret" :event forged :safe ("ok" (1 2))))))
    (should-not (plist-member safe :command))
    (should-not (plist-member safe :event))
    (should (equal '("ok" (1 2)) (plist-get safe :safe)))))

(mevedel-deftest mevedel-telemetry--envelope
  (:doc "clamps process elapsed time when its fallback clock moves backwards")
  (let* ((root (make-temp-file "mevedel-telemetry-envelope-" t))
         (session (test-mevedel-telemetry--session root))
         (mevedel-telemetry--origin 10.0)
         (mevedel-telemetry--last-elapsed-ms 500)
         (times '(11.0 9.0)))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-telemetry--monotonic-now)
                   (lambda () (pop times))))
          (should (= 1000 (plist-get
                           (mevedel-telemetry--envelope session 'one nil)
                           :elapsed-ms)))
          (should (= 1000 (plist-get
                           (mevedel-telemetry--envelope session 'two nil)
                           :elapsed-ms))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--span-id
  (:doc "creates distinct event-prefixed span identifiers")
  (let ((one (mevedel-telemetry--span-id 'hook))
        (two (mevedel-telemetry--span-id 'hook)))
    (should (string-prefix-p "hook-" one))
    (should-not (equal one two))))

(mevedel-deftest mevedel-telemetry--process-output
  (:doc "captures successful process output and returns nil on failure")
  (should (string-match-p "git version"
                          (mevedel-telemetry--process-output
                           "git" "--version")))
  (should-not (mevedel-telemetry--process-output "git" "not-a-command")))

(mevedel-deftest mevedel-telemetry--git-snapshot
  (:doc "captures commit, dirty counts, and exact dirty-content identity")
  (let ((snapshot (mevedel-telemetry--git-snapshot default-directory)))
    (should (stringp (plist-get snapshot :git-head)))
    (should (numberp (plist-get snapshot :dirty-file-count)))
    (should (= 64 (length (plist-get snapshot :dirty-content-hash))))))

(mevedel-deftest mevedel-telemetry--library-snapshot
  (:doc "identifies a loaded library by content and repository commit")
  (let ((snapshot (mevedel-telemetry--library-snapshot 'mevedel-telemetry)))
    (should (= 64 (length (plist-get snapshot :file-hash))))
    (should (> (plist-get snapshot :file-bytes) 0))
    (should (stringp (plist-get snapshot :git-head)))))

(mevedel-deftest mevedel-telemetry--record-environment
  (:doc "records safe repository, dependency, and sandbox identities")
  (let* ((root (make-temp-file "mevedel-telemetry-environment-" t))
         (session (test-mevedel-telemetry--session root)))
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) root)
          (cl-letf (((symbol-function 'mevedel-telemetry--git-snapshot)
                     (lambda (_) '(:git-head "repo")))
                    ((symbol-function 'mevedel-telemetry--library-snapshot)
                     (lambda (feature)
                       (list :file-hash (symbol-name feature)
                             :git-head (format "%s-head" feature))))
                    ((symbol-function 'mevedel-sandbox-probe)
                     (lambda () '(:available t))))
            (mevedel-telemetry--record-environment session 'start))
          (let ((entry (car (test-mevedel-telemetry--read
                             (mevedel-telemetry-path session)))))
            (should (eq 'reproduction-environment (plist-get entry :event)))
            (should (equal "gptel-agent-head"
                           (plist-get entry :gptel-agent-commit)))
            (should (equal "repo" (plist-get entry :git-head)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-profiler-start
		 (:doc "starts one session-owned profiler run and installs its guard")
		 (let* ((root (make-temp-file "mevedel-telemetry-profiler-start-" t))
			(session (test-mevedel-telemetry--session root))
			(mevedel-telemetry--profiler-session nil)
			(mevedel-telemetry--profiler-run-id nil)
			started guarded recorded)
		   (unwind-protect
		       (with-temp-buffer
			 (setq-local mevedel--session session)
			 (setf (mevedel-session-save-path session) root)
			 (cl-letf
			     (((symbol-function 'profiler-start)
			       (lambda (mode) (setq started mode)))
			      ((symbol-function 'mevedel-telemetry--install-prompt-guard)
			       (lambda () (setq guarded t)))
			      ((symbol-function 'mevedel-telemetry--record-environment)
			       (lambda (_session boundary) (setq recorded boundary))))
			   (mevedel-telemetry-profiler-start 'mem))
			 (should (eq 'mem started))
			 (should guarded)
			 (should (eq 'start recorded))
			 (should (eq session mevedel-telemetry--profiler-session))
			 (should (string-prefix-p
				  "run-" mevedel-telemetry--profiler-run-id)))
		     (setq mevedel-telemetry--profiler-session nil
			   mevedel-telemetry--profiler-run-id nil)
		     (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--write-profiler-artifacts
		 (:doc "writes compact native profiles and reports for each active mode")
		 (let* ((root (make-temp-file "mevedel-telemetry-profiler-data-" t))
			(dead-buffer (generate-new-buffer " *telemetry-dead*"))
			(captured dead-buffer)
			(closure (lambda () captured))
			(profiler-cpu-log (make-hash-table :test #'equal))
			(profiler-memory-log (make-hash-table :test #'equal)))
		   (kill-buffer dead-buffer)
		   (puthash (vector closure 'Automatic\ GC) 1000 profiler-cpu-log)
		   (puthash (vector closure 'Automatic\ GC) 2000 profiler-memory-log)
		   (unwind-protect
		       (let ((artifacts
			      (mevedel-telemetry--write-profiler-artifacts root)))
			 (should (equal '(cpu memory)
					(mapcar (lambda (artifact)
						  (plist-get artifact :mode))
						artifacts)))
			 (dolist (artifact artifacts)
			   (let* ((profile-file (plist-get artifact :profile-file))
				  (report-file (plist-get artifact :report-file))
				  (profile (profiler-read-profile profile-file)))
			     (should (eq 'profiler-profile (aref profile 0)))
			     (should
			      (> (file-attribute-size (file-attributes report-file)) 0))
			     (should (< (file-attribute-size
					 (file-attributes profile-file))
					10000))
			     (should-not
			      (string-match-p
			       "#<killed buffer>"
			       (with-temp-buffer
				 (insert-file-contents profile-file)
				 (buffer-string)))))))
		     (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-profiler-stop
		 (:doc "records complete or failed artifact saves and clears run ownership")
		 (let* ((root (make-temp-file "mevedel-telemetry-profiler-stop-" t))
			(session (test-mevedel-telemetry--session root))
			(mevedel-telemetry--profiler-session session)
			(mevedel-telemetry--profiler-run-id "run-test")
			stopped unguarded recorded)
		   (setf (mevedel-session-save-path session) root)
		   (unwind-protect
		       (progn
			 (cl-letf
			     (((symbol-function 'profiler-stop)
			       (lambda () (setq stopped t)))
			      ((symbol-function 'mevedel-telemetry--remove-prompt-guard)
			       (lambda () (setq unguarded t)))
			      ((symbol-function 'mevedel-telemetry--record-environment)
			       (lambda (_session boundary) (setq recorded boundary)))
			      ((symbol-function
				'mevedel-telemetry--write-profiler-artifacts)
			       (lambda (directory)
				 (let ((report (file-name-concat directory "report"))
				       (profile (file-name-concat directory "profile")))
				   (with-temp-file report (insert "report"))
				   (with-temp-file profile (insert "profile"))
				   (list (list :mode 'cpu
					       :report-file report
					       :profile-file profile
					       :report-bytes 6
					       :profile-bytes 7))))))
			   (mevedel-telemetry-profiler-stop))
			 (should stopped)
			 (should unguarded)
			 (should (eq 'stop recorded))
			 (should-not mevedel-telemetry--profiler-session)
			 (should-not mevedel-telemetry--profiler-run-id)
			 (let ((entry
				(car (test-mevedel-telemetry--read
				      (file-name-concat root "telemetry-log.el")))))
			   (should (eq 'profiler-stopped (plist-get entry :event)))
			   (should (equal '(cpu) (plist-get entry :modes)))
			   (should (= 7 (plist-get entry :profile-bytes-total)))
			   (should (= 6 (plist-get entry :report-bytes-total))))
			 (setq mevedel-telemetry--profiler-session session
			       mevedel-telemetry--profiler-run-id "run-failed"
			       unguarded nil)
			 (cl-letf
			     (((symbol-function 'profiler-stop) #'ignore)
			      ((symbol-function 'mevedel-telemetry--remove-prompt-guard)
			       (lambda () (setq unguarded t)))
			      ((symbol-function 'mevedel-telemetry--record-environment)
			       #'ignore)
			      ((symbol-function
				'mevedel-telemetry--write-profiler-artifacts)
			       (lambda (_) (error "Broken artifacts"))))
			   (should-error (mevedel-telemetry-profiler-stop)))
			 (should unguarded)
			 (should-not mevedel-telemetry--profiler-session)
			 (should-not mevedel-telemetry--profiler-run-id)
			 (let ((entry
				(car (last (test-mevedel-telemetry--read
					    (file-name-concat root "telemetry-log.el"))))))
			   (should (eq 'profiler-stop-failed (plist-get entry :event)))
			   (should (eq 'save-artifacts
				       (plist-get entry :failure-stage)))))
		     (setq mevedel-telemetry--profiler-session nil
			   mevedel-telemetry--profiler-run-id nil)
		     (delete-directory root t))))

(provide 'test-mevedel-telemetry)

;;; test-mevedel-telemetry.el ends here
