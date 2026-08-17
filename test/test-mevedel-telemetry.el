;;; test-mevedel-telemetry.el --- Tests for session telemetry -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the durable, redacted session telemetry stream.

;;; Code:

(require 'gptel)
(require 'mevedel-execution-target)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-structs)
(require 'mevedel-telemetry)
(require 'mevedel-view-render)
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
           :type 'file :id root :root root :name "telemetry"
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
  (:quiet t)
  ,test
  (test)
  :doc "buffers safe enriched events until session materialization"
  (let* ((root (make-temp-file "mevedel-telemetry-" t))
         (session (test-mevedel-telemetry--session root))
         (goal (mevedel-goal--create
                :id "goal-1" :status 'active
                :tokens-used 21 :turns-run 2)))
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
            (should (eq 'active (plist-get entry :goal-status)))
            (should (= 21 (plist-get entry :goal-tokens-used)))
            (should (= 2 (plist-get entry :goal-turns-run)))
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
          (should-not (mevedel-session-telemetry-pending session))
          (should-not (string-match-p "Added to"
                                      (or (current-message) ""))))
      (delete-directory root t)))

  :doc "retains a materialized event when persistence fails and retries it"
  (let* ((root (make-temp-file "mevedel-telemetry-retry-" t))
         (blocked (file-name-concat root "blocked"))
         (restored (file-name-concat root "restored"))
         (session (test-mevedel-telemetry--session root)))
    (unwind-protect
        (progn
          (write-region "not a directory" nil blocked nil 'silent)
          (setf (mevedel-session-save-path session) blocked)
          (mevedel-telemetry-record session 'provider-request
                                    :stage 'finish)
          (should (= 1 (length (mevedel-session-telemetry-pending session))))
          (setf (mevedel-session-save-path session) restored)
          (mevedel-telemetry-record session 'provider-response
                                    :stage 'finish)
          (should (= 2 (length (mevedel-session-telemetry-pending session))))
          (mevedel-telemetry-flush session)
          (should-not (mevedel-session-telemetry-pending session))
          (let ((entries
                 (test-mevedel-telemetry--read
                  (file-name-concat restored "telemetry-log.el"))))
            (should (= 2 (length entries)))
            (should (eq 'provider-request
                        (plist-get (car entries) :event)))
            (should (eq 'provider-response
                        (plist-get (cadr entries) :event)))))
      (delete-directory root t)))

  :doc "defers remote events and combines them into one serialized append"
  (let* ((root (make-temp-file "mevedel-telemetry-remote-" t))
         (target
          (mevedel-execution-target-create
           "/ssh:telemetry-host:/workspace/"))
         (session
          (mevedel-session--create
           :authority-mode 'pid-lock
           :name "main" :execution-target target :save-path root))
         calls)
    (unwind-protect
        (cl-letf
            (((symbol-function
               'mevedel-session-publication-append-diagnostic)
              (lambda (_session path content)
                (push (list path content) calls)
                t)))
          (mevedel-telemetry-record session 'provider-request :stage 'start)
          (mevedel-telemetry-record session 'provider-request :stage 'finish)
          (should (= 2 (length
                        (mevedel-session-telemetry-pending session))))
          (should-not calls)
          (mevedel-telemetry-flush session)
          (should-not (mevedel-session-telemetry-pending session))
          (pcase-let ((`((,path ,content)) calls))
            (should
             (equal (file-name-concat root "telemetry-log.el") path))
            (with-temp-buffer
              (insert content)
              (goto-char (point-min))
              (should (eq 'start
                          (plist-get (read (current-buffer)) :stage)))
              (should (eq 'finish
                          (plist-get (read (current-buffer)) :stage))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry--queue-order
  ()
  ,test
  (test)
  :doc "queues newest first and flushes in recording order"
  (let* ((root (make-temp-file "mevedel-telemetry-order-" t))
         (session (test-mevedel-telemetry--session root)))
    (unwind-protect
        (progn
          (dolist (event '(request-queued request-start request-end))
            (mevedel-telemetry-record session event))
          (should (equal '(request-end request-start request-queued)
                         (mapcar (lambda (entry) (plist-get entry :event))
                                 (mevedel-session-telemetry-pending
                                  session))))
          (setf (mevedel-session-save-path session) root)
          (mevedel-telemetry-flush session)
          (should-not (mevedel-session-telemetry-pending session))
          (should (equal '(request-queued request-start request-end)
                         (mapcar (lambda (entry) (plist-get entry :event))
                                 (test-mevedel-telemetry--read
                                  (file-name-concat root
                                                    "telemetry-log.el"))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-session-persistence--flush-diagnostic-logs
  ()
  ,test
  (test)
  :doc "defers a remote flush off the caller's path and keeps local inline"
  (let* ((root (make-temp-file "mevedel-flush-defer-" t))
         (session (test-mevedel-telemetry--session root))
         (flushes 0))
    (require 'mevedel-session-persistence)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--flush-diagnostic-logs-now)
                   (lambda (_session) (cl-incf flushes))))
          ;; Local: inline.
          (setf (mevedel-session-save-path session) root)
          (mevedel-session-persistence--flush-diagnostic-logs session)
          (should (= 1 flushes))
          ;; Remote: nothing runs inside the caller's extent; the zero
          ;; timer hands the flush to the idle transport afterwards.
          (setf (mevedel-session-save-path session)
                "/mevedelmock:flush-host:/tmp/flush/")
          (mevedel-session-persistence--flush-diagnostic-logs session)
          (should (= 1 flushes))
          (let ((deadline (+ (float-time) 2)))
            (while (and (= 1 flushes) (< (float-time) deadline))
              (accept-process-output nil 0.02)))
          (should (= 2 flushes)))
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

(mevedel-deftest mevedel-telemetry-profiler-directory ()
  ,test
  (test)

  :doc "isolates artifacts below the active profiler run directory"
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
      (delete-directory root t)))

  :doc "keeps a target session's artifacts on this client"
  ;; A profile measures this Emacs and no resume consults it, so it never
  ;; crosses the connection.
  (let* ((root (make-temp-file "mevedel-telemetry-directory-" t))
         (session (test-mevedel-telemetry--session root))
         (mevedel-telemetry--profiler-session session)
         (mevedel-telemetry--profiler-run-id "run-test")
         first second)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session)
                "/ssh:user@host:/srv/project/.mevedel/session")
          (setq first (mevedel-telemetry-profiler-directory session))
          (should-not (file-remote-p first))
          (should (file-in-directory-p first temporary-file-directory))
          ;; Every caller derives the same answer without state to leak.
          (should (equal first (mevedel-telemetry-profiler-directory session)))
          (setq mevedel-telemetry--profiler-run-id "run-other")
          (setq second (mevedel-telemetry-profiler-directory session))
          (should-not (equal first second)))
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

(mevedel-deftest mevedel-telemetry-detailed-p
  (:doc "is session-scoped and active only while a profiler run exists")
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "main"))
        (other (mevedel-session--create :authority-mode 'pid-lock :name "other")))
    (let ((mevedel-telemetry--profiler-session session)
          (mevedel-telemetry--profiler-run-id "run-1"))
      (should (mevedel-telemetry-detailed-p session))
      (should-not (mevedel-telemetry-detailed-p other)))
    (let ((mevedel-telemetry--profiler-session session)
          (mevedel-telemetry--profiler-run-id nil))
      (should-not (mevedel-telemetry-detailed-p session)))))

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
  (:doc "captures local and remote commit and dirty-content identity")
  (let ((snapshot (mevedel-telemetry--git-snapshot default-directory)))
    (should (stringp (plist-get snapshot :git-head)))
    (should (numberp (plist-get snapshot :dirty-file-count)))
    (should (= 64 (length (plist-get snapshot :dirty-content-hash)))))
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-telemetry-remote-" t)))
         (remote-root (format "/mevedelmock:telemetry:%s" root)))
    (unwind-protect
        (progn
          (let ((default-directory root))
            (should (zerop (process-file "git" nil nil nil
                                         "init" "--quiet")))
            (should (zerop (process-file "git" nil nil nil
                                         "config" "user.name" "Test")))
            (should (zerop (process-file "git" nil nil nil
                                         "config" "user.email"
                                         "test@example.invalid")))
            (with-temp-file (file-name-concat root "tracked.txt")
              (insert "tracked\n"))
            (should (zerop (process-file "git" nil nil nil
                                         "add" "tracked.txt")))
            (should (zerop (process-file "git" nil nil nil
                                         "commit" "--quiet" "-m" "initial"))))
          (mevedel-test--with-local-shell-tramp '("telemetry")
            (let ((snapshot
                   (mevedel-telemetry--git-snapshot remote-root)))
              (should (stringp (plist-get snapshot :git-head)))
              (should (= 64
                         (length
                          (plist-get snapshot :dirty-content-hash)))))))
      (delete-directory root t))))

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
          (setf (mevedel-session-save-path session) root
                (mevedel-session-sandbox-mode session) 'required)
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
            (should (eq 'required (plist-get entry :sandbox-mode)))
            (should (equal "repo" (plist-get entry :git-head)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-profiler-start
		 (:quiet t :doc "starts one session-owned profiler run and installs its guard")
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
			   (mevedel-telemetry-profiler-start)
			 (should (eq 'cpu+mem started))
			 (setq mevedel-telemetry--profiler-session nil)
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
		 (:quiet t :doc "records complete or failed artifact saves and clears run ownership")
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

(mevedel-deftest mevedel-session-debug
  (:quiet t :doc "toggles profiling and persists captured gptel and view debug logs")
  (let* ((root (make-temp-file "mevedel-session-debug-" t))
         (session (test-mevedel-telemetry--session root))
         (log-buffer (get-buffer-create gptel--log-buffer-name))
         (view-buffer (get-buffer-create mevedel-view-render-debug-buffer-name))
         (gptel-log-level 'info)
         (mevedel-view-render-debug nil)
         (mevedel-telemetry--profiler-session nil)
         (mevedel-telemetry--profiler-run-id nil)
         (mevedel-telemetry--session-debug-marker nil)
         (mevedel-telemetry--session-debug-previous-log-level nil)
         (mevedel-telemetry--session-debug-view-marker nil)
         (mevedel-telemetry--session-debug-previous-view-debug nil)
         started stopped)
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (setf (mevedel-session-save-path session) root)
          (with-current-buffer log-buffer
            (erase-buffer))
          (with-current-buffer view-buffer
            (erase-buffer))
          (cl-letf
              (((symbol-function 'mevedel-telemetry-profiler-start)
                (lambda (&optional _mode)
                  (setq started t
                        mevedel-telemetry--profiler-session session
                        mevedel-telemetry--profiler-run-id "run-test")))
               ((symbol-function 'mevedel-telemetry-profiler-stop)
                (lambda ()
                  (setq stopped t
                        mevedel-telemetry--profiler-session nil
                        mevedel-telemetry--profiler-run-id nil))))
            (mevedel-session-debug)
            (should started)
            (should (eq 'debug gptel-log-level))
            (should mevedel-view-render-debug)
            (with-current-buffer log-buffer
              (insert
               "{\n"
               "  \"gptel\": \"request headers\",\n"
               "  \"timestamp\": \"test\"\n"
               "}\n"
               "{\n"
               "  \"Authorization\": \"Bearer auth-secret\",\n"
               "  \"ChatGPT-Account-Id\": \"account-secret\",\n"
               "  \"Session-Id\": \"session-secret\",\n"
               "  \"X-Unrelated\": \"preserved\"\n"
               "}\n"
               "{\n"
               "  \"gptel\": \"request config\",\n"
               "  \"timestamp\": \"test\"\n"
               "}\n"
               "header = \"Authorization: Bearer config-auth-secret\"\n"
               "header = \"ChatGPT-Account-Id: config-account-secret\"\n"
               "header = \"session-id: config-session-secret\"\n"
               "{\n"
               "  \"gptel\": \"request body\",\n"
               "  \"timestamp\": \"test\"\n"
               "}\n"
               "{\n"
               "  \"Authorization\": \"body-auth-preserved\",\n"
               "  \"Session-Id\": \"body-session-preserved\"\n"
               "}\n"
               "{\n"
               "  \"gptel\": \"response body\",\n"
               "  \"timestamp\": \"test\"\n"
               "}\n"
               "header = \"Authorization: response-auth-preserved\""))
            (with-current-buffer view-buffer
              (insert "captured view trace"))
            (mevedel-session-debug)
            (should stopped)
            (should (eq 'info gptel-log-level))
            (should-not mevedel-view-render-debug)
            (should-not mevedel-telemetry--session-debug-marker)
            (with-current-buffer log-buffer
              (should (string-match-p "auth-secret" (buffer-string))))
            (let ((gptel-file
                   (file-name-concat
                    root "diagnostics" "run-test" "gptel-debug.log"))
                  (view-file
                   (file-name-concat
                    root "diagnostics" "run-test" "view-render-debug.log")))
              (should (= #o600 (logand #o777 (file-modes gptel-file))))
              (should (= #o600 (logand #o777 (file-modes view-file))))
              (with-temp-buffer
                (insert-file-contents gptel-file)
                (let ((contents (buffer-string)))
                  (dolist (secret
                           '("auth-secret" "account-secret" "session-secret"
                             "config-auth-secret" "config-account-secret"
                             "config-session-secret"))
                    (should-not (string-match-p secret contents)))
                  (should (string-match-p
                           "\"Authorization\": \"<redacted>\"" contents))
                  (should (string-match-p
                           "\"ChatGPT-Account-Id\": \"<redacted>\""
                           contents))
                  (should (string-match-p
                           "\"Session-Id\": \"<redacted>\"" contents))
                  (should (string-match-p
                           "header = \"Authorization: <redacted>\""
                           contents))
                  (should (string-match-p
                           "header = \"ChatGPT-Account-Id: <redacted>\""
                           contents))
                  (should (string-match-p
                           "header = \"session-id: <redacted>\"" contents))
                  (should (string-match-p
                           "\"X-Unrelated\": \"preserved\"" contents))
                  (should (string-match-p
                           "\"Authorization\": \"body-auth-preserved\""
                           contents))
                  (should (string-match-p
                           "\"Session-Id\": \"body-session-preserved\""
                           contents))
                  (should (string-match-p
                           "header = \"Authorization: response-auth-preserved\""
                           contents))))
              (with-temp-buffer
                (insert-file-contents view-file)
                (should (equal "captured view trace" (buffer-string)))))
            (setq stopped nil)
            (mevedel-session-debug)
            (kill-buffer
             (marker-buffer mevedel-telemetry--session-debug-marker))
            (should-error (mevedel-session-debug))
            (should stopped)
            (should (eq 'info gptel-log-level))
            (should-not mevedel-view-render-debug)
            (should-not mevedel-telemetry--profiler-session)
            (should-not mevedel-telemetry--session-debug-marker)
            (should-not mevedel-telemetry--session-debug-view-marker)))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer))
      (when (buffer-live-p view-buffer)
        (kill-buffer view-buffer))
      (setq mevedel-telemetry--profiler-session nil
            mevedel-telemetry--profiler-run-id nil)
      (delete-directory root t))))

(provide 'test-mevedel-telemetry)

;;; test-mevedel-telemetry.el ends here
