;;; test-mevedel-execution-remote.el --- Remote managed execution tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests target-aware process dispatch and remote process-group authority.

;;; Code:

(require 'cl-lib)
(require 'mevedel-agents)
(require 'mevedel-diff-apply)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-file-state)
(require 'mevedel-overlays)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-telemetry)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'tramp)
(require 'tramp-container)
(require 'tramp-sh)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))


;;
;;; Test support

(defun test-mevedel-execution-remote--real-root (variable method)
  "Return the opt-in real TRAMP root from VARIABLE for METHOD.

The root must already exist, be writable, and be reachable through normal
  TRAMP authentication.  The tests never provision, start, or stop a target."
  (let ((value (getenv variable)))
    (unless value
      (ert-skip (format "%s is not set" variable)))
    (when (string-empty-p value)
      (ert-fail (format "%s is set but empty" variable)))
    (let ((root (file-name-as-directory value)))
      (unless (file-remote-p root)
        (ert-fail (format "%s must be a TRAMP directory" variable)))
      (unless (equal (symbol-name method)
                     (file-remote-p root 'method 'never))
        (ert-fail
         (format "%s must use the %s TRAMP method" variable method)))
      (when (file-remote-p root 'hop 'never)
        (ert-fail (format "%s must name one target, without hops" variable)))
      (unless (file-remote-p root 'host 'never)
        (ert-fail (format "%s must name a target host" variable)))
      (condition-case err
          (progn
            (unless (file-directory-p root)
              (ert-fail (format "%s is not a directory" variable)))
            (unless (file-writable-p root)
              (ert-fail (format "%s is not writable" variable))))
        (file-error
         (ert-fail
          (format "Could not authenticate or open %s: %s"
                  variable (error-message-string err)))))
      root)))

(defun test-mevedel-execution-remote--real-temp-directory (root stem)
  "Create and return a target-side temporary directory near ROOT named STEM."
  (let ((default-directory root))
    (file-name-as-directory (make-nearby-temp-file stem t))))

(defun test-mevedel-execution-remote--target-process-gone-p (root pid)
  "Return non-nil when target ROOT has no process named by PID."
  (let ((default-directory root)
        (process-environment nil))
    (not
     (zerop
      (process-file
       "bash" nil nil nil "-c" "kill -0 \"$1\" 2>/dev/null"
       "mevedel-process-check" (number-to-string pid))))))

(defun test-mevedel-execution-remote--wait
    (predicate description &optional timeout)
  "Wait for PREDICATE, failing after TIMEOUT seconds with DESCRIPTION."
  (with-timeout ((or timeout 30) (ert-fail description))
    (while (not (funcall predicate))
      (accept-process-output nil 0.05))))

(defun test-mevedel-execution-remote--accept-storage (session)
  "Accept SESSION's target-side durable storage for an opt-in test."
  (puthash
   (mevedel-execution-target-identity
    (mevedel-session-execution-target session))
   t mevedel-session-durability--disclosed-targets))

(defun test-mevedel-execution-remote--run-tool (session name args)
  "Run registered tool NAME with ARGS in SESSION and return its result."
  (let (done result)
    (with-temp-buffer
      (setq default-directory (mevedel-session-working-directory session))
      (setq-local mevedel--workspace (mevedel-session-workspace session)
                  mevedel--session session)
      (let ((mevedel-permission-rules nil)
            (mevedel-protected-paths nil))
        (mevedel-pipeline-run-tool
         (mevedel-tool-ensure name)
         (lambda (value)
           (setq result value
                 done t))
         args)
        (test-mevedel-execution-remote--wait
         (lambda () done) (format "%s did not settle" name) 60)))
    result))

(defun test-mevedel-execution-remote--start-reader
    (session root owner label &optional owner-context)
  "Start a yielded SESSION read-only command for OWNER at ROOT labelled LABEL.
Optional OWNER-CONTEXT identifies the retained agent invocation."
  (let (result)
    (mevedel-execution-start-bash
     (lambda (value) (setq result value))
     :session session :owner owner :owner-context owner-context
     :command
     (list "bash" "-c" "printf '%s-ready\\n' \"$1\"; sleep 30"
           "mevedel-acceptance-reader" label)
     :workdir root :writable-roots (list root)
     :read-only-p t :yield-time-ms 100
     :tool-args (list :command (format "acceptance reader %s" label)))
    (test-mevedel-execution-remote--wait
     (lambda () result) (format "%s reader did not yield" label))
    result))


;;
;;; Remote execution

(mevedel-deftest mevedel-execution-run-one-shot/remote ()
  ,test
  (test)
  :doc "dispatches in a TRAMP workspace and hides launcher control output"
  (let* ((root (make-temp-file "mevedel-remote-execution-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (let ((result
                 (mevedel-execution-run-one-shot
                  :name "mevedel-test-remote-dispatch"
                  :command '("sh" "-c" "printf '%s' \"$PWD\"")
                  :workdir remote-root
                  :writable-roots (list remote-root))))
            (should-not (plist-get result :error))
            (should (= 0 (plist-get result :exit-code)))
            (should (equal (directory-file-name root)
                           (plist-get result :output)))))
      (delete-directory root t)))
  :doc "preserves target environment without forwarding client variables"
  (let* ((root (make-temp-file "mevedel-remote-environment-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          ;; Establish the target shell before adding the client-only value.
          (should (file-directory-p remote-root))
          (let ((process-environment
                 (cons "MEVEDEL_CLIENT_SECRET=do-not-forward"
                       process-environment)))
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-environment"
                    :command
                    '("sh" "-c"
                      "test -n \"$PATH\" && test \"$MEVEDEL_EXECUTION\" = 1 && test -z \"${MEVEDEL_CLIENT_SECRET+x}\"")
                    :workdir remote-root
                    :writable-roots (list remote-root))))
              (should-not (plist-get result :error))
              (should (= 0 (plist-get result :exit-code))))))
      (delete-directory root t)))
  :doc "localizes qualified command arguments in the target path domain"
  (let* ((root (make-temp-file "mevedel-remote-argv-" t))
         (file (expand-file-name "input.txt" root))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (remote-file (concat remote-root "input.txt"))
         (mevedel-sandbox-mode 'off))
    (unwind-protect
        (progn
          (write-region "input" nil file nil 'silent)
          (mevedel-test--with-local-shell-tramp nil
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-argv"
                    :command
                    (list "sh" "-c"
                          "test -f \"$1\" && printf localized"
                          "sh" remote-file)
                    :workdir remote-root
                    :writable-roots (list remote-root))))
              (should-not (plist-get result :error))
              (should (= 0 (plist-get result :exit-code)))
              (should (equal "localized" (plist-get result :output))))))
      (delete-directory root t)))
  :doc "signals the captured target process group and removes descendants"
  (let* ((root (make-temp-file "mevedel-remote-group-" t))
         (pid-file (expand-file-name "child.pid" root))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution--child-kill-delay 0.05)
         (mevedel-sandbox-mode 'off)
         calls child-pid)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (&rest args)
                       (push args calls)
                       (apply original-signal-process args))))
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-group"
                    :command
                    '("sh" "-c"
                      "trap '' TERM; sleep 30 & child=$!; printf '%s' \"$child\" > child.pid; wait")
                    :workdir remote-root
                    :writable-roots (list remote-root)
                    :timeout 0.1)))
              (should (plist-get result :timed-out-p))
              (should (eq 'timed-out (plist-get result :termination)))
              (setq child-pid
                    (string-to-number
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents pid-file)
                        (buffer-string)))))
              (should (> child-pid 0))
              (should
               (cl-some
                (lambda (args)
                  (and (integerp (car args))
                       (< (car args) 0)
                       (memq (cadr args) '(TERM KILL))
                       (equal remote-root (nth 2 args))))
                calls))
              (should-not (zerop (funcall original-signal-process
                                           child-pid 0))))))
      (when (and child-pid
                 (eq 0 (ignore-errors
                         (funcall original-signal-process child-pid 0))))
        (ignore-errors (funcall original-signal-process child-pid 'KILL)))
      (delete-directory root t)))
  :doc "reports an unknown outcome when transport cannot signal the target group"
  (let* ((root (make-temp-file "mevedel-remote-unknown-" t))
         (pid-file (expand-file-name "group.pid" root))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution--child-kill-delay 0.02)
         (mevedel-sandbox-mode 'off)
         group-id)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (cl-letf (((symbol-function 'signal-process)
                     (lambda (&rest args)
                       (if (and (integerp (car args))
                                (< (car args) 0)
                                (memq (cadr args) '(TERM KILL))
                                (equal remote-root (nth 2 args)))
                           (error "Transport lost")
                         (apply original-signal-process args)))))
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-unknown"
                    :command
                    '("sh" "-c"
                      "ps -o pgid= -p $$ | tr -d ' ' > group.pid; trap '' TERM; sleep 30 & wait")
                    :workdir remote-root
                    :writable-roots (list remote-root)
                    :timeout 0.05)))
              (setq group-id
                    (string-to-number
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents pid-file)
                        (buffer-string)))))
              (should (eq 'unknown (plist-get result :termination))))))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t)))
  :doc "never signals a live group after its captured leader identity changes"
  (let* ((root (make-temp-file "mevedel-remote-reused-group-" t))
         (pid-file (expand-file-name "group.pid" root))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution--child-kill-delay 0.02)
         (mevedel-sandbox-mode 'off)
         calls group-id)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (cl-letf (((symbol-function
                      'mevedel-execution--remote-group-identity-status)
                     (lambda (_record) 'mismatch))
                    ((symbol-function 'signal-process)
                     (lambda (&rest args)
                       (push args calls)
                       (apply original-signal-process args))))
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-reused-group"
                    :command
                    '("sh" "-c"
                      "ps -o pgid= -p $$ | tr -d ' ' > group.pid; trap '' TERM; sleep 30 & wait")
                    :workdir remote-root
                    :writable-roots (list remote-root)
                    :timeout 0.05)))
              (setq group-id
                    (string-to-number
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents pid-file)
                        (buffer-string)))))
              (should (eq 'unknown (plist-get result :termination)))
              (should-not
               (cl-some
                (lambda (args)
                  (and (integerp (car args))
                       (< (car args) 0)
                       (memq (cadr args) '(TERM KILL))
                       (equal remote-root (nth 2 args))))
                calls)))))
      (when (and group-id (> group-id 0))
        (ignore-errors
          (funcall original-signal-process (- group-id) 'KILL)))
      (delete-directory root t)))
  :doc "runs the Bubblewrap and FD grant wrappers in the target path domain"
  (let* ((root (make-temp-file "mevedel-remote-bwrap-" t))
         (external-root (make-temp-file "mevedel-remote-grant-" t))
         (client-only (make-temp-file "mevedel-client-protected-" t))
         (external-target (expand-file-name "secret.txt" external-root))
         (external (expand-file-name "secret-link" external-root))
         (bin (expand-file-name "bin" root))
         (bwrap (expand-file-name "bwrap" bin))
         (args-file (expand-file-name "bwrap.args" root))
         (target-protected-name
          (make-temp-name ".mevedel-test-credentials-"))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (remote-external-root
          (format "/mevedelmock:%s:%s/" (system-name) external-root))
         (remote-external
          (format "/mevedelmock:%s:%s" (system-name) external))
         (tramp-remote-path (cons bin tramp-remote-path))
         (mevedel-protected-paths
          `((,(concat client-only "/**") . inaccessible)
            (,(concat "~/" target-protected-name "/**") . inaccessible)
            (,(concat remote-external-root "**") . inaccessible)))
         (mevedel-sandbox--probe-cache nil)
         (mevedel-sandbox-mode 'required))
    (unwind-protect
        (progn
          (make-directory bin t)
          (write-region "secret" nil external-target nil 'silent)
          (make-symbolic-link external-target external)
          (with-temp-file bwrap
            (insert "#!/bin/sh\n"
                    "printf '%s\\n' \"$@\" > "
                    (shell-quote-argument args-file)
                    "\n"
                    "while [ \"$#\" -gt 0 ]; do\n"
                    "  if [ \"$1\" = -- ]; then shift; exec \"$@\"; fi\n"
                    "  shift\n"
                    "done\n"
                    "exit 2\n"))
          (set-file-modes bwrap #o700)
          (mevedel-test--with-local-shell-tramp nil
            (let* ((target-home
                    (file-truename (concat (file-remote-p remote-root) "~/")))
                   (target-protected
                    (file-name-concat target-home target-protected-name)))
              (unwind-protect
                  (progn
                    (make-directory target-protected t)
                    (let ((result
                           (mevedel-execution-run-one-shot
                            :name "mevedel-test-remote-bwrap"
                            :command
                            (list "sh" "-c" "cat \"$1\""
                                  "sh" remote-external)
                            :workdir remote-root
                            :writable-roots (list remote-root)
                            :additional-permissions
                            (list :file-system
                                  (list (list :path remote-external
                                              :access 'read))))))
                      (should-not (plist-get result :error))
                      (should (= 0 (plist-get result :exit-code)))
                      (should (equal "secret" (plist-get result :output)))
                      (should (eq 'bubblewrap
                                  (plist-get
                                   (plist-get result :sandbox-facts)
                                   :sandbox)))
                      (let ((arguments
                             (with-temp-buffer
                               (insert-file-contents args-file)
                               (buffer-string))))
                        (should-not
                         (string-match-p "mevedelmock" arguments))
                        (should-not
                         (string-match-p
                          (regexp-quote client-only) arguments))
                        (should
                         (string-match-p
                          (regexp-quote
                           (file-local-name target-protected))
                          arguments))
                        (should (string-match-p
                                 (regexp-quote external) arguments))
                        (should (string-match-p
                                 (regexp-quote external-target)
                                 arguments)))))
                (when (file-exists-p target-protected)
                  (delete-directory target-protected t)))))
      (delete-directory client-only t)
      (delete-directory external-root t)
      (delete-directory root t)))))

(mevedel-deftest mevedel-execution-start-bash/remote ()
  ,test
  (test)
  :doc "keeps the live spool local and out of model-facing target facts"
  (let* ((root (make-temp-file "mevedel-remote-managed-spool-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session initial execution-id spool)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root)
                initial
                (test-mevedel-execution--start-managed
                 session remote-root
                 '("sh" "-c" "printf ready; sleep 30")
                 :yield-time-ms 10))
          (let ((facts (plist-get initial :facts)))
            (setq execution-id (plist-get facts :execution-id))
            (should (stringp execution-id))
            (should-not (plist-get facts :output-path)))
          (setq spool
                (plist-get (car (mevedel-execution-list-user session))
                           :artifact-path))
          (should (stringp spool))
          (should-not (file-remote-p spool))
          (should (file-exists-p spool)))
      (when session
        (mevedel-execution-teardown-session session))
      (when spool
        (should-not (file-exists-p spool)))
      (delete-directory root t)))

  :doc "classifies transport exit before PGID capture as unknown"
  (let* ((root (make-temp-file "mevedel-remote-missing-pgid-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function 'mevedel-execution--remote-command)
                (lambda (record _command)
                  (setf (mevedel-execution--record-group-marker record)
                        "missing-pgid-marker"
                        (mevedel-execution--record-group-marker-buffer record)
                        "")
                  '("sh" "-c" "exit 0"))))
            (setq result
                  (test-mevedel-execution--start-managed
                   session remote-root '("sh" "-c" "true")
                   :yield-time-ms nil)))
          (should (eq 'unknown
                      (plist-get (plist-get result :facts) :termination)))
          (should (mevedel-execution-mutation-blocked-p session)))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t)))

  :doc "keeps the latch when remote launch errors after process attempt"
  (let* ((root (make-temp-file "mevedel-remote-launch-attempt-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function 'mevedel-execution--launch-record)
                (lambda (record &rest _)
                  (setf (mevedel-execution--record-launch-attempted-p record)
                        t)
                  (error "Injected post-attempt launch failure"))))
            (setq result
                  (test-mevedel-execution--start-managed
                   session remote-root '("sh" "-c" "true")
                   :yield-time-ms nil)))
          (should (eq 'unknown
                      (plist-get (plist-get result :facts) :termination)))
          (should (mevedel-execution-mutation-blocked-p session))
          (should
           (mevedel-session-durability-unsettled-mutation-p session))
          (mevedel-execution-acknowledge-unknown session)
          (should-not
           (mevedel-session-durability-unsettled-mutation-p session)))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t)))

  :doc "stages omitted output on the target before exposing its native path"
  (let* ((root (make-temp-file "mevedel-remote-published-output-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session result published-path published-content published-coding)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-publish-text)
                (lambda (_session path content &optional coding)
                  (setq published-path path
                        published-content content
                        published-coding coding)
                  'published)))
            (setq result
                  (test-mevedel-execution--start-managed
                   session remote-root
                   '("sh" "-c" "printf '%03000d' 0")
                   :yield-time-ms nil)))
          (should (stringp published-path))
          (should (file-remote-p published-path))
          (should (= 3000 (length published-content)))
          (should (eq 'utf-8-unix published-coding))
          (let ((output-path
                 (plist-get (plist-get result :facts) :output-path)))
            (should (stringp output-path))
            (should-not (file-remote-p output-path))
            (should-not (string-match-p "mevedelmock" output-path))
            (should (string-suffix-p
                     "/tool-results/executions/exec-000001.log"
                     output-path))))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t)))

  :doc "does not expose a target output path when publication fails"
  (let* ((root (make-temp-file "mevedel-remote-output-failure-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-publish-text)
                (lambda (&rest _)
                  (error "Publication failed"))))
            (setq result
                  (test-mevedel-execution--start-managed
                   session remote-root
                   '("sh" "-c" "printf '%03000d' 0")
                   :yield-time-ms nil)))
          (should-not (plist-get (plist-get result :facts) :output-path)))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t)))

  :doc "side and root writers share one shallow durable mutation authority"
  (let* ((root (make-temp-file "mevedel-remote-overlap-latch-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         (mevedel-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         parent side root-buffer side-buffer
         first second first-done second-done)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (let ((workspace (test-mevedel-execution--workspace remote-root)))
            (setq parent (mevedel-session-create "main" workspace remote-root)
                  side (mevedel-session-create "side" workspace remote-root)
                  root-buffer (generate-new-buffer " *remote-root-authority*")
                  side-buffer (generate-new-buffer " *remote-side-writer*"))
            (setf (mevedel-session-audit-session side) parent
                  (mevedel-session-sandbox-mode parent) 'off
                  (mevedel-session-sandbox-mode side) 'off)
            (with-current-buffer root-buffer
              (setq-local mevedel--workspace workspace
                          mevedel--session parent)
              (setq default-directory remote-root))
            (with-current-buffer side-buffer
              (setq-local mevedel--workspace workspace
                          mevedel--session side)
              (setq default-directory remote-root)))
          (puthash
           (mevedel-execution-target-identity
            (mevedel-session-execution-target parent))
           t mevedel-session-durability--disclosed-targets)
          (setq first
                (test-mevedel-execution--start-managed
                 side remote-root
                 '("sh" "-c" "printf first-ready; sleep 30")
                 :data-buffer side-buffer
                 :yield-time-ms 10)
                second
                (test-mevedel-execution--start-managed
                 parent remote-root
                 '("sh" "-c" "printf second-ready; sleep 30")
                 :data-buffer root-buffer
                 :yield-time-ms 10))
          (should (mevedel-session-save-path parent))
          (should (buffer-local-value 'buffer-file-name root-buffer))
          (should-not (buffer-local-value 'buffer-file-name side-buffer))
          (should (mevedel-session-durability-unsettled-mutation-p parent))
          (mevedel-execution-stop
           side "main" (plist-get (plist-get first :facts) :execution-id)
           (lambda (value) (setq first-done value)))
          (test-mevedel-execution--wait (lambda () first-done))
          (should (mevedel-session-durability-unsettled-mutation-p parent))
          (mevedel-execution-stop
           parent "main" (plist-get (plist-get second :facts) :execution-id)
           (lambda (value) (setq second-done value)))
          (test-mevedel-execution--wait (lambda () second-done))
          (should-not
           (mevedel-session-durability-unsettled-mutation-p parent)))
      (dolist (session (list side parent))
        (when session
          (mevedel-execution-teardown-session session)))
      (when (and parent (mevedel-session-save-path parent))
        (mevedel-session-durability-lease-release
         (mevedel-session-save-path parent) parent))
      (dolist (buffer (list side-buffer root-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "keeps queued output reachable through its logical session path"
  (let* ((root (make-temp-file "mevedel-remote-output-queued-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (mevedel-sandbox-mode 'off)
         session result)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-publish-text)
                (lambda (&rest _) 'queued)))
            (setq result
                  (test-mevedel-execution--start-managed
                   session remote-root
                   '("sh" "-c" "printf '%03000d' 0")
                   :yield-time-ms nil)))
          (let ((output-path
                 (plist-get (plist-get result :facts) :output-path)))
            (should (stringp output-path))
            (should-not (file-remote-p output-path))
            (should
             (string-suffix-p
              "/tool-results/executions/exec-000001.log"
              output-path))))
      (when session
        (mevedel-execution-teardown-session session))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-unsettled-mutation-p ()
  ,test
  (test)
  :doc "reports a durable latch even when no transient process record exists"
  (let* ((root (make-temp-file "mevedel-remote-durable-latch-" t))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (session nil))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (mevedel-session-persistence-assert-mutation-authority session)
          (should
           (mevedel-session-durability-set-unsettled-mutation session t))
          (should (mevedel-execution-unsettled-mutation-p session))
          (should (mevedel-execution-mutation-blocked-p session))
          (should
           (mevedel-session-durability-set-unsettled-mutation session nil))
          (should-not (mevedel-execution-unsettled-mutation-p session)))
      (when (and session (mevedel-session-save-path session))
        (mevedel-session-durability-lease-release
         (mevedel-session-save-path session) session))
      (when (file-directory-p root)
        (delete-directory root t)))))


;;
;;; Opt-in real transport acceptance

(defun test-mevedel-execution-remote--apply-diff (session root)
  "Apply the acceptance diff through the public diff command at ROOT."
  (let ((buffer (generate-new-buffer " *mevedel real remote diff*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory root)
          (setq-local mevedel--workspace (mevedel-session-workspace session)
                      mevedel--session session)
          (insert
           (concat "diff --git a/acceptance.el b/acceptance.el\n"
                   "--- a/acceptance.el\n"
                   "+++ b/acceptance.el\n"
                   "@@ -1,7 +1,7 @@\n"
                   " ;;; acceptance.el --- real remote acceptance -*- lexical-binding: t -*-\n"
                   " \n"
                   " (defun mevedel-real-remote-target ()\n"
                   "   \"Return the remote acceptance marker.\"\n"
                   "-  \"patched\")\n"
                   "+  \"diff-applied\")\n"
                   " \n"
                   " (defun mevedel-real-remote-caller ()\n"))
          (diff-mode)
          (mevedel-diff-apply-buffer t)
          (when-let* ((file-buffer
                       (find-buffer-visiting
                        (file-name-concat root "acceptance.el"))))
            (kill-buffer file-buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun test-mevedel-execution-remote--exercise-file-tools
    (session target root)
  "Exercise ordinary file tools for SESSION and TARGET below ROOT."
  (let* ((file (file-name-concat root "acceptance.el"))
         (tree-file (file-name-concat root "acceptance.sh"))
         (native-file (mevedel-execution-target-native-path target file))
         (identifier "mevedel-real-remote-target"))
    (write-region
     (concat
      ";;; acceptance.el --- real remote acceptance -*- lexical-binding: t -*-\n"
      "\n"
      "(defun mevedel-real-remote-target ()\n"
      "  \"Return the remote acceptance marker.\"\n"
      "  \"alpha\")\n"
      "\n"
      "(defun mevedel-real-remote-caller ()\n"
      "  (mevedel-real-remote-target))\n"
      "\n"
      ";; acceptance needle\n")
     nil file nil 'silent)
    (should
     (string-match-p
      "acceptance needle"
      (test-mevedel-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (string-match-p
      (regexp-quote native-file)
      (test-mevedel-execution-remote--run-tool
       session "Glob" '(:pattern "*.el" :path "."))))
    (let ((grep
           (test-mevedel-execution-remote--run-tool
            session "Grep"
            '(:pattern "acceptance needle" :path "."
              :output_mode "content"))))
      (should (string-match-p "acceptance needle" grep))
      (should (string-match-p (regexp-quote native-file) grep)))
    (let ((result
           (test-mevedel-execution-remote--run-tool
            session "ApplyPatch"
            (list
             :patch
             (string-join
              '("*** Begin Patch"
                "*** Update File: acceptance.el"
                "@@"
                "-  \"alpha\")"
                "+  \"patched\")"
                "*** End Patch")
              "\n")))))
      (should (string-match-p "Applied patch" result)))
    (should
     (string-match-p
      "patched"
      (test-mevedel-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (test-mevedel-execution-remote--apply-diff session root)
    (should
     (string-match-p
      "diff-applied"
      (test-mevedel-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (string-match-p
      identifier
      (test-mevedel-execution-remote--run-tool
       session "Imenu" '(:file_path "acceptance.el"))))
    (let ((default-directory root)
          (process-environment nil))
      (should (zerop (process-file "git" nil nil nil "init" "--quiet")))
      (should (zerop (process-file "git" nil nil nil "add" "acceptance.el"))))
    (let ((result
           (test-mevedel-execution-remote--run-tool
            session "XrefReferences"
            (list :identifier identifier :file_path "acceptance.el"))))
      (should (string-match-p identifier result))
      (should (string-match-p (regexp-quote native-file) result)))
    (should
     (string-match-p
      "not supported for remote workspaces"
      (test-mevedel-execution-remote--run-tool
       session "XrefDefinitions"
       (list :pattern identifier :file_path "acceptance.el"))))
    (write-region
     "#!/usr/bin/env bash -*- mode: bash-ts -*-\necho acceptance\n"
     nil tree-file nil 'silent)
    (let ((result
           (test-mevedel-execution-remote--run-tool
            session "Treesitter"
            '(:file_path "acceptance.sh" :whole_file t))))
      (should-not (string-prefix-p "Error" result))
      (should (string-match-p "program" result)))
    file))

(defun test-mevedel-execution-remote--exercise-reconnect (target)
  "Close and reconnect TARGET, then assert a fresh ready connection."
  (let* ((root (mevedel-execution-target-workspace-root target))
         (before (mevedel-execution-target-connection-process target)))
    (should (processp before))
    (tramp-cleanup-connection
     (tramp-dissect-file-name root) nil t)
    (should-not (process-live-p before))
    (let* ((readiness (mevedel-execution-target-probe target nil 'off))
           (after (mevedel-execution-target-connection-process target)))
      (should (eq 'ready (plist-get readiness :status)))
      (should (process-live-p after))
      (should-not (eq before after)))))

(defun test-mevedel-execution-remote--drop-target-processes (vector)
  "Delete every live TRAMP process belonging to VECTOR."
  (dolist (process (process-list))
    (when (and (process-live-p process)
               (equal vector (process-get process 'tramp-vector)))
      (set-process-query-on-exit-flag process nil)
      (ignore-errors (delete-process process)))))

(defun test-mevedel-execution-remote--read-group-identity (path)
  "Return the process-group, child PID, and child start time stored at PATH."
  (mapcar
   #'string-to-number
   (split-string
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))))))

(defun test-mevedel-execution-remote--cleanup-target-group
    (root group-id child-pid child-start-time)
  "Kill GROUP-ID at ROOT when CHILD-PID still has CHILD-START-TIME."
  (let ((default-directory root)
        (process-environment nil))
    (process-file
     "bash" nil nil nil "-c"
     (concat
      "pid=$1; group=$2; expected=$3; "
      "test -r \"/proc/$pid/stat\" || exit 0; "
      "IFS= read -r stat < \"/proc/$pid/stat\" || exit 70; "
      "rest=${stat##*) }; set -- $rest; "
      "test \"$3\" = \"$group\" || exit 71; "
      "test \"${20}\" = \"$expected\" || exit 72; "
      "kill -KILL -- \"-$group\"")
     "mevedel-loss-cleanup"
     (number-to-string child-pid)
     (number-to-string group-id)
     (number-to-string child-start-time))))

(defun test-mevedel-execution-remote--exercise-connection-loss
    (variable method)
  "Exercise unprovable connection loss for VARIABLE using METHOD."
  (let* ((base
          (test-mevedel-execution-remote--real-root variable method))
         (root
          (test-mevedel-execution-remote--real-temp-directory
           base (format "mevedel-%s-loss-" method)))
         (identity-file (file-name-concat root "lost-group.identity"))
         session target fault-timer group-id child-pid child-start-time)
    (unwind-protect
        (let* ((mevedel-sandbox-mode 'off)
               (workspace (test-mevedel-execution--workspace root))
               (_ (setq session
                        (mevedel-session-create "main" workspace root)))
               (_ (setf (mevedel-session-sandbox-mode session) 'off))
               (_ (setq target
                        (mevedel-session-execution-target session)))
               (readiness
                (mevedel-execution-target-probe target t 'off))
               (vector (tramp-dissect-file-name root))
               initial execution-id result)
          (test-mevedel-execution-remote--accept-storage session)
          (should (eq 'ready (plist-get readiness :status)))
          (setq
           initial
           (test-mevedel-execution--start-managed
            session root
            (list
             "bash" "-c"
             (concat
              "trap '' HUP INT TERM; "
              "identity=$1; "
              "bash -c 'trap \"\" HUP INT TERM; exec sleep 90' & "
              "child=$!; "
              "IFS= read -r stat < \"/proc/$child/stat\" || exit 70; "
              "rest=${stat##*) }; set -- $rest; "
              "printf '%s %s %s\\n' \"$3\" \"$child\" \"${20}\" > \"$identity\"; "
              "printf 'ready\\n'; "
              "while kill -0 \"$child\" 2>/dev/null; do "
              "wait \"$child\" || :; done")
             "mevedel-loss-workload" identity-file)
            :yield-time-ms 250)
           execution-id
           (plist-get (plist-get initial :facts) :execution-id))
          (should (string-match-p "ready" (plist-get initial :output)))
          (should (stringp execution-id))
          (test-mevedel-execution-remote--wait
           (lambda () (file-readable-p identity-file))
           "Remote loss workload did not publish its identity")
          (pcase-let
              ((`(,group ,child ,start)
                (test-mevedel-execution-remote--read-group-identity
                 identity-file)))
            (setq group-id group
                  child-pid child
                  child-start-time start))
          (should (> group-id 0))
          (should (> child-pid 0))
          (should (> child-start-time 0))
          (setq fault-timer
                (run-at-time
                 0 0.02
                 #'test-mevedel-execution-remote--drop-target-processes
                 vector))
          (tramp-cleanup-connection vector nil t)
          (mevedel-execution-stop
           session "/root" execution-id
           (lambda (value) (setq result value)))
          (test-mevedel-execution-remote--wait
           (lambda () result)
           "Connection-loss execution did not settle" 30)
          (cancel-timer fault-timer)
          (setq fault-timer nil)
          (should
           (eq 'unknown
               (plist-get (plist-get result :facts) :termination)))
          (should (mevedel-execution-mutation-blocked-p session))
          (should
           (eq 'ready
               (plist-get
                (mevedel-execution-target-probe target t 'off)
                :status)))
          (should-error
           (mevedel-execution-start-bash
            #'ignore :session session :owner "/root"
            :command '("bash" "-c" "true")
            :workdir root :writable-roots (list root)
            :yield-time-ms nil)
           :type 'mevedel-execution-error)
          (mevedel-execution-acknowledge-unknown session)
          (should
           (zerop
            (test-mevedel-execution-remote--cleanup-target-group
             root group-id child-pid child-start-time)))
          (test-mevedel-execution-remote--wait
           (lambda ()
             (test-mevedel-execution-remote--target-process-gone-p
              root child-pid))
           "Remote descendant survived connection-loss cleanup"))
      (when (timerp fault-timer)
        (cancel-timer fault-timer))
      (when (and group-id child-pid child-start-time)
        (ignore-errors
          (test-mevedel-execution-remote--cleanup-target-group
           root group-id child-pid child-start-time)))
      (when session
        (mevedel-execution-teardown-session session))
      (when (file-exists-p root)
        (delete-directory root t)))))

(defun test-mevedel-execution-remote--exercise-term-kill (root)
  "Exercise target PGID capture, TERM/KILL escalation, and cleanup at ROOT."
  (let ((identity-file (file-name-concat root "term-group.identity"))
        (term-file (file-name-concat root "term.seen"))
        (mevedel-sandbox-mode 'off)
        result group-id child-pid child-start-time parent-pid parent-start-time)
    (unwind-protect
        (progn
          (setq result
                (mevedel-execution-run-one-shot
                 :name "mevedel-real-remote-group"
                 :command
                 (list
                  "bash" "-c"
                  (concat
                   "term=$1; identity=$2; parent=$$; "
                   "trap 'printf term > \"$term\"' TERM; "
                   "IFS= read -r stat < \"/proc/$parent/stat\" || exit 70; "
                   "rest=${stat##*) }; set -- $rest; "
                   "group=$3; parent_start=${20}; "
                   "bash -c 'trap \"\" TERM; while :; do sleep 1; done' & "
                   "child=$!; "
                   "IFS= read -r stat < \"/proc/$child/stat\" || exit 71; "
                   "rest=${stat##*) }; set -- $rest; "
                   "test \"$3\" = \"$group\" || exit 72; "
                   "printf '%s %s %s %s %s\\n' "
                   "\"$group\" \"$child\" \"${20}\" "
                   "\"$parent\" \"$parent_start\" > \"$identity\"; "
                   "while :; do wait \"$child\" || :; done")
                  "mevedel-real-group" term-file identity-file)
                 :workdir root :writable-roots (list root)
                 :timeout 0.5))
          (should (plist-get result :timed-out-p))
          (should (eq 'timed-out (plist-get result :termination)))
          (should-not (plist-get result :error))
          (should (equal "term"
                         (string-trim
                          (with-temp-buffer
                            (insert-file-contents term-file)
                            (buffer-string)))))
          (pcase-let
              ((`(,group ,child ,child-start ,parent ,parent-start)
                (mapcar
                 #'string-to-number
                 (split-string
                  (string-trim
                   (with-temp-buffer
                     (insert-file-contents identity-file)
                     (buffer-string)))))))
            (setq group-id group
                  child-pid child
                  child-start-time child-start
                  parent-pid parent
                  parent-start-time parent-start))
          (should (> child-pid 0))
          (test-mevedel-execution-remote--wait
           (lambda ()
             (test-mevedel-execution-remote--target-process-gone-p
              root child-pid))
           "Remote descendant survived TERM/KILL escalation"))
      (when (and (null group-id) (file-readable-p identity-file))
        (ignore-errors
          (pcase-let
              ((`(,group ,child ,child-start ,parent ,parent-start)
                (mapcar
                 #'string-to-number
                 (split-string
                  (string-trim
                   (with-temp-buffer
                     (insert-file-contents identity-file)
                     (buffer-string)))))))
            (setq group-id group
                  child-pid child
                  child-start-time child-start
                  parent-pid parent
                  parent-start-time parent-start))))
      (when (and group-id child-pid child-start-time)
        (ignore-errors
          (test-mevedel-execution-remote--cleanup-target-group
           root group-id child-pid child-start-time)))
      (when (and group-id parent-pid parent-start-time)
        (ignore-errors
          (test-mevedel-execution-remote--cleanup-target-group
           root group-id parent-pid parent-start-time))))))

(defun test-mevedel-execution-remote--exercise-pty (session root)
  "Exercise streaming, PTY stdin, and Ctrl-C for SESSION at ROOT."
  (let ((mevedel-sandbox-mode 'off)
        initial after-input final execution-id)
    (setq initial
          (test-mevedel-execution--start-managed
           session root
           '("bash" "-c"
             "printf 'ready\\n'; IFS= read -r line; printf 'input=%s\\n' \"$line\"; while :; do sleep 1; done")
           :tty t :tool-args '(:command "real PTY acceptance")
           :yield-time-ms 250))
    (should (string-match-p "ready" (plist-get initial :output)))
    (should (eq 'running (plist-get (plist-get initial :facts) :state)))
    (setq execution-id
          (plist-get (plist-get initial :facts) :execution-id))
    (should (stringp execution-id))
    (setq after-input
          (test-mevedel-execution--observe
           session execution-id :chars "hello\n" :wait-ms 3000))
    (should (string-match-p "input=hello" (plist-get after-input :output)))
    (setq final
          (test-mevedel-execution--observe
           session execution-id :chars (string 3) :wait-ms 10000))
    (should (plist-get final :claimed-final-p))
    (should (eq 'completed (plist-get (plist-get final :facts) :state)))
    (should-not (zerop (plist-get (plist-get final :facts) :exit-code)))))

(defun test-mevedel-execution-remote--exercise-concurrency
    (session root readable-file)
  "Read READABLE-FILE while root and agent commands run in SESSION at ROOT."
  (let* ((agent-context
          (mevedel-agent-invocation-create (mevedel-agent-default)))
         (_ (setf (mevedel-agent-invocation-path agent-context)
                  "/root/acceptance"
                  (mevedel-agent-invocation-parent-session agent-context)
                  session
                  (mevedel-agent-invocation-transcript-status agent-context)
                  'running))
         (agent-owner
          (mevedel-agent-invocation-require-path agent-context))
         (root-result
          (test-mevedel-execution-remote--start-reader
           session root "/root" "root"))
         (agent-result
          (test-mevedel-execution-remote--start-reader
           session root agent-owner "agent" agent-context))
         (root-id (plist-get (plist-get root-result :facts) :execution-id))
         (agent-id (plist-get (plist-get agent-result :facts) :execution-id)))
    (should (stringp root-id))
    (should (stringp agent-id))
    (should
     (equal '("/root" "/root/acceptance")
            (sort
             (mapcar
              (lambda (entry) (plist-get entry :owner))
              (mevedel-execution-list-user session))
             #'string<)))
    (should
     (string-match-p
      "acceptance needle"
      (test-mevedel-execution-remote--run-tool
       session "Read" (list :file_path readable-file))))
    (should (= 2 (mevedel-execution-count-user session)))))

(defun test-mevedel-execution-remote--exercise-transport (variable method)
  "Exercise the real transport from VARIABLE using expected METHOD."
  (let* ((base
          (test-mevedel-execution-remote--real-root variable method))
         (root
          (test-mevedel-execution-remote--real-temp-directory
           base (format "mevedel-%s-acceptance-" method)))
         session)
    (unwind-protect
        (let* ((mevedel-sandbox-mode 'off)
               (workspace (test-mevedel-execution--workspace root))
               (_ (setq session (mevedel-session-create "main" workspace root)))
               (target (mevedel-session-execution-target session)))
          (test-mevedel-execution-remote--accept-storage session)
          (setf (mevedel-session-permission-mode session) 'full-auto
                (mevedel-session-sandbox-mode session) 'off)
          (should (eq method (mevedel-execution-target-method target)))
          (should (eq method
                      (plist-get (mevedel-execution-target-identity target)
                                 :method)))
          (should (mevedel-execution-target-supported-p target))
          (let ((readiness
                 (mevedel-execution-target-probe target t 'off)))
            (should (eq 'ready (plist-get readiness :status)))
            (should (equal "Linux"
                           (plist-get readiness :operating-system)))
            (should (assoc "HOME"
                           (mevedel-execution-target-environment target)))
            (dolist (capability '(rg bash setsid))
              (should
               (stringp
                (mevedel-execution-target-capability target capability)))))
          (test-mevedel-execution-remote--exercise-reconnect target)
          (let ((file
                 (test-mevedel-execution-remote--exercise-file-tools
                  session target root)))
            (test-mevedel-execution-remote--exercise-term-kill root)
            (test-mevedel-execution-remote--exercise-pty session root)
            (test-mevedel-execution-remote--exercise-concurrency
             session root file)))
      (when session
        (mevedel-execution-teardown-session session))
      (when (file-exists-p root)
        (delete-directory root t)))))

(defun test-mevedel-execution-remote--exercise-bwrap (variable method)
  "Exercise an exact target Bubblewrap grant for VARIABLE and METHOD."
  (let* ((base
          (test-mevedel-execution-remote--real-root variable method))
         (root
          (test-mevedel-execution-remote--real-temp-directory
           base (format "mevedel-%s-bwrap-root-" method)))
         (external-root
          (test-mevedel-execution-remote--real-temp-directory
           base (format "mevedel-%s-bwrap-grant-" method)))
         (target (mevedel-execution-target-create root))
         (secret (file-name-concat external-root "secret.txt"))
         (link (file-name-concat external-root "secret-link"))
         (sibling (file-name-concat external-root "sibling.txt"))
         (mevedel-protected-paths
          `((,(concat external-root "**") . inaccessible)))
         (mevedel-sandbox-mode 'required))
    (unwind-protect
        (progn
          (let ((readiness
                 (mevedel-execution-target-probe target t 'required)))
            (if (eq 'sandbox-unavailable (plist-get readiness :reason))
                (ert-fail
                 (or (plist-get readiness :sandbox-reason)
                     "Required target Bubblewrap is unavailable"))
              (should (eq 'ready (plist-get readiness :status)))
              (should (eq 'bubblewrap
                          (plist-get readiness :sandbox-status)))))
          (write-region "secret" nil secret nil 'silent)
          (write-region "sibling" nil sibling nil 'silent)
          (make-symbolic-link "secret.txt" link)
          (let ((result
                 (mevedel-execution-run-one-shot
                  :name "mevedel-real-remote-bwrap"
                  :command
                  (list
                   "bash" "-c"
                   (concat
                    "cat \"$1\"; "
                    "if test -r \"$2\"; then exit 71; fi; "
                    "while read -r _ destination _; do "
                    "test \"$destination\" != 00000000 || exit 72; "
                    "done < /proc/net/route")
                   "mevedel-real-bwrap" link sibling)
                  :workdir root :writable-roots (list root)
                  :timeout 30
                  :additional-permissions
                  (list :file-system
                        (list (list :path link :access 'read))))))
            (should-not (plist-get result :error))
            (should (= 0 (plist-get result :exit-code)))
            (should (equal "secret" (plist-get result :output)))
            (should
             (eq 'bubblewrap
                 (plist-get (plist-get result :sandbox-facts) :sandbox)))))
      (when (file-exists-p external-root)
        (delete-directory external-root t))
      (when (file-exists-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-real-remote-acceptance/ssh
  (:tags (external remote ssh)
   :doc "exercises the complete opt-in real SSH transport matrix")
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_SSH_ROOT" 'ssh))

(mevedel-deftest mevedel-real-remote-acceptance/docker
  (:tags (external remote docker)
   :doc "exercises the complete opt-in real Docker transport matrix")
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker))

(mevedel-deftest mevedel-real-remote-acceptance/podman
  (:tags (external remote podman)
   :doc "exercises the complete opt-in real Podman transport matrix")
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(mevedel-deftest mevedel-real-remote-loss/ssh
  (:tags (external remote ssh connection-loss)
   :doc "classifies unprovable real SSH loss and cleans its descendant")
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_SSH_ROOT" 'ssh))

(mevedel-deftest mevedel-real-remote-loss/docker
  (:tags (external remote docker connection-loss)
   :doc "classifies unprovable real Docker loss and cleans its descendant")
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker))

(mevedel-deftest mevedel-real-remote-loss/podman
  (:tags (external remote podman connection-loss)
   :doc "classifies unprovable real Podman loss and cleans its descendant")
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(mevedel-deftest mevedel-real-remote-bwrap/ssh
  (:tags (external remote ssh sandbox)
   :doc "confines an exact symlink grant through real SSH Bubblewrap")
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_SSH_ROOT" 'ssh))

(mevedel-deftest mevedel-real-remote-bwrap/docker
  (:tags (external remote docker sandbox)
   :doc "confines an exact symlink grant through real Docker Bubblewrap")
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker))

(mevedel-deftest mevedel-real-remote-bwrap/podman
  (:tags (external remote podman sandbox)
   :doc "confines an exact symlink grant through real Podman Bubblewrap")
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(provide 'test-mevedel-execution-remote)

;;; test-mevedel-execution-remote.el ends here
