;;; test-mevedel-execution-remote.el --- Remote managed execution tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests target-aware process dispatch and remote process-group authority.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel-agents)
(require 'mevedel-diff-apply)
(require 'mevedel-execution)
(require 'mevedel-execution-target)
(require 'mevedel-file-state)
(require 'mevedel-hooks)
(require 'mevedel-overlays)
(require 'mevedel-permissions)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-skills-core)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-session-persistence)
(require 'mevedel-session-recovery)
(require 'mevedel-telemetry)
(require 'mevedel-tool-media)
(require 'mevedel-tool-registry)
(require 'mevedel-tools)
(require 'mevedel-workspace)
(require 'mevedel-workspace-identity)
(require 'mevedel-worktree)
(require 'mevedel-plan-mode)
(require 'mevedel)
(require 'tramp)
(require 'tramp-container)
(require 'tramp-sh)

(declare-function mevedel-version "mevedel" (&optional here message))
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

(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)

(defconst test-mevedel-execution-remote--directory
  (or (and load-file-name (file-name-directory load-file-name))
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory)
  "Directory containing the real remote acceptance tests.")


;;
;;; Test support

(defun test-mevedel-execution-remote--real-root (variable method)
  "Return the opt-in real TRAMP root from VARIABLE for METHOD.

The root must already exist, be writable, and be reachable through normal
  TRAMP authentication.  The tests never provision, start, or stop a target."
  (let ((value (getenv variable)))
    (unless value
      (ert-skip (format "%s is not set" variable)))
    (when-let ((config (and (eq method 'ssh)
                            (getenv "MEVEDEL_TEST_SSH_CONFIG"))))
      (setq tramp-use-connection-share t
            tramp-ssh-controlmaster-options
            (format "-F %s" (shell-quote-argument config))))
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

(defun test-mevedel-execution-remote--real-temp-directory
    (root stem &optional persistent)
  "Create and return a fresh target-side directory named STEM.

The directory is created in the target's temporary directory, which is
where a disposable journey belongs.  PERSISTENT places it inside ROOT
instead: ROOT is the project volume that outlives the target, and a journey
that replaces the target must find its durable state again afterwards."
  (let ((default-directory root))
    (if persistent
        (let ((directory
               (file-name-as-directory
                (file-name-concat root (make-temp-name stem)))))
          (make-directory directory t)
          directory)
      (file-name-as-directory (make-nearby-temp-file stem t)))))

(defun test-mevedel-execution-remote--stage (format-string &rest args)
  "Log a bounded remote acceptance stage when diagnostics are enabled."
  (when (getenv "MEVEDEL_TEST_REMOTE_DIAGNOSTICS")
    (apply #'message (concat "mevedel: remote diagnostic: " format-string)
           args)))

(defun test-mevedel-execution-remote-run-selector ()
  "Run the exact generated ERT journey named by the environment.

`MEVEDEL_TEST_REMOTE_TEST' must be a generated test symbol such as
`mevedel-real-remote-acceptance/test' or
`mevedel-real-remote-acceptance/test@1'.  Reading it inside Emacs keeps the
shell launcher independent of Lisp source interpolation."
  (let ((name (getenv "MEVEDEL_TEST_REMOTE_TEST")))
    (unless (and name (not (string-empty-p name)))
      (error "MEVEDEL_TEST_REMOTE_TEST is not set"))
    (ert-run-tests-batch-and-exit (intern name))))

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

(defun test-mevedel-execution-remote--exercise-ordinary-entry
    (root workspace)
  "Enter a fresh remote session through the ordinary `mevedel' command.

The entry path is transport-neutral, while the target probe and session setup
are real for ROOT.  The temporary chat buffer and its view are cleaned up
before returning."
  (let ((context (generate-new-buffer " *remote-ordinary-entry-context*"))
        chat-buffer)
    (unwind-protect
        (progn
          (puthash
           (mevedel-execution-target-identity
            (mevedel-execution-target-create root))
           t mevedel-session-durability--disclosed-targets)
          (with-current-buffer context
            (setq default-directory root
                  mevedel--workspace workspace)
            (cl-letf (((symbol-function
                        'mevedel-session-persistence-choose-entry)
                       (lambda (_workspace) 'new))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "ordinary-entry"))
                      ((symbol-function 'mevedel--display-chat-buffer)
                       (lambda (buffer) (setq chat-buffer buffer))))
              (mevedel)))
          (should (buffer-live-p chat-buffer))
          (with-current-buffer chat-buffer
            (should (equal "ordinary-entry"
                           (mevedel-session-name mevedel--session)))
            (should
             (eq 'ready
                 (plist-get
                  (mevedel-execution-target-probe
                   (mevedel-session-execution-target mevedel--session)
                   nil 'off)
                  :status))))
          t)
      (when (buffer-live-p chat-buffer)
        (let ((view-buffer
               (buffer-local-value 'mevedel--view-buffer chat-buffer)))
          (with-current-buffer chat-buffer
            (set-buffer-modified-p nil))
          (kill-buffer chat-buffer)
          (when (buffer-live-p view-buffer)
            (kill-buffer view-buffer))))
      (when (buffer-live-p context)
        (kill-buffer context)))))

(defun test-mevedel-execution-remote--exercise-tools-and-resources
    (session root buffer)
  "Exercise Bash, live Eval, an exact resource grant, and a target skill."
  (let* ((resource (file-name-concat root "explicit-resource.txt"))
         (skill-dir (file-name-concat root ".mevedel" "skills"
                                      "remote-acceptance"))
         (skill-file (file-name-concat skill-dir "SKILL.md"))
         (bash-file (file-name-concat root "bash-tool.txt"))
         (eval-file (file-name-concat root "live-eval.txt")))
    (write-region "resource needle\n" nil resource nil 'silent)
    (make-directory skill-dir t)
    ;; A skill is discovered only through fenced frontmatter.
    (write-region
     (concat "---\n"
             "name: remote-acceptance\n"
             "description: Real target acceptance skill\n"
             "---\n"
             "Remote skill marker.\n")
     nil skill-file nil 'silent)
    (with-current-buffer buffer
      (mevedel-permission-add-session-resource-grant session resource 'read))
    (should
     (string-match-p
      "resource needle"
      (test-mevedel-execution-remote--run-tool
       session "Read" (list :file_path resource))))
    ;; A target shell reads target-native paths; a client TRAMP spelling is
    ;; not a path there.  The echoed command also contains the marker text,
    ;; so the file itself is the evidence that the command ran.
    (let ((bash-result
           (test-mevedel-execution-remote--run-tool
            session "Bash"
            (list :command
                  (format "printf 'bash target\\n' > %s; cat %s"
                          (shell-quote-argument (file-local-name bash-file))
                          (shell-quote-argument
                           (file-local-name bash-file)))))))
      (should (string-match-p "bash target" bash-result)))
    (should (file-exists-p bash-file))
    (should (equal "bash target\n"
                   (with-temp-buffer
                     (insert-file-contents bash-file)
                     (buffer-string))))
    (let ((eval-result
           (test-mevedel-execution-remote--run-tool
            session "Eval"
            '(:expression
              "(progn (write-region \"live eval target\\n\" nil \"live-eval.txt\" nil 'silent) 'live-eval-ok)"
              :mode "live"))))
      (should (string-match-p "live-eval-ok" eval-result)))
    (should (file-exists-p eval-file))
    (let ((mevedel-skills-include-bundled nil)
          (mevedel-skill-dirs '(".mevedel/skills/")))
      (mevedel-skills-install session buffer)
      (let ((skill (mevedel-session-get-skill session "remote-acceptance")))
        (should skill)
        (should (eq 'project (mevedel-skill-source skill)))
        (should (file-remote-p (mevedel-skill-source-file skill)))
        (should (string-match-p
                 "Remote skill marker"
                 (mevedel-skill-load-body skill)))))))

(defun test-mevedel-execution-remote--exercise-control-filesystem (root)
  "Round trip pinned control operations against the real target at ROOT.

Session control stages content larger than a command line can carry, so both
the small and the staged path need proof on a real connection rather than on a
local temporary directory.  Durable session work is carried as programs of
pinned operations, and the round trips they save are exactly what a real
connection charges for, so the program path is proved here too."
  (require 'mevedel-session-control-fs)
  (let* ((directory (file-name-concat root "control-fs"))
         (small (file-name-concat directory "small.el"))
         (large (file-name-concat directory "large.bin"))
         (bytes (apply #'unibyte-string
                       (mapcar (lambda (index) (% index 256))
                               (number-sequence 1 (* 64 1024))))))
    (should (mevedel-session-control-fs-make-directory directory t))
    (should (mevedel-session-control-fs-create-file small "(:probe 1)"))
    (should-not (mevedel-session-control-fs-create-file small "(:probe 2)"))
    (should (equal "(:probe 1)"
                   (mevedel-session-control-fs-read-file small)))
    (should (mevedel-session-control-fs-write-file
             large bytes 'no-conversion))
    (should (equal bytes
                   (mevedel-session-control-fs-read-file
                    large 'no-conversion)))
    (should (member small (mevedel-session-control-fs-list-directory
                           directory "\\`small\\.el\\'")))
    (should (integerp (mevedel-session-control-fs-target-time directory)))
    ;; One program carries a whole compare-and-set plus its staged payload
    ;; across the real connection, and its proof still guards its writes.
    ;; This one is oversized for a command line, so it proves the request
    ;; file; the small program below proves argument delivery.
    (let ((results
           (mevedel-session-control-fs-run-program
            (list (list :op 'verify :path small :content "(:probe 1)")
                  (list :op 'write :path small :content "(:probe 3)")
                  (list :op 'write :path large
                        :content bytes :coding 'no-conversion)
                  (list :op 'read :path large :coding 'no-conversion)
                  (list :op 'list-directory :path directory)
                  (list :op 'target-time :path directory)))))
      (should (equal '(ok ok ok ok ok ok)
                     (mapcar (lambda (result) (plist-get result :status))
                             results)))
      (should (equal bytes (plist-get (nth 3 results) :value)))
      (should (member "small.el" (plist-get (nth 4 results) :value)))
      (should (integerp (plist-get (nth 5 results) :value))))
    (should (equal "(:probe 3)"
                   (mevedel-session-control-fs-read-file small)))
    (let ((results
           (mevedel-session-control-fs-run-program
            (list (list :op 'verify :path small :content "(:probe 1)")
                  (list :op 'write :path small :content "(:probe 4)")))))
      (should (equal 'mismatch (plist-get (nth 0 results) :status)))
      (should (equal 'skipped (plist-get (nth 1 results) :status))))
    (should (equal "(:probe 3)"
                   (mevedel-session-control-fs-read-file small)))
    (mevedel-session-control-fs-delete-directory directory)
    (should-not (mevedel-session-control-fs-directory-p directory))))

(defun test-mevedel-execution-remote--exercise-hook-provenance
    (session workspace root)
  "Run a user-loaded agent hook locally while retaining target event facts."
  (let* ((user-dir (make-temp-file "mevedel-remote-hook-user-" t))
         (script (file-name-concat user-dir "agent-hook.sh"))
         (marker (file-name-concat user-dir "agent-hook-ran"))
         (input (file-name-concat user-dir "agent-hook-input.json"))
         (agent-name
          (intern (format "remote-acceptance-agent-%d" (emacs-pid))))
         agent invocation decision done)
    (unwind-protect
        (progn
          (write-region
           (concat "#!/bin/sh\n"
                   "cat > \"$MEVEDEL_REMOTE_HOOK_INPUT\"\n"
                   "printf ran > \"$MEVEDEL_REMOTE_HOOK_MARKER\"\n"
                   "printf '{\"system_message\":\"agent hook ran\"}'\n")
           nil script nil 'silent)
          (set-file-modes script #o755)
          (let ((user-emacs-directory user-dir))
            (eval
             `(mevedel-define-agent ,agent-name
                :description "remote acceptance hook provenance"
                :hooks ((PreToolUse
                         ((:matcher "Bash"
                           :hooks ((:type command :command ,script)))))))))
            (setq agent (mevedel-agent-get agent-name)
                  invocation (mevedel-agent-invocation-create agent))
            (setf (mevedel-agent-invocation-path invocation)
                  "/root/remote-acceptance"
                  (mevedel-agent-invocation-parent-session invocation)
                  session
                  (mevedel-agent-invocation-transcript-status invocation)
                  'running)
            (let* ((payload
                    (mevedel-hooks-event-plist
                     'PreToolUse session workspace
                     :tool-name "Bash"
                     :tool-input '(:command "printf hook")))
                   (handler
                    (car
                     (mevedel-hooks--matching-handlers
                      'PreToolUse payload
                      (mevedel-hooks-effective-rules
                       session workspace nil invocation)))))
              (should handler)
              (should (eq 'user (plist-get handler :source)))
              (should (equal user-dir (plist-get handler :source-root)))
              (let ((process-environment (copy-sequence process-environment)))
                (setenv "MEVEDEL_REMOTE_HOOK_MARKER" marker)
                (setenv "MEVEDEL_REMOTE_HOOK_INPUT" input)
                (with-temp-buffer
                  (mevedel-hooks-run-event
                   'PreToolUse payload
                   (lambda (value)
                     (setq decision value done t))
                   session workspace nil invocation)
                  (test-mevedel-execution-remote--wait
                   (lambda () done) "Agent hook did not settle")))))
          (should-not (plist-get decision :permission-decision))
          (should (file-exists-p marker))
          (should (file-exists-p input))
          (with-temp-buffer
            (insert-file-contents input)
            (should (string-match-p
                     (regexp-quote
                      (mevedel-execution-target-native-path
                       (mevedel-session-execution-target session) root))
                     (buffer-string))))
          t)
      (when agent
        (setq mevedel-agent--registry
              (assoc-delete-all
               (mevedel-agent-name agent) mevedel-agent--registry)))
      (when (file-directory-p user-dir)
        (delete-directory user-dir t))))

(defun test-mevedel-execution-remote--exercise-publication-recovery
    (session buffer)
  "Retain a failed publication, retry it, and clear specialized recovery."
  (with-current-buffer buffer
    (goto-char (point-max))
    (insert "publication failure recovery\n")
    (set-buffer-modified-p t))
  (let (failed)
    (condition-case _err
        (cl-letf
            (((symbol-function
               'mevedel-session-publication--publish-critical-batches)
              (lambda (&rest _args)
                (error "Injected real remote publication failure"))))
          (mevedel-session-persistence-save session buffer))
      (error (setq failed t)))
    (should failed))
  (should (mevedel-session-pending-publication session))
  (should-error
   (mevedel-session-persistence-assert-mutation-authority session buffer)
   :type 'user-error)
  (should (mevedel-session-publication-retry session))
  (should-not (mevedel-session-pending-publication session))
  (let ((recovery-dir (make-temp-file "mevedel-remote-incomplete-" t)))
    (unwind-protect
        (progn
          (write-region "manual recovery evidence\n" nil
                        (file-name-concat recovery-dir "evidence.txt")
                        nil 'silent)
          (mevedel-session-recovery-record-failure
           session "acceptance incomplete rollback" recovery-dir)
          (should (plist-get (mevedel-session-pending-publication session)
                             :manual-recovery-marker))
          (should (mevedel-session-recovery-read
                   (mevedel-session-save-path session)))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (should (mevedel-session-publication-abandon session)))
          (should-not (mevedel-session-pending-publication session))
          (should-not (mevedel-session-recovery-read
                       (mevedel-session-save-path session))))
      (when (file-directory-p recovery-dir)
        (delete-directory recovery-dir t)))))


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
  :doc "bounds a timed-out child whose stop escalation never settles"
  (let* ((root (make-temp-file "mevedel-remote-wedged-" t))
         (pid-file (expand-file-name "group.pid" root))
         (remote-root
          (format "/mevedelmock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (mevedel-execution--child-kill-delay 0.02)
         (mevedel-execution--remote-control-timeout 0.02)
         (mevedel-sandbox-mode 'off)
         group-id)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          ;; A wedged transport can strand the TERM/KILL escalation; the
          ;; timed-out one-shot must still settle within its bounded
          ;; watchdog window instead of spinning forever.
          (cl-letf (((symbol-function 'mevedel-execution--start-stop)
                     #'ignore))
            (let ((result
                   (mevedel-execution-run-one-shot
                    :name "mevedel-test-remote-wedged"
                    :command
                    '("sh" "-c"
                      "ps -o pgid= -p $$ | tr -d ' ' > group.pid; sleep 30")
                    :workdir remote-root
                    :writable-roots (list remote-root)
                    :timeout 0.05)))
              (setq group-id
                    (string-to-number
                     (string-trim
                      (with-temp-buffer
                        (insert-file-contents pid-file)
                        (buffer-string)))))
              (should (plist-get result :timed-out-p))
              (should (eq 'unknown (plist-get result :termination))))))
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
            ;; This case extends `tramp-remote-path' so the target finds its
            ;; stand-in Bubblewrap.  A connection opened by an earlier test
            ;; already cached the old path, so drop it before probing.
            (tramp-cleanup-connection
             (tramp-dissect-file-name remote-root) nil t)
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
                  (delete-directory target-protected t))))))
      (delete-directory client-only t)
      (delete-directory external-root t)
      (delete-directory root t))))

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
          ;; This case classifies transport exit, not target fencing: the
          ;; mock method runs a local shell whose environment depends on
          ;; which caller opens it first, so keep the fence out of it.
          (let ((mevedel-session-persistence--checking-incarnation t))
            (cl-letf
                (((symbol-function 'mevedel-execution--remote-command)
                  (lambda (record _command)
                    (setf (mevedel-execution--record-group-marker record)
                          "missing-pgid-marker"
                          (mevedel-execution--record-group-marker-buffer
                           record)
                          "")
                    '("sh" "-c" "exit 0"))))
              (setq result
                    (test-mevedel-execution--start-managed
                     session remote-root '("sh" "-c" "true")
                     :yield-time-ms nil))))
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
         session result diagnostics)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (setq session (test-mevedel-execution--session remote-root))
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-publish-text)
                (lambda (&rest _)
                  (error "Publication failed"))))
            (mevedel-test--with-captured-diagnostics diagnostics
              (setq result
                    (test-mevedel-execution--start-managed
                     session remote-root
                     '("sh" "-c" "printf '%03000d' 0")
                     :yield-time-ms nil))))
          (should (string-match-p "Publication failed" diagnostics))
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
            ;; A live view registers the parent's root data buffer; the side
            ;; conversation's durable authority resolves through it.
            (require 'mevedel-session-control-transfer)
            (mevedel-session-control-transfer-register-root-buffer
             parent root-buffer)
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

(defun test-mevedel-execution-remote--exercise-project-hook
    (session root)
  "Run a trusted project command hook for SESSION on target ROOT."
  (let* ((hook-dir (file-name-concat root ".mevedel" "hooks"))
         (script (file-name-concat hook-dir "accept-read.sh"))
         (config (file-name-concat root ".mevedel" "hooks.json"))
         (marker (file-name-concat root ".mevedel" "hook-cwd"))
         (native-root
          (mevedel-execution-target-native-path
           (mevedel-session-execution-target session) root))
         (mevedel-hooks-require-project-trust nil))
    (make-directory hook-dir t)
    (write-region
     (concat "#!/bin/sh\n"
             "printf '%s' \"$PWD\" > .mevedel/hook-cwd\n"
             "printf '%s' '{\"system_message\":\"remote hook ran\"}'\n")
     nil script nil 'silent)
    (set-file-modes script #o755)
    (write-region
     (concat
      "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Read\","
      "\"hooks\":[{\"type\":\"command\","
      "\"command\":\".mevedel/hooks/accept-read.sh\","
      "\"timeout\":10,\"failClosed\":true}]}]}}")
     nil config nil 'silent)
    (should
     (string-match-p
      "acceptance needle"
      (test-mevedel-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (equal (directory-file-name native-root)
            (with-temp-buffer
              (insert-file-contents marker)
              (buffer-string))))))

(defun test-mevedel-execution-remote--exercise-publication-media
    (session buffer)
  "Publish SESSION and round-trip durable media through BUFFER."
  (require 'mevedel)
  (with-current-buffer buffer
    (insert "Remote publication transcript\n")
    (should (mevedel-session-persistence-save session buffer)))
  (let* ((save-path (mevedel-session-save-path session))
         (publication
          (mevedel-session-publication-read save-path))
         (tool-results (file-name-concat save-path "tool-results"))
         (media '((:mime "image/png" :kind image :data "QUJD")))
         (attached
          (mevedel-tool-media-attach-result
           "visible" media tool-results "toolu_remote" session)))
    (should publication)
    (should
     (cl-find (mevedel-session-session-id session)
              (mevedel-session-persistence-list-sessions
               (mevedel-session-workspace session))
              :test #'equal
              :key (lambda (entry)
                     (plist-get (plist-get entry :summary) :session-id))))
    (with-current-buffer buffer
      (should (mevedel-session-persistence-save session buffer t)))
    (clrhash mevedel-tool-media--store)
    (let ((restored
           (mevedel-tool-media-extract
            attached tool-results "toolu_remote" nil session)))
      (should (equal "visible" (car restored)))
      (should (equal media (cdr restored))))))

(defun test-mevedel-execution-remote--exercise-worktree-fork
    (session buffer root)
  "Create a target-native Worktree Fork and prove source isolation."
  (let* ((source-file (file-name-concat root "source-only.txt"))
         (child nil)
         (reservation nil)
         (target
          (list :fork-point-id "real-remote-fork"
                :worktree-reservation nil)))
    (write-region "source\n" nil source-file nil 'silent)
    (let ((default-directory root)
          (process-environment nil))
      (should (zerop (process-file "git" nil nil nil "config" "user.email"
                                   "mevedel-acceptance@example.invalid")))
      (should (zerop (process-file "git" nil nil nil "config" "user.name"
                                   "Mevedel Acceptance")))
      (should (zerop (process-file "git" nil nil nil "add" "source-only.txt")))
      (should (zerop (process-file "git" nil nil nil "commit" "-m"
                                   "acceptance baseline"))))
    ;; A settled save normally supplies this index.  Pin one stable fork point
    ;; here so this acceptance remains independent of provider transcript text.
    (setf (mevedel-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                  :fork-point-id "real-remote-fork"
                  :pos 1 :transcript-cutoff 1)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert
       (mevedel--format-hook-audit-record
        '(:type fork-point :fork-point-id "real-remote-fork"
          :segment 1 :turn 1 :file-turn 1 :cum-turn 1
          :captured-file-turn 1)))
      (mevedel-session-persistence-save session buffer))
    (require 'mevedel-worktree)
    (setq reservation (mevedel-worktree-fork-reservation session))
    (setq target (plist-put target :worktree-reservation reservation))
    (setq child (mevedel-session-persistence-worktree-fork buffer target))
    (should (buffer-live-p child))
    (let ((child-session (buffer-local-value 'mevedel--session child))
          (child-directory
           (plist-get reservation :directory))
          (child-native-directory
           (mevedel-execution-target-native-path
            (mevedel-session-execution-target
             (buffer-local-value 'mevedel--session child))
            (plist-get reservation :directory))))
      (should (file-directory-p child-directory))
      (should (stringp (mevedel-session-save-path child-session)))
      (should (mevedel-session-durability-lease-owned-p child-session))
      (should (file-exists-p source-file))
      (should-not (file-exists-p
                   (file-name-concat child-directory "child-only.txt")))
      (write-region "child\n" nil
                    (file-name-concat child-directory "child-only.txt")
                    nil 'silent)
      (should-not (file-exists-p
                   (file-name-concat root "child-only.txt")))
      (with-current-buffer child
        (set-buffer-modified-p nil))
      (ignore-errors
        (mevedel-session-durability-lease-release
         (mevedel-session-save-path child-session) child-session))
      (kill-buffer child)
      (let ((default-directory root))
        (should
          (zerop
          (process-file "git" nil nil nil "worktree" "remove" "--force"
                        child-native-directory)))
        (should-not (file-directory-p child-directory)))))
  t)

(defun test-mevedel-execution-remote--exercise-plan-worktree-entry
    (session root)
  "Validate Plan Worktree branch entry and target-side Git-unavailable failure."
  (let ((entry (list :session session
                     :selection '(:location worktree :context fresh))))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "worktree/accepted-plan")))
      (should (equal "worktree/accepted-plan"
                     (mevedel-plan-mode--read-worktree-branch entry)))))
  (let* ((fake-bin (file-name-concat root ".mevedel" "git-unavailable-bin"))
         (fake-git (file-name-concat fake-bin "git"))
         ;; `tramp-remote-path' names target-side directories, never
         ;; client-side TRAMP file names.
         (tramp-remote-path (cons (file-local-name fake-bin)
                                  tramp-remote-path)))
    (unwind-protect
        (progn
          (make-directory fake-bin t)
          (write-region
           "#!/bin/sh\nprintf 'git unavailable for acceptance\\n' >&2\nexit 127\n"
           nil fake-git nil 'silent)
          (set-file-modes fake-git #o755)
          ;; The live connection already cached the target's search path
          ;; and its resolved `git', so drop it before the fake one has to
          ;; win.
          (tramp-cleanup-connection
           (tramp-dissect-file-name root) nil t)
          (let ((err
                 (should-error
                  (mevedel-worktree-fork-preflight session)
                  :type 'user-error)))
            (should (string-match-p
                     "Git is required on the session execution target"
                     (error-message-string err)))))
      (when (file-directory-p fake-bin)
        (delete-directory fake-bin t)))))

(defun test-mevedel-execution-remote--exercise-cold-entry
    (session buffer workspace)
  "Save, discover, release, and cold-resume SESSION."
  (let ((session-id (mevedel-session-session-id session))
        (save-path (mevedel-session-save-path session)))
    (with-current-buffer buffer
      (mevedel-session-persistence-save session buffer t)
      (set-buffer-modified-p nil))
    (should
     (cl-find session-id
              (mevedel-session-persistence-list-sessions workspace)
              :key (lambda (item)
                     (plist-get (plist-get item :summary) :session-id))
              :test #'equal))
    (mevedel-session-durability-lease-release save-path session)
    (kill-buffer buffer)
    (mevedel-workspace-clear-registry)
    (let* ((fresh-workspace
            (mevedel-workspace-get-or-create
             'project (mevedel-workspace-id workspace)
             (mevedel-workspace-root workspace)
             (mevedel-workspace-name workspace)))
           (restored
            (mevedel-session-persistence-resume-id
             fresh-workspace session-id)))
      (should (buffer-live-p restored))
      (with-current-buffer restored
        (should (eq fresh-workspace mevedel--workspace))
        (should (equal session-id
                       (mevedel-session-session-id mevedel--session)))
        (should-not buffer-read-only))
      restored)))

(defun test-mevedel-execution-remote--exercise-ssh-aliases ()
  "Prove two SSH aliases discover one workspace and Save As children."
  (let ((root-a (getenv "MEVEDEL_TEST_SSH_ALIAS_A_ROOT"))
        (root-b (getenv "MEVEDEL_TEST_SSH_ALIAS_B_ROOT")))
    (unless (and root-a root-b)
      (ert-skip "SSH alias roots are not configured"))
    (when-let ((config (getenv "MEVEDEL_TEST_SSH_CONFIG")))
      (setq tramp-use-connection-share t
            tramp-ssh-controlmaster-options
            (format "-F %s" (shell-quote-argument config))))
    (let* ((root-a (file-name-as-directory root-a))
           (root-b (file-name-as-directory root-b))
           (directory-name
            (format "mevedel-alias-%s"
                    (substring
                     (md5 (format "%s-%s" (float-time) (random))) 0 12)))
           (dir-a (file-name-as-directory
                   (file-name-concat root-a directory-name)))
           (dir-b
            (file-name-as-directory
             (file-name-concat
              root-b directory-name)))
           (workspace-a (test-mevedel-execution--workspace dir-a))
           (workspace-b
            (mevedel-workspace-get-or-create
             'project dir-b dir-b "alias-b"))
           (session nil)
           (parent-id nil)
           (child-id nil)
           (buffer (generate-new-buffer " *remote-alias-save-as*")))
      (unwind-protect
          (progn
            (test-mevedel-execution-remote--stage "make directory")
            (make-directory dir-a t)
            (should (equal
                     (mevedel-workspace-identity-read dir-a)
                     (mevedel-workspace-identity-read dir-b)))
            (test-mevedel-execution-remote--stage "identity read complete")
            (setq session (mevedel-session-create "alias" workspace-a dir-a))
            (test-mevedel-execution-remote--stage "probe alias A")
            (let ((target (mevedel-session-execution-target session)))
              (should
               (eq 'ready
                   (plist-get
                    (mevedel-execution-target-probe target t 'off)
                    :status)))
              (puthash (mevedel-execution-target-identity target) t
                       mevedel-session-durability--disclosed-targets))
            (test-mevedel-execution-remote--stage "probe alias A complete")
            (require 'mevedel)
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--workspace workspace-a
                          mevedel--session session)
              (insert "Alias transcript\n")
              (mevedel-session-persistence-save session buffer)
              (set-buffer-modified-p nil))
            (test-mevedel-execution-remote--stage "save alias A complete")
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)
            (test-mevedel-execution-remote--stage "release alias A complete")
            (mevedel-workspace-clear-registry)
            (setq workspace-b
                  (mevedel-workspace-get-or-create
                   'project dir-b dir-b "alias-b"))
            (let* ((remote-file-name-inhibit-cache t)
                   (_ (test-mevedel-execution-remote--stage
                       "list alias B start"))
                   (listed
                    (mevedel-session-persistence-list-sessions workspace-b)))
              (test-mevedel-execution-remote--stage "list alias B complete: %S"
                                                     listed)
              (unless
                  (cl-find (mevedel-session-session-id session) listed
                           :key (lambda (item)
                                  (plist-get (plist-get item :summary)
                                             :session-id))
                           :test #'equal)
                (let* ((sessions-dir
                        (mevedel-session-persistence--sessions-dir
                         workspace-b))
                       (entries
                        (directory-files
                         sessions-dir t
                         "\\`\\(?:[^.]\\|\\.mevedel-save-as-\\)"))
                       (diagnostics
                        (mapcar
                         (lambda (entry)
                           (condition-case err
                               (let* ((publication
                                       (mevedel-session-publication-read
                                        entry))
                                      (sidecar
                                       (plist-get publication :sidecar))
                                      (summary
                                       (and sidecar
                                            (mevedel-session-persistence--read-summary
                                             sidecar))))
                                 (list entry publication sidecar summary))
                             (error
                              (list entry :error
                                    (error-message-string err)))))
                         entries)))
                  (ert-fail
                   (format "Alias-B listing empty: sessions-dir=%S entries=%S diagnostics=%S"
                           sessions-dir entries diagnostics)))))
            (let ((mevedel-session-durability--disclosed-targets
                   mevedel-session-durability--disclosed-targets))
              (test-mevedel-execution-remote--stage
               "disclose alias B target start")
              (puthash
               (mevedel-execution-target-identity
                (mevedel-execution-target-create dir-b)) t
               mevedel-session-durability--disclosed-targets))
            (test-mevedel-execution-remote--stage
             "disclose alias B target complete")
            (with-current-buffer buffer
              (setq-local mevedel--workspace workspace-a)
              (cl-letf (((symbol-function 'read-string)
                         (lambda (&rest _) "alias-child")))
                (test-mevedel-execution-remote--stage "save-as start")
                (setq parent-id (mevedel-session-session-id session))
                (mevedel-save-session t)))
            (setq child-id (mevedel-session-session-id session))
            (should (not (equal parent-id child-id)))
            (test-mevedel-execution-remote--stage "save-as complete")
            (let ((remote-file-name-inhibit-cache t))
              (test-mevedel-execution-remote--stage "list children start")
              (should (= 2 (length
                            (mevedel-session-persistence-list-sessions
                             workspace-b)))))
            (test-mevedel-execution-remote--stage "list children complete")
            ;; Reopen the Save As child through the other SSH alias.  Killing
            ;; the current child first ensures this is a cold, independent
            ;; restore rather than a live-buffer lookup.
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)
            (setq buffer nil)
            (mevedel-workspace-clear-registry)
            (let* ((workspace-b
                    (mevedel-workspace-get-or-create
                     'project dir-b dir-b "alias-b"))
                   (resumed
                    (mevedel-session-persistence-resume-id
                     workspace-b child-id)))
              (should (buffer-live-p resumed))
              (with-current-buffer resumed
                (should (equal child-id
                               (mevedel-session-session-id mevedel--session)))
                (should (file-remote-p default-directory))
                (should
                 (equal
                  (mevedel-execution-target-identity
                   (mevedel-session-execution-target mevedel--session))
                  (mevedel-execution-target-identity
                   (mevedel-execution-target-create dir-b)))))
              (let ((resumed-session
                     (buffer-local-value 'mevedel--session resumed)))
                (mevedel-session-durability-lease-release
                 (mevedel-session-save-path resumed-session) resumed-session))
              (with-current-buffer resumed (set-buffer-modified-p nil))
              (kill-buffer resumed)))
        (when (and session (mevedel-session-save-path session))
          (ignore-errors
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (ignore-errors
          (when (file-directory-p dir-a)
            (delete-directory dir-a t)))))))

(defun test-mevedel-execution-remote--run-transfer-clients
    (root session-id &optional scenario)
  "Run independent owner and requester Emacsen against ROOT SESSION-ID.
SCENARIO is `transfer', `crash-long', or `recovery'."
  (let* ((client-file
          (file-name-concat
           test-mevedel-execution-remote--directory
           "test-mevedel-execution-remote-client.el"))
         (common (list "npx" "@emacs-eask/cli" "test" "ert" client-file))
         (owner-log (generate-new-buffer " *mevedel-remote-owner-log*"))
         (requester-log
          (generate-new-buffer " *mevedel-remote-requester-log*"))
         owner requester)
    (cl-labels
        ((log (buffer)
           (with-current-buffer buffer (buffer-string)))
         (logs ()
           (format "OWNER:\n%s\nREQUESTER:\n%s"
                   (log owner-log) (log requester-log)))
         (wait-stage (process buffer stage description)
           (with-timeout
               (90
                (ert-fail
                 (format "%s\n%s" description (logs))))
             (while
                 (and (process-live-p process)
                      (with-current-buffer buffer
                        (not (string-match-p (regexp-quote stage)
                                             (buffer-string)))))
               (accept-process-output process 0.1)))
           (unless (with-current-buffer buffer
                     (string-match-p (regexp-quote stage)
                                     (buffer-string)))
             (ert-fail
              (format "%s exited before stage %s\n%s"
                      description stage (logs)))))
         (start-client (role id name buffer)
           (let ((process-environment (copy-sequence process-environment)))
             (setenv "MEVEDEL_REMOTE_CLIENT_ROLE" role)
             (setenv "MEVEDEL_REMOTE_CLIENT_ID" id)
             (setenv "MEVEDEL_REMOTE_CLIENT_ROOT" root)
             (setenv "MEVEDEL_REMOTE_SESSION_ID" session-id)
             (setenv "MEVEDEL_REMOTE_CLIENT_SCENARIO"
                     (or scenario "transfer"))
             (apply #'start-process name buffer common)))
         (wait-exit (process description &optional failure-expected-p)
           (with-timeout
               (120
                (ert-fail (format "%s\n%s" description (logs))))
             (while (process-live-p process)
               (accept-process-output process 0.1)))
           (unless (or failure-expected-p
                       (zerop (process-exit-status process)))
             (ert-fail (format "%s\n%s" description (logs))))))
      (unwind-protect
          (progn
            (setq owner
                  (start-client
                   "owner"
                   "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                   "mevedel-remote-owner" owner-log))
            (wait-stage owner owner-log "owner-marker-a-owner"
                        "Owner client did not establish its lease")
            (setq requester
                  (start-client
                   "requester"
                   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                   "mevedel-remote-requester" requester-log))
            (wait-stage requester requester-log "requester-session-start"
                        "Requester client did not start")
            (wait-exit owner "Owner client failed"
                       (member scenario '("crash-long" "recovery")))
            (wait-exit requester "Requester client failed"))
        (dolist (process (list owner requester))
          (when (process-live-p process)
            (delete-process process)))
        (when (buffer-live-p owner-log)
          (kill-buffer owner-log))
        (when (buffer-live-p requester-log)
          (kill-buffer requester-log))))))

(defun test-mevedel-execution-remote--exercise-transfer-only
    (variable method)
  "Exercise only independent control transfer for VARIABLE and METHOD."
  (require 'mevedel)
  (let* ((base (test-mevedel-execution-remote--real-root variable method))
         (root (test-mevedel-execution-remote--real-temp-directory
                base (format "mevedel-%s-transfer-" method)))
         (workspace (test-mevedel-execution--workspace root))
         (session (mevedel-session-create "transfer" workspace root))
         (buffer (generate-new-buffer " *remote-transfer-only*")))
    (unwind-protect
        (let ((mevedel-sandbox-mode 'off)
              (mevedel-session-lease-seconds 600))
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--workspace workspace
                        mevedel--session session)
            (setq default-directory root)
            (mevedel-session-set-root-buffer session buffer)
            (test-mevedel-execution-remote--accept-storage session)
            (should
             (eq 'ready
                 (plist-get
                  (mevedel-execution-target-probe
                   (mevedel-session-execution-target session) t 'off)
                  :status)))
            (insert "Transfer-only transcript\n")
            (should (mevedel-session-persistence-save session buffer t))
            (set-buffer-modified-p nil))
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)
          (test-mevedel-execution-remote--run-transfer-clients
           root (mevedel-session-session-id session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors
        (when (mevedel-session-durability-lease-owned-p session)
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)))
      (when (file-exists-p root)
        (delete-directory root t)))))

(defun test-mevedel-execution-remote--exercise-transfer-recovery
    (variable method)
  "Exercise crashed-client takeover and target-side recovery with METHOD."
  (let* ((base (test-mevedel-execution-remote--real-root variable method))
         (root (test-mevedel-execution-remote--real-temp-directory
                base (format "mevedel-%s-client-recovery-" method)))
         (workspace (test-mevedel-execution--workspace root))
         (session (mevedel-session-create "recovery" workspace root))
         (buffer (generate-new-buffer " *remote-client-recovery*")))
    (unwind-protect
        (let ((mevedel-sandbox-mode 'off)
              (mevedel-session-lease-seconds 600))
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--workspace workspace
                        mevedel--session session
                        default-directory root)
            (mevedel-session-set-root-buffer session buffer)
            (test-mevedel-execution-remote--accept-storage session)
            (should
             (eq 'ready
                 (plist-get
                  (mevedel-execution-target-probe
                   (mevedel-session-execution-target session) t 'off)
                  :status)))
            (insert "Independent client recovery transcript\n")
            (mevedel-session-persistence-save session buffer t)
            (set-buffer-modified-p nil))
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)
          (test-mevedel-execution-remote--run-transfer-clients
           root (mevedel-session-session-id session) "crash-long")
          (test-mevedel-execution-remote--run-transfer-clients
           root (mevedel-session-session-id session) "recovery"))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors
        (when (mevedel-session-durability-lease-owned-p session)
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)))
      (when (file-exists-p root)
        (delete-directory root t)))))

(mevedel-deftest mevedel-real-remote-alias-only
  (:tags (external remote aliases))
  ,test
  (test)
  :doc "runs only the two-alias publication and Save As acceptance"
  (if (getenv "MEVEDEL_TEST_REMOTE_ALIASES_ONLY")
    (test-mevedel-execution-remote--exercise-ssh-aliases)
    (ert-skip "Alias-only acceptance is not enabled")))

(mevedel-deftest mevedel-real-remote-transfer-only
  (:tags (external remote control-transfer))
  ,test
  (test)
  :doc "runs only the independent SSH control-transfer acceptance"
  (test-mevedel-execution-remote--exercise-transfer-only
   "MEVEDEL_TEST_SSH_ROOT" 'ssh))

(mevedel-deftest mevedel-real-remote-client-recovery-only
  (:tags (external remote control-transfer recovery))
  ,test
  (test)
  :doc "runs crashed-client takeover and second-client recovery on SSH"
  (test-mevedel-execution-remote--exercise-transfer-recovery
   "MEVEDEL_TEST_SSH_ROOT" 'ssh))

(defun test-mevedel-execution-remote--exercise-volume-replacement
    (root session buffer workspace method)
  "Replace the Docker-compatible or Podman target over its persistent volume."
  (let* ((runtime (if (eq method 'podman) "podman" "docker"))
         (volume (getenv (if (eq method 'podman)
                             "MEVEDEL_TEST_PODMAN_VOLUME"
                           "MEVEDEL_TEST_DOCKER_VOLUME")))
         (image (getenv (if (eq method 'podman)
                            "MEVEDEL_TEST_PODMAN_IMAGE"
                          "MEVEDEL_TEST_DOCKER_IMAGE")))
        (container (file-remote-p root 'host 'never)))
    (unless (and volume image container)
      (ert-skip
       (format "%s persistent-volume fixture is not configured" method)))
    (with-current-buffer buffer
      (mevedel-session-persistence-save session buffer t)
      (set-buffer-modified-p nil))
    (let* ((old-incarnation
            (mevedel-execution-target-incarnation
             (mevedel-session-execution-target session)))
           (grant (file-name-concat root "persisted-grant.txt")))
      (write-region "grant\n" nil grant nil 'silent)
      (mevedel-permission-add-session-resource-grant session grant 'read)
      (with-current-buffer buffer
        (mevedel-session-persistence-save session buffer t)
        (set-buffer-modified-p nil))
      (mevedel-session-durability-lease-release
       (mevedel-session-save-path session) session)
      (kill-buffer buffer)
      (let ((status (process-file runtime nil nil nil "rm" "--force"
                                  container)))
        (should (zerop status)))
      (should
       (zerop
        (process-file runtime nil nil nil "run" "--detach"
                      "--name" container "--volume"
                      (format "%s:/workspace" volume) image)))
      (tramp-cleanup-connection (tramp-dissect-file-name root) nil t)
      (mevedel-workspace-clear-registry)
      (let* ((fresh-workspace
              (mevedel-workspace-get-or-create
               'project (mevedel-workspace-id workspace)
               (mevedel-workspace-root workspace)
               (mevedel-workspace-name workspace)))
             (restored
              (mevedel-session-persistence-resume-id
               fresh-workspace (mevedel-session-session-id session)))
             (new-session (buffer-local-value 'mevedel--session restored))
             (new-target (mevedel-session-execution-target new-session)))
        (should (buffer-live-p restored))
        ;; Resume with an acquired lease settles the replacement as one
        ;; transaction: it probes the fresh target, revokes exact
        ;; target-scoped grants, commits a sidecar naming the new
        ;; incarnation, and acknowledges the observation.  The restored
        ;; session therefore carries the new incarnation with no pending
        ;; replacement and no surviving grants.
        (should-not
         (mevedel-execution-target-incarnation-changed-p new-target))
        (should-not (equal old-incarnation
                           (mevedel-execution-target-incarnation new-target)))
        (should-not (mevedel-session-resource-grants new-session))
        ;; A fresh probe of the same replacement target observes no
        ;; further change.
        (should (eq 'ready
                    (plist-get
                     (mevedel-execution-target-probe new-target t 'off)
                     :status)))
        (should-not
         (mevedel-execution-target-incarnation-changed-p new-target))
        ;; The committed sidecar records the new incarnation, so a later
        ;; client resumes against the acknowledged replacement.
        (let* ((publication
                (mevedel-session-publication-read
                 (mevedel-session-save-path new-session)))
               (sidecar
                (mevedel-session-persistence-load-sidecar
                 (plist-get publication :sidecar))))
          (should (equal (mevedel-execution-target-incarnation new-target)
                         (plist-get sidecar :target-incarnation))))
        (with-current-buffer restored
          (should (mevedel-request-p (mevedel-request-begin new-session)))
          (mevedel-request-end t)
          (set-buffer-modified-p nil))
        (cons restored new-session)))))

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

(defmacro test-mevedel-execution-remote--with-lost-connection (root &rest body)
  "Run BODY while ROOT's client-side transport is lost and cannot reconnect.

The target keeps running: only this client's connection is discarded, and
reconnection is refused for the duration, which is what makes the outcome of
work in flight genuinely unprovable rather than merely finished."
  (declare (indent 1) (debug (form body)))
  `(let ((vec (tramp-dissect-file-name ,root)))
     (tramp-cleanup-connection vec nil t)
     (cl-letf (((symbol-function 'tramp-maybe-open-connection)
                (lambda (&rest _)
                  (signal 'file-error
                          (list "Connection lost for acceptance")))))
       ,@body)
     (tramp-cleanup-connection vec nil t)))

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
         session target root-buffer group-id child-pid
         child-start-time)
    (unwind-protect
        (let* ((mevedel-sandbox-mode 'off)
               ;; Real TRAMP setup can synchronously block timer dispatch for
               ;; longer than the production lease window.  Keep this one
               ;; sequential acceptance owner alive across unrelated methods.
               (mevedel-session-lease-seconds 600)
               (workspace (test-mevedel-execution--workspace root))
               (_ (setq session
                        (mevedel-session-create "main" workspace root)))
               (_ (setf (mevedel-session-sandbox-mode session) 'off))
               (_ (setq target
                        (mevedel-session-execution-target session)))
               (readiness
                (mevedel-execution-target-probe target t 'off))
               initial execution-id result)
          (setq root-buffer (generate-new-buffer " *remote-loss*"))
          (with-current-buffer root-buffer
            (setq-local mevedel--workspace workspace
                        mevedel--session session)
            (setq default-directory root))
          ;; Portable mutation admission needs the session's registered
          ;; root data buffer before anything can materialize it.
          (mevedel-session-set-root-buffer session root-buffer)
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
            :owner "/root"
            :yield-time-ms 250)
           execution-id
           (plist-get (plist-get initial :facts) :execution-id))
          (should-not (plist-get initial :error))
          (should (stringp execution-id))
          (unless (string-match-p "ready" (plist-get initial :output))
            (setq initial
                  (test-mevedel-execution--observe
                   session execution-id :owner "/root" :wait-ms 3000)))
          (should (string-match-p "ready" (plist-get initial :output)))
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
          (test-mevedel-execution-remote--with-lost-connection root
            (mevedel-execution-stop
             session "/root" execution-id
             (lambda (value) (setq result value)))
            (test-mevedel-execution-remote--wait
             (lambda () result)
             "Connection-loss execution did not settle" 30))
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
          (when (mevedel-session-pending-publication session)
            (should
             (mevedel-session-publication-retry session)))
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

      (when (and group-id child-pid child-start-time)
        (ignore-errors
          (test-mevedel-execution-remote--cleanup-target-group
           root group-id child-pid child-start-time)))
      (when session
        (mevedel-execution-teardown-session session)
        ;; Mutation admission materialized the session and acquired its
        ;; lease; releasing it also cancels the renewal timer that would
        ;; otherwise keep running target I/O into later journeys.
        (ignore-errors
          (when (and (mevedel-session-save-path session)
                     (mevedel-session-durability-lease-owned-p session))
            (mevedel-session-durability-lease-release
             (mevedel-session-save-path session) session))))
      (when (buffer-live-p root-buffer)
        (with-current-buffer root-buffer
          (set-buffer-modified-p nil))
        (kill-buffer root-buffer))
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
    (should-not (plist-get initial :error))
    (setq execution-id
          (plist-get (plist-get initial :facts) :execution-id))
    (should (stringp execution-id))
    (unless (string-match-p "ready" (plist-get initial :output))
      (setq initial
            (test-mevedel-execution--observe
             session execution-id :wait-ms 3000)))
    (should (string-match-p "ready" (plist-get initial :output)))
    (should (eq 'running (plist-get (plist-get initial :facts) :state)))
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
          ;; The transport journey replaces its target over the persistent
          ;; project volume, so its workspace has to live there.
          (test-mevedel-execution-remote--real-temp-directory
           base (format "mevedel-%s-acceptance-" method) t))
         session root-buffer)
    (unwind-protect
        (let* ((mevedel-sandbox-mode 'off)
               (workspace (test-mevedel-execution--workspace root))
               (_ (setq session (mevedel-session-create "main" workspace root)))
               (target (mevedel-session-execution-target session)))
          (setq root-buffer (generate-new-buffer " *remote-acceptance*"))
          (with-current-buffer root-buffer
            (org-mode)
            (setq-local mevedel--workspace workspace
                        mevedel--session session)
            (setq default-directory root))
          ;; Portable mutation admission needs the session's registered
          ;; root data buffer before anything can materialize it.
          (mevedel-session-set-root-buffer session root-buffer)
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
          (when (eq method 'ssh)
            (message "mevedel: real remote SSH ordinary entry")
            (test-mevedel-execution-remote--exercise-ordinary-entry
             root workspace))
          (test-mevedel-execution-remote--exercise-reconnect target)
          (message "mevedel: real remote %s file tools" method)
          (let ((file
                 (test-mevedel-execution-remote--exercise-file-tools
                  session target root)))
            (message "mevedel: real remote %s Bash/Eval/resources/skills" method)
            (test-mevedel-execution-remote--exercise-tools-and-resources
             session root root-buffer)
            (test-mevedel-execution-remote--exercise-project-hook
             session root)
            (message "mevedel: real remote %s hook provenance" method)
            (test-mevedel-execution-remote--exercise-hook-provenance
             session workspace root)
            (message "mevedel: real remote %s control filesystem" method)
          (test-mevedel-execution-remote--exercise-control-filesystem root)
          (message "mevedel: real remote %s publication/media" method)
            (test-mevedel-execution-remote--exercise-publication-media
             session root-buffer)
            (message "mevedel: real remote %s publication/recovery" method)
            (test-mevedel-execution-remote--exercise-publication-recovery
             session root-buffer)
            (when (memq method '(ssh docker podman))
              (message "mevedel: real remote %s worktree fork" method)
              (test-mevedel-execution-remote--exercise-worktree-fork
               session root-buffer root)
              (when (eq method 'ssh)
                (message "mevedel: real remote SSH Plan Worktree/Git gap")
                (test-mevedel-execution-remote--exercise-plan-worktree-entry
                 session root)))
            (message "mevedel: real remote %s process/pty/concurrency" method)
            (test-mevedel-execution-remote--exercise-term-kill root)
            (test-mevedel-execution-remote--exercise-pty session root)
            (test-mevedel-execution-remote--exercise-concurrency
             session root file)
            (when (eq method 'ssh)
              (message "mevedel: real remote SSH aliases" )
              (test-mevedel-execution-remote--exercise-ssh-aliases))
            (when (memq method '(ssh docker podman))
              (message "mevedel: real remote %s transfer clients" method)
              (mevedel-session-durability-lease-release
               (mevedel-session-save-path session) session)
              (test-mevedel-execution-remote--run-transfer-clients
               root (mevedel-session-session-id session)))
            (when (memq method '(docker podman))
              (message "mevedel: real remote %s volume replacement" method)
                (pcase-let ((`(,new-buffer . ,new-session)
                           (test-mevedel-execution-remote--exercise-volume-replacement
                            root session root-buffer workspace method)))
                (setq root-buffer new-buffer
                      session new-session)))
            (setq root-buffer
                  (progn
                    (message "mevedel: real remote %s cold entry" method)
                    (test-mevedel-execution-remote--exercise-cold-entry
                     session root-buffer workspace)))))
      ;; Replacement and cold entry rebind the buffer to fresh session
      ;; objects.  Each holds its own lease and renewal timer, so the live
      ;; one must be released here or its timer keeps running target I/O
      ;; into every later journey in this Emacs process.
      (let ((live-session
             (and (buffer-live-p root-buffer)
                  (buffer-local-value 'mevedel--session root-buffer))))
        (dolist (entry (delete-dups (delq nil (list session live-session))))
          (mevedel-execution-teardown-session entry)
          (ignore-errors
            (when (and (mevedel-session-save-path entry)
                       (mevedel-session-durability-lease-owned-p entry))
              (mevedel-session-durability-lease-release
               (mevedel-session-save-path entry) entry)))))
      (when (buffer-live-p root-buffer)
        (with-current-buffer root-buffer
          (set-buffer-modified-p nil))
        (kill-buffer root-buffer))
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

(mevedel-deftest mevedel-real-remote-acceptance
  (:tags (external remote))
  ,test
  (test)
  :doc "exercises the core opt-in real SSH transport matrix"
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_SSH_ROOT" 'ssh)
  :doc "exercises the core opt-in real Docker transport matrix"
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker)
  :doc "exercises the core opt-in real Podman transport matrix"
  (test-mevedel-execution-remote--exercise-transport
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(mevedel-deftest mevedel-real-remote-loss
  (:tags (external remote connection-loss))
  ,test
  (test)
  :doc "classifies unprovable real SSH loss and cleans its descendant"
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_SSH_ROOT" 'ssh)
  :doc "classifies unprovable real Docker loss and cleans its descendant"
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker)
  :doc "classifies unprovable real Podman loss and cleans its descendant"
  (test-mevedel-execution-remote--exercise-connection-loss
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(mevedel-deftest mevedel-real-remote-bwrap
  (:tags (external remote sandbox))
  ,test
  (test)
  :doc "confines an exact symlink grant through real SSH Bubblewrap"
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_SSH_ROOT" 'ssh)
  :doc "confines an exact symlink grant through real Docker Bubblewrap"
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_DOCKER_ROOT" 'docker)
  :doc "confines an exact symlink grant through real Podman Bubblewrap"
  (test-mevedel-execution-remote--exercise-bwrap
   "MEVEDEL_TEST_PODMAN_ROOT" 'podman))

(provide 'test-mevedel-execution-remote)

;;; test-mevedel-execution-remote.el ends here
