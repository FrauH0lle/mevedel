;;; test-niwa-execution-remote.el --- Remote managed execution tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests target-aware process dispatch and remote process-group authority.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa-agent-control)
(require 'niwa-agent-exec)
(require 'niwa-agents)
(require 'niwa-chat)
(require 'niwa-diff-apply)
(require 'niwa-execution)
(require 'niwa-execution-process)
(require 'niwa-execution-target)
(require 'niwa-file-state)
(require 'niwa-hooks)
(require 'niwa-overlays)
(require 'niwa-permissions)
(require 'niwa-permissions-list)
(require 'niwa-pipeline)
(require 'niwa-sandbox)
(require 'niwa-skills-core)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-persistence)
(require 'niwa-session-recovery)
(require 'niwa-telemetry)
(require 'niwa-tool-media)
(require 'niwa-tool-registry)
(require 'niwa-tools)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'niwa-worktree)
(require 'niwa-plan-mode)
(require 'niwa)
(require 'tramp)
(require 'tramp-container)
(require 'tramp-sh)

(declare-function niwa-version "niwa-utilities" (&optional here message))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))

(defvar tramp-ssh-controlmaster-options)
(defvar tramp-use-connection-share)

(defconst test-niwa-execution-remote--directory
  (or (and load-file-name (file-name-directory load-file-name))
      (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory)
  "Directory containing the real remote acceptance tests.")


;;
;;; Test support

(defun test-niwa-execution-remote--stage (format-string &rest args)
  "Log a bounded remote acceptance stage when diagnostics are enabled."
  (when (getenv "NIWA_TEST_REMOTE_DIAGNOSTICS")
    (apply #'message (concat "niwa: remote diagnostic: " format-string)
           args)))

(niwa-deftest test-niwa-execution-remote--real-root ()
  ,test
  (test)
  :doc "rejects raw multi-hop roots before any target filesystem probe"
  (let ((process-environment (copy-sequence process-environment))
        probes)
    (setenv "NIWA_TEST_SSH_CONFIG" nil)
    (cl-letf (((symbol-function 'file-directory-p)
               (lambda (root) (push (list 'directory root) probes) t))
              ((symbol-function 'file-writable-p)
               (lambda (root) (push (list 'writable root) probes) t)))
      (dolist (method '(ssh podman docker))
        (let ((root (format "/%s:fixture:/workspace/" method)))
          ;; Establish that both fake probes are reached for a valid root.
          (setq probes nil)
          (setenv "NIWA_TEST_NO_HOP_ROOT" root)
          (should (equal root (test-niwa-execution-remote--real-root
                               "NIWA_TEST_NO_HOP_ROOT" method)))
          (should (equal probes `((writable ,root) (directory ,root))))
          (setq probes nil)
          (setenv "NIWA_TEST_NO_HOP_ROOT"
                  (format "/ssh:hop|%s:fixture:/workspace/" method))
          (should-error (test-niwa-execution-remote--real-root
                         "NIWA_TEST_NO_HOP_ROOT" method)
                        :type 'ert-test-failed)
          (should-not probes))))))

(niwa-deftest test-niwa-execution-remote--real-temp-directory ()
  (dolist (persistent '(nil t))
    (let* ((base "/ssh:fixture:/workspace/")
           (parent (if persistent base "/ssh:fixture:/tmp/"))
           (allocated (file-name-concat parent "owned-123/"))
           (niwa-test--write-roots (list default-directory)))
      (cl-letf (((symbol-function 'temporary-file-directory)
                 (lambda () "/ssh:fixture:/tmp/"))
                ((symbol-function 'make-nearby-temp-file)
                 (lambda (prefix directory-p)
                   (should directory-p)
                   (should (equal (if persistent
                                      (file-name-concat base "owned-")
                                    "owned-") prefix))
                   (should (member parent niwa-test--write-roots))
                   allocated)))
        (should (equal allocated
                       (test-niwa-execution-remote--real-temp-directory
                        base "owned-" persistent))))
      (should (member allocated niwa-test--write-roots))
      (should-not (member parent niwa-test--write-roots))
      (should-not (niwa-test--check-write
                   'write-region (file-name-concat allocated "result")))
      (let ((niwa-test--write-violations nil))
        (should-error (niwa-test--check-write
                       'write-region (file-name-concat parent "sibling")))))))

(niwa-deftest test-niwa-execution-remote--with-temporary-directory ()
  (cl-letf (((symbol-function 'temporary-file-directory)
             (lambda () "/native-temporary/"))
            ((symbol-function 'tramp-get-remote-tmpdir)
             (lambda (_vec) "/native-remote-temporary/")))
    (let ((default-directory "/ssh:fixture:/workspace/"))
      (test-niwa-execution-remote--with-temporary-directory
          "/ssh:fixture:/owned/"
        (should (equal "/ssh:fixture:/owned/" (temporary-file-directory)))
        (should (equal "/ssh:fixture:/owned/"
                       (tramp-get-remote-tmpdir
                        (tramp-dissect-file-name default-directory))))
        (test-niwa-execution-remote--with-temporary-directory
            "/ssh:other:/owned/"
          (should (equal "/ssh:fixture:/owned/" (temporary-file-directory)))
          (should (equal "/ssh:fixture:/owned/"
                         (tramp-get-remote-tmpdir
                          (tramp-dissect-file-name default-directory))))
          (let ((default-directory "/ssh:other:/workspace/"))
            (should (equal "/ssh:other:/owned/" (temporary-file-directory)))
            (should (equal "/ssh:other:/owned/"
                           (tramp-get-remote-tmpdir
                            (tramp-dissect-file-name default-directory))))))
        (let ((default-directory "/ssh:other:/workspace/"))
          (should (equal "/native-temporary/" (temporary-file-directory)))
          (should (equal "/native-remote-temporary/"
                         (tramp-get-remote-tmpdir
                          (tramp-dissect-file-name default-directory))))))
      (should (equal "/native-temporary/" (temporary-file-directory))))))

(defun test-niwa-execution-remote-run-selector ()
  "Run the exact generated ERT journey named by the environment.

`NIWA_TEST_REMOTE_TEST' must be a generated test symbol such as
`niwa-real-remote-acceptance/test' or
`niwa-real-remote-acceptance/test@1'.  Reading it inside Emacs keeps the
shell launcher independent of Lisp source interpolation."
  (let ((name (getenv "NIWA_TEST_REMOTE_TEST")))
    (unless (and name (not (string-empty-p name)))
      (error "NIWA_TEST_REMOTE_TEST is not set"))
    (ert-run-tests-batch-and-exit (intern name))))

(defun test-niwa-execution-remote--target-process-gone-p (root pid)
  "Return non-nil when target ROOT has no process named by PID."
  (let ((default-directory root)
        (process-environment nil))
    (not
     (zerop
      (process-file
       "bash" nil nil nil "-c" "kill -0 \"$1\" 2>/dev/null"
       "niwa-process-check" (number-to-string pid))))))

(defun test-niwa-execution-remote--wait
    (predicate description &optional timeout)
  "Wait for PREDICATE, failing after TIMEOUT seconds with DESCRIPTION."
  (with-timeout ((or timeout 30) (ert-fail description))
    (while (not (funcall predicate))
      (accept-process-output nil 0.05))))

(defun test-niwa-execution-remote--run-tool (session name args &optional buffer)
  "Run registered tool NAME with ARGS in SESSION and return its result.
Optional BUFFER retains the real caller's conversation and agent identity."
  (if (not buffer)
      (with-temp-buffer
        (setq default-directory (niwa-session-working-directory session))
        (setq-local niwa--workspace (niwa-session-workspace session)
                    niwa--session session)
        (test-niwa-execution-remote--run-tool
         session name args (current-buffer)))
    (let (done result)
      (with-current-buffer buffer
        (let ((niwa-permission-rules nil)
              (niwa-protected-paths nil))
          (niwa-pipeline-run-tool
           (niwa-tool-ensure name)
           (lambda (value)
             (setq result value done t))
           args)
          (test-niwa-execution-remote--wait
           (lambda () done) (format "%s did not settle" name) 60)))
      result)))

(defun test-niwa-execution-remote--start-reader
    (session root owner label &optional owner-context)
  "Start a yielded SESSION read-only command for OWNER at ROOT labelled LABEL.
Optional OWNER-CONTEXT identifies the retained agent invocation."
  (let (result)
    (niwa-execution-start-bash
     (lambda (value) (setq result value))
     :session session :owner owner :owner-context owner-context
     :command
     (list "bash" "-c" "printf '%s-ready\\n' \"$1\"; sleep 30"
           "niwa-acceptance-reader" label)
     :workdir root :writable-roots (list root)
     :read-only-p t :yield-time-ms 100
     :tool-args (list :command (format "acceptance reader %s" label)))
    (test-niwa-execution-remote--wait
     (lambda () result) (format "%s reader did not yield" label))
    result))

(defun test-niwa-execution-remote--exercise-ordinary-entry
    (root workspace)
  "Enter a fresh remote session through the ordinary `niwa' command.

The entry path is transport-neutral, while the target probe and session setup
are real for ROOT.  The temporary chat buffer and its view are cleaned up
before returning."
  (let ((context (generate-new-buffer " *remote-ordinary-entry-context*"))
        chat-buffer)
    (unwind-protect
        (progn
          (puthash
           (niwa-execution-target-identity
            (niwa-execution-target-create root))
           t niwa-session-durability--disclosed-targets)
          (with-current-buffer context
            (setq default-directory root
                  niwa--workspace workspace)
            (cl-letf (((symbol-function
                        'niwa-session-persistence-choose-entry)
                       (lambda (_workspace) 'new))
                      ((symbol-function 'completing-read)
                       (lambda (&rest _) "ordinary-entry"))
                      ((symbol-function 'niwa--display-chat-buffer)
                       (lambda (buffer) (setq chat-buffer buffer))))
              (niwa)))
          (should (buffer-live-p chat-buffer))
          (with-current-buffer chat-buffer
            (should (equal "ordinary-entry"
                           (niwa-session-name niwa--session)))
            (should
             (eq 'ready
                 (plist-get
                  (niwa-execution-target-probe
                   (niwa-session-execution-target niwa--session)
                   nil 'off)
                  :status))))
          t)
      (when (buffer-live-p chat-buffer)
        (let ((view-buffer
               (buffer-local-value 'niwa--view-buffer chat-buffer)))
          (with-current-buffer chat-buffer
            (set-buffer-modified-p nil))
          (kill-buffer chat-buffer)
          (when (buffer-live-p view-buffer)
            (kill-buffer view-buffer))))
      (when (buffer-live-p context)
        (kill-buffer context)))))

(defun test-niwa-execution-remote--exercise-tools-and-resources
    (session root buffer)
  "Exercise Bash, live Eval, an exact resource grant, and a target skill."
  (let* ((resource (file-name-concat root "explicit-resource.txt"))
         (skill-dir (file-name-concat root ".niwa" "skills"
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
      (niwa-permission-add-session-resource-grant session resource 'read))
    (should
     (string-match-p
      "resource needle"
      (test-niwa-execution-remote--run-tool
       session "Read" (list :file_path resource))))
    ;; A target shell reads target-native paths; a client TRAMP spelling is
    ;; not a path there.  The echoed command also contains the marker text,
    ;; so the file itself is the evidence that the command ran.
    (let ((bash-result
           (test-niwa-execution-remote--run-tool
            session "Bash"
            (list :description "Verify remote target file"
                  :command
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
    ;; The target login shell is irrelevant: Bash always launches explicit
    ;; `bash' argv, including the final status snapshot, on every transport.
    (dolist (case '(("false | cat" "0" "failure" "1 0")
                    ("set -- sample; (printf '%s\\n' \"$@\"; exit 7) | tail -1"
                     "0" "failure" "7 0")
                    ("for f in *.el; do printf '%s\\n' \"${f}\"; false; done | tail -1"
                     "0" "failure" "1 0")
                    ("for f in x; do trap 'exit $((40 + PIPESTATUS[0]))' EXIT; done; false | cat"
                     "41" "failure" nil)
                    ("yes | head -1" "0" "success" "141 0")
                    ("(exit 7) | yes | head -1" "0" "failure" "7 141 0")
                    ("yes | head -1 && printf continued" "0" "success" nil)
                    ("trap 'printf user-exit-trap' EXIT; false | cat"
                     "0" "success" nil)
                    ("trap 'exit $((40 + PIPESTATUS[0]))' EXIT; false | cat"
                     "41" "failure" nil)))
      (let ((result
             (test-niwa-execution-remote--run-tool
              session "Bash" (list :command (car case)
                                   :description "Check remote pipeline status"))))
        (should (string-search (format "exit_code=\"%s\"" (nth 1 case)) result))
        (should (string-search (format "outcome=\"%s\"" (nth 2 case)) result))
        (when (nth 3 case)
          (should (string-search (format "pipeline_exit_codes=\"%s\"" (nth 3 case))
                                 result)))
        (when (string-search "continued" (car case))
          (should (string-prefix-p "y\ncontinued" result)))
        (when (string-search "user-exit-trap" (car case))
          (should (string-prefix-p "user-exit-trap" result)))))
    (let ((eval-result
           (test-niwa-execution-remote--run-tool
            session "Eval"
            '(:expression
              "(progn (write-region \"live eval target\\n\" nil \"live-eval.txt\" nil 'silent) 'live-eval-ok)"
              :mode "live"))))
      (should (string-match-p "live-eval-ok" eval-result)))
    (should (file-exists-p eval-file))
    (let ((niwa-skills-include-bundled nil)
          (niwa-skill-dirs '(".niwa/skills/")))
      (niwa-skills-install session buffer)
      (let ((skill (niwa-session-get-skill session "remote-acceptance")))
        (should skill)
        (should (eq 'project (niwa-skill-source skill)))
        (should (file-remote-p (niwa-skill-source-file skill)))
        (should (string-match-p
                 "Remote skill marker"
                 (niwa-skill-load-body skill)))))))

(defun test-niwa-execution-remote--exercise-agent-directory
    (session root buffer)
  "Spawn a registered Agent in SESSION at a distinct directory under ROOT.
Only the provider start is substituted; tool admission, retained conversation,
target filesystem, Bash and Eval use their production paths in BUFFER."
  (let* ((child (file-name-as-directory (file-name-concat root "agent-cwd")))
         (file (file-name-concat child "cwd-marker.txt"))
         (grants (copy-tree (niwa-session-resource-grants session)))
         invocation)
    (make-directory child)
    (write-region "root-marker\n" nil
                  (file-name-concat root "cwd-marker.txt") nil 'silent)
    (write-region "child-marker\n" nil file nil 'silent)
    (unwind-protect
        (with-current-buffer buffer
          (let* ((gptel--known-backends (copy-tree gptel--known-backends))
                 (gptel-backend
                  (gptel-make-openai "Remote cwd acceptance"
                    :key "test" :models '(test-model)))
                 (gptel-model 'test-model)
                 (roots (niwa--all-allowed-roots buffer)))
            (cl-letf (((symbol-function 'niwa-agent-exec-run)
                       (lambda (_callback _role _description child-invocation
                                          _buffer &optional _configure)
                         (setq invocation child-invocation)
                         'provider-request)))
              (let ((result
                     (test-niwa-execution-remote--run-tool
                      session "Agent"
                      '(:task_name "cwd acceptance"
                        :message "Inspect the selected directory."
                        :working_directory "agent-cwd") buffer)))
                (should-not (string-prefix-p "Error:" result))
                (should invocation)))
            (let ((agent-buffer (niwa-agent-invocation-buffer invocation)))
              (should (buffer-live-p agent-buffer))
              (with-current-buffer agent-buffer
                (should (equal child (niwa-conversation-working-directory)))
                (should (equal roots (niwa--all-allowed-roots agent-buffer))))
              (dolist (spec '(("Read" (:file_path "cwd-marker.txt"))
                              ("Glob" (:pattern "cwd-marker.txt"))
                              ("Grep" (:pattern "child-marker" :path "."
                                       :output_mode "content"))))
                (let ((result (test-niwa-execution-remote--run-tool
                               session (car spec) (cadr spec) agent-buffer)))
                  (should (string-search (if (equal (car spec) "Glob")
                                            "cwd-marker.txt" "child-marker")
                                         result))
                  (should-not (string-search "root-marker" result))))
              (test-niwa-execution-remote--run-tool
               session "ApplyPatch"
               '(:patch "*** Begin Patch\n*** Add File: cwd-patch.txt\n+child patch\n*** End Patch")
               agent-buffer)
              (should (file-exists-p (file-name-concat child "cwd-patch.txt")))
              (dolist (spec '(("Bash" (:command "cat ./cwd-marker.txt"
                                      :description "Read child cwd marker"))
                              ("Eval" (:expression
                                       "(with-temp-buffer (insert-file-contents \"cwd-marker.txt\") (buffer-string))"
                                       :mode "live"))
                              ("Eval" (:expression
                                       "(with-temp-buffer (insert-file-contents \"cwd-marker.txt\") (buffer-string))"
                                       :mode "fresh"))))
                (let ((result (test-niwa-execution-remote--run-tool
                               session (car spec) (cadr spec) agent-buffer)))
                  (if (and (file-remote-p root)
                           (equal (plist-get (cadr spec) :mode) "fresh"))
                      (should
                       (equal
                        (concat "Error: Permission denied: "
                                "Fresh Eval is unavailable for remote sessions; "
                                "use Live Eval or Bash on the execution target")
                        result))
                    (should (string-search "child-marker" result))
                    (should-not (string-search "root-marker" result)))))
              ;; Directory selection does not rewrite any resource authority.
              (should (equal grants (niwa-session-resource-grants session)))
              (should (equal roots (niwa--all-allowed-roots buffer)))
              (with-current-buffer agent-buffer
                (let ((context (niwa-tool-exec-permission-current-context
                                "Bash" nil session)))
                  (setq context (plist-put context :mode 'ask)
                        context (plist-put context :allowed-roots (list root)))
                  (should
                   (equal (list (niwa-execution-target-native-path
                                 (niwa-session-execution-target session)
                                 (file-name-concat
                                  (file-name-directory (directory-file-name root))
                                  "outside-cwd-authority")))
                          (niwa-bash-policy-missing-resource-paths
                           "cat ../../outside-cwd-authority" context
                           '(:level use-default))))))
              (should (equal root (niwa-session-working-directory session)))
              (should-not (file-exists-p (file-name-concat root "cwd-patch.txt")))
              (should (string-search
                       "root-marker"
                       (test-niwa-execution-remote--run-tool
                        session "Read" '(:file_path "cwd-marker.txt") buffer))))))
      (when invocation
        (niwa-agent-control-interrupt
         session (niwa-agent-invocation-path invocation))
        (when-let* ((agent-buffer (niwa-agent-invocation-buffer invocation))
                    ((buffer-live-p agent-buffer)))
          (with-current-buffer agent-buffer (set-buffer-modified-p nil))
          (kill-buffer agent-buffer))))))

(defun test-niwa-execution-remote--exercise-control-filesystem (root)
  "Round trip pinned control operations against the real target at ROOT.

Session control stages content larger than a command line can carry, so both
the small and the staged path need proof on a real connection rather than on a
local temporary directory.  Durable session work is carried as programs of
pinned operations, and the round trips they save are exactly what a real
connection charges for, so the program path is proved here too."
  (let* ((directory (file-name-concat root "control-fs"))
         (small (file-name-concat directory "small.el"))
         (large (file-name-concat directory "large.bin"))
         (bytes (apply #'unibyte-string
                       (mapcar (lambda (index) (% index 256))
                               (number-sequence 1 (* 64 1024))))))
    (should (niwa-session-control-fs-make-directory directory t))
    (should (niwa-session-control-fs-create-file small "(:probe 1)"))
    (should-not (niwa-session-control-fs-create-file small "(:probe 2)"))
    (should (equal "(:probe 1)"
                   (niwa-session-control-fs-read-file small)))
    (should (niwa-session-control-fs-write-file
             large bytes 'no-conversion))
    (should (equal bytes
                   (niwa-session-control-fs-read-file
                    large 'no-conversion)))
    (should (member small (niwa-session-control-fs-list-directory
                           directory "\\`small\\.el\\'")))
    (should (integerp (niwa-session-control-fs-target-time directory)))
    ;; One program carries a whole compare-and-set plus its staged payload
    ;; across the real connection, and its proof still guards its writes.
    ;; This one is oversized for a command line, so it proves the request
    ;; file; the small program below proves argument delivery.
    (let ((results
           (niwa-session-control-fs-run-program
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
                   (niwa-session-control-fs-read-file small)))
    (let ((results
           (niwa-session-control-fs-run-program
            (list (list :op 'verify :path small :content "(:probe 1)")
                  (list :op 'write :path small :content "(:probe 4)")))))
      (should (equal 'mismatch (plist-get (nth 0 results) :status)))
      (should (equal 'skipped (plist-get (nth 1 results) :status))))
    (should (equal "(:probe 3)"
                   (niwa-session-control-fs-read-file small)))
    (niwa-session-control-fs-delete-directory directory)
    (should-not (niwa-session-control-fs-directory-p directory))))

(defun test-niwa-execution-remote--exercise-hook-provenance
    (session workspace root)
  "Run a user-loaded agent hook locally while retaining target event facts."
  (let* ((user-dir (make-temp-file "niwa-remote-hook-user-" t))
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
                   "cat > \"$NIWA_REMOTE_HOOK_INPUT\"\n"
                   "printf ran > \"$NIWA_REMOTE_HOOK_MARKER\"\n"
                   "printf '{\"system_message\":\"agent hook ran\"}'\n")
           nil script nil 'silent)
          (set-file-modes script #o755)
          (let ((user-emacs-directory user-dir))
            (eval
             `(niwa-define-agent ,agent-name
                                    :description "remote acceptance hook provenance"
                                    :hooks ((PreToolUse
                                             ((:matcher "Bash"
                                                        :hooks ((:type command :command ,script)))))))))
          (setq agent (niwa-agent-get agent-name)
                invocation (niwa-agent-invocation-create agent))
          (setf (niwa-agent-invocation-path invocation)
                "/root/remote-acceptance"
                (niwa-agent-invocation-parent-session invocation)
                session
                (niwa-agent-invocation-transcript-status invocation)
                'running)
          (let* ((payload
                  (niwa-hooks-event-plist
                   'PreToolUse session workspace
                   :tool-name "Bash"
                   :tool-input '(:command "printf hook")))
                 (handler
                  (car
                   (niwa-hooks--matching-handlers
                    'PreToolUse payload
                    (niwa-hooks-effective-rules
                     session workspace nil invocation)))))
            (should handler)
            (should (eq 'user (plist-get handler :source)))
            (should (equal user-dir (plist-get handler :source-root)))
            (let ((process-environment (copy-sequence process-environment)))
              (setenv "NIWA_REMOTE_HOOK_MARKER" marker)
              (setenv "NIWA_REMOTE_HOOK_INPUT" input)
              (with-temp-buffer
                (niwa-hooks-run-event
                 'PreToolUse payload
                 (lambda (value)
                   (setq decision value done t))
                 session workspace nil invocation)
                (test-niwa-execution-remote--wait
                 (lambda () done) "Agent hook did not settle")))))
      (should-not (plist-get decision :permission-decision))
      (should (file-exists-p marker))
      (should (file-exists-p input))
      (with-temp-buffer
        (insert-file-contents input)
        (should (string-match-p
                 (regexp-quote
                  (niwa-execution-target-native-path
                   (niwa-session-execution-target session) root))
                 (buffer-string))))
      t)
    (when agent
      (setq niwa-agent--registry
            (assoc-delete-all
             (niwa-agent-name agent) niwa-agent--registry)))
    (when (file-directory-p user-dir)
      (delete-directory user-dir t))))

(defun test-niwa-execution-remote--exercise-publication-recovery
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
               'niwa-session-publication--publish-critical-batches)
              (lambda (&rest _args)
                (error "Injected real remote publication failure"))))
          (niwa-session-artifacts-save session buffer))
      (error (setq failed t)))
    (should failed))
  (should (niwa-session-pending-publication session))
  (should-error
   (niwa-session-artifacts-assert-mutation-authority session buffer)
   :type 'user-error)
  (should (niwa-session-publication-retry session))
  (should-not (niwa-session-pending-publication session))
  (let ((recovery-dir (make-temp-file "niwa-remote-incomplete-" t)))
    (unwind-protect
        (progn
          (write-region "manual recovery evidence\n" nil
                        (file-name-concat recovery-dir "evidence.txt")
                        nil 'silent)
          (niwa-session-recovery-record-failure
           session "acceptance incomplete rollback" recovery-dir)
          (should (plist-get (niwa-session-pending-publication session)
                             :manual-recovery-marker))
          (should (niwa-session-recovery-read
                   (niwa-session-save-path session)))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (_prompt) t)))
            (should (niwa-session-publication-abandon session)))
          (should-not (niwa-session-pending-publication session))
          (should-not (niwa-session-recovery-read
                       (niwa-session-save-path session))))
      (when (file-directory-p recovery-dir)
        (delete-directory recovery-dir t)))))


;;
;;; Remote execution

(niwa-deftest test-niwa-execution-remote--exercise-agent-directory
  (:quiet t :before-each (niwa-tools-register))
  ,test
  (test)
  :doc "registered Agent cwd journey also works locally without remote provisioning"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-agent-cwd-" t)))
         (workspace (test-niwa-execution--workspace root))
         (session (niwa-session-create "cwd" workspace root))
         (buffer (generate-new-buffer " *agent-cwd-acceptance*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (niwa-chat-prepare-transcript-buffer)
            (setq-local niwa--session session niwa--workspace workspace
                        default-directory root))
          (niwa-session-set-root-buffer session buffer)
          (setf (niwa-session-permission-mode session) 'full-auto
                (niwa-session-sandbox-mode session) 'off)
          (test-niwa-execution-remote--exercise-agent-directory
           session root buffer))
      (niwa-execution-teardown-session session)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t))))

(niwa-deftest niwa-execution-run-one-shot/remote (:quiet t)
  ,test
  (test)
  :doc "dispatches in a TRAMP workspace and hides launcher control output"
  (let* ((root (make-temp-file "niwa-remote-execution-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (let ((result
                                                     (niwa-execution-run-one-shot
                                                      :name "niwa-test-remote-dispatch"
                                                      :command '("sh" "-c" "printf '%s' \"$PWD\"")
                                                      :workdir remote-root
                                                      :writable-roots (list remote-root))))
                                                (should-not (plist-get result :error))
                                                (should (= 0 (plist-get result :exit-code)))
                                                (should-not (plist-get result :termination))
                                                (should (equal (directory-file-name root)
                                                               (plist-get result :output)))))
      (delete-directory root t)))
  :doc "preserves target environment without forwarding client variables"
  (let* ((root (make-temp-file "niwa-remote-environment-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              ;; Establish the target shell before adding the client-only value.
                                              (should (file-directory-p remote-root))
                                              (let ((process-environment
                                                     (cons "NIWA_CLIENT_SECRET=do-not-forward"
                                                           process-environment)))
                                                (let ((result
                                                       (niwa-execution-run-one-shot
                                                        :name "niwa-test-remote-environment"
                                                        :command
                                                        '("sh" "-c"
                                                          "test -n \"$PATH\" && test \"$NIWA_EXECUTION\" = 1 && test -z \"${NIWA_CLIENT_SECRET+x}\"")
                                                        :workdir remote-root
                                                        :writable-roots (list remote-root))))
                                                  (should-not (plist-get result :error))
                                                  (should (= 0 (plist-get result :exit-code))))))
      (delete-directory root t)))
  :doc "localizes qualified command arguments in the target path domain"
  (let* ((root (make-temp-file "niwa-remote-argv-" t))
         (file (expand-file-name "input.txt" root))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (remote-file (concat remote-root "input.txt"))
         (niwa-sandbox-mode 'off))
    (unwind-protect
        (progn
          (write-region "input" nil file nil 'silent)
          (niwa-test--with-local-shell-tramp nil
                                                (let ((result
                                                       (niwa-execution-run-one-shot
                                                        :name "niwa-test-remote-argv"
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
  (let* ((root (make-temp-file "niwa-remote-group-" t))
         (pid-file (expand-file-name "child.pid" root))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-execution-process--child-kill-delay 0.05)
         (niwa-sandbox-mode 'off)
         calls child-pid)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (cl-letf (((symbol-function 'signal-process)
                                                         (lambda (&rest args)
                                                           (push args calls)
                                                           (apply original-signal-process args))))
                                                (let ((result
                                                       (test-niwa-execution--timeout-after-ready
                                                        pid-file
                                                        (lambda ()
                                                          (niwa-execution-run-one-shot
                                                           :name "niwa-test-remote-group"
                                                           :command
                                                           '("sh" "-c"
                                                             "trap '' TERM; sleep 30 & child=$!; printf '%s' \"$child\" > child.pid; wait")
                                                           :workdir remote-root
                                                           :writable-roots (list remote-root)
                                                           :timeout 0.1)))))
                                                  (should (plist-get result :timed-out-p))
                                                  (should (eq 'timed-out (plist-get result :termination)))
                                                  (setq child-pid
                                                        (test-niwa-execution--read-pid pid-file))
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
  (let* ((root (make-temp-file "niwa-remote-unknown-" t))
         (pid-file (expand-file-name "group.pid" root))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-execution-process--child-kill-delay 0.02)
         (niwa-sandbox-mode 'off)
         group-id session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf (((symbol-function 'signal-process)
                                                         (lambda (&rest args)
                                                           (if (and (integerp (car args))
                                                                    (< (car args) 0)
                                                                    (memq (cadr args) '(TERM KILL))
                                                                    (equal remote-root (nth 2 args)))
                                                               (error "Transport lost")
                                                             (apply original-signal-process args)))))
                                                (let ((result
                                                       (test-niwa-execution--timeout-after-ready
                                                        pid-file
                                                        (lambda ()
                                                          (niwa-execution-run-one-shot
                                                           :name "niwa-test-remote-unknown"
                                                           :command
                                                           '("sh" "-c"
                                                             "ps -o pgid= -p $$ | tr -d ' ' > group.pid; trap '' TERM; sleep 30 & wait")
                                                           :workdir remote-root
                                                           :writable-roots (list remote-root)
                                                           :timeout 0.05
                                                           :session session)))))
                                                  (setq group-id
                                                        (test-niwa-execution--read-pid pid-file))
                                                  (should (eq 'unknown (plist-get result :termination)))
                                                  (should (niwa-execution-mutation-blocked-p session))))
                                              (when session
                                                (niwa-execution-teardown-session session))
                                              (when (and group-id (> group-id 0))
                                                (ignore-errors
                                                  (funcall original-signal-process (- group-id) 'KILL)))
                                              (delete-directory root t)))
    :doc "runs the Bubblewrap and FD grant wrappers in the target path domain"
    (let* ((root (make-temp-file "niwa-remote-bwrap-" t))
           (external-root (make-temp-file "niwa-remote-grant-" t))
           (client-only (make-temp-file "niwa-client-protected-" t))
           (external-target (expand-file-name "secret.txt" external-root))
           (external (expand-file-name "secret-link" external-root))
           (bin (expand-file-name "bin" root))
           (bwrap (expand-file-name "bwrap" bin))
           (args-file (expand-file-name "bwrap.args" root))
           (target-protected-name
            (make-temp-name ".niwa-test-credentials-"))
           (remote-root
            (format "/niwamock:%s:%s/" (system-name) root))
           (remote-external-root
            (format "/niwamock:%s:%s/" (system-name) external-root))
           (remote-external
            (format "/niwamock:%s:%s" (system-name) external))
           (tramp-remote-path (cons bin tramp-remote-path))
           (niwa-protected-paths
            `((,(concat client-only "/**") . inaccessible)
              (,(concat "~/" target-protected-name "/**") . inaccessible)
              (,(concat remote-external-root "**") . inaccessible)))
           (niwa-sandbox--probe-cache nil)
           (niwa-sandbox-mode 'required))
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
            (niwa-test--with-local-shell-tramp nil
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
                                                                 (niwa-execution-run-one-shot
                                                                  :name "niwa-test-remote-bwrap"
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
                                                               (string-match-p "niwamock" arguments))
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
        (delete-directory root t)))))

(niwa-deftest niwa-execution-start-bash/remote
  (:quiet t :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "a Monitor through TRAMP's shell adapter keeps stderr out of events"
  (let* ((root (make-temp-file "niwa-remote-monitor-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         (niwa-transport-remote-direct-async nil)
         (make-nearby-temp-file-function
          (symbol-function 'make-nearby-temp-file))
         session events stderr-paths)
    (unwind-protect
        (cl-letf (((symbol-function 'make-nearby-temp-file)
                   ;; Record this test's own target-side files; listing the
                   ;; shared temporary directory races with parallel workers.
                   (lambda (prefix &rest args)
                     (let ((path (apply make-nearby-temp-file-function
                                        prefix args)))
                       (when (string-prefix-p "niwa-stderr-" prefix)
                         (push path stderr-paths))
                       path))))
          (niwa-test--with-local-shell-tramp nil
                                                (setq session (test-niwa-execution--session remote-root))
                                                (let ((niwa-execution-mailbox-delivery-function
                                                       (lambda (event _ ack) (push event events) (funcall ack t nil) t)))
                                                  (niwa-execution-start-bash
                                                   #'ignore
                                                   :kind 'monitor :description "remote" :session session
                                                   :owner "/root" :owner-context session
                                                   :command '("sh" "-c" "printf 'warn\\n' >&2; printf 'event\\n'")
                                                   :workdir remote-root :writable-roots (list remote-root))
                                                  (test-niwa-execution--wait
                                                   (lambda ()
                                                     (cl-find 'terminal events :key (lambda (e) (plist-get e :type))))
                                                   15))
                                                (let* ((terminal (cl-find 'terminal events
                                                                          :key (lambda (e) (plist-get e :type))))
                                                       (bodies (concat
                                                                (mapconcat (lambda (e) (or (plist-get e :body) ""))
                                                                           events "")
                                                                (mapconcat #'identity
                                                                           (plist-get terminal :monitor-lines) "\n"))))
                                                  (should (string-match-p "event" bodies))
                                                  (should-not (string-match-p "warn" bodies))
                                                  ;; The output file still carries stderr.
                                                  (should (string-match-p "warn" (plist-get terminal :whole-output)))
                                                  (should (string-match-p "event" (plist-get terminal :whole-output))))
                                                (should (= 1 (length stderr-paths)))
                                                (should-not (seq-some #'file-exists-p stderr-paths))))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)
      (mapc #'delete-file (seq-filter #'file-exists-p stderr-paths))))

  :doc "keeps the live spool local and out of model-facing target facts"
  (let* ((root (make-temp-file "niwa-remote-managed-spool-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         session initial execution-id spool)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root)
                                                    initial
                                                    (test-niwa-execution--start-managed
                                                     session remote-root
                                                     '("sh" "-c" "printf ready; sleep 30")
                                                     :yield-time-ms 10))
                                              (let ((facts (plist-get initial :facts)))
                                                (setq execution-id (plist-get facts :execution-id))
                                                (should (stringp execution-id))
                                                (should-not (plist-get facts :output-path)))
                                              (setq spool
                                                    (plist-get (car (niwa-execution-list-user session))
                                                               :artifact-path))
                                              (should (stringp spool))
                                              (should-not (file-remote-p spool))
                                              (should (file-exists-p spool)))
      (when session
        (niwa-execution-teardown-session session))
      (when spool
        (should-not (file-exists-p spool)))
      (delete-directory root t)))

  :doc "classifies transport exit before PGID capture as unknown"
  (let* ((root (make-temp-file "niwa-remote-missing-pgid-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         (original-make-process (symbol-function 'make-process))
         session result)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              ;; This case classifies transport exit, not target fencing: the
                                              ;; mock method runs a local shell whose environment depends on
                                              ;; which caller opens it first, so keep the fence out of it.
                                              (let ((niwa-session-artifacts--checking-incarnation t))
                                                (cl-letf
                                                    (((symbol-function 'make-process)
                                                      (lambda (&rest args)
                                                        (if (equal "niwa-bash" (plist-get args :name))
                                                            (let ((default-directory temporary-file-directory))
                                                              (apply original-make-process
                                                                     (plist-put
                                                                      (plist-put args :file-handler nil)
                                                                      :command '("sh" "-c" "exit 0"))))
                                                          (apply original-make-process args)))))
                                                  (setq result
                                                        (test-niwa-execution--start-managed
                                                         session remote-root '("sh" "-c" "true")
                                                         :yield-time-ms nil))))
                                              (should (eq 'unknown
                                                          (plist-get (plist-get result :facts) :termination)))
                                              (should (niwa-execution-mutation-blocked-p session)))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "settles a proven remote spool failure without retaining mutation authority"
  (let* ((root (make-temp-file "niwa-remote-spool-failure-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         (original-write-region (symbol-function 'write-region))
         session result)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf
                                                  (((symbol-function 'write-region)
                                                    (lambda (start end filename &rest args)
                                                      (if (and (stringp filename)
                                                               (string-match-p
                                                                "/\\.niwa-pending-executions/execution-"
                                                                filename)
                                                               (car args))
                                                          (signal 'file-error '("Injected spool failure"))
                                                        (apply original-write-region
                                                               start end filename args)))))
                                                (setq result
                                                      (test-niwa-execution--start-managed
                                                       session remote-root '("sh" "-c" "printf output")
                                                       :yield-time-ms nil)))
                                              ;; A proven local write failure only stays proven while the
                                              ;; target's process-group identity was received and nothing in
                                              ;; the escalation replaced the cause.  Assert all three in one
                                              ;; form, so a settlement that went unknown names what it settled
                                              ;; on instead of only disagreeing about the termination.
                                              (let ((facts (plist-get result :facts)))
                                                (should
                                                 (equal (list 'output-write-failed t "Injected spool failure")
                                                        (list (plist-get facts :termination)
                                                              (integerp (plist-get facts :group-id))
                                                              (error-message-string
                                                               (plist-get result :error))))))
                                              ;; The decision-point predicate, not the pollable one: a
                                              ;; settlement a busy connection refused is owed, and this is
                                              ;; where it is written.
                                              (should-not (niwa-execution-mutation-refused-p session))
                                              (should-not
                                               (niwa-session-durability-unsettled-mutation-p session)))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "keeps the latch when remote launch errors after process attempt"
  (let* ((root (make-temp-file "niwa-remote-launch-attempt-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         session result)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf
                                                  (((symbol-function 'make-process)
                                                    (lambda (&rest _)
                                                      (error "Injected post-attempt launch failure"))))
                                                (setq result
                                                      (test-niwa-execution--start-managed
                                                       session remote-root '("sh" "-c" "true")
                                                       :yield-time-ms nil)))
                                              (should (eq 'unknown
                                                          (plist-get (plist-get result :facts) :termination)))
                                              (should (niwa-execution-mutation-blocked-p session))
                                              (should
                                               (niwa-session-durability-unsettled-mutation-p session))
                                              (niwa-execution-acknowledge-unknown session)
                                              (should-not
                                               (niwa-session-durability-unsettled-mutation-p session)))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "stages omitted output on the target before exposing its native path"
  (let* ((root (make-temp-file "niwa-remote-published-output-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         session result published-path published-content published-coding)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf
                                                  (((symbol-function
                                                     'niwa-session-artifacts-publish-text)
                                                    (lambda (_session path content &optional coding)
                                                      (setq published-path path
                                                            published-content content
                                                            published-coding coding)
                                                      'published)))
                                                (setq result
                                                      (test-niwa-execution--start-managed
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
                                                (should-not (string-match-p "niwamock" output-path))
                                                (should (string-match-p
                                                         "/tool-results/executions/exec-[[:alnum:]]+-000001\\.log\\'"
                                                         output-path))))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "does not expose a target output path when publication fails"
  (let* ((root (make-temp-file "niwa-remote-output-failure-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         session result diagnostics)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf
                                                  (((symbol-function
                                                     'niwa-session-artifacts-publish-text)
                                                    (lambda (&rest _)
                                                      (error "Publication failed"))))
                                                (niwa-test--with-captured-diagnostics diagnostics
                                                                                         (setq result
                                                                                               (test-niwa-execution--start-managed
                                                                                                session remote-root
                                                                                                '("sh" "-c" "printf '%03000d' 0")
                                                                                                :yield-time-ms nil))))
                                              (should (string-match-p "Publication failed" diagnostics))
                                              (should-not (plist-get (plist-get result :facts) :output-path)))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "side and root writers share one shallow durable mutation authority"
  (let* ((root (make-temp-file "niwa-remote-overlap-latch-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal))
         parent side root-buffer side-buffer
         first second first-done second-done)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (let ((workspace (test-niwa-execution--workspace remote-root)))
                                                (setq parent (niwa-session-create "main" workspace remote-root)
                                                      side (niwa-session-create "side" workspace remote-root)
                                                      root-buffer (generate-new-buffer " *remote-root-authority*")
                                                      side-buffer (generate-new-buffer " *remote-side-writer*"))
                                                (setf (niwa-session-audit-session side) parent
                                                      (niwa-session-sandbox-mode parent) 'off
                                                      (niwa-session-sandbox-mode side) 'off)
                                                (with-current-buffer root-buffer
                                                  (setq-local niwa--workspace workspace
                                                              niwa--session parent)
                                                  (setq default-directory remote-root))
                                                ;; A live view registers the parent's root data buffer; the side
                                                ;; conversation's durable authority resolves through it.
                                                (niwa-session-control-transfer-register-root-buffer
                                                 parent root-buffer)
                                                (with-current-buffer side-buffer
                                                  (setq-local niwa--workspace workspace
                                                              niwa--session side)
                                                  (setq default-directory remote-root)))
                                              (puthash
                                               (niwa-execution-target-identity
                                                (niwa-session-execution-target parent))
                                               t niwa-session-durability--disclosed-targets)
                                              (setq first
                                                    (test-niwa-execution--start-managed
                                                     side remote-root
                                                     '("sh" "-c" "printf first-ready; sleep 30")
                                                     :data-buffer side-buffer
                                                     :yield-time-ms 10)
                                                    second
                                                    (test-niwa-execution--start-managed
                                                     parent remote-root
                                                     '("sh" "-c" "printf second-ready; sleep 30")
                                                     :data-buffer root-buffer
                                                     :yield-time-ms 10))
                                              (should (niwa-session-save-path parent))
                                              (should (buffer-local-value 'buffer-file-name root-buffer))
                                              (should-not (buffer-local-value 'buffer-file-name side-buffer))
                                              (should (niwa-session-durability-unsettled-mutation-p parent))
                                              (niwa-execution-stop
                                               side "main" (plist-get (plist-get first :facts) :execution-id)
                                               (lambda (value) (setq first-done value)))
                                              (test-niwa-execution--wait (lambda () first-done))
                                              (should (niwa-session-durability-unsettled-mutation-p parent))
                                              (niwa-execution-stop
                                               parent "main" (plist-get (plist-get second :facts) :execution-id)
                                               (lambda (value) (setq second-done value)))
                                              (test-niwa-execution--wait (lambda () second-done))
                                              (should-not
                                               (niwa-session-durability-unsettled-mutation-p parent)))
      (dolist (session (list side parent))
        (when session
          (niwa-execution-teardown-session session)))
      (when (and parent (niwa-session-save-path parent))
        (niwa-session-durability-lease-release
         (niwa-session-save-path parent) parent))
      (dolist (buffer (list side-buffer root-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "keeps queued output reachable through its logical session path"
  (let* ((root (make-temp-file "niwa-remote-output-queued-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         session result)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (cl-letf
                                                  (((symbol-function
                                                     'niwa-session-artifacts-publish-text)
                                                    (lambda (&rest _) 'queued)))
                                                (setq result
                                                      (test-niwa-execution--start-managed
                                                       session remote-root
                                                       '("sh" "-c" "printf '%03000d' 0")
                                                       :yield-time-ms nil)))
                                              (let ((output-path
                                                     (plist-get (plist-get result :facts) :output-path)))
                                                (should (stringp output-path))
                                                (should-not (file-remote-p output-path))
                                                (should (string-match-p
                                                         "/tool-results/executions/exec-[[:alnum:]]+-000001\\.log\\'"
                                                         output-path))))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t)))

  :doc "a stop settles fast when only zombies remain in the group"
  (let* ((root (make-temp-file "niwa-remote-zombie-settle-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (holder-file (expand-file-name "holder.pid" root))
         (original-signal-process (symbol-function 'signal-process))
         (niwa-sandbox-mode 'off)
         ;; The production grace: the assertion is that a settled group
         ;; does not ride it.
         (niwa-execution-process--child-kill-delay 2)
         session result group-id holder-pid stopped)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              ;; The inner holder forks an exiting child inside the managed
                                              ;; group, then leaves the group via setsid while keeping the
                                              ;; child unreaped: a held zombie whose pgrp stays the managed
                                              ;; group for as long as the holder lives.  A shell can reap its
                                              ;; child before exec, so use a parent that never calls wait.
                                              (cl-letf (((symbol-function
                                                          'niwa-session-artifacts-publish-text)
                                                         (lambda (&rest _) 'queued)))
                                                (setq result
                                                      (test-niwa-execution--start-managed
                                                       session remote-root
                                                       (list "sh" "-c"
                                                             (concat "python3 -c "
                                                                     (shell-quote-argument
                                                                      (string-join
                                                                       '("import os, time"
                                                                         "if os.fork() == 0: os._exit(0)"
                                                                         "os.setsid()"
                                                                         "with open('holder.pid', 'w') as out: out.write(str(os.getpid()))"
                                                                         "time.sleep(300)")
                                                                       "\n"))
                                                                     " & "
                                                                     "printf ready; exec sleep 300"))
                                                       :yield-time-ms 10))
                                                ;; Keep the detached holder reachable for cleanup if the marker never arrives.
                                                (test-niwa-execution--wait
                                                 (lambda () (file-exists-p holder-file)))
                                                (setq holder-pid
                                                      (test-niwa-execution--read-pid holder-file))
                                                ;; An early yield can precede delivery of the remote group marker.
                                                (test-niwa-execution--wait
                                                 (lambda ()
                                                   (setq group-id
                                                         (plist-get
                                                          (plist-get
                                                           (test-niwa-execution--observe
                                                            session
                                                            (plist-get (plist-get result :facts) :execution-id)
                                                            :wait-ms 0)
                                                           :facts)
                                                          :group-id))
                                                   (and (integerp group-id) (> group-id 0))))
                                                ;; Observe the fixture's zombie instead of assuming
                                                ;; the short-lived child ran within 0.3 seconds.
                                                (test-niwa-execution--wait
                                                 (lambda ()
                                                   (cl-some
                                                    (lambda (pid)
                                                      (let ((attributes (process-attributes pid)))
                                                        (and (eql holder-pid (alist-get 'ppid attributes))
                                                             (eql group-id (alist-get 'pgrp attributes))
                                                             (equal "Z" (alist-get 'state attributes)))))
                                                    (list-system-processes))))
                                                (let ((start (float-time)))
                                                  (niwa-execution-stop
                                                   session "main"
                                                   (plist-get (plist-get result :facts) :execution-id)
                                                   (lambda (value) (setq stopped value)))
                                                  (test-niwa-execution--wait (lambda () stopped))
                                                  ;; Riding the grace takes at least the kill delay.  A fixed
                                                  ;; 1 s bound also measured mock TRAMP round trips, which took
                                                  ;; 1.06 s under a full parallel suite.
                                                  (should (< (- (float-time) start)
                                                             niwa-execution-process--child-kill-delay))))
                                              ;; The zombie still answers the raw group probe: only
                                              ;; zombie-awareness settled this stop early.
                                              (should (integerp group-id))
                                              (should (> group-id 0))
                                              (should (zerop (funcall original-signal-process (- group-id) 0)))
                                              (should-not (niwa-execution-unsettled-mutation-p session)))
      (when (and (integerp holder-pid) (> holder-pid 0))
        (ignore-errors (funcall original-signal-process holder-pid 'KILL)))
      (when session
        (niwa-execution-teardown-session session))
      (delete-directory root t))))



(niwa-deftest niwa-execution-unsettled-mutation-p
  (:quiet t :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "reports a durable latch even when no transient process record exists"
  (let* ((root (make-temp-file "niwa-remote-durable-latch-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (session nil))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq session (test-niwa-execution--session remote-root))
                                              (niwa-session-artifacts-assert-mutation-authority session)
                                              (should
                                               (niwa-session-durability-set-unsettled-mutation session t))
                                              (should (niwa-execution-unsettled-mutation-p session))
                                              (should (niwa-execution-mutation-blocked-p session))
                                              (should
                                               (niwa-session-durability-set-unsettled-mutation session nil))
                                              (should-not (niwa-execution-unsettled-mutation-p session)))
      (when (and session (niwa-session-save-path session))
        (niwa-session-durability-lease-release
         (niwa-session-save-path session) session))
      (when (file-directory-p root)
        (delete-directory root t))))

  :doc "settles a proven spool failure whose latch write finds a busy transport"
  ;; Settlement is reached from a filter or a timer, so the durable write
  ;; can come due mid-command.  The control filesystem refuses to nest, and
  ;; that refusal says nothing about the target: spending the proven cause
  ;; on it would block every later mutating execution in the session.
  (let* ((root (make-temp-file "niwa-remote-busy-settlement-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-mode 'off)
         (original-write-region (symbol-function 'write-region))
         (original-set-unsettled
          (symbol-function 'niwa-session-durability-set-unsettled-mutation))
         (refusals 0)
         parent side result)
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
                                              (setq parent (test-niwa-execution--session remote-root)
                                                    side (niwa-session-create
                                                          "side" (niwa-session-workspace parent) remote-root))
                                              (setf (niwa-session-audit-session side) parent
                                                    (niwa-session-sandbox-mode side) 'off)
                                              (cl-letf
                                                  (((symbol-function
                                                     'niwa-session-durability-set-unsettled-mutation)
                                                    (lambda (authority value)
                                                      ;; Only the settling write meets a busy connection, and
                                                      ;; only until the operation in flight completes.
                                                      (if (and (null value) (zerop refusals))
                                                          (progn
                                                            (setq refusals (1+ refusals))
                                                            (niwa-transport-call-as-remote-operation
                                                             (lambda ()
                                                               (funcall original-set-unsettled
                                                                        authority value))))
                                                        (funcall original-set-unsettled authority value))))
                                                   ((symbol-function 'write-region)
                                                    (lambda (start end filename &rest args)
                                                      (if (and (stringp filename)
                                                               (string-match-p
                                                                "/\\.niwa-pending-executions/execution-"
                                                                filename)
                                                               (car args))
                                                          (signal 'file-error '("Injected spool failure"))
                                                        (apply original-write-region
                                                               start end filename args)))))
                                                (dolist (consumer '(admission transfer))
                                                  (setq refusals 0
                                                        result
                                                        (test-niwa-execution--start-managed
                                                         side remote-root '("sh" "-c" "printf output")
                                                         :yield-time-ms nil))
                                                  (should (= 1 refusals))
                                                  (should
                                                   (eq 'output-write-failed
                                                       (plist-get (plist-get result :facts) :termination)))
                                                  ;; The write is owed, not lost, until a parent decision
                                                  ;; point settles the side conversation's shared debt.
                                                  (should
                                                   (niwa-session-durability-unsettled-mutation-p parent))
                                                  (should (niwa-execution-mutation-blocked-p parent))
                                                  (if (eq consumer 'admission)
                                                      (should-not (niwa-execution-mutation-refused-p parent))
                                                    (should (niwa-session-control-transfer-drained-p parent)))
                                                  (should-not
                                                   (niwa-session-durability-unsettled-mutation-p parent)))))
      (dolist (session (list side parent))
        (when session
          (niwa-execution-teardown-session session)))
      (delete-directory root t))))



;;
;;; Opt-in real transport acceptance

(defun test-niwa-execution-remote--apply-diff (session root)
  "Apply the acceptance diff through the public diff command at ROOT."
  (let ((buffer (generate-new-buffer " *niwa real remote diff*")))
    (unwind-protect
        (with-current-buffer buffer
          (setq default-directory root)
          (setq-local niwa--workspace (niwa-session-workspace session)
                      niwa--session session)
          (insert
           (concat "diff --git a/acceptance.el b/acceptance.el\n"
                   "--- a/acceptance.el\n"
                   "+++ b/acceptance.el\n"
                   "@@ -1,7 +1,7 @@\n"
                   " ;;; acceptance.el --- real remote acceptance -*- lexical-binding: t -*-\n"
                   " \n"
                   " (defun niwa-real-remote-target ()\n"
                   "   \"Return the remote acceptance marker.\"\n"
                   "-  \"patched\")\n"
                   "+  \"diff-applied\")\n"
                   " \n"
                   " (defun niwa-real-remote-caller ()\n"))
          (diff-mode)
          (niwa-diff-apply-buffer t)
          (when-let* ((file-buffer
                       (find-buffer-visiting
                        (file-name-concat root "acceptance.el"))))
            (kill-buffer file-buffer)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun test-niwa-execution-remote--exercise-file-tools
    (session target root)
  "Exercise ordinary file tools for SESSION and TARGET below ROOT."
  (let* ((file (file-name-concat root "acceptance.el"))
         (tree-file (file-name-concat root "acceptance.sh"))
         (native-file (niwa-execution-target-native-path target file))
         (identifier "niwa-real-remote-target"))
    (write-region
     (concat
      ";;; acceptance.el --- real remote acceptance -*- lexical-binding: t -*-\n"
      "\n"
      "(defun niwa-real-remote-target ()\n"
      "  \"Return the remote acceptance marker.\"\n"
      "  \"alpha\")\n"
      "\n"
      "(defun niwa-real-remote-caller ()\n"
      "  (niwa-real-remote-target))\n"
      "\n"
      ";; acceptance needle\n")
     nil file nil 'silent)
    (should
     (string-match-p
      "acceptance needle"
      (test-niwa-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (string-match-p
      (regexp-quote native-file)
      (test-niwa-execution-remote--run-tool
       session "Glob" '(:pattern "*.el" :path "."))))
    (let ((grep
           (test-niwa-execution-remote--run-tool
            session "Grep"
            '(:pattern "acceptance needle" :path "."
                       :output_mode "content"))))
      (should (string-match-p "acceptance needle" grep))
      (should (string-match-p (regexp-quote native-file) grep)))
    (let ((result
           (test-niwa-execution-remote--run-tool
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
      (test-niwa-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (test-niwa-execution-remote--apply-diff session root)
    (should
     (string-match-p
      "diff-applied"
      (test-niwa-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (string-match-p
      identifier
      (test-niwa-execution-remote--run-tool
       session "Imenu" '(:file_path "acceptance.el"))))
    (let ((default-directory root)
          (process-environment nil))
      (should (zerop (process-file "git" nil nil nil "init" "--quiet")))
      (should (zerop (process-file "git" nil nil nil "add" "acceptance.el"))))
    (let ((result
           (test-niwa-execution-remote--run-tool
            session "XrefReferences"
            (list :identifier identifier :file_path "acceptance.el"))))
      (should (string-match-p identifier result))
      (should (string-match-p (regexp-quote native-file) result)))
    (should
     (string-match-p
      "not supported for remote workspaces"
      (test-niwa-execution-remote--run-tool
       session "XrefDefinitions"
       (list :pattern identifier :file_path "acceptance.el"))))
    (write-region
     "#!/usr/bin/env bash -*- mode: bash-ts -*-\necho acceptance\n"
     nil tree-file nil 'silent)
    (let ((result
           (test-niwa-execution-remote--run-tool
            session "Treesitter"
            '(:file_path "acceptance.sh" :whole_file t))))
      (should-not (string-prefix-p "Error" result))
      (should (string-match-p "program" result)))
    file))

(defun test-niwa-execution-remote--exercise-project-hook
    (session root)
  "Run a trusted project command hook for SESSION on target ROOT."
  (let* ((hook-dir (file-name-concat root ".niwa" "hooks"))
         (script (file-name-concat hook-dir "accept-read.sh"))
         (config (file-name-concat root ".niwa" "hooks.json"))
         (marker (file-name-concat root ".niwa" "hook-cwd"))
         (native-root
          (niwa-execution-target-native-path
           (niwa-session-execution-target session) root))
         (niwa-hooks-require-project-trust nil))
    (make-directory hook-dir t)
    (write-region
     (concat "#!/bin/sh\n"
             "printf '%s' \"$PWD\" > .niwa/hook-cwd\n"
             "printf '%s' '{\"system_message\":\"remote hook ran\"}'\n")
     nil script nil 'silent)
    (set-file-modes script #o755)
    (write-region
     (concat
      "{\"hooks\":{\"PreToolUse\":[{\"matcher\":\"Read\","
      "\"hooks\":[{\"type\":\"command\","
      "\"command\":\".niwa/hooks/accept-read.sh\","
      "\"timeout\":10,\"failClosed\":true}]}]}}")
     nil config nil 'silent)
    ;; Configured rule layers are memoized per workspace; a hook file
    ;; written mid-session is picked up after an explicit reload.
    (niwa-hooks-reload)
    (should
     (string-match-p
      "acceptance needle"
      (test-niwa-execution-remote--run-tool
       session "Read" '(:file_path "acceptance.el"))))
    (should
     (equal (directory-file-name native-root)
            (with-temp-buffer
              (insert-file-contents marker)
              (buffer-string))))))

(defun test-niwa-execution-remote--exercise-publication-media
    (session buffer)
  "Publish SESSION and round-trip durable media through BUFFER."
  (with-current-buffer buffer
    (insert "Remote publication transcript\n")
    (should (niwa-session-artifacts-save session buffer)))
  (let* ((niwa-tool-media--store nil)
         (save-path (niwa-session-save-path session))
         (publication
          (niwa-session-publication-read save-path))
         (tool-results (file-name-concat save-path "tool-results"))
         (media '((:mime "image/png" :kind image :data "QUJD")))
         (attached
          (niwa-tool-media-attach-result
           "visible" media tool-results "toolu_remote" session)))
    (should publication)
    (should
     (cl-find (niwa-session-session-id session)
              (niwa-session-persistence-list-sessions
               (niwa-session-workspace session))
              :test #'equal
              :key (lambda (entry)
                     (plist-get (plist-get entry :summary) :session-id))))
    (with-current-buffer buffer
      (should (niwa-session-artifacts-save session buffer t)))
    (setq niwa-tool-media--store nil)
    (let ((restored
           (niwa-tool-media-extract
            attached tool-results "toolu_remote" nil session)))
      (should (equal "visible" (car restored)))
      (should (equal media (cdr restored))))))

(defun test-niwa-execution-remote--exercise-worktree-fork
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
                                   "niwa-acceptance@example.invalid")))
      (should (zerop (process-file "git" nil nil nil "config" "user.name"
                                   "Niwa Acceptance")))
      (should (zerop (process-file "git" nil nil nil "add" "source-only.txt")))
      (should (zerop (process-file "git" nil nil nil "commit" "-m"
                                   "acceptance baseline"))))
    ;; A settled save normally supplies this index.  Pin one stable fork point
    ;; here so this acceptance remains independent of provider transcript text.
    (setf (niwa-session-prompt-index session)
          '((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                         :fork-point-id "real-remote-fork"
                         :pos 1 :transcript-cutoff 1)))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert
       (niwa--format-hook-audit-record
        '(:type fork-point :fork-point-id "real-remote-fork"
                :segment 1 :turn 1 :file-turn 1 :cum-turn 1
                :captured-file-turn 1)))
      (niwa-session-artifacts-save session buffer))
    (setq reservation (niwa-worktree-fork-reservation session))
    (setq target (plist-put target :worktree-reservation reservation))
    (setq child (niwa-session-fork-create buffer target 'worktree))
    (should (buffer-live-p child))
    (let ((child-session (buffer-local-value 'niwa--session child))
          (child-directory
           (plist-get reservation :directory))
          (child-native-directory
           (niwa-execution-target-native-path
            (niwa-session-execution-target
             (buffer-local-value 'niwa--session child))
            (plist-get reservation :directory))))
      (should (file-directory-p child-directory))
      (should (stringp (niwa-session-save-path child-session)))
      (should (niwa-session-durability-lease-owned-p child-session))
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
        (niwa-session-durability-lease-release
         (niwa-session-save-path child-session) child-session))
      (kill-buffer child)
      (let ((default-directory root))
        (should
         (zerop
          (process-file "git" nil nil nil "worktree" "remove" "--force"
                        child-native-directory)))
        (should-not (file-directory-p child-directory)))))
  t)

(defun test-niwa-execution-remote--exercise-plan-worktree-entry
    (session root)
  "Validate Plan Worktree branch entry and target-side Git-unavailable failure."
  (let ((entry (list :session session
                     :selection '(:location worktree :context fresh))))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "worktree/accepted-plan")))
      (should (equal "worktree/accepted-plan"
                     (niwa-plan-mode--read-worktree-branch entry)))))
  (let* ((fake-bin (file-name-concat root ".niwa" "git-unavailable-bin"))
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
                  (niwa-worktree-fork-preflight session)
                  :type 'user-error)))
            (should (string-match-p
                     "Git is required on the session execution target"
                     (error-message-string err)))))
      (when (file-directory-p fake-bin)
        (delete-directory fake-bin t)))))

(defun test-niwa-execution-remote--exercise-cold-entry
    (session buffer workspace)
  "Save, discover, release, and cold-resume SESSION."
  (let ((session-id (niwa-session-session-id session))
        (save-path (niwa-session-save-path session)))
    (with-current-buffer buffer
      (niwa-session-artifacts-save session buffer t)
      (set-buffer-modified-p nil))
    (should
     (cl-find session-id
              (niwa-session-persistence-list-sessions workspace)
              :key (lambda (item)
                     (plist-get (plist-get item :summary) :session-id))
              :test #'equal))
    (niwa-session-durability-lease-release save-path session)
    (kill-buffer buffer)
    (niwa-workspace-clear-registry)
    (let* ((fresh-workspace
            (niwa-workspace-get-or-create
             'project (niwa-workspace-id workspace)
             (niwa-workspace-root workspace)
             (niwa-workspace-name workspace)))
           (restored
            (niwa-session-persistence-resume-id
             fresh-workspace session-id)))
      (should (buffer-live-p restored))
      (with-current-buffer restored
        (should (eq fresh-workspace niwa--workspace))
        (should (equal session-id
                       (niwa-session-session-id niwa--session)))
        (should-not buffer-read-only))
      restored)))

(defun test-niwa-execution-remote--exercise-ssh-aliases ()
  "Prove two SSH aliases discover one workspace and Save As children."
  (let ((root-a (getenv "NIWA_TEST_SSH_ALIAS_A_ROOT"))
        (root-b (getenv "NIWA_TEST_SSH_ALIAS_B_ROOT")))
    (unless (and root-a root-b)
      (ert-skip "SSH alias roots are not configured"))
    (when-let* ((config (getenv "NIWA_TEST_SSH_CONFIG")))
      (setq tramp-use-connection-share t
            tramp-ssh-controlmaster-options
            (format "-F %s" (shell-quote-argument config))))
    (let* ((root-a (file-name-as-directory root-a))
           (root-b (file-name-as-directory root-b))
           (directory-name
            (format "niwa-alias-%s"
                    (substring
                     (md5 (format "%s-%s" (float-time) (random))) 0 12)))
           (dir-a (file-name-as-directory
                   (file-name-concat root-a directory-name)))
           (dir-b
            (file-name-as-directory
             (file-name-concat
              root-b directory-name)))
           (niwa-test--write-roots
            (append (list dir-a dir-b) niwa-test--write-roots))
           (workspace-a (test-niwa-execution--workspace dir-a))
           (workspace-b
            (niwa-workspace-get-or-create
             'project dir-b dir-b "alias-b"))
           (session nil)
           (parent-id nil)
           (child-id nil)
           (buffer (generate-new-buffer " *remote-alias-save-as*")))
      (test-niwa-execution-remote--with-temporary-directory dir-a
	(test-niwa-execution-remote--with-temporary-directory dir-b
	  (unwind-protect
	      (progn
		(test-niwa-execution-remote--stage "make directory")
		(make-directory dir-a t)
		(should (equal
			 (niwa-workspace-identity-read dir-a)
			 (niwa-workspace-identity-read dir-b)))
		(test-niwa-execution-remote--stage "identity read complete")
		(setq session (niwa-session-create "alias" workspace-a dir-a))
		(test-niwa-execution-remote--stage "probe alias A")
		(let ((target (niwa-session-execution-target session)))
		  (should
		   (eq 'ready
		       (plist-get
			(niwa-execution-target-probe target t 'off)
			:status)))
		  (puthash (niwa-execution-target-identity target) t
			   niwa-session-durability--disclosed-targets))
		(test-niwa-execution-remote--stage "probe alias A complete")
		(with-current-buffer buffer
		  (org-mode)
		  (setq-local niwa--workspace workspace-a
			      niwa--session session)
		  (insert "Alias transcript\n")
		  (niwa-session-artifacts-save session buffer)
		  (set-buffer-modified-p nil))
		(test-niwa-execution-remote--stage "save alias A complete")
		(niwa-session-durability-lease-release
		 (niwa-session-save-path session) session)
		(test-niwa-execution-remote--stage "release alias A complete")
		(niwa-workspace-clear-registry)
		(setq workspace-b
		      (niwa-workspace-get-or-create
		       'project dir-b dir-b "alias-b"))
		(let* ((remote-file-name-inhibit-cache t)
		       (_ (test-niwa-execution-remote--stage
			   "list alias B start"))
		       (listed
			(niwa-session-persistence-list-sessions workspace-b)))
		  (test-niwa-execution-remote--stage "list alias B complete: %S"
							listed)
		  (unless
		      (cl-find (niwa-session-session-id session) listed
			       :key (lambda (item)
				      (plist-get (plist-get item :summary)
						 :session-id))
			       :test #'equal)
		    (let* ((sessions-dir
			    (niwa-session-artifacts-sessions-dir
			     workspace-b))
			   (entries
			    (directory-files
			     sessions-dir t
			     "\\`\\(?:[^.]\\|\\.niwa-save-as-\\)"))
			   (diagnostics
			    (mapcar
			     (lambda (entry)
			       (condition-case err
				   (let* ((publication
					   (niwa-session-publication-read
					    entry))
					  (sidecar
					   (plist-get publication :sidecar))
					  (summary
					   (and sidecar
						(niwa-session-persistence--read-summary
						 sidecar))))
				     (list entry publication sidecar summary))
				 (error
				  (list entry :error
					(error-message-string err)))))
			     entries)))
		      (ert-fail
		       (format "Alias-B listing empty: sessions-dir=%S entries=%S diagnostics=%S"
			       sessions-dir entries diagnostics)))))
		(let ((niwa-session-durability--disclosed-targets
		       niwa-session-durability--disclosed-targets))
		  (test-niwa-execution-remote--stage
		   "disclose alias B target start")
		  (puthash
		   (niwa-execution-target-identity
		    (niwa-execution-target-create dir-b)) t
		   niwa-session-durability--disclosed-targets))
		(test-niwa-execution-remote--stage
		 "disclose alias B target complete")
		(with-current-buffer buffer
		  (setq-local niwa--workspace workspace-a)
		  (cl-letf (((symbol-function 'read-string)
			     (lambda (&rest _) "alias-child")))
		    (test-niwa-execution-remote--stage "save-as start")
		    (setq parent-id (niwa-session-session-id session))
		    (niwa-save-session t)))
		(setq child-id (niwa-session-session-id session))
		(should (not (equal parent-id child-id)))
		(test-niwa-execution-remote--stage "save-as complete")
		(let ((remote-file-name-inhibit-cache t))
		  (test-niwa-execution-remote--stage "list children start")
		  (should (= 2 (length
				(niwa-session-persistence-list-sessions
				 workspace-b)))))
		(test-niwa-execution-remote--stage "list children complete")
		;; Reopen the Save As child through the other SSH alias.  Killing
		;; the current child first ensures this is a cold, independent
		;; restore rather than a live-buffer lookup.
		(niwa-session-durability-lease-release
		 (niwa-session-save-path session) session)
		(with-current-buffer buffer (set-buffer-modified-p nil))
		(kill-buffer buffer)
		(setq buffer nil)
		(niwa-workspace-clear-registry)
		(let* ((workspace-b
			(niwa-workspace-get-or-create
			 'project dir-b dir-b "alias-b"))
		       (resumed
			(niwa-session-persistence-resume-id
			 workspace-b child-id)))
		  (should (buffer-live-p resumed))
		  (with-current-buffer resumed
		    (should (equal child-id
				   (niwa-session-session-id niwa--session)))
		    (should (file-remote-p default-directory))
		    (should
		     (equal
		      (niwa-execution-target-identity
		       (niwa-session-execution-target niwa--session))
		      (niwa-execution-target-identity
		       (niwa-execution-target-create dir-b)))))
		  (let ((resumed-session
			 (buffer-local-value 'niwa--session resumed)))
		    (niwa-session-durability-lease-release
		     (niwa-session-save-path resumed-session) resumed-session))
		  (with-current-buffer resumed (set-buffer-modified-p nil))
		  (kill-buffer resumed)))
	    (when (and session (niwa-session-save-path session))
	      (ignore-errors
		(niwa-session-durability-lease-release
		 (niwa-session-save-path session) session)))
	    (when (buffer-live-p buffer)
	      (with-current-buffer buffer (set-buffer-modified-p nil))
	      (kill-buffer buffer))
	    (ignore-errors
	      (when (file-directory-p dir-a)
		(delete-directory dir-a t)))))))))

(defun test-niwa-execution-remote--reclaim-lease (session &optional timeout)
  "Take SESSION's portable lease back after its transfer clients exit.

The cooperative handoff ends in a directed release whose fence reserves the
next claim for the requester, and the requester is gone by the time this
returns, so a journey that simply mutates again races the fence: it is
refused while the fence is live and admitted once it lapses.  Waiting for
this client's own claim is what a person does with `niwa-take-control',
and it is the only thing that makes the next mutation deterministic.  An
expired foreign lease confirms its takeover, which no batch client can
answer, so the known-dead clients are confirmed away."
  (let ((deadline (+ (float-time) (or timeout 120))))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (while (not (or (niwa-session-durability-lease-owned-p session)
                      (niwa-session-durability-lease-acquire
                       (niwa-session-save-path session)
                       "acceptance-reclaim" session)))
        (when (> (float-time) deadline)
          (ert-fail
           (format "Journey never reclaimed its session lease from %S"
                   (niwa-session-durability--lease-head
                    (niwa-session-durability--lease-path
                     (niwa-session-save-path session))))))
        (accept-process-output nil 0.2)))))

(defun test-niwa-execution-remote--run-transfer-clients
    (root session-id &optional scenario)
  "Run independent owner and requester Emacsen against ROOT SESSION-ID.
SCENARIO is `transfer', `crash-long', or `recovery'."
  (let* ((client-file
          (file-name-concat
           test-niwa-execution-remote--directory
           "test-niwa-execution-remote-client.el"))
         ;; Dependencies are already provisioned.  `test ert' refreshes remote
         ;; package archives before loading this fixture; use installed packages
         ;; while explicitly retaining the project's per-child HOME/XDG hook.
         (common (list "npx" "@emacs-eask/cli" "emacs" "--batch"
                       "--eval"
                       (prin1-to-string
                        '(progn
                           (unless (and (boundp 'eask-before-test/ert-hook)
                                        (memq 'niwa-eask--isolate-test-home
                                              eask-before-test/ert-hook)
                                        (fboundp 'niwa-eask--isolate-test-home))
                             (error "Eask test isolation hook is unavailable"))
                           (run-hooks 'eask-before-test/ert-hook)))
                       "-l" client-file "-f" "ert-run-tests-batch-and-exit"))
         (owner-log (generate-new-buffer " *niwa-remote-owner-log*"))
         (requester-log
          (generate-new-buffer " *niwa-remote-requester-log*"))
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
           (let ((default-directory
                  (file-name-directory
                   (directory-file-name test-niwa-execution-remote--directory)))
                 (process-environment (copy-sequence process-environment)))
             ;; Eask must discover this checkout, not the case's private cwd.
             (setenv "PKG_EXECPATH" nil)
             (setenv "NIWA_REMOTE_CLIENT_ROLE" role)
             (setenv "NIWA_REMOTE_CLIENT_ID" id)
             (setenv "NIWA_REMOTE_CLIENT_ROOT" root)
             (setenv "NIWA_REMOTE_SESSION_ID" session-id)
             (setenv "NIWA_REMOTE_CLIENT_SCENARIO"
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
                   "niwa-remote-owner" owner-log))
            (wait-stage owner owner-log "owner-marker-a-owner"
                        "Owner client did not establish its lease")
            (setq requester
                  (start-client
                   "requester"
                   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                   "niwa-remote-requester" requester-log))
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

(defun test-niwa-execution-remote--exercise-transfer-only
    (variable method)
  "Exercise only independent control transfer for VARIABLE and METHOD."
  (let* ((base (test-niwa-execution-remote--real-root variable method))
         (root (test-niwa-execution-remote--real-temp-directory
                base (format "niwa-%s-transfer-" method)))
         (workspace (test-niwa-execution--workspace root))
         (session (niwa-session-create "transfer" workspace root))
         (buffer (generate-new-buffer " *remote-transfer-only*")))
    (unwind-protect
        (let ((niwa-sandbox-mode 'off)
              (niwa-session-lease-seconds 600))
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--workspace workspace
                        niwa--session session)
            (setq default-directory root)
            (niwa-session-set-root-buffer session buffer)
            (test-niwa-execution-remote--accept-storage session)
            (should
             (eq 'ready
                 (plist-get
                  (niwa-execution-target-probe
                   (niwa-session-execution-target session) t 'off)
                  :status)))
            (insert "Transfer-only transcript\n")
            (should (niwa-session-artifacts-save session buffer t))
            (set-buffer-modified-p nil))
          (niwa-session-durability-lease-release
           (niwa-session-save-path session) session)
          (test-niwa-execution-remote--run-transfer-clients
           root (niwa-session-session-id session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors
        (when (niwa-session-durability-lease-owned-p session)
          (niwa-session-durability-lease-release
           (niwa-session-save-path session) session)))
      (when (file-exists-p root)
        (delete-directory root t)))))

(defun test-niwa-execution-remote--exercise-transfer-recovery
    (variable method)
  "Exercise crashed-client takeover and target-side recovery with METHOD."
  (let* ((base (test-niwa-execution-remote--real-root variable method))
         (root (test-niwa-execution-remote--real-temp-directory
                base (format "niwa-%s-client-recovery-" method)))
         (workspace (test-niwa-execution--workspace root))
         (session (niwa-session-create "recovery" workspace root))
         (buffer (generate-new-buffer " *remote-client-recovery*")))
    (unwind-protect
        (let ((niwa-sandbox-mode 'off)
              (niwa-session-lease-seconds 600))
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--workspace workspace
                        niwa--session session
                        default-directory root)
            (niwa-session-set-root-buffer session buffer)
            (test-niwa-execution-remote--accept-storage session)
            (should
             (eq 'ready
                 (plist-get
                  (niwa-execution-target-probe
                   (niwa-session-execution-target session) t 'off)
                  :status)))
            (insert "Independent client recovery transcript\n")
            (niwa-session-artifacts-save session buffer t)
            (set-buffer-modified-p nil))
          (niwa-session-durability-lease-release
           (niwa-session-save-path session) session)
          (test-niwa-execution-remote--run-transfer-clients
           root (niwa-session-session-id session) "crash-long")
          (test-niwa-execution-remote--run-transfer-clients
           root (niwa-session-session-id session) "recovery"))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (ignore-errors
        (when (niwa-session-durability-lease-owned-p session)
          (niwa-session-durability-lease-release
           (niwa-session-save-path session) session)))
      (when (file-exists-p root)
        (delete-directory root t)))))

(niwa-deftest niwa-real-remote-alias-only
  (:quiet t :tags (external remote aliases)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "runs only the two-alias publication and Save As acceptance"
  (if (getenv "NIWA_TEST_REMOTE_ALIASES_ONLY")
      (test-niwa-execution-remote--exercise-ssh-aliases)
    (ert-skip "Alias-only acceptance is not enabled")))

(niwa-deftest niwa-real-remote-transfer-only
  (:quiet t :tags (external remote control-transfer)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "runs only the independent SSH control-transfer acceptance"
  (test-niwa-execution-remote--exercise-transfer-only
   "NIWA_TEST_SSH_ROOT" 'ssh))

(niwa-deftest niwa-real-remote-client-recovery-only
  (:quiet t :tags (external remote control-transfer recovery)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "runs crashed-client takeover and second-client recovery on SSH"
  (test-niwa-execution-remote--exercise-transfer-recovery
   "NIWA_TEST_SSH_ROOT" 'ssh))

(defun test-niwa-execution-remote--exercise-volume-replacement
    (root session buffer workspace method)
  "Replace the Docker-compatible or Podman target over its persistent volume."
  (let* ((runtime (if (eq method 'podman) "podman" "docker"))
         (volume (getenv (if (eq method 'podman)
                             "NIWA_TEST_PODMAN_VOLUME"
                           "NIWA_TEST_DOCKER_VOLUME")))
         (image (getenv (if (eq method 'podman)
                            "NIWA_TEST_PODMAN_IMAGE"
                          "NIWA_TEST_DOCKER_IMAGE")))
         (container (file-remote-p root 'host 'never)))
    (unless (and volume image container)
      (ert-skip
       (format "%s persistent-volume fixture is not configured" method)))
    (with-current-buffer buffer
      (niwa-session-artifacts-save session buffer t)
      (set-buffer-modified-p nil))
    (let* ((old-incarnation
            (niwa-execution-target-incarnation
             (niwa-session-execution-target session)))
           (grant (file-name-concat root "persisted-grant.txt")))
      (write-region "grant\n" nil grant nil 'silent)
      (niwa-permission-add-session-resource-grant session grant 'read)
      (with-current-buffer buffer
        (niwa-session-artifacts-save session buffer t)
        (set-buffer-modified-p nil))
      (niwa-session-durability-lease-release
       (niwa-session-save-path session) session)
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
      (niwa-workspace-clear-registry)
      (let* ((fresh-workspace
              (niwa-workspace-get-or-create
               'project (niwa-workspace-id workspace)
               (niwa-workspace-root workspace)
               (niwa-workspace-name workspace)))
             (restored
              (niwa-session-persistence-resume-id
               fresh-workspace (niwa-session-session-id session)))
             (new-session (buffer-local-value 'niwa--session restored))
             (new-target (niwa-session-execution-target new-session)))
        (should (buffer-live-p restored))
        ;; Resume with an acquired lease settles the replacement as one
        ;; transaction: it probes the fresh target, revokes exact
        ;; target-scoped grants, commits a sidecar naming the new
        ;; incarnation, and acknowledges the observation.  The restored
        ;; session therefore carries the new incarnation with no pending
        ;; replacement and no surviving grants.
        (should-not
         (niwa-execution-target-incarnation-changed-p new-target))
        (should-not (equal old-incarnation
                           (niwa-execution-target-incarnation new-target)))
        (should-not (niwa-session-resource-grants new-session))
        ;; A fresh probe of the same replacement target observes no
        ;; further change.
        (should (eq 'ready
                    (plist-get
                     (niwa-execution-target-probe new-target t 'off)
                     :status)))
        (should-not
         (niwa-execution-target-incarnation-changed-p new-target))
        ;; The committed sidecar records the new incarnation, so a later
        ;; client resumes against the acknowledged replacement.
        (let* ((publication
                (niwa-session-publication-read
                 (niwa-session-save-path new-session)))
               (sidecar
                (niwa-session-persistence-load-sidecar
                 (plist-get publication :sidecar))))
          (should (equal (niwa-execution-target-incarnation new-target)
                         (plist-get sidecar :target-incarnation))))
        (with-current-buffer restored
          (should (niwa-request-p (niwa-request-begin new-session)))
          (niwa-request-end t)
          (set-buffer-modified-p nil))
        (cons restored new-session)))))

(defun test-niwa-execution-remote--exercise-reconnect (target)
  "Close and reconnect TARGET, then assert a fresh ready connection."
  (let* ((root (niwa-execution-target-workspace-root target))
         (before (niwa-execution-target-connection-process target)))
    (should (processp before))
    (tramp-cleanup-connection
     (tramp-dissect-file-name root) nil t)
    (should-not (process-live-p before))
    (let* ((readiness (niwa-execution-target-probe target nil 'off))
           (after (niwa-execution-target-connection-process target)))
      (should (eq 'ready (plist-get readiness :status)))
      (should (process-live-p after))
      (should-not (eq before after)))))

(defmacro test-niwa-execution-remote--with-lost-connection (root &rest body)
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

(defun test-niwa-execution-remote--read-group-identity (path)
  "Return the numeric process identity fields stored at PATH."
  (mapcar
   #'string-to-number
   (split-string
    (string-trim
     (with-temp-buffer
       (insert-file-contents path)
       (buffer-string))))))

(defun test-niwa-execution-remote--cleanup-target-group
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
     "niwa-loss-cleanup"
     (number-to-string child-pid)
     (number-to-string group-id)
     (number-to-string child-start-time))))

(defun test-niwa-execution-remote--exercise-connection-loss
    (variable method)
  "Exercise unprovable connection loss for VARIABLE using METHOD."
  (let* ((base
          (test-niwa-execution-remote--real-root variable method))
         (root
          (test-niwa-execution-remote--real-temp-directory
           base (format "niwa-%s-loss-" method)))
         (identity-file (file-name-concat root "lost-group.identity"))
         session target root-buffer group-id child-pid
         child-start-time)
    (unwind-protect
        (let* ((niwa-sandbox-mode 'off)
               ;; Real TRAMP setup can synchronously block timer dispatch for
               ;; longer than the production lease window.  Keep this one
               ;; sequential acceptance owner alive across unrelated methods.
               (niwa-session-lease-seconds 600)
               (workspace (test-niwa-execution--workspace root))
               (_ (setq session
                        (niwa-session-create "main" workspace root)))
               (_ (setf (niwa-session-sandbox-mode session) 'off))
               (_ (setq target
                        (niwa-session-execution-target session)))
               (readiness
                (niwa-execution-target-probe target t 'off))
               initial execution-id result)
          (setq root-buffer (generate-new-buffer " *remote-loss*"))
          (with-current-buffer root-buffer
            (setq-local niwa--workspace workspace
                        niwa--session session)
            (setq default-directory root))
          ;; Portable mutation admission needs the session's registered
          ;; root data buffer before anything can materialize it.
          (niwa-session-set-root-buffer session root-buffer)
          (test-niwa-execution-remote--accept-storage session)
          (should (eq 'ready (plist-get readiness :status)))
          (setq
           initial
           (test-niwa-execution--start-managed
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
             "niwa-loss-workload" identity-file)
            :owner "/root"
            :yield-time-ms 250)
           execution-id
           (plist-get (plist-get initial :facts) :execution-id))
          (should-not (plist-get initial :error))
          (should (stringp execution-id))
          (unless (string-match-p "ready" (plist-get initial :output))
            (setq initial
                  (test-niwa-execution--observe
                   session execution-id :owner "/root" :wait-ms 3000)))
          (should (string-match-p "ready" (plist-get initial :output)))
          (test-niwa-execution-remote--wait
           (lambda () (file-readable-p identity-file))
           "Remote loss workload did not publish its identity")
          (pcase-let
              ((`(,group ,child ,start)
                (test-niwa-execution-remote--read-group-identity
                 identity-file)))
            (setq group-id group
                  child-pid child
                  child-start-time start))
          (should (> group-id 0))
          (should (> child-pid 0))
          (should (> child-start-time 0))
          (test-niwa-execution-remote--with-lost-connection root
                                                               (niwa-execution-stop
                                                                session "/root" execution-id
                                                                (lambda (value) (setq result value)))
                                                               (test-niwa-execution-remote--wait
                                                                (lambda () result)
                                                                "Connection-loss execution did not settle" 30))
          (should
           (eq 'unknown
               (plist-get (plist-get result :facts) :termination)))
          (should (niwa-execution-mutation-blocked-p session))
          (should
           (eq 'ready
               (plist-get
                (niwa-execution-target-probe target t 'off)
                :status)))
          (should-error
           (niwa-execution-start-bash
            #'ignore :session session :owner "/root"
            :command '("bash" "-c" "true")
            :workdir root :writable-roots (list root)
            :yield-time-ms nil)
           :type 'niwa-execution-error)
          (when (niwa-session-pending-publication session)
            (should
             (niwa-session-publication-retry session)))
          (niwa-execution-acknowledge-unknown session)
          (should
           (zerop
            (test-niwa-execution-remote--cleanup-target-group
             root group-id child-pid child-start-time)))
          (test-niwa-execution-remote--wait
           (lambda ()
             (test-niwa-execution-remote--target-process-gone-p
              root child-pid))
           "Remote descendant survived connection-loss cleanup"))

      (when (and group-id child-pid child-start-time)
        (ignore-errors
          (test-niwa-execution-remote--cleanup-target-group
           root group-id child-pid child-start-time)))
      (when session
        (niwa-execution-teardown-session session)
        ;; Mutation admission materialized the session and acquired its
        ;; lease; releasing it also cancels the renewal timer that would
        ;; otherwise keep running target I/O into later journeys.
        (ignore-errors
          (when (and (niwa-session-save-path session)
                     (niwa-session-durability-lease-owned-p session))
            (niwa-session-durability-lease-release
             (niwa-session-save-path session) session))))
      (when (buffer-live-p root-buffer)
        (with-current-buffer root-buffer
          (set-buffer-modified-p nil))
        (kill-buffer root-buffer))
      (when (file-exists-p root)
        (delete-directory root t)))))

(defun test-niwa-execution-remote--exercise-term-kill (root)
  "Exercise target PGID capture, TERM/KILL escalation, and cleanup at ROOT."
  (let ((identity-file (file-name-concat root "term-group.identity"))
        (term-file (file-name-concat root "term.seen"))
        (niwa-sandbox-mode 'off)
        result group-id child-pid child-start-time parent-pid parent-start-time)
    (unwind-protect
        (progn
          (setq result
                (niwa-execution-run-one-shot
                 :name "niwa-real-remote-group"
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
                  "niwa-real-group" term-file identity-file)
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
                (test-niwa-execution-remote--read-group-identity
                 identity-file)))
            (setq group-id group
                  child-pid child
                  child-start-time child-start
                  parent-pid parent
                  parent-start-time parent-start))
          (should (> child-pid 0))
          (test-niwa-execution-remote--wait
           (lambda ()
             (test-niwa-execution-remote--target-process-gone-p
              root child-pid))
           "Remote descendant survived TERM/KILL escalation"))
      (when (and (null group-id) (file-readable-p identity-file))
        (ignore-errors
          (pcase-let
              ((`(,group ,child ,child-start ,parent ,parent-start)
                (test-niwa-execution-remote--read-group-identity
                 identity-file)))
            (setq group-id group
                  child-pid child
                  child-start-time child-start
                  parent-pid parent
                  parent-start-time parent-start))))
      (when (and group-id child-pid child-start-time)
        (ignore-errors
          (test-niwa-execution-remote--cleanup-target-group
           root group-id child-pid child-start-time)))
      (when (and group-id parent-pid parent-start-time)
        (ignore-errors
          (test-niwa-execution-remote--cleanup-target-group
           root group-id parent-pid parent-start-time))))))

(defun test-niwa-execution-remote--exercise-pty (session root)
  "Exercise streaming, PTY stdin, and Ctrl-C for SESSION at ROOT."
  (let ((niwa-sandbox-mode 'off)
        initial after-input final execution-id)
    (setq initial
          (test-niwa-execution--start-managed
           session root
           '("bash" "-c"
             "printf 'ready\\n'; IFS= read -r line; printf 'input=%s\\n' \"$line\"; while :; do sleep 1; done")
           :tty t :tool-args '(:command "real PTY acceptance")
           :yield-time-ms 250))
    (should-not (plist-get initial :error))
    ;; A TTY execution keeps the classic shared-channel spawn: the
    ;; remote pty comes from the connection, not from direct-async.
    (should-not (plist-get (plist-get initial :facts) :direct-async))
    (setq execution-id
          (plist-get (plist-get initial :facts) :execution-id))
    (should (stringp execution-id))
    (unless (string-match-p "ready" (plist-get initial :output))
      (setq initial
            (test-niwa-execution--observe
             session execution-id :wait-ms 3000)))
    (should (string-match-p "ready" (plist-get initial :output)))
    (should (eq 'running (plist-get (plist-get initial :facts) :state)))
    (setq after-input
          (test-niwa-execution--observe
           session execution-id :chars "hello\n" :wait-ms 3000))
    (should (string-match-p "input=hello" (plist-get after-input :output)))
    (setq final
          (test-niwa-execution--observe
           session execution-id :chars (string 3) :wait-ms 10000))
    (should (plist-get final :claimed-final-p))
    (should (eq 'completed (plist-get (plist-get final :facts) :state)))
    (should-not (zerop (plist-get (plist-get final :facts) :exit-code)))))

(defun test-niwa-execution-remote--exercise-concurrency
    (session root readable-file)
  "Read READABLE-FILE while root and agent commands run in SESSION at ROOT."
  (let* ((agent-context
          (niwa-agent-invocation-create (niwa-agent-default)))
         (_ (setf (niwa-agent-invocation-path agent-context)
                  "/root/acceptance"
                  (niwa-agent-invocation-parent-session agent-context)
                  session
                  (niwa-agent-invocation-transcript-status agent-context)
                  'running))
         (agent-owner
          (niwa-agent-invocation-require-path agent-context))
         (root-result
          (test-niwa-execution-remote--start-reader
           session root "/root" "root"))
         (agent-result
          (test-niwa-execution-remote--start-reader
           session root agent-owner "agent" agent-context))
         (root-id (plist-get (plist-get root-result :facts) :execution-id))
         (agent-id (plist-get (plist-get agent-result :facts) :execution-id)))
    (should (stringp root-id))
    (should (stringp agent-id))
    ;; Eligible pipe executions took the private channel, not a silent
    ;; fallback to the shared connection; container targets stay classic.
    (should (eq (niwa-execution-target-direct-async-capable-p
                 (niwa-session-execution-target session))
                (plist-get (plist-get root-result :facts) :direct-async)))
    (should
     (equal '("/root" "/root/acceptance")
            (sort
             (mapcar
              (lambda (entry) (plist-get entry :owner))
              (niwa-execution-list-user session))
             #'string<)))
    ;; Model a transport coalescing the final shell exec and an immediate
    ;; stdin write.  The old interactive shell can read ahead and discard that
    ;; input on exec.  A ready handshake must separate those two writes.
    (let (held-process held-command flush-timer intercepted)
      (unwind-protect
          (cl-letf* ((send (symbol-function 'process-send-string))
                     (make (symbol-function 'make-process))
                     ((symbol-function 'process-send-string)
                      (lambda (proc text)
                        (cond
                         ((and (equal (process-name proc) "niwa-hook")
                               (string-prefix-p "cd " text)
                               (string-match-p " exec " text))
                          (setq held-process proc held-command text intercepted t))
                         ((and held-command (eq proc held-process))
                          (let ((combined (concat held-command text)))
                            (setq held-command nil)
                            (funcall send proc combined)))
                         (t (funcall send proc text)))))
                     ((symbol-function 'make-process)
                      (lambda (&rest args)
                        (let ((proc (apply make args)))
                          (when (and held-command (eq proc held-process))
                            ;; Arm outside the TRAMP frame: its temporary timer
                            ;; list must not discard the probe's bounded flush.
                            (setq flush-timer
                                  (run-at-time
                                   0.1 nil
                                   (lambda ()
                                     (when held-command
                                       (let ((text held-command))
                                         (setq held-command nil)
                                         (funcall send proc text)))))))
                          proc))))
            (should
             (string-match-p
              "acceptance needle"
              (test-niwa-execution-remote--run-tool
               session "Read" (list :file_path readable-file))))
            (should intercepted))
        (when flush-timer (cancel-timer flush-timer))))
    (should (= 2 (niwa-execution-count-user session)))))

(defun test-niwa-execution-remote--exercise-transport (variable method)
  "Exercise the real transport from VARIABLE using expected METHOD."
  (let* ((base
          (test-niwa-execution-remote--real-root variable method))
         (root
          ;; The transport journey replaces its target over the persistent
          ;; project volume, so its workspace has to live there.
          (test-niwa-execution-remote--real-temp-directory
           base (format "niwa-%s-acceptance-" method) t))
         session root-buffer)
    (test-niwa-execution-remote--with-temporary-directory root
      (unwind-protect
	  (let* ((niwa-sandbox-mode 'off)
		 (workspace (test-niwa-execution--workspace root))
		 (_ (setq session (niwa-session-create "main" workspace root)))
		 (target (niwa-session-execution-target session)))
	    (setq root-buffer (generate-new-buffer " *remote-acceptance*"))
	    (with-current-buffer root-buffer
	      ;; A session's root data buffer is a prepared transcript, not a
	      ;; bare Org buffer: hidden audit provenance only survives a save
	      ;; through the bounds `gptel-mode' persists.
	      (niwa-chat-prepare-transcript-buffer)
	      (setq-local niwa--workspace workspace
			  niwa--session session)
	      (setq default-directory root))
	    ;; Portable mutation admission needs the session's registered
	    ;; root data buffer before anything can materialize it.
	    (niwa-session-set-root-buffer session root-buffer)
	    (test-niwa-execution-remote--accept-storage session)
	    (setf (niwa-session-permission-mode session) 'full-auto
		  (niwa-session-sandbox-mode session) 'off)
	    (should (eq method (niwa-execution-target-method target)))
	    (should (eq method
			(plist-get (niwa-execution-target-identity target)
				   :method)))
	    (should (niwa-execution-target-supported-p target))
	    (let ((readiness
		   (niwa-execution-target-probe target t 'off)))
	      (should (eq 'ready (plist-get readiness :status)))
	      (should (equal "Linux"
			     (plist-get readiness :operating-system)))
	      (should (assoc "HOME"
			     (niwa-execution-target-environment target)))
	      (dolist (capability '(rg bash setsid))
		(should
		 (stringp
		  (niwa-execution-target-capability target capability)))))
	    (when (eq method 'ssh)
	      (message "niwa: real remote SSH ordinary entry")
	      (test-niwa-execution-remote--exercise-ordinary-entry
	       root workspace))
	    (test-niwa-execution-remote--exercise-reconnect target)
	    (message "niwa: real remote %s file tools" method)
	    (let ((file
		   (test-niwa-execution-remote--exercise-file-tools
		    session target root)))
	      (message "niwa: real remote %s Bash/Eval/resources/skills" method)
	      (test-niwa-execution-remote--exercise-tools-and-resources
	       session root root-buffer)
	      (test-niwa-execution-remote--exercise-agent-directory
	       session root root-buffer)
	      (test-niwa-execution-remote--exercise-project-hook
	       session root)
	      (message "niwa: real remote %s hook provenance" method)
	      (test-niwa-execution-remote--exercise-hook-provenance
	       session workspace root)
	      (message "niwa: real remote %s control filesystem" method)
	      (test-niwa-execution-remote--exercise-control-filesystem root)
	      (message "niwa: real remote %s publication/media" method)
	      (test-niwa-execution-remote--exercise-publication-media
	       session root-buffer)
	      (message "niwa: real remote %s publication/recovery" method)
	      (test-niwa-execution-remote--exercise-publication-recovery
	       session root-buffer)
	      (when (memq method '(ssh docker podman))
		(message "niwa: real remote %s worktree fork" method)
		(test-niwa-execution-remote--exercise-worktree-fork
		 session root-buffer root)
		(when (eq method 'ssh)
		  (message "niwa: real remote SSH Plan Worktree/Git gap")
		  (test-niwa-execution-remote--exercise-plan-worktree-entry
		   session root)))
	      (message "niwa: real remote %s process/pty/concurrency" method)
	      (test-niwa-execution-remote--exercise-term-kill root)
	      (test-niwa-execution-remote--exercise-pty session root)
	      (test-niwa-execution-remote--exercise-concurrency
	       session root file)
	      (when (eq method 'ssh)
		(message "niwa: real remote SSH aliases" )
		(test-niwa-execution-remote--exercise-ssh-aliases))
	      (when (memq method '(ssh docker podman))
		(message "niwa: real remote %s transfer clients" method)
		(niwa-session-durability-lease-release
		 (niwa-session-save-path session) session)
		(test-niwa-execution-remote--run-transfer-clients
		 root (niwa-session-session-id session))
		(test-niwa-execution-remote--reclaim-lease session))
	      (when (memq method '(docker podman))
		(message "niwa: real remote %s volume replacement" method)
		(pcase-let ((`(,new-buffer . ,new-session)
			     (test-niwa-execution-remote--exercise-volume-replacement
			      root session root-buffer workspace method)))
		  (setq root-buffer new-buffer
			session new-session)))
	      (setq root-buffer
		    (progn
		      (message "niwa: real remote %s cold entry" method)
		      (test-niwa-execution-remote--exercise-cold-entry
		       session root-buffer workspace)))))
	;; Replacement and cold entry rebind the buffer to fresh session
	;; objects.  Each holds its own lease and renewal timer, so the live
	;; one must be released here or its timer keeps running target I/O
	;; into every later journey in this Emacs process.
	(let ((live-session
	       (and (buffer-live-p root-buffer)
		    (buffer-local-value 'niwa--session root-buffer))))
	  (dolist (entry (delete-dups (delq nil (list session live-session))))
	    (niwa-execution-teardown-session entry)
	    (ignore-errors
	      (when (and (niwa-session-save-path entry)
			 (niwa-session-durability-lease-owned-p entry))
		(niwa-session-durability-lease-release
		 (niwa-session-save-path entry) entry)))))
	(when (buffer-live-p root-buffer)
	  (with-current-buffer root-buffer
	    (set-buffer-modified-p nil))
	  (kill-buffer root-buffer))
	(when (file-exists-p root)
	  (delete-directory root t))))))

(defun test-niwa-execution-remote--exercise-bwrap (variable method)
  "Exercise an exact target Bubblewrap grant for VARIABLE and METHOD."
  (let* ((base
          (test-niwa-execution-remote--real-root variable method))
         (root
          (test-niwa-execution-remote--real-temp-directory
           base (format "niwa-%s-bwrap-root-" method)))
         (external-root
          (test-niwa-execution-remote--real-temp-directory
           base (format "niwa-%s-bwrap-grant-" method)))
         (target (niwa-execution-target-create root))
         (secret (file-name-concat external-root "secret.txt"))
         (link (file-name-concat external-root "secret-link"))
         (sibling (file-name-concat external-root "sibling.txt"))
         (niwa-protected-paths
          `((,(concat external-root "**") . inaccessible)))
         (niwa-sandbox-mode 'required))
    (unwind-protect
        (progn
          (let ((readiness
                 (niwa-execution-target-probe target t 'required)))
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
          ;; The same remote child boundary follows the selected permission
          ;; mode even when the stored sandbox preference says the opposite.
          (dolist (mode '(edits full-auto))
            (let* ((session (niwa-session--create
                             :execution-target target
                             :working-directory root :permission-mode mode
                             :sandbox-mode (if (eq mode 'edits) 'off 'required)))
                   (result (niwa-execution-run-one-shot
                            :session session :name "remote-permission-mode"
                            :command (list "bash" "-c"
                                           (if (eq mode 'edits)
                                               "test ! -r \"$1\"" "cat \"$1\"")
                                           "mode-probe" secret)
                            :workdir root :writable-roots (list root) :timeout 30)))
              (should-not (plist-get result :error))
              (should (= 0 (plist-get result :exit-code)))
              (should (eq (if (eq mode 'edits) 'bubblewrap 'off)
                          (plist-get (plist-get result :sandbox-facts) :sandbox)))
              (when (eq mode 'full-auto)
                (should (equal "secret" (plist-get result :output))))))
          (let ((result
                 (niwa-execution-run-one-shot
                  :name "niwa-real-remote-bwrap"
                  :command
                  (list
                   "bash" "-c"
                   (concat
                    "cat \"$1\"; "
                    "if test -r \"$2\"; then exit 71; fi; "
                    "while read -r _ destination _; do "
                    "test \"$destination\" != 00000000 || exit 72; "
                    "done < /proc/net/route")
                   "niwa-real-bwrap" link sibling)
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

(niwa-deftest niwa-real-remote-global-permissions
  (:quiet t :tags (external remote)
          :after-each ((tramp-cleanup-all-connections)))
  (let* ((base (test-niwa-execution-remote--real-root ,variable ,method))
         (root (test-niwa-execution-remote--real-temp-directory
                base "niwa-global-permissions-" t))
         (niwa-user-dir (make-temp-file "niwa-client-permissions-" t))
         (session (test-niwa-execution--session root))
         (workspace (niwa-session-workspace session))
         (target (niwa-session-execution-target session))
         (buffer (generate-new-buffer " *global-permissions-owner*"))
         global-file global-created global-dir-created)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--session session default-directory root)
          (should (eq 'ready (plist-get
                              (niwa-execution-target-probe target t 'off)
                              :status)))
          (setq global-file (niwa-permission-persistence-file workspace 'global))
          ;; This journey uses disposable target accounts, never overwrites
          ;; an existing user configuration, and removes its own file on exit.
          (when (file-exists-p global-file)
            (ert-skip "Global permission journey requires an empty target store"))
          (setq global-dir-created (not (file-directory-p
                                         (file-name-directory global-file))))
          (push (file-name-directory global-file) niwa-test--write-roots)
          (make-directory (file-name-directory global-file) t)
          (should (niwa-session-durability-lease-acquire
                   (niwa-session-save-path session) "main" session))
          (niwa-permission-persistence-write-store
           (file-name-concat niwa-user-dir "permissions.el")
           '(:rules (("Read" :action deny)) :resource-grants nil))
          (setq global-created t)
          (let* ((path (niwa-execution-target-expand-path target "~/.npm"))
                 (grant (list :path path :access 'write :recursive t))
                 (rule '("Bash" :pattern "echo *" :action allow))
                 (store (list :rules (list rule) :resource-grants (list grant)))
                 (context (list :view-buffer buffer :data-buffer buffer
                                :session session :workspace workspace)))
            (niwa-permission-persistence-write-store global-file store target)
            (niwa-permission-persistence-save-resource-grant
             workspace path 'write t)
            (niwa-permission-validate-persistent-stores workspace)
            (should (equal (list rule)
                           (niwa-permission-persistence-load-rules workspace)))
            (should (equal (list grant grant)
                           (niwa-permission-persistence-load-resource-grants
                            workspace)))
            (save-window-excursion
              (with-current-buffer (niwa-permissions-list-open context)
                (dolist (kind '("operation" "resource"))
                  (goto-char (point-min))
                  (while (not (or (eobp)
                                  (when-let* ((entry (tabulated-list-get-entry)))
                                    (and (equal (aref entry 0) "global")
                                         (equal (aref entry 1) kind)))))
                    (forward-line 1))
                  (should-not (eobp))
                  (niwa-permissions-list-revoke))))
            (should (equal '(:rules nil :resource-grants nil)
                           (niwa-permission-persistent-authority workspace 'global)))
            (should (equal (list grant)
                           (niwa-permission-persistence-load-resource-grants
                            workspace)))))
      (when-let* ((cockpit (get-buffer niwa-permissions-list-buffer-name)))
        (kill-buffer cockpit))
      (when global-created (delete-file global-file))
      (when global-dir-created (delete-directory (file-name-directory global-file)))
      (niwa-session-durability-lease-release
       (niwa-session-save-path session) session)
      (kill-buffer buffer)
      (delete-directory root t)
      (delete-directory niwa-user-dir t)))
  (variable method)
  :doc "loads and revokes the SSH target's global authority through the cockpit"
  "NIWA_TEST_SSH_ROOT" 'ssh
  :doc "loads and revokes the Podman target's global authority through the cockpit"
  "NIWA_TEST_PODMAN_ROOT" 'podman)

(niwa-deftest niwa-real-remote-acceptance
  (:quiet t :tags (external remote)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "exercises the core opt-in real SSH transport matrix"
  (test-niwa-execution-remote--exercise-transport
   "NIWA_TEST_SSH_ROOT" 'ssh)
  :doc "exercises the core opt-in real Docker transport matrix"
  (test-niwa-execution-remote--exercise-transport
   "NIWA_TEST_DOCKER_ROOT" 'docker)
  :doc "exercises the core opt-in real Podman transport matrix"
  (test-niwa-execution-remote--exercise-transport
   "NIWA_TEST_PODMAN_ROOT" 'podman))

(niwa-deftest niwa-real-remote-group-status/deadline
  (:quiet t :tags (external remote connection-loss)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "a warm Podman group-status probe refuses before a paused target resumes"
  (let ((root (getenv "NIWA_TEST_PODMAN_ROOT")))
    (skip-unless root)
    (let* ((container (tramp-file-name-host (tramp-dissect-file-name root)))
           (default-directory temporary-file-directory)
           (group-id 9876543)
           paused release)
      (should (eq 'dead (niwa-execution-process-remote-group-status group-id "1" root)))
      (unwind-protect
          (progn
            (should (= 0 (call-process "podman" nil nil nil "pause" container)))
            (setq paused t)
            (setq release
                  (make-process :name "niwa-test-podman-unpause" :noquery t
                                :command (list "sh" "-c" "sleep 3; exec podman unpause \"$1\""
                                               "sh" container)
                                :sentinel #'ignore))
            (let* ((niwa-execution-process--remote-control-timeout 0.5)
                   (started (float-time))
                   (failure (should-error
                             (niwa-execution-process-remote-group-status group-id "1" root))))
              (should (equal "Target process status probe timed out"
                             (error-message-string failure)))
              (should (< (- (float-time) started) 1.5))
              (should (process-live-p release))))
        (when (and release (process-live-p release)) (delete-process release))
        (when paused (call-process "podman" nil nil nil "unpause" container)))
      (should (eq 'dead (niwa-execution-process-remote-group-status group-id "1" root))))))

(niwa-deftest niwa-real-remote-control-lock/orphan
  (:quiet t :tags (external remote connection-loss)
          :after-each ((tramp-cleanup-all-connections)))
  (let ((root (getenv "NIWA_TEST_PODMAN_ROOT")))
    (skip-unless root)
    (let* ((directory (directory-file-name (test-niwa-execution-remote--real-temp-directory root "control-lock-")))
           (local (file-local-name directory))
           (container (file-remote-p root 'host))
           (buffer (generate-new-buffer " *control lock fixture*"))
           client pid successor)
      (unwind-protect
          (progn
            ;; Reproduce the tty transport retaining stdin after client death.
            (setq client (make-process :name "test-podman-control-tty" :buffer buffer :noquery t
                                       :connection-type 'pipe :sentinel #'ignore
                                       :command (list "podman" "exec" "-it" container "bash" "-c"
                                                      "exec 7<\"$1\" && flock -x 7 && echo $$ && read -r reply"
                                                      "control-lock" local)))
            (with-current-buffer buffer
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-live-p client) (< (float-time) deadline)
                            (not (string-match "\\`[0-9]+[\r\n]" (buffer-string))))
                  (accept-process-output client .05)))
              (should (string-match "\\`\\([0-9]+\\)[\r\n]" (buffer-string)))
              (setq pid (match-string 1 (buffer-string))))
            (kill-process client)
            (while (process-live-p client) (accept-process-output client .05))
            (let ((failure (error-message-string
                            (should-error (niwa-session-control-fs-lock-session-open directory 2)
                                          :type 'file-error))))
              (should (string-search (format "holder PID %s on %s" pid container) failure))
              (should (string-search directory failure)))
            ;; Signal only the independently recorded fixture PID, still on the tested inode.
            (should (zerop (call-process "podman" nil nil nil "exec" container "sh" "-c"
                                         "test /proc/\"$1\"/fd/7 -ef \"$2\" && kill -KILL \"$1\""
                                         "control-lock" pid local)))
            (setq successor (niwa-session-control-fs-lock-session-open directory 2))
            (niwa-session-control-fs-program-value
             (car (niwa-session-control-fs-lock-session-run
                   successor (list (list :op 'create :path (file-name-concat directory "slot") :content "applied")))))
            (should (equal "applied" (niwa-session-control-fs-read-file (file-name-concat directory "slot")))))
        (niwa-session-control-fs-lock-session-close successor)
        (when (and client (process-live-p client)) (delete-process client))
        (kill-buffer buffer)
        (delete-directory directory t)))))

(niwa-deftest niwa-real-remote-loss
  (:quiet t :tags (external remote connection-loss)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "classifies unprovable real SSH loss and cleans its descendant"
  (test-niwa-execution-remote--exercise-connection-loss
   "NIWA_TEST_SSH_ROOT" 'ssh)
  :doc "classifies unprovable real Docker loss and cleans its descendant"
  (test-niwa-execution-remote--exercise-connection-loss
   "NIWA_TEST_DOCKER_ROOT" 'docker)
  :doc "classifies unprovable real Podman loss and cleans its descendant"
  (test-niwa-execution-remote--exercise-connection-loss
   "NIWA_TEST_PODMAN_ROOT" 'podman))

(niwa-deftest niwa-real-remote-bwrap
  (:quiet t :tags (external remote sandbox)
          :after-each ((tramp-cleanup-all-connections)))
  ,test
  (test)
  :doc "confines an exact symlink grant through real SSH Bubblewrap"
  (test-niwa-execution-remote--exercise-bwrap
   "NIWA_TEST_SSH_ROOT" 'ssh)
  :doc "confines an exact symlink grant through real Docker Bubblewrap"
  (test-niwa-execution-remote--exercise-bwrap
   "NIWA_TEST_DOCKER_ROOT" 'docker)
  :doc "confines an exact symlink grant through real Podman Bubblewrap"
  (test-niwa-execution-remote--exercise-bwrap
   "NIWA_TEST_PODMAN_ROOT" 'podman))

(provide 'test-niwa-execution-remote)

;;; test-niwa-execution-remote.el ends here
