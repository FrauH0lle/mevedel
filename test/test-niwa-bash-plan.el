;;; test-niwa-bash-plan.el --- Confined Plan read-only inspection tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise Plan Bash through the provider-facing pipeline in real Bubblewrap.
;; Repository setup is local and disposable; no network or Git writes are
;; authorized to the tool calls under test.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'niwa-agents)
(require 'niwa-bash-analysis)
(require 'niwa-bash-git)
(require 'niwa-execution)
(require 'niwa-permission-queue)
(require 'niwa-pipeline)
(require 'niwa-plan-mode)
(require 'niwa-sandbox)
(require 'niwa-session-persistence)
(require 'niwa-tool-exec-registration)
(require 'niwa-tool-registry)
(require 'niwa-view)
(require 'niwa-workspace)

(niwa-deftest niwa-pipeline-run-tool/plan-git-inspection
  (:quiet t)
  (let* ((parser ,parser)
         (plan-kind ,plan-kind)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (format "Real Bubblewrap unavailable: %s"
                        (plist-get availability :reason))))
    (unless (executable-find "git")
      (ert-skip "Git executable unavailable"))
    (when (and (eq parser 'treesit)
               (not (treesit-language-available-p 'bash)))
      (ert-skip "Real Bash Tree-sitter grammar unavailable"))
    (let* ((root (file-name-as-directory (make-temp-file "niwa-plan-git-" t)))
           (git-dir (file-name-concat root ".git"))
           (tracked (file-name-concat root "tracked.txt"))
           (data-buf (generate-new-buffer " *test-plan-git-data*"))
           (view-buf (generate-new-buffer " *test-plan-git-view*"))
           (workspace (niwa-workspace-get-or-create 'project root root "workspace"))
           (session (niwa-session-create "main" workspace))
           (niwa-permission-rules nil)
           (niwa-permission-mode 'ask)
           (niwa-permission-reviewer 'user)
           (niwa-protected-paths '(("**/.git/**" . read-only)))
           (draft "> keep this draft\nand its second line")
           (grammar-available (symbol-function 'treesit-language-available-p))
           (invocation
            (when (eq plan-kind 'retained)
              (niwa-agent-invocation--create
               :agent-id "plan_git--1" :path "/root/plan_git"
               :parent-session session :parent-data-buffer data-buf
               :plan-read-only t)))
           baseline)
      (unwind-protect
          (cl-labels
              ((git (&rest args)
                 (let ((default-directory root))
                   (should (zerop (apply #'process-file "git" nil nil nil args)))))
               (metadata-bytes ()
                 ;; Include the index, refs, config, logs, objects and file set;
                 ;; comparing only HEAD would miss an optional index refresh.
                 (mapcar
                  (lambda (file)
                    (cons (file-relative-name file git-dir)
                          (with-temp-buffer
                            (set-buffer-multibyte nil)
                            (insert-file-contents-literally file)
                            (buffer-string))))
                  (sort (directory-files-recursively git-dir ".") #'string<)))
               (run (command)
                 (let (result)
                   (with-current-buffer data-buf
                     (niwa-pipeline-run-tool
                      (niwa-tool-get "Bash")
                      (lambda (value) (setq result value))
                      (list :command command :description "Inspect Git in Plan" :yield_time_ms 10000)))
                   (let ((deadline (+ (float-time) 15)))
                     (while (and (not result) (< (float-time) deadline))
                       (accept-process-output nil 0.01)))
                   (should (stringp result))
                   (should-not (niwa-session-permission-queue session))
                   (should-not (niwa-session-resource-grants session))
                   (let ((after (metadata-bytes)))
                     (ert-info
                         ((format "After %s; changed .git paths: %S"
                                  command
                                  (cl-remove-if
                                   (lambda (path)
                                     (equal (assoc path baseline) (assoc path after)))
                                   (delete-dups (append (mapcar #'car baseline)
                                                        (mapcar #'car after))))))
                       (should (equal baseline after))))
                   (with-temp-buffer
                     (insert-file-contents-literally tracked)
                     (should (equal "one\ntwo\n" (buffer-string))))
                   (should-not (file-exists-p (file-name-concat root "mutation")))
                   (should-not (file-exists-p (file-name-concat root "unsafe.patch")))
                   (with-current-buffer view-buf
                     (should (equal draft (niwa-view-composer-draft-text))))
                   result)))
            (git "init" "-q" "--initial-branch=main")
            (git "config" "maintenance.auto" "false")
            (git "config" "core.autocrlf" "false")
            (with-temp-file tracked (insert "one\n"))
            (git "add" "--" "tracked.txt")
            (git "-c" "user.name=Plan Fixture" "-c" "user.email=plan@example.invalid"
                 "commit" "--no-gpg-sign" "-qm" "baseline")
            (with-temp-file tracked (insert "one\ntwo\n"))
            (make-directory (file-name-concat root "tmp"))
            (setf (niwa-session-permission-mode session) 'ask
                  (niwa-session-sandbox-mode session) 'required
                  (niwa-session-plan-mode session) t)
            (niwa-tool-exec--register)
            (with-current-buffer data-buf
              (org-mode)
              (setq-local default-directory root
                          niwa--session session
                          niwa--workspace workspace
                          niwa--agent-invocation invocation
                          temporary-file-directory (file-name-concat root "tmp")))
            (niwa-view--setup view-buf data-buf)
            (with-current-buffer view-buf
              (niwa-view-test--insert-composer-draft draft 4))
            (setq baseline (metadata-bytes))
            (should-not (niwa-session-permission-rules session))
            (should-not (niwa-session-resource-grants session))
            (cl-letf
                (((symbol-function 'treesit-language-available-p)
                  (lambda (language &optional detail)
                    (if (and (eq language 'bash) (eq parser 'heuristic))
                        nil
                      (funcall grammar-available language detail))))
                 ((symbol-function 'niwa-permission-queue--render-entry)
                  (lambda (&rest _) (ert-fail "Plan inspection created a permission card"))))
              (dolist (spec '(("git status --short" " M tracked.txt")
                              ("git diff --stat" "1 file changed")
                              ("git status --short && git diff --stat" "1 file changed")
                              ("cmp -- tracked.txt tracked.txt" nil)
                              ("du -s -- tracked.txt" "tracked.txt\n")
                              ("test -f tracked.txt" nil)
                              ("cmp -- tracked.txt tracked.txt && du -s -- tracked.txt | cat && test -f tracked.txt && test -d tmp && echo inspected"
                               "inspected\n")
                              ("test -f missing.txt && echo should-not-run || echo fallback; test -d tmp && echo inspected"
                               "fallback\ninspected\n")))
                (let* ((command (car spec))
                       (analysis (niwa-bash-analysis-analyze command)))
                  (should (eq parser (plist-get analysis :parser)))
                  (should (eq 'read-only (plist-get analysis :class)))
                  (let ((result (run command)))
                    (when (cadr spec)
                      (should (string-match-p (regexp-quote (cadr spec)) result)))
                    (when (equal command "git status --short && git diff --stat")
                      (should (string-match-p " M tracked.txt" result)))
                    (when (equal command "du -s -- tracked.txt")
                      (should (string-match-p "^[0-9]+\ttracked\\.txt\n" result)))
                    (when (string-prefix-p "test -f missing.txt" command)
                      (should (string-prefix-p "fallback\ninspected\n" result)))
                    (should (string-match-p "sandbox: bubblewrap" result))
                    (should (string-match-p "network: isolated" result))
                    (should (string-match-p "exit_code=\"0\"" result)))))
              ;; The command attribute alone exceeds the provider result cap,
              ;; even though managed output already uses a bounded preview.
              ;; Observe the real persistence path and retained spool without
              ;; replacing either implementation or initializing the session.
              (let* ((niwa-pipeline--default-max-result-size 1000)
                     (payload (make-string 3000 ?x))
                     (command (format "echo '%s'" payload))
                     (analysis (niwa-bash-analysis-analyze command))
                     (persist (symbol-function 'niwa-pipeline--persist-result))
                     (retire (symbol-function 'niwa-execution--retire-terminal-record))
                     spool raw-result-size result)
                (should (eq parser (plist-get analysis :parser)))
                (should (eq 'read-only (plist-get analysis :class)))
                (should-not (niwa-session-save-path session))
                (cl-letf
                    (((symbol-function 'niwa-pipeline--persist-result)
                      (lambda (&rest args)
                        (setq raw-result-size (length (car args)))
                        (apply persist args)))
                     ((symbol-function 'niwa-execution--retire-terminal-record)
                      (lambda (record)
                        (when (equal command
                                     (plist-get
                                      (niwa-execution--origin-tool-args
                                       (niwa-execution--record-origin record))
                                      :command))
                          (setq spool (niwa-execution--spool-path record)))
                        (funcall retire record))))
                  (setq result (run command)))
                (should (integerp raw-result-size))
                (should (= 1000 niwa-pipeline--default-max-result-size))
                (should (> (length payload) niwa-execution-inline-output-limit))
                (should (> raw-result-size niwa-pipeline--default-max-result-size))
                (should (string-match-p "Output too large" result))
                (should-not (niwa-session-save-path session))
                (should (stringp spool))
                (should (file-exists-p spool))
                (should (file-in-directory-p spool (file-name-concat root "tmp")))
                (with-temp-buffer
                  (insert-file-contents-literally spool)
                  (should (equal (concat payload "\n") (buffer-string))))
                ;; The same real command stays inline without the lowered cap.
                ;; This distinguishes persistence from command/output failure.
                (let ((niwa-pipeline--default-max-result-size 50000))
                  (setq result (run command))
                  (should-not (string-match-p "Output too large" result))
                  (should (string-prefix-p (make-string 1000 ?x) result))
                  (should (string-match-p "omitted 1001 chars" result))))
              ;; A Plan denial must precede execution, not merely rely on the
              ;; protected .git mount rejecting a write after launch.
              (cl-letf (((symbol-function 'niwa-execution-start-bash)
                         (lambda (&rest _) (ert-fail "Denied Plan Bash reached launch"))))
                (dolist (command '("git reset --hard"
                                   "git branch unwanted"
                                   "git diff --output=unsafe.patch"
                                   "git diff --ext-diff"
                                   "git status --short && touch mutation"
                                   "cmp -- tracked.txt tracked.txt && touch mutation"))
                  (should-not (eq 'read-only
                                  (plist-get (niwa-bash-analysis-analyze command) :class)))
                  (should (string-prefix-p "Error:" (run command))))
                (setf (niwa-session-permission-rules session)
                      '(("Bash" :pattern "git diff:*" :action deny)))
                (should (string-prefix-p "Error:" (run "git diff --stat")))
                (should (string-prefix-p
                         "Error:" (run "git status --short && git diff --stat"))))))
        (niwa-permission-queue-abort-all session)
        (niwa-execution-teardown-session session)
        (remhash session niwa-execution--sessions)
        (when (buffer-live-p view-buf) (kill-buffer view-buf))
        (when (buffer-live-p data-buf) (kill-buffer data-buf))
        (delete-directory root t)
        (niwa-workspace-clear-registry))))
  (parser plan-kind)
  :doc "real Bubblewrap standalone Plan Git inspection uses the scanner"
  'heuristic 'standalone
  :doc "real Bubblewrap standalone Plan Git inspection uses Tree-sitter"
  'treesit 'standalone
  :doc "real Bubblewrap retained-parent Plan Git inspection uses the scanner"
  'heuristic 'retained
  :doc "real Bubblewrap retained-parent Plan Git inspection uses Tree-sitter"
  'treesit 'retained)

(niwa-deftest niwa-pipeline-run-tool/bash-mutation-owner-gate
  (:quiet t :doc "permitted mutating Bash outside Plan keeps execution-owned admission")
  (let* ((availability (niwa-sandbox-probe))
         (root (file-name-as-directory (make-temp-file "niwa-bash-owner-gate-" t)))
         (data-buf (generate-new-buffer " *test-bash-owner-gate*"))
         (workspace (niwa-workspace-get-or-create 'project root root "workspace"))
         (session (niwa-session-create "main" workspace))
         (niwa-permission-rules nil)
         (niwa-permission-mode 'ask)
         (niwa-permission-reviewer 'user)
         (start (symbol-function 'niwa-execution-start-bash))
         (gate (symbol-function 'niwa-session-artifacts-assert-new-mutation-authority))
         (gate-count 0)
         inside-start result)
    (unwind-protect
        (progn
          (unless (plist-get availability :available)
            (ert-skip (format "Real Bubblewrap unavailable: %s"
                              (plist-get availability :reason))))
          (make-directory (file-name-concat root "tmp"))
          (setf (niwa-session-permission-mode session) 'ask
                (niwa-session-sandbox-mode session) 'required
                (niwa-session-permission-rules session)
                '(("Bash" :pattern "touch mutation" :action allow)))
          (niwa-tool-exec--register)
          (with-current-buffer data-buf
            (org-mode)
            (setq-local default-directory root
                        niwa--session session
                        niwa--workspace workspace
                        temporary-file-directory (file-name-concat root "tmp"))
            (cl-letf
                (((symbol-function 'niwa-execution-start-bash)
                  (lambda (&rest args)
                    (setq inside-start t)
                    (unwind-protect (apply start args)
                      (setq inside-start nil))))
                 ((symbol-function 'niwa-session-artifacts-assert-new-mutation-authority)
                  (lambda (owner)
                    (should inside-start)
                    (should (eq owner session))
                    (cl-incf gate-count)
                    (funcall gate owner)))
                 ((symbol-function 'niwa-permission-queue--render-entry)
                  (lambda (&rest _) (ert-fail "Allowed mutation created a permission card"))))
              (niwa-pipeline-run-tool
               (niwa-tool-get "Bash")
               (lambda (value) (setq result value))
               '(:command "touch mutation" :description "Probe mutation owner gate" :yield_time_ms 10000))
              (let ((deadline (+ (float-time) 15)))
                (while (and (not result) (< (float-time) deadline))
                  (accept-process-output nil 0.01)))))
          (should (= 1 gate-count))
          (should (stringp result))
          (should (string-match-p "sandbox: bubblewrap" result))
          (should (string-match-p "exit_code=\"0\"" result))
          (should (file-exists-p (file-name-concat root "mutation")))
          (should-not (niwa-session-permission-queue session))
          (should-not (niwa-session-resource-grants session)))
      (niwa-permission-queue-abort-all session)
      (niwa-execution-teardown-session session)
      (remhash session niwa-execution--sessions)
      (when (buffer-live-p data-buf) (kill-buffer data-buf))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-bash-plan)
;;; test-niwa-bash-plan.el ends here
