;;; test-mevedel-tool-exec-profile-hint.el --- Compound profile diagnostics -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises profile diagnostics through registered Bash and real confinement.

;;; Code:

(require 'mevedel-tool-exec-registration)
(require 'mevedel-tool-exec)
(require 'mevedel-tools)
(require 'mevedel-pipeline)
(require 'mevedel-sandbox)
(require 'mevedel-execution)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-profile-hint-test--bash (command &rest args)
  "Run registered Bash COMMAND with ARGS and return its settled outcome.
Unless ARGS request a yield, a command still running at Bash's default yield
\(a loaded machine) is polled to completion, joining its output."
  (let ((run (lambda (tool tool-args)
               (let (result)
                 (mevedel-pipeline-run-tool-outcome
                  (mevedel-tool-ensure tool) (lambda (value) (setq result value)) tool-args)
                 (with-timeout (60 (ert-fail (format "Confined %s did not settle" tool)))
                   (while (null result) (accept-process-output nil 0.02)))
                 result))))
    (let ((result (funcall run "Bash" (append (list :command command
                                                    :description "Check compound profile diagnostic")
                                              args))))
      (unless (plist-member args :yield_time_ms)
        (while (eq 'running (plist-get (plist-get result :render-data) :state))
          (let ((next (funcall run "WriteStdin"
                               (list :execution_id (plist-get (plist-get result :render-data) :execution-id)
                                     :yield_time_ms 30000))))
            (setq result (plist-put next :result (concat (plist-get result :result) "\n"
                                                         (plist-get next :result)))))))
      result)))

(mevedel-deftest mevedel-tool-exec--bash/profile-hint
  (:quiet t :doc "confined segment-profile misses are diagnostic only, including after yield")
  (progn
    (let ((availability (mevedel-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((root (make-temp-file "mevedel-profile-hint-" t))
           (session (mevedel-skills-test--make-session "profile-hint" root))
           (metadata (file-name-concat root ".git"))
           (command "printf 'built\\n' | tail -1 && git add -- example.txt")
           (grant (list :path metadata :access 'write :recursive t))
           (rule (list "Bash" :pattern "git add *" :action 'allow
                       :file-system (list grant)))
           (mevedel-permission-rules nil)
           (mevedel-protected-paths '(("**/.git/**" . read-only))))
      (setf (mevedel-session-permission-mode session) 'edits
            (mevedel-session-sandbox-mode session) 'required
            (mevedel-session-permission-rules session) (list rule))
      (unwind-protect
          (with-temp-buffer
            (setq default-directory (file-name-as-directory root))
            (should (zerop (process-file "git" nil nil nil "init" "--quiet" root)))
            (with-temp-file (file-name-concat root "example.txt") (insert "contents"))
            (setq-local mevedel--session session
                        mevedel--workspace (mevedel-session-workspace session)
                        mevedel--current-request
                        (mevedel-request--create :session session :ephemeral-p t))
            (let* ((result (mevedel-profile-hint-test--bash command))
                   (text (plist-get result :result)))
              (should (eq 'error (plist-get result :status)))
              (should (string-search "sandbox: bubblewrap" text))
              (should (string-search "Read-only file system" text))
              (should-not (file-exists-p (file-name-concat metadata "index")))
              (should (string-search "Command-scoped filesystem profile not attached" text))
              (should (string-search "git add -- example.txt" text))
              (should (string-search "git add *" text))
              (should (string-search "separate Bash calls" text))
              (should-not (mevedel-session-permission-queue session)))
            ;; No profile, success, and unrelated errors never advertise a miss.
            (setf (mevedel-session-permission-rules session) nil)
            (let ((result (mevedel-profile-hint-test--bash command)))
              (should (eq 'error (plist-get result :status)))
              (should-not (string-search "profile not attached" (plist-get result :result))))
            (setf (mevedel-session-permission-rules session) (list rule))
            (dolist (other '("git --unknown && git add -- example.txt"
                             "false && git add -- example.txt"
                             "true || git add -- example.txt"))
              (let ((result (mevedel-profile-hint-test--bash other)))
                (should-not (string-search "profile not attached" (plist-get result :result)))))
            ;; The launch snapshot survives later rule removal and polling.
            (let* ((result
                    (mevedel-profile-hint-test--bash
                     (concat "bash -c 'until test -e ready; do sleep 0.02; done' && " command)
                     :yield_time_ms 250))
                   (id (plist-get (plist-get result :render-data) :execution-id))
                   settled)
              (should (stringp id))
              (should-not (string-search "profile not attached" (plist-get result :result)))
              (setf (mevedel-session-permission-rules session) nil)
              (write-region "" nil (file-name-concat root "ready") nil 'silent)
              (mevedel-pipeline-run-tool-outcome
               (mevedel-tool-ensure "WriteStdin") (lambda (value) (setq settled value))
               (list :execution_id id :yield_time_ms 5000))
              (with-timeout (60 (ert-fail "Profile-hint poll did not settle"))
                (while (null settled) (accept-process-output nil 0.02)))
              (should (eq 'error (plist-get settled :status)))
              (should (string-search "profile not attached" (plist-get settled :result)))
              (should-not (file-exists-p (file-name-concat metadata "index"))))
            (setf (mevedel-session-permission-rules session) (list rule)
                  (mevedel-session-resource-grants session) (list grant))
            (let ((result (mevedel-profile-hint-test--bash command)))
              (should (eq 'error (plist-get result :status)))
              (should (string-search "Read-only file system" (plist-get result :result)))
              (should (string-search "profile not attached" (plist-get result :result)))
              (should-not (file-exists-p (file-name-concat metadata "index"))))
            (let ((result
                   (mevedel-profile-hint-test--bash
                    command :sandbox_permissions "with_additional_permissions"
                    :additional_permissions
                    (list :file_system (list :write (vector metadata)))
                    :justification "Stage the requested file with existing tree authority")))
              (should (eq 'success (plist-get result :status)))
              (should-not (string-search "profile not attached" (plist-get result :result))))
            (setf (mevedel-session-resource-grants session) nil)
            (let ((result (mevedel-profile-hint-test--bash "git add -- example.txt")))
              (should (eq 'success (plist-get result :status)))
              (should (file-exists-p (file-name-concat metadata "index")))
              (should-not (string-search "profile not attached" (plist-get result :result))))
            (push (list "Bash" :pattern command :action 'allow :file-system (list grant))
                  (mevedel-session-permission-rules session))
            (let ((result (mevedel-profile-hint-test--bash command)))
              (should (eq 'success (plist-get result :status)))
              (should-not (string-search "profile not attached" (plist-get result :result))))
            ;; Even denial-shaped child output does not advertise an
            ;; unattached profile when whole-command authority covers it.
            (with-temp-file (file-name-concat root "fail.sh")
              (insert "printf \"fatal: '" metadata
                      "/index.lock': Permission denied\\n\"\nexit 1\n"))
            (let ((complete "git add -- example.txt && sh ./fail.sh"))
              (push (list "Bash" :pattern complete :action 'allow
                          :file-system (list grant))
                    (mevedel-session-permission-rules session))
              (let ((result (mevedel-profile-hint-test--bash complete)))
                (should (eq 'error (plist-get result :status)))
                (should (string-search "Permission denied" (plist-get result :result)))
                (should-not (string-search "profile not attached" (plist-get result :result))))))
        (mevedel-execution-teardown-session session)
        (delete-directory root t)))))

(mevedel-deftest mevedel-tool-exec-permission-compound-profile-misses
  (:doc "only direct usable unmatched filesystem allow profiles are diagnostic candidates")
  (let* ((root (make-temp-file "mevedel-profile-facts-" t))
         (path (file-name-concat root ".git"))
         (command "true && git add -- example.txt")
         (analysis (mevedel-bash-analysis-analyze command))
         (grant (list :path path :access 'write :recursive t))
         (rule (list "Bash" :pattern "git add *" :action 'allow :file-system (list grant)))
         (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory path)
          (dolist (bucket '(:session :persistent :defcustom))
            (let* ((context (list :buckets (list (cons bucket (list rule))) :resource-grants nil))
                   (before (copy-tree context))
                   (misses (mevedel-tool-exec-permission-compound-profile-misses
                            command analysis '(:level use-default) context)))
              (should (= 1 (length misses)))
              (should (equal path (plist-get (car misses) :path)))
              (should (eq bucket (plist-get (car misses) :bucket)))
              (should (equal before context))))
          (dolist (buckets
                   (list
                    (list (cons :invocation (list rule)))
                    (list (cons :request (list rule)))
                    (list (cons :session (list (list "Bash" :pattern "git add *"
                                                     :action 'deny :file-system (list grant)))))
                    (list (cons :session (list rule '("Bash" :pattern "git add *" :action ask))))
                    (list (cons :session (list rule))
                          '(:request ("Bash" :pattern "git add *" :action deny)))
                    (list (cons :session (list rule))
                          '(:invocation ("Bash" :pattern "git add *" :action deny)))
                    (list (cons :session (list rule (list "Bash" :path path :action 'deny))))
                    (list (cons :session (list rule (list "Bash" :path path :action 'ask))))
                    (list (cons :session (list (append rule '(:sandbox-permissions require-escalated)))))
                    (list (cons :session (list '("Bash" :pattern "git add *" :action allow :network t))))
                    (list (cons :session
                                (list (list "Bash" :pattern "git add *" :action 'allow
                                            :file-system
                                            (list (append grant
                                                          (list :workspace-origin
                                                                (list :root root :relative "./other"))))))))))
            (should-not
             (mevedel-tool-exec-permission-compound-profile-misses
              command analysis '(:level use-default) (list :buckets buckets :resource-grants nil))))
          (let ((context (list :buckets (list (cons :session (list rule))) :resource-grants nil)))
            ;; Whole-command and explicit coverage suppress the same miss.
            (should-not
             (mevedel-tool-exec-permission-compound-profile-misses
              command analysis (list :additional-permissions (list :file-system (list grant))) context))
            (should-not
             (mevedel-tool-exec-permission-compound-profile-misses
              command analysis '(:level escalated) context))
            (should-not
             (mevedel-tool-exec-permission-compound-profile-misses
              command (plist-put (copy-sequence analysis) :complex-p t) nil context))
            (should-not
             (mevedel-tool-exec-permission-compound-profile-misses
              "git add -- example.txt" (mevedel-bash-analysis-analyze "git add -- example.txt") nil context))
            (setf (plist-get context :resource-grants) (list grant))
            ;; Dormant independent authority does not mount paths by itself.
            (should
             (mevedel-tool-exec-permission-compound-profile-misses command analysis nil context))
            ;; A parent tree does not cover protected Git metadata.
            (should
             (mevedel-tool-exec-permission-compound-profile-misses
              command analysis
              (list :additional-permissions
                    (list :file-system (list (list :path root :access 'write :recursive t))))
              context)))
          (let* ((rules (make-list 12 rule))
                 (context (list :buckets (list (cons :session rules)) :resource-grants nil)))
            (should (= 8 (length (mevedel-tool-exec-permission-compound-profile-misses
                                  command analysis nil context))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-exec--compound-profile-hint
  (:doc "only actual failed confined execution with same-line path denial selects a hint")
  (let* ((miss '(:segment "git add -- example.txt" :pattern "git add *"
                          :bucket :session :path "/repo/.git"))
         (observation (list :facts '(:state completed :termination exited :outcome failure
                                            :workdir "/repo/")
                            :sandbox-facts '(:sandbox bubblewrap)
                            :compound-profile-misses (list miss))))
    (dolist (output '("fatal: '/repo/.git/index.lock': Read-only file system"
                      "fatal: '.git/index.lock': Permission denied"
                      "fatal: './.git/index.lock': Operation not permitted"))
      (setf (plist-get observation :output) output)
      (should (mevedel-tool-exec--compound-profile-hint observation)))
    (dolist (output '("fatal: '/repo/other.git/index.lock': Permission denied"
                      "fatal: '/prefix/repo/.git/index.lock': Permission denied"
                      "fatal: 'other.git/index.lock': Permission denied"
                      "fatal: '.git-other/index.lock': Permission denied"
                      "fatal: '/repo/.git-other/index.lock': Permission denied"
                      "fatal: '/repo/.git/index.lock': No such file or directory"
                      "fatal: '/unrelated/index.lock': Permission denied"
                      "fatal: '/repo/.git/index.lock'\nPermission denied"
                      "unrelated syntax error"))
      (setf (plist-get observation :output) output)
      (should-not (mevedel-tool-exec--compound-profile-hint observation)))
    (setf (plist-get observation :output) "fatal: '/repo/.git/index.lock': Permission denied")
    (dolist (override '((:facts (:state running :termination exited :outcome failure))
                        (:facts (:state completed :termination signaled :outcome failure))
                        (:facts (:state completed :termination exited :outcome success))
                        (:facts (:state completed :termination exited :outcome no-match))
                        (:sandbox-facts (:sandbox off))
                        (:sandbox-facts (:sandbox refused))
                        (:sandbox-facts (:sandbox unavailable))
                        (:error "Failed to start")
                        (:compound-profile-misses nil)))
      (let ((copy (copy-tree observation)))
        (setf (plist-get copy (car override)) (cadr override))
        (should-not (mevedel-tool-exec--compound-profile-hint copy))))
    ;; Excerpts are bounded by characters as well as display width, and escaped.
    (setf (plist-get observation :compound-profile-misses)
          (make-list 12 (list :segment (make-string 10000 ?\n)
                              :pattern (make-string 10000 ?\t)
                              :bucket :persistent :path "/repo/.git")))
    (let ((hint (mevedel-tool-exec--compound-profile-hint observation)))
      (should (< (length hint) 4000))
      (should-not (string-search "\n" hint))
      (should (string-search "\\n" hint))
      (should (string-search "..." hint))
      (let ((case-fold-search nil))
        (should (= 4 (length (split-string hint "segment " t))))))
    ;; Suppressed internal expansion and successful control observations stay quiet.
    (should-not
     (string-search "profile not attached"
                    (plist-get (mevedel-tool-exec--observation-envelope observation t) :result)))
    (should-not
     (string-search "profile not attached"
                    (plist-get (mevedel-tool-exec--observation-envelope observation nil t) :result)))))

(provide 'test-mevedel-tool-exec-profile-hint)
;;; test-mevedel-tool-exec-profile-hint.el ends here
