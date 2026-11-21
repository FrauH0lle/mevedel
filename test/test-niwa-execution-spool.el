;;; test-niwa-execution-spool.el --- Logical spool storage tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the storage seam through ordinary resource tools and worker jobs.

;;; Code:

(require 'niwa-execution-spool-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-execution-spool-test-support"))
(require 'niwa-session-test-support
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-session-test-support"))

(require 'niwa-execution-test-helpers
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "niwa-execution-test-helpers"))

(niwa-deftest niwa-execution-foreground-truncation-artifact
  (:doc "a truncated foreground result names an artifact that Read resolves")
  (let* ((root (make-temp-file "niwa-fg-trunc-" t))
         (session (test-niwa-execution--session root))
         (niwa-execution-inline-output-limit 20)
         result)
    (unwind-protect
        (progn
          (niwa-execution-start-bash
           (lambda (value) (setq result value))
           :session session :owner "main" :owner-context session
           :command (list "sh" "-c" "printf 'HEAD\\n'; printf 'MIDDLE-%0200d\\n' 0; printf 'TAIL\\n'")
           :workdir root :writable-roots (list root)
           :artifact-directory
           (file-name-concat (niwa-session-save-path session)
                             "tool-results" "executions")
           :yield-time-ms 30000)
          (test-niwa-execution--wait (lambda () result))
          (should-not (plist-get (plist-get result :facts) :execution-id))
          (let* ((output (plist-get result :output))
                 (address (and (string-match
                                "Read(file_path=\"\\(artifact://[^\"]+\\)\"" output)
                               (match-string 1 output))))
            (should (string-match-p "tool output truncated" output))
            (should address)
            (should (equal address (plist-get (plist-get result :facts) :output-path)))
            (should (string-match-p
                     "MIDDLE-"
                     (plist-get (test-niwa-spool--tool session 'read address)
                                :result)))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-tools
  (:doc "Read pagination, directory/file Grep and logical Glob survive compression")
  (let* ((root (make-temp-file "niwa-spool-tools-" t))
         (session (niwa-session--create :save-path root))
         (directory (file-name-concat root "tool-results" "executions"))
         (file (file-name-concat directory "execution-one.log"))
         (address "artifact://executions/execution-one.log")
         (body (concat "alpha\nneedle\nomega\n" (make-string 70000 ?x) "\n"))
         (make-temp (symbol-function 'make-temp-file)) views)
    (unwind-protect
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (prefix &rest args)
                     (let ((path (apply make-temp prefix args)))
                       (when (string-prefix-p "niwa-spool-view-" prefix)
                         (push path views))
                       path))))
          (make-directory directory t)
          (with-temp-file file (insert body))
          (let ((before (test-niwa-spool--tool session 'read address :offset 2 :limit 1)))
            (niwa-execution-spool-compress file)
            (should (file-exists-p file))
            (test-niwa-spool--wait
             (lambda () (zerop (hash-table-count niwa-execution-spool--compressions))))
            (should-not (file-exists-p file))
            (should (file-exists-p (concat file ".gz")))
            (should (equal before (test-niwa-spool--tool
                                   session 'read address :offset 2 :limit 1))))
          (with-temp-file (file-name-concat directory "execution-raw.log")
            (insert "second needle\n"))
          (dolist (target '("artifact://" "artifact://executions"))
            (let ((listing (plist-get (test-niwa-spool--tool
                                      session 'read target :limit 1) :result))
                  (glob (plist-get (test-niwa-spool--tool
                                    session 'glob target :pattern "**/*.log") :result)))
              (dolist (text (list listing glob))
                (should (string-match-p "artifact://executions/execution-one.log" text))
                (should-not (string-match-p "\\.gz" text))
                (should-not (string-match-p (regexp-quote root) text))))
            (dolist (mode '("content" "count" "files_with_matches"))
              (let ((text (plist-get
                           (test-niwa-spool--tool
                            session 'grep target :pattern "needle" :output_mode mode) :result)))
                (should (string-match-p "artifact://executions/execution-one.log" text))
                (should (string-match-p "artifact://executions/execution-raw.log" text))
                (should-not (string-match-p "\\.gz" text)))))
          (let ((text (plist-get (test-niwa-spool--tool
                                 session 'grep address :pattern "needle" :output_mode "content")
                                :result)))
            (should (string-match-p "needle" text))
            (should-not (string-match-p (regexp-quote root) text)))
          (should views)
          (dolist (view views) (should-not (file-exists-p view)))
          (should-not niwa-execution-spool--readers))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-reader-lease
  (:doc "compression retains raw output until an asynchronous reader releases it")
  (let* ((root (make-temp-file "niwa-spool-lease-" t))
         (path (file-name-concat root "execution.log"))
         deliver cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (setq cancel (niwa-execution-spool-with-path
                        path 'read #'ignore
                        (lambda (_path callback) (setq deliver callback) nil)))
          (niwa-execution-spool-compress path)
          (test-niwa-spool--wait (lambda () (file-exists-p (concat path ".gz"))))
          (accept-process-output nil 0.05)
          (should (file-exists-p path))
          (funcall deliver '(:result "done"))
          (test-niwa-spool--wait (lambda () (not (file-exists-p path))))
          (should-not niwa-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-tramp-consumption
  (:quiet t :doc "a locally compressed spool remains readable and searchable through TRAMP")
  (let* ((root (make-temp-file "niwa-spool-tramp-" t))
         (niwa-session-durability-accept-target-storage t)
         (niwa-session-durability--disclosed-targets (make-hash-table :test #'equal))
         (path (file-name-concat root "tool-results" "executions" "execution.log")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (with-temp-file path
            (insert "first\nneedle\n" (make-string 70000 ?x) "\n"))
          (niwa-execution-spool-compress path)
          (test-niwa-spool--wait (lambda () (not (file-exists-p path))))
          (niwa-test--with-local-shell-tramp '("spool")
            (let* ((remote-root (format "/niwamock:spool:%s/" root))
                   (session (test-niwa-session-persistence--leased-session
                             remote-root :name "main" :sandbox-mode 'off))
                   (default-directory remote-root))
              (unwind-protect
                  (progn
                    (with-temp-buffer
                      (org-mode)
                      (niwa-session-artifacts-save session (current-buffer)))
                    (let (done selected)
                      (niwa-execution-spool-with-path
                       (concat (file-remote-p remote-root) path) 'read
                       (lambda (_result) (setq done t))
                       (lambda (view callback)
                         (setq selected view)
                         (funcall callback '(:result "owned view"))))
                      (test-niwa-spool--wait (lambda () done))
                      (should selected)
                      (should (equal (file-remote-p remote-root) (file-remote-p selected)))
                      (should (string-prefix-p (file-name-as-directory temporary-file-directory)
                                               (file-local-name selected))))
                    (let ((result (test-niwa-spool--tool
                                   session 'read "artifact://executions/execution.log"
                                   :offset 2 :limit 1)))
                      (should-not (eq 'error (plist-get result :status)))
                      (should (string-match-p "2\tneedle" (plist-get result :result))))
                    (dolist (target '("artifact://" "artifact://executions"))
                      (dolist (operation '(read glob grep))
                        (let* ((options (pcase operation
                                          ('read '(:limit 1))
                                          ('glob '(:pattern "**/*.log"))
                                          ('grep '(:pattern "needle" :output_mode "files_with_matches"))))
                               (result (apply #'test-niwa-spool--tool
                                              session operation target options))
                               (text (plist-get result :result)))
                          (should-not (eq 'error (plist-get result :status)))
                          (should (string-match-p "artifact://executions/execution.log" text))
                          (should-not (string-match-p "\\.gz" text))
                          (should-not (string-match-p (regexp-quote root) text)))))
                    (should-not niwa-execution-spool--readers))
                (niwa-session-persistence-lock-release remote-root session)
                (niwa-workspace-clear-registry)))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-view-errors
  (:doc "corrupt gzip produces one error and cleans the disposable view")
  (let* ((root (make-temp-file "niwa-spool-corrupt-" t))
         (session (niwa-session--create :save-path root))
         (path (file-name-concat root "tool-results" "executions" "bad.log.gz"))
         (make-temp (symbol-function 'make-temp-file)) views)
    (unwind-protect
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (prefix &rest args)
                     (let ((path (apply make-temp prefix args)))
                       (when (string-prefix-p "niwa-spool-view-" prefix) (push path views))
                       path))))
          (make-directory (file-name-directory path) t)
          (let ((file-name-handler-alist nil))
            (write-region "not gzip" nil path nil 'silent))
          (let ((result (test-niwa-spool--tool
                         session 'read "artifact://executions/bad.log" :offset 2 :limit 1)))
            (should (eq 'error (plist-get result :status)))
            (should (string-match-p "Could not decode" (plist-get result :result)))
            (should-not (string-match-p (regexp-quote root) (plist-get result :result))))
          (should views)
          (dolist (view views) (should-not (file-exists-p view))))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-worker-cleanup
  (:doc "an interrupted real worker cannot leak its known compression temporary")
  (dolist (mode '(cancel timeout))
  (let* ((root (make-temp-file "niwa-spool-interrupt-" t))
         (path (file-name-concat root "execution.log"))
         (script (file-name-concat root "worker.py"))
         (niwa-execution-spool--worker script)
         (niwa-execution-spool--worker-timeout (if (eq mode 'timeout) 0.15 120))
         cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (with-temp-file script
            (insert "import pathlib,sys,time\n"
                    "p=pathlib.Path(sys.argv[sys.argv.index('--temporary')+1])\n"
                    "p.write_bytes(b'x'*65536)\ntime.sleep(60)\n"))
          (setq cancel (niwa-execution-spool-compress path))
          (test-niwa-spool--wait
           (lambda () (directory-files root nil "\\`\\.niwa-spool-")))
          (when (eq mode 'cancel) (funcall cancel))
          (test-niwa-spool--wait
           (lambda () (zerop (hash-table-count niwa-execution-spool--compressions))))
          (should-not (directory-files root nil "\\`\\.niwa-spool-"))
          (should (file-exists-p path))
          (should-not (file-exists-p (concat path ".gz"))))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t)))))

(niwa-deftest niwa-execution-spool-worker-startup
  (:doc "signalling and nil worker startup release buffers, timers, temporaries and registry")
  (dolist (failure '(signal nil))
    (let* ((timers (lambda ()
                     ;; Any undo-recorded change arms Emacs's own one-shot
                     ;; boundary timer when none is pending, so whether it
                     ;; appears depends on timing and test order, not on
                     ;; the spool.
                     (seq-remove (lambda (timer)
                                   (eq (timer--function timer)
                                       #'undo-auto--boundary-timer))
                                 timer-list)))
           (root (make-temp-file "niwa-spool-startup-" t))
           (path (file-name-concat root "execution.log"))
           (before-buffers (buffer-list))
           (before-timers (funcall timers))
           attempted worker-buffer temporary)
      (unwind-protect
          (progn
            (with-temp-file path (insert (make-string 70000 ?x)))
            (cl-letf (((symbol-function 'make-process)
                       (lambda (&rest arguments)
                         (setq attempted t
                               worker-buffer (plist-get arguments :buffer)
                               temporary (cadr (member "--temporary"
                                                       (plist-get arguments :command))))
                         (when temporary
                           (write-region "partial startup" nil temporary nil 'silent))
                         (when (eq failure 'signal) (error "Injected spool startup failure"))
                         nil)))
              (niwa-execution-spool-compress path))
            (should attempted)
            (should temporary)
            (should-not (buffer-live-p worker-buffer))
            (should (equal before-buffers (buffer-list)))
            (should (equal before-timers (funcall timers)))
            (should-not (file-exists-p temporary))
            (should-not (gethash path niwa-execution-spool--compressions))
            (should (file-exists-p path)))
        (when (buffer-live-p worker-buffer) (kill-buffer worker-buffer))
        (delete-directory root t)))))

(niwa-deftest niwa-execution-spool-view-startup
  (:doc "view startup failures deliver a sanitized error and remove all scratch state")
  (let* ((root (make-temp-file "niwa-spool-view-startup-" t))
         (path (file-name-concat root "execution.log.gz"))
         (make-temp (symbol-function 'make-temp-file))
         worker-buffer views result cancel attempted executed)
    (unwind-protect
        (progn
          (let ((file-name-handler-alist nil))
            (write-region "not decoded" nil path nil 'silent))
          (cl-letf (((symbol-function 'make-temp-file)
                     (lambda (prefix &rest arguments)
                       (let ((temporary (apply make-temp prefix arguments)))
                         (when (string-prefix-p "niwa-spool-view-" prefix)
                           (push temporary views))
                         temporary)))
                    ((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq attempted t worker-buffer (plist-get arguments :buffer))
                       (error "Injected startup failure at %s" (car views)))))
            (setq cancel
                  (niwa-execution-spool-with-path
                   path 'read (lambda (value) (setq result value))
                   (lambda (&rest _) (setq executed t) nil))))
          (should attempted)
          (should-not executed)
          (should (eq (plist-get result :status) 'error))
          (should (string-search "Could not prepare" (plist-get result :result)))
          (should-not (string-search "niwa-spool-view-" (plist-get result :result)))
          (should views)
          (dolist (view views) (should-not (file-exists-p view)))
          (should-not (buffer-live-p worker-buffer))
          (should-not niwa-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (when (buffer-live-p worker-buffer) (kill-buffer worker-buffer))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-retirement-identity
  (:doc "retirement requires the published gzip inode and unchanged raw identity")
  (dolist (changed '(gzip-inode gzip-mtime raw-inode))
    (let* ((root (make-temp-file "niwa-spool-identity-" t))
           (path (file-name-concat root "execution.log"))
           (compressed (concat path ".gz"))
           deliver cancel)
      (unwind-protect
          (progn
            (with-temp-file path (insert (make-string 70000 ?x)))
            (setq cancel
                  (niwa-execution-spool-with-path
                   path 'read #'ignore
                   (lambda (_selected callback) (setq deliver callback) nil)))
            (niwa-execution-spool-compress path)
            (test-niwa-spool--wait
             (lambda () (consp (gethash path niwa-execution-spool--compressions))))
            (should (file-exists-p path))
            (let* ((target (if (eq changed 'raw-inode) path compressed))
                   (attrs (file-attributes target))
                   (file-name-handler-alist nil))
              (if (eq changed 'gzip-mtime)
                  (set-file-times target (time-add (file-attribute-modification-time attrs) 1))
                (rename-file target (concat target ".prior"))
                (with-temp-file target
                  (set-buffer-multibyte nil)
                  (insert (make-string (file-attribute-size attrs) ?z)))
                (set-file-times target (file-attribute-modification-time attrs))))
            (funcall deliver '(:result "done"))
            (should (file-exists-p path))
            (should-not (gethash path niwa-execution-spool--compressions))
            (should-not niwa-execution-spool--readers))
        (when (functionp cancel) (funcall cancel))
        (delete-directory root t)))))

(niwa-deftest niwa-execution-spool-view-cancellation
  (:doc "cancellation releases an inflated view and suppresses late delivery")
  (let* ((root (make-temp-file "niwa-spool-cancel-view-" t))
         (path (file-name-concat root "execution.log"))
         selected deliver result cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (niwa-execution-spool-compress path)
          (test-niwa-spool--wait (lambda () (not (file-exists-p path))))
          (setq cancel
                (niwa-execution-spool-with-path
                 path 'grep (lambda (value) (setq result value))
                 (lambda (file callback) (setq selected file deliver callback) nil)))
          (test-niwa-spool--wait (lambda () selected))
          (should (file-exists-p selected))
          (funcall cancel)
          (should-not (file-exists-p selected))
          (funcall deliver '(:result "late"))
          (should-not result)
          (should-not niwa-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(niwa-deftest niwa-execution-spool-view-cancellation-errors
  (:doc "a throwing executor canceller cannot retain its real gzip view or lease")
  (let* ((root (make-temp-file "niwa-spool-cancel-error-" t))
         (path (file-name-concat root "execution.log"))
         (worker (symbol-function 'niwa-execution-spool--worker))
         selected deliver result cancel executor-cancelled worker-cancelled)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (niwa-execution-spool-compress path)
          (test-niwa-spool--wait (lambda () (not (file-exists-p path))))
          (cl-letf (((symbol-function 'niwa-execution-spool--worker)
                     (lambda (&rest arguments)
                       (let ((cancel-worker (apply worker arguments)))
                         (lambda () (setq worker-cancelled t) (funcall cancel-worker))))))
            (setq cancel
                  (niwa-execution-spool-with-path
                   path 'grep (lambda (value) (setq result value))
                   (lambda (file callback)
                     (setq selected file deliver callback)
                     (lambda () (setq executor-cancelled t) (error "Injected cancel failure"))))))
          (test-niwa-spool--wait (lambda () selected))
          (should (file-exists-p selected))
          (funcall cancel)
          (should executor-cancelled)
          (should worker-cancelled)
          (should-not (file-exists-p (file-name-directory selected)))
          (should-not niwa-execution-spool--readers)
          (funcall deliver '(:result "late"))
          (should-not result)
          (funcall cancel))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(provide 'test-niwa-execution-spool)
;;; test-niwa-execution-spool.el ends here
