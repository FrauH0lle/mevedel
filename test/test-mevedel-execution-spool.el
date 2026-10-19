;;; test-mevedel-execution-spool.el --- Logical spool storage tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the storage seam through ordinary resource tools and worker jobs.

;;; Code:

(require 'helpers
         (file-name-concat (file-name-directory
                            (or buffer-file-name load-file-name byte-compile-current-file))
                           "helpers"))
(require 'mevedel-execution-spool)
(require 'mevedel-pipeline)
(require 'mevedel-resource)
(require 'mevedel-structs)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)

(defun test-mevedel-spool--wait (predicate)
  "Wait boundedly until PREDICATE succeeds."
  (let ((deadline (+ (float-time) 10)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (should (funcall predicate))))

(defun test-mevedel-spool--tool (session operation address &rest options)
  "Call public resource OPERATION on ADDRESS in SESSION with OPTIONS."
  (let* ((attempt (mevedel-resource-prepare operation address (list :session session)))
         (mevedel-resource-current-attempts (list (cons address attempt)))
         (mevedel--session session)
         (args (append (list (if (eq operation 'read) :file_path :path) address) options))
         (handler (pcase operation
                    ('read #'mevedel-tool-fs-read)
                    ('grep #'mevedel-tool-fs-search-grep)
                    ('glob #'mevedel-tool-fs-search-glob)))
         result done cancel)
    (unwind-protect
        (progn
          (setq cancel (funcall handler (lambda (value) (setq result value done t)) args))
          (test-mevedel-spool--wait (lambda () done))
          result)
      (when (functionp cancel) (funcall cancel)))))

(mevedel-deftest mevedel-execution-spool-tools
  (:doc "Read pagination, directory/file Grep and logical Glob survive compression")
  (let* ((root (make-temp-file "mevedel-spool-tools-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path root))
         (directory (file-name-concat root "tool-results" "executions"))
         (file (file-name-concat directory "execution-one.log"))
         (address "artifact://executions/execution-one.log")
         (body (concat "alpha\nneedle\nomega\n" (make-string 70000 ?x) "\n"))
         (make-temp (symbol-function 'make-temp-file)) views)
    (unwind-protect
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (prefix &rest args)
                     (let ((path (apply make-temp prefix args)))
                       (when (string-prefix-p "mevedel-spool-view-" prefix)
                         (push path views))
                       path))))
          (make-directory directory t)
          (with-temp-file file (insert body))
          (let ((before (test-mevedel-spool--tool session 'read address :offset 2 :limit 1)))
            (mevedel-execution-spool-compress file)
            (should (file-exists-p file))
            (test-mevedel-spool--wait
             (lambda () (zerop (hash-table-count mevedel-execution-spool--compressions))))
            (should-not (file-exists-p file))
            (should (file-exists-p (concat file ".gz")))
            (should (equal before (test-mevedel-spool--tool
                                   session 'read address :offset 2 :limit 1))))
          (with-temp-file (file-name-concat directory "execution-raw.log")
            (insert "second needle\n"))
          (dolist (target '("artifact://" "artifact://executions"))
            (let ((listing (plist-get (test-mevedel-spool--tool
                                      session 'read target :limit 1) :result))
                  (glob (plist-get (test-mevedel-spool--tool
                                    session 'glob target :pattern "**/*.log") :result)))
              (dolist (text (list listing glob))
                (should (string-match-p "artifact://executions/execution-one.log" text))
                (should-not (string-match-p "\\.gz" text))
                (should-not (string-match-p (regexp-quote root) text))))
            (dolist (mode '("content" "count" "files_with_matches"))
              (let ((text (plist-get
                           (test-mevedel-spool--tool
                            session 'grep target :pattern "needle" :output_mode mode) :result)))
                (should (string-match-p "artifact://executions/execution-one.log" text))
                (should (string-match-p "artifact://executions/execution-raw.log" text))
                (should-not (string-match-p "\\.gz" text)))))
          (let ((text (plist-get (test-mevedel-spool--tool
                                 session 'grep address :pattern "needle" :output_mode "content")
                                :result)))
            (should (string-match-p "needle" text))
            (should-not (string-match-p (regexp-quote root) text)))
          (should views)
          (dolist (view views) (should-not (file-exists-p view)))
          (should-not mevedel-execution-spool--readers))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-reader-lease
  (:doc "compression retains raw output until an asynchronous reader releases it")
  (let* ((root (make-temp-file "mevedel-spool-lease-" t))
         (path (file-name-concat root "execution.log"))
         deliver cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (setq cancel (mevedel-execution-spool-with-path
                        path 'read #'ignore
                        (lambda (_path callback) (setq deliver callback) nil)))
          (mevedel-execution-spool-compress path)
          (test-mevedel-spool--wait (lambda () (file-exists-p (concat path ".gz"))))
          (accept-process-output nil 0.05)
          (should (file-exists-p path))
          (funcall deliver '(:result "done"))
          (test-mevedel-spool--wait (lambda () (not (file-exists-p path))))
          (should-not mevedel-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-tramp-consumption
  (:quiet t :doc "a locally compressed spool remains readable and searchable through TRAMP")
  (let* ((root (make-temp-file "mevedel-spool-tramp-" t))
         (path (file-name-concat root "tool-results" "executions" "execution.log")))
    (unwind-protect
        (progn
          (make-directory (file-name-directory path) t)
          (with-temp-file path
            (insert "first\nneedle\n" (make-string 70000 ?x) "\n"))
          (mevedel-execution-spool-compress path)
          (test-mevedel-spool--wait (lambda () (not (file-exists-p path))))
          (mevedel-test--with-local-shell-tramp '("spool")
            (let* ((remote-root (format "/mevedelmock:spool:%s/" root))
                   (workspace (mevedel-workspace--create
                               :type 'file :id remote-root :root remote-root :name "spool"))
                   (session (mevedel-session-create "main" workspace))
                   (default-directory remote-root))
              (setf (mevedel-session-save-path session) remote-root
                    (mevedel-session-authority-mode session) 'pid-lock
                    (mevedel-session-sandbox-mode session) 'off)
              (let ((result (test-mevedel-spool--tool
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
                         (result (apply #'test-mevedel-spool--tool
                                        session operation target options))
                         (text (plist-get result :result)))
                    (should-not (eq 'error (plist-get result :status)))
                    (should (string-match-p "artifact://executions/execution.log" text))
                    (should-not (string-match-p "\\.gz" text))
                    (should-not (string-match-p (regexp-quote root) text)))))
              (should-not mevedel-execution-spool--readers))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-view-errors
  (:doc "corrupt gzip produces one error and cleans the disposable view")
  (let* ((root (make-temp-file "mevedel-spool-corrupt-" t))
         (session (mevedel-session--create :authority-mode 'pid-lock :save-path root))
         (path (file-name-concat root "tool-results" "executions" "bad.log.gz"))
         (make-temp (symbol-function 'make-temp-file)) views)
    (unwind-protect
        (cl-letf (((symbol-function 'make-temp-file)
                   (lambda (prefix &rest args)
                     (let ((path (apply make-temp prefix args)))
                       (when (string-prefix-p "mevedel-spool-view-" prefix) (push path views))
                       path))))
          (make-directory (file-name-directory path) t)
          (let ((file-name-handler-alist nil))
            (write-region "not gzip" nil path nil 'silent))
          (let ((result (test-mevedel-spool--tool
                         session 'read "artifact://executions/bad.log" :offset 2 :limit 1)))
            (should (eq 'error (plist-get result :status)))
            (should (string-match-p "Could not decode" (plist-get result :result)))
            (should-not (string-match-p (regexp-quote root) (plist-get result :result))))
          (should views)
          (dolist (view views) (should-not (file-exists-p view))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-worker-cleanup
  (:doc "an interrupted real worker cannot leak its known compression temporary")
  (dolist (mode '(cancel timeout))
  (let* ((root (make-temp-file "mevedel-spool-interrupt-" t))
         (path (file-name-concat root "execution.log"))
         (script (file-name-concat root "worker.py"))
         (mevedel-execution-spool--worker script)
         (mevedel-execution-spool--worker-timeout (if (eq mode 'timeout) 0.15 120))
         cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (with-temp-file script
            (insert "import pathlib,sys,time\n"
                    "p=pathlib.Path(sys.argv[sys.argv.index('--temporary')+1])\n"
                    "p.write_bytes(b'x'*65536)\ntime.sleep(60)\n"))
          (setq cancel (mevedel-execution-spool-compress path))
          (test-mevedel-spool--wait
           (lambda () (directory-files root nil "\\`\\.mevedel-spool-")))
          (when (eq mode 'cancel) (funcall cancel))
          (test-mevedel-spool--wait
           (lambda () (zerop (hash-table-count mevedel-execution-spool--compressions))))
          (should-not (directory-files root nil "\\`\\.mevedel-spool-"))
          (should (file-exists-p path))
          (should-not (file-exists-p (concat path ".gz"))))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t)))))

(mevedel-deftest mevedel-execution-spool-worker-startup
  (:doc "signalling and nil worker startup release buffers, timers, temporaries and registry")
  (dolist (failure '(signal nil))
    (let* ((root (make-temp-file "mevedel-spool-startup-" t))
           (path (file-name-concat root "execution.log"))
           (before-buffers (buffer-list))
           (before-timers (copy-sequence timer-list))
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
              (mevedel-execution-spool-compress path))
            (should attempted)
            (should temporary)
            (should-not (buffer-live-p worker-buffer))
            (should (equal before-buffers (buffer-list)))
            (should (equal before-timers timer-list))
            (should-not (file-exists-p temporary))
            (should-not (gethash path mevedel-execution-spool--compressions))
            (should (file-exists-p path)))
        (when (buffer-live-p worker-buffer) (kill-buffer worker-buffer))
        (delete-directory root t)))))

(mevedel-deftest mevedel-execution-spool-view-startup
  (:doc "view startup failures deliver a sanitized error and remove all scratch state")
  (let* ((root (make-temp-file "mevedel-spool-view-startup-" t))
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
                         (when (string-prefix-p "mevedel-spool-view-" prefix)
                           (push temporary views))
                         temporary)))
                    ((symbol-function 'make-process)
                     (lambda (&rest arguments)
                       (setq attempted t worker-buffer (plist-get arguments :buffer))
                       (error "Injected startup failure at %s" (car views)))))
            (setq cancel
                  (mevedel-execution-spool-with-path
                   path 'read (lambda (value) (setq result value))
                   (lambda (&rest _) (setq executed t) nil))))
          (should attempted)
          (should-not executed)
          (should (eq (plist-get result :status) 'error))
          (should (string-search "Could not prepare" (plist-get result :result)))
          (should-not (string-search "mevedel-spool-view-" (plist-get result :result)))
          (should views)
          (dolist (view views) (should-not (file-exists-p view)))
          (should-not (buffer-live-p worker-buffer))
          (should-not mevedel-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (when (buffer-live-p worker-buffer) (kill-buffer worker-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-retirement-identity
  (:doc "retirement requires the published gzip inode and unchanged raw identity")
  (dolist (changed '(gzip-inode gzip-mtime raw-inode))
    (let* ((root (make-temp-file "mevedel-spool-identity-" t))
           (path (file-name-concat root "execution.log"))
           (compressed (concat path ".gz"))
           deliver cancel)
      (unwind-protect
          (progn
            (with-temp-file path (insert (make-string 70000 ?x)))
            (setq cancel
                  (mevedel-execution-spool-with-path
                   path 'read #'ignore
                   (lambda (_selected callback) (setq deliver callback) nil)))
            (mevedel-execution-spool-compress path)
            (test-mevedel-spool--wait
             (lambda () (consp (gethash path mevedel-execution-spool--compressions))))
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
            (should-not (gethash path mevedel-execution-spool--compressions))
            (should-not mevedel-execution-spool--readers))
        (when (functionp cancel) (funcall cancel))
        (delete-directory root t)))))

(mevedel-deftest mevedel-execution-spool-view-cancellation
  (:doc "cancellation releases an inflated view and suppresses late delivery")
  (let* ((root (make-temp-file "mevedel-spool-cancel-view-" t))
         (path (file-name-concat root "execution.log"))
         selected deliver result cancel)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (mevedel-execution-spool-compress path)
          (test-mevedel-spool--wait (lambda () (not (file-exists-p path))))
          (setq cancel
                (mevedel-execution-spool-with-path
                 path 'grep (lambda (value) (setq result value))
                 (lambda (file callback) (setq selected file deliver callback) nil)))
          (test-mevedel-spool--wait (lambda () selected))
          (should (file-exists-p selected))
          (funcall cancel)
          (should-not (file-exists-p selected))
          (funcall deliver '(:result "late"))
          (should-not result)
          (should-not mevedel-execution-spool--readers))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-spool-view-cancellation-errors
  (:doc "a throwing executor canceller cannot retain its real gzip view or lease")
  (let* ((root (make-temp-file "mevedel-spool-cancel-error-" t))
         (path (file-name-concat root "execution.log"))
         (worker (symbol-function 'mevedel-execution-spool--worker))
         selected deliver result cancel executor-cancelled worker-cancelled)
    (unwind-protect
        (progn
          (with-temp-file path (insert (make-string 70000 ?x)))
          (mevedel-execution-spool-compress path)
          (test-mevedel-spool--wait (lambda () (not (file-exists-p path))))
          (cl-letf (((symbol-function 'mevedel-execution-spool--worker)
                     (lambda (&rest arguments)
                       (let ((cancel-worker (apply worker arguments)))
                         (lambda () (setq worker-cancelled t) (funcall cancel-worker))))))
            (setq cancel
                  (mevedel-execution-spool-with-path
                   path 'grep (lambda (value) (setq result value))
                   (lambda (file callback)
                     (setq selected file deliver callback)
                     (lambda () (setq executor-cancelled t) (error "Injected cancel failure"))))))
          (test-mevedel-spool--wait (lambda () selected))
          (should (file-exists-p selected))
          (funcall cancel)
          (should executor-cancelled)
          (should worker-cancelled)
          (should-not (file-exists-p (file-name-directory selected)))
          (should-not mevedel-execution-spool--readers)
          (funcall deliver '(:result "late"))
          (should-not result)
          (funcall cancel))
      (when (functionp cancel) (funcall cancel))
      (delete-directory root t))))

(provide 'test-mevedel-execution-spool)
;;; test-mevedel-execution-spool.el ends here
