;;; test-niwa-plugin-update-check.el -- Plugin update-check tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the opt-in plugin update check: scheduling, the async git seam,
;; once-per-commit announcements and persisted state.  The fake executor
;; stands in for the network; one test drives the real asynchronous
;; executor against a local file remote.

;;; Code:

(require 'cl-lib)
(require 'niwa-plugin-lifecycle)
(require 'niwa-plugin-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-plugin-test-support"))

(defvar niwa-plugins-test--calls nil
  "Git calls (DIRECTORY . ARGS) the fake async executor received.")

(defvar niwa-plugins-test--pending nil
  "Deferred fake-executor answers, oldest last.")

(defun niwa-plugins-test--clone (repo)
  "Create a managed plugin clone named REPO, with a `.git' marker."
  (let ((root (niwa-plugins-test--github-install-root "o" repo)))
    (niwa-plugins-test--write-manifest
     root (format "{\"name\":\"%s\"}" repo))
    (make-directory (file-name-concat root ".git") t)
    root))

(defun niwa-plugins-test--executor (heads &optional defer)
  "Return a fake async executor answering from HEADS.
HEADS maps a repo name to (:remote SHA :local SHA), or (:fail MESSAGE).
With DEFER, answers wait in `niwa-plugins-test--pending'."
  (lambda (directory args callback)
    (push (cons directory args) niwa-plugins-test--calls)
    (let* ((head (cdr (cl-find-if (lambda (entry)
                                    (string-match-p (car entry) directory))
                                  heads)))
           (result (cond
                    ((plist-get head :fail) (list 128 (plist-get head :fail)))
                    ((equal (car args) "ls-remote")
                     (list 0 (format "%s\tHEAD" (plist-get head :remote))))
                    (t (list 0 (plist-get head :local)))))
           (answer (lambda () (apply callback result))))
      (if defer
          (setq niwa-plugins-test--pending
                (append niwa-plugins-test--pending (list answer)))
        (funcall answer)))))

(defun niwa-plugins-test--wait-idle ()
  "Wait for the running update check to finish."
  (with-timeout (20 (error "Update check did not finish"))
    (while niwa-plugins--update-check-busy
      (accept-process-output nil 0.05))))

(defun niwa-plugins-test--git (directory &rest args)
  "Run real git ARGS in DIRECTORY and return trimmed output."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil
                            "-c" "user.name=t" "-c" "user.email=t@t"
                            "-c" "commit.gpgsign=false" args))
        (error "Git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(niwa-deftest niwa-plugins-check-updates
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-plugin-check-" t)))
           (niwa-user-dir root)
           (niwa-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (niwa-plugins-update-check-interval nil)
           (niwa-plugins--update-cache :unloaded)
           (niwa-plugins--update-check-timer nil)
           (niwa-plugins--update-check-busy nil)
           (niwa-plugins-test--calls nil)
           (niwa-plugins-test--pending nil)
           (messages nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "loading the modules and the default setting never reach the network"
  (let ((timers nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (push 'make-process niwa-plugins-test--calls)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (niwa-plugins-git-executor
               (lambda (&rest _) (push 'sync niwa-plugins-test--calls) '(0 "")))
              (niwa-plugins-async-git-executor
               (lambda (&rest _) (push 'async niwa-plugins-test--calls))))
      (niwa-plugins-test--clone "a")
      (load "niwa-plugin-registry" nil t)
      (load "niwa-plugin-lifecycle" nil t)
      (niwa-plugins-list)
      (should-not niwa-plugins-test--calls)
      (should-not timers)))

  :doc "an enabled interval schedules one idle check and still starts no git"
  (let ((niwa-plugins-update-check-interval 1)
        (timers nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (niwa-plugins-async-git-executor
               (lambda (&rest _) (push 'async niwa-plugins-test--calls))))
      (niwa-plugins-test--clone "a")
      (niwa-plugins-list)
      (niwa-plugins-list)
      (should (= 1 (length timers)))
      (should (> (car (car timers)) 0))
      (should-not niwa-plugins-test--calls)))

  :doc "the last check time persists, so a restart inside the interval is quiet"
  (let ((niwa-plugins-update-check-interval 1)
        (timers nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (niwa-plugins-async-git-executor
               (niwa-plugins-test--executor nil)))
      (niwa-plugins-test--clone "a")
      (niwa-test--with-captured-messages nil
        (niwa-plugins-check-updates))
      ;; Simulate a restart: only the file survives.
      (setq niwa-plugins--update-cache :unloaded)
      (should-not (niwa-plugins--update-check-due-p))
      (niwa-plugins-list)
      (should-not timers)
      ;; Two days later the check is due again.
      (setq niwa-plugins--update-cache
            (list :last-check (- (float-time) (* 2 86400))))
      (should (niwa-plugins--update-check-due-p))))

  :doc "checks one plugin at a time and caches remote versus local HEAD"
  (let ((niwa-plugins-async-git-executor
         (niwa-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            ("/b/" :remote "cccccccccccccccccccccccccccccccccccccccc" :local "cccccccccccccccccccccccccccccccccccccccc"))
          t)))
    (niwa-plugins-test--clone "a")
    (niwa-plugins-test--clone "b")
    (niwa-test--with-captured-messages messages
      (niwa-plugins-check-updates)
      ;; Only one git call is in flight until its answer arrives.
      (while niwa-plugins-test--pending
        (should (= 1 (length niwa-plugins-test--pending)))
        (funcall (pop niwa-plugins-test--pending))))
    (should-not niwa-plugins--update-check-busy)
    (should (equal '("ls-remote" "origin" "HEAD")
                   (cdr (car (last niwa-plugins-test--calls)))))
    (should (= 4 (length niwa-plugins-test--calls)))
    (should (equal "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" (niwa-plugins-update-available-p
                          (niwa-plugins-find "a"))))
    (should-not (niwa-plugins-update-available-p
                 (niwa-plugins-find "b")))
    (should (string-match-p
             "updates available for 1 plugin; /plugin to review" messages)))

  :doc "a second check is a no-op while one is running"
  (let ((niwa-plugins-async-git-executor
         (niwa-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")) t)))
    (niwa-plugins-test--clone "a")
    (niwa-test--with-captured-messages nil
      (niwa-plugins-check-updates)
      (niwa-plugins-check-updates t)
      (should (= 1 (length niwa-plugins-test--calls)))
      (while niwa-plugins-test--pending
        (funcall (pop niwa-plugins-test--pending)))))

  :doc "each upstream commit is announced once, a newer one again"
  (let* ((heads (list (list "/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
         (niwa-plugins-async-git-executor
          (lambda (&rest args)
            (apply (niwa-plugins-test--executor heads) args))))
    (niwa-plugins-test--clone "a")
    (niwa-test--with-captured-messages messages
      (dotimes (_ 3) (niwa-plugins-check-updates)))
    (should (equal "niwa: updates available for 1 plugin; /plugin to review\n"
                   messages))
    (setcdr (car heads) (list :remote "dddddddddddddddddddddddddddddddddddddddd" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (niwa-test--with-captured-messages messages
      (niwa-plugins-check-updates))
    (should (equal "niwa: updates available for 1 plugin; /plugin to review\n"
                   messages)))

  :doc "an offline check stays silent and is shown as a failed check"
  (let ((niwa-plugins-async-git-executor
         (niwa-plugins-test--executor
          '(("/a/" :fail "fatal: unable to access")))))
    (niwa-plugins-test--clone "a")
    (niwa-test--with-captured-messages messages
      (niwa-plugins-check-updates))
    (should (equal "" messages))
    (should (equal "git exited with status 128"
                   (niwa-plugins-update-check-error
                    (niwa-plugins-find "a"))))
    ;; Git's own output may carry credentials and must not reach the file.
    (should-not (string-match-p
                 "fatal" (with-temp-buffer
                           (insert-file-contents
                            (niwa-plugins--update-state-file))
                           (buffer-string))))
    (should-not (niwa-plugins-update-available-p
                 (niwa-plugins-find "a"))))

  :doc "a non-numeric persisted last-check time counts as due"
  (let ((niwa-plugins-update-check-interval 1))
    (setq niwa-plugins--update-cache (list :last-check "yesterday"))
    (should (niwa-plugins--update-check-due-p)))

  :doc "a callback that signals does not leave later checks blocked"
  (let ((niwa-plugins-async-git-executor
         (niwa-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")) t)))
    (niwa-plugins-test--clone "a")
    (cl-letf (((symbol-function 'niwa-plugins--update-set-entry)
               (lambda (&rest _) (error "Disk full"))))
      (niwa-plugins-check-updates)
      (funcall (pop niwa-plugins-test--pending))
      (should-error (funcall (pop niwa-plugins-test--pending))))
    (should-not niwa-plugins--update-check-busy)
    (let ((before (length niwa-plugins-test--calls)))
      (niwa-test--with-captured-messages nil
        (niwa-plugins-check-updates))
      (should (> (length niwa-plugins-test--calls) before))))

  :doc "the on-demand check ignores the nil interval and reports its outcome"
  (let ((niwa-plugins-async-git-executor
         (niwa-plugins-test--executor
          '(("/a/" :remote "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
        (done nil))
    (niwa-plugins-test--clone "a")
    (niwa-test--with-captured-messages messages
      (niwa-plugins-check-updates t (lambda () (setq done t))))
    (should done)
    (should (= 2 (length niwa-plugins-test--calls)))
    (should (string-match-p "checked 1 plugin, 0 with updates available"
                            messages)))

  :doc "unmanaged and non-clone plugins are not checked"
  (let ((external (file-name-concat root "external"))
        (niwa-plugin-extra-roots nil)
        (niwa-plugins-async-git-executor
         (niwa-plugins-test--executor nil)))
    (setq niwa-plugin-extra-roots (list external))
    (niwa-plugins-test--write-manifest external "{\"name\":\"ext\"}")
    (make-directory (file-name-concat external ".git") t)
    (niwa-plugins-test--write-manifest
     (niwa-plugins-test--github-install-root "o" "plain")
     "{\"name\":\"plain\"}")
    (niwa-test--with-captured-messages nil
      (niwa-plugins-check-updates t))
    (should-not niwa-plugins-test--calls))

  :doc "the cached update and its notice survive a restart, and an update clears them"
  (let* ((heads '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
         (plugin-root (niwa-plugins-test--clone "a"))
         (niwa-plugins-async-git-executor
          (niwa-plugins-test--executor heads))
         (niwa-plugins-git-executor (lambda (&rest _) (list 0 "ok"))))
    (niwa-test--with-captured-messages nil
      (niwa-plugins-check-updates))
    (setq niwa-plugins--update-cache :unloaded)
    (should (niwa-plugins-update-available-p (niwa-plugins-find "a")))
    (should (equal "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" (plist-get (niwa-plugins--update-entry-for-root
                                     plugin-root)
                                    :notified)))
    (niwa-plugins-update "a" (niwa-plugins-test--workspace root))
    (should-not (niwa-plugins-update-available-p
                 (niwa-plugins-find "a")))))

(niwa-deftest niwa-plugins--process-git-async-environment
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-plugin-env-" t))))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "the executor switches off every credential prompt path"
  (let (seen-env seen-command (real (symbol-function 'make-process)) done)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest plist)
                 (setq seen-env process-environment
                       seen-command (plist-get plist :command))
                 (apply real (plist-put plist :command '("true"))))))
      (niwa-plugins--process-git-async
       root '("ls-remote" "origin" "HEAD") (lambda (&rest _) (setq done t)))
      (with-timeout (10 (error "Executor did not finish"))
        (while (not done) (accept-process-output nil 0.05))))
    (dolist (entry '("GIT_TERMINAL_PROMPT=0" "GIT_ASKPASS=false"
                     "SSH_ASKPASS_REQUIRE=never" "GCM_INTERACTIVE=never"))
      (should (member entry seen-env)))
    (should (equal seen-command
                   '("git" "-c" "credential.interactive=never"
                     "ls-remote" "origin" "HEAD")))))

(niwa-deftest niwa-plugins--process-git-async
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-plugin-real-check-" t)))
           (niwa-user-dir root)
           (niwa-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (niwa-plugins--update-cache :unloaded)
           (niwa-plugins--update-check-timer nil)
           (niwa-plugins--update-check-busy nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "detects a new commit through real git against a local remote"
  (progn
    (skip-unless (executable-find "git"))
    (let ((origin (file-name-concat root "origin"))
          (dest (niwa-plugins-test--github-install-root "o" "real")))
      (niwa-plugins-test--write-manifest origin "{\"name\":\"real\"}")
      (niwa-plugins-test--git origin "init" "-q")
      (niwa-plugins-test--git origin "add" ".")
      (niwa-plugins-test--git origin "commit" "-q" "-m" "one")
      (make-directory (file-name-directory dest) t)
      (niwa-plugins-test--git root "clone" "-q" origin dest)
      (niwa-test--with-captured-messages nil
        (niwa-plugins-check-updates t)
        (niwa-plugins-test--wait-idle))
      (should-not (niwa-plugins-update-check-error
                   (niwa-plugins-find "real")))
      (should-not (niwa-plugins-update-available-p
                   (niwa-plugins-find "real")))
      (with-temp-file (file-name-concat origin "new") (insert "x"))
      (niwa-plugins-test--git origin "add" ".")
      (niwa-plugins-test--git origin "commit" "-q" "-m" "two")
      (niwa-test--with-captured-messages nil
        (niwa-plugins-check-updates t)
        (niwa-plugins-test--wait-idle))
      (should (equal (niwa-plugins-test--git origin "rev-parse" "HEAD")
                     (niwa-plugins-update-available-p
                      (niwa-plugins-find "real")))))))

(niwa-deftest niwa-plugins--process-git-async-diagnostics
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-plugin-diag-" t)))
           (niwa-user-dir root)
           (niwa-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (niwa-plugins--update-cache :unloaded)
           (niwa-plugins--update-check-timer nil)
           (niwa-plugins--update-check-busy nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "git diagnostics on stderr never become cached commit ids"
  (progn
    (skip-unless (executable-find "git"))
    (let ((origin (file-name-concat root "origin"))
          (dest (niwa-plugins-test--github-install-root "o" "noisy")))
      (niwa-plugins-test--write-manifest origin "{\"name\":\"noisy\"}")
      (niwa-plugins-test--git origin "init" "-q")
      (niwa-plugins-test--git origin "add" ".")
      (niwa-plugins-test--git origin "commit" "-q" "-m" "one")
      (make-directory (file-name-directory dest) t)
      (niwa-plugins-test--git root "clone" "-q" origin dest)
      ;; A successful git that also writes trace lines to stderr.
      (let ((process-environment (cons "GIT_TRACE=1" process-environment))
            (head (niwa-plugins-test--git origin "rev-parse" "HEAD")))
        (niwa-test--with-captured-messages nil
          (niwa-plugins-check-updates t)
          (niwa-plugins-test--wait-idle))
        (let ((entry (niwa-plugins--update-entry-for-root dest)))
          (should (equal head (plist-get entry :remote)))
          (should (equal head (plist-get entry :local)))
          (should-not (plist-get entry :error)))
        (should-not (niwa-plugins-update-available-p
                     (niwa-plugins-find "noisy"))))))

  :doc "output that is not an object id is a failed check, not a cached commit"
  (let ((niwa-plugins-async-git-executor
         (lambda (_dir args callback)
           (funcall callback 0
                    (if (equal (car args) "ls-remote")
                        "12:00:01.5 trace: built-in\tHEAD"
                      "warning: something")))))
    (niwa-plugins-test--clone "a")
    (niwa-test--with-captured-messages nil
      (niwa-plugins-check-updates t))
    (should (niwa-plugins-update-check-error (niwa-plugins-find "a")))
    (should-not (niwa-plugins-update-available-p
                 (niwa-plugins-find "a")))))

(provide 'test-niwa-plugin-update-check)
;;; test-niwa-plugin-update-check.el ends here
