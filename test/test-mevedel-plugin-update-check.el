;;; test-mevedel-plugin-update-check.el -- Plugin update-check tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the opt-in plugin update check: scheduling, the async git seam,
;; once-per-commit announcements and persisted state.  The fake executor
;; stands in for the network; one test drives the real asynchronous
;; executor against a local file remote.

;;; Code:

(require 'cl-lib)
(require 'mevedel-plugin-lifecycle)
(require 'mevedel-plugin-registry)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-plugin-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-plugin-test-support"))

(defvar mevedel-plugins-test--calls nil
  "Git calls (DIRECTORY . ARGS) the fake async executor received.")

(defvar mevedel-plugins-test--pending nil
  "Deferred fake-executor answers, oldest last.")

(defun mevedel-plugins-test--clone (repo)
  "Create a managed plugin clone named REPO, with a `.git' marker."
  (let ((root (mevedel-plugins-test--github-install-root "o" repo)))
    (mevedel-plugins-test--write-manifest
     root (format "{\"name\":\"%s\"}" repo))
    (make-directory (file-name-concat root ".git") t)
    root))

(defun mevedel-plugins-test--executor (heads &optional defer)
  "Return a fake async executor answering from HEADS.
HEADS maps a repo name to (:remote SHA :local SHA), or (:fail MESSAGE).
With DEFER, answers wait in `mevedel-plugins-test--pending'."
  (lambda (directory args callback)
    (push (cons directory args) mevedel-plugins-test--calls)
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
          (setq mevedel-plugins-test--pending
                (append mevedel-plugins-test--pending (list answer)))
        (funcall answer)))))

(defun mevedel-plugins-test--wait-idle ()
  "Wait for the running update check to finish."
  (with-timeout (20 (error "Update check did not finish"))
    (while mevedel-plugins--update-check-busy
      (accept-process-output nil 0.05))))

(defun mevedel-plugins-test--git (directory &rest args)
  "Run real git ARGS in DIRECTORY and return trimmed output."
  (let ((default-directory directory))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil
                            "-c" "user.name=t" "-c" "user.email=t@t"
                            "-c" "commit.gpgsign=false" args))
        (error "Git %S failed: %s" args (buffer-string)))
      (string-trim (buffer-string)))))

(mevedel-deftest mevedel-plugins-check-updates
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-check-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (mevedel-plugins-update-check-interval nil)
           (mevedel-plugins--update-cache :unloaded)
           (mevedel-plugins--update-check-timer nil)
           (mevedel-plugins--update-check-busy nil)
           (mevedel-plugins-test--calls nil)
           (mevedel-plugins-test--pending nil)
           (messages nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "loading the modules and the default setting never reach the network"
  (let ((timers nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (push 'make-process mevedel-plugins-test--calls)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (mevedel-plugins-git-executor
               (lambda (&rest _) (push 'sync mevedel-plugins-test--calls) '(0 "")))
              (mevedel-plugins-async-git-executor
               (lambda (&rest _) (push 'async mevedel-plugins-test--calls))))
      (mevedel-plugins-test--clone "a")
      (load "mevedel-plugin-registry" nil t)
      (load "mevedel-plugin-lifecycle" nil t)
      (mevedel-plugins-list)
      (should-not mevedel-plugins-test--calls)
      (should-not timers)))

  :doc "an enabled interval schedules one idle check and still starts no git"
  (let ((mevedel-plugins-update-check-interval 1)
        (timers nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (mevedel-plugins-async-git-executor
               (lambda (&rest _) (push 'async mevedel-plugins-test--calls))))
      (mevedel-plugins-test--clone "a")
      (mevedel-plugins-list)
      (mevedel-plugins-list)
      (should (= 1 (length timers)))
      (should (> (car (car timers)) 0))
      (should-not mevedel-plugins-test--calls)))

  :doc "the last check time persists, so a restart inside the interval is quiet"
  (let ((mevedel-plugins-update-check-interval 1)
        (timers nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (&rest args) (push args timers) 'timer))
              (mevedel-plugins-async-git-executor
               (mevedel-plugins-test--executor nil)))
      (mevedel-plugins-test--clone "a")
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-check-updates))
      ;; Simulate a restart: only the file survives.
      (setq mevedel-plugins--update-cache :unloaded)
      (should-not (mevedel-plugins--update-check-due-p))
      (mevedel-plugins-list)
      (should-not timers)
      ;; Two days later the check is due again.
      (setq mevedel-plugins--update-cache
            (list :last-check (- (float-time) (* 2 86400))))
      (should (mevedel-plugins--update-check-due-p))))

  :doc "checks one plugin at a time and caches remote versus local HEAD"
  (let ((mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
            ("/b/" :remote "cccccccccccccccccccccccccccccccccccccccc" :local "cccccccccccccccccccccccccccccccccccccccc"))
          t)))
    (mevedel-plugins-test--clone "a")
    (mevedel-plugins-test--clone "b")
    (mevedel-test--with-captured-messages messages
      (mevedel-plugins-check-updates)
      ;; Only one git call is in flight until its answer arrives.
      (while mevedel-plugins-test--pending
        (should (= 1 (length mevedel-plugins-test--pending)))
        (funcall (pop mevedel-plugins-test--pending))))
    (should-not mevedel-plugins--update-check-busy)
    (should (equal '("ls-remote" "origin" "HEAD")
                   (cdr (car (last mevedel-plugins-test--calls)))))
    (should (= 4 (length mevedel-plugins-test--calls)))
    (should (equal "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" (mevedel-plugins-update-available-p
                          (mevedel-plugins-find "a"))))
    (should-not (mevedel-plugins-update-available-p
                 (mevedel-plugins-find "b")))
    (should (string-match-p
             "updates available for 1 plugin; /plugin to review" messages)))

  :doc "a second check is a no-op while one is running"
  (let ((mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")) t)))
    (mevedel-plugins-test--clone "a")
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-check-updates)
      (mevedel-plugins-check-updates t)
      (should (= 1 (length mevedel-plugins-test--calls)))
      (while mevedel-plugins-test--pending
        (funcall (pop mevedel-plugins-test--pending)))))

  :doc "each upstream commit is announced once, a newer one again"
  (let* ((heads (list (list "/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
         (mevedel-plugins-async-git-executor
          (lambda (&rest args)
            (apply (mevedel-plugins-test--executor heads) args))))
    (mevedel-plugins-test--clone "a")
    (mevedel-test--with-captured-messages messages
      (dotimes (_ 3) (mevedel-plugins-check-updates)))
    (should (equal "mevedel: updates available for 1 plugin; /plugin to review\n"
                   messages))
    (setcdr (car heads) (list :remote "dddddddddddddddddddddddddddddddddddddddd" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))
    (mevedel-test--with-captured-messages messages
      (mevedel-plugins-check-updates))
    (should (equal "mevedel: updates available for 1 plugin; /plugin to review\n"
                   messages)))

  :doc "an offline check stays silent and is shown as a failed check"
  (let ((mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor
          '(("/a/" :fail "fatal: unable to access")))))
    (mevedel-plugins-test--clone "a")
    (mevedel-test--with-captured-messages messages
      (mevedel-plugins-check-updates))
    (should (equal "" messages))
    (should (equal "git exited with status 128"
                   (mevedel-plugins-update-check-error
                    (mevedel-plugins-find "a"))))
    ;; Git's own output may carry credentials and must not reach the file.
    (should-not (string-match-p
                 "fatal" (with-temp-buffer
                           (insert-file-contents
                            (mevedel-plugins--update-state-file))
                           (buffer-string))))
    (should-not (mevedel-plugins-update-available-p
                 (mevedel-plugins-find "a"))))

  :doc "a non-numeric persisted last-check time counts as due"
  (let ((mevedel-plugins-update-check-interval 1))
    (setq mevedel-plugins--update-cache (list :last-check "yesterday"))
    (should (mevedel-plugins--update-check-due-p)))

  :doc "a callback that signals does not leave later checks blocked"
  (let ((mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor
          '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")) t)))
    (mevedel-plugins-test--clone "a")
    (cl-letf (((symbol-function 'mevedel-plugins--update-set-entry)
               (lambda (&rest _) (error "Disk full"))))
      (mevedel-plugins-check-updates)
      (funcall (pop mevedel-plugins-test--pending))
      (should-error (funcall (pop mevedel-plugins-test--pending))))
    (should-not mevedel-plugins--update-check-busy)
    (let ((before (length mevedel-plugins-test--calls)))
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-check-updates))
      (should (> (length mevedel-plugins-test--calls) before))))

  :doc "the on-demand check ignores the nil interval and reports its outcome"
  (let ((mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor
          '(("/a/" :remote "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"))))
        (done nil))
    (mevedel-plugins-test--clone "a")
    (mevedel-test--with-captured-messages messages
      (mevedel-plugins-check-updates t (lambda () (setq done t))))
    (should done)
    (should (= 2 (length mevedel-plugins-test--calls)))
    (should (string-match-p "checked 1 plugin, 0 with updates available"
                            messages)))

  :doc "unmanaged and non-clone plugins are not checked"
  (let ((external (file-name-concat root "external"))
        (mevedel-plugin-extra-roots nil)
        (mevedel-plugins-async-git-executor
         (mevedel-plugins-test--executor nil)))
    (setq mevedel-plugin-extra-roots (list external))
    (mevedel-plugins-test--write-manifest external "{\"name\":\"ext\"}")
    (make-directory (file-name-concat external ".git") t)
    (mevedel-plugins-test--write-manifest
     (mevedel-plugins-test--github-install-root "o" "plain")
     "{\"name\":\"plain\"}")
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-check-updates t))
    (should-not mevedel-plugins-test--calls))

  :doc "the cached update and its notice survive a restart, and an update clears them"
  (let* ((heads '(("/a/" :remote "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" :local "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")))
         (plugin-root (mevedel-plugins-test--clone "a"))
         (mevedel-plugins-async-git-executor
          (mevedel-plugins-test--executor heads))
         (mevedel-plugins-git-executor (lambda (&rest _) (list 0 "ok"))))
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-check-updates))
    (setq mevedel-plugins--update-cache :unloaded)
    (should (mevedel-plugins-update-available-p (mevedel-plugins-find "a")))
    (should (equal "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb" (plist-get (mevedel-plugins--update-entry-for-root
                                     plugin-root)
                                    :notified)))
    (mevedel-plugins-update "a" (mevedel-plugins-test--workspace root))
    (should-not (mevedel-plugins-update-available-p
                 (mevedel-plugins-find "a")))))

(mevedel-deftest mevedel-plugins--process-git-async-environment
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-env-" t))))
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
      (mevedel-plugins--process-git-async
       root '("ls-remote" "origin" "HEAD") (lambda (&rest _) (setq done t)))
      (with-timeout (10 (error "Executor did not finish"))
        (while (not done) (accept-process-output nil 0.05))))
    (dolist (entry '("GIT_TERMINAL_PROMPT=0" "GIT_ASKPASS=false"
                     "SSH_ASKPASS_REQUIRE=never" "GCM_INTERACTIVE=never"))
      (should (member entry seen-env)))
    (should (equal seen-command
                   '("git" "-c" "credential.interactive=never"
                     "ls-remote" "origin" "HEAD")))))

(mevedel-deftest mevedel-plugins--process-git-async
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-real-check-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (mevedel-plugins--update-cache :unloaded)
           (mevedel-plugins--update-check-timer nil)
           (mevedel-plugins--update-check-busy nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "detects a new commit through real git against a local remote"
  (progn
    (skip-unless (executable-find "git"))
    (let ((origin (file-name-concat root "origin"))
          (dest (mevedel-plugins-test--github-install-root "o" "real")))
      (mevedel-plugins-test--write-manifest origin "{\"name\":\"real\"}")
      (mevedel-plugins-test--git origin "init" "-q")
      (mevedel-plugins-test--git origin "add" ".")
      (mevedel-plugins-test--git origin "commit" "-q" "-m" "one")
      (make-directory (file-name-directory dest) t)
      (mevedel-plugins-test--git root "clone" "-q" origin dest)
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-check-updates t)
        (mevedel-plugins-test--wait-idle))
      (should-not (mevedel-plugins-update-check-error
                   (mevedel-plugins-find "real")))
      (should-not (mevedel-plugins-update-available-p
                   (mevedel-plugins-find "real")))
      (with-temp-file (file-name-concat origin "new") (insert "x"))
      (mevedel-plugins-test--git origin "add" ".")
      (mevedel-plugins-test--git origin "commit" "-q" "-m" "two")
      (mevedel-test--with-captured-messages nil
        (mevedel-plugins-check-updates t)
        (mevedel-plugins-test--wait-idle))
      (should (equal (mevedel-plugins-test--git origin "rev-parse" "HEAD")
                     (mevedel-plugins-update-available-p
                      (mevedel-plugins-find "real")))))))

(mevedel-deftest mevedel-plugins--process-git-async-diagnostics
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "mevedel-plugin-diag-" t)))
           (mevedel-user-dir root)
           (mevedel-plugin-install-directory
            (file-name-concat root ".agents" "plugins"))
           (mevedel-plugins--update-cache :unloaded)
           (mevedel-plugins--update-check-timer nil)
           (mevedel-plugins--update-check-busy nil))
   :after-each (delete-directory root t))
  ,test
  (test)
  :doc "git diagnostics on stderr never become cached commit ids"
  (progn
    (skip-unless (executable-find "git"))
    (let ((origin (file-name-concat root "origin"))
          (dest (mevedel-plugins-test--github-install-root "o" "noisy")))
      (mevedel-plugins-test--write-manifest origin "{\"name\":\"noisy\"}")
      (mevedel-plugins-test--git origin "init" "-q")
      (mevedel-plugins-test--git origin "add" ".")
      (mevedel-plugins-test--git origin "commit" "-q" "-m" "one")
      (make-directory (file-name-directory dest) t)
      (mevedel-plugins-test--git root "clone" "-q" origin dest)
      ;; A successful git that also writes trace lines to stderr.
      (let ((process-environment (cons "GIT_TRACE=1" process-environment))
            (head (mevedel-plugins-test--git origin "rev-parse" "HEAD")))
        (mevedel-test--with-captured-messages nil
          (mevedel-plugins-check-updates t)
          (mevedel-plugins-test--wait-idle))
        (let ((entry (mevedel-plugins--update-entry-for-root dest)))
          (should (equal head (plist-get entry :remote)))
          (should (equal head (plist-get entry :local)))
          (should-not (plist-get entry :error)))
        (should-not (mevedel-plugins-update-available-p
                     (mevedel-plugins-find "noisy"))))))

  :doc "output that is not an object id is a failed check, not a cached commit"
  (let ((mevedel-plugins-async-git-executor
         (lambda (_dir args callback)
           (funcall callback 0
                    (if (equal (car args) "ls-remote")
                        "12:00:01.5 trace: built-in\tHEAD"
                      "warning: something")))))
    (mevedel-plugins-test--clone "a")
    (mevedel-test--with-captured-messages nil
      (mevedel-plugins-check-updates t))
    (should (mevedel-plugins-update-check-error (mevedel-plugins-find "a")))
    (should-not (mevedel-plugins-update-available-p
                 (mevedel-plugins-find "a")))))

(provide 'test-mevedel-plugin-update-check)
;;; test-mevedel-plugin-update-check.el ends here
