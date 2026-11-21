;;; test-niwa-sandbox.el --- Tests for child confinement -*- lexical-binding: t -*-

;;; Commentary:

;; Tests pure Bubblewrap preparation and the optional real Linux backend.

;;; Code:

(require 'cl-lib)
(require 'niwa-mcp)
(require 'niwa-permission-rules)
(require 'niwa-sandbox)
;; Several cases stub `niwa-session-control-fs-run-program' and call the
;; captured original; a lazy autoload there would replace the stub mid-case.
(require 'niwa-session-control-fs)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar niwa-sandbox-probe-timeout)
(defvar niwa-sandbox-remote-probe-timeout)


;;
;;; Sandbox mode

(niwa-deftest niwa-sandbox-mode-normalize ()
  ,test
  (test)
  :doc "accepts only canonical symbols and strings"
  (dolist (mode '(best-effort required off))
    (should (eq mode (niwa-sandbox-mode-normalize mode)))
    (should (eq mode
                (niwa-sandbox-mode-normalize (symbol-name mode)))))
  (dolist (mode '(auto nil unknown))
    (should-error (niwa-sandbox-mode-normalize mode)
                  :type 'user-error)))

(niwa-deftest niwa-sandbox-mode--set ()
  ,test
  (test)
  :doc "updates only the current session from a session buffer"
  (let ((saved-mode (default-toplevel-value 'niwa-sandbox-mode))
        (buffer (generate-new-buffer " *niwa-sandbox-mode*")))
    (unwind-protect
        (let ((session (niwa-session--create
                        :name "sandbox" :sandbox-mode 'best-effort)))
          (set-default-toplevel-value 'niwa-sandbox-mode 'best-effort)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (niwa-sandbox-mode--set 'niwa-sandbox-mode 'required))
          (should (eq 'required (niwa-session-sandbox-mode session)))
          (should (eq 'required
                      (buffer-local-value 'niwa-sandbox-mode buffer)))
          (should (eq 'best-effort
                      (default-toplevel-value 'niwa-sandbox-mode))))
      (set-default-toplevel-value 'niwa-sandbox-mode saved-mode)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(niwa-deftest niwa-sandbox/cold-load ()
  ,test
  (test)
  :doc "keeps explicit session and global policies when first loaded in a session"
  (dolist (mode '(off required))
    (with-temp-buffer
      (let ((status
             (call-process
              (expand-file-name invocation-name invocation-directory)
              nil t nil "--batch" "-Q" "--eval"
              (prin1-to-string
               `(progn
                  (setq load-path ',load-path)
                  (require 'niwa-structs)
                  (when (featurep 'niwa-sandbox)
                    (error "Sandbox must start unloaded"))
                  (set-default 'niwa-sandbox-mode 'best-effort)
                  (with-temp-buffer
                    (setq-local niwa--session
                                (niwa-session--create :sandbox-mode ',mode))
                    (require 'niwa-sandbox)
                    (unless (and (eq 'best-effort
                                     (default-value 'niwa-sandbox-mode))
                                 (eq ',mode (niwa-session-sandbox-mode
                                             niwa--session))
                                 (not (local-variable-p 'niwa-sandbox-mode)))
                      (error "Cold loading changed explicit %s policy" ',mode))
                    (set-default 'niwa-sandbox-mode ',mode)
                    (setopt niwa-sandbox-mode 'best-effort)
                    (unless (and (eq 'best-effort
                                     (niwa-session-sandbox-mode niwa--session))
                                 (eq ',mode (default-value 'niwa-sandbox-mode)))
                      (error "Explicit customization lost session scoping"))))))))
        (ert-info ((buffer-string))
          (should (equal 0 status))
          (should (equal "" (buffer-string))))))))

(niwa-deftest niwa-sandbox-mode--get ()
  ,test
  (test)
  :doc "reads the current session independently of the global default"
  (let ((saved-mode (default-toplevel-value 'niwa-sandbox-mode))
        (buffer (generate-new-buffer " *niwa-sandbox-mode*")))
    (unwind-protect
        (let ((session (niwa-session--create
                        :name "sandbox" :sandbox-mode 'required)))
          (set-default-toplevel-value 'niwa-sandbox-mode 'best-effort)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (should
             (eq 'required
                 (niwa-sandbox-mode--get 'niwa-sandbox-mode)))))
      (set-default-toplevel-value 'niwa-sandbox-mode saved-mode)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(niwa-deftest niwa-sandbox-mode-effective ()
  ,test
  (test)
  :doc "prefers explicit session state and rejects retired values"
  (let ((session (niwa-session--create :sandbox-mode 'off)))
    (should (eq 'off (niwa-sandbox-mode-effective session)))
    (setf (niwa-session-sandbox-mode session) 'auto)
    (should-error (niwa-sandbox-mode-effective session)
                  :type 'user-error))
  :doc "permission modes derive confinement without changing the preference"
  (let ((session (niwa-session--create
                  :permission-mode 'edits :sandbox-mode 'best-effort)))
    (should (eq 'required (niwa-sandbox-mode-effective session)))
    (setf (niwa-session-permission-mode session) 'full-auto)
    (should (eq 'off (niwa-sandbox-mode-effective session)))
    (should (eq 'best-effort (niwa-session-sandbox-mode session)))
    (setf (niwa-session-permission-mode session) 'ask)
    (should (eq 'best-effort (niwa-sandbox-mode-effective session)))
    (setf (niwa-session-sandbox-mode session) 'off
          (niwa-session-permission-mode session) 'edits)
    (should (eq 'required (niwa-sandbox-mode-effective session)))))


;;
;;; Sandbox preparation

(niwa-deftest niwa-sandbox--probe-command ()
  ,test
  (test)
  :doc "Bubblewrap probe argv:
`niwa-sandbox--probe-command' uses the supplied executable and core profile"
  (let ((command (niwa-sandbox--probe-command "/test/bwrap")))
    (should (equal (car command) "/test/bwrap"))
    (should (member "--ro-bind" command))
    (should (member "--unshare-user" command))
    (should (member "--unshare-pid" command))
    (should (member "--unshare-net" command))
    (should (member "--proc" command))))

(niwa-deftest niwa-sandbox-probe ()
  ,test
  (test)
  :doc "cached probe:
`niwa-sandbox-probe' reuses the first real availability decision"
  (let* ((cached '(:available nil :reason "cached result"))
         (niwa-sandbox--probe-cache (list (cons nil cached))))
    (should (eq (niwa-sandbox-probe) cached)))
  :doc "target cache:
`niwa-sandbox-probe' caches local and remote availability independently"
  (let ((niwa-sandbox--probe-cache nil)
        calls)
    (skip-unless (eq system-type 'gnu/linux))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _) "/test/bwrap"))
              ((symbol-function 'niwa-sandbox--run-probe)
               (lambda (_command &optional workdir)
                 (push workdir calls)
                 '(:status 0))))
      (niwa-sandbox-probe)
      (niwa-sandbox-probe "/ssh:user@host:/srv/project/")
      (niwa-sandbox-probe)
      (niwa-sandbox-probe "/ssh:user@host:/srv/other/")
      (should (= 2 (length calls)))
      (should (= 2 (length niwa-sandbox--probe-cache)))))
  :doc "remote timeout:
`niwa-sandbox--run-probe' does not apply the local timeout over TRAMP"
  (let* ((root (make-temp-file "niwa-sandbox-remote-timeout-" t))
         (remote-root
          (format "/niwamock:%s:%s/" (system-name) root))
         (niwa-sandbox-probe-timeout 0.001)
         (niwa-sandbox-remote-probe-timeout 2))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (let ((result
                 (niwa-sandbox--run-probe
                  '("sh" "-c" "sleep 0.05") remote-root)))
            (should-not (plist-get result :timed-out-p))))
      (delete-directory root t)))
  :doc "local target:
`niwa-sandbox--run-probe' runs a local probe locally from a remote buffer"
  (let ((default-directory "/ssh:user@host:/srv/project/")
        (make (symbol-function 'make-process))
        directory)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq directory default-directory)
                 (apply make args))))
      (niwa-sandbox--run-probe '("true")))
    (should (stringp directory))
    (should-not (file-remote-p directory)))
  :doc "host probe:
`niwa-sandbox-probe' always returns explicit availability facts"
  (let ((niwa-sandbox--probe-cache nil))
    (let ((result (niwa-sandbox-probe)))
      (should (plist-member result :available))
      (if (plist-get result :available)
          (should (stringp (plist-get result :executable)))
        (should (stringp (plist-get result :reason))))))
  :doc "bounded probe:
`niwa-sandbox-probe' terminates a stalled Bubblewrap candidate"
  (let* ((root (make-temp-file "niwa-sandbox-probe-timeout-" t))
         (executable (file-name-concat root "bwrap"))
         (exec-path (list root))
         (niwa-sandbox--probe-cache nil)
         (niwa-sandbox-probe-timeout 0.05)
         (clock 100.0)
         waits result)
    (skip-unless (eq system-type 'gnu/linux))
    (unwind-protect
        (progn
          (with-temp-file executable
            (insert "#!/bin/sh\nexec /bin/sleep 30\n"))
          (set-file-modes executable #o700)
          ;; The probe's isolated wait suppresses timers, including any Lisp
          ;; watchdog.  Advance its clock through the requested wait instead,
          ;; and prove both the configured budget and deadline termination.
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) clock))
                    ((symbol-function 'accept-process-output)
                     (lambda (process seconds &optional _milliseconds just-this-one)
                       (when (process-live-p process)
                         (when waits (error "Probe waited beyond its deadline"))
                         (push (list seconds just-this-one) waits)
                         (setq clock (+ clock seconds))))))
            (setq result (niwa-sandbox-probe)))
          (should (= 1 (length waits)))
          (should (< (abs (- 0.05 (caar waits))) 0.000001))
          (should (equal 1 (cadar waits)))
          (should-not (plist-get result :available))
          (should (string-match-p "timed out" (plist-get result :reason)))
          ;; A probe that never finished proves nothing about the backend.
          (should (plist-get result :retry-on-execution)))
      (delete-directory root t)))
  :doc "isolated wait:
`niwa-sandbox-probe' runs no timers while it waits for the candidate"
  (let* ((root (make-temp-file "niwa-sandbox-probe-isolated-" t))
         (executable (file-name-concat root "bwrap"))
         (exec-path (list root))
         (niwa-sandbox--probe-cache nil)
         (niwa-sandbox-probe-timeout 2)
         ran timer result)
    (skip-unless (eq system-type 'gnu/linux))
    (unwind-protect
        (progn
          (with-temp-file executable
            (insert "#!/bin/sh\nexec /bin/sleep 0.05\n"))
          (set-file-modes executable #o700)
          ;; Stands in for a remote command that a timer would run here and
          ;; charge to the probe's deadline.
          (setq timer (run-at-time 0 nil (lambda () (setq ran t))))
          (setq result (niwa-sandbox-probe))
          (should (plist-get result :available))
          (should-not ran))
      (cancel-timer timer)
      (delete-directory root t)))
  :doc "bounded diagnostics:
`niwa-sandbox-probe' caps output from a noisy Bubblewrap candidate"
  (let* ((root (make-temp-file "niwa-sandbox-probe-output-" t))
         (executable (file-name-concat root "bwrap"))
         (exec-path (list root))
         (niwa-sandbox--probe-cache nil)
         result)
    (skip-unless (eq system-type 'gnu/linux))
    (unwind-protect
        (progn
          (with-temp-file executable
            (insert "#!/bin/sh\nprintf '%s' '"
                    (make-string (* 128 1024) ?x)
                    "'\nexit 1\n"))
          (set-file-modes executable #o700)
          (setq result (niwa-sandbox-probe))
          (should-not (plist-get result :available))
          (should (< (length (plist-get result :reason)) 70000)))
      (delete-directory root t)))
  :doc "fresh proc fallback:
`niwa-sandbox-probe' retains confinement when only `--proc' is rejected"
  (let* ((root (make-temp-file "niwa-sandbox-probe-proc-" t))
         (executable (file-name-concat root "bwrap"))
         (exec-path (list root))
         (niwa-sandbox--probe-cache nil)
         result)
    (skip-unless (eq system-type 'gnu/linux))
    (unwind-protect
        (progn
          (with-temp-file executable
            (insert (concat "#!/bin/sh\n"
                            "for argument in \"$@\"; do\n"
                            "  [ \"$argument\" = --proc ] && exit 1\n"
                            "done\n"
                            "exit 0\n")))
          (set-file-modes executable #o700)
          (setq result (niwa-sandbox-probe))
          (should (plist-get result :available))
          (should-not (plist-get result :mount-proc)))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox-invalidate-probe-cache ()
  ,test
  (test)
  :doc "clears only the cache belonging to the requested execution target"
  (let ((niwa-sandbox--probe-cache
         '(("/ssh:user@host:" . (:available t))
           (nil . (:available t)))))
    (niwa-sandbox-invalidate-probe-cache
     "/ssh:user@other:/srv/project/")
    (should niwa-sandbox--probe-cache)
    (niwa-sandbox-invalidate-probe-cache
     "/ssh:user@host:/srv/project/")
    (should (= 1 (length niwa-sandbox--probe-cache)))
    (should (alist-get nil niwa-sandbox--probe-cache))))

(niwa-deftest niwa-sandbox--canonical-directories ()
  ,test
  (test)
  :doc "canonical writable roots:
`niwa-sandbox--canonical-directories' drops missing and duplicate roots"
  (let* ((root (make-temp-file "niwa-sandbox-canonical-" t))
         (missing (file-name-concat root "missing")))
    (unwind-protect
        (should
         (equal (niwa-sandbox--canonical-directories
                 (list root root missing nil))
                (list (file-name-as-directory (file-truename root)))))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--first-missing-path ()
  ,test
  (test)
  :doc "first missing component:
`niwa-sandbox--first-missing-path' returns the narrow mount target"
  (let ((root (make-temp-file "niwa-sandbox-missing-" t)))
    (unwind-protect
        (should
         (equal (niwa-sandbox--first-missing-path
                 (file-name-concat root "absent" "child"))
                (file-name-concat root "absent")))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--writable-symlink-component ()
  ,test
  (test)
  :doc "writable symlink crossing:
`niwa-sandbox--writable-symlink-component' identifies mutable indirection"
  (let* ((root (make-temp-file "niwa-sandbox-symlink-" t))
         (outside (make-temp-file "niwa-sandbox-symlink-target-" t))
         (link (file-name-concat root "link")))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should
           (equal (niwa-sandbox--writable-symlink-component
                   (file-name-concat link "secret") (list root))
                  link))
          (should-not
           (niwa-sandbox--writable-symlink-component
            (file-name-concat link "secret") (list outside))))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "writable symlink crossing: root boundaries, earliest crossing and fresh checks"
  (let* ((root (make-temp-file "niwa-sandbox-crossings-" t))
         (target (file-name-concat root "target"))
         (link (file-name-concat root "link"))
         (nested (file-name-concat target "nested")))
    (unwind-protect
        (progn
          (make-directory target)
          (make-symbolic-link target link)
          (make-symbolic-link target nested)
          (should (equal link
                         (niwa-sandbox--writable-symlink-component
                          (file-name-concat link "nested" "missing")
                          (list link root))))
          (should (equal link
                         (niwa-sandbox--writable-symlink-component
                          (file-name-as-directory link) (list link))))
          (should-not (niwa-sandbox--writable-symlink-component
                       link (list (concat root "-other"))))
          (delete-file link)
          (make-directory link)
          (should-not (niwa-sandbox--writable-symlink-component
                       (file-name-concat link "missing") (list root)))
          (delete-directory link)
          (make-symbolic-link target link)
          (should (equal link
                         (niwa-sandbox--writable-symlink-component
                          (file-name-concat link "missing") (list root)))))
      (delete-directory root t)))
  :doc "a shared table tests each prefix once and still finds a new symlink"
  (let* ((root (make-temp-file "niwa-sandbox-checked-" t))
         (deep (file-name-concat root "a" "b" "c"))
         (checked (make-hash-table :test #'equal))
         (probes 0)
         (symlink-p (symbol-function 'file-symlink-p)))
    (unwind-protect
        (cl-letf (((symbol-function 'file-symlink-p)
                   (lambda (file) (cl-incf probes) (funcall symlink-p file))))
          (make-directory deep t)
          (should-not (niwa-sandbox--writable-symlink-component
                       (file-name-concat deep "one") (list root) checked))
          (let ((first probes))
            (should-not (niwa-sandbox--writable-symlink-component
                         (file-name-concat deep "two") (list root) checked))
            ;; Only the new leaf is tested again.
            (should (= 1 (- probes first))))
          (make-symbolic-link deep (file-name-concat deep "link"))
          (should (equal (file-name-concat deep "link")
                         (niwa-sandbox--writable-symlink-component
                          (file-name-concat deep "link" "x") (list root) checked))))
      (delete-directory root t)))
  :doc "writable symlink checks preserve the remote path domain"
  (let* ((root (make-temp-file "niwa-sandbox-remote-link-" t))
         (target (file-name-concat root "target"))
         (link (file-name-concat root "link"))
         (prefix "/niwamock:sandbox-check:"))
    (unwind-protect
        (progn
          (make-directory target)
          (make-symbolic-link target link)
          (niwa-test--with-local-shell-tramp '("sandbox-check")
            (should (equal (concat prefix link)
                           (niwa-sandbox--writable-symlink-component
                            (concat prefix link "/missing")
                            (list (concat prefix root)))))))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--git-pointer-targets ()
  ,test
  (test)
  :doc "Git directory pointer:
`niwa-sandbox--git-pointer-targets' resolves a relative worktree target"
  (let* ((root (make-temp-file "niwa-sandbox-gitdir-" t))
         (checkout (file-name-concat root "checkout"))
         (metadata (file-name-concat root "metadata"))
         (pointer (file-name-concat checkout ".git")))
    (unwind-protect
        (progn
          (make-directory checkout)
          (make-directory metadata)
          (with-temp-file pointer
            (insert "gitdir: ../metadata\n"))
          (should
           (equal (niwa-sandbox--git-pointer-targets pointer)
                  (list metadata)))
          (with-temp-file (file-name-concat metadata "commondir")
            (insert "..\n"))
          (should
           (equal (niwa-sandbox--git-pointer-targets pointer)
                  (list metadata root))))
      (delete-directory root t)))
  :doc "Git pointer filenames retain Unicode without editor coding hooks"
  (let* ((root (make-temp-file "niwa-sandbox-git-coding-" t))
         (metadata (file-name-concat root (string #xe9)))
         (pointer (file-name-concat root ".git"))
         (calls 0))
    (unwind-protect
        (progn
          (make-directory metadata)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file pointer (insert "gitdir: " (string #xe9) "\n"))
            (with-temp-file (file-name-concat metadata "commondir") (insert "..\n")))
          (let ((auto-coding-functions (list (lambda (_) (cl-incf calls) nil))))
            (should (equal (niwa-sandbox--git-pointer-targets pointer)
                           (list metadata root))))
          (should (zerop calls)))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--find-protected-directory ()
  (progn
    (skip-unless (and (eq system-type 'gnu/linux) (executable-find "find")))
    (let* ((root (make-temp-file "niwa-native-discovery-" t))
           (outside (make-temp-file "niwa-native-outside-" t))
           (nested (file-name-concat root "space and\nnewline" ".git"))
           (worktree (file-name-concat root "worktree" ".git"))
           (blocked (file-name-concat root "unreadable")))
      (unwind-protect
          (progn
            (make-directory (file-name-concat nested "hidden" ".git") t)
            (make-directory (file-name-directory worktree) t)
            (with-temp-file worktree (insert "gitdir: elsewhere\n"))
            (make-directory (file-name-concat outside "nested" ".git") t)
            (make-symbolic-link outside (file-name-concat root "alias"))
            (make-directory blocked)
            (set-file-modes blocked #o000)
            (let ((rows (niwa-sandbox--find-protected-directory root ".git" (executable-find "find"))))
              (should (assoc nested rows))
              (should (assoc worktree rows))
              (should-not (assoc (file-name-concat nested "hidden" ".git") rows))
              (should-not (assoc (file-name-concat root "alias" "nested" ".git") rows))
              (unless (file-readable-p blocked) (should (cdr (assoc blocked rows)))))
            (make-symbolic-link root (file-name-concat outside "root-link"))
            (should (assoc (file-name-concat outside "root-link" "space and\nnewline" ".git")
                           (niwa-sandbox--find-protected-directory
                            (file-name-concat outside "root-link") ".git" (executable-find "find"))))
            (let ((literal (file-name-concat root "back\\slash")))
              (make-directory literal)
              (should (assoc literal (niwa-sandbox--find-protected-directory
                                      root "back\\slash" (executable-find "find")))))
            (when (executable-find "false")
              (should-error (niwa-sandbox--find-protected-directory root ".git" (executable-find "false"))
                            :type 'niwa-sandbox-policy-error)))
        (set-file-modes blocked #o700)
        (delete-directory root t)
        (delete-directory outside t)))))

(niwa-deftest niwa-sandbox--protected-candidates ()
  ,test
  (test)
  :doc "protects niwa's state directories at each root, once, without walking"
  (dolist (name '("niwa-sandbox-store-" "niwa-sandbox-[store]-"))
    (let* ((root (file-name-as-directory (make-temp-file name t)))
           (store (file-name-concat root ".niwa" "artifacts" ".state" "board"))
           (deep (file-name-concat root "sub" ".niwa" "artifacts" "x"))
           walked
           (niwa-protected-paths
            '(("**/.niwa/artifacts/.state/**" . read-only)
              ("**/.niwa/leases/**" . read-only))))
      (unwind-protect
          (progn
            (make-directory (file-name-concat store "versions") t)
            (make-directory deep t)
            (write-region "{}" nil (file-name-concat store "state.json") nil 'silent)
            (let ((candidates
                   (cl-letf (((symbol-function 'directory-files-recursively)
                              (lambda (&rest _) (setq walked t) nil)))
                     (niwa-sandbox--protected-candidates root (list root)))))
              (should-not walked)
              ;; One mount per directory, however many artifacts, including
              ;; the lease directory that does not exist yet.
              (should (equal (sort (mapcar (lambda (item)
                                             (file-relative-name (plist-get item :path) root))
                                           candidates)
                                   #'string<)
                             '(".niwa/artifacts/.state" ".niwa/leases")))
              (should (cl-every (lambda (item) (eq 'read-only (plist-get item :mode)))
                                candidates))))
        (delete-directory root t))))

  :doc "protects a file below .niwa as a file, creating nothing for it"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-sandbox-anchor-" t)))
         (scratch (file-name-as-directory (make-temp-file "niwa-sandbox-scratch-" t)))
         (niwa-protected-paths
          '(("**/.niwa/input-history.el" . read-only)
            ("**/.niwa/leases/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".niwa") t)
          (let ((candidates (niwa-sandbox--protected-candidates root (list root))))
            (should (equal (list (list :path (file-name-concat root ".niwa/input-history.el")
                                       :mode 'read-only :directory-p nil)
                                 (list :path (file-name-concat root ".niwa/leases")
                                       :mode 'read-only :directory-p t :create-parents t))
                           (sort candidates (lambda (a b) (string< (plist-get a :path)
                                                                   (plist-get b :path))))))
            ;; Nothing is made where the file belongs.
            (should-not (file-exists-p (file-name-concat root ".niwa/input-history.el")))))
      (delete-directory root t)
      (delete-directory scratch t)))

  :doc "retains missing literal state roots without walking the workspace"
  (let* ((root (make-temp-file "niwa-sandbox-missing-state-" t))
         (niwa-protected-paths '(("**/.niwa/artifacts/.state/**" . read-only)
                                   ("**/.niwa/leases/**" . read-only))))
    (unwind-protect
        (let ((candidates (niwa-sandbox--protected-candidates root (list root))))
          (should (= 2 (length candidates)))
          (dolist (candidate candidates)
            (should (plist-get candidate :create-parents))
            (should-not (file-exists-p (plist-get candidate :path)))))
      (delete-directory root t)))

  :doc "protected glob expansion:
`niwa-sandbox--protected-candidates' finds concrete and missing roots"
  (let* ((root (make-temp-file "niwa-sandbox-candidates-" t))
         (secondary (make-temp-file "niwa-sandbox-secondary-" t))
         (nested (file-name-concat root "nested"))
         (dot-git (file-name-concat nested ".git"))
         (secondary-dot-git (file-name-concat secondary "nested" ".git"))
         (unreadable (file-name-concat secondary "unreadable"))
         (credentials (file-name-concat root "credentials"))
         (missing (file-name-concat root "missing"))
         scanned-patterns
         (niwa-protected-paths
          `(("**/.git/**" . read-only)
            (,(concat credentials "/**") . inaccessible)
            (,(concat missing "/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-directory dot-git t)
          (make-directory secondary-dot-git t)
          (make-directory unreadable)
          (set-file-modes unreadable #o000)
          (make-directory credentials)
          (let* ((original
                  (symbol-function 'directory-files-recursively))
                 (candidates
                  (cl-letf
                      (((symbol-function 'directory-files-recursively)
                        (lambda (directory regexp &rest arguments)
                          (push regexp scanned-patterns)
                          (apply original directory regexp arguments))))
                    (niwa-sandbox--protected-candidates
                     root (list root secondary)))))
            (should-not (member "." scanned-patterns))
            (should (cl-find dot-git candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))
            (should (cl-find secondary-dot-git candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))
            (unless (file-readable-p unreadable)
              (should
               (eq (plist-get
                    (cl-find unreadable candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal)
                    :mode)
                   'inaccessible)))
            (should (eq (plist-get
                         (cl-find credentials candidates
                                  :key (lambda (item) (plist-get item :path))
                                  :test #'string-equal)
                         :mode)
                        'inaccessible))
            (should (cl-find missing candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))))
      (delete-directory root t)
      (set-file-modes unreadable #o700)
      (delete-directory secondary t)))

  :doc "temporary root exemption:
`niwa-sandbox--protected-candidates' never walks the temporary root"
  (let* ((root (make-temp-file "niwa-sandbox-candidates-" t))
         (scratch (make-temp-file "niwa-sandbox-scratch-" t))
         (dot-git (file-name-concat root "nested" ".git"))
         (scratch-dot-git (file-name-concat scratch "nested" ".git"))
         scanned-roots
         (niwa-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory dot-git t)
          (make-directory scratch-dot-git t)
          (let* ((original (symbol-function 'directory-files-recursively))
                 (candidates
                  (cl-letf
                      (((symbol-function 'directory-files-recursively)
                        (lambda (directory regexp &rest arguments)
                          (push (file-name-as-directory
                                 (file-truename directory))
                                scanned-roots)
                          (apply original directory regexp arguments))))
                    (niwa-sandbox--protected-candidates
                     root (list root scratch) scratch))))
            (should (cl-find dot-git candidates
                             :key (lambda (item) (plist-get item :path))
                             :test #'string-equal))
            (should-not (cl-find scratch-dot-git candidates
                                 :key (lambda (item) (plist-get item :path))
                                 :test #'string-equal))
            (should-not (member (file-name-as-directory
                                 (file-truename scratch))
                                scanned-roots))))
      (delete-directory root t)
      (delete-directory scratch t))))

(niwa-deftest niwa-sandbox-cleanup ()
  ,test
  (test)
  :doc "owned mount target cleanup:
a busy transport defers the complete cleanup"
  (let (deferred cleaned)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (_key _path thunk &optional _on-cancel)
                 (setq deferred thunk)
                 t))
              ((symbol-function 'niwa-sandbox--cleanup-now)
               (lambda (preparation) (setq cleaned preparation))))
      (let ((preparation '(:cleanup-paths ((:path "/remote/target")))))
        (should (niwa-sandbox-cleanup preparation))
        (should-not cleaned)
        (funcall deferred)
        (should (equal preparation cleaned)))))
  :doc "owned mount target cleanup:
a disabled transport cleans immediately instead of dropping work"
  (let (cleaned)
    (cl-letf (((symbol-function 'niwa-transport-busy-p)
               (lambda (&optional _) t))
              ((symbol-function 'niwa-transport-run-when-idle)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-sandbox--cleanup-now)
               (lambda (preparation) (setq cleaned preparation))))
      (let ((preparation '(:cleanup-paths ((:path "/remote/target")))))
        (should (niwa-sandbox-cleanup preparation))
        (should (equal preparation cleaned)))))
  :doc "owned mount target cleanup:
`niwa-sandbox-cleanup' removes an unchanged empty synthetic directory"
  (let* ((root (make-temp-file "niwa-sandbox-cleanup-" t))
         (path (file-name-concat root "synthetic")))
    (unwind-protect
        (progn
          (make-directory path)
          (let ((inode (file-attribute-inode-number
                        (file-attributes path 'string))))
            (niwa-sandbox-cleanup
             (list :cleanup-paths (list (list :path path :inode inode)))))
          (should-not (file-exists-p path)))
      (delete-directory root t)))
  :doc "owned mount target cleanup: retains nonempty directories without changing modes"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-cleanup-nonempty-" t)))
         (path (file-name-concat root "synthetic")))
    (unwind-protect
        (progn
          (make-directory path)
          (write-region "Keep" nil (file-name-concat path "user-file") nil 'silent)
          (set-file-modes path #o750)
          (niwa-sandbox-cleanup
           (list :cleanup-paths
                 (list (list :path path :inode (file-attribute-inode-number (file-attributes path))))))
          (should (file-exists-p (file-name-concat path "user-file")))
          (should (= #o750 (file-modes path))))
      (delete-directory root t)))
  :doc "owned mount target cleanup: parent replacement after identity check cannot redirect removal"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-cleanup-parent-" t)))
         (outside (make-temp-file "niwa-sandbox-cleanup-foreign-" t))
         (branch (file-name-concat root "branch"))
         (path (file-name-concat branch "synthetic"))
         (foreign (file-name-concat outside "synthetic"))
         (directory-p (symbol-function 'file-directory-p))
         swapped diagnostics)
    (unwind-protect
        (progn
          (make-directory path t)
          (make-directory foreign)
          (set-file-modes foreign #o750)
          (let ((preparation
                 (list :cleanup-paths
                       (list (list :path path :inode (file-attribute-inode-number (file-attributes path)))))))
            (cl-letf (((symbol-function 'file-directory-p)
                       (lambda (file)
                         (prog1 (funcall directory-p file)
                           (when (and (equal file path) (not swapped))
                             (setq swapped t)
                             (rename-file branch (file-name-concat root "moved"))
                             (make-symbolic-link outside branch))))))
              (niwa-test--with-captured-diagnostics diagnostics
                (niwa-sandbox-cleanup preparation))))
          (should swapped)
          (should (string-match-p "Could not remove sandbox mount target" diagnostics))
          (should (file-directory-p foreign))
          (should (= #o750 (file-modes foreign))))
      (delete-directory root t)
      (delete-directory outside t))))

(niwa-deftest niwa-sandbox--protected-physical-path ()
  ,test
  (test)
  :doc "creates private directories on local and target paths without widening lexical roots"
  (let ((parent (file-truename (make-temp-file "niwa-sandbox-pinned-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("sandbox-pinned")
          (dolist (prefix '("" "/niwamock:sandbox-pinned:"))
            (let* ((root (file-name-concat parent (if (equal prefix "") "local" "remote")))
                   (path (file-name-concat root "state"))
                   (roots (list (cons (concat prefix root) (concat prefix root)))))
              (make-directory root)
              (should (equal (concat prefix path)
                             (niwa-sandbox--protected-physical-path (concat prefix path) roots)))
              (niwa-sandbox--protected-restrictions
               (concat prefix root) (list (concat prefix root)) nil
               (list (list (list :path (concat prefix path) :mode 'read-only
                                 :directory-p t :create-parents t))))
              (should (file-directory-p path))
              (should (= #o700 (file-modes path)))
              (should-error
               (niwa-sandbox--protected-physical-path (concat prefix root "-sibling") roots))
              (should-not (file-exists-p (concat root "-sibling"))))))
      (delete-directory parent t)))
  :doc "uses captured canonical root spellings when an immutable ancestor is aliased"
  (let* ((parent (file-truename (make-temp-file "niwa-sandbox-pinned-alias-" t)))
         (real (file-name-concat parent "real"))
         (alias (file-name-concat parent "alias"))
         (root (file-name-concat alias "work")))
    (unwind-protect
        (progn
          (make-directory (file-name-concat real "work") t)
          (make-symbolic-link real alias)
          (should (equal (file-name-concat real "work/state")
                         (niwa-sandbox--protected-physical-path
                          (file-name-concat root "state")
                          (list (cons root (file-name-concat real "work"))))))
          (niwa-sandbox--protected-restrictions
           root (list root) nil
           (list (list (list :path (file-name-concat root "state") :mode 'read-only
                             :directory-p t :create-parents t))))
          (should (= #o700 (file-modes (file-name-concat root "state")))))
      (delete-directory parent t))))

(niwa-deftest niwa-sandbox--protected-restrictions-batches ()
  ,test
  (test)
  :doc "a fresh helper's five private directories use one creation and one cleanup program"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-helper-batch-" t)))
         (names '(".git" ".niwa" ".niwa/artifacts" ".niwa/artifacts/.state" ".niwa/leases"))
         (candidates (list (list :path (file-name-concat root ".git") :mode 'read-only :directory-p t)
                           (list :path (file-name-concat root ".niwa/artifacts/.state")
                                 :mode 'read-only :directory-p t :create-parents t)
                           (list :path (file-name-concat root ".niwa/leases")
                                 :mode 'read-only :directory-p t :create-parents t)))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         programs preparation)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (operations)
                     (push (copy-tree operations) programs)
                     (funcall run operations))))
          (setq preparation (niwa-sandbox--protected-restrictions root (list root) nil (list candidates)))
          (should (= 1 (length programs)))
          (should (= 10 (length (car programs))))
          (should (equal (mapcar (lambda (operation) (plist-get operation :path))
                                (seq-filter (lambda (operation) (eq (plist-get operation :op) 'make-directory))
                                            (car programs)))
                         (mapcar (lambda (name) (file-name-concat root name)) names)))
          (dolist (name names)
            (should (= #o700 (file-modes (file-name-concat root name)))))
          (niwa-sandbox-cleanup preparation)
          (should (= 2 (length programs)))
          (should (equal (car programs)
                         (list (list :op 'delete-empty-directory :path (file-name-concat root ".git") :optional t))))
          (should-not (file-exists-p (file-name-concat root ".git")))
          (dolist (name (cdr names))
            (should (file-directory-p (file-name-concat root name)))))
      (delete-directory root t)))
  :doc "planned aliases preserve candidate-order coarsening and promote required ancestors"
  (let* ((parent (file-truename (make-temp-file "niwa-sandbox-plan-alias-" t)))
         (real (file-name-concat parent "real"))
         (alias (file-name-concat parent "alias"))
         (root (file-name-concat real "work"))
         (aliased (file-name-concat alias "work"))
         (shared (file-name-concat root "shared"))
         (second (file-name-concat shared "second"))
         (state (file-name-concat shared "state/leaf"))
         preparation)
    (unwind-protect
        (progn
          (make-directory root t)
          (make-symbolic-link real alias)
          (setq preparation
                (niwa-sandbox--protected-restrictions
                 root (list aliased root real) nil
                 (list (list (list :path (file-name-concat aliased "shared/first") :mode 'read-only :directory-p t)
                             (list :path second :mode 'inaccessible :directory-p t)
                             (list :path state :mode 'read-only :directory-p t :create-parents t)))))
          ;; First candidate coarsens to shared, but the next sees that planned
          ;; parent through its other spelling and creates second, not shared.
          (should-not (file-exists-p (file-name-concat shared "first")))
          (should (file-directory-p second))
          (should (file-directory-p state))
          (should (equal (mapcar (lambda (entry) (plist-get entry :path))
                                (plist-get preparation :cleanup-paths))
                         (list second)))
          (niwa-sandbox-cleanup preparation)
          (should-not (file-exists-p second))
          (should (file-directory-p state)))
      (delete-directory parent t)))
  :doc "a mkdir conflict skips chmod and later operations, cleaning only the successful prefix"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-batch-conflict-" t)))
         (paths (mapcar (lambda (name) (file-name-concat root name)) '("first" "conflict" "skipped")))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         results cleaned)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations)
                       (if (eq (plist-get (car operations) :op) 'make-directory)
                           (progn
                             ;; Both appear after planning.  Neither belongs to
                             ;; this preparation; skipped mkdir is not ownership.
                             (dolist (path (cdr paths))
                               (make-directory path)
                               (set-file-modes path #o750))
                             (setq results (funcall run operations)))
                         (setq cleaned operations)
                         (funcall run operations)))))
            (should-error
             (niwa-sandbox--protected-restrictions
              root (list root) nil
              (list (mapcar (lambda (path) (list :path path :mode 'read-only :directory-p t)) paths)))
             :type 'niwa-sandbox-policy-error))
          (should (equal (mapcar (lambda (result) (plist-get result :status)) results)
                         '(ok ok conflict skipped skipped skipped)))
          (should (equal (mapcar (lambda (operation) (plist-get operation :path)) cleaned) (list (car paths))))
          (should-not (file-exists-p (car paths)))
          (dolist (path (cdr paths))
            (should (= #o750 (file-modes path)))))
      (delete-directory root t)))
  :doc "a successful synthetic mkdir is cleaned even when its own mode operation fails"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-batch-mode-" t)))
         (path (file-name-concat root "synthetic"))
         (run (symbol-function 'niwa-session-control-fs-run-program)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations)
                       (funcall run
                                (mapcar (lambda (operation)
                                          (if (eq (plist-get operation :op) 'directory-mode)
                                              (plist-put (copy-sequence operation) :content "invalid-mode")
                                            operation)) operations)))))
            (should-error
             (niwa-sandbox--protected-restrictions
              root (list root) nil
              (list (list (list :path path :mode 'read-only :directory-p t))))
             :type 'niwa-sandbox-policy-error))
          (should-not (file-exists-p path)))
      (delete-directory root t)))
  :doc "cleanup batches continue after a nonempty target and do not adopt a replaced inode"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-cleanup-batch-" t)))
         (empty (file-name-concat root "empty"))
         (nonempty (file-name-concat root "nonempty"))
         (replaced (file-name-concat root "replaced"))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         targets programs)
    (unwind-protect
        (progn
          (dolist (path (list empty replaced nonempty))
            (make-directory path)
            (push (list :path path :inode (file-attribute-inode-number (file-attributes path))) targets))
          (rename-file replaced (file-name-concat root "old"))
          (make-directory replaced)
          (set-file-modes nonempty #o750)
          (write-region "Keep" nil (file-name-concat nonempty "user-file") nil 'silent)
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations)
                       (push operations programs)
                       (funcall run operations))))
            (niwa-sandbox-cleanup (list :cleanup-paths (nreverse targets))))
          (should (= 1 (length programs)))
          (should (equal (mapcar (lambda (op) (plist-get op :path)) (car programs)) (list nonempty empty)))
          (should (cl-every (lambda (op) (plist-get op :optional)) (car programs)))
          (should-not (file-exists-p empty))
          (should (file-directory-p replaced))
          (should (= #o750 (file-modes nonempty)))
          (should (file-exists-p (file-name-concat nonempty "user-file"))))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--protected-restrictions ()
  ,test
  (test)
  :doc "mount layering:
`niwa-sandbox--protected-restrictions' masks after writable roots"
  (let* ((root (make-temp-file "niwa-sandbox-restrictions-" t))
         (dot-git (file-name-concat root ".git"))
         (credentials (file-name-concat root "credentials"))
         (missing (file-name-concat root "missing"))
         (niwa-protected-paths
          `((,(concat dot-git "/**") . read-only)
            (,(concat credentials "/**") . inaccessible)
            (,(concat missing "/**") . inaccessible)))
         restrictions)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-directory dot-git)
          (make-directory credentials)
          (setq restrictions
                (niwa-sandbox--protected-restrictions root (list root)))
          (let ((modes (mapcar (lambda (restriction)
                                 (cons (plist-get restriction :path)
                                       (plist-get restriction :mode)))
                               (plist-get restrictions :restrictions))))
            (should (eq 'read-only (alist-get dot-git modes nil nil #'equal)))
            (should (eq 'inaccessible (alist-get credentials modes nil nil #'equal)))
            (should (eq 'inaccessible (alist-get missing modes nil nil #'equal))))
          (should (file-directory-p missing))
          (niwa-sandbox-cleanup restrictions)
          (should-not (file-exists-p missing)))
      (niwa-sandbox-cleanup restrictions)
      (delete-directory root t)))
  :doc "Git pointer target:
`niwa-sandbox--protected-restrictions' protects worktree metadata targets"
  (let* ((root (make-temp-file "niwa-sandbox-pointer-root-" t))
         (metadata (make-temp-file "niwa-sandbox-pointer-meta-" t))
         (checkout (file-name-concat root "nested" "checkout"))
         (pointer (file-name-concat checkout ".git"))
         (niwa-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory checkout t)
          (with-temp-file pointer
            (insert (format "gitdir: %s\n" metadata)))
          (let ((paths (mapcar (lambda (restriction) (plist-get restriction :path))
                               (plist-get (niwa-sandbox--protected-restrictions
                                           root (list root))
                                          :restrictions))))
            (should (member pointer paths))
            (should (member metadata paths))))
      (delete-directory root t)
      (delete-directory metadata t)))
  :doc "nested Git symlink:
`niwa-sandbox--protected-restrictions' rejects a mutable .git symlink"
  (let* ((root (make-temp-file "niwa-sandbox-git-link-root-" t))
         (metadata (make-temp-file "niwa-sandbox-git-link-meta-" t))
         (checkout (file-name-concat root "nested" "checkout"))
         (link (file-name-concat checkout ".git"))
         (niwa-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory checkout t)
          (make-symbolic-link metadata link)
          (should-error
           (niwa-sandbox--protected-restrictions root (list root))
           :type 'niwa-sandbox-policy-error))
      (delete-directory root t)
      (delete-directory metadata t)))
  :doc "writable symlink ambiguity:
`niwa-sandbox--protected-restrictions' refuses a mutable symlink crossing"
  (let* ((root (make-temp-file "niwa-sandbox-restriction-link-" t))
         (outside (make-temp-file "niwa-sandbox-restriction-out-" t))
         (link (file-name-concat root "link"))
         (niwa-protected-paths
          `((,(concat link "/secret/**") . inaccessible))))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-symbolic-link outside link)
          (should-error
           (niwa-sandbox--protected-restrictions root (list root))
           :type 'niwa-sandbox-policy-error))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "root sibling containment:
`niwa-sandbox--protected-restrictions' does not mutate a lexical prefix"
  (let* ((parent (make-temp-file "niwa-sandbox-prefix-" t))
         (root (file-name-concat parent "work"))
         (sibling (file-name-concat parent "work-other"))
         (protected (file-name-concat sibling "missing"))
         (niwa-protected-paths
          `((,(concat protected "/**") . inaccessible))))
    (unwind-protect
        (progn
          (make-directory root)
          (make-directory sibling)
          (let ((restrictions
                 (niwa-sandbox--protected-restrictions root (list root))))
            (should-not (file-exists-p protected))
            (should-not (cl-find protected (plist-get restrictions :restrictions)
                                 :key (lambda (restriction) (plist-get restriction :path))
                                 :test #'equal))))
      (delete-directory parent t)))
  :doc "final validation rejects a symlink installed after the candidate proof"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-recheck-" t)))
         (protected (file-name-concat root "secret"))
         (moved (file-name-concat root "moved"))
         (niwa-protected-paths `((,(concat protected "/**") . inaccessible)))
         (pointer-targets (symbol-function 'niwa-sandbox--git-pointer-targets)))
    (unwind-protect
        (progn
          (make-directory protected)
          ;; Model a concurrent filesystem change at an intervening I/O seam.
          ;; Real paths exercise both canonicalization and the final proof.
          (cl-letf (((symbol-function 'niwa-sandbox--git-pointer-targets)
                     (lambda (path)
                       (prog1 (funcall pointer-targets path)
                         (when (equal path protected)
                           (rename-file protected moved)
                           (make-symbolic-link moved protected))))))
            (should-error
             (niwa-sandbox--protected-restrictions root (list root))
             :type 'niwa-sandbox-policy-error)))
      (delete-directory root t)))
  :doc "state creation does not reuse an ancestor proof from an earlier candidate"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-create-check-" t)))
         (outside (make-temp-file "niwa-sandbox-create-outside-" t))
         (branch (file-name-concat root "branch"))
         (seen (file-name-concat branch "seen"))
         (state (file-name-concat branch ".state"))
         (candidates (list (list :path seen :mode 'read-only :directory-p t)
                           (list :path state :mode 'read-only :directory-p t
                                 :create-parents t)))
         (pointer-targets (symbol-function 'niwa-sandbox--git-pointer-targets)))
    (unwind-protect
        (progn
          (make-directory seen t)
          (cl-letf (((symbol-function 'niwa-sandbox--git-pointer-targets)
                     (lambda (path)
                       (prog1 (funcall pointer-targets path)
                         (when (equal path seen)
                           (rename-file branch (file-name-concat root "moved"))
                           (make-symbolic-link outside branch))))))
            (should-error
             (niwa-sandbox--protected-restrictions root (list root) nil
                                                      (list candidates))
             :type 'niwa-sandbox-policy-error))
          (should-not (file-exists-p (file-name-concat outside ".state"))))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "state creation cannot follow an ancestor swapped during parent discovery"
  (dolist (swap-at '(1 2))
    (let* ((root (file-truename (make-temp-file "niwa-sandbox-mkdir-race-" t)))
           (outside (make-temp-file "niwa-sandbox-mkdir-outside-" t))
           (branch (file-name-concat root "branch"))
           (state (file-name-concat branch "nested/.state"))
           (candidate (list :path state :mode 'read-only :directory-p t
                            :create-parents t))
           (first-missing (symbol-function 'niwa-sandbox--first-missing-path))
           (calls 0))
      (unwind-protect
          (progn
            (make-directory branch)
            (when (= swap-at 2)
              (make-directory (file-name-concat outside "nested")))
            (cl-letf (((symbol-function 'niwa-sandbox--first-missing-path)
                       (lambda (path &optional planned)
                         (prog1 (funcall first-missing path planned)
                           (when (= (cl-incf calls) swap-at)
                             (rename-file branch (file-name-concat root "moved"))
                             (make-symbolic-link outside branch))))))
              (should-error
               (niwa-sandbox--protected-restrictions
                root (list root) nil (list (list candidate)))
               :type 'niwa-sandbox-policy-error))
            (should-not (file-exists-p (file-name-concat outside "nested/.state")))
            (when (= swap-at 1)
              (should-not (file-exists-p (file-name-concat outside "nested")))))
        (delete-directory root t)
        (delete-directory outside t))))
  :doc "synthetic cleanup cannot adopt an outside directory after creation"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-cleanup-race-" t)))
         (outside (make-temp-file "niwa-sandbox-cleanup-outside-" t))
         (branch (file-name-concat root "branch"))
         (path (file-name-concat branch "secret"))
         (foreign (file-name-concat outside "secret"))
         (candidate (list :path path :mode 'inaccessible :directory-p t))
         (run (symbol-function 'niwa-session-control-fs-run-program))
         diagnostics)
    (unwind-protect
        (progn
          (make-directory branch)
          (make-directory foreign)
          (set-file-modes foreign #o750)
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations)
                       (prog1 (funcall run operations)
                         (when (eq (plist-get (car operations) :op) 'make-directory)
                           (rename-file branch (file-name-concat root "moved"))
                           (make-symbolic-link outside branch))))))
            (niwa-test--with-captured-diagnostics diagnostics
              (should-error
               (niwa-sandbox--protected-restrictions
                root (list root) nil (list (list candidate)))
               :type 'niwa-sandbox-policy-error)))
          (should (string-match-p "Could not remove sandbox mount target" diagnostics))
          (should (file-directory-p foreign))
          (should (= #o750 (file-modes foreign))))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "missing state roots mount only the protected leaf and retain permanent parents"
  (let* ((root (make-temp-file "niwa-sandbox-state-roots-" t))
         (state (file-name-concat root ".niwa/artifacts/.state"))
         (niwa-protected-paths '(("**/.niwa/artifacts/.state/**" . read-only)))
         preparation)
    (unwind-protect
        (progn
          (setq preparation (niwa-sandbox--protected-restrictions root (list root)))
          (should (file-directory-p state))
          (should (equal (mapcar (lambda (entry) (plist-get entry :path))
                                (plist-get preparation :restrictions))
                         (list state)))
          (should-not (plist-get preparation :cleanup-paths))
          (niwa-sandbox-cleanup preparation)
          (should (file-directory-p state)))
      (niwa-sandbox-cleanup preparation)
      (delete-directory root t)))
  :doc "failed state-root creation retains permanent ancestors for the next launch"
  (let* ((root (make-temp-file "niwa-sandbox-state-failure-" t))
         (state (file-name-concat root ".niwa/artifacts/.state"))
         (niwa-protected-paths '(("**/.niwa/artifacts/.state/**" . read-only)))
         (run (symbol-function 'niwa-session-control-fs-run-program)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (operations)
                       ;; A real target-side failure after successful parents,
                       ;; leaving subsequent operations skipped as in production.
                       (funcall run
                                (mapcar (lambda (operation)
                                          (if (and (eq (plist-get operation :op) 'make-directory)
                                                   (equal (plist-get operation :path) state))
                                              (list :op 'verify :path state :content "missing")
                                            operation))
                                        operations)))))
            (should-error (niwa-sandbox--protected-restrictions root (list root))
                          :type 'niwa-sandbox-policy-error))
          (should (file-directory-p (file-name-concat root ".niwa/artifacts")))
          (let ((preparation (niwa-sandbox--protected-restrictions root (list root))))
            (should (file-directory-p state))
            (should-not (plist-get preparation :cleanup-paths))))
      (delete-directory root t)))
  :doc "fresh validation passes share prefix proofs without spawning creation programs"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-proof-count-" t)))
         (paths (mapcar (lambda (index)
                          (file-name-concat root "runs" (number-to-string index) ".git"))
                        (number-sequence 1 32)))
         (candidates (mapcar (lambda (path)
                               (list :path path :mode 'read-only :directory-p t))
                             paths))
         (check (symbol-function 'niwa-sandbox--writable-symlink-component))
         (symlink-p (symbol-function 'file-symlink-p))
         (checking nil)
         (probes (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (dolist (path paths) (make-directory path t))
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                     (lambda (&rest _) (ert-fail "Existing targets must not launch a creation program")))
                    ((symbol-function 'niwa-sandbox--writable-symlink-component)
                     (lambda (&rest args)
                       (unwind-protect
                           (progn (setq checking t) (apply check args))
                         (setq checking nil))))
                    ((symbol-function 'file-symlink-p)
                     (lambda (path)
                       (when checking
                         (puthash path (1+ (gethash path probes 0)) probes))
                       (funcall symlink-p path))))
            (niwa-sandbox--protected-restrictions root (list root) nil
                                                    (list candidates)))
          ;; Two common prefixes and two unique prefixes per candidate,
          ;; each tested once in each of the two independent proof passes.
          (should (= 66 (hash-table-count probes)))
          (maphash (lambda (_path count) (should (= count 2))) probes))
      (delete-directory root t)))
  :doc "paths under a canonical root keep their spelling without resolving it again"
  (let* ((root (file-truename (make-temp-file "niwa-sandbox-canonical-" t)))
         (dot-gits (mapcar (lambda (name) (file-name-concat root "runs" name ".git"))
                           '("one" "two")))
         (niwa-protected-paths '(("**/.git/**" . read-only)))
         (resolved nil)
         (truename (symbol-function 'file-truename)))
    (unwind-protect
        (progn
          (dolist (dot-git dot-gits) (make-directory dot-git t))
          (cl-letf (((symbol-function 'file-truename)
                     (lambda (file &rest args)
                       (when (member (directory-file-name file) dot-gits) (push file resolved))
                       (apply truename file args))))
            (let ((paths (mapcar (lambda (restriction) (plist-get restriction :path))
                                 (plist-get (niwa-sandbox--protected-restrictions root (list root))
                                            :restrictions))))
              (dolist (dot-git dot-gits) (should (member dot-git paths)))))
          (should-not resolved))
      (delete-directory root t)))
  :doc "a root reached through a symlinked ancestor also protects the canonical spelling"
  (let* ((parent (file-truename (make-temp-file "niwa-sandbox-alias-" t)))
         (real (file-name-concat parent "real"))
         (alias (file-name-concat parent "alias"))
         (root (file-name-concat alias "work"))
         (niwa-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory (file-name-concat real "work" ".git") t)
          (make-symbolic-link real alias)
          (let ((paths (mapcar (lambda (restriction) (plist-get restriction :path))
                               (plist-get (niwa-sandbox--protected-restrictions root (list root))
                                          :restrictions))))
            (should (member (file-name-concat root ".git") paths))
            (should (member (file-name-concat real "work" ".git") paths))))
      (delete-directory parent t))))

(niwa-deftest niwa-sandbox--unrestricted-facts ()
  ,test
  (test)
  :doc "unrestricted facts:
`niwa-sandbox--unrestricted-facts' discloses both weakened boundaries"
  (should
   (equal (niwa-sandbox--unrestricted-facts 'off "disabled")
          '(:sandbox off :filesystem unrestricted :network unrestricted
            :reason "disabled"))))

(niwa-deftest niwa-sandbox--direct-preparation ()
  ,test
  (test)
  :doc "direct preparation:
`niwa-sandbox--direct-preparation' returns unrestricted facts"
  (let ((prepared
         (niwa-sandbox--direct-preparation
          '("true") 'unavailable "probe failed")))
    (should (eq (plist-get prepared :state) 'unrestricted))
    (should (equal (plist-get prepared :command) '("true")))
    (should (eq (plist-get (plist-get prepared :facts) :sandbox)
                'unavailable))))

(niwa-deftest niwa-sandbox-pending-facts ()
  ,test
  (test)
  :doc "reports the selected confined boundary without mutating launch state"
  (let ((niwa-sandbox-mode 'best-effort))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda () '(:available t :executable "/test/bwrap"
                            :mount-proc t
                            :landlock (:python "/test/python3" :abi 9)))))
      (should
       (equal
        '(:sandbox bubblewrap :filesystem workspace-write :proc fresh
          :network isolated :host-sockets allow-listed)
        (niwa-sandbox-pending-facts)))))
  :doc "previews reachable host sockets with the Landlock fallback's reason"
  (let ((niwa-sandbox-mode 'best-effort))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda () '(:available t :executable "/test/bwrap"
                            :mount-proc t
                            :landlock (:reason "Landlock is not enabled")))))
      (let ((facts (niwa-sandbox-pending-facts)))
        (should (eq 'reachable (plist-get facts :host-sockets)))
        (should (equal "Landlock is not enabled"
                       (plist-get facts :host-sockets-reason))))))
  :doc "reflects requested additive network authority"
  (let ((niwa-sandbox-mode 'required))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda () '(:available t :executable "/test/bwrap"
                            :mount-proc t))))
      (should
       (eq 'unrestricted
           (plist-get
            (niwa-sandbox-pending-facts '(:network t))
            :network)))))
  :doc "reports automatic unrestricted fallback when the backend is unavailable"
  (let ((niwa-sandbox-mode 'best-effort))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda () '(:available nil :reason "probe failed"))))
      (should
       (equal
        '(:sandbox unavailable :filesystem unrestricted
          :network unrestricted :reason "probe failed")
        (niwa-sandbox-pending-facts)))))
  :doc "keeps a retryable runtime failure visible without consuming its retry"
  (let ((niwa-sandbox-mode 'best-effort)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil :reason "runtime failure"
                  :retry-on-execution t)))))
    (should (string-match-p
             "runtime failure"
             (plist-get (niwa-sandbox-pending-facts) :reason)))
    (should (plist-get (alist-get nil niwa-sandbox--probe-cache)
                       :retry-on-execution)))
  :doc "reports required-mode refusal instead of an unrestricted execution"
  (let ((niwa-sandbox-mode 'required))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda () '(:available nil :reason "probe failed"))))
      (should
       (equal
        '(:sandbox refused :filesystem unavailable
          :network unavailable :reason "probe failed")
        (niwa-sandbox-pending-facts)))))
  :doc "reports explicit full escalation as unrestricted"
  (let ((niwa-sandbox-mode 'required))
    (should
     (equal
      '(:sandbox escalated :filesystem unrestricted :network unrestricted
        :reason "Full execution escalation requested")
      (niwa-sandbox-pending-facts nil 'require-escalated))))
  :doc "off remains the selected unrestricted boundary despite escalation inputs"
  (should
   (equal
    '(:sandbox off :filesystem unrestricted :network unrestricted
      :reason "Confinement disabled by niwa-sandbox-mode")
    (niwa-sandbox-pending-facts
     '(:network t) 'require-escalated 'off)))
  :doc "probes the pending command's remote execution target"
  (let (probed-workdir)
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional workdir)
                 (setq probed-workdir workdir)
                 '(:available t :executable "/target/bwrap"
                   :mount-proc t))))
      (niwa-sandbox-pending-facts
       nil nil 'best-effort "/ssh:builder@host:/srv/project/")
      (should
       (equal "/ssh:builder@host:/srv/project/" probed-workdir)))))

(defun test-niwa-sandbox--run-confined (command)
  "Run Bubblewrap COMMAND; return (STATUS . OUTPUT)."
  (with-temp-buffer
    (cons (apply #'call-process (car command) nil t nil (cdr command))
          (buffer-string))))

(defun test-niwa-sandbox--without-tmpfs (command targets)
  "Return COMMAND without its `--tmpfs' mounts of TARGETS."
  (let (result)
    (while command
      (if (and (equal (car command) "--tmpfs") (member (cadr command) targets))
          (setq command (cddr command))
        (push (pop command) result)))
    (nreverse result)))

(niwa-deftest niwa-sandbox--runtime-arguments ()
  ,test
  (test)
  :doc "confined children reach no host bus, agent or keyring socket; unmasked they do"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe))
         (runtime (format "/run/user/%d/" (user-uid)))
         (bus (concat runtime "bus")))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless (file-exists-p bus)
      (ert-skip "No host session bus to protect"))
    (let ((root (file-truename (make-temp-file "niwa-sandbox-runtime-" t)))
          ;; /run/user is masked whatever XDG_RUNTIME_DIR says; no Wayland
          ;; bind-back, so no host socket may remain.
          (process-environment (cons "WAYLAND_DISPLAY" process-environment))
          (agents (cl-remove-if-not
                   #'file-exists-p
                   (mapcar (lambda (name) (concat runtime name))
                           '("bus" "gnupg/S.gpg-agent" "gnupg/S.gpg-agent.ssh"
                             "keyring/control" "systemd/private"))))
          (systemd-run (executable-find "systemd-run"))
          prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (niwa-sandbox--confined-preparation
                   (list "sh" "-c"
                         (concat "find /run/user -type s 2>/dev/null; "
                                 "if DBUS_SESSION_BUS_ADDRESS=unix:path=$1 timeout 10 "
                                 "systemd-run --user --quiet --wait /bin/true >/dev/null 2>&1; "
                                 "then echo systemd-run-reached; fi")
                         "probe" bus)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil))
            (let* ((command (plist-get prepared :command))
                   (masks (mapcar #'directory-file-name
                                  (niwa-sandbox--runtime-masks root)))
                   (masked (test-niwa-sandbox--run-confined command))
                   (unmasked (test-niwa-sandbox--run-confined
                              (test-niwa-sandbox--without-tmpfs command masks))))
              (should (member "/run/user" masks))
              (should (zerop (car masked)))
              (should-not (string-match-p "^/run/user/" (cdr masked)))
              (should-not (string-match-p "systemd-run-reached" (cdr masked)))
              ;; Negative control: the same command without the mask.
              (should (zerop (car unmasked)))
              (dolist (agent agents)
                (should (string-match-p (concat "^" (regexp-quote agent) "$")
                                        (cdr unmasked))))
              (when systemd-run
                (should (string-match-p "systemd-run-reached" (cdr unmasked))))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-directory root t))))
  :doc "binds back the Wayland socket and roots inside a masked runtime directory"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((runtime (file-truename (make-temp-file "niwa-sandbox-xdg-" t)))
           (root (file-name-concat runtime "work"))
           (process-environment
            (append (list (concat "XDG_RUNTIME_DIR=" runtime)
                          "WAYLAND_DISPLAY=wayland-test")
                    process-environment))
           servers prepared)
      (unwind-protect
          (progn
            (make-directory root)
            (dolist (name '("wayland-test" "bus"))
              (push (make-network-process
                     :name name :family 'local :server t :noquery t
                     :service (file-name-concat runtime name))
                    servers))
            (setq prepared
                  (niwa-sandbox--confined-preparation
                   (list "sh" "-c"
                         (concat "test -S \"$XDG_RUNTIME_DIR/wayland-test\" && "
                                 "test ! -e \"$XDG_RUNTIME_DIR/bus\" && "
                                 "touch \"$1/written\"")
                         "probe" root)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil))
            (should (member runtime (plist-get prepared :command)))
            (should (zerop (car (test-niwa-sandbox--run-confined
                                 (plist-get prepared :command)))))
            (should (file-exists-p (file-name-concat root "written"))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (mapc #'delete-process servers)
        (delete-directory runtime t))))
  :doc "a remote target masks its own /run/user, not local runtime state"
  (skip-unless (file-directory-p "/run/user"))
  (let ((runtime (file-truename (make-temp-file "niwa-sandbox-xdg-remote-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("sandbox-runtime")
          (let ((process-environment
                 (append (list (concat "XDG_RUNTIME_DIR=" runtime)
                               (concat "WAYLAND_DISPLAY=" runtime))
                         process-environment)))
            (let ((remote (concat "/niwamock:sandbox-runtime:" runtime "/")))
              (should (equal '("/run/user/") (niwa-sandbox--runtime-masks remote)))
              (should (equal '("--tmpfs" "/run/user")
                             (niwa-sandbox--runtime-arguments
                              remote nil '("/run/user/")))))))
      (delete-directory runtime t)))
  :doc "a confined remote command reaches no runtime socket on its target"
  (let ((root (file-truename (make-temp-file "niwa-sandbox-runtime-remote-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("sandbox-runtime-run")
          (let* ((niwa-sandbox--probe-cache nil)
                 (remote (concat "/niwamock:sandbox-runtime-run:" root "/"))
                 (default-directory remote)
                 (availability (niwa-sandbox-probe remote))
                 (bus (format "/run/user/%d/bus" (user-uid)))
                 prepared)
            (unless (plist-get availability :available)
              (ert-skip (plist-get availability :reason)))
            (unless (file-exists-p bus)
              (ert-skip "No session bus on the mock target"))
            (unwind-protect
                (progn
                  (setq prepared
                        (niwa-sandbox--confined-preparation
                         (list "sh" "-c" "test ! -e \"$1\" && touch written" "probe" bus)
                         remote (list remote) (plist-get availability :executable)
                         (plist-get availability :mount-proc) nil))
                  ;; Execution localizes target paths before launch.
                  (let ((command (mapcar (lambda (argument)
                                           (if (file-remote-p argument)
                                               (file-local-name argument)
                                             argument))
                                         (plist-get prepared :command))))
                    (should (member "/run/user" command))
                    (with-temp-buffer
                      (should (equal '(0 . "")
                                     (cons (apply #'process-file (car command) nil t nil
                                                  (cdr command))
                                           (replace-regexp-in-string
                                            "\\`NIWA_SANDBOX_STARTED_.*\n" ""
                                            (buffer-string)))))))
                  (should (file-exists-p (file-name-concat root "written"))))
              (when prepared (niwa-sandbox-cleanup prepared)))))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox--check-runtime-grants ()
  ,test
  (test)
  :doc "runtime-directory grants are refused; admitted, one would reopen the bus"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe))
         (runtime (format "/run/user/%d" (user-uid)))
         (bus (concat runtime "/bus")))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless (file-exists-p bus)
      (ert-skip "No host session bus to protect"))
    (let ((root (file-truename (make-temp-file "niwa-sandbox-runtime-grant-" t)))
          (process-environment (cons "WAYLAND_DISPLAY" process-environment)))
      (unwind-protect
          (cl-flet ((prepare (path)
                      (niwa-sandbox--confined-preparation
                       (list "sh" "-c" "test -S \"$1\"" "probe" bus)
                       root (list root) (plist-get availability :executable)
                       (plist-get availability :mount-proc)
                       (list :file-system
                             (list (list :path path :access 'read :recursive t))))))
            ;; At, below and above the masked directory.
            (dolist (path (list runtime (concat runtime "/gnupg") "/run"))
              (when (file-directory-p path)
                (should-error (prepare path) :type 'niwa-sandbox-policy-error)))
            ;; The refusal names the escalation a keyring-backed tool needs.
            (should (string-search
                     "require_escalated"
                     (cadr (should-error (prepare runtime)
                                         :type 'niwa-sandbox-policy-error))))
            ;; Negative control: the admitted grant brings the bus back.
            (cl-letf (((symbol-function 'niwa-sandbox--check-runtime-grants)
                       #'ignore))
              (let ((prepared (prepare runtime)))
                (unwind-protect
                    (should (zerop (car (test-niwa-sandbox--run-confined
                                         (plist-get prepared :command)))))
                  (niwa-sandbox-cleanup prepared)))))
        (delete-directory root t)))))

(niwa-deftest niwa-sandbox--check-runtime-grants-spelling ()
  ,test
  (test)
  :doc "doubled-slash and symlinked runtime grants are refused for read and write"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe))
         (runtime (format "/run/user/%d" (user-uid)))
         (bus (concat runtime "/bus")))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless (file-exists-p bus)
      (ert-skip "No host session bus to protect"))
    (let ((root (file-truename (make-temp-file "niwa-sandbox-runtime-spelling-" t)))
          (process-environment (cons "WAYLAND_DISPLAY" process-environment)))
      (unwind-protect
          (cl-flet ((prepare (path access)
                      (niwa-sandbox--confined-preparation
                       (list "sh" "-c" "test -S \"$1\"" "probe" bus)
                       root (list root) (plist-get availability :executable)
                       (plist-get availability :mount-proc)
                       (list :file-system
                             (list (list :path path :access access :recursive t))))))
            ;; Emacs keeps a leading `//'; the kernel reads it as `/'.
            (make-symbolic-link (concat "/" runtime) (file-name-concat root "rt"))
            (dolist (access '(read write))
              (dolist (path (list (concat "/" runtime)
                                  (concat "/" runtime "/gnupg/")
                                  (concat "/:" runtime)
                                  (file-name-concat root "rt")))
                (when (file-directory-p path)
                  (should-error (prepare path access)
                                :type 'niwa-sandbox-policy-error))))
            ;; An exact quoted read used to be dropped silently as needing no mount.
            (should-error (niwa-sandbox--resolve-filesystem-permissions
                           (list (list :path (concat "/:" runtime) :access 'read)))
                          :type 'niwa-sandbox-policy-error)
            ;; Negative control: admitted, the doubled-slash grant reopens the bus.
            (cl-letf (((symbol-function 'niwa-sandbox--check-runtime-grants)
                       #'ignore))
              (dolist (access '(read write))
                (let ((prepared (prepare (concat "/" runtime) access)))
                  (unwind-protect
                      (should (zerop (car (test-niwa-sandbox--run-confined
                                           (plist-get prepared :command)))))
                    (niwa-sandbox-cleanup prepared))))))
        (delete-directory root t)))))

(niwa-deftest niwa-sandbox--socket-arguments ()
  ,test
  (test)
  :doc "a host socket in a masked shared directory is unreachable; unmasked it is reachable"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe))
         (python (executable-find "python3")))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless python (ert-skip "python3 is unavailable"))
    (let* ((shared (file-truename (make-temp-file "niwa-sandbox-host-sockets-" t)))
           (socket (file-name-concat shared "X0"))
           (root (file-truename (make-temp-file "niwa-sandbox-socket-root-" t)))
           (server (make-network-process :name "host-control" :family 'local
                                         :server t :noquery t :service socket))
           (niwa-sandbox--socket-directories (list shared))
           prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (niwa-sandbox--confined-preparation
                   (list python "-c"
                         (concat "import socket, sys; "
                                 "socket.socket(socket.AF_UNIX).connect(sys.argv[1])")
                         socket)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil))
            (let ((command (plist-get prepared :command)))
              (should-not (zerop (car (test-niwa-sandbox--run-confined command))))
              ;; Negative control: the same command without the directory mask.
              (should (zerop (car (test-niwa-sandbox--run-confined
                                   (test-niwa-sandbox--without-tmpfs
                                    command (list shared))))))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-process server)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "shared X11 and ICE socket directories are empty inside confinement"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe))
         (directories (cl-remove-if-not #'file-directory-p
                                        niwa-sandbox--socket-directories)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (unless directories
      (ert-skip "No shared socket directory on this host"))
    (let ((root (file-truename (make-temp-file "niwa-sandbox-x11-" t)))
          prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (niwa-sandbox--confined-preparation
                   (append (list "sh" "-c" "for d; do ls -A \"$d\"; done" "probe")
                           directories)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil))
            (let ((result (test-niwa-sandbox--run-confined
                           (plist-get prepared :command))))
              (should (zerop (car result)))
              (should (string-match-p "\\`NIWA_SANDBOX_STARTED_[^\n]*\n\\'"
                                      (cdr result)))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-directory root t)))))

(niwa-deftest niwa-sandbox--confined-preparation ()
  ,test
  (test)
  :doc "confined preparation:
`niwa-sandbox--confined-preparation' canonicalizes authority and adds marker"
  (let ((root (make-temp-file "niwa-sandbox-confined-" t)))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (let ((prepared
               (niwa-sandbox--confined-preparation
                '("true") root (list root) "/test/bwrap" t nil)))
          (should (eq (plist-get prepared :state) 'confined))
          (should (equal (car (plist-get prepared :command)) "/test/bwrap"))
          (should (string-prefix-p "NIWA_SANDBOX_STARTED_"
                                   (plist-get prepared :marker)))
          (should (plist-get prepared :facts)))
      (delete-directory root t)))
  :doc "confined preparation refusal removes its synthetic protected directories"
  (let* ((root (make-temp-file "niwa-sandbox-refusal-" t))
         (placeholder (file-name-concat root "protected-missing"))
         (niwa-protected-paths `((,(concat placeholder "/**") . inaccessible))))
    (unwind-protect
        (progn
          (should-error
           (niwa-sandbox--confined-preparation
            '("true") root (list root) "bwrap" t
            (list :file-system (list (list :path root :access 'write))))
           :type 'niwa-sandbox-policy-error)
          (should-not (file-exists-p placeholder)))
      (delete-directory root t)))
  :doc "recursive masked-parent grants remain effective while protecting nested children"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let* ((parent (make-temp-file "niwa-sandbox-masked-tree-" t))
           (root (file-name-concat parent "workspace"))
           (tree (file-name-concat parent "resource"))
           (child (file-name-concat tree "restricted"))
           (niwa-protected-paths
            `((,(concat tree "/**") . inaccessible)
              (,(concat child "/**") . read-only))))
      (unwind-protect
          (progn
            (make-directory root)
            (make-directory child t)
            (with-temp-file (file-name-concat tree "visible") (insert "visible contents\n"))
            (dolist (access '(read write))
              (let ((prepared
                     (niwa-sandbox--confined-preparation
                      (list "sh" "-c"
                            (concat "set -eu; cat \"$1/visible\"; "
                                    "if touch \"$1/restricted/forbidden\" 2>/dev/null; then exit 71; fi; "
                                    "if touch \"$1/allowed\" 2>/dev/null; then printf write; else printf read; fi")
                            "fixture" tree)
                      root (list root) (plist-get availability :executable)
                      (plist-get availability :mount-proc)
                      (list :file-system (list (list :path tree :access access :recursive t))))))
                (unwind-protect
                    (with-temp-buffer
                      (should (zerop (apply #'process-file
                                            (car (plist-get prepared :command)) nil t nil
                                            (cdr (plist-get prepared :command)))))
                      (should (string-match-p
                               (concat "visible contents\n" (symbol-name access)) (buffer-string))))
                  (niwa-sandbox-cleanup prepared)))))
        (delete-directory parent t))))
  :doc "confined children cannot reach a live MCP server socket"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let ((runtime (make-temp-file "niwa-sandbox-mcp-" t)))
      (unwind-protect
          (let* ((niwa-mcp--socket-root nil)
                 (process-environment (cons (concat "XDG_RUNTIME_DIR=" runtime)
                                            process-environment))
                 (server (niwa-mcp-start (lambda () []) #'ignore))
                 (socket (file-name-concat (niwa-mcp-directory server) "socket"))
                 prepared)
            (unwind-protect
                (progn
                  (setq prepared
                        (niwa-sandbox--confined-preparation
                         (list "sh" "-c" "test ! -e \"$1\"" "probe" socket)
                         runtime (list runtime) (plist-get availability :executable)
                         (plist-get availability :mount-proc) nil))
                  (should (file-exists-p socket))
                  (should (zerop (apply #'call-process
                                        (car (plist-get prepared :command)) nil nil nil
                                        (cdr (plist-get prepared :command))))))
              (when prepared (niwa-sandbox-cleanup prepared))
              (niwa-mcp-stop server)))
        (delete-directory runtime t))))
  :doc "an unusable socket root leaves ordinary confined commands working"
  (let* ((niwa-sandbox--probe-cache nil)
         (availability (niwa-sandbox-probe)))
    (unless (plist-get availability :available)
      (ert-skip (plist-get availability :reason)))
    (let ((runtime (make-temp-file "niwa-sandbox-mcp-" t)))
      (unwind-protect
          (let* ((niwa-mcp--socket-root nil)
                 (process-environment (cons (concat "XDG_RUNTIME_DIR=" runtime)
                                            process-environment))
                 (squatted (file-name-concat runtime (format "niwa-mcp-%d" (user-uid))))
                 prepared)
            ;; A squatted root: no MCP server can start, so nothing needs masking.
            (make-symbolic-link runtime squatted)
            (unwind-protect
                (progn
                  (setq prepared
                        (niwa-sandbox--confined-preparation
                         (list "true") runtime (list runtime)
                         (plist-get availability :executable)
                         (plist-get availability :mount-proc) nil))
                  (should-not (member squatted (plist-get prepared :command)))
                  (should (zerop (apply #'call-process
                                        (car (plist-get prepared :command)) nil nil nil
                                        (cdr (plist-get prepared :command))))))
              (when prepared (niwa-sandbox-cleanup prepared))))
        (delete-directory runtime t)))))

(niwa-deftest niwa-sandbox-prepare ()
  ,test
  (test)
  :doc "preparation I/O errors cannot select unrestricted execution"
  (let ((niwa-sandbox-mode 'best-effort))
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda (&rest _) '(:available t :executable "bwrap" :mount-proc t)))
              ((symbol-function 'niwa-sandbox--confined-preparation)
               (lambda (&rest _) (signal 'file-error '("Grant target changed")))))
      (let ((prepared (niwa-sandbox-prepare '("true") temporary-file-directory nil)))
        (should (eq 'refused (plist-get prepared :state)))
        (should-not (plist-get prepared :command))
        (should (string-match-p "Grant target changed" (plist-get prepared :error))))))
  :doc "full escalation:
`niwa-sandbox-prepare' bypasses the backend after explicit approval"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache 'unconsulted))
    (let ((prepared
           (niwa-sandbox-prepare
            '("sh" "-c" "printf ok") temporary-file-directory nil nil
            'require-escalated)))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (equal (plist-get prepared :command)
                     '("sh" "-c" "printf ok")))
      (should (eq (plist-get (plist-get prepared :facts) :sandbox)
                  'escalated))
      (should (eq (plist-get (plist-get prepared :facts) :filesystem)
                  'unrestricted))
      (should (eq (plist-get (plist-get prepared :facts) :network)
                  'unrestricted))
      (should (eq niwa-sandbox--probe-cache 'unconsulted))))
  :doc "available backend:
`niwa-sandbox-prepare' builds the confined read-only-host profile"
  (let* ((executable (or (executable-find "bwrap") "bwrap"))
         (root (make-temp-file "niwa-sandbox-root-" t))
         (workdir (file-name-as-directory root))
         (niwa-sandbox-mode 'best-effort)
         (niwa-sandbox--probe-cache
          (list (cons nil
                      (list :available t :executable executable
                            :mount-proc t))))
         prepared)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setq prepared
          (niwa-sandbox-prepare
           '("sh" "-c" "printf ok") workdir (list root)))
    (unwind-protect
        (let ((command (plist-get prepared :command)))
          (should (eq (plist-get prepared :state) 'confined))
          (should (equal (car command) executable))
          (should (member "--ro-bind" command))
          (should (member "--bind" command))
          (should (member "--unshare-user" command))
          (should (member "--unshare-pid" command))
          (should (member "--unshare-net" command))
          (should (member "--proc" command))
          (should (member "--chdir" command))
          (should (equal (plist-get (plist-get prepared :facts) :network)
                         'isolated)))
      (delete-directory root t)))
  :doc "host proc fallback:
`niwa-sandbox-prepare' keeps namespace confinement without a fresh proc mount"
  (let* ((root (make-temp-file "niwa-sandbox-host-proc-" t))
         (niwa-sandbox-mode 'required)
         (niwa-sandbox--probe-cache
          '((nil . (:available t :executable "/test/bwrap"
                    :mount-proc nil))))
         prepared command)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setq prepared (niwa-sandbox-prepare '("true") root (list root))
          command (plist-get prepared :command))
    (unwind-protect
        (progn
          (should (eq (plist-get prepared :state) 'confined))
          (should (member "--unshare-user" command))
          (should (member "--unshare-pid" command))
          (should (member "--unshare-net" command))
          (should-not (member "--proc" command))
          (should (eq (plist-get (plist-get prepared :facts) :proc) 'host))
          (should (string-match-p
                   "proc: host"
                   (niwa-sandbox-status-text
                    (plist-get prepared :facts)))))
      (niwa-sandbox-cleanup prepared)
      (delete-directory root t)))
  :doc "automatic fallback:
`niwa-sandbox-prepare' discloses unrestricted direct execution"
  (let ((niwa-sandbox-mode 'best-effort)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil :reason "test backend unavailable")))))
    (let ((prepared
           (niwa-sandbox-prepare '("true") temporary-file-directory nil)))
      (should (equal (plist-get prepared :command) '("true")))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (eq (plist-get (plist-get prepared :facts) :network)
                  'unrestricted))
      (should (string-match-p
               "test backend unavailable"
               (plist-get (plist-get prepared :facts) :reason)))))
  :doc "runtime failure retry:
`niwa-sandbox-prepare' reprobes after a transient launch failure"
  (let ((niwa-sandbox-mode 'best-effort)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil :reason "transient launch failure"
                  :retry-on-execution t))))
        cache-at-probe)
    (cl-letf (((symbol-function 'niwa-sandbox-probe)
               (lambda ()
                 (setq cache-at-probe niwa-sandbox--probe-cache)
                 '(:available nil :reason "fresh probe unavailable"))))
      (let ((prepared
             (niwa-sandbox-prepare
              '("true") temporary-file-directory nil)))
        (should-not cache-at-probe)
        (should (string-match-p
                 "fresh probe unavailable"
                 (plist-get (plist-get prepared :facts) :reason))))))
  :doc "additive network profile:
`niwa-sandbox-prepare' changes only network isolation"
  (let* ((root (make-temp-file "niwa-sandbox-network-" t))
         (niwa-sandbox-mode 'required)
         (niwa-sandbox--probe-cache
          '((nil . (:available t :executable "/test/bwrap"
                    :mount-proc t))))
         default network)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setq default (niwa-sandbox-prepare '("true") root (list root))
          network (niwa-sandbox-prepare
                   '("true") root (list root) '(:network t)))
    (unwind-protect
        (let ((default-command (plist-get default :command))
              (network-command (plist-get network :command)))
          (should (member "--unshare-net" default-command))
          (should-not (member "--unshare-net" network-command))
          (let ((default-without-network
                 (delete "--unshare-net" (copy-sequence default-command))))
            (setcar (member (plist-get default :marker)
                            default-without-network)
                    "SANDBOX_MARKER")
            (setcar (member (plist-get network :marker) network-command)
                    "SANDBOX_MARKER")
            (should (equal default-without-network network-command)))
          (should (eq 'isolated
                      (plist-get (plist-get default :facts) :network)))
          (should (eq 'unrestricted
                      (plist-get (plist-get network :facts) :network)))
          (should (equal (plist-get (plist-get default :facts)
                                    :writable-roots)
                         (plist-get (plist-get network :facts)
                                    :writable-roots)))
          (should (= (plist-get (plist-get default :facts)
                                :protected-paths)
                     (plist-get (plist-get network :facts)
                                :protected-paths))))
      (niwa-sandbox-cleanup default)
      (niwa-sandbox-cleanup network)
      (delete-directory root t)))
  :doc "additive filesystem profile:
exact read and write mounts follow protected masks without broadening siblings"
  (let* ((root (make-temp-file "niwa-sandbox-filesystem-" t))
         (dot-git (file-name-concat root ".git"))
         (config (file-name-concat dot-git "config"))
         (head (file-name-concat dot-git "HEAD"))
         (credentials (file-name-concat root "credentials"))
         (token (file-name-concat credentials "token"))
         (sibling (file-name-concat credentials "sibling"))
         (niwa-protected-paths
          `(("**/.git/**" . read-only)
            (,(concat credentials "/**") . inaccessible)))
         (niwa-sandbox-mode 'required)
         (niwa-sandbox--probe-cache
          '((nil . (:available t :executable "/test/bwrap"
                    :mount-proc t))))
         prepared)
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-directory dot-git)
          (make-directory credentials)
          (dolist (file (list config head token sibling))
            (with-temp-file file (insert "value")))
          (setq prepared
                (niwa-sandbox-prepare
                 '("true") root (list root)
                 `(:file-system ((:path ,token :access read)
                                 (:path ,config :access write)))))
          (let ((command (plist-get prepared :command)))
            (should (equal "-p" (nth 1 command)))
            (should
             (cl-loop for tail on command
                      thereis (equal (seq-take tail 3)
                                     (list "--ro-bind-fd" "10"
                                           (file-truename token)))))
            (should
             (cl-loop for tail on command
                      thereis (equal (seq-take tail 3)
                                     (list "--bind-fd" "11"
                                           (file-truename config)))))
            (should (member "/test/bwrap" command))
            (should-not (member head command))
            (should-not (member sibling command))
            (should (member "--unshare-net" command))
            (should (= 2 (plist-get (plist-get prepared :facts)
                                    :additional-filesystem)))
            (should (= 1 (plist-get (plist-get prepared :facts)
                                    :additional-filesystem-read)))
            (should (= 1 (plist-get (plist-get prepared :facts)
                                    :additional-filesystem-write)))))
      (niwa-sandbox-cleanup prepared)
      (delete-directory root t)))
  :doc "intrinsic device grants:
`niwa-sandbox-prepare' keeps its private /dev/null instead of remounting it"
  (let* ((root (make-temp-file "niwa-sandbox-dev-null-" t))
         (niwa-sandbox-mode 'required)
         (niwa-sandbox--probe-cache
          '((nil . (:available t :executable "/test/bwrap"
                    :mount-proc t))))
         prepared command facts)
    (skip-unless (not (eq system-type 'windows-nt)))
    (setq prepared
          (niwa-sandbox-prepare
           '("true") root (list root)
           '(:file-system ((:path "/dev/null" :access read))))
          command (plist-get prepared :command)
          facts (plist-get prepared :facts))
    (unwind-protect
        (progn
          (should (equal "/test/bwrap" (car command)))
          (should-not (member "--ro-bind-fd" command))
          (should (= 0 (plist-get facts :additional-filesystem))))
      (niwa-sandbox-cleanup prepared)
      (delete-directory root t)))
  :doc "real device grant:
an approved device can be opened without making ordinary mounts device-capable"
  (skip-unless (eq system-type 'gnu/linux))
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    (let ((root (make-temp-file "niwa-sandbox-device-" t))
          (output (generate-new-buffer " *niwa-device-grant-test*"))
          prepared)
      (unwind-protect
          (dolist (mixed '(nil t))
            (let ((niwa-protected-paths
                   (when mixed '(("/dev/**" . inaccessible))))
                  (grants (append '((:path "/dev/zero" :access write))
                                  (when mixed '((:path "/dev" :access read :recursive t))))))
              (with-temp-buffer
                (should (zerop (call-process "head" nil t nil "-c1" "/dev/full")))
                (should (= 1 (buffer-size))))
              (with-current-buffer output (erase-buffer))
              (setq prepared
                    (niwa-sandbox-prepare
                     (list "sh" "-c"
                           (concat "set -e; test ! -e /proc/self/fd/10; test ! -e /proc/self/fd/11; "
                                   "dd if=/dev/zero of=/dev/zero bs=1 count=1 status=none; "
                                   (when mixed
                                     "test -c /dev/full; if head -c1 /dev/full >denied-read 2>denied-error; then exit 91; fi")))
                     root (list root) (list :file-system grants)))
              (unwind-protect
                  (progn
                    (should (eq 'confined (plist-get prepared :state)))
                    (when mixed
                      (should (member "--remount-ro" (plist-get prepared :command))))
                    (let ((status (apply #'call-process
                                         (car (plist-get prepared :command)) nil output nil
                                         (cdr (plist-get prepared :command)))))
                      (ert-info ((format "Mixed grants: %S; output: %s" mixed
                                         (with-current-buffer output (buffer-string))))
                        (should (zerop status)))))
                (niwa-sandbox-cleanup prepared)
                (setq prepared nil))))
        (kill-buffer output)
        (niwa-sandbox-cleanup prepared)
        (delete-directory root t))))
  :doc "required backend:
`niwa-sandbox-prepare' refuses execution when confinement is unavailable"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache
         '((nil . (:available nil :reason
                  "required backend unavailable")))))
    (let ((prepared
           (niwa-sandbox-prepare '("true") temporary-file-directory nil)))
      (should-not (plist-get prepared :command))
      (should (eq (plist-get prepared :state) 'refused))
      (should (string-match-p "required backend unavailable"
                              (plist-get prepared :error)))))
  :doc "working-directory boundary:
`niwa-sandbox-prepare' never converts invalid authority into fallback"
  (let* ((root (make-temp-file "niwa-sandbox-authority-" t))
         (outside (make-temp-file "niwa-sandbox-outside-" t))
         (niwa-sandbox-mode 'best-effort)
         (niwa-sandbox--probe-cache
          (list
           (cons nil
                 (list :available t
                       :executable (or (executable-find "bwrap") "bwrap")
                       :mount-proc t)))))
    (unwind-protect
        (let ((prepared
               (niwa-sandbox-prepare '("true") outside (list root))))
          (should-not (plist-get prepared :command))
          (should (eq (plist-get prepared :state) 'refused))
          (should (string-match-p "outside writable roots"
                                  (plist-get prepared :error))))
      (delete-directory root t)
      (delete-directory outside t)))
  :doc "dangling symlink grant:
`niwa-sandbox-prepare' refuses an unresolved resource without fallback"
  (let* ((root (make-temp-file "niwa-sandbox-dangling-root-" t))
         (resource (make-temp-file "niwa-sandbox-dangling-resource-" t))
         (link (file-name-concat resource "runner"))
         (niwa-sandbox-mode 'best-effort)
         (niwa-sandbox--probe-cache
          (list
           (cons nil
                 (list :available t
                       :executable (or (executable-find "bwrap") "bwrap")
                       :mount-proc t)))))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-symbolic-link "missing" link)
          (let ((prepared
                 (niwa-sandbox-prepare
                  (list link) root (list root)
                  `(:file-system ((:path ,link :access read))))))
            (should (eq 'refused (plist-get prepared :state)))
            (should-not (plist-get prepared :command))
            (should (string-match-p "unavailable"
                                    (plist-get prepared :error)))))
      (delete-directory resource t)
      (delete-directory root t)))
  :doc "disabled backend:
`niwa-sandbox-prepare' executes directly with visible disclosure"
  (let ((niwa-sandbox-mode 'off)
        (niwa-sandbox--probe-cache 'unconsulted))
    (let ((prepared
           (niwa-sandbox-prepare '("true") temporary-file-directory nil)))
      (should (equal (plist-get prepared :command) '("true")))
      (should (eq (plist-get prepared :state) 'unrestricted))
      (should (eq niwa-sandbox--probe-cache 'unconsulted))
      (should (eq (plist-get (plist-get prepared :facts) :sandbox) 'off))))
  :doc "real backend:
`niwa-sandbox-prepare' permits one writable root and keeps a sibling read-only"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((parent (make-temp-file "niwa-sandbox-real-" t))
             (root (expand-file-name "allowed" parent))
             (outside (expand-file-name "outside" parent))
             (inside-file (expand-file-name "inside" root))
             (outside-file (expand-file-name "denied" outside))
             (descendant-command
              (format "printf inside > %s; printf outside > %s || true"
                      (shell-quote-argument inside-file)
                      (shell-quote-argument outside-file)))
             prepared output-buffer)
        (make-directory root)
        (make-directory outside)
        (unwind-protect
            (progn
              (setq prepared
                    (niwa-sandbox-prepare
                     (list "sh" "-c"
                           (format
                            (concat "sh -c %s; "
                                    "test \"$(wc -l < /proc/net/route)\" -eq 1")
                            (shell-quote-argument descendant-command)))
                     root (list root)))
              (setq output-buffer (generate-new-buffer " *niwa-bwrap-test*"))
              (should (zerop
                       (apply #'call-process
                              (car (plist-get prepared :command)) nil
                              output-buffer nil
                              (cdr (plist-get prepared :command)))))
              (should (file-exists-p inside-file))
              (should-not (file-exists-p outside-file))
              (with-current-buffer output-buffer
                (should (member (plist-get prepared :marker)
                                (split-string (buffer-string) "\n" nil)))))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (niwa-sandbox-cleanup prepared)
          (delete-directory parent t)))))
  :doc "real symlinked executable:
a granted symlink chain starts under the original confined command"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "niwa-sandbox-link-root-" t))
             (resource (make-temp-file "niwa-sandbox-link-resource-" t))
             (hidden (file-name-concat resource "hidden"))
             (bin (file-name-concat hidden "bin"))
             (target (file-name-concat bin "runner"))
             (alias (file-name-concat hidden "runner-alias"))
             (link (file-name-concat resource "runner"))
             (result (file-name-concat root "result"))
             (command (list link "started" result))
             (niwa-protected-paths
              `((,link . inaccessible)
                (,(concat hidden "/**") . inaccessible)))
             prepared output-buffer)
        (unwind-protect
            (progn
              (make-directory bin t)
              (with-temp-file target
                (insert "#!/bin/sh\nprintf '%s' \"$1\" > \"$2\"\n"))
              (set-file-modes target #o700)
              (make-symbolic-link "bin/runner" alias)
              (make-symbolic-link "hidden/runner-alias" link)
              (setq prepared
                    (niwa-sandbox-prepare
                     command root (list root)
                     `(:file-system ((:path ,link :access read)))))
              (should-not (plist-get prepared :error))
              (should (member link (plist-get prepared :command)))
              (should (member (file-truename target)
                              (plist-get prepared :command)))
              (setq output-buffer
                    (generate-new-buffer " *niwa-bwrap-link-test*"))
              (let ((status
                     (apply #'call-process
                            (car (plist-get prepared :command))
                            nil output-buffer nil
                            (cdr (plist-get prepared :command)))))
                (unless (zerop status)
                  (ert-fail
                   (format "Bubblewrap exited %s: %s"
                           status
                           (with-current-buffer output-buffer
                             (buffer-string))))))
              (should (equal "started"
                             (with-temp-buffer
                               (insert-file-contents result)
                               (buffer-string))))
              (let ((facts (plist-get prepared :facts)))
                (should (eq 'bubblewrap (plist-get facts :sandbox)))
                (should (eq 'workspace-write (plist-get facts :filesystem)))
                (should (memq (plist-get facts :proc) '(fresh host)))
                (should (eq 'isolated (plist-get facts :network))))
              (with-current-buffer output-buffer
                (should (member (plist-get prepared :marker)
                                (split-string (buffer-string) "\n" nil)))))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (niwa-sandbox-cleanup prepared)
          (delete-directory resource t)
          (delete-directory root t)))))
  :doc "real additive network:
default confinement cannot reach a host listener but network escalation can"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "niwa-sandbox-network-real-" t))
             (server (make-network-process
                      :name "niwa-sandbox-network-test"
                      :server t :host "127.0.0.1" :service t :noquery t))
             (port (process-contact server :service))
             (command
              (list "bash" "-c"
                    (format "exec 3<>/dev/tcp/127.0.0.1/%d" port)))
             default network)
        (unwind-protect
            (progn
              (setq default
                    (niwa-sandbox-prepare command root (list root)))
              (setq network
                    (niwa-sandbox-prepare
                     command root (list root) '(:network t)))
              (should-not
               (zerop
                (apply #'call-process
                       (car (plist-get default :command)) nil nil nil
                       (cdr (plist-get default :command)))))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get network :command)) nil nil nil
                       (cdr (plist-get network :command))))))
          (when (processp server) (delete-process server))
          (niwa-sandbox-cleanup default)
          (niwa-sandbox-cleanup network)
          (delete-directory root t)))))
  :doc "real additive filesystem:
the named protected files reopen while parent and sibling restrictions remain"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "niwa-sandbox-filesystem-real-" t))
             (dot-git (file-name-concat root ".git"))
             (config (file-name-concat dot-git "config"))
             (head (file-name-concat dot-git "HEAD"))
             (credentials (file-name-concat root "credentials"))
             (token (file-name-concat credentials "token"))
             (sibling (file-name-concat credentials "sibling"))
             (token-copy (file-name-concat root "token-copy"))
             (sibling-copy (file-name-concat root "sibling-copy"))
             (parent-list (file-name-concat root "parent-list"))
             (bash-env (file-name-concat root "bash-env"))
             (bash-env-marker (file-name-concat root "bash-env-ran"))
             (directory-write (file-name-concat credentials "new-token"))
             (niwa-protected-paths
              `(("**/.git/**" . read-only)
                (,(concat credentials "/**") . inaccessible)))
             prepared directory-prepared)
        (unwind-protect
            (progn
              (make-directory dot-git)
              (make-directory credentials)
              (with-temp-file config (insert "old-config"))
              (with-temp-file head (insert "old-head"))
              (with-temp-file token (insert "token-value"))
              (with-temp-file sibling (insert "sibling-value"))
              (with-temp-file bash-env
                (insert (format "printf leaked > %s\n"
                                (shell-quote-argument bash-env-marker))))
              (let ((command
                     (list
                      "sh" "-c"
                      (format
                       (concat
                        "cat %s > %s; "
                        "printf new-config > %s; "
                        "if value=$(cat %s 2>/dev/null); then "
                        "printf '%%s' \"$value\" > %s; fi; "
                        "if value=$(ls %s 2>/dev/null); then "
                        "printf '%%s' \"$value\" > %s; fi; "
                        "printf new-head > %s 2>/dev/null || true")
                       (shell-quote-argument token)
                       (shell-quote-argument token-copy)
                       (shell-quote-argument config)
                       (shell-quote-argument sibling)
                       (shell-quote-argument sibling-copy)
                       (shell-quote-argument credentials)
                       (shell-quote-argument parent-list)
                       (shell-quote-argument head)))))
                (setq prepared
                      (niwa-sandbox-prepare
                       command root (list root)
                       `(:file-system
                         ((:path ,token :access read)
                          (:path ,config :access write)))))
                (should
                 (zerop
                  (let ((process-environment
                         (cons (concat "BASH_ENV=" bash-env)
                               process-environment)))
                    (apply #'call-process
                           (car (plist-get prepared :command)) nil nil nil
                           (cdr (plist-get prepared :command)))))))
              (should-not (file-exists-p bash-env-marker))
              (should (equal "token-value"
                             (with-temp-buffer
                               (insert-file-contents token-copy)
                               (buffer-string))))
              (should (equal "new-config"
                             (with-temp-buffer
                               (insert-file-contents config)
                               (buffer-string))))
              (should-not (file-exists-p sibling-copy))
              (should-not (file-exists-p parent-list))
              (should (equal "old-head"
                             (with-temp-buffer
                               (insert-file-contents head)
                               (buffer-string))))
              (setq directory-prepared
                    (niwa-sandbox-prepare
                     (list "sh" "-c"
                           (format "printf directory-write > %s"
                                   (shell-quote-argument directory-write)))
                     root (list root)
                     `(:file-system
                       ((:path ,(file-name-as-directory credentials)
                         :access write :recursive t)))))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get directory-prepared :command))
                       nil nil nil
                       (cdr (plist-get directory-prepared :command)))))
              (should (equal "directory-write"
                             (with-temp-buffer
                               (insert-file-contents directory-write)
                               (buffer-string)))))
          (niwa-sandbox-cleanup directory-prepared)
          (niwa-sandbox-cleanup prepared)
          (delete-directory root t)))))
  :doc "real additive filesystem parent:
a broad read grant keeps an inaccessible descendant masked"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "niwa-sandbox-parent-root-" t))
             (external (make-temp-file "niwa-sandbox-parent-read-" t))
             (credentials (file-name-concat external "credentials"))
             (secret (file-name-concat credentials "token"))
             (niwa-protected-paths
              `((,(concat credentials "/**") . inaccessible)))
             prepared)
        (unwind-protect
            (progn
              (make-directory credentials)
              (with-temp-file secret (insert "do-not-read"))
              (setq prepared
                    (niwa-sandbox-prepare
                     (list "sh" "-c"
                           (format "test ! -r %s"
                                   (shell-quote-argument secret)))
                     root (list root)
                     `(:file-system
                       ((:path ,external :access read :recursive t)))))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get prepared :command)) nil nil nil
                       (cdr (plist-get prepared :command))))))
          (niwa-sandbox-cleanup prepared)
          (delete-directory external t)
          (delete-directory root t)))))
  :doc "real file masks refuse reads and writes while preserving child stdin"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((root (make-temp-file "niwa-sandbox-file-masks-" t))
           (first (file-name-concat root "first"))
           (second (file-name-concat root "second"))
           (niwa-protected-paths
            (list (cons first 'inaccessible) (cons second 'inaccessible)))
           prepared)
      (unwind-protect
          (progn
            (dolist (file (list first second))
              (with-temp-file file (insert "protected contents")))
            (setq prepared
                  (niwa-sandbox-prepare
                   (list "sh" "-c"
                         (concat "read -r line; test \"$line\" = 'child stdin' || exit 41; "
                                 "for path in \"$@\"; do "
                                 "if cat \"$path\" 2>/dev/null; then exit 42; fi; "
                                 "if chmod 600 \"$path\" 2>/dev/null; then exit 43; fi; "
                                 "done; printf 'mask enforcement complete' ")
                         "masked-files" first second)
                   root (list root)))
            (should (eq 'confined (plist-get prepared :state)))
            (with-temp-buffer
              (insert "child stdin\n")
              (should (zerop
                       (apply #'call-process-region
                              (point-min) (point-max)
                              (car (plist-get prepared :command)) t t nil
                              (cdr (plist-get prepared :command)))))
              (should (string-match-p "mask enforcement complete" (buffer-string))))
            (dolist (file (list first second))
              (with-temp-buffer
                (insert-file-contents file)
                (should (equal "protected contents" (buffer-string))))))
        (niwa-sandbox-cleanup prepared)
        (delete-directory root t))))
  :doc "mixed grant ordering preserves ancestor masks, later grants, and stdin"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (plist-get availability :reason))))
    (let* ((parent (make-temp-file "niwa-sandbox-mixed-mounts-" t))
           (root (file-name-concat parent "workspace"))
           (tree (file-name-concat parent "tree"))
           (secret (file-name-concat tree "secret"))
           (hidden (file-name-concat parent "hidden"))
           (readable (file-name-concat hidden "readable"))
           (sibling (file-name-concat hidden "sibling"))
           (writable (file-name-concat parent "writable"))
           (masked (file-name-concat root "masked"))
           (niwa-protected-paths
            `((,secret . inaccessible)
              (,(concat hidden "/**") . inaccessible)
              (,masked . inaccessible)))
           prepared)
      (unwind-protect
          (progn
            (dolist (directory (list root tree hidden))
              (make-directory directory))
            (dolist (file (list secret readable sibling writable masked))
              (with-temp-file file (insert "original")))
            (setq prepared
                  (niwa-sandbox-prepare
                   (list "sh" "-c"
                         (concat
                          "set -eu; read -r line; test \"$line\" = 'child stdin'; "
                          "test \"$(cat \"$2\")\" = original; "
                          "printf tree > \"$1/new\"; printf changed > \"$3\"; "
                          "if (printf denied > \"$2\") 2>/dev/null; then exit 41; fi; "
                          "for path in \"$4\" \"$5\" \"$6\"; do "
                          "if cat \"$path\" 2>/dev/null; then exit 42; fi; "
                          "if chmod 600 \"$path\" 2>/dev/null; then exit 43; fi; "
                          "done; printf 'mixed mounts complete'")
                         "mixed-mounts" tree readable writable secret sibling masked)
                   root (list root)
                   ;; The ancestor must be mounted first even though it is
                   ;; between two later grants in the requested profile.
                   (list :file-system
                         (list (list :path readable :access 'read)
                               (list :path tree :access 'write :recursive t)
                               (list :path writable :access 'write)))))
            (should (eq 'confined (plist-get prepared :state)))
            (with-temp-buffer
              (insert "child stdin\n")
              (let ((status
                     (apply #'call-process-region
                            (point-min) (point-max)
                            (car (plist-get prepared :command)) t t nil
                            (cdr (plist-get prepared :command)))))
                (ert-info ((buffer-string)) (should (zerop status))))
              (should (string-match-p "mixed mounts complete" (buffer-string))))
            (dolist (pair `((,writable . "changed")
                           (,(file-name-concat tree "new") . "tree")
                           (,secret . "original") (,readable . "original")
                           (,sibling . "original") (,masked . "original")))
              (with-temp-buffer
                (insert-file-contents (car pair))
                (should (equal (cdr pair) (buffer-string))))))
        (niwa-sandbox-cleanup prepared)
        (delete-directory parent t))))
  :doc "real concurrent launches retain state protection after the creator cleans up"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (unless (plist-get (niwa-sandbox-probe) :available)
      (ert-skip "Bubblewrap unavailable"))
    (let* ((root (make-temp-file "niwa-sandbox-concurrent-state-" t))
           (state (file-name-concat root ".niwa/artifacts/.state"))
           (ready (file-name-concat root "ready"))
           (niwa-protected-paths '(("**/.niwa/artifacts/.state/**" . read-only)))
           first second process)
      (unwind-protect
          (progn
            (setq first (niwa-sandbox-prepare '("true") root (list root))
                  second
                  (niwa-sandbox-prepare
                   '("sh" "-c" "set -e; touch ready; while [ ! -f go ]; do sleep 0.01; done; mkdir -p .niwa/artifacts/.state; if (printf forged >.niwa/artifacts/.state/forged) 2>/dev/null; then exit 1; fi; printf authored >.niwa/artifacts/new.html")
                   root (list root)))
            (with-temp-buffer
              (setq process (make-process :name "niwa-concurrent-state-test"
                                          :buffer (current-buffer)
                                          :command (plist-get second :command)
                                          :sentinel #'ignore))
              (let ((deadline (+ (float-time) 5)))
                (while (and (not (file-exists-p ready))
                            (process-live-p process) (< (float-time) deadline))
                  (accept-process-output process 0.01)))
              (should (file-exists-p ready))
              (niwa-sandbox-cleanup first)
              (write-region "go" nil (file-name-concat root "go") nil 'silent)
              (let ((deadline (+ (float-time) 5)))
                (while (and (process-live-p process) (< (float-time) deadline))
                  (accept-process-output process 0.01)))
              (should (eq 'exit (process-status process)))
              (should (zerop (process-exit-status process))))
            (should (file-directory-p state))
            (should-not (file-exists-p (file-name-concat state "forged")))
            (should (file-exists-p (file-name-concat root ".niwa/artifacts/new.html"))))
        (when (and process (process-live-p process)) (delete-process process))
        (niwa-sandbox-cleanup first)
        (niwa-sandbox-cleanup second)
        (delete-directory root t))))
  :doc "real state protection refuses launch if its prepared directory disappears"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (unless (plist-get (niwa-sandbox-probe) :available)
      (ert-skip "Bubblewrap unavailable"))
    (let* ((root (make-temp-file "niwa-sandbox-vanished-state-" t))
           (niwa-protected-paths '(("**/.niwa/artifacts/.state/**" . read-only)))
           prepared)
      (unwind-protect
          (progn
            (setq prepared (niwa-sandbox-prepare '("touch" "launched") root (list root)))
            (delete-directory (file-name-concat root ".niwa/artifacts/.state"))
            (with-temp-buffer
              (should-not (zerop (apply #'call-process (car (plist-get prepared :command))
                                       nil (current-buffer) nil (cdr (plist-get prepared :command))))))
            (should-not (file-exists-p (file-name-concat root "launched"))))
        (niwa-sandbox-cleanup prepared)
        (delete-directory root t))))
  :doc "real state protection permits authored creation and atomic replacement before store use"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (unless (plist-get (niwa-sandbox-probe) :available)
      (ert-skip "Bubblewrap unavailable"))
    (dolist (existing '(nil t))
      (let* ((root (make-temp-file "niwa-sandbox-state-real-" t))
             (store (file-name-concat root ".niwa/artifacts"))
             (state (file-name-concat store ".state"))
             (niwa-protected-paths
              '(("**/.niwa/artifacts/.state/**" . read-only)
                ("**/.niwa/leases/**" . read-only)))
             prepared)
        (unwind-protect
            (progn
              (when existing
                (make-directory (file-name-concat state "old") t)
                (write-region "metadata" nil (file-name-concat state "old/meta.el") nil 'silent))
              (setq prepared
                    (niwa-sandbox-prepare
                     '("sh" "-c" "set -e; cd .niwa/artifacts; mkdir new; printf original >new/index.html; printf replacement >new/tmp; mv new/tmp new/index.html; printf asset >new/style.css; if (printf forged >.state/old/meta.el) 2>/dev/null; then exit 1; fi; if mkdir .state/new 2>/dev/null; then exit 2; fi; if touch ../leases/forged 2>/dev/null; then exit 3; fi")
                     root (list root)))
              (should (eq 'confined (plist-get prepared :state)))
              (with-temp-buffer
                (should (zerop (apply #'call-process (car (plist-get prepared :command))
                                      nil (current-buffer) nil (cdr (plist-get prepared :command))))))
              (should (equal "replacement"
                             (with-temp-buffer
                               (insert-file-contents (file-name-concat store "new/index.html"))
                               (buffer-string))))
              (should (file-exists-p (file-name-concat store "new/style.css")))
              (should-not (file-exists-p (file-name-concat state "new")))
              (when existing
                (should (equal "metadata"
                               (with-temp-buffer
                                 (insert-file-contents (file-name-concat state "old/meta.el"))
                                 (buffer-string))))))
          (niwa-sandbox-cleanup prepared)
          (delete-directory root t)))))
  :doc "real protected paths:
`niwa-sandbox-prepare' keeps Git readable, hides credentials, and guards missing roots"
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (let ((availability (niwa-sandbox-probe)))
      (unless (plist-get availability :available)
        (ert-skip (or (plist-get availability :reason)
                      "Bubblewrap unavailable")))
      (let* ((root (make-temp-file "niwa-sandbox-protected-real-" t))
             (dot-git (file-name-concat root ".git"))
             (head (file-name-concat dot-git "HEAD"))
             (credentials (file-name-concat root "credentials"))
             (secret (file-name-concat credentials "token"))
             (missing (file-name-concat root "missing"))
             (blocked (file-name-concat root "blocked"))
             (blocked-head (file-name-concat blocked ".git" "HEAD"))
             (read-result (file-name-concat root "git-head"))
             (leak-result (file-name-concat root "leaked"))
             (niwa-protected-paths
              `(("**/.git/**" . read-only)
                (,(concat credentials "/**") . inaccessible)
                (,(concat missing "/**") . inaccessible)))
             prepared output-buffer)
        (unwind-protect
            (progn
              (make-directory dot-git)
              (make-directory credentials)
              (make-directory (file-name-directory blocked-head) t)
              (with-temp-file head (insert "ref: refs/heads/main\n"))
              (with-temp-file blocked-head
                (insert "ref: refs/heads/blocked\n"))
              (with-temp-file secret (insert "do-not-leak\n"))
              (set-file-modes blocked #o000)
              (let* ((descendant
                      (format
                       (concat "cat %s > %s; "
                               "printf mutate > %s 2>/dev/null || true; "
                               "if value=$(cat %s 2>/dev/null); then "
                               "printf '%%s' \"$value\" > %s; fi; "
                               "mkdir %s 2>/dev/null || true; "
                               "chmod 700 %s 2>/dev/null || true; "
                               "printf escaped > %s 2>/dev/null || true")
                       (shell-quote-argument head)
                       (shell-quote-argument read-result)
                       (shell-quote-argument head)
                       (shell-quote-argument secret)
                       (shell-quote-argument leak-result)
                       (shell-quote-argument missing)
                       (shell-quote-argument blocked)
                       (shell-quote-argument blocked-head)))
                     (command (list "sh" "-c"
                                    (format "sh -c %s"
                                            (shell-quote-argument descendant)))))
                (setq prepared
                      (niwa-sandbox-prepare command root (list root))))
              (setq output-buffer
                    (generate-new-buffer " *niwa-protected-bwrap-test*"))
              (should
               (zerop
                (apply #'call-process
                       (car (plist-get prepared :command)) nil output-buffer nil
                       (cdr (plist-get prepared :command)))))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents read-result)
                  (buffer-string))
                "ref: refs/heads/main\n"))
              (should
               (equal
                (with-temp-buffer
                  (insert-file-contents head)
                  (buffer-string))
                "ref: refs/heads/main\n"))
              (should-not (file-exists-p leak-result)))
          (when (buffer-live-p output-buffer)
            (kill-buffer output-buffer))
          (niwa-sandbox-cleanup prepared)
          (set-file-modes blocked #o700)
          (should
           (equal
            (with-temp-buffer
              (insert-file-contents blocked-head)
              (buffer-string))
            "ref: refs/heads/blocked\n"))
          (should-not (file-exists-p missing))
          (delete-directory root t))))))

(niwa-deftest niwa-sandbox-launch-failed-p ()
  ,test
  (test)
  :doc "pre-exec failure:
`niwa-sandbox-launch-failed-p' identifies failures before the marker"
  (let ((preparation '(:state confined :marker "private-start-marker")))
    (should (niwa-sandbox-launch-failed-p
             preparation '(:exit-code 125 :timed-out-p nil :output "failed")))
    (should-not
     (niwa-sandbox-launch-failed-p
      preparation
      '(:exit-code 125 :timed-out-p nil
        :output "private-start-marker\ncommand failed")))
    (should-not
     (niwa-sandbox-launch-failed-p
      preparation
      '(:exit-code 15 :termination signaled
        :timed-out-p nil :output "")))
    (should-not
     (niwa-sandbox-launch-failed-p
      preparation '(:exit-code -1 :timed-out-p t :output "")))))

(niwa-deftest niwa-sandbox--marker-script-grant-failure ()
  ,test
  (test)
  :doc "exact-grant failure marker:
the real grant refusal does not claim that the requested command started"
  (let* ((root (make-temp-file "niwa-sandbox-marker-" t))
         (side-effect (file-name-concat root "executed"))
         (marker "NIWA_SANDBOX_STARTED_test")
         (output-buffer (generate-new-buffer " *niwa-sandbox-marker*"))
         exit-code child-result preparation)
    (unwind-protect
        (progn
          (let ((process-environment
                 (cons "NIWA_SANDBOX_GRANT_FAILURE=1"
                       process-environment)))
            (setq exit-code
                  (call-process
                   "sh" nil output-buffer nil
                   "-c" niwa-sandbox--marker-script
                   "niwa-sandbox" marker
                   "sh" "-c"
                   (format ": > %s" (shell-quote-argument side-effect)))))
          (setq child-result
                (list :exit-code exit-code
                      :timed-out-p nil
                      :output (with-current-buffer output-buffer
                                (buffer-string)))
                preparation (list :state 'confined :marker marker))
          (should (= 125 exit-code))
          (should-not
           (member marker
                   (split-string (plist-get child-result :output) "\n" t)))
          (should-not (file-exists-p side-effect))
          (should (niwa-sandbox-launch-failed-p preparation child-result)))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (delete-directory root t))))

(niwa-deftest niwa-sandbox-strip-marker ()
  ,test
  (test)
  :doc "private marker:
`niwa-sandbox-strip-marker' preserves command output and removes its line"
  (should
   (equal
    (plist-get
     (niwa-sandbox-strip-marker
      '(:marker "private-start-marker")
      '(:exit-code 0 :output "notice\nprivate-start-marker\ncommand\n"))
     :output)
    "notice\ncommand\n"))
  :doc "exact marker lines preserve separators, quoted lookalikes, and result metadata"
  (let* ((marker "private.[start]*")
         (preparation (list :state 'confined :marker marker))
         (lines (list "" marker (upcase marker) "ordinary" (concat "prefix " marker) (concat marker " suffix") "unicode: \u754c")))
    (dolist (a lines)
      (dolist (b lines)
        (dolist (c lines)
          (let* ((output (mapconcat #'identity (list a b c) "\n"))
                 (expected (mapconcat #'identity (delete marker (split-string output "\n" nil)) "\n"))
                 (result (list :output output :exit-code 7 :extra 'preserved))
                 (cleaned (niwa-sandbox-strip-marker preparation result)))
            (should (equal expected (plist-get cleaned :output)))
            (should (= 7 (plist-get cleaned :exit-code)))
            (should (eq 'preserved (plist-get cleaned :extra)))
            (should (equal output (plist-get result :output)))
            (should (eq (not (member marker (split-string output "\n" nil)))
                        (and (niwa-sandbox-launch-failed-p preparation result) t)))))))
    (dolist (output (list marker (concat marker "\n") "ordinary" ""
                          (concat "prefix\n" (apply #'concat (make-list 2000 (concat marker "\n"))) "tail\n")
                          (concat "prefix\n" (apply #'concat (make-list 2000 (concat marker "\n"))) marker)))
      (should (equal (mapconcat #'identity (delete marker (split-string output "\n" nil)) "\n")
                     (plist-get (niwa-sandbox-strip-marker preparation (list :output output)) :output))))))

(niwa-deftest niwa-sandbox-status-text ()
  ,test
  (test)
  :doc "confined status:
`niwa-sandbox-status-text' reports filesystem and network boundaries"
  (should
   (equal
    (niwa-sandbox-status-text
     '(:sandbox bubblewrap :filesystem workspace-write :network isolated))
    "sandbox: bubblewrap; filesystem: workspace-write; network: isolated"))
  :doc "additive filesystem status:
`niwa-sandbox-status-text' reports active read and write grant counts"
  (should
   (equal
    (niwa-sandbox-status-text
     '(:sandbox bubblewrap :filesystem workspace-write :network isolated
       :additional-filesystem-read 2 :additional-filesystem-write 1))
    (concat "sandbox: bubblewrap; filesystem: workspace-write; network: isolated"
            "; additional filesystem: 2 read, 1 write")))
  :doc "host socket status:
`niwa-sandbox-status-text' names the Landlock fallback, not the restriction"
  (should
   (equal
    (niwa-sandbox-status-text
     '(:sandbox bubblewrap :filesystem workspace-write :network isolated
       :host-sockets allow-listed))
    "sandbox: bubblewrap; filesystem: workspace-write; network: isolated"))
  (should
   (equal
    (niwa-sandbox-status-text
     '(:sandbox bubblewrap :filesystem workspace-write :network isolated
       :host-sockets reachable :host-sockets-reason "Landlock is not enabled"))
    (concat "sandbox: bubblewrap; filesystem: workspace-write; network: isolated"
            "; host sockets: reachable (Landlock is not enabled)")))
  :doc "unrestricted status:
`niwa-sandbox-status-text' includes the persistent fallback reason"
  (should
   (equal
    (niwa-sandbox-status-text
     '(:sandbox unavailable :filesystem unrestricted :network unrestricted
       :reason "probe failed"))
    "sandbox: unavailable; filesystem: unrestricted; network: unrestricted; reason: probe failed")))

(niwa-deftest niwa-sandbox--record-launch-failure ()
  ,test
  (test)
  :doc "launch failure state:
`niwa-sandbox--record-launch-failure' records a retryable runtime verdict"
  (let ((niwa-sandbox--probe-cache '((nil . (:available t)))))
    (let ((facts
           (niwa-sandbox--record-launch-failure
            '(:exit-code 125 :output "backend refused"))))
      (let ((cached (alist-get nil niwa-sandbox--probe-cache)))
        (should-not (plist-get cached :available))
        (should (plist-get cached :retry-on-execution)))
      (should (eq (plist-get facts :sandbox) 'refused))
      (should (plist-get facts :refused))
      (should (eq (plist-get facts :filesystem) 'unavailable))
      (should (string-match-p "backend refused" (plist-get facts :reason)))))
  :doc "empty backend output:
`niwa-sandbox--record-launch-failure' retains the Bubblewrap exit code"
  (let ((niwa-sandbox--probe-cache '((nil . (:available t)))))
    (let ((facts
           (niwa-sandbox--record-launch-failure
            '(:exit-code 125 :output ""))))
      (should (string-match-p "exit code 125" (plist-get facts :reason)))))
  :doc "process spawn error:
`niwa-sandbox--record-launch-failure' retains structured launcher errors"
  (let ((niwa-sandbox--probe-cache '((nil . (:available t)))))
    (let ((facts
           (niwa-sandbox--record-launch-failure
            '(:exit-code -1 :output ""
              :error (file-error "Process spawn failed")))))
      (should (string-match-p "Process spawn failed"
                              (plist-get facts :reason))))))

(niwa-deftest niwa-sandbox-directory-read-masked-p
  (:doc "read scope accounts for containing masks, globs and canonical aliases")
  (let* ((root (make-temp-file "niwa-mask-scope-" t))
         (tree (file-name-concat root "tree"))
         (child (file-name-concat tree "child"))
         (alias (file-name-concat root "alias")))
    (unwind-protect
        (progn
          (make-directory child t)
          (make-symbolic-link tree alias)
          (dolist (pattern (list tree (concat tree "/**") alias
                                "**/tree/**"))
            (let ((niwa-protected-paths (list (cons pattern 'inaccessible))))
              (should (niwa-sandbox-directory-read-masked-p child root))
              (should (niwa-sandbox-directory-read-masked-p
                       (file-name-concat alias "child") root))
              (should-not (niwa-sandbox-directory-read-masked-p root root))))
          (let ((niwa-protected-paths (list (cons tree 'read-only))))
            (should-not (niwa-sandbox-directory-read-masked-p child root))))
      (delete-directory root t))))

(provide 'test-niwa-sandbox)

;;; test-niwa-sandbox.el ends here
