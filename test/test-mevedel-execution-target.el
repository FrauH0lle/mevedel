;;; test-mevedel-execution-target.el --- Execution target tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers execution-target derivation, path domains, readiness, and identity.

;;; Code:

(require 'ert)
(require 'mevedel-execution-target)
(require 'mevedel-sandbox)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Target derivation

(mevedel-deftest mevedel-execution-target-create ()
  ,test
  (test)
  :doc "derives a supported local target"
  (let ((target (mevedel-execution-target-create "/srv/project/")))
    (should (equal "/srv/project/"
                   (mevedel-execution-target-workspace-root target)))
    (should (equal "/srv/project/"
                   (mevedel-execution-target-native-root target)))
    (should (null (mevedel-execution-target-prefix target)))
    (should (null (mevedel-execution-target-method target)))
    (should (equal '(:method local)
                   (mevedel-execution-target-identity target)))
    (should (eq 'supported
                (mevedel-execution-target-support-tier target))))

  :doc "derives supported SSH, Docker, and Podman targets"
  (dolist (case '(("/ssh:user@host:/srv/project/"
                   "/ssh:user@host:" ssh "/srv/project/")
                  ("/docker:dev:/workspace/"
                   "/docker:dev:" docker "/workspace/")
                  ("/podman:dev:/workspace/"
                   "/podman:dev:" podman "/workspace/")))
    (pcase-let ((`(,root ,prefix ,method ,native-root) case))
      (let ((target (mevedel-execution-target-create root)))
        (should (equal root
                       (mevedel-execution-target-workspace-root target)))
        (should (equal prefix
                       (mevedel-execution-target-prefix target)))
        (should (eq method (mevedel-execution-target-method target)))
        (should (equal method
                       (plist-get (mevedel-execution-target-identity target)
                                  :method)))
        (should (equal native-root
                       (mevedel-execution-target-native-root target)))
        (should (eq 'supported
                    (mevedel-execution-target-support-tier target))))))

  :doc "classifies Kubernetes and multi-hop targets as experimental"
  (dolist (root '("/kubernetes:pod:/workspace/"
                  "/ssh:user@host|docker:dev:/workspace/"))
    (should (eq 'experimental
                (mevedel-execution-target-support-tier
                 (mevedel-execution-target-create root)))))

  :doc "classifies other remote methods as unsupported"
  (should (eq 'unsupported
              (mevedel-execution-target-support-tier
               (mevedel-execution-target-create
                "/sudo:root@localhost:/srv/project/"))))

  :doc "keeps runtime routing syntax out of structured target identity"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal '(:method ssh :user "user" :host "host")
                   (mevedel-execution-target-identity target)))
    (should-not
     (equal (mevedel-execution-target-prefix target)
            (mevedel-execution-target-identity target)))))

(mevedel-deftest mevedel-execution-target-remote-p ()
  ,test
  (test)
  :doc "distinguishes local and TRAMP targets"
  (progn
    (should-not
     (mevedel-execution-target-remote-p
      (mevedel-execution-target-create "/srv/project/")))
    (should
     (mevedel-execution-target-remote-p
      (mevedel-execution-target-create
       "/ssh:user@host:/srv/project/")))))

(mevedel-deftest mevedel-execution-target-supported-p ()
  ,test
  (test)
  :doc "accepts only targets with the supported tier"
  (progn
    (dolist (root '("/ssh:dev:/workspace/"
                    "/scp:dev:/workspace/"
                    "/sshx:dev:/workspace/"
                    "/scpx:dev:/workspace/"
                    "/docker:dev:/workspace/"
                    "/podman:dev:/workspace/"))
      (should
       (mevedel-execution-target-supported-p
        (mevedel-execution-target-create root))))
    (should-not
     (mevedel-execution-target-supported-p
      (mevedel-execution-target-create
       "/kubernetes:pod:/workspace/")))
    (should-not
     (mevedel-execution-target-supported-p
     (mevedel-execution-target-create
       "/sudo:root@localhost:/srv/project/")))))

(mevedel-deftest mevedel-execution-target-label ()
  ,test
  (test)
  :doc "identifies local, SSH, Docker, and Podman targets compactly"
  (dolist (case '(("/srv/project/" . "local")
                  ("/ssh:user@host:/srv/project/" . "ssh:user@host")
                  ("/docker:dev:/workspace/" . "docker:dev")
                  ("/podman:dev:/workspace/" . "podman:dev")))
    (should
     (equal (cdr case)
            (mevedel-execution-target-label
             (mevedel-execution-target-create (car case)))))))

(mevedel-deftest mevedel-execution-target--process-output ()
  ,test
  (test)
  :doc "preserves the client environment for local target processes"
  (let ((process-environment (copy-sequence process-environment))
        (target (mevedel-execution-target-create temporary-file-directory)))
    (setenv "MEVEDEL_TARGET_TEST" "kept")
    (should
     (string-search
      "MEVEDEL_TARGET_TEST=kept\0"
      (mevedel-execution-target--process-output target "env" "-0")))))


;;
;;; Path domains

(mevedel-deftest mevedel-execution-target-native-path ()
  ,test
  (test)
  :doc "removes the current target prefix from a qualified path"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal "/srv/project/lib/a.el"
                   (mevedel-execution-target-native-path
                    target "/ssh:user@host:/srv/project/lib/a.el"))))

  :doc "keeps target-native absolute and relative paths readable"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (should (equal "/workspace/a.el"
                   (mevedel-execution-target-native-path
                    target "/workspace/a.el")))
    (should (equal "lib/a.el"
                   (mevedel-execution-target-native-path
                    target "lib/a.el"))))

  :doc "rejects another remote target"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (mevedel-execution-target-native-path
     target "/ssh:user@other:/srv/project/a.el")
     :type 'mevedel-execution-target-error))

  :doc "rejects explicit local quoting from a remote target"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (dolist (path '("/:/etc/passwd" "/::/etc/passwd"))
      (should-error
       (mevedel-execution-target-native-path target path)
       :type 'mevedel-execution-target-error)))

  :doc "rejects explicit remote authority from a local session"
  (let ((target (mevedel-execution-target-create "/srv/project/")))
    (should-error
     (mevedel-execution-target-native-path
     target "/ssh:user@host:/srv/project/a.el")
     :type 'mevedel-execution-target-error))

  :doc "rejects quoted local paths from a local session"
  (let ((target (mevedel-execution-target-create "/srv/project/")))
    (dolist (path '("/:/home/user/.ssh/id_rsa"
                    "/::/home/user/.ssh/id_rsa"))
      (should-error
       (mevedel-execution-target-native-path target path)
       :type 'mevedel-execution-target-error))))

(mevedel-deftest mevedel-execution-target-expand-path ()
  ,test
  (test)
  :doc "qualifies absolute target-native paths without client expansion"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal "/ssh:user@host:/etc/app.conf"
                   (mevedel-execution-target-expand-path
                    target "/etc/app.conf"))))

  :doc "resolves relative paths against the target-native directory"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (should (equal "/docker:dev:/workspace/lib/a.el"
                   (mevedel-execution-target-expand-path
                    target "a.el" "/workspace/lib/"))))

  :doc "accepts a same-target qualified directory and path"
  (let ((target (mevedel-execution-target-create
                 "/podman:dev:/workspace/")))
    (should (equal "/podman:dev:/workspace/lib/a.el"
                   (mevedel-execution-target-expand-path
                    target
                    "/podman:dev:/workspace/lib/a.el"
                    "/podman:dev:/workspace/"))))

  :doc "expands target home and environment variables"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/home/user")
            ("PROJECT_ROOT" . "/srv/project")))
    (should (equal "/ssh:user@host:/home/user/src/a.el"
                   (mevedel-execution-target-expand-path
                    target "~/src/a.el")))
    (should (equal "/ssh:user@host:/srv/project/lib/a.el"
                   (mevedel-execution-target-expand-path
                    target "${PROJECT_ROOT}/lib/a.el"))))

  :doc "rejects unknown target environment variables"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (mevedel-execution-target-environment target)
          '(("HOME" . "/root")))
    (should-error
     (mevedel-execution-target-expand-path
      target "$UNKNOWN_MEVEDEL_ROOT/a.el")
     :type 'mevedel-execution-target-error))

  :doc "rejects named-user home syntax on a remote target"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (mevedel-execution-target-expand-path target "~other/a.el")
     :type 'mevedel-execution-target-error))

  :doc "preserves local path behavior"
  (let* ((process-environment
          (cons "MEVEDEL_LOCAL_ROOT=/srv/project" process-environment))
         (target (mevedel-execution-target-create "/srv/project/")))
    (should (equal "/srv/project/lib/a.el"
                   (mevedel-execution-target-expand-path
                    target "lib/a.el")))
    (should (equal "/srv/project/lib/a.el"
                   (mevedel-execution-target-expand-path
                    target "$MEVEDEL_LOCAL_ROOT/lib/a.el"))))

  :doc "rejects a directory owned by another target"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (mevedel-execution-target-expand-path
      target "a.el" "/ssh:user@other:/srv/project/")
     :type 'mevedel-execution-target-error)))


;;
;;; Readiness

(mevedel-deftest mevedel-execution-target-probe ()
  ,test
  (test)
  :doc "collects target environment and aggregate capabilities"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        (lookups 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (cl-incf lookups)
                 (unless (equal name "bwrap")
                   (concat "/usr/bin/" name))))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest args)
                 (with-current-buffer destination
                   (insert
                    (cond
                     ((equal program "env")
                      "HOME=/home/user\0PROJECT_ROOT=/srv/project\0")
                     ((equal args '("-r")) "6.8.0-target\n")
                     ((string-suffix-p "bash" program)
                      "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                     (t "Linux\n"))))
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'ready (plist-get readiness :status)))
        (should (equal "6.8.0-target"
                       (plist-get readiness :operating-system-version)))
        (should (equal "/home/user"
                       (cdr (assoc "HOME"
                                   (mevedel-execution-target-environment
                                    target)))))
        (should (equal "/usr/bin/rg"
                       (mevedel-execution-target-capability target 'rg)))
        (should (equal "/usr/bin/gio"
                       (mevedel-execution-target-capability target 'gio)))
        (should (equal "/usr/bin/inotifywait"
                       (mevedel-execution-target-capability
                        target 'inotifywait)))
        (should-not
         (mevedel-execution-target-capability target 'bwrap))
        (should-not
         (mevedel-execution-target-missing-dependencies target)))
      (mevedel-execution-target-probe target)
      (should (= 6 lookups))))

  :doc "does not forward dynamically bound client environment variables"
  (let* ((process-environment
          (cons "MEVEDEL_CLIENT_SECRET=do-not-forward" process-environment))
         (target (mevedel-execution-target-create
                  "/ssh:user@host:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (should-not (getenv "MEVEDEL_CLIENT_SECRET"))
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/home/user\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0)))
      (should
       (eq 'ready
           (plist-get (mevedel-execution-target-probe target) :status)))))

  :doc "reports every missing hard dependency"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (when (equal name "bwrap")
                   (concat "/usr/bin/" name))))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/root\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (equal '(rg bash setsid)
                       (mevedel-execution-target-missing-dependencies
                        target)))
        (should-not (mevedel-execution-target-ready-p target)))))

  :doc "blocks a target whose environment does not define HOME"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "PATH=/usr/bin\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'missing-environment (plist-get readiness :reason)))
        (should (string-match-p
                 "HOME"
                 (mevedel-execution-target-readiness-message target))))))

  :doc "reports empty HOME with every dependency defect"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (unless (equal name "rg")
                   (concat "/usr/bin/" name))))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (cond
                    ((equal program "env") (insert "HOME=\0"))
                    ((equal program "uname") (insert "Linux\n"))
                    ((string-suffix-p "bash" program)
                     (insert "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n"))))
                 (if (equal program "/usr/bin/setsid") 1 0))))
      (let ((message
             (progn
               (mevedel-execution-target-probe target)
               (mevedel-execution-target-readiness-message target))))
        (should (string-match-p "non-empty HOME" message))
        (should (string-match-p "missing required target programs: rg" message))
        (should (string-match-p "incompatible target programs: setsid" message)))))

  :doc "does not blame dependent programs when Bash is missing"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (unless (equal name "bash")
                   (concat "/usr/bin/" name))))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (unless program
                   (error "Missing probe program"))
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/root\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (equal '(bash)
                       (plist-get readiness :missing-dependencies)))
        (should-not (plist-get readiness :incompatible-dependencies)))))

  :doc "blocks present hard dependencies whose required behavior fails"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (cond
                    ((equal program "env") (insert "HOME=/root\0"))
                    ((equal program "uname") (insert "Linux\n"))
                    ((string-suffix-p "bash" program)
                     (insert "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n"))))
                 (if (equal program "/usr/bin/setsid") 1 0))))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'incompatible-dependencies
                    (plist-get readiness :reason)))
        (should (equal '(setsid)
                       (plist-get readiness :incompatible-dependencies)))
        (should (string-match-p
                 "incompatible required target programs: setsid"
                 (mevedel-execution-target-readiness-message target))))))

  :doc "behavior-probes the bounded Glob and Grep ripgrep surface"
  (let* ((root (make-temp-file "mevedel-target-rg-probe-" t))
         (bin (file-name-concat root "bin"))
         (wrapper (file-name-concat bin "rg"))
         (log (file-name-concat root "rg-args"))
         (real-rg (or (executable-find "rg")
                      (ert-skip "rg is required")))
         (exec-path (cons bin exec-path)))
    (unwind-protect
        (progn
          (make-directory bin)
          (with-temp-file wrapper
            (insert
             (format
              "#!/bin/sh\nprintf '%%s\\n' \"$*\" >> %s\nexec %s \"$@\"\n"
              (shell-quote-argument log)
              (shell-quote-argument real-rg))))
          (set-file-modes wrapper #o755)
          (let* ((target (mevedel-execution-target-create root))
                 (readiness (mevedel-execution-target-probe target))
                 (calls
                  (with-temp-buffer
                    (insert-file-contents log)
                    (buffer-string))))
            (should (eq 'ready (plist-get readiness :status)))
            (dolist (flag '("--files" "--follow" "--sort path" "--iglob"
                            "--no-require-git"
                            "--max-columns-preview" "--files-with-matches"
                            "--count" "--multiline-dotall"))
              (should (string-search flag calls)))))
      (delete-directory root t)))

  :doc "blocks non-Linux remote targets"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (if (equal program "env")
                               "HOME=/Users/user\0"
                             "Darwin\n")))
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'unsupported-operating-system
                    (plist-get readiness :reason))))))

  :doc "refreshes a cached probe only when requested"
  (let ((target (mevedel-execution-target-create "/srv/project/"))
        (lookups 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (cl-incf lookups)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (if (equal program "env")
                               "HOME=/home/local\0"
                             "Linux\n")))
                 0)))
      (mevedel-execution-target-probe target)
      (mevedel-execution-target-probe target)
      (should (= 6 lookups))
      (mevedel-execution-target-probe target t)
      (should (= 12 lookups))))

  :doc "blocks unsupported target methods without contacting them"
  (let ((target (mevedel-execution-target-create
                 "/sudo:root@localhost:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected process"))))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'unsupported-target
                    (plist-get readiness :reason)))
        (should-not (mevedel-execution-target-ready-p target)))))

  :doc "settles a timed-out probe as blocked"
  (let ((target (mevedel-execution-target-create "/srv/project/"))
        (mevedel-execution-target--probe-timeout 0.001))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (accept-process-output nil 0.02)
                 0)))
      (let ((readiness (mevedel-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'probe-failed (plist-get readiness :reason)))
        (should (string-match-p
                 "timed out" (plist-get readiness :error))))))

  :doc "blocks a remote session when required Bubblewrap is unavailable"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/home/user\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0))
              ((symbol-function 'mevedel-sandbox-probe)
               (lambda (&optional _workdir)
                 '(:available nil :reason "User namespaces disabled"))))
      (let ((readiness
             (mevedel-execution-target-probe target nil 'required)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'sandbox-unavailable (plist-get readiness :reason)))
        (should-not (mevedel-execution-target-ready-p target)))))

  :doc "discloses unavailable Bubblewrap while allowing best-effort sessions"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/home/user\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0))
              ((symbol-function 'mevedel-sandbox-probe)
               (lambda (&optional _workdir)
                 '(:available nil :reason "User namespaces disabled"))))
      (let ((readiness
             (mevedel-execution-target-probe target nil 'best-effort)))
        (should (eq 'ready (plist-get readiness :status)))
        (should (eq 'unavailable
                    (plist-get readiness :sandbox-status)))
        (should (string-match-p
                 "will be unconfined"
                 (mevedel-execution-target-readiness-message target))))))

  :doc "forced refresh invalidates the matching Bubblewrap probe cache"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        invalidated)
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (if (equal program "env")
                               "HOME=/home/user\0"
                             "Linux\n")))
                 0))
              ((symbol-function 'mevedel-sandbox-invalidate-probe-cache)
               (lambda (&optional workdir) (setq invalidated workdir)))
              ((symbol-function 'mevedel-sandbox-probe)
               (lambda (&optional _workdir) '(:available t))))
      (mevedel-execution-target-probe target t 'best-effort)
      (should
       (equal "/ssh:user@host:/srv/project/" invalidated))))

  :doc "a fresh target invalidates same-prefix Bubblewrap facts"
  (let ((first (mevedel-execution-target-create
                "/ssh:user@host:/srv/project/"))
        (second (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        (invalidations 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (if (equal program "env")
                               "HOME=/home/user\0"
                             "Linux\n")))
                 0))
              ((symbol-function 'mevedel-sandbox-invalidate-probe-cache)
               (lambda (&optional _workdir) (cl-incf invalidations)))
              ((symbol-function 'mevedel-sandbox-probe)
               (lambda (&optional _workdir) '(:available t))))
      (mevedel-execution-target-probe first nil 'best-effort)
      (mevedel-execution-target-probe second nil 'best-effort)
      (should (= 2 invalidations))))

  :doc "reprobes after reconnect and detects a changed target incarnation"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/"))
        (connection 'first)
        (incarnation "first-incarnation")
        (lookups 0)
        (invalidations 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (cl-incf lookups)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (with-current-buffer destination
                   (insert (if (equal program "env")
                               "HOME=/root\0"
                             "Linux\n")))
                 0))
              ((symbol-function
                'mevedel-execution-target--live-connection)
               (lambda (_target) connection))
              ((symbol-function
                'mevedel-execution-target--probe-incarnation)
               (lambda (_target _bash) incarnation))
              ((symbol-function 'mevedel-sandbox-invalidate-probe-cache)
               (lambda (&optional _workdir) (cl-incf invalidations))))
      (mevedel-execution-target-probe target)
      (should (= 6 lookups))
      (should (equal "first-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (mevedel-execution-target-probe target)
      (should (= 6 lookups))
      (setq connection 'second)
      (mevedel-execution-target-probe target)
      (should (= 12 lookups))
      (should (= 2 invalidations))
      (should-not
       (mevedel-execution-target-incarnation-changed-p target))
      (setq connection 'third
            incarnation "replacement-incarnation")
      (mevedel-execution-target-probe target)
      (should (= 18 lookups))
      (should (= 3 invalidations))
      (should
       (mevedel-execution-target-incarnation-changed-p target))
      (should (equal "first-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (should (equal "replacement-incarnation"
                     (mevedel-execution-target-observed-incarnation target)))
      (mevedel-execution-target-prepare-incarnation-acknowledgement target)
      (should (equal "replacement-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (should
       (mevedel-execution-target-incarnation-changed-p target))
      (mevedel-execution-target-acknowledge-incarnation target)
      (should-not
       (mevedel-execution-target-incarnation-changed-p target))
      (should-not
       (mevedel-execution-target-observed-incarnation target))))

  :doc "reprobes when a failed initial connection later becomes live"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        connection
        (probes 0))
    (cl-letf (((symbol-function
                'mevedel-execution-target--live-connection)
               (lambda (_target) connection))
              ((symbol-function
                'mevedel-execution-target--process-output)
               (lambda (_target program &rest _args)
                 (cl-incf probes)
                 (if connection
                     (if (equal program "env")
                         "HOME=/home/user\0"
                       "Linux\n")
                   (error "Host unavailable"))))
              ((symbol-function
                'mevedel-execution-target--probe-capabilities)
               (lambda (_target)
                 '((rg . "/usr/bin/rg")
                   (bash . "/usr/bin/bash")
                   (setsid . "/usr/bin/setsid"))))
              ((symbol-function
                'mevedel-execution-target--probe-dependency-behavior)
               (lambda (_target _capabilities) nil))
              ((symbol-function
                'mevedel-execution-target--probe-incarnation)
               (lambda (_target _bash) "live-incarnation"))
              ((symbol-function
                'mevedel-sandbox-invalidate-probe-cache)
               #'ignore))
      (should (eq 'blocked
                  (plist-get (mevedel-execution-target-probe target)
                             :status)))
      (should (= 1 probes))
      (setq connection 'live)
      (should (eq 'ready
                  (plist-get (mevedel-execution-target-probe target)
                             :status)))
      (should (= 4 probes)))))

(mevedel-deftest mevedel-execution-target-seed-incarnation ()
  ,test
  (test)
  :doc "installs a persisted baseline without reporting replacement"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (mevedel-execution-target-incarnation-changed-p target) t
          (mevedel-execution-target-observed-incarnation target)
          "unacknowledged-incarnation")
    (should (eq target
                (mevedel-execution-target-seed-incarnation
                 target "persisted-incarnation")))
    (should (equal "persisted-incarnation"
                   (mevedel-execution-target-incarnation target)))
    (should-not
     (mevedel-execution-target-observed-incarnation target))
    (should-not
     (mevedel-execution-target-incarnation-changed-p target)))

  :doc "rejects an absent persisted baseline"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (should-error
     (mevedel-execution-target-seed-incarnation target nil))))

(mevedel-deftest
    mevedel-execution-target-prepare-incarnation-acknowledgement ()
  ,test
  (test)
  :doc "promotes only a staged replacement while keeping it unacknowledged"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (mevedel-execution-target-seed-incarnation target "old-incarnation")
    (setf (mevedel-execution-target-observed-incarnation target)
          "new-incarnation"
          (mevedel-execution-target-incarnation-changed-p target) t)
    (should
     (eq target
         (mevedel-execution-target-prepare-incarnation-acknowledgement
          target)))
    (should (equal "new-incarnation"
                   (mevedel-execution-target-incarnation target)))
    (should
     (mevedel-execution-target-incarnation-changed-p target)))

  :doc "rejects acknowledgement preparation without a staged observation"
  (let ((target (mevedel-execution-target-create
                 "/docker:dev:/workspace/")))
    (mevedel-execution-target-seed-incarnation target "old-incarnation")
    (should-error
     (mevedel-execution-target-prepare-incarnation-acknowledgement target))))

(mevedel-deftest mevedel-execution-target--probe-incarnation ()
  ,test
  (test)
  :doc "derives a stable fingerprint from the running Linux environment"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "bash")))
    (let* ((target (mevedel-execution-target-create default-directory))
           (first
            (mevedel-execution-target--probe-incarnation target "bash")))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" first))
      (should (equal first
                     (mevedel-execution-target--probe-incarnation
                      target "bash"))))))

(mevedel-deftest mevedel-execution-target--local-incarnation ()
  ,test
  (test)
  :doc "keeps the local incarnation stable across repeated reads"
  (should (equal (mevedel-execution-target--local-incarnation)
                 (mevedel-execution-target--local-incarnation)))
  :doc "matches the live local shell probe"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "bash")))
    (let ((target (mevedel-execution-target-create default-directory)))
      (should (equal
              (mevedel-execution-target--local-incarnation)
              (mevedel-execution-target--probe-incarnation target "bash")))))
  :doc "refreshes a local replacement before a cached readiness result"
  (let ((incarnation "first-incarnation"))
    (cl-letf (((symbol-function
                'mevedel-execution-target--local-incarnation)
               (lambda () incarnation)))
      (let ((target (mevedel-execution-target-create default-directory)))
        (setf (mevedel-execution-target-readiness target)
              '(:status ready :sandbox-mode nil))
        (setq incarnation "replacement-incarnation")
        (mevedel-execution-target-probe target)
        (should (mevedel-execution-target-incarnation-changed-p target))
        (should (equal "replacement-incarnation"
                       (mevedel-execution-target-observed-incarnation
                        target))))))
  :doc "uses the same canonical bytes for local and target probes"
  (let* ((boot "boot-fixture")
         (machine "machine-fixture")
         (start "123456")
         (hostname "target-fixture")
         (payload
          (mevedel-execution-target--incarnation-payload
           boot machine start hostname)))
    (cl-letf (((symbol-function
                'mevedel-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" boot)
                   ("/etc/machine-id" machine)
                   (_ nil))))
              ((symbol-function
                'mevedel-execution-target--local-pid1-start-time)
               (lambda () start))
              ((symbol-function 'mevedel-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'mevedel-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (mevedel-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (mevedel-execution-target--local-incarnation)
                 (mevedel-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "accepts a boot-id-only observation and matches the target probe"
  (let* ((boot "boot-only")
         (hostname "target-fixture")
         (payload
          (mevedel-execution-target--incarnation-payload
           boot "machine-fixture" nil hostname)))
    (cl-letf (((symbol-function
                'mevedel-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" boot)
                   ("/etc/machine-id" "machine-fixture")
                   (_ nil))))
              ((symbol-function
                'mevedel-execution-target--local-pid1-start-time)
               (lambda () nil))
              ((symbol-function 'mevedel-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'mevedel-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (mevedel-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (mevedel-execution-target--local-incarnation)
                 (mevedel-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "accepts a PID1-start-only observation and matches the target probe"
  (let* ((start "pid1-only")
         (hostname "target-fixture")
         (payload
          (mevedel-execution-target--incarnation-payload
           nil "machine-fixture" start hostname)))
    (cl-letf (((symbol-function
                'mevedel-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" nil)
                   ("/etc/machine-id" "machine-fixture")
                   (_ nil))))
              ((symbol-function
                'mevedel-execution-target--local-pid1-start-time)
               (lambda () start))
              ((symbol-function 'mevedel-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'mevedel-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (mevedel-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (mevedel-execution-target--local-incarnation)
                 (mevedel-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "fails closed when both strong local incarnation sources are absent"
  (cl-letf (((symbol-function
              'mevedel-execution-target--read-local-identity)
             (lambda (path)
               (pcase path
                 ("/proc/sys/kernel/random/boot_id" nil)
                 ("/etc/machine-id" "machine-only")
                 (_ nil))))
            ((symbol-function
              'mevedel-execution-target--local-pid1-start-time)
             (lambda () nil))
            ((symbol-function 'mevedel-execution-target--local-hostname)
             (lambda () "hostname-only")))
    (let ((target (mevedel-execution-target-create "/srv/project/")))
      (should-not (mevedel-execution-target--local-incarnation))
      (should-error
       (mevedel-execution-target-refresh-incarnation target)))))

(mevedel-deftest mevedel-execution-target-restore-incarnation ()
  ,test
  (test)
  :doc "retains a changed local observation when restoring a persisted baseline"
  (cl-letf (((symbol-function
              'mevedel-execution-target--local-incarnation)
             (lambda () "live-incarnation")))
    (let ((target (mevedel-execution-target-create "/srv/project/")))
      (mevedel-execution-target-restore-incarnation
       target "persisted-incarnation")
      (should (equal "persisted-incarnation"
                     (mevedel-execution-target-incarnation target)))
      (should (equal "live-incarnation"
                     (mevedel-execution-target-observed-incarnation target)))
      (should (mevedel-execution-target-incarnation-changed-p target)))))

(mevedel-deftest mevedel-execution-target-readiness-message ()
  ,test
  (test)
  :doc "reports every missing hard dependency and the retry command"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status blocked :reason missing-dependencies
            :missing-dependencies (rg setsid)))
    (let ((message (mevedel-execution-target-readiness-message target)))
      (should (string-match-p "rg, setsid" message))
      (should (string-match-p "mevedel-retry-target-readiness" message))))

  :doc "reports an unsupported target operating system"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status blocked :reason unsupported-operating-system
            :operating-system "Darwin"))
    (should (string-match-p
             "Darwin"
             (mevedel-execution-target-readiness-message target))))

  :doc "reports the underlying probe failure"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status blocked :reason probe-failed :error "Host unavailable"))
    (should (string-match-p
             "Host unavailable"
             (mevedel-execution-target-readiness-message target))))

  :doc "reports required sandbox failure and retry guidance"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target)
          '(:status blocked :reason sandbox-unavailable
            :sandbox-reason "User namespaces disabled"))
    (let ((message (mevedel-execution-target-readiness-message target)))
      (should (string-match-p "User namespaces disabled" message))
      (should (string-match-p "mevedel-retry-target-readiness" message))))

  :doc "reports ready targets compactly"
  (let ((target (mevedel-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (mevedel-execution-target-readiness target) '(:status ready))
    (should (equal "ready"
                   (mevedel-execution-target-readiness-message target)))))

(provide 'test-mevedel-execution-target)

;;; test-mevedel-execution-target.el ends here
