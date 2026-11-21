;;; test-niwa-execution-target.el --- Execution target tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers execution-target derivation, path domains, readiness, and identity.

;;; Code:

(require 'ert)
(require 'niwa-execution-target)
(require 'niwa-sandbox)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Target derivation

(niwa-deftest niwa-execution-target-create ()
  ,test
  (test)
  :doc "derives a supported local target"
  (let ((target (niwa-execution-target-create "/srv/project/")))
    (should (equal "/srv/project/"
                   (niwa-execution-target-workspace-root target)))
    (should (equal "/srv/project/"
                   (niwa-execution-target-native-root target)))
    (should (null (niwa-execution-target-prefix target)))
    (should (null (niwa-execution-target-method target)))
    (should (equal '(:method local)
                   (niwa-execution-target-identity target)))
    (should (eq 'supported
                (niwa-execution-target-support-tier target))))

  :doc "derives supported SSH, Docker, and Podman targets"
  (dolist (case '(("/ssh:user@host:/srv/project/"
                   "/ssh:user@host:" ssh "/srv/project/")
                  ("/docker:dev:/workspace/"
                   "/docker:dev:" docker "/workspace/")
                  ("/podman:dev:/workspace/"
                   "/podman:dev:" podman "/workspace/")))
    (pcase-let ((`(,root ,prefix ,method ,native-root) case))
      (let ((target (niwa-execution-target-create root)))
        (should (equal root
                       (niwa-execution-target-workspace-root target)))
        (should (equal prefix
                       (niwa-execution-target-prefix target)))
        (should (eq method (niwa-execution-target-method target)))
        (should (equal method
                       (plist-get (niwa-execution-target-identity target)
                                  :method)))
        (should (equal native-root
                       (niwa-execution-target-native-root target)))
        (should (eq 'supported
                    (niwa-execution-target-support-tier target))))))

  :doc "classifies Kubernetes and multi-hop targets as experimental"
  (dolist (root '("/kubernetes:pod:/workspace/"
                  "/ssh:user@host|docker:dev:/workspace/"))
    (should (eq 'experimental
                (niwa-execution-target-support-tier
                 (niwa-execution-target-create root)))))

  :doc "classifies other remote methods as unsupported"
  (should (eq 'unsupported
              (niwa-execution-target-support-tier
               (niwa-execution-target-create
                "/sudo:root@localhost:/srv/project/"))))

  :doc "keeps runtime routing syntax out of structured target identity"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal '(:method ssh :user "user" :host "host")
                   (niwa-execution-target-identity target)))
    (should-not
     (equal (niwa-execution-target-prefix target)
            (niwa-execution-target-identity target)))))

(niwa-deftest niwa-execution-target-remote-p ()
  ,test
  (test)
  :doc "distinguishes local and TRAMP targets"
  (progn
    (should-not
     (niwa-execution-target-remote-p
      (niwa-execution-target-create "/srv/project/")))
    (should
     (niwa-execution-target-remote-p
      (niwa-execution-target-create
       "/ssh:user@host:/srv/project/")))))

(niwa-deftest niwa-execution-target-supported-p ()
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
       (niwa-execution-target-supported-p
        (niwa-execution-target-create root))))
    (should-not
     (niwa-execution-target-supported-p
      (niwa-execution-target-create
       "/kubernetes:pod:/workspace/")))
    (should-not
     (niwa-execution-target-supported-p
     (niwa-execution-target-create
       "/sudo:root@localhost:/srv/project/")))))

(niwa-deftest niwa-execution-target-direct-async-capable-p ()
  ,test
  (test)
  :doc "accepts single-hop ssh and scp targets only"
  (progn
    (dolist (root '("/ssh:dev:/workspace/"
                    "/scp:dev:/workspace/"))
      (should
       (niwa-execution-target-direct-async-capable-p
        (niwa-execution-target-create root))))
    ;; sshx and scpx spawn through RemoteCommand, which direct-async
    ;; cannot use; the container clients allocate a tty per exec and
    ;; print notices ahead of the command, corrupting the marker
    ;; protocol; a hop disqualifies the whole connection; local has no
    ;; channel to privatize.
    (dolist (root '("/sshx:dev:/workspace/"
                    "/scpx:dev:/workspace/"
                    "/docker:dev:/workspace/"
                    "/podman:dev:/workspace/"
                    "/ssh:jump|ssh:dev:/workspace/"
                    "/tmp/"))
      (should-not
       (niwa-execution-target-direct-async-capable-p
        (niwa-execution-target-create root))))))

(niwa-deftest niwa-execution-target-label ()
  ,test
  (test)
  :doc "identifies local, SSH, Docker, and Podman targets compactly"
  (dolist (case '(("/srv/project/" . "local")
                  ("/ssh:user@host:/srv/project/" . "ssh:user@host")
                  ("/docker:dev:/workspace/" . "docker:dev")
                  ("/podman:dev:/workspace/" . "podman:dev")
                  ("/ssh:jump|ssh:user@host:/srv/project/" . "ssh:user@host")))
    (should
     (equal (cdr case)
            (niwa-execution-target-label
             (niwa-execution-target-create (car case))))))

  :doc "derives directory and default-directory labels without target discovery"
  (cl-letf (((symbol-function 'niwa-execution-target-create)
             (lambda (&rest _) (error "Unexpected target creation")))
            ((symbol-function 'process-file)
             (lambda (&rest _) (error "Unexpected process")))
            ((symbol-function 'tramp-send-command)
             (lambda (&rest _) (error "Unexpected remote command"))))
    (dolist (case '(("/srv/project/" . "local")
                    ("/ssh:user@host:/srv/project/" . "ssh:user@host")
                    ("/docker:dev:/workspace/" . "docker:dev")
                    ("/podman:dev:/workspace/" . "podman:dev")
                    ("/ssh:jump|ssh:user@host:/srv/project/" . "ssh:user@host")))
      (should (equal (cdr case)
                     (niwa-execution-target-label nil (car case))))
      (let ((default-directory (car case)))
        (should (equal (cdr case) (niwa-execution-target-label nil))))))

  :doc "prefers the supplied target over the directory"
  (let ((target (niwa-execution-target-create "/ssh:user@host:/srv/")))
    (should (equal "ssh:user@host"
                   (niwa-execution-target-label target "/tmp/")))))

(niwa-deftest niwa-execution-target--process-output ()
  ,test
  (test)
  :doc "preserves the client environment for local target processes"
  (let ((process-environment (copy-sequence process-environment))
        (target (niwa-execution-target-create temporary-file-directory)))
    (setenv "NIWA_TARGET_TEST" "kept")
    (should
     (string-search
      "NIWA_TARGET_TEST=kept\0"
      (niwa-execution-target--process-output target "env" "-0")))))


;;
;;; Path domains

(niwa-deftest niwa-execution-target-native-path ()
  ,test
  (test)
  :doc "removes the current target prefix from a qualified path"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal "/srv/project/lib/a.el"
                   (niwa-execution-target-native-path
                    target "/ssh:user@host:/srv/project/lib/a.el"))))

  :doc "keeps target-native absolute and relative paths readable"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (should (equal "/workspace/a.el"
                   (niwa-execution-target-native-path
                    target "/workspace/a.el")))
    (should (equal "lib/a.el"
                   (niwa-execution-target-native-path
                    target "lib/a.el"))))

  :doc "rejects another remote target"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (niwa-execution-target-native-path
     target "/ssh:user@other:/srv/project/a.el")
     :type 'niwa-execution-target-error))

  :doc "rejects explicit local quoting from a remote target"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (dolist (path '("/:/etc/passwd" "/::/etc/passwd"))
      (should-error
       (niwa-execution-target-native-path target path)
       :type 'niwa-execution-target-error)))

  :doc "rejects explicit remote authority from a local session"
  (let ((target (niwa-execution-target-create "/srv/project/")))
    (should-error
     (niwa-execution-target-native-path
     target "/ssh:user@host:/srv/project/a.el")
     :type 'niwa-execution-target-error))

  :doc "rejects quoted local paths from a local session"
  (let ((target (niwa-execution-target-create "/srv/project/")))
    (dolist (path '("/:/home/user/.ssh/id_rsa"
                    "/::/home/user/.ssh/id_rsa"))
      (should-error
       (niwa-execution-target-native-path target path)
       :type 'niwa-execution-target-error))))

(niwa-deftest niwa-execution-target-same-path-domain-p ()
  ,test
  (test)
  :doc "normalizes SSH host case and its explicit default port"
  (progn
    (should
     (niwa-execution-target-same-path-domain-p
      "/ssh:user@HOST:/repo/"
      "/ssh:user@host#22:/other/"))
    (should
     (niwa-execution-target-same-path-domain-p
      "/srv/one" "/srv/two")))

  :doc "preserves opaque container target case"
  (progn
    (should-not
     (niwa-execution-target-same-path-domain-p
      "/docker:Build:/repo/"
      "/docker:build:/repo/"))
    (should-not
     (niwa-execution-target-same-path-domain-p
      "/docker:Build|ssh:user@host:/repo/"
      "/docker:build|ssh:user@host:/repo/")))

  :doc "normalizes every SSH endpoint in a multi-hop domain"
  (should
   (niwa-execution-target-same-path-domain-p
    "/ssh:jump|ssh:user@host:/repo/"
    "/ssh:JUMP#22|ssh:user@HOST#22:/other/"))

  :doc "distinguishes remote users, hosts, and methods"
  (dolist (right '("/ssh:other@host:/repo/"
                   "/ssh:user@other:/repo/"
                   "/docker:user@host:/repo/"))
    (should-not
     (niwa-execution-target-same-path-domain-p
      "/ssh:user@host:/repo/" right))))

(niwa-deftest niwa-execution-target-expand-path ()
  ,test
  (test)
  :doc "qualifies absolute target-native paths without client expansion"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal "/ssh:user@host:/etc/app.conf"
                   (niwa-execution-target-expand-path
                    target "/etc/app.conf"))))

  :doc "collapses a doubled leading slash in the target-native path"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should (equal "/ssh:user@host:/run/user/1000"
                   (niwa-execution-target-expand-path
                    target "//run/user/1000"))))

  :doc "resolves relative paths against the target-native directory"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (should (equal "/docker:dev:/workspace/lib/a.el"
                   (niwa-execution-target-expand-path
                    target "a.el" "/workspace/lib/"))))

  :doc "accepts a same-target qualified directory and path"
  (let ((target (niwa-execution-target-create
                 "/podman:dev:/workspace/")))
    (should (equal "/podman:dev:/workspace/lib/a.el"
                   (niwa-execution-target-expand-path
                    target
                    "/podman:dev:/workspace/lib/a.el"
                    "/podman:dev:/workspace/"))))

  :doc "expands target home and environment variables"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/home/user")
            ("PROJECT_ROOT" . "/srv/project")))
    (should (equal "/ssh:user@host:/home/user/src/a.el"
                   (niwa-execution-target-expand-path
                    target "~/src/a.el")))
    (should (equal "/ssh:user@host:/srv/project/lib/a.el"
                   (niwa-execution-target-expand-path
                    target "${PROJECT_ROOT}/lib/a.el"))))

  :doc "rejects unknown target environment variables"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-environment target)
          '(("HOME" . "/root")))
    (should-error
     (niwa-execution-target-expand-path
      target "$UNKNOWN_NIWA_ROOT/a.el")
     :type 'niwa-execution-target-error))

  :doc "rejects named-user home syntax on a remote target"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (niwa-execution-target-expand-path target "~other/a.el")
     :type 'niwa-execution-target-error))

  :doc "preserves local path behavior"
  (let* ((process-environment
          (cons "NIWA_LOCAL_ROOT=/srv/project" process-environment))
         (target (niwa-execution-target-create "/srv/project/")))
    (should (equal "/srv/project/lib/a.el"
                   (niwa-execution-target-expand-path
                    target "lib/a.el")))
    (should (equal "/srv/project/lib/a.el"
                   (niwa-execution-target-expand-path
                    target "$NIWA_LOCAL_ROOT/lib/a.el"))))

  :doc "rejects a directory owned by another target"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error
     (niwa-execution-target-expand-path
      target "a.el" "/ssh:user@other:/srv/project/")
     :type 'niwa-execution-target-error)))


;;
;;; Readiness

(niwa-deftest niwa-execution-target-probe ()
  ,test
  (test)
  :doc "collects target environment and aggregate capabilities"
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'ready (plist-get readiness :status)))
        (should (equal "6.8.0-target"
                       (plist-get readiness :operating-system-version)))
        (should (equal "/home/user"
                       (cdr (assoc "HOME"
                                   (niwa-execution-target-environment
                                    target)))))
        (should (equal "/usr/bin/rg"
                       (niwa-execution-target-capability target 'rg)))
        (should (equal "/usr/bin/gio"
                       (niwa-execution-target-capability target 'gio)))
        (should (equal "/usr/bin/inotifywait"
                       (niwa-execution-target-capability
                        target 'inotifywait)))
        (should-not
         (niwa-execution-target-capability target 'bwrap))
        (should-not
         (niwa-execution-target-missing-dependencies target)))
      (niwa-execution-target-probe target)
      (should (= 6 lookups))))

  :doc "caches container, VM or WSL detection for local and remote targets"
  (dolist (case '(("/ssh:user@host:/srv/project/" "podman\n" "podman")
                  ("/tmp/" "wsl\n" "wsl")
                  ("/tmp/" "none\n" nil)
                  ("/tmp/" "<garbage>\n" nil)
                  ("/ssh:user@host:/srv/project/" fail nil)))
    (let ((target (niwa-execution-target-create (nth 0 case)))
          (scripts 0))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &optional _remote) (concat "/usr/bin/" name)))
                ((symbol-function 'process-file)
                 (lambda (program _in destination _display &rest args)
                   (if (equal program "sh")
                       (progn
                         (cl-incf scripts)
                         (should (equal "-c" (car args)))
                         (if (eq (nth 1 case) 'fail)
                             1
                           (with-current-buffer destination
                             (insert (nth 1 case)))
                           0))
                     (with-current-buffer destination
                       (insert
                        (cond
                         ((equal program "env") "HOME=/home/user\0")
                         ((equal args '("-r")) "6.8.0-target\n")
                         ((string-suffix-p "bash" program)
                          "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                         (t "Linux\n"))))
                     0))))
        (let ((readiness (niwa-execution-target-probe target)))
          (should (eq 'ready (plist-get readiness :status)))
          (should (equal (nth 2 case)
                         (plist-get readiness :virtualization))))
        (niwa-execution-target-probe target)
        (should (= 1 scripts)))))

  :doc "does not forward dynamically bound client environment variables"
  (let* ((process-environment
          (cons "NIWA_CLIENT_SECRET=do-not-forward" process-environment))
         (target (niwa-execution-target-create
                  "/ssh:user@host:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &optional _remote)
                 (concat "/usr/bin/" name)))
              ((symbol-function 'process-file)
               (lambda (program _in destination _display &rest _args)
                 (should-not (getenv "NIWA_CLIENT_SECRET"))
                 (with-current-buffer destination
                   (insert (cond
                            ((equal program "env") "HOME=/home/user\0")
                            ((string-suffix-p "bash" program)
                             "boot=fixture\nmachine=fixture\npid1-start=1\nhostname=fixture\n")
                            (t "Linux\n"))))
                 0)))
      (should
       (eq 'ready
           (plist-get (niwa-execution-target-probe target) :status)))))

  :doc "reports every missing hard dependency"
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (equal '(rg bash setsid)
                       (niwa-execution-target-missing-dependencies
                        target)))
        (should-not (niwa-execution-target-ready-p target)))))

  :doc "blocks a target whose environment does not define HOME"
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'missing-environment (plist-get readiness :reason)))
        (should (string-match-p
                 "HOME"
                 (niwa-execution-target-readiness-message target))))))

  :doc "reports empty HOME with every dependency defect"
  (let ((target (niwa-execution-target-create
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
               (niwa-execution-target-probe target)
               (niwa-execution-target-readiness-message target))))
        (should (string-match-p "non-empty HOME" message))
        (should (string-match-p "missing required target programs: rg" message))
        (should (string-match-p "incompatible target programs: setsid" message)))))

  :doc "does not blame dependent programs when Bash is missing"
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (equal '(bash)
                       (plist-get readiness :missing-dependencies)))
        (should-not (plist-get readiness :incompatible-dependencies)))))

  :doc "blocks present hard dependencies whose required behavior fails"
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'incompatible-dependencies
                    (plist-get readiness :reason)))
        (should (equal '(setsid)
                       (plist-get readiness :incompatible-dependencies)))
        (should (string-match-p
                 "incompatible required target programs: setsid"
                 (niwa-execution-target-readiness-message target))))))

  :doc "behavior-probes the bounded Glob and Grep ripgrep surface"
  (let* ((root (make-temp-file "niwa-target-rg-probe-" t))
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
          (let* ((target (niwa-execution-target-create root))
                 (readiness (niwa-execution-target-probe target))
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
  (let ((target (niwa-execution-target-create
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
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'unsupported-operating-system
                    (plist-get readiness :reason))))))

  :doc "refreshes a cached probe only when requested"
  (let ((target (niwa-execution-target-create "/srv/project/"))
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
      (niwa-execution-target-probe target)
      (niwa-execution-target-probe target)
      (should (= 6 lookups))
      (niwa-execution-target-probe target t)
      (should (= 12 lookups))))

  :doc "blocks unsupported target methods without contacting them"
  (let ((target (niwa-execution-target-create
                 "/sudo:root@localhost:/srv/project/")))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (&rest _args) (error "Unexpected lookup")))
              ((symbol-function 'process-file)
               (lambda (&rest _args) (error "Unexpected process"))))
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'unsupported-target
                    (plist-get readiness :reason)))
        (should-not (niwa-execution-target-ready-p target)))))

  :doc "settles a timed-out probe as blocked"
  (let ((target (niwa-execution-target-create "/srv/project/"))
        (niwa-execution-target--probe-timeout 0.001))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _args)
                 (accept-process-output nil 0.02)
                 0)))
      (let ((readiness (niwa-execution-target-probe target)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'probe-failed (plist-get readiness :reason)))
        (should (string-match-p
                 "timed out" (plist-get readiness :error))))))

  :doc "suspends foreign timers while probe commands wait"
  ;; A foreign timer that fires during a probe wait starts its own
  ;; remote operation on the shared connection and consumes the probe's
  ;; reply; the whole probe runs with outside timers held while its own
  ;; deadline stays armed.
  (let ((timer (run-at-time 3600 nil #'ignore))
        (target (niwa-execution-target-create "/srv/project/"))
        observed)
    (unwind-protect
        (cl-letf (((symbol-function 'process-file)
                   ;; The last call is a probe command inside the
                   ;; suspension; the first is the local hostname read
                   ;; before it.
                   (lambda (&rest _)
                     (setq observed (cons t timer-list))
                     0)))
          (niwa-execution-target-probe target)
          (should (car observed))
          (should-not (memq timer (cdr observed))))
      (cancel-timer timer)))

  :doc "blocks a remote session when required Bubblewrap is unavailable"
  (let ((target (niwa-execution-target-create
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
              ((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional _workdir)
                 '(:available nil :reason "User namespaces disabled"))))
      (let ((readiness
             (niwa-execution-target-probe target nil 'required)))
        (should (eq 'blocked (plist-get readiness :status)))
        (should (eq 'sandbox-unavailable (plist-get readiness :reason)))
        (should-not (niwa-execution-target-ready-p target)))))

  :doc "discloses unavailable Bubblewrap while allowing best-effort sessions"
  (let ((target (niwa-execution-target-create
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
              ((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional _workdir)
                 '(:available nil :reason "User namespaces disabled"))))
      (let ((readiness
             (niwa-execution-target-probe target nil 'best-effort)))
        (should (eq 'ready (plist-get readiness :status)))
        (should (eq 'unavailable
                    (plist-get readiness :sandbox-status)))
        (should (string-match-p
                 "will be unconfined"
                 (niwa-execution-target-readiness-message target))))))

  :doc "forced refresh invalidates the matching Bubblewrap probe cache"
  (let ((target (niwa-execution-target-create
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
              ((symbol-function 'niwa-sandbox-invalidate-probe-cache)
               (lambda (&optional workdir) (setq invalidated workdir)))
              ((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional _workdir) '(:available t))))
      (niwa-execution-target-probe target t 'best-effort)
      (should
       (equal "/ssh:user@host:/srv/project/" invalidated))))

  :doc "a fresh target invalidates same-prefix Bubblewrap facts"
  (let ((first (niwa-execution-target-create
                "/ssh:user@host:/srv/project/"))
        (second (niwa-execution-target-create
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
              ((symbol-function 'niwa-sandbox-invalidate-probe-cache)
               (lambda (&optional _workdir) (cl-incf invalidations)))
              ((symbol-function 'niwa-sandbox-probe)
               (lambda (&optional _workdir) '(:available t))))
      (niwa-execution-target-probe first nil 'best-effort)
      (niwa-execution-target-probe second nil 'best-effort)
      (should (= 2 invalidations))))

  :doc "reprobes after reconnect and detects a changed target incarnation"
  (let ((target (niwa-execution-target-create
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
                'niwa-execution-target--live-connection)
               (lambda (_target) connection))
              ((symbol-function
                'niwa-execution-target--probe-incarnation)
               (lambda (_target _bash) incarnation))
              ((symbol-function 'niwa-sandbox-invalidate-probe-cache)
               (lambda (&optional _workdir) (cl-incf invalidations))))
      (niwa-execution-target-probe target)
      (should (= 6 lookups))
      (should (equal "first-incarnation"
                     (niwa-execution-target-incarnation target)))
      (niwa-execution-target-probe target)
      (should (= 6 lookups))
      (setq connection 'second)
      (niwa-execution-target-probe target)
      (should (= 12 lookups))
      (should (= 2 invalidations))
      (should-not
       (niwa-execution-target-incarnation-changed-p target))
      (setq connection 'third
            incarnation "replacement-incarnation")
      (niwa-execution-target-probe target)
      (should (= 18 lookups))
      (should (= 3 invalidations))
      (should
       (niwa-execution-target-incarnation-changed-p target))
      (should (equal "first-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should (equal "replacement-incarnation"
                     (niwa-execution-target-observed-incarnation target)))
      (niwa-execution-target-prepare-incarnation-acknowledgement target)
      (should (equal "replacement-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should
       (niwa-execution-target-incarnation-changed-p target))
      (niwa-execution-target-acknowledge-incarnation target)
      (should-not
       (niwa-execution-target-incarnation-changed-p target))
      (should-not
       (niwa-execution-target-observed-incarnation target))))

  :doc "reprobes when a failed initial connection later becomes live"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        connection
        (probes 0))
    (cl-letf (((symbol-function
                'niwa-execution-target--live-connection)
               (lambda (_target) connection))
              ((symbol-function
                'niwa-execution-target--process-output)
               (lambda (_target program &rest _args)
                 (cl-incf probes)
                 (if connection
                     (if (equal program "env")
                         "HOME=/home/user\0"
                       "Linux\n")
                   (error "Host unavailable"))))
              ((symbol-function
                'niwa-execution-target--probe-capabilities)
               (lambda (_target)
                 '((rg . "/usr/bin/rg")
                   (bash . "/usr/bin/bash")
                   (setsid . "/usr/bin/setsid"))))
              ((symbol-function
                'niwa-execution-target--probe-dependency-behavior)
               (lambda (_target _capabilities) nil))
              ((symbol-function
                'niwa-execution-target--probe-incarnation)
               (lambda (_target _bash) "live-incarnation"))
              ((symbol-function
                'niwa-sandbox-invalidate-probe-cache)
               #'ignore))
      (should (eq 'blocked
                  (plist-get (niwa-execution-target-probe target)
                             :status)))
      (should (= 1 probes))
      (setq connection 'live)
      (should (eq 'ready
                  (plist-get (niwa-execution-target-probe target)
                             :status)))
      ;; env, uname -s, uname -r and the virtualization script.
      (should (= 5 probes)))))

(niwa-deftest niwa-execution-target-observe-incarnation ()
  ,test
  (test)
  :doc "refreshes a remote fingerprint without reprobing readiness"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/"))
        (incarnation "first-incarnation")
        (lookups 0)
        (fingerprints 0))
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
                'niwa-execution-target--live-connection)
               (lambda (_target) 'connection))
              ((symbol-function
                'niwa-execution-target--probe-incarnation)
               (lambda (_target _bash)
                 (cl-incf fingerprints)
                 incarnation))
              ((symbol-function 'niwa-sandbox-invalidate-probe-cache)
               #'ignore))
      (niwa-execution-target-probe target)
      (should (= 6 lookups))
      (should (= 1 fingerprints))
      ;; An unchanged target costs one target command and no capability,
      ;; environment, or sandbox work.
      (niwa-execution-target-observe-incarnation target)
      (should (= 6 lookups))
      (should (= 2 fingerprints))
      (should-not
       (niwa-execution-target-incarnation-changed-p target))
      (setq incarnation "replacement-incarnation")
      (niwa-execution-target-observe-incarnation target)
      (should (= 6 lookups))
      (should
       (niwa-execution-target-incarnation-changed-p target))
      (should (equal "first-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should (equal "replacement-incarnation"
                     (niwa-execution-target-observed-incarnation
                      target)))))

  :doc "refuses a remote observation before readiness establishes Bash"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (should-error (niwa-execution-target-observe-incarnation target)))

  :doc "observes a local replacement without any target command"
  (let ((target (niwa-execution-target-create "/srv/project/"))
        (probes 0))
    (cl-letf (((symbol-function 'niwa-execution-target--process-output)
               (lambda (&rest _) (cl-incf probes) ""))
              ((symbol-function 'niwa-execution-target--local-incarnation)
               (lambda () "local-replacement")))
      (setf (niwa-execution-target-incarnation target) "local-baseline")
      (niwa-execution-target-observe-incarnation target)
      (should (= 0 probes))
      (should (niwa-execution-target-incarnation-changed-p target))
      (should (equal "local-replacement"
                     (niwa-execution-target-observed-incarnation
                      target))))))

(niwa-deftest niwa-execution-target-restore-incarnation ()
  ,test
  (test)
  :doc "installs a remote persisted baseline before observing replacement"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-incarnation-changed-p target) t
          (niwa-execution-target-observed-incarnation target)
          "unacknowledged-incarnation")
    (should (eq target
                (niwa-execution-target-restore-incarnation
                 target "persisted-incarnation")))
    (should (equal "persisted-incarnation"
                   (niwa-execution-target-incarnation target)))
    (should-not
     (niwa-execution-target-observed-incarnation target))
    (should-not
     (niwa-execution-target-incarnation-changed-p target)))

  :doc "rejects an absent persisted baseline"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (should-error
     (niwa-execution-target-restore-incarnation target nil)))

  :doc "retains a changed local observation when restoring a persisted baseline"
  (cl-letf (((symbol-function
              'niwa-execution-target--local-incarnation)
             (lambda () "live-incarnation")))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (niwa-execution-target-restore-incarnation
       target "persisted-incarnation")
      (should (equal "persisted-incarnation"
                     (niwa-execution-target-incarnation target)))
      (should (equal "live-incarnation"
                     (niwa-execution-target-observed-incarnation target)))
      (should (niwa-execution-target-incarnation-changed-p target)))))

(niwa-deftest
    niwa-execution-target-prepare-incarnation-acknowledgement ()
  ,test
  (test)
  :doc "promotes only a staged replacement while keeping it unacknowledged"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-incarnation target) "old-incarnation")
    (setf (niwa-execution-target-observed-incarnation target)
          "new-incarnation"
          (niwa-execution-target-incarnation-changed-p target) t)
    (should
     (eq target
         (niwa-execution-target-prepare-incarnation-acknowledgement
          target)))
    (should (equal "new-incarnation"
                   (niwa-execution-target-incarnation target)))
    (should
     (niwa-execution-target-incarnation-changed-p target)))

  :doc "rejects acknowledgement preparation without a staged observation"
  (let ((target (niwa-execution-target-create
                 "/docker:dev:/workspace/")))
    (setf (niwa-execution-target-incarnation target) "old-incarnation")
    (should-error
     (niwa-execution-target-prepare-incarnation-acknowledgement target))))

(niwa-deftest niwa-execution-target--probe-incarnation ()
  ,test
  (test)
  :doc "derives a stable fingerprint from the running Linux environment"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "bash")))
    (let* ((target (niwa-execution-target-create default-directory))
           (first
            (niwa-execution-target--probe-incarnation target "bash")))
      (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" first))
      (should (equal first
                     (niwa-execution-target--probe-incarnation
                      target "bash"))))))

(niwa-deftest niwa-execution-target--local-hostname ()
  ,test
  (test)
  :doc "reads the nodename uname prints without starting a process"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux)
                      (file-readable-p "/proc/sys/kernel/hostname")
                      (executable-find "uname")))
    (let ((expected (string-trim (shell-command-to-string "uname -n")))
          (spawned nil))
      (cl-letf (((symbol-function 'process-file)
                 (lambda (&rest _) (setq spawned t) 1)))
        (should (equal expected (niwa-execution-target--local-hostname))))
      (should-not spawned)))
  :doc "falls back to uname where procfs offers no hostname"
  (progn
    (skip-unless (executable-find "uname"))
    (cl-letf (((symbol-function 'niwa-execution-target--read-local-identity)
               (lambda (_path) nil)))
      (should (equal (string-trim (shell-command-to-string "uname -n"))
                     (niwa-execution-target--local-hostname)))))
  :doc "decodes a non-ASCII hostname as process output is decoded"
  (let ((default-process-coding-system '(utf-8-unix . utf-8-unix)))
    (cl-letf (((symbol-function 'niwa-execution-target--read-local-identity)
               (lambda (_path) (encode-coding-string "höst" 'utf-8))))
      (should (equal "höst" (niwa-execution-target--local-hostname))))))

(niwa-deftest niwa-execution-target--local-incarnation ()
  ,test
  (test)
  :doc "keeps the local incarnation stable across repeated reads"
  (should (equal (niwa-execution-target--local-incarnation)
                 (niwa-execution-target--local-incarnation)))
  :doc "matches the live local shell probe"
  (progn
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "bash")))
    (let ((target (niwa-execution-target-create default-directory)))
      (should (equal
              (niwa-execution-target--local-incarnation)
              (niwa-execution-target--probe-incarnation target "bash")))))
  :doc "refreshes a local replacement before a cached readiness result"
  (let ((incarnation "first-incarnation"))
    (cl-letf (((symbol-function
                'niwa-execution-target--local-incarnation)
               (lambda () incarnation)))
      (let ((target (niwa-execution-target-create default-directory)))
        (setf (niwa-execution-target-readiness target)
              '(:status ready :sandbox-mode nil))
        (setq incarnation "replacement-incarnation")
        (niwa-execution-target-probe target)
        (should (niwa-execution-target-incarnation-changed-p target))
        (should (equal "replacement-incarnation"
                       (niwa-execution-target-observed-incarnation
                        target))))))
  :doc "uses the same canonical bytes for local and target probes"
  (let* ((boot "boot-fixture")
         (machine "machine-fixture")
         (start "123456")
         (hostname "target-fixture")
         (payload
          (niwa-execution-target--incarnation-payload
           boot machine start hostname)))
    (cl-letf (((symbol-function
                'niwa-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" boot)
                   ("/etc/machine-id" machine)
                   (_ nil))))
              ((symbol-function
                'niwa-execution-target--local-pid1-start-time)
               (lambda () start))
              ((symbol-function 'niwa-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'niwa-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (niwa-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (niwa-execution-target--local-incarnation)
                 (niwa-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "accepts a boot-id-only observation and matches the target probe"
  (let* ((boot "boot-only")
         (hostname "target-fixture")
         (payload
          (niwa-execution-target--incarnation-payload
           boot "machine-fixture" nil hostname)))
    (cl-letf (((symbol-function
                'niwa-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" boot)
                   ("/etc/machine-id" "machine-fixture")
                   (_ nil))))
              ((symbol-function
                'niwa-execution-target--local-pid1-start-time)
               (lambda () nil))
              ((symbol-function 'niwa-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'niwa-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (niwa-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (niwa-execution-target--local-incarnation)
                 (niwa-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "accepts a PID1-start-only observation and matches the target probe"
  (let* ((start "pid1-only")
         (hostname "target-fixture")
         (payload
          (niwa-execution-target--incarnation-payload
           nil "machine-fixture" start hostname)))
    (cl-letf (((symbol-function
                'niwa-execution-target--read-local-identity)
               (lambda (path)
                 (pcase path
                   ("/proc/sys/kernel/random/boot_id" nil)
                   ("/etc/machine-id" "machine-fixture")
                   (_ nil))))
              ((symbol-function
                'niwa-execution-target--local-pid1-start-time)
               (lambda () start))
              ((symbol-function 'niwa-execution-target--local-hostname)
               (lambda () hostname))
              ((symbol-function
                'niwa-execution-target--process-output)
               (lambda (&rest _args) payload)))
      (let ((target (niwa-execution-target-create
                     "/ssh:user@target-fixture:/workspace/")))
        (should (equal
                 (niwa-execution-target--local-incarnation)
                 (niwa-execution-target--probe-incarnation
                  target "bash"))))))
  :doc "fails closed when both strong local incarnation sources are absent"
  (cl-letf (((symbol-function
              'niwa-execution-target--read-local-identity)
             (lambda (path)
               (pcase path
                 ("/proc/sys/kernel/random/boot_id" nil)
                 ("/etc/machine-id" "machine-only")
                 (_ nil))))
            ((symbol-function
              'niwa-execution-target--local-pid1-start-time)
             (lambda () nil))
            ((symbol-function 'niwa-execution-target--local-hostname)
             (lambda () "hostname-only")))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (should-not (niwa-execution-target--local-incarnation))
      (should-error
       (niwa-execution-target-refresh-incarnation target)))))

(niwa-deftest niwa-execution-target--install-hint ()
  ,test
  (test)
  :doc "names the packages supplying the missing programs"
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program &optional _remote)
               (and (equal program "apk") "/sbin/apk"))))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (should (equal "apk add ripgrep util-linux"
                     (niwa-execution-target--install-hint
                      target '(rg setsid))))))

  :doc "prefers the first recognized package manager on the target"
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program &optional _remote)
               (and (member program '("dnf" "pacman"))
                    (file-name-concat "/usr/bin" program)))))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (should (equal "dnf install -y ripgrep"
                     (niwa-execution-target--install-hint target '(rg))))))

  :doc "withholds a hint when no recognized package manager is present"
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program &optional _remote) nil)))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (should-not (niwa-execution-target--install-hint target '(rg)))))

  :doc "withholds a hint when no missing program maps to a package"
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program &optional _remote) "/sbin/apk")))
    (let ((target (niwa-execution-target-create "/srv/project/")))
      (should-not (niwa-execution-target--install-hint target '(bwrap))))))

(niwa-deftest niwa-execution-target-readiness-message ()
  ,test
  (test)
  :doc "reports every missing hard dependency and the retry command"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status blocked :reason missing-dependencies
            :missing-dependencies (rg setsid)))
    (let ((message (niwa-execution-target-readiness-message target)))
      (should (string-match-p "rg, setsid" message))
      (should-not (string-match-p "install them with" message))
      (should (string-match-p "niwa-retry-target-readiness" message))))

  :doc "offers the probed install command for missing dependencies"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status blocked :reason missing-dependencies
            :missing-dependencies (rg setsid)
            :install-hint "apk add ripgrep util-linux"))
    (let ((message (niwa-execution-target-readiness-message target)))
      (should (string-match-p "install them with: apk add ripgrep util-linux"
                              message))
      (should (string-match-p "niwa-retry-target-readiness" message))))

  :doc "reports an unsupported target operating system"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status blocked :reason unsupported-operating-system
            :operating-system "Darwin"))
    (should (string-match-p
             "Darwin"
             (niwa-execution-target-readiness-message target))))

  :doc "reports the underlying probe failure"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status blocked :reason probe-failed :error "Host unavailable"))
    (should (string-match-p
             "Host unavailable"
             (niwa-execution-target-readiness-message target))))

  :doc "reports required sandbox failure and retry guidance"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target)
          '(:status blocked :reason sandbox-unavailable
            :sandbox-reason "User namespaces disabled"))
    (let ((message (niwa-execution-target-readiness-message target)))
      (should (string-match-p "User namespaces disabled" message))
      (should (string-match-p "niwa-retry-target-readiness" message))))

  :doc "reports ready targets compactly"
  (let ((target (niwa-execution-target-create
                 "/ssh:user@host:/srv/project/")))
    (setf (niwa-execution-target-readiness target) '(:status ready))
    (should (equal "ready"
                   (niwa-execution-target-readiness-message target)))))

(provide 'test-niwa-execution-target)

;;; test-niwa-execution-target.el ends here
