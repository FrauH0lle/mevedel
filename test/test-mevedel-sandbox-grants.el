;;; test-mevedel-sandbox-grants.el --- Tests for exact sandbox grants -*- lexical-binding: t -*-

;;; Commentary:

;; Tests exact filesystem grant resolution and Bubblewrap argument planning.

;;; Code:

(require 'mevedel-sandbox)
(require 'mevedel-sandbox-grants)
(require 'cl-lib)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))


;;
;;; Grant resolution

(mevedel-deftest mevedel-sandbox--symlink-chain ()
  ,test
  (test)
  :doc "ordered chain:
`mevedel-sandbox--symlink-chain' retains relative targets for every link hop"
  (let* ((root (make-temp-file "mevedel-sandbox-grant-chain-" t))
         (hidden (file-name-concat root "hidden"))
         (target (file-name-concat hidden "target"))
         (alias (file-name-concat hidden "alias"))
         (link (file-name-concat root "link")))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (make-directory hidden)
          (with-temp-file target)
          (make-symbolic-link "target" alias)
          (make-symbolic-link "hidden/alias" link)
          (let ((canonical-root (file-truename root)))
            (should
             (equal
              (last (mevedel-sandbox--symlink-chain link) 2)
              (list
               (cons (file-name-concat canonical-root "link")
                     "hidden/alias")
               (cons (file-name-concat canonical-root "hidden" "alias")
                     "target"))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--resolve-filesystem-permissions ()
  ,test
  (test)
  :doc "canonical grant:
`mevedel-sandbox--resolve-filesystem-permissions' retains source and link data"
  (let* ((root (make-temp-file "mevedel-sandbox-grant-resolve-" t))
         (target (file-name-concat root "target"))
         (link (file-name-concat root "link")))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (with-temp-file target)
          (make-symbolic-link "target" link)
          (let ((grant
                 (car
                  (mevedel-sandbox--resolve-filesystem-permissions
                   `((:path ,link :access read))))))
            (should (equal link (plist-get grant :source-path)))
            (should (file-equal-p target (plist-get grant :path)))
            (should
             (equal
              (list (cons (file-name-concat (file-truename root) "link")
                          "target"))
              (last (plist-get grant :symlinks))))
            (should (eq 'read (plist-get grant :access)))))
      (delete-directory root t)))
  :doc "protected children survive normalization; exact directory authority refuses before launch"
  (let* ((root (make-temp-file "mevedel-protected-overlap-" t))
         (git (file-name-concat root ".git"))
         (file (file-name-concat git "value"))
         (parent `(:path ,root :access write :recursive t))
         (mevedel-protected-paths '(("**/.git/**" . read-only))))
    (unwind-protect
        (progn
          (make-directory git)
          (with-temp-file file)
          (should-error
           (mevedel-sandbox--resolve-filesystem-permissions
            (list parent `(:path ,git :access write)))
           :type 'mevedel-sandbox-policy-error)
          (let ((resolved (mevedel-sandbox--resolve-filesystem-permissions
                           (list parent `(:path ,file :access write)))))
            (should (= 2 (length resolved)))
            (should (equal file (plist-get (cadr resolved) :path))))
          (should (= 1 (length (mevedel-sandbox--resolve-filesystem-permissions
                               (list `(:path ,git :access write :recursive t)
                                     `(:path ,file :access write)))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--grant-paths ()
  ,test
  (test)
  :doc "complete path set:
`mevedel-sandbox--grant-paths' returns source, intermediate, and target paths"
  (should
   (equal
    '("/source" "/middle" "/target")
    (mevedel-sandbox--grant-paths
     '(:source-path "/source"
       :symlinks (("/source" . "middle") ("/middle" . "target"))
       :path "/target")))))


;;
;;; Bubblewrap arguments

(mevedel-deftest mevedel-sandbox--additional-filesystem-mounts ()
  ,test
  (test)
  :doc "FD-backed mounts:
`mevedel-sandbox--additional-filesystem-mounts' assigns ordered exact mounts"
  (should
   (equal
    '("--ro-bind-fd" "20" "/target/a"
      "--bind-fd" "21" "/target/b")
    (mevedel-sandbox--additional-filesystem-mounts
     '((:path "/target/a" :source-path "/source/a" :access read)
       (:path "/target/b" :source-path "/source/b" :access write))
     20))))

(mevedel-deftest mevedel-sandbox--fd-backed-command ()
  ,test
  (test)
  :doc "no grants:
`mevedel-sandbox--fd-backed-command' leaves a command without paths unchanged"
  (let ((command '("printf" "ok")))
    (should (eq command (mevedel-sandbox--fd-backed-command command nil))))
  :doc "mask descriptors:
`mevedel-sandbox--fd-backed-command' opens empty input for file masks"
  (let ((wrapped (mevedel-sandbox--fd-backed-command '("printf" "ok") nil '(12 13))))
    (should (equal (executable-find "bash") (car wrapped)))
    (should (string-match-p "exec 12</dev/null || exit 125; exec 13</dev/null"
                            (nth 3 wrapped)))
    (should (equal '("printf" "ok") (last wrapped 2))))
  :doc "one grant:
`mevedel-sandbox--fd-backed-command' opens and verifies the target identity"
  (let* ((source (make-temp-file "mevedel-sandbox-fd-source-"))
         (grant (list :source-path source
                      :identity (mevedel-sandbox--target-identity source)))
         (wrapped
          (mevedel-sandbox--fd-backed-command
           '("printf" "ok") (list grant))))
    (unwind-protect
        (progn
          (should (equal (executable-find "bash") (car wrapped)))
          (should (string-match-p "exec 10<\"\\$1\"" (nth 3 wrapped)))
          (should (string-match-p "stat -Lc" (nth 3 wrapped)))
          (should (equal (list source
                               (mevedel-sandbox--grant-identity grant)
                               "printf" "ok")
                         (last wrapped 4))))
      (delete-file source))))

(mevedel-deftest mevedel-sandbox--fd-backed-command-race ()
  ,test
  (test)
  :doc "replacement before launch:
the target-side identity check rejects a replaced exact grant"
  (let* ((root (make-temp-file "mevedel-sandbox-fd-race-" t))
         (original (file-name-concat root "original"))
         (replacement (file-name-concat root "replacement"))
         (link (file-name-concat root "link"))
         (executed (file-name-concat root "executed"))
         (grant nil)
         (wrapped nil)
         (output-buffer (generate-new-buffer " *mevedel-sandbox-fd-race*")))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (with-temp-file original (insert "original"))
          (with-temp-file replacement (insert "replacement"))
          (make-symbolic-link "original" link)
          (setq grant (list :source-path link
                            :identity
                            (mevedel-sandbox--target-identity original))
                wrapped
                (mevedel-sandbox--fd-backed-command
                 `("sh" "-c"
                   ,(format
                     "test -z \"$MEVEDEL_SANDBOX_GRANT_FAILURE\" && : > %s"
                     (shell-quote-argument executed)))
                 (list grant)))
          (delete-file link)
          (make-symbolic-link "replacement" link)
          (should
           (= 1
              (apply #'call-process
                     (car wrapped) nil output-buffer nil (cdr wrapped)))))
          (should-not (file-exists-p executed))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--fd-backed-command-exec ()
  ,test
  (test)
  :doc "normal and symlink grants:
the verified descriptor is available to the child process"
  (let* ((root (make-temp-file "mevedel-sandbox-fd-exec-" t))
         (source (file-name-concat root "source"))
         (link (file-name-concat root "link"))
         (grant nil)
         (wrapped nil)
         (output-buffer (generate-new-buffer " *mevedel-sandbox-fd-exec*")))
    (skip-unless (not (eq system-type 'windows-nt)))
    (unwind-protect
        (progn
          (with-temp-file source (insert "granted"))
          (setq grant (list :source-path source
                            :identity
                            (mevedel-sandbox--target-identity source))
                wrapped
                (mevedel-sandbox--fd-backed-command
                 '("sh" "-c" "cat /proc/self/fd/10")
                 (list grant)))
          (should
           (= 0
              (apply #'call-process
                     (car wrapped) nil output-buffer nil (cdr wrapped))))
          (with-current-buffer output-buffer
            (should (equal "granted" (buffer-string))))
          (with-current-buffer output-buffer
            (erase-buffer))
          (make-symbolic-link "source" link)
          (setq grant (list :source-path link
                            :identity
                            (mevedel-sandbox--target-identity source))
                wrapped
                (mevedel-sandbox--fd-backed-command
                 '("sh" "-c" "cat /proc/self/fd/10")
                 (list grant)))
          (should
           (= 0
              (apply #'call-process
                     (car wrapped) nil output-buffer nil (cdr wrapped))))
          (with-current-buffer output-buffer
            (should (equal "granted" (buffer-string)))))
      (when (buffer-live-p output-buffer)
        (kill-buffer output-buffer))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--fd-backed-command-missing-stat ()
  ,test
  (test)
  :doc "missing target stat:
exact-grant preparation refuses when the target cannot inspect descriptors"
  (let* ((source (make-temp-file "mevedel-sandbox-fd-stat-"))
         (grant (list :source-path source
                      :identity (mevedel-sandbox--target-identity source))))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name &rest _arguments)
                     (unless (equal name "stat") "/bin/bash"))))
          (should-error
           (mevedel-sandbox--fd-backed-command
            '("true") (list grant))
           :type 'mevedel-sandbox-policy-error))
      (delete-file source))))

(mevedel-deftest mevedel-sandbox--mount-plan ()
  ,test
  (test)
  :doc "required state roots never silently skip a vanished mount source"
  (should (equal '("--ro-bind" "/w/.state" "/w/.state")
                 (plist-get (mevedel-sandbox--mount-plan
                             '((:path "/w/.state" :mode read-only :directory-p t :required t)) nil)
                            :arguments)))
  :doc "read-only restrictions:
`mevedel-sandbox--mount-plan' binds with the try variant unless the path has a write grant"
  (should
   (equal
    '(:arguments ("--ro-bind-try" "/w/other" "/w/other"
                  "--bind-fd" "10" "/w/.git")
      :grants ((:source-path "/w/.git" :path "/w/.git" :access write))
      :mask-fds nil)
    (mevedel-sandbox--mount-plan
     '((:path "/w/.git" :mode read-only :directory-p t)
       (:path "/w/other" :mode read-only :directory-p t))
     '((:source-path "/w/.git" :path "/w/.git" :access write)))))
  :doc "masked link target:
`mevedel-sandbox--mount-plan' drops the granted file mask, keeps the parent traversable, and numbers masks after grants"
  (let ((grant '(:source-path "/protected/link"
                 :symlinks (("/protected/link" . "bin/runner"))
                 :path "/protected/bin/runner" :access read)))
    (should
     (equal
      `(:arguments ("--perms" "0111" "--tmpfs" "/protected"
                    "--perms" "000" "--ro-bind-data" "11" "/other"
                    "--dir" "/protected/bin"
                    "--symlink" "bin/runner" "/protected/link"
                    "--ro-bind-fd" "10" "/protected/bin/runner"
                    "--remount-ro" "/protected")
        :grants (,grant)
        :mask-fds (11))
      (mevedel-sandbox--mount-plan
       '((:path "/protected" :mode inaccessible :directory-p t)
         (:path "/protected/link" :mode inaccessible :directory-p nil)
         (:path "/other" :mode inaccessible :directory-p nil))
       (list grant)))))
  :doc "granted ancestor tree:
`mevedel-sandbox--mount-plan' binds it first, keeps nested masks, and lifts only its own remount"
  (should
   (equal
    '(:arguments ("--bind-fd" "10" "/meta"
                  "--perms" "000" "--tmpfs" "/meta/child"
                  "--remount-ro" "/meta/child")
      :grants ((:source-path "/meta" :path "/meta" :access write :recursive t))
      :mask-fds nil)
    (mevedel-sandbox--mount-plan
     '((:path "/meta" :mode inaccessible :directory-p t)
       (:path "/meta/child" :mode inaccessible :directory-p t))
     '((:source-path "/meta" :path "/meta" :access write :recursive t)))))
  :doc "read grant on a masked directory:
`mevedel-sandbox--mount-plan' drops the mask but keeps the read-only remount"
  (should
   (equal
    '(:arguments ("--ro-bind-fd" "10" "/p" "--remount-ro" "/p")
      :grants ((:source-path "/p" :path "/p" :access read :recursive t))
      :mask-fds nil)
    (mevedel-sandbox--mount-plan
     '((:path "/p" :mode inaccessible :directory-p t))
     '((:source-path "/p" :path "/p" :access read :recursive t))))))

(provide 'test-mevedel-sandbox-grants)

;;; test-mevedel-sandbox-grants.el ends here
