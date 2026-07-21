;;; test-mevedel-sandbox-grants.el --- Tests for exact sandbox grants -*- lexical-binding: t -*-

;;; Commentary:

;; Tests exact filesystem grant resolution and Bubblewrap argument planning.

;;; Code:

(require 'mevedel-sandbox)
(require 'mevedel-sandbox-grants)
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
    (unwind-protect
        (progn
          (make-directory hidden)
          (with-temp-file target)
          (make-symbolic-link "target" alias)
          (make-symbolic-link "hidden/alias" link)
          (should
           (equal (mevedel-sandbox--symlink-chain link)
                  (list (cons link "hidden/alias")
                        (cons alias "target")))))
      (delete-directory root t))))

(mevedel-deftest mevedel-sandbox--resolve-filesystem-permissions ()
  ,test
  (test)
  :doc "canonical grant:
`mevedel-sandbox--resolve-filesystem-permissions' retains source and link data"
  (let* ((root (make-temp-file "mevedel-sandbox-grant-resolve-" t))
         (target (file-name-concat root "target"))
         (link (file-name-concat root "link")))
    (unwind-protect
        (progn
          (with-temp-file target)
          (make-symbolic-link "target" link)
          (let ((grant
                 (car
                  (mevedel-sandbox--resolve-filesystem-permissions
                   `((:path ,link :access read))))))
            (should (equal link (plist-get grant :source-path)))
            (should (equal target (plist-get grant :path)))
            (should (equal (list (cons link "target"))
                           (plist-get grant :symlinks)))
            (should (eq 'read (plist-get grant :access)))))
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
    '(:arguments
      ("--ro-bind-fd" "20" "/target/a"
       "--bind-fd" "21" "/target/b")
      :paths ("/source/a" "/source/b"))
    (mevedel-sandbox--additional-filesystem-mounts
     '(:file-system
       ((:path "/target/a" :source-path "/source/a" :access read)
        (:path "/target/b" :source-path "/source/b" :access write)))
     20))))

(mevedel-deftest mevedel-sandbox--fd-backed-command ()
  ,test
  (test)
  :doc "no grants:
`mevedel-sandbox--fd-backed-command' leaves a command without paths unchanged"
  (let ((command '("printf" "ok")))
    (should (eq command (mevedel-sandbox--fd-backed-command command nil))))
  :doc "one grant:
`mevedel-sandbox--fd-backed-command' opens the path before executing the command"
  (let ((wrapped
         (mevedel-sandbox--fd-backed-command
          '("printf" "ok") '("/source"))))
    (should (equal (executable-find "bash") (car wrapped)))
    (should (string-match-p "exec 10<\"\\$1\"" (nth 3 wrapped)))
    (should (equal '("/source" "printf" "ok") (last wrapped 3)))))

(mevedel-deftest mevedel-sandbox--open-granted-paths ()
  ,test
  (test)
  :doc "protected exact path:
`mevedel-sandbox--open-granted-paths' removes only granted file masks"
  (should
   (equal
    '("--ro-bind" "/dev/null" "/other" "--chmod" "000" "/other"
      "--perms" "0111" "--tmpfs" "/protected")
    (mevedel-sandbox--open-granted-paths
     '("--ro-bind" "/dev/null" "/protected/link"
       "--chmod" "000" "/protected/link"
       "--ro-bind" "/dev/null" "/other" "--chmod" "000" "/other"
       "--perms" "000" "--tmpfs" "/protected")
     '(:file-system
       ((:source-path "/protected/link"
         :symlinks (("/protected/link" . "bin/runner"))
         :path "/protected/bin/runner" :access read)))))))

(mevedel-deftest mevedel-sandbox--granted-path-mounts ()
  ,test
  (test)
  :doc "masked link target:
`mevedel-sandbox--granted-path-mounts' recreates only required paths and links"
  (should
   (equal
    '("--dir" "/protected/bin"
      "--symlink" "bin/runner" "/protected/alias")
    (mevedel-sandbox--granted-path-mounts
     '("--perms" "0111" "--tmpfs" "/protected")
     '(:file-system
       ((:source-path "/outside/link"
         :symlinks (("/outside/link" . "/protected/alias")
                    ("/protected/alias" . "bin/runner"))
         :path "/protected/bin/runner" :access read)))))))

(mevedel-deftest mevedel-sandbox--protected-remounts ()
  ,test
  (test)
  :doc "writable exact path:
`mevedel-sandbox--protected-remounts' omits only a granted writable remount"
  (should
   (equal
    '("--remount-ro" "/other")
    (mevedel-sandbox--protected-remounts
     '("--remount-ro" "/protected" "--remount-ro" "/other")
     '(:file-system
       ((:source-path "/source" :path "/protected" :access write)))))))

(provide 'test-mevedel-sandbox-grants)

;;; test-mevedel-sandbox-grants.el ends here
