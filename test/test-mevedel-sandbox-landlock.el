;;; test-mevedel-sandbox-landlock.el --- Host Unix socket restriction -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the Landlock launcher wiring in `mevedel-sandbox': the kernel probe
;; and its fallback, the socket allow-list, and real Bubblewrap round trips
;; against servers Emacs owns outside the confined domain.  The launcher's
;; own argument handling and abstract-socket scope are tested in
;; test/test_landlock.py.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-permission-rules)
(require 'mevedel-sandbox)
(require 'mevedel-tool-exec-permission)


;;
;;; Fixtures

(defconst test-mevedel-sandbox-landlock--connect
  (concat "import socket, sys\n"
          "for path in sys.argv[1:]:\n"
          "    try:\n"
          "        socket.socket(socket.AF_UNIX).connect(path)\n"
          "        print('REACH', path)\n"
          "    except OSError as error:\n"
          "        print('block', path, error.errno)\n")
  "Python program reporting which socket paths in its arguments accept a connection.")

(defun test-mevedel-sandbox-landlock--command (&rest paths)
  "Return a command reporting whether each socket in PATHS accepts a connection."
  (append (list (executable-find "python3") "-c" test-mevedel-sandbox-landlock--connect)
          paths))

(defun test-mevedel-sandbox-landlock--reached (prepared)
  "Run PREPARED; return the socket paths its command reached.
The command must have started and exited successfully."
  (with-temp-buffer
    (let ((command (plist-get prepared :command)))
      (ert-info ((buffer-string))
        (should (zerop (apply #'call-process (car command) nil t nil (cdr command)))))
      (goto-char (point-min))
      (let (reached)
        (while (re-search-forward "^REACH \\(.+\\)$" nil t)
          (push (match-string 1) reached))
        (nreverse reached)))))

(defun test-mevedel-sandbox-landlock--server (path)
  "Return a local socket server Emacs owns at PATH, outside every sandbox."
  (make-network-process :name "landlock-test" :family 'local :server t
                        :noquery t :service path))

(defmacro test-mevedel-sandbox-landlock--with-launcher (&rest body)
  "Run BODY with `availability' bound to a real probe that found the launcher.
Skip when Bubblewrap, python3 or Landlock socket restriction is missing."
  (declare (indent 0) (debug t))
  `(let* ((mevedel-sandbox--probe-cache nil)
          (availability (mevedel-sandbox-probe)))
     (unless (plist-get availability :available)
       (ert-skip (plist-get availability :reason)))
     (unless (plist-get (plist-get availability :landlock) :python)
       (ert-skip (plist-get (plist-get availability :landlock) :reason)))
     ,@body))


;;
;;; Probe

(mevedel-deftest mevedel-sandbox--landlock-probe ()
  ,test
  (test)
  :doc "the running kernel's verdict agrees with the launcher's own ABI query"
  (let ((python (executable-find "python3")))
    (skip-unless python)
    (let ((abi (with-temp-buffer
                 (call-process python nil t nil "-I" "-S"
                               mevedel-sandbox--landlock-script "abi")
                 (string-to-number (buffer-string))))
          (probe (mevedel-sandbox--landlock-probe)))
      (if (>= abi mevedel-sandbox--landlock-abi)
          (should (equal (list :python python :abi abi) probe))
        (should (stringp (plist-get probe :reason))))))
  :doc "an older ABI, disabled Landlock, unreadable output and a missing python3
each fall back with their reason"
  (let* ((bin (make-temp-file "mevedel-landlock-probe-" t))
         (python (file-name-concat bin "python3"))
         (exec-path (list bin)))
    (unwind-protect
        (pcase-dolist (`(,output . ,reason)
                       '(("8" . "Landlock ABI 8 lacks socket restriction (needs 9)")
                         ("0" . "Landlock is not enabled")
                         ("9 trailing" . "Landlock probe failed")))
          (with-temp-file python
            (insert "#!/bin/sh\necho '" output "'\n"))
          (set-file-modes python #o700)
          (should (equal (list :reason reason) (mevedel-sandbox--landlock-probe))))
      (delete-directory bin t))
    (should (equal '(:reason "'python3' is not installed")
                   (mevedel-sandbox--landlock-probe))))
  :doc "a remote target's facts record the local-only restriction"
  (let ((mevedel-sandbox--probe-cache nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/test/bwrap"))
              ((symbol-function 'mevedel-sandbox--run-probe)
               (lambda (&rest _) '(:status 0))))
      (should (equal '(:reason "remote execution target")
                     (plist-get (mevedel-sandbox-probe "/ssh:user@host:/srv/") :landlock))))))

(mevedel-deftest mevedel-sandbox--host-socket-facts ()
  ,test
  (test)
  :doc "a launcher restricts host sockets; otherwise the facts keep the reason"
  (should (equal '(:host-sockets allow-listed)
                 (mevedel-sandbox--host-socket-facts '(:python "/usr/bin/python3" :abi 9))))
  (should (equal '(:host-sockets reachable :host-sockets-reason "Landlock is not enabled")
                 (mevedel-sandbox--host-socket-facts '(:reason "Landlock is not enabled"))))
  (should (equal '(:host-sockets reachable :host-sockets-reason "Landlock was not probed")
                 (mevedel-sandbox--host-socket-facts nil))))


;;
;;; Allow-list

(mevedel-deftest mevedel-sandbox--socket-allow-list ()
  ,test
  (test)
  :doc "real Bubblewrap: host sockets in shared /tmp and the system bus are
unreachable, as are pre-existing workspace and directory-write-granted sockets"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((mevedel-sandbox-mode 'required)
           (process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (temporary (file-name-as-directory (file-truename temporary-file-directory)))
           (root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-landlock-shared-" t)))
           (granted (file-name-concat shared "granted"))
           (host (file-name-concat shared "host"))
           (own (file-name-concat root "own"))
           (allowed (file-name-concat granted "socket"))
           (bus "/run/dbus/system_bus_socket")
           (paths (append (list host own allowed)
                          (when (file-exists-p bus) (list bus))))
           servers prepared)
      (unwind-protect
          (progn
            (make-directory granted)
            (setq servers (mapcar #'test-mevedel-sandbox-landlock--server
                                  (list host own allowed)))
            ;; The production roster: shared temporary space is a writable root.
            (setq prepared
                  (mevedel-sandbox-prepare
                   (apply #'test-mevedel-sandbox-landlock--command paths)
                   root (list root temporary)
                   `(:file-system ((:path ,(file-name-as-directory granted)
                                    :access write :recursive t)))
                   nil 'required temporary))
            (should (eq 'allow-listed
                        (plist-get (plist-get prepared :facts) :host-sockets)))
            (should-not (test-mevedel-sandbox-landlock--reached prepared)))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (mapc #'delete-process servers)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap: an exact socket write grant permits only that socket,
even alongside a covering directory grant; a read grant permits no connection"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-landlock-exact-" t)))
           (allowed (file-name-concat shared "allowed"))
           (sibling (file-name-concat shared "sibling"))
           (servers (mapcar #'test-mevedel-sandbox-landlock--server
                            (list allowed sibling)))
           prepared)
      (unwind-protect
          (pcase-dolist (`(,access ,authorized) '((read nil) (write nil) (write t)))
            (setq prepared
                  (mevedel-sandbox-prepare
                   (test-mevedel-sandbox-landlock--command allowed sibling)
                   root (list root)
                   `(:file-system ((:path ,shared :access write :recursive t)
                                   (:path ,allowed :access ,access
                                    :socket-authorized ,authorized)))
                   nil 'required temporary-file-directory))
            (should (eq 'confined (plist-get prepared :state)))
            (should (equal (when authorized (list allowed))
                           (test-mevedel-sandbox-landlock--reached prepared)))
            (mevedel-sandbox-cleanup prepared)
            (setq prepared nil))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (mapc #'delete-process servers)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap: permission-derived directory coverage never becomes socket authority"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (root (file-truename (make-temp-file "mevedel-admission-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-admission-socket-" t)))
           (path (file-name-concat shared "file"))
           (context `(:resource-grants ((:path ,shared :access write :recursive t))
                      :buckets ((:session))))
           (args `(:sandbox_permissions "with_additional_permissions"
                   :additional_permissions (:file_system (:write [,path]))
                   :justification "Access selected path"))
           server prepared)
      (unwind-protect
          (progn
            (write-region "file" nil path nil 'silent)
            (let ((before (mevedel-tool-exec-permission-effective-sandbox-request
                           args "Bash" "true" nil context)))
              (delete-file path)
              (setq server (test-mevedel-sandbox-landlock--server path))
              (pcase-dolist
                  (`(,authorized ,request)
                   (list (list nil before)
                         (list nil (mevedel-tool-exec-permission-effective-sandbox-request
                                    args "Bash" "true" nil context))
                         (list t (let ((mevedel-tool-exec-permission-approved-resources
                                        `((:path ,path :access write))))
                                   (mevedel-tool-exec-permission-effective-sandbox-request
                                    args "Bash" "true" nil context)))))
                (setq prepared
                      (mevedel-sandbox-prepare
                       (test-mevedel-sandbox-landlock--command path)
                       root (list root) (plist-get request :additional-permissions)
                       nil 'required temporary-file-directory))
                (should (eq 'confined (plist-get prepared :state)))
                (should (equal (and authorized (list path))
                               (test-mevedel-sandbox-landlock--reached prepared)))
                (mevedel-sandbox-cleanup prepared)
                (setq prepared nil))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (when server (delete-process server))
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap: replacing an approved socket after preparation refuses
launch before the child starts"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-landlock-replace-" t)))
           (socket (file-name-concat shared "socket"))
           (ran (file-name-concat root "ran"))
           (server (test-mevedel-sandbox-landlock--server socket))
           replacement prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (mevedel-sandbox-prepare
                   (list "touch" ran) root (list root)
                   `(:file-system ((:path ,socket :access write :socket-authorized t)))
                   nil 'required temporary-file-directory))
            (should (eq 'confined (plist-get prepared :state)))
            ;; Keep the approved inode alive so it cannot be reused by bind.
            (rename-file socket (file-name-concat shared "original"))
            (setq replacement (test-mevedel-sandbox-landlock--server socket))
            (with-temp-buffer
              (let* ((command (plist-get prepared :command))
                     (status (apply #'call-process (car command) nil t nil (cdr command))))
                (ert-info ((buffer-string))
                  (should (eql 125 status))
                  (should-not (file-exists-p ran))
                  (should-not (member (plist-get prepared :marker)
                                      (split-string (buffer-string) "\n" nil)))))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (when replacement (delete-process replacement))
        (delete-process server)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap: a write grant on shared temporary space does not reopen
its host sockets"
  (test-mevedel-sandbox-landlock--with-launcher
    ;; A test runner may keep XDG_RUNTIME_DIR inside temporary space, and a
    ;; grant over a masked runtime directory is refused.
    (let* ((process-environment (append '("WAYLAND_DISPLAY" "XDG_RUNTIME_DIR")
                                        process-environment))
           (temporary (file-name-as-directory (file-truename temporary-file-directory)))
           (root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-landlock-shared-" t)))
           (host (file-name-concat shared "host"))
           (server (test-mevedel-sandbox-landlock--server host))
           prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (mevedel-sandbox-prepare
                   (test-mevedel-sandbox-landlock--command host)
                   root (list root)
                   `(:file-system ((:path ,temporary :access write :recursive t)))
                   nil 'required temporary))
            ;; The grant reached the launch; only the allow-list leaves it out.
            (ert-info ((format "%S" (list (plist-get prepared :state)
                                          (plist-get prepared :error))))
              (should (eql 1 (plist-get (plist-get prepared :facts)
                                        :additional-filesystem-write))))
            (should-not (test-mevedel-sandbox-landlock--reached prepared)))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (delete-process server)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap fallback and negative control: without the launcher the
same command reaches the host sockets, and the facts and status say so"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
           (shared (file-truename (make-temp-file "mevedel-landlock-shared-" t)))
           (host (file-name-concat shared "host"))
           (bus "/run/dbus/system_bus_socket")
           (paths (cons host (when (file-exists-p bus) (list bus))))
           (server (test-mevedel-sandbox-landlock--server host))
           prepared)
      (unwind-protect
          (progn
            (setq prepared
                  (mevedel-sandbox--confined-preparation
                   (apply #'test-mevedel-sandbox-landlock--command paths)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil nil nil
                   '(:reason "Landlock ABI 8 lacks socket restriction (needs 9)")))
            (should-not (member mevedel-sandbox--landlock-script
                                (plist-get prepared :command)))
            (should (string-suffix-p
                     "; host sockets: reachable (Landlock ABI 8 lacks socket restriction (needs 9))"
                     (mevedel-sandbox-status-text (plist-get prepared :facts))))
            (should (equal paths (test-mevedel-sandbox-landlock--reached prepared))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (delete-process server)
        (delete-directory shared t)
        (delete-directory root t))))
  :doc "real Bubblewrap: the Wayland socket bound back into the masked runtime
directory stays reachable"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((runtime (file-truename (make-temp-file "mevedel-landlock-xdg-" t)))
           (root (file-name-concat runtime "work"))
           (display (file-name-concat runtime "wayland-test"))
           (process-environment
            (append (list (concat "XDG_RUNTIME_DIR=" runtime)
                          "WAYLAND_DISPLAY=wayland-test")
                    process-environment))
           server prepared)
      (unwind-protect
          (progn
            (make-directory root)
            (setq server (test-mevedel-sandbox-landlock--server display))
            (setq prepared
                  (mevedel-sandbox--confined-preparation
                   (test-mevedel-sandbox-landlock--command display)
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil nil nil
                   (plist-get availability :landlock)))
            (should (equal (list display)
                           (test-mevedel-sandbox-landlock--reached prepared))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (when server (delete-process server))
        (delete-directory runtime t))))
  :doc "real Bubblewrap: a launcher that cannot restrict fails the launch before
the command starts"
  (test-mevedel-sandbox-landlock--with-launcher
    (let ((root (file-truename (make-temp-file "mevedel-landlock-root-" t)))
          prepared)
      (unwind-protect
          (with-temp-buffer
            (setq prepared
                  (mevedel-sandbox--confined-preparation
                   (list "touch" (file-name-concat root "ran"))
                   root (list root) (plist-get availability :executable)
                   (plist-get availability :mount-proc) nil nil nil
                   (list :python (executable-find "false"))))
            (let* ((command (plist-get prepared :command))
                   (status (apply #'call-process (car command) nil t nil (cdr command))))
              (should (mevedel-sandbox-launch-failed-p
                       prepared (list :exit-code status :output (buffer-string))))
              (should-not (file-exists-p (file-name-concat root "ran")))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (delete-directory root t)))))


;;
;;; Workspace file operations

(mevedel-deftest mevedel-sandbox-prepare--file-operations ()
  ,test
  (test)
  :doc "real Bubblewrap: workspace rename, replacement and hard links work across
a former directory-grant boundary while protected and outside paths stay read-only"
  (test-mevedel-sandbox-landlock--with-launcher
    (let* ((parent (file-truename (make-temp-file "mevedel-landlock-files-" t)))
           (root (file-name-concat parent "work"))
           (source (file-name-concat root "source"))
           (target (file-name-concat root "target"))
           (former-grant (file-name-concat root "former-grant"))
           (protected (file-name-concat root ".git"))
           (outside (file-name-concat parent "outside"))
           (mevedel-protected-paths '(("**/.git/**" . read-only)))
           (process-environment (cons "WAYLAND_DISPLAY" process-environment))
           (program
            (concat
             "import errno, os, sys\n"
             "from pathlib import Path\n"
             "source, target, protected, outside, former_grant = map(Path, sys.argv[1:])\n"
             "for directory in (protected, outside):\n"
             "    for name in ('kept', 'new'):\n"
             "        try:\n"
             "            (directory / name).write_bytes(b'changed\\n')\n"
             "        except OSError as error:\n"
             "            assert error.errno in (errno.EROFS, errno.EACCES, errno.EPERM), error\n"
             "        else:\n"
             "            raise AssertionError('write escaped confinement: ' + str(directory / name))\n"
             "os.rename(source / 'rename', target / 'renamed')\n"
             "os.replace(source / 'replace', target / 'replace')\n"
             "os.link(source / 'link', target / 'linked')\n"
             "os.rename(former_grant / 'rename', target / 'grant-renamed')\n"
             "os.replace(former_grant / 'replace', target / 'grant-replace')\n"
             "os.link(former_grant / 'link', target / 'grant-linked')\n"))
           prepared)
      (unwind-protect
          (progn
            ;; Keep this boundary within one mount: an extra directory bind
            ;; would make cross-boundary operations fail with EXDEV, not policy.
            (dolist (directory (list source target protected outside former-grant))
              (make-directory directory t))
            (dolist (entry `((,source "rename" "rename\n")
                             (,source "replace" "replacement\n")
                             (,source "link" "linked\n")
                             (,target "replace" "old\n")
                             (,former-grant "rename" "grant-rename\n")
                             (,former-grant "replace" "grant-replacement\n")
                             (,former-grant "link" "grant-linked\n")
                             (,target "grant-replace" "old\n")
                             (,protected "kept" "protected\n")
                             (,outside "kept" "outside\n")))
              (with-temp-file (file-name-concat (nth 0 entry) (nth 1 entry))
                (insert (nth 2 entry))))
            (setq prepared
                  (mevedel-sandbox-prepare
                   (list (executable-find "python3") "-I" "-S" "-c" program
                         source target protected outside former-grant)
                   root (list root) nil nil 'required temporary-file-directory))
            (ert-info ((format "%S" prepared))
              (should (eq 'confined (plist-get prepared :state))))
            (let ((facts (plist-get prepared :facts)))
              (should (eq 'bubblewrap (plist-get facts :sandbox)))
              (should (eq 'workspace-write (plist-get facts :filesystem)))
              (should (eq 'isolated (plist-get facts :network)))
              (should (eq 'allow-listed (plist-get facts :host-sockets)))
              (should (= 1 (plist-get facts :protected-paths))))
            (should (member mevedel-sandbox--landlock-script
                            (plist-get prepared :command)))
            (with-temp-buffer
              (let* ((command (plist-get prepared :command))
                     (status (apply #'call-process (car command) nil t nil (cdr command))))
                (ert-info ((buffer-string))
                  (should (eql 0 status))
                  (should (member (plist-get prepared :marker)
                                  (split-string (buffer-string) "\n" nil))))))
            (dolist (entry `((,target "renamed" "rename\n")
                             (,target "replace" "replacement\n")
                             (,source "link" "linked\n")
                             (,target "linked" "linked\n")
                             (,target "grant-renamed" "grant-rename\n")
                             (,target "grant-replace" "grant-replacement\n")
                             (,former-grant "link" "grant-linked\n")
                             (,target "grant-linked" "grant-linked\n")
                             (,protected "kept" "protected\n")
                             (,outside "kept" "outside\n")))
              (with-temp-buffer
                (insert-file-contents-literally
                 (file-name-concat (nth 0 entry) (nth 1 entry)))
                (should (equal (nth 2 entry) (buffer-string)))))
            (dolist (path (list (file-name-concat source "rename")
                                (file-name-concat source "replace")
                                (file-name-concat former-grant "rename")
                                (file-name-concat former-grant "replace")
                                (file-name-concat protected "new")
                                (file-name-concat outside "new")))
              (should-not (file-exists-p path)))
            (should (file-equal-p (file-name-concat source "link")
                                 (file-name-concat target "linked")))
            (should (file-equal-p (file-name-concat former-grant "link")
                                 (file-name-concat target "grant-linked"))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (delete-directory parent t)))))

(provide 'test-mevedel-sandbox-landlock)

;;; test-mevedel-sandbox-landlock.el ends here
