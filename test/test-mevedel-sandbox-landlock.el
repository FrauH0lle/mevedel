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
(require 'mevedel-sandbox)


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
  :doc "the broker, the display, writable roots outside shared temporary space and
write grants are allowed; /tmp, roots containing it and read grants are not"
  (let* ((runtime (file-truename (make-temp-file "mevedel-landlock-display-" t)))
         (display (file-name-concat runtime "wayland-test"))
         (server (test-mevedel-sandbox-landlock--server display))
         (process-environment (append (list (concat "XDG_RUNTIME_DIR=" runtime)
                                            "WAYLAND_DISPLAY=wayland-test")
                                      process-environment)))
    (unwind-protect
        (should (equal (list "/broker" display "/work/" "/write")
                       (mevedel-sandbox--socket-allow-list
                        '("/" "/tmp/" "/work/") "/tmp/"
                        '((:path "/read" :access read) (:path "/write" :access write))
                        '(:directory "/broker"))))
      (delete-process server)
      (delete-directory runtime t)))
  :doc "without a display or broker only the roots and write grants remain"
  (let ((process-environment (cons "WAYLAND_DISPLAY" process-environment))
        (temporary-file-directory "/tmp/"))
    (should (equal '("/work/")
                   (mevedel-sandbox--socket-allow-list '("/tmp/" "/work/") nil nil nil)))
    ;; Linux resolves a leading // as /, so //srv/tmp still lies in /srv.
    (should (equal '("/work/")
                   (mevedel-sandbox--socket-allow-list
                    '("/srv/" "/work/") "//srv/tmp/" nil nil))))
  :doc "real Bubblewrap: host sockets in shared /tmp and the system bus are
unreachable; workspace and write-granted sockets stay reachable"
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
            (should (equal (list own allowed)
                           (test-mevedel-sandbox-landlock--reached prepared))))
        (when prepared (mevedel-sandbox-cleanup prepared))
        (mapc #'delete-process servers)
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

(provide 'test-mevedel-sandbox-landlock)

;;; test-mevedel-sandbox-landlock.el ends here
