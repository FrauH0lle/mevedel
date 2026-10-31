;;; test-mevedel-graphical-check.el --- Graphical runner contracts -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise launch mechanics in batch.  The graphical-check command itself
;; covers its real frame preflight, view capture and graphical ERT execution.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'graphical-check
         (file-name-concat
          (file-name-directory
           (or load-file-name buffer-file-name byte-compile-current-file))
          "graphical-check"))

(mevedel-deftest mevedel-graphical-check--options ()
  ,test
  (test)
  :doc "accepts explicit output, repeated grammar roots and a positive deadline"
  (let ((directory (make-temp-file "graphical-options-" t)))
    (unwind-protect
        (should (equal (list (expand-file-name "capture")
                            (list directory directory) 3)
                       (mevedel-graphical-check--options
                        (list "--" "--output" "capture" "--grammar-dir" directory
                              "--grammar-dir" directory "--timeout" "3"))))
      (delete-directory directory t)))
  :doc "rejects unknown options, missing arguments and unsafe deadlines"
  (dolist (args '(("--unknown" "x") ("--output") ("--timeout" "0")
                  ("--timeout" "-1") ("--timeout" "5oops")
                  ("--grammar-dir" "/does-not-exist/graphical-grammar")))
    (should-error (mevedel-graphical-check--options args))))

(mevedel-deftest mevedel-graphical-check--environment ()
  ,test
  (test)
  :doc "privatizes HOME/XDG while preserving the absolute compositor socket"
  (let* ((temporary (make-temp-file "graphical-environment-" t))
         (home (file-name-concat temporary "home"))
         (socket (file-name-concat temporary "wayland-test"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (make-directory home)
          (with-temp-file socket (insert "socket placeholder"))
          (setenv "WAYLAND_DISPLAY" "wayland-test")
          (setenv "XDG_RUNTIME_DIR" temporary)
          (setenv "DBUS_SESSION_BUS_ADDRESS" "do-not-use-host-bus")
          (let ((environment (mevedel-graphical-check--environment home)))
            (should (equal "wayland-test" (getenv "WAYLAND_DISPLAY")))
            (let ((process-environment environment))
              (should (equal home (getenv "HOME")))
              (should (equal socket (getenv "WAYLAND_DISPLAY")))
              (should-not (getenv "DBUS_SESSION_BUS_ADDRESS"))
              (dolist (name '("XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME"
                              "XDG_STATE_HOME" "XDG_RUNTIME_DIR"))
                (should (file-in-directory-p (getenv name) home))
                (should (= #o700 (file-modes (getenv name))))))))
      (delete-directory temporary t)))
  :doc "fails clearly without a display or with an unreachable Wayland socket"
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "DISPLAY" nil)
    (setenv "WAYLAND_DISPLAY" nil)
    (should (string-match-p
             "No graphical display"
             (error-message-string
              (should-error (mevedel-graphical-check--environment "/unused")))))
    (setenv "WAYLAND_DISPLAY" "/does-not-exist/wayland-test")
    (should (string-match-p
             "Wayland socket is unavailable"
             (error-message-string
              (should-error (mevedel-graphical-check--environment "/unused")))))))

(mevedel-deftest mevedel-graphical-check--wait
  (:doc "preserves success, failure and a bounded timeout from real subprocesses")
  (dolist (case '(((kill-emacs 0) nil 10)
                  ((kill-emacs 7) "Graphical Emacs failed (7)" 10)
                  ((sleep-for 10) "timed out" 0.05)))
    (with-temp-buffer
      (let ((process (make-process
                      :name "graphical-wait-test" :noquery t
                      :buffer (current-buffer) :connection-type 'pipe
                      :sentinel #'ignore
                      :command (list (expand-file-name invocation-name invocation-directory)
                                     "-Q" "--batch" "--eval"
                                     (prin1-to-string
                                      `(progn
                                         (princ "ready\n" #'external-debugging-output)
                                         ,(car case)))))))
        (unwind-protect
            (progn
              ;; Startup is not the timeout under test.  Only use the short
              ;; deadline after the child has entered its scenario.
              (mevedel-test--await 10 "wait fixture readiness"
                (string-search "ready\n" (buffer-string)))
              (let ((start (float-time)))
                (if (cadr case)
                    (should (string-match-p
                             (regexp-quote (cadr case))
                             (error-message-string
                              (should-error
                               (mevedel-graphical-check--wait process (nth 2 case))))))
                  (should-not (mevedel-graphical-check--wait process (nth 2 case))))
                (when (equal (cadr case) "timed out")
                  (should (>= (- (float-time) start) (nth 2 case)))))
              (should-not (process-live-p process)))
          (when (process-live-p process) (delete-process process)))))))

(defconst test-mevedel-graphical-check--orphan-owner
  "import ctypes, os, signal, subprocess, sys, time
# Adopt orphans, so the stand-in child's fate after its parent exits is
# observable here rather than racing init or systemd.  A dead child is a
# zombie (Z) or gone; a child that survived its parent is still running.
ctypes.CDLL(None).prctl(36, 1, 0, 0, 0)
pid_file, tmp = sys.argv[1], sys.argv[2]
parent = subprocess.Popen(sys.argv[3:], stdin=subprocess.DEVNULL,
                          stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
child = None
try:
    deadline = time.monotonic() + 20
    while child is None and time.monotonic() < deadline:
        try:
            child = int(open(pid_file).read())
        except (OSError, ValueError):
            time.sleep(0.02)
    print('before', sum(n.startswith('mevedel-graphical-') for n in os.listdir(tmp)))
    parent.send_signal(signal.SIGTERM)
    parent.wait(20)
    state = 'never started'
    if child is not None:
        # SIGKILL was sent before the parent exited; allow only for the
        # kernel to finish killing it, never for a survivor to stop itself.
        deadline = time.monotonic() + 5
        while True:
            try:
                stat = open(f'/proc/{child}/stat').read()
                state = 'running ' + stat[stat.rindex(')') + 2]
            except OSError:
                state = 'dead'
            if state in ('dead', 'running Z', 'running X') \\
               or time.monotonic() > deadline:
                break
            time.sleep(0.02)
        if state in ('running Z', 'running X'):
            state = 'dead'
    print('child', state)
finally:
    parent.kill()
    if child:
        try:
            os.kill(child, signal.SIGKILL)
        except ProcessLookupError:
            pass
    deadline = time.monotonic() + 5
    while time.monotonic() < deadline:
        try:
            if os.waitpid(-1, os.WNOHANG)[0] == 0:
                time.sleep(0.02)
        except ChildProcessError:
            break
"
  "Run argv[3:] as a child subreaper; report the stand-in child after SIGTERM.")

(mevedel-deftest mevedel-graphical-check-run
  (:doc "reaps its child and removes private state when the batch parent gets SIGTERM")
  (let* ((root (make-temp-file "graphical-sigterm-" t))
         (tmp (file-name-concat root "tmp"))
         (bin (file-name-concat root "bin"))
         (pid-file (file-name-concat root "child.pid"))
         (process-environment (copy-sequence process-environment)))
    (unwind-protect
        (progn
          (skip-unless (and (eq system-type 'gnu/linux) (executable-find "python3")))
          (make-directory tmp)
          (make-directory bin)
          ;; Stand-in for a hung graphical child: record its PID, then hang,
          ;; ignoring the SIGHUP batch Emacs sends its processes on exit.
          (with-temp-file (file-name-concat bin "emacs")
            (insert (format "#!/bin/sh\ntrap '' HUP\necho $$ > %s\nexec sleep 300\n"
                            (shell-quote-argument pid-file))))
          (set-file-modes (file-name-concat bin "emacs") #o755)
          (setenv "TMPDIR" tmp)
          (setenv "DISPLAY" ":graphical-sigterm-test")
          (setenv "XAUTHORITY" "/dev/null")
          (setenv "WAYLAND_DISPLAY" nil)
          (should
           (equal
            ;; Private state existed, and the child died with its parent.
            "before 1\nchild dead\n"
            (with-temp-buffer
              (call-process
               "python3" nil t nil "-I" "-c" test-mevedel-graphical-check--orphan-owner
               pid-file tmp
               (expand-file-name invocation-name invocation-directory)
               "-Q" "--batch" "-l"
               (file-name-concat mevedel-graphical-check--root "test" "graphical-check.el")
               "--eval" (format "(setq invocation-directory %S)"
                                (file-name-as-directory bin))
               "--funcall" "mevedel-graphical-check-run" "--"
               "--output" (file-name-concat root "out") "--timeout" "60")
              (buffer-string))))
          (should-not (directory-files tmp nil "\\`mevedel-graphical-")))
      (delete-directory root t))))

(mevedel-deftest mevedel-graphical-check--preflight
  (:doc "does not let a batch frame masquerade as graphical coverage")
  (when noninteractive
    (should (string-match-p
             "real graphical Emacs"
             (error-message-string
              (should-error (mevedel-graphical-check--preflight)))))))

(mevedel-deftest mevedel-graphical-check--child
  (:doc "reports skip reasons and rejects incomplete graphical coverage")
  (let* ((output (make-temp-file "graphical-skipped-" t))
         (test (make-ert-test :name 'graphical-environment-refused
                             :body (lambda () (ert-skip "Compositor refused control resize"))))
         (stats (ert-run-tests (list 'member test) #'ignore))
         (mevedel-graphical-check--files nil)
         status messages)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-graphical-check--preflight) #'ignore)
                    ((symbol-function 'mevedel-graphical-check--capture) #'ignore)
                    ((symbol-function 'set-frame-size) #'ignore)
                    ((symbol-function 'sit-for) #'ignore)
                    ((symbol-function 'ert-run-tests-batch) (lambda (_) stats))
                    ((symbol-function 'kill-emacs) (lambda (code) (setq status code))))
            (mevedel-test--with-captured-messages messages
              (mevedel-graphical-check--child output)))
          (should (= 1 status))
          (should (string-search "Compositor refused control resize" messages))
          (let ((summary (json-read-file (file-name-concat output "summary.json"))))
            (should (= 1 (alist-get 'tests summary)))
            (should (= 1 (alist-get 'skipped summary)))
            (should (= 0 (alist-get 'unexpected summary)))))
      (delete-directory output t))))

(provide 'test-mevedel-graphical-check)
;;; test-mevedel-graphical-check.el ends here
