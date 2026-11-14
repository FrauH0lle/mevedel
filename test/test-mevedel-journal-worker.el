;;; test-mevedel-journal-worker.el --- Independent maintenance tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise real child Emacs processes and on-disk claims without model access.

;;; Code:
(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-worker)
(require 'mevedel-journal-recovery)
(require 'mevedel-journal-cleanup)
(require 'mevedel-memory-pass)

(mevedel-deftest mevedel-journal-worker-local-p ()
  ,test
  (test)
  :doc "only local Linux owners dispatch children"
  (let ((workspace (mevedel-workspace--create :root "/tmp/")))
    (let ((system-type 'gnu/linux))
      (should (mevedel-journal-worker-local-p workspace))
      (should (mevedel-journal-worker-local-p workspace '("/tmp/one" "/tmp/two")))
      (should-not (mevedel-journal-worker-local-p workspace '("/tmp/one" "/ssh:host:/tmp/two")))
      (let ((mevedel-journal-worker--child-p t))
        (should-not (mevedel-journal-worker-local-p workspace)))
      (setf (mevedel-workspace-root workspace) "/ssh:host:/tmp/")
      (should-not (mevedel-journal-worker-local-p workspace)))))

(mevedel-deftest mevedel-journal-worker-start ()
  ,test
  (test)
  :doc "recovery runs independently and cannot seal a live owned source"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (kill-emacs-hook kill-emacs-hook)
            process timer result tick)
       (unwind-protect
           (progn
             (setq process (mevedel-journal-worker-start workspace 'recovery nil (lambda (value) (setq result value)))
                   timer (run-at-time 0 nil (lambda () (setq tick t))))
             (let ((until (+ (float-time) 15)))
               (while (and (not result) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should tick)
             (should (plist-get result :ok))
             (should-not (mevedel-journal-capture-trigger workspace capture))
             (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
             (should-not (memq process mevedel-journal-worker--processes)))
         (when timer (cancel-timer timer))
         (when (and process (process-live-p process)) (delete-process process)))))))

(mevedel-deftest mevedel-journal-worker--read ()
  ,test
  (test)
  :doc "round-trips text holding NUL bytes beside non-ASCII characters"
  ;; Coding detection would read such a file as raw bytes, leaving every
  ;; non-ASCII string unibyte and unserializable as JSON.
  (let* ((file (make-temp-file "mevedel-journal-worker-" nil ".el"))
         (evidence (concat "Mevedel\u2019s header\n\\x89PNG\r\n"
                           (string ?\C-z ?\n 0 0) "\u2014 done"))
         (value (list :prepared (list :capture (list :evidence evidence)))))
    (unwind-protect
        (progn
          (mevedel-journal-worker--write file value)
          (let ((read (plist-get (plist-get (plist-get (mevedel-journal-worker--read file)
                                                       :prepared)
                                            :capture)
                                 :evidence)))
            (should (equal evidence read))
            (should (multibyte-string-p read))
            (should (json-serialize (list :evidence read)))))
      (delete-file file))))

(mevedel-deftest mevedel-journal-worker--run ()
  ,test
  (test)
  :doc "cleanup runs with serialized retention policy and preserves fresh evidence"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (mevedel-session-workspace session))
            (kill-emacs-hook kill-emacs-hook)
            process result)
       (unwind-protect
           (progn
             (setq process (mevedel-journal-worker-start workspace 'cleanup (list :force t) (lambda (value) (setq result value))))
             (let ((until (+ (float-time) 15)))
               (while (and (not result) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should (plist-get result :ok))
             (should (numberp (plist-get result :cleanup-at)))
             (should (= 0 (plist-get result :count)))
             (should (mevedel-journal-capture-list workspace))
             (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (when (and process (process-live-p process)) (delete-process process)))))))

(mevedel-deftest mevedel-journal-worker-stop-all ()
  ,test
  (test)
  :doc "cancellation removes child ownership and its private handoff files"
  (let* ((root (make-temp-file "mevedel-worker-stop-" t))
         (workspace (mevedel-workspace--create :root root))
         (kill-emacs-hook kill-emacs-hook)
         (mevedel-journal-worker--processes nil)
         (mevedel-journal-process--inhibit-scheduling nil)
         (mevedel-journal-cleanup--inhibit-scheduling nil)
         (mevedel-memory-pass--inhibit-scheduling nil)
         process result directory)
    (unwind-protect
        (progn
          (setq process
                (mevedel-journal-worker-start
                 workspace 'recovery
                 nil (lambda (value) (setq result value))))
          (setq directory (file-name-directory
                           (nth 1 (read (car (last (process-command process)))))))
          (should (file-directory-p directory))
          (mevedel-journal-worker-stop-all)
          (accept-process-output nil .01)
          (should-not (file-exists-p directory))
          (should-not (process-live-p process))
          (should-not mevedel-journal-worker--processes)
          (should-not result))
      (when (and process (process-live-p process)) (delete-process process))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-cleanup-schedule/worker ()
  ,test
  (test)
  :doc "local scheduled cleanup coalesces and returns to input before worker completion"
  (let* ((root (make-temp-file "mevedel-cleanup-worker-" t))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-journal-cleanup--pending (make-hash-table :test #'equal))
         (mevedel-journal-cleanup--inhibit-scheduling nil)
         (kill-emacs-hook kill-emacs-hook)
         (mevedel-journal-worker--processes nil)
         first)
    (unwind-protect
        (progn
          (make-directory (file-name-concat root ".mevedel") t)
          (mevedel-journal-cleanup-schedule workspace)
          (setq first (car (gethash root mevedel-journal-cleanup--pending)))
          (mevedel-journal-cleanup-schedule workspace)
          (should (eq first (car (gethash root mevedel-journal-cleanup--pending))))
          (cancel-timer first)
          (apply (timer--function first) (timer--args first))
          (should mevedel-journal-worker--processes)
          (mevedel-test--with-captured-messages nil
            (mevedel-journal-cleanup-wait workspace))
          (should-not (gethash root mevedel-journal-cleanup--pending))
          (should-not mevedel-journal-worker--processes)
          (should (numberp (mevedel-workspace-journal-cleanup-at workspace))))
      (when first (cancel-timer first))
      (let ((mevedel-journal-cleanup--inhibit-scheduling t)) (mevedel-journal-worker-stop-all))
      (delete-directory root t))))

(mevedel-deftest mevedel-journal-worker--stop ()
  (let* ((root (make-temp-file "mevedel-worker-descendants-" t))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (directory (mevedel-journal-store-claim-directory
                     (mevedel-journal-store-directory root) 'digest-run))
         (marker (file-name-concat root "paused"))
         (release (file-name-concat root "continue"))
         (kill-emacs-hook nil)
         (mevedel-journal-enabled t)
         (mevedel-journal-worker--processes nil)
         (spawn (symbol-function 'make-process))
         child shell descendants result alive-at-stop)
    (unwind-protect
        (progn
          ;; Alter timing only, in the real worker's real shell, immediately
          ;; before its existing staged claim publication.  No fake writes.
          (cl-letf (((symbol-function 'make-process)
                     (lambda (&rest args)
                       (let* ((command (copy-sequence (plist-get args :command)))
                              (run (read (car (last command))))
                              (needle "      if test ! -d \"$leaf\" && ln -- \"$payload\" \"$leaf\"; then\n")
                              (pause
                               (format
                                "      if test \"$PWD\" = %s && test \"$leaf\" = 00000000000000000001.claim; then\n        printf '%%s\\n' \"$BASHPID\" >%s\n        for ((i=0; i<300; i++)); do test ! -e %s || break; sleep 0.01; done\n      fi\n"
                                (shell-quote-argument directory)
                                (shell-quote-argument marker)
                                (shell-quote-argument release))))
                         (setcar
                          (last command)
                          (prin1-to-string
                           `(progn
                              (require 'mevedel-session-control-fs)
                              (unless (string-match-p (regexp-quote ,needle)
                                                      mevedel-session-control-fs--program-script)
                                (error "Missing staged publication seam"))
                              (setq mevedel-session-control-fs--program-script
                                    (replace-regexp-in-string
                                     (regexp-quote ,needle) ,(concat pause needle)
                                     mevedel-session-control-fs--program-script t t))
                              ,run)))
                         (apply spawn (plist-put args :command command))))))
            (setq child (mevedel-journal-worker-start
                         workspace 'digest-prepare nil
                         (lambda (reply) (setq result reply)))))
          (let ((until (+ (float-time) 10)))
            (while (and (not (file-exists-p marker)) (< (float-time) until))
              (accept-process-output nil .005)))
          (ert-info ((format "Worker status: %S; reply: %S; claim directory: %S"
                             (process-status child) result directory))
            (should (file-exists-p marker)))
          (setq shell (with-temp-buffer
                        (insert-file-contents marker)
                        (string-to-number (buffer-string))))
          (should (> shell 0))
          (should (process-attributes shell))
          (let ((group (alist-get 'pgrp (process-attributes shell))))
            (setq descendants
                  (seq-filter (lambda (pid)
                                (eql group (alist-get 'pgrp (process-attributes pid))))
                              (list-system-processes))))
          (should-not (mevedel-journal-claim-current directory))
          ,stop
          (setq alive-at-stop
                (seq-filter (lambda (pid)
                              (let ((state (alist-get 'state (process-attributes pid))))
                                (and state (not (equal state "Z")))))
                            descendants))
          (write-region "go" nil release nil 'silent)
          ;; A surviving shell publishes its late claim and exits; wait for that
          ;; instead of a fixed pause.
          (let ((until (+ (float-time) 4)))
            (while (and (let ((state (cdr (assq 'state (process-attributes shell)))))
                          (and state (not (equal state "Z"))))
                        (< (float-time) until))
              (accept-process-output nil .01)))
          ;; Check the durable symptom before the OS-liveness assertion so a
          ;; red run identifies the valid late claim and refused immediate retry.
          (let ((late (mevedel-journal-claim-current directory))
                (retry (mevedel-journal-claim-acquire directory 120)))
            (ert-info ((format "Late claim: %S; immediate retry: %S" late retry))
              (should-not late)
              (should retry)))
          (should-not alive-at-stop)
          (should-not (process-live-p child))
          (should-not mevedel-journal-worker--processes)
          (should-not result))
      (when child (mevedel-journal-worker-cancel child))
      (write-region "go" nil release nil 'silent)
      ;; A red run still owns its paused shell; allow its bounded work to end
      ;; before removing the fixture, rather than leaking the reproduction.
      (let ((until (+ (float-time) 4)))
        (while (and shell
                    (let ((state (cdr (assq 'state (process-attributes shell)))))
                      (and state (not (equal state "Z"))))
                    (< (float-time) until))
          (sleep-for .01)))
      (delete-directory root t)))
  (stop)
  :doc "explicit cancellation stops real shells before staged claim publication"
  (mevedel-journal-worker-cancel child)
  :doc "stop-all stops real shells before staged claim publication"
  (mevedel-journal-worker-stop-all)
  :doc "the registered exit hook stops real shells before staged claim publication"
  (run-hooks 'kill-emacs-hook))

(mevedel-deftest mevedel-journal-worker-stop-all/shutdown-error ()
  (let* ((root (make-temp-file "mevedel-worker-stop-error-" t))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (kill-emacs-hook kill-emacs-hook)
         (mevedel-journal-worker--processes nil)
         (mevedel-journal-process--pending (make-hash-table :test #'equal))
         (mevedel-journal-process--recover-requested (make-hash-table :test #'equal))
         (mevedel-journal-process--workers (make-hash-table :test #'equal))
         (mevedel-journal-process--running (make-hash-table :test #'equal))
         (stop (symbol-function 'mevedel-journal-worker--stop))
         (attempts 0)
         children)
    (unwind-protect
        (progn
          (dotimes (n 2)
            (let ((child (mevedel-journal-worker-start workspace 'recovery nil #'ignore)))
              (push child children)
              (puthash n child mevedel-journal-process--workers)))
          (cl-letf (((symbol-function 'mevedel-journal-worker--stop)
                     (lambda (child)
                       (cl-incf attempts)
                       (funcall stop child)
                       ;; Simulate the deadline failure after real child teardown;
                       ;; do not leave an unkillable process in the test runner.
                       (when (= attempts 1) (error "Injected shutdown timeout")))))
            (should-error ,stop-all :type 'error))
          (should (= attempts 2))
          (dolist (child children)
            (should-not (process-live-p child))
            (should-not (file-exists-p (process-get child 'mevedel-journal-worker-directory))))
          (should-not mevedel-journal-worker--processes))
      (mevedel-journal-worker-stop-all)
      (delete-directory root t)))
  (stop-all)
  :doc "worker stop-all attempts later workers after a shutdown timeout"
  (mevedel-journal-worker-stop-all)
  :doc "digest stop-all attempts later workers after a shutdown timeout"
  (mevedel-journal-process-stop-all))

(mevedel-deftest mevedel-journal-worker-cancel/shutdown-error ()
  ,test
  (test)
  :doc "a single owner still settles and delivers after a failed worker stop"
  (let* ((root (make-temp-file "mevedel-worker-owner-error-" t))
         (workspace (mevedel-workspace--create :root root :id root :type 'project))
         (kill-emacs-hook kill-emacs-hook)
         (mevedel-journal-worker--processes nil)
         (mevedel-memory-pass--running (make-hash-table :test #'equal))
         (claim (mevedel-journal-claim-acquire (file-name-concat root "claim") 120))
         (child (mevedel-journal-worker-start workspace 'recovery nil #'ignore))
         (stop (symbol-function 'mevedel-journal-worker--stop))
         result warning
         (state (list :workspace workspace :claim claim :worker child
                      :admission-aborted t :callback (lambda (value) (setq result value)))))
    (unwind-protect
        (progn
          (puthash (plist-get claim :directory) state mevedel-memory-pass--running)
          (mevedel-test--with-captured-diagnostics warning
            (cl-letf (((symbol-function 'mevedel-journal-worker--stop)
                       (lambda (process)
                         (funcall stop process)
                         (error "Injected shutdown timeout"))))
              (mevedel-memory-pass--finish state '(:outcome aborted))))
          (should (equal warning (format "mevedel [%s]: Injected shutdown timeout\n" root)))
          (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome claim) :status)))
          (should (eq 'aborted (plist-get result :outcome)))
          (should (zerop (hash-table-count mevedel-memory-pass--running)))
          (should-not mevedel-journal-worker--processes)
          (should-not (file-exists-p (process-get child 'mevedel-journal-worker-directory))))
      (mevedel-journal-worker-stop-all)
      (delete-directory root t))))

(provide 'test-mevedel-journal-worker)
;;; test-mevedel-journal-worker.el ends here
