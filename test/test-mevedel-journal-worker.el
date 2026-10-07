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

(mevedel-deftest mevedel-journal-worker-supported-p ()
  ,test
  (test)
  :doc "only local Linux owners dispatch children"
  (let ((workspace (mevedel-workspace--create :root "/tmp/")))
    (let ((system-type 'gnu/linux))
      (should (mevedel-journal-worker-supported-p workspace))
      (let ((mevedel-journal-worker--child-p t))
        (should-not (mevedel-journal-worker-supported-p workspace)))
      (setf (mevedel-workspace-root workspace) "/ssh:host:/tmp/")
      (should-not (mevedel-journal-worker-supported-p workspace)))))

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
             (setq process (mevedel-journal-worker-start workspace 'recovery (lambda (value) (setq result value)))
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
             (setq process (mevedel-journal-worker-start workspace 'cleanup (lambda (value) (setq result value)) t))
             (let ((until (+ (float-time) 15)))
               (while (and (not result) (< (float-time) until))
                 (accept-process-output nil .01)))
             (should (plist-get result :ok))
             (should (numberp (plist-get result :cleanup-at)))
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
                 (lambda (value)
                   (setq result value)
                   (should mevedel-journal-process--inhibit-scheduling)
                   (should mevedel-journal-cleanup--inhibit-scheduling)
                   (should mevedel-memory-pass--inhibit-scheduling))))
          (setq directory (file-name-directory
                           (nth 1 (read (car (last (process-command process)))))))
          (should (file-directory-p directory))
          (mevedel-journal-worker-stop-all)
          (accept-process-output nil .01)
          (should-not (file-exists-p directory))
          (should-not (process-live-p process))
          (should-not mevedel-journal-worker--processes)
          (should (plist-get result :error)))
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

(provide 'test-mevedel-journal-worker)
;;; test-mevedel-journal-worker.el ends here
