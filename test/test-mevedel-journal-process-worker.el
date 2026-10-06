;;; test-mevedel-journal-process-worker.el --- Digest preparation workers -*- lexical-binding: t -*-

;;; Commentary:
;; Storage preparation uses real claims; model calls stay in the editor.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-process)

(mevedel-deftest mevedel-journal-process--prepare ()
  ,test
  (test)
  :doc "selects and fences one sealed capture without starting a request"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (mevedel-session-workspace session))
            (prepared (mevedel-journal-process--prepare workspace)))
       (unwind-protect
           (progn
             (should (mevedel-journal-store-id-p (plist-get prepared :capture-id)))
             (should-not (plist-member prepared :capture))
             (should-not (gethash (mevedel-journal-process--key workspace)
                                  mevedel-journal-process--running))
             (should (mevedel-journal-claim-owned-p (plist-get prepared :admission)
                                                   (plist-get prepared :claim)))
             (should-not (mevedel-journal-process--prepare workspace)))
         (mevedel-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
         (mevedel-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))

  :doc "lists a capture as exhausted only once its third automatic attempt settled"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (workspace (mevedel-session-workspace session))
            (attempts (mevedel-journal-process--attempts workspace capture))
            third)
       (dotimes (_ 2)
         (mevedel-journal-claim-settle (mevedel-journal-claim-acquire attempts 120) 'failed "{}"))
       (setq third (mevedel-journal-claim-acquire attempts 120))
       ;; A live third attempt is still running, not exhausted.
       (should-not (mevedel-journal-process--prepare workspace))
       (mevedel-journal-claim-settle third 'failed "{}")
       (let ((prepared (mevedel-journal-process--prepare workspace)))
         (should (equal (list :exhausted (list (list :id (plist-get capture :id)
                                                     :session-name "capture")))
                        prepared))))))

  :doc "an explicit retry ignores and does not report automatic exhaustion"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (workspace (mevedel-session-workspace session))
            (attempts (mevedel-journal-process--attempts workspace capture)))
       (dotimes (_ 3)
         (mevedel-journal-claim-settle (mevedel-journal-claim-acquire attempts 120) 'failed "{}"))
       (let ((prepared (mevedel-journal-process--prepare workspace (plist-get capture :id))))
         (unwind-protect
             (progn
               (should (plist-get prepared :claim))
               (should-not (plist-member prepared :exhausted)))
           (mevedel-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
           (mevedel-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))))

(mevedel-deftest mevedel-journal-process--warn-exhausted ()
  ,test
  (test)
  :doc "names newly exhausted captures once and points to the job browser"
  (let ((mevedel-journal-process--exhausted-warned (make-hash-table :test #'equal))
        (first (make-string 64 ?a))
        (second (make-string 64 ?b))
        warning)
    (mevedel-test--with-captured-diagnostics warning
      (mevedel-journal-process--warn-exhausted
       (list (list :id first :session-name "Refactor") (list :id second :session-name ""))))
    (should (equal (concat "Journal digest generation failed 3 times for aaaaaaaaaaaa (Refactor), "
                           "bbbbbbbbbbbb; retry or discard them from M-x mevedel-journal-jobs\n")
                   warning))
    (mevedel-test--with-captured-diagnostics warning
      (mevedel-journal-process--warn-exhausted (list (list :id first :session-name "Refactor"))))
    (should (string-empty-p warning))
    (mevedel-test--with-captured-diagnostics warning
      (mevedel-journal-process--warn-exhausted nil))
    (should (string-empty-p warning))))

(mevedel-deftest mevedel-journal-worker-start/digest ()
  ,test
  (test)
  :doc "child selects a sealed capture with a compact fenced handoff"
  (mevedel-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (mevedel-session-workspace session))
            process result completed prepared timer tick)
       (unwind-protect
           (progn
             (setq process (mevedel-journal-worker-start
                            workspace 'digest-prepare
                            (lambda (value) (setq result value completed t)))
                   timer (run-at-time 0 nil (lambda () (setq tick t))))
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until)) (accept-process-output nil .01)))
             (should completed)
             (should tick)
             (should-not (plist-get result :error))
             (setq prepared (plist-get result :prepared))
             (should (plist-get prepared :capture-id))
             (should-not (plist-get prepared :capture))
             (should (mevedel-journal-claim-owned-p (plist-get prepared :admission)
                                                   (plist-get prepared :claim)))
             (should-not (gethash (mevedel-journal-process--key workspace)
                                  mevedel-journal-process--running)))
         (when timer (cancel-timer timer))
         (when (and process (process-live-p process)) (delete-process process))
         (when prepared
           (mevedel-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
           (mevedel-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))))

(mevedel-deftest mevedel-journal-process--prepared ()
  ,test
  (test)
  :doc "a cancelled prepared claim cannot start model inference"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (mevedel-session-workspace session))
            (prepared (mevedel-journal-process--prepare workspace)))
       (mevedel-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Cancelled preparation started inference"))))
         (should-error (mevedel-journal-process--prepared workspace prepared)))
       (should-not (mevedel-journal-claim-owned-p (plist-get prepared :admission)))
       (should-not (gethash (mevedel-journal-process--key workspace)
                            mevedel-journal-process--running)))))

  :doc "disabling journaling during preparation releases claims without inference"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Question" "Answer")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (mevedel-session-workspace session))
            (prepared (mevedel-journal-process--prepare workspace))
            (mevedel-journal-enabled nil))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Disabled journaling started inference"))))
         (should-not (mevedel-journal-process--prepared workspace prepared)))
       (should-not (mevedel-journal-claim-owned-p (plist-get prepared :admission)))
       (should-not (gethash (mevedel-journal-process--key workspace)
                            mevedel-journal-process--running)))))

  :doc "reports exhausted captures without claims or inference"
  (let ((mevedel-journal-process--exhausted-warned (make-hash-table :test #'equal))
        (workspace (mevedel-workspace--create :root temporary-file-directory))
        warning)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (&rest _) (ert-fail "Exhausted captures started inference"))))
      (mevedel-test--with-captured-diagnostics warning
        (should-not (mevedel-journal-process--prepared
                     workspace (list :exhausted (list (list :id (make-string 64 ?c)
                                                            :session-name "Long")))))))
    (should (string-match-p "cccccccccccc (Long); retry or discard it" warning))))

;;; test-mevedel-journal-process-worker.el ends here
