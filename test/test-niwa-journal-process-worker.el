;;; test-niwa-journal-process-worker.el --- Digest preparation workers -*- lexical-binding: t -*-

;;; Commentary:
;; Storage preparation uses real claims; model calls stay in the editor.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-process)

(niwa-deftest niwa-journal-process--prepare ()
  ,test
  (test)
  :doc "selects and fences one sealed capture without starting a request"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (niwa-session-workspace session))
            (prepared (niwa-journal-process--prepare workspace)))
       (unwind-protect
           (progn
             (should (niwa-journal-store-id-p (plist-get prepared :capture-id)))
             (should-not (plist-member prepared :capture))
             (should-not (gethash (niwa-journal-process--key workspace)
                                  niwa-journal-process--running))
             (should (niwa-journal-claim-owned-p (plist-get prepared :admission)
                                                   (plist-get prepared :claim)))
             (should-not (niwa-journal-process--prepare workspace)))
         (niwa-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
         (niwa-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))

  :doc "lists a capture as exhausted only once its third automatic attempt settled"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (workspace (niwa-session-workspace session))
            (attempts (niwa-journal-process--attempts workspace capture))
            third)
       (dotimes (_ 2)
         (niwa-journal-claim-settle (niwa-journal-claim-acquire attempts 120) 'failed "{}"))
       (setq third (niwa-journal-claim-acquire attempts 120))
       ;; A live third attempt is still running, not exhausted.
       (should-not (niwa-journal-process--prepare workspace))
       (niwa-journal-claim-settle third 'failed "{}")
       (let ((prepared (niwa-journal-process--prepare workspace)))
         (should (equal (list :exhausted (list (list :id (plist-get capture :id)
                                                     :session-name "capture")))
                        prepared))))))

  :doc "an explicit retry ignores and does not report automatic exhaustion"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (workspace (niwa-session-workspace session))
            (attempts (niwa-journal-process--attempts workspace capture)))
       (dotimes (_ 3)
         (niwa-journal-claim-settle (niwa-journal-claim-acquire attempts 120) 'failed "{}"))
       (let ((prepared (niwa-journal-process--prepare workspace (plist-get capture :id))))
         (unwind-protect
             (progn
               (should (plist-get prepared :claim))
               (should-not (plist-member prepared :exhausted)))
           (niwa-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
           (niwa-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))))

(niwa-deftest niwa-journal-process--warn-exhausted ()
  ,test
  (test)
  :doc "names newly exhausted captures once and points to the job browser"
  (let ((niwa-journal-process--exhausted-warned (make-hash-table :test #'equal))
        (first (make-string 64 ?a))
        (second (make-string 64 ?b))
        warning)
    (niwa-test--with-captured-diagnostics warning
      (niwa-journal-process--warn-exhausted
       "/tmp/journal-owner/" (list (list :id first :session-name "Refactor") (list :id second :session-name ""))))
    (should (equal (concat "niwa [/tmp/journal-owner/]: Journal digest generation failed 3 times for aaaaaaaaaaaa (Refactor), "
                           "bbbbbbbbbbbb; retry or discard them from M-x niwa-journal-jobs\n")
                   warning))
    (niwa-test--with-captured-diagnostics warning
      (niwa-journal-process--warn-exhausted "/tmp/journal-owner/" (list (list :id first :session-name "Refactor"))))
    (should (string-empty-p warning))
    (niwa-test--with-captured-diagnostics warning
      (niwa-journal-process--warn-exhausted "/tmp/journal-owner/" nil))
    (should (string-empty-p warning))))

(niwa-deftest niwa-journal-worker-start/digest ()
  ,test
  (test)
  :doc "child selects a sealed capture with a compact fenced handoff"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (niwa-session-workspace session))
            process result completed prepared timer tick)
       (unwind-protect
           (progn
             (setq process (niwa-journal-worker-start
                            workspace 'digest-prepare
                            nil (lambda (value) (setq result value completed t)))
                   timer (run-at-time 0 nil (lambda () (setq tick t))))
             (let ((until (+ (float-time) 15)))
               (while (and (not completed) (< (float-time) until)) (accept-process-output nil .01)))
             (should completed)
             (should tick)
             (should-not (plist-get result :error))
             (setq prepared (plist-get result :prepared))
             (should (plist-get prepared :capture-id))
             (should-not (plist-get prepared :capture))
             (should (niwa-journal-claim-owned-p (plist-get prepared :admission)
                                                   (plist-get prepared :claim)))
             (should-not (gethash (niwa-journal-process--key workspace)
                                  niwa-journal-process--running)))
         (when timer (cancel-timer timer))
         (when (and process (process-live-p process)) (delete-process process))
         (when prepared
           (niwa-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
           (niwa-journal-claim-settle (plist-get prepared :admission) 'cancelled "")))))))

(niwa-deftest niwa-journal-process--prepared ()
  ,test
  (test)
  :doc "a cancelled prepared claim cannot start model inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (niwa-session-workspace session))
            (prepared (niwa-journal-process--prepare workspace)))
       (niwa-journal-claim-settle (plist-get prepared :claim) 'cancelled "")
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Cancelled preparation started inference"))))
         (should-error (niwa-journal-process--prepared workspace prepared)))
       (should-not (niwa-journal-claim-owned-p (plist-get prepared :admission)))
       (should-not (gethash (niwa-journal-process--key workspace)
                            niwa-journal-process--running)))))

  :doc "disabling journaling during preparation releases claims without inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let* ((workspace (niwa-session-workspace session))
            (prepared (niwa-journal-process--prepare workspace))
            (niwa-journal-enabled nil))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (&rest _) (ert-fail "Disabled journaling started inference"))))
         (should-not (niwa-journal-process--prepared workspace prepared)))
       (should-not (niwa-journal-claim-owned-p (plist-get prepared :admission)))
       (should-not (gethash (niwa-journal-process--key workspace)
                            niwa-journal-process--running)))))

  :doc "reports exhausted captures without claims or inference"
  (let ((niwa-journal-process--exhausted-warned (make-hash-table :test #'equal))
        (workspace (niwa-workspace--create :root temporary-file-directory))
        warning)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (&rest _) (ert-fail "Exhausted captures started inference"))))
      (niwa-test--with-captured-diagnostics warning
        (should-not (niwa-journal-process--prepared
                     workspace (list :exhausted (list (list :id (make-string 64 ?c)
                                                            :session-name "Long")))))))
    (should (string-match-p "cccccccccccc (Long); retry or discard it" warning))))

;;; test-niwa-journal-process-worker.el ends here
