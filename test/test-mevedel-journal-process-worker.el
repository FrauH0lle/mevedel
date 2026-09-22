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
         (mevedel-journal-claim-settle (plist-get prepared :admission) 'cancelled ""))))))

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
                            mevedel-journal-process--running))))))

;;; test-mevedel-journal-process-worker.el ends here
