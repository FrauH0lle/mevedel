;;; test-mevedel-session-durability-heartbeat-integration.el --- Heartbeat consumers -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real publication, adoption and transfer consumers while a heartbeat
;; holds a real control program's completed result at its callback boundary.

;;; Code:

(require 'cl-lib)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-control-transfer)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-durability)
(require 'mevedel-session-publication)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "mevedel-session-publication-test-helpers"))

(defun test-mevedel-heartbeat-integration--held (session body)
  "Call BODY with a continuation releasing SESSION's held heartbeat result."
  (let ((run (symbol-function 'mevedel-session-control-fs-run-program-async))
        launched resume)
    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
               (lambda (operations callback)
                 (let ((hold (not launched)))
                   (setq launched t)
                   (funcall run operations
                            (lambda (results error)
                              (if hold
                                  (setq resume
                                        (lambda ()
                                          (setq resume nil)
                                          (funcall callback results error)))
                                (funcall callback results error))))))))
      (unwind-protect
          (progn
            (mevedel-session-durability-heartbeat session)
            (test-mevedel-publication-async--wait (lambda () resume))
            (funcall body resume))
        (when resume (funcall resume))
        (test-mevedel-publication-async--wait
         (lambda () (not (mevedel-session-lease-renewal-job session))))))))

(mevedel-deftest mevedel-session-durability-heartbeat/integration (:quiet t)
  ,test
  (test)
  :doc "a queued publication acknowledges once and restores its committed mailbox"
  (test-mevedel-publication-async--fixture
   (lambda (session _buffer)
     (let (acks)
       (test-mevedel-heartbeat-integration--held
        session
        (lambda (resume)
          (should (eq 'pending
                      (test-mevedel-publication-async--send
                       session "behind-heartbeat"
                       (lambda (ok error) (push (list ok error) acks)))))
          (accept-process-output nil 0.02)
          (should-not acks)
          (should-not (mevedel-session-publication-job session))
          (funcall resume)
          (test-mevedel-publication-async--wait (lambda () acks))))
       (should (equal '((t nil)) acks))
       (should (equal (list (secure-hash 'sha256 "/root:behind-heartbeat"))
                      (test-mevedel-publication-async--ids
                       (test-mevedel-publication-async--restored-mail session)))))))

  :doc "adoption drains a heartbeat on either source or destination before moving authority"
  (dolist (held-role '(source destination))
    (test-mevedel-publication-async--fixture
     (lambda (session _buffer)
       (let* ((child-path (file-name-as-directory
                           (make-temp-file "heartbeat-adopt-" t)))
              (child (mevedel-session-create "child" (mevedel-session-workspace session)))
              (source (if (eq held-role 'source) session child))
              (destination (if (eq held-role 'source) child session)))
         (setf (mevedel-session-save-path child) child-path)
         (unwind-protect
             (progn
               (should (mevedel-session-durability-lease-acquire child-path "child" child))
               (test-mevedel-heartbeat-integration--held
                session
                (lambda (resume)
                  (let ((event (run-at-time 0 nil resume)))
                    (unwind-protect
                        (should (mevedel-session-durability-adopt-owned-lease destination source))
                      (cancel-timer event)))
                  (should-not (mevedel-session-lease-renewal-job source))
                  (should-not (mevedel-session-lease-renewal-job destination))))
               (should-not (mevedel-session-lease source))
               (should-not (mevedel-session-lease-renewal-timer source))
               (should (mevedel-session-durability-lease-owned-p destination))
               (should (eq #'mevedel-session-durability-heartbeat
                           (timer--function (mevedel-session-lease-renewal-timer destination)))))
           (mevedel-session-durability--cancel-renewal child)
           (mevedel-session-durability-lease-release
            (mevedel-session-save-path destination) destination))))))

  :doc "transfer rejection and release drain before observing or rotating real authority"
  (dolist (action '(reject release))
    (test-mevedel-publication-async--fixture
     (lambda (session _buffer)
       (let* ((request (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
                         (mevedel-session-transfer-request session)))
              (generation (plist-get (mevedel-session-lease session) :generation))
              (head (symbol-function 'mevedel-session-durability--lease-head)))
         (should request)
         (mevedel-session-transfer-poll session)
         (when (eq action 'release) (mevedel-session-transfer-decide session 'grant))
         (test-mevedel-heartbeat-integration--held
          session
          (lambda (resume)
            (let ((event (run-at-time 0 nil resume)))
              (unwind-protect
                  (cl-letf (((symbol-function 'mevedel-session-durability--lease-head)
                             (lambda (&rest args)
                               (should-not (mevedel-session-lease-renewal-job session))
                               (apply head args))))
                    (if (eq action 'reject)
                        (should (eq 'reject (plist-get (mevedel-session-transfer-decide session 'reject)
                                                       :decision)))
                      (should (equal (plist-get request :request-id)
                                     (plist-get (mevedel-session-transfer-release session) :request-id)))))
                (cancel-timer event)))
            (should-not (mevedel-session-lease-renewal-job session))))
         (if (eq action 'reject)
             (progn
               (should (eq 'rejected (plist-get (mevedel-session-control-transfer session) :state)))
               (should (= (1+ generation) (plist-get (mevedel-session-lease session) :generation)))
               (should (mevedel-session-durability-lease-owned-p session)))
           (should (eq 'released (plist-get (mevedel-session-control-transfer session) :state)))
           (should-not (mevedel-session-lease session))))))))

(mevedel-deftest mevedel-session-control-transfer-acquire/heartbeat (:quiet t)
  ,test
  (test)
  :doc "staged adoption cannot erase a heartbeat launched during instruction loading"
  (test-mevedel-publication-async--fixture
   (lambda (session buffer)
     (let ((load-instructions (symbol-function 'mevedel-session-artifacts-load-instructions))
           (injections 0))
       (test-mevedel-heartbeat-integration--held
        session
        (lambda (resume)
          (let ((event (run-at-time 0 nil resume)))
            (unwind-protect
                (cl-letf (((symbol-function 'mevedel-session-artifacts-load-instructions)
                           (lambda (&rest args)
                             (cl-incf injections)
                             (mevedel-session-durability-heartbeat session)
                             (should-not (mevedel-session-lease-renewal-job session))
                             (apply load-instructions args))))
                  (should (mevedel-session-control-transfer-acquire session buffer)))
              (cancel-timer event)))))
       (should (= injections 1))
       (should-not (mevedel-session-lease-renewal-job session))
       (should (mevedel-session-durability-lease-owned-p session))))))

(provide 'test-mevedel-session-durability-heartbeat-integration)
;;; test-mevedel-session-durability-heartbeat-integration.el ends here
