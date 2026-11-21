;;; test-niwa-session-durability-heartbeat-integration.el --- Heartbeat consumers -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise real publication, adoption and transfer consumers while a heartbeat
;; holds a real control program's completed result at its callback boundary.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-control-fs)
(require 'niwa-session-control-transfer)
(require 'niwa-session-artifacts)
(require 'niwa-session-durability)
(require 'niwa-session-publication)
(require 'niwa-session-transfer)
(require 'niwa-session-publication-test-helpers
         (file-name-concat (file-name-directory
                            (or load-file-name buffer-file-name))
                           "niwa-session-publication-test-helpers"))

(defun test-niwa-heartbeat-integration--held (session body)
  "Call BODY with a continuation releasing SESSION's held heartbeat result."
  (let ((run (symbol-function 'niwa-session-control-fs-run-program-async))
        launched resume)
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
               (lambda (operations callback &optional scope)
                 (let ((hold (not launched)))
                   (setq launched t)
                   (funcall run operations
                            (lambda (results error)
                              (if hold
                                  (setq resume
                                        (lambda ()
                                          (setq resume nil)
                                          (funcall callback results error)))
                                (funcall callback results error)))
                            scope)))))
      (unwind-protect
          (progn
            (niwa-session-durability-heartbeat session)
            (test-niwa-publication-async--wait (lambda () resume))
            (funcall body resume))
        (when resume (funcall resume))
        (test-niwa-publication-async--wait
         (lambda () (not (niwa-session-lease-renewal-job session))))))))

(niwa-deftest niwa-session-durability-heartbeat/integration (:quiet t)
  ,test
  (test)
  :doc "a queued publication acknowledges once and restores its committed mailbox"
  (test-niwa-publication-async--fixture
   (lambda (session _buffer)
     (let (acks)
       (test-niwa-heartbeat-integration--held
        session
        (lambda (resume)
          (should (eq 'pending
                      (test-niwa-publication-async--send
                       session "behind-heartbeat"
                       (lambda (ok error) (push (list ok error) acks)))))
          (accept-process-output nil 0.02)
          (should-not acks)
          (should-not (niwa-session-publication-job session))
          (funcall resume)
          (test-niwa-publication-async--wait (lambda () acks))))
       (should (equal '((t nil)) acks))
       (should (equal (list (secure-hash 'sha256 "/root:behind-heartbeat"))
                      (test-niwa-publication-async--ids
                       (test-niwa-publication-async--restored-mail session)))))))

  :doc "adoption drains a heartbeat on either source or destination before moving authority"
  (dolist (held-role '(source destination))
    (test-niwa-publication-async--fixture
     (lambda (session _buffer)
       (let* ((child-path (file-name-as-directory
                           (make-temp-file "heartbeat-adopt-" t)))
              (child (niwa-session-create "child" (niwa-session-workspace session)))
              (source (if (eq held-role 'source) session child))
              (destination (if (eq held-role 'source) child session)))
         (setf (niwa-session-save-path child) child-path)
         (unwind-protect
             (progn
               (should (niwa-session-durability-lease-acquire child-path "child" child))
               (test-niwa-heartbeat-integration--held
                session
                (lambda (resume)
                  (let ((event (run-at-time 0 nil resume)))
                    (unwind-protect
                        (should (niwa-session-durability-adopt-owned-lease destination source))
                      (cancel-timer event)))
                  (should-not (niwa-session-lease-renewal-job source))
                  (should-not (niwa-session-lease-renewal-job destination))))
               (should-not (niwa-session-lease source))
               (should-not (niwa-session-lease-renewal-timer source))
               (should (niwa-session-durability-lease-owned-p destination))
               (should (eq #'niwa-session-durability-heartbeat
                           (timer--function (niwa-session-lease-renewal-timer destination)))))
           (niwa-session-durability--cancel-renewal child)
           (niwa-session-durability-lease-release
            (niwa-session-save-path destination) destination))))))

  :doc "transfer rejection and release drain before observing or rotating real authority"
  (dolist (action '(reject release))
    (test-niwa-publication-async--fixture
     (lambda (session _buffer)
       (let* ((request (let ((niwa-session-durability--client-id (make-string 64 ?b)))
                         (niwa-session-transfer-request session)))
              (generation (plist-get (niwa-session-lease session) :generation))
              (head (symbol-function 'niwa-session-durability--lease-head)))
         (should request)
         (niwa-session-transfer-poll session)
         (when (eq action 'release) (niwa-session-transfer-decide session 'grant))
         (test-niwa-heartbeat-integration--held
          session
          (lambda (resume)
            (let ((event (run-at-time 0 nil resume)))
              (unwind-protect
                  (cl-letf (((symbol-function 'niwa-session-durability--lease-head)
                             (lambda (&rest args)
                               (should-not (niwa-session-lease-renewal-job session))
                               (apply head args))))
                    (if (eq action 'reject)
                        (should (eq 'reject (plist-get (niwa-session-transfer-decide session 'reject)
                                                       :decision)))
                      (should (equal (plist-get request :request-id)
                                     (plist-get (niwa-session-transfer-release session) :request-id)))))
                (cancel-timer event)))
            (should-not (niwa-session-lease-renewal-job session))))
         (if (eq action 'reject)
             (progn
               (should (eq 'rejected (plist-get (niwa-session-control-transfer session) :state)))
               (should (= (1+ generation) (plist-get (niwa-session-lease session) :generation)))
               (should (niwa-session-durability-lease-owned-p session)))
           (should (eq 'released (plist-get (niwa-session-control-transfer session) :state)))
           (should-not (niwa-session-lease session))))))))

(niwa-deftest niwa-session-control-transfer-acquire/heartbeat (:quiet t)
  ,test
  (test)
  :doc "staged adoption cannot erase a heartbeat launched during instruction loading"
  (test-niwa-publication-async--fixture
   (lambda (session buffer)
     (let ((load-instructions (symbol-function 'niwa-session-artifacts-load-instructions))
           (injections 0))
       (test-niwa-heartbeat-integration--held
        session
        (lambda (resume)
          (let ((event (run-at-time 0 nil resume)))
            (unwind-protect
                (cl-letf (((symbol-function 'niwa-session-artifacts-load-instructions)
                           (lambda (&rest args)
                             (cl-incf injections)
                             (niwa-session-durability-heartbeat session)
                             (should-not (niwa-session-lease-renewal-job session))
                             (apply load-instructions args))))
                  (should (niwa-session-control-transfer-acquire session buffer)))
              (cancel-timer event)))))
       (should (= injections 1))
       (should-not (niwa-session-lease-renewal-job session))
       (should (niwa-session-durability-lease-owned-p session))))))

(provide 'test-niwa-session-durability-heartbeat-integration)
;;; test-niwa-session-durability-heartbeat-integration.el ends here
