;;; test-mevedel-telemetry-memory-pass-integration.el --- Pass watch cleanup -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise lag-watch cleanup through real pass admission and settlement,
;; injecting only startup reentry and automatic application setup failures.

;;; Code:

(require 'mevedel-journal-worker)
(require 'mevedel-memory-decision)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-review)
(require 'mevedel-memory-store)
(require 'mevedel-native)
(require 'mevedel-telemetry)
(require 'mevedel-utilities)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-telemetry-memory-pass-integration--closed (state)
  "Assert STATE's watch has a bounded tail and cleans up after expiry."
  (let* ((handle (plist-get state :lag-watch))
         (until (plist-get (cdr handle) :until))
         (timer mevedel-telemetry--lag-timer))
    (should handle)
    (should (memq handle mevedel-telemetry--lag-pass-windows))
    (should (numberp until))
    (should (< (float-time) until (+ (float-time) 121)))
    (should (mevedel--ui-timer-pending-p timer))
    ;; A late callback cannot extend the tail or reopen this exact handle.
    (mevedel-telemetry-memory-pass-settle handle)
    (should (= until (plist-get (cdr handle) :until)))
    (setf (plist-get (cdr handle) :until) (- (float-time) 1))
    (mevedel--coalesced-timer-call timer)
    (should-not mevedel-telemetry--lag-pass-windows)
    (should-not mevedel-telemetry--lag-timer)
    (should-not (mevedel--ui-timer-pending-p timer))
    (should-not (advice-member-p #'mevedel-telemetry--lag-time-callback
                                 'timer-event-handler))
    (should-not (advice-member-p #'mevedel-telemetry--lag-time-callback
                                 'mevedel--coalesced-timer-call))))

(mevedel-deftest mevedel-telemetry-memory-pass-start/integration
  (:vars ((mevedel-native--load-state "unavailable in fallback tests")
          (mevedel-memory-pass--running (make-hash-table :test #'equal))
          (mevedel-memory-consolidation-mode 'manual)
          (mevedel-memory-dirs nil))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "reentrant cancellation before starter returns still closes its returned handle"
  (let* ((root (make-temp-file "mevedel-pass-start-reentry-" t))
         (workspace (mevedel-workspace--create :root root))
         (noninteractive nil)
         (real-start (symbol-function 'mevedel-telemetry--lag-start))
         state returned outcome (injections 0) (callbacks 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-telemetry--lag-start)
                     (lambda ()
                       (cl-incf injections)
                       ;; Same yield window as cold native loading: telemetry
                       ;; owns a handle, but the pass has not received it yet.
                       (setq state (mevedel-memory-pass-running workspace))
                       (should state)
                       (should-not (plist-get state :lag-watch))
                       (should (= 1 (length mevedel-telemetry--lag-pass-windows)))
                       (funcall real-start)
                       (should (mevedel-memory-pass-cancel workspace))
                       (should (plist-get state :settled))))
                    ((symbol-function 'mevedel-memory-review-request)
                     (lambda (&rest _) (ert-fail "Cancelled startup reached inference"))))
            (setq returned
                  (mevedel-memory-pass-start
                   workspace (lambda (result) (cl-incf callbacks) (setq outcome result))
                   :memory-only t)))
          (should (= 1 injections))
          (should (= 1 callbacks))
          (should-not returned)
          (should (eq 'aborted (plist-get outcome :outcome)))
          (should-not (plist-get state :timer))
          (should-not (mevedel-memory-pass-running workspace))
          (should (eq 'cancelled
                      (plist-get (mevedel-journal-claim-outcome (plist-get state :claim)) :status)))
          (test-mevedel-telemetry-memory-pass-integration--closed state))
      (mevedel-memory-pass-cancel workspace)
      (mevedel-telemetry--lag-stop)
      (delete-directory root t))))

(mevedel-deftest mevedel-telemetry-memory-pass-settle/integration
  (:vars ((mevedel-native--load-state "unavailable in fallback tests")
          (mevedel-memory-pass--running (make-hash-table :test #'equal))
          (mevedel-memory-consolidation-mode 'auto)
          (mevedel-memory-dirs nil))
   :after-each (mevedel-telemetry--lag-stop))
  ,test
  (test)
  :doc "application setup error and quit close a settled pass watch and propagate unchanged"
  (dolist (async '(nil t))
    (dolist (kind '(error quit))
      (let* ((root (make-temp-file "mevedel-pass-apply-unwind-" t))
             (workspace (mevedel-workspace--create :root root))
             (noninteractive nil)
             (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
             (expected (list kind "Injected application setup failure"))
             state review-callback raised (injections 0) (callbacks 0))
        (unwind-protect
            (cl-letf (((symbol-function 'mevedel-journal-worker-supported-p) #'ignore)
                      ((symbol-function 'mevedel-memory-review-request)
                       (lambda (_scope _entries callback &rest _)
                         (setq review-callback callback)
                         (list :cancel #'ignore)))
                      ((symbol-function 'mevedel-memory-pass--async-application-p)
                       (lambda (&rest _) async))
                      ((symbol-function 'mevedel-memory-decision--operation-context)
                       (lambda (&rest _)
                         (cl-incf injections)
                         (should (plist-get state :settled))
                         (should-not (plist-get state :timer))
                         (should-not (plist-get state :application))
                         (signal (car expected) (cdr expected)))))
              ;; Real claims, preparation, acceptance and publication precede
              ;; the injected context failure in either application strategy.
              (setq state (mevedel-memory-pass-start
                           workspace (lambda (_) (cl-incf callbacks)) :memory-only t))
              (should review-callback)
              (should-not (plist-get state :settled))
              (should-not (plist-get (cdr (plist-get state :lag-watch)) :until))
              (setq raised
                    (condition-case caught
                        (progn (funcall review-callback (list :outcome 'success :reply none)) nil)
                      ((error quit) caught)))
              (should (= 1 injections))
              (should (equal expected raised))
              (should (= 0 callbacks))
              (should (plist-get state :settled))
              (should (eq 'completed
                          (plist-get (mevedel-journal-claim-outcome (plist-get state :claim)) :status)))
              (should (mevedel-memory-store-accepted
                       workspace (plist-get (plist-get state :claim) :owner)))
              (test-mevedel-telemetry-memory-pass-integration--closed state))
          (when (timerp (plist-get state :timer)) (cancel-timer (plist-get state :timer)))
          (remhash (mevedel-memory-store--claim-directory workspace) mevedel-memory-pass--running)
          (mevedel-telemetry--lag-stop)
          (delete-directory root t))))))

(provide 'test-mevedel-telemetry-memory-pass-integration)
;;; test-mevedel-telemetry-memory-pass-integration.el ends here
