;;; test-mevedel-session-control-transfer.el --- Transfer coordinator tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-owned transfer decisions without constructing a view.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-agent-control)
(require 'mevedel-execution)
(require 'mevedel-session-control-transfer)
(require 'mevedel-structs)

(mevedel-deftest mevedel-session-control-transfer-drain-registry ()
  ,test
  (test)
  :doc "combines session drains and safely unregisters transient owners"
  (let ((session (mevedel-session--create :name "transfer"))
        (pending t))
    (should (mevedel-session-control-transfer-drained-p session))
    (let ((predicate (lambda () pending)))
      (should (eq predicate
                  (mevedel-session-control-transfer-register-drain
                   session predicate)))
      (should-not (mevedel-session-control-transfer-drained-p session))
      (setq pending nil)
      (should (mevedel-session-control-transfer-drained-p session))
      (mevedel-session-control-transfer-unregister-drain session predicate)
      (should-not (mevedel-session-control-transfer-drains session)))))

(mevedel-deftest mevedel-session-control-transfer-descriptor ()
  ,test
  (test)
  :doc "publishes semantic transfer actions without view presentation state"
  (let ((session (mevedel-session--create :name "transfer")))
    (setf (mevedel-session-control-transfer session)
          '(:state requested
            :request (:requester-label "/worker")))
    (should
     (equal
      '(:kind control-transfer
        :action grant
        :body "Control transfer requested by /worker  [g]rant  [k]eep"
        :help-echo "Grant or keep the current lease")
      (mevedel-session-control-transfer-descriptor session nil)))
    (setf (mevedel-session-control-transfer session) '(:state released))
    (should
     (equal
      '(:kind control-transfer
        :action request
        :body "Session is read-only  [r]equest control"
        :help-echo "Request cooperative control")
      (mevedel-session-control-transfer-descriptor session t)))))

(provide 'test-mevedel-session-control-transfer)

;;; test-mevedel-session-control-transfer.el ends here
