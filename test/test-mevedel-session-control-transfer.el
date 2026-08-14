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

(require 'mevedel-transport)

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

(mevedel-deftest mevedel-session-control-transfer-poll ()
  ,test
  (test)
  :doc "defers target I/O while the transport or a publication is busy"
  (let ((session (mevedel-session--create :name "transfer"))
        (owner 0)
        (requester 0))
    (cl-letf (((symbol-function
                'mevedel-session-control-transfer--poll-owner)
               (lambda (_session) (cl-incf owner) 'owner))
              ((symbol-function
                'mevedel-session-control-transfer--poll-requester)
               (lambda (_session _buffer) (cl-incf requester) 'requester)))
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (eq 'requester
                  (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      ;; Emacs runs filters and timers wherever the main loop waits,
      ;; including inside a TRAMP operation; polling from there wedges the
      ;; operation already running.
      (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                 (lambda (&optional _path) t)))
        (should-not
         (mevedel-session-control-transfer-poll session nil nil))
        (should-not
         (mevedel-session-control-transfer-poll session nil t)))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) t)
      (should-not (mevedel-session-control-transfer-poll session nil nil))
      (should-not (mevedel-session-control-transfer-poll session nil t))
      (should (= 1 owner))
      (should (= 1 requester))
      (setf (mevedel-session-publication-active-p session) nil)
      (should (eq 'owner
                  (mevedel-session-control-transfer-poll session nil nil)))
      (should (= 2 owner)))))

(provide 'test-mevedel-session-control-transfer)

;;; test-mevedel-session-control-transfer.el ends here
