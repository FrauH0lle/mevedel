;;; test-mevedel-execution-stop.el --- User execution stop tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests user-authority settlement and mailbox delivery.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-sandbox)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-execution-test-helpers"))

(mevedel-deftest mevedel-execution-stop-user ()
  ,test
  (test)
  :doc "delivers through the mailbox and retires without a model claim"
  (let* ((root (make-temp-file "mevedel-managed-user-stop-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         events initial id)
    (unwind-protect
        (let ((mevedel-execution-event-functions
               (list (lambda (event) (push event events))))
              (mevedel-execution-mailbox-delivery-function
               (lambda (_event _context) t)))
          (setq initial
                (test-mevedel-execution--start-managed
                 session root '("sh" "-c" "printf ready; sleep 30")))
          (setq id (plist-get (plist-get initial :facts) :execution-id))
          (should (mevedel-execution-stop-user session id))
          (test-mevedel-execution--wait
           (lambda ()
             (cl-find-if
              (lambda (event)
                (eq (plist-get event :type) 'terminal))
              events)))
          (let ((event (cl-find-if
                        (lambda (candidate)
                          (eq (plist-get candidate :type) 'terminal))
                        events)))
            (should (eq 'mailbox (plist-get event :delivery)))
            (should (eq 'stopped
                        (plist-get (plist-get event :facts) :termination))))
          (should-not (mevedel-execution-list session "main")))
      (delete-directory root t))))

(provide 'test-mevedel-execution-stop)
;;; test-mevedel-execution-stop.el ends here
