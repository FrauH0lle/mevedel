;;; test-mevedel-execution-stop.el --- User execution stop tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests session-wide user projection, control, and settlement.

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

(mevedel-deftest mevedel-execution-list-user ()
  ,test
  (test)
  :doc "lists foreground and yielded executions across model owners"
  (let* ((root (make-temp-file "mevedel-managed-user-list-" t))
         (session (test-mevedel-execution--session root))
         (origin-buffer (current-buffer))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         changes
         (mevedel-execution-state-change-hook
          (list (lambda (seen-session seen-data-buffer)
                  (should (eq session seen-session))
                  (should (eq origin-buffer seen-data-buffer))
                  (setq changes (1+ (or changes 0))))))
         callbacks helper-done)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (push value callbacks))
           :session session :data-buffer origin-buffer
           :owner "main" :owner-context session
           :command '("sh" "-c" "printf main; sleep 30")
           :tool-args '(:command "printf main; sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :read-only-p t :yield-time-ms 10000)
          (mevedel-execution-start-bash
           (lambda (value) (push value callbacks))
           :session session :data-buffer origin-buffer
           :owner "explorer--one"
           :owner-context 'agent-context
           :command '("sh" "-c" "printf agent; sleep 30")
           :tool-args '(:command "printf agent; sleep 30")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :read-only-p t :yield-time-ms 10)
          (mevedel-execution-start-one-shot
           (lambda (_result) (setq helper-done t))
           :name "test-helper" :command '("sh" "-c" "sleep 0.2")
           :workdir root :writable-roots (list root) :session session)
          (test-mevedel-execution--wait
           (lambda () (cl-find-if
                       (lambda (value)
                         (plist-get (plist-get value :facts) :execution-id))
                       callbacks)))
          (let ((items (mevedel-execution-list-user session)))
            (should (= 2 (length items)))
            (should (= 2 (mevedel-execution-count-user session)))
            (should (equal '("explorer--one" "main")
                           (sort (mapcar (lambda (item)
                                           (plist-get item :owner))
                                         items)
                                 #'string<)))
            (dolist (item items)
              (should (stringp (plist-get item :execution-id)))
              (should (eq 'running (plist-get item :state)))
              (should (numberp (plist-get item :wall-time-seconds)))
              (should (numberp (plist-get item :output-bytes)))
              (should (stringp (plist-get item :output-tail)))
              (should (file-exists-p (plist-get item :artifact-path))))
            (should
             (equal '("printf agent; sleep 30" "printf main; sleep 30")
                    (sort (mapcar (lambda (item)
                                    (plist-get item :command))
                                  items)
                          #'string<)))
            (let* ((main
                    (cl-find "main" items :key (lambda (item)
                                                  (plist-get item :owner))
                             :test #'equal))
                   (command (plist-get main :command))
                   (artifact (plist-get main :artifact-path)))
              (aset command 0 ?X)
              (aset artifact 0 ?X)
              (setq items (mevedel-execution-list-user session))
              (setq main
                    (cl-find "main" items :key (lambda (item)
                                                  (plist-get item :owner))
                             :test #'equal))
              (should (equal "printf main; sleep 30"
                             (plist-get main :command)))
              (should (file-exists-p (plist-get main :artifact-path))))
            (dolist (item items)
              (mevedel-execution-stop-user
               session (plist-get item :execution-id))))
          (test-mevedel-execution--wait
           (lambda () (zerop (mevedel-execution-count-user session))))
          (test-mevedel-execution--wait (lambda () helper-done))
          (should (= changes 4))
          (should-not (mevedel-execution-list-user session)))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-count-user ()
  ,test
  (test)
  :doc "counts only live managed Bash executions"
  (let* ((root (make-temp-file "mevedel-managed-user-count-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         done)
    (unwind-protect
        (progn
          (should (zerop (mevedel-execution-count-user session)))
          (mevedel-execution-start-bash
           (lambda (_value) (setq done t))
           :session session :owner "main" :owner-context session
           :command '("sh" "-c" "sleep 0.1")
           :tool-args '(:command "sleep 0.1")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (should (= 1 (mevedel-execution-count-user session)))
          (test-mevedel-execution--wait (lambda () done))
          (should (zerop (mevedel-execution-count-user session))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-write-user ()
  ,test
  (test)
  :doc "writes to foreground PTY executions without model ownership"
  (let* ((root (make-temp-file "mevedel-managed-user-control-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         terminal)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq terminal value))
           :session session :owner "explorer--pty"
           :owner-context 'agent-context
           :command '("sh" "-c" "read line; printf 'got:%s\\n' \"$line\"; sleep 30")
           :tool-args '(:command "read line; printf; sleep")
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :tty t :yield-time-ms 10000)
          (test-mevedel-execution--wait
           (lambda () (mevedel-execution-list-user session)))
          (let* ((item (car (mevedel-execution-list-user session)))
                 (id (plist-get item :execution-id)))
            (should (plist-get item :tty))
            (should (mevedel-execution-write-user session id "hello\n"))
            (test-mevedel-execution--wait
             (lambda ()
               (string-match-p
                "got:hello"
                (plist-get (car (mevedel-execution-list-user session))
                           :output-tail))))
            (should (mevedel-execution-stop-user session id))
            (test-mevedel-execution--wait (lambda () terminal))
            (should (eq 'stopped
                        (plist-get (plist-get terminal :facts)
                                   :termination)))))
      (delete-directory root t))))

(mevedel-deftest mevedel-execution-interrupt-user ()
  ,test
  (test)
  :doc "interrupts running work and rejects queued work that has not started"
  (let* ((root (make-temp-file "mevedel-managed-user-interrupt-" t))
         (session (test-mevedel-execution--session root))
         (mevedel-sandbox-mode 'off)
         (mevedel-execution--child-kill-delay 0.05)
         callbacks)
    (unwind-protect
        (progn
          (dolist (owner '("main" "explorer--queued"))
            (mevedel-execution-start-bash
             (lambda (value) (push value callbacks))
             :session session :owner owner :owner-context session
             :command '("sh" "-c" "sleep 30")
             :tool-args (list :command (format "sleep 30 # %s" owner))
             :workdir root :writable-roots (list root)
             :artifact-directory (file-name-concat root "artifacts")
             :yield-time-ms 10000))
          (let* ((items (mevedel-execution-list-user session))
                 (running (cl-find "main" items
                                   :key (lambda (item)
                                          (plist-get item :owner))
                                   :test #'equal))
                 (queued (cl-find "explorer--queued" items
                                  :key (lambda (item)
                                         (plist-get item :owner))
                                  :test #'equal)))
            (should (eq 'running (plist-get running :state)))
            (should (eq 'queued (plist-get queued :state)))
            (should-error
             (mevedel-execution-interrupt-user
              session (plist-get queued :execution-id))
             :type 'mevedel-execution-input-error)
            (mevedel-execution-stop-user
             session (plist-get queued :execution-id))
            (should (mevedel-execution-interrupt-user
                     session (plist-get running :execution-id))))
          (test-mevedel-execution--wait
           (lambda () (zerop (mevedel-execution-count-user session))))
          (should (= 2 (length callbacks))))
      (delete-directory root t))))

(provide 'test-mevedel-execution-stop)
;;; test-mevedel-execution-stop.el ends here
