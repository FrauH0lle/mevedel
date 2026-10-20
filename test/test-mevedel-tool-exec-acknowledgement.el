;;; test-mevedel-tool-exec-acknowledgement.el -- Tests for acknowledged mailbox delivery -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the acknowledgement contract of
;; `mevedel-tool-exec-handle-execution-event'.

;;; Code:

(require 'cl-lib)
(require 'mevedel-agent-control)
(require 'mevedel-structs)
(require 'mevedel-session-publication)
(require 'mevedel-tool-exec)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-tool-exec-ack--event (&optional facts)
  "Return a mailbox terminal event for the root with FACTS."
  (list :type 'terminal :delivery 'mailbox :owner "/root"
        :facts (or facts '(:execution-id "e1" :termination exited))
        :observation '(:output "done")))

(mevedel-deftest mevedel-tool-exec-handle-execution-event/acknowledgement ()
  ,test
  (test)
  :doc "a pending delivery acks and wakes only when the mailbox secures it"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil) secured)
    (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
               (lambda (&rest args) (setq secured (nth 6 args)) 'pending))
              ((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (push 'wake log))))
      (should
       (eq 'pending
           (mevedel-tool-exec-handle-execution-event
            (test-mevedel-tool-exec-ack--event) session
            (lambda (success error) (push (list 'ack success error) log)))))
      (should-not log)
      (funcall secured t nil)
      ;; Bookkeeping settles before the model is woken.
      (should (equal '((ack t nil) wake) (reverse log)))))
  :doc "a failed acknowledgement forwards the failure and never wakes"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil) secured)
    (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
               (lambda (&rest args) (setq secured (nth 6 args)) 'pending))
              ((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (push 'wake log))))
      (mevedel-tool-exec-handle-execution-event
       (test-mevedel-tool-exec-ack--event) session
       (lambda (success error) (push (list 'ack success error) log)))
      (funcall secured nil "Disk full")
      (should (equal '((ack nil "Disk full")) log))))
  :doc "a stopped command acknowledges success without waking"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil) secured)
    (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
               (lambda (&rest args) (setq secured (nth 6 args)) 'pending))
              ((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (push 'wake log))))
      (mevedel-tool-exec-handle-execution-event
       (test-mevedel-tool-exec-ack--event
        '(:execution-id "e1" :termination stopped))
       session
       (lambda (success error) (push (list 'ack success error) log)))
      (funcall secured t nil)
      (should (equal '((ack t nil)) log))))
  :doc "a session that is stopping acknowledges success without waking"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil) secured)
    (setf (mevedel-session-publication-stopping-p session) t)
    (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
               (lambda (&rest args) (setq secured (nth 6 args)) 'pending))
              ((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (push 'wake log))))
      (mevedel-tool-exec-handle-execution-event
       (test-mevedel-tool-exec-ack--event) session
       (lambda (success error) (push (list 'ack success error) log)))
      (funcall secured t nil)
      (should (equal '((ack t nil)) log))))
  :doc "a failing wake is contained after the acknowledgement"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil) secured)
    (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-execution-result)
               (lambda (&rest args) (setq secured (nth 6 args)) 'pending))
              ((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (error "No timers"))))
      (mevedel-tool-exec-handle-execution-event
       (test-mevedel-tool-exec-ack--event) session
       (lambda (success error) (push (list 'ack success error) log)))
      (mevedel-test--with-captured-diagnostics nil
        (funcall secured t nil))
      (should (equal '((ack t nil)) log))))
  :doc "monitor batches and terminals pass the acknowledgement to the monitor mailbox"
  (dolist (type '(monitor-batch terminal))
    (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
          secured result)
      (cl-letf (((symbol-function 'mevedel-agent-control-enqueue-monitor-events)
                 (lambda (&rest args) (setq secured (nth 5 args)) 'pending))
                ((symbol-function 'mevedel-pending-inputs-offer-root-event)
                 #'ignore))
        (setq result
              (mevedel-tool-exec-handle-execution-event
               (list :type type :delivery 'mailbox :owner "/root"
                     :facts '(:execution-id "m1" :description "w" :kind monitor
                              :termination exited)
                     :monitor-lines '("a") :monitor-omitted 0
                     :observation '(:output ""))
               session #'ignore))
        (should (eq 'pending result))
        (should (functionp secured)))))
  :doc "an unsupported event acknowledges failure and returns nil"
  (let (acks)
    (should-not
     (mevedel-tool-exec-handle-execution-event
      '(:type terminal :delivery model) nil
      (lambda (success error) (push (list success error) acks))))
    (should (equal '((nil "Unsupported mailbox event")) acks))))

(defun test-mevedel-tool-exec-ack--wait (predicate)
  "Pump the event loop until PREDICATE holds, failing after five seconds."
  (with-timeout (5 (error "Timed out"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.02))))

(defun test-mevedel-tool-exec-ack--with-busy-session (function)
  "Call FUNCTION with a session whose real gate holds one earlier preparation.
FUNCTION receives the session, a live root buffer, a second buffer and a
log; the earlier preparation pushes `earlier' onto the log when it runs."
  (let* ((directory (make-temp-file "mevedel-tool-exec-ack-" t))
         (root (generate-new-buffer " *tool-exec-ack-root*"))
         (other (generate-new-buffer " *tool-exec-ack-other*"))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :name "test"
                   :save-path (file-name-as-directory directory)))
         (log nil))
    (unwind-protect
        (progn
          (mevedel-session-set-root-buffer session root)
          (setf (mevedel-session-publication-preparations session)
                (list (cons (lambda (_token) (push 'earlier log)) nil)))
          (funcall function session root other (lambda () log)
                   (lambda (entry) (push entry log))))
      (mevedel-agent-control--cancel-notifications session)
      (setf (mevedel-session-publication-preparations session) nil)
      (kill-buffer root)
      (kill-buffer other)
      (delete-directory directory t))))

(mevedel-deftest mevedel-tool-exec--secured-acknowledgement ()
  ,test
  (test)
  :doc "an idle session wakes in place once, after the acknowledgement"
  (let ((root (generate-new-buffer " *tool-exec-ack-root*"))
        (session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil))
    (unwind-protect
        (progn
          (mevedel-session-set-root-buffer session root)
          (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
                     (lambda (&rest args) (push (cons 'wake args) log))))
            (funcall (mevedel-tool-exec--secured-acknowledgement
                      (lambda (success error) (push (list 'ack success error) log))
                      session root t)
                     t nil))
          (should (equal (list '(ack t nil) (list 'wake session root))
                         (reverse log)))
          (should-not (mevedel-session-publication-preparations session)))
      (kill-buffer root)))
  :doc "a failed acknowledgement or a non-waking event never offers the root"
  (let ((session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        (log nil))
    (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
               (lambda (&rest _) (push 'wake log))))
      (funcall (mevedel-tool-exec--secured-acknowledgement
                #'ignore session nil t)
               nil "Disk full")
      (funcall (mevedel-tool-exec--secured-acknowledgement
                #'ignore session nil nil)
               t nil))
    (should-not log))
  :doc "a busy gate defers the wake to the notifier, not the preparation queue"
  (test-mevedel-tool-exec-ack--with-busy-session
   (lambda (session root _other log push-log)
     (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
                (lambda (&rest args) (funcall push-log (cons 'wake args)))))
       (funcall (mevedel-tool-exec--secured-acknowledgement
                 (lambda (success error) (funcall push-log (list 'ack success error)))
                 session root t)
                t nil)
       ;; Settled now; the wake waits in the notifier and the gate's queue
       ;; holds only its own earlier preparation.
       (should (equal '((ack t nil)) (reverse (funcall log))))
       (should (= 1 (length (mevedel-session-publication-preparations session))))
       (should (= 1 (length (mevedel-session-mail-notifications session))))
       (mevedel-session-publication--schedule session)
       (test-mevedel-tool-exec-ack--wait
        (lambda () (cl-find-if (lambda (entry) (eq 'wake (car-safe entry)))
                               (funcall log))))
       ;; No token or transaction crosses: the offer receives the session
       ;; and the originating buffer only.
       (should (equal (list '(ack t nil) 'earlier (list 'wake session root))
                      (reverse (funcall log))))
       (should-not (mevedel-session-mail-notifications session)))))
  :doc "offers for one buffer coalesce while offers for other buffers stay ordered"
  (test-mevedel-tool-exec-ack--with-busy-session
   (lambda (session root other log push-log)
     (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
                (lambda (_session buffer) (funcall push-log (list 'wake buffer)))))
       (dolist (buffer (list root root other))
         (funcall (mevedel-tool-exec--secured-acknowledgement
                   #'ignore session buffer t)
                  t nil))
       (should (= 2 (length (mevedel-session-mail-notifications session))))
       (mevedel-session-publication--schedule session)
       (test-mevedel-tool-exec-ack--wait
        (lambda () (= 2 (cl-count-if (lambda (entry) (eq 'wake (car-safe entry)))
                                     (funcall log)))))
       (should (equal (list 'earlier (list 'wake root) (list 'wake other))
                      (reverse (funcall log)))))))
  :doc "a session that began stopping before the gate opened is never woken"
  (test-mevedel-tool-exec-ack--with-busy-session
   (lambda (session root _other log push-log)
     (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
                (lambda (&rest _) (funcall push-log 'wake))))
       (funcall (mevedel-tool-exec--secured-acknowledgement
                 #'ignore session root t)
                t nil)
       (setf (mevedel-session-publication-stopping-p session) t)
       (mevedel-session-publication--schedule session)
       (test-mevedel-tool-exec-ack--wait
        (lambda () (not (mevedel-session-publication-busy-p session))))
       (accept-process-output nil 0.2)
       ;; Nothing was offered and no notification timer is left armed.
       (should (equal '(earlier) (reverse (funcall log))))
       (should-not (mevedel-session-mail-notification-timer session)))))
  :doc "a failing wake is a diagnostic and the secured acknowledgement stands"
  (let ((root (generate-new-buffer " *tool-exec-ack-root*"))
        (session (mevedel-session--create :authority-mode 'pid-lock :name "test"))
        acks warned)
    (unwind-protect
        (progn
          (mevedel-session-set-root-buffer session root)
          (cl-letf (((symbol-function 'mevedel-pending-inputs-offer-root-event)
                     (lambda (&rest _) (error "No timers"))))
            (mevedel-test--with-captured-diagnostics warned
              (funcall (mevedel-tool-exec--secured-acknowledgement
                        (lambda (success error) (push (list success error) acks))
                        session root t)
                       t nil)))
          (should (equal '((t nil)) acks))
          (should (string-match-p "No timers" (format "%s" warned))))
      (kill-buffer root))))

(provide 'test-mevedel-tool-exec-acknowledgement)
;;; test-mevedel-tool-exec-acknowledgement.el ends here
