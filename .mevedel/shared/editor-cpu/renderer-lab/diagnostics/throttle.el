;;; throttle.el --- Pause curl between stream batches (experiment) -*- lexical-binding: t -*-
;; After each read, stop the curl process and continue it after
;; `throttle-interval'.  The kernel socket buffer holds what the server sends
;; meanwhile, so Emacs reads in bursts instead of once per chunk.
(require 'gptel-request)
(defvar throttle-interval
  (string-to-number (or (getenv "MEVEDEL_LAB_THROTTLE") "0.2")))
(defvar throttle-stops 0)
(defun throttle--filter (process &rest _)
  (when (and (eq (process-status process) 'run)
             (not (process-get process 'throttle-timer)))
    (signal-process process 'SIGSTOP)
    (setq throttle-stops (1+ throttle-stops))
    (process-put process 'throttle-timer
                 (run-at-time throttle-interval nil
                              (lambda ()
                                (process-put process 'throttle-timer nil)
                                (when (process-live-p process)
                                  (signal-process process 'SIGCONT)))))))
(defun throttle--sentinel (fn process status)
  ;; Stop and continue are not completion.
  (unless (memq (process-status process) '(stop run))
    (funcall fn process status)))
(when (equal (getenv "MEVEDEL_LAB_CASE") "throttle")
  (advice-add 'gptel-curl--stream-filter :after #'throttle--filter)
  (advice-add 'gptel-curl--stream-cleanup :around #'throttle--sentinel))
(defun request-lab-diagnostic-result () (list :stops throttle-stops))
(provide 'throttle)
