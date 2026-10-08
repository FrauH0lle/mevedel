;;; gptel-trace.el --- Count gptel stream reads and pauses -*- lexical-binding: t -*-
;; Case `nopace' never pauses curl, for an unpaced comparison.
(defvar gptel-trace-reads 0)
(defvar gptel-trace-bytes 0)
(defvar gptel-trace-pauses 0)
(defvar gptel-trace-first nil)
(defvar gptel-trace-last nil)
(with-eval-after-load 'mevedel-gptel-stream-bridge
  (when (equal (getenv "MEVEDEL_LAB_CASE") "nopace")
    (setq mevedel-gptel-stream-bridge--pace-backlog-bytes 0))
  (advice-add 'mevedel-gptel-stream-bridge--pace-reading :before
              (lambda (_process output)
                (setq gptel-trace-first (or gptel-trace-first (float-time))
                      gptel-trace-last (float-time))
                (cl-incf gptel-trace-reads)
                (cl-incf gptel-trace-bytes (string-bytes output))))
  (advice-add 'mevedel-transport-pause-process :after-while
              (lambda (&rest _) (cl-incf gptel-trace-pauses) t)))
(defun request-lab-diagnostic-result ()
  "Return bounded gptel stream read statistics."
  (list :reads gptel-trace-reads :bytes gptel-trace-bytes :pauses gptel-trace-pauses
        :seconds (and gptel-trace-first (- gptel-trace-last gptel-trace-first))))
(provide 'gptel-trace)
