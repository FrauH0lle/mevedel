;;; stream-profile.el --- Profile a streaming request -*- lexical-binding: t -*-
;; Diagnostic for request-run.py: profile CPU from 3 s to 11 s after the
;; request starts and write a bounded summary to $MEVEDEL_LAB_PROFILE.
(require 'profiler)
(load (expand-file-name "cpuh-prof.el" (getenv "MEVEDEL_LAB_PROF_DIR")) nil t)
(declare-function cpuh-prof--dump "cpuh-prof" (out &optional profile))
(defun stream-profile--start (&rest _)
  (run-at-time 3 nil (lambda () (profiler-start 'cpu)))
  (run-at-time 11 nil
               (lambda ()
                 (profiler-stop)
                 (cpuh-prof--dump (getenv "MEVEDEL_LAB_PROFILE")
                                  (profiler-cpu-profile)))))
(advice-add 'request-lab-start :after #'stream-profile--start)
(provide 'stream-profile)
