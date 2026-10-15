;;; test-mevedel-sleep.el --- System sleep inhibitor tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers reason collection and the inhibitor process lifecycle.  A stand-in
;; command records the announced reasons in a file and then blocks on stdin
;; like the real inhibitor, so no test takes a real system sleep lock.

;;; Code:

(require 'mevedel-sleep)
(require 'mevedel-execution)
(require 'mevedel-turn)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro test-mevedel-sleep--with-state (&rest body)
  "Run BODY with private inhibitor state, releasing everything afterwards."
  (declare (indent 0) (debug (body)))
  `(let ((mevedel-sleep--timer nil)
         (mevedel-sleep--process nil)
         (mevedel-sleep--held-reasons nil)
         (mevedel-sleep--failed nil)
         (mevedel--warn-once-table (make-hash-table :test #'equal))
         (mevedel-sleep-inhibit-command mevedel-sleep-inhibit-command))
     (unwind-protect (progn ,@body)
       (mevedel-sleep-stop))))

(defun test-mevedel-sleep--recording-command (file)
  "Return an inhibitor stand-in that writes its reasons to FILE."
  (list "sh" "-c" "echo \"$1\" > \"$2.tmp\" && mv \"$2.tmp\" \"$2\"; exec cat"
        "inhibitor" "%s" file))

(defun test-mevedel-sleep--wait (predicate)
  "Wait up to five seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun test-mevedel-sleep--announced (file)
  "Return the reasons the stand-in wrote to FILE, or nil."
  (and (file-exists-p file)
       (string-trim (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))

(mevedel-deftest mevedel-sleep--reasons ()
  ,test
  (test)
  :doc "is empty when nothing runs"
  (cl-letf (((symbol-function 'mevedel-busy-p) #'ignore)
            ((symbol-function 'mevedel-execution-live-p) #'ignore)
            ((symbol-function 'mevedel-collaboration-sleep-reasons) #'ignore))
    (should-not (mevedel-sleep--reasons)))

  :doc "names a running turn, a live command and collaboration in order"
  (with-temp-buffer
    (setq-local mevedel--turn-settlements-pending '(settlement))
    (cl-letf (((symbol-function 'mevedel-execution-live-p) #'always)
              ((symbol-function 'mevedel-collaboration-sleep-reasons)
               (lambda (now)
                 (should (floatp now))
                 (list "collaboration share active"))))
      (should (equal '("request running" "command running"
                       "collaboration share active")
                     (mevedel-sleep--reasons)))))

  :doc "a failing reason source counts as no reason and warns"
  (let ((mevedel--warn-once-table (make-hash-table :test #'equal))
        diagnostics)
    (cl-letf (((symbol-function 'mevedel-busy-p) #'always)
              ((symbol-function 'mevedel-execution-live-p)
               (lambda () (error "Registry corrupt"))))
      (mevedel-test--with-captured-diagnostics diagnostics
        (should-not (mevedel-sleep--reasons))))
    (should (string-match-p "Cannot determine whether to block system sleep: \
Registry corrupt"
                            diagnostics))))

(mevedel-deftest mevedel-sleep--reconcile ()
  ,test
  (test)
  :doc "holds, replaces and releases the inhibitor as reasons change"
  (test-mevedel-sleep--with-state
    (let* ((directory (make-temp-file "mevedel-sleep-" t))
           (file (file-name-concat directory "why"))
           (reasons '("request running"))
           (sentinel (symbol-function 'mevedel-sleep--sentinel))
           sentinel-processes
           first)
      (unwind-protect
          (cl-letf (((symbol-function 'mevedel-sleep--reasons)
                     (lambda () reasons))
                    ((symbol-function 'mevedel-sleep--sentinel)
                     (lambda (process event)
                       (push process sentinel-processes)
                       (funcall sentinel process event))))
            (setq mevedel-sleep-inhibit-command
                  (test-mevedel-sleep--recording-command file))
            (mevedel-sleep--reconcile)
            (setq first mevedel-sleep--process)
            (should (process-live-p first))
            (should (timerp mevedel-sleep--timer))
            (test-mevedel-sleep--wait
             (lambda () (equal "request running"
                               (test-mevedel-sleep--announced file))))
            ;; An unchanged reason set keeps the same process.
            (mevedel-sleep--reconcile)
            (should (eq first mevedel-sleep--process))
            (delete-file file)
            (setq reasons '("request running" "command running"))
            (mevedel-sleep--reconcile)
            (should (process-live-p mevedel-sleep--process))
            (should-not (eq first mevedel-sleep--process))
            (should-not (process-live-p first))
            (test-mevedel-sleep--wait
             (lambda () (equal "request running, command running"
                               (test-mevedel-sleep--announced file))))
            ;; The replaced process's sentinel must not disown or fail its
            ;; successor.
            (test-mevedel-sleep--wait
             (lambda () (memq first sentinel-processes)))
            (should (process-live-p mevedel-sleep--process))
            (should-not mevedel-sleep--failed)
            (should (equal reasons mevedel-sleep--held-reasons))
            (let ((held mevedel-sleep--process))
              (setq reasons nil)
              (mevedel-sleep--reconcile)
              (should-not mevedel-sleep--process)
              (should-not mevedel-sleep--held-reasons)
              (should-not mevedel-sleep--timer)
              (should-not (process-live-p held))))
        (delete-directory directory t))))

  :doc "releases a held inhibitor once inhibition is disabled"
  (test-mevedel-sleep--with-state
    (cl-letf (((symbol-function 'mevedel-sleep--reasons)
               (lambda () '("request running"))))
      (setq mevedel-sleep-inhibit-command '("cat"))
      (mevedel-sleep--reconcile)
      (let ((held mevedel-sleep--process))
        (should (process-live-p held))
        (setq mevedel-sleep-inhibit-command nil)
        (mevedel-sleep--reconcile)
        (should-not mevedel-sleep--process)
        (should-not mevedel-sleep--timer)
        (should-not (process-live-p held)))))

  :doc "a failing inhibitor warns once and is retried only in a new episode"
  (test-mevedel-sleep--with-state
    (let ((reasons '("request running"))
          diagnostics)
      (cl-letf (((symbol-function 'mevedel-sleep--reasons)
                 (lambda () reasons)))
        (setq mevedel-sleep-inhibit-command
              '("sh" "-c" "echo access denied; exit 1"))
        (mevedel-test--with-captured-diagnostics diagnostics
          (mevedel-sleep--reconcile)
          (test-mevedel-sleep--wait (lambda () mevedel-sleep--failed)))
        (should (string-match-p "Cannot block system sleep: .*access denied"
                                diagnostics))
        (should-not mevedel-sleep--process)
        (mevedel-sleep--reconcile)
        (should-not mevedel-sleep--process)
        (should (timerp mevedel-sleep--timer))
        (setq reasons nil)
        (mevedel-sleep--reconcile)
        (should-not mevedel-sleep--failed)
        (should-not mevedel-sleep--timer)
        (setq reasons '("request running")
              mevedel-sleep-inhibit-command '("cat"))
        (mevedel-sleep--reconcile)
        (should (process-live-p mevedel-sleep--process)))))

  :doc "a missing inhibitor program warns instead of signaling"
  (test-mevedel-sleep--with-state
    (let (diagnostics)
      (cl-letf (((symbol-function 'mevedel-sleep--reasons)
                 (lambda () '("request running"))))
        (setq mevedel-sleep-inhibit-command
              (list (file-name-concat temporary-file-directory
                                      "mevedel-no-such-inhibitor")))
        (mevedel-test--with-captured-diagnostics diagnostics
          (mevedel-sleep--reconcile))
        (should (string-match-p "Cannot block system sleep" diagnostics))
        (should mevedel-sleep--failed)
        (should-not mevedel-sleep--process)))))

(mevedel-deftest mevedel-sleep-update ()
  ,test
  (test)
  :doc "does nothing while inhibition is disabled"
  (test-mevedel-sleep--with-state
    (setq mevedel-sleep-inhibit-command nil)
    (mevedel-sleep-update 'session 'buffer)
    (should-not mevedel-sleep--timer))

  :doc "coalesces repeated calls into one pending evaluation"
  (test-mevedel-sleep--with-state
    (setq mevedel-sleep-inhibit-command '("cat"))
    (mevedel-sleep-update)
    (let ((first mevedel-sleep--timer))
      (mevedel-sleep-update)
      (should (timerp mevedel-sleep--timer))
      (should-not (memq first timer-list))
      (should (memq mevedel-sleep--timer timer-list))))

  :doc "leaves a held inhibitor to the poll"
  (test-mevedel-sleep--with-state
    (cl-letf (((symbol-function 'mevedel-sleep--reasons)
               (lambda () '("request running"))))
      (setq mevedel-sleep-inhibit-command '("cat"))
      (mevedel-sleep--reconcile)
      (let ((poll mevedel-sleep--timer))
        (mevedel-sleep-update)
        (should (eq poll mevedel-sleep--timer))
        (should (memq poll timer-list))))))

(mevedel-deftest mevedel-sleep-stop ()
  ,test
  (test)
  :doc "releases the inhibitor and cancels the poll"
  (test-mevedel-sleep--with-state
    (cl-letf (((symbol-function 'mevedel-sleep--reasons)
               (lambda () '("command running"))))
      (setq mevedel-sleep-inhibit-command '("cat"))
      (mevedel-sleep--reconcile)
      (let ((held mevedel-sleep--process)
            (timer mevedel-sleep--timer))
        (mevedel-sleep-stop)
        (should-not mevedel-sleep--process)
        (should-not mevedel-sleep--timer)
        (should-not (memq timer timer-list))
        (should-not (process-live-p held))))))

(provide 'test-mevedel-sleep)
;;; test-mevedel-sleep.el ends here
