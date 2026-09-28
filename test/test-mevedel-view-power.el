;;; test-mevedel-view-power.el -- Shared power policy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Deterministic battery.el data and clocks; no real power queries.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-view-power)
(eval-when-compile (require 'tramp))

(defmacro mevedel-view-power-test--isolated (&rest body)
  "Run BODY with isolated power observation state and cancel its timer."
  (declare (indent 0))
  `(let ((mevedel-view-power--watchers (make-hash-table :test #'eq))
         (mevedel-view-power--state 'unknown)
         (mevedel-view-power--sample-time nil)
         (mevedel-view-power--last-query nil)
         (mevedel-view-power--timer nil)
         (battery-update-functions (copy-sequence battery-update-functions))
         (window-state-change-functions
          (copy-sequence window-state-change-functions)))
     (unwind-protect
         (progn ,@body)
       (maphash (lambda (view _callback)
                  (mevedel-view-power-unwatch view))
                (copy-hash-table mevedel-view-power--watchers))
       (when mevedel-view-power--timer
         (mevedel--ui-timer-cancel mevedel-view-power--timer)))))

(defun mevedel-view-power-test--fire-poll ()
  "Simulate the scheduled timer firing without leaving the old timer active."
  (when mevedel-view-power--timer
    (let ((timer mevedel-view-power--timer))
      (mevedel--ui-timer-cancel timer)
      (mevedel-view-power--poll timer))))

(mevedel-deftest mevedel-view-power--normalize
  (:doc "Line and battery evidence classify upstream variants conservatively")
  (dolist (case '((((76 . "on-line") (66 . "charging")) . external)
                  (((76 . "AC") (66 . "fully-charged")) . external)
                  (((76 . "on-line (USB)") (66 . "high")) . external)
                  (((?L . "connected") (?B . "high")) . external)
                  (((76 . "off-line") (66 . "discharging")) . battery)
                  (((?L . "disconnected") (?B . "low")) . battery)
                  (((?L . "backup power") (?B . "critical")) . battery)
                  (((76 . "BAT") (66 . "high")) . battery)
                  (((76 . "on backup") (66 . "low")) . battery)
                  (((76 . "on-line") (66 . "N/A")) . unknown)
                  (((76 . "unknown") (66 . "charging")) . unknown)
                  (((66 . "charging")) . unknown)
                  (((?L . "AC") . bad) . unknown)
                  ("bad backend" . unknown)
                  (nil . unknown)))
    (should (eq (mevedel-view-power--normalize (car case)) (cdr case)))))

(mevedel-deftest mevedel-view-power-framerate
  (:doc "Automatic stale state saves; overrides and motion off never query")
  (mevedel-view-power-test--isolated
    (let* ((now 1000.0) (queries 0)
          (battery-status-function (lambda () (cl-incf queries))))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
        (should (= 0 (mevedel-view-power-framerate 60 0 'auto t)))
        (should (= 20 (mevedel-view-power-framerate 20 30 'save t)))
        (should (= 60 (mevedel-view-power-framerate 60 0 'full t)))
        (should (= 0 (mevedel-view-power-framerate 60 30 'full nil)))
        (mevedel-view-power--sample '((?L . "AC") (?B . "charging")))
        (should (= 60 (mevedel-view-power-framerate 60 30 'auto t)))
        (setq now 1060.0)
        (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
        (should (= 60 (mevedel-view-power-framerate 60 30 'full t)))
        (should (= 0 (mevedel-view-power-framerate 60 0 'save t)))
        (should (= 0 queries))))))

(mevedel-deftest mevedel-view-power-watch
  (:doc "Views share one deferred poll, notifications and callback isolation")
  (mevedel-view-power-test--isolated
    (let* ((now 1000.0) (queries 0) (calls 0) (second-calls 0)
           (one (generate-new-buffer " *power-one*"))
           (two (generate-new-buffer " *power-two*"))
           (other (lambda (_data) nil))
           (battery-status-function
            (lambda ()
              (cl-incf queries)
              '((?L . "on-line") (?B . "high")))))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (add-hook 'battery-update-functions other)
            (mevedel-view-power-watch one (lambda () (cl-incf calls)))
            (let ((timer mevedel-view-power--timer))
              (should (timerp timer))
              (should (memq #'mevedel-view-power--on-window-state-change
                            window-state-change-functions))
              (mevedel-view-power-watch
               two (lambda ()
                     (should (eq (current-buffer) two))
                     (cl-incf second-calls)
                     (error "Callback failed")))
              (should (eq timer mevedel-view-power--timer))
              (should (= 0 queries))
              (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
              (mevedel-view-power-test--fire-poll)
              (should (= 1 queries))
              (should (= 1 calls))
              (should (= 1 second-calls))
              (should (= 60 (mevedel-view-power-framerate 60 30 'auto t)))
              (setq now 1005.0)
              (run-hook-with-args 'battery-update-functions
                                  '((?L . "off-line") (?B . "discharging")))
              (should (= 2 calls))
              (should (= 2 second-calls))
              (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
              (should (= 1 queries))
              ;; Both views share the same fallback query budget.
              (mevedel-view-power-test--fire-poll)
              (should (= 1 queries))
              (mevedel-view-power-unwatch one)
              (should (memq #'mevedel-view-power--sample
                            battery-update-functions))
              (should (timerp mevedel-view-power--timer))
              (kill-buffer two)
              (should (zerop (hash-table-count mevedel-view-power--watchers)))
              (should-not mevedel-view-power--timer)
              (should-not (memq #'mevedel-view-power--sample
                                battery-update-functions))
              (should-not (memq #'mevedel-view-power--on-window-state-change
                                window-state-change-functions))
              (should (memq other battery-update-functions))
              (setq now 1065.0)
              (mevedel-view-power-test--fire-poll)
              (should (= 1 queries))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(mevedel-deftest mevedel-view-power--schedule
  (:doc "A fallback scheduled during TRAMP suspension survives restoration")
  (mevedel-view-power-test--isolated
    (let* ((one (generate-new-buffer " *power-suspended-one*"))
          (two (generate-new-buffer " *power-suspended-two*"))
          (queries 0)
          (calls 0)
          (battery-status-function
           (lambda ()
             (cl-incf queries)
             '((?L . "AC") (?B . "high")))))
      (unwind-protect
          (progn
            (with-tramp-suspended-timers
              (mevedel-view-power-watch one (lambda () (cl-incf calls)))
              (should-not (memq mevedel-view-power--timer timer-list))
              (should (mevedel--ui-timer-pending-p
                       mevedel-view-power--timer)))
            (let ((original mevedel-view-power--timer))
              (should (memq original timer-list))
              (mevedel-view-power-watch two (lambda () (cl-incf calls)))
              (let ((replacement mevedel-view-power--timer))
                (should (eq original replacement))
                (should (= 0 queries))
                (mevedel-view-power-watch one (lambda () (cl-incf calls)))
                (should (eq replacement mevedel-view-power--timer))
                (should (= 2 (hash-table-count mevedel-view-power--watchers)))
                (mevedel-view-power-test--fire-poll)
                (should (= 1 queries))
                (should (= 2 calls))
                (should (= 60 (mevedel-view-power-framerate 60 0 'auto t)))
                (let ((active mevedel-view-power--timer))
                  (mevedel-view-power--poll original)
                  (should (eq active mevedel-view-power--timer))
                  (should (= 1 queries)))
                (mevedel-view-power-unwatch one)
                (mevedel-view-power-unwatch two)
                (should-not (memq replacement timer-list))
                (should-not mevedel-view-power--timer))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(mevedel-deftest mevedel-view-power-unwatch
  (:doc "An outer poll remains shared under TRAMP and stops inside its binding")
  (mevedel-view-power-test--isolated
    (let* ((one (generate-new-buffer " *power-hidden-one*"))
          (two (generate-new-buffer " *power-hidden-two*"))
          (queries 0)
          (battery-status-function
           (lambda ()
             (cl-incf queries)
             '((?L . "AC") (?B . "high")))))
      (unwind-protect
          (progn
            (mevedel-view-power-watch one #'ignore)
            (let ((outer mevedel-view-power--timer))
              (should (memq outer timer-list))
              (with-tramp-suspended-timers
                (should-not (mevedel--timer-pending-p outer))
                (mevedel-view-power-watch two #'ignore)
                (dotimes (_ 5) (mevedel-view-power-watch two #'ignore))
                (should (eq outer mevedel-view-power--timer))
                (should (mevedel--ui-timer-pending-p outer))
                (should-not (memq outer timer-list)))
              (should (memq outer timer-list))
              (mevedel-view-power-watch two #'ignore)
              (should (eq outer mevedel-view-power--timer))
              (should (= 1 (cl-count-if
                            (lambda (timer)
                              (eq (timer--function timer)
                                  #'mevedel-view-power--poll))
                            timer-list)))
              (should (= 0 queries))
              (mevedel-view-power-test--fire-poll)
              (should (= 1 queries))
              (mevedel-view-power-unwatch one)
              (mevedel-view-power-unwatch two)
              (should-not mevedel-view-power--timer)
              (should-not
               (cl-find-if (lambda (timer)
                             (eq (timer--function timer)
                                 #'mevedel-view-power--poll))
                           timer-list)))
            ;; Repeated suspension must not make replacement timers or retain
            ;; references to timers scheduled on a temporary list.
            (mevedel-view-power-watch one #'ignore)
            (dotimes (_ 15)
              (with-tramp-suspended-timers
                (mevedel-view-power-watch two #'ignore))
              (mevedel-view-power-watch one #'ignore)
              (should (= 1 (cl-count-if
                            (lambda (timer)
                              (eq (timer--function timer)
                                  #'mevedel-view-power--poll))
                            timer-list))))
            (let ((last mevedel-view-power--timer))
              (with-tramp-suspended-timers
                (mevedel-view-power-unwatch one)
                (mevedel-view-power-unwatch two)
                (should-not (memq last
                                  (default-toplevel-value 'timer-list))))
              (should-not (memq last timer-list))
              (should-not mevedel-view-power--timer)
              (mevedel-view-power--poll last)
              (should (= 1 queries))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(mevedel-deftest mevedel-view-power--poll
  (:doc "Failed and missing providers back off without warnings or hot-path reads")
  (mevedel-view-power-test--isolated
    (let* ((now 1000.0) (queries 0) (calls 0)
          (view (generate-new-buffer " *power-errors*"))
          (battery-status-function
           (lambda () (cl-incf queries) (error "Provider failed"))))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (mevedel-view-power-watch view (lambda () (cl-incf calls)))
            (should (= 1 (hash-table-count mevedel-view-power--watchers)))
            (should (functionp battery-status-function))
            (mevedel-view-power-test--fire-poll)
            (should (= 1 queries))
            (should (= 0 calls))
            (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
            (setq now 1059.0)
            (mevedel-view-power-test--fire-poll)
            (should (= 1 queries))
            (setq now 1060.0)
            (mevedel-view-power-test--fire-poll)
            (should (= 2 queries))
            (should (= 0 calls)))
        (kill-buffer view)))))

(mevedel-deftest mevedel-view-power--current-state
  (:doc "Expired external samples rearm views even if the backend fails")
  (mevedel-view-power-test--isolated
    (let* ((now 1000.0) (calls 0) (queries 0) (available nil)
           (view (generate-new-buffer " *power-stale*"))
           (battery-status-function
            (lambda ()
              (cl-incf queries)
              (if available
                  '((?L . "AC") (?B . "high"))
                (error "Unavailable")))))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (mevedel-view-power-watch view (lambda () (cl-incf calls)))
            (mevedel-view-power--sample '((?L . "AC") (?B . "high")))
            (should (= 1 calls))
            (setq now 1060.0)
            (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
            (should (= 0 queries))
            (mevedel-view-power-test--fire-poll)
            (should (= 1 queries))
            (should (= 2 calls))
            (should (eq 'unknown (mevedel-view-power--current-state)))
            (should (= 30 (mevedel-view-power-framerate 60 30 'auto t)))
            (setq now 1120.0 available t)
            (mevedel-view-power-test--fire-poll)
            (should (= 2 queries))
            (should (= 3 calls))
            (should (eq 'external (mevedel-view-power--current-state)))
            (should (= 60 (mevedel-view-power-framerate 60 30 'auto t)))
            (should (= 2 queries)))
        (kill-buffer view)))))

(provide 'test-mevedel-view-power)
;;; test-mevedel-view-power.el ends here
