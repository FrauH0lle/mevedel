;;; test-niwa-view-power.el -- Shared power policy tests -*- lexical-binding: t -*-

;;; Commentary:

;; Deterministic battery.el data and clocks; no real power queries.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view-power)
(eval-when-compile (require 'tramp))

;; `battery'
(defvar battery-status-function)
(defvar battery-update-functions)

(defmacro niwa-view-power-test--without-battery-load (&rest body)
  "Run BODY with `battery' treated as loaded and other features delegated.
A poll requires `battery'; loading it for real initializes D-Bus and
selects the host's backend, so cases bind `battery-status-function'
instead."
  (declare (indent 0))
  (let ((original (make-symbol "require")))
    `(let ((,original (symbol-function 'require)))
       (cl-letf (((symbol-function 'require)
                  (lambda (feature &rest args)
                    (if (eq feature 'battery) 'battery
                      (apply ,original feature args)))))
         ,@body))))

(defmacro niwa-view-power-test--isolated (&rest body)
  "Run BODY with isolated power observation state and cancel its timer.
BODY never loads `battery'."
  (declare (indent 0))
  `(let ((niwa-view-power--watchers (make-hash-table :test #'eq))
         (niwa-view-power--state 'unknown)
         (niwa-view-power--sample-time nil)
         (niwa-view-power--last-query nil)
         (niwa-view-power--timer nil)
         (battery-update-functions
          (copy-sequence (bound-and-true-p battery-update-functions)))
         (window-state-change-functions
          (copy-sequence window-state-change-functions)))
     (unwind-protect
         (niwa-view-power-test--without-battery-load ,@body)
       (maphash (lambda (view _callback)
                  (niwa-view-power-unwatch view))
                (copy-hash-table niwa-view-power--watchers))
       (when niwa-view-power--timer
         (niwa--ui-timer-cancel niwa-view-power--timer)))))

(defun niwa-view-power-test--fire-poll ()
  "Simulate the scheduled timer firing without leaving the old timer active."
  (when niwa-view-power--timer
    (let ((timer niwa-view-power--timer))
      (niwa--ui-timer-cancel timer)
      (niwa-view-power--poll timer))))

(niwa-deftest niwa-view-power--normalize
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
    (should (eq (niwa-view-power--normalize (car case)) (cdr case)))))

(niwa-deftest niwa-view-power-framerate
  (:doc "Automatic stale state saves; overrides and motion off never query")
  (niwa-view-power-test--isolated
    (let* ((now 1000.0) (queries 0)
          (battery-status-function (lambda () (cl-incf queries))))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
        (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
        (should (= 0 (niwa-view-power-framerate 60 0 'auto t)))
        (should (= 20 (niwa-view-power-framerate 20 30 'save t)))
        (should (= 60 (niwa-view-power-framerate 60 0 'full t)))
        (should (= 0 (niwa-view-power-framerate 60 30 'full nil)))
        (niwa-view-power--sample '((?L . "AC") (?B . "charging")))
        (should (= 60 (niwa-view-power-framerate 60 30 'auto t)))
        (setq now 1060.0)
        (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
        (should (= 60 (niwa-view-power-framerate 60 30 'full t)))
        (should (= 0 (niwa-view-power-framerate 60 0 'save t)))
        (should (= 0 queries))))))

(niwa-deftest niwa-view-power-watch
  (:doc "Views share one deferred poll, notifications and callback isolation")
  (niwa-view-power-test--isolated
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
            (niwa-view-power-watch one (lambda () (cl-incf calls)))
            (let ((timer niwa-view-power--timer))
              (should (timerp timer))
              (should (memq #'niwa-view-power--on-window-state-change
                            window-state-change-functions))
              (niwa-view-power-watch
               two (lambda ()
                     (should (eq (current-buffer) two))
                     (cl-incf second-calls)
                     (error "Callback failed")))
              (should (eq timer niwa-view-power--timer))
              (should (= 0 queries))
              (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
              (niwa-view-power-test--fire-poll)
              (should (= 1 queries))
              (should (= 1 calls))
              (should (= 1 second-calls))
              (should (= 60 (niwa-view-power-framerate 60 30 'auto t)))
              (setq now 1005.0)
              (run-hook-with-args 'battery-update-functions
                                  '((?L . "off-line") (?B . "discharging")))
              (should (= 2 calls))
              (should (= 2 second-calls))
              (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
              (should (= 1 queries))
              ;; Both views share the same fallback query budget.
              (niwa-view-power-test--fire-poll)
              (should (= 1 queries))
              (niwa-view-power-unwatch one)
              (should (memq #'niwa-view-power--sample
                            battery-update-functions))
              (should (timerp niwa-view-power--timer))
              (kill-buffer two)
              (should (zerop (hash-table-count niwa-view-power--watchers)))
              (should-not niwa-view-power--timer)
              (should-not (memq #'niwa-view-power--sample
                                battery-update-functions))
              (should-not (memq #'niwa-view-power--on-window-state-change
                                window-state-change-functions))
              (should (memq other battery-update-functions))
              (setq now 1065.0)
              (niwa-view-power-test--fire-poll)
              (should (= 1 queries))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(niwa-deftest niwa-view-power--sample
  (:doc "An unknown notification rearms stale external-power watchers immediately")
  (niwa-view-power-test--isolated
    (let* ((now 1000.5) (calls 0) (queries 0)
           (view (generate-new-buffer " *power-expired-notification*"))
           (battery-status-function (lambda () (cl-incf queries) nil)))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (niwa-view-power-watch view (lambda () (cl-incf calls)))
            (run-hook-with-args 'battery-update-functions
                                '((?L . "AC") (?B . "high")))
            (should (= 1 calls))
            (should (= 60 (niwa-view-power-framerate 60 1 'auto t)))
            (setq now 1060.5)
            (should (= 1 (niwa-view-power-framerate 60 1 'auto t)))
            (run-hook-with-args 'battery-update-functions
                                '((?L . "N/A") (?B . "N/A") (?p . "N/A")))
            (should (= 2 calls))
            (should (= 1 (niwa-view-power-framerate 60 1 'auto t)))
            (should (= 0 queries))
            (should (niwa--ui-timer-pending-p niwa-view-power--timer))
            ;; Repeated unknown notifications do not unnecessarily rearm.
            (run-hook-with-args 'battery-update-functions
                                '((?L . "N/A") (?B . "N/A")))
            (should (= 2 calls))
            (run-hook-with-args 'battery-update-functions
                                '((?L . "AC") (?B . "high")))
            (should (= 3 calls))
            (should (= 60 (niwa-view-power-framerate 60 1 'auto t)))
            (should (= 0 queries)))
        (kill-buffer view)))))

(niwa-deftest niwa-view-power--schedule
  (:doc "A fallback scheduled during TRAMP suspension survives restoration")
  (niwa-view-power-test--isolated
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
              (niwa-view-power-watch one (lambda () (cl-incf calls)))
              (should-not (memq niwa-view-power--timer timer-list))
              (should (niwa--ui-timer-pending-p
                       niwa-view-power--timer)))
            (let ((original niwa-view-power--timer))
              (should (memq original timer-list))
              (niwa-view-power-watch two (lambda () (cl-incf calls)))
              (let ((replacement niwa-view-power--timer))
                (should (eq original replacement))
                (should (= 0 queries))
                (niwa-view-power-watch one (lambda () (cl-incf calls)))
                (should (eq replacement niwa-view-power--timer))
                (should (= 2 (hash-table-count niwa-view-power--watchers)))
                (niwa-view-power-test--fire-poll)
                (should (= 1 queries))
                (should (= 2 calls))
                (should (= 60 (niwa-view-power-framerate 60 0 'auto t)))
                (let ((active niwa-view-power--timer))
                  (niwa-view-power--poll original)
                  (should (eq active niwa-view-power--timer))
                  (should (= 1 queries)))
                (niwa-view-power-unwatch one)
                (niwa-view-power-unwatch two)
                (should-not (memq replacement timer-list))
                (should-not niwa-view-power--timer))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(niwa-deftest niwa-view-power-unwatch
  (:doc "An outer poll remains shared under TRAMP and stops inside its binding")
  (niwa-view-power-test--isolated
    (let* ((one (generate-new-buffer " *power-hidden-one*"))
          (two (generate-new-buffer " *power-hidden-two*"))
          (queries 0)
          (battery-status-function
           (lambda ()
             (cl-incf queries)
             '((?L . "AC") (?B . "high")))))
      (unwind-protect
          (progn
            (niwa-view-power-watch one #'ignore)
            (let ((outer niwa-view-power--timer))
              (should (memq outer timer-list))
              (with-tramp-suspended-timers
                (should-not (niwa--timer-pending-p outer))
                (niwa-view-power-watch two #'ignore)
                (dotimes (_ 5) (niwa-view-power-watch two #'ignore))
                (should (eq outer niwa-view-power--timer))
                (should (niwa--ui-timer-pending-p outer))
                (should-not (memq outer timer-list)))
              (should (memq outer timer-list))
              (niwa-view-power-watch two #'ignore)
              (should (eq outer niwa-view-power--timer))
              (should (= 1 (cl-count-if
                            (lambda (timer)
                              (eq (timer--function timer)
                                  #'niwa-view-power--poll))
                            timer-list)))
              (should (= 0 queries))
              (niwa-view-power-test--fire-poll)
              (should (= 1 queries))
              (niwa-view-power-unwatch one)
              (niwa-view-power-unwatch two)
              (should-not niwa-view-power--timer)
              (should-not
               (cl-find-if (lambda (timer)
                             (eq (timer--function timer)
                                 #'niwa-view-power--poll))
                           timer-list)))
            ;; Repeated suspension must not make replacement timers or retain
            ;; references to timers scheduled on a temporary list.
            (niwa-view-power-watch one #'ignore)
            (dotimes (_ 15)
              (with-tramp-suspended-timers
                (niwa-view-power-watch two #'ignore))
              (niwa-view-power-watch one #'ignore)
              (should (= 1 (cl-count-if
                            (lambda (timer)
                              (eq (timer--function timer)
                                  #'niwa-view-power--poll))
                            timer-list))))
            (let ((last niwa-view-power--timer))
              (with-tramp-suspended-timers
                (niwa-view-power-unwatch one)
                (niwa-view-power-unwatch two)
                (should-not (memq last
                                  (default-toplevel-value 'timer-list))))
              (should-not (memq last timer-list))
              (should-not niwa-view-power--timer)
              (niwa-view-power--poll last)
              (should (= 1 queries))))
        (when (buffer-live-p one) (kill-buffer one))
        (when (buffer-live-p two) (kill-buffer two))))))

(niwa-deftest niwa-view-power--poll
  ()
  ,test
  (test)
  :doc "Failed and missing providers back off without warnings or hot-path reads"
  (niwa-view-power-test--isolated
    (let* ((now 1000.0) (queries 0) (calls 0)
          (view (generate-new-buffer " *power-errors*"))
          (battery-status-function
           (lambda () (cl-incf queries) (error "Provider failed"))))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now)))
            (niwa-view-power-watch view (lambda () (cl-incf calls)))
            (should (= 1 (hash-table-count niwa-view-power--watchers)))
            (should (functionp battery-status-function))
            (niwa-view-power-test--fire-poll)
            (should (= 1 queries))
            (should (= 0 calls))
            (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
            (setq now 1059.0)
            (niwa-view-power-test--fire-poll)
            (should (= 1 queries))
            (setq now 1060.0)
            (niwa-view-power-test--fire-poll)
            (should (= 2 queries))
            (should (= 0 calls)))
        (kill-buffer view))))

  :doc "Initialization failures stay deferred, back off, and recover on a later poll"
  (niwa-view-power-test--isolated
    (let* ((now 1000.0) (loads 0) (queries 0)
           (view (generate-new-buffer " *power-init-errors*"))
           ;; The isolated stand-in: it never loads the real `battery'.
           (load-battery (symbol-function 'require))
           (battery-status-function
            (lambda ()
              (cl-incf queries)
              '((?L . "AC") (?B . "high")))))
      (unwind-protect
          (cl-letf (((symbol-function 'float-time) (lambda (&optional _) now))
                    ((symbol-function 'require)
                     (lambda (feature &rest args)
                       (when (eq feature 'battery)
                         (cl-incf loads)
                         (when (= loads 1)
                           (signal 'inhibited-interaction '("Power initialization"))))
                       (apply load-battery feature args))))
            (niwa-view-power-watch view #'ignore)
            (should (= 0 loads))
            (let ((inhibit-interaction t))
              (niwa-view-power-test--fire-poll))
            (should (= 1 loads))
            (should (= 0 queries))
            (should (= 15 (niwa-view-power-framerate 60 15 'auto t)))
            (should (niwa--ui-timer-pending-p niwa-view-power--timer))
            (setq now 1059.0)
            (niwa-view-power-test--fire-poll)
            (should (= 1 loads))
            (setq now 1060.0)
            (niwa-view-power-test--fire-poll)
            (should (= 2 loads))
            (should (= 1 queries))
            (should (= 60 (niwa-view-power-framerate 60 15 'auto t))))
        (kill-buffer view)))))

(niwa-deftest niwa-view-power--current-state
  (:doc "Expired external samples rearm views even if the backend fails")
  (niwa-view-power-test--isolated
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
            (niwa-view-power-watch view (lambda () (cl-incf calls)))
            (niwa-view-power--sample '((?L . "AC") (?B . "high")))
            (should (= 1 calls))
            (setq now 1060.0)
            (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
            (should (= 0 queries))
            (niwa-view-power-test--fire-poll)
            (should (= 1 queries))
            (should (= 2 calls))
            (should (eq 'unknown (niwa-view-power--current-state)))
            (should (= 30 (niwa-view-power-framerate 60 30 'auto t)))
            (setq now 1120.0 available t)
            (niwa-view-power-test--fire-poll)
            (should (= 2 queries))
            (should (= 3 calls))
            (should (eq 'external (niwa-view-power--current-state)))
            (should (= 60 (niwa-view-power-framerate 60 30 'auto t)))
            (should (= 2 queries)))
        (kill-buffer view)))))

(provide 'test-niwa-view-power)
;;; test-niwa-view-power.el ends here
