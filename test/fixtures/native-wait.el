;;; native-wait.el --- Isolated native condition wait checks -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ert)
(require 'mevedel-native)
(require 'mevedel-structs)

(defvar mevedel-test-native-root)

(let ((mevedel-user-dir mevedel-test-native-root))
  (should (mevedel-native-load))
  (should (mevedel-native--timed-wait-available-p)))

(cl-labels
    ((await (predicate)
       (let ((limit (+ (float-time) 3)))
         (while (and (not (funcall predicate)) (< (float-time) limit))
           (sleep-for 0.001))
         (should (funcall predicate))))
     (stop (thread)
       (when (and thread (thread-live-p thread))
         (thread-signal thread 'error '("fixture cleanup")))
       (when thread (ignore-errors (thread-join thread)))))
  ;; Automatic GC before the primitive must not let a hook steal the
  ;; deadline with an unrelated condition wait.  This hangs without the
  ;; native-owned guard, even when condition-wait's subr is captured.
  (let ((mutex (make-mutex)) worker completed injected (hooks 0))
    (unwind-protect
        (progn
          (setq worker
                (make-thread
                 (lambda ()
                   (let* ((gc-cons-threshold 8000)
                          (gc-cons-percentage 0.0)
                          (condition (make-condition-variable mutex))
                          (primitive (symbol-function 'condition-wait))
                          (post-gc-hook
                           (list
                            (lambda ()
                              (cl-incf hooks)
                              (unless injected
                                (if (cl-some
                                     (lambda (frame)
                                       (memq (nth 1 frame)
                                             (list 'condition-wait primitive)))
                                     (backtrace-frames))
                                    (let* ((lock (make-mutex))
                                           (decoy (make-condition-variable lock)))
                                      (setq injected t)
                                      (with-mutex lock (condition-wait decoy)))
                                  ;; Leave another collection due at the
                                  ;; next funcall, including the native one.
                                  (make-string 1000000 ?x)))))))
                     (with-mutex mutex
                       (dotimes (_ 3)
                         (make-string 100000 ?y)
                         (mevedel-native--condition-wait condition 0.01)))
                     (setq completed t)))
                 "native-wait-gc" 'silently))
          (await (lambda () completed))
          (should (> hooks 0))
          (should-not injected))
      (stop worker)))

  ;; Later advice cannot interpose a wait after the native deadline is
  ;; armed.  Ordinary callers still receive that advice.
  (let* ((mutex (make-mutex))
         (condition (make-condition-variable mutex))
         (advice (lambda (&rest _) (error "unexpected advised wait"))))
    (unwind-protect
        (progn
          (advice-add 'condition-wait :override advice)
          (with-mutex mutex
            (mevedel-native--condition-wait condition 0.0)
            (should-error (condition-wait condition))))
      (advice-remove 'condition-wait advice)))

  ;; An error before the POSIX wait must not enter a Lisp debugger with
  ;; the one-shot deadline still armed.
  (let* ((primitive (symbol-function 'condition-wait))
         entered signaled
         (debug-on-error t)
         (debug-on-signal t)
         (debugger
          (lambda (&rest _)
            (when (cl-some (lambda (frame) (eq (nth 1 frame) primitive))
                           (backtrace-frames))
              (setq entered t)))))
    (condition-case nil
        (mevedel-native--condition-wait nil 0.01)
      (error (setq signaled t)))
    (should signaled)
    (should-not entered))

  ;; Independent deadlines, explicit notification, recursive mutex
  ;; restoration, and main-thread collection while a worker is parked.
  (let* ((lock (make-mutex)) (condition (make-condition-variable lock))
         (other-lock (make-mutex))
         (other-condition (make-condition-variable other-lock))
         long short entered completed short-completed (collections 0)
         (threshold gc-cons-threshold))
    (unwind-protect
        (progn
          (setq long
                (make-thread
                 (lambda ()
                   (with-mutex lock
                     (with-mutex lock
                       (setq entered t)
                       (mevedel-native--condition-wait condition 60.0)
                       (mevedel-native--condition-wait condition 0.0))
                     (mevedel-native--condition-wait condition 0.0))
                   (setq completed t))
                 "native-wait-long" 'silently))
          (await (lambda () entered))
          (setq short
                (make-thread
                 (lambda ()
                   (with-mutex other-lock
                     (mevedel-native--condition-wait other-condition 0.02))
                   (setq short-completed t))
                 "native-wait-short" 'silently))
          (await (lambda () short-completed))
          (should-not completed)
          (should (= gc-cons-threshold threshold))
          (let ((post-gc-hook (list (lambda () (cl-incf collections)))))
            (garbage-collect))
          (should (= collections 1))
          (should-not completed)
          (with-mutex lock (condition-notify condition))
          (await (lambda () completed)))
      (stop long)
      (stop short)))

  ;; A failed primitive call and a signaled long wait must clear the TLS
  ;; deadline.  The same thread's following ordinary wait has no timeout.
  (let* ((mutex (make-mutex))
         (condition (make-condition-variable mutex))
         worker entered interrupted ordinary completed)
    (unwind-protect
        (progn
          (setq worker
                (make-thread
                 (lambda ()
                   (with-mutex mutex
                     (should-error (mevedel-native--condition-wait nil 0.01))
                     (setq ordinary 'after-error)
                     (condition-wait condition)
                     (setq entered t)
                     (condition-case nil
                         (mevedel-native--condition-wait condition 60.0)
                       (error (setq interrupted t)))
                     (setq ordinary 'after-signal)
                     (condition-wait condition)
                     (setq completed t)))
                 "native-wait-stop" 'silently))
          (await (lambda () (eq ordinary 'after-error)))
          (sleep-for 0.04)
          (should-not entered)
          (with-mutex mutex (condition-notify condition))
          (await (lambda () entered))
          (thread-signal worker 'error '("stop native wait"))
          (await (lambda () (eq ordinary 'after-signal)))
          (should interrupted)
          (sleep-for 0.04)
          (should-not completed)
          (with-mutex mutex (condition-notify condition))
          (await (lambda () completed)))
      (stop worker))))
