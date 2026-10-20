;;; test-mevedel-session-durability-async.el --- Asynchronous lease operations -*- lexical-binding: t -*-

;;; Commentary:

;; Covers the continuation-passing lease operations: update, publication
;; reserve/commit/finish, head observation, and warm admission.  Each runs
;; against a real local session directory, through the asynchronous carrier
;; and through its synchronous fallback, and none may reach a synchronous
;; target program.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'mevedel-session-persistence)
(require 'mevedel-session-durability)
(require 'mevedel-session-recovery)
(require 'mevedel-session-transfer)
(require 'mevedel-session-publication)
(require 'mevedel-structs)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro test-mevedel-durability-async--with-lease (spec &rest body)
  "Run BODY with SESSION owning a fresh local lease below ROOT.
SPEC is (ROOT SESSION).  Cleans up the directory, timer, and lease."
  (declare (indent 1))
  `(let* ((,(car spec) (file-name-as-directory
                        (make-temp-file "mevedel-durability-async-" t)))
          (,(cadr spec)
           (mevedel-session-create
            "main"
            (mevedel-workspace--create
             :type 'project :id ,(car spec) :root ,(car spec) :name "local")))
          (mevedel-session-durability--client-id (make-string 64 ?a)))
     (setf (mevedel-session-save-path ,(cadr spec)) ,(car spec)
           (mevedel-session-session-id ,(cadr spec)) "async-session")
     (unwind-protect
         (progn
           (should (mevedel-session-durability-lease-acquire
                    ,(car spec) "*owner*" ,(cadr spec)))
           ;; The heartbeat must not rewrite the lease between a case's
           ;; observation and its commit.
           (mevedel-session-durability--cancel-renewal ,(cadr spec))
           ,@body)
       (mevedel-session-durability--cancel-renewal ,(cadr spec))
       (ignore-errors
         (mevedel-session-durability-lease-release
          ,(car spec) ,(cadr spec)))
       (delete-directory ,(car spec) t))))

(defun test-mevedel-durability-async--await (cell)
  "Wait until CELL, a one-element list, holds a result, and return it."
  (let ((steps 0))
    (while (and (null (car cell)) (< (cl-incf steps) 500))
      (accept-process-output nil 0.01))
    (should (car cell))
    (car cell)))

(defun test-mevedel-durability-async--launch (launcher)
  "Call LAUNCHER with a callback and return the (SUCCESS ERROR) it receives.
The callback must run exactly once, and no synchronous target program may
run while it is pending."
  (let ((cell (list nil))
        (calls 0))
    (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
               (lambda (&rest _)
                 (ert-fail "Synchronous target program in an async operation"))))
      (funcall launcher (lambda (success error)
                          (cl-incf calls)
                          (setcar cell (list success error))))
      (test-mevedel-durability-async--await cell))
    (accept-process-output nil 0.02)
    (should (= 1 calls))
    (car cell)))

(defun test-mevedel-durability-async--call (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and a trailing callback, as `--launch' does."
  (test-mevedel-durability-async--launch
   (lambda (callback) (apply function (append arguments (list callback))))))

(mevedel-deftest mevedel-session-durability-capture-context ()
  ,test
  (test)
  :doc "snapshots the transaction cells and riders bound at launch"
  (let (context)
    (let ((mevedel-session-durability--transaction-clock (list 'clock))
          (mevedel-session-durability--asserted-directories (list 'asserted))
          (mevedel-session-recovery--mutation-cache (list 'recovery))
          (mevedel-session-durability--commit-riders (list :before nil)))
      (setq context (mevedel-session-durability-capture-context)))
    (should-not mevedel-session-durability--transaction-clock)
    (should-not mevedel-session-durability--commit-riders)
    (should (equal '((clock) (asserted) (recovery) (:before nil) nil)
                   context)))
  :doc "captures the process owner function in effect at launch"
  (let ((owner (lambda (_process) nil))
        context)
    (let ((mevedel-session-durability-async-process-function owner))
      (setq context (mevedel-session-durability-capture-context)))
    (should (eq owner (nth 4 context)))))

(mevedel-deftest mevedel-session-durability-call-in-context ()
  ,test
  (test)
  :doc "rebinds the transaction cells and riders after the launch binding ends"
  (let (context)
    (let ((mevedel-session-durability--transaction-clock (list 'clock))
          (mevedel-session-durability--asserted-directories (list 'asserted))
          (mevedel-session-recovery--mutation-cache (list 'recovery))
          (mevedel-session-durability--commit-riders (list :before nil)))
      (setq context (mevedel-session-durability-capture-context)))
    (should-not mevedel-session-durability--transaction-clock)
    (should-not mevedel-session-durability--commit-riders)
    (should
     (equal '((clock) (asserted) (recovery) (:before nil))
            (mevedel-session-durability-call-in-context
             context
             (lambda ()
               (list mevedel-session-durability--transaction-clock
                     mevedel-session-durability--asserted-directories
                     mevedel-session-recovery--mutation-cache
                     mevedel-session-durability--commit-riders)))))
    (should-not mevedel-session-durability--transaction-clock))
  :doc "binds the process owner for the call and restores it afterwards"
  (let ((owner (lambda (_process) nil)))
    (should (eq owner
                (mevedel-session-durability-call-in-context
                 (list nil nil nil nil owner)
                 (lambda ()
                   mevedel-session-durability-async-process-function))))
    (should-not mevedel-session-durability-async-process-function)))

(mevedel-deftest mevedel-session-durability--update-owned-lease-async ()
  ,test
  (test)
  :doc "renews through the asynchronous carrier from an observation"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((before (plist-get (mevedel-session-lease session) :expires-at)))
      (setf (mevedel-session-lease session)
            (mevedel-session-durability--strip-assumption
             (mevedel-session-lease session)))
      (sleep-for 1.1)
      (should (equal '(t nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability--update-owned-lease-async
                      session '(active publishing) 'active 90)))
      (should (eq 'owned (plist-get (mevedel-session-lease session) :state)))
      (should (> (plist-get (mevedel-session-lease session) :expires-at)
                 before))
      (should (plist-get (mevedel-session-lease session) :bytes))))
  :doc "states its remembered bytes while the transaction clock is fresh"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((directory (mevedel-session-durability--lease-path root))
          (programs nil)
          (run (symbol-function 'mevedel-session-control-fs-run-program-async)))
      (mevedel-session-durability-with-transaction
        (mevedel-session-durability--note-target-time
         directory (mevedel-session-control-fs-target-time directory))
        (cl-letf (((symbol-function
                    'mevedel-session-control-fs-run-program-async)
                   (lambda (operations callback)
                     (push operations programs)
                     (funcall run operations callback))))
          (should (equal '(t nil)
                         (test-mevedel-durability-async--call
                          #'mevedel-session-durability--update-owned-lease-async
                          session '(active) 'active 90)))))
      ;; One program: the commit, with no preceding observation.
      (should (= 1 (length programs)))
      (should (eq 'verify (plist-get (car (car programs)) :op)))))
  :doc "runs to completion before returning on the synchronous carrier"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((mevedel-session-control-fs--async-wait t)
          settled)
      (mevedel-session-durability--update-owned-lease-async
       session '(active) 'active 90
       (lambda (success error) (setq settled (list success error))))
      (should (equal '(t nil) settled))))
  :doc "declines a head other than the expected publication head"
  (test-mevedel-durability-async--with-lease (root session)
    (should (equal '(nil nil)
                   (test-mevedel-durability-async--launch
                    (lambda (callback)
                      (mevedel-session-durability--update-owned-lease-async
                       session '(active) 'active 90 callback nil t "other")))))
    (should (eq 'owned (plist-get (mevedel-session-lease session) :state)))
    (should (equal 1 (plist-get (mevedel-session-lease session) :generation))))
  :doc "binds a lease another client took as lost"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((directory (mevedel-session-durability--lease-path root))
          (lease (mevedel-session-lease session)))
      (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
        (mevedel-session-durability--write-plist
         (mevedel-session-durability--generation-path
          directory (plist-get lease :generation))
         (mevedel-session-durability--lease-record
          "*thief*" (plist-get lease :generation) 'active nil nil
          (plist-get lease :renewed-at))))
      (should (equal '(nil nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability--update-owned-lease-async
                      session '(active) 'active 90)))
      (should (eq 'lost (plist-get (mevedel-session-lease session) :state)))))
  :doc "carries the launch-time commit riders across the asynchronous steps"
  (test-mevedel-durability-async--with-lease (root session)
    (let* ((log (file-name-concat root "log"))
           (riders (list :before (list (list :op 'append :path log
                                             :content "x"))
                         :consumed nil)))
      (setf (mevedel-session-lease session)
            (mevedel-session-durability--strip-assumption
             (mevedel-session-lease session)))
      (let ((mevedel-session-durability--commit-riders riders))
        (let ((cell (list nil)))
          (mevedel-session-durability--update-owned-lease-async
           session '(active) 'active 90
           (lambda (success error) (setcar cell (list success error))))
          ;; The launch binding ends before the program answers.
          (setq mevedel-session-durability--commit-riders nil)
          (should (equal '(t nil)
                         (test-mevedel-durability-async--await cell)))))
      (should (plist-get riders :consumed))
      (should (equal "x" (mevedel-session-control-fs-read-file log)))))
  :doc "reports a failed program once and keeps the lease binding"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((bound (copy-sequence (mevedel-session-lease session)))
          (calls 0)
          settled)
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
                 (lambda (_operations callback)
                   (funcall callback nil '(file-error "boom"))
                   (funcall callback nil '(file-error "again")))))
        (mevedel-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error)
           (cl-incf calls)
           (setq settled (list success error)))))
      (should (equal '(nil (file-error "boom")) settled))
      (should (= 1 calls))
      (should (equal bound (mevedel-session-lease session)))))
  :doc "reports each launched process to the owner bound at launch"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((processes nil))
      (setf (mevedel-session-lease session)
            (mevedel-session-durability--strip-assumption
             (mevedel-session-lease session)))
      (should (equal '(t nil)
                     (test-mevedel-durability-async--launch
                      (lambda (callback)
                        (let ((mevedel-session-durability-async-process-function
                               (lambda (process) (push process processes))))
                          (mevedel-session-durability--update-owned-lease-async
                           session '(active) 'active 90 callback))))))
      ;; Observation, then commit: each its own process.
      (should (= 2 (length processes)))
      (should (cl-every #'processp processes))))
  :doc "does not call back twice when its callback signals"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((mevedel-session-control-fs--async-wait t)
          (calls 0))
      (should-error
       (mevedel-session-durability--update-owned-lease-async
        session '(active) 'active 90
        (lambda (_success _error)
          (cl-incf calls)
          (error "Callback failed"))))
      (should (= 1 calls))))
  :doc "waits for the transport while a remote operation is in flight"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((busy t)
          (cell (list nil))
          thunk)
      (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                 (lambda (&optional _path) busy))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (_key _path function &rest _)
                   (if busy
                       (setq thunk function)
                     (funcall function))
                   t)))
        (setf (mevedel-session-lease session)
              (mevedel-session-durability--strip-assumption
               (mevedel-session-lease session)))
        (mevedel-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error) (setcar cell (list success error))))
        (should thunk)
        (should-not (car cell))
        (setq busy nil)
        (funcall thunk)
        (should (equal '(t nil) (test-mevedel-durability-async--await cell))))))
  :doc "fails the job when the transport refuses to wait"
  (test-mevedel-durability-async--with-lease (root session)
    (let (settled)
      (cl-letf (((symbol-function 'mevedel-transport-busy-p)
                 (lambda (&optional _path) t))
                ((symbol-function 'mevedel-transport-run-when-idle)
                 (lambda (&rest _) nil)))
        (mevedel-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error) (setq settled (list success error)))))
      (should-not (car settled))
      (should (eq 'user-error (car (cadr settled))))))
  :doc "a quit in a continuation finishes the job once and never escapes the timer"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((settle (symbol-function 'mevedel-session-durability--observe-lease-settle))
          (quits 0)
          (calls nil)
          (escaped 0)
          bound)
      (setf (mevedel-session-lease session)
            (mevedel-session-durability--strip-assumption
             (mevedel-session-lease session)))
      (setq bound (copy-sequence (mevedel-session-lease session)))
      (cl-letf (((symbol-function 'mevedel-session-durability--observe-lease-settle)
                 (lambda (&rest arguments)
                   (cl-incf quits)
                   (signal 'quit nil)
                   (apply settle arguments))))
        (mevedel-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error) (push (list success error) calls)))
        (let ((steps 0))
          (while (and (< (cl-incf steps) 300) (null calls))
            (condition-case nil
                (accept-process-output nil 0.01)
              (quit (cl-incf escaped)))))
        (condition-case nil
            (accept-process-output nil 0.05)
          (quit (cl-incf escaped))))
      (should (= 0 escaped))
      (should (equal '((nil (quit))) calls))
      (should (= 1 quits))
      (should (equal bound (mevedel-session-lease session)))))
  :doc "a quit while reporting the launched process finishes once, escapes nowhere, and the late result is inert"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((run (symbol-function 'mevedel-session-control-fs-run-program-async))
          (launches 0)
          (escaped 0)
          (calls nil)
          bound)
      (setf (mevedel-session-lease session)
            (mevedel-session-durability--strip-assumption
             (mevedel-session-lease session)))
      (setq bound (copy-sequence (mevedel-session-lease session)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
                 (lambda (operations callback)
                   (cl-incf launches)
                   (funcall run operations callback))))
        (let ((mevedel-session-durability-async-process-function
               (lambda (_process) (signal 'quit nil))))
          (condition-case nil
              (mevedel-session-durability--update-owned-lease-async
               session '(active) 'active 90
               (lambda (success error) (push (list success error) calls)))
            (quit (cl-incf escaped))))
        (should (equal '((nil (quit))) calls))
        ;; The program still answers; nothing may continue from it.
        (let ((steps 0))
          (while (and (< (cl-incf steps) 60))
            (condition-case nil
                (accept-process-output nil 0.01)
              (quit (cl-incf escaped))))))
      (should (= 0 escaped))
      (should (= 1 launches))
      (should (equal '((nil (quit))) calls))
      (should (equal bound (mevedel-session-lease session))))))

(mevedel-deftest mevedel-session-durability--async-job ()
  ,test
  (test)
  :doc "finish calls back once"
  (let* (calls
         (job (mevedel-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (car job) t nil)
    (funcall (car job) nil '(error "late"))
    (should (equal '((t nil)) calls)))
  :doc "an error in a guarded thunk finishes the job"
  (let* (calls
         (job (mevedel-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (cdr job) (lambda () (error "Boom")))
    (should (equal '((nil (error "Boom"))) calls)))
  :doc "a quit finishes the job and returns without re-signalling"
  (let* (calls
         (job (mevedel-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (should (eq 'returned (condition-case nil
                              (progn (funcall (cdr job) (lambda () (signal 'quit nil)))
                                     'returned)
                            (quit 'escaped))))
    (should (equal '((nil (quit))) calls)))
  :doc "a quit inside the callback is a warning, not an escape or a second call"
  (let* ((calls 0)
         diagnostics
         (job (mevedel-session-durability--async-job
               (lambda (_success _error) (cl-incf calls) (signal 'quit nil)))))
    (mevedel-test--with-captured-diagnostics diagnostics
      (should (eq 'returned (condition-case nil
                                (progn (funcall (cdr job) (lambda () (error "Boom")))
                                       (funcall (car job) t nil)
                                       'returned)
                              (quit 'escaped)))))
    (should (= 1 calls))
    (should (string-match-p "interrupted" diagnostics)))
  :doc "a quit after the job finished is a warning, not an escape"
  (let* (diagnostics
         (job (mevedel-session-durability--async-job #'ignore)))
    (mevedel-test--with-captured-diagnostics diagnostics
      (should (eq 'returned (condition-case nil
                                (progn
                                  (funcall (cdr job)
                                           (lambda ()
                                             (funcall (car job) t nil)
                                             (signal 'quit nil)))
                                  'returned)
                              (quit 'escaped)))))
    (should (string-match-p "interrupted" diagnostics)))
  :doc "a thunk after the job finished does not run"
  (let* (calls ran
         (job (mevedel-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (car job) nil '(error "aborted"))
    (funcall (cdr job) (lambda () (setq ran t)))
    (should-not ran)
    (should (= 1 (length calls))))
  :doc "an error from the callback itself propagates without a second call"
  (let* ((calls 0)
         (job (mevedel-session-durability--async-job
               (lambda (_success _error) (cl-incf calls) (error "Callback failed")))))
    (should-error (funcall (cdr job) (lambda () (funcall (car job) t nil))))
    (should (= 1 calls))))

(mevedel-deftest mevedel-session-durability-commit-publication-head-async ()
  ,test
  (test)
  :doc "commits a head over a reserved lease and keeps it publishing"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (equal '(t nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-reserve-publication-lease-async
                      session)))
      (should (eq 'publishing
                  (plist-get (mevedel-session-lease session) :status)))
      (should (equal '(t nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-commit-publication-head-async
                      session head)))
      (should (equal head (plist-get (mevedel-session-lease session)
                                     :publication-head)))
      (should (eq 'publishing
                  (plist-get (mevedel-session-lease session) :status)))))
  :doc "a final commit returns the lease to active in the same program"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head ".publications/0002/manifest.el"))
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-reserve-publication-lease-async
                    session)))
      (let ((mevedel-session-durability--final-commit t))
        (should (equal '(t nil)
                       (test-mevedel-durability-async--call
                        #'mevedel-session-durability-commit-publication-head-async
                        session head))))
      (should (eq 'active (plist-get (mevedel-session-lease session) :status)))
      (should (equal head (plist-get (mevedel-session-lease session)
                                     :publication-head)))))
  :doc "rejects a head that is not an immutable manifest without a program"
  (test-mevedel-durability-async--with-lease (root session)
    (let (settled)
      (mevedel-session-durability-commit-publication-head-async
       session "plain.el"
       (lambda (success error) (setq settled (list success error))))
      (should-not (car settled))
      (should (eq 'error (car (cadr settled))))))
  :doc "refuses a stale expected head and leaves the committed head alone"
  (test-mevedel-durability-async--with-lease (root session)
    (progn
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-reserve-publication-lease-async
                    session)))
      (let ((bound (mevedel-session-lease session)))
        (setf (mevedel-session-lease session)
              (plist-put (copy-sequence bound) :publication-head
                         ".publications/stale/manifest.el")))
      (should (equal '(nil nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-commit-publication-head-async
                      session ".publications/0003/manifest.el"))))))

(mevedel-deftest mevedel-session-durability-reserve-publication-lease-async ()
  ,test
  (test)
  :doc "reserves the bounded publishing window through an asynchronous program"
  (test-mevedel-durability-async--with-lease (root session)
    (should (equal '(t nil)
                   (test-mevedel-durability-async--call
                    #'mevedel-session-durability-reserve-publication-lease-async
                    session)))
    (should (eq 'publishing
                (plist-get (mevedel-session-lease session) :status)))
    (should (eq 'owned (plist-get (mevedel-session-lease session) :state)))))

(mevedel-deftest mevedel-session-durability-finish-publication-lease-async ()
  ,test
  (test)
  :doc "returns a publishing lease to active and keeps the committed head"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-reserve-publication-lease-async
                    session)))
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-commit-publication-head-async
                    session head)))
      (should (equal '(t nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-finish-publication-lease-async
                      session)))
      (should (eq 'active (plist-get (mevedel-session-lease session) :status)))
      (should (equal head (plist-get (mevedel-session-lease session)
                                     :publication-head)))))
  :doc "answers before returning when the lease is already active"
  (test-mevedel-durability-async--with-lease (root session)
    (let (settled)
      (mevedel-session-durability-finish-publication-lease-async
       session (lambda (success error) (setq settled (list success error))))
      (should (equal '(t nil) settled)))))

(mevedel-deftest mevedel-session-durability-observe-publication-head-async ()
  ,test
  (test)
  :doc "reports the newest head and ownership without changing the binding"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-reserve-publication-lease-async
                    session)))
      (should (car (test-mevedel-durability-async--call
                    #'mevedel-session-durability-commit-publication-head-async
                    session head)))
      (let* ((bound (copy-sequence (mevedel-session-lease session)))
             (observed (car (test-mevedel-durability-async--call
                             #'mevedel-session-durability-observe-publication-head-async
                             session))))
        (should (equal head (plist-get observed :publication-head)))
        (should (plist-get observed :owned))
        (should (numberp (plist-get observed :now)))
        (should (equal bound (mevedel-session-lease session))))))
  :doc "sees a newer foreign generation as the head and not owned"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((directory (mevedel-session-durability--lease-path root))
          (lease (mevedel-session-lease session)))
      (let ((mevedel-session-durability--client-id (make-string 64 ?b)))
        (mevedel-session-durability--create-plist
         (mevedel-session-durability--generation-path
          directory (1+ (plist-get lease :generation)))
         (mevedel-session-durability--lease-record
          "*thief*" (1+ (plist-get lease :generation)) 'active
          ".publications/9999/manifest.el" nil
          (plist-get lease :renewed-at)))
        (let ((observed
               (car (test-mevedel-durability-async--call
                     #'mevedel-session-durability-observe-publication-head-async
                     session))))
          (should (equal ".publications/9999/manifest.el"
                         (plist-get observed :publication-head)))
          ;; The observer's own client id is the thief's here, so the
          ;; record is live authority for it; under the owner it is not.
          (should (plist-get observed :owned))))
      (let ((observed
             (car (test-mevedel-durability-async--call
                   #'mevedel-session-durability-observe-publication-head-async
                   session))))
        (should (equal ".publications/9999/manifest.el"
                       (plist-get observed :publication-head)))
        (should-not (plist-get observed :owned))))))

(mevedel-deftest mevedel-session-durability-admit-async ()
  ,test
  (test)
  :doc "admits a warm owned session from one program"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((programs 0)
          (run (symbol-function 'mevedel-session-control-fs-run-program-async)))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program-async)
                 (lambda (operations callback)
                   (cl-incf programs)
                   (funcall run operations callback))))
        (mevedel-session-durability-with-transaction
          (should (equal '(t nil)
                         (test-mevedel-durability-async--call
                          #'mevedel-session-durability-admit-async session)))
          ;; The program's PID-lock proof is recorded for the transaction.
          (should (member (mevedel-session-control-fs-physical-path root)
                          (car mevedel-session-durability--asserted-directories)))))
      (should (= 1 programs))))
  :doc "declines without a program when no lease bytes are bound"
  (test-mevedel-durability-async--with-lease (root session)
    (setf (mevedel-session-lease session)
          (mevedel-session-durability--strip-assumption
           (mevedel-session-lease session)))
    (should (equal '(nil nil)
                   (test-mevedel-durability-async--call
                    #'mevedel-session-durability-admit-async session))))
  :doc "declines when the bytes on the target differ from the bound ones"
  (test-mevedel-durability-async--with-lease (root session)
    (progn
      (setf (mevedel-session-lease session)
            (plist-put (copy-sequence (mevedel-session-lease session))
                       :bytes "(:stale t)"))
      (should (equal '(nil nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-admit-async session)))))
  :doc "declines while a recovery marker root exists"
  (test-mevedel-durability-async--with-lease (root session)
    (progn
      (make-directory (file-name-concat root ".recovery") t)
      (should (equal '(nil nil)
                     (test-mevedel-durability-async--call
                      #'mevedel-session-durability-admit-async session)))))
  :doc "reports a PID lock as an error"
  (test-mevedel-durability-async--with-lease (root session)
    (progn
      (with-temp-file (file-name-concat root ".lock") (insert "1"))
      (let ((result (test-mevedel-durability-async--call
                     #'mevedel-session-durability-admit-async session)))
        (should-not (car result))
        (should (cadr result)))))
  :doc "declines while the session is quiescing for control transfer"
  (test-mevedel-durability-async--with-lease (root session)
    (setf (mevedel-session-control-transfer session) '(:state quiescing))
    (should (equal '(nil nil)
                   (test-mevedel-durability-async--call
                    #'mevedel-session-durability-admit-async session)))))

(defmacro test-mevedel-durability-async--gated (session gated &rest body)
  "Run BODY with the publication barrier stubbed to record its arguments.
GATED is a variable bound to the list of (SESSION TOKEN) calls.  The stub
signals a `user-error', so a gated operation must stop before any I/O."
  (declare (indent 2))
  `(let ((,gated nil))
     (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                (lambda (session &optional token)
                  (push (list session token) ,gated)
                  (user-error "Gated")))
               ((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (&rest _) (ert-fail "Gated operation reached target"))))
       ,@body)
     (should (equal (list (list ,session "token")) ,gated))))

(mevedel-deftest mevedel-session-durability--gate ()
  ,test
  (test)
  :doc "every public lease-changing operation barriers with the caller's token"
  (test-mevedel-durability-async--with-lease (root session)
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-lease-acquire
                     root "x" session "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-call-with-abandoned-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-call-with-reserved-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-call-with-held-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-adopt-owned-lease
                     session session "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-forget-removed-session
                     session "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-set-unsettled-mutation
                     session t "token")
                    :type 'user-error))
    (test-mevedel-durability-async--gated session gated
      (should-error (mevedel-session-durability-lease-release
                     root session "token")
                    :type 'user-error))
    (should (mevedel-session-durability-lease-owned-p session)))
  :doc "a call without a token reaches the barrier with a nil token"
  (test-mevedel-durability-async--with-lease (root session)
    (let (seen)
      (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                 (lambda (_session &optional token)
                   (push token seen)
                   (user-error "Gated"))))
        (should-error (mevedel-session-durability-call-with-reserved-lease
                       session #'ignore)
                      :type 'user-error))
      (should (equal '(nil) seen))))
  :doc "the reserved and held wrappers nest inside an enclosing synchronous window"
  (test-mevedel-durability-async--with-lease (root session)
    (let (calls)
      (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                 (lambda (&rest _) (push t calls) (user-error "Gated"))))
        (setf (mevedel-session-publication-active-p session) t)
        (should (eq 'done (mevedel-session-durability-call-with-held-lease
                           session (lambda () 'done))))
        (setf (mevedel-session-publication-active-p session) t)
        (should (eq 'done (mevedel-session-durability-call-with-reserved-lease
                           session (lambda () 'done))))
        (should-not calls)
        ;; With an asynchronous job in the window, the gate applies.
        (setf (mevedel-session-publication-active-p session) t
              (mevedel-session-publication-job session) 'job)
        (should-error (mevedel-session-durability-call-with-held-lease
                       session #'ignore)
                      :type 'user-error)
        (should calls)
        (setf (mevedel-session-publication-active-p session) nil
              (mevedel-session-publication-job session) nil))))
  :doc "inside a queued preparation only its token nests; others reach the barrier"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((token (list 'preparation))
          seen)
      (cl-letf (((symbol-function 'mevedel-session-publication-barrier)
                 (lambda (_session &optional given)
                   (push given seen)
                   (user-error "Gated"))))
        (setf (mevedel-session-publication-active-p session) t
              (mevedel-session-publication-preparation-token session) token)
        (unwind-protect
            (progn
              ;; The preparation's synchronous publication nests.
              (should (eq 'done (mevedel-session-durability-call-with-held-lease
                                 session (lambda () 'done) token)))
              (should-not seen)
              (setf (mevedel-session-publication-active-p session) t)
              ;; Without its token, or with another's, the gate applies.
              (should-error (mevedel-session-durability-call-with-held-lease
                             session #'ignore)
                            :type 'user-error)
              (setf (mevedel-session-publication-active-p session) t)
              (should-error (mevedel-session-durability-call-with-reserved-lease
                             session #'ignore (list 'other))
                            :type 'user-error)
              (should (= 2 (length seen))))
          (setf (mevedel-session-publication-active-p session) nil
                (mevedel-session-publication-preparation-token session) nil)))))
  :doc "an unrelated timer cannot change the latch inside a running preparation"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((token (list 'preparation))
          (before (copy-sequence (mevedel-session-lease session)))
          outcome
          timer)
      (setf (mevedel-session-publication-active-p session) t
            (mevedel-session-publication-preparation-token session) token)
      (unwind-protect
          (progn
            (setq timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (setq outcome
                           (condition-case err
                               (mevedel-session-durability-set-unsettled-mutation
                                session t)
                             (user-error (list 'refused (cadr err))))))))
            (let ((steps 0))
              (while (and (null outcome) (< (cl-incf steps) 200))
                (accept-process-output nil 0.01)))
            (should (eq 'refused (car outcome)))
            (should (equal before (mevedel-session-lease session)))
            ;; The preparation's own token is accepted.
            (should (mevedel-session-durability-set-unsettled-mutation
                     session t token))
            (should (mevedel-session-durability-unsettled-mutation-p session)))
        (when (timerp timer) (cancel-timer timer))
        (setf (mevedel-session-publication-active-p session) nil
              (mevedel-session-publication-preparation-token session) nil)))))

(mevedel-deftest mevedel-session-durability-bind-observed-lease ()
  ,test
  (test)
  :doc "binds an observed owned head with the bytes a commit of it wrote"
  (test-mevedel-durability-async--with-lease (root session)
    (let* ((head (copy-sequence (mevedel-session-lease session)))
           (observed (list :lease head :owned t)))
      (setf (mevedel-session-lease session) nil)
      (should (eq 'owned (mevedel-session-durability-bind-observed-lease
                          session observed)))
      (should (eq 'owned (plist-get (mevedel-session-lease session) :state)))
      (should (equal (mevedel-session-durability--record-bytes head)
                     (plist-get (mevedel-session-lease session) :bytes)))
      (mevedel-session-durability--cancel-renewal session)))
  :doc "binds an unactivated claim beside the lease as contested"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head (list :generation 2 :status 'claiming)))
      (should (eq 'contested (mevedel-session-durability-bind-observed-lease
                              session (list :lease head :owned nil))))
      (should (eq 'contested
                  (plist-get (mevedel-session-lease session) :state)))
      (should-not (plist-get (mevedel-session-lease session) :bytes))
      (mevedel-session-durability--cancel-renewal session)))
  :doc "binds any other unowned head as lost and drops the remembered bytes"
  (test-mevedel-durability-async--with-lease (root session)
    (let ((head (list :generation 2 :status 'active
                      :client-id (make-string 64 ?b))))
      (should (eq 'lost (mevedel-session-durability-bind-observed-lease
                         session (list :lease head :owned nil))))
      (should (eq 'lost (plist-get (mevedel-session-lease session) :state)))
      (should-not (plist-get (mevedel-session-lease session) :bytes)))))

(provide 'test-mevedel-session-durability-async)

;;; test-mevedel-session-durability-async.el ends here
