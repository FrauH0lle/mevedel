;;; test-niwa-session-durability-async.el --- Asynchronous lease operations -*- lexical-binding: t -*-

;;; Commentary:

;; Covers the continuation-passing lease operations: update, publication
;; reserve/commit/finish, head observation, and warm admission.  Each runs
;; against a real local session directory, through the asynchronous carrier
;; and through its synchronous fallback, and none may reach a synchronous
;; target program.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'niwa-session-persistence)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-session-recovery)
(require 'niwa-session-transfer)
(require 'niwa-session-publication)
(require 'niwa-structs)
(require 'niwa-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro test-niwa-durability-async--with-lease (spec &rest body)
  "Run BODY with SESSION owning a fresh local lease below ROOT.
SPEC is (ROOT SESSION).  Cleans up the directory, timer, and lease."
  (declare (indent 1))
  `(let* ((,(car spec) (file-name-as-directory
                        (make-temp-file "niwa-durability-async-" t)))
          (,(cadr spec)
           (niwa-session-create
            "main"
            (niwa-workspace--create
             :type 'project :id ,(car spec) :root ,(car spec) :name "local")))
          (niwa-session-durability--client-id (make-string 64 ?a)))
     (setf (niwa-session-save-path ,(cadr spec)) ,(car spec)
           (niwa-session-session-id ,(cadr spec)) "async-session")
     (unwind-protect
         (progn
           (should (niwa-session-durability-lease-acquire
                    ,(car spec) "*owner*" ,(cadr spec)))
           ;; The heartbeat must not rewrite the lease between a case's
           ;; observation and its commit.
           (niwa-session-durability--cancel-renewal ,(cadr spec))
           ,@body)
       (niwa-session-durability--cancel-renewal ,(cadr spec))
       (ignore-errors
         (niwa-session-durability-lease-release
          ,(car spec) ,(cadr spec)))
       (delete-directory ,(car spec) t))))

(defun test-niwa-durability-async--await (cell)
  "Wait until CELL, a one-element list, holds a result, and return it."
  (let ((steps 0))
    (while (and (null (car cell)) (< (cl-incf steps) 500))
      (accept-process-output nil 0.01))
    (should (car cell))
    (car cell)))

(defun test-niwa-durability-async--launch (launcher)
  "Call LAUNCHER with a callback and return the (SUCCESS ERROR) it receives.
The callback must run exactly once, and no synchronous target program may
run while it is pending."
  (let ((cell (list nil))
        (calls 0))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (&rest _)
                 (ert-fail "Synchronous target program in an async operation"))))
      (funcall launcher (lambda (success error)
                          (cl-incf calls)
                          (setcar cell (list success error))))
      (test-niwa-durability-async--await cell))
    (accept-process-output nil 0.02)
    (should (= 1 calls))
    (car cell)))

(defun test-niwa-durability-async--call (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and a trailing callback, as `--launch' does."
  (test-niwa-durability-async--launch
   (lambda (callback) (apply function (append arguments (list callback))))))

(niwa-deftest niwa-session-durability-capture-context ()
  ,test
  (test)
  :doc "snapshots the transaction cells and riders bound at launch"
  (let (context)
    (let ((niwa-session-durability--transaction-clock (list 'clock))
          (niwa-session-durability--asserted-directories (list 'asserted))
          (niwa-session-recovery--mutation-cache (list 'recovery))
          (niwa-session-durability--commit-riders (list :before nil)))
      (setq context (niwa-session-durability-capture-context)))
    (should-not niwa-session-durability--transaction-clock)
    (should-not niwa-session-durability--commit-riders)
    (should (equal '((clock) (asserted) (recovery) (:before nil) nil nil)
                   context)))
  :doc "captures the process owner function in effect at launch"
  (let ((owner (lambda (_process) nil))
        context)
    (let ((niwa-session-durability-async-process-function owner))
      (setq context (niwa-session-durability-capture-context)))
    (should (eq owner (nth 4 context)))))

(niwa-deftest niwa-session-durability-call-in-context ()
  ,test
  (test)
  :doc "rebinds the transaction cells and riders after the launch binding ends"
  (let (context)
    (let ((niwa-session-durability--transaction-clock (list 'clock))
          (niwa-session-durability--asserted-directories (list 'asserted))
          (niwa-session-recovery--mutation-cache (list 'recovery))
          (niwa-session-durability--commit-riders (list :before nil)))
      (setq context (niwa-session-durability-capture-context)))
    (should-not niwa-session-durability--transaction-clock)
    (should-not niwa-session-durability--commit-riders)
    (should
     (equal '((clock) (asserted) (recovery) (:before nil))
            (niwa-session-durability-call-in-context
             context
             (lambda ()
               (list niwa-session-durability--transaction-clock
                     niwa-session-durability--asserted-directories
                     niwa-session-recovery--mutation-cache
                     niwa-session-durability--commit-riders)))))
    (should-not niwa-session-durability--transaction-clock))
  :doc "binds the process owner for the call and restores it afterwards"
  (let ((owner (lambda (_process) nil)))
    (should (eq owner
                (niwa-session-durability-call-in-context
                 (list nil nil nil nil owner)
                 (lambda ()
                   niwa-session-durability-async-process-function))))
    (should-not niwa-session-durability-async-process-function)))

(niwa-deftest niwa-session-durability-fresh-context ()
  ,test
  (test)
  :doc "never joins a surrounding transaction, unlike a captured context"
  (let (fresh captured outer)
    (niwa-session-durability-with-transaction
      (setq outer (list niwa-session-durability--transaction-clock
                        niwa-session-durability--asserted-directories
                        niwa-session-recovery--mutation-cache)
            captured (niwa-session-durability-capture-context)
            fresh (niwa-session-durability-fresh-context #'ignore)))
    ;; Discriminating control: the captured composition shares every cell.
    (dotimes (index 3)
      (should (eq (nth index outer) (nth index captured)))
      (should (consp (nth index fresh)))
      (should-not (eq (nth index outer) (nth index fresh)))))
  :doc "shares one real target clock reading and its proofs across phases only"
  (let* ((directory (make-temp-file "niwa-fresh-context-" t))
         (read (symbol-function 'niwa-session-control-fs-target-time))
         (first (niwa-session-durability-fresh-context #'ignore))
         (second (niwa-session-durability-fresh-context #'ignore))
         (reads 0)
         (phase (lambda (context)
                  (niwa-session-durability-call-in-context
                   context
                   (lambda ()
                     (list (niwa-session-durability--target-time directory)
                           (car niwa-session-durability--asserted-directories)
                           niwa-session-recovery--mutation-cache))))))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-control-fs-target-time)
                   (lambda (path) (cl-incf reads) (funcall read path))))
          (let ((one (funcall phase first)))
            (should (numberp (car one)))
            (should (= 1 reads))
            (niwa-session-durability-call-in-context
             first
             (lambda () (niwa-session-durability-note-no-pid-lock directory)))
            ;; A later phase of the same job reuses the reading and proof.
            (let ((two (funcall phase first)))
              (should (= 1 reads))
              (should (equal (car one) (car two)))
              (should (cadr two))
              (should (eq (nth 2 one) (nth 2 two))))
            ;; Another job's context reads and proves for itself.
            (let ((other (funcall phase second)))
              (should (= 2 reads))
              (should-not (cadr other))
              (should-not (eq (nth 2 one) (nth 2 other))))))
      (delete-directory directory t)))
  :doc "carries no riders or validity predicate, inheriting the remaining flags"
  (let* ((owner (lambda (_process) nil))
         (context (niwa-session-durability-fresh-context owner)))
    (should-not (nth 3 context))
    (should-not (nth 5 context))
    (should
     (equal (list nil nil owner 42 t t)
            (let ((niwa-session-durability--commit-riders (list :before nil))
                  (niwa-session-durability--async-valid-p #'always)
                  (niwa-session-durability--observed-time 42)
                  (niwa-session-durability--final-commit t)
                  (niwa-session-control-fs--async-wait t))
              (niwa-session-durability-call-in-context
               context
               (lambda ()
                 (list niwa-session-durability--commit-riders
                       niwa-session-durability--async-valid-p
                       niwa-session-durability-async-process-function
                       niwa-session-durability--observed-time
                       niwa-session-durability--final-commit
                       niwa-session-control-fs--async-wait))))))))

(niwa-deftest niwa-session-durability-call-outside-transaction ()
  ,test
  (test)
  :doc "calls with every transaction cell of the caller cleared, then restores them"
  (let ((niwa-session-durability--transaction-clock '(sentinel))
        (niwa-session-durability--asserted-directories '(sentinel))
        (niwa-session-durability--commit-riders '(sentinel))
        (niwa-session-durability--final-commit 'sentinel)
        (niwa-session-durability-async-process-function 'sentinel)
        (niwa-session-recovery--mutation-cache '(sentinel)))
    (should (equal '(nil nil nil nil nil nil)
                   (niwa-session-durability-call-outside-transaction
                    (lambda ()
                      (list niwa-session-durability--transaction-clock
                            niwa-session-durability--asserted-directories
                            niwa-session-durability--commit-riders
                            niwa-session-durability--final-commit
                            niwa-session-durability-async-process-function
                            niwa-session-recovery--mutation-cache)))))
    (should (equal '(sentinel) niwa-session-durability--commit-riders))
    (should (eq 'sentinel niwa-session-durability--final-commit))))

(niwa-deftest niwa-session-durability--update-owned-lease-async ()
  ,test
  (test)
  :doc "renews through the asynchronous carrier from an observation"
  (test-niwa-durability-async--with-lease (root session)
    (let ((before (plist-get (niwa-session-lease session) :expires-at))
          (decode (symbol-function 'niwa-session-control-fs--program-value))
          readings)
      (setf (niwa-session-lease session)
            (niwa-session-durability--strip-assumption
             (niwa-session-lease session)))
      ;; Keep the real target programs and decoding, advancing only their
      ;; whole-second clock readings while the async continuation is pending.
      (cl-letf (((symbol-function 'niwa-session-control-fs--program-value)
                 (lambda (operation payload)
                   (let ((value (funcall decode operation payload)))
                     (if (eq (plist-get operation :op) 'target-time)
                         (progn
                           ;; Only the required observation builds the new
                           ;; lease; an optional commit refresh may be later.
                           (unless (plist-get operation :optional)
                             (push value readings))
                           (+ value 2))
                       value)))))
        (should (equal '(t nil)
                       (test-niwa-durability-async--call
                        #'niwa-session-durability--update-owned-lease-async
                        session '(active publishing) 'active 90))))
      (should readings)
      (should (= 1 (length readings)))
      (should (= (plist-get (niwa-session-lease session) :expires-at)
                 (+ (car readings) 2 90)))
      (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
      (should (> (plist-get (niwa-session-lease session) :expires-at)
                 before))
      (should (plist-get (niwa-session-lease session) :bytes))))
  :doc "states its remembered bytes while the transaction clock is fresh"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (programs nil)
          (run (symbol-function 'niwa-session-control-fs-run-program-async)))
      (niwa-session-durability-with-transaction
        (niwa-session-durability--note-target-time
         directory (niwa-session-control-fs-target-time directory))
        (cl-letf (((symbol-function
                    'niwa-session-control-fs-run-program-async)
                   (lambda (operations callback &optional scope)
                     (push operations programs)
                     (funcall run operations callback scope))))
          (should (equal '(t nil)
                         (test-niwa-durability-async--call
                          #'niwa-session-durability--update-owned-lease-async
                          session '(active) 'active 90)))))
      ;; One program: the commit, with no preceding observation.
      (should (= 1 (length programs)))
      (should (eq 'verify (plist-get (car (car programs)) :op)))))
  :doc "runs to completion before returning on the synchronous carrier"
  (test-niwa-durability-async--with-lease (root session)
    (let ((niwa-session-control-fs--async-wait t)
          settled)
      (niwa-session-durability--update-owned-lease-async
       session '(active) 'active 90
       (lambda (success error) (setq settled (list success error))))
      (should (equal '(t nil) settled))))
  :doc "declines a head other than the expected publication head"
  (test-niwa-durability-async--with-lease (root session)
    (should (equal '(nil (user-error "Portable session publication head changed"))
                   (test-niwa-durability-async--launch
                    (lambda (callback)
                      (niwa-session-durability--update-owned-lease-async
                       session '(active) 'active 90 callback nil t "other")))))
    (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
    (should (equal 1 (plist-get (niwa-session-lease session) :generation))))
  :doc "binds a lease another client took as lost"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (lease (niwa-session-lease session)))
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
        (niwa-session-durability--write-plist
         (niwa-session-durability--generation-path
          directory (plist-get lease :generation))
         (niwa-session-durability--lease-record
          "*thief*" (plist-get lease :generation) 'active nil nil
          (plist-get lease :renewed-at))))
      (should (equal '(nil (user-error "Portable session lease belongs to another client"))
                     (test-niwa-durability-async--call
                      #'niwa-session-durability--update-owned-lease-async
                      session '(active) 'active 90)))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))))
  :doc "carries the launch-time commit riders across the asynchronous steps"
  (test-niwa-durability-async--with-lease (root session)
    (let* ((log (file-name-concat root "log"))
           (riders (list :before (list (list :op 'append :path log
                                             :content "x"))
                         :consumed nil)))
      (setf (niwa-session-lease session)
            (niwa-session-durability--strip-assumption
             (niwa-session-lease session)))
      (let ((niwa-session-durability--commit-riders riders))
        (let ((cell (list nil)))
          (niwa-session-durability--update-owned-lease-async
           session '(active) 'active 90
           (lambda (success error) (setcar cell (list success error))))
          ;; The launch binding ends before the program answers.
          (setq niwa-session-durability--commit-riders nil)
          (should (equal '(t nil)
                         (test-niwa-durability-async--await cell)))))
      (should (plist-get riders :consumed))
      (should (equal "x" (niwa-session-control-fs-read-file log)))))
  :doc "reports a failed program once and keeps the lease binding"
  (test-niwa-durability-async--with-lease (root session)
    (let ((bound (copy-sequence (niwa-session-lease session)))
          (calls 0)
          settled)
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (_operations callback &optional _scope)
                   (funcall callback nil '(file-error "boom"))
                   (funcall callback nil '(file-error "again")))))
        (niwa-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error)
           (cl-incf calls)
           (setq settled (list success error)))))
      (should (equal '(nil (file-error "boom")) settled))
      (should (= 1 calls))
      (should (equal bound (niwa-session-lease session)))))
  :doc "reports each launched process to the owner bound at launch"
  (test-niwa-durability-async--with-lease (root session)
    (let ((processes nil))
      (setf (niwa-session-lease session)
            (niwa-session-durability--strip-assumption
             (niwa-session-lease session)))
      (should (equal '(t nil)
                     (test-niwa-durability-async--launch
                      (lambda (callback)
                        (let ((niwa-session-durability-async-process-function
                               (lambda (process) (push process processes))))
                          (niwa-session-durability--update-owned-lease-async
                           session '(active) 'active 90 callback))))))
      ;; Observation, then commit: each its own process.
      (should (= 2 (length processes)))
      (should (cl-every #'processp processes))))
  :doc "does not call back twice when its callback signals"
  (test-niwa-durability-async--with-lease (root session)
    (let ((niwa-session-control-fs--async-wait t)
          (calls 0))
      (should-error
       (niwa-session-durability--update-owned-lease-async
        session '(active) 'active 90
        (lambda (_success _error)
          (cl-incf calls)
          (error "Callback failed"))))
      (should (= 1 calls))))
  :doc "waits for the transport while a remote operation is in flight"
  (test-niwa-durability-async--with-lease (root session)
    (let ((busy t)
          (cell (list nil))
          thunk)
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _path) busy))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (_key _path function &rest _)
                   (if busy
                       (setq thunk function)
                     (funcall function))
                   t)))
        (setf (niwa-session-lease session)
              (niwa-session-durability--strip-assumption
               (niwa-session-lease session)))
        (niwa-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error) (setcar cell (list success error))))
        (should thunk)
        (should-not (car cell))
        (setq busy nil)
        (funcall thunk)
        (should (equal '(t nil) (test-niwa-durability-async--await cell))))))
  :doc "fails the job when the transport refuses to wait"
  (test-niwa-durability-async--with-lease (root session)
    (let (settled)
      (cl-letf (((symbol-function 'niwa-transport-busy-p)
                 (lambda (&optional _path) t))
                ((symbol-function 'niwa-transport-run-when-idle)
                 (lambda (&rest _) nil)))
        (niwa-session-durability--update-owned-lease-async
         session '(active) 'active 90
         (lambda (success error) (setq settled (list success error)))))
      (should-not (car settled))
      (should (eq 'user-error (car (cadr settled))))))
  :doc "a quit in a continuation finishes the job once and never escapes the timer"
  (test-niwa-durability-async--with-lease (root session)
    (let ((settle (symbol-function 'niwa-session-durability--observe-lease-settle))
          (quits 0)
          (calls nil)
          (escaped 0)
          bound)
      (setf (niwa-session-lease session)
            (niwa-session-durability--strip-assumption
             (niwa-session-lease session)))
      (setq bound (copy-sequence (niwa-session-lease session)))
      (cl-letf (((symbol-function 'niwa-session-durability--observe-lease-settle)
                 (lambda (&rest arguments)
                   (cl-incf quits)
                   (signal 'quit nil)
                   (apply settle arguments))))
        (niwa-session-durability--update-owned-lease-async
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
      (should (equal bound (niwa-session-lease session)))))
  :doc "a quit while reporting the launched process finishes once, escapes nowhere, and the late result is inert"
  (test-niwa-durability-async--with-lease (root session)
    (let ((run (symbol-function 'niwa-session-control-fs-run-program-async))
          (launches 0)
          (escaped 0)
          (calls nil)
          bound)
      (setf (niwa-session-lease session)
            (niwa-session-durability--strip-assumption
             (niwa-session-lease session)))
      (setq bound (copy-sequence (niwa-session-lease session)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (operations callback &optional scope)
                   (cl-incf launches)
                   (funcall run operations callback scope))))
        (let ((niwa-session-durability-async-process-function
               (lambda (_process) (signal 'quit nil))))
          (condition-case nil
              (niwa-session-durability--update-owned-lease-async
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
      (should (equal bound (niwa-session-lease session))))))

(niwa-deftest niwa-session-durability--async-job ()
  ,test
  (test)
  :doc "finish calls back once"
  (let* (calls
         (job (niwa-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (car job) t nil)
    (funcall (car job) nil '(error "late"))
    (should (equal '((t nil)) calls)))
  :doc "an error in a guarded thunk finishes the job"
  (let* (calls
         (job (niwa-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (cadr job) (lambda () (error "Boom")))
    (should (equal '((nil (error "Boom"))) calls)))
  :doc "a quit finishes the job and returns without re-signalling"
  (let* (calls
         (job (niwa-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (should (eq 'returned (condition-case nil
                              (progn (funcall (cadr job) (lambda () (signal 'quit nil)))
                                     'returned)
                            (quit 'escaped))))
    (should (equal '((nil (quit))) calls)))
  :doc "a quit inside the callback is a warning, not an escape or a second call"
  (let* ((calls 0)
         diagnostics
         (job (niwa-session-durability--async-job
               (lambda (_success _error) (cl-incf calls) (signal 'quit nil)))))
    (niwa-test--with-captured-diagnostics diagnostics
      (should (eq 'returned (condition-case nil
                                (progn (funcall (cadr job) (lambda () (error "Boom")))
                                       (funcall (car job) t nil)
                                       'returned)
                              (quit 'escaped)))))
    (should (= 1 calls))
    (should (string-match-p "interrupted" diagnostics)))
  :doc "a quit after the job finished is a warning, not an escape"
  (let* (diagnostics
         (job (niwa-session-durability--async-job #'ignore)))
    (niwa-test--with-captured-diagnostics diagnostics
      (should (eq 'returned (condition-case nil
                                (progn
                                  (funcall (cadr job)
                                           (lambda ()
                                             (funcall (car job) t nil)
                                             (signal 'quit nil)))
                                  'returned)
                              (quit 'escaped)))))
    (should (string-match-p "interrupted" diagnostics)))
  :doc "a thunk after the job finished does not run"
  (let* (calls ran
         (job (niwa-session-durability--async-job
               (lambda (success error) (push (list success error) calls)))))
    (funcall (car job) nil '(error "aborted"))
    (funcall (cadr job) (lambda () (setq ran t)))
    (should-not ran)
    (should (= 1 (length calls))))
  :doc "an error from the callback itself propagates without a second call"
  (let* ((calls 0)
         (job (niwa-session-durability--async-job
               (lambda (_success _error) (cl-incf calls) (error "Callback failed")))))
    (should-error (funcall (cadr job) (lambda () (funcall (car job) t nil))))
    (should (= 1 calls))))

(niwa-deftest niwa-session-durability-commit-publication-head-async ()
  ,test
  (test)
  :doc "commits a head over a reserved lease and keeps it publishing"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (equal '(t nil)
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-reserve-publication-lease-async
                      session)))
      (should (eq 'publishing
                  (plist-get (niwa-session-lease session) :status)))
      (should (equal '(t nil)
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-commit-publication-head-async
                      session head)))
      (should (equal head (plist-get (niwa-session-lease session)
                                     :publication-head)))
      (should (eq 'publishing
                  (plist-get (niwa-session-lease session) :status)))))
  :doc "a final commit returns the lease to active in the same program"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head ".publications/0002/manifest.el"))
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-reserve-publication-lease-async
                    session)))
      (let ((niwa-session-durability--final-commit t))
        (should (equal '(t nil)
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-commit-publication-head-async
                        session head))))
      (should (eq 'active (plist-get (niwa-session-lease session) :status)))
      (should (equal head (plist-get (niwa-session-lease session)
                                     :publication-head)))))
  :doc "rejects a head that is not an immutable manifest without a program"
  (test-niwa-durability-async--with-lease (root session)
    (let (settled)
      (niwa-session-durability-commit-publication-head-async
       session "plain.el"
       (lambda (success error) (setq settled (list success error))))
      (should-not (car settled))
      (should (eq 'error (car (cadr settled))))))
  :doc "refuses a stale expected head and leaves the committed head alone"
  (test-niwa-durability-async--with-lease (root session)
    (progn
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-reserve-publication-lease-async
                    session)))
      (let ((bound (niwa-session-lease session)))
        (setf (niwa-session-lease session)
              (plist-put (copy-sequence bound) :publication-head
                         ".publications/stale/manifest.el")))
      (should (equal '(nil (user-error "Portable session publication head changed"))
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-commit-publication-head-async
                      session ".publications/0003/manifest.el"))))))

(niwa-deftest niwa-session-durability-reserve-publication-lease-async ()
  ,test
  (test)
  :doc "reserves the bounded publishing window through an asynchronous program"
  (test-niwa-durability-async--with-lease (root session)
    (should (equal '(t nil)
                   (test-niwa-durability-async--call
                    #'niwa-session-durability-reserve-publication-lease-async
                    session)))
    (should (eq 'publishing
                (plist-get (niwa-session-lease session) :status)))
    (should (eq 'owned (plist-get (niwa-session-lease session) :state))))
  :doc "declines an unseen target head without bytes and leaves the target unchanged"
  (test-niwa-durability-async--with-lease (root session)
    (let* ((directory (niwa-session-durability--lease-path root))
           (bound (plist-put
                   (niwa-session-durability--strip-assumption
                    (niwa-session-lease session))
                   :publication-head ".publications/unseen/manifest.el"))
           (path (niwa-session-durability--generation-path
                  directory (plist-get bound :generation))))
      (niwa-session-durability--write-generation directory bound)
      (let ((bytes (niwa-session-control-fs-read-file path)))
        (dolist (head '(nil ".publications/stale/manifest.el"))
          (setf (niwa-session-lease session) (copy-sequence bound)
                (niwa-session-publication session) (list :head head))
          (should (equal '(nil (user-error "Portable session publication head changed"))
                         (test-niwa-durability-async--call
                          #'niwa-session-durability-reserve-publication-lease-async
                          session)))
          (should (equal bytes (niwa-session-control-fs-read-file path))))))))

(niwa-deftest niwa-session-durability-finish-publication-lease-async ()
  ,test
  (test)
  :doc "returns a publishing lease to active and keeps the committed head"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-reserve-publication-lease-async
                    session)))
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-commit-publication-head-async
                    session head)))
      (should (equal '(t nil)
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-finish-publication-lease-async
                      session)))
      (should (eq 'active (plist-get (niwa-session-lease session) :status)))
      (should (equal head (plist-get (niwa-session-lease session)
                                     :publication-head)))))
  :doc "answers before returning when the lease is already active"
  (test-niwa-durability-async--with-lease (root session)
    (let (settled)
      (niwa-session-durability-finish-publication-lease-async
       session (lambda (success error) (setq settled (list success error))))
      (should (equal '(t nil) settled)))))

(niwa-deftest niwa-session-durability-observe-publication-head-async ()
  ,test
  (test)
  :doc "reports the newest head and ownership without changing the binding"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head ".publications/0001/manifest.el"))
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-reserve-publication-lease-async
                    session)))
      (should (car (test-niwa-durability-async--call
                    #'niwa-session-durability-commit-publication-head-async
                    session head)))
      (let* ((bound (copy-sequence (niwa-session-lease session)))
             (observed (car (test-niwa-durability-async--call
                             #'niwa-session-durability-observe-publication-head-async
                             session))))
        (should (equal head (plist-get observed :publication-head)))
        (should (plist-get observed :owned))
        (should (numberp (plist-get observed :now)))
        (should (equal bound (niwa-session-lease session))))))
  :doc "sees a newer foreign generation as the head and not owned"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (lease (niwa-session-lease session)))
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
        (niwa-session-durability--create-plist
         (niwa-session-durability--generation-path
          directory (1+ (plist-get lease :generation)))
         (niwa-session-durability--lease-record
          "*thief*" (1+ (plist-get lease :generation)) 'active
          ".publications/9999/manifest.el" nil
          (plist-get lease :renewed-at)))
        (let ((observed
               (car (test-niwa-durability-async--call
                     #'niwa-session-durability-observe-publication-head-async
                     session))))
          (should (equal ".publications/9999/manifest.el"
                         (plist-get observed :publication-head)))
          ;; The observer's own client id is the thief's here, so the
          ;; record is live authority for it; under the owner it is not.
          (should (plist-get observed :owned))))
      (let ((observed
             (car (test-niwa-durability-async--call
                   #'niwa-session-durability-observe-publication-head-async
                   session))))
        (should (equal ".publications/9999/manifest.el"
                       (plist-get observed :publication-head)))
        (should-not (plist-get observed :owned))))))

(niwa-deftest niwa-session-durability-admit-async ()
  ,test
  (test)
  :doc "admits a warm owned session from one program"
  (test-niwa-durability-async--with-lease (root session)
    (let ((programs 0)
          (run (symbol-function 'niwa-session-control-fs-run-program-async)))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (operations callback &optional scope)
                   (cl-incf programs)
                   (funcall run operations callback scope))))
        (niwa-session-durability-with-transaction
          (should (equal '(t nil)
                         (test-niwa-durability-async--call
                          #'niwa-session-durability-admit-async session)))
          ;; The program's PID-lock proof is recorded for the transaction.
          (should (member (niwa-session-control-fs-physical-path root)
                          (car niwa-session-durability--asserted-directories)))))
      (should (= 1 programs))))
  :doc "admits from one observed program without binding missing lease bytes"
  (test-niwa-durability-async--with-lease (root session)
    (let ((programs 0)
          (run (symbol-function 'niwa-session-control-fs-run-program-async))
          (bound (niwa-session-durability--strip-assumption
                  (niwa-session-lease session))))
      (setf (niwa-session-lease session) bound)
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                 (lambda (operations callback &optional scope)
                   (cl-incf programs)
                   (funcall run operations callback scope))))
        (should (equal '(t nil)
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session)))
        (should (eq bound (niwa-session-lease session)))
        (should-not (plist-get (niwa-session-lease session) :bytes)))
      (should (= 1 programs))))
  :doc "declines without bytes when the cached publication lags the owned target"
  (test-niwa-durability-async--with-lease (root session)
    (let* ((directory (niwa-session-durability--lease-path root))
           (bound (plist-put
                   (niwa-session-durability--strip-assumption
                    (niwa-session-lease session))
                   :publication-head ".publications/unseen/manifest.el")))
      (niwa-session-durability--write-generation directory bound)
      ;; A fallback may already know the target lease without its publication.
      (setf (niwa-session-lease session) bound)
      (dolist (head '(nil ".publications/stale/manifest.el"))
        (setf (niwa-session-publication session) (list :head head))
        (should (equal '(nil (user-error "Portable session publication head changed"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session)))
        (should (eq bound (niwa-session-lease session)))
        (should (equal head (plist-get (niwa-session-publication session)
                                      :head))))))
  :doc "declines when the bytes on the target differ from the bound ones"
  (test-niwa-durability-async--with-lease (root session)
    (progn
      (setf (niwa-session-lease session)
            (plist-put (copy-sequence (niwa-session-lease session))
                       :bytes "(:stale t)"))
      (should (equal '(nil (user-error "Portable session lease on the target differs from the bound lease"))
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-admit-async session)))))
  :doc "declines while a recovery marker root exists"
  (test-niwa-durability-async--with-lease (root session)
    (dolist (without-bytes '(nil t))
      (when without-bytes
        (setf (niwa-session-lease session)
              (niwa-session-durability--strip-assumption
               (niwa-session-lease session))))
      (make-directory (file-name-concat root ".recovery") t)
      (should (equal '(nil (user-error "Portable session has a pending recovery marker"))
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-admit-async session)))))
  :doc "reports a PID lock as an error"
  (test-niwa-durability-async--with-lease (root session)
    (progn
      (with-temp-file (file-name-concat root ".lock") (insert "1"))
      (let ((result (test-niwa-durability-async--call
                     #'niwa-session-durability-admit-async session)))
        (should-not (car result))
        (should (cadr result)))))
  :doc "declines while the session is quiescing or released for control transfer"
  (test-niwa-durability-async--with-lease (root session)
    (dolist (without-bytes '(nil t))
      (when without-bytes
        (setf (niwa-session-lease session)
              (niwa-session-durability--strip-assumption
               (niwa-session-lease session))))
      (dolist (state '(quiescing released))
        (setf (niwa-session-control-transfer session) (list :state state))
        (should (equal '(nil (user-error "Session is quiescing for cooperative control transfer"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session))))))
  :doc "declines without remembered bytes when the local lease is not owned"
  (test-niwa-durability-async--with-lease (root session)
    (setf (niwa-session-lease session)
          (plist-put (niwa-session-durability--strip-assumption
                      (niwa-session-lease session)) :state 'lost))
    (should (equal '(nil (user-error "Portable session lease is not owned by this client"))
                   (test-niwa-durability-async--call
                    #'niwa-session-durability-admit-async session))))
  :doc "declines when the lease directory cannot be observed"
  (test-niwa-durability-async--with-lease (root session)
    (progn
      (delete-directory (niwa-session-durability--lease-path root) t)
      (should (equal '(nil (user-error "Portable session lease directory could not be observed"))
                     (test-niwa-durability-async--call
                      #'niwa-session-durability-admit-async session)))))
  :doc "declines when a newer lease generation exists beside the owned one"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (lease (niwa-session-lease session)))
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
        (niwa-session-durability--create-plist
         (niwa-session-durability--generation-path
          directory (1+ (plist-get lease :generation)))
         (niwa-session-durability--lease-record
          "*thief*" (1+ (plist-get lease :generation)) 'active nil nil
          (plist-get lease :renewed-at))))
      (dolist (without-bytes '(nil t))
        (when without-bytes
          (setf (niwa-session-lease session)
                (niwa-session-durability--strip-assumption
                 (niwa-session-lease session))))
        (should (equal '(nil (user-error "A newer portable session lease generation exists"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session))))))
  :doc "declines when another client rewrote the bound generation's record"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (lease (niwa-session-lease session)))
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
        (niwa-session-durability--write-plist
         (niwa-session-durability--generation-path
          directory (plist-get lease :generation))
         (niwa-session-durability--lease-record
          "*thief*" (plist-get lease :generation) 'active nil nil
          (plist-get lease :renewed-at))))
      (dolist (without-bytes '(nil t))
        (when without-bytes
          (setf (niwa-session-lease session)
                (niwa-session-durability--strip-assumption
                 (niwa-session-lease session))))
        (should (equal '(nil (user-error "Portable session lease record is not this client's live lease"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session))))))
  :doc "declines an expired target lease with or without remembered bytes"
  (test-niwa-durability-async--with-lease (root session)
    (let ((directory (niwa-session-durability--lease-path root))
          (lease (copy-sequence (niwa-session-lease session))))
      (setf (plist-get lease :renewed-at) 1
            (plist-get lease :expires-at) 2)
      (niwa-session-durability--write-generation directory lease)
      (dolist (without-bytes '(nil t))
        (when without-bytes
          (setf (niwa-session-lease session)
                (niwa-session-durability--strip-assumption
                 (niwa-session-lease session))))
        (should (equal '(nil (user-error "Portable session lease record is not this client's live lease"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session))))))
  :doc "declines while a live control-transfer request names this client"
  (test-niwa-durability-async--with-lease (root session)
    (progn
      (let ((niwa-session-durability--client-id (make-string 64 ?b)))
        (should (niwa-session-transfer-request session "requester")))
      (dolist (without-bytes '(nil t))
        (when without-bytes
          (setf (niwa-session-lease session)
                (niwa-session-durability--strip-assumption
                 (niwa-session-lease session))))
        (should (equal '(nil (user-error "Portable session has a live control-transfer request"))
                       (test-niwa-durability-async--call
                        #'niwa-session-durability-admit-async session)))))))

(defmacro test-niwa-durability-async--gated (session gated &rest body)
  "Run BODY with the publication barrier stubbed to record its arguments.
GATED is a variable bound to the list of (SESSION TOKEN) calls.  The stub
signals a `user-error', so a gated operation must stop before any I/O."
  (declare (indent 2))
  `(let ((,gated nil))
     (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                (lambda (session &optional token)
                  (push (list session token) ,gated)
                  (user-error "Gated")))
               ((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (&rest _) (ert-fail "Gated operation reached target"))))
       ,@body)
     (should (equal (list (list ,session "token")) ,gated))))

(niwa-deftest niwa-session-durability--gate ()
  ,test
  (test)
  :doc "every public lease-changing operation barriers with the caller's token"
  (test-niwa-durability-async--with-lease (root session)
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-lease-acquire
                     root "x" session "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-call-with-abandoned-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-call-with-reserved-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-call-with-held-lease
                     session #'ignore "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-adopt-owned-lease
                     session session "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-forget-removed-session
                     session "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-set-unsettled-mutation
                     session t "token")
                    :type 'user-error))
    (test-niwa-durability-async--gated session gated
      (should-error (niwa-session-durability-lease-release
                     root session "token")
                    :type 'user-error))
    (should (niwa-session-durability-lease-owned-p session)))
  :doc "a call without a token reaches the barrier with a nil token"
  (test-niwa-durability-async--with-lease (root session)
    (let (seen)
      (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                 (lambda (_session &optional token)
                   (push token seen)
                   (user-error "Gated"))))
        (should-error (niwa-session-durability-call-with-reserved-lease
                       session #'ignore)
                      :type 'user-error))
      (should (equal '(nil) seen))))
  :doc "the reserved and held wrappers nest inside an enclosing synchronous window"
  (test-niwa-durability-async--with-lease (root session)
    (let (calls)
      (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                 (lambda (&rest _) (push t calls) (user-error "Gated"))))
        (setf (niwa-session-publication-active-p session) t)
        (should (eq 'done (niwa-session-durability-call-with-held-lease
                           session (lambda () 'done))))
        (setf (niwa-session-publication-active-p session) t)
        (should (eq 'done (niwa-session-durability-call-with-reserved-lease
                           session (lambda () 'done))))
        (should-not calls)
        ;; With an asynchronous job in the window, the gate applies.
        (setf (niwa-session-publication-active-p session) t
              (niwa-session-publication-job session) 'job)
        (should-error (niwa-session-durability-call-with-held-lease
                       session #'ignore)
                      :type 'user-error)
        (should calls)
        (setf (niwa-session-publication-active-p session) nil
              (niwa-session-publication-job session) nil))))
  :doc "inside a queued preparation only its token nests; others reach the barrier"
  (test-niwa-durability-async--with-lease (root session)
    (let ((token (list 'preparation))
          seen)
      (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                 (lambda (_session &optional given)
                   (push given seen)
                   (user-error "Gated"))))
        (setf (niwa-session-publication-active-p session) t
              (niwa-session-publication-preparation-token session) token)
        (unwind-protect
            (progn
              ;; The preparation's synchronous publication nests.
              (should (eq 'done (niwa-session-durability-call-with-held-lease
                                 session (lambda () 'done) token)))
              (should-not seen)
              (setf (niwa-session-publication-active-p session) t)
              ;; Without its token, or with another's, the gate applies.
              (should-error (niwa-session-durability-call-with-held-lease
                             session #'ignore)
                            :type 'user-error)
              (setf (niwa-session-publication-active-p session) t)
              (should-error (niwa-session-durability-call-with-reserved-lease
                             session #'ignore (list 'other))
                            :type 'user-error)
              (should (= 2 (length seen))))
          (setf (niwa-session-publication-active-p session) nil
                (niwa-session-publication-preparation-token session) nil)))))
  :doc "an unrelated timer cannot change the latch inside a running preparation"
  (test-niwa-durability-async--with-lease (root session)
    (let ((token (list 'preparation))
          (before (copy-sequence (niwa-session-lease session)))
          outcome
          timer)
      (setf (niwa-session-publication-active-p session) t
            (niwa-session-publication-preparation-token session) token)
      (unwind-protect
          (progn
            (setq timer
                  (run-at-time
                   0 nil
                   (lambda ()
                     (setq outcome
                           (condition-case err
                               (niwa-session-durability-set-unsettled-mutation
                                session t)
                             (user-error (list 'refused (cadr err))))))))
            (let ((steps 0))
              (while (and (null outcome) (< (cl-incf steps) 200))
                (accept-process-output nil 0.01)))
            (should (eq 'refused (car outcome)))
            (should (equal before (niwa-session-lease session)))
            ;; The preparation's own token is accepted.
            (should (niwa-session-durability-set-unsettled-mutation
                     session t token))
            (should (niwa-session-durability-unsettled-mutation-p session)))
        (when (timerp timer) (cancel-timer timer))
        (setf (niwa-session-publication-active-p session) nil
              (niwa-session-publication-preparation-token session) nil)))))

(defconst test-niwa-durability-async--updates
  (list (lambda (session) (niwa-session-durability-lease-renew session))
        (lambda (session)
          (niwa-session-durability-set-unsettled-mutation session t))
        (lambda (session)
          (car (test-niwa-durability-async--call
                #'niwa-session-durability--update-owned-lease-async
                session '(active publishing) 'active 90))))
  "The synchronous, unsettled-mutation and asynchronous lease updates.
Each takes the session and returns non-nil only when it committed.")

(niwa-deftest niwa-session-durability--bind-latest ()
  ,test
  (test)
  :doc "every update binds an unactivated claim beside the lease as contested"
  (dolist (update test-niwa-durability-async--updates)
    (dolist (without-bytes '(nil t))
      (test-niwa-durability-async--with-lease (root session)
        (let* ((directory (niwa-session-durability--lease-path root))
               (next (1+ (plist-get (niwa-session-lease session)
                                    :generation))))
          (let ((niwa-session-durability--client-id (make-string 64 ?b)))
            (niwa-session-durability--create-plist
             (niwa-session-durability--generation-path directory next)
             (niwa-session-durability--lease-record
              "*claimant*" next 'claiming nil nil
              (plist-get (niwa-session-lease session) :renewed-at))))
          (when without-bytes
            (setf (niwa-session-lease session)
                  (niwa-session-durability--strip-assumption
                   (niwa-session-lease session))))
          (should-not (funcall update session))
          (should (eq 'contested
                      (plist-get (niwa-session-lease session) :state)))
          (should (= next (plist-get (niwa-session-lease session)
                                     :generation)))))))
  :doc "every update reads past an aborted bound generation to the head"
  (dolist (update test-niwa-durability-async--updates)
    (test-niwa-durability-async--with-lease (root session)
      (let* ((directory (niwa-session-durability--lease-path root))
             (head (niwa-session-lease session))
             (aborted (niwa-session-durability--lease-record
                       "*owner*" (1+ (plist-get head :generation)) 'aborted
                       nil nil (plist-get head :renewed-at))))
        (niwa-session-durability--create-plist
         (niwa-session-durability--generation-path
          directory (plist-get aborted :generation))
         aborted)
        (niwa-session-durability--bind-lease session aborted 'owned)
        (should-not (funcall update session))
        ;; The older generation is still this client's live head.
        (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
        (should (equal (plist-get head :generation)
                       (plist-get (niwa-session-lease session)
                                  :generation)))))))

(niwa-deftest niwa-session-durability-bind-observed-lease ()
  ,test
  (test)
  :doc "binds an observed owned head with the bytes a commit of it wrote"
  (test-niwa-durability-async--with-lease (root session)
    (let* ((head (copy-sequence (niwa-session-lease session)))
           (observed (list :lease head :owned t)))
      (setf (niwa-session-lease session) nil)
      (should (eq 'owned (niwa-session-durability-bind-observed-lease
                          session observed)))
      (should (eq 'owned (plist-get (niwa-session-lease session) :state)))
      (should (equal (niwa-session-durability--record-bytes head)
                     (plist-get (niwa-session-lease session) :bytes)))
      (niwa-session-durability--cancel-renewal session)))
  :doc "binds an unactivated claim beside the lease as contested"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head (list :generation 2 :status 'claiming)))
      (should (eq 'contested (niwa-session-durability-bind-observed-lease
                              session (list :lease head :owned nil))))
      (should (eq 'contested
                  (plist-get (niwa-session-lease session) :state)))
      (should-not (plist-get (niwa-session-lease session) :bytes))
      (niwa-session-durability--cancel-renewal session)))
  :doc "binds any other unowned head as lost and drops the remembered bytes"
  (test-niwa-durability-async--with-lease (root session)
    (let ((head (list :generation 2 :status 'active
                      :client-id (make-string 64 ?b))))
      (should (eq 'lost (niwa-session-durability-bind-observed-lease
                         session (list :lease head :owned nil))))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
      (should-not (plist-get (niwa-session-lease session) :bytes)))))

(niwa-deftest niwa-session-durability-heartbeat/reclaim-discard
  (:doc "delayed async reclaim activation and abort verify the exact claiming record")
  (dolist (abort '(nil t))
    (test-niwa-durability-async--with-lease (root session)
      (let* ((directory (file-name-concat root ".lease"))
             (parent (niwa-session-control-fs-physical-path
                      (file-name-directory (directory-file-name root))))
             (head (niwa-session-durability--lease-head directory))
             (run (symbol-function 'niwa-session-control-fs-run-program-async))
             (claim nil)
             warnings
             (settled nil))
        (plist-put head :renewed-at 0)
        (plist-put head :expires-at 1)
        (niwa-session-durability--write-generation directory head)
        (niwa-test--with-captured-diagnostics warnings
          (cl-letf (((symbol-function 'niwa-session-control-fs-run-program-async)
                     (lambda (operations callback &optional scope lock-directory)
                       (let ((creating (eq 'create (plist-get (car operations) :op))))
                         (when creating
                           (should (equal parent lock-directory))
                           (setq claim (plist-get (car operations) :path))
                           (when abort
                             (niwa-session-durability--write-generation
                              directory (plist-put (copy-sequence head) :buffer "changed"))))
                         (when (and claim
                                    (eq 'verify (plist-get (car operations) :op))
                                    (equal claim (plist-get (car operations) :path)))
                           (setq settled t)
                           (should (equal parent lock-directory)))
                         (funcall run operations
                                  (lambda (results error)
                                    (when (and creating (not error))
                                      (niwa-session-control-fs-delete-file claim))
                                    (funcall callback results error))
                                  scope lock-directory)))))
            (niwa-session-durability-heartbeat session)
            (with-timeout (10 (ert-fail "Reclaim did not settle"))
              (while (niwa-session-lease-renewal-job session)
                (accept-process-output nil 0.01)))))
        (should (string-match-p "heartbeat failed" warnings))
        (ert-info ((format "Captured heartbeat diagnostics: %S" warnings))
          (should claim))
        (should settled)
        (should-not (file-exists-p claim))
        (should (eq 'lost (plist-get (niwa-session-lease session) :state)))))))

(niwa-deftest niwa-session-durability-heartbeat/reclaim-malformed
  (:doc "async reclaim refuses unreadable predecessors without pruning their evidence")
  (test-niwa-durability-async--with-lease (root session)
    (let* ((directory (file-name-concat root ".lease"))
           (head (niwa-session-durability--lease-head directory))
           (path (niwa-session-durability--generation-path directory 2))
           warnings)
      (plist-put head :renewed-at 0)
      (plist-put head :expires-at 1)
      (niwa-session-durability--write-generation directory head)
      (write-region "(:generation" nil path nil 'silent)
      (niwa-test--with-captured-diagnostics warnings
        (niwa-session-durability-heartbeat session)
        (with-timeout (10 (ert-fail "Reclaim did not settle"))
          (while (niwa-session-lease-renewal-job session)
            (accept-process-output nil 0.01))))
      (should (eq 'lost (plist-get (niwa-session-lease session) :state)))
      (should (file-exists-p path))
      (should (equal "(:generation"
                     (with-temp-buffer (insert-file-contents path) (buffer-string))))
      (should (equal head (niwa-session-durability--lease-head directory))))))

(niwa-deftest niwa-session-durability-heartbeat/reclaim-scratch
  (:doc "async own-expiry reclaim preserves both carrier scratch families")
  (dolist (scratch '(nil t))
    (test-niwa-durability-async--with-lease (root session)
      (let* ((directory (file-name-concat root ".lease"))
             (head (niwa-session-durability--lease-head directory))
             (staged (list nil))
             paths warnings)
        (plist-put head :renewed-at 0)
        (plist-put head :expires-at 1)
        (niwa-session-durability--write-generation directory head)
        (when scratch
          (niwa-session-control-fs--stage-payloads
           (list (list :op 'write
                       :path (niwa-session-durability--generation-path directory 2)
                       :content "crash payload")) staged)
          (should (car staged))
          (setq paths (cons (file-name-concat directory ".niwa-control-fs-Ab09Zx")
                            (car staged)))
          (write-region "crash payload" nil (car paths) nil 'silent))
        (niwa-test--with-captured-diagnostics warnings
          (niwa-session-durability-heartbeat session)
          (with-timeout (10 (ert-fail "Reclaim did not settle"))
            (while (niwa-session-lease-renewal-job session)
              (accept-process-output nil 0.01))))
        (ert-info ((format "Heartbeat diagnostics: %S" warnings))
          (should (eq 'owned (plist-get (niwa-session-lease session) :state))))
        (should (> (plist-get (niwa-session-lease session) :generation) 1))
        (dolist (path paths)
          (should (equal "crash payload"
                         (with-temp-buffer (insert-file-contents path) (buffer-string)))))))))

(provide 'test-niwa-session-durability-async)

;;; test-niwa-session-durability-async.el ends here
