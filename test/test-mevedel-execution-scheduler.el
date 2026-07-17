;;; test-mevedel-execution-scheduler.el --- Tests for Bash scheduling -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the fair session-scoped readers/writer scheduler used by Bash.

;;; Code:

(require 'cl-lib)
(require 'mevedel-execution)
(require 'mevedel-execution-scheduler)
(require 'mevedel-sandbox)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-execution-scheduler--session (root)
  "Return a test session rooted at ROOT."
  (mevedel-session-create
   "scheduler"
   (mevedel-workspace--create
    :type 'test :id root :root root :name "scheduler")))

(defun test-mevedel-execution-scheduler--wait (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE."
  (with-timeout ((or timeout 5) (error "Timed out"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.01))))

(mevedel-deftest mevedel-execution-scheduler-submit ()
  ,test
  (test)
  :doc "admits recognized readers concurrently"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started one two)
    (setq one
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'one started))))
    (setq two
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'two started))))
    (should (equal '(two one) started))
    (mevedel-execution-scheduler-release one)
    (mevedel-execution-scheduler-release two))
  :doc "serializes exclusive work"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started one two)
    (setq one
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'one started))))
    (setq two
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'two started))))
    (should (equal '(one) started))
    (mevedel-execution-scheduler-release one)
    (should (equal '(two one) started))
    (mevedel-execution-scheduler-release two))
  :doc "blocks later readers behind a waiting writer, then admits the reader wave"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started reader-one writer reader-two reader-three)
    (setq reader-one
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-one started))))
    (setq writer
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader-two
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-two started))))
    (setq reader-three
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-three started))))
    (should (equal '(reader-one) started))
    (mevedel-execution-scheduler-release reader-one)
    (should (equal '(writer reader-one) started))
    (mevedel-execution-scheduler-release writer)
    (should (equal '(reader-three reader-two writer reader-one) started))
    (mevedel-execution-scheduler-release reader-two)
    (mevedel-execution-scheduler-release reader-three))
  :doc "starts the complete reader wave when one start callback fails"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started writer failed reader next)
    (setq writer
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq failed
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (error "Start failed"))))
    (setq reader
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader started))))
    (mevedel-execution-scheduler-release writer)
    (should (equal '(reader writer) started))
    (setq next
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'next started))))
    (should-not (mevedel-execution-scheduler-release failed))
    (should (equal '(reader writer) started))
    (mevedel-execution-scheduler-release reader)
    (should (equal '(next reader writer) started))
    (mevedel-execution-scheduler-release next))
  :doc "releases a reader whose start callback exits nonlocally"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started writer reader next)
    (setq writer
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (throw 'start-abort nil))))
    (setq next
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'next started))))
    (catch 'start-abort
      (mevedel-execution-scheduler-release writer))
    (should-not (mevedel-execution-scheduler-release reader))
    (should (equal '(next writer) started))
    (mevedel-execution-scheduler-release next)))

(mevedel-deftest mevedel-execution-scheduler-cancel ()
  ,test
  (test)
  :doc "removes queued work and immediately drains newly eligible readers"
  (let ((scheduler (mevedel-execution-scheduler-create))
        started reader-one writer reader-two)
    (setq reader-one
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-one started))))
    (setq writer
          (mevedel-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader-two
          (mevedel-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-two started))))
    (should (mevedel-execution-scheduler-cancel writer))
    (should (equal '(reader-two reader-one) started))
    (should-not (mevedel-execution-scheduler-cancel reader-one))
    (mevedel-execution-scheduler-release reader-one)
    (mevedel-execution-scheduler-release reader-two)))

(mevedel-deftest mevedel-execution-start-bash/scheduler ()
  ,test
  (test)
  :doc "runs proven readers concurrently inside one session"
  (let* ((root (make-temp-file "mevedel-scheduler-readers-" t))
         (session (test-mevedel-execution-scheduler--session root))
         (release (file-name-concat root "release"))
         (one-path (file-name-concat root "one"))
         (two-path (file-name-concat root "two"))
         (mevedel-sandbox-mode 'off)
         one two)
    (unwind-protect
        (progn
          (cl-labels
              ((start-reader
                (path callback)
                (mevedel-execution-start-bash
                 callback
                 :session session :owner "main"
                 :command
                 (list "sh" "-c"
                       "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                       "scheduler-reader" path release)
                 :workdir root :writable-roots (list root)
                 :artifact-directory (file-name-concat root "artifacts")
                 :read-only-p t :yield-time-ms nil)))
            (start-reader one-path (lambda (value) (setq one value)))
            (start-reader two-path (lambda (value) (setq two value))))
          (test-mevedel-execution-scheduler--wait
           (lambda () (and (file-exists-p one-path)
                           (file-exists-p two-path)))
           0.3))
      (with-temp-file release)
      (when (or (file-exists-p one-path) (file-exists-p two-path))
        (test-mevedel-execution-scheduler--wait
         (lambda () (and one two))))
      (delete-directory root t)))
  :doc "request abort removes queued exclusive work without spawning it"
  (let* ((root (make-temp-file "mevedel-scheduler-abort-" t))
         (session (test-mevedel-execution-scheduler--session root))
         (request (mevedel-request--create :session session :origin "main"))
         (release (file-name-concat root "release"))
         (first-path (file-name-concat root "first"))
         (queued-path (file-name-concat root "queued"))
         (mevedel-sandbox-mode 'off)
         first queued
         (preparations 0))
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-blocker" first-path release)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (test-mevedel-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (let ((prepare (symbol-function 'mevedel-sandbox-prepare)))
            (cl-letf (((symbol-function 'mevedel-sandbox-prepare)
                       (lambda (&rest args)
                         (cl-incf preparations)
                         (apply prepare args))))
              (mevedel-execution-start-bash
               (lambda (value) (setq queued value))
               :session session :owner "main" :request request
               :command (list "sh" "-c" "touch \"$1\""
                              "scheduler-queued" queued-path)
               :workdir root :writable-roots (list root)
               :artifact-directory (file-name-concat root "artifacts")
               :yield-time-ms nil)
              (should (zerop preparations))
              (mevedel-request-drain-cancellers request)))
          (should queued)
          (should (zerop preparations))
          (should (eq 'aborted
                      (plist-get (plist-get queued :facts) :termination)))
          (should-not (file-exists-p queued-path)))
      (with-temp-file release)
      (test-mevedel-execution-scheduler--wait (lambda () first))
      (unless queued
        (test-mevedel-execution-scheduler--wait (lambda () queued)))
      (delete-directory root t)))
  :doc "shares one exclusive lane across owners and releases it on yield"
  (let* ((root (make-temp-file "mevedel-scheduler-yield-" t))
         (session (test-mevedel-execution-scheduler--session root))
         (stop (file-name-concat root "stop"))
         (first-path (file-name-concat root "first"))
         (second-path (file-name-concat root "second"))
         (mevedel-sandbox-mode 'off)
         first second final)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-yield" first-path stop)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms 100)
          (test-mevedel-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (mevedel-execution-start-bash
           (lambda (value) (setq second value))
           :session session :owner "agent--scheduler"
           :command (list "sh" "-c" "touch \"$1\""
                          "scheduler-second" second-path)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (accept-process-output nil 0.04)
          (should-not (file-exists-p second-path))
          (test-mevedel-execution-scheduler--wait
           (lambda () (and first second (file-exists-p second-path))))
          (should (eq 'running
                      (plist-get (plist-get first :facts) :state)))
          (with-temp-file stop)
          (mevedel-execution-observe
           session "main"
           (plist-get (plist-get first :facts) :execution-id)
           (lambda (value) (setq final value)) :wait-ms 5000)
          (test-mevedel-execution-scheduler--wait (lambda () final)))
      (unless (file-exists-p stop) (with-temp-file stop))
      (unless first
        (test-mevedel-execution-scheduler--wait (lambda () first)))
      (when (and (eq 'running (plist-get (plist-get first :facts) :state))
                 (not final))
        (mevedel-execution-observe
         session "main" (plist-get (plist-get first :facts) :execution-id)
         (lambda (value) (setq final value)) :wait-ms 5000)
        (test-mevedel-execution-scheduler--wait (lambda () final)))
      (unless second
        (test-mevedel-execution-scheduler--wait (lambda () second)))
      (delete-directory root t)))
  :doc "keeps exclusive admission isolated between sessions"
  (let* ((root (make-temp-file "mevedel-scheduler-sessions-" t))
         (session-one (test-mevedel-execution-scheduler--session root))
         (session-two (test-mevedel-execution-scheduler--session root))
         (release (file-name-concat root "release"))
         (first-path (file-name-concat root "first"))
         (second-path (file-name-concat root "second"))
         (mevedel-sandbox-mode 'off)
         first second)
    (unwind-protect
        (progn
          (mevedel-execution-start-bash
           (lambda (value) (setq first value))
           :session session-one :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-session-one" first-path release)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts-one")
           :yield-time-ms nil)
          (test-mevedel-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (mevedel-execution-start-bash
           (lambda (value) (setq second value))
           :session session-two :owner "main"
           :command (list "sh" "-c" "touch \"$1\""
                          "scheduler-session-two" second-path)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts-two")
           :yield-time-ms nil)
          (test-mevedel-execution-scheduler--wait (lambda () second) 0.3)
          (should (file-exists-p second-path))
          (should-not first))
      (with-temp-file release)
      (test-mevedel-execution-scheduler--wait (lambda () first))
      (unless second
        (test-mevedel-execution-scheduler--wait (lambda () second)))
      (delete-directory root t))))

(provide 'test-mevedel-execution-scheduler)
;;; test-mevedel-execution-scheduler.el ends here
