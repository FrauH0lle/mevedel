;;; test-niwa-execution-scheduler.el --- Tests for Bash scheduling -*- lexical-binding: t -*-

;;; Commentary:

;; Tests the fair session-scoped readers/writer scheduler used by Bash.

;;; Code:

(require 'cl-lib)
(require 'niwa-execution)
(require 'niwa-execution-scheduler)
(require 'niwa-sandbox)
(require 'niwa-session-artifacts)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-niwa-execution-scheduler--session (root)
  "Return a test session rooted at ROOT."
  (niwa-session-create
   "scheduler"
   (niwa-workspace--create
    ;; A real workspace category: starting a mutation resolves the session's
    ;; durable authority from it.
    :type 'file :id root :root root :name "scheduler")))

(defun test-niwa-execution-scheduler--wait (predicate &optional timeout)
  "Wait up to TIMEOUT seconds for PREDICATE."
  (with-timeout ((or timeout 5) (error "Timed out"))
    (while (not (funcall predicate))
      (accept-process-output nil 0.01))))

(niwa-deftest niwa-execution-scheduler-submit (:quiet t)
  ,test
  (test)
  :doc "admits recognized readers concurrently"
  (let ((scheduler (niwa-execution-scheduler-create))
        started one two)
    (setq one
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'one started))))
    (setq two
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'two started))))
    (should (equal '(two one) started))
    (niwa-execution-scheduler-release one)
    (niwa-execution-scheduler-release two))
  :doc "serializes exclusive work"
  (let ((scheduler (niwa-execution-scheduler-create))
        started one two)
    (setq one
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'one started))))
    (setq two
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'two started))))
    (should (equal '(one) started))
    (niwa-execution-scheduler-release one)
    (should (equal '(two one) started))
    (niwa-execution-scheduler-release two))
  :doc "blocks later readers behind a waiting writer, then admits the reader wave"
  (let ((scheduler (niwa-execution-scheduler-create))
        started reader-one writer reader-two reader-three)
    (setq reader-one
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-one started))))
    (setq writer
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader-two
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-two started))))
    (setq reader-three
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-three started))))
    (should (equal '(reader-one) started))
    (niwa-execution-scheduler-release reader-one)
    (should (equal '(writer reader-one) started))
    (niwa-execution-scheduler-release writer)
    (should (equal '(reader-three reader-two writer reader-one) started))
    (niwa-execution-scheduler-release reader-two)
    (niwa-execution-scheduler-release reader-three))
  :doc "starts the complete reader wave when one start callback fails"
  (let ((scheduler (niwa-execution-scheduler-create))
        started writer failed reader next)
    (setq writer
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq failed
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (error "Start failed"))))
    (setq reader
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader started))))
    (niwa-execution-scheduler-release writer)
    (should (equal '(reader writer) started))
    (setq next
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'next started))))
    (should-not (niwa-execution-scheduler-release failed))
    (should (equal '(reader writer) started))
    (niwa-execution-scheduler-release reader)
    (should (equal '(next reader writer) started))
    (niwa-execution-scheduler-release next))
  :doc "releases a reader whose start callback exits nonlocally"
  (let ((scheduler (niwa-execution-scheduler-create))
        started writer reader next)
    (setq writer
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (throw 'start-abort nil))))
    (setq next
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'next started))))
    (catch 'start-abort
      (niwa-execution-scheduler-release writer))
    (should-not (niwa-execution-scheduler-release reader))
    (should (equal '(next writer) started))
    (niwa-execution-scheduler-release next))
  :doc "rejects queued work whose owner became terminal before admission"
  (let ((scheduler (niwa-execution-scheduler-create))
        (owner-live-p t)
        started rejected first queued)
    (setq first
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'first started))))
    (setq queued
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'queued started))
           (lambda () owner-live-p)
           (lambda (_lease) (push 'queued rejected))))
    (setq owner-live-p nil)
    (niwa-execution-scheduler-release first)
    (should (equal '(first) started))
    (should (equal '(queued) rejected))
    (should-not (niwa-execution-scheduler-release queued)))
  :doc "attributes queued failures after the origin buffer dies"
  (let* ((root (make-temp-file "niwa-scheduler-scope-" t))
         (session (test-niwa-execution-scheduler--session root))
         (origin (generate-new-buffer " *scheduler-origin*"))
         (scheduler (niwa-execution-scheduler-create))
         (first (niwa-execution-scheduler-submit
                 scheduler 'exclusive #'ignore))
         diagnostics)
    (unwind-protect
        (progn
          (with-current-buffer origin
            (setq-local niwa--session session)
            (niwa-execution-scheduler-submit
             scheduler 'exclusive
             (lambda (_) (error "Start failed")) nil nil root)
            (niwa-execution-scheduler-submit
             scheduler 'exclusive #'ignore
             (lambda () (error "Admission failed"))
             (lambda (_) (error "Rejection failed")) root))
          (kill-buffer origin)
          (with-temp-buffer
            (setq-local niwa--session
                        (test-niwa-execution-scheduler--session "/other/project"))
            (niwa-test--with-captured-diagnostics diagnostics
              (niwa-execution-scheduler-release first)))
          (dolist (failure '("scheduler start" "admission check" "scheduler rejection"))
            (should (string-search
                     (format "niwa [%s]: execution %s failed:" root failure)
                     diagnostics))))
      (when (buffer-live-p origin) (kill-buffer origin))
      (delete-directory root t))))

(niwa-deftest niwa-execution-scheduler-cancel ()
  ,test
  (test)
  :doc "removes queued work and immediately drains newly eligible readers"
  (let ((scheduler (niwa-execution-scheduler-create))
        started reader-one writer reader-two)
    (setq reader-one
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-one started))))
    (setq writer
          (niwa-execution-scheduler-submit
           scheduler 'exclusive (lambda (_lease) (push 'writer started))))
    (setq reader-two
          (niwa-execution-scheduler-submit
           scheduler 'read (lambda (_lease) (push 'reader-two started))))
    (should (niwa-execution-scheduler-cancel writer))
    (should (equal '(reader-two reader-one) started))
    (should-not (niwa-execution-scheduler-cancel reader-one))
    (niwa-execution-scheduler-release reader-one)
    (niwa-execution-scheduler-release reader-two)))

(defun test-niwa-execution-scheduler--yield-across-owners ()
  "Check exclusive admission across owners and release on yield."
  (let* ((root (make-temp-file "niwa-scheduler-yield-" t))
         (session (test-niwa-execution-scheduler--session root))
         (stop (file-name-concat root "stop"))
         (first-path (file-name-concat root "first"))
         (second-path (file-name-concat root "second"))
         (niwa-sandbox-mode 'off)
         first second final record first-started second-started)
    (unwind-protect
        (progn
          (niwa-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-yield" first-path stop)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (setq first-started t)
          (test-niwa-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (maphash
           (lambda (_id candidate) (setq record candidate))
           (niwa-execution--state-records
            (niwa-session-execution-state session)))
          (should record)
          (niwa-execution-start-bash
           (lambda (value) (setq second value))
           :session session :owner "agent--scheduler"
           :command (list "sh" "-c" "touch \"$1\""
                          "scheduler-second" second-path)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (setq second-started t)
          (should-not (file-exists-p second-path))
          (niwa-execution--yield-managed record)
          (test-niwa-execution-scheduler--wait
           (lambda () (and first second (file-exists-p second-path))))
          (should (eq 'running
                      (plist-get (plist-get first :facts) :state)))
          (with-temp-file stop)
          (niwa-execution-observe
           session "main"
           (plist-get (plist-get first :facts) :execution-id)
           (lambda (value) (setq final value)) :wait-ms 5000)
          (test-niwa-execution-scheduler--wait (lambda () final)))
      (unless (file-exists-p stop) (with-temp-file stop))
      (when (and first-started (not first))
        (test-niwa-execution-scheduler--wait (lambda () first)))
      (when (and (eq 'running (plist-get (plist-get first :facts) :state))
                 (not final))
        (niwa-execution-observe
         session "main" (plist-get (plist-get first :facts) :execution-id)
         (lambda (value) (setq final value)) :wait-ms 5000)
        (test-niwa-execution-scheduler--wait (lambda () final)))
      (when (and second-started (not second))
        (test-niwa-execution-scheduler--wait (lambda () second)))
      (delete-directory root t))))

(niwa-deftest test-niwa-execution-scheduler--yield-across-owners ()
  ,test
  (test)
  :doc "preserves a pre-second failure without waiting for an unrequested callback"
  (let ((starts 0) (waits 0))
    (cl-letf (((symbol-function 'niwa-execution-start-bash)
               (lambda (callback &rest _)
                 (cl-incf starts)
                 (funcall callback '(:facts (:state completed)))))
              ((symbol-function 'test-niwa-execution-scheduler--wait)
               (lambda (&rest _)
                 (cl-incf waits)
                 (if (= waits 1)
                     (error "Injected pre-second failure")
                   (error "Cleanup masked primary failure")))))
      (should (equal '(error "Injected pre-second failure")
                     (should-error
                      (test-niwa-execution-scheduler--yield-across-owners)))))
    (should (= starts 1))
    (should (= waits 1)))
  :doc "does not wait for a callback when the first start signals before returning"
  (cl-letf (((symbol-function 'niwa-execution-start-bash)
             (lambda (&rest _) (error "Injected first-start failure")))
            ((symbol-function 'test-niwa-execution-scheduler--wait)
             (lambda (&rest _) (error "Cleanup masked start failure"))))
    (should (equal '(error "Injected first-start failure")
                   (should-error
                    (test-niwa-execution-scheduler--yield-across-owners))))))

(niwa-deftest niwa-execution-start-bash/scheduler ()
  ;; In-memory fixtures have no root buffer or save path for lease admission.
  (cl-letf (((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
             #'ignore)
            ((symbol-function 'niwa-session-durability-lease-owned-p)
             (lambda (&rest _) t)))
    ,test)
  (test)
  :doc "runs proven readers concurrently inside one session"
  (let* ((root (make-temp-file "niwa-scheduler-readers-" t))
         (session (test-niwa-execution-scheduler--session root))
         (release (file-name-concat root "release"))
         (one-path (file-name-concat root "one"))
         (two-path (file-name-concat root "two"))
         (niwa-sandbox-mode 'off)
         one two one-started two-started)
    (unwind-protect
        (progn
          (cl-labels
              ((start-reader
                (path callback)
                (niwa-execution-start-bash
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
            (setq one-started t)
            (start-reader two-path (lambda (value) (setq two value)))
            (setq two-started t))
          (test-niwa-execution-scheduler--wait
           (lambda () (and (file-exists-p one-path)
                           (file-exists-p two-path)))))
      ;; Both markers must exist before either child is released.  That proves
      ;; concurrency independently of shell startup latency under worker load.
      (with-temp-file release)
      (when (or one-started two-started)
        (test-niwa-execution-scheduler--wait
         (lambda () (and (or (not one-started) one)
                         (or (not two-started) two)))))
      (delete-directory root t)))
  :doc "request abort removes queued exclusive work without spawning it"
  (let* ((root (make-temp-file "niwa-scheduler-abort-" t))
         (session (test-niwa-execution-scheduler--session root))
         (request (niwa-request--create :session session :origin "main"))
         (release (file-name-concat root "release"))
         (first-path (file-name-concat root "first"))
         (queued-path (file-name-concat root "queued"))
         (niwa-sandbox-mode 'off)
         first queued
         (preparations 0))
    (unwind-protect
        (progn
          (niwa-execution-start-bash
           (lambda (value) (setq first value))
           :session session :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-blocker" first-path release)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts")
           :yield-time-ms nil)
          (test-niwa-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (let ((prepare (symbol-function 'niwa-sandbox-prepare)))
            (cl-letf (((symbol-function 'niwa-sandbox-prepare)
                       (lambda (&rest args)
                         (cl-incf preparations)
                         (apply prepare args))))
              (niwa-execution-start-bash
               (lambda (value) (setq queued value))
               :session session :owner "main" :request request
               :command (list "sh" "-c" "touch \"$1\""
                              "scheduler-queued" queued-path)
               :workdir root :writable-roots (list root)
               :artifact-directory (file-name-concat root "artifacts")
               :yield-time-ms nil)
              (should (zerop preparations))
              (niwa-request-drain-cancellers request)))
          (should queued)
          (should (zerop preparations))
          (should (eq 'aborted
                      (plist-get (plist-get queued :facts) :termination)))
          (should-not (file-exists-p queued-path)))
      (with-temp-file release)
      (test-niwa-execution-scheduler--wait (lambda () first))
      (unless queued
        (test-niwa-execution-scheduler--wait (lambda () queued)))
      (delete-directory root t)))
  :doc "shares one exclusive lane across owners and releases it on yield"
  (test-niwa-execution-scheduler--yield-across-owners)
  :doc "keeps exclusive admission isolated between sessions"
  (let* ((root (make-temp-file "niwa-scheduler-sessions-" t))
         (session-one (test-niwa-execution-scheduler--session root))
         (session-two (test-niwa-execution-scheduler--session root))
         (release (file-name-concat root "release"))
         (first-path (file-name-concat root "first"))
         (second-path (file-name-concat root "second"))
         (niwa-sandbox-mode 'off)
         first second)
    (unwind-protect
        (progn
          (niwa-execution-start-bash
           (lambda (value) (setq first value))
           :session session-one :owner "main"
           :command
           (list "sh" "-c"
                 "touch \"$1\"; while [ ! -e \"$2\" ]; do sleep .01; done"
                 "scheduler-session-one" first-path release)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts-one")
           :yield-time-ms nil)
          (test-niwa-execution-scheduler--wait
           (lambda () (file-exists-p first-path)))
          (niwa-execution-start-bash
           (lambda (value) (setq second value))
           :session session-two :owner "main"
           :command (list "sh" "-c" "touch \"$1\""
                          "scheduler-session-two" second-path)
           :workdir root :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts-two")
           :yield-time-ms nil)
          ;; The unreleased first process proves lane independence; callback
          ;; latency under parallel test load is not the contract.
          (test-niwa-execution-scheduler--wait (lambda () second))
          (should (file-exists-p second-path))
          (should-not (file-exists-p release))
          (should-not first))
      (with-temp-file release)
      (test-niwa-execution-scheduler--wait (lambda () first))
      (unless second
        (test-niwa-execution-scheduler--wait (lambda () second)))
      (delete-directory root t))))

(provide 'test-niwa-execution-scheduler)
;;; test-niwa-execution-scheduler.el ends here
