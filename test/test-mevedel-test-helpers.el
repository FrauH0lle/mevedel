;;; test-mevedel-test-helpers.el --- Shared fixture tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise fixture cleanup on both successful and failing test bodies.

;;; Code:

(require 'mevedel-execution-target)
(require 'mevedel-artifact-store)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-engine-test-support"))

(mevedel-deftest mevedel-eask--isolate-test-home
  (:doc "redirects native caches before a fresh Eask run loads dependencies")
  (let* ((script (cadr (member "-scriptload" command-line-args)))
         (cache (make-temp-file "mevedel-test-inherited-eln-" t))
         (fixture (file-name-concat mevedel-test--worktree-root
                                    "test/fixtures/eask-startup.el")))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory mevedel-test--worktree-root)
                 (process
                  (make-process
                   :name "mevedel-eask-startup" :buffer (current-buffer)
                   :connection-type 'pipe :sentinel #'ignore :noquery t
                   :command
                   (list (expand-file-name invocation-name invocation-directory)
                         "--batch" "-Q" "--eval"
                         (prin1-to-string
                          `(startup-redirect-eln-cache ,cache))
                         "--script" script fixture))))
            (with-timeout (60 (delete-process process)
                             (ert-fail "Eask startup timed out"))
              (while (process-live-p process)
                (accept-process-output process 0.05)))
            (accept-process-output process 0)
            (ert-info ((buffer-string))
              (should (zerop (process-exit-status process))))
            (should-not (directory-files cache nil directory-files-no-dot-files-regexp))))
      (delete-directory cache t))))

(mevedel-deftest mevedel-deftest
  (:doc "isolates tool registries and cleans up even when a case fails")
  (let ((registry mevedel-tool--registry)
        (cache mevedel-tool--resolve-cache)
        (gptel--known-tools (list (list "fixture" (cons "probe" 'original)))))
    (puthash '("fixture" "probe") 'original registry)
    (puthash 'fixture 'original cache)
    (unwind-protect
        (dolist (fail '(nil t))
          (eval
           `(mevedel-deftest mevedel-test--nested-registry-case
              (:after-each (put 'mevedel-test--nested-registry-case 'after-each t))
              (progn
                (mevedel-tool-clear-registry)
                (setcdr (car gptel--known-tools) nil)
                (puthash 'fixture 'changed mevedel-tool--resolve-cache)
                (let ((staged (make-temp-file "mevedel-test-staged-" t)))
                  (put 'mevedel-test--nested-registry-case 'staged staged)
                  (mevedel-test--record-staged-publication
                   (list :directory staged)))
                (when ,fail (error "Deliberate fixture failure"))))
           t)
          (let ((body (ert-test-body
                       (ert-get-test 'mevedel-test--nested-registry-case/test))))
            (if fail
                (should-error (funcall body) :type 'error)
              (funcall body)))
          (should (eq registry mevedel-tool--registry))
          (should (eq 'original (gethash '("fixture" "probe") registry)))
          (should (equal gptel--known-tools '(("fixture" ("probe" . original)))))
          (should (eq 'original (gethash 'fixture cache)))
          (should (get 'mevedel-test--nested-registry-case 'after-each))
          (should-not (file-exists-p
                       (get 'mevedel-test--nested-registry-case 'staged)))
          (setplist 'mevedel-test--nested-registry-case nil)
          (ert-delete-test 'mevedel-test--nested-registry-case/test))
      (let ((staged (get 'mevedel-test--nested-registry-case 'staged)))
        (when (and staged (file-directory-p staged))
          (delete-directory staged t)))
      (setplist 'mevedel-test--nested-registry-case nil)
      (when (ert-test-boundp 'mevedel-test--nested-registry-case/test)
        (ert-delete-test 'mevedel-test--nested-registry-case/test)))))

(mevedel-deftest mevedel-test--assert-no-temporary-root-state ()
  ,test
  (test)
  :doc "accepts a clean temporary root"
  (let ((root (make-temp-file "mevedel-test-root-guard-" t)))
    (unwind-protect
        (let ((temporary-file-directory root))
          (should-not (mevedel-test--assert-no-temporary-root-state)))
      (delete-directory root t)))
  :doc "names the test and leaves Git and workspace evidence untouched"
  (let ((root (make-temp-file "mevedel-test-root-guard-" t)))
    (unwind-protect
        (dolist (name '(".git" ".mevedel"))
          (let* ((path (file-name-concat root name))
                 (evidence (file-name-concat path "evidence")))
            (make-directory path)
            (with-temp-file evidence (insert "Do not remove"))
            (let* ((temporary-file-directory root)
                   (err (should-error
                         (mevedel-test--assert-no-temporary-root-state)
                         :type 'error))
                   (text (error-message-string err)))
              (should (string-match-p (regexp-quote path) text))
              (should (string-match-p
                       (regexp-quote
                        (symbol-name (ert-test-name (ert-running-test))))
                       text)))
            (should (equal "Do not remove"
                           (mevedel-test--read-file evidence)))
            (delete-directory path t)))
      (delete-directory root t))))

(mevedel-deftest mevedel-test--cancel-stray-lease-timers
  (:doc "retires delayed store notifications before another test can observe them")
  (let* ((mevedel-artifact-store--content-changes (make-hash-table :test #'equal))
         (workspace (mevedel-workspace--create :root "/fixture/")))
    (mevedel-artifact-store--content-changed workspace)
    (let ((timer (gethash "/fixture/" mevedel-artifact-store--content-changes)))
      (unwind-protect
          (progn
            (should (memq timer timer-list))
            (mevedel-test--cancel-stray-lease-timers)
            (should-not (memq timer timer-list))
            (should (= 0 (hash-table-count mevedel-artifact-store--content-changes))))
        (cancel-timer timer)))))

(mevedel-deftest mevedel-test--ensure-mock-tramp-method
  (:quiet t
   :doc "a fresh mock login establishes its own HOME under a blank client environment")
  (let* ((root (make-temp-file "mevedel-mock-login-" t))
         (remote-root (format "/mevedelmock:%s:%s/" (system-name) root))
         (expected-home (getenv "HOME")))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp nil
          (tramp-cleanup-all-connections)
          (let ((default-directory remote-root)
                (process-environment nil))
            (with-temp-buffer
              (should (zerop (process-file "sh" nil t nil "-c"
                                           "printf '%s' \"$HOME\"")))
              (should (equal expected-home (buffer-string))))))
      (delete-directory root t))))

(mevedel-deftest mevedel-test--template ()
  ,test
  (test)
  :doc "rejects a second body mistaken for unbound template variables"
  (should-error
   (macroexpand '(mevedel-test--template (should t) (should nil)))
   :type 'error)
  :doc "rejects case values without template variables"
  (should-error
   (macroexpand '(mevedel-test--template (should t) nil 1))
   :type 'error)
  :doc "preserves single bodies and parameterized case values"
  (progn
    (should (equal '(("" (progn (should t) (should-not nil))))
                   (macroexpand
                    '(mevedel-test--template
                      (progn (should t) (should-not nil))))))
    (should (equal '(("" (list 1)) ("second" (list 2)))
                   (macroexpand
                    '(mevedel-test--template (list ,value) (value)
                       1 :doc "second" 2))))))

(mevedel-deftest mevedel-engine-test--with-session
  (:doc "cancels timers it owns and fails on, then cancels, any other new timer")
  (let (owned stray)
    (mevedel-engine-test--with-session
      (push (run-at-time 0 nil #'mevedel-goal--scheduled-continuation session buffer nil)
            owned)
      (push (run-at-time 0 nil #'mevedel-view--run-follow-up-drain buffer) owned)
      (push (run-at-time 60 nil #'ignore session) owned))
    (should-not (cl-intersection owned timer-list))
    (should (string-prefix-p
             "Fixture left timers"
             (cadr (should-error
                    (mevedel-engine-test--with-session
                      (setq stray (run-at-time 60 nil #'ignore)))))))
    (should-not (memq stray timer-list))))

(provide 'test-mevedel-test-helpers)
;;; test-mevedel-test-helpers.el ends here
