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
              (should (equal expected-home (buffer-string)))))
          (let* ((default-directory remote-root)
                 (file (make-nearby-temp-file "mevedel-mock-owned-")))
            (unwind-protect
                (should (string-prefix-p
                         (file-name-as-directory temporary-file-directory)
                         (file-remote-p file 'localname)))
              (delete-file file))))
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

(mevedel-deftest mevedel-test--check-write ()
  ,test
  (test)
  :doc "accepts owned paths and rejects siblings, traversal and symlink escapes"
  (let* ((root (make-temp-file "write-owner-" t))
         (outside (make-temp-file "write-outside-" t))
         (link (file-name-concat root "escape")))
    (make-symbolic-link outside link)
    (let ((mevedel-test--write-roots (list root))
          (mevedel-test--write-violations nil))
      (should-not (mevedel-test--check-write
                   'probe (file-name-concat root "new" "file")))
      (dolist (path (list outside
                          (concat root "-sibling/file")
                          (file-name-concat root ".." "escape")
                          (file-name-concat link "not-created")))
        (should (string-match-p
                 "outside owned roots"
                 (error-message-string
                  (should-error (mevedel-test--check-write 'probe path))))))
      (should (= 4 (length mevedel-test--write-violations)))))
  :doc "requires a scoped allowance for a remote connection"
  (let ((mevedel-test--write-roots '("/ssh:fixture:/owned/"))
        (mevedel-test--write-violations nil))
    (should-not (mevedel-test--check-write 'probe "/ssh:fixture:/owned/new"))
    (dolist (path '("/ssh:other:/owned/new" "/ssh:fixture:/owned-other/new"
                    "/ssh:fixture:/owned/../outside"))
      (should-error (mevedel-test--check-write 'probe path)))
    (should (= 3 (length mevedel-test--write-violations))))
  :doc "deletes owned symlink entries without following the final target"
  (let* ((root (make-temp-file "write-links-" t))
         (link (file-name-concat root "dangling")))
    (make-symbolic-link "/no-such-mevedel-fixture-target" link)
    (let ((mevedel-test--write-roots (list root))
          (mevedel-test--write-violations nil))
      (should-error (write-region "escape" nil link nil 'silent))
      (should (= 1 (length mevedel-test--write-violations)))
      (delete-file link))
    (should-not (file-symlink-p link)))
  :doc "guard bookkeeping does not call a product path-resolution stub"
  (let ((mevedel-test--write-violations nil))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _) (error "Product stub must not run"))))
      (should-not (mevedel-test--check-write
                   'write-region (file-name-concat default-directory "new"))))))

(mevedel-deftest mevedel-test--guard-file-mutation
  (:doc "blocks actual file mutations before changing outside evidence")
  (let* ((root (make-temp-file "write-owner-" t))
         (outside (make-temp-file "write-outside-" t))
         (source (file-name-concat root "source"))
         (target (file-name-concat outside "evidence")))
    (with-temp-file source (insert "source"))
    (with-temp-file target (insert "evidence"))
    (let ((mevedel-test--write-roots (list root))
          (mevedel-test--write-violations nil))
      (should-error (write-region "changed" nil target nil 'silent))
      (should-error (make-directory (file-name-concat outside ".git")))
      (should-error (copy-file source target t))
      (should-error (rename-file source target t))
      (should-error (delete-file target))
      (should-error (delete-directory outside t))
      (should-error (make-symbolic-link source (file-name-concat outside "link")))
      (should-error (add-name-to-file source (file-name-concat outside "hardlink")))
      (should-error (set-file-modes target #o600))
      (should-error (set-file-times target))
      (should (= 10 (length mevedel-test--write-violations))))
    (should (equal "evidence" (mevedel-test--read-file target)))
    (should (file-exists-p source))
    (should-not (file-exists-p (file-name-concat outside ".git")))
    ;; DIRECTORY/ destinations name a child, not the directory entry itself.
    ;; Exercise all operations before asserting, so a broken guard reproduces
    ;; every escape using only this case's disposable outside directory.
    (let ((directory-link (file-name-concat root "outside-directory"))
          rejections)
      (make-symbolic-link outside directory-link)
      (dolist (operation '(rename-file add-name-to-file make-symbolic-link copy-file))
        (let ((file (file-name-concat root (symbol-name operation))))
          (with-temp-file file (insert "owned source"))
          (let ((mevedel-test--write-roots (list root))
                (mevedel-test--write-violations nil))
            (push (condition-case err
                      (progn
                        (funcall operation file (file-name-as-directory directory-link))
                        nil)
                    (error (error-message-string err)))
                  rejections))))
      (should (cl-every (lambda (text)
                          (and text (string-match-p "outside owned roots" text)))
                        rejections))
      (should (equal '("evidence")
                     (directory-files outside nil directory-files-no-dot-files-regexp)))
      (should (equal "evidence" (mevedel-test--read-file target))))
    ;; A blanket rejection of DIRECTORY/ would hide the escape too.  Keep the
    ;; same native operations working when their actual child stays owned.
    (let ((directory (file-name-concat root "owned-directory")))
      (make-directory directory)
      (dolist (operation '(rename-file add-name-to-file make-symbolic-link copy-file))
        (let* ((name (concat "directory-" (symbol-name operation)))
               (file (file-name-concat root name)))
          (with-temp-file file (insert "owned source"))
          (let ((mevedel-test--write-roots (list root)))
            (funcall operation file (file-name-as-directory directory)))
          (should (equal "owned source"
                         (mevedel-test--read-file
                          (file-name-concat directory name)))))))
    ;; Without DIRECTORY/ syntax, replacing a final symlink changes the owned
    ;; entry, not its target.  Keep these valid operations distinct from above.
    (dolist (operation '(rename-file add-name-to-file make-symbolic-link))
      (let ((file (file-name-concat root (concat "safe-" (symbol-name operation))))
            (link (file-name-concat root (concat "link-" (symbol-name operation)))))
        (with-temp-file file (insert "owned source"))
        (make-symbolic-link target link)
        (let ((mevedel-test--write-roots (list root)))
          (funcall operation file link t))
        (should (equal "evidence" (mevedel-test--read-file target)))
        (should (equal "owned source" (mevedel-test--read-file link)))))
    ;; Copying into an owned directory can still follow its existing child
    ;; symlink.  Resolve the actual child rather than just the directory.
    (let* ((directory (file-name-concat root "copy-directory"))
           (child (file-name-concat directory (file-name-nondirectory source))))
      (make-directory directory)
      (make-symbolic-link target child)
      (let ((mevedel-test--write-roots (list root))
            (mevedel-test--write-violations nil))
        (should-error (copy-file source (file-name-as-directory directory) t))
        (should (= 1 (length mevedel-test--write-violations))))
      (should (equal "evidence" (mevedel-test--read-file target))))))

(mevedel-deftest mevedel-test--guard-temp-file
  (:doc "rejects explicit shared-temp prefixes without creating a file")
  (let* ((prefix (file-name-concat mevedel-test--shared-temporary-directory
                                  "mevedel-rejected-test-prefix-"))
         (before (file-expand-wildcards (concat prefix "*")))
         (mevedel-test--write-violations nil))
    (should-error (make-temp-file prefix))
    (should (equal before (file-expand-wildcards (concat prefix "*"))))
    (should (equal 'make-temp-file (caar mevedel-test--write-violations)))))

(mevedel-deftest mevedel-test--live-connections
  (:doc "observes real network listeners without including ordinary subprocesses")
  (let (server process)
    (unwind-protect
        (progn
          (setq server (make-network-process :name "fixture-listener"
                                             :server t :host 'local :service t
                                             :family 'ipv4 :noquery t)
                process (make-process :name "fixture-subprocess"
                                      :command '("cat") :connection-type 'pipe
                                      :noquery t :sentinel #'ignore))
          (should (memq server (mevedel-test--live-connections)))
          (should-not (memq process (mevedel-test--live-connections))))
      (when server (delete-process server))
      (when process (delete-process process)))))

(mevedel-deftest mevedel-test--call-with-isolation ()
  ,test
  (test)
  :doc "contains relative subprocess writes and removes private cwd on success or error"
  (dolist (fail '(nil t))
    (let (root)
      (let ((body (lambda ()
                    (setq root default-directory)
                    (should (equal root temporary-file-directory))
                    (should (equal root (getenv "TMPDIR")))
                    (should (zerop (call-process "sh" nil nil nil "-c"
                                                 "mkdir .git; printf test > relative")))
                    (should (file-directory-p (file-name-concat root ".git")))
                    (when fail (error "Deliberate body failure")))))
        (if fail
            (should (string-match-p
                     "Deliberate body failure"
                     (error-message-string
                      (should-error (mevedel-test--call-with-isolation body)))))
          (mevedel-test--call-with-isolation body)))
      (should-not (file-exists-p root))))
  :doc "reports unread events, restores the baseline and runs after failed bodies"
  (dolist (variable '(unread-command-events unread-input-method-events
                      unread-post-input-method-events))
    (let ((before (copy-tree (symbol-value variable))))
      (dolist (fail '(nil t))
        (should (string-match-p
                 (format ":unread-events.*%s.*leftover" variable)
                 (error-message-string
                  (should-error
                   (mevedel-test--call-with-isolation
                    (lambda ()
                      (set variable '(leftover))
                      (when fail (error "Deliberate body failure"))))))))
        (should (equal before (symbol-value variable))))))
  :doc "remembers swallowed file guard errors and rejects shared temp writes"
  (let ((path (file-name-concat mevedel-test--shared-temporary-directory
                                "mevedel-must-not-create-fixture")))
    (should-not (file-exists-p path))
    (should (string-match-p
             ":file-mutations.*make-directory"
             (error-message-string
              (should-error
               (mevedel-test--call-with-isolation
                (lambda () (ignore-errors (make-directory path))))))))
    (should-not (file-exists-p path)))
  :doc "reports and closes a new network connection but preserves the baseline"
  (let ((baseline (make-network-process :name "fixture-baseline" :server t
                                        :host 'local :service t :family 'ipv4
                                        :noquery t))
        leaked)
    (unwind-protect
        (progn
          (should (string-match-p
                   ":connections.*fixture-leaked"
                   (error-message-string
                    (should-error
                     (mevedel-test--call-with-isolation
                      (lambda ()
                        (setq leaked
                              (make-network-process
                               :name "fixture-leaked" :server t :host 'local
                               :service t :family 'ipv4 :noquery t))))))))
          (should-not (process-live-p leaked))
          (should (process-live-p baseline)))
      (delete-process baseline)
      (when (process-live-p leaked) (delete-process leaked))))
  :doc "reports and closes a real mock TRAMP connection on a failed body"
  (let ((remote (format "/mevedelmock:%s:%s" (system-name) default-directory))
        leaked)
    (mevedel-test--with-captured-diagnostics nil
      (should (string-match-p
               ":connections"
               (error-message-string
                (should-error
                 (mevedel-test--call-with-isolation
                  (lambda ()
                    (let ((default-directory remote)
                          (tramp-histfile-override t))
                      (should (zerop (process-file "true")))
                      (setq leaked
                            (tramp-get-connection-process
                             (tramp-dissect-file-name remote))))
                    (error "Deliberate body failure")))))))
      (should-not (process-live-p leaked)))))

(mevedel-deftest mevedel-test--isolate-ert-case
  (:doc "direct ERT cases fail inside ERT and restore their original body")
  (let* ((body (lambda () (setq unread-command-events '(ert-leftover))))
         (test (make-ert-test :name 'mevedel-test--direct-probe :body body))
         (result (ert-run-test test)))
    (should (ert-test-failed-p result))
    (should (string-match-p
             ":unread-events.*ert-leftover"
             (error-message-string
              (ert-test-result-with-condition-condition result))))
    (should (eq body (ert-test-body test)))
    (should-not unread-command-events)))

(provide 'test-mevedel-test-helpers)
;;; test-mevedel-test-helpers.el ends here
