;;; test-niwa-test-helpers.el --- Shared fixture tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise fixture cleanup on both successful and failing test bodies.

;;; Code:

(require 'niwa-execution-target)
(require 'niwa-artifact-store)
(require 'gptel-openai)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-engine-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-engine-test-support"))

(niwa-deftest niwa-eask--isolate-test-home ()
  ,test
  (test)
  :doc "isolates home without touching native caches when native compilation is unavailable"
  (with-temp-buffer
    (let* ((default-directory niwa-test--worktree-root)
           (status
            (call-process
             (expand-file-name invocation-name invocation-directory)
             nil t nil "--batch" "-Q" "--eval"
             (prin1-to-string
              `(progn
                 (require 'cl-lib)
                 (with-temp-buffer
                   (insert-file-contents "Eask")
                   (goto-char (point-min))
                   (search-forward "(defun niwa-eask--isolate-test-home")
                   (goto-char (match-beginning 0))
                   (eval (read (current-buffer)) t))
                 (cl-letf (((symbol-function 'native-comp-available-p) (lambda () nil))
                           ((symbol-function 'startup-redirect-eln-cache)
                            (lambda (&rest _) (error "Native cache is unavailable"))))
                   (niwa-eask--isolate-test-home))
                 (cl-assert (file-directory-p (getenv "HOME")))
                 (cl-assert (file-in-directory-p (getenv "XDG_RUNTIME_DIR")
                                                 (getenv "HOME")))
                 (cl-assert (null package-archives)))))))
      (ert-info ((buffer-string))
        (should (eql status 0))
        (should (equal "" (buffer-string))))))

  :doc "isolates native caches and ignores an archive being refreshed by another worker"
  (let* ((script (cadr (member "-scriptload" command-line-args)))
         (cache (make-temp-file "niwa-test-inherited-eln-" t))
         (packages (make-temp-file "niwa-test-archives-" t))
         (archive (file-name-concat packages "archives" "gnu" "archive-contents"))
         (startup (file-name-concat packages "startup.el"))
         (fixture (file-name-concat niwa-test--worktree-root
                                    "test/fixtures/eask-startup.el")))
    (unwind-protect
        (progn
          ;; Freeze the truncate-before-write window in a private archive tree.
          ;; Installed dependencies were activated before this hook runs.
          (make-directory (file-name-directory archive) t)
          (with-temp-file archive (insert "(1"))
          ;; --script loads before --eval.  Use a wrapper so setup really runs
          ;; before Eask, while preserving its script path and argument parsing.
          (with-temp-file startup
            (insert ";;; -*- lexical-binding: t -*-\n")
            (prin1 `(progn
                      ;; Seed the inherited path at the boundary under test,
                      ;; after Eask has loaded its own startup dependencies.
                      (advice-add 'niwa-eask--isolate-test-home :before
                                  (lambda ()
                                    (when (native-comp-available-p)
                                      (startup-redirect-eln-cache ,cache))))
                      (add-hook 'eask-before-test/ert-hook
                                (lambda () (setq package-user-dir ,packages)) t)
                      (setcar (cdr (member "-scriptload" command-line-args)) ,script)
                      (load ,script nil t))
                   (current-buffer)))
          (with-temp-buffer
            (let* ((default-directory niwa-test--worktree-root)
                   (process
                    (make-process
                     :name "niwa-eask-startup" :buffer (current-buffer)
                     :connection-type 'pipe :sentinel #'ignore :noquery t
                     :command
                     (list (expand-file-name invocation-name invocation-directory)
                           "--batch" "-Q" "--script" startup fixture))))
              (with-timeout (60 (delete-process process)
                               (ert-fail "Eask startup timed out"))
                (while (process-live-p process)
                  (accept-process-output process 0.05)))
              (accept-process-output process 0)
              (ert-info ((buffer-string))
                (should (zerop (process-exit-status process))))
              (should-not (directory-files cache nil directory-files-no-dot-files-regexp))))
          (should (equal "(1" (niwa-test--read-file archive))))
      (delete-directory cache t)
      (delete-directory packages t))))

(niwa-deftest niwa-deftest
  (:doc "isolates tool registries and cleans up even when a case fails")
  (let ((registry niwa-tool--registry)
        (cache niwa-tool--resolve-cache)
        (gptel--known-tools (list (list "fixture" (cons "probe" 'original)))))
    (puthash '("fixture" "probe") 'original registry)
    (puthash 'fixture 'original cache)
    (unwind-protect
        (dolist (fail '(nil t))
          (eval
           `(niwa-deftest niwa-test--nested-registry-case
              (:after-each (put 'niwa-test--nested-registry-case 'after-each t))
              (progn
                (niwa-tool-clear-registry)
                (setcdr (car gptel--known-tools) nil)
                (puthash 'fixture 'changed niwa-tool--resolve-cache)
                (let ((staged (make-temp-file "niwa-test-staged-" t)))
                  (put 'niwa-test--nested-registry-case 'staged staged)
                  (niwa-test--record-staged-publication
                   (list :directory staged)))
                (when ,fail (error "Deliberate fixture failure"))))
           t)
          (let ((body (ert-test-body
                       (ert-get-test 'niwa-test--nested-registry-case/test))))
            (if fail
                (should-error (funcall body) :type 'error)
              (funcall body)))
          (should (eq registry niwa-tool--registry))
          (should (eq 'original (gethash '("fixture" "probe") registry)))
          (should (equal gptel--known-tools '(("fixture" ("probe" . original)))))
          (should (eq 'original (gethash 'fixture cache)))
          (should (get 'niwa-test--nested-registry-case 'after-each))
          (should-not (file-exists-p
                       (get 'niwa-test--nested-registry-case 'staged)))
          (setplist 'niwa-test--nested-registry-case nil)
          (ert-delete-test 'niwa-test--nested-registry-case/test))
      (let ((staged (get 'niwa-test--nested-registry-case 'staged)))
        (when (and staged (file-directory-p staged))
          (delete-directory staged t)))
      (setplist 'niwa-test--nested-registry-case nil)
      (when (ert-test-boundp 'niwa-test--nested-registry-case/test)
        (ert-delete-test 'niwa-test--nested-registry-case/test)))))

(niwa-deftest niwa-test--assert-no-temporary-root-state ()
  ,test
  (test)
  :doc "accepts a clean temporary root"
  (let ((root (make-temp-file "niwa-test-root-guard-" t)))
    (unwind-protect
        (let ((temporary-file-directory root))
          (should-not (niwa-test--assert-no-temporary-root-state)))
      (delete-directory root t)))
  :doc "names the test and leaves Git and workspace evidence untouched"
  (let ((root (make-temp-file "niwa-test-root-guard-" t)))
    (unwind-protect
        (dolist (name '(".git" ".niwa"))
          (let* ((path (file-name-concat root name))
                 (evidence (file-name-concat path "evidence")))
            (make-directory path)
            (with-temp-file evidence (insert "Do not remove"))
            (let* ((temporary-file-directory root)
                   (err (should-error
                         (niwa-test--assert-no-temporary-root-state)
                         :type 'error))
                   (text (error-message-string err)))
              (should (string-match-p (regexp-quote path) text))
              (should (string-match-p
                       (regexp-quote
                        (symbol-name (ert-test-name (ert-running-test))))
                       text)))
            (should (equal "Do not remove"
                           (niwa-test--read-file evidence)))
            (delete-directory path t)))
      (delete-directory root t))))

(niwa-deftest niwa-test--with-idle-application
  (:doc "emulates idle with one-shot callbacks and restores bindings on every exit")
  (let ((idle (symbol-function 'current-idle-time))
        (schedule (symbol-function 'run-with-idle-timer)))
    (dolist (fail '(nil t))
      (let (timer delivered)
        (unwind-protect
            (progn
              (should
               (eq 'finished
                   (niwa-test--with-idle-application
                     (should (= 2 (float-time (current-idle-time))))
                     (setq timer (run-with-idle-timer
                                  3600 t (lambda (&rest args) (setq delivered args))
                                  'payload 42))
                     (should-not delivered)
                     'finished)))
              (should-not (timer--repeat-delay timer))
              ;; Delivery is progress, not the idle fixture's timing contract.
              (niwa-test--await 5 "Emulated idle callback did not run" delivered)
              (should (equal '(payload 42) delivered))
              (when fail
                (should-error
                 (niwa-test--with-idle-application
                   (error "Deliberate fixture failure")) :type 'error))
              (should (eq idle (symbol-function 'current-idle-time)))
              (should (eq schedule (symbol-function 'run-with-idle-timer))))
          (when timer (cancel-timer timer)))))))

(niwa-deftest niwa-test--cancel-stray-lease-timers ()
  ,test
  (test)
  :doc "retires delayed store notifications before another test can observe them"
  (let* ((niwa-artifact-store--content-changes (make-hash-table :test #'equal))
         (workspace (niwa-workspace--create :root "/fixture/")))
    (niwa-artifact-store--content-changed workspace)
    (let ((timer (gethash "/fixture/" niwa-artifact-store--content-changes)))
      (unwind-protect
          (progn
            (should (memq timer timer-list))
            (niwa-test--cancel-stray-lease-timers)
            (should-not (memq timer timer-list))
            (should (= 0 (hash-table-count niwa-artifact-store--content-changes))))
        (cancel-timer timer))))
  :doc "retires the installed lease heartbeat before another test can observe it"
  (niwa-engine-test--with-session
    (let ((timer (niwa-session-lease-renewal-timer session)))
      (should (timerp timer))
      (should (eq (timer--function timer) #'niwa-session-durability-heartbeat))
      (niwa-test--cancel-stray-lease-timers)
      (should-not (memq timer timer-list)))))

(niwa-deftest niwa-test--ensure-mock-tramp-method
  (:quiet t
   :doc "a fresh mock login establishes its own HOME under a blank client environment")
  (let* ((root (make-temp-file "niwa-mock-login-" t))
         (remote-root (format "/niwamock:%s:%s/" (system-name) root))
         (expected-home (getenv "HOME")))
    (unwind-protect
        (niwa-test--with-local-shell-tramp nil
          (tramp-cleanup-all-connections)
          (let ((default-directory remote-root)
                (process-environment nil))
            (with-temp-buffer
              (should (zerop (process-file "sh" nil t nil "-c"
                                           "printf '%s' \"$HOME\"")))
              (should (equal expected-home (buffer-string)))))
          (let* ((default-directory remote-root)
                 (file (make-nearby-temp-file "niwa-mock-owned-")))
            (unwind-protect
                (should (string-prefix-p
                         (file-name-as-directory temporary-file-directory)
                         (file-remote-p file 'localname)))
              (delete-file file))))
      (delete-directory root t))))

(niwa-deftest niwa-test--template ()
  ,test
  (test)
  :doc "rejects a second body mistaken for unbound template variables"
  (should-error
   (macroexpand '(niwa-test--template (should t) (should nil)))
   :type 'error)
  :doc "rejects case values without template variables"
  (should-error
   (macroexpand '(niwa-test--template (should t) nil 1))
   :type 'error)
  :doc "preserves single bodies and parameterized case values"
  (progn
    (should (equal '(("" (progn (should t) (should-not nil))))
                   (macroexpand
                    '(niwa-test--template
                      (progn (should t) (should-not nil))))))
    (should (equal '(("" (list 1)) ("second" (list 2)))
                   (macroexpand
                    '(niwa-test--template (list ,value) (value)
                       1 :doc "second" 2))))))

(niwa-deftest niwa-engine-test--with-session
  (:doc "cancels timers it owns and fails on, then cancels, any other new timer")
  (let (owned stray)
    (niwa-engine-test--with-session
      (push (run-at-time 0 nil #'niwa-goal--scheduled-continuation session buffer nil)
            owned)
      (push (run-at-time 0 nil #'niwa-view--run-follow-up-drain buffer) owned)
      (push (run-at-time 60 nil #'ignore session) owned))
    (should-not (cl-intersection owned timer-list))
    (should (string-prefix-p
             "Fixture left timers"
             (cadr (should-error
                    (niwa-engine-test--with-session
                      (setq stray (run-at-time 60 nil #'ignore)))))))
    (should-not (memq stray timer-list))))

(niwa-deftest niwa-test--check-write ()
  ,test
  (test)
  :doc "accepts owned paths and rejects siblings, traversal and symlink escapes"
  (let* ((root (make-temp-file "write-owner-" t))
         (outside (make-temp-file "write-outside-" t))
         (link (file-name-concat root "escape")))
    (make-symbolic-link outside link)
    (let ((niwa-test--write-roots (list root))
          (niwa-test--write-violations nil))
      (should-not (niwa-test--check-write
                   'probe (file-name-concat root "new" "file")))
      (dolist (path (list outside
                          (concat root "-sibling/file")
                          (file-name-concat root ".." "escape")
                          (file-name-concat link "not-created")))
        (should (string-match-p
                 "outside owned roots"
                 (error-message-string
                  (should-error (niwa-test--check-write 'probe path))))))
      (should (= 4 (length niwa-test--write-violations)))))
  :doc "requires a scoped allowance for a remote connection"
  (let ((niwa-test--write-roots '("/ssh:fixture:/owned/"))
        (niwa-test--write-violations nil))
    (should-not (niwa-test--check-write 'probe "/ssh:fixture:/owned/new"))
    (dolist (path '("/ssh:other:/owned/new" "/ssh:fixture:/owned-other/new"
                    "/ssh:fixture:/owned/../outside"))
      (should-error (niwa-test--check-write 'probe path)))
    (should (= 3 (length niwa-test--write-violations))))
  :doc "deletes owned symlink entries without following the final target"
  (let* ((root (make-temp-file "write-links-" t))
         (link (file-name-concat root "dangling")))
    (make-symbolic-link "/no-such-niwa-fixture-target" link)
    (let ((niwa-test--write-roots (list root))
          (niwa-test--write-violations nil))
      (should-error (write-region "escape" nil link nil 'silent))
      (should (= 1 (length niwa-test--write-violations)))
      (delete-file link))
    (should-not (file-symlink-p link)))
  :doc "guard bookkeeping does not call a product path-resolution stub"
  (let ((niwa-test--write-violations nil))
    (cl-letf (((symbol-function 'file-truename)
               (lambda (&rest _) (error "Product stub must not run"))))
      (should-not (niwa-test--check-write
                   'write-region (file-name-concat default-directory "new"))))))

(niwa-deftest niwa-test--guard-file-mutation
  (:doc "blocks actual file mutations before changing outside evidence")
  (let* ((root (make-temp-file "write-owner-" t))
         (outside (make-temp-file "write-outside-" t))
         (source (file-name-concat root "source"))
         (target (file-name-concat outside "evidence")))
    (with-temp-file source (insert "source"))
    (with-temp-file target (insert "evidence"))
    (let ((niwa-test--write-roots (list root))
          (niwa-test--write-violations nil))
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
      (should (= 10 (length niwa-test--write-violations))))
    (should (equal "evidence" (niwa-test--read-file target)))
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
          (let ((niwa-test--write-roots (list root))
                (niwa-test--write-violations nil))
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
      (should (equal "evidence" (niwa-test--read-file target))))
    ;; A blanket rejection of DIRECTORY/ would hide the escape too.  Keep the
    ;; same native operations working when their actual child stays owned.
    (let ((directory (file-name-concat root "owned-directory")))
      (make-directory directory)
      (dolist (operation '(rename-file add-name-to-file make-symbolic-link copy-file))
        (let* ((name (concat "directory-" (symbol-name operation)))
               (file (file-name-concat root name)))
          (with-temp-file file (insert "owned source"))
          (let ((niwa-test--write-roots (list root)))
            (funcall operation file (file-name-as-directory directory)))
          (should (equal "owned source"
                         (niwa-test--read-file
                          (file-name-concat directory name)))))))
    ;; Without DIRECTORY/ syntax, replacing a final symlink changes the owned
    ;; entry, not its target.  Keep these valid operations distinct from above.
    (dolist (operation '(rename-file add-name-to-file make-symbolic-link))
      (let ((file (file-name-concat root (concat "safe-" (symbol-name operation))))
            (link (file-name-concat root (concat "link-" (symbol-name operation)))))
        (with-temp-file file (insert "owned source"))
        (make-symbolic-link target link)
        (let ((niwa-test--write-roots (list root)))
          (funcall operation file link t))
        (should (equal "evidence" (niwa-test--read-file target)))
        (should (equal "owned source" (niwa-test--read-file link)))))
    ;; Copying into an owned directory can still follow its existing child
    ;; symlink.  Resolve the actual child rather than just the directory.
    (let* ((directory (file-name-concat root "copy-directory"))
           (child (file-name-concat directory (file-name-nondirectory source))))
      (make-directory directory)
      (make-symbolic-link target child)
      (let ((niwa-test--write-roots (list root))
            (niwa-test--write-violations nil))
        (should-error (copy-file source (file-name-as-directory directory) t))
        (should (= 1 (length niwa-test--write-violations))))
      (should (equal "evidence" (niwa-test--read-file target))))))

(niwa-deftest niwa-test--guard-temp-file
  (:doc "rejects explicit shared-temp prefixes without creating a file")
  (let* ((prefix (file-name-concat niwa-test--shared-temporary-directory
                                  "niwa-rejected-test-prefix-"))
         (before (file-expand-wildcards (concat prefix "*")))
         (niwa-test--write-violations nil))
    (should-error (make-temp-file prefix))
    (should (equal before (file-expand-wildcards (concat prefix "*"))))
    (should (equal 'make-temp-file (caar niwa-test--write-violations)))))

(niwa-deftest niwa-test--live-connections
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
          (should (memq server (niwa-test--live-connections)))
          (should-not (memq process (niwa-test--live-connections))))
      (when server (delete-process server))
      (when process (delete-process process)))))

(niwa-deftest niwa-test--refuse-provider-request
  (:doc "fails a real provider request on either gptel transport, naming the test")
  (let* ((gptel--known-backends nil)
         ;; A dead local port keeps a missing guard from reaching a provider.
         (gptel-backend (gptel-make-openai "guard" :host "127.0.0.1:9"
                          :protocol "http" :key "unused" :models '(m)))
         (gptel-model 'm)
         (name (symbol-name (ert-test-name (ert-running-test)))))
    (dolist (gptel-use-curl '(t nil))
      (with-temp-buffer
        (let ((err (should-error (gptel-request "hi" :callback #'ignore))))
          (should (string-match-p "real provider request"
                                  (error-message-string err)))
          (should (string-search name (error-message-string err))))))
    (should-error (gptel--url-retrieve "http://127.0.0.1:9/token" :method 'get))))

(niwa-deftest niwa-test--refuse-provider-process
  (:doc "refuses an installed provider CLI but launches a test-owned fake")
  (let ((niwa-test--provider-programs '("true")))
    (should (string-search
             (symbol-name (ert-test-name (ert-running-test)))
             (error-message-string
              (should-error (make-process :name "real" :command '("true"))))))
    (let ((fake (expand-file-name "true")))
      (write-region "#!/bin/sh\n" nil fake nil 'silent)
      (set-file-modes fake #o700)
      (let ((process (make-process :name "fake" :command (list fake)
                                   :noquery t :sentinel #'ignore)))
        (while (process-live-p process) (accept-process-output process 0.05))
        (should (zerop (process-exit-status process)))))))

(niwa-deftest niwa-test--call-with-isolation ()
  ,test
  (test)
  :doc "contains relative subprocess writes and removes private cwd on success or error"
  (dolist (fail '(nil t))
    (let ((this-command 'outer) root)
      (let ((body (lambda ()
                    (setq root default-directory)
                    (should-not this-command)
                    (setq this-command 'undo)
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
                      (should-error (niwa-test--call-with-isolation body)))))
          (niwa-test--call-with-isolation body)))
      (should (eq this-command 'outer))
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
                   (niwa-test--call-with-isolation
                    (lambda ()
                      (set variable '(leftover))
                      (when fail (error "Deliberate body failure"))))))))
        (should (equal before (symbol-value variable))))))
  :doc "remembers swallowed file guard errors and rejects shared temp writes"
  (let ((path (file-name-concat niwa-test--shared-temporary-directory
                                "niwa-must-not-create-fixture")))
    (should-not (file-exists-p path))
    (should (string-match-p
             ":file-mutations.*make-directory"
             (error-message-string
              (should-error
               (niwa-test--call-with-isolation
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
                     (niwa-test--call-with-isolation
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
  (let ((remote (format "/niwamock:%s:%s" (system-name) default-directory))
        leaked)
    (niwa-test--with-captured-diagnostics nil
      (should (string-match-p
               ":connections"
               (error-message-string
                (should-error
                 (niwa-test--call-with-isolation
                  (lambda ()
                    (let ((default-directory remote)
                          (tramp-histfile-override t))
                      (should (zerop (process-file "true")))
                      (setq leaked
                            (tramp-get-connection-process
                             (tramp-dissect-file-name remote))))
                    (error "Deliberate body failure")))))))
      (should-not (process-live-p leaked)))))

(niwa-deftest niwa-test--isolate-ert-case
  (:doc "direct ERT cases fail inside ERT and restore their original body")
  (let* ((body (lambda () (setq unread-command-events '(ert-leftover))))
         (test (make-ert-test :name 'niwa-test--direct-probe :body body))
         (result (ert-run-test test)))
    (should (ert-test-failed-p result))
    (should (string-match-p
             ":unread-events.*ert-leftover"
             (error-message-string
              (ert-test-result-with-condition-condition result))))
    (should (eq body (ert-test-body test)))
    (should-not unread-command-events)))

(provide 'test-niwa-test-helpers)
;;; test-niwa-test-helpers.el ends here
