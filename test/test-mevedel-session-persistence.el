;;; test-mevedel-session-persistence.el --- Session lifecycle facade tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `mevedel-session-persistence'.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-session-test-support"))


(mevedel-deftest mevedel-session-persistence-write-current-buffer-atomically ()
  ,test
  (test)
  :doc "publishes current buffer contents through a same-directory rename"
  (let ((path (make-temp-file "mevedel-transcript-atomic-")))
    (unwind-protect
        (with-temp-buffer
          (insert "replacement")
          (mevedel-session-persistence-write-current-buffer-atomically path)
          (should
           (equal "replacement"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (when (file-exists-p path) (delete-file path))))
  :doc "preserves the original when publication fails"
  (let* ((root (make-temp-file "mevedel-transcript-atomic-" t))
         (path (file-name-concat root "segment.org")))
    (unwind-protect
        (progn
          (write-region "original" nil path nil 'silent)
          ;; A read-only parent stops the control filesystem before its
          ;; rename, which is the only way the published bytes can appear.
          (set-file-modes root #o500)
          (with-temp-buffer
            (insert "replacement")
            (should-error
             (mevedel-session-persistence-write-current-buffer-atomically
              path)))
          (should
           (equal "original"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (set-file-modes root #o700)
      (delete-directory root t))))


(mevedel-deftest mevedel-session-persistence-allocate-session-id ()
  ,test
  (test)
  :doc "retries until the generated id has no session directory"
  (let ((sessions-dir (make-temp-file "mevedel-id-allocation-" t))
        (calls 0))
    (unwind-protect
        (progn
          (write-region "occupied\n" nil
                        (file-name-concat sessions-dir "taken") nil 'silent)
          (cl-letf (((symbol-function
                      'mevedel-session-artifacts-compute-id)
                     (lambda (_name)
                       (if (= (cl-incf calls) 1) "taken" "fresh"))))
            (should (equal
                     "fresh"
                     (mevedel-session-persistence-allocate-session-id
                      "main" sessions-dir)))
            (should (= calls 2))))
      (delete-directory sessions-dir t))))


(mevedel-deftest mevedel-session-persistence-first-user-message ()
  ,test
  (test)
  :doc "extracts first non-blank line of first user region"
  (with-temp-buffer
    (insert "Refactor the permission chain\n\nMore details follow.")
    (should (equal "Refactor the permission chain"
                   (mevedel-session-persistence-first-user-message
                    (current-buffer)))))
  :doc "skips assistant response regions"
  (with-temp-buffer
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "What about edge cases?\n")
    (should (equal "What about edge cases?"
                   (mevedel-session-persistence-first-user-message
                    (current-buffer)))))
  :doc "returns nil for buffers with no user content"
  (with-temp-buffer
    (insert (propertize "All response.\n" 'gptel 'response))
    (should (null (mevedel-session-persistence-first-user-message
                   (current-buffer)))))
  :doc "truncates long lines"
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (let ((preview (mevedel-session-persistence-first-user-message
                    (current-buffer))))
      (should (= 120 (length preview)))
      (should (string-suffix-p "..." preview)))))


(mevedel-deftest mevedel-session-persistence-root-data-buffer-p ()
  ,test
  (test)
  :doc "recognizes only root session data buffers"
  (let ((session (mevedel-session--create :name "root-role"))
        (root (generate-new-buffer " *test-root-role*"))
        (view (generate-new-buffer " *test-view-role*"))
        (agent (generate-new-buffer " *test-agent-role*")))
    (unwind-protect
        (progn
          (with-current-buffer root
            (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--session session))
          (with-current-buffer view
            (setq-local mevedel--data-buffer root))
          (with-current-buffer agent
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation t))
          (mevedel-session-set-root-buffer session root)
          (should
           (mevedel-session-persistence-root-data-buffer-p root))
          (should-not
           (mevedel-session-persistence-root-data-buffer-p view))
          (should-not
           (mevedel-session-persistence-root-data-buffer-p agent)))
      (dolist (buffer (list root view agent))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))


(mevedel-deftest mevedel-session-persistence-authoritative-buffer ()
  ,test
  (test)
  :doc "returns ordinary data buffers unchanged"
  (let ((buf (generate-new-buffer " *test-data*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (eq buf (mevedel-session-persistence-authoritative-buffer
                           buf))))
      (when (buffer-live-p buf) (kill-buffer buf))))
  :doc "routes interactive view buffers to their data buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (should (eq data-buf
                              (mevedel-session-persistence-authoritative-buffer
                               view-buf)))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not treat transcript inspection views as session segment buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-agent-data*"))
               (view-buf (generate-new-buffer " *test-agent-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace))
                (mevedel-view--setup view-buf data-buf
                                     (list :agent-transcript-p t))
                (with-current-buffer view-buf
                  (should-not
                   (mevedel-session-persistence-authoritative-buffer
                    view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "rejects agent conversation buffers"
  (let ((buf (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local mevedel--agent-invocation t)
          (should-not
           (mevedel-session-persistence-authoritative-buffer buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))


(mevedel-deftest mevedel--instruction-workspace-state ()
  ,test
  (test)
  :doc "keeps instruction alists isolated by workspace"
  (let* ((root-a (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-a-" t)))
         (root-b (file-name-as-directory
                  (make-temp-file "mevedel-test-ws-b-" t)))
         (file-a (file-name-concat root-a "a.el"))
         (file-b (file-name-concat root-b "b.el"))
         (buf-a nil)
         (buf-b nil))
    (unwind-protect
        (progn
          (test-mevedel-session-persistence--reset-instructions)
          (mevedel-workspace-clear-registry)
          (write-region "(message \"a\")\n" nil file-a nil 'silent)
          (write-region "(message \"b\")\n" nil file-b nil 'silent)
          (let ((ws-a (mevedel-workspace-get-or-create
                       'project "a" root-a "a"))
                (ws-b (mevedel-workspace-get-or-create
                       'project "b" root-b "b")))
            (setq buf-a (find-file-noselect file-a))
            (setq buf-b (find-file-noselect file-b))
            (with-current-buffer buf-a
              (setq-local mevedel--workspace ws-a)
              (mevedel--create-reference-in buf-a (point-min) (point-max)))
            (with-current-buffer buf-b
              (setq-local mevedel--workspace ws-b)
              (mevedel--create-reference-in buf-b (point-min) (point-max)))
            (mevedel--instruction-activate-workspace ws-a)
            (should (= 1 (length (alist-get buf-a (mevedel--instruction-alist)))))
            (should-not (assoc buf-b (mevedel--instruction-alist)))
            (mevedel--instruction-activate-workspace ws-b)
            (should (= 1 (length (alist-get buf-b (mevedel--instruction-alist)))))
            (should-not (assoc buf-a (mevedel--instruction-alist)))))
      (when (buffer-live-p buf-a)
        (with-current-buffer buf-a (set-buffer-modified-p nil))
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (with-current-buffer buf-b (set-buffer-modified-p nil))
        (kill-buffer buf-b))
      (delete-directory root-a t)
      (delete-directory root-b t)
      (test-mevedel-session-persistence--reset-instructions)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel--instruction-operation-state-key ()
  ,test
  (test)
  :doc "prefers a dynamic workspace override over the buffer workspace"
  (let ((workspace (mevedel-workspace--create
                    :type 'project :id "buffer" :root "/tmp/buffer/")))
    (with-temp-buffer
      (setq-local mevedel--workspace workspace)
      (let ((mevedel--instruction-state-key-override
             '(project . "explicit")))
        (should (equal '(project . "explicit")
                       (mevedel--instruction-operation-state-key)))))))


(mevedel-deftest mevedel-session-persistence--allow-emacs-exit-p (:quiet t)
  ,test
  (test)
  :doc "pending publication vetoes exit until retry or explicit abandonment"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/pending-exit/"
                     :root "/tmp/pending-exit/" :name "pending-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-pending-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-pending-publication session)
                '(:reason "target unavailable"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (setf (mevedel-session-pending-publication session) nil)
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "unsettled remote mutation vetoes exit until acknowledgement"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/unsettled-exit/"
                     :root "/tmp/unsettled-exit/"
                     :name "unsettled-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-unsettled-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session))
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions)))
          (setf (mevedel-session-lease session)
                '(:state owned :unsettled-mutation nil))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "a foreign read-only inspector can exit with a durable mutation latch"
  (let* ((workspace (mevedel-workspace--create
                     :type 'project :id "/tmp/foreign-exit/"
                     :root "/tmp/foreign-exit/"
                     :name "foreign-exit"))
         (session (mevedel-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-foreign-exit*"))
         (kill-emacs-query-functions
          '(mevedel-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local mevedel--session session)
            (setq-local mevedel-session--read-only-mode t)
            (setq buffer-read-only t))
          (setf (mevedel-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'mevedel-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (should
             (plist-get (mevedel-session-lease session)
                        :unsettled-mutation))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil))
        (kill-buffer buffer)))))


(mevedel-deftest mevedel-session-persistence--kill-emacs-hook (:quiet t)
  ,test
  (test)
  :doc "force-tears down executions before exit persistence"
  (let ((mevedel-workspace--registry nil)
        torn-down)
    (cl-letf (((symbol-function 'mevedel-execution-teardown-all)
               (lambda () (setq torn-down t)))
              ((symbol-function 'buffer-list)
               (lambda (&optional _frame) nil)))
      (mevedel-session-persistence--kill-emacs-hook))
    (should torn-down))
  :doc "modified view buffers are persisted through data buffers on exit"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist '((org-mode . "*** ")))
                  (setq-local mevedel--session session)
                  (setq-local mevedel--workspace workspace)
                  (insert "Exit hook data prompt\n")
                  (set-buffer-modified-p nil))
                (mevedel-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char mevedel-view--input-marker)
                    (insert "Exit hook view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame)
                             (list view-buf data-buf)))
                          ((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (mevedel-session-persistence--kill-emacs-hook))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment-path
                       (mevedel-session-artifacts-segment-path
                        (mevedel-session-save-path session) 1)))
                  (should (file-exists-p segment-path))
                  (with-temp-buffer
                    (insert-file-contents segment-path)
                    (should (string-match-p "Exit hook data prompt"
                                            (buffer-string)))
                    (should-not (string-match-p "Exit hook view chrome"
                                                (buffer-string))))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-mevedel-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "cleans inactive expired sessions before releasing live locks"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (live-session (mevedel-session-create "live" workspace))
               (expired-session (mevedel-session-create "expired" workspace))
               (live-buf (generate-new-buffer " *test-live*"))
               (expired-buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (dolist (pair `((,live-session . ,live-buf)
                                (,expired-session . ,expired-buf)))
                  (with-current-buffer (cdr pair)
                    (org-mode)
                    (setq-local mevedel--session (car pair))
                    (setq-local mevedel--workspace workspace)
                    (insert "Old session\n")
                    (mevedel-session-artifacts-save
                     (car pair) (cdr pair))
                    (set-buffer-modified-p nil)))
                (dolist (session (list live-session expired-session))
                  (test-mevedel-session-persistence--expire-session session))
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path expired-session)
                 expired-session)
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame) (list live-buf))))
                  (mevedel-session-persistence--kill-emacs-hook))
                (should-not
                 (file-directory-p
                  (mevedel-session-save-path expired-session)))
                (should
                 (file-directory-p (mevedel-session-save-path live-session)))
                (should-not
                 (file-exists-p
                  (mevedel-session-persistence--lock-path
                   (mevedel-session-save-path live-session)))))
            (test-mevedel-session-persistence--release-and-kill
             live-buf live-session)
            (test-mevedel-session-persistence--release-and-kill
             expired-buf expired-session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "honors a cleanup run already throttled for the workspace"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "expired" workspace))
               (buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old session\n")
                  (mevedel-session-artifacts-save session buf))
                (let ((save-path (mevedel-session-save-path session)))
                  (test-mevedel-session-persistence--expire-session session)
                  (mevedel-session-persistence-lock-release save-path session)
                  (puthash
                   (cons (mevedel-workspace-type workspace)
                         (mevedel-workspace-id workspace))
                   t mevedel-session-persistence--cleanup-throttle)
                  (cl-letf (((symbol-function 'buffer-list)
                             (lambda (&optional _frame) nil)))
                    (mevedel-session-persistence--kill-emacs-hook))
                  (should (file-directory-p save-path))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "failed exit saves remain protected until cleanup finishes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "live" workspace))
               (buf (generate-new-buffer " *test-live*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local mevedel--session session)
                  (insert "Unsaved exit change\n")
                  (mevedel-session-artifacts-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path save-path)))
                  (test-mevedel-session-persistence--expire-session session)
                  (delete-file sidecar)
                  (make-directory sidecar)
                  (set-file-times
                   sidecar
                   (time-subtract (current-time) (* 14 24 60 60)))
                  (with-current-buffer buf
                    (set-buffer-modified-p t))
                  (cl-letf (((symbol-function 'buffer-list)
                             (lambda (&optional _frame) (list buf))))
                    (mevedel-session-persistence--kill-emacs-hook))
                  (should (file-directory-p save-path))
                  (should (file-directory-p sidecar))
                  (should-not
                   (file-exists-p
                    (mevedel-session-persistence--lock-path save-path)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "cleanup errors do not block sibling workspaces or lock release"
  (let* ((mevedel-session-max-age-days 7)
         (mevedel-session-persistence--cleanup-throttle
          (make-hash-table :test #'equal))
         (mevedel-workspace--registry (make-hash-table :test #'equal))
         (bad-root (file-name-as-directory
                    (make-temp-file "mevedel-test-bad-ws-" t)))
         (good-root (file-name-as-directory
                     (make-temp-file "mevedel-test-good-ws-" t)))
         (bad-workspace
          (mevedel-workspace-get-or-create
           'file "bad" bad-root "bad"))
         (good-workspace
          (mevedel-workspace-get-or-create
           'file "good" good-root "good"))
         (bad-sessions
          (mevedel-session-artifacts-sessions-dir bad-workspace))
         (live-session (mevedel-session-create "live" good-workspace))
         (expired-session (mevedel-session-create "expired" good-workspace))
         (live-buf (generate-new-buffer " *test-live*"))
         (expired-buf (generate-new-buffer " *test-expired*")))
    (unwind-protect
        (progn
          (dolist (pair `((,live-session . ,live-buf)
                          (,expired-session . ,expired-buf)))
            (with-current-buffer (cdr pair)
              (org-mode)
              (setq-local mevedel--session (car pair))
              (insert "Session\n")
              (mevedel-session-artifacts-save (car pair) (cdr pair))
              (set-buffer-modified-p nil)))
          (let ((save-path (mevedel-session-save-path expired-session)))
            (test-mevedel-session-persistence--expire-session expired-session)
            (mevedel-session-persistence-lock-release save-path expired-session))
          (make-directory bad-sessions t)
          (set-file-modes bad-sessions 0)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list live-buf))))
            (mevedel-session-persistence--kill-emacs-hook))
          (should-not
           (file-directory-p (mevedel-session-save-path expired-session)))
          (should-not
           (file-exists-p
            (mevedel-session-persistence--lock-path
             (mevedel-session-save-path live-session)))))
      (when (file-exists-p bad-sessions)
        (set-file-modes bad-sessions #o700))
      (test-mevedel-session-persistence--release-and-kill
       live-buf live-session)
      (test-mevedel-session-persistence--release-and-kill
       expired-buf expired-session)
      (delete-directory bad-root t)
      (delete-directory good-root t))))


;;
;;; Phase 5: read path

(mevedel-deftest mevedel-session-persistence-load-sidecar ()
  ,test
  (test)
  :doc "reads a current-version sidecar"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-codec-write
           tmp (test-mevedel-session-persistence--complete-sidecar
                '(:session-name "x")))
          (let ((plist (mevedel-session-persistence-load-sidecar tmp)))
            (should (equal mevedel-session-codec-format-version
                           (plist-get plist :version)))
            (should (equal "x" (plist-get plist :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "rejects an unsupported sidecar version"
  (let ((tmp (make-temp-file "mevedel-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-codec-write
           tmp '(:version "v0.0.0" :session-name "x"))
          (should-error
           (mevedel-session-persistence-load-sidecar tmp)
           :type 'error))
      (when (file-exists-p tmp) (delete-file tmp)))))


(mevedel-deftest mevedel-session-persistence--cold-workspace ()
  ,test
  (test)
  :doc "derives a target-side cold workspace from the persisted native root"
  (let ((workspace
         (mevedel-session-persistence--cold-workspace
          "/home/user/project/.mevedel/sessions/main-1/"
          '(:workspace (:type project
                        :target-native-root "/home/user/project/"
                        :name "project")))))
    (should (equal "/home/user/project/"
                   (mevedel-workspace-root workspace))))
  :doc "qualifies the persisted native root with a remote client's own prefix"
  (let ((workspace
         (mevedel-session-persistence--cold-workspace
          "/ssh:user@host:/home/user/project/.mevedel/sessions/main-1/"
          '(:workspace (:type project
                        :target-native-root "/home/user/project/"
                        :name "project")))))
    (should (equal "/ssh:user@host:/home/user/project/"
                   (mevedel-workspace-root workspace)))))


(mevedel-deftest mevedel-session-persistence-restore (:quiet t)
  ,test
  (test)
  :doc "restores stale rows as lost but supersedes rows with newer facts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "First user prompt\n")
                  (insert
                   (test-mevedel-session-persistence--execution-tool-block
                    "stale-call"
                    '(:execution-id "exec-stale" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (test-mevedel-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state running
                      :status success :live-execution-p t)))
                  (mevedel-session-artifacts-stabilize-gptel-bounds)
                  (mevedel-session-artifacts-save session buf)
                  (mevedel-session-artifacts-rotate-segment
                   session buf "Earlier conversation")
                  (insert "Second user prompt\n")
                  (insert
                   (test-mevedel-session-persistence--execution-tool-block
                    "current-call"
                    '(:execution-id "exec-current" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (test-mevedel-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state completed
                      :status success :live-execution-p nil)))
                  (insert
                   (mevedel--format-hook-audit-record
                    '(:type execution-completion
                      :tool-use-id "archived-call"
                      :render-data (:execution-id "exec-stale"
                                    :state completed
                                    :live-execution-p nil))))
                  (mevedel-session-artifacts-stabilize-gptel-bounds)
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                ;; Release the lock + kill the buffer (the test buffer didn't
                ;; go through chat-buffer-init-common so the kill-hook isn't
                ;; installed; we mirror its work manually).
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (should (file-exists-p session-dir))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (derived-mode-p 'org-mode))
                  (should (bound-and-true-p gptel-mode))
                  (should mevedel--session)
                  (should (equal "main"
                                 (mevedel-session-name mevedel--session)))
                  (should (= 2 (mevedel-session-current-segment
                                mevedel--session)))
                  (should-not (mevedel-session-execution-state
                               mevedel--session))
                  (should (string-match-p "Second user prompt"
                                          (buffer-string)))
                  (should (string-match-p ":state lost"
                                          (buffer-string))))
                (with-temp-buffer
                  (insert-file-contents
                   (mevedel-session-artifacts-segment-path
                    session-dir 1))
                  (goto-char (point-min))
                  (should (= 2 (how-many ":state archived"))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))
  :doc "read-only inspection trusts published state and skips lifecycle commands"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let (restored)
      (unwind-protect
          (let* ((session (mevedel-session-create "main" workspace))
                 (buffer (generate-new-buffer "*test-published-segment*"))
                 session-dir segment-1 segment-2 probed started)
            (with-current-buffer buffer
              (org-mode)
              (insert "Published transcript\n")
              (mevedel-session-artifacts-save session buffer))
            (setq session-dir (mevedel-session-save-path session)
                  segment-1 (mevedel-session-artifacts-segment-path
                             session-dir 1)
                  segment-2 (mevedel-session-artifacts-segment-path
                             session-dir 2))
            (test-mevedel-session-persistence--release-and-kill
             buffer session)
            (write-region "Unpublished transcript\n" nil segment-2 nil 'silent)
            (cl-letf (((symbol-function
                        'mevedel-session-persistence-lock-acquire)
                       (lambda (&rest _) nil))
                      ((symbol-function 'mevedel--probe-session-target)
                       (lambda (&rest _) (setq probed t)))
                      ((symbol-function 'mevedel--run-session-start-hooks)
                       (lambda (&rest _) (setq started t))))
              ;; Read-only restore announces itself; this case asserts the
              ;; durable state that notice echoes.
              (mevedel-test--with-captured-messages nil
                (setq restored
                      (mevedel-session-persistence-restore session-dir))))
            (with-current-buffer restored
              (should mevedel-session--read-only-mode)
              (should (= 1 (mevedel-session-current-segment
                            mevedel--session)))
              (should (string-match-p "Published transcript"
                                      (buffer-string)))
              (should-not (memq #'mevedel--run-session-end-hooks
                                kill-buffer-hook)))
            (should probed)
            (should-not started)
            (with-temp-buffer
              (insert-file-contents segment-1)
              (should-not
               (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                               (buffer-string))))
            (with-temp-buffer
              (insert-file-contents segment-2)
              (should (equal "Unpublished transcript\n" (buffer-string)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "an actual foreign lease permits inspection without hooks or repair writes"
  (let* ((host "restore-foreign-owner-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-foreign-restore-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b))
         (running
          (mevedel-pipeline--format-render-data-block
           '(:execution-id "foreign-stale" :state running
             :status success :live-execution-p t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment-1)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root (concat "* Chat\n" running))
            (let* ((segment-2
                    (mevedel-session-artifacts-segment-path
                     session-dir 2))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   probed
                   started)
              (write-region
               "Unpublished transcript\n" nil segment-2 nil 'silent)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let ((timer
                            (mevedel-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (mevedel-session-lease-renewal-timer owner) nil)))
              (cl-labels
                  ((snapshot ()
                     (mapcar
                      (lambda (path)
                        (cons
                         (file-relative-name path local-root)
                         (if (file-directory-p path)
                             :directory
                           (with-temp-buffer
                             (insert-file-contents-literally path)
                             (buffer-string)))))
                      (sort
                       (directory-files-recursively local-root ".*" t)
                       #'string<))))
                (let ((before (snapshot))
                      (mevedel-session-durability--client-id inspector-id))
                  (unwind-protect
                      (progn
                        (cl-letf
                            (((symbol-function 'mevedel--probe-session-target)
                              (lambda (&rest _) (setq probed t)))
                             ((symbol-function
                               'mevedel--run-session-start-hooks)
                              (lambda (&rest _) (setq started t)))
                             ((symbol-function
                               'mevedel-session-artifacts-self-heal-segment-counter)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore self-healed")))
                             ((symbol-function
                               'mevedel-session-persistence-reconcile-lost-execution-segments)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore repaired")))
                             ((symbol-function
                               'mevedel-session-persistence--maybe-prune-orphan)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore pruned")))
                             ((symbol-function
                               'mevedel-session-publication-publish)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore published"))))
                          ;; Read-only restore announces itself; this case asserts the
                          ;; durable state that notice echoes.
                          (mevedel-test--with-captured-messages nil
                            (setq restored
                                  (mevedel-session-persistence-restore
                                   session-dir nil nil workspace))))
                        (with-current-buffer restored
                          (should mevedel-session--read-only-mode)
                          (should
                           (eq 'foreign
                               (plist-get
                                (mevedel-session-lease mevedel--session)
                                :state)))
                          (should (= 1 (mevedel-session-current-segment
                                        mevedel--session)))
                          (should
                           (string-match-p ":state running"
                                           (buffer-string)))
                          (should-not
                           (memq #'mevedel--run-session-end-hooks
                                 kill-buffer-hook)))
                        (should probed)
                        (should-not started)
                        (should (equal before (snapshot)))
                        (with-temp-buffer
                          (insert-file-contents segment-1)
                          (should
                           (string-match-p ":state running"
                                           (buffer-string)))
                          (should-not
                           (string-match-p ":state lost"
                                           (buffer-string))))
                        (with-temp-buffer
                          (insert-file-contents segment-2)
                          (should
                           (equal "Unpublished transcript\n"
                                  (buffer-string)))))
                    (test-mevedel-session-persistence--release-and-kill
                     restored
                     (and restored
                          (buffer-local-value 'mevedel--session restored)))
                    (setq restored nil))))
              (let ((mevedel-session-durability--client-id owner-id))
                (mevedel-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "foreign inspection ignores a missing fixed segment cache"
  (let* ((host "restore-foreign-missing-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-foreign-missing-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b)))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (delete-file segment)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let ((timer
                            (mevedel-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (mevedel-session-lease-renewal-timer owner) nil)))
              (let ((before (directory-files-recursively local-root ".*" t))
                    (mevedel-session-durability--client-id inspector-id)
                    restored)
                (unwind-protect
                    (progn
                      (cl-letf
                          (((symbol-function 'yes-or-no-p)
                            (lambda (&rest _)
                              (ert-fail "Foreign restore prompted to prune")))
                           ((symbol-function
                             'mevedel-session-persistence--maybe-prune-orphan)
                            (lambda (&rest _)
                              (ert-fail "Foreign restore pruned"))))
                        ;; Read-only restore announces itself; this case asserts the
                        ;; durable state that notice echoes.
                        (mevedel-test--with-captured-messages nil
                          (setq restored
                                (mevedel-session-persistence-restore
                                 session-dir nil nil workspace))))
                      (with-current-buffer restored
                        (should mevedel-session--read-only-mode)
                        (should (string-match-p
                                 "Published transcript" (buffer-string))))
                      (should (file-directory-p session-dir))
                      (should
                       (equal before
                              (directory-files-recursively
                               local-root ".*" t))))
                  (test-mevedel-session-persistence--release-and-kill
                   restored
                   (and restored
                        (buffer-local-value 'mevedel--session restored)))))
              (let ((mevedel-session-durability--client-id owner-id))
                (mevedel-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "retries cleanly when the publication head changes during lease acquisition"
  (let* ((host "restore-sidecar-race-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-race-" t)))
         restored
         acquired-session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'mevedel-session-persistence-lock-acquire))
                   changed)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (cl-letf
                  (((symbol-function
                     'mevedel-session-persistence-lock-acquire)
                    (lambda (&rest arguments)
                      ;; Another owner commits a new immutable head after our
                      ;; initial capture but before this lease claim.
                      (unless changed
                        (setq changed t)
                        (let ((mevedel-session-durability--client-id
                               (make-string 64 ?d)))
                          (unwind-protect
                              (progn
                                (should
                                 (mevedel-session-durability-lease-acquire
                                  session-dir "*intervening-owner*"
                                  fixture-session))
                                (let* ((publication
                                        (mevedel-session-publication-read
                                         session-dir))
                                       (new-sidecar
                                        (mevedel-session-codec-read
                                         (plist-get publication :sidecar))))
                                  (plist-put
                                   new-sidecar :total-turn-count 17)
                                  (mevedel-session-publication-publish
                                   fixture-session
                                   (list
                                    (list
                                     :path
                                     (mevedel-session-artifacts-sidecar-path
                                      session-dir)
                                     :content
                                     (mevedel-session-artifacts-printed-value
                                      new-sidecar)
                                     :commit-marker t)))))
                            (mevedel-session-durability-lease-release
                             session-dir fixture-session))))
                      (setq acquired-session (nth 2 arguments))
                      (apply acquire-function arguments)))
                   ((symbol-function 'mevedel--probe-session-target)
                    #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-artifacts-load-instructions)
                    #'ignore))
                (let ((err
                       (should-error
                        (setq restored
                              (mevedel-session-persistence-restore
                               session-dir nil nil workspace))
                        :type 'user-error)))
                  (should
                   (string-match-p
                    "changed while acquiring"
                    (error-message-string err)))))
              (should acquired-session)
              (should-not (mevedel-session-lease acquired-session))
              (should-not
               (mevedel-session-lease-renewal-timer acquired-session))
              (should
               (eq 'released
                   (plist-get
                    (mevedel-session-durability--lease-head
                     (mevedel-session-durability--lease-path session-dir))
                    :status)))
              (should
               (= 17
                  (plist-get
                   (mevedel-session-codec-read
                    (plist-get
                     (mevedel-session-publication-read session-dir)
                     :sidecar))
                   :total-turn-count))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "loads immutable sidecar and transcript bytes over poisoned fixed caches"
  (let* ((host "restore-sidecar-cache-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-sidecar-cache-" t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (write-region "(:poisoned fixed sidecar)" nil
                            (mevedel-session-artifacts-sidecar-path
                             session-dir)
                            nil 'silent)
              (write-region "Poisoned fixed transcript\n" nil segment nil 'silent)
              (cl-letf
                  (((symbol-function 'mevedel--probe-session-target) #'ignore)
                   ((symbol-function 'mevedel--chat-buffer-init-common) #'ignore)
                   ((symbol-function 'mevedel-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'mevedel-session-artifacts-load-instructions)
                    #'ignore))
                (setq restored
                      (mevedel-session-persistence-restore
                       session-dir nil nil workspace)))
              (with-current-buffer restored
                (should (equal "main" (mevedel-session-name mevedel--session)))
                (should (string-match-p
                         "Published transcript" (buffer-string)))
                (should-not
                 (string-match-p "Poisoned fixed" (buffer-string)))
                (should (mevedel-session-publication mevedel--session))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "fails closed when an immutable transcript digest no longer matches"
  (let* ((host "restore-digest-corruption")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-digest-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (publication
                    (mevedel-session-publication-read session-dir))
                   (entry
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get publication :artifacts)))))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target fixture-session))
               t mevedel-session-durability--disclosed-targets)
              (write-region "corrupt" nil (plist-get entry :published)
                            nil 'silent)
              (let ((err
                     (should-error
                      (mevedel-session-persistence-restore
                       session-dir nil nil workspace))))
                (should
                 (string-match-p "failed verification"
                                 (error-message-string err))))
              (should-not
               (mevedel-session-persistence-find-live-buffer
                "main-remote-restore" "*unused*"))
              (should
               (eq 'released
                   (plist-get
                    (mevedel-session-durability--lease-head
                     (mevedel-session-durability--lease-path session-dir))
                    :status))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "releases its lease when restore fails before opening a buffer"
  (let* ((host "restore-pre-buffer-failure-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-pre-buffer-failure-" t)))
         acquired-session
         session-dir)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session fixture-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (setq session-dir fixture-dir)
            (let* ((mevedel-session-durability--client-id
                    (make-string 64 ?a))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'mevedel-session-persistence-lock-acquire))
                   artifact-open-attempted-p)
              (unwind-protect
                  (progn
                    (puthash
                     (mevedel-execution-target-identity
                      (mevedel-session-execution-target fixture-session))
                     t mevedel-session-durability--disclosed-targets)
                    (cl-letf
                        (((symbol-function
                           'mevedel-session-persistence-lock-acquire)
                          (lambda (&rest arguments)
                            (setq acquired-session (nth 2 arguments))
                            (apply acquire-function arguments)))
                         ((symbol-function
                           'mevedel-session-artifacts-find-artifact-noselect)
                          (lambda (&rest _)
                            (setq artifact-open-attempted-p t)
                            (error "Injected pre-buffer restore failure"))))
                      (should-error
                       (mevedel-session-persistence-restore
                        session-dir nil nil workspace)
                       :type 'error))
                    (should acquired-session)
                    (should artifact-open-attempted-p)
                    (should-not
                     (mevedel-session-persistence-find-live-buffer
                      "main-remote-restore" "*unused*"))
                    (should-not (mevedel-session-lease acquired-session))
                    (should-not
                     (mevedel-session-lease-renewal-timer acquired-session))
                    (should
                     (eq 'released
                         (plist-get
                          (mevedel-session-durability--lease-head
                           (mevedel-session-durability--lease-path session-dir))
                          :status))))
                (when acquired-session
                  (mevedel-session-durability-lease-release
                   session-dir acquired-session))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "remote repair publishes transcript and sidecar in one ordered batch"
  (let* ((host "restore-publication-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-remote-restore-" t)))
         (running
          (with-temp-buffer
            (org-mode)
            (insert "* Chat\n")
            (insert
             (test-mevedel-session-persistence--execution-tool-block
              "remote-call"
              '(:execution-id "remote-stale" :state running
                :status success :live-execution-p t)))
            (mevedel-session-artifacts-stabilize-gptel-bounds)
            (buffer-substring-no-properties (point-min) (point-max))))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
            (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root running)
            (let ((mevedel-session-durability--client-id
                   (make-string 64 ?a))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal))
                  publications)
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (unwind-protect
                  (progn
                    (cl-letf
                        (((symbol-function 'mevedel--probe-session-target)
                          #'ignore)
                         ((symbol-function 'mevedel--chat-buffer-init-common)
                          #'ignore)
                         ((symbol-function
                           'mevedel-agent-persistence-restore-tree)
                          (lambda (&rest _) 1))
                         ((symbol-function
                           'mevedel-session-artifacts-load-instructions)
                          #'ignore)
                         ((symbol-function
                           'mevedel-session-publication-publish)
                          (lambda (_session artifacts)
                            (push artifacts publications)
                            t)))
                      (setq restored
                            (mevedel-session-persistence-restore
                             session-dir nil nil workspace)))
                    (should (= 1 (length publications)))
                    (let* ((artifacts (car publications))
                           (transcript (car artifacts))
                           (commit (car (last artifacts))))
                      (should (equal segment (plist-get transcript :path)))
                      (should (string-match-p
                               ":state lost" (plist-get transcript :content)))
                      (should-not
                       (string-match-p "MEVEDEL_SEGMENT_FINALIZED_AT"
                                       (plist-get transcript :content)))
                      (should
                       (equal
                        (mevedel-session-artifacts-sidecar-path session-dir)
                        (plist-get commit :path)))
                      (should
                       (string-match-p
                        ":current-segment 1"
                        (plist-get commit :content)))
                      (should (eq t (plist-get commit :commit-marker))))
                    ;; The publisher was replaced with a spy, so a direct
                    ;; repair or finalization write would change target state.
                    (with-temp-buffer
                      (insert-file-contents segment)
                      (should
                       (string-match-p ":state running" (buffer-string)))
                      (should-not
                       (string-match-p ":state lost" (buffer-string)))
                      (should-not
                       (string-match-p
                        "MEVEDEL_SEGMENT_FINALIZED_AT"
                        (buffer-string)))))
                (when restored
                  (test-mevedel-session-persistence--release-and-kill
                   restored
                   (buffer-local-value 'mevedel--session restored)))
                (setq restored nil)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "retargets and persists a missing working directory"
  (cl-destructuring-bind
      (_workspace tempdir _missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (prompt dir default mustmatch &rest _)
                         (should (string-match-p
                                  "deleted-worktree.*missing" prompt))
                         (should (equal tempdir
                                        (file-name-as-directory dir)))
                         (should (equal tempdir
                                        (file-name-as-directory default)))
                         (should mustmatch)
                         replacement-dir)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-artifacts-sidecar-path session-dir))))
              (should (equal replacement-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "keeps a removed Worktree Fork discoverable and preserves its origin"
  (cl-destructuring-bind
      (workspace tempdir missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (let* ((sidecar-path
                    (mevedel-session-artifacts-sidecar-path session-dir))
                   (sidecar
                    (mevedel-session-codec-read sidecar-path)))
              (plist-put sidecar :fork-type 'worktree)
              (plist-put sidecar :forked-from-session-id "source-id")
              (plist-put sidecar :worktree-source-root tempdir)
              (plist-put sidecar :worktree-directory missing-dir)
              (plist-put sidecar :worktree-branch "worktree/main-fork-1")
              (plist-put sidecar :worktree-base-commit "abc123")
              (mevedel-session-codec-write sidecar-path sidecar))
            (let* ((entry
                    (car
                     (mevedel-session-persistence-list-sessions workspace)))
                   (display
                    (mevedel-session-persistence--format-session-candidate
                     entry)))
              (should entry)
              (should (string-match-p "Worktree Fork" display))
              (should (string-match-p "missing" display)))
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir)))
              (setq restored
                    (mevedel-session-persistence-restore session-dir)))
            (should-not (file-exists-p missing-dir))
            (with-current-buffer restored
              (should (eq 'worktree
                          (mevedel-session-fork-type mevedel--session)))
              (should (equal missing-dir
                             (mevedel-session-worktree-directory
                              mevedel--session)))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let* ((summary
                    (mevedel-session-persistence--read-summary
                     (mevedel-session-artifacts-sidecar-path
                      session-dir)))
                   (display
                    (mevedel-session-persistence--format-session-candidate
                     (list :summary summary))))
              (should (equal missing-dir
                             (plist-get summary :worktree-directory)))
              (should (equal replacement-dir
                             (plist-get summary :working-directory)))
              (should (string-match-p "retargeted" display))
              (should (string-match-p
                       (regexp-quote replacement-dir) display))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "does not persist a retargeted directory in read-only mode"
  (cl-destructuring-bind
      (_workspace tempdir missing-dir replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir))
                      ((symbol-function
                        'mevedel-session-persistence-lock-acquire)
                       (lambda (&rest _) nil)))
              ;; Read-only restore announces itself; this case asserts the
              ;; durable state that notice echoes.
              (mevedel-test--with-captured-messages nil
                (setq restored
                      (mevedel-session-persistence-restore session-dir))))
            (with-current-buffer restored
              (should mevedel-session--read-only-mode)
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (mevedel-session-working-directory
                              mevedel--session))))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-artifacts-sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (mevedel-workspace-clear-registry))))
  :doc "rejects an invalid replacement before opening the session"
  (cl-destructuring-bind
      (workspace tempdir missing-dir _replacement-dir session-dir)
      (test-mevedel-session-persistence--make-missing-cwd-session)
    (let ((outside (make-temp-file "mevedel-cwd-outside-" t))
          (buf-name (mevedel-session-buffer-name "main" workspace)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) outside)))
              (should-error
               (mevedel-session-persistence-restore session-dir)
               :type 'user-error))
            (should-not
             (file-exists-p
              (mevedel-session-persistence--lock-path session-dir)))
            (should-not (get-buffer buf-name))
            (let ((sidecar
                   (mevedel-session-persistence-load-sidecar
                    (mevedel-session-artifacts-sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (when (file-directory-p outside)
          (delete-directory outside t))
        (mevedel-workspace-clear-registry))))
  :doc "round-trips a multi-segment (compacted) session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Original prompt\n")
                  (mevedel-session-artifacts-save session buf)
                  (mevedel-session-artifacts-rotate-segment
                   session buf "Summary of segment 1.")
                  (insert "After-compact prompt\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (= 2 (mevedel-session-current-segment
                                mevedel--session)))
                  (should (string-match-p "Summary of segment 1\\."
                                          (buffer-string)))
                  (should (string-match-p "After-compact prompt"
                                          (buffer-string)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "preserves operation, network, and resource authority across resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (setf (mevedel-session-permission-rules session)
                      '(("Read" :path "/tmp/foo/**" :action allow)
                        ("Bash" :pattern "npx test*"
                         :network t
                         :file-system
                         ((:path "/tmp/external-input" :access read))
                         :action allow)))
                (setf (mevedel-session-resource-grants session)
                      '((:path "/tmp/external-input" :access read)))
                (with-current-buffer buf
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should
                   (equal
                    '(("Read" :path "/tmp/foo/**" :action allow)
                      ("Bash" :pattern "npx test*"
                       :network t
                       :file-system
                       ((:path "/tmp/external-input" :access read))
                       :action allow))
                    (mevedel-session-permission-rules
                     mevedel--session)))
                  (should
                   (equal
                    '((:path "/tmp/external-input" :access read))
                    (mevedel-session-resource-grants
                     mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "does not double-rewrite nested relocated working directories"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "mevedel-old-root-" t)))
         (new-root (file-name-as-directory
                    (file-name-concat old-root "packages" "api")))
         (old-cwd (file-name-as-directory
                   (file-name-concat old-root "src")))
         (expected-cwd (file-name-as-directory
                        (file-name-concat new-root "src")))
         buf session session-dir restored opened-workspace)
    (unwind-protect
        (progn
          (make-directory old-cwd t)
          (make-directory expected-cwd t)
          (mevedel-workspace-clear-registry)
          (let ((workspace (mevedel-workspace-get-or-create
                            'project old-root old-root "nested-proj")))
            (setq session (mevedel-session-create "main" workspace))
            (setf (mevedel-session-working-directory session) old-cwd))
          (setq buf (generate-new-buffer "*test-data-buf*"))
          (with-current-buffer buf
            (org-mode)
            (setq-local mevedel--session session)
            (insert "Nested relocation\n")
            (mevedel-session-artifacts-save session buf))
          (setq session-dir (mevedel-session-save-path session))
          (test-mevedel-session-persistence--release-and-kill
           buf session)
          (setq buf nil)
          (make-directory (file-name-concat new-root ".mevedel") t)
          (copy-file
           (file-name-concat old-root ".mevedel" "workspace-id")
           (file-name-concat new-root ".mevedel" "workspace-id"))
          (mevedel-workspace-clear-registry)
          (setq opened-workspace
                (mevedel-workspace-get-or-create
                 'project new-root new-root "nested-proj"))
          (setq restored (mevedel-session-persistence-restore
                          session-dir nil nil opened-workspace))
          (with-current-buffer restored
            (should (equal expected-cwd
                           (mevedel-session-working-directory
                            mevedel--session)))))
      (test-mevedel-session-persistence--release-and-kill
       buf session)
      (test-mevedel-session-persistence--release-and-kill
       restored
       (and restored (buffer-local-value 'mevedel--session restored)))
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (mevedel-workspace-clear-registry)))
  :doc "switches to a live buffer instead of re-loading"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf-name (mevedel-session-buffer-name "main" workspace))
               (buf      (get-buffer-create buf-name))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Live buffer\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (setq restored (mevedel-session-persistence-restore
                                session-dir))
                ;; Restore should return the existing live buffer.
                (should (eq buf restored)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "cold-restores the durable tree, mailboxes, recovery, and follow-up"
  (test-mevedel-session-persistence--cold-agent-tree-round-trip))


(mevedel-deftest mevedel-session-persistence-restore/unsettled-mutation (:quiet t)
  ,test
  (test)
  :doc "restore inherits an unsettled mutation and acknowledgement clears it"
  (let* ((host "restore-unsettled-mutation")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-restore-unsettled-" t)))
         (owner-id (make-string 64 ?a))
         (successor-id (make-string 64 ?b))
         (observer-id (make-string 64 ?c))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target owner))
               t mevedel-session-durability--disclosed-targets)
              (let ((mevedel-session-durability--client-id owner-id))
                (should
                 (mevedel-session-durability-lease-acquire
                  session-dir "*previous-owner*" owner))
                (should
                 (mevedel-session-durability-set-unsettled-mutation
                  owner t))
                (mevedel-session-durability-lease-release
                 session-dir owner))
              (let ((mevedel-session-durability--client-id successor-id))
                (cl-letf
                    (((symbol-function 'mevedel--probe-session-target)
                      #'ignore)
                     ((symbol-function 'mevedel--run-session-start-hooks)
                      #'ignore))
                  (setq restored
                        (mevedel-session-persistence-restore
                         session-dir nil nil workspace)))
                (with-current-buffer restored
                  (require 'mevedel-execution)
                  (should
                   (mevedel-execution-mutation-blocked-p mevedel--session))
                  (mevedel-execution-acknowledge-unknown mevedel--session)
                  (should-not
                   (mevedel-execution-mutation-blocked-p mevedel--session))
                  (should-not
                   (mevedel-session-durability-unsettled-mutation-p
                    mevedel--session))
                  (mevedel-session-durability-lease-release
                   session-dir mevedel--session)
                  (let ((mevedel-session-durability--client-id observer-id))
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*observer*" mevedel--session))
                    (should-not
                     (mevedel-session-durability-unsettled-mutation-p
                      mevedel--session))
                    (mevedel-session-durability-lease-release
                     session-dir mevedel--session)))))))
      (mevedel-test--with-local-shell-tramp (list host)
        (test-mevedel-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'mevedel--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


;;
;;; Phase 6: locking

(mevedel-deftest mevedel-session-persistence--same-host-lock-active-p ()
  ,test
  (test)
  :doc "returns nil for dead PIDs"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) nil)))
    (should-not
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when process start predates lock time"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-subtract lock-time 10))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active within timestamp tolerance"
  (let* ((lock-time (current-time))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (time-add lock-time 1))))
      (should
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "treats live PIDs as stale when process start proves PID reuse"
  (let* ((lock-time (time-subtract (current-time) (* 30 24 60 60)))
         (lock-str  (format-time-string "%FT%H-%M-%S" lock-time)))
    (cl-letf (((symbol-function
                'mevedel-session-persistence--pid-alive-p)
               (lambda (&rest _) t))
              ((symbol-function
                'mevedel-session-persistence--pid-start-time)
               (lambda (&rest _) (current-time))))
      (should-not
       (mevedel-session-persistence--same-host-lock-active-p
        (list :pid 12345 :emacs-invocation-time lock-str)))))
  :doc "keeps live PIDs active when process start is unavailable"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) nil)))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345
            :emacs-invocation-time "2026-04-23T14-30-15"))))
  :doc "keeps live PIDs active when lock time is malformed"
  (cl-letf (((symbol-function
              'mevedel-session-persistence--pid-alive-p)
             (lambda (&rest _) t))
            ((symbol-function
              'mevedel-session-persistence--pid-start-time)
             (lambda (&rest _) (current-time))))
    (should
     (mevedel-session-persistence--same-host-lock-active-p
      (list :pid 12345 :emacs-invocation-time "old")))))


(mevedel-deftest mevedel-session-persistence--active-lock-p ()
  ,test
  (test)
  :doc "treats cross-host locks as active without local PID checks"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "old"
                         :buffer "*remote*")
                   (current-buffer)))
          (should (mevedel-session-persistence--active-lock-p tempdir)))
      (delete-directory tempdir t)))
  :doc "treats same-host reused-PID locks as inactive"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should-not
             (mevedel-session-persistence--active-lock-p tempdir))))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-persistence-lock-acquire ()
  ,test
  (test)
  :doc "writes a fresh lock when none exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          (should (mevedel-session-persistence-lock-acquire
                   tempdir "*test-buf*"
                   (test-mevedel-session-persistence--pid-lock-context)))
          (let ((lock-path
                 (mevedel-session-persistence--lock-path tempdir)))
            (should (file-exists-p lock-path))
            (let ((plist (mevedel-session-persistence--read-lock lock-path)))
              (should (= (emacs-pid) (plist-get plist :pid)))
              (should (equal "*test-buf*" (plist-get plist :buffer))))))
      (delete-directory tempdir t)))
  :doc "unreadable raced lock signals instead of recursing"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-session-persistence--read-lock)
                   (lambda (&rest _) nil))
                  ((symbol-function
                    'mevedel-session-persistence--write-lock-atomic)
                   (lambda (&rest _) nil)))
          (should-error
           (mevedel-session-persistence-lock-acquire
            tempdir "*test-buf*"
            (test-mevedel-session-persistence--pid-lock-context))
           :type 'user-error))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [b]reak overwrites the lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a live PID on this host.
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?b)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*test-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*test-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [r]ead-only returns nil and preserves lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"
                           (test-mevedel-session-persistence--pid-lock-context)))))
          ;; Original lock untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "*other-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "same-host live PID: [a]bort signals user-error"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*other-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?a)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*test-buf*"
              (test-mevedel-session-persistence--pid-lock-context))
             :type 'user-error)))
      (delete-directory tempdir t)))
  :doc "same-host reused PID follows the stale-lock confirmation path"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir))
               (lock-time (time-subtract (current-time) (* 30 24 60 60))))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _)
                       (error "Unexpected live-lock prompt")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "breaks a stale lock when user confirms"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          ;; Plant a lock with a hostname-mismatching PID-alive predicate
          ;; stubbed nil so the stale-lock branch fires deterministically.
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should (mevedel-session-persistence-lock-acquire
                     tempdir "*new-buf*"
                     (test-mevedel-session-persistence--pid-lock-context))))
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= (emacs-pid) (plist-get plist :pid)))
            (should (equal "*new-buf*" (plist-get plist :buffer)))))
      (delete-directory tempdir t)))
  :doc "leaves a stale lock alone when user declines"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*old-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                    ((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (should-error
             (mevedel-session-persistence-lock-acquire
              tempdir "*new-buf*"
              (test-mevedel-session-persistence--pid-lock-context))
             :type 'user-error))
          ;; Original lock remains untouched.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (= 999999 (plist-get plist :pid)))))
      (delete-directory tempdir t)))
  :doc "cross-host: read-only response returns nil"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let* ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote-buf*")
                   (current-buffer)))
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) ?r)))
            (should (null (mevedel-session-persistence-lock-acquire
                           tempdir "*test-buf*"
                           (test-mevedel-session-persistence--pid-lock-context)))))
          ;; The remote lock is still in place.
          (let ((plist (mevedel-session-persistence--read-lock lock-path)))
            (should (equal "other-host" (plist-get plist :hostname)))))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-persistence-lock-release ()
  ,test
  (test)
  :doc "deletes our own lock"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (mevedel-session-persistence-lock-acquire
           tempdir "*x*"
           (test-mevedel-session-persistence--pid-lock-context))
          (should (file-exists-p lock-path))
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
          (should-not (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "leaves alien locks alone"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (let ((lock-path (mevedel-session-persistence--lock-path tempdir)))
          (with-temp-file lock-path
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :buffer "*x*")
                   (current-buffer)))
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
          ;; Lock still present.
          (should (file-exists-p lock-path)))
      (delete-directory tempdir t)))
  :doc "is a no-op when no lock exists"
  (let ((tempdir (file-name-as-directory
                  (make-temp-file "mevedel-lock-test-" t))))
    (unwind-protect
        (progn
          ;; Should not error.
          (mevedel-session-persistence-lock-release
           tempdir (test-mevedel-session-persistence--pid-lock-context))
          (should-not (file-exists-p
                       (mevedel-session-persistence--lock-path tempdir))))
      (delete-directory tempdir t))))


(mevedel-deftest mevedel-session-persistence--sweep-stale-locks ()
  ,test
  (test)
  :doc "removes same-host dead-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-artifacts-sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "stale-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock")))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 999999
                         :hostname (system-name)
                         :emacs-invocation-time "old"
                         :buffer "*gone*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) nil)))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "removes same-host reused-PID lock files silently"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-artifacts-sessions-dir
                              workspace))
               (stale-dir    (file-name-as-directory
                              (file-name-concat sessions-dir "reused-sess")))
               (stale-lock   (file-name-concat stale-dir ".lock"))
               (lock-time    (time-subtract (current-time) (* 30 24 60 60))))
          (make-directory stale-dir t)
          (with-temp-file stale-lock
            (prin1 (list :pid 12345
                         :hostname (system-name)
                         :emacs-invocation-time
                         (format-time-string "%FT%H-%M-%S" lock-time)
                         :buffer "*reused*")
                   (current-buffer)))
          (cl-letf (((symbol-function
                      'mevedel-session-persistence--pid-alive-p)
                     (lambda (&rest _) t))
                    ((symbol-function
                      'mevedel-session-persistence--pid-start-time)
                     (lambda (&rest _) (current-time))))
            (mevedel-session-persistence--sweep-stale-locks workspace))
          (should-not (file-exists-p stale-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves same-host live-PID locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-artifacts-sessions-dir
                              workspace))
               (live-dir     (file-name-as-directory
                              (file-name-concat sessions-dir "live-sess")))
               (live-lock    (file-name-concat live-dir ".lock")))
          (make-directory live-dir t)
          (with-temp-file live-lock
            (prin1 (list :pid (emacs-pid)
                         :hostname (system-name)
                         :emacs-invocation-time "new"
                         :buffer "*live*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p live-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "leaves cross-host locks alone"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir (mevedel-session-artifacts-sessions-dir
                              workspace))
               (remote-dir   (file-name-as-directory
                              (file-name-concat sessions-dir "remote-sess")))
               (remote-lock  (file-name-concat remote-dir ".lock")))
          (make-directory remote-dir t)
          (with-temp-file remote-lock
            (prin1 (list :pid 12345
                         :hostname "other-host"
                         :emacs-invocation-time "..."
                         :buffer "*remote*")
                   (current-buffer)))
          (mevedel-session-persistence--sweep-stale-locks workspace)
          (should (file-exists-p remote-lock)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-control-transfer-poll ()
  ,test
  (test)
  :doc "keeps a drained transfer quiescing when no root buffer can be saved"
  (let* ((session (mevedel-session--create :name "transfer-poll"))
         (transfer '(:state quiescing))
         saved released)
    (setf (mevedel-session-control-transfer session) transfer)
    (cl-letf (((symbol-function
                'mevedel-session-transfer-poll)
               (lambda (_) transfer))
              ((symbol-function
                'mevedel-session-persistence-root-buffer-for-session)
               (lambda (&rest _) nil))
              ((symbol-function 'mevedel-session-artifacts-save)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function
                'mevedel-session-transfer-release)
               (lambda (_) (setq released t))))
      (should
       (eq transfer
           (mevedel-session-control-transfer-poll session nil nil)))
      (should-not saved)
      (should-not released)))
  :doc "refreshes committed transcript and sidecar state before enabling writes"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-transfer-refresh-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "requester" workspace))
         (refreshed (mevedel-session-create "requester" workspace))
         (buffer (generate-new-buffer " *mevedel-transfer-refresh*"))
         (save-path (file-name-as-directory
                     (file-name-concat root "session")))
         (segment-path (file-name-concat save-path "segment-0002.chat.org"))
         instructions-loaded incarnation-checked released)
    (make-directory save-path t)
    (write-region "initial transcript" nil segment-path nil 'silent)
    (setf (mevedel-session-save-path session) save-path
          (mevedel-session-control-transfer session)
          '(:state requested :request (:request-id "request"))
          (mevedel-session-turn-count refreshed) 7
          (mevedel-session-current-segment refreshed) 2)
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name segment-path)
          (insert "stale transcript")
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (setq buffer-read-only t
                mevedel-session--read-only-mode t)
          (write-region "owner committed transcript" nil segment-path
                        nil 'silent)
          (should-not (verify-visited-file-modtime buffer))
          (cl-letf
              (((symbol-function 'mevedel-session-durability-lease-acquire)
                (lambda (_path _name actual)
                  (setf (mevedel-session-lease actual) '(:state owned))
                  t))
               ((symbol-function 'mevedel-session-transfer-observe-decision)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-publication-read)
                (lambda (_) '(:sidecar "/committed/session.meta.el")))
               ((symbol-function
                 'mevedel-session-persistence-load-sidecar)
                (lambda (_) '(:committed t)))
               ((symbol-function 'mevedel-session-codec-deserialize)
                (lambda (&rest _) (list :session refreshed)))
               ((symbol-function 'mevedel-session-artifacts-read-artifact)
                (lambda (&rest _) "fresh transcript"))
               ((symbol-function
                 'mevedel-session-artifacts-check-target-incarnation)
                (lambda (_session checked-buffer)
                  (should-not (eq checked-buffer buffer))
                  (should (buffer-live-p checked-buffer))
                  (should (with-current-buffer buffer buffer-read-only))
                  (setq incarnation-checked t)))
               ((symbol-function 'mevedel-transcript-restore-gptel-state)
                #'ignore)
               ((symbol-function
                 'mevedel-session-artifacts-load-instructions)
                (lambda (&rest _) (setq instructions-loaded t)))
               ((symbol-function
                 'mevedel-session-durability-lease-release)
                (lambda (&rest _) (setq released t)))
               ((symbol-function 'ask-user-about-supersession-threat)
                (lambda (&rest _) (error "Supersession prompt"))))
            ;; Acquiring control announces itself; the case asserts the
            ;; durable state that notice echoes.
            (mevedel-test--with-captured-messages nil
              (should
               (mevedel-session-control-transfer-poll session buffer t)))
            (should (= 7 (mevedel-session-turn-count session)))
            (should (equal "fresh transcript" (buffer-string)))
            (should-not buffer-read-only)
            (should-not mevedel-session--read-only-mode)
            (should instructions-loaded)
            (should incarnation-checked)
            (should-not released)
            (should
             (eq 'acquired
                 (plist-get (mevedel-session-control-transfer session)
                            :state)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry)))
  :doc "releases a newly acquired lease when committed refresh fails"
  (let* ((session (mevedel-session--create
                   :name "failed-refresh" :save-path "/session/"))
         (buffer (generate-new-buffer " *mevedel-transfer-failure*"))
         released)
    (setf (mevedel-session-control-transfer session)
          '(:state requested :request (:request-id "request")))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-read-only t
                mevedel-session--read-only-mode t)
          (cl-letf
              (((symbol-function 'mevedel-session-durability-lease-acquire)
                (lambda (&rest _) t))
               ((symbol-function 'mevedel-session-transfer-observe-decision)
                (lambda (&rest _) nil))
               ((symbol-function
                 'mevedel-session-publication-read)
                (lambda (_) (error "Injected refresh failure")))
               ((symbol-function
                 'mevedel-session-durability-lease-release)
                (lambda (&rest _) (setq released t))))
            (should-error
             (mevedel-session-control-transfer-poll session buffer t))
            (should released)
            (should buffer-read-only)
            (should mevedel-session--read-only-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


;;
;;; Phase 9: fork-on-send + rename-session

(mevedel-deftest mevedel-save-session (:quiet t)
  ,test
  (test)
  :doc "refuses save-as without changing a tree that has an active turn"
  (with-temp-buffer
    (let* ((session (mevedel-session--create :name "save-as"))
           (record
            (mevedel-agent-record--create :activity 'running)))
      (setq-local mevedel--session session)
      (setf (mevedel-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (mevedel-save-session t)
                               :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (mevedel-session-agent-registry session)))))))
  :doc "save-as canonically publishes the parent and cloned sidecar"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (let* ((session (mevedel-session-create "main" workspace))
           (buffer (generate-new-buffer " *save-as-publication*"))
           (save-function
            (symbol-function 'mevedel-session-artifacts-save))
           (publish-function
            (symbol-function 'mevedel-session-artifacts-publish-text))
           (canonical-saves 0)
           published)
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local mevedel--session session)
            (insert "original\n")
            (mevedel-session-artifacts-save session buffer)
            (goto-char (point-max))
            (insert "pending\n")
            (cl-letf
                (((symbol-function 'read-string)
                  (lambda (&rest _) "clone"))
                 ((symbol-function 'mevedel-session-artifacts-save)
                  (lambda (&rest arguments)
                    (cl-incf canonical-saves)
                    (apply save-function arguments)))
                 ((symbol-function
                   'mevedel-session-artifacts-publish-text)
                  (lambda (actual-session path content &optional coding)
                    (push path published)
                    (funcall publish-function
                             actual-session path content coding))))
              (mevedel-save-session t))
            (should (= 1 canonical-saves))
            (should
             (member
              (mevedel-session-artifacts-sidecar-path
               (mevedel-session-save-path session))
              published))
            (should-not (buffer-modified-p)))
        (test-mevedel-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (mevedel-workspace-clear-registry))))
  :doc "remote save-as fences the parent until a fresh child lease is held"
  (let* ((host "save-as-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-save-as-remote-" t)))
         (owner-id (make-string 64 ?a))
         (competitor-id (make-string 64 ?b))
         buffer
         session
         old-id
         old-save-path
         new-save-path)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (unwind-protect
              (cl-destructuring-bind
                  (workspace fixture-session session-dir segment)
                  (test-mevedel-session-persistence--make-remote-restore-fixture
                   host local-root "Parent transcript\n")
                (setq session fixture-session
                      old-id (mevedel-session-session-id fixture-session)
                      old-save-path session-dir
                      buffer (generate-new-buffer " *save-as-lease*"))
                (let* ((mevedel-session-durability--client-id owner-id)
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal))
                       (materialize-function
                        (symbol-function
                         'mevedel-session-rewind-materialize-publication))
                       (publish-function
                        (symbol-function
                         'mevedel-session-publication-publish))
                       parent-generation
                       competitor-blocked
                       fresh-child
                       materialized
                       copy-called
                       child-rewrite-authorized
                       child-session
                       parent-artifacts
                       staging-path
                       target-probed)
                  (puthash
                   (mevedel-execution-target-identity
                    (mevedel-session-execution-target session))
                   t mevedel-session-durability--disclosed-targets)
                  ;; Start the parent above generation one so copied lease
                  ;; state cannot look like a freshly acquired child.
                  (should
                   (mevedel-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (mevedel-session-durability-lease-release
                   old-save-path session)
                  (should
                   (mevedel-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (setq parent-generation
                        (plist-get
                         (mevedel-session-lease session) :generation))
                  (should (> parent-generation 1))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session
                                buffer-file-name segment)
                    (insert "Parent transcript\n"))
                  (cl-letf
                      (((symbol-function 'read-string)
                        (lambda (&rest _) "clone"))
                       ((symbol-function 'mevedel-execution-target-probe)
                        (lambda (&rest _)
                          (setq target-probed t)
                          '(:status ready)))
                       ((symbol-function 'copy-directory)
                        (lambda (&rest _)
                          (setq copy-called t)
                          (error "Remote Save As must not copy a session directory")))
                       ((symbol-function
                         'mevedel-session-rewind-materialize-publication)
                        (lambda (actual-session publication destination)
                          (let ((mevedel-session-durability--client-id
                                 competitor-id))
                            (should-not
                             (mevedel-session-durability-lease-acquire
                              old-save-path "*save-as-competitor*")))
                          (setq competitor-blocked t
                                staging-path destination)
                          (setq parent-artifacts
                                (sort
                                 (mapcar #'car
                                         (plist-get publication :artifacts))
                                 #'string-lessp))
                          (setq materialized t)
                          (funcall materialize-function
                                   actual-session publication destination)
                          (should (file-exists-p
                                   (file-name-concat destination
                                                     "segment-0001.chat.org")))
                          (should-not
                           (file-exists-p
                            (file-name-concat destination ".publications")))
                          (should-not
                           (file-exists-p
                            (file-name-concat destination ".recovery")))
                          (should-not
                           (file-exists-p
                           (file-name-concat destination ".lock")))))
                        ((symbol-function
                        'mevedel-session-publication-publish)
                        (lambda (actual-session artifacts)
                          (let ((sidecar-artifact
                                 (cl-find-if
                                  (lambda (artifact)
                                    (equal
                                     (plist-get artifact :path)
                                     (mevedel-session-artifacts-sidecar-path
                                      (mevedel-session-save-path actual-session))))
                                  artifacts)))
                            (when (and staging-path sidecar-artifact)
                            (should-not (eq actual-session session))
                            (setq fresh-child t)
                            (setq child-session actual-session)
                            (should
                             (equal staging-path
                                    (mevedel-session-save-path actual-session)))
                            (should (plist-get sidecar-artifact
                                               :commit-marker))
                            (should
                             (mevedel-session-durability-lease-owned-p
                              actual-session))
                            (should
                             (eq
                              'publishing
                              (plist-get
                               (mevedel-session-durability--lease-head
                                (mevedel-session-durability--lease-path
                                 old-save-path))
                               :status)))
                            (setq child-rewrite-authorized t)))
                          (funcall publish-function
                                   actual-session artifacts))))
                    (should
                     (eq 'portable
                         (mevedel-session-codec-authority-mode
                          session)))
                    (with-current-buffer buffer
                      (mevedel-save-session t))
                  (should target-probed)
                  (should competitor-blocked)
                  (should materialized)
                  (should-not copy-called)
                  (should fresh-child)
                  (should child-rewrite-authorized)
                  (should child-session)
                  (should-not (mevedel-session-lease child-session))
                  (should
                   (mevedel-session-durability-lease-owned-p session))
                  (should
                   (= 1
                      (plist-get (mevedel-session-lease session)
                                 :generation)))
                  (should
                   (eq
                    'released
                    (plist-get
                     (mevedel-session-durability--lease-head
                      (mevedel-session-durability--lease-path old-save-path))
                     :status)))
                  (setq new-save-path (mevedel-session-save-path session))
                  (let ((publication
                         (mevedel-session-publication-read
                          new-save-path)))
                    (should publication)
                    (should
                     (equal parent-artifacts
                            (sort
                             (mapcar #'car
                                     (plist-get publication :artifacts))
                             #'string-lessp)))
                    (should
                     (= 1
                        (length
                         (directory-files
                          (file-name-concat new-save-path ".publications")
                          nil "\\`generation-"))))
                    (should
                     (> (length
                         (directory-files
                          (file-name-concat old-save-path ".publications")
                          nil "\\`generation-"))
                        1))
                    (let ((transcript
                           (condition-case err
                               (mevedel-session-artifacts-read-artifact
                                session "segment-0001.chat.org" t)
                             (error
                              (ert-fail
                               (format
                                "Save As child transcript read failed: %S; publication=%S"
                                err publication))))))
                      (should (equal "Parent transcript\n" transcript)))
                    (let ((sidecar
                           (with-temp-buffer
                             (insert
                              (mevedel-session-artifacts-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "clone"
                                     (plist-get sidecar :session-name)))
                      (should
                       (equal old-id
                              (plist-get sidecar
                                         :forked-from-session-id)))))))
            (when (and session (mevedel-session-save-path session))
              (ignore-errors
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path session) session)))
            (when old-save-path
              (ignore-errors
                (mevedel-session-persistence-lock-release old-save-path)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))))


;;
;;; Phase 10: resume / list / save commands

(mevedel-deftest mevedel-session-persistence--entry-authority ()
  ,test
  (test)
  :doc "names the action a portable session's lease state actually produces"
  (let* ((root (make-temp-file "mevedel-entry-action-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-id "s-1")))
         (status nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-durability-lease-status)
              (lambda (&rest _) status))
             ((symbol-function
               'mevedel-session-persistence--entry-live-buffer)
              (lambda (&rest _) nil)))
          (setq status '(:state foreign :host "desktop"))
          (should
           (equal '(:action "Join" :detail "held by desktop" :held t)
                  (mevedel-session-persistence--entry-authority
                   workspace entry)))
          ;; An expired lease offers a takeover precisely because nobody is
          ;; writing the session any more, so it is not a live writer.
          (setq status '(:state expired :host "laptop"))
          (should
           (equal '(:action "Take over" :detail "lease expired, was laptop"
                    :held nil)
                  (mevedel-session-persistence--entry-authority
                   workspace entry)))
          ;; An unheld lease resumes wherever its files live; the machine
          ;; that last held it is context, not a different action.
          (setq status (list :state 'available :host "laptop"))
          (should
           (equal '(:action "Resume" :detail "last held by laptop" :held nil)
                  (mevedel-session-persistence--entry-authority
                   workspace entry)))
          (setq status (list :state 'available :host (system-name)))
          (should
           (equal '(:action "Resume" :detail nil :held nil)
                  (mevedel-session-persistence--entry-authority
                   workspace entry)))
          ;; A lease predating recorded hosts still resumes.
          (setq status '(:state available :host nil))
          (should
           (equal "Resume"
                  (mevedel-session-persistence--entry-action
                   workspace entry))))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry)))

  :doc "prefers switching to a session this Emacs already has open"
  (let* ((root (make-temp-file "mevedel-entry-action-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-id "s-1")))
         (live (generate-new-buffer " *mevedel-entry-live*")))
    (unwind-protect
        (cl-letf
            (((symbol-function
               'mevedel-session-persistence--entry-live-buffer)
              (lambda (&rest _) live))
             ((symbol-function 'mevedel-session-durability-lease-status)
              (lambda (&rest _)
                (ert-fail "An open session must not cost a lease read"))))
          (should
           (equal '(:action "Switch" :detail "already open here" :held t)
                  (mevedel-session-persistence--entry-authority
                   workspace entry)))
          (with-current-buffer live
            (setq-local mevedel-session--read-only-mode t))
          (should
           (equal '(:action "Switch" :detail "already open here, read-only"
                    :held t)
                  (mevedel-session-persistence--entry-authority
                   workspace entry))))
      (when (buffer-live-p live)
        (kill-buffer live))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-choose-entry ()
  ,test
  (test)
  :doc "offers safe new-session disclosure and restores the chosen session"
  (let* ((root (make-temp-file "mevedel-entry-choice-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-name "main")))
         (choice "Start new session")
         (warned nil)
         (authority '(:action "Join" :detail "held by desktop" :held t))
         (restored (generate-new-buffer " *mevedel-entry-restored*")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'mevedel-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function
               'mevedel-session-persistence--sweep-stale-locks)
              #'ignore)
             ((symbol-function 'mevedel-session-persistence-list-sessions)
              (lambda (&rest _) (list entry)))
             ((symbol-function
               'mevedel-session-persistence--entry-authority)
              (lambda (&rest _) authority))
             ((symbol-function
               'mevedel-session-persistence--format-session-candidate)
              (lambda (&rest _) "main"))
             ((symbol-function
               'mevedel-session-persistence--ordered-display-collection)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (&rest _) choice))
             ((symbol-function 'yes-or-no-p)
              (lambda (&rest _) (setq warned t)))
             ((symbol-function 'mevedel-session-persistence-restore)
              (lambda (path &rest _)
                (should (equal "/session/" path))
                restored)))
          (should
           (eq 'new
               (mevedel-session-persistence-choose-entry workspace)))
          (should warned)
          (setq choice "Join       main")
          (should
           (eq restored
               (mevedel-session-persistence-choose-entry workspace)))
          ;; Nothing holds an unheld session, so starting an independent one
          ;; alongside it needs no warning.  Neither does an expired lease:
          ;; its writer is gone, which is why the row offers a takeover.
          (dolist (unheld '((:action "Resume" :detail nil :held nil)
                            (:action "Take over" :detail "lease expired"
                             :held nil)))
            (setq authority unheld
                  choice "Start new session"
                  warned nil)
            (should
             (eq 'new
                 (mevedel-session-persistence-choose-entry workspace)))
            (should-not warned)))
      (when (buffer-live-p restored)
        (kill-buffer restored))
      (when (file-directory-p root)
        (delete-directory root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-list-sessions (:quiet t)
  ,test
  (test)
  :doc "lists materialized sessions, sorted newest-first"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "alpha" workspace))
               (b1 (generate-new-buffer "*test-session-alpha*"))
               (s2 (mevedel-session-create "beta" workspace))
               (b2 (generate-new-buffer "*test-session-beta*")))
          (unwind-protect
              (mevedel-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (insert "Hello\n")
                  (mevedel-session-artifacts-save s1 b1))
                ;; Advance the stamps so `:updated-at' differs.
                (setq mevedel-test--timestamp-offset 2)
                (with-current-buffer b2
                  (org-mode)
                  (insert "World\n")
                  (mevedel-session-artifacts-save s2 b2))
                (let ((listed (mevedel-session-persistence-list-sessions
                               workspace)))
                  (should (= 2 (length listed)))
                  ;; b2 (beta) was saved last → first in list.
                  (should (equal "beta"
                                 (plist-get
                                  (plist-get (car listed) :summary)
                                  :session-name)))
                  (should (equal "alpha"
                                 (plist-get
                                  (plist-get (cadr listed) :summary)
                                  :session-name)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)
      (clrhash mevedel-session-persistence--list-sessions-cache)))
  :doc "reuses the last live enumeration only when asked"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "one" workspace))
               (b1 (generate-new-buffer "*test-session-one*"))
               (s2 (mevedel-session-create "two" workspace))
               (b2 (generate-new-buffer "*test-session-two*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "First\n")
                  (mevedel-session-artifacts-save s1 b1))
                (should (= 1 (length
                              (mevedel-session-persistence-list-sessions
                               workspace))))
                (with-current-buffer b2
                  (org-mode)
                  (insert "Second\n")
                  (mevedel-session-artifacts-save s2 b2))
                ;; The cached listing is as old as the last live one.
                (should (= 1 (length
                              (mevedel-session-persistence-list-sessions
                               workspace 'cached))))
                (should (= 2 (length
                              (mevedel-session-persistence-list-sessions
                               workspace))))
                ;; A live enumeration refreshes what cached readers see.
                (should (= 2 (length
                              (mevedel-session-persistence-list-sessions
                               workspace 'cached)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)
      (clrhash mevedel-session-persistence--list-sessions-cache)))
  :doc "returns nil for a workspace with no sessions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (null (mevedel-session-persistence-list-sessions workspace)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "remote listing omits nil or corrupt heads without reading transcript bytes"
  (let* ((host "list-publications")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-list-publications-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace _session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((sessions-dir
                    (mevedel-session-artifacts-sessions-dir workspace))
                   (nil-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "nil-head")))
                   (corrupt-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "corrupt-head")))
                   (publication
                    (mevedel-session-publication-read session-dir))
                   (segment-entry
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get publication :artifacts))))
                   (manifest
                    (file-name-concat session-dir
                                      (plist-get publication :head))))
              ;; Listing eagerly verifies only the immutable sidecar, so a
              ;; corrupted transcript is diagnosed when consumed, not here.
              (write-region "corrupt transcript" nil
                            (plist-get segment-entry :published) nil 'silent)
              (make-directory nil-head t)
              (mevedel-session-codec-write
               (mevedel-session-artifacts-sidecar-path nil-head)
               (test-mevedel-session-persistence--complete-sidecar
                '(:session-id "nil-head" :session-name "nil-head")))
              (copy-directory session-dir corrupt-head nil t t)
              (let* ((corrupt-manifest
                      (file-name-concat corrupt-head
                                        (file-relative-name manifest session-dir)))
                     (value
                      (mevedel-session-codec-read corrupt-manifest))
                     (entry (car (plist-get value :artifacts))))
                (setf (plist-get (cdr entry) :published) "../escape")
                (mevedel-session-codec-write corrupt-manifest value))
              (let ((listed
                     (mevedel-session-persistence-list-sessions workspace)))
                (should (= 1 (length listed)))
                (should
                 (equal session-dir (plist-get (car listed) :save-path)))
                (should (plist-get (car listed) :publication))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "discovers a Save As child through alias B and resumes its full state"
  (let* ((alias-a "portable-resume-a")
         (alias-b "portable-resume-b")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-portable-resume-" t)))
         restored)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list alias-a alias-b)
          (cl-destructuring-bind (_workspace-a session-a session-dir segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               alias-a local-root "Portable transcript\n")
            (let* ((root-a
                    (mevedel-workspace-root
                     (mevedel-session-workspace session-a)))
                   (identity
                    (mevedel-workspace-identity-read root-a))
                   (root-b
                    (format "/mevedelmock:%s:%s"
                            alias-b (file-name-as-directory local-root)))
                   session-id
                   (mevedel-session-durability--client-id
                    (make-string 64 ?b))
                   (mevedel-session-durability--disclosed-targets
                    (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session-a))
               t mevedel-session-durability--disclosed-targets)
              (let ((buffer (generate-new-buffer " *save-as-alias*")))
                (unwind-protect
                    (progn
                      (should
                       (mevedel-session-durability-lease-acquire
                        session-dir "save-as-alias" session-a))
                      (with-current-buffer buffer
                        (org-mode)
                        (setq-local mevedel--session session-a
                                    buffer-file-name segment)
                        (insert "Portable transcript\n")
                        (cl-letf (((symbol-function 'read-string)
                                   (lambda (&rest _) "alias-clone")))
                          (mevedel-save-session t)))
                      (setq session-id
                            (mevedel-session-session-id session-a))
                      (mevedel-session-persistence-lock-release
                       (mevedel-session-save-path session-a) session-a))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (set-buffer-modified-p nil)
                      (set-visited-file-name nil t)
                      (setq-local kill-buffer-hook nil))
                    (kill-buffer buffer))))
              (mevedel-workspace-clear-registry)
              (let* ((workspace-b
                      (mevedel-workspace-get-or-create
                       'project root-b root-b "portable-resume"))
                     (target-b (mevedel-execution-target-create root-b))
                     (listed
                      (mevedel-session-persistence-list-sessions workspace-b))
                     (child
                      (cl-find
                       session-id listed
                       :key (lambda (entry)
                              (plist-get (plist-get entry :summary)
                                         :session-id))
                       :test #'equal)))
                (should
                 (equal identity
                        (mevedel-workspace-identity-read root-b)))
                (should (= 2 (length listed)))
                (should child)
                (should
                 (string-prefix-p root-b
                                  (plist-get child :save-path)))
                (puthash
                 (mevedel-execution-target-identity target-b)
                 t mevedel-session-durability--disclosed-targets)
                (cl-letf
                    (((symbol-function 'mevedel--chat-buffer-init-common)
                      #'ignore)
                     ((symbol-function
                       'mevedel-agent-persistence-restore-tree)
                      (lambda (&rest _) 0))
                     ((symbol-function
                       'mevedel-session-artifacts-load-instructions)
                      #'ignore))
                  (setq restored
                        (mevedel-session-persistence-resume-id
                         workspace-b session-id)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (string-match-p
                           "Portable transcript" (buffer-string)))
                  (should
                   (eq workspace-b
                       (mevedel-session-workspace mevedel--session)))
                  (should
                   (equal root-b
                          (mevedel-workspace-root
                           (mevedel-session-workspace mevedel--session))))
                  (should
                   (equal root-b
                          (mevedel-session-working-directory
                           mevedel--session))))))))
      (when (buffer-live-p restored)
        (mevedel-test--with-local-shell-tramp (list alias-a alias-b)
          (let ((session (buffer-local-value 'mevedel--session restored)))
            (when (and session (mevedel-session-save-path session))
              (mevedel-session-persistence-lock-release
               (mevedel-session-save-path session) session))
            (with-current-buffer restored
              (set-buffer-modified-p nil))
            (kill-buffer restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "resume completion preserves newest-first session order"
  (let* ((displays '("2h ago       new" "yesterday    old"))
         (collection
          (mevedel-session-persistence--ordered-display-collection
           displays 'mevedel-session))
         (metadata (funcall collection "" nil 'metadata)))
    (should (eq 'identity
                (cdr (assq 'display-sort-function (cdr metadata)))))
    (should (eq 'identity
                (cdr (assq 'cycle-sort-function (cdr metadata)))))))


(mevedel-deftest mevedel-session-persistence-conversation-variants ()
  ,test
  (test)
  :doc "finds the persisted Source and one direct Child from either variant"
  (let* ((root (make-temp-file "mevedel-variants-" t))
         (workspace
          (test-mevedel-session-persistence--make-workspace root))
         (source (mevedel-session-create "source" workspace))
         (child (mevedel-session-create "child" workspace))
         (source-entry
          (copy-tree
           '(:save-path "/sessions/source/"
             :summary (:session-id "source-id"
                       :session-name "source"
                       :created-at "2026-07-01T10:00:00+0200"
                       :fork-point-ids ("fork-point-1")
                       :working-directory "/repo/"))))
         (child-entry
          '(:save-path "/sessions/child/"
            :summary (:session-id "child-id"
                      :session-name "child"
                      :created-at "2026-07-01T10:01:00+0200"
                      :fork-point-ids ("fork-point-1" "later-point")
                      :working-directory "/repo/"
                      :forked-from-session-id "source-id"
                      :forked-from-fork-point-id "fork-point-1"
                      :fork-type conversation)))
         (worktree-entry
          '(:save-path "/sessions/worktree/"
            :summary (:session-id "worktree-id"
                      :session-name "worktree"
                      :created-at "2026-07-01T10:02:00+0200"
                      :fork-point-ids ("fork-point-1")
                      :working-directory "/repo/.worktrees/fork/"
                      :forked-from-session-id "source-id"
                      :forked-from-fork-point-id "fork-point-1"
                      :fork-type worktree)))
         (grandchild-entry
          '(:save-path "/sessions/grandchild/"
            :summary (:session-id "grandchild-id"
                      :session-name "grandchild"
                      :created-at "2026-07-01T10:03:00+0200"
                      :fork-point-ids ("later-point")
                      :working-directory "/repo/"
                      :forked-from-session-id "child-id"
                      :forked-from-fork-point-id "later-point"
                      :fork-type conversation)))
         (entries
          (list grandchild-entry worktree-entry child-entry source-entry)))
    (unwind-protect
        (progn
          (setf (mevedel-session-session-id source) "source-id"
                (mevedel-session-save-path source) "/sessions/source/"
                (mevedel-session-session-id child) "child-id"
                (mevedel-session-save-path child) "/sessions/child/"
                (mevedel-session-forked-from-session-id child) "source-id"
                (mevedel-session-forked-from-fork-point-id child)
                "fork-point-1"
                (mevedel-session-fork-type child) 'conversation)
          (cl-letf
              (((symbol-function
                 'mevedel-session-persistence-list-sessions)
                (lambda (_workspace) entries)))
            (dolist (session (list source child))
              (let ((variants
                     (mevedel-session-persistence-conversation-variants
                      session "fork-point-1")))
                (should (equal '("source-id" "child-id" "worktree-id")
                               (mapcar
                                (lambda (entry)
                                  (plist-get
                                   (plist-get entry :summary)
                                   :session-id))
                                variants)))
                (should (equal '(source conversation worktree)
                               (mapcar
                                (lambda (entry)
                                  (plist-get entry :variant-origin))
                                variants)))))
            ;; Forking a later Child response makes that Child the Source of
            ;; an independent group; inherited lineage is not flattened.
            (let ((later
                   (mevedel-session-persistence-conversation-variants
                    child "later-point")))
              (should
               (equal '("child-id" "grandchild-id")
                      (mapcar
                       (lambda (entry)
                         (plist-get (plist-get entry :summary) :session-id))
                       later)))
              (should
               (equal '(source conversation)
                      (mapcar
                       (lambda (entry)
                         (plist-get entry :variant-origin))
                       later))))
            ;; Rewind detaches Source by removing the stable point, but the
            ;; surviving direct Children remain a sibling group.
            (plist-put (plist-get source-entry :summary)
                       :fork-point-ids '("different-point"))
            (should
             (equal '("child-id" "worktree-id")
                    (mapcar
                     (lambda (entry)
                       (plist-get (plist-get entry :summary) :session-id))
                     (mevedel-session-persistence-conversation-variants
                      child "fork-point-1"))))
            ;; Removing a sibling removes only that entry and the affordance
            ;; naturally disappears when the current session is alone.
            (setq entries (list child-entry))
            (should
             (equal '("child-id")
                    (mapcar
                     (lambda (entry)
                       (plist-get (plist-get entry :summary) :session-id))
                     (mevedel-session-persistence-conversation-variants
                      child "fork-point-1"))))))
      (delete-directory root t))))


(mevedel-deftest mevedel-session-persistence-choose-conversation-variant ()
  ,test
  (test)
  :doc "shows stable rich entries and marks the current variant without moving it"
  (let* ((source
          '(:save-path "/sessions/source/"
            :variant-origin source
            :summary (:session-id "source-id"
                      :session-name "source"
                      :working-directory "/repo/"
                      :latest-user-message "original prompt")))
         (conversation
          '(:save-path "/sessions/conversation/"
            :variant-origin conversation
            :summary (:session-id "conversation-id"
                      :session-name "conversation"
                      :working-directory "/repo/"
                      :latest-user-message "shared prompt")))
         (worktree
          '(:save-path "/sessions/worktree/"
            :variant-origin worktree
            :summary (:session-id "worktree-id"
                      :session-name "worktree"
                      :working-directory "/replacement/"
                      :latest-user-message "isolated prompt"
                      :worktree-directory "/missing-worktree/"
                      :worktree-branch "worktree/source-fork-1")))
         displays)
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _)
            (setq displays (all-completions "" collection))
            (car (last displays)))))
      (should
       (eq worktree
           (mevedel-session-persistence-choose-conversation-variant
            (list source conversation worktree) "worktree-id"))))
    (should (string-prefix-p "  Source" (nth 0 displays)))
    (should (string-match-p "/repo/" (nth 0 displays)))
    (should (string-prefix-p "  Conversation" (nth 1 displays)))
    (should (string-match-p "shared files" (nth 1 displays)))
    (should (string-match-p "shared prompt" (nth 1 displays)))
    (should (string-prefix-p "* Worktree" (nth 2 displays)))
    (should (string-match-p "worktree/source-fork-1" (nth 2 displays)))
    (should (string-match-p "retargeted; original missing"
                            (nth 2 displays)))
    (should (string-match-p "isolated prompt" (nth 2 displays)))
    (plist-put (plist-get conversation :summary)
               :working-directory "/other/")
    (cl-letf
        (((symbol-function 'completing-read)
          (lambda (_prompt collection &rest _)
            (setq displays (all-completions "" collection))
            (car displays))))
      (mevedel-session-persistence-choose-conversation-variant
       (list source conversation worktree) "source-id"))
    (should (string-match-p "independent directory" (nth 1 displays)))))


(mevedel-deftest mevedel-session-persistence--read-summary ()
  ,test
  (test)
  :doc "reuses unchanged sidecars and refreshes atomically replaced files"
  (let ((tmp (make-temp-file "mevedel-summary-cache-" nil ".el"))
        (mevedel-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'mevedel-session-codec-read))
        (read-count 0))
    (unwind-protect
        (cl-labels
            ((write-sidecar
              (name)
              (mevedel-session-codec-write
               tmp
               (test-mevedel-session-persistence--complete-sidecar
                `(:session-name ,name :session-id "cache-test")))))
          (cl-letf
              (((symbol-function 'mevedel-session-codec-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (write-sidecar "first")
            (should
             (equal "first"
                    (plist-get
                     (mevedel-session-persistence--read-summary tmp)
                     :session-name)))
            (mevedel-session-persistence--read-summary tmp)
            (should (= 1 read-count))
            (write-sidecar "second")
            (should
             (equal "second"
                    (plist-get
                     (mevedel-session-persistence--read-summary tmp)
                     :session-name)))
            (should (= 2 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "reuses a cached failure for an unchanged invalid sidecar"
  (let ((tmp (make-temp-file "mevedel-summary-cache-invalid-" nil ".el"))
        (mevedel-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'mevedel-session-codec-read))
        (read-count 0))
    (unwind-protect
        (progn
          (write-region "invalid" nil tmp nil 'silent)
          (cl-letf
              (((symbol-function 'mevedel-session-codec-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (should-not
             (mevedel-session-persistence--read-summary tmp))
            (should-not
             (mevedel-session-persistence--read-summary tmp))
            (should (= 1 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "extracts only picker-relevant fields"
  (let ((tmp (make-temp-file "mevedel-summary-test-" nil ".el")))
    (unwind-protect
        (progn
          (mevedel-session-codec-write
           tmp
           (test-mevedel-session-persistence--complete-sidecar
            `(:session-name "demo"
              :session-id "demo-1234"
              :updated-at "2026-04-23T12-00-00"
              :first-user-message "Hello"
              :latest-user-message "Latest"
              :prompt-index
              ((1 . ((:turn 1 :file-turn 1 :cum-turn 1
                      :fork-point-id "fork-point-1")
                     (:turn 2 :file-turn 2 :cum-turn 2)
                     (:turn 3 :file-turn 3 :cum-turn 3
                      :fork-point-id "fork-point-2")))))))
          (let ((s (mevedel-session-persistence--read-summary tmp)))
            (should (equal "demo" (plist-get s :session-name)))
            (should (equal "demo-1234" (plist-get s :session-id)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))
            (should (equal '("fork-point-1" "fork-point-2")
                           (plist-get s :fork-point-ids)))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "returns nil on unreadable file"
  (should (null (mevedel-session-persistence--read-summary
                 "/nonexistent/path"))))


(mevedel-deftest mevedel-session-persistence--format-session-candidate ()
  ,test
  (test)
  :doc "prefers latest preview over first preview"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 2
                      :total-turn-count 4
                      :first-user-message "Original request"
                      :latest-user-message "Newest request")))))
    (should (string-match-p "Newest request" display))
    (should-not (string-match-p "Original request" display)))
  :doc "falls back to first preview for old summaries"
  (let ((display
         (mevedel-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 1
                      :total-turn-count 1
                      :first-user-message "Original request")))))
    (should (string-match-p "Original request" display))))


;;
;;; Phase 12: auto-cleanup

(mevedel-deftest mevedel-session-persistence-parse-iso-time ()
  ,test
  (test)
  :doc "parses our ISO-with-dashes format"
  (let ((time (mevedel-session-persistence-parse-iso-time
               "2026-04-23T14-30-15")))
    (should time)
    (should (equal "2026-04-23T14-30-15"
                   (format-time-string "%FT%H-%M-%S" time))))
  :doc "returns nil for malformed input"
  (should (null (mevedel-session-persistence-parse-iso-time "not a date")))
  (should (null (mevedel-session-persistence-parse-iso-time nil))))


(mevedel-deftest mevedel-session-persistence-cleanup-expired (:quiet t)
  ,test
  (test)
  :doc "deletes sessions older than the cap"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               ;; Reset the throttle so tests don't leak.
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s1 (mevedel-session-create "old" workspace))
               (b1 (generate-new-buffer "*test-old-buf*"))
               (s2 (mevedel-session-create "new" workspace))
               (b2 (generate-new-buffer "*test-new-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-artifacts-save s1 b1))
                (with-current-buffer b2
                  (org-mode)
                  (insert "New\n")
                  (mevedel-session-artifacts-save s2 b2))
                (let ((archive
                       (file-name-concat
                        (mevedel-session-save-path s1)
                        "agents/old.compact-0001.chat.org")))
                  (make-directory (file-name-directory archive) t)
                  (write-region "recovery archive\n" nil archive nil 'silent)
                  (should (file-exists-p archive)))
                ;; Forge :updated-at on the old session to be 14 days ago.
                (let* ((old-path (mevedel-session-save-path s1))
                       (sidecar  (mevedel-session-artifacts-sidecar-path
                                  old-path))
                       (plist    (mevedel-session-codec-read sidecar))
                       (forged   (format-time-string
                                  "%FT%H-%M-%S"
                                  (time-subtract (current-time)
                                                 (* 14 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-codec-write sidecar plist))
                ;; Release locks so cleanup can delete the dirs.
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s1) s1)
                (mevedel-session-persistence-lock-release
                 (mevedel-session-save-path s2) s2)
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 1 deleted))
                  (should-not (file-directory-p
                               (mevedel-session-save-path s1)))
                  (should (file-directory-p
                           (mevedel-session-save-path s2)))))
            (when (buffer-live-p b1)
              (with-current-buffer b1 (set-buffer-modified-p nil))
              (kill-buffer b1))
            (when (buffer-live-p b2)
              (with-current-buffer b2 (set-buffer-modified-p nil))
              (kill-buffer b2))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions with obsolete sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "obsolete" workspace))
               (buf (generate-new-buffer "*test-obsolete-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-artifacts-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path save-path))
                       (plist (mevedel-session-codec-read sidecar))
                       (forged
                        (format-time-string
                         "%FT%H-%M-%S"
                         (time-subtract (current-time) (* 14 24 60 60)))))
                  (cl-remf plist :plan-mode)
                  (plist-put plist :updated-at forged)
                  (mevedel-session-codec-write sidecar plist)
                  (mevedel-session-persistence-lock-release save-path session)
                  (should (= 1
                             (mevedel-session-persistence-cleanup-expired
                              workspace t)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions without sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "missing" workspace))
               (buf (generate-new-buffer "*test-missing-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (mevedel-session-artifacts-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path save-path))
                       (old-time
                        (time-subtract (current-time) (* 14 24 60 60))))
                  (mevedel-session-persistence-lock-release save-path session)
                  (delete-file sidecar)
                  (set-file-times save-path old-time)
                  (should (= 1
                             (mevedel-session-persistence-cleanup-expired
                              workspace t)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "skips locked sessions even when expired"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "stuck" workspace))
               (b (generate-new-buffer "*test-stuck-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-artifacts-save s b))
                ;; Forge old :updated-at.
                (let* ((path (mevedel-session-save-path s))
                       (sidecar (mevedel-session-artifacts-sidecar-path
                                 path))
                       (plist   (mevedel-session-codec-read sidecar))
                       (forged  (format-time-string
                                 "%FT%H-%M-%S"
                                 (time-subtract (current-time)
                                                (* 30 24 60 60)))))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-codec-write sidecar plist))
                ;; The lock from save still exists with our PID — live.
                (let ((deleted
                       (mevedel-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 0 deleted))
                  (should (file-directory-p
                           (mevedel-session-save-path s)))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "never deletes expired remote sessions with an active lease"
  (let* ((host "cleanup-remote-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-cleanup-remote-" t))))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((mevedel-session-max-age-days 7)
                  (mevedel-session-persistence--cleanup-throttle
                   (make-hash-table :test #'equal))
                  (mevedel-session-durability--client-id
                   (make-string 64 ?a))
                  (mevedel-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (mevedel-execution-target-identity
                (mevedel-session-execution-target session))
               t mevedel-session-durability--disclosed-targets)
              (let* ((sidecar-path
                      (mevedel-session-artifacts-sidecar-path session-dir))
                     (sidecar
                      (mevedel-session-codec-read sidecar-path)))
                (plist-put
                 sidecar :updated-at
                 (format-time-string
                  "%FT%H-%M-%S"
                  (time-subtract (current-time) (* 30 24 60 60))))
                (mevedel-session-codec-write sidecar-path sidecar))
              (unwind-protect
                  (progn
                    (should
                     (mevedel-session-durability-lease-acquire
                      session-dir "*cleanup-owner*" session))
                    (should-not
                     (mevedel-session-persistence-cleanup-expired
                      workspace t))
                    (should (file-directory-p session-dir))
                    (should
                     (mevedel-session-durability-lease-owned-p session)))
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry)))
  :doc "deletes expired sessions whose same-host lock has a reused PID"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (mevedel-session-create "reused" workspace))
               (b (generate-new-buffer "*test-reused-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (mevedel-session-artifacts-save s b))
                (let* ((path      (mevedel-session-save-path s))
                       (sidecar   (mevedel-session-artifacts-sidecar-path
                                   path))
                       (plist     (mevedel-session-codec-read sidecar))
                       (old-time  (time-subtract (current-time)
                                                 (* 30 24 60 60)))
                       (forged    (format-time-string "%FT%H-%M-%S"
                                                       old-time))
                       (lock-path (mevedel-session-persistence--lock-path
                                   path)))
                  (plist-put plist :updated-at forged)
                  (mevedel-session-codec-write sidecar plist)
                  (with-temp-file lock-path
                    (prin1 (list :pid 12345
                                 :hostname (system-name)
                                 :emacs-invocation-time forged
                                 :buffer "*old-buf*")
                           (current-buffer))))
                (cl-letf (((symbol-function
                            'mevedel-session-persistence--pid-alive-p)
                           (lambda (&rest _) t))
                          ((symbol-function
                            'mevedel-session-persistence--pid-start-time)
                           (lambda (&rest _) (current-time))))
                  (let ((deleted
                         (mevedel-session-persistence-cleanup-expired
                          workspace t)))
                    (should (= 1 deleted))
                    (should-not (file-directory-p
                                 (mevedel-session-save-path s))))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let ((mevedel-session-max-age-days nil))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace t))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))
  :doc "throttled to at most one run per workspace per Emacs"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 7)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal)))
          ;; First call returns 0 (no sessions); second call (no force) returns nil.
          (should (= 0 (mevedel-session-persistence-cleanup-expired
                        workspace)))
          (should (null (mevedel-session-persistence-cleanup-expired
                         workspace))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; View rerender on resume / rewind

(mevedel-deftest mevedel-session-persistence--hydrate-restored-buffer ()
  ,test
  (test)
  :doc "plants session state after Org setup and before transcript restoration"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-restore-hydrate-" t)))
         (workspace (test-mevedel-session-persistence--make-workspace root))
         (session (mevedel-session-create "main" workspace root))
         (buffer (generate-new-buffer " *mevedel-restore-hydrate*"))
         (segment-path (file-name-concat root "segment-0001.chat.org"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function
                    'mevedel-transcript-restore-gptel-state)
                   (lambda ()
                     (should (derived-mode-p 'org-mode))
                     (should (eq mevedel--session session))
                     (should (eq mevedel--workspace workspace))
                     (push 'transcript events)))
                  ((symbol-function
                    'mevedel-pipeline-reconcile-lost-executions)
                   (lambda (_buffer) 0))
                  ((symbol-function
                    'mevedel-session-artifacts-check-target-incarnation)
                   (lambda (checked-session checked-buffer)
                     (should (eq checked-session session))
                     (should (eq checked-buffer buffer))
                     (push 'incarnation events)))
                  ((symbol-function 'mevedel--chat-buffer-init-common)
                   (lambda (_buffer _workspace source &optional inspection-p)
                     (should (equal source "resume"))
                     (should-not inspection-p)
                     (push 'chat events)))
                  ((symbol-function
                    'mevedel-agent-persistence-restore-tree)
                   (lambda (_session _buffer read-only-p)
                     (should-not read-only-p)
                     (push 'agents events)
                     2))
                  ((symbol-function
                    'mevedel-session-artifacts-load-instructions)
                   (lambda (_session _buffer)
                     (push 'instructions events))))
          (should
           (= 2
              (mevedel-session-persistence--hydrate-restored-buffer
               buffer session workspace segment-path t nil nil)))
          (should (equal '(transcript incarnation chat agents instructions)
                         (nreverse events)))
          (with-current-buffer buffer
            (should (equal root default-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence--finish-restored-buffer ()
  ,test
  (test)
  :doc "persists repairs and loads history before rendering a fresh buffer"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-restore-finish-" t)))
         (workspace
          (test-mevedel-session-persistence--make-file-workspace root))
         (session (mevedel-session-create "main" workspace root))
         (buffer (generate-new-buffer " *mevedel-restore-finish-data*"))
         observer
         events)
    (setf (mevedel-session-save-path session) root)
    (unwind-protect
        (progn
          (setq observer
                (mevedel-session-control-transfer-register-observer
                 session
                 (lambda (event &rest _args)
                   (pcase event
                     ('load-history (push 'history events))
                     ('rerender (push 'render events))))))
          (cl-letf (((symbol-function
                      'mevedel-session-artifacts-build-sidecar)
                     (lambda (_session _buffer) '(:sidecar t)))
                    ((symbol-function 'mevedel-session-codec-write)
                     (lambda (_path _sidecar) (push 'write events))))
            (should
             (eq buffer
                 (mevedel-session-persistence--finish-restored-buffer
                  buffer session nil t))))
          (should (equal '(write history render) (nreverse events))))
      (mevedel-session-control-transfer-unregister-observer session observer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-find-live-buffer ()
  ,test
  (test)
  :doc "finds only root data buffers, never agent or view projections"
  (let* ((session (mevedel-session--create
                   :name "source" :session-id "source-id"))
         (root-buffer (generate-new-buffer " *source-root*"))
         (view-buffer (generate-new-buffer " *source-view*"))
         (agent-buffer (generate-new-buffer " *source-agent*")))
    (unwind-protect
        (progn
          (with-current-buffer root-buffer
            (setq-local mevedel--session session))
          (with-current-buffer view-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--data-buffer root-buffer))
          (with-current-buffer agent-buffer
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation t))
          (mevedel-session-set-root-buffer session root-buffer)
          (should (eq root-buffer
                      (mevedel-session-persistence-find-live-buffer
                       "source-id" " *source-root*")))
          (kill-buffer root-buffer)
          (should-not
           (mevedel-session-persistence-find-live-buffer
            "source-id" " *source-root*")))
      (dolist (buffer (list agent-buffer view-buffer root-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))


(mevedel-deftest mevedel-session-persistence--find-file-noselect ()
  ,test
  (test)
  :doc "disables so-long predicate while opening persisted files"
  (let ((observed :unset)
        (opened (generate-new-buffer " *mevedel-so-long-open*")))
    (unwind-protect
        (cl-letf (((symbol-function 'find-file-noselect)
                   (lambda (_file &rest _args)
                     (setq observed (funcall so-long-predicate))
                     opened)))
          (should (eq opened
                      (mevedel-session-persistence--find-file-noselect
                       "/tmp/session.chat.org")))
          (should (eq observed nil)))
      (when (buffer-live-p opened)
        (kill-buffer opened)))))


(mevedel-deftest mevedel-session-persistence/view-rerender ()
  ,test
  (test)
  :doc "save path does not rebuild the visible transcript"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local mevedel--view-buffer vb)
                (with-current-buffer vb
                  (setq-local mevedel--data-buffer buf))
                (insert "prompt before save\n")
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (mevedel-session-artifacts-save session buf))
                (should (= rerender-count 0)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-mevedel-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path calls mevedel-view--full-rerender"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (rerender-count 0)
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume test\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (cl-letf (((symbol-function 'mevedel-view--full-rerender)
                           (lambda () (cl-incf rerender-count))))
                  (setq restored
                        (mevedel-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                ;; The rerender may fire via init-common's view-ensure
                ;; flow (which touches the view buffer).  We only care
                ;; that it fires at least once.
                (should (>= rerender-count 1)))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
	  (delete-directory tempdir t)
	  (mevedel-workspace-clear-registry)))

  :doc "resume path renders persisted hook audit records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local gptel-prompt-prefix-alist
                              '((org-mode . "*** ")))
                  (insert "\n\n*** rewritten prompt")
                  (insert
                   (mevedel--format-hook-audit-record
                    '(:type prompt-rewrite
                      :event "UserPromptSubmit"
                      :original "original prompt"
                      :submitted "rewritten prompt")))
                  (insert "\n")
                  (mevedel-session-artifacts-stabilize-gptel-bounds)
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (mevedel-view--full-rerender)
                  (let ((text (buffer-substring-no-properties
                               (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "hook changed prompt" text))
                    (should (string-match-p "rewritten prompt" text))
                    (should-not (string-match-p "original prompt" text)))
                  (goto-char (point-min))
                  (search-forward "hook changed prompt")
                  (mevedel-view-toggle-section)
                  (let ((expanded (buffer-substring-no-properties
                                   (point-min) mevedel-view--input-marker)))
                    (should (string-match-p "original prompt" expanded)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "resume path restores view input history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from history resume test\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (let ((history-path
                       (file-name-concat tempdir ".mevedel/input-history.el")))
                  (make-directory (file-name-directory history-path) t)
                  (mevedel-session-codec-write
                   history-path
                   '(:version 2 :entries ("second" "first"))))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (mevedel-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'mevedel--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (should (equal '("second" "first")
                                 (mevedel-view-history--entries)))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))

  :doc "session chooser displays the companion view buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               restored displayed)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setf (mevedel-session-preset-name session) 'test-preset)
                  (insert "hello from resume display test\n")
                  (mevedel-session-artifacts-save session buf))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (let ((default-directory tempdir))
                  (cl-letf (((symbol-function 'mevedel-workspace)
                             (lambda (&optional _arg) workspace))
                            ((symbol-function
                              'mevedel-session-persistence--ordered-display-collection)
                             (lambda (values &rest _) values))
                            ;; The first candidate starts a new session; the
                            ;; second is the persisted one under test.
                            ((symbol-function 'completing-read)
                             (lambda (_prompt collection &rest _)
                               (cadr collection)))
                            ((symbol-function 'display-buffer)
                             (lambda (buffer &optional _action _frame)
                               (setq displayed buffer)
                               buffer)))
                    (let ((mevedel-preset--registry
                           '((test-preset :parents nil :settings nil))))
                      (mevedel))))
                (should (buffer-live-p displayed))
                (setq restored
                      (buffer-local-value 'mevedel--data-buffer displayed))
                (should (buffer-live-p restored))
                (should (eq displayed
                            (buffer-local-value 'mevedel--view-buffer
                                                restored))))
            (test-mevedel-session-persistence--release-and-kill
             buf session)
            (test-mevedel-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Sidecar missing / unreadable fallback on restore

(mevedel-deftest mevedel-session-persistence/sidecar-missing-on-restore ()
  ,test
  (test)
  :doc "deleted sidecar fails closed without a committed authority profile"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (delete-file
                 (mevedel-session-artifacts-sidecar-path session-dir))
                (should-error
                 (mevedel-session-persistence-restore session-dir)
                 :type 'error))
            (ignore-errors
              (test-mevedel-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))
  :doc "corrupt sidecar fails closed instead of synthesizing authority"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (mevedel-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (mevedel-session-artifacts-save session buf))
                (setq session-dir (mevedel-session-save-path session))
                (test-mevedel-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (write-region "this is not a plist" nil
                              (mevedel-session-artifacts-sidecar-path
                               session-dir)
                              nil 'silent)
                (should-error
                 (mevedel-session-persistence-restore session-dir)
                 :type 'error))
            (ignore-errors
              (test-mevedel-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry)))))


;;
;;; Cross-host cleanup behavior

(mevedel-deftest mevedel-session-persistence/cleanup-cross-host-lock ()
  ,test
  (test)
  :doc "cross-host lock prevents cleanup from deleting an expired session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((mevedel-session-max-age-days 1)
               (mevedel-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (mevedel-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello\n")
                  (mevedel-session-artifacts-save session buf))
                (let* ((save-path (mevedel-session-save-path session))
                       (lock-path
                        (mevedel-session-persistence--lock-path save-path))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path save-path))
                       (plist (mevedel-session-codec-read sidecar))
                       (forged (format-time-string
                                "%FT%H-%M-%S"
                                (time-subtract (current-time)
                                               (* 7 24 60 60)))))
                  ;; Forge an expired :updated-at.
                  (plist-put plist :updated-at forged)
                  (mevedel-session-codec-write sidecar plist)
                  ;; Overwrite our lock with a cross-host lock (still
                  ;; active from cleanup's perspective).
                  (with-temp-file lock-path
                    (prin1 (list :pid 99999
                                 :hostname "other-host.example"
                                 :emacs-invocation-time "..."
                                 :buffer "*remote*")
                           (current-buffer)))
                  ;; Run cleanup.
                  (let ((deleted (mevedel-session-persistence-cleanup-expired
                                  workspace t)))
                    (should (= 0 deleted))
                    (should (file-directory-p save-path)))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Same-name sessions in one workspace

(mevedel-deftest mevedel-session-persistence/same-name-sessions ()
  ,test
  (test)
  :doc "restore resolves the right session-id when two sessions share a name"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (mevedel-session-create "main" workspace))
               (b1 (generate-new-buffer "*test-data-1*"))
               (s2 (mevedel-session-create "main" workspace))
               (b2 (generate-new-buffer "*test-data-2*"))
               restored)
          (unwind-protect
              (mevedel-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (setq-local mevedel--session s1)
                  (insert "session one\n")
                  (mevedel-session-artifacts-save s1 b1))
                ;; Advance the stamps so the derived session ids differ.
                (setq mevedel-test--timestamp-offset 120)
                (with-current-buffer b2
                  (org-mode)
                  (setq-local mevedel--session s2)
                  (insert "session two\n")
                  (mevedel-session-artifacts-save s2 b2))
                (should-not (equal (mevedel-session-session-id s1)
                                   (mevedel-session-session-id s2)))
                ;; Both buffers share the default
                ;; `*mevedel:main@...*' buffer name (identical session
                ;; name + workspace).  Restore must match session-id,
                ;; not just the buffer name, and return b1 when asked
                ;; to resume s1's dir.
                (setq restored
                      (mevedel-session-persistence-restore
                       (mevedel-session-save-path s1)))
                (should (buffer-live-p restored))
                (should (eq restored b1))
                (with-current-buffer restored
                  (should (equal (mevedel-session-session-id s1)
                                 (mevedel-session-session-id mevedel--session)))))
            (test-mevedel-session-persistence--release-and-kill b1 s1)
            (test-mevedel-session-persistence--release-and-kill b2 s2)
            (when (and restored (buffer-live-p restored))
              (test-mevedel-session-persistence--release-and-kill
               restored
               (buffer-local-value 'mevedel--session restored)))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


;;
;;; Session-id collision retry loop

(mevedel-deftest mevedel-session-persistence/id-collision-retry ()
  ,test
  (test)
  :doc "ensure-files retries id generation when the target dir already exists"
  (cl-destructuring-bind (workspace . tempdir)
      (test-mevedel-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((sessions-dir
                (mevedel-session-artifacts-sessions-dir workspace))
               ;; Pre-create a directory that a naive `compute-id'
               ;; would collide with.
               (colliding "main-collision-0001")
               (remaining '("main-collision-0002" "main-collision-0003")))
          (make-directory (file-name-concat sessions-dir colliding) t)
          (let ((session (mevedel-session-create "main" workspace))
                (buf     (generate-new-buffer "*test-data-buf*")))
            (unwind-protect
                (cl-letf*
                    ;; First call returns the colliding id, subsequent
                    ;; calls return fresh ids from `remaining'.
                    ((first-call-p t)
                     ((symbol-function
                       'mevedel-session-artifacts-compute-id)
                      (lambda (_name)
                        (cond
                         (first-call-p
                          (setq first-call-p nil)
                          colliding)
                         (t (pop remaining))))))
                  (with-current-buffer buf
                    (org-mode)
                    (insert "hi\n")
                    (mevedel-session-artifacts-ensure-files session buf)
                    ;; Picked a non-colliding id.
                    (should-not (equal colliding
                                       (mevedel-session-session-id session)))
                    ;; Original colliding dir was not touched.
                    (should (file-directory-p
                             (file-name-concat sessions-dir colliding)))))
              (test-mevedel-session-persistence--release-and-kill
               buf session))))
      (delete-directory tempdir t)
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-write-sidecar-now ()
  ,test
  (test)
  :doc "commits a remote sidecar when its fixed cache is missing"
  (let* ((host "write-sidecar-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "mevedel-write-sidecar-remote-" t)))
         (mevedel-session-durability--client-id (make-string 64 ?a))
         buffer)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-mevedel-session-persistence--make-remote-restore-fixture
                         host local-root "sidecar transcript\n"))
                       (sidecar
                        (mevedel-session-artifacts-sidecar-path session-dir))
                       (mevedel-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (mevedel-execution-target-identity
              (mevedel-session-execution-target session))
             t mevedel-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (mevedel-session-durability-lease-acquire
                    session-dir "*write-sidecar*" session))
                  (setf (mevedel-session-publication session)
                        (mevedel-session-publication-read
                         session-dir)
                        (mevedel-session-name session) "updated")
                  (setq buffer
                        (generate-new-buffer " *write-sidecar-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session)
                    (setq buffer-file-name segment)
                    (insert "sidecar transcript\n"))
                  (let ((head-before
                         (plist-get (mevedel-session-publication session)
                                    :head)))
                    (delete-file sidecar)
                    (should
                     (mevedel-session-persistence-write-sidecar-now
                      session buffer))
                    (should-not
                     (equal head-before
                            (plist-get
                             (mevedel-session-publication session) :head)))
                    (should
                     (equal
                      "updated"
                      (plist-get
                       (with-temp-buffer
                         (insert
                          (mevedel-session-artifacts-read-artifact
                           session "session.meta.el" t))
                         (goto-char (point-min))
                         (read (current-buffer)))
                       :session-name)))))
              (when session
                (mevedel-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-save-agent-state ()
  ,test
  (test)
  :doc "forces a full save through the authoritative root buffer"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-agent-state-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id "agent-state" :root tempdir
           :name "agent-state"))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *agent-state-root*"))
         (agent (generate-new-buffer " *agent-state-child*"))
         calls
         required)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir)
          (with-current-buffer root
            (setq-local mevedel--session session)
            (setq-local buffer-file-name
                        (file-name-concat tempdir
                                          "segment-0001.chat.org")))
          (with-current-buffer agent
            (setq-local mevedel--session session)
            (setq-local mevedel--agent-invocation
                        (mevedel-agent-invocation--create)))
          (mevedel-session-control-transfer-register-root-buffer
           session root)
          (cl-letf (((symbol-function 'mevedel-session-artifacts-save)
                     (lambda (seen-session seen-buffer settled force)
                       (setq calls
                             (list seen-session seen-buffer settled force)
                             required
                             mevedel-session-artifacts-require-agent-commit-p)
                       t)))
            (should
             (mevedel-session-persistence-save-agent-state session)))
          (should (eq session (car calls)))
          (should (eq root (cadr calls)))
          (should-not (nth 2 calls))
          (should (nth 3 calls))
          (should required))
      (when (buffer-live-p root) (kill-buffer root))
      (when (buffer-live-p agent) (kill-buffer agent))
      (delete-directory tempdir t)))

  :doc "keeps a committed full agent save successful when its save hook fails"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-agent-post-save-" t)))
         (workspace
          (test-mevedel-session-persistence--make-workspace tempdir))
         (session (mevedel-session-create "main" workspace))
         (root (generate-new-buffer " *agent-post-save-root*"))
         (segment (file-name-concat tempdir "segment-0001.chat.org"))
         published)
    (unwind-protect
        (progn
          (setf (mevedel-session-save-path session) tempdir
                (mevedel-session-authority-mode session) 'portable)
          (with-current-buffer root
            (setq-local mevedel--session session)
            (setq-local buffer-file-name segment)
            (insert "terminal root state\n")
            (setq-local after-save-hook
                        (list (lambda () (error "Post-save hook failed")))))
          (mevedel-session-control-transfer-register-root-buffer session root)
          (cl-letf
              (((symbol-function
                 'mevedel-session-codec-portable-authority-p)
                (lambda (_session) t))
               ((symbol-function
                 'mevedel-session-artifacts-assert-mutation-authority)
                (lambda (&rest _) t))
               ((symbol-function 'mevedel-session-artifacts-ensure-files)
                (lambda (&rest _) tempdir))
               ((symbol-function
                 'mevedel-session-artifacts--instruction-artifacts)
                (lambda (&rest _) nil))
               ((symbol-function 'mevedel-session-publication-committed-p)
                (lambda (&rest _) nil))
               ((symbol-function 'mevedel-session-publication-prune-committed)
                (lambda (_session artifacts) artifacts))
               ((symbol-function 'mevedel-session-publication-publish)
                (lambda (_session _artifacts &optional require-commit)
                  (should require-commit)
                  (setq published t)))
               ((symbol-function
                 'mevedel-session-persistence-notify-session-event)
                #'ignore))
            (mevedel-test--with-captured-diagnostics nil
              (should
               (mevedel-session-persistence-save-agent-state session))))
          (should published)
          (with-current-buffer root
            (should-not (buffer-modified-p))))
      (when (buffer-live-p root)
        (with-current-buffer root (set-buffer-modified-p nil))
        (kill-buffer root))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))
      (mevedel-workspace-clear-registry))))


(mevedel-deftest mevedel-session-persistence-resume-id
  (:doc "resumes an exact persisted session id and reports unavailable ids")
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-resume-id-" t)))
         (workspace
          (mevedel-workspace--create
           :type 'project :id root :root root :name "resume-id"))
         (session-id "main-2026-08-02T12-00-abcd")
         (session-dir
          (file-name-concat root ".mevedel" "sessions" session-id))
         restored)
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (cl-letf (((symbol-function 'mevedel-session-persistence-restore)
                     (lambda (path &rest _)
                       (setq restored path)
                       'restored-buffer)))
            (should (eq 'restored-buffer
                        (mevedel-session-persistence-resume-id
                         workspace session-id)))
            (should (equal (file-name-as-directory session-dir)
                           restored))
            (should-not
             (mevedel-session-persistence-resume-id workspace "missing"))
            (should-error
             (mevedel-session-persistence-resume-id workspace "../escape"))))
      (delete-directory root t))))

(provide 'test-mevedel-session-persistence)
;;; test-mevedel-session-persistence.el ends here
