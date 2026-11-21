;;; test-niwa-session-persistence.el --- Session lifecycle facade tests -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for `niwa-session-persistence'.

;;; Code:

(require 'niwa-notes)
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

;; `gptel'
(defvar gptel--known-presets)

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-resource)
(require 'niwa-agent-control)
(require 'niwa-agent-persistence)
(require 'niwa-chat)
(require 'niwa-tool-exec-permission)
(require 'niwa-journal-pins)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-utilities)
(require 'niwa-view-composer)
(require 'niwa-execution)
(require 'niwa-journal-capture)
(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))


(niwa-deftest niwa-session-persistence-write-current-buffer-atomically ()
  ,test
  (test)
  :doc "publishes current buffer contents through a same-directory rename"
  (let ((path (make-temp-file "niwa-transcript-atomic-")))
    (unwind-protect
        (with-temp-buffer
          (insert "replacement")
          (niwa-session-persistence-write-current-buffer-atomically path)
          (should
           (equal "replacement"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (when (file-exists-p path) (delete-file path))))
  :doc "preserves the original when publication fails"
  (let* ((root (make-temp-file "niwa-transcript-atomic-" t))
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
             (niwa-session-persistence-write-current-buffer-atomically
              path)))
          (should
           (equal "original"
                  (with-temp-buffer
                    (insert-file-contents path)
                    (buffer-string)))))
      (set-file-modes root #o700)
      (delete-directory root t))))


(niwa-deftest niwa-session-persistence-allocate-session-id ()
  ,test
  (test)
  :doc "retries until the generated id has no session directory"
  (let ((sessions-dir (make-temp-file "niwa-id-allocation-" t))
        (calls 0))
    (unwind-protect
        (progn
          (write-region "occupied\n" nil
                        (file-name-concat sessions-dir "taken") nil 'silent)
          (cl-letf (((symbol-function
                      'niwa-session-generate-id)
                     (lambda ()
                       (if (= (cl-incf calls) 1) "taken" "fresh"))))
            (should (equal
                     "fresh"
                     (niwa-session-persistence-allocate-session-id
                      sessions-dir)))
            (should (= calls 2))))
      (delete-directory sessions-dir t)))
  :doc "retries an identity already held by a live unsaved session"
  (let ((directory (make-temp-file "niwa-live-id-" t))
        (calls 0))
    (unwind-protect
        (with-temp-buffer
          (setq-local niwa--session (niwa-session--create :session-id "live"))
          (cl-letf (((symbol-function 'niwa-session-generate-id)
                     (lambda () (if (= (cl-incf calls) 1) "live" "new"))))
            (should (equal "new" (niwa-session-persistence-allocate-session-id directory)))
            (should (= 2 calls))))
      (delete-directory directory t))))


(niwa-deftest niwa-session-persistence-first-user-message ()
  ,test
  (test)
  :doc "extracts first non-blank line of first user region"
  (with-temp-buffer
    (insert "Refactor the permission chain\n\nMore details follow.")
    (should (equal "Refactor the permission chain"
                   (niwa-session-persistence-first-user-message
                    (current-buffer)))))
  :doc "skips assistant response regions"
  (with-temp-buffer
    (insert (propertize "Sure, I'll do that.\n" 'gptel 'response))
    (insert "What about edge cases?\n")
    (should (equal "What about edge cases?"
                   (niwa-session-persistence-first-user-message
                    (current-buffer)))))
  :doc "returns nil for buffers with no user content"
  (with-temp-buffer
    (insert (propertize "All response.\n" 'gptel 'response))
    (should (null (niwa-session-persistence-first-user-message
                   (current-buffer)))))
  :doc "skips structural deliveries before the real prompt"
  (dolist (type '(MAIL RESULT EXECUTION MONITOR MONITOR_TERMINAL))
    (with-temp-buffer
      (niwa--insert-user-role-block-at-marker
       (niwa-tools--message-delivery-block
        (list :type type :sender "/root/view" :recipient "/root"
              :outcome 'completed :payload "Delivery, not a prompt")))
      (goto-char (point-max))
      (insert "Real prompt\n")
      (should (equal "Real prompt"
                     (niwa-session-persistence-first-user-message
                      (current-buffer))))))
  :doc "delivery-only transcripts have no first prompt"
  (with-temp-buffer
    (niwa--insert-user-role-block-at-marker
     (niwa-tools--message-delivery-block
      '(:type MAIL :sender "/root/view" :recipient "/root" :payload "mail")))
    (should-not (niwa-session-persistence-first-user-message
                 (current-buffer))))
  :doc "literal delivery markup in an authored code fence stays a prompt"
  (with-temp-buffer
    (insert "```xml\n"
            (niwa-tools--message-delivery-block
             '(:type MAIL :sender "/root/view" :recipient "/root" :payload "example"))
            "\n```\n")
    (should (equal "```xml"
                   (niwa-session-persistence-first-user-message
                    (current-buffer)))))
  :doc "truncates long lines"
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (let ((preview (niwa-session-persistence-first-user-message
                    (current-buffer))))
      (should (= 120 (length preview)))
      (should (string-suffix-p "..." preview)))))


(niwa-deftest niwa-session-persistence-root-data-buffer-p ()
  ,test
  (test)
  :doc "recognizes only root session data buffers"
  (let ((session (niwa-session--create :name "root-role"))
        (root (generate-new-buffer " *test-root-role*"))
        (view (generate-new-buffer " *test-view-role*"))
        (agent (generate-new-buffer " *test-agent-role*")))
    (unwind-protect
        (progn
          (with-current-buffer root
            (setq-local niwa--session session))
          (with-current-buffer view
            (setq-local niwa--session session))
          (with-current-buffer view
            (setq-local niwa--data-buffer root))
          (with-current-buffer agent
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation t))
          (niwa-session-set-root-buffer session root)
          (should
           (niwa-session-persistence-root-data-buffer-p root))
          (should-not
           (niwa-session-persistence-root-data-buffer-p view))
          (should-not
           (niwa-session-persistence-root-data-buffer-p agent)))
      (dolist (buffer (list root view agent))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))


(niwa-deftest niwa-session-persistence-authoritative-buffer ()
  ,test
  (test)
  :doc "returns ordinary data buffers unchanged"
  (let ((buf (generate-new-buffer " *test-data*")))
    (unwind-protect
        (with-current-buffer buf
          (org-mode)
          (should (eq buf (niwa-session-persistence-authoritative-buffer
                           buf))))
      (when (buffer-live-p buf) (kill-buffer buf))))
  :doc "routes interactive view buffers to their data buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local niwa--session session)
                  (setq-local niwa--workspace workspace))
                (niwa-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (should (eq data-buf
                              (niwa-session-persistence-authoritative-buffer
                               view-buf)))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "does not treat transcript inspection views as session segment buffers"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-agent-data*"))
               (view-buf (generate-new-buffer " *test-agent-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local niwa--session session)
                  (setq-local niwa--workspace workspace))
                (niwa-view--setup view-buf data-buf
                                     (list :agent-transcript-p t))
                (with-current-buffer view-buf
                  (should-not
                   (niwa-session-persistence-authoritative-buffer
                    view-buf))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (when (buffer-live-p data-buf)
              (with-current-buffer data-buf (set-buffer-modified-p nil))
              (kill-buffer data-buf))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "rejects agent conversation buffers"
  (let ((buf (generate-new-buffer " *test-agent-data*")))
    (unwind-protect
        (with-current-buffer buf
          (setq-local niwa--agent-invocation t)
          (should-not
           (niwa-session-persistence-authoritative-buffer buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))


(niwa-deftest niwa--instruction-workspace-state ()
  ,test
  (test)
  :doc "keeps instruction alists isolated by workspace"
  (let* ((root-a (file-name-as-directory
                  (make-temp-file "niwa-test-ws-a-" t)))
         (root-b (file-name-as-directory
                  (make-temp-file "niwa-test-ws-b-" t)))
         (file-a (file-name-concat root-a "a.el"))
         (file-b (file-name-concat root-b "b.el"))
         (buf-a nil)
         (buf-b nil))
    (unwind-protect
        (progn
          (test-niwa-session-persistence--reset-instructions)
          (niwa-workspace-clear-registry)
          (write-region "(message \"a\")\n" nil file-a nil 'silent)
          (write-region "(message \"b\")\n" nil file-b nil 'silent)
          (let ((ws-a (niwa-workspace-get-or-create
                       'project "a" root-a "a"))
                (ws-b (niwa-workspace-get-or-create
                       'project "b" root-b "b")))
            (setq buf-a (find-file-noselect file-a))
            (setq buf-b (find-file-noselect file-b))
            (with-current-buffer buf-a
              (setq-local niwa--workspace ws-a)
              (niwa-instruction-test--create-note-in buf-a (point-min) (point-max)))
            (with-current-buffer buf-b
              (setq-local niwa--workspace ws-b)
              (niwa-instruction-test--create-note-in buf-b (point-min) (point-max)))
            (niwa--instruction-activate-workspace ws-a)
            (should (= 1 (length (alist-get buf-a (niwa--instruction-alist)))))
            (should-not (assoc buf-b (niwa--instruction-alist)))
            (niwa--instruction-activate-workspace ws-b)
            (should (= 1 (length (alist-get buf-b (niwa--instruction-alist)))))
            (should-not (assoc buf-a (niwa--instruction-alist)))))
      (when (buffer-live-p buf-a)
        (with-current-buffer buf-a (set-buffer-modified-p nil))
        (kill-buffer buf-a))
      (when (buffer-live-p buf-b)
        (with-current-buffer buf-b (set-buffer-modified-p nil))
        (kill-buffer buf-b))
      (delete-directory root-a t)
      (delete-directory root-b t)
      (test-niwa-session-persistence--reset-instructions)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa--instruction-operation-state-key ()
  ,test
  (test)
  :doc "prefers a dynamic workspace override over the buffer workspace"
  (let ((workspace (niwa-workspace--create
                    :type 'project :id "buffer" :root "/tmp/buffer/")))
    (with-temp-buffer
      (setq-local niwa--workspace workspace)
      (let ((niwa--instruction-state-key-override
             '(project . "explicit")))
        (should (equal '(project . "explicit")
                       (niwa--instruction-operation-state-key)))))))


(niwa-deftest niwa-session-persistence--allow-emacs-exit-p (:quiet t)
  ,test
  (test)
  :doc "pending publication vetoes exit until retry or explicit abandonment"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/tmp/pending-exit/"
                     :root "/tmp/pending-exit/" :name "pending-exit"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-pending-exit*"))
         (kill-emacs-query-functions
          '(niwa-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (setf (niwa-session-pending-publication session)
                '(:reason "target unavailable"))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (setf (niwa-session-pending-publication session) nil)
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "unsettled remote mutation vetoes exit until acknowledgement"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/tmp/unsettled-exit/"
                     :root "/tmp/unsettled-exit/"
                     :name "unsettled-exit"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-unsettled-exit*"))
         (kill-emacs-query-functions
          '(niwa-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (setf (niwa-session-lease session)
                '(:state owned :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'niwa-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should-not
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions)))
          (setf (niwa-session-lease session)
                '(:state owned :unsettled-mutation nil))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer))))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))))

  :doc "a foreign read-only inspector can exit with a durable mutation latch"
  (let* ((workspace (niwa-workspace--create
                     :type 'project :id "/tmp/foreign-exit/"
                     :root "/tmp/foreign-exit/"
                     :name "foreign-exit"))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *test-foreign-exit*"))
         (kill-emacs-query-functions
          '(niwa-session-persistence--allow-emacs-exit-p)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (setq-local niwa-session--read-only-mode t)
            (setq buffer-read-only t))
          (setf (niwa-session-lease session)
                '(:state foreign :unsettled-mutation t))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function
                      'niwa-execution-unsettled-mutation-p)
                     (lambda (_session) t)))
            (should
             (run-hook-with-args-until-failure
              'kill-emacs-query-functions))
            (should
             (plist-get (niwa-session-lease session)
                        :unsettled-mutation))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq buffer-read-only nil))
        (kill-buffer buffer))))

  :doc "an unresolved publication job vetoes exit, a drainable one does not"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exit-job-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *test-exit-job*"))
         (kill-emacs-query-functions
          '(niwa-session-persistence--allow-emacs-exit-p))
         drained during message)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local niwa--session session))
          (setf (niwa-session-publication-preparations session)
                (list (cons #'ignore nil)))
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list buffer)))
                    ((symbol-function 'run-at-time) #'ignore)
                    ((symbol-function 'niwa-session-publication-barrier)
                     (lambda (s &optional _token)
                       (push s drained)
                       (push (niwa-session-publication-stopping-p s) during)
                       (setf (niwa-session-publication-preparations s) nil))))
            ;; Admissions and wakes stop while draining, then reopen: another
            ;; query may veto the exit.
            (should (run-hook-with-args-until-failure
                     'kill-emacs-query-functions))
            (should (equal (list session) drained))
            (should (equal '(t) during))
            (should-not (niwa-session-publication-stopping-p session))
            (setf (niwa-session-publication-preparations session)
                  (list (cons #'ignore nil)))
            (cl-letf (((symbol-function 'niwa-session-publication-barrier)
                       (lambda (_ &optional _token)
                         (user-error "Outcome is unresolved"))))
              (niwa-test--with-captured-diagnostics message
                (should-not (run-hook-with-args-until-failure
                             'kill-emacs-query-functions)))
              (should (string-search "unresolved" message)))
            (should-not (niwa-session-publication-stopping-p session))))
      (setf (niwa-session-publication-preparations session) nil)
      (kill-buffer buffer)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(defun test-niwa-session-persistence--gate-session (root)
  "Return a portable session at ROOT with its save path set."
  (let ((session (niwa-session-create
                  "main"
                  (test-niwa-session-persistence--make-workspace root))))
    (setf (niwa-session-save-path session) root)
    session))

(niwa-deftest niwa-session-persistence-autosave-buffer/gate (:quiet t)
  ,test
  (test)
  :doc "an owned gate defers the save before capture, and failures mark the buffer"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-auto-gate-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *auto-gate*"))
         (saves 0) message)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-publication--schedule)
                   #'ignore)
                  ((symbol-function 'niwa-session-artifacts-save)
                   (lambda (&rest _) (cl-incf saves) root)))
          (setf (niwa-session-root-buffer session) buffer)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation nil)
            (insert "text"))
          (setf (niwa-session-publication-preparations session)
                (list (cons #'ignore nil)))
          (should-not (niwa-session-persistence-autosave-buffer buffer))
          (should (= 0 saves))
          (should-not (buffer-local-value 'niwa-session--save-failed buffer))
          (let ((entry (cadr (niwa-session-publication-preparations
                              session))))
            (should entry)
            (niwa-test--with-captured-diagnostics message
              (funcall (cdr entry) '(error "Boom")))
            (should (string-search "Boom" message))
            (should (buffer-local-value 'niwa-session--save-failed buffer))
            ;; The queued preparation proceeds once the gate runner owns it.
            (let ((token (list session)))
              (setf (niwa-session-publication-preparation-token session)
                    token)
              (niwa-test--with-captured-diagnostics message
                (should (equal root (funcall (car entry) token))))
              (should (equal "" message))
              (setf (niwa-session-publication-preparation-token session)
                    nil))
            (should (= 1 saves))
            (should-not (buffer-local-value 'niwa-session--save-failed
                                            buffer))))
      (setf (niwa-session-publication-preparations session) nil
            (niwa-session-root-buffer session) nil)
      (kill-buffer buffer)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-session-persistence--drain-for-close (:quiet t)
  ,test
  (test)
  :doc "gate tokens reach every nested save entry"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-token-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *token*"))
         (token (list session))
         seen)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-session-artifacts-artifact-present-p)
                   (lambda (&rest _) t))
                  ((symbol-function
                    'niwa-session-artifacts-publish-sidecar-state)
                   (lambda (_s _b &optional tok) (push (cons 'sidecar tok) seen) t))
                  ((symbol-function 'niwa-session-artifacts-save)
                   (lambda (_s _b &optional _settled _force tok)
                     (push (cons 'save tok) seen) root)))
          (setf (niwa-session-root-buffer session) buffer)
          (should (niwa-session-persistence-save-agent-state session token))
          (setf (niwa-session-save-path session) nil)
          (should (niwa-session-persistence-save-agent-state session token))
          (should (equal `((save . ,token) (sidecar . ,token)) seen)))
      (setf (niwa-session-root-buffer session) nil)
      (kill-buffer buffer)
      (niwa-workspace-clear-registry)
      (delete-directory root t)))
  :doc "a root buffer stops admissions, other buffers only wait, ungated sessions skip"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-drain-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (root-buffer (generate-new-buffer " *drain-root*"))
         (other (generate-new-buffer " *drain-other*"))
         calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-publication-stopping)
                   (lambda (_) (push 'stopping calls)))
                  ((symbol-function 'niwa-session-publication-barrier)
                   (lambda (_ &optional _token) (push 'barrier calls))))
          (setf (niwa-session-root-buffer session) root-buffer)
          (should (niwa-session-persistence--drain-for-close
                   session root-buffer))
          (should (niwa-session-persistence--drain-for-close session other))
          (setf (niwa-session-save-path session) nil)
          (should (niwa-session-persistence--drain-for-close
                   session root-buffer))
          (should (equal '(barrier stopping) calls)))
      (setf (niwa-session-root-buffer session) nil)
      (mapc #'kill-buffer (list root-buffer other))
      (niwa-workspace-clear-registry)
      (delete-directory root t)))
  :doc "an unresolved job warns and returns nil"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-drain-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *drain-fail*"))
         message)
    (unwind-protect
        (progn
          (setf (niwa-session-root-buffer session) buffer
                (niwa-session-publication-job session)
                (niwa-session-publication--job-create
                 :session session :phase 'unresolved))
          (niwa-test--with-captured-diagnostics message
            (should-not (niwa-session-persistence--drain-for-close
                         session buffer)))
          (should (string-search "unresolved" message))
          ;; Recovery must be able to admit work again.
          (should-not (niwa-session-publication-stopping-p session)))
      (setf (niwa-session-publication-job session) nil
            (niwa-session-root-buffer session) nil)
      (kill-buffer buffer)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-session-persistence-allow-buffer-close-p (:quiet t)
  ,test
  (test)
  :doc "closing the root buffer drains and vetoes only an unresolved job"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-close-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *close-root*"))
         (other (generate-new-buffer " *close-other*"))
         reoffered message)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-agent-control-reoffer-mail)
                   (lambda (s) (push s reoffered))))
          (setf (niwa-session-root-buffer session) buffer)
          (with-current-buffer other (setq-local niwa--session session))
          (with-current-buffer buffer
            (setq-local niwa--session session)
            ;; Nothing was suppressed: nothing is re-armed.
            (should (niwa-session-persistence-allow-buffer-close-p))
            (should-not reoffered)
            ;; A notification actually suppressed during the drain.
            (setf (niwa-session-mail-notifications session) (list 'offer))
            (should (niwa-session-persistence-allow-buffer-close-p))
            ;; Another query may veto: admissions are not left stopped.
            (should-not (niwa-session-publication-stopping-p session))
            (should (equal (list session) reoffered))
            ;; A session that was already stopping re-arms nothing.
            (setf (niwa-session-publication-stopping-p session) t)
            (should (niwa-session-persistence-allow-buffer-close-p))
            (should (= 1 (length reoffered)))
            (setf (niwa-session-publication-stopping-p session) nil)
            (setf (niwa-session-mail-notifications session) nil)
            (setf (niwa-session-publication-job session)
                  (niwa-session-publication--job-create
                   :session session :phase 'unresolved))
            (niwa-test--with-captured-diagnostics message
              (should-not (niwa-session-persistence-allow-buffer-close-p)))
            (should (string-search "publication-abandon" message))
            ;; A definitive failure leaves recovery without a running job.
            (setf (niwa-session-publication-job session) nil
                  (niwa-session-pending-publication session) '(:error "Failed checkpoint"))
            (niwa-test--with-captured-diagnostics message
              (should-not (niwa-session-persistence-allow-buffer-close-p)))
            (should (string-search "retry or abandon" message)))
          (should-not (niwa-session-publication-stopping-p session))
          ;; A non-root buffer never decides the session's fate.
          (with-current-buffer other
            (should (niwa-session-persistence-allow-buffer-close-p))))
      (setf (niwa-session-pending-publication session) nil
            (niwa-session-publication-job session) nil
            (niwa-session-root-buffer session) nil)
      (mapc #'kill-buffer (list buffer other))
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-session-persistence--stop-publications (:quiet t)
  ,test
  (test)
  :doc "drains every gated session and reports only the unresolved ones"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-stop-" t)))
         (good (test-niwa-session-persistence--gate-session root))
         (bad (test-niwa-session-persistence--gate-session root))
         (ungated (niwa-session-create
                   "main" (test-niwa-session-persistence--make-workspace
                           root)))
         (buffers (mapcar (lambda (s)
                            (with-current-buffer (generate-new-buffer " *stop*")
                              (setq-local niwa--session s)
                              (current-buffer)))
                          (list good bad ungated)))
         stopped message blocked)
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda (&optional _frame) buffers))
                  ((symbol-function 'niwa-session-publication-stopping)
                   (lambda (s)
                     (push s stopped)
                     (when (eq s bad) (user-error "Outcome is unresolved")))))
          (niwa-test--with-captured-diagnostics message
            (setq blocked (niwa-session-persistence--stop-publications)))
          (should (equal (list bad) blocked))
          (should (memq good stopped))
          (should (memq bad stopped))
          (should-not (memq ungated stopped))
          (should (string-search "unresolved" message)))
      (mapc #'kill-buffer buffers)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))

(niwa-deftest niwa-session-persistence-release-on-kill/gate (:quiet t)
  ,test
  (test)
  :doc "an unresolved job keeps the session unsaved and its lease unreleased"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-kill-" t)))
         (session (test-niwa-session-persistence--gate-session root))
         (buffer (generate-new-buffer " *kill-gate*"))
         saved released message)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-session-persistence-autosave-buffer)
                   (lambda (_) (setq saved t)))
                  ((symbol-function 'niwa-session-persistence-lock-release)
                   (lambda (&rest _) (setq released t)))
                  ((symbol-function 'niwa-journal-capture-seal-and-schedule)
                   #'ignore))
          (setf (niwa-session-root-buffer session) buffer)
          (with-current-buffer buffer
            (setq-local niwa--session session)
            (niwa-test--with-captured-diagnostics message
              (setf (niwa-session-publication-job session)
                    (niwa-session-publication--job-create
                     :session session :phase 'unresolved))
              (niwa-session-persistence-release-on-kill))
            (should-not (or saved released))
            (should (string-search "unresolved" message))
            (setf (niwa-session-publication-job session) nil
                  (niwa-session-pending-publication session) '(:error "Failed checkpoint"))
            (niwa-test--with-captured-diagnostics message
              (niwa-session-persistence-release-on-kill))
            (should-not (or saved released))
            (should (string-search "retry or abandon" message))
            (setf (niwa-session-pending-publication session) nil)
            (niwa-session-persistence-release-on-kill)
            (should (and saved released))))
      (setf (niwa-session-pending-publication session) nil
            (niwa-session-publication-job session) nil
            (niwa-session-root-buffer session) nil)
      (kill-buffer buffer)
      (niwa-workspace-clear-registry)
      (delete-directory root t))))



(niwa-deftest niwa-session-persistence--kill-emacs-hook
    (:quiet t
     ;; Exit walks every registered workspace, including roots discovered by
     ;; buffer setup.  Never let a scratch buffer discover the checkout here.
     :vars* ((default-directory
              (file-name-as-directory (make-temp-file "niwa-exit-cwd-" t))))
     :after-each ((delete-directory default-directory t)))
  ,test
  (test)
  :doc "only an interactive exit sweeps registered workspaces for expired sessions"
  (let ((workspace (niwa-workspace--create
                    :type 'project :id default-directory :root default-directory :name "exit"))
        (niwa-workspace--registry (make-hash-table :test #'equal))
        swept)
    (puthash (cons 'project default-directory) workspace niwa-workspace--registry)
    (cl-letf (((symbol-function 'niwa-session-persistence-cleanup-expired)
               (lambda (ws &rest _) (push ws swept) 0)))
      (let ((noninteractive t)) (niwa-session-persistence--kill-emacs-hook))
      (should-not swept)
      (let ((noninteractive nil)) (niwa-session-persistence--kill-emacs-hook))
      (should (equal (list workspace) swept))))
  :doc "the exit sweep skips a remote root without a live connection"
  (let* ((remote (niwa-workspace--create
                  :type 'project :id "/ssh:sleeping-host:/p/"
                  :root "/ssh:sleeping-host:/p/" :name "remote"))
         (local (niwa-workspace--create
                 :type 'project :id default-directory
                 :root default-directory :name "local"))
         (niwa-workspace--registry (make-hash-table :test #'equal))
         swept)
    (puthash (cons 'project "r") remote niwa-workspace--registry)
    (puthash (cons 'project "l") local niwa-workspace--registry)
    ;; Any remote file operation here would contact the host.
    (cl-letf (((symbol-function 'niwa-session-persistence-cleanup-expired)
               (lambda (ws &rest _) (push ws swept) 0))
              ((symbol-function 'file-remote-p)
               (lambda (file &optional identification connected)
                 (and (string-prefix-p "/ssh:" file)
                      (if connected nil (or identification "/ssh:sleeping-host:"))))))
      (let ((noninteractive nil)) (niwa-session-persistence--kill-emacs-hook))
      (should (equal (list local) swept))))
  :doc "flushes retained agent text before its idle save timer fires"
  (dolist (type '(file project))
    (let* ((root (make-temp-file "niwa-exit-agent-" t))
           (workspace
            (if (eq type 'file)
		(test-niwa-session-persistence--make-file-workspace root)
              (test-niwa-session-persistence--make-workspace root)))
           (session (niwa-session-create "main" workspace))
           (parent (generate-new-buffer " *exit-root*"))
           (buffer (generate-new-buffer " *exit-agent*"))
           (invocation (niwa-agent-invocation--create
		:agent-id "test-agent" :path "/root/test-agent" :buffer buffer
			:parent-session session :parent-data-buffer parent
			:transcript-relative-path "agents/test.chat.org"))
           (niwa-agent-conversation-save-debounce 1000))
      (unwind-protect
          (progn
            (with-current-buffer parent
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session)
              (setq-local niwa--workspace workspace)
              (insert "Root prompt\n")
              (niwa-session-artifacts-save session parent))
            (with-current-buffer buffer
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session)
              (setq-local niwa--agent-invocation invocation)
              (set-visited-file-name
               (file-name-concat (niwa-session-save-path session)
				 "agents/test.chat.org") t t)
              (insert "Agent prompt\n")
              (should (niwa-agent-conversation-save invocation)))
            ;; Native auto-save must commit already written agent batches,
            ;; even with neither transcript marked modified.
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame) (list parent buffer))))
              (run-hooks 'auto-save-hook)
              (with-timeout (2 (ert-fail "Auto-save remained queued"))
                (while (or (> (hash-table-count niwa-session-persistence--autosaves) 0)
                           (niwa-session-publication-busy-p session))
                  (sleep-for .002))))
            (should (string-search
                     "Agent prompt"
                     (niwa-session-artifacts-read-artifact
                      session "agents/test.chat.org" t)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (propertize "Auto-saved agent answer\n" 'gptel 'response)))
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame) (list buffer))))
              (run-hooks 'auto-save-hook)
              (with-timeout (2 (ert-fail "Auto-save remained queued"))
                (while (or (> (hash-table-count niwa-session-persistence--autosaves) 0)
                           (niwa-session-publication-busy-p session))
                  (sleep-for .002))))
            (should (string-search
                     "Auto-saved agent answer"
                     (niwa-session-artifacts-read-artifact
                      session "agents/test.chat.org" t)))
            (with-current-buffer buffer
              (goto-char (point-max))
              (insert (propertize "Partial agent answer\n" 'gptel 'response))
              (niwa-agent-conversation-save invocation t))
            (cl-letf (((symbol-function 'buffer-list)
                       (lambda (&optional _frame) (list parent buffer))))
              (niwa-session-persistence-autosave)
              (should (gethash buffer niwa-session-persistence--autosaves))
              (niwa-session-persistence--kill-emacs-hook)
              (should (zerop (hash-table-count niwa-session-persistence--autosaves))))
            (should (string-search
                     "Partial agent answer"
                     (niwa-session-artifacts-read-artifact
                      session "agents/test.chat.org" t)))
            (should-not
             (niwa-agent-invocation-transcript-save-timer invocation)))
	(niwa-agent-conversation--cancel-save invocation)
	(with-current-buffer buffer
          (set-buffer-modified-p nil)
          (setq kill-buffer-hook nil))
	(kill-buffer buffer)
	(test-niwa-session-persistence--release-and-kill parent session)
	(delete-directory root t))))
  :doc "force-tears down executions before exit persistence"
  (let ((niwa-workspace--registry nil)
        torn-down)
    (cl-letf (((symbol-function 'niwa-execution-teardown-all)
               (lambda () (setq torn-down t)))
              ((symbol-function 'buffer-list)
               (lambda (&optional _frame) nil)))
      (niwa-session-persistence--kill-emacs-hook))
    (should torn-down))
  :doc "drains publication before teardown and spares an unresolved session its save and release"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-exit-gate-" t)))
         (good (test-niwa-session-persistence--gate-session root))
         (bad (test-niwa-session-persistence--gate-session root))
         (buffers (mapcar (lambda (s)
                            (with-current-buffer (generate-new-buffer " *exit-gate*")
                              (setq-local niwa--session s)
                              (insert "dirty")
                              (current-buffer)))
                          (list good bad)))
         (niwa-workspace--registry nil)
         ;; Earlier tests can leave a debounced agent save pending; the
         ;; exit hook drains those sessions too.
         (niwa-session-persistence--deferred-agent-saves
          (make-hash-table :test #'eq))
         events message quit-inhibited)
    (unwind-protect
        (cl-letf (((symbol-function 'buffer-list)
                   (lambda (&optional _frame) buffers))
                  ((symbol-function 'niwa-session-publication-stopping)
                   (lambda (s)
                     (push 'stopping events)
                     (push inhibit-quit quit-inhibited)
                     (when (eq s bad) (user-error "Outcome is unresolved"))))
                  ((symbol-function 'niwa-execution-teardown-all)
                   (lambda () (push 'teardown events)))
                  ((symbol-function
                    'niwa-session-persistence-authoritative-buffer)
                   #'identity)
                  ((symbol-function 'niwa-session-persistence-autosave-buffer)
                   (lambda (b)
                     (push (cons 'save (buffer-local-value 'niwa--session b))
                           events)))
                  ((symbol-function 'niwa-session-persistence-lock-release)
                   (lambda (&rest _) (push 'release events)))
                  ((symbol-function 'niwa-journal-capture-seal-and-schedule)
                   #'ignore)
                  ((symbol-function
                    'niwa-session-persistence--flush-diagnostic-logs-now)
                   #'ignore))
          (niwa-test--with-captured-diagnostics message
            (niwa-session-persistence--kill-emacs-hook))
          (setq events (nreverse events))
          (should (string-search "unresolved" message))
          (should (equal '(t t) quit-inhibited))
          (should (equal '(stopping stopping teardown)
                         (seq-take events 3)))
          (should (member (cons 'save good) events))
          (should-not (member (cons 'save bad) events))
          (should (= 1 (cl-count 'release events))))
      (mapc (lambda (b) (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))
            buffers)
      (delete-directory root t)))
  :doc "modified view buffers are persisted through data buffers on exit"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (data-buf (generate-new-buffer " *test-data*"))
               (view-buf (generate-new-buffer " *test-view*")))
          (unwind-protect
              (progn
                (with-current-buffer data-buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (setq-local niwa--session session)
                  (setq-local niwa--workspace workspace)
                  (insert "Exit hook data prompt\n")
                  (set-buffer-modified-p nil))
                (niwa-view--setup view-buf data-buf)
                (with-current-buffer view-buf
                  (let ((inhibit-read-only t)
                        (inhibit-modification-hooks t))
                    (goto-char (niwa-view-transcript-tail))
                    (insert "Exit hook view chrome\n"))
                  (set-buffer-modified-p t))
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame)
                             (list view-buf data-buf)))
                          ((symbol-function 'read-file-name)
                           (lambda (&rest _)
                             (error "View buffer requested a save filename"))))
                  (niwa-session-persistence--kill-emacs-hook))
                (with-current-buffer view-buf
                  (should-not buffer-file-name)
                  (should-not buffer-file-truename))
                (let ((segment (niwa-session-artifacts-read-artifact
                                session "segment-0001.chat.org" t)))
                  (should (string-match-p "Exit hook data prompt" segment))
                  (should-not (string-match-p "Exit hook view chrome" segment))))
            (when (buffer-live-p view-buf)
              (with-current-buffer view-buf (set-buffer-modified-p nil))
              (kill-buffer view-buf))
            (test-niwa-session-persistence--release-and-kill
             data-buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "cleans inactive expired sessions before releasing live locks"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (live-session (niwa-session-create "live" workspace))
               (expired-session (niwa-session-create "expired" workspace))
               (live-buf (generate-new-buffer " *test-live*"))
               (expired-buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (dolist (pair `((,live-session . ,live-buf)
                                (,expired-session . ,expired-buf)))
                  (with-current-buffer (cdr pair)
                    (org-mode)
                    (setq-local niwa--session (car pair))
                    (setq-local niwa--workspace workspace)
                    (insert "Old session\n")
                    (niwa-session-artifacts-save
                     (car pair) (cdr pair))
                    (set-buffer-modified-p nil)))
                (dolist (session (list live-session expired-session))
                  (test-niwa-session-persistence--expire-session session))
                (test-niwa-session-persistence--release-and-kill
                 expired-buf expired-session)
                (cl-letf (((symbol-function 'buffer-list)
                           (lambda (&optional _frame) (list live-buf))))
                  (test-niwa-session-persistence--call-later
                   (lambda ()
                     (let ((noninteractive nil))
                       (niwa-session-persistence--kill-emacs-hook)))))
                (should-not
                 (file-directory-p
                  (niwa-session-save-path expired-session)))
                (should
                 (file-directory-p (niwa-session-save-path live-session)))
                (should (test-niwa-session-persistence--lease-released-p
                         (niwa-session-save-path live-session))))
            (test-niwa-session-persistence--release-and-kill
             live-buf live-session)
            (test-niwa-session-persistence--release-and-kill
             expired-buf expired-session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "honors a cleanup run already throttled for the workspace"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (niwa-session-create "expired" workspace))
               (buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old session\n")
                  (niwa-session-artifacts-save session buf))
                (let ((save-path (niwa-session-save-path session)))
                  (test-niwa-session-persistence--expire-session session)
                  (niwa-session-persistence-lock-release save-path session)
                  (puthash
                   (cons (niwa-workspace-type workspace)
                         (niwa-workspace-id workspace))
                   (float-time) niwa-session-persistence--cleanup-throttle)
                  (cl-letf (((symbol-function 'buffer-list)
                             (lambda (&optional _frame) nil)))
                    (let ((noninteractive nil)) (niwa-session-persistence--kill-emacs-hook)))
                  (should (file-directory-p save-path))))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "failed exit saves remain protected until cleanup finishes"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (niwa-session-create "live" workspace))
               (buf (generate-new-buffer " *test-live*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local niwa--session session)
                  (insert "Unsaved exit change\n")
                  (niwa-session-artifacts-save session buf))
                (let* ((save-path (niwa-session-save-path session))
                       (publication-directory
                        (file-name-concat save-path ".publications")))
                  (test-niwa-session-persistence--expire-session session)
                  (with-current-buffer buf
                    (set-buffer-modified-p t))
                  (set-file-modes publication-directory #o500)
                  (unwind-protect
                      (cl-letf (((symbol-function 'buffer-list)
                                 (lambda (&optional _frame) (list buf))))
                        (let ((noninteractive nil))
                          (niwa-session-persistence--kill-emacs-hook)))
                    (set-file-modes publication-directory #o700))
                  (should (file-directory-p save-path))
                  (with-current-buffer buf
                    (should niwa-session--save-failed))
                  (should (test-niwa-session-persistence--lease-released-p
                           save-path))))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "cleanup errors do not block sibling workspaces or lock release"
  (let* ((niwa-session-max-age-days 7)
         (niwa-session-keep-recent-count nil)
         (niwa-session-persistence--cleanup-throttle
          (make-hash-table :test #'equal))
         (niwa-workspace--registry (make-hash-table :test #'equal))
         (bad-root (file-name-as-directory
                    (make-temp-file "niwa-test-bad-ws-" t)))
         (good-root (file-name-as-directory
                     (make-temp-file "niwa-test-good-ws-" t)))
         (bad-workspace
          (niwa-workspace-get-or-create
           'file "bad" bad-root "bad"))
         (good-workspace
          (niwa-workspace-get-or-create
           'file "good" good-root "good"))
         (bad-sessions
          (niwa-session-artifacts-sessions-dir bad-workspace))
         (live-session (niwa-session-create "live" good-workspace))
         (expired-session (niwa-session-create "expired" good-workspace))
         (live-buf (generate-new-buffer " *test-live*"))
         (expired-buf (generate-new-buffer " *test-expired*")))
    (unwind-protect
        (progn
          (dolist (pair `((,live-session . ,live-buf)
                          (,expired-session . ,expired-buf)))
            (with-current-buffer (cdr pair)
              (org-mode)
              (setq-local niwa--session (car pair))
              (insert "Session\n")
              (niwa-session-artifacts-save (car pair) (cdr pair))
              (set-buffer-modified-p nil)))
          (let ((save-path (niwa-session-save-path expired-session)))
            (test-niwa-session-persistence--expire-session expired-session)
            (test-niwa-session-persistence--release-and-kill
             expired-buf expired-session))
          (make-directory bad-sessions t)
          (set-file-modes bad-sessions 0)
          (cl-letf (((symbol-function 'buffer-list)
                     (lambda (&optional _frame) (list live-buf))))
            (test-niwa-session-persistence--call-later
             (lambda ()
               (let ((noninteractive nil))
                 (niwa-session-persistence--kill-emacs-hook)))))
          (should-not
           (file-directory-p (niwa-session-save-path expired-session)))
          (should (test-niwa-session-persistence--lease-released-p
                   (niwa-session-save-path live-session))))
      (when (file-exists-p bad-sessions)
        (set-file-modes bad-sessions #o700))
      (test-niwa-session-persistence--release-and-kill
       live-buf live-session)
      (test-niwa-session-persistence--release-and-kill
       expired-buf expired-session)
      (delete-directory bad-root t)
      (delete-directory good-root t))))


;;
;;; Phase 5: read path

(niwa-deftest niwa-session-persistence-load-sidecar ()
  ,test
  (test)
  :doc "reads a current-version sidecar"
  (let ((tmp (make-temp-file "niwa-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (niwa-session-codec-write
           tmp (test-niwa-session-persistence--complete-sidecar
                '(:session-name "x")))
          (let ((plist (niwa-session-persistence-load-sidecar tmp)))
            (should (equal niwa-session-codec-format-version
                           (plist-get plist :version)))
            (should (equal "x" (plist-get plist :session-name)))))
      (when (file-exists-p tmp) (delete-file tmp))))

  :doc "rejects an unsupported sidecar version"
  (let ((tmp (make-temp-file "niwa-meta-test-" nil ".el")))
    (unwind-protect
        (progn
          (niwa-session-codec-write
           tmp '(:version "v0.0.0" :session-name "x"))
          (should-error
           (niwa-session-persistence-load-sidecar tmp)
           :type 'error))
      (when (file-exists-p tmp) (delete-file tmp)))))


(niwa-deftest niwa-session-persistence--cold-workspace ()
  ,test
  (test)
  :doc "derives a target-side cold workspace from the persisted native root"
  (let ((workspace
         (niwa-session-persistence--cold-workspace
          "/home/user/project/.niwa/sessions/main-1/"
          '(:workspace (:type project
                        :target-native-root "/home/user/project/"
                        :name "project")))))
    (should (equal "/home/user/project/"
                   (niwa-workspace-root workspace))))
  :doc "qualifies the persisted native root with a remote client's own prefix"
  (let ((workspace
         (niwa-session-persistence--cold-workspace
          "/ssh:user@host:/home/user/project/.niwa/sessions/main-1/"
          '(:workspace (:type project
                        :target-native-root "/home/user/project/"
                        :name "project")))))
    (should (equal "/ssh:user@host:/home/user/project/"
                   (niwa-workspace-root workspace)))))


(niwa-deftest niwa-session-persistence-restore/custom-workspace (:quiet t)
  ,test
  (test)
  :doc "saves and cold-restores a custom workspace through the portable lease"
  (let* ((root (make-temp-file "niwa-custom-workspace-" t))
         (niwa-workspace-types-alist
          (cons '(custom . (:get-root identity :get-name file-name-nondirectory))
                niwa-workspace-types-alist))
         (niwa-workspace-functions
          (list (lambda () (cons 'custom root))))
         (workspace (with-temp-buffer (niwa-workspace)))
         (session (niwa-session-create "custom" workspace))
         (buffer (generate-new-buffer " *custom-workspace-save*"))
         restored)
    (unwind-protect
        (progn
          (should (eq 'custom (niwa-workspace-type workspace)))
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "Custom workspace transcript\n")
            (should (niwa-session-artifacts-save session buffer)))
          (should (eq 'portable (niwa-session-authority-mode session)))
          (should (niwa-session-durability-lease-owned-p session))
          (let ((directory (niwa-session-save-path session)))
            (should (plist-get (niwa-session-publication-read directory) :head))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (niwa-workspace-clear-registry)
            (setq restored (niwa-session-persistence-restore directory))
            (with-current-buffer restored
              (should (string-match-p "Custom workspace transcript" (buffer-string)))
              (should (eq 'custom
                          (niwa-workspace-type
                           (niwa-session-workspace niwa--session))))
              (should (equal (file-name-as-directory root)
                             (niwa-workspace-root
                              (niwa-session-workspace niwa--session))))
              (should (equal (niwa-session-session-id session)
                             (niwa-session-session-id niwa--session)))
              (should (eq 'portable (niwa-session-authority-mode niwa--session)))
              (should (niwa-session-durability-lease-owned-p niwa--session)))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (test-niwa-session-persistence--release-and-kill
       restored (and (buffer-live-p restored)
                     (buffer-local-value 'niwa--session restored)))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-restore (:quiet t)
  ,test
  (test)
  :doc "restores a lost monitor as non-waking terminal mail, durable across another resume"
  (let* ((root (make-temp-file "niwa-restore-monitor-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "main" workspace))
         (buffer (generate-new-buffer " *restore-monitor*"))
         restored)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (org-mode)
            (insert "Watch CI\n"
                    (test-niwa-session-persistence--execution-tool-block
                     "monitor-tool"
                     '(:kind monitor :execution-id "exec-monitor" :description "CI watch"
                       :state running :status success :live-execution-p t))
                    (test-niwa-session-persistence--execution-tool-block
                     "bash-tool"
                     '(:kind bash :execution-id "exec-bash"
                       :state running :status success :live-execution-p t)))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (niwa-session-artifacts-save session buffer))
          (test-niwa-session-persistence--release-and-kill buffer session)
          (dotimes (_ 2)
            (setq restored (niwa-session-persistence-restore
                            (niwa-session-save-path session) nil nil workspace))
            (with-current-buffer restored
              (let ((mail (niwa-agent-control-context-mailbox niwa--session)))
                (should (= 1 (length mail)))
                (should (eq 'MONITOR_TERMINAL (plist-get (car mail) :type)))
                (should (equal "exec-monitor" (plist-get (car mail) :execution-id)))
                (should (equal "CI watch" (plist-get (car mail) :description)))
                (should (string-search "CI watch" (niwa-tools--message-delivery-block (car mail))))
                (should-not (niwa-agent-control-wake-pending-p niwa--session))))
            (test-niwa-session-persistence--release-and-kill
             restored (buffer-local-value 'niwa--session restored))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (when (buffer-live-p restored)
        (test-niwa-session-persistence--release-and-kill
         restored (buffer-local-value 'niwa--session restored)))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "repair publication keeps authored previews around mailbox deliveries"
  (let* ((tempdir (make-temp-file "niwa-preview-restore-" t))
         (workspace (test-niwa-session-persistence--make-workspace tempdir)))
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *restore-delivery-preview*"))
           restored)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (dolist (type '(MAIL RESULT EXECUTION))
                (niwa--insert-user-role-block-at-marker
                 (niwa-tools--message-delivery-block
                  (list :type type :sender "/root/view" :recipient "/root"
                        :outcome 'completed :payload "First delivery"))))
              (goto-char (point-max))
              (insert "First authored prompt\n"
                      (propertize "Answer\n" 'gptel 'response)
                      "Latest authored prompt\n")
              ;; A real stale execution makes public restore publish repairs.
              (insert (test-niwa-session-persistence--execution-tool-block
                       "preview-stale"
                       '(:execution-id "exec-preview-stale" :state running
                         :status success :live-execution-p t)))
              (niwa--insert-user-role-block-at-marker
               (niwa-tools--message-delivery-block
                '(:type MAIL :sender "/root/view" :recipient "/root"
                  :payload "Latest delivery")))
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (niwa-session-artifacts-save session buffer)
              ;; Do not let the cached first preview hide restore's derivation.
              ;; A sidecar without that optional preview must be rebuilt from
              ;; the persisted transcript by the real repair publication.
              (niwa-session-publication-publish
               session
               (list
                (list :path (niwa-session-artifacts-sidecar-path
                             (niwa-session-save-path session))
                      :content (niwa-session-artifacts-printed-value
                                (plist-put
                                 (niwa-session-artifacts-build-sidecar session buffer)
                                 :first-user-message nil))
                      :commit-marker t))))
            (let* ((directory (niwa-session-save-path session))
                   (head (plist-get (niwa-session-publication session) :head)))
              (test-niwa-session-persistence--release-and-kill buffer session)
              (setq restored
                    (niwa-session-persistence-restore directory nil nil workspace))
              (with-current-buffer restored
                (should-not (equal head (plist-get (niwa-session-publication niwa--session) :head)))
                (should (string-match-p ":state lost" (buffer-string)))
                (let ((sidecar
                       (with-temp-buffer
                         (insert (niwa-session-artifacts-read-artifact
                                  (buffer-local-value 'niwa--session restored)
                                  "session.meta.el"))
                         (goto-char (point-min))
                         (read (current-buffer)))))
                  (should (equal "First authored prompt"
                                 (plist-get sidecar :first-user-message)))
                  (should (equal "Latest authored prompt"
                                 (plist-get sidecar :latest-user-message)))
                  (should (= 2 (length (cdr (assoc 1 (plist-get sidecar :prompt-index))))))))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored (and (buffer-live-p restored)
                       (buffer-local-value 'niwa--session restored)))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "restore ignores a disk segment written outside the publication"
  (dolist (make-workspace
           '(test-niwa-session-persistence--make-file-workspace
             test-niwa-session-persistence--make-workspace))
    (let* ((root (make-temp-file "niwa-restore-counter-" t))
           (workspace (funcall make-workspace root))
           (session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *restore-counter*"))
           restored)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (insert "Saved transcript\n")
              (niwa-session-artifacts-save session buffer))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (let* ((directory (niwa-session-save-path session))
                   (predecessor (niwa-session-artifacts-segment-path directory 1)))
              (write-region "Newer disk transcript\n" nil
                            (niwa-session-artifacts-segment-path directory 2)
                            nil 'silent)
              (setq restored (niwa-session-persistence-restore directory nil nil workspace))
              (with-current-buffer restored
                (should (= 1 (niwa-session-current-segment niwa--session)))
                (should (string-match-p "Saved transcript" (buffer-string))))
              (should-not (string-match-p
                           "NIWA_SEGMENT_FINALIZED_AT"
                           (niwa-session-artifacts-read-artifact
                            session (file-name-nondirectory predecessor) t)))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored (and (buffer-live-p restored)
                       (buffer-local-value 'niwa--session restored)))
        (delete-directory root t)
        (niwa-workspace-clear-registry))))
  :doc "settles a durable ToolCall checkpoint as interrupted, no resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let (buffer restored session session-dir)
      (unwind-protect
          (progn
            (setq session (niwa-session-create "main" workspace)
                  buffer (generate-new-buffer "*test-ptc-recovery*"))
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "Prompt before interrupted ToolCall\n")
              (should
               (niwa-ptc-checkpoint-start
                session buffer "ptc-recovery"
                "(Read :path \"README.md\")"))
              (should
               (niwa-ptc-checkpoint-update
                session buffer "ptc-recovery"
                '(:render-data
                  (:kind ptc :outcome running
                   :calls ((:id "ptc-recovery/1" :tool "Read"
                            :status running)))))))
            (setq session-dir (niwa-session-save-path session))
            (test-niwa-session-persistence--release-and-kill buffer session)
            (setq buffer nil
                  restored
                  (niwa-session-persistence-restore session-dir))
            (with-current-buffer restored
              (should (string-match-p
                       "script interrupted by Emacs restart" (buffer-string)))
              (should
               (niwa-tool-render-data-segment-bounds "ptc-recovery"))
              (should-not (niwa-session-ptc-checkpoints niwa--session)))
            (should-not
             (plist-get
              (niwa-session-persistence-load-sidecar
               (niwa-session-artifacts-sidecar-path session-dir))
              :ptc-checkpoints)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (niwa-workspace-clear-registry))))
  :doc "restores stale rows as lost but supersedes rows with newer facts"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "First user prompt\n")
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "stale-call"
                    '(:execution-id "exec-stale" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state running
                      :status success :live-execution-p t)))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (niwa-session-artifacts-save session buf)
                  (niwa-session-artifacts-rotate-segment
                   session buf "Earlier conversation")
                  (insert "Second user prompt\n")
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "current-call"
                    '(:execution-id "exec-current" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state completed
                      :status success :live-execution-p nil)))
                  (insert
                   (niwa--format-hook-audit-record
                    '(:type execution-completion
                      :tool-use-id "archived-call"
                      :render-data (:execution-id "exec-stale"
                                    :state completed
                                    :live-execution-p nil))))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                ;; Release the lock + kill the buffer (the test buffer didn't
                ;; go through chat-buffer-init-common so the kill-hook isn't
                ;; installed; we mirror its work manually).
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (should (file-exists-p session-dir))
                (setq restored (niwa-session-persistence-restore
                                session-dir))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (derived-mode-p 'org-mode))
                  (should (bound-and-true-p gptel-mode))
                  (should niwa--session)
                  (should (equal "main"
                                 (niwa-session-name niwa--session)))
                  (should (= 2 (niwa-session-current-segment
                                niwa--session)))
                  (should-not (niwa-session-execution-state
                               niwa--session))
                  (should (string-match-p "Second user prompt"
                                          (buffer-string)))
                  (should (string-match-p ":state lost"
                                          (buffer-string))))
                (with-temp-buffer
                  (insert (niwa-session-artifacts-read-artifact
                           (buffer-local-value 'niwa--session restored)
                           "segment-0001.chat.org" t))
                  (goto-char (point-min))
                  (should (= 2 (how-many ":state archived"))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))
  :doc "read-only inspection trusts published state and skips lifecycle commands"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let (restored)
      (unwind-protect
          (let* ((session (niwa-session-create "main" workspace))
                 (buffer (generate-new-buffer "*test-published-segment*"))
                 session-dir segment-1 segment-2 probed started)
            (with-current-buffer buffer
              (org-mode)
              (insert "Published transcript\n")
              (niwa-session-artifacts-save session buffer))
            (setq session-dir (niwa-session-save-path session)
                  segment-1 (niwa-session-artifacts-segment-path
                             session-dir 1)
                  segment-2 (niwa-session-artifacts-segment-path
                             session-dir 2))
            (test-niwa-session-persistence--release-and-kill
             buffer session)
            (write-region "Unpublished transcript\n" nil segment-2 nil 'silent)
            (cl-letf (((symbol-function
                        'niwa-session-persistence-lock-acquire)
                       (lambda (&rest _) nil))
                      ((symbol-function 'niwa--probe-session-target)
                       (lambda (&rest _) (setq probed t)))
                      ((symbol-function 'niwa--run-session-start-hooks)
                       (lambda (&rest _) (setq started t))))
              ;; Read-only restore announces itself; this case asserts the
              ;; durable state that notice echoes.
              (niwa-test--with-captured-messages nil
                (setq restored
                      (niwa-session-persistence-restore session-dir))))
            (with-current-buffer restored
              (should niwa-session--read-only-mode)
              (should (= 1 (niwa-session-current-segment
                            niwa--session)))
              (should (string-match-p "Published transcript"
                                      (buffer-string)))
              (should-not (memq #'niwa--run-session-end-hooks
                                kill-buffer-hook)))
            (should probed)
            (should-not started)
            (should-not
             (string-match-p "NIWA_SEGMENT_FINALIZED_AT"
                             (niwa-session-artifacts-read-artifact
                              (buffer-local-value 'niwa--session restored)
                              (file-name-nondirectory segment-1) t)))
            (with-temp-buffer
              (insert-file-contents segment-2)
              (should (equal "Unpublished transcript\n" (buffer-string)))))
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (niwa-workspace-clear-registry))))
  :doc "an actual foreign lease permits inspection without hooks or repair writes"
  (let* ((host "restore-foreign-owner-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-foreign-restore-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b))
         (running
          (niwa-tool-render-data-format
           '(:execution-id "foreign-stale" :state running
             :status success :live-execution-p t)))
         restored)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment-1)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root (concat "* Chat\n" running))
            (let* ((segment-2
                    (niwa-session-artifacts-segment-path
                     session-dir 2))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   probed
                   started)
              (write-region
               "Unpublished transcript\n" nil segment-2 nil 'silent)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target owner))
               t niwa-session-durability--disclosed-targets)
              (let ((niwa-session-durability--client-id owner-id))
                (should
                 (niwa-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let* ((timer
                            (niwa-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (niwa-session-lease-renewal-timer owner) nil)))
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
                      (niwa-session-durability--client-id inspector-id))
                  (unwind-protect
                      (progn
                        (cl-letf
                            (((symbol-function 'niwa--probe-session-target)
                              (lambda (session &optional refresh)
                                (setq probed t)
                                (niwa-execution-target-probe
                                 (niwa-session-execution-target session)
                                 refresh 'off)))
                             ((symbol-function
                               'niwa--run-session-start-hooks)
                              (lambda (&rest _) (setq started t)))
                             ((symbol-function
                               'niwa-session-persistence-reconcile-lost-execution-segments)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore repaired")))
                             ((symbol-function
                               'niwa-session-publication-publish)
                              (lambda (&rest _)
                                (ert-fail "Foreign restore published"))))
                          ;; Read-only restore announces itself; this case asserts the
                          ;; durable state that notice echoes.
                          (niwa-test--with-captured-messages nil
                            (setq restored
                                  (niwa-session-persistence-restore
                                   session-dir nil nil workspace))))
                        (with-current-buffer restored
                          (should niwa-session--read-only-mode)
                          (should
                           (eq 'foreign
                               (plist-get
                                (niwa-session-lease niwa--session)
                                :state)))
                          (should (= 1 (niwa-session-current-segment
                                        niwa--session)))
                          (should
                           (string-match-p ":state running"
                                           (buffer-string)))
                          (should-not
                           (memq #'niwa--run-session-end-hooks
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
                    (test-niwa-session-persistence--release-and-kill
                     restored
                     (and restored
                          (buffer-local-value 'niwa--session restored)))
                    (setq restored nil))))
              (let ((niwa-session-durability--client-id owner-id))
                (niwa-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "foreign inspection ignores a missing fixed segment cache"
  (let* ((host "restore-foreign-missing-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-foreign-missing-" t)))
         (owner-id (make-string 64 ?a))
         (inspector-id (make-string 64 ?b)))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace owner session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (delete-file segment)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target owner))
               t niwa-session-durability--disclosed-targets)
              (let ((niwa-session-durability--client-id owner-id))
                (should
                 (niwa-session-durability-lease-acquire
                  session-dir "*owner*" owner))
                (when-let* ((timer
                            (niwa-session-lease-renewal-timer owner)))
                  (cancel-timer timer)
                  (setf (niwa-session-lease-renewal-timer owner) nil)))
              (let ((before (directory-files-recursively local-root ".*" t))
                    (niwa-session-durability--client-id inspector-id)
                    restored)
                (unwind-protect
                    (progn
                      (cl-letf
                          (((symbol-function 'yes-or-no-p)
                            (lambda (&rest _)
                              (ert-fail "Foreign restore prompted to prune"))))
                        ;; Read-only restore announces itself; this case asserts the
                        ;; durable state that notice echoes.
                        (niwa-test--with-captured-messages nil
                          (setq restored
                                (niwa-session-persistence-restore
                                 session-dir nil nil workspace))))
                      (with-current-buffer restored
                        (should niwa-session--read-only-mode)
                        (should (string-match-p
                                 "Published transcript" (buffer-string))))
                      (should (file-directory-p session-dir))
                      (should
                       (equal before
                              (directory-files-recursively
                               local-root ".*" t))))
                  (test-niwa-session-persistence--release-and-kill
                   restored
                   (and restored
                        (buffer-local-value 'niwa--session restored)))))
              (let ((niwa-session-durability--client-id owner-id))
                (niwa-session-durability-lease-release
                 session-dir owner)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "retries cleanly when the publication head changes during lease acquisition"
  (let* ((host "restore-sidecar-race-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-sidecar-race-" t)))
         restored
         acquired-session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'niwa-session-persistence-lock-acquire))
                   changed)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target fixture-session))
               t niwa-session-durability--disclosed-targets)
              (cl-letf
                  (((symbol-function
                     'niwa-session-persistence-lock-acquire)
                    (lambda (&rest arguments)
                      ;; Another owner commits a new immutable head after our
                      ;; initial capture but before this lease claim.
                      (unless changed
                        (setq changed t)
                        (let ((niwa-session-durability--client-id
                               (make-string 64 ?d)))
                          (unwind-protect
                              (progn
                                (should
                                 (niwa-session-durability-lease-acquire
                                  session-dir "*intervening-owner*"
                                  fixture-session))
                                (let* ((publication
                                        (niwa-session-publication-read
                                         session-dir))
                                       (new-sidecar
                                        (niwa-session-codec-read
                                         (plist-get publication :sidecar))))
                                  (plist-put
                                   new-sidecar :total-turn-count 17)
                                  (niwa-session-publication-publish
                                   fixture-session
                                   (list
                                    (list
                                     :path
                                     (niwa-session-artifacts-sidecar-path
                                      session-dir)
                                     :content
                                     (niwa-session-artifacts-printed-value
                                      new-sidecar)
                                     :commit-marker t)))))
                            (niwa-session-durability-lease-release
                             session-dir fixture-session))))
                      (setq acquired-session (nth 2 arguments))
                      (apply acquire-function arguments)))
                   ((symbol-function 'niwa--probe-session-target)
                    #'ignore)
                   ((symbol-function 'niwa--chat-buffer-init-common)
                    #'ignore)
                   ((symbol-function
                     'niwa-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'niwa-session-artifacts-load-instructions)
                    #'ignore))
                (let ((err
                       (should-error
                        (setq restored
                              (niwa-session-persistence-restore
                               session-dir nil nil workspace))
                        :type 'user-error)))
                  (should
                   (string-match-p
                    "changed while acquiring"
                    (error-message-string err)))))
              (should acquired-session)
              (should-not (niwa-session-lease acquired-session))
              (should-not
               (niwa-session-lease-renewal-timer acquired-session))
              (should
               (eq 'released
                   (plist-get
                    (niwa-session-durability--lease-head
                     (niwa-session-durability--lease-path session-dir))
                    :status)))
              (should
               (= 17
                  (plist-get
                   (niwa-session-codec-read
                    (plist-get
                     (niwa-session-publication-read session-dir)
                     :sidecar))
                   :total-turn-count))))))
      (niwa-test--with-local-shell-tramp (list host)
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "loads immutable sidecar and transcript bytes over poisoned fixed caches"
  (let* ((host "restore-sidecar-cache-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-sidecar-cache-" t)))
         restored)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (artifact-dir (file-name-concat session-dir "artifacts"))
                   (artifact
                    (file-name-concat artifact-dir "nested" "mockup.html"))
                   (stale (file-name-concat artifact-dir "stale.txt")))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target fixture-session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*artifact-publisher*" fixture-session))
              (unwind-protect
                  (niwa-session-publication-publish
                   fixture-session
                   (list
                    (list :path artifact :content "Published artifact\n")
                    (list
                     :path (niwa-session-artifacts-sidecar-path session-dir)
                     :content
                     (niwa-session-artifacts-read-artifact
                      fixture-session "session.meta.el" t)
                     :commit-marker t)))
                (niwa-session-durability-lease-release
                 session-dir fixture-session))
              (write-region "(:poisoned fixed sidecar)" nil
                            (niwa-session-artifacts-sidecar-path
                             session-dir)
                            nil 'silent)
              (write-region "Poisoned fixed transcript\n" nil segment nil 'silent)
              (make-directory (file-name-directory artifact) t)
              (write-region "Poisoned artifact\n" nil artifact nil 'silent)
              (write-region "Stale artifact\n" nil stale nil 'silent)
              (cl-letf
                  (((symbol-function 'niwa--probe-session-target) #'ignore)
                   ((symbol-function 'niwa--chat-buffer-init-common) #'ignore)
                   ((symbol-function 'niwa-agent-persistence-restore-tree)
                    (lambda (&rest _) 0))
                   ((symbol-function
                     'niwa-session-artifacts-load-instructions)
                    #'ignore))
                (setq restored
                      (niwa-session-persistence-restore
                       session-dir nil nil workspace)))
              (with-current-buffer restored
                (should (equal "main" (niwa-session-name niwa--session)))
                (should (string-match-p
                         "Published transcript" (buffer-string)))
                (should-not
                 (string-match-p "Poisoned fixed" (buffer-string)))
                (should (niwa-session-publication niwa--session)))
              ;; An old published artifact entry stays readable by its
              ;; logical path, and Resume no longer rewrites the folder.
              (should
               (equal "Published artifact\n"
                      (niwa-session-artifacts-read-artifact
                       (buffer-local-value 'niwa--session restored)
                       "artifacts/nested/mockup.html" t)))
              (should (file-exists-p stale)))))
      (niwa-test--with-local-shell-tramp (list host)
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "fails closed when an immutable transcript digest no longer matches"
  (let* ((host "restore-digest-corruption")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-restore-digest-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (publication
                    (niwa-session-publication-read session-dir))
                   (entry
                    (cdr (assoc "segment-0001.chat.org"
                                (plist-get publication :artifacts)))))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target fixture-session))
               t niwa-session-durability--disclosed-targets)
              (write-region "corrupt" nil (plist-get entry :published)
                            nil 'silent)
              (let ((err
                     (should-error
                      (niwa-session-persistence-restore
                       session-dir nil nil workspace))))
                (should
                 (string-match-p "failed verification"
                                 (error-message-string err))))
              (should-not
               (niwa-session-persistence-find-live-buffer
                "main-remote-restore" "*unused*"))
              (should
               (eq 'released
                   (plist-get
                    (niwa-session-durability--lease-head
                     (niwa-session-durability--lease-path session-dir))
                    :status))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "releases its lease when restore fails before opening a buffer"
  (let* ((host "restore-pre-buffer-failure-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-pre-buffer-failure-" t)))
         acquired-session
         session-dir)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace fixture-session fixture-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (setq session-dir fixture-dir)
            (let* ((niwa-session-durability--client-id
                    (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (acquire-function
                    (symbol-function
                     'niwa-session-persistence-lock-acquire))
                   artifact-open-attempted-p)
              (unwind-protect
                  (progn
                    (puthash
                     (niwa-execution-target-identity
                      (niwa-session-execution-target fixture-session))
                     t niwa-session-durability--disclosed-targets)
                    (cl-letf
                        (((symbol-function
                           'niwa-session-persistence-lock-acquire)
                          (lambda (&rest arguments)
                            (setq acquired-session (nth 2 arguments))
                            (apply acquire-function arguments)))
                         ((symbol-function
                           'niwa-session-artifacts-find-artifact-noselect)
                          (lambda (&rest _)
                            (setq artifact-open-attempted-p t)
                            (error "Injected pre-buffer restore failure"))))
                      (should-error
                       (niwa-session-persistence-restore
                        session-dir nil nil workspace)
                       :type 'error))
                    (should acquired-session)
                    (should artifact-open-attempted-p)
                    (should-not
                     (niwa-session-persistence-find-live-buffer
                      "main-remote-restore" "*unused*"))
                    (should-not (niwa-session-lease acquired-session))
                    (should-not
                     (niwa-session-lease-renewal-timer acquired-session))
                    (should
                     (eq 'released
                         (plist-get
                          (niwa-session-durability--lease-head
                           (niwa-session-durability--lease-path session-dir))
                          :status))))
                (when acquired-session
                  (niwa-session-durability-lease-release
                   session-dir acquired-session))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "remote repair publishes transcript and sidecar in one ordered batch"
  (let* ((host "restore-publication-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-restore-" t)))
         (running
          (with-temp-buffer
            (org-mode)
            (insert "* Chat\n")
            (insert
             (test-niwa-session-persistence--execution-tool-block
              "remote-call"
              '(:execution-id "remote-stale" :state running
                :status success :live-execution-p t)))
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (buffer-substring-no-properties (point-min) (point-max))))
         restored)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
            (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root running)
            (let ((niwa-session-durability--client-id
                   (make-string 64 ?a))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal))
                  publications)
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (unwind-protect
                  (progn
                    (cl-letf
                        (((symbol-function 'niwa--probe-session-target)
                          #'ignore)
                         ((symbol-function 'niwa--chat-buffer-init-common)
                          #'ignore)
                         ((symbol-function
                           'niwa-agent-persistence-restore-tree)
                          (lambda (&rest _) 1))
                         ((symbol-function
                           'niwa-session-artifacts-load-instructions)
                          #'ignore)
                         ((symbol-function
                           'niwa-session-publication-publish)
                          (lambda (_session artifacts)
                            (push artifacts publications)
                            t)))
                      (setq restored
                            (niwa-session-persistence-restore
                             session-dir nil nil workspace)))
                    (should (= 1 (length publications)))
                    (let* ((artifacts (car publications))
                           (transcript (car artifacts))
                           (commit (car (last artifacts))))
                      (should (equal segment (plist-get transcript :path)))
                      (should (string-match-p
                               ":state lost" (plist-get transcript :content)))
                      (should-not
                       (string-match-p "NIWA_SEGMENT_FINALIZED_AT"
                                       (plist-get transcript :content)))
                      (should
                       (equal
                        (niwa-session-artifacts-sidecar-path session-dir)
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
                        "NIWA_SEGMENT_FINALIZED_AT"
                        (buffer-string)))))
                (when restored
                  (test-niwa-session-persistence--release-and-kill
                   restored
                   (buffer-local-value 'niwa--session restored)))
                (setq restored nil)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "an acquiring restore defers collection; a live reopen preserves its job"
  (let* ((host "restore-collect-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-remote-restore-" t)))
         (niwa-session-publication-keep-recent-generations 1)
         restored)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "* Chat\n")
            (let ((niwa-session-durability--client-id
                   (make-string 64 ?e))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (should
               (niwa-session-durability-lease-acquire
                session-dir "*restore-collect-publisher*" session))
              (unwind-protect
                  (progn
                    (dolist (text '("one" "one more" "one complete"))
                      (test-niwa-session-persistence--publish-generation
                       session session-dir segment (concat text "\n") 1))
                    (test-niwa-session-persistence--publish-generation
                     session session-dir segment "two\n" 2))
                (niwa-session-durability-lease-release
                 session-dir session))
              (let ((before
                     (length
                      (niwa-session-publication--generation-names
                       session-dir))))
                (unwind-protect
                    (cl-letf
                        (((symbol-function 'niwa--probe-session-target)
                          #'ignore)
                         ((symbol-function 'niwa--chat-buffer-init-common)
                          #'ignore)
                         ((symbol-function
                           'niwa-agent-persistence-restore-tree)
                          (lambda (&rest _) 0))
                         ((symbol-function
                           'niwa-session-artifacts-load-instructions)
                          #'ignore))
                      (setq restored
                            (niwa-session-persistence-restore
                             session-dir nil nil workspace))
                      (should (= before (length
                                         (niwa-session-publication--generation-names
                                          session-dir))))
                      (let ((live-session (buffer-local-value 'niwa--session restored)))
                        (should (gethash live-session niwa-session-collection--jobs))
                        (let ((steps 0))
                          (while-let ((job (gethash live-session niwa-session-collection--jobs)))
                            (should (< (cl-incf steps) 100))
                            (niwa-session-collection--step live-session job)))
                        (should-not (gethash live-session niwa-session-collection--jobs)))
                      (let ((after
                             (length
                              (niwa-session-publication--generation-names
                               session-dir))))
                        (should (< after before))
                        (with-current-buffer restored
                          (test-niwa-session-persistence--publish-generation
                           niwa--session session-dir segment "live\n" 2))
                        (let ((live-before
                               (length
                                (niwa-session-publication--generation-names
                                 session-dir)))
                              (job (gethash
                                    (buffer-local-value 'niwa--session restored)
                                    niwa-session-collection--jobs)))
                          (should (> live-before after))
                          (should job)
                          ;; Publishing scheduled this job.  Re-opening only
                          ;; switches buffers; it neither replaces nor runs it.
                          (should (eq restored
                                      (niwa-session-persistence-restore
                                       session-dir nil nil workspace)))
                          (should
                           (eq job
                               (gethash (buffer-local-value 'niwa--session restored)
                                        niwa-session-collection--jobs)))
                          (should
                           (= live-before
                              (length
                               (niwa-session-publication--generation-names
                                session-dir)))))))
                  (when restored
                    (test-niwa-session-persistence--release-and-kill
                     restored
                     (buffer-local-value 'niwa--session restored)))
                  (setq restored nil))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))

  :doc "retargets and persists a missing working directory"
  (cl-destructuring-bind
      (_workspace tempdir _missing-dir replacement-dir session-dir)
      (test-niwa-session-persistence--make-missing-cwd-session)
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
                    (niwa-session-persistence-restore session-dir)))
            (with-current-buffer restored
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (niwa-session-working-directory
                              niwa--session))))
            (let ((sidecar
                   (niwa-session-persistence-load-sidecar
                    (niwa-session-artifacts-sidecar-path session-dir))))
              (should (equal replacement-dir
                             (plist-get sidecar :working-directory)))))
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (niwa-workspace-clear-registry))))
  :doc "keeps a removed Worktree Fork discoverable and preserves its origin"
  (cl-destructuring-bind
      (workspace tempdir missing-dir replacement-dir session-dir)
      (test-niwa-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (test-niwa-session-persistence--edit-published-sidecar
             workspace session-dir
             (lambda (sidecar)
               (plist-put sidecar :fork-type 'worktree)
               (plist-put sidecar :forked-from-session-id "source-id")
               (plist-put sidecar :worktree-source-root tempdir)
               (plist-put sidecar :worktree-directory missing-dir)
               (plist-put sidecar :worktree-branch "worktree/main-fork-1")
               (plist-put sidecar :worktree-base-commit "abc123")))
            (let* ((entry
                    (car
                     (niwa-session-persistence-list-sessions workspace)))
                   (display
                    (niwa-session-persistence--format-session-candidate
                     entry)))
              (should entry)
              (should (string-match-p "Worktree Fork" display))
              (should (string-match-p "missing" display)))
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir)))
              (setq restored
                    (niwa-session-persistence-restore session-dir)))
            (should-not (file-exists-p missing-dir))
            (with-current-buffer restored
              (should (eq 'worktree
                          (niwa-session-fork-type niwa--session)))
              (should (equal missing-dir
                             (niwa-session-worktree-directory
                              niwa--session)))
              (should (equal replacement-dir
                             (niwa-session-working-directory
                              niwa--session))))
            (let* ((summary
                    (plist-get
                     (niwa-session-persistence--read-summary
                      (plist-get (niwa-session-publication-read session-dir)
                                 :sidecar))
                     :summary))
                   (display
                    (niwa-session-persistence--format-session-candidate
                     (list :summary summary))))
              (should (equal missing-dir
                             (plist-get summary :worktree-directory)))
              (should (equal replacement-dir
                             (plist-get summary :working-directory)))
              (should (string-match-p "retargeted" display))
              (should (string-match-p
                       (regexp-quote replacement-dir) display))))
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (niwa-workspace-clear-registry))))
  :doc "does not persist a retargeted directory in read-only mode"
  (cl-destructuring-bind
      (_workspace tempdir missing-dir replacement-dir session-dir)
      (test-niwa-session-persistence--make-missing-cwd-session)
    (let (restored)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) replacement-dir))
                      ((symbol-function
                        'niwa-session-persistence-lock-acquire)
                       (lambda (&rest _) nil)))
              ;; Read-only restore announces itself; this case asserts the
              ;; durable state that notice echoes.
              (niwa-test--with-captured-messages nil
                (setq restored
                      (niwa-session-persistence-restore session-dir))))
            (with-current-buffer restored
              (should niwa-session--read-only-mode)
              (should (equal replacement-dir default-directory))
              (should (equal replacement-dir
                             (niwa-session-working-directory
                              niwa--session))))
            (let ((sidecar
                   (niwa-session-persistence-load-sidecar
                    (niwa-session-artifacts-sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (test-niwa-session-persistence--release-and-kill
         restored
         (and restored (buffer-local-value 'niwa--session restored)))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (niwa-workspace-clear-registry))))
  :doc "rejects an invalid replacement before opening the session"
  (cl-destructuring-bind
      (workspace tempdir missing-dir _replacement-dir session-dir)
      (test-niwa-session-persistence--make-missing-cwd-session)
    (let ((outside (make-temp-file "niwa-cwd-outside-" t))
          (buf-name (niwa-session-buffer-name "main" workspace)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'read-directory-name)
                       (lambda (&rest _) outside)))
              (should-error
               (niwa-session-persistence-restore session-dir)
               :type 'user-error))
            (should-not
             (file-exists-p
              (file-name-concat session-dir ".lock")))
            (should-not (get-buffer buf-name))
            (let ((sidecar
                   (niwa-session-persistence-load-sidecar
                    (niwa-session-artifacts-sidecar-path session-dir))))
              (should (equal missing-dir
                             (plist-get sidecar :working-directory)))))
        (when (file-directory-p tempdir)
          (delete-directory tempdir t))
        (when (file-directory-p outside)
          (delete-directory outside t))
        (niwa-workspace-clear-registry))))
  :doc "round-trips a multi-segment (compacted) session"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Original prompt\n")
                  (niwa-session-artifacts-save session buf)
                  (niwa-session-artifacts-rotate-segment
                   session buf "Summary of segment 1.")
                  (insert "After-compact prompt\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (niwa-session-persistence-restore
                                session-dir))
                (with-current-buffer restored
                  (should (= 2 (niwa-session-current-segment
                                niwa--session)))
                  (should (string-match-p "Summary of segment 1\\."
                                          (buffer-string)))
                  (should (string-match-p "After-compact prompt"
                                          (buffer-string)))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "preserves operation, network, and resource authority across resume"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (setf (niwa-session-permission-rules session)
                      '(("Read" :path "/tmp/foo/**" :action allow)
                        ("Bash" :pattern "npx test*"
                         :network t
                         :file-system
                         ((:path "/tmp/external-input" :access read))
                         :action allow)))
                (setf (niwa-session-resource-grants session)
                      '((:path "/tmp/independent-input" :access read)))
                (with-current-buffer buf
                  (org-mode)
                  (insert "Hi\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored (niwa-session-persistence-restore
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
                    (niwa-session-permission-rules
                     niwa--session)))
                  (should
                   (equal
                    '((:path "/tmp/independent-input" :access read))
                    (niwa-session-resource-grants niwa--session)))
                  (should
                   (equal
                    '(:level additive :additional-permissions
                      (:network t :file-system
                       ((:path "/tmp/external-input" :access read))))
                    (niwa-tool-exec-permission-effective-sandbox-request
                     '(:command "npx test") "Bash" "npx test" nil
                     `(:session ,niwa--session :workspace ,workspace))))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "does not double-rewrite nested relocated working directories"
  (let* ((old-root (file-name-as-directory
                    (make-temp-file "niwa-old-root-" t)))
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
          (niwa-workspace-clear-registry)
          (let ((workspace (niwa-workspace-get-or-create
                            'project old-root old-root "nested-proj")))
            (setq session (niwa-session-create "main" workspace))
            (setf (niwa-session-working-directory session) old-cwd))
          (setq buf (generate-new-buffer "*test-data-buf*"))
          (with-current-buffer buf
            (org-mode)
            (setq-local niwa--session session)
            (insert "Nested relocation\n")
            (niwa-session-artifacts-save session buf))
          (setq session-dir (niwa-session-save-path session))
          (test-niwa-session-persistence--release-and-kill
           buf session)
          (setq buf nil)
          (make-directory (file-name-concat new-root ".niwa") t)
          (copy-file
           (file-name-concat old-root ".niwa" "workspace-id")
           (file-name-concat new-root ".niwa" "workspace-id"))
          (niwa-workspace-clear-registry)
          (setq opened-workspace
                (niwa-workspace-get-or-create
                 'project new-root new-root "nested-proj"))
          (setq restored (niwa-session-persistence-restore
                          session-dir nil nil opened-workspace))
          (with-current-buffer restored
            (should (equal expected-cwd
                           (niwa-session-working-directory
                            niwa--session)))))
      (test-niwa-session-persistence--release-and-kill
       buf session)
      (test-niwa-session-persistence--release-and-kill
       restored
       (and restored (buffer-local-value 'niwa--session restored)))
      (when (file-directory-p old-root)
        (delete-directory old-root t))
      (niwa-workspace-clear-registry)))
  :doc "switches to a live buffer instead of re-loading"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf-name (niwa-session-buffer-name "main" workspace))
               (buf      (get-buffer-create buf-name))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Live buffer\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (setq restored (niwa-session-persistence-restore
                                session-dir))
                ;; Restore should return the existing live buffer.
                (should (eq buf restored)))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "cold-restores the durable tree, mailboxes, recovery, and follow-up"
  (test-niwa-session-persistence--cold-agent-tree-round-trip))


(niwa-deftest niwa-session-persistence-restore/portable-closed-segments (:quiet t)
  ,test
  (test)
  :doc "a project restore repairs stale rows in its published closed segments"
  (cl-destructuring-bind (workspace . tempdir)
      (let ((r (file-name-as-directory (make-temp-file "niwa-restore-portable-" t)))) (cons (test-niwa-session-persistence--make-workspace r) r))
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "First user prompt\n")
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "stale-call"
                    '(:execution-id "exec-stale" :state running
                      :status success :live-execution-p t
                      :output-tail "你好 😀 café")))
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state running
                      :status success :live-execution-p t
                      :output-tail "你好 😀 café")))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (niwa-session-artifacts-save session buf)
                  (niwa-session-artifacts-rotate-segment
                   session buf "Earlier conversation")
                  (insert "Second user prompt\n")
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "current-call"
                    '(:execution-id "exec-current" :state running
                      :status success :live-execution-p t)))
                  (insert
                   (test-niwa-session-persistence--execution-tool-block
                    "tail-call"
                    '(:execution-id "exec-tail" :state completed
                      :status success :live-execution-p nil)))
                  (insert
                   (niwa--format-hook-audit-record
                    '(:type execution-completion
                      :tool-use-id "archived-call"
                      :render-data (:execution-id "exec-stale"
                                    :state completed
                                    :live-execution-p nil))))
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                ;; Release the lock + kill the buffer (the test buffer didn't
                ;; go through chat-buffer-init-common so the kill-hook isn't
                ;; installed; we mirror its work manually).
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (should (file-exists-p session-dir))
                (setq restored (niwa-session-persistence-restore
                                session-dir))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (derived-mode-p 'org-mode))
                  (should (bound-and-true-p gptel-mode))
                  (should niwa--session)
                  (should (equal "main"
                                 (niwa-session-name niwa--session)))
                  (should (= 2 (niwa-session-current-segment
                                niwa--session)))
                  (should-not (niwa-session-execution-state
                               niwa--session))
                  (should (string-match-p "Second user prompt"
                                          (buffer-string)))
                  (should (string-match-p ":state lost"
                                          (buffer-string))))
                (with-temp-buffer
                  (insert (decode-coding-string
                           (niwa-session-artifacts-read-artifact
                            (buffer-local-value 'niwa--session restored)
                            "segment-0001.chat.org" t)
                           'utf-8-unix))
                  (goto-char (point-min))
                  (should (= 2 (how-many ":state archived")))
                  ;; Non-ASCII output survives the repair unescaped.
                  (should (= 2 (how-many (regexp-quote "你好 😀 café")))))
                ;; The replaced closed segment survives publication cleanup.
                (with-temp-buffer
                  (insert-file-contents
                   (file-name-concat session-dir "segment-repair-backups.el"))
                  (goto-char (point-min))
                  (let ((record (read (current-buffer))))
                    (should (equal "segment-0001.chat.org" (plist-get record :segment)))
                    (should (string-match-p ":state running" (plist-get record :content)))
                    (should (string-match-p (regexp-quote "你好 😀 café")
                                            (plist-get record :content)))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-session-persistence-restore/unsettled-mutation (:quiet t)
  ,test
  (test)
  :doc "restore inherits an unsettled mutation and acknowledgement clears it"
  (let* ((host "restore-unsettled-mutation")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-restore-unsettled-" t)))
         (owner-id (make-string 64 ?a))
         (successor-id (make-string 64 ?b))
         (observer-id (make-string 64 ?c))
         restored
         (niwa-session-durability--disclosed-targets
          (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _)
                 (ert-fail "Unsettled-mutation restore prompted"))))
      (unwind-protect
          (niwa-test--with-local-shell-tramp (list host)
            (cl-destructuring-bind (workspace owner session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target owner))
               t niwa-session-durability--disclosed-targets)
              (let ((niwa-session-durability--client-id owner-id))
                (should
                 (niwa-session-durability-lease-acquire
                  session-dir "*previous-owner*" owner))
                (should
                 (niwa-session-durability-set-unsettled-mutation
                  owner t))
                (niwa-session-durability-lease-release
                 session-dir owner))
              (let ((niwa-session-durability--client-id successor-id))
                (cl-letf
                    (((symbol-function 'niwa--probe-session-target)
                      #'ignore)
                     ((symbol-function 'niwa--run-session-start-hooks)
                      #'ignore))
                  (setq restored
                        (niwa-session-persistence-restore
                         session-dir nil nil workspace))
                  (with-current-buffer restored
                    (should
                     (niwa-execution-mutation-blocked-p niwa--session))
                    (niwa-execution-acknowledge-unknown niwa--session)
                    (should-not
                     (niwa-execution-mutation-blocked-p niwa--session))
                    (should-not
                     (niwa-session-durability-unsettled-mutation-p
                      niwa--session))
                    (niwa-session-durability-lease-release
                     session-dir niwa--session)
                    (let ((niwa-session-durability--client-id observer-id))
                      (should
                       (niwa-session-durability-lease-acquire
                        session-dir "*observer*" niwa--session))
                      (should-not
                       (niwa-session-durability-unsettled-mutation-p
                        niwa--session))
                      (niwa-session-durability-lease-release
                       session-dir niwa--session)))))))
        (niwa-test--with-local-shell-tramp (list host)
          (test-niwa-session-persistence--release-and-kill
           restored
           (and restored
                (buffer-local-value 'niwa--session restored))))
        (when (file-directory-p local-root)
          (delete-directory local-root t))
        (niwa-workspace-clear-registry)))))


;;
;;; Phase 6: locking

(defun test-niwa-session-persistence--identity (ticks &optional boot)
  "Return a well-formed process identity with start TICKS.
BOOT replaces the fixed fixture boot id, standing in for another boot."
  (format "%s:%d" (or boot "0f1e2d3c-4b5a-4968-8776-655443322110") ticks))

(niwa-deftest niwa-session-persistence-lock-acquire-release (:quiet t)
  ,test
  (test)
  :doc "acquires and releases the portable lease without creating a PID lock"
  (let* ((directory (make-temp-file "niwa-lease-wrapper-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace directory))
         (session (niwa-session-create "lease" workspace)))
    (setf (niwa-session-save-path session) directory)
    (unwind-protect
        (progn
          (should (niwa-session-persistence-lock-acquire directory "*lease*" session))
          (should (niwa-session-durability-lease-owned-p session))
          (should-not (file-exists-p (file-name-concat directory ".lock")))
          (niwa-session-persistence-lock-release directory session)
          (should-not (niwa-session-durability-lease-owned-p session))
          (should (eq 'available (niwa-session-durability-lease-state directory))))
      (niwa-session-persistence-lock-release directory session)
      (delete-directory directory t)))
  :doc "refuses legacy PID evidence without prompting or changing it"
  (let* ((directory (make-temp-file "niwa-legacy-wrapper-" t))
         (workspace (test-niwa-session-persistence--make-file-workspace directory))
         (session (niwa-session-create "legacy" workspace))
         (path (file-name-concat directory ".lock"))
         (evidence "legacy holder bytes"))
    (unwind-protect
        (progn
          (write-region evidence nil path nil 'silent)
          (cl-letf (((symbol-function 'read-char-choice)
                     (lambda (&rest _) (ert-fail "Legacy control prompted")))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (ert-fail "Legacy control prompted"))))
            (should-error (niwa-session-persistence-lock-acquire
                           directory "*legacy*" session))
            (should-error (niwa-session-persistence-lock-release directory session)))
          (should (equal evidence (with-temp-buffer
                                    (insert-file-contents path) (buffer-string))))
          (should-not (file-exists-p (file-name-concat directory ".lease"))))
      (delete-directory directory t))))

(niwa-deftest niwa-session-persistence-control-transfer-poll ()
  ,test
  (test)
  :doc "keeps a drained transfer quiescing when no root buffer can be saved"
  (let* ((session (niwa-session--create :name "transfer-poll"))
         (transfer '(:state quiescing))
         saved released)
    (setf (niwa-session-control-transfer session) transfer)
    (cl-letf (((symbol-function
                'niwa-session-transfer-poll)
               (lambda (_) transfer))
              ((symbol-function
                'niwa-session-persistence-root-buffer-for-session)
               (lambda (&rest _) nil))
              ((symbol-function 'niwa-session-artifacts-save)
               (lambda (&rest _) (setq saved t)))
              ((symbol-function
                'niwa-session-transfer-release)
               (lambda (_) (setq released t))))
      (should
       (eq transfer
           (niwa-session-control-transfer-poll session nil nil)))
      (should-not saved)
      (should-not released)))
  :doc "refreshes committed transcript and sidecar state before enabling writes"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-transfer-refresh-" t)))
         (workspace
          (test-niwa-session-persistence--make-file-workspace root))
         (session (niwa-session-create "requester" workspace))
         (refreshed (niwa-session-create "requester" workspace))
         (buffer (generate-new-buffer " *niwa-transfer-refresh*"))
         (save-path (file-name-as-directory
                     (file-name-concat root "session")))
         (segment-path (file-name-concat save-path "segment-0002.chat.org"))
         instructions-loaded incarnation-checked released)
    (make-directory save-path t)
    (write-region "initial transcript" nil segment-path nil 'silent)
    (setf (niwa-session-save-path session) save-path
          (niwa-session-control-transfer session)
          '(:state requested :request (:request-id "request"))
          (niwa-session-turn-count refreshed) 7
          (niwa-session-current-segment refreshed) 2)
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-file-name segment-path)
          (insert "stale transcript")
          (set-buffer-modified-p nil)
          (set-visited-file-modtime)
          (setq buffer-read-only t
                niwa-session--read-only-mode t)
          (write-region "owner committed transcript" nil segment-path
                        nil 'silent)
          (should-not (verify-visited-file-modtime buffer))
          (cl-letf
              (((symbol-function 'niwa-session-durability-lease-acquire)
                (lambda (_path _name actual)
                  (setf (niwa-session-lease actual) '(:state owned))
                  t))
               ((symbol-function 'niwa-session-transfer-observe-decision)
                (lambda (&rest _) nil))
               ((symbol-function
                 'niwa-session-publication-read)
                (lambda (_) '(:sidecar "/committed/session.meta.el")))
               ((symbol-function
                 'niwa-session-persistence-load-sidecar)
                (lambda (_ &optional _scope) '(:committed t)))
               ((symbol-function 'niwa-session-codec-deserialize)
                (lambda (&rest _) (list :session refreshed)))
               ((symbol-function 'niwa-session-artifacts-read-artifact)
                (lambda (&rest _) "fresh transcript"))
               ((symbol-function
                 'niwa-session-artifacts-check-target-incarnation)
                (lambda (_session checked-buffer)
                  (should-not (eq checked-buffer buffer))
                  (should (buffer-live-p checked-buffer))
                  (should (with-current-buffer buffer buffer-read-only))
                  (setq incarnation-checked t)))
               ((symbol-function 'niwa-transcript-restore-gptel-state)
                #'ignore)
               ((symbol-function
                 'niwa-session-artifacts-load-instructions)
                (lambda (&rest _) (setq instructions-loaded t)))
               ((symbol-function
                 'niwa-session-durability-lease-release)
                (lambda (&rest _) (setq released t)))
               ((symbol-function 'ask-user-about-supersession-threat)
                (lambda (&rest _) (error "Supersession prompt"))))
            ;; Acquiring control announces itself; the case asserts the
            ;; durable state that notice echoes.
            (niwa-test--with-captured-messages nil
              (should
               (niwa-session-control-transfer-poll session buffer t)))
            (should (= 7 (niwa-session-turn-count session)))
            (should (equal "fresh transcript" (buffer-string)))
            (should-not buffer-read-only)
            (should-not niwa-session--read-only-mode)
            (should instructions-loaded)
            (should incarnation-checked)
            (should-not released)
            (should
             (eq 'acquired
                 (plist-get (niwa-session-control-transfer session)
                            :state)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  :doc "releases a newly acquired lease when committed refresh fails"
  (let* ((session (niwa-session--create
                   :name "failed-refresh" :save-path "/session/"))
         (buffer (generate-new-buffer " *niwa-transfer-failure*"))
         released)
    (setf (niwa-session-control-transfer session)
          '(:state requested :request (:request-id "request")))
    (unwind-protect
        (with-current-buffer buffer
          (setq buffer-read-only t
                niwa-session--read-only-mode t)
          (cl-letf
              (((symbol-function 'niwa-session-durability-lease-acquire)
                (lambda (&rest _) t))
               ((symbol-function 'niwa-session-transfer-observe-decision)
                (lambda (&rest _) nil))
               ((symbol-function
                 'niwa-session-publication-read)
                (lambda (_) (error "Injected refresh failure")))
               ((symbol-function
                 'niwa-session-durability-lease-release)
                (lambda (&rest _) (setq released t))))
            (should-error
             (niwa-session-control-transfer-poll session buffer t))
            (should released)
            (should buffer-read-only)
            (should niwa-session--read-only-mode)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))


;;
;;; Phase 9: fork-on-send + rename-session

(niwa-deftest niwa-save-session (:quiet t)
  ,test
  (test)
  :doc "refuses save-as without changing a tree that has an active turn"
  (with-temp-buffer
    (let* ((session (niwa-session--create :name "save-as"))
           (record
            (niwa-agent-record--create :activity 'running)))
      (setq-local niwa--session session)
      (setf (niwa-session-agent-registry session)
            (list (cons "/root/worker" record)))
      (let ((err (should-error (niwa-save-session t)
                               :type 'user-error)))
        (should (string-match-p
                 "Interrupt active agent turns"
                 (error-message-string err))))
      (should (eq record
                  (cdr (assoc
                        "/root/worker"
                        (niwa-session-agent-registry session)))))))
  :doc "refuses save-as while input is queued, which both copies would deliver"
  (with-temp-buffer
    (let ((session (niwa-session--create :name "save-as")))
      (setq-local niwa--session session)
      (niwa-session-enqueue-pending-input session 'follow-up '(:input "later"))
      (should (string-match-p
               "Resolve pending input"
               (error-message-string
                (should-error (niwa-save-session t) :type 'user-error))))
      (should (niwa-session-pending-follow-ups session))))
  :doc "save-as canonically publishes the parent once"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "main" workspace))
           (buffer (generate-new-buffer " *save-as-publication*"))
           (save-function
            (symbol-function 'niwa-session-artifacts-save))
           (canonical-saves 0))
      (unwind-protect
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (insert "original\n")
            (niwa-session-artifacts-save session buffer)
            ;; Saving an artifact's conversation as a copy makes a chat.
            (setf (niwa-session-dedicated-artifact session) "board")
            (goto-char (point-max))
            (insert "pending\n")
            (cl-letf
                (((symbol-function 'read-string)
                  (lambda (&rest _) "clone"))
                 ((symbol-function 'niwa-session-artifacts-save)
                  (lambda (&rest arguments)
                    (cl-incf canonical-saves)
                    (apply save-function arguments))))
              (niwa-save-session t))
            (should (= 1 canonical-saves))
            (should-not (niwa-session-dedicated-artifact session))
            (should-not (buffer-modified-p)))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))
  :doc "remote save-as fences the parent until a fresh child lease is held"
  (let* ((host "save-as-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-save-as-remote-" t)))
         (owner-id (make-string 64 ?a))
         (competitor-id (make-string 64 ?b))
         buffer
         session
         old-id
         old-save-path
         new-save-path)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (unwind-protect
              (cl-destructuring-bind
                  (workspace fixture-session session-dir segment)
                  (test-niwa-session-persistence--make-remote-restore-fixture
                   host local-root "Parent transcript\n")
                (setq session fixture-session
                      old-id (niwa-session-session-id fixture-session)
                      old-save-path session-dir
                      buffer (generate-new-buffer " *save-as-lease*"))
                (setf (niwa-session-naming-state session) 'pending
                      (niwa-session-dedicated-artifact session) "board")
                (let* ((niwa-session-durability--client-id owner-id)
                       (niwa-session-durability--disclosed-targets
                        (make-hash-table :test #'equal))
                       (materialize-function
                        (symbol-function
                         'niwa-session-rewind-materialize-publication))
                       (publish-function
                        (symbol-function
                         'niwa-session-publication-publish))
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
                   (niwa-execution-target-identity
                    (niwa-session-execution-target session))
                   t niwa-session-durability--disclosed-targets)
                  ;; Start the parent above generation one so copied lease
                  ;; state cannot look like a freshly acquired child.
                  (should
                   (niwa-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (niwa-session-durability-lease-release
                   old-save-path session)
                  (should
                   (niwa-session-durability-lease-acquire
                    old-save-path "*save-as-parent*" session))
                  (setq parent-generation
                        (plist-get
                         (niwa-session-lease session) :generation))
                  (should (> parent-generation 1))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local niwa--session session
                                buffer-file-name segment)
                    (insert "Parent transcript\n"))
                  (cl-letf
                      (((symbol-function 'read-string)
                        (lambda (&rest _) "clone"))
                       ((symbol-function 'niwa-execution-target-probe)
                        (lambda (&rest _)
                          (setq target-probed t)
                          '(:status ready)))
                       ((symbol-function 'copy-directory)
                        (lambda (&rest _)
                          (setq copy-called t)
                          (error "Remote Save As must not copy a session directory")))
                       ((symbol-function
                         'niwa-session-rewind-materialize-publication)
                        (lambda (actual-session publication destination)
                          (let ((niwa-session-durability--client-id
                                 competitor-id))
                            (should-not
                             (niwa-session-durability-lease-acquire
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
                        'niwa-session-publication-publish)
                        (lambda (actual-session artifacts &rest options)
                          (let ((sidecar-artifact
                                 (cl-find-if
                                  (lambda (artifact)
                                    (equal
                                     (plist-get artifact :path)
                                     (niwa-session-artifacts-sidecar-path
                                      (niwa-session-save-path actual-session))))
                                  artifacts)))
                            (when (and staging-path sidecar-artifact)
                            (should-not (eq actual-session session))
                            (setq fresh-child t)
                            (setq child-session actual-session)
                            (should
                             (equal staging-path
                                    (niwa-session-save-path actual-session)))
                            (should (plist-get sidecar-artifact
                                               :commit-marker))
                            (should
                             (niwa-session-durability-lease-owned-p
                              actual-session))
                            (should
                             (eq
                              'publishing
                              (plist-get
                               (niwa-session-durability--lease-head
                                (niwa-session-durability--lease-path
                                 old-save-path))
                               :status)))
                            (setq child-rewrite-authorized t)))
                          (apply publish-function
                                 actual-session artifacts options))))
                    (should
                     (eq 'portable
                         (niwa-session-authority-mode-for-session
                          session)))
                    (with-current-buffer buffer
                      (niwa-save-session t))
                  (should target-probed)
                  (should (eq 'explicit (niwa-session-naming-state session)))
                  (should-not (niwa-session-dedicated-artifact session))
                  (should competitor-blocked)
                  (should materialized)
                  (should-not copy-called)
                  (should fresh-child)
                  (should child-rewrite-authorized)
                  (should child-session)
                  (should-not (niwa-session-lease child-session))
                  (should
                   (niwa-session-durability-lease-owned-p session))
                  (should
                   (= 1
                      (plist-get (niwa-session-lease session)
                                 :generation)))
                  (should
                   (eq
                    'released
                    (plist-get
                     (niwa-session-durability--lease-head
                      (niwa-session-durability--lease-path old-save-path))
                     :status)))
                  (setq new-save-path (niwa-session-save-path session))
                  (let ((publication
                         (niwa-session-publication-read
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
                               (niwa-session-artifacts-read-artifact
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
                              (niwa-session-artifacts-read-artifact
                               session "session.meta.el" t))
                             (goto-char (point-min))
                             (read (current-buffer)))))
                      (should (equal "clone"
                                     (plist-get sidecar :session-name)))
                      (should
                       (equal old-id
                              (plist-get sidecar
                                         :forked-from-session-id)))))))
            (when (and session (niwa-session-save-path session))
              (ignore-errors
                (niwa-session-persistence-lock-release
                 (niwa-session-save-path session) session)))
            (when old-save-path
              (ignore-errors
                (niwa-session-persistence-lock-release old-save-path)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))))


;;
;;; Phase 10: resume / list / save commands

(niwa-deftest niwa-session-persistence--entry-authority ()
  ,test
  (test)
  :doc "names the action a portable session's lease state actually produces"
  (let* ((root (make-temp-file "niwa-entry-action-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-id "s-1")))
         (status nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-durability-cleanup-assessment)
              (lambda (&rest _) status))
             ((symbol-function
               'niwa-session-persistence--entry-live-buffer)
              (lambda (&rest _) nil)))
          (setq status '(:state foreign :host "desktop"))
          (should
           (equal '(:action "Join" :detail "held by desktop" :held t)
                  (niwa-session-persistence--entry-authority
                   workspace entry)))
          ;; An expired lease offers a takeover precisely because nobody is
          ;; writing the session any more, so it is not a live writer.
          (setq status '(:state expired :host "laptop"))
          (should
           (equal '(:action "Take over" :detail "lease expired, was laptop"
                    :held nil)
                  (niwa-session-persistence--entry-authority
                   workspace entry)))
          ;; An unheld lease resumes wherever its files live; the machine
          ;; that last held it is context, not a different action.
          (setq status (list :state 'available :host "laptop"))
          (should
           (equal '(:action "Resume" :detail "last held by laptop" :held nil)
                  (niwa-session-persistence--entry-authority
                   workspace entry)))
          (setq status (list :state 'available :host (system-name)))
          (should
           (equal '(:action "Resume" :detail nil :held nil)
                  (niwa-session-persistence--entry-authority
                   workspace entry)))
          ;; A lease predating recorded hosts still resumes.
          (setq status '(:state available :host nil))
          (should
           (equal "Resume"
                  (niwa-session-persistence--entry-action
                   workspace entry))))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))

  :doc "prefers switching to a session this Emacs already has open"
  (let* ((root (make-temp-file "niwa-entry-action-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-id "s-1")))
         (live (generate-new-buffer " *niwa-entry-live*")))
    (unwind-protect
        (cl-letf
            (((symbol-function
               'niwa-session-persistence--entry-live-buffer)
              (lambda (&rest _) live))
             ((symbol-function 'niwa-session-durability-lease-status)
              (lambda (&rest _)
                (ert-fail "An open session must not cost a lease read"))))
          (should
           (equal '(:action "Switch" :detail "already open here" :held t)
                  (niwa-session-persistence--entry-authority
                   workspace entry)))
          (with-current-buffer live
            (setq-local niwa-session--read-only-mode t))
          (should
           (equal '(:action "Switch" :detail "already open here, read-only"
                    :held t)
                  (niwa-session-persistence--entry-authority
                   workspace entry))))
      (when (buffer-live-p live)
        (kill-buffer live))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-choose-entry ()
  ,test
  (test)
  :doc "acknowledges shared-files disclosure once across workspaces and restores"
  (let* ((root (make-temp-file "niwa-entry-choice-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-name "main")))
         (choice "Start new session")
         (warned 0)
         (accepted nil)
         (quit-p nil)
         (niwa-session-persistence--shared-files-accepted-p nil)
         (authority '(:action "Join" :detail "held by desktop" :held t))
         (restored (generate-new-buffer " *niwa-entry-restored*")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _)
                (list :sessions (list entry) :incompatible nil)))
             ((symbol-function
               'niwa-session-persistence--entry-authority)
              (lambda (&rest _) authority))
             ((symbol-function
               'niwa-session-persistence--format-session-candidate)
              (lambda (&rest _) "main"))
             ((symbol-function
               'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (&rest _) choice))
             ((symbol-function 'yes-or-no-p)
              (lambda (prompt)
                (should (string-match-p "Independent sessions.*Worktree Fork"
                                        prompt))
                (cl-incf warned)
                (when quit-p (signal 'quit nil))
                accepted))
             ((symbol-function 'niwa-session-persistence-restore)
              (lambda (path &rest _)
                (should (equal "/session/" path))
                restored)))
          ;; A refusal or quit cancels this attempt, without accepting future
          ;; starts.  Only a positive acknowledgement suppresses the advisory.
          (should-error
           (niwa-session-persistence-choose-entry workspace)
           :type 'user-error)
          (should (= 1 warned))
          (should-not niwa-session-persistence--shared-files-accepted-p)
          (setq quit-p t)
          (should (eq 'quit
                      (condition-case nil
                          (niwa-session-persistence-choose-entry workspace)
                        (quit 'quit))))
          (should (= 2 warned))
          (should-not niwa-session-persistence--shared-files-accepted-p)
          (setq quit-p nil accepted t)
          (should
           (eq 'new
               (niwa-session-persistence-choose-entry workspace)))
          (should (= 3 warned))
          (should niwa-session-persistence--shared-files-accepted-p)
          (should (eq 'new
                      (niwa-session-persistence-choose-entry workspace)))
          (should (eq 'new
                      (niwa-session-persistence-choose-entry
                       (niwa-workspace--create :id "another-project"
                                                 :root root))))
          (should (= 3 warned))
          (setq choice "Join       main")
          (should
           (eq restored
               (niwa-session-persistence-choose-entry workspace)))
          (cl-letf (((symbol-function 'niwa-session-persistence-restore)
                     (lambda (&rest _) (user-error "Session authority changed"))))
            (let ((err (should-error
                        (niwa-session-persistence-choose-entry workspace)
                        :type 'user-error)))
              (should (equal (format "niwa [%s]: Session authority changed"
                                     (niwa-workspace-root workspace))
                             (error-message-string err)))))
          ;; Nothing holds an unheld session, so starting an independent one
          ;; alongside it needs no warning.  Neither does an expired lease:
          ;; its writer is gone, which is why the row offers a takeover.
          (setq niwa-session-persistence--shared-files-accepted-p nil)
          (dolist (unheld '((:action "Resume" :detail nil :held nil)
                            (:action "Take over" :detail "lease expired"
                             :held nil)))
            (setq authority unheld
                  choice "Start new session"
                  warned 0)
            (should
             (eq 'new
                 (niwa-session-persistence-choose-entry workspace)))
            (should (= 0 warned))
            (should-not niwa-session-persistence--shared-files-accepted-p)))
      (when (buffer-live-p restored)
        (kill-buffer restored))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))

  :doc "inspects an incompatible row without authority reads or restore"
  (let* ((root (make-temp-file "niwa-entry-inspect-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (entry '(:kind incompatible :save-path "/old-session/"
                  :authority-mode portable :status unsupported
                  :version "v0.5.3" :updated-at "2026-09-01T12-00-00"))
         (inspection (generate-new-buffer " *niwa-entry-inspection*"))
         prompt row)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _)
                (list :sessions nil :incompatible (list entry))))
             ((symbol-function
               'niwa-session-control-fs-tree-sizes)
              (lambda (_paths)
                '((:path "/old-session/" :status ok :value 42))))
             ((symbol-function 'niwa-session-rewind-format-relative-time)
              (lambda (_iso) "4 months"))
             ((symbol-function
               'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (read-prompt choices &rest _)
                (setq prompt read-prompt
                      row (seq-find
                           (lambda (choice) (string-prefix-p "Inspect" choice))
                           choices))))
             ((symbol-function
               'niwa-session-persistence--entry-authority)
              (lambda (&rest _)
                (ert-fail "Inspect must not calculate resume authority")))
             ((symbol-function 'niwa-session-persistence-restore)
              (lambda (&rest _)
                (ert-fail "Inspect must not restore a session")))
             ((symbol-function
               'niwa-session-artifacts-inspect-cold-session)
              (lambda (path &optional publication scope)
                (should (equal "/old-session/" path))
                (should-not publication)
                (should (equal (niwa-workspace-root workspace) scope))
                inspection)))
          (should
           (equal (list :action 'inspect :buffer inspection)
                  (niwa-session-persistence-choose-entry workspace)))
          (should (equal "Niwa session (1 cannot resume): " prompt))
          (should (string-prefix-p "Inspect    4 months" row))
          (should (string-suffix-p "/old-session/" row)))
      (when (buffer-live-p inspection)
        (kill-buffer inspection))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))
  :doc "lists compatible rows before incompatible ones and keeps their actions"
  (let* ((root (make-temp-file "niwa-entry-mixed-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (session '(:save-path "/session/" :summary (:session-name "main")))
         (incompatible '(:kind incompatible :save-path "/old-session/"
                         :authority-mode portable :status missing
                         :diagnostic "Sidecar is missing"))
         (restored (generate-new-buffer " *niwa-entry-mixed-restored*"))
         prompt choices)
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _)
                (list :sessions (list session)
                      :incompatible (list incompatible))))
             ((symbol-function 'niwa-session-control-fs-tree-sizes)
              (lambda (_paths)
                '((:path "/old-session/" :status failed))))
             ((symbol-function
               'niwa-session-persistence--entry-authority)
              (lambda (&rest _) '(:action "Resume" :detail nil :held nil)))
             ((symbol-function
               'niwa-session-persistence--format-session-candidate)
              (lambda (&rest _) "main"))
             ((symbol-function 'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (read-prompt read-choices &rest _)
                (setq prompt read-prompt
                      choices read-choices)
                (nth 1 read-choices)))
             ((symbol-function 'niwa-session-artifacts-inspect-cold-session)
              (lambda (&rest _)
                (ert-fail "A compatible row must not be inspected")))
             ((symbol-function 'niwa-session-persistence-restore)
              (lambda (path &rest _)
                (should (equal "/session/" path))
                restored)))
          (should (eq restored
                      (niwa-session-persistence-choose-entry workspace)))
          (should (equal "Niwa session (1 cannot resume): " prompt))
          (should (equal '("Start new session" "Resume     main")
                         (seq-take choices 2)))
          (should (string-match-p
                   "\\`Inspect .*size unavailable.*/old-session/\\'"
                   (nth 2 choices)))
          (should (= 3 (length choices))))
      (when (buffer-live-p restored)
        (kill-buffer restored))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))
  :doc "keeps a non-inspectable incompatible row visible and fails closed"
  (let* ((root (make-temp-file "niwa-entry-refuse-inspect-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (entry '(:kind incompatible :save-path "/mixed-session/"
                  :authority-mode portable :status undiscoverable
                  :diagnostic "mixed authority")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _)
                (list :sessions nil :incompatible (list entry))))
             ((symbol-function 'niwa-session-control-fs-tree-sizes)
              (lambda (&rest _)
                (signal 'file-error '("Injected sizing failure"))))
             ((symbol-function
               'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (_prompt choices &rest _)
                (car (seq-filter
                      (lambda (choice) (string-prefix-p "Inspect" choice))
                      choices))))
             ((symbol-function
               'niwa-session-artifacts-inspect-cold-session)
              (lambda (&rest _)
                (ert-fail "Unsafe inspection must not read a transcript"))))
          (let ((err
                 (should-error
                  (niwa-session-persistence-choose-entry workspace)
                  :type 'user-error)))
            (should (string-match-p "not safely inspectable" (cadr err)))))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-choose-entry/recovery ()
  (let* ((root (make-temp-file "niwa-entry-recovery-" t))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (entry '(:save-path "/session/" :summary (:session-name "main")))
         (reads 0)
         (action "Cancel")
         (reasons '(publishing))
         (restored (generate-new-buffer " *niwa-recovery-choice*")))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired) #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _) (list :sessions (and entry (list entry)))))
             ((symbol-function 'niwa-session-durability-cleanup-assessment)
              (lambda (&rest _)
                (list :state 'expired :host "laptop" :reasons reasons
                      :recoverable t)))
             ((symbol-function 'niwa-session-durability-lease-status)
              (lambda (&rest _) '(:state expired :host "laptop")))
             ((symbol-function 'niwa-session-persistence--format-session-candidate)
              (lambda (_entry detail)
                (when (equal reasons '(publishing))
                  (should (string-match-p "cleanup blocked: interrupted publication" detail)))
                "main"))
             ((symbol-function 'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (_prompt choices &rest _)
                (if (= 1 (cl-incf reads))
                    (nth 1 choices)
                  (should (equal '("Recover" "Discard" "Cancel") choices))
                  action)))
             ((symbol-function 'niwa-session-persistence-restore)
              (lambda (path &rest _)
                (should (equal "/session/" path))
                restored)))
          ,test)
      (kill-buffer restored)
      (delete-directory root t)
      (niwa-workspace-clear-registry)))
  (test)
  :doc "offers recovery actions and cancels without restoring or starting a session"
  (cl-letf (((symbol-function 'niwa-session-persistence-restore)
             (lambda (&rest _) (ert-fail "Cancel must not restore"))))
    (should (eq 'quit (condition-case nil
                         (niwa-session-persistence-choose-entry workspace)
                       (quit 'quit))))
    (should (= 2 reads)))
  :doc "routes Recover through ordinary restore and takeover"
  (progn
    (setq action "Recover")
    (should (eq restored (niwa-session-persistence-choose-entry workspace)))
    (should (= 2 reads)))
  :doc "takes over a session blocked only by pending journal captures without asking"
  (progn
    (setq reasons '(journal-pins))
    (should (eq restored (niwa-session-persistence-choose-entry workspace)))
    (should (= 1 reads)))
  :doc "refreshes after discard and never implicitly starts a replacement session"
  (cl-letf (((symbol-function 'niwa-session-persistence-discard)
             (lambda (selected-workspace selected)
               (should (eq workspace selected-workspace))
               (should (equal "/session/" (plist-get selected :save-path)))
               (setq entry nil)
               t))
            ((symbol-function 'completing-read)
             (lambda (_prompt choices &rest _)
               (pcase (cl-incf reads)
                 (1 (nth 1 choices))
                 (2 "Discard")
                 (3 (should (equal '("Start new session") choices))
                    (signal 'quit nil))
                 (_ (ert-fail "Unexpected extra chooser"))))))
    (should (eq 'quit (condition-case nil
                         (niwa-session-persistence-choose-entry workspace)
                       (quit 'quit))))
    (should (= 3 reads))))


(niwa-deftest niwa-session-persistence-discard ()
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Keep this?" "Saved."))
     (let* ((workspace (niwa-session-workspace session))
            (directory (niwa-session-save-path session)))
       (niwa-session-durability-lease-release directory session)
       (let ((niwa-journal-enabled nil))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (let ((entry (car (niwa-session-persistence-list-sessions workspace))))
         (should entry)
         (cl-letf (((symbol-function 'niwa-session-persistence-restore)
                    (lambda (&rest _) (ert-fail "Discard must not restore"))))
           ,test)))))
  (test)
  :doc "declining discard preserves the session and lease bytes"
  (let ((lease (niwa-session-durability-lease-status directory)))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-not (niwa-session-persistence-discard workspace entry)))
    (should (file-directory-p directory))
    (should (equal lease (niwa-session-durability-lease-status directory))))
  :doc "deletes a confirmed cold session without restoring conversation buffers"
  (cl-letf (((symbol-function 'yes-or-no-p)
             (lambda (prompt)
               (should (string-match-p "permanently" prompt))
               (should (string-match-p "child" prompt))
               t)))
    (should (niwa-session-persistence-discard workspace entry))
    (should-not (file-exists-p directory))))


(niwa-deftest niwa-session-persistence/missing-published-transcript ()
  ,test
  (test)
  :doc "restore retains a missing published transcript until confirmed discard"
  (niwa-test-journal-capture--with-portable-session
   (lambda (session buffer)
     (let ((niwa-journal-enabled nil))
       (niwa-test-journal-capture--turn session buffer "Retain this" "Saved."))
     (let* ((workspace (niwa-session-workspace session))
            (directory (niwa-session-save-path session))
            (logical (file-name-nondirectory
                      (niwa-session-artifacts-segment-path
                       directory (niwa-session-current-segment session))))
            (publication (niwa-session-publication-read directory))
            (artifact (cdr (assoc logical (plist-get publication :artifacts))))
            (head (plist-get publication :head))
            confirmed)
       (should artifact)
       (should (file-exists-p (plist-get artifact :published)))
       (niwa-session-durability-lease-release directory session)
       (let ((niwa-journal-enabled nil))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file (plist-get artifact :published))
       (let ((err (should-error
                   (niwa-session-persistence-restore directory nil nil workspace)
                   :type 'user-error)))
         (should (string-match-p "Published session segment is unavailable"
                                 (error-message-string err)))
         (should (string-match-p (regexp-quote logical)
                                 (error-message-string err))))
       ;; Failed restore must neither prune the orphan nor publish replacement state.
       (should (file-directory-p directory))
       (should (equal head (niwa-session-durability-publication-head directory)))
       (should (eq 'available
                   (plist-get (niwa-session-durability-lease-status directory)
                              :state)))
       (should-not (niwa-session-persistence-find-live-buffer
                    (niwa-session-session-id session)
                    (niwa-session-buffer-name
                     (niwa-session-name session) workspace)))
       (let ((entry (car (niwa-session-persistence-list-sessions workspace))))
         (should entry)
         (should (equal directory (plist-get entry :save-path)))
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (prompt)
                      (should (string-match-p "permanently" prompt))
                      (should (string-match-p "writers have stopped" prompt))
                      (setq confirmed t))))
           (should (niwa-session-persistence-discard workspace entry)))
         (should confirmed)
         (should-not (file-exists-p directory)))))))


(niwa-deftest niwa-session-persistence--control-artifacts ()
  ,test
  (test)
  :doc "retains a failed optional probe without treating it as absence"
  (let* ((root (make-temp-file "niwa-control-artifacts-" t))
         (entry (file-name-concat root "session"))
         (lock (file-name-concat entry ".lock"))
         (niwa-session-durability--asserted-directories (list nil)))
    (unwind-protect
        (progn
          (make-directory entry)
          (make-symbolic-link "missing" lock)
          (let* ((observed
                  (niwa-session-persistence--control-artifacts (list entry)))
                 (results (cdr (assoc entry observed))))
            (should (eq 'failed (plist-get (car results) :status))))
          (should-not
           (member (niwa-session-control-fs-physical-path entry)
                   (car niwa-session-durability--asserted-directories))))
      (delete-directory root t))))


(niwa-deftest niwa-session-persistence--discover-entry ()
  ,test
  (test)
  :doc "reuses batched publication observations and rejects fresh contradictions"
  (let* ((root (make-temp-file "niwa-discover-proof-" t))
         (entry (file-name-concat root "session"))
         (sidecar (file-name-concat entry "session.meta.el"))
         (session (test-niwa-session-persistence--gate-session entry))
         (native (symbol-function 'process-file))
         (programs 0))
    (unwind-protect
        (progn
          (make-directory entry t)
          ;; Publish obsolete metadata so classification, not absent authority,
          ;; keeps this row incompatible.
          (test-niwa-session-persistence--lease-fixture
           session (list (list :path sidecar :content "(:version 0)"
                               :commit-marker t)))
          (niwa-session-persistence-lock-release entry session)
          (niwa-session-durability-with-transaction
            (let* ((controls
                    (cdar (niwa-session-persistence--control-artifacts
                           (list entry))))
                   (listings (niwa-session-persistence--lease-listings
                              (list entry)))
                   (observations (niwa-session-publication-read-batch
                                  (list entry) listings)))
              (should-not (plist-get (cdar observations) :error))
              (cl-letf (((symbol-function 'process-file)
                         (lambda (&rest args)
                           (cl-incf programs)
                           (apply native args))))
                (let ((row (niwa-session-persistence--discover-entry
                            entry 'portable controls (cdar listings)
                            (cdar observations))))
                  (should (eq 'incompatible (plist-get row :kind)))
                  (should (eq 'unsupported (plist-get row :status)))))
              ;; Discovery consumes the batch; it must not reread the sidecar.
              (should (= programs 0))))
          (with-temp-file (file-name-concat entry ".lock") (insert "(:pid 1)"))
          (let ((controls (cdar (niwa-session-persistence--control-artifacts
                                 (list entry)))))
            (should-error (niwa-session-persistence--discover-entry
                           entry 'portable controls nil)))
          (delete-file (file-name-concat entry ".lock"))
          (delete-directory (file-name-concat entry ".lease") t)
          (make-symbolic-link "missing" (file-name-concat entry ".lease"))
          (let ((controls (cdar (niwa-session-persistence--control-artifacts
                                 (list entry)))))
            (should-error (niwa-session-persistence--discover-entry
                           entry 'portable controls nil))))
      (niwa-session-persistence-lock-release entry session)
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-persistence-choose-entry-remote ()
  ,test
  (test)
  :doc "batches startup retention and picker authority over TRAMP"
  (let* ((root (make-temp-file "niwa-picker-" t))
         (remote (concat "/niwamock:picker:" (file-name-as-directory root)))
         (niwa-session-max-age-days 7)
         (niwa-journal-enabled nil)
         (niwa-session-persistence--cleanup-throttle (make-hash-table :test #'equal))
         (run-program (symbol-function 'niwa-session-control-fs-run-program))
         (programs 0) candidates reserved)
    (unwind-protect
        (let ((workspace (test-niwa-session-persistence--make-workspace root)))
          ;; More than the carrier's optional read-archive limit of 32.
          (dotimes (i 33)
            (let ((session (niwa-session-create (format "session-%02d" i) workspace)))
              (with-temp-buffer
                (org-mode)
                (setq-local niwa--session session)
                (insert "Saved prompt\n")
                (niwa-session-artifacts-save session (current-buffer) t))
              (niwa-session-durability-lease-release (niwa-session-save-path session) session)
              (when (zerop i) (setq reserved session))))
          (let* ((lease-dir (file-name-concat (niwa-session-save-path reserved) ".lease"))
                 (head (niwa-session-durability--lease-head lease-dir))
                 (generation (plist-get head :generation))
                 (fence (niwa-session-transfer--fence-path lease-dir generation)))
            (make-directory (file-name-directory fence) t)
            (with-temp-file fence
              (prin1 (list :protocol-version 1 :generation generation
                           :request-id (make-string 32 ?b)
                           :session-id (niwa-session-session-id reserved)
                           :owner-client-id (plist-get head :client-id)
                           :requester-client-id (make-string 64 ?c)
                           :expires-at (+ (niwa-session-durability--target-time lease-dir) 60))
                     (current-buffer))))
          (niwa-test--with-local-shell-tramp '("picker")
            (let ((remote-workspace (niwa-workspace--create
                                     :type 'project :id "test-id" :root remote :name "picker")))
              ;; Stop at the real chooser's presentation boundary, before
              ;; a selection would need a fresh authority decision.
              (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                         (lambda (&rest args)
                           (when (car args) (cl-incf programs))
                           (apply run-program args)))
                        ((symbol-function 'completing-read)
                         (lambda (_ collection &rest _)
                           (setq candidates (all-completions "" collection))
                           (throw 'displayed nil))))
                (catch 'displayed (niwa-session-persistence-choose-entry remote-workspace)))
              (should (= 34 (length candidates)))
              (should (= 32 (cl-count-if (lambda (row) (string-prefix-p "Resume" row)) candidates)))
              (should (cl-find-if (lambda (row) (string-match-p "\\`Join +.*session-00" row)) candidates))
              (should (<= programs 8)))))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-persistence--format-incompatible-candidate ()
  ,test
  (test)
  :doc "renders age, size, reason, and absolute path in fixed columns"
  (cl-letf (((symbol-function 'niwa-session-rewind-format-relative-time)
             (lambda (_iso) "8 months")))
    (should
     (equal "8 months      size unavailable  missing: Sidecar is missing  /old/"
            (niwa-session-persistence--format-incompatible-candidate
             '(:status missing :diagnostic "Sidecar is missing"
               :updated-at "2026-01-01T00-00-00" :save-path "/old/"))))
    (should
     (equal "8 months      1.5M              unsupported version missing  /old/"
            (niwa-session-persistence--format-incompatible-candidate
             '(:status unsupported :size 1572864 :save-path "/old/"))))
    (should
     (string-suffix-p
      "obsolete: Sidecar lacks :session-id  /old/"
      (niwa-session-persistence--format-incompatible-candidate
       '(:status obsolete :diagnostic "Sidecar lacks :session-id"
         :save-path "/old/"))))))


(niwa-deftest niwa-session-persistence--observed-update-time ()
  ,test
  (test)
  :doc "prefers a parseable stamp, then the sidecar mtime, then the directory"
  (let* ((root (make-temp-file "niwa-update-time-" t))
         (sidecar (file-name-concat root "session.meta.el"))
         (stamp "2026-03-04T05-06-07"))
    (unwind-protect
        (progn
          (should (equal (niwa-session-persistence-parse-iso-time stamp)
                         (niwa-session-persistence--observed-update-time
                          stamp sidecar root)))
          (should (time-equal-p
                   (file-attribute-modification-time (file-attributes root))
                   (niwa-session-persistence--observed-update-time
                    "garbage" sidecar root)))
          (with-temp-file sidecar (insert "(:version \"v0.0.0\")"))
          (set-file-times sidecar (encode-time '(0 0 0 1 1 2020 nil nil t)))
          (should (time-equal-p
                   (encode-time '(0 0 0 1 1 2020 nil nil t))
                   (niwa-session-persistence--observed-update-time
                    nil sidecar root)))
          (should-not (niwa-session-persistence--observed-update-time
                       nil nil (file-name-concat root "absent"))))
      (delete-directory root t))))


(niwa-deftest niwa-session-persistence--enumerate-sessions ()
  ,test
  (test)
  :doc "keeps a current session listed beside an unpublished old one on disk"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let* ((session (niwa-session-create "good" workspace))
           (buffer (generate-new-buffer " *enumerate-mixed*"))
           (niwa-session-persistence--list-sessions-cache
            (make-hash-table :test #'equal))
           (niwa-session-persistence--summary-cache
            (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (insert "Current\n")
              (niwa-session-artifacts-save session buffer))
            (let* ((good (file-name-as-directory (niwa-session-save-path session)))
                   (old (file-name-as-directory
                         (file-name-concat
                          (niwa-session-artifacts-sessions-dir workspace) "old"))))
              (make-directory old)
              (niwa-session-codec-write
               (file-name-concat old "session.meta.el")
               '(:version "v0.5.3" :updated-at "2026-01-01T00-00-00"))
              (let* ((result (niwa-session-persistence--enumerate-sessions workspace))
                     (incompatible (car (plist-get result :incompatible))))
                (should (equal (list good)
                               (mapcar (lambda (item)
                                         (file-name-as-directory (plist-get item :save-path)))
                                       (plist-get result :sessions))))
                (should (= 1 (length (plist-get result :incompatible))))
                (should (equal old (file-name-as-directory
                                    (plist-get incompatible :save-path))))
                ;; A plain sidecar without a lease is an unmigrated session.
                (should (eq 'unsupported (plist-get incompatible :status)))
                (should (string-search "migrate-sessions.el"
                                       (plist-get incompatible :diagnostic))))))
        (test-niwa-session-persistence--release-and-kill buffer session)
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))


(niwa-deftest niwa-session-persistence/pre-lease-file-session (:quiet t)
  ,test
  (test)
  :doc "an unmigrated PID-lock file session points discovery and resume at migration"
  (dolist (lock '(nil t))
    (cl-destructuring-bind (workspace . tempdir)
        (test-niwa-session-persistence--make-tempdir-workspace)
      (unwind-protect
          (let* ((niwa-session-persistence--list-sessions-cache
                  (make-hash-table :test #'equal))
                 (old (file-name-as-directory
                       (file-name-concat
                        (niwa-session-artifacts-sessions-dir workspace) "old"))))
            (make-directory old t)
            (niwa-session-codec-write
             (file-name-concat old "session.meta.el")
             (test-niwa-session-persistence--complete-sidecar
              (list :session-id "old" :authority-mode 'pid-lock
                    :workspace (list :type 'file))))
            (write-region "* Old turn\n" nil
                          (file-name-concat old "segment-0001.chat.org") nil 'silent)
            (when lock
              (write-region "(:pid 1)" nil (file-name-concat old ".lock") nil 'silent))
            (let ((row (car (plist-get
                             (niwa-session-persistence--enumerate-sessions workspace)
                             :incompatible))))
              (should (eq 'unsupported (plist-get row :status)))
              (should (string-search "migrate-sessions.el" (plist-get row :diagnostic))))
            (should (string-search
                     "migrate-sessions.el"
                     (cadr (should-error
                            (niwa-session-persistence-restore old nil nil workspace)
                            :type 'user-error)))))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-session-persistence-list-sessions (:quiet t)
  ,test
  (test)
  :doc "lists materialized sessions, sorted newest-first"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (niwa-session-create "alpha" workspace))
               (b1 (generate-new-buffer "*test-session-alpha*"))
               (s2 (niwa-session-create "beta" workspace))
               (b2 (generate-new-buffer "*test-session-beta*")))
          (unwind-protect
              (niwa-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (insert "Hello\n")
                  (niwa-session-artifacts-save s1 b1))
                ;; Advance the stamps so `:updated-at' differs.
                (setq niwa-test--timestamp-offset 2)
                (with-current-buffer b2
                  (org-mode)
                  (insert "World\n")
                  (niwa-session-artifacts-save s2 b2))
                (let ((listed (niwa-session-persistence-list-sessions
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
            (test-niwa-session-persistence--release-and-kill b1 s1)
            (test-niwa-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)
      (clrhash niwa-session-persistence--list-sessions-cache)))
  :doc "reuses the last live enumeration only when asked"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (niwa-session-create "one" workspace))
               (b1 (generate-new-buffer "*test-session-one*"))
               (s2 (niwa-session-create "two" workspace))
               (b2 (generate-new-buffer "*test-session-two*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "First\n")
                  (niwa-session-artifacts-save s1 b1))
                (should (= 1 (length
                              (niwa-session-persistence-list-sessions
                               workspace))))
                (with-current-buffer b2
                  (org-mode)
                  (insert "Second\n")
                  (niwa-session-artifacts-save s2 b2))
                ;; The cached listing is as old as the last live one.
                (should (= 1 (length
                              (niwa-session-persistence-list-sessions
                               workspace 'cached))))
                (should (= 2 (length
                              (niwa-session-persistence-list-sessions
                               workspace))))
                ;; A live enumeration refreshes what cached readers see.
                (should (= 2 (length
                              (niwa-session-persistence-list-sessions
                               workspace 'cached)))))
            (test-niwa-session-persistence--release-and-kill b1 s1)
            (test-niwa-session-persistence--release-and-kill b2 s2)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)
      (clrhash niwa-session-persistence--list-sessions-cache)))
  :doc "returns nil for a workspace with no sessions"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (should (null (niwa-session-persistence-list-sessions workspace)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "remote listing omits nil or corrupt heads without reading transcript bytes"
  (let* ((host "list-publications")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-list-publications-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace _session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((sessions-dir
                    (niwa-session-artifacts-sessions-dir workspace))
                   (nil-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "nil-head")))
                   (corrupt-head
                    (file-name-as-directory
                     (file-name-concat sessions-dir "corrupt-head")))
                   (publication
                    (niwa-session-publication-read session-dir))
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
              (niwa-session-codec-write
               (niwa-session-artifacts-sidecar-path nil-head)
               (test-niwa-session-persistence--complete-sidecar
                '(:session-id "nil-head" :session-name "nil-head")))
              (copy-directory session-dir corrupt-head nil t t)
              (let* ((corrupt-manifest
                      (file-name-concat corrupt-head
                                        (file-relative-name manifest session-dir)))
                     (value
                      (niwa-session-codec-read corrupt-manifest))
                     (entry (car (plist-get value :artifacts))))
                (setf (plist-get (cdr entry) :published) "../escape")
                (niwa-session-codec-write corrupt-manifest value))
              (let ((listed
                     (niwa-session-persistence-list-sessions workspace)))
                (should (= 1 (length listed)))
                (should
                 (equal session-dir (plist-get (car listed) :save-path)))
                (should (plist-get (car listed) :publication))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "discovers a Save As child through alias B and resumes its full state"
  (let* ((alias-a "portable-resume-a")
         (alias-b "portable-resume-b")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-portable-resume-" t)))
         restored)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list alias-a alias-b)
          (cl-destructuring-bind (_workspace-a session-a session-dir segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               alias-a local-root "Portable transcript\n")
            (let* ((root-a
                    (niwa-workspace-root
                     (niwa-session-workspace session-a)))
                   (identity
                    (niwa-workspace-identity-read root-a))
                   (root-b
                    (format "/niwamock:%s:%s"
                            alias-b (file-name-as-directory local-root)))
                   session-id
                   (niwa-session-durability--client-id
                    (make-string 64 ?b))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session-a))
               t niwa-session-durability--disclosed-targets)
              (let ((buffer (generate-new-buffer " *save-as-alias*")))
                (unwind-protect
                    (progn
                      (should
                       (niwa-session-durability-lease-acquire
                        session-dir "save-as-alias" session-a))
                      (with-current-buffer buffer
                        (org-mode)
                        (setq-local niwa--session session-a
                                    buffer-file-name segment)
                        (insert "Portable transcript\n")
                        (cl-letf (((symbol-function 'read-string)
                                   (lambda (&rest _) "alias-clone")))
                          (niwa-save-session t)))
                      (setq session-id
                            (niwa-session-session-id session-a))
                      (niwa-session-persistence-lock-release
                       (niwa-session-save-path session-a) session-a))
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (set-buffer-modified-p nil)
                      (set-visited-file-name nil t)
                      (setq-local kill-buffer-hook nil))
                    (kill-buffer buffer))))
              (niwa-workspace-clear-registry)
              (let* ((workspace-b
                      (niwa-workspace-get-or-create
                       'project root-b root-b "portable-resume"))
                     (target-b (niwa-execution-target-create root-b))
                     (listed
                      (niwa-session-persistence-list-sessions workspace-b))
                     (child
                      (cl-find
                       session-id listed
                       :key (lambda (entry)
                              (plist-get (plist-get entry :summary)
                                         :session-id))
                       :test #'equal)))
                (should
                 (equal identity
                        (niwa-workspace-identity-read root-b)))
                (should (= 2 (length listed)))
                (should child)
                (should
                 (string-prefix-p root-b
                                  (plist-get child :save-path)))
                (puthash
                 (niwa-execution-target-identity target-b)
                 t niwa-session-durability--disclosed-targets)
                (cl-letf
                    (((symbol-function 'niwa--chat-buffer-init-common)
                      #'ignore)
                     ((symbol-function
                       'niwa-agent-persistence-restore-tree)
                      (lambda (&rest _) 0))
                     ((symbol-function
                       'niwa-session-artifacts-load-instructions)
                      #'ignore))
                  (setq restored
                        (niwa-session-persistence-resume-id
                         workspace-b session-id)))
                (should (buffer-live-p restored))
                (with-current-buffer restored
                  (should (string-match-p
                           "Portable transcript" (buffer-string)))
                  (should
                   (eq workspace-b
                       (niwa-session-workspace niwa--session)))
                  (should
                   (equal root-b
                          (niwa-workspace-root
                           (niwa-session-workspace niwa--session))))
                  (should
                   (equal root-b
                          (niwa-session-working-directory
                           niwa--session))))))))
      (when (buffer-live-p restored)
        (niwa-test--with-local-shell-tramp (list alias-a alias-b)
          (let ((session (buffer-local-value 'niwa--session restored)))
            (when (and session (niwa-session-save-path session))
              (niwa-session-persistence-lock-release
               (niwa-session-save-path session) session))
            (with-current-buffer restored
              (set-buffer-modified-p nil))
            (kill-buffer restored))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-conversation-variants ()
  ,test
  (test)
  :doc "finds the persisted Source and one direct Child from either variant"
  (let* ((root (make-temp-file "niwa-variants-" t))
         (workspace
          (test-niwa-session-persistence--make-workspace root))
         (source (niwa-session-create "source" workspace))
         (child (niwa-session-create "child" workspace))
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
          (setf (niwa-session-session-id source) "source-id"
                (niwa-session-save-path source) "/sessions/source/"
                (niwa-session-session-id child) "child-id"
                (niwa-session-save-path child) "/sessions/child/"
                (niwa-session-forked-from-session-id child) "source-id"
                (niwa-session-forked-from-fork-point-id child)
                "fork-point-1"
                (niwa-session-fork-type child) 'conversation)
          (cl-letf
              (((symbol-function
                 'niwa-session-persistence-list-sessions)
                (lambda (_workspace) entries)))
            (dolist (session (list source child))
              (let ((variants
                     (niwa-session-persistence-conversation-variants
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
                   (niwa-session-persistence-conversation-variants
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
                     (niwa-session-persistence-conversation-variants
                      child "fork-point-1"))))
            ;; Removing a sibling removes only that entry and the affordance
            ;; naturally disappears when the current session is alone.
            (setq entries (list child-entry))
            (should
             (equal '("child-id")
                    (mapcar
                     (lambda (entry)
                       (plist-get (plist-get entry :summary) :session-id))
                     (niwa-session-persistence-conversation-variants
                      child "fork-point-1"))))))
      (delete-directory root t))))


(niwa-deftest niwa-session-persistence-choose-conversation-variant ()
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
           (niwa-session-persistence-choose-conversation-variant
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
      (niwa-session-persistence-choose-conversation-variant
       (list source conversation worktree) "source-id"))
    (should (string-match-p "independent directory" (nth 1 displays)))))


(niwa-deftest niwa-session-persistence--read-summary ()
  ,test
  (test)
  :doc "reuses unchanged sidecars and refreshes atomically replaced files"
  (let ((tmp (make-temp-file "niwa-summary-cache-" nil ".el"))
        (niwa-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'niwa-session-codec-read))
        (read-count 0))
    (unwind-protect
        (cl-labels
            ((write-sidecar
              (name)
              (niwa-session-codec-write
               tmp
               (test-niwa-session-persistence--complete-sidecar
                `(:session-name ,name :session-id "cache-test")))))
          (cl-letf
              (((symbol-function 'niwa-session-codec-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (write-sidecar "first")
            (should
             (equal "first"
                    (plist-get
                     (plist-get
                      (niwa-session-persistence--read-summary tmp)
                      :summary)
                     :session-name)))
            (niwa-session-persistence--read-summary tmp)
            (should (= 1 read-count))
            (write-sidecar "second")
            (should
             (equal "second"
                    (plist-get
                     (plist-get
                      (niwa-session-persistence--read-summary tmp)
                      :summary)
                     :session-name)))
            (should (= 2 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "reuses a cached failure for an unchanged invalid sidecar"
  (let ((tmp (make-temp-file "niwa-summary-cache-invalid-" nil ".el"))
        (niwa-session-persistence--summary-cache
         (make-hash-table :test #'equal))
        (read-function
         (symbol-function 'niwa-session-codec-read))
        (read-count 0))
    (unwind-protect
        (progn
          (write-region "invalid" nil tmp nil 'silent)
          (cl-letf
              (((symbol-function 'niwa-session-codec-read)
                (lambda (path)
                  (cl-incf read-count)
                  (funcall read-function path))))
            (should
             (eq 'unreadable
                 (plist-get
                  (niwa-session-persistence--read-summary tmp) :status)))
            (should
             (eq 'unreadable
                 (plist-get
                  (niwa-session-persistence--read-summary tmp) :status)))
            (should (= 1 read-count))))
      (when (file-exists-p tmp) (delete-file tmp))))
  :doc "extracts only picker-relevant fields"
  (let ((tmp (make-temp-file "niwa-summary-test-" nil ".el")))
    (unwind-protect
        (progn
          (niwa-session-codec-write
           tmp
           (test-niwa-session-persistence--complete-sidecar
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
          (let ((s (plist-get
                    (niwa-session-persistence--read-summary tmp)
                    :summary)))
            (should (equal "demo" (plist-get s :session-name)))
            (should (equal "demo-1234" (plist-get s :session-id)))
            (should (equal "Hello" (plist-get s :first-user-message)))
            (should (equal "Latest" (plist-get s :latest-user-message)))
            (should (equal '("fork-point-1" "fork-point-2")
                           (plist-get s :fork-point-ids)))))
      (when (file-exists-p tmp) (delete-file tmp)))))


(niwa-deftest niwa-session-persistence--classify-sidecar ()
  ,test
  (test)
  :doc "classifies each rejection by processing stage and keeps current sidecars"
  (let ((tmp (make-temp-file "niwa-classify-" nil ".el")))
    (unwind-protect
        (progn
          (should
           (equal '(:status missing :diagnostic "Sidecar is missing")
                  (niwa-session-persistence--classify-sidecar
                   (file-name-concat temporary-file-directory "absent.el"))))
          (write-region "(" nil tmp nil 'silent)
          (should (eq 'unreadable
                      (plist-get
                       (niwa-session-persistence--classify-sidecar tmp)
                       :status)))
          (niwa-session-codec-write tmp '(1 2))
          (should
           (equal '(:status unreadable :diagnostic "Invalid session sidecar")
                  (niwa-session-persistence--classify-sidecar tmp)))
          (niwa-session-codec-write
           tmp '(:version "v0.5.3" :updated-at "2026-09-01T12-00-00"))
          (should
           (equal '(:status unsupported :version "v0.5.3"
                    :updated-at "2026-09-01T12-00-00"
                    :diagnostic "Unsupported session version: v0.5.3")
                  (niwa-session-persistence--classify-sidecar tmp)))
          (niwa-session-codec-write
           tmp (list :version niwa-session-codec-format-version))
          (let ((result (niwa-session-persistence--classify-sidecar tmp)))
            (should (eq 'obsolete (plist-get result :status)))
            (should (equal niwa-session-codec-format-version
                           (plist-get result :version)))
            (should (stringp (plist-get result :diagnostic))))
          (niwa-session-codec-write
           tmp (test-niwa-session-persistence--complete-sidecar
                '(:session-name "current" :session-id "current-1")))
          (let ((result (niwa-session-persistence--classify-sidecar tmp)))
            (should (eq 'current (plist-get result :status)))
            (should (equal "current-1"
                           (plist-get (plist-get result :sidecar)
                                      :session-id)))))
      (when (file-exists-p tmp) (delete-file tmp)))))


(niwa-deftest niwa-session-persistence--format-session-candidate ()
  ,test
  (test)
  :doc "prefers latest preview over first preview"
  (let ((display
         (niwa-session-persistence--format-session-candidate
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
         (niwa-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 1
                      :total-turn-count 1
                      :first-user-message "Original request")))))
    (should (string-match-p "Original request" display)))
  :doc "counts chats, not segments, and names earlier chats newest first"
  (let ((display
         (niwa-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "Background Review"
                      :updated-at "2026-04-23T12-00-00"
                      :current-segment 4
                      :total-turn-count 41
                      :latest-user-message "Please commit"
                      :chats (niwa-session-artifacts-chats
                              '((:segment 1 :title "Review queue timeout" :closed-at "t1")
                                (:segment 3 :title "Cursor Jump" :closed-at "t2"))
                              nil 4 "Background Review"))))))
    (should (string-match-p
             "Background Review  \\[3 chats, 41 turns\\]  ↩ Cursor Jump · Review queue timeout  Please commit\\'"
             display)))
  :doc "a session without clears is one chat with no earlier names"
  (let ((display
         (niwa-session-persistence--format-session-candidate
          (list :summary
                (list :session-name "demo" :updated-at "2026-04-23T12-00-00"
                      :current-segment 3 :total-turn-count 2
                      :latest-user-message "Hi"
                      :chats (niwa-session-artifacts-chats nil nil 3 "demo"))))))
    (should (string-match-p "demo  \\[1 chat, 2 turns\\]  Hi\\'" display))))


;;
;;; Phase 12: auto-cleanup

(niwa-deftest niwa-session-persistence-parse-iso-time ()
  ,test
  (test)
  :doc "parses our ISO-with-dashes format"
  (let ((time (niwa-session-persistence-parse-iso-time
               "2026-04-23T14-30-15")))
    (should time)
    (should (equal "2026-04-23T14-30-15"
                   (format-time-string "%FT%H-%M-%S" time))))
  :doc "returns nil for malformed input"
  (should (null (niwa-session-persistence-parse-iso-time "not a date")))
  (should (null (niwa-session-persistence-parse-iso-time nil))))


(niwa-deftest niwa-session-persistence-without-dedicated ()
  ,test
  (test)
  :doc "drops a conversation only when it and its artifact name each other"
  (cl-letf (((symbol-function 'niwa-artifact-store-dedicated-p)
             (lambda (_workspace session-id id)
               (equal (list session-id id) '("b" "board")))))
    (let ((a '(:summary (:session-id "a")))
          (b '(:summary (:session-id "b" :dedicated-artifact "board")))
          ;; A copied artifact does not name this one back.
          (c '(:summary (:session-id "c" :dedicated-artifact "board"))))
      (should (equal (list a c)
                     (niwa-session-persistence-without-dedicated
                      'workspace (list a b c)))))))

(niwa-deftest niwa-session-persistence-cleanup-expired (:quiet t)
  ,test
  (test)
  :doc "keeps a pinned file session until capture publication or discard"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *journal-pin-cleanup*"))
          (session (niwa-session-create "pinned" workspace))
          (niwa-session-max-age-days 7)
          (niwa-session-keep-recent-count nil)
          (niwa-session-persistence--cleanup-throttle (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (org-mode) (insert "Saved source\n"))
            (niwa-session-artifacts-save session buffer)
            (test-niwa-session-persistence--expire-session session)
            (let* ((directory (niwa-session-save-path session))
                   (capture (make-string 64 ?a)))
              (niwa-journal-pins-retain directory capture nil)
              ;; An open conversation is never swept; close it as a user would.
              (test-niwa-session-persistence--release-and-kill buffer session)
              (should (= 0 (test-niwa-session-persistence--cleanup-later workspace)))
              (should (file-directory-p directory))
              (niwa-journal-pins-release directory capture)
              (should (= 1 (test-niwa-session-persistence--cleanup-later workspace)))
              (should-not (file-directory-p directory))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))

  :doc "keeps an artifact's dedicated session while the artifact exists"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *dedicated-cleanup*"))
          (session (niwa-session-create "Artifact flow" workspace))
          (niwa-session-max-age-days 7)
          (niwa-session-keep-recent-count nil)
          (niwa-session-persistence--cleanup-throttle (make-hash-table :test #'equal)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (org-mode) (insert "Saved source\n"))
            (niwa-session-artifacts-save session buffer)
            (test-niwa-session-persistence--edit-sidecar
             session (lambda (metadata)
                       (plist-put metadata :dedicated-artifact "flow")))
            (test-niwa-session-persistence--expire-session session)
            (test-niwa-session-persistence--release-and-kill buffer session)
            (cl-letf (((symbol-function 'niwa-artifact-store-dedicated-p)
                       (lambda (_workspace session-id id)
                         (equal (list session-id id)
                                (list (niwa-session-session-id session) "flow")))))
              (should (= 0 (test-niwa-session-persistence--cleanup-later workspace))))
            (should (file-directory-p (niwa-session-save-path session)))
            (should (= 1 (test-niwa-session-persistence--cleanup-later workspace))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))

  :doc "deletes sessions older than the cap and removes them from the picker scan"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               ;; Reset the throttle so tests don't leak.
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s1 (niwa-session-create "old" workspace))
               (b1 (generate-new-buffer "*test-old-buf*"))
               (s2 (niwa-session-create "new" workspace))
               (b2 (generate-new-buffer "*test-new-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Old\n")
                  (niwa-session-artifacts-save s1 b1))
                (with-current-buffer b2
                  (org-mode)
                  (insert "New\n")
                  (niwa-session-artifacts-save s2 b2))
                (let ((archive
                       (file-name-concat
                        (niwa-session-save-path s1)
                        "agents/old.compact-0001.chat.org")))
                  (make-directory (file-name-directory archive) t)
                  (write-region "recovery archive\n" nil archive nil 'silent)
                  (should (file-exists-p archive)))
                (test-niwa-session-persistence--expire-session s1)
                ;; Expiry must come from the publication, not its fixed cache.
                (delete-file (niwa-session-artifacts-sidecar-path
                              (niwa-session-save-path s1)))
                ;; Close both so cleanup may delete the expired one.
                (test-niwa-session-persistence--release-and-kill b1 s1)
                (test-niwa-session-persistence--release-and-kill b2 s2)
                (let* ((scan (niwa-session-persistence--scan workspace "\\`[^.]"))
                       (deleted
                        (test-niwa-session-persistence--call-later
                         (lambda ()
                           (niwa-session-persistence-cleanup-expired workspace t scan)))))
                  (should (= 1 deleted))
                  (should-not (file-directory-p
                               (niwa-session-save-path s1)))
                  (should (file-directory-p
                           (niwa-session-save-path s2)))
                  (let ((rows (niwa-session-persistence--enumerate-sessions
                               workspace nil scan)))
                    (should-not (plist-get rows :incompatible))
                    (should (equal (mapcar (lambda (row) (plist-get row :save-path))
                                           (plist-get rows :sessions))
                                   (list (niwa-session-save-path s2)))))))
            (when (buffer-live-p b1)
              (with-current-buffer b1 (set-buffer-modified-p nil))
              (kill-buffer b1))
            (when (buffer-live-p b2)
              (with-current-buffer b2 (set-buffer-modified-p nil))
              (kill-buffer b2))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "deletes expired sessions with obsolete sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (niwa-session-create "obsolete" workspace))
               (buf (generate-new-buffer "*test-obsolete-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (niwa-session-artifacts-save session buf))
                (test-niwa-session-persistence--expire-session session)
                (let ((save-path (niwa-session-save-path session)))
                  (test-niwa-session-persistence--release-and-kill buf session)
                  ;; A fresh publisher has no current checkpoint projection
                  ;; that would reject this intentionally obsolete fixture.
                  (test-niwa-session-persistence--edit-published-sidecar
                   workspace save-path
                   (lambda (metadata)
                     (cl-remf metadata :plan-mode)
                     metadata))
                  (should (= 1
                             (test-niwa-session-persistence--cleanup-later
                              workspace)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "deletes expired sessions without sidecars"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (niwa-session-create "missing" workspace))
               (buf (generate-new-buffer "*test-missing-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old\n")
                  (niwa-session-artifacts-save session buf))
                (let* ((save-path (niwa-session-save-path session))
                       (sidecar
                        (plist-get (niwa-session-publication-read save-path)
                                   :sidecar))
                       (old-time
                        (time-subtract (current-time) (* 14 24 60 60))))
                  (niwa-session-persistence-lock-release save-path session)
                  (delete-file sidecar)
                  (delete-file (niwa-session-artifacts-sidecar-path save-path))
                  (set-file-times save-path old-time)
                  (should (= 1
                             (test-niwa-session-persistence--cleanup-later
                              workspace)))
                  (should-not (file-directory-p save-path))))
            (when (buffer-live-p buf)
              (with-current-buffer buf (set-buffer-modified-p nil))
              (kill-buffer buf))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "skips locked sessions even when expired"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s (niwa-session-create "stuck" workspace))
               (b (generate-new-buffer "*test-stuck-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (org-mode)
                  (insert "Hi\n")
                  (niwa-session-artifacts-save s b))
                (test-niwa-session-persistence--expire-session s 30)
                ;; The open conversation still holds its live lease.
                (let ((deleted
                       (niwa-session-persistence-cleanup-expired
                        workspace t)))
                  (should (= 0 deleted))
                  (should (file-directory-p
                           (niwa-session-save-path s)))))
            (when (buffer-live-p b)
              (with-current-buffer b (set-buffer-modified-p nil))
              (kill-buffer b))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "never deletes expired remote sessions with an active lease"
  (let* ((host "cleanup-remote-lease-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-cleanup-remote-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let ((niwa-session-max-age-days 7)
                  (niwa-session-keep-recent-count nil)
                  (niwa-session-persistence--cleanup-throttle
                   (make-hash-table :test #'equal))
                  (niwa-session-durability--client-id
                   (make-string 64 ?a))
                  (niwa-session-durability--disclosed-targets
                   (make-hash-table :test #'equal)))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (test-niwa-session-persistence--edit-published-sidecar
               workspace session-dir
               (lambda (sidecar)
                 (plist-put sidecar :updated-at
                            (format-time-string
                             "%FT%H-%M-%S"
                             (time-subtract (current-time) (* 30 24 60 60))))))
              (unwind-protect
                  (progn
                    (should
                     (niwa-session-durability-lease-acquire
                      session-dir "*cleanup-owner*" session))
                    (should
                     (eql 0 (niwa-session-persistence-cleanup-expired
                             workspace t)))
                    (should (file-directory-p session-dir))
                    (should
                     (niwa-session-durability-lease-owned-p session)))
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "deletes an abandoned portable session once its lease renewal also expired"
  (let* ((host "cleanup-remote-abandoned-host")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-cleanup-abandoned-" t))))
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (cl-destructuring-bind (workspace session session-dir _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture
               host local-root "Published transcript\n")
            (let* ((niwa-session-max-age-days 7)
                   (niwa-session-durability--client-id (make-string 64 ?a))
                   (niwa-session-durability--disclosed-targets
                    (make-hash-table :test #'equal))
                   (lease-dir (niwa-session-durability--lease-path session-dir))
                   (later (+ (niwa-session-durability--target-time lease-dir)
                             (* 30 24 60 60)))
                   (live nil)
                   (cleanup
                    (lambda (keep)
                      (let ((niwa-session-keep-recent-count keep)
                            (niwa-session-persistence--cleanup-throttle
                             (make-hash-table :test #'equal)))
                        (niwa-session-persistence-cleanup-expired workspace t)))))
              (puthash
               (niwa-execution-target-identity
                (niwa-session-execution-target session))
               t niwa-session-durability--disclosed-targets)
              (test-niwa-session-persistence--edit-published-sidecar
               workspace session-dir
               (lambda (sidecar)
                 (plist-put sidecar :updated-at
                            (format-time-string
                             "%FT%H-%M-%S"
                             (time-subtract (current-time) (* 30 24 60 60))))))
              ;; The fixture's release renewed the lease just now.
              (should (eql 0 (funcall cleanup nil)))
              (cl-letf (((symbol-function 'niwa-session-durability--target-time)
                         (lambda (&optional _) later))
                        ((symbol-function 'niwa-session-control-transfer-root-buffer-for-id)
                         (lambda (_) live)))
                (should (eql 0 (funcall cleanup 1)))
                (setq live (current-buffer))
                (should (eql 0 (funcall cleanup nil)))
                (should (file-directory-p session-dir))
                (setq live nil)
                (should (eql 1 (funcall cleanup nil))))
              (should-not (file-directory-p session-dir)))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry)))
  :doc "keeps the newest sessions regardless of age"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count 1)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (s1 (niwa-session-create "older" workspace))
               (b1 (generate-new-buffer "*test-older-buf*"))
               (s2 (niwa-session-create "newer" workspace))
               (b2 (generate-new-buffer "*test-newer-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer b1
                  (org-mode)
                  (insert "Older\n")
                  (niwa-session-artifacts-save s1 b1))
                (with-current-buffer b2
                  (org-mode)
                  (insert "Newer\n")
                  (niwa-session-artifacts-save s2 b2))
                ;; Both sessions are far past the cap; s2 is the newer one.
                (dolist (spec (list (cons s1 28) (cons s2 14)))
                  (test-niwa-session-persistence--expire-session
                   (car spec) (cdr spec)))
                (test-niwa-session-persistence--release-and-kill b1 s1)
                (test-niwa-session-persistence--release-and-kill b2 s2)
                (let ((deleted
                       (test-niwa-session-persistence--cleanup-later
                        workspace)))
                  (should (= 1 deleted))
                  (should-not (file-directory-p
                               (niwa-session-save-path s1)))
                  (should (file-directory-p
                           (niwa-session-save-path s2)))))
            (when (buffer-live-p b1)
              (with-current-buffer b1 (set-buffer-modified-p nil))
              (kill-buffer b1))
            (when (buffer-live-p b2)
              (with-current-buffer b2 (set-buffer-modified-p nil))
              (kill-buffer b2))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "no-op when cap is nil"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let ((niwa-session-max-age-days nil))
          (should (null (niwa-session-persistence-cleanup-expired
                         workspace t))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "throttled to one run per workspace within the sweep interval"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal)))
          ;; First call returns 0 (no sessions); second call (no force) returns nil.
          (should (= 0 (niwa-session-persistence-cleanup-expired
                        workspace)))
          (should (null (niwa-session-persistence-cleanup-expired
                         workspace))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "a long-running Emacs sweeps again once the interval has passed"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-persistence--cleanup-interval 60)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (key (cons (niwa-workspace-type workspace)
                          (niwa-workspace-id workspace))))
          (puthash key (- (float-time) 61)
                   niwa-session-persistence--cleanup-throttle)
          (should (= 0 (niwa-session-persistence-cleanup-expired
                        workspace)))
          (should (< (- (float-time)
                        (gethash key
                                 niwa-session-persistence--cleanup-throttle))
                     5)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-persistence--cleanup-due-p ()
  ,test
  (test)
  :doc "a workspace never swept in this Emacs is due"
  (let ((niwa-session-persistence--cleanup-throttle
         (make-hash-table :test #'equal))
        (workspace (niwa-workspace--create :type 'file :id "due")))
    (should (niwa-session-persistence--cleanup-due-p workspace)))
  :doc "a recent sweep is not due until the interval has passed"
  (let ((niwa-session-persistence--cleanup-throttle
         (make-hash-table :test #'equal))
        (niwa-session-persistence--cleanup-interval 60)
        (workspace (niwa-workspace--create :type 'file :id "due")))
    (puthash '(file . "due") (- (float-time) 30)
             niwa-session-persistence--cleanup-throttle)
    (should-not (niwa-session-persistence--cleanup-due-p workspace))
    (puthash '(file . "due") (- (float-time) 90)
             niwa-session-persistence--cleanup-throttle)
    (should (niwa-session-persistence--cleanup-due-p workspace))))

(niwa-deftest niwa-session-persistence-schedule-cleanup (:quiet t)
  ,test
  (test)
  :doc "sweeps a due workspace once Emacs is idle, then stops offering"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 7)
               (niwa-session-keep-recent-count nil)
               (noninteractive nil)
               (niwa-transport--background-resume-at 0)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (session (niwa-session-create "expired" workspace))
               (buf (generate-new-buffer " *test-expired*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Old session\n")
                  (niwa-session-artifacts-save session buf))
                (let ((save-path (niwa-session-save-path session)))
                  (test-niwa-session-persistence--expire-session session)
                  (test-niwa-session-persistence--release-and-kill buf session)
                  (should (timerp (niwa-session-persistence-schedule-cleanup
                                   workspace)))
                  ;; Nothing runs inside the command that offered it.
                  (should (file-directory-p save-path))
                  (test-niwa-session-persistence--call-later
                   (lambda ()
                     (with-timeout (2 (ert-fail "Cleanup remained queued"))
                       (while (> (hash-table-count
                                  niwa-session-persistence--cleanups)
                                 0)
                         (sleep-for .002)))))
                  (should-not (file-directory-p save-path))
                  (should-not (niwa-session-persistence-schedule-cleanup
                               workspace))))
            (niwa-transport-cancel-idle
             niwa-session-persistence--cleanups 'session-cleanup)
            (test-niwa-session-persistence--release-and-kill buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))
  :doc "a batch Emacs never sweeps from session activity"
  (let ((niwa-session-max-age-days 7)
        (noninteractive t)
        (niwa-session-persistence--cleanup-throttle
         (make-hash-table :test #'equal)))
    (should-not (niwa-session-persistence-schedule-cleanup
                 (niwa-workspace--create :type 'file :id "batch")))
    (should (zerop (hash-table-count niwa-session-persistence--cleanups))))
  :doc "disabled expiry queues nothing"
  (let ((niwa-session-max-age-days nil)
        (noninteractive nil))
    (should-not (niwa-session-persistence-schedule-cleanup
                 (niwa-workspace--create :type 'file :id "off")))))

(niwa-deftest niwa-session-persistence-delete (:quiet t)
  ,test
  (test)
  :doc "deletes a fresh session only once nothing holds it"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (let ((buffer (generate-new-buffer " *delete-session*"))
          (session (niwa-session-create "doomed" workspace))
          (capture (make-string 64 ?a)))
      (unwind-protect
          (progn
            (with-current-buffer buffer (org-mode) (insert "Saved\n"))
            (niwa-session-artifacts-save session buffer)
            (let ((directory (niwa-session-save-path session)))
              ;; This Emacs still holds its lock.
              (should-not (niwa-session-persistence-delete
                           workspace directory))
              (niwa-session-persistence-lock-release directory session)
              (niwa-journal-pins-retain directory capture nil)
              (should-not (niwa-session-persistence-delete
                           workspace directory))
              (should (file-directory-p directory))
              (niwa-journal-pins-release directory capture)
              (should (niwa-session-persistence-delete
                       workspace directory))
              (should-not (file-directory-p directory))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry)))))


;;
;;; View rerender on resume / rewind

(niwa-deftest niwa-session-persistence--hydrate-restored-buffer ()
  ,test
  (test)
  :doc "plants session state after Org setup and before transcript restoration"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-restore-hydrate-" t)))
         (workspace (test-niwa-session-persistence--make-workspace root))
         (session (niwa-session-create "main" workspace root))
         (buffer (generate-new-buffer " *niwa-restore-hydrate*"))
         (segment-path (file-name-concat root "segment-0001.chat.org"))
         events)
    (unwind-protect
        (cl-letf (((symbol-function
                    'niwa-transcript-restore-gptel-state)
                   (lambda ()
                     (should (derived-mode-p 'org-mode))
                     (should (eq niwa--session session))
                     (should (eq niwa--workspace workspace))
                     (push 'transcript events)))
                  ((symbol-function
                    'niwa-tool-render-data-reconcile-lost-executions)
                   (lambda (_buffer &rest _) 0))
                  ((symbol-function
                    'niwa-session-artifacts-check-target-incarnation)
                   (lambda (checked-session checked-buffer)
                     (should (eq checked-session session))
                     (should (eq checked-buffer buffer))
                     (push 'incarnation events)))
                  ((symbol-function 'niwa--chat-buffer-init-common)
                   (lambda (_buffer _workspace source &optional inspection-p)
                     (should (equal source "resume"))
                     (should-not inspection-p)
                     (push 'chat events)))
                  ((symbol-function
                    'niwa-agent-persistence-restore-tree)
                   (lambda (_session _buffer read-only-p)
                     (should-not read-only-p)
                     (push 'agents events)
                     2))
                  ((symbol-function
                    'niwa-session-artifacts-load-instructions)
                   (lambda (_session)
                     (push 'instructions events))))
          (should
           (= 2
              (niwa-session-persistence--hydrate-restored-buffer
               buffer session workspace segment-path t nil nil)))
          (should (equal '(transcript incarnation chat agents instructions)
                         (nreverse events)))
          (with-current-buffer buffer
            (should (equal root default-directory))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))

  :doc "hydration repairs persist current bounds and no request config"
  ;; Hydration runs before chat setup
  ;; installs niwa's gptel save-state advice, as on a cold first resume,
  ;; so a repair must not write gptel's request configuration to the drawer.
  (dolist (ptc-p '(nil t))
    (let* ((advised (advice-member-p
                     #'niwa-session-artifacts--save-gptel-state-around
                     'gptel--save-state))
           (root (file-name-as-directory (make-temp-file "hydrate-bounds-" t)))
           (workspace (test-niwa-session-persistence--make-file-workspace root))
           (session (niwa-session-create "main" workspace root))
           (buffer (generate-new-buffer " *hydrate-bounds*"))
           (path (file-name-concat root "segment-0001.chat.org"))
           original artifact)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (org-mode)
              (insert "Prompt\n")
              (insert (propertize
                       (concat "#+begin_tool\n(:name \"Bash\" :args (:command \"true\"))\n\noutput\n"
                               (niwa-tool-render-data-format
                                '(:kind bash :state running :execution-id "hydrate-exec"
                                        :live-execution-p t :execution-output "output")
                                "hydrate-bash")
                               "#+end_tool\n")
                       'gptel '(tool . "hydrate-bash")))
              (insert (niwa--format-hook-audit-record
                       '(:type tool-result-rewrite :event "PostToolUse"
                               :tool-name "Bash" :reason "retained audit evidence")))
              (insert (propertize
                       "#+begin_tool\n(:name \"ToolSearch\" :args (:query \"agents\"))\n\nfound\n#+end_tool\n"
                       'gptel '(tool . "hydrate-search")))
              (insert (propertize "Following response.\n" 'gptel 'response))
              (niwa-session-artifacts-stabilize-gptel-bounds)
              (setq original (buffer-string))
              (write-region nil nil path nil 'silent)
              (setq buffer-file-name path))
            (advice-remove 'gptel--save-state
                           #'niwa-session-artifacts--save-gptel-state-around)
            (cl-letf (((symbol-function 'niwa-session-artifacts-check-target-incarnation)
                       #'ignore)
                      ((symbol-function 'niwa-ptc-checkpoint-reconcile)
                       (lambda (_session)
                         (if (not ptc-p) '(0 . 0)
                           ;; A recovered ToolCall result shifts later spans.
                           (goto-char (point-min))
                           (search-forward "Prompt\n")
                           (insert (propertize
                                    "#+begin_tool\n(:name \"ToolCall\" :args (:expression \"nil\"))\n\nnil\n#+end_tool\n"
                                    'gptel '(tool . "hydrate-ptc")))
                           '(1 . 0))))
                      ((symbol-function 'niwa--chat-buffer-init-common) #'ignore)
                      ((symbol-function 'niwa-agent-persistence-restore-tree)
                       (lambda (&rest _) 0))
                      ((symbol-function 'niwa-session-artifacts-load-instructions) #'ignore))
              (niwa-session-persistence--hydrate-restored-buffer
               buffer session workspace path t nil "resume"
               (lambda (value) (setq artifact value))))
            (should (equal path (plist-get artifact :path)))
            (with-temp-buffer
              (insert-file-contents path)
              (should (equal original (buffer-string))))
            (with-current-buffer buffer
              (should (equal (alist-get 'tool
                                        (read (org-entry-get (point-min) "GPTEL_BOUNDS")))
                             (alist-get 'tool (gptel--get-buffer-bounds)))))
            (with-temp-buffer
              (insert (plist-get artifact :content))
              (let ((case-fold-search t) properties)
                (goto-char (point-min))
                (while (re-search-forward "^[ \t]*:\\(GPTEL_[^:]+\\):" nil t)
                  (push (upcase (match-string 1)) properties))
                (should (equal '("GPTEL_BOUNDS") properties)))
              (org-mode)
              (niwa-transcript-restore-properties)
              (should (eq 'lost (plist-get
                                (niwa-tool-render-data-for-tool
                                 (current-buffer) "hydrate-bash") :state)))
              (let ((ranges (alist-get 'tool
                                       (read (org-entry-get (point-min) "GPTEL_BOUNDS")))))
                (should (= (if ptc-p 3 2) (length ranges)))
                (dolist (range ranges)
                  (goto-char (car range))
                  (should (looking-at-p "(:name "))))
              (should (= 1 (length (niwa-transcript-audit-records (buffer-string)))))
              (let ((gptel--known-backends nil)
                    (gptel-model 'gpt-4o))
                (goto-char (point-max))
                (should (gptel--parse-buffer
                         (gptel-make-openai "hydrate-bounds" :key "unused") nil)))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer))
        (when advised
          (niwa-session-artifacts-install-gptel-save-state-advice))
        (delete-directory root t)
        (niwa-workspace-clear-registry)))))

(niwa-deftest niwa-session-persistence--finish-restored-buffer ()
  ,test
  (test)
  :doc "persists repairs and loads history before rendering a fresh buffer"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-restore-finish-" t)))
         (workspace
          (test-niwa-session-persistence--make-file-workspace root))
         (session (niwa-session-create "main" workspace root))
         (buffer (generate-new-buffer " *niwa-restore-finish-data*"))
         observer
         events)
    (setf (niwa-session-save-path session) root)
    (unwind-protect
        (progn
          (setq observer
                (niwa-session-control-transfer-register-observer
                 session
                 (lambda (event &rest _args)
                   (pcase event
                     ('load-history (push 'history events))
                     ('rerender (push 'render events))))))
          (cl-letf (((symbol-function
                      'niwa-session-artifacts-build-sidecar)
                     (lambda (_session _buffer &optional _checkpoint) '(:sidecar t)))
                    ((symbol-function 'niwa-session-publication-publish)
                     (lambda (&rest _) (push 'write events))))
            (should
             (eq buffer
                 (niwa-session-persistence--finish-restored-buffer
                  buffer session nil t))))
          (should (equal '(write history render) (nreverse events))))
      (niwa-session-control-transfer-unregister-observer session observer)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-find-live-buffer ()
  ,test
  (test)
  :doc "finds only root data buffers, never agent or view projections"
  (let* ((session (niwa-session--create
                   :name "source" :session-id "source-id"))
         (root-buffer (generate-new-buffer " *source-root*"))
         (view-buffer (generate-new-buffer " *source-view*"))
         (agent-buffer (generate-new-buffer " *source-agent*")))
    (unwind-protect
        (progn
          (with-current-buffer root-buffer
            (setq-local niwa--session session))
          (with-current-buffer view-buffer
            (setq-local niwa--session session)
            (setq-local niwa--data-buffer root-buffer))
          (with-current-buffer agent-buffer
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation t))
          (niwa-session-set-root-buffer session root-buffer)
          (should (eq root-buffer
                      (niwa-session-persistence-find-live-buffer
                       "source-id" " *source-root*")))
          (kill-buffer root-buffer)
          (should-not
           (niwa-session-persistence-find-live-buffer
            "source-id" " *source-root*")))
      (dolist (buffer (list agent-buffer view-buffer root-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))



(niwa-deftest niwa-session-persistence/view-rerender ()
  ,test
  (test)
  :doc "save path does not rebuild the visible transcript"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (vb      (generate-new-buffer "*test-view-buf*"))
               (rerender-count 0))
          (unwind-protect
              (with-current-buffer buf
                (org-mode)
                (setq-local niwa--view-buffer vb)
                (with-current-buffer vb
                  (setq-local niwa--data-buffer buf))
                (insert "prompt before save\n")
                (cl-letf (((symbol-function 'niwa-view--full-rerender)
                           (lambda () (cl-incf rerender-count)))
                          ((symbol-function 'niwa-view-rerender)
                           (lambda (&rest _) (cl-incf rerender-count))))
                  (niwa-session-artifacts-save session buf))
                (should (= rerender-count 0)))
            (when (buffer-live-p vb) (kill-buffer vb))
            (test-niwa-session-persistence--release-and-kill
             buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))

  :doc "resume path rerenders and exposes the restored root history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               (rerender-count 0)
               session-dir restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from resume test\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (cl-letf (((symbol-function 'niwa-view-rerender)
                           (lambda (&rest _) (cl-incf rerender-count))))
                  (setq restored
                        (niwa-session-persistence-restore session-dir)))
                (should (buffer-live-p restored))
                (let* ((restored-session
                        (buffer-local-value 'niwa--session restored))
                       (history
                        (niwa-resource-execute
                         (niwa-resource-prepare
                          'read "history://root"
                          (list :session restored-session)))))
                  (should (eq restored
                              (niwa-session-root-buffer restored-session)))
                  (should (string-match-p "hello from resume test"
                                          (plist-get history :result))))
                ;; The resume schedules its rerender; init-common's
                ;; view-ensure flow may request another.  We only care
                ;; that one is requested.
                (should (>= rerender-count 1)))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'niwa--session restored)))))
	  (delete-directory tempdir t)
	  (niwa-workspace-clear-registry)))

  :doc "resume path renders persisted hook audit records"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setq-local gptel-response-separator "\n\n")
                  (insert "\n\nrewritten prompt")
                  (insert
                   (niwa--format-hook-audit-record
                    '(:type prompt-rewrite
                      :event "UserPromptSubmit"
                      :original "original prompt"
                      :submitted "rewritten prompt")))
                  (insert "\n")
                  (niwa-session-artifacts-stabilize-gptel-bounds)
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (niwa-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'niwa--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (niwa-view--full-rerender)
                  (let ((text (buffer-substring-no-properties
                               (point-min) (niwa-view-transcript-tail))))
                    (should (string-match-p "hook changed prompt" text))
                    (should (string-match-p "rewritten prompt" text))
                    (should-not (string-match-p "original prompt" text)))
                  (goto-char (point-min))
                  (search-forward "hook changed prompt")
                  (niwa-view-toggle-section)
                  (let ((expanded (buffer-substring-no-properties
                                   (point-min) (niwa-view-transcript-tail))))
                    (should (string-match-p "original prompt" expanded)))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))

  :doc "resume path restores view input history"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               session-dir restored view)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello from history resume test\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (let ((history-path
                       (file-name-concat tempdir ".niwa/input-history.el")))
                  (make-directory (file-name-directory history-path) t)
                  (niwa-session-codec-write
                   history-path
                   '(:version 2 :entries ("second" "first"))))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (setq restored
                      (niwa-session-persistence-restore session-dir))
                (setq view
                      (buffer-local-value 'niwa--view-buffer restored))
                (should (buffer-live-p view))
                (with-current-buffer view
                  (should (equal '("second" "first")
                                 (niwa-view-history--entries)))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))

  :doc "session chooser displays the companion view buffer"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*"))
               restored)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (setf (niwa-session-preset-name session) 'test-preset)
                  (insert "hello from resume display test\n")
                  (niwa-session-artifacts-save session buf))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (save-window-excursion
                  (let ((default-directory tempdir)
                        (transcript-window (selected-window))
                        (gptel-display-buffer-action '(display-buffer-same-window)))
                    (cl-letf (((symbol-function 'niwa-workspace)
                               (lambda (&optional _arg) workspace))
                              ((symbol-function
                                'niwa--ordered-completion-table)
                               (lambda (values &rest _) values))
                              ;; The first candidate starts a new session; the
                              ;; second is the persisted one under test.
                              ((symbol-function 'completing-read)
                               (lambda (_prompt collection &rest _)
                                 (cadr collection))))
                      (let ((niwa-preset--registry
                             '((test-preset :parents nil :settings nil)))
                            (gptel--known-presets
                             (cons '(test-preset :description "Test")
                                   gptel--known-presets)))
                        (niwa)))
                    (let ((displayed (window-buffer transcript-window)))
                      (setq restored
                            (buffer-local-value 'niwa--data-buffer displayed))
                      (should (buffer-live-p restored))
                      (should (eq displayed (buffer-local-value 'niwa--view-buffer restored)))
                      (let ((composer (should (niwa-view-composer-window transcript-window))))
                        (should (eq (selected-window) composer))
                        (should (eq (window-buffer composer)
                                    (niwa-view-composer-buffer displayed))))))))
            (test-niwa-session-persistence--release-and-kill
             buf session)
            (test-niwa-session-persistence--release-and-kill
             restored
             (and restored
                  (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))


;;
;;; Sidecar failure during restore

(niwa-deftest niwa-session-persistence/sidecar-missing-on-restore (:quiet t)
  ,test
  (test)
  :doc "deleted sidecar fails closed with chooser-supplied authority"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                ;; The published sidecar is the authority, not the cache.
                (delete-file
                 (plist-get (niwa-session-publication-read session-dir)
                            :sidecar))
                (should (string-match-p
                         "Invalid session publication sidecar"
                         (cadr (should-error
                                (niwa-session-persistence-restore
                                 session-dir nil nil workspace))))))
            (ignore-errors
              (test-niwa-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))
  :doc "corrupt sidecar fails closed with chooser-supplied authority"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((session (niwa-session-create "main" workspace))
               (buf (generate-new-buffer "*test-data-buf*"))
               session-dir)
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "Some content\n")
                  (niwa-session-artifacts-save session buf))
                (setq session-dir (niwa-session-save-path session))
                (test-niwa-session-persistence--release-and-kill
                 buf session)
                (setq buf nil)
                (write-region "this is not a plist" nil
                              (plist-get (niwa-session-publication-read
                                          session-dir)
                                         :sidecar)
                              nil 'silent)
                (should (string-match-p
                         "Invalid session publication sidecar"
                         (cadr (should-error
                                (niwa-session-persistence-restore
                                 session-dir nil nil workspace))))))
            (ignore-errors
              (test-niwa-session-persistence--release-and-kill
               buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry)))))


;;
;;; Foreign-holder cleanup behavior

(niwa-deftest niwa-session-persistence/cleanup-foreign-lease ()
  ,test
  (test)
  :doc "a foreign client's live lease protects expired metadata without mutation"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((niwa-session-max-age-days 1)
               (niwa-session-keep-recent-count nil)
               (niwa-session-persistence--cleanup-throttle
                (make-hash-table :test #'equal))
               (niwa-session-durability--client-id (make-string 64 ?a))
               (session (niwa-session-create "main" workspace))
               (buf     (generate-new-buffer "*test-data-buf*")))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-mode)
                  (insert "hello\n")
                  (niwa-session-artifacts-save session buf))
                (test-niwa-session-persistence--expire-session session 7)
                (let* ((save-path (niwa-session-save-path session))
                       (niwa-session-durability--client-id (make-string 64 ?b))
                       (lease (niwa-session-durability-lease-status save-path))
                       (head (niwa-session-durability-publication-head save-path)))
                  (should (eq 'foreign (plist-get lease :state)))
                  (let ((deleted (niwa-session-persistence-cleanup-expired
                                  workspace t)))
                    (should (= 0 deleted))
                    (should (file-directory-p save-path))
                    (should (equal lease
                                   (niwa-session-durability-lease-status save-path)))
                    (should (equal head
                                   (niwa-session-durability-publication-head
                                    save-path))))))
            (test-niwa-session-persistence--release-and-kill buf session)))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))


;;
;;; Same-name sessions in one workspace

(niwa-deftest niwa-session-persistence/same-name-sessions ()
  ,test
  (test)
  :doc "restore resolves the right session-id when two sessions share a name"
  (cl-destructuring-bind (workspace . tempdir)
      (test-niwa-session-persistence--make-tempdir-workspace)
    (unwind-protect
        (let* ((s1 (niwa-session-create "main" workspace))
               (b1 (generate-new-buffer "*test-data-1*"))
               (s2 (niwa-session-create "main" workspace))
               (b2 (generate-new-buffer "*test-data-2*"))
               restored)
          (unwind-protect
              (niwa-test--with-shifted-clock
                (with-current-buffer b1
                  (org-mode)
                  (setq-local niwa--session s1)
                  (insert "session one\n")
                  (niwa-session-artifacts-save s1 b1))
                ;; Advance the stamps so the derived session ids differ.
                (setq niwa-test--timestamp-offset 120)
                (with-current-buffer b2
                  (org-mode)
                  (setq-local niwa--session s2)
                  (insert "session two\n")
                  (niwa-session-artifacts-save s2 b2))
                (should-not (equal (niwa-session-session-id s1)
                                   (niwa-session-session-id s2)))
                ;; Both buffers share the default
                ;; `*niwa:main@...*' buffer name (identical session
                ;; name + workspace).  Restore must match session-id,
                ;; not just the buffer name, and return b1 when asked
                ;; to resume s1's dir.
                (setq restored
                      (niwa-session-persistence-restore
                       (niwa-session-save-path s1)))
                (should (buffer-live-p restored))
                (should (eq restored b1))
                (with-current-buffer restored
                  (should (equal (niwa-session-session-id s1)
                                 (niwa-session-session-id niwa--session)))))
            (test-niwa-session-persistence--release-and-kill b1 s1)
            (test-niwa-session-persistence--release-and-kill b2 s2)
            (when (and restored (buffer-live-p restored))
              (test-niwa-session-persistence--release-and-kill
               restored
               (buffer-local-value 'niwa--session restored)))))
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))


;;
;;; Session-id collision retry loop

(niwa-deftest niwa-session-persistence/id-collision-retry ()
  ,test (test)
  :doc "materialization refuses an occupied identity without changing it"
  (pcase-let* ((`(,workspace . ,tempdir)
                 (test-niwa-session-persistence--make-tempdir-workspace))
                (session (niwa-session-create "main" workspace))
                (id (niwa-session-session-id session))
                (path (file-name-concat
                       (niwa-session-artifacts-sessions-dir workspace) id))
                (buffer (generate-new-buffer " *id-collision*")))
    (unwind-protect
        (progn
          (make-directory path t)
          (with-current-buffer buffer
            (org-mode)
            (setq-local niwa--session session)
            (should-error (niwa-session-artifacts-ensure-files session buffer)
                          :type 'file-already-exists)
            (should (equal id (niwa-session-session-id session)))
            (should-not (niwa-session-save-path session))
            (should (equal '("." "..") (directory-files path)))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory tempdir t)
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-write-sidecar-now ()
  ,test
  (test)
  :doc "admits and publishes a portable sidecar in one durable transaction"
  (let ((session (niwa-session--create :name "sidecar"))
        admitted published)
    (setf (niwa-session-save-path session) "/session/")
    (cl-letf (((symbol-function 'niwa-session-codec-portable-authority-p)
               #'always)
              ((symbol-function 'niwa-session-artifacts-artifact-present-p)
               #'always)
              ((symbol-function 'niwa-session-artifacts--live-sidecar)
               (lambda (&rest _) '(:session-id "s")))
              ((symbol-function 'niwa-session-artifacts-assert-mutation-authority)
               (lambda (&rest _)
                 (setq admitted niwa-session-durability--transaction-clock)))
              ((symbol-function 'niwa-session-publication-publish)
               (lambda (&rest _)
                 (setq published niwa-session-durability--transaction-clock)
                 t)))
      (should (niwa-session-persistence-write-sidecar-now session nil))
      ;; One transaction: publication reuses admission's observations.
      (should admitted)
      (should (eq admitted published))))
  :doc "commits a remote sidecar when its fixed cache is missing"
  (let* ((host "write-sidecar-publication")
         (local-root
          (file-name-as-directory
           (make-temp-file "niwa-write-sidecar-remote-" t)))
         (niwa-session-durability--client-id (make-string 64 ?a))
         buffer)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (pcase-let* ((`(,_workspace ,session ,session-dir ,segment)
                        (test-niwa-session-persistence--make-remote-restore-fixture
                         host local-root "sidecar transcript\n"))
                       (sidecar
                        (niwa-session-artifacts-sidecar-path session-dir))
                       (niwa-session-durability--disclosed-targets
                        (make-hash-table :test #'equal)))
            (puthash
             (niwa-execution-target-identity
              (niwa-session-execution-target session))
             t niwa-session-durability--disclosed-targets)
            (unwind-protect
                (progn
                  (should
                   (niwa-session-durability-lease-acquire
                    session-dir "*write-sidecar*" session))
                  (setf (niwa-session-publication session)
                        (niwa-session-publication-read
                         session-dir)
                        (niwa-session-name session) "updated")
                  (setq buffer
                        (generate-new-buffer " *write-sidecar-root*"))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local niwa--session session)
                    (setq buffer-file-name segment)
                    (insert "sidecar transcript\n"))
                  (let ((head-before
                         (plist-get (niwa-session-publication session)
                                    :head)))
                    (delete-file sidecar)
                    (should
                     (niwa-session-persistence-write-sidecar-now
                      session buffer))
                    (should-not
                     (equal head-before
                            (plist-get
                             (niwa-session-publication session) :head)))
                    (should
                     (equal
                      "updated"
                      (plist-get
                       (with-temp-buffer
                         (insert
                          (niwa-session-artifacts-read-artifact
                           session "session.meta.el" t))
                         (goto-char (point-min))
                         (read (current-buffer)))
                       :session-name)))))
              (when session
                (niwa-session-durability-lease-release
                 session-dir session)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-save-agent-state-soon ()
  ,test
  (test)

  :doc "coalesces repeated scheduling into one pending save"
  (let ((session (niwa-session--create :name "debounce")))
    (setf (niwa-session-save-path session) temporary-file-directory)
    (unwind-protect
        (progn
          (niwa-session-persistence-save-agent-state-soon session)
          (let ((timer (gethash
                        session
                        niwa-session-persistence--deferred-agent-saves)))
            (should (timerp timer))
            (niwa-session-persistence-save-agent-state-soon session)
            (should (eq timer
                        (gethash
                         session
                         niwa-session-persistence--deferred-agent-saves)))))
      (niwa-session-persistence-cancel-deferred-agent-save session))
    (should-not (gethash
                 session
                 niwa-session-persistence--deferred-agent-saves))))

(niwa-deftest niwa-session-persistence--deferred-agent-save ()
  ,test
  (test)

  :doc "the deferred save is a sidecar-only registry save, not a commit"
  (let ((session (niwa-session--create :name "debounce"))
        (buffer (generate-new-buffer " *debounce-root*"))
        captured required)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) temporary-file-directory)
          (setf (niwa-session-root-buffer session) buffer)
          (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (_key _path thunk) (funcall thunk)))
                    ((symbol-function
                      'niwa-session-artifacts-save-agent-registry)
                     (lambda (seen-session seen-buffer)
                       (setq captured (list seen-session seen-buffer)
                             required
                             niwa-session-artifacts-require-agent-commit-p)
                       t)))
            (niwa-session-persistence--deferred-agent-save session))
          (should (eq session (car captured)))
          (should (eq buffer (cadr captured)))
          (should-not required))
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "stays visible to exit flushing while transport defers the save"
  (let ((session (niwa-session--create :name "debounce"))
        (buffer (generate-new-buffer " *debounce-root*"))
        deferred
        saved)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) temporary-file-directory)
          (setf (niwa-session-root-buffer session) buffer)
          (niwa-session-persistence-save-agent-state-soon session)
          (cancel-timer
           (gethash session
                    niwa-session-persistence--deferred-agent-saves))
          (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (_key _path thunk) (setq deferred thunk)))
                    ((symbol-function
                      'niwa-session-artifacts-save-agent-registry)
                     (lambda (&rest _) (setq saved t))))
            (niwa-session-persistence--deferred-agent-save session)
            (should (gethash
                     session
                     niwa-session-persistence--deferred-agent-saves))
            (should-not saved)
            (funcall deferred)
            (should saved)
            (should-not (gethash
                         session
                         niwa-session-persistence--deferred-agent-saves))))
      (niwa-session-persistence-cancel-deferred-agent-save session)
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "swallows a registry save failure"
  (let ((session (niwa-session--create :name "debounce"))
        (buffer (generate-new-buffer " *debounce-root*")))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) temporary-file-directory)
          (setf (niwa-session-root-buffer session) buffer)
          (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (_key _path thunk) (funcall thunk)))
                    ((symbol-function
                      'niwa-session-artifacts-save-agent-registry)
                     (lambda (&rest _) (error "Publication unavailable"))))
            (niwa-session-persistence--deferred-agent-save session))
          (should-not (gethash
                       session
                       niwa-session-persistence--deferred-agent-saves)))
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "re-arms instead of writing while a publication is active"
  (let ((session (niwa-session--create :name "debounce"))
        saved)
    (setf (niwa-session-save-path session) temporary-file-directory)
    (setf (niwa-session-publication-active-p session) t)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                     (lambda (_key _path thunk) (funcall thunk)))
                    ((symbol-function
                      'niwa-session-artifacts-save-agent-registry)
                     (lambda (&rest _) (setq saved t))))
            (niwa-session-persistence--deferred-agent-save session))
          (should-not saved)
          (should (gethash
                   session
                   niwa-session-persistence--deferred-agent-saves)))
      (niwa-session-persistence-cancel-deferred-agent-save session))))

(niwa-deftest niwa-session-persistence-cancel-deferred-agent-save ()
  ,test
  (test)
  :doc "cancels both debounce and transport retry state"
  (let ((session (niwa-session--create :name "debounce"))
        cancelled)
    (setf (niwa-session-save-path session) temporary-file-directory)
    (niwa-session-persistence-save-agent-state-soon session)
    (should (gethash
             session niwa-session-persistence--deferred-agent-saves))
    (cl-letf (((symbol-function 'niwa-transport-cancel-pending)
               (lambda (key) (setq cancelled key))))
      (niwa-session-persistence-cancel-deferred-agent-save session))
    (should-not (gethash
                 session niwa-session-persistence--deferred-agent-saves))
    (should (equal (list 'agent-state-save session) cancelled))))

(niwa-deftest niwa-session-persistence-save-agent-state (:quiet t)
  ,test
  (test)

  :doc "a synchronous agent commit absorbs the pending save"
  (let ((session (niwa-session--create :name "debounce" ))
        (buffer (generate-new-buffer " *debounce-root*")))
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) temporary-file-directory)
          (setf (niwa-session-root-buffer session) buffer)
          (niwa-session-persistence-save-agent-state-soon session)
          (should (gethash
                   session
                   niwa-session-persistence--deferred-agent-saves))
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (&rest _) t)))
            (should (niwa-session-persistence-save-agent-state session)))
          (should-not (gethash
                       session
                       niwa-session-persistence--deferred-agent-saves)))
      (niwa-session-persistence-cancel-deferred-agent-save session)
      (when (buffer-live-p buffer) (kill-buffer buffer))))

  :doc "initial materialization forces a full save through the root buffer"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-agent-state-" t)))
         (workspace
          (niwa-workspace--create
           :type 'project :id "agent-state" :root tempdir
           :name "agent-state"))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *agent-state-root*"))
         (agent (generate-new-buffer " *agent-state-child*"))
         calls
         required)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) tempdir)
          (with-current-buffer root
            (setq-local niwa--session session)
            (setq-local buffer-file-name
                        (file-name-concat tempdir
                                          "segment-0001.chat.org")))
          (with-current-buffer agent
            (setq-local niwa--session session)
            (setq-local niwa--agent-invocation
                        (niwa-agent-invocation--create)))
          (niwa-session-control-transfer-register-root-buffer
           session root)
          (cl-letf (((symbol-function 'niwa-session-artifacts-save)
                     (lambda (seen-session seen-buffer settled force &optional _token)
                       (setq calls
                             (list seen-session seen-buffer settled force)
                             required
                             niwa-session-artifacts-require-agent-commit-p)
                       t)))
            (should
             (niwa-session-persistence-save-agent-state session)))
          (should (eq session (car calls)))
          (should (eq root (cadr calls)))
          (should-not (nth 2 calls))
          (should (nth 3 calls))
          (should required))
      (when (buffer-live-p root) (kill-buffer root))
      (when (buffer-live-p agent) (kill-buffer agent))
      (delete-directory tempdir t)))

  :doc "acknowledged mail commits staged artifacts with matching root state"
  (let* ((tempdir (file-name-as-directory (make-temp-file "niwa-ack-sidecar-" t)))
         (workspace (test-niwa-session-persistence--make-workspace tempdir)))
    (let* ((session (niwa-session-create "main" workspace))
           (root (generate-new-buffer " *agent-sidecar-root*"))
           restored directory old-entry)
      (unwind-protect
          (progn
            (with-current-buffer root
              (org-mode)
              (setq-local niwa--session session)
              (insert "Durable prompt\n")
              (niwa-session-artifacts-save session root))
            (niwa-session-control-transfer-register-root-buffer session root)
            (setq directory (niwa-session-save-path session)
                  old-entry (copy-tree
                             (assoc "segment-0001.chat.org"
                                    (plist-get (niwa-session-publication session)
                                               :artifacts))))
            (setf (niwa-session-publication-active-p session) t)
            (unwind-protect
                (with-current-buffer root
                  (should-error
                   (niwa-agent-control-send-message session "/root" "Not queued")
                   :type 'user-error)
                  (should-not (niwa-session-messages session))
                  (should-not (niwa-session-publication-queue session)))
              (setf (niwa-session-publication-active-p session) nil))
            ;; A marker-free transcript batch must become reachable in the
            ;; same head as the acknowledged mailbox record referencing it.
            (niwa-session-publication-publish
             session (list (list :path (file-name-concat directory "agents/answer.chat.org")
                                 :content "Retained answer\n")))
            (should (niwa-session-publication-uncommitted-batches session))
            (with-current-buffer root (insert "Uncheckpointed root text\n"))
            (with-current-buffer root
              (niwa-agent-control-send-message
               session "/root" "See agents/answer.chat.org"))
            (should-not (niwa-session-publication-uncommitted-batches session))
            (should (equal old-entry
                           (assoc "segment-0001.chat.org"
                                  (plist-get (niwa-session-publication session)
                                             :artifacts))))
            ;; Release without a final save: restore sees only committed data.
            (test-niwa-session-persistence--release-and-kill root session)
            (setq root nil
                  restored (niwa-session-persistence-restore directory))
            (with-current-buffer restored
              (should (string-match-p "Durable prompt" (buffer-string)))
              (should-not (string-match-p "Uncheckpointed" (buffer-string)))
              (let* ((mail (niwa-session-messages niwa--session))
                     (entry (cdr (assoc "agents/answer.chat.org"
                                        (plist-get (niwa-session-publication niwa--session)
                                                   :artifacts)))))
                (should (= 1 (length mail)))
                (should (eq 'MAIL (plist-get (car mail) :type)))
                (should (equal "See agents/answer.chat.org"
                               (plist-get (car mail) :payload)))
                (should (equal "Retained answer\n"
                               (with-temp-buffer
                                 (insert-file-contents (plist-get entry :published))
                                 (buffer-string)))))
              ;; Delivery does not save.  A crash retains the durable queue
              ;; until an ordinary receiving transcript checkpoint commits.
              (let ((fsm (gptel-make-fsm
                          :info (list :buffer restored :backend nil
                                      :data (list :messages (vector))))))
                (niwa-tools--handle-message-inject fsm))
              (should-not (niwa-session-messages niwa--session))
              ;; Simulate process loss, not the restored buffer's ordinary
              ;; close hook (which deliberately performs a final checkpoint).
              (setq-local kill-buffer-hook nil))
            (test-niwa-session-persistence--release-and-kill
             restored (buffer-local-value 'niwa--session restored))
            (setq restored (niwa-session-persistence-restore directory))
            (with-current-buffer restored
              (should (= 1 (length (niwa-session-messages niwa--session))))
              (should-not (string-match-p "<agent-message type=\"MAIL\"" (buffer-string)))
              (let ((fsm (gptel-make-fsm
                          :info (list :buffer restored :backend nil
                                      :data (list :messages (vector))))))
                (niwa-tools--handle-message-inject fsm))
              (should (niwa-session-artifacts-save niwa--session restored)))
            (test-niwa-session-persistence--release-and-kill
             restored (buffer-local-value 'niwa--session restored))
            (setq restored (niwa-session-persistence-restore directory))
            (with-current-buffer restored
              (should-not (niwa-session-messages niwa--session))
              (should (string-match-p "See agents/answer.chat.org" (buffer-string)))
              (should (string-match-p "<agent-message type=\"MAIL\"" (buffer-string)))))
        (test-niwa-session-persistence--release-and-kill root session)
        (test-niwa-session-persistence--release-and-kill
         restored (and restored (buffer-local-value 'niwa--session restored)))
        (delete-directory tempdir t)
        (niwa-workspace-clear-registry))))

  :doc "agent mail stays durable until its later transcript checkpoint commits"
  (dolist (nested-failure '(nil t))
    (let* ((tempdir (file-name-as-directory (make-temp-file "niwa-agent-mail-" t)))
           (workspace (test-niwa-session-persistence--make-workspace tempdir))
           (session (niwa-session-create "main" workspace))
           (root (generate-new-buffer " *mail-root*"))
           (buffer (generate-new-buffer " *mail-agent*"))
           (agent (niwa-agent--create :name "default" :description "Test agent"))
           (configuration
            (niwa-agent-configuration--create
             :agent agent
             :request-locals
             (mapcar (lambda (symbol)
                       (cons symbol (pcase symbol
                                      ('gptel-backend gptel-backend)
                                      ('gptel-model gptel-model))))
                     niwa-agent-request-local-symbols)))
           (invocation
            (niwa-agent-invocation--create
             :agent agent :agent-id "mail-agent" :path "/root/worker"
             :buffer buffer :parent-session session :parent-data-buffer root
             :transcript-relative-path "agents/worker.chat.org"))
           (record
            (niwa-agent-record--create
             :id "mail-agent" :path "/root/worker" :parent-path "/root"
             :role "default" :configuration configuration :activity 'idle
             :invocation invocation :conversation-buffer buffer
             :conversation-location "agents/worker.chat.org"))
           restored directory entered (saves 0))
      (unwind-protect
          (progn
            (with-current-buffer root
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session niwa--workspace workspace)
              (insert "Root prompt\n")
              (niwa-session-artifacts-save session root))
            (niwa-session-control-transfer-register-root-buffer session root)
            (setq directory (niwa-session-save-path session))
            (setf (niwa-session-agent-registry session) (list (cons "/root/worker" record))
                  (niwa-session-agent-transcripts session)
                  '(("mail-agent" :agent-type "default" :agent-path "/root/worker"
                     :description "Test agent" :path "agents/worker.chat.org"
                     :status done :created-at "created" :updated-at "updated"
                     :parent-turn 0)))
            (with-current-buffer buffer
              (niwa-chat-prepare-transcript-buffer)
              (setq-local niwa--session session niwa--agent-invocation invocation)
              (set-visited-file-name (file-name-concat directory "agents/worker.chat.org") t t)
              (insert "Agent prompt\n"))
            (should (niwa-agent-conversation-save invocation))
            ;; Queue without starting the idle agent: this case delivers
            ;; the mail at a simulated WAIT boundary.
            (cl-letf (((symbol-function 'niwa-agent-control--capacity-p) #'ignore)
                      ((symbol-function 'niwa-agent-control--schedule-agent-wakes)
                       #'ignore))
              (dolist (payload '("First delivery" "Second delivery"))
                (niwa-agent-control-send-message session "/root/worker" payload)))
            (when nested-failure
              (with-current-buffer buffer
		(setq-local before-save-hook
                            (list (lambda ()
                                    (setq entered t)
                                    (niwa-agent-control-send-message
                                     session "/root" "Nested hook mail")
                                    (error "Injected failure after nested commit"))))))
            (cl-letf* ((save (symbol-function 'niwa-agent-conversation-save))
                       ((symbol-function 'niwa-agent-conversation-save)
			(lambda (&rest args) (cl-incf saves) (apply save args))))
              (let ((fsm (gptel-make-fsm :info (list :buffer buffer :backend nil
                                                     :data (list :messages (vector))
                                                     :niwa-agent-invocation invocation))))
                (niwa-tools--handle-message-inject fsm))
              (should (= 0 saves))
              (should-not entered)
              (should-not (niwa-agent-record-mailbox record))
              ;; Only the ordinary later save runs hooks.  Failed/nested
              ;; saves must not publish queue removal without this transcript.
              (if nested-failure
                  (niwa-test--with-captured-diagnostics nil
                    (should-not (niwa-agent-conversation-save invocation)))
                (should (niwa-agent-conversation-save invocation))))
            (should (= 1 saves))
            (when nested-failure (should entered))
            (should-not (niwa-agent-record-mailbox record))
            (test-niwa-session-persistence--release-and-kill root session)
            (setq root nil restored (niwa-session-persistence-restore directory))
            (with-current-buffer restored
              (let ((restored-record (cdr (assoc "/root/worker"
						 (niwa-session-agent-registry niwa--session))))
                    (text (unless nested-failure
                            (niwa-session-artifacts-read-artifact
                             niwa--session "agents/worker.chat.org" t))))
		(should restored-record)
		(if nested-failure
                    (should (equal '("Second delivery" "First delivery")
                                   (mapcar (lambda (mail) (plist-get mail :payload))
                                           (niwa-agent-record-mailbox restored-record))))
                  (should-not (niwa-agent-record-mailbox restored-record))
                  (should (string-search "First delivery" text))
                  (should (string-search "Second delivery" text))))))
	(with-current-buffer buffer
          (setq kill-buffer-hook nil before-save-hook nil)
          (set-buffer-modified-p nil))
	(kill-buffer buffer)
	(test-niwa-session-persistence--release-and-kill root session)
	(test-niwa-session-persistence--release-and-kill
	 restored (and restored (buffer-local-value 'niwa--session restored)))
	(delete-directory tempdir t)
	(niwa-workspace-clear-registry))))

  :doc "keeps a committed full agent save successful when its save hook fails"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-agent-post-save-" t)))
         (workspace
          (test-niwa-session-persistence--make-workspace tempdir))
         (session (niwa-session-create "main" workspace))
         (root (generate-new-buffer " *agent-post-save-root*"))
         (segment (file-name-concat tempdir "segment-0001.chat.org"))
         published)
    (unwind-protect
        (progn
          (setf (niwa-session-save-path session) tempdir
                (niwa-session-authority-mode session) 'portable)
          (with-current-buffer root
            (setq-local niwa--session session)
            (setq-local buffer-file-name segment)
            (insert "terminal root state\n")
            (setq-local after-save-hook
                        (list (lambda () (error "Post-save hook failed")))))
          (niwa-session-control-transfer-register-root-buffer session root)
          (cl-letf
              (((symbol-function
                 'niwa-session-codec-portable-authority-p)
                (lambda (_session) t))
               ((symbol-function
                 'niwa-session-artifacts-assert-mutation-authority)
                (lambda (&rest _) t))
               ((symbol-function 'niwa-session-artifacts-ensure-files)
                (lambda (&rest _) tempdir))
               ((symbol-function 'niwa-session-publication-committed-p)
                (lambda (&rest _) nil))
               ((symbol-function 'niwa-session-publication-prune-committed)
                (lambda (_session artifacts) artifacts))
               ((symbol-function 'niwa-session-publication-publish)
                (lambda (_session _artifacts &optional require-commit _prepared _token)
                  (should require-commit)
                  (setq published t)))
               ((symbol-function
                 'niwa-session-persistence-notify-session-event)
                #'ignore))
            (niwa-test--with-captured-diagnostics nil
              (should
               (niwa-session-persistence-save-agent-state session))))
          (should published)
          (with-current-buffer root
            (should-not (buffer-modified-p))))
      (when (buffer-live-p root)
        (with-current-buffer root (set-buffer-modified-p nil))
        (kill-buffer root))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))
      (niwa-workspace-clear-registry))))


(niwa-deftest niwa-session-persistence-resume-id
  (:doc "resumes an exact persisted session id and reports unavailable ids")
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-resume-id-" t)))
         (workspace
          (niwa-workspace--create
           :type 'project :id root :root root :name "resume-id"))
         (session-id "main-2026-08-02T12-00-abcd")
         (session-dir
          (file-name-concat root ".niwa" "sessions" session-id))
         restored)
    (unwind-protect
        (progn
          (make-directory session-dir t)
          (cl-letf (((symbol-function 'niwa-session-persistence-restore)
                     (lambda (path &rest _)
                       (setq restored path)
                       'restored-buffer)))
            (should (eq 'restored-buffer
                        (niwa-session-persistence-resume-id
                         workspace session-id)))
            (should (equal (file-name-as-directory session-dir)
                           restored))
            (should-not
             (niwa-session-persistence-resume-id workspace "missing"))
            (should-error
             (niwa-session-persistence-resume-id workspace "../escape"))))
      (delete-directory root t))))

(niwa-deftest niwa-session-durability--pid-alive-p
  (:doc "distinguishes a live process from an actually exited child")
  ,test
  (test)
  (should (niwa-session-durability--pid-alive-p (emacs-pid)))
  (let* ((child (make-process :name "session-dead-pid" :command '("true") :noquery t))
         (pid (process-id child)))
    (while (process-live-p child) (accept-process-output child 0.01))
    (should (integerp pid))
    (should-not (niwa-session-durability--pid-alive-p pid)))
  :doc "requires successful local enumeration to prove a PID dead"
  (let ((default-directory "/ssh:unreachable:/project/"))
    (dolist (observation '(error nil (123 456) (456)))
      (cl-letf (((symbol-function 'list-system-processes)
                 (lambda ()
                   (should-not (file-remote-p default-directory))
                   (if (eq observation 'error)
                       (error "Process enumeration unavailable")
                     observation))))
        (should (eq (not (null (niwa-session-durability--pid-alive-p 123)))
                    (not (equal observation '(456))))))))
  :doc "invalid PIDs never count as proven dead"
  (cl-letf (((symbol-function 'list-system-processes) (lambda () '(123))))
    (dolist (pid '(nil 0 -1 1.5 "123"))
      (should (niwa-session-durability--pid-alive-p pid)))))

(niwa-deftest niwa-session-persistence--sidecar-summary ()
  ,test
  (test)
  :doc "retains attachment ids for workspace listing projections"
  (let* ((sidecar (test-niwa-session-persistence--complete-sidecar
                   '(:attached-artifacts ("a" "b"))))
         (summary (niwa-session-persistence--sidecar-summary sidecar)))
    (should (equal '("a" "b") (plist-get summary :attached-artifacts))))
  :doc "projects the chats from the sidecar alone, without reading segments"
  (let* ((sidecar (test-niwa-session-persistence--complete-sidecar
                   '(:session-name "Now" :current-segment 3
                     :chapters ((:segment 2 :title "Before" :closed-at "t"))))))
    (should (equal '("Before" "Now")
                   (mapcar (lambda (chat) (plist-get chat :title))
                           (plist-get (niwa-session-persistence--sidecar-summary
                                       sidecar)
                                      :chats))))))

(provide 'test-niwa-session-persistence)
;;; test-niwa-session-persistence.el ends here
