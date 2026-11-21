;;; test-niwa-session-persistence-cost.el --- Save round-trip cost -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounds how many target processes one durable save costs.  Every session
;; control operation is one target process, which is one SSH round trip on a
;; remote workspace, so this is the difference between a save the user does not
;; notice and one that stalls Emacs for seconds.  The counts here are upper
;; bounds with headroom, not exact expectations: a change that needs more
;; round trips per save should have to say so here.

;;; Code:

(require 'ert)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-fs)
(require 'niwa-session-control-transfer)
(require 'niwa-session-durability)
(require 'niwa-journal-pins)
(require 'niwa-session-persistence)
(require 'niwa-session-publication)
(require 'niwa-telemetry)
(require 'niwa-utilities)
(require 'niwa-workspace)
(require 'niwa-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar test-niwa-session-persistence-cost--processes 0
  "Target processes counted during one measured save.")

(defmacro test-niwa-session-persistence-cost--measure (&rest body)
  "Run BODY and return the number of target processes it started."
  (declare (indent 0) (debug t))
  `(let ((test-niwa-session-persistence-cost--processes 0)
         (program (symbol-function 'niwa-session-control-fs-run-program)))
     ;; Every control operation is a program now; each program is one
     ;; target process regardless of its operation count.
     (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                (lambda (&rest arguments)
                  (cl-incf test-niwa-session-persistence-cost--processes)
                  (apply program arguments))))
       ,@body)
     test-niwa-session-persistence-cost--processes))

(niwa-deftest niwa-session-artifacts-save/cost ()
  ,test
  (test)
  :doc "one portable save stays within its target round-trip budget"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-save-cost-" t)))
         (workspace (progn
                      (niwa-workspace-clear-registry)
                      (niwa-workspace-get-or-create
                       'project "cost-id" tempdir
                       (file-name-nondirectory
                        (directory-file-name tempdir)))))
         (session (progn (niwa-workspace-identity-ensure tempdir)
                         (niwa-session-create "main" workspace)))
         (buffer (generate-new-buffer " *save-cost*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local niwa--session session)
          (insert "*** First prompt\n")
          (should (eq 'portable
                      (niwa-session-authority-mode-for-session session)))
          ;; Materialization allocates the session directory, its lease and
          ;; its first publication, so it is allowed more than a later save.
          (let ((processes
                 (test-niwa-session-persistence-cost--measure
                   (niwa-session-artifacts-save session buffer))))
            (should (<= processes 45)))
          ;; Cold admission combines transfer, recovery, PID-lock and clock
          ;; observations.  The quiet local mailbox then keeps warm admission
          ;; at that same one-program cost, not zero authority checks.
          (dotimes (_ 2)
            (should (= 1 (test-niwa-session-persistence-cost--measure
                           (niwa-session-artifacts-assert-new-mutation-authority
                            session)))))
          ;; A save that carries a change publishes it; the artifacts whose
          ;; bytes the target already holds are not republished.
          (goto-char (point-max))
          (insert "*** Second prompt\n")
          (let ((processes
                 (test-niwa-session-persistence-cost--measure
                   (niwa-session-artifacts-save session buffer))))
            (should (<= processes 7)))
          ;; A save with nothing to record owes the target no transaction.
          (let ((processes
                 (test-niwa-session-persistence-cost--measure
                   (niwa-session-artifacts-save session buffer))))
            (should (<= processes 2)))
          ;; Forcing publishes even when the state is already committed: a
          ;; new head, from admission, the reservation carrying the fixed
          ;; sidecar write, and the head commit carrying the generation.
          (let* ((head (plist-get (niwa-session-publication session) :head))
                 (processes
                  (test-niwa-session-persistence-cost--measure
                    (niwa-session-artifacts-save session buffer nil t))))
            (should (>= processes 3))
            (should-not (equal head (plist-get (niwa-session-publication session)
                                               :head)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))
      (niwa-workspace-clear-registry)))

  :doc "a settled remote save stays bounded and defers telemetry"
  (let* ((host "save-cost-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-remote-save-cost-" t)))
         (buffer (generate-new-buffer " *remote-save-cost*"))
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((remote-root (format "/niwamock:%s:%s"
                                      host local-root))
                 (workspace (progn
                              (niwa-workspace-clear-registry)
                              (niwa-workspace-get-or-create
                               'project remote-root remote-root
                               "remote-cost"))))
            (niwa-workspace-identity-ensure remote-root)
            (setq session (niwa-session-create "main" workspace))
            (niwa-execution-target-probe
             (niwa-session-execution-target session) t 'off)
            (puthash (niwa-execution-target-identity
                      (niwa-session-execution-target session))
                     t niwa-session-durability--disclosed-targets)
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "*** First prompt\n")
              (should (eq 'portable
                          (niwa-session-authority-mode-for-session
                           session)))
              (niwa-session-artifacts-save session buffer)
              ;; The settled turn a user feels: one transcript change and
              ;; queued telemetry.  The telemetry flush is a whole remote
              ;; publication transaction, so it must not run inside the
              ;; save -- its entries are still queued when save returns.
              (goto-char (point-max))
              (insert "*** Second prompt\n")
              (niwa-telemetry-record session 'cost-probe)
              (should (niwa-session-telemetry-pending session))
              (let ((raw-reads 0)
                    (read-raw
                     (symbol-function 'niwa-session-artifacts-read-file-raw))
                    processes)
                ;; Publication bytes ride raw TRAMP reads, not control
                ;; programs, so the process budget alone cannot see them.
                ;; A settled save of an owned session must not read any
                ;; published artifact back.
                (cl-letf (((symbol-function
                            'niwa-session-artifacts-read-file-raw)
                           (lambda (path)
                             (cl-incf raw-reads)
                             (funcall read-raw path))))
                  (setq processes
                        (test-niwa-session-persistence-cost--measure
                          (niwa-session-artifacts-save
                           session buffer))))
                (should (<= processes 7))
                (should (= 0 raw-reads)))
              (should (niwa-session-telemetry-pending session)))))
      (when session
        (ignore-errors
          (niwa-session-durability--cancel-renewal session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))

;; Each count below is an upper bound on control programs, which are one
;; remote round trip each and block input while they run.
(niwa-deftest niwa-session-durability/cost ()
  ,test
  (test)
  :doc "ordinary remote durability operations stay within their round-trip budgets"
  (let* ((host "durability-cost-host")
         (local-root (file-name-as-directory
                      (make-temp-file "niwa-durability-cost-" t)))
         (buffer (generate-new-buffer " *durability-cost*"))
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((remote-root (format "/niwamock:%s:%s" host local-root))
                 (workspace (progn
                              (niwa-workspace-clear-registry)
                              (niwa-workspace-get-or-create
                               'project remote-root remote-root "durability-cost"))))
            (niwa-workspace-identity-ensure remote-root)
            (setq session (niwa-session-create "main" workspace))
            (niwa-execution-target-probe
             (niwa-session-execution-target session) t 'off)
            (puthash (niwa-execution-target-identity
                      (niwa-session-execution-target session))
                     t niwa-session-durability--disclosed-targets)
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "*** First prompt\n")
              (niwa-session-artifacts-save session buffer)
              ;; A tool call's admission: transfer state, recovery markers,
              ;; the PID-lock proof and the ownership clock.
              (should (= (test-niwa-session-persistence-cost--measure
                            (niwa-session-artifacts-assert-new-mutation-authority
                             session))
                         1))
              ;; The owner's transfer poll observes lease and requests at once.
              (should (= 1 (test-niwa-session-persistence-cost--measure
                             (niwa-session-control-transfer-poll
                              session buffer nil))))
              ;; Arming and settling the latch claim a generation each.
              (should (<= (test-niwa-session-persistence-cost--measure
                            (should (niwa-session-durability-set-unsettled-mutation
                                     session t)))
                          3))
              (should (<= (test-niwa-session-persistence-cost--measure
                            (should (niwa-session-durability-set-unsettled-mutation
                                     session nil)))
                          3))
              ;; A diagnostic flush proves ownership inside its append.
              (niwa-telemetry-record session 'cost-probe)
              (should (<= (test-niwa-session-persistence-cost--measure
                            (niwa-telemetry-flush session))
                          2))
              (should-not (niwa-session-telemetry-pending session))
              ;; An agent transcript only stages bytes for the next commit.
              (should (<= (test-niwa-session-persistence-cost--measure
                            (niwa-session-artifacts-publish-text
                             session "agents/cost.chat.org" "transcript"
                             'utf-8-unix))
                          1))
              (should (niwa-session-publication-uncommitted-artifact
                       session "agents/cost.chat.org")))))
      (when session
        (ignore-errors
          (niwa-session-durability--cancel-renewal session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (niwa-workspace-clear-registry))))

(defun test-niwa-session-persistence-cost--probe-programs (thunk)
  "Call THUNK and return (PROBES . TOTAL) counted in target programs.
PROBES counts the programs whose operations only look at the session
directories -- the existence probes and the lease listings."
  (let ((probes 0)
        (total 0)
        (program (symbol-function 'niwa-session-control-fs-run-program)))
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &rest arguments)
                 (cl-incf total)
                 (when (memq (plist-get (car operations) :op)
                             '(path-exists-p list-directory))
                   (cl-incf probes))
                 (apply program operations arguments))))
      (funcall thunk))
    (cons probes total)))

(niwa-deftest niwa-session-persistence-list-sessions/cost ()
  ,test
  (test)
  :doc "enumerating a workspace probes and lists in a constant number of programs"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "niwa-list-cost-" t)))
         (workspace (progn
                      (niwa-workspace-clear-registry)
                      (niwa-workspace-get-or-create
                       'project "list-cost-id" tempdir
                       (file-name-nondirectory
                        (directory-file-name tempdir)))))
         (buffers nil)
         (sessions nil))
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure tempdir)
          (dolist (name '("one" "two" "three" "four"))
            (let* ((session (niwa-session-create name workspace))
                   (buffer (generate-new-buffer
                            (format " *list-cost-%s*" name))))
              (push session sessions)
              (push buffer buffers)
              (with-current-buffer buffer
                (org-mode)
                (setq-local niwa--session session)
                (insert "*** Prompt\n")
                (niwa-session-artifacts-save session buffer))))
          (let ((four (test-niwa-session-persistence-cost--probe-programs
                       (lambda ()
                         (should (= 4 (length
                                       (niwa-session-persistence-list-sessions
                                        workspace))))))))
            ;; Every candidate is probed in one program and every lease
            ;; directory listed in one more, so four sessions cost two.
            (should (= 2 (car four)))
            ;; Retire half the sessions and enumerate again.
            (dolist (session sessions)
              (when (member (niwa-session-name session) '("three" "four"))
                (delete-directory (niwa-session-save-path session) t)))
            (let ((two (test-niwa-session-persistence-cost--probe-programs
                        (lambda ()
                          (should (= 2 (length
                                        (niwa-session-persistence-list-sessions
                                         workspace))))))))
              ;; Halving the sessions does not change that constant, which is
              ;; what a per-entry probe or listing would break.
              (should (= 2 (car two)))
              ;; Lease records, manifests and sidecars share read batches,
              ;; so these rows cost no additional per-session programs.
              (should (= (cdr two) (cdr four)))
              (should (<= (cdr four) 6)))))
      (dolist (session sessions)
        (ignore-errors (niwa-session-durability--cancel-renewal session)))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))
      (niwa-workspace-clear-registry)
      (clrhash niwa-session-persistence--list-sessions-cache)
      (clrhash niwa-session-persistence--summary-cache))))


(niwa-deftest niwa-session-persistence-choose-entry/cost ()
  ,test
  (test)
  :doc "opening a project chooser never drains journal retention"
  (let* ((root (make-temp-file "niwa-chooser-retention-" t))
         (workspace (niwa-workspace--create :root root :id root :type 'project))
         (directory (file-name-concat root ".niwa/state/journal/captures")))
    (unwind-protect
        (progn
          (make-directory directory t)
          (cl-letf (((symbol-function 'niwa-journal-cleanup-expired)
                     (lambda (&rest _) (ert-fail "Chooser ran journal retention")))
                    ((symbol-function 'niwa-journal-cleanup-schedule)
                     (lambda (&rest _) (ert-fail "Chooser queued journal retention"))))
            (should-not (niwa-session-persistence-choose-entry workspace))))
      (delete-directory root t)))

  :doc "lists released local sessions with one shared scan and one row assessment batch"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-chooser-cost-" t)))
         (workspace (niwa-workspace-get-or-create 'project root root "chooser-cost")))
    (unwind-protect
        (progn
          (niwa-workspace-identity-ensure root)
          (dolist (name '("one" "two" "three"))
            (let ((session (niwa-session-create name workspace)))
              (with-temp-buffer
                (org-mode)
                (setq-local niwa--session session)
                (insert "*** Prompt\n")
                (niwa-session-artifacts-save session (current-buffer))
                (niwa-session-durability-lease-release
                 (niwa-session-save-path session) session))))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_prompt choices &rest _)
                       (should (= 3 (length (all-completions "Resume" choices))))
                       "Start new session")))
            (should (<= (test-niwa-session-persistence-cost--measure
                          (should (eq 'new (niwa-session-persistence-choose-entry workspace))))
                        8)))
          ;; One unreadable lease aborts the batch, but not the local chooser.
          (let ((run (symbol-function 'niwa-session-control-fs-run-program)) bad-path)
            (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                       (lambda (ops &rest args)
                         (let ((rows (seq-filter (lambda (op) (eq 'observation-time (plist-get op :op))) ops)))
                           (when (cdr rows) (setq bad-path (directory-file-name (plist-get (car rows) :path))))
                           (when (and rows (equal bad-path (directory-file-name (plist-get (car rows) :path))))
                             (signal 'file-error '("Injected unreadable authority"))))
                         (apply run ops args)))
                      ((symbol-function 'completing-read)
                       (lambda (_prompt choices &rest _)
                         (should (= 2 (length (all-completions "Resume" choices))))
                         (should (= 1 (length (all-completions "Inspect" choices))))
                         "Start new session")))
              (should (eq 'new (niwa-session-persistence-choose-entry workspace))))))
      (delete-directory root t)
      (niwa-workspace-clear-registry)))

  :doc "sizes every incompatible row in one target program"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-incompatible-cost-" t)))
         (workspace
          (niwa-workspace-get-or-create
           'file "incompatible-cost" root "incompatible-cost"))
         (paths
          (mapcar
           (lambda (name)
             (let ((path (file-name-as-directory
                          (file-name-concat root name))))
               (make-directory path)
               (with-temp-file (file-name-concat path "segment-0001.chat.org")
                 (insert name))
               path))
           '("one" "two" "three")))
         (programs nil))
    (unwind-protect
        (cl-letf
            (((symbol-function 'niwa-session-persistence-cleanup-expired)
              #'ignore)
             ((symbol-function 'niwa-session-persistence--enumerate-sessions)
              (lambda (&rest _)
                (list
                 :sessions nil
                 :incompatible
                 (mapcar
                  (lambda (path)
                    (list :kind 'incompatible :save-path path
                          :authority-mode 'portable :status 'missing))
                  paths))))
             ((symbol-function
               'niwa--ordered-completion-table)
              (lambda (values &rest _) values))
             ((symbol-function 'completing-read)
              (lambda (&rest _) "Start new session"))
             ((symbol-function 'niwa-session-control-fs-run-program)
              (let ((run
                     (symbol-function
                      'niwa-session-control-fs-run-program)))
                (lambda (operations &rest arguments)
                  (push operations programs)
                  (apply run operations arguments)))))
          (should
           (eq 'new
               (niwa-session-persistence-choose-entry workspace)))
          (should (= 1 (length programs)))
          (should (= 3 (length (car programs))))
          (should
           (seq-every-p
            (lambda (operation)
              (eq 'tree-size (plist-get operation :op)))
            (car programs))))
      (when (file-directory-p root)
        (delete-directory root t))
      (niwa-workspace-clear-registry)))

  :doc "a real remote recovery row shares probes across its three cleanup reasons"
  (let* ((host "recovery-chooser-cost")
         (root (make-temp-file "niwa-recovery-chooser-cost-" t))
         (buffer (generate-new-buffer " *recovery-chooser-cost*"))
         session)
    (unwind-protect
        (niwa-test--with-local-shell-tramp (list host)
          (let* ((remote-root (format "/niwamock:%s:%s/" host root))
                 (workspace (niwa-workspace-get-or-create
                             'project remote-root remote-root "recovery-cost")))
            (niwa-workspace-identity-ensure remote-root)
            (setq session (niwa-session-create "main" workspace))
            (niwa-execution-target-probe
             (niwa-session-execution-target session) t 'off)
            (puthash (niwa-execution-target-identity
                      (niwa-session-execution-target session))
                     t niwa-session-durability--disclosed-targets)
            (with-current-buffer buffer
              (org-mode)
              (setq-local niwa--session session)
              (insert "*** Committed prompt\n")
              (niwa-session-artifacts-save session buffer)
              (set-buffer-modified-p nil))
            (let* ((directory (niwa-session-save-path session))
                   (lease (copy-sequence (niwa-session-lease session)))
                   (path (file-name-concat
                          directory ".lease"
                          (format "%020d.el" (plist-get lease :generation)))))
              (niwa-session-durability-lease-release directory session)
              (kill-buffer buffer)
              (setq lease (plist-put lease :status 'publishing)
                    lease (plist-put lease :client-id (secure-hash 'sha256 "stopped-chooser-client"))
                    lease (plist-put lease :renewed-at 0)
                    lease (plist-put lease :expires-at 1)
                    lease (plist-put lease :unsettled-mutation t))
              (niwa-session-control-fs-write-file path (prin1-to-string lease))
              (niwa-journal-pins-retain
               directory (secure-hash 'sha256 "chooser-cost")
               (list (plist-get (niwa-session-publication session) :head)))
              (let ((assess (symbol-function 'niwa-session-durability-assess-batch))
                    (run (symbol-function 'niwa-session-control-fs-run-program))
                    assessing (assessments 0) (programs 0))
                (cl-letf (((symbol-function 'niwa-session-persistence-cleanup-expired) #'ignore)
                          ((symbol-function 'niwa-session-durability-assess-batch)
                           (lambda (&rest arguments)
                             (cl-incf assessments)
                             (setq assessing t)
                             (unwind-protect (apply assess arguments)
                               (setq assessing nil))))
                          ((symbol-function 'niwa-session-control-fs-run-program)
                           (lambda (&rest arguments)
                             (when (and assessing (car arguments)) (cl-incf programs))
                             (apply run arguments)))
                          ((symbol-function 'completing-read)
                           (lambda (_prompt choices &rest _)
                             (let ((row (seq-find
                                         (lambda (choice) (string-match-p "cleanup blocked:" choice))
                                         (all-completions "" choices))))
                               (should row)
                               (dolist (reason '("interrupted publication" "unresolved mutation"))
                                 (should (string-match-p reason row)))
                               ;; Captures settle unattended, so the row never names them.
                               (should-not (string-match-p "journal" row)))
                             "Start new session")))
                  (should (eq 'new (niwa-session-persistence-choose-entry workspace))))
                (should (= 1 assessments))
                (should (= 2 programs))))))
      (when session (niwa-session-durability--cancel-renewal session))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)
      (niwa-workspace-clear-registry))))

(provide 'test-niwa-session-persistence-cost)

;;; test-niwa-session-persistence-cost.el ends here
