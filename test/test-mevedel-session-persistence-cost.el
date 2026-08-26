;;; test-mevedel-session-persistence-cost.el --- Save round-trip cost -*- lexical-binding: t; -*-

;;; Commentary:

;; Bounds how many target processes one durable save costs.  Every session
;; control operation is one target process, which is one SSH round trip on a
;; remote workspace, so this is the difference between a save the user does not
;; notice and one that stalls Emacs for seconds.  The counts here are upper
;; bounds with headroom, not exact expectations: a change that needs more
;; round trips per save should have to say so here.

;;; Code:

(require 'ert)
(require 'mevedel-session-control-fs)
(require 'mevedel-session-durability)
(require 'mevedel-session-persistence)
(require 'mevedel-telemetry)
(require 'mevedel-workspace)
(require 'mevedel-workspace-identity)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar test-mevedel-session-persistence-cost--processes 0
  "Target processes counted during one measured save.")

(defmacro test-mevedel-session-persistence-cost--measure (&rest body)
  "Run BODY and return the number of target processes it started."
  (declare (indent 0) (debug t))
  `(let ((test-mevedel-session-persistence-cost--processes 0)
         (program (symbol-function 'mevedel-session-control-fs-run-program)))
     ;; Every control operation is a program now; each program is one
     ;; target process regardless of its operation count.
     (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                (lambda (&rest arguments)
                  (cl-incf test-mevedel-session-persistence-cost--processes)
                  (apply program arguments))))
       ,@body)
     test-mevedel-session-persistence-cost--processes))

(mevedel-deftest mevedel-session-artifacts-save/cost ()
  ,test
  (test)
  :doc "one portable save stays within its target round-trip budget"
  (let* ((tempdir (file-name-as-directory
                   (make-temp-file "mevedel-save-cost-" t)))
         (workspace (progn
                      (mevedel-workspace-clear-registry)
                      (mevedel-workspace-get-or-create
                       'project "cost-id" tempdir
                       (file-name-nondirectory
                        (directory-file-name tempdir)))))
         (session (progn (mevedel-workspace-identity-ensure tempdir)
                         (mevedel-session-create "main" workspace)))
         (buffer (generate-new-buffer " *save-cost*")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local mevedel--session session)
          (insert "*** First prompt\n")
          (should (eq 'portable
                      (mevedel-session-authority-mode-for-session session)))
          ;; Materialization allocates the session directory, its lease and
          ;; its first publication, so it is allowed more than a later save.
          (let ((processes
                 (test-mevedel-session-persistence-cost--measure
                   (mevedel-session-artifacts-save session buffer))))
            (should (<= processes 45)))
          ;; A save that carries a change publishes it; the artifacts whose
          ;; bytes the target already holds are not republished.
          (goto-char (point-max))
          (insert "*** Second prompt\n")
          (let ((processes
                 (test-mevedel-session-persistence-cost--measure
                   (mevedel-session-artifacts-save session buffer))))
            (should (<= processes 17)))
          ;; A save with nothing to record owes the target no transaction.
          (let ((processes
                 (test-mevedel-session-persistence-cost--measure
                   (mevedel-session-artifacts-save session buffer))))
            (should (<= processes 4)))
          ;; Forcing publishes even when the state is already committed.
          (let ((processes
                 (test-mevedel-session-persistence-cost--measure
                   (mevedel-session-artifacts-save session buffer nil t))))
            (should (> processes 4))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p tempdir)
        (delete-directory tempdir t))
      (mevedel-workspace-clear-registry)))

  :doc "a settled remote save stays bounded and defers telemetry"
  (let* ((host "save-cost-host")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-remote-save-cost-" t)))
         (buffer (generate-new-buffer " *remote-save-cost*"))
         session)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (let* ((remote-root (format "/mevedelmock:%s:%s"
                                      host local-root))
                 (workspace (progn
                              (mevedel-workspace-clear-registry)
                              (mevedel-workspace-get-or-create
                               'project remote-root remote-root
                               "remote-cost"))))
            (mevedel-workspace-identity-ensure remote-root)
            (setq session (mevedel-session-create "main" workspace))
            (mevedel-execution-target-probe
             (mevedel-session-execution-target session) t 'off)
            (puthash (mevedel-execution-target-identity
                      (mevedel-session-execution-target session))
                     t mevedel-session-durability--disclosed-targets)
            (with-current-buffer buffer
              (org-mode)
              (setq-local mevedel--session session)
              (insert "*** First prompt\n")
              (should (eq 'portable
                          (mevedel-session-authority-mode-for-session
                           session)))
              (mevedel-session-artifacts-save session buffer)
              ;; The settled turn a user feels: one transcript change and
              ;; queued telemetry.  The telemetry flush is a whole remote
              ;; publication transaction, so it must not run inside the
              ;; save -- its entries are still queued when save returns.
              (goto-char (point-max))
              (insert "*** Second prompt\n")
              (mevedel-telemetry-record session 'cost-probe)
              (should (mevedel-session-telemetry-pending session))
              (let ((raw-reads 0)
                    (read-raw
                     (symbol-function 'mevedel-session-artifacts-read-file-raw))
                    processes)
                ;; Publication bytes ride raw TRAMP reads, not control
                ;; programs, so the process budget alone cannot see them.
                ;; A settled save of an owned session must not read any
                ;; published artifact back.
                (cl-letf (((symbol-function
                            'mevedel-session-artifacts-read-file-raw)
                           (lambda (path)
                             (cl-incf raw-reads)
                             (funcall read-raw path))))
                  (setq processes
                        (test-mevedel-session-persistence-cost--measure
                          (mevedel-session-artifacts-save
                           session buffer))))
                (should (<= processes 17))
                (should (= 0 raw-reads)))
              (should (mevedel-session-telemetry-pending session)))))
      (when session
        (ignore-errors
          (mevedel-session-durability--cancel-renewal session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p local-root)
        (delete-directory local-root t))
      (mevedel-workspace-clear-registry))))

(provide 'test-mevedel-session-persistence-cost)

;;; test-mevedel-session-persistence-cost.el ends here
