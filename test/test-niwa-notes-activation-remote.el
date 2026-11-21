;;; test-niwa-notes-activation-remote.el --- Remote note activation tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the roots-only activation index through real store operations.

;;; Code:

(require 'niwa)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-execution-target)
(require 'niwa-session-control-fs)
(require 'tramp-sh)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-notes-remote-index
  (:quiet t)
  (niwa-test--with-local-shell-tramp '("notes-index")
    (let* ((local (file-name-as-directory (make-temp-file "notes-index-" t)))
           (root (format "/niwamock:notes-index:%s" local))
           (index (file-name-concat (getenv "XDG_STATE_HOME") "niwa/remote-notes.el"))
           (workspace (niwa-workspace-get-or-create 'project root root "notes"))
           (source nil))
      (unwind-protect
          (progn
            (make-directory (file-name-concat local ".niwa"))
            (write-region "source code\n" nil (file-name-concat local "source.el") nil 'silent)
            ;; Establish the already-read source before exercising the notes
            ;; visit boundary; index fixtures must precede its first lookup.
            (let ((find-file-hook (remq #'niwa-notes-file-visit find-file-hook)))
              (setq source (find-file-noselect (file-name-concat root "source.el"))))
            (with-current-buffer source (setq-local niwa--workspace workspace))
            ,test)
        (when (buffer-live-p source) (kill-buffer source))
        (setq niwa-instruction-store--dirty nil)
        (remove-hook 'post-command-hook #'niwa-instruction-store-flush)
        (delete-directory local t))))
  (test)
  :doc "a remote note write remembers only its root, not source or note content"
  (with-current-buffer source
    (niwa-note-create "Private note text")
    (should (file-exists-p index))
    (should (equal (with-temp-buffer
                     (insert-file-contents index)
                     (read (current-buffer)))
                   (list root))))
  :doc "a remote store with no remaining notes drops its indexed root"
  (with-current-buffer source
    (let ((niwa-notes-closed-max-count 0))
      (niwa-note-delete (niwa-note-create "Remove completely"))
      (niwa-instruction-store-sync workspace))
    (should (equal (with-temp-buffer
                     (insert-file-contents index)
                     (read (current-buffer)))
                   nil)))
  :doc "a note write merges the current root index instead of its stale cache"
  (let* ((other "/niwamock:notes-index:/other/")
         (niwa-notes-activation--remote-index
          (cons index '("/niwamock:notes-index:/stale/")))
         (rename (symbol-function 'rename-file))
         (renames 0))
    (make-directory (file-name-directory index) t)
    (with-temp-file index (prin1 (list other) (current-buffer)))
    (cl-letf (((symbol-function 'rename-file)
               (lambda (from to &rest args)
                 (when (equal to index)
                   (cl-incf renames)
                   (should (equal (with-temp-buffer
                                    (insert-file-contents index) (read (current-buffer)))
                                  (list other))))
                 (apply rename from to args))))
      (with-current-buffer source (niwa-note-create "Remember both clients")))
    (should (= 1 renames))
    (should (equal (sort (with-temp-buffer
                          (insert-file-contents index) (read (current-buffer))) #'string<)
                   (sort (list root other) #'string<))))
  :doc "a local note never creates the remote-roots index"
  (let ((buffer (find-file-noselect (file-name-concat local "source.el"))))
    (unwind-protect
        (with-current-buffer buffer
          (setq-local niwa--workspace
                      (niwa-workspace-get-or-create 'project local local "local"))
          (niwa-note-create "Local only")
          (should-not (file-exists-p index)))
      (kill-buffer buffer)))
  :doc "an unindexed remote visit submits no notes command or background read"
  (let* ((submissions nil)
         (counter (lambda (&rest args) (push (cadr args) submissions))))
    (unwind-protect
        (progn
          (advice-add 'tramp-send-command :before counter)
          (with-current-buffer source (niwa-notes-file-visit))
          (sleep-for 0.1)
          (should-not submissions)
          (should-not (file-exists-p index)))
      (advice-remove 'tramp-send-command counter)))
  :doc "a cold remote read names the workspace without project discovery"
  (let* ((niwa-workspace--registry (make-hash-table :test #'equal))
         (probes 0)
         (counter (lambda (&rest _) (cl-incf probes))))
    (unwind-protect
        (progn
          (advice-add 'project-current :before counter)
          (niwa-instruction-store-read-connected root)
          (should (equal (file-name-nondirectory (directory-file-name local))
                         (niwa-workspace-name
                          (gethash (cons 'project root) niwa-workspace--registry))))
          (should (zerop probes)))
      (advice-remove 'project-current counter)))
  :doc "a stale indexed unsupported target skips once without synchronous fallback"
  (progn
    (make-directory (file-name-directory index) t)
    (with-temp-file index (prin1 (list root) (current-buffer)))
    (let* ((submissions nil)
           (counter (lambda (&rest args) (push (cadr args) submissions)))
           diagnostics)
      (unwind-protect
          (niwa-test--with-captured-diagnostics diagnostics
            (advice-add 'tramp-send-command :before counter)
            (dotimes (_ 3)
              (with-current-buffer source (niwa-notes-file-visit)))
            (sleep-for 0.1)
            (with-current-buffer source (niwa-notes-file-visit))
            (sleep-for 0.1)
            (should-not submissions))
        (advice-remove 'tramp-send-command counter))
      (should (= 1 (with-temp-buffer
                     (insert diagnostics)
                     (how-many "Background note restore skipped" (point-min) (point-max))))))))

(niwa-deftest niwa-session-control-fs-run-program-async/connected-only
  ()
  ,test
  (test)
  :doc "a disconnected target cannot fall back to a synchronous read or reconnect"
  (let (delivered)
    (should-not
     (niwa-session-control-fs-run-program-async
      '((:op read :path "/ssh:niwa-invalid-host:/project/.niwa/instructions.el"
              :optional t))
      (lambda (results failure) (setq delivered (list results failure)))
      'global nil t))
    (should (equal (caar (cdr delivered)) 'file-error))
    (should-not (car delivered)))
  :doc "connected-only still fails closed when ordinary async programs would run synchronously"
  (let ((niwa-session-control-fs--async-wait t)
        (calls 0)
        delivered)
    (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (&rest _) (ert-fail "Connected-only used synchronous fallback"))))
      (should-not
       (niwa-session-control-fs-run-program-async
        '((:op read :path "/ssh:niwa-invalid-host:/project/.niwa/instructions.el"
                :optional t))
        (lambda (results failure)
          (cl-incf calls)
          (setq delivered (list results failure)))
        'global nil t)))
    (should (= calls 1))
    (should-not (car delivered))
    (should (eq 'file-error (car (cadr delivered)))))
  :doc "a connected SSH read returns real bytes without synchronous target probes"
  (let* ((root (or (getenv "NIWA_TEST_SSH_ROOT")
                   (ert-skip "NIWA_TEST_SSH_ROOT is not set")))
         (niwa-test--write-roots (cons root niwa-test--write-roots))
         (tramp-histfile-override t)
         (tramp-use-connection-share t)
         (tramp-ssh-controlmaster-options
          (format "-F %s -o ControlMaster=auto -o ControlPath=%s -o ControlPersist=no"
                  (getenv "NIWA_TEST_SSH_CONFIG")
                  (file-name-concat temporary-file-directory "ssh.%%C")))
         (file (file-name-concat root "ticket-07b-carrier"))
         (pipe-writes 0)
         (pipe-counter (lambda (process &rest _)
                         (when (equal (process-name process) "niwa-control-fs-async")
                           (setq pipe-writes (1+ pipe-writes)))))
         (submissions nil)
         (counter (lambda (&rest args) (push (cadr args) submissions)))
         delivered)
    (unwind-protect
        (progn
          (should-not (file-exists-p file))
          (write-region "Connected bytes\n" nil file nil 'silent)
          (advice-add 'tramp-send-command :before counter)
          (advice-add 'process-send-region :before pipe-counter)
          (let ((process
                 (niwa-session-control-fs-run-program-async
                  (list (list :op 'read :path file :coding 'no-conversion))
                  (lambda (results failure) (setq delivered (list results failure)))
                  root nil t)))
            (should (processp process))
            (should-not delivered)
            (with-timeout (15 (ert-fail "Connected read timed out"))
              (while (not delivered) (accept-process-output nil 0.02))))
          (should-not (cadr delivered))
          (should (equal "Connected bytes\n"
                         (niwa-session-control-fs-program-value (caar delivered))))
          ;; A stalled mux must not block Emacs sending a script to a full pipe.
          (should (zerop pipe-writes))
          (should-not submissions)
          ;; A vanished mux must fail even while TRAMP's shell still looks live.
          ;; Without ProxyCommand=false, SSH would authenticate and read again.
          (setq delivered nil)
          (let ((tramp-ssh-controlmaster-options
                 (replace-regexp-in-string
                  "ControlPath=[^ ]+"
                  (format "ControlPath=%s" (file-name-concat temporary-file-directory "missing.%%C"))
                  tramp-ssh-controlmaster-options t t)))
            (should (processp
                     (niwa-session-control-fs-run-program-async
                      (list (list :op 'read :path file :coding 'no-conversion))
                      (lambda (results failure) (setq delivered (list results failure)))
                      root nil t)))
            (with-timeout (15 (ert-fail "Missing master read timed out"))
              (while (not delivered) (accept-process-output nil 0.02))))
          (should-not (car delivered))
          (should (eq 'file-error (car (cadr delivered))))
          (should-not submissions))
      (advice-remove 'tramp-send-command counter)
      (advice-remove 'process-send-region pipe-counter)
      (when (file-exists-p file) (delete-file file))
      (tramp-cleanup-all-connections))))

(provide 'test-niwa-notes-activation-remote)
;;; test-niwa-notes-activation-remote.el ends here
