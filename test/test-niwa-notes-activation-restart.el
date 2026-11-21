;;; test-niwa-notes-activation-restart.el --- SSH restart acceptance -*- lexical-binding: t -*-

;;; Commentary:
;; Opt in with NIWA_TEST_SSH_ROOT and NIWA_TEST_SSH_CONFIG.  The configured
;; SSH fixture must contain only an empty, pre-existing .niwa directory.
;; Separate writer and reader Emacsen share only isolated HOME/XDG state.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'tramp-sh)

(defun niwa-notes-restart-test--cold (directory config form)
  "Run FORM in a cold Emacs using DIRECTORY for SSH sharing and CONFIG."
  (with-temp-buffer
    (let ((process
           (make-process
            :name "notes-restart" :buffer (current-buffer) :noquery t
            :connection-type 'pipe :sentinel #'ignore
            :command
            (list (expand-file-name invocation-name invocation-directory)
                  "--batch" "-Q" "--eval"
                  (prin1-to-string
                   `(progn
                      (setq load-path ',load-path
                            native-comp-enable-subr-trampolines nil
                            native-comp-jit-compilation nil
                            niwa-note-labels 'expanded)
                      (require 'cl-lib)
                      (require 'tramp-sh)
                      (setq tramp-histfile-override t
                            tramp-persistency-file-name nil
                            tramp-use-connection-share t
                            tramp-ssh-controlmaster-options
                            ,(format "-F %s -o ControlMaster=auto -o ControlPath=%s -o ControlPersist=no"
                                     config (file-name-concat directory "ssh.%%C")))
                      (require 'loaddefs-gen)
                      (let* ((directory (make-temp-file "note-autoloads-" t))
                             (autoloads (file-name-concat directory "autoloads.el"))
                             (inhibit-message t))
                        (unwind-protect
                            (progn
                              (dolist (name '("niwa-notes-activation.el" "niwa-notes.el"))
                                (make-symbolic-link
                                 (file-name-concat
                                  ,(file-name-directory (locate-library "niwa")) name)
                                 (file-name-concat directory name)))
                              (loaddefs-generate directory autoloads nil nil nil t)
                              (load autoloads nil t))
                          (delete-directory directory t)))
                      (let ((inhibit-message t)) ,form)))))))
      (unwind-protect
          (with-timeout (90 (ert-fail "Restart child timed out"))
            (while (process-live-p process) (accept-process-output process 0.05)))
        ;; Remote cleanup must never race a still-running writer child.
        (when (process-live-p process) (delete-process process))
        (while (process-live-p process) (accept-process-output process 0.05)))
      (accept-process-output process 0)
      (ert-info ((buffer-string))
        (should (equal 0 (process-exit-status process)))
        (should (equal "" (buffer-string)))))))

(niwa-deftest niwa-notes-remote-restart
  (:quiet t)
  (let* ((compiled ,variant)
         (root (or (getenv "NIWA_TEST_SSH_ROOT")
                   (ert-skip "NIWA_TEST_SSH_ROOT is not set")))
         (config (or (getenv "NIWA_TEST_SSH_CONFIG")
                     (ert-skip "NIWA_TEST_SSH_CONFIG is not set")))
         (directory (make-temp-file "notes-restart-" t))
         (build (and compiled (file-name-concat directory "build")))
         (source-root (file-name-directory (locate-library "niwa")))
         (load-path (if build (cons build load-path) load-path))
         (index (file-name-concat (getenv "XDG_STATE_HOME") "niwa/remote-notes.el"))
         (result (file-name-concat directory "result.el"))
         (sources (mapcar (lambda (suffix)
                            (file-name-concat root
                                              (format "%s-%s.el"
                                                      (file-name-nondirectory directory) suffix)))
                          '(one two)))
         (state (file-name-concat root ".niwa"))
         (store (file-name-concat state "instructions.el"))
         (leases (file-name-concat state "leases"))
         (niwa-test--write-roots (cons root niwa-test--write-roots))
         (tramp-histfile-override t)
         (tramp-persistency-file-name nil)
         (tramp-use-connection-share t)
         (tramp-ssh-controlmaster-options
          (format "-F %s -o ControlMaster=auto -o ControlPath=%s -o ControlPersist=no"
                  config (file-name-concat directory "ssh.%%C")))
         owned)
    (unwind-protect
        (progn
          (should (equal (file-remote-p root 'method) "ssh"))
          (should-not (file-remote-p root 'hop))
          (should (file-exists-p config))
          (should (equal (directory-files root nil directory-files-no-dot-files-regexp)
                         '(".niwa")))
          (should-not (directory-files state nil directory-files-no-dot-files-regexp))
          (dolist (file (append sources (list store leases)))
            (should-not (file-exists-p file)))
          (setq owned t)
          (should-not (file-exists-p index))
          (when build
            (make-directory build)
            (niwa-notes-restart-test--cold
             directory config
             `(progn
                (require 'bytecomp)
                (let ((byte-compile-verbose nil) (byte-compile-error-on-warn t)
                      (byte-compile-dest-file-function
                       (lambda (file)
                         (file-name-concat ,build (concat (file-name-nondirectory file) "c")))))
                  (dolist (name '("niwa-notes-activation.el" "niwa-persistence.el"
                                  "niwa-session-control-fs.el"))
                    (unless (byte-compile-file (file-name-concat ,source-root name))
                      (error "Compilation failed: %s\n%s" name
                             (if-let* ((log (get-buffer byte-compile-log-buffer)))
                                 (with-current-buffer log (buffer-string))
                               "No compiler log buffer"))))))))
          (niwa-notes-restart-test--cold
           directory config
           `(progn
              (dolist (file ',sources)
                (write-region ";; source code\n" nil file nil 'silent))
              (cl-mapc (lambda (file text)
                         (with-current-buffer (find-file-noselect file)
                           (niwa-note-create text)))
                       ',sources '("Restart one" "Restart two"))))
          (tramp-flush-directory-properties (tramp-dissect-file-name root)
                                            (file-remote-p root 'localname))
          (should (file-exists-p store))
          (should (equal (with-temp-buffer
                           (insert-file-contents index) (read (current-buffer)))
                         (list root)))
          (delete-file index)
          ;; Relearn the roots-only index through observation, without visits.
          (niwa-notes-restart-test--cold
           directory config
           `(progn
              (require 'niwa-workspace)
              (require 'niwa-persistence)
              (niwa-instruction-store-observe
               (niwa-workspace-get-or-create 'project ,root ,root "Restart observer"))))
          (should (equal (with-temp-buffer
                           (insert-file-contents index) (read (current-buffer)))
                         (list root)))
          (niwa-notes-restart-test--cold
           directory config
           `(let ((inside-hook nil) (hook-reads 0) (hook-commands 0)
                  (commands 0) (hook-persistence 0) (jobs 0) (safe-carriers t)
                  (preserved t) (same-connection t) (restored-every-visit t) connection labels)
              (when (featurep 'niwa-persistence) (error "Reader was not cold"))
              (advice-add
               'niwa-notes-file-visit :around
               (lambda (original &rest args)
                 (let ((loaded (featurep 'niwa-persistence)))
                   (setq inside-hook t)
                   (unwind-protect (apply original args)
                     (setq inside-hook nil)
                     (when (and (not loaded) (featurep 'niwa-persistence))
                       (cl-incf hook-persistence))))))
              (dolist (reader '(insert-file-contents insert-file-contents-literally))
                (advice-add reader :before
                            (lambda (&rest args)
                              (when (and inside-hook (file-remote-p (car args)))
                                (cl-incf hook-reads)))))
              (advice-add 'tramp-send-command :before
                          (lambda (&rest _)
                            (cl-incf commands)
                            (when inside-hook (cl-incf hook-commands))))
              (advice-add
               'make-process :before
               (lambda (&rest args)
                 (when (equal (plist-get args :name) "niwa-control-fs-async")
                   (cl-incf jobs)
                   (let ((argv (plist-get args :command)))
                     (unless (and (member "ControlMaster=no" argv)
                                  (member "ProxyCommand=false" argv)
                                  (member "ProxyJump=none" argv)
                                  (member "BatchMode=yes" argv))
                       (setq safe-carriers nil))))))
              ;; Kill and genuinely revisit both files, not just existing buffers.
              (dotimes (_ 2)
                (let ((buffers (mapcar #'find-file-noselect ',sources)) snapshots)
                  (unless connection
                    (setq connection
                          (tramp-get-connection-process
                           (tramp-dissect-file-name ,root))))
                  (dolist (buffer buffers)
                    (with-current-buffer buffer
                      (unless (equal (buffer-string) ";; source code\n")
                        (setq preserved nil))
                      (goto-char (point-max))
                      (insert ";; user edit\n")
                      (goto-char 3)
                      (push (list buffer (buffer-string) (point)) snapshots)))
                  (let ((deadline (+ (float-time) 10)))
                    (while
                        (progn
                          (setq labels
                                (mapcar
                                 (lambda (buffer)
                                   (with-current-buffer buffer
                                     (delq nil
                                           (mapcar
                                            (lambda (overlay)
                                              (let ((text (overlay-get overlay 'after-string)))
                                                (when (and text
                                                           (string-match
                                                            "Note [0-9]+ · You  \\(.*\\)\\'" text))
                                                  (match-string 1 text))))
                                            (overlays-in (point-min) (point-max))))))
                                 buffers))
                          (and (not (equal labels '(("Restart one") ("Restart two"))))
                               (< (float-time) deadline)))
                      (accept-process-output nil 0.02)))
                  (unless (equal labels '(("Restart one") ("Restart two")))
                    (setq restored-every-visit nil))
                  ;; Let delayed duplicate submissions run before counting them.
                  (sit-for 0.2)
                  (dolist (snapshot snapshots)
                    (with-current-buffer (car snapshot)
                      (unless (and (equal (buffer-string) (cadr snapshot))
                                   (= (point) (caddr snapshot)) (buffer-modified-p))
                        (setq preserved nil))
                      (set-buffer-modified-p nil)
                      (kill-buffer)))
                  (unless (and (process-live-p connection)
                               (eq connection
                                   (tramp-get-connection-process
                                    (tramp-dissect-file-name ,root))))
                    (setq same-connection nil))))
              (when ,compiled
                (dolist (symbol '(niwa-notes-file-visit niwa-instruction-store-read-connected
                                  niwa-session-control-fs-run-program-async))
                  (let ((file (symbol-file symbol 'defun)))
                    (unless (and file (string-prefix-p ,build file) (string-suffix-p ".elc" file))
                      (error "Reader did not load private bytecode for %s: %S" symbol file)))))
              (with-temp-file ,result
                (prin1 (list :labels labels :hook-reads hook-reads
                             :hook-commands hook-commands :commands commands
                             :hook-persistence hook-persistence :jobs jobs
                             :restored-every-visit restored-every-visit
                             :safe-carriers safe-carriers :preserved preserved
                             :same-connection same-connection
                             :warnings (when (get-buffer "*Warnings*")
                                         (with-current-buffer "*Warnings*" (buffer-string))))
                       (current-buffer)))))
          (let ((counts (with-temp-buffer
                          (insert-file-contents result) (read (current-buffer)))))
            (ert-info ((format "Restart observations: %S" counts))
              (should (equal (plist-get counts :labels)
                             '(("Restart one") ("Restart two"))))
              (dolist (key '(:hook-reads :hook-commands :hook-persistence))
                (should (zerop (plist-get counts key))))
              (should (= 1 (plist-get counts :jobs)))
              (dolist (key '(:restored-every-visit :safe-carriers :preserved :same-connection))
                (should (plist-get counts key))))))
      (when owned
        (tramp-flush-directory-properties (tramp-dissect-file-name root)
                                          (file-remote-p root 'localname))
        (dolist (file (append sources (list store)))
          (when (file-exists-p file) (delete-file file)))
        (when (file-directory-p leases) (delete-directory leases t))
        (should (equal (directory-files root nil directory-files-no-dot-files-regexp)
                       '(".niwa")))
        (should-not (directory-files state nil directory-files-no-dot-files-regexp)))
      (tramp-cleanup-all-connections)
      (delete-directory directory t)))
  (variant)
  :doc "cold source SSH visits restore observed notes once without hook I/O, reconnects, or lost edits"
  nil
  :doc "cold compiled SSH visits restore observed notes once without hook I/O, reconnects, or lost edits"
  t)

(provide 'test-niwa-notes-activation-restart)
;;; test-niwa-notes-activation-restart.el ends here
