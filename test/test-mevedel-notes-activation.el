;;; test-mevedel-notes-activation.el --- Note first-use tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise visits in cold Emacs children and note creation in real projects.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun mevedel-notes-activation-test--cold (form)
  "Run FORM with package autoloads in a fresh Emacs."
  (with-temp-buffer
    (let ((process
           (make-process
            :name "note-visit" :buffer (current-buffer) :noquery t
            :connection-type 'pipe :sentinel #'ignore
            :command
            (list (expand-file-name invocation-name invocation-directory)
                  "--batch" "-Q" "--eval"
                  (prin1-to-string
                   `(progn
                      (setq load-path ',load-path)
                      (setq native-comp-enable-subr-trampolines nil
                            native-comp-jit-compilation nil)
                      (require 'cl-lib)
                      (require 'loaddefs-gen)
                      (let* ((directory (make-temp-file "note-autoloads-" t))
                             (autoloads (file-name-concat directory "autoloads.el"))
                             (inhibit-message t))
                        (unwind-protect
                            (progn
                              (dolist (name '("mevedel-notes-activation.el" "mevedel-notes.el"))
                                (make-symbolic-link
                                 (file-name-concat
                                  ,(file-name-directory (locate-library "mevedel")) name)
                                 (file-name-concat directory name)))
                              (loaddefs-generate directory autoloads nil nil nil t)
                              (load autoloads nil t))
                          (delete-directory directory t)))
                      ,form))))))
      (with-timeout (60 (delete-process process) (ert-fail "Visit timed out"))
        (while (process-live-p process) (accept-process-output process 0.05)))
      (accept-process-output process 0)
      (ert-info ((buffer-string))
        (should (equal 0 (process-exit-status process)))
        (should (equal "" (buffer-string)))))))

(mevedel-deftest mevedel-notes-file-visit
  ()
  (let* ((root (make-temp-file
                (file-name-concat (getenv "HOME") "mevedel-note-visit-") t))
         (file (file-name-concat root "source.el")))
    (unwind-protect
        (progn
          (write-region "source code\n" nil file nil 'silent)
          ,test)
      (delete-directory root t)))
  (test)
  :doc "a file without notes loads only the visit hook and creates no state"
  (progn
    (mevedel-notes-activation-test--cold
     `(progn
        (find-file-noselect ,file)
        (dolist (feature '(mevedel mevedel-notes mevedel-persistence
                          mevedel-workspace mevedel-chat mevedel-buddy))
          (when (featurep feature) (error "Visit loaded %s" feature)))
        (when (file-exists-p ,(file-name-concat root ".mevedel"))
          (error "Visit created state")))))
  :doc "a cold file visit restores persisted labels without a session or Buddy"
  (progn
    (make-directory (file-name-concat root ".mevedel"))
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (with-current-buffer (find-file-noselect ,file)
          (mevedel-note-create "Remember on visit"))))
    (mevedel-notes-activation-test--cold
     `(with-current-buffer (find-file-noselect ,file)
        (unless (seq-some
                 (lambda (ov)
                   (string-match-p "Remember on visit"
                                   (or (overlay-get ov 'after-string) "")))
                 (overlays-in (point-min) (point-max)))
          (error "Note label did not restore"))
        (dolist (feature '(mevedel-chat mevedel-buddy))
          (when (featurep feature) (error "Visit loaded %s" feature))))))
  :doc "remembered notes restore from the visited buffer without filesystem probes"
  (progn
    (make-directory (file-name-concat root ".mevedel"))
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (let ((source (find-file-noselect ,file)))
          (with-current-buffer source (mevedel-note-create "Cached note"))
          (kill-buffer source))
        (let ((find-file-hook
               (list
                (lambda ()
                  ;; Fail filesystem work at the hook boundary, after Emacs
                  ;; has read the real source file.  TRAMP must need none.
                  (cl-letf (((symbol-function 'file-exists-p)
                             (lambda (&rest _) (error "Note hook statted a file")))
                            ((symbol-function 'file-truename)
                             (lambda (&rest _) (error "Note hook resolved a path")))
                            ((symbol-function 'process-file)
                             (lambda (&rest _) (error "Note hook ran a process")))
                            ((symbol-function 'insert-file-contents)
                             (lambda (&rest _) (error "Note hook read a file"))))
                    (mevedel-notes-file-visit))))))
          (with-current-buffer (find-file-noselect ,file)
            (unless (seq-some
                     (lambda (ov)
                       (string-match-p "Cached note" (or (overlay-get ov 'after-string) "")))
                     (overlays-in (point-min) (point-max)))
              (error "Cached note did not restore")))))))
  :doc "a returning source file restores source-missing legacy directives on visit"
  (progn
    (make-directory (file-name-concat root ".mevedel"))
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (let ((source (find-file-noselect ,file)))
          (with-current-buffer source
            (let ((transient-mark-mode t))
              (goto-char (point-min))
              (set-mark (line-end-position))
              (activate-mark)
              (cl-letf (((symbol-function 'read-from-minibuffer)
                         (lambda (&rest _) "Returning directive")))
                (mevedel-create-directive)))
            (delete-file ,file)
            (run-hooks 'post-command-hook)
            (run-hooks 'post-command-hook))
          (kill-buffer source))
        ;; A later ordinary note write persists the source-missing anchor.
        (write-region "other code\n" nil ,(file-name-concat root "other.el") nil 'silent)
        (with-current-buffer (find-file-noselect ,(file-name-concat root "other.el"))
          (mevedel-note-delete (mevedel-note-create "Trigger store write")))))
    (write-region "source code\n" nil file nil 'silent)
    (mevedel-notes-activation-test--cold
     `(let ((find-file-hook
             (list (lambda ()
                     (cl-letf (((symbol-function 'file-equal-p)
                                (lambda (&rest _) (error "Visit compared files on disk"))))
                       (mevedel-notes-file-visit))))))
        (with-current-buffer (find-file-noselect ,file)
          (unless (seq-some
                   (lambda (ov)
                     (string-match-p "DIRECTIVE" (or (overlay-get ov 'before-string) "")))
                   (overlays-in (point-min) (point-max)))
            (error "Source-missing directive did not return"))))))
  :doc "an unnoted file in a remembered workspace does not reread its store"
  (progn
    (make-directory (file-name-concat root ".mevedel"))
    (write-region "plain code\n" nil (file-name-concat root "plain.el") nil 'silent)
    (mevedel-notes-activation-test--cold
     `(progn
        (with-current-buffer (find-file-noselect ,file)
          (mevedel-note-create "Remember this workspace"))
        (let* ((reads 0)
               (reader (symbol-function 'insert-file-contents))
               (find-file-hook
                (list (lambda ()
                        (cl-letf (((symbol-function 'insert-file-contents)
                                   (lambda (&rest args)
                                     (setq reads (1+ reads))
                                     (apply reader args))))
                          (mevedel-notes-file-visit))))))
          (find-file-noselect ,(file-name-concat root "plain.el"))
          (unless (zerop reads) (error "Visit reread the store %s times" reads))))))
  :doc "a registered parent workspace never hides a nested project's notes"
  (let* ((child (file-name-concat root "child"))
         (child-file (file-name-concat child "source.el")))
    (make-directory (file-name-concat child ".mevedel") t)
    (should (zerop (call-process "git" nil nil nil "init" "-q" child)))
    (write-region "child code\n" nil child-file nil 'silent)
    (mevedel-notes-activation-test--cold
     `(with-current-buffer (find-file-noselect ,child-file)
        (mevedel-note-create "Nested project note")))
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (let ((default-directory ,(file-name-as-directory root))) (mevedel-workspace))
        (with-current-buffer (find-file-noselect ,child-file)
          (unless (seq-some
                   (lambda (ov)
                     (string-match-p "Nested project note"
                                     (or (overlay-get ov 'after-string) "")))
                   (overlays-in (point-min) (point-max)))
            (error "Parent workspace hid the child note"))))))
  :doc "an empty workspace store does not load presentation"
  (progn
    (make-directory (file-name-concat root ".mevedel"))
    (write-region "(:format 2 :files nil :directives nil :ids nil)\n" nil
                  (file-name-concat root ".mevedel/instructions.el") nil 'silent)
    (mevedel-notes-activation-test--cold
     `(progn
        (find-file-noselect ,file)
        (dolist (feature '(mevedel-notes mevedel-persistence mevedel-workspace))
          (when (featurep feature) (error "Empty store loaded %s" feature))))))
  :doc "an unknown remote file does not discover state or load presentation"
  (mevedel-notes-activation-test--cold
   `(with-current-buffer (find-file-noselect ,file)
      ;; Exercise a remote spelling at the post-read file-visit boundary.
      ;; The real SSH round-trip measurement is separate from this guard.
      (setq buffer-file-name "/ssh:mevedel-invalid-host:/project/source.el")
      (let ((find-file-hook '(mevedel-notes-file-visit)))
        (run-hooks 'find-file-hook))
      (dolist (feature '(mevedel-notes mevedel-persistence mevedel-workspace))
        (when (featurep feature) (error "Remote visit loaded %s" feature))))))

(mevedel-deftest mevedel-note-create-activation
  ()
  (let* ((root (make-temp-file "mevedel-first-note-" t))
         (file (file-name-concat root "source.el")))
    (unwind-protect
        (progn
          (write-region "source code\n" nil file nil 'silent)
          ,test)
      (delete-directory root t)))
  (test)
  :doc "the first note creates durable state in a Git project without starting AI"
  (progn
    (should (zerop (call-process "git" nil nil nil "init" "-q" root)))
    (write-region ".mevedel/\n" nil (file-name-concat root ".gitignore") nil 'silent)
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (with-current-buffer (find-file-noselect ,file)
          (mevedel-note-create "First persistent note"))
        (unless (file-exists-p ,(file-name-concat root ".mevedel/instructions.el"))
          (error "First note was not saved"))
        (when (or (featurep 'mevedel-chat) (featurep 'mevedel-buddy))
          (error "Creating a note started AI")))))
  :doc "outside a project notes stay in memory and announce unsaved status once"
  (mevedel-notes-activation-test--cold
   `(progn
      (require 'mevedel)
      (require 'project)
      (let ((project-find-functions nil) messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args)
                     (when format (push (apply #'format format args) messages)))))
          (with-current-buffer (find-file-noselect ,file)
            (mevedel-note-create "One")
            (mevedel-note-create "Two")
            (unless (= 2 (length (seq-filter
                                  (lambda (ov) (overlay-get ov 'after-string))
                                  (overlays-in (point-min) (point-max)))))
              (error "In-memory notes missing"))))
        (unless (= 1 (length (seq-filter
                              (lambda (text) (string-match-p "not saved" text))
                              messages)))
          (error "Missing or repeated unsaved notice: %S" messages)))
      (when (file-exists-p ,(file-name-concat root ".mevedel"))
        (error "Nonproject note created state"))))
  :doc "new project state reports missing gitignore coverage without editing policy"
  (progn
    (should (zerop (call-process "git" nil nil nil "init" "-q" root)))
    (mevedel-notes-activation-test--cold
     `(progn
        (require 'mevedel)
        (let (messages)
          (cl-letf (((symbol-function 'message)
                     (lambda (format &rest args)
                       (when format (push (apply #'format format args) messages)))))
            (with-current-buffer (find-file-noselect ,file)
              (mevedel-note-create "One")
              (mevedel-note-create "Two")))
          (unless (= 1 (length (seq-filter
                                (lambda (text) (string-match-p "not.*gitignore" text))
                                messages)))
            (error "Missing or repeated gitignore notice: %S" messages)))
        (when (file-exists-p ,(file-name-concat root ".gitignore"))
          (error "Note command edited gitignore"))
        (unless (file-exists-p ,(file-name-concat root ".mevedel/instructions.el"))
          (error "Unignored note was not saved")))))
  :doc "project.el projects without VCS also persist their first note"
  (mevedel-notes-activation-test--cold
   `(progn
      (require 'project)
      (let ((project-find-functions
             (list (lambda (_directory) (cons 'transient ,root))))
            messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (format &rest args)
                     (when format (push (apply #'format format args) messages)))))
          ;; This command comes directly from generated package autoloads.
          (with-current-buffer (find-file-noselect ,file)
            (mevedel-note-create "Non-VCS project")))
        (unless (file-exists-p ,(file-name-concat root ".mevedel/instructions.el"))
          (error "Project.el note was not saved"))
        (unless (seq-some (lambda (text) (string-match-p "not.*gitignore" text))
                          messages)
          (error "Missing ignore notice for non-VCS project"))))))

(provide 'test-mevedel-notes-activation)
;;; test-mevedel-notes-activation.el ends here
