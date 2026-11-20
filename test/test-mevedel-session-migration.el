;;; test-mevedel-session-migration.el --- Explicit migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the standalone converter on real files, then use the normal reader.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "mevedel-session-test-support"))
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; converter under test.
(require 'mevedel-migrate-session
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-sessions.el"))

(defun mevedel-migration-test--stable-attributes (file)
  "Return FILE's attributes except access time, which reads may change."
  (let ((attributes (file-attributes file)))
    (setf (nth 4 attributes) nil)
    attributes))

(defconst mevedel-migration-test--steps
  `(("v0.5.99" ,mevedel-session-codec-format-version identity))
  "Test-only upgrade to exercise staging and installation.")

(defmacro mevedel-migration-test--with-source (version &rest body)
  "Run BODY with a two-head VERSION session and a separate destination."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "mevedel-migration-" t))
          (source (file-name-concat root "old"))
          (destination (file-name-concat root "converted"))
          (session (test-mevedel-session-persistence--make-session root))
          (old (mevedel-session-codec-serialize session))
          (head ".publications/generation-bbbbbbbbbbbbbbbbbbbb/manifest.el")
          (lease-file (file-name-concat source ".lease/00000000000000000001.el")))
     (unwind-protect
         (progn
           (setq old (plist-put old :version ,version))
           ;; The native writer shares equal object identities with #N=/#N#.
           (setq old (plist-put old :first-user-message "Shared preview"))
           (setq old (plist-put old :latest-user-message (plist-get old :first-user-message)))
           (make-directory (file-name-concat source ".lease") t)
           (mevedel-migrate-session--write (file-name-concat source "session.meta.el") old)
           (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
             (let* ((directory (concat ".publications/generation-" name))
                    (meta (concat directory "/000001.data"))
                    (chat (concat directory "/000002.data")))
               (make-directory (file-name-concat source directory) t)
               (mevedel-migrate-session--write (file-name-concat source meta) old)
               (with-temp-file (file-name-concat source chat)
                 (insert "#+title: Kept transcript\nUser and assistant text.\n"))
               (mevedel-migrate-session--write
                (file-name-concat source directory "manifest.el")
                (list :sidecar "session.meta.el" :artifacts
                      (list (list "session.meta.el" :published meta :sha256
                                  (mevedel-migrate-session--hash (file-name-concat source meta)))
                            (list "segment-0001.chat.org" :published chat :sha256
                                  (mevedel-migrate-session--hash (file-name-concat source chat))))))))
           (mevedel-migrate-session--write
            lease-file (list :generation 1 :transfer-generation 1 :status 'released
                             :publication-head head :unsettled-mutation nil
                             :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
           ,@body)
       (delete-directory root t)
       (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-migrate-session-copy (:quiet t)
  ,test
  (test)
  :doc "drops retained generations and fixed caches no reader accepts"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let* ((stale ".publications/generation-aaaaaaaaaaaaaaaaaaaa")
           (data (concat stale "/000001.data"))
           (manifest-file (file-name-concat source stale "manifest.el"))
           (manifest (mevedel-migrate-session--read manifest-file)))
      (mevedel-migrate-session--write (file-name-concat source data)
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
            (mevedel-migrate-session--hash (file-name-concat source data)))
      (mevedel-migrate-session--write manifest-file manifest)
      (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (should (mevedel-migrate-session-copy source destination))
      (should-not (file-exists-p (file-name-concat destination stale "manifest.el")))
      (should (file-exists-p (file-name-concat destination data)))
      (should-not (file-exists-p (file-name-concat destination "session.meta.el")))
      (should (file-exists-p (file-name-concat destination head)))
      (let ((before (mevedel-migrate-session--snapshot source)))
        (should (eq 'current (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
        (should (equal before (mevedel-migrate-session--snapshot source))))))

  :doc "takes an expired active lease, left by a crash, as closed"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (mevedel-migrate-session--write
     lease-file (plist-put (mevedel-migrate-session--read lease-file) :status 'active))
    (should (mevedel-migrate-session-copy source destination)))

  :doc "refuses corrupt, unsupported, live and unsafe sources without changing them"
  (dolist (fault '(version checksum path active unsettled missing-lease lock recovery symlink))
    (mevedel-migration-test--with-source mevedel-session-codec-format-version
      (pcase fault
        ;; The published head's sidecar; a stale fixed cache would be dropped.
        ('version
         (let* ((data ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000001.data")
                (file (file-name-concat source head))
                (manifest (mevedel-migrate-session--read file)))
           (mevedel-migrate-session--write (file-name-concat source data)
                                           (plist-put (copy-tree old) :version "v0.5.0"))
           (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                 (mevedel-migrate-session--hash (file-name-concat source data)))
           (mevedel-migrate-session--write file manifest)))
        ('checksum (with-temp-file (file-name-concat source ".publications/generation-aaaaaaaaaaaaaaaaaaaa/000002.data")
                     (insert "corrupt")))
        ('path (let* ((file (file-name-concat source head))
                      (manifest (mevedel-migrate-session--read file)))
                 (setf (plist-get (cdar (plist-get manifest :artifacts)) :published) "../../outside")
                 (mevedel-migrate-session--write file manifest)))
        ((or 'active 'unsettled)
         (let ((lease (mevedel-migrate-session--read lease-file)))
           ;; A live holder still renews its lease.
           (setq lease (if (eq fault 'active)
                           (plist-put (plist-put lease :status 'active)
                                      :expires-at (+ (float-time) 600))
                         (plist-put lease :unsettled-mutation t)))
           (mevedel-migrate-session--write lease-file lease)))
        ('missing-lease (delete-directory (file-name-concat source ".lease") t))
        ('lock (with-temp-file (file-name-concat source ".lock") (insert "held")))
        ('recovery (make-directory (file-name-concat source ".recovery"))
                   (with-temp-file (file-name-concat source ".recovery/pending") (insert "repair")))
        ('symlink (make-symbolic-link root (file-name-concat source "escape"))))
      (let ((before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
        (should-error (mevedel-migrate-session-copy source destination))
        (should-not (file-exists-p destination))
        (should (equal before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el")))))))

  :doc "never overwrites an existing destination or copies into itself"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (make-directory destination)
    (should-error (mevedel-migrate-session-copy source destination))
    (should (file-directory-p destination))
    (should-error (mevedel-migrate-session-copy source (file-name-concat source "nested")))
    (should-not (file-exists-p (file-name-concat source "nested")))))

(mevedel-deftest mevedel-migrate-session--read ()
  (dolist (text '("#1=(:a #1#)" "(:a 1) (:b 2)" "(:a 1 :a 2)" "(setq migration-executed t)"))
    (let ((file (make-temp-file "mevedel-migration-read-")))
      (unwind-protect
          (progn
            (with-temp-file file (insert text))
            (should-error (mevedel-migrate-session--read file)))
        (delete-file file)))))

(mevedel-deftest mevedel-migrate-session-main (:quiet t)
  ,test
  (test)
  :doc "help needs no directory and invalid argument lists fail"
  (let ((command-line-args-left '("--" "--help")))
    (should (string-search "DIRECTORY" (with-output-to-string (mevedel-migrate-session-main))))
    (should-not command-line-args-left))
  (dolist (args '(nil ("--") ("--" "one" "two") ("--unknown")))
    (let ((command-line-args-left args))
      (should-error (mevedel-migrate-session-main)))))

(mevedel-deftest mevedel-migrate-sessions (:quiet t)
  ,test
  (test)
  :doc "in-place migration preserves original bytes in a backup and reruns are a no-op"
  (mevedel-migration-test--with-source "v0.5.99"
    (let* ((mevedel-migrate-session--steps mevedel-migration-test--steps)
           (before (mapcar (lambda (file)
                            (cons (file-relative-name file source)
                                  (mevedel-migrate-session--hash file)))
                          (directory-files-recursively source ".")))
           (row (cdar (mevedel-migrate-sessions source)))
           (backup (plist-get row :backup)))
      (should (eq 'migrated (plist-get row :status)))
      (should (file-directory-p backup))
      (should-not (file-in-directory-p backup source))
      (dolist (entry before)
        (should (equal (cdr entry)
                       (mevedel-migrate-session--hash (file-name-concat backup (car entry))))))
      (let ((files (directory-files root t))
            (attributes (mevedel-migration-test--stable-attributes (file-name-concat source head))))
        (should (eq 'current (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
        (should (equal files (directory-files root t)))
        (should (equal attributes (mevedel-migration-test--stable-attributes
                                   (file-name-concat source head)))))
      (should (equal "v0.5.99"
                     (plist-get (mevedel-migrate-session--read
                                 (file-name-concat backup "session.meta.el")) :version)))))

  :doc "already-current metadata is not rewritten or backed up"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let ((before (directory-files root t))
          (attributes (mevedel-migration-test--stable-attributes
                       (file-name-concat source "session.meta.el"))))
      (should (eq 'current (plist-get (cdar (mevedel-migrate-sessions source)) :status)))
      (should (equal before (directory-files root t)))
      (should (equal attributes (mevedel-migration-test--stable-attributes
                                 (file-name-concat source "session.meta.el"))))))

  :doc "project migration continues after unsupported sessions and leaves them unchanged"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let* ((sessions (file-name-concat root ".mevedel" "sessions"))
           (good (file-name-concat sessions "good"))
           (bad (file-name-concat sessions "unsupported")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory bad)
      (make-directory (file-name-concat sessions ".migration-backup-hidden"))
      (mevedel-migrate-session--write (file-name-concat bad "session.meta.el")
                                      '(:version "v0.5.16"))
      (let* ((before (mevedel-migrate-session--hash (file-name-concat bad "session.meta.el")))
             (report (mevedel-migrate-sessions root)))
        (should (eq 'current (plist-get (cdr (assoc "good" report)) :status)))
        (should (eq 'failed (plist-get (cdr (assoc "unsupported" report)) :status)))
        (should (string-search "unsupported version v0.5.16"
                               (plist-get (cdr (assoc "unsupported" report)) :reason)))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat bad "session.meta.el")))))
      ;; Backups/staging must not become newly discoverable sessions.
      (should (equal '("good" "unsupported")
                     (mapcar #'car (mevedel-migrate-sessions sessions))))))

  :doc "a project with a live session is refused before any other session is changed"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let* ((sessions (file-name-concat root ".mevedel" "sessions"))
           (good (file-name-concat sessions "a-closed"))
           (live (file-name-concat sessions "z-live")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory live)
      (mevedel-migrate-session--write (file-name-concat live "session.meta.el") old)
      (with-temp-file (file-name-concat live ".lock") (insert "held"))
      (let ((before (mevedel-migrate-session--hash (file-name-concat good "session.meta.el"))))
        (should-error (mevedel-migrate-sessions root))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat good "session.meta.el")))))))

  :doc "installation failure restores the original instead of leaving its path missing"
  (mevedel-migration-test--with-source "v0.5.99"
    (let ((mevedel-migrate-session--steps mevedel-migration-test--steps)
          (rename (symbol-function 'rename-file))
          (before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el")))
          injected)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional overwrite)
                   (if (and (not injected)
                            (equal (directory-file-name to) (directory-file-name source)))
                       (progn (setq injected t) (error "Injected install failure"))
                     (funcall rename from to overwrite)))))
        (let ((row (cdar (mevedel-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))
          (should (string-search "Injected install failure" (plist-get row :reason)))))
      (should (equal before (mevedel-migrate-session--hash
                             (file-name-concat source "session.meta.el"))))))

  :doc "source changes during staging abort replacement"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let ((copy (symbol-function 'mevedel-migrate-session-copy))
          (chat (file-name-concat source ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000002.data"))
          injected)
      (cl-letf (((symbol-function 'mevedel-migrate-session-copy)
                 (lambda (&rest args)
                   (prog1 (apply copy args)
                     (unless injected
                       (setq injected t)
                       (write-region "\nConcurrent edit" nil chat t 'silent))))))
        (let ((row (cdar (mevedel-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))))
      (should (equal mevedel-session-codec-format-version
                     (plist-get (mevedel-migrate-session--read
                                 (file-name-concat source "session.meta.el")) :version)))
      (should (string-suffix-p "Concurrent edit"
                               (with-temp-buffer (insert-file-contents chat) (buffer-string))))))

  :doc "project state links are refused rather than migrating their target"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let* ((state (file-name-concat root "elsewhere"))
           (sessions (file-name-concat state "sessions"))
           (actual (file-name-concat sessions "session")))
      (rename-file (file-name-concat root ".mevedel") state)
      (make-directory sessions t)
      (rename-file source actual)
      (make-symbolic-link state (file-name-concat root ".mevedel"))
      (let ((before (mevedel-migrate-session--hash (file-name-concat actual "session.meta.el"))))
        (should-error (mevedel-migrate-sessions root))
        (should (equal before (mevedel-migrate-session--hash
                               (file-name-concat actual "session.meta.el")))))))

  :doc "an existing migration lock blocks replacement and is not removed by the contender"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let ((lock (file-name-concat root ".migration-lock"))
          (before (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
      (make-directory lock)
      (should-error (mevedel-migrate-sessions source))
      (should (file-directory-p lock))
      (should (equal before (mevedel-migrate-session--hash
                             (file-name-concat source "session.meta.el"))))))

  :doc "nil authoritative metadata is invalid, not an unpublished empty session"
  (dolist (authority '(fixed portable))
    (mevedel-migration-test--with-source mevedel-session-codec-format-version
      (if (eq authority 'fixed)
          (progn
            (delete-directory (file-name-concat source ".lease") t)
            (delete-directory (file-name-concat source ".publications") t)
            (mevedel-migrate-session--write (file-name-concat source "session.meta.el") nil))
        (let* ((file (file-name-concat source (file-name-directory head) "000001.data"))
               (manifest-file (file-name-concat source head))
               (manifest (mevedel-migrate-session--read manifest-file)))
          (mevedel-migrate-session--write file nil)
          (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                (mevedel-migrate-session--hash file))
          (mevedel-migrate-session--write manifest-file manifest)))
      (let ((before (directory-files root t))
            (row (cdar (mevedel-migrate-sessions source))))
        (should (eq 'failed (plist-get row :status)))
        (should (stringp (plist-get row :reason)))
        (should (equal before (directory-files root t))))))

  :doc "unsupported, future and missing versions fail without a source rewrite"
  (dolist (version '("v0.5.16" "v99.0.0" nil))
    (mevedel-migration-test--with-source mevedel-session-codec-format-version
      (delete-directory (file-name-concat source ".lease") t)
      (delete-directory (file-name-concat source ".publications") t)
      (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                      (list :version version))
      (let ((before (directory-files root t))
            (digest (mevedel-migrate-session--hash (file-name-concat source "session.meta.el"))))
        (let ((row (cdar (mevedel-migrate-sessions source))))
          (should (eq 'failed (plist-get row :status)))
          (should (string-search (format "unsupported version %s" version)
                                 (plist-get row :reason))))
        (should (equal before (directory-files root t)))
        (should (equal digest (mevedel-migrate-session--hash
                               (file-name-concat source "session.meta.el"))))))))

(mevedel-deftest mevedel-migrate-session-cli (:quiet t)
  ,test
  (test)
  :doc "real batch invocation accepts current sessions and rejects unsupported versions"
  (mevedel-migration-test--with-source mevedel-session-codec-format-version
    (let ((script (file-name-concat (file-name-directory (locate-library "mevedel"))
                                   "scripts" "migrate-sessions.el")))
      (dolist (expected '(0 1))
        (when (= expected 1)
          (delete-directory (file-name-concat source ".lease") t)
          (delete-directory (file-name-concat source ".publications") t)
          (mevedel-migrate-session--write (file-name-concat source "session.meta.el")
                                          '(:version "v0.5.16")))
        (with-temp-buffer
          (let ((status (call-process
                         (expand-file-name invocation-name invocation-directory) nil t nil
                         "--batch" "-Q" "--eval"
                         (prin1-to-string `(setq load-path ',load-path debug-on-error nil))
                         "-l" script "-f" "mevedel-migrate-session-main" "--" source)))
            (if (zerop expected)
                (progn
                  (should (zerop status))
                  (should (string-search "old: CURRENT" (buffer-string))))
              (should-not (zerop status))
              (should (string-search "old: FAILED" (buffer-string)))
              (should (string-search "unsupported version v0.5.16" (buffer-string))))))))))

(provide 'test-mevedel-session-migration)
;;; test-mevedel-session-migration.el ends here
