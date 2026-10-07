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
                           ".." "scripts" "migrate-session-v0.5.6.el"))

(defmacro mevedel-migration-test--with-source (&rest body)
  "Run BODY with a two-head old session and a separate destination."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "mevedel-migration-" t))
          (source (file-name-concat root "old"))
          (destination (file-name-concat root "converted"))
          (session (test-mevedel-session-persistence--make-session root))
          (workspace (mevedel-session-workspace session))
          (old (cl-loop for (key value) on (mevedel-session-codec-serialize session) by #'cddr
                        unless (eq key :external-conversations) append (list key value)))
          (head ".publications/generation-bbbbbbbbbbbbbbbbbbbb/manifest.el")
          (lease-file (file-name-concat source ".lease/00000000000000000001.el")))
     (unwind-protect
         (progn
           (setq old (plist-put old :version "v0.5.6"))
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

(mevedel-deftest mevedel-migrate-session--sidecar ()
  (let* ((data (test-mevedel-session-persistence--complete-sidecar nil))
         (goal '(:id "g" :objective "Keep the goal" :status active :reason nil
                 :token-budget 100 :tokens-used 20 :time-used-seconds 1
                 :turns-run 1 :plan-reference nil :created-at "created" :updated-at "updated")))
    (cl-remf data :external-conversations)
    (setq data (plist-put (plist-put data :version "v0.5.6") :goal goal))
    (let ((restored (mevedel-session-codec--goal-from-plist
                     (plist-get (mevedel-migrate-session--sidecar data) :goal))))
      (should (= 20 (mevedel-goal-tokens-used restored)))
      (should-not (mevedel-goal-tokens-incomplete-p restored)))
    (should-not (plist-member goal :tokens-incomplete-p))))

(mevedel-deftest mevedel-migrate-session-copy (:quiet t)
  ,test
  (test)
  :doc "converts all retained heads, preserves bytes and loads through the current reader"
  (mevedel-migration-test--with-source
    (let ((before (mapcar (lambda (file) (cons file (mevedel-migrate-session--hash file)))
                          (directory-files-recursively source "."))))
      (should (= 3 (mevedel-migrate-session-copy source destination)))
      (dolist (row before)
        (should (equal (cdr row) (mevedel-migrate-session--hash (car row)))))
      (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
        (let* ((publication (mevedel-session-publication-read
                             destination (concat ".publications/generation-" name "/manifest.el")))
               (data (mevedel-session-codec-read (plist-get publication :sidecar)))
               (loaded (plist-get (mevedel-session-codec-deserialize data workspace) :session)))
          (should (equal "v0.5.9" (plist-get data :version)))
          (should (plist-member data :external-conversations))
          (should-not (mevedel-session-external-conversations loaded))
          (should (equal (mevedel-session-session-id session) (mevedel-session-session-id loaded)))
          (cl-loop for (key value) on old by #'cddr unless (eq key :version) do
                   (should (equal value (plist-get data key))))
          (let ((relative (concat ".publications/generation-" name "/000002.data")))
            (should (equal (mevedel-migrate-session--hash (file-name-concat source relative))
                           (mevedel-migrate-session--hash (file-name-concat destination relative)))))))
      ;; Runtime loading remains strict; only this explicit script accepts old data.
      (should-error (mevedel-session-codec-deserialize old workspace))))

  :doc "refuses corrupt, unsupported, live and unsafe sources without changing them"
  (dolist (fault '(version checksum path active unsettled missing-lease lock recovery symlink))
    (mevedel-migration-test--with-source
      (pcase fault
        ('version (mevedel-migrate-session--write
                   (file-name-concat source "session.meta.el") (plist-put old :version "v0.5.0")))
        ('checksum (with-temp-file (file-name-concat source ".publications/generation-aaaaaaaaaaaaaaaaaaaa/000002.data")
                     (insert "corrupt")))
        ('path (let* ((file (file-name-concat source head))
                      (manifest (mevedel-migrate-session--read file)))
                 (setf (plist-get (cdar (plist-get manifest :artifacts)) :published) "../../outside")
                 (mevedel-migrate-session--write file manifest)))
        ((or 'active 'unsettled)
         (let ((lease (mevedel-migrate-session--read lease-file)))
           (setq lease (if (eq fault 'active) (plist-put lease :status 'active)
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

  :doc "converts an unlocked file-workspace session without a publication lease"
  (mevedel-migration-test--with-source
    (delete-directory (file-name-concat source ".lease") t)
    (delete-directory (file-name-concat source ".publications") t)
    (setq old (plist-put old :authority-mode 'pid-lock))
    (setf (plist-get (plist-get old :workspace) :type) 'file)
    (mevedel-migrate-session--write (file-name-concat source "session.meta.el") old)
    (should (= 1 (mevedel-migrate-session-copy source destination)))
    (should (equal "v0.5.9"
                   (plist-get (mevedel-session-codec-read
                               (file-name-concat destination "session.meta.el")) :version))))

  :doc "never overwrites an existing destination or copies into itself"
  (mevedel-migration-test--with-source
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
  (mevedel-migration-test--with-source
    (let ((command-line-args-left (list "--" source destination)))
      (should (string-search "source unchanged" (with-output-to-string (mevedel-migrate-session-main))))
      (should-not command-line-args-left)
      (should (file-exists-p (file-name-concat destination "session.meta.el"))))))

(provide 'test-mevedel-session-migration)
;;; test-mevedel-session-migration.el ends here
