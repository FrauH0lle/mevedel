;;; migrate-sessions.el --- Explicit stepwise session migration -*- lexical-binding: t -*-

;;; Commentary:
;; Point this script at a closed local session, project, or sessions directory.
;; It uses explicit adjacent format steps outside the runtime loader.  No older
;; formats are supported by the current chain.  Each changed
;; session is validated in a private staging copy before replacing its original;
;; the original stays in a hidden sibling backup directory.  Current sessions
;; are untouched.  Keep all selected sessions closed throughout migration.
;; Run from the repository root:
;; npx @emacs-eask/cli emacs --batch -L . -l scripts/migrate-sessions.el \
;;   -f mevedel-migrate-session-main -- SESSION-OR-PROJECT-DIRECTORY

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-session-codec)
(require 'mevedel-session-publication)

(defun mevedel-migrate-session--read (file)
  "Read one literal plist from FILE, refusing circular or trailing data."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((read-circle t)
           (value (read (current-buffer)))
           (pending (list (cons nil value)))
           (visited (make-hash-table :test #'eq))
           keys)
      ;; Native sidecars share objects using #N=/#N#.  Permit sharing, but
      ;; reject actual cycles before copying or validating nested metadata.
      (while pending
        (pcase-let* ((`(,done . ,object) (pop pending)))
          (when (or (consp object) (vectorp object))
            (cond
             (done (puthash object 'done visited))
             ((eq (gethash object visited) 'visiting) (error "Cyclic metadata in %s" file))
             ((not (gethash object visited))
              (puthash object 'visiting visited)
              (push (cons t object) pending)
              (if (consp object)
                  (progn (push (cons nil (cdr object)) pending)
                         (push (cons nil (car object)) pending))
                (mapc (lambda (item) (push (cons nil item) pending)) object)))))))
      (unless (and (proper-list-p value) (cl-evenp (length value)))
        (error "Invalid metadata plist: %s" file))
      (cl-loop for (key _value) on value by #'cddr do
               (unless (and (keywordp key) (not (memq key keys)))
                 (error "Invalid or duplicate metadata key in %s" file))
               (push key keys))
      (forward-comment (point-max))
      (unless (eobp) (error "Trailing metadata in %s" file))
      value)))

(defun mevedel-migrate-session--write (file value)
  "Write literal VALUE to FILE in the private destination copy."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-length nil) (print-level nil) (print-circle t) (print-quoted t))
    (with-temp-file file (prin1 value (current-buffer)) (insert "\n"))))

(defun mevedel-migrate-session--hash (file)
  "Return the SHA-256 of FILE's exact bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(define-error 'mevedel-migrate-session-unconvertible
  "Session has nothing this converter reads")

(defconst mevedel-migrate-session--steps nil
  "Ordered (SOURCE TARGET FUNCTION) format steps.
Each FUNCTION takes one sidecar plist.  Append a step when the format changes.")

(defun mevedel-migrate-session--sidecar (data)
  "Return DATA migrated step by step to the current runtime format.
Source DATA is never modified.  Unsupported versions fail rather than
guessing a path."
  (unless (or (null mevedel-migrate-session--steps)
              (equal (cadar (last mevedel-migrate-session--steps))
                     mevedel-session-codec-format-version))
    (error "Migration chain does not target the current session format"))
  (setq data (copy-tree data))
  (let* ((version (plist-get data :version))
         (steps (member (assoc version mevedel-migrate-session--steps)
                        mevedel-migrate-session--steps)))
    (unless (or steps (equal version mevedel-session-codec-format-version))
      (signal 'mevedel-migrate-session-unconvertible
              (list (format "unsupported version %s; minimum supported is %s"
                            version (or (caar mevedel-migrate-session--steps)
                                        mevedel-session-codec-format-version)))))
    (dolist (step steps)
      (unless (equal (plist-get data :version) (car step))
        (error "Disconnected migration chain at %s" (car step)))
      (setq data (funcall (nth 2 step) data))
      (setq data (plist-put data :version (cadr step)))))
  (when-let* ((goal (plist-get data :goal)))
    (mevedel-session-codec--goal-from-plist goal))
  (mevedel-session-codec-validate-current-sidecar data))

(defun mevedel-migrate-session--closed (directory)
  "Require closed DIRECTORY and return its exact lease snapshot.
A released lease is closed, and so is an active one whose holder stopped
renewing it: its expiry has passed, as after a crash.  Refuse recovery or
unsettled mutation rather than claiming to repair it."
  (when (file-exists-p (file-name-concat directory ".lock"))
    (error "Close the session and release its PID lock first"))
  (when-let* ((recovery (file-name-concat directory ".recovery"))
              ((file-directory-p recovery))
              ((directory-files recovery nil directory-files-no-dot-files-regexp)))
    (error "Resolve session recovery before migration"))
  (let* ((lease-dir (file-name-concat directory ".lease"))
         (files (and (file-directory-p lease-dir)
                     (directory-files lease-dir t "\\`[0-9]\\{20\\}\\.el\\'")))
         (records (mapcar #'mevedel-migrate-session--read (reverse files)))
         (latest (cl-find-if (lambda (row) (not (eq 'aborted (plist-get row :status)))) records)))
    (unless (cl-every #'mevedel-session-durability--valid-lease-p records)
      (error "Invalid portable lease record"))
    (when (and (file-directory-p lease-dir)
               (not (and latest
                         (not (plist-get latest :unsettled-mutation))
                         (or (eq 'released (plist-get latest :status))
                             ;; Local migration: the target clock is ours.
                             (and (eq 'active (plist-get latest :status))
                                  (< (plist-get latest :expires-at) (float-time)))))))
      (error "Close the session and release its portable lease first"))
    ;; No head is a session closed before it ever published.
    (when-let* ((head (plist-get latest :publication-head)))
      (unless (and (stringp head)
                   (mevedel-session-publication-valid-head-p head)
                   (file-regular-p (file-name-concat directory head)))
        (error "Missing or invalid published session head")))
    (mapcar (lambda (file) (cons (file-name-nondirectory file)
                               (mevedel-migrate-session--hash file))) files)))

(defun mevedel-migrate-session--head (lease source)
  "Return SOURCE's published head as a relative path, or nil.
LEASE is the snapshot `mevedel-migrate-session--closed' returned."
  (when lease
    (let ((latest (cl-find-if (lambda (row) (not (eq 'aborted (plist-get row :status))))
                              (mapcar (lambda (entry)
                                        (mevedel-migrate-session--read
                                         (file-name-concat source ".lease" (car entry))))
                                      (reverse lease)))))
      (plist-get latest :publication-head))))

(defun mevedel-migrate-session-copy (source destination)
  "Copy closed local SOURCE to new DESTINATION and convert its metadata.
Retain session identity, transcripts, artifacts and every publication head.
Return the count of distinct converted sidecar files.  Failure removes only
the new copy.  The caller must keep SOURCE closed throughout conversion."
  (setq source (directory-file-name (expand-file-name source))
        destination (directory-file-name (expand-file-name destination)))
  (when (or (file-remote-p source) (file-remote-p destination))
    (error "Migration requires local directories"))
  (unless (file-directory-p source) (error "Session directory does not exist"))
  (when (or (file-exists-p destination) (file-symlink-p destination)
            (file-in-directory-p destination source))
    (error "Destination must be new and outside the source session"))
  (let* ((files (directory-files-recursively source "." t))
         (lease (mevedel-migrate-session--closed source))
         (sidecars (make-hash-table :test #'equal))
         (hashes (make-hash-table :test #'equal))
         manifests success)
    (dolist (file (cons source files))
      (when (or (file-symlink-p file)
                (not (or (file-directory-p file) (file-regular-p file))))
        (error "Session contains a link or special file: %s" file)))
    (make-directory destination)
    (set-file-modes destination #o700)
    (unwind-protect
        (progn
          (copy-directory source (file-name-as-directory destination) t nil t)
          (set-file-modes destination #o700)
          (dolist (file files)
            (let ((relative (file-relative-name file source)))
              (when (string-match-p
                     "\\`\\.publications/generation-[[:xdigit:]]\\{20\\}/manifest\\.el\\'" relative)
                (let* ((path (file-name-concat destination relative))
                       (manifest (mevedel-session-publication--validate-manifest
                                  (mevedel-migrate-session--read path) path))
                       (entries (plist-get manifest :artifacts)))
                  ;; Collection may leave an older manifest naming files of a
                  ;; generation it removed.  Nothing can read that manifest;
                  ;; drop it from the copy, keeping its data files, which later
                  ;; manifests may still name.  The published head must be whole.
                  (unless (cl-every (lambda (entry)
                                      (file-regular-p
                                       (file-name-concat destination
                                                         (plist-get (cdr entry) :published))))
                                    entries)
                    (when (equal relative (mevedel-migrate-session--head lease source))
                      (error "The published session head is incomplete"))
                    (delete-file path)
                    (setq entries nil manifest nil))
                  (dolist (entry entries)
                    (let* ((published (plist-get (cdr entry) :published))
                           (expected (plist-get (cdr entry) :sha256)))
                      (let* ((artifact (file-name-concat destination published))
                             (digest (or (gethash artifact hashes)
                                         (puthash artifact (mevedel-migrate-session--hash artifact) hashes))))
                        (unless (equal expected digest)
                          (error "Artifact checksum mismatch: %s" published))
                        (when (equal (car entry) "session.meta.el")
                          (puthash artifact (cons path (gethash artifact sidecars))
                                   sidecars)))))
                  (when manifest (push (cons path manifest) manifests))))))
          (let ((sidecar (file-name-concat destination "session.meta.el")))
            (when (file-regular-p sidecar) (puthash sidecar 'fixed sidecars)))
          (when (zerop (hash-table-count sidecars))
            (signal 'mevedel-migrate-session-unconvertible (list "no session metadata")))
          (let ((head (when-let* ((relative (mevedel-migrate-session--head lease source)))
                        (file-name-concat destination relative)))
                dropped)
            (maphash
             (lambda (file owners)
               (condition-case err
                   (let ((data (mevedel-migrate-session--sidecar
                                (mevedel-migrate-session--read file))))
                     (when (and (eq 'portable (plist-get data :authority-mode)) (not lease))
                       (error "Portable session is missing its released lease"))
                     (mevedel-migrate-session--write file data))
                 (error
                  ;; An old retained generation whose sidecar no reader
                  ;; accepts, or a portable session's stale fixed cache, is
                  ;; dropped; authoritative sidecars must validate.
                  (if (or (and (eq owners 'fixed) (not lease))
                          (and (listp owners) (member head owners)))
                      (signal (car err) (cdr err))
                    (if (eq owners 'fixed)
                        (delete-file file)
                      (dolist (manifest owners)
                        (when (file-exists-p manifest) (delete-file manifest))
                        (setq manifests (cl-remove manifest manifests
                                                   :key #'car :test #'equal))))
                    (push file dropped)))))
             sidecars)
            (dolist (file dropped) (remhash file sidecars)))
          (clrhash hashes)
          (dolist (row manifests)
            (dolist (entry (plist-get (cdr row) :artifacts))
              (let ((file (file-name-concat destination (plist-get (cdr entry) :published))))
                (when (gethash file sidecars)
                  (setf (plist-get (cdr entry) :sha256)
                        (or (gethash file hashes)
                            (puthash file (mevedel-migrate-session--hash file) hashes))))))
            (mevedel-migrate-session--write (car row) (cdr row)))
          (unless (equal lease (mevedel-migrate-session--closed source))
            (error "Source lease changed during migration; keep it closed and retry"))
          (setq success t)
          (hash-table-count sidecars))
      (unless success (delete-directory destination t)))))

(defun mevedel-migrate-session--snapshot (directory)
  "Return a sorted content and metadata fingerprint of safe DIRECTORY.
Refuse links and special files before reading or copying their contents."
  (let (snapshot)
    (dolist (file (cons directory (directory-files-recursively directory "." t)))
      (when (or (file-symlink-p file)
                (not (or (file-directory-p file) (file-regular-p file))))
        (error "Session contains a link or special file: %s" file))
      (let ((attributes (file-attributes file 'integer)))
        (push (list (file-relative-name file directory)
                    (file-attribute-type attributes)
                    (file-attribute-modes attributes)
                    (file-attribute-modification-time attributes)
                    (unless (file-directory-p file)
                      (mevedel-migrate-session--hash file)))
              snapshot)))
    (sort snapshot (lambda (a b) (string< (car a) (car b))))))

(defun mevedel-migrate-session--directories (directory)
  "Resolve local DIRECTORY to a list of session directories.
Accept one session, a project root, or its `.mevedel/sessions' container."
  (when (file-remote-p directory)
    (error "Migration requires local directories"))
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-directory-p directory)
    (error "Migration directory does not exist: %s" directory))
  (let ((path directory))
    (while path
      (when (file-symlink-p path)
        (error "Migration directory contains a symbolic link: %s" path))
      (setq path (unless (equal path "/")
                   (directory-file-name (file-name-directory path))))))
  (cond
   ((or (file-exists-p (file-name-concat directory "session.meta.el"))
        (file-directory-p (file-name-concat directory ".lease"))
        (file-directory-p (file-name-concat directory ".publications")))
    (list directory))
   ((file-directory-p (file-name-concat directory ".mevedel"))
    (when (file-symlink-p (file-name-concat directory ".mevedel"))
      (error "Project state directory is a symbolic link"))
    (let ((sessions (file-name-concat directory ".mevedel" "sessions")))
      (when (file-symlink-p sessions)
        (error "Sessions directory is a symbolic link: %s" sessions))
      (when (file-directory-p sessions)
        (cl-remove-if-not #'file-directory-p
                          (directory-files sessions t "\\`[^.]")))))
   ((and (equal (file-name-nondirectory directory) "sessions")
         (equal (file-name-nondirectory
                 (directory-file-name (file-name-directory directory))) ".mevedel"))
    (cl-remove-if-not #'file-directory-p
                      (directory-files directory t "\\`[^.]")))
   (t (error "Expected a session directory or project directory: %s" directory))))

(defun mevedel-migrate-session--authoritative-data (directory lease)
  "Read DIRECTORY's authoritative sidecar using captured LEASE.
Only a portable session released without ever publishing returns nil."
  (let ((sidecar
         (if (file-directory-p (file-name-concat directory ".lease"))
             (when-let* ((head (mevedel-migrate-session--head lease directory)))
               (or (plist-get (mevedel-session-publication-read directory head) :sidecar)
                   (error "Missing published session metadata")))
           (file-name-concat directory "session.meta.el"))))
    (when sidecar
      (or (mevedel-migrate-session--read sidecar)
          (error "Empty authoritative session metadata: %s" sidecar)))))

(defun mevedel-migrate-session--install (source staged)
  "Replace SOURCE with validated STAGED and return the original's backup path.
An ordinary failed install restores SOURCE.  Never remove an original backup,
even when rollback itself fails."
  (let* ((parent (file-name-directory source))
         (name (file-name-nondirectory source))
         (container (make-temp-file
                     (file-name-concat parent (concat ".migration-backup-" name "-")) t))
         (backup (file-name-concat container "session"))
         (inhibit-quit t))
    (unwind-protect
        (progn
          (rename-file source backup)
          (condition-case err
              (rename-file staged source)
            (error
             (condition-case rollback
                 (rename-file backup source)
               (error
                (error "Install failed (%s); rollback failed (%s); original is at %s"
                       (error-message-string err) (error-message-string rollback) backup)))
             (signal (car err) (cdr err))))
          backup)
      (unless (file-exists-p backup)
        (delete-directory container)))))

(defun mevedel-migrate-session--one (source)
  "Migrate one closed SOURCE and return its status plist.
The caller owns serialization and reports ordinary failures per session."
  (let* ((before (mevedel-migrate-session--snapshot source))
         (lease (mevedel-migrate-session--closed source))
         (data (mevedel-migrate-session--authoritative-data source lease)))
    (if (null data)
        (list :status 'empty)
      (mevedel-migrate-session--sidecar data)
      (let* ((container (make-temp-file
                         (file-name-concat (file-name-directory source) ".migration-stage-") t))
             (staged (file-name-concat container "session")))
        (unwind-protect
            (progn
              (mevedel-migrate-session-copy source staged)
              (unless (and (equal before (mevedel-migrate-session--snapshot source))
                           (equal lease (mevedel-migrate-session--closed source)))
                (error "Source changed during migration; keep it closed and retry"))
              (if (equal (plist-get data :version) mevedel-session-codec-format-version)
                  (list :status 'current)
                (list :status 'migrated
                      :backup (mevedel-migrate-session--install source staged))))
          (when (file-directory-p container)
            (delete-directory container t)))))))

(defun mevedel-migrate-sessions (directory)
  "Migrate supported sessions under local DIRECTORY to the current format.
DIRECTORY is a session, project, or `.mevedel/sessions' directory.  Return
rows (NAME :status STATUS :backup PATH :reason TEXT), where
STATUS is migrated, current, empty, or failed.  Failed sessions stay in place;
independent sessions continue.  Unsafe ownership aborts before any writes."
  (let ((directories (mevedel-migrate-session--directories directory)))
    (when directories
      ;; Refuse unsafe ownership in any selected session before writing.
      (dolist (source directories)
        (when (file-symlink-p source)
          (error "Session directory is a symbolic link: %s" source))
        (mevedel-migrate-session--closed source))
      (let ((lock (file-name-concat (file-name-directory (car directories)) ".migration-lock"))
            report)
        (condition-case nil
            (make-directory lock)
          (file-already-exists
           (error "Migration lock already exists: %s; check for a running converter before removing it"
                  lock)))
        (unwind-protect
            (progn
              (dolist (source directories)
                (let ((name (file-name-nondirectory source)))
                  (push (cons name
                              (condition-case err
                                  (mevedel-migrate-session--one source)
                                (error (list :status 'failed :reason (error-message-string err)))))
                        report)))
              (nreverse report))
          (delete-directory lock))))))

(defun mevedel-migrate-session-main ()
  "Migrate one DIRECTORY supplied through the batch command line."
  (let ((args command-line-args-left)
        (usage "Usage: mevedel-migrate-session-main -- DIRECTORY\nDIRECTORY is a closed session, project, or .mevedel/sessions directory.\n"))
    (setq command-line-args-left nil)
    (when (equal (car args) "--") (setq args (cdr args)))
    (if (equal args '("--help"))
        (princ usage)
      (unless (= (length args) 1) (error "%s" (string-trim-right usage)))
      (let* ((report (mevedel-migrate-sessions (car args)))
             (failed (cl-count-if (lambda (row) (eq (plist-get (cdr row) :status) 'failed))
                                  report)))
        (dolist (row report)
          (let* ((result (cdr row))
                 (status (plist-get result :status)))
            (princ (format "%s: %s%s\n" (car row) (upcase (symbol-name status))
                           (pcase status
                             ('migrated (format " (backup: %s)" (plist-get result :backup)))
                             ('failed (format " (%s)" (plist-get result :reason)))
                             (_ ""))))))
        (princ (format "%d sessions checked; %d failed; target %s.\n"
                       (length report) failed mevedel-session-codec-format-version))
        (when (> failed 0)
          (error "%d sessions failed migration checks; see above" failed))))))

(provide 'mevedel-migrate-session)
;;; migrate-sessions.el ends here
