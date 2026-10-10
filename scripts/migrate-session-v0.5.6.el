;;; migrate-session-v0.5.6.el --- Convert v0.5.6-v0.5.10 sessions to v0.5.11 -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit v0.5.6/v0.5.9/v0.5.10 -> v0.5.11 conversion, outside the runtime loader.
;; The file keeps the name of its first source format.
;; Copy a closed local session to a new directory; never change the source.
;; Convert all retained publication sidecars and their manifest checksums.
;; Run from the repository root:
;; npx @emacs-eask/cli emacs --batch -L . -l scripts/migrate-session-v0.5.6.el \
;;   -f mevedel-migrate-session-main -- SOURCE DESTINATION

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

(defun mevedel-migrate-session--sidecar (data &optional attached)
  "Return DATA converted to v0.5.11, preserving existing durable fields.
ATTACHED lists the store artifact ids a legacy session starts attached to."
  (unless (equal mevedel-session-codec-format-version "v0.5.11")
    (error "This converter targets v0.5.11; use its matching mevedel checkout"))
  (setq data (copy-tree data))
  (pcase (plist-get data :version)
    ("v0.5.6"
     (when (plist-member data :external-conversations)
       (error "Unexpected external histories in a v0.5.6 sidecar"))
     (setq data (plist-put data :external-conversations nil)))
    ((or "v0.5.9" "v0.5.10" "v0.5.11") nil)
    (_ (error "Unsupported migration source version: %s" (plist-get data :version))))
  ;; v0.5.6 and early v0.5.9 Goals predate incomplete-usage tracking.  The
  ;; loader drops an invalid Goal silently, so validate it here instead.
  (when-let* ((goal (plist-get data :goal)))
    (unless (plist-member goal :tokens-incomplete-p)
      (setq data (plist-put data :goal (plist-put goal :tokens-incomplete-p nil))))
    (mevedel-session-codec--goal-from-plist (plist-get data :goal)))
  (unless (member (plist-get data :version) '("v0.5.10" "v0.5.11"))
    ;; Older formats never persisted input queues or recovery issues.  Do not
    ;; silently discard unexpected values or invent previously unsaved work.
    (cl-loop for (key value) on '(:recovery-issues nil
                                 :pending-follow-ups nil :pending-steering nil
                                 :pending-input-next-id 0 :pending-input-paused nil
                                 :pending-input-failure-paused nil) by #'cddr do
             (when (plist-member data key)
               (error "Unexpected recovery field %s in legacy sidecar" key))
             (setq data (plist-put data key value))))
  (unless (equal (plist-get data :version) "v0.5.11")
    ;; Older formats predate the artifact store: no session is attached yet.
    (when (plist-member data :attached-artifacts)
      (error "Unexpected attached artifacts in legacy sidecar"))
    (setq data (plist-put data :attached-artifacts attached))
    (setq data (plist-put data :version "v0.5.11")))
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

(defun mevedel-migrate-session-copy (source destination &optional attached)
  "Copy closed local SOURCE to new DESTINATION and convert its metadata.
Retain session identity, transcripts, artifacts and every publication head.
ATTACHED lists the store artifact ids a legacy session starts attached to.
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
          (when (zerop (hash-table-count sidecars)) (error "No session metadata found"))
          (let ((head (when-let* ((relative (mevedel-migrate-session--head lease source)))
                        (file-name-concat destination relative)))
                dropped)
            (maphash
             (lambda (file owners)
               (condition-case err
                   (let ((data (mevedel-migrate-session--sidecar
                                (mevedel-migrate-session--read file) attached)))
                     (when (and (eq 'portable (plist-get data :authority-mode)) (not lease))
                       (error "Portable session is missing its released lease"))
                     (mevedel-migrate-session--write file data))
                 (error
                  ;; An old retained generation whose sidecar no reader
                  ;; accepts, or a portable session's stale fixed cache, is
                  ;; dropped; the published head and a PID-lock sidecar must
                  ;; convert.
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

(defun mevedel-migrate-session-main ()
  "Convert SOURCE to DESTINATION supplied as command-line arguments."
  (let ((args (delete "--" command-line-args-left)))
    (setq command-line-args-left nil)
    (unless (= (length args) 2)
      (error "Usage: mevedel-migrate-session-main -- SOURCE DESTINATION"))
    (princ (format "Converted %d sidecar files into %s; source unchanged\n"
                   (mevedel-migrate-session-copy (car args) (cadr args)) (cadr args)))))

(provide 'mevedel-migrate-session)
;;; migrate-session-v0.5.6.el ends here
