;;; migrate-sessions.el --- Explicit stepwise session migration -*- lexical-binding: t -*-

;;; Commentary:
;; Point this script at a closed local session, project, or sessions directory.
;; It migrates supported versions through explicit adjacent format steps,
;; including legacy artifacts, outside the runtime loader.  Each changed
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
(require 'mevedel-artifact-store)
(require 'mevedel-session-artifacts)
(require 'mevedel-shared-editing)

(defvar mevedel-migrate-session--changed nil
  "Non-nil when a staged conversion changes a sidecar or retained manifest.")

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

(defun mevedel-migrate-session--plan-retry (data)
  "Update DATA's unprepared Worktree retry to the fork-first handoff.
Refuse older target-oriented state whose effects cannot be recovered from
one sidecar.  DATA is the private copy owned by the converter."
  (let* ((metadata (plist-get data :plan-metadata))
         (retry (plist-get metadata :implementation-retry)))
    (when (eq (plist-get (plist-get retry :selection) :location) 'worktree)
      ;; Legacy targets were clean sessions, not forks.  Reusing their path
      ;; as a fork or submitting in the source would implement in the wrong
      ;; conversation; resetting them could duplicate already prepared work.
      (when (or (cl-some (lambda (key) (plist-member retry key))
                        '(:target-directory :target-session-id :target-accepted :summary))
                (and (plist-member retry :target-save-path)
                     (not (plist-get retry :worktree-reservation)))
                (not (memq (plist-get retry :step)
                           '(prepare-summary prepare-worktree)))
                (and (eq (plist-get retry :step) 'prepare-summary)
                     (or (not (eq (plist-get (plist-get retry :selection) :context) 'summary))
                         (plist-get retry :worktree-reservation)
                         (plist-get retry :fork-point-id))))
        (error "Legacy Worktree plan retry requires manual recovery; source and target must stay unchanged"))
      (when (eq (plist-get retry :step) 'prepare-summary)
        (setq retry (plist-put retry :step 'prepare-worktree))
        (setq metadata (plist-put metadata :implementation-retry retry))
        (setq data (plist-put data :plan-metadata metadata)))))
  data)

(defun mevedel-migrate-session--v0.5.6-to-v0.5.9 (data _attached)
  "Add native history references to v0.5.6 DATA."
  (when (plist-member data :external-conversations)
    (error "Unexpected external histories in a v0.5.6 sidecar"))
  (plist-put data :external-conversations nil))

(defun mevedel-migrate-session--v0.5.9-to-v0.5.10 (data _attached)
  "Add durable input queues and complete-usage tracking to v0.5.9 DATA."
  (when-let* ((goal (plist-get data :goal)))
    (unless (plist-member goal :tokens-incomplete-p)
      (setq data (plist-put data :goal (plist-put goal :tokens-incomplete-p nil))))
    ;; The runtime loader drops invalid Goals; explicit migration must refuse.
    (mevedel-session-codec--goal-from-plist (plist-get data :goal)))
  (cl-loop for (key value) on '(:recovery-issues nil
                               :pending-follow-ups nil :pending-steering nil
                               :pending-input-next-id 0 :pending-input-paused nil
                               :pending-input-failure-paused nil) by #'cddr do
           (when (plist-member data key)
             (error "Unexpected recovery field %s in legacy sidecar" key))
           (setq data (plist-put data key value)))
  data)

(defun mevedel-migrate-session--v0.5.10-to-v0.5.11 (data attached)
  "Add artifact store references to v0.5.10 DATA using ATTACHED ids."
  (when (or (plist-member data :attached-artifacts)
            (plist-member data :dedicated-artifact))
    (error "Unexpected artifact store fields in legacy sidecar"))
  (setq data (plist-put data :attached-artifacts attached))
  (plist-put data :dedicated-artifact nil))

(defun mevedel-migrate-session--v0.5.11-to-v0.5.12 (data _attached)
  "Add the unrecorded naming input field to v0.5.11 DATA."
  (when (plist-member data :naming-input)
    (error "Unexpected naming input in legacy sidecar"))
  (plist-put data :naming-input nil))

(defun mevedel-migrate-session--v0.5.12-to-v0.5.13 (data _attached)
  "Add the unrecorded chat boundaries field to v0.5.12 DATA."
  (when (plist-member data :chapters)
    (error "Unexpected chat records in legacy sidecar"))
  (plist-put data :chapters nil))

(defun mevedel-migrate-session--v0.5.13-to-v0.5.14 (data _attached)
  "Add the absent per-agent working directory override to v0.5.13 DATA."
  (let ((registry (plist-get data :agent-registry)))
    (unless (proper-list-p registry)
      (error "Invalid legacy agent registry"))
    (plist-put
     data :agent-registry
     (mapcar
      (lambda (entry)
        (unless (and (proper-list-p entry) (cl-evenp (length entry)))
          (error "Invalid legacy agent record"))
        (when (plist-member entry :working-directory)
          (error "Unexpected agent working directory in legacy sidecar"))
        (plist-put entry :working-directory nil))
      registry))))

(defconst mevedel-migrate-session--steps
  '(("v0.5.6" "v0.5.9" mevedel-migrate-session--v0.5.6-to-v0.5.9)
    ("v0.5.9" "v0.5.10" mevedel-migrate-session--v0.5.9-to-v0.5.10)
    ("v0.5.10" "v0.5.11" mevedel-migrate-session--v0.5.10-to-v0.5.11)
    ("v0.5.11" "v0.5.12" mevedel-migrate-session--v0.5.11-to-v0.5.12)
    ("v0.5.12" "v0.5.13" mevedel-migrate-session--v0.5.12-to-v0.5.13)
    ("v0.5.13" "v0.5.14" mevedel-migrate-session--v0.5.13-to-v0.5.14))
  "Ordered (SOURCE TARGET FUNCTION) format steps.
Retire old formats by deleting a prefix and its unused step functions.
Append a step whenever the runtime codec's current format changes.")

(defun mevedel-migrate-session--sidecar (data &optional attached)
  "Return DATA migrated step by step to the current runtime format.
ATTACHED lists store artifact ids for a pre-store session.  Source DATA is
never modified.  Unsupported versions fail rather than guessing a path."
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
    (when (and attached (or (null steps)
                            (not (assoc "v0.5.10" steps))))
      (error "A %s session already uses the artifact store; move its legacy artifacts by hand"
             version))
    (dolist (step steps)
      (unless (equal (plist-get data :version) (car step))
        (error "Disconnected migration chain at %s" (car step)))
      (setq data (funcall (nth 2 step) data attached))
      (setq data (plist-put data :version (cadr step)))))
  ;; The fork-first Worktree handoff changed within v0.5.13.  Its shape,
  ;; rather than the top-level format alone, determines whether repair is needed.
  (setq data (mevedel-migrate-session--plan-retry data))
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
                    (setq mevedel-migrate-session--changed t)
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
                   (let* ((original (mevedel-migrate-session--read file))
                          (data (mevedel-migrate-session--sidecar original attached)))
                     (when (and (eq 'portable (plist-get data :authority-mode)) (not lease))
                       (error "Portable session is missing its released lease"))
                     (unless (equal original data)
                       (setq mevedel-migrate-session--changed t))
                     (mevedel-migrate-session--write file data))
                 (error
                  ;; An old retained generation whose sidecar no reader
                  ;; accepts, or a portable session's stale fixed cache, is
                  ;; dropped; the published head and a PID-lock sidecar must
                  ;; convert.
                  (if (or (and (eq owners 'fixed) (not lease))
                          (and (listp owners) (member head owners)))
                      (signal (car err) (cdr err))
                    (setq mevedel-migrate-session--changed t)
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

(defconst mevedel-migrate-artifacts--comments-prefix
  "artifacts/shared-editing/artifact-comments/"
  "Logical prefix of a legacy session's artifact comment stores.")

(defconst mevedel-migrate-artifacts--items-prefix "artifacts/shared-editing/"
  "Logical prefix of a legacy session's whiteboards and documents.")

(defun mevedel-migrate-artifacts--entries (directory &optional publication)
  "Return closed session DIRECTORY's legacy artifact entries.
Each is (LOGICAL . BYTES) below `artifacts/'.  Portable sessions read their
verified PUBLICATION, never their fixed cache.  PID-lock sessions ignore
hidden files and backups."
  (if (file-directory-p (file-name-concat directory ".lease"))
      (let ((publication (or publication (mevedel-session-publication-read directory))))
        (cl-loop for (logical . _) in (plist-get publication :artifacts)
                 when (string-prefix-p "artifacts/" logical)
                 collect (cons logical
                               (mevedel-session-artifacts--read-published-artifact
                                publication logical))))
    (let ((folder (file-name-concat directory "artifacts")))
      (when (file-directory-p folder)
        (cl-loop for file in (directory-files-recursively folder "." nil)
                 for name = (file-name-nondirectory file)
                 unless (or (file-symlink-p file)
                            (cl-some (lambda (part) (string-prefix-p "." part))
                                     (split-string (file-relative-name file folder) "/"))
                            (string-suffix-p "~" name)
                            (string-prefix-p "#" name))
                 collect (cons (concat "artifacts/" (file-relative-name file folder))
                               (mevedel-migrate-artifacts--bytes file)))))))

(defun mevedel-migrate-artifacts--bytes (file)
  "Return FILE's exact bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun mevedel-migrate-artifacts--json (bytes)
  "Parse legacy UTF-8 JSON BYTES."
  (mevedel-shared-editing--parse (decode-coding-string bytes 'utf-8-unix)))

(defun mevedel-migrate-artifacts--existing (workspace origin)
  "Return the store id an earlier run moved ORIGIN to, or nil."
  (cl-find-if (lambda (id)
                (member origin (plist-get (mevedel-artifact-store-meta workspace id)
                                         :migration-origins)))
              (mevedel-artifact-store-ids workspace)))

(defun mevedel-migrate-artifacts--fresh-id (workspace name)
  "Return an unused store id derived from file NAME."
  (let* ((base (let ((stem (replace-regexp-in-string
                            "[^A-Za-z0-9_-]+" "-" (file-name-sans-extension name))))
                 (truncate-string-to-width
                  (if (string-match-p "\\`[A-Za-z0-9]" stem) stem (concat "artifact" stem))
                  72)))
         (id base)
         (n 1))
    (while (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
               (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace id)))
      (setq id (format "%s-%d" base (cl-incf n))))
    id))

(defun mevedel-migrate-artifacts--write (path bytes)
  "Write literal BYTES to PATH, creating its directory."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil path nil 'silent)))

(defun mevedel-migrate-artifacts--import (workspace id origin writer)
  "Import new artifact ID from ORIGIN using WRITER in WORKSPACE.
Only a complete import is reusable on retry; failed writes remove their
new directory, leaving previous imports untouched."
  (let ((directory (mevedel-artifact-store-artifact-directory workspace id))
        (bookkeeping (mevedel-artifact-store-bookkeeping-directory workspace id))
        complete)
    (when (or (file-exists-p directory) (file-exists-p bookkeeping))
      (error "Artifact %s already exists" id))
    (unwind-protect
        (progn
          (funcall writer)
          (mevedel-artifact-store-update-meta workspace id :migration-origins (list origin))
          (setq complete t)
          id)
      (unless complete
        (when (file-directory-p directory) (delete-directory directory t))
        (when (file-directory-p bookkeeping) (delete-directory bookkeeping t))))))

(defun mevedel-migrate-artifacts--move-file (workspace session-id logical bytes)
  "Move legacy artifact file LOGICAL with BYTES into WORKSPACE's store.
SESSION-ID names the session it came from.  Return the store id."
  (let* ((origin (cons session-id logical))
         (file (file-name-nondirectory logical)))
    (or (when-let* ((id (mevedel-migrate-artifacts--existing workspace origin)))
          (let* ((meta (mevedel-artifact-store-meta workspace id))
                 (path (mevedel-artifact-store-primary-path workspace id meta)))
            (unless (and (not (mevedel-artifact-store-item-p meta))
                         (equal file (plist-get meta :file))
                         (file-regular-p path)
                         (equal bytes (mevedel-migrate-artifacts--bytes path)))
              (error "Previously imported artifact %s for %s/%s is missing or differs from the legacy source; resolve it before retrying"
                     id session-id logical)))
          id)
        (when-let* ((id (cl-find-if
                         (lambda (id)
                           (let ((meta (mevedel-artifact-store-meta workspace id)))
                             (and (plist-get meta :migration-origins)
                                  (equal file (plist-get meta :file))
                                  (equal bytes (mevedel-migrate-artifacts--bytes
                                                (mevedel-artifact-store-primary-path
                                                 workspace id meta))))))
                         (mevedel-artifact-store-ids workspace))))
          (mevedel-artifact-store-update-meta
           workspace id :migration-origins
           (cons origin (plist-get (mevedel-artifact-store-meta workspace id)
                                   :migration-origins)))
          id)
        (let ((id (mevedel-migrate-artifacts--fresh-id workspace file)))
          (mevedel-migrate-artifacts--import
           workspace id origin
           (lambda ()
             (mevedel-migrate-artifacts--write
              (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) file)
              bytes)
             (mevedel-artifact-store-create-meta workspace id file)
             (mevedel-artifact-store-record-version workspace id session-id)))))))

(defun mevedel-migrate-artifacts--move-item (workspace session-id logical state)
  "Move legacy whiteboard or document STATE from LOGICAL into WORKSPACE's store.
Its id stays the item's own, unless a fork's copy diverged from the one
already moved.  SESSION-ID records its origin.  Return the store id."
  (let ((id (plist-get state :id))
        (origin (cons session-id logical)))
    (mevedel-shared-editing--valid-id id)
    (or (when-let* ((existing (mevedel-migrate-artifacts--existing workspace origin)))
          ;; A divergent fork may have been imported under a fresh id.  That
          ;; identity rewrite is the only difference a retry may accept.
          (let ((expected (plist-put (copy-sequence state) :id existing)))
            (unless (and (mevedel-shared-editing-present-p workspace existing)
                         (equal (mevedel-shared-editing--json expected)
                                (mevedel-shared-editing--json
                                 (mevedel-shared-editing--read workspace existing))))
              (error "Previously imported artifact %s for %s/%s is missing or differs from the legacy source; resolve it before retrying"
                     existing session-id logical)))
          existing)
        (and (mevedel-shared-editing-present-p workspace id)
             (equal (mevedel-shared-editing--json state)
                    (mevedel-shared-editing--json
                     (mevedel-shared-editing--read workspace id)))
             (progn
               (mevedel-artifact-store-update-meta
                workspace id :migration-origins
                (cons origin (plist-get (mevedel-artifact-store-meta workspace id)
                                        :migration-origins)))
               id))
        (progn
          (when (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
                    (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace id)))
            (let ((n 1) (base id))
              (while (or (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
                         (file-exists-p (mevedel-artifact-store-bookkeeping-directory workspace id)))
                (setq id (format "%s-%d" base (cl-incf n))))
              (setq state (plist-put (copy-sequence state) :id id))))
          (mevedel-migrate-artifacts--import
           workspace id origin
           (lambda ()
             (mevedel-migrate-artifacts--write
              (file-name-concat (mevedel-artifact-store-bookkeeping-directory workspace id)
                                "state.json")
              (encode-coding-string (mevedel-shared-editing--json state) 'utf-8-unix))
             (mevedel-artifact-store-create-meta
              workspace id "state.json" (intern (plist-get state :kind)) (plist-get state :title))
             (mevedel-artifact-store-update-meta workspace id :revision (plist-get state :revision))
             (mevedel-artifact-store-record-version
              workspace id session-id (mevedel-shared-editing--json
                                       (mevedel-shared-editing--version-state state)))))))))

(defun mevedel-migrate-artifacts--import-entries (workspace session-id entries)
  "Import captured artifact ENTRIES for SESSION-ID into WORKSPACE.
Return their store ids.  Bytes come from the validated source snapshot,
not a second live read of a session that may have changed meanwhile."
  ;; This reserved namespace held direct JSON item states.  Unrecognized
  ;; content must not disappear when the converted copy drops legacy paths.
  ;; Comment stores are the one intentional exclusion from migration.
  (dolist (entry entries)
    (let ((logical (car entry)))
      (when (and (string-prefix-p mevedel-migrate-artifacts--items-prefix logical)
                 (not (string-prefix-p mevedel-migrate-artifacts--comments-prefix logical))
                 (or (not (string-suffix-p ".json" logical))
                     (string-search "/" (substring logical (length mevedel-migrate-artifacts--items-prefix)))))
        (error "Unrecognized legacy shared-editing artifact: %s; resolve it before migrating"
               logical))))
  (let (ids)
    (pcase-dolist (`(,logical . ,bytes) entries)
      (cond
       ((string-prefix-p mevedel-migrate-artifacts--comments-prefix logical))
       ((string-prefix-p mevedel-migrate-artifacts--items-prefix logical)
        (push (mevedel-migrate-artifacts--move-item
               workspace session-id logical (mevedel-migrate-artifacts--json bytes))
              ids))
       (t
        (push (mevedel-migrate-artifacts--move-file workspace session-id logical bytes)
              ids))))
    (delete-dups (nreverse ids))))

(defun mevedel-migrate-artifacts--drop-legacy (target)
  "Drop converted session TARGET's `artifacts/' folder and manifest entries."
  (let ((folder (file-name-concat target "artifacts")))
    (when (file-directory-p folder) (delete-directory folder t)))
  (let ((publications (file-name-concat target ".publications")))
    (when (file-directory-p publications)
      (dolist (manifest (directory-files-recursively publications "\\`manifest\\.el\\'"))
        (let* ((data (mevedel-migrate-session--read manifest))
               (kept (cl-remove-if (lambda (entry) (string-prefix-p "artifacts/" (car entry)))
                                   (plist-get data :artifacts))))
          (unless (equal kept (plist-get data :artifacts))
            (mevedel-migrate-session--write manifest (plist-put data :artifacts kept))))))))

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

(defun mevedel-migrate-session--workspace (directory)
  "Return the project workspace containing session DIRECTORY, or nil."
  (let* ((sessions (directory-file-name (file-name-directory directory)))
         (state (directory-file-name (file-name-directory sessions))))
    (when (and (equal (file-name-nondirectory sessions) "sessions")
               (equal (file-name-nondirectory state) ".mevedel"))
      (mevedel-workspace--create
       :type 'project :id "migration" :name "migration"
       :root (file-name-directory state)))))

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
      ;; Reject unsupported or malformed authoritative metadata before store writes.
      (mevedel-migrate-session--sidecar data)
      (let* ((entries (mevedel-migrate-artifacts--entries source))
             (workspace (mevedel-migrate-session--workspace source))
             (pre-store (assoc "v0.5.10"
                               (member (assoc (plist-get data :version)
                                              mevedel-migrate-session--steps)
                                       mevedel-migrate-session--steps)))
             (container (make-temp-file
                         (file-name-concat (file-name-directory source) ".migration-stage-") t))
             (staged (file-name-concat container "session"))
             (mevedel-migrate-session--changed nil)
             ids)
        (unwind-protect
            (progn
              (when entries
                (unless pre-store
                  (error "Session already uses the artifact store but still has legacy artifacts"))
                (unless workspace
                  (error "Legacy artifacts require a session under PROJECT/.mevedel/sessions"))
                ;; The store is shared with sessions outside a single-session
                ;; selection.  They must be closed too before artifact imports.
                (dolist (other (directory-files (file-name-directory source) t "\\`[^.]"))
                  (when (file-directory-p other)
                    (when (file-symlink-p other)
                      (error "Session directory is a symbolic link: %s" other))
                    (mevedel-migrate-session--closed other))))
              ;; A placeholder validates every convertible sidecar's attachment
              ;; transition before importing any artifact into the workspace.
              (mevedel-migrate-session-copy source staged
                                            (when entries '("migration-validation")))
              (when entries
                (unless (and (equal before (mevedel-migrate-session--snapshot source))
                             (equal lease (mevedel-migrate-session--closed source)))
                  (error "Source changed during migration; keep it closed and retry"))
                (setq ids (mevedel-migrate-artifacts--import-entries
                           workspace (file-name-nondirectory source) entries))
                (delete-directory staged t)
                (mevedel-migrate-session-copy source staged ids))
              (when pre-store
                (mevedel-migrate-artifacts--drop-legacy staged))
              (unless (and (equal before (mevedel-migrate-session--snapshot source))
                           (equal lease (mevedel-migrate-session--closed source)))
                (error "Source changed during migration; keep it closed and retry"))
              (if mevedel-migrate-session--changed
                  (list :status 'migrated :artifacts ids
                        :backup (mevedel-migrate-session--install source staged))
                (list :status 'current)))
          (when (file-directory-p container)
            (delete-directory container t)))))))

(defun mevedel-migrate-sessions (directory)
  "Migrate supported sessions under local DIRECTORY to the current format.
DIRECTORY is a session, project, or `.mevedel/sessions' directory.  Return
rows (NAME :status STATUS :backup PATH :artifacts IDS :reason TEXT), where
STATUS is migrated, current, empty, or failed.  Failed sessions stay in place;
independent sessions continue.  Unsafe ownership aborts before any writes."
  (let ((directories (mevedel-migrate-session--directories directory)))
    (when directories
      ;; Closed-state preflight is global: never write a project's store while
      ;; another selected session might still be using it.
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
             (failed (cl-count 'failed report :key (lambda (row) (plist-get (cdr row) :status)))))
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
          (error "%d sessions failed to migrate; see above" failed))))))

(provide 'mevedel-migrate-session)
;;; migrate-sessions.el ends here
