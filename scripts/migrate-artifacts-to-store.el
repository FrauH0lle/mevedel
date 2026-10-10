;;; migrate-artifacts-to-store.el --- Move session artifacts into the store -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit, one-time move of a workspace's per-session artifacts into its
;; artifact store, outside the runtime loader.  For every closed session of
;; the workspace it copies the session's artifact files, whiteboards,
;; documents and artifact comments into `.mevedel/artifacts/', then converts
;; the session into DESTINATION with `scripts/migrate-session-v0.5.6.el',
;; attached to the artifacts it held and without its old `artifacts/'
;; entries.  Comment threads keep answering in the session that discussed
;; them.  The original sessions are never changed: check DESTINATION, then
;; replace the sessions directory with it.  A rerun reuses artifacts an
;; earlier run already moved, and a fork's identical copy of a file reuses
;; its parent's.
;; Run from the repository root, with every session of the workspace closed:
;; npx @emacs-eask/cli emacs --batch -L . -l scripts/migrate-artifacts-to-store.el \
;;   -f mevedel-migrate-artifacts-main -- WORKSPACE-ROOT DESTINATION

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mevedel-artifact-store)
(require 'mevedel-session-artifacts)
(require 'mevedel-session-publication)
(require 'mevedel-shared-editing)
(require 'mevedel-collaboration-artifact-comments)
(load (expand-file-name "migrate-session-v0.5.6.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst mevedel-migrate-artifacts--comments-prefix
  "artifacts/shared-editing/artifact-comments/"
  "Logical prefix of a legacy session's artifact comment stores.")

(defconst mevedel-migrate-artifacts--items-prefix "artifacts/shared-editing/"
  "Logical prefix of a legacy session's whiteboards and documents.")

(defun mevedel-migrate-artifacts--entries (directory &optional publication)
  "Return closed session DIRECTORY's legacy artifact entries.
Each is (LOGICAL . BYTES) for a logical path below `artifacts/'.  A
portable session is read from its verified publication, never its fixed
cache; a PID-lock session from its folder, without links, hidden files and
backups, which would carry other bytes into the store.
PUBLICATION is the already captured portable publication, when available."
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

(defun mevedel-migrate-artifacts--move-file (workspace session-id logical bytes comments name)
  "Move legacy artifact file LOGICAL with BYTES into WORKSPACE's store.
COMMENTS are its legacy comment threads, which keep answering in session
SESSION-ID called NAME.  Return the store id."
  (let* ((origin (cons session-id logical))
         (file (file-name-nondirectory logical))
         (tagged (mapcar (lambda (comment)
                           (plist-put (plist-put (copy-sequence comment) :session session-id)
                                      :session-name name))
                         comments)))
    (or (mevedel-migrate-artifacts--existing workspace origin)
        ;; A fork copied its parent's files: the same bytes are the same
        ;; artifact, as for whiteboards and documents, and threads the fork
        ;; added join the parent's.
        (when-let* ((id (cl-find-if
                         (lambda (id)
                           (let ((meta (mevedel-artifact-store-meta workspace id)))
                             (and (plist-get meta :migration-origins)
                                  (equal file (plist-get meta :file))
                                  (equal bytes (mevedel-migrate-artifacts--bytes
                                                (mevedel-artifact-store-primary-path
                                                 workspace id meta))))))
                         (mevedel-artifact-store-ids workspace))))
          (let* ((stored (mevedel-collaboration--artifact-comments-read workspace id))
                 (added (cl-remove-if (lambda (comment)
                                        (cl-find (plist-get comment :id) stored
                                                 :key (lambda (c) (plist-get c :id))
                                                 :test #'equal))
                                      tagged)))
            (when added
              (mevedel-collaboration--artifact-comments-write
               workspace id (append stored added))))
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
             (when tagged
               (mevedel-collaboration--artifact-comments-write workspace id tagged))
             (mevedel-artifact-store-record-version workspace id session-id)))))))

(defun mevedel-migrate-artifacts--move-item (workspace session-id logical state)
  "Move legacy whiteboard or document STATE from LOGICAL into WORKSPACE's store.
Its id stays the item's own, unless a fork's copy of the item diverged from
the one already moved.  Return the store id."
  (let ((id (plist-get state :id))
        (origin (cons session-id logical)))
    (mevedel-shared-editing--valid-id id)
    (or (mevedel-migrate-artifacts--existing workspace origin)
        ;; A fork copied its parent's items under the same id: the same
        ;; state is the same item, a diverged one becomes its own.
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

(defun mevedel-migrate-artifacts-session (workspace directory)
  "Move closed session DIRECTORY's artifacts into WORKSPACE's store.
Return the store ids the session holds."
  (let* ((session-id (file-name-nondirectory (directory-file-name directory)))
         (portable (file-directory-p (file-name-concat directory ".lease")))
         (publication (and portable (mevedel-session-publication-read directory)))
         (sidecar (if portable (plist-get publication :sidecar)
                    (file-name-concat directory "session.meta.el")))
         (name (and sidecar (file-regular-p sidecar)
                    (plist-get (mevedel-migrate-session--read sidecar) :session-name)))
         (entries (mevedel-migrate-artifacts--entries directory publication))
         (comments (make-hash-table :test #'equal))
         ids)
    ;; Comment stores name their artifact; collect them first.
    (pcase-dolist (`(,logical . ,bytes) entries)
      (when (string-prefix-p mevedel-migrate-artifacts--comments-prefix logical)
        (let ((store (mevedel-migrate-artifacts--json bytes)))
          (puthash (plist-get store :artifact)
                   (append (plist-get store :comments) nil) comments))))
    (pcase-dolist (`(,logical . ,bytes) entries)
      (cond
       ((string-prefix-p mevedel-migrate-artifacts--comments-prefix logical))
       ((string-prefix-p mevedel-migrate-artifacts--items-prefix logical)
        (when (and (string-suffix-p ".json" logical)
                   (not (string-search "/" (substring logical (length mevedel-migrate-artifacts--items-prefix)))))
          (push (mevedel-migrate-artifacts--move-item
                 workspace session-id logical (mevedel-migrate-artifacts--json bytes))
                ids)))
       (t
        (let ((relative (substring logical (length "artifacts/"))))
          (push (mevedel-migrate-artifacts--move-file
                 workspace session-id logical bytes (gethash relative comments) name)
                ids)))))
    (nreverse ids)))

(defun mevedel-migrate-artifacts--drop-legacy (target)
  "Drop converted session TARGET's `artifacts/' folder and manifest entries.
The store now holds their content, and nothing reads them."
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

(defun mevedel-migrate-artifacts (root destination)
  "Move the artifacts of workspace ROOT's sessions into its store.
Each session is converted into new DESTINATION, attached to its moved
artifacts.  One the converter has nothing to read in -- older than it
reads, or never saved -- is copied there unchanged, and so is one that
failed, so replacing the sessions directory loses nothing.  A rerun into a
new DESTINATION reuses what was already moved.  Return a list of
\(SESSION-ID . IDS), IDS being (:unconverted REASON) or (:failed REASON)
for those sessions."
  (setq root (file-name-as-directory (expand-file-name root))
        destination (directory-file-name (expand-file-name destination)))
  (when (or (file-remote-p root) (file-remote-p destination))
    (error "Migration requires local directories"))
  (when (or (file-exists-p destination) (file-symlink-p destination))
    (error "Destination must be new"))
  (let* ((workspace (mevedel-workspace--create :type 'project :id "migration"
                                               :root root :name "migration"))
         (sessions-dir (file-name-concat root ".mevedel" "sessions"))
         (directories (and (file-directory-p sessions-dir)
                           (cl-remove-if-not #'file-directory-p
                                             (directory-files sessions-dir t "\\`[^.]"))))
         report)
    (when (file-in-directory-p destination sessions-dir)
      (error "Destination must be outside the source sessions directory"))
    ;; Every session must be closed before anything is written.
    (dolist (directory directories)
      (condition-case err
          (mevedel-migrate-session--closed directory)
        (error (error "%s: %s" (file-name-nondirectory directory)
                      (error-message-string err)))))
    (make-directory destination t)
    (set-file-modes destination #o700)
    (dolist (directory directories)
      (let* ((session-id (file-name-nondirectory directory))
             (target (file-name-concat destination session-id))
             ids)
        (condition-case err
            (progn
              (setq ids (mevedel-migrate-artifacts-session workspace directory))
              (mevedel-migrate-session-copy directory target ids)
              (mevedel-migrate-artifacts--drop-legacy target))
          ;; A session no current reader can open stays as it was, so
          ;; replacing the sessions directory loses nothing.
          (mevedel-migrate-session-unconvertible
           (copy-directory directory target t t t)
           (setq ids (list :unconverted (error-message-string err))))
          ;; Anything else is a failure the report names.
          (error
           (when (file-exists-p target) (delete-directory target t))
           (copy-directory directory target t t t)
           (setq ids (list :failed (error-message-string err)))))
        (push (cons session-id ids) report)))
    (nreverse report)))

(defun mevedel-migrate-artifacts-main ()
  "Run `mevedel-migrate-artifacts' on command-line WORKSPACE-ROOT and DESTINATION."
  (let ((args (delete "--" command-line-args-left)))
    (setq command-line-args-left nil)
    (unless (= (length args) 2)
      (error "Usage: mevedel-migrate-artifacts-main -- WORKSPACE-ROOT DESTINATION"))
    (let* ((report (mevedel-migrate-artifacts (car args) (cadr args)))
           (failed (cl-count :failed report :key #'cadr)))
      (pcase-dolist (`(,session . ,ids) report)
        (princ (format "%s: %s\n" session
                       (pcase (car ids)
                         (:unconverted (format "kept unconverted (%s)" (cadr ids)))
                         (:failed (format "FAILED, copied unchanged (%s)" (cadr ids)))
                         ('nil "no artifacts")
                         (_ (string-join ids ", "))))))
      (princ (format "Converted %d sessions into %s; the originals are unchanged.\n"
                     (- (length report) failed) (cadr args)))
      (if (> failed 0)
          (error "%d sessions failed to convert; see above" failed)
        (princ "Check them, then replace .mevedel/sessions with that directory.\n")))))

(provide 'mevedel-migrate-artifacts)
;;; migrate-artifacts-to-store.el ends here
