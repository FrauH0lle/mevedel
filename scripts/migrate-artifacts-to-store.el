;;; migrate-artifacts-to-store.el --- Move session artifacts into the store -*- lexical-binding: t -*-

;;; Commentary:
;; Explicit, one-time move of a workspace's per-session artifacts into its
;; artifact store, outside the runtime loader.  For every closed session of
;; the workspace it copies the session's artifact files, whiteboards,
;; documents and artifact comments into `.mevedel/artifacts/', then converts
;; the session into DESTINATION with `scripts/migrate-session-v0.5.6.el',
;; attached to the artifacts it held.  Comment threads keep answering in
;; the session that discussed them.  The original sessions are never
;; changed: check DESTINATION, then replace the sessions directory with it.
;; A rerun reuses artifacts an earlier run already moved.
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
(load (expand-file-name "migrate-session-v0.5.6.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst mevedel-migrate-artifacts--comments-prefix
  "artifacts/shared-editing/artifact-comments/"
  "Logical prefix of a legacy session's artifact comment stores.")

(defconst mevedel-migrate-artifacts--items-prefix "artifacts/shared-editing/"
  "Logical prefix of a legacy session's whiteboards and documents.")

(defun mevedel-migrate-artifacts--entries (directory)
  "Return closed session DIRECTORY's legacy artifact entries.
Each is (LOGICAL . BYTES) for a logical path below `artifacts/'.  A
portable session is read from its verified publication, never its fixed
cache; a PID-lock session from its folder."
  (if (file-directory-p (file-name-concat directory ".lease"))
      (let ((publication (mevedel-session-publication-read directory)))
        (cl-loop for (logical . _) in (plist-get publication :artifacts)
                 when (string-prefix-p "artifacts/" logical)
                 collect (cons logical
                               (mevedel-session-artifacts--read-published-artifact
                                publication logical))))
    (let ((folder (file-name-concat directory "artifacts")))
      (when (file-directory-p folder)
        (cl-loop for file in (directory-files-recursively folder "." nil)
                 collect (cons (concat "artifacts/" (file-relative-name file folder))
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally file)
                                 (buffer-string))))))))

(defun mevedel-migrate-artifacts--json (bytes)
  "Parse legacy UTF-8 JSON BYTES."
  (mevedel-shared-editing--parse (decode-coding-string bytes 'utf-8-unix)))

(defun mevedel-migrate-artifacts--existing (workspace origin)
  "Return the store id an earlier run moved ORIGIN to, or nil."
  (cl-find-if (lambda (id)
                (equal origin (plist-get (mevedel-artifact-store-meta workspace id)
                                         :migrated-from)))
              (mevedel-artifact-store-ids workspace)))

(defun mevedel-migrate-artifacts--fresh-id (workspace name)
  "Return an unused store id derived from file NAME."
  (let* ((base (let ((stem (replace-regexp-in-string
                            "[^A-Za-z0-9_-]+" "-" (file-name-sans-extension name))))
                 (if (string-match-p "\\`[A-Za-z0-9]" stem) stem (concat "artifact" stem))))
         (id base)
         (n 1))
    (while (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
      (setq id (format "%s-%d" base (cl-incf n))))
    id))

(defun mevedel-migrate-artifacts--write (path bytes)
  "Write literal BYTES to PATH, creating its directory."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'no-conversion))
    (write-region bytes nil path nil 'silent)))

(defun mevedel-migrate-artifacts--move-file (workspace session-id logical bytes comments name)
  "Move legacy artifact file LOGICAL with BYTES into WORKSPACE's store.
COMMENTS are its legacy comment threads, which keep answering in session
SESSION-ID called NAME.  Return the store id."
  (let ((origin (cons session-id logical)))
    (or (mevedel-migrate-artifacts--existing workspace origin)
        (let* ((file (file-name-nondirectory logical))
               (id (mevedel-migrate-artifacts--fresh-id workspace file)))
          (mevedel-migrate-artifacts--write
           (file-name-concat (mevedel-artifact-store-artifact-directory workspace id) file)
           bytes)
          (mevedel-artifact-store-create-meta workspace id file)
          (mevedel-artifact-store-update-meta workspace id :migrated-from origin)
          (when comments
            (mevedel--write-file-atomically
             (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                               "comments.json")
             (mevedel-shared-editing--json
              (list :artifact id
                    :comments (vconcat
                               (mapcar (lambda (comment)
                                         (plist-put (plist-put (copy-sequence comment)
                                                               :session session-id)
                                                    :session-name name))
                                       comments))))))
          (mevedel-artifact-store-record-version workspace id session-id)
          id))))

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
        (and (mevedel-shared-editing--present-p workspace id)
             (equal (mevedel-shared-editing--json state)
                    (mevedel-shared-editing--json
                     (mevedel-shared-editing--read workspace id)))
             id)
        (progn
          (when (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
            (let ((n 1) (base id))
              (while (file-exists-p (mevedel-artifact-store-artifact-directory workspace id))
                (setq id (format "%s-%d" base (cl-incf n))))
              (setq state (plist-put (copy-sequence state) :id id))))
          (mevedel-migrate-artifacts--write
           (file-name-concat (mevedel-artifact-store-artifact-directory workspace id)
                             "state.json")
           (encode-coding-string (mevedel-shared-editing--json state) 'utf-8-unix))
          (mevedel-artifact-store-create-meta
           workspace id "state.json" (intern (plist-get state :kind)) (plist-get state :title))
          (mevedel-artifact-store-update-meta workspace id :migrated-from origin)
          (mevedel-artifact-store-record-version
           workspace id session-id (mevedel-shared-editing--version-content state))
          id))))

(defun mevedel-migrate-artifacts-session (workspace directory)
  "Move closed session DIRECTORY's artifacts into WORKSPACE's store.
Return the store ids the session holds."
  (let* ((session-id (file-name-nondirectory (directory-file-name directory)))
         (sidecar (file-name-concat directory "session.meta.el"))
         (name (and (file-regular-p sidecar)
                    (plist-get (mevedel-migrate-session--read sidecar) :session-name)))
         (entries (mevedel-migrate-artifacts--entries directory))
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

(defun mevedel-migrate-artifacts (root destination)
  "Move the artifacts of workspace ROOT's sessions into its store.
Each session is converted into new DESTINATION, attached to its moved
artifacts; one the converter refuses is copied there unchanged.  Return a
list of (SESSION-ID . IDS), IDS being (:unconverted REASON) for those."
  (setq root (file-name-as-directory (expand-file-name root))
        destination (directory-file-name (expand-file-name destination)))
  (when (file-remote-p root) (error "Migration requires a local workspace"))
  (when (file-exists-p destination) (error "Destination must be new"))
  (let* ((workspace (mevedel-workspace--create :type 'project :id "migration"
                                               :root root :name "migration"))
         (sessions-dir (file-name-concat root ".mevedel" "sessions"))
         (directories (and (file-directory-p sessions-dir)
                           (cl-remove-if-not #'file-directory-p
                                             (directory-files sessions-dir t "\\`[^.]"))))
         report)
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
             (ids (mevedel-migrate-artifacts-session workspace directory))
             (target (file-name-concat destination session-id)))
        (condition-case err
            (mevedel-migrate-session-copy directory target ids)
          ;; A session no current reader can open stays as it was, so
          ;; replacing the sessions directory loses nothing.
          (error
           (copy-directory directory target t t t)
           (setq ids (list :unconverted (error-message-string err)))))
        (push (cons session-id ids) report)))
    (nreverse report)))

(defun mevedel-migrate-artifacts-main ()
  "Run `mevedel-migrate-artifacts' on command-line WORKSPACE-ROOT and DESTINATION."
  (let ((args (delete "--" command-line-args-left)))
    (setq command-line-args-left nil)
    (unless (= (length args) 2)
      (error "Usage: mevedel-migrate-artifacts-main -- WORKSPACE-ROOT DESTINATION"))
    (let ((report (mevedel-migrate-artifacts (car args) (cadr args))))
      (pcase-dolist (`(,session . ,ids) report)
        (princ (format "%s: %s\n" session
                       (cond ((eq (car ids) :unconverted)
                              (format "kept unconverted (%s)" (cadr ids)))
                             (ids (string-join ids ", "))
                             (t "no artifacts")))))
      (princ (format "Converted %d sessions into %s; the originals are unchanged.\n"
                     (length report) (cadr args)))
      (princ "Check them, then replace .mevedel/sessions with that directory.\n"))))

(provide 'mevedel-migrate-artifacts)
;;; migrate-artifacts-to-store.el ends here
