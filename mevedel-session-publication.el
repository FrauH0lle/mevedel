;;; mevedel-session-publication.el -- Immutable session publication -*- lexical-binding: t -*-

;;; Commentary:

;; Owns immutable publication manifests, critical artifact transactions, retryable recovery, and diagnostic publication.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'mevedel-structs))

;; Every control operation in this file runs through the session control
;; filesystem, so its feature is a hard load-time dependency rather than a
;; lazily reachable one.
(require 'mevedel-session-control-fs)
;; Publication validates and reads through durability's shared primitives.
;; Durability reaches back into publication only from inside functions, so
;; this direction is the loadable one.
(require 'mevedel-session-durability)

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-session-control-fs'
(declare-function mevedel-session-control-fs-create-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-control-fs" (path &optional parents))
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-read-file
                  "mevedel-session-control-fs"
                  (path &optional coding-system))
(declare-function mevedel-session-control-fs-write-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))

;; `mevedel-session-durability'
(declare-function mevedel-session-durability--assert-no-pid-lock
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability--create-plist
                  "mevedel-session-durability" (path plist))
(declare-function mevedel-session-durability--lease-head
                  "mevedel-session-durability" (directory))
(declare-function mevedel-session-durability--lease-path
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-durability--read-plist
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-durability--renew-publication-lease
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability--valid-relative-path-p
                  "mevedel-session-durability" (path))
(declare-function mevedel-session-durability-call-with-reserved-lease
                  "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-commit-publication-head
                  "mevedel-session-durability" (session head))
(declare-function mevedel-session-durability-lease-owned-p
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-lease-renew
                  "mevedel-session-durability" (session))
(declare-function mevedel-session-durability-publication-head
                  "mevedel-session-durability" (session-dir))
(declare-function mevedel-session-recovery-refresh
                  "mevedel-session-recovery" (session))

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery--abandon
                  "mevedel-session-recovery" (session))
(declare-function mevedel-session-recovery--delete-local
                  "mevedel-session-recovery" (path))
(declare-function mevedel-session-recovery-refresh-session-buffers
                  "mevedel-session-recovery" (session))

;; `mevedel-structs'
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-uncommitted-batches
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir
                  "mevedel-structs" (workspace))

(defun mevedel-session-publication--control-logical-path-p (path)
  "Return non-nil when logical PATH names durability control storage."
  (or (equal path ".lock")
      (equal path ".lease")
      (string-prefix-p ".lease/" path)
      (equal path ".publications")
      (string-prefix-p ".publications/" path)
      (equal path ".recovery")
      (string-prefix-p ".recovery/" path)))

(defun mevedel-session-publication-logical-path-p (path)
  "Return non-nil when PATH is a normalized session artifact path."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (not (mevedel-session-publication--control-logical-path-p path))))

(defun mevedel-session-publication-valid-head-p (head)
  "Return non-nil when HEAD names an immutable publication manifest."
  (and (mevedel-session-durability--valid-relative-path-p head)
       (string-prefix-p ".publications/" head)
       (equal (file-name-nondirectory head) "manifest.el")))


;;;; Publication recovery state

(defun mevedel-session-publication--delete-batch (batch)
  "Delete BATCH's local recovery directory."
  (when-let* ((directory (plist-get batch :directory))
              ((file-directory-p directory))
              ((file-in-directory-p directory temporary-file-directory)))
    (condition-case err
        (delete-directory directory t)
      (file-error
       (display-warning
        'mevedel
        (format "Could not remove publication recovery: %s"
                (error-message-string err))
        :warning)))))

(defun mevedel-session-publication--delete-batches (batches)
  "Delete local recovery directories owned by BATCHES."
  (dolist (batch (delete-dups (copy-sequence batches)))
    (mevedel-session-publication--delete-batch batch)))

(defun mevedel-session-publication-clear-transient (session)
  "Delete and clear SESSION's locally staged publication state."
  (let ((pending (mevedel-session-pending-publication session)))
    (mevedel-session-publication--delete-batches
     (append
      (plist-get pending :batches)
      (mevedel-session-publication-uncommitted-batches session)
      (mevedel-session-publication-queue session)))
    (setf (mevedel-session-pending-publication session) nil
          (mevedel-session-publication-uncommitted-batches session) nil
          (mevedel-session-publication-queue session) nil)))

(defun mevedel-session-publication--batch-live-p (batch)
  "Return non-nil when BATCH still owns a staged source."
  (cl-some
   (lambda (artifact)
     (file-exists-p (plist-get artifact :source)))
   (plist-get batch :artifacts)))

(defun mevedel-session-publication--retain-uncommitted-batch (session batch)
  "Retain BATCH's session-local artifacts on SESSION until a marker."
  (let ((local
         (cl-remove-if-not
          (lambda (artifact) (plist-get artifact :logical))
          (plist-get batch :artifacts))))
    (if local
        (let ((retained (copy-sequence batch)))
          (setf (plist-get retained :artifacts) local)
          (setf (mevedel-session-publication-uncommitted-batches session)
                (append
                 (mevedel-session-publication-uncommitted-batches session)
                 (list retained)))
          (dolist (artifact (plist-get batch :artifacts))
            (unless (plist-get artifact :logical)
              (condition-case nil
                  (delete-file (plist-get artifact :source))
                (file-error nil)))))
      (mevedel-session-publication--delete-batch batch))))

(defun mevedel-session-publication--record-pending (session batches err)
  "Retain BATCHES as SESSION recovery after publication error ERR."
  (setf (mevedel-session-pending-publication session)
        (list :batches batches
              :failed-at (format-time-string "%FT%T%z")
              :reason (error-message-string err)))
  (display-warning
   'mevedel
   (format
    (concat "Critical session publication failed: %s. "
            "The completed work is retained locally; retry or explicitly "
            "abandon recovery before starting another turn.")
    (error-message-string err))
   :error))


(defun mevedel-session-publication--current-session ()
  "Return the session reached from the current data or view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))
      (user-error "No active mevedel session")))

(defun mevedel-session-publication--valid-published-path-p (path)
  "Return non-nil when PATH names immutable publication storage."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (string-prefix-p ".publications/" path)))

(defun mevedel-session-publication--publication-path (session-dir relative)
  "Return RELATIVE below SESSION-DIR's physical publication root."
  (mevedel-session-control-fs-physical-path session-dir)
  (let* ((session-root
          (file-name-as-directory
           (mevedel-session-control-fs-physical-path session-dir)))
         (root
          (file-name-as-directory
           (mevedel-session-control-fs-physical-path
            (file-name-concat session-dir ".publications"))))
         (path (file-name-concat session-dir relative)))
    (mevedel-session-control-fs-physical-path root)
    (mevedel-session-control-fs-physical-path path)
    (unless (and (not (equal (directory-file-name root)
                             (directory-file-name session-root)))
                 (file-in-directory-p root session-root))
      (error "Session publication root escapes session storage: %s" root))
    (unless (file-in-directory-p path root)
      (error "Session publication path escapes immutable storage: %s" path))
    path))

(defun mevedel-session-publication--artifact-for-session (session artifact)
  "Return normalized publication ARTIFACT for SESSION.

Session-local artifacts receive a target-native `:logical' path.  Artifacts
elsewhere on the same execution target receive nil."
  (let* ((session-dir
          (file-name-as-directory
           (expand-file-name
            (or (mevedel-session-save-path session)
                (error "Session has no durability path")))))
         (raw (plist-get artifact :path))
         (path (and (stringp raw) (expand-file-name raw session-dir)))
         (session-remote (file-remote-p session-dir))
         (path-remote (and path (file-remote-p path))))
    (unless path
      (error "Publication artifact requires a string path and content"))
    (unless (equal session-remote path-remote)
      (error "Publication artifact crosses execution targets: %s" raw))
    (let* ((local-p (string-prefix-p session-dir path))
           (logical (and local-p (substring path (length session-dir))))
           (marker (plist-get artifact :commit-marker))
           (replace (plist-get artifact :replace)))
      (when (and logical
                 (not (mevedel-session-publication-logical-path-p
                       logical)))
        (error "Invalid session publication path: %s" logical))
      (when (and marker (not (eq marker t)))
        (error "Publication commit marker must be t"))
      (when (and replace (not (eq replace t)))
        (error "Publication replacement marker must be t"))
      (when (and replace (not marker))
        (error "Publication replacement requires the commit marker"))
      (when (and marker (not (equal logical "session.meta.el")))
        (error "Publication commit marker must be session.meta.el"))
      (let ((normalized (copy-sequence artifact)))
        (setq normalized (plist-put normalized :path path)
              normalized (plist-put normalized :logical logical))
        normalized))))

(defun mevedel-session-publication--file-sha256 (path)
  "Return the SHA-256 digest of file PATH's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun mevedel-session-publication--validate-manifest (manifest path)
  "Return MANIFEST when it has the publication schema, else error at PATH."
  (let ((sidecar (plist-get manifest :sidecar))
        (artifacts (plist-get manifest :artifacts))
        (seen (make-hash-table :test #'equal))
        valid)
    (setq valid
          (and (proper-list-p manifest)
               (equal sidecar "session.meta.el")
               (plist-member manifest :artifacts)
               (proper-list-p artifacts)))
    (dolist (entry artifacts)
      (let ((logical (car-safe entry))
            (properties (cdr-safe entry)))
        (unless (and valid
                     (proper-list-p entry)
                     (mevedel-session-publication-logical-path-p
                      logical)
                     (proper-list-p properties)
                     (mevedel-session-publication--valid-published-path-p
                      (plist-get properties :published))
                     (stringp (plist-get properties :sha256))
                     (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                                     (plist-get properties :sha256))
                     (not (gethash logical seen)))
          (setq valid nil))
        (when logical
          (puthash logical t seen))))
    (unless (and valid (gethash sidecar seen))
      (error "Invalid session publication manifest: %s" path))
    manifest))

(defun mevedel-session-publication--read-publication-raw
    (session-dir &optional head)
  "Read and validate SESSION-DIR's raw manifest at HEAD or its lease head."
  (let* ((remote-file-name-inhibit-cache t)
         (head (or head
                   (mevedel-session-durability-publication-head session-dir))))
    (when head
      (unless (mevedel-session-publication-valid-head-p head)
        (error "Invalid session publication head: %s" head))
      (let* ((path (mevedel-session-publication--publication-path
                    session-dir head))
             (manifest (mevedel-session-durability--read-plist path)))
        (unless manifest
          (error "Missing session publication manifest: %s" path))
        (setq manifest
              (mevedel-session-publication--validate-manifest manifest path))
        (list :head head
              :sidecar (plist-get manifest :sidecar)
              :artifacts (plist-get manifest :artifacts))))))

(defun mevedel-session-publication--capture-publication
    (session-dir publication)
  "Qualify raw PUBLICATION paths beneath SESSION-DIR for a live client."
  (when publication
    (let* ((artifacts
            (mapcar
             (lambda (entry)
               (let ((copy (copy-tree entry)))
                 (setf (plist-get (cdr copy) :published)
                       (mevedel-session-publication--publication-path
                        session-dir (plist-get (cdr entry) :published)))
                 copy))
             (plist-get publication :artifacts)))
           (sidecar (cdr (assoc "session.meta.el" artifacts))))
      (list :head (plist-get publication :head)
            :sidecar (plist-get sidecar :published)
            :artifacts artifacts))))

(defun mevedel-session-publication-read (session-dir)
  "Return SESSION-DIR's captured immutable publication, or nil.

Manifest structure and control paths are validated uncached.  Only the
sidecar bytes are verified eagerly; consumers verify other artifact hashes
when they read those artifacts."
  (let* ((remote-file-name-inhibit-cache t)
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (publication
          (mevedel-session-publication--read-publication-raw session-dir))
         (captured
          (mevedel-session-publication--capture-publication
           session-dir publication)))
    (when captured
      (let* ((sidecar (plist-get captured :sidecar))
             (entry (cdr (assoc "session.meta.el"
                                (plist-get captured :artifacts))))
             (expected (plist-get entry :sha256)))
        (unless (and (file-regular-p sidecar)
                     (equal expected
                            (mevedel-session-publication--file-sha256 sidecar)))
          (error "Invalid session publication sidecar: %s" sidecar))))
    captured))

(defun mevedel-session-publication--stage-artifacts (session artifacts)
  "Copy SESSION publication ARTIFACTS into one local recovery batch.

Each input artifact is a plist containing `:path', `:content', and optional
`:coding'.  Other metadata is preserved.  Return a batch whose artifacts name
local `:source' files."
  (let ((directory (make-temp-file "mevedel-publication-" t))
        staged
        (marker-count 0))
    (condition-case err
        (progn
          (cl-loop
           for artifact in artifacts
           for index from 1
           for normalized =
           (mevedel-session-publication--artifact-for-session session artifact)
           for target = (plist-get normalized :path)
           for content = (plist-get normalized :content)
           for source = (file-name-concat directory (format "%06d" index))
           do
           (unless (and (stringp target) (stringp content))
             (error "Publication artifact requires string path and content"))
           (when (plist-get normalized :commit-marker)
             (cl-incf marker-count))
           (with-temp-buffer
             (set-buffer-multibyte (multibyte-string-p content))
             (insert content)
             (let ((coding-system-for-write
                    (if (multibyte-string-p content)
                        (or (plist-get normalized :coding) 'utf-8-unix)
                      'no-conversion)))
               (write-region (point-min) (point-max) source nil 'silent)))
           (setq normalized
                 (cl-loop for (key value) on normalized by #'cddr
                          unless (eq key :content)
                          append (list key value))
                 normalized (plist-put normalized :source source))
           (push normalized staged))
          (when (> marker-count 1)
            (error "Publication requires exactly one commit marker"))
          (list :directory directory :artifacts (nreverse staged)))
      (error
       (when (file-directory-p directory)
         (delete-directory directory t))
       (signal (car err) (cdr err))))))

(defun mevedel-session-publication--batch-marker (batch)
  "Return BATCH's commit-marker artifact, or nil."
  (cl-find-if
   (lambda (artifact) (plist-get artifact :commit-marker))
   (plist-get batch :artifacts)))

(defun mevedel-session-publication--deduplicate-artifacts (artifacts)
  "Return session-local ARTIFACTS in last-write-wins order."
  (let (result)
    (dolist (artifact artifacts)
      (when-let ((logical (plist-get artifact :logical)))
        (setq result
              (append
               (cl-remove logical result
                          :key (lambda (item) (plist-get item :logical))
                          :test #'equal)
               (list artifact)))))
    result))

(defun mevedel-session-publication--raw-artifacts (publication session-dir)
  "Return captured PUBLICATION artifacts relative to SESSION-DIR."
  (mapcar
   (lambda (entry)
     (let ((copy (copy-tree entry)))
       (setf (plist-get (cdr copy) :published)
             (file-relative-name
              (plist-get (cdr entry) :published) session-dir))
       copy))
   (plist-get publication :artifacts)))

(defun mevedel-session-publication--overlay-entry (entries entry)
  "Overlay publication ENTRY on ENTRIES with last-write-wins ordering."
  (append
   (cl-remove (car entry) entries :key #'car :test #'equal)
   (list entry)))

(defun mevedel-session-publication--new-generation-directory (root)
  "Create and return one fresh immutable generation directory below ROOT."
  (catch 'directory
    (dotimes (_ 8)
      (let ((path
             (file-name-concat
              root
              (format "generation-%s"
                      (substring
                       (secure-hash
                        'sha256
                        (format "%S" (list (current-time)
                                            (random most-positive-fixnum))))
                       0 20)))))
        (when (mevedel-session-control-fs-make-directory path)
          (throw 'directory (file-name-as-directory path)))))
    (error "Could not create a fresh session publication directory: %s"
           root)))

(defun mevedel-session-publication--write-immutable-artifact
    (directory artifact index session-dir)
  "Write ARTIFACT number INDEX below DIRECTORY and return its manifest entry."
  (let* ((source (plist-get artifact :source))
         (target (file-name-concat directory (format "%06d.data" index))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally source)
      (unless (mevedel-session-control-fs-create-file
               target (buffer-string) 'no-conversion)
        (error "Could not create immutable publication artifact: %s" target)))
    (list (plist-get artifact :logical)
          :published (file-relative-name target session-dir)
          :sha256 (mevedel-session-publication--file-sha256 source))))

(defun mevedel-session-publication--commit-marker-publication
    (session batches marker)
  "Commit retained and current BATCHES for SESSION at sidecar MARKER."
  (let* ((session-dir
          (file-name-as-directory (mevedel-session-save-path session)))
         (previous
          (or (mevedel-session-publication session)
              (mevedel-session-publication-read session-dir)))
         (entries
          (unless (plist-get marker :replace)
            (mevedel-session-publication--raw-artifacts
             previous session-dir)))
         (artifacts
          (mevedel-session-publication--deduplicate-artifacts
           (apply #'append
                  (mapcar
                   (lambda (batch) (plist-get batch :artifacts))
                   batches))))
         (root (file-name-concat session-dir ".publications"))
         directory)
    (mevedel-session-control-fs-physical-path root)
    (mevedel-session-control-fs-make-directory root t)
    (mevedel-session-control-fs-physical-path root)
    ;; Immutable generations grow until session-directory cleanup.
    (setq directory
          (mevedel-session-publication--new-generation-directory root))
    (cl-loop
     for artifact in artifacts
     for index from 1
     do
     (setq entries
           (mevedel-session-publication--overlay-entry
            entries
            (mevedel-session-publication--write-immutable-artifact
             directory artifact index session-dir))))
    (let* ((manifest (list :sidecar "session.meta.el" :artifacts entries))
           (manifest-path (file-name-concat directory "manifest.el"))
           (head (file-relative-name manifest-path session-dir))
           (raw (list :head head
                      :sidecar "session.meta.el"
                      :artifacts entries))
           (captured
            (mevedel-session-publication--capture-publication
             session-dir raw)))
      (mevedel-session-publication--validate-manifest manifest manifest-path)
      (unless (mevedel-session-durability--create-plist manifest-path manifest)
        (error "Could not create session publication manifest: %s"
               manifest-path))
      (unless (mevedel-session-durability-commit-publication-head session head)
        (user-error "Portable session lease was lost before publication commit"))
      (setf (mevedel-session-publication session) captured)
      captured)))

(defun mevedel-session-publication-uncommitted-artifact (session logical)
  "Return SESSION's newest retained or queued local source for LOGICAL.

Return nil when no uncommitted write exists.  This operation never consults
the target's fixed cache or mutates the publication accumulator."
  (unless (mevedel-session-publication-logical-path-p logical)
    (error "Invalid session publication path: %s" logical))
  (catch 'source
    (dolist
        (batch
         (reverse
          (append
           (mevedel-session-publication-uncommitted-batches session)
           (mevedel-session-publication-queue session))))
      (dolist (artifact (reverse (plist-get batch :artifacts)))
        (when (equal logical (plist-get artifact :logical))
          (let ((source (plist-get artifact :source)))
            (when (and source (file-exists-p source))
              (throw 'source source))))))))

(defun mevedel-session-publication--publish-artifact (artifact)
  "Publish staged ARTIFACT through a nearby target temporary file."
  (let* ((path (plist-get artifact :path))
         (source (plist-get artifact :source))
         (directory (file-name-directory (expand-file-name path))))
    (mevedel-session-control-fs-physical-path directory)
    (mevedel-session-control-fs-physical-path path)
    (mevedel-session-control-fs-make-directory directory t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally source)
      (mevedel-session-control-fs-write-file
       (mevedel-session-control-fs-physical-path path)
       (buffer-string) 'no-conversion))))

(defun mevedel-session-publication--publish-batch (session batch)
  "Publish every staged artifact in BATCH while SESSION remains owner."
  (dolist (artifact (plist-get batch :artifacts))
    (unless (mevedel-session-durability--renew-publication-lease session)
      (user-error "Portable session lease was lost during publication"))
    (mevedel-session-publication--publish-artifact artifact)
    (unless (mevedel-session-durability--renew-publication-lease session)
      (user-error "Portable session lease was lost during publication")))
  batch)

(defun mevedel-session-publication--drain (session batches)
  "Publish BATCHES and any batches queued reentrantly for SESSION."
  (let ((remaining batches)
        current
        committed
        result)
    (condition-case err
        (progn
          (while (or remaining
                     (mevedel-session-publication-queue session))
            (unless remaining
              (setq remaining
                    (prog1 (mevedel-session-publication-queue session)
                      (setf (mevedel-session-publication-queue session) nil))))
            (setq current (pop remaining))
            (mevedel-session-publication--publish-batch session current)
            (if-let ((marker
                      (mevedel-session-publication--batch-marker current)))
                (let ((transaction
                       (append
                        (mevedel-session-publication-uncommitted-batches
                         session)
                        (list current))))
                  (setq result
                        (mevedel-session-publication--commit-marker-publication
                         session transaction marker)
                        committed t)
                  (setf
                   (mevedel-session-publication-uncommitted-batches session)
                   nil)
                  (mevedel-session-publication--delete-batches transaction))
              (mevedel-session-publication--retain-uncommitted-batch
               session current)
              (setq result 'published))
            (setq current nil))
          (setf (mevedel-session-pending-publication session) nil)
          (list :committed committed :result result))
      (error
       (let ((recovery
              (delete-dups
               (append
                (mevedel-session-publication-uncommitted-batches session)
                (and current (list current))
                remaining
                (mevedel-session-publication-queue session)))))
         (setf (mevedel-session-publication-uncommitted-batches session) nil
               (mevedel-session-publication-queue session) nil)
         (mevedel-session-publication--record-pending session recovery err))
       (signal (car err) (cdr err))))))

(defun mevedel-session-publication--publish-critical-batches (session batches)
  "Publish BATCHES and discard recovery only after SESSION becomes active."
  (let ((publication-before (mevedel-session-publication session))
        outcome)
    (condition-case err
        (setq outcome
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (setf (mevedel-session-pending-publication session) nil)
                 (mevedel-session-publication--drain session batches))))
      (error
       (unless (mevedel-session-pending-publication session)
         (let* ((committed-p
                 (not (equal publication-before
                             (mevedel-session-publication session))))
                (recovery
                 (cl-remove-if-not
                  #'mevedel-session-publication--batch-live-p
                  (delete-dups
                   (append
                    (mevedel-session-publication-uncommitted-batches session)
                    (unless committed-p batches)
                    (mevedel-session-publication-queue session))))))
           (setf (mevedel-session-publication-uncommitted-batches session)
                 nil
                 (mevedel-session-publication-queue session) nil)
           (when committed-p
             (mevedel-session-publication--delete-batches batches))
           (when recovery
             (mevedel-session-publication--record-pending
              session recovery err))))
       ;; A changed captured head proves the marker CAS committed.  Its local
       ;; sources are terminal even when the final lease normalization failed.
       (when (and (not (equal publication-before
                              (mevedel-session-publication session)))
                  (not (mevedel-session-pending-publication session)))
         (setf (mevedel-session-publication-uncommitted-batches session) nil))
       (signal (car err) (cdr err))))
    (plist-get outcome :result)))

(defun mevedel-session-publication-publish (session artifacts)
  "Serialize and publish SESSION's critical ARTIFACTS.

Each artifact is `(:path TARGET-PATH :content STRING [:coding CODING])'.  One
in-session `session.meta.el' may carry `:commit-marker t' and optional
`:replace t' to commit an immutable logical snapshot.  The complete batch is
staged locally before target I/O.  Pre-commit failure retains the staged batch
  in SESSION and blocks later mutation."
  (require 'mevedel-session-recovery)
  (mevedel-session-recovery-refresh session)
  (when (mevedel-session-pending-publication session)
    (user-error "Session has pending publication; retry or abandon it first"))
  (let ((batch (mevedel-session-publication--stage-artifacts
                session artifacts))
        (publication-before (mevedel-session-publication session)))
    (condition-case err
        (progn
          (unless (or (mevedel-session-publication-active-p session)
                      (mevedel-session-durability-lease-renew session))
            (user-error
             "Portable session lease could not be renewed for publication"))
          (unless (mevedel-session-durability-lease-owned-p session)
            (user-error "Portable session mutation requires its live lease"))
          (if (mevedel-session-publication-active-p session)
              (progn
                (setf (mevedel-session-publication-queue session)
                      (append (mevedel-session-publication-queue session)
                              (list batch)))
                'queued)
            (mevedel-session-publication--publish-critical-batches
             session (list batch))))
      (error
       (unless (mevedel-session-pending-publication session)
         (if (equal publication-before
                    (mevedel-session-publication session))
             (mevedel-session-publication--record-pending
              session (list batch) err)
           (mevedel-session-publication--delete-batch batch)))
       (signal (car err) (cdr err))))))

(defun mevedel-session-publication-retry (&optional session)
  "Retry SESSION's pending critical publication."
  (interactive)
  (require 'mevedel-session-recovery)
  (setq session (or session
                    (mevedel-session-publication--current-session)))
  (mevedel-session-recovery-refresh session)
  (let* ((pending (or (mevedel-session-pending-publication session)
                      (user-error "Session has no pending publication")))
         (batches (plist-get pending :batches)))
    (when (plist-get pending :manual-recovery-marker)
      (user-error
       "Pending transaction requires manual recovery at %s"
       (plist-get pending :manual-recovery)))
    (unless batches
      (user-error
       "Pending transaction requires manual recovery at %s"
       (or (plist-get pending :manual-recovery) "its retained target state")))
    (unless (mevedel-session-durability-lease-renew session)
      (user-error "Portable session lease could not be renewed for publication"))
    (unless (mevedel-session-durability-lease-owned-p session)
      (user-error "Portable session mutation requires its live lease"))
    (prog1
        (mevedel-session-publication--publish-critical-batches
         session batches)
      (mevedel-session-recovery-refresh-session-buffers session))))

(defun mevedel-session-publication-abandon (&optional session)
  "Explicitly discard SESSION's pending local recovery material."
  (interactive)
  (require 'mevedel-session-recovery)
  (setq session (or session
                    (mevedel-session-publication--current-session)))
  (mevedel-session-recovery-refresh session)
  (when-let ((pending (mevedel-session-pending-publication session)))
    (let ((specialized-p (plist-get pending :manual-recovery-marker)))
      (unless
          (yes-or-no-p
           (if specialized-p
               (concat
                "Abandon incomplete portable project Rewind recovery? This permanently "
                "deletes its target recovery bytes and marker. Continue? ")
             (concat
              "Abandon pending session publication? The target project effects "
              "remain, but transcript chronology and recovery state may be lost. ")))
        (user-error "Pending publication was not abandoned"))
      (when specialized-p
        (mevedel-session-recovery--abandon session))
      (when-let ((local-recovery
                  (plist-get pending :manual-recovery-local)))
        (condition-case err
            (mevedel-session-recovery--delete-local local-recovery)
          (error
           (user-error "Could not remove local recovery: %s"
                       (error-message-string err)))))
      (mevedel-session-publication-clear-transient session)
      (mevedel-session-recovery-refresh-session-buffers session)
      (display-warning
       'mevedel
       "Pending session publication was explicitly abandoned"
       :warning)
      t)))

(defun mevedel-session-publication-discard-rolled-back (session)
  "Discard SESSION recovery after its caller fully rolled back an operation.

The operation must have started without pending publication and restored all
pre-operation session and project state.  This function touches no target
state or publication head."
  (mevedel-session-publication-clear-transient session)
  (mevedel-session-recovery-refresh-session-buffers session)
  t)

(defun mevedel-session-publication-append-diagnostic (session path content)
  "Atomically append diagnostic CONTENT to PATH for portable project SESSION.

Return nil when a critical publication is active or pending so the caller can
retain CONTENT for a later retry.  Diagnostic failure never creates critical
pending publication; any local staging bytes are discarded before the error is
returned to the caller."
  (unless (and (stringp path) (stringp content))
    (error "Diagnostic publication requires string path and content"))
  (require 'mevedel-session-recovery)
  (mevedel-session-recovery-refresh session)
  (cond
   ((or (mevedel-session-pending-publication session)
        (mevedel-session-publication-active-p session))
    nil)
   ((not (mevedel-session-durability-lease-renew session))
    (error "Portable project diagnostic publication could not renew the session lease"))
   ((not (mevedel-session-durability-lease-owned-p session))
    (error "Portable project diagnostic publication requires the live session lease"))
   (t
    (let (batch diagnostic-error published result)
      (unwind-protect
          (progn
            (condition-case err
                (setq result
                      (mevedel-session-durability-call-with-reserved-lease
                       session
                       (lambda ()
                         (condition-case diagnostic-err
                             (let ((replacement
                                    (concat
                                     (condition-case nil
                                         (mevedel-session-control-fs-read-file
                                          path)
                                       (mevedel-session-control-fs-absent ""))
                                     content)))
                               (setq batch
                                     (mevedel-session-publication--stage-artifacts
                                      session
                                      (list (list :path path
                                                  :content replacement))))
                               (mevedel-session-publication--publish-batch
                                session batch)
                               (setq published t))
                           (error
                            (setq diagnostic-error diagnostic-err)))
                         ;; A critical publisher may have queued while TRAMP
                         ;; handled the diagnostic I/O.  It retains precedence.
                         (when (mevedel-session-publication-queue session)
                           (mevedel-session-publication--drain session nil))
                         published)))
              (error
               (unless (mevedel-session-pending-publication session)
                 (let ((recovery
                        (cl-remove-if-not
                         #'mevedel-session-publication--batch-live-p
                         (delete-dups
                          (append
                           (mevedel-session-publication-uncommitted-batches
                            session)
                           (mevedel-session-publication-queue session))))))
                   (setf
                    (mevedel-session-publication-uncommitted-batches session)
                    nil
                    (mevedel-session-publication-queue session) nil)
                   (when recovery
                     (mevedel-session-publication--record-pending
                      session recovery err))))
               (signal (car err) (cdr err))))
            (when diagnostic-error
              (signal (car diagnostic-error) (cdr diagnostic-error)))
            result)
        (when batch
          (mevedel-session-publication--delete-batch batch)))))))

(defun mevedel-session-publication-status (session)
  "Return SESSION's stable read-only publication status plist."
  (let ((lease (mevedel-session-lease session))
        (pending (mevedel-session-pending-publication session)))
    (list :lease-state (plist-get lease :state)
          :publication-head (plist-get lease :publication-head)
          :pending-publication (and pending t)
          :pending-reason (and pending (plist-get pending :reason))
          :pending-recovery (and pending
                                 (plist-get pending :manual-recovery))
          :pending-recovery-marker
          (and pending (plist-get pending :manual-recovery-marker))
          :pending-recovery-portable
          (and pending (plist-get pending :recovery-portable))
          :authoritative-state-path
          (mevedel-workspace-state-dir
           (mevedel-session-workspace session)))))

(provide 'mevedel-session-publication)

;;; mevedel-session-publication.el ends here
