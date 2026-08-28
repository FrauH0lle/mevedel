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
(declare-function mevedel-session-control-fs-append-file
                  "mevedel-session-control-fs"
                  (path content &optional coding-system))
(declare-function mevedel-session-control-fs-make-directory
                  "mevedel-session-control-fs" (path &optional parents))
(declare-function mevedel-session-control-fs-physical-path
                  "mevedel-session-control-fs" (path))
(declare-function mevedel-session-control-fs-program-value
                  "mevedel-session-control-fs" (result))
(declare-function mevedel-session-control-fs-run-program
                  "mevedel-session-control-fs" (operations))

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p
                  "mevedel-session-codec" (session))
(autoload 'mevedel-session-codec-portable-authority-p "mevedel-session-codec")

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
(defvar mevedel-session-durability--asserted-directories)
(defvar mevedel-session-durability--transaction-clock)

;; `mevedel-session-recovery'
(declare-function mevedel-session-recovery--abandon
                  "mevedel-session-recovery" (session))
(declare-function mevedel-session-recovery--delete-local
                  "mevedel-session-recovery" (path))
(declare-function mevedel-session-recovery-refresh
                  "mevedel-session-recovery" (session))
(declare-function mevedel-session-recovery-refresh-session-buffers
                  "mevedel-session-recovery" (session))
(autoload 'mevedel-session-recovery--abandon "mevedel-session-recovery")
(autoload 'mevedel-session-recovery--delete-local "mevedel-session-recovery")
(autoload 'mevedel-session-recovery-refresh "mevedel-session-recovery")
(autoload 'mevedel-session-recovery-refresh-session-buffers
  "mevedel-session-recovery")

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

;; `mevedel-workspace'
(declare-function mevedel-workspace-state-dir
                  "mevedel-workspace" (workspace))
(autoload 'mevedel-workspace-state-dir "mevedel-workspace")

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
  "Return non-nil when PATH names immutable publication storage.

The shape is pinned to one artifact name directly inside one
generation directory: a manifest entry naming any other spelling under
the publication root -- such as a planted directory a symlink routes
elsewhere -- could route a read through storage the generation's
immutability never covered."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (string-match-p
        "\\`\\.publications/generation-[0-9a-f]\\{20\\}/[^/]+\\'"
        path)))

(defun mevedel-session-publication--within-p (path directory)
  "Return non-nil when canonical PATH lies strictly below canonical DIRECTORY.

Both arguments have already been through the control filesystem's canonical
spelling, which expands them and collapses any `..', so containment is a
pathname question and this answers exactly that.

`file-in-directory-p' answers a different question -- whether the two names
*resolve* into each other -- by calling `file-truename' on both.  On a target
that is several round trips per artifact, measured at roughly a fifth of a
remote turn's processor time, and it reports a resolution the target is free
to change before the operation runs.  The spelling that matters is proved
target-side instead, in the same process that performs the operation."
  (let ((directory (file-name-as-directory directory)))
    (and (string-prefix-p directory path)
         (> (length path) (length directory)))))

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
         ;; The containment proof compares spellings, so the candidate is
         ;; taken in the same physical spelling as the roots: a session
         ;; directory carrying a doubled slash must not defeat the prefix
         ;; test, and a relative carrying dot components must not pass it.
         (path (mevedel-session-control-fs-physical-path
                (file-name-concat session-dir relative))))
    (mevedel-session-control-fs-physical-path root)
    (unless (and (not (equal (directory-file-name root)
                             (directory-file-name session-root)))
                 (mevedel-session-publication--within-p root session-root))
      (error "Session publication root escapes session storage: %s" root))
    (unless (mevedel-session-publication--within-p path root)
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

(defun mevedel-session-publication--content-sha256 (artifact)
  "Return the digest ARTIFACT's staged bytes will have once written.

Encoding mirrors `mevedel-session-publication--stage-artifacts' exactly, so
this answers what the manifest would record without staging anything."
  (let ((content (plist-get artifact :content)))
    (secure-hash
     'sha256
     (if (multibyte-string-p content)
         (encode-coding-string
          content (or (plist-get artifact :coding) 'utf-8-unix))
       content))))

(defun mevedel-session-publication--comparable-p (session)
  "Return non-nil when SESSION's captured manifest can answer for its bytes.

A queued or retained batch, or pending recovery, means the session owes the
target a transaction whatever any comparison says."
  (and (mevedel-session-publication session)
       (null (mevedel-session-publication-queue session))
       (null (mevedel-session-publication-uncommitted-batches session))
       (null (mevedel-session-pending-publication session))))

(defun mevedel-session-publication--artifact-committed-p
    (session entries artifact)
  "Return non-nil when ARTIFACT matches SESSION's committed manifest ENTRIES.

An artifact outside the session directory is an ordinary fenced write that
never enters the snapshot, so it can never be answered from a manifest."
  (let* ((normalized
          (mevedel-session-publication--artifact-for-session session artifact))
         (logical (plist-get normalized :logical))
         (entry (and logical (cdr (assoc logical entries)))))
    (and entry
         (equal (plist-get entry :sha256)
                (mevedel-session-publication--content-sha256 normalized)))))

(defun mevedel-session-publication-committed-p (session artifacts)
  "Return non-nil when SESSION already committed ARTIFACTS byte for byte.

Only the captured manifest is consulted, so the answer costs no target I/O."
  (and (mevedel-session-publication--comparable-p session)
       (let ((entries
              (plist-get (mevedel-session-publication session) :artifacts)))
         (cl-every
          (lambda (artifact)
            (mevedel-session-publication--artifact-committed-p
             session entries artifact))
          artifacts))))

(defun mevedel-session-publication-prune-committed (session artifacts)
  "Return ARTIFACTS without those SESSION already committed byte for byte.

The manifest overlays the previous one, so an omitted artifact keeps its
existing entry and the resulting logical snapshot is unchanged.  Publication
proves ownership once per artifact written, so dropping bytes the target
already holds removes whole ownership round trips rather than only writes.
The commit marker always survives: it is the transaction's commit point, not
a payload."
  (if (not (mevedel-session-publication--comparable-p session))
      artifacts
    (let ((entries
           (plist-get (mevedel-session-publication session) :artifacts)))
      (seq-filter
       (lambda (artifact)
         (or (plist-get artifact :commit-marker)
             (plist-get artifact :replace)
             (not (mevedel-session-publication--artifact-committed-p
                   session entries artifact))))
       artifacts))))

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

(defcustom mevedel-session-publication-keep-recent-generations 3
  "How many newest published generations a collection always retains.

Collection keeps every settled-turn head, but a reader that resolved a
head moments before it stopped being current still needs its bytes: a
follower re-reads the owner's current head rather than pinning it.  This
grace window is what makes that race harmless in practice; it is not a
lock, and a reader holding an older non-boundary head can still lose it."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-session-publication-summary-scan-max 40
  "How many published sidecars a summary scan reads, newest first.

A sidecar carries the turn count and prompt index a turn-shaped picker
needs, and it is the largest artifact in a generation.  Generations past
this bound are summarized from their file facts alone."
  :type 'integer
  :group 'mevedel)

(defun mevedel-session-publication--generation-names (session-dir)
  "Return SESSION-DIR's published generation directory names, newest first."
  (let* ((root (file-name-concat session-dir ".publications"))
         (paths (mevedel-session-control-fs-list-directory
                 root "\\`generation-[0-9a-f]+\\'")))
    (sort
     (delq nil
           (mapcar
            (lambda (path)
              (let* ((name (file-name-nondirectory
                            (directory-file-name path)))
                     (manifest (file-name-concat path "manifest.el"))
                     (attributes (file-attributes manifest)))
                (when attributes
                  (list :name name
                        :head (file-name-concat
                               ".publications" name "manifest.el")
                        :time (file-attribute-modification-time attributes)))))
            paths))
     (lambda (left right)
       ;; Target filesystems report coarse timestamps, so publishes inside
       ;; one second would otherwise order arbitrarily from run to run.
       ;; Any of them is an equally valid representative of the same
       ;; state; the name only makes the choice reproducible.
       (if (time-equal-p (plist-get left :time) (plist-get right :time))
           (string> (plist-get left :name) (plist-get right :name))
         (time-less-p (plist-get right :time) (plist-get left :time)))))))

(defun mevedel-session-publication--manifest-references (manifest)
  "Return generation names MANIFEST resolves artifacts through.

A committed manifest carries unchanged entries forward verbatim, so it
points into the generations that first wrote those bytes.  Those
generations are as live as the head itself."
  (let (names)
    (dolist (entry (plist-get manifest :artifacts) (delete-dups names))
      (when-let* ((published (plist-get (cdr entry) :published))
                  ((string-match
                    "\\`\\.publications/\\(generation-[0-9a-f]+\\)/"
                    published)))
        (push (match-string 1 published) names)))))

(defun mevedel-session-publication--sidecar-facts
    (session-dir manifest)
  "Return MANIFEST's turn facts, or nil when its sidecar is unreadable.

The returned plist carries `:turn-count', `:segment', `:turn',
`:preview', and `:fork-point-id' for the head's latest prompt."
  (when-let*
      ((entry (cdr (assoc "session.meta.el"
                          (plist-get manifest :artifacts))))
       (published (plist-get entry :published))
       (sidecar (ignore-errors
                  (mevedel-session-durability--read-plist
                   (mevedel-session-publication--publication-path
                    session-dir published)))))
    (let (latest)
      (dolist (segment (plist-get sidecar :prompt-index))
        (dolist (prompt (cdr segment))
          (when (or (null latest)
                    (> (or (plist-get prompt :cum-turn) 0)
                       (or (plist-get latest :cum-turn) 0)))
            (setq latest (append prompt (list :segment (car segment)))))))
      (list :turn-count (or (plist-get sidecar :total-turn-count) 0)
            :segment (or (plist-get latest :segment)
                         (plist-get sidecar :current-segment))
            :turn (plist-get latest :turn)
            ;; The whole prompt travels with the summary so a picker can
            ;; label a published head exactly as it labels a live turn.
            :prompt latest
            :fork-point-id (plist-get latest :fork-point-id)))))

(defun mevedel-session-publication-head-facts (session-dir head)
  "Return HEAD's turn facts, or nil when its publication is unreadable."
  (when-let* ((manifest
               (ignore-errors
                 (mevedel-session-publication--read-publication-raw
                  session-dir head))))
    (mevedel-session-publication--sidecar-facts
     session-dir manifest)))

(defvar mevedel-session-publication--facts-cache
  (make-hash-table :test #'equal)
  "Cache of published generation turn facts, keyed by generation path.

A committed generation is immutable, so its sidecar's answers cannot
change: caching them is sound, and it is what makes collection's
every-sidecar scan affordable.  Reading 752 uncached sidecars measured 17
seconds inside one turn settlement.  Entries live for this Emacs session
or until collection removes their generation.")

(defvar mevedel-session-publication--manifest-cache
  (make-hash-table :test #'equal)
  "Cache of published generation manifests, keyed by generation path.

Immutable for the same reason as the facts cache, and cached only for
listing and collection: authority reads keep going through
`mevedel-session-publication-read', which verifies bytes rather than
trusting a remembered plist.")

(defun mevedel-session-publication--cached-manifest (session-dir head)
  "Return HEAD's manifest below SESSION-DIR, caching the parse."
  (let ((key (mevedel-session-publication--publication-path
              session-dir head)))
    (or (gethash key mevedel-session-publication--manifest-cache)
        (when-let* ((manifest
                     (ignore-errors
                       (mevedel-session-publication--read-publication-raw
                        session-dir head))))
          (puthash key manifest mevedel-session-publication--manifest-cache)
          manifest))))

(defun mevedel-session-publication--cached-sidecar-facts
    (session-dir head manifest)
  "Return HEAD's turn facts for MANIFEST below SESSION-DIR, caching them."
  (let ((key (mevedel-session-publication--publication-path
              session-dir head)))
    (or (gethash key mevedel-session-publication--facts-cache)
        (when-let* ((facts (mevedel-session-publication--sidecar-facts
                            session-dir manifest)))
          (puthash key facts mevedel-session-publication--facts-cache)
          facts))))

(defun mevedel-session-publication-generation-summaries
    (session-dir &optional limit)
  "Return newest-first summaries of SESSION-DIR's published generations.

Each summary carries `:head', `:time', `:transcript-bytes', the manifest's
generation `:references', and -- for
the newest LIMIT generations, whose sidecars are read -- the turn facts
from `mevedel-session-publication--sidecar-facts'.  LIMIT defaults to
`mevedel-session-publication-summary-scan-max'; a sidecar is the largest
artifact in a generation, so scanning every one of them is a cost a
listing does not need to pay."
  (let ((limit (or limit mevedel-session-publication-summary-scan-max))
        (index 0)
        summaries)
    (dolist (generation (mevedel-session-publication--generation-names
                         session-dir)
                        (nreverse summaries))
      (let* ((head (plist-get generation :head))
             (manifest
              (mevedel-session-publication--cached-manifest session-dir head))
             (facts (when (< index limit)
                      (and manifest
                           (mevedel-session-publication--cached-sidecar-facts
                            session-dir head manifest)))))
        (setq index (1+ index))
        (push (append (list :head head
                            :name (plist-get generation :name)
                            :time (plist-get generation :time)
                            :manifest-readable-p (and manifest t)
                            :transcript-bytes
                            (and manifest
                                 (mevedel-session-publication--transcript-bytes
                                  session-dir manifest))
                            :references
                            (and manifest
                                 (mevedel-session-publication--manifest-references
                                  manifest)))
                      facts)
              summaries)))))

(defun mevedel-session-publication--transcript-bytes (session-dir manifest)
  "Return the byte size of MANIFEST's segment transcript, or nil."
  (when-let*
      ((entry (cdr (assoc "segment-0001.chat.org"
                          (plist-get manifest :artifacts))))
       (published (plist-get entry :published))
       (attributes
        (file-attributes (mevedel-session-publication--publication-path
                          session-dir published))))
    (file-attribute-size attributes)))

(defun mevedel-session-publication-settled-summary-p (summary)
  "Return non-nil when SUMMARY names a settled turn state.

A prompt is recorded when its turn is admitted and the turn count when
that turn settles, so a generation published while a response streamed
carries a latest prompt one turn ahead of its count.  Such a head
restores a half-arrived turn: it is the state a crashed owner resumes
from, never a state anyone chooses."
  (let ((turn-count (plist-get summary :turn-count))
        (prompt (plist-get summary :prompt)))
    (and turn-count
         (or (null prompt)
             (= (or (plist-get prompt :cum-turn) 0) turn-count)))))

(defun mevedel-session-publication--retained-generations (summaries)
  "Return generation names a collection must keep, given SUMMARIES.

Three kinds of head are retained: the newest generation per distinct
settled turn state, which is what a restore targets; the newest
`mevedel-session-publication-keep-recent-generations' regardless, which
covers a reader that resolved a head just before it stopped being
current; and every generation those heads resolve artifacts through,
because a manifest carries unchanged entries forward rather than copying
their bytes."
  (let ((seen (make-hash-table :test #'equal))
        (index 0)
        heads)
    (dolist (summary summaries)
      (let* ((settled (mevedel-session-publication-settled-summary-p summary))
             (turn-count (plist-get summary :turn-count))
             (key (and settled
                       (list turn-count (plist-get summary :fork-point-id)))))
        (when (or (< index mevedel-session-publication-keep-recent-generations)
                  (null turn-count)
                  (and settled (not (gethash key seen))))
          (when key (puthash key t seen))
          (push summary heads))
        (setq index (1+ index))))
    (let ((names (mapcar (lambda (summary) (plist-get summary :name)) heads)))
      (dolist (summary heads (delete-dups names))
        (setq names
              (append names (plist-get summary :references)))))))

(defun mevedel-session-publication-collect-generations (session)
  "Delete SESSION's published generations no retained head needs.

Retention is `mevedel-session-publication--retained-generations' plus
the session's current head and everything it resolves through.  Only a
portable session owning its lease may collect: the deletion is a target
mutation, and the current head must not move underneath it.  Every
collectible generation is deleted in one batched target program, so one
pass reclaims everything regardless of backlog size.  Collection is
best-effort: failures warn and return nil rather than breaking turn
settlement or session restore.  Returns the number of generations
deleted."
  (condition-case error
      (when (and (mevedel-session-codec-portable-authority-p session)
                 (mevedel-session-durability-lease-owned-p session)
                 (not (mevedel-session-pending-publication session))
                 (null (mevedel-session-publication-queue session))
                 (not (mevedel-session-publication-active-p session)))
        (let* ((session-dir (mevedel-session-save-path session))
               (current
                (or (plist-get (mevedel-session-publication session) :head)
                    (mevedel-session-durability-publication-head session-dir))))
          (when (and session-dir current)
            (let* ((summaries
                    ;; A picker may bound sidecar reads, but collection needs
                    ;; turn facts for every candidate: coarse target timestamps
                    ;; make "the newest N" unreliable when publishes share a
                    ;; second, so an unscanned generation the current head still
                    ;; needs could fall outside a window.  The immutable facts
                    ;; cache is what keeps scanning every sidecar affordable.
                    (mevedel-session-publication-generation-summaries
                     session-dir most-positive-fixnum))
                   (current-name
                    (file-name-nondirectory
                     (directory-file-name (file-name-directory current))))
                   (current-summary
                    (seq-find
                     (lambda (summary)
                       (equal current-name (plist-get summary :name)))
                     summaries))
                   (deleted 0))
              (when (plist-get current-summary :manifest-readable-p)
                (let* ((retained
                        (append
                         (list current-name)
                         (plist-get current-summary :references)
                         (mevedel-session-publication--retained-generations
                          summaries)))
                       (collectible
                        (seq-remove
                         (lambda (generation)
                           (member (plist-get generation :name) retained))
                         (mevedel-session-publication--generation-names
                          session-dir)))
                       (results
                        (mevedel-session-control-fs-delete-directories
                         (mapcar
                          (lambda (generation)
                            (mevedel-session-publication--publication-path
                             session-dir
                             (file-name-concat
                              ".publications"
                              (plist-get generation :name))))
                          collectible))))
                  (cl-loop for generation in collectible
                           for result in results
                           do
                           (if (eq (plist-get result :status) 'ok)
                               (let ((key
                                      (mevedel-session-publication--publication-path
                                       session-dir
                                       (plist-get generation :head))))
                                 (remhash
                                  key
                                  mevedel-session-publication--manifest-cache)
                                 (remhash
                                  key mevedel-session-publication--facts-cache)
                                 (setq deleted (1+ deleted)))
                             (display-warning
                              'mevedel
                              (format
                               "Could not collect publication generation %s: %s"
                               (plist-get generation :name)
                               (or (plist-get result :diagnostic)
                                   (plist-get result :status)))
                              :warning)))
                  deleted))))))
    (error
     (display-warning
      'mevedel
      (format "Could not collect published generations: %s"
              (error-message-string error))
      :warning)
     nil)))

(defun mevedel-session-publication-read (session-dir &optional head)
  "Return SESSION-DIR's captured immutable publication, or nil.

HEAD reads that published generation instead of the session's current
lease head; every published head remains immutable, so an older one reads
exactly as it was committed.

Manifest structure and control paths are validated uncached.  Only the
sidecar bytes are verified eagerly; consumers verify other artifact hashes
when they read those artifacts."
  (let* ((remote-file-name-inhibit-cache t)
         (_ (mevedel-session-durability--assert-no-pid-lock session-dir))
         (publication
          (mevedel-session-publication--read-publication-raw
           session-dir head))
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

(defun mevedel-session-publication--generation-name ()
  "Return a fresh unique immutable generation directory name."
  (format "generation-%s"
          (substring
           (secure-hash 'sha256
                        (format "%S" (list (current-time)
                                           (random most-positive-fixnum))))
           0 20)))

(defun mevedel-session-publication--immutable-entry
    (directory artifact index session-dir)
  "Return ARTIFACT number INDEX's manifest entry and bytes below DIRECTORY.

The digest is taken from the staged source, which is what the manifest
records, and the bytes are returned for the caller to write."
  (let* ((source (plist-get artifact :source))
         (target (file-name-concat directory (format "%06d.data" index))))
    (list :path target
          :content (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally source)
                     (buffer-string))
          :entry (list (plist-get artifact :logical)
                       :published (file-relative-name target session-dir)
                       :sha256
                       (mevedel-session-publication--file-sha256 source)))))

(defun mevedel-session-publication--write-generation
    (root artifacts session-dir entries)
  "Create one fresh generation below ROOT holding ARTIFACTS and its manifest.

Return a cons of the generation directory and the overlaid manifest ENTRIES.
The directory, every immutable artifact, and the manifest are one target
process: they all land inside the single directory this call creates, so one
pinned parent covers the whole generation.  The manifest is the last operation
in the program, which keeps it the last byte written inside the generation.
A name that already exists ends the program before anything is written, so a
collision simply picks another name."
  (catch 'written
    (dotimes (_ 8)
      (let* ((directory
              (file-name-as-directory
               (file-name-concat
                root (mevedel-session-publication--generation-name))))
             (payloads
              (cl-loop for artifact in artifacts
                       for index from 1
                       collect (mevedel-session-publication--immutable-entry
                                directory artifact index session-dir)))
             (overlaid entries)
             manifest manifest-path operations results)
        (dolist (payload payloads)
          (setq overlaid
                (mevedel-session-publication--overlay-entry
                 overlaid (plist-get payload :entry))))
        (setq manifest (list :sidecar "session.meta.el" :artifacts overlaid)
              manifest-path (file-name-concat directory "manifest.el"))
        (mevedel-session-publication--validate-manifest manifest manifest-path)
        (setq operations
              (append
               ;; The generations root may already exist; the generation
               ;; itself must not, which is the collision signal below.
               (list (list :op 'make-directory
                           :path (directory-file-name
                                  (file-name-as-directory root))
                           :optional t)
                     (list :op 'make-directory
                           :path (directory-file-name directory)))
               (mapcar (lambda (payload)
                         (list :op 'create
                               :path (plist-get payload :path)
                               :content (plist-get payload :content)
                               :coding 'no-conversion))
                       payloads)
               (list (list :op 'create :path manifest-path
                           :content
                           (mevedel-session-durability--record-bytes
                            manifest)))))
        (setq results (mevedel-session-control-fs-run-program operations))
        (cond
         ;; A taken generation name simply picks another one.
         ((eq 'conflict (plist-get (nth 1 results) :status)) nil)
         (t
          ;; The optional root ensure is allowed to report a conflict.
          (dolist (result (cdr results))
            (unless (eq 'ok (plist-get result :status))
              (mevedel-session-control-fs-program-value result)))
          (throw 'written (cons directory overlaid))))))
    (error "Could not create a fresh session publication directory: %s"
           root)))

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
         written directory)
    (mevedel-session-control-fs-physical-path root)
    ;; Immutable generations grow until session-directory cleanup.
    (setq written (mevedel-session-publication--write-generation
                   root artifacts session-dir entries)
          directory (car written)
          entries (cdr written))
    (let* ((manifest-path (file-name-concat directory "manifest.el"))
           (head (file-relative-name manifest-path session-dir))
           (raw (list :head head
                      :sidecar "session.meta.el"
                      :artifacts entries))
           (captured
            (mevedel-session-publication--capture-publication
             session-dir raw)))
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

(defvar mevedel-session-publication--ensured-directories nil
  "Cons cell collecting directories this publication created, or nil.

Every artifact write ensures its own parent, and a publication writes several
artifacts into the same few directories.  Re-creating a directory that this
transaction already created is one target process for a known answer.  A
caller that spans one transaction binds this to a fresh `(list nil)'; outside
such a binding every call reaches the target.")

(defun mevedel-session-publication--publish-artifact (artifact)
  "Publish staged ARTIFACT through a nearby target temporary file.

Ensuring the parent and writing the bytes are one target process.  The ensure
is optional so an existing directory does not end the program, which is what
lets both share one round trip.  That single-level ensure cannot create a
chain of missing parents -- each pinned operation needs its own live parent
directory -- so a write the chain refuses builds the chain component by
component and retries once."
  (let* ((path (plist-get artifact :path))
         (source (plist-get artifact :source))
         (directory (file-name-directory (expand-file-name path)))
         (ensure (not (member directory
                              (car mevedel-session-publication--ensured-directories))))
         (content (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally source)
                    (buffer-string)))
         (write-operation
          (list :op 'write
                :path (mevedel-session-control-fs-physical-path path)
                :content content
                :coding 'no-conversion))
         result)
    (mevedel-session-control-fs-physical-path directory)
    (setq result
          (car (last (mevedel-session-control-fs-run-program
                      (append
                       (when ensure
                         (list (list :op 'make-directory
                                     :path (directory-file-name directory)
                                     :optional t)))
                       (list write-operation))))))
    (when (eq 'absent (plist-get result :status))
      (mevedel-session-control-fs-make-directory
       (directory-file-name directory) t)
      (setq result
            (car (mevedel-session-control-fs-run-program
                  (list write-operation)))))
    (when (and ensure mevedel-session-publication--ensured-directories)
      (push directory
            (car mevedel-session-publication--ensured-directories)))
    (mevedel-session-control-fs-program-value result)))

(defun mevedel-session-publication--publish-batch (session batch)
  "Publish every staged artifact in BATCH while SESSION remains owner."
  (let ((artifacts (plist-get batch :artifacts)))
    (dolist (artifact artifacts)
      (unless (mevedel-session-durability--renew-publication-lease session)
        (user-error "Portable session lease was lost during publication"))
      (mevedel-session-publication--publish-artifact artifact))
    ;; Ownership is proved immediately before every write and once after the
    ;; last one.  Renewing after each write as well only repeated the next
    ;; iteration's proof, with no target write in between, and a renewal is
    ;; itself several target round trips.
    (when artifacts
      (unless (mevedel-session-durability--renew-publication-lease session)
        (user-error "Portable session lease was lost during publication"))))
  batch)

(defun mevedel-session-publication--drain (session batches)
  "Publish BATCHES and any batches queued reentrantly for SESSION."
  (let ((remaining batches)
        (mevedel-session-publication--ensured-directories (list nil))
        ;; One drain is one transaction: the per-artifact lease renewals
        ;; inside it share the clock reading and the pid-lock assertions,
        ;; which is what lets a renewal assume instead of re-observing.
        ;; Read tolerantly: the durability module loads lazily.
        (mevedel-session-durability--transaction-clock
         (or (bound-and-true-p mevedel-session-durability--transaction-clock)
             (list nil)))
        (mevedel-session-durability--asserted-directories
         (or (bound-and-true-p
              mevedel-session-durability--asserted-directories)
             (list nil)))
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

(defun mevedel-session-publication-publish
    (session artifacts &optional require-commit)
  "Serialize and publish SESSION's critical ARTIFACTS.

Each artifact is `(:path TARGET-PATH :content STRING [:coding CODING])'.  One
in-session `session.meta.el' may carry `:commit-marker t' and optional
`:replace t' to commit an immutable logical snapshot.  The complete batch is
staged locally before target I/O.  Pre-commit failure retains the staged batch
in SESSION and blocks later mutation.  When REQUIRE-COMMIT is non-nil, reject
reentrant queueing so the caller returns only after its own batch commits, and
treat a post-commit cleanup error as diagnostic.  Ordinary callers receive
that error so their lifecycle owner can classify it."
  (mevedel-session-recovery-refresh session)
  (when (mevedel-session-pending-publication session)
    (user-error "Session has pending publication; retry or abandon it first"))
  (when (and require-commit
             (mevedel-session-publication-active-p session))
    (user-error "Required session publication cannot be queued"))
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
       (let ((committed-p
              (not (equal publication-before
                          (mevedel-session-publication session)))))
         (unless (mevedel-session-pending-publication session)
           (if (not committed-p)
               (mevedel-session-publication--record-pending
                session (list batch) err)
             (mevedel-session-publication--delete-batch batch)))
         (if (and committed-p require-commit)
             (progn
               (display-warning
                'mevedel
                (format "Session publication committed before cleanup failed: %s"
                        (error-message-string err))
                :warning)
               (mevedel-session-publication session))
           (signal (car err) (cdr err))))))))

(defun mevedel-session-publication-retry (&optional session)
  "Retry SESSION's pending critical publication."
  (interactive)
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

(defvar mevedel-session-publication--diagnostic-batch nil
  "Non-nil while diagnostic appends share one caller-held reservation.

`open' means the caller holds a reserved lease and an append should write
straight to the target.  `unavailable' means the caller established that
the lease cannot carry diagnostics now, so every append in the batch
declines without re-testing.")

(defun mevedel-session-publication-call-with-diagnostic-batch (session function)
  "Call FUNCTION with SESSION's diagnostic appends sharing one lease.

Each `mevedel-session-publication-append-diagnostic\' otherwise pays a
whole transaction of its own: a recovery refresh, a lease renewal, an
ownership reading, then a reservation that renews the lease again on entry
and commits it again on exit.  That is several control-filesystem programs
per append, and a flush point appends once per diagnostic log.  A profiled
remote turn put 96% of its lease reservations here, and those reservations
at 21% of the session\'s CPU -- to persist data this module itself
describes as best-effort and read by nobody live.

The preamble runs once, the appends run inside one reservation, and the
publication queue drains once at the end instead of after each append.

An append that fails inside the batch declines rather than signalling, so
one unwritable log cannot abort the others; its caller retains the content
for the next flush, which is what a nil return already means to it."
  (cond
   (mevedel-session-publication--diagnostic-batch (funcall function))
   ;; A session that does not publish through the lease writes its
   ;; diagnostics directly, so reserving one would renew a lease nothing
   ;; here is going to use -- target I/O, and a warning when the session
   ;; has no lease to renew.
   ((not (mevedel-session-codec-portable-authority-p session))
    (funcall function))
   ((progn (mevedel-session-recovery-refresh session)
           (or (mevedel-session-pending-publication session)
               (mevedel-session-publication-active-p session)
               (not (mevedel-session-durability-lease-renew session))
               (not (mevedel-session-durability-lease-owned-p session))))
    (let ((mevedel-session-publication--diagnostic-batch 'unavailable))
      (funcall function)))
   (t
    (mevedel-session-durability-call-with-reserved-lease
     session
     (lambda ()
       (prog1 (let ((mevedel-session-publication--diagnostic-batch 'open))
                (funcall function))
         ;; A critical publisher may have queued while TRAMP handled the
         ;; diagnostic I/O.  It retains precedence.
         (when (mevedel-session-publication-queue session)
           (mevedel-session-publication--drain session nil))))))))

(defmacro mevedel-session-publication-with-diagnostic-batch (session &rest body)
  "Run BODY with SESSION's diagnostic appends sharing one reserved lease."
  (declare (indent 1) (debug t))
  `(mevedel-session-publication-call-with-diagnostic-batch
    ,session (lambda () ,@body)))

(defun mevedel-session-publication-append-diagnostic (session path content)
  "Append diagnostic CONTENT to PATH for portable project SESSION.

Return nil when the lease cannot carry a diagnostic right now -- a critical
publication is active or pending, renewal is momentarily unavailable, or the
lease is waiting to be reacquired -- so the caller retains CONTENT for a
later retry.  A diagnostic is best-effort data nothing reads live, so an
unavailable moment is a quiet retry, not an error echoed per attempt.
Diagnostic failure never creates critical pending publication.

The append is one pinned target operation carrying only the delta.
Republishing the whole file per flush was quadratic in stream size and
dominated remote-save allocations once a log grew to megabytes."
  (unless (and (stringp path) (stringp content))
    (error "Diagnostic publication requires string path and content"))
  (cond
   ((eq mevedel-session-publication--diagnostic-batch 'unavailable) nil)
   ((eq mevedel-session-publication--diagnostic-batch 'open)
    ;; The caller holds the reservation and drains the queue itself.
    (mevedel-session-publication--artifact-for-session
     session (list :path path :content content))
    (condition-case err
        (progn (mevedel-session-control-fs-append-file path content) t)
      (error
       (mevedel--warn-once
        (list 'diagnostic-append path)
        "Diagnostic append failed for %s, retaining for retry: %s"
        path (error-message-string err))
       nil)))
   ((progn
      (mevedel-session-recovery-refresh session)
      (or (mevedel-session-pending-publication session)
          (mevedel-session-publication-active-p session)
          (not (mevedel-session-durability-lease-renew session))
          (not (mevedel-session-durability-lease-owned-p session))))
    nil)
   (t
    ;; Validate that the path stays inside the session before any
    ;; target I/O; :content is not staged, only checked for shape.
    (mevedel-session-publication--artifact-for-session
     session (list :path path :content content))
    (let (diagnostic-error)
      (prog1
          (condition-case err
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (condition-case diagnostic-err
                     (mevedel-session-control-fs-append-file path content)
                   (error
                    (setq diagnostic-error diagnostic-err)))
                 ;; A critical publisher may have queued while TRAMP
                 ;; handled the diagnostic I/O.  It retains precedence.
                 (when (mevedel-session-publication-queue session)
                   (mevedel-session-publication--drain session nil))
                 (not diagnostic-error)))
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
          (signal (car diagnostic-error) (cdr diagnostic-error))))))))

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
