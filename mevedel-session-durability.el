;;; mevedel-session-durability.el -- Remote session durability -*- lexical-binding: t -*-

;;; Commentary:

;; Owns portable remote-session leases and serialized publication of durable
;; session artifacts.  Local sessions keep their existing PID lock behavior.

;;; Code:

(eval-when-compile (require 'mevedel-structs))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `mevedel-execution-target'
(declare-function mevedel-execution-target-identity
                  "mevedel-execution-target" (cl-x) t)
(declare-function mevedel-execution-target-remote-p
                  "mevedel-execution-target" (target))

;; `mevedel-structs'
(declare-function mevedel-session-execution-target
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-lease-renewal-timer
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-pending-publication
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-active-p
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-queue
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-publication-uncommitted-batches
                  "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-save-path "mevedel-structs" (cl-x) t)
(declare-function mevedel-session-workspace "mevedel-structs" (cl-x) t)
(declare-function mevedel-workspace-state-dir "mevedel-structs" (workspace))
(defvar mevedel--data-buffer)
(defvar mevedel--session)


;;
;;; Customization

(defcustom mevedel-session-lease-seconds 90
  "Seconds for which one remote session lease grants mutation authority."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-session-lease-renewal-seconds 30
  "Seconds between remote session lease renewal attempts."
  :type 'number
  :group 'mevedel)

(defcustom mevedel-session-publication-lease-seconds 3600
  "Seconds reserved for each uninterrupted publication artifact operation.

The serialized publisher renews this window before and after every artifact.
A single target filesystem operation that outlasts it loses mutation authority
and the publication fails closed at the next ownership check."
  :type 'number
  :group 'mevedel)


;;
;;; Target disclosure and leases

(defvar mevedel-session-durability--client-id
  (secure-hash
   'sha256
   (format "%S"
           (list (current-time) (random most-positive-fixnum)
                 (emacs-pid) (system-name))))
  "Opaque identity of this live mevedel client.")

(defvar mevedel-session-durability--disclosed-targets
  (make-hash-table :test #'equal)
  "Execution targets disclosed to the user in this Emacs process.")

(defun mevedel-session-durability--current-session ()
  "Return the session reached from the current data or view buffer."
  (or (and (boundp 'mevedel--session) mevedel--session)
      (and (boundp 'mevedel--data-buffer)
           (buffer-live-p mevedel--data-buffer)
           (buffer-local-value 'mevedel--session mevedel--data-buffer))
      (user-error "No active mevedel session")))

(defun mevedel-session-durability-disclose (session)
  "Confirm SESSION's target-side durable storage before its first write.

The acknowledgement is once per target for this Emacs process.  Local
sessions need no disclosure."
  (let ((target (mevedel-session-execution-target session)))
    (when (and target (mevedel-execution-target-remote-p target))
      (let ((key (mevedel-execution-target-identity target)))
        (unless (gethash key mevedel-session-durability--disclosed-targets)
          (unless
              (yes-or-no-p
               (format
                (concat
                 "Store this project's mevedel state on the target at %s? "
                 "This includes conversations, permissions, agent state, "
                 "checkpoints, snapshots, plans, durable tool results, and "
                 "required logs. The data is not encrypted by mevedel. ")
                (mevedel-workspace-state-dir
                 (mevedel-session-workspace session))))
            (user-error "Remote session storage was not accepted"))
          (puthash key t mevedel-session-durability--disclosed-targets)))))
  t)

(defun mevedel-session-durability--lease-path (session-dir)
  "Return SESSION-DIR's portable lease path."
  (file-name-concat session-dir ".lease"))

(defun mevedel-session-durability--read-plist (path)
  "Return the plist stored at PATH, or nil when PATH is absent."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun mevedel-session-durability--write-plist (path plist)
  "Atomically replace PATH with PLIST through a nearby temporary file."
  (let ((temporary (make-nearby-temp-file
                    (expand-file-name ".mevedel-lease-"
                                      (file-name-directory path)))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil)
                  (print-level nil))
              (prin1 plist (current-buffer))))
          (rename-file temporary path t))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun mevedel-session-durability--create-plist (path plist)
  "Atomically create PATH with PLIST, returning non-nil on success."
  (let ((temporary (make-nearby-temp-file
                    (expand-file-name ".mevedel-lease-"
                                      (file-name-directory path)))))
    (unwind-protect
        (progn
          (with-temp-file temporary
            (let ((print-length nil)
                  (print-level nil))
              (prin1 plist (current-buffer))))
          (condition-case nil
              (progn
                (add-name-to-file temporary path nil)
                t)
            (file-already-exists nil)))
      (when (file-exists-p temporary)
        (delete-file temporary)))))

(defun mevedel-session-durability--valid-relative-path-p (path)
  "Return non-nil when PATH is a normalized session-relative file path."
  (and (stringp path)
       (not (string-empty-p path))
       (not (file-name-absolute-p path))
       (not (file-remote-p path))
       (not (string-prefix-p "~" path))
       (not (string-suffix-p "/" path))
       (not (string-match-p "//" path))
       (not (string-match-p
             "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)" path))))

(defun mevedel-session-durability--valid-publication-head-p (head)
  "Return non-nil when HEAD names an immutable publication manifest."
  (and (mevedel-session-durability--valid-relative-path-p head)
       (string-prefix-p ".publications/" head)
       (equal (file-name-nondirectory head) "manifest.el")))

(defun mevedel-session-durability--control-logical-path-p (path)
  "Return non-nil when logical PATH names durability control storage."
  (or (equal path ".lease")
      (string-prefix-p ".lease/" path)
      (equal path ".publications")
      (string-prefix-p ".publications/" path)))

(defun mevedel-session-durability-logical-path-p (path)
  "Return non-nil when PATH is a normalized session artifact path."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (not (mevedel-session-durability--control-logical-path-p path))))

(defun mevedel-session-durability--valid-published-path-p (path)
  "Return non-nil when PATH names immutable publication storage."
  (and (mevedel-session-durability--valid-relative-path-p path)
       (string-prefix-p ".publications/" path)))

(defun mevedel-session-durability--publication-path (session-dir relative)
  "Return RELATIVE below SESSION-DIR's physical publication root."
  (let* ((session-root
          (file-name-as-directory (file-truename session-dir)))
         (root
          (file-name-as-directory
           (file-truename
            (file-name-concat session-dir ".publications"))))
         (path (file-name-concat session-dir relative)))
    (unless (and (not (equal (directory-file-name root)
                             (directory-file-name session-root)))
                 (file-in-directory-p root session-root))
      (error "Session publication root escapes session storage: %s" root))
    (unless (file-in-directory-p (file-truename path) root)
      (error "Session publication path escapes immutable storage: %s" path))
    path))

(defun mevedel-session-durability--artifact-for-session (session artifact)
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
                 (not (mevedel-session-durability-logical-path-p
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

(defun mevedel-session-durability--file-sha256 (path)
  "Return the SHA-256 digest of file PATH's literal bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun mevedel-session-durability--validate-manifest (manifest path)
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
                     (mevedel-session-durability-logical-path-p
                      logical)
                     (proper-list-p properties)
                     (mevedel-session-durability--valid-published-path-p
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

(defun mevedel-session-durability--read-publication-raw
    (session-dir &optional head)
  "Read and validate SESSION-DIR's raw manifest at HEAD or its lease head."
  (let* ((remote-file-name-inhibit-cache t)
         (head (or head
                   (mevedel-session-durability-publication-head session-dir))))
    (when head
      (unless (mevedel-session-durability--valid-publication-head-p head)
        (error "Invalid session publication head: %s" head))
      (let* ((path (mevedel-session-durability--publication-path
                    session-dir head))
             (manifest (mevedel-session-durability--read-plist path)))
        (unless manifest
          (error "Missing session publication manifest: %s" path))
        (setq manifest
              (mevedel-session-durability--validate-manifest manifest path))
        (list :head head
              :sidecar (plist-get manifest :sidecar)
              :artifacts (plist-get manifest :artifacts))))))

(defun mevedel-session-durability--capture-publication
    (session-dir publication)
  "Qualify raw PUBLICATION paths beneath SESSION-DIR for a live client."
  (when publication
    (let* ((artifacts
            (mapcar
             (lambda (entry)
               (let ((copy (copy-tree entry)))
                 (setf (plist-get (cdr copy) :published)
                       (mevedel-session-durability--publication-path
                        session-dir (plist-get (cdr entry) :published)))
                 copy))
             (plist-get publication :artifacts)))
           (sidecar (cdr (assoc "session.meta.el" artifacts))))
      (list :head (plist-get publication :head)
            :sidecar (plist-get sidecar :published)
            :artifacts artifacts))))

(defun mevedel-session-durability-read-publication (session-dir)
  "Return SESSION-DIR's captured immutable publication, or nil.

Manifest structure and control paths are validated uncached.  Only the
sidecar bytes are verified eagerly; consumers verify other artifact hashes
when they read those artifacts."
  (let* ((remote-file-name-inhibit-cache t)
         (publication
          (mevedel-session-durability--read-publication-raw session-dir))
         (captured
          (mevedel-session-durability--capture-publication
           session-dir publication)))
    (when captured
      (let* ((sidecar (plist-get captured :sidecar))
             (entry (cdr (assoc "session.meta.el"
                                (plist-get captured :artifacts))))
             (expected (plist-get entry :sha256)))
        (unless (and (file-regular-p sidecar)
                     (equal expected
                            (mevedel-session-durability--file-sha256 sidecar)))
          (error "Invalid session publication sidecar: %s" sidecar))))
    captured))

(defun mevedel-session-durability--verify-publication-artifacts
    (session-dir publication)
  "Verify every immutable artifact in raw PUBLICATION below SESSION-DIR."
  (dolist (entry (plist-get publication :artifacts))
    (let ((path
           (mevedel-session-durability--publication-path
            session-dir (plist-get (cdr entry) :published))))
      (unless (and (file-regular-p path)
                   (equal (plist-get (cdr entry) :sha256)
                          (mevedel-session-durability--file-sha256 path)))
        (error "Invalid session publication artifact: %s" path))))
  publication)

(defun mevedel-session-durability-seed-publication-base (session head)
  "Seed fresh owned SESSION with copied immutable publication HEAD as a base.

This one-shot operation requires a nil current head, verifies the copied
manifest and every artifact under SESSION's save path, and records only a
transient merge base.  The next sidecar marker performs the child's first
publication-head commit."
  (unless (mevedel-session-durability--valid-publication-head-p head)
    (error "Publication head must name an immutable manifest"))
  (unless (mevedel-session-durability-lease-owned-p session)
    (user-error "Remote session mutation requires its live lease"))
  (when (plist-get (mevedel-session-lease session) :publication-head)
    (error "Session publication head is already initialized"))
  (when (mevedel-session-publication session)
    (error "Session publication base is already initialized"))
  (let* ((session-dir
          (file-name-as-directory (mevedel-session-save-path session)))
         captured)
    (setq captured
          (mevedel-session-durability-call-with-reserved-lease
           session
           (lambda ()
             (when (plist-get (mevedel-session-lease session)
                              :publication-head)
               (error "Session publication head is already initialized"))
             (mevedel-session-durability--capture-publication
              session-dir
              (mevedel-session-durability--verify-publication-artifacts
               session-dir
               (mevedel-session-durability--read-publication-raw
                session-dir head))))))
    (setf (mevedel-session-publication session) captured)
    captured))

(defun mevedel-session-durability--lease-record
    (buffer-name generation &optional status publication-head
                 unsettled-mutation)
  "Return a fresh lease record for BUFFER-NAME and GENERATION.
STATUS defaults to `active'.  PUBLICATION-HEAD is an immutable manifest path.
UNSETTLED-MUTATION records target mutation whose outcome is not yet proved."
  (let ((now (float-time)))
    (list :generation generation
          :status (or status 'active)
          :publication-head publication-head
          :unsettled-mutation (and unsettled-mutation t)
          :client-id mevedel-session-durability--client-id
          :renewed-at now
          :expires-at (+ now mevedel-session-lease-seconds)
          :buffer buffer-name)))

(defun mevedel-session-durability--generation-path (directory generation)
  "Return GENERATION's immutable record path below DIRECTORY."
  (file-name-concat directory (format "%020d.el" generation)))

(defun mevedel-session-durability--generation-paths (directory)
  "Return DIRECTORY's generation record paths in descending order."
  (when (file-directory-p directory)
    (sort (directory-files directory t "\\`[0-9]\\{20\\}\\.el\\'")
          #'string>)))

(defun mevedel-session-durability--generation (path)
  "Return the generation number encoded by lease record PATH."
  (string-to-number (file-name-base path)))

(defun mevedel-session-durability--lease-head (directory)
  "Return DIRECTORY's latest non-aborted lease record."
  (catch 'head
    (dolist (path (mevedel-session-durability--generation-paths directory))
      (let ((record (mevedel-session-durability--read-plist path)))
        (unless (eq 'aborted (plist-get record :status))
          (throw 'head record))))))

(defun mevedel-session-durability--lease-head-before (directory generation)
  "Return DIRECTORY's latest non-aborted record older than GENERATION."
  (catch 'head
    (dolist (path (mevedel-session-durability--generation-paths directory))
      (when (< (mevedel-session-durability--generation path) generation)
        (let ((record (mevedel-session-durability--read-plist path)))
          (unless (eq 'aborted (plist-get record :status))
            (throw 'head record)))))))

(defun mevedel-session-durability--maximum-generation (directory)
  "Return the highest generation allocated below DIRECTORY, or zero."
  (if-let ((path (car (mevedel-session-durability--generation-paths
                       directory))))
      (mevedel-session-durability--generation path)
    0))

(defun mevedel-session-durability--ensure-lease-directory (directory)
  "Ensure that lease generation DIRECTORY exists."
  (cond
   ((file-directory-p directory))
   ((file-exists-p directory)
    (error "Invalid remote session lease: %s" directory))
   (t
    (condition-case nil
        (make-directory directory)
      (file-already-exists nil))
    (unless (file-directory-p directory)
      (error "Could not create remote session lease: %s" directory)))))

(defun mevedel-session-durability--create-generation (directory record)
  "Exclusively create RECORD below lease DIRECTORY."
  (mevedel-session-durability--create-plist
   (mevedel-session-durability--generation-path
    directory (plist-get record :generation))
   record))

(defun mevedel-session-durability--write-generation (directory record)
  "Atomically replace RECORD's own generation below DIRECTORY."
  (mevedel-session-durability--write-plist
   (mevedel-session-durability--generation-path
    directory (plist-get record :generation))
   record))

(defun mevedel-session-durability--prune-generations-before
    (directory generation)
  "Best-effort delete DIRECTORY records older than GENERATION."
  (dolist (path (mevedel-session-durability--generation-paths directory))
    (when (< (mevedel-session-durability--generation path) generation)
      (condition-case nil
          (delete-file path)
        (file-error nil)))))

(defun mevedel-session-durability--same-generation-p (left right)
  "Return non-nil when lease records LEFT and RIGHT name one generation."
  (and left right
       (equal (plist-get left :generation) (plist-get right :generation))
       (equal (plist-get left :client-id) (plist-get right :client-id))))

(defun mevedel-session-durability--cancel-renewal (session)
  "Cancel SESSION's lease-renewal timer, if any."
  (when-let* ((timer (mevedel-session-lease-renewal-timer session))
              ((timerp timer)))
    (cancel-timer timer))
  (setf (mevedel-session-lease-renewal-timer session) nil))

(defun mevedel-session-durability--bind-lease (session lease state)
  "Record LEASE and STATE on SESSION and maintain its renewal timer."
  (when session
    (let ((live (and lease (copy-sequence lease)))
          (release-pending
           (plist-get (mevedel-session-lease session) :release-pending)))
      (when live
        (setq live (plist-put live :state state)))
      (when release-pending
        (setq live (plist-put live :release-pending t)))
      (setf (mevedel-session-lease session) live)
      (if (eq state 'owned)
          (unless (timerp (mevedel-session-lease-renewal-timer session))
            (setf (mevedel-session-lease-renewal-timer session)
                  (run-at-time
                   mevedel-session-lease-renewal-seconds
                   mevedel-session-lease-renewal-seconds
                   #'mevedel-session-durability-lease-renew session)))
        (mevedel-session-durability--cancel-renewal session)))))

(defun mevedel-session-durability--valid-lease-p (lease)
  "Return non-nil when LEASE has the current portable representation."
  (and (proper-list-p lease)
       (natnump (plist-get lease :generation))
       (> (plist-get lease :generation) 0)
       (memq (plist-get lease :status)
             '(claiming active publishing released aborted))
       (stringp (plist-get lease :client-id))
       (string-match-p "\\`[0-9a-f]\\{64\\}\\'"
                       (plist-get lease :client-id))
       (numberp (plist-get lease :renewed-at))
       (numberp (plist-get lease :expires-at))
       (plist-member lease :publication-head)
       (plist-member lease :unsettled-mutation)
       (booleanp (plist-get lease :unsettled-mutation))
       (or (null (plist-get lease :publication-head))
           (mevedel-session-durability--valid-publication-head-p
            (plist-get lease :publication-head)))))

(defun mevedel-session-durability-publication-head (session-dir)
  "Return SESSION-DIR's validated relative publication head, or nil.

This read-only operation bypasses remote file caches and does not acquire or
otherwise mutate the session lease."
  (let* ((remote-file-name-inhibit-cache t)
         (directory (mevedel-session-durability--lease-path session-dir))
         (lease (mevedel-session-durability--lease-head directory)))
    (cond
     ((null lease) nil)
     ((mevedel-session-durability--valid-lease-p lease)
      (plist-get lease :publication-head))
     (t
      (error "Invalid remote session lease: %s" directory)))))

(defun mevedel-session-durability--claim-next
    (directory expected buffer-name
               &optional status unsettled-mutation-p unsettled-mutation)
  "Claim DIRECTORY's next generation after EXPECTED for BUFFER-NAME.

The candidate fences older writers as soon as its exclusive generation file
appears.  It becomes active only if EXPECTED remained byte-for-byte unchanged;
otherwise it is marked aborted and removed best-effort.  STATUS defaults to
`active'.  When UNSETTLED-MUTATION-P is non-nil, the successor records
UNSETTLED-MUTATION instead of preserving EXPECTED's value."
  (let* ((status (or status 'active))
         (generation
          (1+ (mevedel-session-durability--maximum-generation directory)))
         (candidate
          (let ((mevedel-session-lease-seconds
                 (if (eq status 'publishing)
                     mevedel-session-publication-lease-seconds
                   mevedel-session-lease-seconds)))
            (mevedel-session-durability--lease-record
             buffer-name generation 'claiming
             (plist-get expected :publication-head)
             (if unsettled-mutation-p
                 unsettled-mutation
               (plist-get expected :unsettled-mutation))))))
    (when (mevedel-session-durability--create-generation directory candidate)
      (let ((predecessor
             (mevedel-session-durability--lease-head-before
              directory generation)))
        (if (equal expected predecessor)
            (progn
              (setq candidate (plist-put candidate :status status))
              (mevedel-session-durability--write-generation
               directory candidate)
              (mevedel-session-durability--prune-generations-before
               directory generation)
              candidate)
          (setq candidate (plist-put candidate :status 'aborted))
          (mevedel-session-durability--write-generation directory candidate)
          (condition-case nil
              (delete-file
               (mevedel-session-durability--generation-path
                directory generation))
            (file-error nil))
          nil)))))

(defun mevedel-session-durability--owned-lease-record-p (lease)
  "Return non-nil when LEASE is live authority owned by this client."
  (and (mevedel-session-durability--valid-lease-p lease)
       (memq (plist-get lease :status) '(active publishing))
       (equal mevedel-session-durability--client-id
              (plist-get lease :client-id))
       (> (plist-get lease :expires-at) (float-time))))

(defun mevedel-session-durability-lease-acquire
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.

Return non-nil when this client owns mutation authority and nil when another
unexpired client owns it.  Expired takeover requires explicit confirmation.
When SESSION is non-nil, record the resulting lease state on it."
  (let* ((remote-file-name-inhibit-cache t)
         (directory (mevedel-session-durability--lease-path session-dir))
         (_ (mevedel-session-durability--ensure-lease-directory directory))
         (existing (mevedel-session-durability--lease-head directory))
         (lease
          (cond
           ((null existing)
            (mevedel-session-durability--claim-next
             directory nil buffer-name))
           ((not (mevedel-session-durability--valid-lease-p existing))
            (error "Invalid remote session lease: %s" directory))
           ((and (equal mevedel-session-durability--client-id
                        (plist-get existing :client-id))
                 (eq 'active (plist-get existing :status))
                 (> (plist-get existing :expires-at) (float-time)))
            (let ((record
                   (mevedel-session-durability--lease-record
                    buffer-name (plist-get existing :generation) 'active
                    (plist-get existing :publication-head)
                    (plist-get existing :unsettled-mutation))))
              (mevedel-session-durability--write-generation directory record)
              (and (mevedel-session-durability--same-generation-p
                    record
                    (mevedel-session-durability--lease-head directory))
                   record)))
           ((and (equal mevedel-session-durability--client-id
                        (plist-get existing :client-id))
                 (eq 'publishing (plist-get existing :status))
                 (> (plist-get existing :expires-at) (float-time)))
            existing)
           ((and (memq (plist-get existing :status)
                       '(claiming active publishing))
                 (> (plist-get existing :expires-at) (float-time)))
            nil)
           ((eq 'released (plist-get existing :status))
            (mevedel-session-durability--claim-next
             directory existing buffer-name))
           ((not
             (let ((expired-at
                    (format-time-string
                     "%FT%T%z"
                     (seconds-to-time
                      (plist-get existing :expires-at)))))
               (y-or-n-p
                (if (eq 'publishing (plist-get existing :status))
                    (format
                     (concat
                      "Remote session publishing lease expired at %s. "
                      "A critical write may still be in flight; confirm that "
                      "the prior client is stopped before takeover? ")
                     expired-at)
                  (format
                   "Remote session lease expired at %s; take it over? "
                   expired-at)))))
            nil)
           (t
            (mevedel-session-durability--claim-next
             directory existing buffer-name)))))
    (let* ((current (mevedel-session-durability--lease-head directory))
           (owned
            (and lease
                 (equal lease current)
                 (memq (plist-get current :status) '(active publishing))
                 (equal mevedel-session-durability--client-id
                        (plist-get current :client-id))
                 (> (plist-get current :expires-at) (float-time)))))
      (when session
        (mevedel-session-durability--bind-lease
         session current
         (cond
          (owned
           'owned)
          ((and current
                (memq (plist-get current :status)
                      '(claiming active publishing))
                (> (plist-get current :expires-at) (float-time)))
           'foreign)
          (t 'expired))))
      (and owned t))))

(defun mevedel-session-durability--update-owned-lease
    (session accepted-status status seconds
             &optional publication-head expected-publication-head-p
             expected-publication-head unsettled-mutation-p
             unsettled-mutation)
  "Update SESSION's owned lease from ACCEPTED-STATUS to STATUS for SECONDS.
Replace its preserved head with non-nil PUBLICATION-HEAD.  When
EXPECTED-PUBLICATION-HEAD-P is non-nil, require the current head to equal
EXPECTED-PUBLICATION-HEAD.  When UNSETTLED-MUTATION-P is non-nil, replace the
preserved unsettled-mutation flag with UNSETTLED-MUTATION."
  (when-let ((session-dir (mevedel-session-save-path session)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (bound (mevedel-session-lease session))
           (existing
            (condition-case nil
                (if-let ((generation (plist-get bound :generation)))
                    (mevedel-session-durability--read-plist
                     (mevedel-session-durability--generation-path
                      directory generation))
                  (mevedel-session-durability--lease-head directory))
              (error nil)))
           (head (condition-case nil
                     (mevedel-session-durability--lease-head directory)
                   (error nil))))
      (if (and (mevedel-session-durability--valid-lease-p existing)
               (memq (plist-get existing :status) accepted-status)
               (equal mevedel-session-durability--client-id
                      (plist-get existing :client-id))
               (equal existing head)
               (or (not expected-publication-head-p)
                   (equal expected-publication-head
                          (plist-get existing :publication-head)))
               (> (plist-get existing :expires-at) (float-time)))
          (let ((record
                 (let ((mevedel-session-lease-seconds seconds))
                   (mevedel-session-durability--lease-record
                    (plist-get existing :buffer)
                    (plist-get existing :generation)
                    status
                    (or publication-head
                        (plist-get existing :publication-head))
                    (if unsettled-mutation-p
                        unsettled-mutation
                      (plist-get existing :unsettled-mutation))))))
            (mevedel-session-durability--write-generation directory record)
            (if (equal record
                       (mevedel-session-durability--lease-head directory))
                (progn
                  (mevedel-session-durability--bind-lease
                   session record 'owned)
                  t)
              (let ((latest
                     (mevedel-session-durability--lease-head directory)))
                (mevedel-session-durability--bind-lease
                 session latest
                 (if (mevedel-session-durability--owned-lease-record-p latest)
                     'owned
                   'lost)))
              nil))
        (let ((latest (or head existing)))
          (mevedel-session-durability--bind-lease
           session latest
           (if (mevedel-session-durability--owned-lease-record-p latest)
               'owned
             'lost)))
        nil))))

(defun mevedel-session-durability-set-unsettled-mutation (session value)
  "Set SESSION's durable unsettled-mutation latch to boolean VALUE.
Return non-nil only when the current owned lease generation commits VALUE."
  (unless (booleanp value)
    (error "Unsettled mutation latch must be boolean"))
  (when-let ((session-dir (mevedel-session-save-path session)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (bound (mevedel-session-lease session))
           (existing
            (condition-case nil
                (when-let ((generation (plist-get bound :generation)))
                  (mevedel-session-durability--read-plist
                   (mevedel-session-durability--generation-path
                    directory generation)))
              (error nil)))
           (head (mevedel-session-durability--lease-head directory))
           (status (plist-get existing :status)))
      (if (and (equal existing head)
               (mevedel-session-durability--owned-lease-record-p existing))
          (if-let ((successor
                    (mevedel-session-durability--claim-next
                     directory existing (plist-get existing :buffer)
                     status t value)))
              (progn
                (mevedel-session-durability--bind-lease
                 session successor 'owned)
                t)
            (let ((latest (mevedel-session-durability--lease-head directory)))
              (mevedel-session-durability--bind-lease
               session latest
               (if (mevedel-session-durability--owned-lease-record-p latest)
                   'owned
                 'lost))
              nil))
        (mevedel-session-durability--bind-lease
         session (or head existing) 'lost)
        nil))))

(defun mevedel-session-durability-unsettled-mutation-p (session)
  "Return non-nil when SESSION's bound lease records unsettled mutation."
  (and (plist-get (mevedel-session-lease session) :unsettled-mutation) t))

(defun mevedel-session-durability-commit-publication-head (session head)
  "Commit immutable session-relative HEAD while SESSION is the current owner.

Return non-nil only when SESSION's publishing generation remains the exact
lease head through the commit."
  (unless (mevedel-session-durability--valid-publication-head-p head)
    (error "Publication head must name an immutable manifest"))
  (let ((expected
         (plist-get (mevedel-session-lease session) :publication-head)))
    (mevedel-session-durability--update-owned-lease
     session '(publishing) 'publishing
     mevedel-session-publication-lease-seconds head t expected)))

(defun mevedel-session-durability-lease-renew (session)
  "Renew SESSION's unexpired owned lease, returning non-nil on success.

An active publication performs no target I/O here because timer callbacks may
run reentrantly inside a TRAMP file handler.  The serialized publisher owns its
bounded window renewal instead.  After serialization exits, renewal also
normalizes this client's live `publishing' generation back to `active'."
  (if (mevedel-session-publication-active-p session)
      (mevedel-session-durability-lease-owned-p session)
    (condition-case err
        (mevedel-session-durability--update-owned-lease
         session '(active publishing) 'active mevedel-session-lease-seconds)
      (error
       (mevedel-session-durability--bind-lease
        session (mevedel-session-lease session) 'lost)
       (display-warning
        'mevedel
        (format "Remote session lease renewal failed: %s" err)
        :warning)
       nil))))

(defun mevedel-session-durability-lease-owned-p (session)
  "Return non-nil when SESSION has this client's unexpired lease."
  (let ((lease (mevedel-session-lease session)))
    (and (eq 'owned (plist-get lease :state))
         (memq (plist-get lease :status) '(active publishing))
         (equal mevedel-session-durability--client-id
                (plist-get lease :client-id))
         (numberp (plist-get lease :expires-at))
         (> (plist-get lease :expires-at) (float-time)))))

(defun mevedel-session-durability-lease-release (session-dir &optional session)
  "Release this client's portable lease for SESSION-DIR.
An inactive owned `publishing' generation is releasable, but a live publisher
is never interrupted.  When SESSION is non-nil, cancel its renewal and clear
its lease state."
  (if (and session (mevedel-session-publication-active-p session))
      (let ((lease (copy-sequence (mevedel-session-lease session))))
        (setf (mevedel-session-lease session)
              (plist-put lease :release-pending t)))
    (let* ((remote-file-name-inhibit-cache t)
           (directory (mevedel-session-durability--lease-path session-dir))
           (bound (and session (mevedel-session-lease session)))
           (lease
            (condition-case nil
                (if-let ((generation (plist-get bound :generation)))
                    (mevedel-session-durability--read-plist
                     (mevedel-session-durability--generation-path
                      directory generation))
                  (mevedel-session-durability--lease-head directory))
              (error nil))))
      (when (and (mevedel-session-durability--valid-lease-p lease)
                 (memq (plist-get lease :status) '(active publishing))
                 (equal mevedel-session-durability--client-id
                        (plist-get lease :client-id)))
        (setq lease (plist-put lease :status 'released)
              lease (plist-put lease :expires-at (float-time)))
        (mevedel-session-durability--write-generation directory lease))
      (when session
        (mevedel-session-durability--cancel-renewal session)
        (setf (mevedel-session-lease session) nil)
        (when-let ((queued
                    (and (not (mevedel-session-pending-publication session))
                         (mevedel-session-publication-queue session))))
          (let ((batches
                 (append
                  (mevedel-session-publication-uncommitted-batches session)
                  queued)))
            (setf (mevedel-session-publication-uncommitted-batches session) nil
                  (mevedel-session-publication-queue session) nil)
            (mevedel-session-durability--record-pending
             session batches
             '(user-error
               "Session lease released before queued publication completed"))))
        (unless (mevedel-session-pending-publication session)
          (mevedel-session-durability--clear-transient-publication session))))))


;;
;;; Critical publication

(defun mevedel-session-durability--renew-publication-lease (session)
  "Reserve SESSION's bounded lease window for serialized publication I/O."
  (condition-case nil
      (mevedel-session-durability--update-owned-lease
       session '(active publishing) 'publishing
       mevedel-session-publication-lease-seconds)
    (error nil)))

(defun mevedel-session-durability--finish-publication-lease (session)
  "Return SESSION's publishing lease to its ordinary renewable state."
  (condition-case nil
      (mevedel-session-durability--update-owned-lease
       session '(publishing) 'active mevedel-session-lease-seconds)
    (error nil)))

(defun mevedel-session-durability-call-with-reserved-lease
    (session function)
  "Call FUNCTION while SESSION owns a bounded synchronous mutation lease.

Timer renewal performs no target I/O while FUNCTION runs.  The final ownership
check fails closed.  This wrapper does not publish artifacts, commit a
manifest, or drain SESSION's publication queue."
  (let (entered failure result)
    (setf (mevedel-session-publication-active-p session) t)
    (unwind-protect
        (condition-case err
            (progn
              (unless (mevedel-session-durability--renew-publication-lease
                       session)
                (user-error
                 "Remote session lease could not reserve publication"))
              (setq entered t
                    result (funcall function)))
          (error (setq failure err)))
      (when (and entered
                 (not (mevedel-session-durability--finish-publication-lease
                       session))
                 (not failure))
        (setq failure
              '(user-error
                "Remote session lease was lost during publication")))
      (setf (mevedel-session-publication-active-p session) nil))
    (when (plist-get (mevedel-session-lease session) :release-pending)
      (condition-case err
          (mevedel-session-durability-lease-release
           (mevedel-session-save-path session) session)
        (error
         (unless failure
           (setq failure err)))))
    (when failure
      (signal (car failure) (cdr failure)))
    result))

(defun mevedel-session-durability--stage-artifacts (session artifacts)
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
           (mevedel-session-durability--artifact-for-session session artifact)
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

(defun mevedel-session-durability--batch-marker (batch)
  "Return BATCH's commit-marker artifact, or nil."
  (cl-find-if
   (lambda (artifact) (plist-get artifact :commit-marker))
   (plist-get batch :artifacts)))

(defun mevedel-session-durability--deduplicate-artifacts (artifacts)
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

(defun mevedel-session-durability--raw-artifacts (publication session-dir)
  "Return captured PUBLICATION artifacts relative to SESSION-DIR."
  (mapcar
   (lambda (entry)
     (let ((copy (copy-tree entry)))
       (setf (plist-get (cdr copy) :published)
             (file-relative-name
              (plist-get (cdr entry) :published) session-dir))
       copy))
   (plist-get publication :artifacts)))

(defun mevedel-session-durability--overlay-entry (entries entry)
  "Overlay publication ENTRY on ENTRIES with last-write-wins ordering."
  (append
   (cl-remove (car entry) entries :key #'car :test #'equal)
   (list entry)))

(defun mevedel-session-durability--write-immutable-artifact
    (directory artifact index session-dir)
  "Write ARTIFACT number INDEX below DIRECTORY and return its manifest entry."
  (let* ((source (plist-get artifact :source))
         (target (file-name-concat directory (format "%06d.data" index))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally source)
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point-min) (point-max) target nil 'silent nil 'excl)))
    (list (plist-get artifact :logical)
          :published (file-relative-name target session-dir)
          :sha256 (mevedel-session-durability--file-sha256 source))))

(defun mevedel-session-durability--commit-marker-publication
    (session batches marker)
  "Commit retained and current BATCHES for SESSION at sidecar MARKER."
  (let* ((session-dir
          (file-name-as-directory (mevedel-session-save-path session)))
         (previous
          (or (mevedel-session-publication session)
              (mevedel-session-durability-read-publication session-dir)))
         (entries
          (unless (plist-get marker :replace)
            (mevedel-session-durability--raw-artifacts
             previous session-dir)))
         (artifacts
          (mevedel-session-durability--deduplicate-artifacts
           (apply #'append
                  (mapcar
                   (lambda (batch) (plist-get batch :artifacts))
                   batches))))
         (root (file-name-concat session-dir ".publications"))
         directory)
    (make-directory root t)
    ;; ponytail: immutable generations grow until session-directory cleanup;
    ;; add read pins and garbage collection only if real storage proves costly.
    (setq directory
          (file-name-as-directory
           (make-nearby-temp-file
            (file-name-concat root "generation-") t)))
    (cl-loop
     for artifact in artifacts
     for index from 1
     do
     (setq entries
           (mevedel-session-durability--overlay-entry
            entries
            (mevedel-session-durability--write-immutable-artifact
             directory artifact index session-dir))))
    (let* ((manifest (list :sidecar "session.meta.el" :artifacts entries))
           (manifest-path (file-name-concat directory "manifest.el"))
           (head (file-relative-name manifest-path session-dir))
           (raw (list :head head
                      :sidecar "session.meta.el"
                      :artifacts entries))
           (captured
            (mevedel-session-durability--capture-publication
             session-dir raw)))
      (mevedel-session-durability--validate-manifest manifest manifest-path)
      (unless (mevedel-session-durability--create-plist manifest-path manifest)
        (error "Could not create session publication manifest: %s"
               manifest-path))
      (unless (mevedel-session-durability-commit-publication-head session head)
        (user-error "Remote session lease was lost before publication commit"))
      (setf (mevedel-session-publication session) captured)
      captured)))

(defun mevedel-session-durability--delete-batch (batch)
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

(defun mevedel-session-durability--delete-batches (batches)
  "Delete local recovery directories owned by BATCHES."
  (dolist (batch (delete-dups (copy-sequence batches)))
    (mevedel-session-durability--delete-batch batch)))

(defun mevedel-session-durability--clear-transient-publication (session)
  "Delete and clear SESSION's locally staged publication state."
  (let ((pending (mevedel-session-pending-publication session)))
    (mevedel-session-durability--delete-batches
     (append
      (plist-get pending :batches)
      (mevedel-session-publication-uncommitted-batches session)
      (mevedel-session-publication-queue session)))
    (setf (mevedel-session-pending-publication session) nil
          (mevedel-session-publication-uncommitted-batches session) nil
          (mevedel-session-publication-queue session) nil)))

(defun mevedel-session-durability-forget-removed-session (session)
  "Clear local durability state after SESSION's owned directory was removed.

The caller removes the target directory while its lease is still held.  This
function performs no target I/O and must run only after that removal."
  (mevedel-session-durability--cancel-renewal session)
  (setf (mevedel-session-lease session) nil
        (mevedel-session-publication session) nil
        (mevedel-session-publication-active-p session) nil)
  (mevedel-session-durability--clear-transient-publication session))

(defun mevedel-session-durability--batch-live-p (batch)
  "Return non-nil when BATCH still owns a staged source."
  (cl-some
   (lambda (artifact)
     (file-exists-p (plist-get artifact :source)))
   (plist-get batch :artifacts)))

(defun mevedel-session-durability--retain-uncommitted-batch (session batch)
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
      (mevedel-session-durability--delete-batch batch))))

(defun mevedel-session-durability-uncommitted-artifact (session logical)
  "Return SESSION's newest retained or queued local source for LOGICAL.

Return nil when no uncommitted write exists.  This operation never consults
the target's fixed cache or mutates the publication accumulator."
  (unless (mevedel-session-durability-logical-path-p logical)
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

(defun mevedel-session-durability--publish-artifact (artifact)
  "Publish staged ARTIFACT through a nearby target temporary file."
  (let* ((path (plist-get artifact :path))
         (source (plist-get artifact :source))
         (directory (file-name-directory (expand-file-name path)))
         (modes (and (file-exists-p path) (file-modes path))))
    (make-directory directory t)
    (let ((temporary
           (make-nearby-temp-file
            (expand-file-name ".mevedel-publication-" directory))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally source)
              (let ((coding-system-for-write 'no-conversion))
                (write-region (point-min) (point-max) temporary nil 'silent)))
            (when modes
              (set-file-modes temporary modes))
            (rename-file temporary path t))
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun mevedel-session-durability--publish-batch (session batch)
  "Publish every staged artifact in BATCH while SESSION remains owner."
  (dolist (artifact (plist-get batch :artifacts))
    (unless (mevedel-session-durability--renew-publication-lease session)
      (user-error "Remote session lease was lost during publication"))
    (mevedel-session-durability--publish-artifact artifact)
    (unless (mevedel-session-durability--renew-publication-lease session)
      (user-error "Remote session lease was lost during publication")))
  batch)

(defun mevedel-session-durability--record-pending (session batches err)
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

(defun mevedel-session-durability-record-specialized-failure
    (session reason recovery-path)
  "Block SESSION after a specialized transaction failed incompletely.

REASON describes the inconsistent state.  RECOVERY-PATH names target-side
recovery material that requires manual inspection before explicit abandon."
  (setf (mevedel-session-pending-publication session)
        (list :batches nil
              :failed-at (format-time-string "%FT%T%z")
              :reason reason
              :manual-recovery recovery-path))
  (mevedel-session-durability--refresh-session-buffers session)
  (display-warning
   'mevedel
   (format
    (concat "%s. Automatic retry is unavailable; inspect recovery data at "
            "%s, then explicitly abandon the block when repaired.")
    reason recovery-path)
   :error))

(defun mevedel-session-durability--refresh-session-buffers (session)
  "Refresh visible persistence status for every live buffer of SESSION."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'mevedel--session)
                 (eq mevedel--session session))
        (when (and buffer-file-name (file-exists-p buffer-file-name))
          (setq buffer-file-truename (file-truename buffer-file-name))
          (set-visited-file-modtime))
        (when (boundp 'mevedel-session--save-failed)
          (setq mevedel-session--save-failed nil))
        (force-mode-line-update)))))

(defun mevedel-session-durability--drain (session batches)
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
            (mevedel-session-durability--publish-batch session current)
            (if-let ((marker
                      (mevedel-session-durability--batch-marker current)))
                (let ((transaction
                       (append
                        (mevedel-session-publication-uncommitted-batches
                         session)
                        (list current))))
                  (setq result
                        (mevedel-session-durability--commit-marker-publication
                         session transaction marker)
                        committed t)
                  (setf
                   (mevedel-session-publication-uncommitted-batches session)
                   nil)
                  (mevedel-session-durability--delete-batches transaction))
              (mevedel-session-durability--retain-uncommitted-batch
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
         (mevedel-session-durability--record-pending session recovery err))
       (signal (car err) (cdr err))))))

(defun mevedel-session-durability--publish-critical-batches (session batches)
  "Publish BATCHES and discard recovery only after SESSION becomes active."
  (let ((publication-before (mevedel-session-publication session))
        outcome)
    (condition-case err
        (setq outcome
              (mevedel-session-durability-call-with-reserved-lease
               session
               (lambda ()
                 (setf (mevedel-session-pending-publication session) nil)
                 (mevedel-session-durability--drain session batches))))
      (error
       (unless (mevedel-session-pending-publication session)
         (let* ((committed-p
                 (not (equal publication-before
                             (mevedel-session-publication session))))
                (recovery
                 (cl-remove-if-not
                  #'mevedel-session-durability--batch-live-p
                  (delete-dups
                   (append
                    (mevedel-session-publication-uncommitted-batches session)
                    (unless committed-p batches)
                    (mevedel-session-publication-queue session))))))
           (setf (mevedel-session-publication-uncommitted-batches session)
                 nil
                 (mevedel-session-publication-queue session) nil)
           (when committed-p
             (mevedel-session-durability--delete-batches batches))
           (when recovery
             (mevedel-session-durability--record-pending
              session recovery err))))
       ;; A changed captured head proves the marker CAS committed.  Its local
       ;; sources are terminal even when the final lease normalization failed.
       (when (and (not (equal publication-before
                              (mevedel-session-publication session)))
                  (not (mevedel-session-pending-publication session)))
         (setf (mevedel-session-publication-uncommitted-batches session) nil))
       (signal (car err) (cdr err))))
    (plist-get outcome :result)))

(defun mevedel-session-durability-publish (session artifacts)
  "Serialize and publish SESSION's critical ARTIFACTS.

Each artifact is `(:path TARGET-PATH :content STRING [:coding CODING])'.  One
in-session `session.meta.el' may carry `:commit-marker t' and optional
`:replace t' to commit an immutable logical snapshot.  The complete batch is
staged locally before target I/O.  Pre-commit failure retains the staged batch
in SESSION and blocks later mutation."
  (when (mevedel-session-pending-publication session)
    (user-error "Session has pending publication; retry or abandon it first"))
  (let ((batch (mevedel-session-durability--stage-artifacts
                session artifacts))
        (publication-before (mevedel-session-publication session)))
    (condition-case err
        (progn
          (unless (or (mevedel-session-publication-active-p session)
                      (mevedel-session-durability-lease-renew session))
            (user-error
             "Remote session lease could not be renewed for publication"))
          (unless (mevedel-session-durability-lease-owned-p session)
            (user-error "Remote session mutation requires its live lease"))
          (if (mevedel-session-publication-active-p session)
              (progn
                (setf (mevedel-session-publication-queue session)
                      (append (mevedel-session-publication-queue session)
                              (list batch)))
                'queued)
            (mevedel-session-durability--publish-critical-batches
             session (list batch))))
      (error
       (unless (mevedel-session-pending-publication session)
         (if (equal publication-before
                    (mevedel-session-publication session))
             (mevedel-session-durability--record-pending
              session (list batch) err)
           (mevedel-session-durability--delete-batch batch)))
       (signal (car err) (cdr err))))))

(defun mevedel-session-durability-retry-publication (&optional session)
  "Retry SESSION's pending critical publication."
  (interactive)
  (setq session (or session
                    (mevedel-session-durability--current-session)))
  (let* ((pending (or (mevedel-session-pending-publication session)
                      (user-error "Session has no pending publication")))
         (batches (plist-get pending :batches)))
    (unless batches
      (user-error
       "Pending transaction requires manual recovery at %s"
       (or (plist-get pending :manual-recovery) "its retained target state")))
    (unless (mevedel-session-durability-lease-renew session)
      (user-error "Remote session lease could not be renewed for publication"))
    (unless (mevedel-session-durability-lease-owned-p session)
      (user-error "Remote session mutation requires its live lease"))
    (prog1
        (mevedel-session-durability--publish-critical-batches
         session batches)
      (mevedel-session-durability--refresh-session-buffers session))))

(defun mevedel-session-durability-abandon-publication (&optional session)
  "Explicitly discard SESSION's pending local recovery material."
  (interactive)
  (setq session (or session
                    (mevedel-session-durability--current-session)))
  (when-let ((pending (mevedel-session-pending-publication session)))
    (unless
        (yes-or-no-p
         (concat
          "Abandon pending session publication? The remote project effects "
          "remain, but transcript chronology and recovery state may be lost. "))
      (user-error "Pending publication was not abandoned"))
    (mevedel-session-durability--clear-transient-publication session)
    (mevedel-session-durability--refresh-session-buffers session)
    (display-warning
     'mevedel
     "Pending session publication was explicitly abandoned"
     :warning)
    t))

(defun mevedel-session-durability-discard-rolled-back-publication (session)
  "Discard SESSION recovery after its caller fully rolled back an operation.

The operation must have started without pending publication and restored all
pre-operation session and project state.  This function touches no target
state or publication head."
  (mevedel-session-durability--clear-transient-publication session)
  (mevedel-session-durability--refresh-session-buffers session)
  t)


;;;; Diagnostic publication

(defun mevedel-session-durability-append-diagnostic (session path content)
  "Atomically append diagnostic CONTENT to PATH for remote SESSION.

Return nil when a critical publication is active or pending so the caller can
retain CONTENT for a later retry.  Diagnostic failure never creates critical
pending publication; any local staging bytes are discarded before the error is
returned to the caller."
  (unless (and (stringp path) (stringp content))
    (error "Diagnostic publication requires string path and content"))
  (cond
   ((or (mevedel-session-pending-publication session)
        (mevedel-session-publication-active-p session))
    nil)
   ((not (mevedel-session-durability-lease-renew session))
    (error "Remote diagnostic publication could not renew the session lease"))
   ((not (mevedel-session-durability-lease-owned-p session))
    (error "Remote diagnostic publication requires the live session lease"))
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
                                    (with-temp-buffer
                                      (when (file-exists-p path)
                                        (insert-file-contents path))
                                      (insert content)
                                      (buffer-string))))
                               (setq batch
                                     (mevedel-session-durability--stage-artifacts
                                      session
                                      (list (list :path path
                                                  :content replacement))))
                               (mevedel-session-durability--publish-batch
                                session batch)
                               (setq published t))
                           (error
                            (setq diagnostic-error diagnostic-err)))
                         ;; A critical publisher may have queued while TRAMP
                         ;; handled the diagnostic I/O.  It retains precedence.
                         (when (mevedel-session-publication-queue session)
                           (mevedel-session-durability--drain session nil))
                         published)))
              (error
               (unless (mevedel-session-pending-publication session)
                 (let ((recovery
                        (cl-remove-if-not
                         #'mevedel-session-durability--batch-live-p
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
                     (mevedel-session-durability--record-pending
                      session recovery err))))
               (signal (car err) (cdr err))))
            (when diagnostic-error
              (signal (car diagnostic-error) (cdr diagnostic-error)))
            result)
        (when batch
          (mevedel-session-durability--delete-batch batch)))))))

(defun mevedel-session-durability-status (session)
  "Return SESSION's stable read-only durability status plist."
  (let ((lease (mevedel-session-lease session))
        (pending (mevedel-session-pending-publication session)))
    (list :lease-state (plist-get lease :state)
          :publication-head (plist-get lease :publication-head)
          :pending-publication (and pending t)
          :pending-reason (and pending (plist-get pending :reason))
          :authoritative-state-path
          (mevedel-workspace-state-dir
           (mevedel-session-workspace session)))))

(provide 'mevedel-session-durability)

;;; mevedel-session-durability.el ends here
