;;; mevedel-memory-write.el -- Durable memory write attempts -*- lexical-binding: t -*-

;;; Commentary:

;; Keep complete write intent in the workspace journal and a body-free pending
;; marker at the original memory root. Shared-root claims serialize independent
;; workspaces. An unresolved attempt blocks subsequent writes at that root even
;; after its owner expires. Decisions own settlement and marker retirement.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-apply)
(require 'mevedel-memory-store)

;; `mevedel-tool-patch'
(declare-function mevedel-tool-patch--assert-buffers-unmodified
                  "mevedel-tool-patch" (changes))
(declare-function mevedel-tool-patch--same-snapshot-p
                  "mevedel-tool-patch" (left right))
(declare-function mevedel-tool-patch-commit
                  "mevedel-tool-patch" (changes &optional currentp mutate))
(autoload 'mevedel-tool-patch--assert-buffers-unmodified "mevedel-tool-patch")
(autoload 'mevedel-tool-patch--same-snapshot-p "mevedel-tool-patch")
(autoload 'mevedel-tool-patch-commit "mevedel-tool-patch")

;; `mevedel-memory-notes'
(declare-function mevedel-memory-notes-commit "mevedel-memory-notes" (workspace root target changes currentp &optional before-write))
(declare-function mevedel-memory-notes-control-directory "mevedel-memory-notes" (directory))
(declare-function mevedel-memory-notes-observe "mevedel-memory-notes" (snapshot))
(declare-function mevedel-memory-notes-same-p "mevedel-memory-notes" (left right))
(autoload 'mevedel-memory-notes-commit "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-control-directory "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-observe "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-same-p "mevedel-memory-notes")

(defconst mevedel-memory-write-intent-file-regexp
  (concat "\\`" mevedel-journal-store-hash-regexp "\\.el\\'")
  "Exact private write-intent and decision record filename spelling.")

(defvar mevedel-memory-write--inspected (make-hash-table :test #'eq :weakness 'key)
  "Per-workspace table of inspected write identities and their markers.
Each value maps a write identity to its original-root marker path, or to
`retired' when its source pass was durably retired.  Intent records are
created once and never rewritten, so neither fact changes; whether the
marker exists is still observed afresh on every inspection.")

(defun mevedel-memory-write--scope (intent)
  "Return the captured memory scope of write INTENT's source pass."
  (plist-get (plist-get (plist-get intent :accepted) :prepared) :scope))

(defun mevedel-memory-write--directory (workspace)
  "Return WORKSPACE's private write-intent directory."
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "writes"))

(defun mevedel-memory-write-control-directory (directory)
  "Return shared write coordination for the original physical DIRECTORY.
Standard .mevedel/memory roots use their sibling state/memory-write.
The workspace's shared notes use the sibling state/shared-notes, outside the
folder they clean.  Other memory and instruction roots use
.mevedel/state/memory-write/root inside that root.  The location never
depends on the calling workspace."
  (let* ((directory (directory-file-name directory))
         (parent (file-name-directory directory)))
    (cond
     ((and (equal (file-name-nondirectory directory) "memory")
           (equal (file-name-nondirectory (directory-file-name parent)) ".mevedel"))
      (file-name-concat parent "state" "memory-write"))
     ((mevedel-memory-notes-control-directory directory))
     (t (file-name-concat directory ".mevedel" "state" "memory-write" "root")))))

(defun mevedel-memory-write--target (scope root)
  "Return the coordination directory at original ROOT in SCOPE."
  (mevedel-memory-write-control-directory
   (plist-get (mevedel-memory-scope--root scope root) :dir)))

(defun mevedel-memory-write--call (scope root function &optional intent)
  "Call FUNCTION with exclusive target ownership for original ROOT in SCOPE.
INTENT permits recovery of that exact unresolved write; any other pending marker
blocks entry, including attempts originating in another workspace."
  (let* ((directory (mevedel-memory-write--target scope root))
         (claim (or (mevedel-journal-claim-acquire (file-name-concat directory "claims") 180)
                    (error "Memory root is busy"))))
    (unwind-protect
        (progn
          (dolist (path (mevedel-session-control-fs-list-directory
                         (file-name-concat directory "pending")
                         (concat "\\`" mevedel-journal-store-hash-regexp "\\.pin\\'")))
            (unless (equal (file-name-nondirectory path) (and intent (concat intent ".pin")))
              (error "Memory root has an unresolved write")))
          (funcall function claim))
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defun mevedel-memory-write--after (before changes)
  "Derive complete after-state from BEFORE and prepared CHANGES.
A change carrying an exact :after snapshot supplies it directly."
  (mapcar
   (lambda (row)
     (let* ((original (cdr row))
            (change (seq-find (lambda (change) (equal (plist-get original :path) (plist-get change :path))) changes)))
       (cons (car row)
             (cond ((not change) original)
                   ((plist-member change :after) (plist-get change :after))
                   ((eq (plist-get change :action) 'delete)
                    (list :path (plist-get change :path) :exists nil))
                   (t (list :path (plist-get change :path) :exists t :bytes (plist-get change :bytes)
                            :hash (secure-hash 'sha256 (plist-get change :bytes)) :mode (plist-get change :mode)))))))
   before))

(defun mevedel-memory-write--notes-root (intent)
  "Return INTENT's captured shared-notes root, or nil for memory targets."
  (let ((root (cdr (assoc (plist-get (plist-get intent :item) :root)
                          (plist-get (mevedel-memory-write--scope intent) :roots)))))
    (when (eq (plist-get root :kind) 'notes) root)))

(defun mevedel-memory-write--observe (intent snapshot)
  "Observe SNAPSHOT's current path for INTENT's kind of target."
  (if (mevedel-memory-write--notes-root intent)
      (mevedel-memory-notes-observe snapshot)
    (mevedel-memory-scope--snapshot (plist-get snapshot :path) mevedel-memory-scope--max-file-bytes)))

(defun mevedel-memory-write--same-p (intent left right)
  "Return non-nil when LEFT and RIGHT match for INTENT's kind of target."
  (if (mevedel-memory-write--notes-root intent)
      (mevedel-memory-notes-same-p left right)
    (mevedel-tool-patch--same-snapshot-p left right)))

(defun mevedel-memory-write--commit (workspace target intent changes currentp &optional before-write)
  "Commit CHANGES for INTENT in WORKSPACE under TARGET, rechecking CURRENTP.
Memory roots use the shared patch transaction; shared notes use checked
permanent deletion.  BEFORE-WRITE guards forward writes only;
checked rollback retains CURRENTP's ownership without a cancelled forward gate."
  (if-let* ((root (mevedel-memory-write--notes-root intent)))
      (mevedel-memory-notes-commit workspace root target changes currentp before-write)
    (mevedel-tool-patch-commit
     changes currentp
     (lambda (before after)
       (mevedel-memory-write--mutate
        target before after
        ;; Patch rollback supplies the original before-state as AFTER.  Do not
        ;; block that checked restoration with a forward-admission gate.
        (and before-write
             (not (mevedel-tool-patch--same-snapshot-p
                   after (cdr (seq-find (lambda (row) (equal (plist-get after :path) (plist-get (cdr row) :path)))
                                        (plist-get intent :before)))))
             before-write))))))

(cl-defun mevedel-memory-write--read (workspace id hash &optional accepted observation coordinator evidence)
  "Read WORKSPACE write ID only when its bytes match expected HASH.
Validate its source pass and closed affected-file states.  Private scope remains
bound to the originating client; reading does not authorize target access.
Return nil for an intent whose source pass has been retired.
ACCEPTED may supply this operation's authenticated source bundle.  Callers
must discard it after mutation and never retain it between operations.
OBSERVATION may supply the intent's pinned read from this operation's batch.
COORDINATOR may supply this read's freshly authenticated decision claim.
When it matches the intent's original claim exactly, reuse its immutable
acceptance evidence together with ACCEPTED.  Recovered writes can instead
belong to an older coordinator; read their original evidence normally.
EVIDENCE may supply the pair of original claim/source outcome batch reads.
Neither proof authorizes a later mutation or survives this read observation."
  (unless (and (mevedel-journal-store-id-p id) (mevedel-journal-store-id-p hash)) (error "Invalid memory write identity"))
  (let* ((record (mevedel-memory-store--read-lisp
                  (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el")) observation))
         (text (cdr record)))
    (unless (equal hash (secure-hash 'sha256 text))
      (error "Memory write intent changed"))
    (let ((data (car record)))
      (when (and (proper-list-p data) (equal id (plist-get data :id))
                 (mevedel-journal-store-id-p (plist-get data :pass))
                 (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get data :pass)))
        (cl-return-from mevedel-memory-write--read nil))
      (let* ((claim (append (list :directory (mevedel-memory-store--claim-directory workspace)) (plist-get data :claim)))
             (accepted (or accepted (mevedel-memory-store-accepted workspace (plist-get data :pass) t)))
             (source (plist-get (plist-get accepted :prepared) :claim))
             (coordinator (and coordinator
                               (equal (plist-get coordinator :directory) (plist-get claim :directory))
                               (equal (mevedel-journal-claim--record coordinator)
                                      (mevedel-journal-claim--record claim))
                               coordinator))
             (evidence (unless coordinator
                         (or evidence (mevedel-session-control-fs-run-program
                          (list (list :op 'read :optional t :path (mevedel-journal-claim--path claim nil))
                                (list :op 'read :optional t :path (mevedel-journal-claim--path source t)))))))
             (proposal (seq-find (lambda (item) (equal (plist-get data :proposal) (plist-get item :id))) (plist-get accepted :proposals))))
	(unless (and (proper-list-p data) (= (length data) 20)
                     (cl-every (lambda (key) (plist-member data key))
                               '(:id :claim :target-claim :pass :proposal :source-hash :changes :before :after :reverse-of))
                     (equal id (plist-get data :id)) (equal id (plist-get claim :owner))
                     (or (null coordinator)
                         (equal (plist-get coordinator :directory) (plist-get claim :directory)))
                     (equal (mevedel-journal-claim--record claim)
                            (if coordinator (mevedel-journal-claim--record coordinator)
                              (mevedel-journal-claim--read (mevedel-journal-claim--path claim nil) nil (car evidence))))
                     (equal (plist-get data :pass) (plist-get (plist-get accepted :prepared) :id))
                     proposal
                     (if-let* ((source (plist-get data :reverse-of)))
			 (and (not (mevedel-memory-proposal-note-action-p (plist-get proposal :action)))
                              (proper-list-p source) (= (length source) 2) (not (equal id (car source)))
                              (let ((original (mevedel-memory-write--read workspace (car source) (cadr source) accepted)))
				(and (not (plist-get original :reverse-of))
                                     (equal (plist-get data :pass) (plist-get original :pass))
                                     (equal (plist-get data :proposal) (plist-get original :proposal))
                                     (equal (plist-get data :before) (plist-get original :after))
                                     (equal (plist-get data :after) (plist-get original :before)))))
                       (mevedel-memory-write--before-p proposal (plist-get data :before)))
                     (equal (plist-get data :source-hash)
                            (if coordinator (plist-get accepted :hash)
                              (plist-get (mevedel-journal-claim-outcome source (cadr evidence)) :payload)))
                     (proper-list-p (plist-get data :changes))
                     (cl-every (lambda (change)
				 (and (memq (plist-get change :action) '(write delete))
                                      (seq-some (lambda (row) (and (equal (plist-get change :path) (plist-get (cdr row) :path))
                                                                   (equal (plist-get change :before) (cdr row))))
						(plist-get data :before)))) (plist-get data :changes))
                     (equal (plist-get data :after) (mevedel-memory-write--after (plist-get data :before) (plist-get data :changes))))
          (error "Invalid memory write intent"))
	(append data (list :hash hash :accepted accepted :item proposal))))))

(defun mevedel-memory-write--before-p (proposal before)
  "Whether BEFORE preserves PROPOSAL's captured targets and non-index bytes.
The index may incorporate unrelated edits to other entries or prose. Its
complete bounded snapshot is retained in the independently hashed intent."
  (and (proper-list-p before)
       (equal (mapcar #'car before) (mapcar #'car (plist-get proposal :before)))
       (cl-every
        (lambda (row)
          (let ((original (cdr (assoc (car row) (plist-get proposal :before))))
                (snapshot (cdr row)))
            (or (equal snapshot original)
                (and (equal (car row) "MEMORY.md")
                     (equal (plist-get snapshot :path) (plist-get original :path))
                     (or (equal snapshot (list :path (plist-get original :path) :exists nil))
                         (and (eq (plist-get snapshot :exists) t)
                              (stringp (plist-get snapshot :bytes))
                              (<= (string-bytes (plist-get snapshot :bytes)) mevedel-memory-scope--max-file-bytes)
                              (equal (secure-hash 'sha256 (plist-get snapshot :bytes)) (plist-get snapshot :hash))
                              (integerp (plist-get snapshot :mode))))
                     (condition-case nil
                         (not (mevedel-memory-apply--index-conflicts original snapshot proposal))
                       (error nil)))))) before)))

(defun mevedel-memory-write--state (intent)
  "Classify INTENT's current target against its retained snapshots.
Return before, after, mixed, changed, or unavailable.  Every captured dependency
participates, including unchanged index files."
  (condition-case nil
      (let* ((scope (mevedel-memory-write--scope intent))
             (_ (mevedel-memory-scope--root scope (plist-get (plist-get intent :item) :root)))
             (before t) (after t) (known t))
        (dolist (row (plist-get intent :before))
          (when-let* ((buffer (find-buffer-visiting (plist-get (cdr row) :path)))
                      ((buffer-modified-p buffer)))
            (error "Memory target has an unsaved buffer edit"))
          (let* ((current (mevedel-memory-write--observe intent (cdr row)))
                 (old (mevedel-memory-write--same-p intent current (cdr row)))
                 (new (mevedel-memory-write--same-p intent current (cdr (assoc (car row) (plist-get intent :after))))))
            (setq before (and before old) after (and after new) known (and known (or old new)))))
        (cond (before 'before) (after 'after) (known 'mixed) (t 'changed)))
    (error 'unavailable)))

(defun mevedel-memory-write--pin (intent)
  "Return INTENT's original target marker path after checking root authority."
  (file-name-concat
   (mevedel-memory-write--target (mevedel-memory-write--scope intent)
                                (plist-get (plist-get intent :item) :root))
   "pending" (concat (plist-get intent :id) ".pin")))

(defun mevedel-memory-write--persist (workspace claim target accepted proposal before changes reverse-of)
  "Persist PROPOSAL intent from ACCEPTED under WORKSPACE CLAIM and TARGET.
Retain BEFORE, CHANGES, and optional REVERSE-OF source identity/hash.
The target marker is durable before any curated write; write control only.
ACCEPTED must be authenticated in the caller's current owned phase."
  (unless (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
    (error "Memory write needs workspace ownership"))
  (let* ((scope (plist-get (plist-get accepted :prepared) :scope))
         (id (plist-get claim :owner))
         (data (list :id id :claim (mevedel-journal-claim--record claim) :target-claim (mevedel-journal-claim--record target)
                     :pass (plist-get (plist-get accepted :prepared) :id) :proposal (plist-get proposal :id)
                     :source-hash (plist-get (mevedel-journal-claim-outcome (plist-get (plist-get accepted :prepared) :claim)) :payload)
                     :changes changes :before before :after (mevedel-memory-write--after before changes) :reverse-of reverse-of))
         (text (mevedel-memory-store--print data))
         (hash (secure-hash 'sha256 text))
         (path (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el")))
         (intent (append data (list :hash hash :accepted accepted
                                    :item (seq-find (lambda (item) (equal (plist-get proposal :id) (plist-get item :id)))
                                                    (plist-get accepted :proposals)))))
         (pin (mevedel-memory-write--pin intent)))
    (unless (and (equal (plist-get target :directory) (file-name-concat (mevedel-memory-write--target scope (plist-get proposal :root)) "claims"))
                 (eq (mevedel-memory-write--state intent) 'before))
      (error "Memory targets changed before preparation"))
    (mevedel-tool-patch--assert-buffers-unmodified changes)
    (mevedel-memory-store--assert-owned claim)
    (mevedel-memory-store--assert-owned target)
    (mevedel-session-control-fs-make-directory (file-name-directory path) t)
    (unless (mevedel-session-control-fs-create-file path text) (error "Memory write intent already exists"))
    (mevedel-session-control-fs-make-directory (file-name-directory pin) t)
    (unless (mevedel-session-control-fs-create-file pin hash) (error "Memory root marker already exists"))
    ;; These are the exact bytes just created under both owned claims, bound
    ;; to the bundle authenticated in this owned phase.  Running reverifies them.
    intent))

(defun mevedel-memory-write--prepare (workspace claim target accepted proposal &optional scope)
  "Retain intent for PROPOSAL from ACCEPTED under WORKSPACE CLAIM and TARGET.
Optional SCOPE and PROPOSAL may incorporate checked, unrelated index edits
supplied by the decision owner. No curated file is written."
  (mevedel-memory-write--persist
   workspace claim target accepted proposal (plist-get proposal :before)
   (mevedel-memory-apply-changes (or scope (plist-get (plist-get accepted :prepared) :scope)) proposal) nil))

(defun mevedel-memory-write--changes (before after)
  "Prepare exact changes between retained BEFORE and AFTER snapshots.
Only durable memory and instruction writes can be reversed."
  (let (changes)
    (dolist (row before (nreverse changes))
      (let* ((old (cdr row)) (new (cdr (assoc (car row) after))) (path (plist-get old :path)))
        (unless (and new (equal path (plist-get new :path))) (error "Memory snapshot targets differ"))
        (unless (and (mevedel-tool-patch--same-snapshot-p old new)
                     (equal (plist-get old :hash) (plist-get new :hash))
                     (equal (plist-get old :directory) (plist-get new :directory)))
          (push (append (list :path path :before old :action (if (plist-get new :exists) 'write 'delete)
                              :after new)
                        (when (and (plist-get new :exists) (plist-get new :bytes))
                          (list :bytes (plist-get new :bytes) :mode (plist-get new :mode)
                                :content (decode-coding-string (plist-get new :bytes) 'utf-8-unix)))) changes))))))

(defun mevedel-memory-write--prepare-reverse (workspace claim target intent)
  "Retain a checked reverse of applied INTENT under WORKSPACE CLAIM and TARGET.
The decision owner must verify an accepted application before calling this."
  (when (mevedel-memory-write--notes-root intent)
    (user-error "Shared-note deletion is permanent and cannot be reversed"))
  (unless (and (equal intent (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)))
               (not (plist-get intent :reverse-of)))
    (error "Expected an original memory application intent"))
  (mevedel-memory-write--persist
   workspace claim target (plist-get intent :accepted) (plist-get intent :item)
   (plist-get intent :after) (mevedel-memory-write--changes (plist-get intent :after) (plist-get intent :before))
   (list (plist-get intent :id) (plist-get intent :hash))))

(defun mevedel-memory-write--mutate (target before after &optional before-write)
  "Replace BEFORE with AFTER while fencing TARGET on its execution host.
Claim settlement and this whole check/write program lock the same directory.
A paused old mutation therefore finishes before takeover, or sees the settled
claim and writes nothing. The lock also protects checked rollback.
BEFORE-WRITE, when non-nil, rechecks editor-only forward-write admission."
  (let ((path (plist-get before :path))
        (directory (plist-get target :directory)))
    (unless (equal path (plist-get after :path))
      (error "Memory mutation paths differ"))
    (when (plist-get after :exists)
      (mevedel-session-control-fs-make-directory (file-name-directory path) t))
    (when before-write (funcall before-write))
    (dolist (result
             (mevedel-session-control-fs-run-program
              (append
               (list (list :op 'verify :path (mevedel-journal-claim--path target nil)
                           :content (json-serialize (mevedel-journal-claim--record target)))
                     (list :op 'absent :path (mevedel-journal-claim--path target t))
                     (list :op 'before-time :path directory
                           :content (number-to-string (plist-get target :expires-at))))
               (if (plist-get before :exists)
                   (list (list :op 'verify :path path :content (plist-get before :bytes) :coding 'no-conversion)
                         (list :op 'verify-mode :path path :content (format "%o" (plist-get before :mode))))
                 (list (list :op 'absent :path path)))
               (if (plist-get after :exists)
                   (list (list :op 'write-mode :path path
                               :content (concat (format "%o\n" (plist-get after :mode)) (plist-get after :bytes))
                               :coding 'no-conversion))
                 (list (list :op 'delete-file :path path))))
              directory))
      (mevedel-session-control-fs-program-value result))))

(defun mevedel-memory-write--run (workspace claim target intent &optional before-write)
  "Commit retained INTENT under WORKSPACE CLAIM and TARGET through ApplyPatch.
BEFORE-WRITE guards each forward mutation, but never disables checked rollback.
Reread INTENT's exact bytes; its bundle and coordinator claim belong to this
continuously owned phase.  Each target mutation fences TARGET under its lock."
  (unless (equal intent (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)
                                                    (plist-get intent :accepted) nil claim))
    (error "Memory write intent changed"))
  (unless (equal (plist-get intent :hash) (mevedel-session-control-fs-read-file (mevedel-memory-write--pin intent)))
    (error "Memory root marker changed"))
  (unless (and (equal (mevedel-journal-claim--record claim) (plist-get intent :claim))
               (equal (mevedel-journal-claim--record target) (plist-get intent :target-claim))
               (equal (plist-get target :directory)
                      (file-name-concat
                       (mevedel-memory-write--target (mevedel-memory-write--scope intent)
                                                     (plist-get (plist-get intent :item) :root)) "claims"))
               (eq (mevedel-memory-write--state intent) 'before))
    (error "Memory dependencies or write owner changed"))
  (mevedel-tool-patch--assert-buffers-unmodified (plist-get intent :changes))
  (mevedel-memory-write--commit
   workspace target intent (plist-get intent :changes)
   (lambda ()
     ;; The mutation program itself proves TARGET's claim and deadline.
     (mevedel-memory-store--assert-owned claim)
     (mevedel-memory-scope--root (mevedel-memory-write--scope intent)
                                 (plist-get (plist-get intent :item) :root))
     t)
   (lambda ()
     (when before-write (funcall before-write))
     ;; Include unchanged dependencies, not only paths that will be written.
     (mevedel-tool-patch--assert-buffers-unmodified
      (mapcar (lambda (row) (list :path (plist-get (cdr row) :path)))
              (plist-get intent :before))))))

(defun mevedel-memory-write--rollback (workspace claim target intent)
  "Restore a known pending INTENT under current WORKSPACE CLAIM and TARGET.
Every dependency must match retained before or after bytes.  An intervening
edit prevents the entire rollback.  Commit reverse changes through ApplyPatch;
interruption leaves the original intent sufficient for another checked recovery."
  (when (mevedel-memory-write--notes-root intent)
    (user-error "Shared-note deletion is permanent and cannot be rolled back"))
  (unless (equal intent (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)))
    (error "Memory write intent changed"))
  (let* ((scope (mevedel-memory-write--scope intent))
         (root (plist-get (plist-get intent :item) :root))
         (currentp (lambda ()
                     (mevedel-memory-store--assert-owned claim)
                     (mevedel-memory-store--assert-owned target)
                     (mevedel-memory-scope--root scope root)
                     t)))
    (unless (and (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
                 (equal (plist-get target :directory) (file-name-concat (mevedel-memory-write--target scope root) "claims"))
                 (equal (plist-get intent :hash) (mevedel-session-control-fs-read-file (mevedel-memory-write--pin intent))))
      (error "Memory rollback ownership or marker changed"))
    (funcall currentp)
    (unless (memq (mevedel-memory-write--state intent) '(before after mixed))
      (error "Memory rollback has intervening or unavailable edits"))
    ;; Restore only the rows the interrupted write reached; a row at neither
    ;; retained state is an intervening edit and blocks the whole rollback.
    (let (reached)
      (dolist (row (plist-get intent :before))
        (let* ((before (cdr row))
               (after (cdr (assoc (car row) (plist-get intent :after))))
               (current (mevedel-memory-write--observe intent before)))
          (unless (mevedel-memory-write--same-p intent current before)
            (unless (mevedel-memory-write--same-p intent current after)
              (error "Memory rollback target changed"))
            (push (cons (car row) after) reached))))
      (mevedel-memory-write--commit
       workspace target intent
       (mevedel-memory-write--changes (nreverse reached) (plist-get intent :before)) currentp))))

(defun mevedel-memory-write--retire (target intent)
  "Retire INTENT's marker under TARGET after a durable resolved decision.
The caller must have accepted the decision before releasing this write fence."
  (let ((pin (mevedel-memory-write--pin intent)))
    (unless (equal (plist-get target :directory)
                   (file-name-concat (file-name-directory (directory-file-name (file-name-directory pin))) "claims"))
      (error "Memory root ownership does not match its write marker"))
    (when (mevedel-session-control-fs-path-exists-p pin)
      ;; One target-locked program proves ownership, deadline and marker bytes.
      (dolist (result
               (mevedel-session-control-fs-run-program
                (list (list :op 'verify :path (mevedel-journal-claim--path target nil)
                            :content (json-serialize (mevedel-journal-claim--record target)))
                      (list :op 'absent :path (mevedel-journal-claim--path target t))
                      (list :op 'before-time :path (plist-get target :directory)
                            :content (number-to-string (plist-get target :expires-at)))
                      (list :op 'verify :path pin :content (plist-get intent :hash))
                      (list :op 'delete-file :path pin))
                (plist-get target :directory)))
        (mevedel-session-control-fs-program-value result)))))

(defun mevedel-memory-write--abandon (workspace claim target intent)
  "Abandon an unchanged held INTENT under its original WORKSPACE CLAIM and TARGET.
Authenticate the exact intent and both still-owned tokens, prove full before
state, then elect and freshly authenticate an empty cancelled coordinator
outcome.  While TARGET remains owned, prove before state and marker again in
one target-locked program that only deletes the marker.  The immutable unmarked
intent remains for inspection, but cannot attribute later external edits.
Errors or quit leave retirement unresolved, never permission to retry writes."
  (unless (and (equal intent (mevedel-memory-write--read workspace (plist-get intent :id) (plist-get intent :hash)))
               (not (plist-get intent :reverse-of))
               (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
               (equal (mevedel-journal-claim--record claim) (plist-get intent :claim))
               (equal (mevedel-journal-claim--record target) (plist-get intent :target-claim))
               (equal (plist-get target :directory)
                      (file-name-concat (mevedel-memory-write--target (mevedel-memory-write--scope intent)
                                                                      (plist-get (plist-get intent :item) :root)) "claims")))
    (error "Held memory intent or original ownership changed"))
  (mevedel-memory-store--assert-owned claim)
  (mevedel-memory-store--assert-owned target)
  (unless (eq (mevedel-memory-write--state intent) 'before)
    (error "Held memory intent is not fully unchanged"))
  (mevedel-tool-patch--assert-buffers-unmodified
   (mapcar (lambda (row) (list :path (plist-get (cdr row) :path))) (plist-get intent :before)))
  (mevedel-journal-claim-settle claim 'cancelled "")
  ;; The coordinator and target may be on different hosts.  Authenticate this
  ;; immutable outcome on its original host, not through the target program.
  (let ((outcome (mevedel-journal-claim-outcome claim)))
    (unless (and (eq (plist-get outcome :status) 'cancelled)
                 (equal (plist-get outcome :payload) ""))
      (error "Held memory intent lacks its exact cancellation fence")))
  (let ((pin (mevedel-memory-write--pin intent)) checks)
    (mevedel-memory-store--assert-owned target)
    (dolist (row (plist-get intent :before))
      (let* ((before (cdr row)) (path (plist-get before :path)))
        (setq checks
              (append checks
                      (cond
                       ((not (plist-get before :exists)) (list (list :op 'absent :path path)))
                       ((plist-get before :directory)
                        (let ((paths (mevedel-session-control-fs-list-directory path ".*")))
                          (unless (eq (null paths) (and (plist-get before :empty) t))
                            (error "Held memory directory changed"))
                          (list (list :op 'directory-p :path path)
                                (mevedel-session-control-fs-listing-proof path paths))))
                       (t (list (list :op 'verify-hash :path path :content (plist-get before :hash))
                                (list :op 'verify-mode :path path :content (format "%o" (plist-get before :mode))))))))))
    (mevedel-tool-patch--assert-buffers-unmodified
     (mapcar (lambda (row) (list :path (plist-get (cdr row) :path))) (plist-get intent :before)))
    (dolist (result
             (mevedel-session-control-fs-run-program
              (append
               (list (list :op 'verify :path (mevedel-journal-claim--path target nil)
                           :content (json-serialize (mevedel-journal-claim--record target)))
                     (list :op 'absent :path (mevedel-journal-claim--path target t))
                     (list :op 'before-time :path (plist-get target :directory)
                           :content (number-to-string (plist-get target :expires-at))))
               checks
               (list (list :op 'verify :path pin :content (plist-get intent :hash))
                     (list :op 'delete-file :path pin)))
              (plist-get target :directory)))
      (mevedel-session-control-fs-program-value result))))

(defun mevedel-memory-write--read-batch (workspace ids)
  "Return bounded pinned read observations for WORKSPACE write IDS.
A transport failure makes each identity unavailable without aborting later
batches.  Reject incomplete control responses rather than losing identities."
  (let ((operations
         (mapcar (lambda (id)
                   (list :op 'read :optional t :coding 'utf-8-unix
                         :max-bytes (1+ mevedel-memory-store--max-bytes)
                         :path (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el")))) ids)))
    (condition-case err
        (let ((results (mevedel-session-control-fs-run-program operations)))
          (unless (and (proper-list-p results) (= (length results) (length ids)))
            (error "Incomplete memory write observations"))
          results)
      (error (mapcar (lambda (op)
                       (list :path (plist-get op :path) :status 'failed
                             :diagnostic (error-message-string err))) operations)))))

(defun mevedel-memory-write--groups (workspace ids)
  "Group WORKSPACE write IDS by untrusted pass hint using bounded reads.
The nil key holds unavailable rows.  No bundle or intent body survives here."
  (let ((groups (make-hash-table :test #'equal)))
    (while ids
      (let* ((batch (seq-take ids 8))
             (reads (mevedel-memory-write--read-batch workspace batch)))
        (setq ids (nthcdr (length batch) ids))
        (cl-mapc
         (lambda (id read)
           (condition-case err
               (let ((pass (plist-get (car (mevedel-memory-store--read-lisp (plist-get read :path) read)) :pass)))
                 (unless (mevedel-journal-store-id-p pass) (error "Invalid memory write source pass"))
                 (push id (gethash pass groups)))
             (error (push (list :id id :error (error-message-string err)) (gethash nil groups)))))
         batch reads)))
    groups))

(cl-defun mevedel-memory-write--list (workspace &optional ids (bundle nil supplied) coordinators)
  "Inspect retained write records in WORKSPACE without mutating targets.
Each row has :id, and readable rows have :hash and :intent.  Check original root
authority before reporting :marked.  Preserve unreadable or unavailable records
with :error instead of dropping them or rebinding their target.  Private intent
bodies require a separate original-root authority check before presentation.
IDS restricts inspection to those write identities, still reading each afresh.
Remember each readable identity's marker path for
`mevedel-memory-write--recovery-candidates'.
BUNDLE, when supplied, is this read-only scan's authenticated single-pass proof;
IDS must then belong to that pass.  Discard all returned proofs after the scan.
COORDINATORS may map write IDs to this scan's authenticated decision claims."
  (let ((inspected (or (gethash workspace mevedel-memory-write--inspected)
                       (puthash workspace (make-hash-table :test #'equal)
                                mevedel-memory-write--inspected)))
        (ids (or ids
                 (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                         (mevedel-session-control-fs-list-directory
                          (mevedel-memory-write--directory workspace) mevedel-memory-write-intent-file-regexp))))
        (rows (make-hash-table :test #'equal)))
    ;; Group only identities, not complete bodies, by an untrusted pass hint.
    ;; Each group is reread in bounded batches and fully authenticated below.
    (dolist (id ids)
      (unless (mevedel-journal-store-id-p id) (error "Invalid memory write identity")))
    (let ((groups (if supplied
                      (let ((table (make-hash-table :test #'equal)))
                        (puthash (plist-get (plist-get bundle :prepared) :id) ids table) table)
                    (mevedel-memory-write--groups workspace ids))))
      (dolist (row (gethash nil groups)) (puthash (plist-get row :id) row rows))
      (remhash nil groups)
      (maphash
     (lambda (pass pending)
       (let (accepted failure)
         (condition-case err
             (unless (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) pass)
               (setq accepted (if supplied bundle (mevedel-memory-store-accepted workspace pass))))
           (error (setq failure (error-message-string err))))
         (while pending
           (let* ((batch (seq-take pending 8))
                  (reads (mevedel-memory-write--read-batch workspace batch))
                  (evidence
                   (condition-case nil
                     (when (and accepted
                              (not (and coordinators (cl-every (lambda (id) (gethash id coordinators)) batch))))
                     (mevedel-session-control-fs-run-program
                      (cl-mapcan
                       (lambda (read)
                         (let* ((data (car (mevedel-memory-store--read-lisp (plist-get read :path) read)))
                                (claim (append (list :directory (mevedel-memory-store--claim-directory workspace))
                                               (plist-get data :claim))))
                           (list (list :op 'read :optional t :path (mevedel-journal-claim--path claim nil))
                                 (list :op 'read :optional t :path (mevedel-journal-claim--path
                                                                (plist-get (plist-get accepted :prepared) :claim) t))))) reads)))
                     ;; Fall back to each row's ordinary evidence reader.
                     (error nil)))
                  pins)
             (setq pending (nthcdr (length batch) pending))
             (cl-mapc
              (lambda (id read)
                (let ((row (list :id id)) (proof (list (pop evidence) (pop evidence))))
                  (condition-case err
                      (progn
                        (when failure (error "%s" failure))
                        (let* ((hash (secure-hash 'sha256 (mevedel-session-control-fs-program-value read)))
                               (intent (mevedel-memory-write--read workspace id hash accepted read
                                                                  (and coordinators (gethash id coordinators)) proof)))
                          (if (not intent)
                              (progn (puthash id 'retired inspected) (setq row nil))
                            (setq row (append row (list :hash hash :intent intent)))
                            (let ((pin (mevedel-memory-write--pin intent)))
                              (puthash id pin inspected)
                              (push (cons row pin) pins)))))
                    (error (setq row (append row (list :error (error-message-string err))))))
                  (when row (puthash id row rows))))
              batch reads)
             ;; Authenticate original authority before inspecting target markers.
             (let (targets)
               (dolist (pin pins) (push pin (alist-get (file-remote-p (cdr pin)) targets nil nil #'equal)))
               (dolist (target targets)
                 (let* ((pins (cdr target))
                        (markers (when pins
                                   (condition-case err
                                       (mevedel-session-control-fs-run-program
                                        (mapcar (lambda (pin) (list :op 'read :path (cdr pin) :optional t :max-bytes 65)) pins))
                                     (error (mapcar (lambda (pin) (list :status 'failed :path (cdr pin)
                                                                      :diagnostic (error-message-string err))) pins))))))
               (cl-mapc (lambda (pin read)
                          (condition-case err
                              (unless (eq (plist-get read :status) 'absent)
                                (unless (equal (plist-get (car pin) :hash) (mevedel-session-control-fs-program-value read))
                                  (error "Memory root marker changed"))
                                (setf (plist-get (car pin) :marked) t))
                            (error (setf (plist-get (car pin) :error) (error-message-string err))))
                          (puthash (plist-get (car pin) :id) (car pin) rows)) pins markers))))))))
       groups))
    (delq nil (mapcar (lambda (id) (gethash id rows)) ids))))

(defun mevedel-memory-write--recovery-selection (workspace)
  "Discover marked or unavailable WORKSPACE writes, returning identities only.
The directory listing is the discovery cutoff; later arrivals wait for another
recovery.  Inspect one pass at a time using the reader's proof sharing, dropping
all bodies before the next pass.  This read-only observation grants no authority
to reconcile a target after yielding.  Fail rather than truncate a large reply."
  (let* ((ids (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                      (mevedel-session-control-fs-list-directory
                       (mevedel-memory-write--directory workspace) mevedel-memory-write-intent-file-regexp)))
         (groups (mevedel-memory-write--groups workspace ids))
         (selected (make-hash-table :test #'equal)))
    (dolist (row (gethash nil groups)) (puthash (plist-get row :id) t selected))
    (remhash nil groups)
    (maphash
     (lambda (_pass pending)
       (dolist (row (mevedel-memory-write--list workspace pending))
         (when (or (plist-get row :marked) (plist-get row :error))
           (puthash (plist-get row :id) t selected)))) groups)
    (when (> (hash-table-count selected) 65536)
      (error "Too many memory recovery candidates"))
    (seq-filter (lambda (id) (gethash id selected)) ids)))

(defun mevedel-memory-write--recovery-candidates (workspace)
  "Return WORKSPACE write identities whose recovery needs a full inspection.
An identity `mevedel-memory-write--list' already read is a candidate only while
its marker exists, observed in batches of eight per target.
A retired identity is never a candidate.
Every identity not yet read successfully remains a candidate, so unreadable
records keep reporting their error.  Order follows the directory listing."
  (let* ((ids (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                      (mevedel-session-control-fs-list-directory
                       (mevedel-memory-write--directory workspace)
                       mevedel-memory-write-intent-file-regexp)))
         (inspected (gethash workspace mevedel-memory-write--inspected))
         (marked (make-hash-table :test #'equal))
         groups)
    (when inspected
      ;; Forget identities whose records collection removed.  Records are
      ;; retained for reversal, so avoid a quadratic list scan here.
      (let ((present (make-hash-table :test #'equal :size (length ids)))
            gone)
        (dolist (id ids) (puthash id t present))
        (maphash (lambda (id _) (unless (gethash id present) (push id gone))) inspected)
        (dolist (id gone) (remhash id inspected))))
    (dolist (id ids)
      (let ((pin (and inspected (gethash id inspected))))
        (cond ((null pin) (puthash id t marked))
              ((stringp pin)
               (push (cons id pin) (alist-get (file-remote-p pin) groups nil nil #'equal))))))
    (dolist (group groups)
      (let ((pending (cdr group)))
        (while pending
          (let ((entries (seq-take pending 8)))
            (setq pending (nthcdr (length entries) pending))
            (condition-case nil
                (let ((exists (mevedel-session-control-fs-paths-exist (mapcar #'cdr entries))))
                  (dolist (entry entries)
                    (when (pop exists) (puthash (car entry) t marked))))
              ;; An unobservable target leaves its writes to the full inspection,
              ;; which reports why it is unavailable.
              (error (dolist (entry entries) (puthash (car entry) t marked))))))))
    (seq-filter (lambda (id) (gethash id marked)) ids)))

(defun mevedel-memory-write--marked (workspace scope root &optional proposal)
  "Return marked write intents at original ROOT in SCOPE for WORKSPACE.
List only that target's pending markers, then authenticate each marked intent
against its exact marker hash.  PROPOSAL, when non-nil, keeps only its intents.
Another workspace's markers are ignored; an unreadable marked intent of
WORKSPACE signals rather than being dropped."
  (let ((directory (file-name-concat (mevedel-memory-write--target scope root) "pending"))
        marked)
    (dolist (path (mevedel-session-control-fs-list-directory
                   directory (concat "\\`" mevedel-journal-store-hash-regexp "\\.pin\\'"))
                  (nreverse marked))
      (let* ((id (file-name-base path))
             (hash (mevedel-session-control-fs-read-file path 'utf-8-unix 65))
             ;; Another workspace's marker at a shared root has no intent here.
             (intent (condition-case err (mevedel-memory-write--read workspace id hash)
                       (mevedel-session-control-fs-absent nil)
                       (error (error "Memory write is unavailable: %s" (error-message-string err))))))
        (when (and intent (or (null proposal) (equal proposal (plist-get intent :proposal))))
          (push intent marked))))))

(defun mevedel-memory-write--marked-p (workspace scope root proposal)
  "Return non-nil when PROPOSAL has a marked write at ROOT in SCOPE for WORKSPACE."
  (and (mevedel-memory-write--marked workspace scope root proposal) t))

(provide 'mevedel-memory-write)
;;; mevedel-memory-write.el ends here
