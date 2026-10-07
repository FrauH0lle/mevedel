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

(defconst mevedel-memory-write-intent-file-regexp
  (concat "\\`" mevedel-journal-store-hash-regexp "\\.el\\'")
  "Exact private write-intent and decision record filename spelling.")

(defun mevedel-memory-write-scope (intent)
  "Return the captured memory scope of write INTENT's source pass."
  (plist-get (plist-get (plist-get intent :accepted) :prepared) :scope))

(defun mevedel-memory-write--directory (workspace)
  "Return WORKSPACE's private write-intent directory."
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "writes"))

(defun mevedel-memory-write-control-directory (directory)
  "Return shared write coordination for the original physical DIRECTORY.
Standard .mevedel/memory roots use their sibling state/memory-write.
Other memory and instruction roots use .mevedel/state/memory-write/root
inside that root.  The location never depends on the calling workspace."
  (let* ((directory (directory-file-name directory))
         (parent (file-name-directory directory)))
    (if (and (equal (file-name-nondirectory directory) "memory")
             (equal (file-name-nondirectory (directory-file-name parent)) ".mevedel"))
        (file-name-concat parent "state" "memory-write")
      (file-name-concat directory ".mevedel" "state" "memory-write" "root"))))

(defun mevedel-memory-write--target (scope root)
  "Return the coordination directory at original ROOT in SCOPE."
  (mevedel-memory-write-control-directory
   (plist-get (mevedel-memory-scope--root scope root) :dir)))

(defun mevedel-memory-write-call (scope root function &optional intent)
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
  "Derive complete after-state from BEFORE and prepared CHANGES."
  (mapcar
   (lambda (row)
     (let* ((original (cdr row))
            (change (seq-find (lambda (change) (equal (plist-get original :path) (plist-get change :path))) changes)))
       (cons (car row)
             (if (not change) original
               (if (eq (plist-get change :action) 'delete)
                   (list :path (plist-get change :path) :exists nil)
                 (list :path (plist-get change :path) :exists t :bytes (plist-get change :bytes)
                       :hash (secure-hash 'sha256 (plist-get change :bytes)) :mode (plist-get change :mode))))))) before))

(cl-defun mevedel-memory-write-read (workspace id hash)
  "Read WORKSPACE write ID only when its bytes match expected HASH.
Validate its source pass and closed affected-file states. Private scope remains
bound to the originating client; reading does not authorize target access.
Return nil for an intent whose source pass has been retired."
  (unless (and (mevedel-journal-store-id-p id) (mevedel-journal-store-id-p hash)) (error "Invalid memory write identity"))
  (let* ((record (mevedel-memory-store--read-lisp
                  (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el"))))
         (text (cdr record)))
    (unless (equal hash (secure-hash 'sha256 text))
      (error "Memory write intent changed"))
    (let ((data (car record)))
      (when (and (proper-list-p data) (equal id (plist-get data :id))
                 (mevedel-journal-store-id-p (plist-get data :pass))
                 (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get data :pass)))
        (cl-return-from mevedel-memory-write-read nil))
      (let* ((claim (append (list :directory (mevedel-memory-store--claim-directory workspace)) (plist-get data :claim)))
             (accepted (mevedel-memory-store-accepted workspace (plist-get data :pass)))
             (proposal (seq-find (lambda (item) (equal (plist-get data :proposal) (plist-get item :id))) (plist-get accepted :proposals))))
	(unless (and (proper-list-p data) (= (length data) 20)
                     (cl-every (lambda (key) (plist-member data key))
                               '(:id :claim :target-claim :pass :proposal :source-hash :changes :before :after :reverse-of))
                     (equal id (plist-get data :id)) (equal id (plist-get claim :owner))
                     (equal (mevedel-journal-claim--record claim) (mevedel-journal-claim--read (mevedel-journal-claim--path claim nil) nil))
                     proposal
                     (if-let* ((source (plist-get data :reverse-of)))
			 (and (proper-list-p source) (= (length source) 2) (not (equal id (car source)))
                              (let ((original (mevedel-memory-write-read workspace (car source) (cadr source))))
				(and (not (plist-get original :reverse-of))
                                     (equal (plist-get data :pass) (plist-get original :pass))
                                     (equal (plist-get data :proposal) (plist-get original :proposal))
                                     (equal (plist-get data :before) (plist-get original :after))
                                     (equal (plist-get data :after) (plist-get original :before)))))
                       (mevedel-memory-write--before-p proposal (plist-get data :before)))
                     (equal (plist-get data :source-hash)
                            (plist-get (mevedel-journal-claim-outcome (plist-get (plist-get accepted :prepared) :claim)) :payload))
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

(defun mevedel-memory-write-state (intent)
  "Classify INTENT's current target against its retained snapshots.
Return before, after, mixed, changed, or unavailable. Every captured dependency
participates, including unchanged index files."
  (condition-case nil
      (let* ((scope (mevedel-memory-write-scope intent))
             (_ (mevedel-memory-scope--root scope (plist-get (plist-get intent :item) :root)))
             (before t) (after t) (known t))
        (dolist (row (plist-get intent :before))
          (when-let* ((buffer (find-buffer-visiting (plist-get (cdr row) :path)))
                      ((buffer-modified-p buffer)))
            (error "Memory target has an unsaved buffer edit"))
          (let* ((current (mevedel-memory-scope--snapshot (plist-get (cdr row) :path) mevedel-memory-scope--max-file-bytes))
                 (old (mevedel-tool-patch--same-snapshot-p current (cdr row)))
                 (new (mevedel-tool-patch--same-snapshot-p current (cdr (assoc (car row) (plist-get intent :after))))))
            (setq before (and before old) after (and after new) known (and known (or old new)))))
        (cond (before 'before) (after 'after) (known 'mixed) (t 'changed)))
    (error 'unavailable)))

(defun mevedel-memory-write--pin (intent)
  "Return INTENT's original target marker path after checking root authority."
  (file-name-concat
   (mevedel-memory-write--target (mevedel-memory-write-scope intent)
                                (plist-get (plist-get intent :item) :root))
   "pending" (concat (plist-get intent :id) ".pin")))

(defun mevedel-memory-write--prepare (workspace claim target accepted proposal before changes reverse-of)
  "Persist PROPOSAL intent from ACCEPTED under WORKSPACE CLAIM and TARGET.
Retain BEFORE, CHANGES, and optional REVERSE-OF source identity/hash. The target
marker is durable before any curated write; this function writes control only."
  (mevedel-memory-store--assert-owned claim)
  (mevedel-memory-store--assert-owned target)
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
         (intent (append data (list :hash hash :accepted accepted :item proposal)))
         (pin (mevedel-memory-write--pin intent)))
    (unless (and (equal (plist-get target :directory) (file-name-concat (mevedel-memory-write--target scope (plist-get proposal :root)) "claims"))
                 (eq (mevedel-memory-write-state intent) 'before))
      (error "Memory targets changed before preparation"))
    (mevedel-tool-patch--assert-buffers-unmodified changes)
    (mevedel-memory-store--assert-owned claim)
    (mevedel-memory-store--assert-owned target)
    (mevedel-session-control-fs-make-directory (file-name-directory path) t)
    (unless (mevedel-session-control-fs-create-file path text) (error "Memory write intent already exists"))
    (mevedel-session-control-fs-make-directory (file-name-directory pin) t)
    (unless (mevedel-session-control-fs-create-file pin hash) (error "Memory root marker already exists"))
    (mevedel-memory-write-read workspace id hash)))

(defun mevedel-memory-write-prepare (workspace claim target accepted proposal &optional scope)
  "Retain intent for PROPOSAL from ACCEPTED under WORKSPACE CLAIM and TARGET.
Optional SCOPE and PROPOSAL may incorporate checked, unrelated index edits
supplied by the decision owner. No curated file is written."
  (mevedel-memory-write--prepare
   workspace claim target accepted proposal (plist-get proposal :before)
   (mevedel-memory-apply-changes (or scope (plist-get (plist-get accepted :prepared) :scope)) proposal) nil))

(defun mevedel-memory-write--changes (before after)
  "Prepare exact patch changes between retained BEFORE and AFTER snapshots."
  (let (changes)
    (dolist (row before (nreverse changes))
      (let* ((old (cdr row)) (new (cdr (assoc (car row) after))) (path (plist-get old :path)))
        (unless (and new (equal path (plist-get new :path))) (error "Memory snapshot targets differ"))
        (unless (mevedel-tool-patch--same-snapshot-p old new)
          (push (append (list :path path :before old :action (if (plist-get new :exists) 'write 'delete))
                        (when (plist-get new :exists)
                          (list :bytes (plist-get new :bytes) :mode (plist-get new :mode)
                                :content (decode-coding-string (plist-get new :bytes) 'utf-8-unix)))) changes))))))

(defun mevedel-memory-write-prepare-reverse (workspace claim target intent)
  "Retain a checked reverse of applied INTENT under WORKSPACE CLAIM and TARGET.
The decision owner must verify an accepted application before calling this."
  (unless (and (equal intent (mevedel-memory-write-read workspace (plist-get intent :id) (plist-get intent :hash)))
               (not (plist-get intent :reverse-of)))
    (error "Expected an original memory application intent"))
  (mevedel-memory-write--prepare
   workspace claim target (plist-get intent :accepted) (plist-get intent :item)
   (plist-get intent :after) (mevedel-memory-write--changes (plist-get intent :after) (plist-get intent :before))
   (list (plist-get intent :id) (plist-get intent :hash))))

(defun mevedel-memory-write--mutate (target before after)
  "Replace BEFORE with AFTER while fencing TARGET on its execution host.
Claim settlement and this whole check/write program lock the same directory.
A paused old mutation therefore finishes before takeover, or sees the settled
claim and writes nothing. The lock also protects checked rollback."
  (let ((path (plist-get before :path))
        (directory (plist-get target :directory)))
    (unless (equal path (plist-get after :path))
      (error "Memory mutation paths differ"))
    (when (plist-get after :exists)
      (mevedel-session-control-fs-make-directory (file-name-directory path) t))
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

(defun mevedel-memory-write-run (workspace claim target intent)
  "Commit retained INTENT under WORKSPACE CLAIM and TARGET through ApplyPatch."
  (unless (equal intent (mevedel-memory-write-read workspace (plist-get intent :id) (plist-get intent :hash)))
    (error "Memory write intent changed"))
  (unless (equal (plist-get intent :hash) (mevedel-session-control-fs-read-file (mevedel-memory-write--pin intent)))
    (error "Memory root marker changed"))
  (unless (and (equal (mevedel-journal-claim--record claim) (plist-get intent :claim))
               (equal (mevedel-journal-claim--record target) (plist-get intent :target-claim))
               (equal (plist-get target :directory)
                      (file-name-concat
                       (mevedel-memory-write--target (mevedel-memory-write-scope intent)
                                                    (plist-get (plist-get intent :item) :root)) "claims"))
               (eq (mevedel-memory-write-state intent) 'before))
    (error "Memory dependencies or write owner changed"))
  (mevedel-tool-patch--assert-buffers-unmodified (plist-get intent :changes))
  (mevedel-tool-patch-commit
   (plist-get intent :changes)
   (lambda ()
     (mevedel-memory-store--assert-owned claim)
     (mevedel-memory-store--assert-owned target)
     (mevedel-memory-scope--root (mevedel-memory-write-scope intent)
                                (plist-get (plist-get intent :item) :root))
     t)
   (lambda (before after) (mevedel-memory-write--mutate target before after))))

(defun mevedel-memory-write-rollback (workspace claim target intent)
  "Restore a known pending INTENT under current WORKSPACE CLAIM and TARGET.
Every dependency must match retained before or after bytes. An intervening
edit prevents the entire rollback. Commit reverse changes through ApplyPatch;
interruption leaves the original intent sufficient for another checked recovery."
  (unless (equal intent (mevedel-memory-write-read workspace (plist-get intent :id) (plist-get intent :hash)))
    (error "Memory write intent changed"))
  (let* ((scope (mevedel-memory-write-scope intent))
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
    (unless (memq (mevedel-memory-write-state intent) '(before after mixed))
      (error "Memory rollback has intervening or unavailable edits"))
    ;; Restore only the rows the interrupted write reached; a row at neither
    ;; retained state is an intervening edit and blocks the whole rollback.
    (let (reached)
      (dolist (row (plist-get intent :before))
        (let* ((before (cdr row))
               (after (cdr (assoc (car row) (plist-get intent :after))))
               (current (mevedel-memory-scope--snapshot (plist-get before :path) mevedel-memory-scope--max-file-bytes)))
          (unless (mevedel-tool-patch--same-snapshot-p current before)
            (unless (mevedel-tool-patch--same-snapshot-p current after)
              (error "Memory rollback target changed"))
            (push (cons (car row) after) reached))))
      (mevedel-tool-patch-commit
       (mevedel-memory-write--changes (nreverse reached) (plist-get intent :before)) currentp
       (lambda (before after) (mevedel-memory-write--mutate target before after))))))

(defun mevedel-memory-write-retire (target intent)
  "Retire INTENT's marker under TARGET after a durable resolved decision.
The caller must have accepted the decision before releasing this write fence."
  (let ((pin (mevedel-memory-write--pin intent)))
    (unless (equal (plist-get target :directory)
                   (file-name-concat (file-name-directory (directory-file-name (file-name-directory pin))) "claims"))
      (error "Memory root ownership does not match its write marker"))
    (when (mevedel-session-control-fs-path-exists-p pin)
      (unless (equal (plist-get intent :hash) (mevedel-session-control-fs-read-file pin)) (error "Memory root marker changed"))
      (mevedel-memory-store--assert-owned target)
      (dolist (result
               (mevedel-session-control-fs-run-program
                (list (list :op 'absent :path (mevedel-journal-claim--path target t))
                      (list :op 'before-time :path (plist-get target :directory)
                            :content (number-to-string (plist-get target :expires-at)))
                      (list :op 'verify :path pin :content (plist-get intent :hash))
                      (list :op 'delete-file :path pin))
                (plist-get target :directory)))
        (mevedel-session-control-fs-program-value result)))))

(defun mevedel-memory-write-list (workspace &optional ids)
  "Inspect retained write records in WORKSPACE without mutating targets.
Each row has :id, and readable rows have :hash and :intent. Check original root
authority before reporting :marked. Preserve unreadable or unavailable records
with :error instead of dropping them or rebinding their target. Private intent
bodies require a separate original-root authority check before presentation.
IDS restricts inspection to those write identities, still reading each afresh."
  (let (records)
    (dolist (id (or ids
                    (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                            (mevedel-session-control-fs-list-directory
                             (mevedel-memory-write--directory workspace) mevedel-memory-write-intent-file-regexp))))
      (unless (mevedel-journal-store-id-p id) (error "Invalid memory write identity"))
      (let ((row (list :id id))
            (path (file-name-concat (mevedel-memory-write--directory workspace) (concat id ".el"))))
        (condition-case err
            (let* ((text (mevedel-session-control-fs-read-file path 'utf-8-unix (1+ mevedel-memory-store--max-bytes)))
                   (hash (secure-hash 'sha256 text))
                   (intent (mevedel-memory-write-read workspace (plist-get row :id) hash)))
              (if (not intent) (setq row nil)
                (setq row (append row (list :hash hash :intent intent)))
                (let ((pin (mevedel-memory-write--pin intent)))
                  (when (mevedel-session-control-fs-path-exists-p pin)
                    (unless (equal hash (mevedel-session-control-fs-read-file pin)) (error "Memory root marker changed"))
                    (setq row (append row (list :marked t)))))))
          (error (setq row (append row (list :error (error-message-string err))))))
        (when row (push row records))))
    (nreverse records)))

(defun mevedel-memory-write-pending (workspace proposal)
  "Return unresolved write intents for PROPOSAL in WORKSPACE.
Use original root authority and verify the independent target marker hash."
  (let (pending)
    (dolist (row (mevedel-memory-write-list workspace))
      (let ((intent (plist-get row :intent)))
        (when (or (not intent) (equal proposal (plist-get intent :proposal)))
          (when (plist-get row :error) (error "Memory write is unavailable: %s" (plist-get row :error)))
          (when (plist-get row :marked) (push intent pending)))))
    (nreverse pending)))

(provide 'mevedel-memory-write)
;;; mevedel-memory-write.el ends here
