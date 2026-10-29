;;; mevedel-memory-decision.el -- Durable memory proposal decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Decisions are separate immutable journal records. A workspace claim accepts
;; their private record hash before public publication, allowing replay after
;; process death. Rejection never edits a memory target. Private proposal text
;; enters later review evidence only under its captured root authority.

;;; Code:

(require 'cl-lib)
(require 'mevedel-journal-worker)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-transport)
(require 'mevedel-workspace-identity)

;; `mevedel-journal-cleanup'
(declare-function mevedel-journal-cleanup-active-p "mevedel-journal-cleanup" (workspace))
(declare-function mevedel-journal-cleanup-wait "mevedel-journal-cleanup" (workspace))
(autoload 'mevedel-journal-cleanup-wait "mevedel-journal-cleanup")

;; `mevedel-memory-notes'
(declare-function mevedel-memory-notes-passes "mevedel-memory-notes" (workspace entries))
(declare-function mevedel-memory-notes-recheck "mevedel-memory-notes" (root relative))
(declare-function mevedel-memory-notes-in-use-p "mevedel-memory-notes" (workspace relative))
(autoload 'mevedel-memory-notes-in-use-p "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-passes "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-recheck "mevedel-memory-notes")

;; `mevedel-memory-pass'
(defvar mevedel-memory-pass--drains)
(defvar mevedel-memory-pass--running)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry--monotonic-now "mevedel-telemetry" ())
(autoload 'mevedel-telemetry--monotonic-now "mevedel-telemetry")

(defun mevedel-memory-decision-terminal-status-p (status)
  "Return non-nil when decision STATUS ends its proposal's lifecycle."
  (memq status '(applied rejected reversed)))

(defun mevedel-memory-decision-actionable-status-p (status)
  "Return non-nil when a proposal with STATUS still awaits a user decision."
  (memq status '(pending stale)))

(defun mevedel-memory-decision-written-status-p (status)
  "Return non-nil when decision STATUS records a write that reached its target."
  (memq status '(applied reversed)))

(defun mevedel-memory-decision-resolved-write-status-p (status)
  "Return non-nil when decision STATUS settles a write attempt without recovery."
  (memq status '(applied reversed unavailable)))

(defvar mevedel-memory-decision--inhibit-recovery nil
  "Non-nil suppresses deferred recovery during exit or isolated tests.")

(defvar mevedel-memory-decision--recovery-dispatching (make-hash-table :test #'equal)
  "Observation-only tokens spanning scheduled recovery admission IO.")

(defvar mevedel-memory-decision--recovery-pending (make-hash-table :test #'equal)
  "Coalesced activation recovery timers, keyed by workspace claim directory.")

(defvar mevedel-memory-decision--recovery-workers (make-hash-table :test #'equal)
  "Active publication recovery and live-write checks, keyed by claim directory.")

(defvar mevedel-memory-decision--recovery-generations (make-hash-table :test #'equal)
  "Recoveries that changed committed state, counted by claim directory.")

(defvar mevedel-memory-decision--operations (make-hash-table :test #'equal)
  "Parent-owned async decision operations, keyed by workspace claim directory.")

(defun mevedel-memory-decision--directory (workspace)
  "Return the private decision directory for WORKSPACE."
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "decisions"))

(defun mevedel-memory-decision-status (workspace proposal &optional entries passes writes)
  "Return the latest accepted public decision for PROPOSAL in WORKSPACE.
Verify its immutable acceptance evidence; public text alone cannot authorize
a decision. Recover accepted decisions before using this to decide or apply.
ENTRIES, PASSES and WRITES, when non-nil, are one current read observation,
shared across a scan as `mevedel-memory-decision--read' describes."
  (let ((generation 0) (passes (or passes (make-hash-table :test #'equal))) latest)
    (dolist (entry (or entries (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
      (when (and (eq (plist-get entry :kind) 'decision) (equal proposal (plist-get entry :proposal-id))
                 (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get entry :pass-id))))
        (let* ((accepted (mevedel-memory-decision--published workspace entry passes writes))
               (number (plist-get (plist-get accepted :claim) :generation)))
          (when (> number generation) (setq generation number latest entry)))))
    latest))

(defun mevedel-memory-decision--published (workspace entry &optional passes writes)
  "Return accepted private state matching public decision ENTRY in WORKSPACE.
PASSES and WRITES are the current observation described by the private reader."
  (let* ((path (file-name-concat (mevedel-memory-decision--directory workspace)
                                (concat (plist-get entry :decision-id) ".el")))
         (accepted (mevedel-memory-decision--read workspace path passes writes))
         (metadata (plist-get accepted :metadata)))
    (unless (and accepted
                 (cl-every (lambda (field) (equal (plist-get entry (cdr field)) (plist-get metadata (cdr field))))
                           mevedel-journal-store--decision-fields))
      (error "Published decision has no matching acceptance evidence"))
    accepted))

(defun mevedel-memory-decision--record (workspace path &optional observation evidence)
  "Read authenticated decision metadata at PATH in WORKSPACE, or nil.
Verify immutable claim acceptance independently of the retained proposal body.
The full reader also verifies that body's dependencies before using a decision.
OBSERVATION and EVIDENCE may supply this operation's pinned record read and
the pair of coordinator claim/outcome reads from a bounded batch."
  (let* ((record (mevedel-memory-store--read-lisp path observation))
         (data (car record)) (text (cdr record))
         (claim (append (list :directory (mevedel-memory-store--claim-directory workspace)) (plist-get data :claim)))
         (evidence (or evidence
                       (mevedel-session-control-fs-run-program
                        (list (list :op 'read :path (mevedel-journal-claim--path claim nil))
                              (list :op 'read :path (mevedel-journal-claim--path claim t) :optional t)))))
         (metadata (plist-get data :metadata)))
    (unless (and (proper-list-p data) (= (length data) 6)
                 (plist-member data :claim) (plist-member data :metadata) (plist-member data :write-id)
                 (equal (mevedel-journal-claim--record claim)
                        (mevedel-journal-claim--read (mevedel-journal-claim--path claim nil) nil (car evidence)))
                 (equal (file-name-nondirectory path) (concat (plist-get claim :owner) ".el")))
      (error "Invalid private memory decision"))
    (when-let* ((outcome (mevedel-journal-claim-outcome claim (cadr evidence)))
                ((eq (plist-get outcome :status) 'completed)))
      (unless (equal (secure-hash 'sha256 text) (plist-get outcome :payload))
        (error "Accepted memory decision changed"))
      (mevedel-journal-store--validate-metadata metadata 'decision)
      (unless (equal (plist-get metadata :decision-id) (plist-get claim :owner))
        (error "Decision identity does not match its claim"))
      (unless (or (null (plist-get data :write-id))
                  (mevedel-journal-store-id-p (plist-get data :write-id)))
        (error "Invalid private memory decision write identity"))
      (list :claim claim :metadata metadata :write-id (plist-get data :write-id)
            :hash (secure-hash 'sha256 text)))))

(defun mevedel-memory-decision--record-batch (workspace paths)
  "Authenticate bounded decision PATHS in WORKSPACE with two pinned programs.
Return (PATH . RECORD) rows containing only closed metadata, never proposal
bodies.  Verify record hashes again alongside the claim/outcome reads, so a
change between reading and authentication fails closed."
  (unless (<= (length paths) 8) (error "Too many decision records in one batch"))
  (when paths
    (let* ((reads (mevedel-session-control-fs-run-program
                   (mapcar (lambda (path)
                             (list :op 'read :path path :coding 'utf-8-unix
                                   :max-bytes (1+ mevedel-memory-store--max-bytes))) paths)))
           (operations
            (cl-mapcan
             (lambda (path read)
               (let* ((record (mevedel-memory-store--read-lisp path read))
                      (claim (append (list :directory (mevedel-memory-store--claim-directory workspace))
                                     (plist-get (car record) :claim))))
                 (list (list :op 'verify-hash :path path :content (secure-hash 'sha256 (cdr record)))
                       (list :op 'read :path (mevedel-journal-claim--path claim nil))
                       (list :op 'read :path (mevedel-journal-claim--path claim t) :optional t))))
             paths reads))
           (evidence (mevedel-session-control-fs-run-program operations)))
      (unless (and (= (length reads) (length paths)) (= (length evidence) (* 3 (length paths))))
        (error "Incomplete decision record observations"))
      (cl-mapcar
       (lambda (path read)
         (mevedel-session-control-fs-program-value (pop evidence))
         (let ((proof (list (pop evidence) (pop evidence))))
           (cons path (mevedel-memory-decision--record workspace path read proof))))
       paths reads))))

(defun mevedel-memory-decision--read (workspace path &optional passes writes record)
  "Read an accepted private decision at PATH in WORKSPACE, or nil.
Unaccepted and retired decisions cannot publish. Verify surviving decisions'
original accepted proposal bundle and retained write evidence.
PASSES, when non-nil, holds the last authenticated bundle for this WORKSPACE
read observation. Reuse immutable pass proofs within a scan; callers
must discard the table after mutations and never retain it between operations.
WRITES may hold this observation's freshly authenticated write intents,
keyed by identity.  Only an exact hash and source-bundle match permits reuse.
RECORD may supply this operation's freshly authenticated metadata from
`mevedel-memory-decision--record-batch'; discard it after any publication."
  (when-let* ((data (or record (mevedel-memory-decision--record workspace path)))
              (metadata (plist-get data :metadata))
              ((not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get metadata :pass-id)))))
    (let* ((claim (plist-get data :claim))
           (pass (plist-get metadata :pass-id))
           (accepted (or (and passes (gethash pass passes))
                         (let ((bundle (mevedel-memory-store-accepted workspace pass)))
                           ;; Keep at most one potentially 4-MiB proposal bundle.
                           (when (and passes bundle)
                             (clrhash passes)
                             (puthash pass bundle passes))
                           bundle)))
           (observed (and writes (gethash (plist-get data :write-id) writes)))
           (intent (and (plist-get data :write-id)
                        (if (and observed
                                 (equal (plist-get observed :id) (plist-get data :write-id))
                                 (equal (plist-get observed :hash) (plist-get metadata :state-hash))
                                 (equal (plist-get (plist-get observed :accepted) :hash) (plist-get accepted :hash)))
                            observed
                          (mevedel-memory-write-read workspace (plist-get data :write-id) (plist-get metadata :state-hash)
                                                     accepted nil claim)))))
      (unless (and (equal (plist-get claim :owner) (plist-get metadata :decision-id))
                   (equal (plist-get metadata :workspace) (plist-get (plist-get accepted :review) :workspace))
                   (if intent
                       (and (equal (plist-get intent :pass) (plist-get metadata :pass-id))
                            (equal (plist-get intent :proposal) (plist-get metadata :proposal-id))
                            (or (not (eq (plist-get metadata :status) 'reversed)) (plist-get intent :reverse-of)))
                     (and (memq (plist-get metadata :status) '(rejected stale unavailable))
                          (equal (plist-get metadata :state-hash) (plist-get accepted :hash))))
                   (member (plist-get metadata :proposal-id) (plist-get (plist-get accepted :review) :proposals)))
        (error "Decision does not match its accepted proposal"))
      (list :claim claim :metadata metadata :accepted accepted :intent intent
            :hash (plist-get data :hash)))))

(defun mevedel-memory-decision--settled-p (workspace accepted)
  "Return non-nil when every proposal of ACCEPTED is terminal in WORKSPACE.
Read the public journal once for the whole pass.  An incomplete terminal
index can only refuse release; a complete index still needs private proofs."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (passes (make-hash-table :test #'equal))
        (terminal (make-hash-table :test #'equal))
        (proposals (plist-get (plist-get accepted :review) :proposals)))
    ;; Publication calls this after each decision.  Authenticating a growing
    ;; prefix before discovering the next absent proposal is quadratic IO.
    ;; Public metadata cannot prove settlement, but absence can refuse it.
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision)
                 (mevedel-memory-decision-terminal-status-p (plist-get entry :status)))
        (puthash (plist-get entry :proposal-id) t terminal)))
    ;; This check can follow publication.  Read its pass proof afresh rather
    ;; than seeding from the bundle supplied before that mutation.
    (and (cl-every (lambda (id) (gethash id terminal)) proposals)
         (cl-every (lambda (id)
                     (mevedel-memory-decision-terminal-status-p
                      (plist-get (mevedel-memory-decision-status workspace id entries passes) :status)))
                   proposals))))

(defun mevedel-memory-decision--publish (workspace decision)
  "Publish accepted DECISION in WORKSPACE, then release unneeded evidence.
Public pass metadata stays immutable. All proposals must be terminal before
their pass can release its remaining digest pins."
  (let* ((accepted (plist-get decision :accepted))
         (prepared (plist-get accepted :prepared)))
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (mevedel-memory-store--mutation
     workspace
     (lambda (mutation)
       (mevedel-memory-store--assert-owned mutation)
       (let ((published (mevedel-journal-store-publish-decision
                         (mevedel-workspace-root workspace) (plist-get decision :metadata))))
         (when (mevedel-memory-decision--settled-p workspace accepted)
           (mevedel-memory-store--release-pins workspace prepared nil mutation))
         (setf (mevedel-workspace-journal-observation workspace) nil)
         (mevedel-journal-cleanup-schedule workspace t)
         published)))))

(defun mevedel-memory-decision-recover (workspace)
  "Publish all accepted pending decisions in WORKSPACE without inference.
Malformed accepted state remains an error, preserving evidence for inspection.
Reuse one public observation; already published history needs only one pending
evidence-release check per pass, not repeated republication per decision.
Group metadata by pass so the bounded single-bundle observation
does not reread large proposal bodies for interleaved decision filenames.
Return non-nil when a decision was published or its evidence released."
  (let ((published (make-hash-table :test #'equal)) (checked (make-hash-table :test #'equal))
        (passes (make-hash-table :test #'equal)) (groups (make-hash-table :test #'equal))
        (records-current t) changed)
    (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
      (puthash (plist-get entry :file) (plist-get entry :text) published))
    (let ((paths (mevedel-session-control-fs-list-directory
                  (mevedel-memory-decision--directory workspace) mevedel-memory-write-intent-file-regexp)))
      (while paths
        (let ((batch (seq-take paths 8)))
          (setq paths (nthcdr (length batch) paths))
          (dolist (row (mevedel-memory-decision--record-batch workspace batch))
            (when-let* ((record (cdr row)))
              (push row (gethash (plist-get (plist-get record :metadata) :pass-id) groups)))))))
    (maphash
     (lambda (_pass records)
       (dolist (row records)
         (when-let* ((decision (mevedel-memory-decision--read workspace (car row) passes nil
                                                            (and records-current (cdr row)))))
           (let* ((metadata (plist-get decision :metadata))
                  (prepared (plist-get (plist-get decision :accepted) :prepared))
                  (review (plist-get (plist-get decision :accepted) :review))
                  (review-file (mevedel-journal-store--filename review 'consolidation))
                  (review-text (mevedel-journal-store--encode review nil 'consolidation))
                  (pass (plist-get prepared :id))
                  (file (mevedel-journal-store--filename metadata 'decision))
                  (expected (mevedel-journal-store--encode metadata nil 'decision))
                  ;; Pins outlive the pass until all its proposals are terminal;
                  ;; republishing earlier cannot release them.
                  (release (and (not (gethash pass checked))
                                (seq-some (lambda (entry)
                                            (mevedel-session-control-fs-path-exists-p
                                             (mevedel-memory-store--pin workspace pass (plist-get entry :id))))
                                          (plist-get prepared :entries))
                                (mevedel-memory-decision--settled-p workspace (plist-get decision :accepted)))))
             (puthash pass t checked)
             (when (or release (not (equal expected (gethash file published)))
                       (not (equal review-text (gethash review-file published))))
               (mevedel-memory-decision--publish workspace decision)
               (setq changed t records-current nil)
               (clrhash passes)
               (puthash file expected published)
               (puthash review-file review-text published))))))
     groups)
    changed))

(defun mevedel-memory-decision--accept (workspace claim accepted proposal status reason &optional intent)
  "Accept STATUS and REASON for PROPOSAL from ACCEPTED under WORKSPACE CLAIM.
INTENT retains exact before/after evidence when files may have been written.
This private operation freezes a decision before public publication."
  (mevedel-memory-store--assert-owned claim)
  (let* ((prepared (plist-get accepted :prepared))
         (pass (plist-get prepared :id))
         (id (plist-get claim :owner))
         (metadata (list :decision-id id :pass-id pass :proposal-id (plist-get proposal :id)
                         :workspace (plist-get (plist-get accepted :review) :workspace)
                         :created (mevedel-journal-store-timestamp
                                   (mevedel-session-control-fs-target-time (plist-get claim :directory)))
                         :status status :reason reason
                         :state-hash (or (plist-get intent :hash) (plist-get accepted :hash))))
         (directory (mevedel-memory-decision--directory workspace))
         (path (file-name-concat directory (concat id ".el")))
         text)
    (mevedel-journal-store--validate-metadata metadata 'decision)
    (unless (and (equal accepted (mevedel-memory-store-accepted workspace pass))
                 (member proposal (plist-get accepted :proposals)))
      (error "Proposal is not in the accepted pass"))
    (mevedel-memory-scope--root (plist-get prepared :scope) (plist-get proposal :root))
    (setq text (mevedel-memory-store--print (list :claim (mevedel-journal-claim--record claim) :metadata metadata :write-id (plist-get intent :id))))
    (mevedel-memory-store--assert-owned claim)
    (mevedel-session-control-fs-make-directory directory t)
    (unless (mevedel-session-control-fs-create-file path text) (error "Decision already exists"))
    (unless (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text))
      (error "Decision lost ownership before acceptance"))
    (mevedel-memory-decision--read workspace path)))

(defun mevedel-memory-decision--with-claim (workspace recover-store function)
  "Call FUNCTION with a fresh workspace claim after recovering WORKSPACE state.
Recover published decisions first, and settled pass publications too when
RECOVER-STORE is non-nil, so FUNCTION sees every fenced predecessor's outcome.
Signal when another client owns the workspace.  The claim is cancelled
afterwards; FUNCTION settles durable outcomes itself."
  (let ((claim (or (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)
                   (progn
                     (mevedel-journal-cleanup-wait workspace)
                     (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
                   (user-error "Memory consolidation is busy"))))
    (unwind-protect
        (progn
          (mevedel-memory-decision-recover workspace)
          (when recover-store (mevedel-memory-store-recover workspace))
          (funcall function claim))
      (mevedel-journal-claim-settle claim 'cancelled ""))))

(defun mevedel-memory-decision-reject (workspace pass proposal &optional reason)
  "Reject PROPOSAL from PASS in WORKSPACE with optional REASON.
Acquire workspace ownership, recover accepted state, and verify original target
authority. Repeated decisions return the existing terminal record unchanged."
  (setq reason (or reason ""))
  (unless (and (stringp reason) (<= (string-bytes reason) 4096)) (error "Rejection reason exceeds its byte limit"))
  (mevedel-memory-decision--with-claim
   workspace t
   (lambda (claim)
     (let* ((accepted (mevedel-memory-store-accepted workspace pass))
            (item (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals)))
            (previous (mevedel-memory-decision-status workspace proposal)))
       (unless item (error "Unknown accepted proposal"))
       (mevedel-memory-scope--root (plist-get (plist-get accepted :prepared) :scope) (plist-get item :root))
       (if (mevedel-memory-decision-terminal-status-p (plist-get previous :status)) previous
         (when (mevedel-memory-write-pending workspace proposal) (error "Recover the pending memory write before rejecting"))
         (mevedel-memory-decision--publish
          workspace (mevedel-memory-decision--accept workspace claim accepted item 'rejected reason)))))))

(defun mevedel-memory-decision-rejections (workspace)
  "Return at most 8 KiB of recent rejection evidence for WORKSPACE's next pass.
Include original proposed content and the user's reason, under captured target
authority. Omit whole records when unavailable or too large and report counts.
Evidence can help recognize reworded suggestions; it grants no instruction
authority and promises no permanent semantic suppression."
  (let ((rows nil) (omitted 0))
    (cl-labels ((encode (items count)
                  (decode-coding-string
                   (json-serialize (list :rejections (vconcat items) :omitted count)) 'utf-8-unix)))
      (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (when (and (eq (plist-get entry :kind) 'decision) (eq (plist-get entry :status) 'rejected)
                   (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get entry :pass-id))))
          (let ((row
                 (condition-case nil
                     (let* ((accepted (plist-get (mevedel-memory-decision--published workspace entry) :accepted))
                            (proposal (seq-find (lambda (proposal) (equal (plist-get entry :proposal-id) (plist-get proposal :id)))
                                                (plist-get accepted :proposals)))
                            (root (mevedel-memory-scope--root (plist-get (plist-get accepted :prepared) :scope)
                                                            (plist-get proposal :root))))
                       (list :action (symbol-name (plist-get proposal :action)) :root (plist-get root :label)
                             :file (plist-get proposal :file) :title (plist-get proposal :title)
                             :body (plist-get proposal :body) :reason (plist-get entry :reason)
                             :created (plist-get entry :created)))
                   (error nil))))
            (if (and row (< (length rows) 20)
                     (<= (string-bytes (encode (append rows (list row)) most-positive-fixnum)) 8192))
                (setq rows (append rows (list row)))
              (cl-incf omitted)))))
      (encode rows omitted))))

(defun mevedel-memory-decision--finish-write (workspace claim target intent &optional failed)
  "Record INTENT's observed outcome under WORKSPACE CLAIM and TARGET.
FAILED says execution did not finish normally. Mixed, changed, or unavailable
targets retain the root marker for checked recovery, never blind reapplication."
  (let* ((state (mevedel-memory-write-state intent))
         (reverse (plist-get intent :reverse-of))
         (status (cond ((or (eq state 'after) (and (eq state 'before) (not (plist-get intent :changes))))
                        (if reverse 'reversed 'applied))
                       ((eq state 'before) (if reverse 'applied 'unavailable))
                       (t 'recovery-required)))
         (note (mevedel-memory-proposal-note-action-p (plist-get (plist-get intent :item) :action)))
         (reason (pcase status
                   ('applied (cond (reverse "Reversal left the original application unchanged.")
                                   (note "Applied shared-note decision; previous content is retained for undo.")
                                   (t "Applied captured memory proposal.")))
                   ('reversed (if note "Restored the shared note from its retained content."
                                "Reversed the captured memory application."))
                   ('unavailable (if failed "Attempt left targets unchanged." "Application was interrupted before completion."))
                   (_ "Targets need checked recovery.")))
         (decision (mevedel-memory-decision--accept workspace claim (plist-get intent :accepted) (plist-get intent :item)
                                                     status reason intent))
         (published (mevedel-memory-decision--publish workspace decision)))
    (unless (eq status 'recovery-required) (mevedel-memory-write-retire target intent))
    published))

(defun mevedel-memory-decision--requirements (workspace proposal)
  "Return nil when PROPOSAL's required extractions in WORKSPACE are applied.
Otherwise return (:status STATUS :reason REASON).  An undecided requirement
returns `waiting', which records no decision; any other outcome makes the
dependent note disposal stale, so its note returns to review unchanged."
  (let (waiting failed)
    (dolist (id (plist-get proposal :requires))
      (let ((status (plist-get (mevedel-memory-decision-status workspace id) :status)))
        (cond ((eq status 'applied))
              ((null status) (setq waiting t))
              (t (push status failed)))))
    (cond (failed (list :status 'stale
                        :reason (format "A required memory extraction was not applied (%s); the note stays for review."
                                        (mapconcat #'symbol-name (delete-dups failed) ", "))))
          (waiting (list :status 'waiting
                         :reason "Apply the memory extractions from this note first.")))))

(defun mevedel-memory-decision--application-input (accepted proposal &optional workspace)
  "Prepare checked application input for ACCEPTED PROPOSAL without writes.
Return :scope and :proposal working copies, plus :status and optional :reason.
Only unrelated index edits may advance the captured before-state. Application
must hold original-root ownership across this preparation and the transaction.
With WORKSPACE, a note disposal also checks its required extractions."
  (let* ((scope (copy-tree (plist-get (plist-get accepted :prepared) :scope)))
         (item (copy-tree proposal))
         (index (assoc "MEMORY.md" (plist-get item :before)))
         (check (and workspace (mevedel-memory-decision--requirements workspace proposal))))
    (condition-case err
        (let ((root (mevedel-memory-scope--root scope (plist-get item :root))))
          (when index
            (let* ((current (mevedel-memory-scope--snapshot
                             (plist-get (cdr index) :path) mevedel-memory-scope--max-file-bytes))
                   (conflicts (mevedel-memory-apply--index-conflicts (cdr index) current item)))
              (if conflicts
                  (setq check (list :status 'stale :reason
                                    (format "MEMORY.md entries changed since capture: %s"
                                            (string-join conflicts ", "))))
                (setcdr index current)
                (setf (alist-get "MEMORY.md" (plist-get root :before) nil nil #'equal) current))))
          (unless check
            (setq check (mevedel-memory-scope-check scope (plist-get item :root)
                                                   (mapcar #'car (plist-get item :before)))))
          ;; Housekeeping proved an attachment unreferenced at capture; a
          ;; note written since then may reference it.
          (when (and (eq (plist-get check :status) 'fresh)
                     (equal (plist-get item :type) "attachment"))
            (when-let* ((reason (mevedel-memory-notes-recheck root (plist-get item :file))))
              (setq check (list :status 'stale :reason reason)))))
      (error (setq check (list :status 'unavailable :reason (error-message-string err)))))
    (append check (list :scope scope :proposal item))))

(cl-defun mevedel-memory-decision--apply-owned (workspace claim pass proposal on-applied &optional currentp pending)
  "Apply PASS's PROPOSAL in WORKSPACE under an already recovered CLAIM.
ON-APPLIED receives newly confirmed paths.  CURRENTP guards automatic
forward writes; note decisions also recheck that no open session uses their
file.  PENDING skips any existing decision.  No observation crosses recovery
or a previous attempt."
  (cl-labels ((live (item)
		(and (or (null currentp) (funcall currentp))
		     (or (null currentp)
			 (not (mevedel-memory-proposal-note-action-p (plist-get item :action)))
			 (not (mevedel-memory-notes-in-use-p workspace (plist-get item :file)))))))
    (let* ((accepted (mevedel-memory-store-accepted workspace pass))
	   (item (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals)))
	   (scope (plist-get (plist-get accepted :prepared) :scope))
	   (previous (mevedel-memory-decision-status workspace proposal)))
      (unless item (error "Unknown accepted proposal"))
      (mevedel-memory-scope--root scope (plist-get item :root))
      (when (and pending previous)
	(cl-return-from mevedel-memory-decision--apply-owned '(:status skipped)))
      (if (mevedel-memory-decision-terminal-status-p (plist-get previous :status)) previous
	(unless (live item)
	  (cl-return-from mevedel-memory-decision--apply-owned '(:status held)))
	(mevedel-memory-write-call
	 scope (plist-get item :root)
	 (lambda (target)
	   (let* ((input (mevedel-memory-decision--application-input accepted item workspace))
		  (scope (plist-get input :scope)) (effective (plist-get input :proposal)))
	     (when (eq (plist-get input :status) 'waiting)
	       (user-error "%s" (plist-get input :reason)))
	     (unless (live item)
	       (cl-return-from mevedel-memory-decision--apply-owned '(:status held)))
	     (if (not (eq (plist-get input :status) 'fresh))
		 (mevedel-memory-decision--publish
		  workspace (mevedel-memory-decision--accept workspace claim accepted item (plist-get input :status)
							     (mevedel--truncate-bytes (plist-get input :reason) 4096 "...")))
	       (let ((intent (condition-case err
                                 (mevedel-memory-write-prepare workspace claim target accepted effective scope)
                               (quit (if currentp (signal 'mevedel-memory-decision-interrupted
                                                          (list nil "Quit" "Interrupted memory write preparation: Quit"))
                                       (signal (car err) (cdr err))))))
                     failure original)
		 (condition-case err
		     (mevedel-memory-write-run
		      workspace claim target intent
		      (lambda () (unless (live item) (signal 'mevedel-memory-decision-held nil))))
		   ((error quit) (setq failure err)))
                 (setq original (if (eq (car-safe failure) 'mevedel-tool-patch-partial-rollback)
                                    (cadr failure) failure))
                 (when (and currentp failure (memq (car failure) '(mevedel-memory-decision-held quit))
                            (condition-case nil
                                (eq (mevedel-memory-write-state intent) 'before)
                              (quit (signal 'mevedel-memory-decision-interrupted
                                            (list nil "Quit" "Interrupted memory write observation: Quit")))))
                   (condition-case err
                       (mevedel-memory-write-abandon workspace claim target intent)
                     ((error quit)
                      (let ((message (format "Held memory write retirement failed: %s" (error-message-string err))))
                        (if (or (eq (car err) 'quit) (eq (car-safe original) 'quit))
                            (signal 'mevedel-memory-decision-interrupted (list nil "Quit" message))
                          (error "%s" message)))))
                   (signal (car failure) (cdr failure)))
		 (let ((decision
                        (condition-case err
                            (mevedel-memory-decision--finish-write workspace claim target intent failure)
                          ((error quit)
                           (if (and currentp (or (eq (car err) 'quit) (eq (car-safe original) 'quit)))
                               (signal 'mevedel-memory-decision-interrupted
                                       (list nil "Quit" (format "Interrupted memory write accounting: %s" (error-message-string err))))
                             (signal (car err) (cdr err)))))))
                   (when (and (not currentp) (eq (car-safe failure) 'quit))
                     (signal (car failure) (cdr failure)))
		   (when (and on-applied (eq (plist-get decision :status) 'applied))
		     (funcall on-applied (mapcar (lambda (change) (plist-get change :path))
						 (plist-get intent :changes))))
                   ;; Fully account reached or uncertain writes before cancellation.
                   ;; Manual synchronous callers keep their original quit condition.
                   (when (and currentp (eq (car-safe original) 'quit))
                     (signal 'mevedel-memory-decision-interrupted (list decision "Quit")))
		   decision))))))))))

(defun mevedel-memory-decision-apply (workspace pass proposal &optional on-applied currentp)
  "Apply accepted PROPOSAL from PASS in WORKSPACE with durable write intent.
Acquire both workspace and original-root ownership. Stale targets produce an
immutable decision without writes. Partial results retain checked recovery.
ON-APPLIED receives changed paths only for a newly confirmed application.
Manual and unsupported-target callers retain this synchronous API.  Automatic
callers pass CURRENTP; a note whose file an open session uses is then held,
as status `held' or signal `mevedel-memory-decision-held'."
  (mevedel-memory-decision--with-claim
   workspace t
   (lambda (claim)
     (mevedel-memory-decision--apply-owned workspace claim pass proposal on-applied currentp))))

(defun mevedel-memory-decision--worker-local-p (workspace)
  "Whether WORKSPACE's authenticated retained scopes need no remote target IO.
Current configuration alone cannot classify historical original-root authority.
Read only local acceptance metadata here, before target-sensitive recovery."
  (condition-case nil
      (not (cl-some
            (lambda (accepted)
              (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                       (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
            (mevedel-memory-notes-passes
             workspace (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
    ;; Recovery, not this metadata preflight, owns interrupted accepted expiry
    ;; and incomplete state.  Uncertainty selects the checked native path.
    (error nil)))

(define-error 'mevedel-memory-decision-held "Memory application is held")
(define-error 'mevedel-memory-decision-interrupted "Memory application interrupted" 'quit)

(defun mevedel-memory-decision--operation-context (workspace origin)
  "Capture WORKSPACE and ORIGIN's expected context, not filesystem authority."
  (let* ((session (buffer-local-value 'mevedel--session origin))
         (other (and session (mevedel-session-workspace session))))
    (list :workspace workspace :root (mevedel-workspace-root workspace) :origin origin
          :workspace-id (mevedel-workspace-identity-read (mevedel-workspace-root workspace))
          :origin-session session :origin-workspace other
          :origin-root (and other (mevedel-workspace-root other)))))

(defun mevedel-memory-decision--operation-origin-current-p (job)
  "Return non-nil while JOB's live origin and workspace still match its context."
  (let ((origin (plist-get job :origin)) (other (plist-get job :origin-workspace)))
    (and (buffer-live-p origin)
         (equal (plist-get job :root) (mevedel-workspace-root (plist-get job :workspace)))
         (eq (plist-get job :origin-session) (buffer-local-value 'mevedel--session origin))
         (or (null (plist-get job :origin-session))
             (and (eq other (mevedel-session-workspace (plist-get job :origin-session)))
                  (equal (plist-get job :origin-root) (and other (mevedel-workspace-root other))))))))

(defun mevedel-memory-decision--operation-live-p (job)
  "Return non-nil while JOB can admit its next forward editor operation."
  (and (not (plist-get job :done)) (not (plist-get job :stopped))
       (not mevedel-journal-worker--stopping)
       (eq job (gethash (plist-get (plist-get job :claim) :directory)
			mevedel-memory-decision--operations))
       (mevedel-memory-decision--operation-origin-current-p job)
       ;; The durable identity API caches immutable IDs for ordinary saves.
       ;; Recovery may yield across a replaced workspace: compare its expected
       ;; identity with a fresh read, never that process-wide cache.
       (let ((mevedel-workspace-identity--cache (make-hash-table :test #'equal)))
         (equal (plist-get job :workspace-id)
                (mevedel-workspace-identity-read (plist-get job :root))))
       (or (null (plist-get job :currentp)) (funcall (plist-get job :currentp)))))

(defun mevedel-memory-decision--operation-fence (claim)
  "Establish a durable cancellation, competing outcome or expiry for CLAIM.
A nil settlement is not proof.  Authenticate an existing outcome, or elect
expiry only after freshly observing CLAIM's original target-clock deadline."
  (or (mevedel-journal-claim-settle claim 'cancelled "")
      (mevedel-journal-claim-outcome claim)
      (when (>= (mevedel-session-control-fs-target-time (plist-get claim :directory))
                (plist-get claim :expires-at))
        (mevedel-journal-claim--finish claim 'expired "")
        (mevedel-journal-claim-outcome claim))
      (error "No durable memory operation fence")))

(defun mevedel-memory-decision--recovery-successor-p (job predecessor)
  "Whether JOB directly succeeds its fully recovered PREDECESSOR.
This is a fresh claim-chain proof, not cached proposal or target authority.
An intervening workspace claimant or a changed actor requires full recovery."
  (let ((claim (plist-get job :claim))
        (prior (plist-get predecessor :claim))
        (outcome (plist-get predecessor :outcome)))
    (when (and predecessor (not (plist-get job :decisions-only))
               (integerp (plist-get prior :generation))
               (memq (plist-get outcome :status) '(completed cancelled))
               (equal (plist-get claim :directory) (plist-get prior :directory))
               (= (plist-get claim :generation) (1+ (plist-get prior :generation)))
               (cl-every (lambda (key) (equal (plist-get job key) (plist-get predecessor key)))
                         '(:workspace :root :origin :workspace-id :origin-session :origin-workspace :origin-root)))
      (let ((results
             (mevedel-session-control-fs-run-program
              (list (list :op 'verify-latest :path (mevedel-journal-claim--path claim nil) :content ".claim")
                    (list :op 'verify :path (mevedel-journal-claim--path claim nil)
                          :content (json-serialize (mevedel-journal-claim--record claim)))
                    (list :op 'absent :path (mevedel-journal-claim--path claim t))
                    (list :op 'before-time :path (plist-get claim :directory)
                          :content (number-to-string (plist-get claim :expires-at)))
                    (list :op 'verify :path (mevedel-journal-claim--path prior nil)
                          :content (json-serialize (mevedel-journal-claim--record prior)))
                    (list :op 'verify :path (mevedel-journal-claim--path prior t)
                          :content (json-serialize
                                    (plist-put (copy-sequence outcome) :status
                                               (symbol-name (plist-get outcome :status))))))
              (plist-get claim :directory))))
        (and (= 6 (length results))
             (cl-every (lambda (result) (eq (plist-get result :status) 'ok)) results))))))

(defun mevedel-memory-decision--operation-finish (job result)
  "Release JOB and deliver RESULT exactly once, after any editor work unwinds."
  (unless (plist-get job :done)
    (plist-put job :done t)
    (when-let* ((start (plist-get job :recovery-started-at)))
      (plist-put job :recovery-round-trip-ms
                 (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) start)))))
      (plist-put job :recovery-started-at nil))
    (when (plist-member job :recovery-round-trip-ms)
      (plist-put result :recovery-round-trip-ms (plist-get job :recovery-round-trip-ms)))
    (when (plist-get job :cancelled)
      (plist-put result :cancelled t)
      (plist-put result :reason (or (plist-get result :reason) (plist-get job :stopped))))
    (when-let* ((failure (plist-get job :stop-error)))
      (plist-put result :error (if-let* ((prior (plist-get result :error))) (concat prior "; " failure) failure)))
    (let ((cancelled (plist-get result :cancelled)) failure fenced)
      (condition-case err
          (setq fenced (mevedel-memory-decision--operation-fence (plist-get job :claim)))
        ((error quit)
         (when (eq (car err) 'quit) (setq cancelled t))
         (setq failure (error-message-string err))
         ;; Settlement is an idempotent outcome election.  One bounded retry
         ;; may establish the fence after transient IO; still report the error.
         (condition-case retry
             (setq fenced (mevedel-memory-decision--operation-fence (plist-get job :claim)))
           ((error quit)
            (when (eq (car retry) 'quit) (setq cancelled t))
            (setq failure (concat failure "; " (error-message-string retry)))))))
      (unwind-protect
          (condition-case err
              ;; Never delete an unfenced child.  On persistent storage failure
              ;; it stays tracked until the original deadline's final fencing
              ;; attempt.  Persistent inability to prove a fence stays visible.
              (when fenced
                (when-let* ((worker (plist-get job :worker)) ((process-live-p worker)))
                  (delete-process worker)))
            ((error quit)
             (when (eq (car err) 'quit) (setq cancelled t))
             (setq failure (concat failure "; " (error-message-string err)))))
        (let ((key (plist-get (plist-get job :claim) :directory)))
          (when (eq job (gethash key mevedel-memory-decision--operations))
            (remhash key mevedel-memory-decision--operations))))
      (when (and fenced (plist-get job :recovered) (plist-get result :ok)
                 (memq (plist-get fenced :status) '(completed cancelled))
                 (not (plist-get job :decisions-only))
                 (not failure) (not (plist-get result :error))
                 (not (plist-get result :held)) (not (plist-get result :cancelled))
                 (not (plist-get job :stopped))
                 (let ((status (plist-get (plist-get result :value) :status)))
                   (or (null status) (eq status 'skipped)
                       (mevedel-memory-decision-terminal-status-p status))))
        ;; Carry only an exact completed predecessor, never accepted bundles,
        ;; public observations or target proofs.  The next distinct claim must
        ;; freshly prove adjacency and these immutable bytes before skipping IO.
        (plist-put result :recovery
                   (append (list :claim (copy-tree (plist-get job :claim)) :outcome (copy-tree fenced))
                           (cl-loop for key in '(:workspace :root :origin :workspace-id :origin-session :origin-workspace :origin-root)
                                    append (list key (plist-get job key))))))
      (when failure
        (plist-put job :cleanup-error failure)
        (plist-put result :error (format "%sMemory operation cleanup failed: %s%s"
                                         (if (plist-get result :error) (concat (plist-get result :error) "; ") "") failure
                                         (if fenced "" "; unsettled child remains tracked under its fixed deadline")))
        (plist-put result :cancelled cancelled)
        (plist-put result :unsettled (not fenced))))
    (plist-put job :unsettled (plist-get result :unsettled))
    (unless (plist-get job :unsettled)
      (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer))))
    (if (mevedel-memory-decision--operation-origin-current-p job)
        (with-current-buffer (plist-get job :origin) (funcall (plist-get job :callback) result))
      (with-temp-buffer (funcall (plist-get job :callback) result)))))

(defun mevedel-memory-decision--operation-retire (job)
  "Try terminal fencing and child retirement at JOB's original fixed deadline."
  (when (plist-get job :unsettled)
    (condition-case err
        (progn
          ;; Nil settlement is not proof either: authenticate the exact token's
          ;; durable outcome before deleting any still-live child.
          (mevedel-memory-decision--operation-fence (plist-get job :claim))
          (plist-put job :unsettled nil)
          (when-let* ((worker (plist-get job :worker)) ((process-live-p worker)))
            (delete-process worker)))
      ((error quit)
       (plist-put job :cleanup-error (error-message-string err))
       (display-warning 'mevedel
                        (format "Memory worker retirement remains unresolved: %s" (error-message-string err))
                        :warning)))))

(defun mevedel-memory-decision--operation-deadline (job)
  "Stop JOB or retire an unresolved child at its original unrenewed deadline."
  (unwind-protect
      (unless (plist-get job :done)
        (mevedel-memory-decision--operation-stop job "Memory operation timed out"))
    (when (plist-get job :done) (mevedel-memory-decision--operation-retire job))))

(defun mevedel-memory-decision--operation-stop (job &optional reason)
  "Fence JOB before stopping its child; nil REASON requests normal cancellation.
Non-nil REASON describes an actionable failure, such as a timeout.
An executing editor transaction unwinds before delivering the final callback."
  (unless (plist-get job :done)
    (plist-put job :stopped (or reason "Memory operation cancelled"))
    (if reason
        (plist-put job :stop-error (or (plist-get job :stop-error) reason))
      (plist-put job :cancelled t))
    ;; The child has finished before editor execution starts.  Keep the fixed
    ;; claim through that transaction's unwind so checked rollback stays owned;
    ;; :stopped independently refuses every new forward write.
    (unless (plist-get job :executing)
      (mevedel-memory-decision--operation-finish job
						 (list :reason (plist-get job :stopped))))))

(defun mevedel-memory-decision--operation-ready (job reply)
  "Continue JOB from worker REPLY with fresh ownership and live editor checks."
  (when (and (not (plist-get job :done)) (not (plist-get job :executing))
             (not (plist-get job :stopped))
             (eq job (gethash (plist-get (plist-get job :claim) :directory)
                              mevedel-memory-decision--operations)))
    (plist-put job :worker nil)
    (plist-put job :executing t)
    (let ((result
           (condition-case err
               (progn
                 (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
                 (unless (plist-get reply :ok) (error "Invalid memory recovery reply"))
                 (unless (equal (plist-get reply :operation-id) (plist-get (plist-get job :claim) :owner))
                   (error "Memory recovery reply belongs to another operation"))
                 (unless (mevedel-memory-decision--operation-live-p job)
                   (signal 'mevedel-memory-decision-held '("Memory operation context changed")))
                 (mevedel-memory-store--assert-owned (plist-get job :claim))
                 (with-current-buffer (plist-get job :origin)
                   (when (and (plist-get reply :synchronous) (not (plist-get job :recovered)))
                     ;; Historical remote roots retain their original authority
                     ;; even after configuration changes.  Keep their recovery
                     ;; in the editor, under this same continuously held claim.
                     (mevedel-memory-decision-recover (plist-get job :workspace))
                     (unless (plist-get job :decisions-only)
                       (mevedel-memory-store-recover (plist-get job :workspace)))
                     (unless (mevedel-memory-decision--operation-live-p job)
                       (signal 'mevedel-memory-decision-held '("Memory operation context changed")))
                     (mevedel-memory-store--assert-owned (plist-get job :claim)))
                   (plist-put job :recovered t)
                   (when-let* ((start (plist-get job :recovery-started-at)))
                     (plist-put job :recovery-round-trip-ms
                                (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) start)))))
                     (plist-put job :recovery-started-at nil))
                   (list :ok t :value
                         (funcall (plist-get job :function) (plist-get job :claim)
                                  (lambda () (mevedel-memory-decision--operation-live-p job)) reply))))
             (mevedel-memory-decision-held (list :held t :reason (error-message-string err)))
             (mevedel-memory-decision-interrupted
              (append (when (cadr err) (list :ok t :value (cadr err)))
                      (list :cancelled t :reason (caddr err))
                      (cond ((cadddr err) (list :error (cadddr err)))
                            ((and (cadr err) (not (mevedel-memory-decision-terminal-status-p (plist-get (cadr err) :status))))
                             (list :error "Interrupted memory write requires checked recovery")))))
             (quit (list :cancelled t :reason (error-message-string err)))
             (error (list :error (error-message-string err))))))
      (plist-put job :executing nil)
      (mevedel-memory-decision--operation-finish job result))))

(defun mevedel-memory-decision--start-operation (workspace function callback &optional currentp decisions-only payload recovery)
  "Recover WORKSPACE in a child, then call FUNCTION under the same owned claim.
FUNCTION receives the fresh claim, a live forward-admission predicate and reply.
CALLBACK receives a successful :value, a normal :held or :cancelled result,
or an actionable :error, once.  Actual results survive later cancellation
or cleanup failure, with additional metadata.  CURRENTP is an additional
live admission gate.  DECISIONS-ONLY omits store recovery.
PAYLOAD supplies additional read-only selection inputs, never accepted proofs.
RECOVERY may identify this application sequence's fully completed predecessor.
Only a fresh exact-successor claim proof omits redundant history recovery;
proposal, status, original-target and live-editor checks are never reused.
The fixed 180-second claim covers worker and editor phases; no target ownership
crosses the child.  Unsupported targets recover synchronously in the editor."
  (let* ((context (mevedel-memory-decision--operation-context workspace (current-buffer)))
         (key (mevedel-memory-store--claim-directory workspace))
         (claim (or (mevedel-journal-claim-acquire key 180) (user-error "Memory consolidation is busy")))
         (job (append (list :claim claim :function function :callback callback :currentp currentp
                            :decisions-only decisions-only)
                      context)))
    (puthash key job mevedel-memory-decision--operations)
    (condition-case err
        (progn
          (plist-put job :timer
                     (run-at-time
                      (max 0 (- (plist-get claim :expires-at)
                                (mevedel-session-control-fs-target-time key))) nil
                      #'mevedel-memory-decision--operation-deadline job))
          (when (and (or (null payload) (plist-get payload :pending-selection))
                     (mevedel-memory-decision--recovery-successor-p job recovery))
            (plist-put job :recovered t))
          (unless (plist-get job :recovered)
            (plist-put job :recovery-started-at (mevedel-telemetry--monotonic-now)))
          (if (and (plist-get job :recovered) (null payload))
              (mevedel-memory-decision--operation-ready
               job (list :ok t :operation-id (plist-get claim :owner)))
            (if (mevedel-journal-worker-supported-p workspace)
              (mevedel-journal-worker-start
               workspace 'memory-recover
               (lambda (reply) (mevedel-memory-decision--operation-ready job reply))
               nil (append (list :claim claim :operation-id (plist-get claim :owner)
				 :decisions-only decisions-only
                                 :recovery-predecessor
                                 (when (plist-get job :recovered)
                                   (list :claim (plist-get recovery :claim) :outcome (plist-get recovery :outcome))))
                           payload)
               job)
            (mevedel-memory-decision--operation-ready
             job (list :ok t :operation-id (plist-get claim :owner) :synchronous t)))))
      ((error quit)
       ;; Synchronous fallback also delivers the consumer callback here.  Once
       ;; released, its condition is not a second operation result to swallow.
       (if (plist-get job :done) (signal (car err) (cdr err))
         (mevedel-memory-decision--operation-finish
          job (if (eq (car err) 'quit) (list :cancelled t :reason (error-message-string err))
                (list :error (error-message-string err)))))))
    job))

(defun mevedel-memory-decision-reverse (workspace pass proposal)
  "Reverse the accepted application of PROPOSAL from PASS in WORKSPACE.
Require every target to match that application's exact after-state, including
its shared index. Persist a separate reverse intent before writes and retain
all prior decisions. Repeated reversal returns the same terminal result.
Reversing an extraction first restores every applied note disposal that
depended on it; if a restoration fails, the extraction stays applied."
  (when (eq 'applied (plist-get (mevedel-memory-decision-status workspace proposal) :status))
    (mevedel-memory-decision--reverse-dependents workspace pass proposal))
  (mevedel-memory-decision--with-claim
   workspace nil
   (lambda (claim)
     (let ((previous (mevedel-memory-decision-status workspace proposal)))
       (unless (and (equal pass (plist-get previous :pass-id))
                    (mevedel-memory-decision-written-status-p (plist-get previous :status)))
         (error "Proposal has no resolved application to reverse"))
       (let* ((intent (plist-get (mevedel-memory-decision--published workspace previous) :intent))
              (source (plist-get intent :reverse-of))
              (original (if source (mevedel-memory-write-read workspace (car source) (cadr source)) intent))
              (scope (mevedel-memory-write-scope original)))
         (mevedel-memory-scope--root scope (plist-get (plist-get original :item) :root))
         (if (eq (plist-get previous :status) 'reversed) previous
           (mevedel-memory-write-call
            scope (plist-get (plist-get original :item) :root)
            (lambda (target)
              (let ((reverse (mevedel-memory-write-prepare-reverse workspace claim target original)) failed)
                (condition-case nil (mevedel-memory-write-run workspace claim target reverse)
                  ((error quit) (setq failed t)))
                (mevedel-memory-decision--finish-write workspace claim target reverse failed))))))))))

(defun mevedel-memory-decision--reverse-dependents (workspace pass proposal)
  "Restore applied note disposals in PASS that required extraction PROPOSAL.
Signal before PROPOSAL is reversed when a dependent note cannot be restored,
so its extracted knowledge and its source are never both removed."
  (dolist (item (plist-get (mevedel-memory-store-accepted workspace pass) :proposals))
    (when (and (member proposal (plist-get item :requires))
               (memq (plist-get item :action) '(note-delete note-condense))
               (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status)))
      (let ((result (condition-case err
                        (mevedel-memory-decision-reverse workspace pass (plist-get item :id))
                      (error (list :status 'failed :reason (error-message-string err))))))
        (unless (eq (plist-get result :status) 'reversed)
          (user-error "Restoring shared note %s failed (%s); the memory extraction stays applied"
                      (plist-get item :file)
                      (or (plist-get result :reason) (plist-get result :status))))))))

(defun mevedel-memory-decision--recover-write-owned (workspace claim id hash &optional rollback)
  "Reconcile WORKSPACE write ID and HASH under recovered CLAIM.
ROLLBACK permits checked restoration of this unresolved attempt only."
  (let* ((intent (mevedel-memory-write-read workspace id hash))
         (previous (mevedel-memory-decision-status workspace (plist-get intent :proposal))))
    (mevedel-memory-write-call
     (mevedel-memory-write-scope intent) (plist-get (plist-get intent :item) :root)
     (lambda (target)
       (if (and previous (equal hash (plist-get previous :state-hash))
                (mevedel-memory-decision-resolved-write-status-p (plist-get previous :status)))
           (progn (mevedel-memory-write-retire target intent) previous)
         (unless (equal hash (mevedel-session-control-fs-read-file (mevedel-memory-write--pin intent)))
           (error "Memory root marker changed"))
         (if (and (not rollback) previous (equal hash (plist-get previous :state-hash))
                  (eq (plist-get previous :status) 'recovery-required)
                  (memq (mevedel-memory-write-state intent) '(mixed changed unavailable)))
             previous
           (let (failed)
             (when rollback
               (condition-case nil (mevedel-memory-write-rollback workspace claim target intent)
                 ((error quit) (setq failed t))))
             (mevedel-memory-decision--finish-write workspace claim target intent failed)))))
     id)))

(defun mevedel-memory-decision-recover-write (workspace id hash &optional rollback)
  "Reconcile WORKSPACE write ID against HASH without repeating its file writes.
Completed writes become applied decisions; untouched targets become unavailable
attempts.  Mixed or intervening changes remain recovery-required and fenced.
Optional ROLLBACK restores only checked captured originals."
  (mevedel-memory-decision--with-claim
   workspace nil
   (lambda (claim)
     (mevedel-memory-decision--recover-write-owned workspace claim id hash rollback))))

(defun mevedel-memory-decision--recover-writes (workspace &optional ids claim)
  "Reconcile WORKSPACE's marked writes against current roots and live buffers.
This phase stays in the editor; publication recovery must precede it.
IDS restricts reconciliation to those write identities; without IDS, only
`mevedel-memory-write-recovery-candidates' are inspected.  CLAIM, when supplied,
already owns this operation's completed publication recovery."
  (let ((ids (or ids (mevedel-memory-write-recovery-candidates workspace)))
        results)
    (dolist (row (and ids (mevedel-memory-write-list workspace ids)))
      (cond
       ((plist-get row :error)
        (push (list :write-id (plist-get row :id) :status 'unavailable :reason (plist-get row :error)) results))
       ((plist-get row :marked)
        (push (condition-case err
                  (if claim
                      (mevedel-memory-decision--recover-write-owned workspace claim (plist-get row :id) (plist-get row :hash))
                    (mevedel-memory-decision-recover-write workspace (plist-get row :id) (plist-get row :hash)))
                (error (list :write-id (plist-get row :id) :status 'unavailable :reason (error-message-string err))))
              results))))
    (nreverse results)))

(defun mevedel-memory-decision-recover-pending (workspace)
  "Publish accepted WORKSPACE state and reconcile marked writes without inference.
Unmarked intents cannot attribute coincidentally matching external edits.
Live buffers and original target authority participate in reconciliation."
  (when-let* ((key (mevedel-memory-store--claim-directory workspace))
              (claim (mevedel-journal-claim-acquire key 180)))
    (unwind-protect
        (progn (mevedel-memory-decision-recover workspace) (mevedel-memory-store-recover workspace))
      (mevedel-journal-claim-settle claim 'cancelled ""))
    (mevedel-memory-decision--recover-writes workspace)))

(defun mevedel-memory-decision--recover-next-write (workspace job)
  "Inspect one retained write for WORKSPACE JOB, then return to the editor.
Each record is read afresh and reconciliation checks current root authority
and unsaved buffers.  JOB remains registered until all inspections finish."
  (let ((key (plist-get (plist-get job :claim) :directory)) continued)
    (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
      (unwind-protect
          (condition-case err
              (progn
                (when (>= (float-time) (plist-get (plist-get job :claim) :expires-at))
                  (error "Memory recovery timed out"))
                (unless (funcall (plist-get job :currentp))
                  (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                (unless (eq job (gethash key mevedel-memory-decision--recovery-workers))
                  (signal 'mevedel-memory-decision-held '("Recovery was cancelled")))
                (if (plist-get job :writes)
                    (let* ((ids (list (pop (plist-get job :writes))))
                           (row (car (mevedel-memory-write-list workspace ids))))
                      (unless (and (funcall (plist-get job :currentp))
                                   (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                        (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                      (when-let* ((failure (plist-get row :error)))
                        (display-warning 'mevedel (format "Memory write %s is unavailable: %s" (car ids) failure) :warning))
                      ;; This is selection, never authority.  Only marked work
                      ;; needs a new owned recovery operation, which rereads it.
                      (if (not (plist-get row :marked))
                          (plist-put job :timer (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))
                        (setq row nil)
                        ;; Reconciliation records decisions; displays must reread.
                        (mevedel-memory-decision--recovery-changed workspace)
			(with-current-buffer (plist-get job :origin)
                          (plist-put job :operation
                                     (mevedel-memory-decision--start-operation
                                      workspace
                                      (lambda (claim _live _reply)
					(mevedel-memory-decision--recover-writes workspace ids claim))
                                      (lambda (reply)
					(plist-put job :operation nil)
					(when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                                          (cond
                                           ((plist-get reply :error)
                                            (remhash key mevedel-memory-decision--recovery-workers)
                                            (display-warning 'mevedel (plist-get reply :error) :warning))
                                           ((or (plist-get reply :held) (plist-get reply :cancelled))
                                            (remhash key mevedel-memory-decision--recovery-workers))
                                           (t (plist-put job :timer
							 (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))))))
                                      (lambda ()
					(and (funcall (plist-get job :currentp))
                                             (eq job (gethash key mevedel-memory-decision--recovery-workers))
                                             (< (float-time) (plist-get (plist-get job :claim) :expires-at)))) t))))
                      (setq continued t))
                  (unless (and (funcall (plist-get job :currentp))
                               (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                    (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                  (remhash key mevedel-memory-decision--recovery-workers)
                  (when-let* ((callback (plist-get job :callback))
                              (origin (plist-get job :origin))
                              ((buffer-live-p origin)))
                    (with-current-buffer origin (funcall callback)))))
            (mevedel-memory-decision-held nil)
            ((error quit) (display-warning 'mevedel (format "Memory recovery failed: %s" (error-message-string err)) :warning)))
        (when (and (not continued) (eq job (gethash key mevedel-memory-decision--recovery-workers)))
          (remhash key mevedel-memory-decision--recovery-workers))))))

(defun mevedel-memory-decision--recovery-release (job)
  "Release JOB's publication claim before retiring its child.
An unproved fence leaves the original deadline and tracked child intact."
  (unless (plist-get job :released)
    (condition-case err
        (progn
          (mevedel-memory-decision--operation-fence (plist-get job :claim))
          (plist-put job :released t)
          (plist-put job :unsettled nil))
      ((error quit)
       (plist-put job :unsettled t)
       (signal (car err) (cdr err)))))
  (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
  (when-let* ((worker (plist-get job :worker)) ((process-live-p worker)))
    (delete-process worker)))

(defun mevedel-memory-decision--recovery-deadline (workspace job)
  "Finish or retire publication recovery JOB at WORKSPACE's original deadline."
  (unwind-protect
      (unless (plist-get job :result-handled)
        (mevedel-memory-decision--recovered workspace job '(:error "Memory recovery timed out")))
    (when (plist-get job :unsettled)
      (mevedel-memory-decision--operation-retire job))))

(defun mevedel-memory-decision--recovered (workspace job result)
  "Finish JOB's publication recovery from RESULT, then schedule live checks."
  (let* ((claim (plist-get job :claim))
         (key (plist-get claim :directory))
         (changed (plist-get result :changed))
         continued)
    (when (and (eq job (gethash key mevedel-memory-decision--recovery-workers))
               (not (plist-get job :result-handled)))
      (plist-put job :result-handled t)
      (unwind-protect
          (unless mevedel-journal-worker--stopping
            (condition-case err
                (progn
                  (when (plist-get result :error) (error "%s" (plist-get result :error)))
                  (unless (funcall (plist-get job :currentp))
                    (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                  (unless (eq job (gethash key mevedel-memory-decision--recovery-workers))
                    (signal 'mevedel-memory-decision-held '("Recovery was cancelled")))
                  (mevedel-memory-store--assert-owned claim)
                  (when (plist-get result :synchronous)
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                      (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                    (when (mevedel-memory-decision-recover workspace) (setq changed t))
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                      (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                    (when (mevedel-memory-store-recover workspace) (setq changed t))
                    (mevedel-memory-store--assert-owned claim))
                  (mevedel-memory-decision--recovery-release job)
                  (unless (and (funcall (plist-get job :currentp))
                               (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                    (signal 'mevedel-memory-decision-held '("Recovery context changed")))
                  ;; A no-op recovery leaves committed state and its displays current.
                  (when changed (mevedel-memory-decision--recovery-changed workspace))
                  (plist-put job :writes
                             (mevedel-memory-write-recovery-candidates workspace))
                  (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                    (plist-put job :timer
                               (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))
                    (setq continued t)))
              (mevedel-memory-decision-held nil)
              ((error quit) (display-warning 'mevedel
                                      (format "Memory recovery failed: %s" (error-message-string err))
                                      :warning))))
        (when (and (not continued) (eq job (gethash key mevedel-memory-decision--recovery-workers)))
          (remhash key mevedel-memory-decision--recovery-workers))
        (unless (plist-get job :released)
          (condition-case err
              (mevedel-memory-decision--recovery-release job)
            ((error quit) (display-warning 'mevedel
                                    (format "Memory recovery release remains unresolved: %s" (error-message-string err))
                                    :warning))))))))

(defun mevedel-memory-decision--start-recovery (workspace callback origin currentp original-key)
  "Start owned publication recovery for WORKSPACE, CALLBACK and ORIGIN buffer.
CURRENTP validates the scheduled opportunity across admission IO.
ORIGINAL-KEY requires that exact canonical claim path.  Unsupported workers
use synchronous publication recovery under the same claim and deadline."
  (let ((key (mevedel-memory-store--claim-directory workspace)))
    (when-let* (((funcall currentp))
                ((equal key original-key))
                (claim (mevedel-journal-claim-acquire key 180)))
      (let ((job (list :claim claim :callback callback :origin origin :currentp currentp)))
        ;; Claim IO can dispatch cancellation or a successor. Hand the exact
        ;; token to a deadline owner before any further yielding observation.
        (unless (gethash key mevedel-memory-decision--recovery-workers)
          (puthash key job mevedel-memory-decision--recovery-workers))
        (condition-case err
            (progn
              (plist-put job :timer
                         (run-at-time
                          (max 0 (- (plist-get claim :expires-at)
                                    (mevedel-session-control-fs-target-time key))) nil
                          #'mevedel-memory-decision--recovery-deadline workspace job))
              (let ((supported (mevedel-journal-worker-supported-p workspace)))
		(if (and (eq job (gethash key mevedel-memory-decision--recovery-workers))
			 (funcall currentp)
			 (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                    (progn
                      (if supported
                          (mevedel-journal-worker-start
			   workspace 'memory-recover
			   (lambda (result) (mevedel-memory-decision--recovered workspace job result))
			   nil (list :claim claim) job)
			(mevedel-memory-decision--recovered workspace job '(:ok t :synchronous t)))
                      ;; Request-file IO may cancel before the child exists.
                      ;; The generic launcher still hands that child to JOB;
                      ;; retire it here under the owner's already-proved fence.
                      (unless (and (funcall currentp)
                                   (eq job (gethash key mevedel-memory-decision--recovery-workers)))
			(unwind-protect (mevedel-memory-decision--recovery-release job)
                          (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                            (remhash key mevedel-memory-decision--recovery-workers)))))
                  (unwind-protect (mevedel-memory-decision--recovery-release job)
                    (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                      (remhash key mevedel-memory-decision--recovery-workers))))))
          ((error quit)
           (if (eq job (gethash key mevedel-memory-decision--recovery-workers))
               (mevedel-memory-decision--recovered workspace job (list :error (error-message-string err)))
             (mevedel-memory-decision--recovery-release job))))
        job))))

(defun mevedel-memory-decision-admitted-p (key)
  "Return local admitted decision activity for canonical claim directory KEY.
Queued recovery is excluded so it can yield to a pass or drain.  This is a
non-IO scheduling observation, never proof of native claim ownership."
  (or (plist-get (gethash key mevedel-memory-decision--recovery-dispatching) :admitted)
      (gethash key mevedel-memory-decision--recovery-workers)
      (gethash key mevedel-memory-decision--operations)))

(defun mevedel-memory-decision--recovery-blocked-p (key)
  "Return local pass, drain or decision registrations blocking recovery at KEY.
This observation performs no IO; callers observe active cleanup beforehand."
  (or (and (featurep 'mevedel-memory-pass)
           (or (gethash key mevedel-memory-pass--running)
               (gethash key mevedel-memory-pass--drains)))
      (gethash key mevedel-memory-decision--operations)))

(defun mevedel-memory-decision-recovering-p (workspace)
  "Return non-nil while WORKSPACE has queued, dispatching or active recovery.
This is an observation for presentation, never ownership or write admission."
  (let ((key (mevedel-memory-store--claim-directory workspace)))
    (and (or (gethash key mevedel-memory-decision--recovery-pending)
             (gethash key mevedel-memory-decision--recovery-dispatching)
             (gethash key mevedel-memory-decision--recovery-workers)) t)))

(defun mevedel-memory-decision-recovery-generation (workspace)
  "Return how many recoveries changed WORKSPACE's committed memory state.
Displays compare it across a recovery to learn whether their reads are stale.
This is an observation for presentation, never proof of current state."
  (gethash (mevedel-memory-store--claim-directory workspace) mevedel-memory-decision--recovery-generations 0))

(defun mevedel-memory-decision--recovery-changed (workspace)
  "Record that recovery changed WORKSPACE's committed state; drop its observations."
  (cl-incf (gethash (mevedel-memory-store--claim-directory workspace)
                    mevedel-memory-decision--recovery-generations 0))
  (setf (mevedel-workspace-journal-observation workspace) nil
        (mevedel-workspace-memory-observation workspace) nil))

;;;###autoload
(defun mevedel-memory-decision-schedule-recovery (workspace &optional callback)
  "Recover WORKSPACE after returning to the editor, then call CALLBACK.
Local publication scans run in a child; marked write checks use live buffers.
CALLBACK runs only after successful recovery (or when no state exists), allowing
review admission to follow recovery instead of racing its workspace claim."
  (unless mevedel-memory-decision--inhibit-recovery
    (let* ((root (copy-sequence (mevedel-workspace-root workspace)))
           (key (mevedel-memory-store--claim-directory workspace))
           (origin (current-buffer)))
      (unless (or (gethash key mevedel-memory-decision--recovery-workers)
                  (gethash key mevedel-memory-decision--recovery-dispatching))
        (let ((token (list :origin origin)) timer admitted)
          (cl-labels
           ((current ()
              ;; Publication recovery is mode-independent. Preserve its
              ;; original workspace, not an automatic-application actor.
              (and (not mevedel-memory-decision--inhibit-recovery)
                   (not mevedel-journal-worker--stopping)
                   (not (plist-get token :cancelled))
                   (equal root (mevedel-workspace-root workspace))
                   (equal key (mevedel-memory-store--claim-directory workspace))
                   (not mevedel-memory-decision--inhibit-recovery)
                   (not mevedel-journal-worker--stopping)
                   (not (plist-get token :cancelled))
                   (equal root (mevedel-workspace-root workspace))
                   (or admitted (eq token (gethash key mevedel-memory-decision--recovery-dispatching)))))
            (dispatch ()
              (unless (or mevedel-memory-decision--inhibit-recovery (plist-get token :cancelled))
                (puthash key token mevedel-memory-decision--recovery-dispatching)
                (let (deferred)
                  (unwind-protect
                      (condition-case err
                          (let ((present (mevedel-session-control-fs-path-exists-p key))
                                (cleanup (and (featurep 'mevedel-journal-cleanup)
                                              (mevedel-journal-cleanup-active-p workspace))))
                            (when (current)
                              ;; All yielding observations precede these exact
                              ;; registrations and the admission reservation.
                              (cond
                               ((or cleanup (mevedel-memory-decision--recovery-blocked-p key)) (setq deferred t))
                               ((not present)
                                (when (and callback (buffer-live-p origin))
                                  (with-current-buffer origin (funcall callback))))
                               (t
                                (plist-put token :admitted t)
                                (setq admitted (mevedel-memory-decision--start-recovery workspace callback origin #'current key))))))
                        (error (display-warning 'mevedel
                                                (format "Memory recovery failed: %s" (error-message-string err)) :warning)))
                    (when (eq token (gethash key mevedel-memory-decision--recovery-dispatching))
                      (remhash key mevedel-memory-decision--recovery-dispatching)
                      ;; Re-arm the SAME opportunity and captured callback.
                      ;; Pending recovery must not block the drain it yields to.
                      (when (and deferred (not (plist-get token :cancelled))
                                 (not (gethash key mevedel-memory-decision--recovery-pending)))
                        (puthash key timer mevedel-memory-decision--recovery-pending)
                        (timer-set-time timer (time-add nil 1))
                        (timer-activate timer))))))))
           (setq timer (mevedel-transport-schedule-idle
                        mevedel-memory-decision--recovery-pending key 'memory-recovery key #'dispatch))))))))

(defun mevedel-memory-decision-stop-recovery ()
  "Cancel pending recovery and live checks, fence claims, and stop children."
  (mevedel-transport-cancel-idle mevedel-memory-decision--recovery-pending 'memory-recovery)
  (maphash (lambda (key token)
             (plist-put token :cancelled t)
             (when (eq token (gethash key mevedel-memory-decision--recovery-dispatching))
               (remhash key mevedel-memory-decision--recovery-dispatching)))
           mevedel-memory-decision--recovery-dispatching)
  (let (jobs)
    (maphash (lambda (_key job) (push job jobs)) mevedel-memory-decision--recovery-workers)
    (clrhash mevedel-memory-decision--recovery-workers)
    (dolist (job jobs)
      (when-let* ((operation (plist-get job :operation)))
        (mevedel-memory-decision--operation-stop operation))
      (condition-case err
          (mevedel-memory-decision--recovery-release job)
        ((error quit) (display-warning 'mevedel
                                (format "Memory recovery release remains unresolved: %s" (error-message-string err))
                                :warning))))))

(provide 'mevedel-memory-decision)
;;; mevedel-memory-decision.el ends here
