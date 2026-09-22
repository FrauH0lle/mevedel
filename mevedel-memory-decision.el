;;; mevedel-memory-decision.el -- Durable memory proposal decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Decisions are separate immutable journal records. A workspace claim accepts
;; their private record hash before public publication, allowing replay after
;; process death. Rejection never edits a memory target. Private proposal text
;; enters later review evidence only under its captured root authority.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-worker)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)
(require 'mevedel-transport)

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

(defvar mevedel-memory-decision--recovery-pending (make-hash-table :test #'equal)
  "Coalesced activation recovery timers, keyed by workspace claim directory.")

(defvar mevedel-memory-decision--recovery-workers (make-hash-table :test #'equal)
  "Active publication recovery and live-write checks, keyed by claim directory.")

(defun mevedel-memory-decision--directory (workspace)
  "Return the private decision directory for WORKSPACE."
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "decisions"))

(defun mevedel-memory-decision-status (workspace proposal)
  "Return the latest accepted public decision for PROPOSAL in WORKSPACE.
Verify its immutable acceptance evidence; public text alone cannot authorize
a decision. Recover accepted decisions before using this to decide or apply."
  (let ((generation 0) (passes (make-hash-table :test #'equal)) latest)
    (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
      (when (and (eq (plist-get entry :kind) 'decision) (equal proposal (plist-get entry :proposal-id))
                 (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get entry :pass-id))))
        (let* ((accepted (mevedel-memory-decision--published workspace entry passes))
               (number (plist-get (plist-get accepted :claim) :generation)))
          (when (> number generation) (setq generation number latest entry)))))
    latest))

(defun mevedel-memory-decision--published (workspace entry &optional passes)
  "Return accepted private state matching public decision ENTRY in WORKSPACE.
PASSES is the current read observation described by the private reader."
  (let* ((path (file-name-concat (mevedel-memory-decision--directory workspace)
                                (concat (plist-get entry :decision-id) ".el")))
         (accepted (mevedel-memory-decision--read workspace path passes))
         (metadata (plist-get accepted :metadata)))
    (unless (and accepted
                 (cl-every (lambda (field) (equal (plist-get entry (cdr field)) (plist-get metadata (cdr field))))
                           mevedel-journal-store--decision-fields))
      (error "Published decision has no matching acceptance evidence"))
    accepted))

(defun mevedel-memory-decision--record (workspace path)
  "Read authenticated decision metadata at PATH in WORKSPACE, or nil.
Verify immutable claim acceptance independently of the retained proposal body.
The full reader also verifies that body's dependencies before using a decision."
  (let* ((record (mevedel-memory-store--read-lisp path))
         (data (car record)) (text (cdr record))
         (claim (append (list :directory (mevedel-memory-store--claim-directory workspace)) (plist-get data :claim)))
         (evidence (mevedel-session-control-fs-run-program
                    (list (list :op 'read :path (mevedel-journal-claim--path claim nil))
                          (list :op 'read :path (mevedel-journal-claim--path claim t) :optional t))))
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
      (list :claim claim :metadata metadata :write-id (plist-get data :write-id)
            :hash (secure-hash 'sha256 text)))))

(defun mevedel-memory-decision--read (workspace path &optional passes)
  "Read an accepted private decision at PATH in WORKSPACE, or nil.
Unaccepted and retired decisions cannot publish. Verify surviving decisions'
original accepted proposal bundle and retained write evidence.
PASSES, when non-nil, holds the last authenticated bundle for this WORKSPACE
read observation. Reuse immutable pass proofs within a scan; callers
must discard the table after mutations and never retain it between operations."
  (when-let* ((data (mevedel-memory-decision--record workspace path))
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
           (intent (and (plist-get data :write-id)
                        (mevedel-memory-write-read workspace (plist-get data :write-id) (plist-get metadata :state-hash)))))
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
         (when (cl-every
                (lambda (id)
                  (mevedel-memory-decision-terminal-status-p (plist-get (mevedel-memory-decision-status workspace id) :status)))
                (plist-get (plist-get accepted :review) :proposals))
           (mevedel-memory-store--release-pins workspace prepared nil mutation))
         (setf (mevedel-workspace-journal-observation workspace) nil)
         (mevedel-journal-cleanup-schedule workspace t)
         published)))))

(defun mevedel-memory-decision-recover (workspace)
  "Publish all accepted pending decisions in WORKSPACE without inference.
Malformed accepted state remains an error, preserving evidence for inspection.
Reuse one public observation; already published history needs only one pending
evidence-release check per pass, not repeated republication per decision."
  (let ((published (make-hash-table :test #'equal)) (checked (make-hash-table :test #'equal))
        (passes (make-hash-table :test #'equal)))
    (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
      (puthash (plist-get entry :file) (plist-get entry :text) published))
    (dolist (path (mevedel-session-control-fs-list-directory
                  (mevedel-memory-decision--directory workspace) mevedel-memory-write-intent-file-regexp))
      (when-let* ((decision (mevedel-memory-decision--read workspace path passes)))
        (let* ((metadata (plist-get decision :metadata))
               (prepared (plist-get (plist-get decision :accepted) :prepared))
               (review (plist-get (plist-get decision :accepted) :review))
               (review-file (mevedel-journal-store--filename review 'consolidation))
               (review-text (mevedel-journal-store--encode review nil 'consolidation))
               (pass (plist-get prepared :id))
               (file (mevedel-journal-store--filename metadata 'decision))
               (expected (mevedel-journal-store--encode metadata nil 'decision))
               (release (and (not (gethash pass checked))
                             (seq-some (lambda (entry)
                                         (mevedel-session-control-fs-path-exists-p
                                          (mevedel-memory-store--pin workspace pass (plist-get entry :id))))
                                       (plist-get prepared :entries)))))
          (puthash pass t checked)
          (when (or release (not (equal expected (gethash file published)))
                    (not (equal review-text (gethash review-file published))))
            (mevedel-memory-decision--publish workspace decision)
            (clrhash passes)
            (puthash file expected published)
            (puthash review-file review-text published)))))))

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
                   (error "Memory consolidation is busy"))))
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
         (reason (pcase status
                   ('applied (if reverse "Reversal left the original application unchanged." "Applied captured memory proposal."))
                   ('reversed "Reversed the captured memory application.")
                   ('unavailable (if failed "Attempt left targets unchanged." "Application was interrupted before completion."))
                   (_ "Targets need checked recovery.")))
         (decision (mevedel-memory-decision--accept workspace claim (plist-get intent :accepted) (plist-get intent :item)
                                                     status reason intent))
         (published (mevedel-memory-decision--publish workspace decision)))
    (unless (eq status 'recovery-required) (mevedel-memory-write-retire target intent))
    published))

(defun mevedel-memory-decision--application-input (workspace accepted proposal)
  "Return (SCOPE . PROPOSAL) with confirmed same-pass index edits in WORKSPACE.
Only confirmed applied or reversed writes advance the index expectation. Never
adopt current filesystem bytes as a baseline or mutate ACCEPTED's original data."
  (let* ((scope (copy-tree (plist-get (plist-get accepted :prepared) :scope)))
         (item (copy-tree proposal))
         (index (assoc "MEMORY.md" (plist-get item :before)))
         (pass (plist-get (plist-get accepted :prepared) :id))
         decisions)
    (when index
      (dolist (entry (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (when (and (eq (plist-get entry :kind) 'decision)
                   (mevedel-memory-decision-written-status-p (plist-get entry :status))
                   (equal pass (plist-get entry :pass-id)))
          (let* ((decision (mevedel-memory-decision--published workspace entry))
                 (intent (plist-get decision :intent)))
            (when (and (equal (plist-get proposal :root) (plist-get (plist-get intent :item) :root))
                       (if (plist-get intent :reverse-of) (eq (plist-get entry :status) 'reversed)
                         (eq (plist-get entry :status) 'applied)))
              (push decision decisions)))))
      (dolist (decision (sort decisions (lambda (a b) (< (plist-get (plist-get a :claim) :generation)
                                                         (plist-get (plist-get b :claim) :generation)))))
        (let* ((intent (plist-get decision :intent))
               (before (cdr (assoc "MEMORY.md" (plist-get intent :before))))
               (after (cdr (assoc "MEMORY.md" (plist-get intent :after)))))
          (unless (mevedel-tool-patch--same-snapshot-p (cdr index) before)
            (error "Confirmed memory index history is discontinuous"))
          (setcdr index (copy-tree after))))
      (let ((root (cdr (assoc (plist-get item :root) (plist-get scope :roots)))))
        (setf (alist-get "MEMORY.md" (plist-get root :before) nil nil #'equal) (cdr index))))
    (cons scope item)))

(defun mevedel-memory-decision-apply (workspace pass proposal &optional on-applied)
  "Apply accepted PROPOSAL from PASS in WORKSPACE with durable write intent.
Acquire both workspace and original-root ownership. Stale targets produce an
immutable decision without writes. A partial or changed result retains its
root marker and before/after evidence for explicit checked recovery.
ON-APPLIED, when non-nil, receives the changed paths only for a newly confirmed
application by this call. Repeated terminal decisions do not notify it."
  (mevedel-memory-decision--with-claim
   workspace t
   (lambda (claim)
     (let* ((accepted (mevedel-memory-store-accepted workspace pass))
            (item (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals)))
            (scope (plist-get (plist-get accepted :prepared) :scope))
            (previous (mevedel-memory-decision-status workspace proposal)))
       (unless item (error "Unknown accepted proposal"))
       (mevedel-memory-scope--root scope (plist-get item :root))
       (if (mevedel-memory-decision-terminal-status-p (plist-get previous :status)) previous
         (mevedel-memory-write-call
          scope (plist-get item :root)
          (lambda (target)
            (let* ((input (mevedel-memory-decision--application-input workspace accepted item))
                   (scope (car input)) (effective (cdr input))
                   (fresh (mevedel-memory-scope-check scope (plist-get item :root) (mapcar #'car (plist-get item :before)))))
              (if (not (eq (plist-get fresh :status) 'fresh))
                  (mevedel-memory-decision--publish
                   workspace (mevedel-memory-decision--accept workspace claim accepted item (plist-get fresh :status)
                                                               "Targets changed or are unavailable since capture."))
                (let ((intent (mevedel-memory-write-prepare workspace claim target accepted effective scope)) failed)
                  (condition-case nil (mevedel-memory-write-run workspace claim target intent)
                    ((error quit) (setq failed t)))
                  (let ((decision (mevedel-memory-decision--finish-write workspace claim target intent failed)))
                    (when (and on-applied (eq (plist-get decision :status) 'applied))
                      (funcall on-applied (mapcar (lambda (change) (plist-get change :path))
                                                 (plist-get intent :changes))))
                    decision)))))))))))

(defun mevedel-memory-decision-reverse (workspace pass proposal)
  "Reverse the accepted application of PROPOSAL from PASS in WORKSPACE.
Require every target to match that application's exact after-state, including
its shared index. Persist a separate reverse intent before writes and retain
all prior decisions. Repeated reversal returns the same terminal result."
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

(defun mevedel-memory-decision-recover-write (workspace id hash &optional rollback)
  "Reconcile WORKSPACE write ID against HASH without repeating its file writes.
Completed writes become applied decisions; untouched targets become unavailable
attempts. Mixed or intervening changes remain recovery-required and fenced.
Optional ROLLBACK explicitly restores an unresolved attempt's captured originals
only when every dependency still matches its recorded before or after state."
  (mevedel-memory-decision--with-claim
   workspace nil
   (lambda (claim)
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
        id)))))

(defun mevedel-memory-decision--recover-writes (workspace &optional ids)
  "Reconcile WORKSPACE's marked writes against current roots and live buffers.
This phase stays in the editor; publication recovery must precede it.
IDS restricts reconciliation to those write identities."
  (let (results)
    (dolist (row (mevedel-memory-write-list workspace ids))
      (cond
       ((plist-get row :error)
        (push (list :write-id (plist-get row :id) :status 'unavailable :reason (plist-get row :error)) results))
       ((plist-get row :marked)
        (push (condition-case err
                  (mevedel-memory-decision-recover-write workspace (plist-get row :id) (plist-get row :hash))
                (error (list :write-id (plist-get row :id) :status 'unavailable :reason (error-message-string err))))
              results))))
    (nreverse results)))

(defun mevedel-memory-decision-recover-pending (workspace)
  "Publish accepted WORKSPACE state and reconcile marked writes without inference.
Unmarked intents cannot attribute coincidentally matching external edits.
Live buffers and original target authority participate in reconciliation."
  (when-let* ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
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
                (if (plist-get job :writes)
                    (progn
                      (mevedel-memory-decision--recover-writes workspace (list (pop (plist-get job :writes))))
                      (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                        (plist-put job :timer
                                   (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))
                        (setq continued t)))
                  (remhash key mevedel-memory-decision--recovery-workers)
                  (when-let* ((callback (plist-get job :callback))
                              (origin (plist-get job :origin))
                              ((buffer-live-p origin)))
                    (with-current-buffer origin (funcall callback)))))
            (error (display-warning 'mevedel (format "Memory recovery failed: %s" (error-message-string err)) :warning)))
        (when (and (not continued) (eq job (gethash key mevedel-memory-decision--recovery-workers)))
          (remhash key mevedel-memory-decision--recovery-workers))))))

(defun mevedel-memory-decision--recovered (workspace job result)
  "Finish JOB's publication recovery from RESULT, then schedule live checks."
  (let* ((claim (plist-get job :claim))
         (key (plist-get claim :directory))
         continued)
    (when (and (eq job (gethash key mevedel-memory-decision--recovery-workers))
               (not (plist-get job :result-handled)))
      (plist-put job :result-handled t)
      (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
      (when-let* ((worker (plist-get job :worker)))
        (when (process-live-p worker) (delete-process worker)))
      (unwind-protect
          (unless mevedel-journal-worker--stopping
            (condition-case err
                (progn
                  (when (plist-get result :error) (error "%s" (plist-get result :error)))
                  (mevedel-memory-store--assert-owned claim)
                  (mevedel-journal-claim-settle claim 'cancelled "")
                  (plist-put job :released t)
                  (setf (mevedel-workspace-journal-observation workspace) nil
                        (mevedel-workspace-memory-observation workspace) nil)
                  (plist-put job :writes
                             (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                                     (mevedel-session-control-fs-list-directory
                                      (mevedel-memory-write--directory workspace)
                                      mevedel-memory-write-intent-file-regexp)))
                  (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                    (plist-put job :timer
                               (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))
                    (setq continued t)))
              (error (display-warning 'mevedel
                                      (format "Memory recovery failed: %s" (error-message-string err))
                                      :warning))))
        (when (and (not continued) (eq job (gethash key mevedel-memory-decision--recovery-workers)))
          (remhash key mevedel-memory-decision--recovery-workers))
        (unless (plist-get job :released) (mevedel-journal-claim-settle claim 'cancelled ""))))))

(defun mevedel-memory-decision--start-recovery (workspace callback origin)
  "Start local publication recovery for WORKSPACE, CALLBACK and ORIGIN buffer."
  (let ((key (mevedel-memory-store--claim-directory workspace)))
    (when-let* ((claim (mevedel-journal-claim-acquire key 180)))
      (let ((job (list :claim claim :callback callback :origin origin)))
        (puthash key job mevedel-memory-decision--recovery-workers)
        (condition-case err
            (progn
              (plist-put job :timer
                         (run-at-time 180 nil #'mevedel-memory-decision--recovered workspace job
                                      '(:error "Memory recovery timed out")))
              (plist-put job :worker
                         (mevedel-journal-worker-start
                          workspace 'memory-recover
                          (lambda (result) (mevedel-memory-decision--recovered workspace job result))
                          nil (list :claim claim))))
          (error (mevedel-memory-decision--recovered workspace job
                                                     (list :error (error-message-string err)))))
        job))))

;;;###autoload
(defun mevedel-memory-decision-schedule-recovery (workspace &optional callback)
  "Recover WORKSPACE after returning to the editor, then call CALLBACK.
Local publication scans run in a child; marked write checks use live buffers.
CALLBACK runs only after successful recovery (or when no state exists), allowing
review admission to follow recovery instead of racing its workspace claim."
  (unless mevedel-memory-decision--inhibit-recovery
    (let ((key (mevedel-memory-store--claim-directory workspace))
          (origin (current-buffer)))
      (unless (gethash key mevedel-memory-decision--recovery-workers)
        (mevedel-transport-schedule-idle
         mevedel-memory-decision--recovery-pending key 'memory-recovery key
         (lambda ()
           (unless mevedel-memory-decision--inhibit-recovery
             (condition-case err
                 (cond
                  ((not (mevedel-session-control-fs-path-exists-p key))
                   (when (and callback (buffer-live-p origin))
                     (with-current-buffer origin (funcall callback))))
                  ((mevedel-journal-worker-supported-p workspace)
                   (mevedel-memory-decision--start-recovery workspace callback origin))
                  (t
                   (mevedel-memory-decision-recover-pending workspace)
                   (when (and callback (buffer-live-p origin))
                     (with-current-buffer origin (funcall callback)))))
               (error (display-warning 'mevedel
                                       (format "Memory recovery failed: %s" (error-message-string err))
                                       :warning))))))))))

(defun mevedel-memory-decision-stop-recovery ()
  "Cancel pending recovery and live checks, fence claims, and stop children."
  (mevedel-transport-cancel-idle mevedel-memory-decision--recovery-pending 'memory-recovery)
  (let (jobs)
    (maphash (lambda (_key job) (push job jobs)) mevedel-memory-decision--recovery-workers)
    (clrhash mevedel-memory-decision--recovery-workers)
    (dolist (job jobs)
      (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
      (unwind-protect
          (unless (plist-get job :released)
            (mevedel-journal-claim-settle (plist-get job :claim) 'cancelled ""))
        (when-let* ((worker (plist-get job :worker)))
          (when (process-live-p worker) (delete-process worker)))))))

(provide 'mevedel-memory-decision)
;;; mevedel-memory-decision.el ends here
