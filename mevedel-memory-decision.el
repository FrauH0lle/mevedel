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
(require 'mevedel-utilities)
(require 'mevedel-workspace-identity)

;; `mevedel-journal-cleanup'
(declare-function mevedel-journal-cleanup-active-p "mevedel-journal-cleanup" (workspace))
(declare-function mevedel-journal-cleanup-wait "mevedel-journal-cleanup" (workspace))
(autoload 'mevedel-journal-cleanup-wait "mevedel-journal-cleanup")

;; `mevedel-memory-notes'
(declare-function mevedel-memory-notes-in-use-p "mevedel-memory-notes" (workspace relative))
(declare-function mevedel-memory-notes-mutation-hold "mevedel-memory-notes" (workspace root relative &optional captured-scope))
(declare-function mevedel-memory-notes-passes "mevedel-memory-notes" (workspace entries))
(declare-function mevedel-memory-notes-record-hold "mevedel-memory-notes" (workspace directory relative reason))
(autoload 'mevedel-memory-notes-in-use-p "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-mutation-hold "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-passes "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-record-hold "mevedel-memory-notes")

;; `mevedel-memory-pass'
(defvar mevedel-memory-pass--drains)
(defvar mevedel-memory-pass--running)

;; `mevedel-telemetry'
(declare-function mevedel-telemetry--monotonic-now "mevedel-telemetry" ())
(autoload 'mevedel-telemetry--monotonic-now "mevedel-telemetry")

;; Both pass scheduling and the decision admission boundary read this policy.
;; Keep it available when the decision API is used without loading the pass.
(defcustom mevedel-memory-consolidation-mode 'propose
  "How memory consolidation is scheduled and proposals are handled.
Manual runs only on request.  Propose and auto offer a daily finite shared-note
cohort at digest publication, completed root turns, session opening and idle
maintenance.  Propose leaves decisions for approval; auto applies fresh memory
and instruction proposals, including `AGENTS.md', through the checked path.
Journal-only review retains its separate sparse-digest and elapsed-time gates."
  :type '(choice (const manual) (const propose) (const auto))
  :group 'mevedel)

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

(defvar mevedel-memory-decision--publication-job nil
  "Local automatic operation deferring only its accepted decision publication.")

(defun mevedel-memory-decision--directory (workspace)
  "Return the private decision directory for WORKSPACE."
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "decisions"))

(defun mevedel-memory-decision-status (workspace proposal &optional entries passes writes records)
  "Return the latest accepted public decision for PROPOSAL in WORKSPACE.
Verify its immutable acceptance evidence; public text alone cannot authorize
a decision. Recover accepted decisions before using this to decide or apply.
ENTRIES, PASSES and WRITES, when non-nil, are one current read observation,
shared across a scan as `mevedel-memory-decision--read' describes.
RECORDS may hold this scan's batched decision metadata, keyed by path."
  (let ((generation 0) (passes (or passes (make-hash-table :test #'equal))) latest)
    (dolist (entry (or entries (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
      (when (and (eq (plist-get entry :kind) 'decision) (equal proposal (plist-get entry :proposal-id))
                 (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get entry :pass-id))))
        (let* ((accepted (mevedel-memory-decision--published workspace entry passes writes records))
               (number (plist-get (plist-get accepted :claim) :generation)))
          (when (> number generation) (setq generation number latest entry)))))
    latest))

(defun mevedel-memory-decision--published (workspace entry &optional passes writes records)
  "Return accepted private state matching public decision ENTRY in WORKSPACE.
PASSES, WRITES and RECORDS are the current observation described by the readers."
  (let* ((path (file-name-concat (mevedel-memory-decision--directory workspace)
                                (concat (plist-get entry :decision-id) ".el")))
         (record (and records (gethash path records)))
         (accepted (progn
                     (when (and records (not record)) (error "Unavailable decision record"))
                     (when (stringp record) (error "%s" record))
                     (when (and record
                                (not (equal (plist-get entry :pass-id) (plist-get (plist-get record :metadata) :pass-id))))
                       (error "Published decision source pass changed"))
                     (mevedel-memory-decision--read workspace path passes writes record)))
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
                         (let ((bundle (mevedel-memory-store-accepted workspace pass t)))
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
                          (mevedel-memory-write--read workspace (plist-get data :write-id) (plist-get metadata :state-hash)
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


(defun mevedel-memory-decision--settled-p (workspace accepted &optional passes)
  "Return non-nil when every proposal of ACCEPTED is terminal in WORKSPACE.
Read the public journal once for the whole pass.  An incomplete terminal
index can only refuse release; a complete index still needs private proofs.
PASSES may hold bundles authenticated in the caller's current owned phase."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (passes (or passes (make-hash-table :test #'equal)))
        (terminal (make-hash-table :test #'equal))
        (proposals (plist-get (plist-get accepted :review) :proposals)))
    ;; Publication calls this after each decision.  Authenticating a growing
    ;; prefix before discovering the next absent proposal is quadratic IO.
    ;; Public metadata cannot prove settlement, but absence can refuse it.
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision)
                 (mevedel-memory-decision-terminal-status-p (plist-get entry :status)))
        (puthash (plist-get entry :proposal-id) t terminal)))
    (and (cl-every (lambda (id) (gethash id terminal)) proposals)
         (cl-every (lambda (id)
                     (mevedel-memory-decision-terminal-status-p
                      (plist-get (mevedel-memory-decision-status workspace id entries passes) :status)))
                   proposals))))

(defun mevedel-memory-decision--publish (workspace id hash)
  "Publish WORKSPACE's accepted decision ID with record HASH in one mutation.
Reauthenticate the decision and its proposal bundle under the journal mutation
claim.  Verify the pass review's exact public bytes and republish it when they
are absent or different.  Release the pass's digest pins only after every
proposal has an authenticated terminal decision; public entries can only refuse
release.  Return the published decision metadata."
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((root (mevedel-workspace-root workspace))
            (decision (mevedel-memory-decision--read
                       workspace (file-name-concat (mevedel-memory-decision--directory workspace) (concat id ".el")))))
       (unless (and decision (equal hash (plist-get decision :hash)))
         (error "Accepted decision publication changed"))
       (let* ((accepted (plist-get decision :accepted))
              (prepared (plist-get accepted :prepared))
              (review (plist-get accepted :review))
              (journal (mevedel-journal-store-directory root))
              (observed (car (mevedel-session-control-fs-run-program
                              (list (list :op 'read :optional t :coding 'utf-8-unix
                                          :path (file-name-concat
                                                 journal (mevedel-journal-store--filename review 'consolidation))))))))
         (unless (and (eq (plist-get observed :status) 'ok)
                      (equal (plist-get observed :value) (mevedel-journal-store--encode review nil 'consolidation)))
           (mevedel-memory-store--publish-owned workspace accepted mutation))
         (mevedel-memory-store--assert-owned mutation)
         (let ((published (mevedel-journal-store-publish-decision root (plist-get decision :metadata)))
               (passes (make-hash-table :test #'equal)))
           ;; This bundle was authenticated under the mutation claim held here.
           (puthash (plist-get prepared :id) accepted passes)
           (when (and (plist-get prepared :entries)
                      (seq-some #'identity
                                (mevedel-session-control-fs-paths-exist
                                 (mapcar (lambda (entry)
                                           (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id)))
                                         (plist-get prepared :entries))))
                      (mevedel-memory-decision--settled-p workspace accepted passes))
             (mevedel-memory-store--release-pins workspace prepared nil mutation))
           (setf (mevedel-workspace-journal-observation workspace) nil)
           (mevedel-journal-cleanup-schedule workspace t)
           published))))))

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
               (mevedel-memory-decision--publish workspace (plist-get metadata :decision-id) (plist-get decision :hash))
               (setq changed t records-current nil)
               (clrhash passes)
               (puthash file expected published)
               (puthash review-file review-text published))))))
     groups)
    changed))

(defun mevedel-memory-decision--accept (workspace job accepted proposal status reason &optional intent)
  "Accept STATUS and REASON for PROPOSAL from ACCEPTED under WORKSPACE JOB's claim.
INTENT retains exact before/after evidence when files may have been written.
ACCEPTED was authenticated in this continuously owned phase.  Settling the
claim ends that phase: record the outcome in JOB and return the accepted
decision built from the exact bytes written here.  Publication reauthenticates."
  (let* ((claim (plist-get job :claim))
         (prepared (plist-get accepted :prepared))
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
         text outcome)
    (mevedel-journal-store--validate-metadata metadata 'decision)
    (unless (member proposal (plist-get accepted :proposals))
      (error "Proposal is not in the accepted pass"))
    (mevedel-memory-scope--root (plist-get prepared :scope) (plist-get proposal :root))
    (setq text (mevedel-memory-store--print (list :claim (mevedel-journal-claim--record claim) :metadata metadata :write-id (plist-get intent :id))))
    (mevedel-session-control-fs-make-directory directory t)
    ;; Settlement's own election verifies the claim bytes and deadline.
    (unless (mevedel-session-control-fs-create-file path text) (error "Decision already exists"))
    (unless (setq outcome (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text)))
      (error "Decision lost ownership before acceptance"))
    (plist-put job :outcome outcome)
    (list :claim claim :metadata metadata :accepted accepted :intent intent
          :hash (secure-hash 'sha256 text))))

(defun mevedel-memory-decision--decided (workspace job accepted proposal status reason &optional intent)
  "Accept and publish STATUS for PROPOSAL; return its public metadata.
WORKSPACE, JOB, ACCEPTED, REASON and INTENT are as for
`mevedel-memory-decision--accept'.  A local automatic operation defers
publication to its child, passing only identity and hash."
  (mevedel-memory-decision--publish-accepted
   workspace (mevedel-memory-decision--accept workspace job accepted proposal status reason intent)))

(defun mevedel-memory-decision--publish-accepted (workspace decision)
  "Publish accepted DECISION in WORKSPACE and return its public metadata.
A local automatic operation defers publication to its child, passing only
identity and hash, never accepted proofs."
  (let ((id (plist-get (plist-get decision :metadata) :decision-id)))
    (if (and mevedel-memory-decision--publication-job
             (equal id (plist-get (plist-get mevedel-memory-decision--publication-job :claim) :owner)))
        (progn
          (plist-put mevedel-memory-decision--publication-job :publication
                     (list :decision-id id :hash (plist-get decision :hash)))
          (plist-get decision :metadata))
      (mevedel-memory-decision--publish workspace id (plist-get decision :hash)))))

;;
;;; Decision transactions

(define-error 'mevedel-memory-decision--held "Memory application is held")
(define-error 'mevedel-memory-decision-interrupted "Memory application interrupted" 'quit)

(defconst mevedel-memory-decision-automatic-idle-seconds 1
  "Editor idleness required before each child-assisted automatic decision.")

(defconst mevedel-memory-decision--context-keys
  '(:workspace :root :origin :workspace-id :origin-session :origin-workspace :origin-root :automatic)
  "Context fields a continuation must match exactly: one actor and origin.")

(defun mevedel-memory-decision--context (workspace origin &optional automatic)
  "Capture WORKSPACE and ORIGIN's expected context, not filesystem authority.
AUTOMATIC marks an automatic actor and adds its shared cancellation token."
  (let* ((session (buffer-local-value 'mevedel--session origin))
         (other (and session (mevedel-session-workspace session))))
    (append (list :workspace workspace :root (mevedel-workspace-root workspace) :origin origin
                  :workspace-id (mevedel-workspace-identity-read (mevedel-workspace-root workspace))
                  :origin-session session :origin-workspace other
                  :origin-root (and other (mevedel-workspace-root other)))
            (when automatic (list :automatic t :token (list :cancelled nil :operation nil))))))

(defun mevedel-memory-decision-automatic-context (workspace origin)
  "Return an opaque automatic-decision binding of WORKSPACE to ORIGIN buffer.
This is an origin binding, not frozen permission: every automatic decision
rechecks ORIGIN's live session, workspace identity and `auto' mode at
admission and before each forward write.  Pass it as the AUTOMATIC argument of
the decision verbs; `mevedel-memory-decision-cancel' stops work started with it."
  (mevedel-memory-decision--context workspace origin t))

(defun mevedel-memory-decision--operation-origin-current-p (job)
  "Return non-nil while JOB's live origin and workspace still match its context."
  (let ((origin (plist-get job :origin)) (other (plist-get job :origin-workspace)))
    (and (buffer-live-p origin)
         (equal (plist-get job :root) (mevedel-workspace-root (plist-get job :workspace)))
         (eq (plist-get job :origin-session) (buffer-local-value 'mevedel--session origin))
         (or (null (plist-get job :origin-session))
             (and (eq other (mevedel-session-workspace (plist-get job :origin-session)))
                  (equal (plist-get job :origin-root) (and other (mevedel-workspace-root other))))))))

(defun mevedel-memory-decision-automatic-current-p (context)
  "Return non-nil while automatic CONTEXT's original actor still admits writes.
Stored identity is an expectation, not authority; read it afresh on each check.
A cancelled CONTEXT is never current."
  (cl-flet ((actor ()
              (and (not (plist-get (plist-get context :token) :cancelled))
                   (not mevedel-journal-worker--stopping)
                   (mevedel-memory-decision--operation-origin-current-p context)
                   (eq (buffer-local-value 'mevedel-memory-consolidation-mode (plist-get context :origin)) 'auto))))
    (and (actor)
         (let ((mevedel-workspace-identity--cache (make-hash-table :test #'equal)))
           (equal (plist-get context :workspace-id)
                  (mevedel-workspace-identity-read (plist-get context :root))))
         ;; Identity IO may dispatch events; never reuse the actor check from
         ;; before that yield to admit a dead, reassigned or cancelled origin.
         (actor))))

(defun mevedel-memory-decision-cancel (context)
  "Cancel automatic decisions started with CONTEXT.
Later forward writes refuse; an executing transaction unwinds through checked
rollback first.  Return non-nil when an operation will still deliver its
callback later, nil when nothing remains to deliver."
  (let ((token (plist-get context :token)))
    (plist-put token :cancelled t)
    (when-let* ((operation (plist-get token :operation)))
      (mevedel-memory-decision--operation-stop operation)
      (and (not (plist-get operation :done)) (plist-get operation :executing) t))))

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
                         mevedel-memory-decision--context-keys))
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

(defun mevedel-memory-decision--continuation (job outcome status)
  "Return the opaque continuation after JOB's clean OUTCOME with STATUS, or nil.
Carry only the exact settled claim and actor context, never accepted bundles,
public observations or target proofs.  The next claim must freshly prove direct
succession before it omits historical recovery.  Held, waiting and idle
admissions, failures and recovery-required writes reset continuity."
  (when (and outcome (not (plist-get job :decisions-only))
             (memq (plist-get outcome :status) '(completed cancelled))
             (memq status '(nil skipped applied rejected reversed stale unavailable)))
    (append (list :claim (copy-tree (plist-get job :claim)) :outcome (copy-tree outcome))
            (cl-loop for key in mevedel-memory-decision--context-keys
                     append (list key (plist-get job key))))))

(defun mevedel-memory-decision--snapshot (value)
  "Copy inert decision VALUE, including mutable strings and list tails."
  (cond ((stringp value) (copy-sequence value))
        ((consp value)
         (let ((tail value) result)
           (while (consp tail)
             (push (mevedel-memory-decision--snapshot (car tail)) result)
             (setq tail (cdr tail)))
           (nconc (nreverse result) (mevedel-memory-decision--snapshot tail))))
        (t value)))

(defun mevedel-memory-decision--result (value &optional continuation)
  "Return the public result for internal decision VALUE and CONTINUATION.
VALUE is a public decision entry or metadata, a status plist, or :items.
The result's :decision is a detached copy of that public decision."
  (let ((metadata (and (plist-member value :decision-id) (mevedel-memory-decision--snapshot value))))
    (append (list :status (plist-get value :status) :reason (plist-get value :reason)
                  :decision metadata :continuation continuation)
            (when (plist-member value :items) (list :items (plist-get value :items))))))

(defun mevedel-memory-decision--execute (workspace context continuation decisions-only function)
  "Run FUNCTION as one synchronous owned decision transaction in WORKSPACE.
CONTEXT identifies the actor.  Acquire the workspace claim, then recover
accepted decisions and, unless DECISIONS-ONLY, pass publications, except when
CONTINUATION freshly proves direct succession.  FUNCTION receives the job
plist holding :claim; it settles durable outcomes itself.  Return the public
result with a new continuation, or signal."
  (let* ((key (mevedel-memory-store--claim-directory workspace))
         (claim (or (mevedel-journal-claim-acquire key 180)
                    (progn
                      (mevedel-journal-cleanup-wait workspace)
                      (mevedel-journal-claim-acquire key 180))
                    (user-error "Memory consolidation is busy")))
         (job (append (list :claim claim :decisions-only decisions-only :outcome nil)
                      (copy-sequence context)))
         value)
    (unwind-protect
        (progn
          (unless (mevedel-memory-decision--recovery-successor-p job continuation)
            (mevedel-memory-decision-recover workspace)
            (unless decisions-only (mevedel-memory-store-recover workspace)))
          (setq value (funcall function job)))
      (unless (plist-get job :outcome)
        (plist-put job :outcome (mevedel-journal-claim-settle claim 'cancelled ""))))
    (mevedel-memory-decision--result
     value (mevedel-memory-decision--continuation job (plist-get job :outcome) (plist-get value :status)))))

(defun mevedel-memory-decision--finish-write (workspace job target intent &optional failed)
  "Record INTENT's observed outcome under WORKSPACE JOB's claim and TARGET.
FAILED says execution did not finish normally. Mixed, changed, or unavailable
targets retain the root marker for checked recovery, never blind reapplication."
  (let* ((state (mevedel-memory-write--state intent))
         (reverse (plist-get intent :reverse-of))
         (status (cond ((or (eq state 'after) (and (eq state 'before) (not (plist-get intent :changes))))
                        (if reverse 'reversed 'applied))
                       ((eq state 'before) (if reverse 'applied 'unavailable))
                       (t 'recovery-required)))
         (note (mevedel-memory-proposal-note-action-p (plist-get (plist-get intent :item) :action)))
         (reason (pcase status
                   ('applied (cond (reverse "Reversal left the original application unchanged.")
                                   (note "Permanently deleted the reviewed shared file; no undo copy was kept.")
                                   (t "Applied captured memory proposal.")))
                   ('reversed "Reversed the captured memory application.")
                   ('unavailable (if failed "Attempt left targets unchanged." "Application left targets unchanged."))
                   (_ "Targets need checked recovery.")))
         (decision (mevedel-memory-decision--accept workspace job (plist-get intent :accepted) (plist-get intent :item)
                                                     status reason intent)))
    ;; Retire the marker while the target is still owned, then publish.
    (unless (eq status 'recovery-required) (mevedel-memory-write--retire target intent))
    (mevedel-memory-decision--publish-accepted workspace decision)))

(defun mevedel-memory-decision--requirements (workspace proposal &optional observation)
  "Return nil when PROPOSAL's required extractions in WORKSPACE are applied.
Otherwise return (:status STATUS :reason REASON).  An undecided requirement
returns `waiting', which records no decision; any other outcome makes the
dependent note disposal stale, so its note returns to review unchanged.
OBSERVATION may select required decisions under the current workspace claim;
authenticate each record again without rediscovering public history."
  (let ((requirements (plist-get observation :requirements)) waiting failed)
    (when observation
      (unless (and (plist-member observation :requirements)
                   (proper-list-p requirements)
                   (equal (mapcar #'car requirements) (plist-get proposal :requires)))
        (error "Missing or mismatched extraction decision observations")))
    (dolist (id (plist-get proposal :requires))
      (let* ((entry (if observation
                        (let ((entry (cdr (assoc id requirements))))
                          (when entry
                            (unless (equal id (plist-get entry :proposal-id))
                              (error "Selected extraction decision belongs to another proposal"))
                            (mevedel-memory-decision--published workspace entry))
                          entry)
                      (mevedel-memory-decision-status workspace id)))
             (status (plist-get entry :status)))
        (cond ((eq status 'applied))
              ((null status) (setq waiting t))
              (t (push status failed)))))
    (cond (failed (list :status 'stale
                        :reason (format "A required memory extraction was not applied (%s); the note stays for review."
                                        (mapconcat #'symbol-name (delete-dups failed) ", "))))
          (waiting (list :status 'waiting
                         :reason "Apply the memory extractions from this note first.")))))

(defun mevedel-memory-decision--application-input (accepted proposal &optional workspace observation hold)
  "Prepare checked application input for ACCEPTED PROPOSAL without writes.
Return :scope and :proposal working copies, plus :status and optional :reason.
Only unrelated index edits may advance the captured before-state. Application
must hold original-root ownership across this preparation and the transaction.
With WORKSPACE, a note disposal also checks its required extractions, using
this claim's selected decisions in OBSERVATION when supplied.  HOLD adds the
program-backed note mutation hold, for inspection; application runs that
check freshly at its forward deletion boundary instead."
  (let* ((scope (copy-tree (plist-get (plist-get accepted :prepared) :scope)))
         (item (copy-tree proposal))
         (index (assoc "MEMORY.md" (plist-get item :before)))
         (check (and workspace (mevedel-memory-decision--requirements workspace proposal observation))))
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
          (when (and hold workspace (eq (plist-get check :status) 'fresh)
                     (eq (plist-get item :action) 'note-delete)
                     (member (plist-get item :type) '("note" "attachment")))
            (when-let* ((reason (mevedel-memory-notes-mutation-hold
                                workspace root (plist-get item :file) scope)))
              (setq check (list :status 'waiting :reason reason)))))
      (error (setq check (list :status 'unavailable :reason (error-message-string err)))))
    (append check (list :scope scope :proposal item))))

(defun mevedel-memory-decision--hold (workspace item scope live)
  "Return a hold reason for ITEM from SCOPE in WORKSPACE, or nil to admit it.
LIVE, when non-nil, is the automatic forward-admission predicate.  A note an
open session uses, or whose visiting buffer has unsaved changes, is held.
This editor-local check runs no control program and records the hold for the
note's later retry."
  (if (and live (not (funcall live))) "Memory operation context changed"
    (when-let* (((mevedel-memory-proposal-note-action-p (plist-get item :action)))
                (reason (cond ((mevedel-memory-notes-in-use-p workspace (plist-get item :file))
                               "Shared file is in active local use")
                              ((seq-some (lambda (row)
                                           (when-let* ((buffer (find-buffer-visiting (plist-get (cdr row) :path))))
                                             (buffer-modified-p buffer)))
                                         (plist-get item :before))
                               "Shared file has unsaved buffer changes"))))
      (mevedel-memory-notes-record-hold
       workspace (plist-get (mevedel-memory-scope--root scope (plist-get item :root)) :dir)
       (plist-get item :file) reason)
      reason)))

(defun mevedel-memory-decision--write-boundary (workspace item scope live)
  "Return the forward-write guard for ITEM from SCOPE in WORKSPACE.
Recheck LIVE and local use, then run the program-backed note mutation hold
\(age, version, references, extraction) freshly at the deletion boundary.
A hold unwinds the prepared write through a private signal."
  (lambda ()
    (when-let* ((reason (mevedel-memory-decision--hold workspace item scope live)))
      (signal 'mevedel-memory-decision--held (list reason)))
    (when (and (eq (plist-get item :action) 'note-delete)
               (member (plist-get item :type) '("note" "attachment")))
      (when-let* ((root (mevedel-memory-scope--root scope (plist-get item :root)))
                  (reason (mevedel-memory-notes-mutation-hold workspace root (plist-get item :file) scope)))
        (mevedel-memory-notes-record-hold workspace (plist-get root :dir) (plist-get item :file) reason)
        (signal 'mevedel-memory-decision--held (list reason))))))

(cl-defun mevedel-memory-decision--write (workspace job target accepted item effective scope live on-applied)
  "Write ITEM's EFFECTIVE change from ACCEPTED in WORKSPACE under JOB and TARGET.
SCOPE carries checked index snapshots; LIVE guards automatic forward writes.
ON-APPLIED receives newly confirmed paths.  A late hold abandons the unchanged
prepared intent and returns status `held'."
  (let* ((claim (plist-get job :claim))
         (intent (condition-case err
                     (mevedel-memory-write--prepare workspace claim target accepted effective scope)
                   (quit (if live (signal 'mevedel-memory-decision-interrupted
                                          (list nil "Quit" "Interrupted memory write preparation: Quit"))
                           (signal (car err) (cdr err))))))
         failure original)
    (condition-case err
        (mevedel-memory-write--run workspace claim target intent
                                   (mevedel-memory-decision--write-boundary workspace item scope live))
      ((error quit) (setq failure err)))
    (setq original (if (eq (car-safe failure) 'mevedel-tool-patch-partial-rollback) (cadr failure) failure))
    (when (and failure
               (or (eq (car failure) 'mevedel-memory-decision--held)
                   (and live (eq (car failure) 'quit)))
               (condition-case nil
                   (eq (mevedel-memory-write--state intent) 'before)
                 (quit (signal 'mevedel-memory-decision-interrupted
                               (list nil "Quit" "Interrupted memory write observation: Quit")))))
      (condition-case err
          (mevedel-memory-write--abandon workspace claim target intent)
        ((error quit)
         (let ((message (format "Held memory write retirement failed: %s" (error-message-string err))))
           (if (or (eq (car err) 'quit) (eq (car-safe original) 'quit))
               (signal 'mevedel-memory-decision-interrupted (list nil "Quit" message))
             (error "%s" message)))))
      (if (eq (car failure) 'mevedel-memory-decision--held)
          (cl-return-from mevedel-memory-decision--write
            (list :status 'held :reason (or (cadr failure) "Memory application is held")))
        (signal 'mevedel-memory-decision-interrupted (list nil "Quit"))))
    (let ((decision
           (condition-case err
               (mevedel-memory-decision--finish-write workspace job target intent failure)
             ((error quit)
              (if (and live (or (eq (car err) 'quit) (eq (car-safe original) 'quit)))
                  (signal 'mevedel-memory-decision-interrupted
                          (list nil "Quit" (format "Interrupted memory write accounting: %s" (error-message-string err))))
                (signal (car err) (cdr err)))))))
      (when (and (not live) (eq (car-safe failure) 'quit))
        (signal (car failure) (cdr failure)))
      (when (and on-applied (eq (plist-get decision :status) 'applied))
        (funcall on-applied (mapcar (lambda (change) (plist-get change :path)) (plist-get intent :changes))))
      ;; Fully account reached or uncertain writes before cancellation.
      ;; Manual synchronous callers keep their original quit condition.
      (when (and live (eq (car-safe original) 'quit))
        (signal 'mevedel-memory-decision-interrupted (list decision "Quit")))
      decision)))

(cl-defun mevedel-memory-decision--apply-owned (workspace job pass proposal &key on-applied live automatic observation)
  "Apply PASS's PROPOSAL in WORKSPACE under JOB's recovered consolidation claim.
Authenticate the accepted bundle once for this continuously owned phase.
ON-APPLIED receives newly confirmed paths.  LIVE guards automatic forward
writes.  AUTOMATIC skips any proposal that already has a decision.
OBSERVATION may supply this claim's child-selected :previous and
:requirements decisions; each is authenticated afresh here."
  (let* ((accepted (mevedel-memory-store-accepted workspace pass t))
         (item (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals)))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (passes (let ((table (make-hash-table :test #'equal)))
                   (when accepted (puthash pass accepted table))
                   table))
         (previous (if observation
                       (progn
                         (unless (plist-member observation :previous)
                           (error "Missing selected decision observation"))
                         (when-let* ((entry (plist-get observation :previous)))
                           (unless (equal proposal (plist-get entry :proposal-id))
                             (error "Selected decision belongs to another proposal"))
                           (mevedel-memory-decision--published workspace entry passes)
                           entry))
                     (mevedel-memory-decision-status workspace proposal nil passes))))
    (unless item (error "Unknown accepted proposal"))
    (mevedel-memory-scope--root scope (plist-get item :root))
    (cond
     ((and automatic previous) '(:status skipped))
     ((mevedel-memory-decision-terminal-status-p (plist-get previous :status)) previous)
     ((when-let* ((reason (mevedel-memory-decision--hold workspace item scope live)))
        (list :status 'held :reason reason)))
     (t
      (mevedel-memory-write--call
       scope (plist-get item :root)
       (lambda (target)
         (let* ((input (mevedel-memory-decision--application-input accepted item workspace observation))
                (status (plist-get input :status)))
           (cond
            ((eq status 'waiting)
             (when (mevedel-memory-proposal-note-action-p (plist-get item :action))
               (mevedel-memory-notes-record-hold
                workspace (plist-get (mevedel-memory-scope--root scope (plist-get item :root)) :dir)
                (plist-get item :file) (plist-get input :reason)))
             (list :status 'waiting :reason (plist-get input :reason)))
            ((when-let* ((reason (mevedel-memory-decision--hold workspace item scope live)))
               (list :status 'held :reason reason)))
            ((not (eq status 'fresh))
             (mevedel-memory-decision--decided
              workspace job accepted item status (mevedel--truncate-bytes (plist-get input :reason) 4096 "...")))
            (t (mevedel-memory-decision--write
                workspace job target accepted item (plist-get input :proposal) (plist-get input :scope)
                live on-applied))))))))))

(defun mevedel-memory-decision--deliver (callback function)
  "Call FUNCTION synchronously and deliver its result to CALLBACK once.
Errors and cancellation are delivered as :error or :cancelled results,
retaining an already accounted decision's status."
  (funcall callback
           (condition-case err
               (funcall function)
             (mevedel-memory-decision-interrupted
              (append (mevedel-memory-decision--result (cadr err))
                      (list :cancelled t :reason (caddr err))
                      (when (cadddr err) (list :error (cadddr err)))))
             (quit (list :cancelled t :reason (error-message-string err)))
             (error (list :error (error-message-string err))))))

(defun mevedel-memory-decision--dispatch (callback start)
  "Call START with a once-only completion function for CALLBACK.
Deliver admission errors and quit through CALLBACK too, but propagate errors
from CALLBACK itself.  Do not expose START's private operation handle."
  (let (delivered)
    (cl-labels ((reply (value)
                 (unless delivered
                   (setq delivered t)
                   (funcall callback value))))
      (condition-case err
          (funcall start #'reply)
        ((error quit)
         (if delivered (signal (car err) (cdr err))
           (reply (if (eq (car err) 'quit)
                      (list :cancelled t :reason (error-message-string err))
                    (list :error (error-message-string err)))))))))
  nil)

(cl-defun mevedel-memory-decision-apply (workspace pass proposal &key continuation automatic on-applied callback)
  "Apply accepted PROPOSAL from PASS in WORKSPACE with durable write intent.
Acquire workspace and original-root ownership; no caller sees a claim.  Stale
targets produce an immutable decision without writes; partial results retain
checked recovery.  ON-APPLIED receives changed paths only for a newly
confirmed application.

Return a result plist: :status (applied, rejected, reversed, stale,
unavailable, recovery-required, waiting, held, skipped or idle-wait),
optional :reason, :decision metadata and :continuation.  Pass that
continuation to the next decision of the same explicit serial request (one
actor and origin) to skip historical recovery when the claim chain proves
direct succession; omit it for full recovery.  It is opaque and ephemeral,
never authority, and is nil after held or waiting admission, failure,
interruption or a changed origin.

AUTOMATIC is a context from `mevedel-memory-decision-automatic-context': live
auto policy is rechecked at admission and every forward write, and a proposal
that already has a decision is skipped.  Without CALLBACK the call is
synchronous and signals errors and quit; automatic cancellation signals
`mevedel-memory-decision-interrupted' carrying any accounted decision.
With CALLBACK, the result, an :error or a :cancelled result is delivered
exactly once, retaining confirmed effects.  AUTOMATIC plus CALLBACK on a local
Linux workspace selects child-assisted recovery and selection, waiting for
editor idleness (status `idle-wait' defers); otherwise execution stays in the
editor.  A callback alone grants no automatic authority."
  (let ((context (or automatic (mevedel-memory-decision--context workspace (current-buffer)))))
    (cond
     ((and automatic callback)
      (mevedel-memory-decision--dispatch
       callback
       (lambda (complete)
        (mevedel-memory-decision--start-operation
       workspace
       (lambda (job live reply)
         (if (not (or (plist-get reply :synchronous) (mevedel-memory-decision--idle-p)))
             ;; Recovery yielded to the editor.  Release this fixed claim before
             ;; waiting for idle; the caller retains only the proposal identity.
             '(:status idle-wait)
           (mevedel-memory-decision--apply-owned
            workspace job pass proposal :on-applied on-applied :live live :automatic t
            :observation (unless (plist-get reply :synchronous) reply))))
       (lambda (reply) (funcall complete (mevedel-memory-decision--public-reply reply)))
       (lambda () (mevedel-memory-decision-automatic-current-p automatic))
       nil (list :application t :pass-id pass :proposal-id proposal) continuation automatic))))
     (callback
      (mevedel-memory-decision--deliver
       callback (lambda () (mevedel-memory-decision-apply workspace pass proposal
                                                          :continuation continuation :on-applied on-applied))))
     (t
      (mevedel-memory-decision--execute
       workspace context continuation nil
       (lambda (job)
         (mevedel-memory-decision--apply-owned
          workspace job pass proposal :on-applied on-applied :automatic automatic
          :live (and automatic (lambda () (mevedel-memory-decision-automatic-current-p automatic))))))))))

(defun mevedel-memory-decision--idle-p ()
  "Return non-nil when the editor has been idle long enough for automatic work."
  (and (not (input-pending-p))
       (when-let* ((idle (current-idle-time)))
         (>= (float-time idle) mevedel-memory-decision-automatic-idle-seconds))))

(defun mevedel-memory-decision--pending-items (workspace skip)
  "Return pending automatic proposal identities in WORKSPACE, excluding pass SKIP.
Authenticate status within this read-only selection to omit every decided
proposal.  Retain only identities, never application authority; each later
attempt still freshly reads accepted proof and status."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
        (passes (make-hash-table :test #'equal)) items)
    (dolist (accepted (mevedel-memory-notes-passes workspace entries))
      (clrhash passes)
      (let ((pass (plist-get (plist-get accepted :prepared) :id)))
        (puthash pass accepted passes)
        (when (and (not (equal pass skip))
                   (eq 'auto (plist-get (cdr (mevedel-memory-notes-root
                                              (plist-get (plist-get accepted :prepared) :scope))) :mode)))
          (dolist (proposal (plist-get accepted :proposals))
            (unless (mevedel-memory-decision-status workspace (plist-get proposal :id) entries passes)
              (push (list :pass pass :proposal (plist-get proposal :id) :pending t
                          :note (mevedel-memory-proposal-note-action-p (plist-get proposal :action))
                          :file (plist-get proposal :file))
                    items))))))
    (nreverse items)))

(cl-defun mevedel-memory-decision-pending (workspace &key automatic skip continuation callback)
  "Select WORKSPACE's undecided automatic proposals after owned recovery.
Only reviews whose frozen mode was `auto' qualify; pass SKIP is omitted.
The result's :items hold identities (:pass :proposal :note :file) only, never
authority.  AUTOMATIC, CONTINUATION and CALLBACK are as for
`mevedel-memory-decision-apply'.  With both AUTOMATIC and CALLBACK on a local
workspace, owned recovery and selection run in a child.  Otherwise selection
is a read-only observation without a claim; it returns CONTINUATION unchanged."
  (if (and automatic callback)
      (mevedel-memory-decision--dispatch
       callback
       (lambda (complete)
        (mevedel-memory-decision--start-operation
       workspace
       (lambda (_job _live reply)
         (list :items (if (plist-get reply :synchronous)
                          (mevedel-memory-decision--pending-items workspace skip)
                        (plist-get reply :items))))
       (lambda (reply) (funcall complete (mevedel-memory-decision--public-reply reply)))
       (lambda () (mevedel-memory-decision-automatic-current-p automatic))
       nil (list :pending-selection t :skip skip) continuation automatic)))
    ;; Synchronous selection is read-only and takes no claim, so CONTINUATION
    ;; stays valid; each later decision rechecks status under ownership.
    (let ((result (list :items (mevedel-memory-decision--pending-items workspace skip)
                        :continuation continuation)))
      (if callback (funcall callback result) result))))

(cl-defun mevedel-memory-decision-reject (workspace pass proposal &key reason continuation)
  "Reject PROPOSAL from PASS in WORKSPACE with optional REASON.
Acquire workspace ownership, recover accepted state, and verify original target
authority.  A repeated rejection returns the existing terminal decision.
A marked write for PROPOSAL must be recovered first.  REASON is limited to
4 KiB.  The result and CONTINUATION are as for `mevedel-memory-decision-apply'."
  (setq reason (or reason ""))
  (unless (and (stringp reason) (<= (string-bytes reason) 4096)) (error "Rejection reason exceeds its byte limit"))
  (mevedel-memory-decision--execute
   workspace (mevedel-memory-decision--context workspace (current-buffer)) continuation nil
   (lambda (job)
     (let* ((accepted (mevedel-memory-store-accepted workspace pass t))
            (item (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals)))
            (scope (plist-get (plist-get accepted :prepared) :scope))
            (passes (let ((table (make-hash-table :test #'equal)))
                      (when accepted (puthash pass accepted table))
                      table))
            (previous (mevedel-memory-decision-status workspace proposal nil passes)))
       (unless item (error "Unknown accepted proposal"))
       (mevedel-memory-scope--root scope (plist-get item :root))
       (if (mevedel-memory-decision-terminal-status-p (plist-get previous :status)) previous
         (when (mevedel-memory-write--marked-p workspace scope (plist-get item :root) proposal)
           (error "Recover the pending memory write before rejecting"))
         (mevedel-memory-decision--decided workspace job accepted item 'rejected reason))))))

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

(cl-defun mevedel-memory-decision-reverse (workspace pass proposal &key continuation)
  "Reverse the accepted application of PROPOSAL from PASS in WORKSPACE.
Require every target to match that application's exact after-state, including
its shared index.  Persist a separate reverse intent before writes and retain
all prior decisions.  Repeated reversal returns the same terminal result.
Shared-note disposal is permanent.  Reversing an extraction never restores
notes.  The result and CONTINUATION are as for `mevedel-memory-decision-apply'."
  (mevedel-memory-decision--execute
   workspace (mevedel-memory-decision--context workspace (current-buffer)) continuation t
   (lambda (job)
     (let ((previous (mevedel-memory-decision-status workspace proposal)))
       (unless (and (equal pass (plist-get previous :pass-id))
                    (mevedel-memory-decision-written-status-p (plist-get previous :status)))
         (error "Proposal has no resolved application to reverse"))
       (let* ((intent (plist-get (mevedel-memory-decision--published workspace previous) :intent))
              (source (plist-get intent :reverse-of))
              (original (if source
                            (mevedel-memory-write--read workspace (car source) (cadr source) (plist-get intent :accepted))
                          intent))
              (scope (mevedel-memory-write--scope original)))
         (when (mevedel-memory-proposal-note-action-p (plist-get (plist-get original :item) :action))
           (user-error "Shared-note deletion is permanent and cannot be reversed"))
         (mevedel-memory-scope--root scope (plist-get (plist-get original :item) :root))
         (if (eq (plist-get previous :status) 'reversed) previous
           (mevedel-memory-write--call
            scope (plist-get (plist-get original :item) :root)
            (lambda (target)
              (let ((reverse (mevedel-memory-write--prepare-reverse workspace (plist-get job :claim) target original))
                    failed)
                (condition-case nil (mevedel-memory-write--run workspace (plist-get job :claim) target reverse)
                  ((error quit) (setq failed t)))
                (mevedel-memory-decision--finish-write workspace job target reverse failed))))))))))

(defun mevedel-memory-decision--recover-write-owned (workspace job id hash &optional rollback)
  "Reconcile WORKSPACE write ID and HASH under JOB's recovered claim.
ROLLBACK permits checked restoration of this unresolved attempt only."
  (let* ((intent (mevedel-memory-write--read workspace id hash))
         (previous (mevedel-memory-decision-status workspace (plist-get intent :proposal))))
    (mevedel-memory-write--call
     (mevedel-memory-write--scope intent) (plist-get (plist-get intent :item) :root)
     (lambda (target)
       (if (and previous (equal hash (plist-get previous :state-hash))
                (mevedel-memory-decision-resolved-write-status-p (plist-get previous :status)))
           (progn (mevedel-memory-write--retire target intent) previous)
         (unless (equal hash (mevedel-session-control-fs-read-file (mevedel-memory-write--pin intent)))
           (error "Memory root marker changed"))
         (if (and (not rollback) previous (equal hash (plist-get previous :state-hash))
                  (eq (plist-get previous :status) 'recovery-required)
                  (memq (mevedel-memory-write--state intent) '(mixed changed unavailable)))
             previous
           (let (failed)
             (when rollback
               (condition-case nil (mevedel-memory-write--rollback workspace (plist-get job :claim) target intent)
                 ((error quit) (setq failed t))))
             (mevedel-memory-decision--finish-write workspace job target intent failed)))))
     id)))

(cl-defun mevedel-memory-decision-recover-write (workspace id hash &key rollback continuation)
  "Reconcile WORKSPACE write ID against HASH without repeating its file writes.
Completed writes become applied decisions; untouched targets become unavailable
attempts.  Mixed or intervening changes remain recovery-required and fenced.
ROLLBACK restores only checked captured originals.  The result and
CONTINUATION are as for `mevedel-memory-decision-apply'."
  (mevedel-memory-decision--execute
   workspace (mevedel-memory-decision--context workspace (current-buffer)) continuation t
   (lambda (job) (mevedel-memory-decision--recover-write-owned workspace job id hash rollback))))

(defun mevedel-memory-decision--recover-writes (workspace &optional ids job)
  "Reconcile WORKSPACE's marked writes against current roots and live buffers.
This phase stays in the editor; publication recovery must precede it.
IDS restricts reconciliation to those write identities; without IDS, only
`mevedel-memory-write--recovery-candidates' are inspected.  JOB, when supplied,
already owns this operation's completed publication recovery."
  (let ((ids (or ids (mevedel-memory-write--recovery-candidates workspace)))
        results)
    (dolist (row (and ids (mevedel-memory-write--list workspace ids)))
      (cond
       ((plist-get row :error)
        (push (list :write-id (plist-get row :id) :status 'unavailable :reason (plist-get row :error)) results))
       ((plist-get row :marked)
        (push (condition-case err
                  (if job
                      (mevedel-memory-decision--recover-write-owned workspace job (plist-get row :id) (plist-get row :hash))
                    (plist-get (mevedel-memory-decision-recover-write workspace (plist-get row :id) (plist-get row :hash))
                               :decision))
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

(defun mevedel-memory-decision--worker-local-p (workspace &optional display)
  "Whether WORKSPACE's authenticated retained scopes need no remote target IO.
Current configuration alone cannot classify historical original-root authority.
Read only local acceptance metadata here, before target-sensitive recovery.
DISPLAY checks only workspace locality: the display collector classifies each
authenticated bundle before target IO, and reports invalid bundles unavailable."
  (if display
      (not (file-remote-p (mevedel-workspace-root workspace)))
    (condition-case nil
      (not (cl-some
            (lambda (accepted)
              (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                       (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
            (mevedel-memory-notes-passes
             workspace (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
    ;; Recovery, not this metadata preflight, owns interrupted accepted expiry
    ;; and incomplete state.  Uncertainty selects the checked native path.
      (error nil))))

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
      (when (and fenced (plist-get job :recovered) (not (plist-get job :publication)) (plist-get result :ok)
                 (not failure) (not (plist-get result :error))
                 (not (plist-get result :held)) (not (plist-get result :cancelled))
                 (not (plist-get job :stopped)))
        ;; Application items still need a selection child.
        (plist-put result :recovery
                   (mevedel-memory-decision--continuation
                    job fenced (plist-get (plist-get result :value) :status))))
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
							 (or (plist-get job :result) (list :reason (plist-get job :stopped)))))))

(defun mevedel-memory-decision--public-reply (reply)
  "Return the public verb result for internal operation REPLY.
Keep :error, :cancelled and timing alongside the decision's result."
  (let ((result (mevedel-memory-decision--result (plist-get reply :value) (plist-get reply :recovery))))
    (when (and (plist-get reply :held) (null (plist-get result :status)))
      (plist-put result :status 'held))
    (unless (plist-get result :reason)
      (plist-put result :reason (plist-get reply :reason)))
    (append result
            (cl-loop for key in '(:error :cancelled :unsettled :recovery-round-trip-ms)
                     when (plist-get reply key) append (list key (plist-get reply key))))))

(defun mevedel-memory-decision--publication-ready (job reply)
  "Finish JOB's targeted publication from REPLY without repeating target work."
  (when (and (not (plist-get job :done)) (not (plist-get job :executing)) (plist-get job :publication)
             (eq job (gethash (plist-get (plist-get job :claim) :directory)
                              mevedel-memory-decision--operations)))
    (plist-put job :executing t)
    (let ((result (plist-get job :result)))
      (condition-case err
          (progn
            (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
            (unless (and (plist-get reply :ok)
                         (equal (plist-get reply :operation-id) (plist-get (plist-get job :claim) :owner))
                         (equal (plist-get reply :publication) (plist-get job :publication))
                         (equal (plist-get (mevedel-journal-claim-outcome (plist-get job :claim)) :payload)
                                (plist-get (plist-get job :publication) :hash)))
              (error "Invalid memory decision publication reply"))
            (unless (mevedel-memory-decision--operation-live-p job)
              (signal 'mevedel-memory-decision--held '("Memory operation context changed")))
            (plist-put job :publication nil)
            (setf (mevedel-workspace-journal-observation (plist-get job :workspace)) nil)
            (mevedel-journal-cleanup-schedule (plist-get job :workspace) t))
        (mevedel-memory-decision--held (plist-put result :held t))
        (quit (plist-put result :cancelled t))
        (error (plist-put result :error
                          (if-let* ((prior (plist-get result :error)))
                              (concat prior "; " (error-message-string err))
                            (error-message-string err)))))
      (plist-put job :executing nil)
      (mevedel-memory-decision--operation-finish job result))))

(defun mevedel-memory-decision--operation-ready (job reply)
  "Continue JOB from worker REPLY with fresh ownership and live editor checks."
  (when (and (not (plist-get job :done)) (not (plist-get job :executing))
             (not (plist-get job :publication))
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
                   (signal 'mevedel-memory-decision--held '("Memory operation context changed")))
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
                       (signal 'mevedel-memory-decision--held '("Memory operation context changed")))
                     (mevedel-memory-store--assert-owned (plist-get job :claim)))
                   (plist-put job :recovered t)
                   (when-let* ((start (plist-get job :recovery-started-at)))
                     (plist-put job :recovery-round-trip-ms
                                (round (* 1000 (max 0 (- (mevedel-telemetry--monotonic-now) start)))))
                     (plist-put job :recovery-started-at nil))
                   (let ((mevedel-memory-decision--publication-job
                          (and (plist-get job :application) (not (plist-get reply :synchronous)) job)))
                     (list :ok t :value
                           (funcall (plist-get job :function) job
                                    (lambda () (mevedel-memory-decision--operation-live-p job)) reply)))))
             (mevedel-memory-decision--held (list :held t :reason (cadr err)))
             (mevedel-memory-decision-interrupted
              (append (when (cadr err) (list :ok t :value (cadr err)))
                      (list :cancelled t :reason (caddr err))
                      (cond ((cadddr err) (list :error (cadddr err)))
                            ((and (cadr err) (not (mevedel-memory-decision-terminal-status-p (plist-get (cadr err) :status))))
                             (list :error "Interrupted memory write requires checked recovery")))))
             (quit (list :cancelled t :reason (error-message-string err)))
             (error (list :error (error-message-string err))))))
      (plist-put job :executing nil)
      (plist-put job :result result)
      (if (and (plist-get job :publication) (plist-get result :ok))
          (condition-case err
              (mevedel-journal-worker-start
               (plist-get job :workspace) 'memory-decision-publish
               (lambda (reply) (mevedel-memory-decision--publication-ready job reply))
               nil (append (list :operation-id (plist-get (plist-get job :claim) :owner))
                           (plist-get job :publication)) job)
            ((error quit)
             (if (eq (car err) 'quit) (plist-put result :cancelled t)
               (plist-put result :error
                          (if-let* ((prior (plist-get result :error)))
                              (concat prior "; " (error-message-string err))
                            (error-message-string err))))
             (mevedel-memory-decision--operation-finish job result)))
        (mevedel-memory-decision--operation-finish job result)))))

(defun mevedel-memory-decision--start-operation (workspace function callback &optional currentp decisions-only payload recovery automatic)
  "Recover WORKSPACE in a child, then call FUNCTION under the same owned claim.
FUNCTION receives the job holding the fresh :claim, a live forward-admission
predicate and the child reply.  AUTOMATIC is the caller's automatic context;
its cancellation token learns this operation.
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
  (let* ((context (or automatic (mevedel-memory-decision--context workspace (current-buffer))))
         (key (mevedel-memory-store--claim-directory workspace))
         (claim (or (mevedel-journal-claim-acquire key 180) (user-error "Memory consolidation is busy")))
         (job (append (list :claim claim :function function :callback callback :currentp currentp
                            :application (plist-get payload :application)
                            :decisions-only decisions-only)
                      (copy-sequence context))))
    (puthash key job mevedel-memory-decision--operations)
    (when-let* ((token (plist-get automatic :token))) (plist-put token :operation job))
    (condition-case err
        (progn
          (plist-put job :timer
                     (run-at-time
                      (max 0 (- (plist-get claim :expires-at)
                                (mevedel-session-control-fs-target-time key))) nil
                      #'mevedel-memory-decision--operation-deadline job))
          (when (and (or (null payload) (plist-get payload :pending-selection) (plist-get payload :application))
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

(defun mevedel-memory-decision--recovery-failed (key reason)
  "Report failed recovery at claim directory KEY because of REASON.
Recovery runs after every root turn, so each distinct failure warns once and
later repeats go to *Messages*.  Successful recovery forgets reported failures."
  (mevedel--warn-once (list 'memory-recovery key reason)
                      "Memory recovery failed: %s.  Memory review waits until recovery succeeds"
                      reason))

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
                  (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                (unless (eq job (gethash key mevedel-memory-decision--recovery-workers))
                  (signal 'mevedel-memory-decision--held '("Recovery was cancelled")))
                (if (plist-get job :writes)
                    (let* ((ids (list (pop (plist-get job :writes))))
                           (row (car (mevedel-memory-write--list workspace ids))))
                      (unless (and (funcall (plist-get job :currentp))
                                   (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                        (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                      (when-let* ((failure (plist-get row :error)))
                        (mevedel--warn-once (list 'memory-write-unavailable (car ids) failure)
                                            "Memory write %s is unavailable: %s" (car ids) failure))
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
                                      (lambda (owned _live _reply)
					(mevedel-memory-decision--recover-writes workspace ids owned))
                                      (lambda (reply)
					(plist-put job :operation nil)
					(when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                                          (cond
                                           ((plist-get reply :error)
                                            (remhash key mevedel-memory-decision--recovery-workers)
                                            (mevedel-memory-decision--recovery-failed key (plist-get reply :error)))
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
                    (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                  (remhash key mevedel-memory-decision--recovery-workers)
                  (mevedel--warn-once-reset-site 'memory-recovery)
                  (when-let* ((callback (plist-get job :callback))
                              (origin (plist-get job :origin))
                              ((buffer-live-p origin)))
                    (with-current-buffer origin (funcall callback)))))
            (mevedel-memory-decision--held nil)
            ((error quit) (mevedel-memory-decision--recovery-failed key (error-message-string err))))
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
                  (unless (and (plist-get result :ok)
                               (equal (plist-get result :operation-id) (plist-get claim :owner))
                               (or (plist-get result :synchronous)
                                   (and (plist-member result :writes)
                                        (proper-list-p (plist-get result :writes))
                                        (<= (length (plist-get result :writes)) 65536)
                                        (cl-every #'mevedel-journal-store-id-p (plist-get result :writes)))))
                    (error "Invalid memory recovery selection reply"))
                  (unless (funcall (plist-get job :currentp))
                    (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                  (unless (eq job (gethash key mevedel-memory-decision--recovery-workers))
                    (signal 'mevedel-memory-decision--held '("Recovery was cancelled")))
                  (mevedel-memory-store--assert-owned claim)
                  (when (plist-get result :synchronous)
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                      (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                    (when (mevedel-memory-decision-recover workspace) (setq changed t))
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                      (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                    (when (mevedel-memory-store-recover workspace) (setq changed t))
                    (mevedel-memory-store--assert-owned claim))
                  (mevedel-memory-decision--recovery-release job)
                  (unless (and (funcall (plist-get job :currentp))
                               (eq job (gethash key mevedel-memory-decision--recovery-workers)))
                    (signal 'mevedel-memory-decision--held '("Recovery context changed")))
                  ;; A no-op recovery leaves committed state and its displays current.
                  (when changed (mevedel-memory-decision--recovery-changed workspace))
                  (plist-put job :writes
                             (if (plist-get result :synchronous)
                                 (mevedel-memory-write--recovery-candidates workspace)
                               (plist-get result :writes)))
                  (when (eq job (gethash key mevedel-memory-decision--recovery-workers))
                    (plist-put job :timer
                               (run-at-time .01 nil #'mevedel-memory-decision--recover-next-write workspace job))
                    (setq continued t)))
              (mevedel-memory-decision--held nil)
              ((error quit) (mevedel-memory-decision--recovery-failed key (error-message-string err)))))
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
			   nil (list :claim claim :operation-id (plist-get claim :owner) :write-selection t) job)
			(mevedel-memory-decision--recovered
                         workspace job (list :ok t :operation-id (plist-get claim :owner) :synchronous t)))
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
                        (error (mevedel-memory-decision--recovery-failed key (error-message-string err))))
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

;;
;;; Read-only inspection

(defun mevedel-memory-decision--inspect-proposal (workspace pass proposal)
  "Return a detached inspection of PASS's PROPOSAL in WORKSPACE.
See `mevedel-memory-decision-inspect'."
  (let* ((accepted (mevedel-memory-store-accepted workspace pass t))
         (item (or (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals))
                   (error "Unknown accepted proposal")))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (_ (mevedel-memory-scope--root scope (plist-get item :root)))
         (passes (let ((table (make-hash-table :test #'equal))) (puthash pass accepted table) table))
         (decision (mevedel-memory-decision-status workspace proposal nil passes))
         (intent (and decision (plist-get (mevedel-memory-decision--published workspace decision passes) :intent)))
         (input (unless (or intent (eq (plist-get decision :status) 'rejected))
                  (mevedel-memory-decision--application-input accepted item workspace nil t)))
         (fresh (eq (plist-get input :status) 'fresh))
         (effective (if fresh (plist-get input :proposal) item))
         (before (or (plist-get intent :before) (plist-get effective :before)))
         (after (or (plist-get intent :after)
                    (mevedel-memory-write--after
                     before (mevedel-memory-apply-changes (if fresh (plist-get input :scope) scope) effective))))
         (writes (mapcar (lambda (marked)
                           (let ((state (mevedel-memory-write--state marked)))
                             (list :id (plist-get marked :id) :hash (plist-get marked :hash) :state state
                                   ;; Only a fresh owned recovery can establish eligibility.
                                   :rollback (and (memq state '(before after mixed))
                                                  (not (mevedel-memory-write--notes-root marked))
                                                  'provisional))))
                         (mevedel-memory-write--marked workspace scope (plist-get item :root) proposal))))
    (mevedel-memory-decision--snapshot
     (list :status (or (plist-get decision :status) 'pending)
           :decision (and decision (cl-loop for field in mevedel-journal-store--decision-fields
                                            append (list (cdr field) (plist-get decision (cdr field)))))
           :check (and input (list :status (plist-get input :status) :reason (plist-get input :reason)))
           :before before :after after :writes writes))))

(defun mevedel-memory-decision--inspect-workspace (workspace skip local-only)
  "Return detached proposal and write rows for WORKSPACE, omitting passes in SKIP.
LOCAL-ONLY returns (:native t) before accessing any authenticated remote target.
See `mevedel-memory-decision-inspect'."
  (catch 'mevedel-memory-decision--native
    (let* ((root (mevedel-workspace-root workspace))
           (state (mevedel-journal-store-state-directory (mevedel-journal-store-directory root)))
           ;; A display observation, not a mutation proof. A concurrent expiry can
           ;; remove later evidence: those reads fail unavailable, never applied.
           (mevedel-journal-cleanup--retired-observation
            (cons root (mapcar #'file-name-nondirectory
                              (mevedel-session-control-fs-list-directory
                               (file-name-concat state "retired-passes") mevedel-journal-store-id-regexp))))
           (entries (seq-remove (lambda (entry) (member (plist-get entry :pass-id) skip))
                                (mevedel-journal-store-entries root)))
           (groups (mevedel-memory-write--groups
                    workspace (mapcar (lambda (path) (file-name-sans-extension (file-name-nondirectory path)))
                                      (mevedel-session-control-fs-list-directory
                                       (mevedel-memory-write--directory workspace) mevedel-memory-write-intent-file-regexp))))
           (records (make-hash-table :test #'equal))
           (coordinators (make-hash-table :test #'equal))
           (passes (make-hash-table :test #'equal)) seen rows)
      (let ((paths (delete-dups
                    (cl-loop for entry in entries when (eq (plist-get entry :kind) 'decision)
                             collect (file-name-concat (mevedel-memory-decision--directory workspace)
                                                       (concat (plist-get entry :decision-id) ".el"))))))
        (while paths
          (let ((batch (seq-take paths 8)))
            (setq paths (nthcdr (length batch) paths))
            (condition-case nil
                (dolist (row (mevedel-memory-decision--record-batch workspace batch))
                  (puthash (car row) (cdr row) records)
                  (when-let* ((record (cdr row)) (id (plist-get record :write-id)))
                    (puthash id (plist-get record :claim) coordinators)))
              (error
               ;; Corruption in one record must not hide unrelated passes.
               (dolist (path batch)
                 (puthash path (condition-case failure (mevedel-memory-decision--record workspace path)
                                 (error (error-message-string failure))) records)))))))
      ;; Accepted private state remains inspectable if public publication failed.
      (let* ((directories (seq-remove (lambda (path) (member (file-name-nondirectory path) skip))
                                      (mevedel-session-control-fs-list-directory (file-name-concat state "passes") mevedel-journal-store-id-regexp)))
             (exists (mevedel-session-control-fs-paths-exist
                      (mapcar (lambda (directory) (file-name-concat directory "accepted.el")) directories))))
        (cl-mapc (lambda (directory present)
                   (when present
                     (push (list :kind 'consolidation :pass-id (file-name-nondirectory (directory-file-name directory))) entries)))
                 directories exists))
      (maphash (lambda (pass _ids)
                 (when (and pass (not (member pass skip)))
                   (push (list :kind 'consolidation :pass-id pass) entries))) groups)
      (dolist (entry entries)
        (when (and (eq (plist-get entry :kind) 'consolidation)
                   (not (mevedel-journal-cleanup-pass-retired-p root (plist-get entry :pass-id)))
                   (not (member (plist-get entry :pass-id) seen)))
          (let ((pass (plist-get entry :pass-id)))
            (push pass seen)
            (clrhash passes)
            (condition-case err
                (let* ((accepted (mevedel-memory-store-accepted workspace pass t))
                       (_local (when (and local-only
                                          (seq-some (lambda (row) (file-remote-p (plist-get (cdr row) :dir)))
                                                    (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
                                 (throw 'mevedel-memory-decision--native '(:native t))))
                       (writes (when (gethash pass groups)
                                 (mevedel-memory-write--list workspace (gethash pass groups) accepted coordinators)))
                       (pass-entries (seq-filter (lambda (item) (equal pass (plist-get item :pass-id))) entries))
                       (intents (make-hash-table :test #'equal)))
                  (dolist (write writes)
                    (when-let* ((intent (plist-get write :intent))) (puthash (plist-get intent :id) intent intents)))
                  (when accepted (puthash pass accepted passes))
                  (when (and accepted (null (plist-get accepted :proposals)))
                    (push (list :id pass :kind 'review :pass pass :status 'completed
                                :created (plist-get (plist-get accepted :review) :created)) rows))
                  (dolist (proposal (plist-get accepted :proposals))
                    (let* ((id (plist-get proposal :id))
                           (scope (plist-get (plist-get accepted :prepared) :scope))
                           (target (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
                           (decision (mevedel-memory-decision-status workspace id pass-entries passes intents records))
                           (write (seq-find (lambda (row) (and (equal id (plist-get (plist-get row :intent) :proposal))
                                                              (or (plist-get row :marked) (plist-get row :error)))) writes))
                           (status (or (plist-get decision :status) 'pending)) problem)
                      (condition-case unavailable (mevedel-memory-scope--root scope (plist-get proposal :root))
                        (error (setq problem (error-message-string unavailable))))
                      (setq problem (or problem (plist-get write :error)))
                      (when (plist-get write :marked) (setq status 'recovery-required))
                      (when problem (setq status 'unavailable))
                      (push (list :id id :pass pass :proposal
                                  (list :action (plist-get proposal :action) :type (plist-get proposal :type)
                                        :title (plist-get proposal :title))
                                  :created (plist-get (plist-get accepted :review) :created) :status status :error problem
                                  ;; A recorded stale decision may since have become applicable, or a pending
                                  ;; proposal superseded; label rows by what accepting would do now.
                                  :check (when (mevedel-memory-decision-actionable-status-p status)
                                           (plist-get (mevedel-memory-decision--application-input
                                                       accepted proposal workspace nil t)
                                                      :status))
                                  :target (format "%s%s" (file-name-concat (plist-get target :dir) (plist-get proposal :file))
                                                  (if (plist-get target :client) (format " [%s]" (plist-get target :client)) "")))
                            rows)))
                  (dolist (write writes)
                    (when (and (plist-get write :error) (not (plist-get write :intent)))
                      (push (list :id (plist-get write :id) :pass pass :status 'unavailable :error (plist-get write :error)) rows)))
                  ;; Retirement publishes its marker before removing evidence.
                  ;; Reject this pass's rows if expiry raced our held proofs.
                  (dolist (result (mevedel-session-control-fs-run-program
                                   (list (list :op 'absent :optional t :path (file-name-concat state "retired-passes" pass))
                                         (list :op 'verify-hash :path (file-name-concat (mevedel-memory-store--directory workspace pass) "accepted.el")
                                               :content (plist-get accepted :hash)))))
                    (unless (and (eq (plist-get result :op) 'absent) (eq (plist-get result :status) 'absent))
                      (mevedel-session-control-fs-program-value result))))
              (error
               (setq rows (seq-remove (lambda (row) (equal pass (plist-get row :pass))) rows))
               (push (list :id pass :pass pass :status 'unavailable :error (error-message-string err)) rows)
               (dolist (id (gethash pass groups))
                 (push (list :id id :status 'unavailable :error (error-message-string err)) rows)))))))
      (dolist (write (gethash nil groups))
        (push (list :id (plist-get write :id) :status 'unavailable :error (plist-get write :error)) rows))
      (let (markers)
        (when-let* ((inspected (gethash workspace mevedel-memory-write--inspected)))
          (maphash (lambda (_ pin)
                     (when (stringp pin) (cl-pushnew (directory-file-name (file-name-directory pin)) markers :test #'equal)))
                   inspected))
        (mevedel-memory-decision--snapshot (list :rows (nreverse rows) :markers markers))))))

(cl-defun mevedel-memory-decision-inspect (workspace &key pass proposal skip local-only)
  "Return a detached read-only snapshot of WORKSPACE's decisions and writes.
With PASS and PROPOSAL, describe that proposal: :status, :decision metadata,
the current application :check, exact :before and :after file states (from
its write intent once one exists) and its marked :writes, each with :id,
:hash, observed :state and :rollback eligibility, which is only `provisional':
every action freshly authenticates again.  Otherwise return :rows for every
retained proposal, review and unavailable write, omitting passes in SKIP, and
the authenticated pending-marker directories as :markers.  LOCAL-ONLY returns
\(:native t) before accessing an authenticated remote target.  The snapshot
grants nothing; mutable leaves are copied."
  (if (and pass proposal)
      (mevedel-memory-decision--inspect-proposal workspace pass proposal)
    (mevedel-memory-decision--inspect-workspace workspace skip local-only)))

(defun mevedel-memory-decision-retention (workspace accepted entries)
  "Return terminal ACCEPTED pass history in WORKSPACE from public ENTRIES.
Every decision must be published, every proposal terminal, and every write
resolved without a target marker.  Return :private file captures (relative to
the journal state directory, with their authenticated hashes), :related
public decision records and :resolved-at.  Signal when dependencies remain."
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (resolved-at (float-time (date-to-time (plist-get (plist-get accepted :review) :created))))
         (latest (make-hash-table :test #'equal))
         (passes (make-hash-table :test #'equal))
         (decisions (make-hash-table :test #'equal))
         history private related)
    (puthash pass accepted passes)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision) (equal pass (plist-get entry :pass-id)))
        (setq resolved-at (max resolved-at (float-time (date-to-time (plist-get entry :created)))))
        (let* ((record (mevedel-memory-decision--published workspace entry passes))
               (claim (plist-get record :claim))
               (proposal (plist-get entry :proposal-id))
               (old (gethash proposal latest)))
          (when (> (plist-get claim :generation) (or (car old) 0))
            (puthash proposal (cons (plist-get claim :generation) (plist-get entry :status)) latest))
          (push entry history)
          (push (list :id (plist-get entry :decision-id) :file (plist-get entry :file)
                      :sha256 (secure-hash 'sha256 (plist-get entry :text))) related)
          (puthash (plist-get entry :decision-id) t decisions)
          ;; The reader already proved these exact bytes against acceptance.
          ;; The expiry transaction checks them again immediately before deletion.
          (push (list :file (format "decisions/%s.el" (plist-get entry :decision-id))
                      :sha256 (plist-get record :hash)) private))))
    (unless (cl-every (lambda (proposal) (mevedel-memory-decision-terminal-status-p (cdr (gethash (plist-get proposal :id) latest))))
                     (plist-get accepted :proposals))
      (error "Memory proposal is unresolved"))
    (dolist (path (mevedel-session-control-fs-list-directory (mevedel-memory-decision--directory workspace) mevedel-memory-write-intent-file-regexp))
      (when-let* (((not (gethash (file-name-base path) decisions)))
                  (record (mevedel-memory-decision--record workspace path))
                  (metadata (plist-get record :metadata))
                  ((equal pass (plist-get metadata :pass-id))))
        (error "Memory decision publication is unresolved")))
    (dolist (row (mevedel-memory-write--list workspace))
      (let ((intent (plist-get row :intent)))
        (when (or (not intent) (equal pass (plist-get intent :pass)))
          (when (or (plist-get row :error) (plist-get row :marked)
                    (not (seq-some (lambda (entry)
                                     (and (equal (plist-get row :hash) (plist-get entry :state-hash))
                                          (mevedel-memory-decision-resolved-write-status-p (plist-get entry :status)))) history)))
            (error "Memory write recovery is unresolved"))
          (push (list :file (format "writes/%s.el" (plist-get row :id)) :sha256 (plist-get row :hash)) private))))
    (list :private (nreverse private) :related (nreverse related) :resolved-at resolved-at)))

;;
;;; Maintenance child

(defun mevedel-memory-decision--child-run (operation workspace payload)
  "Run decision child OPERATION for WORKSPACE from frozen PAYLOAD.
`memory-recover' recovers under the parent's claim and selects identities;
`memory-decision-publish' reauthenticates and publishes one accepted decision.
Neither returns reusable acceptance proofs."
  (pcase operation
    ('memory-recover
     (mevedel-memory-store--assert-owned (plist-get payload :claim))
     (let* ((predecessor (plist-get payload :recovery-predecessor))
            (recovered
             (and predecessor
                  (or (mevedel-memory-decision--recovery-successor-p
                       (list :claim (plist-get payload :claim)) predecessor)
                      (error "Memory recovery predecessor changed")))))
       (when (and recovered (not (or (plist-get payload :pending-selection)
                                     (plist-get payload :application))))
         (error "Recovery continuity requires decision discovery"))
       (if (and (not recovered) (not (mevedel-memory-decision--worker-local-p workspace)))
           (list :ok t :operation-id (plist-get payload :operation-id) :synchronous t)
         (let ((changed (unless recovered (mevedel-memory-decision-recover workspace))))
           (unless (or recovered (plist-get payload :decisions-only))
             (when (mevedel-memory-store-recover workspace) (setq changed t)))
           (cond
            ((plist-get payload :application)
             (unless (mevedel-journal-store-id-p (plist-get payload :proposal-id))
               (error "Invalid application proposal identity"))
             (let* ((accepted (mevedel-memory-store-accepted workspace (plist-get payload :pass-id) t))
                    (proposal (seq-find (lambda (item) (equal (plist-get item :id) (plist-get payload :proposal-id)))
                                        (plist-get accepted :proposals)))
                    (entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
                    (passes (make-hash-table :test #'equal)))
               (unless proposal (error "Unknown accepted application proposal"))
               (puthash (plist-get payload :pass-id) accepted passes)
               (cl-labels ((selected (id)
                             (when-let* ((entry (mevedel-memory-decision-status workspace id entries passes)))
                               (cl-loop for field in mevedel-journal-store--decision-fields
                                        append (list (cdr field) (plist-get entry (cdr field)))))))
                 (list :ok t :operation-id (plist-get payload :operation-id)
                       :changed changed :previous (selected (plist-get proposal :id))
                       :requirements (mapcar (lambda (id) (cons id (selected id)))
                                             (plist-get proposal :requires))))))
            ((plist-get payload :pending-selection)
             (list :ok t :operation-id (plist-get payload :operation-id)
                   :changed changed :items
                   (mevedel-memory-decision--pending-items workspace (plist-get payload :skip))))
            (t (append (list :ok t :operation-id (plist-get payload :operation-id)
                             :changed changed)
                       (when (plist-get payload :write-selection)
                         (list :writes (mevedel-memory-write--recovery-selection workspace))))))))))
    ('memory-decision-publish
     (let ((id (plist-get payload :decision-id)))
       (unless (and (mevedel-journal-store-id-p id)
                    (equal id (plist-get payload :operation-id))
                    (mevedel-journal-store-id-p (plist-get payload :hash)))
         (error "Invalid decision publication identity"))
       (mevedel-memory-decision--publish workspace id (plist-get payload :hash))
       (list :ok t :operation-id id
             :publication (list :decision-id id :hash (plist-get payload :hash)))))
    (_ (error "Unknown memory decision child operation"))))

(provide 'mevedel-memory-decision)
;;; mevedel-memory-decision.el ends here
