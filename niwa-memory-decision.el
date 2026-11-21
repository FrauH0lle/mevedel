;;; niwa-memory-decision.el -- Durable memory proposal decisions -*- lexical-binding: t -*-

;;; Commentary:

;; The workspace lock owner elects an immutable decision slot after the target
;; transaction. Journal publication can replay that slot after process death.
;; Private proposal text enters later review evidence only under its captured
;; root authority.

;;; Code:

(require 'cl-lib)
(require 'niwa-journal-worker)
(require 'niwa-memory-store)
(require 'niwa-memory-write)
(require 'niwa-transport)
(require 'niwa-utilities)
(require 'niwa-workspace-identity)

;; `niwa-journal-cleanup'
(declare-function niwa-journal-cleanup-active-p "niwa-journal-cleanup" (workspace))

;; `niwa-memory-notes'
(autoload 'niwa-memory-notes-commit "niwa-memory-notes")
(autoload 'niwa-memory-notes-observe "niwa-memory-notes")
(declare-function niwa-memory-notes-in-use-p "niwa-memory-notes" (workspace relative))
(declare-function niwa-memory-notes-mutation-hold "niwa-memory-notes" (workspace root relative &optional captured-scope))
(declare-function niwa-memory-notes-passes "niwa-memory-notes" (workspace entries))
(declare-function niwa-memory-notes-record-hold "niwa-memory-notes" (workspace directory relative reason))
(autoload 'niwa-memory-notes-in-use-p "niwa-memory-notes")
(autoload 'niwa-memory-notes-mutation-hold "niwa-memory-notes")
(autoload 'niwa-memory-notes-passes "niwa-memory-notes")
(autoload 'niwa-memory-notes-record-hold "niwa-memory-notes")

;; `niwa-memory-pass'
(declare-function niwa-tool-patch--assert-buffers-unmodified "niwa-tool-patch" (changes))
(declare-function niwa-tool-patch-commit "niwa-tool-patch" (changes &optional currentp mutate batch))
(autoload 'niwa-tool-patch--assert-buffers-unmodified "niwa-tool-patch")
(autoload 'niwa-tool-patch-commit "niwa-tool-patch")

(defvar niwa-memory-pass--drains)
(defvar niwa-memory-pass--running)

;; `niwa-telemetry'
(declare-function niwa-telemetry--monotonic-now "niwa-telemetry" ())
(autoload 'niwa-telemetry--monotonic-now "niwa-telemetry")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-warning "niwa-utilities" (scope type message &optional level buffer-name))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; Both pass scheduling and the decision admission boundary read this policy.
;; Keep it available when the decision API is used without loading the pass.
(defcustom niwa-memory-consolidation-mode 'propose
  "How memory consolidation is scheduled and proposals are handled.
Manual runs only on request.  Propose and auto offer a daily finite shared-note
cohort at digest publication, completed root turns, session opening and idle
maintenance.  Propose leaves decisions for approval; auto applies fresh memory
and instruction proposals, including `AGENTS.md', through the checked path.
Journal-only review retains its separate sparse-digest and elapsed-time gates."
  :type '(choice (const manual) (const propose) (const auto))
  :group 'niwa)

(defun niwa-memory-decision-terminal-status-p (status)
  "Return non-nil when decision STATUS ends its proposal's lifecycle."
  (memq status '(applied rejected reversed)))

(defun niwa-memory-decision-actionable-status-p (status)
  "Return non-nil when a proposal with STATUS still awaits a user decision."
  (memq status '(pending stale)))

(defun niwa-memory-decision-written-status-p (status)
  "Return non-nil when decision STATUS records a write that reached its target."
  (memq status '(applied reversed)))

(defvar niwa-memory-decision--inhibit-recovery nil
  "Non-nil suppresses deferred recovery during exit or isolated tests.")

(defvar niwa-memory-decision--recovery-dispatching (make-hash-table :test #'equal)
  "Observation-only tokens spanning scheduled recovery admission IO.")

(defvar niwa-memory-decision--recovery-pending (make-hash-table :test #'equal)
  "Coalesced activation recovery timers, keyed by workspace claim directory.")

(defvar niwa-memory-decision--recovery-workers (make-hash-table :test #'equal)
  "Active publication recovery and live-write checks, keyed by claim directory.")

(defvar niwa-memory-decision--recovery-generations (make-hash-table :test #'equal)
  "Recoveries that changed committed state, counted by claim directory.")

(defvar niwa-memory-decision--operations (make-hash-table :test #'equal)
  "Parent-owned async decision operations, keyed by workspace claim directory.")

(defvar niwa-memory-decision--publication-job nil
  "Local automatic operation deferring only its accepted decision publication.")

(defun niwa-memory-decision--directory (workspace)
  "Return the private decision directory for WORKSPACE."
  (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory (niwa-workspace-root workspace))) "decision-slots"))

(defconst niwa-memory-decision--file-regexp
  (concat "\\`" niwa-journal-store-decision-id-regexp "\\.el\\'")
  "Exact immutable private decision filenames.")

(defun niwa-memory-decision--newer-slot-p (a b)
  "Return whether A takes precedence over B: reversal, decision, then attempts."
  (cond ((string-suffix-p ".reverse.el" b) nil) ((string-suffix-p ".reverse.el" a) t)
        ((not (string-match-p "\\.attempt-" (file-name-nondirectory b))) nil)
        ((not (string-match-p "\\.attempt-" (file-name-nondirectory a))) t)
        (t (string-version-lessp b a))))

(defun niwa-memory-decision--slots (workspace proposal)
  "Return PROPOSAL's private slot paths in WORKSPACE, newest status first."
  (unless (niwa-journal-store-id-p proposal) (error "Invalid proposal identity"))
  (sort (niwa-session-control-fs-list-directory
         (niwa-memory-decision--directory workspace)
         (concat "\\`" proposal "\\(?:\\.reverse\\|\\.attempt-[1-9][0-9]*\\)?\\.el\\'"))
        #'niwa-memory-decision--newer-slot-p))

(defun niwa-memory-decision-status (workspace proposal &optional _entries passes _writes records)
  "Return the latest accepted slot metadata for PROPOSAL in WORKSPACE.
PASSES and RECORDS are optional caches scoped to one observation."
  (when-let* ((path (car (niwa-memory-decision--slots workspace proposal)))
              (metadata (plist-get (niwa-memory-decision--read workspace path passes nil (and records (gethash path records))) :metadata)))
    (niwa-journal-store--decode (niwa-journal-store--encode metadata nil 'decision)
                                   (niwa-journal-store--filename metadata 'decision))))

(defun niwa-memory-decision--published (workspace entry &optional passes writes records)
  "Return accepted private state matching public decision ENTRY in WORKSPACE.
PASSES, WRITES and RECORDS are the current observation described by the readers."
  (let* ((path (file-name-concat (niwa-memory-decision--directory workspace)
                                (concat (plist-get entry :decision-id) ".el")))
         (record (and records (gethash path records)))
         (accepted (progn
                     (when (and records (not record)) (error "Unavailable decision record"))
                     (when (stringp record) (error "%s" record))
                     (when (and record
                                (not (equal (plist-get entry :pass-id) (plist-get (plist-get record :metadata) :pass-id))))
                       (error "Published decision source pass changed"))
                     (niwa-memory-decision--read workspace path passes writes record)))
         (metadata (plist-get accepted :metadata)))
    (unless (and accepted
                 (cl-every (lambda (field) (equal (plist-get entry (cdr field)) (plist-get metadata (cdr field))))
                           niwa-journal-store--decision-fields))
      (error "Published decision has no matching acceptance evidence"))
    accepted))

(defun niwa-memory-decision--record (_workspace path &optional read)
  "Validate the slot envelope at PATH, optionally using the current pinned READ."
  (let* ((record (niwa-memory-store--read-lisp path read))
         (data (car record)) (metadata (plist-get data :metadata))
         (id (plist-get metadata :decision-id)) (proposal (plist-get metadata :proposal-id)))
    (niwa-journal-store--validate-metadata metadata 'decision)
    (unless (equal (cdr record) (niwa-memory-store--print data))
      (error "Accepted memory decision changed"))
    (unless (and (proper-list-p data) (= 8 (length data))
                 (cl-every (lambda (key) (plist-member data key)) '(:metadata :source :before :after))
                 (niwa-journal-store-id-p (plist-get data :source))
                 (equal (file-name-nondirectory path) (concat id ".el"))
                 (pcase (plist-get metadata :status)
                   ((or 'applied 'rejected) (equal id proposal))
                   ('reversed (equal id (concat proposal ".reverse")))
                   ((or 'unavailable 'stale) (string-prefix-p (concat proposal ".attempt-") id))))
      (error "Invalid private decision slot"))
    (append data (list :hash (secure-hash 'sha256 (cdr record))))))

(defun niwa-memory-decision--record-batch (workspace paths)
  "Read and validate a bounded batch of immutable PATHS for WORKSPACE."
  (when (> (length paths) 8) (error "Decision slot batch exceeds eight records"))
  (let ((reads (niwa-session-control-fs-run-program
                (mapcar (lambda (path) (list :op 'read :path path :coding 'utf-8-unix
                                             :max-bytes (1+ niwa-memory-store--max-bytes))) paths))))
    (unless (= (length paths) (length reads)) (error "Incomplete decision slot observation"))
    (cl-mapcar (lambda (path read) (cons path (niwa-memory-decision--record workspace path read))) paths reads)))

(defun niwa-memory-decision--read (workspace path &optional passes _writes record)
  "Verify the slot at PATH against its accepted proposal in WORKSPACE.
PASSES holds at most one source bundle in this observation.  RECORD, if
supplied, is the current pinned slot read from the same observation."
  (let* ((data (or record (niwa-memory-decision--record workspace path)))
         (metadata (plist-get data :metadata)) (pass (plist-get metadata :pass-id)))
    (unless (niwa-journal-cleanup-pass-retired-p (niwa-workspace-root workspace) pass)
      (let* ((accepted (or (and passes (gethash pass passes))
                           (let ((bundle (niwa-memory-store-accepted workspace pass t)))
                             (when passes (clrhash passes) (puthash pass bundle passes)) bundle)))
             (scope (plist-get (plist-get accepted :prepared) :scope))
             (item (seq-find (lambda (p) (equal (plist-get p :id) (plist-get metadata :proposal-id)))
                             (plist-get accepted :proposals)))
             (root (cdr (assoc (plist-get item :root) (plist-get scope :roots))))
             (before (plist-get data :before)) (after (plist-get data :after)))
        (unless (and item root (eq (plist-get scope :decision-format) 'slots)
                     (equal (plist-get data :source) (plist-get accepted :hash))
                     (equal (plist-get metadata :workspace) (plist-get (plist-get accepted :review) :workspace))
                     (equal (plist-get metadata :state-hash)
                            (secure-hash 'sha256 (niwa-memory-store--print (list before after))))
                     (if before
                         (and (niwa-memory-write--before-p item before)
                              (let ((effective (copy-sequence item))
                                    ;; Rederive with the slot's own stamp: recovery may run on a later day.
                                    (date (let ((bytes (plist-get (cdr (assoc (plist-get item :file) after)) :bytes)))
                                            (and (stringp bytes)
                                                 (string-match "^modified: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)$" bytes)
                                                 (match-string 1 bytes)))))
                                (setf (plist-get effective :before) before)
                                (equal after (niwa-memory-write--after
                                              before (niwa-memory-apply--changes root effective (plist-get scope :new-file-mode) date)))))
                       (and (not after) (memq (plist-get metadata :status) '(stale unavailable rejected)))))
          (error "Decision does not match accepted proposal"))
        (when (eq (plist-get metadata :status) 'reversed)
          (let* ((original (niwa-memory-decision--read
                            workspace (file-name-concat (file-name-directory path) (concat (plist-get item :id) ".el")) passes)))
            (unless (and (eq (plist-get (plist-get original :metadata) :status) 'applied)
                         (equal before (plist-get original :before)) (equal after (plist-get original :after))
                         (not (niwa-memory-proposal-note-action-p (plist-get item :action))))
              (error "Reversal does not match applied slot"))))
        (append data (list :accepted accepted :item item))))))

(defun niwa-memory-decision--settled-p (workspace accepted &optional passes)
  "Return non-nil when every proposal of ACCEPTED is terminal in WORKSPACE.
Read the public journal once for the whole pass.  An incomplete terminal
index can only refuse release; a complete index still needs private proofs.
PASSES may hold bundles authenticated in the caller's current owned phase."
  (let ((entries (niwa-journal-store-entries (niwa-workspace-root workspace)))
        (passes (or passes (make-hash-table :test #'equal)))
        (terminal (make-hash-table :test #'equal))
        (proposals (plist-get (plist-get accepted :review) :proposals)))
    ;; Publication calls this after each decision.  Authenticating a growing
    ;; prefix before discovering the next absent proposal is quadratic IO.
    ;; Public metadata cannot prove settlement, but absence can refuse it.
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision)
                 (niwa-memory-decision-terminal-status-p (plist-get entry :status)))
        (push entry (gethash (plist-get entry :proposal-id) terminal))))
    (and (cl-every (lambda (id) (gethash id terminal)) proposals)
         (cl-every (lambda (id)
                     (cl-every (lambda (entry) (niwa-memory-decision--published workspace entry passes))
                               (gethash id terminal)))
                   proposals))))

(defun niwa-memory-decision--publish (workspace id hash)
  "Publish WORKSPACE's accepted decision ID with record HASH in one mutation.
Reauthenticate the decision and its proposal bundle under the journal mutation
claim.  Verify the pass review's exact public bytes and republish it when they
are absent or different.  Release the pass's digest pins only after every
proposal has an authenticated terminal decision; public entries can only refuse
release.  Return the published decision metadata."
  (niwa-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((root (niwa-workspace-root workspace))
            (decision (niwa-memory-decision--read
                       workspace (file-name-concat (niwa-memory-decision--directory workspace) (concat id ".el")))))
       (unless (and decision (equal hash (plist-get decision :hash)))
         (error "Accepted decision publication changed"))
       (let* ((accepted (plist-get decision :accepted))
              (prepared (plist-get accepted :prepared))
              (review (plist-get accepted :review))
              (journal (niwa-journal-store-directory root))
              (path (file-name-concat journal (niwa-journal-store--filename review 'consolidation)))
              (text (niwa-journal-store--encode review nil 'consolidation))
              (observed (car (niwa-session-control-fs-run-program
                              (list (list :op 'read :optional t :coding 'utf-8-unix :path path))))))
         (unless (and (eq (plist-get observed :status) 'ok)
                      (equal (plist-get observed :value) text))
           ;; Public review text is derived from this authenticated bundle.
           ;; The immutable publisher only creates absent records; repair a
           ;; changed projection under mutation ownership before calling it.
           ;; Fence repair against settlement/takeover under their target lock.
           ;; The byte check also refuses changes since the observation.
           (when (eq (plist-get observed :status) 'ok)
             (dolist (result (niwa-session-control-fs-run-program
                             (list (list :op 'verify :path (niwa-journal-claim--path mutation nil)
                                         :content (json-serialize (niwa-journal-claim--record mutation)))
                                   (list :op 'absent :path (niwa-journal-claim--path mutation t))
                                   (list :op 'before-time :path (plist-get mutation :directory)
                                         :content (number-to-string (plist-get mutation :expires-at)))
                                   (list :op 'verify-hash :path path
                                         :content (secure-hash 'sha256 (plist-get observed :value)))
                                   (list :op 'write :path path :content text :coding 'utf-8-unix))
                             (plist-get mutation :directory)))
               (niwa-session-control-fs-program-value result)))
           (niwa-memory-store--publish-owned workspace accepted mutation))
         (niwa-memory-store--assert-owned mutation)
         (let ((published (niwa-journal-store-publish-decision root (plist-get decision :metadata)))
               (passes (make-hash-table :test #'equal)))
           ;; This bundle was authenticated under the mutation claim held here.
           (puthash (plist-get prepared :id) accepted passes)
           (when (and (plist-get prepared :entries)
                      (seq-some #'identity
                                (niwa-session-control-fs-paths-exist
                                 (mapcar (lambda (entry)
                                           (niwa-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id)))
                                         (plist-get prepared :entries))))
                      (niwa-memory-decision--settled-p workspace accepted passes))
             (niwa-memory-store--release-pins workspace prepared nil mutation))
           (setf (niwa-workspace-journal-observation workspace) nil)
           (niwa-journal-cleanup-schedule workspace t)
           published))))))

(defun niwa-memory-decision-recover (workspace)
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
    (dolist (entry (niwa-journal-store-entries (niwa-workspace-root workspace)))
      (puthash (plist-get entry :file) (plist-get entry :text) published))
    (let ((paths (niwa-session-control-fs-list-directory
                  (niwa-memory-decision--directory workspace) niwa-memory-decision--file-regexp)))
      (while paths
        (let ((batch (seq-take paths 8)))
          (setq paths (nthcdr (length batch) paths))
          (dolist (row (niwa-memory-decision--record-batch workspace batch))
            (when-let* ((record (cdr row)))
              (push row (gethash (plist-get (plist-get record :metadata) :pass-id) groups)))))))
    (maphash
     (lambda (_pass records)
       (dolist (row records)
         (when-let* ((decision (niwa-memory-decision--read workspace (car row) passes nil
                                                            (and records-current (cdr row)))))
           (let* ((metadata (plist-get decision :metadata))
                  (prepared (plist-get (plist-get decision :accepted) :prepared))
                  (review (plist-get (plist-get decision :accepted) :review))
                  (review-file (niwa-journal-store--filename review 'consolidation))
                  (review-text (niwa-journal-store--encode review nil 'consolidation))
                  (pass (plist-get prepared :id))
                  (file (niwa-journal-store--filename metadata 'decision))
                  (expected (niwa-journal-store--encode metadata nil 'decision))
                  ;; Pins outlive the pass until all its proposals are terminal;
                  ;; republishing earlier cannot release them.
                  (release (and (not (gethash pass checked))
                                (seq-some (lambda (entry)
                                            (niwa-session-control-fs-path-exists-p
                                             (niwa-memory-store--pin workspace pass (plist-get entry :id))))
                                          (plist-get prepared :entries))
                                (niwa-memory-decision--settled-p workspace (plist-get decision :accepted)))))
             (puthash pass t checked)
             (when (or release (not (equal expected (gethash file published)))
                       (not (equal review-text (gethash review-file published))))
               (niwa-memory-decision--publish workspace (plist-get metadata :decision-id) (plist-get decision :hash))
               (setq changed t records-current nil)
               (clrhash passes)
               (puthash file expected published)
               (puthash review-file review-text published))))))
     groups)
    changed))

(defun niwa-memory-decision--publish-accepted (workspace decision)
  "Publish accepted DECISION in WORKSPACE and return its public metadata.
A local automatic operation defers publication to its child, passing only
identity and hash, never accepted proofs."
  (let* ((metadata (plist-get decision :metadata)) (id (plist-get metadata :decision-id))
         (job niwa-memory-decision--publication-job))
    (if (and job (eq workspace (plist-get job :workspace))
             (equal (plist-get metadata :proposal-id) (plist-get job :application)))
        (progn
          (unless (niwa-journal-claim-settle (plist-get job :claim) 'completed (plist-get decision :hash))
            (error "Decision saved; publication operation lost its claim"))
          (plist-put job :publication
                     (list :decision-id id :hash (plist-get decision :hash)))
          (plist-get decision :metadata))
      (niwa-memory-decision--publish workspace id (plist-get decision :hash)))))

;;
;;; Decision transactions

(define-error 'niwa-memory-decision--held "Memory application is held")
(define-error 'niwa-memory-decision-interrupted "Memory application interrupted" 'quit)

(defconst niwa-memory-decision-automatic-idle-seconds 1
  "Editor idleness required before each child-assisted automatic decision.")

(defconst niwa-memory-decision--context-keys
  '(:workspace :root :origin :workspace-id :origin-session :origin-workspace :origin-root :automatic)
  "Context fields a continuation must match exactly: one actor and origin.")

(defun niwa-memory-decision--context (workspace origin &optional automatic)
  "Capture WORKSPACE and ORIGIN's expected context, not filesystem authority.
AUTOMATIC marks an automatic actor and adds its shared cancellation token."
  (let* ((session (buffer-local-value 'niwa--session origin))
         (other (and session (niwa-session-workspace session))))
    (append (list :workspace workspace :root (niwa-workspace-root workspace) :origin origin
                  :workspace-id (niwa-workspace-identity-read (niwa-workspace-root workspace))
                  :origin-session session :origin-workspace other
                  :origin-root (and other (niwa-workspace-root other)))
            (when automatic (list :automatic t :token (list :cancelled nil :operation nil))))))

(defun niwa-memory-decision-automatic-context (workspace origin)
  "Return an opaque automatic-decision binding of WORKSPACE to ORIGIN buffer.
This is an origin binding, not frozen permission: every automatic decision
rechecks ORIGIN's live session, workspace identity and `auto' mode at
admission and before each forward write.  Pass it as the AUTOMATIC argument of
the decision verbs; `niwa-memory-decision-cancel' stops work started with it."
  (niwa-memory-decision--context workspace origin t))

(defun niwa-memory-decision--operation-origin-current-p (job)
  "Return non-nil while JOB's live origin and workspace still match its context."
  (let ((origin (plist-get job :origin)) (other (plist-get job :origin-workspace)))
    (and (buffer-live-p origin)
         (equal (plist-get job :root) (niwa-workspace-root (plist-get job :workspace)))
         (eq (plist-get job :origin-session) (buffer-local-value 'niwa--session origin))
         (or (null (plist-get job :origin-session))
             (and (eq other (niwa-session-workspace (plist-get job :origin-session)))
                  (equal (plist-get job :origin-root) (and other (niwa-workspace-root other))))))))

(defun niwa-memory-decision-automatic-current-p (context)
  "Return non-nil while automatic CONTEXT's original actor still admits writes.
Stored identity is an expectation, not authority; read it afresh on each check.
A cancelled CONTEXT is never current."
  (cl-flet ((actor ()
              (and (not (plist-get (plist-get context :token) :cancelled))
                   (not niwa-journal-worker--stopping)
                   (niwa-memory-decision--operation-origin-current-p context)
                   (eq (buffer-local-value 'niwa-memory-consolidation-mode (plist-get context :origin)) 'auto))))
    (and (actor)
         (let ((niwa-workspace-identity--cache (make-hash-table :test #'equal)))
           (equal (plist-get context :workspace-id)
                  (niwa-workspace-identity-read (plist-get context :root))))
         ;; Identity IO may dispatch events; never reuse the actor check from
         ;; before that yield to admit a dead, reassigned or cancelled origin.
         (actor))))

(defun niwa-memory-decision-cancel (context)
  "Cancel automatic decisions started with CONTEXT.
Later forward writes refuse; an executing transaction unwinds through checked
rollback first.  Return non-nil when an operation will still deliver its
callback later, nil when nothing remains to deliver."
  (let ((token (plist-get context :token)))
    (plist-put token :cancelled t)
    (when-let* ((operation (plist-get token :operation)))
      (niwa-memory-decision--operation-stop operation)
      (and (not (plist-get operation :done)) (plist-get operation :executing) t))))

(defun niwa-memory-decision--recovery-successor-p (job predecessor)
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
                         niwa-memory-decision--context-keys))
      (let ((results
             (niwa-session-control-fs-run-program
              (list (list :op 'verify-latest :path (niwa-journal-claim--path claim nil) :content ".claim")
                    (list :op 'verify :path (niwa-journal-claim--path claim nil)
                          :content (json-serialize (niwa-journal-claim--record claim)))
                    (list :op 'absent :path (niwa-journal-claim--path claim t))
                    (list :op 'before-time :path (plist-get claim :directory)
                          :content (number-to-string (plist-get claim :expires-at)))
                    (list :op 'verify :path (niwa-journal-claim--path prior nil)
                          :content (json-serialize (niwa-journal-claim--record prior)))
                    (list :op 'verify :path (niwa-journal-claim--path prior t)
                          :content (json-serialize
                                    (plist-put (copy-sequence outcome) :status
                                               (symbol-name (plist-get outcome :status))))))
              (plist-get claim :directory))))
        (and (= 6 (length results))
             (cl-every (lambda (result) (eq (plist-get result :status) 'ok)) results))))))

(defun niwa-memory-decision--continuation (job outcome status)
  "Return the opaque continuation after JOB's clean OUTCOME with STATUS, or nil.
Carry only the exact settled claim and actor context, never accepted bundles,
public observations or target proofs.  The next claim must freshly prove direct
succession before it omits historical recovery.  Held, waiting and idle
admissions and failures reset continuity."
  (when (and outcome (not (plist-get job :decisions-only))
             (memq (plist-get outcome :status) '(completed cancelled))
             (memq status '(nil skipped applied rejected reversed stale unavailable)))
    (append (list :claim (copy-tree (plist-get job :claim)) :outcome (copy-tree outcome))
            (cl-loop for key in niwa-memory-decision--context-keys
                     append (list key (plist-get job key))))))

(defun niwa-memory-decision--snapshot (value)
  "Copy inert decision VALUE, including mutable strings and list tails."
  (cond ((stringp value) (copy-sequence value))
        ((consp value)
         (let ((tail value) result)
           (while (consp tail)
             (push (niwa-memory-decision--snapshot (car tail)) result)
             (setq tail (cdr tail)))
           (nconc (nreverse result) (niwa-memory-decision--snapshot tail))))
        (t value)))

(defun niwa-memory-decision--result (value &optional continuation)
  "Return the public result for internal decision VALUE and CONTINUATION.
VALUE is a public decision entry or metadata, a status plist, or :items.
The result's :decision is a detached copy of that public decision."
  (let ((metadata (niwa-memory-decision--snapshot (if (plist-member value :decision-id) value (plist-get value :decision)))))
    (append (list :status (plist-get value :status) :reason (plist-get value :reason)
                  :decision metadata :continuation continuation)
            (when (plist-member value :items) (list :items (plist-get value :items)))
            (when (plist-get value :error) (list :error (plist-get value :error))))))

(defun niwa-memory-decision--requirements (workspace proposal &optional observation)
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
      (let* ((selected (cdr (assoc id requirements)))
             (entry (niwa-memory-decision-status workspace id))
             (status (plist-get entry :status)))
        (when (and selected (not (equal id (plist-get selected :proposal-id))))
          (error "Selected extraction decision belongs to another proposal"))
        (cond ((eq status 'applied))
              ((null status) (setq waiting t))
              (t (push status failed)))))
    (cond (failed (list :status 'stale
                        :reason (format "A required memory extraction was not applied (%s); the note stays for review."
                                        (mapconcat #'symbol-name (delete-dups failed) ", "))))
          (waiting (list :status 'waiting
                         :reason "Apply the memory extractions from this note first.")))))

(defun niwa-memory-decision--application-input (accepted proposal &optional workspace observation hold)
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
         (check (and workspace (niwa-memory-decision--requirements workspace proposal observation))))
    (condition-case err
        (let ((root (niwa-memory-scope--root scope (plist-get item :root))))
          (when index
            (let* ((current (niwa-memory-scope--snapshot
                             (plist-get (cdr index) :path) niwa-memory-scope--max-file-bytes))
                   (conflicts (niwa-memory-apply--index-conflicts (cdr index) current item)))
              (if conflicts
                  (setq check (list :status 'stale :reason
                                    (format "MEMORY.md entries changed since capture: %s"
                                            (string-join conflicts ", "))))
                (setcdr index current)
                (setf (alist-get "MEMORY.md" (plist-get root :before) nil nil #'equal) current))))
          (unless check
            (setq check (niwa-memory-scope-check scope (plist-get item :root)
                                                   (mapcar #'car (plist-get item :before)))))
          (when (and hold workspace (eq (plist-get check :status) 'fresh)
                     (eq (plist-get item :action) 'note-delete)
                     (member (plist-get item :type) '("note" "attachment")))
            (when-let* ((reason (niwa-memory-notes-mutation-hold
                                workspace root (plist-get item :file) scope)))
              (setq check (list :status 'waiting :reason reason)))))
      (error (setq check (list :status 'unavailable :reason (error-message-string err)))))
    (append check (list :scope scope :proposal item))))

(defun niwa-memory-decision--hold (workspace item scope live)
  "Return a hold reason for ITEM from SCOPE in WORKSPACE, or nil to admit it.
LIVE, when non-nil, is the automatic forward-admission predicate.  A note an
open session uses, or whose visiting buffer has unsaved changes, is held.
This editor-local check runs no control program and records the hold for the
note's later retry."
  (if (and live (not (funcall live))) "Memory operation context changed"
    (when-let* (((niwa-memory-proposal-note-action-p (plist-get item :action)))
                (reason (cond ((niwa-memory-notes-in-use-p workspace (plist-get item :file))
                               "Shared file is in active local use")
                              ((seq-some (lambda (row)
                                           (when-let* ((buffer (find-buffer-visiting (plist-get (cdr row) :path))))
                                             (buffer-modified-p buffer)))
                                         (plist-get item :before))
                               "Shared file has unsaved buffer changes"))))
      (niwa-memory-notes-record-hold
       workspace (plist-get (niwa-memory-scope--root scope (plist-get item :root)) :dir)
       (plist-get item :file) reason)
      reason)))

(defun niwa-memory-decision--write-boundary (workspace item scope live)
  "Return the forward-write guard for ITEM from SCOPE in WORKSPACE.
Recheck LIVE and local use, then run the program-backed note mutation hold
\(age, version, references, extraction) freshly at the deletion boundary.
A hold unwinds the prepared write through a private signal."
  (lambda ()
    (when-let* ((reason (niwa-memory-decision--hold workspace item scope live)))
      (signal 'niwa-memory-decision--held (list reason)))
    (when (and (eq (plist-get item :action) 'note-delete)
               (member (plist-get item :type) '("note" "attachment")))
      (when-let* ((root (niwa-memory-scope--root scope (plist-get item :root)))
                  (reason (niwa-memory-notes-mutation-hold workspace root (plist-get item :file) scope)))
        (niwa-memory-notes-record-hold workspace (plist-get root :dir) (plist-get item :file) reason)
        (signal 'niwa-memory-decision--held (list reason))))))

(defun niwa-memory-decision--slot-eligible-p (accepted)
  "Whether ACCEPTED was prepared for decision slots."
  (eq (plist-get (plist-get (plist-get accepted :prepared) :scope) :decision-format) 'slots))

(defun niwa-memory-decision--preimage (workspace proposal index)
  "Return PROPOSAL's rebased index preimage path for output INDEX in WORKSPACE."
  (file-name-concat (niwa-memory-decision--directory workspace)
                    (concat proposal ".index-" (secure-hash 'sha256 (niwa-memory-store--print index)) ".el")))

(defun niwa-memory-decision--elect (workspace owner accepted item slots status reason plan)
  "Elect a decision in WORKSPACE using the exact lock-holding OWNER.
ACCEPTED binds ITEM to its pass.  SLOTS lists existing attempts in order;
STATUS and REASON describe the outcome; PLAN retains original before/after."
  (let* ((proposal (plist-get item :id))
         (id (pcase status
               ((or 'applied 'rejected) proposal)
               ('reversed (concat proposal ".reverse"))
               ((or 'stale 'unavailable)
                (concat proposal ".attempt-"
                        (number-to-string (1+ (if (and slots (string-match "\\.attempt-\\([0-9]+\\)\\.el\\'" (car slots)))
                                                   (string-to-number (match-string 1 (car slots))) 0)))))))
         (before (plist-get plan :before)) (after (plist-get plan :after))
         (directory (niwa-memory-decision--directory workspace))
         (metadata (list :decision-id id :proposal-id proposal :pass-id (plist-get (plist-get accepted :prepared) :id)
                         :workspace (plist-get (plist-get accepted :review) :workspace)
                         :created (niwa-journal-store-timestamp (niwa-session-control-fs-target-time directory))
                         :reason (niwa--truncate-bytes reason 4096 "...") :status status
                         :state-hash (secure-hash 'sha256 (niwa-memory-store--print (list before after)))))
         (data (list :source (plist-get accepted :hash) :metadata metadata :before before :after after))
         (text (niwa-memory-store--print data)))
    (niwa-journal-store--validate-metadata metadata 'decision)
    (mapc #'niwa-session-control-fs-program-value
          (niwa-session-control-fs-lock-session-run
           owner (list (list :op 'create :path (file-name-concat directory (concat id ".el")) :content text))))
    (append data (list :hash (secure-hash 'sha256 text)))))

(cl-defun niwa-memory-decision--transact (workspace pass proposal verb &key reason live automatic on-applied superseded-by observation)
  "Execute VERB for WORKSPACE's PASS and PROPOSAL under both host locks.
REASON accompanies rejection; LIVE rechecks automatic admission.  AUTOMATIC
skips already decided proposals.  Notify ON-APPLIED only for confirmed paths.
SUPERSEDED-BY identifies a newer accepted review of the same shared file.
OBSERVATION names required extractions; it never authorizes their disposal."
  (let* ((accepted (niwa-memory-store-accepted workspace pass t))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (item (seq-find (lambda (p) (equal (plist-get p :id) proposal)) (plist-get accepted :proposals)))
         (root-id (plist-get item :root))
         (directory (niwa-memory-decision--directory workspace))
         (journal (niwa-journal-store-directory (niwa-workspace-root workspace)))
         confirmed interrupted value)
    (unless item (error "Unknown accepted proposal"))
    (unless (niwa-memory-decision--slot-eligible-p accepted)
      (user-error "Decided or proposed before the decision-format upgrade; the next memory review will propose it again if still relevant"))
    (when (eq verb 'reverse)
      (niwa-memory-store--mutation workspace (lambda (_claim) (niwa-journal-cleanup-recover journal))))
    (niwa-session-control-fs-make-directory directory t)
    (setq value
          (niwa-session-control-fs-call-with-locks
           (list (niwa-memory-write--target scope root-id) (niwa-journal-store-claim-directory journal 'mutation))
           (lambda (target owner)
             (cl-block locked
               (when (and (eq verb 'reverse) (niwa-journal-cleanup-pending-pass-p journal pass))
                 (user-error "An accepted expiry is retiring this memory review"))
               (unless (equal accepted (niwa-memory-store-accepted workspace pass t))
                 (error "Accepted pass changed during lock admission"))
               (let* ((root (niwa-memory-scope--root scope root-id))
                      (note (eq (plist-get root :kind) 'notes))
                      (slots (niwa-memory-decision--slots workspace proposal))
                      (previous (and slots (niwa-memory-decision--read workspace (car slots))))
                      (status (plist-get (plist-get previous :metadata) :status))
                      (guard (niwa-memory-decision--write-boundary workspace item scope live))
                      (currentp (lambda () (and (niwa-memory-scope--root scope root-id)
                                                (process-live-p target) (process-live-p owner))))
                      (before (plist-get item :before)) observed plan preimage-path changes acknowledged)
                 (when (and automatic previous) (cl-return-from locked '(:status skipped)))
                 (when (and (niwa-memory-decision-terminal-status-p status)
                            (not (and (eq verb 'reverse) (eq status 'applied))))
                   (when (and (eq verb 'reverse) (not (eq status 'reversed))) (user-error "No application to reverse"))
                   (cl-return-from locked previous))
                 (when (eq verb 'reverse)
                   (unless (eq status 'applied) (user-error "No application to reverse"))
                   (when note (user-error "Shared-note deletion is permanent and cannot be reversed")))
                 (let* ((review (plist-get accepted :review))
                        (path (file-name-concat journal (niwa-journal-store--filename review 'consolidation)))
                        (expected (secure-hash 'sha256 (encode-coding-string (niwa-journal-store--encode review nil 'consolidation) 'utf-8-unix)))
                        (check (car (niwa-session-control-fs-lock-session-run
                                     owner (list (list :op 'verify-hash :path path :content expected))))))
                   (unless (memq (plist-get check :status) '(ok absent))
                     (user-error "Public memory review changed; inspect it before deciding")))
                 (when (eq verb 'apply)
                   (when-let* ((check (niwa-memory-decision--requirements workspace item observation)))
                     (cl-return-from locked
                       (if (eq (plist-get check :status) 'stale)
                           (niwa-memory-decision--elect workspace owner accepted item slots 'stale (plist-get check :reason) nil)
                         check)))
                   (when-let* ((hold (niwa-memory-decision--hold workspace item scope live)))
                     (cl-return-from locked (list :status 'held :reason hold))))
                 (niwa-tool-patch--assert-buffers-unmodified
                  (mapcar (lambda (row) (list :path (plist-get (cdr row) :path))) before))
                 (setq plan
                       (condition-case err
                           (progn
                             (setq observed (mapcar (lambda (row)
                                                      (cons (car row) (if note (niwa-memory-notes-observe (cdr row))
                                                                        (niwa-memory-scope--snapshot (plist-get (cdr row) :path)
                                                                                                        niwa-memory-scope--max-file-bytes)))) before))
                             (let* ((index (cdr (assoc "MEMORY.md" observed)))
                                    (path (and index (not (eq verb 'reverse)) (niwa-memory-decision--preimage workspace proposal index)))
                                    (saved (when (and path (niwa-session-control-fs-path-exists-p path))
                                             (car (niwa-memory-store--read-lisp path)))))
                               (niwa-memory-write--plan scope item observed saved (and (eq verb 'reverse) previous))))
                         (error (list :status 'unavailable :reason (error-message-string err)))))
                 (when superseded-by
                   (let* ((newer (niwa-memory-store-accepted workspace superseded-by t))
                          (generation (lambda (bundle) (plist-get (plist-get (plist-get bundle :prepared) :claim) :generation)))
                          (current (cdar observed)))
                     (unless (and note (eq verb 'reject) (plist-get current :exists)
                                  (> (funcall generation newer) (funcall generation accepted))
                                  (seq-some (lambda (p)
                                              (and (eq (plist-get p :action) (plist-get item :action))
                                                   (equal (plist-get p :file) (plist-get item :file))
                                                   (niwa-memory-notes-same-p current (cdar (plist-get p :before)))))
                                            (plist-get newer :proposals)))
                       (cl-return-from locked '(:status stale :reason "Shared file no longer matches the later review")))
                     ;; Only a changed regular file gets the exception.  A
                     ;; fresh proposal follows ordinary checked rejection.
                     (when (and (eq (plist-get plan :status) 'stale) (eq (plist-get item :action) 'note-delete)
                                (not (plist-get current :directory)))
                       (mapc #'niwa-session-control-fs-program-value
                             (niwa-session-control-fs-lock-session-run target (niwa-memory-write--verify current)))
                       (unless (funcall currentp) (error "Memory write owner lost before supersession"))
                       (cl-return-from locked (niwa-memory-decision--elect workspace owner accepted item slots 'rejected reason nil)))))
                 (unless (eq 'fresh (plist-get plan :status))
                   (when (eq verb 'reverse) (user-error "%s" (plist-get plan :reason)))
                   (cl-return-from locked
                     (niwa-memory-decision--elect workspace owner accepted item slots (plist-get plan :status) (plist-get plan :reason) nil)))
                 (when (and note (eq verb 'reject) (not (plist-get (cdar observed) :exists)))
                   (user-error "Shared file already gone; Accept records the observed deletion"))
                 (let ((index-before (cdr (assoc "MEMORY.md" (plist-get plan :before))))
                       (index-after (cdr (assoc "MEMORY.md" (plist-get plan :after)))))
                   (when (and index-before (not (equal index-before (cdr (assoc "MEMORY.md" before)))))
                     (setq preimage-path (niwa-memory-decision--preimage workspace proposal index-after))
                     (unless (eq verb 'reverse)
                       (let* ((text (niwa-memory-store--print index-before))
                              (reply (car (niwa-session-control-fs-lock-session-run owner (list (list :op 'create :path preimage-path :content text))))))
                         (if (eq (plist-get reply :status) 'conflict)
                             (unless (equal index-before (car (niwa-memory-store--read-lisp preimage-path)))
                               (error "Conflicting memory index preimage"))
                           (niwa-session-control-fs-program-value reply))))))
                 ;; A note capture can include text that its hash-only observation omits.
                 (unless (and note (eq verb 'reject))
                   (setq changes (niwa-memory-write--changes observed (plist-get plan (if (eq verb 'apply) :after :before)))))
                 (condition-case err
                     (cond (note
                            (if changes
                                (niwa-memory-notes-commit workspace root target changes currentp guard
                                                             (lambda () (setq acknowledged t)))
                              (mapc #'niwa-session-control-fs-program-value
                                    (niwa-session-control-fs-lock-session-run target (niwa-memory-write--verify (cdar observed))))))
                           (changes
                            (niwa-tool-patch-commit
                             changes currentp (lambda (old new) (niwa-memory-write--batch target (list old) (list new)))
                             (lambda (_old new)
                               (funcall guard)
                               (unless (funcall currentp) (error "Memory write owner lost"))
                               (niwa-memory-write--batch target (mapcar #'cdr observed) new t)
                               (when (and live (not (funcall live)))
                                 (signal 'niwa-memory-decision--held '("Memory operation context changed"))))))
                           (t (mapc #'niwa-session-control-fs-program-value
                                    (niwa-session-control-fs-lock-session-run target (cl-mapcan (lambda (row) (niwa-memory-write--verify (cdr row))) observed)))))
                   (niwa-memory-decision--held (cl-return-from locked (list :status 'held :reason (cadr err))))
                   (niwa-tool-patch-partial-rollback
                    (if (eq (car (cadr err)) 'quit)
                        (signal 'niwa-memory-decision-interrupted (list nil "Quit" (error-message-string err)))
                      (signal (car err) (cdr err))))
                   (quit
                    (unless acknowledged (signal (car err) (cdr err)))
                    (setq interrupted t))
                   (error
                    ;; A failed batch can report unchanged files, not infer a write
                    ;; from matching bytes or continue on a replacement owner.
                    (unless (and (or note (eq (car err) 'niwa-memory-apply-mode-refused))
                                 (not (eq verb 'reverse)) (funcall currentp)
                                 (cl-every (lambda (result) (eq 'ok (plist-get result :status)))
                                           (niwa-session-control-fs-lock-session-run
                                            target (cl-mapcan (lambda (row) (niwa-memory-write--verify (cdr row))) observed)))
                                 (funcall currentp))
                      (signal (car err) (cdr err)))
                    (cl-return-from locked (niwa-memory-decision--elect workspace owner accepted item slots
                                                                           'unavailable (error-message-string err) nil))))
                 (unless (funcall currentp) (error "Memory write owner lost before slot election"))
                 (prog1 (niwa-memory-decision--elect
                         workspace owner accepted item slots (pcase verb ('apply 'applied) ('reject 'rejected) ('reverse 'reversed))
                         (pcase verb ('reject (or reason "")) ('reverse "Reversed the captured memory application.")
                                (_ (if changes "Applied captured proposal." "Targets already held this accepted change."))) plan)
                   (when (eq verb 'apply) (setq confirmed (mapcar (lambda (c) (plist-get c :path)) changes)))
                   (when (and preimage-path (not (eq verb 'reverse)))
                     (mapc #'niwa-session-control-fs-program-value
                           (niwa-session-control-fs-lock-session-run owner (list (list :op 'delete-file :path preimage-path)))))))))))
    (when (and confirmed on-applied) (funcall on-applied confirmed))
    (when-let* ((metadata (plist-get value :metadata)))
      (setq value (condition-case nil (niwa-memory-decision--publish-accepted workspace value)
                    (niwa-memory-store-mutation-busy
                     (list :status (plist-get metadata :status) :reason (plist-get metadata :reason) :decision metadata
                           :error "Decision saved; public journal publication is waiting for the current mutation")))))
    (if interrupted (signal 'niwa-memory-decision-interrupted (list value "Quit")) value)))

(cl-defun niwa-memory-decision--apply-owned (workspace _job pass proposal &key on-applied live automatic observation)
  "Apply PROPOSAL from PASS in WORKSPACE, rechecking under its locks.
ON-APPLIED reports confirmed paths; LIVE checks AUTOMATIC admission.
OBSERVATION identifies required extractions."
  (niwa-memory-decision--transact workspace pass proposal 'apply :on-applied on-applied :live live
                                     :automatic automatic :observation observation))

(defun niwa-memory-decision--deliver (callback function)
  "Call FUNCTION synchronously and deliver its result to CALLBACK once.
Errors and cancellation are delivered as :error or :cancelled results,
retaining an already accounted decision's status."
  (funcall callback
           (condition-case err
               (funcall function)
             (niwa-memory-decision-interrupted
              (append (niwa-memory-decision--result (cadr err))
                      (list :cancelled t :reason (caddr err))
                      (when (cadddr err) (list :error (cadddr err)))))
             (quit (list :cancelled t :reason (error-message-string err)))
             (error (list :error (error-message-string err))))))

(defun niwa-memory-decision--dispatch (callback start)
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

(cl-defun niwa-memory-decision-apply (workspace pass proposal &key continuation automatic on-applied callback)
  "Apply accepted PROPOSAL from PASS in WORKSPACE under the target and slot locks.
Stale targets produce an immutable decision without writes.  Accept or Reject
can resolve an interrupted application from the verified file states.
ON-APPLIED receives changed paths only for a newly confirmed application.

Return a result plist: :status (applied, rejected, reversed, stale,
unavailable, waiting, held, skipped or idle-wait),
optional :reason, :decision metadata and :continuation.  Pass that
continuation to the next decision of the same explicit serial request (one
actor and origin) to skip historical recovery when the claim chain proves
direct succession; omit it for full recovery.  It is opaque and ephemeral,
never authority, and is nil after held or waiting admission, failure,
interruption or a changed origin.

AUTOMATIC is a context from `niwa-memory-decision-automatic-context': live
auto policy is rechecked at admission and every forward write, and a proposal
that already has a decision is skipped.  Without CALLBACK the call is
synchronous and signals errors and quit; automatic cancellation signals
`niwa-memory-decision-interrupted' carrying any accounted decision.
With CALLBACK, the result, an :error or a :cancelled result is delivered
exactly once, retaining confirmed effects.  AUTOMATIC plus CALLBACK on a local
Linux workspace selects child-assisted recovery and selection, waiting for
editor idleness (status `idle-wait' defers); otherwise execution stays in the
editor.  A callback alone grants no automatic authority."
  (let ((context (or automatic (niwa-memory-decision--context workspace (current-buffer)))))
    (cond
     ((and automatic callback)
      (niwa-memory-decision--dispatch
       callback
       (lambda (complete)
        (if (not (niwa-memory-decision-automatic-current-p automatic))
            (funcall complete '(:status held :reason "Automatic application is no longer current"))
          (niwa-memory-decision--start-operation
       workspace
       (lambda (job live reply)
         (if (if (plist-member job :idle-admitted) (not (plist-get job :idle-admitted))
               (not (or (plist-get reply :synchronous) (niwa-memory-decision--idle-p))))
             ;; Recovery yielded to the editor.  Release this fixed claim before
             ;; waiting for idle; the caller retains only the proposal identity.
             '(:status idle-wait)
           (niwa-memory-decision--apply-owned
            workspace job pass proposal :on-applied on-applied :live live :automatic t
            :observation (unless (plist-get reply :synchronous) reply))))
       (lambda (reply) (funcall complete (niwa-memory-decision--public-reply reply)))
       (lambda () (niwa-memory-decision-automatic-current-p automatic))
       nil (list :application t :pass-id pass :proposal-id proposal) continuation automatic)))))
     (callback
      (niwa-memory-decision--deliver
       callback (lambda () (niwa-memory-decision-apply workspace pass proposal
                                                          :continuation continuation :on-applied on-applied))))
     (t
      (niwa-session-control-fs-with-reuse
       (niwa-memory-decision--result (niwa-memory-decision--apply-owned
                                         workspace nil pass proposal :on-applied on-applied :automatic automatic
                                         :live (and automatic (lambda () (niwa-memory-decision-automatic-current-p context))))))))))

(defun niwa-memory-decision--idle-p ()
  "Return non-nil when the editor has been idle long enough for automatic work."
  (and (not (input-pending-p))
       (when-let* ((idle (current-idle-time)))
         (>= (float-time idle) niwa-memory-decision-automatic-idle-seconds))))

(defun niwa-memory-decision--pending-items (workspace skip)
  "Return pending automatic proposal identities in WORKSPACE, excluding pass SKIP.
Authenticate status within this read-only selection to omit every decided
proposal.  Retain only identities, never application authority; each later
attempt still freshly reads accepted proof and status."
  (let ((entries (niwa-journal-store-entries (niwa-workspace-root workspace)))
        (passes (make-hash-table :test #'equal)) items)
    (dolist (accepted (niwa-memory-notes-passes workspace entries))
      (clrhash passes)
      (let ((pass (plist-get (plist-get accepted :prepared) :id)))
        (puthash pass accepted passes)
        (when (and (niwa-memory-decision--slot-eligible-p accepted) (not (equal pass skip))
                   (eq 'auto (plist-get (cdr (niwa-memory-notes-root
                                              (plist-get (plist-get accepted :prepared) :scope))) :mode)))
          (dolist (proposal (plist-get accepted :proposals))
            (unless (niwa-memory-decision-status workspace (plist-get proposal :id) entries passes)
              (push (list :pass pass :proposal (plist-get proposal :id) :pending t
                          :note (niwa-memory-proposal-note-action-p (plist-get proposal :action))
                          :file (plist-get proposal :file))
                    items))))))
    (nreverse items)))

(cl-defun niwa-memory-decision-pending (workspace &key automatic skip continuation callback)
  "Select WORKSPACE's undecided automatic proposals after owned recovery.
Only reviews whose frozen mode was `auto' qualify; pass SKIP is omitted.
The result's :items hold identities (:pass :proposal :note :file) only, never
authority.  AUTOMATIC, CONTINUATION and CALLBACK are as for
`niwa-memory-decision-apply'.  With both AUTOMATIC and CALLBACK on a local
workspace, owned recovery and selection run in a child.  Otherwise selection
is a read-only observation without a claim; it returns CONTINUATION unchanged."
  (if (and automatic callback)
      (niwa-memory-decision--dispatch
       callback
       (lambda (complete)
        (niwa-memory-decision--start-operation
       workspace
       (lambda (_job _live reply)
         (list :items (if (plist-get reply :synchronous)
                          (niwa-memory-decision--pending-items workspace skip)
                        (plist-get reply :items))))
       (lambda (reply) (funcall complete (niwa-memory-decision--public-reply reply)))
       (lambda () (niwa-memory-decision-automatic-current-p automatic))
       nil (list :pending-selection t :skip skip) continuation automatic)))
    ;; Synchronous selection is read-only and takes no claim, so CONTINUATION
    ;; stays valid; each later decision rechecks status under ownership.
    (let ((result (list :items (niwa-memory-decision--pending-items workspace skip)
                        :continuation continuation)))
      (if callback (funcall callback result) result))))

(cl-defun niwa-memory-decision-reject (workspace pass proposal &key reason continuation)
  "Reject PROPOSAL from PASS in WORKSPACE with optional REASON.
Acquire workspace ownership, recover accepted state, and verify original target
authority.  A repeated rejection returns the existing terminal decision.
A marked write for PROPOSAL must be recovered first.  REASON is limited to
4 KiB.  The result and CONTINUATION are as for `niwa-memory-decision-apply'."
  (setq reason (or reason ""))
  (unless (and (stringp reason) (<= (string-bytes reason) 4096)) (error "Rejection reason exceeds its byte limit"))
  (ignore continuation)
  (niwa-session-control-fs-with-reuse
   (niwa-memory-decision--result (niwa-memory-decision--transact workspace pass proposal 'reject :reason reason))))

(defun niwa-memory-decision-rejections (workspace)
  "Return at most 8 KiB of recent rejection evidence for WORKSPACE's next pass.
Include original proposed content and the user's reason, under captured target
authority. Omit whole records when unavailable or too large and report counts.
Evidence can help recognize reworded suggestions; it grants no instruction
authority and promises no permanent semantic suppression."
  (let ((rows nil) (omitted 0))
    (cl-labels ((encode (items count)
                  (decode-coding-string
                   (json-serialize (list :rejections (vconcat items) :omitted count)) 'utf-8-unix)))
      (dolist (entry (niwa-journal-store-entries (niwa-workspace-root workspace)))
        (when (and (eq (plist-get entry :kind) 'decision) (eq (plist-get entry :status) 'rejected)
                   (not (niwa-journal-cleanup-pass-retired-p (niwa-workspace-root workspace) (plist-get entry :pass-id))))
          (let ((row
                 (condition-case nil
                     (let* ((accepted (plist-get (niwa-memory-decision--published workspace entry) :accepted))
                            (proposal (seq-find (lambda (proposal) (equal (plist-get entry :proposal-id) (plist-get proposal :id)))
                                                (plist-get accepted :proposals)))
                            (root (niwa-memory-scope--root (plist-get (plist-get accepted :prepared) :scope)
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

(cl-defun niwa-memory-decision-reverse (workspace pass proposal &key continuation)
  "Reverse the accepted application of PROPOSAL from PASS in WORKSPACE.
Restore targets still at their exact after-state, including the shared index.
Targets already restored need no write; a third state refuses reversal.
Elect a separate reversal slot and retain all prior decisions.
Shared-note disposal is permanent.  Reversing an extraction never restores
notes.  The result and CONTINUATION are as for `niwa-memory-decision-apply'."
  (ignore continuation)
  (niwa-session-control-fs-with-reuse
   (niwa-memory-decision--result (niwa-memory-decision--transact workspace pass proposal 'reverse))))

(defun niwa-memory-decision--worker-local-p (workspace &optional display)
  "Whether WORKSPACE's authenticated retained scopes need no remote target IO.
Current configuration alone cannot classify historical original-root authority.
Read only local acceptance metadata here, before target-sensitive recovery.
DISPLAY checks only workspace locality: the display collector classifies each
authenticated bundle before target IO, and reports invalid bundles unavailable."
  (if display
      (not (file-remote-p (niwa-workspace-root workspace)))
    (condition-case nil
      (not (cl-some
            (lambda (accepted)
              (cl-some (lambda (root) (file-remote-p (plist-get (cdr root) :dir)))
                       (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
            (niwa-memory-notes-passes
             workspace (niwa-journal-store-entries (niwa-workspace-root workspace)))))
    ;; Recovery, not this metadata preflight, owns interrupted accepted expiry
    ;; and incomplete state.  Uncertainty selects the checked native path.
      (error nil))))

(defun niwa-memory-decision--operation-live-p (job)
  "Return non-nil while JOB can admit its next forward editor operation."
  (and (not (plist-get job :done)) (not (plist-get job :stopped))
       (not niwa-journal-worker--stopping)
       (eq job (gethash (plist-get (plist-get job :claim) :directory)
			niwa-memory-decision--operations))
       (niwa-memory-decision--operation-origin-current-p job)
       ;; The durable identity API caches immutable IDs for ordinary saves.
       ;; Recovery may yield across a replaced workspace: compare its expected
       ;; identity with a fresh read, never that process-wide cache.
       (let ((niwa-workspace-identity--cache (make-hash-table :test #'equal)))
         (equal (plist-get job :workspace-id)
                (niwa-workspace-identity-read (plist-get job :root))))
       (or (null (plist-get job :currentp)) (funcall (plist-get job :currentp)))))

(defun niwa-memory-decision--operation-fence (claim)
  "Establish a durable cancellation, competing outcome or expiry for CLAIM.
A nil settlement is not proof.  Authenticate an existing outcome, or elect
expiry only after freshly observing CLAIM's original target-clock deadline."
  (or (niwa-journal-claim-settle claim 'cancelled "")
      (niwa-journal-claim-outcome claim)
      (when (>= (niwa-session-control-fs-target-time (plist-get claim :directory))
                (plist-get claim :expires-at))
        (niwa-journal-claim--finish claim 'expired "")
        (niwa-journal-claim-outcome claim))
      (error "No durable memory operation fence")))

(defun niwa-memory-decision--stamp-round-trip (job)
  "Record JOB's recovery round-trip milliseconds once and clear its start."
  (when-let* ((start (plist-get job :recovery-started-at)))
    (plist-put job :recovery-round-trip-ms
               (round (* 1000 (max 0 (- (niwa-telemetry--monotonic-now) start)))))
    (plist-put job :recovery-started-at nil)))

(defun niwa-memory-decision--operation-finish (job result)
  "Release JOB and deliver RESULT exactly once, after any editor work unwinds."
  (unless (plist-get job :done)
    (plist-put job :done t)
    (when-let* ((retry (plist-get job :dispatch-retry))) (cancel-timer retry))
    (niwa-memory-decision--stamp-round-trip job)
    (when (plist-member job :recovery-round-trip-ms)
      (plist-put result :recovery-round-trip-ms (plist-get job :recovery-round-trip-ms)))
    (when (plist-get job :cancelled)
      (plist-put result :cancelled t)
      (plist-put result :reason (or (plist-get result :reason) (plist-get job :stopped))))
    (when-let* ((failure (plist-get job :stop-error)))
      (plist-put result :error (if-let* ((prior (plist-get result :error))) (concat prior "; " failure) failure)))
    (let ((cancelled (plist-get result :cancelled)) failure fenced)
      (condition-case err
          (setq fenced (niwa-memory-decision--operation-fence (plist-get job :claim)))
        ((error quit)
         (when (eq (car err) 'quit) (setq cancelled t))
         (setq failure (error-message-string err))
         ;; Settlement is an idempotent outcome election.  One bounded retry
         ;; may establish the fence after transient IO; still report the error.
         (condition-case retry
             (setq fenced (niwa-memory-decision--operation-fence (plist-get job :claim)))
           ((error quit)
            (when (eq (car retry) 'quit) (setq cancelled t))
            (setq failure (concat failure "; " (error-message-string retry)))))))
      (unwind-protect
          (condition-case err
              ;; Never delete an unfenced child.  On persistent storage failure
              ;; it stays tracked until the original deadline's final fencing
              ;; attempt.  Persistent inability to prove a fence stays visible.
              (when fenced
                (when-let* ((worker (plist-get job :worker)))
                  (niwa-journal-worker-cancel worker)))
            ((error quit)
             (when (eq (car err) 'quit) (setq cancelled t))
             (setq failure (concat failure "; " (error-message-string err)))))
        (let ((key (plist-get (plist-get job :claim) :directory)))
          (when (eq job (gethash key niwa-memory-decision--operations))
            (remhash key niwa-memory-decision--operations))))
      (when (and fenced (plist-get job :recovered) (not (plist-get job :publication)) (plist-get result :ok)
                 (not failure) (not (plist-get result :error))
                 (not (plist-get result :held)) (not (plist-get result :cancelled))
                 (not (plist-get job :stopped)))
        ;; Application items still need a selection child.
        (plist-put result :recovery
                   (niwa-memory-decision--continuation
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
    (if (niwa-memory-decision--operation-origin-current-p job)
        (with-current-buffer (plist-get job :origin) (funcall (plist-get job :callback) result))
      (with-temp-buffer (funcall (plist-get job :callback) result)))))

(defun niwa-memory-decision--operation-retire (job)
  "Try terminal fencing and child retirement at JOB's original fixed deadline."
  (when (plist-get job :unsettled)
    (condition-case err
        (progn
          ;; Nil settlement is not proof either: authenticate the exact token's
          ;; durable outcome before deleting any still-live child.
          (niwa-memory-decision--operation-fence (plist-get job :claim))
          (plist-put job :unsettled nil)
          (when-let* ((worker (plist-get job :worker)))
            (niwa-journal-worker-cancel worker)))
      ((error quit)
       (plist-put job :cleanup-error (error-message-string err))
       (niwa-diagnostic-warning (plist-get job :diagnostic-scope) 'niwa
                        (format "Memory worker retirement remains unresolved: %s" (error-message-string err))
                        :warning)))))

(defun niwa-memory-decision--operation-deadline (job)
  "Stop JOB or retire an unresolved child at its original unrenewed deadline."
  (unwind-protect
      (unless (plist-get job :done)
        (niwa-memory-decision--operation-stop job "Memory operation timed out"))
    (when (plist-get job :done) (niwa-memory-decision--operation-retire job))))

(defun niwa-memory-decision--operation-stop (job &optional reason)
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
      (niwa-memory-decision--operation-finish job
							 (or (plist-get job :result) (list :reason (plist-get job :stopped)))))))

(defun niwa-memory-decision--public-reply (reply)
  "Return the public verb result for internal operation REPLY.
Keep :error, :cancelled and timing alongside the decision's result."
  (let ((result (niwa-memory-decision--result (plist-get reply :value) (plist-get reply :recovery))))
    (when (and (plist-get reply :held) (null (plist-get result :status)))
      (plist-put result :status 'held))
    (unless (plist-get result :reason)
      (plist-put result :reason (plist-get reply :reason)))
    (append result
            (cl-loop for key in '(:error :cancelled :unsettled :recovery-round-trip-ms)
                     when (plist-get reply key) append (list key (plist-get reply key))))))

(defun niwa-memory-decision--publication-ready (job reply)
  "Finish JOB's targeted publication from REPLY without repeating target work."
  (when (and (not (plist-get job :done)) (not (plist-get job :executing)) (plist-get job :publication)
             (eq job (gethash (plist-get (plist-get job :claim) :directory)
                              niwa-memory-decision--operations)))
    (plist-put job :executing t)
    (let ((result (plist-get job :result)))
      (condition-case err
          (progn
            (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
            (unless (and (plist-get reply :ok)
                         (equal (plist-get reply :operation-id) (plist-get (plist-get job :claim) :owner))
                         (equal (plist-get reply :publication) (plist-get job :publication))
                         (equal (plist-get (niwa-journal-claim-outcome (plist-get job :claim)) :payload)
                                (plist-get (plist-get job :publication) :hash)))
              (error "Invalid memory decision publication reply"))
            (unless (niwa-memory-decision--operation-live-p job)
              (signal 'niwa-memory-decision--held '("Memory operation context changed")))
            (plist-put job :publication nil)
            (setf (niwa-workspace-journal-observation (plist-get job :workspace)) nil)
            (niwa-journal-cleanup-schedule (plist-get job :workspace) t))
        (niwa-memory-decision--held (plist-put result :held t))
        (quit (plist-put result :cancelled t))
        (error (plist-put result :error
                          (if-let* ((prior (plist-get result :error)))
                              (concat prior "; " (error-message-string err))
                            (error-message-string err)))))
      (plist-put job :executing nil)
      (niwa-memory-decision--operation-finish job result))))

(defun niwa-memory-decision--queue-application (job reply)
  "Queue REPLY for JOB at the command loop, once and without holding locks."
  (plist-put job :dispatch-retry nil)
  (unless (or (plist-get job :event-reply) (plist-get job :done) (plist-get job :stopped))
    (plist-put job :event-reply reply)
    (plist-put job :input-count num-input-keys)
    (push 'niwa-memory-application unread-command-events)))

(defun niwa-memory-decision--dispatch-application ()
  "Resume one admitted automatic transaction from the command loop."
  (interactive)
  (when-let* ((job (catch 'pending
                     (maphash (lambda (_ job) (when (plist-get job :event-reply) (throw 'pending job)))
                              niwa-memory-decision--operations)
                     nil))
              (reply (plist-get job :event-reply)))
    (plist-put job :event-reply nil)
    (if (or inhibit-quit this-command (active-minibuffer-window) (> (recursion-depth) 0))
        ;; Input readers within an unrelated command also dispatch special events.
        (plist-put job :dispatch-retry
                   (run-with-idle-timer niwa-memory-decision-automatic-idle-seconds nil
                                        #'niwa-memory-decision--queue-application job reply))
      ;; The special event itself ends measured idle time, but consumes no input key.
      (plist-put job :idle-admitted
                 (and (= num-input-keys (plist-get job :input-count)) (not (input-pending-p))))
      (if (plist-get job :idle-admitted) (niwa-memory-decision--operation-ready job reply)
        (niwa-memory-decision--operation-finish job (list :ok t :value '(:status idle-wait)))))))

(define-key special-event-map [niwa-memory-application] #'niwa-memory-decision--dispatch-application)

(defun niwa-memory-decision--receive (job reply)
  "Route an admitted application REPLY for JOB out of its sentinel or timer."
  (if (and inhibit-quit (plist-get job :application) (plist-get reply :ok)
           (equal (plist-get reply :operation-id) (plist-get (plist-get job :claim) :owner))
           (or (plist-get reply :synchronous) (niwa-memory-decision--idle-p)))
      (unless (plist-get job :dispatch-retry) (niwa-memory-decision--queue-application job reply))
    (niwa-memory-decision--operation-ready job reply)))

(defun niwa-memory-decision--operation-ready (job reply)
  "Continue JOB from worker REPLY with fresh ownership and live editor checks."
  (when (and (not (plist-get job :done)) (not (plist-get job :executing))
             (not (plist-get job :publication))
             (not (plist-get job :stopped))
             (eq job (gethash (plist-get (plist-get job :claim) :directory)
                              niwa-memory-decision--operations)))
    (plist-put job :worker nil)
    (plist-put job :executing t)
    (let ((result
           (condition-case err
               (progn
                 (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
                 (unless (plist-get reply :ok) (error "Invalid memory recovery reply"))
                 (unless (equal (plist-get reply :operation-id) (plist-get (plist-get job :claim) :owner))
                   (error "Memory recovery reply belongs to another operation"))
                 (unless (niwa-memory-decision--operation-live-p job)
                   (signal 'niwa-memory-decision--held '("Memory operation context changed")))
                 (niwa-memory-store--assert-owned (plist-get job :claim))
                 (with-current-buffer (plist-get job :origin)
                   (when (and (plist-get reply :synchronous) (not (plist-get job :recovered)))
                     ;; Historical remote roots retain their original authority
                     ;; even after configuration changes.  Keep their recovery
                     ;; in the editor, under this same continuously held claim.
                     (niwa-memory-decision-recover (plist-get job :workspace))
                     (unless (plist-get job :decisions-only)
                       (niwa-memory-store-recover (plist-get job :workspace)))
                     (unless (niwa-memory-decision--operation-live-p job)
                       (signal 'niwa-memory-decision--held '("Memory operation context changed")))
                     (niwa-memory-store--assert-owned (plist-get job :claim)))
                   (plist-put job :recovered t)
                   (niwa-memory-decision--stamp-round-trip job)
                   (let ((niwa-memory-decision--publication-job
                          (and (plist-get job :application) (not (plist-get reply :synchronous)) job)))
                     (list :ok t :value
                           (funcall (plist-get job :function) job
                                    (lambda () (niwa-memory-decision--operation-live-p job)) reply)))))
             (niwa-memory-decision--held (list :held t :reason (cadr err)))
             (niwa-memory-decision-interrupted
              (append (when (cadr err) (list :ok t :value (cadr err)))
                      (list :cancelled t :reason (caddr err))
                      (cond ((cadddr err) (list :error (cadddr err)))
                            ((and (cadr err) (not (niwa-memory-decision-terminal-status-p (plist-get (cadr err) :status))))
                             (list :error "Interrupted memory write requires checked recovery")))))
             (quit (list :cancelled t :reason (error-message-string err)))
             (error (list :error (error-message-string err))))))
      (plist-put job :executing nil)
      (plist-put job :result result)
      (if (and (plist-get job :publication) (plist-get result :ok))
          (condition-case err
              (niwa-journal-worker-start
               (plist-get job :workspace) 'memory-decision-publish
               (append (list :operation-id (plist-get (plist-get job :claim) :owner) :claim (plist-get job :claim))
                           (plist-get job :publication))
               (lambda (reply) (niwa-memory-decision--publication-ready job reply)) :owner job)
            ((error quit)
             (if (eq (car err) 'quit) (plist-put result :cancelled t)
               (plist-put result :error
                          (if-let* ((prior (plist-get result :error)))
                              (concat prior "; " (error-message-string err))
                            (error-message-string err))))
             (niwa-memory-decision--operation-finish job result)))
        (niwa-memory-decision--operation-finish job result)))))

(defun niwa-memory-decision--start-operation (workspace function callback &optional currentp decisions-only payload recovery automatic)
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
  (let* ((context (or automatic (niwa-memory-decision--context workspace (current-buffer))))
         (key (niwa-memory-store--claim-directory workspace))
         (claim (or (niwa-journal-claim-acquire key 180) (user-error "Memory consolidation is busy")))
         (job (append (list :claim claim :function function :callback callback :currentp currentp
                            :diagnostic-scope (niwa-diagnostic-scope (plist-get context :root))
                            :application (and (plist-get payload :application) (plist-get payload :proposal-id))
                            :decisions-only decisions-only)
                      (copy-sequence context))))
    (puthash key job niwa-memory-decision--operations)
    (when-let* ((token (plist-get automatic :token))) (plist-put token :operation job))
    (condition-case err
        (progn
          (plist-put job :timer
                     (run-at-time
                      (max 0 (- (plist-get claim :expires-at)
                                (niwa-session-control-fs-target-time key))) nil
                      #'niwa-memory-decision--operation-deadline job))
          (when (and (or (null payload) (plist-get payload :pending-selection) (plist-get payload :application))
                     (niwa-memory-decision--recovery-successor-p job recovery))
            (plist-put job :recovered t))
          (unless (plist-get job :recovered)
            (plist-put job :recovery-started-at (niwa-telemetry--monotonic-now)))
          (if (and (plist-get job :recovered) (null payload))
              (niwa-memory-decision--operation-ready
               job (list :ok t :operation-id (plist-get claim :owner)))
            (if (niwa-journal-worker-local-p workspace)
              (niwa-journal-worker-start
               workspace 'memory-recover
               (append (list :claim claim :operation-id (plist-get claim :owner)
				 :decisions-only decisions-only
                                 :recovery-predecessor
                                 (when (plist-get job :recovered)
                                   (list :claim (plist-get recovery :claim) :outcome (plist-get recovery :outcome))))
                           payload)
               (lambda (reply) (niwa-memory-decision--receive job reply)) :owner
               job)
            (niwa-memory-decision--receive
             job (list :ok t :operation-id (plist-get claim :owner) :synchronous t)))))
      ((error quit)
       ;; Synchronous fallback also delivers the consumer callback here.  Once
       ;; released, its condition is not a second operation result to swallow.
       (if (plist-get job :done) (signal (car err) (cdr err))
         (niwa-memory-decision--operation-finish
          job (if (eq (car err) 'quit) (list :cancelled t :reason (error-message-string err))
                (list :error (error-message-string err)))))))
    job))

(defun niwa-memory-decision--recovery-failed (scope key reason)
  "Report failed recovery in SCOPE at claim directory KEY because of REASON.
Recovery runs after every root turn, so each distinct failure warns once and
later repeats go to *Messages*.  Successful recovery forgets reported failures."
  (niwa--warn-once (niwa-diagnostic-scope scope) (list 'memory-recovery key reason)
                      "Memory recovery failed: %s.  Memory review waits until recovery succeeds"
                      reason))

(defun niwa-memory-decision--recovery-release (job)
  "Release JOB's publication claim before retiring its child.
An unproved fence leaves the original deadline and tracked child intact."
  (unless (plist-get job :released)
    (condition-case err
        (progn
          (niwa-memory-decision--operation-fence (plist-get job :claim))
          (plist-put job :released t)
          (plist-put job :unsettled nil))
      ((error quit)
       (plist-put job :unsettled t)
       (signal (car err) (cdr err)))))
  (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
  (when-let* ((worker (plist-get job :worker)))
    (niwa-journal-worker-cancel worker)))

(defun niwa-memory-decision--recovery-deadline (workspace job)
  "Finish or retire publication recovery JOB at WORKSPACE's original deadline."
  (unwind-protect
      (unless (plist-get job :result-handled)
        (niwa-memory-decision--recovered workspace job '(:error "Memory recovery timed out")))
    (when (plist-get job :unsettled)
      (niwa-memory-decision--operation-retire job))))

(defun niwa-memory-decision--recovered (workspace job result)
  "Finish JOB's publication recovery from RESULT before admitting its callback."
  (let* ((claim (plist-get job :claim))
         (key (plist-get claim :directory))
         (changed (plist-get result :changed)))
    (when (and (eq job (gethash key niwa-memory-decision--recovery-workers))
               (not (plist-get job :result-handled)))
      (plist-put job :result-handled t)
      (unwind-protect
          (unless niwa-journal-worker--stopping
            (condition-case err
                (progn
                  (when (plist-get result :error) (error "%s" (plist-get result :error)))
                  (unless (and (eq t (plist-get result :ok))
                               (equal (plist-get result :operation-id) (plist-get claim :owner))
                               (if (plist-member result :synchronous)
                                   (and (eq t (plist-get result :synchronous)) (not (plist-member result :changed)))
                                 (and (plist-member result :changed) (memq changed '(t nil)))))
                    (error "Invalid memory recovery reply"))
                  (unless (funcall (plist-get job :currentp))
                    (signal 'niwa-memory-decision--held '("Recovery context changed")))
                  (unless (eq job (gethash key niwa-memory-decision--recovery-workers))
                    (signal 'niwa-memory-decision--held '("Recovery was cancelled")))
                  (niwa-memory-store--assert-owned claim)
                  (when (plist-get result :synchronous)
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key niwa-memory-decision--recovery-workers)))
                      (signal 'niwa-memory-decision--held '("Recovery context changed")))
                    (when (niwa-memory-decision-recover workspace) (setq changed t))
                    (unless (and (funcall (plist-get job :currentp))
                                 (eq job (gethash key niwa-memory-decision--recovery-workers)))
                      (signal 'niwa-memory-decision--held '("Recovery context changed")))
                    (when (niwa-memory-store-recover workspace) (setq changed t))
                    (niwa-memory-store--assert-owned claim))
                  (niwa-memory-decision--recovery-release job)
                  (unless (and (funcall (plist-get job :currentp))
                               (eq job (gethash key niwa-memory-decision--recovery-workers)))
                    (signal 'niwa-memory-decision--held '("Recovery context changed")))
                  ;; A no-op recovery leaves committed state and its displays current.
                  (when changed (niwa-memory-decision--recovery-changed workspace))
                  (remhash key niwa-memory-decision--recovery-workers)
                  (niwa--warn-once-reset-site (plist-get job :diagnostic-scope) 'memory-recovery)
                  (when-let* ((callback (plist-get job :callback)) (origin (plist-get job :origin)) ((buffer-live-p origin)))
                    (with-current-buffer origin (funcall callback))))
              (niwa-memory-decision--held nil)
              ((error quit) (niwa-memory-decision--recovery-failed (plist-get job :diagnostic-scope) key (error-message-string err)))))
        (when (eq job (gethash key niwa-memory-decision--recovery-workers))
          (remhash key niwa-memory-decision--recovery-workers))
        (unless (plist-get job :released)
          (condition-case err
              (niwa-memory-decision--recovery-release job)
            ((error quit) (niwa-diagnostic-warning (plist-get job :diagnostic-scope) 'niwa
                                    (format "Memory recovery release remains unresolved: %s" (error-message-string err))
                                    :warning))))))))

(defun niwa-memory-decision--start-recovery (workspace callback origin currentp original-key)
  "Start owned publication recovery for WORKSPACE, CALLBACK and ORIGIN buffer.
CURRENTP validates the scheduled opportunity across admission IO.
ORIGINAL-KEY requires that exact canonical claim path.  Unsupported workers
use synchronous publication recovery under the same claim and deadline."
  (let ((key (niwa-memory-store--claim-directory workspace))
        (scope (niwa-diagnostic-scope workspace)))
    (when-let* (((funcall currentp))
                ((equal key original-key))
                (claim (niwa-journal-claim-acquire key 180)))
      (let ((job (list :claim claim :callback callback :origin origin :currentp currentp
                       :diagnostic-scope scope)))
        ;; Claim IO can dispatch cancellation or a successor. Hand the exact
        ;; token to a deadline owner before any further yielding observation.
        (unless (gethash key niwa-memory-decision--recovery-workers)
          (puthash key job niwa-memory-decision--recovery-workers))
        (condition-case err
            (progn
              (plist-put job :timer
                         (run-at-time
                          (max 0 (- (plist-get claim :expires-at)
                                    (niwa-session-control-fs-target-time key))) nil
                          #'niwa-memory-decision--recovery-deadline workspace job))
              (let ((supported (niwa-journal-worker-local-p workspace)))
		(if (and (eq job (gethash key niwa-memory-decision--recovery-workers))
			 (funcall currentp)
			 (eq job (gethash key niwa-memory-decision--recovery-workers)))
                    (progn
                      (if supported
                          (niwa-journal-worker-start
			   workspace 'memory-recover
			   (list :claim claim :operation-id (plist-get claim :owner))
			   (lambda (result) (niwa-memory-decision--recovered workspace job result)) :owner job)
			(niwa-memory-decision--recovered
                         workspace job (list :ok t :operation-id (plist-get claim :owner) :synchronous t)))
                      ;; Request-file IO may cancel before the child exists.
                      ;; The generic launcher still hands that child to JOB;
                      ;; retire it here under the owner's already-proved fence.
                      (unless (and (funcall currentp)
                                   (eq job (gethash key niwa-memory-decision--recovery-workers)))
			(unwind-protect (niwa-memory-decision--recovery-release job)
                          (when (eq job (gethash key niwa-memory-decision--recovery-workers))
                            (remhash key niwa-memory-decision--recovery-workers)))))
                  (unwind-protect (niwa-memory-decision--recovery-release job)
                    (when (eq job (gethash key niwa-memory-decision--recovery-workers))
                      (remhash key niwa-memory-decision--recovery-workers))))))
          ((error quit)
           (if (eq job (gethash key niwa-memory-decision--recovery-workers))
               (niwa-memory-decision--recovered workspace job (list :error (error-message-string err)))
             (niwa-memory-decision--recovery-release job))))
        job))))

(defun niwa-memory-decision-admitted-p (key)
  "Return local admitted decision activity for canonical claim directory KEY.
Queued recovery is excluded so it can yield to a pass or drain.  This is a
non-IO scheduling observation, never proof of native claim ownership."
  (or (plist-get (gethash key niwa-memory-decision--recovery-dispatching) :admitted)
      (gethash key niwa-memory-decision--recovery-workers)
      (gethash key niwa-memory-decision--operations)))

(defun niwa-memory-decision--recovery-blocked-p (key)
  "Return local pass, drain or decision registrations blocking recovery at KEY.
This observation performs no IO; callers observe active cleanup beforehand."
  (or (and (featurep 'niwa-memory-pass)
           (or (gethash key niwa-memory-pass--running)
               (gethash key niwa-memory-pass--drains)))
      (gethash key niwa-memory-decision--operations)))

(defun niwa-memory-decision-recovering-p (workspace)
  "Return non-nil while WORKSPACE has queued, dispatching or active recovery.
This is an observation for presentation, never ownership or write admission."
  (let ((key (niwa-memory-store--claim-directory workspace)))
    (and (or (gethash key niwa-memory-decision--recovery-pending)
             (gethash key niwa-memory-decision--recovery-dispatching)
             (gethash key niwa-memory-decision--recovery-workers)) t)))

(defun niwa-memory-decision-recovery-generation (workspace)
  "Return how many recoveries changed WORKSPACE's committed memory state.
Displays compare it across a recovery to learn whether their reads are stale.
This is an observation for presentation, never proof of current state."
  (gethash (niwa-memory-store--claim-directory workspace) niwa-memory-decision--recovery-generations 0))

(defun niwa-memory-decision--recovery-changed (workspace)
  "Record that recovery changed WORKSPACE's committed state; drop its observations."
  (cl-incf (gethash (niwa-memory-store--claim-directory workspace)
                    niwa-memory-decision--recovery-generations 0))
  (setf (niwa-workspace-journal-observation workspace) nil
        (niwa-workspace-memory-observation workspace) nil))

;;;###autoload
(defun niwa-memory-decision-schedule-recovery (workspace &optional callback)
  "Recover WORKSPACE after returning to the editor, then call CALLBACK.
Local publication scans run in a child; marked write checks use live buffers.
CALLBACK runs only after successful recovery (or when no state exists), allowing
review admission to follow recovery instead of racing its workspace claim."
  (unless niwa-memory-decision--inhibit-recovery
    (let* ((root (copy-sequence (niwa-workspace-root workspace)))
           (key (niwa-memory-store--claim-directory workspace))
           (origin (current-buffer)))
      (unless (or (gethash key niwa-memory-decision--recovery-workers)
                  (gethash key niwa-memory-decision--recovery-dispatching))
        (let ((token (list :origin origin)) timer admitted)
          (cl-labels
           ((current ()
              ;; Publication recovery is mode-independent. Preserve its
              ;; original workspace, not an automatic-application actor.
              (and (not niwa-memory-decision--inhibit-recovery)
                   (not niwa-journal-worker--stopping)
                   (not (plist-get token :cancelled))
                   (equal root (niwa-workspace-root workspace))
                   (equal key (niwa-memory-store--claim-directory workspace))
                   (not niwa-memory-decision--inhibit-recovery)
                   (not niwa-journal-worker--stopping)
                   (not (plist-get token :cancelled))
                   (equal root (niwa-workspace-root workspace))
                   (or admitted (eq token (gethash key niwa-memory-decision--recovery-dispatching)))))
            (dispatch ()
              (unless (or niwa-memory-decision--inhibit-recovery (plist-get token :cancelled))
                (puthash key token niwa-memory-decision--recovery-dispatching)
                (let (deferred)
                  (unwind-protect
                      (condition-case err
                          (let ((present (niwa-session-control-fs-path-exists-p key))
                                (cleanup (and (featurep 'niwa-journal-cleanup)
                                              (niwa-journal-cleanup-active-p workspace))))
                            (when (current)
                              ;; All yielding observations precede these exact
                              ;; registrations and the admission reservation.
                              (cond
                               ((or cleanup (niwa-memory-decision--recovery-blocked-p key)) (setq deferred t))
                               ((not present)
                                (when (and callback (buffer-live-p origin))
                                  (with-current-buffer origin (funcall callback))))
                               (t
                                (plist-put token :admitted t)
                                (setq admitted (niwa-memory-decision--start-recovery workspace callback origin #'current key))))))
                        (error (niwa-memory-decision--recovery-failed root key (error-message-string err))))
                    (when (eq token (gethash key niwa-memory-decision--recovery-dispatching))
                      (remhash key niwa-memory-decision--recovery-dispatching)
                      ;; Re-arm the SAME opportunity and captured callback.
                      ;; Pending recovery must not block the drain it yields to.
                      (when (and deferred (not (plist-get token :cancelled))
                                 (not (gethash key niwa-memory-decision--recovery-pending)))
                        (puthash key timer niwa-memory-decision--recovery-pending)
                        (timer-set-time timer (time-add nil 1))
                        (timer-activate timer))))))))
           (setq timer (niwa-transport-schedule-idle
                        niwa-memory-decision--recovery-pending key 'memory-recovery key #'dispatch))))))))

(defun niwa-memory-decision-stop-recovery ()
  "Cancel pending recovery and live checks, fence claims, and stop children."
  (niwa-transport-cancel-idle niwa-memory-decision--recovery-pending 'memory-recovery)
  (maphash (lambda (key token)
             (plist-put token :cancelled t)
             (when (eq token (gethash key niwa-memory-decision--recovery-dispatching))
               (remhash key niwa-memory-decision--recovery-dispatching)))
           niwa-memory-decision--recovery-dispatching)
  (let (jobs)
    (maphash (lambda (_key job) (push job jobs)) niwa-memory-decision--recovery-workers)
    (clrhash niwa-memory-decision--recovery-workers)
    (dolist (job jobs)
      (when-let* ((operation (plist-get job :operation)))
        (niwa-memory-decision--operation-stop operation))
      (condition-case err
          (niwa-memory-decision--recovery-release job)
        ((error quit) (niwa-diagnostic-warning (plist-get job :diagnostic-scope) 'niwa
                                (format "Memory recovery release remains unresolved: %s" (error-message-string err))
                                :warning))))))

;;
;;; Read-only inspection

(defun niwa-memory-decision--inspect-proposal (workspace pass proposal)
  "Return a detached inspection of PASS's PROPOSAL in WORKSPACE.
See `niwa-memory-decision-inspect'."
  (let* ((accepted (niwa-memory-store-accepted workspace pass t))
         (item (or (seq-find (lambda (item) (equal proposal (plist-get item :id))) (plist-get accepted :proposals))
                   (error "Unknown accepted proposal")))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (_ (niwa-memory-scope--root scope (plist-get item :root)))
         (passes (let ((table (make-hash-table :test #'equal))) (puthash pass accepted table) table))
         (decision (niwa-memory-decision-status workspace proposal nil passes))
         (record (and decision (niwa-memory-decision--published workspace decision passes)))
         (input (unless (niwa-memory-decision-terminal-status-p (plist-get decision :status))
                  (niwa-memory-decision--application-input accepted item workspace nil t)))
         (fresh (eq (plist-get input :status) 'fresh))
         (effective (if fresh (plist-get input :proposal) item))
         (before (or (plist-get record :before) (plist-get effective :before)))
         (after (or (plist-get record :after)
                    (niwa-memory-write--after
                     before (niwa-memory-apply-changes (if fresh (plist-get input :scope) scope) effective))))
         (hold (niwa-memory-decision--reversal-hold record)))
    (niwa-memory-decision--snapshot
     (list :status (or (plist-get decision :status) 'pending)
           :decision (and decision (cl-loop for field in niwa-journal-store--decision-fields
                                            append (list (cdr field) (plist-get decision (cdr field)))))
           :check (and input (list :status (plist-get input :status) :reason (plist-get input :reason)))
           :before before :after after :reversal-hold hold))))

(defun niwa-memory-decision--inspect-workspace (workspace skip local-only)
  "Return detached proposal and write rows for WORKSPACE, omitting passes in SKIP.
LOCAL-ONLY returns (:native t) before accessing any authenticated remote target.
See `niwa-memory-decision-inspect'."
  (catch 'niwa-memory-decision--native
    (let* ((root (niwa-workspace-root workspace))
           (state (niwa-journal-store-state-directory (niwa-journal-store-directory root)))
           ;; A display observation, not a mutation proof. A concurrent expiry can
           ;; remove later evidence: those reads fail unavailable, never applied.
           (niwa-journal-cleanup--retired-observation
            (cons root (mapcar #'file-name-nondirectory
                              (niwa-session-control-fs-list-directory
                               (file-name-concat state "retired-passes") niwa-journal-store-id-regexp))))
           (entries (seq-remove (lambda (entry) (member (plist-get entry :pass-id) skip))
                                (niwa-journal-store-entries root)))
           (records (make-hash-table :test #'equal)) (latest (make-hash-table :test #'equal))
           (passes (make-hash-table :test #'equal)) seen rows)
      (let ((paths (niwa-session-control-fs-list-directory
                    (niwa-memory-decision--directory workspace) niwa-memory-decision--file-regexp)))
        (dolist (path paths)
          (let* ((proposal (car (split-string (file-name-base path) "\\."))) (previous (gethash proposal latest)))
            (when (or (not previous) (niwa-memory-decision--newer-slot-p path previous))
              (puthash proposal path latest))))
        (while paths
          (let ((batch (seq-take paths 8)))
            (setq paths (nthcdr (length batch) paths))
            (condition-case nil
                (dolist (row (niwa-memory-decision--record-batch workspace batch))
                  (puthash (car row) (cdr row) records))
              (error
               ;; Corruption in one record must not hide unrelated passes.
               (dolist (path batch)
                 (puthash path (condition-case failure (niwa-memory-decision--record workspace path)
                                 (error (error-message-string failure))) records)))))))
      ;; Accepted private state remains inspectable if public publication failed.
      (let* ((directories (seq-remove (lambda (path) (member (file-name-nondirectory path) skip))
                                      (niwa-session-control-fs-list-directory (file-name-concat state "passes") niwa-journal-store-id-regexp)))
             (exists (niwa-session-control-fs-paths-exist
                      (mapcar (lambda (directory) (file-name-concat directory "accepted.el")) directories))))
        (cl-mapc (lambda (directory present)
                   (when present
                     (push (list :kind 'consolidation :pass-id (file-name-nondirectory (directory-file-name directory))) entries)))
                 directories exists))
      (dolist (entry entries)
        (when (and (eq (plist-get entry :kind) 'consolidation)
                   (not (niwa-journal-cleanup-pass-retired-p root (plist-get entry :pass-id)))
                   (not (member (plist-get entry :pass-id) seen)))
          (let ((pass (plist-get entry :pass-id)))
            (push pass seen)
            (clrhash passes)
            (condition-case err
                (let* ((accepted (niwa-memory-store-accepted workspace pass t))
                       (_local (when (and local-only
                                          (seq-some (lambda (row) (file-remote-p (plist-get (cdr row) :dir)))
                                                    (plist-get (plist-get (plist-get accepted :prepared) :scope) :roots)))
                                 (throw 'niwa-memory-decision--native '(:native t))))
                       (pass-entries (seq-filter (lambda (item) (equal pass (plist-get item :pass-id))) entries)))
                  (unless (niwa-memory-decision--slot-eligible-p accepted)
                    (error "Decided or proposed before the decision-format upgrade; the next memory review will propose it again if still relevant"))
                  (puthash pass accepted passes)
                  (when (and accepted (null (plist-get accepted :proposals)))
                    (push (list :id pass :kind 'review :pass pass :status 'completed
                                :created (plist-get (plist-get accepted :review) :created)) rows))
                  (dolist (proposal (plist-get accepted :proposals))
                    (let* ((id (plist-get proposal :id))
                           (scope (plist-get (plist-get accepted :prepared) :scope))
                           (target (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
                           (path (gethash id latest)) (data (and path (gethash path records)))
                           problem
                           (record (condition-case failure
                                       (cond ((stringp data) (error "%s" data))
                                             (data (niwa-memory-decision--read workspace path passes nil data))
                                             ((seq-some (lambda (e) (equal id (plist-get e :proposal-id))) pass-entries)
                                              (error "Public decision has no private slot")))
                                     (error (setq problem (error-message-string failure)) nil)))
                           (decision (plist-get record :metadata)) (hold (niwa-memory-decision--reversal-hold record))
                           (status (or (plist-get decision :status) 'pending)))
                      (condition-case unavailable (niwa-memory-scope--root scope (plist-get proposal :root))
                        (error (setq problem (error-message-string unavailable))))
                      (when problem (setq status 'unavailable))
                      (push (list :id id :pass pass :proposal
                                  (list :action (plist-get proposal :action) :type (plist-get proposal :type)
                                        :title (plist-get proposal :title))
                                  :created (plist-get (plist-get accepted :review) :created) :status status :error problem :reversal-hold hold
                                  ;; A recorded stale decision may since have become applicable, or a pending
                                  ;; proposal superseded; label rows by what accepting would do now.
                                  :check (when (and (not problem) (niwa-memory-decision-actionable-status-p status))
                                           (plist-get (niwa-memory-decision--application-input
                                                       accepted proposal workspace nil t)
                                                      :status))
                                  :target (format "%s%s" (file-name-concat (plist-get target :dir) (plist-get proposal :file))
                                                  (if (plist-get target :client) (format " [%s]" (plist-get target :client)) "")))
                            rows)))
                  ;; Retirement publishes its marker before removing evidence.
                  ;; Reject this pass's rows if expiry raced our held proofs.
                  (dolist (result (niwa-session-control-fs-run-program
                                   (list (list :op 'absent :optional t :path (file-name-concat state "retired-passes" pass))
                                         (list :op 'verify-hash :path (file-name-concat (niwa-memory-store--directory workspace pass) "accepted.el")
                                               :content (plist-get accepted :hash)))))
                    (unless (and (eq (plist-get result :op) 'absent) (eq (plist-get result :status) 'absent))
                      (niwa-session-control-fs-program-value result))))
              (error
               (setq rows (seq-remove (lambda (row) (equal pass (plist-get row :pass))) rows))
               (push (list :id pass :pass pass :status 'unavailable :error (error-message-string err)) rows))))))
      (niwa-memory-decision--snapshot (list :rows (nreverse rows))))))

(cl-defun niwa-memory-decision-inspect (workspace &key pass proposal skip local-only)
  "Describe WORKSPACE's retained proposal rows, omitting passes in SKIP.
With PASS and PROPOSAL, return its decision, exact file states and check.
LOCAL-ONLY returns (:native t) before accessing an original remote target.
The detached display snapshot grants no authority."
  (if (and pass proposal)
      (niwa-memory-decision--inspect-proposal workspace pass proposal)
    (niwa-memory-decision--inspect-workspace workspace skip local-only)))

(defun niwa-memory-decision--reversal-hold (record)
  "Return why applied RECORD might need reversal retry at its original root.
An exact external undo is indistinguishable from an interrupted reversal."
  (when (and (eq 'applied (plist-get (plist-get record :metadata) :status))
             (not (niwa-memory-proposal-note-action-p (plist-get (plist-get record :item) :action))))
    (condition-case err
        (let (restored foreign)
          (niwa-memory-scope--root (plist-get (plist-get (plist-get record :accepted) :prepared) :scope)
                                      (plist-get (plist-get record :item) :root))
          (dolist (row (plist-get record :after))
            (let ((after (cdr row)) (before (cdr (assoc (car row) (plist-get record :before)))))
              (unless (niwa-change-same-p before after)
                (let* ((path (plist-get after :path))
                       (current (niwa-memory-scope--snapshot path niwa-memory-scope--max-file-bytes)))
                  (when-let* ((buffer (find-buffer-visiting path)) ((buffer-modified-p buffer)))
                    (error "Unsaved changes in %s" path))
                  (cond ((niwa-change-same-p current before) (setq restored t))
                        ((not (niwa-change-same-p current after)) (setq foreign t)))))))
          (when (and restored (not foreign)) "A reversal may be incomplete. Choose Reverse to finish it."))
      (error (format "Cannot check reversal at its original target: %s" (error-message-string err))))))

(defun niwa-memory-decision-retention (workspace accepted entries)
  "Return terminal ACCEPTED pass history in WORKSPACE from public ENTRIES.
Require every private slot to be published and every proposal settled."
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (resolved-at (float-time (date-to-time (plist-get (plist-get accepted :review) :created))))
         (passes (make-hash-table :test #'equal))
         (published (make-hash-table :test #'equal)) private related)
    (unless (niwa-memory-decision--slot-eligible-p accepted)
      (error "Pre-upgrade memory history is not collected"))
    (puthash pass accepted passes)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision) (equal pass (plist-get entry :pass-id)))
        (let* ((id (plist-get entry :decision-id))
               (record (niwa-memory-decision--published workspace entry passes)))
          (when (gethash id published) (error "Duplicate public memory decision"))
          (puthash id record published)
          (setq resolved-at (max resolved-at (float-time (date-to-time (plist-get entry :created)))))
          (push (list :id id :file (plist-get entry :file) :sha256 (secure-hash 'sha256 (plist-get entry :text))) related)
          (push (list :file (format "decision-slots/%s.el" id) :sha256 (plist-get record :hash)) private))))
    (dolist (proposal (plist-get accepted :proposals))
      (let* ((slots (niwa-memory-decision--slots workspace (plist-get proposal :id)))
             (latest (and slots (gethash (file-name-base (car slots)) published))))
        (unless (niwa-memory-decision-terminal-status-p (plist-get (plist-get latest :metadata) :status))
          (error "Memory proposal is unresolved"))
        (when-let* ((hold (niwa-memory-decision--reversal-hold latest))) (error "%s" hold))
        (dolist (path slots)
          (unless (gethash (file-name-base path) published) (error "Memory decision publication is unresolved")))))
    (list :private (nreverse private) :related (nreverse related) :resolved-at resolved-at)))

;;
;;; Maintenance child

(defun niwa-memory-decision-recover-child (workspace payload)
  "Recover WORKSPACE under PAYLOAD's parent claim and select identities.
Never return reusable acceptance proofs."
  (niwa-memory-store--assert-owned (plist-get payload :claim))
  (let* ((predecessor (plist-get payload :recovery-predecessor))
         (recovered
          (and predecessor
               (or (niwa-memory-decision--recovery-successor-p
                    (list :claim (plist-get payload :claim)) predecessor)
                   (error "Memory recovery predecessor changed")))))
    (when (and recovered (not (or (plist-get payload :pending-selection)
                                 (plist-get payload :application))))
      (error "Recovery continuity requires decision discovery"))
    (if (and (not recovered) (not (niwa-memory-decision--worker-local-p workspace)))
        (list :ok t :operation-id (plist-get payload :operation-id) :synchronous t)
      (let ((changed (unless recovered (niwa-memory-decision-recover workspace))))
        (unless (or recovered (plist-get payload :decisions-only))
          (when (niwa-memory-store-recover workspace) (setq changed t)))
        (cond
         ((plist-get payload :application)
          (unless (niwa-journal-store-id-p (plist-get payload :proposal-id))
            (error "Invalid application proposal identity"))
          (let* ((accepted (niwa-memory-store-accepted workspace (plist-get payload :pass-id) t))
                 (proposal (seq-find (lambda (item) (equal (plist-get item :id) (plist-get payload :proposal-id)))
                                     (plist-get accepted :proposals)))
                 (passes (make-hash-table :test #'equal)))
            (unless proposal (error "Unknown accepted application proposal"))
            (puthash (plist-get payload :pass-id) accepted passes)
            (cl-labels ((selected (id)
                          (when-let* ((entry (niwa-memory-decision-status workspace id nil passes)))
                            (cl-loop for field in niwa-journal-store--decision-fields
                                     append (list (cdr field) (plist-get entry (cdr field)))))))
              (list :ok t :operation-id (plist-get payload :operation-id)
                    :changed changed :previous (selected (plist-get proposal :id))
                    :requirements (mapcar (lambda (id) (cons id (selected id)))
                                          (plist-get proposal :requires))))))
         ((plist-get payload :pending-selection)
          (list :ok t :operation-id (plist-get payload :operation-id)
                :changed changed :items
                (niwa-memory-decision--pending-items workspace (plist-get payload :skip))))
         (t (list :ok t :operation-id (plist-get payload :operation-id) :changed changed)))))))

(defun niwa-memory-decision-publish-child (workspace payload)
  "Reauthenticate and publish WORKSPACE's accepted decision named by PAYLOAD."
  (let ((id (plist-get payload :decision-id)) (claim (plist-get payload :claim)) (hash (plist-get payload :hash)))
    (unless (and (niwa-journal-store-decision-id-p id) (niwa-journal-store-id-p hash)
                 (equal (plist-get claim :directory) (niwa-memory-store--claim-directory workspace))
                 (equal (plist-get claim :owner) (plist-get payload :operation-id))
                 (let ((outcome (niwa-journal-claim-outcome claim)))
                   (and (eq 'completed (plist-get outcome :status)) (equal hash (plist-get outcome :payload)))))
      (error "Invalid decision publication identity"))
    (niwa-memory-decision--publish workspace id hash)
    (list :ok t :operation-id (plist-get claim :owner)
          :publication (list :decision-id id :hash (plist-get payload :hash)))))

(provide 'niwa-memory-decision)
;;; niwa-memory-decision.el ends here
