;;; niwa-session-checkpoint.el --- Committed conversation checkpoints -*- lexical-binding: t -*-

;;; Commentary:

;; Live conversations can advance before their transcripts are durable.  This
;; owner projects the last committed root fields and retains consumed mailbox
;; records and deferred ToolCall rows until the receiving transcript commits.
;; Capture at transcript freeze, project the final sidecar with that candidate,
;; and install only after the publisher adopts its actual committed head.
;; Candidates are process-local artifact metadata, never sidecar data.

;;; Code:

(require 'cl-lib)
(require 'niwa-structs)

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-sanitize-mailbox
                  "niwa-agent-persistence" (raw recipient))
(autoload 'niwa-agent-persistence-sanitize-mailbox
  "niwa-agent-persistence")

(defconst niwa-session-checkpoint--root-keys
  '(:current-segment :total-turn-count :prompt-index :file-snapshots
    :untracked-effects :chapters :first-user-message :latest-user-message)
  "Root fields that advance only with a matching transcript commit.")

(cl-defstruct (niwa-session-checkpoint--state
               (:constructor niwa-session-checkpoint--state-create))
  epoch root consumed deferred retired-mail retired-ptc)

(cl-defstruct (niwa-session-checkpoint--candidate
               (:constructor niwa-session-checkpoint--candidate-create))
  epoch conversation consumed-ids retire-ids root)

(defun niwa-session-checkpoint--copy (value)
  "Copy checkpoint data VALUE, including mutable strings and vectors."
  (cond
   ((consp value)
    ;; Iterate along history lists: one frame per turn would exceed the Lisp
    ;; depth limit in long sessions even though the data's nesting is shallow.
    (let (result)
      (while (consp value)
        (push (niwa-session-checkpoint--copy (car value)) result)
        (setq value (cdr value)))
      (nconc (nreverse result) (niwa-session-checkpoint--copy value))))
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (apply #'vector (mapcar #'niwa-session-checkpoint--copy value)))
   (t value)))

(defun niwa-session-checkpoint--root (sidecar)
  "Return a detached root projection from SIDECAR."
  (cl-loop for key in niwa-session-checkpoint--root-keys
           append (list key
                        (niwa-session-checkpoint--copy
                         (or (plist-get sidecar key)
                             (pcase key
                               (:current-segment 1)
                               (:total-turn-count 0)))))))

(defun niwa-session-checkpoint-seed (session sidecar)
  "Seed SESSION from validated committed SIDECAR after decode or adoption.
Discard old pending consumption and invalidate all prior candidates.  Do not
call this for ordinary metadata-only publication or on a save's return."
  (setf (niwa-session-checkpoint-state session)
        (niwa-session-checkpoint--state-create
         :epoch (make-symbol "checkpoint")
         :root (niwa-session-checkpoint--root sidecar)))
  session)

(defun niwa-session-checkpoint--state (session)
  "Return SESSION's owner state, initializing a fresh empty checkpoint."
  (unless (niwa-session-checkpoint-state session)
    ;; Never initialize from live fields: even a first save can happen after
    ;; the live turn counter, prompt index, and mailbox have already advanced.
    (niwa-session-checkpoint-seed session nil))
  (niwa-session-checkpoint-state session))

(defun niwa-session-checkpoint--conversation (conversation)
  "Check that CONVERSATION is a canonical retained conversation path."
  (unless (niwa-agent-path-p conversation)
    (error "Invalid checkpoint conversation: %S" conversation)))

(defun niwa-session-checkpoint-first-user-message (session)
  "Return SESSION's committed first user preview as a detached string."
  (let ((preview
         (plist-get
          (niwa-session-checkpoint--state-root
           (niwa-session-checkpoint--state session))
          :first-user-message)))
    (and preview (copy-sequence preview))))

(defun niwa-session-checkpoint--ids (records)
  "Return durable identities of RECORDS, rejecting missing identities."
  (mapcar
   (lambda (record)
     (let ((id (plist-get record :id)))
       (unless (and (stringp id) (> (length id) 0))
         (error "Checkpoint record has no durable identity"))
       id))
   records))

(defun niwa-session-checkpoint--merge (live pending &optional excluded)
  "Merge LIVE and PENDING records by identity, omitting EXCLUDED IDs.
Both lists are newest first.  LIVE takes precedence over a retained copy."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (id excluded) (puthash id t seen))
    (cl-loop for record in (append live pending)
             for id = (plist-get record :id)
             unless (gethash id seen)
             collect (progn (puthash id t seen) record))))

(defun niwa-session-checkpoint-consume (session conversation records)
  "Retain consumed mailbox RECORDS for SESSION's CONVERSATION.
Call before removing the records from the live mailbox.  RECORDS use the
mailbox's newest-first order and carry durable `:id' values.  No I/O or hooks
run here; ordinary sidecars keep these records until their transcript commits."
  (niwa-session-checkpoint--conversation conversation)
  (niwa-session-checkpoint--ids records)
  (dolist (record records)
    (unless (equal (plist-get record :recipient) conversation)
      (error "Consumed record belongs to another conversation")))
  (let* ((state (niwa-session-checkpoint--state session))
         (ledger (niwa-session-checkpoint--state-consumed state)))
    (setf (alist-get conversation ledger nil nil #'equal)
          (niwa-session-checkpoint--copy
           (niwa-session-checkpoint--merge
            records (alist-get conversation ledger nil nil #'equal)))
          (niwa-session-checkpoint--state-consumed state) ledger)))

(defun niwa-session-checkpoint-pending-message-p (session conversation id)
  "Return non-nil when message ID was consumed by CONVERSATION in SESSION.
The identity is still held by the consumption ledger, or by the retirement
proofs kept while publication work could resurrect it, so the receiving
transcript has not yet made it durable.  A retried delivery of the same
stable ID uses this to avoid queueing a duplicate.  No I/O runs and no
owner state is created or changed."
  (when-let* ((state (niwa-session-checkpoint-state session))
              ((stringp id)))
    (and (or (cl-some
              (lambda (record) (equal id (plist-get record :id)))
              (alist-get conversation
                         (niwa-session-checkpoint--state-consumed state)
                         nil nil #'equal))
             (member id
                     (alist-get
                      conversation
                      (niwa-session-checkpoint--state-retired-mail state)
                      nil nil #'equal)))
         t)))

(defun niwa-session-checkpoint-defer-ptc (session conversation records)
  "Retain removed ToolCall RECORDS until CONVERSATION's transcript commits.
SESSION owns the durable rows.  Starting or updating a ToolCall does not use
this ledger; only removal in favor of a transcript row requires deferral."
  (niwa-session-checkpoint--conversation conversation)
  (niwa-session-checkpoint--ids records)
  (let* ((state (niwa-session-checkpoint--state session))
         (ledger (niwa-session-checkpoint--state-deferred state)))
    (setf (alist-get conversation ledger nil nil #'equal)
          (niwa-session-checkpoint--copy
           (niwa-session-checkpoint--merge
            records (alist-get conversation ledger nil nil #'equal)))
          (niwa-session-checkpoint--state-deferred state) ledger)))

(defun niwa-session-checkpoint-capture
    (session conversation &optional root-projection)
  "Freeze SESSION's pending identities for CONVERSATION.
ROOT-PROJECTION, when supplied, is a raw sidecar whose root fields
describe the frozen root transcript.  Otherwise attach them later using
`niwa-session-checkpoint-with-root'.  Capture neither commits state nor
retires records.  Store the opaque result on the final artifact's `:checkpoint'."
  (niwa-session-checkpoint--conversation conversation)
  (let* ((state (niwa-session-checkpoint--state session))
         (candidate
          (niwa-session-checkpoint--candidate-create
           :epoch (niwa-session-checkpoint--state-epoch state)
           :conversation (copy-sequence conversation)
           :consumed-ids
           (niwa-session-checkpoint--copy
            (niwa-session-checkpoint--ids
             (alist-get conversation
                        (niwa-session-checkpoint--state-consumed state)
                        nil nil #'equal)))
           :retire-ids
           (niwa-session-checkpoint--copy
            (niwa-session-checkpoint--ids
             (alist-get conversation
                        (niwa-session-checkpoint--state-deferred state)
                        nil nil #'equal))))))
    (if root-projection
        (niwa-session-checkpoint-with-root candidate root-projection)
      candidate)))

(defun niwa-session-checkpoint-root-p (candidate)
  "Return non-nil when CANDIDATE checkpoints the root conversation."
  (and (niwa-session-checkpoint--candidate-p candidate)
       (equal "/root"
              (niwa-session-checkpoint--candidate-conversation candidate))))

(defun niwa-session-checkpoint-with-root (candidate sidecar)
  "Return CANDIDATE with frozen root fields from raw SIDECAR attached.
Keep the original freeze-time consumption and retirement identities.  Only
a root conversation candidate may carry a root projection."
  (unless (niwa-session-checkpoint-root-p candidate)
    (error "Only a root checkpoint can advance root fields"))
  (let ((result (copy-niwa-session-checkpoint--candidate candidate)))
    (setf (niwa-session-checkpoint--candidate-root result)
          (niwa-session-checkpoint--root sidecar))
    result))

(defun niwa-session-checkpoint--current-p (state candidate)
  "Return non-nil when CANDIDATE belongs to STATE's current epoch."
  (and (niwa-session-checkpoint--candidate-p candidate)
       (eq (niwa-session-checkpoint--state-epoch state)
           (niwa-session-checkpoint--candidate-epoch candidate))))

(defun niwa-session-checkpoint-valid-p (session candidate)
  "Return non-nil when CANDIDATE can still commit for SESSION.
Publishers check this before committing the head, not just at installation."
  (niwa-session-checkpoint--current-p
   (niwa-session-checkpoint--state session) candidate))

(defun niwa-session-checkpoint-project (session sidecar &optional candidate)
  "Project raw SIDECAR through SESSION's committed checkpoint owner.
Without CANDIDATE use committed root fields and restore all pending consumed
mail and deferred ToolCalls.  With CANDIDATE, advance only its frozen root
projection and exclude only its captured identities.  Return a new sidecar;
neither input nor owner state changes."
  (let* ((state (niwa-session-checkpoint--state session))
         (result (copy-sequence sidecar))
         (consumed (niwa-session-checkpoint--state-consumed state))
         (deferred (niwa-session-checkpoint--state-deferred state)))
    (when (and candidate
               (not (niwa-session-checkpoint--current-p state candidate)))
      (error "Checkpoint candidate belongs to a different owner epoch"))
    (let ((root (or (and candidate
                         (niwa-session-checkpoint--candidate-root candidate))
                    (niwa-session-checkpoint--state-root state)))
          (conversation
           (and candidate
                (niwa-session-checkpoint--candidate-conversation candidate)))
          (excluded
           (and candidate
                (niwa-session-checkpoint--candidate-consumed-ids candidate))))
      (cl-loop for (key value) on root by #'cddr do
               (setq result
                     (plist-put result key
                                (niwa-session-checkpoint--copy value))))
      (cl-labels
          ((mailbox (path live)
             (niwa-session-checkpoint--copy
              (niwa-agent-persistence-sanitize-mailbox
               (niwa-session-checkpoint--merge
                live (alist-get path consumed nil nil #'equal)
                (append
                 (and (equal path conversation) excluded)
                 (alist-get path
                            (niwa-session-checkpoint--state-retired-mail state)
                            nil nil #'equal)))
               path))))
        (setq result
              (plist-put result :messages
                         (mailbox "/root" (plist-get sidecar :messages))))
        (setq result
              (plist-put
               result :agent-registry
               (mapcar
                (lambda (record)
                  (let ((copy (copy-sequence record)))
                    (plist-put copy :mailbox
                               (mailbox (plist-get record :path)
                                        (plist-get record :mailbox)))))
                (plist-get sidecar :agent-registry)))))
      (setq result
            (plist-put
             result :ptc-checkpoints
             (niwa-session-checkpoint--copy
              (niwa-session-checkpoint--merge
               (plist-get sidecar :ptc-checkpoints)
               (apply #'append (mapcar #'cdr deferred))
               (append
                (and candidate
                     (niwa-session-checkpoint--candidate-retire-ids candidate))
                (niwa-session-checkpoint--state-retired-ptc state))))))
      result)))

(defun niwa-session-checkpoint-install (session candidate)
  "Install CANDIDATE after SESSION's matching artifact actually commits.
Retire only captured consumption and ToolCall identities, keeping any records
added after capture.  Repeated installation is harmless.  Return non-nil on
installation, or nil for a candidate invalidated by restore or adoption."
  (let ((state (niwa-session-checkpoint--state session)))
    (when (niwa-session-checkpoint--current-p state candidate)
      (when-let* ((root
                   (niwa-session-checkpoint--candidate-root candidate)))
        (setf (niwa-session-checkpoint--state-root state)
              (niwa-session-checkpoint--copy root)))
      (let* ((conversation
              (niwa-session-checkpoint--candidate-conversation candidate))
             (consumed (niwa-session-checkpoint--state-consumed state))
             (deferred (niwa-session-checkpoint--state-deferred state)))
        ;; Only outstanding publication work can carry stale raw sidecars.
        ;; Retain identity proofs through that drain/retry, not for the session
        ;; lifetime.  Otherwise rotation could archive the only receipt while
        ;; a queued sidecar brings the consumed row back into the successor.
        (when (or (niwa-session-publication-active-p session)
                  (niwa-session-publication-job session)
                  (niwa-session-publication-preparations session))
          (let ((retired (niwa-session-checkpoint--state-retired-mail state)))
            (setf (alist-get conversation retired nil nil #'equal)
                  (delete-dups
                   (append
                    (niwa-session-checkpoint--copy
                     (niwa-session-checkpoint--candidate-consumed-ids candidate))
                    (alist-get conversation retired nil nil #'equal)))
                  (niwa-session-checkpoint--state-retired-mail state) retired
                  (niwa-session-checkpoint--state-retired-ptc state)
                  (delete-dups
                   (append
                    (niwa-session-checkpoint--copy
                     (niwa-session-checkpoint--candidate-retire-ids candidate))
                    (niwa-session-checkpoint--state-retired-ptc state))))))
        (setf (alist-get conversation consumed nil nil #'equal)
              (niwa-session-checkpoint--merge
               (alist-get conversation consumed nil nil #'equal) nil
               (niwa-session-checkpoint--candidate-consumed-ids candidate))
              (alist-get conversation deferred nil nil #'equal)
              (niwa-session-checkpoint--merge
               (alist-get conversation deferred nil nil #'equal) nil
               (niwa-session-checkpoint--candidate-retire-ids candidate))
              (niwa-session-checkpoint--state-consumed state) consumed
              (niwa-session-checkpoint--state-deferred state) deferred))
      t)))

(defun niwa-session-checkpoint-release-proofs (session)
  "Release SESSION's stale-sidecar retirement proofs after a successful drain.
Return nil while any active, queued, staged, or recoverable publication remains.
These proofs outlive neither the outstanding publication work nor its retries."
  (unless (or (niwa-session-publication-active-p session)
              (niwa-session-publication-job session)
              (niwa-session-publication-preparations session)
              (niwa-session-publication-queue session)
              (niwa-session-publication-uncommitted-batches session)
              (niwa-session-pending-publication session))
    (when-let* ((state (niwa-session-checkpoint-state session)))
      (setf (niwa-session-checkpoint--state-retired-mail state) nil
            (niwa-session-checkpoint--state-retired-ptc state) nil))
    t))

(provide 'niwa-session-checkpoint)
;;; niwa-session-checkpoint.el ends here
