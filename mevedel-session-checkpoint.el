;;; mevedel-session-checkpoint.el --- Committed conversation checkpoints -*- lexical-binding: t -*-

;;; Commentary:

;; Live conversations can advance before their transcripts are durable.  This
;; owner projects the last committed root fields and retains consumed mailbox
;; records and deferred ToolCall rows until the receiving transcript commits.
;; Capture at transcript freeze, project the final sidecar with that candidate,
;; and install only after the publisher adopts its actual committed head.
;; Candidates are process-local artifact metadata, never sidecar data.

;;; Code:

(require 'cl-lib)
(require 'mevedel-structs)

;; `mevedel-agent-persistence'
(declare-function mevedel-agent-persistence-sanitize-mailbox
                  "mevedel-agent-persistence" (raw recipient))
(autoload 'mevedel-agent-persistence-sanitize-mailbox
  "mevedel-agent-persistence")

(defconst mevedel-session-checkpoint--root-keys
  '(:current-segment :total-turn-count :prompt-index :file-snapshots :chapters
    :first-user-message :latest-user-message)
  "Root fields that advance only with a matching transcript commit.")

(cl-defstruct (mevedel-session-checkpoint--state
               (:constructor mevedel-session-checkpoint--state-create))
  epoch root consumed deferred retired-mail retired-ptc)

(cl-defstruct (mevedel-session-checkpoint--candidate
               (:constructor mevedel-session-checkpoint--candidate-create))
  epoch conversation consumed-ids retire-ids root)

(defun mevedel-session-checkpoint--copy (value)
  "Copy checkpoint data VALUE, including mutable strings and vectors."
  (cond
   ((consp value)
    ;; Iterate along history lists: one frame per turn would exceed the Lisp
    ;; depth limit in long sessions even though the data's nesting is shallow.
    (let (result)
      (while (consp value)
        (push (mevedel-session-checkpoint--copy (car value)) result)
        (setq value (cdr value)))
      (nconc (nreverse result) (mevedel-session-checkpoint--copy value))))
   ((stringp value) (copy-sequence value))
   ((vectorp value)
    (apply #'vector (mapcar #'mevedel-session-checkpoint--copy value)))
   (t value)))

(defun mevedel-session-checkpoint--root (sidecar)
  "Return a detached root projection from SIDECAR."
  (cl-loop for key in mevedel-session-checkpoint--root-keys
           append (list key
                        (mevedel-session-checkpoint--copy
                         (or (plist-get sidecar key)
                             (pcase key
                               (:current-segment 1)
                               (:total-turn-count 0)))))))

(defun mevedel-session-checkpoint-seed (session sidecar)
  "Seed SESSION from validated committed SIDECAR after decode or adoption.
Discard old pending consumption and invalidate all prior candidates.  Do not
call this for ordinary metadata-only publication or on a save's return."
  (setf (mevedel-session-checkpoint-state session)
        (mevedel-session-checkpoint--state-create
         :epoch (make-symbol "checkpoint")
         :root (mevedel-session-checkpoint--root sidecar)))
  session)

(defun mevedel-session-checkpoint--state (session)
  "Return SESSION's owner state, initializing a fresh empty checkpoint."
  (unless (mevedel-session-checkpoint-state session)
    ;; Never initialize from live fields: even a first save can happen after
    ;; the live turn counter, prompt index, and mailbox have already advanced.
    (mevedel-session-checkpoint-seed session nil))
  (mevedel-session-checkpoint-state session))

(defun mevedel-session-checkpoint--conversation (conversation)
  "Check that CONVERSATION is a canonical retained conversation path."
  (unless (mevedel-agent-path-p conversation)
    (error "Invalid checkpoint conversation: %S" conversation)))

(defun mevedel-session-checkpoint-first-user-message (session)
  "Return SESSION's committed first user preview as a detached string."
  (let ((preview
         (plist-get
          (mevedel-session-checkpoint--state-root
           (mevedel-session-checkpoint--state session))
          :first-user-message)))
    (and preview (copy-sequence preview))))

(defun mevedel-session-checkpoint--ids (records)
  "Return durable identities of RECORDS, rejecting missing identities."
  (mapcar
   (lambda (record)
     (let ((id (plist-get record :id)))
       (unless (and (stringp id) (> (length id) 0))
         (error "Checkpoint record has no durable identity"))
       id))
   records))

(defun mevedel-session-checkpoint--merge (live pending &optional excluded)
  "Merge LIVE and PENDING records by identity, omitting EXCLUDED IDs.
Both lists are newest first.  LIVE takes precedence over a retained copy."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (id excluded) (puthash id t seen))
    (cl-loop for record in (append live pending)
             for id = (plist-get record :id)
             unless (gethash id seen)
             collect (progn (puthash id t seen) record))))

(defun mevedel-session-checkpoint-consume (session conversation records)
  "Retain consumed mailbox RECORDS for SESSION's CONVERSATION.
Call before removing the records from the live mailbox.  RECORDS use the
mailbox's newest-first order and carry durable `:id' values.  No I/O or hooks
run here; ordinary sidecars keep these records until their transcript commits."
  (mevedel-session-checkpoint--conversation conversation)
  (mevedel-session-checkpoint--ids records)
  (dolist (record records)
    (unless (equal (plist-get record :recipient) conversation)
      (error "Consumed record belongs to another conversation")))
  (let* ((state (mevedel-session-checkpoint--state session))
         (ledger (mevedel-session-checkpoint--state-consumed state)))
    (setf (alist-get conversation ledger nil nil #'equal)
          (mevedel-session-checkpoint--copy
           (mevedel-session-checkpoint--merge
            records (alist-get conversation ledger nil nil #'equal)))
          (mevedel-session-checkpoint--state-consumed state) ledger)))

(defun mevedel-session-checkpoint-defer-ptc (session conversation records)
  "Retain removed ToolCall RECORDS until CONVERSATION's transcript commits.
SESSION owns the durable rows.  Starting or updating a ToolCall does not use
this ledger; only removal in favor of a transcript row requires deferral."
  (mevedel-session-checkpoint--conversation conversation)
  (mevedel-session-checkpoint--ids records)
  (let* ((state (mevedel-session-checkpoint--state session))
         (ledger (mevedel-session-checkpoint--state-deferred state)))
    (setf (alist-get conversation ledger nil nil #'equal)
          (mevedel-session-checkpoint--copy
           (mevedel-session-checkpoint--merge
            records (alist-get conversation ledger nil nil #'equal)))
          (mevedel-session-checkpoint--state-deferred state) ledger)))

(defun mevedel-session-checkpoint-capture
    (session conversation &optional root-projection)
  "Freeze SESSION's pending identities for CONVERSATION.
ROOT-PROJECTION, when supplied, is a raw sidecar whose seven root fields
describe the frozen root transcript.  Otherwise attach them later using
`mevedel-session-checkpoint-with-root'.  Capture neither commits state nor
retires records.  Store the opaque result on the final artifact's `:checkpoint'."
  (mevedel-session-checkpoint--conversation conversation)
  (let* ((state (mevedel-session-checkpoint--state session))
         (candidate
          (mevedel-session-checkpoint--candidate-create
           :epoch (mevedel-session-checkpoint--state-epoch state)
           :conversation (copy-sequence conversation)
           :consumed-ids
           (mevedel-session-checkpoint--copy
            (mevedel-session-checkpoint--ids
             (alist-get conversation
                        (mevedel-session-checkpoint--state-consumed state)
                        nil nil #'equal)))
           :retire-ids
           (mevedel-session-checkpoint--copy
            (mevedel-session-checkpoint--ids
             (alist-get conversation
                        (mevedel-session-checkpoint--state-deferred state)
                        nil nil #'equal))))))
    (if root-projection
        (mevedel-session-checkpoint-with-root candidate root-projection)
      candidate)))

(defun mevedel-session-checkpoint-root-p (candidate)
  "Return non-nil when CANDIDATE checkpoints the root conversation."
  (and (mevedel-session-checkpoint--candidate-p candidate)
       (equal "/root"
              (mevedel-session-checkpoint--candidate-conversation candidate))))

(defun mevedel-session-checkpoint-with-root (candidate sidecar)
  "Return CANDIDATE with frozen root fields from raw SIDECAR attached.
Keep the original freeze-time consumption and retirement identities.  Only
a root conversation candidate may carry a root projection."
  (unless (mevedel-session-checkpoint-root-p candidate)
    (error "Only a root checkpoint can advance root fields"))
  (let ((result (copy-mevedel-session-checkpoint--candidate candidate)))
    (setf (mevedel-session-checkpoint--candidate-root result)
          (mevedel-session-checkpoint--root sidecar))
    result))

(defun mevedel-session-checkpoint--current-p (state candidate)
  "Return non-nil when CANDIDATE belongs to STATE's current epoch."
  (and (mevedel-session-checkpoint--candidate-p candidate)
       (eq (mevedel-session-checkpoint--state-epoch state)
           (mevedel-session-checkpoint--candidate-epoch candidate))))

(defun mevedel-session-checkpoint-valid-p (session candidate)
  "Return non-nil when CANDIDATE can still commit for SESSION.
Publishers check this before committing the head, not just at installation."
  (mevedel-session-checkpoint--current-p
   (mevedel-session-checkpoint--state session) candidate))

(defun mevedel-session-checkpoint-project (session sidecar &optional candidate)
  "Project raw SIDECAR through SESSION's committed checkpoint owner.
Without CANDIDATE use committed root fields and restore all pending consumed
mail and deferred ToolCalls.  With CANDIDATE, advance only its frozen root
projection and exclude only its captured identities.  Return a new sidecar;
neither input nor owner state changes."
  (let* ((state (mevedel-session-checkpoint--state session))
         (result (copy-sequence sidecar))
         (consumed (mevedel-session-checkpoint--state-consumed state))
         (deferred (mevedel-session-checkpoint--state-deferred state)))
    (when (and candidate
               (not (mevedel-session-checkpoint--current-p state candidate)))
      (error "Checkpoint candidate belongs to a different owner epoch"))
    (let ((root (or (and candidate
                         (mevedel-session-checkpoint--candidate-root candidate))
                    (mevedel-session-checkpoint--state-root state)))
          (conversation
           (and candidate
                (mevedel-session-checkpoint--candidate-conversation candidate)))
          (excluded
           (and candidate
                (mevedel-session-checkpoint--candidate-consumed-ids candidate))))
      (cl-loop for (key value) on root by #'cddr do
               (setq result
                     (plist-put result key
                                (mevedel-session-checkpoint--copy value))))
      (cl-labels
          ((mailbox (path live)
             (mevedel-session-checkpoint--copy
              (mevedel-agent-persistence-sanitize-mailbox
               (mevedel-session-checkpoint--merge
                live (alist-get path consumed nil nil #'equal)
                (append
                 (and (equal path conversation) excluded)
                 (alist-get path
                            (mevedel-session-checkpoint--state-retired-mail state)
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
             (mevedel-session-checkpoint--copy
              (mevedel-session-checkpoint--merge
               (plist-get sidecar :ptc-checkpoints)
               (apply #'append (mapcar #'cdr deferred))
               (append
                (and candidate
                     (mevedel-session-checkpoint--candidate-retire-ids candidate))
                (mevedel-session-checkpoint--state-retired-ptc state))))))
      result)))

(defun mevedel-session-checkpoint-install (session candidate)
  "Install CANDIDATE after SESSION's matching artifact actually commits.
Retire only captured consumption and ToolCall identities, keeping any records
added after capture.  Repeated installation is harmless.  Return non-nil on
installation, or nil for a candidate invalidated by restore or adoption."
  (let ((state (mevedel-session-checkpoint--state session)))
    (when (mevedel-session-checkpoint--current-p state candidate)
      (when-let* ((root
                   (mevedel-session-checkpoint--candidate-root candidate)))
        (setf (mevedel-session-checkpoint--state-root state)
              (mevedel-session-checkpoint--copy root)))
      (let* ((conversation
              (mevedel-session-checkpoint--candidate-conversation candidate))
             (consumed (mevedel-session-checkpoint--state-consumed state))
             (deferred (mevedel-session-checkpoint--state-deferred state)))
        ;; Only outstanding publication work can carry stale raw sidecars.
        ;; Retain identity proofs through that drain/retry, not for the session
        ;; lifetime.  Otherwise rotation could archive the only receipt while
        ;; a queued sidecar brings the consumed row back into the successor.
        (when (mevedel-session-publication-active-p session)
          (let ((retired (mevedel-session-checkpoint--state-retired-mail state)))
            (setf (alist-get conversation retired nil nil #'equal)
                  (delete-dups
                   (append
                    (mevedel-session-checkpoint--copy
                     (mevedel-session-checkpoint--candidate-consumed-ids candidate))
                    (alist-get conversation retired nil nil #'equal)))
                  (mevedel-session-checkpoint--state-retired-mail state) retired
                  (mevedel-session-checkpoint--state-retired-ptc state)
                  (delete-dups
                   (append
                    (mevedel-session-checkpoint--copy
                     (mevedel-session-checkpoint--candidate-retire-ids candidate))
                    (mevedel-session-checkpoint--state-retired-ptc state))))))
        (setf (alist-get conversation consumed nil nil #'equal)
              (mevedel-session-checkpoint--merge
               (alist-get conversation consumed nil nil #'equal) nil
               (mevedel-session-checkpoint--candidate-consumed-ids candidate))
              (alist-get conversation deferred nil nil #'equal)
              (mevedel-session-checkpoint--merge
               (alist-get conversation deferred nil nil #'equal) nil
               (mevedel-session-checkpoint--candidate-retire-ids candidate))
              (mevedel-session-checkpoint--state-consumed state) consumed
              (mevedel-session-checkpoint--state-deferred state) deferred))
      t)))

(defun mevedel-session-checkpoint-release-proofs (session)
  "Release SESSION's stale-sidecar retirement proofs after a successful drain.
Return nil while any active, queued, staged, or recoverable publication remains.
These proofs outlive neither the outstanding publication work nor its retries."
  (unless (or (mevedel-session-publication-active-p session)
              (mevedel-session-publication-queue session)
              (mevedel-session-publication-uncommitted-batches session)
              (mevedel-session-pending-publication session))
    (when-let* ((state (mevedel-session-checkpoint-state session)))
      (setf (mevedel-session-checkpoint--state-retired-mail state) nil
            (mevedel-session-checkpoint--state-retired-ptc state) nil))
    t))

(provide 'mevedel-session-checkpoint)
;;; mevedel-session-checkpoint.el ends here
