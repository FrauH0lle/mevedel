;;; mevedel-journal-capture.el -- Durable completed-work captures -*- lexical-binding: t -*-

;;; Commentary:

;; Checkpoints completed root work after authoritative save.  Each capture
;; has immutable evidence, notes, model selection, and source identities.
;; Evidence pins are installed before the capture becomes ready.  No model
;; request runs while saving a checkpoint.  Lifecycle events seal ready
;; checkpoints with a trigger from `mevedel-journal-store-triggers'; only
;; sealed captures are eligible for digest generation.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-evidence)
(require 'mevedel-journal-pins)
(require 'mevedel-journal-store)
(require 'mevedel-models)
(require 'mevedel-structs)
(require 'mevedel-workspace-identity)

;; `gptel'
(defvar gptel-max-tokens)
(defvar gptel-stream)

;; `mevedel-journal-process'
(declare-function mevedel-journal-process-schedule "mevedel-journal-process" (workspace &optional recover))
(autoload 'mevedel-journal-process-schedule "mevedel-journal-process")

;; `mevedel-journal-worker'
(declare-function mevedel-journal-worker-start "mevedel-journal-worker"
                  (workspace operation callback &optional force payload))
(declare-function mevedel-journal-worker-supported-p "mevedel-journal-worker" (workspace))

;; `mevedel-session-artifacts'
(declare-function mevedel-session-artifacts-assert-mutation-authority
                  "mevedel-session-artifacts" (session &optional buffer))
(autoload 'mevedel-session-artifacts-assert-mutation-authority "mevedel-session-artifacts")

;; `mevedel-session-codec'
(declare-function mevedel-session-codec-portable-authority-p "mevedel-session-codec" (session))
(autoload 'mevedel-session-codec-portable-authority-p "mevedel-session-codec")

;; `mevedel-session-durability'
(declare-function mevedel-session-durability-call-with-reserved-lease
                  "mevedel-session-durability" (session function))
(declare-function mevedel-session-durability-lease-owned-p "mevedel-session-durability" (session))
(autoload 'mevedel-session-durability-call-with-reserved-lease "mevedel-session-durability")
(autoload 'mevedel-session-durability-lease-owned-p "mevedel-session-durability")
;; The transaction scope is a macro over variables its lazy owners define.
(eval-when-compile (require 'mevedel-session-durability))

;; `mevedel-telemetry'
(declare-function mevedel-telemetry-record "mevedel-telemetry" (session event &rest props))
(autoload 'mevedel-telemetry-record "mevedel-telemetry")

(defcustom mevedel-journal-enabled t
  "Capture completed workspace work for the journal when non-nil.
Disabling capture preserves existing entries, pending work, and evidence pins."
  :type 'boolean :group 'mevedel)

(defun mevedel-journal-capture--identity (session-id turn-ids)
  "Return the durable capture identity for SESSION-ID's completed TURN-IDS."
  (secure-hash 'sha256 (prin1-to-string (list session-id turn-ids))))

(defun mevedel-journal-capture--source-revision (head sources)
  "Return the frozen source revision for publication HEAD and segment SOURCES."
  (secure-hash 'sha256
               (prin1-to-string
                (list head (mapcar (lambda (source)
                                     (list :logical (plist-get source :logical)
                                           :sha256 (plist-get source :sha256)))
                                   sources)))))

(defun mevedel-journal-capture--directory (workspace id)
  "Return WORKSPACE's private capture directory for ID."
  (unless (mevedel-journal-store-id-p id) (error "Invalid journal capture identity"))
  (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "captures" id))

(defun mevedel-journal-capture--file (workspace id name)
  "Return private capture file NAME for capture ID in WORKSPACE.
NAME is one of the closed marker spellings: capture.json holds the frozen
descriptor, ready and retired mark eligibility, seal.json and discard.json
record the trigger or omission, and attempts holds the request claims."
  (file-name-concat (mevedel-journal-capture--directory workspace id) name))

(defun mevedel-journal-capture--marked-p (workspace id name)
  "Return non-nil when capture ID in WORKSPACE has marker file NAME."
  (mevedel-session-control-fs-path-exists-p (mevedel-journal-capture--file workspace id name)))

(defun mevedel-journal-capture--markers (workspace id names)
  "Return, in NAMES order, whether capture ID in WORKSPACE has each marker.
One target program answers them all."
  (mevedel-session-control-fs-paths-exist
   (mapcar (lambda (name) (mevedel-journal-capture--file workspace id name))
           names)))

(defun mevedel-journal-capture--closed-p (workspace id)
  "Return non-nil when capture ID in WORKSPACE is retired or discarded."
  (memq t (mevedel-journal-capture--markers
           workspace id '("retired" "discard.json"))))

(defun mevedel-journal-capture--pending-p (workspace id)
  "Return non-nil when capture ID in WORKSPACE is ready and not retired."
  (equal '(t nil) (mevedel-journal-capture--markers
                   workspace id '("ready" "retired"))))

(defun mevedel-journal-capture--write-seal (workspace id trigger)
  "Record TRIGGER as the seal of capture ID in WORKSPACE, keeping any first seal."
  (mevedel-session-control-fs-create-file
   (mevedel-journal-capture--file workspace id "seal.json")
   (json-serialize (list :capture-id id :trigger (symbol-name trigger)))))

(defun mevedel-journal-capture--retire (workspace capture reason)
  "Retire CAPTURE in WORKSPACE with marker REASON once its outcome is durable.
Release the source pin first, then write the retired marker and drop the raw
evidence bundle; accepted output is never deleted here."
  (let ((id (plist-get capture :id)))
    (mevedel-journal-pins-release (mevedel-journal-capture--source-directory workspace capture) id)
    (mevedel-session-control-fs-create-file (mevedel-journal-capture--file workspace id "retired") reason)
    (condition-case nil
        (mevedel-session-control-fs-delete-file (mevedel-journal-capture--file workspace id "capture.json"))
      (mevedel-session-control-fs-absent nil))))

(defun mevedel-journal-capture--metadata (record)
  "Return RECORD's public metadata with the closed trigger symbol restored."
  (let* ((metadata (copy-tree (plist-get record :metadata)))
         (trigger (intern-soft (plist-get metadata :trigger))))
    (unless (memq trigger mevedel-journal-store-triggers) (error "Invalid journal capture trigger"))
    (plist-put metadata :trigger trigger)))

(defun mevedel-journal-capture--read-operation (workspace id)
  "Return the bounded control operation reading capture ID in WORKSPACE."
  (list :op 'read :path (mevedel-journal-capture--file workspace id "capture.json")
        :coding 'utf-8-unix :max-bytes (1+ mevedel-journal-store--entry-max-bytes)))

(defun mevedel-journal-capture--read (workspace id &optional observation)
  "Read and validate WORKSPACE's immutable capture ID, or nil if absent.
OBSERVATION is the result of `mevedel-journal-capture--read-operation' from
the caller's own program, used instead of a fresh read."
  (condition-case nil
      (let* ((text (mevedel-session-control-fs-program-value
                    (or observation
                        (car (mevedel-session-control-fs-run-program
                              (list (mevedel-journal-capture--read-operation
                                     workspace id)))))))
             (record
              (progn
                (when (> (string-bytes text) mevedel-journal-store--entry-max-bytes)
                  (error "Journal capture descriptor exceeds its byte limit"))
                (json-parse-string text :object-type 'plist :array-type 'list
                                   :null-object nil :false-object nil)))
             (metadata (mevedel-journal-capture--metadata record))
             (sources (plist-get record :sources)))
        (unless (and (proper-list-p record) (= (length record) 24)
                     (cl-every (lambda (key) (plist-member record key))
                               '(:id :metadata :policy :source-directory
                                     :source-client :source-kind :head :sources
                                     :evidence :notes :evidence-sha256 :notes-sha256))
                     (equal id (plist-get record :id))
                     (equal id (plist-get metadata :capture-id))
                     (equal id (mevedel-journal-capture--identity
                                (plist-get metadata :session) (plist-get metadata :turn-ids)))
                     (equal (plist-get metadata :workspace)
                            (mevedel-workspace-identity-read (mevedel-workspace-root workspace)))
                     (stringp (plist-get record :source-directory))
                     (file-name-absolute-p (plist-get record :source-directory))
                     (not (file-remote-p (plist-get record :source-directory)))
                     (stringp (plist-get record :source-client))
                     (mevedel-journal-store-id-p (plist-get record :source-client))
                     (member (plist-get record :source-kind) '("target" "client"))
                     (or (equal (plist-get record :source-kind) "client") (plist-get record :head))
                     (consp sources) (proper-list-p sources)
                     (cl-every
                      (lambda (source)
                        (and (proper-list-p source) (= 4 (length source))
                             (plist-member source :logical) (plist-member source :sha256)
                             (stringp (plist-get source :logical))
                             (string-match-p "\\`segment-[0-9]\\{4,\\}\\.chat\\.org\\'"
                                             (plist-get source :logical))
                             (mevedel-journal-store-id-p (plist-get source :sha256))))
                      sources)
                     (= (length sources)
                        (length (delete-dups (mapcar (lambda (source) (plist-get source :logical)) sources))))
                     (equal (plist-get metadata :source-revision)
                            (mevedel-journal-capture--source-revision (plist-get record :head) sources))
                     (stringp (plist-get record :evidence))
                     (<= (string-bytes (plist-get record :evidence)) (* 1024 1024))
                     (stringp (plist-get record :notes))
                     (<= (string-bytes (plist-get record :notes)) 32768)
                     (let ((policy (plist-get record :policy)))
                       (and (proper-list-p policy)
                            (or (and (= (length policy) 2) (stringp (plist-get policy :error)))
                                (and (= (length policy) 8)
                                     (cl-every (lambda (key) (plist-member policy key))
                                               '(:provider :effort :max-tokens :stream))
                                     (stringp (plist-get policy :provider))
                                     (or (null (plist-get policy :effort)) (stringp (plist-get policy :effort)))
                                     (or (null (plist-get policy :max-tokens))
                                         (and (integerp (plist-get policy :max-tokens))
                                              (< 0 (plist-get policy :max-tokens))))
                                     (memq (plist-get policy :stream) '(nil t))))))
                     (cl-every (lambda (field) (mevedel-journal-store-id-p (plist-get record field)))
                               '(:evidence-sha256 :notes-sha256))
                     (or (null (plist-get record :head))
                         (and (stringp (plist-get record :head))
                              (string-match-p mevedel-journal-pins--head-regexp
                                              (plist-get record :head)))))
          (error "Invalid journal capture descriptor"))
        (mevedel-journal-store--validate-metadata metadata)
        record)
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-journal-capture-list (workspace &optional include-inactive)
  "Return WORKSPACE's ready, unretired capture descriptors.
This inspection remains available when journaling is disabled.
INCLUDE-INACTIVE includes unready and interrupted retired records for recovery.
Fully retired tombstones with no descriptor need no further recovery.
Observe directory markers in bounded pinned batches, not separate processes
for every marker.  Observations belong only to this call."
  (let ((directories
         (mevedel-session-control-fs-list-directory
          (file-name-concat (mevedel-journal-store-state-directory
                             (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
                            "captures")
          mevedel-journal-store-id-regexp))
        records)
    (while directories
      (let* ((batch (seq-take directories 16))
             (observations
              (mevedel-session-control-fs-run-program
               (mapcar (lambda (directory)
                         (list :op 'list-directory :path directory :optional t)) batch)))
             selected)
        (setq directories (nthcdr (length batch) directories))
        (dolist (observed observations)
          (unless (eq (plist-get observed :status) 'absent)
            (let* ((names (mevedel-session-control-fs-program-value observed))
                   (id (file-name-nondirectory (plist-get observed :path)))
                   (retired (member "retired" names)))
              (when (if include-inactive
                        (not (and retired (not (member "capture.json" names))))
                      (and (member "ready" names) (not retired)))
                (push id selected)))))
        ;; The selected descriptors are read together, in one more program.
        (setq selected (nreverse selected))
        (cl-loop
         for id in selected
         for read in (and selected
                          (mevedel-session-control-fs-run-program
                           (mapcar (lambda (id)
                                     (append (mevedel-journal-capture--read-operation workspace id)
                                             (list :optional t)))
                                   selected)))
         do (push (condition-case err
                      (or (mevedel-journal-capture--read workspace id read)
                          (error "Ready journal capture has no descriptor"))
                    (error (list :id id :unreadable t :error (error-message-string err))))
                  records))))
    (sort records (lambda (left right)
                    (string< (or (plist-get (plist-get left :metadata) :created) "")
                             (or (plist-get (plist-get right :metadata) :created) ""))))))

(defun mevedel-journal-capture-evidence (workspace capture)
  "Read and verify frozen evidence for CAPTURE in WORKSPACE."
  (let ((record (or (mevedel-journal-capture--read workspace (plist-get capture :id))
                    (error "Journal capture descriptor is missing")))
        texts)
    (dolist (field '((:evidence . :evidence-sha256) (:notes . :notes-sha256)))
      (let ((text (plist-get record (car field))))
        (unless (and (stringp text)
                     (equal (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix))
                            (plist-get record (cdr field))))
          (error "Journal capture evidence hash mismatch: %s" (car field)))
        (push text texts)))
    (string-join (nreverse texts) "\n")))

(defun mevedel-journal-capture-trigger (workspace capture)
  "Return CAPTURE's sealed trigger in WORKSPACE, or nil when unsealed.
A malformed seal signals an error; it never makes its evidence eligible for
another checkpoint."
  (condition-case nil
      (let* ((id (plist-get capture :id))
             (seal (json-parse-string
                    (mevedel-session-control-fs-read-file
                     (mevedel-journal-capture--file workspace id "seal.json")
                     'utf-8-unix 1025)
                    :object-type 'plist)))
        (let ((trigger (intern-soft (plist-get seal :trigger))))
          (unless (and (= (length seal) 4)
                       (equal id (plist-get seal :capture-id))
                       (memq trigger mevedel-journal-store-triggers))
            (error "Invalid journal capture seal"))
          trigger))
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-journal-capture-seal (session buffer trigger &optional captures)
  "Seal completed SESSION checkpoints with TRIGGER while BUFFER owns it.
TRIGGER is a member of `mevedel-journal-store-triggers'.  Optional CAPTURES
selects a frozen list from before compaction or clear; otherwise select the
current ready checkpoints.
Repeated triggers preserve the first seal.  This reads no mutable transcript,
starts no inference, and returns the sealed capture descriptors."
  (unless (memq trigger mevedel-journal-store-triggers)
    (error "Invalid journal capture trigger"))
  ;; One transaction: the ownership test, the authority assertion and the
  ;; reservation share one target clock reading.
  (mevedel-session-durability-with-transaction
    (when (and mevedel-journal-enabled
               (buffer-live-p buffer)
               (mevedel-workspace-p (mevedel-session-workspace session))
               (eq buffer (mevedel-session-root-buffer session))
               (mevedel-session-save-path session)
               (or (not (mevedel-session-codec-portable-authority-p session))
                   (mevedel-session-durability-lease-owned-p session)))
      (with-current-buffer buffer
        (unless (or (bound-and-true-p mevedel--agent-invocation)
                    (bound-and-true-p mevedel-session--read-only-mode))
          (mevedel-session-artifacts-assert-mutation-authority session buffer)
          (let ((seal (lambda () (mevedel-journal-capture--seal-owned session trigger captures))))
            (if (mevedel-session-codec-portable-authority-p session)
                (mevedel-session-durability-call-with-reserved-lease session seal)
              (funcall seal))))))))

(defun mevedel-journal-capture-seal-and-schedule (session buffer trigger &optional captures)
  "Seal SESSION's checkpoints with TRIGGER, then schedule digest processing.
BUFFER and CAPTURES are as for `mevedel-journal-capture-seal'.  Failures are
reported as warnings so a lifecycle event never fails on journaling; the
scheduler itself declines work while exit inhibits scheduling."
  (condition-case err
      (when (mevedel-journal-capture-seal session buffer trigger captures)
        (mevedel-journal-process-schedule (mevedel-session-workspace session)))
    (error
     (display-warning 'mevedel
                      (format "Journal capture sealing failed: %s" (error-message-string err))
                      :warning))))

(defun mevedel-journal-capture--seal-owned (session trigger captures)
  "Seal selected CAPTURES with TRIGGER while SESSION's authority is held."
  (let* ((workspace (mevedel-session-workspace session))
         (source (mevedel-session-control-fs-physical-path (mevedel-session-save-path session)))
         sealed)
    (dolist (capture (or captures (mevedel-journal-capture-list workspace)))
      (when (equal (plist-get (plist-get capture :metadata) :session)
                   (mevedel-session-session-id session))
        (when (and (equal source (mevedel-session-control-fs-physical-path
                                  (mevedel-journal-capture--source-directory workspace capture)))
                   (mevedel-journal-capture--pending-p workspace (plist-get capture :id)))
          (mevedel-journal-capture-evidence workspace capture)
          (mevedel-journal-capture--write-seal workspace (plist-get capture :id) trigger)
          (mevedel-journal-capture-trigger workspace capture)
          (push capture sealed))))
    (nreverse sealed)))

(defun mevedel-journal-capture--turns (session)
  "Return SESSION's durably indexed completed turns with stable identities.
A turn that auto-compaction carried into a later segment is read from its
recorded `:continuation' there; that segment's inherited summary covers the
work before the compaction."
  (let (turns)
    (dolist (segment (mevedel-session-prompt-index session))
      (dolist (prompt (cdr segment))
        (let* ((continuation (and (not (plist-get prompt :fork-point-id))
                                  (plist-get prompt :continuation)))
               (source (or continuation prompt)))
          (when (and (integerp (plist-get prompt :cum-turn))
                     (<= (plist-get prompt :cum-turn)
                         (mevedel-session-turn-count session))
                     (stringp (plist-get source :fork-point-id))
                     (integerp (plist-get source :transcript-cutoff)))
            (push (list :number (plist-get prompt :cum-turn)
                        :id (secure-hash 'sha256
                                         (plist-get source :fork-point-id))
                        :fork-point (plist-get source :fork-point-id)
                        :segment (if continuation
                                     (plist-get continuation :segment)
                                   (car segment))
                        :start (plist-get (if continuation continuation prompt)
                                          (if continuation :start :pos))
                        :end (plist-get source :transcript-cutoff))
                  turns)))))
    (sort turns (lambda (left right) (< (plist-get left :number) (plist-get right :number))))))

(defun mevedel-journal-capture--source-directory (workspace capture)
  "Resolve CAPTURE's original source directory from WORKSPACE's target.
Client-local evidence never silently rebinds to another client."
  (if (equal (plist-get capture :source-kind) "target")
      (concat (file-remote-p (mevedel-workspace-root workspace))
              (plist-get capture :source-directory))
    (unless (equal (plist-get capture :source-client) (mevedel-workspace-identity-client))
      (error "Journal capture source belongs to another client"))
    (plist-get capture :source-directory)))

(defun mevedel-journal-capture--ready (workspace capture session-dir)
  "Pin CAPTURE's owned SESSION-DIR before making it ready in WORKSPACE.
Return non-nil only when the ready marker was newly created."
  (unless (equal (mevedel-session-control-fs-physical-path session-dir)
                 (mevedel-session-control-fs-physical-path
                  (mevedel-journal-capture--source-directory workspace capture)))
    (error "Journal capture does not belong to the owned source session"))
  (mevedel-journal-pins-retain
   session-dir
   (plist-get capture :id)
   (and (plist-get capture :head) (list (plist-get capture :head))))
  (mevedel-session-control-fs-create-file
   (mevedel-journal-capture--file workspace (plist-get capture :id) "ready")
   "ready\n"))

(defun mevedel-journal-capture--freeze-policy ()
  "Freeze serializable journal selection in the current root buffer."
  (condition-case err
      (let ((policy (mevedel-model-resolve-workload 'journal)))
        (list :provider (mevedel-model--provider-label policy)
              :effort (and (plist-get policy :effort) (symbol-name (plist-get policy :effort)))
              :stream (if gptel-stream t :false)
              :max-tokens (or (plist-get policy :max-tokens) gptel-max-tokens)))
    (error (list :error (error-message-string err)))))

(defun mevedel-journal-capture-checkpoint (session buffer)
  "Checkpoint completed SESSION work after authoritative save of BUFFER.
Return a durable capture descriptor or nil when disabled or no uncovered
completed work exists.  The caller isolates storage errors from conversation
settlement.  This performs no inference and does not seal the capture."
  (when (and mevedel-journal-enabled
             (buffer-live-p buffer)
             (eq buffer (mevedel-session-root-buffer session))
             (mevedel-session-save-path session)
             (not (mevedel-session-pending-publication session))
             (not (mevedel-session-publication-active-p session))
             (null (mevedel-session-publication-queue session)))
    (with-current-buffer buffer
      (unless (or (bound-and-true-p mevedel--agent-invocation)
                  (bound-and-true-p mevedel-session--read-only-mode))
        ;; The assertion and the reservation share one clock reading.
        (mevedel-session-durability-with-transaction
          (mevedel-session-artifacts-assert-mutation-authority session buffer)
          (if (mevedel-session-codec-portable-authority-p session)
              (mevedel-session-durability-call-with-reserved-lease
               session (lambda () (mevedel-journal-capture--checkpoint-owned session)))
            (mevedel-journal-capture--checkpoint-owned session)))))))

(defun mevedel-journal-capture--prepare (session policy client)
  "Prepare SESSION's frozen checkpoint without publishing or pinning it.
POLICY and CLIENT are captured by the owning editor.  Only committed artifacts
are read, so a worker needs no source mutation authority."
  (let* ((workspace (mevedel-session-workspace session))
         (root (mevedel-workspace-root workspace))
         (pending (mevedel-journal-capture-list workspace))
         (covered
          (append (mevedel-journal-store-covered-turns root)
                  (cl-mapcan
                   (lambda (capture sealed)
                     (when sealed
                       (copy-sequence (plist-get (plist-get capture :metadata) :turn-ids))))
                   pending
                   (mevedel-session-control-fs-paths-exist
                    (mapcar (lambda (capture)
                              (mevedel-journal-capture--file
                               workspace (plist-get capture :id) "seal.json"))
                            pending)))))
         (turns (cl-remove-if
                 (lambda (turn) (member (plist-get turn :id) covered))
                 (mevedel-journal-capture--turns session))))
    (when turns
      (let* ((ids (mapcar (lambda (turn) (plist-get turn :id)) turns))
             (id (mevedel-journal-capture--identity (mevedel-session-session-id session) ids)))
        (unless (mevedel-journal-capture--closed-p workspace id)
          (let* ((capture (mevedel-journal-capture--read workspace id))
                 (new (null capture)))
            (unless capture
              (let* ((projection (mevedel-journal-evidence-turns session turns))
                     (evidence (encode-coding-string (plist-get projection :text) 'utf-8-unix))
                     (notes (encode-coding-string
                             (mevedel-journal-evidence-notes session)
                             'utf-8-unix))
                     (head (and (mevedel-session-codec-portable-authority-p session)
                                (plist-get (mevedel-session-publication session) :head)))
                     (metadata
                      (list :capture-id id :session (mevedel-session-session-id session)
                            :session-name (mevedel-session-name session)
                            :workspace (mevedel-workspace-identity-ensure root)
                            :trigger 'session-end :segment (or (mevedel-session-current-segment session) 1)
                            :source-revision (mevedel-journal-capture--source-revision
                                              head (plist-get projection :sources))
                            :turns (mapcar (lambda (turn) (plist-get turn :number)) turns)
                            :turn-ids ids
                            :created (mevedel-journal-store-timestamp
                                      (mevedel-session-control-fs-target-time root))
                            :model (or (plist-get policy :provider) "unavailable"))))
                (mevedel-journal-store--validate-metadata metadata)
                (plist-put metadata :trigger "session-end")
                (plist-put metadata :turns (vconcat (plist-get metadata :turns)))
                (plist-put metadata :turn-ids (vconcat ids))
                (setq capture
                      (list :id id :metadata metadata :policy policy
                            :source-directory (file-local-name (mevedel-session-save-path session))
                            :source-client client
                            :source-kind (if (and (mevedel-session-codec-portable-authority-p session)
                                                  (equal (file-remote-p root)
                                                         (file-remote-p (mevedel-session-save-path session))))
                                             "target" "client")
                            :head head :sources (vconcat (plist-get projection :sources))
                            :evidence (decode-coding-string evidence 'utf-8-unix)
                            :notes (decode-coding-string notes 'utf-8-unix)
                            :evidence-sha256 (secure-hash 'sha256 evidence)
                            :notes-sha256 (secure-hash 'sha256 notes)))))
            (list :capture capture :new new
                  :pending
                  (mapcar (lambda (record)
                            (list :id (plist-get record :id) :metadata (plist-get record :metadata)
                                  :source-kind (plist-get record :source-kind)
                                  :source-client (plist-get record :source-client)
                                  :source-directory (plist-get record :source-directory)))
                          pending))))))))

(defun mevedel-journal-capture--publish (session prepared)
  "Publish and pin PREPARED while SESSION's source mutation authority is held."
  (when prepared
    (let* ((workspace (mevedel-session-workspace session))
           (capture (plist-get prepared :capture))
           (id (plist-get capture :id))
           (pending (plist-get prepared :pending))
           (ids (append (plist-get (plist-get capture :metadata) :turn-ids) nil)))
      (unless (mevedel-journal-capture--closed-p workspace id)
        (when (plist-get prepared :new)
          ;; Descriptor and evidence still share one exclusive publication.
          (let ((text (json-serialize capture)))
            (when (> (string-bytes text) mevedel-journal-store--entry-max-bytes)
              (error "Capture exceeds its storage limit"))
            (mevedel-session-control-fs-make-directory (mevedel-journal-capture--directory workspace id) t)
            (mevedel-session-control-fs-create-file
             (mevedel-journal-capture--file workspace id "capture.json") text))
          (setq capture (mevedel-journal-capture--read workspace id)))
        (when (mevedel-journal-capture--ready workspace capture (mevedel-session-save-path session))
          (mevedel-telemetry-record
           session 'journal-capture-queued :capture-id id :trigger 'checkpoint
           :input-bytes (+ (string-bytes (plist-get capture :evidence))
                           (string-bytes (plist-get capture :notes)))))
        (dolist (old pending)
          (when (and (not (equal id (plist-get old :id)))
                     (equal (plist-get (plist-get old :metadata) :session)
                            (mevedel-session-session-id session))
                     (equal (mevedel-session-control-fs-physical-path
                             (mevedel-journal-capture--source-directory workspace old))
                            (mevedel-session-control-fs-physical-path
                             (mevedel-session-save-path session)))
                     (cl-every (lambda (turn-id) (member turn-id ids))
                               (plist-get (plist-get old :metadata) :turn-ids))
                     (not (mevedel-session-control-fs-path-exists-p
                           (mevedel-journal-capture--file workspace (plist-get old :id) "seal.json"))))
            (mevedel-session-control-fs-create-file
             (mevedel-journal-capture--file workspace (plist-get old :id) "retired")
             (format "superseded by %s\n" id))
            (mevedel-journal-pins-release
             (mevedel-journal-capture--source-directory workspace old) (plist-get old :id))))
        capture))))

(defun mevedel-journal-capture--checkpoint-owned (session)
  "Freeze and pin completed SESSION work while its source authority is held."
  (mevedel-journal-capture--publish
   session (mevedel-journal-capture--prepare
            session (mevedel-journal-capture--freeze-policy) (mevedel-workspace-identity-client))))

;;;###autoload
(defun mevedel-journal-capture-checkpoint-start (session buffer callback)
  "Prepare a local portable SESSION checkpoint outside the editor.
BUFFER must remain SESSION's root and its committed head must remain current.
CALLBACK receives the published capture, nil, or (:error MESSAGE).  Return a
cancellation function; cancellation never publishes or calls CALLBACK."
  (require 'mevedel-journal-worker)
  (unless (and (mevedel-journal-worker-supported-p (mevedel-session-workspace session))
               (mevedel-session-codec-portable-authority-p session)
               (buffer-live-p buffer)
               (eq buffer (mevedel-session-root-buffer session)))
    (error "Checkpoint preparation requires a local portable root session"))
  (let* ((head (plist-get (mevedel-session-publication session) :head))
         (directory (mevedel-session-save-path session))
         (request (buffer-local-value 'mevedel--current-request buffer))
         (payload (with-current-buffer buffer
                    (list :session-id (mevedel-session-session-id session)
                          :name (mevedel-session-name session) :save-path directory
                          :publication (copy-tree (mevedel-session-publication session))
                          :turn-count (mevedel-session-turn-count session)
                          :current-segment (mevedel-session-current-segment session)
                          :prompt-index (copy-tree (mevedel-session-prompt-index session))
                          :client (mevedel-workspace-identity-client)
                          :policy (mevedel-journal-capture--freeze-policy))))
         process timer settled)
    (cl-labels
     ((cancel ()
        (setq settled t)
        (when timer (cancel-timer timer))
        (when (and process (process-live-p process)) (delete-process process)))
      (finish (result)
        (unless settled
          (cancel)
          (funcall
           callback
           (condition-case err
               (progn
                 (when (plist-get result :error) (error "%s" (plist-get result :error)))
                 (unless (and mevedel-journal-enabled (buffer-live-p buffer)
                              (eq buffer (mevedel-session-root-buffer session))
                              (eq session (buffer-local-value 'mevedel--session buffer))
                              (eq request (buffer-local-value 'mevedel--current-request buffer))
                              (equal directory (mevedel-session-save-path session))
                              (equal head (plist-get (mevedel-session-publication session) :head))
                              (not (mevedel-session-pending-publication session))
                              (not (mevedel-session-publication-active-p session))
                              (null (mevedel-session-publication-queue session)))
                   (error "Checkpoint source changed during preparation"))
                 (mevedel-session-artifacts-assert-mutation-authority session buffer)
                 (mevedel-session-durability-call-with-reserved-lease
                  session (lambda () (mevedel-journal-capture--publish session (plist-get result :prepared)))))
             (error (list :error (error-message-string err))))))))
     (setq process (mevedel-journal-worker-start
                    (mevedel-session-workspace session) 'capture-prepare #'finish nil payload))
     (unless settled
       (setq timer (run-at-time 120 nil (lambda () (finish '(:error "Checkpoint preparation timed out"))))))
     #'cancel)))

(provide 'mevedel-journal-capture)
;;; mevedel-journal-capture.el ends here
