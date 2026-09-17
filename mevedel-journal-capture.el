;;; mevedel-journal-capture.el -- Durable completed-work captures -*- lexical-binding: t -*-

;;; Commentary:

;; Checkpoints completed root work after authoritative save.  Each capture
;; has immutable evidence, notes, model selection, and source identities.
;; Evidence pins are installed before the capture becomes ready.  No model
;; request runs while saving a checkpoint.

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

(defun mevedel-journal-capture--pending-p (workspace id)
  "Return non-nil when capture ID in WORKSPACE is ready and not retired."
  (and (mevedel-journal-capture--marked-p workspace id "ready")
       (not (mevedel-journal-capture--marked-p workspace id "retired"))))

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
    (unless (memq trigger '(session-end compaction clear)) (error "Invalid journal capture trigger"))
    (plist-put metadata :trigger trigger)))

(defun mevedel-journal-capture--read (workspace id)
  "Read and validate WORKSPACE's immutable capture ID, or nil if absent."
  (condition-case nil
      (let* ((text (mevedel-session-control-fs-read-file
                (mevedel-journal-capture--file workspace id "capture.json")
                'utf-8-unix (1+ mevedel-journal-store--entry-max-bytes)))
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
                                     (integerp (plist-get policy :max-tokens))
                                     (< 0 (plist-get policy :max-tokens))
                                     (<= (plist-get policy :max-tokens) 4000)
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
INCLUDE-INACTIVE includes unready and retired records for storage recovery."
  (let (records)
    (dolist (directory
             (mevedel-session-control-fs-list-directory
              (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "captures")
              mevedel-journal-store-id-regexp))
      (let ((id (file-name-nondirectory (directory-file-name directory))))
        (when (or include-inactive (mevedel-journal-capture--pending-p workspace id))
          (push (condition-case err
                    (or (mevedel-journal-capture--read workspace id)
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
                       (memq trigger '(compaction session-end clear)))
            (error "Invalid journal capture seal"))
          trigger))
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-journal-capture-seal (session buffer trigger &optional captures)
  "Seal completed SESSION checkpoints with TRIGGER while BUFFER owns it.
TRIGGER is `compaction', `session-end', or `clear'.  Optional CAPTURES selects
a frozen list from before compaction or clear; otherwise select the current
ready checkpoints.
Repeated triggers preserve the first seal.  This reads no mutable transcript,
starts no inference, and returns the sealed capture descriptors."
  (unless (memq trigger '(compaction session-end clear))
    (error "Invalid journal capture trigger"))
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
            (funcall seal)))))))

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
  "Return SESSION's durably indexed completed turns with stable identities."
  (let (turns)
    (dolist (segment (mevedel-session-prompt-index session))
      (dolist (prompt (cdr segment))
        (when (and (integerp (plist-get prompt :cum-turn))
                   (<= (plist-get prompt :cum-turn) (mevedel-session-turn-count session))
                   (stringp (plist-get prompt :fork-point-id))
                   (integerp (plist-get prompt :transcript-cutoff)))
          (push (list :number (plist-get prompt :cum-turn)
                      :id (secure-hash 'sha256 (plist-get prompt :fork-point-id))
                      :fork-point (plist-get prompt :fork-point-id)
                      :segment (car segment) :start (plist-get prompt :pos)
                      :end (plist-get prompt :transcript-cutoff))
                turns))))
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
      (let* ((policy (mevedel-model-resolve-workload 'journal))
             (policy (mevedel-context-summary-digest-policy
                      (plist-put (copy-sequence policy) :max-tokens
                                 (or (plist-get policy :max-tokens) gptel-max-tokens)))))
        (list :provider (mevedel-model--provider-label policy)
              :effort (and (plist-get policy :effort) (symbol-name (plist-get policy :effort)))
              :stream (if gptel-stream t :false)
              :max-tokens (plist-get policy :max-tokens)))
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
        (mevedel-session-artifacts-assert-mutation-authority session buffer)
        (if (mevedel-session-codec-portable-authority-p session)
            (mevedel-session-durability-call-with-reserved-lease
             session (lambda () (mevedel-journal-capture--checkpoint-owned session)))
          (mevedel-journal-capture--checkpoint-owned session))))))

(defun mevedel-journal-capture--checkpoint-owned (session)
  "Freeze and pin completed SESSION work while its source authority is held."
  (let* ((workspace (mevedel-session-workspace session))
         (root (mevedel-workspace-root workspace))
         (pending (mevedel-journal-capture-list workspace))
         (covered
          (append (mevedel-journal-store-covered-turns root)
                  (mapcan
                   (lambda (capture)
                     (when (mevedel-session-control-fs-path-exists-p
                            (mevedel-journal-capture--file workspace (plist-get capture :id) "seal.json"))
                       (copy-sequence (plist-get (plist-get capture :metadata) :turn-ids))))
                   pending)))
         (turns (cl-remove-if
                 (lambda (turn) (member (plist-get turn :id) covered))
                 (mevedel-journal-capture--turns session))))
    (when turns
      (let* ((ids (mapcar (lambda (turn) (plist-get turn :id)) turns))
             (id (mevedel-journal-capture--identity (mevedel-session-session-id session) ids))
             (retired (or (mevedel-journal-capture--marked-p workspace id "retired")
                          (mevedel-journal-capture--marked-p workspace id "discard.json")))
             (capture (and (not retired) (mevedel-journal-capture--read workspace id))))
        (unless retired
          (unless capture
            (let* ((projection (mevedel-journal-evidence-turns session turns))
                   (evidence (encode-coding-string (plist-get projection :text) 'utf-8-unix))
                   (notes (encode-coding-string
                           (mevedel-journal-evidence-notes session)
                           'utf-8-unix))
                   (head (and (mevedel-session-codec-portable-authority-p session)
                              (plist-get (mevedel-session-publication session) :head)))
                   (policy (mevedel-journal-capture--freeze-policy))
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
                          :source-client (mevedel-workspace-identity-client)
                          :source-kind (if (and (mevedel-session-codec-portable-authority-p session)
                                                (equal (file-remote-p root)
                                                       (file-remote-p (mevedel-session-save-path session))))
                                           "target" "client")
                          :head head :sources (vconcat (plist-get projection :sources))
                          :evidence (decode-coding-string evidence 'utf-8-unix)
                          :notes (decode-coding-string notes 'utf-8-unix)
                          :evidence-sha256 (secure-hash 'sha256 evidence)
                          :notes-sha256 (secure-hash 'sha256 notes)))
              ;; One exclusive file publishes the descriptor AND its
              ;; source snapshot, so a crash cannot separate the notes
              ;; from the metadata needed to recover them.
              (let ((text (json-serialize capture)))
                (when (> (string-bytes text) mevedel-journal-store--entry-max-bytes)
                  (error "Capture exceeds its storage limit"))
                (mevedel-session-control-fs-make-directory (mevedel-journal-capture--directory workspace id) t)
                (mevedel-session-control-fs-create-file
                 (mevedel-journal-capture--file workspace id "capture.json") text))
              (setq capture (mevedel-journal-capture--read workspace id))))
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
          capture)))))

(provide 'mevedel-journal-capture)
;;; mevedel-journal-capture.el ends here
