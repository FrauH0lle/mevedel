;;; niwa-memory-store.el -- Durable consolidation evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Private immutable pass records retain the original scope and whole evidence.
;; Journal mutation ownership serializes evidence pinning with accepted expiry.
;; Preparing a pass does not publish review coverage or grant mutation authority
;; over its memory roots.

;;; Code:

(require 'cl-lib)
(require 'niwa-journal-cleanup)
(require 'niwa-memory-scope)

(defvar read-circle)
(defvar read-eval)

;; `niwa-memory-notes'
(declare-function niwa-memory-notes-housekeeping "niwa-memory-notes" (scope))
(declare-function niwa-memory-notes-index "niwa-memory-notes" (scope))
(declare-function niwa-memory-notes-root "niwa-memory-notes" (scope))
(autoload 'niwa-memory-notes-housekeeping "niwa-memory-notes")
(autoload 'niwa-memory-notes-index "niwa-memory-notes")
(autoload 'niwa-memory-notes-root "niwa-memory-notes")

(defconst niwa-memory-store--max-bytes (* 16 1024 1024)
  "Maximum private immutable pass or write record bytes.
Captured note preimages and checked write records may contain repeated byte
strings with up to fourfold escaping, plus decoded content and metadata.")

(defun niwa-memory-store--directory (workspace id)
  "Return WORKSPACE's private pass directory for ID."
  (unless (niwa-journal-store-id-p id) (error "Invalid memory pass identity"))
  (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory (niwa-workspace-root workspace))) "passes" id))

(defun niwa-memory-store--claim-directory (workspace)
  "Return WORKSPACE's consolidation ownership directory."
  (niwa-journal-store-claim-directory (niwa-journal-store-directory (niwa-workspace-root workspace))
                                         'consolidation))

(defun niwa-memory-store--assert-owned (claim)
  "Reject CLAIM unless it is the current unexpired, unsettled owner."
  (unless (niwa-journal-claim-owned-p claim)
    (error "Memory pass ownership is unavailable")))

(define-error 'niwa-memory-store-mutation-busy "Journal mutation is busy")

(defun niwa-memory-store--mutation (workspace function)
  "Call FUNCTION with WORKSPACE's journal mutation claim.
FUNCTION rechecks the claim before each mutation; accepted immutable outcomes
remain recoverable if an operation fails after committing its publication."
  (let ((claim (niwa-journal-claim-acquire
                (niwa-journal-store-claim-directory
                 (niwa-journal-store-directory (niwa-workspace-root workspace)) 'mutation)
                120)))
    (unless claim (signal 'niwa-memory-store-mutation-busy nil))
    (unwind-protect
        (funcall function claim)
      (niwa-journal-claim-settle claim 'completed ""))))

(defun niwa-memory-store--read-lisp (path &optional observation)
  "Read the bounded private Lisp record at PATH as (DATA . TEXT).
DATA is nil when TEXT is oversized, unreadable, or followed by anything but
whitespace; callers validate DATA's shape and signal their own errors.
OBSERVATION may supply PATH's pinned read from this operation's bounded batch."
  (when (and observation (not (equal path (plist-get observation :path))))
    (error "Memory record observation addresses another path"))
  (let* ((text (if observation (niwa-session-control-fs-program-value observation)
                 (niwa-session-control-fs-read-file path 'utf-8-unix (1+ niwa-memory-store--max-bytes))))
         (read-eval nil) (read-circle nil)
         (parsed (and (<= (string-bytes text) niwa-memory-store--max-bytes)
                      (ignore-errors (read-from-string text)))))
    (cons (and parsed (string-blank-p (substring text (cdr parsed))) (car parsed)) text)))

(defun niwa-memory-store--print (value)
  "Encode VALUE as bounded plain durable Lisp, preserving literal byte strings."
  (let* ((print-length nil) (print-level nil) (print-circle nil)
         (print-escape-nonascii t) (print-escape-multibyte t)
         (text (prin1-to-string value)))
    (when (> (string-bytes text) niwa-memory-store--max-bytes)
      (error "Memory pass record exceeds its byte limit"))
    text))

(defun niwa-memory-store--entry (entry)
  "Freeze the complete body and source fingerprint of validated journal ENTRY."
  (append (cl-loop for key in '(:id :file :session :session-name :created :body)
                   append (list key (plist-get entry key)))
          (list :sha256 (secure-hash 'sha256 (plist-get entry :text)))))

(defun niwa-memory-store--pin (workspace id digest)
  "Return WORKSPACE's pin path for pass ID and DIGEST identity."
  (unless (and (niwa-journal-store-id-p id) (niwa-journal-store-id-p digest))
    (error "Invalid memory evidence identity"))
  (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory (niwa-workspace-root workspace))) "evidence-pins" digest (concat id ".pin")))

(defun niwa-memory-store-read (workspace id &optional observation)
  "Read WORKSPACE's immutable prepared pass ID, including its private scope.
This internal storage interface grants no right to display original memory
bodies; inspection must use the original authority checks on the returned scope.
OBSERVATION may supply the :prepared and :claim reads and the earlier
:retired probe from the same batched operation."
  (let* ((root (niwa-workspace-root workspace))
         (path (file-name-concat (niwa-memory-store--directory workspace id) "prepared.el"))
         ;; One program probes retirement, then reads the record.  It is no
         ;; atomic snapshot; retirement is still decided before the record.
         (observed (unless observation
                     (niwa-session-control-fs-run-program
                      (list (list :op 'path-exists-p :optional t
                                  :path (niwa-journal-cleanup--retired-path root id))
                            (list :op 'read :optional t :path path :coding 'utf-8-unix
                                  :max-bytes (1+ niwa-memory-store--max-bytes))))))
         (record (if (niwa-journal-cleanup-pass-retired-p
                      root id (if observation (plist-get observation :retired) (car observed)))
                     (error "Memory pass has expired")
                   (niwa-memory-store--read-lisp
                    path (if observation (plist-get observation :prepared) (cadr observed)))))
         (data (car record)) (text (cdr record))
         (claim (and (proper-list-p data) (plist-get data :claim)))
         (entries (plist-get data :entries)))
    (unless (and (proper-list-p data) (= (length data) 12)
                 (cl-every (lambda (key) (plist-member data key)) '(:id :claim :scope :entries :focus :created))
                 (equal id (plist-get data :id))
                 (equal id (plist-get claim :owner))
                 (equal (plist-get (plist-get data :scope) :workspace)
                        (niwa-workspace-identity-read (niwa-workspace-root workspace)))
                 (stringp (plist-get data :focus)) (<= (string-bytes (plist-get data :focus)) 4096)
                 (proper-list-p entries) (<= (length entries) 20)
                 (cl-every (lambda (entry)
                             (and (proper-list-p entry) (= (length entry) 14)
                                  (cl-every (lambda (key) (plist-member entry key))
                                            '(:id :file :session :session-name :created :body :sha256))
                                  (niwa-journal-store-id-p (plist-get entry :id))
                                  (niwa-journal-store-id-p (plist-get entry :sha256))
                                  (stringp (plist-get entry :file))
                                  (niwa-journal-store-file-name-p (plist-get entry :file))
                                  (stringp (plist-get entry :body))
                                  (<= (string-bytes (plist-get entry :body)) niwa-context-summary--digest-max-bytes))) entries))
      (error "Invalid prepared memory pass"))
    (let ((token (append (list :directory (niwa-memory-store--claim-directory workspace)) claim)))
      (unless (equal claim (niwa-journal-claim--read
                            (niwa-journal-claim--path token nil) nil (plist-get observation :claim)))
        (error "Prepared pass does not match its ownership record"))
      (plist-put data :claim token))
    (append data (list :hash (secure-hash 'sha256 text)))))

(defun niwa-memory-store-prepare (workspace claim scope entries focus)
  "Persist SCOPE and complete ENTRIES for WORKSPACE's current pass CLAIM.
FOCUS must fit the public review record before inference starts. Recover
accepted expiry under journal mutation ownership, verify current evidence,
then retain its pins. Return the restart-readable prepared record.
No coverage is consumed."
  (unless (and (equal (plist-get claim :directory) (niwa-memory-store--claim-directory workspace))
               (stringp focus) (<= (string-bytes focus) 4096)
               (proper-list-p entries) (<= (length entries) 20)
               (niwa-journal-store-id-p (plist-get scope :workspace))
               (equal (plist-get scope :workspace) (niwa-workspace-identity-read (niwa-workspace-root workspace))))
    (error "Invalid memory pass preparation"))
  (niwa-memory-store--assert-owned claim)
  (niwa-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((root (niwa-workspace-root workspace))
            (id (plist-get claim :owner))
            (directory (niwa-memory-store--directory workspace id)))
       (niwa-journal-cleanup-recover (niwa-journal-store-directory root))
       (dolist (entry entries)
         (unless (and (eq (plist-get entry :kind) 'digest)
                      (equal entry (niwa-journal-store-read root (plist-get entry :file))))
           (error "Selected journal evidence changed")))
       (let* ((data (list :id id :claim (niwa-journal-claim--record claim) :scope (cons :decision-format (cons 'slots scope))
                          :entries (mapcar #'niwa-memory-store--entry entries) :focus focus
                          :created (niwa-journal-store-timestamp (- (plist-get claim :expires-at) 180))))
              (text (niwa-memory-store--print data))
              (hash (secure-hash 'sha256 text))
              (path (file-name-concat directory "prepared.el")))
         (niwa-memory-store--assert-owned claim)
         (niwa-memory-store--assert-owned mutation)
         (niwa-session-control-fs-make-directory directory t)
         (unless (niwa-session-control-fs-create-or-verify path text 'utf-8-unix)
           (error "Conflicting prepared memory pass"))
         (dolist (entry entries)
           (niwa-memory-store--assert-owned claim)
           (niwa-memory-store--assert-owned mutation)
           (let ((pin (niwa-memory-store--pin workspace id (plist-get entry :id))))
             (niwa-session-control-fs-make-directory (file-name-directory pin) t)
             (unless (niwa-session-control-fs-create-or-verify pin hash 'utf-8-unix)
               (error "Conflicting memory evidence pin"))))
         (niwa-memory-store-read workspace id))))))

(defun niwa-memory-store-release (workspace id)
  "Release only WORKSPACE pass ID's pins after a fenced unsuccessful outcome.
Repeated release is harmless. Live and accepted passes retain their evidence.
Return the number of pins released."
  (niwa-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((prepared (niwa-memory-store-read workspace id))
            (outcome (niwa-journal-claim-outcome (plist-get prepared :claim))))
       (unless (memq (plist-get outcome :status) '(failed cancelled expired))
         (error "Memory pass still needs its evidence"))
       (niwa-memory-store--release-pins workspace prepared nil mutation)))))

(defun niwa-memory-store--release-pins (workspace prepared keep mutation)
  "Release PREPARED's pins in WORKSPACE except digest IDs in KEEP.
MUTATION holds journal ownership; the caller has proved release eligibility.
Return the number of pins released; already absent pins are not counted."
  (let ((released 0))
    (dolist (entry (plist-get prepared :entries) released)
      (unless (member (plist-get entry :id) keep)
        (let ((pin (niwa-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
          (condition-case nil
              (progn
                (unless (equal (plist-get prepared :hash) (niwa-session-control-fs-read-file pin 'utf-8-unix 65))
                  (error "Memory evidence pin changed"))
                (niwa-memory-store--assert-owned mutation)
                (niwa-session-control-fs-delete-file pin)
                (cl-incf released))
            (niwa-session-control-fs-absent nil)))))))

(defun niwa-memory-store--proposal (scope id proposal)
  "Bind PROPOSAL to pass ID, original SCOPE targets, and complete before-state.
Identity includes the original targets and exact captured before-state."
  (let* ((root (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
         (action (plist-get proposal :action))
         (files (delete-dups
                 (append (list (plist-get proposal :file)) (plist-get proposal :merged-files)
                         (unless (or (eq action 'instructions) (niwa-memory-proposal-note-action-p action))
                           (list "MEMORY.md")))))
         (before (mapcar (lambda (file) (cons file (niwa-memory-scope-before scope (plist-get proposal :root) file)))
                         (sort files #'string<)))
         (identity (list id (plist-get root :dir) (plist-get root :client) (plist-get root :target)
                         proposal before)))
    (append proposal (list :id (secure-hash 'sha256 (niwa-memory-store--print identity)) :before before))))

(defun niwa-memory-store--admitted-notes-p (scope notes)
  "Return non-nil when NOTES uniquely name captured note IDs in SCOPE.
An empty input is valid even when the scope captured notes not supplied to
the review.  Only the producer's admitted roster establishes disposal targets."
  (and (proper-list-p notes)
       (= (length notes) (length (delete-dups (copy-sequence notes))))
       (or (null notes)
           (let ((index (niwa-memory-notes-index scope)))
             (cl-every (lambda (id) (and (stringp id) (assoc id index))) notes)))))

(defun niwa-memory-store-accepted (workspace id &optional batch)
  "Return the fenced accepted bundle for WORKSPACE pass ID, or nil.
Verify its exact bytes against the claim outcome before reading its private
proposal data.  The required :admitted-notes records actual review input IDs,
independently of memory proposal contents.  Return the immutable prepared
record under :prepared and the authenticated accepted bytes' hash under :hash.
BATCH requests bounded pinned reads for this pass alone."
  (let* ((directory (niwa-memory-store--directory workspace id))
         ;; One program probes retirement, then reads both records, in the
         ;; order `niwa-memory-store-read' itself uses.
         (reads (when batch
                  (niwa-session-control-fs-run-program
                   (cons (list :op 'path-exists-p :optional t
                               :path (niwa-journal-cleanup--retired-path (niwa-workspace-root workspace) id))
                         (mapcar (lambda (name)
                                   (list :op 'read :path (file-name-concat directory name)
                                         :optional t :coding 'utf-8-unix :max-bytes (1+ niwa-memory-store--max-bytes)))
                                 '("prepared.el" "accepted.el"))))))
         (retired (car reads))
         (reads (cdr reads))
         (claim (when batch
                  (append (list :directory (niwa-memory-store--claim-directory workspace))
                          (plist-get (car (niwa-memory-store--read-lisp
                                           (file-name-concat directory "prepared.el") (car reads))) :claim))))
         (evidence (when batch
                     (niwa-session-control-fs-run-program
                      (list (list :op 'read :path (niwa-journal-claim--path claim nil))
                            (list :op 'read :path (niwa-journal-claim--path claim t) :optional t)))))
         (prepared (niwa-memory-store-read
                    workspace id (when batch (list :prepared (car reads) :claim (car evidence)
                                                   :retired retired))))
         (outcome (niwa-journal-claim-outcome (plist-get prepared :claim) (cadr evidence))))
    (when (eq (plist-get outcome :status) 'completed)
      (let* ((record (niwa-memory-store--read-lisp
                      (file-name-concat directory "accepted.el") (cadr reads)))
             (text (cdr record)))
        (unless (equal (secure-hash 'sha256 text) (plist-get outcome :payload))
          (error "Accepted memory bundle changed"))
        (let* ((data (car record))
               (review (plist-get data :review)))
          (unless (and (proper-list-p data) (= (length data) 10)
                       (cl-every (lambda (key) (plist-member data key))
                                 '(:prepared-hash :review :proposals :no-action :admitted-notes))
                       (equal (plist-get prepared :hash) (plist-get data :prepared-hash))
                       (niwa-memory-store--admitted-notes-p
                        (plist-get prepared :scope) (plist-get data :admitted-notes))
                       (equal id (plist-get review :pass-id))
                       (equal (plist-get review :workspace) (plist-get (plist-get prepared :scope) :workspace))
                       (equal (plist-get review :proposals)
                              (mapcar (lambda (proposal) (plist-get proposal :id)) (plist-get data :proposals))))
            (error "Invalid accepted memory bundle for pass %s" id))
          (niwa-journal-store--validate-metadata review 'consolidation)
          (append data (list :prepared prepared :hash (plist-get outcome :payload))))))))

(defun niwa-memory-store-accept (workspace prepared reply entries model references
                                              &optional notes)
  "Accept validated REPLY for WORKSPACE's PREPARED pass before public coverage.
ENTRIES are the complete admitted subset; MODEL is its exact provider label and
REFERENCES are bounded dated observations. Retain original target before-state.
NOTES lists only the shared working note IDs actually supplied to the review,
not all captured notes or IDs inferred from model output.  A nil REPLY accepts
a housekeeping-only pass, which admitted no digests or notes for inference.
The immutable claim outcome accepts the bundle hash, allowing later recovery."
  (let ((scope (plist-get prepared :scope)))
    (unless (niwa-memory-store--admitted-notes-p scope notes)
      (error "Admitted note IDs are duplicated or outside the prepared scope"))
    (when (and (null reply) (or entries notes))
      (error "Admitted evidence requires a validated review reply"))
    (niwa-memory-store-accept-proposals
     workspace prepared
     (if reply
         (niwa-memory-proposal-parse
          reply (plist-get scope :roots)
          (append (mapcar (lambda (entry) (plist-get entry :id)) entries) notes))
       (list :proposals nil :no-action "- Shared-folder housekeeping only."))
     entries model references notes)))

(defun niwa-memory-store--note-p (proposal)
  "Return non-nil when PROPOSAL disposes of a shared working file."
  (niwa-memory-proposal-note-action-p (plist-get proposal :action)))

(defun niwa-memory-store-accept-proposals (workspace prepared parsed entries producer references
                                                     &optional admitted-notes)
  "Accept trusted PARSED proposals for WORKSPACE's PREPARED pass.
ENTRIES and REFERENCES retain the same evidence contract as model replies.
PRODUCER is the provider label, or `user' for an explicit cockpit action.
ADMITTED-NOTES is the exact list of note IDs supplied to the model review;
structured user actions and housekeeping supply nil.  Every admitted note gets
an internal deletion proposal after this successful review.  Never infer
admission from the captured roster or model proposals in PARSED.
Callers validate model text before this boundary; direct user actions construct
structured changes from fresh captured snapshots without synthetic model text.
Housekeeping for the scope's captured attachments and empty folders is added
here, after every extraction.  Each note disposal records the extractions
citing its note as :requires, so it cannot apply before them.  Application
must additionally recheck the complete current memory and instruction scope
for remaining citations before deleting a note."
  (let* ((id (plist-get prepared :id))
         (claim (plist-get prepared :claim))
         (scope (plist-get prepared :scope))
         (ids (mapcar (lambda (entry) (plist-get entry :id)) entries)))
    (niwa-memory-store--assert-owned claim)
    (unless (equal prepared (niwa-memory-store-read workspace id))
      (error "Prepared memory evidence changed"))
    (unless (niwa-memory-store--admitted-notes-p scope admitted-notes)
      (error "Admitted note IDs are duplicated or outside the prepared scope"))
    (when (seq-some #'niwa-memory-store--note-p (plist-get parsed :proposals))
      (error "Parsed memory proposals cannot dispose of shared notes"))
    (unless (or entries (null (plist-get prepared :entries)))
      (error "A selected digest batch must admit a complete digest"))
    (dolist (entry entries)
      (let ((original (cl-find (plist-get entry :id) (plist-get prepared :entries)
                               :key (lambda (candidate) (plist-get candidate :id)) :test #'equal)))
        (unless (and original
                     (cl-every (lambda (key) (equal (plist-get entry key) (plist-get original key)))
                               '(:id :file :session :session-name :created :body)))
          (error "Accepted digest was not fully admitted"))
        (unless (equal (plist-get prepared :hash)
                       (niwa-session-control-fs-read-file
                        (niwa-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
          (error "Accepted digest evidence is not retained"))))
    (let* ((proposals
            (let* ((parsed
                    (append
                     (plist-get parsed :proposals)
                     (let ((index (niwa-memory-notes-index scope)))
                       (mapcar
                        (lambda (id)
                          (let ((note (assoc id index)))
                            (list :action 'note-delete :root (cadr note) :file (cddr note) :note id
                                  :type "note" :title (cddr note) :hook ""
                                  :evidence (list id) :body "")))
                        admitted-notes))
                     (niwa-memory-notes-housekeeping scope)))
                   (extractions (mapcar (lambda (proposal) (niwa-memory-store--proposal scope id proposal))
                                        (seq-remove #'niwa-memory-store--note-p parsed))))
              ;; Note disposals depend on every extraction that cites them.
              (append extractions
                      (mapcar (lambda (proposal)
                                (let* ((requires
                                        (when (plist-get proposal :note)
                                          (mapcar (lambda (extraction) (plist-get extraction :id))
                                                  (seq-filter
                                                   (lambda (extraction)
                                                     (member (plist-get proposal :note)
                                                             (plist-get extraction :evidence)))
                                                   extractions)))))
                                  (niwa-memory-store--proposal scope id (append proposal (list :requires requires)))))
                              (seq-filter #'niwa-memory-store--note-p parsed)))))
           (review (list :pass-id id :workspace (plist-get scope :workspace)
                         :created (niwa-journal-store-timestamp
                                   (niwa-session-control-fs-target-time (plist-get claim :directory)))
                         :model producer :focus (plist-get prepared :focus) :digests ids
                         :proposals (mapcar (lambda (proposal) (plist-get proposal :id)) proposals)
                         :references references))
           (_ (niwa-journal-store--validate-metadata review 'consolidation))
           (data (list :prepared-hash (plist-get prepared :hash) :review review
                       :proposals proposals :no-action (plist-get parsed :no-action)
                       :admitted-notes admitted-notes))
           (text (niwa-memory-store--print data))
           (path (file-name-concat (niwa-memory-store--directory workspace id) "accepted.el")))
      (niwa-memory-store--assert-owned claim)
      (unless (niwa-session-control-fs-create-file path text)
        (error "Memory result bundle already exists"))
      (when (niwa-journal-claim-settle claim 'completed (secure-hash 'sha256 text))
        (niwa-memory-store-accepted workspace id)))))

(defun niwa-memory-store--publish-owned (workspace accepted mutation)
  "Publish ACCEPTED's review in WORKSPACE under journal MUTATION ownership.
ACCEPTED must be authenticated under MUTATION.  Verify retained evidence when
the review is absent, publish it and release spare pins.  Pending proposals
keep their admitted evidence; a no-action pass releases it."
  (let* ((prepared (plist-get accepted :prepared))
         (id (plist-get prepared :id))
         (review (plist-get accepted :review))
         (root (niwa-workspace-root workspace))
         (journal (niwa-journal-store-directory root))
         (path (file-name-concat journal (niwa-journal-store--filename review 'consolidation))))
    (niwa-journal-cleanup-recover journal)
    (unless (niwa-session-control-fs-path-exists-p path)
      (dolist (entry (plist-get prepared :entries))
        (when (member (plist-get entry :id) (plist-get review :digests))
          (unless (and (equal (plist-get prepared :hash)
                              (niwa-session-control-fs-read-file
                               (niwa-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
                       (equal (plist-get entry :sha256)
                              (secure-hash 'sha256
                                           (plist-get (niwa-journal-store-read root (plist-get entry :file)) :text))))
            (error "Accepted review evidence is unavailable")))))
    (niwa-memory-store--assert-owned mutation)
    (let ((published (niwa-journal-store-publish-review root review)))
      (setf (niwa-workspace-journal-observation workspace) nil)
      (niwa-memory-store--release-pins workspace prepared
                                         (and (plist-get accepted :proposals) (plist-get review :digests)) mutation)
      (niwa-journal-cleanup-schedule workspace t)
      published)))

(defun niwa-memory-store-publish (workspace id)
  "Recoverably publish WORKSPACE pass ID's accepted review and release spare pins.
The accepted private proposal bundle must already exist. Pending proposals keep
their admitted evidence; a no-action pass releases it after public publication."
  (niwa-memory-store--mutation
   workspace
   (lambda (mutation)
     (niwa-memory-store--publish-owned
      workspace (or (niwa-memory-store-accepted workspace id t)
                    (error "Memory pass has no accepted result"))
      mutation))))

(defun niwa-memory-store-recover (workspace)
  "Recover settled pass publications and evidence releases in WORKSPACE.
Call after acquiring a successor pass claim, so all older attempts are fenced.
Live preparations are left alone. Unreadable accepted state fails closed rather
than letting a new selection silently ignore its unpublished review coverage.
Authenticate every completed bundle, but avoid a publication transaction when
the public bytes already match and no spare evidence pins remain.
Return non-nil when recovery changed committed journal state: finished an
expiry, published, or released evidence pins."
  (let ((published (make-hash-table :test #'equal)) changed)
    ;; Publication used to do this once per completed pass.  Recover accepted
    ;; expiry before observing retirement or taking the no-op publication path.
    (when (> (niwa-memory-store--mutation
              workspace
              (lambda (mutation)
                (niwa-memory-store--assert-owned mutation)
                (niwa-journal-cleanup-recover
                 (niwa-journal-store-directory (niwa-workspace-root workspace)))))
             0)
      (setq changed t))
    (dolist (entry (niwa-journal-store-entries (niwa-workspace-root workspace)))
      (puthash (plist-get entry :file) (plist-get entry :text) published))
    (dolist (directory
             (niwa-session-control-fs-list-directory
              (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory (niwa-workspace-root workspace))) "passes") niwa-journal-store-id-regexp))
      ;; A crash after creating the directory but before preparation retains no
      ;; evidence pins: the immutable prepared file always precedes pin writes.
      (when (and (not (niwa-journal-cleanup-pass-retired-p
                       (niwa-workspace-root workspace) (file-name-nondirectory (directory-file-name directory))))
                 (niwa-session-control-fs-path-exists-p (file-name-concat directory "prepared.el")))
        (let* ((id (file-name-nondirectory (directory-file-name directory)))
               (prepared (niwa-memory-store-read workspace id))
               (outcome (niwa-journal-claim-outcome (plist-get prepared :claim))))
          (pcase (plist-get outcome :status)
            ('completed
             (let* ((accepted (niwa-memory-store-accepted workspace id))
                    (prepared (plist-get accepted :prepared))
                    (review (plist-get accepted :review))
                    (file (niwa-journal-store--filename review 'consolidation))
                    (text (niwa-journal-store--encode review nil 'consolidation))
                    (keep (and (plist-get accepted :proposals) (plist-get review :digests))))
               (when (or (not (equal text (gethash file published)))
                         (seq-some (lambda (entry)
                                     (and (not (member (plist-get entry :id) keep))
                                          (niwa-session-control-fs-path-exists-p
                                           (niwa-memory-store--pin workspace id (plist-get entry :id)))))
                                   (plist-get prepared :entries)))
                 (niwa-memory-store-publish workspace id)
                 (setq changed t))))
            ((or 'failed 'cancelled 'expired)
             (when (> (niwa-memory-store-release workspace id) 0) (setq changed t)))))))
    changed))

(provide 'niwa-memory-store)
;;; niwa-memory-store.el ends here
