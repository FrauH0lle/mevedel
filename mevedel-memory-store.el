;;; mevedel-memory-store.el -- Durable consolidation evidence -*- lexical-binding: t -*-

;;; Commentary:

;; Private immutable pass records retain the original scope and whole evidence.
;; Journal mutation ownership serializes evidence pinning with accepted expiry.
;; Preparing a pass does not publish review coverage or grant mutation authority
;; over its memory roots.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-cleanup)
(require 'mevedel-memory-scope)

(defvar read-circle)
(defvar read-eval)

(defconst mevedel-memory-store--max-bytes (* 4 1024 1024)
  "Maximum private immutable pass record bytes, including escaped snapshots.")

(defun mevedel-memory-store--directory (workspace id)
  "Return WORKSPACE's private pass directory for ID."
  (unless (mevedel-journal-store-id-p id) (error "Invalid memory pass identity"))
  (file-name-concat (mevedel-journal-store-directory (mevedel-workspace-root workspace))
                    "state" "passes" id))

(defun mevedel-memory-store--claim-directory (workspace)
  "Return WORKSPACE's consolidation ownership directory."
  (mevedel-journal-store-claim-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))
                                         'consolidation))

(defun mevedel-memory-store--assert-owned (claim)
  "Reject CLAIM unless it is the current unexpired, unsettled owner."
  (unless (and (equal claim (mevedel-journal-claim-current (plist-get claim :directory)))
               (not (mevedel-journal-claim-outcome claim))
               (< (mevedel-session-control-fs-target-time (plist-get claim :directory))
                  (plist-get claim :expires-at)))
    (error "Memory pass ownership is unavailable")))

(defun mevedel-memory-store--mutation (workspace function)
  "Call FUNCTION with WORKSPACE's journal mutation claim.
FUNCTION rechecks the claim before each mutation; accepted immutable outcomes
remain recoverable if an operation fails after committing its publication."
  (let ((claim (mevedel-journal-claim-acquire
                (mevedel-journal-store-claim-directory
                 (mevedel-journal-store-directory (mevedel-workspace-root workspace)) 'mutation)
                120)))
    (unless claim (error "Journal mutation is busy"))
    (unwind-protect
        (funcall function claim)
      (mevedel-journal-claim-settle claim 'completed ""))))

(defun mevedel-memory-store--read-lisp (path)
  "Read the bounded private Lisp record at PATH as (DATA . TEXT).
DATA is nil when TEXT is oversized, unreadable, or followed by anything but
whitespace; callers validate DATA's shape and signal their own errors."
  (let* ((text (mevedel-session-control-fs-read-file path 'utf-8-unix (1+ mevedel-memory-store--max-bytes)))
         (read-eval nil) (read-circle nil)
         (parsed (and (<= (string-bytes text) mevedel-memory-store--max-bytes)
                      (ignore-errors (read-from-string text)))))
    (cons (and parsed (string-blank-p (substring text (cdr parsed))) (car parsed)) text)))

(defun mevedel-memory-store--print (value)
  "Encode VALUE as bounded plain durable Lisp, preserving literal byte strings."
  (let* ((print-length nil) (print-level nil) (print-circle nil)
         (print-escape-nonascii t) (print-escape-multibyte t)
         (text (prin1-to-string value)))
    (when (> (string-bytes text) mevedel-memory-store--max-bytes)
      (error "Memory pass record exceeds its byte limit"))
    text))

(defun mevedel-memory-store--entry (entry)
  "Freeze the complete body and source fingerprint of validated journal ENTRY."
  (append (cl-loop for key in '(:id :file :session :session-name :created :body)
                   append (list key (plist-get entry key)))
          (list :sha256 (secure-hash 'sha256 (plist-get entry :text)))))

(defun mevedel-memory-store--pin (workspace id digest)
  "Return WORKSPACE's pin path for pass ID and DIGEST identity."
  (unless (and (mevedel-journal-store-id-p id) (mevedel-journal-store-id-p digest))
    (error "Invalid memory evidence identity"))
  (file-name-concat (mevedel-journal-store-directory (mevedel-workspace-root workspace))
                    "state" "evidence-pins" digest (concat id ".pin")))

(defun mevedel-memory-store-read (workspace id)
  "Read WORKSPACE's immutable prepared pass ID, including its private scope.
This internal storage interface grants no right to display original memory
bodies; inspection must use the original authority checks on the returned scope."
  (when (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) id)
    (error "Memory pass has expired"))
  (let* ((record (mevedel-memory-store--read-lisp
                  (file-name-concat (mevedel-memory-store--directory workspace id) "prepared.el")))
         (data (car record)) (text (cdr record))
         (claim (and (proper-list-p data) (plist-get data :claim)))
         (entries (plist-get data :entries)))
    (unless (and (proper-list-p data) (= (length data) 12)
                 (cl-every (lambda (key) (plist-member data key)) '(:id :claim :scope :entries :focus :created))
                 (equal id (plist-get data :id))
                 (equal id (plist-get claim :owner))
                 (equal (plist-get (plist-get data :scope) :workspace)
                        (mevedel-workspace-identity-read (mevedel-workspace-root workspace)))
                 (stringp (plist-get data :focus)) (<= (string-bytes (plist-get data :focus)) 4096)
                 (proper-list-p entries) (<= (length entries) 20)
                 (cl-every (lambda (entry)
                             (and (proper-list-p entry) (= (length entry) 14)
                                  (cl-every (lambda (key) (plist-member entry key))
                                            '(:id :file :session :session-name :created :body :sha256))
                                  (mevedel-journal-store-id-p (plist-get entry :id))
                                  (mevedel-journal-store-id-p (plist-get entry :sha256))
                                  (stringp (plist-get entry :file))
                                  (mevedel-journal-store-file-name-p (plist-get entry :file))
                                  (stringp (plist-get entry :body))
                                  (<= (string-bytes (plist-get entry :body)) mevedel-context-summary--digest-max-bytes))) entries))
      (error "Invalid prepared memory pass"))
    (let ((token (append (list :directory (mevedel-memory-store--claim-directory workspace)) claim)))
      (unless (equal claim (mevedel-journal-claim--read (mevedel-journal-claim--path token nil) nil))
        (error "Prepared pass does not match its ownership record"))
      (plist-put data :claim token))
    (append data (list :hash (secure-hash 'sha256 text)))))

(defun mevedel-memory-store-prepare (workspace claim scope entries focus)
  "Persist SCOPE and complete ENTRIES for WORKSPACE's current pass CLAIM.
FOCUS must fit the public review record before inference starts. Recover
accepted expiry under journal mutation ownership, verify current evidence,
then retain its pins. Return the restart-readable prepared record.
No coverage is consumed."
  (unless (and (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
               (stringp focus) (<= (string-bytes focus) 4096)
               (proper-list-p entries) (<= (length entries) 20)
               (mevedel-journal-store-id-p (plist-get scope :workspace))
               (equal (plist-get scope :workspace) (mevedel-workspace-identity-read (mevedel-workspace-root workspace))))
    (error "Invalid memory pass preparation"))
  (mevedel-memory-store--assert-owned claim)
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((root (mevedel-workspace-root workspace))
            (id (plist-get claim :owner))
            (directory (mevedel-memory-store--directory workspace id)))
       (mevedel-journal-cleanup-recover (mevedel-journal-store-directory root))
       (dolist (entry entries)
         (unless (and (eq (plist-get entry :kind) 'digest)
                      (equal entry (mevedel-journal-store-read root (plist-get entry :file))))
           (error "Selected journal evidence changed")))
       (let* ((data (list :id id :claim (mevedel-journal-claim--record claim) :scope scope
                          :entries (mapcar #'mevedel-memory-store--entry entries) :focus focus
                          :created (mevedel-journal-store-timestamp (- (plist-get claim :expires-at) 180))))
              (text (mevedel-memory-store--print data))
              (hash (secure-hash 'sha256 text))
              (path (file-name-concat directory "prepared.el")))
         (mevedel-memory-store--assert-owned claim)
         (mevedel-memory-store--assert-owned mutation)
         (mevedel-session-control-fs-make-directory directory t)
         (unless (mevedel-session-control-fs-create-or-verify path text 'utf-8-unix)
           (error "Conflicting prepared memory pass"))
         (dolist (entry entries)
           (mevedel-memory-store--assert-owned claim)
           (mevedel-memory-store--assert-owned mutation)
           (let ((pin (mevedel-memory-store--pin workspace id (plist-get entry :id))))
             (mevedel-session-control-fs-make-directory (file-name-directory pin) t)
             (unless (mevedel-session-control-fs-create-or-verify pin hash 'utf-8-unix)
               (error "Conflicting memory evidence pin"))))
         (mevedel-memory-store-read workspace id))))))

(defun mevedel-memory-store-release (workspace id)
  "Release only WORKSPACE pass ID's pins after a fenced unsuccessful outcome.
Repeated release is harmless. Live and accepted passes retain their evidence."
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((prepared (mevedel-memory-store-read workspace id))
            (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
       (unless (memq (plist-get outcome :status) '(failed cancelled expired))
         (error "Memory pass still needs its evidence"))
       (mevedel-memory-store--release-pins workspace prepared nil mutation)))))

(defun mevedel-memory-store--release-pins (workspace prepared keep mutation)
  "Release PREPARED's pins in WORKSPACE except digest IDs in KEEP.
MUTATION holds journal ownership; the caller has proved release eligibility."
  (dolist (entry (plist-get prepared :entries))
    (unless (member (plist-get entry :id) keep)
      (let ((pin (mevedel-memory-store--pin workspace (plist-get prepared :id) (plist-get entry :id))))
        (condition-case nil
            (progn
              (unless (equal (plist-get prepared :hash) (mevedel-session-control-fs-read-file pin 'utf-8-unix 65))
                (error "Memory evidence pin changed"))
              (mevedel-memory-store--assert-owned mutation)
              (mevedel-session-control-fs-delete-file pin))
          (mevedel-session-control-fs-absent nil))))))

(defun mevedel-memory-store--proposal (scope id proposal)
  "Bind PROPOSAL to pass ID, original SCOPE targets, and complete before-state."
  (let* ((root (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
         (files (delete-dups
                 (append (list (plist-get proposal :file)) (plist-get proposal :merged-files)
                         (unless (eq (plist-get proposal :action) 'instructions) (list "MEMORY.md")))))
         (before (mapcar (lambda (file) (cons file (mevedel-memory-scope-before scope (plist-get proposal :root) file)))
                         (sort files #'string<)))
         (identity (list id (plist-get root :dir) (plist-get root :client) (plist-get root :target)
                         proposal before)))
    (append proposal (list :id (secure-hash 'sha256 (mevedel-memory-store--print identity)) :before before))))

(defun mevedel-memory-store-accepted (workspace id)
  "Return the fenced accepted bundle for WORKSPACE pass ID, or nil.
Verify its exact bytes against the claim outcome before reading its private
proposal data. Return the immutable prepared record under :prepared and the
authenticated accepted bytes' hash under :hash."
  (let* ((prepared (mevedel-memory-store-read workspace id))
         (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
    (when (eq (plist-get outcome :status) 'completed)
      (let* ((record (mevedel-memory-store--read-lisp
                      (file-name-concat (mevedel-memory-store--directory workspace id) "accepted.el")))
             (text (cdr record)))
        (unless (equal (secure-hash 'sha256 text) (plist-get outcome :payload))
          (error "Accepted memory bundle changed"))
        (let* ((data (car record))
               (review (plist-get data :review)))
          (unless (and (proper-list-p data) (= (length data) 8)
                       (cl-every (lambda (key) (plist-member data key)) '(:prepared-hash :review :proposals :no-action))
                       (equal (plist-get prepared :hash) (plist-get data :prepared-hash))
                       (equal id (plist-get review :pass-id))
                       (equal (plist-get review :workspace) (plist-get (plist-get prepared :scope) :workspace))
                       (equal (plist-get review :proposals)
                              (mapcar (lambda (proposal) (plist-get proposal :id)) (plist-get data :proposals))))
            (error "Invalid accepted memory bundle"))
          (mevedel-journal-store--validate-metadata review 'consolidation)
          (append data (list :prepared prepared :hash (plist-get outcome :payload))))))))

(defun mevedel-memory-store-accept (workspace prepared reply entries model references)
  "Accept validated REPLY for WORKSPACE's PREPARED pass before public coverage.
ENTRIES are the complete admitted subset; MODEL is its exact provider label and
REFERENCES are bounded dated observations. Retain original target before-state.
The immutable claim outcome accepts the bundle hash, allowing later recovery."
  (let* ((id (plist-get prepared :id))
         (claim (plist-get prepared :claim))
         (scope (plist-get prepared :scope))
         (ids (mapcar (lambda (entry) (plist-get entry :id)) entries))
         (parsed (mevedel-memory-proposal-parse reply (plist-get scope :roots) ids)))
    (mevedel-memory-store--assert-owned claim)
    (unless (equal prepared (mevedel-memory-store-read workspace id))
      (error "Prepared memory evidence changed"))
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
                       (mevedel-session-control-fs-read-file
                        (mevedel-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
          (error "Accepted digest evidence is not retained"))))
    (let* ((proposals (mapcar (lambda (proposal) (mevedel-memory-store--proposal scope id proposal))
                              (plist-get parsed :proposals)))
           (review (list :pass-id id :workspace (plist-get scope :workspace)
                         :created (mevedel-journal-store-timestamp
                                   (mevedel-session-control-fs-target-time (plist-get claim :directory)))
                         :model model :focus (plist-get prepared :focus) :digests ids
                         :proposals (mapcar (lambda (proposal) (plist-get proposal :id)) proposals)
                         :references references))
           (_ (mevedel-journal-store--validate-metadata review 'consolidation))
           (data (list :prepared-hash (plist-get prepared :hash) :review review
                       :proposals proposals :no-action (plist-get parsed :no-action)))
           (text (mevedel-memory-store--print data))
           (path (file-name-concat (mevedel-memory-store--directory workspace id) "accepted.el")))
      (mevedel-memory-store--assert-owned claim)
      (unless (mevedel-session-control-fs-create-file path text)
        (error "Memory result bundle already exists"))
      (when (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text))
        (mevedel-memory-store-accepted workspace id)))))

(defun mevedel-memory-store-publish (workspace id)
  "Recoverably publish WORKSPACE pass ID's accepted review and release spare pins.
The accepted private proposal bundle must already exist. Pending proposals keep
their admitted evidence; a no-action pass releases it after public publication."
  (mevedel-memory-store--mutation
   workspace
   (lambda (mutation)
     (let* ((accepted (or (mevedel-memory-store-accepted workspace id)
                          (error "Memory pass has no accepted result")))
            (prepared (plist-get accepted :prepared))
            (review (plist-get accepted :review))
            (root (mevedel-workspace-root workspace))
            (journal (mevedel-journal-store-directory root))
            (path (file-name-concat journal (mevedel-journal-store--filename review 'consolidation))))
       (mevedel-journal-cleanup-recover journal)
       (unless (mevedel-session-control-fs-path-exists-p path)
         (dolist (entry (plist-get prepared :entries))
           (when (member (plist-get entry :id) (plist-get review :digests))
             (unless (and (equal (plist-get prepared :hash)
                                 (mevedel-session-control-fs-read-file
                                  (mevedel-memory-store--pin workspace id (plist-get entry :id)) 'utf-8-unix 65))
                          (equal (plist-get entry :sha256)
                                 (secure-hash 'sha256
                                              (plist-get (mevedel-journal-store-read root (plist-get entry :file)) :text))))
               (error "Accepted review evidence is unavailable")))))
       (mevedel-memory-store--assert-owned mutation)
       (let ((published (mevedel-journal-store-publish-review root review)))
         (setf (mevedel-workspace-journal-observation workspace) nil)
         (mevedel-memory-store--release-pins workspace prepared
                                            (and (plist-get accepted :proposals) (plist-get review :digests)) mutation)
         published)))))

(defun mevedel-memory-store-recover (workspace)
  "Recover settled pass publications and evidence releases in WORKSPACE.
Call after acquiring a successor pass claim, so all older attempts are fenced.
Live preparations are left alone. Unreadable accepted state fails closed rather
than letting a new selection silently ignore its unpublished review coverage."
  (dolist (directory
           (mevedel-session-control-fs-list-directory
            (file-name-concat (mevedel-journal-store-directory (mevedel-workspace-root workspace))
                              "state" "passes") mevedel-journal-store-id-regexp))
    ;; A crash after creating the directory but before preparation retains no
    ;; evidence pins: the immutable prepared file always precedes pin writes.
    (when (and (not (mevedel-journal-cleanup-pass-retired-p
                     (mevedel-workspace-root workspace) (file-name-nondirectory (directory-file-name directory))))
               (mevedel-session-control-fs-path-exists-p (file-name-concat directory "prepared.el")))
      (let* ((id (file-name-nondirectory (directory-file-name directory)))
             (prepared (mevedel-memory-store-read workspace id))
             (outcome (mevedel-journal-claim-outcome (plist-get prepared :claim))))
        (pcase (plist-get outcome :status)
          ('completed (mevedel-memory-store-publish workspace id))
          ((or 'failed 'cancelled 'expired) (mevedel-memory-store-release workspace id)))))))

(provide 'mevedel-memory-store)
;;; mevedel-memory-store.el ends here
