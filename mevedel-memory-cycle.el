;;; mevedel-memory-cycle.el -- Durable finite shared-note offers -*- lexical-binding: t -*-

;;; Commentary:

;; A daily offer freezes metadata, not bodies or mutation authority.  Every
;; bounded pass uses the original consolidation claim.  Acceptance settles that
;; claim, so only a fresh owner reconciles authenticated admission receipts.
;; One inflight slot pins that proof until reconciliation survives a restart.

;;; Code:

(require 'cl-lib)
(require 'mevedel-memory-store)

(defvar read-circle)
(defvar read-eval)

;; `mevedel-memory-notes'
(declare-function mevedel-memory-notes-directory "mevedel-memory-notes" (workspace))
(declare-function mevedel-memory-notes-root "mevedel-memory-notes" (scope))
(autoload 'mevedel-memory-notes-directory "mevedel-memory-notes")
(autoload 'mevedel-memory-notes-root "mevedel-memory-notes")

(defconst mevedel-memory-cycle--max-bytes (* 4 1024 1024)
  "Maximum serialized metadata bytes in one finite note cycle.")

(defun mevedel-memory-cycle--path (workspace)
  "Return WORKSPACE's private cycle record path."
  (file-name-concat (mevedel-workspace-root workspace)
                    ".mevedel" "state" "shared-notes" "cycle.el"))

(defun mevedel-memory-cycle--shape-p (value keys)
  "Return non-nil when VALUE is a proper plist with exactly KEYS."
  (and (proper-list-p value) (= (length value) (* 2 (length keys)))
       (cl-every (lambda (key) (plist-member value key)) keys)))

(defun mevedel-memory-cycle--member-p (member)
  "Return non-nil for one bounded metadata-only cohort MEMBER."
  (let ((file (plist-get member :file)) (version (plist-get member :version)))
    (and (mevedel-memory-cycle--shape-p member '(:file :kind :version))
         (stringp file) (< 0 (length file) 4096)
         (not (file-name-absolute-p file))
         (not (member ".." (split-string file "/")))
         (memq (plist-get member :kind) '(text attachment directory))
         (mevedel-memory-cycle--shape-p
          version '(:type :mtime :ctime :mode :size :device :inode :hash :empty))
         (memq (plist-get version :type) '(file directory))
         (stringp (plist-get version :mode))
         (natnump (plist-get version :size))
         (or (null (plist-get version :hash))
             (mevedel-journal-store-id-p (plist-get version :hash)))
         ;; Raw times and device/inode spellings are platform data.  They must
         ;; remain literal bounded numbers/conses, never bodies or executable data.
         (cl-labels ((number-tree-p (value)
                       (or (null value) (integerp value)
                           (and (consp value) (number-tree-p (car value))
                                (number-tree-p (cdr value))))))
           (cl-every (lambda (key) (number-tree-p (plist-get version key)))
                     '(:mtime :ctime :device :inode)))
         (memq (plist-get version :empty) '(nil t)))))

(defun mevedel-memory-cycle--print (record)
  "Serialize metadata RECORD within the cycle owner's independent bound."
  (let* ((print-length nil) (print-level nil) (print-circle nil)
         (print-escape-nonascii t) (print-escape-multibyte t)
         (text (prin1-to-string record)))
    (when (> (string-bytes text) mevedel-memory-cycle--max-bytes)
      (error "Shared-note cycle exceeds its metadata byte limit"))
    text))

(defun mevedel-memory-cycle--hash (members)
  "Return the stable scheduling hash of ordered MEMBERS."
  (secure-hash 'sha256 (mevedel-memory-cycle--print members)))

(defun mevedel-memory-cycle--valid-p (record)
  "Return non-nil for the exact bounded scheduling RECORD schema."
  (let ((members (plist-get record :members))
        (attempted (plist-get record :attempted))
        (deferred (plist-get record :deferred))
        (inflight (plist-get record :inflight)))
    (and (mevedel-memory-cycle--shape-p
          record '(:workspace :dir :target :id :admitted-at :cohort-hash
                   :members :attempted :deferred :inflight :status :error))
         (cl-every (lambda (key) (mevedel-journal-store-id-p (plist-get record key)))
                   '(:workspace :id :cohort-hash))
         (let ((target (plist-get record :target)))
           (and (proper-list-p target) (<= 2 (length target) 8)
                (zerop (% (length target) 2))
                (let ((method (plist-get target :method)))
                  (or (eq method 'local)
                      (and (stringp method) (< 0 (length method) 4096))))
                (cl-loop for (key value) on target by #'cddr
                         always (and (memq key '(:method :user :host :hop))
                                     (or (and (eq key :method) (symbolp value))
                                         (and (stringp value) (< (length value) 4096)))))))
         (stringp (plist-get record :dir)) (file-name-absolute-p (plist-get record :dir))
         (integerp (plist-get record :admitted-at))
         (memq (plist-get record :status) '(active complete paused))
         (memq (plist-get record :error) '(nil failed cancelled expired application interrupted))
         (proper-list-p members) (<= (length members) 4096)
         (cl-every #'mevedel-memory-cycle--member-p members)
         (= (length members) (length (delete-dups (mapcar (lambda (row) (plist-get row :file)) members))))
         (equal (plist-get record :cohort-hash) (mevedel-memory-cycle--hash members))
         (proper-list-p attempted) (proper-list-p deferred)
         (<= (+ (length attempted) (length deferred)) (length members))
         (cl-every (lambda (row)
                     (and (mevedel-memory-cycle--shape-p row '(:member :pass))
                          (member (plist-get row :member) members)
                          (mevedel-journal-store-id-p (plist-get row :pass)))) attempted)
         (cl-every (lambda (row)
                     (and (mevedel-memory-cycle--shape-p row '(:member :reason))
                          (member (plist-get row :member) members)
                          (memq (plist-get row :reason)
                                '(unavailable-or-changed changed missing in-use unsaved
                                  unreadable not-admitted held)))) deferred)
         (= (+ (length attempted) (length deferred))
            (length (delete-dups (mapcar (lambda (row) (plist-get row :member))
                                         (append attempted deferred)))))
         (or (null inflight)
             (and (mevedel-memory-cycle--shape-p inflight '(:pass :claim))
                  (mevedel-journal-store-id-p (plist-get inflight :pass))
                  (let ((claim (plist-get inflight :claim)))
                    (and (mevedel-memory-cycle--shape-p claim '(:directory :generation :owner :expires-at))
                         (stringp (plist-get claim :directory))
                         (integerp (plist-get claim :generation))
                         (> (plist-get claim :generation) 0)
                         (integerp (plist-get claim :expires-at))
                         (> (plist-get claim :expires-at) 0)
                         (equal (plist-get inflight :pass) (plist-get claim :owner)))))))))

(defun mevedel-memory-cycle--read (workspace)
  "Return WORKSPACE's (RECORD . EXACT-TEXT), or nil when absent.
Malformed, oversized and unreadable state fails closed."
  (condition-case nil
      (let* ((text (mevedel-session-control-fs-read-file
                    (mevedel-memory-cycle--path workspace) 'utf-8-unix
                    (1+ mevedel-memory-cycle--max-bytes)))
             (read-eval nil) (read-circle nil)
             (parsed (and (<= (string-bytes text) mevedel-memory-cycle--max-bytes)
                          (ignore-errors (read-from-string text))))
             (record (car parsed)))
        (unless (and parsed (string-blank-p (substring text (cdr parsed)))
                     (mevedel-memory-cycle--valid-p record)
                     (equal (plist-get record :workspace)
                            (mevedel-workspace-identity-read (mevedel-workspace-root workspace))))
          (error "Invalid shared-note cycle record"))
        (cons record text))
    (mevedel-session-control-fs-absent nil)))

(defun mevedel-memory-cycle--write (workspace claim previous record)
  "CAS WORKSPACE's PREVIOUS observation to RECORD under fresh original CLAIM."
  (unless (and (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
               (mevedel-memory-cycle--valid-p record))
    (error "Invalid shared-note cycle mutation"))
  ;; Capture yields after reservation.  Neither terminal staging nor a new
  ;; envelope may silently replace active work under another actual namespace.
  (when (or (eq (plist-get (car previous) :status) 'active)
            (plist-get (car previous) :inflight))
    (mevedel-memory-cycle--assert-namespace workspace (car previous)))
  (mevedel-memory-cycle--assert-namespace workspace record)
  (let* ((path (mevedel-memory-cycle--path workspace))
         (text (mevedel-memory-cycle--print record)))
    (mevedel-session-control-fs-make-directory (file-name-directory path) t)
    (mapc #'mevedel-session-control-fs-program-value
          (mevedel-session-control-fs-run-program
           (list (list :op 'verify-latest :path (mevedel-journal-claim--path claim nil) :content ".claim")
                 (list :op 'verify :path (mevedel-journal-claim--path claim nil)
                       :content (json-serialize (mevedel-journal-claim--record claim)))
                 (list :op 'absent :path (mevedel-journal-claim--path claim t))
                 (list :op 'before-time :path (plist-get claim :directory)
                       :content (number-to-string (plist-get claim :expires-at)))
                 (if previous (list :op 'verify :path path :content (cdr previous) :coding 'utf-8-unix)
                   (list :op 'absent :path path))
                 (list :op 'write :path path :content text :coding 'utf-8-unix))
           (plist-get claim :directory)))
    record))

(defun mevedel-memory-cycle-pending (cycle)
  "Return CYCLE's remaining exact members in their frozen order."
  (let ((done (make-hash-table :test #'equal)))
    (dolist (row (append (plist-get cycle :attempted) (plist-get cycle :deferred)))
      (puthash (plist-get row :member) t done))
    (seq-remove (lambda (member) (gethash member done)) (plist-get cycle :members))))

(defun mevedel-memory-cycle-cohort (cycle)
  "Return CYCLE's closed pending capture envelope, including an empty cohort."
  (list :workspace (plist-get cycle :workspace) :dir (plist-get cycle :dir)
        :target (plist-get cycle :target) :members (mevedel-memory-cycle-pending cycle)))

(defun mevedel-memory-cycle-summary (cycle)
  "Return body-free scalar telemetry and result fields for observed CYCLE."
  (when cycle
    (list :note-cycle-id (plist-get cycle :id)
          :note-cycle-count (length (plist-get cycle :members))
          :note-cycle-attempted (length (plist-get cycle :attempted))
          :note-cycle-deferred (length (plist-get cycle :deferred))
          :note-cycle-remaining (length (mevedel-memory-cycle-pending cycle))
          :note-cycle-status (plist-get cycle :status))))

(defun mevedel-memory-cycle-pins-pass-p (workspace pass)
  "Return non-nil while WORKSPACE's unreconciled cycle pins PASS.
Malformed or unreadable scheduling state signals, preventing unsafe cleanup."
  (when-let* ((inflight (plist-get (car (mevedel-memory-cycle--read workspace)) :inflight)))
    (equal pass (plist-get inflight :pass))))

(defun mevedel-memory-cycle-inflight-claim (workspace)
  "Return WORKSPACE's strict unreconciled original claim for retention, or nil.
This observation pins proof only.  It grants no admission or write authority."
  (when-let* ((inflight (plist-get (car (mevedel-memory-cycle--read workspace)) :inflight)))
    (let ((claim (plist-get inflight :claim)))
      (unless (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
        (error "Shared-note cycle claim addresses another workspace"))
      claim)))

(defun mevedel-memory-cycle--assert-namespace (workspace cycle)
  "Reject CYCLE unless WORKSPACE still names its exact actual directory/target.
Do not infer equivalence from client names, aliases or shared inode guesses.
Unreconciled work needs its original authority before another namespace can
replace the single record."
  (let* ((configured (mevedel-memory-notes-directory workspace))
         (directory (and configured (file-name-as-directory (file-truename configured))))
         (target (and directory (mevedel-execution-target-identity
                                 (mevedel-execution-target-create directory)))))
    (unless (and (equal directory (plist-get cycle :dir))
                 (equal target (plist-get cycle :target)))
      (error "Shared-note cycle namespace changed; original authority is required"))))

(defun mevedel-memory-cycle-recover (workspace claim)
  "Reconcile WORKSPACE's accepted predecessor under fresh owned CLAIM.
Call before cleanup-capable recovery.  Never credit proposals, acknowledgements
or an unaccepted request.  An interrupted unaccepted batch pauses its cohort."
  (mevedel-memory-store--assert-owned claim)
  (let* ((previous (mevedel-memory-cycle--read workspace))
         (cycle (copy-tree (car previous)))
         (inflight (plist-get cycle :inflight)))
    (when (or inflight (eq (plist-get cycle :status) 'active))
      (mevedel-memory-cycle--assert-namespace workspace cycle))
    (when inflight
      (let* ((old-claim (plist-get inflight :claim))
             (pass (plist-get inflight :pass))
             outcome)
        ;; Validate the already-read predecessor before addressing its outcome.
        ;; Calling the public getter here would reread mutable cycle state.
        (unless (and (equal (plist-get claim :directory) (mevedel-memory-store--claim-directory workspace))
                     (equal (plist-get old-claim :directory) (plist-get claim :directory))
                     (< (plist-get old-claim :generation) (plist-get claim :generation)))
          (error "Shared-note cycle predecessor is not fenced"))
        (setq outcome (mevedel-journal-claim-outcome old-claim))
        (unless outcome (error "Shared-note cycle predecessor is not fenced"))
        (if (eq (plist-get outcome :status) 'completed)
            (let* ((accepted (mevedel-memory-store-accepted workspace pass))
                   (prepared (plist-get accepted :prepared))
                   (scope (plist-get prepared :scope))
                   (link (plist-get scope :note-cycle))
                   (root (cdr (mevedel-memory-notes-root scope))))
              (unless (and accepted (equal old-claim (plist-get prepared :claim))
                           (equal link (list :id (plist-get cycle :id)
                                             :cohort-hash (plist-get cycle :cohort-hash)))
                           (equal (plist-get cycle :dir) (plist-get root :dir))
                           (equal (plist-get cycle :target) (plist-get root :target)))
                (error "Accepted pass does not authenticate its note cycle"))
              (dolist (row (plist-get root :before))
                (let* ((member (cl-find (car row) (mevedel-memory-cycle-pending cycle)
                                        :key (lambda (member) (plist-get member :file)) :test #'equal))
                       (note (cdr (assoc (car row) (plist-get root :notes))))
                       (admitted (or (not note) (member (plist-get note :id) (plist-get accepted :admitted-notes)))))
                  (unless (and member (equal (plist-get member :version) (plist-get (cdr row) :cohort-version)))
                    (error "Accepted pass contains a non-cohort version"))
                  (if admitted
                      (push (list :member member :pass pass) (plist-get cycle :attempted))
                    (push (list :member member :reason 'not-admitted) (plist-get cycle :deferred)))))
              (unless (mevedel-memory-cycle-pending cycle) (plist-put cycle :status 'complete)))
          (plist-put cycle :status 'paused)
          (plist-put cycle :error (if (memq (plist-get outcome :status) '(failed cancelled expired))
                                     (plist-get outcome :status) 'interrupted)))
        (plist-put cycle :inflight nil)
        (mevedel-memory-cycle--write workspace claim previous cycle)))
    cycle))

(defun mevedel-memory-cycle--reserve (workspace claim previous cycle)
  "Persist CYCLE in WORKSPACE, reserving active CLAIM against PREVIOUS.
Reservation precedes yielding capture, not just immutable preparation, so an
interrupted or failed capture has a durable predecessor to fence and pause."
  (setq cycle (copy-tree cycle))
  (when (mevedel-memory-cycle-pending cycle)
    (plist-put cycle :inflight (list :pass (plist-get claim :owner) :claim claim)))
  (mevedel-memory-cycle--write workspace claim previous cycle))

(defun mevedel-memory-cycle-admit (workspace claim cohort-function &optional force)
  "Admit or resume WORKSPACE's finite cohort under CLAIM.
COHORT-FUNCTION freezes the entire eligible metadata inventory only when due.
FORCE bypasses daily time and resumes paused work, never replaces active work.
Return nil when not due or paused.  Active admission reserves CLAIM before
capture can fail or yield; empty cohorts persist complete without IO
to a provider.  Target-clock equality at 86400 seconds is due."
  (mevedel-memory-store--assert-owned claim)
  (let* ((previous (mevedel-memory-cycle--read workspace))
         (cycle (car previous))
         (now (mevedel-session-control-fs-target-time (plist-get claim :directory))))
    (when (plist-get cycle :inflight) (error "Reconcile shared-note cycle before admission"))
    (cond
     ((eq (plist-get cycle :status) 'active)
      (mevedel-memory-cycle--reserve workspace claim previous cycle))
     ((and (eq (plist-get cycle :status) 'paused) force)
      (setq cycle (copy-tree cycle))
      (plist-put cycle :status (if (mevedel-memory-cycle-pending cycle) 'active 'complete))
      (plist-put cycle :error nil)
      (mevedel-memory-cycle--reserve workspace claim previous cycle))
     ((and (eq (plist-get cycle :status) 'paused)
           (< now (+ (plist-get cycle :admitted-at) 86400))) nil)
     ((or force (null cycle) (>= now (+ (plist-get cycle :admitted-at) 86400)))
      (when-let* ((cohort (funcall cohort-function)))
        (unless (and (eq (plist-get cohort :complete) t)
                     (equal (plist-get cohort :workspace)
                            (mevedel-workspace-identity-read (mevedel-workspace-root workspace))))
          (error "Shared-note cohort inventory is incomplete"))
        (setq cycle (list :workspace (plist-get cohort :workspace) :dir (plist-get cohort :dir)
                          :target (plist-get cohort :target) :id (plist-get claim :owner)
                          :admitted-at now :cohort-hash (mevedel-memory-cycle--hash (plist-get cohort :members))
                          :members (plist-get cohort :members) :attempted nil :deferred nil :inflight nil
                          :status (if (plist-get cohort :members) 'active 'complete) :error nil))
        (mevedel-memory-cycle--reserve workspace claim previous cycle)))
     (t nil))))

(cl-defun mevedel-memory-cycle-stage (workspace claim cycle &key capture stop)
  "Persist CAPTURE's deferred outcomes or STOP for WORKSPACE CYCLE under CLAIM.
A nonempty capture retains this admission's reserved inflight pass.
STOP is a bounded error classification; it pauses without admitting inference."
  (let* ((previous (mevedel-memory-cycle--read workspace))
         (fresh (car previous)))
    (unless (equal cycle fresh) (error "Shared-note cycle changed before staging"))
    (setq cycle (copy-tree cycle))
    (when (and (plist-get cycle :inflight)
               (not (equal claim (plist-get (plist-get cycle :inflight) :claim))))
      (error "Shared-note cycle has another inflight pass"))
    (dolist (row (plist-get capture :deferred))
      (unless (member (plist-get row :member) (mevedel-memory-cycle-pending cycle))
        (error "Deferred note is outside the remaining cohort"))
      (push row (plist-get cycle :deferred)))
    (cond
     (stop (plist-put cycle :status 'paused) (plist-put cycle :error stop)
           (plist-put cycle :inflight nil))
     ((plist-get capture :root)
      (plist-put cycle :inflight (list :pass (plist-get claim :owner) :claim claim)))
     ((null (mevedel-memory-cycle-pending cycle))
      (plist-put cycle :status 'complete) (plist-put cycle :inflight nil)))
    (mevedel-memory-cycle--write workspace claim previous cycle)))

(defun mevedel-memory-cycle-pause (workspace reason &optional expected-id)
  "Pause WORKSPACE's remaining note work for bounded REASON with fresh ownership.
EXPECTED-ID, when non-nil, prevents pausing a replacement cycle.
Acceptance has settled the old token; this metadata-only admission must precede
any further request.  Signal on contention rather than losing the stop."
  (let ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
    (unless claim (error "Shared-note cycle pause needs fresh ownership"))
    (unwind-protect
        (when-let* ((cycle (mevedel-memory-cycle-recover workspace claim)))
          (when (and expected-id (not (equal expected-id (plist-get cycle :id))))
            (error "Shared-note cycle changed before pause"))
          (mevedel-memory-cycle-stage workspace claim cycle :stop reason))
      (mevedel-journal-claim-settle claim 'completed ""))))

(provide 'mevedel-memory-cycle)
;;; mevedel-memory-cycle.el ends here
