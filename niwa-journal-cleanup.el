;;; niwa-journal-cleanup.el -- Recoverable workspace journal expiry -*- lexical-binding: t -*-

;;; Commentary:

;; Physical expiry is a bounded journal mutation, separate from ordinary recall
;; age limits.  Unreviewed digests and unresolved evidence stay recoverable.
;; An immutable manifest is accepted before storage is retired or removed.
;; Accepted manifests are replayed before selecting further evidence.  Private
;; turn coverage survives, while retired capture payloads may be collected.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'niwa-journal-claim)
(require 'niwa-journal-index)
(require 'niwa-journal-store)
(require 'niwa-structs)
(require 'niwa-journal-worker)

(autoload 'niwa-journal-gc "niwa-journal-gc")

;; `niwa-memory-cleanup'
(declare-function niwa-memory-cleanup-entry "niwa-memory-cleanup" (workspace entry entries))
(autoload 'niwa-memory-cleanup-entry "niwa-memory-cleanup")

;; `niwa-memory-decision'
(declare-function niwa-memory-decision-recovering-p "niwa-memory-decision" (workspace))
(declare-function niwa-memory-decision-admitted-p "niwa-memory-decision" (key))

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-drain-status "niwa-memory-pass" (workspace))
(declare-function niwa-memory-pass-running "niwa-memory-pass" (workspace))

;; `niwa-state-cleanup'
(declare-function niwa-state-cleanup "niwa-state-cleanup" (workspace))
(autoload 'niwa-state-cleanup "niwa-state-cleanup")

;; `niwa-transport'
(declare-function niwa-transport-run-when-idle "niwa-transport"
                  (key path thunk &optional on-cancel))

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")

(defconst niwa-journal-cleanup--max-bytes (* 4 1024 1024)
  "Maximum encoded expiry manifest, including dependent file identities.")

(defvar niwa-journal-cleanup--finished nil
  "Non-nil once `niwa-journal-cleanup--apply' finishes a manifest now.
`niwa-journal-cleanup-recover' binds it per manifest.")

(defun niwa-journal-cleanup--memory-entry-p (entry)
  "Validate ENTRY's closed memory dependency paths and identity hashes."
  (let* ((id (gethash "id" entry))
         (related (gethash "related" entry))
         (private (gethash "private" entry)))
    (and (member (gethash "scope" entry) '("general" "focused"))
         (memq (gethash "journal-bearing" entry) '(t :false))
         (vectorp related)
         (cl-every
          (lambda (record)
            (and (hash-table-p record) (= 3 (hash-table-count record))
                 (niwa-journal-store-decision-id-p (gethash "id" record))
                 (niwa-journal-store-id-p (gethash "sha256" record))
                 (niwa-journal-store-file-name-p (gethash "file" record))
                 (string-suffix-p (concat "-" (niwa-journal-store--record-id (list :decision-id (gethash "id" record)) 'decision) ".md")
                                  (gethash "file" record)))) related)
         (cl-every (lambda (record)
                     (and (hash-table-p record) (= 2 (hash-table-count record))
                          (stringp (gethash "file" record))
                          (niwa-journal-store-id-p (gethash "sha256" record)))) private)
         (let* ((paths (mapcar (lambda (record) (gethash "file" record)) private))
                ;; Only replay of an existing expiry may consume the old grammar.
                (old (seq-some (lambda (path) (string-match-p "\\`\\(?:decisions\\|writes\\)/" path)) paths))
                (required (append (list (format "passes/%s/accepted.el" id) (format "passes/%s/prepared.el" id))
                                  (mapcar (lambda (record) (format "%s/%s.el" (if old "decisions" "decision-slots")
                                                                  (gethash "id" record))) related))))
           (and (= (length paths) (length (delete-dups (copy-sequence paths))))
                (or (not old) (cl-every (lambda (r) (niwa-journal-store-id-p (gethash "id" r))) related))
                (cl-every (lambda (path) (member path paths)) required)
                (cl-every (lambda (path)
                            (or (member path required)
                                (and old (string-match-p (concat "\\`writes/" niwa-journal-store-hash-regexp "\\.el\\'") path)))) paths))))))

(defun niwa-journal-cleanup--read (root path)
  "Read a bounded immutable expiry manifest at PATH under journal ROOT."
  (let* ((text (niwa-session-control-fs-read-file path 'utf-8-unix (1+ niwa-journal-cleanup--max-bytes)))
         (object (condition-case err (json-parse-string text)
                   (json-error (error "Invalid journal expiry manifest %s: %s" path (error-message-string err)))))
         (generation (and (hash-table-p object) (gethash "generation" object)))
         (owner (and (hash-table-p object) (gethash "owner" object)))
         (expires (and (hash-table-p object) (gethash "expires-at" object)))
         (entries (and (hash-table-p object) (gethash "entries" object))))
    (unless (and (<= (string-bytes text) niwa-journal-cleanup--max-bytes)
                 (hash-table-p object) (= 4 (hash-table-count object))
                 (integerp generation) (> generation 0)
                 (integerp expires) (> expires 0)
                 (niwa-journal-store-id-p owner)
                 (equal (concat owner ".json") (file-name-nondirectory path))
                 (vectorp entries) (<= 1 (length entries) 50)
                 (cl-every
                  (lambda (entry)
                    (let ((file (and (hash-table-p entry) (gethash "file" entry)))
                          (id (and (hash-table-p entry) (gethash "id" entry)))
                          (kind (and (hash-table-p entry) (gethash "kind" entry)))
                          (private (and (hash-table-p entry) (gethash "private" entry)))
                          (hash (and (hash-table-p entry) (gethash "sha256" entry))))
                      (and (hash-table-p entry) (= (if (equal kind "digest") 5 8) (hash-table-count entry))
                           (niwa-journal-store-file-name-p file)
                           (niwa-journal-store-id-p id)
                           (niwa-journal-store-id-p hash)
                           (member kind '("digest" "consolidation"))
                           (vectorp private)
                           (if (equal kind "digest") (= 0 (length private))
                             (niwa-journal-cleanup--memory-entry-p entry))
                           (string-suffix-p
                            (concat "-" (niwa-journal-store--record-id
                                         (if (equal kind "digest") (list :capture-id id) (list :pass-id id))
                                         (intern kind)) ".md")
                            file))))
                  entries))
      (error "Invalid journal expiry manifest: %s" path))
    (list :path path :hash (secure-hash 'sha256 text) :entries entries
          :token (list :directory (niwa-journal-store-claim-directory root 'mutation)
                       :generation generation :owner owner :expires-at expires))))

(defun niwa-journal-cleanup--apply (root manifest)
  "Apply MANIFEST only if its immutable claim outcome accepted its exact hash.
An accepted manifest remains recoverable after its owner expires.  Return the
number of public files deleted; finishing the manifest now also sets
`niwa-journal-cleanup--finished', even when no public file remained."
  (let* ((outcome (niwa-journal-claim-outcome (plist-get manifest :token)))
         (hash (plist-get manifest :hash))
         (done (concat (plist-get manifest :path) ".done"))
         (deleted 0))
    (when (and (eq 'completed (plist-get outcome :status))
               (not (string-empty-p (plist-get outcome :payload)))
               (not (equal hash (plist-get outcome :payload))))
      (error "Accepted journal expiry manifest changed"))
    (when (and (eq 'completed (plist-get outcome :status))
               (equal hash (plist-get outcome :payload)))
      (if (niwa-session-control-fs-path-exists-p done)
          (unless (equal hash (niwa-session-control-fs-read-file done))
            (error "Invalid journal expiry completion"))
        (niwa-session-control-fs-make-directory (file-name-concat (niwa-journal-store-state-directory root) "expired") t)
        (seq-doseq (entry (plist-get manifest :entries))
          (let* ((file (gethash "file" entry))
                 (expected (gethash "sha256" entry))
                 (capture (and (equal (gethash "kind" entry) "digest")
                               (file-name-concat (niwa-journal-store-state-directory root) "captures" (gethash "id" entry)))))
            (when (equal (gethash "kind" entry) "consolidation")
              (let ((retired (file-name-concat (niwa-journal-store-state-directory root) "retired-passes" (gethash "id" entry)))
                    (record (json-serialize (list :sha256 expected :scope (gethash "scope" entry)
                                                  :journal-bearing (gethash "journal-bearing" entry)
                                                  :created (substring file 0 20)))))
                (niwa-session-control-fs-make-directory (file-name-directory retired) t)
                (unless (niwa-session-control-fs-create-or-verify retired record)
                  (error "Conflicting memory retirement marker"))))
            (dolist (record (cons entry (append (gethash "related" entry) nil)))
              (let* ((file (gethash "file" record)) (expected (gethash "sha256" record))
                     (path (file-name-concat root file))
                     (marker (niwa-journal-store-expired-marker root file)))
                (unless (niwa-session-control-fs-create-or-verify marker expected)
                  (error "Conflicting journal expiry marker"))
                (condition-case nil
                    (let ((text (niwa-session-control-fs-read-file
                                 path 'utf-8-unix (1+ niwa-journal-store--entry-max-bytes))))
                      (unless (equal expected (secure-hash 'sha256 text))
                        (error "Expired journal entry changed; retained for inspection"))
                      (niwa-session-control-fs-delete-file path)
                      (cl-incf deleted))
                  (niwa-session-control-fs-absent nil))))
            (when (and capture (niwa-session-control-fs-path-exists-p (file-name-concat capture "retired")))
              (niwa-session-control-fs-delete-directory capture))
            (seq-doseq (record (gethash "private" entry))
              (let ((path (file-name-concat (niwa-journal-store-state-directory root)
                                            (gethash "file" record))))
                (condition-case nil
                    (let ((text (niwa-session-control-fs-read-file path 'utf-8-unix (1+ (* 4 1024 1024)))))
                      (unless (equal (gethash "sha256" record) (secure-hash 'sha256 text))
                        (error "Expired private memory state changed; retained for inspection"))
                      (niwa-session-control-fs-delete-file path))
                  (niwa-session-control-fs-absent nil))))))
        (unless (niwa-session-control-fs-create-or-verify done hash)
          (error "Conflicting journal expiry completion"))
        (setq niwa-journal-cleanup--finished t)))
    deleted))

(defvar niwa-journal-cleanup--retired-observation nil
  "Display scan's (WORKSPACE-ROOT . IDS) retirement observation, or nil.
Bind only around read-only collection; never reuse for recovery or mutation.")

(defun niwa-journal-cleanup--retired-path (workspace-root id)
  "Return the retirement marker path of pass ID in WORKSPACE-ROOT."
  (unless (niwa-journal-store-id-p id) (error "Invalid memory pass identity"))
  (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory workspace-root)) "retired-passes" id))

(defun niwa-journal-cleanup-pass-retired-p (workspace-root id &optional observation)
  "Return non-nil when pass ID in WORKSPACE-ROOT is durably retired.
This only prevents reuse of expired private state; it never authorizes deletion.
OBSERVATION may be the marker's optional `path-exists-p' result from this
operation's batch; it must never be retained between operations."
  (let ((path (niwa-journal-cleanup--retired-path workspace-root id)))
    (cond ((equal workspace-root (car niwa-journal-cleanup--retired-observation))
           (member id (cdr niwa-journal-cleanup--retired-observation)))
          (observation
           (unless (equal path (plist-get observation :path))
             (error "Retirement observation addresses another pass"))
           (pcase (plist-get observation :status)
             ('ok t)
             ('absent nil)
             (_ (niwa-session-control-fs-program-value observation))))
          (t (niwa-session-control-fs-path-exists-p path)))))

(defun niwa-journal-cleanup-recover (root)
  "Finish accepted expiry manifests under journal ROOT before new mutations.
Callers must hold journal mutation ownership while selecting subsequent
evidence.  Return how many manifests this call finished; already finished
manifests are verified but not counted."
  (let ((finished 0))
    (dolist (path (niwa-session-control-fs-list-directory
                   (file-name-concat (niwa-journal-store-state-directory root) "expiry")
                   (concat "\\`" niwa-journal-store-hash-regexp "\\.json\\'")))
      (let ((niwa-journal-cleanup--finished nil))
        (niwa-journal-cleanup--apply root (niwa-journal-cleanup--read root path))
        (when niwa-journal-cleanup--finished (cl-incf finished))))
    finished))

(defun niwa-journal-cleanup--protected-p (root entry)
  "Return non-nil when ENTRY retains pending capture or pinned review evidence."
  (let ((capture (file-name-concat (niwa-journal-store-state-directory root) "captures" (plist-get entry :capture-id))))
    (or (equal '(t nil) (niwa-session-control-fs-paths-exist
                         (list capture (file-name-concat capture "retired"))))
        (niwa-session-control-fs-list-directory
         (file-name-concat (niwa-journal-store-state-directory root) "evidence-pins" (plist-get entry :id))
         "\\`[^.]"))))

(defun niwa-journal-cleanup-pending-pass-p (root pass)
  "Whether an accepted, unfinished expiry in journal ROOT retires PASS.
Call while holding the workspace mutation flock to exclude new elections."
  (seq-some
   (lambda (path)
     (unless (niwa-session-control-fs-path-exists-p (concat path ".done"))
       (let* ((manifest (niwa-journal-cleanup--read root path))
              (outcome (niwa-journal-claim-outcome (plist-get manifest :token))))
         (and (eq 'completed (plist-get outcome :status))
              (not (string-empty-p (plist-get outcome :payload)))
              (or (equal (plist-get outcome :payload) (plist-get manifest :hash))
                  (error "Accepted journal expiry manifest changed: %s" path))
              (seq-some (lambda (entry) (and (equal "consolidation" (gethash "kind" entry))
                                             (equal pass (gethash "id" entry))))
                        (plist-get manifest :entries))))))
   (niwa-session-control-fs-list-directory
    (file-name-concat (niwa-journal-store-state-directory root) "expiry")
    (concat "\\`" niwa-journal-store-hash-regexp "\\.json\\'"))))

(defun niwa-journal-cleanup--owned (workspace claim limit owner)
  "Recover and accept expiry for WORKSPACE under CLAIM.
OWNER holds its workspace flock from selection through manifest election.
Select at most LIMIT content groups, including recovery.  Return
\(:count DELETED :more MORE), where MORE is non-nil when LIMIT was reached."
  (let* ((workspace-root (niwa-workspace-root workspace))
         (root (niwa-journal-store-directory workspace-root))
         (deleted 0)
         (remaining limit)
         observed unreviewed entries)
    ;; Recovery consumes the same content budget as new work.  A manifest is
    ;; indivisible: defer one that does not fit until the next idle batch.
    (dolist (path (niwa-session-control-fs-list-directory
                   (file-name-concat (niwa-journal-store-state-directory root) "expiry")
                   (concat "\\`" niwa-journal-store-hash-regexp "\\.json\\'")))
      (unless (niwa-session-control-fs-path-exists-p (concat path ".done"))
        (let* ((manifest (niwa-journal-cleanup--read root path))
               (count (length (plist-get manifest :entries)))
               (outcome (niwa-journal-claim-outcome (plist-get manifest :token))))
          (when (and (<= count remaining)
                     (eq 'completed (plist-get outcome :status))
                     (not (string-empty-p (plist-get outcome :payload))))
            (cl-incf deleted (niwa-journal-cleanup--apply root manifest))
            (cl-decf remaining count)))))
    (setq observed (niwa-journal-store-entries workspace-root)
          unreviewed (niwa-journal-index-unreviewed observed))
    (dolist (entry (reverse observed))
      (when (< (length entries) remaining)
        (pcase (plist-get entry :kind)
          ('digest
           (unless (or (memq entry unreviewed)
                       (niwa-journal-cleanup--protected-p root entry))
             (niwa-journal-store--record-coverage workspace-root entry)
             (push (list :kind "digest" :id (plist-get entry :capture-id) :file (plist-get entry :file)
                         :sha256 (secure-hash 'sha256 (plist-get entry :text)) :private []) entries)))
          ('consolidation
           (when-let* ((candidate (niwa-memory-cleanup-entry workspace entry observed)))
             (push candidate entries))))))
    (when entries
      (let* ((directory (file-name-concat (niwa-journal-store-state-directory root) "expiry"))
             (path (file-name-concat directory (concat (plist-get claim :owner) ".json")))
             (text (json-serialize
                    (append (niwa-journal-claim--record claim)
                            (list :entries (vconcat (nreverse entries)))))))
        (when (> (string-bytes text) niwa-journal-cleanup--max-bytes)
          (error "Journal expiry dependency group exceeds its byte limit"))
        (niwa-session-control-fs-make-directory directory t)
        (unless (niwa-session-control-fs-create-file path text)
          (error "Journal expiry manifest already exists"))
        (when (niwa-journal-claim-settle claim 'completed (secure-hash 'sha256 text) owner)
          (cl-incf deleted
                   (niwa-journal-cleanup--apply root (niwa-journal-cleanup--read root path))))))
    (list :count deleted
          :more (or (zerop remaining) (and entries (= (length entries) remaining))))))

(defvar niwa-journal-cleanup--journal-only nil
  "Non-nil in a worker whose editor already handled live artifact retention.")

(iter-defun niwa-journal-cleanup--steps (workspace force)
  "Yield between WORKSPACE cleanup phases, bypassing its throttle with FORCE.
Claims remain held across yields.  Closing the iterator settles acquired
claims.  Live-buffer artifact retention remains an indivisible phase.  A batch
ends with (:count DELETED :more MORE); a throttled or unavailable run, or an
error in the editor, ends with nil."
  (when (and workspace (niwa-workspace-root workspace)
             (or force (null (niwa-workspace-journal-cleanup-at workspace))
                 (>= (- (float-time) (niwa-workspace-journal-cleanup-at workspace)) 3600)))
    (setf (niwa-workspace-journal-cleanup-at workspace) (float-time))
    (unless niwa-journal-cleanup--journal-only (niwa-state-cleanup workspace))
    (iter-yield nil)
    (condition-case err
        (let* ((root (niwa-journal-store-directory (niwa-workspace-root workspace)))
               (present (or (niwa-session-control-fs-directory-p
                             (niwa-journal-store-state-directory root))
                            (niwa-session-control-fs-directory-p root)))
               claim digest consolidation)
          (unwind-protect
              (progn
                ;; The asynchronous owner checks this before every resumed
                ;; step, including yields delegated by coordination GC.  A
                ;; suspension must not let expired claims protect stale
                ;; dependency observations against a successor.
                (iter-yield
                 (lambda ()
                   (when (and claim
                              (not (apply #'niwa-journal-claim-owned-p
                                          (delq nil (list claim digest consolidation)))))
                     (error "Journal cleanup ownership expired before resumption"))))
                (when present
                  (setq claim (niwa-journal-claim-acquire
                               (niwa-journal-store-claim-directory root 'mutation) 120)))
                (iter-yield nil)
                (when claim
                  (setq digest (niwa-journal-claim-acquire
                                (niwa-journal-store-claim-directory root 'digest-run) 120
                                (plist-get claim :expires-at))))
                (iter-yield nil)
                (when digest
                  (setq consolidation (niwa-journal-claim-acquire
                                       (niwa-journal-store-claim-directory root 'consolidation) 120
                                       (plist-get claim :expires-at))))
                (iter-yield nil)
                (if (and claim digest consolidation)
                    (let* ((collected (iter-yield-from (niwa-journal-gc workspace claim)))
                           (_ (iter-yield nil))
                           (owned (niwa-session-control-fs-call-with-locks
                                   (list (plist-get claim :directory))
                                   (lambda (owner) (niwa-journal-cleanup--owned
                                                    workspace claim (plist-get collected :remaining) owner))))
                           (deleted (plist-get owned :count))
                           ;; A short batch already inspected the remaining work.
                           ;; Rescanning on any progress lets cleanup feed itself:
                           ;; each pass creates claims the next pass can collect.
                           (more (and (> (+ (plist-get collected :progress) deleted) 0)
                                      (or (plist-get collected :more) (plist-get owned :more))
                                      t)))
                      (when more (niwa-journal-cleanup-schedule workspace t))
                      (list :count deleted :more more))
                  (when present (setf (niwa-workspace-journal-cleanup-at workspace) nil))
                  nil))
            (setf (niwa-workspace-journal-observation workspace) nil
                  (niwa-workspace-memory-observation workspace) nil)
            (when consolidation (ignore-errors (niwa-journal-claim-settle consolidation 'completed "")))
            (when digest (ignore-errors (niwa-journal-claim-settle digest 'completed "")))
            (when claim (ignore-errors (niwa-journal-claim-settle claim 'completed "")))))
      (error
       (if niwa-journal-worker--child-p
           (signal (car err) (cdr err))
         (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "journal cleanup failed: %s" (error-message-string err))
         nil)))))

(defvar niwa-journal-cleanup--inhibit-scheduling nil
  "Non-nil suppresses idle cleanup during exit or isolated tests.")

(defvar niwa-journal-cleanup--active (make-hash-table :test #'equal)
  "Exact local invocations admitted to cleanup IO, keyed by workspace roots.
This is a local activity observation, never filesystem ownership proof.")

(defvar niwa-journal-cleanup--pending (make-hash-table :test #'equal)
  "Coalesced (TIMER . FORCE) cleanup requests keyed by workspace roots.")

(defun niwa-journal-cleanup-active-p (workspace)
  "Return non-nil while this client's WORKSPACE cleanup has entered IO.
Queued timers and transport-deferred requests are not active.  This observation
does not grant ownership or permit cancellation of another client's cleanup."
  (gethash (niwa-workspace-root workspace) niwa-journal-cleanup--active))

;;;###autoload
(defun niwa-journal-cleanup-expired (workspace &optional force)
  "Collect one batch of resolved WORKSPACE journal state synchronously.
Throttle opportunities to once an hour unless FORCE is non-nil.  Live mutation,
digest owners or another local cleanup postpone it.  No inference runs.  Return
\(:count DELETED :more MORE) when a batch ran, where MORE is non-nil if it
reached a work limit and may leave more, even for zero deletions.  Return nil
when busy, throttled, or unavailable."
  (when (and workspace (not (niwa-journal-cleanup-active-p workspace)))
    (let ((root (niwa-workspace-root workspace)) (token (list 'synchronous)) guard)
      (puthash root token niwa-journal-cleanup--active)
      (unwind-protect
          (iter-do (value (niwa-journal-cleanup--steps workspace force))
		   (when (functionp value) (setq guard value))
		   (when guard (funcall guard)))
        (when (eq token (gethash root niwa-journal-cleanup--active))
          (remhash root niwa-journal-cleanup--active))))))

(defun niwa-journal-cleanup--memory-busy-p (workspace)
  "Return non-nil while loaded memory owners have local work in WORKSPACE.
Unloaded optional owners cannot have admitted work; observing cleanup must not
load review or provider machinery.  Target claims remain authoritative."
  (or (and (featurep 'niwa-memory-pass)
           (or (niwa-memory-pass-running workspace)
               (niwa-memory-pass-drain-status workspace)))
      (and (featurep 'niwa-memory-decision)
           (or (niwa-memory-decision-recovering-p workspace)
               (niwa-memory-decision-admitted-p
                (niwa-journal-store-claim-directory
                 (niwa-journal-store-directory (niwa-workspace-root workspace)) 'consolidation))))))

(defun niwa-journal-cleanup-wait (workspace)
  "Let this client's active WORKSPACE cleanup release its claims.
Queued work and reentry into synchronous cleanup need no wait.  Allow at most
the scheduled cleanup claim's 120-second
lifetime, and allow quitting.  Suppress further batches while waiting; never
kill a child holding claims or steal another owner's claim."
  (let* ((root (niwa-workspace-root workspace))
         (job (gethash root niwa-journal-cleanup--pending)))
    (when (and job (eq job (gethash root niwa-journal-cleanup--active)))
      (let ((deadline (+ (float-time) 120))
            (niwa-journal-cleanup--inhibit-scheduling t))
        (message "niwa: waiting for journal cleanup")
        (while (and (eq job (gethash root niwa-journal-cleanup--pending))
                    (< (float-time) deadline))
          (accept-process-output nil .05))))))

;;;###autoload
(defun niwa-journal-cleanup-schedule (workspace &optional force)
  "Schedule WORKSPACE cleanup in phases, bypassing the hourly gate with FORCE.
Start at idle and return to the event loop between phases.  Repeated requests
coalesce; a forced request received during cleanup runs after that batch."
  (unless niwa-journal-cleanup--inhibit-scheduling
    (let* ((root (niwa-workspace-root workspace))
           (old (gethash root niwa-journal-cleanup--pending)))
      (if old
          (setcdr old (or force (cdr old)))
        (let ((job (cons nil force)) iterator guard worker finishing)
          (cl-labels
           ((finish ()
              (unless finishing
                (setq finishing t)
                (unwind-protect
                    (when iterator
                      (let ((closing iterator))
                        (setq iterator nil)
                        (iter-close closing)))
                  (when (timerp (car job)) (cancel-timer (car job)))
                  (when (eq job (gethash root niwa-journal-cleanup--active))
                    (remhash root niwa-journal-cleanup--active))
                  (when (eq job (gethash root niwa-journal-cleanup--pending))
                    (remhash root niwa-journal-cleanup--pending))
                  (when worker (niwa-journal-worker-cancel worker))
                  (when (and (cdr job) (not (gethash root niwa-journal-cleanup--pending)))
                    (niwa-journal-cleanup-schedule workspace t)))))
            (cancel ()
              (setcdr job nil)
              (finish))
            (defer-memory ()
              ;; Only admission is deferred.  An active cleanup must finish
              ;; even if a drain was queued while its claims were held.
              (let ((active (gethash root niwa-journal-cleanup--active)))
                (when (and (not niwa-journal-cleanup--inhibit-scheduling)
                           (not (eq job active))
                           (or active (niwa-journal-cleanup--memory-busy-p workspace)))
                  (setcar job (run-at-time 1 nil #'step))
                  t)))
            (step ()
              (when (and (not finishing) (eq job (gethash root niwa-journal-cleanup--pending)))
                (setcar job nil)
                (unless (or (defer-memory)
                            (niwa-transport-run-when-idle
                             (list 'journal-cleanup root) root
                             (lambda ()
                               (condition-case err
                                   (cond
                                    (niwa-journal-cleanup--inhibit-scheduling (finish))
                                    ;; Transport deferral may span a new pass.
                                    ((defer-memory))
                                    (t
                                     (puthash root job niwa-journal-cleanup--active)
                                     (if (niwa-journal-worker-local-p workspace)
                                         (let ((force (cdr job)))
                                           (setcdr job nil)
                                           (if (and (not force)
                                                    (niwa-workspace-journal-cleanup-at workspace)
                                                    (< (- (float-time) (niwa-workspace-journal-cleanup-at workspace)) 3600))
                                               (finish)
                                             (niwa-state-cleanup workspace)
                                             (setq worker
                                                   (niwa-journal-worker-start
                                                    workspace 'cleanup
                                                    (list :force force)
                                                    (lambda (result)
                                                      (when (eq job (gethash root niwa-journal-cleanup--pending))
							(setf (niwa-workspace-journal-observation workspace) nil
                                                              (niwa-workspace-memory-observation workspace) nil
                                                              (niwa-workspace-journal-cleanup-at workspace)
                                                              (plist-get result :cleanup-at))
							(when (plist-get result :more) (setcdr job t))
							(when (and (plist-get result :error)
                                                                   (not niwa-journal-cleanup--inhibit-scheduling))
                                                          (niwa-diagnostic-message (niwa-diagnostic-scope root) "journal cleanup failed: %s" (plist-get result :error)))
							(finish)))))))
                                       (unless iterator
                                         (setq iterator (niwa-journal-cleanup--steps workspace (cdr job)))
                                         (setcdr job nil))
                                       (when guard (funcall guard))
                                       (when-let* ((value (iter-next iterator)))
                                         (when (functionp value) (setq guard value)))
                                       (setcar job (run-at-time .001 nil #'step)))))
                                 (iter-end-of-sequence (finish))
                                 ((error quit)
                                  (finish)
                                  (niwa-diagnostic-message (niwa-diagnostic-scope root) "journal cleanup failed: %s" (error-message-string err)))))
                             #'cancel))
                  (cancel)))))
           (puthash root job niwa-journal-cleanup--pending)
           (setcar job (run-with-idle-timer .1 nil #'step))))))))

(defun niwa-journal-cleanup-child (workspace payload)
  "Run journal-only retention for WORKSPACE using PAYLOAD's :force flag."
  (let* ((niwa-journal-cleanup--journal-only t)
         (batch (niwa-journal-cleanup-expired workspace (plist-get payload :force))))
    (list :ok t :count (plist-get batch :count) :more (plist-get batch :more)
          :cleanup-at (niwa-workspace-journal-cleanup-at workspace))))

(provide 'niwa-journal-cleanup)
;;; niwa-journal-cleanup.el ends here
