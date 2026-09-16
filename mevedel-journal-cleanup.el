;;; mevedel-journal-cleanup.el -- Recoverable workspace journal expiry -*- lexical-binding: t -*-

;;; Commentary:

;; Physical expiry is a bounded journal mutation, separate from ordinary recall
;; age limits.  Unreviewed digests and unresolved evidence stay recoverable.
;; An immutable manifest is accepted before storage is retired or removed.
;; Accepted manifests are replayed before selecting further evidence.  Private
;; turn coverage survives, while retired capture payloads may be collected.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-claim)
(require 'mevedel-journal-index)
(require 'mevedel-journal-store)
(require 'mevedel-structs)

(autoload 'mevedel-journal-gc "mevedel-journal-gc")

;; `mevedel-state-cleanup'
(declare-function mevedel-state-cleanup "mevedel-state-cleanup" (workspace))
(autoload 'mevedel-state-cleanup "mevedel-state-cleanup")

;; `mevedel-memory-cleanup'
(declare-function mevedel-memory-cleanup-entry "mevedel-memory-cleanup" (workspace entry entries))
(autoload 'mevedel-memory-cleanup-entry "mevedel-memory-cleanup")

;; `mevedel-transport'
(declare-function mevedel-transport-run-when-idle "mevedel-transport"
                  (key path thunk &optional on-cancel))

(defconst mevedel-journal-cleanup--max-bytes (* 4 1024 1024)
  "Maximum encoded expiry manifest, including dependent file identities.")

(defun mevedel-journal-cleanup--memory-entry-p (entry)
  "Validate ENTRY's closed memory dependency paths and identity hashes."
  (let* ((id (gethash "id" entry))
         (related (gethash "related" entry))
         (private (gethash "private" entry)))
    (and (member (gethash "scope" entry) '("general" "focused"))
         (vectorp related)
         (cl-every
          (lambda (record)
            (and (hash-table-p record) (= 3 (hash-table-count record))
                 (mevedel-journal-store-id-p (gethash "id" record))
                 (mevedel-journal-store-id-p (gethash "sha256" record))
                 (mevedel-journal-store-file-name-p (gethash "file" record))
                 (string-suffix-p (concat "-" (mevedel-journal-store--record-id (list :decision-id (gethash "id" record)) 'decision) ".md")
                                  (gethash "file" record)))) related)
         (cl-every (lambda (record)
                     (and (hash-table-p record) (= 2 (hash-table-count record))
                          (stringp (gethash "file" record))
                          (mevedel-journal-store-id-p (gethash "sha256" record)))) private)
         (let ((required (append (list (format "passes/%s/accepted.el" id) (format "passes/%s/prepared.el" id))
                                 (mapcar (lambda (record) (format "decisions/%s.el" (gethash "id" record))) related)))
               (paths (mapcar (lambda (record) (gethash "file" record)) private)))
           (and (= (length paths) (length (delete-dups (copy-sequence paths))))
                (cl-every (lambda (path) (member path paths)) required)
                (cl-every (lambda (path) (or (member path required)
                                           (string-match-p (concat "\\`writes/" mevedel-journal-store-hash-regexp "\\.el\\'") path))) paths))))))

(defun mevedel-journal-cleanup--read (root path)
  "Read a bounded immutable expiry manifest at PATH under journal ROOT."
  (let* ((text (mevedel-session-control-fs-read-file path 'utf-8-unix (1+ mevedel-journal-cleanup--max-bytes)))
         (object (json-parse-string text))
         (generation (and (hash-table-p object) (gethash "generation" object)))
         (owner (and (hash-table-p object) (gethash "owner" object)))
         (expires (and (hash-table-p object) (gethash "expires-at" object)))
         (entries (and (hash-table-p object) (gethash "entries" object))))
    (unless (and (<= (string-bytes text) mevedel-journal-cleanup--max-bytes)
                 (hash-table-p object) (= 4 (hash-table-count object))
                 (integerp generation) (> generation 0)
                 (integerp expires) (> expires 0)
                 (mevedel-journal-store-id-p owner)
                 (equal (concat owner ".json") (file-name-nondirectory path))
                 (vectorp entries) (<= 1 (length entries) 50)
                 (cl-every
                  (lambda (entry)
                    (let ((file (and (hash-table-p entry) (gethash "file" entry)))
                          (id (and (hash-table-p entry) (gethash "id" entry)))
                          (kind (and (hash-table-p entry) (gethash "kind" entry)))
                          (private (and (hash-table-p entry) (gethash "private" entry)))
                          (hash (and (hash-table-p entry) (gethash "sha256" entry))))
                      (and (hash-table-p entry) (= (if (equal kind "digest") 5 7) (hash-table-count entry))
                           (mevedel-journal-store-file-name-p file)
                           (mevedel-journal-store-id-p id)
                           (mevedel-journal-store-id-p hash)
                           (member kind '("digest" "consolidation"))
                           (vectorp private)
                           (if (equal kind "digest") (= 0 (length private))
                             (mevedel-journal-cleanup--memory-entry-p entry))
                           (string-suffix-p
                            (concat "-" (mevedel-journal-store--record-id
                                         (if (equal kind "digest") (list :capture-id id) (list :pass-id id))
                                         (intern kind)) ".md")
                            file))))
                  entries))
      (error "Invalid journal expiry manifest"))
    (list :path path :hash (secure-hash 'sha256 text) :entries entries
          :token (list :directory (mevedel-journal-store-claim-directory root 'mutation)
                       :generation generation :owner owner :expires-at expires))))

(defun mevedel-journal-cleanup--apply (root manifest)
  "Apply MANIFEST only if its immutable claim outcome accepted its exact hash.
An accepted manifest remains recoverable after its owner expires."
  (let* ((outcome (mevedel-journal-claim-outcome (plist-get manifest :token)))
         (hash (plist-get manifest :hash))
         (done (concat (plist-get manifest :path) ".done"))
         (deleted 0))
    (when (and (eq 'completed (plist-get outcome :status))
               (not (string-empty-p (plist-get outcome :payload)))
               (not (equal hash (plist-get outcome :payload))))
      (error "Accepted journal expiry manifest changed"))
    (when (and (eq 'completed (plist-get outcome :status))
               (equal hash (plist-get outcome :payload)))
      (if (mevedel-session-control-fs-path-exists-p done)
          (unless (equal hash (mevedel-session-control-fs-read-file done))
            (error "Invalid journal expiry completion"))
        (mevedel-session-control-fs-make-directory (file-name-concat (mevedel-journal-store-state-directory root) "expired") t)
        (seq-doseq (entry (plist-get manifest :entries))
          (let* ((file (gethash "file" entry))
                 (expected (gethash "sha256" entry))
                 (capture (and (equal (gethash "kind" entry) "digest")
                               (file-name-concat (mevedel-journal-store-state-directory root) "captures" (gethash "id" entry)))))
            (when (equal (gethash "kind" entry) "consolidation")
              (let ((retired (file-name-concat (mevedel-journal-store-state-directory root) "retired-passes" (gethash "id" entry)))
                    (record (json-serialize (list :sha256 expected :scope (gethash "scope" entry)
                                                  :created (substring file 0 20)))))
                (mevedel-session-control-fs-make-directory (file-name-directory retired) t)
                (unless (mevedel-session-control-fs-create-or-verify retired record)
                  (error "Conflicting memory retirement marker"))))
            (dolist (record (cons entry (append (gethash "related" entry) nil)))
              (let* ((file (gethash "file" record)) (expected (gethash "sha256" record))
                     (path (file-name-concat root file))
                     (marker (mevedel-journal-store-expired-marker root file)))
                (unless (mevedel-session-control-fs-create-or-verify marker expected)
                  (error "Conflicting journal expiry marker"))
                (condition-case nil
                    (let ((text (mevedel-session-control-fs-read-file
                                 path 'utf-8-unix (1+ mevedel-journal-store--entry-max-bytes))))
                      (unless (equal expected (secure-hash 'sha256 text))
                        (error "Expired journal entry changed; retained for inspection"))
                      (mevedel-session-control-fs-delete-file path)
                      (cl-incf deleted))
                  (mevedel-session-control-fs-absent nil))))
            (when (and capture (mevedel-session-control-fs-path-exists-p (file-name-concat capture "retired")))
              (mevedel-session-control-fs-delete-directory capture))
            (seq-doseq (record (gethash "private" entry))
              (let ((path (file-name-concat (mevedel-journal-store-state-directory root)
                                            (gethash "file" record))))
                (condition-case nil
                    (let ((text (mevedel-session-control-fs-read-file path 'utf-8-unix (1+ (* 4 1024 1024)))))
                      (unless (equal (gethash "sha256" record) (secure-hash 'sha256 text))
                        (error "Expired private memory state changed; retained for inspection"))
                      (mevedel-session-control-fs-delete-file path))
                  (mevedel-session-control-fs-absent nil))))))
        (unless (mevedel-session-control-fs-create-or-verify done hash)
          (error "Conflicting journal expiry completion"))))
    deleted))

(defun mevedel-journal-cleanup-pass-retired-p (workspace-root id)
  "Return non-nil when pass ID in WORKSPACE-ROOT is durably retired.
This only prevents reuse of expired private state; it never authorizes deletion."
  (unless (mevedel-journal-store-id-p id) (error "Invalid memory pass identity"))
  (mevedel-session-control-fs-path-exists-p
   (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory workspace-root)) "retired-passes" id)))

(defun mevedel-journal-cleanup-recover (root)
  "Finish accepted expiry manifests under journal ROOT before new mutations.
Callers must hold journal mutation ownership while selecting subsequent
evidence."
  (let ((deleted 0))
    (dolist (path (mevedel-session-control-fs-list-directory
                  (file-name-concat (mevedel-journal-store-state-directory root) "expiry")
                  (concat "\\`" mevedel-journal-store-hash-regexp "\\.json\\'")))
      (cl-incf deleted (mevedel-journal-cleanup--apply root (mevedel-journal-cleanup--read root path))))
    deleted))

(defun mevedel-journal-cleanup--protected-p (root entry)
  "Return non-nil when ENTRY retains pending capture or pinned review evidence."
  (let ((capture (file-name-concat (mevedel-journal-store-state-directory root) "captures" (plist-get entry :capture-id))))
    (or (and (mevedel-session-control-fs-path-exists-p capture)
             (not (mevedel-session-control-fs-path-exists-p (file-name-concat capture "retired"))))
        (mevedel-session-control-fs-list-directory
         (file-name-concat (mevedel-journal-store-state-directory root) "evidence-pins" (plist-get entry :id))
         "\\`[^.]"))))

(defun mevedel-journal-cleanup--owned (workspace claim &optional limit)
  "Recover and accept expiry for WORKSPACE under CLAIM.
Select at most LIMIT content groups (default 50), including recovery."
  (let* ((workspace-root (mevedel-workspace-root workspace))
         (root (mevedel-journal-store-directory workspace-root))
         (deleted 0)
         (remaining (or limit 50))
         observed unreviewed entries)
    ;; Recovery consumes the same content budget as new work.  A manifest is
    ;; indivisible: defer one that does not fit until the next idle batch.
    (dolist (path (mevedel-session-control-fs-list-directory
                  (file-name-concat (mevedel-journal-store-state-directory root) "expiry")
                  (concat "\\`" mevedel-journal-store-hash-regexp "\\.json\\'")))
      (unless (mevedel-session-control-fs-path-exists-p (concat path ".done"))
        (let* ((manifest (mevedel-journal-cleanup--read root path))
               (count (length (plist-get manifest :entries)))
               (outcome (mevedel-journal-claim-outcome (plist-get manifest :token))))
          (when (and (<= count remaining)
                     (eq 'completed (plist-get outcome :status))
                     (not (string-empty-p (plist-get outcome :payload))))
            (cl-incf deleted (mevedel-journal-cleanup--apply root manifest))
            (cl-decf remaining count)))))
    (setq observed (mevedel-journal-store-entries workspace-root)
          unreviewed (mevedel-journal-index-unreviewed observed))
    (dolist (entry (reverse observed))
      (when (< (length entries) remaining)
        (pcase (plist-get entry :kind)
          ('digest
           (unless (or (memq entry unreviewed)
                       (mevedel-journal-cleanup--protected-p root entry))
             (mevedel-journal-store--record-coverage workspace-root entry)
             (push (list :kind "digest" :id (plist-get entry :capture-id) :file (plist-get entry :file)
                         :sha256 (secure-hash 'sha256 (plist-get entry :text)) :private []) entries)))
          ('consolidation
           (when-let* ((candidate (mevedel-memory-cleanup-entry workspace entry observed)))
             (push candidate entries))))))
    (when entries
      (let* ((directory (file-name-concat (mevedel-journal-store-state-directory root) "expiry"))
             (path (file-name-concat directory (concat (plist-get claim :owner) ".json")))
             (text (json-serialize
                    (append (mevedel-journal-claim--record claim)
                            (list :entries (vconcat (nreverse entries)))))))
        (when (> (string-bytes text) mevedel-journal-cleanup--max-bytes)
          (error "Journal expiry dependency group exceeds its byte limit"))
        (mevedel-session-control-fs-make-directory directory t)
        (unless (mevedel-session-control-fs-create-file path text)
          (error "Journal expiry manifest already exists"))
        (when (mevedel-journal-claim-settle claim 'completed (secure-hash 'sha256 text))
          (cl-incf deleted
                   (mevedel-journal-cleanup--apply root (mevedel-journal-cleanup--read root path))))))
    deleted))

;;;###autoload
(defun mevedel-journal-cleanup-expired (workspace &optional force)
  "Collect one batch of resolved WORKSPACE journal state.
Throttle opportunities to once an hour unless FORCE is non-nil.  Live mutation
or digest owners postpone cleanup.  No inference runs.  Return a deletion count
or nil when busy, throttled, or unavailable."
  (when (and workspace (mevedel-workspace-root workspace)
             (or force (null (mevedel-workspace-journal-cleanup-at workspace))
                 (>= (- (float-time) (mevedel-workspace-journal-cleanup-at workspace)) 3600)))
    (setf (mevedel-workspace-journal-cleanup-at workspace) (float-time))
    (mevedel-state-cleanup workspace)
    (condition-case err
        (let* ((root (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
               (present (or (mevedel-session-control-fs-directory-p
                             (mevedel-journal-store-state-directory root))
                            (mevedel-session-control-fs-directory-p root)))
               (claim (and present
                           (mevedel-journal-claim-acquire (mevedel-journal-store-claim-directory root 'mutation) 120)))
               digest consolidation)
          (unwind-protect
              (progn
                (when claim
                  (setq digest (mevedel-journal-claim-acquire
                                (mevedel-journal-store-claim-directory root 'digest-run) 120
                                (plist-get claim :expires-at))))
                (when digest
                  (setq consolidation (mevedel-journal-claim-acquire
                                       (mevedel-journal-store-claim-directory root 'consolidation) 120
                                       (plist-get claim :expires-at))))
                (if (and claim digest consolidation)
                    (let* ((collected (mevedel-journal-gc workspace claim))
                           (deleted (mevedel-journal-cleanup--owned workspace claim (plist-get collected :remaining))))
                      (when (> (+ (plist-get collected :progress) deleted) 0)
                        (mevedel-journal-cleanup-schedule workspace t))
                      deleted)
                  (when present (setf (mevedel-workspace-journal-cleanup-at workspace) nil))
                  nil))
            (setf (mevedel-workspace-journal-observation workspace) nil
                  (mevedel-workspace-memory-observation workspace) nil)
            (when consolidation (ignore-errors (mevedel-journal-claim-settle consolidation 'completed "")))
            (when digest (ignore-errors (mevedel-journal-claim-settle digest 'completed "")))
            (when claim (ignore-errors (mevedel-journal-claim-settle claim 'completed "")))))
      (error
       (message "mevedel: journal cleanup failed: %s" (error-message-string err))
       nil))))

(defvar mevedel-journal-cleanup--inhibit-scheduling nil
  "Non-nil suppresses idle cleanup during exit or isolated tests.")

(defvar mevedel-journal-cleanup--pending (make-hash-table :test #'equal)
  "Coalesced (TIMER . FORCE) cleanup requests keyed by workspace roots.")

;;;###autoload
(defun mevedel-journal-cleanup-schedule (workspace &optional force)
  "Schedule WORKSPACE cleanup at idle, bypassing the hourly gate with FORCE."
  (unless mevedel-journal-cleanup--inhibit-scheduling
    (let* ((root (mevedel-workspace-root workspace))
           (old (gethash root mevedel-journal-cleanup--pending)))
      (setq force (or force (cdr old)))
      (when (timerp (car old)) (cancel-timer (car old)))
      (puthash
       root
       (cons (run-with-idle-timer
        0.1 nil
        (lambda ()
          (remhash root mevedel-journal-cleanup--pending)
          (unless mevedel-journal-cleanup--inhibit-scheduling
            (mevedel-transport-run-when-idle
             (list 'journal-cleanup root) root
             (lambda () (mevedel-journal-cleanup-expired workspace force)))))) force)
       mevedel-journal-cleanup--pending))))

(provide 'mevedel-journal-cleanup)
;;; mevedel-journal-cleanup.el ends here
