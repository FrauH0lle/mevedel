;;; niwa-file-state.el -- File state tracking -*- lexical-binding: t -*-

;;; Commentary:

;; File state tracking and LRU-bounded workspace file cache.
;;
;; The cache stores `niwa-file-state' entries keyed by absolute path,
;; bounded by a maximum entry count and a maximum total content size.
;; Entries are promoted to MRU on every get and put; eviction drops the
;; least recently used entries until both bounds are satisfied again.
;;
;; Per-session interaction metadata (read-turn, modified-turn) is stored
;; separately on the session `touched-files' hash-table and is updated
;; by tool handlers as a side effect.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'niwa-structs)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-sidecar-path
                  "niwa-session-artifacts" (save-path))
(autoload 'niwa-session-artifacts-sidecar-path
  "niwa-session-artifacts")

;; `niwa-turn'
(declare-function niwa-current-turn "niwa-turn" (session))
(autoload 'niwa-current-turn "niwa-turn")


;;
;;; Customization

(defcustom niwa-file-cache-max-entries 100
  "Maximum number of files kept in a workspace file cache.

When the cache exceeds this count, least recently used entries are
evicted until the limit is satisfied."
  :type 'integer
  :group 'niwa)

(defcustom niwa-file-cache-max-bytes (* 25 1024 1024)
  "Maximum total content size (bytes) kept in a workspace file cache.

When the cache exceeds this total, least recently used entries are
evicted until the limit is satisfied.  Very large single files may
cause the cache to drop all other entries; that is intentional."
  :type 'integer
  :group 'niwa)

(defcustom niwa-file-cache-max-file-bytes (* 1024 1024)
  "Largest file whose content a workspace file cache stores.

A file over this size is cached as a fingerprint only: modification
time, change time, and reported size, with no content.  External change
detection still notices it move and still reports it, just without a
diff.  Keeps one multi-megabyte log or generated file from consuming the
whole `niwa-file-cache-max-bytes' budget and evicting every entry
that describes real work."
  :type 'integer
  :group 'niwa)


;;
;;; File state factory

(defun niwa-file-state-from-file (path)
  "Read PATH from disk and return a fresh `niwa-file-state'.

Returns nil when PATH does not exist or cannot be read.  CONTENT is
captured via `insert-file-contents-literally' so encoding rewrites do
not distort byte counts.  A file the filesystem reports as larger than
`niwa-file-cache-max-file-bytes' is captured as a fingerprint: the
timestamps and reported size without CONTENT, so it can still be seen
to move without holding megabytes of the cache budget."
  (let ((expanded (expand-file-name path)))
    (when (file-readable-p expanded)
      (let* ((attrs (file-attributes expanded))
             (mtime (and attrs (file-attribute-modification-time attrs)))
             (ctime (and attrs (file-attribute-status-change-time attrs)))
             (stat-size (and attrs (file-attribute-size attrs)))
             (content (unless (and (integerp stat-size)
                                   (> stat-size
                                      niwa-file-cache-max-file-bytes))
                        (with-temp-buffer
                          (insert-file-contents-literally expanded)
                          (buffer-string)))))
        (niwa-file-state--create
         :path expanded
         :content content
         :mtime mtime
         :size (length (or content ""))
         :ctime ctime
         :stat-size stat-size)))))


;;
;;; LRU cache primitives

(defun niwa-file-cache-get (cache path)
  "Return the `niwa-file-state' for PATH in CACHE, or nil if missing.

As a side effect, PATH is promoted to the most recently used position
in CACHE when found."
  (let* ((key (expand-file-name path))
         (state (gethash key (niwa-file-cache-table cache))))
    (when state
      (setf (niwa-file-cache-order cache)
            (cons key (delete key (niwa-file-cache-order cache)))))
    state))

(defun niwa-file-cache-remove (cache path)
  "Remove PATH from CACHE, and return non-nil when a removal occurred."
  (let* ((key (expand-file-name path))
         (state (gethash key (niwa-file-cache-table cache))))
    (when state
      (remhash key (niwa-file-cache-table cache))
      (setf (niwa-file-cache-order cache)
            (delete key (niwa-file-cache-order cache)))
      (cl-decf (niwa-file-cache-total-bytes cache)
               (or (niwa-file-state-size state) 0))
      t)))

(defun niwa-file-cache--evict (cache)
  "Evict least recently used entries from CACHE until limits are satisfied.

Drops entries from the tail of CACHE's order list until both
`niwa-file-cache-max-entries' and `niwa-file-cache-max-bytes'
are respected (or CACHE is empty).  Reverses the order list once so
each eviction pops the head in O(1); total cost is O(n) per call
rather than O(n*k) when k entries are evicted."
  (let ((table (niwa-file-cache-table cache))
        (rev (nreverse (niwa-file-cache-order cache))))
    (while (and rev
                (or (> (hash-table-count table)
                       niwa-file-cache-max-entries)
                    (> (niwa-file-cache-total-bytes cache)
                       niwa-file-cache-max-bytes)))
      (let* ((victim (pop rev))
             (state (gethash victim table)))
        (when state
          (remhash victim table)
          (cl-decf (niwa-file-cache-total-bytes cache)
                   (or (niwa-file-state-size state) 0)))))
    (setf (niwa-file-cache-order cache) (nreverse rev))))

(defun niwa-file-cache-put (cache state)
  "Insert or update STATE in CACHE, promoting it to MRU.

STATE must be a `niwa-file-state' whose PATH is the cache key.
Evicts old entries as needed to respect
`niwa-file-cache-max-entries' and `niwa-file-cache-max-bytes'.
Returns STATE."
  (let* ((key (expand-file-name (niwa-file-state-path state)))
         (table (niwa-file-cache-table cache))
         (prior (gethash key table))
         (prior-size (and prior (or (niwa-file-state-size prior) 0))))
    (when prior
      (cl-decf (niwa-file-cache-total-bytes cache) prior-size))
    (puthash key state table)
    (cl-incf (niwa-file-cache-total-bytes cache)
             (or (niwa-file-state-size state) 0))
    (setf (niwa-file-cache-order cache)
          (cons key (delete key (niwa-file-cache-order cache))))
    (niwa-file-cache--evict cache)
    state))

;;
;;; External change detection

(defun niwa-file-state--stamps-equal-p (cached current)
  "Return non-nil when timestamps CACHED and CURRENT are the same instant.
Nil is not a timestamp: `time-equal-p' reads it as now, so an entry
captured without one is never called equal to anything."
  (and cached current (time-equal-p cached current)))

(defun niwa-file-state-current-p (state attrs)
  "Return non-nil when STATE still describes the file ATTRS reports.
Compares the modification time, the inode change time, and the reported
size.  A modification time alone is not evidence: restoring timestamps,
a clock stepping backwards, and a coarse-grained filesystem all leave it
unchanged.  A change time cannot be set from userland, so it moves even
when the modification time is restored."
  (and (niwa-file-state--stamps-equal-p
        (niwa-file-state-mtime state)
        (file-attribute-modification-time attrs))
       (niwa-file-state--stamps-equal-p
        (niwa-file-state-ctime state)
        (file-attribute-status-change-time attrs))
       (equal (niwa-file-state-stat-size state)
              (file-attribute-size attrs))))

(defun niwa-file-cache-detect-external-changes (cache)
  "Return external-change records for CACHE entries.

Iterates over every cached `niwa-file-state' and stats the
corresponding file on disk.  An entry is included when the on-disk
file has been modified since CACHE captured it, or when the file has
been deleted.

Each entry is a plist with these keys:
  :path    - absolute path
  :status  - `modified' or `deleted'
  :old     - cached content string (may be empty), nil when omitted
  :new     - current on-disk content string, or nil when deleted
  :content-omitted - t when the entry is a fingerprint, so no diff
                     is available for this change
  :stat-size - present with `:content-omitted', the size on disk

Does not mutate CACHE; pair with
`niwa-file-cache-consume-external-changes' after reporting so
changes are not re-reported."
  (let (changes)
    (maphash
     (lambda (path state)
       (let ((remote-file-name-inhibit-cache
              (if (file-remote-p path)
                  t
                remote-file-name-inhibit-cache)))
         (cond
          ((not (file-exists-p path))
           (push (list :path path
                       :status 'deleted
                       :old (niwa-file-state-content state)
                       :new nil)
                 changes))
          ((file-readable-p path)
           (let ((attrs (file-attributes path)))
             ;; ponytail: a rewrite that restores mtime, size, and ctime
             ;; together still reads as unchanged; that needs a content
             ;; compare, which costs a full read of every cached file.
             (unless (niwa-file-state-current-p state attrs)
               (if (null (niwa-file-state-content state))
                   ;; Fingerprint entry: the move is real, but reading
                   ;; the file back to diff it is the cost this kind of
                   ;; entry exists to avoid.
                   (push (list :path path
                               :status 'modified
                               :old nil
                               :new nil
                               :content-omitted t
                               :stat-size (file-attribute-size attrs))
                         changes)
                 (let ((new-content (with-temp-buffer
                                      (insert-file-contents-literally path)
                                      (buffer-string))))
                   (unless (equal new-content
                                  (niwa-file-state-content state))
                     (push (list :path path
                                 :status 'modified
                                 :old (niwa-file-state-content state)
                                 :new new-content)
                           changes))))))))))
     (niwa-file-cache-table cache))
    (nreverse changes)))

(defun niwa-file-cache-consume-external-changes (cache changes)
  "Update CACHE from external-change records.

CHANGES is a list returned by
`niwa-file-cache-detect-external-changes'.  Deleted files are
removed from CACHE; modified files are re-read from disk and inserted
as fresh `niwa-file-state' entries."
  (dolist (change changes)
    (let ((path (plist-get change :path))
          (status (plist-get change :status)))
      (pcase status
        ('deleted
         (niwa-file-cache-remove cache path))
        ('modified
         (when-let* ((state (niwa-file-state-from-file path)))
           (niwa-file-cache-put cache state)))))))


;;
;;; Session recording helpers

(defun niwa-session-record-interaction (session path kind turn-count
                                                   &optional offset limit)
  "Record that PATH was accessed in SESSION via KIND at TURN-COUNT.

KIND is either `read' or `modify'.  Creates or updates the
`niwa-file-interaction' entry for PATH in the session's
`touched-files' hash-table.  For `read' calls, OFFSET and LIMIT are
the range arguments and are stored to support read deduplication."
  (let* ((key (expand-file-name path))
         (table (niwa-session-touched-files session))
         (entry (or (gethash key table)
                    (niwa-file-interaction--create))))
    (pcase kind
      ('read
       (setf (niwa-file-interaction-read-turn entry) turn-count
             (niwa-file-interaction-read-offset entry) offset
             (niwa-file-interaction-read-limit entry) limit))
      ('modify (setf (niwa-file-interaction-modified-turn entry) turn-count))
      (_ (error "Unknown interaction kind: %S" kind)))
    (puthash key entry table)
    entry))

(defun niwa-session-forget-read-ranges (session)
  "Stop SESSION's earlier Reads from counting as duplicates.
Compaction removes their contents from the model's history, so a repeated
Read must return the file again.  Read turns remain for compaction notices."
  (maphash (lambda (_path entry)
             (setf (niwa-file-interaction-read-offset entry) 'compacted))
           (niwa-session-touched-files session)))

(defun niwa-session--session-directory-for (session path)
  "Return the niwa session directory containing PATH, or nil.

SESSION's own save path is the first candidate.  Sibling directories
under the same sessions root are candidates too, because reading a
second live session's files churns exactly like reading this one's --
each is confirmed by its sidecar rather than assumed, so an ordinary
directory that merely neighbours a session is not swept in.  That probe
only runs for a path already under the root and outside SESSION's own
directory, so the common case stays a string comparison."
  (when-let* ((save-path (niwa-session-save-path session))
              (own (expand-file-name save-path))
              (file (expand-file-name path)))
    (if (string-prefix-p (file-name-as-directory own) file)
        own
      (let ((root (file-name-directory (directory-file-name own))))
        (when (string-prefix-p root file)
          (when-let* ((rest (substring file (length root)))
                      (slash (string-search "/" rest))
                      (dir (substring file 0 (+ (length root) slash)))
                      ((file-exists-p
                        (niwa-session-artifacts-sidecar-path dir))))
            dir))))))

(defun niwa-session-file-cache-excluded-p (session path)
  "Return non-nil when PATH is niwa's own bookkeeping for SESSION.

A session directory holds append-only logs, transcript segments, and
sidecars that niwa rewrites every turn.  Caching one would report
niwa's own writes back to the model as external edits every turn for
the life of the session, and let a multi-megabyte telemetry log evict
every cache entry describing real work."
  (and (niwa-session--session-directory-for session path) t))

(defun niwa-session-record-file-access (session path kind
                                                    &optional offset limit)
  "Record SESSION's tool interaction with PATH of KIND.

Also refreshes the workspace file cache entry for PATH so external
change detection can diff against the freshly captured content.  KIND
is either `read' or `modify'.  For `read' calls, OFFSET and LIMIT are
the range arguments and are stored on the interaction entry.  Silently
skips the cache update when the session has no workspace, when PATH
cannot be read, or when PATH is niwa's own session bookkeeping
\(see `niwa-session-file-cache-excluded-p'); the interaction entry
is always updated.  Returns the updated `niwa-file-interaction'."
  (when-let* (((not (niwa-session-file-cache-excluded-p session path)))
              (ws (niwa-session-workspace session))
              (cache (niwa-workspace-file-cache ws))
              (state (niwa-file-state-from-file path)))
    (niwa-file-cache-put cache state))
  (niwa-session-record-interaction
   session path kind
   (if (and (niwa-request-p niwa--current-request)
            (eq session (niwa-request-session niwa--current-request)))
       (niwa-current-turn session)
     (or (niwa-session-turn-count session) 0))
   offset limit))

(defun niwa-session-read-is-duplicate-p (session path offset limit)
  "Return non-nil if the previous Read of PATH covers the same range.

A read is considered a duplicate when SESSION has a prior read
interaction for PATH with matching OFFSET and LIMIT, no tool-driven
modify has happened since that read, the workspace file cache has a
state entry for PATH, and the on-disk mtime has not advanced past the
cached mtime.  Returns nil when any of those conditions fail or when
SESSION has no workspace."
  (when-let* ((ws (niwa-session-workspace session))
              (cache (niwa-workspace-file-cache ws))
              (key (expand-file-name path))
              (entry (gethash key (niwa-session-touched-files session)))
              (read-turn (niwa-file-interaction-read-turn entry))
              ((let ((mod-turn (niwa-file-interaction-modified-turn entry)))
                 (or (null mod-turn) (< mod-turn read-turn))))
              ((equal offset (niwa-file-interaction-read-offset entry)))
              ((equal limit (niwa-file-interaction-read-limit entry)))
              (state (niwa-file-cache-get cache key))
              ;; Freshness observes the target now, not what TRAMP cached up
              ;; to ten seconds ago, and that covers the readable check too.
              (attrs (let ((remote-file-name-inhibit-cache
                            (if (file-remote-p key)
                                t
                              remote-file-name-inhibit-cache)))
                       (and (file-readable-p key)
                            (file-attributes key)))))
    (niwa-file-state-current-p state attrs)))

(provide 'niwa-file-state)
;;; niwa-file-state.el ends here
