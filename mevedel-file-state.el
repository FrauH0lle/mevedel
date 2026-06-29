;;; mevedel-file-state.el -- File state tracking -*- lexical-binding: t -*-

;;; Commentary:

;; File state tracking and LRU-bounded workspace file cache.
;;
;; The cache stores `mevedel-file-state' entries keyed by absolute path,
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

(require 'mevedel-structs)


;;
;;; Customization

(defcustom mevedel-file-cache-max-entries 100
  "Maximum number of files kept in a workspace file cache.

When the cache exceeds this count, least recently used entries are
evicted until the limit is satisfied."
  :type 'integer
  :group 'mevedel)

(defcustom mevedel-file-cache-max-bytes (* 25 1024 1024)
  "Maximum total content size (bytes) kept in a workspace file cache.

When the cache exceeds this total, least recently used entries are
evicted until the limit is satisfied.  Very large single files may
cause the cache to drop all other entries; that is intentional."
  :type 'integer
  :group 'mevedel)


;;
;;; File state factory

(defun mevedel-file-state-from-file (path)
  "Read PATH from disk and return a fresh `mevedel-file-state'.

Returns nil when PATH does not exist or cannot be read.  CONTENT is
captured via `insert-file-contents-literally' so encoding rewrites do
not distort byte counts."
  (let ((expanded (expand-file-name path)))
    (when (file-readable-p expanded)
      (let* ((attrs (file-attributes expanded))
             (mtime (and attrs (file-attribute-modification-time attrs)))
             (content (with-temp-buffer
                        (insert-file-contents-literally expanded)
                        (buffer-string))))
        (mevedel-file-state--create
         :path expanded
         :content content
         :mtime mtime
         :size (length content))))))


;;
;;; LRU cache primitives

(defun mevedel-file-cache-create ()
  "Return a fresh empty `mevedel-file-cache' struct."
  (mevedel-file-cache--create
   :table (make-hash-table :test #'equal)
   :order nil
   :total-bytes 0))

(defun mevedel-file-cache-get (cache path)
  "Return the `mevedel-file-state' for PATH in CACHE, or nil if missing.

As a side effect, PATH is promoted to the most recently used position
in CACHE when found."
  (let* ((key (expand-file-name path))
         (state (gethash key (mevedel-file-cache-table cache))))
    (when state
      (setf (mevedel-file-cache-order cache)
            (cons key (delete key (mevedel-file-cache-order cache)))))
    state))

(defun mevedel-file-cache-remove (cache path)
  "Remove PATH from CACHE, and return non-nil when a removal occurred."
  (let* ((key (expand-file-name path))
         (state (gethash key (mevedel-file-cache-table cache))))
    (when state
      (remhash key (mevedel-file-cache-table cache))
      (setf (mevedel-file-cache-order cache)
            (delete key (mevedel-file-cache-order cache)))
      (cl-decf (mevedel-file-cache-total-bytes cache)
               (or (mevedel-file-state-size state) 0))
      t)))

(defun mevedel-file-cache--evict (cache)
  "Evict least recently used entries from CACHE until limits are satisfied.

Drops entries from the tail of CACHE's order list until both
`mevedel-file-cache-max-entries' and `mevedel-file-cache-max-bytes'
are respected (or CACHE is empty).  Reverses the order list once so
each eviction pops the head in O(1); total cost is O(n) per call
rather than O(n*k) when k entries are evicted."
  (let ((table (mevedel-file-cache-table cache))
        (rev (nreverse (mevedel-file-cache-order cache))))
    (while (and rev
                (or (> (hash-table-count table)
                       mevedel-file-cache-max-entries)
                    (> (mevedel-file-cache-total-bytes cache)
                       mevedel-file-cache-max-bytes)))
      (let* ((victim (pop rev))
             (state (gethash victim table)))
        (when state
          (remhash victim table)
          (cl-decf (mevedel-file-cache-total-bytes cache)
                   (or (mevedel-file-state-size state) 0)))))
    (setf (mevedel-file-cache-order cache) (nreverse rev))))

(defun mevedel-file-cache-put (cache state)
  "Insert or update STATE in CACHE, promoting it to MRU.

STATE must be a `mevedel-file-state' whose PATH is the cache key.
Evicts old entries as needed to respect
`mevedel-file-cache-max-entries' and `mevedel-file-cache-max-bytes'.
Returns STATE."
  (let* ((key (expand-file-name (mevedel-file-state-path state)))
         (table (mevedel-file-cache-table cache))
         (prior (gethash key table))
         (prior-size (and prior (or (mevedel-file-state-size prior) 0))))
    (when prior
      (cl-decf (mevedel-file-cache-total-bytes cache) prior-size))
    (puthash key state table)
    (cl-incf (mevedel-file-cache-total-bytes cache)
             (or (mevedel-file-state-size state) 0))
    (setf (mevedel-file-cache-order cache)
          (cons key (delete key (mevedel-file-cache-order cache))))
    (mevedel-file-cache--evict cache)
    state))

(defun mevedel-file-cache-clear (cache)
  "Remove all entries from CACHE."
  (clrhash (mevedel-file-cache-table cache))
  (setf (mevedel-file-cache-order cache) nil
        (mevedel-file-cache-total-bytes cache) 0))


;;
;;; External change detection

(defun mevedel-file-cache-detect-external-changes (cache)
  "Return external-change records for CACHE entries.

Iterates over every cached `mevedel-file-state' and stats the
corresponding file on disk.  An entry is included when the on-disk
file has been modified since CACHE captured it, or when the file has
been deleted.

Each entry is a plist with these keys:
  :path    - absolute path
  :status  - `modified' or `deleted'
  :old     - cached content string (may be empty)
  :new     - current on-disk content string, or nil when deleted

Does not mutate CACHE; pair with
`mevedel-file-cache-consume-external-changes' after reporting so
changes are not re-reported."
  (let (changes)
    (maphash
     (lambda (path state)
       (cond
        ((not (file-exists-p path))
         (push (list :path path
                     :status 'deleted
                     :old (mevedel-file-state-content state)
                     :new nil)
               changes))
        ((file-readable-p path)
         (let* ((current-mtime (file-attribute-modification-time
                                (file-attributes path)))
                (cached-mtime (mevedel-file-state-mtime state)))
           (when (and current-mtime cached-mtime
                      (time-less-p cached-mtime current-mtime))
             (let ((new-content (with-temp-buffer
                                  (insert-file-contents-literally path)
                                  (buffer-string))))
               (unless (equal new-content
                              (mevedel-file-state-content state))
                 (push (list :path path
                             :status 'modified
                             :old (mevedel-file-state-content state)
                             :new new-content)
                       changes))))))))
     (mevedel-file-cache-table cache))
    (nreverse changes)))

(defun mevedel-file-cache-consume-external-changes (cache changes)
  "Update CACHE from external-change records.

CHANGES is a list returned by
`mevedel-file-cache-detect-external-changes'.  Deleted files are
removed from CACHE; modified files are re-read from disk and inserted
as fresh `mevedel-file-state' entries."
  (dolist (change changes)
    (let ((path (plist-get change :path))
          (status (plist-get change :status)))
      (pcase status
        ('deleted
         (mevedel-file-cache-remove cache path))
        ('modified
         (when-let* ((state (mevedel-file-state-from-file path)))
           (mevedel-file-cache-put cache state)))))))


;;
;;; Session recording helpers

(defun mevedel-session-record-interaction (session path kind turn-count
                                                   &optional offset limit)
  "Record that PATH was accessed in SESSION via KIND at TURN-COUNT.

KIND is either `read' or `modify'.  Creates or updates the
`mevedel-file-interaction' entry for PATH in the session's
`touched-files' hash-table.  For `read' calls, OFFSET and LIMIT are
the range arguments and are stored to support read deduplication."
  (let* ((key (expand-file-name path))
         (table (mevedel-session-touched-files session))
         (entry (or (gethash key table)
                    (mevedel-file-interaction--create :path key))))
    (pcase kind
      ('read
       (setf (mevedel-file-interaction-read-turn entry) turn-count
             (mevedel-file-interaction-read-offset entry) offset
             (mevedel-file-interaction-read-limit entry) limit))
      ('modify (setf (mevedel-file-interaction-modified-turn entry) turn-count))
      (_ (error "Unknown interaction kind: %S" kind)))
    (puthash key entry table)
    entry))

(defun mevedel-session-record-file-access (session path kind
                                                    &optional offset limit)
  "Record SESSION's tool interaction with PATH of KIND.

Also refreshes the workspace file cache entry for PATH so external
change detection can diff against the freshly captured content.  KIND
is either `read' or `modify'.  For `read' calls, OFFSET and LIMIT are
the range arguments and are stored on the interaction entry.  Silently
skips the cache update when the session has no workspace or PATH
cannot be read; the interaction entry is always updated.  Returns the
updated `mevedel-file-interaction'."
  (when-let* ((ws (mevedel-session-workspace session))
              (cache (mevedel-workspace-file-cache ws))
              (state (mevedel-file-state-from-file path)))
    (mevedel-file-cache-put cache state))
  (mevedel-session-record-interaction
   session path kind (mevedel-session-turn-count session) offset limit))

(defun mevedel-session-read-is-duplicate-p (session path offset limit)
  "Return non-nil if the previous Read of PATH covers the same range.

A read is considered a duplicate when SESSION has a prior read
interaction for PATH with matching OFFSET and LIMIT, no tool-driven
modify has happened since that read, the workspace file cache has a
state entry for PATH, and the on-disk mtime has not advanced past the
cached mtime.  Returns nil when any of those conditions fail or when
SESSION has no workspace."
  (when-let* ((ws (mevedel-session-workspace session))
              (cache (mevedel-workspace-file-cache ws))
              (key (expand-file-name path))
              (entry (gethash key (mevedel-session-touched-files session)))
              (read-turn (mevedel-file-interaction-read-turn entry))
              ((let ((mod-turn (mevedel-file-interaction-modified-turn entry)))
                 (or (null mod-turn) (< mod-turn read-turn))))
              ((equal offset (mevedel-file-interaction-read-offset entry)))
              ((equal limit (mevedel-file-interaction-read-limit entry)))
              (state (gethash key (mevedel-file-cache-table cache)))
              (cached-mtime (mevedel-file-state-mtime state))
              ((file-readable-p key))
              (current-mtime (file-attribute-modification-time
                              (file-attributes key))))
    (not (time-less-p cached-mtime current-mtime))))

(provide 'mevedel-file-state)
;;; mevedel-file-state.el ends here
