;;; niwa-memory-write.el -- Checked memory row mutations -*- lexical-binding: t -*-

;;; Commentary:

;; Derive exact before/after rows and verify them on the original target.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-change)
(require 'niwa-memory-apply)
(require 'niwa-memory-store)

;; `niwa-tool-patch'
(declare-function niwa-tool-patch-missing-parent-directories "niwa-tool-patch" (path))
;; `niwa-memory-notes'
(declare-function niwa-memory-notes-control-directory "niwa-memory-notes" (directory))
(declare-function niwa-memory-notes-same-p "niwa-memory-notes" (left right))
(autoload 'niwa-memory-notes-control-directory "niwa-memory-notes")
(autoload 'niwa-memory-notes-same-p "niwa-memory-notes")

(defun niwa-memory-write-control-directory (directory)
  "Return shared write coordination for the original physical DIRECTORY.
Standard .niwa/memory roots use their sibling state/memory-write.
The workspace's shared notes use the sibling state/shared-notes, outside the
folder they clean.  Other memory and instruction roots use
.niwa/state/memory-write/root inside that root.  The location never
depends on the calling workspace."
  (let* ((directory (directory-file-name directory))
         (parent (file-name-directory directory)))
    (cond
     ((and (equal (file-name-nondirectory directory) "memory")
           (equal (file-name-nondirectory (directory-file-name parent)) ".niwa"))
      (file-name-concat parent "state" "memory-write"))
     ((niwa-memory-notes-control-directory directory))
     (t (file-name-concat directory ".niwa" "state" "memory-write" "root")))))

(defun niwa-memory-write--target (scope root)
  "Return the coordination directory at original ROOT in SCOPE."
  (niwa-memory-write-control-directory
   (plist-get (niwa-memory-scope--root scope root) :dir)))

(defun niwa-memory-write--after (before changes)
  "Derive complete after-state from BEFORE and prepared CHANGES.
A change carrying an exact :after snapshot supplies it directly."
  (mapcar
   (lambda (row)
     (let* ((original (cdr row))
            (change (seq-find (lambda (change) (equal (plist-get original :path) (plist-get change :path))) changes)))
       (cons (car row)
             (cond ((not change) original)
                   ((plist-member change :after) (plist-get change :after))
                   ((eq (plist-get change :action) 'delete)
                    (list :path (plist-get change :path) :exists nil))
                   (t (list :path (plist-get change :path) :exists t :bytes (plist-get change :bytes)
                            :hash (secure-hash 'sha256 (plist-get change :bytes)) :mode (plist-get change :mode)))))))
   before))

(defun niwa-memory-write--before-p (proposal before)
  "Whether BEFORE preserves PROPOSAL's captured targets and non-index bytes.
The index may incorporate unrelated edits to other entries or prose. Its
complete bounded snapshot is retained in the decision slot."
  (and (proper-list-p before)
       (equal (mapcar #'car before) (mapcar #'car (plist-get proposal :before)))
       (cl-every
        (lambda (row)
          (let ((original (cdr (assoc (car row) (plist-get proposal :before))))
                (snapshot (cdr row)))
            (or (equal snapshot original)
                (and (equal (car row) "MEMORY.md")
                     (equal (plist-get snapshot :path) (plist-get original :path))
                     (or (equal snapshot (list :path (plist-get original :path) :exists nil))
                         (and (eq (plist-get snapshot :exists) t)
                              (stringp (plist-get snapshot :bytes))
                              (<= (string-bytes (plist-get snapshot :bytes)) niwa-memory-scope--max-file-bytes)
                              (equal (secure-hash 'sha256 (plist-get snapshot :bytes)) (plist-get snapshot :hash))
                              (integerp (plist-get snapshot :mode))))
                     (condition-case nil
                         (not (niwa-memory-apply--index-conflicts original snapshot proposal))
                       (error nil)))))) before)))

(defun niwa-memory-write--plan (scope item observed preimage &optional retained)
  "Classify OBSERVED rows for ITEM in SCOPE, without mutating any file.
PREIMAGE may supply an authenticated rebased index.  RETAINED, for reversal,
is the original applied record; its complete index must not be rebased.
Return before/after and observed rows, or stale with the names that changed."
  (let* ((before (copy-tree (or (plist-get retained :before) (plist-get item :before))))
         (root (cdr (assoc (plist-get item :root) (plist-get scope :roots))))
         (note (eq (plist-get root :kind) 'notes))
         (same (if note #'niwa-memory-notes-same-p #'niwa-change-same-p))
         (index (assoc "MEMORY.md" before))
         (current (cdr (assoc "MEMORY.md" observed)))
         after conflicts stale)
    (unless (equal (mapcar #'car before) (mapcar #'car observed))
      (error "Incomplete memory target observation"))
    (cl-labels ((derive ()
                  (let ((proposal (copy-sequence item)))
                    (setf (plist-get proposal :before) before)
                    (niwa-memory-write--after
                     before (niwa-memory-apply--changes root proposal (plist-get scope :new-file-mode))))))
      (when (and index (not retained))
        (cond (preimage
               (setcdr index preimage)
               (unless (and (niwa-memory-write--before-p item before)
                            (funcall same current (cdr (assoc "MEMORY.md" (derive)))))
                 (error "Saved memory index preimage does not match this result")))
              ((funcall same current (cdr (assoc "MEMORY.md" (derive)))))
              ((not (setq conflicts (niwa-memory-apply--index-conflicts (cdr index) current item)))
               (setcdr index current))))
      (setq after (or (plist-get retained :after) (derive)))
      (dolist (row observed)
        (unless (or (funcall same (cdr row) (cdr (assoc (car row) before)))
                    (funcall same (cdr row) (cdr (assoc (car row) after))))
          (push (car row) stale)))
      (cond (conflicts (list :status 'stale :reason (format "MEMORY.md entries changed since capture: %s"
                                                           (string-join conflicts ", "))))
            (stale (list :status 'stale :reason (format "Files changed since capture: %s" (string-join (nreverse stale) ", "))))
            (t (list :status 'fresh :before before :after after :observed observed))))))

(defun niwa-memory-write--verify (snapshot)
  "Return pinned operations verifying the exact state of SNAPSHOT."
  (let ((path (plist-get snapshot :path)))
    (cond ((not (plist-get snapshot :exists)) (list (list :op 'absent :path path)))
          ((plist-get snapshot :directory)
           (list (list :op 'directory-p :path path) (list :op 'verify-list :path path :content "")))
          (t (list (list :op 'verify-hash :path path :content (or (plist-get snapshot :hash)
                                                                   (secure-hash 'sha256 (plist-get snapshot :bytes))))
                   (list :op 'verify-mode :path path :content (format "%o" (plist-get snapshot :mode))))))))

(defun niwa-memory-write--batch (target checks after &optional create-mode)
  "Verify CHECKS and install AFTER on TARGET, optionally enforcing CREATE-MODE.
The caller must include unchanged dependencies in CHECKS."
  (let ((lock (process-get target 'lock)) operations dirs)
    (dolist (snapshot after)
      (when (plist-get snapshot :exists)
        (let ((path (plist-get snapshot :path)))
          (when create-mode
            (niwa-memory-apply-check-new-mode
             (seq-find (lambda (row) (equal path (plist-get row :path))) checks) snapshot))
          (setq dirs (append dirs (niwa-tool-patch-missing-parent-directories path))))))
    (setq operations
          (append
           (list (list :op 'begin-files :path lock))
           (mapcar (lambda (dir) (list :op 'mkdir :path dir)) (delete-dups dirs))
           (cl-mapcan #'niwa-memory-write--verify checks)
           (mapcar (lambda (snapshot)
                     (let ((path (plist-get snapshot :path)))
                       (if (not (plist-get snapshot :exists)) (list :op 'delete-file :path path)
                         (list :op 'write-mode :path path :coding 'no-conversion
                               :content (concat (format "%o\n" (plist-get snapshot :mode)) (plist-get snapshot :bytes)))))) after)
           (list (list :op 'commit-files :path lock))))
    (mapc #'niwa-session-control-fs-program-value
          (niwa-session-control-fs-lock-session-run target operations))))

(defun niwa-memory-write--changes (before after)
  "Prepare exact changes between retained BEFORE and AFTER snapshots.
Only durable memory and instruction writes can be reversed."
  (let (changes)
    ;; Publish the index last: a killed batch may leave an unindexed topic,
    ;; but must not advertise a topic it has not installed yet.
    (dolist (row (append (assoc-delete-all "MEMORY.md" (copy-sequence before) #'equal)
                         (when-let* ((index (assoc "MEMORY.md" before))) (list index)))
                 (nreverse changes))
      (let* ((old (cdr row)) (new (cdr (assoc (car row) after))) (path (plist-get old :path)))
        (unless (and new (equal path (plist-get new :path))) (error "Memory snapshot targets differ"))
        (unless (and (niwa-change-same-p old new)
                     (equal (plist-get old :hash) (plist-get new :hash))
                     (equal (plist-get old :directory) (plist-get new :directory)))
          (push (append (list :path path :before old :action (if (plist-get new :exists) 'write 'delete)
                              :after new)
                        (when (and (plist-get new :exists) (plist-get new :bytes))
                          (list :bytes (plist-get new :bytes) :mode (plist-get new :mode)
                                :content (decode-coding-string (plist-get new :bytes) 'utf-8-unix)))) changes))))))

(provide 'niwa-memory-write)
;;; niwa-memory-write.el ends here
