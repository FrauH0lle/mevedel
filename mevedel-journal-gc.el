;;; mevedel-journal-gc.el --- Bounded journal coordination collection -*- lexical-binding: t -*-

;;; Commentary:
;; Collect obsolete control records under the workspace's existing cleanup
;; claims.  Domain records supply retained claim references; claim pruning owns
;; the locked deletion and generation anchor.  Unreadable references stop GC.

;;; Code:
(eval-when-compile (require 'cl-lib))
(require 'generator)
(require 'mevedel-memory-store)
(require 'mevedel-memory-write)

(defun mevedel-journal-gc--empty (directory)
  "Remove DIRECTORY only if empty, returning non-nil on success."
  (eq 'ok (plist-get (car (mevedel-session-control-fs-run-program
                          (list (list :op 'delete-empty-directory :path directory)))) :status)))

(defun mevedel-journal-gc--remove (files lock)
  "Delete observed FILES under LOCK after verifying all exact bytes.
FILES is an alist of absolute paths and their complete contents."
  (mapc #'mevedel-session-control-fs-program-value
        (mevedel-session-control-fs-run-program
         (append (mapcar (lambda (file) (list :op 'verify :path (car file) :content (cdr file))) files)
                 (mapcar (lambda (file) (list :op 'delete-file :path (car file))) files)) lock)))

(iter-defun mevedel-journal-gc--manifests (root now limit)
  "Yield while removing ROOT manifests whose claims expired by NOW.
Remove at most LIMIT groups; return protected generations and removal count."
  (let ((directory (file-name-concat (mevedel-journal-store-state-directory root) "expiry")) (removed 0) protected)
    (dolist (path (mevedel-session-control-fs-list-directory directory "\\.json\\'"))
      (iter-yield nil)
      (let* ((manifest (mevedel-journal-cleanup--read root path))
             (token (plist-get manifest :token))
             (done (concat path ".done"))
             (finished (mevedel-session-control-fs-path-exists-p done))
             (outcome (mevedel-journal-claim-outcome token)))
        (if (and (< removed limit) (<= (plist-get token :expires-at) now)
                 (or finished (memq (plist-get outcome :status) '(failed cancelled expired))
                     (and (eq 'completed (plist-get outcome :status))
                          (string-empty-p (plist-get outcome :payload)))))
            (progn
              (when finished
                (unless (and (eq 'completed (plist-get outcome :status))
                             (equal (plist-get manifest :hash) (plist-get outcome :payload)))
                  (error "Journal expiry has no accepted proof")))
              (unless (or (not finished) (equal (plist-get manifest :hash) (mevedel-session-control-fs-read-file done)))
                (error "Journal expiry completion changed"))
              (mevedel-journal-gc--remove
               (append (list (cons path (mevedel-session-control-fs-read-file path)))
                       (when finished (list (cons done (plist-get manifest :hash))))) (plist-get token :directory))
              (cl-incf removed))
          (push (plist-get token :generation) protected))))
    ;; If interruption followed manifest removal, only its harmless receipt
    ;; remains.  It cannot authorize publication or deletion on its own.
    (dolist (path (mevedel-session-control-fs-list-directory directory "\\.json\\.done\\'"))
      (iter-yield nil)
      (when (and (< removed limit)
                 (not (mevedel-session-control-fs-path-exists-p (string-remove-suffix ".done" path))))
        (mevedel-journal-gc--remove (list (cons path (mevedel-session-control-fs-read-file path)))
                                   (mevedel-journal-store-claim-directory root 'mutation))
        (cl-incf removed)))
    (list :protected protected :removed removed)))

(iter-defun mevedel-journal-gc--passes (workspace mutation now limit)
  "Yield while retiring WORKSPACE preparations under MUTATION at NOW.
Return the number of removed preparations, at most LIMIT per opportunity."
  (let ((removed 0)
        (cutoff (- now (* mevedel-memory-history-max-age-days 86400))))
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "passes")
                        mevedel-journal-store-id-regexp))
      (iter-yield nil)
      (let ((id (file-name-nondirectory directory)))
        (when (and (< removed limit)
                   (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) id))
                   (mevedel-session-control-fs-path-exists-p (file-name-concat directory "prepared.el")))
          (let* ((prepared (mevedel-memory-store-read workspace id))
                 (claim (plist-get prepared :claim))
                 (outcome (mevedel-journal-claim-outcome claim)))
            (when (and (memq (plist-get outcome :status) '(failed cancelled expired))
                       (<= (plist-get claim :expires-at) cutoff))
              (mevedel-memory-store--release-pins workspace prepared nil mutation)
              (let (files)
                ;; Remove the preparation last so interrupted deletion keeps
                ;; the authority and deadline needed to finish collection.
                (dolist (name '("accepted.el" "prepared.el"))
                  (iter-yield nil)
                  (let ((path (file-name-concat directory name)))
                    (when (mevedel-session-control-fs-path-exists-p path)
                      (push (cons path (mevedel-session-control-fs-read-file path)) files))))
                (mevedel-memory-store--assert-owned mutation)
                (mevedel-journal-gc--remove (nreverse files) (plist-get mutation :directory))
                (cl-incf removed)))))
        (iter-yield nil)
        (when (and (< removed limit) (mevedel-journal-gc--empty directory))
          (cl-incf removed))))
    removed))

(iter-defun mevedel-journal-gc--references (root)
  "Yield while collecting claim generations referenced by ROOT records.
Read all remaining records, including partially retired groups.  Refuse broken
references instead of guessing that their claim can be removed."
  (let (files protected)
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-state-directory root) "passes") mevedel-journal-store-id-regexp))
      (iter-yield nil)
      (let ((path (file-name-concat directory "prepared.el")))
        (when (mevedel-session-control-fs-path-exists-p path) (push path files))))
    (dolist (kind '("decisions" "writes"))
      (iter-yield nil)
      (setq files (append files (mevedel-session-control-fs-list-directory
                                 (file-name-concat (mevedel-journal-store-state-directory root) kind) mevedel-memory-write-intent-file-regexp))))
    (dolist (path files)
      (iter-yield nil)
      (let* ((data (car (mevedel-memory-store--read-lisp path)))
             (claim (plist-get data :claim))
             (generation (plist-get claim :generation)))
        (unless (and (integerp generation) (> generation 0)
                     (mevedel-journal-store-id-p (plist-get claim :owner)))
          (error "Unreadable retained claim reference: %s" path))
        (push generation protected)))
    (delete-dups protected)))

(iter-defun mevedel-journal-gc (workspace mutation)
  "Yield while collecting WORKSPACE state under live MUTATION ownership.
The caller also holds digest and consolidation admission.  Return progress
and the remaining content budget.  Delete at most 200 claim pairs and 50
content groups across all directories in one batch."
  (let* ((root (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
         (now (mevedel-session-control-fs-target-time (plist-get mutation :directory)))
         (manifests (iter-yield-from (mevedel-journal-gc--manifests root now 50)))
         (groups (plist-get manifests :removed))
         (groups (+ groups (iter-yield-from (mevedel-journal-gc--passes workspace mutation now (- 50 groups)))))
         (removed groups)
         (references (iter-yield-from (mevedel-journal-gc--references root)))
         (remaining 200)
         (directories (list (cons (mevedel-journal-store-claim-directory root 'mutation) (plist-get manifests :protected))
                            (cons (mevedel-journal-store-claim-directory root 'digest-run) nil)
                            (cons (mevedel-journal-store-claim-directory root 'consolidation) references))))
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-state-directory root) "captures") mevedel-journal-store-id-regexp))
      (iter-yield nil)
      (push (cons (file-name-concat directory "attempts") nil) directories))
    (dolist (memory (mevedel-system--memory-roots workspace))
      (iter-yield nil)
      (let ((control (mevedel-memory-write-control-directory
                      (file-truename (plist-get memory :dir)))))
        ;; Pending markers may belong to another workspace.  Keep their
        ;; target history until that workspace has finished recovery.
        (when (and (mevedel-session-control-fs-directory-p control)
                   (not (mevedel-session-control-fs-list-directory (file-name-concat control "pending") "\\`[^.]")))
          (push (cons (file-name-concat control "claims") nil) directories))))
    (dolist (entry directories)
      (iter-yield nil)
      (when (and (> remaining 0) (mevedel-session-control-fs-directory-p (car entry)))
        (mevedel-memory-store--assert-owned mutation)
        (let ((count (iter-yield-from (mevedel-journal-claim-prune (car entry) (cdr entry) remaining))))
          (cl-incf removed count)
          (cl-decf remaining count))))
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-state-directory root) "evidence-pins") mevedel-journal-store-id-regexp))
      (iter-yield nil)
      (when (and (< groups 50) (mevedel-journal-gc--empty directory))
        (cl-incf groups)
        (cl-incf removed)))
    (list :progress removed :remaining (- 50 groups))))

(provide 'mevedel-journal-gc)
;;; mevedel-journal-gc.el ends here
