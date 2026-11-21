;;; niwa-memory-cleanup.el -- Memory retention dependencies -*- lexical-binding: t -*-

;;; Commentary:

;; Select completed memory state only after its evidence dependencies end.
;; The journal expiry transaction owns retirement and checked file deletion.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-memory-decision)

;; `niwa-memory-cycle'
(declare-function niwa-memory-cycle-pins-pass-p "niwa-memory-cycle" (workspace pass))
(autoload 'niwa-memory-cycle-pins-pass-p "niwa-memory-cycle")

(defun niwa-memory-cleanup--file (workspace file expected)
  "Capture private FILE in WORKSPACE when its bytes match EXPECTED hash.
FILE is relative to the private journal state directory."
  (let ((text (niwa-session-control-fs-read-file
               (file-name-concat (niwa-journal-store-state-directory
                                  (niwa-journal-store-directory (niwa-workspace-root workspace))) file)
               'utf-8-unix (1+ niwa-memory-store--max-bytes))))
    (unless (equal expected (secure-hash 'sha256 text)) (error "Memory expiry source changed"))
    (list :file file :sha256 expected)))

(defun niwa-memory-cleanup--history (workspace accepted entries)
  "Capture published, terminal ACCEPTED pass slots in WORKSPACE from ENTRIES."
  (let ((history (niwa-memory-decision-retention workspace accepted entries)))
    (list :private (plist-get history :private)
          :related (vconcat (plist-get history :related))
          :expires (+ (plist-get history :resolved-at) (* niwa-memory-history-max-age-days 86400)))))

(defun niwa-memory-cleanup-entry (workspace entry entries)
  "Return an expiry group for an unreferenced completed review ENTRY.
ENTRIES is a fresh public journal observation in WORKSPACE. The caller holds
journal mutation and consolidation admission. Retain unresolved decision/write
dependencies; this function never deletes or publishes state."
  (let ((id (plist-get entry :pass-id)))
    (unless (and (string-empty-p (or (plist-get entry :focus) ""))
                 (seq-some (lambda (other) (and (eq (plist-get other :kind) 'digest)
                                         (member (plist-get other :id) (plist-get entry :digests)))) entries))
      (condition-case nil
          (let* (;; Public timestamps can rule out expiry, never authorize it.
                 ;; The latest decision can only extend the review's lifetime.
                 ;; Keep the authenticated history checks below for candidates.
                 (created (float-time (date-to-time (plist-get entry :created))))
                 (_ (when (> (+ created (* niwa-memory-history-max-age-days 86400))
                             (niwa-session-control-fs-target-time (niwa-workspace-root workspace)))
                      (error "Memory history is still recent")))
                 (_ (when (niwa-memory-cycle-pins-pass-p workspace id)
                      (error "Note cycle has not reconciled its accepted admission")))
                 (accepted (niwa-memory-store-accepted workspace id))
                 (review (plist-get accepted :review))
                 (prepared (plist-get accepted :prepared)))
            (when (and (niwa-memory-decision--slot-eligible-p accepted)
                       (cl-every (lambda (digest)
                                   (not (niwa-session-control-fs-path-exists-p
                                         (niwa-memory-store--pin workspace id (plist-get digest :id)))))
                                 (plist-get prepared :entries))
                       (equal (plist-get entry :text) (niwa-journal-store--encode review nil 'consolidation)))
              (let ((history (niwa-memory-cleanup--history workspace accepted entries)))
                (unless (<= (plist-get history :expires)
                            (niwa-session-control-fs-target-time (niwa-workspace-root workspace)))
                  (error "Memory history is still recent"))
                (list :kind "consolidation" :id id :file (plist-get entry :file)
                    :scope (if (string-empty-p (plist-get review :focus)) "general" "focused")
                    :journal-bearing (if (plist-get review :digests) t :false)
                    :sha256 (secure-hash 'sha256 (plist-get entry :text))
                    :related (plist-get history :related)
                    :private
                    (vconcat
                     (mapcar (lambda (name)
                               (niwa-memory-cleanup--file
                                workspace (format "passes/%s/%s" id name)
                                (if (equal name "prepared.el") (plist-get prepared :hash)
                                  (plist-get accepted :hash))))
                             '("prepared.el" "accepted.el"))
                     (plist-get history :private))))))
        (error nil)))))

(provide 'niwa-memory-cleanup)
;;; niwa-memory-cleanup.el ends here
