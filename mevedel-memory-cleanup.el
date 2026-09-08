;;; mevedel-memory-cleanup.el -- Memory retention dependencies -*- lexical-binding: t -*-

;;; Commentary:

;; Select completed memory state only after its evidence dependencies end.
;; The journal expiry transaction owns retirement and checked file deletion.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-decision)

(defun mevedel-memory-cleanup--file (workspace file expected)
  "Capture FILE in WORKSPACE only when its bytes match accepted EXPECTED hash."
  (let ((text (mevedel-session-control-fs-read-file
               (file-name-concat (mevedel-journal-store-directory (mevedel-workspace-root workspace)) file)
               'utf-8-unix (1+ mevedel-memory-store--max-bytes))))
    (unless (equal expected (secure-hash 'sha256 text)) (error "Memory expiry source changed"))
    (list :file file :sha256 expected)))

(defun mevedel-memory-cleanup--history (workspace accepted entries)
  "Capture terminal ACCEPTED pass history in WORKSPACE from public ENTRIES.
Every decision must be old and published, every proposal terminal, and every
write resolved without a target marker. Signal when dependencies remain."
  (let* ((pass (plist-get (plist-get accepted :prepared) :id))
         (cutoff (- (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace))
                    (* mevedel-journal-max-age-days 86400)))
         (latest (make-hash-table :test #'equal))
         history private related)
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'decision) (equal pass (plist-get entry :pass-id)))
        (unless (< (float-time (date-to-time (plist-get entry :created))) cutoff) (error "Memory decision is still recent"))
        (let* ((record (mevedel-memory-decision--published workspace entry))
               (claim (plist-get record :claim))
               (proposal (plist-get entry :proposal-id))
               (old (gethash proposal latest)))
          (when (> (plist-get claim :generation) (or (car old) 0))
            (puthash proposal (cons (plist-get claim :generation) (plist-get entry :status)) latest))
          (push entry history)
          (push (list :id (plist-get entry :decision-id) :file (plist-get entry :file)
                      :sha256 (secure-hash 'sha256 (plist-get entry :text))) related)
          (push (mevedel-memory-cleanup--file
                 workspace (format "state/decisions/%s.el" (plist-get entry :decision-id))
                 (plist-get (mevedel-journal-claim-outcome claim) :payload)) private))))
    (unless (cl-every (lambda (proposal) (mevedel-memory-decision-terminal-status-p (cdr (gethash (plist-get proposal :id) latest))))
                     (plist-get accepted :proposals))
      (error "Memory proposal is unresolved"))
    (dolist (path (mevedel-session-control-fs-list-directory (mevedel-memory-decision--directory workspace) mevedel-memory-write-intent-file-regexp))
      (when-let* ((record (mevedel-memory-decision--record workspace path))
                  (metadata (plist-get record :metadata))
                  ((equal pass (plist-get metadata :pass-id))))
        (unless (seq-some (lambda (entry) (equal (plist-get metadata :decision-id) (plist-get entry :decision-id))) history)
          (error "Memory decision publication is unresolved"))))
    (dolist (row (mevedel-memory-write-list workspace))
      (let ((intent (plist-get row :intent)))
        (when (or (not intent) (equal pass (plist-get intent :pass)))
          (when (or (plist-get row :error) (plist-get row :marked)
                    (not (seq-some (lambda (entry)
                                     (and (equal (plist-get row :hash) (plist-get entry :state-hash))
                                          (mevedel-memory-decision-resolved-write-status-p (plist-get entry :status)))) history)))
            (error "Memory write recovery is unresolved"))
          (push (mevedel-memory-cleanup--file workspace (format "state/writes/%s.el" (plist-get row :id)) (plist-get row :hash)) private))))
    (list :private (nreverse private) :related (vconcat (nreverse related)))))

(defun mevedel-memory-cleanup-entry (workspace entry entries)
  "Return an expiry group for an unreferenced completed review ENTRY.
ENTRIES is a fresh public journal observation in WORKSPACE. The caller holds
journal mutation and consolidation admission. Retain unresolved decision/write
dependencies; this function never deletes or publishes state."
  (let ((id (plist-get entry :pass-id)))
    (unless (seq-some (lambda (other) (and (eq (plist-get other :kind) 'digest)
                                         (member (plist-get other :id) (plist-get entry :digests)))) entries)
      (condition-case nil
          (let* ((accepted (mevedel-memory-store-accepted workspace id))
                 (review (plist-get accepted :review))
                 (prepared (plist-get accepted :prepared)))
            (when (and accepted
                       (cl-every (lambda (digest)
                                   (not (mevedel-session-control-fs-path-exists-p
                                         (mevedel-memory-store--pin workspace id (plist-get digest :id)))))
                                 (plist-get prepared :entries))
                       (equal (plist-get entry :text) (mevedel-journal-store--encode review nil 'consolidation)))
              (let ((history (mevedel-memory-cleanup--history workspace accepted entries)))
                (list :kind "consolidation" :id id :file (plist-get entry :file)
                    :scope (if (string-empty-p (plist-get review :focus)) "general" "focused")
                    :sha256 (secure-hash 'sha256 (plist-get entry :text))
                    :related (plist-get history :related)
                    :private
                    (vconcat
                     (mapcar (lambda (name)
                               (mevedel-memory-cleanup--file
                                workspace (format "state/passes/%s/%s" id name)
                                (if (equal name "prepared.el") (plist-get prepared :hash)
                                  (plist-get (mevedel-journal-claim-outcome (plist-get prepared :claim)) :payload))))
                             '("prepared.el" "accepted.el"))
                     (plist-get history :private))))))
        (error nil)))))

(provide 'mevedel-memory-cleanup)
;;; mevedel-memory-cleanup.el ends here
