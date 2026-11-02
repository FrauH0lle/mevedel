;;; mevedel-memory-cleanup.el -- Memory retention dependencies -*- lexical-binding: t -*-

;;; Commentary:

;; Select completed memory state only after its evidence dependencies end.
;; The journal expiry transaction owns retirement and checked file deletion.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-memory-decision)

;; `mevedel-memory-cycle'
(declare-function mevedel-memory-cycle-pins-pass-p "mevedel-memory-cycle" (workspace pass))
(autoload 'mevedel-memory-cycle-pins-pass-p "mevedel-memory-cycle")

(defun mevedel-memory-cleanup--file (workspace file expected)
  "Capture private FILE in WORKSPACE when its bytes match EXPECTED hash.
FILE is relative to the private journal state directory."
  (let ((text (mevedel-session-control-fs-read-file
               (file-name-concat (mevedel-journal-store-state-directory
                                  (mevedel-journal-store-directory (mevedel-workspace-root workspace))) file)
               'utf-8-unix (1+ mevedel-memory-store--max-bytes))))
    (unless (equal expected (secure-hash 'sha256 text)) (error "Memory expiry source changed"))
    (list :file file :sha256 expected)))

(defun mevedel-memory-cleanup--history (workspace accepted entries)
  "Capture terminal ACCEPTED pass history in WORKSPACE from public ENTRIES.
Every decision must be published, every proposal terminal, and every
write resolved without a target marker. Signal when dependencies remain."
  (let ((history (mevedel-memory-decision-retention workspace accepted entries)))
    (list :private (mapcar (lambda (file)
                             ;; Write intents are recaptured byte-exactly here.
                             (if (string-prefix-p "writes/" (plist-get file :file))
                                 (mevedel-memory-cleanup--file workspace (plist-get file :file) (plist-get file :sha256))
                               file))
                           (plist-get history :private))
          :related (vconcat (plist-get history :related))
          :expires (+ (plist-get history :resolved-at) (* mevedel-memory-history-max-age-days 86400)))))

(defun mevedel-memory-cleanup-entry (workspace entry entries)
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
                 (_ (when (> (+ created (* mevedel-memory-history-max-age-days 86400))
                             (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace)))
                      (error "Memory history is still recent")))
                 (_ (when (mevedel-memory-cycle-pins-pass-p workspace id)
                      (error "Note cycle has not reconciled its accepted admission")))
                 (accepted (mevedel-memory-store-accepted workspace id))
                 (review (plist-get accepted :review))
                 (prepared (plist-get accepted :prepared)))
            (when (and accepted
                       (cl-every (lambda (digest)
                                   (not (mevedel-session-control-fs-path-exists-p
                                         (mevedel-memory-store--pin workspace id (plist-get digest :id)))))
                                 (plist-get prepared :entries))
                       (equal (plist-get entry :text) (mevedel-journal-store--encode review nil 'consolidation)))
              (let ((history (mevedel-memory-cleanup--history workspace accepted entries)))
                (unless (<= (plist-get history :expires)
                            (mevedel-session-control-fs-target-time (mevedel-workspace-root workspace)))
                  (error "Memory history is still recent"))
                (list :kind "consolidation" :id id :file (plist-get entry :file)
                    :scope (if (string-empty-p (plist-get review :focus)) "general" "focused")
                    :journal-bearing (if (plist-get review :digests) t :false)
                    :sha256 (secure-hash 'sha256 (plist-get entry :text))
                    :related (plist-get history :related)
                    :private
                    (vconcat
                     (mapcar (lambda (name)
                               (mevedel-memory-cleanup--file
                                workspace (format "passes/%s/%s" id name)
                                (if (equal name "prepared.el") (plist-get prepared :hash)
                                  (plist-get accepted :hash))))
                             '("prepared.el" "accepted.el"))
                     (plist-get history :private))))))
        (error nil)))))

(provide 'mevedel-memory-cleanup)
;;; mevedel-memory-cleanup.el ends here
