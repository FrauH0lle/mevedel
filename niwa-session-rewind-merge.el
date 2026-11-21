;;; niwa-session-rewind-merge.el --- Resolve Rewind conflicts -*- lexical-binding: t -*-

;;; Commentary:

;; Resolve discarded turns newest first without writing files.  The ordinary
;; Rewind transaction commits only after every intermediate conflict is resolved.

;;; Code:

(require 'niwa-session-rewind)
(require 'niwa-note-merge)

(defvar niwa-session-artifacts-backup-cache)

(defun niwa-session-rewind-merge-open (buffer target boundary plan resolutions)
  "Resolve BUFFER's Rewind PLAN for TARGET and BOUNDARY without source writes.
RESOLUTIONS holds already resolved newer turns.  Confirmation resumes Rewind."
  (let* ((session (buffer-local-value 'niwa--session buffer))
         (cache niwa-session-artifacts-backup-cache)
         (tick (with-current-buffer buffer (buffer-chars-modified-tick)))
         (history (copy-tree (niwa-session-file-snapshots session)))
         (turn (plist-get target :cum-turn))
         (prepared (niwa-session-rewind--prepare-buffers-for-restore
                    session turn plan "Rewind merge" (not (eq boundary 'after))
                    (lambda () (niwa-session-rewind--revert-plan
                                session turn (not (eq boundary 'after)) resolutions))))
         sections)
    (unless (eq prepared :abort)
      (dolist (entry prepared)
        (when (plist-get entry :conflicts)
          (let* ((path (plist-get entry :path))
                 (disk (niwa-tool-patch--snapshot path))
                 (source (find-buffer-visiting path))
                 (coding (if source (buffer-local-value 'buffer-file-coding-system source)
                           (if (plist-get disk :exists)
                               (cdr (niwa-tool-patch--read-file path t)) 'utf-8-unix))))
            (push (list :path path :disk disk :coding coding :data entry
                        :source-yours (niwa-note-merge-current-text path coding)
                        :yours (decode-coding-string (or (plist-get (plist-get entry :current) :bytes) "") coding)
                        :seen (decode-coding-string (or (plist-get (plist-get entry :base) :bytes) "") coding)
                        :proposed (decode-coding-string (or (plist-get (plist-get entry :proposed) :bytes) "") coding)
                        :change (list :action (unless (and (plist-get (plist-get entry :proposed) :exists)
                                                          (plist-get (plist-get entry :current) :exists))
                                                'delete)))
                  sections))))
      (if (null sections)
          (let ((niwa-session-rewind--merge-resolutions resolutions))
            (niwa-session-rewind-rewind buffer target boundary))
        (let ((merge
               (niwa-note-merge-open
                "*Rewind merge*" (nreverse sections)
                (lambda (changes)
                  (unless (and (buffer-live-p buffer)
                               (= tick (with-current-buffer buffer (buffer-chars-modified-tick)))
                               (equal history (niwa-session-file-snapshots session)))
                    (user-error "Conversation changed; restart Rewind"))
                  (pcase-dolist (`(,change . ,entry) changes)
                    (let ((path (plist-get change :path)))
                      (push (cons (cons path (plist-get entry :turn))
                                  (list :before (plist-get entry :current)
                                        :after (list :path path :exists (not (eq (plist-get change :action) 'delete))
                                                     :bytes (unless (eq (plist-get change :action) 'delete)
                                                              (plist-get change :bytes)))))
                            resolutions)))
                  (niwa-note-merge-cancel)
                  (let ((niwa-session-rewind--merge-resolutions resolutions)
                        (niwa-session-artifacts-backup-cache cache))
                    (niwa-session-rewind-rewind buffer target boundary))))))
          (with-current-buffer merge
            (setq-local header-line-format
                        " Rewind: smerge upper = current · lower = before turn · C-c C-c confirm · C-c C-k cancel"))
          merge)))))

(provide 'niwa-session-rewind-merge)
;;; niwa-session-rewind-merge.el ends here
