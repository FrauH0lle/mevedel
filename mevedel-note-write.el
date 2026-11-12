;;; mevedel-note-write.el -- Model writes workspace notes -*- lexical-binding: t -*-

;;; Commentary:

;; Model-facing note operations reuse source commands and sync once per batch.

;;; Code:

(require 'cl-lib)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-permissions)
(require 'mevedel-session-artifacts)

(defun mevedel-tool-note-write (args)
  "Perform the workspace note operations in ARGS."
  (let* ((session (or mevedel--session (error "Notes require a session")))
         (workspace (mevedel-session-workspace session))
         (operations (append (plist-get args :operations) nil))
         results)
    (unless operations (error "Choose at least one note operation"))
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-instruction-store-observe workspace)
    (dolist (operation operations)
      (unless (equal "create" (plist-get operation :action))
        (error "Unknown note action: %s" (plist-get operation :action)))
      (let* ((file (expand-file-name (or (plist-get operation :file_path)
                                       (error "Create needs file_path"))
                                   (mevedel-workspace-root workspace)))
             (context (mevedel-permission--invocation-context
                       :tool-name "Read" :session session :workspace workspace
                       :request mevedel--current-request :invocation mevedel--agent-invocation
                       :buffer (current-buffer) :path file
                       :workspace-root (mevedel-workspace-root workspace)))
             (start (plist-get operation :start_line))
             (end (or (plist-get operation :end_line) start)))
        (unless (and (file-in-directory-p file (mevedel-workspace-root workspace))
                     (file-regular-p file)
                     (eq 'allow (mevedel-check-permission "Read" :normalized-context context)))
          (error "Note source is not readable in this workspace"))
        (unless (and (integerp start) (> start 0) (integerp end) (>= end start))
          (error "Invalid note line range"))
        (with-current-buffer (find-file-noselect file)
          (setq-local mevedel--workspace workspace)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (unless (zerop (forward-line (1- start))) (error "Note line is beyond source"))
              (let ((beg (point)))
                (unless (zerop (forward-line (- end start))) (error "Note line is beyond source"))
                (let ((transient-mark-mode t) (mark-active t))
                  (push-mark beg t t)
                  (goto-char (line-end-position))
                  (let ((note (mevedel-note-create (plist-get operation :text) :defer-write t)))
                    (overlay-put note 'mevedel-note-author 'agent)
                    (when (plist-member operation :tags)
                      (mevedel-note-set-tags (mapcar #'intern (append (plist-get operation :tags) nil)) note))
                    (mevedel-notes--render note)
                    (push (format "Note #%s notes://%s.md" (overlay-get note 'mevedel-id)
                                  (overlay-get note 'mevedel-uuid)) results)))))))))
    (mevedel-instruction-store-sync workspace)
    (list :result (mapconcat #'identity (nreverse results) "\n"))))

(provide 'mevedel-note-write)
;;; mevedel-note-write.el ends here
