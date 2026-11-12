;;; mevedel-note-write.el -- Model writes workspace notes -*- lexical-binding: t -*-

;;; Commentary:

;; Model-facing note operations reuse source commands and sync once per batch.

;;; Code:

(require 'cl-lib)
(require 'mevedel-notes)
(require 'mevedel-persistence)
(require 'mevedel-permissions)
(require 'mevedel-session-artifacts)
(require 'mevedel-agents)
(require 'mevedel-presets)
(require 'mevedel-note-change)

(defun mevedel-note-write--check (args session)
  "Refuse ARGS' existing-note operations for read-only SESSION owners."
  (when (and (or (mevedel-plan-read-only-p session)
                 (and mevedel--agent-invocation
                      (mevedel-agent-invocation-agent mevedel--agent-invocation)
                      (not (mevedel-agent--specs-contain-tool-p
                            (mevedel-agent--effective-specs
                             (mevedel-agent-invocation-agent mevedel--agent-invocation))
                            "ApplyPatch"))))
             (cl-some (lambda (operation)
                        (not (equal "create" (plist-get operation :action))))
                      (append (plist-get args :operations) nil)))
    (signal 'mevedel-permission-denied
            '("Read-only conversations may create notes, not change existing notes"))))

(defun mevedel-tool-note-check (_tool args)
  "Authorize note ARGS while preserving read-only restrictions."
  (unless mevedel--session (error "Notes require a session"))
  (mevedel-note-write--check args mevedel--session)
  'allow)

(defun mevedel-note-write--source (file session)
  "Authorize reading FILE in SESSION's workspace and return its absolute path."
  (let* ((workspace (mevedel-session-workspace session))
         (root (mevedel-workspace-root workspace))
         (path (expand-file-name (or file (error "Create needs file_path")) root))
         (context (mevedel-permission--invocation-context
                   :tool-name "Read" :session session :workspace workspace
                   :request mevedel--current-request :invocation mevedel--agent-invocation
                   :buffer (current-buffer) :path path :workspace-root root)))
    (unless (and (file-in-directory-p path root)
                 (eq 'allow (mevedel-check-permission "Read" :normalized-context context)))
      (error "Note source is not readable in this workspace"))
    path))

(defun mevedel-note-write--create (operation session)
  "Create OPERATION's note in SESSION without syncing the store."
  (let* ((file (mevedel-note-write--source (plist-get operation :file_path) session))
         (start (plist-get operation :start_line))
         (end (or (plist-get operation :end_line) start)))
    (unless (file-regular-p file) (error "Notes need an existing source file"))
    (unless (and (integerp start) (> start 0) (integerp end) (>= end start))
      (error "Invalid note line range"))
    (with-current-buffer (find-file-noselect file)
      (when (and mevedel--workspace
                 (not (equal (mevedel--instruction-workspace-key mevedel--workspace)
                             (mevedel--instruction-workspace-key
                              (mevedel-session-workspace session)))))
        (error "Note source belongs to a different workspace"))
      (setq-local mevedel--workspace (mevedel-session-workspace session))
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (unless (zerop (forward-line (1- start))) (error "Note line is beyond source"))
          (let ((beg (point)))
            (unless (zerop (forward-line (- end start))) (error "Note line is beyond source"))
            (let ((transient-mark-mode t) (mark-active t))
              (set-mark beg)
              (goto-char (line-end-position))
              (let ((note (mevedel-note-create (plist-get operation :text) :defer-write t)))
                (overlay-put note 'mevedel-note-author 'agent)
                note))))))))

(defun mevedel-note-write--resolve (id session)
  "Resolve open note UUID ID in SESSION without accepting another workspace."
  (let* ((workspace (mevedel-session-workspace session))
         (entry (cl-find id (mevedel-notes--entries workspace)
                         :key (lambda (entry) (plist-get entry :uuid)) :test #'equal)))
    (unless entry (error "No open note %s" id))
    (mevedel-note-write--source (plist-get entry :file) session)
    (or (mevedel-notes--resolve id workspace) (error "Note source is unavailable"))))

(defun mevedel-note-write--prepared (patch session)
  "Capture suggested PATCH for SESSION without applying it."
  (let ((proposal (mevedel-tool-patch-parse
                   patch (mevedel-workspace-root (mevedel-session-workspace session)))))
    (dolist (operation (plist-get proposal :operations))
      (when (or (mevedel-tool-patch-resource-address-p (plist-get operation :rel-path))
                (and (plist-get operation :move-rel-path)
                     (mevedel-tool-patch-resource-address-p (plist-get operation :move-rel-path))))
        (error "Prepared note changes need ordinary source paths, not resources")))
    (dolist (path (mevedel-tool-patch-get-paths-from-proposal proposal))
      (mevedel-note-write--source path session)
      (let* ((workspace (mevedel-session-workspace session))
             (context (mevedel-permission--invocation-context
                       :tool-name "ApplyPatch" :session session :workspace workspace
                       :request mevedel--current-request :invocation mevedel--agent-invocation
                       :buffer (current-buffer) :path path))
             (decision (mevedel-check-permission-with-metadata
                        "ApplyPatch" :normalized-context context))
             (outcome (mevedel-permission-decision-raw-outcome decision)))
        ;; Preparing is read-only; user apply supplies approval, not a deny override.
        (when (or (and (eq outcome 'deny) (not (eq (plist-get decision :via) 'plan-mode)))
                  (eq outcome 'internal-error)
                  (and (plist-get context :protected-path-p)
                       (not (plist-get context :resource-granted-p))
                       (not (eq (plist-get context :mode) 'full-auto))))
          (error "Prepared change is denied for %s" path))))
    (mevedel-tool-patch--capture-baseline proposal)
    (let ((changes (mevedel-tool-patch-planned-changes proposal)))
      (mevedel-tool-patch-assert-baseline proposal)
      (mevedel-note-change-record proposal changes (current-buffer)))))

(defun mevedel-tool-note-write (args)
  "Perform the workspace note operations in ARGS, syncing once even on failure."
  (let* ((session (or mevedel--session (error "Notes require a session")))
         (workspace (mevedel-session-workspace session))
         (operations (append (plist-get args :operations) nil))
         changed results to-send)
    (unless operations (error "Choose at least one note operation"))
    (mevedel-note-write--check args session)
    (mevedel-session-artifacts-assert-new-mutation-authority session)
    (mevedel-instruction-store-observe workspace)
    (condition-case err
        (unwind-protect
            (progn
              (dolist (operation operations)
                (let* ((action (plist-get operation :action))
                       (prepared (when (plist-get operation :prepared_change)
                                   (unless (equal action "create")
                                     (error "Attach prepared_change only when creating a note"))
                                   (mevedel-note-write--prepared
                                    (plist-get operation :prepared_change) session)))
                       (note (if (equal action "create")
                                 (mevedel-note-write--create operation session)
                               (mevedel-note-write--resolve (plist-get operation :id) session))))
                  (setq changed t)
                  (pcase action
                    ((or "create" "edit")
                     (when (plist-member operation :text)
                       (mevedel-note-edit (plist-get operation :text) note))
                     (when (plist-member operation :tags)
                       (mevedel-note-set-tags
                        (mapcar #'intern (append (plist-get operation :tags) nil)) note))
                     (when prepared
                       (overlay-put note 'mevedel-note-prepared-change prepared)
                       (overlay-put note 'mevedel-note-attention-state 'ready))
                     (mevedel-notes--render note))
                    ((or "close" "done") (mevedel-note-done note))
                    ("delete" (mevedel-note-delete note))
                    ("send" (push note to-send))
                    (_ (error "Unknown note action: %s" action)))
                  (unless (equal action "send")
                    (push (format "%s Note #%s notes://%s.md" action (overlay-get note 'mevedel-id)
                                  (overlay-get note 'mevedel-uuid)) results))))
              (when to-send
                (setq to-send (nreverse to-send))
                (mevedel-note-send-batch to-send :no-display t :defer-write t)
                (dolist (note to-send)
                  (push (format "send Note #%s notes://%s.md" (overlay-get note 'mevedel-id)
                                (overlay-get note 'mevedel-uuid)) results))))
          (when changed (mevedel-instruction-store-sync workspace)))
      (error
       (error "%s%s" (error-message-string err)
              (if results
                  (concat "\nCompleted operations (do not repeat):\n"
                          (mapconcat #'identity (reverse results) "\n"))
                ""))))
    (list :result (mapconcat #'identity (nreverse results) "\n"))))

(provide 'mevedel-note-write)
;;; mevedel-note-write.el ends here
