;;; niwa-note-write.el -- Model writes workspace notes -*- lexical-binding: t -*-

;;; Commentary:

;; Model-facing note operations reuse source commands and sync once per batch.

;;; Code:

(require 'cl-lib)
(require 'niwa-notes)
(require 'niwa-persistence)
(require 'niwa-permissions)
(require 'niwa-session-artifacts)
(require 'niwa-agents)
(require 'niwa-presets)
(require 'niwa-note-change)

(defun niwa-note-write--check (args session)
  "Refuse ARGS' existing-note operations for read-only SESSION owners."
  (when (and (or (niwa-plan-read-only-p session)
                 (and niwa--agent-invocation
                      (niwa-agent-invocation-agent niwa--agent-invocation)
                      (not (niwa-agent--specs-contain-tool-p
                            (niwa-agent--effective-specs
                             (niwa-agent-invocation-agent niwa--agent-invocation))
                            "ApplyPatch"))))
             (cl-some (lambda (operation)
                        (not (equal "create" (plist-get operation :action))))
                      (append (plist-get args :operations) nil)))
    (signal 'niwa-permission-denied
            '("Read-only conversations may create notes, not change existing notes"))))

(defun niwa-tool-note-check (_tool args)
  "Authorize note ARGS while preserving read-only restrictions."
  (unless niwa--session (error "Notes require a session"))
  (niwa-note-write--check args niwa--session)
  'allow)

(defun niwa-note-write--source (file session)
  "Authorize reading FILE in SESSION's workspace and return its absolute path."
  (let* ((workspace (niwa-session-workspace session))
         (root (niwa-workspace-root workspace))
         (path (expand-file-name (or file (error "Create needs file_path")) root))
         (context (niwa-permission--invocation-context
                   :tool-name "Read" :session session :workspace workspace
                   :request niwa--current-request :invocation niwa--agent-invocation
                   :buffer (current-buffer) :path path :workspace-root root)))
    (unless (and (file-in-directory-p path root)
                 (eq 'allow (niwa-check-permission "Read" :normalized-context context)))
      (error "Note source is not readable in this workspace"))
    path))

(defun niwa-note-write--match-range (operation beg limit start end)
  "Resolve OPERATION's exact match in BEG..LIMIT, on lines START..END."
  (let ((text (plist-get operation :match))
        (occurrence (plist-get operation :occurrence))
        (select (plist-member operation :occurrence))
        (case-fold-search nil)
        (hits 0) range)
    (unless (and (stringp text) (> (length text) 0))
      (error "Note match must be nonempty text"))
    (when (and select (not (and (integerp occurrence) (> occurrence 0))))
      (error "Note occurrence must be a positive integer"))
    (goto-char beg)
    (while (search-forward text limit t)
      (let ((from (- (point) (length text))))
        (when (= (cl-incf hits) (or occurrence 1))
          (setq range (cons from (point))))
        (goto-char (1+ from))))
    (cond
     ((zerop hits) (error "Note match not found in lines %d–%d" start end))
     ((not range) (error "Note occurrence %d is out of range (%d matches in lines %d–%d)"
                         occurrence hits start end))
     ((and (not select) (> hits 1))
      (error "Note match is ambiguous: %d matches in lines %d–%d; specify occurrence"
             hits start end)))
    range))

(defun niwa-note-write--create (operation session)
  "Create OPERATION's note in SESSION without syncing the store."
  (let* ((file (niwa-note-write--source (plist-get operation :file_path) session))
         (start (plist-get operation :start_line))
         (end (or (plist-get operation :end_line) start))
         (match (plist-member operation :match)))
    (unless (file-regular-p file) (error "Notes need an existing source file"))
    (unless (and (integerp start) (> start 0) (integerp end) (>= end start))
      (error "Invalid note line range"))
    (when (and (not match) (plist-member operation :occurrence))
      (error "Note occurrence requires match"))
    (with-current-buffer (find-file-noselect file)
      (when (and niwa--workspace
                 (not (equal (niwa--instruction-workspace-key niwa--workspace)
                             (niwa--instruction-workspace-key
                              (niwa-session-workspace session)))))
        (error "Note source belongs to a different workspace"))
      (setq-local niwa--workspace (niwa-session-workspace session))
      (save-mark-and-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (or (not (zerop (forward-line (1- start))))
                    (and (eobp) (bolp) (> (point) (point-min))))
            (error "Note line is beyond source"))
          (let ((beg (point)))
            (when (or (not (zerop (forward-line (- end start))))
                      (and (eobp) (bolp) (> (point) (point-min))))
              (error "Note line is beyond source"))
            (let* ((limit (line-end-position))
                   (range (if match (niwa-note-write--match-range operation beg limit start end)
                            (cons beg limit)))
                   (workspace (niwa--instruction-buffer-workspace (current-buffer))))
              (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
                (setq workspace (niwa-notes--ensure-workspace workspace))
                (prog1 (niwa-note-make (current-buffer) (car range) (cdr range)
                                          :text (plist-get operation :text)
                                          :author 'agent :span (and match 'char))
                  (niwa-instruction-store-touch workspace))))))))))

(defun niwa-note-write--resolve (id session)
  "Resolve open note UUID ID in SESSION without accepting another workspace."
  (let* ((workspace (niwa-session-workspace session))
         (entry (cl-find id (niwa-notes-open workspace)
                         :key (lambda (entry) (plist-get entry :uuid)) :test #'equal)))
    (unless entry (error "No open note %s" id))
    (niwa-note-write--source (plist-get entry :file) session)
    (or (niwa-note-resolve id workspace) (error "Note source is unavailable"))))

(defun niwa-note-write--prepared (patch session)
  "Capture suggested PATCH for SESSION without applying it."
  (let ((proposal (niwa-tool-patch-parse
                   patch (niwa-workspace-root (niwa-session-workspace session)))))
    (dolist (operation (plist-get proposal :operations))
      (when (or (niwa-tool-patch-resource-address-p (plist-get operation :rel-path))
                (and (plist-get operation :move-rel-path)
                     (niwa-tool-patch-resource-address-p (plist-get operation :move-rel-path))))
        (error "Prepared note changes need ordinary source paths, not resources")))
    (dolist (path (niwa-tool-patch-get-paths-from-proposal proposal))
      (niwa-note-write--source path session)
      (let* ((workspace (niwa-session-workspace session))
             (context (niwa-permission--invocation-context
                       :tool-name "ApplyPatch" :session session :workspace workspace
                       :request niwa--current-request :invocation niwa--agent-invocation
                       :buffer (current-buffer) :path path))
             (decision (niwa-check-permission-with-metadata
                        "ApplyPatch" :normalized-context context))
             (outcome (niwa-permission-decision-raw-outcome decision)))
        ;; Preparing is read-only; user apply supplies approval, not a deny override.
        (when (or (and (eq outcome 'deny) (not (eq (plist-get decision :via) 'plan-mode)))
                  (eq outcome 'internal-error)
                  (and (plist-get context :protected-path-p)
                       (not (plist-get context :resource-granted-p))
                       (not (eq (plist-get context :mode) 'full-auto))))
          (error "Prepared change is denied for %s" path))))
    (niwa-tool-patch--capture-baseline proposal)
    (let ((changes (niwa-tool-patch-planned-changes proposal)))
      (niwa-tool-patch-assert-baseline proposal)
      (niwa-note-change-record proposal changes (current-buffer)))))

(defun niwa-tool-note-write (args)
  "Perform the workspace note operations in ARGS, syncing once even on failure."
  (let* ((session (or niwa--session (error "Notes require a session")))
         (workspace (niwa-session-workspace session))
         (operations (append (plist-get args :operations) nil))
         changed results to-send)
    (unless operations (error "Choose at least one note operation"))
    (niwa-note-write--check args session)
    (niwa-session-artifacts-assert-new-mutation-authority session)
    (niwa-instruction-store-observe workspace)
    (condition-case err
        (unwind-protect
            (progn
              (dolist (operation operations)
                (let* ((action (plist-get operation :action))
                       (tags (when (plist-member operation :tags)
                               (niwa-notes--validate-tags
                                (mapcar #'intern (append (plist-get operation :tags) nil)))))
                       (prepared (when (plist-get operation :prepared_change)
                                   (unless (equal action "create")
                                     (error "Attach prepared_change only when creating a note"))
                                   (niwa-note-write--prepared
                                    (plist-get operation :prepared_change) session)))
                       (note (if (equal action "create")
                                 (niwa-note-write--create operation session)
                               (niwa-note-write--resolve (plist-get operation :id) session))))
                  (setq changed t)
                  (pcase action
                    ((or "create" "edit")
                     (when (and (equal action "edit") (plist-member operation :text))
                       (niwa-note-edit (plist-get operation :text) note))
                     (when prepared
                       (setq prepared
                             (plist-put prepared :note-extents
                                        (delq nil (list (niwa-note-change--base-extent
                                                         note prepared)))))
                       (niwa-note-hold-change (list note) prepared))
                     (if (plist-member operation :tags)
                         (niwa-note-set-tags tags note)
                       (when (equal action "create")
                         (niwa-notes--render note))))
                    ((or "close" "done") (niwa-note-done note))
                    ("delete" (niwa-note-delete note))
                    ("send" (push note to-send))
                    (_ (error "Unknown note action: %s" action)))
                  (unless (equal action "send")
                    (push (format "%s Note #%s notes://%s.md" action (overlay-get note 'niwa-id)
                                  (overlay-get note 'niwa-uuid)) results))))
              (when to-send
                (setq to-send (nreverse to-send))
                (niwa-note-send-batch to-send :no-display t :defer-write t)
                (dolist (note to-send)
                  (push (format "send Note #%s notes://%s.md" (overlay-get note 'niwa-id)
                                (overlay-get note 'niwa-uuid)) results))))
          (when changed (niwa-instruction-store-sync workspace)))
      (error
       (error "%s%s" (error-message-string err)
              (if results
                  (concat "\nCompleted operations (do not repeat):\n"
                          (mapconcat #'identity (reverse results) "\n"))
                ""))))
    (list :result (mapconcat #'identity (nreverse results) "\n"))))

(provide 'niwa-note-write)
;;; niwa-note-write.el ends here
