;;; mevedel-memory-list.el -- Memory proposal cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Review workspace proposals through the shared table cockpit. Collection is
;; read-only; decision commands revalidate persisted evidence and original roots.
;; The session supplies navigation context, not ownership of the background pass.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-cockpit)
(require 'mevedel-memory-pass)
(require 'mevedel-report)

;; `mevedel-journal-jobs'
(autoload 'mevedel-journal-jobs "mevedel-journal-jobs")

(defun mevedel-memory-list--collect (context)
  "Return persisted proposal rows for CONTEXT without writing or inferring."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
         (writes (mevedel-memory-write-list workspace)) last-pass seen rows)
    ;; Accepted private state remains inspectable if public publication failed.
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-directory (mevedel-workspace-root workspace)) "state" "passes")
                        mevedel-journal-store-id-regexp))
      (when (mevedel-session-control-fs-path-exists-p (file-name-concat directory "accepted.el"))
        (push (list :kind 'consolidation :pass-id (file-name-nondirectory (directory-file-name directory))) entries)))
    (dolist (entry entries)
      (when (and (eq (plist-get entry :kind) 'consolidation)
                 (not (mevedel-journal-cleanup-pass-retired-p (mevedel-workspace-root workspace) (plist-get entry :pass-id)))
                 (not (member (plist-get entry :pass-id) seen)))
        (let ((pass (plist-get entry :pass-id)))
          (push pass seen)
          (condition-case err
              (let ((accepted (mevedel-memory-store-accepted workspace pass)))
                (when accepted
                  (let ((date (plist-get (plist-get accepted :review) :created)))
                    (when (string> date (or last-pass "")) (setq last-pass date))))
                (dolist (proposal (plist-get accepted :proposals))
                  (let* ((id (plist-get proposal :id))
                         (scope (plist-get (plist-get accepted :prepared) :scope))
                         (root (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
                         (decision (mevedel-memory-decision-status workspace id))
                         (write (seq-find (lambda (row) (and (equal id (plist-get (plist-get row :intent) :proposal))
                                                            (or (plist-get row :marked) (plist-get row :error)))) writes))
                         (status (or (plist-get decision :status) 'pending)) problem)
                    (condition-case unavailable (mevedel-memory-scope--root scope (plist-get proposal :root))
                      (error (setq problem (error-message-string unavailable))))
                    (setq problem (or problem (plist-get write :error)))
                    (when (plist-get write :marked) (setq status 'recovery-required))
                    (when problem (setq status 'unavailable))
                    (push (list :id id :pass pass :proposal proposal :accepted accepted :decision decision :write write
                                :created (plist-get (plist-get accepted :review) :created) :status status :error problem
                                :target (format "%s%s" (file-name-concat (plist-get root :dir) (plist-get proposal :file))
                                                (if (plist-get root :client) (format " [%s]" (plist-get root :client)) ""))) rows))))
            (error (push (list :id pass :pass pass :status 'unavailable :error (error-message-string err)) rows))))))
    (dolist (write writes)
      (when (and (plist-get write :error) (not (plist-get write :intent)))
        (push (list :id (plist-get write :id) :status 'unavailable :error (plist-get write :error) :write write) rows)))
    (setf (mevedel-workspace-memory-observation workspace)
          (list :at (float-time) :last-pass last-pass
                :pending (seq-count (lambda (row) (mevedel-memory-decision-actionable-status-p (plist-get row :status))) rows)
                :recovery (seq-count (lambda (row) (eq (plist-get row :status) 'recovery-required)) rows)
                :unavailable (seq-count (lambda (row) (eq (plist-get row :status) 'unavailable)) rows)))
    (sort (nreverse rows) (lambda (left right) (string> (or (plist-get left :created) "") (or (plist-get right :created) ""))))))

;;;###autoload
(defun mevedel-memory-list-summary (context &optional refresh)
  "Return disposable proposal counts for cockpit CONTEXT.
Reuse observations for ten seconds unless REFRESH is non-nil. Collection is
read-only; this snapshot never authorizes a decision or a file write."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (observation (mevedel-workspace-memory-observation workspace)))
    (when (or refresh (null observation) (>= (- (float-time) (plist-get observation :at)) 10))
      (condition-case nil
          (mevedel-memory-list--collect context)
        (error (setf (mevedel-workspace-memory-observation workspace)
                     (list :at (float-time) :pending 0 :recovery 0 :unavailable 1)))))
    (mevedel-workspace-memory-observation workspace)))

(defun mevedel-memory-list--entry (item _context)
  "Return ITEM's table row."
  (let ((proposal (plist-get item :proposal)))
    (list (plist-get item :id)
          (vector (if proposal (symbol-name (plist-get proposal :action)) "record")
                  (or (plist-get proposal :type) "")
                  (or (plist-get proposal :title) "Unavailable record")
                  (symbol-name (plist-get item :status))
                  (or (plist-get item :target) (plist-get item :id))))))

(defun mevedel-memory-list--header (items context)
  "Return workspace, pass date, mode, and counts for ITEMS in CONTEXT."
  (let ((workspace (mevedel-cockpit-context-workspace context)))
    (mevedel-cockpit-format-header
     "memory" (mevedel-workspace-root workspace)
     (format "%s | %s | %d pending%s" mevedel-memory-consolidation-mode
             (or (plist-get (mevedel-workspace-memory-observation workspace) :last-pass) "no completed pass")
             (seq-count (lambda (item) (mevedel-memory-decision-actionable-status-p (plist-get item :status))) items)
             (if (mevedel-memory-pass-running workspace) " | running" "")))))

(defun mevedel-memory-list--diff (before after)
  "Return a unified diff of exact retained BEFORE and AFTER snapshots."
  (or (mevedel--unified-diff (decode-coding-string (or (plist-get before :bytes) "") 'utf-8-unix)
                             (decode-coding-string (or (plist-get after :bytes) "") 'utf-8-unix))
      "No text changes.\n"))

(defun mevedel-memory-list--details (item context)
  "Return ITEM's captured body, before/after diff, and evidence in CONTEXT.
Check original root authority before disclosing private topic contents."
  (when (plist-get item :error) (user-error "%s" (plist-get item :error)))
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (accepted (mevedel-memory-store-accepted workspace (plist-get item :pass)))
         (proposal (seq-find (lambda (proposal) (equal (plist-get proposal :id) (plist-get item :id))) (plist-get accepted :proposals)))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (_ (mevedel-memory-scope--root scope (plist-get proposal :root)))
         (decision (mevedel-memory-decision-status workspace (plist-get item :id)))
         (intent (and decision (plist-get (mevedel-memory-decision--published workspace decision) :intent)))
         (input (unless intent
                  (if (eq (plist-get decision :status) 'rejected) (cons scope proposal)
                    (mevedel-memory-decision--application-input workspace accepted proposal))))
         (before (or (plist-get intent :before) (plist-get (cdr input) :before)))
         (after (or (plist-get intent :after)
                    (mevedel-memory-write--after before (mevedel-memory-apply-changes (car input) (cdr input))))))
    (list
     :title "Memory proposal"
     :subtitle (format "%s · %s" (plist-get proposal :title) (plist-get item :status))
     :identity (list (mevedel-workspace-root workspace) (plist-get item :pass) (plist-get item :id))
     :navigator t :initial 'body
     :validate (lambda () (mevedel-memory-scope--root scope (plist-get proposal :root)))
     :sections
     (append
      (list
       (list :id 'decision :title "Decision"
             :body (mevedel-report-fields
                    (list "Status" (plist-get item :status))
                    (list "Reason" (plist-get proposal :reason))
                    (list "Decision" (or (plist-get decision :reason) "Pending"))))
       (list :id 'body :title "Proposed body" :mode 'markdown-mode
             :body (plist-get proposal :body)))
      (mapcar
       (lambda (row)
         (let ((new (cdr (assoc (car row) after))))
           (list :id (cons 'change (car row)) :title (concat "Changes · " (car row))
                 :mode 'diff-mode
                 :body (format "Before exists: %s, mode: %S\nAfter exists: %s, mode: %S\n\n%s"
                               (plist-get (cdr row) :exists) (plist-get (cdr row) :mode)
                               (plist-get new :exists) (plist-get new :mode)
                               (mevedel-memory-list--diff (cdr row) new)))))
       before)
      (if (not (plist-get proposal :evidence))
          (list (list :id 'evidence :title "Retained evidence" :body "No digest evidence recorded.\n"))
        (mapcar
         (lambda (id)
           (let ((entry (seq-find (lambda (entry) (equal id (plist-get entry :id)))
                                  (plist-get (plist-get accepted :prepared) :entries))))
             (list :id (cons 'evidence id)
                   :title (concat "Evidence · " (or (plist-get entry :file) id))
                   :mode 'markdown-mode
                   :body (if entry
                             (format "memory://journal/%s\n\n%s\n"
                                     (plist-get entry :file) (plist-get entry :body))
                           (format "%s: retained evidence unavailable.\n" id)))))
         (plist-get proposal :evidence)))
      (list
       (list :id 'references :title "Reference checks"
             :body
             (if (not (plist-get (plist-get accepted :review) :references))
                 "No reference checks recorded.\n"
               (mapconcat
                (lambda (reference)
                  (format "%s: %s -- %s (%s)\nScope: %s\n"
                          (plist-get reference :topic) (plist-get reference :token)
                          (plist-get reference :result) (plist-get reference :checked)
                          (plist-get reference :scope)))
                (plist-get (plist-get accepted :review) :references) "\n")))
       (list :id 'target :title "Target & pass"
             :body (mevedel-report-fields
                    (list "Target" (plist-get item :target))
                    (list "Pass" (plist-get item :pass)))))))))

(defun mevedel-memory-list--act (action &optional reason all)
  "Apply ACTION with REASON to the selected proposal, or ALL pending rows."
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
         (items (if all (copy-sequence (mevedel-cockpit-surface-items)) (list (mevedel-cockpit-surface-selected)))))
    (unwind-protect
        (dolist (item items)
          (when (or (not all) (mevedel-memory-decision-actionable-status-p (plist-get item :status)))
            (unless (plist-get item :proposal) (user-error "This record has no available proposal"))
            (pcase action
              ('accept (mevedel-memory-decision-apply workspace (plist-get item :pass) (plist-get item :id)))
              ('reject (mevedel-memory-decision-reject workspace (plist-get item :pass) (plist-get item :id) reason))
              ('reverse (mevedel-memory-decision-reverse workspace (plist-get item :pass) (plist-get item :id))))))
      (mevedel-cockpit-surface-refresh))))

(defun mevedel-memory-list-accept ()
  "Accept the selected memory proposal."
  (interactive) (mevedel-memory-list--act 'accept))

(defun mevedel-memory-list-reject (&optional reason)
  "Reject the selected memory proposal with optional REASON."
  (interactive (list (when current-prefix-arg (read-string "Rejection reason (optional): "))))
  (mevedel-memory-list--act 'reject reason))

(defun mevedel-memory-list-accept-all ()
  "Accept pending proposals sequentially with their captured checks."
  (interactive) (mevedel-memory-list--act 'accept nil t))

(defun mevedel-memory-list-reject-all (&optional reason)
  "Reject pending proposals with optional REASON."
  (interactive (list (when current-prefix-arg (read-string "Rejection reason (optional): "))))
  (mevedel-memory-list--act 'reject reason t))

(defun mevedel-memory-list-reverse ()
  "Reverse the selected applied proposal if its files remain unchanged."
  (interactive) (mevedel-memory-list--act 'reverse))

(defun mevedel-memory-list-recover (&optional rollback)
  "Reconcile the selected pending write, or explicitly ROLLBACK its attempt."
  (interactive)
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
         (item (mevedel-cockpit-surface-selected))
         (intents (mevedel-memory-write-pending workspace (plist-get item :id))))
    (unless intents (user-error "No marked write to recover for this proposal"))
    (unwind-protect
        (dolist (intent intents)
          (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash) rollback))
      (mevedel-cockpit-surface-refresh))))

(defun mevedel-memory-list-rollback ()
  "Roll back the selected interrupted write only from recorded states."
  (interactive) (mevedel-memory-list-recover t))

(defun mevedel-memory-list-running ()
  "Inspect this client's running consolidation without changing its request buffer."
  (interactive)
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
         (request (plist-get (mevedel-memory-pass-running workspace) :request))
         (source (plist-get request :buffer))
         (report (plist-get request :report)))
    (unless (and (buffer-live-p source) report)
      (user-error "No consolidation request is running in this client"))
    (let ((buffer (mevedel-report-show "*mevedel running consolidation*" (funcall report))))
      (with-current-buffer buffer (mevedel-report-follow-source source report))
      buffer)))

(defun mevedel-memory-list-kill ()
  "Cancel this client's running consolidation."
  (interactive)
  (mevedel-memory-pass-cancel (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
  (mevedel-cockpit-surface-refresh))

(defun mevedel-memory-list-jobs ()
  "Inspect pending journal captures and their retry/discard actions."
  (interactive)
  (mevedel-journal-jobs (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context))))

(defun mevedel-memory-list--finished (workspace result)
  "Refresh WORKSPACE's open table and report settled consolidation RESULT."
  (when-let* ((buffer (get-buffer "*mevedel memory proposals*")))
    (with-current-buffer buffer
      (when (and mevedel-cockpit--context (eq workspace (mevedel-cockpit-context-workspace mevedel-cockpit--context)))
        (condition-case nil (mevedel-cockpit-surface-refresh) (error nil)))))
  (pcase (plist-get result :outcome)
    ('success
     (if (eq (plist-get result :mode) 'auto)
         (message "mevedel: memory review updated %d files; %d proposals held, %d not applied; %d digests remain"
                  (plist-get result :updated-files) (plist-get result :held-count)
                  (plist-get result :unapplied-count) (or (plist-get result :remaining) 0))
       (let ((entry (plist-get result :entry)))
         (message "mevedel: memory review covered %d digests, proposed %d changes; %d digests remain"
                  (length (plist-get entry :digests)) (length (plist-get entry :proposals)) (or (plist-get result :remaining) 0)))))
    ('aborted (message "mevedel: memory review cancelled"))
    (_ (message "mevedel: memory review failed: %s" (or (plist-get result :error) "Unavailable")))))

;;;###autoload
(defun mevedel-remember (&optional focus context)
  "Review workspace memory now, with optional FOCUS, using cockpit CONTEXT.
Run sessionless and open its proposals table. With a prefix argument, ask for
a focus. An explicit review may inspect current memory without eligible digests."
  (interactive (list (if current-prefix-arg (read-string "Memory review focus: ") "")))
  (setq context (or context (mevedel-cockpit-current-context)))
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (state (mevedel-memory-pass-start workspace (lambda (result) (mevedel-memory-list--finished workspace result))
                                           :focus (or focus "") :memory-only t)))
    (mevedel-memory-list-open context)
    state))

(defconst mevedel-memory-list--surface
  '(:buffer-name "*mevedel memory proposals*" :label "memory proposals" :row-label "proposal"
    :mode mevedel-memory-list-mode
    :format [("Action" 12 t) ("Type" 10 t) ("Title" 24 t) ("Status" 20 t) ("Target / origin" 0 t)]
    :collect mevedel-memory-list--collect :entry mevedel-memory-list--entry :header mevedel-memory-list--header
    :details mevedel-memory-list--details :details-buffer "*mevedel memory proposal*"
    :keys (("i" "Inspect body, diff, and evidence" mevedel-cockpit-surface-details)
           ("a" "Accept proposal" mevedel-memory-list-accept) ("r" "Reject proposal" mevedel-memory-list-reject)
           ("A" "Accept all pending proposals" mevedel-memory-list-accept-all)
           ("R" "Reject all pending proposals" mevedel-memory-list-reject-all)
           ("u" "Reverse an applied proposal" mevedel-memory-list-reverse)
           ("c" "Reconcile interrupted write" mevedel-memory-list-recover)
           ("U" "Roll back interrupted write" mevedel-memory-list-rollback)
           ("v" "Inspect running consolidation" mevedel-memory-list-running)
           ("k" "Kill running consolidation" mevedel-memory-list-kill)
           ("m" "Run consolidation (prefix: focus)" mevedel-remember)
           ("j" "Inspect pending journal jobs" mevedel-memory-list-jobs)))
  "Shared cockpit surface for memory proposals.")

(define-derived-mode mevedel-memory-list-mode tabulated-list-mode "mevedel-memory"
  "Inspect and decide persisted memory proposals."
  (mevedel-cockpit-setup-tabulated-surface mevedel-memory-list--surface))

;;;###autoload
(defun mevedel-memory-list-open (&optional context)
  "Open memory proposals for session cockpit CONTEXT and recover marked writes."
  (interactive)
  (setq context (or context (mevedel-cockpit-current-context)))
  (let ((workspace (mevedel-cockpit-context-workspace context)))
    ;; A live owner or unavailable record does not prevent read-only inspection.
    ;; Collection keeps unavailable records visible with their captured reason.
    (condition-case nil (mevedel-memory-decision-recover-pending workspace) (error nil)))
  (mevedel-cockpit-open-surface mevedel-memory-list--surface context))

(provide 'mevedel-memory-list)
;;; mevedel-memory-list.el ends here
