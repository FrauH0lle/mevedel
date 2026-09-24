;;; mevedel-memory-list.el -- Memory proposal cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Review workspace proposals through the shared table cockpit. Collection is
;; read-only; decision commands revalidate persisted evidence and original roots.
;; The session supplies navigation context, not ownership of the background pass.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-cockpit)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-library)
(require 'mevedel-memory-cleanup)
(require 'mevedel-report)

;; `mevedel-journal-jobs'
(autoload 'mevedel-journal-jobs "mevedel-journal-jobs")

(defvar-local mevedel-memory-list--view nil "Current memory table view.")
(defvar-local mevedel-memory-list--positions nil "Selected row IDs by view.")

(defun mevedel-memory-list-switch (view)
  "Show VIEW in the current memory cockpit, preserving each view's selection."
  (unless (memq view '(candidates memories history)) (user-error "Unknown memory view"))
  (setf (alist-get mevedel-memory-list--view mevedel-memory-list--positions) (tabulated-list-get-id))
  (setq mevedel-memory-list--view view)
  (mevedel-cockpit-surface-refresh (alist-get view mevedel-memory-list--positions)))

(defun mevedel-memory-list-candidates ()
  "Show outstanding memory candidates."
  (interactive) (mevedel-memory-list-switch 'candidates))

(defun mevedel-memory-list-memories ()
  "Show currently stored memories."
  (interactive) (mevedel-memory-list-switch 'memories))

(defun mevedel-memory-list-history ()
  "Show retained memory decisions and completed reviews."
  (interactive) (mevedel-memory-list-switch 'history))

(defun mevedel-memory-list--visible (context)
  "Collect rows for the selected view in CONTEXT."
  (let* ((rows (mevedel-memory-list--collect context))
         (pending (seq-remove (lambda (row) (memq (plist-get row :status) '(applied rejected reversed completed))) rows)))
    (unless mevedel-memory-list--view
      (setq mevedel-memory-list--view (if pending 'candidates 'memories)))
    (pcase mevedel-memory-list--view
      ('candidates pending)
      ('memories (mevedel-memory-library-list (mevedel-cockpit-context-workspace context)))
      ('history (seq-difference rows pending #'equal)))))

(defun mevedel-memory-list-open-file ()
  "Open the selected stored memory for normal Emacs editing."
  (interactive)
  (find-file (mevedel-memory-library-path
              (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context))
              (mevedel-cockpit-surface-selected))))

(defun mevedel-memory-list-delete ()
  "Delete the selected stored memory and index entry with checked undo."
  (interactive)
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
         (item (mevedel-cockpit-surface-selected))
         (path (mevedel-memory-library-path workspace item)))
    (when (yes-or-no-p (format "Delete memory %s (%s)? " (plist-get item :title) path))
      (unwind-protect
          (let ((decision (mevedel-memory-library-delete workspace item)))
            (unless (eq 'applied (plist-get decision :status))
              (user-error "%s" (or (plist-get decision :reason) "Memory deletion was not applied"))))
        (mevedel-cockpit-surface-refresh)))))

(defun mevedel-memory-list--collect (context)
  "Return persisted proposal rows for CONTEXT without writing or inferring."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
         (writes (mevedel-memory-write-list workspace)) last-pass seen rows)
    ;; Accepted private state remains inspectable if public publication failed.
    (dolist (directory (mevedel-session-control-fs-list-directory
                        (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-store-directory (mevedel-workspace-root workspace))) "passes")
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
                (when (and accepted (null (plist-get accepted :proposals)))
                  (push (list :id pass :kind 'review :pass pass :accepted accepted :status 'completed
                              :created (plist-get (plist-get accepted :review) :created)) rows))
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

(defun mevedel-memory-list--entry (item _context)
  "Return ITEM's table row."
  (let ((proposal (plist-get item :proposal)))
    (list (plist-get item :id)
          (vector (cond (proposal (symbol-name (plist-get proposal :action)))
                        ((eq (plist-get item :kind) 'memory) "memory")
                        ((eq (plist-get item :kind) 'review) "review") (t "record"))
                  (or (plist-get proposal :type) (plist-get item :type) "")
                  (or (plist-get proposal :title) (plist-get item :title)
                      (and (eq (plist-get item :kind) 'review) "No proposed changes") "Unavailable record")
                  (symbol-name (plist-get item :status))
                  (or (plist-get item :target) (plist-get item :created) (format "%s" (plist-get item :id)))))))

(defun mevedel-memory-list--header (_items context)
  "Return view selectors and cached workspace counts for CONTEXT."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (observation (mevedel-workspace-memory-observation workspace)))
    (concat
     (mapconcat
      (lambda (view)
        (let ((map (make-sparse-keymap)))
          (define-key map [tab-line mouse-1]
                      (lambda (event) (interactive "e")
                        (select-window (posn-window (event-start event)))
                        (mevedel-memory-list-switch view)))
          (propertize (capitalize (symbol-name view)) 'local-map map 'mouse-face 'highlight
                      'face (if (eq view mevedel-memory-list--view) 'bold 'link))))
      '(candidates memories history) " | ")
     "    "
     (mevedel-cockpit-format-header
      "memory" (mevedel-workspace-root workspace)
      (format "%d pending | %d recovery | history %d days%s"
              (or (plist-get observation :pending) 0) (or (plist-get observation :recovery) 0)
              mevedel-memory-history-max-age-days (if (mevedel-memory-pass-running workspace) " | running" ""))))))

(defun mevedel-memory-list--retention (workspace accepted)
  "Describe ACCEPTED history's expiry or retaining dependency in WORKSPACE."
  (condition-case err
      (let* ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
             (history (mevedel-memory-cleanup--history workspace accepted entries))
             (review (plist-get accepted :review)))
        (when (and (string-empty-p (or (plist-get review :focus) ""))
                   (seq-some (lambda (entry) (and (eq 'digest (plist-get entry :kind))
                                                 (member (plist-get entry :id) (plist-get review :digests)))) entries))
          (error "Reviewed source notes still have retained references"))
        (format "History eligible for cleanup: %s\n"
                (format-time-string "%Y-%m-%d %H:%M UTC" (seconds-to-time (plist-get history :expires)) t)))
    (error (format "History retained: %s\n" (error-message-string err)))))

(defun mevedel-memory-list--review-details (item context)
  "Return the complete no-candidate review and its evidence for ITEM in CONTEXT."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (accepted (mevedel-memory-store-accepted workspace (plist-get item :pass)))
         (prepared (plist-get accepted :prepared))
         (scope (plist-get prepared :scope)))
    (dolist (root (plist-get scope :roots)) (mevedel-memory-scope--root scope (car root)))
    (list :title "Completed memory review" :identity (plist-get item :id) :navigator t
          :validate (lambda () (dolist (root (plist-get scope :roots)) (mevedel-memory-scope--root scope (car root))))
          :sections
          (cons (list :id 'decision :title "Result"
                      :body (concat (or (plist-get accepted :no-action) "") "\n\n"
                                    (mevedel-memory-list--retention workspace accepted)))
                (mapcar (lambda (entry) (list :id (plist-get entry :id) :title (plist-get entry :file)
                                             :mode 'markdown-mode :body (plist-get entry :body)))
                        (plist-get prepared :entries))))))

(defun mevedel-memory-list--diff (before after)
  "Return a unified diff of exact retained BEFORE and AFTER snapshots."
  (or (mevedel--unified-diff (decode-coding-string (or (plist-get before :bytes) "") 'utf-8-unix)
                             (decode-coding-string (or (plist-get after :bytes) "") 'utf-8-unix))
      "No text changes.\n"))

(cl-defun mevedel-memory-list--details (item context)
  "Return ITEM's captured body, before/after diff, and evidence in CONTEXT.
Check original root authority before disclosing private topic contents."
  (when (plist-get item :error) (user-error "%s" (plist-get item :error)))
  (pcase (plist-get item :kind)
    ('memory (cl-return-from mevedel-memory-list--details
               (mevedel-memory-library-details (mevedel-cockpit-context-workspace context) item)))
    ('review (cl-return-from mevedel-memory-list--details (mevedel-memory-list--review-details item context))))
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (accepted (mevedel-memory-store-accepted workspace (plist-get item :pass)))
         (proposal (seq-find (lambda (proposal) (equal (plist-get proposal :id) (plist-get item :id))) (plist-get accepted :proposals)))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (_ (mevedel-memory-scope--root scope (plist-get proposal :root)))
         (decision (mevedel-memory-decision-status workspace (plist-get item :id)))
         (intent (and decision (plist-get (mevedel-memory-decision--published workspace decision) :intent)))
         (input (unless (or intent (eq (plist-get decision :status) 'rejected))
                  (mevedel-memory-decision--application-input accepted proposal)))
         (effective (if (eq (plist-get input :status) 'fresh) (plist-get input :proposal) proposal))
         (effective-scope (if (eq (plist-get input :status) 'fresh) (plist-get input :scope) scope))
         (before (or (plist-get intent :before) (plist-get effective :before)))
         (after (or (plist-get intent :after)
                    (mevedel-memory-write--after before (mevedel-memory-apply-changes effective-scope effective)))))
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
                    (list "Decision" (or (plist-get decision :reason) "Pending"))
                    (when input (list "Current check" (or (plist-get input :reason) "Ready to apply")))
                    (list "History" (mevedel-memory-list--retention workspace accepted))))
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
        (let ((mevedel-journal-cleanup--inhibit-scheduling t))
          (dolist (item items)
            (when (or (not all) (mevedel-memory-decision-actionable-status-p (plist-get item :status)))
              (unless (plist-get item :proposal) (user-error "This record has no available proposal"))
              (pcase action
                ('accept (mevedel-memory-decision-apply workspace (plist-get item :pass) (plist-get item :id)))
                ('reject (mevedel-memory-decision-reject workspace (plist-get item :pass) (plist-get item :id) reason))
                ('reverse (mevedel-memory-decision-reverse workspace (plist-get item :pass) (plist-get item :id)))))))
      (mevedel-journal-cleanup-schedule workspace t)
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
  (when-let* ((buffer (get-buffer "*mevedel memory*")))
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
  '(:buffer-name "*mevedel memory*" :label "memory" :row-label "item"
    :mode mevedel-memory-list-mode
    :format [("Action" 12 t) ("Type" 10 t) ("Title" 24 t) ("Status" 20 t) ("Target / origin" 0 t)]
    :collect mevedel-memory-list--visible :entry mevedel-memory-list--entry :header mevedel-memory-list--header
    :details mevedel-memory-list--details :details-buffer "*mevedel memory proposal*"
    :keys (("1" "Show candidates" mevedel-memory-list-candidates)
           ("2" "Show stored memories" mevedel-memory-list-memories)
           ("3" "Show history" mevedel-memory-list-history)
           ("o" "Open stored memory file" mevedel-memory-list-open-file)
           ("d" "Delete stored memory" mevedel-memory-list-delete)
           ("i" "Inspect body, diff, and evidence" mevedel-cockpit-surface-details)
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
  "Shared cockpit surface for candidates, stored memories, and history.")

(define-derived-mode mevedel-memory-list-mode tabulated-list-mode "mevedel-memory"
  "Manage memory candidates, stored topics, and retained decisions."
  (mevedel-cockpit-setup-tabulated-surface mevedel-memory-list--surface)
  ;; Keep native column headings in the header line; views use the tab line.
  (setq-local tab-line-format '(:eval (mevedel-cockpit-surface-header-line))))

;;;###autoload
(defun mevedel-memory-list-open (&optional context)
  "Open the memory cockpit for CONTEXT and recover marked writes."
  (interactive)
  (setq context (or context (mevedel-cockpit-current-context)))
  (let ((workspace (mevedel-cockpit-context-workspace context)))
    ;; A live owner or unavailable record does not prevent read-only inspection.
    ;; Collection keeps unavailable records visible with their captured reason.
    (condition-case nil (mevedel-memory-decision-recover-pending workspace) (error nil)))
  (mevedel-cockpit-open-surface mevedel-memory-list--surface context))

(provide 'mevedel-memory-list)
;;; mevedel-memory-list.el ends here
