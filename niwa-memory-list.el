;;; niwa-memory-list.el -- Memory proposal cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Review workspace proposals through the shared table cockpit. Collection is
;; read-only; decision commands revalidate persisted evidence and original roots.
;; The session supplies navigation context, not ownership of the background pass.

;;; Code:

(require 'cl-lib)
(require 'niwa-cockpit)
(require 'niwa-memory-pass)
(require 'niwa-memory-library)
(require 'niwa-utilities)
(require 'niwa-memory-cleanup)
(require 'niwa-report)
(require 'niwa-journal-worker)

;; `niwa-journal-jobs'
(autoload 'niwa-journal-jobs "niwa-journal-jobs" nil t)

;; `niwa-utilities'
(declare-function niwa-diagnostic-message "niwa-utilities" (scope format &rest args))
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

(defvar-local niwa-memory-list--view nil "Current memory table view.")
(defvar-local niwa-memory-list--positions nil "Selected row IDs by view.")
(defvar-local niwa-memory-list--window-positions nil "Selected row IDs by window and view.")
(defvar-local niwa-memory-list--snapshot nil "Display-only proposal observation.")
(defvar-local niwa-memory-list--memories nil "Displayed topic rows, possibly from an earlier observation.")
(defvar-local niwa-memory-list--memories-state 'stale
  "Topic rows' state: `stale' until observed, `refreshing', or `current'.")
(defvar-local niwa-memory-list--memories-job nil "Current topic observation token.")
(defvar-local niwa-memory-list--job nil "Current read-only history scan.")
(defvar-local niwa-memory-list--error nil "Last history scan failure.")
(defvar-local niwa-memory-list--recovery nil "Display watcher for checked recovery.")
(defvar-local niwa-memory-list--recovery-warning nil "Retired recovery watch limitation.")
(defvar-local niwa-memory-list--notice nil "Non-row table status overlay.")

(defvar niwa-memory-list--reuse (make-hash-table :test #'eq :weakness 'key)
  "Last display replies by workspace, forgotten when this Emacs exits.")

(defun niwa-memory-list--terminal-p (row)
  "Return non-nil when display ROW has a terminal status that cannot change."
  (memq (plist-get row :status) '(applied rejected reversed completed)))

(defun niwa-memory-list--update-notice (items &optional unavailable)
  "Show static table status for ITEMS, or UNAVAILABLE owner, without adding rows."
  (let ((text (cond
               (unavailable unavailable)
               (items nil)
               ((eq niwa-memory-list--view 'memories)
                (if (eq niwa-memory-list--memories-state 'current) "No stored memories." "Loading memories..."))
               (niwa-memory-list--error "Could not load memory data; press g to retry")
               (niwa-memory-list--job "Loading history...")
               ((and niwa-memory-list--recovery (not niwa-memory-list--snapshot))
                "Loading committed memory data; recovery pending...")
               ((eq niwa-memory-list--view 'history) "No retained memory history.")
               (t "No pending memory candidates."))))
    (if text
        (progn
          (unless (overlayp niwa-memory-list--notice)
            (setq niwa-memory-list--notice (make-overlay (point-min) (point-min))))
          (move-overlay niwa-memory-list--notice (point-min) (point-min))
          (overlay-put niwa-memory-list--notice 'before-string
                       (propertize (concat "\n  " text "\n") 'face 'shadow)))
      (when (overlayp niwa-memory-list--notice) (delete-overlay niwa-memory-list--notice))
      (setq niwa-memory-list--notice nil))))

(defun niwa-memory-list--display-row (row)
  "Project ROW to display metadata, without accepted evidence or write proofs."
  (let (display)
    (dolist (key '(:id :kind :pass :created :status :error :check :target :title :type
                       :file :configured :root :client :reversal-hold))
      (when (plist-member row key) (setq display (plist-put display key (plist-get row key)))))
    (when-let* ((proposal (plist-get row :proposal)))
      (setq display (plist-put display :proposal
                               (list :action (plist-get proposal :action)
                                     :type (plist-get proposal :type) :title (plist-get proposal :title)))))
    (niwa-memory-library-copy display)))

(defun niwa-memory-list--cancel ()
  "Retire this buffer's scan, retaining ownership if cleanup fails."
  (when-let* ((job niwa-memory-list--job))
    ;; A queued reply must be inert, but a failed stop must not lose
    ;; the only child handle. Keep the retired job until every cleanup succeeds.
    (plist-put job :retired t)
    (condition-case err
        (progn
          (let ((inhibit-quit t))
            (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
            (plist-put job :timer nil)
            (when-let* ((child (plist-get job :worker))) (niwa-journal-worker-cancel child))
            (plist-put job :worker nil))
          (setq niwa-memory-list--job nil))
      ((error quit)
       (setq niwa-memory-list--error (format "Memory display cleanup failed: %s" (error-message-string err)))
       (niwa-memory-list--update-notice niwa-cockpit--items)
       (force-mode-line-update)
       (signal (car err) (cdr err))))))

(defun niwa-memory-list--remember-position ()
  "Remember this tab's buffer and window rows without saving loading gaps."
  (when-let* ((id (tabulated-list-get-id)))
    (setf (alist-get niwa-memory-list--view niwa-memory-list--positions) id))
  (setq niwa-memory-list--window-positions
        (seq-filter (lambda (entry) (and (window-live-p (car entry))
                                        (eq (window-buffer (car entry)) (current-buffer))))
                    niwa-memory-list--window-positions))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when-let* ((id (save-excursion (goto-char (window-point window)) (tabulated-list-get-id))))
      (setf (alist-get niwa-memory-list--view
                      (alist-get window niwa-memory-list--window-positions)) id))))

(defun niwa-memory-list--restore-positions ()
  "Restore this tab's displayed rows without selecting a window."
  (let ((fallback (tabulated-list-get-id)))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let* ((id (or (alist-get niwa-memory-list--view
                               (alist-get window niwa-memory-list--window-positions)) fallback))
             (position (save-excursion (niwa-cockpit-goto-id id) (point))))
        (set-window-point window position)))))

(defun niwa-memory-list--cancel-memories ()
  "Retire this buffer's topic observation; a late reply is ignored."
  (when-let* ((timer (plist-get niwa-memory-list--memories-job :timer))) (cancel-timer timer))
  (setq niwa-memory-list--memories-job nil))

(defun niwa-memory-list--observe-memories (context)
  "Show CONTEXT's last topic rows now and observe current ones after display.
Rows stay marked refreshing until the observation replaces them.  Retiring the
token stops it; a changed root or memory configuration restarts it."
  (let* ((buffer (current-buffer))
         (workspace (niwa-cockpit-context-workspace context))
         (root (niwa-workspace-root workspace))
         (roots (niwa-system--memory-roots workspace))
         (token (list :context context)))
    (unless niwa-memory-list--memories
      (setq niwa-memory-list--memories (niwa-memory-library-cached workspace)))
    (setq niwa-memory-list--memories-state 'refreshing niwa-memory-list--memories-job token)
    (cl-labels ((live-p ()
                  (and (buffer-live-p buffer)
                       (eq token (buffer-local-value 'niwa-memory-list--memories-job buffer))
                       (eq context (buffer-local-value 'niwa-cockpit--context buffer))))
                (deliver (rows)
                  (when (live-p)
                    (with-current-buffer buffer
                      (setq niwa-memory-list--memories-job nil)
                      ;; Like the history fence, rows observed for another root
                      ;; or memory configuration never land as current.
                      (if (not (and (equal root (niwa-workspace-root workspace))
                                    (equal roots (niwa-system--memory-roots workspace))))
                          (setq niwa-memory-list--memories nil
                                niwa-memory-list--memories-state 'stale)
                        (setq niwa-memory-list--memories rows
                              niwa-memory-list--memories-state 'current))
                      ;; A closed owner context keeps the history path's notice.
                      (when (niwa-cockpit--live-context-p context)
                        (niwa-memory-list--remember-position)
                        (niwa-cockpit-surface-refresh
                         (or (tabulated-list-get-id)
                             (alist-get niwa-memory-list--view niwa-memory-list--positions)))
                        (niwa-memory-list--restore-positions))))))
      (plist-put
       token :timer
       (run-at-time
        0 nil
        (lambda ()
          (plist-put token :timer nil)
          (when (live-p)
            ;; Per-root failures arrive as rows; anything else must not leave
            ;; the table refreshing forever.  A retired token stops the reads.
            (condition-case err
                (niwa-memory-library-refresh workspace #'deliver #'live-p)
              (error
               (deliver (list (list :kind 'memory :id '(unavailable inventory)
                                    :title "Unavailable memory inventory" :status 'unavailable
                                    :error (error-message-string err)))))))))))))

(defun niwa-memory-list-refresh ()
  "Request a fresh display observation without reusing any acceptance proof."
  (interactive)
  (niwa-memory-list--remember-position)
  (niwa-memory-list--cancel)
  (niwa-memory-list--cancel-memories)
  ;; Topic rows stay visible, marked refreshing, until their observation lands.
  (setq niwa-memory-list--snapshot nil niwa-memory-list--memories-state 'stale
        niwa-memory-list--error nil)
  (niwa-cockpit-surface-refresh)
  (niwa-memory-list--restore-positions))

(defun niwa-memory-list--close ()
  "Release display work without cancelling workspace-owned recovery."
  (when (overlayp niwa-memory-list--notice) (delete-overlay niwa-memory-list--notice))
  (setq niwa-memory-list--notice nil)
  (niwa-memory-list--cancel)
  (niwa-memory-list--cancel-memories)
  (when (timerp (plist-get niwa-memory-list--recovery :timer))
    (cancel-timer (plist-get niwa-memory-list--recovery :timer)))
  (setq niwa-memory-list--recovery nil))

(defun niwa-memory-list--unavailable (reason)
  "Clear rows and display REASON without requiring a live session owner."
  (setq niwa-memory-list--error reason niwa-memory-list--snapshot nil
        niwa-cockpit--items nil niwa-cockpit--row-items (make-hash-table :test #'equal)
        tabulated-list-entries nil)
  (tabulated-list-print t)
  (niwa-memory-list--update-notice nil reason)
  (force-mode-line-update))

(defun niwa-memory-list--observe-recovery (buffer token)
  "Refresh BUFFER after TOKEN's coalesced workspace recovery owner retires."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq token niwa-memory-list--recovery)
        (let ((workspace (niwa-cockpit-context-workspace niwa-cockpit--context)))
          (cond
           ((or (not (eq (plist-get token :context) niwa-cockpit--context))
                (not (equal (plist-get token :root)
                            (niwa-workspace-root (niwa-cockpit-context-workspace niwa-cockpit--context))))
                (not (niwa-cockpit--live-context-p niwa-cockpit--context)))
            (cancel-timer (plist-get token :timer))
            (setq niwa-memory-list--recovery nil)
            (when (and (eq (plist-get token :context) niwa-cockpit--context)
                       (or (not niwa-memory-list--job)
                           (eq (plist-get token :context) (plist-get niwa-memory-list--job :context))))
              (niwa-memory-list--cancel)
              (niwa-memory-list--unavailable "Memory display owner is unavailable; reopen the cockpit")))
           ((> (float-time) (plist-get token :expires))
            (cancel-timer (plist-get token :timer))
            (setq niwa-memory-list--recovery nil
                  niwa-memory-list--recovery-warning
                  "Recovery observation timed out; completion not observed; g refreshes data; reopen to watch recovery")
            (niwa-memory-list--update-notice niwa-cockpit--items)
            (force-mode-line-update))
           ((not (niwa-memory-decision-recovering-p workspace))
            (cancel-timer (plist-get token :timer))
            (setq niwa-memory-list--recovery nil niwa-memory-list--recovery-warning nil)
            (if (= (plist-get token :generation) (niwa-memory-decision-recovery-generation workspace))
                ;; Recovery changed nothing: a history scan, in flight or
                ;; complete, still reads current committed state.
                (progn (niwa-memory-list--update-notice niwa-cockpit--items)
                       (force-mode-line-update))
              ;; Reconciliation publishes/classifies retained writes; it does not
              ;; reapply curated files. Keep the independent topic inventory.
              (niwa-memory-list--remember-position)
              (niwa-memory-list--cancel)
              (setq niwa-memory-list--snapshot nil niwa-memory-list--error nil)
              (niwa-cockpit-surface-refresh)
              (niwa-memory-list--restore-positions)))))))))

(defun niwa-memory-list--received (buffer job reply)
  "Present read-only REPLY for BUFFER's current JOB, without selecting a window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq job niwa-memory-list--job) (not (plist-get job :retired)))
        (niwa-memory-list--remember-position)
        ;; Retire before rendering: reentrant or duplicate replies cannot win.
        (niwa-memory-list--cancel)
        (when (and (derived-mode-p 'niwa-memory-list-mode)
                   (not (eq (plist-get job :context) niwa-cockpit--context)))
          (niwa-memory-list--unavailable "Memory display context changed; refresh required"))
        (when (and (derived-mode-p 'niwa-memory-list-mode)
                   (eq (plist-get job :context) niwa-cockpit--context))
          (let* ((context (plist-get job :context))
                 (workspace (niwa-cockpit-context-workspace context)))
            (condition-case err
                (progn
                  (unless (and (derived-mode-p 'niwa-memory-list-mode)
                               (eq context niwa-cockpit--context)
                               (niwa-cockpit--live-context-p context)
                               (eq (plist-get job :session) (niwa-cockpit-context-session context))
                               (or (null (plist-get job :session))
                                   (and (eq workspace (niwa-session-workspace (plist-get job :session)))
                                        (eq (plist-get job :session)
                                            (buffer-local-value 'niwa--session (niwa-cockpit-context-data-buffer context)))))
                               (equal (plist-get job :root) (niwa-workspace-root workspace))
                               (equal (plist-get job :identity)
                                      (let ((niwa-workspace-identity--cache (make-hash-table :test #'equal)))
                                        (niwa-workspace-identity-read (plist-get job :root))))
                               (equal (plist-get job :client) (niwa-workspace-identity-client))
                               (equal (plist-get job :roots) (niwa-system--memory-roots workspace)))
                    (error "Memory display context changed; refresh required"))
                  (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
                  ;; Historical remote roots keep the native reader and authority.
                  (when (plist-get reply :native)
                    (setq reply (list :rows (niwa-memory-list--collect context)
                                      :observation (niwa-workspace-memory-observation workspace))))
                  (unless (and (plist-member reply :rows)
                               (numberp (plist-get (plist-get reply :observation) :at))
                               (proper-list-p (plist-get reply :rows))
                               (cl-every (lambda (row)
                                           (and (plist-get row :id)
                                                (or (memq (plist-get row :status)
                                                          '(pending stale unavailable))
                                                    (niwa-memory-list--terminal-p row))
                                                (not (cl-some (lambda (key) (plist-member row key))
                                                              '(:accepted :decision :write)))))
                                         (plist-get reply :rows)))
                    (error "Invalid memory display observation"))
                  (setq niwa-memory-list--snapshot (niwa-memory-library-copy reply))
                  (puthash workspace niwa-memory-list--snapshot niwa-memory-list--reuse)
                  (setf (niwa-workspace-memory-observation workspace)
                        (niwa-memory-library-copy (plist-get reply :observation))))
              (error (setq niwa-memory-list--error (error-message-string err))))
            (if (niwa-cockpit--live-context-p context)
                (progn
                  (niwa-cockpit-surface-refresh
                   (or (tabulated-list-get-id)
                       (alist-get niwa-memory-list--view niwa-memory-list--positions)))
                  (niwa-memory-list--restore-positions))
              ;; The owner check intentionally prevents the ordinary refresh.
              ;; Clear old rows and update the error header without resurrecting it.
              (niwa-memory-list--unavailable niwa-memory-list--error))))))))

(defun niwa-memory-list--fingerprint (workspace)
  "Return WORKSPACE history metadata hashes, or nil on uncertainty.
This is a display hint, not content authentication or an atomic snapshot."
  (condition-case nil
      (let* ((journal (niwa-journal-store-directory (niwa-workspace-root workspace)))
             (directories (list (directory-file-name (file-name-directory (niwa-journal-store-state-directory journal))) journal))
             (results (niwa-session-control-fs-run-program
                       (mapcar (lambda (path)
                                 (when (file-remote-p path) (error "Remote history metadata"))
                                 (list :op 'stat-tree :path path :optional t)) directories)))
             manifest)
        (unless (= (length results) (length directories)) (error "Incomplete history metadata"))
        (dolist (result results)
          (let ((path (plist-get result :path)) (fields (plist-get result :value)) entries)
            (if (eq (plist-get result :status) 'absent)
                (push (cons path 'absent) manifest)
              (unless (and (eq (plist-get result :status) 'ok) fields (zerop (% (length fields) 10)))
                (error "Unavailable history metadata"))
              (while fields
                (let ((entry (cl-loop repeat 10 collect (pop fields))))
                  (unless (member (car entry) '("f" "d")) (error "Unsafe history metadata"))
                  (push entry entries)))
              (push (cons path (secure-hash 'sha256
                                           (prin1-to-string (sort entries (lambda (a b) (string< (cadr a) (cadr b))))))) manifest))))
        (nreverse manifest))
    (error nil)))

(defun niwa-memory-list--scan (workspace options)
  "Return display rows for WORKSPACE, reusing unchanged terminal OPTIONS rows."
  (let* ((previous (plist-get options :previous))
         (key (list (niwa-workspace-root workspace)
                    (niwa-workspace-identity-read (niwa-workspace-root workspace))
                    (plist-get options :client) (plist-get options :roots)
                    ;; Root authority compares physical roots, so a replaced root
                    ;; must not reuse rows its fresh check would make unavailable.
                    (mapcar (lambda (root) (file-truename (expand-file-name (plist-get root :dir))))
                            (plist-get options :roots))))
         (fingerprint (niwa-memory-list--fingerprint workspace))
         (same (and fingerprint (equal key (plist-get previous :key))
                    (equal fingerprint (plist-get previous :fingerprint))))
         (old (and same (plist-get previous :rows)))
         (changing (seq-filter (lambda (row) (or (and (eq 'applied (plist-get row :status))
                                                     (not (niwa-memory-proposal-note-action-p
                                                           (plist-get (plist-get row :proposal) :action))))
                                                (not (niwa-memory-list--terminal-p row)))) old))
         (passes (mapcar (lambda (row) (plist-get row :pass)) changing))
         (reused (and same (not (memq nil passes))
                      (seq-remove (lambda (row) (member (plist-get row :pass) passes)) old)))
         (rows (if (and same (not changing)) old
                 (niwa-memory-list--collect (niwa-cockpit-workspace-context workspace) t reused))))
    (when (and same (not changing))
      (setf (niwa-workspace-memory-observation workspace)
            (plist-put (copy-sequence (plist-get previous :observation)) :at (float-time))))
    (let* ((after (niwa-memory-list--fingerprint workspace))
           (stable (and (equal fingerprint after) after)))
      (if (and same (not stable))
          ;; Reused rows may be stale: this open performs the normal full scan.
          (niwa-memory-list--scan workspace (append '(:previous nil) options))
        (list :rows rows :key key :fingerprint stable
              :observation (niwa-workspace-memory-observation workspace))))))

(defun niwa-memory-list-scan-child (workspace payload)
  "Collect display-only WORKSPACE rows under frozen PAYLOAD configuration."
  (cl-letf (((symbol-function 'niwa-workspace-identity-client) (lambda () (plist-get payload :client)))
            ((symbol-function 'niwa-system--memory-roots) (lambda (_) (plist-get payload :roots))))
    (catch 'niwa-memory-list-native
      (niwa-memory-list--scan workspace payload))))

(defun niwa-memory-list--start (context)
  "Start a cancellable read-only history observation for CONTEXT."
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (buffer (current-buffer))
         (job (list :context context :root (niwa-workspace-root workspace)
                    :session (niwa-cockpit-context-session context)
                    :identity (niwa-workspace-identity-read (niwa-workspace-root workspace))
                    :client (niwa-workspace-identity-client) :roots (niwa-system--memory-roots workspace))))
    (setq niwa-memory-list--job job)
    (condition-case err
        (progn
          (if (not (niwa-journal-worker-local-p
                    workspace (mapcar (lambda (root) (plist-get root :dir)) (plist-get job :roots))))
              (niwa-memory-list--received buffer job (list :native t))
            (progn
              (niwa-journal-worker-start
               workspace 'memory-display
               (list :client (plist-get job :client) :roots (plist-get job :roots)
                     :previous (gethash workspace niwa-memory-list--reuse))
               (lambda (result) (niwa-memory-list--received buffer job result)) :owner job)
              (let ((inhibit-quit t))
                (plist-put job :timer
                           (run-at-time 180 nil
                                        (lambda ()
                                          (niwa-memory-list--received buffer job '(:error "Memory display scan timed out"))))))))
          ;; The final timer registration may be the last evaluation before
          ;; returning. Deliver its deferred quit inside the cleanup owner.
          (when (and quit-flag (not inhibit-quit))
            (setq quit-flag nil)
            (signal 'quit nil)))
      ((error quit)
       (niwa-memory-list--cancel)
       (setq niwa-memory-list--error (error-message-string err))
       (when (eq (car err) 'quit) (signal (car err) (cdr err)))))))

(defun niwa-memory-list-quit ()
  "Cancel display work and return to the invoking layout."
  (interactive)
  (niwa-memory-list--close)
  (niwa-cockpit-surface-quit))

(defun niwa-memory-list-switch (view)
  "Show VIEW in the current memory cockpit, preserving each view's selection."
  (unless (memq view '(candidates memories history)) (niwa-diagnostic-user-error (niwa-diagnostic-scope (niwa-cockpit-context-workspace (niwa-cockpit-surface-context))) "Unknown memory view"))
  (niwa-memory-list--remember-position)
  (setq niwa-memory-list--view view)
  (niwa-cockpit-surface-refresh (alist-get view niwa-memory-list--positions))
  (niwa-memory-list--restore-positions))

(defun niwa-memory-list--candidate-count (context)
  "Return CONTEXT's last observed candidate count, or nil when unknown.
Count pending and unavailable rows, as Candidates shows."
  (when-let* ((observation (niwa-workspace-memory-observation (niwa-cockpit-context-workspace context))))
    (apply #'+ (mapcar (lambda (key) (or (plist-get observation key) 0)) '(:pending :unavailable)))))

(defun niwa-memory-list-candidates ()
  "Show outstanding memory candidates."
  (interactive) (niwa-memory-list-switch 'candidates))

(defun niwa-memory-list-memories ()
  "Show currently stored memories."
  (interactive) (niwa-memory-list-switch 'memories))

(defun niwa-memory-list-history ()
  "Show retained memory decisions and completed reviews."
  (interactive) (niwa-memory-list-switch 'history))

(cl-defun niwa-memory-list--visible (context)
  "Return display-only rows for CONTEXT's selected view."
  ;; Report refreshes have their own buffer, not this cockpit's snapshot owner.
  (unless (derived-mode-p 'niwa-memory-list-mode)
    (cl-return-from niwa-memory-list--visible
      (append (niwa-memory-list--collect context)
              (niwa-memory-library-list (niwa-cockpit-context-workspace context)))))
  ;; Known alerts open Candidates; otherwise Memories shows at once.  Passes,
  ;; decisions, changing recovery and cleanup discard the counts.
  (let ((counts (niwa-memory-list--candidate-count context)))
    (unless (or niwa-memory-list--view niwa-memory-list--snapshot)
      (setq niwa-memory-list--view (if (and counts (> counts 0)) 'candidates 'memories)))
    ;; Stored topics never need proposal history; Memories scans only to learn
    ;; unknown candidate counts for its tab label, and never switches views.
    (unless (or niwa-memory-list--snapshot niwa-memory-list--job niwa-memory-list--error
                (and (eq niwa-memory-list--view 'memories) counts))
      (niwa-memory-list--start context)))
  (let* ((rows (plist-get niwa-memory-list--snapshot :rows))
         (pending (seq-remove #'niwa-memory-list--terminal-p rows)))
    (let ((items
           (pcase niwa-memory-list--view
             ('candidates pending)
             ('memories
              (when (eq niwa-memory-list--memories-state 'stale)
                (niwa-memory-list--observe-memories context))
              niwa-memory-list--memories)
             ('history (seq-filter #'niwa-memory-list--terminal-p rows)))))
      (niwa-memory-list--update-notice items)
      (niwa-memory-library-copy items))))

(defun niwa-memory-list-open-file ()
  "Open the selected stored memory for normal Emacs editing."
  (interactive)
  (find-file (niwa-memory-library-path
              (niwa-cockpit-context-workspace (niwa-cockpit-surface-context))
              (niwa-cockpit-surface-selected))))

(defun niwa-memory-list-delete ()
  "Delete the selected stored memory and index entry with checked undo."
  (interactive)
  (let* ((workspace (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
         (item (niwa-cockpit-surface-selected))
         (path (niwa-memory-library-path workspace item)))
    (when (yes-or-no-p (format "Delete memory %s (%s)? " (plist-get item :title) path))
      (unwind-protect
          (let ((decision (niwa-memory-library-delete workspace item)))
            (unless (eq 'applied (plist-get decision :status))
              (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "%s" (or (plist-get decision :reason) "Memory deletion was not applied"))))
        (niwa-memory-list-refresh)))))

(defun niwa-memory-list--collect (context &optional local-only reused)
  "Return detached persisted proposal rows for CONTEXT without writing.
LOCAL-ONLY throws `niwa-memory-list-native' with a native reply before
accessing any authenticated remote target.  Invalid local evidence stays
unavailable; it cannot supply target paths or authorize recovery.
REUSED contains display-only rows of unchanged, entirely terminal passes."
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (snapshot (niwa-memory-decision-inspect
                    workspace :skip (mapcar (lambda (row) (plist-get row :pass)) reused) :local-only local-only))
         (rows (append (reverse reused) (plist-get snapshot :rows)))
         last-pass)
    (when (plist-get snapshot :native) (throw 'niwa-memory-list-native '(:native t)))
    (dolist (row rows)
      (when (string> (or (plist-get row :created) "") (or last-pass ""))
        (setq last-pass (plist-get row :created))))
    (setf (niwa-workspace-memory-observation workspace)
          (list :at (float-time) :last-pass last-pass
                :pending (seq-count (lambda (row) (niwa-memory-decision-actionable-status-p (plist-get row :status))) rows)
                :unavailable (seq-count (lambda (row) (eq (plist-get row :status) 'unavailable)) rows)))
    (sort (mapcar #'niwa-memory-list--display-row rows)
          (lambda (left right) (string> (or (plist-get left :created) "") (or (plist-get right :created) ""))))))

(defun niwa-memory-list--entry (item _context)
  "Return ITEM's table row."
  (let ((proposal (plist-get item :proposal)))
    (list (plist-get item :id)
          (vector (cond (proposal (symbol-name (plist-get proposal :action)))
                        ((eq (plist-get item :kind) 'memory) "memory")
                        ((eq (plist-get item :kind) 'review) "review") (t "record"))
                  (or (plist-get proposal :type) (plist-get item :type) "")
                  (or (plist-get proposal :title) (plist-get item :title)
                      (and (eq (plist-get item :kind) 'review) "No proposed changes") "Unavailable record")
                  (pcase (list (plist-get item :status) (plist-get item :check))
                    (`(applied ,_) (if (plist-get item :reversal-hold) "check reversal" "applied"))
                    ('(stale fresh) "ready")
                    (`(,_ waiting) "awaits extraction")
                    (`(,_ stale) "superseded")
                    (`(,_ unavailable) "unavailable")
                    (`(,status ,_) (symbol-name status)))
                  (or (plist-get item :target) (plist-get item :created) (format "%s" (plist-get item :id)))))))

(defun niwa-memory-list--header (_items context)
  "Return view selectors and cached workspace counts for CONTEXT."
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (observation (niwa-workspace-memory-observation workspace)))
    (concat
     (mapconcat
      (lambda (view)
        (let ((map (make-sparse-keymap)))
          (define-key map [tab-line mouse-1]
                      (lambda (event) (interactive "e")
                        (select-window (posn-window (event-start event)))
                        (niwa-memory-list-switch view)))
          (concat
           (propertize (capitalize (symbol-name view)) 'local-map map 'mouse-face 'highlight
                       'face (if (eq view niwa-memory-list--view) 'bold 'link))
           ;; Signal waiting candidates on their tab instead of switching to it.
           (when-let* (((eq view 'candidates))
                       (count (niwa-memory-list--candidate-count context))
                       ((> count 0)))
             (propertize (format " (%d)" count) 'face 'warning 'local-map map 'mouse-face 'highlight)))))
      '(candidates memories history) " | ")
     (cond (niwa-memory-list--error (format "    Scan failed: %s (g retries)" niwa-memory-list--error))
           (niwa-memory-list--job "    Loading history...")
           ((not niwa-memory-list--snapshot) "    History not loaded")
           (t "    Committed snapshot (g refreshes)"))
     (if (and (eq niwa-memory-list--view 'memories) niwa-memory-list--memories-job)
         " | refreshing memories" "")
     (if niwa-memory-list--recovery " | recovery pending" "")
     (if niwa-memory-list--recovery-warning
         (concat " | " niwa-memory-list--recovery-warning) "")
     "    "
     (niwa-cockpit-format-header
      "memory" (niwa-workspace-root workspace)
      (format "%s | history %d days%s"
              (cond ((not observation) "proposals not checked")
                    ;; Counts from an earlier scan, not this table's snapshot.
                    ((not niwa-memory-list--snapshot)
                     (format "%d pending (cached)" (or (plist-get observation :pending) 0)))
                    (t (format "%d pending" (or (plist-get observation :pending) 0))))
              niwa-memory-history-max-age-days
              (if-let* ((drain (niwa-memory-pass-drain-status workspace)))
                  (format " | drain batch %d (%s), %s notes remain"
                          (1+ (plist-get drain :batch)) (plist-get drain :status)
                          (or (plist-get drain :notes-remaining) "?"))
                (if (niwa-memory-pass-running workspace) " | running" "")))))))

(defun niwa-memory-list--retention (workspace accepted)
  "Describe ACCEPTED history's expiry or retaining dependency in WORKSPACE."
  (condition-case err
      (let* ((entries (niwa-journal-store-entries (niwa-workspace-root workspace)))
             (history (niwa-memory-cleanup--history workspace accepted entries))
             (review (plist-get accepted :review)))
        (when (and (string-empty-p (or (plist-get review :focus) ""))
                   (seq-some (lambda (entry) (and (eq 'digest (plist-get entry :kind))
                                                 (member (plist-get entry :id) (plist-get review :digests)))) entries))
          (error "Reviewed source notes still have retained references"))
        (format "History eligible for cleanup: %s\n"
                (format-time-string "%Y-%m-%d %H:%M UTC" (seconds-to-time (plist-get history :expires)) t)))
    (error (format "History retained: %s\n" (error-message-string err)))))

(defun niwa-memory-list--review-details (item context)
  "Return the complete no-candidate review and its evidence for ITEM in CONTEXT."
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (accepted (niwa-memory-store-accepted workspace (plist-get item :pass)))
         (prepared (plist-get accepted :prepared))
         (scope (plist-get prepared :scope)))
    (dolist (root (plist-get scope :roots)) (niwa-memory-scope--root scope (car root)))
    (list :title "Completed memory review" :identity (plist-get item :id) :navigator t
          :validate (lambda () (dolist (root (plist-get scope :roots)) (niwa-memory-scope--root scope (car root))))
          :sections
          (cons (list :id 'decision :title "Result"
                      :body (concat (or (plist-get accepted :no-action) "") "\n\n"
                                    (niwa-memory-list--retention workspace accepted)))
                (mapcar (lambda (entry) (list :id (plist-get entry :id) :title (plist-get entry :file)
                                             :mode 'markdown-mode :body (plist-get entry :body)))
                        (plist-get prepared :entries))))))

(defun niwa-memory-list--diff (before after)
  "Return a unified diff of exact retained BEFORE and AFTER snapshots.
Attachments and folders carry no text and are described by hash and kind."
  (cl-flet ((describe (snapshot)
              (cond ((not (plist-get snapshot :exists)) "absent")
                    ((plist-get snapshot :directory) "empty directory")
                    (t (format "%s bytes, sha256 %s"
                               (or (plist-get snapshot :size) (length (plist-get snapshot :bytes)))
                               (plist-get snapshot :hash))))))
    (if (seq-some (lambda (snapshot) (and (plist-get snapshot :exists) (not (plist-get snapshot :bytes))))
                  (list before after))
        (format "Before: %s\nAfter: %s\n" (describe before) (describe after))
      (or (niwa--unified-diff (decode-coding-string (or (plist-get before :bytes) "") 'utf-8-unix)
                                 (decode-coding-string (or (plist-get after :bytes) "") 'utf-8-unix))
          "No text changes.\n"))))

(cl-defun niwa-memory-list--details (item context)
  "Return ITEM's captured body, before/after diff, and evidence in CONTEXT.
Check original root authority before disclosing private topic contents."
  (when (plist-get item :error) (niwa-diagnostic-user-error (niwa-diagnostic-scope (niwa-cockpit-context-workspace context)) "%s" (plist-get item :error)))
  (pcase (plist-get item :kind)
    ('memory (cl-return-from niwa-memory-list--details
               (niwa-memory-library-details (niwa-cockpit-context-workspace context) item)))
    ('review (cl-return-from niwa-memory-list--details (niwa-memory-list--review-details item context))))
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (accepted (niwa-memory-store-accepted workspace (plist-get item :pass)))
         (proposal (seq-find (lambda (proposal) (equal (plist-get proposal :id) (plist-get item :id))) (plist-get accepted :proposals)))
         (scope (plist-get (plist-get accepted :prepared) :scope))
         (inspection (niwa-memory-decision-inspect workspace :pass (plist-get item :pass) :proposal (plist-get item :id)))
         (decision (plist-get inspection :decision))
         (status (or (plist-get decision :status) (plist-get item :status)))
         (input (plist-get inspection :check))
         (before (plist-get inspection :before))
         (after (plist-get inspection :after)))
    (list
     :title "Memory proposal"
     :subtitle (format "%s · %s" (plist-get proposal :title) status)
     :identity (list (niwa-workspace-root workspace) (plist-get item :pass) (plist-get item :id))
     :navigator t :initial 'body
     :validate (lambda () (niwa-memory-scope--root scope (plist-get proposal :root)))
     :sections
     (append
      (list
       (list :id 'decision :title "Decision"
             :body (niwa-report-fields
                    (list "Status" status)
                    (when-let* ((reason (plist-get proposal :reason))) (list "Reason" reason))
                    (list "Decision" (or (plist-get inspection :reversal-hold) (plist-get decision :reason) "Pending"))
                    (when input (list "Current check" (or (plist-get input :reason) "Ready to apply")))
                    (list "History" (niwa-memory-list--retention workspace accepted))))
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
                               (niwa-memory-list--diff (cdr row) new)))))
       before)
      (if (not (plist-get proposal :evidence))
          (list (list :id 'evidence :title "Retained evidence" :body "No digest evidence recorded.\n"))
        (mapcar
         (lambda (id)
           (let ((entry (seq-find (lambda (entry) (equal id (plist-get entry :id)))
                                  (plist-get (plist-get accepted :prepared) :entries)))
                 (note (seq-find (lambda (note) (equal id (plist-get note :id)))
                                 (niwa-memory-notes-documents scope))))
             (list :id (cons 'evidence id)
                   :title (concat "Evidence · " (or (plist-get entry :file) (plist-get note :path) id))
                   :mode 'markdown-mode
                   :body (cond (entry (format "memory://journal/%s\n\n%s\n"
                                              (plist-get entry :file) (plist-get entry :body)))
                               (note (format "%s (captured %s)\n\n%s\n"
                                             id (plist-get note :modified) (plist-get note :body)))
                               (t (format "%s: retained evidence unavailable.\n" id))))))
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
             :body (niwa-report-fields
                    (list "Target" (plist-get item :target))
                    (list "Pass" (plist-get item :pass)))))))))

(defun niwa-memory-list--act (action &optional reason all)
  "Apply ACTION with REASON to the selected proposal, or ALL pending rows."
  (let* ((workspace (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
         (items (if all (copy-sequence (niwa-cockpit-surface-items)) (list (niwa-cockpit-surface-selected))))
         continuation)
    (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
      (unwind-protect
          (let ((niwa-journal-cleanup--inhibit-scheduling t))
            (dolist (item items)
              (when (or (not all) (niwa-memory-decision-actionable-status-p (plist-get item :status)))
                (unless (plist-get item :proposal) (user-error "This record has no available proposal"))
                (pcase action
                  ('accept
                   ;; One explicit serial request: carry its opaque continuation.
                   (let ((result (niwa-memory-decision-apply workspace (plist-get item :pass) (plist-get item :id)
                                                                :continuation continuation)))
                     (setq continuation (plist-get result :continuation))
                     (when (memq (plist-get result :status) '(held waiting))
                       (user-error "%s" (or (plist-get result :reason) "Memory application is held")))))
                  ('reject (setq continuation
                                 (plist-get (niwa-memory-decision-reject workspace (plist-get item :pass) (plist-get item :id)
                                                                            :reason reason :continuation continuation)
                                            :continuation)))
                  ('reverse (niwa-memory-decision-reverse workspace (plist-get item :pass) (plist-get item :id)))))))
        (niwa-journal-cleanup-schedule workspace t)
        (niwa-memory-list-refresh)))))

(defun niwa-memory-list-accept ()
  "Accept the selected memory proposal."
  (interactive) (niwa-memory-list--act 'accept))

(defun niwa-memory-list-reject (&optional reason)
  "Reject the selected memory proposal with optional REASON."
  (interactive (list (when current-prefix-arg (read-string "Rejection reason (optional): "))))
  (niwa-memory-list--act 'reject reason))

(defun niwa-memory-list-accept-all ()
  "Accept pending proposals sequentially with their captured checks."
  (interactive) (niwa-memory-list--act 'accept nil t))

(defun niwa-memory-list-reject-all (&optional reason)
  "Reject pending proposals with optional REASON."
  (interactive (list (when current-prefix-arg (read-string "Rejection reason (optional): "))))
  (niwa-memory-list--act 'reject reason t))

(defun niwa-memory-list-reverse ()
  "Reverse the selected applied proposal if its files remain unchanged."
  (interactive) (niwa-memory-list--act 'reverse))

(defun niwa-memory-list-running ()
  "Inspect this client's running consolidation without changing its request buffer."
  (interactive)
  (let* ((workspace (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
         (request (plist-get (niwa-memory-pass-running workspace) :request))
         (source (plist-get request :buffer))
         (report (plist-get request :report)))
    (unless (and (buffer-live-p source) report)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "No consolidation request is running in this client"))
    (let ((buffer (niwa-report-show "*niwa running consolidation*" (funcall report))))
      (with-current-buffer buffer (niwa-report-follow-source source report))
      buffer)))

(defun niwa-memory-list-kill ()
  "Cancel this client's running consolidation."
  (interactive)
  (niwa-memory-pass-cancel (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
  (niwa-memory-list-refresh))

(defun niwa-memory-list-jobs ()
  "Inspect pending journal captures and their retry/discard actions."
  (interactive)
  (niwa-journal-jobs (niwa-cockpit-context-workspace (niwa-cockpit-surface-context))))

(defun niwa-memory-list-notes ()
  "Inspect the latest shared-folder observation: ready, waiting and blocked items."
  (interactive)
  (let* ((workspace (niwa-cockpit-context-workspace (niwa-cockpit-surface-context)))
         (observation (or (niwa-memory-notes-observation workspace)
                          (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "No shared working folder has been observed yet"))))
    (niwa-report-show
     "*niwa shared notes*"
     (list :title "Shared working notes" :subtitle (format "Observed %s" (plist-get observation :at))
           :identity (list 'shared-notes (niwa-workspace-root workspace))
           :sections
           (list (list :id 'summary :title "Latest review selection"
                       :body (niwa-report-fields
                              (list "Text notes" (plist-get observation :notes))
                              (list "Attachments" (plist-get observation :attachments))
                              (list "Empty folders" (plist-get observation :directories))
                              (list "Waiting for a later review" (plist-get observation :remaining))))
                 (list :id 'blocked :title "Blocked items"
                       :body (if (plist-get observation :blocked)
                                 (concat (mapconcat (lambda (row) (format "%s: %s" (car row) (cdr row)))
                                                    (plist-get observation :blocked) "\n")
                                         (let ((hidden (- (or (plist-get observation :blocked-count) 0)
                                                          (length (plist-get observation :blocked)))))
                                           (if (> hidden 0) (format "\n...and %d more blocked items.\n" hidden) "\n")))
                               "None.\n")))))))

(defun niwa-memory-list--finished (workspace result)
  "Refresh WORKSPACE's open table and report settled consolidation RESULT."
  (when-let* ((buffer (get-buffer "*niwa memory*")))
    (with-current-buffer buffer
      (when (and niwa-cockpit--context (eq workspace (niwa-cockpit-context-workspace niwa-cockpit--context)))
        (condition-case nil (niwa-memory-list-refresh) (error nil)))))
  (let ((notes (if (cl-some (lambda (key) (> (or (plist-get result key) 0) 0))
                            '(:notes-reviewed :notes-remaining :notes-blocked))
                   (format "; %d shared notes reviewed, %d waiting, %d blocked (s in the memory cockpit)"
                           (or (plist-get result :notes-reviewed) 0) (or (plist-get result :notes-remaining) 0)
                           (or (plist-get result :notes-blocked) 0))
                 "")))
    (pcase (plist-get result :outcome)
      ('success
       (if (eq (plist-get result :mode) 'auto)
           (message "niwa: memory review updated %d files; %d proposals held, %d not applied; %d digests remain%s"
                    (plist-get result :updated-files) (plist-get result :held-count)
                    (plist-get result :unapplied-count) (or (plist-get result :remaining) 0) notes)
         (let ((entry (plist-get result :entry)))
           (message "niwa: memory review covered %d digests, proposed %d changes; %d digests remain%s"
                    (length (plist-get entry :digests)) (length (plist-get entry :proposals))
                    (or (plist-get result :remaining) 0) notes))))
      ('aborted (message "niwa: memory review cancelled"))
      ('restart-required (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "%s" (plist-get result :error)))
      (_ (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "memory review failed: %s" (or (plist-get result :error) "Unavailable"))))))

;;;###autoload
(defun niwa-remember (&optional focus context)
  "Review workspace memory now, with optional FOCUS, using cockpit CONTEXT.
Run sessionless and open its proposals table. With a prefix argument, ask for
a focus. An explicit review may inspect current memory without eligible digests."
  (interactive (list (if current-prefix-arg (read-string "Memory review focus: ") "")))
  (setq context (or context (niwa-cockpit-current-context)))
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (state (niwa-memory-pass-start workspace (lambda (result) (niwa-memory-list--finished workspace result))
                                           :focus (or focus "") :memory-only t)))
    (niwa-memory-list-open context)
    state))

;;;###autoload
(defun niwa-remember-drain (&optional context)
  "Review ready workspace notes and digests in successive bounded batches.
Use cockpit CONTEXT, or the current workspace.  Stop with `k' in the memory
cockpit, or `niwa-memory-pass-cancel'.  Active turns wait rather than
capturing shared notes; failures stop the drain.  Automatic cost caps do not
limit this explicit command.  Proposals still follow the configured mode."
  (interactive)
  (setq context (or context (niwa-cockpit-current-context)))
  (let* ((workspace (niwa-cockpit-context-workspace context))
         (state
          (niwa-memory-pass-drain
           workspace
           (lambda (result)
             (unless (eq (plist-get result :outcome) 'empty)
               (niwa-memory-list--finished workspace result))
             (pcase (plist-get result :outcome)
               ('success
                (if (plist-get result :stopped)
                    (message "niwa: memory drain stopped after batch %d; remaining decisions held"
                             (plist-get result :batch))
                  (message "niwa: drain batch %d complete; %d notes and %d digests remain (k stops)"
                           (plist-get result :batch) (plist-get result :notes-remaining)
                           (plist-get result :remaining))))
               ('empty (message "niwa: memory drain complete after %d batches; no ready notes or digests remain"
                                (plist-get result :batch))))))))
    (niwa-memory-list-open context)
    state))

(defconst niwa-memory-list--surface
  '(:buffer-name "*niwa memory*" :label "memory" :row-label "item"
    :mode niwa-memory-list-mode
    :format [("Action" 12 t) ("Type" 10 t) ("Title" 24 t) ("Status" 20 t) ("Target / origin" 0 t)]
    :setup niwa-memory-list--setup
    :collect niwa-memory-list--visible :entry niwa-memory-list--entry :header niwa-memory-list--header
    :details niwa-memory-list--details :details-buffer "*niwa memory proposal*"
    :keys (("1" "Show candidates" niwa-memory-list-candidates)
           ("2" "Show stored memories" niwa-memory-list-memories)
           ("3" "Show history" niwa-memory-list-history)
           ("o" "Open stored memory file" niwa-memory-list-open-file)
           ("d" "Delete stored memory" niwa-memory-list-delete)
           ("i" "Inspect body, diff, and evidence" niwa-cockpit-surface-details)
           ("a" "Accept proposal" niwa-memory-list-accept) ("r" "Reject proposal" niwa-memory-list-reject)
           ("A" "Accept all pending proposals" niwa-memory-list-accept-all)
           ("R" "Reject all pending proposals" niwa-memory-list-reject-all)
           ("u" "Reverse an applied proposal" niwa-memory-list-reverse)
           ("v" "Inspect running consolidation" niwa-memory-list-running)
           ("k" "Kill running consolidation" niwa-memory-list-kill)
           ("m" "Run consolidation (prefix: focus)" niwa-remember)
           ("D" "Drain ready notes and digests (k stops)" niwa-remember-drain)
           ("j" "Inspect pending journal jobs" niwa-memory-list-jobs)
           ("s" "Inspect shared-note backlog" niwa-memory-list-notes)))
  "Shared cockpit surface for candidates, stored memories, and history.")

(define-derived-mode niwa-memory-list-mode tabulated-list-mode "niwa-memory"
  "Manage memory candidates, stored topics, and retained decisions."
  (niwa-cockpit-setup-tabulated-surface niwa-memory-list--surface)
  (local-set-key (kbd "g") #'niwa-memory-list-refresh)
  (local-set-key (kbd "q") #'niwa-memory-list-quit)
  (add-hook 'kill-buffer-hook #'niwa-memory-list--close nil t)
  (add-hook 'change-major-mode-hook #'niwa-memory-list--close nil t)
  ;; Keep native column headings in the header line; views use the tab line.
  (setq-local tab-line-format '(:eval (niwa-cockpit-surface-header-line))))

(defun niwa-memory-list--setup (context)
  "Request CONTEXT's checked recovery independently of its display scan."
  (let ((buffer (current-buffer))
        (workspace (niwa-cockpit-context-workspace context)))
    (setq niwa-memory-list--recovery-warning nil
          niwa-memory-list--recovery
          (list :context context :root (niwa-workspace-root workspace)
                :expires (+ (float-time) 180)
                ;; Compared at retirement: only a changing recovery stales reads.
                :generation (niwa-memory-decision-recovery-generation workspace)))
    ;; The existing owner retains workspace claims, original target checks,
    ;; and live-buffer reconciliation. Closing the display does not cancel
    ;; workspace maintenance or grant a second operation its ownership.
    (condition-case err
        (progn
          (niwa-memory-decision-schedule-recovery workspace)
          ;; Recovery coalesces by workspace, including work started outside
          ;; this cockpit. Follow that owner, not its optional callback.
          (let ((inhibit-quit t))
            (plist-put niwa-memory-list--recovery :timer
                       (run-at-time .05 .05 #'niwa-memory-list--observe-recovery buffer niwa-memory-list--recovery))))
      ((error quit)
       (niwa-memory-list--close)
       (setq niwa-memory-list--error (error-message-string err))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun niwa-memory-list-open (&optional context)
  "Open CONTEXT's memory cockpit and request checked recovery without blocking."
  (interactive)
  (niwa-cockpit-open-surface niwa-memory-list--surface context))

(provide 'niwa-memory-list)
;;; niwa-memory-list.el ends here
