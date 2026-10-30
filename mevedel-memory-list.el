;;; mevedel-memory-list.el -- Memory proposal cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Review workspace proposals through the shared table cockpit. Collection is
;; read-only; decision commands revalidate persisted evidence and original roots.
;; The session supplies navigation context, not ownership of the background pass.

;;; Code:

(require 'cl-lib)
(require 'mevedel-cockpit)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-library)
(require 'mevedel-memory-cleanup)
(require 'mevedel-report)
(require 'mevedel-journal-worker)

;; `mevedel-journal-jobs'
(autoload 'mevedel-journal-jobs "mevedel-journal-jobs" nil t)

(defvar-local mevedel-memory-list--view nil "Current memory table view.")
(defvar-local mevedel-memory-list--positions nil "Selected row IDs by view.")
(defvar-local mevedel-memory-list--window-positions nil "Selected row IDs by window and view.")
(defvar-local mevedel-memory-list--snapshot nil "Display-only proposal observation.")
(defvar-local mevedel-memory-list--memories nil "Displayed topic rows, possibly from an earlier observation.")
(defvar-local mevedel-memory-list--memories-state 'stale
  "Topic rows' state: `stale' until observed, `refreshing', or `current'.")
(defvar-local mevedel-memory-list--memories-job nil "Current topic observation token.")
(defvar-local mevedel-memory-list--job nil "Current read-only history scan.")
(defvar-local mevedel-memory-list--error nil "Last history scan failure.")
(defvar-local mevedel-memory-list--recovery nil "Display watcher for checked recovery.")
(defvar-local mevedel-memory-list--recovery-warning nil "Retired recovery watch limitation.")
(defvar-local mevedel-memory-list--notice nil "Non-row table status overlay.")

(defun mevedel-memory-list--update-notice (items &optional unavailable)
  "Show static table status for ITEMS, or UNAVAILABLE owner, without adding rows."
  (let ((text (cond
               (unavailable unavailable)
               (items nil)
               ((eq mevedel-memory-list--view 'memories)
                (if (eq mevedel-memory-list--memories-state 'current) "No stored memories." "Loading memories..."))
               (mevedel-memory-list--error "Could not load memory data; press g to retry")
               (mevedel-memory-list--job "Loading history...")
               ((and mevedel-memory-list--recovery (not mevedel-memory-list--snapshot))
                "Loading committed memory data; recovery pending...")
               ((eq mevedel-memory-list--view 'history) "No retained memory history.")
               (t "No pending memory candidates."))))
    (if text
        (progn
          (unless (overlayp mevedel-memory-list--notice)
            (setq mevedel-memory-list--notice (make-overlay (point-min) (point-min))))
          (move-overlay mevedel-memory-list--notice (point-min) (point-min))
          (overlay-put mevedel-memory-list--notice 'before-string
                       (propertize (concat "\n  " text "\n") 'face 'shadow)))
      (when (overlayp mevedel-memory-list--notice) (delete-overlay mevedel-memory-list--notice))
      (setq mevedel-memory-list--notice nil))))

(defun mevedel-memory-list--display-row (row)
  "Project ROW to display metadata, without accepted evidence or write proofs."
  (let (display)
    (dolist (key '(:id :kind :pass :created :status :error :check :target :title :type
                       :file :configured :root :client))
      (when (plist-member row key) (setq display (plist-put display key (plist-get row key)))))
    (when-let* ((proposal (plist-get row :proposal)))
      (setq display (plist-put display :proposal
                               (list :action (plist-get proposal :action)
                                     :type (plist-get proposal :type) :title (plist-get proposal :title)))))
    (mevedel-memory-library-copy display)))

(defun mevedel-memory-list--cancel ()
  "Retire this buffer's scan, retaining ownership if cleanup fails."
  (when-let* ((job mevedel-memory-list--job))
    ;; A synchronous sentinel must be inert, but a failed stop must not lose
    ;; the only child handle. Keep the retired job until every cleanup succeeds.
    (plist-put job :retired t)
    (condition-case err
        (progn
          (let ((inhibit-quit t))
            (when (timerp (plist-get job :timer)) (cancel-timer (plist-get job :timer)))
            (plist-put job :timer nil)
            (when-let* ((child (plist-get job :worker)) ((process-live-p child))) (delete-process child))
            (plist-put job :worker nil))
          (when-let* ((directory (plist-get job :directory)))
            (when (file-directory-p directory) (delete-directory directory t)))
          (setq mevedel-memory-list--job nil))
      ((error quit)
       (setq mevedel-memory-list--error (format "Memory display cleanup failed: %s" (error-message-string err)))
       (mevedel-memory-list--update-notice mevedel-cockpit--items)
       (force-mode-line-update)
       (signal (car err) (cdr err))))))

(defun mevedel-memory-list--remember-position ()
  "Remember this tab's buffer and window rows without saving loading gaps."
  (when-let* ((id (tabulated-list-get-id)))
    (setf (alist-get mevedel-memory-list--view mevedel-memory-list--positions) id))
  (setq mevedel-memory-list--window-positions
        (seq-filter (lambda (entry) (and (window-live-p (car entry))
                                        (eq (window-buffer (car entry)) (current-buffer))))
                    mevedel-memory-list--window-positions))
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (when-let* ((id (save-excursion (goto-char (window-point window)) (tabulated-list-get-id))))
      (setf (alist-get mevedel-memory-list--view
                      (alist-get window mevedel-memory-list--window-positions)) id))))

(defun mevedel-memory-list--restore-positions ()
  "Restore this tab's displayed rows without selecting a window."
  (let ((fallback (tabulated-list-get-id)))
    (dolist (window (get-buffer-window-list (current-buffer) nil t))
      (let* ((id (or (alist-get mevedel-memory-list--view
                               (alist-get window mevedel-memory-list--window-positions)) fallback))
             (position (save-excursion (mevedel-cockpit-goto-id id) (point))))
        (set-window-point window position)))))

(defun mevedel-memory-list--cancel-memories ()
  "Retire this buffer's topic observation; a late reply is ignored."
  (when-let* ((timer (plist-get mevedel-memory-list--memories-job :timer))) (cancel-timer timer))
  (setq mevedel-memory-list--memories-job nil))

(defun mevedel-memory-list--observe-memories (context)
  "Show CONTEXT's last topic rows now and observe current ones after display.
Rows stay marked refreshing until the observation replaces them.  Retiring the
token stops it; a changed root or memory configuration restarts it."
  (let* ((buffer (current-buffer))
         (workspace (mevedel-cockpit-context-workspace context))
         (root (mevedel-workspace-root workspace))
         (roots (mevedel-system--memory-roots workspace))
         (token (list :context context)))
    (unless mevedel-memory-list--memories
      (setq mevedel-memory-list--memories (mevedel-memory-library-cached workspace)))
    (setq mevedel-memory-list--memories-state 'refreshing mevedel-memory-list--memories-job token)
    (cl-labels ((live-p ()
                  (and (buffer-live-p buffer)
                       (eq token (buffer-local-value 'mevedel-memory-list--memories-job buffer))
                       (eq context (buffer-local-value 'mevedel-cockpit--context buffer))))
                (deliver (rows)
                  (when (live-p)
                    (with-current-buffer buffer
                      (setq mevedel-memory-list--memories-job nil)
                      ;; Like the history fence, rows observed for another root
                      ;; or memory configuration never land as current.
                      (if (not (and (equal root (mevedel-workspace-root workspace))
                                    (equal roots (mevedel-system--memory-roots workspace))))
                          (setq mevedel-memory-list--memories nil
                                mevedel-memory-list--memories-state 'stale)
                        (setq mevedel-memory-list--memories rows
                              mevedel-memory-list--memories-state 'current))
                      ;; A closed owner context keeps the history path's notice.
                      (when (mevedel-cockpit--live-context-p context)
                        (mevedel-memory-list--remember-position)
                        (mevedel-cockpit-surface-refresh
                         (or (tabulated-list-get-id)
                             (alist-get mevedel-memory-list--view mevedel-memory-list--positions)))
                        (mevedel-memory-list--restore-positions))))))
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
                (mevedel-memory-library-refresh workspace #'deliver #'live-p)
              (error
               (deliver (list (list :kind 'memory :id '(unavailable inventory)
                                    :title "Unavailable memory inventory" :status 'unavailable
                                    :error (error-message-string err)))))))))))))

(defun mevedel-memory-list-refresh ()
  "Request a fresh display observation without reusing any acceptance proof."
  (interactive)
  (mevedel-memory-list--remember-position)
  (mevedel-memory-list--cancel)
  (mevedel-memory-list--cancel-memories)
  ;; Topic rows stay visible, marked refreshing, until their observation lands.
  (setq mevedel-memory-list--snapshot nil mevedel-memory-list--memories-state 'stale
        mevedel-memory-list--error nil)
  (mevedel-cockpit-surface-refresh)
  (mevedel-memory-list--restore-positions))

(defun mevedel-memory-list--close ()
  "Release display work without cancelling workspace-owned recovery."
  (when (overlayp mevedel-memory-list--notice) (delete-overlay mevedel-memory-list--notice))
  (setq mevedel-memory-list--notice nil)
  (mevedel-memory-list--cancel)
  (mevedel-memory-list--cancel-memories)
  (when (timerp (plist-get mevedel-memory-list--recovery :timer))
    (cancel-timer (plist-get mevedel-memory-list--recovery :timer)))
  (setq mevedel-memory-list--recovery nil))

(defun mevedel-memory-list--unavailable (reason)
  "Clear rows and display REASON without requiring a live session owner."
  (setq mevedel-memory-list--error reason mevedel-memory-list--snapshot nil
        mevedel-cockpit--items nil mevedel-cockpit--row-items (make-hash-table :test #'equal)
        tabulated-list-entries nil)
  (tabulated-list-print t)
  (mevedel-memory-list--update-notice nil reason)
  (force-mode-line-update))

(defun mevedel-memory-list--observe-recovery (buffer token)
  "Refresh BUFFER after TOKEN's coalesced workspace recovery owner retires."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq token mevedel-memory-list--recovery)
        (let ((workspace (mevedel-cockpit-context-workspace mevedel-cockpit--context)))
          (cond
           ((or (not (eq (plist-get token :context) mevedel-cockpit--context))
                (not (equal (plist-get token :root)
                            (mevedel-workspace-root (mevedel-cockpit-context-workspace mevedel-cockpit--context))))
                (not (mevedel-cockpit--live-context-p mevedel-cockpit--context)))
            (cancel-timer (plist-get token :timer))
            (setq mevedel-memory-list--recovery nil)
            (when (and (eq (plist-get token :context) mevedel-cockpit--context)
                       (or (not mevedel-memory-list--job)
                           (eq (plist-get token :context) (plist-get mevedel-memory-list--job :context))))
              (mevedel-memory-list--cancel)
              (mevedel-memory-list--unavailable "Memory display owner is unavailable; reopen the cockpit")))
           ((> (float-time) (plist-get token :expires))
            (cancel-timer (plist-get token :timer))
            (setq mevedel-memory-list--recovery nil
                  mevedel-memory-list--recovery-warning
                  "Recovery observation timed out; completion not observed; g refreshes data; reopen to watch recovery")
            (mevedel-memory-list--update-notice mevedel-cockpit--items)
            (force-mode-line-update))
           ((not (mevedel-memory-decision-recovering-p workspace))
            (cancel-timer (plist-get token :timer))
            (setq mevedel-memory-list--recovery nil mevedel-memory-list--recovery-warning nil)
            (if (= (plist-get token :generation) (mevedel-memory-decision-recovery-generation workspace))
                ;; Recovery changed nothing: a history scan, in flight or
                ;; complete, still reads current committed state.
                (progn (mevedel-memory-list--update-notice mevedel-cockpit--items)
                       (force-mode-line-update))
              ;; Reconciliation publishes/classifies retained writes; it does not
              ;; reapply curated files. Keep the independent topic inventory.
              (mevedel-memory-list--remember-position)
              (mevedel-memory-list--cancel)
              (setq mevedel-memory-list--snapshot nil mevedel-memory-list--error nil)
              (mevedel-cockpit-surface-refresh)
              (mevedel-memory-list--restore-positions)))))))))

(defun mevedel-memory-list--received (buffer job reply)
  "Present read-only REPLY for BUFFER's current JOB, without selecting a window."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq job mevedel-memory-list--job) (not (plist-get job :retired)))
        (mevedel-memory-list--remember-position)
        ;; Retire before rendering: reentrant or duplicate replies cannot win.
        (mevedel-memory-list--cancel)
        (when (and (derived-mode-p 'mevedel-memory-list-mode)
                   (not (eq (plist-get job :context) mevedel-cockpit--context)))
          (mevedel-memory-list--unavailable "Memory display context changed; refresh required"))
        (when (and (derived-mode-p 'mevedel-memory-list-mode)
                   (eq (plist-get job :context) mevedel-cockpit--context))
          (let* ((context (plist-get job :context))
                 (workspace (mevedel-cockpit-context-workspace context)))
            (condition-case err
                (progn
                  (unless (and (derived-mode-p 'mevedel-memory-list-mode)
                               (eq context mevedel-cockpit--context)
                               (mevedel-cockpit--live-context-p context)
                               (eq (plist-get job :session) (mevedel-cockpit-context-session context))
                               (or (null (plist-get job :session))
                                   (and (eq workspace (mevedel-session-workspace (plist-get job :session)))
                                        (eq (plist-get job :session)
                                            (buffer-local-value 'mevedel--session (mevedel-cockpit-context-data-buffer context)))))
                               (equal (plist-get job :root) (mevedel-workspace-root workspace))
                               (equal (plist-get job :identity)
                                      (let ((mevedel-workspace-identity--cache (make-hash-table :test #'equal)))
                                        (mevedel-workspace-identity-read (plist-get job :root))))
                               (equal (plist-get job :client) (mevedel-workspace-identity-client))
                               (equal (plist-get job :roots) (mevedel-system--memory-roots workspace)))
                    (error "Memory display context changed; refresh required"))
                  (when (plist-get reply :error) (error "%s" (plist-get reply :error)))
                  ;; Historical remote roots keep the native reader and authority.
                  (when (plist-get reply :native)
                    (setq reply (list :rows (mapcar #'mevedel-memory-list--display-row
                                                    (mevedel-memory-list--collect context))
                                      :observation (mevedel-workspace-memory-observation workspace))))
                  (unless (and (plist-member reply :rows)
                               (numberp (plist-get (plist-get reply :observation) :at))
                               (proper-list-p (plist-get reply :rows))
                               (cl-every (lambda (row)
                                           (and (plist-get row :id)
                                                (memq (plist-get row :status)
                                                      '(pending stale unavailable recovery-required applied rejected reversed completed))
                                                (not (cl-some (lambda (key) (plist-member row key))
                                                              '(:accepted :decision :write)))))
                                         (plist-get reply :rows)))
                    (error "Invalid memory display observation"))
                  (setq mevedel-memory-list--snapshot (mevedel-memory-library-copy reply))
                  (setf (mevedel-workspace-memory-observation workspace)
                        (mevedel-memory-library-copy (plist-get reply :observation))))
              (error (setq mevedel-memory-list--error (error-message-string err))))
            (if (mevedel-cockpit--live-context-p context)
                (progn
                  (mevedel-cockpit-surface-refresh
                   (or (tabulated-list-get-id)
                       (alist-get mevedel-memory-list--view mevedel-memory-list--positions)))
                  (mevedel-memory-list--restore-positions))
              ;; The owner check intentionally prevents the ordinary refresh.
              ;; Clear old rows and update the error header without resurrecting it.
              (mevedel-memory-list--unavailable mevedel-memory-list--error))))))))

(defun mevedel-memory-list--run (request reply)
  "Read cockpit REQUEST in a batch child and write display-only REPLY."
  (unless noninteractive (error "Memory display reader requires batch Emacs"))
  (let* ((options (mevedel-journal-worker--read request))
         (workspace (mevedel-workspace--create :root (plist-get options :root) :type 'project))
         (mevedel-journal-worker--child-p t)
         (result
          (condition-case err
              (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (plist-get options :client)))
                        ((symbol-function 'mevedel-system--memory-roots) (lambda (_) (plist-get options :roots))))
                       (if (not (mevedel-memory-decision--worker-local-p workspace))
                           (list :native t)
                         (list :rows (mapcar #'mevedel-memory-list--display-row
                                             (mevedel-memory-list--collect (mevedel-cockpit-workspace-context workspace)))
                               :observation (mevedel-workspace-memory-observation workspace))))
            (error (list :error (error-message-string err))))))
    (mevedel-journal-worker--write reply result)))

(defun mevedel-memory-list--start (context)
  "Start a cancellable read-only history observation for CONTEXT."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (buffer (current-buffer))
         (job (list :context context :root (mevedel-workspace-root workspace)
                    :session (mevedel-cockpit-context-session context)
                    :identity (mevedel-workspace-identity-read (mevedel-workspace-root workspace))
                    :client (mevedel-workspace-identity-client) :roots (mevedel-system--memory-roots workspace))))
    (setq mevedel-memory-list--job job)
    (condition-case err
        (progn
          (if (or (not (mevedel-journal-worker-supported-p workspace))
                  (seq-some (lambda (root) (file-remote-p (plist-get root :dir))) (plist-get job :roots)))
              (mevedel-memory-list--received buffer job (list :native t))
            (let* ((directory (let ((inhibit-quit t))
                                (plist-put job :directory (make-temp-file "mevedel-memory-display-" t))
                                (plist-get job :directory)))
                   (request (file-name-concat directory "request.el"))
                   (reply (file-name-concat directory "reply.el")))
              (mevedel-journal-worker--write request
                                             (list :root (plist-get job :root) :client (plist-get job :client)
                                                   :roots (plist-get job :roots)))
              (let ((inhibit-quit t))
                (plist-put job :worker
                           (make-process
                            :name "mevedel-memory-display" :noquery t :connection-type 'pipe :buffer nil
                            :command (list (expand-file-name invocation-name invocation-directory) "--batch" "-Q"
                                           "--eval" (let ((print-length nil) (print-level nil))
                                                      (prin1-to-string `(setq load-path ',load-path)))
                                           "-l" (locate-library "mevedel-memory-list") "--eval"
                                           (prin1-to-string `(mevedel-memory-list--run ,request ,reply)))
                            :sentinel
                            (lambda (child _event)
                              (when (memq (process-status child) '(exit signal))
                                (unwind-protect
                                    (mevedel-memory-list--received
                                     buffer job
                                     (condition-case failure
                                         (progn
                                           (unless (and (zerop (process-exit-status child)) (file-exists-p reply))
                                             (error "Memory display reader exited with status %s" (process-exit-status child)))
                                           (mevedel-journal-worker--read reply))
                                       (error (list :error (error-message-string failure)))))
                                  (when (file-directory-p directory) (delete-directory directory t))))))))
              (let ((inhibit-quit t))
                (plist-put job :timer
                           (run-at-time 180 nil
                                        (lambda ()
                                          (mevedel-memory-list--received buffer job '(:error "Memory display scan timed out"))))))))
          ;; The final timer registration may be the last evaluation before
          ;; returning. Deliver its deferred quit inside the cleanup owner.
          (when (and quit-flag (not inhibit-quit))
            (setq quit-flag nil)
            (signal 'quit nil)))
      ((error quit)
       (mevedel-memory-list--cancel)
       (setq mevedel-memory-list--error (error-message-string err))
       (when (eq (car err) 'quit) (signal (car err) (cdr err)))))))

(defun mevedel-memory-list-quit ()
  "Cancel display work and return to the invoking layout."
  (interactive)
  (mevedel-memory-list--close)
  (mevedel-cockpit-surface-quit))

(defun mevedel-memory-list-switch (view)
  "Show VIEW in the current memory cockpit, preserving each view's selection."
  (unless (memq view '(candidates memories history)) (user-error "Unknown memory view"))
  (mevedel-memory-list--remember-position)
  (setq mevedel-memory-list--view view)
  (mevedel-cockpit-surface-refresh (alist-get view mevedel-memory-list--positions))
  (mevedel-memory-list--restore-positions))

(defun mevedel-memory-list--candidate-count (context)
  "Return CONTEXT's last observed candidate count, or nil when unknown.
Count pending, recovery-required and unavailable rows, as Candidates shows."
  (when-let* ((observation (mevedel-workspace-memory-observation (mevedel-cockpit-context-workspace context))))
    (apply #'+ (mapcar (lambda (key) (or (plist-get observation key) 0)) '(:pending :recovery :unavailable)))))

(defun mevedel-memory-list-candidates ()
  "Show outstanding memory candidates."
  (interactive) (mevedel-memory-list-switch 'candidates))

(defun mevedel-memory-list-memories ()
  "Show currently stored memories."
  (interactive) (mevedel-memory-list-switch 'memories))

(defun mevedel-memory-list-history ()
  "Show retained memory decisions and completed reviews."
  (interactive) (mevedel-memory-list-switch 'history))

(cl-defun mevedel-memory-list--visible (context)
  "Return display-only rows for CONTEXT's selected view."
  ;; Report refreshes have their own buffer, not this cockpit's snapshot owner.
  (unless (derived-mode-p 'mevedel-memory-list-mode)
    (cl-return-from mevedel-memory-list--visible
      (append (mapcar #'mevedel-memory-list--display-row (mevedel-memory-list--collect context))
              (mevedel-memory-library-list (mevedel-cockpit-context-workspace context)))))
  ;; Known alerts open Candidates; otherwise Memories shows at once.  Passes,
  ;; decisions, changing recovery and cleanup discard the counts.
  (let ((counts (mevedel-memory-list--candidate-count context)))
    (unless (or mevedel-memory-list--view mevedel-memory-list--snapshot)
      (setq mevedel-memory-list--view (if (and counts (> counts 0)) 'candidates 'memories)))
    ;; Stored topics never need proposal history; Memories scans only to learn
    ;; unknown candidate counts for its tab label, and never switches views.
    (unless (or mevedel-memory-list--snapshot mevedel-memory-list--job mevedel-memory-list--error
                (and (eq mevedel-memory-list--view 'memories) counts))
      (mevedel-memory-list--start context)))
  (let* ((rows (plist-get mevedel-memory-list--snapshot :rows))
         (pending (seq-remove (lambda (row) (memq (plist-get row :status) '(applied rejected reversed completed))) rows)))
    (let ((items
           (pcase mevedel-memory-list--view
             ('candidates pending)
             ('memories
              (when (eq mevedel-memory-list--memories-state 'stale)
                (mevedel-memory-list--observe-memories context))
              mevedel-memory-list--memories)
             ('history (seq-filter (lambda (row) (memq (plist-get row :status) '(applied rejected reversed completed))) rows)))))
      (mevedel-memory-list--update-notice items)
      (mevedel-memory-library-copy items))))

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
        (mevedel-memory-list-refresh)))))

(defun mevedel-memory-list--collect (context)
  "Return persisted proposal rows for CONTEXT without writing or inferring."
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
         (writes (mevedel-memory-write-list workspace))
         (intents (make-hash-table :test #'equal))
         (passes (make-hash-table :test #'equal)) last-pass seen rows)
    (dolist (write writes)
      (when-let* ((intent (plist-get write :intent)))
        (puthash (plist-get intent :id) intent intents)))
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
                (clrhash passes)
                (when accepted
                  (puthash pass accepted passes)
                  (let ((date (plist-get (plist-get accepted :review) :created)))
                    (when (string> date (or last-pass "")) (setq last-pass date))))
                (when (and accepted (null (plist-get accepted :proposals)))
                  (push (list :id pass :kind 'review :pass pass :accepted accepted :status 'completed
                              :created (plist-get (plist-get accepted :review) :created)) rows))
                (dolist (proposal (plist-get accepted :proposals))
                  (let* ((id (plist-get proposal :id))
                         (scope (plist-get (plist-get accepted :prepared) :scope))
                         (root (cdr (assoc (plist-get proposal :root) (plist-get scope :roots))))
                         (decision (mevedel-memory-decision-status workspace id entries passes intents))
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
                                ;; A recorded stale decision may since have become applicable, or a pending
                                ;; proposal superseded; label rows by what accepting would do now.
                                :check (when (mevedel-memory-decision-actionable-status-p status)
                                         (plist-get (mevedel-memory-decision--application-input accepted proposal workspace)
                                                    :status))
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
                  (pcase (list (plist-get item :status) (plist-get item :check))
                    ('(stale fresh) "ready")
                    (`(,_ waiting) "awaits extraction")
                    (`(,_ stale) "superseded")
                    (`(,_ unavailable) "unavailable")
                    (`(,status ,_) (symbol-name status)))
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
          (concat
           (propertize (capitalize (symbol-name view)) 'local-map map 'mouse-face 'highlight
                       'face (if (eq view mevedel-memory-list--view) 'bold 'link))
           ;; Signal waiting candidates on their tab instead of switching to it.
           (when-let* (((eq view 'candidates))
                       (count (mevedel-memory-list--candidate-count context))
                       ((> count 0)))
             (propertize (format " (%d)" count) 'face 'warning 'local-map map 'mouse-face 'highlight)))))
      '(candidates memories history) " | ")
     (cond (mevedel-memory-list--error (format "    Scan failed: %s (g retries)" mevedel-memory-list--error))
           (mevedel-memory-list--job "    Loading history...")
           ((not mevedel-memory-list--snapshot) "    History not loaded")
           (t "    Committed snapshot (g refreshes)"))
     (if (and (eq mevedel-memory-list--view 'memories) mevedel-memory-list--memories-job)
         " | refreshing memories" "")
     (if mevedel-memory-list--recovery " | recovery pending" "")
     (if mevedel-memory-list--recovery-warning
         (concat " | " mevedel-memory-list--recovery-warning) "")
     "    "
     (mevedel-cockpit-format-header
      "memory" (mevedel-workspace-root workspace)
      (format "%s | history %d days%s"
              (cond ((not observation) "proposals not checked")
                    ;; Counts from an earlier scan, not this table's snapshot.
                    ((not mevedel-memory-list--snapshot)
                     (format "%d pending | %d recovery (cached)"
                             (or (plist-get observation :pending) 0) (or (plist-get observation :recovery) 0)))
                    (t (format "%d pending | %d recovery"
                               (or (plist-get observation :pending) 0) (or (plist-get observation :recovery) 0))))
              mevedel-memory-history-max-age-days
              (if-let* ((drain (mevedel-memory-pass-drain-status workspace)))
                  (format " | drain batch %d (%s), %s notes remain"
                          (1+ (plist-get drain :batch)) (plist-get drain :status)
                          (or (plist-get drain :notes-remaining) "?"))
                (if (mevedel-memory-pass-running workspace) " | running" "")))))))

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
      (or (mevedel--unified-diff (decode-coding-string (or (plist-get before :bytes) "") 'utf-8-unix)
                                 (decode-coding-string (or (plist-get after :bytes) "") 'utf-8-unix))
          "No text changes.\n"))))

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
         (status (or (plist-get decision :status) (plist-get item :status)))
         (intent (and decision (plist-get (mevedel-memory-decision--published workspace decision) :intent)))
         (input (unless (or intent (eq (plist-get decision :status) 'rejected))
                  (mevedel-memory-decision--application-input accepted proposal workspace)))
         (effective (if (eq (plist-get input :status) 'fresh) (plist-get input :proposal) proposal))
         (effective-scope (if (eq (plist-get input :status) 'fresh) (plist-get input :scope) scope))
         (before (or (plist-get intent :before) (plist-get effective :before)))
         (after (or (plist-get intent :after)
                    (mevedel-memory-write--after before (mevedel-memory-apply-changes effective-scope effective)))))
    (list
     :title "Memory proposal"
     :subtitle (format "%s · %s" (plist-get proposal :title) status)
     :identity (list (mevedel-workspace-root workspace) (plist-get item :pass) (plist-get item :id))
     :navigator t :initial 'body
     :validate (lambda () (mevedel-memory-scope--root scope (plist-get proposal :root)))
     :sections
     (append
      (list
       (list :id 'decision :title "Decision"
             :body (mevedel-report-fields
                    (list "Status" status)
                    (when-let* ((reason (plist-get proposal :reason))) (list "Reason" reason))
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
                                  (plist-get (plist-get accepted :prepared) :entries)))
                 (note (seq-find (lambda (note) (equal id (plist-get note :id)))
                                 (mevedel-memory-notes-documents scope))))
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
      (mevedel-memory-list-refresh))))

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
      (mevedel-memory-list-refresh))))

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
  (mevedel-memory-list-refresh))

(defun mevedel-memory-list-jobs ()
  "Inspect pending journal captures and their retry/discard actions."
  (interactive)
  (mevedel-journal-jobs (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context))))

(defun mevedel-memory-list-notes ()
  "Inspect the latest shared-folder observation: ready, waiting and blocked items."
  (interactive)
  (let* ((workspace (mevedel-cockpit-context-workspace (mevedel-cockpit-surface-context)))
         (observation (or (mevedel-memory-notes-observation workspace)
                          (user-error "No shared working folder has been observed yet"))))
    (mevedel-report-show
     "*mevedel shared notes*"
     (list :title "Shared working notes" :subtitle (format "Observed %s" (plist-get observation :at))
           :identity (list 'shared-notes (mevedel-workspace-root workspace))
           :sections
           (list (list :id 'summary :title "Latest review selection"
                       :body (mevedel-report-fields
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

(defun mevedel-memory-list--finished (workspace result)
  "Refresh WORKSPACE's open table and report settled consolidation RESULT."
  (when-let* ((buffer (get-buffer "*mevedel memory*")))
    (with-current-buffer buffer
      (when (and mevedel-cockpit--context (eq workspace (mevedel-cockpit-context-workspace mevedel-cockpit--context)))
        (condition-case nil (mevedel-memory-list-refresh) (error nil)))))
  (let ((notes (if (cl-some (lambda (key) (> (or (plist-get result key) 0) 0))
                            '(:notes-reviewed :notes-remaining :notes-blocked))
                   (format "; %d shared notes reviewed, %d waiting, %d blocked (s in the memory cockpit)"
                           (or (plist-get result :notes-reviewed) 0) (or (plist-get result :notes-remaining) 0)
                           (or (plist-get result :notes-blocked) 0))
                 "")))
    (pcase (plist-get result :outcome)
      ('success
       (if (eq (plist-get result :mode) 'auto)
           (message "mevedel: memory review updated %d files; %d proposals held, %d not applied; %d digests remain%s"
                    (plist-get result :updated-files) (plist-get result :held-count)
                    (plist-get result :unapplied-count) (or (plist-get result :remaining) 0) notes)
         (let ((entry (plist-get result :entry)))
           (message "mevedel: memory review covered %d digests, proposed %d changes; %d digests remain%s"
                    (length (plist-get entry :digests)) (length (plist-get entry :proposals))
                    (or (plist-get result :remaining) 0) notes))))
      ('aborted (message "mevedel: memory review cancelled"))
      ('restart-required (message "mevedel: %s" (plist-get result :error)))
      (_ (message "mevedel: memory review failed: %s" (or (plist-get result :error) "Unavailable"))))))

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

;;;###autoload
(defun mevedel-remember-drain (&optional context)
  "Review ready workspace notes and digests in successive bounded batches.
Use cockpit CONTEXT, or the current workspace.  Stop with `k' in the memory
cockpit, or `mevedel-memory-pass-cancel'.  Active turns wait rather than
capturing shared notes; failures stop the drain.  Automatic cost caps do not
limit this explicit command.  Proposals still follow the configured mode."
  (interactive)
  (setq context (or context (mevedel-cockpit-current-context)))
  (let* ((workspace (mevedel-cockpit-context-workspace context))
         (state
          (mevedel-memory-pass-drain
           workspace
           (lambda (result)
             (unless (eq (plist-get result :outcome) 'empty)
               (mevedel-memory-list--finished workspace result))
             (pcase (plist-get result :outcome)
               ('success
                (if (plist-get result :stopped)
                    (message "mevedel: memory drain stopped after batch %d; remaining decisions held"
                             (plist-get result :batch))
                  (message "mevedel: drain batch %d complete; %d notes and %d digests remain (k stops)"
                           (plist-get result :batch) (plist-get result :notes-remaining)
                           (plist-get result :remaining))))
               ('empty (message "mevedel: memory drain complete after %d batches; no ready notes or digests remain"
                                (plist-get result :batch))))))))
    (mevedel-memory-list-open context)
    state))

(defconst mevedel-memory-list--surface
  '(:buffer-name "*mevedel memory*" :label "memory" :row-label "item"
    :mode mevedel-memory-list-mode
    :format [("Action" 12 t) ("Type" 10 t) ("Title" 24 t) ("Status" 20 t) ("Target / origin" 0 t)]
    :setup mevedel-memory-list--setup
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
           ("D" "Drain ready notes and digests (k stops)" mevedel-remember-drain)
           ("j" "Inspect pending journal jobs" mevedel-memory-list-jobs)
           ("s" "Inspect shared-note backlog" mevedel-memory-list-notes)))
  "Shared cockpit surface for candidates, stored memories, and history.")

(define-derived-mode mevedel-memory-list-mode tabulated-list-mode "mevedel-memory"
  "Manage memory candidates, stored topics, and retained decisions."
  (mevedel-cockpit-setup-tabulated-surface mevedel-memory-list--surface)
  (local-set-key (kbd "g") #'mevedel-memory-list-refresh)
  (local-set-key (kbd "q") #'mevedel-memory-list-quit)
  (add-hook 'kill-buffer-hook #'mevedel-memory-list--close nil t)
  (add-hook 'change-major-mode-hook #'mevedel-memory-list--close nil t)
  ;; Keep native column headings in the header line; views use the tab line.
  (setq-local tab-line-format '(:eval (mevedel-cockpit-surface-header-line))))

(defun mevedel-memory-list--setup (context)
  "Request CONTEXT's checked recovery independently of its display scan."
  (let ((buffer (current-buffer))
        (workspace (mevedel-cockpit-context-workspace context)))
    (setq mevedel-memory-list--recovery-warning nil
          mevedel-memory-list--recovery
          (list :context context :root (mevedel-workspace-root workspace)
                :expires (+ (float-time) 180)
                ;; Compared at retirement: only a changing recovery stales reads.
                :generation (mevedel-memory-decision-recovery-generation workspace)))
    ;; The existing owner retains workspace claims, original target checks,
    ;; and live-buffer reconciliation. Closing the display does not cancel
    ;; workspace maintenance or grant a second operation its ownership.
    (condition-case err
        (progn
          (mevedel-memory-decision-schedule-recovery workspace)
          ;; Recovery coalesces by workspace, including work started outside
          ;; this cockpit. Follow that owner, not its optional callback.
          (let ((inhibit-quit t))
            (plist-put mevedel-memory-list--recovery :timer
                       (run-at-time .05 .05 #'mevedel-memory-list--observe-recovery buffer mevedel-memory-list--recovery))))
      ((error quit)
       (mevedel-memory-list--close)
       (setq mevedel-memory-list--error (error-message-string err))
       (signal (car err) (cdr err))))))

;;;###autoload
(defun mevedel-memory-list-open (&optional context)
  "Open CONTEXT's memory cockpit and request checked recovery without blocking."
  (interactive)
  (mevedel-cockpit-open-surface mevedel-memory-list--surface context))

(provide 'mevedel-memory-list)
;;; mevedel-memory-list.el ends here
