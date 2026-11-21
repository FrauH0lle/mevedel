;;; niwa-schedules-list.el --- Workspace standing jobs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Inspect and manage the current workspace's standing jobs.  The schedule
;; store owns timing, mutations, and history; this table retains only row IDs
;; and a snapshot for display.

;;; Code:

(require 'niwa-cockpit)
(require 'niwa-report)
(require 'niwa-workspace)

(autoload 'niwa-schedule-timing-label "niwa-schedule")
(autoload 'niwa--display-chat-buffer "niwa-chat")
(autoload 'niwa-session-persistence-resume-id "niwa-session-persistence")

;; `niwa-workspace-schedule'
(autoload 'niwa-workspace-schedule-list "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-runs "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-delete "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-set-paused "niwa-workspace-schedule-runs")

(defun niwa-schedules-list--time (seconds)
  "Format SECONDS as a local date and time, or a dash when absent."
  (if seconds (format-time-string "%Y-%m-%d %H:%M" seconds) "—"))

(defun niwa-schedules-list--entry (job _context)
  "Return JOB's tabulated entry."
  (list (plist-get job :id)
        (vector (plist-get job :name) (symbol-name (plist-get job :status))
                (niwa-schedules-list--time (plist-get job :due))
                (niwa-schedule-timing-label (plist-get job :timing)))))

(defun niwa-schedules-list--run-line (workspace run)
  "Describe RUN, linking to its saved session in WORKSPACE when present."
  (let ((id (plist-get run :session-id)))
    (concat (niwa-schedules-list--time (or (plist-get run :fired-at)
                                              (plist-get run :scheduled-at)))
            "  " (symbol-name (or (plist-get run :outcome) (plist-get run :status)))
            (if (plist-get run :notified) "  notified" "  quiet")
            (when id
              (concat "  "
                      (buttonize (copy-sequence id)
                                 (lambda (_)
                                   (if-let* ((data (niwa-session-persistence-resume-id workspace id)))
                                       (niwa--display-chat-buffer data)
                                     (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace)
                                                                    "Run session unavailable: %s" id)))
                                 nil "Open run session")))
            (when-let* ((message (plist-get run :error))) (concat "  " message)))))

(defun niwa-schedules-list--details (job context)
  "Return JOB's details and most recent runs first for CONTEXT."
  (let* ((ws (niwa-cockpit-context-workspace context))
         (id (plist-get job :id)) (runs (niwa-workspace-schedule-runs ws id)))
    (list :title (plist-get job :name) :subtitle id
          :sections (list (list :id 'job :title "Standing job"
                                :body (niwa-report-fields
                                       (list "Status" (plist-get job :status))
                                       (when-let* ((reason (plist-get job :pause-reason)))
                                         (list "Pause reason" reason))
                                       (list "Timing" (niwa-schedule-timing-label (plist-get job :timing)))
                                       (list "Next run" (niwa-schedules-list--time (plist-get job :due)))
                                       (list "Expires" (niwa-schedules-list--time (plist-get job :expires-at)))))
                          (list :id 'history :title "Run history"
                                :body (if runs
                                          (mapconcat (lambda (run) (niwa-schedules-list--run-line ws run))
                                                     (reverse runs) "\n")
                                        "No runs yet."))))))

(defun niwa-schedules-list--set-paused (paused)
  "Set the selected job's PAUSED state and refresh its row."
  (let ((id (plist-get (niwa-cockpit-surface-selected) :id)))
    (niwa-workspace-schedule-set-paused (niwa-cockpit-context-workspace) id paused)))

(defun niwa-schedules-list-pause ()
  "Pause the selected standing job."
  (interactive)
  (niwa-schedules-list--set-paused t))

(defun niwa-schedules-list-resume ()
  "Resume the selected standing job."
  (interactive)
  (niwa-schedules-list--set-paused nil))

(defun niwa-schedules-list-delete ()
  "Delete the selected standing job after confirmation."
  (interactive)
  (let* ((job (niwa-cockpit-surface-selected))
         (id (plist-get job :id))
         (workspace (niwa-cockpit-context-workspace)))
    (when (yes-or-no-p (format "Delete standing job %s (%s)? " (plist-get job :name) id))
      (niwa-workspace-schedule-delete workspace id))))

(defconst niwa-schedules-list--surface
  '(:label "standing jobs" :row-label "job" :buffer-name "*niwa standing jobs*"
    :mode niwa-schedules-list-mode
    :keys (("p" "Pause job" niwa-schedules-list-pause)
           ("r" "Resume job" niwa-schedules-list-resume)
           ("d" "Delete job" niwa-schedules-list-delete))
    :format [("Job" 20 t) ("Status" 8 t) ("Next run" 16 t) ("Timing" 16 t)]
    :entry niwa-schedules-list--entry
    :details niwa-schedules-list--details
    :collect (lambda (context)
               (niwa-workspace-schedule-list (niwa-cockpit-context-workspace context)))
    :header (lambda (jobs context)
              (niwa-cockpit-format-header
               "standing jobs" (niwa-workspace-name (niwa-cockpit-context-workspace context))
               (format "%d jobs" (length jobs)))))
  "Standing jobs cockpit surface.")

(define-derived-mode niwa-schedules-list-mode tabulated-list-mode "Standing Jobs"
  "Browse the workspace's standing jobs."
  (niwa-cockpit-setup-tabulated-surface niwa-schedules-list--surface))

(defun niwa-schedules-list--refresh-workspace (workspace)
  "Refresh the standing jobs cockpit if it shows WORKSPACE's store."
  (when-let* ((buffer (get-buffer (plist-get niwa-schedules-list--surface :buffer-name))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'niwa-schedules-list-mode)
                 (equal (niwa-workspace-state-dir workspace)
                        (niwa-workspace-state-dir (niwa-cockpit-context-workspace))))
        (niwa-cockpit-surface-refresh)))))

(add-hook 'niwa-workspace-schedule-changed-functions #'niwa-schedules-list--refresh-workspace)

;;;###autoload
(defun niwa-schedules-list-open (&optional context)
  "Open the standing jobs of CONTEXT or the current workspace."
  (interactive)
  (niwa-cockpit-open-surface
   niwa-schedules-list--surface
   (or context (niwa-cockpit-context-for-buffer (current-buffer))
       (niwa-cockpit-workspace-context
        (or (niwa-workspace)
            (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
                                           "No workspace here"))
        (current-buffer)))))

(provide 'niwa-schedules-list)
;;; niwa-schedules-list.el ends here
