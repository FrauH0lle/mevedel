;;; mevedel-schedules-list.el --- Workspace standing jobs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Inspect and manage the current workspace's standing jobs.  The schedule
;; store owns timing, mutations, and history; this table retains only row IDs
;; and a snapshot for display.

;;; Code:

(require 'mevedel-cockpit)
(require 'mevedel-report)
(require 'mevedel-workspace)

(autoload 'mevedel-schedule-timing-label "mevedel-schedule")
(autoload 'mevedel--display-chat-buffer "mevedel-chat")
(autoload 'mevedel-session-persistence-resume-id "mevedel-session-persistence")

;; `mevedel-workspace-schedule'
(autoload 'mevedel-workspace-schedule-list "mevedel-workspace-schedule")
(autoload 'mevedel-workspace-schedule-runs "mevedel-workspace-schedule")
(autoload 'mevedel-workspace-schedule-delete "mevedel-workspace-schedule")
(autoload 'mevedel-workspace-schedule-set-paused "mevedel-workspace-schedule-runs")

(defun mevedel-schedules-list--time (seconds)
  "Format SECONDS as a local date and time, or a dash when absent."
  (if seconds (format-time-string "%Y-%m-%d %H:%M" seconds) "—"))

(defun mevedel-schedules-list--entry (job _context)
  "Return JOB's tabulated entry."
  (list (plist-get job :id)
        (vector (plist-get job :name) (symbol-name (plist-get job :status))
                (mevedel-schedules-list--time (plist-get job :due))
                (mevedel-schedule-timing-label (plist-get job :timing)))))

(defun mevedel-schedules-list--run-line (workspace run)
  "Describe RUN, linking to its saved session in WORKSPACE when present."
  (let ((id (plist-get run :session-id)))
    (concat (mevedel-schedules-list--time (or (plist-get run :fired-at)
                                              (plist-get run :scheduled-at)))
            "  " (symbol-name (or (plist-get run :outcome) (plist-get run :status)))
            (if (plist-get run :notified) "  notified" "  quiet")
            (when id
              (concat "  "
                      (buttonize (copy-sequence id)
                                 (lambda (_)
                                   (if-let* ((data (mevedel-session-persistence-resume-id workspace id)))
                                       (mevedel--display-chat-buffer data)
                                     (mevedel-diagnostic-user-error (mevedel-diagnostic-scope workspace)
                                                                    "Run session unavailable: %s" id)))
                                 nil "Open run session")))
            (when-let* ((message (plist-get run :error))) (concat "  " message)))))

(defun mevedel-schedules-list--details (job context)
  "Return JOB's details and most recent runs first for CONTEXT."
  (let* ((ws (mevedel-cockpit-context-workspace context))
         (id (plist-get job :id)) (runs (mevedel-workspace-schedule-runs ws id)))
    (list :title (plist-get job :name) :subtitle id
          :sections (list (list :id 'job :title "Standing job"
                                :body (mevedel-report-fields
                                       (list "Status" (plist-get job :status))
                                       (when-let* ((reason (plist-get job :pause-reason)))
                                         (list "Pause reason" reason))
                                       (list "Timing" (mevedel-schedule-timing-label (plist-get job :timing)))
                                       (list "Next run" (mevedel-schedules-list--time (plist-get job :due)))
                                       (list "Expires" (mevedel-schedules-list--time (plist-get job :expires-at)))))
                          (list :id 'history :title "Run history"
                                :body (if runs
                                          (mapconcat (lambda (run) (mevedel-schedules-list--run-line ws run))
                                                     (reverse runs) "\n")
                                        "No runs yet."))))))

(defun mevedel-schedules-list--set-paused (paused)
  "Set the selected job's PAUSED state and refresh its row."
  (let ((id (plist-get (mevedel-cockpit-surface-selected) :id)))
    (mevedel-workspace-schedule-set-paused (mevedel-cockpit-context-workspace) id paused)))

(defun mevedel-schedules-list-pause ()
  "Pause the selected standing job."
  (interactive)
  (mevedel-schedules-list--set-paused t))

(defun mevedel-schedules-list-resume ()
  "Resume the selected standing job."
  (interactive)
  (mevedel-schedules-list--set-paused nil))

(defun mevedel-schedules-list-delete ()
  "Delete the selected standing job after confirmation."
  (interactive)
  (let* ((job (mevedel-cockpit-surface-selected))
         (id (plist-get job :id))
         (workspace (mevedel-cockpit-context-workspace)))
    (when (yes-or-no-p (format "Delete standing job %s (%s)? " (plist-get job :name) id))
      (mevedel-workspace-schedule-delete workspace id))))

(defconst mevedel-schedules-list--surface
  '(:label "standing jobs" :row-label "job" :buffer-name "*mevedel standing jobs*"
    :mode mevedel-schedules-list-mode
    :keys (("p" "Pause job" mevedel-schedules-list-pause)
           ("r" "Resume job" mevedel-schedules-list-resume)
           ("d" "Delete job" mevedel-schedules-list-delete))
    :format [("Job" 20 t) ("Status" 8 t) ("Next run" 16 t) ("Timing" 16 t)]
    :entry mevedel-schedules-list--entry
    :details mevedel-schedules-list--details
    :collect (lambda (context)
               (mevedel-workspace-schedule-list (mevedel-cockpit-context-workspace context)))
    :header (lambda (jobs context)
              (mevedel-cockpit-format-header
               "standing jobs" (mevedel-workspace-name (mevedel-cockpit-context-workspace context))
               (format "%d jobs" (length jobs)))))
  "Standing jobs cockpit surface.")

(define-derived-mode mevedel-schedules-list-mode tabulated-list-mode "Standing Jobs"
  "Browse the workspace's standing jobs."
  (mevedel-cockpit-setup-tabulated-surface mevedel-schedules-list--surface))

(defun mevedel-schedules-list--refresh-workspace (workspace)
  "Refresh the standing jobs cockpit if it shows WORKSPACE's store."
  (when-let* ((buffer (get-buffer (plist-get mevedel-schedules-list--surface :buffer-name))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'mevedel-schedules-list-mode)
                 (equal (mevedel-workspace-state-dir workspace)
                        (mevedel-workspace-state-dir (mevedel-cockpit-context-workspace))))
        (mevedel-cockpit-surface-refresh)))))

(add-hook 'mevedel-workspace-schedule-changed-functions #'mevedel-schedules-list--refresh-workspace)

;;;###autoload
(defun mevedel-schedules-list-open (&optional context)
  "Open the standing jobs of CONTEXT or the current workspace."
  (interactive)
  (mevedel-cockpit-open-surface
   mevedel-schedules-list--surface
   (or context (mevedel-cockpit-context-for-buffer (current-buffer))
       (mevedel-cockpit-workspace-context
        (or (mevedel-workspace)
            (mevedel-diagnostic-user-error (mevedel-diagnostic-scope (current-buffer))
                                           "No workspace here"))
        (current-buffer)))))

(provide 'mevedel-schedules-list)
;;; mevedel-schedules-list.el ends here
