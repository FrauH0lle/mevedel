;;; mevedel-workspace-schedule-runs.el --- Workspace run history and control -*- lexical-binding: t; -*-

;;; Commentary:

;; The local cockpit and the workspace lobby change the same schedule on disk.

;;; Code:

(require 'mevedel-workspace-schedule)
(require 'mevedel-notify)
(require 'mevedel-session-durability)

(defun mevedel-workspace-schedule-set-paused (workspace id paused)
  "Pause WORKSPACE schedule ID, or resume it if PAUSED is nil.
Return the committed schedule.  Repeating the same operation is harmless."
  (mevedel-workspace-schedule-update
   workspace id
   (lambda (entry)
     (let ((status (plist-get entry :status))
           (desired (if paused 'paused 'active)))
       (unless (memq status '(active paused))
         (user-error "Schedule %s is %s" id status))
       (unless (eq status desired)
         (unless paused
           (let* ((now (mevedel-schedule-now))
                  (timing (plist-get entry :timing))
                  (due (plist-get entry :due))
                  (next (if (and due (or (> due now) (eq (plist-get timing :kind) 'once)))
                            due (mevedel-schedule-next-run timing now)))
                  (expiry (plist-get entry :expires-at)))
             (when (or (null next) (and expiry (> (max now next) expiry)))
               (user-error "No run remains before the expiry of %s" id))
             (setf (plist-get entry :due) next
                   (plist-get entry :failures) 0
                   (plist-get entry :pause-reason) nil)))
         (setf (plist-get entry :status) desired))
       entry))))

(defvar mevedel-turn-settlement-reason)

(defun mevedel-workspace-schedule--account (workspace run outcome &optional session error-text)
  "Commit RUN's terminal OUTCOME in WORKSPACE, notifying SESSION with ERROR-TEXT."
  (let* ((id (plist-get run :schedule-id))
         (at (plist-get run :scheduled-at))
         (job-path (mevedel-workspace-schedule--path workspace id "schedule.el"))
         (run-path (mevedel-workspace-schedule--run-path workspace id at))
         (failed (eq outcome 'failed)))
    (cl-loop repeat 5 do
             (pcase-let* ((`(,job . ,job-bytes) (mevedel-workspace-schedule--read job-path))
                          (`(,record . ,run-bytes) (mevedel-workspace-schedule--read run-path)))
               (unless (and job record (not (plist-get record :outcome))
                            (or (not (plist-member run :status))
                                (eq (plist-get run :status) (plist-get record :status))))
                 (cl-return nil))
               (let* ((counts (and (memq outcome '(done failed))
                                   (> at (or (plist-get job :last-counted-at) 0))))
                      (count (or (plist-get job :failures) 0))
                      (reason (or error-text (plist-get record :error) "request failed"))
                      (pause (and counts failed (= count 2) (eq 'active (plist-get job :status))
                                  (not (eq 'once (plist-get (plist-get job :timing) :kind))))))
                 (setf (plist-get record :outcome) outcome
                       (plist-get record :settled-at) (mevedel-schedule-now)
                       (plist-get record :notified) (or failed (plist-get record :notified)))
                 (when failed (setf (plist-get record :error) reason))
                 ;; A newer overlap skip owns the displayed outcome, not the streak.
                 (when (eql at (plist-get job :last-scheduled-at))
                   (setf (plist-get job :last-outcome) outcome))
                 (when counts
                   (setf (plist-get job :failures) (if failed (1+ count) 0)
                         (plist-get job :last-counted-at) at))
                 (when pause
                   (setf (plist-get job :status) 'paused
                         (plist-get job :pause-reason) (format "Three consecutive failures: %s" reason)))
                 (unless (mevedel-workspace-schedule--commit
                          workspace (list (list :op 'verify :path job-path :content job-bytes)
                                          (list :op 'verify :path run-path :content run-bytes)
                                          (list :op 'write :path run-path
                                                :content (mevedel-workspace-schedule--bytes record))
                                          (list :op 'write :path job-path
                                                :content (mevedel-workspace-schedule--bytes job))))
                   (when failed
                     (mevedel-notify (or session workspace) 'failure
                                     (format "Schedule %s failed: %s%s" (plist-get job :name) reason
                                             (if pause "; paused after three consecutive failures" ""))))
                   (mevedel-workspace-schedule--refresh workspace)
                   (cl-return record))))
             finally (error "Schedule run %s@%d keeps changing" id at))))

(defun mevedel-workspace-schedule-run-settled (run outcome)
  "Record the first root request's OUTCOME for RUN in its session buffer."
  (mevedel-workspace-schedule--account
   (mevedel-session-workspace mevedel--session) run
   (pcase outcome ('success 'done) ((or 'abort 'aborted) 'skipped) (_ 'failed))
   mevedel--session (bound-and-true-p mevedel-turn-settlement-reason)))

(defun mevedel-workspace-schedule-run-fired (workspace run)
  "Account for RUN in WORKSPACE if the firing itself was terminal."
  (pcase (plist-get run :status)
    ((and status (or 'skipped 'expired))
     (mevedel-workspace-schedule--account workspace run status))
    ('setup-error
     (let ((session (when-let* ((id (plist-get run :session-id)))
                      (cl-loop for (_ . buf) in (mevedel--workspace-sessions workspace)
                               for s = (buffer-local-value 'mevedel--session buf)
                               when (equal id (mevedel-session-session-id s)) return s))))
       (mevedel-workspace-schedule--account workspace run 'failed session)))))

(defun mevedel-workspace-schedule--run-lease (workspace run)
  "Assess WORKSPACE's RUN session lease, if it has a session."
  (when-let* ((id (plist-get run :session-id)))
    (mevedel-session-durability-cleanup-assessment
     (file-name-concat (mevedel-session-artifacts-sessions-dir workspace) id))))

(defun mevedel-workspace-schedule--host-parts (host)
  "Split a validated Linux holder HOST into boot, machine and PID namespace."
  (let ((parts (and (stringp host) (split-string host "/")))
        (case-fold-search nil))
    (pcase parts
      (`(,boot ,machine ,ns)
       (and (string-match-p mevedel--process-identity-regexp (concat boot ":0"))
            (not (equal boot "00000000-0000-0000-0000-000000000000"))
            (string-match-p "\\`[0-9a-f]\\{32\\}\\'" machine)
            (not (equal machine (make-string 32 ?0)))
            (string-match-p "\\`pid:\\[[1-9][0-9]*\\]\\'" ns) parts)))))

(defun mevedel-workspace-schedule--holder-dead-p (record)
  "Return non-nil for local process death or a proved reboot of RECORD's machine."
  (or (mevedel-session-durability--holder-dead-p record)
      (when-let* ((holder (plist-get record :holder)) ((proper-list-p holder))
                  (old (mevedel-workspace-schedule--host-parts (plist-get holder :host)))
                  (here (mevedel-workspace-schedule--host-parts (mevedel-session-durability--host)))
                  (process (plist-get holder :process)) (pid (plist-get holder :pid)))
        (let ((case-fold-search nil))
          (and (equal (nth 1 here) (nth 1 old)) (not (equal (car old) (car here)))
               (eql (user-uid) (plist-get holder :uid)) (natnump pid) (<= 1 pid 4194304)
               (stringp process) (string-match-p mevedel--process-identity-regexp process)
               (string-prefix-p (concat (car old) ":") process))))))

(defun mevedel-workspace-schedule-reconcile-run (workspace run &optional lease)
  "Settle an interrupted RUN in WORKSPACE, reusing LEASE if already assessed."
  (pcase (plist-get run :status)
    ((or 'skipped 'expired 'setup-error) (mevedel-workspace-schedule-run-fired workspace run))
    ((or 'launched 'claimed)
     (when (if (plist-get run :session-id)
               (let ((lease (or lease (mevedel-workspace-schedule--run-lease workspace run))))
                 ;; Expiry alone cannot distinguish a suspended foreign owner.
                 (or (and (eq 'available (plist-get lease :state))
                          (eq 'released (plist-get (plist-get lease :head) :status))
                          (not (memq 'transfer-reserved (plist-get lease :reasons))))
                     (and (eq 'expired (plist-get lease :state))
                          (mevedel-workspace-schedule--holder-dead-p (plist-get lease :head)))))
             (mevedel-workspace-schedule--holder-dead-p run))
       (mevedel-workspace-schedule--account
        workspace run 'failed nil "Run interrupted before its outcome was recorded")))))

(defun mevedel-workspace-schedule-run-blocks-p (workspace run due)
  "Return non-nil if unopened RUN must block WORKSPACE's firing at DUE."
  (unless (or (plist-get run :outcome) (plist-get run :settled-at))
    (let ((lease (mevedel-workspace-schedule--run-lease workspace run)))
      (not (or (mevedel-workspace-schedule-reconcile-run workspace run lease)
               (eq 'expired (plist-get lease :state))
               (and (eq 'claimed (plist-get run :status))
                    (< (plist-get run :scheduled-at) due)
                    (mevedel-workspace-schedule--account workspace run 'skipped)))))))

(provide 'mevedel-workspace-schedule-runs)
;;; mevedel-workspace-schedule-runs.el ends here
