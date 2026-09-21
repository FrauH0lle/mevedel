;;; mevedel-session-collection.el --- Incremental publication cleanup -*- lexical-binding: t -*-

;;; Commentary:

;; Session startup and settlement only schedule reclamation.  Immutable history
;; is scanned in idle slices; a changed publication restarts the scan before
;; any deletion.  Publication owns retention and the final mutation authority.

;;; Code:

(require 'cl-lib)
(require 'mevedel-session-publication)
(require 'mevedel-structs)
(require 'mevedel-transport)

;; `mevedel-request'
(defvar mevedel--current-request)

;; `mevedel-session-recovery'
(defvar mevedel-session-recovery--mutation-cache)

;; `mevedel-structs'
(defvar mevedel--session)

(defvar mevedel-session-collection--jobs (make-hash-table :test #'eq)
  "Session to its coalesced collection job and idle timer.")

(defvar mevedel-session-collection--slice-seconds 0.05
  "Maximum time before yielding between generation reads.")

(defun mevedel-session-collection-cancel (session)
  "Cancel SESSION's pending collection and release its scan state."
  (when-let* ((job (gethash session mevedel-session-collection--jobs)))
    (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
    (remhash session mevedel-session-collection--jobs)
    (when (buffer-live-p (plist-get job :buffer))
      (with-current-buffer (plist-get job :buffer)
        (remove-hook 'kill-buffer-hook #'mevedel-session-collection--on-kill t)))))

(defun mevedel-session-collection--on-kill ()
  "Cancel the current root buffer's publication collection."
  (when mevedel--session (mevedel-session-collection-cancel mevedel--session)))

(defun mevedel-session-collection--arm (session job)
  "Schedule the next idle slice for SESSION and JOB."
  (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
  (setf (plist-get job :timer)
        (run-with-idle-timer
         (+ 0.2 (if-let* ((idle (current-idle-time))) (float-time idle) 0))
         nil #'mevedel-session-collection--step session job)))

(defun mevedel-session-collection-schedule (session)
  "Schedule bounded publication cleanup for SESSION without doing target I/O."
  (when (and (mevedel-session-codec-portable-authority-p session)
             (mevedel-session-save-path session)
             (buffer-live-p (mevedel-session-root-buffer session))
             (not (gethash session mevedel-session-collection--jobs)))
    (let ((job (list :buffer (mevedel-session-root-buffer session)
                     :head nil :remaining nil :summaries nil :plan nil :timer nil)))
      (puthash session job mevedel-session-collection--jobs)
      (with-current-buffer (plist-get job :buffer)
        (add-hook 'kill-buffer-hook #'mevedel-session-collection--on-kill nil t))
      (mevedel-session-collection--arm session job))))

(defun mevedel-session-collection--step (session job)
  "Advance JOB by a bounded slice, preserving SESSION's live ownership."
  (when (eq job (gethash session mevedel-session-collection--jobs))
    (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
    (setf (plist-get job :timer) nil)
    (condition-case err
        (let ((buffer (plist-get job :buffer))
              (directory (mevedel-session-save-path session)))
          (cond
           ((or (not (buffer-live-p buffer))
                (not (eq buffer (mevedel-session-root-buffer session)))
                (not (plist-get (mevedel-session-publication session) :head))
                (not (eq 'owned (plist-get (mevedel-session-lease session) :state))))
            (mevedel-session-collection-cancel session))
           ((or (input-pending-p)
                (active-minibuffer-window)
                (mevedel-transport-busy-p directory)
                (buffer-local-value 'mevedel--current-request buffer)
                (mevedel-session-pending-publication session)
                (mevedel-session-publication-uncommitted-batches session)
                (mevedel-session-publication-queue session)
                (mevedel-session-publication-active-p session))
            (mevedel-session-collection--arm session job))
           (t
            (mevedel-transport-with-exclusive-connection
              (mevedel-session-durability-with-transaction
                (if (not (mevedel-session-durability-lease-owned-p session))
                    (mevedel-session-collection-cancel session)
                  (let ((head (plist-get (mevedel-session-publication session) :head))
                        (deadline (+ (float-time) mevedel-session-collection--slice-seconds))
                        (count 0))
                    (unless (equal head (plist-get job :head))
                      (setf (plist-get job :head) head
                            (plist-get job :summaries) nil
                            (plist-get job :plan) nil
                            (plist-get job :remaining)
                            (mevedel-session-publication--generation-names directory)))
                    (while (and (plist-get job :remaining)
                                (< count 8)
                                (or (zerop count) (< (float-time) deadline))
                                (not (input-pending-p)))
                      (push (mevedel-session-publication-generation-summary
                             directory (car (plist-get job :remaining)) t)
                            (plist-get job :summaries))
                      (setf (plist-get job :remaining) (cdr (plist-get job :remaining)))
                      (setq count (1+ count)))
                    (if (or (plist-get job :remaining) (> count 0) (input-pending-p))
                        (mevedel-session-collection--arm session job)
                      (unless (plist-get job :plan)
                        (setf (plist-get job :plan)
                              (mevedel-session-publication-collection-plan
                               session (reverse (plist-get job :summaries)))))
                      (if (mevedel-session-publication-collect-step
                           session (plist-get job :plan))
                          (mevedel-session-collection--arm session job)
                        (mevedel-session-collection-cancel session))))))))))

      (error
       (mevedel-session-collection-cancel session)
       (display-warning 'mevedel
                        (format "Could not collect published generations: %s"
                                (error-message-string err)) :warning)))))

(provide 'mevedel-session-collection)
;;; mevedel-session-collection.el ends here
