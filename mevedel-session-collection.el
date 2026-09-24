;;; mevedel-session-collection.el --- Incremental publication cleanup -*- lexical-binding: t -*-

;;; Commentary:

;; Session startup and settlement only schedule reclamation.  Immutable history
;; is scanned in idle slices; a changed publication restarts the scan before
;; any deletion.  Publication owns retention and the final mutation authority.

;;; Code:

(require 'cl-lib)
(require 'mevedel-session-publication)
(require 'mevedel-journal-worker)
(require 'mevedel-structs)
(require 'mevedel-transport)

;; `mevedel-request'
(defvar mevedel--current-request)

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
    (when-let* ((worker (plist-get job :worker)))
      (when (process-live-p worker) (delete-process worker)))
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

(defun mevedel-session-collection--observed (session job directory head generations result)
  "Accept JOB's immutable observations for HEAD and GENERATIONS from RESULT.
Cache observations only; the editor still checks authority and pins at deletion."
  (when mevedel-journal-worker--stopping
    (mevedel-session-collection-cancel session))
  (when (eq job (gethash session mevedel-session-collection--jobs))
    (plist-put job :worker nil)
    (condition-case err
        (let ((observations (plist-get result :observations))
              (current-directory (mevedel-session-save-path session)))
          (unless (equal directory current-directory)
            (error "Collection source directory changed"))
          (when (plist-get result :error) (error "%s" (plist-get result :error)))
          (unless (equal (mapcar #'car observations)
                         (mapcar (lambda (generation) (plist-get generation :head)) generations))
            (error "Collection observations name another generation set"))
          (dolist (observation observations)
            (let ((key (mevedel-session-publication--publication-path directory (car observation))))
              (when (nth 1 observation)
                (puthash key (nth 1 observation) mevedel-session-publication--generation-cache))
              (when (nth 2 observation)
                (puthash key (nth 2 observation) mevedel-session-publication--facts-cache))))
          (plist-put job :worker-scanned head)
          (mevedel-session-collection--arm session job))
      (error
       (mevedel-session-collection-cancel session)
       (display-warning 'mevedel
                        (format "Could not prepare publication collection: %s" (error-message-string err))
                        :warning)))))

(defun mevedel-session-collection--resumed (session job error)
  "Continue JOB for SESSION after a deleting program, reporting ERROR."
  (when (eq job (gethash session mevedel-session-collection--jobs))
    (plist-put job :pending nil)
    (if (not error)
        (mevedel-session-collection--arm session job)
      (mevedel-session-collection-cancel session)
      (display-warning 'mevedel
                       (format "Could not collect published generations: %s"
                               (error-message-string error)) :warning))))

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
           ((or (plist-get job :worker) (plist-get job :pending)) nil)
           (t
            (mevedel-transport-with-exclusive-connection
             (mevedel-session-durability-with-transaction
              ;; Ownership is proved on the target where collection deletes;
              ;; reading immutable generations needs only the local lease.
              (let ((head (plist-get (mevedel-session-publication session) :head))
                    (deadline (+ (float-time) mevedel-session-collection--slice-seconds))
                    (count 0))
                (unless (equal head (plist-get job :head))
                  (setf (plist-get job :head) head
                        (plist-get job :summaries) nil
                        (plist-get job :plan) nil
                        (plist-get job :remaining)
                        (mevedel-session-publication--generation-names directory)))
                (when (and (not (equal head (plist-get job :worker-scanned)))
                           (not (file-remote-p directory))
                           (mevedel-session-workspace session)
                           (mevedel-journal-worker-supported-p (mevedel-session-workspace session)))
                  (let ((cold
                         (seq-filter
                          (lambda (generation)
                            (let ((key (mevedel-session-publication--publication-path directory (plist-get generation :head))))
                              (not (and (gethash key mevedel-session-publication--generation-cache)
                                        (gethash key mevedel-session-publication--facts-cache)))))
                          (plist-get job :remaining))))
                    (when (> (length cold) 16)
                      (plist-put job :worker
                                 (mevedel-journal-worker-start
                                  (mevedel-session-workspace session) 'generation-observations
                                  (lambda (result) (mevedel-session-collection--observed session job directory head cold result))
                                  nil (list :directory directory :generations cold))))))
                (while (and (not (plist-get job :worker))
                            (plist-get job :remaining)
                            (< count 8)
                            (or (zerop count) (< (float-time) deadline))
                            (not (input-pending-p)))
                  (push (mevedel-session-publication-generation-summary
                         directory (car (plist-get job :remaining)) t)
                        (plist-get job :summaries))
                  (setf (plist-get job :remaining) (cdr (plist-get job :remaining)))
                  (setq count (1+ count)))
                (if (or (plist-get job :remaining) (> count 0) (input-pending-p))
                    (unless (plist-get job :worker)
                      (mevedel-session-collection--arm session job))
                  (unless (plist-get job :plan)
                    (setf (plist-get job :plan)
                          (mevedel-session-publication-collection-plan
                           session (reverse (plist-get job :summaries)))))
                  (let (failure)
                    (pcase (mevedel-session-publication-collect-step
                            session (plist-get job :plan)
                            (lambda (error)
                              ;; A background program reports from a sentinel,
                              ;; perhaps inside another remote command.
                              (if (plist-get job :pending)
                                  (mevedel-transport-run-at-time
                                   0 #'mevedel-session-collection--resumed session job error)
                                (setq failure error))))
                      ('pending (plist-put job :pending t))
                      ('nil (mevedel-session-collection-cancel session))
                      (_ (when failure (signal (car failure) (cdr failure)))
                         (mevedel-session-collection--arm session job)))))))))))

      (error
       (mevedel-session-collection-cancel session)
       (display-warning 'mevedel
                        (format "Could not collect published generations: %s"
                                (error-message-string err)) :warning)))))

(provide 'mevedel-session-collection)
;;; mevedel-session-collection.el ends here
