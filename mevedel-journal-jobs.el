;;; mevedel-journal-jobs.el -- Inspect and control pending digests -*- lexical-binding: t -*-

;;; Commentary:

;; A small read-only job browser with explicit retry, evidence inspection,
;; and discard commands.  Decisions are revalidated by the processing owner;
;; this view grants no model tools or session mutation authority.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'mevedel-journal-process)
(require 'mevedel-workspace)

;; `mevedel-cockpit'
(declare-function mevedel-cockpit-context-workspace "mevedel-cockpit" (&optional context))
(autoload 'mevedel-cockpit-context-workspace "mevedel-cockpit")

(defvar-local mevedel-journal-jobs--workspace nil
  "Workspace inspected by the current journal jobs buffer.")

(defun mevedel-journal-jobs--workspace ()
  "Return the current journal or session workspace."
  (or mevedel-journal-jobs--workspace
      (condition-case nil
          (mevedel-cockpit-context-workspace)
        (user-error nil))
      (mevedel-workspace)
      (user-error "No workspace in this buffer")))

(defun mevedel-journal-jobs--records (workspace)
  "Return inspectable pending job states for WORKSPACE without mutation."
  (let ((entries (mevedel-journal-store-entries (mevedel-workspace-root workspace))))
    (mapcar
     (lambda (capture)
       (let ((id (plist-get capture :id)) status detail attempts)
         (condition-case err
             (let* ((claim (mevedel-journal-claim-current (mevedel-journal-process--attempts workspace capture)))
                    (outcome (and claim (mevedel-journal-claim-outcome claim))))
               (setq attempts (or (plist-get claim :generation) 0))
               (setq status
                     (cond
                      ((mevedel-journal-discard-accepted outcome id)
                       (setq detail "Accepted discard awaiting completion") 'recovery)
                      ((or (mevedel-journal-store-entry-for-capture entries id)
                           (eq 'completed (plist-get outcome :status)))
                       (setq detail (if (plist-get capture :unreadable)
                                        "Restore the descriptor to recover the accepted result"
                                      "Accepted result awaiting publication or pin release")) 'recovery)
                      ((plist-get capture :unreadable)
                       (setq detail (plist-get capture :error)) 'unreadable)
                      ((and claim (not outcome)
                            (< (mevedel-session-control-fs-target-time (plist-get claim :directory))
                               (plist-get claim :expires-at))) 'running)
                      ((not (mevedel-journal-capture--marked-p workspace id "ready"))
                       (setq detail "Checkpoint publication is incomplete; recovery must verify source ownership")
                       'recovery)
                      ((not (mevedel-journal-capture-trigger workspace capture)) 'checkpoint)
                      ((condition-case unavailable
                           (progn (mevedel-journal-process--policy workspace capture) nil)
                         (error (setq detail (error-message-string unavailable)) t)) 'unavailable)
                      ((>= attempts 3) 'exhausted)
                      ((> attempts 0) 'failed)
                      (t 'pending)))
               (when (and (eq status 'recovery) (not (plist-get capture :unreadable)))
                 (condition-case unavailable
                     (mevedel-journal-capture--source-directory workspace capture)
                   (error (setq detail (error-message-string unavailable)))))
               (when (and (not detail) outcome
                          (memq (plist-get outcome :status) '(failed cancelled)))
                 (setq detail (plist-get (json-parse-string (plist-get outcome :payload) :object-type 'plist) :error))))
           (error (setq status 'unreadable detail (error-message-string err))))
         (list :id id :capture capture :status status :attempts attempts :detail detail)))
     (cl-remove-if
      (lambda (capture) (mevedel-journal-capture--marked-p workspace (plist-get capture :id) "retired"))
      (mevedel-journal-capture-list workspace t)))))

(defun mevedel-journal-jobs--selection ()
  "Return the workspace and selected pending job identity for a command."
  (let* ((workspace (mevedel-journal-jobs--workspace))
         (at-point (get-text-property (point) 'mevedel-journal-capture-id))
         (choices (mapcar (lambda (job)
                            (cons (format "%s %s" (plist-get job :id) (plist-get job :status))
                                  (plist-get job :id)))
                          (mevedel-journal-jobs--records workspace))))
    (unless choices (user-error "No pending journal jobs"))
    (list workspace (or (and (member at-point (mapcar #'cdr choices)) at-point)
                        (cdr (assoc (completing-read "Journal job: " choices nil t) choices))))))

(defvar mevedel-journal-jobs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mevedel-journal-jobs)
    (define-key map (kbd "RET") #'mevedel-journal-inspect)
    (define-key map (kbd "r") #'mevedel-journal-retry)
    (define-key map (kbd "d") #'mevedel-journal-discard)
    map)
  "Keys for journal job inspection and explicit decisions.")

(define-derived-mode mevedel-journal-jobs-mode special-mode "Journal jobs"
  "Inspect pending journal work; RET inspects, r retries, and d discards.")

;;;###autoload
(defun mevedel-journal-jobs (&optional workspace)
  "Inspect pending journal jobs in WORKSPACE, including failed or unreadable jobs."
  (interactive)
  (setq workspace (or workspace (mevedel-journal-jobs--workspace)))
  (let ((jobs (mevedel-journal-jobs--records workspace))
        (buffer (get-buffer-create "*mevedel journal jobs*")))
    (with-current-buffer buffer
      (mevedel-journal-jobs-mode)
      (setq-local mevedel-journal-jobs--workspace workspace)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Journal jobs: %s\n\nRET inspect   r retry   d discard   g refresh\n\n"
                        (mevedel-workspace-root workspace)))
        (if (not jobs) (insert "No pending jobs.\n")
          (dolist (job jobs)
            (let* ((start (point))
                   (metadata (plist-get (plist-get job :capture) :metadata)))
              (insert (format "[%s] %s | %s | attempts %s\n%s\n%s\n\n"
                              (plist-get job :status) (or (plist-get metadata :session-name) "unknown session")
                              (or (plist-get metadata :model) "unknown model") (or (plist-get job :attempts) "unknown")
                              (plist-get job :id) (or (plist-get job :detail) "")))
              (add-text-properties start (point) (list 'mevedel-journal-capture-id (plist-get job :id))))))
        (goto-char (point-min))))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun mevedel-journal-inspect (workspace id)
  "Inspect frozen evidence and diagnostics for pending capture ID in WORKSPACE."
  (interactive (mevedel-journal-jobs--selection))
  (let ((job (cl-find id (mevedel-journal-jobs--records workspace)
                      :key (lambda (job) (plist-get job :id)) :test #'equal)))
    (unless job (user-error "Journal capture is no longer pending"))
    (with-help-window "*mevedel journal evidence*"
      (princ (format "Capture %s\nStatus: %s\nAttempts: %s\n%s\n\n"
                     id (plist-get job :status) (or (plist-get job :attempts) "unknown") (or (plist-get job :detail) "")))
      (condition-case err
          (princ (mevedel-journal-capture-evidence workspace (plist-get job :capture)))
        (error (princ (format "Evidence unavailable: %s\n" (error-message-string err))))))))

;;;###autoload
(defun mevedel-journal-retry (workspace id)
  "Retry pending capture ID in WORKSPACE once, retaining its frozen model policy."
  (interactive (mevedel-journal-jobs--selection))
  (mevedel-journal-process-retry workspace id)
  (mevedel-journal-jobs workspace))

;;;###autoload
(defun mevedel-journal-discard (workspace id &optional original-source)
  "Discard pending capture ID in WORKSPACE and record its omission.
For an unreadable descriptor, ORIGINAL-SOURCE must hold the original pin.
Interactive use asks for that missing source instead of guessing it."
  (interactive (mevedel-journal-jobs--selection))
  (let ((result
         (condition-case err
             (mevedel-journal-process-discard workspace id original-source)
           (mevedel-journal-discard-source-required
            (if (called-interactively-p 'interactive)
                (mevedel-journal-process-discard
                 workspace id (read-directory-name "Descriptor unreadable; original source directory: " nil nil t))
              (signal (car err) (cdr err)))))))
    (mevedel-journal-jobs workspace)
    (message (if (eq 'digest (plist-get result :kind))
                 "mevedel: recovered the already accepted digest"
               "mevedel: journal capture discarded"))
    result))

(provide 'mevedel-journal-jobs)
;;; mevedel-journal-jobs.el ends here
