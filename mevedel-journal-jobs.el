;;; mevedel-journal-jobs.el -- Inspect and control pending digests -*- lexical-binding: t -*-

;;; Commentary:

;; A small read-only job browser with explicit retry, evidence inspection,
;; and discard commands.  Decisions are revalidated by the processing owner;
;; this view grants no model tools or session mutation authority.

;;; Code:

(require 'mevedel-report)

(eval-when-compile (require 'cl-lib))
(require 'button)
(require 'mevedel-journal-process)
(require 'mevedel-memory-list)
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

(defun mevedel-journal-jobs--retained (workspace)
  "Describe overdue public evidence still stored in WORKSPACE.
Observe existing review coverage, capture state and evidence pins. These rows
explain retention for humans; they grant neither recall nor deletion authority."
  (let* ((root (mevedel-journal-store-directory (mevedel-workspace-root workspace)))
         (entries (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
         (unreviewed (mevedel-journal-index-unreviewed entries))
         (now (float-time))
         (proposals (when (seq-some
                          (lambda (entry)
                            (and (eq (plist-get entry :kind) 'consolidation)
                                 (plist-get entry :proposals)
                                 (not (mevedel-journal-store-recall-p entry now)))) entries)
                      (mevedel-memory-list--collect (list :workspace workspace))))
         rows)
    (dolist (entry entries (nreverse rows))
      (unless (mevedel-journal-store-recall-p entry now)
        (let (reasons)
          (when (memq entry unreviewed) (push "Unreviewed" reasons))
          (pcase (plist-get entry :kind)
            ('digest
             (let ((capture (file-name-concat (mevedel-journal-store-state-directory root) "captures" (plist-get entry :capture-id))))
               (when (and (mevedel-session-control-fs-path-exists-p capture)
                          (not (mevedel-session-control-fs-path-exists-p
                                (file-name-concat capture "retired"))))
                 (push "Capture completion pending" reasons)))
             (when (mevedel-session-control-fs-list-directory
                    (file-name-concat (mevedel-journal-store-state-directory root) "evidence-pins" (plist-get entry :id)) "\\`[^.]")
               (push "Pinned for review, proposal or recovery" reasons)))
            ('consolidation
             (when (seq-some (lambda (other)
                              (and (eq (plist-get other :kind) 'digest)
                                   (member (plist-get other :id) (plist-get entry :digests)))) entries)
               (push "Reviewed digest evidence still retained" reasons))
             (when (plist-get entry :proposals)
               (let ((states (delete-dups
                              (mapcar (lambda (row) (plist-get row :status))
                                      (seq-filter
                                       (lambda (row) (equal (plist-get entry :pass-id) (plist-get row :pass)))
                                       proposals)))))
                 (dolist (state states)
                   (pcase state
                     ((or 'pending 'stale) (push "Awaiting proposal decisions" reasons))
                     ('recovery-required (push "Write recovery required" reasons))
                     ('unavailable (push "Proposal or recovery evidence requires inspection" reasons))
                     (_ (unless (mevedel-memory-decision-terminal-status-p state)
                          (push "Unresolved proposal decision" reasons)))))
                 (unless states (push "Proposal acceptance requires inspection" reasons)))
               (when (seq-some
                      (lambda (other)
                        (and (eq (plist-get other :kind) 'decision)
                             (equal (plist-get entry :pass-id) (plist-get other :pass-id))
                             (mevedel-journal-store-recall-p other now))) entries)
                 (push "Recent decision evidence" reasons))))
            ('decision (push "Retained with its consolidation history" reasons)))
          (push (list :entry entry
                      :age (floor (/ (- now (float-time (date-to-time (plist-get entry :created)))) 86400))
                      :reasons (or (nreverse reasons) '("Awaiting safe cleanup"))) rows))))))

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
    (set-keymap-parent map mevedel-report-mode-map)
    (define-key map (kbd "g") #'mevedel-journal-jobs)
    (define-key map (kbd "RET") #'mevedel-journal-inspect)
    (define-key map (kbd "r") #'mevedel-journal-retry)
    (define-key map (kbd "d") #'mevedel-journal-discard)
    map)
  "Keys for journal job inspection and explicit decisions.")

(define-derived-mode mevedel-journal-jobs-mode mevedel-report-mode "Journal jobs"
  "Inspect pending journal work; RET inspects, r retries, and d discards.")

;;;###autoload
(defun mevedel-journal-jobs (&optional workspace)
  "Inspect pending jobs and overdue retained journal evidence in WORKSPACE."
  (interactive)
  (setq workspace (or workspace (mevedel-journal-jobs--workspace)))
  (let* ((jobs (mevedel-journal-jobs--records workspace))
         (retained (mevedel-journal-jobs--retained workspace))
         (pending-text (with-temp-buffer
			 (if (not jobs) (insert "No pending jobs.\n")
			   (dolist (job jobs)
			     (let* ((start (point))
				    (metadata (plist-get (plist-get job :capture) :metadata)))
			       (insert (format "[%s] %s | %s | attempts %s\n%s\n%s\n\n"
					       (plist-get job :status) (or (plist-get metadata :session-name) "unknown session")
					       (or (plist-get metadata :model) "unknown model") (or (plist-get job :attempts) "unknown")
					       (plist-get job :id) (or (plist-get job :detail) "")))
			       (add-text-properties start (point) (list 'mevedel-journal-capture-id (plist-get job :id))))))
                         (buffer-string)))
         (retained-text (with-temp-buffer
			  (insert (format "\nOverdue retained evidence: %d | review mode: %s\n"
					  (length retained) mevedel-memory-consolidation-mode))
			  (when retained
			    (insert "Hidden from ordinary recall; retained until review and recovery dependencies resolve.\n")
			    (insert-text-button "Review unprocessed evidence"
						'action (lambda (_) (mevedel-remember nil (list :workspace workspace)))
						'follow-link t)
			    (insert "   ")
			    (insert-text-button "Inspect proposals and recovery"
						'action (lambda (_) (mevedel-memory-list-open (list :workspace workspace)))
						'follow-link t)
			    (insert "\n\n")
			    (dolist (row retained)
			      (let* ((entry (plist-get row :entry))
				     (file (plist-get entry :file)))
				(insert (format "%s | %d days | %s\n%s\n"
						(plist-get entry :kind) (plist-get row :age)
						(string-join (plist-get row :reasons) "; ") file))
				(insert-text-button
				 "Inspect retained entry"
				 'action (lambda (_)
					   (let ((fresh (mevedel-journal-store-read (mevedel-workspace-root workspace) file)))
					     (mevedel-report-show
					      "*mevedel journal evidence*"
					      (list :title "Retained journal evidence"
						    :subtitle file :identity (list workspace file)
						    :sections (list (list :id 'evidence :title "Retained evidence"
									  :body (plist-get fresh :text)
									  :mode 'markdown-mode))))))
				 'follow-link t)
				(insert "\n\n"))))
                          (buffer-string)))
         (buffer (mevedel-report-show
                  "*mevedel journal jobs*"
                  (list :title "Journal jobs" :subtitle (mevedel-workspace-root workspace)
                        :identity workspace :mode 'mevedel-journal-jobs-mode
                        :sections (list (list :id 'jobs :title "Pending jobs" :body pending-text)
                                        (list :id 'retained :title "Overdue retained evidence"
                                              :body retained-text))))))
    (with-current-buffer buffer
      (setq-local mevedel-journal-jobs--workspace workspace
                  header-line-format "RET inspect · r retry · d discard · g refresh · TAB next heading/link · q close"))
    buffer))

;;;###autoload
(defun mevedel-journal-jobs--capture-report (workspace id)
  "Inspect frozen evidence and diagnostics for pending capture ID in WORKSPACE."
  (let ((job (cl-find id (mevedel-journal-jobs--records workspace)
                      :key (lambda (job) (plist-get job :id)) :test #'equal)))
    (unless job (user-error "Journal capture is no longer pending"))
    (list :title "Journal capture" :subtitle (format "%s · %s" id (plist-get job :status))
          :identity (list workspace id) :navigator t :initial 'evidence
          :refresh (lambda () (mevedel-journal-jobs--capture-report workspace id))
          :sections
          (list (list :id 'diagnostics :title "Capture diagnostics"
                      :body (concat (mevedel-report-fields
                                     (list "Capture" id)
                                     (list "Status" (plist-get job :status))
                                     (list "Attempts" (or (plist-get job :attempts) "unknown")))
                                    (or (plist-get job :detail) "")))
                (list :id 'evidence :title "Frozen evidence" :mode 'markdown-mode
                      :body (condition-case err
                                (mevedel-journal-capture-evidence workspace (plist-get job :capture))
                              (error (format "Evidence unavailable: %s\n" (error-message-string err)))))))))

;;;###autoload
(defun mevedel-journal-inspect (workspace id)
  "Inspect frozen evidence and diagnostics for pending capture ID in WORKSPACE."
  (interactive (mevedel-journal-jobs--selection))
  (mevedel-report-show "*mevedel journal evidence*"
                       (mevedel-journal-jobs--capture-report workspace id)))

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
