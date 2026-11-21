;;; niwa-journal-jobs.el -- Inspect and control pending digests -*- lexical-binding: t -*-

;;; Commentary:

;; A small read-only job browser with explicit retry, evidence inspection,
;; and discard commands.  Decisions are revalidated by the processing owner;
;; this view grants no model tools or session mutation authority.

;;; Code:

(require 'niwa-report)

(eval-when-compile (require 'cl-lib))
(require 'button)
(require 'niwa-journal-process)
(require 'niwa-memory-list)
(require 'niwa-workspace)

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-workspace "niwa-cockpit" (&optional context))
(autoload 'niwa-cockpit-context-workspace "niwa-cockpit")

;; `niwa-utilities'
(declare-function niwa-diagnostic-scope "niwa-utilities" (owner))
(declare-function niwa-diagnostic-user-error "niwa-utilities" (scope format &rest args))
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")

(defvar-local niwa-journal-jobs--workspace nil
  "Workspace inspected by the current journal jobs buffer.")

(defun niwa-journal-jobs--workspace ()
  "Return the current journal or session workspace."
  (or niwa-journal-jobs--workspace
      (condition-case nil
          (niwa-cockpit-context-workspace)
        (user-error nil))
      (niwa-workspace)
      (niwa-diagnostic-user-error 'unavailable "No workspace in this buffer")))

(defun niwa-journal-jobs--records (workspace)
  "Return inspectable pending job states for WORKSPACE without mutation."
  (let ((entries (niwa-journal-store-entries (niwa-workspace-root workspace))))
    (mapcar
     (lambda (capture)
       (let ((id (plist-get capture :id)) status detail attempts)
         (condition-case err
             (let* ((claim (niwa-journal-claim-current (niwa-journal-process--attempts workspace capture)))
                    (outcome (and claim (niwa-journal-claim-outcome claim))))
               (setq attempts (or (plist-get claim :generation) 0))
               (setq status
                     (cond
                      ((niwa-journal-discard-accepted outcome id)
                       (setq detail "Accepted discard awaiting completion") 'recovery)
                      ((or (niwa-journal-store-entry-for-capture entries id)
                           (eq 'completed (plist-get outcome :status)))
                       (setq detail (if (plist-get capture :unreadable)
                                        "Restore the descriptor to recover the accepted result"
                                      "Accepted result awaiting publication or pin release")) 'recovery)
                      ((plist-get capture :unreadable)
                       (setq detail (plist-get capture :error)) 'unreadable)
                      ((and claim (not outcome)
                            (< (niwa-session-control-fs-target-time (plist-get claim :directory))
                               (plist-get claim :expires-at))) 'running)
                      ((not (niwa-journal-capture--marked-p workspace id "ready"))
                       (setq detail "Checkpoint publication is incomplete; recovery must verify source ownership")
                       'recovery)
                      ((not (niwa-journal-capture-trigger workspace capture)) 'checkpoint)
                      ((condition-case unavailable
                           (progn (niwa-journal-process--policy workspace capture) nil)
                         (error (setq detail (error-message-string unavailable)) t)) 'unavailable)
                      ((>= attempts niwa-journal-process--automatic-attempts) 'exhausted)
                      ((> attempts 0) 'failed)
                      (t 'pending)))
               (when (and (eq status 'recovery) (not (plist-get capture :unreadable)))
                 (condition-case unavailable
                     (niwa-journal-capture--source-directory workspace capture)
                   (error (setq detail (error-message-string unavailable)))))
               (when (and (not detail) outcome
                          (memq (plist-get outcome :status) '(failed cancelled)))
                 (setq detail (plist-get (json-parse-string (plist-get outcome :payload) :object-type 'plist) :error))))
           (error (setq status 'unreadable detail (error-message-string err))))
         (list :id id :capture capture :status status :attempts attempts :detail detail)))
     (let ((captures (niwa-journal-capture-list workspace t)))
       (cl-loop for capture in captures
                for retired in (niwa-session-control-fs-paths-exist
                                (mapcar (lambda (capture)
                                          (niwa-journal-capture--file
                                           workspace (plist-get capture :id) "retired"))
                                        captures))
                unless retired collect capture)))))

(defun niwa-journal-jobs--retained (workspace)
  "Describe overdue public evidence still stored in WORKSPACE.
Observe existing review coverage, capture state and evidence pins. These rows
explain retention for humans; they grant neither recall nor deletion authority."
  (let* ((root (niwa-journal-store-directory (niwa-workspace-root workspace)))
         (entries (niwa-journal-store-entries (niwa-workspace-root workspace)))
         (unreviewed (niwa-journal-index-unreviewed entries))
         (now (float-time))
         (proposals (when (seq-some
                          (lambda (entry)
                            (and (eq (plist-get entry :kind) 'consolidation)
                                 (plist-get entry :proposals)
                                 (not (niwa-journal-store-recall-p entry now)))) entries)
                      (niwa-memory-list--collect (list :workspace workspace))))
         rows)
    (dolist (entry entries (nreverse rows))
      (unless (niwa-journal-store-recall-p entry now)
        (let (reasons)
          (when (memq entry unreviewed) (push "Unreviewed" reasons))
          (pcase (plist-get entry :kind)
            ('digest
             (let ((capture (file-name-concat (niwa-journal-store-state-directory root) "captures" (plist-get entry :capture-id))))
               (when (equal '(t nil) (niwa-session-control-fs-paths-exist
                                      (list capture (file-name-concat capture "retired"))))
                 (push "Capture completion pending" reasons)))
             (when (niwa-session-control-fs-list-directory
                    (file-name-concat (niwa-journal-store-state-directory root) "evidence-pins" (plist-get entry :id)) "\\`[^.]")
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
                     ('unavailable (push "Proposal requires inspection" reasons))
                     (_ (unless (niwa-memory-decision-terminal-status-p state)
                          (push "Unresolved proposal decision" reasons)))))
                 (unless states (push "Proposal acceptance requires inspection" reasons)))
               (when (seq-some
                      (lambda (other)
                        (and (eq (plist-get other :kind) 'decision)
                             (equal (plist-get entry :pass-id) (plist-get other :pass-id))
                             (niwa-journal-store-recall-p other now))) entries)
                 (push "Recent decision evidence" reasons))))
            ('decision (push "Retained with its consolidation history" reasons)))
          (push (list :entry entry
                      :age (floor (/ (- now (float-time (date-to-time (plist-get entry :created)))) 86400))
                      :reasons (or (nreverse reasons) '("Awaiting safe cleanup"))) rows))))))

(defun niwa-journal-jobs--selection ()
  "Return the workspace and selected pending job identity for a command."
  (let* ((workspace (niwa-journal-jobs--workspace))
         (at-point (get-text-property (point) 'niwa-journal-capture-id))
         (choices (mapcar (lambda (job)
                            (cons (format "%s %s" (plist-get job :id) (plist-get job :status))
                                  (plist-get job :id)))
                          (niwa-journal-jobs--records workspace))))
    (unless choices (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "No pending journal jobs"))
    (list workspace (or (and (member at-point (mapcar #'cdr choices)) at-point)
                        (cdr (assoc (completing-read "Journal job: " choices nil t) choices))))))

(defvar niwa-journal-jobs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map niwa-report-mode-map)
    (define-key map (kbd "g") #'niwa-journal-jobs)
    (define-key map (kbd "RET") #'niwa-journal-inspect)
    (define-key map (kbd "r") #'niwa-journal-retry)
    (define-key map (kbd "d") #'niwa-journal-discard)
    map)
  "Keys for journal job inspection and explicit decisions.")

(define-derived-mode niwa-journal-jobs-mode niwa-report-mode "Journal jobs"
  "Inspect pending journal work; RET inspects, r retries, and d discards.")

;;;###autoload
(defun niwa-journal-jobs (&optional workspace)
  "Inspect pending jobs and overdue retained journal evidence in WORKSPACE."
  (interactive)
  (setq workspace (or workspace (niwa-journal-jobs--workspace)))
  (let* ((jobs (niwa-journal-jobs--records workspace))
         (retained (niwa-journal-jobs--retained workspace))
         (pending-text (with-temp-buffer
			 (if (not jobs) (insert "No pending jobs.\n")
			   (dolist (job jobs)
			     (let* ((start (point))
				    (metadata (plist-get (plist-get job :capture) :metadata)))
			       (insert (format "[%s] %s | %s | attempts %s\n%s\n%s\n\n"
					       (plist-get job :status) (or (plist-get metadata :session-name) "unknown session")
					       (or (plist-get metadata :model) "unknown model") (or (plist-get job :attempts) "unknown")
					       (plist-get job :id) (or (plist-get job :detail) "")))
			       (add-text-properties start (point) (list 'niwa-journal-capture-id (plist-get job :id))))))
                         (buffer-string)))
         (retained-text (with-temp-buffer
			  (insert (format "\nOverdue retained evidence: %d | review mode: %s\n"
					  (length retained) niwa-memory-consolidation-mode))
			  (when retained
			    (insert "Hidden from ordinary recall; retained until review and recovery dependencies resolve.\n")
			    (insert-text-button "Review unprocessed evidence"
						'action (lambda (_) (niwa-remember nil (list :workspace workspace)))
						'follow-link t)
			    (insert "   ")
			    (insert-text-button "Inspect proposals and recovery"
						'action (lambda (_) (niwa-memory-list-open (list :workspace workspace)))
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
					   (let ((fresh (niwa-journal-store-read (niwa-workspace-root workspace) file)))
					     (niwa-report-show
					      "*niwa journal evidence*"
					      (list :title "Retained journal evidence"
						    :subtitle file :identity (list workspace file)
						    :sections (list (list :id 'evidence :title "Retained evidence"
									  :body (plist-get fresh :text)
									  :mode 'markdown-mode))))))
				 'follow-link t)
				(insert "\n\n"))))
                          (buffer-string)))
         (buffer (niwa-report-show
                  "*niwa journal jobs*"
                  (list :title "Journal jobs" :subtitle (niwa-workspace-root workspace)
                        :identity workspace :mode 'niwa-journal-jobs-mode
                        :sections (list (list :id 'jobs :title "Pending jobs" :body pending-text)
                                        (list :id 'retained :title "Overdue retained evidence"
                                              :body retained-text))))))
    (with-current-buffer buffer
      (setq-local niwa-journal-jobs--workspace workspace
                  header-line-format "RET inspect · r retry · d discard · g refresh · TAB next heading/link · q close"))
    buffer))

;;;###autoload
(defun niwa-journal-jobs--capture-report (workspace id)
  "Inspect frozen evidence and diagnostics for pending capture ID in WORKSPACE."
  (let ((job (cl-find id (niwa-journal-jobs--records workspace)
                      :key (lambda (job) (plist-get job :id)) :test #'equal)))
    (unless job (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Journal capture is no longer pending"))
    (list :title "Journal capture" :subtitle (format "%s · %s" id (plist-get job :status))
          :identity (list workspace id) :navigator t :initial 'evidence
          :refresh (lambda () (niwa-journal-jobs--capture-report workspace id))
          :sections
          (list (list :id 'diagnostics :title "Capture diagnostics"
                      :body (concat (niwa-report-fields
                                     (list "Capture" id)
                                     (list "Status" (plist-get job :status))
                                     (list "Attempts" (or (plist-get job :attempts) "unknown")))
                                    (or (plist-get job :detail) "")))
                (list :id 'evidence :title "Frozen evidence" :mode 'markdown-mode
                      :body (condition-case err
                                (niwa-journal-capture-evidence workspace (plist-get job :capture))
                              (error (format "Evidence unavailable: %s\n" (error-message-string err)))))))))

;;;###autoload
(defun niwa-journal-inspect (workspace id)
  "Inspect frozen evidence and diagnostics for pending capture ID in WORKSPACE."
  (interactive (niwa-journal-jobs--selection))
  (niwa-report-show "*niwa journal evidence*"
                       (niwa-journal-jobs--capture-report workspace id)))

;;;###autoload
(defun niwa-journal-retry (workspace id)
  "Retry pending capture ID in WORKSPACE once, retaining its frozen model policy."
  (interactive (niwa-journal-jobs--selection))
  (niwa-journal-process-retry workspace id)
  (niwa-journal-jobs workspace))

;;;###autoload
(defun niwa-journal-discard (workspace id &optional original-source)
  "Discard pending capture ID in WORKSPACE and record its omission.
For an unreadable descriptor, ORIGINAL-SOURCE must hold the original pin.
Interactive use asks for that missing source instead of guessing it."
  (interactive (niwa-journal-jobs--selection))
  (let ((result
         (condition-case err
             (niwa-journal-process-discard workspace id original-source)
           (niwa-journal-discard-source-required
            (if (called-interactively-p 'interactive)
                (niwa-journal-process-discard
                 workspace id (read-directory-name "Descriptor unreadable; original source directory: " nil nil t))
              (signal (car err) (cdr err)))))))
    (niwa-journal-jobs workspace)
    (message (if (eq 'digest (plist-get result :kind))
                 "niwa: recovered the already accepted digest"
               "niwa: journal capture discarded"))
    result))

(provide 'niwa-journal-jobs)
;;; niwa-journal-jobs.el ends here
