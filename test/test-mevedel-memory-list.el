;;; test-mevedel-memory-list.el -- Memory proposal cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise persisted proposals through the real table and decision commands.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-list)
(require 'mevedel-report-test-support
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name))
          "mevedel-report-test-support"))
(require 'mevedel-menu)
(require 'mevedel-skills-ui)
(require 'gptel-openai)
(require 'mevedel-view)
(require 'mevedel-system)

(mevedel-deftest mevedel-memory-list-open
    (:vars* ((root (make-temp-file "mevedel-memory-cockpit-" t))
             (memory (file-name-concat root "memory"))
             (workspace (mevedel-workspace--create :root root :type 'project))
             (session (mevedel-session-create "memory-test" workspace))
             (data (generate-new-buffer " *memory-test-data*"))
             (view (generate-new-buffer " *memory-test-view*"))
             (mevedel-memory-dirs (list memory)) context claim id)
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (when-let* ((buffer (get-buffer "*mevedel memory proposals*"))) (kill-buffer buffer))
                  (when (buffer-live-p view) (kill-buffer view))
                  (when (buffer-live-p data) (kill-buffer data))
                  (delete-directory root t)))
  (progn
    (make-directory memory)
    (write-region "Original topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
    (write-region "- [Topic](topic.md) - Original\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (mevedel-workspace-identity-ensure root)
    (with-current-buffer data
      (setq-local mevedel--session session default-directory (file-name-as-directory root)))
    (mevedel-view--setup view data)
    (with-current-buffer view (setq context (mevedel-cockpit-current-context)))
    (let* ((scope (mevedel-memory-scope-capture workspace))
           (root-id (caar (plist-get scope :roots)))
           (_ (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
           (prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
           (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                  "type: \"project\"\ntitle: \"Topic\"\nhook: \"Current\"\nreason: \"New evidence\"\nevidence: []\n"
                                  "---\nUpdated topic.\n```\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id))
           (accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil)))
      (setq id (plist-get (car (plist-get accepted :proposals)) :id))
      (mevedel-memory-store-publish workspace (plist-get prepared :id)))
    ,test)
  (test)
  :doc "the main cockpit reports pending and recovery counts from an observation without disturbing a draft"
  (let ((draft "> quoted\nsecond line") intent)
    (with-current-buffer view (mevedel-view-test--insert-composer-draft draft 4))
    (cl-letf (((symbol-function 'transient-setup) #'ignore))
      (with-current-buffer view (mevedel-menu-open 'top)))
    (should (= 1 (plist-get (mevedel-memory-list-summary context) :pending)))
    (with-current-buffer view
      (should (string-search "1 pending" (mevedel-menu--memory-description))))
    (let* ((accepted (mevedel-memory-store-accepted workspace
                      (plist-get (car (mevedel-memory-list--collect context)) :pass)))
           (scope (plist-get (plist-get accepted :prepared) :scope))
           (proposal (car (plist-get accepted :proposals))))
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
      (mevedel-memory-write-call
       scope (plist-get proposal :root)
       (lambda (target) (setq intent (mevedel-memory-write-prepare workspace claim target accepted proposal))))
      (mevedel-journal-claim-settle claim 'cancelled ""))
    (should (= 1 (plist-get (mevedel-memory-list-summary context) :pending)))
    (setf (plist-get (mevedel-workspace-memory-observation workspace) :at) 0)
    (should (= 1 (plist-get (mevedel-memory-list-summary context) :recovery)))
    (with-current-buffer view
      (should (string-search "1 recovery" (mevedel-menu--memory-description))))
    (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash))
    (mevedel-memory-decision-reject workspace (plist-get intent :pass) id)
    (should (= 0 (plist-get (mevedel-memory-list-summary context t) :pending)))
    (with-current-buffer view
      (should-not (string-search "1 recovery" (mevedel-menu--memory-description)))
      (should (equal draft (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 4)))))
  :doc "shows captured evidence and applies a proposal without disturbing a composer draft"
  (let ((draft "> quoted\nsecond line"))
    (with-current-buffer view (mevedel-view-test--insert-composer-draft draft 4))
    (with-current-buffer (save-window-excursion (mevedel-memory-list-open context))
      (should (derived-mode-p 'tabulated-list-mode))
      (mevedel-cockpit-goto-id id)
      (should (eq 'pending (plist-get (mevedel-cockpit-surface-selected) :status)))
		     (save-window-excursion
		       (unwind-protect
			   (progn
			     (mevedel-cockpit-surface-details)
			     (with-current-buffer "*mevedel memory proposal*"
			       (should (string-search "Updated topic." (buffer-string)))
			       (should-not (string-search "Original topic." (buffer-string)))
			       (mevedel-report-select-section '(change . "topic.md"))
			       (should (string-search "Original topic." (buffer-string)))
			       (should (string-search "Updated topic." (buffer-string)))
			       (mevedel-report-select-section 'decision)
			       (should (string-search "New evidence" (buffer-string)))))
			 (when (get-buffer "*mevedel memory proposal*")
			   (kill-buffer "*mevedel memory proposal*"))))
      (mevedel-memory-list-accept)
      (should (eq 'applied (plist-get (mevedel-cockpit-surface-selected) :status)))
      (mevedel-memory-list-reverse)
      (should (eq 'reversed (plist-get (mevedel-cockpit-surface-selected) :status)))
      (mevedel-cockpit-surface-refresh))
    (with-current-buffer view
      (should (equal draft (mevedel-view--input-text)))
      (should (= (point) (+ (mevedel-view--input-start) 4)))))
  :doc "rejects the selected proposal with a retained reason and no target writes"
  (with-current-buffer (save-window-excursion (with-current-buffer view (mevedel-menu-open 'memory)))
    (mevedel-cockpit-goto-id id)
    (mevedel-memory-list-reject "This is already documented.")
    (should (eq 'rejected (plist-get (mevedel-cockpit-surface-selected) :status)))
    (should (string-search "This is already documented."
					  (mevedel-report-test-text (mevedel-memory-list--details (mevedel-cockpit-surface-selected) context))))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
  :doc "another active pass does not prevent read-only inspection or lose its ownership"
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (with-current-buffer (save-window-excursion (mevedel-memory-list-open context))
      (mevedel-cockpit-goto-id id)
      (should (eq 'pending (plist-get (mevedel-cockpit-surface-selected) :status)))
      (should-error (mevedel-memory-list-accept)))
    (should-not (mevedel-journal-claim-outcome claim))
    (should (equal claim (mevedel-journal-claim-current (mevedel-memory-store--claim-directory workspace)))))
  :doc "accepted proposals remain inspectable when their public review cannot be republished"
  (let* ((entry (seq-find (lambda (entry) (eq (plist-get entry :kind) 'consolidation)) (mevedel-journal-store-entries root)))
         (path (file-name-concat (mevedel-journal-store-directory root) (plist-get entry :file))))
    (write-region "Damaged public review.\n" nil path nil 'silent)
    (with-current-buffer (save-window-excursion (mevedel-memory-list-open context))
      (mevedel-cockpit-goto-id id)
      (should (equal id (plist-get (mevedel-cockpit-surface-selected) :id)))
		     (should (string-search "Updated topic." (mevedel-report-test-text (mevedel-memory-list--details (mevedel-cockpit-surface-selected) context))))
      (should-error (mevedel-memory-list-accept))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))
  :doc "inspection includes retained digest evidence even when its public entry is unavailable"
  (let* ((digest (mevedel-journal-store-publish-digest
                  root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                             :workspace (mevedel-workspace-identity-read root) :trigger 'session-end :segment 1
                             :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (make-string 64 ?c))
                             :created "2026-09-07T12:00:00Z" :model "test:model")
                  "## Done\n- Observed: Frozen lesson.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
         (scope (mevedel-memory-scope-capture workspace))
         (_ (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
         (prepared (mevedel-memory-store-prepare workspace claim scope (list digest) ""))
         (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                "type: \"project\"\ntitle: \"Topic\"\nhook: \"Current\"\nreason: \"Retain evidence\"\nevidence: [%S]\n"
                                "---\nUpdated topic.\n```\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none")
                        (caar (plist-get scope :roots)) (plist-get digest :id)))
         (accepted (mevedel-memory-store-accept workspace prepared reply (list digest) "test:model" nil))
         (proposal (car (plist-get accepted :proposals))))
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (delete-file (file-name-concat (mevedel-journal-store-directory root) (plist-get digest :file)))
    (with-current-buffer (save-window-excursion (mevedel-memory-list-open context))
      (mevedel-cockpit-goto-id (plist-get proposal :id))
		     (let ((details (mevedel-report-test-text (mevedel-memory-list--details (mevedel-cockpit-surface-selected) context))))
        (should (string-search "Observed: Frozen lesson." details))
        (should (string-search (concat "memory://journal/" (plist-get digest :file)) details)))))
  :doc "the remember command starts a focused sessionless pass and preserves the draft on completion"
  (let* ((gptel--known-backends nil)
         (model (make-symbol "memory-command-model"))
         (backend (gptel-make-openai "memory-command-test" :key "test-only" :models (list model)))
         (real-request (symbol-function 'gptel-request))
         (draft "> retained draft\nsecond line") request provider)
    (put model :context-window 128)
    (put model :capabilities '(tool-use))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-model-resolve-workload) (lambda (&rest _) (list :backend backend :model model)))
                  ((symbol-function 'gptel-request)
                   (lambda (prompt &rest args)
                     (let ((fsm (apply real-request prompt (plist-put (copy-sequence args) :dry-run t))))
                       (unless (plist-get args :dry-run) (setq request fsm provider (plist-get args :callback))) fsm))))
          (with-current-buffer view
            (mevedel-view-test--insert-composer-draft draft 4)
            (save-window-excursion (mevedel-cmd--remember "maintenance")))
          (should (eq 'mevedel-cmd--remember (cdr (assoc "remember" mevedel-slash-commands))))
          (let ((state (mevedel-memory-pass-running workspace)))
            (should state)
            (should (equal "maintenance" (plist-get (plist-get state :prepared) :focus)))
            (with-current-buffer (plist-get (plist-get state :request) :buffer) (should-not mevedel--session)))
          (mevedel-test--with-captured-diagnostics nil
            (funcall provider "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No changes." (gptel-fsm-info request))
            (gptel--fsm-transition request 'DONE))
          (should-not (mevedel-memory-pass-running workspace))
          (should (seq-some (lambda (entry) (equal "maintenance" (plist-get entry :focus))) (mevedel-journal-store-entries root)))
          (save-window-excursion (mevedel-remember "" context))
          (let ((running-buffer (plist-get (plist-get (mevedel-memory-pass-running workspace) :request) :buffer)))
            (save-window-excursion
              (with-current-buffer (get-buffer "*mevedel memory proposals*")
                (mevedel-memory-list-running)
			       (should (derived-mode-p 'mevedel-report-mode))
			       (should-not (eq running-buffer (current-buffer)))
			       (should (buffer-live-p running-buffer))
			       (should (string-search "Model response" (buffer-string)))))
            (with-current-buffer (get-buffer "*mevedel memory proposals*")
              (mevedel-test--with-captured-diagnostics nil (mevedel-memory-list-kill))))
          (should-not (mevedel-memory-pass-running workspace))
          (should (= 2 (seq-count (lambda (entry) (eq (plist-get entry :kind) 'consolidation)) (mevedel-journal-store-entries root))))
          (with-current-buffer view
            (should (equal draft (mevedel-view--input-text)))
            (should (= (point) (+ (mevedel-view--input-start) 4)))))
		     (mevedel-test--with-captured-diagnostics nil (mevedel-memory-pass-cancel workspace))
		     (when (get-buffer "*mevedel running consolidation*")
		       (kill-buffer "*mevedel running consolidation*"))))
  :doc "foreign client roots remain visible but private details and writes are unavailable"
  (cl-letf (((symbol-function 'mevedel-workspace-identity-client) (lambda () (make-string 64 ?f))))
    (with-current-buffer (save-window-excursion (mevedel-memory-list-open context))
      (mevedel-cockpit-goto-id id)
      (should (eq 'unavailable (plist-get (mevedel-cockpit-surface-selected) :status)))
		     (should-error (mevedel-report-test-text (mevedel-memory-list--details (mevedel-cockpit-surface-selected) context)))
      (should-error (mevedel-memory-list-accept))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))))

(provide 'test-mevedel-memory-list)
;;; test-mevedel-memory-list.el ends here
