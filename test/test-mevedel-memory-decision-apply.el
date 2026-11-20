;;; test-mevedel-memory-decision-apply.el -- Recoverable memory application -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise accepted proposals through root ownership, writes, and decisions.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-session-control-fs)
(require 'mevedel-memory-write)
(require 'mevedel-journal-capture)
(require 'mevedel-journal-cleanup)
(require 'mevedel-session-persistence)
(require 'mevedel-system)

(defun mevedel-test-memory-decision-owner-loss (workspace pass item memory victim successor)
  "Kill VICTIM after writing ITEM, then run SUCCESSOR on PASS in WORKSPACE.
Verify the exact rebased index in MEMORY, including its trailing blank line."
  (let* ((id (plist-get item :id))
         (index (file-name-concat memory "MEMORY.md"))
         (topic (file-name-concat memory "topic.md"))
         (original "Unrelated prose.\n- [Topic](topic.md) - original\n\n")
         (batch (symbol-function 'mevedel-memory-write--batch))
         (elect (symbol-function 'mevedel-memory-decision--elect))
         killed)
    (write-region original nil index nil 'silent)
    (cl-labels ((kill-owner (process)
                  (should (process-live-p process))
                  (signal-process process 'SIGKILL)
                  (with-timeout (3 (ert-fail "Lock owner did not die"))
                    (while (process-live-p process) (accept-process-output process .01)))
                  (setq killed t)))
      (cl-letf (((symbol-function 'mevedel-memory-write--batch)
                 (lambda (target &rest args)
                   (prog1 (apply batch target args)
                     (when (eq victim 'target) (kill-owner target)))))
                ((symbol-function 'mevedel-memory-decision--elect)
                 (lambda (ws owner &rest args)
                   (when (eq victim 'workspace) (kill-owner owner))
                   (apply elect ws owner args))))
        (should-error (mevedel-memory-decision-apply workspace pass id))))
    (should killed)
    (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file topic)))
    (should-not (mevedel-memory-decision-status workspace id))
    (let ((dir (mevedel-memory-decision--directory workspace)))
      (should (= 1 (length (directory-files dir nil "\\.index-"))))
      (if (eq successor 'reject)
          (progn
            (should (eq 'rejected (plist-get (mevedel-memory-decision-reject workspace pass id) :status)))
            (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file topic))))
        (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass id) :status)))
        (should (string-prefix-p "Unrelated prose.\n" (mevedel-session-control-fs-read-file index)))
        (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass id) :status))))
      (should-not (directory-files dir nil "\\.index-"))
      (should (equal original (mevedel-session-control-fs-read-file index))))))

(defun mevedel-test-memory-cleanup-race (workspace pass item memory first)
  "Race FIRST against its peer on PASS's ITEM in WORKSPACE with root MEMORY."
  (let* ((root (mevedel-workspace-root workspace)) (id (plist-get item :id))
         (mutex (mevedel-journal-store-claim-directory (mevedel-journal-store-directory root) 'mutation))
         (ready (file-name-concat root "owner-ready")) (release (file-name-concat root "release"))
         (runner (file-name-concat root "contender.el")) (deadline (+ (float-time) 15))
         (output (generate-new-buffer " *memory-cleanup-race*")) process)
    (unwind-protect
        (progn
          (with-temp-file runner
            (insert ";;; -*- lexical-binding: t -*-\n")
            (prin1
             `(progn
                (setq load-path ',load-path)
                (require 'mevedel-journal-cleanup) (require 'mevedel-memory-decision)
                (require 'mevedel-system)
                (let ((mevedel-memory-dirs (list ,memory)) (workspace (mevedel-workspace--create :root ,root))
                      (mevedel-journal-cleanup--inhibit-scheduling t) (mevedel-memory-history-max-age-days 0))
                  (advice-add ',(if (eq first 'apply) 'mevedel-memory-decision--elect 'mevedel-journal-cleanup--owned)
                              :before (lambda (&rest _)
                                        (write-region "held" nil ,ready nil 'silent)
                                        (while (and (not (file-exists-p ,release)) (< (float-time) ,deadline)) (sleep-for .01))
                                        (unless (file-exists-p ,release) (error "Contender never reached mutex"))))
                  ,(if (eq first 'apply)
                       `(unless (eq 'applied (plist-get (mevedel-memory-decision-apply workspace ,pass ,id) :status))
                          (error "Apply did not commit"))
                     '(unless (mevedel-journal-cleanup-expired workspace t) (error "Cleanup did not run")))))
             (current-buffer)))
          (setq process (make-process :name "memory-cleanup-race" :buffer output :noquery t :sentinel #'ignore
                                      :command (list (file-name-concat invocation-directory invocation-name) "-Q" "--batch" "-l" runner)))
          (while (and (not (file-exists-p ready)) (process-live-p process) (< (float-time) deadline))
            (accept-process-output process .01))
          (unless (file-exists-p ready) (ert-fail (with-current-buffer output (buffer-string))))
          (let* ((second-edge (if (eq first 'apply) 'mevedel-journal-claim-acquire 'mevedel-session-control-fs--acquire))
                 (original (symbol-function second-edge))
                 (mevedel-journal-cleanup--inhibit-scheduling t) (mevedel-memory-history-max-age-days 0))
            (cl-letf (((symbol-function second-edge)
                       (lambda (dir &rest args)
                         (when (and (equal dir mutex) (not (file-exists-p release)))
                           ;; Prove the first real critical section excludes this independent client.
                           (should (= 1 (call-process "flock" nil nil nil "-xn" dir "true")))
                           (write-region "contending" nil release nil 'silent))
                         (apply original dir args))))
              (if (eq first 'apply) (should (mevedel-journal-cleanup-expired workspace t))
                (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass id) :status))))))
          (while (and (process-live-p process) (< (float-time) deadline)) (accept-process-output process .01))
          (unless (and (not (process-live-p process)) (zerop (process-exit-status process)))
            (ert-fail (with-current-buffer output (buffer-string))))
          (should (file-exists-p release))
          (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer output))))

(mevedel-deftest mevedel-memory-decision-apply
    (:vars* ((root (make-temp-file "mevedel-memory-application-" t))
             (memory (make-temp-file "mevedel-shared-memory-" t))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs (list memory)) claim accepted item pass scope root-id)
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t) (delete-directory memory t)))
  (progn
    (write-region "- [Topic](topic.md) - original\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (write-region "Original topic.\n" nil (file-name-concat memory "topic.md") nil 'silent)
    (setq scope (mevedel-memory-scope-capture workspace) root-id (caar (plist-get scope :roots)))
    ;; Scenarios constructing their own accepted bundle need the original
    ;; files and scope, not an unrelated pending review in their history.
    (when ,initial-review
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
      (let* ((reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                   "type: \"project\"\ntitle: \"Topic\"\nhook: \"Updated context\"\nreason: \"New evidence\"\n"
                                   "evidence: []\n---\nNew topic.\n```\n## Merge\n- none\n## Remove\n- none\n"
                                   "## Instructions\n- none\n## No action\n- none") root-id))
             (prepared (mevedel-memory-store-prepare workspace claim scope nil "")))
        (setq accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil)
              item (car (plist-get accepted :proposals)) pass (plist-get prepared :id))
        (mevedel-memory-store-publish workspace pass)))
    ,test)
  (initial-review test)
  :doc "apply races cleanup while its target write is acknowledged but its slot has not been elected"
  t (mevedel-test-memory-cleanup-race workspace pass item memory 'apply)
  :doc "cleanup selects the pending pass while a second editor starts applying its proposal"
  t (mevedel-test-memory-cleanup-race workspace pass item memory 'cleanup)
  :doc "journal inference admission and its publication evidence do not lock out decisions"
  t
  (let* ((journal (mevedel-journal-store-directory root))
         (digest (mevedel-journal-claim-acquire (mevedel-journal-store-claim-directory journal 'digest-run) 120)))
    (should digest)
    (unwind-protect
        (progn
          (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
          (should (mevedel-journal-claim-owned-p digest)))
      (mevedel-journal-claim-settle digest 'cancelled "")))
  :doc "an active publication claim delays only public projection, not the decision or its files"
  t
  (let* ((journal (mevedel-journal-store-directory root)) (id (plist-get item :id))
         (writer (mevedel-journal-claim-acquire (mevedel-journal-store-claim-directory journal 'mutation) 120)))
    (should writer)
    (unwind-protect
        (let ((result (mevedel-memory-decision-apply workspace pass id)))
          (should (eq 'applied (plist-get (plist-get result :decision) :status)))
          (should (string-match-p "Decision saved" (plist-get result :error)))
          (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace id) :status)))
          (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
          (should (= 1 (length (mevedel-journal-store-entries root)))))
      (mevedel-journal-claim-settle writer 'completed ""))
    (mevedel-memory-decision-recover workspace)
    (should (= 2 (length (mevedel-journal-store-entries root)))))
  :doc "a timer cannot enter a blocking synchronous decision or modify the target"
  t
  (let (result blocked)
    (run-at-time
     0 nil (lambda ()
             (cl-letf (((symbol-function 'mevedel-session-control-fs--helper-wait)
                        (lambda (&rest _) (setq blocked t) (error "Unexpected blocking wait"))))
               (setq result (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id))
                                          :type 'user-error)))))
    (with-timeout (2 (ert-fail "Decision timer did not return"))
      (while (not result) (accept-process-output nil .01)))
    (should (string-match-p "Cannot wait for a memory decision" (error-message-string result)))
    (should-not blocked)
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
  :doc "applies complete topic/index changes with one immutable decision slot"
  t
  (let* (notifications
         (notify (lambda (files) (push files notifications)))
         (decision (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id) :on-applied notify) :decision))
         (repeated (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id) :on-applied notify) :decision)))
    (should (eq 'applied (plist-get decision :status)))
    (should (equal decision repeated))
    (should (= 1 (length notifications)))
    (should (equal (sort (car notifications) #'string<)
                   (list (file-name-concat memory "MEMORY.md") (file-name-concat memory "topic.md"))))
    (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - Updated context\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should-not (string-match-p "New topic" (plist-get decision :text)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (let ((record (mevedel-memory-decision--read workspace (car (mevedel-memory-decision--slots workspace (plist-get item :id))))))
      (should (equal (plist-get item :before) (plist-get record :before)))
      (should (equal (plist-get item :id) (plist-get (plist-get record :metadata) :decision-id)))
      (dolist (row (plist-get record :after))
        (should (equal (plist-get (cdr row) :bytes) (mevedel-session-control-fs-read-file (plist-get (cdr row) :path)))))))
  :doc "an applied slot still validates when recovery reads it on a later day"
  t
  (progn
    (mevedel-memory-decision-apply workspace pass (plist-get item :id))
    (let* ((slot (car (mevedel-memory-decision--slots workspace (plist-get item :id))))
           (now (format-time-string "%Y-%m-%d"))
           (orig (symbol-function 'format-time-string)))
      (cl-letf (((symbol-function 'format-time-string)
                 (lambda (format &optional time zone)
                   (if (and (equal format "%Y-%m-%d") (null time))
                       (funcall orig format (time-add nil (* 2 86400)) zone)
                     (funcall orig format time zone)))))
        (should-not (equal now (format-time-string "%Y-%m-%d")))
        (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision--read workspace slot) :metadata)
                                        :status))))))
  :doc "workspace owner dies before election; successor Accept completes and retains exact reversal"
  t (mevedel-test-memory-decision-owner-loss workspace pass item memory 'workspace 'apply)
  :doc "workspace owner dies before election; successor Reject restores exact rebased bytes"
  t (mevedel-test-memory-decision-owner-loss workspace pass item memory 'workspace 'reject)
  :doc "target owner dies after write acknowledgement; no slot is elected for the dead target"
  t (mevedel-test-memory-decision-owner-loss workspace pass item memory 'target 'reject)
  :doc "checked reversal restores an applied proposal and remains terminal and idempotent"
  t
  (let* ((applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))
         (reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision)))
    (should (eq 'reversed (plist-get reversed :status)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - original\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should (equal reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision)))
    (should (equal reversed (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
    (should (equal reversed (plist-get (mevedel-memory-decision-reject workspace pass (plist-get item :id)) :decision)))
    (should (mevedel-memory-decision--published workspace applied))
    (should (= 3 (length (mevedel-journal-store-entries root)))))
  :doc "reversal preserves intervening disk and unsaved buffer edits"
  t
  (let* ((applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))
         (path (file-name-concat memory "topic.md"))
         (written (mevedel-session-control-fs-read-file path)))
    (write-region "A later correction.\n" nil path nil 'silent)
    (should-error (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision))
    (should (equal "A later correction.\n" (mevedel-session-control-fs-read-file path)))
    (write-region written nil path nil 'silent)
    (let ((buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-max)) (insert "Unsaved correction.\n")
            (should-error (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision))
            (should (buffer-modified-p))
            (should (string-suffix-p "Unsaved correction.\n" (buffer-string))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (should (equal applied (mevedel-memory-decision-status workspace (plist-get item :id))))
    (should (eq 'reversed (plist-get (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision) :status))))
  :doc "a reversal interrupted after restoring targets is completed only by explicit Reverse"
  t
  (let ((id (plist-get item :id)) reached)
    (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass id) :status)))
    (cl-letf (((symbol-function 'mevedel-memory-decision--elect)
               (lambda (&rest _) (setq reached t) (error "Interrupted before reversal slot"))))
      (should-error (mevedel-memory-decision-reverse workspace pass id)))
    (should reached)
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (mevedel-memory-decision-recover workspace)
    (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace id) :status)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass id) :status))))
  :doc "a partial reversal restores only the row still at its applied state"
  t
  (let* ((id (plist-get item :id)) (topic (file-name-concat memory "topic.md"))
         (batch (symbol-function 'mevedel-memory-write--batch)) written)
    (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass id) :status)))
    (write-region "Original topic.\n" nil topic nil 'silent)
    (cl-letf (((symbol-function 'mevedel-memory-write--batch)
               (lambda (target checks after &optional mode)
                 (setq written (mapcar (lambda (row) (plist-get row :path)) after))
                 (funcall batch target checks after mode))))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass id) :status))))
    (should (equal written (list (file-name-concat memory "MEMORY.md")))))
  :doc "activation republishes a private slot without writing over unsaved editor changes"
  t
  (let* ((mevedel-memory-decision--inhibit-recovery nil) (mevedel-journal-enabled nil)
         (id (plist-get item :id)) buffer completed)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mevedel-memory-decision--publish) (lambda (&rest _) (error "Stopped before publication"))))
            (should-error (mevedel-memory-decision-apply workspace pass id)))
          (should (= 1 (length (mevedel-journal-store-entries root))))
          (setq buffer (find-file-noselect (file-name-concat memory "topic.md")))
          (with-current-buffer buffer (goto-char (point-max)) (insert "Unsaved correction.\n"))
          (mevedel-memory-decision-schedule-recovery workspace (lambda () (setq completed t)))
          (mevedel-memory-decision-schedule-recovery workspace)
          (should (= 1 (hash-table-count mevedel-memory-decision--recovery-pending)))
          (with-timeout (10 (ert-fail "Publication recovery did not complete"))
            (while (not completed) (accept-process-output nil .01)))
          (should (= 2 (length (mevedel-journal-store-entries root))))
          (with-current-buffer buffer
            (should (buffer-modified-p)) (should (string-suffix-p "Unsaved correction.\n" (buffer-string)))))
      (when (buffer-live-p buffer) (with-current-buffer buffer (set-buffer-modified-p nil)) (kill-buffer buffer))
      (mevedel-memory-decision-stop-recovery)))
  :doc "unrelated manual index edits survive application and reversal"
  t
  (let* ((index (file-name-concat memory "MEMORY.md"))
         (manual "# My notes\n- [Other](other.md) - Manual addition\n- [Topic](topic.md) - original\n"))
    (write-region manual nil index nil 'silent)
    (set-file-modes index #o600)
    (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision) :status)))
    (should (equal (string-replace "- original" "- Updated context" manual)
                   (mevedel-session-control-fs-read-file index)))
    (should (= #o600 (file-modes index)))
    (should (eq 'reversed (plist-get (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :decision) :status)))
    (should (equal manual (mevedel-session-control-fs-read-file index))))
  :doc "independent reviews compose without replaying index decision history"
  t
  (let* ((next-claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
         (prepared (mevedel-memory-store-prepare workspace next-claim scope nil ""))
         (second (mevedel-memory-store-accept-proposals
                  workspace prepared
                  (list :proposals (list (list :action 'promote :root root-id :file "new.md"
                                              :type "project" :title "New" :hook "Context"
                                              :reason "New evidence" :body "New guide.")))
                  nil "test:model" nil))
         (second-id (plist-get (car (plist-get second :proposals)) :id)))
    (mevedel-memory-store-publish workspace (plist-get prepared :id))
    (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision) :status)))
    (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace (plist-get prepared :id) second-id) :decision) :status)))
    (should (equal "- [Topic](topic.md) - Updated context\n- [New](new.md) - Context\n"
                   (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should (equal accepted (mevedel-memory-store-accepted workspace pass))))
  :doc "a stale index prevents every topic write and records the failed attempt"
  t
  (progn
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (let ((decision (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
      (should (eq 'stale (plist-get decision :status)))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
      (should (equal "External index edit.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))))
  :doc "changed topics still block application after unrelated index edits"
  t
  (let ((index (file-name-concat memory "MEMORY.md"))
        (topic (file-name-concat memory "topic.md")))
    (write-region "# Manual notes\n- [Topic](topic.md) - original\n" nil index nil 'silent)
    (write-region "Later topic correction.\n" nil topic nil 'silent)
    (let ((decision (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
      (should (eq 'stale (plist-get decision :status)))
      (should (equal "Files changed since capture: topic.md" (plist-get decision :reason))))
    (should (equal "Later topic correction.\n" (mevedel-session-control-fs-read-file topic)))
    (should (equal "# Manual notes\n- [Topic](topic.md) - original\n" (mevedel-session-control-fs-read-file index))))
  :doc "invalid current indexes are unavailable and leave every target untouched"
  t
  (let ((index (file-name-concat memory "MEMORY.md")))
    (dolist (text (list "- [Topic](topic.md) - original\n- [Alias](%74opic.md) - Duplicate\n"
                       "Unsupported [link](topic.md)\n" (make-string 32769 ?x)))
      (write-region text nil index nil 'silent)
      (should (eq 'unavailable (plist-get (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision) :status)))
      (should (equal text (mevedel-session-control-fs-read-file index)))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))
  :doc "a later application decision supersedes a stale attempt by claim generation"
  t
  (progn
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (should (eq 'stale (plist-get (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision) :status)))
    (write-region "- [Topic](topic.md) - original\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (let ((applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision)))
      (should (eq 'applied (plist-get applied :status)))
      (should (equal applied (mevedel-memory-decision-status workspace (plist-get item :id))))))
  :doc "promotion honors the capturing editor's mode without widening this editor's permissions"
  nil
  (dolist (modes '((#o600 #o644 "private.md") (#o644 #o600 "public.md")))
    (let* ((filename (nth 2 modes)) (path (file-name-concat memory filename))
           (scope (cl-letf (((symbol-function 'default-file-modes) (lambda () (car modes))))
                    (mevedel-memory-scope-capture workspace)))
           (reply (format (concat "## Promote\n```proposal\nroot: %S\nfile: %S\ntype: \"project\"\ntitle: \"Guide\"\n"
                                  "hook: \"Context\"\nreason: \"New evidence\"\nevidence: []\n---\nGuide.\n```\n"
                                  "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none")
                          root-id filename)))
      (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
      (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
             (bundle (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil))
             (id (plist-get (car (plist-get bundle :proposals)) :id))
             (pass (plist-get prepared :id)))
        (mevedel-memory-store-publish workspace pass)
        (cl-letf (((symbol-function 'default-file-modes) (lambda () (cadr modes))))
          (let ((result (mevedel-memory-decision-apply workspace pass id)))
            (if (= (car modes) #o600)
                (progn
                  (should (eq (plist-get result :status) 'applied))
                  (should (= #o600 (file-modes path)))
                  (should (equal (plist-get result :decision) (mevedel-memory-decision-status workspace id))))
              (should (eq (plist-get result :status) 'unavailable))
              (should (string-match-p "0644.*0600" (plist-get result :reason)))
              (should-not (file-exists-p path))))))))
  :doc "same-pass index changes compose while edits to the affected entry remain stale"
  nil
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let* ((reply (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\ntitle: \"New\"\n"
                                 "hook: \"Context\"\nreason: \"New evidence\"\nevidence: []\n---\nNew guide.\n```\n"
                                 "## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\ntype: \"project\"\ntitle: \"Topic\"\n"
                                 "hook: \"Updated context\"\nreason: \"New evidence\"\nevidence: []\n---\nNew topic.\n```\n"
                                 "## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id root-id))
           (prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
           (batch (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil))
           (id (plist-get prepared :id)) (items (plist-get batch :proposals)))
      (mevedel-memory-store-publish workspace id)
      (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace id (plist-get (cadr items) :id)) :decision) :status)))
      (let ((index (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
        (write-region (concat index "- [New](new.md) - Foreign entry\n") nil (file-name-concat memory "MEMORY.md") nil 'silent)
        (should (eq 'stale (plist-get (plist-get (mevedel-memory-decision-apply workspace id (plist-get (car items) :id)) :decision) :status)))
        (should-not (file-exists-p (file-name-concat memory "new.md")))
        (write-region index nil (file-name-concat memory "MEMORY.md") nil 'silent))
      (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace id (plist-get (car items) :id)) :decision) :status)))
      (should (equal "- [Topic](topic.md) - Updated context\n- [New](new.md) - Context\n"
                     (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
      (should-error (plist-get (mevedel-memory-decision-reverse workspace id (plist-get (cadr items) :id)) :decision))
      (should (eq 'reversed (plist-get (plist-get (mevedel-memory-decision-reverse workspace id (plist-get (car items) :id)) :decision) :status)))
      (should-not (file-exists-p (file-name-concat memory "new.md")))
      (should (eq 'reversed (plist-get (plist-get (mevedel-memory-decision-reverse workspace id (plist-get (cadr items) :id)) :decision) :status)))
      (let ((input (mevedel-memory-decision--application-input batch (car items))))
        (should (equal (plist-get (car items) :before) (plist-get (plist-get input :proposal) :before))))))
  :doc "an index edit after preparation cannot be overwritten by the locked batch"
  t
  (let* ((index (file-name-concat memory "MEMORY.md")) (id (plist-get item :id))
         (batch (symbol-function 'mevedel-memory-write--batch))
         (foreign "# Concurrent change\n- [Topic](topic.md) - original\n"))
    (cl-letf (((symbol-function 'mevedel-memory-write--batch)
               (lambda (&rest args) (write-region foreign nil index nil 'silent) (apply batch args))))
      (should-error (mevedel-memory-decision-apply workspace pass id)))
    (should-not (mevedel-memory-decision-status workspace id))
    (should (equal foreign (mevedel-session-control-fs-read-file index)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
  :doc "unsaved index edits prevent application even when disk entries match"
  t
  (let ((buffer (find-file-noselect (file-name-concat memory "MEMORY.md"))))
    (unwind-protect
        (progn
          (with-current-buffer buffer (goto-char (point-max)) (insert "Unsaved notes.\n"))
          (should-error (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))
          (with-current-buffer buffer
            (should (buffer-modified-p))
            (should (string-suffix-p "Unsaved notes.\n" (buffer-string))))
          (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
      (with-current-buffer buffer (set-buffer-modified-p nil))
      (kill-buffer buffer)))
  :doc "index creation and deletion preserve unrelated current state through apply and undo"
  nil
  (let ((index (file-name-concat memory "MEMORY.md")))
    (dolist (originally-present '(nil t))
      (if originally-present
          (write-region "# Original notes\n" nil index nil 'silent)
        (delete-file index))
      (let* ((scope (mevedel-memory-scope-capture workspace))
             (next-claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
             (prepared (mevedel-memory-store-prepare workspace next-claim scope nil ""))
             (batch (mevedel-memory-store-accept-proposals
                     workspace prepared
                     (list :proposals (list (list :action 'promote :root root-id :file "new.md"
                                                 :type "project" :title "New" :hook "Context"
                                                 :reason "New evidence" :body "New guide.")))
                     nil "test:model" nil))
             (id (plist-get (car (plist-get batch :proposals)) :id))
             (pass (plist-get prepared :id)))
        (mevedel-memory-store-publish workspace pass)
        (if originally-present (delete-file index)
          (write-region "# Manual notes\n" nil index nil 'silent))
        (should (eq 'applied (plist-get (plist-get (mevedel-memory-decision-apply workspace pass id) :decision) :status)))
        (should (equal (concat (unless originally-present "# Manual notes\n") "- [New](new.md) - Context\n")
                       (mevedel-session-control-fs-read-file index)))
        (should (eq 'reversed (plist-get (plist-get (mevedel-memory-decision-reverse workspace pass id) :decision) :status)))
        (if originally-present (should-not (file-exists-p index))
          (should (equal "# Manual notes\n" (mevedel-session-control-fs-read-file index)))))))
  :doc "merge source topic edits prevent every write despite an unrelated index change"
  nil
  (let ((source (file-name-concat memory "source.md"))
        (index (file-name-concat memory "MEMORY.md")))
    (write-region "Original source.\n" nil source nil 'silent)
    (let* ((scope (mevedel-memory-scope-capture workspace))
           (next-claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
           (prepared (mevedel-memory-store-prepare workspace next-claim scope nil ""))
           (batch (mevedel-memory-store-accept-proposals
                   workspace prepared
                   (list :proposals (list (list :action 'merge :root root-id :file "topic.md"
                                               :merged-files '("source.md") :type "project" :title "Topic"
                                               :hook "Merged context" :reason "Merge" :body "Merged guide.")))
                   nil "test:model" nil)))
      (mevedel-memory-store-publish workspace (plist-get prepared :id))
      (write-region "Edited source.\n" nil source nil 'silent)
      (write-region "# Manual notes\n- [Topic](topic.md) - original\n" nil index nil 'silent)
      (let ((decision (plist-get (mevedel-memory-decision-apply
                       workspace (plist-get prepared :id) (plist-get (car (plist-get batch :proposals)) :id)) :decision)))
        (should (eq 'stale (plist-get decision :status)))
        (should (equal "Files changed since capture: source.md" (plist-get decision :reason))))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
      (should (equal "Edited source.\n" (mevedel-session-control-fs-read-file source)))))
  :doc "many conflicting merge entries retain a bounded decision reason"
  nil
  (let ((index (file-name-concat memory "MEMORY.md")) sources lines)
    (dotimes (number 50)
      (let ((file (format "%02d-%s.md" number (make-string 45 ?\u00e9))))
        (push file sources)
        (push (format "- [Source](%s) - Original\n" file) lines)
        (write-region "Source.\n" nil (file-name-concat memory file) nil 'silent)))
    (write-region (apply #'concat lines) nil index nil 'silent)
    (let* ((scope (mevedel-memory-scope-capture workspace))
           (next-claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
           (prepared (mevedel-memory-store-prepare workspace next-claim scope nil ""))
           (batch (mevedel-memory-store-accept-proposals
                   workspace prepared
                   (list :proposals (list (list :action 'merge :root root-id :file "topic.md"
                                               :merged-files sources :type "project" :title "Topic"
                                               :hook "Merged context" :reason "Merge" :body "Merged guide.")))
                   nil "test:model" nil)))
      (mevedel-memory-store-publish workspace (plist-get prepared :id))
      (write-region "" nil index nil 'silent)
      (let ((decision (plist-get (mevedel-memory-decision-apply
                       workspace (plist-get prepared :id) (plist-get (car (plist-get batch :proposals)) :id)) :decision)))
        (should (eq 'stale (plist-get decision :status)))
        (should (<= (string-bytes (plist-get decision :reason)) 4096))
        (should (string-suffix-p "..." (plist-get decision :reason))))))
  :doc "a root claim held outside this workspace prevents application"
  t
  (mevedel-session-control-fs-call-with-locks
   (list (mevedel-memory-write--target scope root-id))
   (lambda (_target)
     (should-error (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))
     (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))
  :doc "an independent Emacs in another workspace serializes edits at a shared memory root"
  t
  (let* ((runner (file-name-concat root "runner.el"))
         (ready (file-name-concat root "ready"))
         (release (file-name-concat root "release"))
         (other (file-name-concat root "other-workspace"))
         (output (generate-new-buffer " *memory-root-owner*"))
         (deadline (+ (float-time) 30)) process)
    (unwind-protect
        (progn
          (make-directory other)
          (with-temp-file runner
            (insert ";;; -*- lexical-binding: t -*-\n")
            (prin1
             `(progn
                (setq load-path ',load-path)
                (require 'mevedel-memory-decision)
                (require 'mevedel-system)
                (let* ((mevedel-memory-dirs (list ,memory))
                       (workspace (mevedel-workspace--create :root ,other))
                       (_ (mevedel-workspace-identity-ensure ,other))
                       (scope (mevedel-memory-scope-capture workspace))
                       (deadline (+ (float-time) 25)))
                  (mevedel-session-control-fs-call-with-locks
                   (list (mevedel-memory-write--target scope (caar (plist-get scope :roots))))
                   (lambda (_target)
                     (write-region "ready" nil ,ready nil 'silent)
                     (while (and (not (file-exists-p ,release)) (< (float-time) deadline))
                       (accept-process-output nil 0.01))
                     (unless (file-exists-p ,release) (error "Release barrier timed out"))
                     (write-region "Other workspace edit.\n" nil ,(file-name-concat memory "topic.md") nil 'silent)))))
             (current-buffer)))
          (setq process (make-process :name "memory-root-owner" :buffer output :noquery t :sentinel #'ignore
                                      :command (list (file-name-concat invocation-directory invocation-name)
                                                     "--batch" "-Q" "-l" runner)))
          (while (and (not (file-exists-p ready)) (process-live-p process) (< (float-time) deadline))
            (accept-process-output nil 0.01))
          (should (file-exists-p ready))
          (should (process-live-p process))
          (should-error (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision))
          (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
          (write-region "release" nil release nil 'silent)
          (while (and (process-live-p process) (< (float-time) deadline)) (accept-process-output nil 0.01))
          (should-not (process-live-p process))
          (should (= 0 (process-exit-status process)))
          (should (eq 'stale (plist-get (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :decision) :status)))
          (should (equal "Other workspace edit.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer output)))
  :doc "an index operation failure after the topic rename restores its exact original bytes"
  t
  (let* ((id (plist-get item :id)) (index (file-name-concat memory "MEMORY.md"))
         (topic (file-name-concat memory "topic.md"))
         (run (symbol-function 'mevedel-session-control-fs-lock-session-run)) injected)
    (cl-letf (((symbol-function 'mevedel-session-control-fs-lock-session-run)
               (lambda (owner ops)
                 (when (eq (plist-get (car ops) :op) 'begin-files)
                   (let ((first (seq-position ops topic (lambda (op path) (and (eq 'write-mode (plist-get op :op))
                                                                              (equal path (plist-get op :path)))))))
                     (should first)
                     (should (equal index (plist-get (nth (1+ first) ops) :path)))
                     (setq ops (append (seq-take ops (1+ first)) (list (list :op 'absent :path topic)) (nthcdr (1+ first) ops))
                           injected t)))
                 (funcall run owner ops))))
      (should (string-search "all changes rolled back"
                             (error-message-string (should-error (mevedel-memory-decision-apply workspace pass id))))))
    (should injected)
    (should-not (mevedel-memory-decision-status workspace id))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file topic)))
    (should (equal "- [Topic](topic.md) - original\n" (mevedel-session-control-fs-read-file index))))
  :doc "SIGKILL between the topic and index renames leaves no slot; Reject restores the topic"
  t
  (let* ((id (plist-get item :id)) (topic (file-name-concat memory "topic.md"))
         (index (file-name-concat memory "MEMORY.md"))
         (original mevedel-session-control-fs--program-script))
    ;; Kill the dedicated target owner and its batch after the real rename, before rollback.
    (let ((mevedel-session-control-fs--program-script
           (string-replace "    case \"$op\" in\n      write-mode|delete-file|mkdir)\n"
                           (concat "    if [[ $op == write-mode && $leaf == topic.md ]]; then kill -KILL \"$$\" \"$BASHPID\"; fi\n"
                                   "    case \"$op\" in\n      write-mode|delete-file|mkdir)\n") original)))
      (should-not (equal original mevedel-session-control-fs--program-script))
      (should-error (mevedel-memory-decision-apply workspace pass id)))
    (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file topic)))
    (should (equal "- [Topic](topic.md) - original\n" (mevedel-session-control-fs-read-file index)))
    (should-not (mevedel-memory-decision-status workspace id))
    (should (eq 'rejected (plist-get (mevedel-memory-decision-reject workspace pass id) :status)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file topic))))
  :doc "a third-state edit during a partial application is preserved by both Accept and Reject"
  t
  (let* ((id (plist-get item :id)) (topic (file-name-concat memory "topic.md")) (index (file-name-concat memory "MEMORY.md")))
    (let ((changes (mevedel-memory-apply--changes (mevedel-memory-scope--root scope root-id) item (plist-get scope :new-file-mode))))
      (mevedel-tool-patch-commit (list (seq-find (lambda (c) (equal topic (plist-get c :path))) changes))))
    (write-region "User edit.\n" nil index nil 'silent)
    (dolist (act '(mevedel-memory-decision-apply mevedel-memory-decision-reject))
      (should (eq 'stale (plist-get (funcall act workspace pass id) :status)))
      (should (equal "User edit.\n" (mevedel-session-control-fs-read-file index)))
      (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file topic))))
    (write-region "- [Topic](topic.md) - original\n" nil index nil 'silent)
    (should (eq 'rejected (plist-get (mevedel-memory-decision-reject workspace pass id) :status)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file topic)))))

(provide 'test-mevedel-memory-decision-apply)
;;; test-mevedel-memory-decision-apply.el ends here
