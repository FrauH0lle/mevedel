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
(require 'mevedel-journal-capture)
(require 'mevedel-session-persistence)
(require 'mevedel-system)

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
    (setq scope (mevedel-memory-scope-capture workspace) root-id (caar (plist-get scope :roots))
          claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let* ((reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\nfile: \"topic.md\"\n"
                                 "type: \"project\"\ntitle: \"Topic\"\nhook: \"Updated context\"\nreason: \"New evidence\"\n"
                                 "evidence: []\n---\nNew topic.\n```\n## Merge\n- none\n## Remove\n- none\n"
                                 "## Instructions\n- none\n## No action\n- none") root-id))
           (prepared (mevedel-memory-store-prepare workspace claim scope nil "")))
      (setq accepted (mevedel-memory-store-accept workspace prepared reply nil "test:model" nil)
            item (car (plist-get accepted :proposals)) pass (plist-get prepared :id))
      (mevedel-memory-store-publish workspace pass))
    ,test)
  (test)
  :doc "applies complete topic/index changes with private intent and an immutable decision"
  (let* (notifications
         (notify (lambda (files) (push files notifications)))
         (decision (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify))
         (repeated (mevedel-memory-decision-apply workspace pass (plist-get item :id) notify)))
    (should (eq 'applied (plist-get decision :status)))
    (should (equal decision repeated))
    (should (= 1 (length notifications)))
    (should (equal (sort (car notifications) #'string<)
                   (list (file-name-concat memory "MEMORY.md") (file-name-concat memory "topic.md"))))
    (should (string-suffix-p "New topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - Updated context\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should-not (string-match-p "New topic" (plist-get decision :text)))
    (should (= 2 (length (mevedel-journal-store-entries root))))
    (let ((intent (plist-get (mevedel-memory-decision--read
                             workspace (file-name-concat (mevedel-memory-decision--directory workspace)
                                                         (concat (plist-get decision :decision-id) ".el"))) :intent)))
      (should intent)
      (should (eq 'after (mevedel-memory-write-state intent)))
      (should-not (file-exists-p (mevedel-memory-write--pin intent)))))
  :doc "checked reversal restores an applied proposal and remains terminal and idempotent"
  (let* ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
         (reversed (mevedel-memory-decision-reverse workspace pass (plist-get item :id))))
    (should (eq 'reversed (plist-get reversed :status)))
    (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal "- [Topic](topic.md) - original\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
    (should (equal reversed (mevedel-memory-decision-reverse workspace pass (plist-get item :id))))
    (should (equal reversed (mevedel-memory-decision-apply workspace pass (plist-get item :id))))
    (should (equal reversed (mevedel-memory-decision-reject workspace pass (plist-get item :id))))
    (should (mevedel-memory-decision--published workspace applied))
    (should (= 3 (length (mevedel-journal-store-entries root)))))
  :doc "reversal preserves intervening disk and unsaved buffer edits"
  (let* ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
         (path (file-name-concat memory "topic.md"))
         (written (mevedel-session-control-fs-read-file path)))
    (write-region "A later correction.\n" nil path nil 'silent)
    (should-error (mevedel-memory-decision-reverse workspace pass (plist-get item :id)))
    (should (equal "A later correction.\n" (mevedel-session-control-fs-read-file path)))
    (write-region written nil path nil 'silent)
    (let ((buffer (find-file-noselect path)))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-max)) (insert "Unsaved correction.\n")
            (should-error (mevedel-memory-decision-reverse workspace pass (plist-get item :id)))
            (should (buffer-modified-p))
            (should (string-suffix-p "Unsaved correction.\n" (buffer-string))))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))
    (should (equal applied (mevedel-memory-decision-status workspace (plist-get item :id))))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
  :doc "completed reversal is reconciled after a crash before its decision"
  (let* ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
         (original (plist-get (mevedel-memory-decision--published workspace applied) :intent)) reverse)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq reverse (mevedel-memory-write-prepare-reverse workspace claim target original))
       (mevedel-memory-write-run workspace claim target reverse)))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should (equal applied (mevedel-memory-decision-status workspace (plist-get item :id))))
    (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get reverse :id) (plist-get reverse :hash))))
      (should (eq 'reversed (plist-get decision :status)))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
      (should-not (file-exists-p (mevedel-memory-write--pin reverse)))
      (should (equal decision (mevedel-memory-decision-reverse workspace pass (plist-get item :id))))))
  :doc "partial reversal stays fenced and checked rollback restores the applied state"
  (let* ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
         (original (plist-get (mevedel-memory-decision--published workspace applied) :intent)) reverse)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq reverse (mevedel-memory-write-prepare-reverse workspace claim target original))
       (mevedel-tool-patch-commit (list (car (plist-get reverse :changes))))))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should (eq 'recovery-required
                (plist-get (mevedel-memory-decision-recover-write workspace (plist-get reverse :id) (plist-get reverse :hash)) :status)))
    (should-error (mevedel-memory-decision-reverse workspace pass (plist-get item :id)))
    (should (file-exists-p (mevedel-memory-write--pin reverse)))
    (should (eq 'applied
                (plist-get (mevedel-memory-decision-recover-write workspace (plist-get reverse :id) (plist-get reverse :hash) t) :status)))
    (should (eq 'after (mevedel-memory-write-state original)))
    (should-not (file-exists-p (mevedel-memory-write--pin reverse)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
  :doc "an untouched reversal resolves to applied and can be retried explicitly"
  (let* ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
         (original (plist-get (mevedel-memory-decision--published workspace applied) :intent)) reverse)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id (lambda (target) (setq reverse (mevedel-memory-write-prepare-reverse workspace claim target original))))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should (eq 'applied
                (plist-get (mevedel-memory-decision-recover-write workspace (plist-get reverse :id) (plist-get reverse :hash)) :status)))
    (should (eq 'after (mevedel-memory-write-state original)))
    (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace pass (plist-get item :id)) :status))))
  :doc "recovery discovers a committed attempt without an explicit intent identity"
  (let (intent)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
       (mevedel-memory-write-run workspace claim target intent)))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should (eq 'applied (plist-get (car (mevedel-memory-decision-recover-pending workspace)) :status)))
    (should-not (file-exists-p (mevedel-memory-write--pin intent)))
    (should-not (mevedel-memory-decision-recover-pending workspace)))
  :doc "activation recovery coalesces and settles a committed attempt with journaling disabled"
  (let ((mevedel-memory-decision--inhibit-recovery nil)
        (mevedel-journal-enabled nil) intent)
    (unwind-protect
        (progn
          (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
          (mevedel-memory-write-call
           scope root-id
           (lambda (target)
             (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
             (mevedel-memory-write-run workspace claim target intent)))
          (mevedel-journal-claim-settle claim 'cancelled "")
          (should-not (mevedel-session-persistence-choose-entry workspace))
          (mevedel-memory-decision-schedule-recovery workspace)
          (should (= 1 (hash-table-count mevedel-memory-decision--recovery-pending)))
          (should-not (mevedel-memory-decision-status workspace (plist-get item :id)))
          (let ((deadline (+ (float-time) 5)))
            (while (and (> (hash-table-count mevedel-memory-decision--recovery-pending) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (= 0 (hash-table-count mevedel-memory-decision--recovery-pending)))
          (should (eq 'applied (plist-get (mevedel-memory-decision-status workspace (plist-get item :id)) :status)))
          (should-not (file-exists-p (mevedel-memory-write--pin intent))))
      (mevedel-memory-decision-stop-recovery)))
  :doc "an unmarked orphan cannot claim a coincidentally matching external edit"
  (let (intent)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
       (delete-file (mevedel-memory-write--pin intent))))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (mevedel-tool-patch-commit (plist-get intent :changes))
    (should-not (mevedel-memory-decision-recover-pending workspace))
    (should-not (mevedel-memory-decision-status workspace (plist-get item :id)))
    (should (eq 'after (mevedel-memory-write-state intent))))
  :doc "a stale index prevents every topic write and records the failed attempt"
  (progn
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (let ((decision (mevedel-memory-decision-apply workspace pass (plist-get item :id))))
      (should (eq 'stale (plist-get decision :status)))
      (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
      (should (equal "External index edit.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))))
  :doc "a later application decision supersedes a stale attempt by claim generation"
  (progn
    (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (should (eq 'stale (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
    (write-region "- [Topic](topic.md) - original\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
    (let ((applied (mevedel-memory-decision-apply workspace pass (plist-get item :id))))
      (should (eq 'applied (plist-get applied :status)))
      (should (equal applied (mevedel-memory-decision-status workspace (plist-get item :id))))))
  :doc "same-pass index changes compose while external index edits still make later approval stale"
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
      (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace id (plist-get (cadr items) :id)) :status)))
      (let ((index (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
        (write-region "External index edit.\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
        (should (eq 'stale (plist-get (mevedel-memory-decision-apply workspace id (plist-get (car items) :id)) :status)))
        (should-not (file-exists-p (file-name-concat memory "new.md")))
        (write-region index nil (file-name-concat memory "MEMORY.md") nil 'silent))
      (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace id (plist-get (car items) :id)) :status)))
      (should (equal "- [Topic](topic.md) - Updated context\n- [New](new.md) - Context\n"
                     (mevedel-session-control-fs-read-file (file-name-concat memory "MEMORY.md"))))
      (should-error (mevedel-memory-decision-reverse workspace id (plist-get (cadr items) :id)))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace id (plist-get (car items) :id)) :status)))
      (should-not (file-exists-p (file-name-concat memory "new.md")))
      (should (eq 'reversed (plist-get (mevedel-memory-decision-reverse workspace id (plist-get (cadr items) :id)) :status)))
      (let ((input (mevedel-memory-decision--application-input workspace batch (car items))))
        (should (equal (plist-get (car items) :before) (plist-get (cdr input) :before))))))
  :doc "a root claim held outside this workspace prevents application"
  (mevedel-memory-write-call
   scope root-id
   (lambda (_target)
     (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
     (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))))
  :doc "an independent Emacs in another workspace serializes edits at a shared memory root"
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
                  (mevedel-memory-write-call
                   scope (caar (plist-get scope :roots))
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
          (should-error (mevedel-memory-decision-apply workspace pass (plist-get item :id)))
          (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
          (write-region "release" nil release nil 'silent)
          (while (and (process-live-p process) (< (float-time) deadline)) (accept-process-output nil 0.01))
          (should-not (process-live-p process))
          (should (= 0 (process-exit-status process)))
          (should (eq 'stale (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status)))
          (should (equal "Other workspace edit.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer output)))
  :doc "completed target writes are reconciled after a crash before the decision"
  (let (intent)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
       (should (eq 'before (mevedel-memory-write-state intent)))
       (should (file-exists-p (mevedel-memory-write--pin intent)))
       (mevedel-memory-write-run workspace claim target intent)))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should-not (mevedel-memory-decision-status workspace (plist-get item :id)))
    (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash))))
      (should (eq 'applied (plist-get decision :status)))
      (should-not (file-exists-p (mevedel-memory-write--pin intent)))
      (should (equal decision (mevedel-memory-decision-apply workspace pass (plist-get item :id))))))
  :doc "a partial write stays visible and blocks every competing workspace at the root"
  (let (intent)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
       (mevedel-tool-patch-commit (list (car (plist-get intent :changes))))))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash))))
      (should (eq 'recovery-required (plist-get decision :status)))
      (should (eq 'mixed (mevedel-memory-write-state intent)))
      (should-error (mevedel-memory-write-call scope root-id #'ignore))
      (should-error (mevedel-memory-decision-reject workspace pass (plist-get item :id)))
      (should (file-exists-p (mevedel-memory-write--pin intent)))))
  :doc "explicit rollback refuses foreign edits and restores only a known partial attempt"
  (let (intent)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-write-call
     scope root-id
     (lambda (target)
       (setq intent (mevedel-memory-write-prepare workspace claim target accepted item))
       (mevedel-tool-patch-commit (list (car (plist-get intent :changes))))))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (let* ((change (car (plist-get intent :changes)))
           (path (plist-get change :path))
           (written (mevedel-session-control-fs-read-file path)))
      (write-region "User changed this file.\n" nil path nil 'silent)
      (should (eq 'recovery-required
                  (plist-get (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash) t) :status)))
      (should (equal "User changed this file.\n" (mevedel-session-control-fs-read-file path)))
      (should (file-exists-p (mevedel-memory-write--pin intent)))
      (write-region written nil path nil 'silent)
      (let ((decision (mevedel-memory-decision-recover-write workspace (plist-get intent :id) (plist-get intent :hash) t)))
        (should (eq 'unavailable (plist-get decision :status)))
        (should (eq 'before (mevedel-memory-write-state intent)))
        (should-not (file-exists-p (mevedel-memory-write--pin intent)))
        (should (equal "Original topic.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md")))))
      (should (eq 'applied (plist-get (mevedel-memory-decision-apply workspace pass (plist-get item :id)) :status))))))

(provide 'test-mevedel-memory-decision-apply)
;;; test-mevedel-memory-decision-apply.el ends here
