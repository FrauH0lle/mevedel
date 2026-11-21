;;; test-niwa-memory-pass-scheduling.el -- Automatic memory review -*- lexical-binding: t -*-

;;; Commentary:

;; Real journal publications exercise automatic admission without network calls.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'niwa-session-control-fs)
(require 'niwa-memory-pass)
(require 'niwa-journal-capture)
(require 'niwa)
(require 'niwa-system)
(require 'gptel-openai)

(niwa-deftest niwa-memory-pass-schedule
                 (:vars* ((root (make-temp-file "niwa-memory-schedule-" t))
                          (workspace (niwa-workspace--create :root root))
                          (identity (niwa-workspace-identity-ensure root))
                          (niwa-memory-dirs nil)
                          (niwa-journal-max-age-days 14)
                          (niwa-memory-consolidation-mode 'propose)
                          (niwa-memory-consolidation-min-hours 24)
                          ;; Keep the real five-digest gate in the first case;
                          ;; unrelated scheduling cases need only two digests.
                          (niwa-memory-consolidation-min-digests 2)
                          (niwa-memory-pass--inhibit-scheduling nil)
                          (niwa-memory-decision--inhibit-recovery nil)
                          (gptel--known-backends nil)
                          (backend (gptel-make-openai "schedule-test" :key "test-only" :models '(test-model)))
                          (now 1800000000)
                          (calls 0) callback selected state result
                          (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."))
                         :after-each ((niwa-test--with-captured-messages nil
                                        (niwa-memory-pass-stop-all))
                                      (niwa-memory-decision-stop-recovery)
                                      (delete-directory root t)))
                 (cl-letf (((symbol-value 'niwa-journal-worker--child-p) t)
                           ((symbol-function 'niwa-session-control-fs-target-time) (lambda (_) now))
            ((symbol-function 'niwa-session-control-fs-observation-time) (lambda (_) now))
                           ((symbol-function 'niwa-memory-review-request)
                            (lambda (_scope entries cb &rest _)
                              (cl-incf calls)
                              (setq callback cb selected entries)
                              (list :cancel #'ignore))))
                          (cl-labels ((digest (number &optional age-days)
                                        (niwa-journal-store-publish-digest
                                         root (list :capture-id (format "%064x" number) :session "closed" :session-name "Closed"
                                                    :workspace identity :trigger 'session-end :segment 1
                                                    :source-revision (make-string 64 ?c) :turns '(1)
                                                    :turn-ids (list (format "%064x" number))
                                                    :created (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                                                 (- now (* (or age-days 0) 86400)) t) :model "test:model")
                                         "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
                                      (start (&rest args)
                                        (setq state (apply #'niwa-memory-pass-start workspace (lambda (value) (setq result value)) args)))
                                      (finish ()
                                        (funcall callback (list :outcome 'success :entries selected :reply none
                                                                :policy (list :backend backend :model 'test-model))))
                                      (proposals ()
                                        (let ((memory (file-name-concat root "memory")))
                                          (setq niwa-memory-dirs (list memory))
                                          (make-directory memory)
                                          (write-region "Existing guidance.\n" nil (file-name-concat root "AGENTS.md") nil 'silent)
                                          (let* ((scope (niwa-memory-scope-capture workspace))
                                                 (memory-id (caar (plist-get scope :roots)))
                                                 (instruction-id (car (seq-find (lambda (row) (eq 'instructions (plist-get (cdr row) :kind)))
                                                                                (plist-get scope :roots)))))
                                            (setq none
                                                  (concat
                                                   "## Promote\n"
                                                   (mapconcat (lambda (name)
                                                                (format (concat "```proposal\nroot: %S\nfile: %S\ntype: \"project\"\ntitle: %S\n"
                                                                                "hook: \"Current\"\nreason: \"Evidence\"\nevidence: []\n---\nDurable lesson.\n```\n")
                                                                        memory-id (concat name ".md") name)) '("one" "two") "")
                                                   "## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n"
                                                   (format (concat "```proposal\nroot: %S\nfile: \"AGENTS.md\"\ntype: \"project\"\ntitle: \"Guidance\"\n"
                                                                   "hook: \"Guidance\"\nreason: \"Evidence\"\nevidence: []\n---\nExisting guidance.\nUse the checked workflow.\n```\n") instruction-id)
                                                   "## No action\n- none")))
                                          memory))
                                      (drain ()
                                        (let ((deadline (+ (float-time) 10)))
                                          (while (and (or (> (hash-table-count niwa-memory-pass--pending) 0)
                                                           (> (hash-table-count niwa-memory-decision--recovery-pending) 0)
                                                           (> (hash-table-count niwa-memory-decision--recovery-workers) 0))
                                                      (< (float-time) deadline))
                                            (accept-process-output nil 0.01))
                                          (should (= 0 (hash-table-count niwa-memory-pass--pending))))))
                                     ,test))
                 (test)
                 :doc "automatic admission requires five eligible digests while explicit review bypasses the gate"
                 (let ((niwa-memory-consolidation-min-digests 5))
                   (dotimes (n 4) (digest (1+ n)))
                   (should-not (start :automatic t))
                   (should (= 0 calls))
                   (should-not (niwa-memory-pass-running workspace))
                   (should (> (plist-get (niwa-workspace-memory-schedule workspace) :after) (float-time)))
                   (digest 5)
                   (should (start :automatic t))
                   (should (= 5 (length selected)))
                   (finish)
                   (should-not (start :automatic t))
                   (let ((niwa-memory-consolidation-mode 'manual))
                     (should (start :memory-only t))
                     (finish))
                   (should (= 2 calls)))
                 :doc "sparse work is offered a general review one day before ordinary expiry"
                 (progn
                   (digest 1 12)
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (should (start :automatic t))
                   (should (= 1 (length selected)))
                   (finish)
                   (cl-incf now (* 15 86400))
                   (should-not (start :automatic t))
                   (should (= 1 calls)))
                 :doc "custom and disabled expiry preserve their review-age policy and the elapsed-time gate"
                 (let ((niwa-journal-max-age-days 30))
                   (digest 1 28)
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (let ((niwa-journal-max-age-days nil))
                     (should-not (start :automatic t)))
                   (should (start :automatic t))
                   (finish)
                   (digest 2 40)
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (should (start :automatic t))
                   (finish))
                 :doc "focused completion leaves the general clock and coverage available"
                 (progn
                   (cl-decf now 86401)
                   (start :memory-only t)
                   (finish)
                   (cl-incf now 86401)
                   (dotimes (n 2) (digest (1+ n)))
                   (start :focus "Source conditions")
                   (finish)
                   (should (start :automatic t))
                   (should (= 2 (length selected)))
                   (finish)
                   (should (= 3 calls)))
                 :doc "retired journal-bearing completion postpones review without retaining private bodies"
                 (progn
                   (dotimes (n 2) (digest (1+ n)))
                   (start)
                   (should (= 2 (length selected)))
                   (finish)
                   (let ((niwa-memory-history-max-age-days 0))
                     (cl-incf now 1)
                     (niwa-journal-cleanup-expired workspace)
                     ;; A general review retains its consumed digests until
                     ;; their expiry group commits; the next observation can
                     ;; then retire the now-unreferenced review itself.
                     (niwa-journal-cleanup-expired workspace t)
                     (should (niwa-journal-cleanup-pass-retired-p
                              root (plist-get (plist-get state :claim) :owner))))
                   (dotimes (n 2) (digest (+ 3 n)))
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (should (start :automatic t))
                   (should (= 2 (length selected)))
                   (finish))
                 :doc "recovered note-only publication leaves journal admission available"
                 (let* ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
                        (prepared (niwa-memory-store-prepare workspace claim (niwa-memory-scope-capture workspace) nil "")))
                   (niwa-memory-store-accept workspace prepared none nil "test:model" nil)
                   (dotimes (n 2) (digest (1+ n)))
                   (should (start :automatic t))
                   (should (= 2 (length selected)))
                   (should (= 1 calls))
                   (finish)
                   (should (= 4 (length (niwa-journal-store-entries root)))))
                 :doc "another client's live claim postpones automatic admission without settling it"
                 (let ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
                   (unwind-protect
                       (progn
                         (dotimes (n 2) (digest (1+ n)))
                         (should-not (start :automatic t))
                         (should-not (niwa-journal-claim-outcome claim))
                         (should (= 0 calls)))
                     (niwa-journal-claim-settle claim 'cancelled "")))
                 :doc "a busy queued opportunity does not suppress the post-recovery offer"
                 (let ((claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180)))
                   (unwind-protect
                       (progn
                         (dotimes (n 2) (digest (1+ n)))
                         (niwa-memory-pass-schedule workspace)
                         (drain)
                         (should (= 0 calls))
                         (niwa-journal-claim-settle claim 'cancelled "")
                         (niwa-memory-pass-schedule workspace)
                         (drain)
                         (should (= 1 calls))
                         (should (= 2 (length selected)))
                         (let (messages)
                           (niwa-test--with-captured-messages messages (finish))
                           (should (string-search "memory review covered 2 digests" messages))))
                     (niwa-journal-claim-settle claim 'cancelled "")))
                 :doc "manual and cached count failures perform no filesystem work at turn completion"
                 (progn
                   (should-not (start :automatic t))
                   (cl-letf (((symbol-function 'niwa-session-control-fs-path-exists-p) (lambda (&rest _) (ert-fail "Hot gate performed I/O")))
                             ((symbol-function 'niwa-journal-store-entries) (lambda (&rest _) (ert-fail "Hot gate scanned the journal"))))
                            (niwa-memory-pass-schedule workspace)
                            (should (= 0 (hash-table-count niwa-memory-pass--pending)))
                            (setf (niwa-workspace-memory-schedule workspace) nil)
                            (let ((niwa-memory-consolidation-mode 'manual))
                              (niwa-memory-pass-schedule workspace)
                              (should (= 0 (hash-table-count niwa-memory-pass--pending))))))
                 :doc "queued turns coalesce, publish one review, and never recursively drain the backlog"
                 (progn
                   (dotimes (n 24) (digest (1+ n)))
                   (niwa-test--with-captured-messages nil
                                                         (niwa-memory-pass-schedule workspace)
                                                         (niwa-memory-pass-schedule workspace)
                                                         (should (= 1 (hash-table-count niwa-memory-pass--pending)))
                                                         (drain)
                                                         (should (= 1 calls))
                                                         (should (= 20 (length selected)))
                                                         (finish)
                                                         (accept-process-output nil 0.02))
                   (should (= 1 calls))
                   (should (= 4 (length (niwa-journal-index-unreviewed (niwa-journal-store-entries root))))))
                 :doc "auto applies memory and instruction proposals sequentially and counts shared files once"
                 (let ((memory (proposals))
                       (niwa-memory-consolidation-mode 'auto))
                   ;; Admission follows the live origin, never the unrelated
                   ;; buffer delivering the asynchronous callback.
                   (with-temp-buffer
                     (setq-local niwa-memory-consolidation-mode 'auto)
                     (start :memory-only t)
                     (with-temp-buffer
                       (setq-local niwa-memory-consolidation-mode 'manual)
                       (finish)))
                   (should (eq 'success (plist-get result :outcome)))
                   (should (= 4 (plist-get result :updated-files)))
                   (should (= 3 (plist-get result :applied-count)))
                   (should (= 0 (plist-get result :held-count)))
                   (should (file-exists-p (file-name-concat memory "one.md")))
                   (should (file-exists-p (file-name-concat memory "two.md")))
                   (let ((guidance (niwa-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))
                     (should (string-prefix-p "Existing guidance.\n" guidance))
                     (should (string-search "Use the checked workflow." guidance)))
                   (should (= 3 (length (seq-filter (lambda (entry) (eq 'decision (plist-get entry :kind)))
                                                    (niwa-journal-store-entries root))))))
                 :doc "exit cancels queued and running work and fences late queue callbacks"
                 (progn
                   (dotimes (n 2) (digest (1+ n)))
                   (niwa-memory-pass-schedule workspace)
                   (let* ((timer (gethash workspace niwa-memory-pass--pending))
                          (function (timer--function timer)) (args (timer--args timer)))
                     (niwa-memory-pass-stop-all)
                     (apply function args)
                     (should (= 0 calls)))
                   (start)
                   (niwa-memory-pass-stop-all)
                   (should (eq 'aborted (plist-get result :outcome)))
                   (should-not (niwa-memory-pass-running workspace))
                   (finish)
                   (should (= 2 (length (niwa-journal-store-entries root)))))
                 :doc "auto preserves an intervening creation and can apply an independent fresh proposal"
                 (let ((memory (proposals))
                       (niwa-memory-consolidation-mode 'auto))
                   (start :memory-only t)
                   (write-region "User's new lesson.\n" nil (file-name-concat memory "one.md") nil 'silent)
                   (finish)
                   (should (= 3 (plist-get result :updated-files)))
                   (should (= 1 (plist-get result :unapplied-count)))
                   (should (string-search "Use the checked workflow."
                                          (niwa-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))
                   (should (equal "User's new lesson.\n" (niwa-session-control-fs-read-file (file-name-concat memory "one.md"))))
                   (should (file-exists-p (file-name-concat memory "two.md"))))
                 :doc "cancellation during auto application holds the remaining proposals"
                 (let ((memory (proposals))
                       (niwa-memory-consolidation-mode 'auto)
                       (original (symbol-function 'niwa-memory-decision-apply)))
                   (start :memory-only t)
                   (cl-letf (((symbol-function 'niwa-memory-decision-apply)
                              (lambda (&rest args)
                                (prog1 (apply original args) (niwa-memory-pass-cancel workspace)))))
                            (finish))
                   (should (= 2 (plist-get result :updated-files)))
                   (should (= 2 (plist-get result :held-count)))
                   (should (file-exists-p (file-name-concat memory "one.md")))
                   (should-not (file-exists-p (file-name-concat memory "two.md"))))
                 :doc "propose leaves fresh memory and instruction changes for explicit approval"
                 (let ((memory (proposals)))
                   (start :memory-only t)
                   (finish)
                   (should (= 3 (length (plist-get (plist-get result :entry) :proposals))))
                   (should-not (file-exists-p (file-name-concat memory "one.md")))
                   (should-not (file-exists-p (file-name-concat memory "two.md")))
                   (should-not (plist-get result :updated-files)))
                 :doc "a failed count is throttled for ten minutes and a later turn rechecks it"
                 (niwa-test--with-captured-messages nil
                                                       (digest 1)
                                                       (niwa-memory-pass-schedule workspace)
                                                       (drain)
                                                       (should (= 0 calls))
                                                       (digest 2)
                                                       (niwa-memory-pass-schedule workspace)
                                                       (should (= 0 (hash-table-count niwa-memory-pass--pending)))
                                                       (setf (plist-get (niwa-workspace-memory-schedule workspace) :after) (1- (float-time)))
                                                       (niwa-memory-pass-schedule workspace)
                                                       (drain)
                                                       (should (= 1 calls))
                                                       (should (= 2 (length selected)))
                                                       (finish))
                 :doc "transport deferral can be cancelled without inference or a leaked opportunity"
                 (let ((key (list 'memory-pass workspace)))
                   (dotimes (n 2) (digest (1+ n)))
                   ;; Queue the opportunity before the transport becomes busy;
                   ;; scheduling inside a handler now correctly holds its timer.
                   (niwa-memory-pass-schedule workspace)
                   (let ((niwa-transport--depth 1))
                     (accept-process-output nil 0.05)
                     (should (= 0 calls))
                     (should (gethash key niwa-transport--pending))
                     (should (= 1 (hash-table-count niwa-memory-pass--pending)))
                     (niwa-transport-cancel-pending key)
                     (should (= 0 (hash-table-count niwa-memory-pass--pending)))))
                 :doc "opening preserves an overdue backlog; saved turns offer it only in propose mode"
                 (niwa-test--with-captured-diagnostics nil
                   (digest 1 30)
                   (setf (niwa-workspace-type workspace) 'file
                         (niwa-workspace-id workspace) root)
                   (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
                   (dolist (mode '(manual propose))
                     (let ((niwa-memory-consolidation-mode mode)
                           buffer view)
                       (unwind-protect
                           (progn
                             (setq buffer (niwa--chat-buffer
                                           (symbol-name mode) t workspace root))
                             (setq view (buffer-local-value 'niwa--view-buffer buffer))
                             (drain)
                             (should (= 0 calls))
                             (should (= 1 (length (niwa-journal-index-unreviewed
                                                  (niwa-journal-store-entries root)))))
                             (with-current-buffer buffer
                               (insert "Completed turn\n")
                               (setf (niwa-session-turn-count niwa--session) 1))
                             (let ((niwa-journal-enabled nil))
                               (niwa--run-turn-steps (gptel-make-fsm :info (list :buffer buffer)) '(niwa--turn-save niwa--turn-checkpoint)))
                             (drain)
                             (if (eq mode 'manual)
                                 (progn (should (= 0 calls))
                                        (should (= 1 (length (niwa-journal-index-unreviewed
                                                              (niwa-journal-store-entries root))))))
                               (should (= 1 calls))
                               (should (= 1 (length selected)))
                               (finish)))
                         (let ((niwa-journal-enabled nil))
                           (when (buffer-live-p view) (kill-buffer view))
                           (when (buffer-live-p buffer) (kill-buffer buffer)))))))
                 :doc "an empty workspace stays empty and caches its unsuccessful opportunity"
                 (progn
                   (niwa-memory-pass-schedule workspace)
                   (drain)
                   (should-not (file-exists-p (niwa-journal-store-directory root)))
                   (niwa-memory-pass-schedule workspace)
                   (should (= 0 (hash-table-count niwa-memory-pass--pending)))
                   (should (= 0 calls)))
                 :doc "a quiet shared note alone offers a review even with journaling disabled"
                 (let ((niwa-journal-enabled nil)
                       (note (file-name-concat root ".niwa" "shared" "handoff.md")))
                   (make-directory (file-name-directory note) t)
                   (write-region "Finding to review.\n" nil note nil 'silent)
                   (set-file-times note (seconds-to-time (- now (* 15 86400))))
                   (niwa-memory-pass-schedule workspace)
                   (drain)
                   (should (= 1 calls))
                   (should-not selected)
                   (let (messages)
                     (niwa-test--with-captured-messages messages
                       (niwa-memory-pass-cancel workspace))
                     (should (string-match-p "memory review cancelled" messages)))))

(provide 'test-niwa-memory-pass-scheduling)
;;; test-niwa-memory-pass-scheduling.el ends here
