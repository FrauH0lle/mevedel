;;; test-mevedel-memory-pass-scheduling.el -- Automatic memory review -*- lexical-binding: t -*-

;;; Commentary:

;; Real journal publications exercise automatic admission without network calls.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file)) "helpers"))
(require 'mevedel-memory-pass)
(require 'mevedel-journal-capture)
(require 'mevedel)
(require 'mevedel-system)
(require 'gptel-openai)

(mevedel-deftest mevedel-memory-pass-schedule
                 (:vars* ((root (make-temp-file "mevedel-memory-schedule-" t))
                          (workspace (mevedel-workspace--create :root root))
                          (identity (mevedel-workspace-identity-ensure root))
                          (mevedel-memory-dirs nil)
                          (mevedel-journal-max-age-days 14)
                          (mevedel-memory-consolidation-mode 'propose)
                          (mevedel-memory-consolidation-min-hours 24)
                          (mevedel-memory-consolidation-min-digests 5)
                          (mevedel-memory-pass--inhibit-scheduling nil)
                          (gptel--known-backends nil)
                          (backend (gptel-make-openai "schedule-test" :key "test-only" :models '(test-model)))
                          (now 1800000000)
                          (calls 0) callback selected state result
                          (none "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes."))
                         :after-each ((mevedel-memory-pass-stop-all) (delete-directory root t)))
                 (cl-letf (((symbol-function 'mevedel-session-control-fs-target-time) (lambda (_) now))
                           ((symbol-function 'mevedel-memory-review-request)
                            (lambda (_scope entries cb &rest _)
                              (cl-incf calls)
                              (setq callback cb selected entries)
                              (list :cancel #'ignore))))
                          (cl-labels ((digest (number &optional age-days)
                                        (mevedel-journal-store-publish-digest
                                         root (list :capture-id (format "%064x" number) :session "closed" :session-name "Closed"
                                                    :workspace identity :trigger 'session-end :segment 1
                                                    :source-revision (make-string 64 ?c) :turns '(1)
                                                    :turn-ids (list (format "%064x" number))
                                                    :created (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                                                 (- now (* (or age-days 0) 86400)) t) :model "test:model")
                                         "## Done\n- Observed: Tests passed.\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none"))
                                      (start (&rest args)
                                        (setq state (apply #'mevedel-memory-pass-start workspace (lambda (value) (setq result value)) args)))
                                      (finish ()
                                        (funcall callback (list :outcome 'success :entries selected :reply none
                                                                :policy (list :backend backend :model 'test-model))))
                                      (proposals ()
                                        (let ((memory (file-name-concat root "memory")))
                                          (setq mevedel-memory-dirs (list memory))
                                          (make-directory memory)
                                          (write-region "Existing guidance.\n" nil (file-name-concat root "AGENTS.md") nil 'silent)
                                          (let* ((scope (mevedel-memory-scope-capture workspace))
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
                                                                   "hook: \"Guidance\"\nreason: \"Evidence\"\nevidence: []\n---\nUse the checked workflow.\n```\n") instruction-id)
                                                   "## No action\n- none")))
                                          memory))
                                      (drain ()
                                        (let ((deadline (+ (float-time) 10)))
                                          (while (and (> (hash-table-count mevedel-memory-pass--pending) 0)
                                                      (< (float-time) deadline))
                                            (accept-process-output nil 0.01))
                                          (should (= 0 (hash-table-count mevedel-memory-pass--pending))))))
                                     ,test))
                 (test)
                 :doc "automatic admission requires five eligible digests while explicit review bypasses the gate"
                 (progn
                   (dotimes (n 4) (digest (1+ n)))
                   (should-not (start :automatic t))
                   (should (= 0 calls))
                   (should-not (mevedel-memory-pass-running workspace))
                   (should (> (plist-get (mevedel-workspace-memory-schedule workspace) :after) (float-time)))
                   (digest 5)
                   (should (start :automatic t))
                   (should (= 5 (length selected)))
                   (finish)
                   (should-not (start :automatic t))
                   (let ((mevedel-memory-consolidation-mode 'manual))
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
                 (let ((mevedel-journal-max-age-days 30))
                   (digest 1 28)
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (let ((mevedel-journal-max-age-days nil))
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
                   (dotimes (n 5) (digest (1+ n)))
                   (start :focus "Source conditions")
                   (finish)
                   (should (start :automatic t))
                   (should (= 5 (length selected)))
                   (finish)
                   (should (= 3 calls)))
                 :doc "retired general completion still postpones review without retaining private bodies"
                 (progn
                   (start :memory-only t)
                   (finish)
                   (let ((mevedel-memory-history-max-age-days 0))
                     (cl-incf now 1)
                     (should (= 1 (mevedel-journal-cleanup-expired workspace))))
                   (should-not (mevedel-journal-store-entries root))
                   (dotimes (n 5) (digest (1+ n)))
                   (should-not (start :automatic t))
                   (cl-incf now 86400)
                   (should (start :automatic t))
                   (finish))
                 :doc "a recovered general publication advances the gate before a successor selects evidence"
                 (let* ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
                        (prepared (mevedel-memory-store-prepare workspace claim (mevedel-memory-scope-capture workspace) nil "")))
                   (mevedel-memory-store-accept workspace prepared none nil "test:model" nil)
                   (dotimes (n 5) (digest (1+ n)))
                   (should-not (start :automatic t))
                   (should (= 0 calls))
                   (should (= 6 (length (mevedel-journal-store-entries root)))))
                 :doc "another client's live claim postpones automatic admission without settling it"
                 (let ((claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180)))
                   (unwind-protect
                       (progn
                         (dotimes (n 5) (digest (1+ n)))
                         (should-not (start :automatic t))
                         (should-not (mevedel-journal-claim-outcome claim))
                         (should (= 0 calls)))
                     (mevedel-journal-claim-settle claim 'cancelled "")))
                 :doc "manual and cached count failures perform no filesystem work at turn completion"
                 (progn
                   (should-not (start :automatic t))
                   (cl-letf (((symbol-function 'mevedel-session-control-fs-path-exists-p) (lambda (&rest _) (ert-fail "Hot gate performed I/O")))
                             ((symbol-function 'mevedel-journal-store-entries) (lambda (&rest _) (ert-fail "Hot gate scanned the journal"))))
                            (mevedel-memory-pass-schedule workspace)
                            (should (= 0 (hash-table-count mevedel-memory-pass--pending)))
                            (setf (mevedel-workspace-memory-schedule workspace) nil)
                            (let ((mevedel-memory-consolidation-mode 'manual))
                              (mevedel-memory-pass-schedule workspace)
                              (should (= 0 (hash-table-count mevedel-memory-pass--pending))))))
                 :doc "queued turns coalesce, publish one review, and never recursively drain the backlog"
                 (progn
                   (dotimes (n 24) (digest (1+ n)))
                   (mevedel-test--with-captured-messages nil
                                                         (mevedel-memory-pass-schedule workspace)
                                                         (mevedel-memory-pass-schedule workspace)
                                                         (should (= 1 (hash-table-count mevedel-memory-pass--pending)))
                                                         (drain)
                                                         (should (= 1 calls))
                                                         (should (= 20 (length selected)))
                                                         (finish)
                                                         (accept-process-output nil 0.02))
                   (should (= 1 calls))
                   (should (= 4 (length (mevedel-journal-index-unreviewed (mevedel-journal-store-entries root))))))
                 :doc "auto applies memory proposals sequentially, counts shared files once, and holds instructions"
                 (let ((memory (proposals))
                       (mevedel-memory-consolidation-mode 'auto))
                   (start :memory-only t)
                   ;; The mode is frozen when the pass starts, not read from the current buffer
                   ;; when its asynchronous callback arrives.
                   (let ((mevedel-memory-consolidation-mode 'manual)) (finish))
                   (should (eq 'success (plist-get result :outcome)))
                   (should (= 3 (plist-get result :updated-files)))
                   (should (= 2 (plist-get result :applied-count)))
                   (should (= 1 (plist-get result :held-count)))
                   (should (file-exists-p (file-name-concat memory "one.md")))
                   (should (file-exists-p (file-name-concat memory "two.md")))
                   (should (equal "Existing guidance.\n" (mevedel-session-control-fs-read-file (file-name-concat root "AGENTS.md"))))
                   (should (= 2 (length (seq-filter (lambda (entry) (eq 'decision (plist-get entry :kind)))
                                                    (mevedel-journal-store-entries root))))))
                 :doc "exit cancels queued and running work and fences late queue callbacks"
                 (progn
                   (dotimes (n 5) (digest (1+ n)))
                   (mevedel-memory-pass-schedule workspace)
                   (let* ((timer (gethash workspace mevedel-memory-pass--pending))
                          (function (timer--function timer)) (args (timer--args timer)))
                     (mevedel-memory-pass-stop-all)
                     (apply function args)
                     (should (= 0 calls)))
                   (start)
                   (mevedel-memory-pass-stop-all)
                   (should (eq 'aborted (plist-get result :outcome)))
                   (should-not (mevedel-memory-pass-running workspace))
                   (finish)
                   (should (= 5 (length (mevedel-journal-store-entries root)))))
                 :doc "auto preserves an intervening creation and can apply an independent fresh proposal"
                 (let ((memory (proposals))
                       (mevedel-memory-consolidation-mode 'auto))
                   (start :memory-only t)
                   (write-region "User's new lesson.\n" nil (file-name-concat memory "one.md") nil 'silent)
                   (finish)
                   (should (= 2 (plist-get result :updated-files)))
                   (should (= 1 (plist-get result :unapplied-count)))
                   (should (equal "User's new lesson.\n" (mevedel-session-control-fs-read-file (file-name-concat memory "one.md"))))
                   (should (file-exists-p (file-name-concat memory "two.md"))))
                 :doc "cancellation during auto application holds the remaining proposals"
                 (let ((memory (proposals))
                       (mevedel-memory-consolidation-mode 'auto)
                       (original (symbol-function 'mevedel-memory-decision-apply)))
                   (start :memory-only t)
                   (cl-letf (((symbol-function 'mevedel-memory-decision-apply)
                              (lambda (&rest args)
                                (prog1 (apply original args) (mevedel-memory-pass-cancel workspace)))))
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
                 (mevedel-test--with-captured-messages nil
                                                       (dotimes (n 4) (digest (1+ n)))
                                                       (mevedel-memory-pass-schedule workspace)
                                                       (drain)
                                                       (should (= 0 calls))
                                                       (digest 5)
                                                       (mevedel-memory-pass-schedule workspace)
                                                       (should (= 0 (hash-table-count mevedel-memory-pass--pending)))
                                                       (setf (plist-get (mevedel-workspace-memory-schedule workspace) :after) (1- (float-time)))
                                                       (mevedel-memory-pass-schedule workspace)
                                                       (drain)
                                                       (should (= 1 calls))
                                                       (finish))
                 :doc "transport deferral can be cancelled without inference or a leaked opportunity"
                 (let ((key (list 'memory-pass root)))
                   (dotimes (n 5) (digest (1+ n)))
                   (let ((mevedel-transport--depth 1))
                     (mevedel-memory-pass-schedule workspace)
                     (accept-process-output nil 0.05)
                     (should (= 0 calls))
                     (should (gethash key mevedel-transport--pending))
                     (should (= 1 (hash-table-count mevedel-memory-pass--pending)))
                     (mevedel-transport-cancel-pending key)
                     (should (= 0 (hash-table-count mevedel-memory-pass--pending)))))
                 :doc "opening preserves an overdue backlog; saved turns offer it only in propose mode"
                 (mevedel-test--with-captured-diagnostics nil
                   (digest 1 30)
                   (setf (mevedel-workspace-type workspace) 'file
                         (mevedel-workspace-id workspace) root)
                   (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
                   (dolist (mode '(manual propose))
                     (let ((mevedel-memory-consolidation-mode mode)
                           buffer view)
                       (unwind-protect
                           (progn
                             (setq buffer (mevedel--chat-buffer
                                           (symbol-name mode) t workspace root))
                             (setq view (buffer-local-value 'mevedel--view-buffer buffer))
                             (drain)
                             (should (= 0 calls))
                             (should (= 1 (length (mevedel-journal-index-unreviewed
                                                  (mevedel-journal-store-entries root)))))
                             (with-current-buffer buffer
                               (insert "Completed turn\n")
                               (setf (mevedel-session-turn-count mevedel--session) 1))
                             (let ((mevedel-journal-enabled nil))
                               (mevedel--turn-autosave
                                (gptel-make-fsm :info (list :buffer buffer))))
                             (drain)
                             (if (eq mode 'manual)
                                 (progn (should (= 0 calls))
                                        (should (= 1 (length (mevedel-journal-index-unreviewed
                                                              (mevedel-journal-store-entries root))))))
                               (should (= 1 calls))
                               (should (= 1 (length selected)))
                               (finish)))
                         (let ((mevedel-journal-enabled nil))
                           (when (buffer-live-p view) (kill-buffer view))
                           (when (buffer-live-p buffer) (kill-buffer buffer)))))))
                 :doc "an empty workspace stays empty and caches its unsuccessful opportunity"
                 (progn
                   (mevedel-memory-pass-schedule workspace)
                   (drain)
                   (should-not (file-exists-p (mevedel-journal-store-directory root)))
                   (mevedel-memory-pass-schedule workspace)
                   (should (= 0 (hash-table-count mevedel-memory-pass--pending)))
                   (should (= 0 calls))))

(provide 'test-mevedel-memory-pass-scheduling)
;;; test-mevedel-memory-pass-scheduling.el ends here
