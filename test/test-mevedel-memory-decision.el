;;; test-mevedel-memory-decision.el -- Durable proposal decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Real accepted passes verify rejection, recovery, privacy, and evidence pins.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-memory-decision)
(require 'mevedel-memory-pass)
(require 'mevedel-session-persistence)
(require 'mevedel-memory-review)
(require 'mevedel-system)
(require 'gptel-openai)

(mevedel-deftest mevedel-memory-decision-reject
    (:vars* ((root (make-temp-file "mevedel-memory-decision-" t))
             (memory (file-name-concat root ".agents" "memory"))
             (workspace (mevedel-workspace--create :root root))
             (identity (mevedel-workspace-identity-ensure root))
             (mevedel-memory-dirs (list memory))
             claim accepted proposal pass review digest)
     :after-each ((when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (progn
  (make-directory memory t)
  (write-region "- [Topic](topic.md) - old guidance\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
  (write-region "PRIVATE ORIGINAL TOPIC\n" nil (file-name-concat memory "topic.md") nil 'silent)
  (setq digest (mevedel-journal-store-publish-digest
                root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                           :workspace identity :trigger 'session-end :segment 1
                           :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (make-string 64 ?c))
                           :created "2020-01-01T00:00:00Z" :model "test:model")
                "## Done\n- Observed: Tests passed.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
  (let* ((scope (mevedel-memory-scope-capture workspace))
         (root-id (caar (plist-get scope :roots)))
         (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\n"
                                "file: \"topic.md\"\ntype: \"project\"\ntitle: \"Topic\"\nhook: \"Current context\"\n"
                                "reason: \"Preserve evidence\"\nevidence: []\n---\nPRIVATE PROPOSED TOPIC\n```\n"
                                "## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)))
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let ((prepared (mevedel-memory-store-prepare workspace claim scope (list digest) "")))
      (setq accepted (mevedel-memory-store-accept workspace prepared reply (list digest) "test:model" nil)
            pass (plist-get prepared :id)
            proposal (car (plist-get accepted :proposals))
            review (mevedel-memory-store-publish workspace pass)))
    (should (file-exists-p (mevedel-memory-store--pin workspace pass (plist-get digest :id))))
    ,test))
  (test)
  :doc "rejects idempotently without editing memory or rewriting its completed review"
  (let ((decision (mevedel-memory-decision-reject workspace pass (plist-get proposal :id) "Abandoned choice")))
    (should (eq 'rejected (plist-get decision :status)))
    (let ((original (symbol-function 'mevedel-session-control-fs-run-program))
          (calls 0))
      (cl-letf (((symbol-function 'mevedel-session-control-fs-run-program)
                 (lambda (&rest args) (cl-incf calls) (apply original args))))
        (should (equal (plist-get decision :decision-id)
                       (plist-get
                        (plist-get
                         (mevedel-memory-decision--record
                          workspace
                          (file-name-concat (mevedel-memory-decision--directory workspace)
                                            (concat (plist-get decision :decision-id) ".el")))
                         :metadata)
                        :decision-id))))
      (should (= calls 2)))
    (should (equal decision (mevedel-memory-decision-reject workspace pass (plist-get proposal :id) "A second reason")))
    (should (equal decision (mevedel-memory-decision-status workspace (plist-get proposal :id))))
    (should (equal "Abandoned choice" (plist-get decision :reason)))
    (should (equal "PRIVATE ORIGINAL TOPIC\n" (mevedel-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal review (mevedel-journal-store-read root (plist-get review :file))))
    (should-not (string-match-p "PRIVATE" (plist-get decision :text)))
    (should-not (file-exists-p (mevedel-memory-store--pin workspace pass (plist-get digest :id))))
    (let ((evidence (mevedel-memory-decision-rejections workspace)))
      (should (string-match-p "Abandoned choice" evidence))
      (should (string-match-p "PRIVATE PROPOSED TOPIC" evidence))
      (should (<= (string-bytes evidence) 8192)))
    (should (= 3 (length (mevedel-journal-store-entries root)))))
  :doc "completed rejection releases evidence for ordinary journal expiry"
  (let ((mevedel-journal-max-age-days 0))
    (should (= 0 (mevedel-journal-cleanup-expired workspace t)))
    (mevedel-memory-decision-reject workspace pass (plist-get proposal :id))
    (should (= 1 (mevedel-journal-cleanup-expired workspace t)))
    (mevedel-memory-decision-recover workspace)
    (should (eq 'rejected (plist-get (mevedel-memory-decision-status workspace (plist-get proposal :id)) :status))))
  :doc "recent rejection reasons and original suggestions reach the next bounded request"
  (let* ((gptel--known-backends nil)
         (model (make-symbol "rejection-test-model"))
         (backend (gptel-make-openai "rejection-test" :key "test-only" :models (list model)))
         (real-request (symbol-function 'gptel-request)) sent)
    (put model :context-window 128)
    (put model :capabilities '(tool-use))
    (mevedel-memory-decision-reject workspace pass (plist-get proposal :id) "Abandoned-choice-sentinel \u2013 \u754c")
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload) (lambda (&rest _) (list :backend backend :model model)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest args)
                 (unless (plist-get args :dry-run) (setq sent prompt))
                 (apply real-request prompt (plist-put (copy-sequence args) :dry-run t)))))
      (unwind-protect
          (progn
            (mevedel-memory-pass-start workspace #'ignore :memory-only t)
            (let ((deadline (+ (float-time) 10)))
              (while (and (not sent) (< (float-time) deadline))
                (accept-process-output nil .01)))
            (should (string-match-p "Abandoned-choice-sentinel" sent))
            (should (string-match-p "PRIVATE PROPOSED TOPIC" sent)))
        (mevedel-memory-pass-cancel workspace))))
  :doc "an oversized rejected suggestion is omitted whole with a bounded omission count"
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let* ((prepared (mevedel-memory-store-prepare workspace claim scope nil ""))
           (large (mevedel-memory-store-accept workspace prepared
                                              (string-replace "PRIVATE PROPOSED TOPIC" (make-string 9000 ?x) reply)
                                              nil "test:model" nil))
           (item (car (plist-get large :proposals))))
      (mevedel-memory-store-publish workspace (plist-get prepared :id))
      (mevedel-memory-decision-reject workspace (plist-get prepared :id) (plist-get item :id))
      (let* ((text (mevedel-memory-decision-rejections workspace)) (data (json-parse-string text)))
        (should (<= (string-bytes text) 8192))
        (should (= 1 (gethash "omitted" data)))
        (should (= 0 (length (gethash "rejections" data)))))))
  :doc "one pending proposal retains shared pass evidence until the last rejection"
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (let* ((extra (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                 "title: \"New\"\nhook: \"Context\"\nreason: \"Evidence\"\nevidence: []\n---\nNew context.\n```") root-id))
           (prepared (mevedel-memory-store-prepare workspace claim scope (list digest) ""))
           (batch (mevedel-memory-store-accept workspace prepared (string-replace "## Promote\n- none" extra reply)
                                              (list digest) "test:model" nil))
           (id (plist-get prepared :id))
           (items (plist-get batch :proposals))
           (pin (mevedel-memory-store--pin workspace id (plist-get digest :id))))
      (should (= 2 (length items)))
      (mevedel-memory-store-publish workspace id)
      (mevedel-memory-decision-reject workspace id (plist-get (car items) :id))
      (should (file-exists-p pin))
      (mevedel-memory-decision-reject workspace id (plist-get (cadr items) :id))
      (should-not (file-exists-p pin))
      (should (file-exists-p (mevedel-memory-store--pin workspace pass (plist-get digest :id))))))
  :doc "a decision waits for this client's active cleanup to release ownership"
  (let* ((mevedel-journal-worker--child-p t)
         (mevedel-journal-cleanup--pending (make-hash-table :test #'equal))
         (iterator (mevedel-journal-cleanup--steps workspace t))
         (job (cons nil nil)) timer)
    (unwind-protect
        (progn
          (dotimes (_ 5) (iter-next iterator))
          (should-not (mevedel-journal-claim-acquire
                       (mevedel-memory-store--claim-directory workspace) 180))
          (puthash root job mevedel-journal-cleanup--pending)
          (setq timer (run-at-time .05 nil
                                   (lambda ()
                                     (iter-close iterator)
                                     (remhash root mevedel-journal-cleanup--pending))))
          (mevedel-test--with-captured-messages nil
            (should (eq 'rejected
                        (plist-get (mevedel-memory-decision-reject
                                    workspace pass (plist-get proposal :id)) :status)))))
      (when timer (cancel-timer timer))
      (iter-close iterator)))
  :doc "busy ownership and unknown proposal identity cannot create decisions"
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (should-error (mevedel-memory-decision-reject workspace pass (plist-get proposal :id)))
    (should-not (mevedel-journal-claim-outcome claim))
    (mevedel-journal-claim-settle claim 'cancelled "")
    (should-error (mevedel-memory-decision-reject workspace pass (make-string 64 ?f)))
    (should (equal (list review digest) (mevedel-journal-store-entries root))))
  :doc "an accepted rejection recovers after the owner stops before public publication"
  (progn
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-memory-decision--accept workspace claim accepted proposal 'rejected "Superseded")
    (should (equal (list review digest) (mevedel-journal-store-entries root)))
    (mevedel-memory-decision-recover workspace)
    (let ((decision (mevedel-memory-decision-status workspace (plist-get proposal :id))))
      (should (eq 'rejected (plist-get decision :status)))
      (should (equal "Superseded" (plist-get decision :reason)))
      (mevedel-memory-decision-recover workspace)
      (should (equal decision (mevedel-memory-decision-status workspace (plist-get proposal :id))))
      (should (= 3 (length (mevedel-journal-store-entries root))))
      ;; Recovery must also repair missing review publication or interrupted
      ;; pin release when the decision itself is already public.
      (delete-file (file-name-concat (mevedel-journal-store-directory root) (plist-get review :file)))
      (should-error (mevedel-memory-decision-recover workspace)
                    :type 'mevedel-session-control-fs-absent)
      (let ((pin (mevedel-memory-store--pin workspace pass (plist-get digest :id))))
        (make-directory (file-name-directory pin) t)
        (write-region (plist-get (plist-get accepted :prepared) :hash) nil pin nil 'silent)
        (mevedel-memory-decision-recover workspace)
        (should (= 3 (length (mevedel-journal-store-entries root))))
        (should-not (file-exists-p pin))
        (write-region (plist-get (plist-get accepted :prepared) :hash) nil pin nil 'silent)
        (mevedel-memory-decision-recover workspace)
        (should-not (file-exists-p pin)))))
  :doc "a public decision without immutable acceptance cannot decide a pending proposal"
  (progn
    (mevedel-journal-store-publish-decision
     root (list :decision-id (make-string 64 ?e) :pass-id pass :proposal-id (plist-get proposal :id)
                :workspace identity :created "2026-09-07T12:00:00Z" :status 'rejected
                :reason "Forged rejection" :state-hash (make-string 64 ?f)))
    (should-error (mevedel-memory-decision-status workspace (plist-get proposal :id)))
    (should-error (mevedel-memory-decision-reject workspace pass (plist-get proposal :id)))
    (should (file-exists-p (mevedel-memory-store--pin workspace pass (plist-get digest :id))))
    (let ((evidence (mevedel-memory-decision-rejections workspace)))
      (should-not (string-match-p "Forged rejection" evidence))
      (should (= 1 (gethash "omitted" (json-parse-string evidence))))))
  :doc "changed accepted before-state cannot authorize a rejection"
  (let* ((path (file-name-concat (mevedel-memory-store--directory workspace pass) "prepared.el"))
         (original (mevedel-session-control-fs-read-file path)))
    ;; Changing original scope bytes invalidates its accepted bundle fingerprint.
    (write-region (concat original "\n") nil path nil 'silent)
    (should-error (mevedel-memory-decision-reject workspace pass (plist-get proposal :id)))
    (should (equal (list review digest) (mevedel-journal-store-entries root))))
  :doc "each rejection-evidence observation revalidates the accepted private bundle"
  (let ((path (file-name-concat (mevedel-memory-store--directory workspace pass) "accepted.el")))
    (mevedel-memory-decision-reject workspace pass (plist-get proposal :id))
    (should (string-match-p "PRIVATE PROPOSED TOPIC" (mevedel-memory-decision-rejections workspace)))
    (write-region "\n" nil path t 'silent)
    (let* ((text (mevedel-memory-decision-rejections workspace)) (data (json-parse-string text)))
      (should-not (string-match-p "PRIVATE PROPOSED TOPIC" text))
      (should (= 1 (gethash "omitted" data)))
      (should (= 0 (length (gethash "rejections" data))))))
  :doc "retargeting the configured memory root makes rejection unavailable"
  (let ((elsewhere (file-name-concat root "elsewhere")))
    (rename-file memory elsewhere)
    (make-directory memory t)
    (rename-file memory (concat memory "-empty"))
    (make-symbolic-link (concat memory "-empty") memory)
    (should-error (mevedel-memory-decision-reject workspace pass (plist-get proposal :id)))
    (should (equal (list review digest) (mevedel-journal-store-entries root)))
    (should (equal "PRIVATE ORIGINAL TOPIC\n"
                   (mevedel-session-control-fs-read-file (file-name-concat elsewhere "topic.md"))))))

(mevedel-deftest mevedel-memory-decision-schedule-recovery
    (:vars* ((root (make-temp-file "mevedel-memory-activation-" t))
             (workspace (mevedel-workspace--create :root root))
             (mevedel-memory-decision--inhibit-recovery nil) claim)
     :after-each ((mevedel-memory-decision-stop-recovery)
                  (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels ((drain ()
                (let ((deadline (+ (float-time) 5)))
                  (while (and (or (> (hash-table-count mevedel-memory-decision--recovery-pending) 0)
                                 (> (hash-table-count mevedel-memory-decision--recovery-workers) 0))
                              (< (float-time) deadline))
                    (accept-process-output nil 0.01)))
                (should (= 0 (hash-table-count mevedel-memory-decision--recovery-pending)))))
    ,test)
  (test)
  :doc "activation of a new workspace creates no memory state"
  (progn
    (mevedel-memory-decision-schedule-recovery workspace)
    (drain)
    (should-not (file-exists-p (file-name-concat root ".mevedel"))))
  :doc "activation leaves a live owner untouched without diagnostics"
  (let (diagnostics)
    (setq claim (mevedel-journal-claim-acquire (mevedel-memory-store--claim-directory workspace) 180))
    (mevedel-test--with-captured-diagnostics diagnostics
      (mevedel-memory-decision-schedule-recovery workspace)
      (drain))
    (should (string-empty-p diagnostics))
    (should-not (mevedel-journal-claim-outcome claim))
    (should (equal claim (mevedel-journal-claim-current (plist-get claim :directory)))))
  :doc "activation waits for transport and cleans up a cancelled deferred callback"
  (let ((key (list 'memory-recovery (mevedel-memory-store--claim-directory workspace))))
    (let ((mevedel-transport--depth 1))
      (mevedel-memory-decision-schedule-recovery workspace)
      (let ((deadline (+ (float-time) 2)))
        (while (and (not (gethash key mevedel-transport--pending)) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (gethash key mevedel-transport--pending))
      (should (= 1 (hash-table-count mevedel-memory-decision--recovery-pending)))
      (mevedel-transport-cancel-pending key)
      (should (= 0 (hash-table-count mevedel-memory-decision--recovery-pending))))
    (mevedel-memory-decision-schedule-recovery workspace)
    (drain)
    (should-not (file-exists-p (file-name-concat root ".mevedel")))))

(mevedel-deftest mevedel-memory-decision-stop-recovery ()
  ,test
  (test)
  :doc "exit cancels queued memory recovery without creating state or accepting late timers"
  (let* ((root (make-temp-file "mevedel-memory-exit-" t))
         (workspace (mevedel-workspace--create :root root))
         (mevedel-memory-decision--inhibit-recovery nil))
    (unwind-protect
        (progn
          (mevedel-memory-decision-schedule-recovery workspace)
          (let ((timer (gethash (mevedel-memory-store--claim-directory workspace)
                                mevedel-memory-decision--recovery-pending)))
            (should (timerp timer))
            (mevedel-session-persistence--kill-emacs-hook)
            (should-not (memq timer timer-list))
            (mevedel-memory-decision-schedule-recovery workspace)
            (let ((successor (gethash (mevedel-memory-store--claim-directory workspace)
                                     mevedel-memory-decision--recovery-pending)))
              (funcall (timer--function timer))
              (should (eq successor (gethash (mevedel-memory-store--claim-directory workspace)
                                             mevedel-memory-decision--recovery-pending))))
            (mevedel-memory-decision-stop-recovery))
          (should (= 0 (hash-table-count mevedel-memory-decision--recovery-pending)))
          (should-not (file-exists-p (file-name-concat root ".mevedel"))))
      (mevedel-memory-decision-stop-recovery)
      (delete-directory root t))))

(mevedel-deftest mevedel-memory-decision-terminal-status-p
  (:doc "ends a proposal's lifecycle only for applied, rejected, and reversed")
  (progn
    (dolist (status '(applied rejected reversed)) (should (mevedel-memory-decision-terminal-status-p status)))
    (dolist (status '(pending stale unavailable recovery-required nil))
      (should-not (mevedel-memory-decision-terminal-status-p status)))))

(mevedel-deftest mevedel-memory-decision-actionable-status-p
  (:doc "awaits a decision only while pending or stale")
  (progn
    (dolist (status '(pending stale)) (should (mevedel-memory-decision-actionable-status-p status)))
    (dolist (status '(applied rejected reversed unavailable recovery-required))
      (should-not (mevedel-memory-decision-actionable-status-p status)))))

(mevedel-deftest mevedel-memory-decision-written-status-p
  (:doc "names decisions whose write reached the target")
  (progn
    (dolist (status '(applied reversed)) (should (mevedel-memory-decision-written-status-p status)))
    (dolist (status '(rejected stale unavailable recovery-required))
      (should-not (mevedel-memory-decision-written-status-p status)))))

(mevedel-deftest mevedel-memory-decision-resolved-write-status-p
  (:doc "settles a write attempt for applied, reversed, and unavailable only")
  (progn
    (dolist (status '(applied reversed unavailable))
      (should (mevedel-memory-decision-resolved-write-status-p status)))
    (dolist (status '(rejected stale recovery-required pending))
      (should-not (mevedel-memory-decision-resolved-write-status-p status)))))

(provide 'test-mevedel-memory-decision)
;;; test-mevedel-memory-decision.el ends here
