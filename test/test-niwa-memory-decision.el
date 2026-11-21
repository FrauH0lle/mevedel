;;; test-niwa-memory-decision.el -- Durable proposal decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Real accepted passes verify rejection, recovery, privacy, and evidence pins.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-memory-decision)
(require 'niwa-journal-cleanup)
(require 'niwa-memory-pass)
(require 'niwa-session-control-fs)
(require 'niwa-session-persistence)
(require 'niwa-memory-review)
(require 'niwa-models)
(require 'niwa-system)
(require 'gptel-openai)

(niwa-deftest niwa-memory-decision-reject
    (:vars* ((root (make-temp-file "niwa-memory-decision-" t))
             (memory (file-name-concat root ".agents" "memory"))
             (workspace (niwa-workspace--create :root root))
             (identity (niwa-workspace-identity-ensure root))
             (niwa-memory-dirs (list memory))
             claim accepted proposal pass review digest)
     :after-each ((when claim (niwa-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (progn
  (make-directory memory t)
  (write-region "- [Topic](topic.md) - old guidance\n" nil (file-name-concat memory "MEMORY.md") nil 'silent)
  (write-region "PRIVATE ORIGINAL TOPIC\n" nil (file-name-concat memory "topic.md") nil 'silent)
  (setq digest (niwa-journal-store-publish-digest
                root (list :capture-id (make-string 64 ?a) :session "closed" :session-name "Closed"
                           :workspace identity :trigger 'session-end :segment 1
                           :source-revision (make-string 64 ?b) :turns '(1) :turn-ids (list (make-string 64 ?c))
                           :created "2020-01-01T00:00:00Z" :model "test:model")
                "## Done\n- Observed: Tests passed.\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none"))
  (let* ((scope (niwa-memory-scope-capture workspace))
         (root-id (caar (plist-get scope :roots)))
         (reply (format (concat "## Promote\n- none\n## Update\n```proposal\nroot: %S\n"
                                "file: \"topic.md\"\ntype: \"project\"\ntitle: \"Topic\"\nhook: \"Current context\"\n"
                                "reason: \"Preserve evidence\"\nevidence: []\n---\nPRIVATE PROPOSED TOPIC\n```\n"
                                "## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- none") root-id)))
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (let ((prepared (niwa-memory-store-prepare workspace claim scope (list digest) "")))
      (setq accepted (niwa-memory-store-accept workspace prepared reply (list digest) "test:model" nil)
            pass (plist-get prepared :id)
            proposal (car (plist-get accepted :proposals))
            review (niwa-memory-store-publish workspace pass)))
    (should (file-exists-p (niwa-memory-store--pin workspace pass (plist-get digest :id))))
    ,test))
  (test)
  :doc "rejects idempotently without editing memory or rewriting its completed review"
  (let ((decision (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id) :reason "Abandoned choice") :decision)))
    (should (eq 'rejected (plist-get decision :status)))
    (let ((original (symbol-function 'niwa-session-control-fs-run-program))
          (calls 0))
      (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (&rest args) (cl-incf calls) (apply original args))))
        (should (equal (plist-get decision :decision-id)
                       (plist-get
                        (plist-get
                         (niwa-memory-decision--record
                          workspace
                          (file-name-concat (niwa-memory-decision--directory workspace)
                                            (concat (plist-get decision :decision-id) ".el")))
                         :metadata)
                        :decision-id))))
      (should (= calls 1)))
    (should (equal decision (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id) :reason "A second reason") :decision)))
    (should (equal decision (niwa-memory-decision-status workspace (plist-get proposal :id))))
    (should (equal "Abandoned choice" (plist-get decision :reason)))
    (should (equal "PRIVATE ORIGINAL TOPIC\n" (niwa-session-control-fs-read-file (file-name-concat memory "topic.md"))))
    (should (equal review (niwa-journal-store-read root (plist-get review :file))))
    (should-not (string-match-p "PRIVATE" (plist-get decision :text)))
    (should-not (file-exists-p (niwa-memory-store--pin workspace pass (plist-get digest :id))))
    (let ((evidence (niwa-memory-decision-rejections workspace)))
      (should (string-match-p "Abandoned choice" evidence))
      (should (string-match-p "PRIVATE PROPOSED TOPIC" evidence))
      (should (<= (string-bytes evidence) 8192)))
    (should (= 3 (length (niwa-journal-store-entries root)))))
  :doc "publication repairs different review bytes and releases pins only after authenticated terminal acceptance"
  (let* ((file (file-name-concat (niwa-journal-store-directory root) (plist-get review :file)))
         (text (niwa-session-control-fs-read-file file))
         (pin (niwa-memory-store--pin workspace pass (plist-get digest :id)))
         (publish (symbol-function 'niwa-memory-decision--publish)) injected)
    (cl-letf (((symbol-function 'niwa-memory-decision--publish)
               (lambda (&rest args)
                 (setq injected t)
                 (should (file-exists-p pin))
                 (write-region (concat text "\nDamaged review bytes\n") nil file nil 'silent)
                 (apply publish args))))
      (should (eq 'rejected (plist-get (niwa-memory-decision-reject
                                       workspace pass (plist-get proposal :id)) :status))))
    (should injected)
    (should (equal text (niwa-session-control-fs-read-file file)))
    (should-not (file-exists-p pin)))
  :doc "review repair refuses a second byte change and retains the accepted decision and digest pin"
  (let* ((file (file-name-concat (niwa-journal-store-directory root) (plist-get review :file)))
         (pin (niwa-memory-store--pin workspace pass (plist-get digest :id)))
         (publish (symbol-function 'niwa-memory-decision--publish))
         (program (symbol-function 'niwa-session-control-fs-run-program))
         damaged raced)
    (cl-letf (((symbol-function 'niwa-memory-decision--publish)
               (lambda (&rest args)
                 (setq damaged t)
                 (write-region "Damaged projection" nil file nil 'silent)
                 (apply publish args)))
              ((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &rest args)
                 (when (and damaged (seq-some (lambda (op)
                                   (and (eq 'verify-hash (plist-get op :op))
                                        (equal file (plist-get op :path)))) operations))
                   (setq raced t)
                   (write-region "Intervening projection" nil file nil 'silent))
                 (apply program operations args))))
      (should-error (niwa-memory-decision-reject workspace pass (plist-get proposal :id))))
    (should damaged)
    (should raced)
    (should (equal "Intervening projection" (niwa-session-control-fs-read-file file)))
    (should (file-exists-p pin))
    (should (= 1 (length (niwa-memory-decision--slots workspace (plist-get proposal :id))))))
  :doc "review repair refuses a fenced mutation owner before overwriting the projection"
  (let* ((file (file-name-concat (niwa-journal-store-directory root) (plist-get review :file)))
         (pin (niwa-memory-store--pin workspace pass (plist-get digest :id)))
         (publish (symbol-function 'niwa-memory-decision--publish))
         (program (symbol-function 'niwa-session-control-fs-run-program))
         damaged fenced)
    (cl-letf (((symbol-function 'niwa-memory-decision--publish)
               (lambda (&rest args)
                 (setq damaged t)
                 (write-region "Damaged projection" nil file nil 'silent)
                 (apply publish args)))
              ((symbol-function 'niwa-session-control-fs-run-program)
               (lambda (operations &optional lock)
                 (when (and damaged lock (seq-some (lambda (op)
                                  (and (eq 'verify-hash (plist-get op :op))
                                       (equal file (plist-get op :path)))) operations))
                   (setq fenced (niwa-journal-claim-settle
                                 (niwa-journal-claim-current lock) 'cancelled "")))
                 (funcall program operations lock))))
      (should-error (niwa-memory-decision-reject workspace pass (plist-get proposal :id))))
    (should fenced)
    (should (equal "Damaged projection" (niwa-session-control-fs-read-file file)))
    (should (file-exists-p pin)))
  :doc "completed rejection releases evidence for ordinary journal expiry"
  (let ((niwa-journal-max-age-days 0))
    (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
    (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :decision)
    (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
    (niwa-memory-decision-recover workspace)
    (should (eq 'rejected (plist-get (niwa-memory-decision-status workspace (plist-get proposal :id)) :status))))
  :doc "recent rejection reasons and original suggestions reach the next bounded request"
  (let* ((gptel--known-backends nil)
         (model (make-symbol "rejection-test-model"))
         (backend (gptel-make-openai "rejection-test" :key "test-only" :models (list model)))
         (real-request (symbol-function 'gptel-request)) sent)
    (put model :context-window 128)
    (put model :capabilities '(tool-use))
    (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id) :reason "Abandoned-choice-sentinel \u2013 \u754c") :decision)
    (cl-letf (((symbol-function 'niwa-model-resolve-workload) (lambda (&rest _) (list :backend backend :model model)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest args)
                 (unless (plist-get args :dry-run) (setq sent prompt))
                 (apply real-request prompt (plist-put (copy-sequence args) :dry-run t)))))
      (unwind-protect
          (progn
            (niwa-memory-pass-start workspace #'ignore :memory-only t)
            (let ((deadline (+ (float-time) 10)))
              (while (and (not sent) (< (float-time) deadline))
                (accept-process-output nil .01)))
            (should (string-match-p "Abandoned-choice-sentinel" sent))
            (should (string-match-p "PRIVATE PROPOSED TOPIC" sent)))
        (niwa-memory-pass-cancel workspace))))
  :doc "an oversized rejected suggestion is omitted whole with a bounded omission count"
  (progn
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (let* ((prepared (niwa-memory-store-prepare workspace claim scope nil ""))
           (large (niwa-memory-store-accept workspace prepared
                                              (string-replace "PRIVATE PROPOSED TOPIC" (make-string 9000 ?x) reply)
                                              nil "test:model" nil))
           (item (car (plist-get large :proposals))))
      (niwa-memory-store-publish workspace (plist-get prepared :id))
      (plist-get (niwa-memory-decision-reject workspace (plist-get prepared :id) (plist-get item :id)) :decision)
      (let* ((text (niwa-memory-decision-rejections workspace)) (data (json-parse-string text)))
        (should (<= (string-bytes text) 8192))
        (should (= 1 (gethash "omitted" data)))
        (should (= 0 (length (gethash "rejections" data)))))))
  :doc "one pending proposal retains shared pass evidence, unsettled and unrepublished, until the last rejection"
  (progn
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (let* ((extra (format (concat "## Promote\n```proposal\nroot: %S\nfile: \"new.md\"\ntype: \"project\"\n"
                                 "title: \"New\"\nhook: \"Context\"\nreason: \"Evidence\"\nevidence: []\n---\nNew context.\n```") root-id))
           (prepared (niwa-memory-store-prepare workspace claim scope (list digest) ""))
           (batch (niwa-memory-store-accept workspace prepared (string-replace "## Promote\n- none" extra reply)
                                              (list digest) "test:model" nil))
           (id (plist-get prepared :id))
           (items (plist-get batch :proposals))
           (pin (niwa-memory-store--pin workspace id (plist-get digest :id))))
      (should (= 2 (length items)))
      (niwa-memory-store-publish workspace id)
      (let ((program (symbol-function 'niwa-session-control-fs-run-program)) (calls 0))
        (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (&rest args) (cl-incf calls) (apply program args))))
          (should-not (niwa-memory-decision--settled-p workspace batch)))
        (should (= calls 3)))
      (plist-get (niwa-memory-decision-reject workspace id (plist-get (car items) :id)) :decision)
      (should (file-exists-p pin))
      (let ((published (symbol-function 'niwa-memory-decision--published))
            (program (symbol-function 'niwa-session-control-fs-run-program))
            (reads 0) (calls 0))
        (cl-letf (((symbol-function 'niwa-memory-decision--published)
                   (lambda (&rest args) (cl-incf reads) (apply published args)))
                  ((symbol-function 'niwa-session-control-fs-run-program)
                   (lambda (&rest args) (cl-incf calls) (apply program args))))
          (should-not (niwa-memory-decision--settled-p workspace batch)))
        ;; An incomplete public index cannot authorize pin release.  Do not
        ;; reauthenticate its growing prefix on every proposal publication.
        (should (= reads 0))
        (should (= calls 3)))
      (let ((publish (symbol-function 'niwa-memory-decision--publish)) (calls 0))
        (cl-letf (((symbol-function 'niwa-memory-decision--publish)
                   (lambda (&rest args) (cl-incf calls) (apply publish args))))
          (niwa-memory-decision-recover workspace))
        (should (= 0 calls)))
      (plist-get (niwa-memory-decision-reject workspace id (plist-get (cadr items) :id)) :decision)
      (let ((published (symbol-function 'niwa-memory-decision--published)) (reads 0))
        (cl-letf (((symbol-function 'niwa-memory-decision--published)
                   (lambda (&rest args) (cl-incf reads) (apply published args))))
          (should (niwa-memory-decision--settled-p workspace batch)))
        ;; A complete public index is only a hint: full immutable acceptance
        ;; and write evidence must still authenticate every proposal.
        (should (= reads 2)))
      (should-not (file-exists-p pin))
      (should (file-exists-p (niwa-memory-store--pin workspace pass (plist-get digest :id))))
      (let* ((status (niwa-memory-decision-status workspace (plist-get (car items) :id)))
             (path (file-name-concat (niwa-memory-decision--directory workspace)
                                     (concat (plist-get status :decision-id) ".el"))))
        ;; A complete public index cannot hide changed private acceptance bytes.
        (write-region "(:changed t)\n" nil path nil 'silent)
        (should-error (niwa-memory-decision--settled-p workspace batch)))))
  :doc "a decision waits for this client's active cleanup to release ownership"
  (let* ((niwa-journal-worker--child-p t)
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         (niwa-journal-cleanup--active (make-hash-table :test #'equal))
         (iterator (niwa-journal-cleanup--steps workspace t))
         (job (cons nil nil)) timer)
    (unwind-protect
        (progn
          (dotimes (_ 5) (iter-next iterator))
          (should-not (niwa-journal-claim-acquire
                       (niwa-memory-store--claim-directory workspace) 180))
          (puthash root job niwa-journal-cleanup--pending)
          ;; This iterator owns real cleanup claims; represent admitted IO,
          ;; not merely a queued job, at the decision's waiting boundary.
          (puthash root job niwa-journal-cleanup--active)
          (setq timer (run-at-time .05 nil
                                   (lambda ()
                                     (iter-close iterator)
                                     (remhash root niwa-journal-cleanup--active)
                                     (remhash root niwa-journal-cleanup--pending))))
          (niwa-test--with-captured-messages nil
            (should (eq 'rejected
                        (plist-get (niwa-memory-decision-reject
                                    workspace pass (plist-get proposal :id)) :status)))))
      (when timer (cancel-timer timer))
      (iter-close iterator)))
  :doc "unknown proposals cannot create slots and a decision does not settle a live pass claim"
  (progn
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (should-error (niwa-memory-decision-reject workspace pass (make-string 64 ?f)))
    (should (equal (list review digest) (niwa-journal-store-entries root)))
    (should (eq 'rejected (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :status)))
    (should-not (niwa-journal-claim-outcome claim)))
  :doc "an accepted rejection recovers after the owner stops before public publication"
  (progn
    (cl-letf (((symbol-function 'niwa-memory-decision--publish) (lambda (&rest _) (error "Publication stopped"))))
      (should-error (niwa-memory-decision-reject workspace pass (plist-get proposal :id) :reason "Superseded")))
    (should (equal (list review digest) (niwa-journal-store-entries root)))
    (niwa-memory-decision-recover workspace)
    (let ((decision (niwa-memory-decision-status workspace (plist-get proposal :id))))
      (should (eq 'rejected (plist-get decision :status)))
      (should (equal "Superseded" (plist-get decision :reason)))
      (niwa-memory-decision-recover workspace)
      (should (equal decision (niwa-memory-decision-status workspace (plist-get proposal :id))))
      (should (= 3 (length (niwa-journal-store-entries root))))
      ;; Recovery must also repair missing review publication or interrupted
      ;; pin release when the decision itself is already public.
      (delete-file (file-name-concat (niwa-journal-store-directory root) (plist-get review :file)))
      (should-error (niwa-memory-decision-recover workspace)
                    :type 'niwa-session-control-fs-absent)
      (let ((pin (niwa-memory-store--pin workspace pass (plist-get digest :id))))
        (make-directory (file-name-directory pin) t)
        (write-region (plist-get (plist-get accepted :prepared) :hash) nil pin nil 'silent)
        (niwa-memory-decision-recover workspace)
        (should (= 3 (length (niwa-journal-store-entries root))))
        (should-not (file-exists-p pin))
        (write-region (plist-get (plist-get accepted :prepared) :hash) nil pin nil 'silent)
        (niwa-memory-decision-recover workspace)
        (should-not (file-exists-p pin)))))
  :doc "a public decision without immutable acceptance cannot decide a pending proposal"
  (progn
    (niwa-journal-store-publish-decision
     root (list :decision-id (make-string 64 ?e) :pass-id pass :proposal-id (plist-get proposal :id)
                :workspace identity :created "2026-09-07T12:00:00Z" :status 'rejected
                :reason "Forged rejection" :state-hash (make-string 64 ?f)))
    (should-not (niwa-memory-decision-status workspace (plist-get proposal :id)))
    (should-error (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :decision))
    (should (file-exists-p (niwa-memory-store--pin workspace pass (plist-get digest :id))))
    (let ((evidence (niwa-memory-decision-rejections workspace)))
      (should-not (string-match-p "Forged rejection" evidence))
      (should (= 1 (gethash "omitted" (json-parse-string evidence))))))
  :doc "a public decision must match the private slot before evidence pins can be released"
  (let* ((id (plist-get proposal :id))
         (pin (niwa-memory-store--pin workspace pass (plist-get digest :id)))
         (mutation (niwa-journal-claim-acquire
                    (niwa-journal-store-claim-directory
                     (niwa-journal-store-directory root) 'mutation) 180))
         decision)
    (unwind-protect
        (setq decision (plist-get (niwa-memory-decision-reject workspace pass id :reason "Original reason") :decision))
      (niwa-journal-claim-settle mutation 'cancelled ""))
    (should (file-exists-p pin))
    (niwa-journal-store-publish-decision root (plist-put (copy-sequence decision) :reason "Changed reason"))
    (should-error (niwa-memory-decision--settled-p workspace accepted))
    (should (file-exists-p pin)))
  :doc "changed accepted before-state cannot authorize a rejection"
  (let* ((path (file-name-concat (niwa-memory-store--directory workspace pass) "prepared.el"))
         (original (niwa-session-control-fs-read-file path)))
    ;; Changing original scope bytes invalidates its accepted bundle fingerprint.
    (write-region (concat original "\n") nil path nil 'silent)
    (should-error (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :decision))
    (should (equal (list review digest) (niwa-journal-store-entries root))))
  :doc "each rejection-evidence observation revalidates the accepted private bundle"
  (let ((path (file-name-concat (niwa-memory-store--directory workspace pass) "accepted.el")))
    (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :decision)
    (should (string-match-p "PRIVATE PROPOSED TOPIC" (niwa-memory-decision-rejections workspace)))
    (write-region "\n" nil path t 'silent)
    (let* ((text (niwa-memory-decision-rejections workspace)) (data (json-parse-string text)))
      (should-not (string-match-p "PRIVATE PROPOSED TOPIC" text))
      (should (= 1 (gethash "omitted" data)))
      (should (= 0 (length (gethash "rejections" data))))))
  :doc "retargeting the configured memory root makes rejection unavailable"
  (let ((elsewhere (file-name-concat root "elsewhere")))
    (rename-file memory elsewhere)
    (make-directory memory t)
    (rename-file memory (concat memory "-empty"))
    (make-symbolic-link (concat memory "-empty") memory)
    (should-error (plist-get (niwa-memory-decision-reject workspace pass (plist-get proposal :id)) :decision))
    (should (equal (list review digest) (niwa-journal-store-entries root)))
    (should (equal "PRIVATE ORIGINAL TOPIC\n"
                   (niwa-session-control-fs-read-file (file-name-concat elsewhere "topic.md"))))))

(niwa-deftest niwa-memory-decision-schedule-recovery
    (:vars* ((root (make-temp-file "niwa-memory-activation-" t))
             (workspace (niwa-workspace--create :root root))
             (niwa-memory-decision--inhibit-recovery nil) claim)
     :after-each ((niwa-memory-decision-stop-recovery)
                  (when claim (niwa-journal-claim-settle claim 'cancelled ""))
                  (delete-directory root t)))
  (cl-labels ((drain ()
                (let ((deadline (+ (float-time) 5)))
                  (while (and (or (> (hash-table-count niwa-memory-decision--recovery-pending) 0)
                                 (> (hash-table-count niwa-memory-decision--recovery-workers) 0))
                              (< (float-time) deadline))
                    (accept-process-output nil 0.01)))
                (should (= 0 (hash-table-count niwa-memory-decision--recovery-pending)))))
    ,test)
  (test)
  :doc "activation of a new workspace creates no memory state"
  (progn
    (niwa-memory-decision-schedule-recovery workspace)
    (drain)
    (should-not (file-exists-p (file-name-concat root ".niwa"))))
  :doc "activation leaves a live owner untouched without diagnostics"
  (let (diagnostics)
    (setq claim (niwa-journal-claim-acquire (niwa-memory-store--claim-directory workspace) 180))
    (niwa-test--with-captured-diagnostics diagnostics
      (niwa-memory-decision-schedule-recovery workspace)
      (drain))
    (should (string-empty-p diagnostics))
    (should-not (niwa-journal-claim-outcome claim))
    (should (equal claim (niwa-journal-claim-current (plist-get claim :directory)))))
  :doc "activation waits for transport and cleans up a cancelled deferred callback"
  (let ((key (list 'memory-recovery (niwa-memory-store--claim-directory workspace))))
    ;; Let the initial opportunity reach the busy transport queue; a timer
    ;; created inside a handler is held until that handler unwinds instead.
    (niwa-memory-decision-schedule-recovery workspace)
    (let ((niwa-transport--depth 1))
      (let ((deadline (+ (float-time) 2)))
        (while (and (not (gethash key niwa-transport--pending)) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (gethash key niwa-transport--pending))
      (should (= 1 (hash-table-count niwa-memory-decision--recovery-pending)))
      (niwa-transport-cancel-pending key)
      (should (= 0 (hash-table-count niwa-memory-decision--recovery-pending))))
    (niwa-memory-decision-schedule-recovery workspace)
    (drain)
    (should-not (file-exists-p (file-name-concat root ".niwa")))))

(niwa-deftest niwa-memory-decision-stop-recovery ()
  ,test
  (test)
  :doc "exit cancels queued memory recovery without creating state or accepting late timers"
  (let* ((root (make-temp-file "niwa-memory-exit-" t))
         (workspace (niwa-workspace--create :root root))
         (niwa-memory-decision--inhibit-recovery nil))
    (unwind-protect
        (progn
          (niwa-memory-decision-schedule-recovery workspace)
          (let ((timer (gethash (niwa-memory-store--claim-directory workspace)
                                niwa-memory-decision--recovery-pending)))
            (should (timerp timer))
            (niwa-session-persistence--kill-emacs-hook)
            (should-not (memq timer timer-list))
            (niwa-memory-decision-schedule-recovery workspace)
            (let ((successor (gethash (niwa-memory-store--claim-directory workspace)
                                     niwa-memory-decision--recovery-pending)))
              (funcall (timer--function timer))
              (should (eq successor (gethash (niwa-memory-store--claim-directory workspace)
                                             niwa-memory-decision--recovery-pending))))
            (niwa-memory-decision-stop-recovery))
          (should (= 0 (hash-table-count niwa-memory-decision--recovery-pending)))
          (should-not (file-exists-p (file-name-concat root ".niwa"))))
      (niwa-memory-decision-stop-recovery)
      (delete-directory root t))))

(niwa-deftest niwa-memory-decision-terminal-status-p
  (:doc "ends a proposal's lifecycle only for applied, rejected, and reversed")
  (progn
    (dolist (status '(applied rejected reversed)) (should (niwa-memory-decision-terminal-status-p status)))
    (dolist (status '(pending stale unavailable nil))
      (should-not (niwa-memory-decision-terminal-status-p status)))))

(niwa-deftest niwa-memory-decision-actionable-status-p
  (:doc "awaits a decision only while pending or stale")
  (progn
    (dolist (status '(pending stale)) (should (niwa-memory-decision-actionable-status-p status)))
    (dolist (status '(applied rejected reversed unavailable))
      (should-not (niwa-memory-decision-actionable-status-p status)))))

(niwa-deftest niwa-memory-decision-written-status-p
  (:doc "names decisions whose write reached the target")
  (progn
    (dolist (status '(applied reversed)) (should (niwa-memory-decision-written-status-p status)))
    (dolist (status '(rejected stale unavailable))
      (should-not (niwa-memory-decision-written-status-p status)))))

(provide 'test-niwa-memory-decision)
;;; test-niwa-memory-decision.el ends here
