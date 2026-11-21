;;; test-niwa-journal-cleanup.el -- Journal expiry tests -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises real target storage, immutable deletion election, and recovery.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-journal-cleanup)
(require 'niwa-journal-index)
(require 'niwa-journal-pins)
(require 'niwa-session-control-fs)
(require 'niwa-session-persistence)

(defun niwa-test-journal-cleanup--entry (root name &optional fresh)
  "Publish a digest named NAME under ROOT, old and reviewed unless FRESH."
  (let ((entry
         (niwa-journal-store-publish-digest
          root
          (list :capture-id (secure-hash 'sha256 name) :session name :session-name name
                :workspace (make-string 64 ?b) :trigger 'session-end :segment 1
                :source-revision (make-string 64 ?c) :turns '(1)
                :turn-ids (list (secure-hash 'sha256 (concat name "-turn")))
                :created (if fresh (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                           "2000-01-01T00:00:00Z")
                :model "provider:model")
          "## Done\n- Observed: completed (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")))
    (unless fresh
      (niwa-journal-store-publish-review
       root (list :pass-id (secure-hash 'sha256 (concat "review-" name))
                  :workspace (plist-get entry :workspace)
                  :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                  :model "test:model" :focus "" :digests (list (plist-get entry :id))
                  :proposals nil :references nil)))
    entry))

(defun niwa-test-journal-cleanup--digests (root)
  "Return the remaining digest records in ROOT, excluding review fixtures."
  (seq-filter (lambda (entry) (eq 'digest (plist-get entry :kind)))
              (niwa-journal-store-entries root)))

(niwa-deftest niwa-journal-cleanup-expired ()
  ,test
  (test)
  :doc "old review coverage survives expiry while its digest remains"
  (let* ((root (make-temp-file "niwa-review-retention-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (let* ((digest (niwa-test-journal-cleanup--entry root "surviving" t))
               (review (niwa-journal-store-publish-review
                        root (list :pass-id (make-string 64 ?e) :workspace (make-string 64 ?b)
                                   :created "2000-01-01T00:00:00Z" :model "provider:model"
                                   :focus "" :digests (list (plist-get digest :id))
                                   :proposals nil :references nil))))
          (let ((pins (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) "evidence-pins" (plist-get digest :id))))
            (make-directory pins t)
            (write-region "Another review still needs this note" nil (file-name-concat pins "review.pin") nil 'silent))
          (niwa-test-journal-cleanup--entry root "expired")
          (niwa-journal-cleanup-expired workspace)
          (should (equal review (niwa-journal-store-read root (plist-get review :file))))
          (let ((entries (niwa-journal-store-entries root)))
            (should (= 3 (length entries)))
            (should-not (niwa-journal-index-unreviewed entries))))
      (delete-directory root t)))

  :doc "project journal expiry runs independently of disabled session cleanup and is throttled"
  (let* ((root (make-temp-file "niwa-journal-cleanup-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root))
         (niwa-journal-max-age-days 365)
         (niwa-session-max-age-days nil))
    (unwind-protect
        (let ((old (niwa-test-journal-cleanup--entry root "old"))
              (fresh (niwa-test-journal-cleanup--entry root "fresh" t)))
          (niwa-session-persistence-cleanup-expired workspace)
          (niwa-journal-cleanup-expired workspace)
          (should (equal (list fresh) (niwa-test-journal-cleanup--digests root)))
          (should-not (file-exists-p (file-name-concat (niwa-journal-store-directory root) (plist-get old :file))))
          (should (member (car (plist-get old :turn-ids)) (niwa-journal-store-covered-turns root)))
          (should-error (niwa-journal-store-read root (plist-get old :file)))
          (should-error (niwa-test-journal-cleanup--entry root "old"))
          (niwa-test-journal-cleanup--entry root "another-old")
          (should-not (niwa-journal-cleanup-expired workspace))
          (should (= 2 (length (niwa-test-journal-cleanup--digests root))))
          (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count))))
      (delete-directory root t)))

  :doc "recovery and fresh expiry share the fifty-group batch limit"
  (let* ((root (make-temp-file "niwa-journal-cleanup-batch-" t))
         (workspace (niwa-workspace--create :root root))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (progn
          (dotimes (n 51) (niwa-test-journal-cleanup--entry root (format "old-%d" n)))
          ;; A written but unaccepted full manifest must not consume later
          ;; recovery budgets or prevent the next attempt from making progress.
          (let ((settle (symbol-function 'niwa-journal-claim-settle)))
            (cl-letf (((symbol-function 'niwa-journal-claim-settle)
                       (lambda (claim status payload &optional owner)
                         (unless (and (eq status 'completed) (not (string-empty-p payload)))
                           (funcall settle claim status payload owner)))))
              (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace) :count)))))
          (niwa-test--with-captured-messages nil
            (cl-letf (((symbol-function 'niwa-journal-cleanup--apply)
                       (lambda (&rest _) (error "Interrupted before deletion"))))
              (should-not (niwa-journal-cleanup-expired workspace t))))
          (should (equal '(:count 50 :more t) (niwa-journal-cleanup-expired workspace t)))
          (should (= 1 (length (niwa-test-journal-cleanup--digests root))))
          (should-not (niwa-journal-cleanup-expired workspace))
          (should (= 51 (length (niwa-journal-store-covered-turns root))))
          (should (equal '(:count 1 :more nil) (niwa-journal-cleanup-expired workspace t))))
      (delete-directory root t)))

  :doc "preserves pending capture pins and review evidence while collecting retired payloads"
  (let* ((root (make-temp-file "niwa-journal-cleanup-" t))
         (workspace (niwa-workspace--create :root root))
         (directory (niwa-journal-store-directory root))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (let* ((pending (niwa-test-journal-cleanup--entry root "pending"))
               (pinned (niwa-test-journal-cleanup--entry root "pinned"))
               (retired (niwa-test-journal-cleanup--entry root "retired"))
               (source (file-name-concat root "source-session"))
               (capture (file-name-concat (niwa-journal-store-state-directory directory) "captures" (plist-get retired :capture-id)))
               (pin-directory (file-name-concat (niwa-journal-store-state-directory directory) "evidence-pins" (plist-get pinned :id))))
          (make-directory (file-name-concat (niwa-journal-store-state-directory directory) "captures" (plist-get pending :capture-id)) t)
          (niwa-journal-pins-retain source (plist-get pending :capture-id) nil)
          (make-directory pin-directory t)
          (write-region "pending proposal evidence" nil (file-name-concat pin-directory "proposal.json") nil 'silent)
          (make-directory capture t)
          (write-region "published" nil (file-name-concat capture "retired") nil 'silent)
          (write-region "retired accepted payload" nil (file-name-concat capture "capture.json") nil 'silent)
          (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace) :count)))
          (should (= 2 (length (niwa-test-journal-cleanup--digests root))))
          (should (niwa-journal-pins-present-p source))
          (should (file-exists-p pin-directory))
          (should-not (file-exists-p capture)))
      (delete-directory root t)))

  :doc "remote project expiry shares the same target-native rules without session expiry"
  (let ((root (make-temp-file "niwa-journal-cleanup-remote-" t))
        (niwa-journal-max-age-days 365)
        (niwa-session-max-age-days nil))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("journal-expiry")
          (let* ((remote (format "/niwamock:journal-expiry:%s/" root))
                 (workspace (niwa-workspace--create :type 'project :id remote :root remote))
                 (entry (niwa-test-journal-cleanup--entry remote "remote-old")))
            (niwa-session-persistence-cleanup-expired workspace)
          (niwa-journal-cleanup-expired workspace)
            (should-not (niwa-test-journal-cleanup--digests remote))
            (should (equal (plist-get entry :turn-ids) (niwa-journal-store-covered-turns remote)))))
      (delete-directory root t)))

  :doc "disabled expiry creates no state and live mutation or digest owners postpone deletion"
  (let* ((root (make-temp-file "niwa-journal-cleanup-" t))
         (workspace (niwa-workspace--create :root root)))
    (unwind-protect
        (progn
          (let ((niwa-journal-max-age-days nil))
            (should-not (niwa-journal-cleanup-expired workspace)))
          (should-not (file-exists-p (file-name-concat root ".niwa")))
          (dolist (scope '("mutation" "digest-run"))
            (let* ((entry (niwa-test-journal-cleanup--entry root scope))
                   (claim (niwa-journal-claim-acquire
                           (file-name-concat (niwa-journal-store-state-directory (niwa-journal-store-directory root)) scope) 120))
                   (niwa-journal-max-age-days 365))
              (should-not (niwa-journal-cleanup-expired workspace t))
              (should (niwa-journal-store-read root (plist-get entry :file)))
              (niwa-journal-claim-settle claim 'completed "")
              (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count))))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup--owned/capture-probes ()
  ,test
  (test)
  :doc "batches capture probes but never lists a pending capture's evidence directory"
  (niwa-test--with-local-shell-tramp '("journal-pin-check")
    (dolist (prefix '("" "/niwamock:journal-pin-check:"))
      (let* ((local (make-temp-file "niwa-cleanup-pin-link-" t))
             (root (concat prefix local))
             (workspace (niwa-workspace--create :root root))
             (directory (niwa-journal-store-directory root))
             (state (niwa-journal-store-state-directory directory))
             (pins (file-name-concat state "evidence-pins"))
             (niwa-journal-max-age-days 365)
             (run (symbol-function 'niwa-session-control-fs-run-program))
             captures probes listings claim)
        (unwind-protect
            (progn
              (dolist (name '("first" "second"))
                (let ((entry (niwa-test-journal-cleanup--entry root name)))
                  (push (file-name-concat state "captures" (plist-get entry :capture-id)) captures)
                  (make-directory (car captures) t)))
              (make-directory (file-name-concat local "pin-target"))
              (make-symbolic-link (file-name-concat local "pin-target") (file-local-name pins))
              (setq claim (niwa-journal-claim-acquire (niwa-journal-store-claim-directory directory 'mutation) 120))
              (niwa-session-control-fs-call-with-locks
               (list (plist-get claim :directory))
               (lambda (owner)
                 (cl-letf (((symbol-function 'niwa-session-control-fs-run-program)
                            (lambda (ops &optional lock)
                              (when (and (eq (plist-get (car ops) :op) 'path-exists-p)
                                         (member (plist-get (car ops) :path) captures))
                                (push (length ops) probes))
                              (dolist (op ops)
                                (when (and (eq 'list-directory (plist-get op :op))
                                           (string-prefix-p (file-name-as-directory pins) (plist-get op :path)))
                                  (push (plist-get op :path) listings)))
                              (funcall run ops lock))))
                   (should (equal '(:count 0 :more nil) (niwa-journal-cleanup--owned workspace claim 2 owner)))
                   (should (equal probes '(4)))
                   (should-not listings)
                   (setq probes nil)
                   (should (equal '(:count 0 :more nil) (niwa-journal-cleanup--owned workspace claim 1 owner)))
                   (should (equal probes '(2 2)))
                   (write-region "retired" nil (file-name-concat (car captures) "retired") nil 'silent)
                   (should-error (niwa-journal-cleanup--owned workspace claim 2 owner) :type 'file-error)
                   (should (= 1 (length listings)))
                   (should (= 2 (length (niwa-test-journal-cleanup--digests root))))
                   (delete-file pins)
                   ;; Isolate a capture-probe failure: no evidence pins remain.
                   (let* ((capture (car captures))
                          (physical (file-local-name capture))
                          (saved (concat physical ".saved"))
                          (before (niwa-test-journal-cleanup--digests root)))
                     (rename-file physical saved)
                     (make-symbolic-link saved physical)
                     (setq listings nil)
                     (should-error (niwa-journal-cleanup--owned workspace claim 2 owner) :type 'file-error)
                     (should (equal before (niwa-test-journal-cleanup--digests root)))
                     (should-not listings)
                     (delete-file physical)
                     (rename-file saved physical))
                   (should (= 1 (plist-get (niwa-journal-cleanup--owned workspace claim 2 owner) :count)))))))
          (when claim (niwa-journal-claim-settle claim 'completed ""))
          (delete-directory local t))))))

(niwa-deftest niwa-journal-cleanup--owned ()
  ,test
  (test)
  :doc "one workspace's continuation does not leak into another workspace's short batch"
  (let* ((root-a (make-temp-file "niwa-cleanup-budget-a-" t))
         (root-b (make-temp-file "niwa-cleanup-budget-b-" t))
         (workspace-a (niwa-workspace--create :root root-a))
         (workspace-b (niwa-workspace--create :root root-b))
         (niwa-journal-max-age-days 365)
         claim-a claim-b)
    (unwind-protect
        (progn
          (niwa-test-journal-cleanup--entry root-a "first")
          (niwa-test-journal-cleanup--entry root-a "second")
          (setq claim-a (niwa-journal-claim-acquire
                         (niwa-journal-store-claim-directory (niwa-journal-store-directory root-a) 'mutation) 120)
                claim-b (niwa-journal-claim-acquire
                         (niwa-journal-store-claim-directory (niwa-journal-store-directory root-b) 'mutation) 120))
          ;; A's budget of one group is exhausted with work left over.
          (let ((result-a (niwa-session-control-fs-call-with-locks
                           (list (plist-get claim-a :directory)) (lambda (owner) (niwa-journal-cleanup--owned workspace-a claim-a 1 owner))))
                (result-b (niwa-session-control-fs-call-with-locks
                           (list (plist-get claim-b :directory)) (lambda (owner) (niwa-journal-cleanup--owned workspace-b claim-b 50 owner)))))
            (should (equal '(:count 1 :more t) result-a))
            (should (equal '(:count 0 :more nil) result-b))))
      (when claim-a (niwa-journal-claim-settle claim-a 'completed ""))
      (when claim-b (niwa-journal-claim-settle claim-b 'completed ""))
      (delete-directory root-a t)
      (delete-directory root-b t))))

(niwa-deftest niwa-journal-cleanup-recover ()
  ,test
  (test)
  :doc "replays an accepted expiry after interrupted deletion, retaining first publication coverage"
  (let* ((root (make-temp-file "niwa-journal-expiry-recover-" t))
         (workspace (niwa-workspace--create :root root))
         (entry (niwa-test-journal-cleanup--entry root "recover"))
         (directory (niwa-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (delete-file-fn (symbol-function 'niwa-session-control-fs-delete-file))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (progn
          (niwa-test--with-captured-messages nil
            (cl-letf (((symbol-function 'niwa-session-control-fs-delete-file)
                       (lambda (target)
                         (if (equal target path) (error "Injected interruption")
                           (funcall delete-file-fn target)))))
              (should-not (niwa-journal-cleanup-expired workspace))))
          (should (file-exists-p path))
          (should-not (niwa-test-journal-cleanup--digests root))
          (let* ((claim (niwa-journal-claim-current (file-name-concat (niwa-journal-store-state-directory directory) "mutation")))
                 (manifest (niwa-journal-cleanup--read
                            directory (file-name-concat (niwa-journal-store-state-directory directory) "expiry"
                                                        (concat (plist-get claim :owner) ".json")))))
            (cl-letf (((symbol-function 'niwa-session-control-fs-target-time)
                       (lambda (_path) (1+ (plist-get claim :expires-at)))))
              (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count))))
            (should (= 0 (niwa-journal-cleanup--apply directory manifest))))
          (should-not (file-exists-p path))
          (should (= 0 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
          (should (equal (plist-get entry :turn-ids) (niwa-journal-store-covered-turns root))))
      (delete-directory root t)))
  :doc "counts only manifests it finishes now, so a steady-state recovery reports none"
  (let* ((root (make-temp-file "niwa-journal-expiry-finished-" t))
         (workspace (niwa-workspace--create :root root))
         (entry (niwa-test-journal-cleanup--entry root "finished"))
         (directory (niwa-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (delete-file-fn (symbol-function 'niwa-session-control-fs-delete-file))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (progn
          (niwa-test--with-captured-messages nil
            (cl-letf (((symbol-function 'niwa-session-control-fs-delete-file)
                       (lambda (target)
                         (if (equal target path) (error "Injected interruption")
                           (funcall delete-file-fn target)))))
              (should-not (niwa-journal-cleanup-expired workspace))))
          ;; The interrupted manifest still finishes even if its public file is gone.
          (delete-file path)
          (should (= 1 (niwa-journal-cleanup-recover directory)))
          (should (= 0 (niwa-journal-cleanup-recover directory))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup--apply ()
  ,test
  (test)
  :doc "an expired unaccepted manifest cannot delete after a successor wins"
  (let* ((root (make-temp-file "niwa-journal-expiry-fence-" t))
         (workspace (niwa-workspace--create :root root))
         (entry (niwa-test-journal-cleanup--entry root "fenced"))
         (directory (niwa-journal-store-directory root))
         (scope (file-name-concat (niwa-journal-store-state-directory directory) "mutation"))
         (claim (niwa-journal-claim-acquire scope 120))
         (niwa-journal-max-age-days 365)
         successor)
    (unwind-protect
        (progn
          ;; Expire the synthetic native record so the final target-side
          ;; settlement guard observes the same deadline as the caller.
          (plist-put claim :expires-at 1)
          (niwa-session-control-fs-write-file
           (niwa-journal-claim--path claim nil)
           (json-serialize (niwa-journal-claim--record claim)))
          (should (= 0 (plist-get (niwa-session-control-fs-call-with-locks
                                   (list scope) (lambda (owner) (niwa-journal-cleanup--owned workspace claim 50 owner))) :count)))
          (should-not (niwa-journal-claim-outcome claim))
          (setq successor (niwa-journal-claim-acquire scope 120))
          (should successor)
          (let ((manifest (niwa-journal-cleanup--read
                           directory (file-name-concat (niwa-journal-store-state-directory directory) "expiry"
                                                       (concat (plist-get claim :owner) ".json")))))
            (should-not (niwa-journal-claim-settle claim 'completed (plist-get manifest :hash)))
            (should (= 0 (niwa-journal-cleanup--apply directory manifest)))
            (should (niwa-journal-store-read root (plist-get entry :file))))
          (niwa-journal-claim-settle successor 'completed "")
          (should (= 1 (plist-get (niwa-journal-cleanup-expired workspace t) :count)))
          (should-not (file-exists-p (file-name-concat (niwa-journal-store-state-directory directory) "expiry"
                                                     (concat (plist-get claim :owner) ".json")))))
      (delete-directory root t)))

  :doc "changed public bytes remain on disk after accepted expiry recovery"
  (let* ((root (make-temp-file "niwa-journal-expiry-changed-" t))
         (workspace (niwa-workspace--create :root root))
         (entry (niwa-test-journal-cleanup--entry root "changed"))
         (directory (niwa-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (delete-file-fn (symbol-function 'niwa-session-control-fs-delete-file))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (progn
          (niwa-test--with-captured-messages nil
            (cl-letf (((symbol-function 'niwa-session-control-fs-delete-file)
                       (lambda (target)
                         (if (equal target path) (error "Injected interruption")
                           (funcall delete-file-fn target)))))
              (niwa-journal-cleanup-expired workspace)))
          (write-region "intervening edit" nil path nil 'silent)
          (let (messages)
            (niwa-test--with-captured-messages messages
              (should-not (niwa-journal-cleanup-expired workspace t)))
            (should (string-match-p "changed; retained for inspection" messages)))
          (should (equal "intervening edit" (niwa-session-control-fs-read-file path))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup--read ()
  ,test
  (test)
  :doc "changed or malformed accepted manifests cannot authorize recovery deletion"
  (let* ((root (make-temp-file "niwa-journal-expiry-manifest-" t))
         (workspace (niwa-workspace--create :root root))
         (entry (niwa-test-journal-cleanup--entry root "manifest"))
         (directory (niwa-journal-store-directory root))
         (path (file-name-concat directory (plist-get entry :file)))
         (apply-fn (symbol-function 'niwa-journal-cleanup--apply))
         (niwa-journal-max-age-days 365))
    (unwind-protect
        (progn
          (niwa-test--with-captured-messages nil
            (cl-letf (((symbol-function 'niwa-journal-cleanup--apply)
                       (lambda (&rest _) (error "Injected interruption before application"))))
              (should-not (niwa-journal-cleanup-expired workspace))))
          (let* ((job (car (niwa-session-control-fs-list-directory
                           (file-name-concat (niwa-journal-store-state-directory directory) "expiry") "\\.json\\'")))
                 (object (json-parse-string (niwa-session-control-fs-read-file job))))
            (puthash "sha256" (make-string 64 ?0)
                     (aref (gethash "entries" object) 0))
            (write-region (json-serialize object) nil job nil 'silent)
            (should-error (funcall apply-fn directory (niwa-journal-cleanup--read directory job)))
            (should (file-exists-p path))
            (write-region "{}" nil job nil 'silent)
            (should-error (niwa-journal-cleanup--read directory job))
            (should (file-exists-p path))))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "coalesces idle requests and preserves an earlier forced cleanup"
  (let* ((workspace (niwa-workspace--create :root temporary-file-directory))
         (niwa-journal-cleanup--inhibit-scheduling nil)
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         first current calls)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-transport-run-when-idle)
                   (lambda (_key _root callback &optional _on-cancel) (funcall callback) t))
                  ((symbol-function 'niwa-journal-cleanup--steps)
                   (iter-lambda (owner force) (push (list owner force) calls))))
          (niwa-journal-cleanup-schedule workspace t)
          (setq first (car (gethash temporary-file-directory niwa-journal-cleanup--pending)))
          (niwa-journal-cleanup-schedule workspace)
          (setq current (car (gethash temporary-file-directory niwa-journal-cleanup--pending)))
          (should (eq first current))
          (should (memq current timer-idle-list))
          (should (= 1 (hash-table-count niwa-journal-cleanup--pending)))
          (cancel-timer current)
          (apply (timer--function current) (timer--args current))
          (should (equal (list (list workspace t)) calls))
          (should (= 0 (hash-table-count niwa-journal-cleanup--pending))))
      (when (timerp first) (cancel-timer first))
      (when (timerp current) (cancel-timer current)))))

(niwa-deftest niwa-journal-cleanup-schedule/responsiveness ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "services another event before claiming cleanup and retains fresh evidence"
  (let* ((root (make-temp-file "niwa-cleanup-responsive-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root))
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         timer input-ran claimed blocked
         (claim-probe (lambda (&rest _)
                        (setq claimed t)
                        (unless input-ran (setq blocked t)))))
    (unwind-protect
        (progn
          (niwa-test-journal-cleanup--entry root "fresh" t)
          (advice-add 'niwa-journal-claim-acquire :before claim-probe)
          (let ((niwa-journal-cleanup--inhibit-scheduling nil))
            (niwa-journal-cleanup-schedule workspace)
            (setq timer (run-at-time 0 nil (lambda () (setq input-ran t))))
            ;; Start the real idle opportunity without depending on batch
            ;; Emacs becoming interactively idle.
            (let ((start (car (gethash root niwa-journal-cleanup--pending))))
              (cancel-timer start)
              (apply (timer--function start) (timer--args start)))
            (let ((deadline (+ (float-time) 5)))
              (while (and (> (hash-table-count niwa-journal-cleanup--pending) 0)
                          (< (float-time) deadline))
                (sleep-for .002)))
            (should claimed)
            (should-not blocked)
            (should (= 0 (hash-table-count niwa-journal-cleanup--pending))))
          (should (= 1 (length (niwa-test-journal-cleanup--digests root)))))
      (when timer (cancel-timer timer))
      (advice-remove 'niwa-journal-claim-acquire claim-probe)
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-wait ()
  ,test
  (test)
  :doc "queued idle cleanup does not delay an interactive decision"
  (let* ((root (make-temp-file "niwa-cleanup-wait-" t))
         (workspace (niwa-workspace--create :root root))
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         (niwa-journal-cleanup--inhibit-scheduling nil)
         timer)
    (unwind-protect
        (progn
          (niwa-journal-cleanup-schedule workspace t)
          (setq timer (car (gethash root niwa-journal-cleanup--pending)))
          (niwa-journal-cleanup-wait workspace)
          (should (eq timer (car (gethash root niwa-journal-cleanup--pending))))
          (should (memq timer timer-idle-list)))
      (when timer (cancel-timer timer))
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup--steps ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "closing between phases settles every acquired claim"
  (let* ((root (make-temp-file "niwa-cleanup-cancel-" t))
         (workspace (niwa-workspace--create :type 'project :id root :root root))
         (directory (niwa-journal-store-directory root))
         (iterator (niwa-journal-cleanup--steps workspace t)))
    (unwind-protect
        (progn
          (niwa-test-journal-cleanup--entry root "retained" t)
          (dotimes (_ 5) (iter-next iterator))
          (dolist (kind '(mutation digest-run consolidation))
            (should (niwa-journal-claim-owned-p
                     (niwa-journal-claim-current
                      (niwa-journal-store-claim-directory directory kind)))))
          (iter-close iterator)
          (dolist (kind '(mutation digest-run consolidation))
            (should (eq 'completed
                        (plist-get
                         (niwa-journal-claim-outcome
                          (niwa-journal-claim-current
                           (niwa-journal-store-claim-directory directory kind)))
                         :status))))
          (should (= 1 (length (niwa-test-journal-cleanup--digests root)))))
      (iter-close iterator)
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule/ownership ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "fenced ownership between callbacks stops cleanup before deletion"
  (let* ((root (make-temp-file "niwa-cleanup-fenced-" t))
         (workspace (niwa-workspace--create :root root))
         (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
         taken timer diagnostics
         (fence (lambda (token)
                  (when (and token (not taken))
                    (setq taken token
                          timer (run-at-time 0 nil
                                             (lambda ()
                                               (niwa-journal-claim-settle token 'cancelled "fenced")))))
                  token)))
    (unwind-protect
        (progn
          (niwa-test-journal-cleanup--entry root "retained-after-fencing")
          (advice-add 'niwa-journal-claim-acquire :filter-return fence)
          (niwa-test--with-captured-diagnostics diagnostics
            (let ((niwa-journal-cleanup--inhibit-scheduling nil))
              (niwa-journal-cleanup-schedule workspace)
              (let ((start (car (gethash root niwa-journal-cleanup--pending))))
                (cancel-timer start)
                (apply (timer--function start) (timer--args start)))
              (let ((deadline (+ (float-time) 5)))
                (while (and (gethash root niwa-journal-cleanup--pending)
                            (< (float-time) deadline))
                  (sleep-for .002)))))
          (should taken)
          (should-not (gethash root niwa-journal-cleanup--pending))
          (should (string-search "ownership expired before resumption" (format "%S" diagnostics)))
          (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome taken) :status)))
          (should (= 1 (length (niwa-test-journal-cleanup--digests root)))))
      (when timer (cancel-timer timer))
      (advice-remove 'niwa-journal-claim-acquire fence)
      (delete-directory root t))))

(niwa-deftest niwa-journal-cleanup-schedule/cancellation ()
  (let ((niwa-journal-worker--child-p t)) ,test)
  (test)
  :doc "transport cancellation and disabled dispatch close acquired claims"
  (dolist (disabled '(nil t))
    (let* ((root (make-temp-file "niwa-cleanup-transport-" t))
           (workspace (niwa-workspace--create :root root))
           (directory (niwa-journal-store-directory root))
           (niwa-journal-cleanup--pending (make-hash-table :test #'equal))
           (niwa-transport--pending (make-hash-table :test #'equal))
           (niwa-transport--enabled-p t)
           (niwa-journal-cleanup--inhibit-scheduling nil))
      (unwind-protect
          (progn
            (niwa-test-journal-cleanup--entry root "retained" t)
            (niwa-journal-cleanup-schedule workspace t)
            ;; Run through acquisition, then suspend at the transport boundary.
            (dotimes (_ 5)
              (let ((timer (car (gethash root niwa-journal-cleanup--pending))))
                (cancel-timer timer)
                (apply (timer--function timer) (timer--args timer))))
            (let ((niwa-transport--depth (if disabled 0 1))
                  (niwa-transport--enabled-p (not disabled))
                  (timer (car (gethash root niwa-journal-cleanup--pending))))
              (cancel-timer timer)
              (apply (timer--function timer) (timer--args timer)))
            (unless disabled
              (should (gethash (list 'journal-cleanup root) niwa-transport--pending))
              (niwa-transport-cancel-pending (list 'journal-cleanup root)))
            (should-not (gethash root niwa-journal-cleanup--pending))
            (dolist (kind '(mutation digest-run consolidation))
              (should (eq 'completed
                          (plist-get
                           (niwa-journal-claim-outcome
                            (niwa-journal-claim-current
                             (niwa-journal-store-claim-directory directory kind)))
                           :status))))
            (should (= 1 (length (niwa-test-journal-cleanup--digests root)))))
        (niwa-transport-cancel-pending)
        (when-let* ((timer (car (gethash root niwa-journal-cleanup--pending))))
          (cancel-timer timer))
        (delete-directory root t)))))

(provide 'test-niwa-journal-cleanup)
;;; test-niwa-journal-cleanup.el ends here
