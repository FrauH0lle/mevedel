;;; test-niwa-session-collection.el --- Idle collection tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise collection against real leased publications, advancing timers directly.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "niwa-session-test-support"))
(require 'niwa-session-collection)

(defmacro test-niwa-collection--with-session (&rest body)
  "Run BODY with a published SESSION and BUFFER in disposable storage."
  (declare (indent 0))
  `(let* ((root (make-temp-file "niwa-collection-" t))
          (workspace (test-niwa-session-persistence--make-workspace root))
          (session (niwa-session-create "collection" workspace))
          (buffer (generate-new-buffer " *collection*"))
          (niwa-session-collection--jobs (make-hash-table :test #'eq))
          (niwa-session-publication--generation-cache (make-hash-table :test #'equal))
          (niwa-session-publication--facts-cache (make-hash-table :test #'equal))
          (niwa-session-publication-keep-recent-generations 1))
     (unwind-protect
         (with-current-buffer buffer
           (niwa-chat-prepare-transcript-buffer)
           (setq-local niwa--session session)
           (setq-local niwa--workspace workspace)
           (niwa-session-set-root-buffer session buffer)
           (dotimes (i 4)
             (insert (format "Checkpoint %d\n" i))
             (niwa-session-artifacts-save session buffer))
           ,@body)
       (niwa-session-collection-cancel session)
       (test-niwa-session-persistence--release-and-kill buffer session)
       (niwa-workspace-clear-registry)
       (delete-directory root t))))

(niwa-deftest niwa-session-collection-schedule (:quiet t)
  ,test
  (test)
  :doc "publication schedules collection before the first root turn settles"
  (test-niwa-collection--with-session
    (should (= 0 (niwa-session-turn-count session)))
    (should (gethash session niwa-session-collection--jobs))
    (let ((job (gethash session niwa-session-collection--jobs))
          (steps 0))
      (while (gethash session niwa-session-collection--jobs)
        (niwa-session-collection--step session job)
        (accept-process-output nil 0.01)
        (should (< (cl-incf steps) 100))))
    (should (= 1 (length (niwa-session-publication--generation-names
                         (niwa-session-save-path session))))))
  :doc "coalesces work without scanning or touching the target"
  (test-niwa-collection--with-session
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _) (error "Scheduling performed target I/O"))))
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (should (timerp (plist-get job :timer)))
        (should (= 0 (hash-table-count (plist-get job :summaries))))
        (niwa-session-collection-schedule session)
        (should (eq job (gethash session niwa-session-collection--jobs)))))))

(niwa-deftest niwa-session-collection-cancel (:quiet t)
  ,test
  (test)
  :doc "lease release cancels the timer and releases scan state"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((timer (plist-get (gethash session niwa-session-collection--jobs) :timer)))
      (niwa-session-durability-lease-release
       (niwa-session-save-path session) session)
      (should-not (gethash session niwa-session-collection--jobs))
      (should-not (memq timer timer-idle-list))
      (should-not (memq #'niwa-session-collection--on-kill kill-buffer-hook)))))

(niwa-deftest niwa-session-collection--on-kill (:quiet t)
  ,test
  (test)
  :doc "closing the root cancels pending collection"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (set-buffer-modified-p nil)
    (kill-buffer buffer)
    (should-not (gethash session niwa-session-collection--jobs))))

(niwa-deftest niwa-session-collection--arm (:quiet t)
  ,test
  (test)
  :doc "rearming replaces the old idle timer"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let* ((job (gethash session niwa-session-collection--jobs))
           (old (plist-get job :timer)))
      (niwa-session-collection--arm session job)
      (should-not (memq old timer-idle-list))
      (should (memq (plist-get job :timer) timer-idle-list)))))

(niwa-deftest niwa-session-collection--waited (:quiet t)
  ,test
  (test)
  :doc "after a long absence the returning user waits only the ordinary time"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((job (gethash session niwa-session-collection--jobs))
          (idle 3600.0)
          steps)
      (cl-letf (((symbol-function 'current-idle-time)
                 (lambda () (seconds-to-time idle)))
                ((symbol-function 'niwa-session-collection--step)
                 (lambda (&rest args) (push args steps))))
        (niwa-session-collection--arm session job 8.0)
        (let ((timer (plist-get job :timer)))
          ;; The user returns before the wait elapses.
          (setq idle 2.0)
          (cancel-timer timer)
          (apply (timer--function timer) (timer--args timer)))
        (should-not steps)
        (let ((timer (plist-get job :timer)))
          (should (memq timer timer-idle-list))
          (should (= 8.0 (float-time (timer--time timer)))))
        ;; Still idle when the wait elapses: the slice runs.
        (setq idle 3608.0)
        (niwa-session-collection--arm session job 8.0)
        (let ((timer (plist-get job :timer)))
          (should (memq timer timer-list))
          (should (> (float-time (timer--time timer)) 8.0))
          (cancel-timer timer)
          (apply (timer--function timer) (timer--args timer)))
        (should (equal (list (list session job)) steps)))))

  :doc "a wait restored after cancellation cannot rearm the cancelled job"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((job (gethash session niwa-session-collection--jobs))
          (idle 3600.0))
      (cl-letf (((symbol-function 'current-idle-time)
                 (lambda () (and idle (seconds-to-time idle)))))
        (niwa-session-collection--arm session job 8.0)
        (let ((timer (plist-get job :timer)))
          (unwind-protect
              (progn
                ;; Target I/O suspends timers; cancellation cannot reach one
                ;; on the temporarily hidden list.
                (niwa-transport-with-exclusive-connection
                  (niwa-session-collection-cancel session))
                (should (memq timer timer-list))
                (setq idle nil)
                (timer-event-handler timer)
                (should-not (gethash session niwa-session-collection--jobs))
                (should-not (memq (plist-get job :timer) timer-idle-list)))
            (cancel-timer timer)
            (cancel-timer (plist-get job :timer))))))))

(niwa-deftest niwa-session-collection--retry (:quiet t)
  ,test
  (test)
  :doc "a blocked collection waits longer each time, up to a bound"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((job (gethash session niwa-session-collection--jobs))
          waits)
      (setf (niwa-session-publication-active-p session) t)
      (dotimes (_ 8)
        (niwa-session-collection--step session job)
        (push (plist-get job :wait) waits))
      (setf (niwa-session-publication-active-p session) nil)
      (should (equal '(1.0 2.0 4.0 8.0 8.0 8.0 8.0 8.0) (nreverse waits)))
      ;; A slice that runs starts the next wait over.
      (niwa-session-collection--step session job)
      (should-not (plist-get job :wait)))))

(niwa-deftest niwa-session-collection--resumed (:quiet t)
  ,test
  (test)
  :doc "async collection failures identify their session, not the current workspace"
  (test-niwa-collection--with-session
    (let ((job (gethash session niwa-session-collection--jobs))
          warning)
      (with-temp-buffer
        (setq-local niwa--workspace
                    (test-niwa-session-persistence--make-workspace
                     (file-name-concat root "unrelated")))
        (cl-letf (((symbol-function 'display-warning)
                   (lambda (type text &optional level buffer-name)
                     (setq warning (list type text level buffer-name)))))
          (niwa-session-collection--resumed
           session job '(error "Injected collection failure"))))
      (should (equal warning
                     (list 'niwa
                           (format "niwa [%s]: Could not collect published generations: Injected collection failure"
                                   (directory-file-name root))
                           :warning nil)))
      (should-not (gethash session niwa-session-collection--jobs)))))

(niwa-deftest niwa-session-collection--observed (:quiet t)
  ,test
  (test)
  :doc "cold scans use a child and recheck a changed head before collection"
  (test-niwa-collection--with-session
    (let ((kill-emacs-hook kill-emacs-hook))
      ;; Seventeen cold generations cross the production worker threshold.
      (dotimes (index 13)
        (insert (format "More history %d\n" index))
        (niwa-session-artifacts-save session buffer))
      (should (= 17 (length (niwa-session-publication--generation-names
                            (niwa-session-save-path session)))))
      (clrhash niwa-session-publication--generation-cache)
      (clrhash niwa-session-publication--facts-cache)
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (niwa-session-collection--step session job)
        (should (processp (plist-get job :worker)))
        (should (= 0 (hash-table-count (plist-get job :summaries))))
        (insert "A new publication during the cold scan\n")
        (niwa-session-artifacts-save session buffer)
        (let ((deadline (+ (float-time) 15)))
          (while (and (plist-get job :worker) (< (float-time) deadline))
            (accept-process-output nil .01)))
        (should (eq job (gethash session niwa-session-collection--jobs)))
        (should (> (hash-table-count niwa-session-publication--generation-cache) 16))
        (should-not (plist-get job :plan))
        (niwa-session-collection--step session job)
        (should (equal (plist-get job :head) (plist-get (niwa-session-publication session) :head)))
        (should-not (plist-get job :worker))
        (let ((steps 0))
          (while (gethash session niwa-session-collection--jobs)
            (niwa-session-collection--step session job)
            (accept-process-output nil 0.01)
            (should (< (cl-incf steps) 200))))
        (should (string-search "A new publication" (niwa-session-artifacts-read-artifact
                                                    session "segment-0001.chat.org" t))))))
  :doc "cancellation removes the worker before a late reply can rearm collection"
  (test-niwa-collection--with-session
    (let ((kill-emacs-hook kill-emacs-hook))
      (dotimes (index 13)
        (insert (format "More history %d\n" index))
        (niwa-session-artifacts-save session buffer))
      (should (= 17 (length (niwa-session-publication--generation-names
                            (niwa-session-save-path session)))))
      (clrhash niwa-session-publication--generation-cache)
      (clrhash niwa-session-publication--facts-cache)
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (niwa-session-collection--step session job)
        (let ((worker (plist-get job :worker)))
          (should (processp worker))
          (niwa-session-collection-cancel session)
          (accept-process-output nil .01)
          (should-not (process-live-p worker))
          (should-not (gethash session niwa-session-collection--jobs))
          (should-not (plist-get job :timer)))))))

(niwa-deftest niwa-session-collection--step (:quiet t)
  ,test
  (test)
  :doc "warm history is checked for worker eligibility only once per head"
  (test-niwa-collection--with-session
    ;; Still large enough to start a worker if the warm-cache proof is lost.
    (dotimes (index 13)
      (insert (format "Warm history %d\n" index))
      (niwa-session-artifacts-save session buffer))
    (let* ((directory (niwa-session-save-path session))
           (names (niwa-session-publication--generation-names directory))
           (job (gethash session niwa-session-collection--jobs))
           ;; Force multiple slices without depending on machine speed.
           (niwa-session-collection--slice-seconds 0)
           (filter (symbol-function 'seq-filter))
           (visits 0))
      (should (= 17 (length names)))
      (dolist (generation names)
        (niwa-session-publication-generation-summary directory generation t))
      (cl-letf (((symbol-function 'seq-filter)
                 (lambda (predicate sequence)
                   (when (eq sequence (plist-get job :remaining))
                     (cl-incf visits (length sequence)))
                   (funcall filter predicate sequence))))
        (dotimes (_ 3)
          (niwa-session-collection--step session job))
        (should (= 3 (hash-table-count (plist-get job :summaries))))
        (should-not (plist-get job :worker))
        (should (= 17 visits))
        ;; A moved head must still reconsider the new generation set.
        (insert "New warm head\n")
        (niwa-session-artifacts-save session buffer)
        (niwa-session-collection--step session job)
        (should (= 32 visits))
        (niwa-session-collection--step session job)
        (should (= 32 visits)))))
  :doc "a cold history below the worker threshold is checked only once"
  (test-niwa-collection--with-session
    (clrhash niwa-session-publication--generation-cache)
    (clrhash niwa-session-publication--facts-cache)
    (let ((job (gethash session niwa-session-collection--jobs))
          (niwa-session-collection--slice-seconds 0)
          (filter (symbol-function 'seq-filter))
          (visits 0))
      (cl-letf (((symbol-function 'seq-filter)
                 (lambda (predicate sequence)
                   (when (eq sequence (plist-get job :remaining))
                     (cl-incf visits (length sequence)))
                   (funcall filter predicate sequence))))
        (dotimes (_ 3)
          (niwa-session-collection--step session job)))
      (should (= 3 (hash-table-count (plist-get job :summaries))))
      (should-not (plist-get job :worker))
      (should (= 4 visits))))
  :doc "cheap marking and sweep steps share one bounded idle slice"
  (test-niwa-collection--with-session
    (let ((job (gethash session niwa-session-collection--jobs))
          (niwa-session-collection--slice-seconds 10)
          (calls 0))
      (plist-put job :plan '(prepared t))
      (cl-letf (((symbol-function 'niwa-session-publication-collect-step)
                 (lambda (&rest _) (cl-incf calls) t)))
        (niwa-session-collection--step session job)
        (should (= 8 calls))
        ;; Input arriving between steps stops the slice, not just the next one.
        (cl-letf (((symbol-function 'input-pending-p) (lambda () (>= calls 10))))
          (niwa-session-collection--step session job))
        (should (= 10 calls))
        (should (timerp (plist-get job :timer))))))
  :doc "yields between reads and deletes only after the complete retention scan"
  (test-niwa-collection--with-session
    (let* ((niwa-session-collection--slice-seconds 0)
           (directory (niwa-session-save-path session))
           (before (niwa-session-publication--generation-names directory)))
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (niwa-session-collection--step session job)
        (should (= 1 (hash-table-count (plist-get job :summaries))))
        (should (= (length before)
                   (length (niwa-session-publication--generation-names directory))))
        (let ((steps 0))
          (while (gethash session niwa-session-collection--jobs)
            (niwa-session-collection--step session job)
            (accept-process-output nil 0.01)
            (should (< (cl-incf steps) 100))))
        (should-not (gethash session niwa-session-collection--jobs))
        (should (< (length (niwa-session-publication--generation-names directory))
                   (length before)))
      (should (string-search "Checkpoint 3"
                             (niwa-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t))))))
  :doc "keeps collecting while the session publishes, keeping every head whole"
  (test-niwa-collection--with-session
    (require 'niwa-telemetry)
    (let* ((niwa-session-collection--slice-seconds 0)
           (directory (niwa-session-save-path session))
           (steps 0)
           (publications 0)
           (deletion-publications 0))
      ;; The four fixture generations already leave reclaimable history.
      (let ((before (length (niwa-session-publication--generation-names directory))))
        (should (= 4 before))
        (niwa-session-collection-schedule session)
        (let ((job (gethash session niwa-session-collection--jobs)))
          (while (gethash session niwa-session-collection--jobs)
            ;; Publish between steps in every phase, deletion included.
            (when (zerop (% steps 3))
              (when (plist-get (plist-get job :plan) :operations)
                (cl-incf deletion-publications))
              (insert (format "Published during collection %d\n" (cl-incf publications)))
              (niwa-session-artifacts-save session buffer))
            (unless (plist-get job :pending)
              (niwa-session-collection--step session job))
            (accept-process-output nil 0.01)
            (should (< (cl-incf steps) 600))))
        (should (> deletion-publications 0))
        (should (< (length (niwa-session-publication--generation-names directory))
                   (+ before publications))))
      ;; Every remaining head still resolves to files that exist.
      (dolist (generation (niwa-session-publication--generation-names directory))
        (let ((publication (niwa-session-publication-read
                            directory (plist-get generation :head))))
          (dolist (artifact (plist-get publication :artifacts))
            (should (file-exists-p (plist-get (cdr artifact) :published))))))
      (should (string-search (format "Published during collection %d" publications)
                             (niwa-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t)))
      (niwa-telemetry-flush session)
      (let ((event (with-temp-buffer
                     (insert-file-contents (niwa-telemetry-path session))
                     (goto-char (point-max))
                     (when (search-backward ":event publication-collection" nil t)
                       (beginning-of-line)
                       (read (current-buffer))))))
        (should (eq 'completed (plist-get event :outcome)))
        (should (> (plist-get event :deleted-file-count) 0))
        (should (natnump (plist-get event :duration-ms)))
        (should-not (plist-get event :dropped-keys)))))

  :doc "a new publication adds only its generation to the scan"
  (test-niwa-collection--with-session
    (let ((niwa-session-collection--slice-seconds 0))
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (niwa-session-collection--step session job)
        (let ((old-head (plist-get job :head))
              (scanned (hash-table-keys (plist-get job :summaries))))
          (insert "New publication\n")
          (niwa-session-artifacts-save session buffer)
          (niwa-session-collection--step session job)
          (should-not (equal old-head (plist-get job :head)))
          ;; Immutable generations keep their summaries; one more was read.
          (should (= 2 (hash-table-count (plist-get job :summaries))))
          (dolist (head scanned)
            (should (gethash head (plist-get job :summaries))))
          (should (equal (plist-get (niwa-session-publication session) :head)
                         (plist-get (car (plist-get job :names)) :head)))))))
  :doc "pending input prevents all target I/O even after scanning finishes"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((job (gethash session niwa-session-collection--jobs)))
      (setf (plist-get job :head) (plist-get (niwa-session-publication session) :head))
      (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
                ((symbol-function 'process-file)
                 (lambda (&rest _) (ert-fail "Collection performed target I/O with pending input"))))
        (niwa-session-collection--step session job))
      (should (eq job (gethash session niwa-session-collection--jobs)))
      (should (timerp (plist-get job :timer)))
      (should-not (plist-get job :plan))))
  :doc "deletes through background programs on a pipe-capable target"
  (test-niwa-collection--with-session
    (let* ((niwa-session-collection--slice-seconds 0)
           (directory (niwa-session-save-path session))
           (before (length (niwa-session-publication--generation-names directory)))
           (steps 0)
           (waits 0))
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (with-timeout (30 (ert-fail "Background collection never finished"))
          (while (gethash session niwa-session-collection--jobs)
            (if (not (plist-get job :pending))
                (niwa-session-collection--step session job)
              (cl-incf waits)
              (accept-process-output nil 0.02))
            (should (< (cl-incf steps) 1000)))))
      (should (> waits 0))
      (should (< (length (niwa-session-publication--generation-names directory))
                 before))))
  :doc "a background deletion holds the job until its program reports"
  (test-niwa-collection--with-session
    (let ((niwa-session-collection--slice-seconds 0)
          (steps 0)
          continue)
      (niwa-session-collection-schedule session)
      (let ((job (gethash session niwa-session-collection--jobs)))
        (cl-letf (((symbol-function 'niwa-session-publication-collect-step)
                   (lambda (_session _plan callback) (setq continue callback) 'pending))
                  ((symbol-function 'niwa-transport-run-at-time)
                   (lambda (_seconds function &rest args) (apply function args))))
          (while (not continue)
            (niwa-session-collection--step session job)
            (should (< (cl-incf steps) 100)))
          (should (plist-get job :pending))
          (should-not (plist-get job :timer))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _) (ert-fail "Collection ran beside its pending program"))))
            (niwa-session-collection--step session job))
          (funcall continue nil)
          (should-not (plist-get job :pending))
          (should (timerp (plist-get job :timer)))
          (plist-put job :pending t)
          (funcall continue '(error "Injected deletion failure"))
          (should-not (gethash session niwa-session-collection--jobs))))))
  :doc "an active request does not defer collection; lost ownership cancels it"
  (test-niwa-collection--with-session
    (niwa-session-collection-schedule session)
    (let ((job (gethash session niwa-session-collection--jobs)))
      ;; A Goal keeps a request active for hours; collection must progress.
      (setq-local niwa--current-request (niwa-request--create :id "busy"))
      (niwa-session-collection--step session job)
      (should (plist-get job :head))
      (should (timerp (plist-get job :timer)))
      (setq-local niwa--current-request nil)
      (let ((lease (niwa-session-lease session)))
        (unwind-protect
            (progn
              (setf (niwa-session-lease session) '(:state lost))
              (niwa-session-collection--step session job)
              (should-not (gethash session niwa-session-collection--jobs)))
          (setf (niwa-session-lease session) lease))))))

(provide 'test-niwa-session-collection)
;;; test-niwa-session-collection.el ends here
