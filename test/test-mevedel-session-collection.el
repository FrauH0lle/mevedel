;;; test-mevedel-session-collection.el --- Idle collection tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise collection against real leased publications, advancing timers directly.

;;; Code:

(require 'mevedel-session-test-support
         (file-name-concat
          (file-name-directory (or load-file-name buffer-file-name))
          "mevedel-session-test-support"))
(require 'mevedel-session-collection)

(defmacro test-mevedel-collection--with-session (&rest body)
  "Run BODY with a published SESSION and BUFFER in disposable storage."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mevedel-collection-" t))
          (workspace (test-mevedel-session-persistence--make-workspace root))
          (session (mevedel-session-create "collection" workspace))
          (buffer (generate-new-buffer " *collection*"))
          (mevedel-session-collection--jobs (make-hash-table :test #'eq))
          (mevedel-session-publication--generation-cache (make-hash-table :test #'equal))
          (mevedel-session-publication--facts-cache (make-hash-table :test #'equal))
          (mevedel-session-publication-keep-recent-generations 1))
     (unwind-protect
         (with-current-buffer buffer
           (mevedel-chat-prepare-transcript-buffer)
           (setq-local mevedel--session session)
           (setq-local mevedel--workspace workspace)
           (mevedel-session-set-root-buffer session buffer)
           (dotimes (i 4)
             (insert (format "Checkpoint %d\n" i))
             (mevedel-session-artifacts-save session buffer))
           ,@body)
       (mevedel-session-collection-cancel session)
       (test-mevedel-session-persistence--release-and-kill buffer session)
       (mevedel-workspace-clear-registry)
       (delete-directory root t))))

(mevedel-deftest mevedel-session-collection-schedule (:quiet t)
  ,test
  (test)
  :doc "coalesces work without scanning or touching the target"
  (test-mevedel-collection--with-session
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _) (error "Scheduling performed target I/O"))))
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (should (timerp (plist-get job :timer)))
        (should (= 0 (hash-table-count (plist-get job :summaries))))
        (mevedel-session-collection-schedule session)
        (should (eq job (gethash session mevedel-session-collection--jobs)))))))

(mevedel-deftest mevedel-session-collection-cancel (:quiet t)
  ,test
  (test)
  :doc "lease release cancels the timer and releases scan state"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let ((timer (plist-get (gethash session mevedel-session-collection--jobs) :timer)))
      (mevedel-session-durability-lease-release
       (mevedel-session-save-path session) session)
      (should-not (gethash session mevedel-session-collection--jobs))
      (should-not (memq timer timer-idle-list))
      (should-not (memq #'mevedel-session-collection--on-kill kill-buffer-hook)))))

(mevedel-deftest mevedel-session-collection--on-kill (:quiet t)
  ,test
  (test)
  :doc "closing the root cancels pending collection"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (set-buffer-modified-p nil)
    (kill-buffer buffer)
    (should-not (gethash session mevedel-session-collection--jobs))))

(mevedel-deftest mevedel-session-collection--arm (:quiet t)
  ,test
  (test)
  :doc "rearming replaces the old idle timer"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let* ((job (gethash session mevedel-session-collection--jobs))
           (old (plist-get job :timer)))
      (mevedel-session-collection--arm session job)
      (should-not (memq old timer-idle-list))
      (should (memq (plist-get job :timer) timer-idle-list)))))

(mevedel-deftest mevedel-session-collection--retry (:quiet t)
  ,test
  (test)
  :doc "a blocked collection waits longer each time, up to a bound"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let ((job (gethash session mevedel-session-collection--jobs))
          waits)
      (setf (mevedel-session-publication-active-p session) t)
      (dotimes (_ 8)
        (mevedel-session-collection--step session job)
        (push (plist-get job :wait) waits))
      (setf (mevedel-session-publication-active-p session) nil)
      (should (equal '(1.0 2.0 4.0 8.0 8.0 8.0 8.0 8.0) (nreverse waits)))
      ;; A slice that runs starts the next wait over.
      (mevedel-session-collection--step session job)
      (should-not (plist-get job :wait)))))

(mevedel-deftest mevedel-session-collection--observed (:quiet t)
  ,test
  (test)
  :doc "cold scans use a child and recheck a changed head before collection"
  (test-mevedel-collection--with-session
    (let ((kill-emacs-hook kill-emacs-hook))
      (dotimes (index 14)
        (insert (format "More history %d\n" index))
        (mevedel-session-artifacts-save session buffer))
      (clrhash mevedel-session-publication--generation-cache)
      (clrhash mevedel-session-publication--facts-cache)
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (mevedel-session-collection--step session job)
        (should (processp (plist-get job :worker)))
        (should (= 0 (hash-table-count (plist-get job :summaries))))
        (insert "A new publication during the cold scan\n")
        (mevedel-session-artifacts-save session buffer)
        (let ((deadline (+ (float-time) 15)))
          (while (and (plist-get job :worker) (< (float-time) deadline))
            (accept-process-output nil .01)))
        (should (eq job (gethash session mevedel-session-collection--jobs)))
        (should (> (hash-table-count mevedel-session-publication--generation-cache) 16))
        (should-not (plist-get job :plan))
        (mevedel-session-collection--step session job)
        (should (equal (plist-get job :head) (plist-get (mevedel-session-publication session) :head)))
        (should-not (plist-get job :worker))
        (let ((steps 0))
          (while (gethash session mevedel-session-collection--jobs)
            (mevedel-session-collection--step session job)
            (should (< (cl-incf steps) 200))))
        (should (string-search "A new publication" (mevedel-session-artifacts-read-artifact
                                                    session "segment-0001.chat.org" t))))))
  :doc "cancellation removes the worker before a late reply can rearm collection"
  (test-mevedel-collection--with-session
    (let ((kill-emacs-hook kill-emacs-hook))
      (dotimes (index 14)
        (insert (format "More history %d\n" index))
        (mevedel-session-artifacts-save session buffer))
      (clrhash mevedel-session-publication--generation-cache)
      (clrhash mevedel-session-publication--facts-cache)
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (mevedel-session-collection--step session job)
        (let ((worker (plist-get job :worker)))
          (should (processp worker))
          (mevedel-session-collection-cancel session)
          (accept-process-output nil .01)
          (should-not (process-live-p worker))
          (should-not (gethash session mevedel-session-collection--jobs))
          (should-not (plist-get job :timer)))))))

(mevedel-deftest mevedel-session-collection--step (:quiet t)
  ,test
  (test)
  :doc "yields between reads and deletes only after the complete retention scan"
  (test-mevedel-collection--with-session
    (let* ((mevedel-session-collection--slice-seconds 0)
           (directory (mevedel-session-save-path session))
           (before (mevedel-session-publication--generation-names directory)))
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (mevedel-session-collection--step session job)
        (should (= 1 (hash-table-count (plist-get job :summaries))))
        (should (= (length before)
                   (length (mevedel-session-publication--generation-names directory))))
        (let ((steps 0))
          (while (gethash session mevedel-session-collection--jobs)
            (mevedel-session-collection--step session job)
            (should (< (cl-incf steps) 100))))
        (should-not (gethash session mevedel-session-collection--jobs))
        (should (< (length (mevedel-session-publication--generation-names directory))
                   (length before)))
      (should (string-search "Checkpoint 3"
                             (mevedel-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t))))))
  :doc "keeps collecting while the session publishes, keeping every head whole"
  (test-mevedel-collection--with-session
    (require 'mevedel-telemetry)
    (let* ((mevedel-session-collection--slice-seconds 0)
           (directory (mevedel-session-save-path session))
           (steps 0)
           (publications 0))
      (dotimes (index 8)
        (insert (format "Earlier history %d\n" index))
        (mevedel-session-artifacts-save session buffer))
      (let ((before (length (mevedel-session-publication--generation-names directory))))
        (mevedel-session-collection-schedule session)
        (let ((job (gethash session mevedel-session-collection--jobs)))
          (while (gethash session mevedel-session-collection--jobs)
            ;; Publish between steps in every phase, deletion included.
            (when (zerop (% steps 3))
              (insert (format "Published during collection %d\n" (cl-incf publications)))
              (mevedel-session-artifacts-save session buffer))
            (unless (plist-get job :pending)
              (mevedel-session-collection--step session job))
            (should (< (cl-incf steps) 600))))
        (should (< (length (mevedel-session-publication--generation-names directory))
                   (+ before publications))))
      ;; Every remaining head still resolves to files that exist.
      (dolist (generation (mevedel-session-publication--generation-names directory))
        (let ((publication (mevedel-session-publication-read
                            directory (plist-get generation :head))))
          (dolist (artifact (plist-get publication :artifacts))
            (should (file-exists-p (plist-get (cdr artifact) :published))))))
      (should (string-search (format "Published during collection %d" publications)
                             (mevedel-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t)))
      (mevedel-telemetry-flush session)
      (let ((event (with-temp-buffer
                     (insert-file-contents (mevedel-telemetry-path session))
                     (goto-char (point-max))
                     (when (search-backward ":event publication-collection" nil t)
                       (beginning-of-line)
                       (read (current-buffer))))))
        (should (eq 'completed (plist-get event :outcome)))
        (should (> (plist-get event :deleted-file-count) 0))
        (should (natnump (plist-get event :duration-ms)))
        (should-not (plist-get event :dropped-keys)))))

  :doc "a new publication adds only its generation to the scan"
  (test-mevedel-collection--with-session
    (let ((mevedel-session-collection--slice-seconds 0))
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (mevedel-session-collection--step session job)
        (let ((old-head (plist-get job :head))
              (scanned (hash-table-keys (plist-get job :summaries))))
          (insert "New publication\n")
          (mevedel-session-artifacts-save session buffer)
          (mevedel-session-collection--step session job)
          (should-not (equal old-head (plist-get job :head)))
          ;; Immutable generations keep their summaries; one more was read.
          (should (= 2 (hash-table-count (plist-get job :summaries))))
          (dolist (head scanned)
            (should (gethash head (plist-get job :summaries))))
          (should (equal (plist-get (mevedel-session-publication session) :head)
                         (plist-get (car (plist-get job :names)) :head)))))))
  :doc "pending input prevents all target I/O even after scanning finishes"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let ((job (gethash session mevedel-session-collection--jobs)))
      (setf (plist-get job :head) (plist-get (mevedel-session-publication session) :head))
      (cl-letf (((symbol-function 'input-pending-p) (lambda () t))
                ((symbol-function 'process-file)
                 (lambda (&rest _) (ert-fail "Collection performed target I/O with pending input"))))
        (mevedel-session-collection--step session job))
      (should (eq job (gethash session mevedel-session-collection--jobs)))
      (should (timerp (plist-get job :timer)))
      (should-not (plist-get job :plan))))
  :doc "deletes through background programs on a pipe-capable target"
  (test-mevedel-collection--with-session
    (let* ((mevedel-session-collection--slice-seconds 0)
           (mevedel-session-control-fs--pipe-local t)
           (directory (mevedel-session-save-path session))
           (before (length (mevedel-session-publication--generation-names directory)))
           (steps 0)
           (waits 0))
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (with-timeout (30 (ert-fail "Background collection never finished"))
          (while (gethash session mevedel-session-collection--jobs)
            (if (not (plist-get job :pending))
                (mevedel-session-collection--step session job)
              (cl-incf waits)
              (accept-process-output nil 0.02))
            (should (< (cl-incf steps) 1000)))))
      (should (> waits 0))
      (should (< (length (mevedel-session-publication--generation-names directory))
                 before))))
  :doc "a background deletion holds the job until its program reports"
  (test-mevedel-collection--with-session
    (let ((mevedel-session-collection--slice-seconds 0)
          (steps 0)
          continue)
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (cl-letf (((symbol-function 'mevedel-session-publication-collect-step)
                   (lambda (_session _plan callback) (setq continue callback) 'pending))
                  ((symbol-function 'mevedel-transport-run-at-time)
                   (lambda (_seconds function &rest args) (apply function args))))
          (while (not continue)
            (mevedel-session-collection--step session job)
            (should (< (cl-incf steps) 100)))
          (should (plist-get job :pending))
          (should-not (plist-get job :timer))
          (cl-letf (((symbol-function 'process-file)
                     (lambda (&rest _) (ert-fail "Collection ran beside its pending program"))))
            (mevedel-session-collection--step session job))
          (funcall continue nil)
          (should-not (plist-get job :pending))
          (should (timerp (plist-get job :timer)))
          (plist-put job :pending t)
          (funcall continue '(error "Injected deletion failure"))
          (should-not (gethash session mevedel-session-collection--jobs))))))
  :doc "an active request does not defer collection; lost ownership cancels it"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let ((job (gethash session mevedel-session-collection--jobs)))
      ;; A Goal keeps a request active for hours; collection must progress.
      (setq-local mevedel--current-request (mevedel-request--create :id "busy"))
      (mevedel-session-collection--step session job)
      (should (plist-get job :head))
      (should (timerp (plist-get job :timer)))
      (setq-local mevedel--current-request nil)
      (let ((lease (mevedel-session-lease session)))
        (unwind-protect
            (progn
              (setf (mevedel-session-lease session) '(:state lost))
              (mevedel-session-collection--step session job)
              (should-not (gethash session mevedel-session-collection--jobs)))
          (setf (mevedel-session-lease session) lease))))))

(provide 'test-mevedel-session-collection)
;;; test-mevedel-session-collection.el ends here
