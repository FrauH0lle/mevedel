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
          (mevedel-session-publication--manifest-cache (make-hash-table :test #'equal))
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
        (should-not (plist-get job :summaries))
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
        (should (= 1 (length (plist-get job :summaries))))
        (should (= (length before)
                   (length (mevedel-session-publication--generation-names directory))))
        (dotimes (_ (length before))
          (mevedel-session-collection--step session job))
        (should-not (gethash session mevedel-session-collection--jobs))
        (should (< (length (mevedel-session-publication--generation-names directory))
                   (length before)))
      (should (string-search "Checkpoint 3"
                             (mevedel-session-artifacts-read-artifact
                              session "segment-0001.chat.org" t))))))
  :doc "a new publication restarts the scan before deleting anything"
  (test-mevedel-collection--with-session
    (let ((mevedel-session-collection--slice-seconds 0))
      (mevedel-session-collection-schedule session)
      (let ((job (gethash session mevedel-session-collection--jobs)))
        (mevedel-session-collection--step session job)
        (let ((old-head (plist-get job :head)))
          (insert "New publication\n")
          (mevedel-session-artifacts-save session buffer)
          (mevedel-session-collection--step session job)
          (should-not (equal old-head (plist-get job :head)))
          (should (= 1 (length (plist-get job :summaries))))))))
  :doc "active requests defer collection and obsolete ownership cancels it"
  (test-mevedel-collection--with-session
    (mevedel-session-collection-schedule session)
    (let ((job (gethash session mevedel-session-collection--jobs)))
      (setq-local mevedel--current-request (mevedel-request--create :id "busy"))
      (mevedel-session-collection--step session job)
      (should-not (plist-get job :head))
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
