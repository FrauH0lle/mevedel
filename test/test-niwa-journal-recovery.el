;;; test-niwa-journal-recovery.el -- Abandoned capture recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises source ownership and frozen capture recovery on real storage.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-recovery)

(niwa-deftest niwa-journal-recovery-run ()
  ,test
  (test)
  :doc "a never-compacted abandoned session yields its frozen digest without resuming"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Remember the result" "Decisive saved evidence")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (digest "## Done\n- Observed: Decisive saved evidence (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")
            requested)
       (should (eq 'held (plist-get (car (niwa-journal-recovery-run workspace)) :status)))
       (should-not (niwa-journal-capture-trigger workspace capture))
       ;; Simulate loss of the root before its normal journal sealing hook.
       (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
       (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (setq requested t)
                    (should (string-match-p "Decisive saved evidence" prompt))
                    (funcall (plist-get args :callback) digest nil))))
         (should (eq 'sealed (plist-get (car (niwa-journal-recovery-run workspace)) :status)))
         (should-not requested)
         (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
         (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
         (niwa-journal-process-next workspace)
         (should requested))
       (should (equal id (plist-get (car (niwa-journal-store-entries (niwa-workspace-root workspace))) :capture-id)))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "disabled or busy recovery leaves the completed checkpoint and its pin untouched"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (admission (file-name-concat (niwa-journal-store-state-directory (niwa-journal-process--key workspace)) "digest-run")))
       (let ((niwa-journal-enabled nil))
         (should-not (niwa-journal-recovery-run workspace)))
       (should-not (file-exists-p admission))
       (let ((claim (niwa-journal-claim-acquire admission 120)))
         (unwind-protect
             (progn
               (should-not (niwa-journal-recovery-run workspace))
               (should-not (niwa-journal-claim-outcome claim)))
           (niwa-journal-claim-settle claim 'completed "")))
       (should-not (niwa-journal-capture-trigger workspace capture))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "source session mismatch is reported without repinning or sealing an unready capture"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (source (niwa-session-save-path session))
            (ready (file-name-concat (niwa-journal-capture--directory workspace id) "ready")))
       (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
       (niwa-session-persistence-lock-release source session)
       (delete-file ready)
       (niwa-journal-pins-release source id)
       ;; Keep the captured publication readable but give it a foreign identity.
       (let* ((publication (niwa-session-publication-read source (plist-get capture :head)))
              (path (plist-get publication :sidecar))
              (manifest-path (file-name-concat source (plist-get capture :head)))
              (manifest (niwa-session-codec-read manifest-path))
              (record (niwa-session-codec-read path)))
         (plist-put record :session-id "different-source-session")
         (niwa-session-codec-write path record)
         (setf (plist-get (cdr (assoc "session.meta.el" (plist-get manifest :artifacts))) :sha256)
               (secure-hash 'sha256 (niwa-session-artifacts-read-file-raw path)))
         (niwa-session-codec-write manifest-path manifest))
       (let ((result (car (niwa-journal-recovery-run workspace))))
         (should (eq 'unavailable (plist-get result :status)))
         (should (equal "Capture does not belong to its source session" (plist-get result :error))))
       (should-not (file-exists-p ready))
       (should-not (niwa-journal-pins-present-p source))
       (should-not (niwa-journal-capture-trigger workspace capture)))))

  :doc "repairs an interrupted pin/ready sequence only after source authority becomes available"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (ready (file-name-concat (niwa-journal-capture--directory workspace id) "ready")))
       (delete-file ready)
       (niwa-journal-pins-release (niwa-session-save-path session) id)
       (should-not (niwa-journal-capture-list workspace))
       (should (eq 'held (plist-get (car (niwa-journal-recovery-run workspace)) :status)))
       (should-not (file-exists-p ready))
       (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
       (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
       (should (eq 'sealed (plist-get (car (niwa-journal-recovery-run workspace)) :status)))
       (should (file-exists-p ready))
       (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (should (eq 'sealed (plist-get (car (niwa-journal-recovery-run workspace)) :status))))))

  :doc "finishes superseded pin release and prevents an old completed-turn set from being repinned"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "First" "First result")
     (let* ((workspace (niwa-session-workspace session))
            (old (car (niwa-journal-capture-list workspace)))
            (old-id (plist-get old :id))
            (source (niwa-session-save-path session))
            (old-directory (niwa-journal-capture--directory workspace old-id)))
       (niwa-test-journal-capture--turn session buffer "Second" "Second result")
       ;; Reproduce the state after retirement but before old pin release.
       (niwa-journal-pins-retain source old-id nil)
       (let ((new-id (plist-get (car (niwa-journal-capture-list workspace)) :id)))
         (should (file-exists-p (niwa-journal-pins--path source old-id)))
         (should (niwa-journal-recovery-run workspace))
         (should-not (file-exists-p (niwa-journal-pins--path source old-id)))
         (should (file-exists-p (niwa-journal-pins--path source new-id)))
         (should-not (file-exists-p (file-name-concat old-directory "capture.json")))
         (setf (niwa-session-turn-count session) 1)
         (should-not (niwa-journal-capture-checkpoint session buffer))
         (should-not (file-exists-p (niwa-journal-pins--path source old-id)))))))

  :doc "portable restart fences an expired target lease and preserves the captured publication head"
  (let ((local-root (file-name-as-directory (make-temp-file "niwa-journal-restart-" t)))
        (buffer (generate-new-buffer " *journal-restart*"))
        (niwa-session-durability--disclosed-targets (make-hash-table :test #'equal))
        (niwa-journal-enabled t))
    (unwind-protect
        (niwa-test--with-local-shell-tramp '("journal-restart")
          (cl-destructuring-bind (workspace session directory _segment)
              (test-niwa-session-persistence--make-remote-restore-fixture "journal-restart" local-root "")
            (unwind-protect
                (niwa-skills-test--with-model-backends
                  (puthash (niwa-execution-target-identity (niwa-session-execution-target session))
                           t niwa-session-durability--disclosed-targets)
                  (should (niwa-session-durability-lease-acquire directory "journal source" session))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local niwa--session session
                                default-directory (niwa-workspace-root workspace)
                                gptel-backend (gptel-get-backend "Fast") gptel-model 'fast-model)
                    (gptel-mode 1)
                    (niwa-session-set-root-buffer session buffer)
                    (setf (niwa-session-publication session) (niwa-session-publication-read directory))
                    (niwa-test-journal-capture--turn session buffer "Check target" "Target passed"))
                  (let* ((capture (car (niwa-journal-capture-list workspace)))
                         (head (plist-get capture :head))
                         (lease-dir (file-name-concat directory ".lease")))
                    (should capture)
                    (should head)
                    (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
                    (niwa-session-durability-lease-release directory session)
                    (let ((record (niwa-session-durability--lease-head lease-dir))
                          (now (niwa-session-durability--target-time lease-dir)))
                      (setq record (plist-put record :client-id (make-string 64 ?f)))
                      (setq record (plist-put record :status 'active))
                      (setq record (plist-put record :renewed-at (- now 1000)))
                      (setq record (plist-put record :expires-at (- now 500)))
                      (niwa-session-durability--write-generation lease-dir record))
                    (should (eq 'sealed (plist-get (car (niwa-journal-recovery-run workspace)) :status)))
                    (should (eq 'session-end (niwa-journal-capture-trigger workspace capture)))
                    (should (equal head (niwa-session-durability-publication-head directory)))
                    (should (member head (niwa-journal-pins-heads directory)))
                    (should (eq 'available (niwa-session-durability-lease-state directory)))))
              (niwa-session-durability-lease-release directory session))))
      (let ((niwa-journal-enabled nil))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory local-root t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-journal-process-schedule/recovery ()
  ,test
  (test)
  :doc "an activation coalesced into a turn opportunity recovers before inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session))
           (niwa-journal-process--inhibit-scheduling nil)
           requested)
       (let ((niwa-journal-enabled nil)) (kill-buffer buffer))
       (niwa-session-persistence-lock-release (niwa-session-save-path session) session)
       (unwind-protect
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (_prompt &rest args)
                        (setq requested t)
                        (funcall (plist-get args :callback)
                                 "## Done\n- Observed: Result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none" nil))))
             (niwa-journal-process-schedule workspace)
             (niwa-journal-process-schedule workspace t)
             (should-not requested)
             (let ((stop (+ (float-time) 5)))
               (while (and (not requested) (< (float-time) stop)) (accept-process-output nil 0.05)))
             (should requested)
             (should (= 1 (length (niwa-journal-store-entries (niwa-workspace-root workspace))))))
         (niwa-journal-process-stop-all))))))

(provide 'test-niwa-journal-recovery)
;;; test-niwa-journal-recovery.el ends here
