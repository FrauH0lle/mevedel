;;; test-mevedel-journal-recovery.el -- Abandoned capture recovery -*- lexical-binding: t -*-

;;; Commentary:

;; Exercises source ownership and frozen capture recovery on real storage.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-recovery)

(mevedel-deftest mevedel-journal-recovery-run ()
  ,test
  (test)
  :doc "a never-compacted abandoned session yields its frozen digest without resuming"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Remember the result" "Decisive saved evidence")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (digest "## Done\n- Observed: Decisive saved evidence (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none")
            requested)
       (should (eq 'held (plist-get (car (mevedel-journal-recovery-run workspace)) :status)))
       (should-not (mevedel-journal-capture-trigger workspace capture))
       ;; Simulate loss of the root before its normal journal sealing hook.
       (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
       (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (setq requested t)
                    (should (string-match-p "Decisive saved evidence" prompt))
                    (funcall (plist-get args :callback) digest nil))))
         (should (eq 'sealed (plist-get (car (mevedel-journal-recovery-run workspace)) :status)))
         (should-not requested)
         (should (eq 'session-end (mevedel-journal-capture-trigger workspace capture)))
         (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
         (mevedel-journal-process-next workspace)
         (should requested))
       (should (equal id (plist-get (car (mevedel-journal-store-entries (mevedel-workspace-root workspace))) :capture-id)))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "disabled or busy recovery leaves the completed checkpoint and its pin untouched"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (admission (file-name-concat (mevedel-journal-store-state-directory (mevedel-journal-process--key workspace)) "digest-run")))
       (let ((mevedel-journal-enabled nil))
         (should-not (mevedel-journal-recovery-run workspace)))
       (should-not (file-exists-p admission))
       (let ((claim (mevedel-journal-claim-acquire admission 120)))
         (unwind-protect
             (progn
               (should-not (mevedel-journal-recovery-run workspace))
               (should-not (mevedel-journal-claim-outcome claim)))
           (mevedel-journal-claim-settle claim 'completed "")))
       (should-not (mevedel-journal-capture-trigger workspace capture))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "source session mismatch is reported without repinning or sealing an unready capture"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (source (mevedel-session-save-path session))
            (ready (file-name-concat (mevedel-journal-capture--directory workspace id) "ready")))
       (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
       (mevedel-session-persistence-lock-release source session)
       (delete-file ready)
       (mevedel-journal-pins-release source id)
       (let* ((path (mevedel-session-artifacts-sidecar-path source))
              (record (mevedel-session-codec-read path)))
         (plist-put record :session-id "different-source-session")
         (mevedel-session-codec-write path record))
       (let ((result (car (mevedel-journal-recovery-run workspace))))
         (should (eq 'unavailable (plist-get result :status)))
         (should (equal "Capture does not belong to its source session" (plist-get result :error))))
       (should-not (file-exists-p ready))
       (should-not (mevedel-journal-pins-present-p source))
       (should-not (mevedel-journal-capture-trigger workspace capture)))))

  :doc "repairs an interrupted pin/ready sequence only after source authority becomes available"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (ready (file-name-concat (mevedel-journal-capture--directory workspace id) "ready")))
       (delete-file ready)
       (mevedel-journal-pins-release (mevedel-session-save-path session) id)
       (should-not (mevedel-journal-capture-list workspace))
       (should (eq 'held (plist-get (car (mevedel-journal-recovery-run workspace)) :status)))
       (should-not (file-exists-p ready))
       (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
       (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)
       (should (eq 'sealed (plist-get (car (mevedel-journal-recovery-run workspace)) :status)))
       (should (file-exists-p ready))
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (should (eq 'sealed (plist-get (car (mevedel-journal-recovery-run workspace)) :status))))))

  :doc "finishes superseded pin release and prevents an old completed-turn set from being repinned"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "First" "First result")
     (let* ((workspace (mevedel-session-workspace session))
            (old (car (mevedel-journal-capture-list workspace)))
            (old-id (plist-get old :id))
            (source (mevedel-session-save-path session))
            (old-directory (mevedel-journal-capture--directory workspace old-id)))
       (mevedel-test-journal-capture--turn session buffer "Second" "Second result")
       ;; Reproduce the state after retirement but before old pin release.
       (mevedel-journal-pins-retain source old-id nil)
       (let ((new-id (plist-get (car (mevedel-journal-capture-list workspace)) :id)))
         (should (file-exists-p (mevedel-journal-pins--path source old-id)))
         (should (mevedel-journal-recovery-run workspace))
         (should-not (file-exists-p (mevedel-journal-pins--path source old-id)))
         (should (file-exists-p (mevedel-journal-pins--path source new-id)))
         (should-not (file-exists-p (file-name-concat old-directory "capture.json")))
         (setf (mevedel-session-turn-count session) 1)
         (should-not (mevedel-journal-capture-checkpoint session buffer))
         (should-not (file-exists-p (mevedel-journal-pins--path source old-id)))))))

  :doc "portable restart fences an expired target lease and preserves the captured publication head"
  (let ((local-root (file-name-as-directory (make-temp-file "mevedel-journal-restart-" t)))
        (buffer (generate-new-buffer " *journal-restart*"))
        (mevedel-session-durability--disclosed-targets (make-hash-table :test #'equal))
        (mevedel-journal-enabled t))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("journal-restart")
          (cl-destructuring-bind (workspace session directory _segment)
              (test-mevedel-session-persistence--make-remote-restore-fixture "journal-restart" local-root "")
            (unwind-protect
                (mevedel-skills-test--with-model-backends
                  (puthash (mevedel-execution-target-identity (mevedel-session-execution-target session))
                           t mevedel-session-durability--disclosed-targets)
                  (should (mevedel-session-durability-lease-acquire directory "journal source" session))
                  (with-current-buffer buffer
                    (org-mode)
                    (setq-local mevedel--session session
                                default-directory (mevedel-workspace-root workspace)
                                gptel-backend (gptel-get-backend "Fast") gptel-model 'fast-model)
                    (gptel-mode 1)
                    (mevedel-session-set-root-buffer session buffer)
                    (setf (mevedel-session-publication session) (mevedel-session-publication-read directory))
                    (mevedel-test-journal-capture--turn session buffer "Check target" "Target passed"))
                  (let* ((capture (car (mevedel-journal-capture-list workspace)))
                         (head (plist-get capture :head))
                         (lease-dir (file-name-concat directory ".lease")))
                    (should capture)
                    (should head)
                    (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
                    (mevedel-session-durability-lease-release directory session)
                    (let ((record (mevedel-session-durability--lease-head lease-dir))
                          (now (mevedel-session-durability--target-time lease-dir)))
                      (setq record (plist-put record :client-id (make-string 64 ?f)))
                      (setq record (plist-put record :status 'active))
                      (setq record (plist-put record :renewed-at (- now 1000)))
                      (setq record (plist-put record :expires-at (- now 500)))
                      (mevedel-session-durability--write-generation lease-dir record))
                    (should (eq 'sealed (plist-get (car (mevedel-journal-recovery-run workspace)) :status)))
                    (should (eq 'session-end (mevedel-journal-capture-trigger workspace capture)))
                    (should (equal head (mevedel-session-durability-publication-head directory)))
                    (should (member head (mevedel-journal-pins-heads directory)))
                    (should (eq 'available (mevedel-session-durability-lease-state directory)))))
              (mevedel-session-durability-lease-release directory session))))
      (let ((mevedel-journal-enabled nil))
        (when (buffer-live-p buffer) (kill-buffer buffer)))
      (delete-directory local-root t)
      (mevedel-workspace-clear-registry))))

(mevedel-deftest mevedel-journal-process-schedule/recovery ()
  ,test
  (test)
  :doc "an activation coalesced into a turn opportunity recovers before inference"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-journal-process--inhibit-scheduling nil)
           requested)
       (let ((mevedel-journal-enabled nil)) (kill-buffer buffer))
       (mevedel-session-persistence-lock-release (mevedel-session-save-path session) session)
       (unwind-protect
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (_prompt &rest args)
                        (setq requested t)
                        (funcall (plist-get args :callback)
                                 "## Done\n- Observed: Result (turn 1).\n\n## Learned\n- none\n\n## Surprised\n- none\n\n## Unfinished\n- none" nil))))
             (mevedel-journal-process-schedule workspace)
             (mevedel-journal-process-schedule workspace t)
             (should-not requested)
             (let ((stop (+ (float-time) 5)))
               (while (and (not requested) (< (float-time) stop)) (accept-process-output nil 0.05)))
             (should requested)
             (should (= 1 (length (mevedel-journal-store-entries (mevedel-workspace-root workspace))))))
         (mevedel-journal-process-stop-all))))))

(provide 'test-mevedel-journal-recovery)
;;; test-mevedel-journal-recovery.el ends here
