;;; test-niwa-journal-process.el -- Digest processing tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real capture files and claim settlement, with only model delivery faked.

;;; Code:

(require 'niwa-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-journal-test-support"))
(require 'niwa-journal-claim)
(require 'niwa-memory-pass)
(require 'niwa-session-control-fs)
(require 'niwa-journal-process)
(require 'niwa-journal-worker)
(require 'niwa-transport)

(niwa-deftest niwa-journal-process-schedule/attribution ()
  ,test
  (test)
  :doc "deferred worker failure retains captured scope after origin death and buffer switch"
  (let* ((workspace (niwa-workspace--create :root "/ssh:origin:/project with spaces/"))
         (origin (generate-new-buffer " *journal-origin*"))
         (niwa-journal-enabled t)
         (niwa-journal-process--inhibit-scheduling nil)
         (niwa-journal-process--workers (make-hash-table :test #'equal))
         (niwa-journal-process--running (make-hash-table :test #'equal))
         (niwa-journal-process--recover-requested (make-hash-table :test #'equal))
         dispatch complete warnings)
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-journal-process--key) (lambda (_) "origin-key"))
                  ((symbol-function 'niwa-transport-schedule-idle)
                   (lambda (_table _key _tag _path thunk) (setq dispatch thunk)))
                  ((symbol-function 'niwa-journal-worker-local-p) (lambda (_) t))
                  ((symbol-function 'niwa-journal-worker-start)
                   (lambda (_workspace _operation _payload callback &rest _)
                     (setq complete callback) 'origin-worker))
                  ((symbol-function 'display-warning)
                   (lambda (type text &optional level _buffer) (push (list type text level) warnings))))
          (with-current-buffer origin (niwa-journal-process-schedule workspace))
          (kill-buffer origin)
          ;; A mutable owner must not rebind an already scheduled diagnostic.
          (setf (niwa-workspace-root workspace) "/tmp/rebound/")
          (with-temp-buffer
            (setq-local niwa--workspace (niwa-workspace--create :root "/tmp/current/"))
            (funcall dispatch)
            (funcall complete '(:error "Digest preparation failed")))
          (should (equal warnings
                         '((niwa "niwa [/ssh:origin:/project with spaces/]: Digest preparation failed" :warning)))))
      (when (buffer-live-p origin) (kill-buffer origin)))))
(autoload 'niwa-journal-process-next "niwa-journal-process")
(autoload 'niwa-journal-process-cancel "niwa-journal-process")
(defvar niwa-journal-process--timeout-seconds)

(autoload 'niwa-journal-process-schedule "niwa-journal-process")
(autoload 'niwa-journal-process-stop-all "niwa-journal-process")
(defvar niwa-journal-process--pending)
(defvar niwa-journal-process--exhausted-warned)

(defconst niwa-test-journal-process--digest
  "## Done\n- Local runner passed (turn 1, observed result).\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")

(niwa-deftest niwa-journal-process-next ()
  ,test
  (test)
  :doc "processes a sealed capture after source-buffer death and publishes once"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Use local runner" "Local runner passed")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'compaction)))
            callbacks prompts request-buffers)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (push prompt prompts)
                    (push (plist-get args :callback) callbacks)
                    (push (plist-get args :buffer) request-buffers))))
         (unwind-protect
             (progn
               (should (niwa-journal-process-next workspace))
               (should-not (niwa-journal-process-next workspace))
               (should (= 1 (length callbacks)))
               (should (string-match-p "Local runner passed" (car prompts)))
               (should (eq 'fast-model (buffer-local-value 'gptel-model (car request-buffers))))
               (should-not (buffer-local-value 'gptel-use-tools (car request-buffers)))
               (should-not (buffer-local-value 'gptel-max-tokens (car request-buffers)))
               (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
               (funcall (car callbacks) niwa-test-journal-process--digest
                        '(:tokens (:input 100 :output 20)))
               (funcall (car callbacks) "Late replacement" nil)
               (with-temp-buffer
                 (insert-file-contents
                  (file-name-concat root ".niwa" "state" "diagnostics" "telemetry-log.el"))
                 (goto-char (point-min))
                 (let ((event (read (current-buffer))))
                   (should (eq 'journal-digest-written (plist-get event :event)))
                   (should (= 100 (plist-get event :input-tokens)))
                   (should (= 20 (plist-get event :output-tokens)))
                   (should (equal (plist-get capture :id) (plist-get event :capture-id))))
                 (should-not (string-match-p "Local runner passed" (buffer-string))))
               (let ((entries (niwa-journal-store-entries root)))
                 (should (= 1 (length entries)))
                 (should (equal (plist-get capture :id) (plist-get (car entries) :capture-id)))
                 (should (eq 'compaction (plist-get (car entries) :trigger))))
               (should-not (niwa-journal-capture-list workspace))
               (should-not (file-exists-p
                            (file-name-concat (niwa-journal-capture--directory workspace (plist-get capture :id))
                                              "capture.json")))
               (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
               (should-not (buffer-live-p (car request-buffers)))
               (should-not (niwa-journal-process-next workspace)))
           (niwa-journal-process-cancel workspace))))))

  :doc "nothing noteworthy completes without a note, repeat capture, or memory review"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Routine question" "Routine answer")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'compaction)))
            (calls 0) scheduled)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf calls)
                    (funcall (plist-get args :callback)
                             "## Done\n- none\n\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none" nil)))
                 ((symbol-function 'niwa-memory-pass-schedule)
                  (lambda (&rest _) (setq scheduled t))))
         (let ((state (niwa-journal-process-next workspace)))
           (should-not (plist-get state :error))
           (should (plist-get state :entry)))
         (should-not (niwa-journal-store-entries root))
         (should-not (niwa-journal-capture-list workspace))
         (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
         (should (equal (append (plist-get (plist-get capture :metadata) :turn-ids) nil)
                        (niwa-journal-store-covered-turns root)))
         (should-not (niwa-journal-capture-seal session buffer 'compaction))
         (should-not (niwa-journal-process-next workspace))
         (should (= 1 calls))
         (should-not scheduled)
         (with-temp-buffer
           (insert-file-contents (file-name-concat root ".niwa/state/diagnostics/telemetry-log.el"))
           (goto-char (point-min))
           (let ((event (read (current-buffer))))
             (should (eq 'journal-digest-omitted (plist-get event :event)))
             (should (eq 'omitted (plist-get event :outcome)))))))))

  :doc "accepted nothing-noteworthy output recovers interrupted retirement without inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Question" "Answer")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'compaction)))
            (claim (niwa-journal-claim-acquire
                    (niwa-journal-process--attempts workspace capture) 120)))
       (niwa-journal-claim-settle
        claim 'completed "## Done\n- none\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (ert-fail "Repeated inference"))))
         (cl-letf (((symbol-function 'niwa-journal-capture--retire)
                    (lambda (&rest _) (error "Interrupted retirement"))))
           (should-error (niwa-journal-process--recover workspace capture nil)))
         (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
         (should (niwa-journal-store-covered-turns root))
         (niwa-journal-process-next workspace)
         (should-not (niwa-journal-capture-list workspace))
         (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
         (should-not (niwa-journal-store-entries root))))))

  :doc "failed generation retries only on later opportunities, stops after three, and warns once"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (workspace (niwa-session-workspace session))
            (niwa-journal-process--exhausted-warned (make-hash-table :test #'equal))
            (calls 0) first-warning second-warning)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (dotimes (attempt 3)
           (niwa-journal-process-next workspace)
           (should (= (1+ attempt) calls)))
         (niwa-test--with-captured-diagnostics first-warning
           (should-not (niwa-journal-process-next workspace)))
         (should (string-match-p
                  (concat "failed 3 times for " (substring (plist-get capture :id) 0 12)
                          " (capture); retry or discard it from M-x niwa-journal-jobs")
                  first-warning))
         (niwa-test--with-captured-diagnostics second-warning
           (should-not (niwa-journal-process-next workspace)))
         (should (string-empty-p second-warning))
         (should (= 3 calls))
         (with-temp-buffer
           (insert-file-contents
            (file-name-concat (niwa-workspace-root workspace)
                              ".niwa" "state" "diagnostics" "telemetry-log.el"))
           (goto-char (point-min))
           (dotimes (attempt 3)
             (let ((event (read (current-buffer))))
               (should (eq 'journal-digest-failed (plist-get event :event)))
               (should (= (1+ attempt) (plist-get event :attempt-generation))))))
         (should (= 1 (length (niwa-journal-capture-list workspace))))
         (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
         (should-not (niwa-journal-store-entries (niwa-workspace-root workspace))))))
   )

  :doc "disabled processing preserves sealed work without a model request"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let ((workspace (niwa-session-workspace session))
           (niwa-journal-enabled nil)
           requested)
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t))))
         (should-not (niwa-journal-process-next workspace)))
       (should-not requested)
       (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "accepted output survives publication failure and recovers without inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (root (niwa-workspace-root workspace))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (blocked (file-name-concat (niwa-journal-store-directory root)
                                       (niwa-journal-store--filename
                                        (niwa-journal-capture--metadata capture))))
            (calls 0))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (make-directory blocked t)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
         (let* ((state (niwa-journal-process-next workspace))
                (outcome (niwa-journal-claim-outcome (plist-get state :claim))))
           (should (plist-get state :error))
           (should (eq 'completed (plist-get outcome :status)))
           (should (equal niwa-test-journal-process--digest (plist-get outcome :payload)))
           (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
         (delete-directory blocked)
         (should-not (niwa-journal-process-next workspace))
         (should (= 1 calls)))
       (should (= 1 (length (niwa-journal-store-entries root))))
       (should-not (niwa-journal-capture-list workspace))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "multiple sealed captures use one request at a time without recursive draining"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "First request" "First result")
     (niwa-journal-capture-seal session buffer 'compaction)
     (niwa-test-journal-capture--turn session buffer "Second request" "Second result")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let ((workspace (niwa-session-workspace session)) callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (niwa-journal-process-next workspace)
               (should (= 1 (length callbacks)))
               (should-not (niwa-journal-process-next workspace))
               (funcall (car callbacks) niwa-test-journal-process--digest nil)
               (should (= 1 (length callbacks)))
               (should (= 1 (length (niwa-journal-capture-list workspace))))
               (niwa-journal-process-next workspace)
               (should (= 2 (length callbacks)))
               (funcall (car callbacks) niwa-test-journal-process--digest nil)
               (should (= 2 (length (niwa-journal-store-entries (niwa-workspace-root workspace)))))
               (should-not (niwa-journal-capture-list workspace)))
           (niwa-journal-process-cancel workspace))))))

  :doc "cancellation fences late delivery while a successor request runs"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let ((workspace (niwa-session-workspace session)) callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (let ((first (niwa-journal-process-next workspace)))
               (should (niwa-journal-process-cancel workspace))
               (should (eq 'cancelled (plist-get (niwa-journal-claim-outcome (plist-get first :claim)) :status)))
               (let ((second (niwa-journal-process-next workspace)))
                 (funcall (cadr callbacks) niwa-test-journal-process--digest nil)
                 (should-not (niwa-journal-claim-outcome (plist-get second :claim)))
                 (should-not (niwa-journal-store-entries (niwa-workspace-root workspace)))
                 (funcall (car callbacks) niwa-test-journal-process--digest nil)
                 (should (= 1 (length (niwa-journal-store-entries (niwa-workspace-root workspace)))))))
           (niwa-journal-process-cancel workspace))))))

  :doc "deadline delivery cancels a live request and preserves evidence, directly and on the real clock"
  (dolist (delivery '(direct event-loop))
    (niwa-test-journal-capture--with-session
     (lambda (session buffer)
       (niwa-test-journal-capture--turn session buffer "Request" "Result")
       (niwa-journal-capture-seal session buffer 'session-end)
       (let ((workspace (niwa-session-workspace session))
             (target-time (symbol-function 'niwa-session-control-fs-target-time))
             (register-timer (symbol-function 'run-at-time))
             clock-reading registrations callback request-buffer)
         (cl-letf (((symbol-function 'gptel-request)
                    (lambda (_prompt &rest args)
                      (setq callback (plist-get args :callback)
                            request-buffer (plist-get args :buffer))))
                   ((symbol-function 'niwa-session-control-fs-target-time)
                    (lambda (directory)
                      (let ((now (funcall target-time directory)))
                        (setq clock-reading (cons directory now))
                        now)))
                   ((symbol-function 'run-at-time)
                    (lambda (delay repeat function &rest args)
                      (let* ((reading clock-reading)
                             (before (current-time))
                             (timer (apply register-timer delay repeat function args))
                             (after (current-time)))
                        (push (list :timer timer :delay delay :repeat repeat
                                    :clock reading :before before :after after)
                              registrations)
                        timer))))
           (unwind-protect
               (let* ((state (niwa-journal-process-next workspace))
                      (timer (plist-get state :timer))
                      (registration (cl-find timer registrations
                                             :key (lambda (entry) (plist-get entry :timer))))
                      (stop (+ (float-time) 6)))
                 ;; Keep ordinary production admission and deadline registration.
                 ;; Only advance delivery once inference is demonstrably live.
                 (should callback)
                 (should (buffer-live-p request-buffer))
                 (should-not (plist-get state :settled))
                 (should (timerp timer))
                 (should (memq timer timer-list))
                 (should (> (float-time (timer--time timer)) (float-time)))
                 ;; Target-clock IO and scheduling can delay registration.  Check
                 ;; the actual returned clock and bracket the real registration,
                 ;; rather than assuming a fixed local/target clock discrepancy.
                 (should registration)
                 (let ((reading (plist-get registration :clock))
                       (delay (plist-get registration :delay))
                       (claim (plist-get state :claim)))
                   (should reading)
                   (should (equal (plist-get claim :directory) (car reading)))
                   (should (= (max 0 (- (plist-get claim :expires-at) (cdr reading))) delay))
                   (should-not (plist-get registration :repeat))
                   (should-not (time-less-p (timer--time timer)
                                            (time-add (plist-get registration :before) delay)))
                   (should-not (time-less-p (time-add (plist-get registration :after) delay)
                                            (timer--time timer))))
                 (should (niwa-journal-pins-present-p (niwa-session-save-path session)))
                 (cancel-timer timer)
                 (if (eq delivery 'direct)
                     (apply (timer--function timer) (timer--args timer))
                   ;; Retain one real event-loop delivery, without making the
                   ;; request's preparation race a shortened startup deadline.
                   (timer-set-time timer (time-add (current-time) 0.05))
                   (timer-activate timer)
                   (while (and (not (plist-get state :settled)) (< (float-time) stop))
                     (accept-process-output nil 0.01)))
                 (should (plist-get state :settled))
                 (let ((outcome (niwa-journal-claim-outcome (plist-get state :claim))))
                   (should (eq 'failed (plist-get outcome :status)))
                   (let ((payload (json-parse-string (plist-get outcome :payload))))
                     (should (equal "timeout" (gethash "class" payload)))
                     (should (equal "Digest request timed out" (gethash "error" payload)))))
                 (should-not (buffer-live-p request-buffer))
                 (funcall callback niwa-test-journal-process--digest nil)
                 (should-not (niwa-journal-store-entries (niwa-workspace-root workspace)))
                 (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
             (niwa-journal-process-cancel workspace)))))))

  :doc "missing frozen provider stays unavailable instead of selecting a fallback"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let ((workspace (niwa-session-workspace session)) requested)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (setq requested t)
                    (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
         (let* ((gptel--known-backends nil)
                (state (niwa-journal-process-next workspace))
                (outcome (niwa-journal-claim-outcome (plist-get state :claim))))
           (should (eq 'failed (plist-get outcome :status)))
           (should (equal "unavailable" (gethash "class" (json-parse-string (plist-get outcome :payload)))))
           (should-not requested)
           (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
         (niwa-journal-process-next workspace)
         (should requested)
         (should-not (niwa-journal-capture-list workspace))))))

  :doc "foreign accepted-result recovery does not starve another sealed capture"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "First request" "First result")
     (let* ((workspace (niwa-session-workspace session))
            (first (car (niwa-journal-capture-seal session buffer 'session-end)))
            (id (plist-get first :id))
            (path (file-name-concat (niwa-journal-capture--directory workspace id) "capture.json"))
            (claim (niwa-journal-claim-acquire (niwa-journal-process--attempts workspace first) 120))
            requested)
       (should (niwa-journal-claim-settle claim 'completed niwa-test-journal-process--digest))
       (niwa-test-journal-capture--turn session buffer "Second request" "Second result")
       (niwa-journal-capture-seal session buffer 'session-end)
       (let ((record (json-parse-string (niwa-session-control-fs-read-file path))))
         (puthash "source-kind" "client" record)
         (puthash "source-client" (make-string 64 ?0) record)
         (puthash "created" "2000-01-01T00:00:00Z" (gethash "metadata" record))
         (write-region (json-serialize record) nil path nil 'silent))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf requested)
                    (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
         (setq requested 0)
         (should (niwa-journal-process-next workspace)))
       (should (= 1 requested))
       (should (= 2 (length (niwa-journal-store-entries (niwa-workspace-root workspace)))))
       (should (eq 'completed (plist-get (niwa-journal-claim-outcome claim) :status)))
       (should (equal (list id) (mapcar (lambda (capture) (plist-get capture :id))
                                       (niwa-journal-capture-list workspace))))
       (should (file-exists-p (file-name-concat (niwa-session-save-path session) ".journal-pins" (concat id ".json"))))
       (should-error (niwa-journal-process-retry workspace id)))))

  :doc "unavailable frozen effort fails before a provider call and keeps source pins"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (path (file-name-concat (niwa-journal-capture--directory workspace (plist-get capture :id))
                                    "capture.json"))
            (original (niwa-session-control-fs-read-file path))
            (unknown (concat "missing-effort-" (plist-get capture :id)))
            requested)
       (unwind-protect
           (progn
             (let ((record (json-parse-string original)))
               (puthash "effort" unknown (gethash "policy" record))
               (write-region (json-serialize record) nil path nil 'silent))
             (cl-letf (((symbol-function 'gptel-request)
                        (lambda (_prompt &rest args)
                          (setq requested t)
                          (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
               (let* ((state (niwa-journal-process-next workspace))
                      (outcome (niwa-journal-claim-outcome (plist-get state :claim))))
                 (should-not requested)
                 (should (eq 'failed (plist-get outcome :status)))
                 (should (equal "unavailable" (gethash "class" (json-parse-string (plist-get outcome :payload)))))
                 (should-not (intern-soft unknown))
                 (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))
         (write-region original nil path nil 'silent)))))

  :doc "large real captures reach the digest with bounded intent and final decisions"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     ;; A large model window isolates the journal byte cap from token admission.
     (put 'fast-model :context-window 1000)
     (niwa-test-journal-capture--turn
      session buffer "User correction: isolate the runner to protect local state."
      (concat (make-string 100000 #x754c)
              "\nFinal decision: use the isolated runner because it protects local state."))
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (original (niwa-journal-capture-evidence workspace capture))
            body)
       (should (> (string-bytes (niwa-session-artifacts-read-artifact
                                session "segment-0001.chat.org" t))
                  niwa-journal-evidence-digest-max-bytes))
       (should (<= (string-bytes original) niwa-journal-evidence-digest-max-bytes))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (setq body prompt)
                    (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
         (should-not (plist-get (niwa-journal-process-next workspace) :error)))
       (should body)
       ;; The generator adds its evidence envelope outside the source allowance.
       (should (<= (string-bytes body)
                   (+ niwa-journal-evidence-digest-max-bytes 256)))
       (should (string-match-p "User correction: isolate the runner" body))
       (should (string-match-p "Final decision: use the isolated runner" body))
       (should (string-match-p "Completed turn 1; source: segment-0001.chat.org" body))
       (should (string-match-p "middle of capture unavailable" body)))))

  :doc "an already-frozen oversized capture still meets the request backstop"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (put 'fast-model :context-window 1000)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (path (file-name-concat (niwa-journal-capture--directory workspace (plist-get capture :id))
                                    "capture.json"))
            ;; Baseline shape: one unclassified blob, far above the producer's budgets.
            (frozen (concat "Frozen opening\n" (make-string 700000 ?x) "\nFrozen closing\n"))
            (record (json-parse-string (niwa-session-control-fs-read-file path)))
            body)
       (puthash "evidence" frozen record)
       (puthash "evidence-sha256" (secure-hash 'sha256 (encode-coding-string frozen 'utf-8-unix)) record)
       (write-region (json-serialize record) nil path nil 'silent)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (prompt &rest args)
                    (setq body prompt)
                    (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
         (should-not (plist-get (niwa-journal-process-next workspace) :error)))
       (should (> (string-bytes frozen) (* 5 niwa-journal-evidence-digest-max-bytes)))
       (should (<= (string-bytes body) (+ niwa-journal-evidence-digest-max-bytes 256)))
       (should (string-match-p "Frozen opening" body))
       (should (string-match-p "Frozen closing" body))
       (should (string-match-p "middle of capture unavailable" body)))))

  :doc "multibyte evidence is explicitly bounded to the captured model's input budget"
  (let ((saved-plist (copy-tree (symbol-plist 'fast-model)))
        (niwa-model-reserve-tokens 0))
    (unwind-protect
        (progn
          (niwa-test-journal-capture--with-session
           (lambda (session buffer)
             ;; The fixture rebinds the model's plist, so set the window inside.
             (put 'fast-model :context-window 12)
             (niwa-test-journal-capture--turn session buffer "Inspect output" (make-string 12000 #x754c))
             (let* ((workspace (niwa-session-workspace session))
                    (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
                    (policy (niwa-journal-process--policy workspace capture))
                    body system)
               (cl-letf (((symbol-function 'gptel-request)
                          (lambda (prompt &rest args)
                            (setq body prompt system (plist-get args :system))
                            (funcall (plist-get args :callback) niwa-test-journal-process--digest nil))))
                 (niwa-journal-process-next workspace))
               (should (string-match-p "omitted completed-work evidence" body))
               (should (<= (niwa-context-summary--estimated-tokens system body)
                           (niwa-model-usable-input-tokens policy)))
               (should (= 1 (length (niwa-journal-store-entries (niwa-workspace-root workspace)))))))))
      (setplist 'fast-model saved-plist))))

(niwa-deftest niwa-journal-process-retry ()
  ,test
  (test)
  :doc "explicit retry grants one attempt without resetting automatic exhaustion"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (niwa-journal-process--exhausted-warned (make-hash-table :test #'equal))
            (calls 0))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (dotimes (_ 3) (niwa-journal-process-next workspace))
         (should (= 3 calls))
         (should (niwa-journal-process-retry workspace (plist-get capture :id)))
         (should (= 4 calls))
         (niwa-test--with-captured-diagnostics nil
           (should-not (niwa-journal-process-next workspace)))
         (should (gethash (list (niwa-workspace-root workspace) (plist-get capture :id))
                          niwa-journal-process--exhausted-warned))
         (should (= 4 calls))
         (should (niwa-journal-pins-present-p (niwa-session-save-path session))))))))

(niwa-deftest niwa-journal-process-discard ()
  ,test
  (test)
  :doc "discard records an omission and releases its pin even when capture is disabled"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (niwa-journal-enabled nil))
       (let ((record (niwa-journal-process-discard workspace id)))
         (should (equal "discard" (plist-get record :class)))
         (should (equal record (niwa-journal-process-discard workspace id)))
         (should (file-exists-p (file-name-concat (niwa-journal-capture--directory workspace id) "discard.json"))))
       (should-not (niwa-journal-capture-list workspace))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "discard fences this client's active request before releasing evidence"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            callback)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (setq callback (plist-get args :callback)))))
         (unwind-protect
             (progn
               (niwa-journal-process-next workspace)
               (niwa-journal-process-discard workspace (plist-get capture :id))
               (funcall callback niwa-test-journal-process--digest nil)
               (should-not (niwa-journal-store-entries (niwa-workspace-root workspace)))
               (should-not (niwa-journal-capture-list workspace)))
           (niwa-journal-process-stop-all))))))

  :doc "unreadable capture requires an explicit source with its matching pin"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (source (niwa-session-save-path session)))
       (write-region "damaged descriptor" nil
                     (file-name-concat (niwa-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (should-error (niwa-journal-process-discard workspace id)
                     :type 'niwa-journal-discard-source-required)
       (should-error (niwa-journal-process-discard workspace id (niwa-workspace-root workspace)))
       (should (niwa-journal-pins-present-p source))
       (should (niwa-journal-process-discard workspace id source))
       (should-not (niwa-journal-pins-present-p source))
       (should-not (niwa-journal-capture-list workspace)))))

  :doc "discarded work is not repinned by another checkpoint of the same completed turns"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (id (plist-get (car (niwa-journal-capture-list workspace)) :id)))
       (niwa-journal-process-discard workspace id)
       (should-not (niwa-journal-capture-checkpoint session buffer))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))
       (niwa-test-journal-capture--turn session buffer "New work" "New result")
       (should (= 1 (length (niwa-journal-capture-list workspace)))))))

  :doc "foreign workspace ownership prevents discard without releasing evidence"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (id (plist-get (car (niwa-journal-capture-seal session buffer 'session-end)) :id))
            (claim (niwa-journal-claim-acquire
                    (file-name-concat (niwa-journal-store-state-directory (niwa-journal-process--key workspace)) "digest-run") 120)))
       (unwind-protect
           (progn
             (should-error (niwa-journal-process-discard workspace id) :type 'user-error)
             (should-not (niwa-journal-claim-outcome claim))
             (should (niwa-journal-pins-present-p (niwa-session-save-path session))))
         (niwa-journal-claim-settle claim 'cancelled "")))))

  :doc "accepted discard recovers its pin release even after descriptor corruption"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (payload (niwa-journal-discard-prepare workspace id))
            (claim (niwa-journal-claim-acquire (niwa-journal-process--attempts workspace capture) 120))
            requested)
       (should (niwa-journal-claim-settle claim 'cancelled payload))
       (write-region "corrupt descriptor" nil
                     (file-name-concat (niwa-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t))))
         (should-not (niwa-journal-process-next workspace)))
       (should-not requested)
       (should-not (niwa-journal-capture-list workspace))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))))

  :doc "an accepted digest completes before discard and remains idempotently inspectable"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (niwa-session-workspace session))
            (capture (car (niwa-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (claim (niwa-journal-claim-acquire (niwa-journal-process--attempts workspace capture) 120)))
       (should (niwa-journal-claim-settle claim 'completed niwa-test-journal-process--digest))
       (let ((entry (niwa-journal-process-discard workspace id)))
         (should (eq 'digest (plist-get entry :kind)))
         (should (equal entry (niwa-journal-process-discard workspace id))))
       (should-not (file-exists-p (file-name-concat (niwa-journal-capture--directory workspace id) "discard.json")))
       (should-not (niwa-journal-pins-present-p (niwa-session-save-path session)))))))

(niwa-deftest niwa-journal-process-schedule ()
  ,test
  (test)
  :doc "coalesces close opportunities and waits until the source caller returns"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (niwa-journal-capture-seal session buffer 'session-end)
     (let ((workspace (niwa-session-workspace session))
           (niwa-journal-process--inhibit-scheduling nil)
           callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (niwa-journal-process-schedule workspace)
               (niwa-journal-process-schedule workspace)
               (should-not callbacks)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer)
               (let ((stop (+ (float-time) 2)))
                 (while (and (not callbacks) (< (float-time) stop)) (accept-process-output nil 0.05)))
               (should (= 1 (length callbacks)))
               (funcall (car callbacks) niwa-test-journal-process--digest nil)
               (should-not (niwa-journal-capture-list workspace))
               (should (= 0 (hash-table-count niwa-journal-process--pending))))
           (niwa-journal-process-stop-all))))))

  :doc "Emacs exit cancels queued processing before any model inference"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session))
           (niwa-journal-process--inhibit-scheduling nil)
           requested)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t)))
                 ((symbol-function 'buffer-list) (lambda (&optional _frame) (list buffer))))
         (unwind-protect
             (progn
               (niwa-journal-process-schedule workspace)
               (niwa-session-persistence--kill-emacs-hook)
               (should-not requested)
               (should (= 0 (hash-table-count niwa-journal-process--pending)))
               (should (eq 'session-end (niwa-journal-capture-trigger
                                         workspace (car (niwa-journal-capture-list workspace))))))
           (niwa-journal-process-stop-all))))))

  :doc "closing a real view pair queues and publishes its session-end digest"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (niwa-session-workspace session))
           (view (generate-new-buffer " *journal-process-view*"))
           (niwa-journal-process--inhibit-scheduling nil)
           callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (with-current-buffer buffer
                 (set-buffer-modified-p nil)
                 (add-hook 'kill-buffer-hook #'niwa-session-persistence-release-on-kill nil t))
               (niwa-view--setup view buffer)
               (kill-buffer buffer)
               (should-not (buffer-live-p view))
               (should-not callbacks)
               (let ((stop (+ (float-time) 2)))
                 (while (and (not callbacks) (< (float-time) stop)) (accept-process-output nil 0.05)))
               (should (= 1 (length callbacks)))
               (funcall (car callbacks) niwa-test-journal-process--digest nil)
               (let ((entry (car (niwa-journal-store-entries (niwa-workspace-root workspace)))))
                 (should entry)
                 (should (eq 'session-end (plist-get entry :trigger))))
               (should-not (niwa-journal-pins-present-p (niwa-session-save-path session))))
           (niwa-journal-process-stop-all)
           (when (buffer-live-p view) (kill-buffer view))))))))

(niwa-deftest niwa-journal-process--recover (:quiet t)
  ,test
  (test)
  :doc "completed digest publication offers review while its source conversation stays live"
  (niwa-test-journal-capture--with-session
   (lambda (session buffer)
     (niwa-test-journal-capture--turn session buffer "Request" "Completed evidence")
     (niwa-journal-capture-seal session buffer 'compaction)
     (let ((workspace (niwa-session-workspace session))
           (niwa-memory-pass--inhibit-scheduling nil)
           (niwa-memory-consolidation-mode 'propose)
           (niwa-memory-consolidation-min-digests 1)
           (niwa-memory-consolidation-min-hours 24)
           (calls 0) selected)
       (unwind-protect
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (_prompt &rest args)
                        (funcall (plist-get args :callback) niwa-test-journal-process--digest nil)))
                     ((symbol-function 'niwa-memory-review-request)
                      (lambda (_scope entries _callback &rest _)
                        (cl-incf calls)
                        (setq selected entries)
                        (list :cancel #'ignore))))
             (niwa-journal-process-next workspace)
             (with-timeout (5 (ert-fail "Published digest did not offer review"))
               (while (or (> (hash-table-count niwa-memory-pass--pending) 0)
                          (zerop calls))
                 (accept-process-output nil 0.01)))
             (should (= 1 calls))
             (should (= 1 (length selected)))
             (should (buffer-live-p buffer))
             (should (equal (niwa-session-session-id session) (plist-get (car selected) :session)))
             (should (niwa-memory-pass-running workspace))
             (should-not (niwa-journal-capture-list workspace)))
         (niwa-memory-pass-stop-all)
         (niwa-journal-process-cancel workspace))))))

(provide 'test-niwa-journal-process)
;;; test-niwa-journal-process.el ends here
