;;; test-mevedel-journal-process.el -- Digest processing tests -*- lexical-binding: t -*-

;;; Commentary:

;; Real capture files and claim settlement, with only model delivery faked.

;;; Code:

(require 'mevedel-journal-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "mevedel-journal-test-support"))
(require 'mevedel-journal-claim)
(require 'mevedel-memory-pass)
(autoload 'mevedel-journal-process-next "mevedel-journal-process")
(autoload 'mevedel-journal-process-cancel "mevedel-journal-process")
(defvar mevedel-journal-process--timeout-seconds)

(autoload 'mevedel-journal-process-schedule "mevedel-journal-process")
(autoload 'mevedel-journal-process-stop-all "mevedel-journal-process")
(defvar mevedel-journal-process--pending)

(defconst mevedel-test-journal-process--digest
  "## Done\n- Local runner passed (turn 1, observed result).\n## Learned\n- none\n## Surprised\n- none\n## Unfinished\n- none")

(mevedel-deftest mevedel-journal-process-next ()
  ,test
  (test)
  :doc "processes a sealed capture after source-buffer death and publishes once"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Use local runner" "Local runner passed")
     (let* ((workspace (mevedel-session-workspace session))
            (root (mevedel-workspace-root workspace))
            (capture (car (mevedel-journal-capture-seal session buffer 'compaction)))
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
               (should (mevedel-journal-process-next workspace))
               (should-not (mevedel-journal-process-next workspace))
               (should (= 1 (length callbacks)))
               (should (string-match-p "Local runner passed" (car prompts)))
               (should (eq 'fast-model (buffer-local-value 'gptel-model (car request-buffers))))
               (should-not (buffer-local-value 'gptel-use-tools (car request-buffers)))
               (should (= 4000 (buffer-local-value 'gptel-max-tokens (car request-buffers))))
               (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
               (funcall (car callbacks) mevedel-test-journal-process--digest
                        '(:tokens (:input 100 :output 20)))
               (funcall (car callbacks) "Late replacement" nil)
               (with-temp-buffer
                 (insert-file-contents
                  (file-name-concat root ".mevedel" "diagnostics" "telemetry-log.el"))
                 (goto-char (point-min))
                 (let ((event (read (current-buffer))))
                   (should (eq 'journal-digest-written (plist-get event :event)))
                   (should (= 100 (plist-get event :input-tokens)))
                   (should (= 20 (plist-get event :output-tokens)))
                   (should (equal (plist-get capture :id) (plist-get event :capture-id))))
                 (should-not (string-match-p "Local runner passed" (buffer-string))))
               (let ((entries (mevedel-journal-store-entries root)))
                 (should (= 1 (length entries)))
                 (should (equal (plist-get capture :id) (plist-get (car entries) :capture-id)))
                 (should (eq 'compaction (plist-get (car entries) :trigger))))
               (should-not (mevedel-journal-capture-list workspace))
               (should-not (file-exists-p
                            (file-name-concat (mevedel-journal-capture--directory workspace (plist-get capture :id))
                                              "capture.json")))
               (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
               (should-not (buffer-live-p (car request-buffers)))
               (should-not (mevedel-journal-process-next workspace)))
           (mevedel-journal-process-cancel workspace))))))

  :doc "failed generation retries only on later opportunities and stops after three"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session))
           (calls 0))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (dotimes (attempt 3)
           (mevedel-journal-process-next workspace)
           (should (= (1+ attempt) calls)))
         (should-not (mevedel-journal-process-next workspace))
         (should (= 3 calls))
         (with-temp-buffer
           (insert-file-contents
            (file-name-concat (mevedel-workspace-root workspace)
                              ".mevedel" "diagnostics" "telemetry-log.el"))
           (goto-char (point-min))
           (dotimes (attempt 3)
             (let ((event (read (current-buffer))))
               (should (eq 'journal-digest-failed (plist-get event :event)))
               (should (= (1+ attempt) (plist-get event :attempt-generation))))))
         (should (= 1 (length (mevedel-journal-capture-list workspace))))
         (should (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
         (should-not (mevedel-journal-store-entries (mevedel-workspace-root workspace))))))
   )

  :doc "disabled processing preserves sealed work without a model request"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-journal-enabled nil)
           requested)
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t))))
         (should-not (mevedel-journal-process-next workspace)))
       (should-not requested)
       (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "accepted output survives publication failure and recovers without inference"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (root (mevedel-workspace-root workspace))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (blocked (file-name-concat (mevedel-journal-store-directory root)
                                       (mevedel-journal-store--filename
                                        (mevedel-journal-capture--metadata capture))))
            (calls 0))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (make-directory blocked)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil))))
         (let* ((state (mevedel-journal-process-next workspace))
                (outcome (mevedel-journal-claim-outcome (plist-get state :claim))))
           (should (plist-get state :error))
           (should (eq 'completed (plist-get outcome :status)))
           (should (equal mevedel-test-journal-process--digest (plist-get outcome :payload)))
           (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (delete-directory blocked)
         (should-not (mevedel-journal-process-next workspace))
         (should (= 1 calls)))
       (should (= 1 (length (mevedel-journal-store-entries root))))
       (should-not (mevedel-journal-capture-list workspace))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "multiple sealed captures use one request at a time without recursive draining"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "First request" "First result")
     (mevedel-journal-capture-seal session buffer 'compaction)
     (mevedel-test-journal-capture--turn session buffer "Second request" "Second result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session)) callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (mevedel-journal-process-next workspace)
               (should (= 1 (length callbacks)))
               (should-not (mevedel-journal-process-next workspace))
               (funcall (car callbacks) mevedel-test-journal-process--digest nil)
               (should (= 1 (length callbacks)))
               (should (= 1 (length (mevedel-journal-capture-list workspace))))
               (mevedel-journal-process-next workspace)
               (should (= 2 (length callbacks)))
               (funcall (car callbacks) mevedel-test-journal-process--digest nil)
               (should (= 2 (length (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
               (should-not (mevedel-journal-capture-list workspace)))
           (mevedel-journal-process-cancel workspace))))))

  :doc "cancellation fences late delivery while a successor request runs"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session)) callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (let ((first (mevedel-journal-process-next workspace)))
               (should (mevedel-journal-process-cancel workspace))
               (should (eq 'cancelled (plist-get (mevedel-journal-claim-outcome (plist-get first :claim)) :status)))
               (let ((second (mevedel-journal-process-next workspace)))
                 (funcall (cadr callbacks) mevedel-test-journal-process--digest nil)
                 (should-not (mevedel-journal-claim-outcome (plist-get second :claim)))
                 (should-not (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
                 (funcall (car callbacks) mevedel-test-journal-process--digest nil)
                 (should (= 1 (length (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))))
           (mevedel-journal-process-cancel workspace))))))

  :doc "a real deadline cancels the request and preserves evidence for a later attempt"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-journal-process--timeout-seconds 3)
           callback request-buffer)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (setq callback (plist-get args :callback)
                          request-buffer (plist-get args :buffer)))))
         (unwind-protect
             (let ((state (mevedel-journal-process-next workspace))
                   (stop (+ (float-time) 6)))
               (while (and (not (plist-get state :settled)) (< (float-time) stop))
                 (accept-process-output nil 0.05))
               (should (plist-get state :settled))
               (should-not (buffer-live-p request-buffer))
               (funcall callback mevedel-test-journal-process--digest nil)
               (should-not (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
               (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
           (mevedel-journal-process-cancel workspace))))))

  :doc "missing frozen provider stays unavailable instead of selecting a fallback"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session)) requested)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (setq requested t)
                    (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil))))
         (let* ((gptel--known-backends nil)
                (state (mevedel-journal-process-next workspace))
                (outcome (mevedel-journal-claim-outcome (plist-get state :claim))))
           (should (eq 'failed (plist-get outcome :status)))
           (should (equal "unavailable" (gethash "class" (json-parse-string (plist-get outcome :payload)))))
           (should-not requested)
           (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (mevedel-journal-process-next workspace)
         (should requested)
         (should-not (mevedel-journal-capture-list workspace))))))

  :doc "foreign accepted-result recovery does not starve another sealed capture"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "First request" "First result")
     (let* ((workspace (mevedel-session-workspace session))
            (first (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (id (plist-get first :id))
            (path (file-name-concat (mevedel-journal-capture--directory workspace id) "capture.json"))
            (claim (mevedel-journal-claim-acquire (mevedel-journal-process--attempts workspace first) 120))
            requested)
       (should (mevedel-journal-claim-settle claim 'completed mevedel-test-journal-process--digest))
       (mevedel-test-journal-capture--turn session buffer "Second request" "Second result")
       (mevedel-journal-capture-seal session buffer 'session-end)
       (let ((record (json-parse-string (mevedel-session-control-fs-read-file path))))
         (puthash "source-client" (make-string 64 ?0) record)
         (puthash "created" "2000-01-01T00:00:00Z" (gethash "metadata" record))
         (write-region (json-serialize record) nil path nil 'silent))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf requested)
                    (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil))))
         (setq requested 0)
         (should (mevedel-journal-process-next workspace)))
       (should (= 1 requested))
       (should (= 2 (length (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
       (should (eq 'completed (plist-get (mevedel-journal-claim-outcome claim) :status)))
       (should (equal (list id) (mapcar (lambda (capture) (plist-get capture :id))
                                       (mevedel-journal-capture-list workspace))))
       (should (file-exists-p (file-name-concat (mevedel-session-save-path session) ".journal-pins" (concat id ".json"))))
       (should-error (mevedel-journal-process-retry workspace id)))))

  :doc "unavailable frozen effort fails before a provider call and keeps source pins"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (path (file-name-concat (mevedel-journal-capture--directory workspace (plist-get capture :id))
                                    "capture.json"))
            (original (mevedel-session-control-fs-read-file path))
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
                          (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil))))
               (let* ((state (mevedel-journal-process-next workspace))
                      (outcome (mevedel-journal-claim-outcome (plist-get state :claim))))
                 (should-not requested)
                 (should (eq 'failed (plist-get outcome :status)))
                 (should (equal "unavailable" (gethash "class" (json-parse-string (plist-get outcome :payload)))))
                 (should-not (intern-soft unknown))
                 (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))
         (write-region original nil path nil 'silent)))))

  :doc "multibyte evidence is explicitly bounded to the captured model's input budget"
  (let ((saved-plist (copy-tree (symbol-plist 'fast-model)))
        (mevedel-model-reserve-tokens 0))
    (unwind-protect
        (progn
          (put 'fast-model :context-window 12)
          (mevedel-test-journal-capture--with-session
           (lambda (session buffer)
             (mevedel-test-journal-capture--turn session buffer "Inspect output" (make-string 12000 #x754c))
             (let* ((workspace (mevedel-session-workspace session))
                    (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
                    (policy (mevedel-journal-process--policy workspace capture))
                    body system)
               (cl-letf (((symbol-function 'gptel-request)
                          (lambda (prompt &rest args)
                            (setq body prompt system (plist-get args :system))
                            (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil))))
                 (mevedel-journal-process-next workspace))
               (should (string-match-p "omitted completed-work evidence" body))
               (should (<= (mevedel-context-summary--estimated-tokens system body)
                           (mevedel-model-usable-input-tokens policy)))
               (should (= 1 (length (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))))))
      (setplist 'fast-model saved-plist))))

(mevedel-deftest mevedel-journal-process-retry ()
  ,test
  (test)
  :doc "explicit retry grants one attempt without resetting automatic exhaustion"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (calls 0))
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args)
                    (cl-incf calls)
                    (funcall (plist-get args :callback) nil '(:error "Provider failed")))))
         (dotimes (_ 3) (mevedel-journal-process-next workspace))
         (should (= 3 calls))
         (should (mevedel-journal-process-retry workspace (plist-get capture :id)))
         (should (= 4 calls))
         (should-not (mevedel-journal-process-next workspace))
         (should (= 4 calls))
         (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))))

(mevedel-deftest mevedel-journal-process-discard ()
  ,test
  (test)
  :doc "discard records an omission and releases its pin even when capture is disabled"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (mevedel-journal-enabled nil))
       (let ((record (mevedel-journal-process-discard workspace id)))
         (should (equal "discard" (plist-get record :class)))
         (should (equal record (mevedel-journal-process-discard workspace id)))
         (should (file-exists-p (file-name-concat (mevedel-journal-capture--directory workspace id) "discard.json"))))
       (should-not (mevedel-journal-capture-list workspace))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "discard fences this client's active request before releasing evidence"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            callback)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (setq callback (plist-get args :callback)))))
         (unwind-protect
             (progn
               (mevedel-journal-process-next workspace)
               (mevedel-journal-process-discard workspace (plist-get capture :id))
               (funcall callback mevedel-test-journal-process--digest nil)
               (should-not (mevedel-journal-store-entries (mevedel-workspace-root workspace)))
               (should-not (mevedel-journal-capture-list workspace)))
           (mevedel-journal-process-stop-all))))))

  :doc "unreadable capture requires an explicit source with its matching pin"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (source (mevedel-session-save-path session)))
       (write-region "damaged descriptor" nil
                     (file-name-concat (mevedel-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (should-error (mevedel-journal-process-discard workspace id)
                     :type 'mevedel-journal-discard-source-required)
       (should-error (mevedel-journal-process-discard workspace id (mevedel-workspace-root workspace)))
       (should (mevedel-journal-pins-present-p source))
       (should (mevedel-journal-process-discard workspace id source))
       (should-not (mevedel-journal-pins-present-p source))
       (should-not (mevedel-journal-capture-list workspace)))))

  :doc "discarded work is not repinned by another checkpoint of the same completed turns"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (id (plist-get (car (mevedel-journal-capture-list workspace)) :id)))
       (mevedel-journal-process-discard workspace id)
       (should-not (mevedel-journal-capture-checkpoint session buffer))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))
       (mevedel-test-journal-capture--turn session buffer "New work" "New result")
       (should (= 1 (length (mevedel-journal-capture-list workspace)))))))

  :doc "foreign workspace ownership prevents discard without releasing evidence"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (id (plist-get (car (mevedel-journal-capture-seal session buffer 'session-end)) :id))
            (claim (mevedel-journal-claim-acquire
                    (file-name-concat (mevedel-journal-process--key workspace) "state" "digest-run") 120)))
       (unwind-protect
           (progn
             (should-error (mevedel-journal-process-discard workspace id) :type 'user-error)
             (should-not (mevedel-journal-claim-outcome claim))
             (should (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
         (mevedel-journal-claim-settle claim 'cancelled "")))))

  :doc "accepted discard recovers its pin release even after descriptor corruption"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-list workspace)))
            (id (plist-get capture :id))
            (payload (mevedel-journal-discard-prepare workspace id))
            (claim (mevedel-journal-claim-acquire (mevedel-journal-process--attempts workspace capture) 120))
            requested)
       (should (mevedel-journal-claim-settle claim 'cancelled payload))
       (write-region "corrupt descriptor" nil
                     (file-name-concat (mevedel-journal-capture--directory workspace id) "capture.json") nil 'silent)
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t))))
         (should-not (mevedel-journal-process-next workspace)))
       (should-not requested)
       (should-not (mevedel-journal-capture-list workspace))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))))

  :doc "an accepted digest completes before discard and remains idempotently inspectable"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let* ((workspace (mevedel-session-workspace session))
            (capture (car (mevedel-journal-capture-seal session buffer 'session-end)))
            (id (plist-get capture :id))
            (claim (mevedel-journal-claim-acquire (mevedel-journal-process--attempts workspace capture) 120)))
       (should (mevedel-journal-claim-settle claim 'completed mevedel-test-journal-process--digest))
       (let ((entry (mevedel-journal-process-discard workspace id)))
         (should (eq 'digest (plist-get entry :kind)))
         (should (equal entry (mevedel-journal-process-discard workspace id))))
       (should-not (file-exists-p (file-name-concat (mevedel-journal-capture--directory workspace id) "discard.json")))
       (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session)))))))

(mevedel-deftest mevedel-journal-process-schedule ()
  ,test
  (test)
  :doc "coalesces close opportunities and waits until the source caller returns"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (mevedel-journal-capture-seal session buffer 'session-end)
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-journal-process--inhibit-scheduling nil)
           callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (mevedel-journal-process-schedule workspace)
               (mevedel-journal-process-schedule workspace)
               (should-not callbacks)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer)
               (let ((stop (+ (float-time) 2)))
                 (while (and (not callbacks) (< (float-time) stop)) (accept-process-output nil 0.05)))
               (should (= 1 (length callbacks)))
               (funcall (car callbacks) mevedel-test-journal-process--digest nil)
               (should-not (mevedel-journal-capture-list workspace))
               (should (= 0 (hash-table-count mevedel-journal-process--pending))))
           (mevedel-journal-process-stop-all))))))

  :doc "Emacs exit cancels queued processing before any model inference"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-journal-process--inhibit-scheduling nil)
           requested)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (cl-letf (((symbol-function 'gptel-request) (lambda (&rest _) (setq requested t)))
                 ((symbol-function 'buffer-list) (lambda (&optional _frame) (list buffer))))
         (unwind-protect
             (progn
               (mevedel-journal-process-schedule workspace)
               (mevedel-session-persistence--kill-emacs-hook)
               (should-not requested)
               (should (= 0 (hash-table-count mevedel-journal-process--pending)))
               (should (eq 'session-end (mevedel-journal-capture-trigger
                                         workspace (car (mevedel-journal-capture-list workspace))))))
           (mevedel-journal-process-stop-all))))))

  :doc "closing a real view pair queues and publishes its session-end digest"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Result")
     (let ((workspace (mevedel-session-workspace session))
           (view (generate-new-buffer " *journal-process-view*"))
           (mevedel-journal-process--inhibit-scheduling nil)
           callbacks)
       (cl-letf (((symbol-function 'gptel-request)
                  (lambda (_prompt &rest args) (push (plist-get args :callback) callbacks))))
         (unwind-protect
             (progn
               (with-current-buffer buffer
                 (set-buffer-modified-p nil)
                 (add-hook 'kill-buffer-hook #'mevedel-session-persistence-release-on-kill nil t))
               (mevedel-view--setup view buffer)
               (kill-buffer buffer)
               (should-not (buffer-live-p view))
               (should-not callbacks)
               (let ((stop (+ (float-time) 2)))
                 (while (and (not callbacks) (< (float-time) stop)) (accept-process-output nil 0.05)))
               (should (= 1 (length callbacks)))
               (funcall (car callbacks) mevedel-test-journal-process--digest nil)
               (let ((entry (car (mevedel-journal-store-entries (mevedel-workspace-root workspace)))))
                 (should entry)
                 (should (eq 'session-end (plist-get entry :trigger))))
               (should-not (mevedel-journal-pins-present-p (mevedel-session-save-path session))))
           (mevedel-journal-process-stop-all)
           (when (buffer-live-p view) (kill-buffer view))))))))

(mevedel-deftest mevedel-journal-process--recover (:quiet t)
  ,test
  (test)
  :doc "completed digest publication offers review while its source conversation stays live"
  (mevedel-test-journal-capture--with-session
   (lambda (session buffer)
     (mevedel-test-journal-capture--turn session buffer "Request" "Completed evidence")
     (mevedel-journal-capture-seal session buffer 'compaction)
     (let ((workspace (mevedel-session-workspace session))
           (mevedel-memory-pass--inhibit-scheduling nil)
           (mevedel-memory-consolidation-mode 'propose)
           (mevedel-memory-consolidation-min-digests 1)
           (mevedel-memory-consolidation-min-hours 24)
           (calls 0) selected)
       (unwind-protect
           (cl-letf (((symbol-function 'gptel-request)
                      (lambda (_prompt &rest args)
                        (funcall (plist-get args :callback) mevedel-test-journal-process--digest nil)))
                     ((symbol-function 'mevedel-memory-review-request)
                      (lambda (_scope entries _callback &rest _)
                        (cl-incf calls)
                        (setq selected entries)
                        (list :cancel #'ignore))))
             (mevedel-journal-process-next workspace)
             (with-timeout (5 (ert-fail "Published digest did not offer review"))
               (while (> (hash-table-count mevedel-memory-pass--pending) 0)
                 (accept-process-output nil 0.01)))
             (should (= 1 calls))
             (should (= 1 (length selected)))
             (should (buffer-live-p buffer))
             (should (equal (mevedel-session-session-id session) (plist-get (car selected) :session)))
             (should (mevedel-memory-pass-running workspace))
             (should-not (mevedel-journal-capture-list workspace)))
         (mevedel-memory-pass-stop-all)
         (mevedel-journal-process-cancel workspace))))))

(provide 'test-mevedel-journal-process)
;;; test-mevedel-journal-process.el ends here
