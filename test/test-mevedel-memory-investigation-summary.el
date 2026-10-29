;;; test-mevedel-memory-investigation-summary.el -- Private call audit -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise delivery metadata through real tools, review settlement and storage.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'gptel-request)
(require 'gptel-openai)
(require 'mevedel-memory-investigation)
(require 'mevedel-memory-review)
(require 'mevedel-memory-pass)
(require 'mevedel-memory-store)
(require 'mevedel-memory-scope)
(require 'mevedel-journal-worker)
(require 'mevedel-telemetry)
(require 'mevedel-models)
(require 'mevedel-system)
(require 'mevedel-sandbox)

(defun mevedel-test-summary--call (state operation args)
  "Run STATE's real OPERATION with ARGS and return its delivered text."
  (let (done result)
    (mevedel-memory-investigation-call
     state operation args (lambda (text) (setq result text done t)))
    (with-timeout (10 (ert-fail "Investigation did not settle"))
      (while (not done) (accept-process-output nil 0.01)))
    result))

(mevedel-deftest mevedel-memory-investigation-summary
    (:vars* ((directory (make-temp-file "mevedel-summary-" t))
             (workspace (mevedel-workspace--create :root directory))
             (mevedel-memory-dirs nil)
             (mevedel-sandbox-mode 'off)
             (state (mevedel-memory-investigation-create
                     (mevedel-memory-scope-capture workspace) nil (lambda () t) #'ignore)))
     :after-each ((mevedel-memory-investigation-stop state)
                  (mevedel-execution-teardown-all)
                  (delete-directory directory t)))
  ,test
  (test)
  :doc "real Read, Grep and Glob retain targets and delivery facts but never result bodies"
  (progn
    (with-temp-file (file-name-concat directory "source.txt")
      (insert "PRIVATE-CONTENT-NEVER-PERSISTED\n"))
    (should (equal '(:calls nil :omitted-count 0) (mevedel-memory-investigation-summary state)))
    (let* ((text (mevedel-test-summary--call state 'read '(:root "workspace" :path " ./source.txt " :offset 1 :limit 1)))
           (snapshot (mevedel-memory-investigation-summary state))
           (row (car (plist-get snapshot :calls))))
      (should (equal "Read" (plist-get row :tool-name)))
      (should (equal "source.txt" (plist-get row :investigation-path)))
      (should (= 1 (plist-get row :offset)))
      (should (= 1 (plist-get row :limit)))
      (should (eq 'delivered (plist-get row :status)))
      (should (= (string-bytes text) (plist-get row :result-bytes)))
      (should (equal (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix)) (plist-get row :result-hash)))
      (should-not (plist-get row :partial))
      (should-not (plist-get row :truncated))
      (should-not (string-search "PRIVATE-CONTENT" (prin1-to-string snapshot)))
      ;; Consumer edits must not corrupt the canonical audit.
      (aset (plist-get row :investigation-path) 0 ?X)
      (plist-put row :status 'changed)
      (should (equal "source.txt" (plist-get (car (plist-get (mevedel-memory-investigation-summary state) :calls)) :investigation-path))))
    (mevedel-test-summary--call state 'grep '(:root "workspace" :path "." :pattern "NEVER"))
    (mevedel-test-summary--call state 'glob '(:root "workspace" :path "." :pattern "*.txt"))
    (mevedel-test-summary--call state 'read '(:root "workspace" :path "missing.txt"))
    (let* ((snapshot (mevedel-memory-investigation-summary state))
           (rows (plist-get snapshot :calls)))
      (should (equal '(1 2 3 4) (mapcar (lambda (row) (plist-get row :call-id)) rows)))
      (should (equal '("Read" "Grep" "Glob" "Read") (mapcar (lambda (row) (plist-get row :tool-name)) rows)))
      (should (equal "NEVER" (plist-get (nth 1 rows) :investigation-pattern)))
      (should (eq 'error (plist-get (nth 3 rows) :status)))
      (should-not (string-search "Source file is unavailable" (prin1-to-string snapshot)))
      ;; Neither Read nor search delivery retains a source-body accumulator.
      (should-not (string-search "PRIVATE-CONTENT-NEVER-PERSISTED" (prin1-to-string state)))))
  :doc "partial snapshots, long-line clipping and result clipping are observable independently"
  (progn
    (with-temp-file (file-name-concat directory "huge.txt") (insert (make-string 600000 ?x)))
    (with-temp-file (file-name-concat directory "long.txt") (insert (make-string 3000 ?y)))
    (with-temp-file (file-name-concat directory "many.txt")
      (dotimes (_ 100) (insert (make-string 200 ?z) "\n")))
    (mevedel-test-summary--call state 'grep '(:root "workspace" :path "." :pattern "absent"))
    (mevedel-test-summary--call state 'read '(:root "workspace" :path "long.txt"))
    (mevedel-test-summary--call state 'read '(:root "workspace" :path "many.txt"))
    (let ((rows (plist-get (mevedel-memory-investigation-summary state) :calls)))
      (should (plist-get (nth 0 rows) :partial))
      (should-not (plist-get (nth 0 rows) :truncated))
      (should (plist-get (nth 1 rows) :truncated))
      (should-not (plist-get (nth 1 rows) :partial))
      (should (plist-get (nth 2 rows) :truncated))
      (should (<= (plist-get (nth 2 rows) :result-bytes) 8192))))
  :doc "summary count, escaped serialized bytes and Unicode query fields are bounded with explicit omissions"
  (let ((target (make-string 255 ?x))
        (pattern (make-string 1024 ?\u754c)))
    (dotimes (_ 64)
      (mevedel-test-summary--call state 'read (list :root target :path target :pattern pattern)))
    (let* ((snapshot (mevedel-memory-investigation-summary state))
           (rows (plist-get snapshot :calls))
           (print-escape-nonascii t) (print-escape-multibyte t))
      (should (> (plist-get snapshot :omitted-count) 0))
      (should (= 64 (+ (length rows) (plist-get snapshot :omitted-count))))
      (should (<= (string-bytes (prin1-to-string snapshot)) 32768))
      (should (cl-every (lambda (row) (and (plist-get row :target-truncated)
                                         (<= (string-bytes (plist-get row :investigation-pattern)) 256))) rows))
      (mevedel-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Refused call delivered")))
      (should (equal snapshot (mevedel-memory-investigation-summary state))))
    (should (mevedel-memory-investigation-stopped state)))
  :doc "negative control: complete short source lines containing clipping-marker literals remain complete"
  (progn
    (with-temp-file (file-name-concat directory "literal.el")
      (insert "(format \" [... Read(file_path=%S, offset=%d, limit=1, char_offset=%d) to continue this line]\")\n"
              ";; [mevedel: output truncated]\n"
              ";; ... Read output truncated\n"
              ";; Warning: results are partial.\n"))
    (mevedel-test-summary--call state 'read '(:root "workspace" :path "literal.el"))
    (let ((row (car (plist-get (mevedel-memory-investigation-summary state) :calls))))
      (should-not (plist-get row :truncated))
      (should-not (plist-get row :partial))))
  :doc "a partial snapshot cannot disguise a real helper error as a successful delivery"
  (progn
    (with-temp-file (file-name-concat directory "huge.txt") (insert (make-string 600000 ?x)))
    (with-temp-file (file-name-concat directory "source.txt") (insert "small"))
    (let ((text (mevedel-test-summary--call state 'grep '(:root "workspace" :path "." :pattern "["))))
      (should (string-prefix-p "[Partial source snapshot:" text)))
    (let ((row (car (plist-get (mevedel-memory-investigation-summary state) :calls))))
      (should (plist-get row :partial))
      (should (eq 'error (plist-get row :status)))))
  :doc "ordinary search head limits set truncation without claiming a partial source snapshot"
  (progn
    (dotimes (index 105)
      (with-temp-file (file-name-concat directory (format "match-%03d.txt" index)) (insert "hit\n")))
    (mevedel-test-summary--call state 'glob '(:root "workspace" :path "." :pattern "*.txt"))
    (let ((row (car (plist-get (mevedel-memory-investigation-summary state) :calls))))
      (should (plist-get row :truncated))
      (should-not (plist-get row :partial))))
  :doc "rejected absolute targets and arbitrary commands never enter diagnostic metadata"
  (progn
    (mevedel-test-summary--call state 'read '(:root "workspace" :path "/PRIVATE-ABSOLUTE-PATH"))
    (mevedel-test-summary--call state 'git '(:command "PRIVATE-NON-GIT-COMMAND" :pattern "query"))
    (let* ((snapshot (mevedel-memory-investigation-summary state))
           (rows (plist-get snapshot :calls)))
      (should (equal "<invalid>" (plist-get (car rows) :investigation-path)))
      (should (equal "<invalid>" (plist-get (nth 1 rows) :investigation-command)))
      (should-not (string-search "PRIVATE-" (prin1-to-string snapshot)))))
  :doc "Git records the requested query and delivered hash without its commit message"
  (progn
    (should (= 0 (call-process "git" nil nil nil "-C" directory "init" "-q")))
    (should (= 0 (call-process "git" nil nil nil "-C" directory
                              "-c" "user.name=Test" "-c" "user.email=test@example.invalid"
                              "commit" "-q" "--allow-empty" "-m" "PRIVATE-COMMIT-MESSAGE")))
    (let* ((text (mevedel-test-summary--call state 'git '(:command "log" :pattern "PRIVATE")))
           (snapshot (mevedel-memory-investigation-summary state))
           (row (car (plist-get snapshot :calls))))
      (should (string-search "PRIVATE-COMMIT-MESSAGE" text))
      (should (equal "Git" (plist-get row :tool-name)))
      (should (equal "workspace" (plist-get row :investigation-root)))
      (should (equal "." (plist-get row :investigation-path)))
      (should (equal "log" (plist-get row :investigation-command)))
      (should (equal "PRIVATE" (plist-get row :investigation-pattern)))
      (should (= (string-bytes text) (plist-get row :result-bytes)))
      (should-not (string-search "PRIVATE-COMMIT-MESSAGE" (prin1-to-string snapshot)))))
  :doc "stopping an actual running search records cancellation and suppresses late delivery"
  (let* ((bin (file-name-concat directory "bin"))
         (marker (file-name-concat directory "started"))
         delivered)
    (make-directory bin)
    (with-temp-file (file-name-concat bin "rg")
      (insert "#!/bin/sh\nprintf started > " (shell-quote-argument marker) "\nexec sleep 30\n"))
    (set-file-modes (file-name-concat bin "rg") #o755)
    (let ((exec-path (cons bin exec-path)))
      (mevedel-memory-investigation-call
       state 'glob '(:root "workspace" :path "." :pattern "*")
       (lambda (_) (setq delivered t))))
    (with-timeout (10 (ert-fail "Search did not start"))
      (while (not (file-exists-p marker)) (accept-process-output nil 0.01)))
    (should (eq 'pending (plist-get (car (plist-get (mevedel-memory-investigation-summary state) :calls)) :status)))
    (mevedel-memory-investigation-stop state)
    (accept-process-output nil 0.02)
    (let ((row (car (plist-get (mevedel-memory-investigation-summary state) :calls))))
      (should (eq 'cancelled (plist-get row :status)))
      (should (= 0 (plist-get row :result-bytes)))
      (should-not (plist-get row :result-hash)))
    (should-not delivered))
  :doc "result budget refusal has zero delivery and retirement cannot add a later audit row"
  (progn
    (with-temp-file (file-name-concat directory "source.txt") (insert "not delivered"))
    (setf (mevedel-memory-investigation-bytes state) 65536)
    (mevedel-memory-investigation-call state 'read '(:root "workspace" :path "source.txt")
                                       (lambda (_) (ert-fail "Over-budget result delivered")))
    (let* ((snapshot (mevedel-memory-investigation-summary state))
           (row (car (plist-get snapshot :calls))))
      (should (eq 'output-limit (plist-get row :status)))
      (should (= 0 (plist-get row :result-bytes)))
      (should-not (plist-get row :result-hash))
      (mevedel-memory-investigation-call state 'read nil (lambda (_) (ert-fail "Retired call delivered")))
      (should (equal snapshot (mevedel-memory-investigation-summary state))))))

(mevedel-deftest mevedel-memory-investigation-summary/persistence
    (:quiet t
     :vars* ((directory (make-temp-file "mevedel-summary-publish-" t))
             (workspace (mevedel-workspace--create :root directory))
             (mevedel-memory-dirs nil)
             (gptel--known-backends nil)
             (model (make-symbol "summary-model"))
             (backend (gptel-make-openai "summary-test" :key "unused" :models (list model)))
             (real-request (symbol-function 'gptel-request))
             (reply "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n## No action\n- No supported changes.")
             claim handle result fsm callback)
     :before-each ((put model :context-window 128) (put model :capabilities '(tool-use))
                   (mevedel-workspace-identity-ensure directory))
     :after-each ((when handle (funcall (plist-get handle :cancel)))
                  (when claim (mevedel-journal-claim-settle claim 'cancelled ""))
                  (delete-directory directory t)))
  (progn
    (with-temp-file (file-name-concat directory "source.txt") (insert "UNLOGGED-DELIVERED-BODY\n"))
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (&rest _) (list :backend backend :model model)))
              ((symbol-function 'gptel-request)
               (lambda (prompt &rest args)
                 (let ((request (apply real-request prompt (plist-put (copy-sequence args) :dry-run t))))
                   (unless (plist-get args :dry-run)
                     (setq fsm request callback (plist-get args :callback)))
                   request))))
      (setq handle (mevedel-memory-review-request
                    (mevedel-memory-scope-capture workspace) nil
                    (lambda (value) (setq result value)) :memory-only t))
      ;; Use the request-local native tool function, not a synthetic summary.
      (let ((tool (car (plist-get (gptel-fsm-info fsm) :tools))))
        (funcall (gptel-tool-function tool) #'ignore "workspace" "./source.txt" 1 1))
      ,test))
  (test)
  :doc "review DONE retains a metadata-only snapshot without changing the accepted bundle schema"
  (progn
    (funcall callback reply (gptel-fsm-info fsm))
    (gptel--fsm-transition fsm 'DONE)
    (should (eq 'success (plist-get result :outcome)))
    (should (equal (plist-get result :investigation)
                   (plist-get (funcall (plist-get handle :usage)) :investigation)))
    (setq claim (mevedel-journal-claim-acquire
                 (mevedel-journal-store-claim-directory (mevedel-journal-store-directory directory) 'consolidation) 180))
    (let* ((prepared (mevedel-memory-store-prepare workspace claim (plist-get result :scope) nil ""))
           (published (mevedel-memory-pass--publish
                       workspace (list :prepared prepared :reply reply :model "test:model")))
           (accepted (mevedel-memory-store-accepted workspace (plist-get prepared :id)))
           (text (with-temp-buffer
                   (insert-file-contents (file-name-concat (mevedel-memory-store--directory workspace (plist-get prepared :id)) "accepted.el"))
                   (buffer-string))))
      (should (plist-get published :ok))
      (should (= 1 (length (plist-get (plist-get result :investigation) :calls))))
      (should-not (string-search "UNLOGGED-DELIVERED-BODY" text))
      (should-not (plist-member accepted :investigation))
      (should-not (plist-member (plist-get accepted :review) :investigation))
      (should-not (plist-member (plist-get published :entry) :investigation))
      (should-not (plist-get accepted :proposals))))
  :doc "cancel settlement preserves observed delivery without inventing a successful review"
  (progn
    (funcall (plist-get handle :cancel))
    (should (eq 'aborted (plist-get result :outcome)))
    (should (= 1 (length (plist-get (plist-get result :investigation) :calls))))
    (should-not (plist-get result :reply))
    (funcall callback reply (gptel-fsm-info fsm))
    (should (eq 'aborted (plist-get result :outcome)))))

(mevedel-deftest mevedel-memory-investigation-summary/pass
    (:quiet t
     :vars* ((directory (make-temp-file "mevedel-summary-pass-" t))
             (workspace (mevedel-workspace--create :root directory))
             (mevedel-memory-dirs nil)
             (mevedel-memory-consolidation-mode 'manual)

             (mevedel-telemetry-enabled t)
             (gptel--known-backends nil)
             (model (make-symbol "summary-pass-model"))
             (backend (gptel-make-openai "summary-pass-test" :key "unused" :models (list model)))
             (real-request (symbol-function 'gptel-request))
             (real-supported (symbol-function 'mevedel-journal-worker-supported-p))
             (real-worker (symbol-function 'mevedel-journal-worker-start))
             (reply (concat "## Promote\n- none\n## Update\n- none\n## Merge\n- none\n## Remove\n- none\n## Instructions\n- none\n"
                            "## Notes\n````proposal\nnote: \"work://shared/note.md\"\ndisposition: \"delete\"\n"
                            "reason: \"Nothing qualifies for extraction\"\n---\n````\n"
                            "## No action\n- none"))
             worker-enabled worker-operations state result fsm callback)
     :before-each ((put model :context-window 128) (put model :capabilities '(tool-use)))
     :after-each ((mevedel-memory-pass-cancel workspace)
                  (delete-directory directory t)))
  (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
             (lambda (&rest _) (list :backend backend :model model)))
            ((symbol-function 'gptel-request)
             (lambda (prompt &rest args)
               (let ((request (apply real-request prompt (plist-put (copy-sequence args) :dry-run t))))
                 (unless (plist-get args :dry-run)
                   (setq fsm request callback (plist-get args :callback)))
                 request)))
            ((symbol-function 'mevedel-journal-worker-supported-p)
             (lambda (workspace) (and worker-enabled (funcall real-supported workspace))))
            ((symbol-function 'mevedel-journal-worker-start)
             (lambda (workspace operation &rest args)
               (push operation worker-operations)
               (apply real-worker workspace operation args))))
    (cl-labels
        ((run (worker &optional terminal)
           (setq worker-enabled worker)
           (with-temp-file (file-name-concat directory "source.txt") (insert "PRIVATE-PASS-RESULT\n"))
           (make-directory (file-name-concat directory ".mevedel" "shared") t)
           (with-temp-file (file-name-concat directory ".mevedel" "shared" "note.md")
             (insert "A completed observation.\n"))
           ;; Target clocks can have whole-second precision.  A just-written
           ;; subsecond mtime must not accidentally look newer than that clock.
           (set-file-times (file-name-concat directory ".mevedel" "shared" "note.md")
                           (time-subtract (current-time) (* 15 86400)))
           (setq state (mevedel-memory-pass-start workspace (lambda (value) (setq result value)) :memory-only t))
           (with-timeout (20 (ert-fail "Pass did not start its review"))
             (while (and state (not fsm) (not result)) (accept-process-output nil 0.01)))
           (should state)
           (should fsm)
           (funcall (gptel-tool-function (car (plist-get (gptel-fsm-info fsm) :tools)))
                    #'ignore "workspace" "./source.txt" 1 1)
           (let ((observed (plist-get (funcall (plist-get (plist-get state :request) :usage)) :investigation)))
             (should (= 1 (length (plist-get observed :calls))))
             (should (equal '("work://shared/note.md")
                            (plist-get (funcall (plist-get (plist-get state :request) :usage)) :investigation-notes)))
             (if (eq terminal 'cancel)
                 (mevedel-memory-pass-cancel workspace)
               (funcall callback (if (eq terminal 'error) "invalid reply" reply) (gptel-fsm-info fsm))
               (gptel--fsm-transition fsm 'DONE))
             (with-timeout (20 (ert-fail "Pass did not publish its result"))
               (while (not result) (accept-process-output nil 0.01)))
             (should (eq (pcase terminal ('cancel 'aborted) ('error 'error) (_ 'success)) (plist-get result :outcome)))
             (should (equal observed (plist-get result :investigation)))
             (unless terminal
               (let ((accepted (mevedel-memory-store-accepted workspace (plist-get result :id))))
                 (should-not (plist-member accepted :investigation))
                 (should-not (plist-member (plist-get accepted :review) :investigation))))
             ;; Read the actual append after both publication and sanitation.
             ;; Dropping --finish forwarding or any nested allowlisted field
             ;; fails these assertions, even when publication succeeds.
             (let ((path (file-name-concat directory ".mevedel" "state" "diagnostics" "telemetry-log.el")))
               (if (not mevedel-telemetry-enabled)
                   (should-not (file-exists-p path))
                 (let* ((events (with-temp-buffer
                                  (insert-file-contents path)
                                  (goto-char (point-min))
                                  (let ((read-eval nil) rows)
                                    (while (< (point) (point-max))
                                      (push (read (current-buffer)) rows)
                                      (skip-chars-forward " \t\r\n"))
                                    (nreverse rows))))
                        (event (cl-find (pcase terminal ('cancel 'memory-consolidation-killed)
                                                        ('error 'memory-consolidation-failed)
                                                        (_ 'memory-consolidation-completed))
                                        events :key (lambda (row) (plist-get row :event))))
                        (summary (plist-get event :investigation))
                        (row (car (plist-get summary :calls))))
                   (should (equal (plist-get result :id) (plist-get event :pass-id)))
                   (should (= 1 (length (plist-get summary :calls))))
                   (should (= 0 (plist-get summary :omitted-count)))
                   (should (equal "Read" (plist-get row :tool-name)))
                   (should (equal "source.txt" (plist-get row :investigation-path)))
                   (should (equal "workspace" (plist-get row :investigation-root)))
                   (should (eq 'delivered (plist-get row :status)))
                   (should (= (plist-get (car (plist-get observed :calls)) :result-bytes) (plist-get row :result-bytes)))
                   (should (equal (plist-get (car (plist-get observed :calls)) :result-hash) (plist-get row :result-hash)))
                   (should (equal '("work://shared/note.md") (plist-get event :investigation-notes)))
                   (should-not (plist-get event :dropped-keys))
                   (should-not (string-search "PRIVATE-PASS-RESULT" (prin1-to-string event))))))
             (if (and worker (not terminal))
                 (should (memq 'memory-publish worker-operations))
               (unless worker (should-not worker-operations))))))
      ,test))
  (test)
  :doc "synchronous pass settlement appends the actual review metadata after deep sanitation"
  (run nil)
  :doc "real preparation and publication children retain the actual review's delivery metadata"
  (run t)
  :doc "disabled telemetry still publishes the review but creates no diagnostic summary file"
  (let ((mevedel-telemetry-enabled nil)) (run nil))
  :doc "cancelled pass telemetry retains delivery and admitted note identity without accepting a review"
  (run nil 'cancel)
  :doc "failed review telemetry retains delivery and admitted note identity without accepting a review"
  (run nil 'error)
  :doc "negative control: omitting terminal summary forwarding fails the durable call-count assertion"
  (let ((emitter (symbol-function 'mevedel-telemetry-record-workspace)) injected)
    (cl-letf (((symbol-function 'mevedel-telemetry-record-workspace)
               (lambda (workspace event &rest props)
                 (when (eq event 'memory-consolidation-completed)
                   (setq injected (and (plist-get props :investigation) t))
                   (setq props (plist-put props :investigation nil)))
                 (apply emitter workspace event props))))
      (let ((failure (should-error (run nil) :type 'ert-test-failed)))
        (should injected)
        (should (equal '(= 1 0) (plist-get (cdadr failure) :form))))))
  :doc "negative control: omitting a nested allowlist key fails the durable target assertion"
  (let ((mevedel-telemetry--allowed-keys (remq :investigation-path mevedel-telemetry--allowed-keys)))
    (let ((failure (should-error (run nil) :type 'ert-test-failed)))
      (should (equal '(equal "source.txt" nil) (plist-get (cdadr failure) :form))))))

(provide 'test-mevedel-memory-investigation-summary)
;;; test-mevedel-memory-investigation-summary.el ends here
