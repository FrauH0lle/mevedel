;;; consolidation.el -- Opt-in proposal quality evaluation -*- lexical-binding: t -*-

;;; Commentary:

;; Extend the accepted digest cases through real memory review and checked writes.
;; Expectations remain outside the model input. All writes use temporary roots.

;;; Code:

(load (file-name-concat (file-name-directory load-file-name) "run.el") nil t)
(ert-delete-test 'mevedel-memory-quality-real-provider)
(require 'mevedel-memory-pass)
(require 'mevedel-system)

(defconst mevedel-quality--prior-memory
  '(("user-correction" . "Production uses MariaDB 11. Logical replication is unavailable for the production export.")
    ("test-fixed" . "NULL cache keys fail to round-trip on SQLite. This test failure remains unresolved.")
    ("abandoned-decision" . "Deploy Redis for parser results across workers. It should reduce total request latency by 30 percent.")
    ("buried-lesson" . nil)
    ("repeated-summary" . "Three independent ext4 power-loss tests confirmed fsync before rename. Treat this as broadly verified storage guidance.")
    ("journal-missing" . "SQLite 3.46 cache tests passed 8/8 after explicit None handling. Empty-string behavior is unchanged. PostgreSQL integration has not been run.")
    ("journal-failed" . "SQLite 3.46 cache tests passed 8/8 after explicit None handling. Empty-string behavior is unchanged. PostgreSQL integration has not been run."))
  "Synthetic existing guidance; deliberate stale claims challenge consolidation.")

(defun mevedel-quality--digest-result (name)
  "Read NAME's accepted stage-one digest artifact without its review notes."
  (let ((directory (or (getenv "MEVEDEL_QUALITY_DIGESTS")
                       (file-name-concat mevedel-quality--directory "sol-production"))))
    (with-temp-buffer
      (insert-file-contents (file-name-concat directory (concat name ".md")))
      (goto-char (point-min))
      (unless (search-forward "## Model result\n\n" nil t) (error "Missing stage-one model result"))
      (let ((start (point)))
        (unless (search-forward "\n\n## Retrieval" nil t) (error "Missing stage-one result boundary"))
        (string-trim (buffer-substring-no-properties start (match-beginning 0)))))))

(defun mevedel-quality--consolidate (case mode)
  "Run CASE through real review in MODE and return synthetic evidence only.
Manual mode simulates accepting every proposal to exercise the application
path; this is fixture execution, not a claim of human semantic approval."
  (let* ((root (make-temp-file "mevedel-consolidation-quality-" t))
         (memory (file-name-concat root "memory"))
         (workspace (mevedel-workspace--create :root root))
         (name (plist-get case :name))
         (prior (cdr (assoc name mevedel-quality--prior-memory)))
         (mevedel-memory-dirs (list memory))
         (mevedel-memory-consolidation-mode mode)
         (review-request (symbol-function 'mevedel-memory-review-request))
         (parse-reply (symbol-function 'mevedel-memory-proposal-parse))
         (encode-input (symbol-function 'mevedel-memory-review--input))
         (start (float-time)) (deadline (+ start 195))
         done result reply input references decisions files diagnostics digest)
    (unwind-protect
        (progn
          (mevedel-workspace-identity-ensure root)
          (make-directory memory)
          (write-region "Never deploy. Never log raw credentials.\n" nil (file-name-concat root "AGENTS.md") nil 'silent)
          (when prior
            (write-region (concat prior "\n") nil (file-name-concat memory "topic.md") nil 'silent)
            (write-region "- [Existing guidance](topic.md) - Current project context\n" nil
                          (file-name-concat memory "MEMORY.md") nil 'silent))
          (unless (plist-get case :purpose)
            (setq digest (mevedel-quality--digest-result name))
            (let ((id (secure-hash 'sha256 name)))
              (mevedel-journal-store-publish-digest
               root (list :capture-id id :session name :session-name name
                          :workspace (mevedel-workspace-identity-read root)
                          :trigger 'session-end :segment 1 :source-revision id
                          :turns '(1) :turn-ids (list id) :created "2026-09-07T13:49:00Z"
                          :model "Codex:gpt-5.6-sol") digest)))
          (mevedel-test--with-captured-diagnostics diagnostics
            (cl-letf (((symbol-function 'mevedel-memory-review-request)
                       (lambda (scope entries callback &rest args)
                         (apply review-request scope entries
                                (lambda (value)
                                  (setq reply (or (plist-get value :reply) reply (plist-get value :error))
                                        references (plist-get value :references))
                                  (funcall callback value)) args)))
                      ((symbol-function 'mevedel-memory-proposal-parse)
                       (lambda (text &rest args)
                         (setq reply text)
                         (apply parse-reply text args)))
                      ((symbol-function 'mevedel-memory-review--input)
                       (lambda (&rest args) (setq input (apply encode-input args)))))
              (mevedel-memory-pass-start workspace (lambda (value) (setq result value done t)) :memory-only t)
              (while (and (not done) (< (float-time) deadline))
                (accept-process-output nil 0.1))
              (unless done
                (mevedel-memory-pass-cancel workspace)
                (setq result '(:outcome error :error "Evaluation deadline exceeded")))
              (when (and (eq mode 'manual) (eq (plist-get result :outcome) 'success))
                (let* ((pass (plist-get result :id))
                       (accepted (mevedel-memory-store-accepted workspace pass)))
                  (dolist (proposal (plist-get accepted :proposals))
                    (condition-case err
                        (mevedel-memory-decision-apply workspace pass (plist-get proposal :id))
                      (error (push (list :error (error-message-string err)) decisions))))))))
          (dolist (entry (mevedel-journal-store-entries root))
            (when (eq (plist-get entry :kind) 'decision)
              (push (list :proposal (plist-get entry :proposal-id) :status (plist-get entry :status)) decisions)))
          (dolist (path (directory-files-recursively memory "\\.md\\'"))
            (push (cons (file-relative-name path memory) (mevedel-session-control-fs-read-file path)) files))
          (list :outcome (plist-get result :outcome) :error (plist-get result :error)
                :elapsed (- (float-time) start) :input-tokens (plist-get result :input-tokens)
                :cached-tokens (plist-get result :cached-tokens) :output-tokens (plist-get result :output-tokens)
                :updated-files (plist-get result :updated-files) :held (plist-get result :held-count)
                :unapplied (plist-get result :unapplied-count) :input input :reply reply
                :references references :before prior :digest digest :decisions (nreverse decisions)
                :after (nreverse files) :instructions (mevedel-session-control-fs-read-file (file-name-concat root "AGENTS.md"))
                :diagnostics diagnostics))
      (mevedel-memory-pass-cancel workspace)
      (delete-directory root t))))

(ert-deftest mevedel-memory-quality-consolidation-real-provider ()
  (let* ((config-path (getenv "MEVEDEL_QUALITY_PROVIDER"))
         (config (with-temp-buffer (insert-file-contents config-path) (read (current-buffer))))
         (model (if-let* ((name (getenv "MEVEDEL_QUALITY_MODEL"))) (intern name) (plist-get config :model)))
         (backend (apply (pcase (plist-get config :type)
                           ('gptel-deepseek #'gptel-make-deepseek)
                           ('gptel-openai-oauth #'gptel-make-openai-oauth)
                           (_ (error "Unsupported quality backend")))
                         (plist-get config :name) :models (plist-get config :models)
                         (plist-get config :backend-options)))
         (policy (list :backend backend :model model :effort (plist-get config :effort)))
         (gptel-stream (plist-get config :stream))
         (output (file-name-concat mevedel-quality--directory (or (getenv "MEVEDEL_QUALITY_RESULTS") "consolidation")))
         failures)
    (unwind-protect
        (progn
          (unless (assq model (plist-get config :models)) (error "Evaluation model is not registered"))
          (make-directory output t)
          (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
                     (lambda (workload &rest _)
                       (unless (eq workload 'memory) (error "Unexpected evaluation workload")) policy)))
            (dolist (case (mevedel-quality--cases))
              (dolist (mode '(manual auto))
                (let* ((name (plist-get case :name))
                       (evidence (mevedel-quality--consolidate case mode))
                       (path (file-name-concat output (format "%s-%s.md" name mode))))
                  (with-temp-file path
                    (insert (format "# %s / %s\n\nModel: %s / %s; workload: memory; effort: %S\n\nPrompt SHA256: %s\n\nOutcome: %S; elapsed seconds: %.2f\n\nInput tokens: %S; cached input: %S; output tokens: %S\n\nConfigured price per million tokens: input %S; output %S (not a billing receipt).\n\nHuman judgment: PENDING\n\nReview question: %s\n\n"
                                    name mode (plist-get config :name) model (plist-get policy :effort)
                                    (secure-hash 'sha256 (mevedel-system-render-prompt-file "prompts/memory/consolidation.md"))
                                    (plist-get evidence :outcome) (plist-get evidence :elapsed)
                                    (plist-get evidence :input-tokens) (plist-get evidence :cached-tokens) (plist-get evidence :output-tokens)
                                    (get model :input-cost) (get model :output-cost) (plist-get case :review)))
                    (insert "Manual application is simulated acceptance in a temporary fixture. Auto runs the production mode. Both variants apply instruction proposals.\n\n"
                            "## Prior memory\n\n" (or (plist-get evidence :before) "No existing topic.")
                            "\n\n## Admitted request input\n\n" (or (plist-get evidence :input) "Unavailable")
                            "\n\n## Model reply\n\n" (or (plist-get evidence :reply) (plist-get evidence :error) "Unavailable")
                            "\n\n## Applied memory files\n\n")
                    (dolist (file (plist-get evidence :after))
                      (insert "### " (car file) "\n\n" (cdr file) "\n"))
                    (insert (format "\n## Decisions\n\n%S\n\nUpdated files: %S; held: %S; unapplied: %S\n\n## Instructions after review\n\n%s\n"
                                    (plist-get evidence :decisions) (plist-get evidence :updated-files)
                                    (plist-get evidence :held) (plist-get evidence :unapplied) (plist-get evidence :instructions))))
                  (unless (eq (plist-get evidence :outcome) 'success) (push (cons name mode) failures)))))))
      (delete-file config-path)
      (setq config nil policy nil backend nil))
    (when failures (ert-fail (format "Consolidation requests failed: %S" failures)))))

(provide 'mevedel-memory-quality-consolidation)
;;; consolidation.el ends here
