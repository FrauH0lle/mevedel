;;; run.el -- Opt-in real-provider memory quality cases -*- lexical-binding: t -*-

;;; Commentary:

;; Run with Eask's ERT command, never with the ordinary test wildcard.
;; MEVEDEL_QUALITY_PROVIDER names a mode-0600 temporary provider configuration.
;; Results contain synthetic evidence only. Provider credentials are never saved.

;;; Code:

(when-let* ((directory (getenv "MEVEDEL_QUALITY_GPTEL")))
  (setq load-prefer-newer t)
  (add-to-list 'load-path directory))

(require 'helpers (file-name-concat (locate-dominating-file load-file-name "Eask")
                                   "test" "helpers"))
(require 'gptel-openai-extras)
(require 'gptel-openai-oauth)
(require 'mevedel-context-summary)
(require 'mevedel-journal-store)
(require 'mevedel-resource)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-workspace)

(defconst mevedel-quality--directory
  (if-let* ((directory (getenv "MEVEDEL_QUALITY_OUTPUT")))
      (expand-file-name directory)
    (file-name-concat (locate-dominating-file load-file-name "Eask")
                      ".scratch" "memory-quality")))

(defun mevedel-quality--request (source purpose policy)
  "Run one real bounded request over SOURCE for PURPOSE with POLICY."
  (let* ((deadline (+ (float-time) 120))
         done result cancel diagnostics)
    (unwind-protect
        (progn
          (mevedel-test--with-captured-diagnostics diagnostics
            (setq cancel (mevedel-context-summary-generate
                          source purpose (lambda (value) (setq result value done t))
                          :policy policy))
            (while (and (not done) (< (float-time) deadline))
              (accept-process-output nil 0.1)))
          (unless done
            (setq result '(:outcome error :error-class timeout
                          :error "Quality request timed out")))
          (append result (list :diagnostics diagnostics)))
      (unless done (funcall cancel)))))

(defun mevedel-quality--grep (workspace query)
  "Use the ordinary journal Grep handler in WORKSPACE for QUERY."
  (let* ((args (list :path "memory://journal/" :pattern query :output_mode "content"))
         (attempt (mevedel-resource-prepare 'grep "memory://journal/"
                                            (list :workspace workspace :args args)))
         (mevedel-resource-current-attempts (list (cons "memory://journal/" attempt)))
         (deadline (+ (float-time) 20))
         done result)
    (unwind-protect
        (progn
          (mevedel-tool-fs-search-grep
           (lambda (value) (setq result (plist-get value :result) done t)) args)
          (while (and (not done) (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (unless done (error "Journal retrieval timed out"))
          result)
      (mevedel-resource-discard-attempts (list attempt)))))

(defun mevedel-quality--cases ()
  "Return synthetic cases and review questions; expectations are not model input."
  (list
   (list :name "user-correction" :query "logical replication"
         :review "Does the digest preserve PostgreSQL 16 and the staging exception, with MariaDB explicitly superseded?"
         :source (concat
                  "[user-correction turn 1, model inference] I assume production uses MariaDB 11, so logical replication is unavailable.\n"
                  "[user-correction turn 2, user] Correction: production uses PostgreSQL 16. MariaDB is only in a disposable staging fixture. Use logical replication for the production export.\n"
                  "[user-correction turn 3, observed file config/production.env] DB_ENGINE=postgresql; DB_MAJOR=16.\n"
                  "[user-correction turn 4, model] Updated export design to PostgreSQL logical replication. No production migration was run.\n"))
   (list :name "test-fixed" :query "NULL|null"
         :review "Is the NULL-key failure resolved for SQLite 3.46, with PostgreSQL coverage still unverified?"
         :source (concat
                  "[test-fixed turn 1, user] Fix NULL cache-key handling without changing empty-string behavior.\n"
                  "[test-fixed turn 2, observed command] pytest -q tests/test_cache.py on SQLite 3.46: FAILED test_null_key_roundtrip (expected None, got empty string); 7 passed, 1 failed.\n"
                  "[test-fixed turn 3, observed edit cache.py] Replaced key or '' with an explicit key is None branch; empty strings retain their original branch.\n"
                  "[test-fixed turn 4, observed command] pytest -q tests/test_cache.py on SQLite 3.46: 8 passed. test_null_key_roundtrip and test_empty_key_roundtrip both passed.\n"
                  "[test-fixed turn 5, model] PostgreSQL integration was not run; no evidence about that backend.\n"))
   (list :name "abandoned-decision" :query "Redis"
         :review "Is Redis described as abandoned after measurement, with no standing instruction to deploy it?"
         :source (concat
                  "[abandoned-decision turn 1, model proposal] Add Redis to store parser results across workers; estimate a 30% latency reduction.\n"
                  "[abandoned-decision turn 2, observed benchmark parser-bench.json] Parsing accounts for 0.7% of total request latency across 500 requests. No Redis benchmark was performed.\n"
                  "[abandoned-decision turn 3, user] Abandon Redis. The measured parsing cost cannot justify a new service. Keep the existing process-local dictionary.\n"
                  "[abandoned-decision turn 4, observed edit design/parser.md] Removed the Redis deployment proposal and recorded the 0.7% measurement.\n"))
   (list :name "buried-lesson" :query "CLOCK_MONOTONIC"
         :review "Can ordinary journal search recover the CLOCK_MONOTONIC lesson and its turn-503 source among routine output?"
         :source (concat
                  "[buried-lesson turn 1, user] Diagnose intermittent timeout failures. Preserve the condition that explains the fix.\n"
                  (mapconcat (lambda (n) (format "[buried-lesson tool output %d] cache hit; request completed in 12ms; no warnings.\n" n))
                             (number-sequence 2 502) "")
                  "[buried-lesson turn 503, observed reproducer clock-jump.sh] Moving wall time backward by 30 seconds kept a 2-second retry loop alive for 32 seconds.\n"
                  "[buried-lesson turn 504, observed edit retry.c] Compute retry deadlines using CLOCK_MONOTONIC, because elapsed time must not depend on wall-clock adjustments.\n"
                  "[buried-lesson turn 505, observed reproducer clock-jump.sh] With the same backward clock jump, retry ended after 2 seconds.\n"
                  (mapconcat (lambda (n) (format "[buried-lesson tool output %d] cache hit; request completed in 11ms; no warnings.\n" n))
                             (number-sequence 506 1005) "")))
   (list :name "repeated-summary" :query "fsync"
         :review "Is the fsync result one inherited observation with no independent rerun, and is the new spelling-only edit distinguished?"
         :source (concat
                  "[repeated-summary prior compaction A, inherited context only] Observed in original turn 12: adding fsync before rename survived one power-loss simulation on ext4. Other filesystems untested.\n"
                  "[repeated-summary prior compaction B, inherited context only] Summary of compaction A: fsync before rename survived one ext4 power-loss simulation from original turn 12.\n"
                  "[repeated-summary prior compaction C, inherited context only] Repeats original turn 12 via compaction B: fsync before rename passed the ext4 simulation.\n"
                  "[repeated-summary current turn 21, user] Those are copies of the same test, not three independent confirmations. We have not rerun it.\n"
                  "[repeated-summary current turn 22, observed edit docs/storage.md] Fixed spelling only; no implementation change or test execution.\n"))
   (list :name "journal-missing" :purpose 'continuation :query ""
         :review "Without any journal, does continuation retain the hard constraint, decisive test result, and remaining PostgreSQL work?"
         :source (concat
                  "[journal-missing turn 1, user] Fix NULL cache keys. Never log raw credentials. Do not deploy.\n"
                  "[journal-missing turn 2, observed edit cache.py] Explicit None handling added; empty-string semantics preserved.\n"
                  "[journal-missing turn 3, observed command] pytest -q tests/test_cache.py: 8 passed on SQLite 3.46. PostgreSQL suite not run.\n"
                  "[journal-missing turn 4, observed filesystem] Workspace .mevedel/journal/ does not exist.\n"
                  "[journal-missing turn 5, user] Next, check PostgreSQL integration, then report results.\n"))
   (list :name "journal-failed" :purpose 'continuation :query ""
         :review "After an actual capped digest request fails, does continuation still preserve all decisive constraints/results?"
         :source (concat
                  "[journal-failed turn 1, user] Fix NULL cache keys. Never log raw credentials. Do not deploy.\n"
                  "[journal-failed turn 2, observed command] pytest -q tests/test_cache.py: 8 passed on SQLite 3.46 after explicit None handling. Empty-string behavior is unchanged.\n"
                  "[journal-failed turn 3, model] PostgreSQL integration is not yet tested.\n"
                  "[journal-failed turn 4, user] Next, check PostgreSQL integration, then report results.\n"))))

(ert-deftest mevedel-memory-quality-real-provider ()
  (let* ((config-path (getenv "MEVEDEL_QUALITY_PROVIDER"))
         (config (with-temp-buffer (insert-file-contents config-path) (read (current-buffer))))
         (model (if-let* ((name (getenv "MEVEDEL_QUALITY_MODEL")))
                    (intern name)
                  (plist-get config :model)))
         (backend (apply (pcase (plist-get config :type)
                           ('gptel-deepseek #'gptel-make-deepseek)
                           ('gptel-openai-oauth #'gptel-make-openai-oauth)
                           (_ (error "Unsupported quality backend")))
                         (plist-get config :name)
                         :models (plist-get config :models)
                         (plist-get config :backend-options)))
         (policy (list :backend backend :model model :effort (plist-get config :effort)))
         (gptel-stream (plist-get config :stream))
         (root (make-temp-file "mevedel-memory-quality-workspace-" t))
         (workspace (mevedel-workspace--create :type 'project :id root :root root :name "Quality fixtures"))
         (results-directory (file-name-concat mevedel-quality--directory
                                             (or (getenv "MEVEDEL_QUALITY_RESULTS") "results")))
         failures)
    (unwind-protect
        (progn
          (should (assq model (plist-get config :models)))
          (make-directory results-directory t)
          (dolist (case (mevedel-quality--cases))
            (let* ((name (plist-get case :name))
                   (purpose (or (plist-get case :purpose) 'digest))
                   (source (plist-get case :source))
                   (case-policy (if (eq purpose 'digest)
                                    (if-let* ((effort (getenv "MEVEDEL_QUALITY_DIGEST_EFFORT")))
                                        (plist-put (copy-sequence policy) :effort (intern effort))
                                      policy)
                                  policy))
                   (case-root (file-name-concat root name))
                   (start (float-time))
                   failed-digest result retrieval address)
              (make-directory case-root t)
              (setf (mevedel-workspace-root workspace) case-root)
              (should-not (file-exists-p (mevedel-journal-store-directory case-root)))
              (when (equal name "journal-failed")
                ;; A tiny byte allowance also exercises failure on providers
                ;; without a server token control, using the real callback.
                (let ((mevedel-context-summary--digest-max-bytes 1))
                  (setq failed-digest
                        (mevedel-quality--request source 'digest
                                                 (append '(:max-tokens 1) policy)))))
              (setq result (mevedel-quality--request
                            source purpose
                            case-policy))
              (when (and (eq purpose 'digest) (eq 'success (plist-get result :outcome)))
                (let* ((hash (secure-hash 'sha256 source))
                       (entry (mevedel-journal-store-publish-digest
                               case-root
                               (list :capture-id hash :session name :session-name name
                                     :workspace (secure-hash 'sha256 "quality-workspace")
                                     :trigger 'session-end :segment 1 :source-revision hash
                                     :turns '(1) :turn-ids (list hash)
                                     :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                                     :model (format "%s:%s" (plist-get config :name) model))
                               (plist-get result :summary))))
                  (setq address (concat "memory://journal/" (mevedel-resource-encode-component (plist-get entry :file)))
                        retrieval (mevedel-quality--grep workspace (plist-get case :query)))))
              (with-temp-file (file-name-concat results-directory (concat name ".md"))
                (insert (format "# %s\n\nModel: %s / %s; effort: %S\n\nPurpose: %s\n\nPrompt SHA256: %s\n\nElapsed seconds: %.2f\n\nOutcome: %S\n\nInput tokens: %S; output tokens: %S\n\nConfigured costs per million tokens: input %S; output %S (configuration values, not a billing receipt).\n\nHuman judgment: PENDING\n\nReview question: %s\n\n"
                                name (plist-get config :name) model
                                (plist-get case-policy :effort)
                                purpose (secure-hash 'sha256 (mevedel-context-summary--prompt purpose))
                                (- (float-time) start) (plist-get result :outcome)
                                (plist-get result :input-tokens) (plist-get result :output-tokens)
                                (get model :input-cost) (get model :output-cost)
                                (plist-get case :review)))
                (when failed-digest
                  (insert (format "Injected one-byte/one-token digest allowance outcome: %S; error class: %S; input tokens: %S; output tokens: %S\n\n"
                                  (plist-get failed-digest :outcome) (plist-get failed-digest :error-class)
                                  (plist-get failed-digest :input-tokens) (plist-get failed-digest :output-tokens))))
                (insert "Limit mode: unmodified production admission; supported server caps, client deadline and digest byte limit. Codex has no server token cap.\n\n")
                (unless (string-empty-p (or (plist-get result :diagnostics) ""))
                  (insert (format "Captured provider diagnostics: %S\n\n"
                                  (plist-get result :diagnostics))))
                (when failed-digest
                  (insert (format "Failed-digest diagnostic: %S\n\n"
                                  (plist-get failed-digest :error))))
                (insert (format "Cached input tokens: %S (separate from uncached input above).\n\n"
                                (plist-get result :cached-tokens)))
                (insert (format "gptel request source: %s\n\n"
                                (symbol-file 'gptel-request 'defun)))
                (insert "## Model result\n\n" (or (plist-get result :summary) (plist-get result :error) "No result")
                        "\n\n## Retrieval\n\nQuery: `" (plist-get case :query) "`\n\n"
                        (or address (if (eq purpose 'digest) "Digest unavailable"
                                      "Continuation has no journal dependency")) "\n\n"
                        (or retrieval "Not applicable") "\n\n## Frozen input\n\n" source))
              (unless (and (eq 'success (plist-get result :outcome))
                           (or (null failed-digest) (eq 'error (plist-get failed-digest :outcome)))
                           (or (not (eq purpose 'digest))
                               (and address retrieval
                                    (string-match-p (regexp-quote address) retrieval))))
                (push name failures))))
          (should-not failures))
      ;; Do not leave provider credentials or real model registrations behind.
      (delete-file config-path)
      (delete-directory root t)
      (mevedel-workspace-clear-registry))))

(provide 'mevedel-memory-quality-run)
;;; run.el ends here
