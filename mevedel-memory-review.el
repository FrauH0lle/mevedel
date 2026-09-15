;;; mevedel-memory-review.el -- Sessionless consolidation requests -*- lexical-binding: t -*-

;;; Commentary:

;; Prepares bounded model input and runs one read-only consolidation request.
;; Consumers own candidate selection, evidence pins, ownership, publication,
;; and proposal decisions. Success here is validated text, not publication.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'gptel-request)
(require 'mevedel-context-summary)
(require 'mevedel-memory-investigation)
(require 'mevedel-memory-proposal)
(require 'mevedel-memory-reference)
(require 'mevedel-models)
(require 'mevedel-reminders)

;; `mevedel-structs'
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file "mevedel-system" (relative-path &optional replacements))
(autoload 'mevedel-system-render-prompt-file "mevedel-system")

(defcustom mevedel-memory-review-max-tokens 64000
  "Cumulative output token budget for one memory review.
Both the client estimate and reported provider usage must fit this budget.
Reasoning, intermediate replies, and normalized tool arguments share it
with the final reply.  Supported provider limits use the remaining budget.
Captured when the request starts; must be a positive integer."
  :type 'natnum
  :group 'mevedel)

(defcustom mevedel-memory-review-max-bytes (* 256 1024)
  "Cumulative output byte budget for one memory review.
Includes reasoning, intermediate replies, and normalized tool arguments.
The final proposal reply has its own independent 32 KiB parser limit.
Captured when the request starts; must be a positive integer."
  :type 'natnum
  :group 'mevedel)

(defun mevedel-memory-review--input (scope entries documents references focus rejections omitted)
  "Encode admitted SCOPE, ENTRIES, DOCUMENTS, and REFERENCES as untrusted input.
FOCUS and REJECTIONS are bounded caller text. OMITTED counts unread digests."
  (decode-coding-string
   (gptel--json-encode
   (list
    :focus (or focus "") :recent_rejections (or rejections "")
    :roots
    (vconcat
     (mapcar (lambda (row)
               (let ((root (cdr row)))
                 (list :id (car row) :kind (symbol-name (plist-get root :kind))
                       :label (or (plist-get root :label) "Captured root")
                       :complete_names (if (plist-get root :complete) t :json-false)
                       :admitted_files (vconcat (plist-get root :files))
                       :existing_names (vconcat (plist-get root :existing)))))
             (plist-get scope :roots)))
    :digests (vconcat (mapcar (lambda (entry)
                              (list :id (plist-get entry :id) :session (plist-get entry :session)
                                    :created (plist-get entry :created) :body (plist-get entry :body))) entries))
    :documents (vconcat documents)
    :reference_checks (vconcat (plist-get references :references))
    :reference_omissions (vconcat (plist-get references :omissions))
    :omitted_digests omitted
    :document_guidance "Other admitted snapshots remain available through Read. Only fully supplied digests can enter this review's coverage."))
   'utf-8-unix))

(defun mevedel-memory-review--tokens (fsm)
  "Estimate the complete prepared payload in FSM, including tools and roles."
  (mevedel-context-summary--estimated-tokens
   "" (gptel--json-encode (plist-get (gptel-fsm-info fsm) :data))))

(cl-defun mevedel-memory-review-request
    (scope entries callback &key focus rejections memory-only (currentp (lambda () t)))
  "Run a bounded read-only review of captured SCOPE and candidate ENTRIES.
CALLBACK receives :outcome success/error/aborted and the admitted :entries,
:scope, :references, model :policy, and usage. Success also carries the exact
:reply, parsed :proposals, and :no-action. It never writes coverage
or proposals.

CURRENTP checks the consumer's current ownership generation. MEMORY-ONLY
permits an explicitly requested empty digest batch. FOCUS and REJECTIONS
are optional bounded text. Return a plist with :buffer for inspection,
an idempotent :cancel function, and a :usage function returning a snapshot
of provider usage and client output counters. The deadline is 180 seconds,
including input preparation.  The cumulative budgets are captured from
`mevedel-memory-review-max-tokens' and `mevedel-memory-review-max-bytes'.
Callback results and usage snapshots include :output-bytes and
:output-estimated-tokens for accumulated reply, reasoning and normalized
tool arguments; :result-bytes measures the current round's reply alone.
The breakdown is :reasoning-bytes, :reply-bytes (all rounds), and
:tool-call-bytes, plus admitted :tool-call-count and completed :rounds.
A guard failure adds :budget-kind and :output-limit, naming the guard and
its numeric threshold. Sparse reminders report remaining budgets at WAIT."
  (let* ((buffer (generate-new-buffer " *mevedel-memory-review*"))
         (caller (current-buffer))
         (stream gptel-stream)
         (token-budget mevedel-memory-review-max-tokens)
         (byte-budget mevedel-memory-review-max-bytes)
         (deadline (+ (float-time) 180))
         settled timer investigation policy references system input admitted response-start
         (documents nil) (chunks nil) (output "")
         (reply-bytes 0) budget-kind output-limit
         (reasoning-bytes 0) (reply-total-bytes 0) (tool-call-bytes 0)
         (completed-rounds 0) (last-completed-round -1) (reminder-level 0)
         (round 0) (recorded-round -1)
         (usage (list :input-tokens 0 :cached-tokens 0 :output-tokens 0)))
    (cl-labels
        ((live () (and (not settled) (< (float-time) deadline) (funcall currentp)))
         (finish (outcome &optional error parsed buffer-killed)
           (unless settled
             (setq settled t)
             (when timer (cancel-timer timer))
             (when investigation (mevedel-memory-investigation-stop investigation))
             (when (buffer-live-p buffer)
               (unless (eq outcome 'success) (ignore-errors (gptel-abort buffer)))
               (unless buffer-killed (kill-buffer buffer)))
             (with-current-buffer (if (buffer-live-p caller) caller (current-buffer))
               (funcall callback
                        (append (list :outcome outcome :error error
                                      :scope scope :entries admitted
                                      :references references :policy policy :omitted-digests (- (length entries) (length admitted)))
                                parsed (snapshot))))))
         (estimated ()
           (if (string-empty-p output) 0
             (mevedel-context-summary--estimated-tokens "" output)))
         (snapshot ()
           (append (list :output-bytes (string-bytes output)
                         :output-estimated-tokens (estimated)
                         :result-bytes reply-bytes
                         :reasoning-bytes reasoning-bytes :reply-bytes reply-total-bytes
                         :tool-call-bytes tool-call-bytes
                         :tool-call-count (if investigation (mevedel-memory-investigation-calls investigation) 0)
                         :rounds completed-rounds
                         :budget-kind budget-kind :output-limit output-limit)
                   (copy-sequence usage)))
         (check-limit (kind value limit)
           (when (> value limit)
             (setq budget-kind kind output-limit limit)
             (error "Review %s limit exceeded (%d > %d)" kind value limit)))
         (check-output ()
           (check-limit 'output-bytes (string-bytes output) byte-budget)
           (check-limit 'output-estimated-tokens (estimated) token-budget)
           (check-limit 'output-tokens (plist-get usage :output-tokens) token-budget))
         (ensure-live ()
           (unless (live) (error "Review deadline or ownership expired")))
         (show (text)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (save-excursion (goto-char (point-max)) (insert text)))))
         (charge (text kind)
           (setq output (concat output text))
           (pcase kind
             ('reasoning (cl-incf reasoning-bytes (string-bytes text)))
             ('reply (cl-incf reply-total-bytes (string-bytes text)))
             ('tool (cl-incf tool-call-bytes (string-bytes text)))))
         (record-usage (info &optional completed)
           (when (and completed (/= last-completed-round round))
             (setq last-completed-round round)
             (cl-incf completed-rounds))
           (unless (= recorded-round round)
             (when-let* ((tokens (plist-get info :tokens)))
               (setq recorded-round round)
               (cl-loop for (source . target) in '((:input . :input-tokens) (:cached . :cached-tokens) (:output . :output-tokens))
                        for value = (plist-get tokens source)
                        when (numberp value) do (cl-incf (plist-get usage target) value)))))
         (provider (response info)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (unless (plist-get info :stream)
                     (record-usage info response))
                   (pcase response
                     ((pred stringp)
                      (charge response 'reply)
                      (cl-incf reply-bytes (string-bytes response))
                      (push response chunks)
                      (show response))
                     (`(reasoning . ,(and (pred stringp) text))
                      (charge text 'reasoning) (show (concat "\n[Reasoning]\n" text)))
                     (`(tool-result . ,results)
                      (dolist (row results)
                        (show (format "\n\n[%s]\n%s\n" (gptel-tool-name (car row)) (nth 2 row)))))
                     ('t (record-usage info t)))
                   (check-output))
               (error (finish 'error (error-message-string err))))))
         (budget-reminder (fsm)
           ;; Like agent turn warnings, fire sparsely at request boundaries.
           ;; This request owns no session transcript or reminder queue.
           (when (> round 0)
             (let* ((tokens (max (estimated) (plist-get usage :output-tokens)))
                    (bytes (string-bytes output))
                    (calls (mevedel-memory-investigation-calls investigation))
                    (results (mevedel-memory-investigation-bytes investigation))
                    (seconds (max 0 (floor (- deadline (float-time)))))
                    (pressure (max (/ (* 100 tokens) token-budget)
                                   (/ (* 100 bytes) byte-budget)
                                   (/ (* 100 calls) mevedel-memory-investigation--max-calls)
                                   (/ (* 100 results) mevedel-memory-investigation--max-bytes)
                                   (/ (* 100 (- 180 seconds)) 180)))
                    (level (cond ((>= pressure 90) 90) ((>= pressure 75) 75) (t 0))))
               (when (> level reminder-level)
                 (let* ((body (format
                               "Memory review budget reminder (%d%% threshold). Remaining: %d output tokens (larger of client estimate and reported usage), %d accounted output bytes, %d tool calls, %d tool-result bytes, approximately %d seconds. %s Keep the final proposal reply within %d bytes. Unresolved claims remain unknown; No action is valid."
                               level (max 0 (- token-budget tokens)) (max 0 (- byte-budget bytes))
                               (max 0 (- mevedel-memory-investigation--max-calls calls))
                               (max 0 (- mevedel-memory-investigation--max-bytes results)) seconds
                               (if (= level 90)
                                   "Stop using tools and return the final review now."
                                 "Avoid new lines of investigation; finish essential checks and prepare the final review.")
                               mevedel-memory-proposal--max-bytes))
                        (block (mevedel-reminders-format-block body))
                        (info (gptel-fsm-info fsm))
                        (backend (plist-get info :backend)))
                   (with-current-buffer buffer
                     ;; Administrative messages must not accumulate provider
                     ;; cache breakpoints. The simple format also fits Bedrock.
                     (let ((gptel-cache nil))
                       (gptel--inject-prompt
                        backend (plist-get info :data)
                        (car (gptel--parse-list backend (list block))))))
                   (setq reminder-level level)
                   block)))))
         (wait-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (check-output)
                   (when (>= (string-bytes output) byte-budget)
                     (setq budget-kind 'output-bytes output-limit byte-budget)
                     (error "Review output-bytes budget exhausted (%d/%d)"
                            (string-bytes output) byte-budget))
                   (let* ((spent (max (estimated) (plist-get usage :output-tokens)))
                          (remaining (- token-budget spent)))
                     (when (<= remaining 0)
                       (setq budget-kind (if (>= (plist-get usage :output-tokens) (estimated))
                                             'output-tokens 'output-estimated-tokens)
                             output-limit token-budget)
                       (error "Review %s budget exhausted (%d/%d)" budget-kind spent token-budget))
                     (mevedel-context-summary--limit-digest-request
                      fsm (min remaining (plist-get policy :max-tokens))))
                   (let ((reminder (budget-reminder fsm)))
                     ;; Account for injected guidance before provider dispatch.
                     (when (> (mevedel-memory-review--tokens fsm)
                              (if (= round 0) (min 32000 (mevedel-model-usable-input-tokens policy))
                                (mevedel-model-usable-input-tokens policy)))
                       (error "Review request exceeds usable input context"))
                     (when reminder (show (concat "\n\n" reminder "\n"))))
                   (setq chunks nil reply-bytes 0)
                   (cl-incf round)
                   (with-current-buffer buffer (gptel--handle-wait fsm)))
               (error (finish 'error (error-message-string err))))))
         (tool-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (record-usage (gptel-fsm-info fsm) t)
                   (dolist (call (plist-get (gptel-fsm-info fsm) :tool-use))
                     (unless (member (plist-get call :name) '("Read" "Glob" "Grep"))
                       (error "Unavailable review tool: %s" (plist-get call :name))))
                   ;; gptel exposes executable arguments here, not raw JSON
                   ;; spelling or argument fragments still in flight.
                   (charge (decode-coding-string
                            (gptel--json-encode
                             (vconcat (mapcar (lambda (call) (list :name (plist-get call :name) :args (plist-get call :args)))
                                             (plist-get (gptel-fsm-info fsm) :tool-use)))) 'utf-8-unix) 'tool)
                   (check-output)
                   (with-current-buffer buffer (gptel--handle-tool-use fsm)))
               (error (finish 'error (error-message-string err))))))
         (done-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (record-usage (gptel-fsm-info fsm) t)
                   (check-output)
                   (let ((reply (apply #'concat (nreverse chunks))))
                     (check-limit 'proposal-bytes (string-bytes reply) mevedel-memory-proposal--max-bytes)
                     (finish 'success nil
                             (append (mevedel-memory-proposal-parse
                                      reply (plist-get scope :roots)
                                      (mapcar (lambda (entry) (plist-get entry :id)) admitted))
                                     (list :reply reply)))))
               (error (finish 'error (error-message-string err))))))
         (machine ()
           (gptel-make-fsm
            :handlers
            (append (list (list 'WAIT #'wait-handler) (list 'TOOL #'tool-handler)
                          (list 'DONE #'done-handler)
                          (list 'ERRS (lambda (fsm) (finish 'error (format "%s" (or (plist-get (gptel-fsm-info fsm) :error) "Review request failed")))))
                          (list 'ABRT (lambda (_) (finish 'aborted))))
                    (cl-remove-if (lambda (row) (memq (car row) '(WAIT TOOL DONE ERRS ABRT))) gptel-request--handlers))))
         (prepare (candidate-entries candidate-documents)
           (ensure-live)
           (let* ((text (mevedel-memory-review--input scope candidate-entries candidate-documents references focus rejections
                                                    (- (length entries) (length candidate-entries))))
                  (fsm (with-current-buffer buffer
                         (gptel-request text :buffer buffer :system system :stream stream
                                        :transforms nil :dry-run t))))
             (when (<= (mevedel-memory-review--tokens fsm) (min 32000 (mevedel-model-usable-input-tokens policy))) text)))
         (add-document (document)
           (when-let* ((text (prepare admitted (append documents (list document)))))
             (setq documents (append documents (list document)) input text))))
      (setq timer (run-at-time 180 nil (lambda () (finish 'error "Review timed out"))))
      (condition-case err
          (progn
            (dolist (bound (list token-budget byte-budget))
              (unless (and (integerp bound) (> bound 0))
                (error "Review output budgets must be positive integers")))
            (unless (or entries memory-only) (error "Empty digest batch requires an explicit memory-only review"))
            (dolist (text (list focus rejections))
              (unless (or (null text) (and (stringp text) (<= (string-bytes text) 8192)))
                (error "Review guidance exceeds its text bound")))
            (setq policy (copy-sequence (mevedel-model-resolve-workload 'memory)))
            (unless (and (plist-get policy :backend) (plist-get policy :model))
              (error "No model resolves for the memory workload"))
            (setq policy (plist-put policy :max-tokens
                                    (min token-budget (or (plist-get policy :max-tokens) token-budget)
                                         (max 1 (/ (mevedel-model-effective-context-window (plist-get policy :model)) 2)))))
            (setq system (mevedel-system-render-prompt-file
                          "prompts/memory/consolidation.md"
                          `(("REVIEW_LIMITS" . ,(format
                                                "- Cumulative output: %d tokens and %d accounted bytes, including reasoning, intermediate replies, and normalized tool calls.\n- Initial per-response output ceiling: %d tokens; followups have only the remaining cumulative budget.\n- Investigation: at most %d tool calls and %d tool-result bytes total (8 KiB per result).\n- Final proposal reply: at most %d bytes, independently of reasoning and earlier rounds.\n- Deadline: 180 seconds for the entire review, including preparation and tools."
                                                token-budget byte-budget (plist-get policy :max-tokens)
                                                mevedel-memory-investigation--max-calls
                                                mevedel-memory-investigation--max-bytes
                                                mevedel-memory-proposal--max-bytes))))
                  references (mevedel-memory-reference-check scope)
                  investigation (mevedel-memory-investigation-create scope nil #'live (lambda (reason) (finish 'error reason))))
            (with-current-buffer buffer
              (setq-local default-directory (plist-get scope :workspace-root)
                          mevedel--session nil mevedel--agent-invocation nil mevedel--current-request nil
                          gptel-backend (plist-get policy :backend) gptel-model (plist-get policy :model)
                          gptel-reasoning-effort (plist-get policy :effort)
                          gptel-max-tokens (unless (eq (type-of gptel-backend) 'gptel-openai-oauth) (plist-get policy :max-tokens))
                          gptel--request-params (plist-get policy :request-params)
                          gptel-use-context nil gptel-track-response nil gptel-use-tools t
                          gptel-confirm-tool-calls nil
                          gptel-tools (mevedel-memory-investigation-tools investigation)
                          gptel-system-prompt system gptel-stream stream))
            (setq admitted (and entries (list (car entries))) input (prepare admitted nil))
            (unless input (error "Review cannot admit its required scope and a complete digest"))
            (let (primary topics)
              (dolist (row (plist-get scope :roots))
                (dolist (file (plist-get (cdr row) :before))
                  (when (plist-get (cdr file) :exists)
                    (let ((document (list :root (car row) :file (car file)
                                          :body (decode-coding-string (plist-get (cdr file) :bytes) 'utf-8-unix))))
                      (if (or (eq (plist-get (cdr row) :kind) 'instructions) (equal (car file) "MEMORY.md"))
                          (push document primary) (push document topics))))))
              (dolist (document (seq-take (nreverse primary) 64)) (add-document document))
              (catch 'full
                (dolist (entry (cdr (seq-take entries 20)))
                  (let* ((candidate (append admitted (list entry))) (text (prepare candidate documents)))
                    (unless text (throw 'full nil))
                    (setq admitted candidate input text))))
              (dolist (document (seq-take (nreverse topics) (max 0 (- 64 (length documents))))) (add-document document)))
            (setf (mevedel-memory-investigation-entries investigation) admitted)
            (ensure-live)
            (with-current-buffer buffer
              (show (concat "# Admitted review evidence\n\n" input "\n\n# Model response\n\n"))
              (setq response-start (copy-marker (point-max))
                    buffer-read-only t)
              (add-hook 'kill-buffer-hook (lambda () (finish 'aborted nil nil t)) nil t)
              (gptel-request input :buffer buffer :system system :stream stream :transforms nil
                             :fsm (machine) :callback #'provider)))
        (error (finish 'error (error-message-string err))))
      (list :buffer buffer :cancel (lambda () (finish 'aborted))
            :report (lambda ()
                      (list :title "Running consolidation" :subtitle "In progress"
                            :identity buffer :navigator t :initial 'response
                            :validate (lambda ()
                                        (unless (and (buffer-live-p buffer) (live))
                                          (user-error "Consolidation inspection source is no longer live")))
                            :sections
                            (list (list :id 'evidence :title "Admitted review evidence" :body input
                                        :mode 'json-mode)
                                  (list :id 'response :title "Model response" :mode 'markdown-mode
                                        :body (with-current-buffer buffer
                                                (buffer-substring-no-properties response-start (point-max)))))))
            :usage #'snapshot))))

(provide 'mevedel-memory-review)
;;; mevedel-memory-review.el ends here
