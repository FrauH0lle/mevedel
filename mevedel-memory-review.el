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

;; `mevedel-structs'
(defvar mevedel--agent-invocation)
(defvar mevedel--current-request)
(defvar mevedel--session)

;; `mevedel-system'
(declare-function mevedel-system-render-prompt-file "mevedel-system" (relative-path &optional replacements))
(autoload 'mevedel-system-render-prompt-file "mevedel-system")

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
an idempotent :cancel function, and a :usage function returning a copy of
provider-reported usage received so far. The deadline is 180 seconds,
including input preparation; supported provider output limits are capped
at 8000 tokens."
  (let* ((buffer (generate-new-buffer " *mevedel-memory-review*"))
         (caller (current-buffer))
         (stream gptel-stream)
         (deadline (+ (float-time) 180))
         settled timer investigation policy references system input admitted
         (documents nil) (chunks nil) (output "")
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
                        (append (list :outcome outcome :error error :scope scope :entries admitted
                                      :references references :policy policy :omitted-digests (- (length entries) (length admitted)))
                                parsed usage)))))
         (ensure-live ()
           (unless (live) (error "Review deadline or ownership expired")))
         (show (text)
           (with-current-buffer buffer
             (let ((inhibit-read-only t))
               (save-excursion (goto-char (point-max)) (insert text)))))
         (charge (text)
           (setq output (concat output text))
           (when (or (> (string-bytes output) (* 32 1024))
                     (> (mevedel-context-summary--estimated-tokens "" output) 8000))
             (error "Review output limit exceeded")))
         (record-usage (info)
           (unless (= recorded-round round)
             (when-let* ((tokens (plist-get info :tokens)))
               (setq recorded-round round)
               (cl-loop for (source . target) in '((:input . :input-tokens) (:cached . :cached-tokens) (:output . :output-tokens))
                        for value = (plist-get tokens source)
                        when (numberp value) do (cl-incf (plist-get usage target) value))
               (when (> (plist-get usage :output-tokens) 8000)
                 (error "Review output token budget exceeded")))))
         (provider (response info)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (pcase response
                     ((pred stringp)
                      (unless (plist-get info :stream) (record-usage info))
                      (charge response)
                      (push response chunks)
                      (show response))
                     (`(reasoning . ,(and (pred stringp) text))
                      (charge text) (show (concat "\n[Reasoning]\n" text)))
                     (`(tool-result . ,results)
                      (dolist (row results)
                        (show (format "\n\n[%s]\n%s\n" (gptel-tool-name (car row)) (nth 2 row)))))
                     ('t (record-usage info))))
               (error (finish 'error (error-message-string err))))))
         (wait-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (mevedel-context-summary--limit-digest-request fsm (plist-get policy :max-tokens))
                   (when (> (mevedel-memory-review--tokens fsm)
                            (if (= round 0) (min 32000 (mevedel-model-usable-input-tokens policy))
                              (mevedel-model-usable-input-tokens policy)))
                     (error "Review request exceeds usable input context"))
                   (setq chunks nil)
                   (cl-incf round)
                   (with-current-buffer buffer (gptel--handle-wait fsm)))
               (error (finish 'error (error-message-string err))))))
         (tool-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (record-usage (gptel-fsm-info fsm))
                   (dolist (call (plist-get (gptel-fsm-info fsm) :tool-use))
                     (unless (member (plist-get call :name) '("Read" "Glob" "Grep"))
                       (error "Unavailable review tool: %s" (plist-get call :name))))
                   (charge (decode-coding-string
                            (gptel--json-encode
                             (vconcat (mapcar (lambda (call) (list :name (plist-get call :name) :args (plist-get call :args)))
                                             (plist-get (gptel-fsm-info fsm) :tool-use)))) 'utf-8-unix))
                   (with-current-buffer buffer (gptel--handle-tool-use fsm)))
               (error (finish 'error (error-message-string err))))))
         (done-handler (fsm)
           (unless settled
             (condition-case err
                 (progn
                   (ensure-live)
                   (record-usage (gptel-fsm-info fsm))
                   (let ((reply (apply #'concat (nreverse chunks))))
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
            (unless (or entries memory-only) (error "Empty digest batch requires an explicit memory-only review"))
            (dolist (text (list focus rejections))
              (unless (or (null text) (and (stringp text) (<= (string-bytes text) 8192)))
                (error "Review guidance exceeds its text bound")))
            (setq policy (copy-sequence (mevedel-model-resolve-workload 'buddy)))
            (unless (and (plist-get policy :backend) (plist-get policy :model))
              (error "No model resolves for the buddy workload"))
            (setq policy (plist-put policy :max-tokens
                                    (min 8000 (or (plist-get policy :max-tokens) 8000)
                                         (max 1 (/ (mevedel-model-effective-context-window (plist-get policy :model)) 2)))))
            (setq system (mevedel-system-render-prompt-file "prompts/memory/consolidation.md")
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
              (setq buffer-read-only t)
              (add-hook 'kill-buffer-hook (lambda () (finish 'aborted nil nil t)) nil t)
              (gptel-request input :buffer buffer :system system :stream stream :transforms nil
                             :fsm (machine) :callback #'provider)))
        (error (finish 'error (error-message-string err))))
      (list :buffer buffer :cancel (lambda () (finish 'aborted))
            :usage (lambda () (copy-sequence usage))))))

(provide 'mevedel-memory-review)
;;; mevedel-memory-review.el ends here
