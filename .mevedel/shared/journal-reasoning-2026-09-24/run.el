;;; run.el --- Opt-in journal reasoning experiment -*- lexical-binding: t -*-
;; Never loaded by the ordinary test wildcard. See protocol.md.
(add-to-list 'load-path (getenv "JOURNAL_GPTEL"))
(setq load-prefer-newer nil)
(require 'helpers (expand-file-name "test/helpers" default-directory))
(require 'gptel-openai-extras)
(require 'gptel-openai-oauth)
(require 'mevedel-context-summary)
(require 'mevedel-models)
(require 'json)
(defvar journal-eval-usage nil)
(defvar journal-eval-controls nil)
(defvar journal-eval-cap nil)
(defun journal-eval-read (path)
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list :null-object nil :false-object nil)))
(defun journal-eval-control-data (fsm)
  (let ((data (plist-get (gptel-fsm-info fsm) :data)) out)
    (dolist (key '(:model :stream :thinking :reasoning_effort :reasoning
                         :max_tokens :max_completion_tokens :max_output_tokens))
      (when (plist-member data key)
        (setq out (append out (list key (plist-get data key))))))
    out))
(defun journal-eval-usage (usage _info)
  (when usage (setq journal-eval-usage (copy-tree usage))))
(defun journal-eval-backend (path)
  (let ((config (with-temp-buffer (insert-file-contents path) (read (current-buffer)))))
    (apply (pcase (plist-get config :type)
             ('gptel-deepseek #'gptel-make-deepseek)
             ('gptel-openai-oauth #'gptel-make-openai-oauth)
             (_ (error "Unexpected backend")))
           (plist-get config :name) :models (plist-get config :models)
           (plist-get config :backend-options))))
(ert-deftest journal-reasoning-experiment ()
  (let* ((directory (getenv "JOURNAL_OUTPUT"))
         (fixtures (journal-eval-read (expand-file-name "fixtures.json" directory)))
         (preflight (getenv "JOURNAL_PREFLIGHT"))
         (jobs (journal-eval-read (getenv "JOURNAL_JOBS")))
         (backends (list (cons "DeepSeek" (journal-eval-backend (getenv "JOURNAL_DEEPSEEK")))
                         (cons "Codex" (journal-eval-backend (getenv "JOURNAL_CODEX")))))
         (real-limit (symbol-function 'mevedel-context-summary--limit-digest-request))
         (real-request (symbol-function 'gptel-request))
         (gptel-stream t)
         (gptel-log-level nil)
         completed)
    (when (and preflight (not (file-exists-p (expand-file-name "loaded-libraries.json" directory))))
      (with-temp-file (expand-file-name "loaded-libraries.json" directory)
        (insert (json-encode
                 (vconcat (mapcar (lambda (library)
                           (let ((path (locate-library library)))
                             (list :library library :path path
                                   :sha256 (with-temp-buffer
                                             (set-buffer-multibyte nil)
                                             (insert-file-contents-literally path)
                                             (secure-hash 'sha256 (current-buffer))))))
                         '("gptel" "gptel-request" "gptel-openai" "gptel-openai-extras"
                           "gptel-openai-oauth" "gptel-openai-responses"
                           "mevedel-context-summary" "mevedel-models")))))))
    (advice-add 'gptel--openai-update-tokens :before #'journal-eval-usage)
    (advice-add 'gptel--openai-responses-update-tokens :before #'journal-eval-usage)
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-context-summary--limit-digest-request)
                   (lambda (fsm limit)
                     (when limit (funcall real-limit fsm limit))
                     (setq journal-eval-controls (journal-eval-control-data fsm))))
                  ((symbol-function 'gptel-request)
                   (lambda (prompt &rest args)
                     (if preflight
                         (let ((fsm (apply real-request prompt
                                           (plist-put args :dry-run t))))
                           (mevedel-context-summary--limit-digest-request fsm journal-eval-cap)
                           fsm)
                       (let ((fsm (apply real-request prompt args)))
                         (setq journal-eval-controls (journal-eval-control-data fsm))
                         fsm)))))
          (dolist (job jobs)
            (let* ((arm (plist-get job :arm))
                   (fixture (cl-find (plist-get job :fixture) fixtures
                                     :key (lambda (f) (plist-get f :id)) :test #'equal))
                   (backend (cdr (assoc (plist-get arm :backend) backends)))
                   (model (intern (plist-get arm :model)))
                   (effort (and (plist-get arm :effort) (intern (plist-get arm :effort))))
                   (journal-eval-cap (plist-get arm :cap))
                   (policy (list :backend backend :model model :effort effort
                                 :max-tokens journal-eval-cap))
                   (start (float-time))
                   (deadline (+ start 120))
                   (journal-eval-usage nil)
                   (journal-eval-controls nil)
                   result done cancel diagnostics)
              (should (memq model (gptel-backend-models backend)))
              (should (or (null effort) (memq effort (mevedel-model-supported-efforts model))))
              (unwind-protect
                  (progn
                    (mevedel-test--with-captured-diagnostics diagnostics
                      (setq cancel
                            (mevedel-context-summary-generate
                             (plist-get fixture :source) 'digest
                             (lambda (value) (setq result value done t)) :policy policy))
                      (unless preflight
                        (while (and (not done) (< (float-time) deadline))
                          (accept-process-output nil 0.1))))
                    (when preflight
                      (should journal-eval-controls)
                      (should (equal (plist-get journal-eval-controls :model) (symbol-name model)))
                      (if (equal (plist-get arm :backend) "DeepSeek")
                          (progn
                            (should (equal (plist-get journal-eval-controls :thinking)
                                           (and effort (list :type (if (eq effort 'disabled) "disabled" "enabled")))))
                            (should (equal (plist-get journal-eval-controls :reasoning_effort)
                                           (and effort (not (eq effort 'disabled)) (symbol-name effort))))
                            (should (equal (plist-get journal-eval-controls :max_tokens) journal-eval-cap)))
                        (should (equal (plist-get (plist-get journal-eval-controls :reasoning) :effort)
                                       (and effort (symbol-name effort))))
                        (should-not (plist-member journal-eval-controls :max_output_tokens))))
                    (unless (or done preflight)
                      (setq result '(:outcome error :error-class timeout :error "120-second deadline")))
                    (let* ((summary (plist-get result :summary))
                           (record (append
                                    (list :id (plist-get job :id) :arm (plist-get arm :id)
                                          :fixture (plist-get fixture :id) :replicate (plist-get job :replicate)
                                          :backend-type (symbol-name (type-of backend))
                                          :elapsed (- (float-time) start) :controls journal-eval-controls
                                          :usage journal-eval-usage
                                          :empty (and summary (mevedel-context-summary-digest-empty-p summary))
                                          :bytes (and summary (string-bytes summary)))
                                    (cl-loop for key in '(:outcome :summary :error :error-class
                                                          :input-tokens :cached-tokens :output-tokens
                                                          :model :effort)
                                             when (plist-member result key)
                                             append (list key (plist-get result key))))))
                      (with-temp-buffer
                        (insert (json-encode record) "\n")
                        (write-region (point-min) (point-max) (getenv "JOURNAL_RESULTS") t 'silent)))
                    (push (plist-get job :id) completed))
                (unless done (when cancel (funcall cancel))))))
          (should (= (length completed) (length jobs))))
      (advice-remove 'gptel--openai-update-tokens #'journal-eval-usage)
      (advice-remove 'gptel--openai-responses-update-tokens #'journal-eval-usage))))
