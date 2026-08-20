;;; test-mevedel-compact-estimation.el -- Tests for compaction estimation -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'gptel-request)
(require 'mevedel-compact-estimation)
(require 'mevedel-models)
(require 'mevedel-structs)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defvar gptel--request-params)
(defvar gptel-backend)
(defvar gptel-max-tokens)
(defvar gptel-model)
(defvar gptel-reasoning-effort)

(mevedel-deftest mevedel-compact-estimation--file-local-variables-start ()
  ,test
  (test)
  :doc "returns nil when no file-local variables"
  (with-temp-buffer
    (insert "Hello world\n")
    (should (null (mevedel-compact-estimation--file-local-variables-start))))

  :doc "detects elisp-style file-local variables"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n")
      (should (= (mevedel-compact-estimation--file-local-variables-start) start))))

  :doc "detects markdown-style file-local variables"
  (with-temp-buffer
    (insert "# Markdown content\n\n")
    (let ((start (point)))
      (insert "<!-- Local Variables: -->\n")
      (insert "<!-- gptel-model: \"test\" -->\n")
      (insert "<!-- End: -->\n")
      (should (= (mevedel-compact-estimation--file-local-variables-start) start))))

  :doc "finds first Local Variables block when multiple exist"
  (with-temp-buffer
    (insert "Content here\n\n")
    (let ((first-start (point)))
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test\"\n")
      (insert ";; End:\n\n")
      (insert ";; Local Variables:\n")
      (insert ";; gptel-model: \"test2\"\n")
      (insert ";; End:\n")
      (should (= (mevedel-compact-estimation--file-local-variables-start) first-start)))))

(mevedel-deftest mevedel-compact-estimation-estimate-tokens ()
  ,test
  (test)
  :doc "counts tokens without file-local variables"
  (with-temp-buffer
    (insert "Hello world")  ; 11 chars / 4 = 2 tokens
    (should (= (mevedel-compact-estimation-estimate-tokens) 2)))

  :doc "excludes elisp-style file-local variables from count"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"claude-sonnet-4\"\n")
    (insert ";; End:\n")
    (should (= (mevedel-compact-estimation-estimate-tokens) 3)))

  :doc "excludes markdown-style file-local variables from count"
  (with-temp-buffer
    (insert "# Title\n\nContent\n\n")  ; 18 chars / 4 = 4 tokens
    (insert "<!-- Local Variables: -->\n")
    (insert "<!-- gptel-model: \"test\" -->\n")
    (insert "<!-- End: -->\n")
    (should (= (mevedel-compact-estimation-estimate-tokens) 4)))

  :doc "respects gptel ignore property"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world")
    (should (= (mevedel-compact-estimation-estimate-tokens) 2)))  ; "Hello world" = 11 chars / 4 = 2 tokens

  :doc "combines gptel ignore property and file-local variables exclusion"
  (with-temp-buffer
    (insert "Hello ")
    (let ((start (point)))
      (insert "ignored ")
      (put-text-property start (point) 'gptel 'ignore))
    (insert "world\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n")
    (should (= (mevedel-compact-estimation-estimate-tokens) 3)))  ; "Hello world\n\n" = 13 chars / 4 = 3 tokens

  :doc "excludes all file-local variables when multiple blocks exist"
  (with-temp-buffer
    (insert "Hello world\n\n")  ; 13 chars / 4 = 3 tokens
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test\"\n")
    (insert ";; End:\n\n")
    (insert ";; Local Variables:\n")
    (insert ";; gptel-model: \"test2\"\n")
    (insert ";; End:\n")
    (should (= (mevedel-compact-estimation-estimate-tokens) 3))))

(mevedel-deftest mevedel-compact-estimation-estimate-data-tokens ()
  ,test
  (test)
  :doc "keeps chars/4 behavior for plain realized data"
  (should (= (mevedel-compact-estimation-estimate-data-tokens
              (list :messages
                    (vector (list :role "user"
                                  :content "abcdefgh"))))
             3))

  :doc "counts OpenAI Responses input_image data URLs as image tokens"
  (let* ((mevedel-compact-estimation-image-token-estimate 123)
         (prefix "data:image/png;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :type "input_image" :image_url url))
         (expected (+ mevedel-compact-estimation-image-token-estimate
                      (/ (+ (length "input_image") (length prefix)) 4))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               expected)))

  :doc "counts OpenAI chat image_url data URLs as image tokens"
  (let* ((mevedel-compact-estimation-image-token-estimate 123)
         (prefix "data:image/jpeg;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :type "image_url"
                     :image_url (list :url url)))
         (expected (+ mevedel-compact-estimation-image-token-estimate
                      (/ (+ (length "image_url") (length prefix)) 4))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               expected)))

  :doc "counts Anthropic base64 image blocks as image tokens"
  (let* ((mevedel-compact-estimation-image-token-estimate 123)
         (data (list :type "image"
                     :source (list :type "base64"
                                   :media_type "image/png"
                                   :data (make-string 4000 ?A))))
         (expected (+ mevedel-compact-estimation-image-token-estimate
                      (/ (+ (length "image")
                            (length "base64")
                            (length "image/png"))
                         4))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               expected)))

  :doc "counts Bedrock image bytes blocks as image tokens"
  (let* ((mevedel-compact-estimation-image-token-estimate 123)
         (data (list :image (list :format "png"
                                  :source (list :bytes
                                                (make-string 4000 ?A)))))
         (expected (+ mevedel-compact-estimation-image-token-estimate
                      (/ (length "png") 4))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               expected)))

  :doc "adds image estimates for multiple images"
  (let* ((mevedel-compact-estimation-image-token-estimate 123)
         (prefix "data:image/png;base64,")
         (url (concat prefix (make-string 4000 ?A)))
         (data (list :content
                     (vector "abcd"
                             (list :type "input_image" :image_url url)
                             (list :type "input_image" :image_url url))))
         (expected (+ (* 2 mevedel-compact-estimation-image-token-estimate)
                      (/ (+ (length "abcd")
                            (* 2 (+ (length "input_image")
                                    (length prefix))))
                         4))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               expected)))

  :doc "leaves non-image data URLs as text"
  (let* ((url (concat "data:text/plain;base64,"
                      (make-string 400 ?A)))
         (data (list :url url)))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               (/ (length url) 4))))

  :doc "leaves quoted image data URLs as text"
  (let* ((url (concat "data:image/png;base64,"
                      (make-string 400 ?A)))
         (data (list :content url)))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               (/ (length url) 4))))

  :doc "leaves unrelated base64 strings as text"
  (let ((payload (make-string 400 ?A)))
    (should (= (mevedel-compact-estimation-estimate-data-tokens
                (list :data payload))
               (/ (length payload) 4))))

  :doc "falls back to text counting for malformed image blocks"
  (let ((data (list :type "image"
                    :source (list :type "base64"
                                  :media_type "image/png"))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               (/ (+ (length "image")
                     (length "base64")
                     (length "image/png"))
                  4))))

  :doc "falls back to text counting for malformed Bedrock image blocks"
  (let* ((payload (make-string 400 ?A))
         (data (list :image (list :source (list :bytes payload)))))
    (should (= (mevedel-compact-estimation-estimate-data-tokens data)
               (/ (length payload) 4)))))

(mevedel-deftest mevedel-compact-estimation--token-usage-input ()
  ,test
  (test)
  :doc "sums valid prompt token fields"
  (should (= 10 (mevedel-compact-estimation--token-usage-input
                 '(:input 1 :cached 2 :cache-read 3 :cache_read 4))))
  :doc "treats absent optional fields as zero"
  (should (= 4 (mevedel-compact-estimation--token-usage-input '(:input 4))))
  :doc "rejects absent, malformed, and negative usage"
  (dolist (tokens '(nil (:output 4) (:input "4") (:input -1)
                         (:input . 4)))
    (should-not (mevedel-compact-estimation--token-usage-input tokens))))

(mevedel-deftest mevedel-compact-estimation--token-usage-count ()
  ,test
  (test)
  :doc "adds valid output usage to prompt usage"
  (should (= 12 (mevedel-compact-estimation--token-usage-count
                 '(:input 4 :cached 3 :output 5))))
  :doc "treats an absent output field as zero"
  (should (= 4 (mevedel-compact-estimation--token-usage-count '(:input 4))))
  :doc "rejects absent, malformed, and negative usage"
  (dolist (tokens '(nil (:output 5) (:input "4") (:input -1)
                         (:input 4 :output "5") (:input 4 :output -1)
                         (:input . 4)))
    (should-not (mevedel-compact-estimation--token-usage-count tokens))))

(mevedel-deftest mevedel-compact-estimation-record-token-baseline ()
  ,test
  (test)
  :doc "records gptel-reported tokens as estimate baseline"
  (with-temp-buffer
    (insert "abcd")
    (let* ((gptel-model nil)
           (chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :model 'provider-model
                             :tokens '(:input 10
                                       :cached 5
                                       :output 7)
                             :tokens-full '(:input 10
                                            :cached 5
                                            :output 7)))))
      (mevedel-compact-estimation-record-token-baseline fsm)
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :tokens) 22))
      (should (eq (plist-get mevedel-compact-estimation--known-token-baseline :source)
                  'provider-context))
      (should (eq (plist-get mevedel-compact-estimation--known-token-baseline :model)
                  'provider-model))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline
                            :provider-context-tokens)
                 22))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline
                            :cumulative-usage-tokens)
                 22))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :input-tokens) 15))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :output-tokens) 7))
      (goto-char (point-max))
      (insert "abcdefgh")
      (should (= (mevedel-compact-estimation-estimate-tokens) 24))))

  :doc "prefers latest request tokens over cumulative tokens-full"
  (with-temp-buffer
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens '(:input 10 :output 5)
                             :tokens-full '(:input 1000 :output 500)))))
      (mevedel-compact-estimation-record-token-baseline fsm)
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :tokens) 15))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline
                            :cumulative-usage-tokens)
                 1500))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :input-tokens) 10))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :output-tokens) 5))))

  :doc "missing provider context:
falls back to the fresh visible prompt and never cumulative tool-loop usage"
  (with-temp-buffer
    (insert (make-string 400 ?x))
    (let* ((gptel-model nil)
           (mevedel-model-context-limit 258000)
           (chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :tokens-full '(:input 9396000 :output 0)))))
      (mevedel-compact-estimation-record-token-baseline fsm)
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline :tokens) 100))
      (should (eq (plist-get mevedel-compact-estimation--known-token-baseline :source)
                  'fresh-estimate))
      (should-not (plist-get mevedel-compact-estimation--known-token-baseline
                             :provider-context-tokens))
      (should (eq (plist-get mevedel-compact-estimation--known-token-baseline
                             :provider-context-status)
                  'missing))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline
                            :cumulative-usage-tokens)
                 9396000))
      (should (= (plist-get mevedel-compact-estimation--known-token-baseline
                            :fresh-visible-prompt-estimate)
                 100))
      (should (< (mevedel-compact-estimation-estimate-tokens)
                 (mevedel-compact-estimation-threshold-tokens)))))

  :doc "invalid provider context:
zero and over-window provider values use a fresh visible-prompt estimate"
  (dolist (provider-count '(0 300000))
    (with-temp-buffer
      (insert (make-string 400 ?x))
      (let* ((gptel-model nil)
             (mevedel-model-context-limit 258000)
             (chat-buffer (current-buffer))
             (fsm (gptel-make-fsm
                   :info (list :buffer chat-buffer
                               :tokens `(:input ,provider-count :output 0)
                               :tokens-full '(:input 9000000 :output 0)))))
        (mevedel-compact-estimation-record-token-baseline fsm)
        (should (= (plist-get mevedel-compact-estimation--known-token-baseline :tokens) 100))
        (should (eq (plist-get mevedel-compact-estimation--known-token-baseline :source)
                    'fresh-estimate))
        (should (eq (plist-get mevedel-compact-estimation--known-token-baseline
                               :provider-context-status)
                    (if (zerop provider-count) 'zero 'over-window))))))

  :doc "partial provider context:
missing or zero prompt-side usage cannot become the active baseline"
  (dolist (case '(((:output 500) missing)
                  ((:input 0 :output 500) zero)
                  ((:input "bad" :output 500) missing)))
    (with-temp-buffer
      (insert (make-string 400 ?x))
      (let* ((gptel-model nil)
             (mevedel-model-context-limit 258000)
             (chat-buffer (current-buffer))
             (fsm (gptel-make-fsm
                   :info (list :buffer chat-buffer :tokens (car case)))))
        (mevedel-compact-estimation-record-token-baseline fsm)
        (should (= 100 (plist-get mevedel-compact-estimation--known-token-baseline :tokens)))
        (should (eq 'fresh-estimate
                    (plist-get mevedel-compact-estimation--known-token-baseline :source)))
        (should (eq (cadr case)
                    (plist-get mevedel-compact-estimation--known-token-baseline
                               :provider-context-status))))))

  :doc "loads model metadata at the cold owner boundary"
  (let ((root
         (file-name-directory
          (locate-library "mevedel-compact-estimation")))
        (emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (should
       (= 0
          (call-process
           emacs nil t nil "--batch" "-Q" "-L" root
           "--eval"
           (prin1-to-string
            '(progn
               (require 'cl-lib)
               (require 'mevedel-compact-estimation)
               (cl-letf (((symbol-function 'gptel-fsm-info)
                          (lambda (_fsm)
                            (list :buffer (current-buffer)
                                  :model 'cold-model
                                  :tokens '(:input 4 :output 1)))))
                 (with-temp-buffer
                   (mevedel-compact-estimation-record-token-baseline 'fsm)))
               (unless (featurep 'mevedel-models)
                 (error "Estimation did not load Models")))))))
      (should (string-empty-p (string-trim (buffer-string))))))

  :doc "ignores compaction request token usage"
  (with-temp-buffer
    (insert "abcd")
    (let* ((chat-buffer (current-buffer))
           (fsm (gptel-make-fsm
                 :info (list :buffer chat-buffer
                             :context '(:mevedel-context-summary t)
                             :tokens-full '(:input 10 :output 7)))))
      (mevedel-compact-estimation-record-token-baseline fsm)
      (should (null mevedel-compact-estimation--known-token-baseline)))))

(mevedel-deftest mevedel-compact-estimation-model-visible-chars
  (:doc "counts only model-visible characters outside file-local variables")
  (with-temp-buffer
    (insert "abcdWXYZ")
    (put-text-property 5 9 'gptel 'ignore)
    (should (= 4 (mevedel-compact-estimation-model-visible-chars)))))

(mevedel-deftest mevedel-compact-estimation-current-request-id
  (:doc "returns only a live mevedel request identifier")
  (with-temp-buffer
    (let ((mevedel--current-request
           (mevedel-request--create :id "request-test" :origin "/root")))
      (should (equal "request-test" (mevedel-compact-estimation-current-request-id))))))

(mevedel-deftest mevedel-compact-estimation-telemetry-inputs
  (:doc "captures provider counts, thresholds, marker, and visible size")
  (with-temp-buffer
    (insert "abcdefgh")
    (put-text-property 5 9 'gptel 'ignore)
    (let* ((target-model (make-symbol "model"))
           (mevedel-compact-estimation--known-token-baseline
            (list :source 'provider-context
                  :provider-context-usage '(:input 18 :output 3)
                  :cumulative-usage '(:input 20 :output 5)
                  :provider-context-tokens 21
                  :cumulative-usage-tokens 25
                  :provider-context-status 'valid
                  :fresh-visible-prompt-estimate 2
                  :model 'provider-model
                  :model-context-window 900
                  :position (copy-marker 5))))
      (put target-model :context-window 1)
      (cl-letf (((symbol-function 'mevedel-compact-estimation-workload-policy)
                 (lambda () '(:kind summary)))
                ((symbol-function 'mevedel-compact-estimation-policy-threshold-tokens)
                 (lambda (policy)
                   (if (eq (plist-get policy :kind) 'summary) 80 90)))
                ((symbol-function 'mevedel-compact-estimation-estimate-buffer-tokens)
                 (lambda (_) 2)))
        (let ((facts (mevedel-compact-estimation-telemetry-inputs
                      25 (list :model target-model :kind 'target))))
          (should (= 21 (plist-get facts :provider-context-tokens)))
          (should (= 25 (plist-get facts :cumulative-usage-tokens)))
          (should (equal '(:input 18 :output 3)
                         (plist-get facts :provider-context-usage)))
          (should (equal '(:input 20 :output 5)
                         (plist-get facts :cumulative-usage)))
          (should (eq 'valid (plist-get facts :provider-context-status)))
          (should (eq 'provider-model
                      (plist-get facts :provider-context-model)))
          (should (= 900 (plist-get facts :provider-context-window)))
          (should (eq 'provider-context (plist-get facts :chosen-source)))
          (should (eq target-model (plist-get facts :target-model)))
          (should (= 2 (plist-get facts :fresh-visible-prompt-estimate)))
          (should (= 80 (plist-get facts :threshold)))
          (should (= 1000 (plist-get facts :model-context-window)))
          (should (= 8 (plist-get facts :buffer-chars-total)))
          (should (= 4 (plist-get facts :buffer-chars-model-visible))))))))

(mevedel-deftest mevedel-compact-estimation-workload-policy ()
  ,test
  (test)
  :doc "resolves the summarization workload with default request settings"
  (let ((gptel-max-tokens 999)
        (gptel--request-params '(:temperature 0.5)))
    (cl-letf (((symbol-function 'mevedel-model-resolve-workload)
               (lambda (workload &rest _)
                 (should (eq workload 'summarization))
                 '(:backend summary-backend :model summary-model))))
      (let ((policy (mevedel-compact-estimation-workload-policy)))
        (should (eq (plist-get policy :backend) 'summary-backend))
        (should (eq (plist-get policy :model) 'summary-model))
        (should (plist-member policy :max-tokens))
        (should-not (plist-get policy :max-tokens))
        (should (plist-member policy :request-params))
        (should-not (plist-get policy :request-params))))))

(mevedel-deftest mevedel-compact-estimation-target-policy ()
  ,test
  (test)
  :doc "captures the realized request settings used for target budgeting"
  (let ((gptel-backend 'target-backend)
        (gptel-model 'target-model)
        (gptel-reasoning-effort 'high)
        (gptel-max-tokens 300)
        (gptel--request-params '(:temperature 0.5)))
    (should
     (equal
      (mevedel-compact-estimation-target-policy)
      '(:backend target-backend :model target-model :effort high :max-tokens 300
        :request-params (:temperature 0.5))))))

(mevedel-deftest mevedel-compact-estimation-policy-threshold-tokens ()
  ,test
  (test)
  :doc "applies the configured ratio to the policy's usable context"
  (put 'mevedel-policy-model :context-window 0.2)
  (let ((mevedel-compact-estimation-token-threshold 0.5)
        (mevedel-model-reserve-tokens 20))
    (should
     (= 90
        (mevedel-compact-estimation-policy-threshold-tokens
         '(:backend nil :model mevedel-policy-model :max-tokens 10))))))

(mevedel-deftest mevedel-compact-estimation-admission ()
  ,test
  (test)
  :doc "distinguishes below-threshold, summarizer-only, and target pressure"
  (put 'mevedel-admission-target :context-window 0.2)
  (put 'mevedel-admission-summary :context-window 0.1)
  (let ((mevedel-compact-estimation-token-threshold 0.5)
        (mevedel-model-reserve-tokens 0)
        (summary-policy
         '(:backend nil :model mevedel-admission-summary :max-tokens 0))
        (target-policy
         '(:backend nil :model mevedel-admission-target :max-tokens 0)))
    (cl-letf (((symbol-function 'mevedel-compact-estimation-workload-policy)
               (lambda () summary-policy)))
      (should-not (mevedel-compact-estimation-admission 49 target-policy))
      (should
       (equal (mevedel-compact-estimation-admission 50 target-policy)
              (list :summary-policy summary-policy
                    :target-pressure nil)))
      (should
       (equal (mevedel-compact-estimation-admission 100 target-policy)
              (list :summary-policy summary-policy
                    :target-pressure t))))))

(mevedel-deftest mevedel-compact-estimation-threshold-tokens ()
  ,test
  (test)
  :doc "rejects an integer threshold"
  (let ((mevedel-compact-estimation-token-threshold 150000))
    (should-error (mevedel-compact-estimation-threshold-tokens) :type 'user-error))

  :doc "rejects ratio endpoints and out-of-range values"
  (dolist (threshold '(0.0 1.0 -0.1 1.1 invalid))
    (let ((mevedel-compact-estimation-token-threshold threshold))
      (should-error (mevedel-compact-estimation-threshold-tokens) :type 'user-error)))

  :doc "float threshold applies to usable context"
  ;; The configured limit only applies when no model declares a window.
  (let ((mevedel-model-context-limit 200000)
        (mevedel-model-reserve-tokens 20000)
        (mevedel-compact-estimation-token-threshold 0.8)
        (gptel-model nil)
        (gptel-max-tokens nil))
    (should (= (mevedel-compact-estimation-threshold-tokens) 144000)))

  :doc "reserve is capped on small context windows"
  (let ((mevedel-model-context-limit 8000)
        (mevedel-model-reserve-tokens 20000)
        (mevedel-compact-estimation-token-threshold 0.8)
        (gptel-model nil)
        (gptel-max-tokens nil))
    (should (= (mevedel-compact-estimation-usable-tokens) 4000))
    (should (= (mevedel-compact-estimation-threshold-tokens) 3200))))

(mevedel-deftest mevedel-compact-estimation-baseline-source
  (:doc "reports the current baseline source and the fresh-estimate fallback")
  (with-temp-buffer
    (should (eq 'fresh-estimate
                (mevedel-compact-estimation-baseline-source)))
    (setq-local mevedel-compact-estimation--known-token-baseline
                '(:source provider-context))
    (should (eq 'provider-context
                (mevedel-compact-estimation-baseline-source)))))

(mevedel-deftest mevedel-compact-estimation-clear-baseline
  (:doc "clears the current buffer's token baseline")
  (with-temp-buffer
    (setq-local mevedel-compact-estimation--known-token-baseline
                '(:source provider-context))
    (mevedel-compact-estimation-clear-baseline)
    (should-not mevedel-compact-estimation--known-token-baseline)))

(provide 'test-mevedel-compact-estimation)

;;; test-mevedel-compact-estimation.el ends here
