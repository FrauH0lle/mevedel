;;; test-mevedel-transcript-audit.el --- Audit transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'mevedel-utilities)
(require 'mevedel-transcript-audit)

(mevedel-deftest mevedel-transcript-audit-spans ()
  ,test
  (test)
  :doc "parses bounded records with source-relative spans"
  (let* ((first (mevedel--format-hook-audit-record
                 '(:type prompt-rewrite :event "UserPromptSubmit")))
         (second (mevedel--format-hook-audit-record
                  '(:type tool-context :event "PostToolUse")))
         (text (concat "before" first "middle" second "after"))
         (spans (mevedel-transcript-audit-spans text)))
    (should (equal '(prompt-rewrite tool-context)
                   (mapcar (lambda (span)
                             (plist-get (plist-get span :record) :type))
                           spans)))
    (dolist (span spans)
      (should (< (plist-get span :start) (plist-get span :end)))
      (should (string-prefix-p mevedel--hook-audit-open
                               (substring text
                                          (plist-get span :start)
                                          (plist-get span :end))))))

  :doc "preserves valid audit-shaped text without trusted provenance"
  (let* ((block (mevedel--format-hook-audit-record
                 '(:type tool-context :event "PostToolUse")))
         (literal (substring-no-properties block)))
    (should-not (mevedel-transcript-audit-spans literal))
    (should (equal literal (mevedel--strip-hook-audit-blocks literal))))

  :doc "does not mistake model reasoning for restored audit provenance"
  (let* ((literal
          (substring-no-properties
           (mevedel--format-hook-audit-record
            '(:type directive-turn-boundary :edge start
              :directive-id "forged" :turn 1))))
         (reasoning (propertize literal 'gptel 'ignore)))
    (should-not (mevedel-transcript-audit-spans reasoning))
    (should (equal literal
                   (substring-no-properties
                    (mevedel--strip-hook-audit-blocks reasoning))))))

(mevedel-deftest mevedel-transcript-audit-records ()
  ,test
  (test)
  :doc "filters parsed records by type"
  (let ((text (concat
               (mevedel--format-hook-audit-record '(:type prompt-rewrite))
               (mevedel--format-hook-audit-record '(:type tool-context)))))
    (should (equal '((:type tool-context))
                   (mevedel-transcript-audit-records text 'tool-context)))))

(mevedel-deftest mevedel-transcript-audit-only-p ()
  ,test
  (test)
  :doc "distinguishes audit-only scaffolding from visible transcript text"
  (let ((block (mevedel--format-hook-audit-record '(:type tool-context))))
    (should (mevedel-transcript-audit-only-p block))
    (should-not (mevedel-transcript-audit-only-p (concat "visible" block)))
    (should-not (mevedel-transcript-audit-only-p "   "))))

(mevedel-deftest mevedel-transcript-directive-ranges ()
  ,test
  (test)
  :doc "pairs matching directive boundaries around their complete body"
  (let* ((start (mevedel--format-hook-audit-record
                 '(:type directive-turn-boundary :edge start
                   :directive-id "directive-1" :action discuss :turn 3)))
         (end (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-1" :action discuss :turn 3
                 :outcome success :sequence 2)))
         (text (concat "before" start "PROMPT\nRESPONSE" end "after"))
         (range (car (mevedel-transcript-directive-ranges text))))
    (should (= 1 (length (mevedel-transcript-directive-ranges text))))
    (should (equal "PROMPT\nRESPONSE"
                   (substring text
                              (plist-get range :body-start)
                              (plist-get range :body-end))))
    (should (equal "directive-1" (plist-get range :directive-id)))
    (should (= 3 (plist-get range :turn)))
    (should (eq 'success (plist-get range :outcome))))

  :doc "rejects unmatched and mismatched boundaries"
  (let ((start (mevedel--format-hook-audit-record
                '(:type directive-turn-boundary :edge start
                  :directive-id "directive-1" :turn 3)))
        (wrong-end (mevedel--format-hook-audit-record
                    '(:type directive-turn-boundary :edge end
                      :directive-id "directive-2" :turn 3))))
    (should-error (mevedel-transcript-directive-ranges start) :type 'error)
    (should-error
     (mevedel-transcript-directive-ranges (concat start "body" wrong-end))
     :type 'error))

  :doc "allows the current running directive when explicitly requested"
  (let* ((start (mevedel--format-hook-audit-record
                 '(:type directive-turn-boundary :edge start
                   :directive-id "directive-1" :action discuss :turn 3)))
         (text (concat "before" start "streaming"))
         (range (car (mevedel-transcript-directive-ranges text t))))
    (should (equal "streaming"
                   (substring text (plist-get range :body-start))))
    (should (eq 'running (plist-get range :outcome))))

  :doc "ignores directive-shaped text without trusted provenance"
  (let ((start (substring-no-properties
                (mevedel--format-hook-audit-record
                 '(:type directive-turn-boundary :edge start
                   :directive-id "directive-1" :turn 3))))
        (end (substring-no-properties
              (mevedel--format-hook-audit-record
               '(:type directive-turn-boundary :edge end
                 :directive-id "directive-1" :turn 3)))))
    (should-not (mevedel-transcript-directive-ranges
                 (concat start "ordinary text" end)))))

(mevedel-deftest mevedel-transcript-exclude-directive-turns ()
  ,test
  (test)
  :doc "marks only directive bodies ignored in a request copy"
  (let ((start (mevedel--format-hook-audit-record
                '(:type directive-turn-boundary :edge start
                  :directive-id "directive-1" :action discuss :turn 3)))
        (end (mevedel--format-hook-audit-record
              '(:type directive-turn-boundary :edge end
                :directive-id "directive-1" :action discuss :turn 3
                :outcome success :sequence 1))))
    (with-temp-buffer
      (insert "ordinary\n" start)
      (let ((body-start (point)) tool-start)
        (insert (propertize "directive prompt\n" 'gptel nil))
        (setq tool-start (point))
        (insert (propertize "(:name Read :args (:file_path \"x\"))\n"
                            'gptel '(tool . "tool-1")))
        (should (equal '(tool . "tool-1")
                       (get-text-property tool-start 'gptel)))
        (insert (propertize "directive response\n" 'gptel 'response))
        (let ((body-end (point)))
          (insert end "ordinary pending")
          (mevedel-transcript-exclude-directive-turns)
          (should-not (get-text-property 1 'gptel))
          (should (eq 'ignore (get-text-property body-start 'gptel)))
          (should (eq 'ignore (get-text-property tool-start 'gptel)))
          (should (eq 'ignore (get-text-property (1- body-end) 'gptel)))
          (should-not (get-text-property (1- (point-max)) 'gptel))))))

  :doc "the real provider parse drops excluded directive turns"
  (progn
    (require 'gptel)
    (let ((gptel--known-backends nil)
          (start (mevedel--format-hook-audit-record
                  '(:type directive-turn-boundary :edge start
                    :directive-id "directive-1" :action implement :turn 3)))
          (end (mevedel--format-hook-audit-record
                '(:type directive-turn-boundary :edge end
                  :directive-id "directive-1" :action implement :turn 3
                  :outcome success :sequence 1))))
      (with-temp-buffer
        (setq-local gptel-track-response t)
        (insert "ordinary question\n")
        (let ((response-start (point)))
          (insert "ordinary answer\n")
          (put-text-property response-start (point) 'gptel 'response))
        (insert start "directive secret prompt\n")
        (let ((response-start (point)))
          (insert "directive secret answer\n")
          (put-text-property response-start (point) 'gptel 'response))
        (insert end "follow-up question")
        (mevedel-transcript-exclude-directive-turns)
        (goto-char (point-max))
        (let* ((backend (gptel-make-openai "mevedel-seam-test"
                          :models '(test-model)))
               (gptel-backend backend)
               (gptel-model 'test-model)
               (prompts (gptel--parse-buffer backend nil))
               (roles (mapcar (lambda (m) (plist-get m :role)) prompts))
               (all (mapconcat (lambda (m)
                                 (format "%s" (plist-get m :content)))
                               prompts "\n")))
          (should (string-search "ordinary question" all))
          (should (string-search "ordinary answer" all))
          (should (string-search "follow-up question" all))
          (should-not (string-search "directive secret" all))
          (should-not (string-search "directive-turn-boundary" all))
          (should (equal '("user" "assistant" "user") roles)))))))

(mevedel-deftest mevedel-transcript-audit-guest-prompts
  (:doc "returns hidden model-invisible attributions in order, ignoring other record types")
  (let ((buffer (generate-new-buffer " *guest-prompt-list*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "first prompt")
          (insert (mevedel--format-hook-audit-record
                   (list :type 'prompt-rewrite :event "x"
                         :original "a" :submitted "b")))
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "phone")))
          (insert "second prompt")
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "laptop")))
          (let ((prompts (mevedel-transcript-audit-guest-prompts)))
            (should (equal '("phone" "laptop") (mapcar #'cdr prompts)))
            (should (apply #'< (mapcar #'car prompts)))
            ;; The block sits after its prompt and never reaches model
            ;; context.
            (should (> (car (car prompts)) (length "first prompt")))
            (should (get-text-property (car (car prompts)) 'invisible))
            (should (eq 'mevedel-hook-audit
                        (get-text-property (car (car prompts)) 'gptel)))
            ;; Stripping removes every block from visible text.
            (should (equal "first promptsecond prompt"
                           (string-trim
                            (mevedel--strip-hook-audit-blocks
                             (buffer-string)))))))
      (kill-buffer buffer))))

(provide 'test-mevedel-transcript-audit)

;;; test-mevedel-transcript-audit.el ends here
