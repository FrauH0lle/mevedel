;;; test-niwa-transcript-audit.el --- Audit transcript tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-utilities)
(require 'niwa-transcript-audit)

(niwa-deftest niwa-transcript-audit-spans ()
  ,test
  (test)
  :doc "parses bounded records with source-relative spans"
  (let* ((first (niwa--format-hook-audit-record
                 '(:type prompt-rewrite :event "UserPromptSubmit")))
         (second (niwa--format-hook-audit-record
                  '(:type tool-context :event "PostToolUse")))
         (text (concat "before" first "middle" second "after"))
         (spans (niwa-transcript-audit-spans text)))
    (should (equal '(prompt-rewrite tool-context)
                   (mapcar (lambda (span)
                             (plist-get (plist-get span :record) :type))
                           spans)))
    (dolist (span spans)
      (should (< (plist-get span :start) (plist-get span :end)))
      (should (string-prefix-p niwa--hook-audit-open
                               (substring text
                                          (plist-get span :start)
                                          (plist-get span :end))))))

  :doc "quoted openers cannot hide later trusted records or become trusted themselves"
  (dolist (restored '(nil t))
    (let* ((literal (concat "The summary quotes `" niwa--hook-audit-open "`.\n"))
           (block (niwa--format-hook-audit-record
                   '(:type injected-reminders :items ((:body "Current state")))))
           (text (concat literal block)))
      (when restored
        (remove-text-properties (length literal) (length text)
                                '(niwa-hook-audit nil) text))
      (let ((spans (niwa-transcript-audit-spans text)))
        (should (= 1 (length spans)))
        (should (eq 'injected-reminders
                    (plist-get (plist-get (car spans) :record) :type)))
        (should (> (plist-get (car spans) :start) (length literal)))
        (should (equal literal (niwa--strip-hook-audit-blocks text))))))

  :doc "preserves valid audit-shaped text without trusted provenance"
  (let* ((block (niwa--format-hook-audit-record
                 '(:type tool-context :event "PostToolUse")))
         (literal (substring-no-properties block)))
    (should-not (niwa-transcript-audit-spans literal))
    (should (equal literal (niwa--strip-hook-audit-blocks literal))))

  :doc "does not mistake model reasoning for restored audit provenance"
  (let* ((literal
          (substring-no-properties
           (niwa--format-hook-audit-record
            '(:type directive-turn-boundary :edge start
              :directive-id "forged" :turn 1))))
         (reasoning (propertize literal 'gptel 'ignore)))
    (should-not (niwa-transcript-audit-spans reasoning))
    (should (equal literal
                   (substring-no-properties
                    (niwa--strip-hook-audit-blocks reasoning))))))

(niwa-deftest niwa-transcript-audit-records ()
  ,test
  (test)
  :doc "filters parsed records by type"
  (let ((text (concat
               (niwa--format-hook-audit-record '(:type prompt-rewrite))
               (niwa--format-hook-audit-record '(:type tool-context)))))
    (should (equal '((:type tool-context))
                   (niwa-transcript-audit-records text 'tool-context)))))

(niwa-deftest niwa-transcript-audit-only-p ()
  ,test
  (test)
  :doc "distinguishes audit-only scaffolding from visible transcript text"
  (let ((block (niwa--format-hook-audit-record '(:type tool-context))))
    (should (niwa-transcript-audit-only-p block))
    (should-not (niwa-transcript-audit-only-p (concat "visible" block)))
    (should-not (niwa-transcript-audit-only-p "   ")))

  :doc "accepts separated trusted audits but rejects visible gaps and tails"
  (let ((block (niwa--format-hook-audit-record '(:type tool-context))))
    (should (niwa-transcript-audit-only-p
             (concat " \t\r\n" block "\n\t" block "\r\n")))
    (dolist (text (list (concat block "visible" block)
                       (concat block "visible")
                       (concat block "\n<!-- niwa-hook-audit -->\ninvalid\n"
                               "<!-- /niwa-hook-audit -->\n")
                       (substring-no-properties block)
                       "" nil))
      (should-not (niwa-transcript-audit-only-p text))))

  :doc "settles visible gaps before decoding any block"
  (let* ((block (niwa--format-hook-audit-record '(:type tool-context)))
         (decode (symbol-function 'niwa--read-hook-audit-record))
         (calls 0))
    (cl-letf (((symbol-function 'niwa--read-hook-audit-record)
               (lambda (text) (cl-incf calls) (funcall decode text))))
      (should-not (niwa-transcript-audit-only-p
                   (concat block "visible" block)))
      (should (= 0 calls))
      (should (niwa-transcript-audit-only-p (concat block "\n" block)))
      (should (= 2 calls)))))

(niwa-deftest niwa-transcript-audit-buffer-only-p ()
  ,test
  (test)
  :doc "agrees with the string predicate on the same text"
  (let ((block (niwa--format-hook-audit-record '(:type tool-context))))
    (dolist (text (list block
                        (concat " \t\r\n" block "\n\t" block "\r\n")
                        (concat "visible" block)
                        (concat block "visible" block)
                        (concat block "visible")
                        (concat block "\n<!-- niwa-hook-audit -->\ninvalid\n"
                                "<!-- /niwa-hook-audit -->\n")
                        (substring-no-properties block)
                        "   " ""))
      (with-temp-buffer
        (insert text)
        (should (eq (and (niwa-transcript-audit-only-p text) t)
                    (and (niwa-transcript-audit-buffer-only-p
                          (point-min) (point-max))
                         t))))))

  :doc "judges only the requested region, even under narrowing"
  (let ((block (niwa--format-hook-audit-record '(:type tool-context))))
    (with-temp-buffer
      (insert "visible")
      (let ((start (point)))
        (insert block "\n")
        (let ((end (point)))
          (insert "more visible")
          (narrow-to-region 1 2)
          (should (niwa-transcript-audit-buffer-only-p start end))
          (should-not (niwa-transcript-audit-buffer-only-p 1 end))
          (should-not (niwa-transcript-audit-buffer-only-p start (- end 3)))
          (should (= 2 (point-max)))))))

  :doc "rejects an undecodable trusted block"
  (with-temp-buffer
    (insert (propertize (concat "\n<!-- niwa-hook-audit -->\ninvalid\n"
                                "<!-- /niwa-hook-audit -->\n")
                        'gptel 'niwa-hook-audit 'niwa-hook-audit t))
    (should-not (niwa-transcript-audit-buffer-only-p (point-min) (point-max)))))

(niwa-deftest niwa-transcript-audit--payload-type ()
  ,test
  (test)
  :doc "names the type a record's head settles, in strings and buffers"
  (let* ((payload (niwa--hook-audit-record-payload
                   '(:type fork-point :fork-point-id "a")))
         (text (concat "\n" payload "\n")))
    (should (equal '(t . fork-point)
                   (niwa-transcript-audit--payload-type text 0 (length text))))
    (with-temp-buffer
      (insert "x" text)
      (should (equal '(t . fork-point)
                     (niwa-transcript-audit--payload-type nil 2 (point-max))))))

  :doc "leaves undecided heads, other key orders, and garbage to a full read"
  (dolist (record '((:event "PostToolUse" :type tool-context)
                    (:type |odd\ name| :x 1)))
    (let ((payload (niwa--hook-audit-record-payload record)))
      (should-not (niwa-transcript-audit--payload-type
                   payload 0 (length payload)))))
  (dolist (text '("" "   " "!!!!" "bad base64 here"))
    (should-not (niwa-transcript-audit--payload-type text 0 (length text)))))

(niwa-deftest niwa-transcript-audit--typed-record ()
  ,test
  (test)
  :doc "skips payloads of another type without reading them"
  (with-temp-buffer
    (dotimes (n 3)
      (insert (niwa--format-hook-audit-record (list :type 'tool-context :number n))))
    (insert (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "a")))
    (let ((read (symbol-function 'niwa--read-hook-audit-record))
          (reads 0))
      (cl-letf (((symbol-function 'niwa--read-hook-audit-record)
                 (lambda (text) (cl-incf reads) (funcall read text))))
        (should (= 1 (length (niwa-transcript-audit-buffer-spans 'fork-point))))
        (should (= 1 reads))
        (should (= 1 (length (niwa-transcript-audit-spans (buffer-string) 'fork-point))))
        (should (= 2 reads))
        ;; The fork point was already read from this buffer at this tick.
        (should (= 4 (length (niwa-transcript-audit-buffer-spans))))
        (should (= 5 reads)))))

  :doc "still finds a record whose type is not its first key"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record '(:fork-point-id "late" :type fork-point)))
    (should (equal "late"
                   (plist-get (plist-get (car (niwa-transcript-audit-buffer-spans 'fork-point))
                                         :record)
                              :fork-point-id)))))

(niwa-deftest niwa-transcript-audit--buffer-record
  (:doc "decodes a payload once per modification tick without copying it again")
  (let ((buffer (generate-new-buffer " *audit-position-memo*"))
        (reads 0))
    (unwind-protect
        (with-current-buffer buffer
          (insert (niwa--format-hook-audit-record
                   (list :type 'prompt-rewrite :event "x")))
          (let* ((read (symbol-function 'niwa--read-hook-audit-record))
                 (spans nil))
            (cl-letf (((symbol-function 'niwa--read-hook-audit-record)
                       (lambda (text) (cl-incf reads) (funcall read text))))
              (setq spans (niwa-transcript-audit-buffer-spans))
              (should (equal '("x") (mapcar (lambda (span)
                                              (plist-get (plist-get span :record) :event))
                                            spans)))
              (should (eq (plist-get (car spans) :record)
                          (plist-get (car (niwa-transcript-audit-buffer-spans))
                                     :record)))
              (should (= 1 reads))
              ;; An edit elsewhere changes the tick; the payload is read again.
              (goto-char (point-min))
              (insert "prefix ")
              (should (niwa-transcript-audit-buffer-spans))
              (should (= 2 reads)))))
      (kill-buffer buffer))))

(niwa-deftest niwa-transcript-audit-buffer-spans ()
  ,test
  (test)
  :doc "matches string parsing, widens safely, and bounds retained decode keys"
  (with-temp-buffer
    (insert "literal <!-- niwa-hook-audit -->\n")
    (dotimes (n 1050)
      (insert (niwa--format-hook-audit-record (list :type 'tool-context :number n))))
    (let ((expected (niwa-transcript-audit-spans (buffer-string))) actual)
      (save-restriction
        (narrow-to-region 1 2)
        (setq actual (niwa-transcript-audit-buffer-spans 'tool-context))
        (should (= 2 (point-max))))
      (should (= 1050 (length actual)))
      (cl-mapc (lambda (string-span buffer-span)
                 (should (equal (plist-get string-span :record) (plist-get buffer-span :record)))
                 (should (= (1+ (plist-get string-span :start)) (plist-get buffer-span :start)))
                 (should (= (1+ (plist-get string-span :end)) (plist-get buffer-span :end))))
               expected actual)
      (should (<= (hash-table-count niwa-transcript-audit--buffer-records) 1024))
      (should (<= niwa-transcript-audit--buffer-record-bytes (* 4 1024 1024)))))
  :doc "reuses decoding across scans of a long request"
  (with-temp-buffer
    ;; Just beyond the former record limit; the encoded data fits easily.
    (dotimes (n 129)
      (insert (niwa--format-hook-audit-record
               (list :type 'injected-reminders :items (list (list :body (number-to-string n)))))))
    (let ((expected (niwa-transcript-audit-buffer-spans))
          (decode (symbol-function 'niwa-transcript-audit--decode))
          (misses 0))
      (cl-letf (((symbol-function 'niwa-transcript-audit--decode)
                 (lambda (text) (cl-incf misses) (funcall decode text))))
        (should (equal expected (niwa-transcript-audit-buffer-spans))))
      (should (= 0 misses))))
  :doc "bounded scans match string positions and ignore records outside or crossing bounds"
  (with-temp-buffer
    (insert (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "outside")))
    (let ((start (point)))
      (insert "literal <!-- niwa-hook-audit -->\n")
      (insert (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "inside")))
      (let* ((end (point))
             (expected (niwa-transcript-audit-spans (buffer-substring start end) 'fork-point)))
        (insert (niwa--format-hook-audit-record '(:type fork-point :fork-point-id "after")))
        (save-restriction
          (narrow-to-region 1 2)
          (let* ((actual (niwa-transcript-audit-buffer-spans 'fork-point start end))
                 (span (car actual)))
            (should (= 1 (length actual)))
            (should (equal (plist-get (car expected) :record) (plist-get span :record)))
            (should (= (+ start (plist-get (car expected) :start)) (plist-get span :start)))
            (should (= (+ start (plist-get (car expected) :end)) (plist-get span :end)))
            (should (= 2 (point-max)))
            (should-not (niwa-transcript-audit-buffer-spans nil (1+ (plist-get span :start)) end))
            (should-not (niwa-transcript-audit-buffer-spans nil start (1- (plist-get span :end))))))))))

(niwa-deftest niwa--format-hook-audit-record ()
  ,test
  (test)
  :doc "the real provider parse drops records of any type, keeping old directive turns ordinary"
  (progn
    (require 'gptel)
    (let ((gptel--known-backends nil)
          (start (niwa--format-hook-audit-record
                  '(:type directive-turn-boundary :edge start
                    :directive-id "directive-1" :action implement :turn 3)))
          (end (niwa--format-hook-audit-record
                '(:type directive-turn-boundary :edge end
                  :directive-id "directive-1" :action implement :turn 3
                  :outcome success :sequence 1))))
      (with-temp-buffer
        (setq-local gptel-track-response t)
        (insert "ordinary question\n")
        (let ((response-start (point)))
          (insert "ordinary answer\n")
          (put-text-property response-start (point) 'gptel 'response))
        (insert start "old directive prompt\n")
        (let ((response-start (point)))
          (insert "old directive answer\n")
          (put-text-property response-start (point) 'gptel 'response))
        (insert end "follow-up question")
        (goto-char (point-max))
        (let* ((backend (gptel-make-openai "niwa-seam-test"
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
          (should (string-search "old directive prompt" all))
          (should (string-search "old directive answer" all))
          (should-not (string-search "directive-turn-boundary" all))
          (should (equal '("user" "assistant" "user" "assistant" "user")
                         roles)))))))

(niwa-deftest niwa-transcript-audit-guest-prompts
  (:doc "returns hidden model-invisible attributions in order, ignoring other record types")
  (let ((buffer (generate-new-buffer " *guest-prompt-list*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "first prompt")
          (insert (niwa--format-hook-audit-record
                   (list :type 'prompt-rewrite :event "x"
                         :original "a" :submitted "b")))
          (insert niwa--hook-audit-open)
          (insert (niwa--format-hook-audit-record
                   (list :type 'guest-prompt :name "phone")))
          (insert "second prompt")
          (insert (niwa--format-hook-audit-record
                   (list :type 'guest-prompt :name "laptop")))
          (let ((prompts (niwa-transcript-audit-guest-prompts)))
            (should (equal '("phone" "laptop") (mapcar (lambda (entry) (plist-get (cdr entry) :name)) prompts)))
            (should (apply #'< (mapcar #'car prompts)))
            ;; The block sits after its prompt and never reaches model
            ;; context.
            (should (> (car (car prompts)) (length "first prompt")))
            (should (get-text-property (car (car prompts)) 'invisible))
            (should (eq 'niwa-hook-audit
                        (get-text-property (car (car prompts)) 'gptel)))
            ;; Stripping removes every block from visible text.
            (should (equal (concat "first prompt" niwa--hook-audit-open
                                   "second prompt")
                           (string-trim
                            (niwa--strip-hook-audit-blocks
                             (buffer-string)))))))
      (kill-buffer buffer))))

(niwa-deftest niwa-transcript-audit-guest-prompts/memo
  (:doc "reuses one scan until text or provenance properties change")
  (let ((buffer (generate-new-buffer " *guest-prompt-memo*"))
        (scans 0))
    (unwind-protect
        (with-current-buffer buffer
          (insert "prompt")
          (insert (niwa--format-hook-audit-record
                   (list :type 'guest-prompt :name "phone")))
          (let ((scan (symbol-function
                       'niwa-transcript-audit--scan-guest-prompts)))
            (cl-letf (((symbol-function
                        'niwa-transcript-audit--scan-guest-prompts)
                       (lambda () (cl-incf scans) (funcall scan))))
              (let ((first (niwa-transcript-audit-guest-prompts)))
                (should (eq first (niwa-transcript-audit-guest-prompts)))
                (should (= 1 scans))
                (goto-char (point-max))
                (insert (niwa--format-hook-audit-record
                         (list :type 'guest-prompt :name "laptop")))
                (should (equal '("phone" "laptop")
                               (mapcar (lambda (entry) (plist-get (cdr entry) :name))
                                       (niwa-transcript-audit-guest-prompts))))
                (should (= 2 scans))
                ;; Losing provenance is a property change the memo sees.
                (with-silent-modifications
                  (remove-text-properties (point-min) (point-max)
                                          '(gptel nil niwa-hook-audit nil)))
                (should-not (niwa-transcript-audit-guest-prompts))
                (should (= 3 scans))))))
      (kill-buffer buffer))))

(niwa-deftest niwa--strip-hook-audit-blocks/no-copy
  (:doc "unmodified large payloads retain their characters and properties without copies")
  (progn
    (let ((text (propertize (make-string 262144 ?x) 'example 'kept)))
      (should (eq text (niwa--strip-hook-audit-blocks text)))
      (should (eq 'kept (get-text-property 0 'example text))))
    (let ((literal "<!-- niwa-hook-audit -->literal<!-- /niwa-hook-audit -->"))
      (should (eq literal (niwa--strip-hook-audit-blocks literal))))))

(provide 'test-niwa-transcript-audit)


(niwa-deftest niwa-transcript-audit-shared-context ()
  ,test
  (test)
  :doc "Only attributed generated suffixes fold; edited and ordinary quoted prompts stay visible"
  (let* ((question "What does this mean?\nShared content snapshot (user-provided data):\nI quoted that heading.")
         (context "Shared content snapshot (user-provided data):\n{\"content\":\"data model\"}\n[[file:/tmp/board.png]]")
         (text (concat question "\n\n" context))
         (shared (list :text question :title "Notes" :scope "selection" :revision 5))
         (display (niwa-transcript-audit-shared-context text shared)))
    (should (equal (plist-get display :text) question))
    (should (equal (plist-get display :context) context))
    (should (equal "Discussion · Notes · Selection" (plist-get display :label)))
    (should-not (niwa-transcript-audit-shared-context text nil))
    (should-not (niwa-transcript-audit-shared-context text (plist-put (copy-sequence shared) :edited t)))
    (should-not (niwa-transcript-audit-shared-context (concat "A host rewrite\n" text) shared))
    (should-not (niwa-transcript-audit-shared-context "An ordinary question" shared)))
  :doc "Discussions label the item and the discussed target, single-line"
  (let* ((display (niwa-transcript-audit-shared-context
                   "Bigger\n\nShared content snapshot (user-provided data):\nOn a.html"
                   (list :kind "artifact" :title "a\nb.html" :text "Bigger" :scope "selection"
                         :targetLabel "Intro › word \"share\""))))
    (should (equal (plist-get display :text) "Bigger"))
    (should (equal (plist-get display :label)
                   "Discussion · a b.html · Intro › word \"share\"")))
  :doc "A whole-item discussion, or one without a target label, says so"
  (dolist (case '(("whole" . "Discussion · Notes · Whole item")
                  ("selection" . "Discussion · Notes · Selection")))
    (should (equal (cdr case)
                   (plist-get (niwa-transcript-audit-shared-context
                               "Q\n\nShared content snapshot (user-provided data):\nX"
                               (list :text "Q" :title "Notes" :scope (car case)))
                              :label)))))

;;; test-niwa-transcript-audit.el ends here
