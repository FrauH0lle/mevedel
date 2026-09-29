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

  :doc "quoted openers cannot hide later trusted records or become trusted themselves"
  (dolist (restored '(nil t))
    (let* ((literal (concat "The summary quotes `" mevedel--hook-audit-open "`.\n"))
           (block (mevedel--format-hook-audit-record
                   '(:type injected-reminders :items ((:body "Current state")))))
           (text (concat literal block)))
      (when restored
        (remove-text-properties (length literal) (length text)
                                '(mevedel-hook-audit nil) text))
      (let ((spans (mevedel-transcript-audit-spans text)))
        (should (= 1 (length spans)))
        (should (eq 'injected-reminders
                    (plist-get (plist-get (car spans) :record) :type)))
        (should (> (plist-get (car spans) :start) (length literal)))
        (should (equal literal (mevedel--strip-hook-audit-blocks text))))))

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
    (should-not (mevedel-transcript-audit-only-p "   ")))

  :doc "accepts separated trusted audits but rejects visible gaps and tails"
  (let ((block (mevedel--format-hook-audit-record '(:type tool-context))))
    (should (mevedel-transcript-audit-only-p
             (concat " \t\r\n" block "\n\t" block "\r\n")))
    (dolist (text (list (concat block "visible" block)
                       (concat block "visible")
                       (concat block "\n<!-- mevedel-hook-audit -->\ninvalid\n"
                               "<!-- /mevedel-hook-audit -->\n")
                       (substring-no-properties block)
                       "" nil))
      (should-not (mevedel-transcript-audit-only-p text))))

  :doc "settles visible gaps before decoding any block"
  (let* ((block (mevedel--format-hook-audit-record '(:type tool-context)))
         (decode (symbol-function 'mevedel--read-hook-audit-record))
         (calls 0))
    (cl-letf (((symbol-function 'mevedel--read-hook-audit-record)
               (lambda (text) (cl-incf calls) (funcall decode text))))
      (should-not (mevedel-transcript-audit-only-p
                   (concat block "visible" block)))
      (should (= 0 calls))
      (should (mevedel-transcript-audit-only-p (concat block "\n" block)))
      (should (= 2 calls)))))

(mevedel-deftest mevedel-transcript-audit-buffer-only-p ()
  ,test
  (test)
  :doc "agrees with the string predicate on the same text"
  (let ((block (mevedel--format-hook-audit-record '(:type tool-context))))
    (dolist (text (list block
                        (concat " \t\r\n" block "\n\t" block "\r\n")
                        (concat "visible" block)
                        (concat block "visible" block)
                        (concat block "visible")
                        (concat block "\n<!-- mevedel-hook-audit -->\ninvalid\n"
                                "<!-- /mevedel-hook-audit -->\n")
                        (substring-no-properties block)
                        "   " ""))
      (with-temp-buffer
        (insert text)
        (should (eq (and (mevedel-transcript-audit-only-p text) t)
                    (and (mevedel-transcript-audit-buffer-only-p
                          (point-min) (point-max))
                         t))))))

  :doc "judges only the requested region, even under narrowing"
  (let ((block (mevedel--format-hook-audit-record '(:type tool-context))))
    (with-temp-buffer
      (insert "visible")
      (let ((start (point)))
        (insert block "\n")
        (let ((end (point)))
          (insert "more visible")
          (narrow-to-region 1 2)
          (should (mevedel-transcript-audit-buffer-only-p start end))
          (should-not (mevedel-transcript-audit-buffer-only-p 1 end))
          (should-not (mevedel-transcript-audit-buffer-only-p start (- end 3)))
          (should (= 2 (point-max)))))))

  :doc "rejects an undecodable trusted block"
  (with-temp-buffer
    (insert (propertize (concat "\n<!-- mevedel-hook-audit -->\ninvalid\n"
                                "<!-- /mevedel-hook-audit -->\n")
                        'gptel 'mevedel-hook-audit 'mevedel-hook-audit t))
    (should-not (mevedel-transcript-audit-buffer-only-p (point-min) (point-max)))))

(mevedel-deftest mevedel-transcript-audit--payload-type ()
  ,test
  (test)
  :doc "names the type a record's head settles, in strings and buffers"
  (let* ((payload (mevedel--hook-audit-record-payload
                   '(:type fork-point :fork-point-id "a")))
         (text (concat "\n" payload "\n")))
    (should (equal '(t . fork-point)
                   (mevedel-transcript-audit--payload-type text 0 (length text))))
    (with-temp-buffer
      (insert "x" text)
      (should (equal '(t . fork-point)
                     (mevedel-transcript-audit--payload-type nil 2 (point-max))))))

  :doc "leaves undecided heads, other key orders, and garbage to a full read"
  (dolist (record '((:event "PostToolUse" :type tool-context)
                    (:type |odd\ name| :x 1)))
    (let ((payload (mevedel--hook-audit-record-payload record)))
      (should-not (mevedel-transcript-audit--payload-type
                   payload 0 (length payload)))))
  (dolist (text '("" "   " "!!!!" "bad base64 here"))
    (should-not (mevedel-transcript-audit--payload-type text 0 (length text)))))

(mevedel-deftest mevedel-transcript-audit--typed-record ()
  ,test
  (test)
  :doc "skips payloads of another type without reading them"
  (with-temp-buffer
    (dotimes (n 3)
      (insert (mevedel--format-hook-audit-record (list :type 'tool-context :number n))))
    (insert (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "a")))
    (let ((read (symbol-function 'mevedel--read-hook-audit-record))
          (reads 0))
      (cl-letf (((symbol-function 'mevedel--read-hook-audit-record)
                 (lambda (text) (cl-incf reads) (funcall read text))))
        (should (= 1 (length (mevedel-transcript-audit-buffer-spans 'fork-point))))
        (should (= 1 reads))
        (should (= 1 (length (mevedel-transcript-audit-spans (buffer-string) 'fork-point))))
        (should (= 2 reads))
        ;; The fork point was already read from this buffer at this tick.
        (should (= 4 (length (mevedel-transcript-audit-buffer-spans))))
        (should (= 5 reads)))))

  :doc "still finds a record whose type is not its first key"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record '(:fork-point-id "late" :type fork-point)))
    (should (equal "late"
                   (plist-get (plist-get (car (mevedel-transcript-audit-buffer-spans 'fork-point))
                                         :record)
                              :fork-point-id)))))

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

(mevedel-deftest mevedel-transcript-audit--buffer-record
  (:doc "decodes a payload once per modification tick without copying it again")
  (let ((buffer (generate-new-buffer " *audit-position-memo*"))
        (reads 0))
    (unwind-protect
        (with-current-buffer buffer
          (insert (mevedel--format-hook-audit-record
                   (list :type 'prompt-rewrite :event "x")))
          (let* ((read (symbol-function 'mevedel--read-hook-audit-record))
                 (spans nil))
            (cl-letf (((symbol-function 'mevedel--read-hook-audit-record)
                       (lambda (text) (cl-incf reads) (funcall read text))))
              (setq spans (mevedel-transcript-audit-buffer-spans))
              (should (equal '("x") (mapcar (lambda (span)
                                              (plist-get (plist-get span :record) :event))
                                            spans)))
              (should (eq (plist-get (car spans) :record)
                          (plist-get (car (mevedel-transcript-audit-buffer-spans))
                                     :record)))
              (should (= 1 reads))
              ;; An edit elsewhere changes the tick; the payload is read again.
              (goto-char (point-min))
              (insert "prefix ")
              (should (mevedel-transcript-audit-buffer-spans))
              (should (= 2 reads)))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-transcript-audit-buffer-spans ()
  ,test
  (test)
  :doc "matches string parsing, widens safely, and bounds retained decode keys"
  (with-temp-buffer
    (insert "literal <!-- mevedel-hook-audit -->\n")
    (dotimes (n 1050)
      (insert (mevedel--format-hook-audit-record (list :type 'tool-context :number n))))
    (let ((expected (mevedel-transcript-audit-spans (buffer-string))) actual)
      (save-restriction
        (narrow-to-region 1 2)
        (setq actual (mevedel-transcript-audit-buffer-spans 'tool-context))
        (should (= 2 (point-max))))
      (should (= 1050 (length actual)))
      (cl-mapc (lambda (string-span buffer-span)
                 (should (equal (plist-get string-span :record) (plist-get buffer-span :record)))
                 (should (= (1+ (plist-get string-span :start)) (plist-get buffer-span :start)))
                 (should (= (1+ (plist-get string-span :end)) (plist-get buffer-span :end))))
               expected actual)
      (should (<= (hash-table-count mevedel-transcript-audit--buffer-records) 1024))
      (should (<= mevedel-transcript-audit--buffer-record-bytes (* 4 1024 1024)))))
  :doc "reuses decoding across scans of a long request"
  (with-temp-buffer
    ;; Just beyond the former record limit; the encoded data fits easily.
    (dotimes (n 129)
      (insert (mevedel--format-hook-audit-record
               (list :type 'injected-reminders :items (list (list :body (number-to-string n)))))))
    (let ((expected (mevedel-transcript-audit-buffer-spans))
          (decode (symbol-function 'mevedel-transcript-audit--decode))
          (misses 0))
      (cl-letf (((symbol-function 'mevedel-transcript-audit--decode)
                 (lambda (text) (cl-incf misses) (funcall decode text))))
        (should (equal expected (mevedel-transcript-audit-buffer-spans))))
      (should (= 0 misses))))
  :doc "bounded scans match string positions and ignore records outside or crossing bounds"
  (with-temp-buffer
    (insert (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "outside")))
    (let ((start (point)))
      (insert "literal <!-- mevedel-hook-audit -->\n")
      (insert (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "inside")))
      (let* ((end (point))
             (expected (mevedel-transcript-audit-spans (buffer-substring start end) 'fork-point)))
        (insert (mevedel--format-hook-audit-record '(:type fork-point :fork-point-id "after")))
        (save-restriction
          (narrow-to-region 1 2)
          (let* ((actual (mevedel-transcript-audit-buffer-spans 'fork-point start end))
                 (span (car actual)))
            (should (= 1 (length actual)))
            (should (equal (plist-get (car expected) :record) (plist-get span :record)))
            (should (= (+ start (plist-get (car expected) :start)) (plist-get span :start)))
            (should (= (+ start (plist-get (car expected) :end)) (plist-get span :end)))
            (should (= 2 (point-max)))
            (should-not (mevedel-transcript-audit-buffer-spans nil (1+ (plist-get span :start)) end))
            (should-not (mevedel-transcript-audit-buffer-spans nil start (1- (plist-get span :end))))))))))

(mevedel-deftest mevedel-transcript-buffer-directive-ranges ()
  ,test
  (test)
  :doc "stream appends reuse audit decoding while edits and trust changes remain visible"
  (with-temp-buffer
    (let* ((start (mevedel--format-hook-audit-record
                   '(:type directive-turn-boundary :edge start :directive-id "d" :turn 1)))
           (end (mevedel--format-hook-audit-record
                 '(:type directive-turn-boundary :edge end :directive-id "d" :turn 1)))
           (decode (symbol-function 'mevedel-transcript-audit--decode))
           (calls 0))
      (insert start "body" end)
      (cl-letf (((symbol-function 'mevedel-transcript-audit--decode)
                 (lambda (text) (cl-incf calls) (funcall decode text))))
        (let ((expected (mevedel-transcript-buffer-directive-ranges)))
          (should (= 1 (length expected)))
          (dotimes (_ 5)
            (goto-char (point-max)) (insert "stream")
            (should (equal expected (mevedel-transcript-buffer-directive-ranges))))
          (should (= 2 calls)))
        ;; Identical bytes without provenance must never reuse trusted ranges.
        (remove-text-properties (point-min) (point-max)
                                '(gptel nil mevedel-hook-audit nil))
        (should-not (mevedel-transcript-buffer-directive-ranges))
        (erase-buffer)
        (insert start "changed body")
        (should-error (mevedel-transcript-buffer-directive-ranges))
        (let ((range (car (mevedel-transcript-buffer-directive-ranges t))))
          (should (= (point-max) (plist-get range :end)))
          (should (eq 'running (plist-get range :outcome))))))))

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
          (insert mevedel--hook-audit-open)
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "phone")))
          (insert "second prompt")
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "laptop")))
          (let ((prompts (mevedel-transcript-audit-guest-prompts)))
            (should (equal '("phone" "laptop") (mapcar (lambda (entry) (plist-get (cdr entry) :name)) prompts)))
            (should (apply #'< (mapcar #'car prompts)))
            ;; The block sits after its prompt and never reaches model
            ;; context.
            (should (> (car (car prompts)) (length "first prompt")))
            (should (get-text-property (car (car prompts)) 'invisible))
            (should (eq 'mevedel-hook-audit
                        (get-text-property (car (car prompts)) 'gptel)))
            ;; Stripping removes every block from visible text.
            (should (equal (concat "first prompt" mevedel--hook-audit-open
                                   "second prompt")
                           (string-trim
                            (mevedel--strip-hook-audit-blocks
                             (buffer-string)))))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel-transcript-audit-guest-prompts/memo
  (:doc "reuses one scan until text or provenance properties change")
  (let ((buffer (generate-new-buffer " *guest-prompt-memo*"))
        (scans 0))
    (unwind-protect
        (with-current-buffer buffer
          (insert "prompt")
          (insert (mevedel--format-hook-audit-record
                   (list :type 'guest-prompt :name "phone")))
          (let ((scan (symbol-function
                       'mevedel-transcript-audit--scan-guest-prompts)))
            (cl-letf (((symbol-function
                        'mevedel-transcript-audit--scan-guest-prompts)
                       (lambda () (cl-incf scans) (funcall scan))))
              (let ((first (mevedel-transcript-audit-guest-prompts)))
                (should (eq first (mevedel-transcript-audit-guest-prompts)))
                (should (= 1 scans))
                (goto-char (point-max))
                (insert (mevedel--format-hook-audit-record
                         (list :type 'guest-prompt :name "laptop")))
                (should (equal '("phone" "laptop")
                               (mapcar (lambda (entry) (plist-get (cdr entry) :name))
                                       (mevedel-transcript-audit-guest-prompts))))
                (should (= 2 scans))
                ;; Losing provenance is a property change the memo sees.
                (with-silent-modifications
                  (remove-text-properties (point-min) (point-max)
                                          '(gptel nil mevedel-hook-audit nil)))
                (should-not (mevedel-transcript-audit-guest-prompts))
                (should (= 3 scans))))))
      (kill-buffer buffer))))

(mevedel-deftest mevedel--strip-hook-audit-blocks/no-copy
  (:doc "unmodified large payloads retain their characters and properties without copies")
  (progn
    (let ((text (propertize (make-string 262144 ?x) 'example 'kept)))
      (should (eq text (mevedel--strip-hook-audit-blocks text)))
      (should (eq 'kept (get-text-property 0 'example text))))
    (let ((literal "<!-- mevedel-hook-audit -->literal<!-- /mevedel-hook-audit -->"))
      (should (eq literal (mevedel--strip-hook-audit-blocks literal))))))

(provide 'test-mevedel-transcript-audit)


(mevedel-deftest mevedel-transcript-audit-shared-context ()
  ,test
  (test)
  :doc "Only attributed generated suffixes fold; edited and ordinary quoted prompts stay visible"
  (let* ((question "What does this mean?\nShared content snapshot (user-provided data):\nI quoted that heading.")
         (context "Shared content snapshot (user-provided data):\n{\"content\":\"data model\"}\n[[file:/tmp/board.png]]")
         (text (concat question "\n\n" context))
         (shared (list :text question :title "Notes" :scope "selection" :revision 5))
         (display (mevedel-transcript-audit-shared-context text shared)))
    (should (equal (plist-get display :text) question))
    (should (equal (plist-get display :context) context))
    (should (string-match-p "Notes.*Selection.*revision 5" (plist-get display :label)))
    (should-not (mevedel-transcript-audit-shared-context text nil))
    (should-not (mevedel-transcript-audit-shared-context text (plist-put (copy-sequence shared) :edited t)))
    (should-not (mevedel-transcript-audit-shared-context (concat "A host rewrite\n" text) shared))
    (should-not (mevedel-transcript-audit-shared-context "An ordinary question" shared)))
  :doc "Artifact comments label the artifact and the commented part, single-line"
  (let* ((display (mevedel-transcript-audit-shared-context
                   "Bigger\n\nShared content snapshot (user-provided data):\nComment on a.html"
                   (list :kind "artifact" :artifact "a\nb.html" :text "Bigger"
                         :anchor (list :label "Intro › word \"share\"")))))
    (should (equal (plist-get display :text) "Bigger"))
    (should (equal (plist-get display :label)
                   "Artifact comment · a b.html · Intro › word \"share\""))))

;;; test-mevedel-transcript-audit.el ends here
