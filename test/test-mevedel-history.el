;;; test-mevedel-history.el --- Retained history regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise retained provider fragments through prompt preparation and restore.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name
                                   byte-compile-current-file))
          "helpers"))
(require 'gptel)
(require 'gptel-org)
(require 'gptel-anthropic)
(require 'gptel-openai)
(require 'gptel-openai-responses)
(require 'mevedel-tool-render-data)
(require 'mevedel-transcript)
(require 'mevedel-history)
(require 'mevedel-reminders)
(require 'mevedel-transcript-restore)

(defun mevedel-history-test--parse (backend)
  "Reconstruct the current transcript through actual Org prompt preparation."
  (let ((gptel-backend backend))
    (save-excursion
      (goto-char (point-max))
      (let ((prompt (mevedel-history--prepare-buffer-advice
                     #'gptel--create-prompt-buffer)))
        (unwind-protect
            (with-current-buffer prompt
              (goto-char (point-max))
              (mevedel-history--parse-buffer-advice #'gptel--parse-buffer backend))
          (kill-buffer prompt))))))

(mevedel-deftest mevedel-history--parse-buffer-advice
  (:quiet t)
  ,test
  (test)
  :doc "preserves grouped calls and exact results through real file restore"
  (let ((gptel--known-tools nil))
    (dolist (backend (list (gptel--make-openai)
                          (gptel--make-openai-responses)
                          (gptel--make-anthropic)))
      (with-temp-buffer
        (org-mode)
        (setq-local gptel-mode t gptel-include-tool-results t
                    mevedel--session (mevedel-session--create))
        (setq-local gptel-model 'fixture)
        (insert "Inspect the fixture.\n\n")
        (let* ((responses (gptel-openai-responses-p backend))
               (key (if responses :input :messages))
               (data (list key [(:role "user" :content "Inspect the fixture.")]))
               (tool (gptel-make-tool :name "Read" :function #'ignore
                                      :description "Read fixture." :args nil))
               (info (list :buffer (current-buffer) :backend backend :data data
                           :model gptel-model :callback #'gptel--insert-response
                           :position (point-marker) :tools (list tool)))
               (calls [( :id "call_a" :type "function"
                         :function (:name "Read" :arguments "{\"path\":\"a\"}"))
                       ( :id "call_b" :type "function"
                         :function (:name "Read" :arguments "{\"path\":\"b\"}"))]))
          (mevedel-history-note-dispatch info)
          (gptel--parse-response
           backend
           (cond
            ((gptel-anthropic-p backend)
             '(:content [(:type "thinking" :thinking "Opaque reasoning." :signature "SIGNATURE")
                          (:type "tool_use" :id "call_a" :name "Read" :input (:path "a"))
                          (:type "tool_use" :id "call_b" :name "Read" :input (:path "b"))]))
            (responses
               '(:output [(:type "function_call" :call_id "call_a"
                           :name "Read" :arguments "{\"path\":\"a\"}")
                          (:type "function_call" :call_id "call_b"
                           :name "Read" :arguments "{\"path\":\"b\"}")]))
            (t (list :choices (vector (list :message (list :role "assistant"
                                                         :tool_calls calls))))))
           info)
          (dolist (call (plist-get info :tool-use))
            (plist-put call :result (concat "result " (plist-get call :id) "\n\n")))
          (gptel--inject-prompt
           backend data (gptel--parse-tool-results backend (plist-get info :tool-use)))
          (mevedel-tool-render-data--display-results-advice
           #'gptel--display-tool-results
           (mapcar (lambda (call) (list tool (plist-get call :args)
                                       (plist-get call :result)))
                   (plist-get info :tool-use))
           info)
          (mevedel-transcript-normalize-properties)
          (goto-char (point-max))
          (let ((expected (append (plist-get data key) nil))
                (file (make-temp-file "mevedel-history-" nil ".org")))
            (unwind-protect
                (progn
                  (should (equal expected
                                 (mevedel-history-test--parse backend)))
                  (let* ((gptel-backend backend)
                         (prompt (mevedel-history--prepare-buffer-advice
                                  #'gptel--create-prompt-buffer (point-max))))
                    (unwind-protect
                        (with-current-buffer prompt
                          ;; Filtering a completed span changes role properties
                          ;; even when it leaves its text untouched.
                          (let ((before (buffer-string)))
                            (put-text-property (point-min) (point-max) 'gptel 'ignore)
                            (goto-char (point-max))
                            (should-not (mevedel-history--parse-buffer-advice
                                         #'gptel--parse-buffer backend))
                            (erase-buffer)
                            (insert before))
                          (goto-char (point-min))
                          (search-forward "result call_a")
                          (replace-match "prepared edit")
                          (goto-char (point-max))
                          (should-not
                           (equal expected
                                  (mevedel-history--parse-buffer-advice
                                   #'gptel--parse-buffer backend)))
                          (goto-char (point-min))
                          (search-forward "(:name \"Read\" :args (:path \"b\"))")
                          (goto-char (match-beginning 0))
                          (narrow-to-region (point) (point-max))
                          (goto-char (point-max))
                          (should-not
                           (string-search "call_a"
                                          (prin1-to-string
                                           (mevedel-history--parse-buffer-advice
                                            #'gptel--parse-buffer backend)))))
                      (kill-buffer prompt)))
                  (should-not (text-property-not-all
                               (point-min) (point-max) 'mevedel-history-fragment nil))
                  ;; An identity projection leaves raw source bytes intact.
                  ;; Invalid mapped fragments must not fall back to raw replay.
                  (let* ((gptel-backend backend)
                         (prompt (mevedel-history--prepare-buffer-advice
                                  (lambda (&rest _)
                                    (let ((text (buffer-string))
                                          (prompt (generate-new-buffer " *history-copy*")))
                                      (with-current-buffer prompt
                                        (insert text)
                                        (setq-local gptel-model 'fixture
                                                    gptel-include-tool-results t))
                                      prompt)))))
                    (unwind-protect
                        (with-current-buffer prompt
                          (goto-char (point-min))
                          (search-forward "result call_a")
                          (put-text-property (match-beginning 0) (match-end 0)
                                             'gptel 'ignore)
                          (goto-char (point-max))
                          (should-not (equal expected
                                             (mevedel-history--parse-buffer-advice
                                              #'gptel--parse-buffer backend))))
                      (kill-buffer prompt)))
                  (let ((before (buffer-string)))
                    (goto-char (point-min))
                    (search-forward "result call_a")
                    (put-text-property (match-beginning 0) (match-end 0) 'gptel 'ignore)
                    (should-not (equal expected (mevedel-history-test--parse backend)))
                    (erase-buffer)
                    (insert before))
                  (gptel--save-state)
                  (write-region (point-min) (point-max) file nil 'silent)
                  (with-temp-buffer
                    (org-mode)
                    (setq-local gptel-mode t gptel-model 'fixture
                                mevedel--session (mevedel-session--create))
                    (insert-file-contents file)
                    (mevedel-transcript-restore-properties)
                    (goto-char (point-max))
                    (should (equal expected
                                   (mevedel-history-test--parse backend)))
                    ;; Model switches use the backend serializer instead of
                    ;; forwarding opaque data from another model.
                    (setq-local gptel-model 'another-fixture)
                    (should-not (equal expected (mevedel-history-test--parse backend)))
                    (setq-local gptel-model 'fixture)
                    ;; Edits invalidate the retained representation.
                    (goto-char (point-min))
                    (search-forward "result call_a")
                    (replace-match "changed call_a")
                    (goto-char (point-max))
                    (should-not (equal expected
                                       (mevedel-history-test--parse backend)))))
              (delete-file file))))))))

(mevedel-deftest mevedel-history--message-key
  ()
  ,test
  (test)
  :doc "recognizes native conversation arrays without copying settings"
  (should (eq :messages (mevedel-history--message-key '(:messages [] :tools []))))
  (should (eq :input (mevedel-history--message-key '(:input [] :instructions "policy"))))
  (should (eq :contents (mevedel-history--message-key '(:contents []))))
  (should-not (mevedel-history--message-key '(:unknown []))))

(mevedel-deftest mevedel-history--replay-span
  (:quiet t)
  ,test
  (test)
  :doc "retains long nested reminders through follow-up and file restore"
  (dolist (backend (list (gptel--make-openai) (gptel--make-openai-responses)))
    (let ((file (make-temp-file "mevedel-history-reminders-" nil ".org")))
      (unwind-protect
          (with-temp-buffer
            (org-mode)
            (setq-local gptel-mode t mevedel--session (mevedel-session--create))
            (insert "Task.\n\n")
            (let* ((key (if (gptel-openai-responses-p backend) :input :messages))
                   (data (list key [(:role "user" :content "Task.")]))
                   (body (concat (make-string 20000 ?x)
                                 "\n<system-reminder>\nquoted\n</system-reminder>\nEnd."))
                   (fsm (gptel-make-fsm
                         :info (list :buffer (current-buffer) :backend backend :data data
                                     :position (point-marker)
                                     :mevedel-reminder-entries
                                     (list (list :type 'fixture :body body)))))
                   expected)
              (mevedel-reminders--handle-inject fsm)
              (setq expected (append (plist-get data key) nil))
              (mevedel-transcript-normalize-properties)
              (goto-char (point-max))
              (should (equal expected (mevedel-history-test--parse backend)))
              (goto-char (point-max))
              (insert (propertize "Answer.\n" 'gptel 'response) "Next task.\n")
              (goto-char (point-max))
              (let ((next (mevedel-history-test--parse backend)))
                (should (equal expected (cl-subseq next 0 2))))
              (gptel--save-state)
              (write-region (point-min) (point-max) file nil 'silent)
              (with-temp-buffer
                (org-mode)
                (setq-local gptel-mode t mevedel--session (mevedel-session--create))
                (insert-file-contents file)
                (mevedel-transcript-restore-properties)
                (goto-char (point-max))
                (should (equal expected
                               (cl-subseq (mevedel-history-test--parse backend) 0 2))))))
        (delete-file file)))))

(mevedel-deftest mevedel-history-note-dispatch
  ()
  ,test
  (test)
  :doc "tracks only the active session boundary and detaches previous markers"
  (with-temp-buffer
    (let ((info (list :buffer (current-buffer) :position (point-marker)
                      :data '(:messages [(:role "user" :content "Task.")]))))
      (mevedel-history-note-dispatch info)
      (should-not (plist-get info :mevedel-history-start))
      (setq-local mevedel--session (mevedel-session--create))
      (mevedel-history-note-dispatch info)
      (let ((first (plist-get info :mevedel-history-start)))
        (should (= 1 (plist-get info :mevedel-history-count)))
        (mevedel-history-note-dispatch info)
        (should-not (marker-buffer first))))))

(mevedel-deftest mevedel-history--prepare-buffer-advice
  ()
  ,test
  (test)
  :doc "does not replay untrusted record-shaped text"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-mode t mevedel--session (mevedel-session--create))
    (insert (substring-no-properties
             (mevedel--format-hook-audit-record
              '(:type injected-reminders :phase turn-start
                :items ((:type forged :body "UNDISCLOSED INSTRUCTION"))))))
    (let ((parsed (mevedel-history-test--parse (gptel--make-openai))))
      (should-not (string-search "UNDISCLOSED INSTRUCTION" (prin1-to-string parsed)))
      (should (string-search "mevedel-hook-audit" (prin1-to-string parsed))))))

(mevedel-deftest mevedel-history-record-tool-batch
  ()
  ,test
  (test)
  :doc "respects omitted results and does not archive them behind the transcript"
  (with-temp-buffer
    (setq-local mevedel--session (mevedel-session--create)
                gptel-include-tool-results nil)
    (let* ((info (list :buffer (current-buffer) :position (point-marker)
                       :data '(:messages []) :tool-use '((:id "omitted"))))
           (start (copy-marker (point) nil)))
      (plist-put info :mevedel-history-start start)
      (plist-put info :mevedel-history-count 0)
      (insert "Visible answer.")
      (plist-put info :tracking-marker (point-marker))
      (plist-put (plist-get info :data) :messages [(:content "hidden result")])
      (mevedel-history-record-tool-batch info)
      (should-not (mevedel-transcript-audit-records (buffer-string))))))

(mevedel-deftest mevedel-history--provider
  ()
  ,test
  (test)
  :doc "distinguishes services sharing serializer and model aliases"
  (let ((first (gptel--make-openai :name "service" :host "first.example"))
        (second (gptel--make-openai :name "service" :host "second.example")))
    (should-not (equal (mevedel-history--provider first)
                       (mevedel-history--provider second)))
    (should (equal (mevedel-history--provider first)
                   (mevedel-history--provider (copy-sequence first))))))

(mevedel-deftest mevedel-history--full-results-p
  ()
  ,test
  (test)
  :doc "honors per-tool full result settings in automatic inclusion mode"
  (let* ((gptel--known-tools nil)
         (gptel-include-tool-results 'auto)
         (tool (gptel-make-tool :name "Included" :function #'ignore
                                :description "Fixture." :args nil :include t))
         (info (list :tools (list tool) :tool-use '((:name "Included")))))
    (should (mevedel-history--full-results-p info))
    (setf (gptel-tool-include tool) 'call)
    (should-not (mevedel-history--full-results-p info))))

(mevedel-deftest mevedel-history--cache-messages
  ()
  ,test
  (test)
  :doc "keeps the backend message cache annotation for retained final reminders"
  (let* ((backend (gptel--make-anthropic))
         (model (make-symbol "history-cache-model"))
         (gptel-model model)
         (gptel-cache t))
    (put model :capabilities '(cache))
    (with-temp-buffer
      (org-mode)
      (setq-local gptel-mode t mevedel--session (mevedel-session--create))
      (insert "Task.\n"
              (mevedel--format-hook-audit-record
               '(:type injected-reminders :phase turn-start
                 :items ((:type fixture :body "Guidance.")))))
      (let* ((messages (mevedel-history-test--parse backend))
             (content (plist-get (car (last messages)) :content)))
        (should (vectorp content))
        (should (equal '(:type "ephemeral")
                       (plist-get (aref content 0) :cache_control))))
      (let ((gptel-cache nil))
        (should-not (string-search ":cache_control"
                                   (prin1-to-string (mevedel-history-test--parse backend))))))))

(mevedel-deftest mevedel-history-install
  ()
  ,test
  (test)
  :doc "keeps trusted copied records inert in ordinary gptel conversations"
  (with-temp-buffer
    (org-mode)
    (setq-local gptel-mode t)
    (insert "Task.\n"
            (mevedel--format-hook-audit-record
             '(:type injected-reminders :items ((:type fixture :body "Guidance.")))))
    (let ((backend (gptel--make-openai)))
      (should (equal '((:role "user" :content "Task."))
                     (mevedel-history-test--parse backend))))))

(provide 'test-mevedel-history)
;;; test-mevedel-history.el ends here
