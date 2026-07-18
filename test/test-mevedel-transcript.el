;;; test-mevedel-transcript.el --- Tests for transcript structure -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))
(require 'mevedel-transcript)
(require 'mevedel-transcript-audit)
(require 'mevedel-utilities)

(defmacro mevedel-transcript-test--with-buffer (&rest body)
  "Run BODY with a temporary Org transcript bound as `data-buf'."
  (declare (indent 0) (debug t))
  `(let ((data-buf (generate-new-buffer " *test-transcript*")))
     (unwind-protect
         (progn
           (with-current-buffer data-buf
             (org-mode))
           ,@body)
       (when (buffer-live-p data-buf)
         (kill-buffer data-buf)))))

(defun mevedel-transcript-test--insert (data-buf text property)
  "Append TEXT to DATA-BUF with gptel PROPERTY."
  (with-current-buffer data-buf
    (goto-char (point-max))
    (let ((start (point)))
      (insert text)
      (when property
        (put-text-property start (point) 'gptel property)))))

(mevedel-deftest mevedel-transcript-prompt-transform-start ()
  ,test
  (test)
  :doc "returns the prompt after leading system reminders"
  (with-temp-buffer
    (insert "<system-reminder>\nremember\n</system-reminder>\n\n")
    (let ((prompt-start (point)))
      (insert "User prompt")
      (should (= prompt-start
                 (mevedel-transcript-prompt-transform-start)))))

  :doc "skips trailing ignored render data when locating prompt start"
  (with-temp-buffer
    (let ((prompt-start (point)))
      (insert (propertize "User prompt" 'gptel 'prompt))
      (insert (propertize "\n<render-data />" 'gptel 'ignore))
      (insert "\n\n")
      (should (= prompt-start
                 (mevedel-transcript-prompt-transform-start))))))

;;
;;; Segment extraction

(mevedel-deftest mevedel-transcript-segments ()
  ,test
  (test)
  :doc "single user segment"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "*** Hello\n" nil)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'user (caar segs))))))

  :doc "user + response segments"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "*** Hello\n" nil)
    (mevedel-transcript-test--insert data-buf "Hi there\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 2 (length segs)))
        (should (eq 'user (caar segs)))
        (should (eq 'response (caadr segs))))))

  :doc "response + tool + response segments"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "Some response\n" 'response)
    (mevedel-transcript-test--insert
     data-buf
     "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nresult\n"
     '(tool . "call_1"))
    (mevedel-transcript-test--insert data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 3 (length segs)))
        (should (eq 'response (caar segs)))
        (should (eq 'tool (caadr segs)))
        (should (eq 'response (car (caddr segs)))))))

  :doc "response table continuation gaps stay in the response"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "| Name" 'response)
    (with-current-buffer data-buf
      (let ((start (point)))
        (insert " | Role |\n")
        (remove-text-properties start (point) '(gptel nil))))
    (mevedel-transcript-test--insert
     data-buf
     "|------|------|\n| Alice | Engineer |\n"
     'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'response (caar segs))))))

  :doc "expands partial start/end to full gptel runs"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "Some response\n" 'response)
    (mevedel-transcript-test--insert
     data-buf
     "\n(:name \"Grep\" :args (:pattern \"foo\"))\n\nmatch\n"
     '(tool . "call_1"))
    (mevedel-transcript-test--insert data-buf "More response\n" 'response)
    (with-current-buffer data-buf
      ;; Simulate incremental rerender entering in the middle of the tool run.
      (let* ((tool-start (next-single-property-change (point-min) 'gptel))
             (mid-start (+ tool-start 2))
             (mid-end (+ tool-start 10))
             (segs (mevedel-transcript-segments mid-start mid-end)))
        (should (= 1 (length segs)))
        (pcase-let ((`(,kind ,seg-start ,_seg-end) (car segs)))
          (should (eq 'tool kind))
          (should (eq ?\n (char-after seg-start)))
          (should (string-prefix-p "\n(:name \"Grep\""
                                   (buffer-substring-no-properties seg-start (+ seg-start 20))))))))

  :doc "repairs restored tool blocks and adjacent response fragments"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (mevedel-transcript-test--insert data-buf "Assistant intro.\n" 'response)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        ;; Simulate stale GPTEL_BOUNDS that cover the begin marker but
        ;; stop before the org block end marker.
        (put-text-property block-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (insert "I added the missing declaration.\n")
        (save-excursion
          (search-backward "eclaration")
          (setq response-start (point)))
        (put-text-property response-start (point) 'gptel 'response))
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (equal '(response tool response) (mapcar #'car segs)))
        (should (string-prefix-p "#+begin_tool"
                                 (buffer-substring-no-properties
                                  (cadr (cadr segs))
                                  (+ (cadr (cadr segs)) 12))))
        (should (string-match-p
                 "I added the missing declaration"
                 (buffer-substring-no-properties
                  (cadr (caddr segs)) (caddr (caddr segs))))))))
  :doc "splits assistant prefixes after restored agent-result blocks"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start prefix-start response-start)
        (setq block-start (point))
        (insert "<agent-result agent-id=\"verifier--1\" type=\"verifier\" description=\"Verify\">\n"
                "VERDICT: PASS\n"
                "</agent-result>\n")
        (setq prefix-start (point))
        (insert "Green loo")
        (setq response-start (point))
        (insert "p completed.\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(mailbox response) (mapcar #'car segs)))
          (should (= (cadr (car segs)) block-start))
          (should (= (caddr (car segs)) prefix-start))
          (should (string-prefix-p
                   "Green loop completed"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "does not treat literal tool markers as tool blocks without a tool run"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert
     data-buf
     (concat "Text mentioning markers:\n"
             "#+begin_tool (Read :file_path \"/tmp/f\")\n"
             "(:name \"Read\")\n#+end_tool\n")
     'response)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'response (caar segs))))))
  :doc "preserves text after a stale tool run extends beyond end marker"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (insert "Tail text must survive.\n")
        ;; Stale bounds can cover the block and spill into following text.
        (put-text-property (+ block-start 20) (point)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool user) (mapcar #'car segs)))
          (should (= block-end (cadr (cadr segs))))
          (should (string-match-p
                   "Tail text must survive"
                   (buffer-substring-no-properties
                    (cadr (cadr segs)) (caddr (cadr segs)))))))))
  :doc "keeps literal end-tool markers inside recovered tool results"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "before\n"
                "#+end_tool\n"
                "After\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property block-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "keeps persisted-looking tool blocks inside recovered tool results"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n"
                "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n"
                "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property block-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not extend recovered tool block to response marker text"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property block-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (setq response-start (point))
        (insert "Assistant text can mention markers.\n#+end_tool\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not extend stale spill through response marker text"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "file body\n"
                "#+end_tool\n")
        (setq block-end (point))
        (setq response-start (point))
        (insert "Assistant text can mention markers.\n#+end_tool\n")
        (put-text-property block-start (point)
                           'gptel '(tool . "call_1"))
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= block-end (caddr (car segs))))))))
  :doc "recovers outer block when stale bounds start at nested marker"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end nested-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n")
        (setq nested-start (point))
        (insert "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n"
                "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property nested-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not cross unpropertized output after a literal close"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end nested-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n"
                "#+end_tool\n"
                "still outer output\n")
        (setq nested-start (point))
        (insert "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n"
                "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property nested-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= nested-start (cadr (cadr segs))))
          (should (= block-end (caddr (cadr segs))))))))
  :doc "keeps same tool run across a nested-looking marker"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n"
                "#+end_tool\n"
                "still outer output\n"
                "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n"
                "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property block-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "splits adjacent persisted tool blocks sharing a stale tool prop"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (first-start first-end second-start second-end)
        (setq first-start (point))
        (insert "#+begin_tool (Agent :subagent_type \"reviewer\")\n"
                "(:name \"Agent\" :args (:subagent_type \"reviewer\"))\n\n"
                "reviewer body\n"
                "#+end_tool\n")
        (setq first-end (point))
        (setq second-start (point))
        (insert "#+begin_tool (Agent :subagent_type \"verifier\")\n"
                "(:name \"Agent\" :args (:subagent_type \"verifier\"))\n\n"
                "verifier body\n"
                "#+end_tool\n")
        (setq second-end (point))
        (put-text-property first-start second-end
                           'gptel '(tool . "call_shared"))
        (let ((segs (mevedel-transcript-segments
                     (point-min) (point-max))))
          (should (equal '(tool tool) (mapcar #'car segs)))
          (should (= first-start (cadr (car segs))))
          (should (= first-end (caddr (car segs))))
          (should (= second-start (cadr (cadr segs))))
          (should (= second-end (caddr (cadr segs))))))))
  :doc "does not extend stale tool block across mailbox delivery"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (first-start first-end second-start second-end)
        (setq first-start (point))
        (insert "#+begin_tool (TaskList)\n"
                "(:name \"TaskList\" :args nil)\n\n"
                "tasks\n"
                "#+end_tool\n"
                "Waiting on verifier result.\n"
                "<agent-result agent-id=\"verifier--tool-gap\" type=\"verifier\">\n"
                "VERDICT: FAIL\n"
                "</agent-result>\n")
        (setq second-start (point))
        (insert "#+begin_tool (TaskUpdate)\n"
                "(:name \"TaskUpdate\" :args nil)\n\n"
                "updated\n"
                "#+end_tool\n")
        (setq second-end (point))
        (save-excursion
          (goto-char first-start)
          (re-search-forward "^#\\+end_tool[^\n]*\n")
          (setq first-end (point)))
        ;; Simulate stale restored bounds that paint the whole range as
        ;; one tool run even though the middle contains assistant prose
        ;; and a mailbox delivery.
        (put-text-property first-start second-end
                           'gptel '(tool . "call_stale"))
        (let ((segs (mevedel-transcript-segments
                     (point-min) (point-max))))
          (should (equal '(tool user mailbox tool) (mapcar #'car segs)))
          (should (= first-start (cadr (car segs))))
          (should (= first-end (caddr (car segs))))
          (should (= second-start (cadr (cadddr segs))))
          (should (= second-end (caddr (cadddr segs))))
          (should (string-match-p
                   "verifier--tool-gap"
                   (buffer-substring-no-properties
                    (cadr (caddr segs)) (caddr (caddr segs)))))))))
  :doc "does not cross a blank unpropertized gap after a literal close"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end nested-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n"
                "#+end_tool\n\n"
                "still outer output\n")
        (setq nested-start (point))
        (insert "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n"
                "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property nested-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= nested-start (cadr (cadr segs))))
          (should (= block-end (caddr (cadr segs))))))))
  :doc "recovers outer block when stale bounds start after a nested marker"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start block-end stale-start)
        (setq block-start (point))
        (insert "#+begin_tool (RecoverRead :file_path \"/tmp/f\")\n"
                "(:name \"RecoverRead\" :args (:file_path \"/tmp/f\"))\n\n"
                "outer before\n"
                "#+begin_tool (Bash :command \"echo nested\")\n"
                "(:name \"Bash\" :args (:command \"echo nested\"))\n"
                "nested result\n"
                "#+end_tool\n")
        (setq stale-start (point))
        (insert "outer after\n"
                "#+end_tool\n")
        (setq block-end (point))
        (put-text-property stale-start (- block-end 12)
                           'gptel '(tool . "call_1"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool) (mapcar #'car segs)))
          (should (= block-start (cadr (car segs))))
          (should (= block-end (caddr (car segs))))))))
  :doc "does not merge a previous unpropertized block into a later tool"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (real-start real-end)
        (insert "#+begin_tool (Read :file_path \"/quoted\")\n"
                "(:name \"Read\" :args (:file_path \"/quoted\"))\n"
                "quoted body\n"
                "#+end_tool\n")
        (setq real-start (point))
        (insert "#+begin_tool (Bash :command \"echo real\")\n"
                "(:name \"Bash\" :args (:command \"echo real\"))\n"
                "real output\n"
                "#+end_tool\n")
        (setq real-end (point))
        (put-text-property (+ real-start 20) (- real-end 12)
                           'gptel '(tool . "call_real"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= real-start (cadr (cadr segs))))
          (should (= real-end (caddr (cadr segs))))))))
  :doc "does not merge a prose gap before a later unpropertized tool"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (real-start real-end)
        (insert "#+begin_tool (Read :file_path \"/quoted\")\n"
                "(:name \"Read\" :args (:file_path \"/quoted\"))\n"
                "quoted body\n"
                "#+end_tool\n"
                "Normal assistant text before real tool.\n")
        (setq real-start (point))
        (insert "#+begin_tool (Bash :command \"echo real\")\n"
                "(:name \"Bash\" :args (:command \"echo real\"))\n"
                "real output\n"
                "#+end_tool\n")
        (setq real-end (point))
        (put-text-property (+ real-start 20) (- real-end 12)
                           'gptel '(tool . "call_real"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(user tool) (mapcar #'car segs)))
          (should (= real-start (cadr (cadr segs))))
          (should (= real-end (caddr (cadr segs))))))))
  :doc "does not cross response text before a stale tool run"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (response-start real-start real-end)
        (insert "#+begin_tool (Read :file_path \"/quoted\")\n"
                "(:name \"Read\" :args (:file_path \"/quoted\"))\n"
                "quoted body\n"
                "#+end_tool\n\n")
        (setq response-start (point))
        (insert "Normal assistant text before real tool.\n")
        (put-text-property response-start (point) 'gptel 'response)
        (setq real-start (point))
        (insert "#+begin_tool (Bash :command \"echo real\")\n"
                "(:name \"Bash\" :args (:command \"echo real\"))\n"
                "real output\n"
                "#+end_tool\n")
        (setq real-end (point))
        (put-text-property (+ real-start 20) (- real-end 12)
                           'gptel '(tool . "call_real"))
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(user response tool) (mapcar #'car segs)))
          (should (= real-start (cadr (caddr segs))))
          (should (= real-end (caddr (caddr segs))))))))
  :doc "recovers a structural close line misclassified as response"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start close-start close-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (Read :file_path \"/tmp/f\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
                "body\n")
        (setq close-start (point))
        (insert "#+end_tool\n")
        (setq close-end (point))
        (setq response-start (point))
        (insert "Assistant response.\n")
        (put-text-property block-start close-start
                           'gptel '(tool . "call_1"))
        (put-text-property close-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close when body lost tool property"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start body-start close-start close-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (Read :file_path \"/tmp/f\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n")
        (setq body-start (point))
        (insert "body\n")
        (setq close-start (point))
        (insert "#+end_tool\n")
        (setq close-end (point))
        (setq response-start (point))
        (insert "Assistant response.\n")
        (put-text-property block-start body-start
                           'gptel '(tool . "call_1"))
        (put-text-property close-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close after a literal output marker"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start close-start close-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (Read :file_path \"/tmp/f\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
                "body before\n"
                "#+end_tool\n"
                "body after\n")
        (setq close-start (point))
        (insert "#+end_tool\n")
        (setq close-end (point))
        (setq response-start (point))
        (insert "Assistant response.\n")
        (put-text-property block-start close-start
                           'gptel '(tool . "call_1"))
        (put-text-property close-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "recovers response-marked close after unclassified body text"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (block-start gap-start close-start close-end response-start)
        (setq block-start (point))
        (insert "#+begin_tool (Read :file_path \"/tmp/f\")\n"
                "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\n"
                "body before\n"
                "#+end_tool\n")
        (setq gap-start (point))
        (insert "body after\n")
        (setq close-start (point))
        (insert "#+end_tool\n")
        (setq close-end (point))
        (setq response-start (point))
        (insert "Assistant response.\n")
        (put-text-property block-start gap-start
                           'gptel '(tool . "call_1"))
        (put-text-property close-start (point) 'gptel 'response)
        (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
          (should (equal '(tool response) (mapcar #'car segs)))
          (should (= close-end (caddr (car segs))))
          (should (= response-start (cadr (cadr segs))))))))
  :doc "does not swallow legitimate user prompt between tool and response"
  (mevedel-transcript-test--with-buffer
    (with-current-buffer data-buf
      (let (tool-start response-start)
        (setq tool-start (point))
        (insert "(:name \"Read\" :args (:file_path \"/tmp/f\"))\n\nbody\n")
        (put-text-property tool-start (point) 'gptel '(tool . "call_1"))
        (insert "please explain that output\n")
        (setq response-start (point))
        (insert "sure, here is the explanation\n")
        (put-text-property response-start (point) 'gptel 'response)
        (let ((segments (mevedel-transcript-segments
                         (point-min) (point-max))))
          (should (equal '(tool user response)
                         (mapcar #'car segments)))))))

  :doc "ignore segment"
  (mevedel-transcript-test--with-buffer
    (mevedel-transcript-test--insert data-buf "thinking...\n" 'ignore)
    (with-current-buffer data-buf
      (let ((segs (mevedel-transcript-segments (point-min) (point-max))))
        (should (= 1 (length segs)))
        (should (eq 'ignored (caar segs))))))
  :doc "classifies every persisted transcript control range"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let ((tool-start (point)))
      (insert "#+begin_tool\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (put-text-property tool-start (point) 'gptel '(tool . "call-1")))
    (insert "#+begin_reasoning\nthinking\n#+end_reasoning\n"
            "<agent-result agent-id=\"agent-1\">\ndone\n</agent-result>\n"
            "<system-reminder>\nremember\n</system-reminder>\n"
            "<hook-context>\n<hook-event name=\"UserPromptSubmit\">ctx</hook-event>\n"
            "</hook-context>\n"
            "<!-- mevedel-render-data -->\n(:kind diff)\n"
            "<!-- /mevedel-render-data -->\n"
            ":PROMPT:\nhidden prompt\n:END:\n"
            "<!-- mevedel-hook-audit -->\naudit\n"
            "<!-- /mevedel-hook-audit -->\n")
    (let ((types (mapcar #'car
                         (mevedel-transcript-segments
                          (point-min) (point-max)))))
      (dolist (type '(tool reasoning mailbox reminder hook-context
                           render-data prompt ignored))
        (should (memq type types)))))

  :doc "leaves incomplete live control text as ordinary transcript text"
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_reasoning\npartial")
    (should (equal '(user)
                   (mapcar #'car
                           (mevedel-transcript-segments
                            (point-min) (point-max))))))
  :doc "skips leading metadata and classifies transcript spans"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:foo: bar\n:END:\n")
    (insert "#+begin_summary\nold summary\n#+end_summary\n\n")
    (let (user-start user-end response-start response-end tool-start tool-end)
      (setq user-start (point))
      (insert "User request\n")
      (setq user-end (point))
      (setq response-start (point))
      (insert (propertize "Assistant response\n" 'gptel 'response))
      (setq response-end (point))
      (setq tool-start (point))
      (insert (propertize "#+begin_tool\n(:name \"Read\")\n#+end_tool\n"
                          'gptel '(tool . "1")))
      (setq tool-end (point))
      (should (equal (mevedel-transcript-segments
                      (point-min) (point-max))
                     `((user ,user-start ,user-end)
                       (response ,response-start ,response-end)
                       (tool ,tool-start ,tool-end))))))

  :doc "keeps restored newline-delimited prompts between responses"
  (with-temp-buffer
    (let (first-response-start first-response-end
                               second-prompt-start second-prompt-end
                               second-response-start second-response-end)
      (setq first-response-start (point))
      (insert "First response.\n")
      (setq first-response-end (1- (point)))
      (put-text-property first-response-start first-response-end
                         'gptel 'response)
      (setq second-prompt-start (point))
      (insert "Second prompt.\n")
      (setq second-prompt-end (point))
      (setq second-response-start (point))
      (insert "Second response.")
      (setq second-response-end (point))
      (put-text-property second-response-start second-response-end
                         'gptel 'response)
      (let ((segs (mevedel-transcript-segments
                   (point-min) (point-max))))
        (should (equal '(response user response)
                       (mapcar #'car segs)))
        (should (<= (cadr (cadr segs)) second-prompt-start))
        (should (= second-prompt-end (caddr (cadr segs))))))))


(mevedel-deftest mevedel-transcript--skip-leading-properties-drawer ()
  ,test
  (test)
  :doc "advances past a well-formed :PROPERTIES: drawer at POS"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\n:END:\nhello\n")
    (let ((after (mevedel-transcript--skip-leading-properties-drawer (point-min))))
      (should (> after (point-min)))
      (should (string= "hello\n" (buffer-substring-no-properties
                                  after (point-max))))))
  :doc "returns POS unchanged when no drawer is present"
  (with-temp-buffer
    (insert "no drawer here\n")
    (should (= (point-min)
               (mevedel-transcript--skip-leading-properties-drawer (point-min)))))
  :doc "returns POS unchanged when drawer is malformed (no :END:)"
  (with-temp-buffer
    (insert ":PROPERTIES:\n:GPTEL_MODEL: x\nstuff\n")
    (should (= (point-min)
               (mevedel-transcript--skip-leading-properties-drawer (point-min))))))


(mevedel-deftest mevedel-transcript--skip-leading-summary-block ()
  ,test
  (test)
  :doc "advances past a leading compaction summary block"
  (with-temp-buffer
    (insert "#+begin_summary mevedel-role=compaction-summary\nsummary\n#+end_summary\nlive\n")
    (let ((after (mevedel-transcript--skip-leading-summary-block (point-min))))
      (should (> after (point-min)))
      (should (string= "live\n" (buffer-substring-no-properties
                                 after (point-max))))))
  :doc "returns POS unchanged when no summary starts there"
  (with-temp-buffer
    (insert "live\n")
    (should (= (point-min)
               (mevedel-transcript--skip-leading-summary-block (point-min))))))



(defun mevedel-transcript-test--all-gptel-prop-p (start end expected)
  "Return non-nil when START..END all has gptel EXPECTED."
  (let ((pos start)
        (ok t))
    (while (and ok (< pos end))
      (unless (equal (get-text-property pos 'gptel) expected)
        (setq ok nil))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    ok))


(defun mevedel-transcript-test--no-tool-prop-p (start end)
  "Return non-nil when START..END has no tool gptel property."
  (let ((pos start)
        (ok t))
    (while (and ok (< pos end))
      (let ((prop (get-text-property pos 'gptel)))
        (when (or (eq prop 'tool)
                  (and (consp prop) (eq (car prop) 'tool)))
          (setq ok nil)))
      (setq pos (or (next-single-property-change pos 'gptel nil end)
                    end)))
    ok))


(defun mevedel-transcript-test--tool-block-props-p
    (start end tool-prop)
  "Return non-nil when org tool START..END has parseable TOOL-PROP.
The `#+begin_tool' and `#+end_tool' scaffolding must not carry a tool
property, while the readable `(:name ...)' sexp and result body carry
TOOL-PROP."
  (when-let* ((parts (mevedel-transcript--org-tool-block-parts
                      start end)))
    (and (mevedel-transcript-test--no-tool-prop-p
          (plist-get parts :prefix-start)
          (plist-get parts :prefix-end))
         (mevedel-transcript-test--all-gptel-prop-p
          (plist-get parts :tool-start)
          (plist-get parts :tool-end)
          tool-prop)
         (mevedel-transcript-test--no-tool-prop-p
          (plist-get parts :suffix-start)
          (plist-get parts :suffix-end)))))


(mevedel-deftest mevedel-transcript-normalize-properties ()
  ,test
  (test)
  :doc "repairs stale tool, mailbox, and structural transcript properties"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n*** User prompt\n")
    (let (tool-start tool-end response-start response-end task-start task-end
          waiting-start waiting-end mailbox-start mailbox-end after-start
          after-end reasoning-start reasoning-end)
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff --stat\")\n"
              "(:name \"Bash\" :args (:command \"git diff --stat\"))\n\n"
              " file.el | 1 +\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nThinking text.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      (setq response-start (point))
      (insert "I found uncommitted changes on top of the requested commits.\n")
      (setq response-end (point))
      (setq task-start (point))
      (insert "#+begin_tool (TaskList :status \"in_progress\")\n"
              "(:name \"TaskList\" :args (:status \"in_progress\"))\n\n"
              "Tasks with status in_progress:\n"
              "#3 [in_progress] Run reviewer and verifier on current diff\n"
              "#+end_tool\n")
      (setq task-end (point))
      (setq waiting-start (point))
      (insert "Waiting for the reviewer and verifier results.\n\n")
      (setq waiting-end (point))
      (setq mailbox-start (point))
      (insert "<agent-result agent-id=\"reviewer--abc\" type=\"reviewer\" "
              "description=\"Review current diff\">\n"
              "{\"overall_correctness\":\"patch is incorrect\"}\n"
              "</agent-result>\n")
      (setq mailbox-end (point))
      (setq after-start (point))
      (insert "The reviewer found a blocking permission-mode issue.\n")
      (setq after-end (point))
      ;; Simulate stale saved-session structure: tool ids exist only on
      ;; fragments and response properties bleed into structural blocks.
      (put-text-property (+ tool-start 54) (- tool-end 12)
                         'gptel '(tool . "call_diff"))
      (put-text-property reasoning-start reasoning-end 'gptel 'ignore)
      (put-text-property (- response-start 20) (- response-end 10)
                         'gptel 'response)
      (put-text-property (+ task-start 50) (- task-end 12)
                         'gptel '(tool . "call_task"))
      (put-text-property waiting-start (- waiting-end 20)
                         'gptel 'response)
      (put-text-property (- mailbox-end 24) (- after-end 12)
                         'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--tool-block-props-p
               tool-start tool-end '(tool . "call_diff")))
      (should (mevedel-transcript-test--tool-block-props-p
               task-start task-end '(tool . "call_task")))
      (should (mevedel-transcript-test--all-gptel-prop-p
               reasoning-start reasoning-end 'ignore))
      (should (mevedel-transcript-test--all-gptel-prop-p
               response-start (- response-end 10) 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p (- response-end 10) response-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p
               waiting-start (- waiting-end 20) 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p (- waiting-end 20) waiting-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p mailbox-start mailbox-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p after-start (- after-end 12) 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p (- after-end 12) after-end nil))))
  :doc "does not promote the org drawer or plain first prompt to response"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:MEVEDEL_SESSION_ID: main\n:END:\n\n")
    (let ((drawer-end (point))
          user-start user-end response-start response-end tool-start tool-end)
      (setq user-start (point))
      (insert "Do a review of the changes. Do not modify code.\n\n")
      (setq user-end (point))
      (setq response-start (point))
      (insert "I will inspect the diff and report findings.\n")
      (setq response-end (point))
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output\n"
              "#+end_tool\n")
      (setq tool-end (point))
      ;; User text is nil-property, while the assistant text has a partial
      ;; response run.  Normalization must not infer missing prose bounds.
      (put-text-property response-start (- response-end 10)
                         'gptel 'response)
      (put-text-property (+ tool-start 45) (- tool-end 12)
                         'gptel '(tool . "call_diff"))
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--all-gptel-prop-p (point-min) drawer-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p user-start user-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p
               response-start (- response-end 10) 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p (- response-end 10) response-end nil))
      (should (mevedel-transcript-test--tool-block-props-p
               tool-start tool-end '(tool . "call_diff")))))
  :doc "leaves pasted tool-shaped text in user prompts unclassified"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (user-start user-end)
      (setq user-start (point))
      (insert "Please inspect this transcript:\n\n"
              "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output copied from another editor\n"
              "#+end_tool\n\n"
              "What happened here?\n")
      (setq user-end (point))
      (mevedel-transcript-normalize-properties)
      (let ((pos user-start)
            (ok t))
        (while (and ok (< pos user-end))
          (when (get-text-property pos 'gptel)
            (setq ok nil))
          (setq pos (or (next-single-property-change pos 'gptel nil user-end)
                        user-end)))
        (should ok))))

  :doc "restores persisted hook audit side channels as ignored text"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n"
            "#+begin_summary mevedel-role=compaction-summary\n"
            "summary\n"
            (substring-no-properties
             (mevedel--format-hook-audit-record
              '(:type compact-context
                      :event "PreCompact"
                      :context "private note")))
            "#+end_summary\n")
    (goto-char (point-min))
    (search-forward "<!-- mevedel-hook-audit -->")
    (should-not (get-text-property (match-beginning 0) 'gptel))
    (mevedel-transcript-normalize-properties)
    (should (eq (get-text-property (match-beginning 0) 'gptel)
                'ignore)))

  :doc "leaves pasted tool-shaped text inside reasoning unclassified"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (reasoning-start tool-start tool-end reasoning-end)
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nUser pasted transcript starts here.\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"git diff\")\n"
              "(:name \"Bash\" :args (:command \"git diff\"))\n\n"
              "diff output copied by user\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (insert "Pasted transcript tail.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      (mevedel-transcript-normalize-properties)
      (should (eq (get-text-property reasoning-start 'gptel) 'ignore))
      (should-not (text-property-any tool-start tool-end 'gptel '(tool . "")))
      (should (eq (get-text-property (1- reasoning-end) 'gptel) 'ignore))))
  :doc "keeps later plain user prompts outside repaired response runs"
  (with-temp-buffer
    (org-mode)
    (let (user1-start user1-end response1-start response1-end
          user2-start user2-end response2-start response2-end)
      (setq user1-start (point))
      (insert "First plain prompt\n\n")
      (setq user1-end (point))
      (setq response1-start (point))
      (insert "First answer.\n\n")
      (setq response1-end (point))
      (setq user2-start (point))
      (insert "Second plain prompt\n\n")
      (setq user2-end (point))
      (setq response2-start (point))
      (insert "Second answer with missing tail property.\n")
      (setq response2-end (point))
      (put-text-property response1-start response1-end
                         'gptel 'response)
      (put-text-property response2-start (- response2-end 10)
                         'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--all-gptel-prop-p user1-start user1-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p response1-start response1-end 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p user2-start user2-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p
               response2-start (- response2-end 10) 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p (- response2-end 10) response2-end nil))))
  :doc "repairs mid-line response prefixes after structural blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (tool-start tool-end tool-prefix-start tool-response-start
          tool-response-end agent-start agent-end agent-prefix-start
          agent-response-start agent-response-end)
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq tool-prefix-start (point))
      (insert "Focu")
      (setq tool-response-start (point))
      (insert "sed tests are green.\n")
      (setq tool-response-end (point))
      (insert "\n")
      (setq agent-start (point))
      (insert "<agent-result agent-id=\"verifier--1\" type=\"verifier\" description=\"Verify\">\n"
              "VERDICT: PASS\n"
              "</agent-result>\n")
      (setq agent-end (point))
      (setq agent-prefix-start (point))
      (insert "The verifier retur")
      (setq agent-response-start (point))
      (insert "ned PASS.\n")
      (setq agent-response-end (point))
      (put-text-property (+ tool-start 42) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (put-text-property tool-response-start tool-response-end
                         'gptel 'response)
      (put-text-property agent-response-start agent-response-end
                         'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--tool-block-props-p
               tool-start tool-end '(tool . "call_read")))
      (should (mevedel-transcript-test--all-gptel-prop-p
               tool-prefix-start tool-response-end 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p agent-start agent-end nil))
      (should (mevedel-transcript-test--all-gptel-prop-p agent-prefix-start agent-response-end
                                                         'response))))
  :doc "does not repair response prefixes across a real prompt"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (tool-start tool-end second-prompt-start
          response-prefix-start response-start response-end)
      (insert "First prompt\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq second-prompt-start (point))
      (insert "Second prompt\n")
      (setq response-prefix-start (point))
      (insert "Conti")
      (setq response-start (point))
      (insert "nuing the answer.\n")
      (setq response-end (point))
      (put-text-property (+ tool-start 42) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (put-text-property response-start response-end
                         'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should-not (get-text-property second-prompt-start 'gptel))
      (should-not (get-text-property response-prefix-start 'gptel))
      (should (eq (get-text-property response-start 'gptel) 'response))))
  :doc "repairs multi-run response prefixes after reasoning blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n")
    (let (reasoning1-start reasoning1-end reasoning1-prefix-start
          reasoning1-response-start reasoning1-response-end
          reasoning2-start reasoning2-end reasoning2-prefix-start
          reasoning2-nil-start reasoning2-response-start
          reasoning2-response-end)
      (setq reasoning1-start (point))
      (insert "#+begin_reasoning\nThinking.\n#+end_reasoning\n")
      (setq reasoning1-end (point))
      (insert "\n")
      (setq reasoning1-prefix-start (point))
      (insert "I")
      (setq reasoning1-response-start (point))
      (insert "’ll start by checking bounds.\n")
      (setq reasoning1-response-end (point))
      (insert "\n")
      (setq reasoning2-start (point))
      (insert "#+begin_reasoning\nMore thinking.\n#+end_reasoning\n")
      (setq reasoning2-end (point))
      (insert "\n")
      (setq reasoning2-prefix-start (point))
      (insert "Whi")
      (setq reasoning2-nil-start (point))
      (insert "l")
      (setq reasoning2-response-start (point))
      (insert "e the agents run, I’ll test.\n")
      (setq reasoning2-response-end (point))
      (put-text-property reasoning1-start reasoning1-end
                         'gptel 'ignore)
      (put-text-property reasoning1-response-start reasoning1-response-end
                         'gptel 'response)
      (put-text-property reasoning2-start reasoning2-nil-start
                         'gptel 'ignore)
      (put-text-property reasoning2-response-start reasoning2-response-end
                         'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--all-gptel-prop-p reasoning1-start reasoning1-end 'ignore))
      (should (mevedel-transcript-test--all-gptel-prop-p reasoning1-prefix-start
                                                         reasoning1-response-end 'response))
      (should (mevedel-transcript-test--all-gptel-prop-p reasoning2-start reasoning2-end 'ignore))
      (should (mevedel-transcript-test--all-gptel-prop-p reasoning2-prefix-start
                                                         reasoning2-response-end 'response))))
  :doc "preserves tool blocks nested inside reasoning blocks"
  (with-temp-buffer
    (org-mode)
    (let (reasoning-start tool-start tool-end reasoning-tail-start
          reasoning-end)
      (setq reasoning-start (point))
      (insert "#+begin_reasoning\nBefore tool.\n")
      (setq tool-start (point))
      (insert "#+begin_tool (Read :file_path \"a.el\")\n"
              "(:name \"Read\" :args (:file_path \"a.el\"))\n\n"
              "contents\n"
              "#+end_tool\n")
      (setq tool-end (point))
      (setq reasoning-tail-start (point))
      (insert "After tool.\n#+end_reasoning\n")
      (setq reasoning-end (point))
      ;; Simulate the backend shape where the outer reasoning block and the
      ;; inner tool call both have persisted GPTEL_BOUNDS ranges.  The
      ;; normalization pass must not let the broad reasoning range overwrite
      ;; the nested tool range.
      (put-text-property reasoning-start reasoning-end 'gptel 'ignore)
      (put-text-property (+ tool-start 46) (- tool-end 12)
                         'gptel '(tool . "call_read"))
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--all-gptel-prop-p reasoning-start tool-start 'ignore))
      (should (mevedel-transcript-test--tool-block-props-p
               tool-start tool-end '(tool . "call_read")))
      (should (mevedel-transcript-test--all-gptel-prop-p
               reasoning-tail-start reasoning-end 'ignore))))
  :doc "keeps render-data separators out of tool-call property runs"
  (with-temp-buffer
    (org-mode)
    (let (tool-start tool-end separator-start separator-end response-start)
      (setq tool-start (point))
      (insert "#+begin_tool (Bash :command \"date\")\n"
              "(:name \"Bash\" :args (:command \"date\"))\n\n"
              "Execution completed.\n"
              "<!-- mevedel-render-data -->\n"
              "(:kind bash-result :exit-code 0)\n"
              "<!-- /mevedel-render-data -->\n")
      (setq separator-start (point))
      (insert "\n")
      (setq separator-end (point))
      (insert "#+end_tool\n")
      (setq tool-end (point)
            response-start (point))
      (insert "The command completed.\n")
      (put-text-property (+ tool-start 42) (- tool-end 12)
                         'gptel '(tool . "call_date"))
      (put-text-property response-start (point) 'gptel 'response)
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--all-gptel-prop-p
               separator-start separator-end 'ignore))))
  :doc "returns and leaves unclosed structural openers unclassified"
  (dolist (snippet `("#+begin_reasoning\npartial thought"
                     ,(concat "#+begin_tool (Bash :command \"date\")\n"
                              "(:name \"Bash\" :args (:command \"date\"))\n\n"
                              "partial output")
                     "<system-reminder>\npartial reminder"
                     "<hook-context>\npartial hook"
                     "<!-- mevedel-render-data -->\n(:kind agent-transcript)"
                     ":PROMPT:\npartial prompt"))
    (with-temp-buffer
      (org-mode)
      (insert ":PROPERTIES:\n:END:\n*** User prompt\n\n")
      (let ((start (point)))
        (insert snippet)
        (with-timeout
            (1 (ert-fail
                (format "normalization looped on incomplete block: %S"
                        snippet)))
          (mevedel-transcript-normalize-properties))
        (should-not (get-text-property start 'gptel)))))

  :doc "clears stale tool properties from closed unparseable org tool blocks"
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:END:\n*** User prompt\n\n")
    (let ((start (point)))
      (insert "#+begin_tool (Bash :command \"date\")\nnot a sexp\n#+end_tool\n")
      (put-text-property start (point) 'gptel '(tool . "call-bad"))
      (mevedel-transcript-normalize-properties)
      (should (mevedel-transcript-test--no-tool-prop-p
               start (point))))))

(provide 'test-mevedel-transcript)
;;; test-mevedel-transcript.el ends here
